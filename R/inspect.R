## Module responsibility
#
# Examine the data without modifying it.

box::use(
    dplyr[
        across,
        mutate
    ],
    naniar[
        miss_scan_count,
        miss_var_summary
    ],
    readr[problems],
    skimr[skim_without_charts],
    tidyselect[where]
)

#' Normalize character columns for case-insensitive quality checks.
#'
#' @param data A data frame.
#' @return `data` with character values trimmed and converted to lowercase.
#' @noRd
normalize_character_data <- function(data) {
    data |>
        mutate(
            across(
                where(is.character),
                ~ tolower(trimws(.x))
            )
        )
}

#' Summarize missing and duplicated identifiers.
#'
#' @param data A data frame.
#' @param id_column Name of the identifier column, or `NULL` to skip the check.
#' @return A list containing counts of missing and duplicated identifiers.
#' @noRd
inspect_identifier <- function(data, id_column) {
    if (is.null(id_column)) {
        return(
            list(
                missing_ids = NULL,
                duplicated_ids = NULL
            )
        )
    }

    if (!id_column %in% names(data)) {
        stop(
            "The requested identifier column was not found: ",
            id_column,
            call. = FALSE
        )
    }

    id_values <- data[[id_column]]

    missing_ids <- is.na(id_values) |
        trimws(as.character(id_values)) == ""

    valid_ids <- id_values[!missing_ids]

    list(
        missing_ids = sum(missing_ids),
        duplicated_ids = sum(duplicated(valid_ids))
    )
}

#' Inspect the untouched external CSV after a permissive read.
#'
#' This inspection records the raw dimensions, column names, and
#' parser diagnostics.
#'
#' @param data A data frame read from the external source snapshot.
#' @return A list of structural and parsing diagnostics.
inspect_external <- function(data) {
    if (!is.data.frame(data)) {
        stop(
            "'data' must be a data frame.",
            call. = FALSE
        )
    }

    list(
        number_of_rows = nrow(data),
        number_of_columns = ncol(data),
        column_names = names(data),
        parsing_problems = problems(data)
    )
}

#' Run data-quality checks on the correctly imported source data.
#'
#' @param data The imported source data.
#' @param id_column Optional identifier column to check for missing values and
#'   duplicates.
#' @param special_values Character values that should be counted as possible
#'   textual representations of missing data.
#' @return A list containing parser, duplication, missingness, and overview
#'   diagnostics.
inspect_imported <- function(
    data,
    id_column = NULL,
    special_values
) {
    if (!is.data.frame(data)) {
        stop(
            "'data' must be a data frame.",
            call. = FALSE
        )
    }

    identifier_summary <- inspect_identifier(
        data,
        id_column
    )

    normalized_data <- normalize_character_data(data)

    search_patterns <- paste0(
        "^",
        tolower(trimws(special_values)),
        "$"
    )

    parsing_problems <- attr(
        data,
        "import_parsing_problems",
        exact = TRUE
    )

    if (is.null(parsing_problems)) {
        parsing_problems <- data.frame()
    }

    list(
        parsing_problems = parsing_problems,
        duplicated_rows = sum(duplicated(data)),
        missing_ids = identifier_summary$missing_ids,
        duplicated_ids = identifier_summary$duplicated_ids,
        dataset_overview = skim_without_charts(data),
        special_values_summary = miss_scan_count(
            normalized_data,
            search = search_patterns
        ),
        missing_values_summary = miss_var_summary(data)
    )
}

#' Inspect the overall quality of a filtered candidate sample.
#'
#' @param data The candidate analysis sample.
#' @return A list with dimensions, a dataset overview, and missing-value counts
#'   for every column.
inspect_sample <- function(data) {
    list(
        number_of_rows = nrow(data),
        number_of_columns = ncol(data),
        dataset_overview = skim_without_charts(data),
        missing_values_summary = miss_var_summary(data)
    )
}

#' Identify missing values in metadata selected for manual enrichment.
#'
#' This targeted check is separate from the general sample inspection so the
#' chosen enrichment fields remain explicit rather than appearing data-driven.
#'
#' @param data The candidate analysis sample.
#' @param metadata_columns Columns selected for documented manual enrichment.
#' @return Rows with at least one missing selected field, restricted to
#'   `app_id`, `name`, and the requested metadata columns.
inspect_metadata_gaps <- function(data, metadata_columns) {
    missing_metadata <- vapply(
        data[metadata_columns],
        function(values) {
            is.na(values) |
                trimws(as.character(values)) == ""
        },
        logical(nrow(data))
    )

    data[
        rowSums(missing_metadata) > 0,
        c("app_id", "name", metadata_columns),
        drop = FALSE
    ]
}

#' Summarize the validation results.
#'
#' Count applications by validation decision and reason, and collect
#' the applications that belong to duplicate-name groups.
#'
#' @param validation_log A validation log produced by `validate_sample()`.
#' @return A list with validation counts, summaries
#' and duplicate-name applications.
inspect_validation <- function(validation_log) {
    decision_summary <- as.data.frame(
        table(validation_log$validation_decision),
        stringsAsFactors = FALSE
    )
    names(decision_summary) <- c(
        "decision",
        "applications"
    )

    reason_summary <- as.data.frame(
        table(validation_log$validation_reason),
        stringsAsFactors = FALSE
    )
    names(reason_summary) <- c(
        "reason",
        "applications"
    )

    duplicate_name_applications <- validation_log[
        validation_log$duplicate_name,
        c(
            "app_id",
            "name",
            "duplicate_name_group_size",
            "canonical_name_app",
            "validation_decision",
            "validation_reason"
        ),
        drop = FALSE
    ]

    list(
        number_of_candidate_rows = nrow(validation_log),
        number_of_validated_rows = sum(
            validation_log$validation_decision == "keep"
        ),
        number_of_excluded_rows = sum(
            validation_log$validation_decision == "exclude"
        ),
        number_of_review_rows = sum(
            validation_log$validation_decision == "review"
        ),
        decision_summary = decision_summary,
        reason_summary = reason_summary,
        duplicate_name_applications =
            duplicate_name_applications
    )
}

box::export(
    inspect_external,
    inspect_imported,
    inspect_sample,
    inspect_metadata_gaps,
    inspect_validation
)
