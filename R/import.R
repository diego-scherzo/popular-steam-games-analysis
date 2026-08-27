## Module responsibility
#
# Imports external data used by the pipeline and handles source-specific
# format issues.

box::use(
    readr[
        problems,
        read_csv
    ]
)

# These constants describe a known defect in the frozen upstream CSV.
source_id_column <- "AppID"
malformed_header <- "DiscountDLC count"
corrected_header <- "Discount,DLC count"
header_rows_to_skip <- 1

#' Import the archived Steam dataset after repairing its malformed header.
#'
#' The source snapshot joins `Discount` and `DLC count` in its first line. This
#' function repairs that header in memory, reads the remaining rows with the
#' corrected column names, and rejects any parsing problems.
#'
#' @param path Path to the archived source CSV.
#' @return A tibble containing the imported source data.
import_data <- function(path) {
    if (!file.exists(path)) {
        stop(
            paste0(
                "The dataset was not found here:\n",
                path
            ),
            call. = FALSE
        )
    }

    source_header <- readLines(
        path,
        n = 1,
        warn = FALSE
    )

    if (!grepl(
        malformed_header,
        source_header
    )) {
        stop(
            "The known malformed header was not found.",
            call. = FALSE
        )
    }

    corrected_source_header <- sub(
        malformed_header,
        corrected_header,
        source_header
    )

    column_names <- strsplit(
        corrected_source_header,
        ","
    )[[1]]

    data <- read_csv(
        path,
        skip = header_rows_to_skip,
        col_names = column_names,
        show_col_types = FALSE
    )

    parsing_problems <- as.data.frame(
        problems(data)
    )

    if (nrow(parsing_problems) > 0) {
        stop(
            paste(
                "The dataset contains parsing problems after import.",
                paste(
                    "Number of problems:",
                    nrow(parsing_problems)
                ),
                sep = "\n"
            ),
            call. = FALSE
        )
    }

    # Save parsing problems as a regular data frame before
    # returning the data. This avoids issues when `targets`
    # saves and reloads the dataset from cache.
    attr(data, "problems") <- NULL
    attr(data, "import_parsing_problems") <- parsing_problems

    message(
        "The malformed header '",
        malformed_header,
        "' was corrected during import."
    )

    data
}

box::export(
    import_data,
    source_id_column
)
