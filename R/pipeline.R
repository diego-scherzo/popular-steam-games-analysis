## Module responsibility
#
# Provide helper functions used by the targets pipeline to save outputs,
# run pipeline checks, and connect steps across the project.

box::use(
    readr[write_csv]
)

#' Write a pipeline data product as CSV.
#'
#' @param data Data frame to write.
#' @param path Destination path.
#' @return `path`, for use as a `targets` file target.
write_csv_artifact <- function(data, path) {
    dir.create(
        dirname(path),
        recursive = TRUE,
        showWarnings = FALSE
    )

    write_csv(data, path)

    path
}

#' Make sure validation is complete before continuing.
#'
#' Stops the pipeline if too many rows still require manual review.
#'
#' @param validated_games Rows that passed validation.
#' @param validation_summary Summary produced by `inspect_validation()`.
#' @param maximum_review_rows Maximum number of unresolved review rows allowed.
#' @param validation_log_path Path to the validation log.
#' @return `validated_games` when the check passes.
require_completed_validation <- function(
    validated_games,
    validation_summary,
    maximum_review_rows,
    validation_log_path
) {
    if (
        validation_summary$number_of_review_rows >
            maximum_review_rows
    ) {
        stop(
            "Candidate sample validation requires manual review. ",
            "See the validation log: ",
            validation_log_path,
            call. = FALSE
        )
    }

    validated_games
}

box::export(
    write_csv_artifact,
    require_completed_validation
)
