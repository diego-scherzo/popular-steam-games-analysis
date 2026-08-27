## Module responsibility
#
# Integrates documented metadata from external sources into the
# project's data.

#' Complete missing metadata from a versioned override table.
#'
#' Rows are matched by `app_id`. Only missing or blank values in the metadata
#' columns supplied in `metadata_columns` are filled.
#'
#' @param data Candidate analysis data containing `app_id` and metadata fields.
#' @param overrides Manual metadata values keyed by `app_id`.
#' @param metadata_columns Character vector naming the metadata columns eligible
#'   for enrichment.
#' @return `data` with eligible missing metadata completed where available.
enrich_metadata <- function(data, overrides, metadata_columns) {
    override_rows <- match(
        data$app_id,
        overrides$app_id
    )

    for (column in metadata_columns) {
        replacement <- overrides[[column]][override_rows]
        missing_value <- is.na(data[[column]]) |
            trimws(data[[column]]) == ""
        available_replacement <- !is.na(replacement) &
            trimws(replacement) != ""

        rows_to_enrich <- missing_value &
            available_replacement

        data[[column]][rows_to_enrich] <-
            replacement[rows_to_enrich]
    }

    data
}

box::export(
    enrich_metadata
)
