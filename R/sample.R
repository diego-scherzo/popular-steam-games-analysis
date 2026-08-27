## Module responsibility
#
# Select candidate data by applying the study's basic eligibility criteria.
# Final inclusion and exclusion decisions are handled in validate.R.

box::use(
    dplyr[
        between,
        filter
    ]
)

#' Create the candidate sample.
#'
#' Select the applications that meet the configured study criteria and
#' form the sample intended for the analysis.
#'
#' @param data Transformed Steam application data.
#' @param start_year First eligible release year, inclusive.
#' @param end_year Last eligible release year, inclusive.
#' @param minimum_reviews Minimum number of total reviews required.
#' @param paid_at_snapshot_only Whether to retain only applications whose
#'   snapshot price exceeds the configured threshold.
#' @param minimum_snapshot_price_usd Lower price boundary used when
#'   `paid_at_snapshot_only` is `TRUE`.
#' @return The rows that satisfy all configured sample restrictions.
sample_create <- function(
    data,
    start_year,
    end_year,
    minimum_reviews,
    paid_at_snapshot_only,
    minimum_snapshot_price_usd
) {
    candidate_sample <- data |>
        filter(
            between(
                release_year,
                start_year,
                end_year
            ),
            total_reviews >= minimum_reviews
        )

    if (paid_at_snapshot_only) {
        candidate_sample <- candidate_sample |>
            filter(
                snapshot_price_usd >
                    minimum_snapshot_price_usd
            )
    }

    candidate_sample
}

box::export(
    sample_create
)
