## Module responsibility
#
# Transform imported data into the standard structure used by the project.
# Keep data transformations and derived variables in this module.

box::use(
    dplyr[
        if_else,
        mutate,
        select
    ],
    janitor[clean_names],
    readr[
        locale,
        parse_date
    ]
)

# Settings that describe how values are stored in the archived CSV.
release_date_format <- "%b %d, %Y"
release_date_locale <- "en"
metacritic_missing_value <- 0
list_delimiter <- ","
empty_list_value <- "[]"

#' Count the items stored in comma-separated list fields.
#'
#' Missing or blank values stay missing, while an explicit empty list
#' is counted as zero.
#'
#' @param values A character vector containing comma-separated lists.
#' @return An integer vector with the number of items in each value.
count_list_items <- function(values) {
    vapply(
        values,
        function(value) {
            if (is.na(value) || trimws(value) == "") {
                return(NA_integer_)
            }

            if (trimws(value) == empty_list_value) {
                return(0L)
            }

            length(
                strsplit(
                    value,
                    split = list_delimiter,
                    fixed = TRUE
                )[[1]]
            )
        },
        integer(1)
    )
}

#' Prepare the imported Steam data for the rest of the pipeline.
#'
#' Clean column names, standardize dates and missing values, create a few
#' derived measures, and keep the variables used in the processed dataset.
#'
#' @param data The imported Steam dataset with its original column names.
#' @return A transformed tibble ready for candidate-sample filtering.
transform_import_data <- function(data) {
    cleaned_data <- data |>
        clean_names()

    transformed_data <- cleaned_data |>
        mutate(
            release_date = parse_date(
                release_date,
                format = release_date_format,
                locale = locale(release_date_locale)
            ),
            release_year = as.integer(
                format(
                    release_date,
                    "%Y"
                )
            ),
            metacritic_score = if_else(
                metacritic_score ==
                    metacritic_missing_value,
                NA_real_,
                as.numeric(metacritic_score)
            ),
            supported_language_count = count_list_items(
                supported_languages
            ),
            full_audio_language_count = count_list_items(
                full_audio_languages
            ),
            positive_reviews = positive,
            negative_reviews = negative,
            total_reviews = positive_reviews + negative_reviews,
            positive_review_rate = if_else(
                total_reviews > 0,
                positive_reviews / total_reviews,
                NA_real_
            )
        ) |>
        select(
            app_id,
            name,
            release_date,
            release_year,
            previous_day_peak_ccu = peak_ccu,
            snapshot_price_usd = price,
            snapshot_discount_percent = discount,
            dlc_count,
            supported_languages,
            supported_language_count,
            full_audio_languages,
            full_audio_language_count,
            windows,
            mac,
            linux,
            metacritic_score,
            positive_reviews,
            negative_reviews,
            total_reviews,
            positive_review_rate,
            achievement_count = achievements,
            steam_recommendations = recommendations,
            developers,
            publishers,
            categories,
            genres,
            tags
        )

    transformed_data
}

box::export(
    transform_import_data
)
