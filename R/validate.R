## Module responsibility
#
# Validate candidate records before they enter the final dataset.
# Handle inclusion and exclusion rules and check the resulting data.

box::use(
    stats[ave]
)

#' Test for exact tokens in comma-separated metadata fields.
#'
#' Exact, case-insensitive matching prevents substrings from being mistaken for
#' complete genre, tag, or category labels.
#'
#' @param values A character vector of comma-separated metadata.
#' @param tokens One or more tokens to search for.
#' @return A logical vector indicating whether any requested token is present.
has_token <- function(values, tokens) {
    normalized_tokens <- tolower(
        trimws(
            unlist(tokens, use.names = FALSE)
        )
    )

    vapply(
        values,
        function(value) {
            if (is.na(value) || trimws(value) == "") {
                return(FALSE)
            }

            value_tokens <- strsplit(
                value,
                split = ",",
                fixed = TRUE
            )[[1]]

            any(
                tolower(trimws(value_tokens)) %in%
                    normalized_tokens
            )
        },
        logical(1)
    )
}

#' Normalize application names for duplicate detection.
#'
#' @param names A character vector of application names.
#' @return Lowercase names with surrounding and repeated whitespace removed.
normalize_name <- function(names) {
    gsub(
        "[[:space:]]+",
        " ",
        tolower(trimws(names))
    )
}

#' Validate candidate applications and produce the processed dataset.
#'
#' The validation rules identify early-access entries, non-game software,
#' and duplicate names. Ambiguous cases are sent to manual review rather
#' than being automatically kept or excluded.
#'
#' @param data The candidate sample.
#' @param non_game_genres Exact genre tokens associated with non-game software.
#' @param non_game_tags Exact tag tokens associated with non-game software.
#' @param gameplay_categories Category tokens that provide evidence of gameplay.
#' @param early_access_genre Exact token identifying early-access applications.
#' @param canonical_name_app_ids AppIDs to keep when multiple applications
#'   share the same normalized name.
#' @param exclude_early_access Whether early-access entries should be excluded.
#' @return A list containing `validated_games` and a row-level `validation_log`.
validate_sample <- function(
    data,
    non_game_genres,
    non_game_tags,
    gameplay_categories,
    early_access_genre,
    canonical_name_app_ids,
    exclude_early_access
) {
    non_game_genre <- has_token(
        data$genres,
        non_game_genres
    )
    non_game_tag <- has_token(
        data$tags,
        non_game_tags
    )
    gameplay_category <- has_token(
        data$categories,
        gameplay_categories
    )
    early_access <- has_token(
        data$genres,
        early_access_genre
    )

    normalized_name <- normalize_name(data$name)
    duplicate_name_group_size <- ave(
        rep.int(1L, nrow(data)),
        normalized_name,
        FUN = sum
    )
    duplicate_name <- duplicate_name_group_size > 1

    canonical_app_ids <- as.numeric(
        unlist(
            canonical_name_app_ids,
            use.names = FALSE
        )
    )

    canonical_name_app <- data$app_id %in%
        canonical_app_ids

    # A duplicate group is resolved only if exactly one canonical AppID
    # is configured for it.
    canonical_apps_in_group <- ave(
        as.integer(canonical_name_app),
        normalized_name,
        FUN = sum
    )

    unresolved_duplicate_name <- duplicate_name &
        canonical_apps_in_group != 1
    noncanonical_name_app <- duplicate_name &
        canonical_apps_in_group == 1 &
        !canonical_name_app

    # Mark an application as non-game only when genre and tag agree.
    # Keep mixed cases when a gameplay category suggests it is actually a game.
    metadata_non_game <- non_game_genre &
        non_game_tag &
        !gameplay_category
    partial_non_game_signal <-
        (non_game_genre | non_game_tag) &
        !metadata_non_game

    needs_review <- partial_non_game_signal |
        unresolved_duplicate_name

    # Handle clear exclusions first, then review ambiguous cases.
    validation_decision <- ifelse(
        metadata_non_game,
        "exclude",
        ifelse(
            noncanonical_name_app,
            "exclude",
            ifelse(
                exclude_early_access & early_access,
                "exclude",
                ifelse(
                    needs_review,
                    "review",
                    "keep"
                )
            )
        )
    )

    validation_reason <- ifelse(
        metadata_non_game,
        "non_game_software",
        ifelse(
            noncanonical_name_app,
            "duplicate_name_noncanonical_app",
            ifelse(
                exclude_early_access & early_access,
                "early_access_at_snapshot",
                ifelse(
                    unresolved_duplicate_name,
                    "duplicate_name_requires_review",
                    ifelse(
                        partial_non_game_signal,
                        "partial_non_game_signal",
                        "eligible_released_game"
                    )
                )
            )
        )
    )

    validation_log <- data.frame(
        app_id = data$app_id,
        name = data$name,
        duplicate_name = duplicate_name,
        duplicate_name_group_size =
            as.integer(duplicate_name_group_size),
        canonical_name_app = canonical_name_app,
        early_access_at_snapshot = early_access,
        non_game_genre = non_game_genre,
        non_game_tag = non_game_tag,
        gameplay_category = gameplay_category,
        validation_decision = validation_decision,
        validation_reason = validation_reason,
        stringsAsFactors = FALSE
    )

    validated_games <- data[
        validation_decision == "keep",
        ,
        drop = FALSE
    ]

    list(
        validated_games = validated_games,
        validation_log = validation_log
    )
}

box::export(
    validate_sample
)
