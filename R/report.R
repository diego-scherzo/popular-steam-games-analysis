## Module responsibility
#
# Keeps output consistent across the project's Quarto reports.

box::use(
    gt[
        gt,
        sub_missing,
        fmt_markdown,
        tab_options,
        pct,
        px,
        cols_align,
        cols_label,
        cols_width
    ],
    rlang[
        new_formula,
        sym
    ],
    stats[setNames],
    tidyselect[all_of],
    utils[head]
)

#' Format numbers for Italian report text.
#'
#' @param value A numeric vector.
#' @return A character vector with Italian thousands and decimal separators.
format_integer_it <- function(value) {
    format(
        value,
        big.mark = ".",
        decimal.mark = ",",
        scientific = FALSE,
        trim = TRUE
    )
}

#' Format dates for Italian report text.
#'
#' @param value A value that can be converted to `Date`.
#' @return A character vector with dates written in Italian.
format_date_it <- function(value) {
    date <- as.Date(value)
    italian_months <- c(
        "gennaio", "febbraio", "marzo", "aprile", "maggio", "giugno",
        "luglio", "agosto", "settembre", "ottobre", "novembre", "dicembre"
    )

    paste(
        as.integer(format(date, "%d")),
        italian_months[as.integer(format(date, "%m"))],
        format(date, "%Y")
    )
}

#' Check if values are URLs.
#'
#' @param values Values to check.
#' @return TRUE for URLs, FALSE otherwise.
#' @noRd
is_table_url <- function(values) {
    values <- as.character(values)

    !is.na(values) &
        grepl(
            "^https?://[^[:space:]]+$",
            trimws(values)
        )
}

#' Shorten long text cells in tables.
#'
#' @param data A data frame.
#' @param max_cell_characters Maximum number of characters per text cell,
#'   or `NULL` to keep the full text.
#' @return `data` with long text cells shortened.
#' @noRd
truncate_table_cells <- function(data, max_cell_characters) {
    if (is.null(max_cell_characters)) {
        return(data)
    }

    max_cell_characters <- as.integer(max_cell_characters)
    text_columns <- vapply(
        data,
        function(values) {
            is.character(values) || is.factor(values)
        },
        logical(1)
    )

    data[text_columns] <- lapply(
        data[text_columns],
        function(values) {
            values <- as.character(values)
            too_long <- !is.na(values) &
                nchar(values, type = "chars") > max_cell_characters &
                !is_table_url(values)

            values[too_long] <- paste0(
                substr(
                    values[too_long],
                    1L,
                    max_cell_characters - 3L
                ),
                "..."
            )

            values
        }
    )

    data
}

#' Format a data frame as a consistent report table.
#'
#' @param data A data frame to display.
#' @param column_labels Optional named list of display labels accepted by
#'   `gt::cols_label()`.
#' @param column_alignments Optional named character vector mapping columns to
#'   alignments such as `"left"` or `"right"`.
#' @param column_widths Optional named numeric vector of percentage widths.
#' @param max_cell_characters Maximum number of characters displayed in a text
#'   cell before the remaining content is replaced by `...`. Use `NULL` to
#'   disable truncation.
#' @return A `gt` table configured for the project's PDF reports.
report_table <- function(
    data,
    column_labels = NULL,
    column_alignments = NULL,
    column_widths = NULL,
    max_cell_characters = 80L
) {
    url_rows <- lapply(
        data,
        function(values) {
            if (!is.character(values) && !is.factor(values)) {
                return(integer(0))
            }

            which(is_table_url(values))
        }
    )

    data <- truncate_table_cells(
        data,
        max_cell_characters
    )

    date_columns <- vapply(
        data,
        inherits,
        logical(1),
        what = "Date"
    )

    data[date_columns] <- lapply(
        data[date_columns],
        format,
        format = "%Y-%m-%d"
    )

    for (column in names(url_rows)[lengths(url_rows) > 0L]) {
        rows <- url_rows[[column]]

        data[[column]][rows] <- paste0(
            "[Link](",
            trimws(data[[column]][rows]),
            ")"
        )
    }

    table <- data |>
        gt() |>
        sub_missing(
            missing_text = "\u2014"
        ) |>
        tab_options(
            table.width = pct(100),
            table.font.size = px(10),
            data_row.padding = px(4),
            table.border.top.width = px(1),
            table.border.bottom.width = px(1),
            latex.use_longtable = TRUE
        )

    for (column in names(url_rows)[lengths(url_rows) > 0L]) {
        table <- table |>
            fmt_markdown(
                columns = all_of(column),
                rows = url_rows[[column]]
            )
    }

    if (!is.null(column_labels)) {
        table <- table |>
            cols_label(
                .list = column_labels
            )
    }

    if (!is.null(column_alignments)) {
        for (alignment in unique(column_alignments)) {
            aligned_columns <- names(column_alignments)[
                column_alignments == alignment
            ]

            table <- table |>
                cols_align(
                    align = alignment,
                    columns = all_of(aligned_columns)
                )
        }
    }

    if (!is.null(column_widths)) {
        width_formulas <- Map(
            function(column, width) {
                new_formula(
                    sym(column),
                    pct(width)
                )
            },
            names(column_widths),
            unname(column_widths)
        )

        table <- table |>
            cols_width(
                .list = width_formulas
            )
    }

    table
}

#' Build a small table preview.
#'
#' Shows the first three rows followed by "...".
#' Missing values are shown as `NA`.
#'
#' @param ... Named vectors to display as columns.
#' @return A `gt` table created with `report_table()`.
report_preview_table <- function(...) {
    data <- data.frame(..., check.names = FALSE)
    data <- head(data, 3L)
    data[] <- lapply(
        data,
        function(values) {
            values <- as.character(values)
            values[is.na(values)] <- "NA"
            values[startsWith(values, "[")] <- paste0(
                "\u00a0",
                values[startsWith(values, "[")]
            )
            values
        }
    )

    preview <- rbind(
        data,
        setNames(
            as.list(rep("...", ncol(data))),
            names(data)
        )
    )

    if (ncol(preview) == 2L) {
        preview <- data.frame(
            .preview_left_spacer = rep("", nrow(preview)),
            preview,
            .preview_right_spacer = rep("", nrow(preview)),
            check.names = FALSE
        )

        return(
            report_table(
                preview,
                column_labels = list(
                    .preview_left_spacer = "",
                    .preview_right_spacer = ""
                ),
                column_alignments = setNames(
                    rep("center", ncol(preview)),
                    names(preview)
                ),
                column_widths = setNames(
                    c(15, 35, 35, 15),
                    names(preview)
                )
            )
        )
    }

    report_table(preview)
}

box::export(
    format_date_it,
    format_integer_it,
    report_table,
    report_preview_table
)
