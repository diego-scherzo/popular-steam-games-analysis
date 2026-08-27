## Module responsibility
#
# Keeps output consistent across the project's Quarto reports.

box::use(
    gt[
        gt,
        fmt_missing,
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
    tidyselect[all_of]
)

#' Format a data frame as a consistent report table.
#'
#' @param data A data frame to display.
#' @param column_labels Optional named list of display labels accepted by
#'   `gt::cols_label()`.
#' @param column_alignments Optional named character vector mapping columns to
#'   alignments such as `"left"` or `"right"`.
#' @param column_widths Optional named numeric vector of percentage widths.
#' @return A `gt` table configured for the project's PDF reports.
report_table <- function(
    data,
    column_labels = NULL,
    column_alignments = NULL,
    column_widths = NULL
) {
    table <- data |>
        gt() |>
        fmt_missing(
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

    if (!is.null(column_labels)) {
        table <- table |>
            cols_label(
                .list = column_labels
            )
    }

    if (!is.null(column_alignments)) {
        # Apply the same alignment to all matching columns.
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
        # Convert the requested column widths to the format expected by `gt`.
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

box::export(
    report_table
)
