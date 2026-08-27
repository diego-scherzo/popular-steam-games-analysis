# Declarative data and reporting pipeline.
box::use(
    readr[read_csv],
    tarchetypes[tar_quarto],
    targets[
        tar_option_set,
        tar_target
    ],
    yaml[read_yaml],
    R/download[download_external],
    R/enrich[enrich_metadata],
    R/import[
        import_data,
        source_id_column
    ],
    R/inspect[
        inspect_external,
        inspect_imported,
        inspect_metadata_gaps,
        inspect_sample,
        inspect_validation
    ],
    R/pipeline[
        require_completed_validation,
        write_csv_artifact
    ],
    R/sample[sample_create],
    R/transform[transform_import_data],
    R/validate[validate_sample]
)

tar_option_set(
    error = "stop"
)

# Read the config early because report targets use these values
# before the main targets list is created.
project_config <- read_yaml("_variables.yml")

# Files that should trigger report rebuilds when they change.
report_dependencies <- c(
    "_variables.yml",
    "_quarto.yml",
    "_quarto-reports.yml",
    "R/report.R",
    "reports/_metadata.yml",
    "reports/latex"
)

list(
    tar_target(
        variables_file,
        "_variables.yml",
        format = "file"
    ),
    tar_target(
        variables,
        read_yaml(variables_file)
    ),
    tar_target(
        metadata_columns,
        variables$enrichment$metadata_columns
    ),
    tar_target(
        download_module_file,
        "R/download.R",
        format = "file"
    ),
    tar_target(
        import_module_file,
        "R/import.R",
        format = "file"
    ),
    tar_target(
        inspect_module_file,
        "R/inspect.R",
        format = "file"
    ),
    tar_target(
        transform_module_file,
        "R/transform.R",
        format = "file"
    ),
    tar_target(
        sample_module_file,
        "R/sample.R",
        format = "file"
    ),
    tar_target(
        enrich_module_file,
        "R/enrich.R",
        format = "file"
    ),
    tar_target(
        validate_module_file,
        "R/validate.R",
        format = "file"
    ),
    tar_target(
        pipeline_module_file,
        "R/pipeline.R",
        format = "file"
    ),
    tar_target(
        external_file,
        {
            download_module_file
            download_external(
                url = variables$dataset$zenodo_file_url,
                destination = file.path(
                    variables$paths$data$root,
                    variables$paths$data$external,
                    variables$dataset$external_file
                ),
                expected_checksum = variables$dataset$checksum$value,
                algorithm = variables$dataset$checksum$algorithm
            )
        },
        format = "file"
    ),
    tar_target(
        external_data_inspection,
        {
            inspect_module_file
            inspect_external(
                read_csv(
                    external_file,
                    show_col_types = FALSE
                )
            )
        }
    ),
    tar_target(
        imported_data,
        {
            import_module_file
            import_data(external_file)
        }
    ),
    tar_target(
        imported_data_inspection,
        {
            import_module_file
            inspect_module_file
            inspect_imported(
                imported_data,
                id_column = source_id_column,
                special_values = variables$inspection$special_values
            )
        }
    ),
    tar_target(
        transformed_data,
        {
            transform_module_file
            transform_import_data(imported_data)
        }
    ),
    tar_target(
        candidate_sample,
        {
            sample_module_file
            sample_create(
                transformed_data,
                start_year = variables$analysis$start_year,
                end_year = variables$analysis$end_year,
                minimum_reviews = variables$analysis$minimum_reviews,
                paid_at_snapshot_only =
                    variables$analysis$paid_at_snapshot_only,
                minimum_snapshot_price_usd =
                    variables$analysis$minimum_snapshot_price_usd
            )
        }
    ),
    tar_target(
        candidate_sample_inspection,
        {
            inspect_module_file
            inspect_sample(candidate_sample)
        }
    ),
    tar_target(
        metadata_gaps,
        {
            inspect_module_file
            inspect_metadata_gaps(
                candidate_sample,
                metadata_columns
            )
        }
    ),
    tar_target(
        metadata_overrides_file,
        file.path(
            variables$paths$data$root,
            variables$paths$data$manual,
            variables$files$data$manual$metadata_overrides
        ),
        format = "file"
    ),
    tar_target(
        metadata_overrides,
        read_csv(
            metadata_overrides_file,
            show_col_types = FALSE
        )
    ),
    tar_target(
        enriched_candidate_sample,
        {
            enrich_module_file
            enrich_metadata(
                candidate_sample,
                metadata_overrides,
                metadata_columns
            )
        }
    ),
    tar_target(
        validation_result,
        {
            validate_module_file
            validate_sample(
                enriched_candidate_sample,
                non_game_genres =
                    variables$validation$non_game_genres,
                non_game_tags =
                    variables$validation$non_game_tags,
                gameplay_categories =
                    variables$validation$gameplay_categories,
                early_access_genre =
                    variables$validation$early_access_genre,
                canonical_name_app_ids =
                    variables$validation$canonical_name_app_ids,
                exclude_early_access =
                    variables$validation$exclude_early_access
            )
        }
    ),
    tar_target(
        validation_log,
        validation_result$validation_log
    ),
    tar_target(
        validation_summary,
        {
            inspect_module_file
            inspect_validation(validation_log)
        }
    ),
    tar_target(
        candidate_sample_csv,
        {
            pipeline_module_file
            write_csv_artifact(
                candidate_sample,
                file.path(
                    variables$paths$data$root,
                    variables$paths$data$interim,
                    variables$files$data$interim$candidate_sample
                )
            )
        },
        format = "file"
    ),
    tar_target(
        metadata_gaps_csv,
        {
            pipeline_module_file
            write_csv_artifact(
                metadata_gaps,
                file.path(
                    variables$paths$data$root,
                    variables$paths$data$interim,
                    variables$files$data$interim$metadata_gaps
                )
            )
        },
        format = "file"
    ),
    tar_target(
        enriched_candidate_sample_csv,
        {
            pipeline_module_file
            write_csv_artifact(
                enriched_candidate_sample,
                file.path(
                    variables$paths$data$root,
                    variables$paths$data$interim,
                    variables$files$data$interim$enriched_candidate_sample
                )
            )
        },
        format = "file"
    ),
    tar_target(
        validation_log_csv,
        {
            pipeline_module_file
            write_csv_artifact(
                validation_log,
                file.path(
                    variables$paths$data$root,
                    variables$paths$data$interim,
                    variables$files$data$interim$validation_log
                )
            )
        },
        format = "file"
    ),
    tar_target(
        validated_games,
        {
            pipeline_module_file
            require_completed_validation(
                validation_result$validated_games,
                validation_summary,
                variables$validation$maximum_review_rows,
                validation_log_csv
            )
        }
    ),
    tar_target(
        validated_games_csv,
        {
            pipeline_module_file
            write_csv_artifact(
                validated_games,
                file.path(
                    variables$paths$data$root,
                    variables$paths$data$processed$root,
                    variables$files$data$processed$validated_games
                )
            )
        },
        format = "file"
    ),
    tar_quarto(
        readme,
        path = "README-source.qmd",
        output_file = "README.md",
        extra_files = "_variables.yml"
    ),
    tar_quarto(
        report_it,
        path = file.path(
            project_config$paths$reports$root,
            project_config$reports$it$input_file
        ),
        output_file =
            project_config$reports$it$output_file,
        quarto_args = c(
            "--metadata",
            paste0(
                "title:",
                project_config$reports$it$title
            ),
            "--metadata",
            paste0(
                "author:",
                project_config$project$author
            )
        ),
        extra_files = report_dependencies,
        profile = "reports"
    ),
    tar_quarto(
        report_en,
        path = file.path(
            project_config$paths$reports$root,
            project_config$reports$en$input_file
        ),
        output_file =
            project_config$reports$en$output_file,
        quarto_args = c(
            "--metadata",
            paste0(
                "title:",
                project_config$reports$en$title
            ),
            "--metadata",
            paste0(
                "author:",
                project_config$project$author
            )
        ),
        extra_files = report_dependencies,
        profile = "reports"
    )
)
