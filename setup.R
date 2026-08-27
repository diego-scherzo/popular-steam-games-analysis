if (!file.exists("renv.lock")) {
    stop(
        "'renv.lock' was not found. Run this script from the project root.",
        call. = FALSE
    )
}

if (!requireNamespace("renv", quietly = TRUE)) {
    message("Installing renv...")

    install.packages(
        "renv",
        repos = "https://cloud.r-project.org"
    )
}

message("Restoring R dependencies...")

renv::restore(prompt = FALSE)

message("Project setup completed successfully.")
