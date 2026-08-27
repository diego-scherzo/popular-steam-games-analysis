# Avoid Windows issues with the renv sandbox.
if (identical(.Platform$OS.type, "windows")) {
    Sys.setenv(RENV_CONFIG_SANDBOX_ENABLED = "FALSE")
}

# Let box find the project's local modules when targets runs them.
project_root <- normalizePath(
    ".",
    winslash = "/",
    mustWork = TRUE
)
options(
    box.path = unique(
        c(
            project_root,
            getOption("box.path")
        )
    )
)
rm(project_root)

source("renv/activate.R")
