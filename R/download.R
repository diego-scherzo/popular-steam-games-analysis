## Module responsibility
#
# Manages the retrieval of data from external sources.

box::use(
    digest[digest],
    utils[download.file]
)

#' Verify that a file matches its recorded checksum.
#'
#' @param path Path to the file to verify.
#' @param expected_checksum Checksum recorded for the archived snapshot.
#' @param algorithm Hash algorithm accepted by `digest::digest()`.
#' @return `path`, invisibly. A mismatch raises an error.
verify_checksum <- function(
    path,
    expected_checksum,
    algorithm
) {
    actual_checksum <- digest(
        path,
        algo = algorithm,
        serialize = FALSE,
        file = TRUE
    )

    if (!identical(
        tolower(actual_checksum),
        tolower(trimws(expected_checksum))
    )) {
        stop(
            paste(
                "The dataset does not match the expected snapshot.",
                paste("Checksum algorithm:", algorithm),
                paste("Expected checksum:", expected_checksum),
                paste("Actual checksum:", actual_checksum),
                sep = "\n"
            ),
            call. = FALSE
        )
    }

    invisible(path)
}

#' Download and verify the archived source dataset.
#'
#' Use the local copy when it is already available and valid.
#' Otherwise, download the file and verify its checksum before saving it.
#'
#' @param url URL of the archived source file.
#' @param destination Local path for the verified dataset.
#' @param expected_checksum Checksum recorded for the archived snapshot.
#' @param algorithm Hash algorithm used for checksum verification.
#' @return The local path to the verified dataset.
download_external <- function(
    url,
    destination,
    expected_checksum,
    algorithm
) {
    dir.create(
        dirname(destination),
        recursive = TRUE,
        showWarnings = FALSE
    )

    if (file.exists(destination)) {
        verify_checksum(
            destination,
            expected_checksum,
            algorithm
        )

        message(
            "Dataset already available and checksum verified: ",
            destination
        )

        return(destination)
    }

    output_dir <- dirname(destination)

    temporary_file <- tempfile(
        pattern = paste0(basename(destination), "-"),
        tmpdir = output_dir,
        fileext = ".download"
    )

    on.exit(
        unlink(temporary_file),
        add = TRUE
    )

    message(
        "Downloading the archived dataset snapshot from Zenodo..."
    )

    tryCatch(
        download.file(
            url = url,
            destfile = temporary_file,
            mode = "wb",
            quiet = TRUE
        ),
        error = function(error) {
            stop(
                paste(
                    "Dataset download failed.",
                    conditionMessage(error),
                    sep = "\n"
                ),
                call. = FALSE
            )
        }
    )

    verify_checksum(
        temporary_file,
        expected_checksum,
        algorithm
    )

    if (!file.rename(temporary_file, destination)) {
        stop(
            paste(
                "The downloaded file could not be moved",
                "to its final destination:",
                destination
            ),
            call. = FALSE
        )
    }

    message(
        "Dataset downloaded and checksum verified: ",
        destination
    )

    destination
}

box::export(
    download_external
)
