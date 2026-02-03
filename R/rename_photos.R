#' Rename photo files using an explicit mapping
#'
#' Renames photo files in a folder using a named character vector mapping of
#' `old_filename -> new_filename`. Uses a two-step temporary rename to reduce
#' risk of collisions on case-insensitive filesystems and to avoid partial
#' overwrites.
#'
#' @param folder_path Character scalar. Folder containing the files to rename.
#' @param mapping Named character vector. Names are existing filenames (sources),
#'   values are desired filenames (targets).
#' @param dry_run Logical. If TRUE, perform checks and return the preview table
#'   without renaming anything.
#' @param overwrite Logical. If TRUE, allow overwriting existing target files.
#'   Default FALSE.
#' @param tmp_prefix Character. Prefix used for the temporary filenames.
#' @param verbose Logical. If TRUE, prints a preview and progress messages.
#'
#' @return A data.frame with columns `old_path`, `new_path`, `status`
#'   (returned invisibly).
#'
#' @export
rename_photos <- function(folder_path,
                          mapping,
                          dry_run = FALSE,
                          overwrite = FALSE,
                          tmp_prefix = ".tmp_",
                          verbose = TRUE) {

  # ---- validate inputs ----
  if (!is.character(folder_path) || length(folder_path) != 1L || is.na(folder_path)) {
    stop("`folder_path` must be a single non-missing character string.")
  }
  if (!dir.exists(folder_path)) stop("Folder not found: ", folder_path)

  if (!is.character(mapping) || length(mapping) < 1L) {
    stop("`mapping` must be a non-empty named character vector.")
  }
  if (is.null(names(mapping)) || any(names(mapping) == "")) {
    stop("`mapping` must be named: names = old filenames, values = new filenames.")
  }
  if (anyNA(names(mapping)) || anyNA(unname(mapping))) {
    stop("`mapping` cannot contain NA values.")
  }

  # ---- build paths ----
  old_paths <- file.path(folder_path, names(mapping))
  new_paths <- file.path(folder_path, unname(mapping))

  # ---- checks ----
  missing_old <- !file.exists(old_paths)
  if (any(missing_old)) {
    stop(
      "These source files are missing:\n",
      paste(basename(old_paths[missing_old]), collapse = "\n")
    )
  }

  if (any(duplicated(new_paths))) {
    stop("Target filenames must be unique.")
  }

  existing_targets <- file.exists(new_paths)
  if (any(existing_targets) && !overwrite) {
    stop(
      "These target files already exist:\n",
      paste(basename(new_paths[existing_targets]), collapse = "\n"),
      "\nSet `overwrite = TRUE` to allow overwriting."
    )
  }

  # ---- preview ----
  preview <- data.frame(
    old_path = old_paths,
    new_path = new_paths,
    stringsAsFactors = FALSE
  )

  if (verbose) {
    print(
      data.frame(
        old_name = basename(old_paths),
        new_name = basename(new_paths),
        stringsAsFactors = FALSE
      ),
      row.names = FALSE
    )
  }

  if (dry_run) {
    preview$status <- "dry_run"
    return(preview)
  }

  # ---- two-step rename ----
  tmp_paths <- file.path(folder_path, paste0(tmp_prefix, basename(new_paths)))

  if (overwrite && any(existing_targets)) {
    if (!all(file.remove(new_paths[existing_targets]))) {
      stop("Failed to remove existing target files.")
    }
  }

  if (!all(file.rename(old_paths, tmp_paths))) {
    stop("Temporary rename failed.")
  }

  if (!all(file.rename(tmp_paths, new_paths))) {
    stop("Final rename failed.")
  }

  preview$status <- "renamed"
  if (verbose) message("Photo renaming complete!")

  invisible(preview)
}
