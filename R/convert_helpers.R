# Input validation function
validate_input_output <- function(
  input,
  output,
  extensions = c("R", "r", "qmd", "rmd", "Rmd")
) {
  if (length(input) != 1) {
    stop("`input` must be a single character string (a path)")
  }

  if (!is.null(output) && length(output) != 1) {
    stop("`output` must be a single character string (a path) or NULL")
  }

  # Set output_non_norm path - if NULL, use input path
  # keep non-normalized for display, before normalizing input
  output_non_norm <- output %||% input

  # Normalize input path and check if it exists
  input <- normalizePath(input, mustWork = TRUE)

  # Check file type
  if (!(tools::file_ext(input) %in% extensions)) {
    stop("`input` does not have a valid file extension")
  }

  # Set output path - if NULL, use input path (now normalised)
  output <- output %||% input

  # Create parent directory if it doesn't exist
  output_dir <- dirname(output)
  if (!dir.exists(output_dir) && output_dir != ".") {
    dir.create(output_dir, recursive = TRUE)
  }

  # Create output file if it doesn't exist
  if (!file.exists(output)) {
    file.create(output)
  }

  # Return validated paths
  list(
    input = input,
    output = normalizePath(output),
    output_non_norm = output_non_norm
  )
}

# Main substitution function
penguins_substitute <- function(file_content, output_non_norm) {
  # Define patterns to look for
  pp <- "library\\(['\"]?palmerpenguins['\"]?\\)" # may have "" or '' around it
  dp <- "data\\(['\"]?penguins['\"]?, package\\s?=\\s?['\"]palmerpenguins['\"]\\)"
  pattern <- paste(pp, dp, sep = "|")

  # Check for use of palmerpenguins package
  matches_pp <- any(grepl(pattern, file_content))

  # If palmerpenguins package used, perform substitutions
  if (matches_pp) {
    # Remove references to palmerpenguins
    file_content <- file_content |>
      gsub(pp, "", x = _) |>
      gsub(dp, 'data("penguins", package = "datasets")', x = _)

    # Replace variable names with shorter versions
    file_content <- file_content |>
      gsub("bill_length_mm", "bill_len", x = _, fixed = TRUE) |>
      gsub("bill_depth_mm", "bill_dep", x = _, fixed = TRUE) |>
      gsub("flipper_length_mm", "flipper_len", x = _, fixed = TRUE) |>
      gsub("body_mass_g", "body_mass", x = _, fixed = TRUE)

    # Check for ends_with("_mm") pattern
    ew <- 'ends_with\\(["\']_mm["\']\\)'
    ew_matches <- grep(ew, file_content)

    if (length(ew_matches) > 0) {
      # Format message about substitutions
      ew_matches_str <- paste0(ew_matches, collapse = ", ")
      cond <- length(ew_matches) == 1
      lns <- ifelse(cond, "line", "lines")
      subs <- ifelse(cond, "subsitution is", "substitutions are")

      message(
        paste0(
          '- ends_with("_mm") replaced on ',
          lns,
          " ",
          ew_matches_str,
          " in ",
          output_non_norm
        )
      )
    }

    # Replace ends_with() pattern
    file_content <- file_content |>
      gsub(
        ew,
        'starts_with("flipper_"), starts_with("bill_")',
        x = _
      )
  }

  # Return modified content and match status
  list(
    content = file_content,
    matches = matches_pp
  )
}

# Main function combining validation and substitution
penguins_convert <- function(input, output) {
  paths <- validate_input_output(input, output)

  file_content <- readLines(paths$input, warn = FALSE)

  result <- penguins_substitute(file_content, paths$output_non_norm)

  # Only write if changes were made or if output is different from input
  # This avoids missing EOL getting added to end of otherwise not_changed files
  # Don't want a diff in this case
  if (result$matches) {
    writeLines(result$content, paths$output)
  } else if (paths$input != paths$output) {
    # No modifications, but need to copy to output location
    file.copy(paths$input, paths$output, overwrite = TRUE)
  } # If no matches, and input = output, don't need to do anything

  # Return logical indicating whether file was changed
  invisible(result$matches)
}
