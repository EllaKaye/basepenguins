# Main substitution function
penguins_substitute <- function(file_content, output_short) {
  # Define patterns to look for
  pp <- "library\\(['\"]?palmerpenguins['\"]?\\)" # may have "" or '' around it
  bl <- "bill_length_mm"
  bd <- "bill_depth_mm"
  fl <- "flipper_length_mm"
  bm <- "body_mass_g"
  ew <- 'ends_with\\(["\']_mm["\']\\)'

  patterns <- c(pp, bl, bd, fl, bm, ew)
  pattern <- paste0(patterns, collapse = "|")

  # Check if any patterns match
  matches <- any(grepl(pattern, file_content))

  # If matches found, perform substitutions
  if (matches) {
    # Remove call(s) to palmerpenguins library
    file_content <- gsub(pp, "", file_content)

    # Replace variable names with shorter versions
    file_content <- file_content |>
      gsub(bl, "bill_len", x = _, fixed = TRUE) |>
      gsub(bd, "bill_dep", x = _, fixed = TRUE) |>
      gsub(fl, "flipper_len", x = _, fixed = TRUE) |>
      gsub(bm, "body_mass", x = _, fixed = TRUE)

    # Check for ends_with("_mm") pattern
    ew_matches <- grep(ew, file_content)

    if (length(ew_matches) > 0) {
      # Format message about substitutions
      ew_matches_str <- paste0(ew_matches, collapse = ", ")
      cond <- length(ew_matches) == 1
      lns <- ifelse(cond, "line", "lines")
      subs <- ifelse(cond, "subsitution is", "substitutions are")

      message(
        paste0(
          '- In ',
          output_short,
          ', ends_with("_mm") replaced on ',
          lns,
          " ",
          ew_matches_str,
          " - ",
          "please check that the ",
          subs,
          " appropriate."
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
    matches = matches
  )
}

# Main function combining validation and substitution
penguins_convert <- function(input, output) {
  # Validate inputs
  paths <- validate_penguins_input(input, output)

  # Read file content
  file_content <- readLines(paths$input)

  # Perform substitutions
  result <- penguins_substitute(file_content, paths$output_short)

  # Write output
  writeLines(result$content, paths$output)

  # Return logical indicating whether file was changed
  invisible(result$matches)
}
