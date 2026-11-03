# Helpers for the HTML reports and CSV downloads (decision 0003).
# A report is one self-contained HTML file: tables are HTML, plots are PNGs
# embedded as data URIs, so it opens offline and needs no pandoc or LaTeX.
# https://github.com/xlai/dosefinderhub/issues/94

#' A data frame as an HTML table
#' @param df Data frame
#' @param rownames Whether to show the row names as the first column
#' @param digits Numbers are rounded to this many decimal places
report_table <- function(df, rownames = TRUE, digits = 2) {
  df <- as.data.frame(df, check.names = FALSE)
  cells <- lapply(df, function(column) {
    if (is.numeric(column)) as.character(round(column, digits))
    else as.character(column)
  })
  header <- c(if (rownames) "", names(df))
  rows <- lapply(seq_len(nrow(df)), function(i) {
    htmltools::tags$tr(
      if (rownames) htmltools::tags$th(row.names(df)[i]),
      lapply(cells, function(column) htmltools::tags$td(column[i]))
    )
  })
  htmltools::tags$table(
    htmltools::tags$thead(htmltools::tags$tr(lapply(header, htmltools::tags$th))),
    htmltools::tags$tbody(rows)
  )
}

#' A plot as an embedded PNG image
#' @param plot A ggplot (or anything that draws when printed)
#' @param width,height Size in pixels
report_plot <- function(plot, width = 700, height = 450) {
  file <- tempfile(fileext = ".png")
  on.exit(unlink(file), add = TRUE)
  # The same PNG device renderPlot() uses, so it also works in the browser app.
  shiny::plotPNG(function() print(plot), filename = file, width = width, height = height, res = 96)
  htmltools::tags$img(
    src = base64enc::dataURI(file = file, mime = "image/png"),
    alt = plot$labels$title %||% "Plot",
    width = width
  )
}

#' Versions of R and the packages that compute the results
report_versions <- function() {
  packages <- c("escalation", "BOIN", "dfcrm", "shiny")
  data.frame(
    Version = c(paste(R.version$major, R.version$minor, sep = "."),
                vapply(packages, function(p) as.character(utils::packageVersion(p)), "")),
    row.names = c("R", packages),
    check.names = FALSE
  )
}

#' Questionnaire answers as a table, or a sentence when there are none
#' @param answers Named list of answers (question variable = answer)
report_answers <- function(answers) {
  answers <- Filter(Negate(is.null), answers)
  if (length(answers) == 0) {
    return(htmltools::p("The questionnaire was not answered in this session."))
  }
  report_table(data.frame(
    Answer = vapply(answers, function(a) paste(a, collapse = ", "), ""),
    row.names = names(answers),
    check.names = FALSE
  ))
}

#' A whole report as an HTML string
#' @param title Report title
#' @param ... Sections (htmltools tags)
report_page <- function(title, ...) {
  style <- "
    body { font-family: system-ui, sans-serif; max-width: 960px; margin: 2em auto; padding: 0 1em; color: #212529; }
    h1 { font-size: 1.6em; } h2 { font-size: 1.3em; margin-top: 1.6em; border-bottom: 1px solid #dee2e6; } h3 { font-size: 1.05em; }
    table { border-collapse: collapse; margin: 0.5em 0 1em; }
    th, td { border: 1px solid #dee2e6; padding: 4px 8px; text-align: right; }
    thead th, tbody th { background: #f8f9fa; } tbody th { text-align: left; }
    img { max-width: 100%; height: auto; }
  "
  # The head is written by hand: htmltools moves head tags out of a tag tree.
  head <- htmltools::tagList(
    htmltools::tags$meta(charset = "utf-8"),
    htmltools::tags$title(title),
    htmltools::tags$style(htmltools::HTML(style))
  )
  body <- htmltools::tags$body(
    htmltools::h1(title),
    htmltools::p(paste("Created with DoseFinderHub on", format(Sys.time(), "%Y-%m-%d %H:%M %Z"))),
    ...
  )
  paste0("<!DOCTYPE html>\n<html lang=\"en\">\n<head>\n", as.character(head),
         "\n</head>\n", as.character(body), "\n</html>\n")
}

#' Named data frames in one long table, for a CSV download
#' @param tables Named list of data frames; the names become the Table column
report_long_table <- function(tables) {
  rows <- lapply(names(tables), function(name) {
    df <- as.data.frame(tables[[name]], check.names = FALSE)
    if (nrow(df) == 0 || ncol(df) == 0) return(NULL)
    data.frame(
      Table = name,
      Row = rep(row.names(df), times = ncol(df)),
      Column = rep(names(df), each = nrow(df)),
      Value = unlist(lapply(df, as.character), use.names = FALSE),
      check.names = FALSE
    )
  })
  do.call(rbind, rows)
}
