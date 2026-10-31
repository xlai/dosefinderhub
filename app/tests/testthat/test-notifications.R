# showNotification() only accepts these types; any other closes the session
# with an error (the cause of the Reset and Save crashes in
# https://github.com/xlai/dosefinderhub/issues/93).

test_that("every showNotification() type in the app is one shiny accepts", {
  allowed <- eval(formals(shiny::showNotification)$type)
  files <- list.files(test_path("..", ".."), pattern = "\\.R$", recursive = TRUE, full.names = TRUE)
  files <- files[!grepl("/tests/", files)]
  types <- unlist(lapply(files, function(file) {
    calls <- list()
    find <- function(e) {
      if (is.call(e)) {
        name <- e[[1]]
        if (is.name(name) && as.character(name) == "showNotification" ||
            is.call(name) && identical(as.character(name)[3], "showNotification")) {
          calls[[length(calls) + 1]] <<- e
        }
        for (part in as.list(e)[-1]) if (!missing(part)) find(part)
      }
    }
    for (e in parse(file, keep.source = FALSE)) find(e)
    unlist(lapply(calls, function(call) if (is.character(call$type)) call$type))
  }))
  expect_gt(length(types), 0)
  expect_true(all(types %in% allowed), label = paste(setdiff(types, allowed), collapse = ", "))
})
