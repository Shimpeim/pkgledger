test_that("default styles write header, versions, text and bibtex", {
  f <- tempfile(fileext = ".txt")
  expect_message(res <- write_package_citations(f, packages = c("stats", "utils")),
                 "Wrote versions")
  txt <- readLines(f)
  expect_true(any(grepl(R.version.string, txt, fixed = TRUE)))
  expect_true(any(txt == "[text]"))
  expect_true(any(txt == "[bibtex]"))
  expect_true(any(grepl("@Manual{R-base,", txt, fixed = TRUE)))
  expect_true(any(grepl("CONSOLIDATED BIBTEX", txt, fixed = TRUE)))
  expect_equal(res$package, c("R", "stats", "utils"))
  expect_equal(res$version[2], as.character(packageVersion("stats")))
})

test_that("all styles are written", {
  f <- tempfile(fileext = ".txt")
  suppressMessages(write_package_citations(f, packages = "stats", styles = "all"))
  txt <- readLines(f)
  for (s in c("text", "bibtex", "latex", "html", "textVersion", "R")) {
    expect_true(any(txt == paste0("[", s, "]")), info = s)
  }
  expect_true(any(txt == "(not provided)"))  # base R has no textVersion
})

test_that("non-bibtex styles omit consolidated block", {
  f <- tempfile(fileext = ".txt")
  suppressMessages(write_package_citations(f, packages = "stats", styles = "text"))
  expect_false(any(grepl("CONSOLIDATED BIBTEX", readLines(f), fixed = TRUE)))
})

test_that("multi-entry citations get suffixed keys", {
  skip_if_not(length(suppressWarnings(citation("testthat"))) >= 1L)
  e <- pkgledger:::.pkg_entry("testthat")
  e$citation <- c(e$citation, e$citation)
  k <- pkgledger:::.keyed_citation(e)
  expect_equal(unlist(k$key), c("R-testthat-1", "R-testthat-2"))
})

test_that("scope and include_base select packages", {
  f <- tempfile(fileext = ".txt")
  res <- suppressMessages(write_package_citations(f, scope = "attached",
                                                  include_base = FALSE))
  expect_false(any(c("stats", "utils", "base") %in% res$package))
  expect_true("testthat" %in% res$package)
  res2 <- suppressMessages(write_package_citations(f, overwrite = TRUE))
  expect_true(all(c("utils", "pkgledger") %in% res2$package))
})

test_that("informative errors", {
  f <- tempfile(fileext = ".txt")
  expect_error(write_package_citations(f, packages = "notARealPkg123"),
               "not installed")
  expect_error(write_package_citations(f, styles = "apa"), "Unknown style")
  writeLines("x", f)
  expect_error(write_package_citations(f, packages = "stats"), "already exists")
})
