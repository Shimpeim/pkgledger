#' Citation styles supported by pkgledger
#'
#' Maps the user-facing style names to the `style` argument of
#' [utils::format.bibentry()].
#' @noRd
.citation_styles <- c(
  text        = "text",
  bibtex      = "Bibtex",
  latex       = "latex",
  html        = "html",
  textVersion = "textVersion",
  R           = "R"
)

#' Write R and package versions with citations to a text file
#'
#' Collects the running R version and the versions of loaded (or attached)
#' packages, retrieves each one's citation with [utils::citation()], and
#' writes everything to a plain-text report. The report has three parts:
#' a session header (R version, platform, OS), a version summary table,
#' and per-package citations in each requested style. When `"bibtex"` is
#' among `styles`, a consolidated BibTeX block with unique keys
#' (`R-base`, `R-<pkg>`, `R-<pkg>-<i>`) is appended so it can be pasted
#' straight into a `.bib` file.
#'
#' @param file Path of the text file to write.
#' @param packages Character vector of package names to report. `NULL`
#'   (default) uses the packages selected by `scope`.
#' @param scope Which packages to use when `packages` is `NULL`:
#'   `"loaded"` (all loaded namespaces, the default) or `"attached"`
#'   (packages on the search path, i.e. loaded via `library()`).
#' @param styles Citation styles to write. Any of `"text"`, `"bibtex"`,
#'   `"latex"`, `"html"`, `"textVersion"`, `"R"`, or `"all"` for every
#'   style. Default `c("text", "bibtex")`.
#' @param include_base Logical; include base-priority packages
#'   (`stats`, `utils`, ...) in the report? R itself is always included.
#' @param overwrite Logical; overwrite `file` if it already exists?
#'
#' @return Invisibly, a data frame with one row per entry (R first) and
#'   columns `package`, `version`, `source`, and `n_citations`.
#'
#' @examples
#' f <- tempfile(fileext = ".txt")
#' write_package_citations(f, packages = c("stats", "utils"))
#' cat(readLines(f, n = 20), sep = "\n")
#'
#' @export
write_package_citations <- function(file = "package_citations.txt",
                                    packages = NULL,
                                    scope = c("loaded", "attached"),
                                    styles = c("text", "bibtex"),
                                    include_base = TRUE,
                                    overwrite = FALSE) {
  scope <- match.arg(scope)
  if (!is.character(file) || length(file) != 1L || is.na(file)) {
    stop("`file` must be a single file path.", call. = FALSE)
  }
  if (file.exists(file) && !isTRUE(overwrite)) {
    stop("File already exists: ", file,
         "\nUse `overwrite = TRUE` to replace it.", call. = FALSE)
  }
  styles <- .resolve_styles(styles)

  if (is.null(packages)) {
    packages <- if (scope == "loaded") loadedNamespaces() else .packages()
  } else {
    if (!is.character(packages)) {
      stop("`packages` must be a character vector or NULL.", call. = FALSE)
    }
    missing_pkgs <- packages[!vapply(packages, .is_installed, logical(1L))]
    if (length(missing_pkgs) > 0L) {
      stop("Package(s) not installed: ",
           paste(missing_pkgs, collapse = ", "), call. = FALSE)
    }
  }
  packages <- setdiff(unique(packages), "base")
  if (!isTRUE(include_base)) {
    packages <- packages[!vapply(packages, .is_base_pkg, logical(1L))]
  }
  packages <- sort(packages)

  entries <- c(list(.r_entry()), lapply(packages, .pkg_entry))

  lines <- c(
    .header_lines(styles),
    .summary_lines(entries),
    .citation_lines(entries, styles),
    if ("bibtex" %in% styles) .bibtex_block(entries)
  )
  writeLines(enc2utf8(lines), file, useBytes = TRUE)

  out <- data.frame(
    package     = vapply(entries, `[[`, character(1L), "name"),
    version     = vapply(entries, `[[`, character(1L), "version"),
    source      = vapply(entries, `[[`, character(1L), "source"),
    n_citations = vapply(entries, function(e) length(e$citation), integer(1L)),
    stringsAsFactors = FALSE
  )
  message("Wrote versions and citations for R and ", length(packages),
          " package(s) to ", file)
  invisible(out)
}

# ---- helpers ---------------------------------------------------------------

.resolve_styles <- function(styles) {
  if (!is.character(styles) || length(styles) == 0L) {
    stop("`styles` must be a non-empty character vector.", call. = FALSE)
  }
  if ("all" %in% styles) return(names(.citation_styles))
  bad <- setdiff(styles, names(.citation_styles))
  if (length(bad) > 0L) {
    stop("Unknown style(s): ", paste(bad, collapse = ", "),
         ". Choose from: ",
         paste(c(names(.citation_styles), "all"), collapse = ", "),
         call. = FALSE)
  }
  unique(styles)
}

.is_installed <- function(pkg) {
  nzchar(system.file(package = pkg))
}

.is_base_pkg <- function(pkg) {
  identical(utils::packageDescription(pkg, fields = "Priority"), "base")
}

.pkg_source <- function(pkg) {
  d <- suppressWarnings(utils::packageDescription(pkg))
  if (!inherits(d, "packageDescription")) return("local/unknown")
  if (identical(d$Priority, "base")) return("base R")
  if (!is.null(d$RemoteType) && !is.null(d$RemoteRepo)) {
    repo <- paste(c(d$RemoteUsername, d$RemoteRepo), collapse = "/")
    ref <- if (!is.null(d$RemoteSha)) {
      paste0("@", substr(d$RemoteSha, 1L, 7L))
    } else ""
    return(paste0(d$RemoteType, " (", repo, ref, ")"))
  }
  if (!is.null(d$Repository)) return(d$Repository)
  if (!is.null(d$biocViews)) return("Bioconductor")
  "local/unknown"
}

.safe_citation <- function(pkg) {
  # citation() warns when DESCRIPTION lacks Date fields; that is not
  # actionable for the user, so suppress it. Errors become NULL.
  tryCatch(suppressWarnings(utils::citation(pkg)), error = function(e) NULL)
}

.r_entry <- function() {
  list(name     = "R",
       key      = "R-base",
       version  = paste(R.version$major, R.version$minor, sep = "."),
       source   = R.version.string,
       citation = .safe_citation("base"))
}

.pkg_entry <- function(pkg) {
  list(name     = pkg,
       key      = paste0("R-", pkg),
       version  = as.character(utils::packageVersion(pkg)),
       source   = .pkg_source(pkg),
       citation = .safe_citation(pkg))
}

.format_citation <- function(cit, style) {
  if (is.null(cit) || length(cit) == 0L) return(character(0L))
  txt <- tryCatch(
    format(cit, style = .citation_styles[[style]], bibtex = FALSE,
           citMsg = FALSE),
    error = function(e) character(0L)
  )
  txt[nzchar(trimws(txt))]
}

.keyed_citation <- function(entry) {
  cit <- entry$citation
  n <- length(cit)
  for (i in seq_len(n)) {
    cit[i]$key <- if (n == 1L) entry$key else paste0(entry$key, "-", i)
  }
  cit
}

.rule <- function(ch = "=", width = 72L) {
  paste(rep(ch, width), collapse = "")
}

.header_lines <- function(styles) {
  si <- utils::sessionInfo()
  c(.rule(),
    "R package versions and citations (generated by pkgledger)",
    .rule(),
    paste0("Generated : ", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")),
    paste0("R version : ", R.version.string),
    paste0("Platform  : ", R.version$platform),
    paste0("Running   : ", if (is.null(si$running)) "unknown" else si$running),
    paste0("Styles    : ", paste(styles, collapse = ", ")),
    "")
}

.summary_lines <- function(entries) {
  tab <- data.frame(
    Package = vapply(entries, `[[`, character(1L), "name"),
    Version = vapply(entries, `[[`, character(1L), "version"),
    Source  = vapply(entries, function(e) {
      if (e$name == "R") "" else e$source
    }, character(1L)),
    stringsAsFactors = FALSE
  )
  body <- utils::capture.output(print(tab, row.names = FALSE, right = FALSE))
  c(.rule(), "VERSIONS", .rule(), body, "")
}

.citation_lines <- function(entries, styles) {
  out <- c(.rule(), "CITATIONS", .rule())
  for (e in entries) {
    out <- c(out, "", paste0("--- ", e$name, " (", e$version, ") ",
                             .rule("-", max(3L, 60L - nchar(e$name) -
                                              nchar(e$version)))))
    if (is.null(e$citation) || length(e$citation) == 0L) {
      out <- c(out, "  (no citation information available)")
      next
    }
    cit <- .keyed_citation(e)
    for (s in styles) {
      txt <- .format_citation(cit, s)
      if (length(txt) == 0L) txt <- "(not provided)"
      out <- c(out, paste0("[", s, "]"), txt, "")
    }
  }
  c(out, "")
}

.bibtex_block <- function(entries) {
  bib <- unlist(lapply(entries, function(e) {
    if (is.null(e$citation) || length(e$citation) == 0L) return(NULL)
    c(.format_citation(.keyed_citation(e), "bibtex"), "")
  }))
  c(.rule(),
    "CONSOLIDATED BIBTEX (copy into a .bib file)",
    .rule(),
    "",
    bib)
}
