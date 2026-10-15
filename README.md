# pkgledger

Tools supporting R package version management for reproducible analyses.

## Install

```r
# install.packages("remotes")
remotes::install_github("Shimpeim/pkgledger")
```

## Quick start

```r
library(pkgledger)
library(survival)

# R + all loaded namespaces, text + BibTeX citations
write_package_citations("package_citations.txt")

# Only packages attached with library(), every citation style, no base pkgs
write_package_citations("session_refs.txt", scope = "attached",
                        styles = "all", include_base = FALSE,
                        overwrite = TRUE)
```

The report contains:

1. **Header**: generation time, R version, platform, OS.
2. **VERSIONS**: package, version, and source (CRAN, Bioconductor,
   GitHub with short SHA, base R, or local/unknown).
3. **CITATIONS**: per-package blocks in each requested style.
4. **CONSOLIDATED BIBTEX** (when `"bibtex"` is requested): all entries
   with unique keys (`R-base`, `R-<pkg>`, `R-<pkg>-<i>`) ready for a `.bib`.

## Key arguments of `write_package_citations()`

| Argument | Default | Meaning |
|---|---|---|
| `file` | `"package_citations.txt"` | Output path |
| `packages` | `NULL` | Explicit package names; `NULL` = use `scope` |
| `scope` | `"loaded"` | `"loaded"` (all namespaces) or `"attached"` (`library()`-ed) |
| `styles` | `c("text", "bibtex")` | Any of `text`, `bibtex`, `latex`, `html`, `textVersion`, `R`, or `"all"` |
| `include_base` | `TRUE` | Include base-priority packages (`stats`, `utils`, ...) |
| `overwrite` | `FALSE` | Replace an existing file |

Returns (invisibly) a data frame: `package`, `version`, `source`, `n_citations`.
