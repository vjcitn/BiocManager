#' Check whether your Bioconductor version has a snapshot
#'
#' This function provides the version map located in
#' \url{https://bioconductor.org/config.yaml} for a particular Bioconductor
#' version. For out-of-date Bioconductor versions, it is recommended that
#' the user use an appropriate snapshot of the packages in CRAN history that
#' coincides with the last-built date of that Bioconductor version. `snapshot`
#' gives the user a `data.frame` of MRAN snapshots and RSPM repositories
#' available for a particular Bioconductor version. Note that there can only
#' be one CRAN repository set at a time. Recent versions of Bioconductor
#' do not need to use a snapshot and the default CRAN repository is
#' recommended.
#'
#' @details To make persistent changes across sessions to `options("repos")`,
#'     it is recommended that users enter the adequate CRAN snapshot in
#'     their `.Rprofile` file as given by `snapshot`. This file is usually
#'     located under the user's home directory, see `normalizePath("~")`. The
#'     `options()` function call should look like the following:
#'     \preformatted{
#'         options(
#'             repos = c(CRAN = "https://mran.microsoft.com/snapshot/date")
#'         )
#'     }
#'
#' @section MRAN:
#'     The Microsoft R Archive Network provides a historical repository of CRAN
#'     packages by date. The date in the snapshot link is formatted as:
#'     \preformatted{
#'         https://mran.microsoft.com/snapshot/YYYY-MM-DD
#'     }
#'
#' @param version character(1) A Bioconductor version number, as given by
#'     `BiocManager::version()`.
#'
#' @return A `data.frame` of mapped R and Bioconductor versions and their
#'     respective snapshot links where available
#'
#' @md
#'
#' @examples
#'
#' snapshot("3.2")
#'
#' @export
snapshot <-
    function(version = BiocManager::version())
{
    map <- .version_map()
    if (identical(map, .VERSION_MAP_SENTINEL))
        return(.VERSION_MAP_UNABLE_TO_VALIDATE)

    map[map$Bioc == version, ]
}
