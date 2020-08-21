.repo_mran_link <-
    function(version)
{
    map <- .version_map()
    if (identical(map, .VERSION_MAP_SENTINEL))
        return(.VERSION_MAP_UNABLE_TO_VALIDATE)

    mran_link <- map$MRAN[map$Bioc == version]

    if (is.na(mran_link))
        warning(sprintf(
            "MRAN for Bioconductor version '%s' is not supported",
            version
        ))
    mran_link
}

.repo_get_snapdate <-
    function(version)
{
    map <- .version_map()
    if (identical(map, .VERSION_MAP_SENTINEL))
        return(.VERSION_MAP_UNABLE_TO_VALIDATE)

    datetxt <- map$BiocLastDate[map$Bioc == version]
    as.Date(datetxt, "%m/%d/%Y")
}

.repo_check_snap <-
    function(repos, version)
{
    pattern <- "/(20[[:digit:]]{2}-[[:digit:]]{2}-[[:digit:]]{2})/*$"
    is_snapshot <- grepl(pattern, repos)
    has_snap <- grepl("snapshot", repos)

    if (length(is_snapshot) != 1L || length(has_snap) != 1L)
        .stop("More than one CRAN snapshot in 'getOption(\"repos\")'")

    snap_miss <- has_snap & !is_snapshot
    if (snap_miss) {
        .warning(
            paste(
                "Malformed CRAN snapshot location in 'getOption(\"repos\")'.",
                "See ?snapshot for details."
            )
        )
    }

    full_snap <- has_snap & is_snapshot
    if (full_snap) {
        snaprepo <- repos[full_snap]
        reposnap <- as.Date(basename(snaprepo), "%Y-%m-%d")
        snapdate <- .repo_get_snapdate(version)
        if (!length(snapdate) || any(is.na(snapdate)))
            .stop(
                "No CRAN snapshot available for Bioconductor '%s'",
                version
            )
        else if (!identical(snapdate, reposnap)) {
            fmt <- paste0(
                "Out-of-date Bioconductor version detected. ",
                "See ?snapshot for details."
            )
            .warning(fmt, call. = FALSE, wrap. = FALSE)
        }
    }

    repos
}

.repositories_cache_file <-
    function(cachedir = getOption("BiocManagerCache"))
{
    if (is.null(cachedir)) {
        cachedir <- if (.get_R_version() >= '4.0.0')
                tools::R_user_dir("BiocManager", "cache")
        else
            normalizePath(file.path("~", ".cache", "R", "BiocManager"))
    }
    if (!dir.exists(cachedir)) {
        qtxt <- sprintf(
            "Create BiocManager cache at \n    %s? [y/n]: ",
            cachedir
        )
        answer <- .getAnswer(qtxt, allowed = c("y", "Y", "n", "N"))
        if (identical(answer, "y"))
            dir.create(cachedir, recursive = TRUE, showWarnings = FALSE)
    }

    file.path(cachedir, "cache_warn.rda")
}

.make_data <- function(response = NA_character_) {
    data.frame(
        version = BiocManager::version(),
        R_version = .get_R_version(),
        date = as.Date(format(Sys.time(), "%Y-%m-%d")),
        response = response,
        stringsAsFactors = FALSE
    )
}

.repositories_cache_data <- function(cachefile) {
    if (file.exists(cachefile)) {
        dat <- new.env(parent = emptyenv())
        load(cachefile, envir = dat)
        last_warn <- dat[["last_warn"]]
        erow <- last_warn[["version"]] == version() &
            last_warn[["R_version"]] == .get_R_version()
        if (!length(last_warn[erow, "date"]))
            last_warn <- rbind(last_warn, .make_data())
    } else {
        last_warn <- .make_data()
    }
    last_warn
}

.repositories_cache_frame <- function(cachefile, column = TRUE) {
    last_warn <- .repositories_cache_data(cachefile)
    erow <- last_warn[["version"]] == version() &
        last_warn[["R_version"]] == .get_R_version()
    last_warn[erow, column]
}

.repositories_warn_cachedate <- function(cachefile) {
    nowDate <- as.Date(format(Sys.time(), "%Y-%m-%d"))
    cachedf <- .repositories_cache_frame(cachefile)
    futureDate <- as.Date(
        as.numeric(cachedf[["date"]]) + 7L, origin = "1970-01-01"
    )
    nowDate > futureDate || is.na(cachedf[["response"]])
}

.repositories_store_response <- function(response, cachefile) {
    last_warn <- .repositories_cache_frame(cachefile)
    last_warn[, "response"] <- response
    save(last_warn, file = cachefile, compress = FALSE)
}

.repositories_check_repos <-
    function(repos)
{
    version <- version()
    conflict <- names(repos) %in% names(.repositories_bioc(version))
    conflicts <- repos[conflict]
    has_cran <- names(repos) %in% "CRAN"
    cranflicts <- has_cran & repos != "@CRAN@"

    if (sum(has_cran) > 1L)
        .stop("More than one CRAN repository in 'getOption(\"repos\")'")

    rename <- repos == "@CRAN@"
    repos[rename] <- "https://cran.rstudio.com"

    outofdate <- .version_is(version, .get_R_version(), "out-of-date")
    if (outofdate) {
        cachefile <- .repositories_cache_file()
        warn <- .repositories_warn_cachedate(cachefile)
        if (warn) {
            txt <- paste0("Out-of-date Bioconductor installation detected,",
                "\n  would you like to use MRAN snapshots? [y/n]: ")
            resp <- .getAnswer(txt, allowed = c("y", "Y", "n", "N"))
            .repositories_store_response(resp, cachefile)
            if (identical(resp, "n"))
                .warning(paste(
                    "CRAN snapshot repository not used for out-of-date",
                    " Bioconductor version %s, see '?snapshot'"
                    ), version
                )
        } else {
            resp <- .repositories_cache_frame(cachefile, "response")
        }
        if (identical(resp, "y"))
            repos[has_cran] <- .repo_mran_link(version)
    }

    if (any(cranflicts))
        repos[cranflicts] <- .repo_check_snap(repos[cranflicts], version)


    if (length(conflicts)) {
        txt <- paste(
            "'getOption(\"repos\")' replaces Bioconductor standard ",
            "repositories, see '?repositories' for details"
        )
        fmt <- paste0(
            .msg(txt, exdent = 0),
            "\n\nreplacement repositories:",
            "\n    %s\n"
        )
        repos_string <- paste0(
            names(conflicts), ": ", unname(conflicts),
            collapse = "\n    "
        )

        FUN <- ifelse(
            getOption("BiocManager.check_repositories", TRUE),
            .stop, .message
        )
        FUN(fmt, repos_string, call. = FALSE, wrap. = FALSE)
    }

    repos
}

.repositories_base <-
    function()
{
    repos <- getOption("repos")
    repos <- .repositories_check_repos(repos)
    repos
}

#' @importFrom stats setNames
.repositories_bioc <-
    function(version)
{
    mirror <- getOption("BioC_mirror", "https://bioconductor.org")
    paths <- c(
        BioCsoft = "bioc", BioCann = "data/annotation",
        BioCexp = "data/experiment", BioCworkflows = "workflows"
    )
    bioc_repos <- paste(mirror, "packages", version, paths, sep="/")
    setNames(bioc_repos, names(paths))
}

.repositories_filter <-
    function(repos)
{
    urls <- paste0(contrib.url(repos), "/PACKAGES")
    online <- vapply(urls, .url_exists, logical(1))
    repos[online]
}

.repositories <-
    function(site_repository, version)
{
    base <- .repositories_base()
    bioc <- .repositories_bioc(version)

    repos <- c(site_repository = site_repository, bioc, base)
    repos[!duplicated(names(repos))]
}

#' Display current Bioconductor and CRAN repositories.
#'
#' `repositories()` reports the URLs from which to install
#' _Bioconductor_ and CRAN packages. It is used by
#' `BiocManager::install()` and other functions.
#'
#' @param site_repository (Optional) `character(1)` representing an
#'     additional repository (e.g., a URL to an organization's
#'     internally maintained repository) in which to look for packages
#'     to install. This repository will be prepended to the default
#'     repositories returned by the function.
#' @param version (Optional) `character(1)` or `package_version`
#'     indicating the _Bioconductor_ version (e.g., "3.8") for which
#'     repositories are required.
#'
#' @details
#'
#' _Bioconductor_ has a 'release' and a 'devel' semi-annual release
#' cycle. Packages within a release have been tested against each
#' other and the current version of packages on CRAN. _Bioconductor_
#' best practice is to use packages from the same release, and from
#' the appropriate CRAN repository. `repositories()` returns the
#' appropriate package repositories for your version of
#' _Bioconductor_.
#'
#' It may be desireable to specify different default repositories,
#' especially CRAN, for intentionally out-of-date _Bioconductor_
#' releases (e.g., to support reproducible research). Use the approach
#' provided by base _R_ to specify alternative repositories, e.g.,
#' `options(repos = c(CRAN =
#' "https://mran.microsoft.com/snapshot/2020-02-08"))`. This is
#' supported, but generates a warning because specification of an
#' inappropriate CRAN repository (one providing packages not
#' consistent with the dates of the _Bioconductor_ release) results in
#' use of CRAN packages not consistent with _Bioconductor_ best
#' practices.
#'
#' If alternative default repositories are known to provide
#' appropriate version of CRAN or _Bioconductor_ packages, the warning
#' may be silenced (displayed as a message) with
#' `options(BiocManager.check_repositories = FALSE)`. A message is
#' still printed, to serve as a reminder when debugging problems
#' related to incompatible pacakge installation.
#'
#' The intended use of `site_repository =` is to enable installation of
#' packages not available in the default repositories, e.g., packages
#' internal to an organization and not yet publicly available. A
#' secondary use might provide alternative versions (e.g., compiled
#' binaries) of packages available in the default repositories. Note
#' that _R_'s standard rules of package selection apply, so the most
#' recent version of candidate packages is selected independent of the
#' location of the repository in the vector returned by `repositories()`.
#'
#' For greater flexiblity in installing packages while still adhering
#' as much as possible to _Bioconductor_ best practices, use
#' `repositories()` as a basis for constructing the `repos =` argument
#' to `install.packages()` and related functions.
#'
#' @return Named `character()` of repositories.
#'
#' @seealso
#'
#' `BiocManager::\link{install}()` Installs or updates Bioconductor,
#'  CRAN, and GitHub packages.
#'
#' `\link{chooseBioCmirror}()` choose an alternative Bioconductor
#' mirror; not usually necessary.
#'
#' `\link{chooseCRANmirror}()` choose an alternative CRAN mirror; not
#' usually necessary.
#'
#' `\link{setRepositories}()` Select additional repositories for
#' searching.
#'
#' @keywords environment
#'
#' @examples
#' BiocManager::repositories()
#' \dontrun{
#' BiocManager::repositories(version="3.8")
#' }
#'
#' @md
#' @export repositories
repositories <-
    function(site_repository = character(), version = BiocManager::version())
{
    stopifnot(
        length(site_repository) <= 1L,
        is.character(site_repository), !anyNA(site_repository)
    )
    version <- .version_validate(version)
    .repositories(site_repository, version)
}
