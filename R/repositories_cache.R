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

.repositories_make_cache <- function(response = NA_character_) {
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
            last_warn <- rbind(last_warn, .repositories_make_cache())
    } else {
        last_warn <- .repositories_make_cache()
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

