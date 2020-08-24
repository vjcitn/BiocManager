library(rvest)
library(xml2)
library(lubridate)
library(yaml)

buildrep <- xml2::read_html("https://bioconductor.org/checkResults/")
html_text(html_nodes(buildrep, "li"))

nodes <- html_nodes(buildrep, "div")[-1:-2]
bioc_names <- html_text(html_nodes(nodes, "h3"))
bioc_vers <- gsub("Bioconductor ", "", bioc_names, fixed = TRUE)
names(nodes) <- bioc_vers
last_bioc_dates <- lapply(nodes, function(x) {
    builddate <- grep("Software.*last\\ results|Last",
                      html_text( html_nodes(html_nodes(x, "table"), "li") ),
                      value = TRUE)
    lastdate <- gsub(".*([A-Za-z]{3}\\ [0-9]{1,2},\\ [0-9]{4}).*", "\\1",
                     builddate)
    lastdate <- gsub("ril", "Apr", lastdate, fixed = TRUE)
    format(mdy(lastdate), "%m/%d/%Y")
})
last_run_dates <- last_bioc_dates[rev(names(last_bioc_dates))]

config <- readLines("https://bioconductor.org/config.yaml")
last_runs <- apply(cbind(
    last_run = last_run_dates,
    mran_link = paste(
        "https://mran.microsoft.com/snapshot",
        as.Date(unlist(last_run_dates), "%m/%d/%Y"),
        sep = "/"
    ),
    rspm_link = paste(
        "https://packagemanager.rstudio.com",
        seq_along(last_run_dates),
        sep = "/"
    )
), 1L, c)
last_runs <- list(last_run_dates = last_runs)
last_runs_yaml <- as.yaml(last_runs)
last_run_vector <- capture.output(cat(last_runs_yaml))

update_config <- append(config, last_run_vector,
    after = which(config == "  \"3.11\": \"04/28/2020\""))

bioc_yaml <- "~/test/bioc_config.yaml"
writeLines(update_config, con = bioc_yaml)
readLines(bioc_yaml)

