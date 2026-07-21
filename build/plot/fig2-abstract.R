# Timeseries of evolving mutation rate experiments.
# 2-page PDF:
#   Page 1: Best fitness vs update, faceted by change rate, colored by selection treatment
#   Page 2: Average mutation rate vs update, faceted by change rate, colored by selection treatment
# Selection treatments (3 lines per facet): Tournament 3, Tournament 6, Lexicase
#
# File naming: em_t{tourny}_c{change}_{seed}.csv

.libPaths(c("~/R/library", .libPaths()))

library(data.table)
library(dplyr)
library(ggplot2)

# ---------------------------------------------------------------------
# CONFIG
# ---------------------------------------------------------------------
DATA_DIR <- Sys.getenv("DATA_DIR", unset = "/mnt/scratch/suzuekar/data/")
LEXICASE_DATA_DIR <- Sys.getenv("LEXICASE_DATA_DIR", unset = "/mnt/scratch/suzuekar/data/")
OUT_DIR <- "."

SUBSAMPLE_STEP <- 1000
LOG_FLOOR <- 1e-6
MAX_REPS <- as.numeric(Sys.getenv("MAX_REPS", unset = Inf))

OUT_NAME_FITNESS <- "fig2_abstract_fitness.png"
OUT_NAME_FITNESS_FREE <- "fig2_abstract_free_fitness.png"
OUT_NAME_MUTATION <- "fig2_abstract_mutation.png"
OUT_NAME_MUTATION_FREE <- "fig2_abstract_free_mutation.png"

# Leave unset to plot all mutation rates, 
# otherwise specify a string of values separated by commas (no spaces!)
CHANGE_RATES_RAW <- Sys.getenv("FIG2_CHANGE_RATES", unset = "")
CHANGE_RATE_FILTER <- if (CHANGE_RATES_RAW != "") as.numeric(strsplit(CHANGE_RATES_RAW, ",")[[1]]) else NULL

REGEX_PATTERN <- "^em_(t[^_]+|lexicase)_c([^_]+)_([0-9]+)\\.csv$"

FITNESS_COL <- "Fittest Organism Selected Fitness"
MUTRATE_COL <- "Average Mutation Rate"
COLS_NEEDED <- c("Update", FITNESS_COL, MUTRATE_COL)

TOURNY_LABELS <- c("2" = "Tournament 2", "3" = "Tournament 3", "6" = "Tournament 6", "10" = "Tournament 10", "lexicase" = "Lexicase")

CACHE <- file.path(OUT_DIR, "fig2_abstract_cache.csv")
USE_CACHE <- as.logical(Sys.getenv("USE_CACHE", unset = file.exists(CACHE)))

# ---------------------------------------------------------------------
# HELPER FUNCTIONS
# ---------------------------------------------------------------------
parse_filename <- function(file) {
    nm <- basename(file)
    parts <- regmatches(nm, regexec(REGEX_PATTERN, nm))[[1]]
    if (length(parts) == 0) { print(paste("Error parsing filename:", nm)); return(NULL) }
    raw <- parts[2]
    list(
        tourny_size = if (raw == "lexicase") "lexicase" else sub("^t", "", raw),
        change_per_update = as.double(parts[3]),
        seed = as.integer(parts[4])
    )
}

read_timeseries_rows <- function(file) {
    meta <- parse_filename(file)
    if (is.null(meta)) return(NULL)
    fread(file, select = COLS_NEEDED) %>%
        filter(Update %% SUBSAMPLE_STEP == 0) %>%
        mutate(tourny_size = meta$tourny_size,
               change_per_update = meta$change_per_update,
               seed = meta$seed)
}

subsample_files <- function(files, parse_fn) {
    if (is.infinite(MAX_REPS)) return(files)
    meta <- lapply(files, parse_fn)
    keep <- !sapply(meta, is.null)
    files <- files[keep]; meta <- meta[keep]
    dt <- data.table(file = files,
                     tourny_size = sapply(meta, `[[`, "tourny_size"),
                     seed = sapply(meta, `[[`, "seed"))
    dt[, .SD[seed %in% head(sort(unique(seed)), MAX_REPS)], by = tourny_size]$file
}

compile_timeseries <- function(files) {
    lapply(files, read_timeseries_rows) %>%
        Filter(Negate(is.null), .) %>%
        rbindlist()
}

aggregate_ts <- function(ts_data, value_col) {
    ts_data %>%
        rename(value = all_of(value_col)) %>%
        group_by(tourny_size, change_per_update, Update) %>%
        summarise(med = median(value, na.rm = TRUE),
                  lo = quantile(value, 0.25, na.rm = TRUE),
                  hi = quantile(value, 0.75, na.rm = TRUE),
                  .groups = "drop") %>%
        mutate(across(c(med, lo, hi), ~ pmax(.x, LOG_FLOOR)),
               tourny_label = recode(tourny_size, !!!TOURNY_LABELS),
               tourny_label = factor(tourny_label, levels = TOURNY_LABELS))
}

make_ts_plot <- function(summary_df, y_label, free_y = FALSE) {
    ggplot(summary_df, aes(x = Update, y = med, color = tourny_label, fill = tourny_label)) +
        geom_ribbon(aes(ymin = lo, ymax = hi), alpha = 0.2, color = NA) +
        geom_line() +
        scale_y_log10() +
        facet_wrap(~ change_per_update,
                   scales = if (free_y) "free_y" else "fixed",
                   labeller = label_both) +
        labs(x = "Update", y = y_label, color = NULL, fill = NULL) +
        theme_minimal()
}

# ---------------------------------------------------------------------
# RUN
# ---------------------------------------------------------------------
if (USE_CACHE) {
    cat("Loading cached data from", CACHE, "\n")
    all_ts <- fread(CACHE, colClasses = list(character = "tourny_size"))
} else {
    files <- list.files(c(DATA_DIR, LEXICASE_DATA_DIR), pattern = REGEX_PATTERN, full.names = TRUE)
    cat("Found", length(files), "files\n")
    files <- subsample_files(files, parse_filename)
    cat("After replicate subsampling:", length(files), "files\n")

    all_ts <- compile_timeseries(files)
    fwrite(all_ts, CACHE)
    cat("Wrote cache to", CACHE, "\n")
}

if (!is.null(CHANGE_RATE_FILTER)) {
    all_ts <- all_ts %>% filter(change_per_update %in% CHANGE_RATE_FILTER)
    cat("Filtered to change rates:", paste(CHANGE_RATE_FILTER, collapse = ", "), "\n")
}

fit_summary <- aggregate_ts(all_ts, FITNESS_COL)
mutrate_summary <- aggregate_ts(all_ts, MUTRATE_COL)

out_fit_path <- file.path(OUT_DIR, OUT_NAME_FITNESS)
out_free_fit_path <- file.path(OUT_DIR, OUT_NAME_FITNESS_FREE)
out_mut_path <- file.path(OUT_DIR, OUT_NAME_MUTATION)
out_free_mut_path <- file.path(OUT_DIR, OUT_NAME_MUTATION_FREE)
ggsave(out_fit_path, make_ts_plot(fit_summary, y_label = "Best fitness"),
       width = 16, height = 10, dpi = 300, bg = "white")

ggsave(out_mut_path, make_ts_plot(mutrate_summary, y_label = "Average mutation rate"),
       width = 16, height = 10, dpi = 300, bg = "white")
cat("Wrote", out_fit_path, "and", out_mut_path, "\n")

ggsave(out_free_fit_path, make_ts_plot(fit_summary, y_label = "Best fitness", free_y = TRUE),
       width = 16, height = 10, dpi = 300, bg = "white")

ggsave(out_free_mut_path, make_ts_plot(mutrate_summary, y_label = "Average mutation rate", free_y = TRUE),
       width = 16, height = 10, dpi = 300, bg = "white")
cat("Wrote", out_free_fit_path, "and", out_free_mut_path, "\n")
