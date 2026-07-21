# Heatmap of static mutation rate experiments.
# x-axis: environment change rate
# y-axis: per-gene mutation rate
# fill: best/mean/worst fitness (set via WHICH_FITNESS)
# Output: one PDF, one page with 3 plots faceted by selection treatment
#
# File naming: sm_t{tourny}_c{change}_m{mu}_{seed}.csv

.libPaths(c("~/R/library", .libPaths()))

library(data.table)
library(dplyr)
library(ggplot2)
library(viridis)

# ---------------------------------------------------------------------
# CONFIG
# ---------------------------------------------------------------------
DATA_DIR <- Sys.getenv("DATA_DIR", unset = "/mnt/scratch/suzuekar/data/")
LEXICASE_DATA_DIR <- Sys.getenv("LEXICASE_DATA_DIR", unset = "/mnt/scratch/suzuekar/data/")
OUT_DIR <- "."

TURNOVER_COUNT <- 10
MAX_UPDATES <- 200000
LOG_FLOOR <- 1e-6
MAX_REPS <- as.numeric(Sys.getenv("MAX_REPS", unset = Inf))

# Leave unset to plot all mutation rates, 
# otherwise specify a string of values separated by commas (no spaces!)
MUT_RATES_RAW <- Sys.getenv("MUT_RATES", unset = "")
MUT_RATE_FILTER <- if (MUT_RATES_RAW != "") as.numeric(strsplit(MUT_RATES_RAW, ",")[[1]]) else NULL

WHICH_FITNESS <- Sys.getenv("WHICH_FITNESS", unset = "Best")
FITNESS_MAP <- list(
    Best = "Fittest Organism Selected Fitness",
    Mean = "Average Organism Selected Fitness",
    Worst = "Worst Organism Selected Fitness"
)
FITNESS_COL <- FITNESS_MAP[[WHICH_FITNESS]]
FILE_TAG <- tolower(WHICH_FITNESS)

OUT_NAME <- paste0("fig1_abstract_", FILE_TAG, ".png")

COLS_NEEDED <- c("Update", FITNESS_COL)

REGEX_PATTERN <- "^sm_(t[^_]+|lexicase)_c([^_]+)_m([^_]+)_([0-9]+)\\.csv$"

CACHE <- file.path(OUT_DIR, paste0("fig1_abstract_cache_", FILE_TAG, ".csv"))
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
        mut_rate = as.double(parts[4]),
        seed = as.integer(parts[5])
    )
}

read_turnover_rows <- function(file) {
    meta <- parse_filename(file)
    if (is.null(meta)) return(NULL)

    header <- names(fread(file, nrows = 0))
    total_rows <- as.integer(system(paste("wc -l <", shQuote(file)), intern = TRUE)) - 1
    col_positions <- match(COLS_NEEDED, header)

    if (meta$change_per_update == 0) {
        df <- fread(file, select = col_positions, skip = total_rows, header = FALSE)
        setnames(df, c("update", "fitness"))
        return(df %>% mutate(tourny_size = meta$tourny_size,
                             change_per_update = meta$change_per_update,
                             mut_rate = meta$mut_rate, seed = meta$seed))
    }

    updates_per_turnover <- 100 / meta$change_per_update
    output_freq <- min(updates_per_turnover, 100)
    rows_per_turnover <- updates_per_turnover / output_freq

    num_turnovers <- min(MAX_UPDATES / updates_per_turnover, TURNOVER_COUNT)
    rows_needed <- ceiling(rows_per_turnover * num_turnovers) + 1
    skip_n <- max(1, total_rows - rows_needed + 1)

    if (is.na(skip_n)) {
        cat("DEBUG — file:", file, "change_per_update:", meta$change_per_update,
            "total_rows:", total_rows, "rows_needed:", rows_needed, "\n")
        return(NULL)
    }

    df <- fread(file, select = col_positions, skip = skip_n, header = FALSE)
    setnames(df, COLS_NEEDED)

    rows <- list()
    for (turnover in 0:(num_turnovers - 1)) {
        idx <- nrow(df) - round(rows_per_turnover * turnover)
        if (idx < 1 || idx > nrow(df)) next
        rows[[length(rows) + 1]] <- df[idx, ]
    }

    if (length(rows) == 0) { 
        print(paste("No usable turnover rows in", file))
        return(NULL) 
    }

    rbindlist(rows) %>%
        setnames(c("update", "fitness")) %>%
        mutate(tourny_size = meta$tourny_size, change_per_update = meta$change_per_update,
               mut_rate = meta$mut_rate, seed = meta$seed)
}

subsample_files <- function(files, parse_fn) {
    if (is.infinite(MAX_REPS)) return(files)
    meta <- lapply(files, parse_fn)
    keep <- !sapply(meta, is.null)
    files <- files[keep]
    meta <- meta[keep]
    dt <- data.table(file = files,
                     tourny_size = sapply(meta, `[[`, "tourny_size"),
                     seed = sapply(meta, `[[`, "seed"))
    dt[, .SD[seed %in% head(sort(unique(seed)), MAX_REPS)], by = tourny_size]$file
}

compile_all_rows <- function(files) {
    lapply(files, read_turnover_rows) %>%
        Filter(Negate(is.null), .) %>%
        rbindlist(fill = TRUE)
}

# Median across turnovers per replicate
compile_per_replicate <- function(all_rows) {
    all_rows %>%
        group_by(tourny_size, change_per_update, mut_rate, seed) %>%
        summarise(med_fitness = median(fitness, na.rm = TRUE),
                  .groups = "drop") %>%
        arrange(tourny_size, change_per_update, mut_rate, seed)
}

# Median across replicates per grid cell
compile_grid <- function(per_replicate) {
    per_replicate %>%
        group_by(tourny_size, change_per_update, mut_rate) %>%
        summarise(med_fitness = median(med_fitness, na.rm = TRUE),
                  n_replicates = n(),
                  .groups = "drop")
}


make_heatmap <- function(grid) {
    tourny_levels <- c(sort(unique(grid$tourny_size[grid$tourny_size != "lexicase"])), "lexicase")
    grid %>%
        mutate(
            tourny_size = factor(tourny_size, levels = tourny_levels),
            change_per_update = factor(change_per_update, levels = sort(unique(change_per_update))),
            mut_rate = factor(mut_rate, levels = sort(unique(mut_rate)))
        ) %>%
        group_by(tourny_size, change_per_update) %>%
        mutate(
            log_fit = log10(pmax(med_fitness, LOG_FLOOR)),
            log_range = max(log_fit) - min(log_fit),
            norm_fitness = if_else(log_range == 0, 1, (log_fit - min(log_fit)) / log_range)
        ) %>%
        ungroup() %>%
        ggplot(aes(x = change_per_update, y = mut_rate, fill = norm_fitness)) +
        geom_tile(color = "white", linewidth = 0.5) +
        scale_fill_viridis_c(name = "Relative fitness\n(per column)") +
        facet_wrap(~ tourny_size, nrow = 1,
                   labeller = labeller(tourny_size = function(x)
                       ifelse(x == "lexicase", "Lexicase", paste("Tournament", x)))) +
        labs(x = "Environment change rate (genes/update)", y = "Per-gene mutation rate") +
        theme_minimal() +
        # theme(
        #     axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
        #     axis.text.y = element_text(size = 12),
        #     strip.text = element_text(size = 12),
        #     legend.title = element_text(size = 14),
        #     legend.text = element_text(size = 12),
        #     panel.grid = element_blank()
        # )
        theme(
            axis.text.x = element_text(
                angle = 45,
                hjust = 1,
                size = 20
            ),
            axis.title.x = element_text(
                size = 25
            ),
            axis.text.y = element_text(size = 20),
            axis.title.y = element_text(size = 25),
            strip.text = element_text(size = 20),
            legend.title = element_text(size = 20),
            legend.text = element_text(size = 20),
            panel.grid = element_blank()
        )
}

# ---------------------------------------------------------------------
# RUN
# ---------------------------------------------------------------------
if (USE_CACHE) {
    cat("Loading cached data from", CACHE, "\n")
    grid <- fread(CACHE, colClasses = list(
        character = "tourny_size", 
        numeric = "med_fitness"))
} else {
    files <- list.files(c(DATA_DIR, LEXICASE_DATA_DIR), pattern = REGEX_PATTERN, full.names = TRUE)
    cat("Found", length(files), "files\n")
    files <- subsample_files(files, parse_filename)
    cat("After replicate subsampling:", length(files), "files\n")

    all_rows <- compile_all_rows(files)
    per_replicate <- compile_per_replicate(all_rows)
    grid  <- compile_grid(per_replicate)

    fwrite(grid, CACHE)
    cat("Wrote cache to", CACHE, "\n")
}

if (!is.null(MUT_RATE_FILTER)) {
    grid <- grid %>% filter(mut_rate %in% MUT_RATE_FILTER)
    cat("Filtered to mutation rates:", paste(MUT_RATE_FILTER, collapse = ", "), "\n")
}

out_path <- file.path(OUT_DIR, OUT_NAME)
ggsave(out_path, make_heatmap(grid),
        width = 18, height = 7, units = "in", dpi = 300, bg = "white")
cat("Wrote", out_path, "\n")
