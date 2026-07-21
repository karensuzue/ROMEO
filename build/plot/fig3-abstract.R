# Fitness vs mutation rate plot.
# Black line + IQR ribbon: static mutation rate sweep.
# Colored dots (color = update): evolving run trajectory.
# Red diamond + error bars: final evolved (mut_rate, fitness).
# facet_grid: rows = selection treatment, columns = change rate.
# Output: one PNG per fitness type (Best/Mean/Worst).
#
# Static files:  sm_t{tourny}_c{change}_m{mu}_{seed}.csv
# Evolving files: em_t{tourny}_c{change}_{seed}.csv

.libPaths(c("~/R/library", .libPaths()))

library(data.table)
library(dplyr)
library(ggplot2)
library(viridis)
library(scales)

# ---------------------------------------------------------------------
# CONFIG
# ---------------------------------------------------------------------
STATIC_DATA_DIR <- Sys.getenv("STATIC_DATA_DIR", unset = "/mnt/scratch/suzuekar/data/")
EVOLVE_DATA_DIR <- Sys.getenv("EVOLVE_DATA_DIR", unset = "/mnt/scratch/suzuekar/data/")
LEXICASE_STATIC_DIR <- Sys.getenv("LEXICASE_STATIC_DIR", unset = "/mnt/scratch/suzuekar/data/")
LEXICASE_EVOLVE_DIR <- Sys.getenv("LEXICASE_EVOLVE_DIR", unset = "/mnt/scratch/suzuekar/data/")
OUT_DIR <- "."

TURNOVER_COUNT <- 10
MAX_UPDATES <- 200000
TRAJECTORY_SUBSAMPLE <- 1000
LOG_FLOOR <- 1e-6
MAX_REPS <- as.numeric(Sys.getenv("MAX_REPS", unset = Inf))

# Comma-separated change rates to plot, or empty for all
CHANGE_RATES_RAW <- Sys.getenv("FIG3_CHANGE_RATES", unset = "")
CHANGE_RATE_FILTER <- if (CHANGE_RATES_RAW != "") as.numeric(strsplit(CHANGE_RATES_RAW, ",")[[1]]) else NULL

# Comma-separated tournament sizes to plot (use "lexicase" for lexicase), or empty for all
TOURNY_RAW <- Sys.getenv("FIG3_TOURNY", unset = "")
TOURNY_FILTER <- if (TOURNY_RAW != "") strsplit(TOURNY_RAW, ",")[[1]] else NULL

WHICH_FITNESS <- Sys.getenv("WHICH_FITNESS", unset = "Best")
FITNESS_MAP <- list(
    Best  = "Fittest Organism Selected Fitness",
    Mean  = "Average Organism Selected Fitness",
    Worst = "Worst Organism Selected Fitness"
)
FITNESS_COL <- FITNESS_MAP[[WHICH_FITNESS]]
FILE_TAG <- tolower(WHICH_FITNESS)
OUT_NAME <- paste0("fig3_", FILE_TAG, ".png")

COLS_NEEDED_STATIC <- c("Update", FITNESS_COL)
COLS_NEEDED_EVOLVE <- c("Update", FITNESS_COL, "Average Mutation Rate")

STATIC_REGEX <- "^sm_(t[^_]+|lexicase)_c([^_]+)_m([^_]+)_([0-9]+)\\.csv$"
EVOLVE_REGEX <- "^em_(t[^_]+|lexicase)_c([^_]+)_([0-9]+)\\.csv$"

TOURNY_LABELS <- c("2" = "Tournament 2", "3" = "Tournament 3", "6" = "Tournament 6", "10" = "Tournament 10", "lexicase" = "Lexicase")

CACHE_STATIC <- file.path(OUT_DIR, paste0("fig3_static_", FILE_TAG, ".csv"))
CACHE_EVOLVE <- file.path(OUT_DIR, paste0("fig3_evolve_", FILE_TAG, ".csv"))
CACHE_TRAJ   <- file.path(OUT_DIR, paste0("fig3_traj_", FILE_TAG, ".csv"))
USE_CACHE <- as.logical(Sys.getenv("USE_CACHE",
    unset = file.exists(CACHE_STATIC) & file.exists(CACHE_EVOLVE) & file.exists(CACHE_TRAJ)))

# ---------------------------------------------------------------------
# HELPER FUNCTIONS
# ---------------------------------------------------------------------
parse_static_filename <- function(file) {
    nm <- basename(file)
    parts <- regmatches(nm, regexec(STATIC_REGEX, nm))[[1]]
    if (length(parts) == 0) { print(paste("Error parsing filename:", nm)); return(NULL) }
    raw <- parts[2]
    list(
        tourny_size = if (raw == "lexicase") "lexicase" else sub("^t", "", raw),
        change_per_update = as.double(parts[3]),
        mut_rate = as.double(parts[4]),
        seed = as.integer(parts[5])
    )
}

parse_evolve_filename <- function(file) {
    nm <- basename(file)
    parts <- regmatches(nm, regexec(EVOLVE_REGEX, nm))[[1]]
    if (length(parts) == 0) { print(paste("Error parsing filename:", nm)); return(NULL) }
    raw <- parts[2]
    list(
        tourny_size = if (raw == "lexicase") "lexicase" else sub("^t", "", raw),
        change_per_update = as.double(parts[3]),
        seed = as.integer(parts[4])
    )
}

static_read_turnover_rows <- function(file) {
    meta <- parse_static_filename(file)
    if (is.null(meta)) return(NULL)

    header <- names(fread(file, nrows = 0))
    total_rows <- as.integer(system(paste("wc -l <", shQuote(file)), intern = TRUE)) - 1
    col_positions <- match(COLS_NEEDED_STATIC, header)

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
    setnames(df, COLS_NEEDED_STATIC)

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

evolve_read_turnover_rows <- function(file) {
    meta <- parse_evolve_filename(file)
    if (is.null(meta)) return(NULL)

    header <- names(fread(file, nrows = 0))
    total_rows <- as.integer(system(paste("wc -l <", shQuote(file)), intern = TRUE)) - 1
    col_positions <- match(COLS_NEEDED_EVOLVE, header)

    if (meta$change_per_update == 0) {
        df <- fread(file, select = col_positions, skip = total_rows, header = FALSE)
        setnames(df, c("update", "fitness", "avg_mut_rate"))
        return(df %>% mutate(tourny_size = meta$tourny_size,
                             change_per_update = meta$change_per_update,
                             seed = meta$seed))
    }

    updates_per_turnover <- 100 / meta$change_per_update
    num_turnovers <- min(2000 * meta$change_per_update, TURNOVER_COUNT)
    rows_needed <- ceiling(num_turnovers * updates_per_turnover)
    skip_n <- max(1, total_rows - rows_needed + 1)

    if (is.na(skip_n)) {
        cat("DEBUG — file:", file, "change_per_update:", meta$change_per_update,
            "total_rows:", total_rows, "rows_needed:", rows_needed, "\n")
        return(NULL)
    }

    df <- fread(file, select = col_positions, skip = skip_n, header = FALSE)
    setnames(df, COLS_NEEDED_EVOLVE)

    rows <- list()
    for (turnover in 0:(num_turnovers - 1)) {
        idx <- nrow(df) - round(updates_per_turnover * turnover)
        if (idx < 1 || idx > nrow(df)) next
        rows[[length(rows) + 1]] <- df[idx, ]
    }

    if (length(rows) == 0) { 
        print(paste("No usable turnover rows in", file))
        return(NULL) 
    }

    rbindlist(rows) %>%
        setnames(c("update", "fitness", "avg_mut_rate")) %>%
        mutate(tourny_size = meta$tourny_size, change_per_update = meta$change_per_update,
               seed = meta$seed)
}

evolve_read_trajectory <- function(file) {
    meta <- parse_evolve_filename(file)
    if (is.null(meta)) return(NULL)
    fread(file, select = COLS_NEEDED_EVOLVE) %>%
        setnames(c("update", "fitness", "avg_mut_rate")) %>%
        filter(update %% TRAJECTORY_SUBSAMPLE == 0) %>%
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

# Compress everything into one function...
compile_static <- function(files) {
    lapply(files, static_read_turnover_rows) %>%
        Filter(Negate(is.null), .) %>%
        rbindlist(fill = TRUE) %>%
        group_by(tourny_size, change_per_update, mut_rate, seed) %>%
        summarise(fitness = median(fitness, na.rm = TRUE), .groups = "drop") %>%
        group_by(tourny_size, change_per_update, mut_rate) %>%
        summarise(med_fitness = median(fitness, na.rm = TRUE),
                  lo_fitness = quantile(fitness, 0.25, na.rm = TRUE),
                  hi_fitness = quantile(fitness, 0.75, na.rm = TRUE),
                  .groups = "drop")
}

compile_evolve <- function(files) {
    lapply(files, evolve_read_turnover_rows) %>%
        Filter(Negate(is.null), .) %>%
        rbindlist(fill = TRUE) %>%
        group_by(tourny_size, change_per_update, seed) %>%
        summarise(evolved_mut_rate = median(avg_mut_rate, na.rm = TRUE),
                  evolved_fitness = median(fitness, na.rm = TRUE),
                  .groups = "drop") %>%
        group_by(tourny_size, change_per_update) %>%
        summarise(med_mut_rate = median(evolved_mut_rate, na.rm = TRUE),
                  lo_mut_rate = quantile(evolved_mut_rate, 0.25, na.rm = TRUE),
                  hi_mut_rate = quantile(evolved_mut_rate, 0.75, na.rm = TRUE),
                  med_fitness = median(evolved_fitness, na.rm = TRUE),
                  lo_fitness = quantile(evolved_fitness, 0.25, na.rm = TRUE),
                  hi_fitness = quantile(evolved_fitness, 0.75, na.rm = TRUE),
                  .groups = "drop")
}

compile_trajectory <- function(files) {
    lapply(files, evolve_read_trajectory) %>%
        Filter(Negate(is.null), .) %>%
        rbindlist() %>%
        group_by(tourny_size, change_per_update, update) %>%
        summarise(avg_mut_rate = median(avg_mut_rate, na.rm = TRUE),
                  fitness = median(fitness, na.rm = TRUE),
                  .groups = "drop")
}

make_fvm_plot <- function(static_sum, evolve_sum, traj_sum) {
    all_tournys <- unique(static_sum$tourny_size)
    tourny_levels <- unname(
        TOURNY_LABELS[names(TOURNY_LABELS) %in% all_tournys]
    )
    label_tourny <- function(df) {
        df %>% mutate(tourny_label = recode(tourny_size, !!!TOURNY_LABELS),
                      tourny_label = factor(tourny_label, levels = tourny_levels))
    }

    static_sum <- label_tourny(static_sum)
    evolve_sum <- label_tourny(evolve_sum)
    traj_sum <- label_tourny(traj_sum)

    ggplot(static_sum, aes(x = mut_rate, y = med_fitness)) +
        geom_ribbon(aes(ymin = lo_fitness, ymax = hi_fitness),
                    fill = "grey70", alpha = 0.5) +
        geom_line(color = "black", linewidth = 1.5) +
        geom_point(data = traj_sum,
                   aes(x = avg_mut_rate, y = fitness, size = update, color = update),
                   inherit.aes = FALSE, alpha = 0.55) +
        scale_size_continuous(range = c(1.5, 8), guide = "none") +
        scale_color_viridis_c(option = "plasma", name = "Update") +
        geom_errorbarh(data = evolve_sum,
                       aes(y = med_fitness, xmin = lo_mut_rate, xmax = hi_mut_rate),
                       inherit.aes = FALSE, height = 0, linewidth = 1.2, color = "red") +
        geom_errorbar(data = evolve_sum,
                      aes(x = med_mut_rate, ymin = lo_fitness, ymax = hi_fitness),
                      inherit.aes = FALSE, width = 0, linewidth = 1.2, color = "red") +
        geom_point(data = evolve_sum,
                   aes(x = med_mut_rate, y = med_fitness),
                   inherit.aes = FALSE, shape = 18, size = 8, color = "red") +
        facet_grid(tourny_label ~ change_per_update, scales = "free_y",
                   labeller = labeller(change_per_update = label_both)) +
        scale_y_log10() +
        scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x, n = 6),
                      labels = trans_format("log10", math_format(10^.x))) +
        labs(x = "Mutation rate", y = paste(WHICH_FITNESS, "fitness")) +
        theme_minimal(base_size = 20) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 18),
              axis.text.y = element_text(size = 18),
              axis.title.x = element_text(size = 24),
              axis.title.y = element_text(size = 24),
              strip.text = element_text(size = 18),
              legend.title = element_text(size = 18),
              legend.text = element_text(size = 16))
}

# ---------------------------------------------------------------------
# RUN
# ---------------------------------------------------------------------
if (USE_CACHE) {
    cat("Loading cached data\n")
    static_summary <- fread(CACHE_STATIC, colClasses = list(character = "tourny_size"))
    evolve_summary <- fread(CACHE_EVOLVE, colClasses = list(character = "tourny_size"))
    traj_summary <- fread(CACHE_TRAJ,   colClasses = list(character = "tourny_size"))
} else {
    static_files <- list.files(c(STATIC_DATA_DIR, LEXICASE_STATIC_DIR),
                               pattern = STATIC_REGEX, full.names = TRUE)
    cat("Found", length(static_files), "static files\n")
    static_files <- subsample_files(static_files, parse_static_filename)
    cat("After replicate subsampling:", length(static_files), "static files\n")
    static_summary <- compile_static(static_files)
    fwrite(static_summary, CACHE_STATIC)

    evolve_files <- list.files(c(EVOLVE_DATA_DIR, LEXICASE_EVOLVE_DIR),
                               pattern = EVOLVE_REGEX, full.names = TRUE)
    cat("Found", length(evolve_files), "evolving files\n")
    evolve_files <- subsample_files(evolve_files, parse_evolve_filename)
    cat("After replicate subsampling:", length(evolve_files), "evolving files\n")
    evolve_summary <- compile_evolve(evolve_files)
    fwrite(evolve_summary, CACHE_EVOLVE)

    traj_summary <- compile_trajectory(evolve_files)
    fwrite(traj_summary, CACHE_TRAJ)

    cat("Wrote caches\n")
}

if (!is.null(CHANGE_RATE_FILTER)) {
    static_summary <- static_summary %>% filter(change_per_update %in% CHANGE_RATE_FILTER)
    evolve_summary <- evolve_summary %>% filter(change_per_update %in% CHANGE_RATE_FILTER)
    traj_summary <- traj_summary %>% filter(change_per_update %in% CHANGE_RATE_FILTER)
    cat("Filtered to change rates:", paste(CHANGE_RATE_FILTER, collapse = ", "), "\n")
}

if (!is.null(TOURNY_FILTER)) {
    static_summary <- static_summary %>% filter(tourny_size %in% TOURNY_FILTER)
    evolve_summary <- evolve_summary %>% filter(tourny_size %in% TOURNY_FILTER)
    traj_summary <- traj_summary %>% filter(tourny_size %in% TOURNY_FILTER)
    cat("Filtered to selection treatments:", paste(TOURNY_FILTER, collapse = ", "), "\n")
}

out_path <- file.path(OUT_DIR, OUT_NAME)
ggsave(out_path, make_fvm_plot(static_summary, evolve_summary, traj_summary),
       width = 14, height = 10, dpi = 300, units = "in", bg = "white")
cat("Wrote", out_path, "\n")
