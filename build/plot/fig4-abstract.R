# Summary of evolving mutation rate experiments.
# 1-page PDF with two stacked panels:
#   Top: change rate vs best fitness
#   Bottom: change rate vs average mutation rate
# 3 lines per panel, one per selection treatment (T3, T6, Lexicase).
# Per-replicate dots shown behind lines.
#
# File naming: em_t{tourny}_c{change}_{seed}.csv

.libPaths(c("~/R/library", .libPaths()))

library(data.table)
library(dplyr)
library(ggplot2)
library(patchwork)

# ---------------------------------------------------------------------
# CONFIG
# ---------------------------------------------------------------------
DATA_DIR <- Sys.getenv("DATA_DIR", unset = "/mnt/scratch/suzuekar/data/")
LEXICASE_DATA_DIR <- Sys.getenv("LEXICASE_DATA_DIR", unset = "/mnt/scratch/suzuekar/data/")
OUT_DIR <- "."

TURNOVER_COUNT <- 10
LOG_FLOOR <- 1e-6
MAX_REPS <- as.numeric(Sys.getenv("MAX_REPS", unset = Inf))
OUT_NAME <- "fig4_abstract.png"

REGEX_PATTERN <- "^em_(t[^_]+|lexicase)_c([^_]+)_([0-9]+)\\.csv$"

FITNESS_COL  <- "Fittest Organism Selected Fitness"
MUTRATE_COL  <- "Average Mutation Rate"
COLS_NEEDED  <- c("Update", FITNESS_COL, MUTRATE_COL)

TOURNY_LABELS <- c("2" = "Tournament 2", "3" = "Tournament 3", "6" = "Tournament 6", "10" = "Tournament 10", "lexicase" = "Lexicase")

CACHE <- file.path(OUT_DIR, "fig4_abstract_cache.csv")
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

read_turnover_rows <- function(file) {
    meta <- parse_filename(file)
    if (is.null(meta)) return(NULL)

    header <- names(fread(file, nrows = 0))
    total_rows <- as.integer(system(paste("wc -l <", shQuote(file)), intern = TRUE)) - 1
    col_positions <- match(COLS_NEEDED, header)

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
    setnames(df, COLS_NEEDED)

    rows <- lapply(0:(num_turnovers - 1), function(t) {
        idx <- nrow(df) - round(updates_per_turnover * t)
        if (idx < 1 || idx > nrow(df)) return(NULL)
        df[idx, ]
    })
    rows <- Filter(Negate(is.null), rows)
    if (length(rows) == 0) { print(paste("No usable turnover rows in", file)); return(NULL) }

    rbindlist(rows) %>%
        setnames(c("update", "fitness", "avg_mut_rate")) %>%
        mutate(tourny_size = meta$tourny_size, change_per_update = meta$change_per_update,
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

compile_per_replicate <- function(files) {
    lapply(files, read_turnover_rows) %>%
        Filter(Negate(is.null), .) %>%
        rbindlist(fill = TRUE) %>%
        mutate(across(c(fitness, avg_mut_rate), as.numeric)) %>%
        group_by(tourny_size, change_per_update, seed) %>%
        summarise(best_fitness = median(fitness, na.rm = TRUE),
                  avg_mut_rate = median(avg_mut_rate, na.rm = TRUE),
                  .groups = "drop")
}

# Median across replicates
compile_grid <- function(per_rep) {
    per_rep %>%
        group_by(tourny_size, change_per_update) %>%
        summarise(best_fitness = median(best_fitness, na.rm = TRUE),
                  avg_mut_rate = median(avg_mut_rate, na.rm = TRUE),
                  .groups = "drop")
}

make_summary_panel <- function(per_rep, grid, value_col, y_label, x_trans, x_breaks, x_nonzero_min, add_x_label = FALSE) {
    per_rep <- per_rep %>%
        mutate(tourny_label = recode(tourny_size, !!!TOURNY_LABELS),
               tourny_label = factor(tourny_label, levels = TOURNY_LABELS)) %>%
               droplevels()
    grid <- grid %>%
        mutate(tourny_label = recode(tourny_size, !!!TOURNY_LABELS),
               tourny_label = factor(tourny_label, levels = TOURNY_LABELS)) %>%
               droplevels()

    p <- ggplot(grid, aes(x = change_per_update, y = .data[[value_col]], color = tourny_label)) +
        geom_point(data = per_rep,
                   aes(y = .data[[value_col]]),
                   alpha = 0.35, size = 5, shape = 16,
                   position = position_jitter(width = x_nonzero_min / 50, height = 0)) +
        geom_line(linewidth = 2) +
        scale_color_manual(values = c("#0072B2", "#E69F00", "#009E73")) +
        scale_linetype_manual(values = c("solid", "dashed", "dotted")) +
        scale_x_continuous(trans = x_trans, breaks = x_breaks,
                           labels = scales::label_number(drop0trailing = TRUE),
                           minor_breaks = NULL) +
        scale_y_log10() +
        labs(x = if (add_x_label) "Rate of environment change (genes per update)" else NULL,
             y = y_label, color = NULL) +
        theme_minimal(base_size = 18) +
        theme(
            axis.text.y = element_text(size = 20),
            axis.title.y = element_text(size = 25),
            legend.title = element_text(size = 20),
            legend.text = element_text(size = 20),
            legend.key.height = unit(1.2, "lines"),
            legend.key.width = unit(1.5, "lines"),
            panel.grid.minor = element_blank(),
            panel.grid.major = element_line(linewidth = 1)
        )

        if (!add_x_label) {
            p <- p + theme(
                axis.text.x = element_blank(),
                axis.ticks.x = element_blank()
            )
        } else {
            p <- p + theme(
                axis.text.x = element_text(
                    angle = 45,
                    hjust = 1,
                    size = 20
                ),
                axis.title.x = element_text(
                    size = 25
                )
            )
        }
}

# ---------------------------------------------------------------------
# RUN
# ---------------------------------------------------------------------
if (USE_CACHE) {
    cat("Loading cached data from", CACHE, "\n")
    per_rep <- fread(CACHE, colClasses = list(character = "tourny_size"))
} else {
    files <- list.files(c(DATA_DIR, LEXICASE_DATA_DIR), pattern = REGEX_PATTERN, full.names = TRUE)
    cat("Found", length(files), "files\n")
    files <- subsample_files(files, parse_filename)
    cat("After replicate subsampling:", length(files), "files\n")

    per_rep <- compile_per_replicate(files)
    fwrite(per_rep, CACHE)
    cat("Wrote cache to", CACHE, "\n")
}

grid <- compile_grid(per_rep)

x_breaks_all <- sort(unique(grid$change_per_update))
x_nonzero_min <- min(x_breaks_all[x_breaks_all > 0])
x_trans <- scales::pseudo_log_trans(sigma = x_nonzero_min / 10, base = 10)
x_breaks_display <- c(0, 10^seq(log10(x_nonzero_min), log10(max(x_breaks_all)), by = 1))

p_fit <- make_summary_panel(per_rep, grid, "best_fitness",
    y_label = "Best selected fitness",
    x_trans = x_trans, x_breaks = x_breaks_display, x_nonzero_min = x_nonzero_min,
    add_x_label = FALSE)

p_mut <- make_summary_panel(per_rep, grid, "avg_mut_rate",
    y_label = "Average mutation rate",
    x_trans = x_trans, x_breaks = x_breaks_display, x_nonzero_min = x_nonzero_min,
    add_x_label = TRUE)

out_path <- file.path(OUT_DIR, OUT_NAME)
ggsave(out_path, p_fit / p_mut, width = 12, height = 10, units = "in", dpi = 300)
cat("Wrote", out_path, "\n")
