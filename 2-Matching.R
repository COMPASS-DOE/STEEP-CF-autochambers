
## Goals of this code:

# 1.  Provide fast and easy matching to align 8100 and 7810 timestamps

## Setup ----

library(ggplot2)
theme_set(theme_bw())
library(lubridate)
library(dplyr)
library(tidyr)
library(readr)
library(fluxfinder)
library(arrow)

DATE_TO_PROCESS <- ymd("2026-06-08")

RAW_DIR <- "Raw_Data/"
PROCESSED_DIR <- "Processed_Data"

PROCESSED_FILE_FINAL <-
    file.path(PROCESSED_DIR, "SoilFluxes_8100_Metadata_processed_final.parquet")

## Read raw 7810 data - prep ----

# for each timestamp x, determine the y timestamps that fall within
# x - left_extra and x + length + right_extra
# returns a numeric vector of length y giving which x matched; NA otherwise
quick_match_numeric <- function(x, y, length = 90,
                                left_extra = 0, right_extra = 0) {
    stopifnot(tz(x) == tz(y)) # defensive assertion
    matches <- rep(NA_integer_, length(y))
    for(i in seq_along(x)) {
        matches[(y >= x[i] - left_extra & y < x[i] + length + right_extra)] <- i
    }
    return(matches)
}
# two observations in the metadata...
testx <- ymd_hms("2026-10-16 11:31:00", "2026-10-16 11:35:00")
# ...10 minutes of data
testy <- seq.POSIXt(from = ymd_hms("2026-10-16 11:30:00"),
                    to = ymd_hms("2026-10-16 11:40:00"),
                    by = "1 sec")
# should be 2 x 90 seconds = 180 matches at the correct times
stopifnot(sum(!is.na(quick_match_numeric(testx, testy))) == 180)

#
search_offsets <- function(recorded_start_times, obs_time, obs_x, mst_length) {
    stopifnot(length(obs_time) == length(obs_x))
    msmt_indices <- 0:(mst_length - 1)

    # THE OFFSET IS WHAT NEEDS TO BE **ADDED** TO THE RECORDED START TIME
    # Search every 5 seconds within ± 2 minutes...
    search_offsets1 <- paste(seq(0, 2 * 60, by = 5), "sec")
    # ...and every 10 seconds from ±2 to ±10 minutes
    search_offsets2 <- paste(seq(2 * 60, 10 * 60, by = 10), "sec")
    offsets_positive <- as.period(c(search_offsets1, search_offsets2))
    offsets <- c(offsets_positive, -offsets_positive)

    message(length(offsets), " offsets to test")
    results <- tibble(offset = sort(unique(offsets)),
                      mean_R2 = NA_real_)
    # For each potential offset, adjust start times and fit models
    i_vals <- seq_len(nrow(results))
    i_prints <- floor(seq.int(from = 10, 100, by = 10) / 100 * nrow(results))
    for(i in i_vals) {
        i_offset <- results$offset[i]
        if(i %in% i_prints) {
            message(round(i / nrow(results) * 100), "%...", appendLF = FALSE)
        }
        test_start_times <- recorded_start_times + i_offset
        # For each test start time, fit a model and record R2
        r2 <- 0.0
        n <- 0
        for(j in seq_along(test_start_times)) {
            idx <- which.min(abs(obs_time - test_start_times[j]))
            try({
                m <- lm(obs_x[idx + msmt_indices] ~ msmt_indices)
                r2 <- r2 + summary(m)$adj.r.squared
                n <- n + 1
            })
        }
        results$mean_R2[i] <- r2 / n
    }
    results |> arrange(desc(mean_R2))
}


# Metadata
message("Reading ", basename(PROCESSED_FILE_FINAL))
md_all <- read_parquet(PROCESSED_FILE_FINAL)
message("Metadata file is ", nrow(md_all), " x ", ncol(md_all))
md_all |>
    filter(date(start_time) == DATE_TO_PROCESS) |>
    mutate(start_time = ymd_hms(start_time, tz = "EST"),
           end_time = ymd_hms(end_time, tz = "EST")) ->
    md
message("Metadata for ", DATE_TO_PROCESS, " is ", nrow(md), " x ", ncol(md))
if(nrow(md) == 0) {
    stop("No metadata for this date!")
}

# Gas concentration data
li7810file <- file.path(RAW_DIR, paste0("LI7810_", gsub("-", "", DATE_TO_PROCESS), ".parquet"))
if(file.exists(li7810file)) {
    message("Reading ", li7810file)
    li7810dat <- read_parquet(li7810file)
    li7810dat$TIMESTAMP <- force_tz(li7810dat$TIMESTAMP, "EST")
} else {
    # else what?
    stop()
}
message("Concentration data file is ", nrow(li7810dat), " x ", ncol(li7810dat))

# Mapping from 7810s to the plots they're in. Note that this is super fragile;
# if we ever change machines, we'll need to completely redo this!
li7810_map <- tribble(~SN,           ~Plot,
                      "TG10-01746",  "Salt",
                      "TG10-01725",  "Fresh",
                      "TG10-01712",  "Control")
li7810dat |>
    left_join(li7810_map, by = "SN", relationship = "many-to-one") ->
    li7810dat
if(any(is.na(li7810dat$Plot))) {
    stop("There are unknown SNs in this data file!")
}

## Matchy match ----

# For each plot, combine algorithmic analysis with human QAQC to set
# (1) The overall offset between 8100 (metadata) and 7810 (concentration) clocks
# (2) Any collar-specific offsets
#plots <- unique(md$Plot)
plots <- "Control" # testing
for(pl in plots) {
    md |>
        filter(Plot == pl) |>
        arrange(start_time) |>
        select(label, start_time, end_time) ->
        md_plot
    md_plot$group <- seq_len(nrow(md_plot))
    li7810dat |>
        filter(Plot == pl) |>
        select(TIMESTAMP, CO2) |>
        arrange(TIMESTAMP) |>
        mutate(hour = hour(TIMESTAMP)) ->
        li7810_plot
    message(DATE_TO_PROCESS, " ", pl)
    message("\tmetadata:  ", nrow(md_plot), " x ", ncol(md_plot))
    message("\tconcentrations: ", nrow(li7810_plot), " x ", ncol(li7810_plot))

    # Find the metadata group for each row in the 7810 observations,
    # as well as the 'context window' group, which is the group with +/-
    # some amount of time on either side
    li7810_plot$group <-
        quick_match_numeric(md_plot$start_time,
                            li7810_plot$TIMESTAMP,
                            length = 90)
    li7810_plot$context_window <-
        quick_match_numeric(md_plot$start_time,
                            li7810_plot$TIMESTAMP,
                            length = 90,
                            left_extra = 60, right_extra = 60)

    # For plotting, we want to associate the start time with each
    # context window, so that each group's data can be plotted with
    # context (previous and following 7810 data that do not, currently,
    # 'match')
    li7810_plot |>
        left_join(md_plot, by = c("context_window" = "group")) |>
        mutate(start_time_offset = as.numeric(TIMESTAMP - start_time)) ->
        li7810_plotgroups

    # Search for the best offset between the LI8100 and LI7810 clocks
    message("Searching for best-performing clock offset...")
    offset_results <- search_offsets(md_plot$start_time,
                                     li7810_plotgroups$TIMESTAMP,
                                     li7810_plotgroups$CO2,
                                     mst_length = 90)

    offset_results |>
        filter(mean_R2 == max(mean_R2)) |>
        summarise(mean_offset = mean(offset)) |>
        pull(mean_offset) ->
        offset

    p <- ggplot(offset_results, aes(x = offset, y = mean_R2)) +
        geom_point() +
        xlab("Tested clock offset (s)") +
        ggtitle(paste(DATE_TO_PROCESS, pl, "offset analysis; best =", offset))
    print(p)


    # the clocks won't drift super fast; for plotting clarity, down-select
    li7810_plotgroups |>
        filter(hour %% 4 == 0) |>
        group_by(hour) |>
        mutate(CO2 = CO2 - min(CO2)) |>
        ggplot(aes(start_time_offset, CO2, color = is.na(group))) +
        geom_point(size = 0.5, na.rm = TRUE) +
        # the current measurement mapping
        geom_vline(xintercept = 0, linetype = 1) +
        geom_vline(xintercept = 90, linetype = 1) +
        # proposed mapping with offset
        geom_vline(xintercept = 0 + offset, linetype = 2, color = "red") +
        geom_vline(xintercept = 90 + offset, linetype = 2, color = "red") +
        facet_wrap(~hour, ncol = 3) +
        guides(color = "none") +
        xlab("LI7810 time relative to metadata start_time (s)") +
        ylab("Relative CO2 (ppm)") +
        ggtitle(paste(DATE_TO_PROCESS, pl, "(select hours); proposed offset of",
                      offset, "in red")) ->
        p
    print(p)
}
