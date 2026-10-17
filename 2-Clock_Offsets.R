# 2-Clock_Offsets.R
# BBL October 2026

## Goals of this code:

# 1.  Provide fast and easy matching to align 8100 and 7810 timestamps

message("Welcome to 2-Clock_Offsets.R")

## Setup ----

library(ggplot2)
theme_set(theme_bw())
library(lubridate)
library(dplyr)
library(tidyr)
library(readr)
library(arrow)

MSMT_LENGTH <- 90

DATE_TO_PROCESS <- ymd("2026-06-08")

RAW_DIR <- "Raw_Data/"
PROCESSED_DIR <- "Processed_Data"

PROCESSED_FILE_FINAL <-
    file.path(PROCESSED_DIR, "SoilFluxes_8100_Metadata_processed_final.parquet")

## Existing plot offset data, if any ----

PLOT_OFFSET_FILE <- file.path(PROCESSED_DIR, "offsets-plot-specific.csv")
if(file.exists(PLOT_OFFSET_FILE)) {
    message("Reading ", PLOT_OFFSET_FILE)
    plot_offsets <- read_csv(PLOT_OFFSET_FILE, col_types = "Dcdc")
} else {
    message("Creating new ", PLOT_OFFSET_FILE)
    plot_offsets <- tibble()
}

COLLAR_OFFSET_FILE <- file.path(PROCESSED_DIR, "offsets-collar-specific.csv")
if(file.exists(COLLAR_OFFSET_FILE)) {
    message("Reading ", COLLAR_OFFSET_FILE)
    collar_specific_offsets <- read_csv(COLLAR_OFFSET_FILE, col_types = "Dcd")
} else {
    message("Creating new ", COLLAR_OFFSET_FILE)
    collar_specific_offsets <- tibble()
}

## Utility functions ----

# Utility function: quick_match_numeric()
# for each timestamp x, determine the y timestamps that fall within
# x - left_extra and x + length + right_extra
# returns a numeric vector of length y giving which x matched; NA otherwise
quick_match_numeric <- function(x, y, length = MSMT_LENGTH,
                                left_extra = 0, right_extra = 0) {
    stopifnot(tz(x) == tz(y)) # defensive assertion
    matches <- rep(NA_integer_, length(y))
    for(i in seq_along(x)) {
        matches[(y >= x[i] - left_extra & y < x[i] + length + right_extra)] <- i
    }
    return(matches)
}
# Test code
# two observations in the metadata...
testx <- ymd_hms("2026-10-16 11:31:00", "2026-10-16 11:35:00")
# ...10 minutes of data
testy <- seq.POSIXt(from = ymd_hms("2026-10-16 11:30:00"),
                    to = ymd_hms("2026-10-16 11:40:00"),
                    by = "1 sec")
# should be 2 x 90 seconds = 180 matches at the correct times
stopifnot(sum(!is.na(quick_match_numeric(testx, testy))) ==
              2 * MSMT_LENGTH)

# Utility function: search_offsets()
# Given a series of observations at specific times (i.e., LI7810), and nominal
# start times of each observation (LI8100), try varying time offsets between
# the two clocks to determine what gives the best results--specifically,
# the highest average amount of variance explained
search_offsets <- function(recorded_start_times, obs_time, obs_x, mst_length) {
    stopifnot(length(obs_time) == length(obs_x))
    msmt_indices <- 0:(mst_length - 1)

    # THE OFFSET IS WHAT NEEDS TO BE **ADDED** TO THE RECORDED START TIME
    message("\t\tsearch every 5 seconds within ±2 minutes")
    search_offsets1 <- paste(seq(0, 2 * 60, by = 5), "sec")
    message("\t\tsearch every 10 seconds from ±2 to ±10 minutes")
    search_offsets2 <- paste(seq(2 * 60, 10 * 60, by = 10), "sec")
    offsets_positive <- as.period(c(search_offsets1, search_offsets2))
    offsets <- c(offsets_positive, -offsets_positive)

    # Set up results data frame with one row per offset
    results <- tibble(offset = sort(unique(offsets)),
                      mean_R2 = NA_real_)
    message("\t\t", nrow(results), " offsets x ",
            length(recorded_start_times) ," starts")

    # For each potential offset, adjust start times and fit models
    pb <- txtProgressBar(min = 1, max = nrow(results), style = 3)
    for(i in seq_len(nrow(results))) {
        setTxtProgressBar(pb, i)
        i_offset <- results$offset[i]
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


# Read metadata from script 1 ----

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

# Read gas concentration data ----

# Read the day-specific file prepared by script 1
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

# Plot-to-7810 mapping ----

# The mapping from 7810s to the plots they're in. Note that this is very
# fragile; if we ever change machines, we'll need to completely redo this!
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

if(nrow(plot_offsets) > 0 &&
   nrow(plot_offsets[plot_offsets$"Date" == DATE_TO_PROCESS,])) {
    message("WARNING: this date ", DATE_TO_PROCESS, " already exists in ", PLOT_OFFSET_FILE)
    ok <- askYesNo("Continue, overwriting previous data? ")
    stopifnot(ok)
}

# For each plot, combine algorithmic analysis with human QAQC to set
# (1) The overall offset between 8100 (metadata) and 7810 (concentration) clocks
# (2) Any collar-specific offsets
plots <- unique(md$Plot)
#plots <- "Control" # for testing
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
    message("------------------------------------------------")
    message(DATE_TO_PROCESS, " ", pl)
    message("\tmetadata:  ", nrow(md_plot), " x ", ncol(md_plot))
    message("\tconcentrations: ", nrow(li7810_plot), " x ", ncol(li7810_plot))

    # Find the metadata group for each row in the 7810 observations,
    # as well as the 'context window' group, which is the group with +/-
    # some amount of time on either side
    li7810_plot$group <-
        quick_match_numeric(md_plot$start_time,
                            li7810_plot$TIMESTAMP,
                            length = MSMT_LENGTH)
    li7810_plot$context_window <-
        quick_match_numeric(md_plot$start_time,
                            li7810_plot$TIMESTAMP,
                            length = MSMT_LENGTH,
                            left_extra = 60, right_extra = 60)

    # For plotting, we want to associate the start time with each
    # context window, so that each group's data can be plotted with
    # context (previous and following 7810 data that do not, currently,
    # 'match')
    li7810_plot |>
        left_join(md_plot, by = c("context_window" = "group")) |>
        mutate(start_time_offset = as.numeric(TIMESTAMP - start_time)) ->
        li7810_plotgroups

    # Algorithmic plot-level offset ----

    # Search for the best offset between the LI8100 and LI7810 clocks
    message("\tsearching for best-performing clock offset...")
    offset_results <- search_offsets(md_plot$start_time,
                                     li7810_plotgroups$TIMESTAMP,
                                     li7810_plotgroups$CO2,
                                     mst_length = MSMT_LENGTH)

    offset_results |>
        filter(mean_R2 == max(mean_R2)) |>
        summarise(mean_offset = mean(offset)) |>
        pull(mean_offset) ->
        alg_offset
    current_offset <- alg_offset
    offset_type <- "algorithmic"

    p_offset_results <- ggplot(offset_results, aes(x = offset, y = mean_R2)) +
        geom_point() +
        xlab("Tested clock offset (s)") +
        ggtitle(paste(DATE_TO_PROCESS, pl, "offset analysis; best =", alg_offset))
    print(p_offset_results)
    # Save
    DATE_NO_DASHES <- gsub("-", "", DATE_TO_PROCESS)
    pfn <- file.path(PROCESSED_DIR, paste0(DATE_NO_DASHES, "_", pl, "_alloffsets.pdf"))
    message("Saving ", pfn)
    ggsave(pfn, width = 8, height = 5)

    # Show user the data for this plot and date, with vertical lines
    # to mark the algorithmic and (if desired, manual) offset
    done <- FALSE
    while(!done) {
        # the clocks won't drift super fast; for plotting clarity,
        # only show a subselection of hours in the day
        li7810_plotgroups |>
            filter(hour %% 4 == 0) |>
            # normalize CO2 just to make more effective use of graphing space
            group_by(hour) |>
            mutate(CO2 = CO2 - min(CO2)) |>
            ggplot(aes(start_time_offset, CO2, color = is.na(group))) +
            geom_point(size = 0.5, na.rm = TRUE) +
            # the current measurement mapping
            geom_vline(xintercept = 0, linetype = 1) +
            geom_vline(xintercept = MSMT_LENGTH, linetype = 1) +
            # proposed mapping with offset
            geom_vline(xintercept = 0 + alg_offset, linetype = 2, color = "red") +
            geom_vline(xintercept = MSMT_LENGTH + alg_offset, linetype = 2, color = "red") +
            facet_wrap(~hour, ncol = 3) +
            guides(color = "none") +
            xlab("LI7810 time relative to metadata start_time (s)") +
            ylab("Relative CO2 (ppm)") ->
            p_plot_offset

        title <- paste(DATE_TO_PROCESS, pl, "(select hours)")
        subtitle <- paste("Metadata times in black;",
                          "auto-offset of", alg_offset, "in red")
        # If user has requested a manual offset, display that too
        if(offset_type == "manual") {
            p_plot_offset <- p_plot_offset +
                geom_vline(xintercept = 0 + current_offset, linetype = 2, color = "pink") +
                geom_vline(xintercept = MSMT_LENGTH + current_offset, linetype = 2, color = "pink")
            subtitle <- paste0(subtitle, "; manual offset of ", current_offset, " in pink")
        }
        p_plot_offset <- p_plot_offset + ggtitle(title, subtitle = subtitle)
        print(p_plot_offset)

        done <- askYesNo(paste0(pl, ": accept ", offset_type, " offset of ",
                                current_offset, "? "))
        if(!done) {
            readline(paste(pl, ": manual offset (0=none): ")) |>
                as.numeric() ->
                current_offset
            offset_type <- "manual"
        }
    } # while
    message(paste0(pl, ": final plot-level offset is ", current_offset,
                   " (", offset_type, ")"))

    pfn <- file.path(PROCESSED_DIR, paste0(DATE_NO_DASHES, "_", pl, "_plotoffset.pdf"))
    message("Saving ", pfn)
    ggsave(pfn, width = 8, height = 5)

    # Save plot-level offset information
    newdat <- tibble(Date = DATE_TO_PROCESS,
                     Plot = pl,
                     Clock_offset = current_offset,
                     Offset_type = offset_type)
    if(nrow(plot_offsets) > 0) {
        plot_offsets |>
            filter(Date != DATE_TO_PROCESS | Plot != pl) |>
            bind_rows(newdat) ->
            plot_offsets
    } else {
        plot_offsets <- newdat
    }
    message("Updating ", PLOT_OFFSET_FILE)
    write_csv(plot_offsets, PLOT_OFFSET_FILE)

    # Collar-specific offset changes ----

    offset_data <- tibble(Date = DATE_TO_PROCESS,
                          label = unique(li7810_plotgroups$label),
                          collar_offset = current_offset)
    collar_adjustment_made <- FALSE

    not_done <- TRUE
    while(not_done) {
        li7810_plotgroups |>
            group_by(hour) |>
            mutate(CO2 = CO2 - min(CO2)) |>
            ggplot(aes(start_time_offset, CO2, color = is.na(group))) +
            geom_point(size = 0.5, na.rm = TRUE) +
            # the current measurement mapping
            geom_vline(xintercept = 0, linetype = 1) +
            geom_vline(xintercept = MSMT_LENGTH, linetype = 1) +
            # collar-specific offset
            geom_vline(data = offset_data, aes(xintercept = 0 + collar_offset),
                       linetype = 2, color = "pink") +
            geom_vline(data = offset_data, aes(xintercept = MSMT_LENGTH + collar_offset),
                       linetype = 2, color = "pink") +
            # proposed mapping with offset
            geom_vline(xintercept = 0 + current_offset, linetype = 2, color = "red") +
            geom_vline(xintercept = MSMT_LENGTH + current_offset, linetype = 2, color = "red") +
            facet_wrap(~label, ncol = 3) +
            guides(color = "none") +
            xlab("LI7810 time relative to metadata start_time (s)") +
            ylab("Relative CO2 (ppm)") ->
            p_collar_offset

        title <- paste(DATE_TO_PROCESS, pl, "(by collar)")
        subtitle <- paste("Metadata times in black; plot offset of",
                          current_offset,
                          "in red; collar-specific offsets in pink")
        p_collar_offset <- p_collar_offset + ggtitle(title, subtitle = subtitle)
        print(p_collar_offset)

        not_done <- askYesNo(paste0(pl, ": any",
                                    if_else(collar_adjustment_made, " more", ""),
                                    " collar-specific offsets to change? "))
        if(not_done) {
            collar_to_change <- readline(paste0(pl, ": which collar (0=none): "))

            if(collar_to_change %in% offset_data$label) {
                new_offset <- readline(paste0(pl, " ", collar_to_change,
                                              ": new offset?: "))
                offset_data$collar_offset[offset_data$label == collar_to_change] <-
                    as.numeric(new_offset)
                collar_adjustment_made <- TRUE
            } else {
                message("That isn't one of the collars!")
            }
        }
    } # while

    if(collar_adjustment_made) {
        message(paste0(pl, ": the following collar adjustments were made"))
        offset_data |>
            filter(collar_offset != current_offset) ->
            new_offsets
        print(new_offsets)
        collar_specific_offsets |>
            bind_rows(new_offsets) ->
            collar_specific_offsets

        message("Updating ", COLLAR_OFFSET_FILE)
        write_csv(collar_specific_offsets, COLLAR_OFFSET_FILE)

        pfn <- file.path(PROCESSED_DIR, paste0(DATE_NO_DASHES, "_", pl,
                                               "_collaroffsets.pdf"))
        message("Saving ", pfn)
        ggsave(pfn, width = 8, height = 5)
    } else {
        message(paste0(pl, ": no collar adjustments made"))
    }

    # TODO: offer to adjust measurement length?

} # for pl

message("All done")
