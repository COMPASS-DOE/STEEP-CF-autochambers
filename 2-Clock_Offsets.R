# 2-Clock_Offsets.R
# BBL October 2026

## Goals of this code:

# 1.  Provide fast and easy matching to align 8100 and 78x0 timestamps

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

DATE_TO_PROCESS <- ymd("2026-06-09")

source("utilities.R")

# The POSSIBLE_LI78x0_TABLE tells us, for each "Instrument" (which is
# used in the LI78x0 filename written by script 1), (1) the name of the
# variable to use in the matching process and (2) its units
POSSIBLE_LI78x0_TABLE <-
    tribble(~Instrument, ~Matching_variable, ~Units,
            "LI7810",    "CO2",              "ppm",
            "LI7820",    "H2O",              "ppm")
message("Available instruments to match: ",
        paste(POSSIBLE_LI78x0_TABLE$Instrument, collapse = ", "))

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

# ===================================================
# Main loop: go through possible LI78x0 instruments
# ===================================================

ins_table_rows <- seq_len(nrow(POSSIBLE_LI78x0_TABLE))
#ins_table_rows <- 1 # for testing
for(ins in ins_table_rows) {
    ins_name <- POSSIBLE_LI78x0_TABLE$Instrument[ins]
    ins_matchvar <- POSSIBLE_LI78x0_TABLE$Matching_variable[ins]
    ins_units <- POSSIBLE_LI78x0_TABLE$Units[ins]

    message("\n==============================================")
    message(ins_name, " matching variable: ", ins_matchvar, " (", ins_units, ")")

    # Read gas concentration data: the day-specific file prepared by script 1
    li78x0file <- file.path(RAW_DIR,
                            paste0(ins_name,
                                   "_",
                                   gsub("-", "", DATE_TO_PROCESS),
                                   ".parquet"))
    if(file.exists(li78x0file)) {
        message("Reading ", li78x0file)
        li78x0dat <- read_parquet(li78x0file)
        message("\tConcentration data file is ", nrow(li78x0dat), " x ", ncol(li78x0dat))

        # Prep 1: Make sure timestamps are EST
        li78x0dat$TIMESTAMP <- force_tz(li78x0dat$TIMESTAMP, "EST")
        # Prep 2: We could use nonstandard evaluation below, but I find it
        # easier to create a new column for this instrument's match variable
        li78x0dat$matchvar <- li78x0dat[[ins_matchvar]]
    } else {
        message("WARNING: no file available for ", ins_name, " on ", DATE_TO_PROCESS)
        next
    }

    ## Matchy match ----

    # Check: is there are already offset info for this date and instrument?
    if(nrow(plot_offsets) > 0) {
        plot_offsets |>
            filter(Date == DATE_TO_PROCESS, Instrument == ins_name) ->
            this_offsets

        if(nrow(this_offsets) > 0) {
            message("WARNING: an entry for this date ", DATE_TO_PROCESS,
                    " and instrument ", ins_name,
                    " already exists in ", PLOT_OFFSET_FILE)
            ok <- askYesNo(paste("Continue, overwriting previous", ins_name, "data? "))
            stopifnot(ok)
        }
    }

    # ===================================================
    # Secondary loop: go through plots
    # ===================================================

    # For each plot, combine algorithmic analysis with human QAQC to set
    # (1) The overall offset between 8100 (metadata) and 78x0 (concentration) clocks
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
        li78x0dat |>
            filter(Plot == pl) |>
            select(TIMESTAMP, matchvar) |>
            arrange(TIMESTAMP) |>
            mutate(hour = hour(TIMESTAMP)) ->
            li78x0_plot
        message("------------------------------------------------")
        message(DATE_TO_PROCESS, " ", pl)
        message("\tmetadata:  ", nrow(md_plot), " x ", ncol(md_plot))
        message("\tconcentrations: ", nrow(li78x0_plot), " x ", ncol(li78x0_plot))

        # Find the metadata group for each row in the 78x0 observations,
        # as well as the 'context window' group, which is the group with +/-
        # some amount of time on either side
        li78x0_plot$group <-
            quick_match_numeric(md_plot$start_time,
                                li78x0_plot$TIMESTAMP,
                                length = MSMT_LENGTH)
        li78x0_plot$context_window <-
            quick_match_numeric(md_plot$start_time,
                                li78x0_plot$TIMESTAMP,
                                length = MSMT_LENGTH,
                                left_extra = 60, right_extra = 60)

        # For plotting, we want to associate the start time with each
        # context window, so that each group's data can be plotted with
        # context (previous and following 78x0 data that do not, currently,
        # 'match')
        li78x0_plot |>
            left_join(md_plot, by = c("context_window" = "group")) |>
            mutate(start_time_offset = as.numeric(TIMESTAMP - start_time)) ->
            li78x0_plotgroups

        # Algorithmic plot-level offset ----

        # Search for the best offset between the LI8100 and LI78x0 clocks
        message("\tsearching for best-performing clock offset...")
        offset_results <- search_offsets(md_plot$start_time,
                                         li78x0_plotgroups$TIMESTAMP,
                                         li78x0_plotgroups$matchvar,
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
            ggtitle(paste(DATE_TO_PROCESS, ins_name, pl, "offset analysis; best =", alg_offset))
        print(p_offset_results)
        readline("Hit <Return> to see next plot: ")

        # Save
        DATE_NO_DASHES <- gsub("-", "", DATE_TO_PROCESS)
        pfn <- file.path(PROCESSED_DIR,
                         paste0(DATE_NO_DASHES, "_", ins_name, "_", pl, "_alloffsets.pdf"))
        message("Saving ", pfn)
        ggsave(pfn, width = 8, height = 5)

        # Show user the data for this plot and date, with vertical lines
        # to mark the algorithmic and (if desired, manual) offset
        done <- FALSE
        while(!done) {
            # the clocks won't drift super fast; for plotting clarity,
            # only show a subselection of hours in the day
            li78x0_plotgroups |>
                filter(hour %% 4 == 0) |>
                mutate(in_offset = start_time_offset >= 0 + current_offset &
                           start_time_offset <= MSMT_LENGTH + current_offset) |>
                # normalize the match variable just to make more effective use of graphing space
                group_by(hour) |>
                mutate(matchvar = matchvar - min(matchvar)) |>
                # main plot, with points colored by being within the offset window
                ggplot(aes(start_time_offset, matchvar, color = in_offset)) +
                geom_point(size = 0.5, na.rm = TRUE) +
                scale_color_manual(values = c("grey", "black")) +
                # the current measurement mapping
                geom_vline(xintercept = 0, linetype = 1) +
                geom_vline(xintercept = MSMT_LENGTH, linetype = 1) +
                # proposed mapping with offset
                geom_vline(xintercept = 0 + alg_offset, linetype = 2, color = "red") +
                geom_vline(xintercept = MSMT_LENGTH + alg_offset, linetype = 2, color = "red") +
                facet_wrap(~hour, ncol = 3) +
                guides(color = "none") +
                xlab(paste(ins_name, "time relative to metadata start_time (s)")) +
                ylab(paste0("Relative ", ins_matchvar, " (", ins_units, ")")) ->
                p_plot_offset

            title <- paste(DATE_TO_PROCESS, ins_name, pl, "(select hours)")
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

            done <- readline(paste0(DATE_TO_PROCESS, " ",
                                    ins_name, " ",
                                    pl, ": accept ", offset_type,
                                    " offset of ", current_offset, "? "))
            done <- tolower(trimws(done)) %in% c("y", "yes")
            if(!done) {
                readline(paste(pl, ": manual offset (0=none): ")) |>
                    as.numeric() ->
                    current_offset
                offset_type <- "manual"
            }
        } # while
        message(paste0(ins_name, " ", pl, ": final plot-level offset is ",
                       current_offset, " (", offset_type, ")"))

        pfn <- file.path(PROCESSED_DIR,
                         paste0(DATE_NO_DASHES, "_", ins_name, "_", pl,"_plotoffset.pdf"))
        message("Saving ", pfn)
        ggsave(pfn, width = 8, height = 5)

        # Save plot-level offset information
        newdat <- tibble(Date = DATE_TO_PROCESS,
                         Instrument = ins_name,
                         Plot = pl,
                         Clock_offset = current_offset,
                         Offset_type = offset_type)
        if(nrow(plot_offsets) > 0) {
            plot_offsets |>
                filter(Date != DATE_TO_PROCESS | Plot != pl | Instrument != ins_name) |>
                bind_rows(newdat) ->
                plot_offsets
        } else {
            plot_offsets <- newdat
        }
        message("Updating ", PLOT_OFFSET_FILE)
        write_csv(plot_offsets, PLOT_OFFSET_FILE)

        # Collar-specific offset changes ----

        offset_data <- tibble(Date = DATE_TO_PROCESS,
                              Instrument = ins_name,
                              label = unique(li78x0_plotgroups$label),
                              collar_offset = current_offset)
        collar_adjustment_made <- FALSE

        not_done <- TRUE
        while(not_done) {
            li78x0_plotgroups |>
                mutate(in_offset = start_time_offset >= 0 + current_offset &
                           start_time_offset <= MSMT_LENGTH + current_offset) |>
                group_by(hour) |>
                mutate(matchvar = matchvar - min(matchvar)) |>
                # main plot, with points colored by being within the offset window
                ggplot(aes(start_time_offset, matchvar, color = in_offset)) +
                geom_point(size = 0.5, na.rm = TRUE) +
                scale_color_manual(values = c("grey", "black")) +
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
                xlab(paste(ins_name, "time relative to metadata start_time (s)")) +
                ylab(paste0("Relative ", ins_matchvar, " (", ins_units, ")")) ->
                p_collar_offset

            title <- paste(DATE_TO_PROCESS, ins_name, pl, "(by collar)")
            subtitle <- paste("Metadata times in black; plot offset of",
                              current_offset,
                              "in red; collar-specific offsets in pink")
            p_collar_offset <- p_collar_offset + ggtitle(title, subtitle = subtitle)
            print(p_collar_offset)

            not_done <- readline(paste0(ins_name, " ", pl, ": any",
                                        if_else(collar_adjustment_made, " more", ""),
                                        " collar-specific offsets to change? (do not enter collar number yet)"))
            not_done <- tolower(trimws(not_done)) %in% c("y", "yes")
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

            pfn <- file.path(PROCESSED_DIR,
                             paste0(DATE_NO_DASHES, "_", ins_name, "_", pl, "_collaroffsets.pdf"))
            message("Saving ", pfn)
            ggsave(pfn, width = 8, height = 5)
        } else {
            message(paste0(pl, ": no collar adjustments made"))
        }

        # TODO: offer to adjust measurement length?

    } # for pl

} # for ins


message("All done")

