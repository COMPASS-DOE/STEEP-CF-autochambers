# utilities.R

## Utility functions used by script 2 ----

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

