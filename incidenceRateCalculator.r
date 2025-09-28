rate_calculator <- function(data, event, time, strata = NULL, unit = 100000) {
  if (!is.data.frame(data)) {
    stop("Error: 'data' must be a data.frame.")
  }
  if (!is.character(event) || !(event %in% names(data))) {
    stop("Error: 'event' must be a valid column name in the data.")
  }
  if (!is.character(time) || !(time %in% names(data))) {
    stop("Error: 'time' must be a valid column name in the data.")
  }
  if (!is.numeric(data[[time]])) {
    stop("Error: The 'time' column must be numeric.")
  }
  if (!is.null(strata) &&
      (!is.character(strata) || !(strata %in% names(data)))) {
    stop("Error: 'strata' must be a valid column name in the data.")
  }

  if (is.null(strata)) {
    total_person_years <- sum(data[[time]], na.rm = TRUE)
    event_counts <- as.data.frame(table(data[[event]], dnn = event))
    names(event_counts)[2] <- "count"
    event_counts$person_years <- total_person_years
    event_counts$rate <- (event_counts$count / event_counts$person_years) *
      unit
    return(event_counts)
  }

  data_split_by_strata <- split(data, as.factor(data[[strata]]))

  results_list <- lapply(
    names(data_split_by_strata),
    function(stratum_level) {
      stratum_data <- data_split_by_strata[[stratum_level]]
      person_years_stratum <- sum(stratum_data[[time]], na.rm = TRUE)

      event_counts_stratum <- as.data.frame(
        table(stratum_data[[event]], dnn = event)
      )
      names(event_counts_stratum)[2] <- "count"

      all_event_levels <- levels(as.factor(data[[event]]))
      missing_events <- all_event_levels[
        !all_event_levels %in% event_counts_stratum[[event]]
      ]

      if (length(missing_events) > 0) {
        missing_df <- data.frame(x = missing_events, count = 0)
        names(missing_df)[1] <- event
        event_counts_stratum <- rbind(event_counts_stratum, missing_df)
      }

      event_counts_stratum$person_years <- person_years_stratum
      event_counts_stratum$rate <- (event_counts_stratum$count /
        event_counts_stratum$person_years) * unit
      event_counts_stratum[[strata]] <- stratum_level
      event_counts_stratum
    }
  )

  final_results <- do.call(rbind, results_list)
  final_results <- final_results[, c(
    strata, event, "count", "person_years", "rate"
  )]
  rownames(final_results) <- NULL
  final_results
}
