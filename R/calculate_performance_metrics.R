#' @name calculate_performance_metrics
#' @title Calculate performance metrics
#' @description This function calculates performance metrics (MAPE, RMSE, MSA) for a given set of predictions and observations.
#'
#' @param predicted Predicted values.
#' @param observed Observed values.
#' @param weighted Logical, indicating whether to calculate weighted metrics.
#' @return A data frame with calculated performance metrics.
#' @export
calculate_performance_metrics <- function(predicted, observed, weighted = FALSE) {
  # Combine predicted and observed into a data frame
  data <- data.frame(predicted, observed)

  # Exclude rows with NA values
  data <- na.omit(data)

  if (nrow(data) == 0) {
    # Return NA if there are no non-NA values
    return(data.frame(
      MAPE = NA,
      RMSE = NA,
      MSA = NA,
      RMSE_weighted = NA,
      MSA_weighted = NA,
      MAPE_weighted = NA
    ))
  }

  # Separate the data into vectors
  predicted <- data$predicted
  observed <- data$observed

  error <- observed - predicted
  APE <- abs(error / observed)

  performance_metrics <- data.frame(
    MAPE = mean(APE, na.rm = TRUE) * 100,
    RMSE = sqrt(mean(error^2, na.rm = TRUE)),
    MSA = 100 * (exp(mean(abs(log(observed / predicted)), na.rm = TRUE)) - 1)
  )

  if (weighted) {
    # Perform weighted metric calculation
    weighted_RMSE <- weighted_metric(error, observed, "RMSE")
    weighted_MSA <- weighted_metric(error, observed, "MSA")
    weighted_MAPE <- weighted_metric(APE, observed, "MAPE")

    performance_metrics$RMSE_weighted <- weighted_RMSE
    performance_metrics$MSA_weighted <- weighted_MSA
    performance_metrics$MAPE_weighted <- weighted_MAPE
  }

  return(performance_metrics)
}

weighted_metric <- function(error, observed, metric) {
  # Adjusted weighted metric calculation to handle NAs
  weights <- !is.na(error) & !is.na(observed)
  if (any(weights)) {
    if (metric == "RMSE") {
      return(sqrt(sum((error[weights]^2 / observed[weights])) / sum(weights)))
    } else if (metric == "MSA") {
      return(100 * (exp(mean(abs(log(observed[weights] / predicted[weights])), na.rm = TRUE)) - 1))
    } else if (metric == "MAPE") {
      return(mean(error[weights] / observed[weights]) * 100)
    } else {
      stop("Unknown metric")
    }
  } else {
    warning("Not enough data to calculate weighted metrics. Set weighted = FALSE.")
    return(NA)
  }
}
