#' @name rolling_perf
#' @title Calculate rolling performance of models
#' @description This function calculates the rolling performance of models on time series data.
#'
#' This function calculates the rolling performance of models on time series data.
#'
#' @param one_aheads A data frame with one-step ahead model predictions.
#' @param series A data frame with time series data that includes 'year' and 'abundance' columns.
#' @param roll_years The number of years for rolling calculation (default is 15).
#' @param mod_include The number of top-performing models to include (default is 5).
#' @param TY_ensemble The number of years for the ensemble analysis (default: 16).
#' @param model_list A data frame with information about each model, including the 'model' column.
#' @return A list with three elements:
#'   - 'all_mods': A data frame with all models' performance metrics.
#'   - 'top_mods': A data frame with the top-performing models' performance metrics.
#'   - 'performance': A summary of the performance metrics for the top models.
#'
#' @importFrom dplyr %>% select filter arrange group_by mutate ungroup summarize desc mutate row_number between lag
#' @import zoo
#' @import lubridate
#' @import tidyr
#' @import tibble
#' @import readr
#' @import stats

#' @export
rolling_perf <- function(one_aheads, series, roll_years, mod_include, TY_ensemble, model_list) {
  abundance <- series$abundance
  predicted_abundance <- one_aheads$predicted_abundance
  model <- one_aheads$model
  error = abundance - predicted_abundance
  APE = abs(error / abundance)
  MAPE=NULL
  .=NULL

  out <- one_aheads %>%
    left_join(series %>% dplyr::select(year, abundance)) %>%
    mutate(
      error = abundance - predicted_abundance,
      APE = abs(error / abundance)
    ) %>%
    arrange(model, year) %>%
    group_by(model) %>%
    mutate(
      MAPE = dplyr::lag(100 * zoo::rollmean(APE, k = roll_years, fill = NA, align = "right"))
    ) %>%
    group_by(year) %>%
    mutate(
      rank = rank(MAPE),
      model = as.character(model)
    )

  tops <- out %>%
    dplyr::filter(., dplyr::between(year, 2023 - TY_ensemble + 1, 2023), rank <= mod_include) %>%
    dplyr::left_join(model_list, by = "model") %>%
    dplyr::arrange(dplyr::desc(year), rank) %>% dplyr::select(-1)

  # performance of the best model
  perf <- tops %>%
    dplyr::ungroup() %>%
    dplyr::filter(rank == 1) %>%
    dplyr::summarize(
      MAPE = mean(APE, na.rm = TRUE) * 100,
      RMSE = sqrt(mean(error^2, na.rm = TRUE)),
      MSA = 100 * (exp(mean(abs(log(abundance / predicted_abundance)), na.rm = TRUE)) - 1)
    )

  return(list(
    all_mods = out %>% dplyr::ungroup(),
    top_mods = tops %>% dplyr::ungroup(),
    performance = perf
  ))
}
