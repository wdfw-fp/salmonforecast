# all_subsets.R

#' all_subsets Function
#'
#'
#' @param series Data with annual values of response (abundance) and predictor covariates.
#' @param covariates Vector of covariates to include in models.
#' @param min Minimum number of covariates within individual model.
#' @param max Maximum number of covariates within individual model.
#' @param type Specifies whether an in-season or pre-season forecast.
#' @param fit Flag for whether to fit models.
#' @return A list containing ...
#'
#'
#' @examples
#' \dontrun{
#'   library(allsubsetspackagenew)  # Load your package
#'
#'   # Example code for using the all_subsets function
#'   data <- read.csv("data/dat.csv")
#'
#'   covariates <- c(
#'     "lag1_log_JackOPI",
#'     "lag1_log_SmAdj",
#'     "lag1_NPGO",
#'     "lag1_PDO",
#'     "WSST_A",
#'     "PDO.MJJ",
#'     "MEI.OND",
#'     "UWI.JAS",
#'     "SST.AMJ",
#'     "SSH.AMJ",
#'     "UWI.SON"
#'   )
#'
#'   min_covariates <- 0
#'   max_covariates <- 1
#'   forecast_type <- "preseason"
#'
#'   results <- all_subsets(
#'     series = data,
#'     covariates = covariates,
#'     min = min_covariates,
#'     max = max_covariates,
#'     type = forecast_type,
#'     fit = TRUE
#'   )
#'
#'   cat("Number of models:", length(results[[1]]), "\n")
#'   cat("Best model formula:", results[[2]]$formula[1], "\n")
#'   cat("AICc of the best model:", results[[2]]$AICc[1], "\n")
#' }
#'
#' @export
#' @import dplyr
#' @import forecast
#' @import testthat


all_subsets <- function(series, covariates, min, max, type, fit = TRUE) {
  freq <- ifelse(type == "inseason", 2, 1)
  seasonal <- ifelse(type == "inseason", T, F)

  series <- series %>%
    ungroup() %>%
    dplyr::select(year, species, period, abundance, all_of(covariates)) %>%
    filter(if_any(all_of(covariates), ~ !is.na(.x)))

  vars <- list()
  AICc <- c()
  formula <- c()
  model_num <- c()
  tmin <- ifelse(min == 0, 1, min)

  for (i in tmin:max) { # loop over the number of covariates per model
    temp <- combn(covariates, i, simplify = FALSE) # create all unique combinations of length i
    for (j in 1:length(temp)) {
      vars[[length(vars) + 1]] <- temp[[j]]
    }
  }

  total <- ifelse(min == 0, length(vars) + 1, length(vars))
  print(paste0("There are ", total, " models to fit! Fitting model number:"))

  if (fit) {
    for (i in 1:length(vars)) {
      print(paste0(i, " out of ", total))
      xreg <- series %>%
        ungroup() %>%
        dplyr::select(all_of(vars[[i]])) %>%
        as.matrix()

      m1 <- series %>%
        ungroup() %>%
        dplyr::select(abundance) %>%
        unlist() %>%
        ts(frequency = freq) %>%
        auto.arima(lambda = 0, seasonal = seasonal, xreg = xreg)
      AICc[i] <- m1$aicc
      formula[i] <- paste0("abundance ~ ", paste(all_of(vars[[i]]), collapse = " + "))
      model_num[i] <- i
    }
    if (min == 0) {
      i <- total
      print(paste0(i, " out of ", total))
      m1 <- series %>%
        ungroup() %>%
        dplyr::select(abundance) %>%
        unlist() %>%
        ts(frequency = freq) %>%
        auto.arima(lambda = 0, seasonal = seasonal, xreg = NULL)
      AICc[i] <- m1$aicc
      formula[i] <- "abundance ~ 1"
      model_num[i] <- i
    }
    table <- as_tibble(data.frame(model_num, AICc, formula)) %>%
      arrange(AICc)
  } else {
    table <- NULL
  }
  results <- list(vars, table)
  return(results)
}




data <- read.csv("data/dat.csv")

covariates <- c(
  "lag1_log_JackOPI",
  "lag1_log_SmAdj",
  "lag1_NPGO",
  "lag1_PDO",
  "WSST_A",
  "PDO.MJJ",
  "MEI.OND",
  "UWI.JAS",
  "SST.AMJ",
  "SSH.AMJ",
  "UWI.SON"
)

min_covariates <- 0
max_covariates <- 1
forecast_type <- "preseason"

results <- all_subsets(
  series = data,
  covariates = covariates,
  min = min_covariates,
  max = max_covariates,
  type = forecast_type,
  fit = TRUE
)

cat("Number of models:", length(results[[1]]), "\n")
cat("Best model formula:", results[[2]]$formula[1], "\n")
cat("AICc of the best model:", results[[2]]$AICc[1], "\n")


