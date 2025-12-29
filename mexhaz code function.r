# Charvat, H., & Belot, A. (2021). mexhaz: An R Package for Fitting Flexible Hazard-Based Regression Models for Overall and Excess Mortality with a Random Effect.
# Journal of Statistical Software, 98(14), 1–36. https://doi.org/10.18637/jss.v098.i14

#` @param data Put dataframe for analysis
#` @param time1 Start time for interval censoring
#` @param time2 End time for interval cansoring (Default NULL for right censor)
#` @param expose The main event you are interested in (Default NULL)
#` @param status Event indicator
#` @param event_of_interest Which event you would like to focus if data contains serveral outcome
#` @param continous_vars A vector indicating the continous variable names
#` @param categorical_vars A vector indicating the categorical variable names
#` @param random_effects A variable which you would like to proceed hierarchical analysis
#` @param nph_vars A vector indicating the variables which violate p-h assumption
#` @param base_function Chosen from "weibull", "exp.bs", "exp.ns", and "pw.cst". The detail is in R document of mexhaz package
#` @param degree When using B-spline, degree should be set, and 1 to 3 are accepted. (Default is 3)
#` @param knots A vector containing the time knots, available in "exp.bs", "exp.ns" and "pw.cst"
#` @param summary Whether need to summary (Default is True)
#` @param exactGradHess Decide whether maximisation of the likelihood should be based on the analytic gradient and Hessian computed internally
#` @param ... Represent additional parameters directly passed to mexhaz to control the optimisation process
mexhaz_cox <- function(data
                       time1,
                       time2 = NULL,
                       expose = NULL,
                       status,
                       event_of_interest = 1,
                       continuous_vars = NULL,
                       categorical_vars = NULL,
                       random_effects = NULL,
                       nph_vars = NULL,
                       base_function = "exp.bs",
                       degree = 3,
                       knots = NULL,
                       summary = TRUE,
                       exactGradHess = TRUE, # nolint
                       ...) {

  required_packages <- c("mexhaz")

  for (pkg in required_packages) {
    if (!require(pkg, character.only = TRUE)) {
      stop(paste("Package", pkg, "is required but not installed."))
    }
  }

  if (missing(time1) || missing(status) || missing(data)) {
    stop("Arguments 'time1', 'status', and 'data' are required.")
  }

  all_vars <- c(time1, time2, status, continuous_vars, expose,
                categorical_vars, random_effects)
  missing_vars <- setdiff(all_vars, names(data))
  if (length(missing_vars) > 0) {
    stop(paste("The following variables are not found in data:",
               paste(missing_vars, collapse = ", ")))
  }
  if (!all(nph_vars %in% c(continuous_vars, categorical_vars, expose))) {
    stop("nph variates must be one of continuous_vars, categorical vars, 
          or exposure.")
  }

  analysis_data <- data

  categorical_vars <- c(categorical_vars, expose)
  if (!is.null(categorical_vars)) {
    for (var in categorical_vars) {
      analysis_data[[var]] <- as.factor(analysis_data[[var]])
    }
  }

  if (!is.null(random_effects)) {
    analysis_data[[random_effects]] <- as.factor(
      analysis_data[[random_effects]]
    )
  }

  event_types <- sort(unique(analysis_data[[status]]))
  n_event_types <- sum(event_types != 0)

  cat("Event type distribution:\n")
  event_table <- table(analysis_data[[status]], useNA = "ifany")
  print(event_table)
  cat("\n")

  if (n_event_types > 1) {
    cat(paste("Analyzing event type", event_of_interest))
    status_for_analysis <- ifelse(
      analysis_data[[status]] == event_of_interest, 1, 0
    )
  } else {
    status_for_analysis <- analysis_data[[status]]
  }

  if (is.null(time2)) {
    surv_obj <- Surv(analysis_data[[time1]], status_for_analysis)
  } else {
    surv_obj <- Surv(analysis_data[[time1]], analysis_data[[time2]],
                     status_for_analysis, type = "interval")
  }
  covariate_terms <- c()

  if (!is.null(continuous_vars)) {
    covariate_terms <- c(covariate_terms, continuous_vars)
  }

  if (!is.null(categorical_vars)) {
    covariate_terms <- c(covariate_terms, categorical_vars)
  }

  if (!is.null(nph_vars)) {
    covariate_terms <- c(covariate_terms,
                         paste0("nph(", paste(nph_vars, collapse = " + "), ")"))
  }

  if (length(covariate_terms) == 0) {
    formula_str <- "surv_obj ~ 1"
  } else {
    formula_str <- paste("surv_obj ~", paste(covariate_terms, collapse = " + "))
  }

  flexcox_formula <- as.formula(formula_str)
  cat("Model formula:", deparse(flexcox_formula), "\n\n")

  flexcox_model <- mexhaz::mexhaz(
    formula = flexcox_formula,
    data = analysis_data,
    base = base_function,
    degree = degree,
    knots = knots,
    verbose = 1000,
    random = random_effects,
    exactGradHess = exactGradHess,
    ...
  )
  aic <- -2 * flexcox_model$loglik + 2 * flexcox_model$n.par

  result <- list(
    model = flexcox_model,
    formula = flexcox_formula,
    event_info = list(
      event_types = event_types,
      event_of_interest = event_of_interest,
      event_counts = as.list(event_table)
    ),
    aic = aic
  )

  if (summary) {
    cat("Generating model summary...\n")
    result$summary <- summary(flexcox_model)
    print(result$summary)
  }

  return(result)
}

# At the end, normally you shoule choose the model based on the minimum AIC.
