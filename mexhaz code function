# Charvat, H., & Belot, A. (2021). mexhaz: An R Package for Fitting Flexible Hazard-Based Regression Models for Overall and Excess Mortality with a Random Effect.
# Journal of Statistical Software, 98(14), 1–36. https://doi.org/10.18637/jss.v098.i14

library(mexhaz)
mexhaz_model <- function(data, variable, status, base, knots, random_cov = "random", nph = TRUE) {

  # Create base formula based on status (mayve for cause-specific model)
  if (status == 1) {
    formula_str <- "Surv(f_LC, status_lc == 1) ~ "
  } else if (status == 2) {
    formula_str <- "Surv(f_LC, status_lc == 2) ~ "
  } else {
    stop("Status must be either 1 or 2.")
  }
  
  # Convert variable to factor
  data[[variable]] <- as.factor(data[[variable]])
  
  # Define covariates etc.
  covs <- c("age", "sex",
            "education", "smoking",
            "family_history", "bmi", "copd",
            "income")
  
  # Combine covariates with '+' to create formula string
  covs_str <- paste(covs, collapse = " + ")
  
  # Add the main variable to the formula
  formula_str <- paste0(formula_str, covs_str, " + ", variable)
  
  # Set non-proportional hazard terms based on status
  if (nph == TRUE) {
    if (status == 1) {
      nph_str <- paste0(" + nph(age + ", variable, ")")
    } else {
      nph_str <- " + nph(age + bmi)"
    }
  }
  
  # Create final formula string and convert to formula object
  formula_str <- paste0(formula_str, nph_str)
  final_formula <- as.formula(formula_str)

  model <- mexhaz(formula = final_formula,
                 data = data,
                 base = base,
                 knots = knots,
                 random = random_cov,
                 verbose = 1000)
  aic <- -2 * model$loglik + 2 * model$n.par
  return(list(model = model, aic = aic))
}
