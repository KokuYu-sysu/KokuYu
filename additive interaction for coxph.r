# This could set factor level in the function
# Don't have the parameter of 'recode', so you need to adjust the factor levels first
# By default, 'factor' = False

```{R}

additive_interactions = function(model, exposure, CI.level = 0.95, factor = F, levels = c("1", "1")) {
  if (class(model) != "coxph") {
    stop("Model must be of class 'coxph'.")
  }

  if(factor == F){
    exposure1 <- exposure[1]
    exposure2 <- exposure[2]
  } else if (factor == T) {
    exposure1 <- paste0(exposure[1], levels[1])
    exposure2 <- paste0(exposure[2], levels[2])
  }

  interaction.string = paste(exposure1, exposure2, sep=":") 

  if ( !(interaction.string %in% names( coef(model) ) ) ) {
    stop("Could not identify the interaction coefficient in the model formula")
  }

  # name of the interaction term
  keepers = c(exposure1, exposure2, interaction.string)
  
  # get the co-variance matrix
  vcov_matrix = vcov(model)
  V2 = vcov_matrix[keepers, keepers]

  # calculate and extract the interaction coefficient and HR
  b10 = coef(model)[exposure1]
  b01 = coef(model)[exposure2]
  bint = coef(model)[interaction.string]
  HR00 = 1.0 # REF
  HR10 = exp(b10)
  HR01 = exp(b01)
  HR11 = exp(b10 + b01 + bint)

  if( HR10 <= 1 | HR01 <= 1 ) {
    stop("Error: At least one exposure has negative association with outcome. ")
  }
  
  # Calculate RERI
  require(msm)
  RERI = HR11 - HR10 - HR01 + 1
  se_reri = deltamethod( ~ exp(x1 + x2 + x3) - exp(x1) - exp(x2) + 1,
                         mean=c(b10, b01, bint), 
                         cov=V2)
  
  AP = RERI/HR11
  se_ap = deltamethod( ~ (exp(x1 + x2 + x3) - exp(x1) - exp(x2) + 1) / exp(x1 + x2 + x3),
                       mean=c(b10, b01, bint),
                       cov=V2)
  alpha = 1 - CI.level
  z = qnorm(1 - alpha / 2)  # critical value

  SI = (HR11 - 1) / (HR10 + HR01 - 2)
  se_si = deltamethod( ~ (exp(x1 + x2 + x3) - 1) / (exp(x1) + exp(x2) - 2),
                       mean=c(b10, b01, bint),
                       cov=V2)

  p_reri = 2 * pnorm(abs(RERI / se_reri), lower.tail = FALSE)
  p_ap = 2 * pnorm(abs(AP) / se_ap, lower.tail = FALSE)
  p_si = 2 * pnorm(abs(SI - 1) / se_si, lower.tail = FALSE)

  ci_reri_low = RERI - z * se_reri
  ci_reri_high = RERI + z * se_reri
  ci_ap_low = AP - z * se_ap
  ci_ap_high = AP + z * se_ap
  ci_si_low = SI - z * se_si
  ci_si_high = SI + z * se_si

  reri_string = sprintf("RERI: %.2f (%.2f, %.2f)", RERI, ci_reri_low, ci_reri_high)
  ap_string = sprintf("AP: %.2f (%.2f, %.2f)", AP, ci_ap_low, ci_ap_high)
  si_string = sprintf("SI: %.2f (%.2f, %.2f)", SI, ci_si_low, ci_si_high)

  rs <- data.frame(
    stat = c("RERI", "AP", "SI"),
    estimate = c(RERI, AP, SI),
    ci_low = c(ci_reri_low, ci_ap_low, ci_si_low),
    ci_high = c(ci_reri_high, ci_ap_high, ci_si_high),
    p_value = c(p_reri, p_ap, p_si),
    se = c(se_reri, se_ap, se_si)
  )
  cat(paste0("The RERI between ", exposure[1], " and ", exposure[2], " is: ", reri_string, "\n"))
  cat(paste0("The AP between ", exposure[1], " and ", exposure[2], " is: ", ap_string, "\n"))
  cat(paste0("The SI between ", exposure[1], " and ", exposure[2], " is: ", si_string, "\n"))
  return(rs)
  print(rs)
}

# For example, not run
# score has 3 levels: 0, 1, 2
# age has 2 levels: <60 and >=60
# age_interaction_score <- coxph(Surv(fu, event) ~ score*age ,
                data = total3)
# A * B is consitent with A + B + A:B
# additive_interactions(age_interaction_score, c("score", "age"), factor = T, levels = c("1", ">=60"))
```
