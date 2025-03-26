# Reference：
# Suissa, S. “Relative Excess Risk: An Alternative Measure of Comparative Risk.” American Journal of Epidemiology, vol. 150, no. 3, Aug. 1999, pp. 279–82. DOI.org (Crossref), https://doi.org/10.1093/oxfordjournals.aje.a009999.

# Calculate relative excess risk through HR which obtained from survival object

# v1 as denominator in RER calculate and v3 as numeratior in RER calculate
rer_calculate <- function(model, interest_v1, interest_v3){
        # Model could be survival, logistic
        # interest_v1, interest_v3 are the variables of interest

        # 1. Get the RR or HR from the model
        result_RR <- broom::tidy(model, conf.int = TRUE, exponentiate = TRUE) %>%
                select(term, estimate) %>%
                select(term, estimate,) %>%
                filter(grepl(interest_v1, term) | grepl(interest_v3, term))
        
        # 2. Get the RER value
        # interest_v1 as denominator, interest_v3 as numerator
        RR_v3 <- result_RR[2,2]
        RR_v1 <- result_RR[1,2]
        rer <- (RR_v3 - 1) / (RR_v1 - 1)

        # 3. Get the variance of variable
        model_vcov <- vcov(model)
        v_v1 <- model_vcov[interest_v1, interest_v1]
        v_v3 <- model_vcov[interest_v3, interest_v3]
        cov13 <- model_vcov[interest_v1, interest_v3]

        # 4. Calculate 95% CI for RER
        # interest_v1 as denominator, interest_v3 as numerator
        numeratior_v3 <- RR_v3^2 * v_v3
        numeratior_v1 <- RR_v1^2 * rer^2 * v_v1
        numeratior_cov <- 2 * RR_v3 * RR_v1 * rer * cov13
        numerator <- numeratior_v3 + numeratior_v1 - numeratior_cov
        denominator <- (RR_v1 - 1)^2
        rer_var <- 1.96 * sqrt(numerator/denominator)

        # 5. print the result
        result <- data.frame(
                RER = rer,
                RER_CI = sprintf("%0.2f (%0.2f - %0.2f)", rer, rer - rer_var, rer + rer_var)
        )

        return(result$RER_CI)
}

'''usage'''
rer_calculate(cox_object, drug1, drug2)
>>> 12.37 (-0.19 - 24.93)
