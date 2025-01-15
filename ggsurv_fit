library(survival)
library(survminer)
library(dplyr)
library(ggplot2)

plot_survival <- function(
    var, 
    data,   
    palette_col = c("#FEAC5E","#6441A5"), # default
    legend_title = "Variable",
    plot_title = "Cumulative Incidence",
    xlab_str = "Time (years)",
    ylab_str = "Survival Probability",
    fun_str  = "event",
    ylim_vec = c(0, 0.02),
    pval_arg = TRUE,                   # Set pval = TRUE to show computed log-rank p-value and maybe it would not show when fun_str == "event"
    pval_txt = NULL,                   # Alternatively, provide a custom p-value text, e.g. "Log-rank Test <0.001"
    conf_int = TRUE,
    run_survdiff = TRUE              # It would run diffsurv if True
) {

  # Change to factor if the var isn't transfered to factor
  if (!is.factor(data[[var]])) {
    data[[var]] <- as.factor(data[[var]])
  }

# Create a survival formula based on input variable
  form_surv <- as.formula(paste("Surv(time, event) ~", var))

# Fit survival curves
  surv_obj <- surv_fit(form_surv, data = data)  # It should be surv_fit but not survfit, or it would meet with error
  
# If the parameter of run_survdiff == T, it would run.
  if (run_survdiff) {
    test_result <- survdiff(form_surv, data = data)
    print(test_result)
  }
  
# Prepare labels from the factor levels of the variable
  labs <- levels(data[[var]])

# Call ggsurvplot
  plot <- ggsurvplot(
    fit           = surv_obj,
    data          = data,
    conf.int      = conf_int,
    pval          = if (!is.null(pval_txt)) pval_txt else pval_arg, 
    legend.title  = legend_title,
    legend.labs   = labs,
    palette       = palette_col,
    title         = plot_title,
    xlab          = xlab_str,
    ylab          = ylab_str,
    fun           = fun_str,
    ylim          = ylim_vec
  )
  return(plot)
}

# When run the code like:
# p <- plot_survival(
#   var = "A",
#   data = dataset,
#   legend_title = "Category of A",
#   ...
# )

# When combine plot, it can use following code:
# for example, there are 10 plot name p1, p2, ... ,p10

library(grid)
library(gridExtra)

plot_list <- mget(ls(pattern = "^p\\d+$")) # in this case, you should sure that the name of plot is p[i]
plot_combined <- lapply(plot_list, function(x) x$plot)
plot_all <- do.call(grid.arrange, c(plot_combined), ncol = 2) # ncol can be change any number >=1, also nrow.
ggsave("name.png", plot_all, width = 10, height = 10) # Commonly each png should be width=5 and height=5.
