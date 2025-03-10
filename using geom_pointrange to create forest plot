# For result contains several group by one variable
# Such as one variable contain several HRs and CIs

library(ggplot2)
library(dplyr)

# IF data likes HR (95% CI) it should extract HR and Lower CI and upper CI alone

extract_hr_ci <- function(hr_string) {
  # Extract HR value
  hr <- as.numeric(str_extract(hr_string, "^\\d+\\.?\\d*"))
  
  # Extract CI values
  ci_part <- str_extract(hr_string, "\\(.*\\)")
  ci_values <- str_extract_all(gsub("[\\(\\)]", "", ci_part), "\\d+\\.?\\d*")[[1]]
  lci <- as.numeric(ci_values[1])
  uci <- as.numeric(ci_values[2])
  
  return(list(hr = hr, lci = lci, uci = uci))
}

# For example: my colnames are HR_1, HR_2 and so on.
hr_columns <- grep("^HR_[1-5]", names(major), value = TRUE)
hr_matrix <- matrix(NA, nrow = nrow(major), ncol = length(hr_columns))
lci_matrix <- matrix(NA, nrow = nrow(major), ncol = length(hr_columns))
uci_matrix <- matrix(NA, nrow = nrow(major), ncol = length(hr_columns))

# Use 'For' loop to extract 
for (i in 1:length(hr_columns)) {
  for (j in 1:nrow(major)) {
    if (!is.na(major[j, hr_columns[i]])) {
      values <- extract_hr_ci(major[j, hr_columns[i]])
      hr_matrix[j, i] <- values$hr
      lci_matrix[j, i] <- values$lci
      uci_matrix[j, i] <- values$uci
    }
  }
}

# Data frame example
head(df)
>            label HR_1 LCL_1 UCL_1 HR_2 LCL_2 UCL_2 HR_3 LCL_3 UCL_3 HR_4 LCL_4 UCL_4 HR_5 LCL_5 UCL_5
1 Characteristics   NA    NA    NA   NA    NA    NA   NA    NA    NA   NA    NA    NA   NA    NA     NA
2       Variable1 1.09  0.98  1.21 1.27  1.05  1.53 2.38  2.31  2.44 1.07  0.94  1.23 1.02  0.79   1.23
3           Level   NA    NA    NA   NA    NA    NA   NA    NA    NA   NA    NA    NA   NA    NA     NA
4               0   NA    NA    NA   NA    NA    NA   NA    NA    NA   NA    NA    NA   NA    NA     NA
5               1 1.05  0.98  1.32 1.00  0.88  1.14 1.95  1.61  2.69 1.01  0.92  1.10 1.00  0.84   1.23
6               2 1.10  0.98  1.23 1.23  0.99  1.51 3.03  2.93  3.13 1.01  0.87  1.18 1.04  0.77   1.23

# Trans into long data frame
df_long <- data.frame(
  label = rep(df$label, 5),
  transition = rep(1:5, each = nrow(df)),
  HR = c(df$HR_1, df$HR_2, df$HR_3, df$HR_4, df$HR_5),
  LCL = c(df$LCL_1, df$LCL_2, df$LCL_3, df$LCL_4, df$LCL_5),
  UCL = c(df$UCL_1, df$UCL_2, df$UCL_3, df$UCL_4, df$UCL_5)
)

# Remove NA values
df_long <- df_long %>% filter(!is.na(HR))

# Create group labels for faceting or coloring
df_long$transition_label <- factor(
  df_long$transition,
  levels = 1:5,
  labels = c("Baseline -> incidence",
             "Baseline -> mortality",
             "Baseline -> Non specific mortality",
             "incidence -> mortality",
             "incidence -> Non specific mortality")
)

# Identify header rows for bold formatting
header_rows <- c(3, 9, 14, 21, 26) # Choose category label often
df_long$is_header <- df_long$label %in% df$label[header_rows]

# Set up for creating stripe to rows for a better version effect
df_long <- df_long %>%
  arrange(label) %>%  # Maybe sometimes this code should be deleted
  mutate(
    # Create a unique identifier for each unique label
    label_id = match(label, unique(label)),
    # Create alternating pattern
    is_even_row = (label_id %% 2 == 0)
  )

# IF use arrange() function but want to recover
unique_labels <- unique(df_long$label)
bg_data <- data.frame(
  label = unique_labels,
  # Simple alternating pattern
  is_even = seq_along(unique_labels) %% 2 == 0,
  # Mark header rows
  is_header = unique_labels %in% df$label[header_rows],
  # Y position for plotting (in reverse order)
  y_pos = length(unique_labels):1
)

# Create the forest plot
p <- ggplot() +
# Add background stripes for all rows, including NA
  geom_rect(data = bg_data,
            aes(xmin = 0, xmax = 12,  # Use 0 as the minimum
                ymin = y_pos - 0.45, ymax = y_pos + 0.45,
                fill = is_even),  # For background setting
            alpha = 0.2) +
# Add reference line at HR=1
  geom_vline(xintercept = 1, linetype = "dashed", color = "#665656d7") +
# Add points and error bars only for non-NA rows
  geom_pointrange(data = all_rows_df %>% filter(!is.na(HR)),
                  aes(x = HR * (1),
                      y = length(all_labels) - match(label, all_labels) + 1,
                      color = transition_label,
                      xmin = LCL * (1),
                      xmax = UCL * (1), 
                      size = is_header),
                  position = position_dodge(width = 1), # To depart the overlapped errorline
                  shape = 20, fatten = 3) +  # shape of mean
# Use linear scale from 0 to 12
  scale_x_continuous(
    breaks = c(0, 1, 2, 4, 6, 8, 10, 12),
    limits = c(0, 12)
  ) +
# Define colors for the 5 transitions
  scale_color_manual(values = c("#1B9E77", "#ff9d51", "#3425fa", "#E7298A", "#5f635b"),
                     name = "Transition",
                     labels = c("Baseline -> incidence",
                                "Baseline -> mortality",
                                "Baseline -> Non specific mortality",
                                "incidence -> mortality",
                                "incidence -> Non specific mortality")) +
# Set point sizes for regular vs header rows
  scale_size_manual(values = c(0.8, 1.2), guide = "none") +
# Set colors for alternating backgrounds
  scale_fill_manual(values = c("white", "#919090"), guide = "none") +
# Set custom y axis with all labels in original order
  scale_y_continuous(
    breaks = bg_data$y_pos,
    labels = bg_data$label,
    expand = expansion(add = 0.5)
  ) +
# Labels
  labs(x = "Hazard Ratio (95% CI)", y = "") +
# Customize theme
  theme_bw() +
  theme(
# Place legend inside the plot
    legend.position = c(0.9, 0.4),    # Adjust these values to change position
    legend.justification = c(1, 0),   # Anchor point
    legend.direction = "vertical",
    legend.background = element_rect(fill = alpha("white", 0.7)),  # Semi-transparent background
    legend.spacing.y = unit(0.1, "cm"),
    legend.key = element_rect(color = "gray70", size = 0.2),  # Add border around legend keys
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 10, face = "bold"),
    legend.margin = margin(6, 6, 6, 6),

    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(color = "gray90"),
    strip.background = element_blank(),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 12, face = "bold"),
    plot.margin = margin(1, 1, 1, 1, "cm"),
  # Make header rows bold
    axis.text.y = element_text(face = ifelse(bg_data$is_header, "bold", "plain"))
  )

# Sometimes would make warning()
> Warning message:
  Vectorized input to `element_text()` is not officially supported.
  i Results may be unexpected or may change in future versions of ggplot2.
