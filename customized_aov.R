
# Load required libraries
library(dplyr)
library(ggplot2)  
library(multcompView)  


# Create the data frame
data <- data.frame(
  group = as.factor(c("A","B","C","A","B","C","A","B","C","A","B","C","A","B","C","A","B","C")),
  value_1 = c(1:3, 4:6, 1:3, 1:3, 4:6, 1:3),
  value_2 = c(3:1, 6:4, 3:1, 1:3, 4:6, 1:3)
)


# Define the test function
customized_aov <- function(data.name, col_1, col_2) {
  # Use as.formula to create a formula dynamically
  formula <- as.formula(paste(col_1, "~", col_2))
  
  # Perform ANOVA
  aov_result <- aov(formula, data = data.name)
  # print(summary(aov_result))
  
  # Perform Tukey HSD test
  tukey_result <- TukeyHSD(aov_result)
  tukey_cld <- multcompLetters4(aov_result, tukey_result)
  
  # Extract group labels
  group_labels <- data.frame(group = names(tukey_cld$group$Letters), 
                             Letters = tukey_cld$group$Letters)
  
  # Join group_labels with the original data
  data_summary <- data.name %>%
    group_by(!!sym(col_2)) %>%
    summarise(mean = mean(!!sym(col_1)), sd = sd(!!sym(col_1)), .groups = 'drop')
  
  max_value <- data.name %>%
    group_by(!!sym(col_2)) %>%
    summarize(MaxValue = max(!!sym(col_1)))
  
  group_labels <- group_labels %>% left_join(max_value, by = "group")
  
  # Create plot
  Fig_plot <- ggplot(data.name, aes_string(x = col_2, y = col_1, fill = col_2)) +
    geom_violin(aes(group = !!sym(col_2))) +
    geom_dotplot(binaxis = "y", stackdir = "center", binwidth = 0.1, dotsize = 0.1, position = position_dodge(0)) +
    geom_text(data = group_labels, aes(label = Letters, x = group, y = MaxValue*1.1)) +
    labs(title = "One-way ANOVA with Tukey HSD", x = "", y = col_1) +  
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 0.5))
  
  # Create dynamic filename
  file_name <- paste0(deparse(substitute(data.name)), "_", col_1, "_", col_2, ".png")
  
  # Save plot
  ggsave(file_name, plot = Fig_plot, scale = 1, width = 1200, height = 627, units = "px", dpi = 144, limitsize = TRUE)
  }

# Call the test function
customized_aov(data, col_1 = "value_1", col_2 = "group")
