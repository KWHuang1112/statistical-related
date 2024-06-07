library(readxl)
library(dplyr)
library(ggplot2)
library(multcompView)

# # read excel file
# file_path <- "data_TNF_ALPHA.xlsx"
# data <- read_excel(file_path)
# 
# # data selection
# current_data <- data %>% filter(Type == "Control_ES")

current_data <- data.frame(Group=c("Control","Control","Control", 
                           "Group_A", "Group_A", "Group_A",
                           "Group_B", "Group_B", "Group_B",
                           "Group_C", "Group_C", "Group_C",
                           "Group_D", "Group_D", "Group_D",
                           "Control","Control","Control", 
                           "Group_A", "Group_A", "Group_A",
                           "Group_B", "Group_B", "Group_B",
                           "Group_C", "Group_C", "Group_C",
                           "Group_D", "Group_D", "Group_D", 
                           "Group_C", "Group_C", "Group_C",
                           "Group_D", "Group_D", "Group_D" ),
                   TNF_ALPHA=c(100, 100, 100, 
                               92.9417357848625, 94.1323177512788, 92.9673159917764, 
                               73.9188529611381, 71.8351252061385, 70.6887644827109, 
                               83.0844876292385, 84.8342981879207, 77.2609883734105, 
                               70.6887644827109, 76.1670153054646, 77.8692257103406, 
                               80.6455116008868, 82.1916425637897, 79.6667220662332, 
                               66.393570143358, 72.4229804749947, 73.6356650519804, 
                               75.4637469046423, 72.3365321379257, 73.6356650519804, 
                               75.4549897120337, 77.6850890472645, 74.5834025827916, 
                               78.6220878137068, 74.5834025827916, 77.6850890472645, 
                               87.6571640247118, 82.1766799680702, 78.2342853537108, 
                               84.4332889565395, 81.3756318767475, 87.5196590330911)
)
# ANOVA
anova_result <- aov(TNF_ALPHA ~ Group, data = current_data)

# Result of ANOVA
summary(anova_result)

# Tukey multiple comparision
tukey_result <- TukeyHSD(anova_result)

# Result of Tukey multiple comparision
print(tukey_result)

# Significance 
tukey_cld <- multcompLetters4(anova_result, tukey_result)

# Extract info
group_labels <- data.frame(Group = names(tukey_cld$Group$Letters), 
                           Letters = tukey_cld$Group$Letters)



# Join group_labels and data 
current_data <- current_data %>% left_join(group_labels, by = "Group")

### test
max_value <- current_data %>%
  group_by(Group) %>%
  summarize(MaxValue=max(TNF_ALPHA))

group_labels <- group_labels%>% left_join(max_value, by = "Group")

# reorder the X-axis
current_data$Group <- factor(current_data$Group,
                             levels = c("Control",
                                       "Group_A","Group_B","Group_C","Group_D")
)


# Plot
ggplot(current_data, aes(x = Group, y = TNF_ALPHA)) +
  geom_boxplot(aes(colour = Group)) +
  # geom_text(aes(label = Letters, y = TNF_ALPHA + 1), vjust = 0) +
  geom_text(data=group_labels,aes(label = Letters, x = Group,y=MaxValue+1))+
  # theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=0.5))+
  labs(title = "One-way ANOVA with Tukey HSD",
       x = "Group",
       y = "TNF_ALPHA")