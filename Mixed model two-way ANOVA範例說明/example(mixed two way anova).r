library(openxlsx)
library(tidyverse)
library(ggpubr)
library(rstatix)
library(datarium)

data <- read.xlsx("example.xlsx")
data$ID <-factor(data$ID)
data$time <-factor(data$time)
data$Analyzer <-factor(data$Analyzer)

data_A <- data[data$Analyzer=="A",]
data_B <- data[data$Analyzer=="B",]
data_C <- data[data$Analyzer=="C",]
data_D <- data[data$Analyzer=="D",]
data_E <- data[data$Analyzer=="E",]

data_AB <- rbind(data_A,data_B)
data_CD <- rbind(data_C,data_D)
data_AC <- rbind(data_A,data_C)

data_DE <- rbind(data_D,data_E)


rm(list=c("data_A","data_B","data_C","data_D","data_E"))

# Two-way mixed ANOVA test
res.aov <- anova_test(
  data = data_DE, dv = Money, wid = ID,
  between = Analyzer, within = time
)
get_anova_table(res.aov)

#---------------------
# Whole data set
#---------------------
bxp <- ggboxplot(
  data, x = "time", y = "Money",
  color = "Analyzer", palette = "jco"
)
bxp

bxp2 <- ggboxplot(
  data, x = "Analyzer", y = "Money",
  color = "time", palette = "jco"
)
bxp2

#Check assumptions
#Outliers

data %>%
  group_by(time, Analyzer) %>%
  identify_outliers(Money)

#Normality assumption
data %>%
  group_by(time, Analyzer) %>%
  shapiro_test(Money)

ggqqplot(data, "Money", ggtheme = theme_bw()) +
  facet_grid(time ~ Analyzer)

#Homogneity of variance assumption
data %>%
  group_by(time) %>%
  levene_test(Money ~ Analyzer)

#Homogeneity of covariances assumption
box_m(data[, "Money", drop = FALSE], data$Analyzer)

#Computation
# Two-way mixed ANOVA test
res.aov <- anova_test(
  data = data, dv = Money, wid = ID,
  between = Analyzer, within = time
)
get_anova_table(res.aov)

#Post-hoc tests
# Effect of group at each time point
one.way <- data %>%
  group_by(time) %>%
  anova_test(dv = Money, wid = ID, between = Analyzer) %>%
  get_anova_table() %>%
  adjust_pvalue(method = "bonferroni")
one.way

# Pairwise comparisons between group levels
pwc <- data %>%
  group_by(time) %>%
  pairwise_t_test(Money ~ Analyzer, p.adjust.method = "bonferroni")
pwc

# Effect of time at each level of exercises group
one.way2 <- data %>%
  group_by(Analyzer) %>%
  anova_test(dv = Money, wid = ID, within = time) %>%
  get_anova_table() %>%
  adjust_pvalue(method = "bonferroni")
one.way2

# Pairwise comparisons between time points at each group levels
# Paired t-test is used because we have repeated measures by time
pwc2 <- data %>%
  group_by(Analyzer) %>%
  pairwise_t_test(
    Money ~ time, paired = TRUE, 
    p.adjust.method = "bonferroni"
  ) %>%
  select(-df, -statistic, -p) # Remove details
pwc2

data %>%
  pairwise_t_test(
    Money ~ Analyzer, 
    p.adjust.method = "bonferroni"
  )

# Visualization: boxplots with p-values
pwc <- pwc %>% add_xy_position(x = "time")
pwc.filtered <- pwc %>% filter(time != "t1")
bxp + 
  stat_pvalue_manual(pwc.filtered, tip.length = 0, hide.ns = TRUE) +
  labs(
    subtitle = get_test_label(res.aov, detailed = TRUE),
    caption = get_pwc_label(pwc)
  )


# Visualization: boxplots with p-values 2
pwc2 <- pwc2 %>% add_xy_position(x = "Analyzer")

bxp2 + 
  stat_pvalue_manual(pwc2, tip.length = 0, hide.ns = TRUE) +
  labs(
    subtitle = get_test_label(res.aov, detailed = TRUE),
    caption = get_pwc_label(pwc2)
  )
