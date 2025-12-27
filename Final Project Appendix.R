#Install packages
install.packages("ggpubr")
install.packages("psych")
install.packages("DescTools")
install.packages("emmeans")
install.packages("multcompView")

# Load necessary packages
library(tidyverse)
library(car)
library(ggplot2)
library(ggpubr)
library(psych)
library(DescTools)
library(emmeans)
library(multcompView)

# Read the dataset
df <- read.csv("salaries_cyber.csv")

# Convert experience_level to an ordinal numeric variable
df$experience_numeric <- dplyr::recode(df$experience_level,
                                       "EN" = 1,
                                       "MI" = 2,
                                       "SE" = 3,
                                       "EX" = 4)

# Create remote_group (Remote = 100, Non-Remote = 0 or 50)
df$remote_group <- ifelse(df$remote_ratio == 100, 
                          "Remote", 
                          "Non-Remote")

df$remote_group <- factor(df$remote_group)

# Boxplot to identify extreme salary outliers
boxplot(df$salary_in_usd, main = "Salary Outliers")

str(df)
summary(df)

#5A.1 Scatterplots
# Scatterplot: Salary vs Experience
ggplot(df, aes(x = experience_numeric, y = salary_in_usd)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm") +
  labs(title = "Salary vs Experience")

# Scatterplot: Salary vs Remote Ratio
ggplot(df, aes(x = remote_ratio, y = salary_in_usd)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm") +
  labs(title = "Salary vs Remote Ratio")

#5A.2 Correlation Matrix: Numeric variables for correlation
numeric_vars <- df %>% 
  select(salary_in_usd, experience_numeric, remote_ratio)

pairs.panels(numeric_vars)

#5A.3 Multiple Linear Regression model
model_mlr <- lm(salary_in_usd ~ experience_numeric + remote_ratio, data = df)

summary(model_mlr)

#5A.5 Model significance tests: ANOVA table for regression
anova(model_mlr)


#5A.6 R-squared and Adjusted R-squared
summary(model_mlr)$r.squared
summary(model_mlr)$adj.r.squared


#5A.7 Diagnostic plots
par(mfrow = c(2,2))
plot(model_mlr)
par(mfrow = c(1,1))

#5A.8 Normality Test
shapiro.test(residuals(model_mlr))


# 5A.9 Homoscedasticity test
ncvTest(model_mlr)

#STEP 5B: ONE-WAY ANOVA
# 5B.1 Boxplot
ggplot(df, aes(x = company_size, y = salary_in_usd)) +
  geom_boxplot() +
  labs(title = "Salary by Company Size")

#5B.2 Summary Statistics
df %>% group_by(company_size) %>% 
  summarise(mean_salary = mean(salary_in_usd),
            sd_salary = sd(salary_in_usd),
            n = n())


#5B.3 ANOVA model
anova1 <- aov(salary_in_usd ~ company_size, data = df)
summary(anova1)


# 5B.4 Tukey HSD + Compact Letter Display
# Tukey test
tukey <- TukeyHSD(anova1)
tukey

# Compact letter display
cld <- multcompLetters4(anova1, tukey)
cld


#5B.5 Assumption checks
# Normality check
shapiro.test(residuals(anova1))

# Homogeneity of variance
leveneTest(salary_in_usd ~ company_size, data = df)


#5B.6 Effect size
EtaSq(anova1)


# STEP 5C: TWO-WAY ANOVA
# 5C.1 Interaction plot
interaction.plot(df$experience_level, df$remote_group, df$salary_in_usd,
                 col = c("blue", "red"), lwd = 2,
                 ylab = "Salary", xlab = "Experience Level",
                 trace.label = "Remote Group")


# 5C.2 Two-way ANOVA model
anova2 <- aov(salary_in_usd ~ experience_level * remote_group, data = df)
summary(anova2)


# 5C.3 Post-hoc tests (if interaction significant)
emmeans(anova2, pairwise ~ experience_level | remote_group)
emmeans(anova2, pairwise ~ remote_group | experience_level)


# 5C.4 Assumption checks
# Normality
shapiro.test(residuals(anova2))

# Homogeneity of variance
leveneTest(salary_in_usd ~ experience_level * remote_group, data = df)


# 5C.5 Effect size
EtaSq(anova2)












