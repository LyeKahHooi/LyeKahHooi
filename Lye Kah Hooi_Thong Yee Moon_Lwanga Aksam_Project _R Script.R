#Installing the packages
install.packages('tidyverse')
install.packages('janitor')
install.packages('lubridate')
# Loading the packages
library(tidyverse)
library(janitor)
library(lubridate)

# Read the data from github
df <- read.csv("https://raw.githubusercontent.com/yeemoonthong/Statistic-Data-Science/main/Sleep_health_and_lifestyle_dataset.csv")
head(df)

#DATA EXPLORATION
#Modified to display a summary of data structure
## Data Exploration
# List of column names
# colnames(df)
glimpse(df)

# Row and columns of data frame
nrow(df)
ncol(df)

unique(df$Person.ID)

#Dimensions of the data frame
dim(df)
# Observe the first 5 rows of data frame
head(df, n=5)

# Observe the last 5 rows of the data frame
tail(df, n=5)

# random 5 row for observation
sample_n(df, 5)

#DATA PREPROCESSING
# Separate Blood Pressure into 2 col = [systolic/Diastolic]
df <- df %>% separate(Blood.Pressure, c('Systolic.Blood.Pressure', 'Diastolic.Blood.Pressure'))
# Another method
# df[c('Systolic Blood Pressure', 'Diastolic Blood Pressure')] <- str_split_fixed(df$Blood.Pressure, '/', 2)
head(df,n=3)
# To observe the column information and structure of data frame
str(df)
# To change the data type
df <- mutate(df, Person.ID = as.character(Person.ID),
             Systolic.Blood.Pressure = as.integer(Systolic.Blood.Pressure),
             Diastolic.Blood.Pressure = as.integer(Diastolic.Blood.Pressure),
             Sleep.Duration=as.numeric(Sleep.Duration))

str(df)

# To check number of missing value
colSums(is.na(df))
# Ans = no missing value found

#DATA DESCRIPTIVE
# Histogram
# Histogram of Age
df %>%
  ggplot(aes(x = Age)) +
  geom_histogram(binwidth = 1, fill = "cornflowerblue", color = "black") +
  labs(title = "Age Distribution", x = "Age", y = "Count")

# hist(df$Stress.Level,xlim=c(1,10),xlab="Stress Level Ranking",main="Stress Level Ranking Frequency")
# Histogram for Stress Level
df %>%
  filter(!is.na(Stress.Level)) %>%
  ggplot(aes(x = Stress.Level)) +
  geom_histogram(binwidth = 1, fill = "cornflowerblue", color = "black") +
  coord_cartesian(xlim = c(1, 10)) +
  labs(x = "Stress Level Ranking", y = "Frequency", title = "Stress Level Ranking Frequency") +
  theme_minimal()

# Histogram for Quality of Sleep
df %>%
  filter(!is.na(Quality.of.Sleep)) %>%
  ggplot(aes(x = Quality.of.Sleep)) +
  geom_histogram(binwidth = 1, fill = "cornflowerblue", color = "black") +
  coord_cartesian(xlim = c(1, 10)) +
  labs(x = "Quality of Sleep Ranking", y = "Frequency", title = "Quality of Sleep Ranking Frequency") +
  theme_minimal()

# Pie Chart
table(df$Gender)
# Pie Chart of gender
df %>%
  count(Gender) %>%
  mutate(percentage = n / sum(n) * 100) %>%
  ggplot(aes(x = "", y = n, fill = Gender)) +
  geom_bar(stat = "identity", width = 1) +
  geom_text(aes(label = paste0(round(percentage), "%")), position = position_stack(vjust = 0.5), size = 12, color = "white", family = "Courier") +
  coord_polar("y", start = 0) +
  theme_void() +
  theme(
    legend.text = element_text(size = 18),
    legend.title = element_text(size = 18),
    plot.title = element_text(size = 18)
  ) +
  labs(title = "Gender Distribution")

# Bar Plot
table(df$BMI.Category)

# Bar chart for BMI category
df %>%
  ggplot(aes(x = BMI.Category, fill = BMI.Category)) +
  geom_bar() +
  scale_fill_manual(values = c("cornflowerblue", "darkorange", "lightgreen", "pink", "yellow")) +
  geom_text(stat = 'count', aes(label = ..count..), vjust = -1) +
  labs(title = "BMI Category Distribution", x = "BMI Category", y = "Count") +
  theme_minimal()

# Bar chart of Sleep Disorder by BMI Category
ggplot(df, aes(x = BMI.Category, fill = Sleep.Disorder)) +
  geom_bar(position = "dodge") +
  labs(title = "Sleep Disorder by BMI Category", x = "BMI Category", y = "Count") +
  theme_minimal() +
  theme(
    axis.title.x = element_text(size = 14),
    axis.text.x = element_text(size = 14),
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.text.y = element_text(size = 14)
  )

# Bar chart of Sleep Disorder by Gender
ggplot(df, aes(x = Gender, fill = Sleep.Disorder)) +
  geom_bar(position = "dodge") +
  labs(title = "Sleep Disorder by Gender", x = "Gender", y = "Count") +
  theme_minimal() +
  theme(
    axis.title.x = element_text(size = 14),
    axis.text.x = element_text(size = 14),
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.text.y = element_text(size = 14)
  )

#TABLE 
table(df$Occupation)

df %>%
  ggplot(aes(x = Occupation, fill = Occupation)) +
  geom_bar(color = "black", fill = "cornflowerblue") +
  geom_text(stat = 'count', aes(label = ..count..), vjust = -1, color = "white") +
  labs(title = "Occupation Distribution", x = "Occupation", y = "Count") +
  scale_x_discrete(guide = guide_axis(angle = 45)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Single Box Plot
boxplot(df$Heart.Rate,
        horizontal = TRUE,
        main = 'Heart Rate Distribution',
        xlab = 'Heart Rate',
        col = 'cornflowerblue',
        border = 'black')

boxplot(df$Daily.Steps,
        horizontal = TRUE,
        main = 'Daily Steps Distribution',
        xlab = 'Daily Steps',
        col = 'cornflowerblue',
        border = 'black')

# Systolic Blood Pressure Boxplot
SBP_Plot <- boxplot(df$Systolic.Blood.Pressure,
                    horizontal = TRUE,
                    main = 'Systolic Blood Pressure Distribution',
                    xlab = 'Systolic Blood Pressure',
                    col = 'cornflowerblue',
                    border = 'black',
                    names = 'Systolic')

# Diastolic Blood Pressure Boxplot
DBP_Plot <- boxplot(df$Diastolic.Blood.Pressure,
                    horizontal = TRUE,
                    main = 'Diastolic Blood Pressure Distribution',
                    xlab = 'Diastolic Blood Pressure',
                    col = 'cornflowerblue',
                    border = 'black',
                    names = 'Diastolic')

# Plot the combined boxplot
mylist <- list(SBP_Plot, DBP_Plot)
groupbxp <- do.call(mapply, c(cbind, mylist))
bxp(groupbxp, main = 'Systolic and Diastolic Blood Pressure Distribution',
    xlab = 'Blood Pressure',
    col = c('cornflowerblue', 'cornflowerblue'),
    names = c('Systolic', 'Diastolic'))


# Box plot
boxplot(df$Quality.of.Sleep ~ df$Sleep.Disorder,
        data = df,
        main = 'Quality of Sleep based on Sleep Disorder Category',
        xlab = 'Sleep Disorder',
        ylab = 'Quality of Sleep',
        col = 'cornflowerblue',
        border = 'black')

boxplot(df$Sleep.Duration ~ df$Sleep.Disorder,
        data = df,
        main = 'Sleep Duration based on Sleep Disorder Category',
        xlab = 'Sleep Disorder',
        ylab = 'Sleep Duration',
        col = 'cornflowerblue',
        border = 'black')

# Define a color palette with a sufficient number of colors
color_palette <- rainbow(length(unique(df$Occupation)))

# Increase the left margin to accommodate the y-axis label
par(mar = c(5, 10, 4, 2) + 0.1)

boxplot(df$Physical.Activity.Level ~ df$Occupation,
        data = df,
        main = 'Physical Activity Level based on Occupation Type',
        xlab = 'Physical Activity Level',
        ylab = '',
        horizontal = TRUE,
        col = color_palette,
        border = 'black',
        las = 1)  # Rotate x-axis labels horizontally
# Define a color palette with a sufficient number of colors
color_palette <- rainbow(length(unique(df$Occupation)))

# Increase the left margin to accommodate the y-axis label
par(mar = c(5, 10, 4, 2) + 0.1)

boxplot(df$Quality.of.Sleep ~ df$Occupation,
        data = df,
        main = 'Quality of Sleep based on Occupation Type',
        xlab = 'Quality of Sleep',
        ylab = '',
        horizontal = TRUE,
        col = color_palette,
        border = 'black',
        las = 1)  # Rotate x-axis labels horizontally

# Scatter Plot of Heart Rate vs Systolic Blood Pressure
df %>%
  ggplot(aes(x = Heart.Rate, y = Systolic.Blood.Pressure)) +
  geom_point(color = 'cornflowerblue', alpha = 0.6) +
  labs(x = "Heart Rate", y = "Systolic BP", title = "Heart Rate vs Systolic BP") +
  theme_minimal() +
  theme(axis.text = element_text(color = "black"),
        axis.title = element_text(color = "black"),
        plot.title = element_text(hjust = 0.5))

#Scatter Plot of Heart Rate vs Diastolic Blood Pressure
df %>%
  ggplot(aes(x = Heart.Rate, y = Diastolic.Blood.Pressure)) +
  geom_point(color = 'cornflowerblue', alpha = 0.6) +
  labs(x = "Heart Rate", y = "Diastolic BP", title = "Heart Rate vs Diastolic BP") +
  theme_minimal() +
  theme(axis.text = element_text(color = "black"),
        axis.title = element_text(color = "black"),
        plot.title = element_text(hjust = 0.5))
#Scatter Plot of Physical Activity Level vs Daily Steps
df %>%
  ggplot(aes(x = Physical.Activity.Level, y = Daily.Steps)) +
  geom_point(color = 'cornflowerblue', alpha = 0.6) +
  labs(x = "Physical Activity Level", y = "Daily Steps", title = "Activity Level vs Daily Steps") +
  theme_minimal() +
  theme(axis.text = element_text(color = "black"),
        axis.title = element_text(color = "black"),
        plot.title = element_text(hjust = 0.5))

#Stem & Leaf
# Steam & Leaf
#Stem-and-leaf plot for Sleep Duration
cat("Stem-and-Leaf Plot for Sleep Duration:\n")
stem(df$Sleep.Duration, scale = 0.5)


#Stem-and-leaf plot for Daily Steps
cat("\nStem-and-Leaf Plot for Daily Steps:\n")
stem(df$Daily.Steps, scale = 1)

#STATISTICAL SUMMARY
# Statistical summary of data. basic descriptive statistics
summary(df)

unique(df$Occupation)
# Hypothesis Testing
# Assume the mean sleep duration should be 8 hours per day.
# In a sample of 374 respondents, it was found that sleep duration is less than 8 hours on average.
# At 0.05 significance level can we claim that the mean sleep duration of the population is less than 8 hours per day?

# H0 : mean = 8
# H1 : mean < 8 (claim)

n <- nrow(df)
sigma <- sd(df$Sleep.Duration)
alpha = 0.05
mean_0 = 8
mean_duration= mean(df$Sleep.Duration)

z <- (mean_duration-mean_0)/(sigma/sqrt(n))
z.alpha = qnorm(1-alpha)
z.alpha
#alternative solution: using p value One-sample t-test
pval = pnorm(z)
pval

#ONE_SAMPLE T-TEST
#One-sample t-test for Sleep Duration against the recommended 8 hours
t_test_result <- t.test(df$Sleep.Duration, mu = 8, alternative = "less")
t_test_result

# GOODNESS OF FIT TEST
# A researcher found that 15% of the world population suffer from insomnia,
# 18% of them suffer from sleep apnea and the rest 67% have no sleep disorder.
# At 0.05 significance level, test the claim that the percentages are equal.

# H0 : 15% of the population suffer from insomnia,18% of them suffer from sleep apnea and the rest 67% have no sleep disorder.(Claim)
# H1 : The distribution is not the same as stated in the null hypothesis.

# Count occurrences of each category
count_insomnia <- sum(df$Sleep.Disorder == "Insomnia")
count_sleep_apnea <- sum(df$Sleep.Disorder == "Sleep Apnea")
count_none <- sum(df$Sleep.Disorder == "None")

# Total count
total_count <- nrow(df)

# Calculate expected proportions
expected_insomnia <- count_insomnia / total_count
expected_sleep_apnea <- count_sleep_apnea / total_count
expected_none <- count_none / total_count

# Perform chi-square goodness-of-fit test
observed <- c(expected_insomnia, expected_sleep_apnea, expected_none)  # Observed proportions
expected <- c(0.15, 0.18, 0.67)  # Expected proportions under the null hypothesis
chi_squared_test <- chisq.test(observed, p = expected)

alpha <- 0.05  # Significance level

print(chi_squared_test)

if (chi_squared_test$p.value < alpha) {
  print("Reject null hypothesis: The distribution is not the same as stated in the null hypothesis.")
} else {
  print("Fail to reject null hypothesis: The distribution is the same as stated in the null hypothesis.")
}
#CHI-SQUARE TEST
# Chi-Square Test of Independence for Gender and Sleep Disorder at 0.05 significance level.

# H0 : Sleep disorder is independent of gender.
# H1 : Sleep disorder is dependent on gender.(Claim)

contingency_table <- table(df$Gender, df$Sleep.Disorder)
contingency_table

chi_sq_test <- chisq.test(contingency_table)
chi_sq_test

#CORRELATION

# Correlation between Sleep Duration and Quality of Sleep
# Added to perform a correlation test and provide a p-value
cor_test_result <- cor.test(df$Sleep.Duration, df$Quality.of.Sleep)
cor_test_result

# Create scatter plot
plot(df$Sleep.Duration, df$Quality.of.Sleep,
     xlab = "Sleep Duration", ylab = "Quality of Sleep",
     main = "Scatter Plot of Sleep Duration vs Quality of Sleep")


#REGRESSION
# Perform linear regression of Age versus Quality of Sleep
model <- lm(df$Quality.of.Sleep ~ df$Age, data = df)

# Summary of the regression model
summary(model)

# Plot the regression line
plot(df$Age, df$Quality.of.Sleep,
     xlab = "Age", ylab = "Quality of Sleep",
     main = "Regression Line of Age vs Quality of Sleep")
abline(model, col = "red")


# ANOVA to compare Quality of Sleep across different Occupations

# H0 : All occupations have same quality of sleep mean.
# H1 : At least one occupation has different quality of sleep mean.(Claim)

# Converts Occupation into a factor variable
df$Occupation <- as.factor(df$Occupation)
df$Occupation

anova_test <- aov(Quality.of.Sleep ~ Occupation, data = df)
anova_test

anova_summary <- summary(anova_test)
anova_summary
