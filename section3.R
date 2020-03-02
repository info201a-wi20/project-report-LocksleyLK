# install.packages("ggplot2")
library("ggplot2")
# install.packages("dplyr")
library("dplyr")
# install.packages("tidyr")
library("tidyr")


# Lynzley's Question 3 Analysis
tech_salary <- read.csv("reddit_tech_salary_sheet.csv", stringsAsFactors = FALSE)

View(tech_salary)

# pull out gender and salary for from Reddit Tech Data
tech_salary <- tech_salary %>%
  select(
    Total.Base.Salary.in.2018..in.USD., 
    Gender
  )

individual_wealth <- individual_wealth[individual_wealth$Variable == "Mean net wealth per person (current prices)", ]

# Seperate salaries into gender categories
tech_salary_males <- tech_salary[tech_salary$Gender == "Male", ]
tech_salary_females <- tech_salary[tech_salary$Gender == "Female", ]

# find average salaries
tech_salary_males_mean <- mean(tech_salary_males$Total.Base.Salary.in.2018..in.USD., na.rm = TRUE)
tech_salary_males_mean <- round(tech_salary_males_mean, digits = 2)
tech_salary_females_mean <- mean(tech_salary_females$Total.Base.Salary.in.2018..in.USD., na.rm = TRUE)
tech_salary_females_mean <- round(tech_salary_females_mean, digits = 2)

# creating bar chart
salary_means <- data.frame(
  "Gender" = c("Male", "Female"),
  "Salary" = c(tech_salary_males_mean, tech_salary_females_mean)
)
View(salary_means)

# creating bar chart for visual analysis of gender global wage gap
lynzley_plot <- ggplot(data = salary_means) +
  geom_col(aes(x = Gender, y = Salary)) +
  labs(
    title = "Average Salary by Gender",
    x = "Gender",
    y = "Salary"
  )
lynzley_plot


