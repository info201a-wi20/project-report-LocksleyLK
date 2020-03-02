
# Reddit Tech Salary DataFrame.

tech_salary <- read.csv("reddit_tech_salary_sheet.csv", stringsAsFactors = FALSE)

# Wrangling dataframe to only include necessary columns.
tech_salary <- tech_salary %>%
  select(
    Company.Name, 
    Primary.Location..Country., 
    Primary.Location..City., 
    Highest.Level.of.Formal.Education.Completed, 
    Total.Base.Salary.in.2018..in.USD., 
    Gender
  )

# Creating a small sample of the dataframe.
tech_salary_sample <- tech_salary[2:4, ]


# Individual Wealth Dataframe.
individual_wealth <- read.csv("individual wealth.csv", stringsAsFactors = FALSE)

# Only including one variable.
individual_wealth <- individual_wealth[individual_wealth$Variable == "Mean net wealth per person (current prices)", ]

# Wrangling to only the necessary columns.
individual_wealth <- individual_wealth %>%
  select(
    Country,
    Variable,
    Population,
    Value
  )

# Creating a small sample of the dataframe.
individual_wealth_sample <- individual_wealth[1:3, ]


# For our tech slararies data table, the main features we focused on was 
# salary in relation to highest level of education and gender.

# Removing all salaries that are null, greater than $250000 and less than $10000.
tech_salary_shrunk <- tech_salary[tech_salary$Total.Base.Salary.in.2018..in.USD. < 250000, ]
tech_salary_shrunk <- tech_salary_shrunk[tech_salary_shrunk$Total.Base.Salary.in.2018..in.USD. > 10000, ]
tech_salary_shrunk <- tech_salary_shrunk[2:nrow(tech_salary_shrunk), ]

summary(tech_salary_shrunk$Total.Base.Salary.in.2018..in.USD.)

# Including only male and female genders.
tech_salary_gender <- tech_salary_shrunk[tech_salary_shrunk$Gender == "Male" | tech_salary_shrunk$Gender == "Female", ]

# Getting a summary of the female data.
tech_female <- tech_salary_shrunk[tech_salary_shrunk$Gender == "Female", ]
summary(tech_female$Total.Base.Salary.in.2018..in.USD.)

# Getting a summary of the male data.
tech_male <- tech_salary_shrunk[tech_salary_shrunk$Gender == "Male", ]
summary(tech_male$Total.Base.Salary.in.2018..in.USD.)

# Creating the first gender plot.
gender_plot <- ggplot(data = tech_salary_gender) +
  geom_boxplot(
    mapping = aes(x = Gender, y = Total.Base.Salary.in.2018..in.USD.)
  )

# Wrangling the education to include the top four degrees.
tech_salary_education <- tech_salary_shrunk[tech_salary_shrunk$Highest.Level.of.Formal.Education.Completed == "High School" |
                                           tech_salary_shrunk$Highest.Level.of.Formal.Education.Completed == "Undergraduate Degree" |
                                           tech_salary_shrunk$Highest.Level.of.Formal.Education.Completed == "Graduate Degree" |
                                           tech_salary_shrunk$Highest.Level.of.Formal.Education.Completed == "Doctorate or Post-doctorate",
                                         ]

# The below code creates a summary of all the education relative to gender.
tech_highschool <- tech_salary_shrunk[tech_salary_shrunk$Highest.Level.of.Formal.Education.Completed == "High School",]
summary(tech_highschool$Total.Base.Salary.in.2018..in.USD.)

tech_associate <- tech_salary_shrunk[tech_salary_shrunk$Highest.Level.of.Formal.Education.Completed == "Associates Degree",]
summary(tech_associate$Total.Base.Salary.in.2018..in.USD.)

tech_undergraduate <- tech_salary_shrunk[tech_salary_shrunk$Highest.Level.of.Formal.Education.Completed == "Undergraduate Degree", ] 
summary(tech_undergraduate$Total.Base.Salary.in.2018..in.USD.)

tech_graduate <- tech_salary_shrunk[tech_salary_shrunk$Highest.Level.of.Formal.Education.Completed == "Graduate Degree", ] 
summary(tech_graduate$Total.Base.Salary.in.2018..in.USD)

tech_doctorate <- tech_salary_shrunk[tech_salary_shrunk$Highest.Level.of.Formal.Education.Completed == "Doctorate or Post-doctorate", ] 
summary(tech_doctorate$Total.Base.Salary.in.2018..in.USD)

education_plot <- ggplot(data = tech_salary_education) +
  geom_boxplot(
    mapping = aes(x = Highest.Level.of.Formal.Education.Completed, y = Total.Base.Salary.in.2018..in.USD.),
    na.rm = TRUE
  )


# For the individual salaries dataset, the main feature we focused on was mean net wealth per person.
wealth_value <- individual_wealth$Value <- as.numeric(individual_wealth$Value)

individual_wealth_shrunk <- individual_wealth[wealth_value < 250000, ]

summary(individual_wealth_shrunk$Value)

individual_plot <- ggplot(data = individual_wealth_shrunk) +
  geom_boxplot(
    mapping = aes(x = Variable, y = Value),
    na.rm = TRUE
  )




# Lynzley's Part 3

# pull out gender and salary for from Reddit Tech Data
tech_salary_gender <- tech_salary %>%
  select(
    Total.Base.Salary.in.2018..in.USD.,
    Gender
  )


# Seperate salaries into gender categories
tech_salary_males <- tech_salary_gender[tech_salary$Gender == "Male", ]
tech_salary_females <- tech_salary_gender[tech_salary$Gender == "Female", ]

# find average salaries
tech_salary_males_mean <- mean(tech_salary_males$Total.Base.Salary.in.2018..in.USD., na.rm = TRUE)
tech_salary_males_mean <- round(tech_salary_males_mean, digits = 2)
tech_salary_females_mean <- mean(tech_salary_females$Total.Base.Salary.in.2018..in.USD., na.rm = TRUE)
tech_salary_females_mean <- round(tech_salary_females_mean, digits = 2)

salary_means <- data.frame(
  "gender" = c("Male", "Female"),
  "salary" = c(77737.52, 71317.76)
)


# creating scatterplot for visual analysis of gender global wage gap
lynzley_plot <- ggplot(data = salary_means) +
  geom_bar(aes(x = gender, y = salary), stat = "identity") +
  ylim(0, 100000) +
  labs(
    title = "Average Salary by Gender",
    x = "Gender",
    y = "Salary"
  )



# Locksley's Part 3
# The below code gets a summary of all the salaries relative to education AND gender.
tech_salary_shrunk_female <- tech_salary_shrunk[tech_salary_shrunk$Gender == "Female", ]
# View(tech_salary_shrunk_female)
tech_salary_shrunk_female <- tech_salary_shrunk_female[tech_salary_shrunk_female$Highest.Level.of.Formal.Education.Completed == "High School", ]
summary(tech_salary_shrunk_female$Total.Base.Salary.in.2018..in.USD.)

tech_salary_shrunk_female <- tech_salary_shrunk[tech_salary_shrunk$Gender == "Female", ]
# View(tech_salary_shrunk_female)
tech_salary_shrunk_female <- tech_salary_shrunk_female[tech_salary_shrunk_female$Highest.Level.of.Formal.Education.Completed == "Associates Degree", ]
summary(tech_salary_shrunk_female$Total.Base.Salary.in.2018..in.USD.)

tech_salary_shrunk_female <- tech_salary_shrunk[tech_salary_shrunk$Gender == "Female", ]
# View(tech_salary_shrunk_female)
tech_salary_shrunk_female <- tech_salary_shrunk_female[tech_salary_shrunk_female$Highest.Level.of.Formal.Education.Completed == "Undergraduate Degree", ]
summary(tech_salary_shrunk_female$Total.Base.Salary.in.2018..in.USD.)

tech_salary_shrunk_female <- tech_salary_shrunk[tech_salary_shrunk$Gender == "Female", ]
# View(tech_salary_shrunk_female)
tech_salary_shrunk_female <- tech_salary_shrunk_female[tech_salary_shrunk_female$Highest.Level.of.Formal.Education.Completed == "Graduate Degree", ]
summary(tech_salary_shrunk_female$Total.Base.Salary.in.2018..in.USD.)

tech_salary_shrunk_female <- tech_salary_shrunk[tech_salary_shrunk$Gender == "Female", ]
# View(tech_salary_shrunk_female)
tech_salary_shrunk_female <- tech_salary_shrunk_female[tech_salary_shrunk_female$Highest.Level.of.Formal.Education.Completed == "Doctorate or Post-doctorate", ]
summary(tech_salary_shrunk_female$Total.Base.Salary.in.2018..in.USD.)



tech_salary_shrunk_female <- tech_salary_shrunk[tech_salary_shrunk$Gender == "Male", ]
# View(tech_salary_shrunk_female)
tech_salary_shrunk_female <- tech_salary_shrunk_female[tech_salary_shrunk_female$Highest.Level.of.Formal.Education.Completed == "High School", ]
summary(tech_salary_shrunk_female$Total.Base.Salary.in.2018..in.USD.)

tech_salary_shrunk_female <- tech_salary_shrunk[tech_salary_shrunk$Gender == "Male", ]
# View(tech_salary_shrunk_female)
tech_salary_shrunk_female <- tech_salary_shrunk_female[tech_salary_shrunk_female$Highest.Level.of.Formal.Education.Completed == "Associates Degree", ]
summary(tech_salary_shrunk_female$Total.Base.Salary.in.2018..in.USD.)

tech_salary_shrunk_female <- tech_salary_shrunk[tech_salary_shrunk$Gender == "Male", ]
# View(tech_salary_shrunk_female)
tech_salary_shrunk_female <- tech_salary_shrunk_female[tech_salary_shrunk_female$Highest.Level.of.Formal.Education.Completed == "Undergraduate Degree", ]
summary(tech_salary_shrunk_female$Total.Base.Salary.in.2018..in.USD.)

tech_salary_shrunk_female <- tech_salary_shrunk[tech_salary_shrunk$Gender == "Male", ]
# View(tech_salary_shrunk_female)
tech_salary_shrunk_female <- tech_salary_shrunk_female[tech_salary_shrunk_female$Highest.Level.of.Formal.Education.Completed == "Graduate Degree", ]
summary(tech_salary_shrunk_female$Total.Base.Salary.in.2018..in.USD.)

tech_salary_shrunk_female <- tech_salary_shrunk[tech_salary_shrunk$Gender == "Male", ]
# View(tech_salary_shrunk_female)
tech_salary_shrunk_female <- tech_salary_shrunk_female[tech_salary_shrunk_female$Highest.Level.of.Formal.Education.Completed == "Doctorate or Post-doctorate", ]
summary(tech_salary_shrunk_female$Total.Base.Salary.in.2018..in.USD.)

# Dataframe to utilize for plotting.
tech_salaries <- data.frame(
  "Education" = c("1 - High School Diploma", "2 - Bachelor's Degree", "3 - Master's Degree", "4 - Doctorate"),
  "Male" = c(75256, 81280, 75826, 111570),
  "Female" = c(65119, 77830, 78086, 121000)
)

# Plot for data.
locksley_plot <- ggplot(data = tech_salaries) +
  
  geom_point(
    mapping = aes(x = Education, y = Female, color = "blue")
  ) +
  
  geom_point(
    mapping = aes(x = Education, y = Male, color = "red")
  ) +
  
  scale_color_discrete(labels = c("Women", "Men")) +
  
  labs(
    title = "Tech Salary Gender Comparison",
    x = "Education Level",
    y = "Salary ($)"
  )

