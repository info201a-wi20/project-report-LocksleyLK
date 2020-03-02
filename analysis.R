library("dplyr")
library("ggplot2")

# Reddit Tech Salary DataFrame

tech_salary <- read.csv("reddit_tech_salary_sheet.csv", stringsAsFactors = FALSE)

tech_salary <- tech_salary %>%
  select(
    Company.Name, 
    Primary.Location..Country., 
    Primary.Location..City., 
    Highest.Level.of.Formal.Education.Completed, 
    Total.Base.Salary.in.2018..in.USD., 
    Gender
  )

tech_salary_sample <- tech_salary[2:4, ]


# Individual Wealth Dataframe

individual_wealth <- read.csv("individual wealth.csv", stringsAsFactors = FALSE)

individual_wealth <- individual_wealth[individual_wealth$Variable == "Mean net wealth per person (current prices)", ]

individual_wealth <- individual_wealth %>%
  select(
    Country,
    Variable,
    Population,
    Value
  )

individual_wealth_sample <- individual_wealth[1:3, ]


View(tech_salary)
View(individual_wealth)
# For our tech slararies data table, the main features we focused on was 
#salary in relation to highest level of education and gender.

# The summary of just tech salary is:
tech_salary_shrunk <- tech_salary[tech_salary$Total.Base.Salary.in.2018..in.USD. < 250000, ]
tech_salary_shrunk <- tech_salary_shrunk[tech_salary_shrunk$Total.Base.Salary.in.2018..in.USD. > 10000, ]
tech_salary_shrunk <- tech_salary_shrunk[2:nrow(tech_salary_shrunk), ]

summary(tech_salary_shrunk$Total.Base.Salary.in.2018..in.USD.)

tech_salary_gender <- tech_salary_shrunk[tech_salary_shrunk$Gender == "Male" | tech_salary_shrunk$Gender == "Female", ]
View(tech_salary_gender)

tech_female <- tech_salary_shrunk[tech_salary_shrunk$Gender == "Female", ]
summary(tech_female$Total.Base.Salary.in.2018..in.USD.)

tech_male <- tech_salary_shrunk[tech_salary_shrunk$Gender == "Male", ]
summary(tech_male$Total.Base.Salary.in.2018..in.USD.)

gender_plot <- ggplot(data = tech_salary_gender) +
  geom_boxplot(
    mapping = aes(x = Gender, y = Total.Base.Salary.in.2018..in.USD.)
  )


tech_salary_education <- tech_salary_shrunk[tech_salary_shrunk$Highest.Level.of.Formal.Education.Completed == "High School" |
                                           tech_salary_shrunk$Highest.Level.of.Formal.Education.Completed == "Undergraduate Degree" |
                                           tech_salary_shrunk$Highest.Level.of.Formal.Education.Completed == "Graduate Degree" |
                                           tech_salary_shrunk$Highest.Level.of.Formal.Education.Completed == "Doctorate or Post-doctorate",
                                         ]

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
individual_wealth$Value <- as.numeric(individual_wealth$Value)

individual_wealth_shrunk <- individual_wealth[individual_wealth$Value < 250000, ]

summary(individual_wealth_shrunk$Value)

individual_plot <- ggplot(data = individual_wealth_shrunk) +
  geom_boxplot(
    mapping = aes(x = Variable, y = Value),
    na.rm = TRUE
  )




# Lynzley's Part 3

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

salary_means <- data.frame(
  "gender" = c("Male", "Female"),
  "salary" = c(77737.52, 71317.76)
)
View(salary_means)

# creating scatterplot for visual analysis of gender global wage gap
lynzley_plot <- ggplot(data = salary_means) +
  geom_bar(aes(x = gender, y = salary), stat = "identity") +
  ylim(0, 100000) +
  labs(
    title = "Average Salary by Gender",
    x = "Gender",
    y = "Salary"
  )

# data_country <- data.frame(country = c("China", "Germany", "UK", "US"), 
#                            conversion_rate = c(0.001331558,0.062428188, 0.052612025, 0.037800687))
# ggplot(data_country, aes(x=country,y = conversion_rate)) +geom_bar(stat = "identity")








# Locksley's Part 3
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


tech_salaries <- data.frame(
  "Education" = c("1 - High School Diploma", "2 - Bachelor's Degree", "3 - Master's Degree", "4 - Doctorate"),
  "Male" = c(75256, 81280, 75826, 111570),
  "Female" = c(65119, 77830, 78086, 121000)
)


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

####################################################
######## Kaamna's Section Three, Question 3 + 4 ####
####################################################

library("dplyr")
library("ggplot2")
library("tidyr")
library("data.table")


# Individual Wealth Dataframe
individual_wealth <- read.csv("individual wealth.csv", stringsAsFactors = FALSE, skip = 1)

individual_wealth <- individual_wealth[individual_wealth$Variable == "Mean net wealth per person (current prices)", ]

individual_wealth <- individual_wealth %>%
  select(
    Country,
    Variable,
    Population,
    Value
  )

individual_usa <- individual_wealth %>% 
  filter(Country == "United States") 

numeric_individual<- as.numeric(individual_usa$Value)
mean_individual<- mean(numeric_individual)


# data table
individual_data <- individual_usa[1:4]
individual_table <- as.data.table(individual_data, keep.rownames = FALSE)


# Reddit Tech Salary DataFrame

tech_salary <- read.csv("reddit_tech_salary_sheet.csv", stringsAsFactors = FALSE)

tech_salary <- tech_salary %>%
  select(
    Company.Name, 
    Primary.Location..Country., 
    Primary.Location..City., 
    Highest.Level.of.Formal.Education.Completed, 
    Total.Base.Salary.in.2018..in.USD., 
    Gender
  )

microsoft_female <- tech_salary %>% 
  filter(
    Company.Name == "Microsoft",
    Gender == "Female"
  ) %>% 
  mutate(
    Value = Total.Base.Salary.in.2018..in.USD.,
  )

numeric_female <- as.numeric(microsoft_female$Value)
microsoft_mean <- mean(numeric_female)


# data table
microsoft_table <- microsoft_female[1:5]
reddit_table <- as.data.table(microsoft_table, keep.rownames = FALSE)

# the plot of data
net_income_usa <- ggplot() +
  geom_col(microsoft_female, mapping = aes(fill = "Company.Name", x = Primary.Location..Country., y = microsoft_mean), stat = "identity", width = 1) +
  geom_col(individual_usa, mapping = aes(fill = "Total Population", x = Country, y = mean_individual), stat = "identity", width = 1) +
  labs(title = "Salary of Woman vs. the USA", x = "Country", y = "Dollars") +
  theme(axis.text.x = element_text(size = 10)) +
  scale_fill_discrete(name = "Type of Work", labels = c("Technology Company (Microsoft)", "USA Average"))

####################################################
############ end of Kaamna's section ###############
####################################################


