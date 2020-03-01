library("dplyr")

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

print(summary(tech_salary_shrunk$Total.Base.Salary.in.2018..in.USD.))

boxplot(tech_salary_shrunk$Total.Base.Salary.in.2018..in.USD.,
        main = "Total Base Salary",
        xlab = "Dollars",
        horizontal = TRUE 
)



tech_female <- tech_salary_shrunk[tech_salary_shrunk$Gender == "Female", ]

print(summary(tech_female$Total.Base.Salary.in.2018..in.USD.))

boxplot(tech_female$Total.Base.Salary.in.2018..in.USD.,
        horizontal = TRUE 
)

tech_male <- tech_salary_shrunk[tech_salary_shrunk$Gender == "Male", ]
print(summary(tech_male$Total.Base.Salary.in.2018..in.USD.))

boxplot(tech_male$Total.Base.Salary.in.2018..in.USD.,
        horizontal = TRUE 
)


tech_highschool <- tech_salary_shrunk[tech_salary_shrunk$Highest.Level.of.Formal.Education.Completed == "High School",]
print(summary(tech_highschool$Total.Base.Salary.in.2018..in.USD.))

boxplot(tech_highschool$Total.Base.Salary.in.2018..in.USD.,
        horizontal = TRUE 
)

tech_tradeschool <- tech_salary_shrunk[tech_salary_shrunk$Highest.Level.of.Formal.Education.Completed == "Trade School",]
print(summary(tech_tradeschool$Total.Base.Salary.in.2018..in.USD.))

boxplot(tech_tradeschool$Total.Base.Salary.in.2018..in.USD.,
        horizontal = TRUE 
)

tech_certifications <- tech_salary_shrunk[tech_salary_shrunk$Highest.Level.of.Formal.Education.Completed == "Professional Certifications",]
print(summary(tech_certifications$Total.Base.Salary.in.2018..in.USD.))

boxplot(tech_certifications$Total.Base.Salary.in.2018..in.USD.,
        horizontal = TRUE 
)

tech_associate <- tech_salary_shrunk[tech_salary_shrunk$Highest.Level.of.Formal.Education.Completed == "Associates Degree",]
print(summary(tech_associate$Total.Base.Salary.in.2018..in.USD.))

boxplot(tech_associate$Total.Base.Salary.in.2018..in.USD.,
        horizontal = TRUE 
)

tech_undergraduate <- tech_salary_shrunk[tech_salary_shrunk$Highest.Level.of.Formal.Education.Completed == "Undergraduate Degree", ] 
print(summary(tech_undergraduate$Total.Base.Salary.in.2018..in.USD.))

boxplot(tech_undergraduate$Total.Base.Salary.in.2018..in.USD.,
        horizontal = TRUE 
)

tech_graduate <- tech_salary_shrunk[tech_salary_shrunk$Highest.Level.of.Formal.Education.Completed == "Graduate Degree", ] 
print(summary(tech_graduate$Total.Base.Salary.in.2018..in.USD))

boxplot(tech_graduate$Total.Base.Salary.in.2018..in.USD.,
        horizontal = TRUE 
)

tech_doctorate <- tech_salary_shrunk[tech_salary_shrunk$Highest.Level.of.Formal.Education.Completed == "Doctorate or Post-doctorate", ] 
print(summary(tech_doctorate$Total.Base.Salary.in.2018..in.USD))

boxplot(tech_doctorate$Total.Base.Salary.in.2018..in.USD.,
        horizontal = TRUE 
)

# For the individual salaries dataset, the main feature we focused on was mean net wealth per person.
print(summary(individual_wealth$Value))


