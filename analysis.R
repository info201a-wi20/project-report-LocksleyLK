library("dplyr")

# Reddit Tech Salary DataFrame

tech_salary <- read.csv("reddit_tech_salary_sheet.csv", stringsAsFactors = FALSE)

tech_salary <- tech_salary %>%
  select(
    Employment.Type, 
    Company.Name, 
    Company.Size.....Employees, 
    Primary.Location..Country., 
    Primary.Location..City., 
    Highest.Level.of.Formal.Education.Completed, 
    Total.Base.Salary.in.2018..in.USD., 
    Total.Bonus.in.2018..cumulative.annual.value.in.USD., 
    Total.Stock.Options.Equity.in.2018..cumulative.annual.value.in.USD., 
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
