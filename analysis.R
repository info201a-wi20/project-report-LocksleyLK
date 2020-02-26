library("dplyr")

setwd("~/Info201/project-report-LocksleyLK")

tech_salary <- read.csv("reddit_tech_salary_sheet.csv", stringsAsFactors = FALSE)

tech_salary <- tech_salary %>%
  select(
    Timestamp, 
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
