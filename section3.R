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


