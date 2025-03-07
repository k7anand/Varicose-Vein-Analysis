# First import the Patients_Dataset.csv into R before investigating relationships.
patients_dataset <- read.csv("C:\\Users\\Local Admin\\Downloads\\Patients_Dataset.csv")
# Only the relevant columns of patients_dataset will be considered.
patients_dataset <- patients_dataset[c("Patient_ID", "Parental_History", "Gender",
                                       "Age_Category", "Status")]
# We will use head to see the first six rows.
print(head(patients_dataset, 6))
# Build a contingency table by leveraging the table() function and 
#  using the two variables of interest (gender and status) as the arguments.
print(table(patients_dataset$Gender, patients_dataset$Status))
# We can visualize this contingency table by using the barplot() function.
barplot(table(patients_dataset$Gender, patients_dataset$Status),
        xlab = "Patient Status", ylab = "Count",
        main = "Patient Status vs. Gender",
        col = c("darkorange", "darkred"),
        legend.text = c("Female", "Male"))
# Calculate row and column totals in the contingency table
sum_Female_Total <- 703 + 283 # total of female row
sum_Male_Total <- 797 + 217 # total of male row
sum_No_Total <- 703 + 797 # total of no column
sum_Yes_Total <- 283 + 217 # total of yes column
# Calculate grand total.
grand_total <- 703 + 797 + 283 + 217
# Calculate expected cell count.
e_Female_No <- (sum_Female_Total * sum_No_Total) / grand_total # Female X No
e_Male_No <- (sum_Male_Total * sum_No_Total) / grand_total # Male X No
e_Female_Yes <- (sum_Female_Total * sum_Yes_Total) / grand_total # Female X Yes
e_Male_Yes <- (sum_Male_Total * sum_Yes_Total) / grand_total # Male X Yes
# Calculate test statistic by using the formula.
test_statistic_status_gender <- (703 - e_Female_No) ** 2 / e_Female_No + 
  (797 - e_Male_No) ** 2 / e_Male_No + (283 - e_Female_Yes) ** 2 / e_Female_Yes + 
  (217 - e_Male_Yes) ** 2 / e_Male_Yes
# Report the test statistic:
cat("The test statistic is: ", test_statistic_status_gender)
# Compute and report the p-value based on the test statistic:
cat("The p-value is: ", pchisq(test_statistic_status_gender, df = 1, 
                               lower.tail = FALSE))
# Check the test statistic and p-value using chisq.test as follows:
chisq.test(table(patients_dataset$Gender, patients_dataset$Status))
# Build a contingency table by leveraging the table() function and 
#  using the two variables of interest (parental history and status) as the arguments.
print(table(patients_dataset$Parental_History, patients_dataset$Status))
# We can visualize this contingency table by using the barplot() function.
barplot(table(patients_dataset$Parental_History, patients_dataset$Status),
        xlab = "Patient Status", ylab = "Count",
        main = "Patient Status vs. Parental History",
        col = c("darkorange", "darkred"),
        legend.text = c("No", "Yes"))
# Calculate row and column totals in the contingency table
sum_No_Row_Total <- 824 + 204 # total of row "No"
sum_Yes_Row_Total <- 676 + 296 # total of row "Yes"
sum_No_Column_Total <- 824 + 676 # total of column "No"
sum_Yes_Column_Total <- 204 + 296 # total of column "Yes"
# Calculate grand total.
grand_total <- 824 + 204 + 676 + 296
# Calculate expected cell count.
e_No_No <- (sum_No_Row_Total * sum_No_Column_Total) / grand_total # No X No
e_Yes_No <- (sum_Yes_Row_Total * sum_No_Column_Total) / grand_total # Yes X No
e_No_Yes <- (sum_No_Row_Total * sum_Yes_Column_Total) / grand_total # No X Yes
e_Yes_Yes <- (sum_Yes_Row_Total * sum_Yes_Column_Total) / grand_total # Yes X Yes
# Calculate test statistic by using the formula.
test_statistic_status_parental_history <- (824 - e_No_No) ** 2 / e_No_No + 
  (676 - e_Yes_No) ** 2 / e_Yes_No + (204 - e_No_Yes) ** 2 / e_No_Yes + 
  (296 - e_Yes_Yes) ** 2 / e_Yes_Yes
# Report the test statistic:
cat("The test statistic is: ", test_statistic_status_parental_history)
# Compute and report the p-value based on the test statistic:
cat("The p-value is: ", pchisq(test_statistic_status_parental_history, df = 1, 
                               lower.tail = FALSE))
# Build a contingency table by leveraging the table() function and 
#  using the two variables of interest (age and status) as the arguments.
print(table(patients_dataset$Age_Category, patients_dataset$Status))
# We can visualize this contingency table by using the barplot() function.
barplot(table(patients_dataset$Age_Category, patients_dataset$Status),
        xlab = "Status", ylab = "Count", main = "Patient Status vs. Age",
        col = c("blue", "green", "yellow"),
        legend.text = c("Above 60", "Below 25", "Between 25 and 60"))
# Calculate row and column totals in the contingency table.
sum_a60 <- 443 + 241 # sum of row "Above 60"
sum_b25 <- 564 + 96 # sum of row "Below 25"
sum_25t60 <- 493 + 163 # sum of row "Between 25 and 60"
sum_No <- 443 + 564 + 493 # sum of column "No"
sum_Yes <- 241 + 96 + 163 # sum of column "Yes
# Calculate the grand total
grand_total <- 443 + 564 + 493 + 241 + 96 + 163
# Calculate the expected cell count.
e_a60_No <- (sum_a60 * sum_No) / grand_total # "Above 60" X "No"
e_b25_No <- (sum_b25 * sum_No) / grand_total # "Below 25" X "No"
e_25t60_No <- (sum_25t60 * sum_No) / grand_total # "Between 25 and 60" X "No"
e_a60_Yes <- (sum_a60 * sum_Yes) / grand_total # "Above 60" X "Yes"
e_b25_Yes <- (sum_b25 * sum_Yes) / grand_total # "Below 25" X "Yes"
e_25t60_Yes <- (sum_25t60 * sum_Yes) / grand_total # "Between 25 and 60" X "Yes"
# Calculate test statistic by using the formula.
test_statistic_status_age <- (443 - e_a60_No) ** 2 / e_a60_No +
  (564 - e_b25_No) ** 2 / e_b25_No + (493 - e_25t60_No) ** 2 / e_25t60_No +
  (241 - e_a60_Yes) ** 2 / e_a60_Yes + (96 - e_b25_Yes) ** 2 / e_b25_Yes +
  (163 - e_25t60_Yes) ** 2 / e_25t60_Yes
# Report the test statistic:
cat("The test statistic is: ", test_statistic_status_age)
# Compute and report the p-value:
cat("The p-value is: ", pchisq(test_statistic_status_age, df = 2, 
                               lower.tail = FALSE))
# Report the test statistics for each relationship in an organized way:
cat(" Chi-squared Score for Status vs. Gender: ", test_statistic_status_gender, 
    "\n", "Chi-squared score for Status vs. Parental History: ", 
    test_statistic_status_parental_history, "\n",
    "Chi-squared score for Status vs. Age: ", test_statistic_status_age)
# Report the test statistic per degree of freedom for status vs. age:
cat("The Chi-squared score per degree of freedom for Status vs. Age: ",
    test_statistic_status_age / 2)
# Now report the p-values for all three relationships in an organized way:
cat("The p-value associated with the: ", "\n", "Chi-squared score for Status vs. Gender: ",
    pchisq(test_statistic_status_gender, df = 1, lower.tail = FALSE), "\n",
    "Chi-squared score for Status vs. Parental History: ",
    pchisq(test_statistic_status_parental_history, df = 1, lower.tail = FALSE), 
    "\n", "Chi-squared score for Status vs. Age: ",
    pchisq(test_statistic_status_age, df = 1, lower.tail = FALSE))
