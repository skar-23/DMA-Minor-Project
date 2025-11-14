# -------------------------------
# Step 1: Load Excel dataset & initial exploration
# -------------------------------

# 1️⃣ Install and load required packages (only if not installed)
if(!requireNamespace("readxl", quietly = TRUE)) install.packages("readxl")
if(!requireNamespace("janitor", quietly = TRUE)) install.packages("janitor")
if(!requireNamespace("dplyr", quietly = TRUE)) install.packages("dplyr")
if(!requireNamespace("skimr", quietly = TRUE)) install.packages("skimr")

library(readxl)
library(janitor)
library(dplyr)
library(skimr)

# 2️⃣ Read the Excel file
file_path <- "C:/Users/Medha Trust/OneDrive/Desktop/labs 5th sem/DMA LAB/currency usage patterns dataset.xlsx"

# Check available sheet names
excel_sheets(file_path)

# Load the first sheet (or replace with actual sheet name if needed)
df <- read_excel(file_path, sheet = 1)

# 3️⃣ Clean column names (for easier handling)
df <- df %>% janitor::clean_names()

# 4️⃣ Basic info
cat("\n--- Dataset Overview ---\n")
dim(df)        # Rows and columns
names(df)      # Cleaned column names
head(df, 5)    # Preview first few rows
str(df)        # Data types

# 5️⃣ Summary statistics
cat("\n--- Summary Statistics ---\n")
summary(df)

# 6️⃣ Check for missing values
cat("\n--- Missing Values per Column ---\n")
missing_by_col <- sapply(df, function(x) sum(is.na(x)))
missing_by_col

# 7️⃣ Missing value percentage
missing_pct <- round(100 * missing_by_col / nrow(df), 2)
missing_pct

# 8️⃣ Check for duplicate rows
cat("\n--- Duplicate Rows ---\n")
num_duplicates <- sum(duplicated(df))
cat("Number of duplicate rows:", num_duplicates, "\n")

# 9️⃣ Quick frequency counts for key columns (if present)
possible_cols <- c("age_range", "gender", "occupation", "how_do_you_usually_pay_for_your_things")
for (col in possible_cols) {
  if (col %in% names(df)) {
    cat("\n----", col, "----\n")
    print(sort(table(df[[col]], useNA = "ifany"), decreasing = TRUE))
  }
}

# 10️⃣ Save a cleaned-name version (optional backup)
write.csv(df, "df_step1_raw_clean_names_from_excel.csv", row.names = FALSE)

cat("\n✅ Step 1 complete: Excel dataset loaded and explored successfully.\n")
cat("Next Step: Data Cleaning and Standardization.\n")









# -------------------------------
# Step 2: Data Cleaning & Standardization
# -------------------------------

# 1️⃣ Drop irrelevant columns
df_clean <- df %>% select(-timestamp, -email_address)

# 2️⃣ Handle missing values
# For categorical columns → replace NA with "Unknown"
df_clean <- df_clean %>%
  mutate(across(where(is.character), ~ ifelse(is.na(.), "Unknown", .))) %>%
  mutate(across(where(is.numeric), ~ ifelse(is.na(.), median(., na.rm = TRUE), .)))

# 3️⃣ Standardize text formats (trim spaces, proper case)
df_clean <- df_clean %>%
  mutate(across(where(is.character), ~ trimws(.))) %>%  # remove extra spaces
  mutate(across(where(is.character), tolower))          # convert to lowercase for uniformity

# 4️⃣ Standardize categorical entries manually (fix similar spellings)
df_clean <- df_clean %>%
  mutate(
    if_you_use_digital_wallet_upi_app_which_one_do_you_use_the_most =
      case_when(
        grepl("phone", if_you_use_digital_wallet_upi_app_which_one_do_you_use_the_most, ignore.case = TRUE) ~ "phonepe",
        grepl("google", if_you_use_digital_wallet_upi_app_which_one_do_you_use_the_most, ignore.case = TRUE) ~ "google pay",
        grepl("paytm", if_you_use_digital_wallet_upi_app_which_one_do_you_use_the_most, ignore.case = TRUE) ~ "paytm",
        grepl("amazon", if_you_use_digital_wallet_upi_app_which_one_do_you_use_the_most, ignore.case = TRUE) ~ "amazon pay",
        TRUE ~ if_you_use_digital_wallet_upi_app_which_one_do_you_use_the_most
      ),
    gender = case_when(
      gender %in% c("male", "m") ~ "male",
      gender %in% c("female", "f") ~ "female",
      TRUE ~ gender
    ),
    occupation = case_when(
      grepl("student", occupation, ignore.case = TRUE) ~ "student",
      grepl("working", occupation, ignore.case = TRUE) ~ "working professional",
      grepl("home", occupation, ignore.case = TRUE) ~ "homemaker",
      TRUE ~ occupation
    )
  )

# 5️⃣ Clean unusual ranges (fix inconsistent formatting)
df_clean <- df_clean %>%
  mutate(
    age_range = gsub(" ", "", age_range),    # remove spaces
    what_percentage_of_your_monthly_income_do_you_typically_save =
      gsub("May-25", "5-25", what_percentage_of_your_monthly_income_do_you_typically_save)
  )

# 6️⃣ Verify cleaning results
cat("\n--- Cleaned Column Sample ---\n")
head(df_clean[, c("age_range", "gender", "occupation", "if_you_use_digital_wallet_upi_app_which_one_do_you_use_the_most")], 10)

# 7️⃣ Check missing values again
cat("\n--- Missing Values After Cleaning ---\n")
colSums(is.na(df_clean))

# 8️⃣ Save cleaned dataset
write.csv(df_clean, "df_step2_cleaned_standardized.csv", row.names = FALSE)

cat("\n✅ Step 2 complete: Data cleaned and standardized.\n")
cat("Next Step: Encoding categorical data.\n")





# -------------------------------
# Step 3: Encoding Categorical Data
# -------------------------------

# Load necessary packages
if(!requireNamespace("caret", quietly = TRUE)) install.packages("caret")
if(!requireNamespace("fastDummies", quietly = TRUE)) install.packages("fastDummies")

library(caret)
library(fastDummies)
library(dplyr)

# 1️⃣ Identify categorical & numeric columns
cat_cols <- names(df_clean)[sapply(df_clean, is.character)]
num_cols <- names(df_clean)[sapply(df_clean, is.numeric)]

cat("\nCategorical columns:\n")
print(cat_cols)
cat("\nNumeric columns:\n")
print(num_cols)

# 2️⃣ Convert categorical columns to factors
df_encoded <- df_clean %>%
  mutate(across(all_of(cat_cols), as.factor))

# 3️⃣ Apply one-hot encoding (for multi-category columns)
# This creates dummy variables (0/1) for each category
df_encoded <- fastDummies::dummy_cols(df_encoded,
                                      select_columns = cat_cols,
                                      remove_first_dummy = TRUE,   # drop one to avoid multicollinearity
                                      remove_selected_columns = TRUE)

# 4️⃣ Verify encoding
cat("\n--- Encoded Dataset Preview ---\n")
print(head(df_encoded[, 1:10], 5))  # Show first 10 columns

# 5️⃣ Check structure after encoding
str(df_encoded)

# 6️⃣ Save encoded dataset
write.csv(df_encoded, "df_step3_encoded.csv", row.names = FALSE)

cat("\n✅ Step 3 complete: All categorical columns encoded successfully.\n")
cat("Next Step: Data Transformation / Normalization.\n")










# -------------------------------
# Step 4: Data Transformation / Normalization
# -------------------------------

# Load necessary package
if(!requireNamespace("caret", quietly = TRUE)) install.packages("caret")
library(caret)

# 1️⃣ Create a copy to normalize
df_normalized <- df_encoded

# 2️⃣ Identify numeric columns
num_cols <- names(df_normalized)[sapply(df_normalized, is.numeric)]

cat("\nNumeric columns found:\n")
print(length(num_cols))
head(num_cols, 10)

# 3️⃣ Apply Min-Max Normalization using caret::preProcess
preproc <- preProcess(df_normalized[, num_cols], method = c("range"))
df_normalized[, num_cols] <- predict(preproc, df_normalized[, num_cols])

# 4️⃣ Verify normalization (values between 0 and 1)
cat("\n--- Normalized Data Summary ---\n")
summary(df_normalized[, 1:10])

# 5️⃣ Check range of a few columns
cat("\nValue Range Check:\n")
apply(df_normalized[, num_cols[1:5]], 2, range)

# 6️⃣ Save normalized dataset
write.csv(df_normalized, "df_step4_normalized.csv", row.names = FALSE)

cat("\n✅ Step 4 complete: Data normalized successfully.\n")
cat("All numeric features are now scaled between 0 and 1.\n")
cat("Dataset is ready for visualization or model building.\n")






# Install if not already
if(!requireNamespace("ggplot2", quietly = TRUE)) install.packages("ggplot2")
if(!requireNamespace("dplyr", quietly = TRUE)) install.packages("dplyr")

library(ggplot2)
library(dplyr)

#1111111111
ggplot(df_clean, aes(x = age_range, fill = age_range)) +
  geom_bar() +
  labs(title = "Age Range Distribution",
       x = "Age Group", y = "Number of Participants") +
  theme_minimal()

#22222
ggplot(df_clean, aes(x = how_do_you_usually_pay_for_your_things, fill = how_do_you_usually_pay_for_your_things)) +
  geom_bar() +
  labs(title = "Preferred Payment Methods",
       x = "Payment Mode", y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 25, hjust = 1))

#333333
ggplot(df_clean, aes(x = gender, fill = if_you_use_digital_wallet_upi_app_which_one_do_you_use_the_most)) +
  geom_bar(position = "dodge") +
  labs(title = "Most Used UPI App by Gender",
       x = "Gender", y = "Number of Users", fill = "UPI App") +
  theme_minimal()


##4444444
ggplot(df_clean, aes(x = your_average_monthly_digital_transactions_amount_is, fill = your_average_monthly_digital_transactions_amount_is)) +
  geom_bar() +
  labs(title = "Average Monthly Digital Transaction Amount",
       x = "Transaction Range (INR)", y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 25, hjust = 1))


#5555
ggplot(df_clean, aes(x = what_percentage_of_your_monthly_income_do_you_typically_save, fill = what_percentage_of_your_monthly_income_do_you_typically_save)) +
  geom_bar() +
  labs(title = "Monthly Savings Percentage",
       x = "Savings %", y = "Number of Respondents") +
  theme_minimal()


####66666666
ggplot(df_clean, aes(x = how_would_you_rate_the_security_of_online_banking_platforms)) +
  geom_histogram(binwidth = 1, fill = "#0072B2", color = "white") +
  labs(title = "Perceived Security of Online Banking",
       x = "Security Rating (1–5)", y = "Count") +
  theme_minimal()


#####777777
ggplot(df_clean, aes(x = have_you_ever_been_a_victim_of_financial_fraud_or_scams, fill = have_you_ever_been_a_victim_of_financial_fraud_or_scams)) +
  geom_bar() +
  labs(title = "Financial Fraud Experience",
       x = "Fraud Victim?", y = "Number of Respondents") +
  theme_minimal()


###888888
ggplot(df_clean, aes(x = how_familiar_are_you_with_digital_currencies_cryptocurrency, fill = how_familiar_are_you_with_digital_currencies_cryptocurrency)) +
  geom_bar() +
  labs(title = "Familiarity with Digital Currencies",
       x = "Familiarity Level", y = "Count") +
  theme_minimal()









# --------------------------------------------
# Step 6: Logistic Regression – Fraud Prediction Model (Final Version)
# --------------------------------------------

# Load packages
if(!requireNamespace("caret", quietly = TRUE)) install.packages("caret")
if(!requireNamespace("ggplot2", quietly = TRUE)) install.packages("ggplot2")
if(!requireNamespace("pROC", quietly = TRUE)) install.packages("pROC")

library(caret)
library(ggplot2)
library(pROC)
library(dplyr)

# 1️⃣ Define target and predictors
target <- "have_you_ever_been_a_victim_of_financial_fraud_or_scams_yes"

# 2️⃣ Select relevant predictor variables (behavioral + awareness factors)
model_data <- df_normalized %>%
  select(
    have_you_ever_been_a_victim_of_financial_fraud_or_scams_yes,
    how_would_you_rate_the_security_of_online_banking_platforms,
    `how_familiar_are_you_with_digital_currencies_cryptocurrency_slightly familiar`,
    have_you_ever_purchased_or_used_digital_currency_yes,
    `how_do_you_usually_pay_for_your_things_upi/digital wallets`,
    `what_percentage_of_your_monthly_income_do_you_typically_save_5 -25`
  )

# 3️⃣ Split train-test (80–20)
set.seed(123)
train_index <- createDataPartition(model_data[[target]], p = 0.8, list = FALSE)
train_data <- model_data[train_index, ]
test_data <- model_data[-train_index, ]

# 4️⃣ Train Logistic Regression Model
fraud_model <- glm(
  formula = as.formula(paste(target, "~ .")),
  data = train_data,
  family = binomial
)

# 5️⃣ Predict on test data
pred_prob <- predict(fraud_model, newdata = test_data, type = "response")
pred_class <- ifelse(pred_prob > 0.5, 1, 0)

# 6️⃣ Evaluate model performance
conf_mat <- confusionMatrix(
  factor(pred_class),
  factor(test_data[[target]])
)
print(conf_mat)

# 7️⃣ ROC Curve
roc_obj <- roc(test_data[[target]], pred_prob)

plot(roc_obj, col = "#2E86AB", lwd = 3,
     main = "ROC Curve – Fraud Prediction Model")
abline(a = 0, b = 1, lty = 2, col = "gray")

auc_value <- auc(roc_obj)
cat("\nAUC (Area Under Curve):", auc_value, "\n")

# 8️⃣ Visualize predicted probabilities
ggplot(data.frame(pred_prob, Actual = test_data[[target]]),
       aes(x = pred_prob, fill = factor(Actual))) +
  geom_histogram(position = "identity", alpha = 0.6, bins = 20) +
  scale_fill_manual(values = c("#E74C3C", "#2ECC71"),
                    name = "Actual Fraud", labels = c("No", "Yes")) +
  labs(title = "Predicted Probability Distribution",
       x = "Predicted Fraud Probability", y = "Count") +
  theme_minimal()




