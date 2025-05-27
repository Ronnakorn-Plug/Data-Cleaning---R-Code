## install
install.packages("imputeTS")
install.packages("dplyr")
install.packages("stringr")
install.packages("DescTools")

# Library
library(imputeTS) # imputation 
library(dplyr) # grammar of data manipulation
library(stringr) # a fundamental data structure in R
library(DescTools) # Tools for Descriptive Statistics (Mode)


## -- Import Dataset .CSV --
df <- read.csv("D:\\Zane\\Swinburne University\\Semester_3_2025\\INF80051-Artificial Intelligence and Insights\\Assignment 2_group\\train_dataset.csv",
                        colClasses = c("ID" = "character"),
                        stringsAsFactors = FALSE)

#--------   Data Cleaning   ----------
# -- Clean Name column --
df$`Name` <- ifelse(str_trim(df$`Name`) == "" | is.na(df$`Name`), NA, df$`Name`)

df <- df %>%
  group_by(Customer_ID) %>%
  mutate(`Name` = ifelse(is.na(`Name`),
                         names(sort(table(`Name`), decreasing = TRUE))[1],
                         `Name`)) %>% ungroup()

df$`Name` <- str_replace_all(df$`Name`, '[",]', "") 
df$`Name` <- sub('"[^"]*$', "", df$`Name`) 

# -- Clean Age --
df$Age <- gsub("[^0-9]", "", df$Age) 
df$Age <- as.integer(df$Age) 
df$Age[df$Age < 0 | df$Age > 100] <- NA

# (some group's consistent value have equate value)
resolve_value <- function(x) {
  x <- na.omit(x)  
  if (length(x) == 0) return(NA)  
  m <- Mode(x)
  if (length(m) == 1) {
    return(m)  # unique mode
  } else {
    return(max(x))  # multiple modes → fallback to max
  }
}

df <- df %>%
  group_by(Customer_ID) %>%
  mutate(Age = resolve_value(Age)) %>%  # Apply Mode to entire group, not just NA
  ungroup()

# -- Clean SSN --
valid_ssn_pattern <- "^\\d{3}-\\d{2}-\\d{4}$" 
df$SSN <- ifelse(str_detect(df$SSN, valid_ssn_pattern), df$SSN, NA)
df <- df %>%
  group_by(Customer_ID) %>%
  mutate(SSN = ifelse(is.na(SSN),
                      names(sort(table(SSN), decreasing = TRUE))[1],
                      SSN)) %>% ungroup()


# -- Clean Occupation --
df$`Occupation` <- ifelse(df$`Occupation` == "_______", NA, df$`Occupation`)
df$`Occupation` <- str_replace_all(df$`Occupation`, "_", " ")
df <- df %>%
  group_by(Customer_ID) %>%
  mutate(`Occupation` = ifelse(is.na(`Occupation`),
                               names(sort(table(`Occupation`), decreasing = TRUE))[1],
                               `Occupation`)) %>% ungroup()


# -- Annual_Income --
df$Annual_Income <- gsub("[^0-9.]", "", df$Annual_Income) # gsub used for replacement operation for all matches.
df$Annual_Income <- round(as.numeric(df$Annual_Income), 2) # round Specified the number of decimal places
df <- df %>%
  group_by(Customer_ID) %>%
  mutate(Annual_Income = ifelse(is.na(Annual_Income),
                                round(Mode(Annual_Income, na.rm = TRUE), 2),
                                round(Mode(Annual_Income, na.rm = TRUE), 2))) %>% ungroup()


# -- Monthly_Inhand_Salary --
df$Monthly_Inhand_Salary <- gsub("[^0-9.]", "", df$Monthly_Inhand_Salary)
df$Monthly_Inhand_Salary <- round(as.numeric(df$Monthly_Inhand_Salary), 2)
df <- df %>%
  group_by(Customer_ID) %>%
  mutate(Monthly_Inhand_Salary = ifelse(is.na(Monthly_Inhand_Salary),
                                        Mode(Monthly_Inhand_Salary, na.rm = TRUE),
                                        Mode(Monthly_Inhand_Salary, na.rm = TRUE))) %>% ungroup()


# -- Num_Bank_Accounts --
df$Num_Bank_Accounts <- ifelse(df$Num_Bank_Accounts %in% c("0", "-", ""), NA, df$Num_Bank_Accounts)  # %in% used to compare two vectors
df$Num_Bank_Accounts <- gsub("[^0-9.]", "", df$Num_Bank_Accounts)
df$Num_Bank_Accounts <- as.integer(df$Num_Bank_Accounts)
df <- df %>%
  group_by(Customer_ID) %>%
  mutate(Num_Bank_Accounts = resolve_value(Num_Bank_Accounts)) %>%
  ungroup()
df$Num_Bank_Accounts[is.na(df$Num_Bank_Accounts)] <- 1


# -- Num_Credit_Card -- 
df$Num_Credit_Card <- gsub("[^0-9]", "", df$Num_Credit_Card)
df$Num_Credit_Card <- as.integer(df$Num_Credit_Card)
df <- df %>%
  group_by(Customer_ID) %>%
  mutate(Num_Credit_Card = resolve_value(Num_Credit_Card)) %>% ungroup()


# -- Interest_Rate --
df$Interest_Rate <- gsub("[^0-9]", "", df$Interest_Rate)
df$Num_Credit_Card <- as.integer(df$Num_Credit_Card)
df <- df %>%
  group_by(Customer_ID) %>%
  mutate(Interest_Rate = ifelse(is.na(Interest_Rate),
                                Mode(Interest_Rate, na.rm = TRUE),
                                Mode(Interest_Rate, na.rm = TRUE))) %>% ungroup()


# -- Num_of_Loan --
df$Num_of_Loan <- gsub("[^0-9]", "", df$Num_of_Loan)
df$Num_of_Loan <- as.integer(df$Num_of_Loan)
df <- df %>%
  group_by(Customer_ID) %>%
  mutate(Num_of_Loan = ifelse(is.na(Num_of_Loan),
                              Mode(Num_of_Loan, na.rm = TRUE),
                              Mode(Num_of_Loan, na.rm = TRUE))) %>% ungroup()


# -- Type_of_Loan --
df$Type_of_Loan <- ifelse(str_trim(df$Type_of_Loan) == "" | is.na(df$Type_of_Loan), NA, df$Type_of_Loan)
df$Type_of_Loan[is.na(df$Type_of_Loan)] <- "Not Specified"

###########################################################################

# Step 2: Replace "_" and empty strings with NA
df[df == "_"] <- NA
df[df == ""] <- NA

# Step 3: Define numeric and categorical columns
numeric_cols <- c("Age", "Annual_Income", "Monthly_Inhand_Salary", 
                  "Num_Bank_Accounts", "Num_Credit_Card", "Interest_Rate",
                  "Num_of_Loan", "Delay_from_due_date", "Num_of_Delayed_Payment",
                  "Changed_Credit_Limit", "Num_Credit_Inquiries", 
                  "Outstanding_Debt", "Credit_Utilization_Ratio",
                  "Total_EMI_per_month", "Amount_invested_monthly", 
                  "Monthly_Balance")

categorical_cols <- c("Credit_Mix", "Payment_of_Min_Amount", "Payment_Behaviour")

# Step 4: Clean and convert numeric columns
df[numeric_cols] <- lapply(df[numeric_cols], function(x) {
  x <- gsub(",", "", as.character(x))               # Remove commas
  x <- gsub("\\$", "", x)                           # Remove dollar signs
  x[x %in% c("NA", "na", "n/a", "NaN", "Unknown", "unknown")] <- NA
  suppressWarnings(as.numeric(x))                   # Convert to numeric safely
})

# Step 5: Convert Credit_History_Age to months
df <- df %>% rename(Credit_History_Age_Months = Credit_History_Age)
df$Credit_History_Age_Months <- sapply(df$Credit_History_Age_Months, function(x) {
  if (is.na(x)) return(NA)
  years <- as.numeric(str_extract(x, "\\d+(?=\\sYears)"))
  months <- as.numeric(str_extract(x, "\\d+(?=\\sMonths)"))
  return(years * 12 + months)
})

# Step 6: Standardize categorical values
df$Payment_of_Min_Amount <- str_to_title(df$Payment_of_Min_Amount)
df$Credit_Mix <- str_to_title(df$Credit_Mix)
df$Payment_Behaviour <- str_replace_all(df$Payment_Behaviour, "_", " ")

# Step 7: Impute missing numeric values using linear interpolation
df[numeric_cols] <- lapply(df[numeric_cols], function(x) na_interpolation(x, option = "linear"))

# Step 8: Impute missing categorical values using mode
get_mode <- function(x) {
  ux <- na.omit(x)
  ux[which.max(tabulate(match(x, ux)))]
}

for (col in categorical_cols) {
  df[[col]][is.na(df[[col]])] <- get_mode(df[[col]])
}

# Step 9: Impute missing Credit_History_Age_Months
df$Credit_History_Age_Months[is.na(df$Credit_History_Age_Months)] <- 
  round(mean(df$Credit_History_Age_Months, na.rm = TRUE), 0)

# Step 10: Removing negatives
df$Delay_from_due_date <- abs(df$Delay_from_due_date)
df$Num_of_Delayed_Payment <- abs(df$Num_of_Delayed_Payment)

# Step 11: Apply IQR-based outlier capping
for (col in numeric_cols) {
  Q1 <- quantile(df[[col]], 0.25, na.rm = TRUE)
  Q3 <- quantile(df[[col]], 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  lower <- Q1 - 1.5 * IQR
  upper <- Q3 + 1.5 * IQR
  df[[col]] <- pmin(pmax(df[[col]], lower), upper)
}

# Step 12: Noise reduction on selected columns using rolling average
smooth_cols <- c("Monthly_Balance", "Outstanding_Debt")
df[smooth_cols] <- lapply(df[smooth_cols], function(x) na_ma(x, k = 3, weighting = "simple"))

# Step 13: Round selected columns to 2 decimal places
round_cols <- c("Credit_Utilization_Ratio", "Total_EMI_per_month", 
                "Amount_invested_monthly", "Monthly_Balance")
df[round_cols] <- lapply(df[round_cols], function(x) round(x, 2))

# Step 14: Remove duplicates
df <- df %>% distinct()


# Change Nu to No
df$Payment_of_Min_Amount <- gsub("Nm", "No", df$Payment_of_Min_Amount)


# Remove multiple columns (ID, Name, Type_of_Loan, Payment_Behaviour)
df <- df %>% select(-c(ID, Name, Type_of_Loan, Payment_Behaviour))


###################################################################
# Normalization
normalize_min_max <- function(x) {
  return((x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE)))
}

# Apply to all numeric columns only
df_normalized <- df %>%
  mutate(across(where(is.numeric), normalize_min_max))

###################################################################

# Change Character to Numeric (Poor=0, Standard=1, Good=2)
df_numeric_normalized$Credit_Score_Num <- case_when(
  df_numeric_normalized$Credit_Score == "Good" ~ 2,
  df_numeric_normalized$Credit_Score == "Standard" ~ 1,
  df_numeric_normalized$Credit_Score == "Poor" ~ 0,
  TRUE ~ NA_real_  # for handling missing or unknown values
)

# Change Character to Numeric
credit_map <- c("Poor" = 0, "Standard" = 1, "Good" = 2)
payment_map <- c("No" = 0, "Yes" = 1)

df_normalized <- df_normalized %>% 
  mutate(
    across(c(Credit_Mix, Credit_Score), ~ credit_map[.]), # Apply to columns
    Payment_of_Min_Amount = payment_map[Payment_of_Min_Amount]
  )

###################################################################

# Export final dataset
# write.csv(df, "Credit_score_classification_Clean_Dataset.csv", row.names = FALSE)
# write.csv(df_normalized, "df_normalized_Dataset.csv", row.names = FALSE)
