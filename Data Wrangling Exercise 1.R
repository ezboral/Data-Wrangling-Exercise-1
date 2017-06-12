# Step 0: Load the data in RStudio

library(readr)
library(dplyr)
library(tidyr)

refine_original <- read_csv("~/Documents/Eitan/R/Data Wrangling Exercises/Exercise 1/refine_original.csv")

# Step 1: Clean up names

refine_original$company <- tolower(refine_original$company)
refine_original$company <- replace(refine_original$company, grep("ps", refine_original$company), "philips")
refine_original$company <- replace(refine_original$company, grep("ak", refine_original$company), "akzo")
refine_original$company <- replace(refine_original$company, grep("van", refine_original$company), "van houten")
refine_original$company <- replace(refine_original$company, grep("ver", refine_original$company), "unilever")

# Step 2: Separate product and code #

refine_original <- refine_original %>% separate("Product code / number", c("product code", "product number"), sep = "-")

# Step 3: Add product categories

category <- function(x) {
  if (x == "p"){
    return("Smartphone")
  }   else if (x=="v"){
    return("TV")
  }   else if (x=="x"){
    return("Laptop")
  }   else if (x=="q"){
    return("Tablet")
  }
}

refine_original <- mutate(refine_original, category=sapply(refine_original$`product code`, category))

# Step 4: Add full address column

refine_original <- unite(refine_original, full_address, address:country, sep=",")

# Step 5: Add binary columns for company and product

refine_original$company_philips <- as.numeric(refine_original$company == "philips")
refine_original$company_akzo <- as.numeric(refine_original$company == "akzo")
refine_original$company_van_houten <- as.numeric(refine_original$company == "van houten")
refine_original$company_unilever <- as.numeric(refine_original$company == "unilever")

refine_original$product_smartphone <- as.numeric(refine_original$category == "Smartphone")
refine_original$product_tv <- as.numeric(refine_original$category == "TV")
refine_original$product_laptop <- as.numeric(refine_original$category == "Laptop")
refine_original$product_tablet <- as.numeric(refine_original$category == "Tablet")

# Step 6: Create clean csv file

write.csv(refine_original, "refine_clean.csv")
