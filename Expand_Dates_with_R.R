# first load any necessary packages
library(tidyverse)

# next we assign our function give it a task
expand_dates <- function(df) { 
  
  # any date columns from Prep will need to be coerced to dates with the "%Y-%m-%d" format
  df$Invoice_Start <- as.Date(df$Invoice_Start, "%Y-%m-%d")
  df$Invoice_End <- as.Date(df$Invoice_End, "%Y-%m-%d")
  
  # our dates are recognized as dates within R; we can manipulate our data frame as we please
  # below, we use expand() to generate one row per invoice per month from Invoice_Start to Invoice_End
  df <- df %>% 
    group_by(Customer_Name, Product, Value, Invoice_Number, Invoice_Start, Invoice_End) %>% 
    expand(Monthly_Invoice_Date = seq.Date(Invoice_Start, Invoice_End, by="month"))
  
  # now that our df has been transformed to our liking, we must convert dates columns back to strings
  # if date columns are not coerced to strings, they will be display as integers in Prep (days since 1970)
  df$Invoice_Start = as.character(df$Invoice_Start)
  df$Invoice_End = as.character(df$Invoice_End)
  df$Monthly_Invoice_Date = as.character(df$Monthly_Invoice_Date)
  
  # our df is now ready for Prep
  return(df)
}

# Lastly, we have to provide an output schema for Prep
# that is, we explicitly tell it what to expect (i.e. columns and data types)
# any columns we'd like to drop (e.g. Invoice_Start, Invoice_End) can be left out

getOutputSchema <- function() {
  return(data.frame(
    Customer_Name = prep_string(),
    Product = prep_string(),
    Value = prep_int(),
    Invoice_Number = prep_int(),
    Invoice_Start = prep_string(), # note that our date columns are now strings
    Invoice_End = prep_string(),
    Monthly_Invoice_Date = prep_string()
  ))
}