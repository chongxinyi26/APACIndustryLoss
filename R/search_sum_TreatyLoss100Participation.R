#' APAC Industry Loss Numbers
#'
#' @description
#' Search and summarise the Industry Loss Numbers based on a given EventID and ModelCode
#'
#' @param event_id The EventID to search for
#' @param model_code The ModelCode to search for
#' @param insured_csv Path to the insured CSV file, remember to include ""
#' @param insurable_csv Path to the insurable CSV file, remember to include ""
#' @param output_csv Path to the output CSV file, remember to include ""
#' @return A data frame with the Event ID, Model Code, Country Code, Sum of Insured and Insurable Loss
#' @export
#' @examples apac(267483,18,"insured.csv file path","insurable.csv file path", "output.csv file path")
#' @name apac

# Load required libraries
library(readr)
library(dplyr)

apac <- function(event_id, model_code, insured_csv, insurable_csv, output_csv) {
    # Read data from CSV files
    insured <- read_csv(insured_csv)
    insurable <- read_csv(insurable_csv)

    # Filter data for Insured and Insurable based on EventID and ModelCode
    insured_match <- filter(insured, EventID == event_id, ModelCode == model_code)
    insurable_match <- filter(insurable, EventID == event_id, ModelCode == model_code)

    # Check if matches are found
    if (nrow(insured_match) == 0 | nrow(insurable_match) == 0) {
      cat("No matches found for EventID", event_id, "with ModelCode", model_code, "\n")
      return(NULL)
    }

    # Extract TreatyLoss100Participation values and calculate sum
    insured_sum <- sum(insured_match$TreatyLoss100Participation)
    insurable_sum <- sum(insurable_match$TreatyLoss100Participation)

    # Extract CountryCode
    insured_country_code <- unique(insured_match$CountryCode)

    # Print sum of TreatyLoss100Participation
    cat("Sum of TreatyLoss100Participation for EventID", event_id, "with ModelCode", model_code, " (Insured): ", insured_sum, "\n")
    cat("Sum of TreatyLoss100Participation for EventID", event_id, "with ModelCode", model_code, " (Insurable): ", insurable_sum, "\n")
    cat("CountryCode for EventID", event_id, "with ModelCode", model_code, " (Insured): ", insured_country_code, "\n")

    # Create a data frame for the result
    result <- data.frame(EventID = event_id,
                         ModelCode = model_code,
                         InsuredLoss = insured_sum,
                         InsurableLoss = insurable_sum,
                         CountryCode = insured_country_code)

    # Write result to CSV file
    if (!file.exists(output_csv)) {
      write.csv(result, file = output_csv, row.names = FALSE)
    } else {
      write.table(result, file = output_csv, append = TRUE, sep = ",", col.names = !file.exists(output_csv), row.names = FALSE)
    }

    cat("Results saved to:", output_csv, "\n")
  }
