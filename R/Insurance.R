#' Make requests to OCBC Insurance category APIs
#'
#' This function makes the information from the OCBC Insurance category APIs easily accessible in an R-readable format.
#' There are 7 different types of insurance, each with their own unique API \cr
#' 1. Accident and Health Insurance \cr
#' 2. Car Insurance \cr
#' 3. Endowment Insurance \cr
#' 4. Home and Mortgage Insurance \cr
#' 5. Life Insurance \cr
#' 6. Maternity Insurance \cr
#' 7. Travel Insurance \cr
#'
#' @import httr jsonlite dplyr ggplot2
#' @importFrom purrr map_dfr
#'
#' @param insurance_type The insurance type, hence the API that the function will call from. Required. Options are Accident and Health, Car, Endowment, Home and Mortgage, Life, Maternity and Travel
#' @param acctoken Account token. Required. Users can sign up for their api key, api secret and account token at https://api.ocbc.com/store/home
#' @param subCategory Filter policies by subcategory. Optional.
#' @param productName Filter policies by product name. Optional.
#' @param benefit Filter policies by benefit. Optional.
#' @return A detailed dataframe of OCBC insurance policies with variables such as product name, benefits, premiums, eligibility etc.
#' @examples
#' ocbc_insurance("Accident and Health", acctoken = Sys.getenv("ACC_TOKEN_OCBC"),
#' productName = "Supreme Health")
#'@export


ocbc_insurance <- function(insurance_type = c('Accident and Health', 'Car', 'Endowment', 'Home and Mortgage', 'Life', 'Maternity', 'Travel'), acctoken, subCategory = NULL, productName = NULL, benefit = NULL){

  #some error checks to make sure the parameter inputs are in the correct format
  if(any(!is.character(insurance_type))) {
    stop("Make sure insurance type is in quotation marks!")
  }

  clean_up <- function(string) {
    return(gsub("<.*?>", "", string))
  }

  insurance_function <- function(url_link, acc_token = acctoken , SubCategory = subCategory, ProductName = productName, Benefit = benefit) {
    endpoint <- url_link
    query_params <- list("subCategory" = SubCategory, "productName" = ProductName, "benefit" = Benefit)
    get_insurance <- GET(endpoint, query = query_params, add_headers(Authorization = paste("Bearer", acc_token)))
    status <- http_status(get_insurance)
    if(status$category != "Success") {
      stop("Unsuccessful API call for ", insurance_type, ". Please check the parameters or connection.")
    }
    else{
      message("API call for ", insurance_type, " successful!")
    }
    if(insurance_type == 'Car'| insurance_type == 'Travel'){
      insurance <- content(get_insurance)[[1]][[1]][[2]][[1]][[3]]
      insurance_table <- fromJSON(toJSON(insurance), simplifyDataFrame = TRUE) %>%
        map_dfr(clean_up)
    }
    else if(insurance_type == 'Accident and Health'| insurance_type == 'Life'| insurance_type == 'Maternity'){
      insurance_table <- data.frame()
      insurance <- content(get_insurance)[[1]][[1]][[1]]
      for (i in 1:length(insurance)) {
        bridge <- insurance[[i]][[3]]
        bridge_table <- fromJSON(toJSON(bridge), simplifyDataFrame = TRUE)
        insurance_table <- rbind(insurance_table, bridge_table) %>%
          map_dfr(clean_up)
      }
    }
    else{
      insurance_table <- data.frame()
      insurance <- content(get_insurance)[[1]][[1]][[2]]
      for (i in 1:length(insurance)) {
        bridge <- insurance[[i]][[3]]
        bridge_table <- fromJSON(toJSON(bridge), simplifyDataFrame = TRUE)
        insurance_table <- rbind(insurance_table, bridge_table) %>%
          map_dfr(clean_up)
      }
    }

    return(insurance_table)
  }

  if(insurance_type == 'Accident and Health'){
    account_output <- insurance_function("https://api.ocbc.com:8243/Accident_and_Health_Insurance/1.0")
  }
  else if(insurance_type == 'Car'){
    account_output <- insurance_function("https://api.ocbc.com:8243/Car_Insurance/1.0")
  }
  else if(insurance_type == 'Endowment'){
    account_output <- insurance_function("https://api.ocbc.com:8243/insurances/endowment/1.0")
  }
  else if(insurance_type == 'Home and Mortgage'){
    account_output <- insurance_function("https://api.ocbc.com:8243/insurances/homeandmortgage/1.0")
  }
  else if(insurance_type == 'Life'){
    account_output <- insurance_function("https://api.ocbc.com:8243/Life_Insurance/1.0")
  }
  else if(insurance_type == 'Maternity'){
    account_output <- insurance_function("https://api.ocbc.com:8243/Maternity_Insurance/1.0")
  }
  else if(insurance_type == 'Travel'){
    account_output <- insurance_function("https://api.ocbc.com:8243/Travel_Insurance/1.0")
  }
  else{
    stop("Invalid insurance type!")
  }

  return(account_output)
}
