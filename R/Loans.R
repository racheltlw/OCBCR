#' Make requests to OCBC Loans category APIs
#'
#' This function makes the information from the OCBC Loans category APIs easily accessible in an R-readable format.
#' There are 8 different types of Loans, each with their own unique API. The 'Loans Accounts List' category requires an OAuth 2.0 authentication, hence only 7 APIs are made available. \cr
#' 1. Car Loans \cr
#' 2. Cash Loans \cr
#' 3. Home Loans \cr
#' 4. Property Loans \cr
#' 5. Secured Overdraft \cr
#' 6. SME Loans \cr
#' 7. Study Loans \cr
#'
#' @import httr jsonlite dplyr ggplot2
#' @importFrom purrr map_dfr
#'
#' @param loan_type The loan type, hence the API that the function will call from. Required. Options are Car, Cash, Home, Property, Secured Overdraft, SME and Study.
#' @param acctoken Account token. Required. Users can sign up for their api key, api secret and account token at https://api.ocbc.com/store/home
#' @param subCategory Filter loans by subcategory names. Optional.
#' @param productName Filter loans by product name.
#' @param benefit Filter loans by benefit.
#' @return A detailed dataframe of OCBC loans with variables such as product name, description, eligbility, benefits, loan amount etc.
#' @examples
#' ocbc_loans("Car", acctoken = Sys.getenv("ACC_TOKEN_OCBC"), productName = "New Car Financing")
#'@export



ocbc_loans <- function(loan_type = c('Car', 'Cash', 'Home', 'Property', 'Secured Overdraft', 'SME', 'Study'), acctoken, subCategory = NULL, productName = NULL, benefit = NULL){

  if(any(!is.character(loan_type))) {
    stop("Make sure loan type is in quotation marks!")
  }

  clean_up <- function(string) {
    return(gsub("<.*?>", "", string))
  }

  loan_function <- function(url_link, acc_token = acctoken , SubCategory = subCategory, ProductName = productName, Benefit = benefit) {
    endpoint <- url_link
    query_params <- list("subCategory" = SubCategory, "productName" = ProductName, "benefit" = Benefit)
    get_loan <- GET(endpoint, query = query_params, add_headers(Authorization = paste("Bearer", acc_token)))
    status <- http_status(get_loan)
    if(status$category != "Success") {
      stop("Unsuccessful API call for ", loan_type, ". Please check the parameters or connection.")
    }
    else{
      message("API call for ", loan_type, " successful!")
    }
    loan_table <- data.frame()
    loan <- content(get_loan)[[1]][[1]][[1]]
    for (i in 1:length(loan)) {
      bridge <- loan[[i]][[3]]
      bridge_table <- fromJSON(toJSON(bridge), simplifyDataFrame = TRUE)
      loan_table <- rbind(loan_table, bridge_table) %>%
        map_dfr(clean_up)
    }
    return(loan_table)
    }


  if(loan_type == 'Car'){
    account_output <- loan_function("https://api.ocbc.com:8243/loans/car/1.0")
  }
  else if(loan_type == 'Cash'){
    account_output <- loan_function("https://api.ocbc.com:8243/loans/cash/1.0")
  }
  else if(loan_type == 'Home'){
    account_output <- loan_function("https://api.ocbc.com:8243/loans/home/1.0")
  }
  else if(loan_type == 'Secured Overdraft'){
    account_output <- loan_function("https://api.ocbc.com:8243/loans/securedoverdraft/1.0")
  }
  else if(loan_type == 'Property'){
    account_output <- loan_function("https://api.ocbc.com:8243/loans/property/1.0")
  }
  else if(loan_type == 'SME'){
    account_output <- loan_function("https://api.ocbc.com:8243/sme/getProducts/1.0")
  }
  else if(loan_type == 'Study'){
    account_output <- loan_function("https://api.ocbc.com:8243/loans/study/1.0")
  }
  else{
    stop("Invalid loan type!")
  }

  return(account_output)
}
