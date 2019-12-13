#' Make requests to OCBC Investments category APIs
#'
#' This function makes the information from the OCBC Investments category APIs easily accessible in an R-readable format.
#' There are 7 different types of investments, each with their own unique API \cr
#' 1. Bonds \cr
#' 2. Equity and Forex \cr
#' 3. Listed Securities \cr
#' 4. Leveraged Forex and Futures \cr
#' 5. Treasury \cr
#' 6. Unlisted Securities \cr
#' 7. Unit Trusts
#'
#' @import httr jsonlite dplyr ggplot2
#' @importFrom purrr map_dfr
#'
#' @param investment_type The investment type, hence the API that the function will call from. Required. Options are Bonds, Equity and Forex, Listed Securities, Leveraged Forex, Treasury, Unlisted Securities and Unit Trusts
#' @param acctoken Account token. Required. Users can sign up for their api key, api secret and account token at https://api.ocbc.com/store/home
#' @param productName Filter investment products by product name. Optional. E.g Bonds
#' @param benefit Filter investment products by benefit. Optional. E.g interest
#' @return A detailed dataframe of OCBC investment products with variables such as product name, benefits, risks etc.
#' @examples
#' ocbc_investments("Bonds", acctoken = Sys.getenv("ACC_TOKEN_OCBC"))
#'@export


ocbc_investments <- function(investment_type = c('Bonds', 'Equity and Forex', 'Listed Securities ', 'Leveraged Forex', 'Treasury', 'Unlisted Securities', 'Unit Trusts'), acctoken, productName = NULL, benefit = NULL){

  if(any(!is.character(investment_type))) {
    stop("Make sure investment type is in quotation marks!")
  }

  clean_up <- function(string) {
    return(gsub("<.*?>", "", string))
  }
  investment_function <- function(url_link, acc_token = acctoken , ProductName = productName, Benefit = benefit) {
    endpoint <- url_link
    query_params <- list("productName" = ProductName, "benefit" = Benefit)
    get_investment <- GET(endpoint, query = query_params, add_headers(Authorization = paste("Bearer", acc_token)))
    status <- http_status(get_investment)
    if(status$category != "Success") {
      stop("Unsuccessful API call for ", investment_type, ". Please check the parameters or connection.")
    }
    else{
      message("API call for ", investment_type, " successful!")
    }
    investment <- content(get_investment)[[1]][[1]][[2]][[1]]
    investment_table <- flatten(as.data.frame(fromJSON(toJSON(investment)))) %>%
    map_dfr(clean_up)
  }
  if(investment_type == 'Bonds'){
    account_output <- investment_function("https://api.ocbc.com:8243/investments/ospl/bonds/1.0")
  }
  else if(investment_type == 'Equity and Forex'){
    account_output <- investment_function("https://api.ocbc.com:8243/investments/equityandforex/1.0")
  }
  else if(investment_type == 'Listed Securities'){
    account_output <- investment_function("https://api.ocbc.com:8243/investments/ospl/listedsecurities/1.0")
  }
  else if(investment_type == 'Leveraged Forex'){
    account_output <- investment_function("https://api.ocbc.com:8243/investments/ospl/leveragedforexandfutures/1.0")
  }
  else if(investment_type == 'Treasury'){
    account_output <- investment_function("https://api.ocbc.com:8243/investments/treasury/1.0")
  }
  else if(investment_type == 'Unlisted Securities'){
    account_output <- investment_function("https://api.ocbc.com:8243/investments/ospl/unlistedsecurities/1.0")
  }
  else if(investment_type == 'Unit Trusts'){
    account_output <- investment_function("https://api.ocbc.com:8243/investments/unittrust/1.0")
  }
  else{
    stop("Invalid investment type!")
  }

  return(account_output)
}
