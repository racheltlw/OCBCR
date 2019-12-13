#' Make requests to OCBC Accounts category APIs
#'
#' This function makes the information from the OCBC Accounts category APIs easily accessible in an R-readable format.
#' There are 6 different types of accounts, each with their own unique API \cr
#' 1. Children's Accounts \cr
#' 2. Deposit Accounts \cr
#' 3. Foreign Accounts \cr
#' 4. Saving Accounts \cr
#' 5. Current Accounts \cr
#' 6. Trading Accounts \cr
#'
#' @import httr jsonlite dplyr ggplot2
#' @importFrom purrr map_dfr
#'
#' @param account_type The account type, hence the API that the function will call from. Required. Options are Children, Deposit, Foreign, Saving, Current, Trading. Note: it is recommended to only specify 1 account type at a time.
#' @param acctoken Account token. Required. Users can sign up for their api key, api secret and account token at https://api.ocbc.com/store/home
#' @param country Either "SG" for Singapore, or "MY" for Malaysia. Optional. Note: Deposit, Foreign and Trading Accounts do not have the country parameter. If specified, the country parameter is automatically ignored.
#' @param subCategory Filter accounts by subcategories. Optional. (e.g Children's account, Foreign Currency) Note: subCategory should correspond to the account_type (e.g Children's account for account_type = 'Children')
#' @param productName Filter accounts by keywords. Optional. Note: productName should correspond to the account_type (e.g Mighty Savers, Child Development for account_type = 'Children')
#' @param benefit Filter accounts by benefit. Optional. (e.g Bonus Interest, Interest)
#' @return A detailed dataframe of OCBC accounts with variables such as eligibility, minimumBalance, feesAndcharges, benefits, providing the user with the full information about the accounts.
#' @examples
#' ocbc_accounts("Children", acctoken = Sys.getenv("ACC_TOKEN_OCBC"),
#' country = "SG", productName = "Mighty Savers")
#'@export

ocbc_accounts <- function(account_type = c('Children', 'Deposit', 'Foreign', 'Saving', 'Current', 'Trading'), acctoken, country = NULL, subCategory = NULL, productName = NULL, benefit = NULL){

  #some error checks to make sure the parameter inputs are in the correct format
  if(any(!is.character(account_type))) {
    stop("Make sure account type is in quotation marks!")
  }

  clean_up <- function(string) {
    return(gsub("<.*?>", "", string))
  }

  ocbc_function <- function(url_link, acc_token = acctoken , Country = country, SubCategory = subCategory, ProductName = productName, Benefit = benefit) {
    endpoint <- url_link
    query_params <- list("country" = Country, "subCategory" = SubCategory, "productName" = ProductName, "benefit" = Benefit)
    get_ocbc <- GET(endpoint, query = query_params, add_headers(Authorization = paste("Bearer", acc_token)))
    Sys.sleep(6)
    status <- http_status(get_ocbc)
    if (status$category != "Success") {
      stop("Unsuccessful API call for ", account_type, ". Please check your parameters or connection.")
    }
    else{
      message("API call for ", paste0(account_type, ", "), "successful!")
    }
    if(any(account_type == 'Trading')){
      ocbc <- content(get_ocbc, as = "parsed")
      ocbc_table <- fromJSON(toJSON(ocbc$OSPLAccountList), simplifyDataFrame = TRUE) %>%
        mutate_all(~unlist(.)) %>%
        map_dfr(clean_up)
    }
    else{
      ocbc <- content(get_ocbc, as = "parsed")
      ocbc_table <- fromJSON(toJSON(ocbc$CASAAccountsList[[1]]$subCategoryList[[1]]$product), simplifyDataFrame = TRUE) %>%
        mutate_all(~unlist(.)) %>%
        map_dfr(clean_up)
    }
    return(ocbc_table)
  }

  if(account_type == 'Children'){
    account_output <- ocbc_function("https://api.ocbc.com:8243/Children_Accounts/1.1")
  }
  else if(account_type == 'Deposit'){
    account_output <- ocbc_function("https://api.ocbc.com:8243/Deposit_Accounts/1.0")
  }
  else if(account_type == 'Foreign'){
    account_output <- ocbc_function("https://api.ocbc.com:8243/Foreign_Accounts/1.0")
  }
  else if(account_type == 'Saving'){
   account_output <- ocbc_function("https://api.ocbc.com:8243/accounts/savings/1.0")
  }
  else if(account_type == 'Current'){
    account_output <- ocbc_function("https://api.ocbc.com:8243/accounts/current/1.0")
  }
  else if(account_type == 'Trading'){
    account_output <- ocbc_function("https://api.ocbc.com:8243/accounts/ospl/trading/1.0")
  }
  else{
    stop("Invalid account type!")
  }

  return(account_output)
}




