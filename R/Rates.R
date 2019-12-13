#' Make requests to the OCBC Rates category APIs
#'
#' This function makes the information from the OCBC Rates category APIs easily accessible in an R-readable format. There are three different types of rates, each with their own unique API \cr
#' 1. Fixed Deposit Rates \cr
#' 2. Forex Rates \cr
#' 3. Unit Trust Rates  \cr
#'
#' @import  httr jsonlite ggplot2 dplyr stringr
#' @importFrom rlang .data
#' @param rate_type Rate type, hence the API that the function will call from. Required. Options are "fixed deposit" for Fixed Deposit Rates, "forex" for Forex Rates, "unit trust" for Unit Trust Rates. Note: it is recommended to only specify 1 rate type at a time.
#' @param acctoken Account token. Required. Users can sign up for their api key, api secret and account token at https://api.ocbc.com/store/home
#' @param local_currency The currency in which the rates will be shown. Optional. Options are "SG" for Singapore and "MY" for Malaysia
#' @param foreign_currency The currency for foreign exchange. Input the official ISO 4217 currency code (e.g GBP for British Pounds). Optional. Note: Only required for Forex Rates. If specified for Fixed Deposit and Unit Trust Rates, the foreign_currency parameter is automatically ignored.
#' @return Returns various items depending on the type of rate specified \cr
#' 1. For Fixed Deposits the function returns a list containing a dataframe of the fixed deposit interest rates according to the duration in months as well as a graph of this information. \cr
#' 2. For Foreign Exchange the function returns a dataframe containing the bank buying rate and bank selling rate, information on the local currency and foreign currency as well as the rate list unit. \cr
#' 3. For Unit Trust the function returns an empty dataframe as this information is not yet available through the API.
#' @examples
#' ocbc_rates("fixed deposit", acctoken = Sys.getenv("ACC_TOKEN_OCBC"))
#' @export

ocbc_rates <- function(rate_type = c("fixed deposit", "forex", "unit trust"), acctoken, local_currency = NULL, foreign_currency = NULL){

  if(any(!is.character(rate_type))) {
    stop("Make sure rate type is in quotation marks!")
  }

  rates_api <- function(url_link, acc_token = acctoken , country = local_currency){
    endpoint <- url_link
    query_params <- list('country' = country)
    get_rates <- GET(endpoint, query = query_params, add_headers(Authorization = paste("Bearer", acc_token)))
    Sys.sleep(6)
    status <- http_status(get_rates)
    if(status$category != "Success") {
      stop("Unsuccessful API call for ", rate_type, ". Please check the parameters or connection.")
    }
    else{
      message("API call for ", rate_type, " successful!")
    }
    if(rate_type == "unit trust"){
      rates <- content(get_rates, as = "parsed")[[1]]
      rate_list <- flatten(as.data.frame(fromJSON(toJSON(rates))))
    }
    else{
      rates <- content(get_rates, as = "parsed")
      rate_list <- flatten(as.data.frame(fromJSON(toJSON(rates)))) %>%
        mutate_all(~unlist(.))
    }
    return(rate_list)
  }

  if(rate_type == "fixed deposit"){
    local_currency = "MY" #SG is not yet available for fixed deposit, hence setting MY as the default
    rate_df <- rates_api("https://api.ocbc.com:8243/rates/fixeddeposit/1.0") %>%
      rename("duration" = "RateList.duration", "interest_rate" = "RateList.interestRate", "currency" = "RateList.currency")
    rate_df_plot <- rate_df %>%
      mutate(duration = as.numeric(str_extract(.data$duration, "(\\d)+"))) %>%
      ggplot(aes(x = .data$duration, y = .data$interest_rate)) + labs(title = "Graph of OCBC Fixed Deposit Interest Rates by Number of Months", x = "Months", y = "Interest Rates (%)") + geom_line()
    rate_output <- list(rate_df, rate_df_plot)
  }
  else if(rate_type == "forex" & !is.null(foreign_currency)){
    rate_output <- rates_api("https://api.ocbc.com:8243/Forex/1.1") %>%
      filter("RateList.toCurrency" == foreign_currency) %>%
      rename("bank buying rate" = "RateList.bankBuyingRateTT", "bank selling rate" = "RateList.bankSellingRate", "from currency" = "RateList.fromCurrency", "to currency" = "RateList.toCurrency", "unit" = "RateList.unit")
  }
  else if(rate_type == "forex"){
    rate_output <- rates_api("https://api.ocbc.com:8243/Forex/1.1") %>%
      rename("bank buying rate" = "RateList.bankBuyingRateTT", "bank selling rate" = "RateList.bankSellingRate", "from currency" = "RateList.fromCurrency", "to currency" = "RateList.toCurrency", "unit" = "RateList.unit")
  }
  else if(rate_type == "unit trust"){
    local_currency = 'MY' #SG not available, setting MY as default
    rate_output <- rates_api("https://api.ocbc.com:8243/rates/unittrust/1.0")
  }
  else{
    print("Invalid rate type")
  }
return(rate_output)
}


