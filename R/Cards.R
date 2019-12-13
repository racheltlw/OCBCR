#' Make requests to OCBC Cards category APIs
#'
#' This function makes the information from the OCBC Cards category APIs easily accessible in an R-readable format.
#' There are 5 different categories, each with their own unique API \cr
#' 1. Credit Cards \cr
#' 2. Credit Card Advisor \cr
#' 3. Credit Card Promotions \cr
#' 4. Debit Card Advisor \cr
#' 5. Rewards Catalogue \cr
#'
#' @import httr jsonlite dplyr ggplot2
#' @importFrom purrr map_dfr
#'
#' @param card_type The card category, hence the API that the function will call from. Required.  Options are Credit, Credit Advisor, Credit Promotions, Debit Advisor and Rewards. Note: it is recommended to only specify 1 card type at a time.
#' @param acctoken Account token. Required. Users can sign up for their api key, api secret and account token at https://api.ocbc.com/store/home
#' @param country Country for which available credit cards products are retrieved. Currently supported only for Malaysia (MY). Optional. Note: Parameter for 'Credit' category only. If specified for other categories, this parameter will be automatically ignored.
#' @param product_type Product type. Optional. Note: Parameter for 'Credit' category only. If specified for other categories, this parameter will be automatically ignored.
#' @param keyword Recommend a credit or debit card based on this keyword. Optional. Note: Parameter for 'Credit Advisor' and 'Debit Advisor categories only.
#' @param tag Filter promotions by categories and subcategories. Optional. Note: Parameter for 'Credit Promotions category only.
#' @param name Filter promotions by merchant names. Optional. Note: Parameter for 'Credit Promotions category only.
#' @param address Filter promotions by address. Optional. Note: Parameter for 'Credit Promotions category only.
#' @param page Specify page number for paginated response. Optional. Note: Parameter for 'Credit Promotions' and 'Rewards Catalogue' category only.
#' @param limit Number of promotion entries to return per page. Default = 10. Optional. Note: Parameter for 'Credit Promotions' and 'Rewards Catalogue' category only.
#' @param category Filter rewards by categories (e.g: retail, dining, entertainment, travel, other). Optional. Note: Parameter for 'Rewards Catalogue' category only.
#' @param merchant Filter rewards by merchant names. Optional. Note: Parameter for 'Rewards Catalogue' category only.
#' @param price Filter maximum OCBC$ cost of rewards. Optional. Note: Parameter for 'Rewards Catalogue' category only.
#'
#' @return A detailed dataframe of OCBC card categories with information related to the category specified (e.g A dataframe with information such as merchant, redemption item and validity date of each reward). For the 'Credit Promotions' category, the function returns a list of 2 dataframes: one of the information of the promotions and the other the terms and conditions.
#' @examples
#' ocbc_cards("Rewards", acctoken = Sys.getenv("ACC_TOKEN_OCBC"), limit = "30")
#'@export

ocbc_cards <- function(card_type = c('Credit', 'Credit Advisor', 'Credit Promotions ', 'Debit Advisor', 'Rewards'), acctoken, country = NULL, product_type = NULL, keyword = NULL, tag = NULL, name = NULL, address = NULL, page = NULL, limit = 10, category = NULL, merchant = NULL, price = NULL){

  #some error checks to make sure the parameter inputs are in the correct format
  if(any(!is.character(card_type))) {
    stop("Make sure card type is in quotation marks!")
  }

  clean_up <- function(string) {
    return(gsub("<.*?>", " ", string))
  }
  card_function <- function(url_link, acc_token = acctoken , Country = country, Product_type = product_type, Keyword = keyword, Tag = tag, Name = name, Address = address, Page = page, Limit = limit, Category = category, Merchant = merchant, Price = price) {
    endpoint <- url_link
    query_params <- list("country" = Country, "product_type" = Product_type, "keyword" = Keyword, "tag" = Tag, "name" = Name, "address" = Address, "page" = Page, "limit" = Limit, "category" = Category, "merchant" = Merchant, "price" = Price)
    get_card <- GET(endpoint, query = query_params, add_headers(Authorization = paste("Bearer", acc_token)))
    Sys.sleep(6)
    status <- http_status(get_card)
    if(status$category != "Success") {
      stop("Unsuccessful API call for ", card_type, ". Please check the parameters or connection.")
    }
    else{
      message("API call for ", paste0(card_type, ", "), "successful!")
    }
    if(any(card_type == 'Credit')){
      card <- content(get_card, as = "parsed")
      card_table <- flatten(as.data.frame(fromJSON(toJSON(card[[1]][[1]]))))
    }
    else if(card_type == 'Credit Promotions'){
      card <- content(get_card)[[1]]
      table <- fromJSON(toJSON(card), simplifyDataFrame = TRUE) %>%
        select(1:7, 9:12) %>%
        mutate_all(~unlist(.)) %>%
        map_dfr(clean_up)
      terms <- as.data.frame(fromJSON(toJSON(card[[1]][[8]]), simplifyDataFrame = TRUE)) %>%
        rename("Terms" = "V1")  %>%
        map_dfr(clean_up)
      card_table <- list(table, terms)
    }
    else{
      card <- content(get_card)[[1]]
      card_table <- fromJSON(toJSON(card), simplifyDataFrame = TRUE) %>%
        map_dfr(clean_up)
    }
  return(card_table)
  }

  if(any(card_type == 'Credit')){
    account_output <- card_function("https://api.ocbc.com:8243/cards/getProducts/1.0")
  }
  else if(card_type == 'Credit Advisor'){
    account_output <- card_function("https://api.ocbc.com:8243/CC/1.0")
  }
  else if(card_type == 'Credit Promotions'){
    account_output <- card_function("https://api.ocbc.com:8243/Card_Promotions/1.0")
  }
  else if(card_type == 'Debit Advisor'){
    account_output <- card_function("https://api.ocbc.com:8243/cards/debitcard/1.0")
  }
  else if(card_type == 'Rewards'){
    account_output <- card_function("https://api.ocbc.com:8243/Card_Rewards/1.0")
  }
  else{
    stop("Invalid card type!")
  }

  return(account_output)
}








