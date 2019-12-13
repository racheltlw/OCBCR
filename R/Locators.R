## 7 eleven does not work for deposits? or branches?? - need to make this abit better in terms of the error messages - an error message if the datafame is empty then say that there are no results- double check this


#' Make requests to the OCBC Locator category APIs
#'
#'This function makes the information from the OCBC Locators category APIs easily accessible in an R-readable format. It also produces
#'an interactive map of the ATMs and their locations
#' There are 3 different types of locators, each with their own unique API \cr
#' 1. ATMs \cr
#' 2. OCBC Branches and Centres \cr
#' 3. Deposit Machines and Safe Deposit Boxes \cr
#'
#' @import  httr jsonlite dplyr ggplot2 sf mapview
#' @importFrom purrr map_dfr
#' @param locator_type Locator type, hence the API that the function will call from. Required. Options are "ATM" for ATMs, "Branch" for Branches and Centres, "Deposit" for Deposit Machines and Safe Deposit Boxes
#' @param acctoken Account token. Required. Users can sign up for their api key, api secret and account token at https://api.ocbc.com/store/home
#' @param category filter locators by category. Optional. \cr
#' \cr
#' For ATMs (locator_type = "ATM") options are: \cr
#''1' for ATMs \cr
#''2' for Fastlanes
#''3' for Passbook update machines \cr
#'\cr
#'For OCBC Branches and Centres (locator_type = "Branch") options are: \cr
#''1' for Al-Amin Bank Branch \cr
#''2' for Bank Branch \cr
#''3' for Business Centre \cr
#''4' for Frank Store (Note: API content currently unavailable)\cr
#''5' for Premier Centre (Note: API  content currently unavailable)\cr
#''6' for Sunday Branch (Note: API content currently unavailable)\cr
#''7' for Trade Centre (Note: API content currently unavailable)\cr
#'\cr
#'For Deposit Machines and Safe Deposit Boxes (locator_type = "Deposit") options are: \cr
#''1' for Cash Deposit Machines \cr
#''2' for Quick Cheque Deposit \cr
#''3' for Safe Deposit \cr
#'
#' @param country Filter locators by country. Optional. Options are 'SG' for Singapore and 'MY' for Malaysia
#' @param landmark Filter locators by local landmark. Optional. E.g 'Esplanade', '7-Eleven', 'park'
#' @param latitude Filter locators by latitude. Optional. (Requires longitutde and radius to filter for nearest locator)
#' @param longitude Filter locators by longtitude. Optional. (Requires latitude and radius to filter for nearest locator)
#' @param radius Filter locators by radius. Optional. (Requires longtitude and latitude to filter for nearest locator)
#' @return Returns a list containing 2 items. \cr
#' 1. The first is a detailed dataframe with variables of key information about the locators (e.g the category of the locator, the address of the locator, landmark where the locator is located, geological latitude and longitude of the locator, postal code, remarks and disclaimers). \cr
#' 2.The second is an interactive map indicating the locations of the locators featured in the dataframe. Each point on the map correponds to a row in the above dataframe, users can click on each point to reveal a summary information table about the locator.
#' @examples
#' ocbc_locator(locator_type = "ATM", acctoken = Sys.getenv("ACC_TOKEN_OCBC"),
#' category = 1, country = "SG")
#' @export

#if api call is left blank the default is the ATMs ?????

ocbc_locator <- function(locator_type = c("ATM", "Branch", "Deposit"), acctoken, category = NULL, country = NULL, landmark = NULL, latitude = NULL, longitude = NULL, radius = NULL){

  #some error checks to make sure the parameter inputs are in the correct format
  if(any(!is.character(locator_type))) {
    stop("Make sure locator type is in quotation marks!")
  }

  locator_function<- function(url_link, acc_token = acctoken, category_ = category, country_ = country, landmark_ = landmark, latitude_ = latitude, longitude_ = longitude, radius_ = radius) {
    endpoint <- url_link
    query_params <- list('category' = category_, 'country' = country_, 'landmark' = landmark_, 'latitude' = latitude_, 'longitude' = longitude_, 'radius' = radius_)
    get_locator <- GET(endpoint, query = query_params, add_headers(Authorization = paste("Bearer", acc_token)))
    status <- http_status(get_locator)
    if(status$category != "Success") {
      stop("Unsuccessful API call for ", locator_type, ". Please check the parameters or connection.")
    }
    else{
      message("API call for ", locator_type, " successful!")
    }
    locator <- content(get_locator, as = "parsed")
    if(length(locator[[1]]) == 0){
      print('Search returned no results!')
    }
    else{
      ocbc_locators <- flatten(as.data.frame(fromJSON(toJSON(locator)))) %>%
        mutate_all(~unlist(.))
      locator_location <- ocbc_locators %>%
        rename("latitude" = 4, "longitude" = 5)
      locations <- st_as_sf(locator_location, coords = c("longitude", "latitude"), crs = 4326)
      map_locator <- mapview(locations)
    }
    return(list(ocbc_locators, map_locator))
  }

   if(locator_type == "ATM" & is.null(category)){
    locator_output <- locator_function("https://api.ocbc.com:8243/atm_locator/1.1")
  }
  else if(locator_type == "Branch" & is.null(category)){
    locator_output <- locator_function("https://api.ocbc.com:8243/branch_locator/1.1")
  }
  else if(locator_type == "Deposit" & is.null(category)){
    locator_output <- locator_function("https://api.ocbc.com:8243/locators/depositmachinesandsafeboxes/1.0")
  }
  else if(locator_type == "ATM" & (category == 1 | category == 2 | category == 3)){
    locator_output <- locator_function("https://api.ocbc.com:8243/atm_locator/1.1")
  }
  else if(locator_type == "Branch" & (category == 1|category == 2|category == 3)){ #4-7 currently excluded as currently unavailable according to OCBC
    locator_output <- locator_function("https://api.ocbc.com:8243/branch_locator/1.1")
  }
  else if(locator_type == "Deposit" & (category == 1|category == 2|category == 3)){
    locator_output <- locator_function("https://api.ocbc.com:8243/locators/depositmachinesandsafeboxes/1.0")
  }
  else {
    stop("Invalid locator type or category out of range! Check that locator is 'ATM' or 'Branch' or 'Deposit' and read documentation for the corresponding categories")
  }
  return(locator_output)
}







