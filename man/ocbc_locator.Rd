% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Locators.R
\name{ocbc_locator}
\alias{ocbc_locator}
\title{Make requests to the OCBC Locator category APIs}
\usage{
ocbc_locator(
  locator_type = c("ATM", "Branch", "Deposit"),
  acctoken,
  category = NULL,
  country = NULL,
  landmark = NULL,
  latitude = NULL,
  longitude = NULL,
  radius = NULL
)
}
\arguments{
\item{locator_type}{Locator type, hence the API that the function will call from. Required. Options are "ATM" for ATMs, "Branch" for Branches and Centres, "Deposit" for Deposit Machines and Safe Deposit Boxes. Note: it is recommended to only specify 1 locator type at a time.}

\item{acctoken}{Account token. Required. Users can sign up for their api key, api secret and account token at https://api.ocbc.com/store/home}

\item{category}{filter locators by category. Optional. \cr
\cr
For ATMs (locator_type = "ATM") options are: \cr
'1' for ATMs \cr
'2' for Fastlanes
'3' for Passbook update machines \cr
\cr
For OCBC Branches and Centres (locator_type = "Branch") options are: \cr
'1' for Al-Amin Bank Branch \cr
'2' for Bank Branch \cr
'3' for Business Centre \cr
'4' for Frank Store (Note: API content currently unavailable)\cr
'5' for Premier Centre (Note: API  content currently unavailable)\cr
'6' for Sunday Branch (Note: API content currently unavailable)\cr
'7' for Trade Centre (Note: API content currently unavailable)\cr
\cr
For Deposit Machines and Safe Deposit Boxes (locator_type = "Deposit") options are: \cr
'1' for Cash Deposit Machines \cr
'2' for Quick Cheque Deposit \cr
'3' for Safe Deposit \cr}

\item{country}{Filter locators by country. Optional. Options are 'SG' for Singapore and 'MY' for Malaysia}

\item{landmark}{Filter locators by local landmark. Optional. E.g 'Esplanade', '7-Eleven', 'park'}

\item{latitude}{Filter locators by latitude. Optional. (Requires longitutde and radius to filter for nearest locator)}

\item{longitude}{Filter locators by longtitude. Optional. (Requires latitude and radius to filter for nearest locator)}

\item{radius}{Filter locators by radius. Optional. (Requires longtitude and latitude to filter for nearest locator)}
}
\value{
Returns a list containing 2 items. \cr
1. The first is a detailed dataframe with variables of key information about the locators (e.g the category of the locator, the address of the locator, landmark where the locator is located, geological latitude and longitude of the locator, postal code, remarks and disclaimers). \cr
2.The second is an interactive map indicating the locations of the locators featured in the dataframe. Each point on the map correponds to a row in the above dataframe, users can click on each point to reveal a summary information table about the locator.
}
\description{
This function makes the information from the OCBC Locators category APIs easily accessible in an R-readable format. It also produces
an interactive map of the ATMs and their locations
There are 3 different types of locators, each with their own unique API \cr
1. ATMs \cr
2. OCBC Branches and Centres \cr
3. Deposit Machines and Safe Deposit Boxes \cr
}
\examples{
ocbc_locator(locator_type = "ATM", acctoken = Sys.getenv("ACC_TOKEN_OCBC"),
category = 1, country = "SG")
}
