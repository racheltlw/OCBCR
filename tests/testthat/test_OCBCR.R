

library(OCBCR)

context("Function outputs")

#note - I do not test for specific dimensions of dataframe since the information via the API is constantly being updated

test_that("Output of accounts function is as expected", {
  expect_is(ocbc_accounts(account_type = 'Children', acctoken = Sys.getenv("ACC_TOKEN_OCBC")), 'data.frame')
  expect_is(ocbc_accounts(account_type = 'Deposit', acctoken = Sys.getenv("ACC_TOKEN_OCBC")), 'data.frame')
  expect_is(ocbc_accounts(account_type = 'Foreign', acctoken = Sys.getenv("ACC_TOKEN_OCBC")), 'data.frame')
  expect_is(ocbc_accounts(account_type = 'Saving', acctoken = Sys.getenv("ACC_TOKEN_OCBC")), 'data.frame')
  expect_is(ocbc_accounts(account_type = 'Current', acctoken = Sys.getenv("ACC_TOKEN_OCBC")), 'data.frame')
  expect_is(ocbc_accounts(account_type = 'Trading', acctoken = Sys.getenv("ACC_TOKEN_OCBC")), 'data.frame')

})
test_that("Output of locator function is as expected", {
  expect_is(ocbc_locator(locator_type = 'ATM', acctoken = Sys.getenv("ACC_TOKEN_OCBC")), 'list')
  expect_is(ocbc_locator(locator_type = 'Branch', acctoken = Sys.getenv("ACC_TOKEN_OCBC")), 'list')
  expect_is(ocbc_locator(locator_type = 'Deposit', acctoken = Sys.getenv("ACC_TOKEN_OCBC")), 'list')
})

test_that('Output of card function is as expected', {
  expect_is(ocbc_cards(card_type = "Credit", acctoken = Sys.getenv("ACC_TOKEN_OCBC")), 'data.frame')
  expect_is(ocbc_cards(card_type = "Credit Advisor", acctoken = Sys.getenv("ACC_TOKEN_OCBC")), 'data.frame')
  expect_is(ocbc_cards(card_type = "Credit Promotions", acctoken = Sys.getenv("ACC_TOKEN_OCBC")), 'data.frame')
  expect_is(ocbc_cards(card_type = "Debit Advisor", acctoken = Sys.getenv("ACC_TOKEN_OCBC")), 'data.frame')
  expect_is(ocbc_cards(card_type = "Rewards", acctoken = Sys.getenv("ACC_TOKEN_OCBC")), 'data.frame')
})

test_that('Output of insurance function is as expected', {
  expect_is(ocbc_insurance(insurance_type = "Accident and Health", acctoken = Sys.getenv("ACC_TOKEN_OCBC")), 'data.frame')
  expect_is(ocbc_insurance(insurance_type = "Car", acctoken = Sys.getenv("ACC_TOKEN_OCBC")), 'data.frame')
  expect_is(ocbc_insurance(insurance_type = "Endowment", acctoken = Sys.getenv("ACC_TOKEN_OCBC")), 'data.frame')
  expect_is(ocbc_insurance(insurance_type = "Home and Mortgage", acctoken = Sys.getenv("ACC_TOKEN_OCBC")), 'data.frame')
  expect_is(ocbc_insurance(insurance_type = "Life", acctoken = Sys.getenv("ACC_TOKEN_OCBC")), 'data.frame')
  expect_is(ocbc_insurance(insurance_type = "Maternity", acctoken = Sys.getenv("ACC_TOKEN_OCBC")), 'data.frame')
  expect_is(ocbc_insurance(insurance_type = "Travel", acctoken = Sys.getenv("ACC_TOKEN_OCBC")), 'data.frame')
})

test_that("Output of rate function is as expected", {
  expect_is(ocbc_rates(rate_type = 'fixed deposit', acctoken = Sys.getenv("ACC_TOKEN_OCBC")), 'list')
  expect_is(ocbc_rates(rate_type = 'forex', acctoken = Sys.getenv("ACC_TOKEN_OCBC")), 'data.frame')
  expect_is(ocbc_rates(rate_type = 'unit trust', acctoken = Sys.getenv("ACC_TOKEN_OCBC")), 'data.frame')
})

test_that("Output of investment function is as expected", {
  expect_is(ocbc_investments(investment_type = 'Bonds', acctoken = Sys.getenv("ACC_TOKEN_OCBC")), 'data.frame')
  expect_is(ocbc_investments(investment_type = 'Equity and Forex', acctoken = Sys.getenv("ACC_TOKEN_OCBC")), 'data.frame')
  expect_is(ocbc_investments(investment_type = 'Listed Securities', acctoken = Sys.getenv("ACC_TOKEN_OCBC")), 'data.frame')
  expect_is(ocbc_investments(investment_type = 'Leveraged Forex', acctoken = Sys.getenv("ACC_TOKEN_OCBC")), 'data.frame')
  expect_is(ocbc_investments(investment_type = 'Treasury', acctoken = Sys.getenv("ACC_TOKEN_OCBC")), 'data.frame')
  expect_is(ocbc_investments(investment_type = 'Unlisted Securities', acctoken = Sys.getenv("ACC_TOKEN_OCBC")), 'data.frame')
  expect_is(ocbc_investments(investment_type = 'Unit Trusts', acctoken = Sys.getenv("ACC_TOKEN_OCBC")), 'data.frame')
})

test_that("Output of loan function is as expected", {
  expect_is(ocbc_loans(loan_type = 'Car', acctoken = Sys.getenv("ACC_TOKEN_OCBC")), 'data.frame')
  expect_is(ocbc_loans(loan_type = 'Cash', acctoken = Sys.getenv("ACC_TOKEN_OCBC")), 'data.frame')
  expect_is(ocbc_loans(loan_type = 'Home', acctoken = Sys.getenv("ACC_TOKEN_OCBC")), 'data.frame')
  expect_is(ocbc_loans(loan_type = 'Property', acctoken = Sys.getenv("ACC_TOKEN_OCBC")), 'data.frame')
  expect_is(ocbc_loans(loan_type = 'Secured Overdraft', acctoken = Sys.getenv("ACC_TOKEN_OCBC")), 'data.frame')
  expect_is(ocbc_loans(loan_type = 'SME', acctoken = Sys.getenv("ACC_TOKEN_OCBC")), 'data.frame')
  expect_is(ocbc_loans(loan_type = 'Study', acctoken = Sys.getenv("ACC_TOKEN_OCBC")), 'data.frame')
})





