library(OCBCR)

context("Function inputs")

#some examples of tests that can be used to test the argument input

test_that("Input of accounts function is as expected", {
  expect_error(ocbc_accounts(account_type = Children, acctoken = Sys.getenv("ACC_TOKEN_OCBC")))
  expect_error(ocbc_accounts(account_type = 1234, acctoken = Sys.getenv("ACC_TOKEN_OCBC")))
  expect_error(ocbc_accounts(account_type = NA, acctoken = Sys.getenv("ACC_TOKEN_OCBC")))
  expect_error(ocbc_accounts(account_type = "Not one of the specified arguments" , acctoken = Sys.getenv("ACC_TOKEN_OCBC")))
})


test_that("Input of cards function is as expected", {
  expect_error(ocbc_cards(card_type = Credit, acctoken = Sys.getenv("ACC_TOKEN_OCBC")))
  expect_error(ocbc_cards(card_type = list("Credit Advisor, Debit Advisor"), acctoken = Sys.getenv("ACC_TOKEN_OCBC")))
  expect_error(ocbc_cards(card_type = NA, acctoken = Sys.getenv("ACC_TOKEN_OCBC")))
  expect_error(ocbc_cards(card_type = "Not one of the specified arguments" , acctoken = Sys.getenv("ACC_TOKEN_OCBC")))
})

test_that("Input of insurance function is as expected", {
  expect_error(ocbc_insurance(insurance_type = Travel, acctoken = Sys.getenv("ACC_TOKEN_OCBC")))
  expect_error(ocbc_insurance(insurance_type = data.frame(x = 1, y = 1:10), acctoken = Sys.getenv("ACC_TOKEN_OCBC")))
  expect_error(ocbc_insurance(insurance_type = NA, acctoken = Sys.getenv("ACC_TOKEN_OCBC")))
  expect_error(ocbc_insurance(insurance_type= "Not one of the specified arguments" , acctoken = Sys.getenv("ACC_TOKEN_OCBC")))
})

test_that("Input of investments function is as expected", {
  expect_error(ocbc_investments(investment_type = data.frame(x = 1, y = 1:10), acctoken = Sys.getenv("ACC_TOKEN_OCBC")))
  expect_error(ocbc_investments(investment_type = FALSE, acctoken = Sys.getenv("ACC_TOKEN_OCBC")))
  expect_error(ocbc_investments(investment_type = NA, acctoken = Sys.getenv("ACC_TOKEN_OCBC")))
  expect_error(ocbc_investments(investment_type = "Not one of the specified arguments" , acctoken = Sys.getenv("ACC_TOKEN_OCBC")))
})

test_that("Input of loans function is as expected", {
  expect_error(ocbc_loans(loan_type = Cash, acctoken = Sys.getenv("ACC_TOKEN_OCBC")))
  expect_error(ocbc_loans(loan_type = TRUE, acctoken = Sys.getenv("ACC_TOKEN_OCBC")))
  expect_error(ocbc_loans(loan_type = NA, acctoken = Sys.getenv("ACC_TOKEN_OCBC")))
  expect_error(ocbc_loans(loan_type = "Not one of the specified arguments" , acctoken = Sys.getenv("ACC_TOKEN_OCBC")))
})


test_that("Input of locators function is as expected", {
  expect_error(ocbc_locators(locator_type = ATM, acctoken = Sys.getenv("ACC_TOKEN_OCBC")))
  expect_error(ocbc_locators(locator_type = matrix(c(1,2,3, 11,12,13), nrow = 2, ncol = 3, byrow = TRUE), acctoken = Sys.getenv("ACC_TOKEN_OCBC")))
  expect_error(ocbc_locators(locator_type = NA, acctoken = Sys.getenv("ACC_TOKEN_OCBC")))
  expect_error(ocbc_locators(locator_type = "Not one of the specified arguments" , acctoken = Sys.getenv("ACC_TOKEN_OCBC")))
})

test_that("Input of rates function is as expected", {
  expect_error(ocbc_rates(rate_type = Forex, acctoken = Sys.getenv("ACC_TOKEN_OCBC")))
  expect_error(ocbc_rates(rate_type = NULL, acctoken = Sys.getenv("ACC_TOKEN_OCBC")))
  expect_error(ocbc_rates(rate_type = NA, acctoken = Sys.getenv("ACC_TOKEN_OCBC")))
  expect_error(ocbc_rates(rate_type = "Not one of the specified arguments" , acctoken = Sys.getenv("ACC_TOKEN_OCBC")))
})



