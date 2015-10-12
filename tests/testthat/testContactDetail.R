library(prideR)
context("Contact Detail")

test.contact <- ContactDetail(title="Mr", first.name="Rui", last.name="Wang", email="my@email.com", affiliation="My workplace")

test_that("A ContactDetail is created correctly", {
  expect_equal(title(test.contact), "Mr")
  expect_equal(first.name(test.contact), "Rui")
  expect_equal(last.name(test.contact), "Wang")
  expect_equal(email(test.contact), "my@email.com")
  expect_equal(affiliation(test.contact), "My workplace")
})

test_that("Setters for ContactDetail are working", {
  title(test.contact) <- "Dr"
  expect_equal(title(test.contact), "Dr")
  
  first.name(test.contact) <- "Jose"
  expect_equal(first.name(test.contact), "Jose")
  
  last.name(test.contact) <- "Dianes"
  expect_equal(last.name(test.contact), "Dianes")
  
  email(test.contact) <- "my@gmail.com"
  expect_equal(email(test.contact), "my@gmail.com")
  
  affiliation(test.contact) <- "My home"
  expect_equal(affiliation(test.contact), "My home")
})

# test_that("Email validation is working", {
#   expect_error(email(test.contact)<- "my.wrong.email", "invalid class “ContactDetail” object")
# })


