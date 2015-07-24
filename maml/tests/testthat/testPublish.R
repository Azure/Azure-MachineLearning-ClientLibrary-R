add <- function (x, y) {
  print("This will add x and y")
  return(x + y)
}

test_that("Wrapper code (less maml.input/output) correctly applies function to output", {

})

test_that("GetFunctionString gets a function source code with no quotes or newlines", {
  expect_equal(getFunctionString(add), "function (x, y) \r\n{\r\n    print(\"This will add x and y\")\r\n    return(x + y)\r\n}")
})

test_that("packDependencies returns a zip file encoded as a base64 string", {

})

test_that("packDependencies handles recursive packaging", {

})

test_that("packDepedencies cleans up after itself, even in case of error", {

})

test_that("publishWebService returns a working web service", {

})

test_that("publishWebService handles bad input correctly", {
  expect_error(, "Data type not supported")
  expect_error(, "inputSchema length doesn't match function signature")
})

test_that("publishWebService handles various HTTP error codes correctly", {

})

test_that("updateWebService correctly updates a web service", {

})

