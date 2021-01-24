test_that("DropNA rejects bad input", {
  expect_error(DropNA(matrix(c(1, 2, 3))), "not an atomic vector")
})

test_that("DropNA works on standard input", {
  expect_equal(DropNA(c(1, 2, NA, 4)), c(1, 2, 4))
  expect_equal(DropNA(NULL), NULL)
})

test_that("PascalCase rejects non-character input", {
  expect_error(PascalCase(42))
  expect_error(PascalCase(NULL))
})

test_that("PascalCase works on standard input", {
  expect_equal(PascalCase("foo bar"), "FooBar")
  expect_equal(PascalCase("aBc_D"), "ABcD")
  expect_equal(PascalCase("A b c D"), "ABCD")
})
