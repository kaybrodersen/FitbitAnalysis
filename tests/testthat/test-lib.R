test_that("PascalCase rejects non-character input", {
  expect_error(PascalCase(42))
  expect_error(PascalCase(NULL))
})

test_that("PascalCase works on standard input", {
  expect_equal(PascalCase("foo bar"), "FooBar")
  expect_equal(PascalCase("aBc_D"), "ABcD")
  expect_equal(PascalCase("A b c D"), "ABCD")
})
