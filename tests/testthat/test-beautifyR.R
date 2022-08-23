test_that("don't beatuify correct table", {
  inputstring <- "| col 1 | col 2 |
| :---- | :---- |
| value | value |"
  outputstring <- "| col 1 | col 2 |
| :---- | :---- |
| value | value |"
  expect_equal(beautifyR(inputstring), outputstring)
})

test_that("insert missing formatting line", {
  inputstring <- "| column1 | column2 | column3 |
|a| another longer string | x |"
  outputstring <- "| column1 | column2               | column3 |
| :------ | :-------------------- | :------ |
| a       | another longer string | x       |"
  expect_equal(beautifyR(inputstring), outputstring)
})

test_that("add missing column", {
  inputstring <- "| column1 | column2 | column3 |  | 
| :------ | :-------: | -------: |
|a| another longer string | x |"
  outputstring <- "| column1 |        column2        | column3 |     |
| :------ | :-------------------: | ------: | :-- |
| a       | another longer string |       x |     |"
  expect_equal(beautifyR(inputstring), outputstring)
})

test_that("shrink column", {
  inputstring <- "| column1           |        column2        |  column3 |
| :---------------- | :-------------------: | -------: |
| a                 | another longer |        x |
| a string and more |        string         |        x |
| a                 |        string         |        x |
| a                 |        string         |        x |"
  outputstring <- "| column1           |    column2     | column3 |
| :---------------- | :------------: | ------: |
| a                 | another longer |       x |
| a string and more |     string     |       x |
| a                 |     string     |       x |
| a                 |     string     |       x |"
  expect_equal(beautifyR(inputstring), outputstring)
})


test_that("testing recusion: should be correct after applying the function ONCE", {
  inputstring <- "| Some stuff | in | a | table |
|oaslkjf|olaksjf|olkjdsf|"
  outputstring <- "| Some stuff | in      | a       | table |
| :--------- | :------ | :------ | :---- |
| oaslkjf    | olaksjf | olkjdsf |       |"
  expect_equal(beautifyR(inputstring), outputstring)
})