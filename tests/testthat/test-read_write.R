context("read_write")


test_that(desc="regular_functions", code={

  test_folder <- "/tmp/rpkgs_test_folder/rcreds"

  ret <- write_credentials_to_file(
      username = "I am the user"
    , password = "super secret batman"
    , list_value = list("is", 1:5, c("even", "possible?"))
    , empty_value = c()
    , empty_string = ""
    , folder = test_folder
  )

  received_value <- read_credentials_from_file(folder=test_folder)

  expected_value <- list(
      username = "I am the user"
    , password = "super secret batman"
    , list_value = list("is", 1:5, c("even", "possible?"))
    , empty_value = structure(list(), .Names = character(0))
    , empty_string = ""
  )

  ## SAME
  expect_equal(received_value, expected_value)

  ## USE SOME OTHER KEY
  key3 <- create_key()
  expect_error(read_credentials_from_file(folder=test_folder, key=key3))
})

## TEST TODO:
## Can I overwrite a file?


test_that(desc="DB Funcs", code={

  test_folder <- "/tmp/rpkgs_test_folder/rcreds"

  ret <- write_db_credentials_to_file(
      dbname = "this_is_the_db"
    , port = 1234L
    , host = "very graceful"
    , username = "hopscotch"
    , password = "hopgin"
    , folder = test_folder
  )

  received_value <- read_db_credentials_from_file(folder=test_folder)

  expected_value <- list(    
      dbname = "this_is_the_db"
    , host = "very graceful"
    , port = 1234L
    , username = "hopscotch"
    , password = "hopgin"
  )

  ## The ordering of the 
  expect_equal(received_value[order(names(received_value))], expected_value[order(names(expected_value))])
  expect_equal(received_value, expected_value)

  ## USE SOME OTHER KEY
  key2 <- create_key()
  expect_error(read_db_credentials_from_file(folder=test_folder, key=key2))
})


