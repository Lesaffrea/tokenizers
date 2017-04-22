context("Interoperability")

test_that("Lists of tokens can be converted to data.frame", {
  x <- c(a = "This is the demo sentence.", b = "This is a second document.")
  tokens_l <- tokenize_words(x)
  tokens_df <- tokens_list_to_df(tokens_l)
  expect_named(tokens_df, c("doc_id", "token_index", "token"))
  expect_identical(tokens_df_to_list(tokens_df), tokens_l)
})