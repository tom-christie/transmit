test_that("constructing codebook", {
  codebook <- construct_codebook(c('A','B','C'))
  codebook <- list(unlist(codebook))
  #assert_that(codebook[['A.list']] == '1')
  #assert_that(codebook$A.size == '1')
  #assert_that(codebook$A.symbol == 'A')
})
