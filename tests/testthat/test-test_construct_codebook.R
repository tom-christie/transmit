test_that("construct simple codebook", {
    codebook <- construct_codebook(c('A', 'B', 'C'))
    expect_equal(codebook$A$index,1)
    expect_equal(codebook$A$size,1)
    expect_equal(codebook$A$symbol,'A')
})
