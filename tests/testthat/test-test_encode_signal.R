test_that("encode a signal", {
    codebook <- construct_codebook(c('A', 'B', 'C'), c(1, 1, 1))
    
    result <- encode_signal(
        codebook=codebook,
        symbol = 'A',
        signal_power = 3,
        timesteps = 10
    )
    
    expect_true('group_index' %in% colnames(result))
    expect_true('spike_time' %in% colnames(result))
    expect_true(nrow(result) > 1)
    expect_true(1 %in% result$group_index)
    expect_false(2 %in% result$group_index)
})

test_that("encode a signal with a leak", {
    codebook <- construct_codebook(c('A', 'B', 'C'), c(1, 1, 1))
    
    result <- encode_signal(
        codebook=codebook,
        symbol = 'A',
        signal_power = 3,
        timesteps = 10,
        leak_symbols = c('B'),
        leak_powers = c(3)
    )
    
    expect_true('group_index' %in% colnames(result))
    expect_true('spike_time' %in% colnames(result))
    expect_true(nrow(result) > 1)
    expect_true(1 %in% result$group_index)
    expect_true(2 %in% result$group_index)
    
    
})
