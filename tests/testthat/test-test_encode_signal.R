test_that("encode a signal", {
    codebook <- construct_codebook(c('A', 'B', 'C'))
    
    result <- encode_signal(
        codebook=codebook,
        symbol = 'A',
        signal_power = 10,
        duration_in_seconds = 10
    )
    
    expect_true('group_index' %in% colnames(result))
    expect_true('spike_time' %in% colnames(result))
    expect_true(nrow(result) > 1)
    expect_true(1 %in% result$group_index)
    expect_false(2 %in% result$group_index)
})

test_that("encode a signal with a leak", {
    codebook <- construct_codebook(c('A', 'B', 'C'))
    
    result <- encode_signal(
        codebook=codebook,
        symbol = 'A',
        signal_power = 10,
        duration_in_seconds = 10,
        leak_symbols = c('B'),
        leak_powers = c(3)
    )
    
    expect_true('group_index' %in% colnames(result))
    expect_true('spike_time' %in% colnames(result))
    expect_true(nrow(result) > 1)
    expect_true(1 %in% result$group_index)
    expect_true(2 %in% result$group_index)
    
    
})



test_that("add a leak to transmitted signal - epected failure", {
    codebook <- construct_codebook(c('A', 'B', 'C'))
    
    expect_error({
        result <- encode_signal(
            codebook=codebook,
            symbol = 'A',
            signal_power = 10,
            duration_in_seconds = 10,
            leak_symbols = c('A'),
            leak_powers = c(3)
        )
    })
    
})



test_that("send a symbol not in the codebook - expected failure", {
    codebook <- construct_codebook(c('A', 'B', 'C'))
    
    expect_error({
        result <- encode_signal(
            codebook=codebook,
            symbol = 'D',
            signal_power = 10,
            timesteps = 10
        )
    })
    
})

