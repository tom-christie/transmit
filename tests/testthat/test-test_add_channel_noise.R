test_that("testing adding channel noise", {
    
    codebook <- construct_codebook(c('A', 'B', 'C'), c(1, 1, 1))
    
    signal <- encode_signal(
        codebook=codebook,
        symbol = 'A',
        signal_power = 3,
        timesteps = 10
    )
    
    result <- add_channel_noise(
        codebook,
        signal = signal,
        noise_power = 10,
        timesteps=10
    )
    
    expect_true('group_index' %in% colnames(result))
    expect_true('spike_time' %in% colnames(result))
    expect_true('type' %in% colnames(result))
    expect_true(nrow(result) > 1)
    
})
