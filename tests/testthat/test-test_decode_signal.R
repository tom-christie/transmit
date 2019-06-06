test_that("decoding a signal", {
    codebook <- construct_codebook(c('A', 'B', 'C'), c(1, 1, 1))
    
    signal <- encode_signal(
        codebook=codebook,
        symbol = 'A',
        signal_power = 3,
        timesteps = 10
    )

    signal_plus_noise <- add_channel_noise(
        codebook = codebook,
        signal = signal,
        noise_power = 10,
        timesteps=10
    )
    
    decoded_signal <- decode_signal(
        codebook,
        signal_plus_noise = signal_plus_noise,
        signal_power = 3,
        noise_power = 10,
        time_interval = 0.1,
        entropy_threshold = 0.1,
        return_entropy_trace = FALSE,
        return_posterior_trace = FALSE
    )
    
    
    expect_true('decoded_symbol' %in% names(decoded_signal))
    expect_true('stop_time' %in% names(decoded_signal))
    expect_true('entropy_threshold' %in% names(decoded_signal))
    expect_true('posterior_at_stop_time' %in% names(decoded_signal))
    expect_false('entropy_trace' %in% names(decoded_signal))
    expect_false('posterior_trace' %in% names(decoded_signal))
    expect_equal(length(decoded_signal[['posterior_at_stop_time']]), 1)
    
    decoded_signal <- decode_signal(
        codebook,
        signal_plus_noise = signal_plus_noise,
        signal_power = 3,
        noise_power = 10,
        time_interval = 0.1,
        entropy_threshold = 0.1,
        return_entropy_trace = FALSE,
        return_posterior_trace = FALSE,
        return_posterior_at_stop_time = TRUE
    )
    expect_equal(length(decoded_signal[['posterior_at_stop_time']][[1]]), 3)
})


test_that("decoded symbol has an entropy trace", {
    codebook <- construct_codebook(c('A', 'B', 'C'), c(1, 1, 1))
    
    signal <- encode_signal(
        codebook=codebook,
        symbol = 'A',
        signal_power = 3,
        timesteps = 10
    )
    
    signal_plus_noise <- add_channel_noise(
        codebook = codebook,
        signal = signal,
        noise_power = 10,
        timesteps=10
    )
    
    decoded_signal <- decode_signal(
        codebook,
        signal_plus_noise = signal_plus_noise,
        signal_power = 3,
        noise_power = 10,
        time_interval = 0.1,
        entropy_threshold = 0.1,
        return_entropy_trace = TRUE,
        return_posterior_trace = FALSE
    )
    
    
    expect_true('decoded_symbol' %in% names(decoded_signal))
    expect_true('stop_time' %in% names(decoded_signal))
    expect_true('entropy_threshold' %in% names(decoded_signal))
    expect_true('entropy_trace' %in% names(decoded_signal))
    expect_false('posterior_trace' %in% names(decoded_signal))
    expect_gt(length(decoded_signal$entropy_trace), 1)
    
})


test_that("decoded symbol has a posterior trace", {
    codebook <- construct_codebook(c('A', 'B', 'C'), c(1, 1, 1))
    
    signal <- encode_signal(
        codebook=codebook,
        symbol = 'A',
        signal_power = 3,
        timesteps = 10
    )
    
    signal_plus_noise <- add_channel_noise(
        codebook = codebook,
        signal = signal,
        noise_power = 10,
        timesteps=10
    )
    
    decoded_signal <- decode_signal(
        codebook,
        signal_plus_noise = signal_plus_noise,
        signal_power = 3,
        noise_power = 10,
        time_interval = 0.1,
        entropy_threshold = 0.1,
        return_entropy_trace = FALSE,
        return_posterior_trace = TRUE
    )
    
    expect_true('decoded_symbol' %in% names(decoded_signal))
    expect_true('stop_time' %in% names(decoded_signal))
    expect_true('entropy_threshold' %in% names(decoded_signal))
    expect_false('entropy_trace' %in% names(decoded_signal))
    expect_true('posterior_trace' %in% names(decoded_signal))
    expect_gt(nrow(decoded_signal$posterior_trace), 1)
    expect_equal(ncol(decoded_signal$posterior_trace), 3)
    
})

