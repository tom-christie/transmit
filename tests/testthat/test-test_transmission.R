test_that("example transmission", {
    codebook <- construct_codebook(c('A', 'B', 'C'))
    decoded_signal <- transmit_signal(
        codebook=codebook,
        symbol = 'A',
        signal_power = 10,
        noise_power = 10,
        duration_in_seconds = 10,
        time_interval = 0.1,
        entropy_threshold = 0.1,
        return_posterior_at_stop_time = TRUE
        #repeats=1:1000
    )
    
    expect_true('decoded_symbol' %in% names(decoded_signal))
    expect_true('stop_time' %in% names(decoded_signal))
    expect_true('entropy_threshold' %in% names(decoded_signal))
    expect_true('posterior_at_stop_time' %in% names(decoded_signal))
    expect_false('entropy_trace' %in% names(decoded_signal))
    expect_false('posterior_trace' %in% names(decoded_signal))
    expect_equal(length(decoded_signal[['posterior_at_stop_time']][[1]]), 3)
})
