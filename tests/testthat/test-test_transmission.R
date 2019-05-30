test_that("example transmission", {
    codebook <- construct_codebook(c('A', 'B', 'C'), c(1, 1, 1))
    result <- transmit_signal(
        codebook=codebook,
        symbol = 'A',
        signal_power = 3,
        noise_power = 10,
        timesteps = 10,
        time_interval = 0.1,
        entropy_threshold = 0.1
        #repeats=1:1000
    )
    
})
