#' Encode the signal, add noise, and decode the signal
#'
#' Helper function to encapsulate function calls for a 'signal transmission' event
#'
#' @param codebook constructed using construct_codebook function
#' @param symbol symbol to transmit. Must be defined in the codebook.
#' @param signal_power spikes per second allocated to signal process
#' @param noise_power spikes per second allocated to every process (including signal process)
#' @param timesteps number of time (in seconds) to simulate for -- limits max decoding time. Signals not decoded by this time will be interpreted as 'NA'
#' @param prior_distribution initial guess at the distribution of messages. Influences decoding times.
#' @param entropy_threshold transmission stops after entropy reaches this threshold
#' @param leak_symbols array of symbols 'accidentally' allocated signal power
#' @param leak_powers array of signal powers to allocated to leak_symbols
#' 
#' @return returns which symbol is decided plus response time and which symbol was sent
#' 
#' 
#' @example
#' 
#' 
#' 
#' @export


transmit_signal <- function(
    codebook,
    symbol,
    signal_power,
    noise_power,
    timesteps,
    prior_distribution = c(),
    time_interval = 0.1,
    entropy_threshold = 0.1,
    return_posteriors = FALSE,
    leak_symbols = NA,
    leak_powers = NA
){
    signal <- encode_signal(
        codebook = codebook,
        symbol = symbol,
        signal_power = signal_power,
        timesteps = timesteps,
        leak_symbols = leak_symbols,
        leak_powers = leak_powers
    )
    signal_plus_noise <- add_channel_noise(
        codebook = codebook, 
        signal = signal,
        noise_power = noise_power,
        timesteps = timesteps
    )
    z <- decode_signal(
        codebook = codebook,
        signal_plus_noise = signal_plus_noise,
        signal_power = signal_power,
        noise_power = noise_power,
        prior_distribution = prior_distribution,
        time_interval = time_interval,
        entropy_threshold = entropy_threshold,
        return_posteriors = return_posteriors
    )
    
    z$sent_symbol <- symbol
    
    return(z)

    }
