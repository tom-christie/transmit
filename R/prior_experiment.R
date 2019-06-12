#' Run a bunch of simulations with a given 
#'
#' Some other stuff here.  
#'
#' @param codebook constructed using construct_codebook function
#' @param source_distribution source distribution of messages. Messages are generated in simulations according to this distribution.
#' @param prior_distribution initial guess at the distribution of messages. Influences decoding times.
#' @param entropy_threshold transmission stops after entropy reaches this threshold
#' @param signal_power spikes per second allocated to signal process
#' @param noise_power spikes per second allocated to every process (including signal process)
#' @param duration_in_seconds number of time (in seconds) to simulate for -- limits max decoding time. Signals not decoded by this time will be interpreted as 'NA'
#' @param time_interval time resolution for simulations (like dt)
#' @param repeats number of times to run the simulation
#' 
#' @return data frame containing encoded and decoded symbols 
#' 
#' @importFrom data.table rbindlist
#' @importFrom parallel mcmapply detectCores
#' 
#' @example
#' 
#' 
#' 
#' @export
#' 

prior_experiment <- function(
    codebook,
    source_distribution, 
    prior_distribution,
    threshold_type,
    threshold_value,
    signal_power,
    noise_power,
    duration_in_seconds,
    time_interval,
    repeats
){
    # Generate symbols to send according to the source_distribution
    symbols <- unname(unlist(lapply(codebook,function(x) x$symbol)))
    z <- rmultinom(1, repeats, source_distribution)[,]
    symbol_sequence <- unlist(sapply(1:length(z), function(i){rep(symbols[i], times=z[i])}))
    symbol_sequence <- sample(symbol_sequence, length(symbol_sequence), replace=FALSE) #randomize order, not that it really matters
    
    results.temp <- mcmapply(transmit_signal,
                             symbol = symbol_sequence,
                             signal_power = signal_power,
                             noise_power = noise_power,
                             duration_in_seconds = duration_in_seconds,
                             time_interval = time_interval,
                             entropy_threshold = entropy_threshold,
                             MoreArgs = list(codebook=codebook,
                                             prior_distribution=prior_distribution),
                             mc.cores=detectCores()-2,
                             SIMPLIFY=FALSE)
    results <- rbindlist(results.temp)
    return(results)
}