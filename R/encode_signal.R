#' Encode signal into Poisson spikes
#'
#' Converts a symbol to a list of spike times
#'
#' Some other stuff here.  
#'
#' @param codebook constructed using construct_codebook function
#' @param current_symbol symbol to be encoded
#' @param signal_power spikes per second allocated to signal process
#' @param timesteps number of time (in seconds) to simulate for -- limits max decoding time. Signals not decoded by this time will be interpreted as 'NA'
#' @param leak_symbols 
#' @param leak_powers
#' 
#' @return 
#' 
#' 
#' @example
#' 
#' 
#' 
#' @export


encode_signal <- function(codebook,
                          current_symbol,
                          signal_power,
                          timesteps,
                          leak_symbols = NA,
                          leak_powers = NA) {
    
    # signal_power=1
    # codebook = construct_codebook(c('a','b','c'))
    # timesteps = 100
    # current_symbol = 'a'
    # leak_symbols = 'c'
    # leak_powers = .1
    ## codebook - codebook from 'construct_codebook' function
    ## current_symbol - symbol from codebook to transmit
    ## signal_power - signal power in terms of Poisson firing rate for a group of neurons
    ## timesteps - number of timesteps to simulate for, in same units as signal_power rate
    
    ## The 'code' is just a Poisson spike train for a given group of neurons
    ## This function will return a data frame with (group_index,spike_time) columns
    
    #generate more spikes than necessary, then cull down
    spikes <- hpp.event.times(rate = signal_power, num.events = timesteps*signal_power*2, num.sims = 1, t0 = 0)
    spikes <- spikes[which(spikes < timesteps)]
    
    #get symbol position
    z <- sapply(codebook, function(x){if(x$symbol == current_symbol){x$index}else{NA}})
    spikes_df <- data.frame(group_index=unname(z)[!is.na(z)], spike_time = spikes)
    #add leak spikes if appropriate
    if( (length(leak_symbols) > 1) || !is.na(leak_symbols)){
        #loop through leak_symbols and add then to data frame -- optimize later, but this loop probably won't be big.
        for(i in 1:length(leak_symbols)){
            spikes_leak <- hpp.event.times(rate = leak_powers[i], num.events = timesteps*leak_powers[i]*2, num.sims = 1, t0 = 0)
            spikes_leak <- spikes_leak[which(spikes_leak < timesteps)]
            z <- sapply(codebook, function(x){if(x$symbol == leak_symbols[i]){x$index}else{NA}})
            leak_df <- data.frame(group_index=unname(z)[!is.na(z)], spike_time = spikes_leak)
            spikes_df <- rbind(spikes_df,leak_df)
            
        }
    }
    
    return(spikes_df)
}