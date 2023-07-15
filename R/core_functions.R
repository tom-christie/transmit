
#' Encode signal into Poisson spikes
#'
#' The 'code' is just a Poisson spike train for a given group of neurons
#' This function will return a data frame with (group_index,spike_time) columns
#'
#' Some other stuff here.  
#'
#' @param codebook constructed using construct_codebook function
#' @param symbol symbol to be encoded
#' @param signal_power spikes per second allocated to signal process
#' @param duration_in_seconds number of time (in seconds) to simulate for -- limits max decoding time. Signals not decoded by this time will be interpreted as 'NA'
#' @param leak_symbols vector of symbols to which a 'leak' signal power should be added. Should not include the symbol specified by the 'symbol' parameter
#' @param leak_powers vector of signal powers (spikes/second), to assign to leak symbols. Must be same length as leak_symbols.
#' 
#' @return 
#' 
#' @importFrom poisson hpp.event.times
#' @importFrom assertthat assert_that
#' 
#' @example
#' 
#' 
#' 
#' @export

encode_signal <- function(codebook,
                          symbol,
                          signal_power,
                          duration_in_seconds,
                          leak_symbols = NA,
                          leak_powers = NA) {
    
    #assert_that(!(symbol %in% leak_symbols), 
    #            msg='Do not include your transmitted signal in the list of leak signals')
    assert_that(symbol %in% lapply(codebook,function(x){x$symbol}),
                msg='Symbol to be transmitted is not in the codebook provided.')
    assert_that(length(leak_symbols) == length(leak_powers),
                msg='leak_symbols and leak_powers must be the same length.')
    
    #generate more spikes than necessary, then cull down
    spikes <- hpp.event.times(rate = signal_power, num.events = duration_in_seconds*signal_power*2, num.sims = 1, t0 = 0)
    spikes <- spikes[which(spikes < duration_in_seconds)]
    
    #get symbol position
    z <- sapply(codebook, function(x){if(x$symbol == symbol){x$index}else{NA}})
    spikes_df <- data.frame(group_index=unname(z)[!is.na(z)], 
                            spike_time = spikes)
 
    #add leak spikes if appropriate
    if( (length(leak_symbols) > 1) || !is.na(leak_symbols)){
        #loop through leak_symbols and add then to data frame -- optimize later, but this loop probably won't be big.
        for(i in 1:length(leak_symbols)){
            spikes_leak <- hpp.event.times(rate = leak_powers[i], num.events = duration_in_seconds*leak_powers[i]*2, num.sims = 1, t0 = 0)
            spikes_leak <- spikes_leak[which(spikes_leak < duration_in_seconds)]
            z <- sapply(codebook, function(x){if(x$symbol == leak_symbols[i]){x$index}else{NA}})
            leak_df <- data.frame(group_index=unname(z)[!is.na(z)], spike_time = spikes_leak)
            spikes_df <- rbind(spikes_df,leak_df)
        }
    }
    
    return(spikes_df)
}



#' Add channel noise
#'
#' Takes a list of 'signal' spikes and adds noise spikes
#'
#' @param codebook constructed using construct_codebook function
#' @param signal data_frame output from transmit_signal, that has spikes allocated by signal_power
#' @param noise_power spikes per second to add to every Poisson process, i.e. for each symbol in the codebook
#' @param duration_in_seconds length of time to simulate, in seconds. needed to know how much noise to produce.
#' 
#' @importFrom poisson hpp.event.times
#' @importFrom data.table rbindlist
#' @importFrom assertthat assert_that
#' 
#' @return data_frame with (group_index,spike_times) columns, representing the signal with noise appended
#' 
#' @example
#' 
#' @export

add_channel_noise <- function(
    codebook, 
    signal,
    noise_power,
    duration_in_seconds
){
    assert_that(duration_in_seconds > max(signal$spike_time),
                msg='Your noise will be simulated for less time than your signal. Reconsider...and incrase duration_in_seconds.')
    
    signal$type='signal'
    noise_spikes <- lapply(codebook, function(x){
        #generate lots of Poisson events, then filter to the time range specified by duration_in_seconds
        spikes <- hpp.event.times(rate = noise_power*x$size, #multiply rate by number of neurons allocated
                                  num.events = duration_in_seconds*noise_power*2*x$size, 
                                  num.sims = 1, 
                                  t0 = 0)
        spikes <- spikes[which(spikes < duration_in_seconds)]
        return(data.frame(group_index = x$index,
                          spike_time = spikes,
                          type='noise'))
    })
    noise_spikes <- rbindlist(noise_spikes)

    return(rbind(signal, noise_spikes))
}



#' Construct a codebook object that maps symbols to neurons
#'
#' Some other stuff here.  
#'
#' @param symbols list of strings, representing symbols to be encoded
#' 
#' @return list of entries, where each entry details a symbol and the neuron/neural group that is allocated signal power when transmitted
#' 
#' @importFrom assertthat assert_that
#' 
#' @example
#' 
#' 
#' 
#' @export


construct_codebook <- function(symbols) {
    
    assert_that(length(symbols) > 0, 
                msg="You need a vector of characters with length > 0.")
    assert_that(all(is.character(symbols)),
                msg="You need a vector of characters with length > 0.")
    
    codebook <- lapply(1:length(symbols), function(i){list(index = i,
                                               size = 1, #placeholder for future functionality
                                               symbol = symbols[i])})
    names(codebook) <- symbols

    return(codebook)
}


#' Determine which symbol is being transmitted.
#'
#' Decoder takes a list of spike times for each Poisson process.  It knows that one of them is 
#' firing at a higher rate, and tries to determine which it is by comparing likelihoods of observations
#' 
#' @param codebook constructed using construct_codebook function
#' @param signal_plus_noise signal to be decoded, output by add_channel_noise
#' @param signal_power spikes per second allocated to signal process
#' @param noise_power spikes per second allocated to every process (including signal process)
#' @param time_interval_in_seconds the interval at which to perform inference
#' @param prior_distribution initial guess at the distribution of messages. Influences decoding times.
#' @param entropy_threshold transmission stops after entropy reaches this threshold
#' @param return_entropy_trace return posterior entropy for each time step
#' @param return_posterior_at_stop_time returns posterior values at stop time
#' @param return_posterior_trace return trace of posterior values for every time step

#' @return list of data frames. Will list decoded symbol, stop_time (in units of interval), entropy threshold, posterior at stop time, and potentially other time series as specified in the function call
#' 
#' @import Rcpp
#' @importFrom assertthat assert_that
#' @importFrom data.table rbindlist
#' @useDynLib transmit
#' 
#' 
#' @example
#' 
#' @export


decode_signal <- function(
    codebook,
    signal_plus_noise,
    signal_power,
    noise_power,
    prior_distribution = c(),
    time_interval_in_seconds = 0.1,
    entropy_threshold = 0.1,
    return_entropy_trace = FALSE,
    return_posterior_trace = FALSE,
    return_posterior_at_stop_time = FALSE
){
    if(length(prior_distribution) < 1){
        prior_distribution <- rep.int(1,times = length(codebook))/length(codebook)
    }
    prior_distribution <- prior_distribution/sum(prior_distribution)
    
    assert_that(length(codebook) == length(prior_distribution),
                msg='Your prior must be the same length as your codebook.')
    
    # for each neuron group (currently each group has one element), we know the (1) count of neurons (2) noise power (3) signal power
    # that means we have an expected noise rate and signal+noise rate for each group
    group_sizes <- rep(NA,length(codebook))
    group_indexes <- rep(NA,length(codebook))
    symbols <- rep(NA,length(codebook))
    noise_rates <- rep(NA,length(codebook)) #expected rates if noise
    signal_rates <- rep(NA,length(codebook)) #expected rates if signal (i.e. includes noise)
    for(i in codebook){
        group_sizes[i$index] <- i$size
        group_indexes[i$index] <- i$index
        symbols[i$index] <- i$symbol
        noise_rates[i$index] <- i$size*noise_power
        signal_rates[i$index] <- i$size*noise_power + signal_power 
    }
    
    # Want to calculate the posterior probability that the configuration represents a 'signal' for each group
    # this depends on the signal_rates, noise_rates, and observed counts at each timestep
    # Ultimately need to calculate the log likelihoods of observations given each symbol choice
    # Luckily these are the sums of the log likelihoods of each neuron group
    # We can store this information in two matrices
    # (1) a 'signal' matrix, of (timesteps x groups), 
    #     that stores likelihoods of each observation if that group were signal
    # (2) a 'noise' matrix, of (timesteps x groups),
    #     that stores likelihoods of each observation if that group were noise
    
    max_time <- max(signal_plus_noise$spike_time)
    timesteps <- seq(0, max_time, by=time_interval_in_seconds)
    
    #First need counts at each time interval
    #this is (timesteps x groups)
    #this is an Rcpp function
    counts <- count_spikes(signal_plus_noise$group_index,length(group_indexes), signal_plus_noise$spike_time, timesteps)
    
    #not necessary to get counts per neuron fwhile each group only has one element
    #counts_per_neuron <- counts / 
    #    matrix(rep(group_sizes, length(timesteps)), ncol=length(group_indexes), byrow=TRUE)
    
    ## Calculate log likelihood of each neuron's observation assuming a SIGNAL rate
    #Step 1- convert counts to rates
    poisson_rate_at_each_timestep <-  matrix(rep(signal_rates, length(timesteps)), 
                                             ncol=length(group_indexes), byrow=TRUE) * 
        matrix(rep(timesteps, length(group_indexes)), ncol=length(group_indexes), byrow=FALSE)
    
    #Step 2 - calculate log likelihood
    ## EXPENSIVE b/c it's a mapply in an sapply, but it's computing likelihoods in C so I'm not sure we can speed it up :-(
    ## We could parallelize it but usually this decode function is already called in parallel, so that wouldn't help.
    signal_log_likelihood <- sapply(group_indexes, function(index){
        log2(mapply(dpois, counts[,index], poisson_rate_at_each_timestep[,index]))
    })
    
    ## Calculate log likelihood of each neuron's observation assuming a NOISE rate
    # Step 1 - convert counts to rates
    poisson_rate_at_each_timestep <-  matrix(rep(noise_rates, length(timesteps)), 
                                             ncol=length(group_indexes), byrow=TRUE) * 
        matrix(rep(timesteps, length(group_indexes)), ncol=length(group_indexes), byrow=FALSE)
    # Step 2 - calculate log likelihood
    noise_log_likelihood <- sapply(group_indexes, function(index){
        log2(mapply(dpois, counts[,index], poisson_rate_at_each_timestep[,index]))
    })
    
    #For each possible signal being transmitted, use the noise and signal likelihoods to 
    #calculate the likelihood of that signal being the transmitted one given the observations
    #Since neurons are independent, this is equivalent to adding the log likelihoods
    #For each symbol, we add the signal_log_likelihood for the neuron corresponding to that symbol
    #plus the noise_log_likelihood for all other neurons
    #for each timestep
    symbol_log_likelihoods <- sapply(group_indexes, 
                                     function(index){
                                         rowSums(cbind(signal_log_likelihood[,index], noise_log_likelihood[,-index]))
                                     })
    
    
    ## Now incorporate prior information and normalize.  
    #add on priors -- this is one way in log-space, but below we do by multiplying and re-normalizing
    #symbol_log_likelihoods <- symbol_log_likelihoods + log2(matrix(rep(prior,length(timesteps)),byrow=TRUE, ncol=length(group_indexes)))
    
    ## Normalization first
    #exp-normalization trick https://timvieira.github.io/blog/post/2014/02/11/exp-normalize-trick/
    #normalize WHILE exponentiating using the exp trick
    #Subtract off the max from each, and then exponentiate. For some reason this works.
    row_maxes <- apply(symbol_log_likelihoods, 1, max)
    row_maxes_matrix <- matrix(rep(row_maxes, times=length(group_indexes)), ncol=length(group_indexes))
    normalized = 2^(symbol_log_likelihoods-row_maxes_matrix)
    normalized = normalized/rowSums(normalized)
    
    ## Now multiply by prior and re-normalize
    posterior = normalized*matrix(rep(prior_distribution,length(timesteps)),byrow=TRUE, ncol=length(group_indexes))
    posterior = posterior/rowSums(posterior)
    
    #Calculate entropy of posterior
    entropy = -rowSums(posterior * log2(posterior))
    
    ## Compute time of decision -- first timestep after you cross the entropy threshold
    ## Then decide what to return based on the arguments
    stop_time <- which(entropy < entropy_threshold)
    if(length(stop_time) > 0){
        #print(stop_time[1])
        stop_time <- stop_time[1]
        neuron_choice <- which(posterior[stop_time, ] == max(posterior[stop_time,]))
        if(length(neuron_choice) >= 1){
            neuron_choice <- neuron_choice[1]
            if(return_posterior_at_stop_time){
                posterior_at_stop_time = I(list(posterior[stop_time, ]))
            }else{
                posterior_at_stop_time = NA
            }
            response <- list(
                decoded_symbol=symbols[neuron_choice],
                stop_time=stop_time,
                stop_time_in_seconds = stop_time*time_interval_in_seconds,
                entropy_threshold = entropy_threshold,
                posterior_at_stop_time = posterior_at_stop_time
            )
            
        }else if(length(neuron_choice) == 0){ #why would this ever happen?
            neuron_choice <- -1
            response <- list(
                decoded_symbol="UNKNOWN",
                stop_time=NA,
                stop_time_in_seconds = NA,
                entropy_threshold = entropy_threshold,
                posterior_at_stop_time = NA
            )
        }
    }else{
        response <- list(
            decoded_symbol=NA,
            stop_time=NA,
            stop_time_in_seconds = NA,
            entropy_threshold = entropy_threshold,
            posterior_at_stop_time = NA
        )
    }
    
    if(return_entropy_trace){
        if(return_posterior_trace){
            response$entropy_trace = entropy
            response$posterior_trace = posterior
            
        }else{
            response$entropy_trace = entropy
        }
    }else{
        if(return_posterior_trace){
            response$posterior_trace = posterior
        }
    }
    return(response)
}



#' Encode the signal, add noise, and decode the signal
#'
#' Helper function to encapsulate function calls for a 'signal transmission' event
#'
#' @param codebook constructed using construct_codebook function
#' @param symbol symbol to transmit. Must be defined in the codebook.
#' @param signal_power spikes per second allocated to signal process
#' @param noise_power spikes per second allocated to every process (including signal process)
#' @param duration_in_seconds number of time (in seconds) to simulate for -- limits max decoding time. Signals not decoded by this time will be interpreted as 'NA'
#' @param time_interval the interval at which to perform inference
#' @param prior_distribution initial guess at the distribution of messages. Influences decoding times.
#' @param entropy_threshold transmission stops after entropy reaches this threshold
#' @param return_posterior_at_stop_time returns posterior values at stop time
#' @param return_posterior_trace return trace of posterior values for every time step
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
    duration_in_seconds,
    prior_distribution = c(),
    time_interval = 0.1,
    entropy_threshold = 0.1,
    return_posterior_at_stop_time = FALSE,
    return_posterior_trace = FALSE,
    leak_symbols = NA,
    leak_powers = NA
){
    signal <- encode_signal(
        codebook = codebook,
        symbol = symbol,
        signal_power = signal_power,
        duration_in_seconds = duration_in_seconds,
        leak_symbols = leak_symbols,
        leak_powers = leak_powers
    )
    signal_plus_noise <- add_channel_noise(
        codebook = codebook, 
        signal = signal,
        noise_power = noise_power,
        duration_in_seconds = duration_in_seconds
    )
    z <- decode_signal(
        codebook = codebook,
        signal_plus_noise = signal_plus_noise,
        signal_power = signal_power,
        noise_power = noise_power,
        prior_distribution = prior_distribution,
        time_interval = time_interval,
        entropy_threshold = entropy_threshold,
        return_posterior_at_stop_time = return_posterior_at_stop_time,
        return_posterior_trace = return_posterior_trace
    )
    
    z$sent_symbol <- symbol
    
    return(z)
    
}

