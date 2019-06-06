#' Determine which symbol is being transmitted.
#'
#' Decoder takes a list of spike times for each Poisson process.  It knows that one of them is 
#' firing at a higher rate, and tries to determine which it is by comparing likelihoods of observations
#' 
#' @param codebook constructed using construct_codebook function
#' @param signal_plus_noise signal to be decoded, output by add_channel_noise
#' @param signal_power spikes per second allocated to signal process
#' @param noise_power spikes per second allocated to every process (including signal process)
#' @param time_interval the interval at which to perform inference
#' @param prior_distribution initial guess at the distribution of messages. Influences decoding times.
#' @param entropy_threshold transmission stops after entropy reaches this threshold
#' @param return_entropy_trace return posterior entropy for each time step
#' @param return_posterior_at_stop_time returns posterior values at stop time
#' @param return_posterior_trace return trace of posterior values for every time step

#' @return list of data frames. Will list decoded symbol, stop_time (in units of interval), entropy threshold, posterior at stop time, and potentially other time series as specified in the function call
#' 
#' @import Rcpp
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
    time_interval = 0.1,
    entropy_threshold = 0.1,
    return_entropy_trace = FALSE,
    return_posterior_trace = FALSE,
    return_posterior_at_stop_time = FALSE
){
    #require(data.table)

    #this function tries to decide which neuron group is firing fastest, 
    #i.e. in which group each neuron has the 'signal' firing rate
    #the function is 'omnicient' in that it knows the codebook and the signal and noise powers
    
    if(length(prior_distribution) < 1){
        prior_distribution <- rep.int(1,times = length(codebook))/length(codebook)
    }
    prior_distribution <- prior_distribution/sum(prior_distribution)
    
    
    # for each neuron group, we know the (1) count of neurons (2) noise power (3) signal power
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
        #size doesn't multiply by signal_power here...
        # since we want signal power to represent total extra energy into the system
    }
    
    # ## Construct threshold in terms of time or entropy
    # if(threshold == 'error_rate'){
    #     n <- length(group_indexes)
    #     entropy_threshold <- (n-1)*((1-threshold_value)/(n-1))*log2((1-threshold_value)/(n-1)) + threshold_value*log2(threshold_value)
    #     entropy_threshold <- -1*entropy_threshold
    #     threshold <- 'entropy'
    # }else if(threshold == 'entropy'){
    #     entropy_threshold = threshold_value
    # }
    
    
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
    timesteps <- seq(0, max_time, by=time_interval)
    
    #First need counts at each time interval
    #this is (timesteps x groups)
    
    counts <- count_spikes(signal_plus_noise$group_index,length(group_indexes), signal_plus_noise$spike_time, timesteps)
    
    
    # counts <- sapply(group_indexes, 
    #        function(index){
    #            sapply(timesteps, function(x){
    #                sum(signal_plus_noise[signal_plus_noise$group_index == index,]$spike_time <= x)
    #         })
    #        })
    
    counts_per_neuron <- counts / 
        matrix(rep(group_sizes, length(timesteps)), ncol=length(group_indexes), byrow=TRUE)
    
    poisson_rate_at_each_timestep <-  matrix(rep(signal_rates, length(timesteps)), 
                                             ncol=length(group_indexes), byrow=TRUE) * 
        matrix(rep(timesteps, length(group_indexes)), ncol=length(group_indexes), byrow=FALSE)
    ## EXPENSIVE b/c it's a mapply in an sapply, but it's computing likelihoods so I'm not sure we can speed it up :-(
    signal_log_likelihood <- sapply(group_indexes, function(index){
        log2(mapply(dpois, counts[,index], poisson_rate_at_each_timestep[,index]))
    })
    
    poisson_rate_at_each_timestep <-  matrix(rep(noise_rates, length(timesteps)), 
                                             ncol=length(group_indexes), byrow=TRUE) * 
        matrix(rep(timesteps, length(group_indexes)), ncol=length(group_indexes), byrow=FALSE)
    
    noise_log_likelihood <- sapply(group_indexes, function(index){
        log2(mapply(dpois, counts[,index], poisson_rate_at_each_timestep[,index]))
    })
    
    #I don't have to do any of that cumulative-sum stuff here since we're not looking at counts
    #Just need to calculate the log likelihood for each possible condition
    #Which is the sum of the log likelihoods for each observation, since they're independent
    #i.e. a signal in group 1 would be the first column of the 'signal' matrix plus the remainder of columns
    #from the 'noise' matrix
    symbol_log_likelihoods <- sapply(group_indexes, 
                                     function(index){
                                         rowSums(cbind(signal_log_likelihood[,index], noise_log_likelihood[,-index]))
                                     })
    
    #add on priors
    #TODO - should these have an explicit weight or strength other than additive???
    #symbol_log_likelihoods <- symbol_log_likelihoods + log2(matrix(rep(prior,length(timesteps)),byrow=TRUE, ncol=length(group_indexes)))
    
    #exp-normalization trick https://timvieira.github.io/blog/post/2014/02/11/exp-normalize-trick/
    #normalize WHILE exponentiating using the exp trick
    #Subtract off the max from each, and then exponentiate. For some reason this works.
    row_maxes <- apply(symbol_log_likelihoods, 1, max)
    row_maxes_matrix <- matrix(rep(row_maxes, times=length(group_indexes)), ncol=length(group_indexes))
    normalized = 2^(symbol_log_likelihoods-row_maxes_matrix)
    normalized = normalized/rowSums(normalized)
    #multiply by prior and re-normalize
    #prior <- c(0.05, 0.9, 0.05)
    posterior = normalized*matrix(rep(prior_distribution,length(timesteps)),byrow=TRUE, ncol=length(group_indexes))
    posterior = posterior/rowSums(posterior)
    entropy = -rowSums(posterior * log2(posterior))
    
    ## Compute time of decision
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
                stop_time_in_seconds = stop_time*time_interval,
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
