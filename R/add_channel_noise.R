#' Add channel noise
#'
#' Takes a list of 'signal' spikes and adds noise spikes
#'
#' @param codebook constructed using construct_codebook function
#' @param signal data_frame output from transmit_signal, that has spikes allocated by signal_power
#' @param noise_power spikes per second to add to every Poisson process, i.e. for each symbol in the codebook
#' @param timesteps length of time to simulate. needed to know how much noise to produce.
#' 
#' @return data frame with noise appended
#' 
#' @example
#' 
#' @export

require(poisson)

add_channel_noise <- function(
    codebook, 
    signal,
    noise_power,
    timesteps
){
    #codebook - codebook from the 'construct_codebook' function
    #signal - signal data.frame passed in from 'encode_as_spikes' function
    #noise_power - Poisson rate applied to each single neuron
    #timesteps - how long to simulate for
    #output is a data_frame with (group_index,spike_times) columns
    
    assert_that(timesteps > max(signal$spike_time))
    signal$type='signal'
    noise_spikes <- lapply(codebook, function(x){
        spikes <- hpp.event.times(rate = noise_power*x$size, #multiply rate by number of neurons allocated
                                  num.events = timesteps*noise_power*2*x$size, 
                                  num.sims = 1, 
                                  t0 = 0)
        spikes <- spikes[which(spikes < timesteps)]
        return(data.frame(group_index = x$index,
                          spike_time = spikes,
                          type='noise'))
    })
    noise_spikes <- rbindlist(noise_spikes)
    #noise_spikes <- do.call('rbind',noise_spikes)
    
    return(rbind(signal, noise_spikes))
}