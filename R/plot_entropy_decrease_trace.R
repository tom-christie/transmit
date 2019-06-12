#' Plots spikes and an entropy trace for a single transmitted signal.
#'
#' Optionally save the result to file.
#'
#' @param symbols vector of character symbols 
#' @param symbol_to_transmit Symbol that signal power should be allocated to
#' @param signal_power spikes per second for signal power
#' @param noise_power spikes per second for noise power
#' @param max_stop_time_in_seconds search until the stop_time is between max and min
#' @param min_stop_time_in_seconds search until the stop_time is between max and min
#' @param duration_in_seconds seconds to simulate for - should be highish
#' @param entropy_threshold stopping_threshold. Should be > log2(length(symbols))
#' @param file_path optional file path for saving the generated lot
#' 
#' @return list with three elements, (1) signal_plus_noise, (2) decoded signal with entropy trace, (3) generated plot
#' 
#' @importFrom forcats fct_rev
#' @importFrom purrr map_chr
#' @importFrom cowplot save_plot plot_grid
#' @importFrom assertthat assert_that
#' @import dplyr
#' @import ggplot2
#' 
#' @example
#' plot_entropy_decrease_trace(file_path = '~/test.png')
#' 
#' @export

plot_entropy_decrease_trace <- function(symbols = c('A', 'B', 'C', 'D'),
                                  symbol_to_transmit = 'A',
                                  signal_power=3,
                                  noise_power=3,
                                  max_stop_time_in_seconds = 5,
                                  min_stop_time_in_seconds = 3,
                                  duration_in_seconds = 50,
                                  entropy_threshold = 0.3,
                                  file_path = NA
                                  ){
    
    assert_that(symbol_to_transmit %in% symbols, msg='Your symbol_to_transmit is not in your symbols array.')
    codebook <- construct_codebook(symbols)
    
    
    
    #loop for a while until you find a signal that is decoded within your desired time range
    #i.e. so you don't end up with a perverse example not good for plotting
    while(TRUE){
        signal <- encode_signal(
            codebook = codebook,
            symbol = symbol_to_transmit,
            signal_power = signal_power,
            duration_in_seconds = duration_in_seconds
        )

        signal_plus_noise <- add_channel_noise(
            codebook = codebook,
            signal = signal,
            noise_power = noise_power,
            duration_in_seconds = duration_in_seconds
        )
        
        response <- decode_signal(
            codebook = codebook,
            signal_plus_noise = signal_plus_noise,
            signal_power = signal_power,
            noise_power = noise_power,
            prior_distribution = rep(1, length(symbols))/length(symbols),
            time_interval = 0.01,
            entropy_threshold = entropy_threshold,
            return_entropy_trace = TRUE
        )
        
        print(response$stop_time_in_seconds)
        
        #get a result that's reasonable
        if(response$stop_time_in_seconds < max_stop_time_in_seconds & response$stop_time_in_seconds > min_stop_time_in_seconds){
            decoding <- data.frame(
                entropy = response$entropy_trace,
                spike_time = seq(0,max(signal_plus_noise$spike_time), by=0.01)
            )
            break()
        }
    }

    p1 <- signal_plus_noise %>%
        mutate(neuron = map_chr(group_index,function(x){codebook[[x]]$symbol}),
               neuron = fct_rev(as.factor(neuron))) %>%
        filter(spike_time < max_stop_time_in_seconds) %>%
        ggplot() + 
        geom_point(aes(spike_time, neuron, color=type), shape='|', size=8) + 
        labs(x='Time (s)',y='') + 
        scale_color_manual(guide=FALSE,
                           values=c('black',#signal
                                    'darkgray')#noise
                           ) + 
        #scale_y_discrete(labels=symbols) + 
        scale_x_continuous()+ 
        theme(
            axis.text = element_text(size = rel(1.1)),
            axis.title = element_text(size = rel(1.1))
        )
    
    p2 <- decoding %>%
        filter(spike_time < max_stop_time_in_seconds) %>%
        ggplot() + 
        geom_line(aes(spike_time,entropy)) +
        geom_vline(aes(xintercept=response$stop_time_in_seconds)) + 
        annotate("text",x=response$stop_time_in_seconds-.5, y=max(response$entropy_trace)-.2, 
                 label='Decision\ntime') + 
        geom_hline(aes(yintercept=entropy_threshold)) + 
        annotate("text",x=0.8, y=response$entropy_threshold-0.1, label='Entropy threshold') + 
        labs(x='Time (s)',y='Entropy remaining\n(bits)') + 
        scale_x_continuous()+ 
        scale_y_continuous(limits=c(0, NA)) + 
        theme(
            axis.text = element_text(size = rel(1.1)),
            axis.title = element_text(size = rel(1.1)),
            #axis.text.y = element_text(size=rel(0.9))#,
            #axis.title.y = element_text(margin(t=1,r=1,b=1,l=1, unit="cm"))
        )
    p <- plot_grid(p1, p2, ncol=1, align='v', labels="AUTO")
    print(p)
    
    if(!is.na(file_path)){
        save_plot(file_path, p,  base_height=5, dpi=600)
    }
    
    return(
        list(
            signal_plus_noise = signal_plus_noise,
            decoding = decoding,
            plot = p
        )
    )
}
