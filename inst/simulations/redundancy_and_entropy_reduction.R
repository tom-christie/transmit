setwd("~/Dropbox/Apps/ShareLaTeX/CogSci 2019/code/")

require(tidyverse)
require(cowplot)
source('transmit/R/transmit.R')


codebook <- construct_codebook(c('A', 'B', 'C', 'D'), c(1, 1, 1, 1))

noise_power = 10
timesteps=50


traces <- list()
symbols <- data_frame()
results <- data_frame()
for(signal_power in c(1.5,2,3)){
  print(paste('working on signal power',signal_power))
  for(i in 1:2000){
    
    signal <- encode_signal(
      codebook = codebook,
      current_symbol = 'A',
      signal_power = signal_power,
      timesteps = timesteps
    )
    
    signal_plus_noise <- add_channel_noise(
      codebook = codebook,
      signal = signal,
      noise_power = noise_power,
      timesteps = timesteps
    )
    table(signal_plus_noise$group_index)
    
    
    z <- decode_signal(
      codebook = codebook,
      signal_plus_noise = signal_plus_noise,
      signal_power = signal_power,
      noise_power = noise_power,
      prior = c(0.333, 0.333, 0.333, 0.333),
      time_interval = 0.01,
      threshold = 'error_rate',
      threshold_value = 0.95,
      return_entropy_trace = TRUE
    )
    traces[[i]] <- z$entropy_trace[1:6000]
  }
  
  z <- do.call('rbind',traces)
  
  df <- as.data.frame(t(z[,])) %>%
    gather('neuron','entropy') %>%
    group_by(neuron) %>%
    mutate(t=1:n()) %>%
    ungroup() %>%
    separate(neuron, c('V','neuron'), 'V') %>%
    mutate(neuron = as.numeric(neuron),
           signal_power = signal_power)
  results <- results %>%
    bind_rows(df)
  
}

d <- results %>%
  filter(t < 4000) %>%
  group_by(t, signal_power) %>%
  summarise(entropy = mean(entropy, na.rm=TRUE)) %>%
  ungroup()
#todo - cut out outliers before calculating mean?
p <- d %>%
    ggplot() + 
    geom_line(aes(t/1000,entropy, color=as.factor(signal_power)), size=2) + 
    labs(title='Expected information gain over time', x='Time (s)', y='Remaining entropy (bits)') + 
    scale_colour_grey('Signal power',labels=c('Low','Medium','High')) + 
    theme(legend.position = c(0.7, 0.6))

show(p)
save_plot('../figures/redundancy_example.png', p, base_aspect_ratio = 1.5)



## Also plot entropy vs error rates, since that's not totally obvious. Maybe do it

