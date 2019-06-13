require(tidyverse)
setwd('~/Dropbox/Apps/ShareLaTeX/CogSci 2019/code/')
source(file='transmit/R/transmit.R')

alphabet_length <- 4
symbols <- c('A','B','C','D')
codebook <- construct_codebook(symbols)
timesteps <- 1000

#Basic idea here is to pick a distribution, then send INDIVIDUAL messages using that distribution, and measure response times
distribution <- 2^(1:4)/sum(2^(1:4))


# Where uniform is highest entropy, and other distributions
require(entropy)

results_summary <- data.frame()
for(signal_power in c(1.5,2,3)){
  print(paste('working on signal power',signal_power))
  df <- data_frame()
  for(i in 1:1000){
    
    ix <- sample(c(1,2,3,4),1)
    current_symbol = symbols[ix]
    signal <- encode_signal(
      codebook = codebook,
      current_symbol = current_symbol,
      signal_power = signal_power,
      timesteps = timesteps
    )
    
    signal_plus_noise <- add_channel_noise(
      codebook = codebook,
      signal = signal,
      noise_power = noise_power,
      timesteps = timesteps
    )
    #table(signal_plus_noise$group_index)
    
    
    z <- decode_signal(
      codebook = codebook,
      signal_plus_noise = signal_plus_noise,
      signal_power = signal_power,
      noise_power = noise_power,
      prior = distribution,
      time_interval = 0.1,
      threshold = 'error_rate',
      threshold_value = 0.99,
      return_entropy_trace = FALSE
    )
    z$encoded_symbol <- current_symbol
    z$signal_power <- signal_power
    z$information <- -log2(distribution[ix])
    df <- df %>% bind_rows(as.data.frame(z))
  }
  results_summary <- results_summary %>%
    bind_rows(df)
}

results_summary %>%
  ggplot() + 
  geom_boxplot(aes(as.factor(information),stop_time, color=as.factor(signal_power)),size=2) + 
  geom_smooth(aes(as.factor(information),stop_time,group=as.factor(signal_power)), method='lm')


write_tsv(results_summary, '~/Dropbox/Apps/ShareLaTeX/CogSci 2019/code/data/hick_hyman_noise_10.tsv')
hyman %>%
  mutate(accurate = encoded_symbol == decoded_symbol) %>%
  count(signal_power,accurate)
p <- hyman %>% 
  filter(stop_time/100 < 8) %>%
  mutate(signal_power = as.character(signal_power)) %>%
  mutate(signal_power = ifelse(signal_power == '1.5', 'Low',
                               ifelse(signal_power == '2','Medium',
                                      ifelse(signal_power == '3','High', 'NA')))) %>%
  mutate(signal_power = factor(signal_power,levels=c("Low","Medium","High")))%>%
  #filter(signal_power != 'Medium') %>%
  ggplot() + 
  geom_histogram(aes(stop_time/100), bins=50) + 
   
  geom_line(data=data_frame(x=seq(1,7,by=0.1),
                            y=dnorm(seq(1, 7, by=0.1), mean=5.4, sd=.6),
                            signal_power=as.factor('Low')) %>%
              mutate(x=exp(x),y=y*65),
            aes(x/100,y), color='gray',size=2,alpha=0.9) + 
  
 geom_line(data=data_frame(x=seq(1,7,by=0.1),
                           y=dnorm(seq(1, 7, by=0.1), mean=4.8, sd=.6),
                           signal_power='Medium') %>%
             mutate(x=exp(x),y=y*120),
           aes(x/100,y), color='gray',size=2,alpha=0.9) +

geom_line(data=data_frame(x=seq(1,7,by=0.1),
                          y=dnorm(seq(1, 7, by=0.1), mean=4, sd=.6),
                          signal_power='High') %>%
            mutate(x=exp(x),y=y*245),
          aes(x/100,y), color='gray',size=2,alpha=0.9) + 
  facet_grid(signal_power ~ .) +
  scale_x_continuous(limits=c(0,8)) + 
  theme(axis.ticks.y = element_blank(),
        axis.text.y = element_blank()) + 
  labs(x='Decoding time (s)', y='Count', title='Decoding time follows\na lognormal distribution') + 
  theme(strip.text.y = element_text(size = 12))
show(p)
save_plot('../figures/response_times.png', p, base_aspect_ratio = 1.4)
