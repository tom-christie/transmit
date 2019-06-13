setwd('~/git/dissertation/thesis/Chapter 05 - The Power Law of Learning/src/')
source(file='../../Chapter 04 - A variable-length code/src/transmit/R/transmit.R')
require(tidyverse)
require(entropy)
require(cowplot)


hyman_transmit <- function(
  codebook,
  signal_power,
  noise_power,
  timesteps,
  time_interval,
  true_distribution,
  prior,
  threshold_value = 0.3,
  repeats=1
){
  
  symbols <- rep(NA,length(codebook))
  for(i in codebook){
    symbols[i$index] <- i$symbol
  }
  current_symbol = sample(symbols, size=1, prob = true_distribution)
  
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
  
  z <- decode_signal(
    codebook = codebook,
    signal_plus_noise = signal_plus_noise,
    signal_power = signal_power,
    noise_power = noise_power,
    prior = prior,
    time_interval = time_interval,
    threshold = 'entropy',
    threshold_value = threshold_value,
    return_entropy_trace = FALSE,
    return_posteriors = FALSE
  )
  posterior <- unlist(z$posterior_at_stop_time)
  
  #z$current_symbol_frequency <- true_distribution[ix]
  z$threshold_value <- threshold_value
  z$encoded_symbol <- current_symbol
  z$signal_power <- signal_power
  z$information <- get_kl(posterior, prior)#
  z$information2 <- get_kl(prior, posterior)#
  return(z)
}




get_p_size <- function(size,n_full){
  epsilon = 0.01
  p <- c(rep(epsilon,(size-n_full)), rep(1-epsilon*(size-n_full)/n_full,n_full))
  p <- p/sum(p)
  q <- rep(1,size)/sum(rep(1,size))
  return(list(
    p=p,
    q=q,
    kl = get_kl(p,q)
  ))
}

alphabet_length <- 16
symbols <- paste0('A',1:alphabet_length)
codebook <- construct_codebook(symbols)

args <- expand.grid(
  repeats=1:2000,
  noise_power=10,
  signal_power=4,
  threshold_value=0.3,
  time_interval=0.1,
  timesteps=50
)

results <- data_frame()
for(n_full in c(2)){
  print('n',n_full)
  z <- get_p_size(alphabet_length, 2)
  for(strength in 2^(1:10)){
    print(paste('strength',strength))
    prior<- rowSums(rmultinom(1000, strength, z$p)) + 2000 #the 2000 is to mimic a weak uniform dirichlet prior of 2 observations per category
    prior <- prior/sum(prior)
    
    results.temp <- mcmapply(hyman_transmit,
                             noise_power=args$noise_power,
                             signal_power=args$signal_power,
                             threshold_value=args$threshold_value,
                             time_interval=args$time_interval,
                             timesteps=args$timesteps,
                             MoreArgs = list(codebook=codebook,
                                             prior=prior,
                                             true_distribution=z$p),
                             mc.cores=8,
                             SIMPLIFY=FALSE)
    
    results.temp = rbindlist(results.temp)
    
    results.temp <- results.temp %>%
      mutate(stop_time = stop_time*0.1) %>%
      filter(encoded_symbol == decoded_symbol) %>%
      summarise(upper = quantile(stop_time,.9),
                lower=quantile(stop_time,.1),
                stop_time_sd = sd(stop_time),
                stop_time=mean(stop_time,na.rm=T),
                se = stop_time_sd/sqrt(n())
      ) %>%
      mutate(kl=get_kl(z$p,prior),
             n_full=n_full,
             strength=strength)
    
    results <- results %>%
      bind_rows(results.temp)
    
    
  }
}
write_tsv(results, 'data/power_law_with_poisson_stuff.tsv')
results <- read_tsv('~/Dropbox/Apps/ShareLaTeX/CogSci 2019 (Camera Ready)/code/data/power_law_with_poisson_stuff.tsv')
p <- results %>%
  ggplot() + 
  geom_smooth(aes(x=strength,y=stop_time),color='lightgray', method='lm', se = FALSE) + 
  geom_ribbon(aes(x=strength,ymin=lower, ymax=upper), width=0, alpha=0.1) + 
  geom_point(aes(strength, stop_time), size=2) + 
  scale_x_log10(breaks=c(1, 10, 100, 1000), labels=c(1, 10, 100, 1000),limits=c(1, 1024)) + 
  scale_y_log10(breaks=c(2.5, 5, 7.5, 10)) + 
  labs(x='Number of observations (log scale)', y='Transmission time (s, log scale)', title='Transmission times follow\nthe Power Law of Learning') 
show(p)  


save_plot('~/Dropbox/Apps/ShareLaTeX/CogSci 2019 (Camera Ready)/figures/power_law_with_poisson_ribbon.png',
          p, dpi=450,
          base_height=5,base_aspect_ratio = 1.5)
