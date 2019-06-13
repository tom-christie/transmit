require(tidyverse)
setwd('~/Dropbox/Apps/ShareLaTeX/CogSci 2019/code/')
source(file='transmit/R/transmit.R')

prior_experiment_entropy <- function(
  codebook,
  actual_proportion,
  prior_proportion,
  expected_entropy_threshold,
  signal_power,
  noise_power,
  timesteps,
  time_interval,
  repeats
){
  
  # Go through the codebook and send messages at the actual_proportion
  symbols <- rep(NA,length(codebook))
  for(i in codebook){
    symbols[i$index] <- i$symbol
  }
  z <- rmultinom(1, repeats, actual_proportion)[,]
  symbol_sequence <- unlist(sapply(1:length(z), function(i){rep(symbols[i], times=z[i])}))
  symbol_sequence <- sample(symbol_sequence, length(symbol_sequence), replace=FALSE) #randomize order, not that it really matters
  profiling = FALSE
  if(profiling){
    results.temp <- mapply(transmit_signal,
                           symbol = symbol_sequence,
                           signal_power = signal_power,
                           noise_power = noise_power,
                           timesteps = timesteps,
                           time_interval = time_interval,
                           threshold = 'entropy',
                           threshold_value = expected_entropy_threshold,
                           MoreArgs = list(codebook=codebook,
                                           prior=prior_proportion))
    return(data_frame(
      sent_symbol='a',
      decoded_symbol='b',
      stop_time = 2))
  }else{
    results.temp <- mcmapply(transmit_signal,
                             symbol = symbol_sequence,
                             signal_power = signal_power,
                             noise_power = noise_power,
                             timesteps = timesteps,
                             time_interval = time_interval,
                             threshold = 'entropy',
                             threshold_value = expected_entropy_threshold,
                             MoreArgs = list(codebook=codebook,
                                             prior=prior_proportion),
                             mc.cores=8,
                             SIMPLIFY=FALSE)
    results <- rbindlist(results.temp)
    results.temp[[4]]
    return(results)
  }
}


# Want to vary prior_proportion so that the KL divergence changes...
# For now we can just pick random numbers and scale.
alphabet_length <- 4
codebook <- construct_codebook(paste('A',seq(1,alphabet_length)))

actual_proportion <- runif(alphabet_length)
actual_proportion <- actual_proportion/sum(actual_proportion)
require(entropy)
results_summary <- data.frame()
#require(parallel)
for(kl in seq(5.00, 0.25, by=-0.5)){  #gives us a range of efficiencies, want enough to convince ourselves of linearity
    print(paste('kl iteration',kl))

        tries <- 0
        
        while(TRUE){
            tries = tries + 1
            prior_proportion <- runif(alphabet_length)
            prior_proportion <- prior_proportion/sum(prior_proportion)
            
            
            if(tries > 500000) 
            {
              prior_proportion <- prior_proportion * (rbinom(alphabet_length,1, runif(1))+.0001)
              prior_proportion <- prior_proportion/sum(prior_proportion)
            }
            
            measured_kl <- get_kl(actual_proportion, prior_proportion)
            
            if(abs(measured_kl - kl) < 0.01){
              print(paste('found after',tries,'tries'))
              break
            } 
          }
            
            #print(paste('prior_proportion',prior_proportion, tries))
            repeats = 5000
            results <- prior_experiment_entropy(
                codebook,
                actual_proportion = actual_proportion,
                prior_proportion = prior_proportion,
                expected_entropy_threshold = 0.3,
                signal_power = 4,
                noise_power = 10,
                timesteps = 50,
                time_interval = .1,
                repeats=repeats
            )
            number_correct <- results %>%
                count(sent_symbol,decoded_symbol) %>%
                filter(sent_symbol==decoded_symbol) %>%
                pull(n) %>%
                sum()
            mutual_info <- mi.empirical(table(results$sent_symbol, results$decoded_symbol))
            accuracy <- number_correct/repeats #TODO: parameterize this
            print(paste('actual accuracy',accuracy))
            avg_time <- mean(results$stop_time, na.rm=T)
            avg_time_q1 <- quantile(results$stop_time, 0.05, na.rm=T)
            avg_time_q2 <- quantile(results$stop_time, 0.95, na.rm=T)
            proportion_finished <- 1-sum(is.na(results$stop_time))/repeats
            print(paste('proportion finished',proportion_finished))
            results_summary <- results_summary %>%
                bind_rows(
                    data_frame(kl = get_kl(actual_proportion, prior_proportion),
                               accuracy = accuracy,
                               avg_time = avg_time,
                               avg_time_q1 = avg_time_q1,
                               avg_time_q2 = avg_time_q2,
                               mi = mutual_info,
                               proportion_finished = proportion_finished
                    )
                )

}
# 
# write_tsv(results_summary, '~/Dropbox/Apps/ShareLaTeX/CogSci 2019/code/data/kl_signal_03_noise_10.tsv')
# require(cowplot)
# 
# hyman <- read_tsv('~/Dropbox/Apps/ShareLaTeX/CogSci 2019/code/data/hick_hyman_noise_10.tsv')
# kl_data <- read_tsv('~/Dropbox/Apps/ShareLaTeX/CogSci 2019/code/data/kl_signal_03_noise_10.tsv')

results_summary %>%
  mutate(avg_time = avg_time*.1) %>%
  ggplot() + 
  geom_point(aes(kl,avg_time)) + 
  geom_smooth(aes(kl,avg_time),method='lm', se=FALSE, color='darkgray') + 
  scale_shape_discrete('Decoding Accuracy %') + 
  labs(title='Transmission time as a \nfunction of KL(P||Q)',x='KL Divergence (bits)', y="Transmission time (s)") + 
  theme_cowplot() + 
  theme(legend.position="bottom", legend.justification="center") 


require(cowplot)
kl_plot <- kl_data %>%
  mutate(avg_time = avg_time*0.5) %>%
  ggplot() + 
  geom_point(aes(kl,avg_time)) + 
  geom_smooth(aes(kl,avg_time),method='lm', se=FALSE, color='darkgray') + 
  scale_shape_discrete('Decoding Accuracy %') + 
  labs(title='Transmission time as a \nfunction of KL(P||Q)',x='KL Divergence (bits)', y="Transmission time (s)") + 
  theme_cowplot() + 
  theme(legend.position="bottom", legend.justification="center") 
show(kl_plot)
save_plot('../figures/kl_divergence_plot.png', kl_plot, base_aspect_ratio = 1.5)

# 
# z <- hyman %>%
#   filter(signal_power == 3) %>%
#   filter(encoded_symbol == 'A')
#   
# 
hh_plot <- hyman %>%
  mutate(information = round(information)) %>%
  filter(signal_power == 3) %>%
  group_by(encoded_symbol, signal_power, round(information)) %>%
  mutate(stop_time = stop_time/10,
         m = mean(stop_time,na.rm=TRUE),
         sem = m/sqrt(n())) %>%
  ungroup() %>%
  ggplot() +
  geom_point(aes(information,m, group=information)) +
  geom_smooth(aes(information, m), method='lm', color='darkgray') + 
  geom_errorbar(aes(x=information, ymin=m-sem, ymax=m+sem)) + 
  labs(title='Transmission time is linear \nwith information transmission',
         x='Message surprisal (bits)', y='Transmission time (s)')
  #scale_x_continuous(limits=c(0,4)) +
  #scale_y_continuous(limits=c(0,20))
show(hh_plot)
combined <- plot_grid(hh_plot, kl_plot, labels = c("A", "B"), ncol=1)
show(combined)
save_plot('../figures/h_kl.png', combined, base_aspect_ratio = 0.8, base_height=6)

# 
# geom_smooth(aes(as.factor(information),stop_time,group=as.factor(signal_power)), method='lm')
#   labs(title='Transmission time as a function of H(X)',x='H(X)', y="Transmission time (AU)") + 
#   theme_cowplot() + 
#   theme(legend.position="bottom", legend.justification="center") 
# show(kl_plot)
# 
# 
# 
# #IS KL DIVERGENCE CALCULATED CORRECTLY? I think so...
# save_plot('../../reports/figures/kl_divergence_plot.png', p, base_aspect_ratio = 1.5)
# 
# 
# 
kl_multinomial <- function(q,p){
  #p is actual probabilities
  #q is probabilities or counts -   #NOTE THE ORDER
  p <- p/sum(p)
  q <- q/sum(q)
  return(sum(p*log2(p/q)))
}

pr <- c(.1, .5, .9, 5, .3, .3, .1, 2)
data_multinomial <- data_frame()
for(user_number in 1:2000){
  print(user_number)
  for(n_samples in 2^seq(2,14,by=0.2)){
    #generate n_samples from actual distribution
    x <- rmultinom(n_samples, length(pr), prob=pr)
    
    #estimate parameters from distribution based on samples
    q <- colSums(t(x))
    q <- q/sum(q)
    
    data_multinomial <- data_multinomial %>% 
      bind_rows(data_frame(user_number = user_number, 
                           KL=kl_multinomial(q,pr), #REVERSE order
                           KL2=kl_multinomial(pr,q), #REVERSE order
                           samples = n_samples))
  }
}

#uniform distribution with 16 params
data_multinomial <- read_csv('data/kl_multinomial.csv')
p_multinomial <- data_multinomial %>%
  filter(!is.na(KL),
         is.finite(KL)) %>%
  filter(samples <= 10000) %>%
  group_by(samples) %>% #average across users
  summarise(mean_kl = mean(KL,na.rm=T)) %>%
  ungroup() %>%
  ggplot() + 
  geom_point(aes(samples,mean_kl)) + 
  scale_x_log10(breaks=c( 1, 10, 100, 1000,10000)) + 
  scale_y_log10(breaks=c(.001, 0.01, 0.1, 1, 10)) + 
  scale_color_grey('Distribution') +
  labs(title="Response time improvement by stimulus distribution",
       x='Number of transmissions',
       y='KL(P || Q) =\nexpected added code length') + 
  theme(
    legend.position = c(.95, .95),
    legend.justification = c("right", "top"))
save_plot('../figures/power_law_of_learing.png',
          p_multinomial,
          base_aspect_ratio = 1.5)

#SO now take the KL and find a transmission time to go along with it




