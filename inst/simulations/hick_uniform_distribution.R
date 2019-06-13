source('~/git/dissertation/thesis/Chapter 04 - A variable-length code/src/transmit/R/transmit.R')
require(entropy)
require(cowplot)
require(tidyverse)

entropy_threshold_experiment <- function(
  codebook,
  actual_proportion,
  prior_proportion,
  entropy_threshold,
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
                           threshold_value = entropy_threshold,
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
                             threshold_value = entropy_threshold,
                             MoreArgs = list(codebook=codebook,
                                             prior=prior_proportion),
                             mc.cores=8,
                             SIMPLIFY=FALSE)
    results <- rbindlist(results.temp)
    results.temp[[4]]
    return(results)
  }
}


results_summary <- data.frame()
alphabet <- as.character(seq(1,64))
codebook_sizes <- 2^(1:6)
entropy_thresholds <- c(0.3, 0.5, 1)
loops <- length(codebook_sizes) * length(entropy_threshold)
for(codebook_size in codebook_sizes){
  
  codebook <- construct_codebook(alphabet[1:codebook_size], rep(1,codebook_size)) #1 neuron per possible 'signal'
  print(paste('codebook size',codebook_size))
  #uniform prior
  prior_proportion <- rep(1,codebook_size)
  prior_proportion <- prior_proportion/sum(prior_proportion)
  actual_proportion <- prior_proportion
  
  for(entropy_threshold in entropy_thresholds){ #want high res here b/c we will translate to accuracy and MI
    start_time = Sys.time()
    print(paste('entropy_threshold',entropy_threshold))
    repeats = 1000
    results <- entropy_threshold_experiment(
      codebook,
      actual_proportion = actual_proportion,
      prior_proportion = prior_proportion,
      entropy_threshold = entropy_threshold,
      signal_power = 4,
      noise_power = 10,
      timesteps = 500,
      time_interval = .1,
      repeats=repeats
    )
    
    if(nrow(results) != repeats){
      print("STOP NOW, SOMETHING IS WRONG")
    }
    number_correct <- results %>%
      count(sent_symbol,decoded_symbol) %>%
      filter(sent_symbol==decoded_symbol) %>%
      pull(n) %>%
      sum()
    z <- table(results$sent_symbol, results$decoded_symbol)
    z <- z[,rownames(z)]
    mutual_info <- mi.empirical(z, unit='log2')
    print(paste('mutual info',mutual_info))
    #mutual_info <- mi.empirical(table(results$sent_symbol, results$decoded_symbol))
    accuracy <- number_correct/repeats #TODO: parameterize this
    print(paste('actual accuracy',accuracy))
    avg_time <- mean(results$stop_time, na.rm=T)
    proportion_finished <- 1-sum(is.na(results$stop_time))/repeats
    print(paste('proportion finished',proportion_finished))
    
    results$kl = get_kl(actual_proportion, prior_proportion)
    results$accuracy = accuracy
    results$entropy_threshold = entropy_threshold
    results$mi = mutual_info
    results$proportion_finished = proportion_finished
    results$codebook_size = codebook_size

            results_summary <- results_summary %>%
                            bind_rows(results)
    
            end_time = Sys.time()
    loops <- loops-1
    print(paste('expected time remaining',difftime(end_time,start_time,units='mins')*loops,'minutes')) #need timediff to get proper units
  }
}

#write_rds(results_summary, path = '~/Dropbox/Apps/ShareLaTeX/CogSci 2019 (Camera Ready)/code/data/hick_signal_4_noise_10.rdata')
results_summary <- read_rds(path = '~/Dropbox/Apps/ShareLaTeX/CogSci 2019 (Camera Ready)/code/data/hick_signal_4_noise_10.rdata')
require(ggridges)

#results_summary <- read_tsv('~/git/dissertation/thesis/Chapter 04 - A variable-length code/src/data/hick_signal_4_noise_10.tsv')
# plot aes(spikes_per_bit, codebook_size)
# spikes per bit is E_b, or energy per bit
results_summary <- results_summary %>% filter(entropy_threshold == 0.3) %>%
    mutate(stop_time = stop_time/10)
p1 <- ggplot() + 
    # geom_density_ridges2(data=results_summary,
    #                      aes(y=as.numeric(stop_time), x=as.factor(codebook_size)))
  geom_point(data=results_summary %>%
               group_by(codebook_size, entropy_threshold) %>%
               summarise(avg_time = mean(stop_time)) %>%
               ungroup(), 
             aes(x=codebook_size, y=avg_time), size=1) + 
  geom_line(data=results_summary %>%
                group_by(codebook_size, entropy_threshold) %>%
                summarise(avg_time = mean(stop_time)) %>%
                ungroup(),
            aes(codebook_size, avg_time, group=entropy_threshold)) + 
    geom_ribbon(data=results_summary %>%
                      group_by(codebook_size, entropy_threshold) %>%
                      summarise(top = quantile(stop_time, .95),
                                bottom = quantile(stop_time, .05),
                                avg_time = mean(stop_time)) %>%
                      ungroup(),
              aes(codebook_size, ymin=bottom, ymax=top, group=entropy_threshold), width=0, alpha=0.1) + 
  geom_ribbon(data=results_summary %>%
                group_by(codebook_size, entropy_threshold) %>%
                summarise(top = quantile(stop_time, .75),
                          bottom = quantile(stop_time, .25),
                          avg_time = mean(stop_time)) %>%
                ungroup(),
              aes(codebook_size, ymin=bottom, ymax=top, group=entropy_threshold), width=0, alpha=0.2) + 
  labs( x = 'Codebook size', y=
         'Transmission time (s)') + 
  scale_shape("Entropy\nthreshold\n(bits)") + 
  scale_x_continuous(limits=c(0,70)) + 
  scale_y_continuous(limits=c(0,15)) 

p2 <- results_summary %>%
    group_by(codebook_size, entropy_threshold) %>%
    summarise(.top = quantile(stop_time, .95),
              .bottom = quantile(stop_time, .05),
              .top2 = quantile(stop_time, .75),
              .bottom2 = quantile(stop_time, .25),
              avg_time = mean(stop_time),
              mi = first(mi)) %>%
    ungroup() %>% #4 is the signal power
  ggplot() + 
  geom_point(aes(mi,avg_time),size=1) + 
  geom_ribbon(aes(x=mi,ymax=.top, ymin=.bottom), alpha=0.1) + 
  geom_ribbon(aes(x=mi,ymax=.top2, ymin=.bottom2), alpha=0.2) + 
    #geom_point(aes(x=mi,y=.bottom), width=0, color='red') + 
  geom_line(aes(mi, avg_time)) + 
  #scale_x_log10() + 
  #scale_y_log10() + 
  labs(x = 'Mutual information (bits)', y='Transmission time (s)') + 
  scale_shape("Entropy\nthreshold\n(bits)") +
  theme(legend.position = c(.65, 0.3)) + 
  scale_x_continuous(limits=c(0,7)) + 
  scale_y_continuous(limits=c(0,15))

p <- plot_grid(p1, p2, labels = c("A", "B"))
show(p)


save_plot('~/Dropbox/Apps/ShareLaTeX/CogSci 2019 (Camera Ready)/figures/hick_simulation_ribbon.png',
p, dpi=450,
base_height=5,base_aspect_ratio = 1.5)


# 
# results_summary <- data.frame()
# alphabet <- as.character(seq(1,64))
# signal_powers <- c(1,2^(1:5))
# entropy_thresholds <- c(0.5)
# for(signal_power in signal_powers){
#   codebook_size = 8  
#   codebook <- construct_codebook(alphabet[1:codebook_size], rep(1,codebook_size)) #1 neuron per possible 'signal'
#   print(paste('codebook size',codebook_size))
#   #uniform prior
#   prior_proportion <- rep(1,codebook_size)
#   prior_proportion <- prior_proportion/sum(prior_proportion)
#   actual_proportion <- prior_proportion
#   
#   for(entropy_threshold in entropy_thresholds){ #want high res here b/c we will translate to accuracy and MI
#     start_time = Sys.time()
#     print(paste('entropy_threshold',entropy_threshold))
#     repeats = 1000
#     results <- entropy_threshold_experiment(
#       codebook,
#       actual_proportion = actual_proportion,
#       prior_proportion = prior_proportion,
#       entropy_threshold = entropy_threshold,
#       signal_power = signal_power,
#       noise_power = 10,
#       timesteps = 500,
#       time_interval = .1,
#       repeats=repeats
#     )
#     
#     if(nrow(results) != repeats){
#       print("STOP NOW, SOMETHING IS WRONG")
#     }
#     number_correct <- results %>%
#       count(sent_symbol,decoded_symbol) %>%
#       filter(sent_symbol==decoded_symbol) %>%
#       pull(n) %>%
#       sum()
#     z <- table(results$sent_symbol, results$decoded_symbol)
#     z <- z[,rownames(z)]
#     mutual_info <- mi.empirical(z, unit='log2')
#     print(paste('mutual info',mutual_info))
#     #mutual_info <- mi.empirical(table(results$sent_symbol, results$decoded_symbol))
#     accuracy <- number_correct/repeats #TODO: parameterize this
#     print(paste('actual accuracy',accuracy))
#     avg_time <- mean(results$stop_time, na.rm=T)
#     print(paste('avg time',avg_time))
#     proportion_finished <- 1-sum(is.na(results$stop_time))/repeats
#     print(paste('proportion finished',proportion_finished))
#     results_summary <- results_summary %>%
#       bind_rows(
#         data_frame(kl = get_kl(actual_proportion, prior_proportion),
#                    accuracy = accuracy,
#                    signal_power = signal_power,
#                    avg_time = avg_time,
#                    entropy_threshold = entropy_threshold,
#                    mi = mutual_info,
#                    proportion_finished = proportion_finished,
#                    codebook_size = codebook_size
#         )
#       )
#     end_time = Sys.time()
#     loops <- loops-1
#     print(paste('expected time remaining',difftime(end_time,start_time,units='mins')*loops,'minutes')) #need timediff to get proper units
#   }
# }
# 
# p <- results_summary %>%
#   mutate(spikes_per_bit = avg_time*4/mi) %>% #4 is the signal power
#   ggplot() + 
#   geom_point(aes(signal_power,spikes_per_bit), size=2) + 
#   geom_smooth(aes(signal_power, spikes_per_bit),se=FALSE,method='lm', color='gray')+
#   scale_x_log10() + 
#   scale_y_log10(limits=c(1, 1100)) + 
#   labs(x='Signal power',y='Spikes per bit', title='Transmission cost as signal power is increased')
# show(p)
# save_plot('~/git/dissertation/thesis/Chapter 02 - Why do some tasks feel more effortful than others?/reports/figures/efficiency_increases_with_signal_strength.png', p, dpi=450,
#           base_height=5,base_aspect_ratio = 1.5)
 
