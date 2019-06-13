require(tidyverse)
require(entropy)
setwd('~/git/dissertation/thesis/Chapter 04 - A variable-length code/src/simulations/')
source(file='~/Dropbox/Apps/ShareLaTeX/CogSci 2019 (Camera Ready)/code/transmit/R/transmit.R')

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

alphabet_length <- 4
symbols <- c('A','B','C','D')
codebook <- construct_codebook(symbols)
timesteps <- 1000

#Basic idea here is to pick a distribution, then send INDIVIDUAL messages using that distribution, and measure response times
distribution <- rev(2^(1:4)/sum(2^(1:4)))
distribution <- distribution/sum(distribution)

args <- expand.grid(
  repeats=1:20000,
  noise_power=10,
  signal_power=4,
  threshold_value=0.3,
  time_interval=0.1,
  timesteps=50
)

#test
hyman_transmit(
  noise_power=10,
  signal_power=3,
  threshold_value=0.3,
  time_interval=0.1,
  timesteps=1000,
  true_distribution = c(0.1, 0.2, 0.4, 0.3),
  prior=distribution,
  codebook=codebook
)

#uniform prior
results.temp <- mcmapply(hyman_transmit,
                         noise_power=args$noise_power,
                         signal_power=args$signal_power,
                         threshold_value=args$threshold_value,
                         time_interval=args$time_interval,
                         timesteps=args$timesteps,
                         MoreArgs = list(codebook=codebook,
                                         prior=c(0.25, 0.25, 0.25, 0.25),
                                         true_distribution=distribution),
                         mc.cores=8,
                         SIMPLIFY=FALSE)
results_uniform <- rbindlist(results.temp)
correct <- results_uniform %>%
  count(encoded_symbol, decoded_symbol) %>%
  mutate(correct = n*(encoded_symbol == decoded_symbol)) %>%
  pull(correct) %>% sum()
print(paste('accuracy',correct/20000))

#just 4 draws
z <- rowSums(rmultinom(1000, 4, distribution)) + 2000 #the 2000 is to mimic a weak uniform dirichlet prior of 2 observations per category
weak_prior <- z/sum(z)
weak_prior
results.temp <- mcmapply(hyman_transmit,
                         noise_power=args$noise_power,
                         signal_power=args$signal_power,
                         threshold_value=args$threshold_value,
                         time_interval=args$time_interval,
                         timesteps=args$timesteps,
                         MoreArgs = list(codebook=codebook,
                                         prior=weak_prior,
                                         true_distribution=distribution),
                         mc.cores=8,
                         SIMPLIFY=FALSE)
results_weak <- rbindlist(results.temp)
correct <- results_weak %>%
  count(encoded_symbol, decoded_symbol) %>%
  mutate(correct = n*(encoded_symbol == decoded_symbol)) %>%
  pull(correct) %>% sum()
print(paste('accuracy',correct/20000))


#midway prior...draw say 10 draws from the categorical distribution above
#do this 1000 times
#average the number of draws in each category
z <- rowSums(rmultinom(1000, 10, distribution)) + 2000 #the 2000 is to mimic a weak uniform dirichlet prior of 2 observations per category
strong_prior <- z/sum(z)
strong_prior
results.temp <- mcmapply(hyman_transmit,
                         noise_power=args$noise_power,
                         signal_power=args$signal_power,
                         threshold_value=args$threshold_value,
                         time_interval=args$time_interval,
                         timesteps=args$timesteps,
                         MoreArgs = list(codebook=codebook,
                                         prior=strong_prior,
                                         true_distribution=distribution),
                         mc.cores=8,
                         SIMPLIFY=FALSE)
results_middle <- rbindlist(results.temp)
correct <- results_middle %>%
  count(encoded_symbol, decoded_symbol) %>%
  mutate(correct = n*(encoded_symbol == decoded_symbol)) %>%
  pull(correct) %>% sum()
print(paste('accuracy',correct/20000))



#midway prior...draw say 10 draws from the categorical distribution above
#do this 1000 times
#average the number of draws in each category
z <- rowSums(rmultinom(1000, 100, distribution)) + 2000 #the 2000 is to mimic a weak uniform dirichlet prior of 2 observations per category
very_strong_prior <- z/sum(z)
very_strong_prior
results.temp <- mcmapply(hyman_transmit,
                         noise_power=args$noise_power,
                         signal_power=args$signal_power,
                         threshold_value=args$threshold_value,
                         time_interval=args$time_interval,
                         timesteps=args$timesteps,
                         MoreArgs = list(codebook=codebook,
                                         prior=very_strong_prior,
                                         true_distribution=distribution),
                         mc.cores=8,
                         SIMPLIFY=FALSE)
results_verystrong <- rbindlist(results.temp)
correct <- results_verystrong %>%
  count(encoded_symbol, decoded_symbol) %>%
  mutate(correct = n*(encoded_symbol == decoded_symbol)) %>%
  pull(correct) %>% sum()
print(paste('accuracy',correct/20000))


results.temp <- mcmapply(hyman_transmit,
                         noise_power=args$noise_power,
                         signal_power=args$signal_power,
                         threshold_value=args$threshold_value,
                         time_interval=args$time_interval,
                         timesteps=args$timesteps,
                         MoreArgs = list(codebook=codebook,
                                         prior=distribution,
                                         true_distribution=distribution),
                         mc.cores=8,
                         SIMPLIFY=FALSE)
results_true <- rbindlist(results.temp)
correct <- results_true %>%
  count(encoded_symbol, decoded_symbol) %>%
  mutate(correct = n*(encoded_symbol == decoded_symbol)) %>%
  pull(correct) %>% sum()
print(paste('accuracy',correct/20000))

results <- results_uniform %>% mutate(prior='Uniform') %>%
  bind_rows(results_verystrong %>% mutate(prior='Good')) %>%
  bind_rows(results_middle %>% mutate(prior='Approximate')) %>%
  bind_rows(results_weak %>% mutate(prior = 'Weak')) %>%
  bind_rows(results_true %>% mutate(prior = 'True')) %>%
  mutate(stop_time = stop_time*.1) #time interval

saveRDS(results, file = '~/git/dissertation/thesis/Chapter 04 - A variable-length code/src/data/hyman_multiple_distributions_4_noise_10.rdata')
# results <- readRDS('~/git/dissertation/thesis/Chapter 04 - A variable-length code/src/data/hyman_multiple_distributions_4_noise_10.rdata')


results <- readRDS('~/Dropbox/Apps/ShareLaTeX/CogSci 2019 (Camera Ready)/code/data/hyman_multiple_distributions_4_noise_10.rdata')

p <- results %>%
  filter(prior != 'Weak', prior != 'Good') %>%
  #filter(encoded_symbol == decoded_symbol) %>%
  #mutate(encoded_symbol = factor(encoded_symbol, levels = c('A','B','C','D'))) %>%
  mutate(prior = factor(prior, levels = c('True','Approximate','Uniform'))) %>%
  group_by(prior, encoded_symbol) %>%
  summarise(upper = quantile(stop_time,.9),
            lower=quantile(stop_time,.1),
            stop_time = mean(stop_time,na.rm=T),
            information = mean(information)) %>%
  ungroup() %>%
  ggplot()+ 
  geom_point(aes(encoded_symbol,stop_time), size=3) + 
  geom_line(aes(encoded_symbol,stop_time, group=prior, linetype=prior)) + 
  labs(x='Transmitted symbol', y='Transmission time (s)', title='Transmission time of a symbol\ndepends on the prior distribution') + 
  scale_color_discrete(guide=FALSE) + 
  scale_linetype_discrete('Prior distribution')  + 
  theme(legend.position = 'bottom',
        legend.justification = 'center') 
#scale_y_continuous(limits=c(0,max(stop_time)))
  
show(p)
##Save this plot here...
save_plot('~/Dropbox/Apps/ShareLaTeX/Mental Effort and Efficient Information Representation/figures/ch03-coding/hyman_multiple_priors.png',
          p, dpi=450,
          base_height=5,base_aspect_ratio = 1.2)


##Then using the true distribution only, make another plot with the same data, but now show actual info transmitted
results %>% filter(prior == 'True') %>%
  filter(encoded_symbol == decoded_symbol) %>%
  mutate(stop_time=stop_time*.1) %>%
  ggplot() + 
  geom_density(aes(information)) + 
  facet_grid(encoded_symbol ~ .)
df <- results %>%
    filter(prior == 'True') %>%
  filter(encoded_symbol == decoded_symbol) %>%
  mutate(stop_time=stop_time) %>%
  group_by(encoded_symbol) %>%
  summarise(mean_time = mean(stop_time,na.rm=T),
         mean_info = mean(information,na.rm=T)) %>%
  ungroup()

p <- ggplot()+ 
  geom_point(data = results %>%
               filter(encoded_symbol == decoded_symbol,
                      prior == 'True') %>%
               mutate(stop_time=stop_time),
             aes(information,stop_time, shape=encoded_symbol), alpha=0.1) +
  geom_line(data=df,aes(mean_info, mean_time), size=1, color='gray') + 
    geom_point(data=df,aes(mean_info, mean_time, shape=encoded_symbol), size=4, color='darkgray') + 
    scale_x_continuous(breaks=c(1, 2, 3, 4), label=c(1, 2, 3, 4), limits=c(0, 4))+
  labs(x='Entropy reduced, or information transmitted (bits)',
       y='Transmission time (s)', title="Transmission time is linear with surprisal") + 
  scale_shape_manual('Encoded symbol', values=c(15, 16, 17, 18)) + 
  theme(legend.position = 'bottom',
        legend.justification = 'center')  + 
    scale_y_continuous(limits=c(0, 15))
show(p)
##Save this plot here...
save_plot('~/Dropbox/Apps/ShareLaTeX/CogSci 2019 (Camera Ready)/figures/hyman_single.png',
          p, dpi=450,
          base_height=5,base_aspect_ratio = 1.5)



############################# NEED BETTER KL's here
############################# Want to keep the entropy threshold...so compare mostly-zero arrays with uniform arrays
############################ Way to get big KL's is to have the p distribution be weird, like mostly zeros and a few ones
# Then start out with a uniform distribution and make pesudo-observations (draws from the prior) and update - lowering the KL

get_p_size <- function(size,n_full){
  epsilon = 0.01
  p <- c(rep(epsilon,(size-n_full)), rep(1-epsilon*(size-n_full)/n_full,n_full))
  p <- p/sum(p)
  q <- rep(1,size)/sum(rep(1,size))
  #print(p)
  #print(q)
  #print(get_kl(p,q))
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
  timesteps=200
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
      #filter(encoded_symbol == decoded_symbol) %>%
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

#What the heck shape is this...maybe I should/not filter by correct transmissions?
results %>%
ggplot() +
  geom_point(aes(kl,stop_time), size=3)
  
results %>%
  ggplot() + 
  geom_point(aes(strength, stop_time), size=3) + 
  geom_errorbar(aes(x=strength,ymin=stop_time-lower, ymax=stop_time+upper)) + 
  geom_smooth(aes(x=strength,y=stop_time),color='lightgray', method='lm', se = FALSE) + 
  scale_x_log10() + 
  scale_y_log10()
  

results %>%
  geom_errorbar(aes(x=KL,ymin=stop_time-se, ymax=stop_time+se)) + 
  geom_line(aes(KL,stop_time)) + 
  labs(x='KL-divergence between P(X) and Q(X) (bits)', y='Transmission time (s)') + 
  scale_color_discrete(guide=FALSE) + 
  scale_linetype_discrete('Prior distribution')  + 
  theme(legend.position = 'bottom',
        legend.justification = 'center')
show(p)

save_plot('~/Dropbox/Apps/ShareLaTeX/Mental Effort and Efficient Information Representation/figures/ch03-coding/kl_time.png',
          p, dpi=450,
          base_height=5,base_aspect_ratio = 1.2)












# 
# get_p_dist <- function(size){
#   epsilon = 0.01
#   p <- c(rep(epsilon,(size-2)), rep(1-epsilon*(size-2)/2,2))
#   p <- p/sum(p)
#   q <- rep(1,size)/sum(rep(1,size))
#   print(p)
#   print(q)
#   print(get_kl(p,q))
#   return(p)
# }
# get_p_dist(8)


##Then aggregating over priors, calculate average response time and mutual information.  
## Will need to make the 
very_strong_kl <- get_kl(distribution, very_strong_prior)
strong_kl <- get_kl(distribution, strong_prior)
uniform_kl <- get_kl( distribution, c(0.25, 0.25, 0.25, 0.25))
weak_kl <- get_kl(distribution, weak_prior)


results <- results_uniform %>% mutate(prior='Uniform') %>%
  bind_rows(results_weak %>% mutate(prior='Good')) %>%
  bind_rows(results_middle %>% mutate(prior='Better')) %>%
  bind_rows(results_verystrong %>% mutate(prior='Best')) %>%
  bind_rows(results_true %>% mutate(prior = 'True')) %>%
  mutate(stop_time = stop_time*.1) #time interval
p <- results %>%
  filter(encoded_symbol == decoded_symbol) %>%
  #filter(encoded_symbol == 'D') %>%
  mutate(prior = factor(prior, levels = c('True','Best','Better','Good','Uniform'))) %>%
  filter(prior != 'Best') %>%
  #group_by(prior) %>%
  #mutate(limit = quantile(stop_time,.99)) %>%
  #ungroup() %>%
  #filter(stop_time < limit) %>%
  group_by(prior) %>%
  summarise(stop_time_sd = sd(stop_time),
            stop_time=mean(stop_time,na.rm=T),
            se = stop_time_sd/sqrt(n())
            ) %>%
  ungroup() %>%
  mutate(KL=case_when(
            prior == 'True'          ~ 0,
            prior == 'Good'          ~ weak_kl,
            prior == 'Better'   ~     strong_kl,
            prior == 'Best'   ~       very_strong_kl,
            prior == 'Uniform'       ~ uniform_kl
  )) %>%
  ggplot() +
  geom_point(aes(KL,stop_time, shape=prior), size=3) + 
  geom_errorbar(aes(x=KL,ymin=stop_time-se, ymax=stop_time+se)) + 
  geom_line(aes(KL,stop_time)) + 
  labs(x='KL-divergence between P(X) and Q(X) (bits)', y='Transmission time (s)') + 
  scale_color_discrete(guide=FALSE) + 
  scale_linetype_discrete('Prior distribution')  + 
  theme(legend.position = 'bottom',
        legend.justification = 'center') + 
  scale_shape('Prior')
show(p)

save_plot('~/Dropbox/Apps/ShareLaTeX/Mental Effort and Efficient Information Representation/figures/ch03-coding/kl_time.png',
          p, dpi=450,
          base_height=5,base_aspect_ratio = 1.2)




# 
# results_summary <- data.frame()
# for(threshold_value in c(0.8, 0.9, 0.95,  0.99)){
#   print(paste('working on threshold_value',threshold_value))
#   df <- data_frame()
#   for(i in 1:200){
#     
#     ix <- sample(c(1,2,3,4),1)
#     current_symbol = symbols[ix]
#     
#     signal <- encode_signal(
#       codebook = codebook,
#       current_symbol = current_symbol,
#       signal_power = signal_power,
#       timesteps = timesteps
#     )
#     
#     signal_plus_noise <- add_channel_noise(
#       codebook = codebook,
#       signal = signal,
#       noise_power = noise_power,
#       timesteps = timesteps
#     )
#     #table(signal_plus_noise$group_index)
#     
#     
#     z <- decode_signal(
#       codebook = codebook,
#       signal_plus_noise = signal_plus_noise,
#       signal_power = signal_power,
#       noise_power = noise_power,
#       prior = distribution,
#       time_interval = 0.01,
#       threshold = 'error_rate',
#       threshold_value = threshold_value,
#       return_entropy_trace = FALSE,
#       return_posteriors = FALSE
#     )
#     posterior <- unlist(z$posterior_at_stop_time)
#     
#     z$current_symbol_frequency <- distribution[ix]
#     z$threshold_value <- threshold_value
#     z$encoded_symbol <- current_symbol
#     z$signal_power <- signal_power
#     z$information <- get_kl(posterior, distribution)#
#     z$information2 <- get_kl(distribution, posterior)#
#     df <- df %>% bind_rows(as.data.frame(z))
#   }
#   results_summary <- results_summary %>%
#     bind_rows(df)
# }
# 
# carequire(cowplot)
# #forewards
# results_summary %>%
#   ggplot() + 
#   geom_point(aes(information,stop_time, color=as.factor(threshold_value)),size=2) + 
#   scale_x_continuous(limits=c(0,4)) + 
#   facet_grid(. ~ as.factor(threshold_value))
# 
# #forewards
# results_summary %>%
#   group_by(current_symbol_frequency, threshold_value) %>%
#   summarise(stop_time = mean(stop_time,na.rm=T),
#             information = mean(information,na.rm=T),
#             information2 = mean(information2,na.rm=T)) %>%
#   ungroup() %>%
#   ggplot() + 
#   geom_point(aes(information,stop_time, color=as.factor(threshold_value)),size=2) + 
#   scale_x_continuous(limits=c(0,4)) + 
#   scale_y_continuous(limits=c(0,100))
# 
# 
# #forewards
# results_summary %>%
#   mutate(information=round(information)) %>%
#   ggplot() + 
#   geom_boxplot(aes(as.factor(information),stop_time, color=as.factor(threshold_value)),size=2) + 
#   geom_smooth(aes(as.factor(information),stop_time,group=as.factor(threshold_value)), method='lm')
# 
# #backwards
# results_summary %>%
#   mutate(information=round(information2)) %>%
#   ggplot() + 
#   geom_boxplot(aes(as.factor(information),stop_time, color=as.factor(threshold_value)),size=2) + 
#   geom_smooth(aes(as.factor(information),stop_time,group=as.factor(threshold_value)), method='lm')
# 
# 
# #forewards
# results_summary %>%
#   mutate(information=round(information)) %>%
#   group_by(information, threshold_value) %>%
#   summarise(stop_time = mean(stop_time,na.rm=T)) %>%
#   ungroup() %>%
#   ggplot() + 
#   geom_point(aes(information,stop_time, color=as.factor(threshold_value)),size=2) + 
#   geom_smooth(aes(information,stop_time,group=as.factor(threshold_value)), method='lm') + 
#   scale_x_continuous(limits=c(0,5)) +
#   labs(x='Info transmitted (bits)', y='transmission time')
# 
# #backwards
# results_summary %>%
#   mutate(information=round(information)) %>%
#   group_by(information, threshold_value) %>%
#   summarise(stop_time = mean(stop_time,na.rm=T)) %>%
#   ungroup() %>%
#   ggplot() + 
#   geom_point(aes(information,stop_time, color=as.factor(threshold_value)),size=2) + 
#   geom_smooth(aes(information,stop_time,group=as.factor(threshold_value)), method='lm') + 
#   scale_x_continuous(limits=c(0,5)) +
#   labs(x='Info transmitted (bits)', y='transmission time')
# 
# 
# 
# write_tsv(results_summary, '~/git/dissertation/thesis/Chapter 04 - A variable-length code/src/data/hick_hyman_noise_10.tsv')
# 
# hyman <- read_tsv('~/Dropbox/Apps/ShareLaTeX/CogSci 2019/code/data/hick_hyman_noise_10.tsv')
# 
# hyman %>%
#   mutate(accurate = encoded_symbol == decoded_symbol) %>%
#   count(signal_power,accurate)
# p <- hyman %>% 
#   filter(stop_time/100 < 8) %>%
#   mutate(signal_power = as.character(signal_power)) %>%
#   mutate(signal_power = ifelse(signal_power == '1.5', 'Low',
#                                ifelse(signal_power == '2','Medium',
#                                       ifelse(signal_power == '3','High', 'NA')))) %>%
#   mutate(signal_power = factor(signal_power,levels=c("Low","Medium","High")))%>%
#   #filter(signal_power != 'Medium') %>%
#   ggplot() + 
#   geom_histogram(aes(stop_time/100), bins=50) + 
#    
#   geom_line(data=data_frame(x=seq(1,7,by=0.1),
#                             y=dnorm(seq(1, 7, by=0.1), mean=5.4, sd=.6),
#                             signal_power=as.factor('Low')) %>%
#               mutate(x=exp(x),y=y*65),
#             aes(x/100,y), color='gray',size=2,alpha=0.9) + 
#   
#  geom_line(data=data_frame(x=seq(1,7,by=0.1),
#                            y=dnorm(seq(1, 7, by=0.1), mean=4.8, sd=.6),
#                            signal_power='Medium') %>%
#              mutate(x=exp(x),y=y*120),
#            aes(x/100,y), color='gray',size=2,alpha=0.9) +
# 
# geom_line(data=data_frame(x=seq(1,7,by=0.1),
#                           y=dnorm(seq(1, 7, by=0.1), mean=4, sd=.6),
#                           signal_power='High') %>%
#             mutate(x=exp(x),y=y*245),
#           aes(x/100,y), color='gray',size=2,alpha=0.9) + 
#   facet_grid(signal_power ~ .) +
#   scale_x_continuous(limits=c(0,8)) + 
#   theme(axis.ticks.y = element_blank(),
#         axis.text.y = element_blank()) + 
#   labs(x='Decoding time (s)', y='Count', title='Decoding time follows\na lognormal distribution') + 
#   theme(strip.text.y = element_text(size = 12))
# show(p)
# save_plot('../figures/response_times.png', p, base_aspect_ratio = 1.4)
