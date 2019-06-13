require(tidyverse)

results <- read_tsv('~/git/dissertation/thesis/Chapter 04 - A variable-length code/src/data/response_time_signal_1_noise_15_accuracy_08.tsv')


rt <- results %>%
    mutate(correct = ifelse(decoded_symbol == sent_symbol,'Correct','Incorrect')) %>%
    group_by(correct) %>%
    summarise(rt = mean(stop_time),
              n = n()) %>%
    ungroup()

p <- ggplot() + 
    geom_histogram(data=results %>%
                      mutate(correct = ifelse(decoded_symbol == sent_symbol,'Correct','Incorrect')),
                  aes(stop_time/100),bins=100) +
    geom_line(data=data_frame(x=seq(1,7,by=0.1),
                              y=dnorm(seq(1, 7, by=0.1), mean=5.3, sd=.6),
                              correct = 'Correct') %>%
                  mutate(x=exp(x),y=y*570),
            aes(x/100,y), color='gray',size=2,alpha=0.5) + 
    geom_line(data=data_frame(x=seq(1,7,by=0.1),
                              y=dnorm(seq(1, 7, by=0.1), mean=5.3, sd=.6),
                              correct = 'Incorrect') %>%
                  mutate(x=exp(x),y=y*150),
              aes(x/100,y), color='darkgray',size=2,alpha=0.9) + 
    facet_grid(correct ~ .) + 
    theme(axis.ticks.y = element_blank(),
          axis.text.y = element_blank()) + 
    labs(x='Decoding time (s)', y='Count', title='Decoding time follows\na lognormal distribution') + 
    theme(strip.text.y = element_text(size = 15))
show(p)

ggplot() + 
  geom_line(data=data_frame(x=seq(0,7,by=0.1),
                            y=dweibull(seq(0, 7, by=0.1), shape=1.4, scale=2),
                            correct = 'Correct') %>%
              mutate(x=x,y=y*570),
            aes(x,y), color='red',size=2,alpha=0.5) + 
  facet_grid(correct ~ .) + 
  theme(axis.ticks.y = element_blank(),
        axis.text.y = element_blank()) + 
  labs(x='Decoding time (s)', y='Count', title='Decoding time follows\na lognormal distribution') + 
  theme(strip.text.y = element_text(size = 15))
show(p)


save_plot('../figures/response_time_distributions.png', p, base_aspect_ratio = 1.5)






