require(tidyverse)
source(file='/Volumes/Data/git/dissertation/thesis/Chapter 04 - A variable-length code/src/transmit/R/transmit.R')

# Want to vary the entropy (i.e. the distribution or proportion of what you're transmitting) and see how response time changes

alphabet_length = 128
codebook <- construct_codebook(paste('A',seq(1,alphabet_length)))
get_H <- function(d){
    return(-sum(d*log2(d)))
}
# This means we want the actual and expected proportion to be the SAME - but just want the entropy of those to be different
# Where uniform is highest entropy, and other distributions
require(entropy)
require(dplyr)
results_summary <- data.frame()
for(H in seq(0.1, log2(alphabet_length)-0.1, by=0.25)){  #gives us a range of efficiencies, want enough to convince ourselves of linearity
    print(paste('H iteration',H))
    noise <- 10
        print(paste('expected accuracy',expected_accuracy))
        #H = 4.75
        tries <- 0
        while(TRUE){
            tries = tries + 1
            dist <- runif(alphabet_length)
            if(tries > 50000){
                #create a mask to set several to 0
                dist <- dist * (rbinom(alphabet_length,1, runif(1))+.0001)
            }
            dist <- dist/sum(dist)
            get_H(dist)
            measured_H <- get_H(dist)
            if(abs(measured_H - H) < 0.01){
                actual_proportion <- dist
                prior_proportion <- dist
                break
            }
            if(tries >= 5000000) 
            {
                print('fail')
                break
            }
        }
        if(tries < 5000000){
            
            print(paste('working after tries:', tries))
            repeats = 1000
            results <- prior_experiment(
                codebook,
                actual_proportion = actual_proportion,
                prior_proportion = prior_proportion,
                expected_accuracy_threshold = expected_accuracy,
                signal_power = 3,
                noise_power = noise,
                timesteps = 50,
                time_interval = .1,
                repeats=repeats
            )
            number_correct <- results %>%
                count(sent_symbol,decoded_symbol) %>%
                filter(sent_symbol==decoded_symbol) %>%
                pull(n) %>%
                sum()
            print(table(results$sent_symbol, results$decoded_symbol))
            mutual_info <- mi.empirical(table(results$sent_symbol, results$decoded_symbol))
            print(paste('empirical',mi.empirical(table(results$sent_symbol, results$decoded_symbol))))
            #print(paste('plugin',mi.plugin(table(results$sent_symbol, results$decoded_symbol))))
            #print(paste('shrink',mi.shrink(table(results$sent_symbol, results$decoded_symbol))))
            accuracy <- number_correct/repeats #TODO: parameterize this
            print(paste('actual accuracy',accuracy))
            avg_time <- mean(results$stop_time, na.rm=T)
            sem <- sd(results$stop_time, na.rm=T)/sqrt(repeats)
            proportion_finished <- 1-sum(is.na(results$stop_time))/repeats
            print(paste('proportion_finished',proportion_finished))
            results_summary <- results_summary %>%
                bind_rows(
                    data_frame(H = get_H(actual_proportion),
                               accuracy = accuracy,
                               avg_time = avg_time,
                               sem = sem,
                               noise_power=noise,
                               expected_accuracy = expected_accuracy,
                               mi = mutual_info,
                               #mi.shrink = mi.shrink(table(results$sent_symbol, results$decoded_symbol)),
                               #mi.plugin = mi.plugin(table(results$sent_symbol, results$decoded_symbol)),
                               proportion_finished = proportion_finished
                    )
                )
        }
}

write_tsv(results_summary, '~/git/dissertation/thesis/Chapter 04 - A variable-length code/src/data/hick_hyman_signal_03_noise_10.tsv')


results_summary <- read_tsv('~/git/dissertation/thesis/Chapter 04 - A variable-length code/src/data/hick_hyman_signal_03_noise_10.tsv')
require(cowplot)
p1 <- results_summary %>%
    ggplot() + 
    #geom_smooth(aes(mi,avg_time),se=FALSE, color='darkgray') + 
    geom_point(aes(mi,avg_time,shape=as.factor(noise_power)), size=2) +
    #geom_errorbar(aes(x=mi, ymin=avg_time-sem, ymax=avg_time+sem)) + 
    #scale_x_continuous(limits=c(0, 4)) + 
    #scale_y_continuous(limits=c(0, 20)) + 
    scale_shape_discrete('Noise rate %') + 
    labs(title='Transmission time is (sub-)linear \nwith information transmitted',x='Mutual information (bits)', y="Transmission time (AU)") + 
    theme_cowplot() + 
    theme(legend.position="bottom", legend.justification="center") 


p2 <- results_summary %>%
    ggplot() + 
    geom_smooth(aes(log2(2^(mi)-3),avg_time,shape=noise),method='lm', se=FALSE, color='darkgray') + 
    geom_point(aes(log2(2^(mi)-3),avg_time,shape=as.factor(noise_power)), size=2) +
    #geom_errorbar(aes(x=log2(2^(mi)-1), ymin=avg_time-sem, ymax=avg_time+sem)) + 
    scale_x_continuous(limits=c(0.0, 3), expand=c(0,0)) + 
    #scale_y_continuous(limits=c(0, 20)) + 
    scale_shape_discrete('Decoding Accuracy %') + 
    labs(title='Transmission time is (sub-)linear \nwith information transmitted',x='Mutual information (bits)', y="Transmission time (AU)") + 
    theme_cowplot() + 
    theme(legend.position="bottom", legend.justification="center") 

log2(2^3.1-1)

he = log(Ne+1)
2^he = Ne-1
2^he-1 = Ne
log2(2^he-1) = ??

#does mutual information equal H? It should...but it won't because of accuracy. 
#but mutual information is really what we care about...accuracy just varies that. 
#MI is varied by both accuracy and the entropy of the signal
#so just plot MI vs time I think?  Or maybe just simulate with a fixed mutual information threshold?

# If MI was really linear with time, wouldn't the line go through 0? 
# Perhaps rather than linear, it's log(N+1)?????


    


#but humans aren't 'at capacity' - they are very likely below capacity
#so make:
# ENCODING distribution stays the same - it's the same task, after all
# TRUE distribution changes to reflect different task parameters
# see what transmission time is - probably more like KL than H??????

#WHY ISN"T KL varying when I vary P??


require(entropy)
require(dplyr)
results_summary <- data.frame()
for(H in seq(0.1, log2(alphabet_length)-0.1, by=0.25)){  #gives us a range of efficiencies, want enough to convince ourselves of linearity
    print(paste('H iteration',H))
    #for(noise in c(3, 10, 20)){ #want high res here b/c we will translate to accuracy and MI
    noise <- 10
        print(paste('expected accuracy',expected_accuracy))
        #H = 4.75
        tries <- 0
        while(TRUE){
            tries = tries + 1
            dist <- runif(alphabet_length)
            if(tries > 50000){
                #create a mask to set several to 0
                dist <- dist * (rbinom(alphabet_length,1, runif(1))+.0001)
            }
            dist <- dist/sum(dist)
            get_H(dist)
            measured_H <- get_H(dist)
            if(abs(measured_H - H) < 0.01){
                actual_proportion <- dist
                prior_proportion <- rep(1,length(actual_proportion))/sum(rep(1,length(actual_proportion)))
                break
            }
            if(tries >= 5000000) 
            {
                print('fail')
                break
            }
        }
        if(tries < 5000000){
            
            print(paste('working after tries:', tries))
            repeats = 1000
            results <- prior_experiment(
                codebook,
                actual_proportion = actual_proportion,
                prior_proportion = prior_proportion,
                expected_accuracy_threshold = expected_accuracy,
                signal_power = 3,
                noise_power = noise,
                timesteps = 50,
                time_interval = .1,
                repeats=repeats
            )
            number_correct <- results %>%
                count(sent_symbol,decoded_symbol) %>%
                filter(sent_symbol==decoded_symbol) %>%
                pull(n) %>%
                sum()
            print(paste("KL",get_kl(actual_proportion, prior_proportion)))
            #print(table(results$sent_symbol, results$decoded_symbol))
            mutual_info <- mi.empirical(table(results$sent_symbol, results$decoded_symbol))
            print(paste('empirical',mi.empirical(table(results$sent_symbol, results$decoded_symbol))))
            #print(paste('plugin',mi.plugin(table(results$sent_symbol, results$decoded_symbol))))
            #print(paste('shrink',mi.shrink(table(results$sent_symbol, results$decoded_symbol))))
            accuracy <- number_correct/repeats #TODO: parameterize this
            print(paste('actual accuracy',accuracy))
            avg_time <- mean(results$stop_time, na.rm=T)
            sem <- sd(results$stop_time, na.rm=T)/sqrt(repeats)
            proportion_finished <- 1-sum(is.na(results$stop_time))/repeats
            print(paste('proportion_finished',proportion_finished))
            results_summary <- results_summary %>%
                bind_rows(
                    data_frame(H = get_H(actual_proportion),
                               accuracy = accuracy,
                               avg_time = avg_time,
                               sem = sem,
                               noise_power=noise,
                               expected_accuracy = expected_accuracy,
                               mi = mutual_info,
                               kl = get_kl(actual_proportion, prior_proportion),
                               #mi.shrink = mi.shrink(table(results$sent_symbol, results$decoded_symbol)),
                               #mi.plugin = mi.plugin(table(results$sent_symbol, results$decoded_symbol)),
                               proportion_finished = proportion_finished
                    )
                )
        }
        
    #}
}

results_summary %>%
    ggplot() + 
    #geom_smooth(aes(mi,avg_time),se=FALSE, color='darkgray') + 
    geom_point(aes(kl + H,avg_time), size=2) +
    #geom_errorbar(aes(x=mi, ymin=avg_time-sem, ymax=avg_time+sem)) + 
    #scale_x_continuous(limits=c(0, 4)) + 
    #scale_y_continuous(limits=c(0, 20)) + 
    scale_shape_discrete('Decoding Accuracy %') + 
    labs(title='Transmission time is (sub-)linear \nwith information transmitted',x='Mutual information (bits)', y="Transmission time (AU)") + 
    theme_cowplot() + 
    theme(legend.position="bottom", legend.justification="center") 




require(entropy)
require(dplyr)
results_summary <- data.frame()
for(H in seq(0.1, log2(alphabet_length)-0.1, by=0.25)){  #gives us a range of efficiencies, want enough to convince ourselves of linearity
    print(paste('H iteration',H))
    for(noise in c(3, 10, 20)){ #want high res here b/c we will translate to accuracy and MI
        print(paste('expected accuracy',expected_accuracy))
        #H = 4.75
        tries <- 0
        while(TRUE){
            tries = tries + 1
            dist <- runif(alphabet_length)
            if(tries > 50000){
                #create a mask to set several to 0
                dist <- dist * (rbinom(alphabet_length,1, runif(1))+.0001)
            }
            dist <- dist/sum(dist)
            get_H(dist)
            measured_H <- get_H(dist)
            if(abs(measured_H - H) < 0.01){
                actual_proportion <- dist
                prior_proportion <- rep(1,length(actual_proportion))/sum(rep(1,length(actual_proportion)))
                break
            }
            if(tries >= 5000000) 
            {
                print('fail')
                break
            }
        }
        if(tries < 5000000){
            
            print(paste('working after tries:', tries))
            repeats = 1000
            
            print(paste("KL",get_kl(actual_proportion, prior_proportion)))
            #print(table(results$sent_symbol, results$decoded_symbol))
            print(paste('empirical',mi.empirical(table(results$sent_symbol, results$decoded_symbol))))
            print(paste('H', H))
            
            results_summary <- results_summary %>%
                bind_rows(
                    data_frame(H = get_H(actual_proportion),
                               kl = get_kl(actual_proportion, prior_proportion),
                               proportion_finished = proportion_finished
                    )
                )
            
        }
        
    }
}


results_summary %>%
    ggplot() + 
    #geom_smooth(aes(mi,avg_time),se=FALSE, color='darkgray') + 
    geom_point(aes(kl,H,shape=as.factor(noise_power)), size=2) +
    #geom_errorbar(aes(x=mi, ymin=avg_time-sem, ymax=avg_time+sem)) + 
    #scale_x_continuous(limits=c(0, 4)) + 
    #scale_y_continuous(limits=c(0, 20)) + 
    scale_shape_discrete('Decoding Accuracy %') + 
    labs(title='Transmission time is (sub-)linear \nwith information transmitted',x='Mutual information (bits)', y="Transmission time (AU)") + 
    theme_cowplot() + 
    theme(legend.position="bottom", legend.justification="center") 

