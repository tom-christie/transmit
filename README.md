# transmit 

The aim of this package is to enable simulations of information transmission using Poisson processes and Bayesian inference as described [in this paper](https://github.com/tom-christie/transmit/blob/master/vignettes/Christie_and_Schrater_2019.pdf).



## Installation

```r
# Install the development version from GitHub:
# install.packages("devtools")
devtools::install_github("tom-christie/transmit")
```

## Example

### Transmitting and decoding a signal
```r
codebook <- construct_codebook(c('A', 'B', 'C'))
decoded_signal <- transmit_signal(
    codebook=codebook,
    symbol = 'A',
    signal_power = 5,
    noise_power = 10,
    duration_in_seconds = 10,
    time_interval = 0.1,
    entropy_threshold = 0.1,
    return_posterior_at_stop_time = TRUE
    #repeats=1:1000
)
> decoded_signal$stop_time_in_seconds    
[1] 2.6    
> decoded_signal$decoded_symbol
[1] "A"
> decoded_signal$posterior_at_stop_time
[[1]]
[1] 0.995076435 0.001514943 0.003408622
```

### Creating a plot of an entropy trace

```r
plot_entropy_decrease_trace(file_path='~/test.png')
```

<img src="https://github.com/tom-christie/transmit/blob/master/inst/entropy_decrease_example.png?raw=true" width="600"/>



