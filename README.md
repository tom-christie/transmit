# transmit 

The aim of this package is to enable simulations of information transmission using Poisson processes and Bayesian inference as described [in this paper](https://github.com/tom-christie/transmit/blob/master/vignettes/Christie_and_Schrater_2019.pdf).



## Installation

```r
# Install the development version from GitHub:
# install.packages("devtools")
devtools::install_github("tom-christie/transmit")
require(transmit)
```

## Example

### Transmitting and decoding a signal
```r
codebook <- construct_codebook(c('A', 'B', 'C', 'D'))
decoded_signal <- transmit_signal(
    codebook=codebook,
    symbol = 'A',
    signal_power = 5,
    noise_power = 10,
    duration_in_seconds = 10,
    time_interval = 0.1,
    entropy_threshold = 0.1,
    return_posterior_at_stop_time = TRUE
)
> decoded_signal$stop_time_in_seconds    
[1] 1.1
> decoded_signal$decoded_symbol
[1] "A"
> decoded_signal$posterior_at_stop_time
[[1]]
[1] 0.9899113279 0.0022606192 0.0001984631 0.0076295897
```

### Creating a plot of an entropy trace

```r
plot_entropy_decrease_trace(file_path='~/test.png')
```

<img src="https://github.com/tom-christie/transmit/blob/master/inst/entropy_decrease_example.png?raw=true" width="600"/>



