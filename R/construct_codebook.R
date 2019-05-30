#' Construct a codebook object that maps symbols to neurons
#'
#' Some other stuff here.  
#'
#' @param symbols list of strings, representing symbols to be encoded
#' @param neuron_allocation a vector of counts, i.e. how many neurons to associate with each symbol.
#' 
#' @return 
#' 
#' 
#' @example
#' 
#' 
#' 
#' @export



construct_codebook <- function(symbols,
                               neuron_allocation = c()) {
    ## symbols: a vector of strings
    ## neuron_allocation: a vector of counts, i.e. how many neurons to associate with each symbol.
    ##      If NA, one will be allocated to each
    
    ## In the encoder, the signal power will be spread across all neurons associated with the given symbol
    ## So the count distribution effectively indicates how noise will be distributed...
    
    ## What's the best way to allocate???
    ## We want the confusability of the messages to be as low as possible
    ## Each message will end up as an expected count vector, with Gaussian-ish noise (not mean 0!)
    ## The allocation essentially specifies noise in each of the dimensions
    ## This is similar to the 'sphere-packing' argument for Gaussian but everything is positive
    
    require(assertthat)
    
    if (length(neuron_allocation) < 1) {
        neuron_allocation <- rep.int(1, length(symbols))
    } else{
        assert_that(length(symbols) == length(neuron_allocation))
    }
    
    codebook <- list()
    for (i in 1:length(symbols)) {
        codebook[[symbols[i]]] <- list(index = i,
                                       size = neuron_allocation[i],
                                       symbol = symbols[i])
    }
    return(codebook)
}