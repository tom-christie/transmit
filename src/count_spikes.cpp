#include <Rcpp.h>
using namespace Rcpp;

// This is a simple example of exporting a C++ function to R. You can
// source this function into an R session using the Rcpp::sourceCpp 
// function (or via the Source button on the editor toolbar). Learn
// more about Rcpp at:
//
//   http://www.rcpp.org/
//   http://adv-r.had.co.nz/Rcpp.html
//   http://gallery.rcpp.org/
//

// [[Rcpp::export]]
IntegerMatrix count_spikes(IntegerVector group_index, 
                           int num_groups, 
                           NumericVector spike_time, 
                           NumericVector timesteps) {
    
    
    IntegerMatrix counts(timesteps.size(),num_groups);
    float timestep_interval = timesteps[2]-timesteps[1]; //so we know which index to insert results into
    
    for(int group=0; group < num_groups; group++){ //r group indexes are 1-indexed
        //go through each spike, checking for group index number, and updating counts accordingly
        for(int observation=0; observation < spike_time.size(); observation++){
            //both spike_time and group_index are the same length
            //so we can use observation to access group_index
            if(group_index[observation] == (group+1)){ //groups are 1-indexed but columns are 0-indexed
                //counts matrix starts out filled with zeros
                //so just go through and increment the appropriate timesteps
                //and then after we will do a cumulative sum operation.
                //fill in up-to with the previous value
                //if this time is bigger than the max (which it COULD be...maybe?) then it could cause an error
                if(int(spike_time[observation]/timestep_interval) < timesteps.size()){
                    counts(int(spike_time[observation]/timestep_interval),group) += 1;
                }
            }
        }
    }
    //now do a cumulative sum
    for(int group=0; group < num_groups; group++){ 
        for(int time=1; time < timesteps.size(); time++){
            counts(time, group) += counts(time-1, group);
        }
    }
    return counts;
}

// You can include R code blocks in C++ files processed with sourceCpp
// (useful for testing and development). The R code will be automatically 
// run after the compilation.
//
// 
