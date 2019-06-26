#include <Rcpp.h>

/* Created by Jakob Peder Pettersen 20th of June 2019
 * 
 * Samples lotto series, designed for performance
 */

// [[Rcpp::export]]
Rcpp::IntegerMatrix create_random_series(int n_series, int possible_numbers=34, int numbers_per_series= 7) {
  Rcpp::IntegerMatrix res(numbers_per_series,n_series);
  for(int i = 0; i < n_series; i++){
    res(Rcpp::_, i) = Rcpp::sample(possible_numbers,numbers_per_series,false);
  }
  return res;
}
