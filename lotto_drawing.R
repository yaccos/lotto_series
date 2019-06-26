# Lotto simultor, intended to test performance of comparison algorithms
# Written by Jakob Peder Pettersen on 20th of June 2019
#'
#' @title Lotto simulator
#'
#' @description This program generates random lotto series, compares to a reference and weeds out the incorrect lotto series sequencially
#' 
#' @param n_series Integer, the number of series to compare with
#' 
#' @param possible_numbers Integer, the number of numbers numbers to draw from ( maximal \deqn{N} that any drawn number is among the numbers
#'  \deqn{1,2,...,N}
#'  
#'  @param numbers_per_series Integer, the length of each lotto series
#'
library(magrittr)
library(Rcpp)
sourceCpp('series_sampling.cpp')
lotto_simulator <- function(n_series,  possible_numbers=34, numbers_per_series=7){
  message('Generating random series...')
	series_matrix <- create_random_series(n_series = n_series, possible_numbers=possible_numbers, numbers_per_series=numbers_per_series)
	message('At the start, there are ',n_series,' series')
	correct_series <- sample(x=seq_len(possible_numbers),size= numbers_per_series, replace=FALSE)
	series_remaining <- seq_len(n_series)
	for (i in seq_len(numbers_per_series)){
		correct_number <- correct_series[i]	
		message('Number ',i,' of the correct series is: ',correct_number)
		found_occurences <-  (series_matrix[,series_remaining] == correct_number) %>% which(arr.ind=TRUE)
		series_with_number <- found_occurences[,'col',drop=TRUE] 
		message(length(series_with_number),' of the ', length(series_remaining), ' remaining series had the requested number')
		series_remaining <- series_remaining[series_with_number]
	}	

}

# Did not turn out to be efficient enough, use the C++ code instead
# create_random_series <- function(n_series,possible_numbers=34L, numbers_per_series=7L){
# 	# series_matrix <- replicate(n=n_series, expr=sample(x=seq_len(possible_numbers),size= numbers_per_series, replace=FALSE))
# 	selected_numbers <- matrix(NA,nrow=numbers_per_series,ncol=n_series)
# 	numbers_unselected <- rep(seq_len(possible_numbers),n_series) 
# 	for (i in seq_len(numbers_per_series)){
# 		     n_remaining_numbers <- possible_numbers-i+1L
# 		     selected_indecies <- sample(x=seq_len(n_remaining_numbers),size= n_series,replace=TRUE)
# 		     offset_vector <- seq(from=0L,to=(n_series-1L)*n_remaining_numbers,by=n_remaining_numbers)
# 		     real_selected_indecies <- selected_indecies + offset_vector
# 		     selected_numbers[i,] <- numbers_unselected[real_selected_indecies]
# 		     message('Tic')
#      		     number_unselected <- numbers_unselected[-real_selected_indecies]		
# 	     	     message('Toc')	     
# }
# 	return(selected_numbers)
# }
