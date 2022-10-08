#' @title Estimate learning curve
#' @aliases estimate_learning_cuve
#' @description This function takes the ouput form \code{train_pred_model_on_subsets()} and a sample size up to which the errorates should be predicted, to be later displayed with \code{plot_learning_curve()}.
#'
#' @param output Takes the dataframe from \code{train_pred_model_on_subsets()} to perform an estimation of the learning curve.
#' @param sample_size An integer defining up to which sample size the errorrate should be predicted.
#' @param chance A double defining what the maximum pvalue for the errorate that are used should be (Default: chance = 0.05)
#'
#' @return A dataframe containing the samplesizes which were predicted and the predicted mean and 25/75 % quantile errorates for these samplesizes.
#'
#' @author Jonathan Krause
#'
#' @seealso \code{train_pred_model_on_subsets()}, \code{plot_learning_curve()}.
#'
#' @export

estimate_learning_curve <- function(output, sample_size, chance = 0.05){
  # Test paramters
  if(is.data.frame(output) == FALSE){
    stop(paste0("output should be of type data.frame, is of type ", typeof(output), "."))
  }
  type_check(sample_size, 'double')
  type_check(chance, 'double')
  if(sample_size <= 0){
    stop('sample_size should be larger than zero')
  }
  if(sample_size < max(output$size)){
    stop('sample_size has to be larger than the original group size')
  }
  if(chance <= 0 | chance > 1){
    stop('chance should be between zero and one')
  }

  # Extract significant sample sizes
  error <- subset(output, pvalue <= chance)
  if(nrow(error) <= 3){
    warning('Only 3 or less groupsizes fit the set chancevalue.')
  }

  # Fit the learning curve
  estimate_mean <- fit_learning_curve(x = error$size, y = error$error_mean)
  estimate_lower <- fit_learning_curve(x = error$size, y = error$error_lower_quant)
  estimate_upper <- fit_learning_curve(x = error$size, y = error$error_upper_quant)

  # Get the values at which the errors should be predicted
  pred_size <- c(error$size , seq(max(error$size), sample_size, (sample_size - max(error$size)) / 10))

  # Predict for larger sample sizes
  pred_mean <- predict(estimate_mean, list(x = pred_size))
  pred_lower <- predict(estimate_lower, list(x = pred_size))
  pred_upper <- predict(estimate_upper, list(x = pred_size))

  # Construct the return dataframe
  learn_curve <- data.frame(pred_size, pred_mean, pred_lower, pred_upper)
  return(learn_curve)
}
