#' @title  Plot learning curve
#' @aliases plot_learning_curve
#'
#' @description A function taking both the outputs from \code{estimate_learning_curve()} and \code{train_pred_model_on_subsets()} to display the errorates together in a plot.
#'
#' @param estimate learning_curve The dataframe output of \code{estimate_learning_curve()}.
#' @param train_pred_model_on_subsets The dataframe output of \code{train_pred_model_on_subsets()}.
#' @param title The title for the plot
#'
#' @return A plot displaying the errorcurves.
#'
#' @author Jonathan Krause
#'
#' @seealso \code{estimate_learning_curve()}, \code{train_pred_model_on_subsets()}.
#'
#' @export

plot_learning_curve <- function(estimate_learning_curve, train_pred_model_on_subset, title = "Learning curve"){
  # Check parameters
  if(is.data.frame(estimate_learning_curve) == FALSE){
    stop(paste0("estimate_learning_curve should be of type data.frame, is of type ", typeof(estimate_learning_curve), "."))
  }
  if(is.data.frame(train_pred_model_on_subset) == FALSE){
    stop(paste0("train_pred_model_on_subset should be of type data.frame, is of type ", typeof(train_pred_model_on_subset), "."))
  }
  type_check(title, 'character')

  # Isolate the variables from the dataframes
  pred_size <- estimate_learning_curve$pred_size
  pred_mean <- estimate_learning_curve$pred_mean
  pred_lower <- estimate_learning_curve$pred_lower
  pred_upper <- estimate_learning_curve$pred_upper

  # Generate the Plot
  plot(x = pred_size, y = pred_mean,
              ylim = c(0, max(train_pred_model_on_subset[, 2:4])),
       type = "n",
       xlab = "sample size", ylab = "error rate",
       main = title)

  # Plot the predicted graphs
  lines(pred_size, pred_lower, lwd = 3, col = "blue", lty = 2)
  lines(pred_size, pred_mean, lwd = 4, col = "red")
  lines(pred_size, pred_upper, lwd = 3, col = "blue", lty = 2)

  # Plot trained error rates made by the first function
  points(train_pred_model_on_subset$size, train_pred_model_on_subset$error_mean, col = "red")

  # Generate the legend
  legend('topright', legend = c("75th Quantile", "Mean error", "25th Quantile"),
         col = c("blue", "red", "blue"), lty = c(2,1,2))

}
