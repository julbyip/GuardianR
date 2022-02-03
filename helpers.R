HR_mean_plot <- function(data, imputation, info){
  df <- data
  
  if (imputation == "Mean") {
        df$HR_mean[is.na(df$HR_mean)] <- mean(df$HR_mean, na.rm = T)
      }
  else if (imputation == "Mode") {
        df$HR_mean[is.na(df$HR_mean)] <- mode(df$HR_mean)
  }
  
  out <- ggplot(df, aes(x=StartTimeHR, y=HR_mean)) + geom_line() + geom_point()
  out <- out + theme_light() + scale_x_datetime(date_labels="%H:%M:%S")
  out <- out + ggtitle(paste("HR_mean of ", info[1]),
                 subtitle = paste("Ratio of NA: ", info[4]))

  return(out)
}
  