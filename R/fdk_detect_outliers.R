#' Detect outliers in a time series
#'
#' The function fills in the existing column to hold outlier flags,
#' and either overwrites the original file or outputs a data structure.
#'
#' @param df dataframe with dates (date) and values (value) to smooth
#' @param iterations number of iterations in order to detect outliers
#' @param sigma number of deviations to exclude outliers at
#' @param plot visualize the process, mostly for debugging
#' (\code{TRUE} / \code{FALSE} = default)
#' @export

fdk_detect_outliers <- function(
    df,
    iterations=20,
    sigma = 2,
    plot = FALSE
    ){

  # specify the double exponential (laplace) distribution
  # standard deviation function, used to screen outliers
  laplace_sd <- function(x,...){
    n <- length(!is.na(x))
    xbar <- mean(x, na.rm = TRUE)
    sqrt(2) * sum(abs(x - xbar),na.rm=T) / n
  }

  # create year column in df / will be removed in new
  # data files as the year column exists
  month = as.numeric(format(as.Date(df$date),"%m"))
  df$year = as.numeric(format(as.Date(df$date),"%Y"))

  # loop over all years to calculate outliers in each year
  for (i in unique(df$year)){

    # subset the data, with padding of a month to accomodate edge effects
    dec = which(month == 12 & df$year == (i - 1) )
    jan = which(month == 1 & df$year == (i + 1) )
    yr = which(df$year == i)
    df_subset = df[c(dec,yr,jan),]

    # create date and greenness vectors
    dates = strptime(df_subset$date,"%Y-%m-%d")
    gcc = df_subset[,which(colnames(df_subset) == "values")]
    gcc_orig = gcc

    # skip year if the gcc vector is (almost) empty
    gcc_length = length(which(gcc != "NA"))
    if (gcc_length <= 3){
      next
    }

    # grab the locations of this years data
    current_year_loc = which(as.numeric(format(dates,"%Y")) == i )

    # calculate the amplitude, and threshold t
    # of the time series
    upper = stats::quantile(gcc,0.9,na.rm=T)
    lower = stats::quantile(gcc,0.1,na.rm=T)
    amp = upper - lower
    t = amp / 5

    # drop NA's, messes with diff()
    gcc_change = c(NA,diff(gcc[!is.na(gcc)]))

    # select days that drop more than 1/4 in amplitude
    selection = which(gcc_change < ( t * -1 ) )

    outliers = rep(0,length(gcc))
    tmp_loc = c() # temporary outlier locations

    # calculate variability (threshold of values to keep)
    # between days
    daily_var = laplace_sd(gcc_change, na.rm=T)

    # if the dialy variability is very small be less
    # restrictive on selecting outliers. Low variability
    # series have little structure, removing outliers very
    # stringently imposes artificial patterns. Basically
    # these series are ~flat and should remain so.
    sigma_current = sigma

    # set iterator for while loop
    j = 1
    while (j < iterations){

      if (j == 1){
        gcc[selection] = NA
      }

      # calculate optimal span / smoothing factor
      span = suppressWarnings(fdk_optimal_span(gcc, step = 0.01))

      # remove old projections
      if ( exists('pred') ){
        rm('pred')
      }

      if (is.null(span)) {
        gcc_smooth = gcc
      } else {

        # calculate fit using the optimal span / smoothing factor
        fit = suppressWarnings(stats::loess(gcc ~ as.numeric(dates), span = span))

        # predict values using the fit
        pred = suppressWarnings(stats::predict(fit,as.numeric(dates), se = TRUE))

        # loess data
        gcc_smooth = pred$fit
      }

      # get the difference
      gcc_dif = gcc_orig - gcc_smooth

      # calculate outliers (up or down), change direction of the
      # assymetrical criteria for the GRVI (outliers are upward)
      loc_up = which(gcc_dif > 3 * sigma_current * daily_var )
      loc_down = which(gcc_dif <=  sigma_current * -daily_var )
      loc = c(loc_up,loc_down)

      # only retain last iteration values
      # in the next iteration
      gcc = gcc_orig # reset to original values
      gcc[loc] = NA # remove current outliers
      outliers[loc] = 1
      outliers[-loc] = 0

      # break conditions (when no change is detected),
      # if not met update the temporary location vector
      if (sum(tmp_loc - loc) == 0 & j != 1){
        break # exit while loop
      }else{

        # visualize iterations, for debugging
        if ( plot == TRUE ){
          graphics::par(mfrow=c(1,1))
          plot(dates,gcc_orig)
          graphics::points(dates[loc],gcc_orig[loc],col='red',pch=19)

          if ( exists('pred') ){
            graphics::lines(dates,pred$fit)
          }
          Sys.sleep(1)
        }

        # overwrite previous locations with
        # the new ones (kept for the next
        # iteration)
        tmp_loc = loc
      }

      # increase the counter, go again...
      j = j + 1
    }

    # put everything back into the dataframe
    df$outlierflag[df$year == i] <- outliers[current_year_loc]

  } # loop over years

  return(df)
}
