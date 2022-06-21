# Can be used to add hours to a time vector
hrs <- function(u) {
  x <- u * 3600
  return(x)
}

# To add or subtract a year from time stamp
# Can't understand why I can't find a ready R function for this...
# Using this month function from https://stackoverflow.com/questions/14169620/add-a-month-to-a-date

add_months <- function(date, n) {
  seq(date, by = paste (n, "months"), length = 2)[2]
}

