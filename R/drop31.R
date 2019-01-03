#' Cutting spurious days from a series with 31 days a month.
#' 
#' Changing a series with 31 days a month to a series with the regular number of observations per month.
#' @param x_ts Input time series in the ts format
#' @param new_start New start date as day of the year. Value from 1 to 366.
#' @param new_end New end date as day of the year. Value from 1 to 366.
#' @author Daniel Ollech
#' @details This function is used internally in dsa()
#' @examples x <- xts::xts(rnorm(1095, 100,1), seq.Date(as.Date("2009-01-01"), length.out=1095, by="days"))
#' a31 <- fill31(x)
#' a <- drop31(a31, 1, 365)
#' @export


drop31 <- function(x_ts, new_start=335, new_end=55) {
  
  a <- paste(expand.grid(1:31, 1:12, stats::start(x_ts)[1]:stats::end(x_ts)[1])$Var3, ifelse(expand.grid(01:31, 1:12, stats::start(x_ts)[1]:stats::end(x_ts)[1])$Var2<9.5, paste("0", expand.grid(01:31, 1:12,stats::start(x_ts)[1]:stats::end(x_ts)[1])$Var2, sep=""),expand.grid(01:31, 1:12, stats::start(x_ts)[1]:stats::end(x_ts)[1])$Var2), ifelse(expand.grid(01:31, 1:12, stats::start(x_ts)[1]:stats::end(x_ts)[1])$Var1<9.5, paste("0", expand.grid(01:31, 1:12, stats::start(x_ts)[1]:stats::end(x_ts)[1])$Var1, sep=""),expand.grid(01:31, 1:12, stats::start(x_ts)[1]:stats::end(x_ts)[1])$Var1) , sep="-")
  
st <- base::as.Date(paste(stats::start(x_ts)[1], new_start), format="%Y %j")
en <- base::as.Date(paste(stats::end(x_ts)[1], new_end), format="%Y %j")

fill_date <- a[grep(st, a):grep(en, a)]

round <- 0
while(length(x_ts) > length(fill_date) & round < 10) {   
  round <- round + 1 ;                                 
  en <- en + 1; # print(paste(en), 1)
  fill_date <- a[grep(st, a):grep(en, a)];  
}

while(length(x_ts) < length(fill_date) & round < 10) {   
  round <- round + 1 ;                                 
  en <- en - 1; # print(paste(en), 1)
  fill_date <- a[grep(st, a):grep(en, a)];  
}

b <- is.na(as.Date(fill_date))
long_xts <- xts::xts(x_ts, order.by=zoo::na.locf(base::as.Date(fill_date)))

long_xts <- long_xts[!b]

long_xts
  
}

