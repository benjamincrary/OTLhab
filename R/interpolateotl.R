#' interpolateOTL
#'
#' @param profiles Dataframe - vertical profiling data. Fields must include c("Lake", "Date", "Station", "Depth_ft", "Parameter", "Value", "Units")
#' @param criteria List - DO and Temperature thresholds. Accepts list output from setCriteria()
#' @param precision numeric precision for vertical interpolation. 0.1 foot recommended.
#'
#' @return
#' @export
#'
#' @examples
interpolateOTL <- function(profiles, criteria, precision) {

  #Verify colnames

  #if(sum(colnames(profile) != c("Lake", "Date", "Station", "Depth_ft", "Parameter", "Value", "Units")) {
  #  stop("Data structure incomplete or contains mislabeled colnames")
  #}

  #Verify Date Format

  #set precision (add default precision into function, allow for custom)
  precision <- precision


  #create activity IDs
  data <- profile %>%
    mutate(ActivityID = paste(Station, "-", Date))

  #define creteria

  DOval <- 6
  Tempval <- 66

  #define output structure
  Act <- character(0)
  Site <- character(0)
  Band_ft <- numeric(0)
  Band_m <- numeric(0)
  Band_low <- numeric(0)
  Band_high <- numeric(0)
  date <- character(0)
  Lake <- character(0)
  ActOut <- data.frame()



  activities <- levels(as.factor(data$ActivityID))
  parameters <- levels(as.factor(data$Parameter))

  #Interpolate each activity

  for (activity in activities) {
    df <- data %>%
      filter(ActivityID == activity)
    Result <- numeric(0)
    for (parameter in parameters) {
      ds <- df %>%
        filter(Parameter == parameter)
      mindepth <- min(ds$Depth_ft)
      maxdepth <- max(ds$Depth_ft)
      depthrange <- seq(mindepth,maxdepth, by=precision)
      valinterp <- approx(ds$Depth_ft, ds$Value, depthrange)
      result <- data.frame(depthrange, valinterp$y)
      colnames(result) <- c('Depth_ft', 'Value_interp')
      result$Parameter <- parameter
      Result <- rbind(Result, result)
    }
    ActResult <- spread(Result, Parameter, Value_interp) %>%
      mutate(Lake = df$Lake[1],
             Date = df$date[1],
             Station = df$Station[1])
    ActBand <- ActResult %>%
      filter(DO > DOval & Temperature < Tempval)
    Act <- c(Act, activity)
    Site <- c(Site, as.character(df$Station[1]))
    Lake <- c(Lake, as.character(df$Lake[1]))
    date <- c(date, as.character(df$Date[1]))
    Band_ft <- c(Band_ft, dim(ActBand)[1]/(1/precision)) #
    Band_m <- c(Band_m, dim(ActBand)[1]/(1/precision)*0.3048) #convert to meters
    low <- ifelse(dim(ActBand)[1] == 0,0,min(ActBand$Depth_ft))
    high <- ifelse(dim(ActBand)[1] == 0, 0, max(ActBand$Depth_ft))
    Band_low <- c(Band_low, low)
    Band_high <- c(Band_high, high)
    ActOut <- rbind(ActOut, ActBand)
  }

  BandRibbon <- data.frame(Lake, date, Site, Band_low, Band_high) %>%
    mutate(date = as.Date(date, format = "%m/%d/%Y"),
           year = year(date))

  BandRibbon$Band_low[BandRibbon$Band_low==0 & BandRibbon$Band_high == 0] <- NA
  BandRibbon$Band_high[is.na(BandRibbon$Band_low) & BandRibbon$Band_high == 0] <- NA

  return(BandRibbon)

}



