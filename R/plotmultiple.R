#' Title
#'
#' @param Bands Dataframe - ouput of interpolateOTL
#' @param station Character Vector - names of stations within Bands to plot
#' @param year Numeric Vector - years to plot
#' @param criteria List - output of setCriteria
#'
#' @return
#' @export
#' @importFrom magrittr %>%
#' @import ggplot2
#' @import dplyr
#' @import zoo
#' @import lubridate
#'
#' @examples
plotMultiple <- function(Bands, stations, years, criteria) {

  z <- Bands %>%
    filter(Site %in% stations & year %in% years) %>%
    arrange(date) %>%
    group_by(Site, year) %>%
    mutate(Band_low = ifelse(Band_low==0 & Band_high ==0,NA,Band_low),
           Band_high = ifelse(is.na(Band_low) & Band_high ==0, NA,Band_high),
           Hab = ifelse(is.na(Band_high) & is.na(Band_low), "No", "Yes"),
           Habline = ifelse(Hab=="No", max(Band_high, na.rm=T)+1, Band_high),
           Band_hzoo = zoo::na.locf(zoo::zoo(Band_high)),
           Band_lzoo = zoo::na.locf(zoo::zoo(Band_low)),
           Band_line_low = ifelse(is.na(Band_low),31,Band_low),
           Band_line_high = ifelse(is.na(Band_high),31,Band_high),
           NoHabMark = ifelse(Hab=="No", (Band_hzoo-Band_lzoo)/2+Band_lzoo, NA),
           PlotDate = as.Date(paste("2015", lubridate::month(date), lubridate::day(date), sep="-"), format="%Y-%m-%d"))


  mindate <- as.Date(paste0(2015, "-05-15"),format= "%Y-%m-%d")
  maxdate <- as.Date(paste0(2015, "-10-20"),format= "%Y-%m-%d")
  upperlimit <- ceiling(max(Bands$Band_high, na.rm=T)/10)*10
  breaks <- rev(seq(0, upperlimit, by=10))



  p <- ggplot(z) +
    geom_ribbon(aes(x=PlotDate, ymin=Band_low, ymax=Band_high), fill="steelblue", alpha=0.1) +
    geom_linerange(aes(x=PlotDate, ymin= Band_low, ymax=Band_high), color="steelblue", alpha=0.5) +
    geom_point(aes(x=PlotDate, y=NoHabMark), shape = 4, color="firebrick3") +
    facet_grid(Site~year, scales="free_x") +
    scale_y_reverse(limits= c(upperlimit,0), expand=c(0,0), breaks=breaks) +
    scale_x_date(limits = c(mindate, maxdate), date_labels = "%b %d") +
    scale_color_manual(guide=FALSE,values=c("firebrick3", "grey80")) +
    ggtitle(paste0("OTL"), paste0("(Temperature < ", criteria$Temperature, " Deg F & DO > ", criteria$DO," mg/L)")) +
    xlab("") +
    ylab("Depth (ft)") +
    theme_minimal() +
    theme(panel.background = element_rect(fill="grey98", color="grey98"),
          panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),
          panel.spacing=unit(1,"lines"),
          axis.text.x=element_text(angle=90))

  return(p)

  #, paste0(year, " (", "Temperature < ", criteria$Temperature, " Deg F & DO > ", criteria$DO," mg/L)")) +


}
