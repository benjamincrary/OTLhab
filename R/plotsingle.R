plotSingle <- function(Bands, station, year, criteria) {

  #add feature to read in profiles here, allows plotting samples overtop of bands
  #samples <- profiles %>%
  #  mutate(Date = as.Date(Date, format="%m/%d/%Y")) %>%
  #  mutate(Year = year(Date)) %>%
  #  filter(Year == year & Station == station & Parameter == "Temperature")

  z <- Bands %>%
    filter(Site == station & year == year) %>%
    arrange(date) %>%
    mutate(Band_low = ifelse(Band_low == 0 & Band_high == 0,NA,Band_low),
           Band_high = ifelse(is.na(Band_low) & Band_high == 0, NA,Band_high),
           Hab = ifelse(is.na(Band_high) & is.na(Band_low), "No", "Yes"),
           Habline = ifelse(Hab == "No", max(Band_high, na.rm = T) + 1, Band_high),
           Band_hzoo = zoo::na.locf(zoo::zoo(Band_high)),
           Band_lzoo = zoo::na.locf(zoo::zoo(Band_low)),
           Band_line_low = ifelse(is.na(Band_low),31,Band_low),
           Band_line_high = ifelse(is.na(Band_high),31,Band_high),
           NoHabMark = ifelse(Hab == "No", (Band_hzoo - Band_lzoo)/2 + Band_lzoo, NA))

  mindate <- as.Date(paste0(year, "-05-15"),format= "%Y-%m-%d")
  maxdate <- as.Date(paste0(year, "-10-20"),format= "%Y-%m-%d")
  upperlimit <- ceiling(max(Bands$Band_high, na.rm=T)/10)*10
  breaks <- rev(seq(0, upperlimit, by=10))


  p1 <- ggplot(z) +
    geom_ribbon(aes(x=date, ymin=Band_low, ymax=Band_high), fill="steelblue", alpha=0.1) +
    geom_linerange(aes(x=date, ymin= Band_low, ymax=Band_high),size=2, color="steelblue", alpha=0.5) +
    geom_point(aes(x=date, y=NoHabMark), shape = 4, color="firebrick3") +
    scale_y_reverse(limits= c(upperlimit,0), expand=c(0,0), breaks=breaks) +
    scale_x_date(limits = c(mindate, maxdate),breaks = c(z$date), date_labels = "%b %d") +
    scale_color_manual(guide=FALSE,values=c("firebrick3", "grey80")) +
    xlab("") +
    ylab("Depth (ft)") +
    ggtitle(paste0(station), paste0(year, " (", "Temperature < ", criteria$Temperature, " Deg F & DO > ", criteria$DO," mg/L)")) +
    theme_minimal() +
    theme(panel.background = element_rect(fill="grey98", color="grey98"),
          panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),
          axis.text.x=element_text(angle=90))

  return(p1)


}
