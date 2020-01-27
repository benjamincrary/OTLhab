# OTLhab

The goal of OTLhab is to estimate oxythermal habitat in lakes using temperature and dissolved oxygen profiling data. 

## Installation (not yet live)

``` r
devtools::install_github("benjamincrary/OTLhab")
```

## Example


``` r
## basic example code

#Load example data
data(lco)

#Set criteria for DO minimum and Temperature maximum
myCriteria <- setCriteria(6,66)

#Define vertical OTL bands. Interpolate over 0.1 feet. 
Bands <- interpolateOTL(lco, myCriteria, 0.1)

#Plot OTL bands at Station "LCO2" for 2016 season
LCO2_2016 <- plotSingle(Bands, "LCO2", 2016)

#Save plot 
LCO2_2016 + ggsave("pathtofile/output.pdf") 

#Plot OTL bands for multiple stations and multiple seasons
majorbasins <- plotMultiple(plotMultiple(Bands, c("LCO2", "LCO3", "LCO4"), c(2015,2016,2017,2018),myCriteria))


```

