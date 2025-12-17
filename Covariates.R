################################################################################
### CREATE THE EXTERNAL COVARIATES #############################################
################################################################################

### Get the external data described in Section 2.4 of the paper

### Load libraries
library(jsonlite)
library(openxlsx2)
library(splines)
library(lubridate)

library(tikzDevice)

### Setup for Tikz
textwidth <- 455.24411
options(tikzDocumentDeclaration = '\\documentclass[11pt,a4paper]{scrartcl}',
        tikzLatexPackages =c(getOption('tikzLatexPackages'),'\\usepackage{amssymb}'))


### Load data
load('Verdichtet.RData')
data <- data_V_MZ

time_inter <- range(data$ZAEHLdate)

### Weather-related covariates #################################################

# Precipitation
precipitation <- data.frame(year = NULL, month = NULL, ger = NULL)
for (mon in 1:12) {
  file <- paste0('https://opendata.dwd.de/climate_environment/CDC/regional_averages_DE/monthly/precipitation/regional_averages_rr_', 
                 sprintf('%02d', mon),'.txt')
  
  df <- read.table(file, header = TRUE, comment.char = '#', sep = ';', na.strings = '-999', skip = 1)
  precipitation <- rbind(precipitation, df[df$Jahr %in% c(2019:2023),c('Jahr', 'Monat', 'Deutschland')])
}
precipitation <- precipitation[!(precipitation$year == 2019 & precipitation$month %in% 1:10),] # From 2019 only November and December

# Temperature
temperature <- data.frame(year = NULL, month = NULL, ger = NULL)
for (mon in 1:12) {
  file <- paste0('https://opendata.dwd.de/climate_environment/CDC/regional_averages_DE/monthly/air_temperature_mean/regional_averages_tm_', 
                 sprintf('%02d', mon),'.txt')
  
  df <- read.table(file, header = TRUE, comment.char = '#', sep = ';', na.strings = '-999', skip = 1)
  temperature <- rbind(temperature, df[df$Jahr %in% c(2019:2023),c('Jahr', 'Monat', 'Deutschland')])
}
temperature <- temperature[!(temperature$year == 2019 & temperature$month %in% 1:10),] # From 2019 only November and December

# Sunshine duration
sunshine <- data.frame(year = NULL, month = NULL, ger = NULL)
for (mon in 1:12) {
  file <- paste0('https://opendata.dwd.de/climate_environment/CDC/regional_averages_DE/monthly/sunshine_duration/regional_averages_sd_', 
                 sprintf('%02d', mon),'.txt')
  
  df <- read.table(file, header = TRUE, comment.char = '#', sep = ';', na.strings = '-999', skip = 1)
  sunshine <- rbind(sunshine, df[df$Jahr %in% c(2019:2023),c('Jahr', 'Monat', 'Deutschland')])
}
sunshine <- sunshine[!(sunshine$year == 2019 & sunshine$month %in% 1:10),] # From 2019 only November and December


# Get dates
precipitation$date <- as.Date(paste0(precipitation$year,'-',sprintf('%02d', precipitation$month), '-15'))
precipitation <- precipitation[order(precipitation$date),]
rownames(precipitation) <- NULL

temperature$date <- as.Date(paste0(temperature$year,'-',sprintf('%02d', temperature$month), '-15'))
temperature <- temperature[order(temperature$date),]
rownames(temperature) <- NULL

sunshine$date <- as.Date(paste0(sunshine$year,'-',sprintf('%02d', sunshine$month), '-15'))
sunshine <- sunshine[order(sunshine$date),]
rownames(sunshine) <- NULL

# Daily sequence
dates_daily <- data.frame(date = seq(as.Date('2019-11-01'), as.Date('2023-12-31'), by = 'day'))

# Get mean precipitation
precipitation$mean <- as.numeric(precipitation$ger/table(format(dates_daily, '%Y-%m')))

# Get mean sunshine duration
sunshine$mean <- as.numeric(sunshine$ger/table(format(dates_daily, '%Y-%m')))


### Interpolation with splines

# Precipitation
spline_fit <- spline(x = as.numeric(precipitation$date), y = precipitation$mean, xout = as.numeric(dates_daily$date))

splines_prec <- data.frame(date = as.Date(spline_fit$x, origin = '1970-01-01'),
                           precipation = spline_fit$y)

# Plot
tikz(paste0('figures/plot_precipitation.tex'),
     width = 0.8 * textwidth / 72.27,
     height= 0.55 * textwidth / 72.27)
par(mar = c(2.5,3.5,2,0.5), cex = 0.8, mgp = c(2.5,1,0))
plot(y = rep(precipitation$mean, times = table(format(dates_daily, '%Y-%m'))), 
     x = dates_daily, type = 'l', col = NULL, lwd = 2, 
     ylim = range(c(precipitation$mean, splines_prec$precipation)),
     ylab = 'Mean Daily Precipitation [mm]', xlab = '')
abline(h = axTicks(2), v = axTicks(1), col = 'grey85', lty = 2)
lines(y = rep(precipitation$mean, times = table(format(dates_daily, '%Y-%m'))), 
      x = dates_daily$date, col = 'grey65', lwd = 2)
lines(splines_prec, col = 'darkgreen', lwd = 2)
legend('top', legend = c('Monthly Average', 'Interpolated Daily Values'),
       col = c('grey65', 'darkgreen'),  lwd= 2, bty = 'n',
       inset = c(0, -0.09), xpd = TRUE, horiz = TRUE, cex = 1,
       text.width = strwidth('Interpolated Daily Values')+0.5)
dev.off()

# Temperature
spline_fit <- spline(x = as.numeric(temperature$date), y = temperature$ger, 
                     xout = as.numeric(dates_daily$date))

splines_temp <- data.frame(date = as.Date(spline_fit$x, origin = '1970-01-01'),
                           temperature = spline_fit$y)

tikz(paste0('figures/plot_temperature.tex'),
     width = 0.8 * textwidth / 72.27,
     height= 0.55 * textwidth / 72.27)
par(mar = c(2.5,3.5,2,0.5), cex = 0.8, mgp = c(2.5,1,0))
plot(y = rep(temperature$ger, times = table(format(dates_daily, '%Y-%m'))), 
     x = dates_daily, type = 'l', 
     ylim = range(c(temperature$ger, splines_temp$temperature)),
     ylab = 'Mean Daily Temperature [°C]', xlab = '', col = NULL)
abline(h = axTicks(2), v = axTicks(1), col = 'grey85', lty = 2)
lines(y = rep(temperature$ger, times = table(format(dates_daily, '%Y-%m'))), 
      x = dates_daily$date, col = 'grey65', lwd = 2)
lines(splines_temp, col = 'darkgreen', lwd = 2)
legend('top', legend = c('Monthly Average', 'Interpolated Daily Values'),
       col = c('grey65', 'darkgreen'),  lwd= 2, bty = 'n',
       inset = c(0, -0.09), xpd = TRUE, horiz = TRUE, cex = 1,
       text.width = strwidth('Interpolated Daily Values')+0.5)
dev.off()

# Sunshine duration
spline_fit <- spline(x = as.numeric(sunshine$date), y = sunshine$mean, 
                     xout = as.numeric(dates_daily$date))

splines_sun <- data.frame(date = as.Date(spline_fit$x, origin = '1970-01-01'),
                          sunshine = spline_fit$y)

tikz(paste0('figures/plot_sunshine.tex'),
     width = 0.8 * textwidth / 72.27,
     height= 0.55 * textwidth / 72.27)
par(mar = c(2.5,3.5,2,0.5), cex = 0.8, mgp = c(2.5,1,0))
plot(y = rep(sunshine$mean, times = table(format(dates_daily, '%Y-%m'))), 
     x = dates_daily, type = 'l', col = NULL, lwd = 2, 
     ylim = range(c(sunshine$mean, splines_sun$sunshine)),
     ylab = 'Mean Daily Hours of Sunshine', xlab = '')
abline(h = axTicks(2), v = axTicks(1), col = 'grey85', lty = 2)
lines(y = rep(sunshine$mean, times = table(format(dates_daily, '%Y-%m'))), 
      x = dates_daily$date, col = 'grey65', lwd = 2)
lines(splines_sun, col = 'darkgreen', lwd = 2)
legend('top', legend = c('Monthly Average', 'Interpolated Daily Values'),
       col = c('grey65', 'darkgreen'),  lwd= 2, bty = 'n',
       inset = c(0, -0.09), xpd = TRUE, horiz = TRUE, cex = 1,
       text.width = strwidth('Interpolated Daily Values')+0.5)
dev.off()

### School holidays ############################################################

# Read in data
hol_school <- read.csv2(file = 'data/Ferientage_2016-2023.csv')
hol_school$date <- as.Date(hol_school$date, format = '%d.%m.%Y')

# Restrict on relevant interval
hol_school <- hol_school[hol_school$date >= time_inter[1] & hol_school$date <= time_inter[2], ]

colnames(hol_school)[colnames(hol_school) == 'BR'] <- 'BB'
fed_state <- colnames(hol_school)[5:20]

### Public holidays ############################################################

## Get data
hol_pub <- fromJSON('https://get.api-feiertage.de/?years=2021,2022,2023,2024')$feiertage
hol_pub$date <- as.Date(hol_pub$date)
hol_pub <- hol_pub[!(year(hol_pub$date) == 2024 & hol_pub$date != as.Date('2024-01-01')),]
hol_pub <- hol_pub[,-20:-22]
colnames(hol_pub)[4:19] <- fed_state
hol_pub[,fed_state] <- apply(hol_pub[, fed_state],2, as.numeric)

# 2020
hol_pub_2020 <- fromJSON('https://openholidaysapi.org/PublicHolidays?countryIsoCode=DE&languageIsoCode=DE&validFrom=2020-01-01&validTo=2020-12-31')
hol_pub_2020 <- hol_pub_2020[,c('startDate', 'name', 'nationwide', 'subdivisions')]
hol_pub_2020$name <- unlist(hol_pub_2020$name)[2*1:21]
hol_pub_2020 <- hol_pub_2020[hol_pub_2020$name != 'Friedensfest',]

hol_pub_2020$subdivisions <- sapply(hol_pub_2020$subdivisions, function(df) paste(df[[2]], collapse = ', '))
hol_pub_2020[,fed_state] <- 0
hol_pub_2020$startDate <- as.Date(hol_pub_2020$startDate)


for (i in 1:nrow(hol_pub_2020)) {
  fed_states <- unlist(strsplit(hol_pub_2020$subdivisions[i], split = ', '))
  hol_pub_2020[i,fed_states] <- 1
}

hol_pub_2020[hol_pub_2020$nationwide, fed_state] <- 1
colnames(hol_pub_2020) <- c('date', 'fname', 'nationwide', 'subdivisions', fed_state)

# Merge
hol_pub <- rbind(hol_pub_2020[, c('date', 'fname', fed_state)],
                   hol_pub[, c('date', 'fname', fed_state)])

### Population size ############################################################
pop_size <- read_xlsx(file = 'data/2010130197005_Korr_24112021.xlsx', sheet = 'Tabelle 3.1')
pop_size <- pop_size[10:26,c(1,15)]
colnames(pop_size) <- c('fed_state', 'size')

pop_size$prop <- pop_size$size / 
  pop_size$size[pop_size$fed_state == 'Deutschland']

rownames(pop_size) <- c(fed_state, 'DE')

### Calculate school holiday and public holiday proportions ####################

# School holidays
hol_school$hol_school_prop <- apply(hol_school[,fed_state], 1, function(x) sum(x * pop_size[fed_state, 'prop'], na.rm = TRUE))

# Public holidays
hol_pub$hol_pub_prop <- apply(hol_pub[,fed_state], 1, function(x) sum(x * pop_size[fed_state, 'prop']))

# Create same data structure as for school holidays
hol_pub_fin <- data.frame(date = seq(time_inter[1], time_inter[2], by = 1))
hol_pub_fin[,c(fed_state, 'hol_pub_prop')] <- 0

for (i in seq_along(hol_pub$date)) {
  date <- hol_pub$date[i]
  hol_pub_fin[hol_pub_fin$date == date,] <- hol_pub[i,c('date', fed_state, 'hol_pub_prop')]
}

### Weekdays ###################################################################

time_inter_daily <- seq(time_inter[1], time_inter[2], 'day')

wdays <- data.frame(date = time_inter_daily, 
                    wday = factor(weekdays(time_inter_daily),
                                  levels = c('Monday', 'Tuesday', 'Wednesday', 
                                             'Thursday', 'Friday', 
                                             'Saturday', 'Sunday')))

### Meteorological season ######################################################

seasons <- as.factor(month(time_inter_daily))
levels(seasons) <- c(rep('winter',2), rep('spring', 3),
                     rep('summer',3), rep('autumn',3), 'winter')

seasons <- data.frame(date = time_inter_daily,
                      season = seasons)

### Distance to holidays #######################################################

next_hol <- sapply(time_inter_daily, function(date, hol_pub){
  diff <- hol_pub - date
  return(as.numeric(min(diff[diff >= 0])))
}, hol_pub = hol_pub$date)


last_hol <- sapply(time_inter_daily, function(date, hol_pub){
  diff <- date - hol_pub
  return(as.numeric(min(diff[diff >= 0])))
}, hol_pub = hol_pub$date)

dist_hol <- data.frame(date = time_inter_daily, next_hol = next_hol,
                        last_hol = last_hol)


### Put everything together ####################################################

exo <- list(splines_prec[, c('date', 'precipation')],
            splines_temp[, c('date', 'temperature')],
            splines_sun[, c('date', 'sunshine')],
            hol_school[, c('date', 'hol_school_prop')],
            hol_pub_fin[, c('date', 'hol_pub_prop')],
            wdays[, c('date', 'wday')],
            seasons[, c('date', 'season')],
            dist_hol[, c('date', 'next_hol', 'last_hol')])


exo_cov <- Reduce(function(x, y) merge(x, y, by = 'date', all = TRUE), exo)

save(file = 'data/exo_cov.RData', exo_cov)