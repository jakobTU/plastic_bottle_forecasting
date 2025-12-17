################################################################################
### CHECK THE CROSS-CORRELATION ################################################
################################################################################

### Perform the cross-correlation analysis described in Section 4.1 of the paper

### Load libraries
library(tikzDevice)
library(scales)

### Load data
load('Verdichtet.RData')
load('best_k.RData')
load('exo_cov.RData')

data <- data_V_MZ

### Setup for Tikz
textwidth <- 455.24411
options(tikzDocumentDeclaration = '\\documentclass[11pt,a4paper]{scrartcl}',
        tikzLatexPackages =c(getOption('tikzLatexPackages'),'\\usepackage{amssymb}'))

new_colors <- c('#f1a208', '#d35f6a', '#2d7ea0')

targetVariable <- 'ANZAHL_GEBINDE_NMZ'

sunshine_lag <- data.frame(material = levels(data$MATERIALART), lag = NA)
temperature_lag <- data.frame(material = levels(data$MATERIALART), lag = NA)
precipation_lag <- data.frame(material = levels(data$MATERIALART), lag = NA)

### Small figures
for (i in 1:7) {
  material <- levels(data$MATERIALART)[i]
  tikz(paste0('figures/cross_cor/cor_', material, '_small.tex'),
       width = 0.5* textwidth / 72.27,
       height= 0.5*0.75 * textwidth / 72.27)

  data_material <- data[data$MATERIALART == material, c('ZAEHLDATUM', targetVariable)]
  ts_material <- ts(data_material[order(data_material$ZAEHLDATUM), targetVariable])
  
  exo <- exo_cov[exo_cov$Datum %in% data_material[order(data_material$ZAEHLDATUM), 'ZAEHLDATUM'],]
  
  crossCorr_sun <- ccf(ts_material, ts(exo$sunshine), lag.max = 251,
                       plot = FALSE)
  crossCorr_temp <- ccf(ts_material, ts(exo$temperature), lag.max = 251,
                        plot = FALSE)
  crossCorr_prec <- ccf(ts_material, ts(exo$precipation), lag.max = 251,
                        plot = FALSE)
  
  par(mar = c(3.6,3.5,1.5,0.5), cex = 0.8, mgp = c(2.5,1,0))
  plot(NA, NA, xlab = 'Lag', ylab = 'Cross-Correlation', main = '', xlim = c(0,251), 
       ylim = c(-0.65,0.7))
  abline(h = axTicks(2), v = axTicks(1), col = 'grey85', lty = 2)
  
  points(crossCorr_sun$lag[252:503], crossCorr_sun$acf[252:503], type = 'h', col = alpha(new_colors[1], 0.8))
  points(crossCorr_temp$lag[252:503], crossCorr_temp$acf[252:503], type = 'h', col = alpha(new_colors[2], 0.8))
  points(crossCorr_prec$lag[252:503], crossCorr_prec$acf[252:503], type = 'h', col = alpha(new_colors[3], 0.8))
  
  abline(h = 0, col = 'grey15')
  
  sunshine_lag$lag[i] <- crossCorr_sun$lag[252:503][which.max(abs(crossCorr_sun$acf[252:503]))]
  abline(v = sunshine_lag$lag[i], col = new_colors[1], lwd = 2)
  
  temperature_lag$lag[i] <- crossCorr_temp$lag[252:503][which.max(abs(crossCorr_temp$acf[252:503]))]
  abline(v = temperature_lag$lag[i], col = new_colors[2], lwd = 2)
  
  precipation_lag$lag[i] <- crossCorr_prec$lag[252:503][which.max(abs(crossCorr_prec$acf[252:503]))]
  abline(v = precipation_lag$lag[i], col = new_colors[3], lwd = 2)
  

  legend('bottomright', legend = c(paste0('Lag = ', sunshine_lag$lag[i]),
                                   paste0('Lag = ', temperature_lag$lag[i]),
                                   paste0('Lag = ', precipation_lag$lag[i])), 
         text.col = new_colors, bty = 'n', cex=0.8, text.width = strwidth('Lag = 999'))
  dev.off()
}


# Add the legend
tikz('figures/cross_cor/legend.tex',
     width = 1 * textwidth / 72.27,
     height = 0.025 * textwidth / 72.27)
par(mar = c(0, 4, 0, 0), cex = 0.9)
plot(NA, xlim = c(1,20), ylim = 0:1, xlab = '', ylab = '', axes = FALSE)
legend('center', legend = c('Sunshine Duration', 'Temperature', 'Precipitation'),
       col = new_colors, pch = 16, bty = 'n',
       inset = c(0, 0), xpd = TRUE, horiz = TRUE, cex = 1,
       text.width = strwidth('Sunshine Duration')+1)
dev.off()


## Figure for PET

material <- levels(data$MATERIALART)[4]
tikz(paste0('figures/cross_cor/cor_', material, '.tex'),
     width = 0.8* textwidth / 72.27,
     height= 0.55 * textwidth / 72.27)

data_material <- data[data$MATERIALART == material, c('ZAEHLDATUM', targetVariable)]
ts_material <- ts(data_material[order(data_material$ZAEHLDATUM), targetVariable])

exo <- exo_cov[exo_cov$Datum %in% data_material[order(data_material$ZAEHLDATUM), 'ZAEHLDATUM'],]

crossCorr_sun <- ccf(ts_material, ts(exo$sunshine), lag.max = 251,
                     plot = FALSE)
crossCorr_temp <- ccf(ts_material, ts(exo$temperature), lag.max = 251,
                      plot = FALSE)
crossCorr_prec <- ccf(ts_material, ts(exo$precipation), lag.max = 251,
                      plot = FALSE)

par(mar = c(3.6,3.5,1.5,0.5), cex = 0.8, mgp = c(2.5,1,0))
plot(NA, NA, xlab = 'Lag', ylab = 'Cross-Correlation', main = '', xlim = c(0,251), 
     ylim = range(crossCorr_sun$acf, crossCorr_temp$acf, crossCorr_prec$acf))
abline(h = axTicks(2), v = axTicks(1), col = 'grey85', lty = 2)

points(crossCorr_sun$lag[252:503], crossCorr_sun$acf[252:503], type = 'h', col = alpha(new_colors[1], 0.8))
points(crossCorr_temp$lag[252:503], crossCorr_temp$acf[252:503], type = 'h', col = alpha(new_colors[2], 0.8))
points(crossCorr_prec$lag[252:503], crossCorr_prec$acf[252:503], type = 'h', col = alpha(new_colors[3], 0.8))

abline(h = 0, col = 'grey15')

sunshine_lag$lag[i] <- crossCorr_sun$lag[252:503][which.max(abs(crossCorr_sun$acf[252:503]))]
abline(v = sunshine_lag$lag[i], col = new_colors[1], lwd = 2)

temperature_lag$lag[i] <- crossCorr_temp$lag[252:503][which.max(abs(crossCorr_temp$acf[252:503]))]
abline(v = temperature_lag$lag[i], col = new_colors[2], lwd = 2)

precipation_lag$lag[i] <- crossCorr_prec$lag[252:503][which.max(abs(crossCorr_prec$acf[252:503]))]
abline(v = precipation_lag$lag[i], col = new_colors[3], lwd = 2)

legend('topleft', legend = c('Sunshine Duration', 'Temperature', 'Precipitation'),
       col = new_colors, pch = 16, bty = 'n',
       inset = c(0, -0.1), xpd = TRUE, horiz = TRUE, cex = 1,
       text.width = strwidth('Sunshine Duration')+30)
legend('bottomright', legend = c(paste0('Lag = ', sunshine_lag$lag[i]),
                                 paste0('Lag = ', temperature_lag$lag[i]),
                                 paste0('Lag = ', precipation_lag$lag[i])), 
       text.col = new_colors, bty = 'n', text.width = strwidth('Lag = 999')+3)
dev.off()

