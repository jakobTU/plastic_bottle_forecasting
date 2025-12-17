################################################################################
### DETERMINE THE BEST FOURIER ORDER FOR EACH MATERIAL #########################
################################################################################

# Load libraries 
library(forecast)
library(lubridate)
library(matrixStats)

# Load data
load('Verdichtet.RData')
data <- data_V_MZ

# Setup for Tikz
textwidth <- 455.24411

library(tikzDevice)

options(tikzDocumentDeclaration = '\\documentclass[11pt,a4paper]{scrartcl}',
        tikzLatexPackages =c(getOption('tikzLatexPackages'),'\\usepackage{amssymb}'))

colors <- c('#5f4690', '#1d6996', '#38a6a5', '#0f8554', '#73af48', '#edad08', '#cc503e')

### Fourier order with lowest AICc

# Function for calculating k with the lowest AICc
get_bestK <- function(TS){
  max_K <- floor(frequency(TS)/2)
  aicc_k <- numeric(max_K)

  for(k_value in 1:max_K) {
    fourierP <- fourier(TS, K = k_value)
    mod <- tslm(TS ~ trend + fourierP)
    aicc_k[k_value] <- CV(mod)[3]
  }
  return(which(aicc_k == min(aicc_k)))
}

# Determine best k
best_k <- numeric(length(levels(data$MATERIALART)))
for (i in seq_along(levels(data$MATERIALART))) {
  data_material <- data[data$MATERIALART == levels(data$MATERIALART)[i], c('ZAEHLDATUM', 'ANZAHL_GEBINDE')]
  ts_material <- ts(data_material$ANZAHL_GEBINDE[order(data_material$ZAEHLDATUM)], frequency = 251)
  best_k[i] <- get_bestK(ts_material)
}
names(best_k) <- levels(data$MATERIALART)
save(best_k, file = 'best_k.RData')


### Plot

label_m <- data.frame(de = levels(data$MATERIALART),
                      en = c('Aluminium', 'Glass', 'PET with Nylon',
                             'PET', 'Plastics', 
                             'Steel', 'Unknown'))

fourier_plot <- function(material, best_k, tikzFile = FALSE){
  
  if(tikzFile){
    tikz(paste0('figures/fourier/fourier_order_', material,'.tex'), 
         width = 0.5 * textwidth / 72.27,
         height= 0.45* textwidth / 72.27)
  }

  data_material <- data[data$MATERIALART == material, c('ZAEHLDATUM', 'ANZAHL_GEBINDE')]
  TS_here <- ts(data_material$ANZAHL_GEBINDE[order(data_material$ZAEHLDATUM)], start = c(2020,2), frequency = 251)
  
  best_k_here <- best_k[material]
  calc_fourier <- fourier(TS_here, K = best_k_here)
  fourier_here <- fitted(tslm(TS_here ~ trend + calc_fourier))
  
  par(mar = c(3,4,1.5,0.5), cex = 0.8, mgp = c(2.5,1,0))
  plot(NA, xlim = range(time(TS_here)), ylim = range(c(TS_here, fourier_here)), xlab = '', 
       ylab = 'Quantity of Containers')
  abline(h = axTicks(2), v = axTicks(1), col = 'grey85', lty = 2)
  
  lines(TS_here, col = 'grey50')
  lines(fourier_here, col = colors[1], lwd = 4)
  legend('topleft', legend = c('Original Data', paste('Fourier with k =', best_k_here)), 
        col = c('grey50', colors[1]), lwd = c(1,2), bty = 'n')
  if(tikzFile){
    dev.off()
  }
}

### Generate plots
for (i in levels(data$MATERIALART)) {
  fourier_plot(i, best_k = best_k)
}

### Tikz
for (i in levels(data$MATERIALART)) {
  fourier_plot(i, best_k = best_k, tikzFile = TRUE)
}
