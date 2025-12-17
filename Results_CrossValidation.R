################################################################################
### GENERATE PLOTS FOR THE FORECAST HORIZONS ###################################
################################################################################

### Generate the plots in Figure 1 and 2 of the paper

# Load libraries
library(matrixStats)
library(tikzDevice)
library(scales)

# Load data
load('Verdichtet.RData')
data <- data_V_MZ
rm(data_V)
rm(data_V_MZ)

# Setup for graphics
colors <- c('#5f4690', '#1d6996', '#38a6a5', '#0f8554', '#73af48', '#edad08', '#cc503e')

colors_model <- colors
names(colors_model) <- c('naive', 'ARIMA', 'DHR', 'TBATS', 'rf', 'xgb', 'LSTM')

# Setup for Tikz
textwidth <- 455.24411
options(tikzDocumentDeclaration = '\\documentclass[11pt,a4paper]{scrartcl}',
        tikzLatexPackages =c(getOption('tikzLatexPackages'),'\\usepackage{amssymb}', '\\usepackage{amsmath}' ))


# Load the results
all_files <- list.files('cv_results/residuals', full.names = TRUE)
all_names <- gsub('cv_results/residuals/res_', '', gsub('.RData', '', all_files))

n_files <- length(all_files)
MSE_tab <- data.frame(model = rep(NA, n_files),
                      material = rep(NA, n_files),
                      h = rep(NA, n_files),
                      MSE = rep(NA, n_files))

for (i in seq_along(all_files)) {

  identifier <- unlist(strsplit(all_names[i], '_'))

  if(length(identifier)==4){
    identifier <- identifier[-2]
  }

  MSE_tab[i,1:3] <- identifier

  load(all_files[i])
  residuals <- as.matrix(residuals)

  MSE_tab[i,4] <- mean(residuals[,ncol(residuals)]^2)
}
rm(residuals)
rm(identifier)

MSE_tab$model <- factor(MSE_tab$model, levels = c('naive', 'ARIMA', 'DHR', 'TBATS', 'rf', 'xgb', 'LSTM'))
MSE_tab$material <- factor(MSE_tab$material)
MSE_tab$h <- as.numeric(MSE_tab$h)

# Plot for single material
label_m <- data.frame(de = levels(MSE_tab$material),
                             en = c('Aluminium', 'Glass', 'Plastics', 'PET with Nylon',
                                    'Polyethylene Terephthalate', 'Steel', 'Unknown'))

for (material in levels(MSE_tab$material)) {
  tikz(paste0('figures/mse/mse_', material, '_h.tex'),
       width = 0.8 * textwidth / 72.27,
       height= 0.55 * textwidth / 72.27)

  MSE_material <- MSE_tab[MSE_tab$material == material,]
  
  par(mar = c(4,4,2,0.5), cex = 0.8, mgp = c(2.5,1,0))
  plot(NULL, type = 'b', ylab = 'MSE$(h)$', xlab = 'Forecast Horizon $h$', 
       col = colors[1], pch = 16, xlim = c(1,20), ylim = range(MSE_material$MSE))
  abline(h = axTicks(2), v = axTicks(1), col = 'grey85', lty = 2)
  
  models <- levels(MSE_material$model)
  
  for(i in seq_along(models)){
    MSE_model <- MSE_material[MSE_material$model == models[i], ]
    MSE_model <- MSE_model[order(MSE_model$h),]
    
    points(x = MSE_model$h, y = MSE_model$MSE, col = colors[i],
           type = 'b', pch = 16)
    
  }
  
  legend('top', legend = toupper(models),
         col = colors, pch = 16, bty = 'n', 
         inset = c(0, -0.1), xpd = TRUE, horiz = TRUE, cex = 0.85,
         text.width = strwidth('TBATS')+0.5)
  dev.off()
}

# Small plots
for (material in levels(MSE_tab$material)) {
  tikz(paste0('figures/mse/mse_', material, '_h_small.tex'),
       width = 0.5* textwidth / 72.27,
       height= 0.5*0.75 * textwidth / 72.27)
  
  MSE_material <- MSE_tab[MSE_tab$material == material,]
  
  par(mar = c(4,4,2,0.5), cex = 0.7, mgp = c(2.5,1,0))
  plot(NULL, type = 'b', ylab = 'MSE$(h)$', xlab = 'Forecast Horizon $h$', 
       col = colors[1], pch = 16, xlim = c(1,20), ylim = range(MSE_material$MSE))
  abline(h = axTicks(2), v = axTicks(1), col = 'grey85', lty = 2)
  
  models <- levels(MSE_material$model)
  
  for(i in seq_along(models)){
    MSE_model <- MSE_material[MSE_material$model == models[i], ]
    MSE_model <- MSE_model[order(MSE_model$h),]
    
    points(x = MSE_model$h, y = MSE_model$MSE, col = colors[i],
           type = 'b', pch = 16)
    
  }
  dev.off()
}


# Add the legend
tikz('figures/mse/legend.tex',
     width = 1 * textwidth / 72.27,
     height = 0.025 * textwidth / 72.27)
par(mar = c(0, 4, 0, 0), cex = 0.9)
plot(NA, xlim = c(1,20), ylim = 0:1, xlab = '', ylab = '', axes = FALSE)
legend('center', legend = toupper(models), horiz = TRUE,
       col = colors, pch = 16, bty = 'n', cex = 0.9, xpd = TRUE,
       text.width = strwidth('TBATS')+0.5)
dev.off()


## Table for best model:

sums <- aggregate(MSE~model+material, sum, data = MSE_tab)
sums_mins <- aggregate(MSE ~ material, data = sums, min)

res <- sums[sums$MSE %in% sums_mins$MSE,]
cbind(res[,1:2], round(res$MSE, 3))

mins <- aggregate(MSE~model+material, min, data = MSE_tab)
maxs <- aggregate(MSE~model+material, max, data = MSE_tab)



cbind(mins[,1:2], round(mins$MSE, 3))
cbind(maxs[,1:2], round(maxs$MSE, 3))

## Save Best MSE for later use:

best <- c('DHR', 'ARIMA', 'LSTM', 'TBATS', 'DHR', 'DHR', 'DHR')
names(best) <- levels(data$MATERIALART)

Best_MSE <- lapply(levels(data$MATERIALART), 
                   function(material){
                     Best_MSE <- MSE_tab[MSE_tab$material == material & 
                                         MSE_tab$model == best[material],]
                     return(Best_MSE[order(Best_MSE$h),])
                   })

names(Best_MSE) <- levels(data$MATERIALART)

# Models with exogenous covariates #############################################

# Load results
all_files <- list.files('cv_results/residuals_x', full.names = TRUE)
all_names <- gsub('cv_results/residuals_x/res_', '', gsub('.RData', '', all_files))

n_files <- length(all_files)
MSE_tab_exo <- data.frame(model = rep(NA, n_files),
                          material = rep(NA, n_files),
                          h = rep(NA, n_files),
                          MSE = rep(NA, n_files))

## Dateien Einlesen
for (i in seq_along(all_files)) {
  
  identifier <- unlist(strsplit(all_names[i], '_'))
  
  if(length(identifier)==5){
    identifier <- identifier[-3]
  }
  
  identifier <- identifier[-2]
  
  MSE_tab_exo[i,1:3] <- identifier
  
  load(all_files[i])
  residuals <- as.matrix(residuals)
  
  MSE_tab_exo[i,4] <- mean(residuals[,ncol(residuals)]^2)
}
rm(residuals)
rm(identifier)

MSE_tab_exo$model <- factor(MSE_tab_exo$model, levels = c('naive', 'ARIMA', 'DHR', 'TBATS', 'rf', 'xgb', 'LSTM'))
MSE_tab_exo$material <- factor(MSE_tab_exo$material)
MSE_tab_exo$h <- as.numeric(MSE_tab_exo$h)

# Plot for single material
for (material in levels(MSE_tab_exo$material)) {
  tikz(paste0('figures/mse/mse_', material, '_h_Exo.tex'),
       width = 0.8 * textwidth / 72.27,
       height= 0.55 * textwidth / 72.27)
  
  MSE_material <- MSE_tab_exo[MSE_tab_exo$material == material,]
  MSE_material_old <- MSE_tab[MSE_tab$material == material,]
  
  MSE_material_old <- MSE_material_old[MSE_material_old$model %in% c('DHR', 'rf', 'xgb'),]
  
  par(mar = c(4,4,2,0.5), cex = 0.8, mgp = c(2.5,1,0))
  plot(NULL, type = 'b', ylab = 'MSE$(h)$', xlab = 'Forecast Horizon $h$', 
       col = colors[1], pch = 16, xlim = c(1,20), 
       ylim = range(c(MSE_material$MSE, Best_MSE[[material]]$MSE, MSE_material_old$MSE)))
  abline(h = axTicks(2), v = axTicks(1), col = 'grey85', lty = 2)
  
  models <- levels(MSE_material$model)
  
  for(i in seq_along(models)){
    MSE_model <- MSE_material[MSE_material$model == models[i], ]
    MSE_model <- MSE_model[order(MSE_model$h),]
    
    
    MSE_model_old <- MSE_material_old[MSE_material_old$model == models[i], ]
    MSE_model_old <- MSE_model_old[order(MSE_model_old$h),]
    
    points(x=MSE_model_old$h, y = MSE_model_old$MSE, col = alpha(colors[i], 0.5),
           type = 'b', pch = 24)
    
    points(x = MSE_model$h, y = MSE_model$MSE, col = colors[i],
           type = 'b', pch = 16)
  }
  
  
  points(x = Best_MSE[[material]]$h, y = Best_MSE[[material]]$MSE,
         col = alpha(colors_model[best[material]], 0.5), type = 'b', pch = 24, 
         bg = alpha(colors_model[best[material]], 0.75))

  legend('top', legend = toupper(models),
         col = colors, pch = 16, bty = 'n', 
         inset = c(0, -0.1), xpd = TRUE, horiz = TRUE, cex = 0.85,
         text.width = strwidth('TBATS')+0.5)
  
  legend('topleft', pch = c(16,24,17), legend = c('Models with Covariates',
                                                  'Models without Covariates',
                                                  'Best Model without Covariates'),
         bty = 'n', col = 'grey30', text.col = 'grey30')
  
  dev.off()
}


# Small Plots
for (material in levels(MSE_tab$material)) {
  tikz(paste0('figures/mse/mse_', material, '_h_small_exo.tex'),
       width = 0.5* textwidth / 72.27,
       height= 0.5*0.75 * textwidth / 72.27)
  
  MSE_material <- MSE_tab_exo[MSE_tab_exo$material == material,]
  MSE_material_old <- MSE_tab[MSE_tab$material == material,]
  
  MSE_material_old <- MSE_material_old[MSE_material_old$model %in% c('DHR', 'rf', 'xgb'),]
  
  par(mar = c(4,4,2,0.5), cex = 0.7, mgp = c(2.5,1,0))
  plot(NULL, type = 'b', ylab = 'MSE$(h)$', xlab = 'Forecast Horizon $h$', 
       col = colors[1], pch = 16, xlim = c(1,20), 
       ylim = range(c(MSE_material$MSE, Best_MSE[[material]]$MSE, MSE_material_old$MSE)))
  abline(h = axTicks(2), v = axTicks(1), col = 'grey85', lty = 2)
  
  models <- levels(MSE_material$model)
  
  for(i in seq_along(models)){
    MSE_model <- MSE_material[MSE_material$model == models[i], ]
    MSE_model <- MSE_model[order(MSE_model$h),]
    
    
    MSE_model_old <- MSE_material_old[MSE_material_old$model == models[i], ]
    MSE_model_old <- MSE_model_old[order(MSE_model_old$h),]
    
    points(x=MSE_model_old$h, y = MSE_model_old$MSE, col = alpha(colors[i], 0.5),
           type = 'b', pch = 24)
    
    points(x = MSE_model$h, y = MSE_model$MSE, col = colors[i],
           type = 'b', pch = 16)
  }
  
  
  points(x = Best_MSE[[material]]$h, y = Best_MSE[[material]]$MSE,
         col = alpha(colors_model[best[material]], 0.5), type = 'b', pch = 24, 
         bg = alpha(colors_model[best[material]], 0.75))
  dev.off()
}


# Add the legend
tikz('Grafiken/MSE/Legend2.tex',
     width = 1 * textwidth / 72.27,
     height = 0.075 * textwidth / 72.27)
par(mar = c(0, 0, 0, 0), cex = 0.7, mfrow = c(2,1))
plot(NA, xlim = c(1,20), ylim = 0:1, xlab = '', ylab = '', axes = FALSE)
legend('center', legend = toupper(models), horiz = TRUE,
       col = colors, pch = 16, bty = 'n', cex = 0.85, xpd = TRUE,
       text.width = strwidth('TBATS')+0.5)
plot(NA, xlim = c(1,20), ylim = 0:1, xlab = '', ylab = '', axes = FALSE)
legend('center', pch = c(16,24,17), legend = c('Models with Covariates',
                                                'Models without Covariates',
                                                'Best Model without Covariates'),
       bty = 'n', col = 'grey30', text.col = 'grey30', horiz = TRUE, xpd = TRUE,
       cex = 0.8)
dev.off()

# Table for best models ########################################################


Best_MSE[[1]]

MSE_tab_exo


sums_exo <- aggregate(MSE~model+material, sum, data = MSE_tab_exo)
sums_mins_exo <- aggregate(MSE ~ material, min, data = sums_exo)

res_exo <- sums_exo[sums_exo$MSE %in% sums_mins_exo$MSE,]
cbind(res_exo[,1:2], round(res_exo$MSE, 3))

mins <- aggregate(MSE~model+material, min, data = MSE_tab_exo)
maxs <- aggregate(MSE~model+material, max, data = MSE_tab_exo)



cbind(mins[,1:2], round(mins$MSE, 3))
cbind(maxs[,1:2], round(maxs$MSE, 3))


# Models for long term Prediction ##############################################

# Load results
all_files <- list.files('cv_results/residuals_long', full.names = TRUE)
all_names <- gsub('cv_results/residuals_longs/res_', '', gsub('.RData', '', all_files))

n_files <- length(all_files)
MSE_tab_long <- data.frame(model = rep(NA, n_files),
                          material = rep(NA, n_files),
                          h = rep(NA, n_files),
                          MSE = rep(NA, n_files))

for (i in seq_along(all_files)) {
  
  identifier <- unlist(strsplit(all_names[i], '_'))
  
  if(length(identifier)==4){
    identifier <- identifier[-2]
  }
  if(length(identifier)==5){
    identifier <- identifier[-(2:3)]
  }
  
  
  MSE_tab_long[i,1:3] <- identifier
  
  load(all_files[i])
  residuals <- as.matrix(residuals)

  MSE_tab_long[i,4] <- mean(residuals^2)

}
rm(residuals)
rm(identifier)
MSE_tab_long <- MSE_tab_long[,-3]

MSE_tab_long$model <- factor(MSE_tab_long$model, levels = c('naive', 'ARIMA', 'DHR', 'TBATS', 'rf', 'xgb', 'LSTM'))
MSE_tab_long$material <- factor(MSE_tab_long$material)


# Convert the data to a matrix format for grouped bar plot, grouping by material
MSE_matrix <- t(sapply(unique(MSE_tab_long$model), function(mod) {
  sapply(unique(MSE_tab_long$material), function(mat) {
    mse_value <- MSE_tab_long$MSE[MSE_tab_long$material == mat & MSE_tab_long$model == mod]
    if (length(mse_value) == 0) return(NA) else return(mse_value)
  })
}))

colnames(MSE_matrix) <- unique(MSE_tab_long$material)
rownames(MSE_matrix) <- unique(MSE_tab_long$model)

MSE_matrix <- MSE_matrix[models[-(1:2)],]

MSE_matrix[c('TBATS', 'LSTM'), 'PET mit Nylon'] <- NA


tikz(paste0('figures/mse/plot_Long.tex'),
     width = 0.8 * textwidth / 72.27,
     height= 0.6 * textwidth / 72.27)
par(mar = c(6,3.5,2,0.5), cex = 0.8, mgp = c(2.5,1,0))
plot(1, type = 'n', xlim = c(1, 42), 
     ylim = c(0, 0.045), 
     ylab = '$\\text{MSE}_{251}$', xlab = '', xaxt = 'n')

abline(h = axTicks(2), v = seq(6.5, 36.5, by=6), col = 'grey85', lty = 2)

barplot(MSE_matrix, beside = TRUE, col = colors_model[-(1:2)], add = TRUE, 
        border = 'white', names.arg = rep(NA,7), xpd = FALSE)

legend('top', legend = toupper(models)[-(1:2)],
       col = colors_model[-(1:2)], pch = 16, bty = 'n', 
       inset = c(0, -0.075), xpd = TRUE, horiz = TRUE, cex = 0.9,
       text.width = strwidth('TBATS')+0.5)
text(x = seq(3.5, 39.5, by = 6), y = -0.005, labels = gsub('e T', 'e\nT', label_m$en), 
     srt = 45, adj = 1, xpd = TRUE)
dev.off()