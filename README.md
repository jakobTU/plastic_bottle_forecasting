# Material Flow Composition Forecasting in the Returnable Bottle Sector

This repository supplements the code to the paper *Material Flow Composition Forecasting in the Returnable Bottle Sector* available at TBA.

### Structure

- `Covariates.R`: Gets the external data described in Section 2.4 of the paper
- `CrossValidationPlots.R`: Analyze the cross-correlation of the external data
- `CrossValidation_Cluster.R`: Performs the analysis described in Section 4.3 of the paper
- `CrossValidation_Exogen_Cluster.R`: Performs the analysis described in Section 4.4 of the paper
- `CrossValidation_Longterm_Cluster.R`: Performs the analysis described in Section 4.5 of the paper
- `FourierOrder.R`: Determines the best Fourier order for each material
- `LSTM_CV.R`: Perform the cross-validation analysis for the LSTM described in Section 4.3 of the paper
- `LSTM_CV_Long.R`: Performs the cross-validation analysis for the LSTM described in Section 4.5 of the paper
- `Results_CrossValidation.R`: Generates the plots in Figures 1 and 2 of the paper
