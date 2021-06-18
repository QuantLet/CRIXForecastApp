CRIXForecastApp contains the app interface and server, it binds the other scripts together. CRIXFAAnalysisFuns and CRIXFAModelFuns tie directly into
it by defining the graphs the app uses and helping decongest the code of CRIXForecastApp.

CRIXLRETS, CRIXLRML and CRIXLRAnalysis are non-essential for the functioning of the App. They were used for analysis and model training. CRIXCompPlot
was used to get the plots of the CRIX components used on the thesis paper. CRIXdataloader is a function used to load data and it is used throughout.