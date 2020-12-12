
#import data, calculate logreturns
adata <- a_dataprep$prepareData(adata = a_data_import$yahoo())

#create technical indicators
TSMOM_df <- a_dataprep$TI_gen_TSMOM()
MA_df <- a_dataprep$TI_gen_MA()
MA_TSMOM_df <- a_dataprep$TI_gen_MA_TSMOM()

#final data ready
Input_data_df <- a_dataprep$data_ready()

#model fit econometric modelling




#trading strategy
proba <- a_dataprep$data_ready() 

#plots



tibble(embed(adata$log_returns,2)[,1]) %>% View
