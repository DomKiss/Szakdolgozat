library(quantmod)
library(R6)
library(dplyr)
library(readxl)

datadownload_factory <- R6Class(
  "DataDownload",
  private = list(
    ticker = "BTC-USD",
    start = "2016-01-01",
    end = "2020-02-28"
  ),
  public = list(
    yahoo = function(ticker = private$ticker,
                     start = private$start,
                     end = private$end) {
      adatax <-
        getSymbols.yahoo(
          paste(ticker),
          env = globalenv(),
          from = start,
          to = end,
          auto.assign = FALSE,
          return.class = 'data.frame'
        )
      adata <- cbind(as.data.frame.Date(rownames(adatax)), adatax)
      colnames(adata)[1] <- c("Date")
      return(adata)
    },
    csv = function() {
      
    }
  )
)

a_data_import <- datadownload_factory$new()

#adatelokeszites
data_prep_factory <- R6Class(
  "DataPrep",
  inherit = datadownload_factory,
  private = list(
    T_colnumber = 1,
    #hanyadik oszlopban van a datum
    P_colnumber = 7,
    adata = a_data_import$yahoo(),
    #hanyadik oszlopban van az arfolyam
    n_vector = seq(28, 52, by = 2),
    #milyen technikai indikátorokat generálok
    Ind_name_default="MA"
  ),
  public = list(
    #loghozamok kiszamitasa
    prepareData = function(T_colnumber = private$T_colnumber,
                           P_colnumber = private$P_colnumber,
                           adata = private$adata) {
      DB_length <- nrow(adata)
      dates <- adata[2:DB_length, T_colnumber]
      prices <- adata[2:DB_length, P_colnumber]
      log_returns <- diff(log(adata[, P_colnumber]), p = 1)
      return(data.frame(dates, prices, log_returns))
    },
  #technikai indikatorok generalasa
  TI_gen_MA =
    function(data = self$prepareData()$prices,
             date=self$prepareData()$dates,
             n_vector = private$n_vector,
             Ind_name=private$Ind_name_default) {
      library(TTR)
      
      result = data.frame()
      result <- as.data.frame.Date(date)
      
      if (Ind_name == "MA") {
        for (n in n_vector) {
          ind <- SMA(data, n = n)
          result <- cbind(result, ind)
        }
      } else{
        for (n in n_vector) {
          ind <- momentum(data, n = n)
          result <- cbind(result, ind)
        }
      }
      
      for (i in 1:length(n_vector)) {
        colnames(result)[i + 1] <- paste(Ind_name, n_vector[i], sep = "")
      }
      
      colnames(result)[1] <- c("Date")
      return(result)
    },
  TI_gen_TSMOM = function(data = self$prepareData()$prices,
                          date = self$prepareData()$dates,
                          n_vector = private$n_vector,
                          Ind_name = private$Ind_name_default) {
    return(self$TI_gen_MA(
      Ind_name = "TSMOM",
      data = data,
      date = date,
      n_vector = n_vector
    ))
  },
  TI_gen_MA_TSMOM = function(data = self$prepareData()$prices,
                             date = self$prepareData()$dates,
                             n_vector = private$n_vector,
                             Ind_name = private$Ind_name_default) {
    return(cbind(
      self$TI_gen_MA(
        data = data,
        date = date,
        n_vector = n_vector
      ),
      self$TI_gen_TSMOM(
        data = data,
        date = date,
        n_vector = n_vector
      )[-1]
    ))
  },
  data_ready = function(input_TI_df=self$TI_gen_MA_TSMOM(), log_returns=self$prepareData()$log_returns){
    input_TI_df=ifelse(private$Ind_name_default=="MA", self$TI_gen_MA(),
                       ifelse(private$Ind_name_default=="TSMOM", self$TI_gen_TSMOM(), self$TI_gen_MA_TSMOM()))
    input_df_nonlagged <- na.omit(cbind(input_TI_df, log_returns))
    colnames(input_df_nonlagged)[length(input_df_nonlagged)] <- "Log_returns"
    input_df_nonlagged$Log_returns <- rbind(NA, tibble(embed(input_df_nonlagged$Log_returns,2)[,1]))
    input_df_nonlagged <- input_df_nonlagged[2:nrow(input_df_nonlagged),]
    colnames(input_df_nonlagged)[ncol(input_df_nonlagged)] <- "Log_returns"
    input_df_nonlagged$Log_returns <- input_df_nonlagged$Log_returns %>% unlist
    return(input_df_nonlagged)
  }
  
))

a_dataprep <- data_prep_factory$new()

modell_fit_factory <- R6Class("ModelFit", inherit="DataPrep",
                              private=list(windowsSize=round*nrow(a_dataprep$data_ready())))
