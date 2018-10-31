##########################################################################################################
###################################### A S3 Class used to download hist price ############################
##########################################################################################################
###################################### S3 Class reference: ###############################################
###################################### http://www.cyclismo.org/tutorial/R/s3Classes.html #################
##########################################################################################################

#devtools::install_github("sboysel/fredr")
lib <- c("xts","hash","fredr","lubridate","Quandl","dplyr","rjson","blsAPI","bea.R","censusapi")
lapply(lib, function(x){library(x, character.only = TRUE)})

#
# Constants/Default values
#

# Key needed here. It came from webapp admin.

mth.seq <- c("01","02","03","04","05","06","07","08","09","10","11","12")
qtr.seq <- c("Q1","Q1","Q1","Q2","Q2","Q2","Q3","Q3","Q3","Q4","Q4","Q4")
# yr.seq <- seq(start.year, end.year, by=1)

#
# Define EconomicIndicators
#
EconomicIndicators <- function(id = 1,
                               fred_items = NULL,
                               quandl_items = NULL,
                               hist_startdate = NULL,
                               hist_enddate = NULL){
  
  fredr::fredr_set_key(fred_api_key)
  res <- list(EI_instance_id = id,
              EI_fred_items = fred_items,
              EI_quandl_items = quandl_items,
              EI_hist_startdate = hist_startdate,
              EI_hist_enddate = hist_enddate,
              EI_fred_data = NULL,
              EI_quandl_data = NULL)
  class(res) <- append(class(res), "EconomicIndicators")
  
  return(res)
}

#
# St. Louis FRED data
#

#
# Function set required items for St. Louis FRED API
#
EISetFredItems <- function(ei, itm) UseMethod("EISetFredItems")
EISetFredItems.default <- function(ei, itm){ return(ei) }
EISetFredItems.EconomicIndicators <- function(ei, itm){
  ifelse(class(itm) == "hash",
         EI$EI_fred_items <- itm,
         print("Error loading Fred items"))
  return(ei)
}

#
# Function download one item
#}
DownloadFredItem <- function(key, item, st_date, ed_date){
  
  # res <- fredr::fredr(series_id = "UNRATE", observation_start = st_date) %>%
  #   window(., start = st_date, end = ed_date)

  res <- fredr::fredr(
    series_id = item, 
    observation_start = st_date, 
    observation_end = ed_date
  ) 
  
  if(nrow(res) == 0 | is.null(res) | is.vector(res)){
    dates <- seq(from = st_date, to = ed_date, by = "month")
    values <- rep(0, length(dates))
    res <- xts::xts(values, dates)
  } else {
    res <- xts::xts(res$value, res$date)
  }
  
  colnames(res) <- key
  return(res)
}

#
# Function download all items
#
EIDownloadAllFredItems <- function(ei) UseMethod("EIDownloadAllFredItems")
EIDownloadAllFredItems.default <- function(ei){ return(ei) }
EIDownloadAllFredItems.EconomicIndicators <- function(ei){
  st_date <- ei$EI_hist_startdate
  ed_date <- ei$EI_hist_enddate
  dict_keys <- names(ei$EI_fred_items)
  
  prelim.output <- NULL
  for(i in 1:length(dict_keys)){
    print(i)
    res <- DownloadFredItem(dict_keys[i], ei$EI_fred_items[dict_keys[i]], st_date, ed_date)
    prelim.output <- merge.xts(prelim.output, res, all=c(TRUE, TRUE))
  }
  #colnames(prelim.output) <- dict_keys
  
  ei$EI_fred_data <- prelim.output
  return(ei)
}

#
# St. Louis FRED data
#

#
# Function set required items for Quandl
#
EISetQuandlItems <- function(ei, itm) UseMethod("EISetQuandlItems")
EISetQuandlItems.default <- function(ei, itm){ return(EI) }
EISetQuandlItems.EconomicIndicators <- function(ei, itm){
  ifelse(class(itm) == "hash",
         EI$EI_quandl_items <- itm,
         print("Error loading Quandl items"))
  return(EI)
}

#
# Function download one item
#}
DownloadQuandlItem <- function(item, st_date){
  
  Quandl::Quandl.api_key(quandl_key)
  res <- Quandl(item, start_date = st_date)
  res.val <- res[,2]
  res.xts <- xts(res.val, res$Date)  
  
  return(res.xts)
}

#
# Function download all items
#
EIDownloadAllQuandlItems <- function(ei) UseMethod("EIDownloadAllQuandlItems")
EIDownloadAllQuandlItems.default <- function(ei){ return(ei) }
EIDownloadAllQuandlItems.EconomicIndicators <- function(ei){
  st_date <- ei$EI_hist_startdate
  ed_date <- ei$EI_hist_enddate
  dict_keys <- names(ei$EI_quandl_items)
  
  final.ism.output <- NULL
  for(i in 1:length(dict_keys)){
    ism.output <- DownloadQuandlItem(ei$EI_quandl_items[dict_keys[i]], st_date)
    final.ism.output <- merge.xts(final.ism.output, 
                                  ism.output,
                                  all=c(TRUE, TRUE))

  }
  colnames(final.ism.output) <- dict_keys
  ei$EI_quandl_data <- final.ism.output
  
  return(ei)
}