main_path <- "/Users/martonzorenyi/Downloads/MTS_final_models/"
bloomberg_folder <- paste0(main_path, "bloombergdata/")
source(paste0(main_path, "scripts/iv_functions.R"))

read_in_function <- function(security, duration){
  security <- tolower(security)
  path <- paste0(bloomberg_folder, security, "_", duration, ".xlsx")
  
  ## Read in Option Data
  option_data <- read.xlsx(path, startRow = 3)
  
  # Function to modify excel sheet
  modify_df <- function(df){
    colnames(df) <- c("Strikes","Ticker1","Geld","Brief","Schl","IVM","Vol.","Strikes","Ticker","Geld","Brief","Schl","IVM","Vol.")
    df <- df[grepl(end_date, df$Ticker1),]
    df_calls <- df[,1:7]
    df_puts <- df[,8:ncol(df)]
    list(calls = df_calls, puts = df_puts)
  }
  output <- modify_df(option_data)
  # Get Stock Data from quantmod
  quantmod_symbol <- toupper(security)
  quantmod_data <- getSymbols(quantmod_symbol, auto.assign = F, from = as.Date("2020-06-17"), to = as.Date(end_date_qmod) + 1)
  colnames(quantmod_data) <- c("Open", "High", "Low", "Close", "Vol.", "Adj.")
  quantmod_data <- quantmod_data$Close
  output <- append(output, list(stockdata = quantmod_data))
  output
}

nvda <- read_in_function("NVDA", "1m")
pltr <- read_in_function("PLTR", "1m")
jnj <- read_in_function("JNJ", "1m")
pg <- read_in_function("PG", "1m")


cor(nvda$stockdata["2020-09-30/"],pltr$stockdata)
cor(nvda$stockdata["2020-09-30/"],jnj$stockdata["2020-09-30/"])
cor(pg$stockdata["2020-09-30/"],jnj$stockdata["2020-09-30/"])
cor(pg$stockdata,jnj$stockdata)
## Get correlations between relevant asset pairs
#a1 <- nvda
#a2 <- pltr

get_corr <- function(a1, a2){
  a1$stockdata <- a1$stockdata["2020-09-30/2025-06-16"]
  a2$stockdata <- a2$stockdata["2020-09-30/2025-06-16"]
  merged_df <- cbind(a1$stockdata, a2$stockdata)
  rolling_corr <- rollapply(
    merged_df,
    width = 30,
    FUN = function(z) cor(z[, 1], z[, 2]),
    by.column = FALSE,
    align = "right",
    fill = NA
  )
  #rolling_corr <- na.omit(rolling_corr)
  rolling_corr
}
nvda_pltr <- get_corr(nvda, pltr)
dates <- index(nvda_pltr) # extract dates for plotting
nvda_pltr <- as.numeric(nvda_pltr) # take numeric values for plotting
nvda_jnj <- as.numeric(get_corr(nvda, jnj))
jnj_pg <- as.numeric(get_corr(pg, jnj))

par(mfrow = c(2,2))
plot(dates, nvda_pltr, type = "line", xlab = "t", ylab = "30-day Rolling Correlation", main = "NVDA/PLTR", ylim = c(-1,1))

plot(dates, nvda_jnj, type = "l", xlab = "", ylab = "30-day Rolling Correlation", main = "NVDA/JNJ", ylim = c(-1,1))

plot(dates, jnj_pg, type = "l", xlab = "", ylab = "30-day Rolling Correlation", main = "JNJ/PG", ylim = c(-1,1))
