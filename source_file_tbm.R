## Source file to source multiple files
  print(paste0("Starting with Script 1 at ",  Sys.time()))
  source("/Users/martonzorenyi/Downloads/MTS_final_models/scripts/basket_const_const_new.R")
  print(paste0("Script 1 successfully terminated at ", Sys.time()))
  source("/Users/martonzorenyi/Downloads/MTS_final_models/scripts/exchange_const_stoch.R")
  print(paste0("Script 2 successfully terminated at ", Sys.time()))
  source("/Users/martonzorenyi/Downloads/MTS_final_models/scripts/exchange_stoch_stoch.R")
  print(paste0("Script 3 successfully terminated at ", Sys.time()))
  source("/Users/martonzorenyi/Downloads/MTS_final_models/scripts/basket_const_stoch.R")
  print(paste0("Script 4 successfully terminated at ", Sys.time()))
  source("/Users/martonzorenyi/Downloads/MTS_final_models/scripts/basket_stoch_stoch.R")
  print(paste0("Script 5 successfully terminated at ", Sys.time()))





## Command to copy:
#source("/Users/martonzorenyi/Downloads/MTS_final_models/scripts/source_file_tbm.R")
