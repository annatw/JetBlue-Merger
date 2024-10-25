compile_financial_records(output = "02.Intermediate/Financial_Compile.rds"){
  file_head <- "01.Input/21.FinancialStatements"
  file_list <- list.files(path = file_head)
  
  compile <- paste(file_head, file_list, sep = "/") %>%
    map_df(~fread(.))
  
  colnames(compile) <- tolower(colnames(compile))
  
  # First, restrict to Domestic Operations
  compile <- compile[region == "D",]
  compile[, region := NULL]
  
  
}