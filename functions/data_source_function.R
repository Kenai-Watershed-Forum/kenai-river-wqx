# Choose which data source to feed into boxplot function based on parameter name

read_csv_based_on_condition <- function(script_file, csv_file1, csv_file2, keyword) {
  # Check if the script file exists
  if (!file.exists(script_file)) {
    stop("Script file does not exist.")
  }
  
  # Read the script file
  script_content <- readLines(script_file)
  
  # Check for the presence of the keyword
  if (any(grepl(keyword, script_content))) {
    message("Keyword found. Reading: ", csv_file1)
    return(read.csv(csv_file1))
  } else {
    message("Keyword not found. Reading: ", csv_file2)
    return(read.csv(csv_file2))
  }
}