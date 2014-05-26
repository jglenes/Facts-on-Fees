# Fetch a google spreadsheet (published as a CSV) and return a dataframe

getGoogleSpreadsheet <- function(url) {
  curl <- getURL(url)
  data <- read.csv(textConnection(curl),sep=",")
  return(data)
}