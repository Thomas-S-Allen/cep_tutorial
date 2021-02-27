VOTable_to_dataframe <- function(VOTable_object) {
  
  library(tidyverse)
  library(xml2)
  
  # Read VOTable object as XML
  dat <- read_xml(VOTable_object)
  
  # Strip XML of name spaces
  dat <- xml_ns_strip(dat)
  
  # Find fields and get field names  
  field <- dat %>% xml_find_all("/VOTABLE/RESOURCE/TABLE/FIELD")
  fnames <- xml_attr(x=field,attr="name")
  
  # Find all TD elements, assign to list, and replace missing values with NA's
  data <- dat %>% xml_find_all("/VOTABLE/RESOURCE/TABLE/DATA/TABLEDATA/TR/TD") %>% xml_text()
  data <- replace(data,data=="",NA)
  
  # Convert list of data elements into a data frame
  df <- data.frame(matrix(unlist(data), nrow=length(data)/length(fnames), byrow=T),stringsAsFactors=FALSE)
  
  # Assign field names to the columns of the data frame
  colnames(df) <- fnames
  
  df
}
