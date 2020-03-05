library(readxl)

table_subjects <- read_excel("inst/raw_data/Table_Group_Mapping.xlsx")
names(table_subjects) <- c("table_subject", "table_subject_name")

use_data(table_subjects)
