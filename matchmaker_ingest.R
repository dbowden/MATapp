# merge into a single dataframe with indicators for the original types

# load data
programs <- read.csv("dbhids_geocoded.csv")
programs <- subset(programs, select=-c(X))

providers <- read.csv("BupePrescribersData_ToPrint - allnames_June2020.csv")

