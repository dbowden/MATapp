library(dplyr)
library(tidyr)
library(tidygeocoder)

programs <- read.csv("SampleDjangoAppData_2021q1 - RShiny_HybridTable_sample.csv", skip = 4)

programs <- programs %>% 
  filter(System.Name != "") %>% 
  separate(Address, c("Address", "Address2", "Address3"), sep=",") %>% 
  geocode(street = Address, city = City, state = State, postalcode = ZIP, method = 'osm', lat = latitude, lon = longitude)
# census: 8/10

write.csv(programs, "SampleDjangoAppData_2021q1 - RShiny_HybridTable_sample_geocoded.csv", row.names = F)
