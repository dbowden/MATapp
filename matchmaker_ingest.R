library(tidyverse)
library(tidygeocoder)

samhsa_programs <- read_csv("Tables_for_2020Q3_MATchMapper - samhsa_sa_bupe_site_listings.csv")

# geocode addresses
samhsa_programs <- samhsa_programs %>% geocode(street = street1, city = city, state = state_usa, postalcode = zip5, method = 'census')

write_csv(samhsa_programs, "samhsa_programs_geocoded.csv")
