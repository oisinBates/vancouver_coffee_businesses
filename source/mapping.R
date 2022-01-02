library(dplyr)
library(htmlwidgets)
library(leaflet)
library(VancouvR)
library(withr)

# Set API Option from .Renviron
VancouverOpenDataApiKey <- Sys.getenv("vancouver_open_data_api_key")


# Get business with active licenses
# Escaped apostrophes break the API queries, so searching without them and applying additional filtering in Dplyr
query_string = paste0("(businesstradename LIKE 'Blenz' OR businesstradename LIKE 'JJ Bean' OR businesstradename LIKE 'McDonald*' OR businesstradename LIKE 'Starbucks' OR businesstradename LIKE 'Tim Horton*' ) AND status='Issued' AND expireddate>'", format(Sys.time(), '%Y-%m-%d'), "'")
active_businesses <- get_cov_data(dataset_id = "business-licences", where = query_string)


coffee_franchises <- active_businesses %>%
  mutate(
    formatted_tradename = case_when(
      grepl("Blenz", businesstradename) ~ "blenz",
      grepl("JJ Bean", businesstradename) ~ "jj_bean",
      grepl("McDonalds", businesstradename) ~ "mcdonalds",
      grepl("McDonald's", businesstradename) ~ "mcdonalds",
      grepl("Starbucks", businesstradename) ~ "starbucks",
      grepl("Tim Hortons", businesstradename) ~ "tim_hortons",
      grepl("Tim Horton's", businesstradename) ~ "tim_hortons",
      TRUE ~ businesstradename
    )
  ) %>%
  filter(
    formatted_tradename %in% c(
      "blenz",
      "jj_bean",
      "mcdonalds",
      "starbucks",
      "tim_hortons"
    )
  )


formatted_coffee_franchises <- coffee_franchises %>%
  mutate( Long = as.numeric( sub(".*\\[(.*?),.*", "\\1", geom) ), Lat = as.numeric( sub(".* (.*)\\].*","\\1",geom) ) ) %>%
  select(businessname, businesstradename, formatted_tradename, numberofemployees, Long, Lat)


coffee_icon_list <- iconList(
  blenz = makeIcon("geo_mapping/assets/blenz_logo.jpeg", "geo_mapping/assets/blenz_logo.jpeg", 32, 32, shadowWidth = 5),
  jj_bean = makeIcon("geo_mapping/assets/jj_bean_logo.png", "geo_mapping/assets/jj_bean_logo.png", 34, 28),
  mcdonalds = makeIcon("geo_mapping/assets/mcdonalds_canada_logo.png", "geo_mapping/assets/mcdonalds_canada_logo.png", 32, 32),
  starbucks = makeIcon("geo_mapping/assets/starbucks_logo.png", "geo_mapping/assets/starbucks_logo.png", 30, 30),
  tim_hortons = makeIcon("geo_mapping/assets/tim_hortons_logo.jpg", "geo_mapping/assets/tim_hortons_logo.jpg", 32, 26)
)


mapped_franchise_locations <- leaflet(formatted_coffee_franchises) %>%
  addTiles() %>%
  # Select from coffee_icon_list based on formatted_coffee_franchises$formatted_tradename
  addMarkers(icon = ~coffee_icon_list[formatted_tradename])


# Ouput leaflet widget as html
with_dir('output', saveWidget(mapped_franchise_locations, file="franchise_locations.html"))
