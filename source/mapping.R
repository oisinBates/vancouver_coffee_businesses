library(dplyr)
library(htmlwidgets)
library(leaflet)
library(VancouvR)
library(withr)

# Set API Option from .Renviron
VancouverOpenDataApiKey <- Sys.getenv("vancouver_open_data_api_key")


# Get business with active licenses
# Escaped apostrophes break the API queries, so searching without them and applying additional filtering in Dplyr
query_string =
  "(businesstradename LIKE 'Blenz' OR businesstradename LIKE 'JJ Bean' OR businesstradename LIKE 'McDonald*' OR businesstradename LIKE 'Starbucks' OR businesstradename LIKE 'Tim Horton*' ) AND (businesstype='Ltd Service Food Establishment' OR businesstype='Restaurant Class 1' ) AND (folderyear='17' OR folderyear='20' OR folderyear='23') AND status='Issued'"
licensed_coffee_businesses <-
  get_cov_data(dataset_id = "business-licences", where = query_string)

formatted_coffee_businesses <- licensed_coffee_businesses %>%
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
    formatted_tradename %in% c("blenz",
                               "jj_bean",
                               "mcdonalds",
                               "starbucks",
                               "tim_hortons")
  )

# Setting missing coordinates for '2501 Main Street'
formatted_coffee_businesses$geom[formatted_coffee_businesses$house == "2501" &
                                   formatted_coffee_businesses$street == "MAIN ST"] <-
  "{\"coordinates\": [-123.10139591801362, 49.26279740435901]}"
# Setting missing coordinates for '1752 Commercial Drive'
formatted_coffee_businesses$geom[formatted_coffee_businesses$house == "1752" &
                                   formatted_coffee_businesses$street == "Commercial Dr"] <-
  "{\"coordinates\": [-123.06942508243132, 49.26885285075189]}"
# Setting missing coordinates for '2517 Commercial Drive'
formatted_coffee_businesses$geom[formatted_coffee_businesses$house == "2517" &
                                   formatted_coffee_businesses$street == "Commercial Dr"] <-
  "{\"coordinates\": [-123.07014326074759, 49.262025289438824]}"


formatted_coffee_businesses <- formatted_coffee_businesses %>%
  mutate(Long = as.numeric(sub(".*\\[(.*?),.*", "\\1", geom)), Lat = as.numeric(sub(".* (.*)\\].*", "\\1", geom)))


coffee_icon_list <- iconList(
  blenz = makeIcon(
    "source/assets/blenz_logo.jpeg",
    "source/assets/blenz_logo.jpeg",
    32,
    32,
    shadowWidth = 5
  ),
  jj_bean = makeIcon(
    "source/assets/jj_bean_logo.png",
    "source/assets/jj_bean_logo.png",
    34,
    28
  ),
  mcdonalds = makeIcon(
    "source/assets/mcdonalds_canada_logo.png",
    "source/assets/mcdonalds_canada_logo.png",
    32,
    32
  ),
  starbucks = makeIcon(
    "source/assets/starbucks_logo.png",
    "source/assets/starbucks_logo.png",
    30,
    30
  ),
  tim_hortons = makeIcon(
    "source/assets/tim_hortons_logo.jpg",
    "source/assets/tim_hortons_logo.jpg",
    32,
    26
  )
)

create_interactive_map <-
  function(year,
           coffee_icon_list,
           formatted_coffee_businesses) {
    formatted_year = substr(year, 3, 4)

    filtered_data <-
      formatted_coffee_businesses %>% filter(folderyear == formatted_year)

    mapped_business_locations <-
      leaflet(filtered_data) %>%
      addTiles() %>%
      # Select from coffee_icon_list based on formatted_coffee_businesses$formatted_tradename
      addMarkers(icon = ~ coffee_icon_list[formatted_tradename])

    output_filename <-
      paste0("coffee_chain_locations_", year, ".html")

    # Outputting the leaflet widget as html
    # Currently generating PNG images of the map via manual screenshot.
    # A solution like 'mapshot' could be utilised to automate this - https://r-spatial.github.io/mapview/reference/mapshot.html
    with_dir(
      'output/interactive_maps',
      saveWidget(mapped_business_locations, file = output_filename)
    )


  }

create_interactive_map("2023", coffee_icon_list, formatted_coffee_businesses)
create_interactive_map("2020", coffee_icon_list, formatted_coffee_businesses)
create_interactive_map("2017", coffee_icon_list, formatted_coffee_businesses)
