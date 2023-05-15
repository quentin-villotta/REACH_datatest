library(googleway)
library(dplyr)
library(sf)
library(readxl)
library(colorRamps)

#Linking to GEOS 3.11.2, GDAL 3.6.4, PROJ 9.2.0; sf_use_s2() is TRUE

# Google Maps API
gm_api_key <- "AIzaSyDiDjJXNXRz1cFawQXYyXQDMUGsCxch910"

# Syria district gdb database
file_gdb <- "data/SYR_AdminBoundaries_candidate.gdb/"
sda_gdb <- st_read(file_gdb)

# HSOS syria 03/2023 data
file_hsos <- "data/REACH_SYR_HSOS_Dataset_March2023_NWS.xlsx"
hsos <- read_excel(path=file_hsos, sheet="Dataset")

#n_distinct(hsos$`Sub-district p-code`)
#Create a function to generate a continuous color palette
rbPal <- colorRampPalette(c('green','yellow', 'red'))



# Get proportion of communities by district where tents is one of the three main kind of sheltter
# ten_col <- `Three most common IDP shelter types - Tent`


data_color <- hsos %>%
  mutate(logical_tent = case_when(`Three most common IDP shelter types - Tent` == "Yes" ~ TRUE,
                                  `Three most common IDP shelter types - Tent` == "No" ~ FALSE,
                                  !`Three most common IDP shelter types - Tent` %in%  c("No","Yes") ~ NA,
                                  .default = NA)) %>%
  group_by(`District p-code`) %>% 
  summarise(prop_tent = mean(logical_tent, na.rm = TRUE)) %>%
  inner_join(x = sda_gdb,
             by = c("admin2Pcode" ="District p-code")) %>%
  mutate(color_col = rbPal(10)[as.numeric(cut(prop_tent, breaks = 10))],
         district_name = toupper(admin2Name_en),
         info = paste0(district_name, " | ", round(prop_tent,2)))

# Google Map
  data_color  %>%
  google_map(key = gm_api_key) %>%
  add_polygons(polyline = "Shape", 
               mouse_over = "info",
               fill_colour = "color_col",
               update_map_view = TRUE)

