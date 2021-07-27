## Load individual house prices (NOT run)

# Buildings for year round living
res_codes <- tribble (~ENH_ANVEND_KODE, ~type,
                      120, "Single-family house",
                      121, "Single-family house",
                      122, "Single-family house",
                      130, "Semi-detached house",
                      131, "Semi-detached house",
                      132, "Semi-detached house",
                      140, "Multi-storey") %>%
  # Convert type to factor
  mutate(type = factor(type)) %>%
  # Convert to data.table
  as.data.table()

# Function for reading residential units in the study area from a BBR files (.csv):
f_runits_oft_capital <- function(.file, .muni = study_area_codes) {
  fread(.file) %>% 
    # Select only Residential houses - Buildings for year-round living
    filter.(ENH_ANVEND_KODE %in% res_codes$ENH_ANVEND_KODE) %>%
    # Format KomKode and select municipalities of the study area
    mutate.(KomKode = paste0("0", KomKode)) %>% 
    filter.(KomKode %in% .muni) %>% 
    # Remove residential units not used for residential purpose
    filter.(BOLIGTYPE_KODE != "E" | BOLIGTYPE_KODE != "5") %>% 
    # Remove tiny dwellings (area < 10 m2)
    filter.(BEBO_ARL >= 10) %>%
    # Add year of the BBR dataset 
    mutate.(year = parse_number(stringr::str_extract(.file, "_[0-9]+_"))) %>% 
    # Convert house price (KONTANT_KOEBESUM) to numeric and kDKK
    mutate.(price_kDKK = KONTANT_KOEBESUM / 1000,
            price_kDKK = as.numeric(price_kDKK)) %>% 
    # Drop unused factors levels
    droplevels() %>% 
    # ordinary free trade or auction
    filter.(OVERDRAGELSES_KODE == "1" | OVERDRAGELSES_KODE == "3") %>% 
    # Remove prices > 0 kDKK
    filter.(price_kDKK > 0) %>% 
    # House prices per m2
    mutate.(price_kDKK_m2 = price_kDKK / BEBO_ARL)
}

# Load residential units (from .csv files)
csv_files_path <- list.files(path = Sys.getenv("OneDrive_BBR_path"),
                             pattern = "*.csv",
                             full.names = TRUE)
study_area_codes <- capital_muni$muni_id

runits_oft_capital_read <- f_runits_oft_capital(csv_files_path[17])

# Clean dataset
runits_oft_capital <- runits_oft_capital_read %>% 
  # Input empty cells in Etagebetegn (buildings with only one floor) as "st"
  mutate.(Etagebetegn = fifelse(Etagebetegn == "", "st", Etagebetegn)) %>% 
  # Etagebetegn as ordered factor
  mutate.(Etagebetegn = factor(Etagebetegn,
                               c("k2", "kl", "st", seq(1, 36, 1)),
                               ordered = TRUE)) %>% 
  # Group floor levels with 5 or more
  mutate.(floor_level = fct_other(Etagebetegn,
                                  drop = factor(seq(5, 36)),
                                  other_level = "5 or more")) %>% 
  # Add residential description (type) into the dataset
  left_join.(res_codes, by = "ENH_ANVEND_KODE") %>% 
  # Convert columns with codes (*_KODE) to character
  mutate.(across.(ends_with("KODE"), ~as.character(.))) 

# Convert to sf objects and add population data by ancestry
runits_oft_capital_sf <- runits_oft_capital %>% 
  # Convert to sf object
  st_as_sf(coords = c("etrs89koordinat_ost",
                      "etrs89koordinat_nord"),
           crs = "EPSG:25832") %>% 
  # Detect to what parish belongs a house
  st_join(., capital_prsh, join = st_nearest_feature)

## Summary house prices by parish
sum_runits_oft_prices <- runits_oft_capital_sf %>% 
  as_tibble() %>% 
  group_by(prsh_id) %>%  
  summarise(n_runits_oft = n(), 
            mean_kDKK_m2   = mean(price_kDKK_m2, na.rm = TRUE),
            median_kDKK_m2 = median(price_kDKK_m2, na.rm = TRUE)) %>% 
  # Link with population data by ancestry 
  left_join(capital_prsh_ancestry_sf) %>% 
  st_sf() 

saveRDS(sum_runits_oft_prices, "sum_runits_oft_prices.rds")
