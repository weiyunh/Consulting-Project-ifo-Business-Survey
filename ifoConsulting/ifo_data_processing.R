# ===============================================================================
# ifo Business Survey - Complete Data Processing Script
# Simplified Version for DFM Analysis (No Missing Data Analysis)
# ===============================================================================

suppressPackageStartupMessages({
  library(readxl)
  library(tidyverse)
})

# ===============================================================================
# SECTION 1: DATA READING FUNCTIONS
# ===============================================================================

read_single_excel <- function(pathname) {
  df <- read_excel(pathname, skip = 1, col_names = TRUE)
  df <- df %>% slice(-1)
  dates <- df %>% pull(1)
  df <- df %>% select(-1)
  
  split_tibbles <- map(
    seq(1, ncol(df), by = 15),
    ~ df[, .x:min(.x + 14, ncol(df))]
  ) %>%
    map(augment_single_tibble, dates = dates)
  
  return(split_tibbles)
}

augment_single_tibble <- function(tib, dates) {
  cols <- names(tib)
  cols_split <- str_split(cols, ':', simplify = TRUE)
  industry_code <- cols_split[1, 1]
  
  if (any(cols_split[, 1] != industry_code)) {
    warning("Industry codes do not match in columns")
  }
  
  names(tib) <- cols_split[, 2]
  
  tib <- tib %>%
    mutate(
      date = dates,
      industry_code = industry_code
    ) %>%
    select(date, industry_code, everything())
  
  return(tib)
}

read_ifo_data <- function(data_path) {
  all_files <- list.files(data_path, pattern = "\\.xlsx$", full.names = TRUE)
  filepaths <- all_files[!grepl("~\\$", basename(all_files))]
  
  ifo_tbl <- filepaths %>%
    map(read_single_excel) %>%
    flatten() %>%
    bind_rows()
  
  return(ifo_tbl)
}

# ===============================================================================
# SECTION 2: CLASSIFICATION & METADATA FUNCTIONS
# ===============================================================================
# The ifo Business Survey uses TWO PARALLEL classification systems:
#
# 1. STANDARD HIERARCHICAL CLASSIFICATION (by industry sector):
#    - Based on official German economic activity classification (0-5)
#    - Follows tree structure: C0000000 → C1000000 → C1010000 → ...
#    - Organized by production sector (food, machinery, automotive, etc.)
#    - Examples: C1000000 (Food), C2800000 (Machinery), C2900000 (Automotive)
#
# 2. CROSS-CUTTING PRODUCT-TYPE CLASSIFICATION (MIG - Main Industrial Groups):
#    - Groups industries by PRODUCT USE regardless of sector
#    - Cuts across the standard hierarchy horizontally
#    - Three main categories based on end use:
#      * Intermediate goods (C00000A0): Raw materials & components
#      * Capital goods (C00000B0): Machinery & equipment for production
#      * Consumer goods (C00000C0): Products for final consumers
#    - Further subdivisions:
#      * Durable goods (C0000CD0): Long-lasting consumer products
#      * Non-durable goods (C0000CN0): Consumable products
#
# 3. SPECIAL AGGREGATES:
#    - CONG0000: Manufacturing excluding food industry
#    - C00FOBE0: Food, beverages, and tobacco aggregate (WZ 10, 11, 12)
#    - These are alternative top-level aggregations
# These special codes are NOT part of the standard Level 0-5 hierarchy.
# They represent functional aggregations that span multiple standard sectors.
# In this function, we assign them to Level 6 to distinguish them from 
# standard hierarchical codes.

get_level <- function(code) {
  special_level6 <- c(
    "C00000A0", "C00000B0", "C00000C0", "C0000CD0", "C0000CN0",
    "C00FOBE0", "CONG0000", "CONG00C0", "CONG0CN0"
  )
  
  if (code %in% special_level6) return(6L)
  
  digits <- substring(code, 2)
  if (grepl("[^0-9]", digits)) return(NA_integer_)
  
  digits <- sub("0+$", "", digits)
  if (digits == "" || digits == "0") return(0L)
  
  len <- nchar(digits)
  return(max(1L, len - 1L))
}

# English Name mapping functions for level1
get_industry_name_en <- function(code) {
  industry_names <- c(
    "C0000000" = "Manufacturing Total",
    "C1000000" = "Food Products",
    "C1100000" = "Beverages",
    "C1300000" = "Textiles",
    "C1400000" = "Wearing Apparel",
    "C1500000" = "Leather Products",
    "C1600000" = "Wood Products",
    "C1700000" = "Paper Products",
    "C1800000" = "Printing",
    "C1900000" = "Coke and petroleum refining",
    "C2000000" = "Chemicals",
    "C2100000" = "Pharmaceuticals",
    "C2200000" = "Rubber and Plastics",
    "C2300000" = "Glass, ceramics, and stone processing",
    "C2400000" = "Basic Metals",
    "C2500000" = "Fabricated Metal Products",
    "C2600000" = "Computer and Electronics",
    "C2700000" = "Electrical Equipment",
    "C2800000" = "Machinery",
    "C2900000" = "Motor Vehicles",
    "C3000000" = "Other Transport Equipment",
    "C3100000" = "Furniture",
    "C3200000" = "Other Manufacturing"
  )
  name <- industry_names[code]
  if (is.na(name)) return(code)
  return(as.character(name))
}

get_industry_name <- function(code) {
  industry_names <- c(
    "C0000000" = "Verarbeitendes Gewerbe",
    "C00000A0" = "Herstellung von Vorleistungsgütern",
    "C00000B0" = "Herstellung von Investitionsgütern",
    "C00000C0" = "Herstellung von Konsumgütern (Ge- und Verbrauchsgüter)",
    "C0000CD0" = "Herstellung von Gebrauchsgütern",
    "C0000CN0" = "Herstellung von Verbrauchsgütern",
    "C00FOBE0" = "Ernährungsgewerbe und Tabakverarbeitung",
    "C1000000" = "Herstellung von Nahrungs- und Futtermitteln",
    "C1010000" = "Schlachten und Fleischverarbeitung",
    "C1020000" = "Fischverarbeitung",
    "C1030000" = "Obst- und Gemüseverarbeitung",
    "C1050000" = "Milchverarbeitung",
    "C1060000" = "Herstellung von Mahl- und Schälmühlen  Stärkeerzeugnissen",
    "C1070000" = "Herstellung von Back- und Teigwaren",
    "C1071000" = "Herstellung von Backwaren (ohne Dauerbackwaren)",
    "C1080000" = "Herstellung von sonstigen Nahrungsmitteln",
    "C1082000" = "Herstellung von Süßwaren (einschließlich Dauerbackwaren)",
    "C1090000" = "Herstellung von Futtermitteln",
    "C1100000" = "Getränkeherstellung",
    "C1101000" = "Herstellung von Spirituosen",
    "C1105000" = "Herstellung von Bier",
    "C1107000" = "Herstellung von Erfrischungsgetränken und Mineralwasser",
    "C1300000" = "Herstellung von Textilien",
    "C1320000" = "Weberei",
    "C1321000" = "Baumwollweberei",
    "C1330000" = "Veredlung von Textilien und Bekleidung",
    "C1390000" = "Herstellung von sonstigen Textilwaren",
    "C1392000" = "Herstellung konfektionierter Textilwaren (ohne Bekleidung)",
    "C1400000" = "Herstellung von Bekleidung",
    "C1410000" = "Herstellung von Bekleidung (ohne Pelzbekleidung)",
    "C1413000" = "Herstellung von sonstiger Oberbekleidung",
    "C1414000" = "Herstellung von Wäsche",
    "C1414100" = "Herstellung von gewebter Wäsche (ohne Miederwaren)",
    "C1431000" = "Herstellung von Strumpfwaren",
    "C1500000" = "Herstellung von Leder  Lederwaren und Schuhen",
    "C1510000" = "Herstellung von Leder und Lederwaren (ohne Lederbekleidung)",
    "C1520000" = "Herstellung von Schuhen",
    "C1600000" = "Holz-  Flecht-  Korb- und Korkwarenherstellung (ohne Möbel)",
    "C1610000" = "Säge-  Hobel- und Holzimprägnierwerke",
    "C1620000" = "Herstellung sonstiger Holz-  Flecht-  Korb- und Korkwaren",
    "C1621000" = "Herstellung von Furnier-  Sperrholz- und Holzspanplatten",
    "C1623000" = "Herstellung von Fertigbauteilen und Ausbauelementen",
    "C1624000" = "Herstellung von Verpackungsmitteln aus Holz",
    "C1700000" = "Papiergewerbe",
    "C1710000" = "Herstellung von Holz- und Zellstoff  Papier  Karton und Pappe",
    "C1712000" = "Herstellung von Papier  Karton und Pappe",
    "C1712100" = "Herstellung von Papier",
    "C1712200" = "Herstellung von Karton und Pappe",
    "C1720000" = "Papier-  Karton- und Pappeverarbeitung",
    "C1721000" = "Herstellung von Wellpapier und -pappe  Verpackungsmitteln",
    "C1723000" = "Herstellung von Schreibwaren und Bürobedarf aus Papier",
    "C1800000" = "Herstellung von Druckerzeugnissen",
    "C1810000" = "Herstellung von Druckerzeugnissen",
    "C1811000" = "Zeitungsdruck",
    "C1812000" = "Drucken a.n.g.",
    "C1813000" = "Druck und Medienvorstufe",
    "C1900000" = "Kokerei und Mineralölverarbeitung",
    "C2000000" = "Herstellung von chemischen Erzeugnissen",
    "C2010000" = "Herstellung von chemischen Grundstoffen",
    "C2012000" = "Herstellung von Farbstoffen und Pigmenten",
    "C2014000" = "Herstellung sonstiger organischer Grundstoffe",
    "C2016000" = "Herstellung von Kunststoff in Primärformen",
    "C2030000" = "Herstellung von Anstrichmitteln  Druckfarben und Kitten",
    "C2040000" = "Herstellung von Wasch-  Reinigungs- u. Körperpflegemitteln",
    "C2050000" = "Herstellung von sonstigen chemischen Erzeugnissen ",
    "C2100000" = "Herstellung von pharmazeutischen Erzeugnissen",
    "C2200000" = "Herstellung von Gummi- und Kunststoffwaren",
    "C2210000" = "Herstellung von Gummiwaren",
    "C2219000" = "Herstellung von sonstigen Gummiwaren",
    "C2220000" = "Herstellung von Kunststoffwaren",
    "C2221000" = "Herstellung von Platten  Folien  Schläuchen und Profilen",
    "C2222000" = "Herstellung von Verpackungsmitteln aus Kunststoffen",
    "C2223000" = "Herstellung von Baubedarfsartikeln aus Kunststoffen",
    "C2229000" = "Herstellung von sonstigen Kunststoffwaren",
    "C2300000" = "Glas-  Keramikgewerbe  Verarbeitung von Steinen und Erden",
    "C2310000" = "Herstellung von Glas und Glaswaren",
    "C2312000" = "Veredelung und Bearbeitung von Flachglas",
    "C2313000" = "Herstellung von Hohlglas",
    "C2319000" = "Herstellung von technischen Glaswaren",
    "C2330000" = "Herstellung von keramischen Baumaterialien",
    "C2340000" = "Herstellung von sonstiger Keramik ",
    "C2341000" = "Herstellung von keramischen Haushaltswaren",
    "C2350000" = "Herstellung von Zement  Kalk und gebranntem Gips",
    "C2351000" = "Herstellung von Zement",
    "C2360000" = "Herstellung von Erzeugnissen aus Beton  Zement und Gips",
    "C2361000" = "Herstellung von Erzeugnissen aus Beton",
    "C2370000" = "Be- und Verarbeitung von Naturwerk- und Natursteinen",
    "C2390000" = "Herstellung sonstiger nichtmetallischer Erzeugnisse a.n.g.",
    "C2391000" = "Herstellung von Schleifkörpern und Schleifmitteln",
    "C2400000" = "Metallerzeugung und -bearbeitung",
    "C2420000" = "Herstellung von Stahlrohren",
    "C2440000" = "Erzeugung und erste Bearbeitung von NE-Metallen",
    "C2450000" = "Gießereien",
    "C2450010" = "Eisen-  Stahl- und Temperguss",
    "C2450020" = "NE-Metallguss",
    "C2500000" = "Herstellung von Metallerzeugnissen",
    "C2510000" = "Stahl- und Leichtmetallbau",
    "C2511000" = "Herstellung von Metallkonstruktionen",
    "C2512000" = "Herstellung von Ausbauelementen aus Metall",
    "C2520000" = "Herstellung von Metalltanks  Heizkörpern und -kesseln",
    "C2521000" = "Herstellung von Heizkesseln und -körpern für Zentralheizungen",
    "C2521120" = "Herstellung von Heizkesseln für Zentralheizungen",
    "C2529000" = "Herstellung von Behältern",
    "C2550000" = "Herstellung von Schmiede-  Press-  Zieh- und Stanzteilen",
    "C2560000" = "Oberflächenveredlung  Wärmebehandlung und Mechanik a.n.g.",
    "C2562000" = "Mechanik a.n.g.",
    "C2570000" = "Herstellung von Schneidwaren  Werkzeugen  Schlössern",
    "C2571000" = "Herstellung von Schneidwaren und Bestecken",
    "C2572000" = "Herstellung von Schlössern und Beschlägen",
    "C2573000" = "Herstellung von Werkzeugen",
    "C2590000" = "Herstellung sonstiger Metallwaren",
    "C2594000" = "Herstellung von Schrauben und Nieten",
    "C2599000" = "Herstellung von sonstigen Metallwaren a.n.g.",
    "C2600000" = "Herstellung von Datenverarbeitungsgeräten",
    "C2620000" = "Herstellung von Datenverarbeitungsgeräten",
    "C2680000" = "Herstellung magnetischer und optischer Datenträger",
    "C2700000" = "Herstellung elektrischer Ausrüstungen",
    "C2710000" = "Herstellung von Elektromotoren und Transformatoren",
    "C2712000" = "Elektrizitätsverteilungs- und Schalteinrichtungen",
    "C2730000" = "Herstellung von Kabeln und Elektroinstallationsmaterial",
    "C2732000" = "Herstellung isolierter Elektrokabel  -leitungen und -drähte",
    "C2733000" = "Herstellung von elektrischem Installationsmaterial",
    "C2740000" = "Herstellung von elektrischen Lampen und Leuchten",
    "C2750000" = "Herstellung von Haushaltsgeräten",
    "C2800000" = "Maschinenbau",
    "C2810000" = "Herstellung nicht wirtschaftszweigspezifischer Maschinen",
    "C2812000" = "Herstellung von hydraulischen und pneumatischen Komponenten",
    "C2812110" = "Herstellung von Ölhydraulik",
    "C2812210" = "Herstellung von pneumatischen Steuerungen",
    "C2813010" = "Herstellung von Flüssigkeitspumpen",
    "C2814000" = "Herstellung von Armaturen a.n.g.",
    "C2815000" = "Herstellung  von Lagern  Getrieben  Zahnrädern",
    "C2815100" = "Herstellung von Lagern",
    "C2815200" = "Herstellung von Getrieben  Zahnrädern und Antriebselementen",
    "C2820000" = "Herstellung von nicht-branchenspezifischen Maschinen",
    "C2822000" = "Herstellung von Hebezeugen und Fördermitteln",
    "C2825000" = "Herstellung von kälte- und lufttechnischen Erzeugnissen",
    "C2829000" = "Herstellung sonstiger nicht spezifischer Maschinen a.n.g.",
    "C2830000" = "Herstellung von land- und forstwirtschaftlichen Maschinen",
    "C2840000" = "Herstellung von Werkzeugmaschinen",
    "C2841000" = "Herstellung von Werkzeugmaschinen für die Metallbearbeitung",
    "C2849200" = "Herstellung von Maschinen zur Bearbeitung harter Stoffe",
    "C2890000" = "Herstellung von Maschinen für sonstige bestimmte Branchen",
    "C2891000" = "Herstellung von Metallerzeugungsmaschinen",
    "C2892000" = "Herstellung von Bergwerks-  Bau- und Baustoffmaschinen",
    "C2892200" = "Herstellung von Bau- und Baustoffmaschinen",
    "C2893000" = "Herstellung von Maschinen für das Ernährungsgewerbe",
    "C2894000" = "Herstellung von Maschinen für das Textilgewerbe",
    "C2896000" = "Herstellung von Gummi- und Kunststoffindustriemaschinen",
    "C2899000" = "Herstellung sonstiger branchespezifischer Maschinen a.n.g.",
    "C2900000" = "Herstellung von Kraftwagen und Kraftwagenteilen",
    "C2910000" = "Herstellung von Kraftwagen und -motoren",
    "C2911000" = "Herstellung von Nutzkraftwagen",
    "C2912000" = "Herstellung von Personenkraftwagen einschließlich Kombis",
    "C2920000" = "Herstellung von Karosserien  Aufbauten und Anhängern",
    "C2930000" = "Herstellung von Teilen und Zubehör für Kraftwagen",
    "C3000000" = "Sonstiger Fahrzeugbau",
    "C3100000" = "Herstellung von Möbeln",
    "C3101000" = "Herstellung von Büro- und Ladenmöbeln",
    "C3109000" = "Herstellung von sonstigen Möbel",
    "C3109040" = "Herstellung von Polstermöbeln",
    "C3200000" = "Herstellung von sonstigen Waren",
    "C3250000" = "Herstellung von medizinischen Apparaten und Materialien",
    "C3250200" = "Herstellung von medizinmechanischen Erzeugnissen",
    "C3291000" = "Herstellung von Besen und Bürsten",
    "CONG0000" = "Verarbeitendes Gewerbe (ohne Ernährungsgewerbe)",
    "CONG00C0" = "Herstellung von Konsumgütern (ohne Ernährungsgewerbe)",
    "CONG0CN0" = "Herstellung von Verbrauchsgütern (ohne Ernährungsgewerbe)"
  )
  
  name <- industry_names[code]
  if (is.na(name)) return(code) 
  return(as.character(name))
}

get_question_name <- function(code) {
  question_names <- c(
    "KLD" = "Business Climate",
    "GUS" = "Current Situation Assessment",
    "GES" = "Business Expectations",
    "LUS" = "Finished Goods Inventory",
    "BUS" = "Order Book Assessment",
    "XUS" = "Export Order Assessment",
    "AVS" = "Demand vs Previous Month",
    "BVS" = "Order Book vs Previous Month",
    "QVS" = "Production vs Previous Month",
    "PVS" = "Prices vs Previous Month",
    "QES" = "Production Plans/Expectations",
    "PWS" = "Price Expectations",
    "XES" = "Export Expectations",
    "BES" = "Employment Expectations",
    "PRS" = "Productivity Expectations"
  )
  name <- question_names[code]
  if (is.na(name)) return(code)
  return(as.character(name))
}

get_question_name_en <- function(code) {
  question_names <- c(
    "KLD" = "Business Climate",
    "GUS" = "Current Situation Assessment",
    "GES" = "Business Expectations",
    "LUS" = "Finished Goods Inventory",
    "BUS" = "Order Book Assessment",
    "XUS" = "Export Order Assessment",
    "AVS" = "Demand vs Previous Month",
    "BVS" = "Order Book vs Previous Month",
    "QVS" = "Production vs Previous Month",
    "PVS" = "Prices vs Previous Month",
    "QES" = "Production Plans",
    "PWS" = "Price Expectations",
    "XES" = "Export Expectations",
    "BES" = "Employment Expectations",
    "PRS" = "Productivity Expectations"
  )
  name <- question_names[code]
  if (is.na(name)) return(code)
  return(as.character(name))
}


get_question_type <- function(code) {
  question_types <- c(
    "KLD" = "Composite",
    "GUS" = "Current", "LUS" = "Current", "BUS" = "Current", "XUS" = "Current",
    "GES" = "Expectation", "QES" = "Expectation", "XES" = "Expectation", 
    "BES" = "Expectation", "PRS" = "Expectation",
    "AVS" = "Comparison", "BVS" = "Comparison", "QVS" = "Comparison", "PVS" = "Comparison",
    "PWS" = "Price"
  )
  
  type <- question_types[code]
  if (is.na(type)) return("Other")
  return(as.character(type))
}

get_question_group <- function(code) {
  question_groups <- c(
    # Group A: Current business situation (contemporaneous)
    "GUS" = "A: Current",
    "LUS" = "A: Current",
    "BUS" = "A: Current",
    "XUS" = "A: Current",
    # Group B: Business expectations (forward-looking)
    "GES" = "B: Expectations",
    "QES" = "B: Expectations",
    "XES" = "B: Expectations",
    "BES" = "B: Expectations",
    "PRS" = "B: Expectations",
    # Group C: Price-related (inflation signals)
    "PVS" = "C: Prices",
    "PWS" = "C: Prices",
    # Group D: Month-over-month changes (short-term dynamics)
    "AVS" = "D: MoM Changes",
    "BVS" = "D: MoM Changes",
    "QVS" = "D: MoM Changes",
    # Composite
    "KLD" = "Composite"
  )
  group <- question_groups[code]
  if (is.na(group)) return("Other")
  return(as.character(group))
}

# ===============================================================================
# SECTION 3: DATA PREPROCESSING
# ===============================================================================

preprocess_ifo_data <- function(df) {
  df <- df %>%
    mutate(date = as.Date(paste0("01/", date), format = "%d/%m/%Y"))
  
  df <- df %>%
    rename_with(~ gsub("§.*$", "", .x), .cols = -c(date, industry_code)) %>%
    rename_with(~ gsub("<U\\+[0-9A-F]+>", "", .x), .cols = -c(date, industry_code)) %>%
    rename_with(~ trimws(.x), .cols = -c(date, industry_code))
  
  question_cols <- setdiff(names(df), c("date", "industry_code"))
  df <- df %>%
    mutate(across(all_of(question_cols), as.numeric))
  
  df <- df %>%
    mutate(level = vapply(industry_code, get_level, integer(1)))
  
  return(df)
}

# ===============================================================================
# SECTION 4: DATA TRANSFORMATION
# ===============================================================================

convert_to_long <- function(df_wide) {
  question_cols <- setdiff(names(df_wide), c("date", "industry_code", "level"))
  
  df_long <- df_wide %>%
    pivot_longer(
      cols = all_of(question_cols),
      names_to = "question",
      values_to = "value"
    )
  
  return(df_long)
}

add_names <- function(df_long) {
  df_long <- df_long %>%
    mutate(
      industry_name = vapply(industry_code, get_industry_name, character(1)),
      question_name = vapply(question, get_question_name, character(1)),
      question_type = vapply(question, get_question_type, character(1)),
      variable = paste(industry_code, question, sep = "_")
    )
  
  return(df_long)
}

# ===============================================================================
# SECTION 5: MAIN EXECUTION FUNCTION
# ===============================================================================

main <- function(data_path) {
  
  # Step 1: Read data 
  df_intermediate <- read_ifo_data(data_path)
  
  # Step 2: Preprocess
  df_intermediate <- preprocess_ifo_data(df_intermediate)
  
  # Step 3: Convert to long format
  df_long <- convert_to_long(df_intermediate)
  
  # Step 4: Add names to long format
  df_long <- add_names(df_long)
  
  # Step 5: Create wide format
  df_wide <- df_long %>%
    select(date, industry_code, question, value) %>%
    pivot_wider(
      names_from = c(industry_code, question),
      names_sep = "_",
      values_from = value
    )
  
  return(list(
    data_long = df_long,
    data_wide = df_wide 
  ))
}

