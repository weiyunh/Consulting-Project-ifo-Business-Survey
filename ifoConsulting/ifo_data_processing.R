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

extract_industry_code <- function(var_name) {
  strsplit(var_name, "_")[[1]][1]
}

extract_question_code <- function(var_name) {
  strsplit(var_name, "_")[[1]][2]
}

# English Name mapping functions
get_industry_name_en <- function(code) {
  
  industry_names_en <- c(
    
    "C0000000" = "Manufacturing",
    "CONG0000" = "Manufacturing (excluding food industry)",
    
    "C00000A0" = "Intermediate Goods",
    "C00000B0" = "Capital Goods",
    "C00000C0" = "Consumer Goods",
    "C0000CD0" = "Durable Consumer Goods",
    "C0000CN0" = "Non-Durable Consumer Goods",
    "CONG00C0" = "Consumer Goods (excluding food industry)",
    
    # Plastics
    "C2220000" = "Manufacture of Plastic Products",
    "C2221000" = "Manufacture of Plastic Plates, Sheets, Tubes and Profiles",
    "C2222000" = "Manufacture of Plastic Packaging",
    "C2223000" = "Manufacture of Plastic Construction Materials",
    "C2229000" = "Other Plastic Products",
    
    # Non-metallic minerals
    "C2300000" = "Manufacture of Other Non-Metallic Mineral Products",
    "C2310000" = "Manufacture of Glass and Glass Products",
    "C2312000" = "Shaping and Processing of Flat Glass",
    "C2313000" = "Manufacture of Hollow Glass",
    "C2319000" = "Manufacture of Technical Glass",
    "C2330000" = "Manufacture of Ceramic Building Materials",
    "C2340000" = "Manufacture of Other Ceramic Products",
    "C2341000" = "Manufacture of Ceramic Household Articles",
    "C2350000" = "Manufacture of Cement, Lime and Plaster",
    "C2351000" = "Manufacture of Cement",
    "C2360000" = "Manufacture of Articles of Concrete, Cement and Plaster",
    "C2361000" = "Manufacture of Concrete Products",
    "C2370000" = "Cutting and Shaping of Stone",
    "C2390000" = "Other Non-Metallic Mineral Products n.e.c.",
    "C2391000" = "Manufacture of Abrasive Products",
    
    # Metals
    "C2400000" = "Basic Metals",
    "C2420000" = "Manufacture of Steel Tubes",
    "C2440000" = "Production and Processing of Non-Ferrous Metals",
    "C2450000" = "Foundries",
    "C2450010" = "Iron, Steel and Malleable Cast Iron Foundries",
    "C2450020" = "Non-Ferrous Metal Foundries",
    
    "C2500000" = "Fabricated Metal Products",
    "C2510000" = "Structural Metal Products",
    "C2511000" = "Manufacture of Metal Structures",
    "C2512000" = "Manufacture of Metal Doors and Windows",
    "C2520000" = "Manufacture of Metal Tanks and Radiators",
    "C2521000" = "Manufacture of Central Heating Boilers and Radiators",
    "C2521120" = "Manufacture of Central Heating Boilers",
    "C2529000" = "Manufacture of Containers",
    "C2550000" = "Forging, Pressing, Stamping and Roll-Forming of Metal",
    "C2560000" = "Surface Treatment and Mechanical Engineering n.e.c.",
    "C2562000" = "Mechanical Engineering n.e.c.",
    "C2570000" = "Manufacture of Cutlery, Tools and Locks",
    "C2571000" = "Manufacture of Cutlery",
    "C2572000" = "Manufacture of Locks and Fittings",
    "C2573000" = "Manufacture of Tools",
    "C2573010" = "Manufacture of Tools (excluding precision tools)",
    "C2573020" = "Manufacture of Precision Tools",
    "C2590000" = "Other Fabricated Metal Products",
    "C2592000" = "Manufacture of Metal Packaging and Closures",
    "C2594000" = "Manufacture of Fasteners",
    "C2600000" = "Computer, Electronic and Optical Products",
    "C2610000" = "Manufacture of Electronic Components",
    "C2620000" = "Manufacture of Computers and Peripheral Equipment",
    "C2630000" = "Manufacture of Communication Equipment",
    "C2651000" = "Manufacture of Measuring and Navigation Instruments",
    "C2651100" = "Manufacture of Electrical Measuring Instruments",
    "C2651200" = "Manufacture of Optical Measuring Instruments",
    "C2670000" = "Manufacture of Optical and Photographic Equipment",
    "C2670010" = "Manufacture of Optical Instruments and Lasers",
    
    # Electrical equipment
    "C2700000" = "Electrical Equipment",
    "C2710000" = "Electric Motors, Generators and Transformers",
    "C2711000" = "Manufacture of Electric Motors and Generators",
    "C2712000" = "Electricity Distribution and Control Equipment",
    "C2730000" = "Manufacture of Wiring and Wiring Devices",
    "C2732000" = "Manufacture of Insulated Wires and Cables",
    "C2733000" = "Manufacture of Wiring Devices",
    "C2740000" = "Manufacture of Electric Lighting Equipment",
    "C2750000" = "Manufacture of Domestic Appliances",
    
    # Machinery
    "C2800000" = "Machinery and Equipment",
    "C2810000" = "General Purpose Machinery",
    "C2812000" = "Manufacture of Fluid Power Equipment",
    "C2812110" = "Manufacture of Hydraulic Equipment",
    "C2812210" = "Manufacture of Pneumatic Control Equipment",
    "C2813010" = "Manufacture of Pumps",
    "C2814000" = "Manufacture of Valves n.e.c.",
    "C2815000" = "Manufacture of Bearings, Gears and Drive Elements",
    "C2815100" = "Manufacture of Bearings",
    "C2815200" = "Manufacture of Gears and Drive Elements",
    "C2820000" = "Special Purpose Machinery",
    "C2822000" = "Manufacture of Lifting and Handling Equipment",
    "C2825000" = "Manufacture of Refrigeration and Ventilation Equipment",
    "C2829000" = "Other Special Purpose Machinery n.e.c.",
    "C2830000" = "Agricultural and Forestry Machinery",
    "C2840000" = "Machine Tools",
    "C2841000" = "Machine Tools for Metal Working",
    "C2849200" = "Machine Tools for Hard Materials",
    "C2890000" = "Machinery for Specific Industries",
    "C2891000" = "Metallurgical Machinery",
    "C2892000" = "Mining and Construction Machinery",
    "C2892200" = "Construction Machinery",
    "C2893000" = "Food Processing Machinery",
    "C2894000" = "Textile Machinery",
    "C2896000" = "Rubber and Plastics Machinery",
    "C2899000" = "Other Industry-Specific Machinery",
    
    # Automotive
    "C2900000" = "Motor Vehicles",
    "C2910000" = "Motor Vehicles and Engines",
    "C2911000" = "Commercial Vehicles",
    "C2912000" = "Passenger Cars",
    "C2920000" = "Bodies and Trailers",
    "C2930000" = "Motor Vehicle Parts and Accessories",
    
    # Other transport
    "C3000000" = "Other Transport Equipment",
    
    # Furniture
    "C3100000" = "Furniture",
    "C3101000" = "Office and Shop Furniture",
    "C3109000" = "Other Furniture",
    "C3109040" = "Upholstered Furniture",
    
    # Other manufacturing
    "C3200000" = "Other Manufacturing",
    "C3250000" = "Medical and Dental Instruments",
    "C3250200" = "Medical Mechanical Products",
    "C3291000" = "Brooms and Brushes",
    
    # Food
    "C00FOBE0" = "Food, Beverage and Tobacco",
    "C1000000" = "Food Products",
    "C1010000" = "Meat Processing",
    "C1020000" = "Fish Processing",
    "C1030000" = "Fruit and Vegetable Processing",
    "C1050000" = "Dairy Products",
    "C1060000" = "Grain Milling and Starch Products",
    "C1070000" = "Bakery and Pasta Products",
    "C1071000" = "Bakery Products (excluding long-life bakery products)",
    "C1080000" = "Other Food Products",
    "C1082000" = "Sugar and Confectionery Products",
    "C1090000" = "Animal Feed",
    "C1100000" = "Beverages",
    "C1101000" = "Spirits",
    "C1105000" = "Beer",
    "C1107000" = "Soft Drinks and Mineral Water",
    
    # Textiles & apparel
    "C1300000" = "Textiles",
    "C1320000" = "Weaving",
    "C1321000" = "Cotton Weaving",
    "C1330000" = "Finishing of Textiles",
    "C1390000" = "Other Textiles",
    "C1392000" = "Made-up Textile Articles",
    "C1400000" = "Wearing Apparel",
    "C1410000" = "Wearing Apparel (excluding fur)",
    "C1413000" = "Outerwear",
    "C1414000" = "Underwear",
    "C1414100" = "Woven Underwear",
    "C1431000" = "Hosiery",
    
    # Leather
    "C1500000" = "Leather and Footwear",
    "C1510000" = "Leather Goods",
    "C1520000" = "Footwear",
    
    # Wood
    "C1600000" = "Wood Products (excluding furniture)",
    "C1610000" = "Sawmilling and Planing of Wood",
    "C1620000" = "Other Wood Products",
    "C1621000" = "Veneer and Wood-Based Panels",
    "C1623000" = "Prefabricated Building Elements",
    "C1624000" = "Wood Packaging",
    
    # Paper
    "C1700000" = "Paper Products",
    "C1710000" = "Pulp, Paper and Paperboard",
    "C1712000" = "Paper and Paperboard",
    "C1712100" = "Paper",
    "C1712200" = "Paperboard",
    "C1720000" = "Paper and Paperboard Products",
    "C1721000" = "Corrugated Paper and Packaging",
    "C1723000" = "Stationery Products",
    
    # Printing
    "C1800000" = "Printing",
    "C1810000" = "Printing Services",
    "C1811000" = "Newspaper Printing",
    "C1812000" = "Printing n.e.c.",
    "C1813000" = "Pre-Press and Media Services",
    
    # Petroleum
    "C1900000" = "Coke and Refined Petroleum Products",
    
    # Chemicals
    "C2000000" = "Chemicals",
    "C2010000" = "Basic Chemicals",
    "C2012000" = "Dyes and Pigments",
    "C2014000" = "Other Organic Basic Chemicals",
    "C2016000" = "Plastics in Primary Forms",
    "C2030000" = "Paints, Varnishes and Coatings",
    "C2040000" = "Cleaning and Personal Care Products",
    "C2050000" = "Other Chemical Products",
    
    # Pharma
    "C2100000" = "Pharmaceuticals",
    
    # Rubber
    "C2200000" = "Rubber and Plastics",
    "C2210000" = "Rubber Products",
    "C2219000" = "Other Rubber Products"
    
  )
  
  nm <- industry_names_en[code]
  if (is.na(nm)) return(code)
  return(as.character(nm))
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

