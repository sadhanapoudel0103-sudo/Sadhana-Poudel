raw <- read.csv("./data/Raw/bagmati_raw_inventory.csv")

cat("Total rows:", nrow(raw), "\n")
cat("Total columns:", ncol(raw), "\n")

str(raw)
print(sapply(raw, class))

cat("Number of forest stands:", length(unique(raw$Stand_ID)), "\n")
cat("Number of plots:", length(unique(raw$Plot_ID)), "\n")
cat("Number of tree records:", nrow(raw), "\n")

print(table(raw$Stand_ID))

print(colSums(is.na(raw)))

print(table(raw$Species))

raw$Species <- trimws(raw$Species)
raw$Species[raw$Species == "S. robusta"] <- "Shorea robusta"
raw$Species[raw$Species == "Shorea rubusta"] <- "Shorea robusta"
raw$Species[raw$Species == "Schima wallichi"] <- "Schima wallichii"
raw$Species[raw$Species == "C. indica"] <- "Castanopsis indica"
cat("Species after fixing names:", length(unique(raw$Species)), "\n")
print(table(raw$Species))

duplicate_rows <- duplicated(raw[, c("Stand_ID", "Plot_ID", "Tree_ID")])
cat("Duplicate tree records found:", sum(duplicate_rows), "\n")
print(raw[duplicate_rows, c("Stand_ID", "Plot_ID", "Tree_ID", "DBH_cm", "Biomass_kg")])

raw <- raw[!duplicate_rows, ]
cat("Rows after removing duplicates:", nrow(raw), "\n")

dbh_problem <- is.na(raw$DBH_cm) | raw$DBH_cm <= 0 | raw$DBH_cm > 150
cat("Trees with DBH problems:", sum(dbh_problem, na.rm = TRUE), "\n")

height_problem <- raw$Height_m <= 0
cat("Trees with height problems:", sum(height_problem, na.rm = TRUE), "\n")

raw$check_needed <- dbh_problem | height_problem
raw$check_needed[is.na(raw$check_needed)] <- TRUE
cat("Trees marked for further investigation:", sum(raw$check_needed), "\n")



carbon_fraction <- 0.47
raw$carbon_kg <- raw$Biomass_kg * carbon_fraction


raw$carbon_Mg <- raw$carbon_kg / 1000

print(head(raw[, c("Stand_ID", "Plot_ID", "Tree_ID", "Biomass_kg", "carbon_kg", "carbon_Mg")]))

stand_carbon <- function(stand_data, min_trees = 30) {

  stand_name <- stand_data$Stand_ID[1]


  if (!is.data.frame(stand_data)) {
    stop("Input must be a data frame")
  }

  needed_columns <- c("Plot_ID", "Species", "Biomass_kg", "Plot_area_ha", "Wood_density")
  if (!all(needed_columns %in% names(stand_data))) {
    stop("Some required columns are missing")
  }

  missing_rows <- is.na(stand_data$Biomass_kg) | is.na(stand_data$Plot_area_ha)
  if (sum(missing_rows) > 0) {
    message(stand_name, ": removed ", sum(missing_rows), " trees with missing biomass or plot area")
  }
  stand_data <- stand_data[!missing_rows, ]

  
  if (nrow(stand_data) < min_trees) {
    stop(stand_name, ": too few trees (", nrow(stand_data), "), at least ", min_trees, " needed")
  }

  if (any(stand_data$Biomass_kg <= 0)) {
    stop(stand_name, ": biomass must be greater than 0")
  }

  
  if (any(stand_data$Plot_area_ha <= 0 | stand_data$Plot_area_ha > 1)) {
    stop(stand_name, ": plot area must be between 0 and 1 ha")
  }

  if (any(stand_data$Wood_density < 0.1 | stand_data$Wood_density > 1.2)) {
    stop(stand_name, ": wood density must be between 0.1 and 1.2 g/cm3")
  }

  large_trees <- sum(stand_data$Biomass_kg > 3000)
  if (large_trees > 0) {
    message(stand_name, ": ", large_trees, " trees have biomass above 3000 kg, please check")
  }

  
  if ("check_needed" %in% names(stand_data)) {
    flagged <- sum(stand_data$check_needed)
    if (flagged > 0) {
      message(stand_name, ": ", flagged, " trees have DBH or height problems, kept because biomass is valid")
    }
  }

 
  carbon_kg <- stand_data$Biomass_kg * 0.47
  carbon_Mg <- carbon_kg / 1000

 
  total_carbon_Mg <- sum(carbon_Mg)

 
  one_row_per_plot <- !duplicated(stand_data$Plot_ID)
  total_area_ha <- sum(stand_data$Plot_area_ha[one_row_per_plot])

  
  carbon_density <- total_carbon_Mg / total_area_ha

  
  result <- c(
    Total_trees            = nrow(stand_data),
    Species_richness       = length(unique(stand_data$Species)),
    Max_tree_biomass_kg    = max(stand_data$Biomass_kg),
    Total_carbon_Mg        = total_carbon_Mg,
    Total_sampled_area_ha  = total_area_ha,
    Carbon_density_MgC_ha  = carbon_density
  )
  return(result)
}


stand_list <- split(raw, raw$Stand_ID)
cat("Stands found:", names(stand_list), "\n")


summary_matrix <- sapply(stand_list, stand_carbon)


by(raw, raw$Stand_ID, function(d) round(sum(d$carbon_Mg), 3))


summary_matrix <- round(summary_matrix, 3)
print(summary_matrix)


density_row <- summary_matrix["Carbon_density_MgC_ha", ]
cat("Highest carbon density:", names(which.max(density_row)), "=", max(density_row), "Mg C/ha\n")
cat("Lowest carbon density:", names(which.min(density_row)), "=", min(density_row), "Mg C/ha\n")


write.csv(summary_matrix, "outputs/summary_stand_carbon.csv")
cat("Summary saved to outputs/summary_stand_carbon.csv\n")
