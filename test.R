dhb <-28.1
ba <-1 pi * (dhb^2) / 40000

dbh <- 20
ht <- 10.01
tree_count <- 20L
species_name <- "Shorea Robusta"
terai_is_plain <- TRUE

typeof(dbh)
class(dbh)

class(ht)
typeof(ht)

typeof(tree_count)
class(tree_count)

typeof(species_name)
class(species_name)

typeof(terai_is_plain) #bujhns lso ,strs
class(terai_is_plain)
# verify types using is.*() functions is. numeric(dbh)
is.integer(tree_count)
is.numeric(dbh)
is.character(species_name)

a = c("5", 5, 78, "987")
as.numeric(a)
as.character(a)

b = c( a, 35, "45", 22, d, E, 55)
as.numeric(b)

s = c(12.5, '33', 53.1, '9', 11)
as.integer(s) 
plot_codes_factor <- factor( c( 101, 102,103))
as.numeric(plot_codes_factor)
as.numeric(as.character(plot_codes_factor))

forest_types <- c("Community", "Private", "Community","Protected")
forest_factor <- as.factor(forest_types)
class(forest_factor)
levels(forest_factor)

dbh > 20
dbh <= 10
species_chr
species_chr == "Shorea robusta"
species_chr!= "Pinux roxburghii"

(dbh > 30) & (height > 18)

(dbh >= 10) & (dbh <= 30)

(dbh <=0) | (dbh > 150)

!(dbh < 0)
# 3. Conditional Decision-Making with if/ else (for a single value
)
if (dbh >=30) {
    cat("classification: Sawtimber tree\n")
} else if (dbh >=10) {
    cat("classification: Pole-sized tree\n")
else{
    cat("classification: Sapling\n")
}}
#combine multiple DBH measurements into a numeric vector using c()
tree_dbh <- c(28.4, 35.1, 17.8, 42.0, 22.4)
# Vectorized math: R applies the basal area formula to all 5 trees simultaneously 
trees_ba <- pi * (trees_dbh^2) / 40000
round(trees_ba, digits = 3)

ifelse(test = trees_dbh >= 30, yes= "large stem", no= "Small/Medium Stem)")

#handling missing values Na in field data
heights <- c(19.2, 22.4, NA, 25.1, 17.1)

mean(heights)

mean(heights, na.rm = TRUE)

is.na(heights)
sum(is.na(heights))

test_tress <- c(28.4, -12.0, 0, 999, NA)
test_trees <= 0
test_trees > 150 & !is.na(test_trees)
any(test_trees <=0, na.rm = TRUE)

calculate_basal_area <- function(dbh_cm){
    if (!is.numeric(dbh_cm)){}
}
# Build a defensive Basal Area Function

calculate_basal_area <- function(dbh_cm) {
#Guard 1: Check data type
if (!is.numeric(dbh_cm)) {
    stop("Error: DBH must be numeric (measured in cm)!")
    }
    

#Guard 2: Stop on biologically impossible negative or zero values
    if(any(dbh_cm <=0, na.nm = TRUE)) {
    stop("Error: DBH must be strictly greater than 0!")
# Guared 3: Warn on potential unit errors (e.g, mm recorded insted of cm)
if (any(dbh_cm > 150, na.nm = TRUE){
warning("Warning: DBH > 150 cm detected. Did you record in mm?")
}

#ba_m2 <- pi * (dbh_cm^2) / 40000
return(ba_m2)

calculate_volume_of_tree <- function(h_cm, f_cm) {
if (!is.numeric(h_cm, f_cm) {
stop("Error: h & f must be numeric (measured in cm)")
}
if(any(h_cm <=0, na.nm = TRUE)){
    stop("Error: h & f must be greater than 0!")
    }
    if (any(h_cm > 150, f_cm > 150 na.nm = TRUE)){
        warning("warning: h > 150 cm detected. Did you record in mm?")
        warning("warning: f > 150 cm detected. Did you record in mm")
    }
    v <- {(pi* d^2)/4} * (h*f)
    return(v)
    }
calculate_volume_of_tree(12)


trees <- read.csv(data\Raw\unit2_starter_trees.csv)
head(trees)
head(trees, 3)
nrow(trees)
ncol(trees)
names(trees)
str(trees)

trees$dbh_cm

calc_cv(trees$dbh_cm)
trees$ba_m2 <- calculate_basal_area(trees$dbh_cm0)
head(trees)

trees$dbh_cm[trees$dbh_cm > 30]

trees$species[trees$dbh_cm > 30]

which.max(trees$dbh_cm)

trees[which.max(trees$dbh_cm),]
