raw <- read.csv ("./data/Raw/bagmati_raw_inventory.csv")

#Mean function creation

mean <- function(x) {
cal_mean <- sum(x, na.rm = TRUE) / length(x[!is.na(x)])
return(cal_mean)
}

mean(raw$DBH_cm)

#sd function creation
sum_sq_diff <- function(x) {
    sum(( x - mean(x))^2, na.rm = TRUE)
}

my_sd <- function(x) {
    sqrt(sum_sq_diff(x) / (length(x[!is.na(x)])-1))
}

m <- mean(raw$DBH_cm)
s <- my_sd(raw$DBH_cm)

#for cv

perc.cv <- function(x) {
    if (any(!is.numeric(x))) {
        stop("Error: DBH must be numeric")
    }
    if (any(x <= 0, na.rm = TRUE)) {
        stop("Error: DBH must be greater than 0")
    }
    if (any(x > 150, na.rm = TRUE)) {
        warning("Warning: DBH > 150 cm detected. Did you record in mm?")
    }

    cv <- (m / s) * 100
    return(cv)
}

x <- c(12,13,25,-11,NA)
#Cleaning Data
cleaning_data <- function(data_to_clean){
    cleaned_data <- data_to_clean[data_to_clean >0]
    cleaned_data <- cleaned_data[!is.na(cleaned_data)]
    return(cleaned_data)
}

cleaning_data(x)
cle <- cleaning_data(x)
cle
