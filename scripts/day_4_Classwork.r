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
}
perc.cv <- function(x) {
    cv <- (m / s) * 100
    return(cv)
}
perc.cv(raw$DBH_cm)
