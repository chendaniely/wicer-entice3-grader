gle <- function(pt_value, comparison_value, less=1, equal=2, greater=3) {
    if (is.na(pt_value) | is.na(comparison_value)) {
        return(NA)
    }
    if (pt_value < comparison_value) {
        return(less)
    } else if (pt_value == comparison_value) {
        return(equal)
    } else if (pt_value > comparison_value) {
        return(greater)
    } else {
        return(NA)
    }
}

gle_range <- function(pt_value, comparison_value, buffer=5) {
    if (is.na(pt_value) | is.na(comparison_value)) {
        return(NA)
    }
    if (pt_value < comparison_value - buffer) {
        return(1)
    } else if (pt_value >= comparison_value - buffer & pt_value <= comparison_value + buffer) {
        return(2)
    } else if (pt_value > comparison_value + buffer) {
        return(3)
    } else {
        return(NA)
    }
}

calculate_panel_colver_ck <- function(value) {
    if (!value %in% c(0, 1, 2, 3, 4)) {
        return(NA)
    } else {
        return(value + 1)
    }
}
