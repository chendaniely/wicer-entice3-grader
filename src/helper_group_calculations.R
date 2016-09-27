calculate_age_group <- function(pt_age){
    # 18-30; 31-50; 51-64; 65+
    if (is.na(pt_age)) {
        return(NA)
    }
    if (pt_age >= 18 & pt_age <= 30) {
        return("18-30")
    } else if (pt_age >= 31 & pt_age <= 50) {
        return("31-50")
    } else if (pt_age >= 51 & pt_age <= 64) {
        return("51-64")
    } else if (pt_age >= 65) {
        return("65+")
    }
}

calculate_fruit_servings_recommended <- function(pt_sex, pt_age){
    if (pt_sex == 1 | pt_sex == 'M') {
        return(2)
    } else if (pt_sex == 2 | pt_sex == 'F') {
        if (pt_age >= 18 & pt_age <= 30) {
            return(2)
        } else{
            return(1.5)
        }
    }
}

calculate_vegetable_servings_recommended <- function(pt_sex, pt_age){
    if (pt_sex == 1 | pt_sex == 'M') {
        if (pt_age < 50) {
            return(3)
        } else {
            return(2.5)
        }
    } else if (pt_sex == 2 | pt_sex == 'F') {
        if (pt_age < 50) {
            return(2.5)
        } else {
            return(2)
        }
    }
}

calculate_vigorous_exercise_recommended <- function(pt_sex, pt_age) {
    return(75)
}

calculate_moderate_exercise_recommended <- function(pt_sex, pt_age) {
    return(150)
}

calculate_depression_gauge <- function(depression_value) {
    if (depression_value >= 0 & depression_value <= 4) {
        return(1) # very low
    } else if (depression_value >= 5 & depression_value <= 9) {
        return(2) # low
    } else if (depression_value >= 10 & depression_value <= 14) {
        return(3) # moderate
    } else if (depression_value >= 15 & depression_value <= 19) {
        return(4) # high
    } else if (depression_value >= 20 & depression_value <= 27) {
        return(5) # very high
    } else {
        return(NA)
    }
}

calculate_stress_gauge <- function(stress_value) {
    # prolonged stress
    if (stress_value >= 0 & stress_value <= 1) {
        return(1) # very low
    } else if (stress_value == 2) {
        return(2) # low
    } else if (stress_value == 3) {
        return(3) # moderate
    } else if (stress_value == 4) {
        return(4) # high
    } else if (stress_value == 5) {
        return(5) # very high
    } else {
        return(NA)
    }
}

calculate_pt_bmi_cat <- function(pt_bmi_value){
    if (is.na(pt_bmi_value)) {
        return(NA)
    } else if (pt_bmi_value <= 0) {
        stop("BMI is less than or equal to 0")
    } else if (pt_bmi_value < 18.5) {
        return(1) # underweight
    } else if (pt_bmi_value >= 18.5 & pt_bmi_value < 25) {
        return(2) # normal
    } else if (pt_bmi_value >= 25 & pt_bmi_value < 30) {
        return(3) # overweight
    } else if (pt_bmi_value >= 30) {
        return(4) # obese
    }
    else{
        stop("unknown pt_bmi_value value passed")
    }
}

calculate_pt_waist_in_cat <- function(pt_waist_in, pt_sex){
    if (is.na(pt_waist_in)) {
        return(NA)
    } else if (pt_waist_in <= 15) {
        stop("Waist inch measure is less than or equal to 15, this doesn't make sense")
    } else if (tolower(pt_sex) == "male" | pt_sex == 1 | pt_sex == "M") {
        if (pt_waist_in < 40) {
            return(1)
        } else if (pt_waist_in >= 40) {
            return(2)
        } else{
            stop("unknown weight size for male")
        }
    } else if (tolower(pt_sex) == 'female' | pt_sex == 2 | pt_sex == "F") {
        if (pt_waist_in < 35) {
            return(1)
        } else if (pt_waist_in >= 35) {
            return(2)
        } else{
            stop("unknown waist size for female")
        }
    } else{
        stop("Unknown pt_sex passed into calculate_pt_waist_in_cat")
    }
}

calculate_pt_bp_s_cat <- function(pt_bp_s){
    if (pt_bp_s <= 120) {
        return(1) # healthy
    } else if (pt_bp_s < 140) {
        return(2) # borderline
    } else if (pt_bp_s >= 140) {
        return(3) # unhealthy
    }
}

calculate_pt_bp_d_cat <- function(pt_bp_d){
    if (pt_bp_d <= 80) {
        return(1) # healthy
    } else if (pt_bp_d < 90) {
        return(2) # borderline
    } else if (pt_bp_d >= 90) {
        return(3) # unhealthy
    }
}

is_between <- function(x, a, b) {
    x > a & x < b
}

get_health_summary_colors <- function(prolonged_stress_value,
                                      bmi_value,
                                      oral_health_value,
                                      blood_pressure_s_value,
                                      blood_pressure_d_value) {
    if (prolonged_stress_value == 0) {
        prolonged_stress_color <- "Green"
    } else if (prolonged_stress_value == 1 | prolonged_stress_value == 2) {
        prolonged_stress_color <- "Yellow"
    } else if (prolonged_stress_value >= 3) {
        prolonged_stress_color <- "Red"
    } else {
        stop("Unknown value for prolonged_stress")
    }

    bmi_cat <- calculate_pt_bmi_cat(bmi_value)
    # 1: underweight
    # 2: normal
    # 3: overweight
    # 4: obese
    if (bmi_cat == 2) {
        bmi_color <- "Green"
    } else if (bmi_cat == 1 | bmi_cat == 3) {
        bmi_color <- "Yellow"
    } else if (bmi_cat == 4) {
        bmi_color <- "Red"
    } else {
        stop("Unknown value for bmi_cat and/or bmi_value")
    }

    if (oral_health_value %in% c(1, 2)) {
        oral_health_color <- "Green"
    } else if (oral_health_value == 3) {
        oral_health_color <- "Yellow"
    } else if (oral_health_value == 4) {
        oral_health_color <- "Red"
    } else {
        stop("Unknown value for oral_health")
    }

    if (blood_pressure_s_value <= 120 & blood_pressure_d_value <= 80) {
        blood_pressure_color <- "Green"
    } else if (is_between(blood_pressure_s_value, 120, 140) | is_between(blood_pressure_d_value, 80, 90)) {
        blood_pressure_color <- "Yellow"
    } else if (blood_pressure_s_value >= 140 | blood_pressure_d_value >= 90) {
        blood_pressure_color <- "Red"
    } else {
        stop("Unknown blood_pressure_s_value and/or blood_pressure_d_value")
    }

    color_values = c(prolonged_stress_color,
                     bmi_color,
                     oral_health_color,
                     blood_pressure_color)
    borderline = color_values == 'Yellow'
    return(sum(borderline))
}


calculate_cloverleaf_fruit_value <- function(fruit_value, vegetable_value,
                                             pt_sex, pt_age){
    fruit_min <- calculate_fruit_servings_recommended(
        pt_sex, pt_age)

    vegetable_min <- calculate_vegetable_servings_recommended(
        pt_sex, pt_age)
    if (is.na(fruit_value)){
        fruit_value <- 0
    }
    if (is.na(vegetable_value)){
        vegetable_value <- 0
    }
    if (fruit_value == 0 & vegetable_value == 0){
        return("Poor")
    } else if (fruit_value >= fruit_min & vegetable_value >= vegetable_min){
        return("Excellent")
    } else if (fruit_value >= fruit_min | vegetable_value >= vegetable_min){
        return("Good")
    } else if (fruit_value > 0 | vegetable_value > 0){
        return("Fair")
    }
}

#'full petal for 0 days (excellent/excelente).
#'Largish petal for 1-5 days (good/buena).
#'Medium petal for 6-10 days (fair/regular).
#'Small petal for >10 days (poor/mala).
calculate_cloverleaf_mental_value <- function(mental_value){
    if (is.na(mental_value)){
        return(NA)
    }
    if (mental_value == 0){
        return("Excellent")
    } else if (mental_value >= 1 & mental_value <= 5){
        return("Good")
    } else if (mental_value >= 6 & mental_value <= 10){
        return("Fair")
    } else if (mental_value > 10){
        return("Poor")
    } else {
        return(NA)
    }
}

#' Five petal sizes correspond to poor, fair, good, very good, and
#' excellent / mala, regular, buena, muy buena, excelente
calculate_cloverleaf_overall_value <- function(overall_value){
    if (is.na(overall_value)){
        return(NA)
    } else if (overall_value == 1){
        return("Poor")
    } else if (overall_value == 2){
        return("Fair")
    } else if (overall_value == 3){
        return("Good")
    } else if (overall_value == 4){
        return("Very Good")
    } else if (overall_value == 5){
        return("Excellent")
    }
}

#' [moderate + 2(vigorous)= adjusted minutes].
#' Full petal if adjusted minutes > 150 (excellent/excelente).
#' Middle petal if 75 < adj. min. < 150 (good/bueno).
#' Small petal if 0 < adj. min. <     75 min (fair/bajo).
#' Tiny petal if zero (poor/muy bajo)
calculate_cloverleaf_physical_value <- function(moderate_value, vigorous_value){
    moderate_value <- as.numeric(moderate_value)
    vigorous_value <- as.numeric(vigorous_value)

    if (is.na(moderate_value)){
        moderate_value <- 0
    }
    if (is.na(vigorous_value)){
        vigorous_value <- 0
    }
    adj_mins <- moderate_value + (2 * vigorous_value)
    if (adj_mins == 0){
        return("Poor")
    } else if (adj_mins < 75){
        return("Fair")
    } else if (adj_mins < 150){
        return("Good")
    } else if (adj_mins >= 150){
        return("Excellent")
    }
}

calculate_cloverleaf_ck <- function(fruit, physical, overall, mental) {
    values <- c(fruit, physical, overall, mental)
    ideal_values <- values == "Excellent"
    count_ideal <- sum(ideal_values)
    return(count_ideal)
}
