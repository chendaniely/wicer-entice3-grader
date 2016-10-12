# ck_value_index <- str_detect(string = ls(), pattern = CK_PATTERN)
# expect_equal(sum(ck_value_index), 22)

ck_vars <- ls(pattern = CK_PATTERN)

expect_equal(length(ck_vars), 23)

ck_value <- c(rep(x = NA, 23))
names(ck_value) <- ck_vars
expect_equal(length(ck_value), 23)

for (i in 1:length(ck_vars)) {
    ck_value[i] <- get(ck_vars[i])
}

ck_value <- c('patient_id' = PT_ID, ck_value)

pt_write_value <- as.matrix(ck_value) %>% t() %>% as.data.frame()
pt_write_value

if (any(is.na(pt_write_value))){
    stop("Something's wrong, I found a missing value.")
}

write.csv(x = pt_write_value, file = sprintf('output/%s.csv', PT_ID), row.names = FALSE)
