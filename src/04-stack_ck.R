library(stringr)

rm(list = ls())

if (file.exists('output/all_ck.csv')) {
    file.remove('output/all_ck.csv')
}

single_cks <- list.files(path = 'output/', pattern = '*.csv')
base_names <- str_replace(single_cks, '\\.csv', '')

list_of_df <- vector(mode = 'list', length = length(base_names))
names(list_of_df) <- base_names

for (fn in base_names) {
    name_to_use <- fn
    value_to_use <- read.csv(sprintf('output/%s.csv', fn), stringsAsFactors = FALSE)
    expect_equal(value_to_use[1, 1], fn)

    assign(x = name_to_use, value = value_to_use)
    list_of_df[[fn]] <- value_to_use
}

stacked <- do.call(rbind, list_of_df)

write.csv(x = stacked, file = 'output/all_ck.csv',row.names = FALSE)
