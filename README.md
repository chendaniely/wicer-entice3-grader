# wicer-entice3-grader
Repository to check responses from wicer-entice3

# How to run

1. Clone/download this repository
  1. click the green clone/download button above
  2. either use `git` to clone the repository
    1. copy the linke
    2. go to the directory you want the folder to be downloaded into
    3. type `git clone LINK_YOU_JUST_COPIED`
  3. or click download zip
2. use RStudio to open the `wicer-entice3-grader.Rproj` file
3. in the `Files` tab in rstudio (bottom right corner) open the `src/00-config.R` file
4. edit the data path
  1. around line 17 there is a line that reads: `FULL_DATA <- 'data/Dummy infographics data.xlsx'`
  2. edit the file in quotes such that it points to the actual dataset
5. Run the script  
  There are 3 ways you can do this:
  1. select all (`ctrl+a`) and hit `ctrl+enter`
  2. click the `source` button
  3. hit `ctrl+shift+s`

the combined results will be in the `output` folder

# Notes

This repo replies on some of the 'mistakes' from the wicer-entice3 project

1. the `proms_depression_t` variable in the column names is spelled wrong (was suppoed to be `promis_depression_t`)
2. there is no column name for the first column
