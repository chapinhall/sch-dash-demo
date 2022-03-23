# Read sch data
# Generate loop across schools
# render 
# 

### Set environment ------------------------------------------------------------

packages_list <- 
  c("dplyr", "tidyr", "ggplot2", "scales", "DT", "data.table", "plotly", 
    "leaflet", "glue", "stringr", "crosstalk")
for (p in packages_list) {
  if (!p %in% installed.packages()[ "Packges"]) install.packages(p)
  library(p, character.only = TRUE)
}

root_path <- "C:/Users/nsmad/Documents/GitHub/sch-dash-demo/"
data_path   <- glue("{root_path}data/")
output_path <- glue("{root_path}output/")

### Read a set up data ---------------------------------------------------------

sch <- read.csv(glue("{data_path}Chicago_Public_Schools_-_School_Profile_Information_SY1819.csv"))
hsch <- 
  filter(sch, Primary_Category == "HS")

calc_cols <- 
  tribble(~field_suffix, ~source_suffix,
          "lowinc",     "Low_Income",
          "sped",       "Special_Ed",
          "ell",        "English_Learners",
          "raceeth_bl", "Black",
          "raceeth_hi", "Hispanic",
          "raceeth_wh", "White",
          "raceeth_as", "Asian",
          "raceeth_na", "Native_American",
          "raceeth_ot", "Other_Ethnicity",
          "raceeth_ap", "Asian_Pacific_Islander",
          "raceeth_mu", "Multi",
          "raceeth_hp", "Hawaiian_Pacific_Islander",
          "raceeth_un", "Ethnicity_Not_Available") %>% 
  mutate(var = paste0("pct_", field_suffix),
         var_label = str_replace_all(source_suffix, "_", " "),
         var_label = ifelse(str_detect(var, "raceeth"), 
                            paste0("Race/Eth: ", var_label),
                            var_label))

hsch_calc <- 
  hsch %>% 
  data.table() %>% 
  .[j = calc_cols$var := lapply(calc_cols$source_suffix,
                                function(s) get(paste0("Student_Count_", s)) / Student_Count_Total)]

hsch_avg <- 
  hsch_calc[j = lapply(.SD, mean, na.rm = TRUE),
            .SDcols = calc_cols$var] %>% 
  melt(measure.vars = colnames(.))


### Loop run of reports --------------------------------------------------------

for (hs in hsch_calc$Long_Name) {
  my_sch <- hsch_calc[Long_Name == hs]
  print(glue("Working on report for {hs}"))
  rmarkdown::render(input = glue("{root_path}sch_rpt_template.Rmd"),
                    output_file = glue("{output_path}High School Comparison Report -- {hs}.html"))
}
