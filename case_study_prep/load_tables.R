library(data.table)
library(ggplot2)
library(tidyr)
library(magrittr)
library(gridExtra)
library(GGally)

# transport things explore :)

file_path <- file.path("/Users", "donatabuozyte", "Desktop", "Uni", "Master", "Datasets", "barcelona_data")

filenames <- list.files(file_path, pattern="*.csv", full.names=TRUE)
all_tables <- lapply(filenames, fread)

accidents <- all_tables[[1]]
air_quality <- all_tables[[2]]
air_stations <- all_tables[[3]]
births <- all_tables[[4]]
bus_stops <- all_tables[[5]]
deaths <- all_tables[[6]]
immigrants_by_nat <- all_tables[[7]]
immi_emi_by_age <- all_tables[[8]]
immi_emi_by_dest <- all_tables[[9]]
immi_emi_by_dest2 <- all_tables[[10]]
immi_emi_by_sex <- all_tables[[11]]
life_exp <- all_tables[[12]]
freq_baby_names <- all_tables[[13]]
freq_names <- all_tables[[14]]
pop <- all_tables[[15]]
transport <- all_tables[[16]]
unemployment <- all_tables[[17]]

all_summaries <- vector('list', 17)
for (i in 1:17) {
  all_summaries[[i]] <- local({
    i <- i
    p1 <- summary(all_tables[[i]])
  })
}
