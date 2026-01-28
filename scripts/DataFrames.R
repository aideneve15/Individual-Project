library(tidyverse) # for data manipulation and visualization
library(patchwork) # for combining plots
library(here) # for file path management
library(arrow) # for saving dataframes
library(ptw)

# Set the path to your folder
folder_path <- here::here("Data2")


# List all files in the folder
files <- list.files(path = folder_path, full.names = TRUE)

all_data_long <- tibble()

# iterate through files
for(i in 1:length(files)) {
  
  # Read the CSV file
  data <- read_csv(file = files[i], col_names = c("time", "intensity")) |> 
    mutate(file = files[i])
  
  all_data_long <- all_data_long |> 
    bind_rows(
      data
    )
}


dataLong <- all_data_long |> 
  mutate(basename = basename(file)) |> 
  select(-file) |> 
  separate_wider_delim(
    basename,
    delim = "_",
    names = c("enzyme", "instrument", "species", "sample", "time_point")
  ) |> 
  filter(time > 0.75 & time < 10) |> 
  mutate(
  time_point = str_extract(time_point, "^\\d+")
)

dataLong |> 
  filter(instrument == "DAD") |> 
  group_by(species) |> 
  ggplot(aes(x=time, y = intensity, color = time_point))+
  geom_point()+
  facet_wrap(~species)


all_times <- dataLong |> 
  select(time) |> 
  distinct() |> 
  arrange(time)

dataLong <- dataLong |> 
  group_by(enzyme, sample, instrument, species, time_point) |>
  group_modify(~ {
    interp_results <- approx(x = .x$time, y = .x$intensity, xout = all_times$time)
    tibble(
      time = interp_results$x,
      intensity = interp_results$y
    )
  }) |> 
  ungroup()


na_times <- dataLong |> 
  filter(is.na(intensity)) |> 
  select(time) |> 
  distinct() |> 
  arrange(time)

dataLong <- dataLong |> 
  filter(! time %in% na_times$time) |> 
  mutate(time_point = as.character(time_point))

write_csv(dataLong, file = 'data/dataLongRaw.csv')



######---------Pipe One--------#######
#Baseline Correlation

dataWide <- dataLong |> 
  mutate(time_point = as.character(time_point)) |> 
  pivot_wider(
    names_from = time,
    values_from = intensity
  ) |> 
  filter(instrument == "DAD")

metadata <- dataWide |> 
  select(!where(is.numeric)) 

dataCor <- dataWide |> 
  select(where(is.numeric)) |> 
  baseline.corr()

dfCorWide <- bind_cols(metadata, dataCor) |> 
  mutate(time_point = factor(time_point, levels = c("0", "2", "5", "15", "30")))

dfCorLong <- dfCorWide |> 
  pivot_longer(
    cols = where(is.numeric),
    names_to = 'time',
    values_to = 'intensity'
  ) 


#Plot comparison
dfCorLong |> 
  ggplot(aes(x = time, y = intensity, color = time_point))+
  geom_point(size = .1, alpha = .1)

dataLong |> 
  filter(instrument == "DAD") |> 
  ggplot(aes(x = time, y = intensity, color = time_point))+
  geom_point(size = .1, alpha = .1)

write_csv(dfCorLong, file = 'data/dfCorLong.csv')
write_csv(dfCorWide, file = 'data/dfCorWide.csv')


######---------Pipe Two---------########
#normalize
library(tidymodels)

norm_rec <- recipe(time_point + sample + species ~ ., data = dataWide) |> 
  step_normalize(all_numeric_predictors()) |> 
  step_pca(all_numeric_predictors(), num_comp = 5) |> 
  prep()

pcadf <- bake(norm_rec, new_data = NULL)

ggplot(pcadf, aes(x = PC1, y = PC2))+
  geom_point()

library(GGally)
pcadf |> 
  ggpairs(aes(color = species))

tidy(nci_rec)


###########-----------Pipe Three--------------############
#Smoothing
metadata <- dataWide |> select(enzyme, instrument, species, time_point, sample)
spectra  <- dataWide |> select(-c(enzyme, instrument, species, time_point, sample)) |> as.matrix()


t_2 <- savitzkyGolay(spectra, p = 2, w = 151, m = 0)


smoothed_df <- bind_cols(metadata, as.data.frame(t_2))

# 1. Prepare Original Long Data (subsetted to match metadata)
original_long <- bind_cols(metadata, as.data.frame(spectra)) |> 
  pivot_longer(cols = -c(1:5), names_to = "time", values_to = "intensity") |> 
  mutate(time = as.numeric(time), type = "Original")

# 2. Prepare Smoothed Long Data
smoothed_long <- smoothed_df |> 
  pivot_longer(cols = -c(1:5), names_to = "time", values_to = "intensity") |> 
  mutate(time = as.numeric(time), type = "Smoothed")

# 3. Combine and Filter for one sample
plot_data <- bind_rows(original_long, smoothed_long) |> 
  filter(time_point == "0", species == 'early', sample == 'M4')

# 4. Plot
ggplot(plot_data, aes(x = time, y = intensity, color = type)) +
  geom_line(linewidth = 1) +
  scale_color_manual(values = c("Original" = "grey70", "Smoothed" = "firebrick")) +
  theme_minimal() +
  labs(title = "Smoothing Comparison: Sample [ID]",
       color = "Processing State")

