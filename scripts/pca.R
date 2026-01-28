library(tidyverse) # for data manipulation and visualization

######----------PCA------------#########
dataLongRaw <- read_csv('data/dataLongRaw.csv')

dataWide <- dataLongRaw |> 
  mutate(time_point = as.character(time_point)) |> 
  pivot_wider(
    names_from = time,
    values_from = intensity
  )

metadata <- dataWide |> 
  select(!where(is.numeric))

dataPCA <- dataWide |> 
  filter(instrument == 'DAD')|>
  select(where(is.numeric)) 

pca_res = prcomp(
  dataPCA,
  center = TRUE,
  scale. = TRUE
)

pca_scores = as.data.frame(pca_res$x)

metadata <- metadata[rownames(pca_scores), , drop = FALSE] #align the metadata rows

pca_df <- bind_cols(metadata, pca_scores) |> 
  mutate(time_point = factor(time_point, levels = c("0", "2", "5", "15", "30")))

loadings <- as.data.frame(pca_res$rotation)

variance <- tibble(
  PC = paste0("PC", seq_along(pca_res$sdev)),
  variance = pca_res$sdev^2 / sum(pca_res$sdev^2)
)

ggplot(pca_df, aes(x = PC1, y = PC2))+
  geom_point(aes(color = time_point))+
  theme_minimal()


########---------PIPE ONE---------########
#baseline correction
dfCorLong <- read_csv('data/dfCorLong.csv')
dfCorWide <- read_csv('data/dfCorWide.csv')

dfCorWide <- dfCorWide |> 
  mutate(time_point = factor(time_point, levels = c("0", "2", "5", "15", "30")))

metadata <- dfCorWide |> 
  select(!where(is.numeric))

dataPCA <- dfCorWide |> 
  select(where(is.numeric)) 

pca_res = prcomp(
  dataPCA,
  center = TRUE,
  scale. = TRUE
)

pca_scores = as.data.frame(pca_res$x)
pca_loadings = as.data.frame(pca_res$rotation)
pca_var = as.data.frame(pca_res$sdev)
pca_var <- pca_var |> 
  mutate(proportion = pca_res$sdev/sum(pca_res$sdev),
        index = row_number())

ggplot(pca_var, aes(x = index, y = proportion))+
  geom_line()

metadata <- metadata[rownames(pca_scores), , drop = FALSE] #align the metadata rows

pca_df <- bind_cols(metadata, pca_scores)

ggplot(pca_df, aes(x = PC1, y = PC2))+
  geom_point(aes(color = time_point))+
  theme_minimal()


variance <- tibble(
  PC = paste0("PC", seq_along(pca_res$sdev)),
  variance = pca_res$sdev^2 / sum(pca_res$sdev^2)
)



loadings <- as_tibble(pca_res$rotation, rownames = "time") |> 
  mutate(time = as.numeric(time))

ggplot(loadings, aes(x = time, y = PC2))+
  geom_line()

