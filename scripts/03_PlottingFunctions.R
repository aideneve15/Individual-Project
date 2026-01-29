library(tidyverse)
library(scales)

##########--------------SCREE--------------##########
data = screeData_0
make_scree_plot <-
  ggplot(data, aes(x = PC, y = var_explained, group = 1)) +
    geom_line() +
    geom_point(size = 1) +
    geom_text(
      data = data %>% slice(1:4),
      aes(label = paste0(round(cumulative * 100), "%")),
      vjust = -0.6,
      hjust = -0.3,
      size = 3
    ) +
    scale_y_continuous(labels = scales::percent) +
    scale_x_discrete(labels = as.character(seq_len(nrow(data)))) +
    labs(
      title = "Scree Plot of Principal Components",
      x = "Principal Component",
      y = "Proportion of Variance Explained",
      caption = "Percentages indicate cumulative variance explained"
    )

ggsave('documents/ScreePlot01.png')

##########--------------Loadings--------------##########



##########--------------Score--------------##########
