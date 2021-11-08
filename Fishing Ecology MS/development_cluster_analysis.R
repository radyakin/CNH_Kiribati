# Island variable cluster analysis

# Load packages
library(tidyverse)
library(corrplot)
library(factoextra) 
library(ggpubr)
library(ggdendro)

# Load data
df_raw <- read.csv("Fishing Ecology MS/island_data.csv")
df_raw$Development <- factor(df_raw$Development, levels = c("Low", "Medium", "High"))

df <- df_raw %>%
  select(Island, Development, Pop_2020, Pop_density_2020_km2, HH_AnyMemberFish_2020,
         MaxN_mean_motorboat_effort, Derelict_fishing_gear_km2_mean, Plastic_pollution_km2_mean) 

df_cor <- df %>%
  select(-Island) %>%
  mutate(Development = as.numeric(Development)) %>%
  rename("Development" = Development, "Population" = Pop_2020, "Population \n density" = Pop_density_2020_km2, 
        "N Fishing HH" =  HH_AnyMemberFish_2020, "Max motorboat \n effort" = MaxN_mean_motorboat_effort, 
        "Derelict \n fishing gear" = Derelict_fishing_gear_km2_mean, 
        "Plastic pollution" = Plastic_pollution_km2_mean) %>%
  cor(use='pairwise') 

pal_blue_yellow_coral_emdash <- c(
  colorRampPalette(c('#004B87','#FAE053'))(50),
  colorRampPalette(c('#FAE053','#FF808B'))(50)
)

corrplot.mixed(df_cor,
               tl.cex=.45,tl.col='gray10',
               lower='number',
               upper='color',
               order = 'AOE',
               lower.col = pal_blue_yellow_coral_emdash,
               upper.col = pal_blue_yellow_coral_emdash,
               number.cex=0.9
)

#__________________________________________________________________________________#
# Cluster analysis using k-means
#__________________________________________________________________________________#
# Normalize the data
df_norm <- df %>%
  # Remove island and development status to see how the other data groups
  select(-Island, -Development)
means <- apply(df_norm,2,mean)
sds <- apply(df_norm,2,sd)
df_norm <- scale(df_norm,center=means,scale=sds)

# View hierarchical clustering 
# Change "method =" to see that alternate methods give the same groups
df_clust <- hclust(dist(df_norm), method="complete")
plot(df_clust, labels = df$Island, hang=-1) 

# Format as dendrogram object
df_clust <- dendro_data(as.dendrogram(df_clust))
df_clust[["labels"]]$lab.name <- df$Island[as.numeric(df_clust[["labels"]]$label)]
df_clust[["labels"]]$lab.dev <- df$Development[as.numeric(df_clust[["labels"]]$label)]

ggplot(df_clust$segments) + 
  geom_segment(aes(x = x, y = y, xend = xend, yend = yend))+
  geom_text(data = df_clust$labels, aes(x, y, label = lab.name, color = lab.dev),
            hjust = 1, angle = 90, size = 3) +
  scale_colour_manual(values=c(pal_blue_yellow_coral_emdash[10],
                               pal_blue_yellow_coral_emdash[50],
                               pal_blue_yellow_coral_emdash[90]),
                      name = "Development \n status") +
  ylim(-3, 15) + 
  theme(legend.position = "right", legend.key=element_blank()) +
  theme_dendro()


# K-means clustering
kmeans(df_norm, 4)
fviz_cluster(kmeans(df_norm, 4), data = df_norm, ggtheme = theme_pubr())
