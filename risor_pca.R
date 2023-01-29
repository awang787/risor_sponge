library(bipartite)
library(tidyverse)
Risor <- read.csv("data/Risor_Master.csv", fileEncoding="UTF-8-BOM") # read file in without adding random "." in front of column 1
Risor <- Risor %>% rename(Depth_ft = Depth..ft., SL_mm = SL..mm., TL_mm = TL..mm.) #rename cols

Sponge_Fish <- Risor %>% select(FishSpecies, SpongeGenus) %>%
  filter(FishSpecies != "xx") %>% filter(SpongeGenus != "tunicate")

SGenus_FSpecies <- data.frame(table(Sponge_Fish$SpongeGenus, Sponge_Fish$FishSpecies))

# spread data frame back into table format? 
SG_FS <- spread(data = SGenus_FSpecies, Var2, Freq)
rownames(SG_FS) = SG_FS$Var1

# making a pca 
test_pca <- rda(SG_FS[-1], scale = T)

FS_SG <- spread(data = SGenus_FSpecies, Var1, Freq)
rownames(FS_SG) = FS_SG$Var2

pca_sum <- summary(test_pca)
sgfs_scores <- as.data.frame(pca_sum$species) %>% mutate(FS_SG[1]) 
colnames(sgfs_scores)[6] <- "Species"
sgfs_loadings <- as.data.frame(pca_sum$sites) %>%
  mutate(trait = rownames(.))

spe_colors <- c(Species1 = "#F8766D", Species2 = "#C49A00",
                Species3 = "#00C094", Species4 = "#08306B",
                Species5 = "#00B6EB", Species6 = "#A58AFF",
                Species7 = "#FB61D7")

pca.ggplot <- ggplot(data = sgfs_scores, aes(x = PC1, y = PC2)) +
  geom_point(aes(color = Species), size = 3) +
  scale_color_manual(values=spe_colors) + 
  scale_fill_manual(values =spe_colors) +
  geom_segment(data = sgfs_loadings, aes(x=0, xend = PC1/2.75, y= 0, yend = PC2/2.75), lwd=0.1)+
  geom_text(data = sgfs_loadings, aes(x=PC1/3, y=PC2/3, label = trait, fontface = "italic"), size = 4.5) +
  theme_bw() + # theme_classic() + 
  theme(text = element_text(size = 16)) +
  theme(legend.position = "top") +
  labs(color = expression(~italic("Risor")~ "Species")) +
  xlab("PC1 (38.67%)") + 
  ylab("PC2 (27.44%)") 

pca.ggplot
ggsave("figures/pca_risor.pdf", width = 8, height = 6)
