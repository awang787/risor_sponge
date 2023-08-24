library(tidyverse)
library(bipartite)

scale_morphs <- read.csv("data/Risor_Morphology.csv", fileEncoding="UTF-8-BOM")

spe_colors <- c(Lineage1 = "#F8766D", Lineage2 = "#C49A00",
                Lineage3 = "#00C094", Lineage4 = "#08306B",
                Lineage5 = "#00B6EB", Lineage6 = "#A58AFF",
                Lineage7 = "#FB61D7")

scales <- scale_morphs %>% select(-FishID) %>% 
  mutate(Lineage=recode(Lineage, 
                        '1' = "Lineage1", '2' =  "Lineage2",
                        '3' = "Lineage3", '4' = "Lineage4",
                        '5' = "Lineage5", '6' = "Lineage6", '7' = "Lineage7"))

##### Producing convex hulls based on Lineage #####
scales_lineage <- scales[1] %>% mutate(as.data.frame(scale(scales[,-c(1,12)])))

pca_scales <- rda(scales_lineage[-1], scale = T)
pca_sum_scales <- summary(pca_scales)

scales_scores <- as.data.frame(pca_sum_scales$sites) %>% mutate(scales[1])
scales_loadings <- as.data.frame(pca_sum_scales$species) %>% mutate(trait = rownames(.))

scales_lineage_hulls <- scales_scores %>%
  group_by(Lineage) %>%
  slice(chull(PC1, PC2))

pca.ggplot.scales <- ggplot(data = scales_scores, aes(x = PC1, y = PC2)) +
  geom_point(aes(color = Lineage), size = 3) +
  geom_polygon(data = scales_lineage_hulls, aes(x = PC1, y = PC2, fill = Lineage), alpha = 0.2, show.legend = F) +
  scale_color_manual(values=spe_colors) + 
  scale_fill_manual(values =spe_colors) +
  geom_segment(data = scales_loadings, aes(x=0, xend = PC1, y= 0, yend = PC2), lwd=0.1)+
  geom_text(data = scales_loadings, aes(x=PC1*1.1, y=PC2*1.1, label = trait), size = 5.5) +
  theme_bw() + 
  theme_classic() + 
  theme(text = element_text(size = 16)) +
  theme(legend.position = "top") +
  labs(color = expression(~italic("Risor")~ "lineages")) +
  xlab("PC1 (28.67%)") + 
  ylab("PC2 (21.64%)") 

pca.ggplot.scales
#ggsave("figures/pca_scales.pdf", width = 8, height = 8)

##### Producing convex hulls based on sponge genus #####
scales_sponge <- scales[12] %>% mutate(scales_lineage[,-1]) %>% filter(SpongeGenus != "na")
pca_sponge <- rda(scales_sponge[-1], scale = T)
pca_sum_sponge <- summary(pca_sponge)

sponge_scores <- as.data.frame(pca_sum_sponge$sites) %>% mutate(scales_sponge[1])
sponge_loadings <- as.data.frame(pca_sum_sponge$species) %>% mutate(trait = rownames(.))

genus_hulls <- sponge_scores %>%
  group_by(SpongeGenus) %>%
  slice(chull(PC1, PC2))

sponge_colors <- c(Aplysina="#30123BFF", Ircinia="#B12A90FF",
                   Verongula="#F0F921FF", Xestospongia="#46F884FF", 
                   Aiolochroia="#ff0000", Callyspongia="steelblue")

pca.ggplot.sponge <- ggplot(data = sponge_scores, aes(x = PC1, y = PC2)) +
  geom_point(aes(color = SpongeGenus), size = 3) +
  geom_polygon(data = genus_hulls, aes(x = PC1, y = PC2, fill = SpongeGenus), alpha = 0.2, show.legend = F) +
  scale_color_manual(values=sponge_colors) + 
  scale_fill_manual(values=sponge_colors) +
  geom_segment(data = sponge_loadings, aes(x=0, xend = PC1, y= 0, yend = PC2), lwd=0.1)+
  geom_text(data = sponge_loadings, aes(x=PC1*1.1, y=PC2*1.1, label = trait), size = 5.5) +
  theme_bw() + 
  theme_classic() + 
  theme(text = element_text(size = 16)) + 
  theme(legend.text = element_text(face='italic')) +
  theme(legend.position = "top") +
  labs(color = expression("Sponge Genera")) +
  xlab("PC1 (28.49%)") + 
  ylab("PC2 (20.93%)") 

pca.ggplot.sponge
#ggsave("figures/pca_sponge.pdf", width = 8, height = 8)
