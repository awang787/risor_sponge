library(tidyverse)
library(bipartite)

scale_morphs <- read.csv("data/Risor.Scale.and.Morphs.csv", fileEncoding="UTF-8-BOM")
scale_morphs <- scale_morphs %>% rename(MaxScalesInRow = X..Scales.in.longest.row, ScaleExtent = Scale.Extent, NumRows = Rows.of.Scales, HeadLength = Head.Length, EyeLength = Eye.Length, JawLength = Jaw.Length, FirstDorsalDepth = Depth.1st.dorsal, SecondDorsalDepth = Depth.2nd.dorsal, LeastCaudalPeduncleDepth = Minimum.Caudal.Peduncle.depth, PostOrbit = Post.Orbit)

spe_colors <- c(Lineage1 = "#F8766D", Lineage2 = "#C49A00",
                Lineage3 = "#00C094", Lineage4 = "#08306B",
                Lineage5 = "#00B6EB", Lineage6 = "#A58AFF",
                Lineage7 = "#FB61D7")

scales <- scale_morphs %>% select(-Tissue.Number) %>% 
  mutate(Species=recode(Species, 
                        '1' = "Lineage1", '2' =  "Lineage2",
                        '3' = "Lineage3", '4' = "Lineage4",
                        '5' = "Lineage5", '6' = "Lineage6", '7' = "Lineage7"))
scales2 <- scales[1] %>% mutate(as.data.frame(scale(scales[,-1])))

pca_scales <- rda(scales2[-1], scale = T)
pca_sum_scales <- summary(pca_scales)

scales_scores <- as.data.frame(pca_sum_scales$sites) %>% mutate(scales[1])
scales_loadings <- as.data.frame(pca_sum_scales$species) %>% mutate(trait = rownames(.))

scales_lineage_hulls <- scales_scores %>%
  group_by(Species) %>%
  slice(chull(PC1, PC2))

pca.ggplot.scales <- ggplot(data = scales_scores, aes(x = PC1, y = PC2)) +
  geom_point(aes(color = Species), size = 3) +
  geom_polygon(data = scales_lineage_hulls, aes(x = PC1, y = PC2, fill = Species), alpha = 0.2, show.legend = F) +
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
