library(tidyverse)
library(bipartite)

Risor <- read.csv("data/Risor_Master.csv", fileEncoding="UTF-8-BOM") # read file in without adding random "." in front of column 1
Risor <- Risor %>% rename(Depth_ft = Depth..ft., SL_mm = SL..mm., TL_mm = TL..mm., Tissue.Number = Luke.Fish.ID) #rename cols
sponge <- Risor %>% select(Tissue.Number, SpongeGenus)
scale_morphs <- read.csv("data/Risor.Scale.and.Morphs.csv", fileEncoding="UTF-8-BOM")
scale_morphs <- scale_morphs %>% rename(MaxScalesInRow = X..Scales.in.longest.row, ScaleExtent = Scale.Extent, NumRows = Rows.of.Scales, HeadLength = Head.Length, EyeLength = Eye.Length, JawLength = Jaw.Length, FirstDorsalDepth = Depth.1st.dorsal, SecondDorsalDepth = Depth.2nd.dorsal, LeastCaudalPeduncleDepth = Minimum.Caudal.Peduncle.depth, PostOrbit = Post.Orbit)
combo <- scale_morphs %>% inner_join(sponge, by = "Tissue.Number")
#write.csv(combo, "data/ScaleSpongeCombined.csv", row.names=F)

combo2 <- combo %>% select(-Tissue.Number, -Species, -SpongeGenus) 

combo3 <- combo[13] %>% mutate(as.data.frame(scale(combo2)))
pca_combo <- rda(combo3[-1], scale = T)
pca_sum_combo <- summary(pca_combo)

scores <- as.data.frame(pca_sum_combo$sites) %>% mutate(combo3[1])
loadings <- as.data.frame(pca_sum_combo$species) %>% mutate(trait = rownames(.))

genus_hulls <- scores %>%
  group_by(SpongeGenus) %>%
  slice(chull(PC1, PC2))

viridis::plasma(4)
sponge_colors <- c(Aplysina="#0D0887FF", Ircinia="#9C179EFF",
                   Verongula="#ED7953FF", Xestospongia="#F0F921FF")

pca.ggplot.sponge <- ggplot(data = scores, aes(x = PC1, y = PC2)) +
  geom_point(aes(color = SpongeGenus), size = 3) +
  geom_polygon(data = genus_hulls, aes(x = PC1, y = PC2, fill = SpongeGenus), alpha = 0.2, show.legend = F) +
  scale_color_manual(values=sponge_colors) + 
  scale_fill_manual(values=sponge_colors) +
  geom_segment(data = loadings, aes(x=0, xend = PC1, y= 0, yend = PC2), lwd=0.1)+
  geom_text(data = loadings, aes(x=PC1*1.1, y=PC2*1.1, label = trait), size = 3.8) +
  theme_bw() + 
  theme_classic() + 
  theme(text = element_text(size = 14)) + 
  theme(legend.text = element_text(face='italic')) +
  theme(legend.position = "top") +
  labs(color = expression("Sponge Genera")) +
  xlab("PC1 (30.63%)") + 
  ylab("PC2 (21.17%)") 

pca.ggplot.sponge
#ggsave("figures/pca_sponge.pdf", width = 10, height = 10)
