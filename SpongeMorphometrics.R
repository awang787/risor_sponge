library(tidyverse)
library(bipartite)

combo <- read.csv("data/ScaleSpongeCombined_update.csv")

combo2 <- combo %>% select(-Tissue.Number, -Species, -SpongeGenus) 

combo3 <- combo[13] %>% mutate(as.data.frame(scale(combo2)))
pca_combo <- rda(combo3[-1], scale = T)
pca_sum_combo <- summary(pca_combo)

scores <- as.data.frame(pca_sum_combo$sites) %>% mutate(combo3[1])
loadings <- as.data.frame(pca_sum_combo$species) %>% mutate(trait = rownames(.))

genus_hulls <- scores %>%
  group_by(SpongeGenus) %>%
  slice(chull(PC1, PC2))

sponge_colors <- c(Aplysina="#30123BFF", Ircinia="#B12A90FF",
                   Verongula="#F0F921FF", Xestospongia="#46F884FF", 
                   Aiolochroia="#ff0000", Callyspongia="steelblue")

pca.ggplot.sponge <- ggplot(data = scores, aes(x = PC1, y = PC2)) +
  geom_point(aes(color = SpongeGenus), size = 3) +
  geom_polygon(data = genus_hulls, aes(x = PC1, y = PC2, fill = SpongeGenus), alpha = 0.2, show.legend = F) +
  scale_color_manual(values=sponge_colors) + 
  scale_fill_manual(values=sponge_colors) +
  geom_segment(data = loadings, aes(x=0, xend = PC1, y= 0, yend = PC2), lwd=0.1)+
  geom_text(data = loadings, aes(x=PC1*1.1, y=PC2*1.1, label = trait), size = 5.5) +
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
