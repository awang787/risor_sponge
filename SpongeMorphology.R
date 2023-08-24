library(tidyverse)
library(bipartite)

Risor <- read.csv("data/Risor_Master.csv", fileEncoding="UTF-8-BOM") 
Risor <- Risor %>% rename(Depth_ft = Depth..ft., SL_mm = SL..mm., TL_mm = TL..mm.) #rename cols

m <- Risor %>% select(Country, FishSpecies, SpongeID, SpongeGenus) %>%
  filter(FishSpecies != "xx") %>% filter(SpongeGenus != "tunicate")
morphology <- m %>% 
  mutate(morph = case_when(SpongeGenus =="Ircinia" ~ "Massive",
                           SpongeGenus =="Aplysina" | SpongeGenus =="Callyspongia" ~ "Columnar",
                           SpongeGenus =="Xestospongia" | SpongeGenus =="Verongula" ~ "Barrel",
                           SpongeGenus =="Aiolochroia" ~ "Digitate"))
morphology$morph[morphology$SpongeID=="Verongula_rigida"] = "Digitate"
morphology <- morphology %>% mutate(FishSpecies=recode(FishSpecies, 
                                                       'Species1' = "Lineage1", 'Species2' =  "Lineage2",
                                                       'Species3' = "Lineage3", 'Species4' = "Lineage4",
                                                       'Species5' = "Lineage5", 'Species6' = "Lineage6",
                                                       'Species7' = "Lineage7")) %>%
  rename(FishLineages = FishSpecies)

df <- data.frame(table(morphology$morph, morphology$FishLineages))
df2 <- spread(data = df, Var2, Freq)
rownames(df2) = df2$Var1
morph_pca <- rda(df2[-1], scale = T)
morph_sum <- summary(morph_pca)
morph_scores <- as.data.frame(morph_sum$species) %>% mutate(Lineages = rownames(.))
morph_loadings <- as.data.frame(morph_sum$sites) %>%
  mutate(trait = rownames(.))

pca_morph <- ggplot(data = morph_scores, aes(x = PC1, y = PC2)) +
  geom_point(aes(color = Lineages), size = 3) +
  scale_color_manual(values=spe_colors) + 
  scale_fill_manual(values =spe_colors) +
  geom_segment(data = morph_loadings, aes(x=0, xend = PC1/3, y= 0, yend = PC2/3), lwd=0.1)+
  geom_text(data = morph_loadings, aes(x=PC1/3, y=PC2/3, label = trait), size = 5.5) +
  theme_bw() +
  theme(text = element_text(size = 16)) +
  theme(legend.position = "top") +
  labs(color = expression(~italic("Risor")~ "Lineages")) +
  xlab("PC1 (43.73%)") + 
  ylab("PC2 (34.90%)") 

pca_morph
#ggsave("figures/pca_sponge_morphology_si.pdf", width=8, height=8)

