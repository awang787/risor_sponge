library(bipartite)
library(tidyverse)

Risor <- read.csv("data/Risor_Master.csv", fileEncoding="UTF-8-BOM") 
Risor <- Risor %>% rename(Depth_ft = Depth..ft., SL_mm = SL..mm., TL_mm = TL..mm.) #rename cols

Sponge_Fish <- Risor %>% select(FishLineage, SpongeGenus) %>%
  filter(FishLineage != "na") %>% filter(SpongeGenus != "tunicate")

SGenus_FSpecies <- data.frame(table(Sponge_Fish$SpongeGenus, Sponge_Fish$FishLineage))

# spread data frame back into table format
SG_FS <- spread(data = SGenus_FSpecies, Var2, Freq)
rownames(SG_FS) = SG_FS$Var1

# making a pca 
test_pca <- rda(SG_FS[-1], scale = T)

FS_SG <- spread(data = SGenus_FSpecies, Var1, Freq)
rownames(FS_SG) = FS_SG$Var2

pca_sum <- summary(test_pca)
sgfs_scores <- as.data.frame(pca_sum$species) %>% mutate(FS_SG[1]) 
colnames(sgfs_scores)[6] <- "Lineages"
sgfs_loadings <- as.data.frame(pca_sum$sites) %>%
  mutate(trait = rownames(.))

spe_colors <- c(Lineage1 = "#F8766D", Lineage2 = "#C49A00",
                Lineage3 = "#00C094", Lineage4 = "#08306B",
                Lineage5 = "#00B6EB", Lineage6 = "#A58AFF",
                Lineage7 = "#FB61D7")

pca.ggplot <- ggplot(data = sgfs_scores, aes(x = PC1, y = PC2)) +
  geom_point(aes(color = Lineages), size = 3) +
  scale_color_manual(values=spe_colors) + 
  scale_fill_manual(values =spe_colors) +
  geom_segment(data = sgfs_loadings, aes(x=0, xend = PC1/2.75, y= 0, yend = PC2/2.75), lwd=0.1)+
  geom_text(data = sgfs_loadings, aes(x=PC1/3, y=PC2/3, label = trait, fontface = "italic"), size = 5.5) +
  theme_bw() + # theme_classic() + 
  theme(text = element_text(size = 16)) +
  theme(legend.position = "top") +
  labs(color = expression(~italic("Risor")~ "lineages")) +
  xlab("PC1 (38.67%)") + 
  ylab("PC2 (27.44%)") 

pca.ggplot
#ggsave("figures/pca_spongefish.pdf", width = 10, height = 10)

##### Grouping sponge genus into morphology #####
m <- Risor %>% select(FishLineage, SpongeID, SpongeGenus) %>%
  filter(FishLineage != "na") %>% filter(SpongeGenus != "tunicate")
morphology <- m %>% 
  mutate(morph = case_when(SpongeGenus =="Ircinia" ~ "Massive",
                           SpongeGenus =="Aplysina" | SpongeGenus =="Callyspongia" ~ "Columnar",
                           SpongeGenus =="Xestospongia" | SpongeGenus =="Verongula" ~ "Barrel",
                           SpongeGenus =="Aiolochroia" ~ "Digitate"))
morphology$morph[morphology$SpongeID=="Verongula_rigida"] = "Digitate"


df <- data.frame(table(morphology$morph, morphology$FishLineage))
df2 <- spread(data = df, Var2, Freq)
rownames(df2) = df2$Var1
morph_pca <- rda(df2[-1], scale = T)
morph_sum <- summary(morph_pca)
morph_scores <- as.data.frame(morph_sum$species) %>% mutate(Lineage = rownames(.))
morph_loadings <- as.data.frame(morph_sum$sites) %>%
  mutate(trait = rownames(.))

pca_morph <- ggplot(data = morph_scores, aes(x = PC1, y = PC2)) +
  geom_point(aes(color = Lineage), size = 3) +
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



