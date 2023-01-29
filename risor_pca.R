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
ggsave("figures/pca_notuni.pdf", width = 8, height = 6)

# bipartite graph creation
Risor <- read.csv("data/Risor_Master_EditVer.csv", fileEncoding="UTF-8-BOM") # read file in without adding random "." in front of column 1
Risor <- Risor %>% rename(Depth_ft = Depth..ft., SL_mm = SL..mm., TL_mm = TL..mm.) #rename cols
Risor_df <- data.frame(table(Risor$FishSpecies, Risor$SpongeGenus)) #make the columns into a table and dataframe
Risor_df2 <- spread(data = Risor_df, Var1, Freq) # put data into a table format
Risor_df3 <- Risor_df2[,-c(9)]

rownames(Risor_df3) = Risor_df3$Var2
Risor_df3 <- Risor_df3[-1]

Risor_mat <- as.matrix(Risor_df3)
spe_color <- c("plum", "orange", "yellow", "green", "red", "pink", "turquoise")
plotweb(Risor_mat, method = "cca", labsize = 1.2, arrow = "down", col.interaction = c(rep(spe_color[1],7),rep(spe_color[2],7),rep(spe_color[3],7),rep(spe_color[4],7),rep(spe_color[5],7),rep(spe_color[6],7),rep(spe_color[7],7)), 
        high.lab.dis = 0.07, low.lab.dis = 0.07)

nested(Risor_mat) #calculated a nestedness metric
nested(Risor_mat[-1,]) # removing Agelas because all values are 0
nested(Risor_mat)
clustering_tm(Risor_mat)
NOS(Risor_mat)
NOS(Risor_mat[-1,])
computeModules(Risor_mat, method = "Beckett")
computeModules(Risor_mat[-1,], method = "Beckett")
#metaComputeModules(Risor_mat)


table(Risor$FishSpecies)
