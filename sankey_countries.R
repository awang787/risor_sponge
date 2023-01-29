# sankey diagrams 
library(networkD3)
library(tidyverse)

Risor <- read.csv("data/Risor_Master.csv", fileEncoding="UTF-8-BOM", stringsAsFactors = FALSE) # read file in without adding random "." in front of column 1
Risor <- Risor %>% rename(Depth_ft = Depth..ft., SL_mm = SL..mm., TL_mm = TL..mm.) #rename cols
a <- Risor %>% filter(FishSpecies != "xx") %>% filter(SpongeGenus != "tunicate") %>% select(FishSpecies, SpongeGenus, Country)

##### ALL RISOR #####
r <- spread(data.frame(table(a$SpongeGenus, a$FishSpecies)), Var2, Freq)
rlong <- r %>% pivot_longer(names_to = "FishSpecies", values_to = "count", -c(Var1))
rlong <- rlong %>% filter(count != 0)
species <- sort(unique(as.character(a$FishSpecies)))
sponges <- unique(as.character(a$SpongeGenus))
nodes <- data.frame(node = c(0:12), name = c(species, sponges))

links <- rlong %>% mutate(target = recode(Var1, Ircinia = 7, Verongula = 8, Callyspongia = 9, Aplysina = 10, Aiolochroia = 11, Xestospongia = 12)) %>%
  mutate(source = recode(FishSpecies, Species1 = 0, Species2 =  1, Species3 = 2, Species4 = 3, Species5 = 4, Species6 = 5, Species7 = 6)) %>%
  mutate(group = as.factor(c("a", "d", "f", "b", "z", "a", "c", "e", "f", "z", "b", "f", "z", "c")))

all_cols <- 'd3.scaleOrdinal() .domain(["Species1", "Species2", "Species3",
"Species4", "Species5", "Species6", "Species7", "Ircinia", "Verongula", 
"Callyspongia", "Aplysina", "Aiolochroia", "Xestospongia", "z", "a", "b", "c",
"d", "e", "f", "g"]) .range(["#F8766D",
"#C49A00", "#00C094","#08306B", "#00B6EB", "#A58AFF", "#FB61D7",
"#636363", "#D9D9D9", "#BDBDBD", "#969696", "#F7F7F7", "#252525",  
"#F8766D",
"#C49A00", "#00C094", "#08306B", "#00B6EB", "#A58AFF", "#FB61D7"])'

sankey_all <- networkD3::sankeyNetwork(Links = links, Nodes = nodes, 
                                       Source = 'source', 
                                       Target = 'target', 
                                       Value = 'count',
                                       NodeID = 'name', 
                                       LinkGroup = "group",
                                       sinksRight=FALSE,
                                       colourScale = all_cols,
                                       fontSize = 0,
                                       font = "Arial")
sankey_all

##### BELIZE #####
bel <- a %>% filter(Country == "Belize")
bel2 <- spread(data.frame(table(bel$SpongeGenus, bel$FishSpecies)), Var2, Freq)
bel_long <- bel2 %>% pivot_longer(names_to = "FishSpecies", values_to = "count", -c(Var1))
bel_long <- bel_long %>% filter(count != 0)
belfish <- sort(unique(as.character(bel$FishSpecies)))
belspo <- unique(as.character(bel$SpongeGenus))
belnode <- data.frame(node = c(0:5), name = c(belfish, belspo))

bel_link <- bel_long %>% mutate(target = recode(Var1, Ircinia = 3, Verongula = 4, Callyspongia = 5)) %>%
  mutate(source = recode(FishSpecies, Species2 = 0, Species3 =  1, Species6 = 2)) %>%
  mutate(group = as.factor(c("b","a","c","b")))

bel_cols <- 'd3.scaleOrdinal() .domain(["Species2", "Species3", "Species6",
"Callyspongia", "Ircinia", "Verongula", "a", "b", "c"])
.range(["#C49A00", "#53B400", "#A58AFF", 
"#BDBDBD", "#636363", "#D9D9D9", 
"#C49A00", "#53B400", "#A58AFF"])'

bel_sankey <- sankeyNetwork(Links = bel_link, Nodes = belnode, 
                                       Source = 'source', 
                                       Target = 'target', 
                                       Value = 'count',
                                       NodeID = 'name', 
                                       LinkGroup = "group",
                                       sinksRight=FALSE,
                                       colourScale = bel_cols,
                                       fontSize = 0,
                                       font = "Arial")
bel_sankey

##### PANAMA #####
pan <- a %>% filter(Country == "Panama")
pan2 <- spread(data.frame(table(pan$SpongeGenus, pan$FishSpecies)), Var2, Freq)
pan_long <- pan2 %>% pivot_longer(names_to = "FishSpecies", values_to = "count", -c(Var1))
pan_long <- pan_long %>% filter(count != 0)
panfish <- sort(unique(as.character(pan$FishSpecies)))
panspo <- unique(as.character(pan$SpongeGenus))
pannode <- data.frame(node = c(0:6), name = c(panfish, panspo))

pan_link <- pan_long %>% mutate(target = recode(Var1, Ircinia = 3, Aplysina = 4, Verongula = 5, Aiolochroia = 6)) %>%
  mutate(source = recode(FishSpecies, Species2 = 0, Species6 =  1, Species7 = 2)) %>%
  mutate(group = as.factor(c("a","c","a","b", "c", "c")))

pan_cols <- 'd3.scaleOrdinal() .domain(["Species2", "Species6", "Species7",
"Ircinia", "Aplysina", "Verongula", "Aiolochroia", "a", "b", "c"])
.range(["#C49A00", "#A58AFF", "#FB61D7", 
"#636363", "#969696", "#D9D9D9", "#F7F7F7",  
"#C49A00", "#A58AFF", "#FB61D7"])'

pan_sankey <- sankeyNetwork(Links = pan_link, Nodes = pannode, 
                            Source = 'source', 
                            Target = 'target', 
                            Value = 'count',
                            NodeID = 'name', 
                            LinkGroup = "group",
                            sinksRight=FALSE,
                            colourScale = pan_cols,
                            fontSize = 0,
                            font = "Arial")
pan_sankey

##### ST EUSTATIUS #####
seus <- a %>% filter(Country == "St Eustatius")
seus2 <- spread(data.frame(table(seus$SpongeGenus, seus$FishSpecies)), Var2, Freq)
seus_long <- seus2 %>% pivot_longer(names_to = "FishSpecies", values_to = "count", -c(Var1))
seus_long <- seus_long %>% filter(count != 0)
seusfish <- sort(unique(as.character(seus$FishSpecies)))
seusspo <- unique(as.character(seus$SpongeGenus))
seusnode <- data.frame(node = c(0:6), name = c(seusfish, seusspo))

seus_link <- seus_long %>% mutate(target = recode(Var1, Ircinia = 4, Xestospongia = 5, Aplysina = 6)) %>%
  mutate(source = recode(FishSpecies, Species1 = 0, Species4 =  1, Species5 = 2, Species6 = 3)) %>%
  mutate(group = as.factor(c("c","b","d","a", "b")))

seus_cols <- 'd3.scaleOrdinal() .domain(["Species1", "Species4", "Species5", "Species6",
"Ircinia", "Xestospongia", "Aplysina", "a", "b", "c", "d"])
.range(["#F8766D", "#08306B", "#00B6EB", "#A58AFF",
"#636363", "#252525", "#969696", 
"#F8766D", "#08306B", "#00B6EB", "#A58AFF"])'

seus_sankey <- sankeyNetwork(Links = seus_link, Nodes = seusnode, 
                             Source = 'source', 
                             Target = 'target', 
                             Value = 'count',
                             NodeID = 'name', 
                             LinkGroup = "group",
                             sinksRight=FALSE,
                             colourScale = seus_cols,
                             fontSize = 0,
                             font = "Arial")

seus_sankey

##### ROATAN #####
roa <- a %>% filter(Country == "Roatan")
roa2 <- spread(data.frame(table(roa$SpongeGenus, roa$FishSpecies)), Var2, Freq)
roa_long <- roa2 %>% pivot_longer(names_to = "FishSpecies", values_to = "count", -c(Var1))
roafish <- unique(as.character(roa$FishSpecies))
roaspo <- unique(as.character(roa$SpongeGenus))
roanode <- data.frame(node = c(0:3), name = c(roafish, roaspo))

roa_link <- roa_long %>% mutate(target = recode(Var1, Xestospongia = 1, Ircinia = 2, Verongula = 3)) %>%
  mutate(source = recode(FishSpecies, Species1 = 0)) %>%
  mutate(group = as.factor(c("a", "a", "a")))

roa_cols <- 'd3.scaleOrdinal() .domain(["Species1", 
"Ircinia", "Xestospongia", "Verongula", "a"])
.range(["#F8766D", 
"#636363", "#252525", "#D9D9D9", 
"#F8766D"])'

roa_sankey <- sankeyNetwork(Links = roa_link, Nodes = roanode, 
                            Source = 'source', 
                            Target = 'target', 
                            Value = 'count',
                            NodeID = 'name', 
                            LinkGroup = "group",
                            sinksRight=FALSE,
                            colourScale = roa_cols,
                            fontSize = 0,
                            font = "Arial")

roa_sankey

##### SAVE TO PDF #####
library(htmlwidgets)
library(webshot)

# sankey_all 
saveWidget(sankey_all, file=paste0(getwd(), "/figures/sankey.html"))
webshot("C:/Users/awang/Documents/risor_sponge/figures/sankey.html", "C:/Users/awang/Documents/risor_sponge/figures/sankey.pdf")

# belize 
saveWidget(bel_sankey, file=paste0(getwd(), "/figures/bel_sankey.html"))
webshot("C:/Users/awang/Documents/risor_sponge/figures/bel_sankey.html", "C:/Users/awang/Documents/risor_sponge/figures/bel_sankey.pdf")

# panama
saveWidget(pan_sankey, file=paste0(getwd(), "/figures/pan_sankey.html"))
webshot("C:/Users/awang/Documents/risor_sponge/figures/pan_sankey.html", "C:/Users/awang/Documents/risor_sponge/figures/pan_sankey.pdf")

# st eustatius
saveWidget(seus_sankey, file=paste0(getwd(), "/figures/seus_sankey.html"))
webshot("C:/Users/awang/Documents/risor_sponge/figures/seus_sankey.html", "C:/Users/awang/Documents/risor_sponge/figures/seus_sankey.pdf")

# roatan 
saveWidget(roa_sankey, file=paste0(getwd(), "/figures/roa_sankey.html"))
webshot("C:/Users/awang/Documents/risor_sponge/figures/roa_sankey.html", "C:/Users/awang/Documents/risor_sponge/figures/roa_sankey.pdf")
