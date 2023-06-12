# packages
library(bipartite)
library(tidyverse)
library(patchwork)

# loading dataset 
Risor <- read.csv("data/Risor_Master.csv", fileEncoding="UTF-8-BOM") # read file in without adding random "." in front of column 1
Risor <- Risor %>% rename(Depth_ft = Depth..ft., SL_mm = SL..mm., TL_mm = TL..mm.) #rename cols
a <- Risor %>% filter(FishSpecies != "xx") %>% filter(SpongeGenus != "tunicate")

# create binary matrix 
bin.mat <- function(x){
  mat <- spread(data.frame(table(x$SpongeGenus, x$FishSpecies)), Var2, Freq)
  rownames(mat) <- mat$Var1
  mat <- as.matrix(mat[-1])
  mat <- ifelse(mat != 0,1,0)
}

# verify that there is at least one 0 and 1 in the null models
ifallsame <- function(y){
  if (length(unique(y))/ncol(y) == 1){
    if (y[1,1]==1){
      replace(y,sample(1:length(y),1), 0)}
    else (replace(y,sample(1:length(y),1),1))
  }
  return (y)
}

# given a binary matrix, randomize the contents within; 
# treating each interaction between genus and species as independent 
nullmod <- function(y){
  for(i in 1:nrow(y)){ 
    for(j in 1:ncol(y)){ 
      y[i,j] <- sample(0:1,1) # either 0 or 1 bc binary
    }
  }
  ifallsame(y)
  return (y) 
}

# generating many null models and adding them to a list
make.null<- function(x){
  l <- list()
  for (i in 1:1000){ 
    mat <- x
    l[[i]] <- nullmod(mat)
  }
  return (l)
}

# nestedness test
nestedness <- function(x){
  l <- list()
  for (i in 1:length(x)){
    l[[i]]<- unname(nested(x[[i]]))
  }
  return (l)
}

# modularity test using computeModules() 
# this does not work if one species is present everywhere (in all genera)
get.mods <- function(x){
  l <- list()
  for (i in 1:length(x)){
    m <- computeModules(x[[i]])
    l[[i]] <- m@likelihood
  }
  return (l)
} 

# function to make histogram using list of 1000 nestedness null model values 
make.hist <- function(y){
  df <- as.data.frame(unlist(y))
  df <- df %>% rename(nestedness = `unlist(y)` )
  hist <- ggplot(data = df, aes(nestedness)) + 
    geom_histogram(binwidth = 5, color = "black", fill = "steelblue", ) + 
    geom_vline(aes(xintercept=ris),
               color="red", linetype="dashed", size=1) + 
    theme_classic()
  return (hist)
}

make.hist2 <- function(y){
  df <- as.data.frame(unlist(y))
  df <- df %>% rename(nestedness = `unlist(y)` )
  hist <- ggplot(data = df, aes(nestedness)) + 
    geom_histogram(binwidth = 2, color = "black", fill = "steelblue", ) + 
    geom_vline(aes(xintercept=ris),
               color="red", linetype="dashed", size=1) + 
    theme_classic()
  return (hist)
}

# can't use the same get.hist() as nestedness bc way different in scale
mod.hist <- function(y){
  df <- as.data.frame(unlist(y))
  df <- df %>% rename(modularity = `unlist(y)` )
  hist <- ggplot(data = df, aes(modularity)) + 
    geom_histogram(binwidth = 0.025, color = "black", fill = "steelblue") +
    geom_vline(aes(xintercept=rmod),
               color="red", linetype="dashed", size=1) +
    theme_classic()
}

mod.hist2 <- function(y){
  df <- as.data.frame(unlist(y))
  df <- df %>% rename(modularity = `unlist(y)` )
  hist <- ggplot(data = df, aes(modularity)) + 
    geom_histogram(binwidth = 0.005, color = "black", fill = "steelblue") +
    geom_vline(aes(xintercept=rmod),
               color="red", linetype="dashed", size=1) +
    theme_classic()
}

# function to get another function to calculate percentile 
# in reference to null model values 
get.percentile <- function(y){
  perc <- ecdf(sort(unlist(y)))
  return (perc)
}

# all countries
ris.bin <- bin.mat(a)
ris.null <- make.null(ris.bin)
ris <- nested(ris.bin)
ris.nest <- nestedness(ris.null)
ris.nest.hist <- make.hist(ris.nest) +
  theme(text = element_text(size = 16)) +
  ggtitle("a)") 
ris.nest.hist 

ris.percentile <- get.percentile(ris.nest)
ris.percentile(ris) 

rmod <- computeModules(ris.bin)@likelihood
ris.mod <- get.mods(ris.null)
ris.mod.hist <- mod.hist(ris.mod) + 
  theme(text = element_text(size = 16)) +
  ggtitle("b)")
ris.mod.hist

ris.mod.per <- get.percentile(ris.mod)
ris.mod.per(rmod) 

multi_randnull <- ris.nest.hist + ris.mod.hist
multi_randnull
#ggsave("figures/multi_randnull.pdf", multi_randnull, width=14, height=6)

# risor null model using shuffle.web
bipartite_null <- nullmodel(ris.bin, method = "shuffle.web")
ris.nest.bi <- nestedness(bipartite_null)
ris.hist.bi <- make.hist2(ris.nest.bi) +
  theme(text = element_text(size = 16)) +
  ggtitle("a)")# more skewed than random function
ris.hist.bi
ris.percentile.bi <- get.percentile(ris.nest.bi)
ris.percentile.bi(ris) 
ris.mod.bi <- get.mods(bipartite_null)
ris.mod.hist.bi <- mod.hist2(ris.mod.bi) + 
  theme(text = element_text(size = 16)) +
  ggtitle("b)")
ris.mod.hist.bi

multi_shuffle <- ris.hist.bi + ris.mod.hist.bi
multi_shuffle
#ggsave("figures/multi_shuffle.pdf", multi_shuffle, width=14, height=6)
