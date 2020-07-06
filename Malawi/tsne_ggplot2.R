rm(list = ls())
graphics.off()

library(ggplot2)

eco <- data.frame(read.csv(file = "./data/eco_tsne_ageIn_2.csv"),
                  read.csv(file = "./data/eco_tsne_ageOut_2.csv"),
                  read.csv(file = "./data/Last600_Eco2.csv"))

colnames(eco)[1:4] <- c("tsne_ageIn_X", 
                        "tsne_ageIn_Y",
                        "tsne_ageOut_X",
                        "tsne_ageOut_Y")

old <- eco$age > 85
eco$color <- c("red", "black")[1 + old]

################################
#### color by age threshold ####
################################

png(filename = "./pictures/ggplots/tsne_ageIn_colorByThreshold.png")
ggplot(data = eco) +
  geom_point(mapping = aes(x = tsne_ageIn_X, 
                           y = tsne_ageIn_Y, 
                           color = Age)) +
  ggtitle("t-SNE of Charcoal, Lake Level,\nSpecies Richness, and Age") +
  xlab("t-SNE X") +
  ylab("t-SNE Y")
dev.off()

png(filename = "./pictures/ggplots/tsne_ageOut_colorByThreshold.png")
ggplot(data = eco) +
  geom_point(mapping = aes(x = tsne_ageOut_X, 
                           y = tsne_ageOut_Y, 
                           color = Age)) +
  ggtitle("t-SNE of Charcoal, Lake Level,\nand Species Richness") +
  xlab("t-SNE X") +
  ylab("t-SNE Y")
dev.off()

#######################
#### color by char ####
#######################

png(filename = "./pictures/ggplots/tsne_ageIn_colorByChar.png")
ggplot(data = eco) +
  geom_point(mapping = aes(x = tsne_ageIn_X, 
                           y = tsne_ageIn_Y, 
                           color = char)) +
  ggtitle("t-SNE of Charcoal, Lake Level,\nSpecies Richness, and Age") +
  scale_color_gradient(low = "blue", high = "tan") +
  xlab("t-SNE X") +
  ylab("t-SNE Y")
dev.off()

png(filename = "./pictures/ggplots/tsne_ageOut_colorByChar.png")
ggplot(data = eco) +
  geom_point(mapping = aes(x = tsne_ageOut_X, 
                           y = tsne_ageOut_Y, 
                           color = char)) +
  ggtitle("t-SNE of Charcoal, Lake Level,\nand Species Richness") +
  scale_color_gradient(low = "blue", high = "tan") +
  xlab("t-SNE X") +
  ylab("t-SNE Y")
dev.off()

#######################
#### color by lake ####
#######################

png(filename = "./pictures/ggplots/tsne_ageIn_colorByLake.png")
ggplot(data = eco) +
  geom_point(mapping = aes(x = tsne_ageIn_X, 
                           y = tsne_ageIn_Y, 
                           color = lake)) +
  ggtitle("t-SNE of Charcoal, Lake Level,\nSpecies Richness, and Age") +
  scale_color_gradient(low = "blue", high = "tan") +
  xlab("t-SNE X") +
  ylab("t-SNE Y")
dev.off()

png(filename = "./pictures/ggplots/tsne_ageOut_colorByLake.png")
ggplot(data = eco) +
  geom_point(mapping = aes(x = tsne_ageOut_X, 
                           y = tsne_ageOut_Y, 
                           color = lake)) +
  ggtitle("t-SNE of Charcoal, Lake Level,\nand Species Richness") +
  scale_color_gradient(low = "blue", high = "tan") +
  xlab("t-SNE X") +
  ylab("t-SNE Y")
dev.off()

#######################
#### color by rich ####
#######################

png(filename = "./pictures/ggplots/tsne_ageIn_colorByRich.png")
ggplot(data = eco) +
  geom_point(mapping = aes(x = tsne_ageIn_X, 
                           y = tsne_ageIn_Y, 
                           color = rich)) +
  ggtitle("t-SNE of Charcoal, Lake Level,\nSpecies Richness, and Age") +
  scale_color_gradient(low = "blue", high = "tan") +
  xlab("t-SNE X") +
  ylab("t-SNE Y")
dev.off()

png(filename = "./pictures/ggplots/tsne_ageOut_colorByRich.png")
ggplot(data = eco) +
  geom_point(mapping = aes(x = tsne_ageOut_X, 
                           y = tsne_ageOut_Y, 
                           color = rich)) +
  ggtitle("t-SNE of Charcoal, Lake Level,\nand Species Richness") +
  scale_color_gradient(low = "blue", high = "tan") +
  xlab("t-SNE X") +
  ylab("t-SNE Y")
dev.off()

######################
#### color by age ####
######################

png(filename = "./pictures/ggplots/tsne_ageIn_colorByAge.png")
ggplot(data = eco) +
  geom_point(mapping = aes(x = tsne_ageIn_X, 
                           y = tsne_ageIn_Y, 
                           color = -age)) +
  ggtitle("t-SNE of Charcoal, Lake Level,\nSpecies Richness, and Age") +
  scale_color_gradient(low = "blue", high = "tan") +
  xlab("t-SNE X") +
  ylab("t-SNE Y")
dev.off()

png(filename = "./pictures/ggplots/tsne_ageOut_colorByAge.png")
ggplot(data = eco) +
  geom_point(mapping = aes(x = tsne_ageOut_X, 
                           y = tsne_ageOut_Y, 
                           color = -age)) +
  ggtitle("t-SNE of Charcoal, Lake Level,\nand Species Richness") +
  scale_color_gradient(low = "blue", high = "tan") +
  xlab("t-SNE X") +
  ylab("t-SNE Y")
dev.off()