library("igraph")
library("viridis")
library("ggplot2")
library("gridExtra")

setwd("/home/j/BIOINFORMATICA/REDES/")
# image1
er <- sample_gnm(n=50, m=150) 
# vector
er_vector <- as_data_frame(er) 
# plot
set.seed(1)
plot.igraph(er,
            edge.width=1.5,
            main = "ER",
            vertex.label.color="black",
            vertex.label.dist=0,
            vertex.label.cex=0.1,
            vertex.label.family="Times new roman",
            vertex.frame.color="white",
            vertex.color =  "black",
            edge.color = "black",
            edge.arrow.size=0,
            vertex.size = 11,
            layout=layout.fruchterman.reingold
)

rrg <- sample_k_regular(50, 4, directed = FALSE, multiple = FALSE)
plot.igraph(rrg, 
            edge.width=1.5,
            main = "RR",
            vertex.label.color="black",
            vertex.label.dist=0,
            vertex.label.cex=0.1,
            vertex.label.family="Times new roman",
            vertex.frame.color="white",
            vertex.color =  "black",
            edge.color = "black",
            edge.arrow.size=0,
            vertex.size = 11,
            layout=layout.fruchterman.reingold,
)


# image 2 scatter plot with no smooth

# Pearson correlation 
df <- mtcars

# ranked pearson correlation 
spearman <- cor.test(x = df$mpg,
      y = df$wt,
      method = "spearman")

### Sepal.Length, Sepal.Width
scatter_plot <- ggplot(data = df, aes(mpg, wt)) +
  geom_point(size = 1) +
  theme_classic() +
  theme(axis.text=element_text(size=20),
        axis.title=element_text(size=20),
        text=element_text(family="Times New Roman", size=20))

ggsave( scatter_plot, filename = "image2.svg" , units = "cm", width = 15*1.3, height = 15, dpi = 320, device = "svg")
library(dplyr)
df %>% select(wt, mpg)
rank <- df %>% mutate(wt = rank(wt), mpg = rank(mpg)) %>% select(wt, mpg)


# image 3 scatter plot with smooth
scatter_smooth_pearson_rank <- ggplot(data = df, aes(rank(wt), rank(mpg))) +
  geom_point(size = 1) +
  geom_smooth(method=lm, se=FALSE) +
  theme_classic() +
  theme(axis.text=element_text(size=20),
        axis.title=element_text(size=20),
        text=element_text(family="Times New Roman", size=20))


ggsave( scatter_smooth_pearson_rank, filename = "image3.svg" , units = "cm", width = 15*1.3, height = 15, dpi = 320, device = "svg")

# image pearson
p_negative <- read.table("pearson-negative.csv", header = T, sep = ",")
p_positive <- read.table("pearson-positive.csv", header = T, sep = ",")
p_zero <- read.table("pearson-zero.csv", header = T, sep = ",")

negative <- ggplot(data = p_negative, aes(x = x, y = y)) +
  geom_line() +
  geom_point() +
  ggtitle("r = -1") +
  scale_y_continuous(breaks = c(0,1)) +
  scale_x_continuous(breaks = c(0,1)) +
  xlab("x") +
  ylab("y") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=12),
        text=element_text(family="Times New Roman", size=12))


positive <- ggplot(data = p_positive, aes(x = x, y = y)) +
  geom_line() +
  geom_point()+
  ggtitle("r = 1") +
  scale_y_continuous(breaks = c(0,1)) +
  scale_x_continuous(breaks = c(0,1)) +
  xlab("x") +
  ylab("y") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=12),
        text=element_text(family="Times New Roman", size=12))

zero <- ggplot() +
  geom_line(data = p_zero, aes(x = x, y = y)) +
  geom_point(aes(x = runif(100, 0, 1), y = runif(100, 0 ,1))) +
  ggtitle("r = 0") +
  scale_y_continuous(breaks = c(0,1)) +
  scale_x_continuous(breaks = c(0,1)) +
  xlab("x") +
  ylab("y") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=12),
        text=element_text(family="Times New Roman", size=12))



pearson <- grid.arrange(positive, zero , negative, ncol=3)

ggsave( pearson, filename = "test_pearson.svg" , units = "mm", width = 714, height = 237, device = "svg")

