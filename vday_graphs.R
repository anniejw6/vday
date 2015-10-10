library(ggplot2)
library(scales)
library(gridExtra)
library(Cairo)
setwd("~/Dropbox/vday")
library(RColorBrewer)

#### Graph 1: Self Pity #####
x <- seq(as.Date("2013/1/1"), as.Date("2013/12/31"), "days")
y <- rnorm(length(x), 15, 5)
y[x > "2013/06/01" & x < "2013/07/15"] <- rnorm(length(y[x > "2013/06/01" & x < "2013/07/15"]), 
                                                25, 15)
y.lo <- loess.smooth(1:length(y), y, family = "gaussian", span = .03, 
                     evaluation = length(y))
data <- data.frame(y.lo)
data$x <- x
data$y[x == "2013/3/1"] <- 95
data$y[x == "2013/3/2"] <- 100
#data$y[x == "2013/2/16"] <- 95
data$y[x == "2013/12/29"] <- 70
data$y[x == "2013/12/30"] <- 50

colora = "#E0545F"
self.pity <-  ggplot(data, aes(x,y)) + 
  geom_line(colour = "#530918") + theme_bw() + 
  scale_x_date(breaks = date_breaks("months"),
               labels = date_format("%b"),
               limits = c(as.Date("2013-1-1"), as.Date("2013-12-31")),
               expand=c(0,0)) +
#  ggtitle("Level of Self-Pity") + 
  theme(panel.grid.major=element_blank(),
        axis.ticks=element_blank(),
        axis.line=element_line(colour = "#A5969A"),
        legend.position="none",
        axis.text.y = element_blank(),
        axis.text.x=element_text(hjust = -1.1, colour = "#584F51"),
        axis.title.x = element_blank(),
        panel.border=element_blank(),
        panel.background=element_blank(),
        panel.grid.minor=element_blank()) +
  annotate("text", label = "wedding season",
                     x = as.Date("2013-06-22"), y = 36,
                     size = 4, colour = colora) + 
  annotate("text", label = "v-day",
           x = as.Date("2013-03-15"), y = 95,
           size = 4, colour = colora) + 
  annotate("text", label = "new year's eve",
             x = as.Date("2013-12-01"), y = 60,
             size = 4, colour = colora) +
  ylab("Self-Pity")
print(self.pity)

svg("pity.svg", width = 8.25, height = 4.25)
print(self.pity)
dev.off()

rm(list=ls())
##### Nutrition ####
nutri.norm <- data.frame(food = c(rep("protein", 6), 
                             rep("fruits", 10), 
                             rep("veggies", 5),
                             rep("sweets", 6), 
                             rep("grains", 8)))

nutri.val <- data.frame(food = c("chocolate", "ice cream"))


blues <- brewer.pal(9, "Blues")[2:6]
reds <- brewer.pal(3, "RdPu")[1:2]
titlesize = 16
labelsize = 4
pie1 <- ggplot(nutri.norm, aes(x = factor(1), fill = factor(food))) +
  geom_bar(width = 1) +
  scale_fill_manual(values = blues) +
 coord_polar(theta = "y") + 
  ggtitle("Normal Day") +
  annotate("text", label = unique(as.character(factor(nutri.norm$food))), 
           x = 1.11, y = c(5, 14, 21.25, 26.75, 32.5),
           colour = "black", size = labelsize) +
  theme(panel.grid.major=element_blank(),
        axis.ticks=element_blank(),
        axis.line=element_blank(),
        legend.position="none",
        axis.text=element_blank(),
        axis.title = element_blank(),
        panel.border=element_blank(),
        panel.background=element_blank(),
        panel.grid.minor=element_blank(),
        plot.title=element_text(size=titlesize),
        legend.title=element_blank())
print(pie1)

pie2<- ggplot(nutri.val, aes(x = factor(1), fill = factor(food))) +
  geom_bar(width = 1) + 
  scale_fill_manual(values = reds) +
  coord_polar(theta = "y") + 
  ggtitle("Valentine's Day") +
  annotate("text", label = unique(as.character(factor(nutri.val$food))), 
           x = c(1,1), y = c(0.5,1.5),
           colour = "black", size = labelsize) +
  theme(panel.grid.major=element_blank(),
        axis.ticks=element_blank(),
        axis.line=element_blank(),
        legend.position="none",
        axis.text=element_blank(),
        axis.title = element_blank(),
        panel.border=element_blank(),
        panel.background=element_blank(),
        panel.grid.minor=element_blank(),
        plot.title=element_text(size=titlesize),
        legend.title=element_blank())
print(pie2)


svg("nutrition.svg", width = 8.25, height = 4.25)
grid.arrange(pie1,pie2, ncol=2,nrow=1, main = 
               textGrob("Nutritional Intake\n", 
                        gp = gpar(fontsize =20)))
dev.off()

rm(list=ls())


### Friendship ####
norm <- c(rep("couples", 40), rep("singles", 45))
val <- c(rep("couples", 15), rep("singles", 95))
colors <- c("#d7b5d8","#df65b0")

before <- ggplot(data.frame(norm), aes(factor(norm), fill = factor(norm))) + 
  geom_bar(width=.75) + 
  xlab("February 13") + 
  scale_y_continuous(limits = c(0, 100)) +
  scale_fill_manual(values=colors) +
  theme(panel.grid.major=element_blank(),
        axis.ticks=element_blank(),
        axis.line=element_blank(),
        legend.position="none",
        axis.text.x = element_text(colour = "#584F51", size = 12),
        axis.title.x = element_text(colour = "#584F51", size = 12),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        panel.border=element_blank(),
        panel.background=element_blank(),
        panel.grid.minor=element_blank(),
        legend.title=element_blank())

after <- ggplot(data.frame(val), aes(factor(val), fill = factor(val))) + 
  geom_bar(width=.75) + 
  xlab("February 14") + 
  scale_y_continuous(limits = c(0, 100)) +
  scale_fill_manual(values=colors) +
  theme(panel.grid.major=element_blank(),
        axis.ticks=element_blank(),
        axis.line=element_blank(),
        legend.position="none",
        axis.text.x = element_text(colour = "#584F51", size = 12),
        axis.title.x = element_text(colour = "#584F51", size = 12),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        panel.border=element_blank(),
        panel.background=element_blank(),
        panel.grid.minor=element_blank(),
        legend.title=element_blank())

after2 <- ggplot(data.frame(val), aes(factor(val), fill = factor(val))) + 
  geom_bar(width=.75, fill=colors) + 
  scale_y_continuous(limits = c(0, 100)) + 
  ggtitle("Value of Friendship on February 14") +
  theme(panel.grid.major=element_blank(),
        axis.ticks=element_blank(),
        axis.line=element_line(colour = "white"),
        legend.position="none",
        axis.text.y=element_blank(),
        axis.text.x = element_text(colour = "#584F51", size = 12),
        axis.title = element_blank(),
        panel.border=element_blank(),
        panel.background=element_blank(),
        panel.grid.minor=element_blank(),
        legend.title=element_blank())


g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

mylegend<-g_legend(after)

svg("friendship3.svg", width = 8.25, height = 3.75)

grid.arrange(before,after, ncol=2,nrow=1, 
             main = textGrob("Dependence on Friends", 
                        gp = gpar(fontsize=16)))

grid.arrange(arrangeGrob(before + theme(legend.position="none"),
                         after + theme(legend.position="none"),
                         nrow=1),
             mylegend, ncol = 2, widths = c(8,1), 
             main = textGrob("Dependence on Friends",
                             gp = gpar(fontsize=16)))
dev.off()

rm(list=ls())

#### Friendship Bar ####

df1 <- data.frame(type       = factor(rep(c("Couples", "Singles"), each = 2)),
                  date      = factor(rep(c("February 13", "February 14"), 2), 
                                     levels=c("February 13","February 14")),
                  friend= c(40, 15, 45, 95))

ggplot(df1, aes(x = date, y = friend, fill = type)) + 
  geom_bar(position = "dodge", stat = "identity", width = 0.5) +
  scale_fill_manual(values=colors)


#### Stages of the Night ####
x <- c(0:5)
y <- c(-0.5, 2, 0.5, -2.5, 1, 0)
fit2 <- lm(y~poly(x,4,raw=TRUE))
xx <- seq(0,5, by = 0.01)
yy <- predict(fit2, data.frame(x = xx))

meany <- mean(c(min(yy), max(yy)))

stage <- data.frame(xx, yy)

pointx <- c(0, 0.74, 1.75, 2.78, 4.51, 5)
pointy <- predict(fit2, data.frame(x = pointx))

lab <- c("Dammit, still single.",
               "I should call my ex.",
               "Is there anyone\non okCupid?",
               "No one loves me.",
               "Sob to best friend.",
               "I don't need\nno (wo)man.")
colora = "#d6604d"

q<-
  ggplot(stage, aes(xx, yy)) + 
  geom_line(size = 0.5, colour = "#67001f") + theme_bw() + 
  annotate("text", label =lab[1], x = pointx[1] + 0.1, pointy[1] - 0.2,
           size = 4, colour = colora) + 
  annotate("text", label =lab[2], x = pointx[2], y = pointy[2] + 0.2,
             size = 4, colour = colora) + 
  annotate("text", label =lab[3], x = pointx[3] + 0.2, y = pointy[3] + 1,
             size = 4, colour = colora) + 
  annotate("text", label =lab[4], x = pointx[4] + 0.02, y = pointy[4] - 0.18,
                  size = 4, colour = colora) + 
  annotate("text", label =lab[5], x = pointx[5], y = pointy[5] + 0.2,
             size = 4, colour = colora) +
  annotate("text", label =lab[6], x = pointx[6], y = pointy[6] - 0.3,
                  size = 4, colour = colora) +
  annotate("text", label = "active", x = -0.62, y = meany + 0.25, colour = "#4d4d4d", size = 4)  +
  annotate("text", label = "passive", x = -0.65, y = meany - 0.25, colour = "#4d4d4d", size = 4)  +
  geom_segment(aes(x = -.75, y = meany, 
                   xend = 5.25, yend = meany), colour = "grey") +
  theme(axis.text =element_blank(),
    panel.grid.major=element_blank(),
       axis.ticks=element_blank(),
        axis.line=element_line(colour = "white"),
        legend.position="none",
        axis.title = element_blank(),
        panel.border=element_blank(),
        panel.background=element_blank(),
        panel.grid.minor=element_blank(),
        legend.title=element_blank())

svg("stages.svg", width = 8.25, height = 4.25)
q
dev.off()


### Mixed Feelings ####
x <- seq(0, 2, by = 0.01)
lone = 5*x^2
jeal <- rev(lone)

feel <- data.frame(x = rep(x, 2),
                   type = rep(c("lone", "jeal"), each = length(x)),
                   value = c(lone*0.3, jeal*0.3))
mycols <- c("#F8766D", "#00BFC4")
feel.plot <- ggplot(feel, aes(x = x, y = value, colour=factor(type))) + 
  geom_line() + 
  xlab("How Well You Know the Couple") +
  ylab("Intensity of Emotion") + 
  theme(panel.grid.major=element_blank(),
        axis.ticks=element_blank(),
        axis.line=element_line(colour = "grey"),
        legend.position="none",
        axis.text = element_blank(),
        axis.title = element_text(colour = "#584F51", size = 12),
        axis.text.y = element_blank(),
        panel.border=element_blank(),
        panel.background=element_blank(),
        panel.grid.minor=element_blank(),
        legend.title=element_blank()) +
  annotate("text", label = c("Loneliness", "Jealousy"),
           x = c(1.75, 1.75), y = c(6, 1),
           colour = mycols[2:1])

svg("feel.svg", width = 7, height = 5)
feel.plot
dev.off()
