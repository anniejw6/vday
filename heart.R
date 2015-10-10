library(animation)
library(SDMTools)


dir.create("examples")
setwd("examples")

dat<- data.frame(t=seq(0, 2*pi, by=0.05) )
  xhrt <- function(t) 16*sin(t)^3
  yhrt <- function(t) 13*cos(t)-5*cos(2*t)-2*cos(3*t)-cos(4*t)
  
  dat$x=xhrt(dat$t)
  dat$y=yhrt(dat$t)
  
  new.point <- data.frame(x = runif(20000, -18, 18), 
                          y = runif(20000, -18, 18))
  
  
  new.point$res <- pnt.in.poly(new.point, dat[,-1])[,3]
  new <- new.point[new.point$res == 1, -3]
  
  new2 <- rbind(dat[,2:3], new)
  
  
  x <- c(3, 5, 10, 15, 20, 30, 40,50, 60, 75, 90, 100, 125, 140, 150, 175, 200, 220, 250, 300, 400, 500, 700, 900, 1000, 2000, 3000, 4500, rep(nrow(new), 6))

png(file="example%02d.png", width=200, height=200)
  for (i in 1:length(x)) {
    q1 <- ggplot(data.frame(new, random = rnorm(nrow(new)))[sample(1:nrow(new), x[i]),]
                 , aes(x, y, colour = random)) + 
      scale_colour_gradient(low = "white", high = "red", space = "Lab") +
      geom_point(alpha = 0.5, size = 1) + geom_jitter() + theme_bw()+
      #    ggtitle(titles.real[i]) + xlim(c(-16, 16)) + ylim(-14,14) + 
      theme(axis.line=element_blank(),axis.text.x=element_blank(),
            axis.text.y=element_blank(),axis.ticks=element_blank(),
            axis.title.x=element_blank(),
            axis.title.y=element_blank(),legend.position="none",
            panel.background=element_rect(fill = "#FFFFFF"),
            plot.background = element_rect(fill = "#FFFFFF"),
            panel.border=element_blank(),panel.grid.major=element_blank(),
            panel.grid.minor=element_blank(),plot.background=element_blank())
    print(q1)
}
dev.off()
system("convert -delay 11 *.png heart.gif")
dev.new()

