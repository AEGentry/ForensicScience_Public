### Age of Evidence: Filtered Contact Cell Features
### Data from Chris Ehrhardt, May 2022
### Scripts by Amanda Elswick Gentry, 2022
library(ggpubr)
library(ggplot2)
library(patchwork)
### Plotting Predictions ###
### Set directories
workDir <- "<redacted>"
pubDir <- "<redacted>"

### Load the test set predictions 
load(paste(pubDir, "<redacted>", sep=""))

### Add columns for the errors
Test$GBM.Err <- Test$Time.Days - Test$GBM.Pred.Days
Test$GLMM.Err <- Test$Time.Days - Test$GLMM.Pred.Days
Test$GBM.Abs.Err <- abs(Test$GBM.Err)
Test$GLMM.Abs.Err <- abs(Test$GLMM.Err)
summary(Test$GBM.Abs.Err)
summary(Test$GLMM.Abs.Err)

### Square, shaded bins
p <- ggplot(Test, aes(Time.Days, GBM.Abs.Err)) + 
              ylim(0,550) +
              xlab("Observed TSD") + ylab("Absolute Error") +
              ggtitle("Absolute Prediction Error, GBM Model") +
              geom_bin2d(binwidth=c(20,20))
p
### Points
p <- ggplot(Test, aes(Time.Days, GBM.Abs.Err)) + 
  ylim(0,525) +
  xlab("Observed TSD") + ylab("Absolute Error") +
  ggtitle("Absolute Prediction Error, GBM Model") +
  geom_point(shape=1,
             position=position_jitter(width=1,height=.5),
             alpha=0.5,
             color="royalblue4")
p

### Points with densities ###
### Calculate some medians ###
round(median(Test$GBM.Abs.Err[Test$Time.Days <= 100]),1)
round(median(Test$GBM.Abs.Err[(Test$Time.Days > 100) & (Test$Time.Days <= 200)]),1)
round(median(Test$GBM.Abs.Err[(Test$Time.Days > 200) & (Test$Time.Days <= 300)]),1)
round(median(Test$GBM.Abs.Err[(Test$Time.Days > 300) & (Test$Time.Days <= 400)]),1)
round(median(Test$GBM.Abs.Err[Test$Time.Days > 400]),1)

round(median(Test$GLMM.Abs.Err[Test$Time.Days <= 100]),1)
round(median(Test$GLMM.Abs.Err[(Test$Time.Days > 100) & (Test$Time.Days <= 200)]),1)
round(median(Test$GLMM.Abs.Err[(Test$Time.Days > 200) & (Test$Time.Days <= 300)]),1)
round(median(Test$GLMM.Abs.Err[(Test$Time.Days > 300) & (Test$Time.Days <= 400)]),1)
round(median(Test$GLMM.Abs.Err[Test$Time.Days > 400]),1)

####### GBM #############
plot1 <- ggplot(Test, aes(x = Time.Days, y = GBM.Abs.Err)) + 
  geom_point(color = "royalblue4", size = 3, shape=1,
             position=position_jitter(width=1,height=.5),
             alpha=0.5) + 
  
  geom_point(aes(x=50,y=19.1),colour="firebrick3", shape=19, size=2.25) + 
  annotate('text', x = 40, y = 325, label = 'TSD 1-100 days \n MedAE 19.1',
           color = 'black', size = 2.5, angle = 25) +
  
  geom_point(aes(x=150,y=42.8),colour="firebrick3", shape=19, size=2.25) + 
  annotate('text', x = 140, y = 350, label = 'TSD 100-200 days \n MedAE 42.8',
           color = 'black', size = 2.5, angle = 25) +
  
  geom_point(aes(x=250,y=208.5),colour="firebrick3", shape=19, size=2.25) + 
  annotate('text', x = 240, y = 375, label = 'TSD 200-300 days \n MedAE 208.5',
           color = 'black', size = 2.5, angle = 25) +
  
  geom_point(aes(x=350,y=129.8),colour="firebrick3", shape=19, size=2.25) + 
  annotate('text', x = 340, y = 400, label = 'TSD 300-400 days \n MedAE 129.8',
           color = 'black', size = 2.5, angle = 25) +
  
  geom_point(aes(x=450,y=215.3),colour="firebrick3", shape=19, size=2.25) + 
  annotate('text', x = 440, y = 425, label = 'TSD 400+ days \n MedAE 215.3',
           color = 'black', size = 2.5, angle = 25) +
  
  annotate("segment", x = 100, xend = 100, y = 0, yend = 325,
           color = "firebrick3", linetype="dotted") +
  annotate("segment", x = 200, xend = 200, y = 0, yend = 350,
           color = "firebrick3", linetype="dotted") +
  annotate("segment", x = 300, xend = 300, y = 0, yend = 375,
           color = "firebrick3", linetype="dotted") +
  annotate("segment", x = 400, xend = 400, y = 0, yend = 400,
           color = "firebrick3", linetype="dotted") +
  
  scale_y_continuous(name = "Absolute Error", 
                     limits = c(-5, 525), expand = c(0, 0)) + 
  scale_x_continuous(name = "Observed TSD", 
                     limits = c(-5, 475), expand = c(0, 0)) + 
  theme_pubr() +
  theme(legend.position = "none") 
# plot1
dens1 <- ggplot(Test, aes(x = Time.Days)) +
  scale_x_continuous(limits = c(-5,525)) +
  geom_density(alpha = 0.4, color = "royalblue4") + 
  theme_void() + 
  theme(legend.position = "none")+
  ggtitle("Absolute Prediction Error, GBM Model")

dens2 <- ggplot(Test, aes(x = GBM.Abs.Err)) + 
  scale_x_continuous(limits = c(-5,450)) +
  geom_density(alpha = 0.4, color = "royalblue4") + 
  theme_void() + 
  theme(legend.position = "none") + 
  coord_flip()

p <- dens1 + plot_spacer() + plot1 + dens2 + 
  plot_layout(ncol = 2, nrow = 2, widths = c(4, 1), heights = c(1, 4)) 
ggsave(filename=paste(pubDir, "<redacted>", sep=""), p)
####### GLMM #############  
plot1 <- ggplot(Test, aes(x = Time.Days, y = GLMM.Abs.Err)) + 
    geom_point(color = "darkgreen", size = 3, shape=1,
               position=position_jitter(width=1,height=.5),
               alpha=0.5) + 
  
  geom_point(aes(x=50,y=10.3),colour="firebrick3", shape=19, size=2.25) + 
  annotate('text', x = 40, y = 350, label = 'TSD 1-100 days \n MedAE 10.3',
           color = 'black', size = 2.5, angle = 25) +
  
  geom_point(aes(x=150,y=25.5),colour="firebrick3", shape=19, size=2.25) + 
  annotate('text', x = 140, y = 375, label = 'TSD 100-200 days \n MedAE 25.5',
           color = 'black', size = 2.5, angle = 25) +
  
  geom_point(aes(x=250,y=129.2),colour="firebrick3", shape=19, size=2.25) + 
  annotate('text', x = 240, y = 400, label = 'TSD 200-300 days \n MedAE 129.2',
           color = 'black', size = 2.5, angle = 25) +
  
  geom_point(aes(x=350,y=132.6),colour="firebrick3", shape=19, size=2.25) + 
  annotate('text', x = 340, y = 425, label = 'TSD 300-400 days \n MedAE 132.6',
           color = 'black', size = 2.5, angle = 25) +
  
  geom_point(aes(x=450,y=229.6),colour="firebrick3", shape=19, size=2.25) + 
  annotate('text', x = 440, y = 450, label = 'TSD 400+ days \n MedAE 229.6',
           color = 'black', size = 2.5, angle = 25) +
  
  annotate("segment", x = 100, xend = 100, y = 0, yend = 350,
           color = "firebrick3", linetype="dotted") +
  annotate("segment", x = 200, xend = 200, y = 0, yend = 375,
           color = "firebrick3", linetype="dotted") +
  annotate("segment", x = 300, xend = 300, y = 0, yend = 400,
           color = "firebrick3", linetype="dotted") +
  annotate("segment", x = 400, xend = 400, y = 0, yend = 425,
           color = "firebrick3", linetype="dotted") +
  
  scale_y_continuous(name = "Absolute Error", 
                       limits = c(-5, 525), expand = c(0, 0)) + 
    scale_x_continuous(name = "Observed TSD", 
                       limits = c(-5, 475), expand = c(0, 0)) + 
    theme_pubr() +
    theme(legend.position = "none") 
# plot1
dens1 <- ggplot(Test, aes(x = Time.Days)) +
    scale_x_continuous(limits = c(-5,525)) +
    geom_density(alpha = 0.4, color = "darkgreen") + 
    theme_void() + 
    theme(legend.position = "none")+
    ggtitle("Absolute Prediction Error, GLMM Model")
  
dens2 <- ggplot(Test, aes(x = GLMM.Abs.Err)) + 
    scale_x_continuous(limits = c(-5,450)) +
    geom_density(alpha = 0.4, color = "darkgreen") + 
    theme_void() + 
    theme(legend.position = "none") + 
    coord_flip()
  
p <- dens1 + plot_spacer() + plot1 + dens2 + 
    plot_layout(ncol = 2, nrow = 2, widths = c(4, 1), heights = c(1, 4)) 

ggsave(filename=paste(pubDir, "<redacted>", sep=""), p)
