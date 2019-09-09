library(readxl)
# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#

#===================================

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

#===================================
LMS <- read_excel("C:/Users/Dana_2/Dropbox/CWC/Envira2019/POster/Spectra_R/CWC_gamma_LMS.xlsx")
#a new data frame with counts per day - transfering only those columns used later
LMS.cps <- LMS#[-c(1:5), ] #removing the first 5 rows (if some old spectra have high values in the first channels)

LMS.cps$bkg_32268=LMS$bkg_32268/LMS$bkg_32268[1]*3600*24
LMS.cps$spe_32269=LMS$spe_32269/LMS$spe_32269[1]*3600*24
LMS.cps$bg140653=LMS$bg140653/LMS$bg140653[1]*3600*24
LMS.cps$sp140865=LMS$sp140865/LMS$sp140865[1]*3600*24

EcalD3 <- 1/0.2497
EcalD14bkg <- 1/0.4649
#normalizing the bkg spectra: couns per day per keV
LMS.cps$bkg_32268norm=LMS$bkg_32268/EcalD3
LMS.cps$bg140653norm=LMS$bg140653/EcalD14bkg

summary(LMS.cps)

#LMS.cps <- Det5[-c(1:5), ]
plot(LMS.cps$E, LMS.cps$bkg_32268, type="l", xlim=c(40,600), ylim=c(0, 100), xlab="E [keV]", ylab="cps"  )
lines(LMS.cps$E, LMS.cps$spe_32269, type="l", col="blue")
lines(LMS.cps$E2, LMS.cps$bg140653, type="l", col="red")
lines(LMS.cps$E3, LMS.cps$sp140865, type="l", col="orange")


library(ggplot2)

ggplot(LMS.cps, aes(x=E, y=bkg_32268)) + geom_line() + xlim(20,2000) + ylim(0, 100) 


#comparison LMS to HADES bkg
p<-ggplot(LMS.cps) + 
  geom_line(aes(x = E, y = bkg_32268norm),  color = "#99CCFF")+
  #geom_line(aes(x = E, y = spe_32269),  color = "#006633")+
  geom_line(aes(x = E2, y = bg140653norm),  color = "#666666")+
  #geom_line(aes(x = E3, y = sp140865),  color = "#660000")+
  xlim(30,2000) + 
  xlab('E [keV]') +
  ylab('Normalized count rate [ counts / day / keV]')
  
p <- p + scale_y_log10(limits = c(0.1, 1000))+
    annotate("text",  x=150, y=400, label="LMS", size=5, colour="#99CCFF")+
    annotate("text",  x=150, y=30, label="HADES", size=5, colour="#666666")

  #coral spectra
  q<-ggplot(LMS.cps) + 
    #geom_line(aes(x = E, y = bkg_32268),  color = "#99CCFF")+
    #geom_line(aes(x = E, y = spe_32269),  color = "#006633")+
    #geom_line(aes(x = E2, y = bg140653),  color = "#666666")+
    geom_line(aes(x = E3, y = sp140865),  color = "#660000")+
    xlim(30,400) + ylim(0, 10.5) +
    xlab('E [keV]') +
    ylab('Count rate [counts / day]')
  #q <- q + scale_y_continuous(trans = 'log10')#+
    #annotate("text",  x=150, y=0.0003, label="LMS", size=5, colour="#99CCFF")#+
    #annotate("text",  x=150, y=0.00004, label="HADES", size=5, colour="#666666")
  
  
 q <- q + 
    annotate("text",  x=46, y=2.5, label="Pb-210", size=4, colour="gold4") +
   annotate("text",  x=63, y=10, label="Th-234", size=4, colour="gold4") +
   annotate("text",  x=93, y=10.2, label="Th-234", size=4, colour="gold4") +
   annotate("text",  x=144, y=2.5, label="U-235", size=4, colour="gold4") +
    annotate("text",  x=185, y=8, label="Ra-226 + U-235", size=4, colour="gold4") +
   annotate("text",  x=242, y=2, label="Pb-214 (Ra-226)", size=4, colour="gold4") +
   annotate("text",  x=295, y=2.5, label="Pb-214 (Ra-226)", size=4, colour="gold4") +
    annotate("text",  x=352, y=3, label="Pb-214 (Ra-226)", size=4, colour="gold4") 
    
  
  multiplot(p, q, cols=2)
  
  p<-ggplot(LMS.cps) + aes(linetype = variable) + geom_line()

p<-ggplot(LMS.cps) + 
  geom_line(aes(x = E, y = bkg_32268, group=1,color = "Bkg Det3"), size=0.5) +
  #geom_line(aes(x = E, y = spe_32269, group=1, color = "Sample Det3"), size=1) +
  #geom_line(aes(x = E2, y = bg140653, group=1, color = "Bkg D14"), size=0.5) +
  #geom_line(aes(x = Enew, y = Sp52619, group=1), color = "blue") +
  xlim(30,600) + ylim(0.000, 0.01) +
  xlab('E [keV]') +
  ylab('cps')+
  scale_color_manual('', values=c(Det.3='gold4', Det.5="firebrick4", Det.6='darkslategrey'),
                     breaks=c("Det.3","Det.5","Det.6"), labels=c("Det.3: over 105 days","Det.5: over 160 days","Det.6: over 172 days"))


p
+ 
  annotate("text",  x=560, y=0.00025, label="Cd-114m", size=2)+
  annotate("text",  x=570, y=0.0002,  label="Ac-228", size=2) +
  annotate("text",  x=585, y=0.0002, label="Tl-208", size=2) +
  annotate("text",  x=600, y=0.0002, label="Ge-74*", size=2) +
  annotate("text",  x=620, y=0.0003,  label="Bi-214", size=2) +
  annotate("text",  x=662, y=0.00017, label="Cs-137", size=3) +
  annotate("text",  x=670, y=0.0002,  label="Cu-63*", size=2) +
  annotate("text",  x=695, y=0.00017, label="Ge-72*", size=2) +
  annotate("text",  x=689, y=0.00041, label="Cs-137 peak:", size=4) +
  annotate("text",  x=700, y=0.00038, label="D3: 0.106(14)E-3 cps", size=4, colour="gold4") +
  annotate("text",  x=700, y=0.00036, label="D5: 0.256(13)E-3 cps", size=4, colour="firebrick4") +
  annotate("text",  x=700, y=0.00034, label="D6: 0.138(11)E-3 cps", size=4, colour="darkslategrey")

###############################################################################
#comparison of detector 5 bacgrounds before and after
p<-ggplot(LMS.cps) + 
  #  geom_line(aes(x = E3, y = sum3/2, group=1,color = "Det.3"), size=0.5) +
  geom_line(aes(x = E, y = sum/2, group=1, color = "Det.5"), size=1) +
  geom_line(aes(x = Enew, y = Sp52619, group=1, color = "after"), size=0.5) +
  #  geom_line(aes(x = Enew, y = Sp52263, group=1, color = "before"), size=0.5) +
  
  
  xlim(600,700) + ylim(0, 0.00055) +
  xlab('E [keV]') +
  ylab('cps')+
  scale_color_manual('', values=c(Det.5='firebrick4', after="blue", before='darkslategrey'),
                     breaks=c("Det.5","after","before"), labels=c("Bkg sum over 105 days (before)","Bkg over 1 day (after)","Bkg over 1 day (before)"))


p + 
  
  annotate("text",  x=610, y=0.00030,  label="Bi-214", size=2) +
  annotate("text",  x=667, y=0.00045, label="Cs-137", size=3) +
  annotate("text",  x=670, y=0.00022,  label="Cu-63*", size=2) +
  annotate("text",  x=695, y=0.00022, label="Ge-72*", size=2) +
  annotate("text",  x=611, y=0.00051, label="Cs-137 peak:", size=4) +
  annotate("text",  x=620, y=0.00048, label="before: 0.256(13)E-3 cps", size=4, colour="firebrick4") +
  annotate("text",  x=620, y=0.00046, label="after:      2.36(10)E-3 cps", size=4, colour="blue")
