#set.seed(1234)
#* 1.1. A vs G
source("./z1.rs4680 HWE.R")
madata <- madata %>%     
  mutate( 
    A.Case = (2*AA.Case) + GA.Case,
    G.Case = (2*GG.Case) + GA.Case,
    Total.Case = A.Case + G.Case,
    
    A.Control = (2*AA.Control) + GA.Control,
    G.Control = (2*GG.Control) + GA.Control,
    Total.Control = A.Control + G.Control
  )
#
meta1 <- metabin(A.Case, Total.Case, A.Control, Total.Control,
                data=madata, subset=seq(dim(madata)[1]),
                sm="OR", method="I",comb.random=T,
                studlab=paste(Author, Year))
meta1 <- trimfill(meta1)
##############
funnel<-funnel(meta1,lwd = 2, cex = 2, xlab = "OR",  ylab = "se") 

tiff(file='F_AvsG.tif',width=600,height=600) 
funnel<-funnel(meta1,lwd = 2, cex = 2, xlab = NULL,  ylab = NULL)
dev.off()


##############%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%################################################
################# 
#oldpar <- par(mfrow=c(1, 2))
#funnel<-funnel(meta1,lwd = 2, cex = 2, xlab = NULL,  ylab = NULL)
#funnel<-funnel(meta2,lwd = 2, cex = 2, xlab = NULL,  ylab = NULL)

#tiff(file='com_fun_plot_legend.tif',width=1200,height=500, pointsize = 26)
#oldpar <- par(mfrow=c(1, 2))
#funnel<-funnel(meta1,lwd = 2, cex = 2, xlab = NULL,  ylab = NULL)
#funnel<-funnel(meta2,lwd = 2, cex = 2, xlab = NULL,  ylab = NULL)
##########funnel<-funnel(meta3,lwd = 2, cex = 2, xlab = NULL,  ylab = NULL)
###########legend(.2, .7, c("aaaaaaaaaaaaaaaaaa")) ############????????????????????????????????????????????????????????????????????????????
#dev.off()

