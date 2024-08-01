
##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#1.1 Homozygote model / Multiplicative model (AA vs GG) 
source("./z1.rs4680 HWE.R")
##########
madata <- madata %>% 
        filter(AA.Case> 0) %>%  
        mutate(                 
                Total.Case = AA.Case + GG.Case,   
                Total.Control = AA.Control + GG.Control     
        )                                              
                                                        
#######
meta1 <- metabin(AA.Case, Total.Case, AA.Control, Total.Control,
                 data=madata, 
                 subset=seq(dim(madata)[1]),
                 sm="OR", 
                 method="I",
                 studlab=paste(Author, Year, sep="_"))

forest(meta1 , 
       fontsize=5,
       fs.heading = 7, 
       spacing=0.5,
       col.by="blue", 
       sortvar=Year,
       label.left="Decreasing Risk", 
       label.right="Increasing Risk",
       comb.fixed=F,
       col.square="darkgray",
       col.diamond="black",
       digits.pval=max(gs("digits.pval")-2, 4),
       digits.pval.Q=max(gs("digits.pval.Q")-2, 4),
       test.overall.random=TRUE,
       smlab="IV Odds Ratio", 
       leftlabs=c("AA vs GG\nStudy", NA, NA, NA, NA))
##
png(file='Overall AA vs GG.png',width=800,height=2000) 
forest.jama<-forest(meta1, 
                    fontsize=10,
                    fs.heading = 12, 
                    spacing=.8, 
                    sortvar=Year,
                    comb.fixed=F,
                    col.by="black",
                    label.left="Decreasing Risk", 
                    label.right="Increasing Risk",
                    digits.pval=max(gs("digits.pval")-2, 4),
                    digits.pval.Q=max(gs("digits.pval.Q")-2, 4),
                    test.overall.random=TRUE,
                    test.effect.subgroup.random=TRUE,
                    smlab="IV Odds Ratio", 
                    leftlabs=c("AA vs GG \nStudy", NA, NA, NA, NA))
dev.off()

############ Subgroup analysis by REM###
region.subgroup<-update.meta(meta1, 
                             byvar=Ethnicity, 
                             comb.random = T)#, 
                            # comb.fixed = T)

forest(region.subgroup, fontsize=12,fs.heading = 19,fs.fixed=16, spacing=1.2,
       digits.pval=max(gs("digits.pval")-2, 4),digits.pval.Q=max(gs("digits.pval.Q")-2, 4),
       label.left="Decreasing Risk", label.right="Increasing Risk",leftlabs=c("AA vs GG\nStudy", NA, NA, NA, NA),
       col.by=2, sortvar=Year,fs.test.effect.subgroup=12,fs.hetstat=12,ff.study = "bold",
       test.overall.random=TRUE,test.effect.subgroup.random=TRUE) #this one is good.
##
png(file='Sub Ethnicity AA vs GG.png',width=1500,height=3000) 
forest(region.subgroup, fontsize=12,fs.heading = 19,fs.fixed=16, spacing=1.2,
       digits.pval=max(gs("digits.pval")-2, 4),digits.pval.Q=max(gs("digits.pval.Q")-2, 4),
       label.left="Decreasing Risk", label.right="Increasing Risk",leftlabs=c("AA vs GG\nStudy", NA, NA, NA, NA),
       col.by=2, sortvar=Year,fs.test.effect.subgroup=12,fs.hetstat=12,ff.study = "bold",
       test.overall.random=TRUE,test.effect.subgroup.random=TRUE)#this one is good.
dev.off()
#############################

Types_of_Cancer.subgroup<-update.meta(meta1, 
                                        byvar=Types_of_Cancer, 
                                        comb.random = T)#, 
                                        #comb.fixed = F)

forest(Types_of_Cancer.subgroup, fontsize=12,fs.heading = 19,fs.fixed=16, spacing=1.2,
       digits.pval=max(gs("digits.pval")-2, 4),digits.pval.Q=max(gs("digits.pval.Q")-2, 4),
       label.left="Decreasing Risk", label.right="Increasing Risk",leftlabs=c("AA vs GG\nStudy", NA, NA, NA, NA),
       col.by=2, sortvar=Year,fs.test.effect.subgroup=12,fs.hetstat=12,ff.study = "bold",
       test.overall.random=TRUE,test.effect.subgroup.random=TRUE)#this one is good.
#
png(file='Sub Types_of_Cancer AA vs GG.png',width=1500,height=3000) 
forest.jama<-forest(Types_of_Cancer.subgroup, fontsize=12,fs.heading = 19,fs.fixed=16, spacing=1.2,
                    digits.pval=max(gs("digits.pval")-2, 4),digits.pval.Q=max(gs("digits.pval.Q")-2, 4),
                    label.left="Decreasing Risk", label.right="Increasing Risk",leftlabs=c("AA vs GG\nStudy", NA, NA, NA, NA),
                    col.by=2, sortvar=Year,fs.test.effect.subgroup=12,fs.hetstat=12,ff.study = "bold",
                    test.overall.random=TRUE,test.effect.subgroup.random=TRUE)#this one is good.
#
dev.off()
############ Subgroup analysis by REM###
Source_of_Controls.subgroup<-update.meta(meta1, 
                                         byvar=Source_of_Controls, 
                                         comb.random = T)#, 
#comb.fixed = F)
#names(Source_of_Controls.subgroup)
#Source_of_Controls.subgroup[133]
forest(Source_of_Controls.subgroup, fontsize=12,fs.heading = 19,fs.fixed=16, spacing=1.2,
       digits.pval=max(gs("digits.pval")-2, 4),digits.pval.Q=max(gs("digits.pval.Q")-2, 4),
       label.left="Decreasing Risk", label.right="Increasing Risk",leftlabs=c("AA vs GG\nStudy", NA, NA, NA, NA),
       col.by=2, sortvar=Year,fs.test.effect.subgroup=12,fs.hetstat=12,ff.study = "bold",
       test.overall.random=TRUE,test.effect.subgroup.random=TRUE) #this one is good.
##
png(file='Sub Source_of_Controls AA vs GG.png',width=1500,height=3000) 
forest(Source_of_Controls.subgroup, fontsize=12,fs.heading = 19,fs.fixed=16, spacing=1.2,
       digits.pval=max(gs("digits.pval")-2, 4),digits.pval.Q=max(gs("digits.pval.Q")-2, 4),
       label.left="Decreasing Risk", label.right="Increasing Risk",leftlabs=c("AA vs GG\nStudy", NA, NA, NA, NA),
       col.by=2, sortvar=Year,fs.test.effect.subgroup=12,fs.hetstat=12,ff.study = "bold",
       test.overall.random=TRUE,test.effect.subgroup.random=TRUE)#this one is good.
dev.off()
#############################
      

##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#1.2 Heterozygote model (GA vs GG)
source("./z1.rs4680 HWE.R")
madata <- madata %>% 
        filter(AA.Case> 0) %>%  
        mutate(
                Total.Case = GA.Case + GG.Case,
                Total.Control = GA.Control + GG.Control
        )
#######
meta1 <- metabin(GA.Case, Total.Case, GA.Control, Total.Control,
                 data=madata, 
                 subset=seq(dim(madata)[1]),
                 sm="OR", 
                 method="I",
                 studlab=paste(Author, Year, sep="_"))

forest(meta1 , 
       fontsize=10,
       fs.heading = 12, 
       spacing=1,
       col.by="blue", 
       sortvar=Year,
       label.left="Decreasing Risk", 
       label.right="Increasing Risk",
       comb.fixed=F,
       col.square="darkgray",
       col.diamond="black",
       digits.pval=max(gs("digits.pval")-2, 4),
       digits.pval.Q=max(gs("digits.pval.Q")-2, 4),
       test.overall.random=TRUE,smlab="IV Odds Ratio", 
       leftlabs=c("AA vs GG\nStudy", NA, NA, NA, NA))
##
png(file='Overall GA vs GG.png',width=800,height=2000) 
forest.jama<-forest(meta1, 
                    fontsize=10,
                    fs.heading = 12, 
                    spacing=.8, 
                    sortvar=Year,
                    comb.fixed=F,
                    col.by="black",
                    label.left="Decreasing Risk", 
                    label.right="Increasing Risk",
                    digits.pval=max(gs("digits.pval")-2, 4),
                    digits.pval.Q=max(gs("digits.pval.Q")-2, 4),
                    test.overall.random=TRUE,
                    test.effect.subgroup.random=TRUE,
                    smlab="IV Odds Ratio", 
                    leftlabs=c("GA vs GG \nStudy", NA, NA, NA, NA))
dev.off()

############ Subgroup analysis by REM###
region.subgroup<-update.meta(meta1, 
                             byvar=Ethnicity, 
                             comb.random = T, 
                             comb.fixed = F)

forest(region.subgroup, fontsize=6,fs.heading = 9,fs.fixed=9, spacing=.6,
       digits.pval=max(gs("digits.pval")-2, 4),digits.pval.Q=max(gs("digits.pval.Q")-2, 4),
       label.left="Decreasing Risk", label.right="Increasing Risk",leftlabs=c("GA vs GG\nStudy", NA, NA, NA, NA),
       col.by=2, sortvar=Year,fs.test.effect.subgroup=6,fs.hetstat=5,ff.study = "bold",
       test.overall.random=TRUE,test.effect.subgroup.random=TRUE)
#
png(file='Sub Ethnicity GA vs GG.png',width=1000,height=2000) 
forest.jama<-forest(region.subgroup, fontsize=6,fs.heading = 9,fs.fixed=9, spacing=.6,
                    digits.pval=max(gs("digits.pval")-2, 4),digits.pval.Q=max(gs("digits.pval.Q")-2, 4),
                    label.left="Decreasing Risk", label.right="Increasing Risk",leftlabs=c("GA vs GG\nStudy", NA, NA, NA, NA),
                    col.by=2, sortvar=Year,fs.test.effect.subgroup=6,fs.hetstat=5,ff.study = "bold",
                    test.overall.random=TRUE,test.effect.subgroup.random=TRUE)
dev.off()
##

#############################
Types_of_Cancer.subgroup<-update.meta(meta1, 
                                      byvar=Types_of_Cancer, 
                                      comb.random = T, 
                                      comb.fixed = F)

forest(Types_of_Cancer.subgroup, fontsize=6,fs.heading = 9,fs.random=9, spacing=.6,
       digits.pval=max(gs("digits.pval")-2, 6),digits.pval.Q=max(gs("digits.pval.Q")-2, 4),
       label.left="Decreasing Risk", label.right="Increasing Risk",leftlabs=c("GA vs GG\nStudy", NA, NA, NA, NA),
       col.by=1, sortvar=Year,fs.test.effect.subgroup=6,fs.hetstat=5,
       test.overall.random=TRUE,test.effect.subgroup.random=TRUE)
#
png(file='Sub Types_of_Cancer GA vs GG.png',width=1000,height=2000) 
forest.jama<-forest(Types_of_Cancer.subgroup, fontsize=6,fs.heading = 9,fs.random=9, spacing=.6,
                    digits.pval=max(gs("digits.pval")-2, 6),digits.pval.Q=max(gs("digits.pval.Q")-2, 4),
                    label.left="Decreasing Risk", label.right="Increasing Risk",leftlabs=c("GA vs GG\nStudy", NA, NA, NA, NA),
                    col.by=1, sortvar=Year,fs.test.effect.subgroup=6,fs.hetstat=5,
                    test.overall.random=TRUE,test.effect.subgroup.random=TRUE)
dev.off()
############ Subgroup analysis by REM###
Source_of_Controls.subgroup<-update.meta(meta1, 
                                         byvar=Source_of_Controls, 
                                         comb.random = T)#, 

forest(Source_of_Controls.subgroup, fontsize=12,fs.heading = 19,fs.fixed=16, spacing=1.2,
       digits.pval=max(gs("digits.pval")-2, 4),digits.pval.Q=max(gs("digits.pval.Q")-2, 4),
       label.left="Decreasing Risk", label.right="Increasing Risk",leftlabs=c("GA vs GG\nStudy", NA, NA, NA, NA),
       col.by=2, sortvar=Year,fs.test.effect.subgroup=12,fs.hetstat=12,ff.study = "bold",
       test.overall.random=TRUE,test.effect.subgroup.random=TRUE) #this one is good.
##
png(file='Sub Source_of_Controls GA vs GG.png',width=1500,height=3000) 
forest(Source_of_Controls.subgroup, fontsize=12,fs.heading = 19,fs.fixed=16, spacing=1.2,
       digits.pval=max(gs("digits.pval")-2, 4),digits.pval.Q=max(gs("digits.pval.Q")-2, 4),
       label.left="Decreasing Risk", label.right="Increasing Risk",leftlabs=c("GA vs GG\nStudy", NA, NA, NA, NA),
       col.by=2, sortvar=Year,fs.test.effect.subgroup=12,fs.hetstat=12,ff.study = "bold",
       test.overall.random=TRUE,test.effect.subgroup.random=TRUE)#this one is good.
dev.off()
#