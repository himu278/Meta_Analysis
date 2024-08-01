
set.seed(1234)
#######################################################################################################

#########################################################################
source("./z1.rs4680 HWE.R")
###############################################################################
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
meta <- metabin(A.Case, Total.Case, A.Control, Total.Control,
                data=madata, subset=seq(dim(madata)[1]),
                sm="OR", method="I",allstudies=T,
                studlab=paste(Author, Year, sep=" "))
#########################################################################
# madata <- read.csv("Dominant model (AA+AG vs GG) rs_5219.csv")
# meta <- metabin(AA.AG.Case, Total.Case, AA.AG.Control, Total.Control,
#                 data=madata, subset=seq(dim(madata)[1]),
#                 sm="OR", method="I",
#                 studlab=paste(Author, Year, sep=" "))
# #########################################################################
# madata <- read.csv("Multiplicative model (AA vs GG) rs_5219.csv")
# meta <- metabin(AA.Case, Total.Case, AA.Control, Total.Control,
#                 data=madata, subset=seq(dim(madata)[1]),
#                 sm="OR", method="I",
#                 studlab=paste(Author, Year, sep=" "))
# #########################################################################
# madata <- read.csv("Overdominant model(AA+GG vs AG) rs_5219.csv")
# meta <- metabin(GA.Case, Total.Case, GA.Control, Total.Control,
#                 data=madata, subset=seq(dim(madata)[1]),
#                 sm="OR", method="I",
#                 studlab=paste(Author, Year, sep=" "))
# #########################################################################
# madata <- read.csv("Recessive model(AA vs AG+GG) rs_5219.csv")
# meta <- metabin(AA.Case, Total.Case, AA.Control, Total.Control,
#                 data=madata, subset=seq(dim(madata)[1]),
#                 sm="OR", method="I",
#                 studlab=paste(Author, Year, sep=" "))
# #########################################################################
# #########################################################################
# ######################## subgroup data ##################################
# 
# d=subset(madata, Ethnicity=="Asian") 
# d=subset(madata, Ethnicity=="Caucasian")
# d=subset(madata, Ethnicity=="Mixed")
# d=subset(madata, Ethnicity=="African")
# 
# d=subset(madata, Source.of.Controls =="Hospital")          
# d=subset(madata, Source.of.Controls =="Population")
# 
# d=subset(madata, Cancer.Types=="Endometrial Cancer")
# d=subset(madata, Cancer.Types=="Liver  Cancer")
# d=subset(madata, Cancer.Types=="Breast Cancer")
# d=subset(madata, Cancer.Types=="Renal Cancer")
# d=subset(madata, Cancer.Types=="Colon Cancer")
# 
# d=subset(madata, Cancer.Types=="Colon Cancer")
# d=subset(madata, Cancer.Types=="Lung Cancer")
# d=subset(madata, Cancer.Types=="Renal  Cancer")
# d=subset(madata, Cancer.Types=="Ovarian Cancer")
# d=subset(madata, Cancer.Types=="Esophageal Cancer")
# 
# 
# meta <- metabin(GA.Case, Total.Case, GA.Control, Total.Control,
#                 data=d, subset=seq(dim(d)[1]),
#                 sm="OR", method="I",
#                 studlab=paste(Author, Year, sep="_"))

#########################################################################

y =meta[24][[1]];		# y=log(OR) #TE
names(meta)
w.r =meta[33][[1]]		# random weight
v.r =1/w.r;			# variance random
v.r[is.infinite(v.r)]=0
Sigma =diag(v.r)
dim(Sigma)
set.seed(1234)

N =10^5
mu =matrix(0,nrow=dim(Sigma)[1])
S2 = mvrnorm(N, mu = mu, Sigma = Sigma)
dim(S2)

ad= ad.test(y);
AD0=ad$statistic[[1]]

cvm= cvm.test(y);
CvM0=cvm$statistic[[1]];

sf= shapiro.test(y);
SW0=sf$statistic[[1]];

AD=0;CvM=0;SW=0;

for (i in 1:N){
  g1=ad.test(S2[i,])
  AD[i]=g1$statistic[[1]]
  g2=cvm.test(S2[i,])
  CvM[i]=g2$statistic[[1]]
  g3=shapiro.test(S2[i,])
  SW[i]=g3$statistic[[1]]
}


p.AD=sum(AD>AD0)/N;	
p.CvM=sum(CvM>CvM0)/N;	
p.SW=sum(SW<SW0)/N;

length(y)
res=c(p.AD,p.CvM,p.SW);		res

