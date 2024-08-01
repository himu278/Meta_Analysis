#set.seed(1234)
## egg's and egger's test:
    
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
meta <- metabin(A.Case, Total.Case, A.Control, Total.Control,
                 data=madata, subset=seq(dim(madata)[1]),
                 sm="OR", method="I",comb.random=T,
                 studlab=paste(Author, Year))
#meta <- trimfill(meta)
##############
##Added from the picture
begg =metabias(meta,method.bias="rank",plotit=T) ;begg## BEGG's
png(file='BEGG A vs G.png',width=450,height=500) 
metabias(meta,method.bias="rank",plotit=T)
dev.off()

egger =metabias(meta,method.bias="linreg",plotit=T) ;egger ## Egger's
png(file='EGGER A vs G.png',width=450,height=500) 
metabias(meta,method.bias="linreg",plotit=T)
dev.off()

#
df_begg <- data.frame(begg["statistic"], begg["pval"])%>%
  rename(z_value_begg = statistic, p_value_begg = pval)%>%
  round(digits = 3) %>% 
  mutate(
    comment_begg = if_else(p_value_begg < 0.1, 'Has publication bias', 'No publication bias'))

df_egger <- data.frame(egger["statistic"], egger["pval"])%>%
  rename(t_value_egger = statistic, p_value_egger = pval)%>%
  round(digits = 3) %>% 
  mutate(
    comment_egger = if_else(p_value_egger < 0.1, 'Has publication bias', 'No publication bias'))
# 
rownames(df_begg)[rownames(df_begg)== "z" ] = "A vs G "
rownames(df_egger)[rownames(df_egger)== "t" ] = "A vs G "
#
df_AvsG <- df_begg%>%
  bind_cols(df_egger)
########################**********************************************************************
