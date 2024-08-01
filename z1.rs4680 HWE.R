
#set.seed(1234)
library(meta)

data <- read.csv("./HWE rs_4680.csv")

n=dim(data)[1]
p.val=0
Chisq=0

for(i in 1:n){
  
  p = (2*data$GG.Control[i]+data$GA.Control[i])/
    (2*(data$GG.Control[i]+data$GA.Control[i]+data$AA.Control[i]))
  
  q = 1-p
  
  e.gg = p^2*(
    data$GG.Control[i]
    +data$GA.Control[i]
    +data$AA.Control[i]
  )
  
  e.ga = 2*p*q*(
    data$GG.Control[i]
    +data$GA.Control[i]
    +data$AA.Control[i]
  )
  
  e.aa = q^2*(
    data$GG.Control[i]
    +data$GA.Control[i]
    +data$AA.Control[i]
  )
  
  Chisq[i] = ((data$GG.Control[i]-e.gg)^2/e.gg)
  +((data$GA.Control[i]-e.ga)^2/e.ga)
  +((data$AA.Control[i]-e.aa)^2/e.aa)
  
  p.val[i] = pchisq(
    Chisq[i], df=1, ncp = 0, lower.tail = FALSE, log.p = FALSE)
}

p.v=round(p.val,digit=3)
Chi=round(Chisq,digit=3)

P.value=data.frame(p.v)

show=data.frame(data$Author,data$Year, data$Ethnicity,
                data$Cases,data$Controls,Chi,p.v)
colnames(show)=c("Author","Year","Ethnicity",
                 "Cases","Controls","Chi-square value",
                 "p_value")

# %>%

madata <- data %>%
  left_join(
    show,
    by = c("Author","Year","Ethnicity",
           "Cases","Controls")
  ) %>%
  mutate(
    Types_of_Cancer  = case_when(
      Types_of_Cancer  == "nhl" ~ "Other Cancer", 
      Types_of_Cancer  == "Renal Cancer" ~ "Other Cancer", 
      Types_of_Cancer  == "Cervical Cancer" ~ "Other Cancer",
      Types_of_Cancer  == "Colon Cancer" ~ "Other Cancer",
      Types_of_Cancer  == "TgcT" ~ "Other Cancer",
      TRUE~Types_of_Cancer )
  ) 
