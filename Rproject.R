library(tidyr)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(hrbrthemes)
library(RColorBrewer)
library(ggthemes)

#DS <- `US_births(2018)`
#DS <- DS[c(1:600000),]

#### TRASFORMAZIONE DELLA TABELLA

DS$ATTEND <- as.factor(DS$ATTEND)
levels(DS$ATTEND) <- c("Doctor of Medicine","Doctor of Osteopathy","Certified Nurse Midwife","Other Midwife",
                       "Other", "Unknown")

DS$BFACIL <- as.factor(DS$BFACIL)
levels(DS$BFACIL) <- c("Hospital","Freestanding Birth Center","Home (intended)","Home (not intended)", "Home (unknown)",
                       "Clinic / Doctor's Office", "Others", "Unknown")

DS$DLMP_MM <- as.factor(DS$DLMP_MM)
levels(DS$DLMP_MM) <- c("January","February","March","April","May","June","July","August","September",
                        "October","November","December","Unknown")

DS$DMAR <- as.factor(DS$DMAR)
levels(DS$DMAR) <- c("Married","Unmarried")

DS$DOB_MM <- as.factor(DS$DOB_MM)
levels(DS$DOB_MM) <- c("January","February","March","April","May","June","July","August","September",
                       "October","November","December")

DS$DOB_WK <- as.factor(DS$DOB_WK)
levels(DS$DOB_WK) <- c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday")

DS$FEDUC <- as.factor(DS$FEDUC)
levels(DS$FEDUC) <- c("8th grade or less","9th to 12th grade with no diploma","High School",
                      "College credit, no degree","Associate degree","Bachelor's degree",
                      "Master's degree", "Doctorate", "Unknown")

DS$FHISPX <- as.factor(DS$FHISPX)
levels(DS$FHISPX) <- c("Non Hispanic","Mexican","Puerto Rican","Cuban","Central or South American",
                       "Dominican","Other and Unknown Hispanic", "Unknown or not stated")

DS$DOB_WK <- as.factor(DS$DOB_WK)
levels(DS$DOB_WK) <- c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday")

DS$FRACE6 <- as.factor(DS$FRACE6)
levels(DS$FRACE6) <- c("White","Black","AIAN","Asian","NHOPI","More than one race","Unknown")

DS$MBSTATE_REC <- as.factor(DS$MBSTATE_REC)
levels(DS$MBSTATE_REC) <- c("Born in the U.S.","Born outside the U.S.","Unknown")

DS$MEDUC <- as.factor(DS$MEDUC)
levels(DS$MEDUC) <- c("8th grade or less","9th to 12th grade with no diploma","High School",
                      "College credit, no degree","Associate degree","Bachelor's degree",
                      "Master's degree", "Doctorate", "Unknown")

DS$MHISPX <- as.factor(DS$MHISPX)
levels(DS$MHISPX) <- c("Non Hispanic","Mexican","Puerto Rican","Cuban","Central or South American",
                       "Dominican","Other and Unknown Hispanic", "Unknown or not stated")

colnames(DS)[36] <- "MRACE6"
DS$MRACE6 <- as.factor(DS$MRACE6)
levels(DS$MRACE6) <- c("White","Black","AIAN","Asian","NHOPI","More than one race")

DS$PAY <- as.factor(DS$PAY)
levels(DS$PAY) <- c("Medicaid", "Private Insurance","Self-Pay","Indian Health Service",
                    "CHAMPUS / TRICARE", "Other Government","Other","Unknown")

DS$RDMETH_REC <- as.factor(DS$RDMETH_REC)
levels(DS$RDMETH_REC) <- c("Vaginal","Vaginal after previous C-section","Primary C-section",
                           "Repeated C-section","Vaginal (unknown if previous C-section)",
                           "C-section (unknown if previous C-section)","Not stated")

DS$RESTATUS <- as.factor(DS$RESTATUS)
levels(DS$RESTATUS) <- c("RESIDENT","INTRASTATE NONRESIDENT","INTRASTATE RESIDENT","FOREIGN RESIDENT")


######## GRAFICO A TORTA DEL SESSO DEL NEONATO

piechart <- as.data.frame(table(DS$SEX))
names(piechart) <- c("sex","freq")
piechart <- cbind(piechart, percent = c(piechart$freq*100/sum(piechart$freq)))
piechart <- piechart %>%
  arrange(desc(freq)) %>%
  mutate(ypos = cumsum(freq) - 0.5*freq)
piechart

ggplot(piechart, aes(x = "", y = freq, fill = sex)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y", start = 0) +
  theme_void() +
  geom_text(aes(label = paste(round(percent,2),"%")),position = position_stack(vjust = 0.5), 
            size = 3.5, color = "white") +
  labs(x=NULL, y=NULL, fill = NULL, title = "Newborn's Gender") +
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"))

########## NELLA POPOLAZIONE SI ANALIZZA LA VARIABILE X "PESO", QUANTI BAMBINI SONO NORMOPESO? 

ds <- DS[(DS$DBWT != 9999),]
ds <- as.data.frame(ds %>%
                      group_by(SEX) %>%
                      summarise(mean_DBWT = mean(DBWT), sd_DBWT = sd(DBWT), var_DBWT = var(DBWT), count = n()))
ds


paste(round((pnorm((4500-ds[2,2])/ds[2,3]) - pnorm((2500-ds[2,2])/ds[2,3]))*100,2),"% dei neonati maschi è normopeso")
paste(round((pnorm((4500-ds[1,2])/ds[1,3]) - pnorm((2500-ds[1,2])/ds[1,3]))*100,2),"% dei neonati femmine è normopeso")


######### SI ESTRAE UN CAMPIONE CASUALE DI 2000 INDIVIDUI E SI ESEGUE LA REGRESSIONE TRA IL PESO
######### DEL NEONATO E IL BMI DELLA MADRE

wt <- DS[(DS$DBWT != 9999),]     #rimuovo neonati con peso sconosciuto
wt <- wt[(wt$BMI != 99.9),]    #rimuovo mamme con BMI sconosciuto
rand <- sample(1:nrow(wt), 2000)

point <- data.frame(matrix(ncol=ncol(DS), nrow = 0))
colnames(point) <- colnames(DS)
point

for (j in 1:length(rand))
{
  point[j,] <- wt[(rand[j]),]
}

point$SEX <- as.factor(point$SEX)
levels(point$SEX) <- c("F","M")

ggplot(point, aes(x=BMI, y = DBWT, fill = SEX)) + 
  geom_point(size = 1, aes(colour = SEX), show.legend = T) +
  facet_wrap(~SEX) +
  scale_y_continuous(breaks = seq(0,max(point$DBWT)+300,200), name = "Born's weight (g)\n") +
  theme(axis.text=element_text(size=7), axis.title=element_text(size=10,face="bold"), legend.title = element_blank(),
        legend.position = "top", panel.background = element_rect(fill = "white"), 
        plot.title = element_text(face = "bold"), panel.grid = element_line(colour = "lightgray")) +
  scale_x_continuous(name = "\nMother's BMI", breaks = seq(0,max(wt$BMI)+2,3)) +
  ggtitle("Newborn's Weight (g) ~ Mother's BMI") +
  geom_smooth(method='lm', se = T, show.legend = F)
#stat_ellipse(geom = "polygon",level = 0.98, alpha = 0.15) 


########### NEL CAMPIONE C'E' CORRELAZIONE TRA IL PESO DEL BAMBINO E IL BMI DELLA MADRE?

campione <- as.data.frame(point %>%
                            group_by(SEX) %>%
                            summarise(mean_BMI = mean(BMI), mean_DBWT = mean(DBWT), 
                                      sd_DBWT = sd(DBWT), sd_BMI = sd(BMI), cov = cov(point$BMI,point$DBWT), count = n()))
campione

paste("il coefficiente di correlazione per i maschi è",round(campione[2,6]/(campione[2,4]*campione[2,5]),3))
paste("il coefficiente di correlazione per le femmine è",round(campione[1,6]/(campione[1,4]*campione[1,5]),3))


################# ISTOGRAMMA DISTRIBUZIONE PESO

#str(hist(wt$DBWT, breaks = c(min(wt$DBWT),2500,4500,6500,max(wt$DBWT))))

ggplot(wt,aes(x=DBWT, fill = SEX,color = SEX)) + 
  # facet_wrap(~SEX) +
  geom_histogram(aes(y=..density..),alpha = 0.3, show.legend = T, binwidth = 50, position = "identity") +
  scale_x_continuous(breaks = c(2500, 4500, 6500, mean(wt$DBWT)), name = "\nInfant's weight (g)") +
  theme(axis.text = element_text(size = 6.8), legend.position = "top", axis.title = element_text(size = 9.5), legend.title = element_blank()) +
  scale_y_continuous(name = "density\n") +
  geom_vline(xintercept = mean(wt$DBWT, color = SEX), size = 0.4, linetype = "dashed") + 
  ggtitle("Weight classes distribution\n") + 
  geom_density(alpha = 0.08, show.legend = F, size = 0.4) + theme_fivethirtyeight()

ggplot(wt,aes(x=DBWT, fill = SEX,color = SEX)) + 
  #facet_wrap(~SEX) +
  geom_histogram(aes(y=..density..,color = SEX), fill = "white", alpha = 0.3, show.legend = T, binwidth = 50, position = "identity") +
  scale_x_continuous(breaks = c(2500, 4500, 6500, mean(wt$DBWT)), name = "\nInfant's weight (g)") +
  theme(axis.text = element_text(size = 6.8), legend.position = "top", axis.title = element_text(size = 9.5), legend.title = element_blank()) +
  scale_y_continuous(name = "density\n") +
  geom_vline(xintercept = mean(wt$DBWT, color = SEX), size = 0.5, linetype = "dashed") + 
  ggtitle("Weight classes distribution\n") + 
  geom_density(alpha = 0.08, show.legend = F) + theme_fivethirtyeight()


##### GRAFICO DI DENSITA' TRA LA RAZZA DELLA MADRE E IL PESO

library(BioStatR)

df <- DS[(DS$DBWT != 9999),]

ggplot(df, aes(x=DBWT, fill=MRACE6)) +
  geom_density(alpha = 0.4,show.legend = F) +
  facet_wrap(~MRACE6) +
  scale_fill_brewer(palette = "Blues") +
  theme_fivethirtyeight() +
  scale_x_continuous(name = "density\n", breaks = c(2500,4500,6500)) +
  ggtitle("Density plot by race\n") +
  theme(legend.title = element_blank(), axis.text.x = element_text(size = 8), 
        strip.text.x = element_text(face="bold"))

chi <- chisq.test(df$MRACE6,df$DBWT)
chi_norm <- chi$statistic/(nrow(df)*min(nrow(df)-1,ncol(df)-1))
chi_norm
eta2(df$DBWT,df$MRACE6)


########### GRAFICO DI DENSITA' TRA IL PESO E LA RAZZA DEL PADRE

library(BioStatR)

df <- DS[which(DS$DBWT != 9999 & DS$FRACE6 != "Unknown"),]

ggplot(df, aes(x=DBWT, fill=FRACE6)) +
  geom_density(alpha = 0.4,show.legend = F) +
  facet_wrap(~FRACE6) +
  scale_fill_brewer(palette = "Blues") +
  theme_fivethirtyeight() +
  scale_x_continuous(name = "Weight", breaks = c(2500,4500,6500)) +
  ggtitle("Density plot by father's ethnicity ~ Newborn's weight (g)\n") +
  theme(legend.title = element_blank(), axis.text.x = element_text(size = 8), 
        strip.text.x = element_text(face="bold"))

chi <- chisq.test(df$FRACE6,df$DBWT)
chi_norm <- chi$statistic/(nrow(df)*min(nrow(df)-1,ncol(df)-1))
chi_norm
eta2(df$DBWT,df$FRACE6)


############# INTERVALLO DI CONFIDENZA DELLA PERCENTUALE DI BAMBINI SOTTOPESO NEL CAMPIONE

# prima rilancio il comando che esclude i valori sconosciuti di DBWT

p <- pnorm((2500-mean(wt$DBWT))/sd(wt$DBWT)) #PROBABILITà CHE SIA < 2500g
a = 0.98
qnorm(1-((1-a)/2))

p + qnorm(1-((1-a)/2))*sqrt((p*(1-p))/nrow(point))
p - qnorm(1-((1-a)/2))*sqrt((p*(1-p))/nrow(point))

############# INTERVALLO DI CONFIDENZA DELLA MEDIA DEL PESO DEI BAMBINI NEL CAMPIONE

mean(wt$DBWT) - qnorm(1-((1-a)/2)) * sqrt(var(wt$DBWT)/nrow(point))
mean(wt$DBWT) + qnorm(1-((1-a)/2)) * sqrt(var(wt$DBWT)/nrow(point))





## QUESTO VALORE CADE NELL'AREA DI RIFIUTO DI Z CON a = 0.95??? (INTERVALLI ESTERNI)

a = 0.98
qnorm(1-((1-a)/2))

abs((pnorm((2500-mean(point$DBWT))/sd(point$DBWT)) - 0.08)/sqrt((0.08*(1-0.08))/nrow(point))) > qnorm(1-((1-a)/2))

############ INTERVALLO DI CONFIDENZA : voglio una stima della media del peso del neonato nel campione
# senza distinzione di sesso

mean(df$DBWT) - qnorm(1-((1-a)/2)) * (sd(df$DBWT)/sqrt(2000))

paste("IC: [",mean(df$DBWT) - qnorm(1-((1-a)/2)) * (sd(df$DBWT)/sqrt(2000)),",",
      mean(df$DBWT) + qnorm(1-((1-a)/2)) * (sd(df$DBWT)/sqrt(2000)),"]"
      )


ggplot(point, aes(x=c(1:nrow(point)), y = DBWT, fill = "#5F9EA0")) + 
  geom_point(size = 1, aes(colour = SEX), show.legend = F) +
  #facet_wrap(~SEX) +
  scale_y_continuous(name = "Born's weight (g)\n", breaks = c(mean(df$DBWT), seq(0,8000,1000))) +
  theme(axis.text=element_text(size=7), axis.title=element_text(size=10,face="bold"), 
        legend.title = element_blank()) +
  scale_x_continuous(name = "", breaks = seq(0,nrow(df),100)) +
  ggtitle("Weight mean confidence interval in the sample\n") +
  stat_ellipse(geom = "polygon",level = 0.95, alpha = 0.4, type = "t", show.legend = F) +
  scale_fill_manual(values=c("#5F9EA0")) +
  geom_hline(yintercept = mean(point$DBWT), linetype = "dashed", show.legend = F) +
  theme_minimal()
  
