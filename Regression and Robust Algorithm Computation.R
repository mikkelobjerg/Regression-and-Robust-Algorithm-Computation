#GODE Shortcuts at huske: 
# Ctrl + L - Clean Consol
# Alt+bindestreg giver "<-"
# Ctrl + Enter = Run et markeret script
# getwd() - for at se hvilket directory du arbejder fra 

#__________________________________________________________________________Modellering____________________________________________________________________________#


library(readxl)
my_data <- read_excel("Desktop/ALL - OLS Regression.xlsx")
View(my_data)

library(readxl)
my_data <- read_excel("Desktop/ALL - 127 filtered.xlsx")
View(my_data)


#Kopieret et felt fra Excel i din udklipsholder, s? bliver det automatisk importeret her.
#For at imoportere alt kan du blot trykke Ctrl+A i det Excel-ark
my_data <- read.csv(file = "clipboard",
                    sep = "\t", header=TRUE)

#Fjern alle datapunkter man ikke kan bruge (NA). E.g. hvor vi ikke har noget data. Denne command renser alts? vores datas?t. OBS. hvis cellen er tom ??? Fjern i Excel f?rst!
my_data <- na.omit(my_data)



#N?ste stykke kode er for at transformere vores data om fra "." til ",". Vi har nogle forskellige variabler som nogle gange bliver defineret som "characters" i stedet for "Integers".
write.csv2(my_data, "/Users/mikkelobjerg/Desktop/Desktopmntdata1.csv", row.names = FALSE)
my_data  <-  read.csv2("/Users/mikkelobjerg/Desktop/Desktopmntdata1.csv")


#For at tjekke data for hvilke variabler de er samt obs. nr. mm. Her kan Summary eller sapply bruges 
summary(my_data)
lapply(my_data,class)



#Navngiver vores datas?t:
"Return" <- my_data$Return
"SimpleReturnportf." <- my_data$Simple.Returns.portfolio
"BHAR_INX" <- my_data$BHAR_INX
"BHAR_EWI" <- my_data$BHAR_EWI
"BHAR_CRSP" <- my_data$BHAR_CRSP
"BHAR" <- my_data$BHAR
"Price" <- my_data$Price
"Price2" <- my_data$Price2
"LogPrice" <- my_data$LogPrice
"Size" <- my_data$Firm.Size
"Leverage" <- my_data$Leverage
"EBITDA" <- my_data$EBITDA
"FCF" <- my_data$FCF
"ROE" <- my_data$ROE
"TotalEquity" <- my_data$Total.Equity
"RE" <- my_data$Retained.Earnings
"OP" <- my_data$Tot..Offer.proceeds
"Volatility" <- my_data$VIX
"Age" <- my_data$Age...26
"E.Var" <- my_data$E
"S.Var"<- my_data$S
"G.Var"<- my_data$G
"ESG.Var"<- my_data$ESG
"I.BIO" <- my_data$IND_BIO_Dummy
"I.IND" <- my_data$IND_IND_Dummy
"I.Chem" <- my_data$IND_CHEM_Dummy
"B.I.C-Non" <- my_data$Bloomberg_IND_Consumer_NonCyclical
"B.I.C-Cyc" <- my_data$Bloomberg_IND_Consumer_Cyclical
"B.I.Energy" <- my_data$Bloomberg_IND_Energy
"B.I.Fin" <- my_data$Bloomberg_IND_Financial
"B.I.Tech" <- my_data$Bloomberg_IND_Tech





#Model ALL
IPOModelBHAR <- lm(BHAR ~ Size + Leverage + EBITDA + FCF + ROE + TotalEquity + RE + Volatility + OP + Age + `B.I.C-Cyc` + `B.I.C-Non` + B.I.Energy
                   + B.I.Fin + B.I.Tech)
IPOModelBHAR_INX <- lm(BHAR_INX ~ Size + Leverage + EBITDA + FCF + ROE + TotalEquity + RE + Volatility + OP + Age + `B.I.C-Cyc` + `B.I.C-Non` + B.I.Energy
                       + B.I.Fin + B.I.Tech)
IPOModelBHAR_EWI <- lm(BHAR_EWI ~ Size + Leverage + EBITDA + FCF + ROE + TotalEquity + RE + Volatility + OP + Age + `B.I.C-Cyc` + `B.I.C-Non` + B.I.Energy
                       + B.I.Fin + B.I.Tech)
IPOModelBHAR_CRSP <- lm(BHAR_CRSP ~ Size + Leverage + EBITDA + FCF + ROE + TotalEquity + RE + Volatility + OP + Age + `B.I.C-Cyc` + `B.I.C-Non` + B.I.Energy
                        + B.I.Fin + B.I.Tech)
IPOModelPrice <- lm(Price ~ Size + Leverage + EBITDA + FCF + ROE + TotalEquity + RE + Volatility + OP + Age + `B.I.C-Cyc` + `B.I.C-Non` + B.I.Energy
                    + B.I.Fin + B.I.Tech)
IPOModelLogPrice <- lm(LogPrice ~ Size + Leverage + EBITDA + FCF + ROE + TotalEquity + RE + Volatility + OP + Age + `B.I.C-Cyc` + `B.I.C-Non` + B.I.Energy
                       + B.I.Fin + B.I.Tech)

#Filtered
IPOfilterBHAR <- lm(BHAR ~ Size + Leverage + EBITDA + FCF + ROE + TotalEquity + RE + Volatility + OP + Age + `B.I.C-Cyc` + `B.I.C-Non` + B.I.Energy
                    + B.I.Fin + B.I.Tech + E.Var + S.Var + G.Var)
IPOfilterReturn <- lm(Return ~ Size + Leverage + EBITDA + FCF + ROE + TotalEquity + RE + Volatility + OP + Age + `B.I.C-Cyc` + `B.I.C-Non` + B.I.Energy
                      + B.I.Fin + B.I.Tech + E.Var + S.Var + G.Var)
IPOfilterSimpleReturn <- lm(SimpleReturnportf. ~ Size + Leverage + EBITDA + FCF + ROE + TotalEquity + RE + Volatility + OP + Age + `B.I.C-Cyc` + `B.I.C-Non` + B.I.Energy
                            + B.I.Fin + B.I.Tech + E.Var + S.Var + G.Var)
IPOfilterBHAR_INX <- lm(BHAR_INX ~ Size + Leverage + EBITDA + FCF + ROE + TotalEquity + RE + Volatility + OP + Age + `B.I.C-Cyc` + `B.I.C-Non` + B.I.Energy
                        + B.I.Fin + B.I.Tech + E.Var + S.Var + G.Var)
IPOfilterBHAR_EWI <- lm(BHAR_EWI ~ Size + Leverage + EBITDA + FCF + ROE + TotalEquity + RE + Volatility + OP + Age + `B.I.C-Cyc` + `B.I.C-Non` + B.I.Energy
                        + B.I.Fin + B.I.Tech + E.Var + S.Var + G.Var)
IPOfilterBHAR_CRSP <- lm(BHAR_CRSP ~ Size + Leverage + EBITDA + FCF + ROE + TotalEquity + RE + Volatility + OP + Age + `B.I.C-Cyc` + `B.I.C-Non` + B.I.Energy
                         + B.I.Fin + B.I.Tech + E.Var + S.Var + G.Var)
IPOfilterPrice <- lm(Price ~ Size + Leverage + EBITDA + FCF + ROE + TotalEquity + RE + Volatility + OP + Age + `B.I.C-Cyc` + `B.I.C-Non` + B.I.Energy
                     + B.I.Fin + B.I.Tech + E.Var + S.Var + G.Var)
IPOfilterLogPrice <- lm(LogPrice ~ Size + Leverage + EBITDA + FCF + ROE + TotalEquity + RE + Volatility + OP + Age + `B.I.C-Cyc` + `B.I.C-Non` + B.I.Energy
                        + B.I.Fin + B.I.Tech + E.Var + S.Var + G.Var)



#stargazer (For at vise output)
#ALL
stargazer(IPOModelBHAR, type = "text")
stargazer(IPOModelBHAR_INX, type = "text")
stargazer(IPOModelBHAR_EWI, type = "text")
stargazer(IPOModelBHAR_CRSP, type = "text")
stargazer(IPOModelPrice, type = "text")
stargazer(IPOModelLogPrice, type = "text")


#filter
stargazer(IPOfilterBHAR, type = "text")
stargazer(IPOfilterBHAR_INX, type = "text")
stargazer(IPOfilterBHAR_EWI, type = "text")
stargazer(IPOfilterBHAR_CRSP, type = "text")
stargazer(IPOfilterPrice, type = "text")
stargazer(IPOfilterLogPrice, type = "text")
stargazer(IPOfilterReturn, type = "text")


#Tjekker for multikollinaritet 
library(car)
vif()

vif(IPOfilterBHAR)
vif(IPOfilterBHAR_INX)
vif(IPOfilterBHAR_EWI)
vif(IPOfilterBHAR_CRSP)
vif(IPOfilterPrice)
vif(IPOfilterLogPrice)
vif(IPOfilterReturn)



vif(IPOModelBHAR)
vif(IPOModelBHAR_INX)
vif(IPOModelBHAR_EWI)
vif(IPOModelBHAR_CRSP)
vif(IPOModelPrice)
vif(IPOModelLogPrice)
vif(IPOModelReturn)



----- #Robust reg -------
install.packages("robustbase")
library(robustbase)

#ALL
Rob.IPOModelBHAR <- lmrob(BHAR ~ Size + Leverage + EBITDA + FCF + ROE + TotalEquity + RE + Volatility + OP + Age + `B.I.C-Cyc` + `B.I.C-Non` + B.I.Energy
                          + B.I.Fin + B.I.Tech)
Rob.IPOModelBHAR_INX <- lmrob(BHAR_INX ~ Size + Leverage + EBITDA + FCF + ROE + TotalEquity + RE + Volatility + OP + Age + `B.I.C-Cyc` + `B.I.C-Non` + B.I.Energy
                              + B.I.Fin + B.I.Tech)
Rob.IPOModelBHAR_EWI <- lmrob(BHAR_EWI ~ Size + Leverage + EBITDA + FCF + ROE + TotalEquity + RE + Volatility + OP + Age + `B.I.C-Cyc` + `B.I.C-Non` + B.I.Energy
                              + B.I.Fin + B.I.Tech)
Rob.IPOModelBHAR_CRSP <- lmrob(BHAR_CRSP ~ Size + Leverage + EBITDA + FCF + ROE + TotalEquity + RE + Volatility + OP + Age + `B.I.C-Cyc` + `B.I.C-Non` + B.I.Energy
                               + B.I.Fin + B.I.Tech)
Rob.IPOModelPrice <- lmrob(Price ~ Size + Leverage + EBITDA + FCF + ROE + TotalEquity + RE + Volatility + OP + Age + `B.I.C-Cyc` + `B.I.C-Non` + B.I.Energy
                           + B.I.Fin + B.I.Tech)
Rob.IPOModelLogPrice <- lmrob(LogPrice ~ Size + Leverage + EBITDA + FCF + ROE + TotalEquity + RE + Volatility + OP + Age + `B.I.C-Cyc` + `B.I.C-Non` + B.I.Energy
                              + B.I.Fin + B.I.Tech)

summary(Rob.IPOModelBHAR)
summary(Rob.IPOModelBHAR_INX)
summary(Rob.IPOModelBHAR_EWI)
summary(Rob.IPOModelBHAR_CRSP)
summary(Rob.IPOModelPrice)
summary(Rob.IPOModelLogPrice)


#Filtered

Rob.IPOfilterBHAR <- lmrob(BHAR ~ Size + Leverage + EBITDA + FCF + ROE + TotalEquity + RE + Volatility + OP + Age + `B.I.C-Cyc` + `B.I.C-Non` + B.I.Energy
                           + B.I.Fin + B.I.Tech + E.Var + S.Var + G.Var)
Rob.IPOfilterReturn <- lmrob(Return ~ Size + Leverage + EBITDA + FCF + ROE + TotalEquity + RE + Volatility + OP + Age + `B.I.C-Cyc` + `B.I.C-Non` + B.I.Energy
                             + B.I.Fin + B.I.Tech + E.Var + S.Var + G.Var)
Rob.IPOfilterSimpleReturn <- lmrob(SimpleReturnportf. ~ Size + Leverage + EBITDA + FCF + ROE + TotalEquity + RE + Volatility + OP + Age + `B.I.C-Cyc` + `B.I.C-Non` + B.I.Energy
                                   + B.I.Fin + B.I.Tech + E.Var + S.Var + G.Var)
Rob.IPOfilterBHAR_INX <- lmrob(BHAR_INX ~ Size + Leverage + EBITDA + FCF + ROE + TotalEquity + RE + Volatility + OP + Age + `B.I.C-Cyc` + `B.I.C-Non` + B.I.Energy
                               + B.I.Fin + B.I.Tech + E.Var + S.Var + G.Var)
Rob.IPOfilterBHAR_EWI <- lmrob(BHAR_EWI ~ Size + Leverage + EBITDA + FCF + ROE + TotalEquity + RE + Volatility + OP + Age + `B.I.C-Cyc` + `B.I.C-Non` + B.I.Energy
                               + B.I.Fin + B.I.Tech + E.Var + S.Var + G.Var)
Rob.IPOfilterBHAR_CRSP <- lmrob(BHAR_CRSP ~ Size + Leverage + EBITDA + FCF + ROE + TotalEquity + RE + Volatility + OP + Age + `B.I.C-Cyc` + `B.I.C-Non` + B.I.Energy
                                + B.I.Fin + B.I.Tech + E.Var + S.Var + G.Var)
Rob.IPOfilterPrice <- lmrob(Price ~ Size + Leverage + EBITDA + FCF + ROE + TotalEquity + RE + Volatility + OP + Age + `B.I.C-Cyc` + `B.I.C-Non` + B.I.Energy
                            + B.I.Fin + B.I.Tech + E.Var + S.Var + G.Var)
Rob.IPOfilterLogPrice <- lmrob(LogPrice ~ Size + Leverage + EBITDA + FCF + ROE + TotalEquity + RE + Volatility + OP + Age + `B.I.C-Cyc` + `B.I.C-Non` + B.I.Energy
                               + B.I.Fin + B.I.Tech + E.Var + S.Var + G.Var)


summary(Rob.IPOfilterBHAR)
summary(Rob.IPOfilterBHAR_INX)
summary(Rob.IPOfilterBHAR_EWI)
summary(Rob.IPOfilterBHAR_CRSP)
summary(Rob.IPOfilterPrice)
summary(Rob.IPOfilterLogPrice)


library("dplyr")
install.packages("ggpubr")



shapiro.test(Size)
shapiro.test(Leverage)
shapiro.test(EBITDA)
shapiro.test(FCF)
shapiro.test(ROE)
shapiro.test(TotalEquity)
shapiro.test(RE)
shapiro.test(Volatility)
shapiro.test(OP)
shapiro.test(Age)
shapiro.test(Size)
shapiro.test(`B.I.C-Cyc`)
shapiro.test(`B.I.C-Non`)
shapiro.test(B.I.Energy)
shapiro.test(B.I.Fin)
shapiro.test(B.I.Tech)
shapiro.test(E.Var)
shapiro.test(S.Var)
shapiro.test(G.Var)


ModelExample <- lm(BHAR_INX ~ EBITDA)
ModelExample2 <- lm(BHAR_INX ~ Size)
ModelExample3 <- lm(BHAR_INX ~ Age)
ModelExample4 <- lm(BHAR_INX ~ S.Var)


install.packages("lmtest")
library("lmtest")
bptest(ModelExample)
bptest(ModelExample2)
bptest(ModelExample3)
bptest(ModelExample4)

bptest(IPOfilterBHAR_EWI)
bptest(IPOfilterBHAR_INX)
