library(ggplot2)
library(corrplot)

######################
# OPG 2.1
###################
boligsiden <- read.csv("boligsiden.csv")
boligsiden <- boligsiden[-1,]

# data rens
# Konverterer tal til numerisk, fjerner ikke-numeriske tegn og formaterer med punktum som tusindseparator
boligsiden$pris <- as.numeric(gsub("[^0-9]", "", boligsiden$pris))
boligsiden$liggetid <- as.numeric(gsub("[^0-9]", "", boligsiden$liggetid))# få data til at være i t.kr
boligsiden$kvmpris <- boligsiden$kvmpris*1000
boligsiden$mdudg <- boligsiden$mdudg*1000
boligsiden$grund <- boligsiden$grund*1000
boligsiden$størrelse <- as.numeric(boligsiden$størrelse)

# se om der er outliers
plot(boligsiden$kvmpris,
     main = "Scatterplot af kvmpris mod størrelse",
     ylab = "Kvmpris",
     las = 2)  # Roter x-aksen labels for bedre læsbarhed

# fjern outliers
z <- (boligsiden$kvmpris - mean(boligsiden$kvmpris, na.rm = TRUE)) / sd(boligsiden$kvmpris, na.rm = TRUE)
outliers <- which(abs(z) > 3)
boligsiden_clean <- boligsiden[-outliers,]

# Beskrivende 
gennemsnit <- aggregate(kvmpris ~ værelser, data = boligsiden_clean, FUN = mean)
gennemsnit$kvmpris <- round(gennemsnit$kvmpris, digits = 2)
gennemsnit$antal <- table(boligsiden$værelser)

# værelser vs kvmpris
{ggplot(data = gennemsnit, aes(x = værelser, y = kvmpris)) +
  geom_bar(stat = "identity", fill = "steelblue") + 
  geom_text(aes(label = round(kvmpris)), hjust = 0.5, angle = 90, position = position_stack(vjust = 0.5)) +  # Tilføj tekst centreret i barerne
  labs(title = "Antallet af værelser har ikke en effekt på kvmprisen",
       x = "Antal værelser",
       y = "Kvadratmeterpris (DKK)") +
  theme_minimal() }

# værelser vs antal af bolig
{ggplot(data = gennemsnit, aes(x = værelser, y = antal)) +
  geom_bar(stat = "identity", fill = "steelblue") + 
  geom_text(aes(label = antal), hjust = 0.5, angle = 90, position = position_stack(vjust = 0.5)) + 
  labs(title = "Flest boliger til med 4 & 5 værelser",
       x = "Antal værelser",
       y = "Antal boliger") +
  theme_minimal() }


##################
# OPG 2.2
##############

correlation <- cor(boligsiden_clean$størrelse, boligsiden_clean$pris, use = "complete.obs")
View(correlation)

###################
# OPG 2.3
####################

model1 <- lm(kvmpris ~ grund, data = boligsiden_clean)
model2 <- lm(kvmpris ~ mdudg, data = boligsiden_clean)
model3 <- lm(kvmpris ~ værelser, data = boligsiden_clean)
model4 <- lm(kvmpris ~ størrelse, data = boligsiden_clean)
model5 <- lm(kvmpris ~ liggetid, data = boligsiden_clean)
summary(model1)
summary(model2)
summary(model3)
summary(model4)
summary(model5)

# korrelations matrix
df <- data.frame(
  kvmpris = boligsiden_clean$kvmpris,
  størrelse = boligsiden_clean$størrelse,
  mdudg = boligsiden_clean$mdudg,
  grund = boligsiden_clean$grund,
  værelser = boligsiden_clean$værelser,
  liggetid = boligsiden_clean$liggetid
)

correlation_matrix <- cor(df, use = "complete.obs")
corrplot(correlation_matrix, method = "color", addCoef.col = "red", type = "upper", diag = FALSE)

# Er der forskel correlationen hvis man vælger en region ud fra postnummer?

####################
# OPG. 2.4
##################

# se besvarelse
