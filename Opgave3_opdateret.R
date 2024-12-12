###########################################
### OPGAVE 3.1 – Funktion til terninger ###
###########################################

# Funktion til 25.000 kast med en terning
kast25000 <- function() {
  # Generer 25.000 kast med en terning (1-6)
  kast <- sample(1:6, size = 25000, replace = TRUE)
  
  # Beregn antallet af 5'ere og sandsynligheden
  antal_5ere <- sum(kast == 5)
  sandsynlighed_5er <- antal_5ere / 25000
  
  # Udskriv resultater
  cat("Antal 5'ere slået:", antal_5ere, "\n")
  cat("Sandsynligheden for at slå en 5'er:", sandsynlighed_5er, "\n")
}

# Kør funktionen
kast25000()

###########################
### OPGAVE 3.2 – Plot I ###
###########################

# Funktion til at slå 6 terninger
kast_terninger <- function() {
  # Slå med 6 terninger og beregn summen
  kast_resultater <- sample(1:6, size = 6, replace = TRUE)
  sum_kast <- sum(kast_resultater)
  
  # Udskriv resultaterne
  cat("Resultater af terningekast:", kast_resultater, "\n")
  cat("Summen af de 6 terninger:", sum_kast, "\n")
  
  # Returner summen for videre analyse
  return(sum_kast)
}

# Kør funktionen
kast_terninger()

# Gentag kastet 10.000 gange og gem resultaterne
antal_kast <- 10000
resultater10000 <- replicate(antal_kast, kast_terninger())

# Lav barplot af summerne
barplot(table(resultater10000),
        main = "Frekvens af summer ved 10.000 kast med 6 terninger",
        xlab = "Sum af terningekast",
        ylab = "Frekvens (Antal kast)",
        col = "lightblue",
        border = "black")

############################
### OPGAVE 3.3 – Plot II ###
############################

# Gentag kastet 1.000.000 gange og gem resultaterne
antal_kast2 <- 1000000
resultater1_000_000 <- replicate(antal_kast2, kast_terninger())

# Lav barplot af summerne
barplot(table(resultater1_000_000),
        main = "Frekvens af summer ved 1.000.000 kast med 6 terninger",
        xlab = "Sum af terningekast",
        ylab = "Frekvens (Antal kast)",
        col = "lightblue",
        border = "black")

########################################
### OPGAVE 3.4 – Lav dine egne data ###
########################################

# Tilfældig række af værdier
random <- sample(c(1, 2, 3, 5, 6))

# Ikke tilfældig række
notrandom <- 2:6

# Lav en matrix med de to rækker
matrix <- cbind(notrandom, random)

# Udskriv matrixen
cat("Matrix:\n")
print(matrix)
