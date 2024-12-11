boligsiden <- read.csv("boligsiden.csv")

#remove row
# boligsiden <-boligsiden [-1704,]
#opdater række tal
#row.names(boligsiden)=NULL

# data rens - fjern kr.
# Konverterer tal til numerisk, fjerner ikke-numeriske tegn og formaterer med punktum som tusindseparator
boligsiden$pris <- format(as.numeric(gsub("[^0-9]", "", boligsiden$pris)), big.mark = ".", decimal.mark = ",", scientific = FALSE)

# OPG 1.1
match <- data.frame(
  subset(
    boligsiden,
    (vej == "tousvej" & vejnr == 106) | (vej == "egevej" & vejnr == 20)
  )
)


# OPG 1.2
match_2 <- data.frame(
  subset(
    boligsiden,
    (vej == "salviehaven" & vejnr == 8) | (vej == "hundigevaenget" & vejnr == 11)
  )
)


# OPG 1.3
# - Se OLA 1 docs

# OPG 1.4
# - Se OLA 1 docs






