# OPG 4.1
library(readxl)
alko <- read_excel("alko.xlsx")
alko <- alko[-1,]
colnames(alko) <- as.character(unlist(alko[1, ]))

# Illustrer udviklingen i de enkelte grupper.
alko_trans <- t(alko)
alko_trans <- as.data.frame(alko_trans)
colnames(alko_trans) <- as.character(unlist(alko_trans[1, ]))
alko_trans <- alko_trans[,-11]
alko_trans$År=seq(from = 2000, to = 2022, by = 1)

# gør kollonnerne numeriske
install.packages("dplyr")
library("dplyr")
alko_trans <- alko_trans %>%
  mutate(across(1:11, as.numeric))

# visualiser udviklingen
install.packages("tidyr")
library(ggplot2)
library(tidyr)

# alko_trans har følgende struktur:
# År | 02.1.1.1 Spiritus | 02.1.1.2 Alkoholiske læskedrikke | ...

colnames(alko_trans) <- gsub("[0-9.]+", "", colnames(alko_trans))

# Smelt data til et længere format
alko_long <- alko_trans %>%
  pivot_longer(cols = -År, names_to = "Drikkevare", values_to = "Forbrug")

# plot
ggplot(alko_long, aes(x = År, y = Forbrug)) +
  geom_line() +
  facet_wrap(~ Drikkevare, scales = "free_y") +
  labs(x = "År", y = "Forbrug",
       caption = "Kilde: Danmarks Statistik") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, face = "bold", size = 11),
        strip.text = element_text(size = 14, face = "bold")) +
  scale_x_continuous(breaks = seq(2000, 2022, by = 1))


ggplot(alko_trans, aes(x = År, y = Forbrug)) +
  geom_line() +
  facet_wrap(~ Drikkevare, scales = "free_y") +
  labs(x = "År", y = "Forbrug") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 12))
axis.title.x = element_text(size = 14, face = "bold")
axis.title.y = element_text(size = 14, face = "bold")

# Samlet plot og med farver
ggplot(alko_long, aes(x = År, y = Forbrug, color = Drikkevare)) +
  geom_line() +
  labs(x = "År", y = "Forbrug") +
  theme_minimal()

ggplot(alko_long, aes(x = År, y = Forbrug, color = Drikkevare)) +
  geom_line() +
  labs(x = "År", y = "Forbrug", color = "Drikkevare",
       caption = "Kilde: Danmarks Statistik") +
  scale_color_manual(values = c("red", "blue", "green", "purple", "orange", "yellow", "black", "gray", "pink", "brown")) +
  scale_y_continuous(breaks = seq(0, max(alko_long$Forbrug), by = 100)) +
  theme_minimal() +
  theme(legend.position = "bottom",
        legend.text = element_text(size = 12))+ 
  scale_x_continuous(breaks = seq(2000, 2022, by = 1))

# OPG 4.2
# Lav korrelations matrix
install.packages("datarium")
install.packages("reshape2")
install.packages("corrplot")
install.packages("ggcorrplot")
library(datarium)
library(reshape2)
library(corrplot)
library(ggcorrplot)

# Visualisering

# Eksempel 1
corrplot(cor(alko_trans), method = "square")

## Eksempel 2
ggcorrplot(cor(alko_trans))