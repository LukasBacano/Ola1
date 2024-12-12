#### 4 ####
#### 4.1 ####
library(dkstat)
#indlæs filen med api

alko <- dst_meta(table = "FU02", lang = "da")
#Filter
alko_filter <- list(
  KONSUMGRP = c("02.1.1.1 Spiritus og likør",
                "02.1.1.2 Alkoholiske læskedrikke",
                "02.1.2.1 Vin af druer",
                "02.1.2.2 Vin af andre frugter",
                "02.1.2.3 Hedvin",
                "02.1.2.4 Vinbaserede drikkevarer og alkoholfri vin",
                "02.1.3.1 Pilsnerøl, guldøl",
                "02.1.3.2 Andre alkoholholdige øl",
                "02.1.3.3 Øl med lavt alkoholindhold og alkoholfri øl",
                "02.1.3.4 Øl-baserede drikkevarer"),
  PRISENHED = "Faste priser",
  Tid = "*")
#brug filter på hentet data
alko <- dst_get_data(table = "FU02", query = alko_filter, lang = "da")

#lav tid om til date kun i år
alko$TID <- as.Date(alko$TID) 
alko$Year <- format(alko$TID, "%Y")  

# Omstrukturér data til bredt format
alko <- reshape(alko, 
                timevar = "Year",    # Kolonnen, der bliver kolonne-navne
                idvar = "KONSUMGRP", # Unik identifikator (rækker)
                direction = "wide",  # Omstrukturér til bredt format
                v.names = "value")   # Kolonne med værdier

# Fjern "value"  i kol-navn for maximum dataframelækkerhed
colnames(alko) <- sub("value.", "", colnames(alko))
#fjern prisenhed og col med 1994
alko <- alko[,-c(2,3)]
#fjerne alle na værdier i df
alko <- na.omit(alko)
#Lav en row med årstal
alko[11,] <- colnames(alko)
#vend skidtet om
alko <- t(alko)
#giv rigtige colnames
colnames(alko) <- alko[1,]
#slet ligegyldig linje
alko <- alko[-1,]
#giv årene et rigtigt colnavn
colnames(alko)[11] <- "Year"
#Korrekt retning
alko <- as.data.frame(alko[,c(11,1,2,3,4,5,6,7,8,9,10)])
#reset række numre 
rownames(alko) <- NULL
# fjern de tal som er med i overskrifterne
colnames(alko) <- gsub("^[0-9.]+ ", "", colnames(alko))
alko <- as.data.frame(alko)
#LAV TIL NUMERIC
alko[[1]] <- as.numeric(alko[[1]])
alko[[2]] <- as.numeric(alko[[2]])
alko[[3]] <- as.numeric(alko[[3]])
alko[[4]] <- as.numeric(alko[[4]])
alko[[5]] <- as.numeric(alko[[5]])
alko[[6]] <- as.numeric(alko[[6]])
alko[[7]] <- as.numeric(alko[[7]])
alko[[8]] <- as.numeric(alko[[8]])
alko[[9]] <- as.numeric(alko[[9]])
alko[[10]] <- as.numeric(alko[[10]])
alko[[11]] <- as.numeric(alko[[11]])
#skær til 2000
alko <- alko[7:nrow(alko),]


#PROCENT FORDELING AF HELE SPRUTDATAEN
PROC <- colSums(alko[2:ncol(alko)])
Procent <- PROC / sum(PROC) * 100

plot_data <- data.frame(
  Kategori = names(PROC),
  Procent = Procent
)
plot_data <- plot_data[c(1,3,7),]

# Lav et ggplot
# Lav et ggplot
ggplot(plot_data, aes(x = reorder(Kategori, -Procent), y = Procent, fill = Kategori)) +
  geom_bar(stat = "identity") +  # Barplot
  geom_text(aes(label = sprintf("%.1f %%", Procent)),  # Tilføj procenter som tekst
            vjust = -0.5,  # Placér teksten lidt over søjlerne
            size = 4) +  # Justér tekststørrelsen
  labs(
    title = "Tilsammen udgør vores tre største kategori'er hele 93,1%",
    x = "Kategori",
    y = "Procent (%)"
  ) +
  theme_minimal(base_size = 14) +  # Minimal theme
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  # Rotér x-aksens tekst for bedre læsbarhed
    legend.position = "none"  # Skjul legenden, da kategorier er på x-aksen
  )


#df med kun sprut, vin og bajz
realalko <- alko[,c(1,2,4,8,9)]
#realalko$`Pilsnerøl, guldøl` <- realalko$`Pilsnerøl, guldøl`+realalko$`Andre alkoholholdige øl`
realalko <- realalko[,-5]
#colnames(realalko)[4] <- "Alle sorter øl"


library(ggplot2) # brugt til at lave et plot
library(tidyr)  # brugt til at omforme det sidste data

# Omdanne data til long format 
alko_long <- pivot_longer(realalko, cols = -Year, names_to = "Kategori", values_to = "Forbrug")

# Convert the Forbrug column to numeric
alko_long$Forbrug <- as.numeric(alko_long$Forbrug)

# Remove the unwanted category
alko_long <- alko_long[alko_long$Kategori != "ALKOHOLISKE DRIKKEVARER OG TOBAK", ]

# Skabe plot 
ggplot(alko_long, aes(x = Year, y = Forbrug, color = Kategori, group = Kategori)) +
  geom_line(size = 1.5) +  # Tyggelse af linjerne
  geom_point(size = 1.7) +   # størrelsen af punkterne
  labs(
    title = "De danske husstande bruger flest penge på vin",
    x = "År",
    y = "Faste priser i kr. pr. husstand",
    color = "Kategori"
  ) +
  scale_y_continuous(breaks = seq(0, 3000, by = 1000)) +  # punkter på y axis
  theme_minimal(base_size = 14) +  # Minimal theme with larger text
  theme(
    text = element_text(size = 12),
    plot.title = element_text(hjust = 0.5, size = 16),  # Sæt titlen i midten af plottet
    legend.position = "bottom",                         # Move legend to the bottom
    legend.title = element_text(size = 12),             # Adjust legend title size
    legend.text = element_text(size = 10)               # Adjust legend text size
  )+
  annotate("text", x = max(alko_long$Year), y = 0, 
           label = "* Det lille dip i øl i år 2014 skyldes kategoriændringer fra DST, som blev ændret tilbage i 2017", hjust = 1, vjust = -1, size = 3, color = "gray50")  # Tilføj note

# fokus på de fire kategorier

# Reshape data from wide to long format
alko_long <- pivot_longer(alko, cols = -Year, names_to = "Kategori", values_to = "Forbrug")

# Convert the Forbrug column to numeric
alko_long$Forbrug <- as.numeric(alko_long$Forbrug)

# Remove the unwanted category
alko_long <- alko_long[alko_long$Kategori != "ALKOHOLISKE DRIKKEVARER OG TOBAK", ]

# Define the categories to highlight
highlight_categories <- c(
  "Spiritus og likør",
  "Vin af druer",
  "Pilsnerøl, guldøl",
  "Øl med lavt alkoholindhold og alkoholfri øl"
)

# Add a new column to group categories into highlight or other
alko_long$color_group <- ifelse(
  alko_long$Kategori %in% highlight_categories,
  alko_long$Kategori,  # Keep the category name for highlighted ones
  "Andre"  # Group all others into "Andre"
)

# Define custom colors for highlighted categories and gray for others
custom_colors <- c(
  "Spiritus og likør" = "blue",
  "Vin af druer" = "purple",
  "Pilsnerøl, guldøl" = "green",
  "Øl med lavt alkoholindhold og alkoholfri øl" = "red",
  "Andre" = "gray"
)

# Create the plot
ggplot(alko_long, aes(x = Year, y = Forbrug, color = color_group, group = Kategori)) +
  geom_line(size = 0.5) +  # Line thickness
  geom_point(size = 0) + # Point size
  labs(
    title = "Udvikling i alkoholdforbrug over tid",
    x = "År",
    y = "Faste priser i kr. pr. husstand",
    color = "Kategori"
  ) +
  scale_color_manual(values = custom_colors) +  # Apply custom colors
  theme_minimal(base_size = 14) +  # Minimal theme with larger base text size
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),  # Centered title
    legend.position = "bottom",             # Legend at the bottom
    legend.title = element_text(size = 12), # Legend title size
    legend.text = element_text(size = 10)   # Legend text size
  )



#### 4.2 #### 

library(ggcorrplot)

# lav en dataframe med disse tre koloner fra alko dataframen 
kor_mat_plot <- alko[, c("Spiritus og likør", "Vin af druer", "Pilsnerøl, guldøl")]

# konvertere koloner til numerics
kor_mat_plot$`Spiritus og likør` <- as.numeric(kor_mat_plot$`Spiritus og likør`)
kor_mat_plot$`Vin af druer` <- as.numeric(kor_mat_plot$`Vin af druer`)
kor_mat_plot$`Pilsnerøl, guldøl` <- as.numeric(kor_mat_plot$`Pilsnerøl, guldøl`)

# Beregn korrelationsmatrix
kor_matrix <- cor(kor_mat_plot, use = "complete.obs")

# Vis korrelationsmatrix
print(kor_matrix)

# Plot korrelationmatrixen
ggcorrplot(kor_matrix, 
           method = "square",    # Shape of the plot (square tiles)
           type = "lower",       # Display only lower triangle of the matrix
           lab = TRUE,           # Add correlation coefficients to the plot
           lab_size = 5,         # Size of the labels
           colors = c("blue", "white", "red"), # Color gradient
           title = "Correlation Matrix",
           legend.title = "Corr") # Title of the legend

#### OPG 4.3 ####

# skal skrives i tekst