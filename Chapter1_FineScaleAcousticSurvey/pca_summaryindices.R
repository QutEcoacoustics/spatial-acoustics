#PCA

getDataPath <- function (...) {
  return(file.path("C:/Users/n10393021/OneDrive - Queensland University of Technology/Documents/PhD/Project",  ...))
}

library(vegan)
library(tidyverse)
library(ggplot2)

df <- read.csv(getDataPath("Fieldwork_Bowra", "Oct2019", "WindRemoval_SummaryIndices_Channel1", "14.01.2019_glm_preparation.csv"))

#PCA for all the sites together

resultado <- rda(df_Highlycorrelatedremoved[,1:9], scale = F)
summary(resultado)
biplot(resultado)
ordihull(resultado, groups = df$PointData)
ls(resultado)
resultado$CA$u

pc <- resultado$CA$u

df_pc <- cbind(df_Highlycorrelatedremoved, pc)

df_wider <- gather(df_pc, c(PC1, PC2, PC3, PC4, PC5, PC6, PC7, PC8, PC9), key = "Index", value = "value")

screeplot(resultado, type = "l", npcs = 7, main = "Screeplot of the 9 PCs")
abline(h = 1, col="red", lty=5)
legend("topright", legend=c("Eigenvalue = 1"),
       col=c("red"), lty=5, cex=0.6)

indicesbytimeofday <- ggplot(df_wider, aes(x = time, y = value)) +
  geom_smooth(aes(colour = Index)) +
  theme_minimal() +
  scale_y_continuous(name = "PC1 and PC2") +
  ggtitle("PC1 and PC2 per time of day (in hours) - full dataset") 
indicesbytimeofday + scale_x_continuous(name = "Time (Beginning of recording in hours)", breaks = c(00000, 020000, 040000, 060000, 080000, 100000, 120000, 140000, 160000, 180000, 200000, 220000, 240000)) +
  ggsave(getDataPath("Fieldwork_Bowra","Aug2019_SummaryIndices_Prepared", "Figures", "28.01.2020_pc1and2bytimeofday.jpg"))


filtered_byTransect <- filter(df_wider, Transect == "WA")

indicesbytimeofday_pertransect <- ggplot(filtered_byTransect, aes(x = time, y = value)) +
  geom_smooth(aes(colour = Index)) +
  theme_minimal() +
  scale_y_continuous(name = "PC1 and PC2") +
  ggtitle("PC1 and PC2 per time of day (in hours) - full dataset") 


indicesbytimeofday_pertransect +
  scale_x_continuous(name = "Time (Beginning of recording in hours)", breaks = c(000000, 060000, 120000, 180000, 240000), labels = c("00", "06", "12", "18", "00")) +
  facet_wrap(Transectpoint ~ .) +
  ggsave(getDataPath("Fieldwork_Bowra", "Aug2019_SummaryIndices_Prepared", "Figures", "28.01.2020_pcabytimeofdaypersite_PCAWA.jpg"))

cor <- abs(cor(df_pc[91:97], use = "complete.obs", method = "spearman")) %>% 
  write.csv(getDataPath("Fieldwork_Bowra", "Oct2019", "WindRemoval_SummaryIndices_Channel1", "correlationmatrix_PCAwholedata.csv")) 


#PCA per site
unique(df$PointData)

#wb06
wb06 <- filter(df, df$PointData == "WB06")
resultado <- rda(wb06[,11:17], scale = F)
summary(resultado)
biplot(resultado)
ordihull(resultado, groups = wb06$beginning_rec_modified)
ls(resultado)
resultado$CA$u

pc <- resultado$CA$u

wb06_pc <- cbind(wb06, pc)

wb06_wider <- gather(wb06_pc, c(PC1, PC2, PC3, PC4, PC5, PC6, PC7), key = "Index", value = "value")

indicesbytimeofday <- ggplot(df_wider, aes(x = beginning_rec, y = value)) +
  geom_smooth(aes(colour = Index)) +
  theme_minimal() +
  scale_y_continuous(name = "PC1 and PC2") +
  ggtitle("PC1 and PC2 per time of day (in hours) - WB06") 
indicesbytimeofday + scale_x_continuous(name = "Time (Beginning of recording in hours)", breaks = c(00000, 020000, 040000, 060000, 080000, 100000, 120000, 140000, 160000, 180000, 200000, 220000, 240000)) #+
  #ggsave(getDataPath("Fieldwork_Bowra", "Oct2019", "Figures", "20.01.2020_pc1and2bytimeofday_WB06.jpg"))


screeplot(resultado, type = "l", npcs = 7, main = "Screeplot of the 7 PCs")
abline(h = 1, col="red", lty=5)
legend("topright", legend=c("Eigenvalue = 1"),
       col=c("red"), lty=5, cex=0.6)

#wb11
wb11 <- filter(df, df$PointData == "WB11")
resultado <- rda(wb11[,11:17], scale = F)
summary(resultado)
biplot(resultado)
ordihull(resultado, groups = wb11$beginning_rec_modified)
ls(resultado)
resultado$CA$u

pc <- resultado$CA$u
wb11_pc <- cbind(wb11, pc)

wb11_wider <- gather(wb11_pc, c(PC1, PC2, PC3, PC4, PC5, PC6, PC7), key = "Index", value = "value")

indicesbytimeofday <- ggplot(df_wider, aes(x = beginning_rec, y = value)) +
  geom_smooth(aes(colour = Index)) +
  theme_minimal() +
  scale_y_continuous(name = "PC1 and PC2") +
  ggtitle("PC1 and PC2 per time of day (in hours) - WB11") 
indicesbytimeofday + scale_x_continuous(name = "Time (Beginning of recording in hours)", breaks = c(00000, 020000, 040000, 060000, 080000, 100000, 120000, 140000, 160000, 180000, 200000, 220000, 240000)) #+
  #ggsave(getDataPath("Fieldwork_Bowra", "Oct2019", "Figures", "20.01.2020_pc1and2bytimeofday_WB11.jpg"))


screeplot(resultado, type = "l", npcs = 7, main = "Screeplot of the 7 PCs")
abline(h = 1, col="red", lty=5)
legend("topright", legend=c("Eigenvalue = 1"),
       col=c("red"), lty=5, cex=0.6)

#wb15
wb15 <- filter(df, df$PointData == "WB15")
resultado <- rda(wb15[,11:17], scale = F)
summary(resultado)
biplot(resultado)
ordihull(resultado, groups = wb15$beginning_rec_modified)
ls(resultado)
resultado$CA$u

pc <- resultado$CA$u
wb15_pc <- cbind(wb15, pc)

wb15_wider <- gather(wb15_pc, c(PC1, PC2, PC3, PC4, PC5, PC6, PC7), key = "Index", value = "value")

indicesbytimeofday <- ggplot(df_wider, aes(x = beginning_rec, y = value)) +
  geom_smooth(aes(colour = Index)) +
  theme_minimal() +
  scale_y_continuous(name = "PC1 and PC2") +
  ggtitle("PC1 and PC2 per time of day (in hours) - WB15") 
indicesbytimeofday + scale_x_continuous(name = "Time (Beginning of recording in hours)", breaks = c(00000, 020000, 040000, 060000, 080000, 100000, 120000, 140000, 160000, 180000, 200000, 220000, 240000)) #+
  #ggsave(getDataPath("Fieldwork_Bowra", "Oct2019", "Figures", "20.01.2020_pc1and2bytimeofday_WB15.jpg"))


screeplot(resultado, type = "l", npcs = 7, main = "Screeplot of the 7 PCs")
abline(h = 1, col="red", lty=5)
legend("topright", legend=c("Eigenvalue = 1"),
       col=c("red"), lty=5, cex=0.6)

#wb22
wb22 <- filter(df, df$PointData == "WB22")
resultado <- rda(wb22[,11:17], scale = F)
summary(resultado)
biplot(resultado)
ordihull(resultado, groups = wb22$beginning_rec_modified)
ls(resultado)
resultado$CA$u

pc <- resultado$CA$u
wb22_pc <- cbind(wb22, pc)

wb22_wider <- gather(wb22_pc, c(PC1, PC2, PC3, PC4, PC5, PC6, PC7), key = "Index", value = "value")

indicesbytimeofday <- ggplot(df_wider, aes(x = beginning_rec, y = value)) +
  geom_smooth(aes(colour = Index)) +
  theme_minimal() +
  scale_y_continuous(name = "PC1 and PC2") +
  ggtitle("PC1 and PC2 per time of day (in hours) - WB22") 
indicesbytimeofday + scale_x_continuous(name = "Time (Beginning of recording in hours)", breaks = c(00000, 020000, 040000, 060000, 080000, 100000, 120000, 140000, 160000, 180000, 200000, 220000, 240000)) #+
  #ggsave(getDataPath("Fieldwork_Bowra", "Oct2019", "Figures", "20.01.2020_pc1and2bytimeofday_WB22.jpg"))


screeplot(resultado, type = "l", npcs = 7, main = "Screeplot of the 7 PCs")
abline(h = 1, col="red", lty=5)
legend("topright", legend=c("Eigenvalue = 1"),
       col=c("red"), lty=5, cex=0.6)

#wb25
wb25 <- filter(df, df$PointData == "WB25")
resultado <- rda(wb25[,11:17], scale = F)
summary(resultado)
biplot(resultado)
ordihull(resultado, groups = wb25$beginning_rec_modified)
ls(resultado)
resultado$CA$u

pc <- resultado$CA$u
wb25_pc <- cbind(wb25, pc)

wb25_wider <- gather(wb25_pc, c(PC1, PC2, PC3, PC4, PC5, PC6, PC7), key = "Index", value = "value")

indicesbytimeofday <- ggplot(df_wider, aes(x = beginning_rec, y = value)) +
  geom_smooth(aes(colour = Index)) +
  theme_minimal() +
  scale_y_continuous(name = "PC1 and PC2") +
  ggtitle("PC1 and PC2 per time of day (in hours) - WB25") 
indicesbytimeofday + scale_x_continuous(name = "Time (Beginning of recording in hours)", breaks = c(00000, 020000, 040000, 060000, 080000, 100000, 120000, 140000, 160000, 180000, 200000, 220000, 240000)) #+
  #ggsave(getDataPath("Fieldwork_Bowra", "Oct2019", "Figures", "20.01.2020_pc1and2bytimeofday_WB25.jpg"))


screeplot(resultado, type = "l", npcs = 7, main = "Screeplot of the 7 PCs")
abline(h = 1, col="red", lty=5)
legend("topright", legend=c("Eigenvalue = 1"),
       col=c("red"), lty=5, cex=0.6)

#wb28
wb28 <- filter(df, df$PointData == "WB28")
resultado <- rda(wb28[,11:17], scale = F)
summary(resultado)
biplot(resultado)
ordihull(resultado, groups = wb28$beginning_rec_modified)
ls(resultado)
resultado$CA$u

pc <- resultado$CA$u
wb28_pc <- cbind(wb28, pc)

wb28_wider <- gather(wb28_pc, c(PC1, PC2, PC3, PC4, PC5, PC6, PC7), key = "Index", value = "value")

indicesbytimeofday <- ggplot(df_wider, aes(x = beginning_rec, y = value)) +
  geom_smooth(aes(colour = Index)) +
  theme_minimal() +
  scale_y_continuous(name = "PC1 and PC2") +
  ggtitle("PC1 and PC2 per time of day (in hours) - WB28") 
indicesbytimeofday + scale_x_continuous(name = "Time (Beginning of recording in hours)", breaks = c(00000, 020000, 040000, 060000, 080000, 100000, 120000, 140000, 160000, 180000, 200000, 220000, 240000)) #+
  #ggsave(getDataPath("Fieldwork_Bowra", "Oct2019", "Figures", "20.01.2020_pc1and2bytimeofday_WB28.jpg"))


screeplot(resultado, type = "l", npcs = 7, main = "Screeplot of the 7 PCs")
abline(h = 1, col="red", lty=5)
legend("topright", legend=c("Eigenvalue = 1"),
       col=c("red"), lty=5, cex=0.6)


#wb34
wb34 <- filter(df, df$PointData == "WB34")
resultado <- rda(wb34[,11:17], scale = F)
summary(resultado)
biplot(resultado)
ordihull(resultado, groups = wb34$beginning_rec_modified)
ls(resultado)
resultado$CA$u

pc <- resultado$CA$u
wb34_pc <- cbind(wb34, pc)

wb34_wider <- gather(wb34_pc, c(PC1, PC2, PC3, PC4, PC5, PC6, PC7), key = "Index", value = "value")

indicesbytimeofday <- ggplot(df_wider, aes(x = beginning_rec, y = value)) +
  geom_smooth(aes(colour = Index)) +
  theme_minimal() +
  scale_y_continuous(name = "PC1 and PC2") +
  ggtitle("PC1 and PC2 per time of day (in hours) - WB34") 
indicesbytimeofday + scale_x_continuous(name = "Time (Beginning of recording in hours)", breaks = c(00000, 020000, 040000, 060000, 080000, 100000, 120000, 140000, 160000, 180000, 200000, 220000, 240000)) #+
  #ggsave(getDataPath("Fieldwork_Bowra", "Oct2019", "Figures", "20.01.2020_pc1and2bytimeofday_WB34.jpg"))


screeplot(resultado, type = "l", npcs = 7, main = "Screeplot of the 7 PCs")
abline(h = 1, col="red", lty=5)
legend("topright", legend=c("Eigenvalue = 1"),
       col=c("red"), lty=5, cex=0.6)

#wb35
wb35 <- filter(df, df$PointData == "WB35")
resultado <- rda(wb35[,11:17], scale = F)
summary(resultado)
biplot(resultado)
ordihull(resultado, groups = wb35$beginning_rec_modified)
ls(resultado)
resultado$CA$u

pc <- resultado$CA$u
wb35_pc <- cbind(wb35, pc)

wb35_wider <- gather(wb35_pc, c(PC1, PC2, PC3, PC4, PC5, PC6, PC7), key = "Index", value = "value")

indicesbytimeofday <- ggplot(df_wider, aes(x = beginning_rec, y = value)) +
  geom_smooth(aes(colour = Index)) +
  theme_minimal() +
  scale_y_continuous(name = "PC1 and PC2") +
  ggtitle("PC1 and PC2 per time of day (in hours) - WB35") 
indicesbytimeofday + scale_x_continuous(name = "Time (Beginning of recording in hours)", breaks = c(00000, 020000, 040000, 060000, 080000, 100000, 120000, 140000, 160000, 180000, 200000, 220000, 240000)) #+
  #ggsave(getDataPath("Fieldwork_Bowra", "Oct2019", "Figures", "20.01.2020_pc1and2bytimeofday_WB35.jpg"))


screeplot(resultado, type = "l", npcs = 7, main = "Screeplot of the 7 PCs")
abline(h = 1, col="red", lty=5)
legend("topright", legend=c("Eigenvalue = 1"),
       col=c("red"), lty=5, cex=0.6)

#wb43
wb43 <- filter(df, df$PointData == "WB43")
resultado <- rda(wb43[,11:17], scale = F)
summary(resultado)
biplot(resultado)
ordihull(resultado, groups = wb43$beginning_rec_modified)
ls(resultado)
resultado$CA$u

pc <- resultado$CA$u
wb43_pc <- cbind(wb43, pc)

wb43_wider <- gather(wb43_pc, c(PC1, PC2, PC3, PC4, PC5, PC6, PC7), key = "Index", value = "value")

indicesbytimeofday <- ggplot(df_wider, aes(x = beginning_rec, y = value)) +
  geom_smooth(aes(colour = Index)) +
  theme_minimal() +
  scale_y_continuous(name = "PC1 and PC2") +
  ggtitle("PC1 and PC2 per time of day (in hours) - WB43") 
indicesbytimeofday + scale_x_continuous(name = "Time (Beginning of recording in hours)", breaks = c(00000, 020000, 040000, 060000, 080000, 100000, 120000, 140000, 160000, 180000, 200000, 220000, 240000)) #+
  #ggsave(getDataPath("Fieldwork_Bowra", "Oct2019", "Figures", "20.01.2020_pc1and2bytimeofday_WB43.jpg"))


screeplot(resultado, type = "l", npcs = 7, main = "Screeplot of the 7 PCs")
abline(h = 1, col="red", lty=5)
legend("topright", legend=c("Eigenvalue = 1"),
       col=c("red"), lty=5, cex=0.6)

#wb46
wb46 <- filter(df, df$PointData == "WB46")
resultado <- rda(wb46[,11:17], scale = F)
summary(resultado)
biplot(resultado)
ordihull(resultado, groups = wb46$beginning_rec_modified)
ls(resultado)
resultado$CA$u

pc <- resultado$CA$u
wb46_pc <- cbind(wb46, pc)

wb46_wider <- gather(wb46_pc, c(PC1, PC2, PC3, PC4, PC5, PC6, PC7), key = "Index", value = "value")

indicesbytimeofday <- ggplot(df_wider, aes(x = beginning_rec, y = value)) +
  geom_smooth(aes(colour = Index)) +
  theme_minimal() +
  scale_y_continuous(name = "PC1 and PC2") +
  ggtitle("PC1 and PC2 per time of day (in hours) - WB46") 
indicesbytimeofday + scale_x_continuous(name = "Time (Beginning of recording in hours)", breaks = c(00000, 020000, 040000, 060000, 080000, 100000, 120000, 140000, 160000, 180000, 200000, 220000, 240000)) #+
  #ggsave(getDataPath("Fieldwork_Bowra", "Oct2019", "Figures", "20.01.2020_pc1and2bytimeofday_WB46.jpg"))


screeplot(resultado, type = "l", npcs = 7, main = "Screeplot of the 7 PCs")
abline(h = 1, col="red", lty=5)
legend("topright", legend=c("Eigenvalue = 1"),
       col=c("red"), lty=5, cex=0.6)

#wb49
wb49 <- filter(df, df$PointData == "WB49")
resultado <- rda(wb49[,11:17], scale = F)
summary(resultado)
biplot(resultado)
ordihull(resultado, groups = wb49$beginning_rec_modified)
ls(resultado)
resultado$CA$u

pc <- resultado$CA$u
wb49_pc <- cbind(wb49, pc)

wb49_wider <- gather(wb49_pc, c(PC1, PC2, PC3, PC4, PC5, PC6, PC7), key = "Index", value = "value")

indicesbytimeofday <- ggplot(df_wider, aes(x = beginning_rec, y = value)) +
  geom_smooth(aes(colour = Index)) +
  theme_minimal() +
  scale_y_continuous(name = "PC1 and PC2") +
  ggtitle("PC1 and PC2 per time of day (in hours) - WB49") 
indicesbytimeofday + scale_x_continuous(name = "Time (Beginning of recording in hours)", breaks = c(00000, 020000, 040000, 060000, 080000, 100000, 120000, 140000, 160000, 180000, 200000, 220000, 240000)) #+
  #ggsave(getDataPath("Fieldwork_Bowra", "Oct2019", "Figures", "20.01.2020_pc1and2bytimeofday_WB49.jpg"))


screeplot(resultado, type = "l", npcs = 7, main = "Screeplot of the 7 PCs")
abline(h = 1, col="red", lty=5)
legend("topright", legend=c("Eigenvalue = 1"),
       col=c("red"), lty=5, cex=0.6)

#wb56
wb56 <- filter(df, df$PointData == "WB56")
resultado <- rda(wb56[,11:17], scale = F)
summary(resultado)
biplot(resultado)
ordihull(resultado, groups = wb56$beginning_rec_modified)
ls(resultado)
resultado$CA$u

pc <- resultado$CA$u
wb56_pc <- cbind(wb56, pc)

wb56_wider <- gather(wb56_pc, c(PC1, PC2, PC3, PC4, PC5, PC6, PC7), key = "Index", value = "value")

indicesbytimeofday <- ggplot(df_wider, aes(x = beginning_rec, y = value)) +
  geom_smooth(aes(colour = Index)) +
  theme_minimal() +
  scale_y_continuous(name = "PC1 and PC2") +
  ggtitle("PC1 and PC2 per time of day (in hours) - WB56") 
indicesbytimeofday + scale_x_continuous(name = "Time (Beginning of recording in hours)", breaks = c(00000, 020000, 040000, 060000, 080000, 100000, 120000, 140000, 160000, 180000, 200000, 220000, 240000)) #+
  #ggsave(getDataPath("Fieldwork_Bowra", "Oct2019", "Figures", "20.01.2020_pc1and2bytimeofday_WB56.jpg"))


screeplot(resultado, type = "l", npcs = 7, main = "Screeplot of the 7 PCs")
abline(h = 1, col="red", lty=5)
legend("topright", legend=c("Eigenvalue = 1"),
       col=c("red"), lty=5, cex=0.6)

dfs <- as.list(ls(pattern = "*_wider"))
dfs1 <- lapply(dfs, get)
df_complete <- do.call(rbind, dfs1)

dfs_pc <- as.list(ls(pattern = "*_pc"))
dfs_pc1 <- lapply(dfs_pc, get)
df_pc_complete <- do.call(rbind, dfs_pc1)


indicesbytimeofday <- ggplot(df_complete, aes(x = beginning_rec, y = value)) +
  geom_smooth(aes(colour = Index)) +
  theme_minimal() +
  scale_y_continuous(name = "PC's") +
  ggtitle("PCA per time of day (in hours)") 
indicesbytimeofday + scale_x_continuous(name = "Time (Beginning of recording in hours)", breaks = c(00000, 020000, 040000, 060000, 080000, 100000, 120000, 140000, 160000, 180000, 200000, 220000, 240000)) #+
#ggsave(getDataPath("Fieldwork_Bowra", "Oct2019", "Figures", "20.01.2020_pc1and2bytimeofday.jpg"))


indicesbytimeofday +
  scale_x_continuous(name = "Time (Beginning of recording in hours)", breaks = c(000000, 060000, 120000, 180000, 240000), labels = c("00", "06", "12", "18", "00")) +
  facet_wrap(PointData ~ .)

#Spearman rank correlation of PC1 and PC2

library(data.table)
df_PC1 <- select(df_complete, PointData, Index, value, X) %>%
  filter(Index == "PC2") %>%
  spread(., key = "PointData", value = "value", fill = 0)
  
cor <- abs(cor(df_PC1[3:14], use = "complete.obs", method = "spearman")) %>% 
  write.csv(getDataPath("Fieldwork_Bowra", "Oct2019", "WindRemoval_SummaryIndices_Channel1", "correlationmatrix_PC2persite.csv"))

#correlation matrix between normalised indices
cor <- abs(cor(df_pc_complete[91:97], use = "complete.obs", method = "spearman")) %>% 
write.csv(getDataPath("Fieldwork_Bowra", "Oct2019", "WindRemoval_SummaryIndices_Channel1", "correlationmatrix_PCAPersite.csv")) 
