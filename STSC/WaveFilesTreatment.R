library(tidyverse)
library(tuneR)
library(seewave)

file1 <- read.table("C:/Work/STSC/BOW-JZ1-WB28_20191014_180000.Waveform 1.samples.txt", header = T) %>% 
  select(., ch1) %>% 
  write.table(., "C:/Work/STSC/BOW-JZ1-WB28_20191014_180000.Waveformch1.txt")


wav <- readWave("C:/Work/STSC/JZ001_WB28/BOW-JZ1-WB28_20191014_180000.wav", from = 0, to = 60, units = "seconds") %>%
  mono(., which = "left") %>%
  export(., filename = "C:/Work/STSC/BOW-JZ1-WB28_20191014_180100.txt", f = 22050, header = F)

f <- read.table("C:/Work/STSC/BOW-JZ1-WB28_20191014_180000.txt")


g <- read.table("C:/Work/STSC/BOW-JZ1-WB28_20191014_180000.Waveformch1.txt")
