#!/bin/bash


#time java -Xmx8g -jar HIME_release.jar demo.txt 4 300  > tmp.log
#grep -i Motif tmp.log | cut -d' ' -f 2- > res.txt
#-i = not case sensitive
#-d = delimiter
#-f = number of fields 

java -jar HIME_release.jar BOW-JZ1-WB28_20191014_180100.txt 4 300  > tmp.log
select-string Motif tmp.log > reswav_wb28_4300.txt

plotVLMotif('18.08.2020_WB06_OCT_ACI.txt','resACI_wb06_432.txt')
plotVLMotif('12.08.2020_WB11_OCT.txt','res1.txt'

#SERF201603

java -jar HIME_release.jar "SERF/AI_TS_monthly/201603AcousticComplexitySERF.txt" 4 32  > tmp.log
select-string Motif tmp.log > "SERF/Results_chp2/resACI_201603_432.txt"

java -jar HIME_release.jar "SERF/AI_TS_monthly/201603EventsPerSecondSERF.txt" 4 32  > tmp.log
select-string Motif tmp.log > "SERF/Results_chp2/resEVN_201603_432.txt"

java -jar HIME_release.jar "SERF/AI_TS_monthly/201603TemporalEntropySERF.txt" 4 32  > tmp.log
select-string Motif tmp.log > "SERF/Results_chp2/resENT_201603_432.txt"

#SERF201604

java -jar HIME_release.jar "SERF/AI_TS_monthly/201604AcousticComplexitySERF.txt" 4 32  > tmp.log
select-string Motif tmp.log > "SERF/Results_chp2/resACI_201604_432.txt"

java -jar HIME_release.jar "SERF/AI_TS_monthly/201604EventsPerSecondSERF.txt" 4 32  > tmp.log
select-string Motif tmp.log > "SERF/Results_chp2/resEVN_201604_432.txt"

java -jar HIME_release.jar "SERF/AI_TS_monthly/201604TemporalEntropySERF.txt" 4 32  > tmp.log
select-string Motif tmp.log > "SERF/Results_chp2/resENT_201604_432.txt"

#SERF201605

java -jar HIME_release.jar "SERF/AI_TS_monthly/201605AcousticComplexitySERF.txt" 4 32  > tmp.log
select-string Motif tmp.log > "SERF/Results_chp2/resACI_201605_432.txt"

java -jar HIME_release.jar "SERF/AI_TS_monthly/201605EventsPerSecondSERF.txt" 4 32  > tmp.log
select-string Motif tmp.log > "SERF/Results_chp2/resEVN_201605_432.txt"

java -jar HIME_release.jar "SERF/AI_TS_monthly/201605TemporalEntropySERF.txt" 4 32  > tmp.log
select-string Motif tmp.log > "SERF/Results_chp2/resENT_201605_432.txt"

#SERF201606

java -jar HIME_release.jar "SERF/AI_TS_monthly/201606AcousticComplexitySERF.txt" 4 32  > tmp.log
select-string Motif tmp.log > "SERF/Results_chp2/resACI_201606_432.txt"

java -jar HIME_release.jar "SERF/AI_TS_monthly/201606EventsPerSecondSERF.txt" 4 32  > tmp.log
select-string Motif tmp.log > "SERF/Results_chp2/resEVN_201606_432.txt"

java -jar HIME_release.jar "SERF/AI_TS_monthly/201606TemporalEntropySERF.txt" 4 32  > tmp.log
select-string Motif tmp.log > "SERF/Results_chp2/resENT_201606_432.txt"

#SERF201607

java -jar HIME_release.jar "SERF/AI_TS_monthly/201607AcousticComplexitySERF.txt" 4 32  > tmp.log
select-string Motif tmp.log > "SERF/Results_chp2/resACI_201607_432.txt"

java -jar HIME_release.jar "SERF/AI_TS_monthly/201607EventsPerSecondSERF.txt" 4 32  > tmp.log
select-string Motif tmp.log > "SERF/Results_chp2/resEVN_201607_432.txt"

java -jar HIME_release.jar "SERF/AI_TS_monthly/201607TemporalEntropySERF.txt" 4 32  > tmp.log
select-string Motif tmp.log > "SERF/Results_chp2/resENT_201607_432.txt"

#SERF201608

java -jar HIME_release.jar "SERF/AI_TS_monthly/201608AcousticComplexitySERF.txt" 4 32  > tmp.log
select-string Motif tmp.log > "SERF/Results_chp2/resACI_201608_432.txt"

java -jar HIME_release.jar "SERF/AI_TS_monthly/201608EventsPerSecondSERF.txt" 4 32  > tmp.log
select-string Motif tmp.log > "SERF/Results_chp2/resEVN_201608_432.txt"

java -jar HIME_release.jar "SERF/AI_TS_monthly/201608TemporalEntropySERF.txt" 4 32  > tmp.log
select-string Motif tmp.log > "SERF/Results_chp2/resENT_201608_432.txt"

#SERF201609

java -jar HIME_release.jar "SERF/AI_TS_monthly/201609AcousticComplexitySERF.txt" 4 32  > tmp.log
select-string Motif tmp.log > "SERF/Results_chp2/resACI_201609_432.txt"

java -jar HIME_release.jar "SERF/AI_TS_monthly/201609EventsPerSecondSERF.txt" 4 32  > tmp.log
select-string Motif tmp.log > "SERF/Results_chp2/resEVN_201609_432.txt"

java -jar HIME_release.jar "SERF/AI_TS_monthly/201609TemporalEntropySERF.txt" 4 32  > tmp.log
select-string Motif tmp.log > "SERF/Results_chp2/resENT_201609_432.txt"

#SERF201610

java -jar HIME_release.jar "SERF/AI_TS_monthly/201610AcousticComplexitySERF.txt" 4 32  > tmp.log
select-string Motif tmp.log > "SERF/Results_chp2/resACI_201610_432.txt"

java -jar HIME_release.jar "SERF/AI_TS_monthly/201610EventsPerSecondSERF.txt" 4 32  > tmp.log
select-string Motif tmp.log > "SERF/Results_chp2/resEVN_201610_432.txt"

java -jar HIME_release.jar "SERF/AI_TS_monthly/201610TemporalEntropySERF.txt" 4 32  > tmp.log
select-string Motif tmp.log > "SERF/Results_chp2/resENT_201610_432.txt"

#SERF201611

java -jar HIME_release.jar "SERF/AI_TS_monthly/201611AcousticComplexitySERF.txt" 4 32  > tmp.log
select-string Motif tmp.log > "SERF/Results_chp2/resACI_201611_432.txt"

java -jar HIME_release.jar "SERF/AI_TS_monthly/201611EventsPerSecondSERF.txt" 4 32  > tmp.log
select-string Motif tmp.log > "SERF/Results_chp2/resEVN_201611_432.txt"

java -jar HIME_release.jar "SERF/AI_TS_monthly/201611TemporalEntropySERF.txt" 4 32  > tmp.log
select-string Motif tmp.log > "SERF/Results_chp2/resENT_201611_432.txt"

#SERF201612

java -jar HIME_release.jar "SERF/AI_TS_monthly/201612AcousticComplexitySERF.txt" 4 32  > tmp.log
select-string Motif tmp.log > "SERF/Results_chp2/resACI_201612_432.txt"

java -jar HIME_release.jar "SERF/AI_TS_monthly/201612EventsPerSecondSERF.txt" 4 32  > tmp.log
select-string Motif tmp.log > "SERF/Results_chp2/resEVN_201612_432.txt"

java -jar HIME_release.jar "SERF/AI_TS_monthly/201612TemporalEntropySERF.txt" 4 32  > tmp.log
select-string Motif tmp.log > "SERF/Results_chp2/resENT_201612_432.txt"

#SERF201701

java -jar HIME_release.jar "SERF/AI_TS_monthly/201701AcousticComplexitySERF.txt" 4 32  > tmp.log
select-string Motif tmp.log > "SERF/Results_chp2/resACI_201701_432.txt"

java -jar HIME_release.jar "SERF/AI_TS_monthly/201701EventsPerSecondSERF.txt" 4 32  > tmp.log
select-string Motif tmp.log > "SERF/Results_chp2/resEVN_201701_432.txt"

java -jar HIME_release.jar "SERF/AI_TS_monthly/201701TemporalEntropySERF.txt" 4 32  > tmp.log
select-string Motif tmp.log > "SERF/Results_chp2/resENT_201701_432.txt"

#SERF201702

java -jar HIME_release.jar "SERF/AI_TS_monthly/201702AcousticComplexitySERF.txt" 4 32  > tmp.log
select-string Motif tmp.log > "SERF/Results_chp2/resACI_201702_432.txt"

java -jar HIME_release.jar "SERF/AI_TS_monthly/201702EventsPerSecondSERF.txt" 4 32  > tmp.log
select-string Motif tmp.log > "SERF/Results_chp2/resEVN_201702_432.txt"

java -jar HIME_release.jar "SERF/AI_TS_monthly/201702TemporalEntropySERF.txt" 4 32  > tmp.log
select-string Motif tmp.log > "SERF/Results_chp2/resENT_201702_432.txt"

#SERF201703

java -jar HIME_release.jar "SERF/AI_TS_monthly/201703AcousticComplexitySERF.txt" 4 32  > tmp.log
select-string Motif tmp.log > "SERF/Results_chp2/resACI_201703_432.txt"

java -jar HIME_release.jar "SERF/AI_TS_monthly/201703EventsPerSecondSERF.txt" 4 32  > tmp.log
select-string Motif tmp.log > "SERF/Results_chp2/resEVN_201703_432.txt"

java -jar HIME_release.jar "SERF/AI_TS_monthly/201703TemporalEntropySERF.txt" 4 32  > tmp.log
select-string Motif tmp.log > "SERF/Results_chp2/resENT_201703_432.txt"