library(readr)
library(jsonlite)
library(dplyr)
library(httr)
library(RCurl)
library(knitr)
library(rmarkdown)
library(rvest)
library(ggplot2)
library(choroplethr)
library(choroplethrMaps)
library(ggmap)
library(data.table)
library(plotly)
library(rworldmap)
#Qustion1-3 Country
Foreign_inTW_103_C <- read_csv("http://stats.moe.gov.tw/files/detail/103/103_ab103_C.csv")
Foreign_inTW_104_C <- read_csv("http://stats.moe.gov.tw/files/detail/104/104_ab104_C.csv")
Foreign_inTW_105_C <- read_csv("http://stats.moe.gov.tw/files/detail/105/105_ab105_C.csv")
Foreign_inTW_106_C <- read_csv("http://stats.moe.gov.tw/files/detail/106/106_ab105_C.csv")
col_Name_C <- c("州","國家","學位生-正式修讀學位外國生","學位生-僑生(含港澳)","學位生-正式修讀學位陸生",
              "非學位生-外國交換生","非學位生-外國短期研習及個人選讀","非學位生-大專附設華語文中心學生","非學位生-大陸研修生","非學位生-海青班","境外專班")
names(Foreign_inTW_103_C) <- col_Name_C 
names(Foreign_inTW_104_C) <- col_Name_C 
names(Foreign_inTW_105_C) <- col_Name_C 
names(Foreign_inTW_106_C) <- col_Name_C 
Foreign_inTW_ALL_C <- rbind(Foreign_inTW_103_C,Foreign_inTW_104_C) %>% rbind(Foreign_inTW_105_C,Foreign_inTW_106_C)
#Question1-3 School
Foreign_inTW_103_S <- read_csv("http://stats.moe.gov.tw/files/detail/103/103_ab103_S.csv")
Foreign_inTW_104_S <- read_csv("http://stats.moe.gov.tw/files/detail/104/104_ab104_S.csv")
Foreign_inTW_105_S <- read_csv("http://stats.moe.gov.tw/files/detail/105/105_ab105_S.csv")
Foreign_inTW_106_S <- read_csv("http://stats.moe.gov.tw/files/detail/106/106_ab105_S.csv")
col_Name_S <- c("學校類型","學校代碼","學校名稱","學位生-正式修讀學位外國生","學位生-僑生(含港澳)","學位生-正式修讀學位陸生",
                "非學位生-外國交換生","非學位生-外國短期研習及個人選讀","非學位生-大專附設華語文中心學生","非學位生-大陸研修生","非學位生-海青班","境外專班")
names(Foreign_inTW_103_S) <- col_Name_S
names(Foreign_inTW_104_S) <- col_Name_S
names(Foreign_inTW_105_S) <- col_Name_S
names(Foreign_inTW_106_S) <- col_Name_S
Foreign_inTW_ALL_S <- rbind(Foreign_inTW_103_S,Foreign_inTW_104_S) %>% rbind(Foreign_inTW_105_S,Foreign_inTW_106_S)
Foreign_inTW_ALL_S$`非學位生-大陸研修生` <- ifelse(Foreign_inTW_ALL_S$`非學位生-大陸研修生`== "…",0,Foreign_inTW_ALL_S$`非學位生-大陸研修生`) %>% as.numeric()
#question 1-3
Foreign_inTW_ALL_C$studentNumber <- rowSums(Foreign_inTW_ALL_C[,3:11]) 
Foreign_inTW_ALL_S$studentNumber <- rowSums(Foreign_inTW_ALL_S[,4:12])
Foreign_inTW_ALL_C <- data.table(Foreign_inTW_ALL_C)
Foreign_inTW_ALL_S <- data.table(Foreign_inTW_ALL_S)
Student_countryTOP10 <- Foreign_inTW_ALL_C[,.(`學生人數` = sum(studentNumber)),by= `國家`] %>%
  arrange(desc(`學生人數`)) %>% head(10)
Student_SchoolTOP10 <- Foreign_inTW_ALL_S[,.("學生人數" = sum(studentNumber)),by= "學校名稱"] %>%
  arrange(desc("學生人數")) %>% head(10)

#Question 4-6
Question4Data2 <- read.csv("~/GitHub/106bigdatacguimhw2-mctony999/Question4Data2.csv", stringsAsFactors=FALSE)
Question4Data2 <- data.table(Question4Data2)
Question4Data2$對方國別 <- ifelse(Question4Data2$對方國別 == "大陸地區","中國大陸",Question4Data2$對方國別)
TWStudent_countryTOP10 <- Question4Data2[,.(學生人數 = sum(總計)),by= 對方國別] %>%
  arrange(desc(學生人數)) %>% head(10)
TWStudent_SchoolTOP10 <- Question4Data2[,.(學生人數 = sum(總計)),by= 學校名稱] %>%
  arrange(desc(學生人數)) %>% head(10)
#Qestion 7-8
Question7Data <- read_csv("a.csv")
Question7Data <- Question7Data[,1:3] %>% data.table()
Question7Top10 <- arrange(Question7Data,desc(總人數)) %>% head(10)





#question 1
knitr::kable(Student_countryTOP10)
knitr::kable(Student_SchoolTOP10)
unknownSchool <- Foreign_inTW_ALL_S[c(304,458,612),]
knitr::kable(unknownSchool)
#question2
question2 <- Student_countryTOP10
ggplot()+
  geom_bar(data = question2,aes(x = 國家,y = 學生人數),stat = "identity") 
 #+theme(axis.text.x = element_text(angle = 90, size = 0.1))
#question 3
Student_countryTOP10$country <- c("CHN","MYS","HKG","JPN","VNM","MAC","IDN","KOR","USA","THA")
question3Plot <- joinCountryData2Map(Student_countryTOP10, joinCode="ISO3", nameJoinColumn="country")
mapCountryData(question3Plot , nameColumnToPlot="學生人數", mapTitle="外國學生人數與國家面量圖", catMethod = "pretty", colourPalette = "heat",lwd = 0.01)
#question 4
knitr::kable(TWStudent_countryTOP10)
knitr::kable(TWStudent_SchoolTOP10)
#question 5
ggplot()+geom_bar(data = TWStudent_countryTOP10,aes(x = 對方國別, y = 學生人數),stat = "identity")
#question 6
TWStudent_countryTOP10$country <- c("CHN","JPN","USA","KOR","DEU","FRA","GBR","CAN","ESP","HKG")
question6Plot <- joinCountryData2Map(TWStudent_countryTOP10, joinCode="ISO3", nameJoinColumn="country")
mapCountryData(question6Plot , nameColumnToPlot="學生人數", mapTitle="台灣進修交流面量圖", catMethod = "pretty", colourPalette = "heat",lwd = 0.01)
#question 7 
knitr::kable(Question7Top10)
#question 8 
Question7Top10$country <- c("USA","AUS","JPN","CAN","GBR","DEU","NZL","POL","MYS","AUT")
question8Plot <- joinCountryData2Map(Question7Top10, joinCode="ISO3", nameJoinColumn="country")
mapCountryData(question8Plot , nameColumnToPlot="總人數", mapTitle="台灣出國留學國家面量圖", catMethod = "pretty", colourPalette = "heat",lwd = 0.01)


#self





