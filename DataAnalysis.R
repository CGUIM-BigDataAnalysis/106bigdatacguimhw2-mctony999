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

Student_country <- Foreign_inTW_ALL_C[,.(`學生人數` = sum(studentNumber)),by= `國家`] %>% arrange(desc(`學生人數`)) #外國學生全部資料
Student_country$國家[1] <- "中國"
Question3Data <- Student_country
#make people less 200 into others
Question2Data<- Student_country[1:67,]
a <- c("其他國家" ,4494)
Question2Data <- rbind(Question2Data,a)
Question2Data$學生人數 <- as.numeric(Question2Data$學生人數)

Student_countryTOP10 <- Student_country %>% head(10) #取前10名

Student_School <- Foreign_inTW_ALL_S[,.(`學生人數` = sum(studentNumber)),by= `學校名稱`] %>% arrange(desc(`學生人數`)) #本國學生全部資料
Student_SchoolTOP10 <- Student_School %>% head(10) #取前10名

countrycodelist <- fromJSON("https://gist.githubusercontent.com/jacobbubu/060d84c2bdf005d412db/raw/845c78f55e49fee89814bdc599355069f07b7ee6/countries.json")
countrycodelist <- countrycodelist[,c(2,5,7)]
countrycodelist[48,3] <- "中國"

#Question 4-6
Question4Data2 <- read.csv("~/GitHub/106bigdatacguimhw2-mctony999/Question4Data2.csv", stringsAsFactors=FALSE)
Question4Data2 <- data.table(Question4Data2)
Question4Data2$對方國別 <- ifelse(Question4Data2$對方國別 == "大陸地區","中國大陸",Question4Data2$對方國別)
TWStudent_country <- Question4Data2[,.(學生人數 = sum(總計)),by= 對方國別] %>%
  arrange(desc(學生人數))
TWStudent_country$對方國別[1] <- "中國"
Question6Data<- TWStudent_country
#question5
Question5Data <- TWStudent_country[1:29,]
a <- c("其他國家" ,1629)
Question5Data <- rbind(Question5Data,a)
Question5Data$學生人數 <- as.numeric(Question5Data$學生人數)

TWStudent_countryTOP10 <-TWStudent_country %>% head(10)
TWStudent_SchoolTOP10 <- Question4Data2[,.(學生人數 = sum(總計)),by= 學校名稱] %>%
  arrange(desc(學生人數)) %>% head(10)
#Qestion 7-8
Question7Data <- read_csv("a.csv")
Question7Data <- Question7Data[,1:3] %>% data.table()
Question7Top10 <- arrange(Question7Data,desc(總人數)) %>% head(10)




#For Rmarkdown
#question 1
knitr::kable(Student_countryTOP10)
knitr::kable(Student_SchoolTOP10)
unknownSchool <- Foreign_inTW_ALL_S[c(304,458,612),]
knitr::kable(unknownSchool)
#question2
ggplot(data = Question2Data)+
  geom_bar(aes(x = 國家,y = 學生人數),stat = "identity") + 
  theme(axis.text.x = element_text(angle = 90, size = 9, hjust = 1,vjust = 0.5))
#question 3
for (i in 1:nrow(Question3Data)){
  for (j in 1:nrow(countrycodelist)){
    if (Question3Data$國家[i] == countrycodelist$Taiwan[j]){
      Question3Data$ISO3[i] = countrycodelist$ISO3[j]
    }
  }
}
question3Plot <- joinCountryData2Map(Question3Data, joinCode="ISO3", nameJoinColumn="ISO3")
mapCountryData(question3Plot , nameColumnToPlot="學生人數", mapTitle="外國學生來台人數與國家面量圖", catMethod = "pretty", colourPalette = "heat",lwd = 0.01)
#question 4
knitr::kable(TWStudent_countryTOP10)
knitr::kable(TWStudent_SchoolTOP10)
#question 5
ggplot(data = Question5Data) +
  geom_bar(aes(x = 對方國別, y = 學生人數),stat = "identity") + 
  theme(axis.text.x = element_text(angle = 90, size = 9, hjust = 1,vjust = 0.5))
#question 6
for (i in 1:nrow(Question6Data)){
  for (j in 1:nrow(countrycodelist)){
    if (Question6Data$對方國別[i] == countrycodelist$Taiwan[j]){
      Question6Data$ISO3[i] = countrycodelist$ISO3[j]
    }
  }
}
question6Plot <- joinCountryData2Map(Question6Data, joinCode="ISO3", nameJoinColumn="ISO3")
mapCountryData(question6Plot , nameColumnToPlot="學生人數", mapTitle="台灣進修交流面量圖", catMethod = "pretty", colourPalette = "heat",lwd = 0.01)
#question 7 
knitr::kable(Question7Top10)
#question 8 
for (i in 1:nrow(Question7Data)){
  for (j in 1:nrow(countrycodelist)){
    if (Question7Data$國別[i] == countrycodelist$Taiwan[j]){
      Question7Data$ISO3[i] = countrycodelist$ISO3[j]
    }
  }
}
question8Plot <- joinCountryData2Map(Question7Data, joinCode="ISO3", nameJoinColumn="ISO3")
mapCountryData(question8Plot , nameColumnToPlot="總人數", mapTitle="台灣出國留學國家面量圖", catMethod = "pretty", colourPalette = "heat",lwd = 0.01)







