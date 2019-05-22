install.packages("readr")
install.packages("rvest")
install.packages("dplyr")
library(readr)
library(rvest)
library(dplyr)
X103 <- read_csv("C:/Users/user/Desktop/103.csv")
X104 <- read_csv("C:/Users/user/Desktop/104.csv")
X105 <- read_csv("C:/Users/user/Desktop/105.csv")
X106 <- read_csv("C:/Users/user/Desktop/106.csv")
for(n in 1:14){
  X103[[n]]<-gsub("—|_|…|、", " ", X103[[n]])
  X104[[n]]<-gsub("—|_|…|、", " ", X104[[n]])
  X105[[n]]<-gsub("—|_|…|、", " ", X105[[n]])
  X106[[n]]<-gsub("—|_|…|、", " ", X106[[n]])
}
X103$"大學-薪資"<-as.numeric((X103$"大學-薪資"))
X106$"大學-薪資"<-as.numeric((X106$"大學-薪資"))
X103106<-inner_join(X103,X106,by="大職業別")
X103106$"薪資比較"<-(X106$"大學-薪資")/(X103$"大學-薪資")
X103106compare1<-filter(X103106,X103106$"薪資比較">1)
X103106compare1<-arrange(X103106compare1,desc(X103106compare1$"薪資比較"))
X103106compare2<-filter(X103106,X103106$"薪資比較">1.05)
X103106compare2<-arrange(X103106compare2,desc(X103106compare2$"薪資比較"))
Job<-strsplit (X103106filter$"大職業別","-")
mainjob<-c()
for(n in 1:58){
  mainjob<-c(mainjob,Job[[n]][1])
}
table(mainjob)

X103$"經常性薪資-女/男"<-as.numeric((X103$"經常性薪資-女/男"))
X104$"經常性薪資-女/男"<-as.numeric((X104$"經常性薪資-女/男"))
X105$"經常性薪資-女/男"<-as.numeric((X105$"經常性薪資-女/男"))
X106$"經常性薪資-女/男"<-as.numeric((X106$"經常性薪資-女/男"))
all1<-rbind(head(X103mf,10),
            head(X104mf,10),
            head(X105mf,10),
            head(X106mf,10))


X103mf1<-filter(X103,X103$"經常性薪資-女/男">100)
X104mf1<-filter(X104,X104$"經常性薪資-女/男">100)
X105mf1<-filter(X105,X105$"經常性薪資-女/男">100)
X106mf1<-filter(X106,X106$"經常性薪資-女/男">100)
all2<-rbind(head(X103mf1,10),
            head(X104mf1,10),
            head(X105mf1,10),
            head(X106mf1,10))
all2<-arrange(all2,desc(all2$"經常性薪資-女/男"))

X106$"研究所及以上-薪資"<-as.numeric((X106$"研究所及以上-薪資"))
X106$"大學-薪資"<-as.numeric((X106$"大學-薪資"))
X106$Cost<-(X106$"研究所及以上-薪資"/X106$"大學-薪資")
Cost<-head(arrange(X106,desc(X106$Cost)),10)

myjob<-X106[c(71,113,127,129),c(2,11,13)]
