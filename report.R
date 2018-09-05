library(readxl)
library(rvest)
library(tmcn)
library(dplyr)
library(ggplot2)
library(Rmisc)
number<-1:6
URL<-paste0("https://www.com.tw/exam/groupid-2110",number,".html")
for(x in c(1:6))
{
  listfilename<-paste0(100+x,"學年度前一百個志願.csv")
  Output<-function(URL)
  {
    
    html<-read_html(URL)
    list<-html_nodes(html,"div a")%>%html_text()%>%data.frame()
    list<-list[-c(1:209),]%>%data.frame
    list<-slice(list,1:100)
    scores<-html_nodes(html,"td div")%>%html_text()%>%data.frame()
    scores1<-as.data.frame(scores)
    y<-c(1:100)
    scores2<-slice(scores1,7*y+13)
    schoolscores<-cbind(list,scores2)
    write.csv(schoolscores, listfilename)
  }
  
  Output(URL[x])
  
}



file1<-read.csv("101學年度前一百個志願.csv")
file2<-read.csv("102學年度前一百個志願.csv")
file3<-read.csv("103學年度前一百個志願.csv")
file4<-read.csv("104學年度前一百個志願.csv")
file5<-read.csv("105學年度前一百個志願.csv")
file6<-read.csv("106學年度前一百個志願.csv")
print("以下為101年度到106年度二類組指考前一百個志願")
file1
file2
file3
file4
file5
file6
#擷取台大資工各個年度的志願排名
名次101<-grep("資訊工程",file1$.)[1]
名次102<-grep("資訊工程",file2$.)[1]
名次103<-grep("資訊工程",file3$.)[1]
名次104<-grep("資訊工程",file4$.)[1]
名次105<-grep("資訊工程",file5$.)[1]
名次106<-grep("資訊工程",file6$.)[1]

名次<-c(名次101,名次102,名次103,名次104,名次105,名次106)
年度<-c(101:106)
年度資工系志願名次<-data.frame(Year=年度,Ranking=名次)

  {
    年度資工系志願名次2<-1/年度資工系志願名次$Ranking
    年度資工系志願名次2<-data.frame(Year=年度,Ranking_Reciprocal=年度資工系志願名次2)
  }


ggplot(年度資工系志願名次,aes(x=Year,y=Ranking))+geom_point()+labs(title = "NTU-CS Ranking Through Time")
g1<-ggplot(年度資工系志願名次2,aes(x=Year,y=Ranking_Reciprocal))+geom_point()+labs(title = "NTU-CS Ranking Through Time")
#畫散布圖：台大資工系的年度與志願排名的關係：我們可以看到台大資工的排名隨時間又往前進的趨勢，說明高中生也明白大CS時代的來臨!!

Grad101<-read_excel("報考人數-101碩.xls")
Grad102<-read_excel("報考人數-102碩.xls")
Grad103<-read_excel("報考人數-103碩.xls")
Grad104<-read_excel("報考人數-104碩.xls")
Grad105<-read_excel("報考人數-105碩.xls")
Grad106<-read_excel("報考人數-106碩.xls")

報考人數1<-slice(Grad101,grep("資訊工程學系",Grad101$系所別))[4]
報考人數2<-slice(Grad102,grep("資訊工程學系",Grad102$系所別))[4]
報考人數3<-slice(Grad103,grep("資訊工程學系",Grad103$X__1))[4]
報考人數4<-slice(Grad104,grep("資訊工程學系",Grad104$X__1))[4]
報考人數5<-slice(Grad105,grep("資訊工程學系",Grad105$X__1))[4]
報考人數6<-slice(Grad106,grep("資訊工程學系",Grad106$X__1))[4]
報考人數<-c(報考人數1,報考人數2,報考人數3,報考人數4,報考人數5,報考人數6)
人數<-data.frame(報考人數)%>%t()
報考人數與年度<-data.frame(Year=c(101:106),People=人數)
g2<-ggplot(報考人數與年度,aes(x=Year,y=People))+geom_point()+labs(title = "NTU-CS People Through Time")
合併<-left_join(年度資工系志願名次2,報考人數與年度,by="Year")

result<-multiplot(g1, g2,cols=2)
