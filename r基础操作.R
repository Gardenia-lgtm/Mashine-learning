# 创建数据框
Class1<-data.frame(
  姓名 = c("张青松", "王宇翔", "田思雨", "徐丽娜"),
  统计学 = c(68, 85, 74, 88),
  数学 = c(85, 91, 74, 100),
  营销学 = c(84, 63, 61, 49),
  管理学 = c(89, 76, 80, 71),
  会计学 = c(86, 66, 69, 66)
)

# 保存为RData文件（路径D:/Dataset）
save(Class1, file = "D:/Dataset/Class1.RData")

#先创建TXT文件，用制表符隔开（先存在Excel里，再复制到TXT里面）
read.table("D:/Dataset/成绩.txt", header = TRUE, fileEncoding = "GBK")

write.table(Class1, file = "D:/Dataset/Class1.txt", sep = "\t", row.names = FALSE)

# 安装（首次使用需执行）
install.packages("readxl")
# 加载包
library(readxl)
# 读取文件
read_excel("D:/Dataset/Class2.xlsx")
#第七题
# 加载Class1.RData文件（会自动加载数据框Class1）
load("D:/Dataset/Class1.RData")
# 移除数据框Class1
rm(Class1)

library(readxl)
data<-read_excel("D:/Dataset/Class2.xlsx")
stats_score<-data$"统计学"

#第八题
Class1<-Class1[order(-Class1$"管理学",na.last=TRUE),]


#第九题
set.seed(1234)
x <- seq(1,10)
# 1）无放回随机抽样
a <- sample(x, 7, replace = FALSE)
# 2）sort函数和order函数排序
a
sort(a)
order(a)
## 区别：sort函数直接对向量元素排序并返回排序后的向量；
#order函数返回排序后的元素在原向量中的位置索引


#第十题
y<-c(1,3,NA,7,9)
is.na(y)
y[is.na(y)]<-mean(y,na.rm = TRUE)

#十一题
data <- c(1,2,3,5,7,8,10,14,15,18,20)
# 分箱（分成3个箱子，每个箱子右边为闭区间，保留2位有效数）



cut(data, breaks = 3, right = TRUE, dig.lab = 2, include.lowest = FALSE,ordered_result = TRUE)
#cut(x, breaks, labels = NULL,include.lowest = FALSE, 
#right = TRUE, dig.lab = 3,ordered_result = FALSE, ...)
#参数x是数据向量，参数breaks是划分区间，参数labels是否给不同区间指定标签，
#参数include.lowest是否包含最小值，参数right指定闭区间方向，
#dig.lab 表示几位有效数字，ordered_result 表示是否给出大小排序。

#十二题
library(dplyr)
# 哑变量处理：高于等于85分为1，低于为0，结果存入Class.new
Class.new <- Class1 %>%
  mutate(
    across(统计学:会计学, ~ ifelse(. >= 85, 1, 0))
  )
# 查看处理后的结果
print(Class.new)
#十二题
Class.new <- Class1[c('姓名')] #去掉c也可以
Class.new <- Class1[1]

Class.new$统计学<-ifelse(Class1$统计学 >85, 1, 0)
Class.new$数学<-ifelse(Class1$数学 > 85, 1, 0)
Class.new$营销学<-ifelse(Class1$营销学 > 85, 1, 0)
Class.new$管理学<-ifelse(Class1$管理学 >85, 1, 0)
Class.new$会计学<-ifelse(Class1$会计学 > 85, 1, 0)

#13题


  year <- c(2022, 2022, 2022, 2022,2022,2022)
  month <- c(10, 10, 11, 11,12,12)
  day <- c(10, 12, 14, 16,NA,20)
  flight.no <- c(221010, 221012, 221014, 221016,221018,221020)

flights<-data.frame(year=year,month=month,day=day,flight.no=flight.no )
tibble(flights)
fights$flight.no

#14
library(dplyr)
filter(flights,month==11|month==12)

filter(flights,month==10&day==10)

filter(flights,month>=11&is.na(day)|day>=15)

arrange(flights, desc(year), desc(month), desc(day))

#15
set.seed(1234)
df<-data.frame(x1=sample(1:3,8,replace =T ),x2=sample(letters[1:3],8,replace =T))
df
a<-unique(df)

df<-transform(df,x3 = ifelse(x2=="b",8,0))   

#16              
iris[1:10,c(1,3,4,5)]        
iris[iris$Sepal.Length>4.5 & iris$Species=='setosa',]

#17
r1<-matrix(1:6,nrow=2)
r2<-matrix(7:12,nrow=2)
cbind(r1,r2)# 列合并


#20.绘图

x<- c(50,60,62,63,65,68,73,74,75,77,80,88,95)
y<- c(1,2,3,3,2,7,6,8,10,5,3,1,1)
plot(x,y, main="成绩人数散点图",sub="副标题",
        col="black",col.main="red",col.sub="blue",
        col.axis="black",col.lab="purple",font.main=3,cex.main=1.5,
        xlab="X轴",ylab="Y轴")
#plot(x轴,y轴, main=list("成绩人数散点图",col="red",font=3,cex=1.5)#list()函数针对主标题进行操作
#添加图例
legend("topright",legend="正态分布", pch =15,col="blue")
text(70,8,pos=2,labels="最大值",offset=0.2) 

#
points(71,8,pch =21,bg="yellow",cex=2)
 abline(h=mean(y),v=mean(x),col='green',lwd=2,lty=3)

 # 绘制拟合直线
 abline(lm(y[1:8]~x[1:8]),col='black')







#17
x<-c(-pi:pi)
type <- c('sin','cos','tan','abs','log','exp')
par(mfrow=c(2,3))

plot(x,sin(x),main=paste("函数",type[1]))
plot(x,cos(x),main=paste("函数",type[2]))
plot(x,tan(x),main=paste("函数",type[3]))
plot(x,log(x),main=paste("函数",type[4]))
plot(x,abs(x),main=paste("函数",type[5]))
plot(x,exp(x),main=paste("函数",type[6]))
rm(x)


#17循环结构（未完成）
x<-c(-pi:pi)
type <- c('sin','cos','tan','abs','log','exp')
par(mfrow=c(2,3))

for(i in 1:6){
  
plot(x,sin(x),main=paste("函数",type[i]))
}

#18




#主成分分析6.1
library(readxl)
setwd("F:/Users/李亚兰/Downloads/主成分+因子分析+R语言/data")
d6.1<- read_excel("eg6.1.xlsx")
R=round(cor(d6.1),3)
symnum(cor(d6.1,use="complete.obs"))
PCA6.1=princomp(d6.1,cor=T)
PCA6.1
summary(PCA6.1,loadings=T)
round(predict(PCA6.1),3)
plot(PCA6.1,type="lines")
load=loadings(PCA6.1)
plot(load[,1:2],xlim=c(-0.6,1))
rnames=c("数学","物理","化学","语文","历史","英语")
text(load[,1],load[,2],labels=rnames,adj=c(-0.3,1.5))
abline(h=0,v=0)
biplot(PCA6.1,scale=0.5)

data <- matrix(c(
  40.4, 24.7, 7.2, 6.1, 8.3, 8.7, 2.442, 20,
  25, 12.7, 11.2, 11, 12.9, 20.2, 3.542, 9.1,
  13.2, 3.3, 3.9, 4.3, 4.4, 5.5, 0.578, 3.6,
  22.3, 6.7, 5.6, 3.7, 6, 7.4, 0.176, 7.3,
  34.3, 11.8, 7.1, 7.1, 8, 8.9, 1.726, 27.5,
  35.6, 12.5, 16.4, 16.7, 22.8, 29.3, 3.017, 26.6,
  22, 7.8, 9.9, 10.2, 12.6, 17.6, 0.847, 10.6,
  48.4, 13.4, 10.9, 9.9, 10.9, 13.9, 1.772, 17.8,
  40.6, 19.1, 19.8, 19, 29.7, 39.6, 2.449, 35.8,
  24.8, 8, 9.8, 8.9, 11.9, 16.2, 0.789, 13.7,
  12.5, 9.7, 4.2, 4.2, 4.6, 6.5, 0.874, 3.9,
  1.8, 0.6, 0.7, 0.7, 0.8, 1.1, 0.056, 1,
  32.3, 13.9, 9.4, 8.3, 9.8, 13.3, 2.126, 17.1,
  38.5, 9.1, 11.3, 9.5, 12.2, 16.4, 1.327, 11.6,
  26.2, 10.1, 5.6, 15.6, 7.7, 30.1, 0.126, 25.9
),nrow=15,byrow=TRUE)
R=round(cor(data),3)
symnum(cor(data,use="complete.obs"))
PCAdata=princomp(data,cor=T)
summary(PCAdata,loadings=T)
round(predict(PCAdata),3)
plot(PCAdata,type="lines")
load=loadings(PCAdata)
plot(load[,1:2],xlim=c(-0.6,1))




