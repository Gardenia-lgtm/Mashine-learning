#决策树
#install.packages("C50")
library(C50)
tree_mod <- C5.0(x = iris[,c('Petal.Length','Petal.Width')],
                             y = iris$Species)
#若提示 C5.0 models require a factor outcome， 则转化下 
#例如：iris$Species <- as.factor(iris$Species) #转成factor 类型

tree_mod
summary(tree_mod) # 查看详细信息
plot(tree_mod) # 树模型可视化

#分类树构建预测
#install.packages("rpart.plot")
library(rpart)
library(rpart.plot)
tree_clf <- rpart(Species ~ Petal.Length + Petal.Width,data = iris) #波浪线把左边和右边链接起来形成公式formula，波浪线左边是因变量，右边是自变量
tree_clf
rpart.plot(tree_clf,extra = 3,digits = 4) #extra取 0-10表示显示细节不一样
predict(tree_clf,newdata = data.frame("Petal.Length" = 5,
                                      "Petal.Width" = 1.5),
        type = 'class')
predict(tree_clf,newdata = data.frame("Petal.Length" = 5,
                                      "Petal.Width" = 1.5))

#用自己的数据
library(C50)
print(ccx)
ccx$'类别'<- as.factor(ccx$'类别')
tree_mod <- C5.0(x = ccx[,c('长度','宽度','高度')],y = ccx$'类别')

tree_mod
summary(tree_mod) # 查看详细信息
plot(tree_mod) # 树模型可视化

#install.packages("rpart.plot")
library(rpart)
library(rpart.plot)
ccx$'长度'<- as.double(ccx$'长度')#将长度变量转为双精度浮点数
#波浪线把左边和右边链接起来形成公式formula，波浪线左边是因变量，右边是自变量
tree_clf <- rpart(类别~ 长度+宽度+高度,data = ccx) 
tree_clf
rpart.plot(tree_clf,extra = 3,digits = 4) #extra取 0-10表示显示细节不一样
predict(tree_clf,newdata = data.frame(高度 = 5, 长度 = 1.5,宽度 = 2),
                type = 'class')
predict(tree_clf,newdata = data.frame(高度 = 5, 长度 = 1.5,宽度 = 2))

#神经网络
#ccx$'类别'<- as.factor(ccx$'类别')
#ccx$'长度'<- as.double(ccx$'长度')
#以上数值类型一定要转换，不然容易死机
#install.packages("caret")

library(RSNNS)

#例题复现
# 对因子型的因变量进行哑变量处理
library(neuralnet) 
library(caret)
dmy1 <- dummyVars(~ ., data = iris, levelsOnly = TRUE)
train_dmy <- predict(dmy1, newdata = iris)   # 原来是 train
test_dmy  <- predict(dmy1, newdata = iris)   # 原来是 test
# 训练神经网络模型
set.seed(1234)
iris_neuralnet <- neuralnet(setosa + versicolor + virginica ~ 
                              Sepal.Length + Sepal.Width + Petal.Length + Petal.Width,
                            data = train_dmy,
                            hidden = 3)
iris_neuralnet$result.matrix
plot(iris_neuralnet)


#例题复现
library(RSNNS)
set.seed(12)
# 准备数据
# 将因变量进行哑变量处理
library(caret)
dmy <- dummyVars(~.,data = iris,levelsOnly = TRUE)
iris1 <- predict(dmy,newdata = iris)
# 将自变量进行标准化处理
iris1[,1:4] <- apply(iris[,1:4],2,scale)
# 将数据进行分区
ind <- createDataPartition(iris$Species,p = 0.8,list = FALSE) 
train <- iris1[ind,] # 训练集
test <- iris1[-ind,] # 测试集
# 使用mlp()函数，建立具有两个隐藏层，分别具有神经元数量为8，4的多层感知器网络
mlp.nnet <- mlp(train[,1:4],train[,5:7],size = c(8,4), learnFunc="Quickprop", 
                   learnFuncParams=c(0.1, 2.0, 0.0001, 0.1),maxit=100)

#利用上面建立的模型进行预测, 得到预测概率矩阵
pred_prob = predict(mlp.nnet,test[,1:4])
head(pred_prob,3)
# 然后，通过找到概率最大的那一列，得到其他可能的类别
pred_class <- unique(iris[-ind,]$Species)[apply(pred_prob,1,which.max)]
#生成混淆矩阵，观察预测精度 
table('actual' = iris[-ind,]$Species,
         'prediction'= pred_class)

#基于神经网络进行类别预测- neuralnet包
#用自己的数据
library(neuralnet) 
library(caret)
ccx$长度 <- as.double(ccx$长度)
ccx$类别 <- as.factor(ccx$类别)
# 导入和划分数据
set.seed(1234) # 设置随机种子
ind <- createDataPartition(ccx$类别,p = 0.7,list = FALSE) 
train <- ccx[ind,] # 训练集
test <- ccx[-ind,] # 测试集
# 对因子型的因变量进行哑变量处理
dmy1 <- dummyVars(~.,data = train,levelsOnly = TRUE)
#dmy <- dummyVars(~长度+宽度+高度+类别,data = ccx,levelsOnly = TRUE)
train_dmy <- predict(dmy1,newdata = train) #转成可以
test_dmy <- predict(dmy1,newdata = test)
# 训练神经网络模型
set.seed(1234)
#根据结果调整hidden ,threshold 两个参数
ccx_neuralnet <- neuralnet( A + B + C ~ 长度 + 宽度 + 高度 ,
                            data = train_dmy,hidden = c(4,3,2),
                            threshold = 0.005) # 构建模型,neuralnet()参数可调
ccx_neuralnet$result.matrix # 输出结果矩阵
plot(ccx_neuralnet) # 模型可视化

#基于神经网络进行类别预测- RSNNS包
#用自己的数据
library(RSNNS)
ccx$长度 <- as.double(ccx$长度)
ccx$类别 <- as.factor(ccx$类别)
set.seed(12)
# 将因变量进行哑变量处理
library(caret)
dmy <- dummyVars(~.,data = ccx,levelsOnly = TRUE)
ccx1 <- predict(dmy,newdata = ccx)
# 将自变量进行标准化处理
ccx1[,2:4] <- apply(ccx1[,2:4],2,scale)#注意自变量是哪几列（从一开始数，没有0列）
# 将数据进行分区
ind <- createDataPartition(ccx$类别,p = 0.8,list = FALSE) 
train <- ccx1[ind,] # 训练集
test <- ccx1[-ind,] # 测试集
# 使用mlp()函数，建立具有两个隐藏层，分别具有神经元数量为8，4的多层感知器网络
mlp.nnet <- mlp(train[,2:4],train[,6:8],size = c(8,4), learnFunc="Quickprop", 
                learnFuncParams=c(0.1, 2.0, 0.0001, 0.1),maxit=100)
#利用上面建立的模型进行预测, 得到预测概率矩阵
pred_prob = predict(mlp.nnet,test[,2:4])
head(pred_prob,3)
# 然后，通过找到概率最大的那一列，得到其他可能的类别
pred_class <- unique(ccx[-ind,]$类别)[apply(pred_prob,1,which.max)]
#生成混淆矩阵，观察预测精度 
table('actual' = ccx[-ind,]$类别,
      'prediction'= pred_class)


#关联规则
#install.packages("arulesViz")
library(arules)
data(Groceries)
#通过inspect的数查看Groceries数据集的前5次交易记录
inspect(Groceries[1:5])
grocery_rules <- apriori(data = Groceries, parameter=list(support=0.006,confidence=0.5))
inspect(grocery_rules[1:5])


#关联度
library(arules)
a_list=as.list(zxc)
names(a_list) <-paste("T",c(100,200,300,400,500,600,700,800,900))
trans1<- transactions(a_list)
rules<- apriori(trans1)#先运行到169，然后运行174，这时候是没有支持度、置信度的结果，
                         #再运行173、174，展示有置信度、支持度的结果
summary(rules) 
inspect(rules)   #未设定最小支持度（不低于0.2）和置信度
rules <- apriori(trans1, parameter=list(supp=0.2, conf=0.8, target="rules"))
inspect(rules)   #设定最小支持度和置信度





















