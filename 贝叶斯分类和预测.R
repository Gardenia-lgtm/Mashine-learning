#贝叶斯实现分类和预测
str(iris)
#将iris数据集划分为两个子集: 其中70%的数据用于训练，剩下的30%做测试
ind<- sample(2,nrow(iris),replace=TRUE, prob=c(0.7,0.3))
trainData<- iris[ind==1,]
testData<- iris[ind==2,]
if (!require(klaR)) {
  install.packages("klaR")
}
library(klaR)
#myformula指定了Species为目标变量，其余的所有变量为自变量
myformula  <- Species~Sepal.Length+ Sepal.Width+ Petal.Length + Petal.Width
#用klaR包中的的数Naivebayes用来建分类模型
NB_model  <- NaiveBayes(myformula, data=trainData)

#预测结果
train_predict = predict(NB_model) #训练数据集
test_predict = predict(NB_model, newdata=testData)  #测试数据集

#输出训练数据的分类结果
train_predictdata = cbind(trainData, predictedclass = train_predict$class)
#输出训炼数据的混淆矩阵
train_confusion = table(actual = trainData$Species, predictedclass = train_predict$class)
#输出测试数据的分类结果
test_predictdata = cbind(testData, predictedclass = test_predict$class)
#输出测试数据的混淆矩阵
test_confusion = table(actual = testData$Species, predictedclass = test_predict$class)

test_confusion  











