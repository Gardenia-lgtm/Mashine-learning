#聚类
#install.packages(c('factoextra','dendextend'))

library(factoextra)
hc <- hclust(dist(scale(USArrests),method = "euclidean"),
                         method = "ward.D2")  # 层次聚类算法
 # 自定义颜色
 fviz_dend(hc, k = 4, 
                     k_colors = c("skyblue", "violetred3", "springgreen2", "yellow4"),
                     color_labels_by_k = TRUE, 
                     rect = TRUE,
                     rect_border = "cluster",   #不同填充颜色，换成gray就是灰色了
          rect_fill = TRUE)
fviz_dend(hc, k = 4,
         k_colors = "uchicago", type = "circular")    # 画第二幅图
#install.packages("igraph") #画出第三幅图
fviz_dend(hc, k = 4,
         k_colors = "uchicago", type = "phylogenic")




#导入自己的数据集
churn$churn=as.factor(churn$churn)
hc <- hclust(dist(scale(churn[,-10]),method = "euclidean"),
             method = "ward.D2")  # 层次聚类算法
# 自定义颜色
fviz_dend(hc, k = 4, 
          k_colors = c("skyblue", "violetred3", "springgreen2", "yellow4"),
          color_labels_by_k = TRUE, 
          rect = TRUE,
          rect_border = "cluster",   #不同填充颜色，换成gray就是灰色了
          rect_fill = TRUE)
fviz_dend(hc, k = 4,
          k_colors = "uchicago", type = "circular")    # 画第二幅图
#install.packages("igraph") #画出第三幅图
fviz_dend(hc, k = 4,
          k_colors = "uchicago", type = "phylogenic")


#k均值
library(igraph)
df=churn[,1:8] # 选取前四列
km.res <- kmeans(df, 3, nstart = 25) 
names(km.res)
 # fviz_nbclust寻找最佳的簇k值
fviz_cluster(km.res,churn[,1:8])



#密度聚类
#install.packages('fpc')
library(factoextra)
library(fpc)
data("multishapes")
 # 数据可视化
library(ggplot2)
ggplot(data = multishapes,
      aes(x = x,y = y,shape = factor(shape),color = factor(shape)))+
geom_point()
#    
set.seed(123)
library(factoextra)
km.res <- kmeans(multishapes[,1:2], 5, nstart = 25)
fviz_cluster(km.res, multishapes[,1:2],  geom = "point", 
                        ellipse= FALSE, show.clust.cent = FALSE,
                        palette = "jco", ggtheme = theme_classic(),
                        main = "K-Means聚类结果可视化")
    
## 密度聚类算法及结果可视化
db <- dbscan(multishapes[,1:2],eps = 0.15,MinPts = 5)
fviz_cluster(db, data = multishapes[,1:2], stand = FALSE,
                         ellipse = FALSE, show.clust.cent = FALSE,
                         geom = "point",palette = "jco", ggtheme = theme_classic(),
                         main = "DBSCAN聚类结果可视化")




# 数据准备
df <- iris[,1:4]
random_df <- apply(df, 2, 
                     function(x){runif(length(x), min(x), (max(x)))})
random_df <- as.data.frame(random_df)
df <- scale(df)
rarandom_df <- scale(random_df)
# 数据可视化
fviz_pca_ind(prcomp(df), title = "PCA - Iris data", 
                          habillage = iris$Species,  palette = "jco",
                          geom = "point", ggtheme = theme_classic(),
                         legend = "bottom")
#habillage 分类变量，palette 颜色分类
# Plot the random df
fviz_pca_ind(prcomp(random_df), title = "PCA - Random data", 
                       geom = "point", ggtheme = theme_classic())









