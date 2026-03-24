#计算Hopkins统计量
library(factoextra)
df <- data
print(df)
res1 <- get_clust_tendency(df, n = nrow(df)-1, graph = FALSE)
res1$hopkins_stat 

#画出碎石图    
# wss method
fviz_nbclust(df, kmeans, method = "wss") +
  labs(subtitle = "Wss method")+
  geom_vline(xintercept = 2,linetype="dashed")#在x=2处加一条线
# Silhouette method
fviz_nbclust(df, kmeans, method = "silhouette")+
  labs(subtitle = "Silhouette method")
set.seed(123)
fviz_nbclust(df, kmeans, nstart = 25,  method = "gap_stat", nboot = 50)+
  labs(subtitle = "Gap statistic method")
#根据手肘法聚成2类 

#k均值聚类
set.seed(1234) # 设置随机种子，保证每次运行结果一致
km.res <- kmeans(df, 2, nstart = 25) # 构建k-means聚类
names(km.res) 
# 绘制二元聚类图
fviz_cluster(km.res,df) 
 
  
  
  
  
  
  
  
  
    
  
  
