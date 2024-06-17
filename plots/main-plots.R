###figure 2 1000 bootsrap
###For 1000 bootstrap
df2 <- data.frame(team=rep(c('Shortest-Path', 'Average Neighbor2', 'Average Betweenness','Average Triangle', 
                             'Adhesion', 'Average Coreness', 'Average Degree','Average Neighbor1', 'Average CommonNeighbor',
                             'Average Neighbor6','Average Neighbor5',
                             'Average Neighbor4',
                             'Average Neighbor3', 'Cohesion','Average Eccentricity', 'Community Detection',
                             'Average Constraint','Average Closeness',
                             'Average Eigen-Centrality', 'Average Hub-Score'), each=18),
                  Rank=rep(c('Rank1', 'Rank2', 'Rank3', 'Rank4', 'Rank5', 'Rank6',
                             'Rank7', 'Rank8', 'Rank9', 'Rank10', 'Rank11','Rank12',
                             'Rank13','Rank14','Rank15', 'Rank16', 'Rank17', 'Rank18'), times=20),
                  points=c(1000,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
                           0,958,39,2,1,0,0,0,0,0,0,0,0,0,0,0,0,0,
                           0,12,437,373,178,0,0,0,0,0,0,0,0,0,0,0,0,0,
                           0,25,324,309,339,3,0,0,0,0,0,0,0,0,0,0,0,0,
                           0,5,200,316,479,0,0,0,0,0,0,0,0,0,0,0,0,0,
                           0,0,0,0,2,539,189,101,64,38,36,20,8,1,1,0,0,0,
                           0,0,0,0,0,136,220,209,192,115,62,36,27,3,0,0,0,0,
                           0,0,0,0,0,135,218,216,196,106,67,47,12,0,0,0,0,0,
                           0,0,0,0,0,130,215,231,216,113,52,31,8,4,0,0,0,0,
                           0,0,0,0,0,38,104,132,146,191,181,127,62,11,5,0,0,0,
                           0,0,0,0,0,12,30,54,79,195,260,210,123,29,7,0,0,0,
                           0,0,0,0,0,2,14,36,72,175,198,269,165,52,16,0,0,0,
                           0,0,0,0,0,2,7,11,27,45,75,133,302,248,150,0,0,0,
                           0,0,0,0,0,2,1,1,6,5,24,50,137,328,446,0,0,0,
                           0,0,0,0,0,0,0,9,2,17,45,77,156,320,373,0,0,0,
                           0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,729,271,0,
                           0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,271,729,0,
                           0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1000,
                           0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
                           0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
                  ))
df2$team <- factor(df2$team, levels = c('Shortest-Path', 'Average Neighbor2', 'Average Betweenness','Average Triangle', 
                                        'Adhesion', 'Average Coreness', 'Average Degree','Average Neighbor1', 'Average CommonNeighbor',
                                        'Average Neighbor6','Average Neighbor5',
                                        'Average Neighbor4',
                                        'Average Neighbor3', 'Cohesion','Average Eccentricity', 'Community Detection',
                                        'Average Constraint','Average Closeness',
                                        'Average Eigen-Centrality', 'Average Hub-Score'))
df2$Rank = factor(df2$Rank,levels = c('Rank1', 'Rank2', 'Rank3', 'Rank4', 'Rank5', 'Rank6',
                                      'Rank7', 'Rank8', 'Rank9', 'Rank10', 'Rank11','Rank12',
                                      'Rank13','Rank14','Rank15', 'Rank16', 'Rank17', 'Rank18'))
df2$Rank <- as.numeric(df2$Rank)
anyNA(df2)
df3= na.omit(df2)
tiff("fig2",width = 1600, height = 1200, res = 300)
par(mar=c(0.5,2.5,0.5,0.5),cex.axis=0.5, font.axis=1,cex.lab=1.3, font.lab=2)
ggplot(df3, aes(fill=Rank, y=points, x=team)) + theme_bw() + theme_classic() + 
  geom_bar(position='stack', stat='identity',width = 0.65,colour="black", size=0.2)+
  theme(axis.text.x = element_text(angle = 45, hjust=1),axis.ticks.x = element_blank())+ xlab("")+ ylab("Frequency") +
  #scale_fill_gradient(legend_image)
  # scale_fill_manual('Position', 
  #                  values=legend_image) #instaed of legend_image could have given 
  #c('#000000','#0F0F0F', '#252525','#373737','#525252','#3C3C3C','#414141','#505050','#737373','#696969','#6E6E6E','#bdbdbd','#D2D2D2','#D7D7D7','#DCDCDC','#E1E1E1','#E6E6E6')
  scale_fill_gradientn(
    colours = gray.colors(20,start=0,end=0.9, rev = FALSE), 
  )
dev.off()
###figure3
sl = read.csv("/home/user/MAGICAL/MAGICAL-core/biogrid-sl-pairwise-new-netprop")
sl$gi = "SL"
sv = read.csv("/home/user/MAGICAL/MAGICAL-core/biogrid-sv-pairwise-new-netprop")
sv$gi = "SV"
sl2 = read.csv("/home/user/MAGICAL/MAGICAL-core/cgidb-sl-pairwise-new-netprop")
sl2$gi = "SL"
sv2 = read.csv("/home/user/MAGICAL/MAGICAL-core/cgidb-sv-pairwise-new-netprop")
sv2$gi = "SV"
sl3 = read.csv("/home/user/MAGICAL/MAGICAL-core/sldb-sl-pairwise-new-netprop")
sl3$gi = "SL"
sv3 = read.csv("/home/user/MAGICAL/MAGICAL-core/sldb-sv-pairwise-new-netprop")
sv3$gi = "SV"
not = read.csv("/home/user/MAGICAL/MAGICAL-core/not0.pairwiseprop.csv")
not$gi = "NOT"
traindata = unique(rbind(sl,sl2,sl3,sv,sv2,sv3,not))
traindata = unique(rbind(sl,sv,not))
table(traindata$gi)
traindata$gi = as.factor(traindata$gi)
traindatabal = DMwR::SMOTE(gi ~ ., traindata, perc.under = 200)
traindatabal$gene1 = as.integer(traindatabal$gene1)
traindatabal$gene2 = as.integer(traindatabal$gene2)
#write.csv(traindatabal, "/home/nikola/biogrid-new-ppi/magical-imp-prop-new-entrez/figures/magical-bal-data-with-geneids.csv", row.names = F, quote = F)
table(traindatabal$gi)
traindatabal = traindatabal[,-c(1,2)]
index = sample(2, nrow(traindatabal), replace = TRUE, prob = c(0.70, 0.30))
train = traindatabal[index == 1,]
test = traindatabal[index == 2,]
train.magical = train[,c(2,10,15,18,20,21)]
test.magical = test[,c(2,10,15,18,20,21)]
training = train.magical
testing = test.magical
###shortest-path
tiff("fig3a.tiff",width = 600, height = 1000, res = 300)
par(mar=c(0.5,2.5,0.5,0.5),cex.axis=1, font.axis=2,cex.lab=1.3, font.lab=2)
train_nb$gi <- factor(train_nb$gi, levels = c("SL", "SV", "NOT"))
train_nb %>%
  ggplot(aes(x= gi, y=shortest_path, fill=gi,alpha=0.3)) +
  geom_boxplot(outlier.shape = NA) +
  scale_fill_manual(values=c("#980043", "#008B8B", "#969696")) +
  scale_y_continuous(limits = quantile(train_nb$shortest_path, c(0.1, 0.9))) + xlab("") + 
  theme(
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank()
  ) +  theme(axis.text = element_text(face="bold"))+
  ylab("Shortest Path") + theme_classic() + theme(legend.position="none")+
  theme(legend.key.size = unit(1, 'cm'), #change legend key size
        legend.key.height = unit(1, 'cm'), #change legend key height
        legend.key.width = unit(1, 'cm'), #change legend key width
        axis.text.x = element_text(size = 12),
        legend.title = element_text(size=20),legend.text = element_text(size=15))
dev.off()
###average neighbor2
tiff("fig3b.tiff",width = 600, height = 1000, res = 300)
par(mar=c(0.5,2.5,0.5,0.5),cex.axis=1, font.axis=2,cex.lab=1.3, font.lab=2)
train_nb$gi <- factor(train_nb$gi, levels = c("SL", "SV", "NOT"))
train_nb %>%
  ggplot(aes(x= gi, y=avgneighbor2, fill=gi,alpha=0.3)) +
  geom_boxplot(outlier.shape = NA) + 
  scale_fill_manual(values=c("#980043", "#008B8B", "#969696"), breaks = c("SL","SV","NOT"))+
  #scale_fill_manual(values=c("#FFDB58","#008B8B", "#191970")) +
  scale_y_continuous(limits = quantile(train_nb$avgneighbor2, c(0.1, 0.9))) + xlab("") + 
  theme(
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank()
  ) +  theme(axis.text = element_text(face="bold"))+ 
  ylab("Average Neighbor2") + theme_classic() + theme(legend.position="none")+
  theme(legend.key.size = unit(1, 'cm'), #change legend key size
        legend.key.height = unit(1, 'cm'), #change legend key height
        legend.key.width = unit(1, 'cm'), #change legend key width
        axis.text.x = element_text(size = 12),
        legend.title = element_text(size=20),legend.text = element_text(size=15))
dev.off()
#betweennness
train_nb = train.magical
tiff("fig3c.tiff",width = 600, height = 1000, res = 300)
par(mar=c(0.5,2.5,0.5,0.5),cex.axis=1, font.axis=2,cex.lab=1.3, font.lab=2)
train_nb$gi <- factor(train_nb$gi, levels = c("SL", "SV", "NOT"))
train_nb %>%
  ggplot(aes(x= gi, y=avgbet, fill=gi,alpha=0.3)) +
  geom_boxplot(outlier.shape = NA) + ##old colors "#FFDB58","#008B8B", "#191970"
  scale_fill_manual(values=c("#980043", "#008B8B", "#969696")) +
  scale_y_continuous(limits = quantile(train_nb$avgbet, c(0.1, 0.9))) + xlab("") + 
  theme(
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank()
  ) +  theme(axis.text = element_text(face="bold"))+
  ylab("Average Betweenness") + theme_classic() + theme(legend.position="none")+
  theme(legend.key.size = unit(1, 'cm'), #change legend key size
        legend.key.height = unit(1, 'cm'), #change legend key height
        legend.key.width = unit(1, 'cm'), #change legend key width
        axis.text.x = element_text(size = 12),
        legend.title = element_text(size=20),legend.text = element_text(size=15))
dev.off()
#triangle
setwd("/home/nikola/biogrid-new-ppi/magical-imp-prop-new-entrez/figures/")
tiff("fig3d.tiff",width = 600, height = 1000, res = 300)
par(mar=c(0.5,2.5,0.5,0.5),cex.axis=1.4, font.axis=4,cex.lab=1.4, font.lab=4)
train_nb$gi <- factor(train_nb$gi, levels = c("SL", "SV", "NOT"))
train_nb %>%
  ggplot(aes(x= gi, y=avgtriangle, fill=gi,alpha=0.3)) +
  geom_boxplot(outlier.shape = NA) +
  scale_fill_manual(values=c("#980043", "#008B8B", "#969696")) +
  scale_y_continuous(limits = quantile(train_nb$avgtriangle, c(0.1, 0.9))) + xlab("") + 
  theme(
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank()
  ) +  theme(axis.text = element_text(face="bold"))+
  ylab("Average Traingle") + theme_classic() + theme(legend.position="none")+
  theme(legend.key.size = unit(1, 'cm'), #change legend key size
        legend.key.height = unit(1, 'cm'), #change legend key height
        legend.key.width = unit(1, 'cm'), #change legend key width
        axis.text.x = element_text(size = 12),
        legend.title = element_text(size=20),legend.text = element_text(size=15))
dev.off()
####adhesion
tiff("fig3e.tiff",width = 600, height = 1000, res = 300)
par(mar=c(0.5,2.5,0.5,0.5),cex.axis=1, font.axis=2,cex.lab=1.3, font.lab=2)
train_nb$gi <- factor(train_nb$gi, levels = c("SL", "SV", "NOT"))
train_nb %>%
  ggplot(aes(x= gi, y=adhesion, fill=gi,alpha=0.3)) +
  geom_boxplot(outlier.shape = NA) + 
  scale_fill_manual(values=c("#980043", "#008B8B", "#969696"), breaks = c("SL","SV","NOT"))+
  #scale_fill_manual(values=c("#FFDB58","#008B8B", "#191970")) +
  scale_y_continuous(limits = quantile(train_nb$adhesion, c(0.1, 0.9))) + xlab("") + 
  theme(
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank()
  ) +  theme(axis.text = element_text(face="bold"))+ 
  ylab("Adhesion") + theme_classic() + theme(legend.position="none")+
  theme(legend.key.size = unit(1, 'cm'), #change legend key size
        legend.key.height = unit(1, 'cm'), #change legend key height
        legend.key.width = unit(1, 'cm'), #change legend key width
        axis.text.x = element_text(size = 12),
        legend.title = element_text(size=20),legend.text = element_text(size=15))
dev.off()
###figure4
library(randomForest)
sl = read.csv("/home/user/MAGICAL/MAGICAL-core/biogrid-sl-pairwise-new-netprop")
sl$gi = "SL"
sv = read.csv("/home/user/MAGICAL/MAGICAL-core/biogrid-sv-pairwise-new-netprop")
sv$gi = "SV"
sl2 = read.csv("/home/user/MAGICAL/MAGICAL-core/cgidb-sl-pairwise-new-netprop")
sl2$gi = "SL"
sv2 = read.csv("/home/user/MAGICAL/MAGICAL-core/cgidb-sv-pairwise-new-netprop")
sv2$gi = "SV"
sl3 = read.csv("/home/user/MAGICAL/MAGICAL-core/sldb-sl-pairwise-new-netprop")
sl3$gi = "SL"
sv3 = read.csv("/home/user/MAGICAL/MAGICAL-core/sldb-sv-pairwise-new-netprop")
sv3$gi = "SV"
not = read.csv("/home/user/MAGICAL/MAGICAL-core/not0.pairwiseprop.csv")
not$gi = "NOT"
traindata = unique(rbind(sl,sl2,sl3,sv,sv2,sv3,not))
traindata = unique(rbind(sl,sv,not))
table(traindata$gi)
traindata$gi = as.factor(traindata$gi)
traindatabal = DMwR::SMOTE(gi ~ ., traindata, perc.under = 200)
traindatabal$gene1 = as.integer(traindatabal$gene1)
traindatabal$gene2 = as.integer(traindatabal$gene2)
#write.csv(traindatabal, "/home/nikola/biogrid-new-ppi/magical-imp-prop-new-entrez/figures/magical-bal-data-with-geneids.csv", row.names = F, quote = F)
table(traindatabal$gi)
traindatabal = traindatabal[,-c(1,2)]
index = sample(2, nrow(traindatabal), replace = TRUE, prob = c(0.70, 0.30))
train = traindatabal[index == 1,]
test = traindatabal[index == 2,]
train.magical = train[,c(2,10,15,18,20,21)]
test.magical = test[,c(2,10,15,18,20,21)]
training = train.magical
testing = test.magical
mag.bio = randomForest(gi ~ . , data = training, importance = T)
prediction = predict(mag.bio, newdata = testing[-6])
predict_gi = predict(mag.bio, newdata = testing)
#magical = randomForest(gi ~ . , data = training, importance = T)
# prediction = predict(magical, newdata = testing[-6])
# predict_gi = predict(magical, newdata = testing)
testing$predict_gi = predict_gi
cnf_mat = table(testing$gi, testing$predict_gi)
cnf_mat
accuracy = sum(diag(cnf_mat)/sum(cnf_mat))
varImpPlot(magical, sort = T, n.var = 5, main = "Discriminatory variables magical-exp")
saveRDS(magical, "magical.rds")
#####magical-exp
library(dplyr)
stack1 = data.frame (class  = c("SL", "SL", "SL","SV","SV","SV","NOT","NOT","NOT"),
                     category = c("SL", "SV", "NOT","SV","SL","NOT","NOT", "SL", "SV"),
                     score = c(82.76,12.66,4.56,83.32,14.76,1.90,68.12,23.20,8.67)
)
stack1$category <- factor(stack1$category, levels = c("SL", "SV", "NOT"))
tiff("fig4a.tiff",width = 1200, height = 1200, res = 300)
par(mar=c(0.5,2.5,0.5,0.5),cex.axis=1, font.axis=2,cex.lab=1.3, font.lab=2)
stack1 %>%
  mutate(class =ordered(class, levels = unique(class))) %>%
  ggplot(aes(class,score,fill=category, alpha = 0.3))+ theme_classic()+ 
  theme(axis.text.x = element_text(size = 20), axis.text.y = element_text(size = 20),
        text = element_text(size = 20))+
  ggtitle("")+ ylab("Accuracy")+ theme(legend.position="none")+
  geom_bar(stat="identity")+   #3CB371 #619CFF
  scale_fill_manual(values=c("#980043", "#008B8B", "#969696"))+ xlab("")
dev.off()
####magical-exp-roc-plot
traindatabal2 = traindatabal[,c(2,10,15,18,20,21)]
library(caret)
repeat_cv <- trainControl(method='repeatedcv', number=10, repeats=3)
train_index <- createDataPartition(y=traindatabal2$gi, p=0.7, list=FALSE)
train <- traindatabal2[train_index, ]
test <- traindatabal2[-train_index, ]
training_set = train
testing_set = test
forest <- train(
  
  # Formula. We are using all variables to predict Species
  gi~., 
  
  # Source of data; remove the Species variable
  data=training_set, 
  
  # `rf` method for random forest
  method='rf', 
  
  # Add repeated cross validation as trControl
  trControl=repeat_cv,
  
  # Accuracy to measure the performance of the model
  metric='Accuracy')
forest$finalModel
prediction = predict(forest, newdata = testing_set[-6])
#the prediction value is taken where magical.model was built
library(pROC)
testing_set$gi = as.factor(testing_set$gi)
result <- pROC::multiclass.roc(as.numeric(prediction), 
                               as.numeric(testing_set$gi))

#prueba$gi was changed to numeric for multiclass.roc.
#blue shade #6495ED
tiff("fig4b.tiff",width = 1200, height = 1200, res = 300)
par(mar=c(0.5,2.5,0.5,0.5),cex.axis=1, font.axis=1,cex.lab=1.1, font.lab=1.5)
plot.roc(result$rocs[[1]], 
         print.auc=T, col = "#969696", lwd = 5,
         legacy.axes = T, cex.lab = 2, cex.axis =2, plot = T, asp = NA)
#axis(side = 2, font = 5)
plot.roc(result$rocs[[2]],
         add=T, col = '#980043',
         print.auc = T,
         legacy.axes = T, lwd = 5,
         print.auc.adj = c(0,3))
plot.roc(result$rocs[[3]],add=T, col = '#008B8B',
         print.auc=T,
         legacy.axes = T, lwd = 5,
         print.auc.adj = c(0,5),scales=list(x=list(font=2,cex=5)))
dev.off()
#######magical-pred#######
sl = read.csv("/home/user/MAGICAL/MAGICAL-combined/biogrid-sl-pairwise-new-netprop")
sl$gi = "SL"
sv = read.csv("/home/user/MAGICAL/MAGICAL-combined/biogrid-sv-pairwise-new-netprop")
sv$gi = "SV"
sl2 = read.csv("/home/user/MAGICAL/MAGICAL-combined/cgidb-sl-pairwise-new-netprop")
sl2$gi = "SL"
sv2 = read.csv("/home/user/MAGICAL/MAGICAL-combined/cgidb-sv-pairwise-new-netprop")
sv2$gi = "SV"
sl3 = read.csv("/home/user/MAGICAL/MAGICAL-combined/sldb-sl-pairwise-new-netprop")
sl3$gi = "SL"
sv3 = read.csv("/home/user/MAGICAL/MAGICAL-combined/sldb-sv-pairwise-new-netprop")
sv3$gi = "SV"
not = read.csv("/home/user/MAGICAL/MAGICAL-combined/not-pairwise-new-netprop")
#not = not[c(1:2500),]
not$gi = "NOT"
traindata = unique(rbind(sl,sl2,sl3,sv,sv2,sv3,not))
table(traindata$gi)
traindata$gi = as.factor(traindata$gi)
traindatabal = DMwR::SMOTE(gi ~ ., traindata[,-c(1,2)], perc.under = 200)
table(traindatabal$gi)
#training = traindatabal
index = sample(2, nrow(traindatabal), replace = TRUE, prob = c(0.70, 0.30))
train = traindatabal[index == 1,]
test = traindatabal[index == 2,]
training = train[,c(2,10,15,18,20,21)]
testing = test[,c(2,10,15,18,20,21)]
set.seed(123)
library(randomForest)
model.pred = randomForest(gi ~ . , data = training, importance = T)
prediction = predict(model.pred, newdata = testing[-6])
predict_gi = predict(model.pred, newdata = testing)
testing$predict_gi = predict_gi
cnf_mat = table(testing$gi, testing$predict_gi)
cnf_mat
accuracy = sum(diag(cnf_mat)/sum(cnf_mat)) #80.390
test_pred2 <- predict(model.pred, testing, type = "prob")[,2]
ci.auc(testing$gi, test_pred2, conf.level = 0.9) #90% CI: 0.846-0.871 (DeLong)
varImpPlot(model.pred, sort = T, n.var = 5, main = "Discriminatory variables magical-pred")
#####barplot
library(ggplot2)
stack1 = data.frame (class  = c("SL", "SL", "SL","SV","SV","SV","NOT","NOT","NOT"),
                     category = c("SL", "SV", "NOT","SV","SL","NOT","NOT", "SL", "SV"),
                     score = c(91.02,7.33,1.64,72.32,25.96,1.71,55.23,36.97,7.78)
)
stack1$category <- factor(stack1$category, levels = c("SL", "SV", "NOT"))
tiff("fig4c.tiff",width = 1200, height = 1200, res = 300)
par(mar=c(0.5,2.5,0.5,0.5),cex.axis=1, font.axis=2,cex.lab=1.3, font.lab=2)
stack1 %>%
  mutate(class =ordered(class, levels = unique(class))) %>%
  ggplot(aes(class,score,fill=category, alpha=0.3))+ theme_classic()+
  theme(axis.text.x = element_text(size = 20), axis.text.y = element_text(size = 20),
        text = element_text(size = 20))+
  ggtitle("")+ ylab("Accuracy")+ theme(legend.position="none")+
  geom_bar(stat="identity")+   #3CB371 #619CFF
  scale_fill_manual(values=c("#980043","#008B8B","#969696"))+ xlab("")
dev.off()
######magical-pred-roc
library(caret)
traindatabal2 = traindatabal[,c(2,10,15,18,20,21)]
repeat_cv <- trainControl(method='repeatedcv', number=10, repeats=3)
train_index <- createDataPartition(y=traindatabal2$gi, p=0.7, list=FALSE)
train <- traindatabal2[train_index, ]
test <- traindatabal2[-train_index, ]
training_set = train
testing_set = test
forest2 <- train(
  
  # Formula. We are using all variables to predict Species
  gi~., 
  
  # Source of data; remove the Species variable
  data=training_set, 
  
  # `rf` method for random forest
  method='rf', 
  
  # Add repeated cross validation as trControl
  trControl=repeat_cv,
  
  # Accuracy to measure the performance of the model
  metric='Accuracy')
forest2$finalModel
prediction2 = predict(forest2, newdata = testing_set[-6])
#the prediction value is taken where magical.model was built
library(pROC)
testing_set$gi = as.factor(testing_set$gi)
result2 <- pROC::multiclass.roc(as.numeric(prediction2), 
                                as.numeric(testing_set$gi))

#prueba$gi was changed to numeric for multiclass.roc.
#blue shade #6495ED
tiff("fig4d.tiff",width = 1200, height = 1200, res = 300)
par(mar=c(0.5,2.5,0.5,0.5),cex.axis=1, font.axis=1,cex.lab=1.1, font.lab=1.5)
plot.roc(result2$rocs[[1]], 
         print.auc=T, col = "#969696", lwd = 5,
         legacy.axes = T, cex.lab = 2, cex.axis =2, plot = T, asp = NA)
#axis(side = 2, font = 5)
plot.roc(result2$rocs[[2]],
         add=T, col = '#980043',
         print.auc = T,
         legacy.axes = T, lwd = 5,
         print.auc.adj = c(0,3))
plot.roc(result2$rocs[[3]],add=T, col = '#008B8B',
         print.auc=T,
         legacy.axes = T, lwd = 5,
         print.auc.adj = c(0,5),scales=list(x=list(font=2,cex=5)))
legend("bottomright", 
       legend = c("SL", "SV", "NOT"), 
       col = c("#980043","#008B8B","#969696"),
       lwd = 5, cex = 0.6)
dev.off()
###figure 5a,b
library(caret)
library(randomForest)
control <- trainControl(method="cv", summaryFunction=twoClassSummary, classProbs=T,
                        savePredictions = T)
sl = read.csv("/home/user/MAGICAL/MAGICAL-core/biogrid-sl-pairwise-new-netprop")
sl$gi = "SL"
sv = read.csv("/home/user/MAGICAL/MAGICAL-core/biogrid-sv-pairwise-new-netprop")
sv$gi = "NOT"
not = read.csv("/home/user/MAGICAL/MAGICAL-core/not0.pairwiseprop.csv")
not$gi = "NOT"
traindata = unique(rbind(sl,sv,not))
table(traindata$gi)
traindata$gi = as.factor(traindata$gi)
traindata = na.omit(traindata)
traindatabal = traindata
table(traindatabal$gi)
traindatabalcp = traindata[,c(1,2,6,21,22,23)]
ind = sample(2, nrow(traindatabalcp[,-c(1,2)]), replace = TRUE, prob = c(0.70, 0.30))
tr2 = traindatabalcp[ind == 1,]
te2 = traindatabalcp[ind == 2,]
slant.unbal.train = tr2
slant.unbal.test = te2
slant.unbal <- train(gi~., data=select(slant.unbal.train, -gene1, -gene2), method='ranger', trControl=control, tuneLength=5, metric="ROC", preProc=c("center", "scale"))
prediction = predict(slant.unbal, newdata = slant.unbal.test[,-c(1,2)])
predict_gi = predict(slant.unbal, newdata = slant.unbal.test[,-c(1,2)])
slant.unbal.test$predict_gi = predict_gi
cnf_mat = table(slant.unbal.test$gi, slant.unbal.test$predict_gi)
cnf_mat
accuracy.slant.unbal = sum(diag(cnf_mat)/sum(cnf_mat))
###slant bal data read
slantbal = read.csv("/home/user/MAGICAL/MAGICAL-SLant-comparison/slant-data.csv")
table(slantbal$gi)
traindatabalcp = slantbal
table(traindatabalcp$gi)
ind = sample(2, nrow(traindatabalcp[,-c(1,2)]), replace = TRUE, prob = c(0.70, 0.30))
tr2 = traindatabalcp[ind == 1,]
te2 = traindatabalcp[ind == 2,]
library(dplyr)
slant.bal.train = tr2
slant.bal.test = te2
slant.bal <- train(gi~., data=select(slant.bal.train, -gene1, -gene2), method='ranger', trControl=control, tuneLength=5, metric="ROC", preProc=c("center", "scale"))
prediction = predict(slant.bal, newdata = slant.bal.test[,-c(1,2)])
predict_gi = predict(slant.bal, newdata = slant.bal.test[,-c(1,2)])
slant.bal.test$predict_gi = predict_gi
cnf_mat = table(slant.bal.test$gi, slant.bal.test$predict_gi)
cnf_mat
accuracy.slant.bal = sum(diag(cnf_mat)/sum(cnf_mat))
#########unbal slant
prediction.slant2 = predict(slant.unbal, newdata = slant.unbal.test[,-c(1,2,6,7)])
slant.unbal.test$gi = as.factor(slant.unbal.test$gi)
result.slant2 <- pROC::multiclass.roc(as.numeric(prediction.slant2), 
                                      as.numeric(slant.unbal.test$gi))
#########unbal magical
#testing2 = testing[,-c(7)]
testing2 = testing.unbal.mag[,-c(7)]
prediction.magical2 = predict(magical.unbal.bio, newdata = testing2[,-c(6)])
testing2$gi = as.factor(testing2$gi)
result.magical2 <- pROC::multiclass.roc(as.numeric(prediction.magical2), 
                                        as.numeric(testing2$gi))
###plot
#tiff("slant-magical-unbal-final.tiff",width = 1600, height = 1400, res = 300)
tiff("fig5a.tiff",width = 1600, height = 1400, res = 300)
par(mar=c(0.5,2.5,0.5,0.5),cex.axis=1, font.axis=1,cex.lab=1.1, font.lab=1.5)
plot.roc(result.slant2$rocs[[1]], 
         print.auc=T, col = "black", lwd = 3, #gray color #969696
         legacy.axes = T, cex.lab = 2, cex.axis =2, plot = T, asp = NA)
plot.roc(result.magical2$rocs[[2]],add=T, col = "#980043", ##F8766D 3lwd = 5, print.auc.x=0.8,print.auc.y=0.33,
         print.auc=T,cex.lab = 2, cex.axis =2,
         legacy.axes = T, lwd = 5, 
         print.auc.adj = c(0,5),scales=list(x=list(font=2,cex=5)))
legend("bottomright", 
       legend = c("MAGICAL-SL", "SLant-SL"), 
       col = c("#980043","black"), #969696 gray color
       lty = c(1, 1),
       cex = 0.6,
       lwd = c(5,3))
dev.off()
###slant bal
#te3 = te2[,-c(1,2,7)]
# te3 = slant.bal.test[,-c(1,2,7)]
# prediction.slant3 = predict(slant.bal, newdata = te3[,-c(4)])
# te3$gi = as.factor(te3$gi)
# result.slant3 <- pROC::multiclass.roc(as.numeric(prediction.slant3), 
#                                       as.numeric(te3$gi))
te3 = slant.bal.test[,-c(1,2,7)]
prediction.slant3 = predict(slant.bal, newdata = te3[,-c(4)])
te3$gi = as.factor(te3$gi)
result.slant3 <- pROC::multiclass.roc(as.numeric(prediction.slant3), 
                                      as.numeric(te3$gi))
###magical bal
#testing3 = testing[,-c(7)]
testing3 = testing.bal.mag[,-c(7)]
prediction.magical3 = predict(magical.bio, newdata = testing3[,-c(6)])
testing3$gi = as.factor(testing3$gi)
result.magical3 <- pROC::multiclass.roc(as.numeric(prediction.magical3), 
                                      as.numeric(testing3$gi))
#tiff("slant-magical-bal-final.tiff",width = 1600, height = 1400, res = 300)
tiff("fig5b.tiff",width = 1600, height = 1400, res = 300)
par(mar=c(0.5,2.5,0.5,0.5),cex.axis=1, font.axis=1,cex.lab=1.1, font.lab=1.5)
plot.roc(result.slant3$rocs[[1]], 
         print.auc=T, col = "black", lwd = 3,
         legacy.axes = T, cex.lab = 2, cex.axis =2, plot = T, asp = NA)
plot.roc(result.magical3$rocs[[2]],add=T, col = "#980043", ##F8766D 3lwd = 5, print.auc.x=0.8,print.auc.y=0.33,
         print.auc=T,cex.lab = 2, cex.axis =2,
         legacy.axes = T, lwd = 5, 
         print.auc.adj = c(0,5),scales=list(x=list(font=2,cex=5)))
legend("bottomright", 
       legend = c("MAGICAL-SL", "SLant-SL"), 
       col = c("#980043","black"), #969696 gray color
       lty = c(1, 1),
       cex = 0.6,
       lwd = c(5,3))
dev.off()

###figure5c depmap and crispr data
depmap = read.csv("/home/nikola/biogrid-new-ppi/new-entrez/depmap.pairwiseprop-newentrez.csv")
depmap$gi = "SL"
svdr = read.csv("/home/nikola/biogrid-new-ppi/new-entrez/svdr.pairwiseprop-newentrez.csv")
svdr$gi = "SV"
dusr = read.csv("/home/nikola/biogrid-new-ppi/new-entrez/dusr.pairwiseprop-newentrez.csv")
ddsr = read.csv("/home/nikola/biogrid-new-ppi/new-entrez/ddsr.pairwiseprop-newentrez.csv")
sr = unique(rbind(dusr,ddsr))
sr$gi = "SV"
svtot = data.frame(unique(rbind(svdr,dusr,ddsr)))
svtot$gi = "SV"
not2 = read.csv("/home/nikola/biogrid-new-ppi/new-entrez/not1.pairwiseprop.csv")
not2$gi = "NOT"
test.depmap = data.frame(unique(rbind(depmap,svtot, not2)))
test.depmap = data.frame(unique(rbind(depmap,svdr, not2)))
test.depmap = data.frame(unique(rbind(depmap,sr,not2)))
test.depmap = data.frame(unique(rbind(depmap,svdr,sr,not2)))
test.depmap = test.depmap[,-c(1,2)]
test.depmap$gi = as.factor(test.depmap$gi)
test.depmap.bal = DMwR::SMOTE(gi ~ ., test.depmap, perc.under = 200)
testing_set = test.depmap.bal[,c(2,10,15,18,20,21)]
forest$finalModel
prediction = predict(forest, newdata = testing_set[-6])
#the prediction value is taken where magical.model was built
library(pROC)
testing_set$gi = as.factor(testing_set$gi)
result.depmap <- pROC::multiclass.roc(as.numeric(prediction), 
                                      as.numeric(testing_set$gi))
tiff("fig5c.tiff",width = 1600, height = 1400, res = 300)
par(mar=c(0.5,2.5,0.5,0.5),cex.axis=1, font.axis=1,cex.lab=1.1, font.lab=1.5)
plot.roc(result.depmap$rocs[[2]], 
         print.auc=T, col = 'black', lwd = 2,
         legacy.axes = T, cex.lab = 2, cex.axis =2, plot = T, asp = NA)
plot.roc(result.crispr$rocs[[2]],add=T, col = "black", ##F8766D 3lwd = 5, print.auc.x=0.8,print.auc.y=0.33,
         print.auc=T,cex.lab = 2, cex.axis =2, lty = 3,
         legacy.axes = T, lwd = 2, 
         print.auc.adj = c(0,5),scales=list(x=list(font=2,cex=5)))
legend("bottomright", 
       legend = c("DEPMAP-SL", "CRISPR-SL"), 
       col = c("black","black"), #969696 gray color
       lty = c(1, 3),
       cex = 0.6,
       lwd = c(2,2))
dev.off()
#####crispr data
crispr.sl = read.csv("/home/nikola/biogrid-new-ppi/new-entrez/crispr-sl-pairwise-new-entrez")
crispr.sv = read.csv("/home/nikola/biogrid-new-ppi/new-entrez/crispr-sv-pairwise-new-entrez")
crispr.sl$gi =  "SL"
crispr.sv$gi = "SV"
not2 = read.csv("/home/nikola/biogrid-new-ppi/new-entrez/not1.pairwiseprop.csv")
not2$gi = "NOT"
crispr.tot2 = unique(rbind(crispr.sl,crispr.sv,not2))
crispr.tot2 = crispr.tot2[,-c(1,2)]
testing_set = crispr.tot2[,c(2,10,15,18,20,21)]
prediction = predict(forest, newdata = testing_set[-6])
testing_set$gi = as.factor(testing_set$gi)
result.crispr <- pROC::multiclass.roc(as.numeric(prediction), 
                                      as.numeric(testing_set$gi))
plot.roc(result.crispr$rocs[[2]],add=T, col = '#c994c7', ##F8766D 3lwd = 5, print.auc.x=0.8,print.auc.y=0.33,
         print.auc=T,cex.lab = 2, cex.axis =2,
         legacy.axes = T, lwd = 5, print.auc.x=1.0,print.auc.y=0.35,
         print.auc.adj = c(0,5),scales=list(x=list(font=2,cex=5)))
