##figure S1
library(ggplot2)
tab = data.frame(model = c(1:100), acc = c(82, 83, 84, 83, 83, 84, 83, 83, 84, 83, 83, 83, 83, 83, 84, 84, 83, 83, 84,
                                           83, 83, 83, 83, 84, 83, 83, 83, 83, 85, 83, 83, 84, 83, 82, 84, 83, 83,
                                           84, 83, 83, 83, 83,83, 83, 83, 83, 83, 84, 83, 82, 83, 84, 83, 83, 84,
                                           84, 84, 83, 83, 83, 83, 83, 84, 84, 83, 84, 83, 83, 84, 83, 84, 84, 84,
                                           83, 84, 83, 83, 83, 83, 83, 84, 84, 83, 84, 83, 83, 83, 84, 83, 83, 84,
                                           87, 83, 83, 84, 83, 83, 83, 84, 83
))
tiff("S1.tiff",width = 1000, height = 1000, res = 300)
par(mar=c(0.5,2.5,0.5,0.5),cex.axis=1, font.axis=2,cex.lab=1.3, font.lab=2)
ggplot(tab, aes(x=acc)) + geom_histogram(binwidth = 1, fill = "#969696", 
                                         color = "#e9ecef", alpha = 0.9)+
  xlab("Accuracy")+ ylab("Number of Models")+theme_classic()+
  theme(plot.title = element_text(size = 15),axis.text.y = element_text(size = 12),
        axis.text.x = element_text(size = 12))
dev.off()

###figure S2
other_table2 = matrix(
  # Taking sequence of elements  
  c(0.81,0.78,0.80,0.79,0.78,0.76,0.79,0.77,0.79,0.77,0.79,0.77,0.78,0.76,0.78,0.77,0.79,0.77,0.79,0.78), 
  # No of rows
  nrow = 5,   
  # No of columns
  ncol = 4,         
  byrow = T         
)
rownames(other_table2) = c("Average", "Summation", "Difference", "Maximum", "Minimum")
colnames(other_table2) = c("Average","Precision", "Recall", "F1-score")
gfg <- data.frame(x = c(0.81,0.78,0.80,0.79,0.78,0.76,0.79,0.77,0.79,0.77,0.79,0.77,0.78,0.76,0.78,0.77,0.79,0.77,0.79,0.78),  
                  grp = rep(c("Average", "Average", "Average","Average",
"Summation", "Summation", "Summation","Summation", "Difference", "Difference", 
"Difference", "Difference", "Maximum","Maximum", "Maximum","Maximum", "Minimum",
"Minimum", "Minimum", "Minimum")),
                  Metric = c("Accuracy","Precision", "Recall", "F1-score")) 
tiff("S2.tiff",width = 1400, height = 1200, res = 300)
par(mar=c(0.5,2.5,0.5,0.5),cex.axis=0.5, font.axis=1,cex.lab=1.3, font.lab=2)
ggplot(gfg,aes(x = grp, y = x, fill = Metric)) + xlab("") + ylab("Values") +
  scale_fill_manual(values=c("#781163","#ae017e","#f768a1","#fbb4b9")) + theme_bw() +
  geom_bar(stat = "identity", position = "dodge") + theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust=1))+
  theme(legend.key.size = unit(1, 'cm'), #change legend key size
        legend.key.height = unit(1, 'cm'), #change legend key height
        legend.key.width = unit(1, 'cm'), #change legend key width
        legend.title = element_text(size=12),legend.text = element_text(size=12))
dev.off()
##figure S3
other_table = matrix(
  # Taking sequence of elements  
  c(0.886,0.903,0.894,0.747,0.798,0.763,0.661,0.715,0.668,0.680,0.616,0.577), 
  # No of rows
  nrow = 4,   
  # No of columns
  ncol = 3,         
  byrow = TRUE         
)
rownames(other_table) = c("RF", "KNN", "DT", "NB")
colnames(other_table) = c("Precision", "Recall", "F1-score")
gfg <- data.frame(x = c(0.873,0.886,0.903,0.894,0.754,0.747,0.798,0.763,0.650,0.661,0.715,0.668,0.604,0.680,0.616,0.577),  
                  grp = rep(c("RF", "RF", "RF","RF","KNN", "KNN", "KNN","KNN", "DT", "DT", "DT", "DT", "NB","NB", "NB","NB")),
                  Metric = c("Accuracy","Precision", "Recall", "F1-score")) 
tiff("S3.tiff",width = 1400, height = 1200, res = 300)
par(mar=c(0.5,2.5,0.5,0.5),cex.axis=0.5, font.axis=1,cex.lab=1.3, font.lab=2)
ggplot(gfg,aes(x = grp, y = x, fill = Metric)) + xlab("ML Models") + ylab("Values") +
  scale_fill_manual(values=c("#781163","#ae017e","#f768a1","#fbb4b9")) + theme_bw() +
  geom_bar(stat = "identity", position = "dodge") + theme_classic() +
  theme(legend.key.size = unit(1, 'cm'), #change legend key size
        legend.key.height = unit(1, 'cm'), #change legend key height
        legend.key.width = unit(1, 'cm'), #change legend key width
        legend.title = element_text(size=12),legend.text = element_text(size=12))
dev.off()

##figure S4
magical = readRDS("/home/user/MAGICAL/plots/magical.rds")
training = train
testing = test
magical.all = randomForest(gi ~ . , data = training, importance = T)
model = magical.all
varimp = as.data.frame(importance(model))
dfm2 = varimp
#change the row names
rownames(dfm2)[1] = "Average Betweenness"
rownames(dfm2)[2] = "Average Neighbor2"
rownames(dfm2)[3] = "Average Triangle"
rownames(dfm2)[4] = "Shortest-Path"
rownames(dfm2)[5] = "Adhesion"
####
rownames(dfm2)[1] = "Average Degree"
rownames(dfm2)[2] = "Average Betweenness"
rownames(dfm2)[3] = "Average Closeness"
rownames(dfm2)[4] = "Average Coreness"
rownames(dfm2)[5] = "Average Constraint"
rownames(dfm2)[6] = "Average Eccentricity"
rownames(dfm2)[7] = "Average Eigen-Centrality"
rownames(dfm2)[8] = "Average Hub-Score"
rownames(dfm2)[9] = "Average Neighbor1"
rownames(dfm2)[10] = "Average Neighbor2"
rownames(dfm2)[11] = "Average Neighbor3"
rownames(dfm2)[12] = "Average Neighbor4"
rownames(dfm2)[13] = "Average Neighbor5"
rownames(dfm2)[14] = "Average Neighbor6"
rownames(dfm2)[15] = "Average Triangle"
rownames(dfm2)[16] = "Average Common-Neighbor"
rownames(dfm2)[17] = "Average Community-Detection"
rownames(dfm2)[18] = "Shortest-Path"
rownames(dfm2)[19] = "Cohesion"
rownames(dfm2)[20] = "Adhesion"
#dfm2$netprop = c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0)
dfm2$netprop = c("Node-wise","Node-wise","Node-wise","Pair-wise","Pair-wise")
dfm2$netprop = c("Node-wise","Node-wise","Node-wise","Node-wise","Node-wise","Node-wise","Node-wise","Node-wise","Node-wise","Node-wise","Node-wise","Node-wise","Node-wise","Node-wise","Node-wise","Node-wise","Node-wise","Pair-wise","Pair-wise","Pair-wise")
dfm2$netprop = as.factor(dfm2$netprop)
dfm2$name <- rownames(dfm2)
colnames(dfm2)[6] = "Network property"
colnames(dfm2)[4] = "Mean decrease in accuracy"
library(ggpubr)
tiff("S4.tiff",width = 1800, height = 1200, res = 300)
par(mar=c(0.5,2.5,0.5,0.5),cex.axis=1, font.axis=2,cex.lab=1.3, font.lab=2)
ggdotchart(dfm2, x = "name", y = "Mean decrease in accuracy",
           color = "Network property",                                # Color by groups
           palette = c("#00AFBB", "#AA4A44"), # Custom color palette
           sorting = "descending",                       # Sort value in descending order
           rotate = TRUE,                                # Rotate vertically
           dot.size = 2.5,                                 # Large dot size
           y.text.col = FALSE, #if true the legends will of the same color                            # Color y text by groups
           ggtheme = theme_pubr()                        # ggplot2 theme
)+ #theme(axis.title = element_text(face="bold"))+
  theme_cleveland()
dev.off()

##figure S5
df2 <- data.frame(team=rep(c('Shortest-Path', 'Average Neighbor2', 'Average Triangle', 'Average Betweenness',
                             'Adhesion', 'Average Coreness', 'Average Neighbor1', 'Average Degree','Average Neighbor6',
                             'Average CommonNeighbor','Average Neighbor4',
                             'Average Neighbor5',
                             'Average Neighbor3', 'Cohesion','Average Eccentricity', 'Community Detection',
                             'Average Constraint','Average Closeness',
                             'Average Eigen-Centrality', 'Average Hub-Score'), each=18),
                  Rank=rep(c('Rank1', 'Rank2', 'Rank3', 'Rank4', 'Rank5', 'Rank6',
                             'Rank7', 'Rank8', 'Rank9', 'Rank10', 'Rank11','Rank12',
                             'Rank13','Rank14','Rank15', 'Rank16', 'Rank17', 'Rank18'), times=20),
                  points=c(100,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
                           0,95,4,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
                           0,3,40,27,30,0,0,0,0,0,0,0,0,0,0,0,0,0,
                           0,0,37,37,26,0,0,0,0,0,0,0,0,0,0,0,0,0,
                           0,2,19,35,44,0,0,0,0,0,0,0,0,0,0,0,0,0,
                           0,0,0,0,0,62,20,7,6,3,2,0,0,0,0,0,0,0,
                           0,0,0,0,0,9,27,25,22,7,5,4,1,0,0,0,0,0,
                           0,0,0,0,0,15,17,31,19,10,3,4,1,0,0,0,0,0,
                           0,0,0,0,0,1,9,15,22,17,19,12,2,3,0,0,0,0,
                           0,0,0,0,0,12,22,17,12,22,10,2,2,1,0,0,0,0,
                           0,0,0,0,0,0,0,2,4,14,30,23,21,6,0,0,0,0,
                           0,0,0,0,0,0,4,3,1,12,17,21,30,9,2,1,0,0,
                           0,0,0,0,0,0,1,0,3,8,5,16,34,23,10,0,0,0,
                           0,0,0,0,0,0,0,0,0,2,2,5,22,37,32,0,0,0,
                           0,0,0,0,0,0,0,0,0,0,3,4,8,28,57,0,0,0,
                           0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,71,29,0,
                           0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,29,71,0,
                           0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,100,
                           0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
                           0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
                  ))
df2$team <- factor(df2$team, levels = c('Shortest-Path', 'Average Neighbor2', 'Average Triangle', 'Average Betweenness',
                             'Adhesion', 'Average Coreness', 'Average Neighbor1', 'Average Degree','Average Neighbor6',
                             'Average CommonNeighbor','Average Neighbor4',
                             'Average Neighbor5',
                             'Average Neighbor3', 'Cohesion','Average Eccentricity', 'Community Detection',
                             'Average Constraint','Average Closeness',
                             'Average Eigen-Centrality', 'Average Hub-Score'))
df2$Rank = factor(df2$Rank,levels = c('Rank1', 'Rank2', 'Rank3', 'Rank4', 'Rank5', 'Rank6',
                                      'Rank7', 'Rank8', 'Rank9', 'Rank10', 'Rank11','Rank12',
                                      'Rank13','Rank14','Rank15', 'Rank16', 'Rank17', 'Rank18'))
df2$Rank <- as.numeric(df2$Rank)
df3 = na.omit(df2)
tiff("S5.tiff",width = 1600, height = 1200, res = 300)
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

##figure S6
library(Hmisc)
library(corrplot)
sl = read.csv("/home/user/MAGICAL/MAGICAL-CORE/biogrid-sl-pairwise-new-netprop")
sl$gi = "SL"
sv = read.csv("/home/user/MAGICAL/MAGICAL-CORE/biogrid-sv-pairwise-new-netprop")
sv$gi = "SV"
sl2 = read.csv("/home/user/MAGICAL/MAGICAL-CORE/cgidb-sl-pairwise-new-netprop")
sl2$gi = "SL"
sv2 = read.csv("/home/user/MAGICAL/MAGICAL-CORE/cgidb-sv-pairwise-new-netprop")
sv2$gi = "SV"
sl3 = read.csv("/home/user/MAGICAL/MAGICAL-CORE/sldb-sl-pairwise-new-netprop")
sl3$gi = "SL"
sv3 = read.csv("/home/user/MAGICAL/MAGICAL-CORE/sldb-sv-pairwise-new-netprop")
sv3$gi = "SV"
not = read.csv("/home/user/MAGICAL/MAGICAL-CORE/not0.pairwiseprop.csv")
not$gi = "NOT"
traindata = unique(rbind(sl,sl2,sl3,sv,sv2,sv3,not))
traindata = na.omit(traindata)
dummy = traindata[,-c(1,2,23)]
colnames(dummy)[1] = "Average Degree"
colnames(dummy)[2] = "Average Betweenness"
colnames(dummy)[3] = "Average Closeness"
colnames(dummy)[4] = "Average Coreness"
colnames(dummy)[5] = "Average Constraint"
colnames(dummy)[6] = "Average Eccentricity"
colnames(dummy)[7] = "Average Eigen-Centrality"
colnames(dummy)[8] = "Average Hub-Score"
colnames(dummy)[9] = "Average Neighbor1"
colnames(dummy)[10] = "Average Neighbor2"
colnames(dummy)[11] = "Average Neighbor3"
colnames(dummy)[12] = "Average Neighbor4"
colnames(dummy)[13] = "Average Neighbor5"
colnames(dummy)[14] = "Average Neighbor6"
colnames(dummy)[15] = "Average Triangle"
colnames(dummy)[16] = "Average Common-Neighbor"
colnames(dummy)[17] = "Average Community-Detection"
colnames(dummy)[18] = "Shortest-Path"
colnames(dummy)[19] = "Cohesion"
colnames(dummy)[20] = "Adhesion"
corr.data = dummy
cor_5 <- rcorr(as.matrix(corr.data))
M <- cor_5$r
p_mat <- cor_5$P
corrplot(M, type = "upper", order = "hclust",
         p.mat = p_mat, sig.level = 0.01)
#corr and p value for nodewise properties
#the below one is final
corrplot(M, type="upper", order="hclust",tl.cex = 0.8,insig = "blank", sig.level = 0.01)
corrplot(M, type="upper", order="hclust",tl.cex = 0.9)
tiff("S6.tiff",width = 1200, height = 1200, res = 300)
par(mar=c(0.5,2.5,0.5,0.5),cex.axis=1, font.axis=2,cex.lab=1.3, font.lab=2)
corrplot(M, type="upper", order="hclust",tl.cex = 0.6,insig = "blank", sig.level = 0.01,cl.cex = 0.5)
dev.off()

##figure S7
data = read.csv("/home/user/data2.csv")
data$Feature <- factor(data$Feature)
data %>%
  mutate(Feature =ordered(Feature, levels = unique(Feature))) %>%
  ggplot(aes(Feature,Accuracy,fill=Feature))+ theme_classic()+ 
  theme(axis.text.x = element_text(size = 10), axis.text.y = element_text(size = 10),
        text = element_text(size = 10))+
  ggtitle("")+ ylab("Accuracy")+ theme(legend.position="none")+ xlab("Features Drop")+
  geom_bar(stat="identity")+ theme(axis.text.x = element_text(angle = 45, hjust=1),axis.ticks.x = element_blank())+
  scale_fill_manual(values=c("#49006a","#7a0177","#ae017e","#dd3497","#f768a1"))

##figure S8
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
magical.try17 = randomForest(gi ~ . , data = training, importance = T,cutoff=c(0.1,0.1,0.1))
magical.try18 = randomForest(gi ~ . , data = training, importance = T,cutoff=c(0.2,0.2,0.2))
magical.try19 = randomForest(gi ~ . , data = training, importance = T,cutoff=c(0.3,0.3,0.3))
magical.try20 = randomForest(gi ~ . , data = training, importance = T,cutoff=c(0.4,0.3,0.1))
magical.try21 = randomForest(gi ~ . , data = training, importance = T,cutoff=c(0.5,0.4,0.1))
magical.try22 = randomForest(gi ~ . , data = training, importance = T,cutoff=c(0.6,0.3,0.1))
magical.try23 = randomForest(gi ~ . , data = training, importance = T,cutoff=c(0.7,0.2,0.1))
magical.try24 = randomForest(gi ~ . , data = training, importance = T,cutoff=c(0.8,0.1,0.1))
magical.try25 = randomForest(gi ~ . , data = training, importance = T,cutoff=c(0.4,0.4,0.2))
magical.try27 = randomForest(gi ~ . , data = training, importance = T,cutoff=c(0.1,0.2,0.3))
magical.try28 = randomForest(gi ~ . , data = training, importance = T,cutoff=c(0.9,0.05,0.05))
prediction = predict(magical.try28, newdata = testing[-6])
predict_gi = predict(magical.try28, newdata = testing)
testing$predict_gi = predict_gi
cnf_mat = table(testing$gi, testing$predict_gi)
cnf_mat
accuracy = sum(diag(cnf_mat)/sum(cnf_mat))
##for each of the modles thje ROC is generated
testing_set = testing[-7]
prediction = predict(magical.try28, newdata = testing_set[-6])
#library(pROC)
testing_set$gi = as.factor(testing_set$gi)
result <- pROC::multiclass.roc(as.numeric(prediction), 
                               as.numeric(testing_set$gi))
tiff("cutoff-0.9-0.05-0.05.tiff",width = 1200, height = 1200, res = 300)
par(mar=c(0.5,2.5,0.5,0.5),cex.axis=1, font.axis=1,cex.lab=1.1, font.lab=1.5)
plot.roc(result$rocs[[1]], 
         print.auc=T, col = "#969696", lwd = 5,
         legacy.axes = T, cex.lab = 2, cex.axis =2, plot = T, asp = NA)

##figure S9
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
rf1 = randomForest(gi ~ . , data = training, importance = T)
rf1 = randomForest(gi ~ . , data = training, importance = T, ntree=5000)
rf1 = randomForest(gi ~ . , data = training, importance = T, ntree = 500)
rf1 = randomForest(gi ~ . , data = training, importance = T, ntree = 200)
rf1 = randomForest(gi ~ . , data = training, importance = T, ntree = 200)
prediction = predict(rf1, newdata = testing[-6])
predict_gi = predict(rf1, newdata = testing)
testing$predict_gi = predict_gi
cnf_mat = table(testing$gi, testing$predict_gi)
cnf_mat
accuracy = sum(diag(cnf_mat)/sum(cnf_mat)) #80.413 with 500 trees 
##acc is 80.474 with 5000 trees
##acc is 80.264 with 200 trees
##acc is 80.202 with 100 trees
plot(rf1,log="x")
print(rf1)
