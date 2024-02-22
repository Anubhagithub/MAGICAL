#slant on bal data
control <- trainControl(method="cv", summaryFunction=twoClassSummary, classProbs=T,
                        savePredictions = T)
#sl = read.csv("/home/nikola/biogrid-new-ppi/new-entrez/biogrid-sl-pairwise-new-netprop")
sl = read.csv("/home/user/MAGICAL/MAGICAL-CORE/biogrid-sl-pairwise-new-netprop")
sl$gi = "SL"
sl = sl[1:411,]
#sv = read.csv("/home/nikola/biogrid-new-ppi/new-entrez/biogrid-sv-pairwise-new-netprop")
sv = read.csv("/home/user/MAGICAL/MAGICAL-CORE/biogrid-sv-pairwise-new-netprop")
sv$gi = "NOT"
sv = sv[1:205,]
#not = read.csv("/home/nikola/biogrid-new-ppi/new-entrez/not0.pairwiseprop.csv")
not = read.csv("/home/user/MAGICAL/MAGICAL-CORE/not0.pairwiseprop.csv")
not$gi = "NOT"
not = not[1:206,]
traindata = unique(rbind(sl,sv,not))
table(traindata$gi)
traindata$gi = as.factor(traindata$gi)
traindata = na.omit(traindata)
traindatabal = traindata
table(traindatabal$gi)
#traindatabalcp = traindata[,c(1,2,6,21,22,23)]
traindatabalcp = traindatabal 
ind = sample(2, nrow(traindatabalcp[,-c(1,2)]), replace = TRUE, prob = c(0.70, 0.30))
tr2 = traindatabalcp[ind == 1,]
te2 = traindatabalcp[ind == 2,]
slant.bal.train = tr2[,c(1,2,6,21,22,23)]
slant.bal.test = te2[,c(1,2,6,21,22,23)]
slant.bal <- train(gi~., data=select(slant.bal.train, -gene1, -gene2), method='ranger', trControl=control, tuneLength=5, metric="ROC", preProc=c("center", "scale"))
prediction = predict(slant.bal, newdata = slant.bal.test[,-c(1,2)])
predict_gi = predict(slant.bal, newdata = slant.bal.test[,-c(1,2)])
slant.bal.test$predict_gi = predict_gi
cnf_mat = table(slant.bal.test$gi, slant.bal.test$predict_gi)
cnf_mat
accuracy.slant.bal = sum(diag(cnf_mat)/sum(cnf_mat))
test.slant = slant.bal.test 
prediction.slant = predict(slant.bal, newdata = test.slant[-c(6,7)])
test.slant$gi = as.factor(test.slant$gi)
result.slant <- pROC::roc(as.numeric(prediction.slant), 
                           as.numeric(test.slant$gi))
###magical on bal data
#sl = read.csv("/home/nikola/biogrid-new-ppi/new-entrez/biogrid-sl-pairwise-new-netprop")
sl = read.csv("/home/user/MAGICAL/MAGICAL-CORE/biogrid-sl-pairwise-new-netprop")
sl$gi = "SL"
#sv = read.csv("/home/nikola/biogrid-new-ppi/new-entrez/biogrid-sv-pairwise-new-netprop")
sv = read.csv("/home/user/MAGICAL/MAGICAL-CORE/biogrid-sv-pairwise-new-netprop")
sv$gi = "SV"
#not = read.csv("/home/nikola/biogrid-new-ppi/new-entrez/not0.pairwiseprop.csv")
not = read.csv("/home/user/MAGICAL/MAGICAL-CORE/not0.pairwiseprop.csv")
not$gi = "NOT"
traindata = unique(rbind(sl,sv,not))
table(traindata$gi)
traindata$gi = as.factor(traindata$gi)
traindata = na.omit(traindata)
traindatabal = DMwR::SMOTE(gi ~ ., traindata[,-c(1,2)], perc.under = 200)
index = sample(2, nrow(traindatabal[,-c(1,2)]), replace = TRUE, prob = c(0.70, 0.30))
train = traindatabal[index == 1,]
test = traindatabal[index == 2,]
training = train[,c(2,10,15,18,20,21)]
testing = test[,c(2,10,15,18,20,21)]
magical = randomForest(gi ~ . , data = training, importance = T)
prediction = predict(magical, newdata = testing[-c(6)])
predict_gi = predict(magical, newdata = testing)
testing$predict_gi = predict_gi
cnf_mat = table(testing$gi, testing$predict_gi)
cnf_mat
accuracy = sum(diag(cnf_mat)/sum(cnf_mat))
test.mag = testing
prediction.mag = predict(magical, newdata = test.mag[-c(6,7)])
test.mag$gi = as.factor(test.mag$gi)
result.mag <- pROC::multiclass.roc(as.numeric(prediction.mag), 
                                       as.numeric(test.mag$gi))
tiff("magical-slant-bal.tiff",width = 1600, height = 1400, res = 300)
par(mar=c(0.5,2.5,0.5,0.5),cex.axis=1, font.axis=1,cex.lab=1.1, font.lab=1.5)
plot.roc(result.mag$rocs[[2]], 
         print.auc=T, col = '#980043', lwd = 4, print.auc.x=0.2,print.auc.y=0.7,
         legacy.axes = T, cex.lab = 2, cex.axis =2, plot = T, asp = NA)
plot.roc(result.slant,add=T, col = "black", ##F8766D 3lwd = 5, print.auc.x=0.8,print.auc.y=0.33,
         print.auc=T,cex.lab = 2, cex.axis =2, lty = 1,
         legacy.axes = T, lwd = 3, print.auc.x=0.2,print.auc.y=0.7,
         print.auc.adj = c(0,5),scales=list(x=list(font=2,cex=5)))
legend("bottomright", 
       legend = c("MAGICAL-SL", "SLANT-SL"), 
       col = c("#980043","black"), #969696 gray color
       lty = c(1,1),
       lwd = c(4,3))
dev.off()
###slant on unbal data
control <- trainControl(method="cv", summaryFunction=twoClassSummary, classProbs=T,
                        savePredictions = T)
#sl = read.csv("/home/nikola/biogrid-new-ppi/new-entrez/biogrid-sl-pairwise-new-netprop")
sl = read.csv("/home/user/MAGICAL/MAGICAL-CORE/biogrid-sl-pairwise-new-netprop")
sl$gi = "SL"
#sv = read.csv("/home/nikola/biogrid-new-ppi/new-entrez/biogrid-sv-pairwise-new-netprop")
sv = read.csv("/home/user/MAGICAL/MAGICAL-CORE/biogrid-sv-pairwise-new-netprop")
sv$gi = "NOT"
#not = read.csv("/home/nikola/biogrid-new-ppi/new-entrez/not0.pairwiseprop.csv")
not = read.csv("/home/user/MAGICAL/MAGICAL-CORE/not0.pairwiseprop.csv")
not$gi = "NOT"
traindata = unique(rbind(sl,sv,not))
table(traindata$gi)
traindata$gi = as.factor(traindata$gi)
traindata = na.omit(traindata)
ind = sample(2, nrow(traindata[,-c(1,2)]), replace = TRUE, prob = c(0.70, 0.30))
tr2 = traindata[ind == 1,]
te2 = traindata[ind == 2,]
slant.unbal.train = tr2[,c(1,2,6,21,22,23)]
slant.unbal.test = te2[,c(1,2,6,21,22,23)]
slant.unbal <- train(gi~., data=select(slant.unbal.train, -gene1, -gene2), method='ranger', trControl=control, tuneLength=5, metric="ROC", preProc=c("center", "scale"))
prediction.unbal = predict(slant.unbal, newdata = slant.unbal.test[,-c(1,2)])
predict_gi = predict(slant.unbal, newdata = slant.unbal.test[,-c(1,2)])
slant.unbal.test$predict_gi = predict_gi
cnf_mat = table(slant.unbal.test$gi, slant.unbal.test$predict_gi)
cnf_mat
accuracy.slant.unbal = sum(diag(cnf_mat)/sum(cnf_mat))
test.slant = slant.unbal.test 
prediction.slant = predict(slant.unbal, newdata = test.slant[-c(6,7)])
test.slant$gi = as.factor(test.slant$gi)
result.slant <- pROC::roc(as.numeric(prediction.slant), 
                          as.numeric(test.slant$gi))
###magical on unbal data
#sl = read.csv("/home/nikola/biogrid-new-ppi/new-entrez/biogrid-sl-pairwise-new-netprop")
sl = read.csv("/home/user/MAGICAL/MAGICAL-CORE/biogrid-sl-pairwise-new-netprop")
sl$gi = "SL"
#sv = read.csv("/home/nikola/biogrid-new-ppi/new-entrez/biogrid-sv-pairwise-new-netprop")
sv = read.csv("/home/user/MAGICAL/MAGICAL-CORE/biogrid-sv-pairwise-new-netprop")
sv$gi = "SV"
#sl2 = read.csv("/home/nikola/biogrid-new-ppi/new-entrez/cgidb-sl-pairwise-new-netprop")
sl2 = read.csv("/home/user/MAGICAL/MAGICAL-CORE/cgidb-sl-pairwise-new-netprop")
sl2$gi = "SL"
#sv2 = read.csv("/home/nikola/biogrid-new-ppi/new-entrez/cgidb-sv-pairwise-new-netprop")
sv2 = read.csv("/home/user/MAGICAL/MAGICAL-CORE/cgidb-sv-pairwise-new-netprop")
sv2$gi = "SV"
#sl3 = read.csv("/home/nikola/biogrid-new-ppi/new-entrez/sldb-sl-pairwise-new-netprop")
sl3 = read.csv("/home/user/MAGICAL/MAGICAL-CORE/sldb-sl-pairwise-new-netprop")
sl3$gi = "SL"
#sv3 = read.csv("/home/nikola/biogrid-new-ppi/new-entrez/sldb-sv-pairwise-new-netprop")
sv3 = read.csv("/home/user/MAGICAL/MAGICAL-CORE/sldb-sv-pairwise-new-netprop")
sv3$gi = "SV"
#not = read.csv("/home/nikola/biogrid-new-ppi/new-entrez/not0.pairwiseprop.csv")
not = read.csv("/home/user/MAGICAL/MAGICAL-CORE/not0.pairwiseprop.csv")
not$gi = "NOT"
traindata = unique(rbind(sl,sl2,sl3,sv,sv2,sv3,not))
table(traindata$gi)
traindata$gi = as.factor(traindata$gi)
traindata = traindata[,-c(1,2)]
index = sample(2, nrow(traindata), replace = TRUE, prob = c(0.70, 0.30))
train = traindata[index == 1,]
test = traindata[index == 2,]
train.magical = train[,c(2,10,15,18,20,21)]
test.magical = test[,c(2,10,15,18,20,21)]
training = train.magical
testing = test.magical
library(randomForest)
magical = randomForest(gi ~ . , data = training, importance = T)
prediction = predict(magical, newdata = testing[-6])
predict_gi = predict(magical, newdata = testing)
testing$predict_gi = predict_gi
cnf_mat = table(testing$gi, testing$predict_gi)
cnf_mat
accuracy = sum(diag(cnf_mat)/sum(cnf_mat))
test.mag = testing
prediction.mag = predict(magical, newdata = test.mag[-c(6,7)])
test.mag$gi = as.factor(test.mag$gi)
result.mag <- pROC::multiclass.roc(as.numeric(prediction.mag), 
                                   as.numeric(test.mag$gi))
tiff("magical-slant-unbal.tiff",width = 1600, height = 1400, res = 300)
par(mar=c(0.5,2.5,0.5,0.5),cex.axis=1, font.axis=1,cex.lab=1.1, font.lab=1.5)
plot.roc(result.mag$rocs[[2]], 
         print.auc=T, col = '#980043', lwd = 4, print.auc.x=0.2,print.auc.y=0.7,
         legacy.axes = T, cex.lab = 2, cex.axis =2, plot = T, asp = NA)
plot.roc(result.slant,add=T, col = "black", ##F8766D 3lwd = 5, print.auc.x=0.8,print.auc.y=0.33,
         print.auc=T,cex.lab = 2, cex.axis =2, lty = 1,
         legacy.axes = T, lwd = 3, print.auc.x=0.2,print.auc.y=0.7,
         print.auc.adj = c(0,5),scales=list(x=list(font=2,cex=5)))
legend("bottomright", 
       legend = c("MAGICAL-SL", "SLANT-SL"), 
       col = c("#980043","black"), #969696 gray color
       lty = c(1,1),
       lwd = c(4,3))
dev.off()
