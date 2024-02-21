###MAGICAL-COMBINED
setwd("/home/user/MAGICAL/MAGICAL-combined")
#sl = read.csv("/home/nikola/string-full-nocuff/biogrid-sl-pairwise-new-netprop")
sl = read.csv("biogrid-sl-pairwise-new-netprop")
sl$gi = "SL"
#sv = read.csv("/home/nikola/string-full-nocuff/biogrid-sv-pairwise-new-netprop")
sv = read.csv("biogrid-sv-pairwise-new-netprop")
sv$gi = "SV"
#sl2 = read.csv("/home/nikola/string-full-nocuff/cgidb-sl-pairwise-new-netprop")
sl2 = read.csv("cgidb-sl-pairwise-new-netprop")
sl2$gi = "SL"
#sv2 = read.csv("/home/nikola/string-full-nocuff/cgidb-sv-pairwise-new-netprop")
sv2 = read.csv("cgidb-sv-pairwise-new-netprop")
sv2$gi = "SV"
#sl3 = read.csv("/home/nikola/string-full-nocuff/sldb-sl-pairwise-new-netprop")
sl3 = read.csv("sldb-sl-pairwise-new-netprop")
sl3$gi = "SL"
#sv3 = read.csv("/home/nikola/string-full-nocuff/sldb-sv-pairwise-new-netprop")
sv3 = read.csv("sldb-sv-pairwise-new-netprop")
sv3$gi = "SV"
#not = read.csv("/home/nikola/string-full-nocuff/not-pairwise-new-netprop") 
not = read.csv("not-pairwise-new-netprop") 
not$gi = "NOT"
traindata = unique(rbind(sl,sl2,sl3,sv,sv2,sv3,not))
table(traindata$gi)
traindata$gi = as.factor(traindata$gi)
traindata$gene1 = as.factor(traindata$gene1)
traindata$gene2 = as.factor(traindata$gene2)
traindatabal = DMwR::SMOTE(gi ~ ., traindata, perc.under = 200)
#traindatabal = DMwR::SMOTE(gi ~ ., traindata[,-c(1,2)], perc.under = 200)
table(traindatabal$gi)
write.csv(traindatabal, "magical-combined-data.csv", row.names = F, quote = F)
#training = traindatabal
traindatabal = read.csv("magical-combined-data.csv")
#traindatabal$gene1 = as.factor(traindatabal$gene1)
#traindatabal$gene2 = as.factor(traindatabal$gene2)
traindatabal$gi = as.factor(traindatabal$gi)
traindatabal = traindatabal[,-c(1,2)]
index = sample(2, nrow(traindatabal), replace = TRUE, prob = c(0.70, 0.30))
train = traindatabal[index == 1,]
test = traindatabal[index == 2,]
training = train[,c(2,10,15,18,20,21)]
testing = test[,c(2,10,15,18,20,21)]
set.seed(123)
model.pred = randomForest(gi ~ . , data = training, importance = T)
prediction = predict(model.pred, newdata = testing[-6])
predict_gi = predict(model.pred, newdata = testing)
testing$predict_gi = predict_gi
cnf_mat = table(testing$gi, testing$predict_gi)
cnf_mat
accuracy = sum(diag(cnf_mat)/sum(cnf_mat))
saveRDS(model.pred, "magical-combined.rds")
