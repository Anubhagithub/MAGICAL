###MAGICAL-CORE
setwd("/home/user/MAGICAL/MAGICAL-CORE")
#sl = read.csv("/home/nikola/biogrid-new-ppi/new-entrez/biogrid-sl-pairwise-new-netprop")
sl = read.csv("biogrid-sl-pairwise-new-netprop")
sl$gi = "SL"
#sv = read.csv("/home/nikola/biogrid-new-ppi/new-entrez/biogrid-sv-pairwise-new-netprop")
sv = read.csv("biogrid-sv-pairwise-new-netprop")
sv$gi = "SV"
#sl2 = read.csv("/home/nikola/biogrid-new-ppi/new-entrez/cgidb-sl-pairwise-new-netprop")
sl2 = read.csv("cgidb-sl-pairwise-new-netprop")
sl2$gi = "SL"
#sv2 = read.csv("/home/nikola/biogrid-new-ppi/new-entrez/cgidb-sv-pairwise-new-netprop")
sv2 = read.csv("cgidb-sv-pairwise-new-netprop")
sv2$gi = "SV"
#sl3 = read.csv("/home/nikola/biogrid-new-ppi/new-entrez/sldb-sl-pairwise-new-netprop")
sl3 = read.csv("sldb-sl-pairwise-new-netprop")
sl3$gi = "SL"
#sv3 = read.csv("/home/nikola/biogrid-new-ppi/new-entrez/sldb-sv-pairwise-new-netprop")
sv3 = read.csv("sldb-sv-pairwise-new-netprop")
sv3$gi = "SV"
#not = read.csv("/home/nikola/biogrid-new-ppi/new-entrez/not0.pairwiseprop.csv")
not = read.csv("not0.pairwiseprop.csv")
not$gi = "NOT"
traindata = unique(rbind(sl,sl2,sl3,sv,sv2,sv3,not))
table(traindata$gi)
traindata$gi = as.factor(traindata$gi)
traindatabal = DMwR::SMOTE(gi ~ ., traindata, perc.under = 200)
traindatabal$gene1 = as.integer(traindatabal$gene1)
traindatabal$gene2 = as.integer(traindatabal$gene2)
write.csv(traindatabal, "magical-core-data.csv", row.names = F, quote = F)
#training = traindatabal
traindatabal = read.csv("magical-core-data.csv")
traindata$gi = as.factor(traindata$gi)
traindatabal$gene1 = as.integer(traindatabal$gene1)
traindatabal$gene2 = as.integer(traindatabal$gene2)
index = sample(2, nrow(traindatabal), replace = TRUE, prob = c(0.70, 0.30))
train = traindatabal[index == 1,]
test = traindatabal[index == 2,]
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
saveRDS(magical, "magical-core.rds")
