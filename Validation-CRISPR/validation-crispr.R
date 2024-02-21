###################################################################################
######## crispr data model run ##############################################3
sl2 = read.csv("/home/nikola/biogrid-new-ppi/new-entrez/crispr-sl-pairwise-new-entrez-final")
sv2 = read.csv("/home/nikola/biogrid-new-ppi/new-entrez/crispr-sv-pairwise-new-entrez-final")
sl2$gi = "SL"
sv2$gi = "SV"
crispr.sl = read.csv("/home/nikola/biogrid-new-ppi/new-entrez/crispr-sl-pairwise-new-entrez-final")
crispr.sv = read.csv("/home/nikola/biogrid-new-ppi/new-entrez/crispr-sv-pairwise-new-entrez-final")
crispr.sl$gi =  "SL"
crispr.sv$gi = "SV"
not2 = read.csv("/home/nikola/biogrid-new-ppi/new-entrez/not1.pairwiseprop.csv")
not2$gi = "NOT"
crispr.tot2 = unique(rbind(crispr.sl,crispr.sv,not2))
testing = crispr.tot2[,-c(1,2)]
testing = testing[,c(2,12,15,18,20,21)]
training = train.magical[,c(2,12,15,18,20,21)]
#training = read.csv("/home/nikola/new_netprop/newdata.bal.train.csv")
training$gi = as.factor(training$gi)
magical = randomForest(gi ~ . , data = training, importance = T)
prediction = predict(magical, newdata = testing[-10])
predict_gi = predict(magical, newdata = testing)
testing$predict_gi = predict_gi
cnf_mat = table(testing$gi, testing$predict_gi)
cnf_mat
accuracy = sum(diag(cnf_mat)/sum(cnf_mat))
###crispr magical overlap
magical = readRDS("magical.final.rds")
prediction = predict(magical, newdata = testing[-6])
predict_gi = predict(magical, newdata = testing)
testing$predict_gi = predict_gi
cnf_mat = table(testing$gi, testing$predict_gi)
cnf_mat
accuracy = sum(diag(cnf_mat)/sum(cnf_mat))
data2 = data[c(1,2,3,5,13,18,21,23,24)] 
pre = predict(magical, newdata = data2[,-c(1,2,3)])
pred_gi = predict(magical, newdata = data2[,-c(1,2,3)])
data2$pred_gi = pred_gi
data3 = data2
data3.sl = data3[which(data3$gi == "SL"),]
overlap = data3.sl[which(data3.sl$gi == data3.sl$pred_gi),] 
crispr = data3.sl[which(data3.sl$gi != data3.sl$pred_gi),] 
magical.sl = data3[which(data3$pred_gi == "SL"),] 
magicalsl = magical.sl[which(magical.sl$gi != magical.sl$pred_gi),] 
pred <- predict(magical,data2[,-c(1,2,3,9,10)],"prob")
pred = as.data.frame(pred)
database = data2
database = cbind(database,pred)
database$max = pmax(database$NOT, database$SL, database$SV)
database = database[,c(1,2,10,14)]
print(head(database))
######for sv
sl2 = read.csv("/home/nikola/biogrid-new-ppi/new-entrez/crispr-sl-pairwise-new-entrez-final")
sv2 = read.csv("/home/nikola/biogrid-new-ppi/new-entrez/crispr-sv-pairwise-new-entrez-final")
sl2$gi = "SL"
sv2$gi = "SV"
data = unique(rbind(sl2,sv2))
data2 = data[c(1,2,3,5,13,18,21,23,24)]
pre = predict(magical, newdata = data2[,-c(1,2,3)])
pred_gi = predict(magical, newdata = data2[,-c(1,2,3)])
data2$pred_gi = pred_gi
data3 = data2
data3.sv = data3[which(data3$gi == "SV"),]
overlapsv = data3.sv[which(data3.sv$gi == data3.sv$pred_gi),] 
crisprsv = data3.sv[which(data3.sv$gi != data3.sv$pred_gi),] 
magical.sv = data3[which(data3$pred_gi == "SV"),] 
magicalsv = magical.sv[which(magical.sv$gi != magical.sv$pred_gi),]
