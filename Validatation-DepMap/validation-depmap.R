#################################DEPMAP data#######################################
###Validating pairs from DepMap and CRISPR data
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
test.depmap = test.depmap[,-c(1,2)]
training = train.magical
testing = test.depmap[,c(2,10,15,18,20,21)]
model.depmap = randomForest(gi ~ . , data = training, importance = T)
prediction = predict(model.depmap, newdata = testing[-6])
predict_gi = predict(model.depmap, newdata = testing)
testing$predict_gi = predict_gi
cnf_mat = table(testing$gi, testing$predict_gi)
cnf_mat
accuracy = sum(diag(cnf_mat)/sum(cnf_mat)) 
