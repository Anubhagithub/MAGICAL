###map entrez ids to gene names database inp
##importing library
import pandas as pd
#merge.gene = pd.read_csv("/home/mito/srf2023/all-proteins/db-scores/magical-score/mapped-entrez-id-having-genename.csv")
#gene_merge= pd.read_csv("/home/mito/srf2023/all-proteins/db-scores/magical-score/mapped-entrez-id-having-genename.csv")
gene_merge= pd.read_csv("/home/user/MAGICAL/DATA/mapped-entrez-id-having-genename.csv")
data1 = gene_merge
###below data being huge can be either found in the supplementary data, or downloaded from database
pairs_db = pd.read_csv("/home/mito/srf2023/all-proteins/db-scores/magical-score/magical-db-input-except-not-final2.csv")
print(pairs_db.shape)
print(pairs_db.head())
pairs_db = pairs_db.drop_duplicates()
pairs_db = pairs_db.dropna()
print(pairs_db.shape)
data2 = pairs_db
data2 = data2.rename(columns={'gene1': 'V4'})
data2 = data2.rename(columns={'gene2': 'V2'})
print(data2.head())
data2 = data2.astype({'V4': str,'V2': str})
##id match using map.dict
data2['V2'] = data2['V4'].map(dict(zip(data1['V4'],data1['V2'])))
print(data2.head())
print(data2.shape)
data3 = pairs_db
data3 = data3.rename(columns={'gene2': 'V4'})
data3 = data3.rename(columns={'gene1': 'V2'})
print(data3.head())
print(data1.head())
data3 = data3.astype({'V4': str,'V2': str})
data3['V2'] = data3['V4'].map(dict(zip(data1['V4'],data1['V2'])))                                                                                   
print(data3.head())
print(data2.head())
print(pairs_db.head())
print(pairs_db.tail())
print(pairs_db.tail())
print(data2.tail())
print(data3.tail())
print(data2.shape)
print(data3.shape)
print(pairs_db.shape)
result = pd.concat([data2, data3], axis=1, join='inner')
print(result.shape)
result2 = result.drop_duplicates()
print(result2.shape)
result3 = result2.dropna()
print(result3.shape)
print(result3.head())
print(result3.tail())
result4 = result3.iloc[:,[1,4,2,3]]
result4.to_csv("/home/mito/srf2023/all-proteins/db-scores/magical-score")       
print(result4.head())
##Save the mapped file
result4.to_csv("/home/mito/srf2023/all-proteins/db-scores/magical-score/pairs-db-input-genename.csv")

