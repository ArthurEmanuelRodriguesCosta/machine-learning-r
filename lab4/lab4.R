library(recommenderlab)
library(dplyr)

data.wide <- read.csv("data.csv")
data.long <- read.csv("data2.csv")

teste <- data.wide %>% filter((ALU_ANO_INGRESSO > 2013 | (ALU_ANO_INGRESSO == 2013 & ALU_PERIODO_INGRESSO == 2)))
treino <- data.wide %>% filter(ALU_ANO_INGRESSO < 2013 | (ALU_ANO_INGRESSO == 2013 & ALU_PERIODO_INGRESSO == 1))

treino <- treino %>% select(-MAT_NOVA_MATRICULA, -ALU_ANO_INGRESSO, -ALU_PERIODO_INGRESSO)
teste <- teste %>% select(-MAT_NOVA_MATRICULA, -ALU_ANO_INGRESSO, -ALU_PERIODO_INGRESSO)

treino <- as.matrix(treino)
teste <- as.matrix(teste)

p.treino <- as(treino, "realRatingMatrix")
p.teste <- as(teste, "realRatingMatrix")

rec=Recommender(p.treino,method="UBCF", param=list(normalize = "Z-score",method="Cosine",nn=5, minRating=1))
rec

print(rec)
names(getModel(rec))
getModel(rec)$nn

recom <- predict(rec, p.teste, type="ratings")
recom

error.ubcf<-calcPredictionAccuracy(p.ubcf)
