library(dplyr)
library(caret)

data1 <- read.csv("treino.csv")

data <- data1 %>% filter(MAT_TUR_ANO > 2010)

data <- data1 %>% group_by(MAT_ALU_MATRICULA, MAT_TUR_ANO, MAT_TUR_PERIODO) %>% summarise(media = mean(MAT_MEDIA_FINAL, na.rm = T), evadiu = names(table(EVADIU)))
 
data <- data %>% mutate(media = ifelse(is.na(media), 10, media))

training <- data %>% filter(MAT_TUR_ANO <= 2014 | MAT_TUR_ANO == 2015 & MAT_TUR_PERIODO == 1 )
testing <- data %>% filter(MAT_TUR_ANO == 2015 & MAT_TUR_PERIODO == 2)

caret_model <- train(evadiu ~ media,  data=training, method="glm", family="binomial")

caret.probs <- predict(caret_model, newdata=testing, type="prob")

summary(caret_model)

caret.results <- predict(caret_model,newdata=testing)
confusionMatrix(data=caret.results, reference=testing$evadiu)
