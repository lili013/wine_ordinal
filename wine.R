# Getting Started ---------------------------------------------------------------------

#loading packages
Packages <-
  c(
    "tidyverse",
    "DataExplorer",
    "dplyr",
    "caret",
    "bootStepAIC",
    "glmnet",
    "MASS" ,
    "nnet",
    "ggplot2",
    "randomForest"
  ) 
lapply(Packages, library, character.only = TRUE)

#loading data
mydata <- read_delim("winequality-white.csv", ";")
names(mydata) <- gsub(" ", "", names(mydata))

# preparing data
pie_before_mapping <- ggplot(mydata, aes(x = "", fill = factor(quality))) + 
  geom_bar(width = 1) +
  coord_polar(theta = "y", start=0)+
  theme(axis.line = element_blank(), 
        plot.title = element_text(hjust=0.5),
        axis.text = element_blank()) + 
        scale_fill_brewer(palette = "Set3") +
  labs(fill="Quality", 
       x=NULL, 
       y=NULL, 
       title="Target Class Proportion", 
       caption="The classes of quality are not evenly distributed.")
pie_before_mapping

# Simplifying dataset: remapping classes of target variable
mydata$quality[mydata$quality <= 5] <- 3
mydata$quality[mydata$quality == 6] <- 2
mydata$quality[mydata$quality >= 7] <- 1

pie_after_mapping <- ggplot(mydata, aes(x = "", fill = factor(quality))) + 
  geom_bar(width = 1) +
  coord_polar("y", start=0) +
  theme(axis.line = element_blank(), 
        plot.title = element_text(hjust=0.5),
        axis.text = element_blank()) + 
  scale_fill_brewer(palette = "Set3") +
  labs(fill="Quality", 
       x=NULL, 
       y=NULL, 
       title="Target Class Proportion", 
       caption="The proportion classes of quality after remapping.")
pie_after_mapping

# Sampling ---------------------------------------------------------------------
set.seed(1002234552)

# superficially correcting for the unbalanced dataset
df <- mydata %>%
  group_by(quality) %>%
  sample_n(1000)

# creating datasets 80/30 
samplesize = 0.80*nrow(df)
index = sample(seq_len(nrow(df)), size = samplesize)

#Creating training and test set 
datatrain = df[index,] 
datavalidation = df[-index,]


## PUSHDOWN ON LINE 119
# #changing y in an ordered factor
# datatrain$quality <- factor(datatrain$quality,
#                       order = TRUE,
#                       levels = c("3", "2", "1"))
# 
# datavalidation$quality <- factor(datavalidation$quality,
#                            order = TRUE,
#                            levels = c("3", "2", "1"))


#creation de matrice des x pour datatrain et datavalidation
allxtrain = as.matrix(datatrain[1:11]) 
allxvalidation = as.matrix(datavalidation[1:11])
ytrain = datatrain$quality
ytest = datavalidation$quality


# EDA ---------------------------------------------------------------------

#boxplot
plot_boxplot(datatrain, by = 'quality', ncol = 2)

#On remarque que les variables freesulfurdioxide, totalsulfurdioxide
#et residualsugar ne sont pas sur la même échelle que les autres variables.

#De plus, seulement la varariable alcohol et density semble
#différencier le y.

#correlation
plot_correlation(mydata, cor_args = list( 'use' = 'complete.obs'))

#Une tres forte cor. positive (0.85) entre density et residual sugar 
#Une forte cor. négative (0.76) entre density et alcohol
#Une cor. positive (0.62) entre freesulfurdioxide et totalsulfurdioxide 
#Une cor. positive (0.52) entre density et totalsulfurdioxide


# # Feature Transformation ----------------------------------------------------------------

allxtrain = as.matrix(datatrain[1:11])
allxvalidation = as.matrix(datavalidation[1:11])
ytrain = datatrain$quality
yvalidation = datavalidation$quality

ntrain=nrow(datatrain)
ntest=nrow(datavalidation)

#transformer au carré, log et toutes les interactions (two-way) 
allx=data.frame(rbind(allxtrain,allxvalidation))

# interactions
allxi = data.frame(model.matrix(
  ~ (
    fixedacidity + volatileacidity + citricacid
    + residualsugar + chlorides + freesulfurdioxide
    + totalsulfurdioxide + density + pH + sulphates
    + alcohol)^2 - 1,
  allx
))
# power transformation
allxsq=data.frame(allx^2)
names(allxsq)=paste(names(allxsq),"sq", sep="")

#lof transformation
allxlog = data.frame(log(allx))
names(allxlog)=paste(names(allxlog),"log", sep="")
allxlog$citricacidlog <- NULL

allx= as.data.frame(cbind(allxi, allxlog)) # not using allxsq
allxtrain = allx[1:ntrain,]
allxvalidation = allx[ntrain + 1:ntest,]

#run correlations
cor_mat <- cor(allxtrain)
#the index of the columns to be removed because they have a high correlation
index <- findCorrelation(cor_mat, .75)
to_be_removed <- colnames(cor_mat)[index]
allxtrain<-allxtrain[!names(allxtrain) %in% to_be_removed]
allxvalidation<-allxvalidation[!names(allxvalidation) %in% to_be_removed]

#scaling
allxtrain<- scale(allxtrain)
allxvalidation<- scale(allxvalidation)

# put it all together
datatrain= as.data.frame(cbind(allxtrain, ytrain))
datavalidation= as.data.frame(cbind(allxvalidation, yvalidation))

#changing y in an ordered factor
datatrain$ytrain <- factor(datatrain$ytrain,
                            order = TRUE,
                            levels = c("3", "2", "1"))
datavalidation$yvalidation <- factor(datavalidation$yvalidation,
                                 order = TRUE,
                                 levels = c("3", "2", "1"))

# Multinominal ------------------------------------------------------------

full.multinominal <- multinom(ytrain~(.), data = datatrain) 
step.multinominal<-stepAIC(full.multinominal, direction = "both", trace = FALSE)

predMultinom <- predict(step.multinominal, datavalidation)
nbMultinom <-length(step.multinominal$coefnames)-1
mean(predMultinom == factor(datavalidation$yvalidation, ordered = F))
referencey <- datavalidation$yvalidation
confMulti <- confusionMatrix(referencey, predMultinom) 
confMultiClass <- confMulti$byClass

#POLR --------------------------------------------------------------------

#modele simple
full.polr <- polr(ytrain~(.), data = datatrain, Hess=TRUE) 
step.polr <- stepAIC(full.polr, direction = "both", trace = FALSE)

predpolr <- predict(step.polr, datavalidation) 
nbpolr <-length(step.polr$coefnames)-1
mean(predpolr == factor(datavalidation$yvalidation, ordered = F))
confPolr <- confusionMatrix(referencey, predpolr) 
confPolrClass <- confPolr$byClass


#Lasso ----------------------------------------------------------------------
cvlasso = cv.glmnet(
  allxtrain,
  datatrain$ytrain,
  alpha = 1,
  family = "multinomial",
  type.multinomial = "grouped"
)

plot(cvlasso)
bestlasso <- cvlasso$lambda.min
predlasso = predict(cvlasso, new= allxvalidation, s= bestlasso, type = "class") 

#Elastic Net ----------------------------------------------------------------------
#alpha .5
cvelastic = cv.glmnet(
  allxtrain,
  datatrain$ytrain,
  alpha = .5,
  family = "multinomial",
  type.multinomial = "grouped")

plot(cvelastic)
bestelastic <- cvelastic$lambda.min
predelastic = predict(cvelastic, new= allxvalidation, s= bestelastic, type = "class") 
predict(cvelastic, s = bestelastic, type = "coefficients")

#Ridge ----------------------------------------------------------------------
cvridge = cv.glmnet(
  allxtrain,
  datatrain$ytrain,
  alpha = 0,
  family = "multinomial",
  type.multinomial = "grouped",
  nfolds = 30)

plot(cvridge)
bestridge <- cvridge$lambda.min
predridge = predict(cvridge, new= allxvalidation, s= bestridge, type = "class")

predridge <- factor(predridge, order = TRUE,
                    levels = c("3", "2", "1"))
predlasso <- factor(predlasso, order = TRUE,
                    levels = c("3", "2", "1"))
predelastic <- factor(predelastic, order = TRUE,
                      levels = c("3", "2", "1"))

conflasso <- confusionMatrix(referencey, predlasso) 
conflassoClass <- conflasso$byClass
confelas <- confusionMatrix(referencey, predelastic) 
confelasClass <- confelas$byClass
confridge <- confusionMatrix(referencey, predridge) 
confridgeClass <- confridge$byClass

nbVariable_cvlasso <- NULL
nbVariable_cvelastic<- NULL
nbVariable_cvridge<- NULL

co=coef(cvlasso, s=0)
x <- as.vector(co[[1]])
nbVariable_cvlasso$class1 <- sum(ifelse(x > 0, 1, 0)) 
co=coef(cvelastic, s=0)
x <- as.vector(co[[1]])
nbVariable_cvelastic$class1 <- sum(ifelse(x > 0, 1, 0)) 
co=coef(cvridge, s=0)
x <- as.vector(co[[1]])
nbVariable_cvridge$class1 <- sum(ifelse(x > 0, 1, 0))
co=coef(cvlasso, s=0)
x <- as.vector(co[[1]])
nbVariable_cvlasso$class2 <- sum(ifelse(x > 0, 1, 0))
co=coef(cvelastic, s=0)
x <- as.vector(co[[2]])
nbVariable_cvelastic$class2 <- sum(ifelse(x > 0, 1, 0)) 
co=coef(cvridge, s=0)
x <- as.vector(co[[2]])
nbVariable_cvridge$class2 <- sum(ifelse(x > 0, 1, 0))
co=coef(cvlasso, s=0)
x <- as.vector(co[[3]])
nbVariable_cvlasso$class3 <- sum(ifelse(x > 0, 1, 0)) 
co=coef(cvelastic, s=0)
x <- as.vector(co[[3]])
nbVariable_cvelastic$class3 <- sum(ifelse(x > 0, 1, 0))
co=coef(cvridge, s=0)
x <- as.vector(co[[3]])
nbVariable_cvridge$class3 <- sum(ifelse(x > 0, 1, 0))

x <- (as.data.frame(nbVariable_cvelastic))
nbAvgElas <- round((x$class1[1] + x$class2[1] + x$class3[1])/3) 
x <- (as.data.frame(nbVariable_cvridge))
nbAvgRid <- round((x$class1[1] + x$class2[1] + x$class3[1])/3) 
x <- (as.data.frame(nbVariable_cvlasso))
nbAvgLas <- round((x$class1[1] + x$class2[1] + x$class3[1])/3)


# Random Forest: Tuning mtry ----------------------------------------------------------- 
#tune mtry & ntree

# mtry: Number of variables randomly sampled as candidates at each split. 
# ntree: Number of trees to grow.

#default
control <- trainControl(method="repeatedcv", number=10, repeats=5)
metric <- "Accuracy"
mtry <- sqrt(ncol(x))
tunegrid <- expand.grid(.mtry=mtry)

rf_default <-
  train(
    ytrain ~ .,
    data = datatrain,
    method = "rf",
    metric = metric,
    tuneGrid = tunegrid,
    trControl = control)
print(rf_default)

# Random Search
control <-trainControl(method = "repeatedcv", number = 10, repeats = 5, search = "random")

rf_random <-
  train(
    ytrain ~ .,
    data = datatrain,
    method = "rf",
    metric = metric,
    tuneLength = 6,
    trControl = control) 

print(rf_random)
table1 <- cbind(models ="rf_random", rf_random$bestTune) 

#grid search
control <- trainControl(method = "repeatedcv", number = 10, repeats = 5, search = "grid")
tunegrid <- expand.grid(.mtry = c(1:15))

rf_gridsearch <-
  train(
    ytrain ~ .,
    data = datatrain,
    method = "rf",
    metric = metric,
    tuneGrid = tunegrid,
    trControl = control)

print(rf_gridsearch)
plot(rf_gridsearch)
table2 <- cbind(models ="rf_gridsearch", rf_gridsearch$bestTune)

#tuning with algo
rf_bestmtry <-tuneRF(datatrain[1:15],
                     datatrain$ytrain,
                     stepFactor = 1.5,
                     improve = 1e-5,
                     ntree = 500) 

table3 <- cbind(models ="rf_bestmtry", mtry= 3)

mtrytable <- rbind(table1, table2, table3)


# RF: Tuning maxnode w/Mtry = 3 ----------------------------------------------------------------
# Define the control
trControl <- trainControl(method = "cv",
                          number = 10,
                          search = "grid")
#tuning maxnodes
store_maxnode <- list()
tuneGrid <- expand.grid(.mtry = 3) #mtry = 3 selon rf_random
for (maxnodes in c(10:32)) {
  rf_maxnode <- train(
    ytrain ~ .,
    data = datatrain,
    method = "rf",
    metric = "Accuracy",
    tuneGrid = tuneGrid,
    trControl = trControl,
    importance = TRUE,
    nodesize = 14,
    maxnodes = maxnodes,
    ntree = 500)
  current_iteration <- toString(maxnodes)
  store_maxnode[[current_iteration]] <- rf_maxnode}

results_mtry <- resamples(store_maxnode) 
summary(results_mtry)


#tuning the ntree
store_maxtrees <- list()
for (ntree in c(250, 300, 350, 400, 450, 500, 550, 600, 800, 1000, 2000)) {
  set.seed(5678)
  rf_maxtrees <- train(
    ytrain ~ .,
    data = datatrain,
    method = "rf",
    metric = "Accuracy",
    tuneGrid = tuneGrid,
    trControl = trControl,
    importance = TRUE,
    nodesize = 14,
    maxnodes = maxnodes,
    ntree = ntree)
  key <- toString(ntree)
  store_maxtrees[[key]] <- rf_maxtrees}

results_tree <- resamples(store_maxtrees) 
summary(results_tree)
# best ntree == 800
# maxnodes = 32
# mtry = 3


save.image(file = "Data_ligne442.RData")
#load("Data_ligne442.RData")

# RF: Tuning maxnode w/Mtry = 1 ----------------------------------------------------------------
#tuning maxnodes
store_maxnode <- list()
tuneGrid <- expand.grid(.mtry = 1) #mtry = 1
for (maxnodes in c(10:32)) {
  set.seed(12336814)
  rf_maxnode <- train(
    ytrain ~ .,
    data = datatrain,
    method = "rf",
    metric = "Accuracy",
    tuneGrid = tuneGrid,
    trControl = trControl,
    importance = TRUE,
    nodesize = 14,
    maxnodes = maxnodes,
    ntree = 500
  )
  current_iteration <- toString(maxnodes)
  store_maxnode[[current_iteration]] <- rf_maxnode
}
results_mtry <- resamples(store_maxnode) 
summary(results_mtry)
# a mtry = 30 nodes

store_maxtrees <- list()
for (ntree in c(250, 300, 350, 400, 450, 500, 550, 600, 800, 1000, 2000)) {
  set.seed(5678) 
  rf_maxtrees <- train(
    y ~ .,
    data = datatrain,
    method = "rf",
    metric = "Accuracy",
    tuneGrid = tuneGrid,
    trControl = trControl,
    importance = TRUE,
    nodesize = 14,
    maxnodes = maxnodes,
    ntree = ntree
  )
  key <- toString(ntree)
  store_maxtrees[[key]] <- rf_maxtrees
}
results_tree <- resamples(store_maxtrees) 
summary(results_tree)
# best ntree == 800
# maxnodes = 30
# mtry = 1


#RF: Final models -----------------------------------------------------------
# best ntree == 800
# maxnodes = 32
# mtry = 3
rf1 = randomForest(
  ytrain~.,
  data = datatrain,
  ntree = 800,
  maxnodes = 32,
  mtry = 3
)
predrf1=predict(rf1,newdata=datavalidation)
mean(predrf1 == factor(datavalidation$y, ordered = F))


rf2 = randomForest(
  ytrain~.,
  data = datatrain,
  ntree = 800,
  maxnodes = 30,
  mtry = 1
)
predrf2=predict(rf2,newdata=datavalidation)
mean(predrf2 == factor(datavalidation$y, ordered = F)) #0.605753

# best ntree == 800
# maxnodes = 30
# mtry = 1

confrf1 <- confusionMatrix(referencey, predrf1) 
confrf2 <- confusionMatrix(referencey, predrf2) 
confrf1Class <- confrf1$byClass
confrf2Class <- confrf2$byClass 

# LR: one-on-one ----------------------------------------------------------

#split training set
# train
datatrain$d2 <- ifelse(datatrain$ytrain == 2, 1,0)
datatrain$d3 <- ifelse(datatrain$ytrain == 3, 1,0)
datatrain$d2 <- as.factor(datatrain$d2)
datatrain$d3 <- as.factor(datatrain$d3) 

# validation
datavalidation$d2 <- ifelse(datavalidation$yvalidation == 2, 1,0) 
datavalidation$d3 <- ifelse(datavalidation$yvalidation == 3, 1,0)
datavalidation$d2 <- as.factor(datavalidation$d2) 
datavalidation$d3 <- as.factor(datavalidation$d3)


#changer lordre des colonnes et rendre d2 et d3 en facteur
# mydatalog <- mydatalog[, c(1:12, 14,15, 16, 17, 13)]

# Transformation de la variable d'int ́er^et en facteur, avec des noms pour les niveaux
levels <- unique(datatrain$d2)
datatrain$d2=factor(datatrain$d2, labels=make.names(levels))



#model1 d2 (si prediction = 1 alors on predit 2 et si prediction = 0 alors on predit 1 ou 3) 
modeld2 = glm(d2 ~ . -d2 - d3 - ytrain,
  family = "binomial",
  data = datatrain
)
step(modeld2)

modeld2 = glm(
  d2 ~ fixedacidity + residualsugar + alcohol + fixedacidity.alcohol + 
    volatileacidity.citricacid + volatileacidity.density + freesulfurdioxide.totalsulfurdioxide + 
    density.sulphates + freesulfurdioxidelog,
  family = "binomial",
  data = datatrain
) 
summary(modeld2)
#si predictiond2 et d3 = 1 alors on predit 3
#si predictiond2 et d3 = 0 alors on predit 1
#si predictiond2 = 1 et d3 = 0 alors on predit 2

#model1 d3 (si prediction = 1 alors on predit 2 et si prediction = 0 alors on predit 1 ou 3)
modeld3 = glm(
  d3 ~. - d2 -d3 - ytrain,
  family = "binomial",
  data = datatrain
) 
step(modeld3)


modeld3 = glm(
  d3 ~ fixedacidity + residualsugar + alcohol + fixedacidity.alcohol + 
    volatileacidity.citricacid + volatileacidity.density + chlorides.density + 
    freesulfurdioxide.totalsulfurdioxide + totalsulfurdioxide.sulphates + 
    totalsulfurdioxide.alcohol + freesulfurdioxidelog,
  family = "binomial",
  data = datatrain
) 
summary(modeld3)

#tester modele sur test
finalpred <- NULL
pred1on1 <- NULL
predd2 = predict(modeld2, newdata=datavalidation, type="response") 
predd3 = predict(modeld3, newdata=datavalidation, type="response")
pred1on1 <- cbind(predd2,predd3)

finalpredd2 <- ifelse(pred1on1[,1] > .4 ,1,0) 
finalpredd3 <- ifelse(pred1on1[,2] > .4 ,1,0)
pred1on1 <- cbind(pred1on1, finalpredd2, finalpredd3)

finalpred <- ifelse(pred1on1[,3]== 1 & pred1on1[,4]== 0 | pred1on1[,3]== 0 & pred1on1[,4]== 1 ,2,
                    ifelse(pred1on1[,3]== 0 & pred1on1[,4]== 0, 1, 3)) 
pred1on1 <- cbind(pred1on1, finalpred)
table(pred1on1[,5])
vectorpred1on1 <-c("NA",
                   round(mean(finalpred == factor(datavalidation$yvalidation,ordered = F)), 2),
                   "NA", "NA", "NA")

# #Table des resultats -----------------------------------------------------
# 
# #resultats
resultats <- NULL
c<- "NA"
resultats <- data.frame(rbind(nbMultinom,nbpolr,
                              nbAvgLas, nbAvgRid,
                              nbAvgElas, c,c))


predridge = predict(cvridge, new= allxvalidation, s= bestridge, type = "class")
predelastic = predict(cvelastic, new= allxvalidation, s= bestelastic, type = "class")
predlasso = predict(cvlasso, new= allxvalidation, s= bestlasso, type = "class")

Precision <- NULL
Precision <- data.frame(rbind(round(mean(predMultinom == factor(datavalidation$yvalidation, ordered = F)),2),
                              round(mean(predpolr == factor(datavalidation$yvalidation, ordered = F)),2),
                              round(mean(predridge == factor(datavalidation$yvalidation, ordered = F)),2),
                              round(mean(predelastic == factor(datavalidation$yvalidation,ordered = F)),2),
                              round(mean(predlasso == factor(datavalidation$yvalidation,ordered = F)),2),
                              round(mean(predrf1 == factor(datavalidation$yvalidation, ordered = F)),2),
                              round(mean(predrf2 == factor(datavalidation$yvalidation, ordered = F)),2)))
Precision

PrecisionClasses <-data.frame(confMultiClass[, 11],confPolrClass[, 11],
                              conflassoClass[, 11],confridgeClass[, 11],
                              confelasClass[, 11],confrf1Class[, 11],
                              confrf2Class[, 11])

PrecisionClassesT <- transpose(PrecisionClasses)
PrecisionClasse1 <- round(as.vector(unlist(PrecisionClassesT[1])),2)
PrecisionClasse2 <- round(as.vector(unlist(PrecisionClassesT[2])),2)
PrecisionClasse3 <- round(as.vector(unlist(PrecisionClassesT[3])),2)

resultats <- cbind(resultats, Precision, PrecisionClasse1, PrecisionClasse2, PrecisionClasse3)
names(resultats) = c("NbVariable", "Precision", "PrecisionClasse1", "PrecisionClasse2", "PrecisionClasse3")
resultats <- rbind(resultats, vectorpred1on1)
row.names(resultats) = c(
  "multinomSimple",
  "polrSimple",
  "lasso",
  "elasticNet",
  "ridge",
  "rf1",
  "rf2",
  "1on1RL")

save.image(file = "Data_final.RData")
#load("Data_final.RData")