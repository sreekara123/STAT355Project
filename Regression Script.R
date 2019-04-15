Main_Data <- read.csv("Admission_Predict.csv")
Main_Data <- Main_Data[, -1]
attach(Main_Data)
dataset <- Main_Data[, -8]

modelNone <- lm(Chance.of.Admit ~ 1, data=dataset)
summary(modelNone)

modelAll <- lm(Chance.of.Admit ~., data=dataset)
summary(modelAll)

plot(modelAll)

step(modelAll, scope=list(lower=modelNone, upper=modelAll), direction=c("backward"))
step(modelNone, scope=formula(modelAll), direction=c("forward"))

bestfit <- lm(formula = Chance.of.Admit ~ CGPA + GRE.Score + LOR + Research + TOEFL.Score, data = dataset)


predict(bestfit, newdata = data.frame(GRE.Score=340, TOEFL.Score=120, CGPA=9.5, LOR=5.0, Research=0), interval = "prediction")
predict(bestfit, newdata = data.frame(GRE.Score=340, TOEFL.Score=120, CGPA=9.5, LOR=5.0, Research=1), interval = "prediction")
predict(bestfit, newdata = data.frame(GRE.Score=339, TOEFL.Score=120, CGPA=9.5, LOR=5.0, Research=1), interval = "prediction")
predict(bestfit, newdata = data.frame(GRE.Score=330, TOEFL.Score=120, CGPA=9.5, LOR=5.0, Research=1), interval = "prediction")
predict(bestfit, newdata = data.frame(GRE.Score=340, TOEFL.Score=110, CGPA=9.5, LOR=5.0, Research=1), interval = "prediction")
