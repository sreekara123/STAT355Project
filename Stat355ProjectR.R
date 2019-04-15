Admission_Predict <- read.csv("~/Downloads/graduate-admissions/Admission_Predict.csv", header=TRUE, sep=",")
View(Admission_Predict)
summary(Admission_Predict)
#correlation
pairs(Admission_Predict[, -1])
round(cor(Admission_Predict[, -1]), 2)
#mean without and witrh research
apply(Admission_Predict[Admission_Predict$Research == 1, -1], 2, mean)
apply(Admission_Predict[Admission_Predict$Research == 0, -1], 2, mean)
plot(density(Admission_Predict$CGPA))
qqnorm(Admission_Predict$CGPA)
Admission_Predict[Admission_Predict$Chance.of.Admit > 0.95,]
mySample = sample(Admission_Predict$CGPA, 100, replace=FALSE)
z = qnorm((1-0.05/2), mean=0,sd=1) 
upperLimit = mean(mySample) + z*(sd(Admission_Predict$CGPA)/sqrt(100))
lowerLimit = mean(mySample) - z*(sd(Admission_Predict$CGPA)/sqrt(100))
t.test(mySample, mu=8.5)
hist(Admission_Predict$University.Rating, w=100, xlab = "University Rating", ylab = "Students", main="Student Enrollement Based on University Rating")
boxplot(Admission_Predict[,-1])
par(mfrow=c(2, 2))
boxplot(Admission_Predict$GRE.Score)
boxplot(Admission_Predict$CGPA)
boxplot(Admission_Predict$TOEFL.Score)
boxplot(Admission_Predict$Chance.of.Admit)
boxplot(Admission_Predict$CGPA~Admission_Predict$GRE.Score > 320, xlab="GRE Score Greater Than 320")
boxplot(Admission_Predict$CGPA~Admission_Predict$TOEFL.Score > 110, xlab="TOEFL Score Greater Than 110")
boxplot(Admission_Predict$CGPA~Admission_Predict$Research, xlab="Research")
boxplot(Admission_Predict$CGPA~Admission_Predict$University.Rating > 3, xlab="University Rating Greater Than 3")
sam = Admission_Predict[sample(nrow(Admission_Predict), 100), ]
cor.test(sam$CGPA, sam$Chance.of.Admit)
par(mfrow=c(1,1))
set.seed(1)
n=100
nboot=100
student_cgpa = matrix(NA, nrow = nboot, ncol = n)
for (i in 1:nboot) {
  CGPASample = sample(1:n, n, replace = T)
  student_cgpa[i,] = Admission_Predict$CGPA[CGPASample]
}

cgpaAvg = apply(student_cgpa, 1, mean)
hist(cgpaAvg, xlab='Bootstrap mean', main="histogram of bootstap means of CGPA")
abline(v=mean(cgpaAvg), col='red', lwd=2)

