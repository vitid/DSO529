library(dplyr)
library(stringr)
library(corrplot)
library(car)

setwd("/home/vitidn/mydata/repo_git/DSO529/Project")
score = read.csv("data/processed_score.csv")

applyTransformation<-function(data)
{
    data$DBN = NULL
    data$City = NULL
    data$Latitude = NULL
    data$Longitude = NULL
    data$Average_SAT_Reading = NULL
    data$Average_SAT_Writing = NULL
    data$Female_Percent = NULL
    data$Average_SAT_Math = log(data$Average_SAT_Math)
    data$Percent_White = log(data$Percent_White + 0.1)
    data$Percent_Black = log(data$Percent_Black + 0.1)
    data$Percent_Hispanic = log(data$Percent_Hispanic + 0.1)
    data$Percent_Asian = log(data$Percent_Asian + 0.1)
    data$EngLearner_Percent = log(data$EngLearner_Percent + 0.1)
    return(data)
}

parkTest <- function(model){
    park_test = data.frame(r_square=model$residuals^2,y_hat=model$fitted.values)
    park_test_model = lm(park_test$r_square ~ y_hat, data = park_test)
    print(summary(park_test_model))
    
    park_test = data.frame(log_r_square=log(model$residuals^2),y_hat=model$fitted.values)
    park_test_model = lm(park_test$log_r_square ~ y_hat, data = park_test)
    print(summary(park_test_model))
}

score = applyTransformation(score)

###Try Weighted Least Square, weight on the size of enrollment
score2 = score
enroll_size = score2$Student_Enrollment
for(colname in colnames(score2))
{
    if(colname != "Borough")
    {
        score2[colname] = score2[colname]*sqrt(enroll_size)
    }
}
score2$Sqrn_Student_Enrollment = sqrt(enroll_size)
score2$Student_Enrollment = NULL
#regress through origin
model = lm(Average_SAT_Math ~ . - 1,data=score2)

plot(model)
parkTest(model)

###Try Weighted Least Square, 2-Stages Approach
model = lm(Average_SAT_Math ~ .,data=score)
residual_group = data.frame(r=model$residuals,group_name = score$Borough)
residual_group$r = sqrt(residual_group$r^2)
boxplot(r~group_name,data=residual_group)
grouped = residual_group %>% group_by(group_name)
residual_group = as.data.frame(summarise(grouped,weighted=mean(r)))

score2 = score
score2 = score2 %>% inner_join(residual_group,by = c("Borough" = "group_name"))
weighted = score2$weighted
for(colname in colnames(score2))
{
    if(colname != "Borough")
    {
        score2[colname] = score2[colname]/weighted
    }
}
score2$weighted = 1/weighted
#regress through origin
model = lm(Average_SAT_Math ~ . - 1,data=score2)
parkTest(model)

write.csv(score2,file = "data/weighted_score.csv",quote=FALSE,row.names=FALSE)
write.csv(residual_group,file = "data/mean_residual_sq_group.csv",quote=FALSE,row.names=FALSE)
