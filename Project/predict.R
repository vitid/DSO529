library(dplyr)
library(caret)

setwd("/home/vitidn/mydata/repo_git/DSO529/Project")

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
    data$Queens = ifelse(data$Borough == "Queens",1,0)
    return(data)
}

parkTest <- function(result){
    park_test = data.frame(r_square=result$residual^2,y_hat=result$predict)
    park_test_model = lm(park_test$r_square ~ y_hat, data = park_test)
    print(summary(park_test_model))
    
    park_test = data.frame(log_r_square=log(result$residual^2),y_hat=result$predict)
    park_test_model = lm(park_test$log_r_square ~ y_hat, data = park_test)
    print(summary(park_test_model))
}

score = read.csv("data/processed_score.csv")

actual_values = score$Average_SAT_Math
score = applyTransformation(score)

predicts = 0.000039167*score$Student_Enrollment - 0.024356*score$Percent_Black + 0.037114*score$Percent_Asian +
            -0.001578*score$Disabilities_Percent - 0.041791*score$EngLearner_Percent +
            -0.001405*score$Poverty_Percent - 0.006013*score$Systems_for_Improvement_Satisfaction +
            0.0074627*score$School_Culture_Satisfaction + 6.1121735 +
            -0.033514*score$Queens

##assess Hetero with residual plot
predict_result = data.frame(actual=score$Average_SAT_Math,predict=predicts)
predict_result$residual = predict_result$actual - predict_result$predict
predict_result$Borough = score$Borough

weighted_group = read.csv("data/mean_residual_sq_group.csv")
predict_result = predict_result %>% inner_join(weighted_group,by=c("Borough"="group_name"))

plot(x=predict_result$predict/predict_result$weighted,y=predict_result$residual/predict_result$weighted)

#no error weighted
parkTest(predict_result)

##transform to the original space
predict_result = data.frame(actual=actual_values,predict=exp(1)^predicts)
#RMSE -> 30.85
caret::RMSE(predict_result$predict,predict_result$actual)
#R-Squared -> 0.82
1 - (sum((predict_result$actual-predict_result$predict )^2)/sum((predict_result$actual-mean(predict_result$actual))^2))
#prediction from the 1st row:
#  actual  predict
#1    657 636.54
