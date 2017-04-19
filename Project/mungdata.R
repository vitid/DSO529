library(dplyr)
library(stringr)
library(corrplot)
library(car)

setwd("/home/vitidn/mydata/repo_git/DSO529/Project")

score = read.csv("data/scores.csv",stringsAsFactors = FALSE)
colnames(score)[1] = "DBN"

demographic = read.csv("data/demographics.csv",stringsAsFactors = FALSE)
demographic = demographic %>% filter(Year == "2014-15")

score = (score %>% inner_join(demographic,by=c("DBN")))

survey = read.csv("data/survey_2014.csv",stringsAsFactors = FALSE)

score = (score %>% inner_join(survey,by=c("DBN")))

score$Start.Time = strptime(score$Start.Time,"%I:%M %p")
score$End.Time = strptime(score$End.Time,"%I:%M %p")
score$Class.Hours = as.numeric(difftime(score$End.Time , score$Start.Time,units = "hours"))
score$Start.Time = score$Start.Time$hour + 0.01 * score$Start.Time$min
score$End.Time = score$End.Time$hour + 0.01 * score$End.Time$min

score[is.na(score$Start.Time),"Start.Time"] = median(score$Start.Time,na.rm = TRUE)
score[is.na(score$End.Time),"End.Time"] = median(score$End.Time,na.rm = TRUE)
score[is.na(score$Class.Hours),"Class.Hours"] = median(score$Class.Hours,na.rm = TRUE)

convertPercentColumnToNumeric <- function(x)
{
    return(as.numeric(str_replace(x,"%","")))
}

score$Percent.White = convertPercentColumnToNumeric(score$Percent.White)
score$Percent.Black = convertPercentColumnToNumeric(score$Percent.Black)
score$Percent.Hispanic = convertPercentColumnToNumeric(score$Percent.Hispanic)
score$Percent.Asian = convertPercentColumnToNumeric(score$Percent.Asian)
score$X..Students.with.Disabilities.1 = convertPercentColumnToNumeric(score$X..Students.with.Disabilities.1)
score$X..English.Language.Learners.1 = convertPercentColumnToNumeric(score$X..English.Language.Learners.1)
score$X..Poverty.1 = convertPercentColumnToNumeric(score$X..Poverty.1)
score$Total.Parent..Response.Rate = convertPercentColumnToNumeric(score$Total.Parent..Response.Rate)
score$Total.Teacher.Response.Rate = convertPercentColumnToNumeric(score$Total.Teacher.Response.Rate)
score$Percent.Satisfaction..Instructional.Core = convertPercentColumnToNumeric(score$Percent.Satisfaction..Instructional.Core)
score$Percent.Satisfaction..Systems.for.Improvement = convertPercentColumnToNumeric(score$Percent.Satisfaction..Systems.for.Improvement)
score$Percent.Satisfaction..School.Culture = convertPercentColumnToNumeric(score$Percent.Satisfaction..School.Culture)
score$X..Female.1 = convertPercentColumnToNumeric(score$X..Female.1)
score$X..Male.1 = convertPercentColumnToNumeric(score$X..Male.1)

score = score[,c("DBN","Borough","City","Latitude","Longitude","Start.Time","End.Time",
                 "Student.Enrollment","Percent.White","Percent.Black",
                 "Percent.Hispanic","Percent.Asian",
                 "Average.Score..SAT.Math.","Average.Score..SAT.Reading.","Average.Score..SAT.Writing."
                 ,"X..Female.1","X..Male.1","X..Students.with.Disabilities.1","X..English.Language.Learners.1"
                 ,"X..Poverty.1","Total.Parent..Response.Rate",
                 "Total.Teacher.Response.Rate","Percent.Satisfaction..Instructional.Core"
                 ,"Percent.Satisfaction..Systems.for.Improvement","Percent.Satisfaction..School.Culture",
                 "Class.Hours")]

colnames(score) = c("DBN","Borough","City","Latitude","Longitude","Start_Time","End_Time",
                    "Student_Enrollment","Percent_White","Percent_Black",
                    "Percent_Hispanic","Percent_Asian",
                    "Average_SAT_Math","Average_SAT_Reading","Average_SAT_Writing"
                    ,"Female_Percent","Male_Percent","Disabilities_Percent","EngLearner_Percent"
                    ,"Poverty_Percent","Parent_Response_Rate",
                    "Teacher_Response_Rate","Instructional_Core_Satisfaction"
                    ,"Systems_for_Improvement_Satisfaction","School_Culture_Satisfaction",
                    "Class_Hours")

score = na.omit(score)
write.csv(score,file="data/processed_score.csv",row.names = FALSE,quote = FALSE)
