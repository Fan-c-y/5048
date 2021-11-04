
library("readxl")

my_data <- read_excel("cases_model.xlsx")
my_data["number"]<-c(1:nrow(my_data))
my_data<-my_data[1:25,]
plot(my_data)
model = lm(count~poly(number,2), data = my_data[1:14,])
model = lm(log(count)~number, data = my_data[1:14,])
pred <- exp(predict(model, my_data))
my_data["pred"] <- pred

pred
ggplot(my_data, aes(number)) + 
  geom_line(aes(y = count, colour = "Reality"), size = 3) + 
  geom_line(aes(y = pred, colour = "Modelling"),size = 2)+
  ylab("Cases")+ xlab("Days in wave 1")+
  annotate(geom = "vline",
           x = 14,
           xintercept = 14,
           linetype = "dashed") +
  annotate(geom = "text",
           label = "Border Closure",
           y = 600,
           x = 14,
           angle = 90, 
           vjust = 1)+ theme_bw()+ggtitle("Modelling overseas cases in wave one")
