#bank loan approval 


data <- read.csv("Z:\\semester4\\bank.csv",sep=';',header = T)

data$y <- as.integer(data$y)-1
#print(data)

age <- data$age
job<- data$job
marital <- data$marital
education <- data$education
default <- data$default
balance <- data$balance
housing <- data$housing
loan <- data$loan
contact <- data$contact
day <- data$day
month <- data$month
duration <- data$duration
campaign <- data$campaign
pdays <- data$pdays
previous <- data$previous
poutcome <- data$poutcome
y <- data$y



lr <- glm(y ~ age+balance+pdays+previous,data = data,family = "binomial")
print(summary(lr))


print("Predict")
for(i in 1:length(age))
{
  z=1.314e-02 *age[i]+1.160e-05*balance[i]+1.653e-03*pdays+9.711e-02*previous+2.758e+00
  if(z<0.5)
    print("loan not approved")
  else{
    print("loan approved")
  }
}

newdat <- data.frame(hp=seq(min(age+balance+pdays+previous), max(age+balance+pdays+previous),len=4521))
Approval = predict(lr, newdata=newdat, type="response")
plot(Approval ~ age+balance+pdays+previous, col="blue")
abline(lr, col="green4", lwd=2)
