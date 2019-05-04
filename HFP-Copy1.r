
library(readr)
library(data.table)
library(datasets)
library(dplyr)
library(lubridate)
library(ggplot2)
library(ggthemes)
library(plotly)
library(Amelia)
library(caTools)
library(class)
library(scales)

HFI <- read.csv("HPI_master.csv")



drops <- c("hpi_type","hpi_flavor", "level")
HFI <- HFI[ , !(names(HFI) %in% drops)]
HFI <- na.omit(HFI)

head(HFI)


summary(HFI)

glimpse(HFI)

str(HFI)

colSums(is.na(HFI))

is.null(HFI)

num.cols <- sapply(HFI, is.numeric)
dataset_hfi <- HFI[, num.cols]
cor(dataset_hfi)

melted_corr <- melt(cor(dataset_hfi))
ggplot(data = melted_corr, aes(x = Var1, y = Var2, 
fill = value)) + 
geom_tile()+ 
scale_fill_gradient(low="grey", high="darkred") 
+ geom_text(aes(x = Var1, y = Var2, label = round(value, 2)), size = 4) + 
labs(title = " Correlation Matrix", x = "Numeric Column(s)", 
     y = "Numeric Column(s)", fill = "Coefficient Range") + 
     theme(axis.text.x=element_text(angle=45, vjust=0.5))

rm(num.cols)
rm(dataset_hfi)
rm(melted_corr)

library(GGally)
ggpairs(data=HFI, columns = 5:7, title="Housing Data" )

summary(HFI$index_nsa)
summary(HFI$index_sa)
summary(HFI$period)

a <- ggplot(HFI, aes(HFI$index_nsa, HFI$index_sa))
b <- ggplot(HFI)

b + geom_qq(aes(sample=HFI$index_sa))

a + geom_area()
a + geom_point(position = "jitter")

fit <- lm(HFI$index_sa ~ HFI$yr + HFI$period)
fit2 <- glm(HFI$index_nsa ~ HFI$yr + HFI$period)
summary(fit)
summary(fit2)

layout(matrix(c(1,2,3,4),2,2))
plot(fit)

ggplot(HFI, aes(fit$residuals)) + geom_histogram(binwidth=8, color="black", fill="blue")

ggplot(HFI, aes(HFI$index_sa, HFI$index_nsa)) + geom_point() + stat_smooth(method="lm", col="dodgerblue3") +
theme(panel.background = element_rect(fill = "white"), 
     axis.line.x=element_line(),
     axis.line.y=element_line()) +
ggtitle("Linear Model Fitted to Data")

pred_fit <- predict(fit)

ggplot(HFI, aes(HFI$index_sa, pred_fit)) + geom_point() + stat_smooth(method="lm", col="dodgerblue3") +
theme(panel.background = element_rect(fill = "white"), 
     axis.line.x=element_line(),
     axis.line.y=element_line()) +
ggtitle("Expected VS. Predicted")
