library(tidyverse)
library(plyr) #count()
library(GGally) #ggcorr() and ggpairs()
library(reshape) #melt()
library(corrplot) #corrplot

#import
marketing <- read.csv('marketing_campaign.csv')
head(marketing)

#collect column names and split by a delimiter '.'
col.names <- str_split(colnames(marketing), '\\.') %>%
  unlist()
col.names <- col.names[1:length(col.names)]

#separate the data by delimiter ';' into each new col.names column
marketing <- marketing %>%
  separate(colnames(marketing), col.names, sep = ';', fill = 'right')

#find structure of new dataframe
str(marketing)

#columns that will be made numeric
num.cols <- col.names[-c(1,3,4,8)]

marketing <- marketing %>%
  mutate_at(num.cols, as.numeric) %>%
  mutate(Dt_Customer = as.Date(Dt_Customer)) #create Date column

count(marketing$Marital_Status)

#Create New Cohesive Categories
marketing$Rel_Status[marketing$Marital_Status %in% c('Alone', 'Divorced', 'Widow', 'Single')] <- 'Single'
marketing$Rel_Status[marketing$Marital_Status %in% c('Married', 'Together')] <- 'Coupled'
marketing$Rel_Status[marketing$Marital_Status %in% c('Absurd', 'YOLO')] <- '' #insert blanks to be handled later

count(marketing$Education)

ggplot(marketing, aes(x = Income)) +
  geom_boxplot()

outliers <- boxplot(marketing$Income, plot = FALSE)$out
marketing <- marketing %>%
  filter(Income < max(outliers) - 1)

#look at unknown variables
summary(marketing$Z_CostContact)
summary(marketing$Z_Revenue)

#Date signed up and year of birth
ggplot(marketing, aes(Dt_Customer)) +
  geom_density(color = "darkblue", fill = "lightblue") +
  geom_vline(aes(xintercept = mean(Dt_Customer)), color = 'red', linetype = 'dashed', size = 1)
ggplot(marketing, aes(Year_Birth)) +
  geom_density(color = "darkblue", fill = "lightblue") +
  geom_vline(aes(xintercept = mean(Year_Birth)), color = 'red', linetype = 'dashed', size = 1)

marketing <- marketing %>%
  #creating new variables based off old ones
  mutate(MntSpent = MntFishProducts + MntMeatProducts + MntFruits + MntSweetProducts + MntWines + MntGoldProds) %>%
  mutate(NumPurchases = NumCatalogPurchases + NumStorePurchases + NumWebPurchases) %>%
  mutate(MinorsHome = Kidhome + Teenhome)  %>%
  mutate(AcceptedPrv = AcceptedCmp1 + AcceptedCmp2 + AcceptedCmp3 + AcceptedCmp4 + AcceptedCmp5) %>%
  mutate(Age = as.numeric(format(Dt_Customer, format = '%Y')) - Year_Birth) 

marketing <- marketing  %>%
  #sorting columns alphabetically
  select(sort(colnames(marketing))) %>%
  #dropping unneccessary columns
  select(-one_of(c('ID', 'Kidhome', 'Marital_Status', 'Teenhome', 'Year_Birth', 'Z_CostContact', 'Z_Revenue'))) %>%
  #dropping na and blank data
  mutate_all(na_if, '') %>% na.omit()

head(marketing)

#comprehensive boxplots
unwant.cols <- c('AcceptedCmp1', 'AcceptedCmp2', 'AcceptedCmp3', 'AcceptedCmp4', 'AcceptedCmp5', 'Complain',
                 'Education', 'Rel_Status', 'Dt_Customer', 'Response', 'AcceptedPrv')
melt.marketing <- marketing %>%
  select(-one_of(unwant.cols)) %>%
  melt()

ggplot(melt.marketing, aes(factor(variable), value)) +
  geom_boxplot(color = 'steelblue') +
  facet_wrap(~variable, scale = 'free') +
  labs(title = 'Boxplots of Various Variables', x = 'Variables', y = 'Ranges')

#remove outliers from age variable
outliers <- boxplot(marketing$Age, plot = FALSE)$out
marketing <- marketing %>%
  filter(Age < min(outliers))

#list of products
products <- c('MntWines', 'MntFruits', 'MntMeatProducts', 'MntFishProducts', 'MntSweetProducts', 'MntGoldProds')

#sum amounts spent on products and set these values in df
products.df <- marketing %>%
  select(products) %>% summarize_each(sum) %>%
  t() %>% as.data.frame() %>%
  rownames_to_column('Products')

#clean up structures
colnames(products.df) <- c('Products', 'Sums')
products <- gsub('Products', '', gsub(c('Mnt'), '', products))

#creating pie chart
ggplot(products.df, aes(x = '', y = Sums, fill = Products)) +
  geom_bar(stat = 'identity', width = 1, color = 'black') +
  geom_text(aes(label = paste('$', Sums)), color = 'white', position = position_stack(vjust = 0.5)) +
  coord_polar('y', start = 0) +
  labs(title = 'Percentage of Total Sales from Products', fill = 'Products', 
       caption = paste('Total Revenue: $', sum(products.df$Sums))) +
  scale_fill_discrete(labels = sort(products)) +
  theme(axis.ticks=element_blank(), axis.text.y=element_blank(), axis.text.x=element_text(colour='black'),
        axis.title=element_blank()) +
  scale_y_continuous(breaks = cumsum(products.df$Sums) - products.df$Sums / 2,
                     labels = paste(round(products.df$Sums/sum(products.df$Sums) * 100, 1), '%'))

purchase <- c('NumCatalogPurchases', 'NumStorePurchases', 'NumWebPurchases')

purchase.df <- marketing %>%
  select(purchase) %>% summarize_each(sum) %>%
  t() %>% as.data.frame() %>%
  rownames_to_column('Place')

colnames(purchase.df) <- c('Place', 'Sums')
purchase <- gsub('Purchases', '', gsub(c('Num'), '', purchase))

ggplot(purchase.df, aes(x = '', y = Sums, fill = Place)) +
  geom_bar(stat = 'identity', width = 1, color = 'black') +
  geom_text(aes(label = paste(Sums)), color = 'white', position = position_stack(vjust = 0.5)) +
  coord_polar('y', start = 0) +
  labs(title = 'Percentage of Total Num of Purchases', fill = 'Places', 
       caption = paste('Total Num: ', sum(purchase.df$Sums))) +
  scale_fill_discrete(labels = sort(purchase)) +
  theme(axis.ticks=element_blank(), axis.text.y=element_blank(), axis.text.x=element_text(colour='black'),
        axis.title=element_blank()) +
  scale_y_continuous(breaks = cumsum(purchase.df$Sums) - purchase.df$Sums / 2,
                     labels = paste(round(purchase.df$Sums/sum(purchase.df$Sums) * 100, 1), '%'))

#correlation plot between numeric vectors
Correlation_plot <- ggcorr(select(marketing, -one_of(unwant.cols)), 
                           geom = 'blank', label = TRUE, hjust = 0.75, layout.exp = 3) +
  geom_point(size = 10, aes(color = coefficient > 0, alpha = abs(coefficient) > 0.6)) +
  scale_alpha_manual(values = c('TRUE' = 0.25, 'FALSE' = 0)) +
  guides(color = 'none', alpha = 'none') +
  labs(title = 'Correlation Map')

Correlation_plot

#income v mntspent
ggplot(marketing, aes(x = MntSpent, y = Income)) +
  geom_point() +
  geom_smooth(method = lm) +
  labs(title = 'Income Against Amount Spent', x = 'Amount Spent ($)', y = 'Yearly Income ($)')

#income by age
ggplot(marketing, aes(x = NumWebVisitsMonth, y = Income)) +
  geom_point() +
  geom_smooth(method = lm) +
  labs(title = 'Income Against Age', x = '# of Web Visits per Month', y = 'Yearly Income ($)')
#pie chart of complaints
complaint.counts <- count(marketing$Complain)
ggplot(complaint.counts, aes(x = '', y = freq, fill = as.character(x))) +
  geom_bar(stat = 'identity', width = 1) +
  coord_polar('y', start = 0) +
  labs(title = 'Share of Complaints', subtitle = 'In the last 2 Years') +
  scale_fill_discrete(name = "Complant?", labels = c("No", "Yes")) +
  theme_void()

#boxplot Income by accepted previous
ggplot(marketing, aes(x = as.character(AcceptedPrv), y = Income)) +
  geom_boxplot(color = 'steelblue') +
  labs(x = 'Previously Accepted Campaigns')

#boxplot Income by Response
ggplot(marketing, aes(x = as.character(Response), y = Income)) +
  geom_boxplot(color = 'steelblue') +
  labs(x = 'Response', y = 'Annual Income')

#boxplot Minors Home by accepted previous
ggplot(marketing, aes(x = as.character(AcceptedPrv), y = MinorsHome)) +
  geom_boxplot(color = 'steelblue') +
  labs(x = 'Previously Accepted Campaigns', y = 'Kids at Home')

#boxplot amount spent by Minors Home
ggplot(marketing, aes(x = as.character(MinorsHome), y = MntSpent)) +
  geom_boxplot(color = 'steelblue') +
  labs(x = 'Kids at Home', y = 'Amount Spent')

#boxplot Age by accepted previous
ggplot(marketing, aes(x = as.character(AcceptedPrv), y = Age)) +
  geom_boxplot(color = 'steelblue') +
  labs(x = 'Previously Accepted Campaigns', y = 'Age')

#boxplot Recency by Response
ggplot(marketing, aes(x = as.character(Response), y = Recency)) +
  geom_boxplot(color = 'steelblue') +
  labs(x = 'Response', y = 'Recency')

marketing_1<-marketing[marketing$AcceptedPrv != 0,]

ggplot(marketing_1, aes(x = as.character(AcceptedPrv), fill = Education)) +
  geom_bar(position = 'stack') +
  labs(x = 'Previously Accepted Campaigns', fill = 'Education')

#Education by Accepted previous
ggplot(marketing, aes(x = as.character(AcceptedPrv), fill = Education)) +
  geom_bar(position = 'stack') +
  labs(x = 'Previously Accepted Campaigns', fill = 'Education')

chisq <- chisq.test(table(marketing$AcceptedPrv, marketing$Education))
chisq

round(chisq$residuals, 3)

#Relationship by Accepted previous
ggplot(marketing, aes(x = as.character(AcceptedPrv), fill = Rel_Status)) +
  geom_bar(position = 'stack') +
  labs(x = 'Previously Accepted Campaigns', fill = 'Relationship')

chisq <- chisq.test(table(marketing$AcceptedPrv, marketing$Rel_Status))
chisq

round(chisq$residuals, 3)

#Current campaign response by Accepted previous
ggplot(marketing, aes(x = as.character(AcceptedPrv), fill = as.character(Response))) +
  geom_bar(position = 'stack') +
  labs(x = 'Previously Accepted Campaigns', fill = 'Relationship')

chisq <- chisq.test(table(marketing$AcceptedPrv, marketing$Response))
chisq

round(chisq$residuals, 3)

corrplot(chisq$residuals, is.cor = FALSE)

#Income by Accepted previous
marketing <- marketing %>% 
  mutate(IncomeCategory = ifelse(Income < 10000, "Cat1", 
                               ifelse(Income > 11000 & Income < 50000, "Cat2", 
                                      ifelse(Income > 51000 & Income < 100000, "Cat3", 
                                             ifelse(Income > 100001 & Income < 150000, "Cat4", 
                                                    ifelse(Income > 150001, "Cat5", "Cat5" )
  )))))


ggplot(marketing, aes(x = as.character(AcceptedPrv), fill = IncomeCategory)) +
  geom_bar(position = 'stack') +
  labs(x = 'Previously Accepted Campaigns', fill = 'IncomeCategory')

chisq <- chisq.test(table(marketing$AcceptedPrv, marketing$IncomeCategory))
chisq

round(chisq$residuals, 3)

#Age by Accepted previous
min(marketing$Age)
max(marketing$Age)
marketing <- marketing %>% 
  mutate(AgeCategory = ifelse(Age < 20, "AgeCat1", 
                                 ifelse(Age > 21 & Age < 30, "AgeCat2", 
                                        ifelse(Age > 31 & Age < 40, "AgeCat3",
                                               ifelse(Age > 41 & Age < 50, "AgeCat4", 
                                                      ifelse(Age > 51 & Age < 60, "AgeCat5", "AgeCat6")
                                               )))))


ggplot(marketing, aes(x = as.character(AcceptedPrv), fill = AgeCategory)) +
  geom_bar(position = 'stack') +
  labs(x = 'Previously Accepted Campaigns', fill = 'AgeCategory')

chisq <- chisq.test(table(marketing$AcceptedPrv, marketing$AgeCategory))
chisq

round(chisq$residuals, 3)

#Amount Spent by Accepted previous
min(marketing$MntSpent)
max(marketing$MntSpent)
marketing <- marketing %>% 
  mutate(SpendCategory = ifelse(MntSpent < 50, "SpendCat1", 
                                 ifelse(MntSpent > 51 & MntSpent < 100, "SpendCat2", 
                                        ifelse(MntSpent > 101 & MntSpent < 200, "SpendCat3", 
                                               ifelse(MntSpent > 201 & MntSpent < 500, "SpendCat4", 
                                                      ifelse(MntSpent > 501 & MntSpent < 1000, "SpendCat5", 
                                                                ifelse(MntSpent > 1001, "SpendCat6", "SpendCat7")
                                               ))))))

ggplot(marketing, aes(x = as.character(AcceptedPrv), fill = SpendCategory)) +
  geom_bar(position = 'stack') +
  labs(x = 'Previously Accepted Campaigns', fill = 'SpendCategory')

chisq <- chisq.test(table(marketing$AcceptedPrv, marketing$SpendCategory))
chisq

round(chisq$residuals, 3)

#bar chart of most successful marketing campaign
cmps <- c('AcceptedCmp1', 'AcceptedCmp2', 'AcceptedCmp3', 'AcceptedCmp4', 'AcceptedCmp5', 'Response')

#considering making a function out of this from how much I use these lines of code
cmp.df <- marketing %>%
  select(cmps) %>% summarize_each(sum) %>%
  t() %>% as.data.frame() %>%
  rownames_to_column('Campaigns') #two columns, one is name of column and the next is totals

#clean up the structure for easier manipulation
cmp.df <- cmp.df %>%
  mutate(Percents = V1 / nrow(marketing)) %>% #create percents
  select(-V1) #drop sums

#bar plot
ggplot(cmp.df, aes(y = reorder(Campaigns, Percents), x = Percents)) +
  geom_bar(stat = 'identity', fill = 'steelblue') +
  labs(x = 'Percentage', y = 'Campaigns')

#second library import
library(randomForest)
library(caret)
library(ranger)
library(rsample)
library(class)
library(e1071)

#split data
set.seed(101)
index <- sample(1: nrow(marketing), 0.7 * nrow(marketing))
train <- marketing[index,]
test <- marketing[-index,]

#Random Forest Decision Tree
set.seed(101)
rf <- randomForest(Response ~ ., data = train)
rf

plot(rf)

which.min(rf$mse)
rf$mse[which.min(rf$mse)]

#creating grid for our hyperparameters
hyper_grid <- expand.grid(
  mtry       = seq(4, 10, by = 2),
  node_size  = seq(3, 9, by = 2),
  sampe_size = c(.55, .632, .70, .80),
  OOB_RMSE   = 0
)

for(i in 1:nrow(hyper_grid)) {
  # train model
  model <- ranger(
    formula         = Response ~ ., 
    data            = train, 
    num.trees       = 350,
    mtry            = hyper_grid$mtry[i],
    min.node.size   = hyper_grid$node_size[i],
    sample.fraction = hyper_grid$sampe_size[i],
    seed            = 101
  )
  
  # add OOB error to grid
  hyper_grid$OOB_RMSE[i] <- sqrt(model$prediction.error)
}

#display hyperparameters by RMSE
hyper_grid %>% 
  dplyr::arrange(OOB_RMSE) %>%
  head()

#creating a vector for the future graph
OOB_RMSE <- vector(mode = "numeric", length = 100)
for(i in seq_along(OOB_RMSE)) {
  #constructing an optimal model to see if there's any change
  optimal_ranger <- ranger(
    formula         = Response ~ ., 
    data            = train, 
    num.trees       = 500,
    mtry            = 10,
    min.node.size   = 3,
    sample.fraction = .5,
    importance      = 'impurity'
  )
  #getting RMSE instead of MSE
  OOB_RMSE[i] <- sqrt(optimal_ranger$prediction.error)
}

optimal_ranger
hist(OOB_RMSE, breaks = 20)

#show level of importance of each variable
optimal_ranger$variable.importance %>% 
  tidy() %>%
  dplyr::arrange(desc(x)) %>%
  dplyr::top_n(10) %>%
  ggplot(aes(reorder(names, x), x)) +
  geom_col() +
  coord_flip() +
  labs(title = 'Top 10 Important Variables', x = 'Variables')

#Random Forest Decision Tree
set.seed(101)
#remove linearly contributing factors
train.t <- train %>%
  select(-one_of('AcceptedCmp1', 'AcceptedCmp2', 'AcceptedCmp3', 'AcceptedCmp4', 'AcceptedCmp5'))
rf <- randomForest(AcceptedPrv ~ ., data = train.t)
rf

#further analyses may investigate the contributing factors AcceptedPrv

#KNN utilize Euclidean space so categorical variables will have to leave
set.seed(101)
train <- train %>% dplyr::select(where(is.numeric))
test <- test %>% dplyr::select(where(is.numeric))

#normalize data
normalize <- function(x) {
  return (x - min(x)) / (max(x) - min(x)) }

train.n <- as.data.frame(lapply(train, normalize))
test.n <- as.data.frame(lapply(test, normalize))

cl.tr <- train.n$Response
cl.ts <- test.n$Response

#perform algorithm and construct confusion Matrix
knn <- knn(train.n, test.n, train.n$Response, k = 39)
confusionMatrix(table(knn, test.n$Response), positive = '1')
