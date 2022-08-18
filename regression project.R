#Removing from memory, packages
rm(list=ls())

#data input
library(Matrix) #for matrix computations
library(olsrr) #for Variable selection algorithms
library(car) # for VIF
library(mctest) #for variance decomposition algorithms
library(readxl) #for importing xlsx file
library(ggplot2) #for visualizations
data1 = read_excel(file.choose())

#data cleaning

data1 <- subset (data1, select = -date)
data1 <- subset (data1, select = -quarter)

data1$open = as.numeric(gsub("\\$","",data1$open))
data1$high = as.numeric(gsub("\\$","",data1$high))
data1$low = as.numeric(gsub("\\$","",data1$low))
data1$close = as.numeric(gsub("\\$","",data1$close))
data1$next_weeks_open = as.numeric(gsub("\\$","",data1$next_weeks_open))
data1$next_weeks_close = as.numeric(gsub("\\$","",data1$next_weeks_close))

#removing rows which contain NA values

data1 <- na.omit(data1)




#Step 1: Plot the pairwise scatter diagrams and try to figure out the relations between the response 
#and the predictor variables from these diagrams.

#converting categorical variable into factor

data1$stock <- as.factor(data1$stock)

y = data1$`percent_change_next_weeks_price`

x1 = data1$`stock`
x2 = data1$`open`
x3 = data1$`high`
x4 = data1$`low`
x5 = data1$`close`
x6 = data1$`volume`
x7 = data1$`percent_change_price`
x8 = data1$`percent_change_volume_over_last_wk`
x9 = data1$`previous_weeks_volume`
x10 = data1$`next_weeks_open`
x11 = data1$`next_weeks_close`
x12 = data1$`days_to_next_dividend`
x13 = data1$`percent_return_next_dividend`

plot(x1, y, xlab = 'stock', ylab = 'percent_change_next_weeks_price', main = 'boxplot between stock and percent_change_next_weeks_price')
plot(x2, y ,xlab = 'open', ylab = 'percent_change_next_weeks_price', main = 'scatter plot between open and percent_change_next_weeks_price')
plot(x3, y, xlab = 'high', ylab = 'percent_change_next_weeks_price', main = 'scatter plot between high and percent_change_next_weeks_price')
plot(x4, y, xlab = 'low', ylab = 'percent_change_next_weeks_price', main = 'scatter plot between low and percent_change_next_weeks_price')
plot(x5, y, xlab = 'close', ylab = 'percent_change_next_weeks_price', main = 'scatter plot between close and percent_change_next_weeks_price')
plot(x6, y, xlab = 'volume', ylab = 'percent_change_next_weeks_price', main = ' scatter plot between volume and percent_change_next_weeks_price')
plot(x7, y, xlab = 'percent_change_price', ylab = 'percent_change_next_weeks_price', main = ' scatter plot between percent_change_price and percent_change_next_weeks_price')
plot(x8, y, xlab = 'percent_change_volume_over_last_wk', ylab = 'percent_change_next_weeks_price', main = ' scatter plot between percent_change_volume_over_last_wk and percent_change_next_weeks_price')
plot(x9, y, xlab = 'previous_weeks_volume', ylab = 'percent_change_next_weeks_price', main = ' scatter plot between previous_weeks_volume and percent_change_next_weeks_price')
plot(x10, y, xlab = 'next_weeks_open',ylab = 'percent_change_next_weeks_price', main = ' scatter plot between next_weeks_open and percent_change_next_weeks_price')
plot(x11, y, xlab = 'next_weeks_close',ylab = 'percent_change_next_weeks_price', main = ' scatter plot between next_weeks_close and percent_change_next_weeks_price')
plot(x12, y, xlab = 'days_to_next_dividend', ylab = 'percent_change_next_weeks_price', main = 'scatter plot between days_to_next_dividend and percent_change_next_weeks_price')
plot(x13, y, xlab = 'percent_return_next_dividend',ylab = 'percent_change_next_weeks_price', main = ' scatter plot between percent_return_next_dividend and percent_change_next_weeks_price')

#conclusion from scatter plots: on plotting the polynomials of the predictor variables we do not obtain any kind of 
#linear scatter plot but on plotting the logarithmic of some predictor variables we obtain a little better linear plot
#on comparison to the original. Hence, we conclude that the relation followed by the predictor variables with respect
#to response variables is somewhat linear.


#converting categorical variable(factor) into numeric variable

data1$stock <- as.numeric(data1$stock)






#Step 2: Based on the relations guessed in Step 1, propose a linear regression model (remember this allows 
#polynomial terms in the predictor variables) with appropriate assumptions.

#we propose a Multiple Linear Regression Model (MLRM)

#this dataset is a design experiment hence the predictor variables are non-stochastic

#checking assumption of linear independence (A5)

X = matrix(1,length(y),14)
X[,2]=x1
X[,3]=x2
X[,4]=x3
X[,5]=x4
X[,6]=x5
X[,7]=x6
X[,8]=x7
X[,9]= x8
X[,10]=x9
X[,11]=x10
X[,12]=x11
X[,13]=x12
X[,14]=x13


det(t(X)%*%X)

# Determinant of (X^\prime X) is large. But it may be misleading to conclude about the multi-collinearity.


x1 = as.numeric(x1)


#Step 3: Fit the model proposed in Step 2 to your dataset.


#mlrm model fitting

mlrm = lm(y ~ x1+x2+x3+x4+x5+x6+x7+x8+x9+x10+x11+x12+x13)


#Step 4,5,6: 

summary(mlrm)


#Step 7:

# Forward selection method
f = ols_step_forward_p(mlrm, details = FALSE) 
summary(f$model) # r2=0.7032


# backward elimination method
b = ols_step_backward_p(mlrm, details=FALSE)
summary(b$model) # r2=0.7032


# Step-wise selection method
s = ols_step_both_p(mlrm, details=FALSE)
summary(s$model) # r2=0.7032

data1 <- subset (data1, select = -c(stock,open,high,low,close,volume,percent_change_price,percent_change_volume_over_last_wk,days_to_next_dividend))


#Step 8:


# Centering and scaling variables
n = length(y)
scaled_data1 = data.frame(sqrt(1/(n-1))*scale.default(data1, center=TRUE,scale=TRUE))
# scaled.default scales each column by its standar deviation which use (n-1) as divisor.
# But we want to scale each column by its standar deviaion multiplied by squareroot of (n-1).
# Hence we multiple scale.default() by 1/sqrt(n-1).

scaled_fit1 = lm(scaled_data1$percent_change_next_weeks_price~.-c(scaled_data1$percent_change_next_weeks_price), data=scaled_data1)
summary(scaled_fit1)

design_scaled = data.matrix(scaled_data1[,-4])
design_scaled
Correlation_matrix = round(t(design_scaled)%*%(design_scaled),2)
Correlation_matrix

det(Correlation_matrix)
#multicollinearity suspected

# Computing VIF
round(vif(scaled_fit1),2)

# vif computes VIF of a multiple linear regression model.
# We use  cut-off 10. One may also use appropriate higher cut-off.

# Condition indices

e = eigen(Correlation_matrix)$values  #eigen(A) computes both eigenvalues and eigenvectors of A.
#The command eigen(A)$values prints only the eigenvalues of A.

Condition_indices = rep(0,length(e))  #Computation of condition indices
for(i in 1:length(e)){
  Condition_indices[i] = max(e)/e[i] 
}
round(Condition_indices,2)
# We use cut-off 10.

# Calculating measures based on variance decomposition
scaled_fit = lm( percent_change_next_weeks_price~ ., data = scaled_data1)
VP = eigprop(scaled_fit, Inter=FALSE)
VP$pi[4,]
# eigprop computes many things along with the measures based on variances decomposition.
# eigprop()$pi prints only the measures based on variances decomposition.

# PC regression

pca = prcomp(scaled_data1[,-4]) 
# prcomp(A) performs principal component analysis on the data matrix A. 
# We want principal components of only predictor variables.
# The fourth column of scaled_data contains response variable.
# scaled_data[,-4] removes the fourth column and is remained with only the predictor variables.

pc_variances = summary(pca) # summary(prcomp()) prints sample variance of the proncipal components.
pc = pca$x # The i-th column of prcomp(A)$x provides the i-th principal component of the data matrix A.
pc_data = data.frame(cbind(scaled_data1$percent_change_next_weeks_price, pc))
pc_data$V1
# cbind(a,A) augments the column 'a' (as the first column) in the matrix A.
# pc_data contains data on the standarized response variable and all principal components corresponding to the data matrix of predictor variables.
# The response variable in pc_data is as 'V1'.

summary(lm(V1~.-1,pc_data)) # Fit multiple linear regression model for the standarized response on the principal components.
# This shows only the 1st, 2nd and 4th principal components are significant.

final_fit = lm(V1~PC1+PC2+PC4-1,pc_data)
summary(final_fit)# Fit final multiple linear regression model with significant principal components.
# This provides our final fitted principal component regression model.


round(pca$rotation[,c(1,2,4)],2) # This provides the linear combination for selected principal components.
 
# After computing this linear combinations for all the observations, the data matrix is obtained by pc (pca$x) as given above.


#Step 9:

#normality
ols_plot_resid_qq(final_fit)
ols_test_normality(final_fit)

#homoscedasticity
ols_plot_resid_fit(final_fit) 

#random error asumption
acf(final_fit$residuals,plot=TRUE)

#Step 10:

#Outlier detection
lev = ols_plot_resid_lev(scaled_fit1)


# Computing leverage measure for all observations

# leverage points
which(lev$data$color=="leverage")

# outliers
which(lev$data$color=="outlier")

# outlier and leverage
which(lev$data$color=="outlier & leverage")


lev1=ols_plot_cooksd_bar(final_fit) # Computing Cook's distance statistics
which(lev1$data$color=="outlier")

lev2=ols_plot_dffits(final_fit) # Computing DFFFITS statistics
lev2
which(lev2$data$color=="outlier")

ols_plot_dfbetas(final_fit)# Computing DFBETAS statistics


#yoyo