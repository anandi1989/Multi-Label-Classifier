#Author: Amit Nandi
#Dated : 7th Feb, 2017

library(caret)
library(e1071)
library(foreign)

BR_model <- function(data_set,test_data_set,num_lebel)
{
	#Input : 
	#    data_set      : tabular data with target labels at the end to train the model
	#    test_data_set : tabular data without target
	#    num_label     : number of labels present in the dataset
	#Output : matrix of all predicted label
  	
	#Extracting X attributes
	num_data_x <- ncol(data_set) - num_lebel
	data_set_x <- data_set[,1:num_data_x]
	#Extracting Y lebel
	data_set_y <- data_set[,(num_data_x + 1):ncol(data_set)]

	#Extracting X and Y lebel of test data
	predicted_mat <- matrix(nrow = nrow(test_data_set), ncol = num_lebel)
	
	for(j in 1:num_lebel)
	{
		cat("\nCreating Model:", j)
		model <- svm(data_set_x,as.factor(data_set_y[,j]), kernel = "radial")
		cat("\npredicting on test data")
		pred <- predict(model,data.matrix(test_data_set))
		predicted_mat[,j] <- as.numeric(paste(pred))
		cat("\nend")
	} #End of binary relevance

	return(predicted_mat)
}
