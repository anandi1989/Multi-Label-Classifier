#Author: Amit Nandi
#Dated: 23rd Nov, 2016

library(e1071)
library(caret)

### --- binary to decimal converter --- ###
binaryToDecimal <- function(x)
{
  return(sum( rev(x)* 2^(rev(seq_along(x)) - 1)))

}

LP_model <- function(data_set,test_data_set,label)
{
	#Input : 
        #    data_set      : tabular data with target labels at the end to train the model
        #    test_data_set : tabular data without target
        #    label     : number of labels present in the dataset
        #Output : matrix of all predicted label

	col_num <- ncol(data_set)
	attr_col_num <- (col_num - label)

	data_x <- data_set[,1:attr_col_num]
	data_y <- data_set[,(attr_col_num + 1):col_num]
	
	multiclass_vect <- NULL
	for(c in 1:nrow(data_y))
	{
		bD <- binaryToDecimal(data_y[c,])
		multiclass_vect <- c(multiclass_vect,bD)
	}
	
	multiclass_vect <- as.factor(multiclass_vect)
	model <- svm(data_x,multiclass_vect,kernal="radial")
	pred <- as.numeric(predict(model,test_data_set))
	
	y_lebel_output <- NULL
	for(b in 1:length(pred))
	{
		bits <- as.numeric(intToBits(pred[b]))
		y_lebel_output <- rbind(y_lebel_output,bits[1:label])
	}

	return(y_lebel_output)
}
