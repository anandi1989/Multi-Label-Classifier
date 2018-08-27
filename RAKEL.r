#Author: Amit Nandi
#Dated : 28th Aug, 2016

##########################

library(caret)
library(e1071)
library(foreign)

vote <- function(y_input,comb_mat,lebel,k_value, m_value)
{
	#Input : 
        #    y_input       : output matrix of each grouping
        #    comb_mat      : Possible combinations between target labels matrix
        #    label         : number of labels present in the dataset
	#    k_value       : grouping value
	#    m_value       : total count of combination
        #Output : matrix of all predicted label after voting

	cat("\nconverting lebel from multiclass to multilabel...")
	final_mat <- matrix(0,nrow = nrow(y_input), ncol = lebel)
	for(i in 1:nrow(y_input))
	{
		convert_mat <- matrix(0,nrow = m_value, ncol = lebel)
		for(j in 1:m_value)
		{
			bits_conv <- as.numeric(intToBits(y_input[i,j] - 1))
			bits_conv <- bits_conv[1:k_value]
			for(k in 1:k_value)
			{
				index_num <- comb_mat[j,k]
				convert_mat[j,index_num] <- bits_conv[k] 
			}
		}
		
		col_sum <- colSums(convert_mat)
		for(l in 1:length(col_sum))
		{
			#condition of majority vote
			if(col_sum[l]> (lebel/2)) 
				final_mat[i,l] <- 1
		}
		
	}
	
	return(final_mat)
}

RAkEL_model <- function(data_set,test_data_set,num_lebel,k)
{
	#Input : 
        #    data_set      : tabular data with target labels at the end to train the model
        #    test_data_set : tabular data without target
        #    num_label     : number of labels present in the dataset
	#    k		   : grouping value that satisfy following condition -- (num_label<= k =>2)
        #Output : matrix of all predicted label	


	#total number of possible combination of lebelset
	m <- choose(num_lebel,k) #possible models

	vector_of_lebel <- 1:num_lebel

	#fixing the possible combination
	comb_value <- t(combn(vector_of_lebel,k)) #row: number of models.

	source("k_lebel_data_set.r")

	k_lebel_data_set <- k_lebel_data_set(data_set,num_lebel,k,m,comb_value)
	cat("\nend of calling k_lebel_data_set")

	pred_model_mat <- matrix(nrow=nrow(test_data_set),ncol=length(k_lebel_data_set))	
	for(j in 1:length(k_lebel_data_set))
	{

		k_data <- k_lebel_data_set[[j]]
		k_data_X <- k_data[,1:(ncol(k_data) - 1)]
		k_data_Y <- as.factor(k_data[,ncol(k_data)]) #for classification purpose passing lebel value as factor
		#running SVM model
		model <- svm(k_data_X,k_data_Y,kernel = "radial")
		pred <- predict(model, test_data_set)
		pred_model_mat[,j] <- pred
	}

	y_lebel_output <- vote(pred_model_mat,comb_value,num_lebel,k,m)

	return(y_lebel_output)
}


