

#### model selection and shrinkage


	####   Subset selection methods
	##### 


	## data set Hitters.  response variable is Salary.

	library (ISLR)
	fix(Hitters )   ## show data in excel form
	names(Hitters )


	sum(is.na(Hitters$Salary))   ##  is.na()   is used to identify missing values.  



	Hitters =na.omit(Hitters )    ##  na.omit():  remove observations with missing values.
	dim(Hitters )
	
	sum(is.na(Hitters ))


	##  regsubsets():  perform best subset selection by identifying the best model that contains a given number of predictors.


	install.packages("leaps")
	library (leaps)
	regfit.full=regsubsets (Salary~.,Hitters )
	summary(regfit.full)     ## asterisk indicates that a given variable is included in the correspondingmodel


	regfit.full=regsubsets (Salary~.,data=Hitters ,nvmax =19)   ## nvmax=:  specify the maximum number of predictors in the model
	reg.summary =summary(regfit.full) 


	names(reg.summary )   ## show the names of the output in reg.summary


	reg.summary$rsq
	reg.summary$cp



	par(mfrow =c(2,2))
	plot(reg.summary$rss ,xlab=" Number of Variables ",ylab=" RSS",type="l")  ## plot RSS against the number of variables.
	plot(reg.summary$adjr2 ,xlab =" Number of Variables ",ylab=" Adjusted RSq",type="l")   ##  plot Adjusted R^2



	which.max(reg.summary$adjr2)   ## identify the model with the largest adjusted R^2

	points(11, reg.summary$adjr2[11], col ="red",cex =2, pch =20)   ## show this model on the plot previously obtained.




	plot(reg.summary$cp,xlab ="Number of Variables ",ylab="Cp",type="l")   ## plot C_p
	which.min(reg.summary$cp )

	points(10, reg.summary$cp[10], col ="red",cex =2, pch =20)
	which.min(reg.summary$bic )

	plot(reg.summary$bic ,xlab=" Number of Variables ",ylab="BIC",type="l")   ## plot BIC
	points(6, reg.summary$bic[6], col ="red",cex =2, pch =20)


	## display the selected variables for the best model with a given number of predictors
	plot(regfit.full ,scale ="r2")
	plot(regfit.full ,scale ="adjr2")
	plot(regfit.full ,scale ="Cp")
	plot(regfit.full ,scale ="bic")



	


#################################################
####  Forward and Backward Stepwise Selection
#################################################



	regfit.fwd=regsubsets(Salary~.,data=Hitters ,nvmax =19,method ="forward")
	summary(regfit.fwd )

	regfit.bwd=regsubsets(Salary~.,data=Hitters ,nvmax =19,method ="backward")
	summary (regfit.bwd )


	coef(regfit.full ,7)   ## show coefficient of model with 7 predictors

	coef(regfit.fwd ,7)     ## show coefficient of model with 7 predictors

	coef(regfit.bwd ,7)    ## show coefficient of model with 7 predictors




	### select best model using validation set and cross-validation


	## validation set method
	set.seed (1)
	train=sample(c(TRUE ,FALSE), nrow(Hitters ),rep=TRUE)
	test =(!train )

	regfit.best=regsubsets (Salary~.,data=Hitters [train ,],nvmax =19)   ##  train the model on training data
	test.mat=model.matrix(Salary~.,data=Hitters [test ,])   ## test data


	val.errors =rep(NA ,19)

	## calculate the test error of each model

	for(i in 1:19){
		coefi=coef(regfit.best,id=i)
		pred=test.mat[,names(coefi)]%*%coefi
		val.errors[i]= mean(( Hitters$Salary[test]-pred)^2)
	}


	val.errors

	which.min (val.errors )  ## identify the best model

	coef(regfit.best ,10)




	## K-fold cross-validation method to select the best model


	k=10
	set.seed (1)
	folds=sample(1:k,nrow(Hitters),replace=TRUE)   ## randomly divide into k groups
	cv.errors=matrix(NA,k,19, dimnames =list(NULL , paste (1:19) ))

	### predict() cannot be applied to regsubsets(), we need to define a predict function
	predict.regsubsets =function (object ,newdata ,id ){
		form=as.formula (object$call [[2]])
		mat=model.matrix(form ,newdata )
		coefi =coef(object ,id=id)
		xvars =names(coefi )
		mat[,xvars ]%*% coefi
	}

	for(j in 1:k){
		best.fit=regsubsets(Salary~.,data=Hitters[folds !=j,],nvmax =19)
		for(i in 1:19){
			pred=predict(best.fit,Hitters[folds==j,],id=i)
			cv.errors[j,i]=mean((Hitters$Salary[folds==j]-pred)^2)
		}
	}

	mean.cv.errors =apply(cv.errors ,2, mean)
	mean.cv.errors

	par(mfrow =c(1,1))
	plot(mean.cv.errors ,type="b")  ## model with 11 variables is the best


	reg.best=regsubsets (Salary~.,data=Hitters , nvmax =19)
	coef(reg.best ,11)













###########################################
####   Ridge Regression and the Lasso
##########################################

	
	## ridge regression
	#use glmnet package and Hitters data set

	install.packages("glmnet")
	library(glmnet)


	x=model.matrix(Salary~.,Hitters)[,-1]   ### model.matrix():  get the input variables as a matrix.
	y=Hitters$Salary					## missing values should be removed

	grid =10^seq(10,-2, length =100)

	## glmnet(): alpha=0 is ridge regression;  alpha=1 is lasso regression
	## by default, the X is standardized.
	ridge.mod =glmnet(x,y,alpha =0, lambda =grid)  ## the input variables must be a matrix X.
	dim(coef(ridge.mod ))   ## coefficients for 100 lambda

	ridge.mod$lambda[50]  ## l2 norm of coefficient corresponds to lambda[50]
	coef(ridge.mod)[,50]
	sqrt(sum(coef(ridge.mod)[ -1 ,50]^2) )

	ridge.mod$lambda [60]
	coef(ridge.mod)[,60]
	sqrt(sum(coef(ridge.mod)[ -1 ,60]^2) )   ## norm is larger than lambda[50]

	predict (ridge.mod ,s=50, type ="coefficients")[1:20 ,]  ## use the model with lambda[50] to predict



	#### use test data to evaluate test error of ridge regression and lasso

	set.seed (1)
	train=sample (1: nrow(x), nrow(x)/2)
	test=(- train )
	y.test=y[test]

	ridge.mod =glmnet (x[train ,],y[train],alpha =0, lambda =grid ,thresh =1e-12)
	ridge.pred=predict (ridge.mod ,s=4, newx=x[test ,])
	mean(( ridge.pred -y.test)^2)

	ridge.pred=predict(ridge.mod ,s=1e10 ,newx=x[test ,])   ## use different lambda
	mean(( ridge.pred -y.test)^2)


	ridge.pred=predict(ridge.mod,s=0, newx=x[test ,])  ### s=0:  least square estimate
	 mean(( ridge.pred -y.test)^2)



	## CV to select best lambda
	set.seed (1)
	cv.out =cv.glmnet (x[train ,],y[train],alpha =0)
	plot(cv.out)
	bestlam =cv.out$lambda.min
	bestlam

	# use the best lambda to predict
	ridge.pred=predict (ridge.mod ,s=bestlam ,newx=x[test ,])
	mean(( ridge.pred -y.test)^2)

	## use the best lambda to full data set
	out=glmnet (x,y,alpha =0)
	predict (out ,type="coefficients",s=bestlam )[1:20 ,]




	## lasso 
	lasso.mod =glmnet(x[train ,],y[train],alpha =1, lambda =grid) ### note: alpha=1 corresponds to lasso

	plot(lasso.mod)




	set.seed (1)
	cv.out =cv.glmnet(x[train ,],y[train],alpha =1)  ### note: alpha=1 corresponds to lasso
	plot(cv.out)
	bestlam =cv.out$lambda.min
	lasso.pred=predict(lasso.mod,s=bestlam ,newx=x[test ,])
	mean(( lasso.pred -y.test)^2)

	





































































