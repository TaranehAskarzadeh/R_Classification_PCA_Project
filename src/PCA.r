

###  PCA and K-mean clustering
	
###############################
##  Principal Components Analysis
##################################	
	## USArrests data set

	states =row.names(USArrests )  ## show row names: each row is a state
	states

	names(USArrests )

	apply(USArrests , 2, mean)  ## find column means.

	apply(USArrests , 2, var)   ## find column variance.
	
	## prcomp():  perform PCA analysis
	pr.out =prcomp (USArrests , scale =TRUE)  ## scale=TRUE: scale variables to have standard variance one.

	names(pr.out )

	pr.out$center   ## show the means used to scale the variable
	pr.out$scale	## show the variances used to scale the variable

	pr.out$rotation	## show the loading vector of each component

	pr.out$x   ## show the scores of each principal component
	dim(pr.out$x )

	biplot (pr.out , scale =0)  ## plot the first two components. scale=0: show the loadings


	## principal components are unique up to negative or positive sign. We can change the sign.

	pr.out$rotation=-pr.out$rotation
	pr.out$x=-pr.out$x
	biplot (pr.out , scale =0)


	pr.out$sdev  ## show the standard deviation of each component

	pr.var =pr.out$sdev ^2  ## varaince
	pr.var

	pve=pr.var/sum(pr.var )  ## variance explained by each component
	pve

	## plot PVE
	plot(pve , xlab=" Principal Component ", ylab=" Proportion ofVariance Explained ", ylim=c(0,1) ,type="b")

	## plot cumulative PVE.  cumsum(): calculate the cumulative sum.
	plot(cumsum(pve ), xlab=" Principal Component ", ylab ="Cumulative Proportion of Variance Explained ", ylim=c(0,1) ,type="b")




###############################
##  K-Means Clustering
##################################	


	## kmeans(): fit k-mean clustering


	set.seed (2)
	x=matrix (rnorm (50*2) , ncol =2)
	x[1:25 ,1]=x[1:25 ,1]+3
	x[1:25 ,2]=x[1:25 ,2] -4

	km.out =kmeans (x,2, nstart =20)  ## K=2;  nstart=20: run K-mean clustering 20 times and select the best one.
	
	km.out$cluster  ## show cluster of each observation

	plot(x, col =(km.out$cluster +1) , main="K-Means Clustering Results with K=2", xlab ="", ylab="", pch =20, cex =2)

	## change K=2 to K=3
	set.seed (4)
	km.out =kmeans (x,3, nstart =20)
	km.out


	## try different nstart
	set.seed (3)
	km.out =kmeans (x,3, nstart =1)
	km.out$tot.withinss   ## show the smallest within-class variation
	
	km.out =kmeans (x,3, nstart =20)
	km.out$tot.withinss




###############################
##  PCA applied to NCI60 gene data
##################################	


	## NCI60:  n=64 , p=6830

	library (ISLR)
	nci.labs=NCI60$labs    ##  cancer type
	nci.data=NCI60$data


	dim(nci.data)

	nci.labs [1:4]


	pr.out =prcomp (nci.data , scale=TRUE)

	## plot the principal components
	### assign a color to each element of vector "vec".
	Cols=function (vec ){
		cols=rainbow (length (unique (vec )))
		return(cols[as.numeric (as.factor (vec))])
	}

	par(mfrow =c(1,2))
	plot(pr.out$x [,1:2], col =Cols(nci.labs), pch =19,xlab ="Z1",ylab="Z2")      ## plot Z1,Z2
	plot(pr.out$x[,c(1,3) ], col =Cols(nci.labs), pch =19,xlab ="Z1",ylab="Z3")   ## plot Z1,Z3

	summary (pr.out)  ## outpt PVE, cumulative PVE and so on.
	plot(pr.out)  ## plot PVE


	pr.out$sdev

	## plot PVE and cumulative PVE
	pve =100* pr.out$sdev ^2/ sum(pr.out$sdev ^2)
	par(mfrow =c(1,2))
	plot(pve , type ="o", ylab="PVE ", xlab=" Principal Component ",col =" blue")
	plot(cumsum (pve ), type="o", ylab =" Cumulative PVE", xlab="Principal Component ", col =" brown3 ")

































