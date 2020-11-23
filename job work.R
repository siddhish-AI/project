
#upload a file
f<-file.choose()
data<-read.csv(f)
data
colnames(data)
names(data)

#new data frame creation
scale_col<-data.frame(matrix(ncol=(ncol(data)), nrow=(nrow(data))))
scale_col
scale_row<-data.frame(matrix(ncol=(ncol(data)-1), nrow=(nrow(data)-1)))
#step-2
# column normalisation
i<-1
#column total
col_sum<-colSums(data)
col_sum

for (i in 1:ncol(data))
{
  scale_col[,i]<-print(data[i]/col_sum[i])
  
}
scale_col<- scale_col*100
scale_col

scale_col<-plyr::rename(scale_col, c("X1"="10006", "X2"="70010","X3"="127", "X4"="417","X5"="438", "X6"="161","X7"="40008","X8"="412","X9"="307","X10"="143","X11"="311"))
scale_col

#row normalization
library(clusterSim)
ScaledMat_row <- data.Normalization(scale_col[1:11],type="n1",normalization="row")

ScaledMat_row




#KNN matrix from correlation matrix
knn<-60 # Input (k = 60) => 60% of NN
knn<-floor(ncol(scale_col)*knn/100) #convert percentage to numbers
garb<-ifelse(knn >= ncol(scale_col)/2, print(paste(knn," is more than max KNN max available i.e. ",
                                                   floor(ncol(scale_col)/2))), print(paste("compute KNN matrix with knn = ",knn)))#Assign to avoid dup prints!

for (i in 1:ncol(scale_col)) {
  leftfill <- max(min(i-1,floor(knn/2)),((i-1)- floor(knn)))
  rightfill <- max(0,min(floor(knn),(floor(knn)-leftfill)))
  print(paste("leftfill = ", leftfill,"    righfill= ", rightfill))
}


#choose normalized_column file
#f<-file.choose()
#scale_col<-read.csv(f)
scale_col
#choose cor_coeff file
f<-file.choose()
data_cor_coeff<-read.csv(f)
data_cor_coeff
##### matrix created
data_pred<-matrix(data=NaN, nrow=(nrow(scale_col)), ncol=(ncol(scale_col)))
data_pred[is.na(data_pred)] <- 0
data_pred

# loop for forward and backward motion
for (col in 1:ncol(scale_col)){
  k<-6
  mid<-k/2
  data_sum<-0
  #data1
  print("Col is:")
  print(col)
  back<-col
  print(scale_col[,col])
  print("back before for")
  print(back)
  #backward loop
  while(back-1>0 & k>mid)
    
  {
    print("K is now:")
    print(k)
    back<-back-1
    print("back is:")
    print(back)
    
    data_cor_coeff_row<-col
    data_cor_coeff_col<-back
    print(data_cor_coeff_row)
    print(data_cor_coeff_col)
    
    data_pred[,col]<-c(data_pred[,col]+(scale_col[,back]*data_cor_coeff[data_cor_coeff_row,data_cor_coeff_col]))
    data_sum<-data_sum+data_cor_coeff[data_cor_coeff_row,data_cor_coeff_col]
    
    
    k<-k-1
    
  }
 
  #forward loop
  forward<-col+1
  print("now in forward loop")
  print(forward)
  while(k>0 & forward<=ncol(scale_col)){
    k<-k-1
    print(k)
    
    data_cor_coeff_row<-col
   
    data_cor_coeff_col<-forward
    
    data_pred[,col]<-c(data_pred[,col]+(scale_col[,forward]*data_cor_coeff[data_cor_coeff_row,data_cor_coeff_col]))
    data_sum<-data_sum+data_cor_coeff[data_cor_coeff_row,data_cor_coeff_col]
    
    forward<-forward+1
    print(forward)
  }
  
  
  
  while(k!=0){
    
    data_cor_coeff_row<-col
    
    data_cor_coeff_col<-col-k-mid
    print("......data2_col...")
    data_pred[,col]<-c(data_pred[,col]+(scale_col[,col-k-mid]*data_cor_coeff[data_cor_coeff_row,data_cor_coeff_col]))
    data_sum<-data_sum + data_cor_coeff[data_cor_coeff_row,data_cor_coeff_col]
    k<-k-1
    
  }
  
  data_pred[,col]<-data_pred[,col]/data_sum
  
}
data_pred

data_cor_coeff

scale_col
data_pred
# matrix creation(actual-predicted)/actual
data_error<-matrix(data=NaN, nrow=(nrow(scale_col)), ncol=(ncol(scale_col)))
data_error[is.na(data_error)] <- 0
data_error

data_error<-(data_pred-scale_col )/scale_col
data_error

ncol(data_error)

#upload cost values data
f<-file.choose()
data_cost<-read.csv(f)
data_cost
#matrix creation

data_missed<-matrix(data=NaN, nrow=(nrow(data_normalized_column)), ncol=(ncol(data_normalized_column)))
data_missed[is.na(data_missed)] <- 0
data_missed
for (col in 1:ncol(data_error))
{
  col
  row<-1
  
  k<- 1
while (k<nrow(data_error)+1){
    data_missed[row,col]<-sum(data_cost[,col]) * data_error[row,col]/100
 
       row<-row+1
       k<-k+1
}

col<-col-1

}

data_missed
#convert to csv
write.csv(data_x,file="c:\\Users\\siddh\\OneDrive\\Desktop\\company project\\correct_data_x.csv", row.names = FALSE)  