mentions = read.csv("mentions_binary.csv")
car_models = names(mentions)
lift_mat = mat.or.vec(ncol(mentions), ncol(mentions)) 

#calulate lifts
for (i in 1:length(car_models)){
  for(j in 1:length(car_models)){
    both = mentions[,i] & mentions[,j]
    lift_mat[i,j] = sum(both)*nrow(mentions)/(sum(mentions[,i])*sum(mentions[,j]))
  }
}

lift = as.data.frame(lift_mat, row.names = car_models, col.names = car_models)
colnames(lift) = car_models
write.csv(lift, file = "Lift calculations.csv")