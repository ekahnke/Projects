#all
mentions = read.csv("mentions_binary.csv")
hashtags = names(mentions)
lift_mat = mat.or.vec(ncol(mentions), ncol(mentions)) 
#calulate lifts
for (i in 1:length(hashtags)){
  for(j in 1:length(hashtags)){
    both = mentions[,i] & mentions[,j]
    lift_mat[i,j] = sum(both)*nrow(mentions)/(sum(mentions[,i])*sum(mentions[,j]))
  }
}
lift = as.data.frame(lift_mat, row.names = hashtags, col.names = hashtags)
colnames(lift) = hashtags
write.csv(lift, file = "Lift Matrix.csv")

#1. SB Start
mentions = read.csv("mentions_binary1.csv")
hashtags = names(mentions)
lift_mat = mat.or.vec(ncol(mentions), ncol(mentions)) 
#calulate lifts
for (i in 1:length(hashtags)){
  for(j in 1:length(hashtags)){
    both = mentions[,i] & mentions[,j]
    lift_mat[i,j] = sum(both)*nrow(mentions)/(sum(mentions[,i])*sum(mentions[,j]))
  }
}
lift = as.data.frame(lift_mat, row.names = hashtags, col.names = hashtags)
colnames(lift) = hashtags
write.csv(lift, file = "Lift Matrix1.csv")

#2. SB End
mentions = read.csv("mentions_binary2.csv")
hashtags = names(mentions)
lift_mat = mat.or.vec(ncol(mentions), ncol(mentions)) 
#calulate lifts
for (i in 1:length(hashtags)){
  for(j in 1:length(hashtags)){
    both = mentions[,i] & mentions[,j]
    lift_mat[i,j] = sum(both)*nrow(mentions)/(sum(mentions[,i])*sum(mentions[,j]))
  }
}
lift = as.data.frame(lift_mat, row.names = hashtags, col.names = hashtags)
colnames(lift) = hashtags
write.csv(lift, file = "Lift Matrix2.csv")

#3 Post SB
mentions = read.csv("mentions_binary3.csv")
hashtags = names(mentions)
lift_mat = mat.or.vec(ncol(mentions), ncol(mentions)) 
#calulate lifts
for (i in 1:length(hashtags)){
  for(j in 1:length(hashtags)){
    both = mentions[,i] & mentions[,j]
    lift_mat[i,j] = sum(both)*nrow(mentions)/(sum(mentions[,i])*sum(mentions[,j]))
  }
}
lift = as.data.frame(lift_mat, row.names = hashtags, col.names = hashtags)
colnames(lift) = hashtags
write.csv(lift, file = "Lift Matrix3.csv")

#4 Midweek
mentions = read.csv("mentions_binary4.csv")
hashtags = names(mentions)
lift_mat = mat.or.vec(ncol(mentions), ncol(mentions)) 
#calulate lifts
for (i in 1:length(hashtags)){
  for(j in 1:length(hashtags)){
    both = mentions[,i] & mentions[,j]
    lift_mat[i,j] = sum(both)*nrow(mentions)/(sum(mentions[,i])*sum(mentions[,j]))
  }
}
lift = as.data.frame(lift_mat, row.names = hashtags, col.names = hashtags)
colnames(lift) = hashtags
write.csv(lift, file = "Lift Matrix4.csv")

#5 Weekend
mentions = read.csv("mentions_binary5.csv")
hashtags = names(mentions)
lift_mat = mat.or.vec(ncol(mentions), ncol(mentions)) 
#calulate lifts
for (i in 1:length(hashtags)){
  for(j in 1:length(hashtags)){
    both = mentions[,i] & mentions[,j]
    lift_mat[i,j] = sum(both)*nrow(mentions)/(sum(mentions[,i])*sum(mentions[,j]))
  }
}
lift = as.data.frame(lift_mat, row.names = hashtags, col.names = hashtags)
colnames(lift) = hashtags
write.csv(lift, file = "Lift Matrix5.csv")

#SB Day
mentions = read.csv("mentions_binarySB.csv")
hashtags = names(mentions)
lift_mat = mat.or.vec(ncol(mentions), ncol(mentions)) 
#calulate lifts
for (i in 1:length(hashtags)){
  for(j in 1:length(hashtags)){
    both = mentions[,i] & mentions[,j]
    lift_mat[i,j] = sum(both)*nrow(mentions)/(sum(mentions[,i])*sum(mentions[,j]))
  }
}
lift = as.data.frame(lift_mat, row.names = hashtags, col.names = hashtags)
colnames(lift) = hashtags
write.csv(lift, file = "Lift MatrixSB.csv")
