# Anggota :
# Muhammad Khairii Sufyaan (5003201077)
# Zaky Izmi Syakura (5003201148)


# PCA dan FAKTOR ANALYSIS DATA KUALITAS AIR SUNGAI SUANGI CHINA

data <- read.csv("D:/MK/analisis data/Water Quality Suangi China.csv", sep=";")
View(data)
data = data[, -1:-2]

################################################################################

# PCA

library(psych)
r <- cor(data)
KMO(mat)
bartlett.test(data)

library(svd)
pc <- eigen(r)
p <- ncol(data);p

##Menghitung proporsi varians dan kumulatifnya
sumvar <- sum(pc$values)

##Menempatkan hasil proporsi variance dan cumulative
vcv <- matrix(0,p,2)

##Mendapatkan nilai PC
for(i in 1:p){
  vcv[i,1] <- pc$values[i]/sumvar
  if (i == 1) {
    vcv[i,2] <- vcv[i,1]}
  else{
    vcv[i,2] <- vcv[i-1,2]+vcv[i,1]
  }
}
vcv <- as.data.frame(vcv)
colnames(vcv) <- c("Proportion of Variance","Cumulative Proportion")
rownames(vcv) <- paste0("PC",c(1:p))
print(vcv)

#prcomp
PCA.mod <- prcomp(data, center=TRUE, scale=TRUE)
summary(PCA.mod)

screeplot(PCA.mod, type="l")
cumpro <- cumsum(PCA.mod$sdev^2 / sum(PCA.mod$sdev^2))
plot(cumpro, xlab="PC#", ylab="Amount of explained variance", main="Cumulative variance plot")

PCA.mod$rotation
PCA.mod$x

################################################################################

# FA
mat = cor(data)

stdev <- sqrt(eigen(mat)$values)
eigenvec <- eigen(mat)$vectors

plot(eigen(mat)$values, ylab="Amount of explained variance")
abline(h = 1, col="blue")

factors <- 7

#loadings
loadings <- eigenvec[,1:factors] %*% diag(stdev[1:factors])
loadings

#rotasi pakai varimax
rotated.loadings <- varimax(loadings)
rotated.loadings
