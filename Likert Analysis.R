row1 <- c(10,7,1,6,6) # Kategori A (1-5)
row2 <- c(8,6,1,6,9) # Kategori B (1-5)
data.table <- rbind(row1,row2)
data.table
chisq.test(data.table)
fisher.test(data.table)
# Karena nilai p > 0.05 artinya menerima hipotesa nol yang artinya tidak terdapat perbedaan antara kategori A dan Kategori B

row3 <- c(8,1,6,6,9) # Kategori A (1-5)
row4 <- c(1,0,1,17,11) # Kategori B (1-5)
data.table <- rbind(row3,row4)
data.table
chisq.test(data.table)
fisher.test(data.table)
Karena nilai p > 0.05 artinya menerima hipotesa nol yang artinya tidak terdapat perbedaan antara kategori A dan Kategori B