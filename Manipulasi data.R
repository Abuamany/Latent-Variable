library(dplyr)
read.csv("mammals.csv", header=T, sep=",")
dplyr::tbl_df(mammals)
mammals#Melihat data
glimpse(mammals)

#Manipulasi Data Kolom
x<-select(mammals, adult_head_body_len_mm)#Memilih 1 column
y<-na.omit(x)
View(y)
summary(y)
str(y)
mean(y$adult_head_body_len_mm, na.rm = TRUE)
hist(y$adult_head_body_len_mm, na.rm = TRUE)

select(mammals, adult_head_body_len_mm, litter_size)#Memilih 2 column
select(mammals, adult_head_body_len_mm:litter_size)#Memilih range pada column tertentu
select(mammals, -adult_head_body_len_mm)#Menghapus 1 column
select(mammals, contains("body"))#Memilih Column dengan kata tertentu
select(mammals, starts_with("adult"))#Memilih Column dengan awal kata"adult"
select(mammals, ends_with("g"))#Memilih Column dengan akhir kata"g"
select(mammals, 1:3)#memilih column 1 sampai 3
#Manipulasi Data Baris
filter(mammals, adult_body_mass_g > 1e7)[ , 1:3]
filter(mammals, species == "Balaena mysticetus")
filter(mammals, order == "Carnivora" & adult_body_mass_g < 200)
arrange(mammals, adult_body_mass_g)[ , 1:3]
arrange(mammals, desc(adult_body_mass_g))[ , 1:3]#sort descending
arrange(mammals, order, adult_body_mass_g)[ , 1:3]
#Merubah Variabel Baru
glimpse(mutate(mammals, adult_body_mass_kg = adult_body_mass_g / 1000))#membuat data baru dengan perhitungan
glimpse(mutate(mammals, g_per_mm = adult_body_mass_g / adult_head_body_len_mm))
glimpse(mutate(mammals, g_per_mm = adult_body_mass_g / adult_head_body_len_mm, kg_per_mm = g_per_mm / 1000))
#Menyimpulkan Data Kolom
summarise(mammals, mean_mass = mean(adult_body_mass_g, na.rm = TRUE))
head(summarise(group_by(mammals, order), mean_mass = mean(adult_body_mass_g, na.rm = TRUE)))# berdasarkan group

mammals %>% arrange(adult_body_mass_g)
mammals %>%mutate(mass_to_length = adult_body_mass_g / adult_head_body_len_mm) %>% arrange(desc(mass_to_length)) %>% select(species, mass_to_length)
select(arrange(mutate(mammals, mass_to_length = adult_body_mass_g / adult_head_body_len_mm), desc(mass_to_length)),species, mass_to_length)
#syntax 43 lbh pendek dibanding syntax 44

mammals %>% group_by(order) %>% summarise(median_litter = median(litter_size, na.rm = TRUE)) %>% filter(median_litter > 3) %>% arrange(desc(median_litter)) %>%select(order, median_litter)
