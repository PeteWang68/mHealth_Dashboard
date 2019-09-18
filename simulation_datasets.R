library(tidyverse)
##### phonecall datasets
phonecall = c(1:365) %>% as.matrix()

Nrep = 24

for (i in 1:Nrep){
  a = sample(10, 365, replace = T) %>% as.matrix()
  phonecall = cbind(phonecall, a)
}
  
phonecall_01 = phonecall %>% as.data.frame()
phonecall_02 = phonecall %>% as.data.frame()
phonecall_03 = phonecall %>% as.data.frame()
phonecall_04 = phonecall %>% as.data.frame()
phonecall_05 = phonecall %>% as.data.frame()

phonecall = rbind(phonecall_01, phonecall_02, phonecall_03, phonecall_04, phonecall_05)
write.csv(phonecall, file = "./dashboard/data/phonecall.csv", row.names = FALSE)

##### text datasets
text = c(1:365) %>% as.matrix()

Nrep = 24

for (i in 1:Nrep){
  a = sample(10, 365, replace = T) %>% as.matrix()
  text = cbind(text, a)
}

text_01 = text %>% as.data.frame()
text_02 = text %>% as.data.frame()
text_03 = text %>% as.data.frame()
text_04 = text %>% as.data.frame()
text_05 = text %>% as.data.frame()

text = rbind(text_01, text_02, text_03, text_04, text_05)
write.csv(text, file = "./dashboard/data/text.csv", row.names = FALSE)
