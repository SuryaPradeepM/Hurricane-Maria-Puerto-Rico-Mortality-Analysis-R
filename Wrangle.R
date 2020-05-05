library(tidyverse)
library(pdftools)
library(ggplot2)
library(dslabs) #Package by Harvard Data Science containing datasets including RD Mortality Report pdf
options(digits = 3)    # report 3 significant digitslibrary(tidyverse)

#Stores the path to the report file in the dslabas/extdata directory,Alternatively change it as required.
fn <- system.file("extdata", "RD-Mortality-Report_2015-18-180531.pdf", package="dslabs")

system("cmd.exe", input = paste("start", fn))

text <- pdf_text(fn)

#Getting page 9, entry 9 from text which corresponds to the month of September as we're interested in
#Hurricane Maria which occured on Sep 20,2017
x  <- str_split(text[9],pattern="\n")
s <- x[[1]]
s <- str_trim(s)

header_index <- as.numeric(str_which(s,"2015")[1])
header_mat <- str_split(s[header_index],"\\s+",simplify = TRUE)
month<-header_mat[1]
header<-header_mat[-1]

tail_index  <- str_which(s, "Total")
tail_index

n <- str_count(s, "\\d+")

#Cut out strings before header and after tail and strings with single numbers
out <- c(1:header_index, which(n==1), tail_index:length(s))
s <- s[-out]

#Remove all non numeric characters in the strings
pattern <- "[^\\d\\s]"
str_remove_all(s,pattern)

s_mat <- str_split_fixed(s, "\\s+", n = 6)[,1:5]

col_names<-c("day",header)

tab <- data.frame(s_mat)%>%
  set_names(col_names)%>%
  mutate_all(as.numeric)

#Change to tidy format
tab <- tab %>% gather(year, deaths, -day) %>%
  mutate(deaths = as.numeric(deaths))

tab %>% filter(year < 2018) %>% 
  ggplot(aes(day, deaths, color = year)) +
  geom_line() +
  geom_vline(xintercept = 20) +
  geom_point()+
  xlab("Day of September")+
  ylab("Deaths per day in years 2015,2016 & 2017")
