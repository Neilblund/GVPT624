library(tidyverse)
library(countrycode)
# Codebook: https://correlatesofwar.org/wp-content/uploads/IGO-Codebook_v3_short-copy.pdf

url <- 'https://correlatesofwar.org/wp-content/uploads/igo_year_formatv3.zip'


if(!file.exists('igo_year_formatv3.zip')){
  cowdata<-download.file(url = url, destfile ='igo_year_formatv3.zip', mode='wb')
  
}

igo_members<-read_tsv(unzip('igo_year_formatv3.zip', "igo_year_formatv3.csv"))


igo_members|>
  select(year, economic, political, social)|>
  
  pivot_longer(cols=c(economic, political,social), names_to  = 'type')|>
  filter(value ==1)|>
  count(year, type)|>
  ggplot(aes(x= year, y=n, fill=type)) + geom_area(alpha=.8) +
  theme_bw() +
  scale_fill_manual(values=c("orange",'lightblue', "grey")) +
  labs(title ='IGOs by type and year',
       y='count'
       ) 
  


# converting to the country-IGO-year format used in the Davis and Pratt paper:

labels <- c("No membership","Full membership","Associate membership",
            "Observer","Missing","State not system member")

levels <-c(0, 1, 2, 3, -9, -1)

# converting to country-year format
igo_long<-igo_members|>
  pivot_longer(cols=c(afghanistan:zimbabwe), names_to='country',values_to='membership')|>
  # add cowcodes (would need a little manual cleanup here)
  mutate(cowcode = countrycode(country, origin='country.name', destination='cown'),
         
         # add membership status labels
         membership = factor(membership, levels=levels, labels=labels)
         )





