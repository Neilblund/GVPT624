library(dataverse)
library(tidyverse)

# Accessing the GTD ideology data
# codebook:
#	Global Terrorism Database Ideology in the United States Auxiliary Dataset 2017.pdf




if(!file.exists('gtd.xlsx')){
  download.file('https://www.start.umd.edu/system/files/globalterrorismdb_0522dist.xlsx', mode='wb',
                
                destfile = 'gtd.xlsx'
  )  
}

if(!file.exists('gtd_codebook.pdf')){
  download.file('https://www.start.umd.edu/sites/default/files/2024-10/Codebook.pdf', mode='wb',
                destfile = 'gtd_codebook.pdf'
  )
  
}

gtd<-readxl::read_xlsx('gtd.xlsx')


suicide_by_year<-gtd|>
  filter(nkill >0)|>
  mutate(month_year = as.Date(paste0(iyear, "-", imonth, "-01")))|>
  count(month_year, suicide)|>
  complete(month_year=seq.Date(min(month_year, na.rm=T), max(month_year, na.rm=T), by='month'), 
           nesting(suicide),
           
           fill=list(n=0) )|>
  mutate(suicide = factor(suicide, labels=c("Conventional", "Suicide")))
  

citation<-'START (National Consortium for the Study of Terrorism and Responses to Terrorism). (2022). Global Terrorism Database 1970 - 2020 [data file]. https://www.start.umd.edu/data-tools/GTD'

suicide_by_year|>
  ggplot(aes(x=month_year, y=n , color=suicide)) + 
  geom_point(alpha=.3) +
  geom_smooth(se=FALSE) +
  theme_bw() +
  scale_color_manual(values = c('black', 'red')) +
  labs(x = 'Month',
       y ='number of attacks',
       title = 'Monthly total suicide and conventional attack. Globally. 1970-2020',
       subtitle = "(attacks with at least one death)",
       caption =citation
       )


# Attacks and ideology -----

# downloading and merging with ideology data for US attacks

gtd_ideology <- get_dataframe_by_name(
  filename = "globalterrorismdb_USideology_1970-2016.tab",
  dataset = "doi:10.7910/DVN/SACQNK",
  server = "dataverse.harvard.edu")



# merging with GTD data based on event ID
gtd_merged<-gtd_ideology|>
  left_join(gtd, by='eventid')



# filtering responses to include only left/right ideology
gtd_ideology_long<-gtd_merged|>
  pivot_longer(cols=ENVIRONMENTAL:`SINGLE`, names_to='ideology')|>
  filter(value ==1)


# calculating annual attacks/deaths/injuries by ideology 
left_right_attacks<-gtd_ideology_long|>
  drop_na(iyear)|>
  filter(ideology == "LEFT-WING" | ideology=="RIGHT-WING")|>
  group_by(iyear, ideology)|>
  summarise(total = n(),
            number_killed = sum(nkill, na.rm=T),
            number_injured = sum(nwound, na.rm=T)
            
            )|>
  ungroup()|>
  # filling missing years with "0"
  complete(iyear = min(iyear):max(iyear), nesting(ideology), 
           fill=list(total = 0, number_killed=0, number_injured = 0))
  


citation <- 'Miller, Erin, 2017, "Global Terrorism Database Ideological Motivations of Terrorism in the United States", https://doi.org/10.7910/DVN/SACQNK, Harvard Dataverse, V2'

# plotting attacks by year
left_right_attacks|>
ggplot(aes(x = iyear, y= total, color=ideology)) + 
  geom_line(lwd=1) +
  geom_point() +
  theme_bw() + 
  labs(title = 'Attacks by year and ideology',
       subtitle = '(1970 - 2016)',
       caption = citation,
       y = 'attacks',
       x = 'year'
      
       ) +
  theme(plot.caption = element_text(hjust = 0)) +
  scale_color_manual(values = c('black', 'red'))


# plotting attacks by year since 1990
left_right_attacks|>
  filter(iyear>1990)|>
  ggplot(aes(x = iyear, y= total, fill=ideology)) + 
  geom_area(alpha=.6) + 
  theme_bw() + 
  labs(title = 'Attacks by year and ideology',
       subtitle = '(1970 - 2016)',
       caption = citation,
       y = 'attacks',
       x = 'year'
       
  ) +
  theme(plot.caption = element_text(hjust = 0)) +
  scale_fill_manual(values = c('black', 'red'))








