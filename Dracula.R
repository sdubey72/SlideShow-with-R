
#install gutenbergr first

drac<-gutenberg_download(345)

#pull tidytext
#pull dplyr

#Make a bar plot with top 10 +ve/-ve words 

drac_words<-drac%>%
  unnest_tokens(word,text)

bing<-get_sentiments('bing')
#bing<-get_sentiments('bing')

drac_words<-inner_join(drac_words,bing)
#drac_words<-inner_join(drac_words,bing,by='words')

drac_words$gutenberg_id<-NULL

#-----------------------------------------------------------------------------
drac_pos<-drac_words%>%
  filter(sentiment=='positive')%>%
  group_by(word)%>%
  summarize(count=n(),sentiment=first(sentiment))%>%
  arrange(count)%>%
  #filter(count>=66)
  top_n(10,wt=count)

drac_pos$word<-factor(drac_pos$word,level=drac_pos$word)

ggplot()+
  geom_bar(data = drac_pos,aes(x=word,y=count),stat='identity')+
  coord_flip()

#-------------------------------------------------------------------------------

drac_neg<-drac_words%>%
  filter(sentiment=='negative')%>%
  group_by(word)%>%
  summarize(count=n(),sentiment=first(sentiment))%>%
  arrange(count)%>%
  filter(word!='miss')%>%
  #filter(count>=49)
  top_n(10,wt=count)

drac_neg$word<-factor(drac_neg$word,level=drac_neg$word)

ggplot()+
  geom_bar(data = drac_neg,aes(x=word,y=count),stat='identity')+
  coord_flip()
#------------------------------------------------------------------------------

#rbind is binding two tables by row

drac_compare<-rbind(drac_pos,drac_neg)

#facet_wrap(~sentiment) - to create two graphs based on sentiment columns
ggplot()+
  geom_bar(data=drac_compare,aes(x=word,y=count,fill=sentiment,color=sentiment),stat = 'identity')+
  coord_flip()+ # flip the coordinates
  facet_wrap(~sentiment,scales = 'free_y')+ #creates two graphs independent of y
  scale_fill_manual(values=c('green','yellow'))+ #define the color to be filled inside the bar graph
  scale_color_manual(values = c('red','pink')) # defines the color of the boundaries
#---------------------------------------------------------------------------------
