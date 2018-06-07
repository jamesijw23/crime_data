library(ggplot2)
library(tidyr)

kill_injured = gun_data_mod1 %>%
  group_by(state)%>%
  summarise(d_state = sum(n_killed),i_state = sum(n_injured)) %>%
  arrange(desc(d_state)) %>%
  head(n=10)

gathered_ki = gather(kill_injured,type,number,-state)
gathered_ki$type = ifelse(gathered_ki$type =="i_state","Injured","Killed")

ggplot(gathered_ki, aes(x=state,y=number,fill = type)) + 
  geom_bar(stat = "identity", position=position_dodge()) +
  xlab("Top 10 States") +
  ylab("Count") + 
  ggtitle("Top Murders by State") + 
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))  + 
  scale_fill_manual("legend", values = c("Injured" = "black", "Killed" = "red"))
