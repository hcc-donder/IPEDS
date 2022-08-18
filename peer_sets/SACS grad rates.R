library(tidyverse)

source("code/peers.R")
source("code/get_data.R")
source("code/tables.R")

idbc <- FUDBAccess::FU_db_connect()$IPEDS

sacs4 <- get_peer_group("peer sets/sacs.R") 

grad_rates <- FUPeers::get_grad_rates(idbc, sacs4$UNITID)

ggplot(grad_rates, aes(x = Year, y = Grad_rate, group = UNITID)) +
  geom_line(alpha = .3)


get_slope <- function(Year, Grad_rate){
  
  tdf <- lm(Grad_rate ~ Year) %>% 
    broom::tidy()
  
  return(tdf$estimate[2])
}

his <- grad_rates %>% 
  filter(!is.na(Grad_rate)) %>% 
 # mutate(Rate5 = ntile(Grad_rate, 5)) %>% 
  group_by(UNITID) %>% 
  summarize(slope = get_slope(Year, Grad_rate),
            avg_rate = mean(Grad_rate, na.rm = TRUE))

his %>% 
  ggplot(aes(x = slope)) +
  geom_histogram(fill = "steelblue", color = "white", bins = 50) +
  theme_bw() +
  geom_vline(xintercept = 0, color = "red") 

his %>% 
  ggplot(aes(x = avg_rate, y = slope)) +
  geom_point(alpha = .4)

# South university richmond
grad_rates %>% 
  filter(UNITID == 459259) # very small sample

# overall rate
grad_rates %>% 
  na.omit() %>% 
  mutate(grads = Cohort_size*Grad_rate) %>% 
  group_by(Year) %>% 
  summarize(AvgRate = sum(grads)/sum(Cohort_size)) %>% 
  ggplot(aes(x = Year, y = AvgRate)) +
  geom_line() +
  ylim(0,NA)

