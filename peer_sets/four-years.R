library(tidyverse)

source("code/peers.R")
source("code/get_data.R")
source("code/tables.R")

idbc <- FUDBAccess::FU_db_connect()$IPEDS

fouryr <- get_peer_group("peer sets/4-year.R") 

grad_rates <- FUPeers::get_grad_rates(idbc, fouryr$UNITID)

# use the 4-year rates
grad_rates <- grad_rates %>% 
  select(UNITID, Year, Grad_rate = Grad_rate_4yr, Cohort_size = Cohort_size_4yr)

# filter to those with complete histories
grad_rate_n <- grad_rates %>% 
  count(UNITID) %>% 
  filter(n == 14) %>% 
  select(UNITID)

grad_rates <- grad_rates %>% 
  inner_join(grad_rate_n)

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


# overall rates by sector and accreditor
grad_rates %>% 
  left_join(fouryr %>% select(UNITID,Accreditor, Sector = `(SECTOR) Sector of institution`)) %>% 
  na.omit() %>% 
  mutate(grads = Cohort_size*Grad_rate) %>% 
  group_by(Year, Accreditor, Sector) %>% 
  summarize(N = n(),
            AvgRate = sum(grads)/sum(Cohort_size)) %>% 
  filter(N > 40) %>% 
  ggplot(aes(x = Year, y = AvgRate, color = Accreditor)) +
  #geom_line() +
  geom_smooth() +
  ylim(0,NA) + 
  facet_grid(. ~ Sector) +
  theme_bw() +
  ylab("Avg 4-Year Grad Rate")


# overall rates by grad rate quintile
rate2005 <- grad_rates %>% 
  na.omit() %>% 
  filter(Year == 2005) %>% 
  mutate(Q = as.factor(ntile(Grad_rate,5))) %>% 
  select(UNITID,Q)

grad_rates %>% 
  na.omit() %>% 
  left_join(rate2005) %>% 
  mutate(grads = Cohort_size*Grad_rate) %>% 
  group_by(Year, Q) %>% 
  summarize(N = n(),
            AvgRate = sum(grads)/sum(Cohort_size)) %>% 
  na.omit() %>% 
  ggplot(aes(x = Year, y = AvgRate, color = Q, group = Q)) +
  geom_line() +
  ylim(0,NA) 

# density
grad_rates %>% 
  na.omit() %>% 
  left_join(rate2005) %>% 
  mutate(grads = Cohort_size*Grad_rate) %>% 
  group_by(Year, Q) %>% 
  summarize(N = n(),
            AvgRate = sum(grads)/sum(Cohort_size)) %>% 
  na.omit() %>% 
  ggplot(aes(x = Year, y = AvgRate, color = Q, group = Q)) +
  geom_line() +
  ylim(0,NA) 