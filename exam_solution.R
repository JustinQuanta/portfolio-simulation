library(simmer)
library(simmer.plot)
library(triangle)
  
set.seed(123)
portfolio <- function(initial_cap, inflow){

  portfolio_env <- simmer() %>% 
    add_global("capital", initial_cap) %>% 
    add_global("total_stocks", 0) %>%
    add_global("dividend", 0) %>% 
    add_global("inflow", inflow)

  get_inflow <- function() get_global(portfolio_env, "inflow")
  get_stocks <- function() get_global(portfolio_env, "total_stocks")  
  
  m4 <- trajectory("portfolio") %>% 
    log_("Begin buying/selling my stocks..") %>%
    branch(function() {
      if(get_global(portfolio_env, "total_stocks") <= 0){
        return(1)
        } else {
          sample(2,size=1, prob=c(0.9,0.1))
        }
      }, continue = TRUE,
      trajectory() %>%
        set_attribute("Type", 1) %>%
        log_("Buying into stocks using my capital") %>% 
        set_attribute("stocks_bought", 
                      function() rtriangle(1,0,100,10)*rnorm(1,50,10)) %>% 
        set_global("capital", 
                   function() -get_attribute(portfolio_env, "stocks_bought"), mod="+") %>% 
        set_global("total_stocks",
                   function() get_attribute(portfolio_env, "stocks_bought"), mod="+"),
      trajectory() %>%
        set_attribute("Type", 2) %>%
        log_("Selling my stocks..") %>% 
        set_attribute("stocks_sold", 
                      function() rtriangle(1,0,100,10)*rnorm(1,60,15)) %>%
        branch(function() {
          stocks <- c(get_global(portfolio_env,"total_stocks"),
                      get_attribute(portfolio_env, "stocks_sold"))
          if(stocks[1] >= stocks[2]){
            return (1)
            } else{
              return (2)
            }
          }, continue= TRUE,
          trajectory() %>% 
            set_global("total_stocks",
                       function() -get_attribute(portfolio_env, "stocks_sold"), mod="+") %>% 
            set_global("capital", 
                       function() get_attribute(portfolio_env, "stocks_sold"), mod="+"),
          trajectory() %>% 
            log_("Invalid transaction, not enough stocks owned to sell")
          )
      ) %>%
    log_("This is the monthly dividend I get..") %>% 
    set_global("dividend",
               function() get_global(portfolio_env, "total_stocks")*runif(1,0.04,0.06)/12, mod="+") %>% 
    log_("Lets see what is my gain/loss from the stocks this month") %>% 
    set_global("total_stocks", 
               function() get_global(portfolio_env, "total_stocks")*rtriangle(1,0.90,1.10,1.01)) %>% 
    set_global("capital", get_inflow, mod="+") %>% 
    branch(function() {
      attrib <- get_global(portfolio_env, 
                           c("capital","total_stocks","dividend"))
      if(sum(attrib) >= 2*initial_cap){
        return(1)
        } else if(attrib[1] < 0){
          return(2)
        } else{
          return(0)
        }
      }, continue = FALSE,
      trajectory() %>% 
        log_("Hooray! Ive gained 100% from my initial capital! I am going to sell!") %>%
        set_attribute("selling-type", 1) %>% 
        set_global("capital", get_stocks, mod="+") %>% 
        set_global("total_stocks", 0) %>% 
        set_global("capital", get_inflow, mod="+"),
      trajectory() %>% 
        log_("I have negative balance on my capital, hence I am forced to sell all my stocks!") %>% 
        set_attribute("selling-type", 2) %>%
        set_global("capital", get_stocks, mod="+") %>% 
        set_global("total_stocks", 0) %>% 
        set_global("capital", get_inflow, mod="+")
      ) %>% 
    log_("I have done my review for this month!")

  #plot(m4)
  
  
  portfolio_env %>% 
    add_generator("month", m4, at(c(1:24)), mon=2) %>%
    run(until=25)
}

get_attribute1 <- get_mon_attributes(portfolio(10000,2000))




