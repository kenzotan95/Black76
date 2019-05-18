library(ggplot2)
library(quantmod)

# Sample data.frame
df = data.frame(strike = c(50,20),                  # the option strike - in $
                type = c("C", "P"),                 # either "c" for call option or "p" for a put option
                optionPrice = c(1.62, 0.01),        # the option price - in $
                futurePrice = c(48.03, 48.03),      # the price of the underlying future - in $
                time_to_expiry = c(0.1423, 0.1423)) # the option time to expiry - in year

# black76, https://en.wikipedia.org/wiki/Black_model
# Primary application for pricing options on future contracts, bond options, Interest rate cap and floors, and swaptions
# Generalized into a class of models known as log-normal forward models, LIBOR market model.

# https://rpubs.com/stvrd/blackscholes
# Parameters needed:
# S = Spot price
# K = Strike price
# r = risk-free interest rate
# T = Time to maturity

# Referenced from Jason Yip's work
# https://github.com/jasonyip184
# getImpliedVol = function(df) {
#   
#   # where do i get risk free rate?
#   
#   # call option formula function
#   call_form = function(vola, futurePrice, )
# }


plotImpliedVol = function(df) {
  # risk free rate  using European options
  getSymbols("BAMLHE00EHYIOAS", src="FRED")
  risk = as.data.frame(BAMLHE00EHYIOAS)[date, 1]/100
  
  list_of_v = vector(length=nrow(df))
  list_of_strike = vector(length=nrow(df))
  list_of_type = vector(length=nrow(df))
  list_of_time_to_expiry = vector(length=nrow(df))
  list_of_optionPrice = vector(length=nrow(df))
  list_of_futurePrice = vector(length=nrow(df))
  
  # call formula
  call_f = function(sig, f_p, K, risk, t, c) {
    d1 = (log(f_p/K) + (r + (sig**2)/2)*t) / (sig*sqrt(t))
    d2 = d1 - sig*sqrt(t)
    return (exp(-risk*t) * (f_p*pnorm(d1,0,1) - K*pnorm(d2,0,1)) - c)
  }
  
  # put formula
  put_f = function(sig, f_p, K, risk, t, p) {
    d1 = (log(f_p/K) + (r + (sig**2)/2)*t) / (sig*sqrt(t))
    d2 = d1 - sig*sqrt(t)
    return (exp(-risk*t) * (K*pnorm(-d2,0,1) - f_p*pnorm(-d1,0,1)) - p)
  }
  
  
  # check if call or put option to calculate volatility
  volatility = vector(length=nrow(df))
  for (i in 1:nrow(df)) {
    row = df[i,]
    f_p = row$futurePrice
    K = row$strike
    t = row$time_to_expiry
    opt = row
    
    if (df$type[i] == "C") {
      c = row$optionPrice
      impliedvol = uniroot(f=call_f, interval=c(0,1), tol = 0.0001, fp = f_p, x = K, r = risk, t = t, c = c)$root
      list_of_optionPrice[i] = c
    } else {
      p = row$optionPrice
      implied_vol = uniroot(f=put_f,interval=c(0, 1),tol=0.0001,fp=f_p,x=K,r=risk,t=t,p=p)$root
      list_of_optionPrice[i] = p
    }
    
    # return volatility and strike
    list_of_v[i] = round(implied_vol, 3)
    list_of_strike[i] = x
    list_of_type[i] = type
    list_of_time_to_expiry[i] = round(t, 3)
    list_of_futurePrice[i] = round(fp, 3)
  }
  
  results = data.frame(strike=list_of_strike, type=list_of_type, optionPrice=list_of_optionPrice, futurePrice=list_of_futurePrice, time_to_expiry=list_of_time_to_expiry, implied_volatility=list_of_v)
  
  # plot graph
  title = "Implied Volatility of Call vs Put option"
  ggplot(data = results, aes(x=strike,y=implied_volatility,colour=type)) +
    geom_line() +
    geom_point() +
    labs(title = title, x = "Strike", y = "Implied Volatility") +
    theme(legend.position="right",
          legend.key.size=unit(1.2, "cm"),
          text = element_text(color = "#444444"),
          panel.background = element_rect(fill = '#444B5A'),
          panel.grid.minor = element_line(color = '#586174'),
          panel.grid.major = element_line(color = '#586174'),
          plot.title = element_text(size = 12),
          axis.title = element_text(size = 14, color = '#555555'))
  
}
