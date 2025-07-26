# Imports
library(tidyverse)

one_mh_iteration = function(y, trials, alpha_par, beta_par, w, current){
  # 1. Proposing the next chain location (uniformly at random)
  proposal = rbeta(1, alpha_par, beta_par)
  
  # 2. Calculate plausibility
  proposal_plaus = dbeta(proposal, alpha_par, beta_par) * dbinom(x=y, size=trials, prob=proposal)
  current_plaus = dbeta(current, alpha_par, beta_par) * dbinom(x=y, size=trials, prob=current)
  proposal_q = dbeta(proposal, alpha_par, beta_par)
  current_q = dbeta(current, alpha_par, beta_par)
  
  # 3. Running simulation
  alpha = min(1, (proposal_plaus/current_plaus)*(current_q /proposal_q))
  next_stop = sample(c(proposal, current), size = 1, prob = c(alpha, 1-alpha))
  
  return (data.frame(proposal, alpha, next_stop))
}

mh_tour = function(start_pos, y, trials, alpha_par, beta_par, N, w){
  # 1. Initializing the chain at 3
  current = start_pos
  mu = rep(0, N)
  
  # 2. Simulating N steps
  
  for (i in 1:N){
    sim = one_mh_iteration(y, trials, alpha_par, beta_par, w, current)
    mu[i] = sim$next_stop
    current = sim$next_stop
  }
  
  return(data.frame(iteration = c(1:N), mu))
}

# Testing the algorithm
set.seed(84735)
mh_simulation_1 = mh_tour(start_pos =0.5, y = 60, trials = 100, alpha_par = 1, beta_par = 1, N = 5000, w=0.1)

# Simulated distribution
ggplot(mh_simulation_1, aes(x = mu)) + 
  geom_histogram(aes(y = ..density..), color = "white", bins = 20)

# Trace plot
ggplot(mh_simulation_1, aes(x = iteration, y = mu)) + geom_line()

# Example using Tokyo rainfall data
install.packages("https://cran.r-project.org/src/contrib/rgdal_0.9-1.tar.gz", repos = NULL, type="source", configure.args = "--with-gdal-config=/Library/Frameworks/GDAL.framework/Versions/1.10/unix/bin/gdal-config --with-proj-include=/Library/Frameworks/PROJ.framework/unix/include --with-proj-lib=/Library/Frameworks/PROJ.framework/unix/lib")
library(rnoaa)

# Search for a station near a location (e.g., Seattle, WA)
stations <- ghcnd_stations()
seattle_station <- stations[grep("SEATTLE", stations$name), ][1, ]

# Download daily data for the station
data <- ghcnd(stationid = seattle_station$id)

# Filter precipitation data
prcp <- subset(data$data, element == "PRCP")

# Convert date and extract year
prcp$date <- as.Date(prcp$date)
prcp$year <- format(prcp$date, "%Y")

# Count rainy days per year (precipitation > 0)
prcp$rainy <- prcp$value > 0
rainy_days_per_year <- aggregate(rainy ~ year, data = prcp, sum)

head(rainy_days_per_year)

