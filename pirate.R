## File: pirate.R
## Description: This script is 97% chum free
## Jason Miller, MS, MS
## http://hack-r.com
## http://github.com/hack-r
## http://hack-r.github.io

# International Talk Like a Pirate Day is Today, Sept. 19 =)
# http://www.talklikeapirate.com/

# Load Packages -----------------------------------------------------------
require(gvlma) #Screw library(), require() rules
require(MASS)


# Source Pirate Functions -------------------------------------------------
source("pirate_functions.R")

# Grab data on pirates ----------------------------------------------------
# Top 10 Pirated Movies by week and their legal availability across forms of media
download.file("http://piracydata.org/csv", destfile = "p.csv")
piracy <- read.csv("p.csv")

# Invert Rank So that Higher = More Piracy --------------------------------
piracy$pirate <- 1/piracy$rank

# Determine if Legal Un-availability of Movies Drives Piracy -----------
mod <- lm(pirate ~ available_digital + streaming + rental + purchase + dvd +
            netflix_instant + amazon_prime_instant_video + hulu_movies +
            crackle + youtube_free + epix + streampix + amazon_video_rental +
            apple_itunes_rental + android_rental + vudu_rental + youtube_rental +
            amazon_video_purchase + apple_itunes_purchase + android_purchase +
            vudu_purchase + amazon_dvd + amazon_bluray + netflix_dvd +
            redbox,data = piracy)
summary.lm(mod)
gvlma(mod)

# Arrr! Seems that we have a singularity in the Ordinary Least Squares regression 
#   above with all these predictors, matee! Well blow me down! 

# There was no identifying variation in some of the explanatory variables!
# This means many of these highly pirated movies have absolutely NO avaialability
#  in many of these channels, for example:
mean(piracy$netflix_instant)
mean(piracy$amazon_prime_instant_video)
mean(piracy$youtube_free)
mean(piracy$epix)
mean(piracy$crackle)
mean(piracy$streampix)
mean(piracy$hulu_movies)

# This provides evidence that unavailability is associated with piracy, however
#   we need non-zero datapoints to use these variables in a regression! 
# Best we scrape the barnicals off our rudder, statistically speaking:

step <- stepAIC(mod, direction = "both")
step$anova

# Fully Stepwise Model:
mod <- lm(pirate ~ vudu_rental + youtube_rental + amazon_video_purchase + 
             vudu_purchase, data = piracy)
summary.lm(mod)
gvlma(mod)

# So, this model says that if movies are legally available on Vudu or Amazon that
#     people are less likely to pirate them, however some of the diagnostic tests 
#     from GVLMA (Global Validation of Linear Model Assumptions) indicate that
#     the OLS assumptions are not satisfied. I'm going to try transforming the 
#     outcome to binary and using a probit (probability) model with boostrap estimation
#     of the marginal effects:

piracy$higher_piracy <- 0
piracy$higher_piracy[piracy$pirate >= .5] <- 1
table(piracy$higher_piracy)

probit <- glm(higher_piracy ~ amazon_video_purchase + redbox + amazon_dvd 
              ,data = piracy,  family =binomial(link = "probit"))
summary.glm(probit)

mfxboot(modform = "higher_piracy ~ amazon_video_purchase + redbox + amazon_dvd ",
        dist = "probit",
        data = piracy)

