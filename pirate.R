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

# Grab data on pirates ----------------------------------------------------
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

# Arrr! Seems that we have a singularity in the Ordinary Least Squares regression 
#   above with all these predictors, matee! Well blow me down! Best we scrape
#   the barnicals off our rudder, statistically speaking:

mod <- lm(rank ~ available_digital + week,data = piracy)
summary.lm(mod)
step <- stepAIC(mod, direction = "forward")

gvlma(mod)
