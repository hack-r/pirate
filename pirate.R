## File: pirate.R
## Description: This script is 97% chum free
## Jason Miller, MS, MS
## http://hack-r.com
## http://github.com/hack-r
## http://hack-r.github.io

# International Talk Like a Pirate Day is Today, Sept. 19 =)
# http://www.talklikeapirate.com/


# Load Packages -----------------------------------------------------------
require() #Screw library(), require() rules

# Grab data on pirates ----------------------------------------------------
download.file("http://piracydata.org/csv", destfile = "p.csv")
piracy <- read.csv("p.csv")

# Determine if legal (un-) availability of movies drives piracy -----------


mod <- lm(rank ~ available_digital + streaming + rental + purchase + dvd +
            netflix_instant + amazon_prime_instant_video + hulu_movies +
            crackle + youtube_free + epix + streampix + amazon_video_rental +
            apple_itunes_rental + android_rental + vudu_rental + youtube_rental +
            amazon_video_purchase + apple_itunes_purchase + android_purchase +
            vudu_purchase + amazon_dvd + amazon_bluray + netflix_dvd +
            redbox,data = piracy)
