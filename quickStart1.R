## A few demonstrations related to the 4-Feb-2016 LOPSA-NJ Meeting

## Load common libraries in a good order

library(ggplot2)
library(plyr)  # plyr, followed by dplyr
library(dplyr) # Provides %>% and functions used for filtering and aggregation - See RStudio's Data Wrangling Cheatsheet
library(readr)
library(lubridate)

Sys.setenv(TZ="UTC") # Often prevents TZ issues

## Example 1 - Small Number of Data Points - Inline ##
## Note that read_delim from the readr package requires a single space between fields

IPSraw <- 'Time tcp_syn.dropped tcp_syn.forwarded
2015-09-17T21:33 49050 48483
2015-09-17T21:36 87309 85551
2015-09-17T21:39 163092 99578
2015-09-17T21:42 247875 114235
2015-09-17T21:45 335129 129098
2015-09-17T21:48 405430 135635
2015-09-17T21:51 473972 143137
2015-09-17T21:54 541985 149912
2015-09-17T21:57 651037 158139
2015-09-17T22:00 759434 166562
2015-09-17T22:03 812341 170244
2015-09-17T22:06 877437 174047
2015-09-17T22:09 942562 177693
2015-09-17T22:12 987158 180936'

IPS <- read_delim(IPSraw, delim = ' ', col_types = list(Time = col_datetime('%Y-%m-%dT%H:%M')))

PointSize <- 1.0  # Also used in Example 2 below

Title <- 'Server to Backend - SYNs Dropped / 3 Minutes'

ggplot(IPS) +
    geom_line(aes(x = Time, y = tcp_syn.dropped), size=0.1) +
    geom_point(aes(x = Time, y = tcp_syn.dropped),
    size=PointSize + 2, color = 'red', shape=19) +
    xlab('') +
    ggtitle(Title)

## Have a Look at the Data

IPS

str(IPS)

glimpse(IPS)

## -------------------------------------------------------------------------------------------------------

## Example 2 - Data Wrangling

site_servers <- list() # Servers of interest
site_servers[["nyc"]] <- c("nyc-bc-drp-01", "nyc-bc-drp-02")   # A form of hash
site_servers[["bos"]] <- c("bos-bc-drp-01", "bos-bc-drp-02")

servers_of_interest <- unique(unlist(site_servers))            # Vector of server names


File <- 'si20151117.dat'                                       ## If R can find the file without a path ##
File <- '/home/meekj/wpl/talks/lopsanj-2016-1/si20151117.dat'  ## Otherwise, adjust the path for your data location ##
File <- '/Users/meekj/wpl/talks/lopsanj-2016-1/si20151117.dat' ## Otherwise, adjust the path for your data location ##

bc <- read.table(File, header = TRUE)                          # Base R read.table
bc <- bc %>% filter(Server %in% servers_of_interest)           # Keep only servers of interest
bc$Time <- as.POSIXct(bc$utime, tz="UTC", origin="1970-01-01") # Convert UNIX seconds to POSIXct

global_agg <- bc %>% group_by(Time) %>% summarise(TotalUsers = sum(Users), ServerCount = n()) # Pipe-like

MedianServerCount <- median(global_agg$ServerCount)                   # How many servers are active?
global_agg <- global_agg %>% filter(ServerCount == MedianServerCount) # Drop minutes with missing server data

bc$Egress  <- substr(bc$Server, 1, 3)            # Get data center site code from server name
SiteCodes  <- unique(bc$Egress)                  # Character vector of unique DC names
cSiteCodes <- paste(SiteCodes, collapse = ' & ') # Collapse into simple string

PointSize <- 1.0

Title <- paste('Concurrent Users via', cSiteCodes, 'Using', MedianServerCount, 'Servers')

ggplot(global_agg) +
    geom_line(aes(x = Time, y = TotalUsers), size=PointSize - 0.6, color = 'dodgerblue') + # Line first
    geom_point(aes(x = Time, y = TotalUsers), size=PointSize, color = 'blue', shape=19) +  # Points on top
    xlab("") + ylab("Global Concurrent Users") +
    scale_x_datetime(date_minor_breaks = "1 hour") + # ggplot2 2.0.0, remove 'date_' for previous versions!
    ggtitle(Title)


## Look at data structures

str(bc)

str(global_agg)

## -------------------------------------------------------------------------------------------------------

PointSize <- 0.6


## Faceted plot, separate for each server

ggplot(bc) +
    geom_point(aes(x = Time, y = Users), size=PointSize, color = 'blue', shape=19) +  # Points on top
    xlab("") + ylab("Concurrent Users") +
    scale_x_datetime(date_minor_breaks = "1 hour") + # ggplot2 2.0.0, remove 'date_' for previous versions!
    facet_grid(Server ~ ., scales="free_y") +
    theme(strip.text.y = element_text(size = rel(1.2), colour = "black", face = 'bold')) + # Enhance readability of facet labels
    ggtitle(Title)


## Single plot, each server gets a different color

ggplot(bc) +
    geom_point(aes(x = Time, y = Users, color = Server), size=PointSize, shape=19) +  # Points on top
    xlab("") + ylab("Concurrent Users") +
    scale_x_datetime(date_minor_breaks = "1 hour") + # ggplot2 2.0.0, remove 'date_' for previous versions!
    ggtitle(Title) +
    scale_colour_manual(values=c("blue", "red", "green", "yellow"))

## -------------------------------------------------------------------------------------------------------
## -------------------------------------------------------------------------------------------------------

## Example 3 - More Data Wrangling and Boxplots

## Re-read the data to get all the server data again
#
File <- 'si20151117.dat'                                       ## If R can find the file without a path ##
File <- '/home/meekj/wpl/talks/lopsanj-2016-1/si20151117.dat'  ## Otherwise, adjust the path for your data location ##
File <- '/Users/meekj/wpl/talks/lopsanj-2016-1/si20151117.dat' ## Otherwise, adjust the path for your data location ##

bc <- read.table(File, header = TRUE)                          # Base R read.table

## Look only at servers with '-sg-' in their name
#
all_servers <- as.character(unique(bc$Server))                 # vector of all server names
servers_of_interest <- all_servers[grep('-sg-', all_servers)]  # grep returns indicies

bc <- bc %>% filter(Server %in% servers_of_interest)           # Keep only servers of interest
bc$Time <- as.POSIXct(bc$utime, tz="UTC", origin="1970-01-01") # Convert UNIX seconds to POSIXct

Title <- 'CPU Utilization'

## Make a boxplot with actual data points displayed with geom_jitter
## Boxplot goes on top of data points, otherwise it would be obscurred
## Set outlier.colour=NA so that redundant outlier points are not plotted
## alpha sets the transparency of the box so that the 'jittered' data points can be seen

ggplot(bc) +
  geom_jitter(aes(group = Server, x = Server, y = CPU), colour  = "dodgerblue", size = 0.8, shape=19) +
  geom_boxplot(aes(group = Server, x = Server, y = CPU), fill = "darkblue", outlier.colour=NA, alpha=0.2) +
  xlab('') + ylab('') + theme(axis.text.x=element_text(angle = 60, hjust=1)) +
  ggtitle(Title)



Title <- 'CPU Utilization per 100 Users'
bc$CPUper100Users <- 100 * bc$CPU / bc$Users # Generate a new  data frame column in the 'Base R way', compare to dplyr::mutate used below

ggplot(bc) +
  geom_jitter(aes(group = Server, x = Server, y = CPUper100Users), colour  = "dodgerblue", size = 0.8, shape=19) +
  geom_boxplot(aes(group = Server, x = Server, y = CPUper100Users), fill = "lightblue", outlier.colour=NA, alpha=0.2) +
  xlab('') + ylab('') + theme(axis.text.x=element_text(angle = 60, hjust=1)) +
  ggtitle(Title)


## CPU / User is less meaningful at low user counts (or low CPU utilization) so we will set a threshold level for the next plot

## Compute 95th percentile Concurrent User count for each server of interest
server_summary <- bc %>% group_by(Server) %>% summarise(q95Users = quantile(Users, probs=0.95))

server_summary # Look at summary

ThresholdPercent <- 30 # Threshold in percent

## Compute reporting threshold by adding a column to the data frame
server_summary <- server_summary %>% mutate(Threshold = ThresholdPercent * q95Users / 100)

server_summary$Server <- as.character(server_summary$Server) # Change data type from factor to character string

server_summary # Look at summary again

## Make a new data frame dropping points below the threshold for each server
bct <- NULL # Start empty
for (server in servers_of_interest) { # Build data frame one server at a time (using a somewhat ugly way to reference the threshold value)
    bct <- rbind(bct, bc %>% filter(Server == server & Users >= server_summary[server_summary$Server == server, ]$Threshold))
}

## Check that the filtering worked

bc  %>% group_by(Server) %>% summarise(Count = n(), MinUsers = min(Users)) # Full data set
bct %>% group_by(Server) %>% summarise(Count = n(), MinUsers = min(Users)) # Reduced data set


Title <- paste('CPU Utilization per 100 Users Above ', ThresholdPercent, '% of 95th Percentile per Server', sep = '')

ggplot(bct) +
  geom_jitter(aes(group = Server, x = Server, y = CPUper100Users), colour  = "dodgerblue", size = 0.4, shape=19) +
  geom_boxplot(aes(group = Server, x = Server, y = CPUper100Users), fill = "lightblue", outlier.colour=NA, alpha=0.4) +
  xlab('') + ylab('') + theme(axis.text.x=element_text(angle = 60, hjust=1)) +
  ggtitle(Title)

