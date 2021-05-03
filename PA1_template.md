---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---
## Loading necessary libraries

```r
if(!require(tidyverse)){
     install.packages("tidyverse")
}
library(tidyverse)
library(lubridate)
```

## Loading and preprocessing the data

```r
fileName <- unzip("activity.zip", list=TRUE)$Name
data <- read.csv(unz("activity.zip", fileName[[1]]))
```


## What is mean total number of steps taken per day?

```r
stepstat <- filter(data, !is.na(steps)) %>%
     group_by(date) %>%
     summarize(Totalsteps = sum(steps))

ggplot(stepstat, aes(x = Totalsteps)) + 
  geom_histogram(
    bins = 18, 
    fill = "#8bd3fc", 
    color = "#023b5c"
    ) +
  labs(
    x = "Total Steps Taken Each Day",
    title = "Frequency Distribution of Total Steps Taken Each Day"
  ) +
  scale_x_continuous(
    expand = c(0,0),
    breaks = scales::breaks_width(2000)
  ) +
  scale_y_continuous(
    expand = c(0,0),
    breaks = scales::breaks_width(2)
  ) +
  theme(
    axis.title.y = element_blank(),
    axis.title.x = element_text(margin = margin(10,0,0,0), face = "bold"),
    plot.title = element_text(face = "bold", hjust = 0.5, margin = margin(0,0,10,0)),
    panel.background = element_blank(),
    panel.grid = element_line(color = "gray90"),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank()
  )
```

![](PA1_template_files/figure-html/meansteps-1.png)<!-- -->

```r
print(paste("Mean = ", round(mean(stepstat$Totalsteps))))
```

```
## [1] "Mean =  10766"
```

```r
print(paste("Median = ", median(stepstat$Totalsteps)))
```

```
## [1] "Median =  10765"
```


## What is the average daily activity pattern?
In this step, we first filter the data frame to remove the NA values. We then group and summarize
it by the interval and mean, respectively.

```r
daily <- filter(data, !is.na(steps)) %>%
  group_by(interval) %>%
  summarize(avgsteps = mean(steps))

maxsteps <- daily[match(max(daily$avgsteps), daily$avgsteps),]

ggplot(daily, aes(x = interval, y = avgsteps)) +
  geom_line(
    color = "#3689ff",
    size = 0.7
    ) +
  geom_vline(
    xintercept = maxsteps[[1]],
    color = "#a10000",
    linetype = "twodash",
    size = 0.4
  ) +
  annotate(
    "text",
    x = c(max(daily$interval) - 500, max(daily$interval) - 435),
    y = c(max(daily$avgsteps) - 10, max(daily$avgsteps) - 20),
    label = c(paste("Max Average Steps Taken in One Day =", round(maxsteps[[2]])), 
              paste("Interval with Most Average Steps =", maxsteps[[1]])),
    size = 3.4,
    color = "#a10000"
  ) +
  labs(
    x = "5-Minute Interval",
    y = "Average Steps",
    title = "Average Steps Taken across All Days Divided into 5-Minute Intervals"
  ) +
  scale_x_continuous(
    expand = c(0,0),
    breaks = scales::breaks_width(200),
    limits = c(0, max(daily$interval) + 100)
  ) +
  scale_y_continuous(
    expand = c(0,0),
    breaks = scales::breaks_width(25),
    limits = c(0, maxsteps[[2]] + 10)
  ) +
  theme(
    axis.title.x = element_text(margin = margin(10,0,0,0), face = "bold"),
    axis.title.y = element_text(margin = margin(0,10,0,0), face = "bold"),
    plot.title = element_text(face = "bold", hjust = 0.5),
    panel.background = element_blank(),
    panel.grid = element_line(color = "gray95")
  )
```

![](PA1_template_files/figure-html/daily activity-1.png)<!-- -->


## Inputing missing values
Here, we are displaying the number of missing values in the dataset. We are then substituting the missing values with the mean number of steps taken in that interval

```r
numbermissing <- count(data, is.na(steps))[[2,2]]
print(paste("The number of missing values is:", numbermissing))
```

```
## [1] "The number of missing values is: 2304"
```


```r
steps <- filter(data, !is.na(steps)) %>%
  group_by(interval) %>%
  summarize(Mean = mean(steps))

nacorrect <- data
nacorrect[which(is.na(nacorrect$steps)), 1] <- steps$Mean
nacorrect <- group_by(nacorrect, date) %>%
  summarize(Totalsteps = sum(steps))

ggplot(nacorrect, aes(x = Totalsteps)) +
  geom_histogram(
    bins = 18,
    fill = "#5bd3f5",
    color = "#1900ff"
  ) +
  labs(
    x = "Total Steps Taken Each Day",
    title = "Frequency Distribution of Total Steps Taken Each Day"
  ) +
  scale_x_continuous(
    breaks = scales::breaks_width(2000),
    expand = c(0,0)
  ) +
  scale_y_continuous(
    breaks = scales::breaks_width(2),
    expand = c(0,0)
  ) +
  theme(
    plot.title = element_text(face = "bold", hjust= 0.5),
    axis.title.y = element_blank(),
    axis.title.x = element_text(margin = margin(10,0,0,0), face = "bold"),
    panel.background = element_blank(),
    panel.grid = element_line(color = "gray90")
  )
```

![](PA1_template_files/figure-html/substituting values-1.png)<!-- -->

```r
print(paste("Mean = ", round(mean(nacorrect$Totalsteps))))
```

```
## [1] "Mean =  10766"
```

```r
print(paste("Median = ", round(median(nacorrect$Totalsteps))))
```

```
## [1] "Median =  10766"
```

## Are there differences in activity patterns between weekdays and weekends?

We are using the same steps as the above section to substitute the NA values. Using the recode_factor
function, we can introduce the Week Days and Weekend factors into the data.frame. The factorized
data.frame is grouped by the "date" and "interval" variable names and then summarized based on the
mean. 

```r
correct <- data
correct[which(is.na(correct$steps)), 1] <- steps$Mean
correct <- mutate(correct, date = weekdays(ymd(date))) %>%
  mutate(date = recode_factor(date, 
                                     "Monday" = "Week Days",
                                     "Tuesday" = "Week Days",
                                     "Wednesday" = "Week Days",
                                     "Thursday" = "Week Days",
                                     "Friday" = "Week Days",
                                     "Saturday" = "Weekend",
                                     "Sunday" ="Weekend")) %>%
  group_by(date, interval) %>%
  summarize("Mean" = mean(steps), .groups = "drop")

ggplot(correct, aes(x = interval, y = Mean, color = date)) +
  geom_line() +
  labs(
    x = "5-Minute Intervals",
    y = "Average Steps Taken",
    title = "Average Steps Taken in 5-Minute Intervals Faceted by Week Days and Weekends"
  ) +
  facet_wrap(
    ~ date, 
    nrow = 2, 
    ncol = 1
  ) +
  scale_color_manual(values = c("Week Days" = "#707070", 
                                "Weekend" = "#24ff53")
  ) +
  scale_x_continuous(
    breaks = scales::breaks_width(200),
    expand = c(0,0)
  ) +
  scale_y_continuous(
    expand = c(0,0)
  ) +
  theme(
    axis.title.x = element_text(margin = margin(10,0,0,0)),
    axis.title.y = element_text(margin = margin(0,10,0,0)),
    legend.position = "none",
    strip.background = element_rect(fill = "#ff4f4f"),
    strip.text = element_text(color = "white"),
    plot.title = element_text(face = "bold", hjust = 0.5, size = 12),
    panel.background = element_blank(),
    panel.grid.major = element_line(color = "gray95")
  )
```

![](PA1_template_files/figure-html/weekdays-1.png)<!-- -->
