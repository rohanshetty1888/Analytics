## PIG 

<b>Case study : Airline Flight Delay</b>

Find the proportion of flights delayed. A flight is delayed if the delay is greater than 15 minutes. <br>
Compute the fraction of delayed flights per month in the year 2008. <br>
The delay could be any kind of delay (carrier delay, weather delay, etc)

Hint:

-- A flight is delayed if the delay is greater than 15 minutes.

-- delay = arrival time - scheduled arrival time

 -- Compute the fraction of delayed flights per different time

-- granularities (hour, day, week, month, year)

(let's focus on a month)

Foreach month:

 -- compute the total number of flights

 -- compute delay relation: only those flights with delay > 15 min appear here

 -- compute the total number of delayed flights

 -- output relation: month, ratio delayed/total

You need to calculate:

Fraction delay from the departure<br>
Fraction delay due to carrier<br>
Fraction delay due to weather
 
