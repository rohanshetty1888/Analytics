-- Fraction delay due to carrier
 
RAW_DATA = LOAD '/home/data/datasets/pig-2/Airlines_Data.csv' USING PigStorage(',') AS (year: int, month: int, day: int, dow: int
, dtime: int, sdtime: int, arrtime: int, satime: int, carrier: chararray,fn: int, tn: chararray, etime: int, setime: int, airtime: int, adelay: int, 
ddelay: int, scode: chararray,dcode:chararray,dist: int, tintime: int, touttime: int, cancel: chararray, cancelcode: chararray, diverted: int, cdelay
: int, wdelay: int, ndelay: int, sdelay: int, latedelay: int);

AIRLINES = FILTER RAW_DATA BY year == 2008;

A = FOREACH AIRLINES GENERATE day AS d, dow AS dow, month AS m, cdelay AS delay;

B = GROUP A BY (m,dow);

COUNT_TOTAL = FOREACH B {
	C = FILTER A BY (delay > 15);
	GENERATE group, COUNT(A) AS tot, COUNT(C) AS del, (float) COUNT(C)/COUNT(A) AS frac;
}

DUMP COUNT_TOTAL;