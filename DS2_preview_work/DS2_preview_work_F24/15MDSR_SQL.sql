-- !preview conn=dbConnect_scidb("airlines")

SELECT 
  c.name,
  SUM(1) AS N, 
  SUM(arr_delay <= 15) / SUM(1) AS pct_ontime
FROM flights AS f
JOIN carriers AS c ON f.carrier = c.carrier
WHERE year = 2016 AND month = 9
  AND dest = 'JFK'
GROUP BY name 
HAVING N >= 100
ORDER BY pct_ontime DESC
LIMIT 0,4;

