
SELECT table_name
FROM information_schema.tables
WHERE table_schema = 'public';

SELECT *
FROM information_schema.columns
WHERE table_name = 'flights_2014'

SELECT count(*) FROM flights_2014;

SELECT day_of_week,avg(dep_delay) AS avg_dep_delay,
                   max(arr_delay) AS max_arr_delay,
                   avg(distance) AS avg_distance,
                   avg(air_time) AS avg_air_time
FROM flights_2014
WHERE day_of_week BETWEEN 1 AND 7
GROUP BY day_of_week
ORDER BY day_of_week
LIMIT 10;

select count(*) from stocks;

select table_name from information_schema.tables
where table_schema='public';

select * from bigdata_need limit 10;
