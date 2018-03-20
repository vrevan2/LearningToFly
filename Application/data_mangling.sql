
-- Numerical LatLong values and hasWeatherData for airports and weather stations
update Airport set LatitudeValue = CAST(LatitudeString as float), LongitudeValue = CAST(LongitudeString as float);

update Station set LatitudeValue = CAST(LatitudeString as float), LongitudeValue = CAST(LongitudeString as float),
HasWeather = ( select count(distinct [Date]) from weather w where w.[Station ID] = StationId and 
	[Type] in ('PRCP', 'SNOW', 'SNWD', 'TMAX', 'TMIN', 'TAVG') );

-- Nearest weather station with at least 100 days of weather data for each American airport
drop table if exists AirportStation;
with a (AirportId, StationId, Distance) as (
	select a.AirportId, s.StationId, 	abs(a.LatitudeValue - s.LatitudeValue) + abs(a.LongitudeValue - s.LongitudeValue) Distance
	from airport a, station s where s.[State] <> '' and a.IATA <> '\N' and TimezoneString <> '\N' and a.Country = 'United States' and s.HasWeather > 100)
select * into AirportStation from (
	select *, ROW_NUMBER() over (partition by airportId order by distance) rn from a
) x where rn = 1;

-- Horizontal weather data
DROP TABLE IF EXISTS WeatherData;
SELECT [Station ID], [Date], [PRCP], [SNOW], [SNWD], [TMAX], [TMIN], [TAVG]
INTO WeatherData FROM (
	SELECT [Station ID], [Date], [Type], [Value] FROM weather w join AirportStation s on w.[Station ID] = s.StationId) p
	PIVOT (MIN ([Value]) FOR [Type] IN ( [PRCP], [SNOW], [SNWD], [TMAX], [TMIN], [TAVG] )
) AS pvt ORDER BY pvt.[Station ID], pvt.[Date];

select a.AirportId, a.IATA, a.[Name] AirportName, a.City, a.LatitudeValue AirportLatitude, a.LongitudeValue AirportLongitude, a.TimezoneString, Distance, 
s.StationId, s.[Name] StationName, s.[State], s.LatitudeValue StationLatitude, s.LongitudeValue StationLongitude from AirportStation r
join Airport a on a.AirportId = r.AirportId join Station s on s.StationId = r.StationId
order by IATA;

select w.* from WeatherData w join AirportStation s on w.[Station ID] = s.StationId

SELECT 
       [Month]
      ,[DayofMonth]
      ,[DayOfWeek]
	  ,([CRSDepTime] / 100) % 24 as CRSDepHour
	  ,([DepTime] / 100) % 24 as DepHour
	  ,([CRSArrTime] / 100) % 24 as CRSArrHour
	  ,([ArrTime] / 100) % 24 as ArrHour
      ,[AirlineID]
      ,[Origin]
      ,[Dest]
	  ,[OriginState]
	  ,[DestState]
      ,[CancellationCode]
      ,[Diverted]
      ,[CRSElapsedTime]
	  ,cast([CRSElapsedTime] / 60 as int) as CRSElapsedTimeGroup
      ,[Distance]
      ,[DistanceGroup]
      ,[CarrierDelay]
      ,[WeatherDelay]
      ,[NASDelay]
      ,[SecurityDelay]
      ,[LateAircraftDelay]
  FROM [cs424].[dbo].[On_Time_Performance_2017]
  where [OriginState] = 'IL' or [DestState] = 'IL'
