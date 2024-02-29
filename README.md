# time-series

DISCUSSING
The aim of the project was to conduct a time series analysis and select the optimal model to predict future values. For the analysis, we considered various methods, including differentiation, Box-Cox transformation, regression-based decomposition, classical decomposition, and Holt smoothing.
First, we analyzed the components of the time series to understand its structure and possible trends. We then tested different modeling methods to choose the one that best fit the analyzed series. These methods included various data transformation techniques and decomposition techniques that allow the series to be separated into its components: trend, seasonality and random component.
DATA
The data we worked on concerned global gold prices from October 1, 2000 to August 1, 2020.

#Econometric model 

Data
Our set of analyzed data, used to build an econometric model, contains information on midweek prices for renting apartments on AirBnB in Amsterdam. The variables available for analysis are:
• realSum - The total price of the Airbnb listing. (Numerical)
• room_type - The type of room offered (e.g. private, shared, etc.). (Categorical)
• room_shared - Whether the room is shared or not. (Boolean)
• room_private - Whether the room is private or not. (Boolean)
• person_capacity - Maximum number of people who can stay in the room. (Numeric) • host_is_superhost - Whether the host is a superhost or not. (Boolean)
• multi - Whether the offer applies to many rooms or not. (Boolean)
• biz - Whether the offer is intended for business purposes or not. (Boolean)
• cleanliness_rating - Rating of the cleanliness of the offer. (Numerical)
• guest_satisfaction_overall - Overall rating of guests' satisfaction with the offer. (Numerical)
• bedrooms - Number of bedrooms on offer. (Numerical)
• dist - Distance from the city center. (Numerical)
• metro_dist - Distance from the nearest metro stop. (Numerical)
• lng - Geographic longitude of the offer. (Numerical)
• years - The geographical latitude of the offer. (Numerical)
