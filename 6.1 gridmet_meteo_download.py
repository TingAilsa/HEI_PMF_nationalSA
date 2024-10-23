import ee
from datetime import datetime, timedelta

# Initialize the Earth Engine API
ee.Initialize()

# Define the date range for the entire period (from 2011-01-01 to 2020-12-31)
start_year = 2011
end_year = 2020

# Define the GRIDMET dataset
dataset = ee.ImageCollection('IDAHO_EPSCOR/GRIDMET')

# Define the list of variables to download
variables = [
    "tmmx", "tmmn", "rmax", "rmin",  # 4, Max & Min temperature and RH
    "vs", "th",  # 2, Wind speed (vs) at 10m and Wind direction (th)
    "fm100", "fm1000", "bi",  # 3, 100-hour and 1000-hour dead fuel moisture, burning index
    "pr", "vpd", "etr"  # 3, Precipitation (pr), Vapor Pressure Deficit (vpd), Evapotranspiration (etr)
]

# Define the region for export (Mainland US)
us_region = ee.Geometry.Rectangle([-125, 24, -67, 49])

# Function to generate a list of all dates between two dates
def daterange(start_date, end_date):
    for n in range(int((end_date - start_date).days) + 1):
        yield start_date + timedelta(n)

# Loop through each day for each variable and download the data
for meteo_year in range(start_year, end_year + 1):
    start_date = datetime(meteo_year, 1, 1)
    end_date = datetime(meteo_year, 12, 31)
    
    for single_date in daterange(start_date, end_date):
        formatted_date = single_date.strftime("%Y-%m-%d")
        next_date = single_date + timedelta(1)
        formatted_next_date = next_date.strftime("%Y-%m-%d")
        
        # Filter the dataset for the specific day
        dataset_meteo_day = dataset.filter(ee.Filter.date(formatted_date, formatted_next_date))
        
        for meteo_var in variables:
            # Select the variable to download
            img = dataset_meteo_day.select(meteo_var)
            
            # Export the image to Google Drive
            task = ee.batch.Export.image.toDrive(
                image=img.mean(),
                description=f'{meteo_var}_{single_date.strftime("%Y_%m_%d")}',
                scale=4000,  # Resolution in meters
                region=us_region,
                fileFormat='GeoTIFF'
            )
            
            # Start the export task
            task.start()
            print(f'Started task for {meteo_var} on {formatted_date}')
            
