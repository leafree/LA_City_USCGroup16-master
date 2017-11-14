# -*- coding: utf-8 -*-
"""
Created on Wed Nov  1 12:46:28 2017

@author: brendanbailey
"""

import pandas as pd
import geopandas as gpd #For sjoin to work, pip install Rtree and brew install spatialindex
from shapely.geometry import Point
from urllib import urlretrieve
from zipfile import ZipFile
import datetime
import os

def get_shape_file(url, gisfilename):

        # download file
        urlretrieve(url, gisfilename)
        zipped = os.getcwd() + "/" + gisfilename

        # unzip file
        to_unzip = ZipFile(zipped, 'r')
        unzipped = os.getcwd() + "/" + gisfilename + '_unzipped'
        to_unzip.extractall(unzipped)
        to_unzip.close()

        # get shape file
        for item in os.listdir(unzipped):
            if item.endswith(".shp"):
                shape_file =  unzipped + '/' + item

        # return full file path
        return shape_file

def add_cts(call_df, census_shapefile, LAT = "LATITUDE", LONG = "LONGITUDE"):
    call_df.columns = [column.upper() for column in call_df.columns]
    call_df.dropna(subset=[LAT,LONG], inplace = True)
    call_geometry = [Point(xy) for xy in zip(call_df[LONG].astype(float), call_df[LAT].astype(float))]
    #call_df = pd.read_json("https://data.lacity.org/resource/az43-p47q.json", dtype = False)
    #call_geometry = [Point(xy) for xy in zip(call_df.longitude.astype(float), call_df.latitude.astype(float))]
    call_df = gpd.GeoDataFrame(call_df, geometry = call_geometry, crs = "+init=epsg:4326")
    census_df = gpd.GeoDataFrame.from_file(census_shapefile)
    census_df.to_crs("+init=epsg:4326", inplace = True)
    merged_df = gpd.sjoin(call_df, census_df, how="inner")
    return merged_df

if __name__ == "__main__":
    #Getting Shapefile
    census_shapefile = get_shape_file("https://egis3.lacounty.gov/dataportal/wp-content/uploads/2011/07/CENSUS_TRACTS_2010.zip", "data/raw_data/CENSUS_TRACTS_2010.zip")

    #Getting original dataframes
    call_dataframe = pd.read_csv("data/raw_data/311_Homeless_Encampments_Requests.csv", dtype = str)
    shelter_dataframe = pd.read_csv("data/raw_data/Homeless_Shelters_and_Services.csv", dtype = str)
    shelter_dataframe.drop(["latitude","longitude"],axis=1, inplace=True)
    shelter_dataframe = shelter_dataframe.rename(columns = {"X":"LONGITUDE", "Y":"LATITUDE"})
    crime_dataframe = pd.read_csv("data/raw_data/Crime__Homeless_Victim_8_16_-_8_17.csv", dtype = str)
    crime_lat_long_dataframe = crime_dataframe["Location "].str.replace(")","").str.replace("(","").str.split(",",expand=True)
    crime_lat_long_dataframe.columns = ["LATITUDE", "LONGITUDE"]
    crime_dataframe = crime_dataframe.merge(crime_lat_long_dataframe, left_index = True, right_index = True)
    crime_dataframe = crime_dataframe[crime_dataframe.LATITUDE != 0]

    #Adding CTs
    df_dict = {"311_calls_w_CTs":call_dataframe, "shelters_w_CTs": shelter_dataframe, "crime_w_CTs": crime_dataframe}
    for item in df_dict:
        print item
        df_dict[item] = add_cts(df_dict[item], census_shapefile)
        df_dict[item].to_csv("data/" + item + datetime.datetime.now().strftime("%Y%m%d%H%M%S") + ".csv", index = False)
