#!/usr/bin/env python
# coding: utf-8

# # THREDDS Servier Inventory for LOCA2 Aggregated Regions

# In[ ]:


####################################################
#
# Libraries
#

print("====")

print("BEGIN THREDDS INVENTORY SCRIPT")

print("----")

print("Libraries")

import numpy              as np
import cartopy.crs        as ccrs
import cartopy.feature    as cfeature
import matplotlib.patches as mpatches
import matplotlib.pyplot  as plt
import pandas             as pd
import shapely            as shapely
import os                 as os
import pyproj             as proj

import geopandas          as gp

from   siphon.catalog import TDSCatalog

import socket

print("----")

#
####################################################


# ## Creating SD Mines Branded Colors

# In[ ]:


####################################################
#
# Mines Colors and Fonts
#

print("Mines Colors and Fonts for Local Branding")

Mines_Blue = "#002554"


plt.rcParams.update({'text.color'      : Mines_Blue,
                     'axes.labelcolor' : Mines_Blue,
					 'axes.edgecolor'  : Mines_Blue,
					 'xtick.color'     : Mines_Blue,
					 'ytick.color'     : Mines_Blue,
                     'font.family'     : "Open Sans"})

print("----")

#
####################################################


# ## Determine What Machine we are working on.

# In[ ]:


####################################################
#
# Kyrill vs Mandrenke
#

print("Begin Script: Identify Machine")

print(socket.gethostname())

myhostname = socket.gethostname()

if ("mandrenke" in socket.gethostname()): # Mandrenke
    target_dir  = "/Users/wjc/GitHub/LOCA_Percentile_Processing/"
    display_img = True
else:  # Kyrill
    target_dir = "/var/www/html/wjc/eduresources/"
    display_img = False

print(target_dir)

print("----")

#
####################################################


# ## Pulling Inventories from the THREDDS Server

# In[ ]:


####################################################
#
# Pulling Available RData Inventories from THREDDS Server 
#

print("Pulling Inventory from THREDDS Server")

dir_hucs = "http://kyrill.ias.sdsmt.edu:8080/thredds/catalog/LOCA2/Specific_Regional_Aggregate_Sets/USGS_HUC08_Basins/R_Annual_Files/catalog.xml"
dir_cdiv = "http://kyrill.ias.sdsmt.edu:8080/thredds/catalog/LOCA2/Specific_Regional_Aggregate_Sets/NCEI_Climate_Divisions/R_Annual_Files/catalog.xml"

dir_hucs_csv = "http://kyrill.ias.sdsmt.edu/wjc/eduresources/USGS_HUC08_LUT.csv"
dir_cdiv_csv = "http://kyrill.ias.sdsmt.edu/wjc/eduresources/NCEI_nClimDiv_LUT.csv"

pre_hucs = "LOCA2_V1_HUC08_ANNUAL_"
pre_cdiv = "LOCA2_V1_nCLIMDIV_ANNUAL_"

cat_hucs = TDSCatalog(dir_hucs).datasets
cat_cdiv = TDSCatalog(dir_cdiv).datasets

available_hucs = []
for file_huc in cat_hucs:
    available_hucs.append(file_huc.replace(pre_hucs, "").replace(".RData", ""))
    
available_cdiv = []
for file_cdiv in cat_cdiv:
    available_cdiv.append(file_cdiv.replace(pre_cdiv, "").replace(".RData", ""))

table_hucs = pd.read_csv(filepath_or_buffer = dir_hucs_csv)
table_cdiv = pd.read_csv(filepath_or_buffer = dir_cdiv_csv)

available_hucs = np.array(available_hucs, dtype=np.int32)
available_cdiv = np.array(available_cdiv, dtype=np.int32)

print("----")

#
####################################################


# ## Cleaning the HUC-08 Inventories

# In[ ]:


####################################################
#
# Cleaning Available HUC Inventory
#

print("Cleaning USGS HUC 08 Inventory")

table_huc = table_hucs[table_hucs["huc08"].isin(available_hucs)]

table_huc = table_huc[['huc08',
                       'huc08_name', 
                       'huc06_name', 
                       'huc04_name', 
                       'huc02_name']].rename(columns = {"huc08"     : "HUC-08",
                                                        "huc08_name": "BASIN NAME",
                                                        "huc06_name": "HUC-06 BASIN",
                                                        "huc04_name": "HUC-04 BASIN",
                                                        "huc02_name": "HUC-02 REGION"})

table_huc["HUC-08"] = [str(x).zfill(8)   for x in table_huc["HUC-08"].values]
table_huc = table_huc.sort_values("HUC-08")
table_huc.to_csv(path_or_buf = target_dir + "HUC_table_avail.csv", index=False)
print(target_dir + "HUC_table_avail.csv")


if (display_img) :
    display(table_huc)

os.system('csvtotable --caption "Available HUC-08 Files" --overwrite ' +  target_dir + "HUC_table_avail.csv " + target_dir + "HUC_table_avail.html")

print("----")

#
####################################################


# ## Cleaning the Climate Division Inventories

# In[ ]:


####################################################
#
# Cleaning Available ClimDiv Inventory
#

print("Cleaning NCEI ClimDiv Inventory")

table_div = table_cdiv[table_cdiv["climdiv"].isin(available_cdiv)][["climdiv",
                                                                    "climdiv_name",
                                                                    "climdiv_state_name"]]

table_div["climdiv"] = [str(x).zfill(4) for x in table_div["climdiv"].values]

table_div  = table_div.rename(columns = {"climdiv":"DIVISION",
                                         "climdiv_name":"NAME",
                                          "climdiv_state_name":"STATE"}).sort_values("DIVISION")

table_div.to_csv(path_or_buf = target_dir + "ClimDiv_table_avail.csv", index=False)

print(target_dir + "ClimDiv_table_avail.csv")

if (display_img) :
    display(table_div)

os.system('csvtotable --caption "Available ClimDiv Files" --overwrite ' +  target_dir + "ClimDiv_table_avail.csv " + target_dir + "ClimDiv_table_avail.html")


print("----")

#
####################################################


# ## Pulling GeoJSON Polygon Files

# In[ ]:


####################################################
#
# Cleaning Available ClimDiv Inventory
#

print("Read the GeoJSON Polygon Files")

shp_hucs = gp.read_file("GeoJSON_Files/CONUS_USGS_HUC-08.geojson")
shp_cdiv = gp.read_file("GeoJSON_Files/CONUS_NCEI_Climate_Divisions.geojson")

print("Initial Polygon Elements")
print("       HUC08:RAW:",len(shp_hucs), len(shp_hucs.get_coordinates()))
print("     CLIMDIV:RAW:",len(shp_cdiv), len(shp_cdiv.get_coordinates()))

print("----")

#
####################################################


# ## Retain only Polygons Associated with an Available RData File

# In[ ]:


####################################################
#
# Retain only Polygons Associated with an Available RData File
#

print("Retain only Polygons Associated with an Available RData File")

shp_cdiv = shp_cdiv[shp_cdiv["CLIMDIV"].isin(available_cdiv)]
shp_hucs = shp_hucs[shp_hucs[  "huc8" ].isin(available_hucs)]

print("  Available Polygon Elements")
print("       HUC08:CUT:",len(shp_hucs), len(shp_hucs.get_coordinates()))
print("     CLIMDIV:CUT:",len(shp_cdiv), len(shp_cdiv.get_coordinates()))

#
# Plot Climate Divisions if in a Jupyter Session
#

if (display_img) :
    display(shp_cdiv)
    display(shp_hucs)

print("----")

#
####################################################


# ## Dissolve Available Polygons

# In[ ]:


####################################################
#
# Dissolve Available Polygons
#

print("Dissolve All Available Polygons")

shp_cdiv = shp_cdiv.assign(CLIMDIV=1) 
shp_hucs = shp_hucs.assign(   huc8=1) 

shp_cdivD = shp_cdiv.dissolve(by = "CLIMDIV")
shp_hucsD = shp_hucs.dissolve(by =    "huc8")

print("  Available Polygon Elements")
print("       HUC08:DIS:",len(shp_hucsD), len(shp_hucs.get_coordinates()))
print("     CLIMDIV:DIS:",len(shp_cdivD), len(shp_cdiv.get_coordinates()))

print("----")

#
####################################################


# ## Produce HUC Image

# In[ ]:


####################################################
#
# Produce HUC Image
#

print("Produce HUC Image")


ccrs_proj = ccrs.AlbersEqualArea(central_longitude  =   -96, 
                                 central_latitude   =  37.5, 
                                 false_easting      =   0.0, 
                                 false_northing     =   0.0, 
                                 standard_parallels = (29.5, 45.5))

fig = plt.figure(figsize=[9,5], facecolor="white")
ax  = fig.add_subplot(1, 1, 1, 
                      projection = ccrs_proj)

#fig.suptitle("CMIP6 LOCA-2 Available Regions", color = Mines_Blue, fontsize= 'xx-large')
#ax.set_title("USGS HUC-08 Basins", color = Mines_Blue, fontsize= 'x-large')

ax.set_extent([-119, -73, 22.5, 51])

ax.add_feature(cfeature.COASTLINE, edgecolor = Mines_Blue)
ax.add_feature(cfeature.LAKES,     facecolor = 'None')
#ax.add_feature(cfeature.RIVERS,    edgecolor = Mines_Blue)
ax.add_feature(cfeature.STATES,    edgecolor = Mines_Blue, linewidth=0.5)


#ax.set_frame_on(False)

shp_hucs.plot(ax        = ax,
              aspect    = 'equal',
              facecolor = "Blue",
              edgecolor = 'white', 
              alpha     = 0.7,
              transform = ccrs.PlateCarree(),
              linewidth = .5)
plt.tight_layout()
plt.savefig(target_dir + "./LOCA2_HUCs_Available_Regions_Map.png")
if (display_img) :
    plt.show()

print("----")

#
####################################################


# In[ ]:





# ## Produce ClimDiv Image

# In[ ]:


####################################################
#
# Produce ClimDiv Image
#

ccrs_proj = ccrs.AlbersEqualArea(central_longitude  =   -96, 
                                 central_latitude   =  37.5, 
                                 false_easting      =   0.0, 
                                 false_northing     =   0.0, 
                                 standard_parallels = (29.5, 45.5))

fig = plt.figure(figsize=[9,5], facecolor="white")
ax  = fig.add_subplot(1, 1, 1, 
                      projection = ccrs_proj)

#fig.suptitle("CMIP6 LOCA-2 Available Regions", color = Mines_Blue, fontsize= 'xx-large')
#ax.set_title("NCEI State Climate Divisions", color = Mines_Blue, fontsize= 'x-large')
ax.set_extent([-119, -73, 22.5, 51])

ax.add_feature(cfeature.COASTLINE, edgecolor = Mines_Blue)
ax.add_feature(cfeature.LAKES,     facecolor = 'None')
#ax.add_feature(cfeature.RIVERS,    edgecolor = Mines_Blue)
ax.add_feature(cfeature.STATES,    edgecolor = Mines_Blue, linewidth=0.5)

#ax.set_frame_on(False)

shp_cdiv.plot(ax        = ax,
              aspect    = 'equal',
              facecolor = "red",
              edgecolor = 'white',
              alpha     = 0.7,
              transform = ccrs.PlateCarree(),
              linewidth = .5)

plt.tight_layout()
plt.savefig(target_dir + "./LOCA2_ClimDiv_Available_Regions_Map.png")
if (display_img) :
    plt.show()
    
print("----")

#
####################################################


# ## Produce Combined ClimDiv + HUC Image

# In[ ]:


####################################################
#
# Produce Combined ClimDiv + HUC Image
#

print("Produce Combined ClimDiv + HUC Image")

ccrs_proj = ccrs.AlbersEqualArea(central_longitude  =   -96, 
                                 central_latitude   =  37.5, 
                                 false_easting      =   0.0, 
                                 false_northing     =   0.0, 
                                 standard_parallels = (29.5, 45.5))

fig = plt.figure(figsize=[9,5], facecolor="white")
ax  = fig.add_subplot(1, 1, 1, 
                      projection = ccrs_proj)

#fig.suptitle("CMIP6 LOCA-2 Available Regions", color = Mines_Blue, fontsize= 'xx-large')
#ax.set_title("NCEI State Climate Divisions", color = Mines_Blue, fontsize= 'x-large')
ax.set_extent([-119, -73, 22.5, 51])

ax.add_feature(cfeature.COASTLINE, edgecolor = Mines_Blue)
ax.add_feature(cfeature.LAKES,     facecolor = 'None')
#ax.add_feature(cfeature.RIVERS,    edgecolor = Mines_Blue)
ax.add_feature(cfeature.STATES,    edgecolor = Mines_Blue, linewidth=0.5)

#ax.set_frame_on(False)

shp_cdiv.plot(ax        = ax,
              aspect    = 'equal',
              facecolor = "magenta",
              edgecolor = 'white', 
              alpha     = 0.5,
              transform = ccrs.PlateCarree(),
              linewidth = .5)

shp_hucs.plot(ax        = ax,
              aspect    = 'equal',
              facecolor = "cyan",
              edgecolor = 'white', 
              alpha     = 0.5,
              transform = ccrs.PlateCarree(),
              linewidth = .5)
plt.tight_layout()
plt.savefig(target_dir + "./LOCA2_Available_Regions_Map.png")
if (display_img) :
    plt.show()

print("----")

print("WE'RE OUTTA HERE LIKE VLADIMIR!")

print("====")

#
####################################################

