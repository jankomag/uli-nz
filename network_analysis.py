import numpy as np
import pandas as pd
import osmnx as ox
import geopandas as gpd
from shapely.geometry import MultiPolygon, Point
from shapely.ops import transform
import pyproj
import networkx as nx
import math
import matplotlib.pyplot as plt
from pyproj import CRS
import itertools
import networkx as nx
print("loaded libraries")

auckland_path = "data/geographic/territorial/auckland_polygon_diy.gpkg"
auckland = gpd.read_file(auckland_path)
auckland = auckland.to_crs(crs=4326)
polygon = auckland.iloc[0]['geometry']
print("loaded polygon of auckland")

G = ox.graph.graph_from_polygon(polygon, network_type="walk", simplify = True)
print("loaded graph_from_polygon of auckland")
G = ox.simplification.consolidate_intersections(G, rebuild_graph=True, tolerance=10, dead_ends=False)
print("loaded graph_from_polygon of auckland")


stations = gpd.read_file("data/transport/public_transport/trains_auckland_with_distances_on_network.gpkg")
stations['lon'] = stations['geometry'].x
stations['lat'] = stations['geometry'].y
stations["node"] = stations.geometry.apply(lambda x: ox.distance.nearest_nodes(G, x.x, x.y))
stations = gpd.GeoDataFrame(stations, geometry = stations.geometry)
print("loaded stations of auckland")


sa1 = gpd.read_file("data/geographic/sa1_centorids_clean.gpkg")
sa1 = gpd.GeoDataFrame(sa1, geometry = sa1.geometry)
sa1 = sa1.to_crs(stations.crs)
sa1["node"] = sa1.geometry.apply(lambda x: ox.distance.nearest_nodes(G, x.x, x.y))                                                                         
print("loaded sa1s of auckland")

# define function to acquire path
def get_path(G, origin, dest):
    path=ox.shortest_path(G, origin, dest, weight='travel_time') 
    return path

# define function to acquire path length
def get_path_len(G, path):
    path = sum(ox.utils_graph.get_route_edge_attributes(G, path, 'length'))
    return path


# for loop
distances = []
for pidx, point in enumerate(sa1.itertuples()):
    path_lengths = []

    for sidx, station in enumerate(stations.itertuples()):
        # get path
        path = get_path(G, sa1['node'].iloc[pidx], stations['node'].iloc[sidx])
        path_len = get_path_len(G, path)
        path_lengths.append(path_len)
        print("doing station ", sidx)
        
    min_dist_to_station = min(path_lengths)
    distances.append(min_dist_to_station)
    print("doing point ", pidx)

sa1['dist_station'] = distances

sa1.to_file("sa1_station_dists.gpkg", driver="GPKG")



