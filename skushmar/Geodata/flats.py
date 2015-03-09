# coding=utf8

import csv
import urllib.parse
import urllib.request
import json
from math import radians, cos, sin, asin, sqrt


def run():
    flats_address = read_flats_address()
    station_coordinates = read_station_coordinates()
    flats_geodata = get_flats_geodata(flats_address, station_coordinates)
    write_flats_geodata(flats_geodata)


def read_flats_address():
    flats_address = []
    with open('flats_address.csv', newline='') as f:
        reader = csv.reader(f, delimiter=';')
        for row in reader:
            flats_address.append(row)

    flats_address = flats_address[1:]  # exclude headers
    return flats_address


def read_station_coordinates():
    station_coordinates = []
    with open('metro_coordinates_2010.csv', newline='') as f:
        reader = csv.reader(f, delimiter=',')
        for row in reader:
            station_coordinates.append(row)

    station_coordinates = station_coordinates[1:]  # exclude headers

    # convert longitude and latitude to float
    for station in station_coordinates:
        station[1] = float(station[1])
        station[2] = float(station[2])

    return station_coordinates


def get_flats_geodata(flats_address, station_coordinates):
    for flat in flats_address:
        geocode = 'Минск, ' + flat[1]

        data = urllib.parse.urlencode({'format': 'json', 'geocode': geocode})
        data = data.encode('utf-8')

        # https://tech.yandex.ru/maps/doc/geocoder/desc/concepts/About-docpage/
        request = urllib.request.Request("http://geocode-maps.yandex.ru/1.x/")
        request.add_header("Content-Type", "application/x-www-form-urlencoded;charset=utf-8")
        response = urllib.request.urlopen(request, data)

        json_string = response.read().decode('utf-8')
        json_object = json.loads(json_string)

        coordinates = json_object['response']['GeoObjectCollection']['featureMember'][0]['GeoObject']['Point']['pos']
        (longitude, latitude) = coordinates.split()
        (longitude, latitude) = (float(longitude), float(latitude))

        center_distance = get_distance_to_center(longitude, latitude)
        closest_station, station_distance = get_station_geodata(station_coordinates, longitude, latitude)

        flat.append(longitude)
        flat.append(latitude)
        flat.append(center_distance)
        flat.append(closest_station)
        flat.append(station_distance)

    return flats_address


def write_flats_geodata(flats_geodata):
    # add headers
    flats_geodata.insert(0, ['id', 'Address', 'Longitude', 'Latitude', 'CenterDistance', 'Station', 'StationDistance'])
    with open('flats_geodata.csv', 'w', newline='') as f:
        writer = csv.writer(f)
        writer.writerows(flats_geodata)


def get_distance_to_center(flat_longitude, flat_latitude):
    # 'Кастрычніцкая плошча' coordinates
    center_longitude = 27.56187
    center_latitude = 53.902275

    return haversine(flat_longitude, flat_latitude, center_longitude, center_latitude)


def get_station_geodata(station_coordinates, flat_longitude, flat_latitude):
    min_distance = 100000  # set initial distance to 100 km
    closest_station = ''
    for station in station_coordinates:
        station_latitude = station[1]
        station_longitude = station[2]

        distance = haversine(flat_longitude, flat_latitude, station_latitude, station_longitude)
        if distance < min_distance:
            min_distance = distance
            closest_station = station[0]

    return closest_station, min_distance


# http://stackoverflow.com/questions/15736995/how-can-i-quickly-estimate-the-distance-between-two-latitude-longitude-points
def haversine(lon1, lat1, lon2, lat2):
    """
    Calculate the great circle distance between two points
    on the earth (specified in decimal degrees)
    """
    # convert decimal degrees to radians
    lon1, lat1, lon2, lat2 = map(radians, [lon1, lat1, lon2, lat2])
    # haversine formula
    dlon = lon2 - lon1
    dlat = lat2 - lat1
    a = sin(dlat/2)**2 + cos(lat1) * cos(lat2) * sin(dlon/2)**2
    c = 2 * asin(sqrt(a))
    metres = 6367000 * c
    return round(metres)

if __name__ == '__main__':
    run()
