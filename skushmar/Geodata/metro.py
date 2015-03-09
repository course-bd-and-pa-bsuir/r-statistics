# coding=utf8

import csv
import urllib.parse
import urllib.request
import json


def run():
    station_list = read_station_list()
    stations = get_station_coordinates(station_list)
    write_station_coordinates(stations)


def read_station_list():
    with open('metro_stations_2010.txt') as f:
        station_list = f.read().splitlines()
    return station_list


def get_station_coordinates(station_list):
    stations = []

    for station_name in station_list:
        geocode = 'Минск, метро ' + station_name

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

        stations.append([station_name, longitude, latitude])

    return stations


def write_station_coordinates(stations):
    stations.insert(0, ['Station', 'Longitude', 'Latitude'])  # add headers
    with open('metro_coordinates_2010.csv', 'w', newline='') as f:
        writer = csv.writer(f)
        writer.writerows(stations)


if __name__ == '__main__':
    run()
