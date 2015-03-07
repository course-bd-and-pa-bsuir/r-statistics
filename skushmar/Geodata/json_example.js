var r = {
    'response': {
        'GeoObjectCollection': {
            'metaDataProperty': {
                'GeocoderResponseMetaData': {
                    'request': 'Минск, метро Петровщина',
                    'found': '1',
                    'results': '10'}
            },
            'featureMember': [
                {
                    'GeoObject': {
                        'name': 'метро Петровщина',
                        'metaDataProperty': {
                            'GeocoderMetaData': {
                                'AddressDetails': {
                                    'Country': {
                                        'CountryNameCode': 'BY',
                                        'CountryName': 'Беларусь',
                                        'AddressLine': 'Минск, Московская линия, метро Петровщина',
                                        'AdministrativeArea': {
                                            'AdministrativeAreaName': 'Минск',
                                            'Locality': {
                                                'LocalityName': 'Минск',
                                                'Thoroughfare': {
                                                    'Premise': {
                                                        'PremiseName': 'метро Петровщина'
                                                    },
                                                    'ThoroughfareName': 'Московская линия'
                                                }
                                            }
                                        }
                                    }
                                },
                                'precision': 'other',
                                'kind': 'metro',
                                'text': 'Беларусь, Минск, Московская линия, метро Петровщина'
                            }
                        },
                        'Point': {
                            'pos': '27.485834 53.864788'
                        },
                        'description': 'Московская линия, Минск, Беларусь',
                        'boundedBy': {
                            'Envelope': {
                                'lowerCorner': '27.477605 53.859924',
                                'upperCorner': '27.494062 53.869651'
                            }
                        }
                    }
                }
            ]
        }
    }
};

