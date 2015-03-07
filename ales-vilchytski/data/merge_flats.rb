require 'csv'

flats = CSV.read("lab5_flat.csv", encoding: 'utf-8', headers: true)
geodata = CSV.read("flats_geodata.csv", encoding: 'utf-8', headers: true)

flats.each do |flat|
  geodata_flat = geodata.find { |gd| gd['id'] == flat['N'] }
  flat << geodata_flat.to_hash.tap do |gf| 
    gf.delete('id')
    gf['LnCenterDistance'] = Math.log(gf['CenterDistance'].to_f)
    gf['LnStationDistance'] = Math.log(gf['StationDistance'].to_f)
  end
end

CSV.open( "flats_plus_geodata.csv", 'w:utf-8', headers: true ) do |writer|
  writer << flats.headers
  flats.each do |f|
    writer << f
  end
end
