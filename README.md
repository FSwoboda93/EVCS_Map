OpenChargeMap is a global, open, free and public data collector which gathers information about charging stations for electrical vehicles. The OCM allows data access and contributions to the database through the web site/app, the mobile app and a public API for software developers. The OCM location data can be entered either manually provided (“crowd sourced”) by the users or imported from commercial, governmental and other public sources. Due the variety of information sources, there is no general license agreement for the whole OCM. Instead, each OCM location is licensed independently, therefore a licence and attribution is attached to the data for each location. [1]

• What data is existent and can be accessed by you?

The following data exists and can be accessed by any user with the OpenChargeMap API, which provides an export of the (charging location) data in different formats (json, xml, kml, csv).

Data retrieved with csv-file:
-	Charging location identifiers: ID, UUID
-	Location address: title, address line(s), town, state or province, postcode, country, latitude/longitude
-	Location information: telephone(s), email, access comments, general comments, related URL
-	Connection: connection type, charger type, usage type, number of points, general comments
-	Location status: last confirmed date, status type, last status update date, date created
-	URL request parameters: distance (from “x latitude, y longitude”), distance unit (miles or km)

Data retrieved with json format:
-	Open data request here (https://api.openchargemap.io/v2/poi/?output=json&countrycode=DE&maxresults=1)

•  What can (cannot) be realized with this data source?

Depending on the export data format, some data can be lost:
-	Charging usage cost exists in json but not in csv-file.
-	Charging connection details such as: charging station current ("Amps"), voltage ("Voltage"), Power ("PowerKW") and current type 	description among others, exist in json format, in contrast a csv-file only provides the connection type title.

Charging station occupancy, availability and waiting times data of each station cannot be realized.


•  Dashboard-element mockup (sketch or working prototype screenshot) on how to visualize the data

•  Geographic extend (Germany, Europe, World?)

OCM has a globally extension of charging stations, but the scope of the project is Germany.
Especially 134761 charging stations across 66881 locations in Germany exist.


•  Challenges for implementation, maybe needed assistance from other Groups
	
-	Use json format as data instead of a conventional csv file.
- 	Data lost when csv-file is requested as export format.
- 	Data provided by users ("crowd sourced") might be missing or incomplete.
- 	Data duplication due that OCM location data comes from several data sources including contributions from OCM users. 
-       	OCM API is sometimes not responding

References:

[1] OpenChargeMap. Retrieved from https://wiki.openstreetmap.org/wiki/OpenChargeMap [Last access 06.11.2018]