# hab-template

HAB occurrence templates for OBIS and scripts to convert the HAB template to Darwin Core. Download the latest template [here](https://github.com/iobis/hab-template/blob/master/templates/habtemplate_a_v5.xlsx?raw=true).

## Documentation

The table below describes the fields present in version 5 of the template.

| Field | Description | Example |
| ---- | ---- | ---- |
| Scientific name | Scientific name, can be any rank (drop down). | Alexandrium catenella |
| Reported name | Scientific name in the original source, if different from the scientific name entered in the first column. |      |
| Identification status | Quality assessment of the identification (drop down). | 1 - good |
| Reference | Reference to primary source of the occurrence. |      |
| Additional references | References to literature associated with the occurrence, separated by `|`. |      |
| HAEDAT URL | URL of the HAEDAT event this occurrence is linked to. | http://haedat.iode.org/viewEvent.php?eventID=7453 |
| Last modified |      |      |
| Date | Date of the occurrence in ISO 8601 format. | 2021-08-31 |
| Verbatim date | Date as specified in the original source. |      |
| Latitude | Latitude in decimal degrees. | 52.38 |
| Longitude | Longitude in decimal degrees. | 1.56 |
| Coordinate uncertainty | Coordinate uncertainty in meters. | 200 |
| WKT | WKT string for a polygon or linestring. |      |
| Locality |  | Baie de Morlaix |
| Minimum depth | Minimum depth in meters. | 0 |
| Maximum depth | Maximum depth in meters. | 20 |
| Phytoplankton quantity value |      | 100000 |
| Phytoplankton quantity unit | | cells per litre |
| Toxicity toxin | | CTX3C |
| Toxicity value | | 2.49 |
| Toxicity unit | | fg eq cell-1 |
| Remarks | | |

