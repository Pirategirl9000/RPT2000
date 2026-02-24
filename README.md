# RPT2000
![Image of Output](assets/RPT2000Output.png)
## Author
* [Violet French](https://github.com/Pirategirl9000)

## Table of Contents
* [Author](#author)
* [Purpose](#purpose)
* [Script Breakdown](#script-breakdown)

## Purpose
This program uses a dataset to produce a report based on customer sales reports. The resulting report will be stored to a new dataset. The report details the spendings for this year and last as well as the difference between the two for each customer.

## Script Breakdown

### File Definitions
* `CUSTMAST` - The name of the input file
  * `CUSTOMER-MASTER-RECORD` - A record containing all the information about each customer
* `ORPT2000` - The COBOL alias for the output file which is RPT2000
  * `PRINT-AREA` - 130 size picture clause for writing to the file  
### Notable Data Items & Records
* `CUSTMAST-EOF-SWITCH` - Marks when the end of the file has been reached
* `PRINT-FIELDS` - Record containing information about the page including lines per page, current line, and page number
* `TOTAL-FIELDS` - Record containing information about the grand totals for last YTD and this YTD
* `CURRENT-DATE-AND-TIME` - Record used for grabbing the current data and time via the CURRENT-DATE-AND-TIME function
* `CHANGE-AMOUNT` - Contains the difference in sales between last YTD and this YTD
