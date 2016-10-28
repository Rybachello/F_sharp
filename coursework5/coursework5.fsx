(*

  ITT8060 -- Advanced Programming 2016
  Department of Computer Science
  Tallinn University of Technology
  ------------------------------------

  Coursework 5: Working with data: intro to type providers and charting

  ------------------------------------
  Name: mykola rybak
  TUT Student ID: myryba
  ------------------------------------


  Answer the questions below.  You answers to questions 1--6 should be
  correct F# code written after the question. This file is an F#
  script file, it should be possible to load the whole file at
  once. If you can't then you have introduced a syntax error
  somewhere.

  This coursework will be graded. It has to be submitted to the TUT
  git system using the instructions on the course web page by October 29, 2016.
*)

// 1) The following three different fuel consumption units for vehicles are in use:
//    * litres per 100 km
//    * miles per imperial gallon (in use in the UK)
//    * miles per US gallon (in use in the US)
// 1.a) Define the units in terms of units of measure.

[<Measure>] type litresPer100km
[<Measure>] type mpgUK 
[<Measure>] type mpgUS

// 1.b) Define 2 functions that convert the appropriate US and imperial mpg values to
//      litres per 100 km. 

let convertMilesUKtoLitres (temp:float<mpgUK>) = (100.0<litresPer100km>*4.54509<mpgUK>) / (1.609344 * temp)
let convertMilesUStoLitres (temp:float<mpgUS>) =(100.0<litresPer100km>*3.785411784<mpgUS>) / (1.609344 * temp)
//converMilesUStoLitres 85.6<mpgUK>
//val it : float<litresPer100km> = 14.12093996
//convertMilesUStoLitres  85.6<mpgUS>
//val it : float<litresPer100km> = 11.76072917

// 1.c) Define a function that converts litres per 100 km of appropriate fuel to
//      CO2 emissions g per km.

[<Measure>] type CO2perKmGasoline
[<Measure>] type CO2perKmDiesel
[<Measure>] type CO2perKmAutogas

//litres per 100 km to gram of CO2 per km (Gasoline)
let convertLiterPer100ToCO2emGasoline (temp:float<litresPer100km>) = temp * 26.5<CO2perKmGasoline> / 1.0<litresPer100km>
//litres per 100 km to gram of CO2 per km (Diesel)
let convertLiterPer100ToCO2emDiesel (temp:float<litresPer100km>) = temp * 23.2<CO2perKmDiesel> / 1.0<litresPer100km>
//litres per 100 km to gram of CO2 per km (Autogas)
let convertLiterPer100ToCO2emAutogas (temp:float<litresPer100km>) = temp * 19.0<CO2perKmAutogas> / 1.0<litresPer100km>
convertLiterPer100ToCO2emGasoline 20.0<litresPer100km>
//val it : float<CO2perKmGasoline> = 530.0
convertLiterPer100ToCO2emDiesel 20.0<litresPer100km>
//val it : float<CO2perKmDiesel> = 464.0
convertLiterPer100ToCO2emAutogas 20.0<litresPer100km>
//val it : float<CO2perKmGasoline> = 380.0

// 2) Get the fuel consumption data
// 2.a) in imperial MPG (miles per imperial gallon) of at least 20 vehicles from
// http://carfueldata.direct.gov.uk/search-by-fuel-economy.aspx
// Save the data in file called imperial.csv

 
#r @"..\packages\FSharp.Data.2.3.2\lib\net40\FSharp.Data.dll"
open FSharp.Data

//let firstRow = t.Rows |> Seq.head
//let manufacturer = firstRow.Manufacturer
//let CO2 = firstRow.CO2

// 2.b) Get the fuel consumption data of at least 20 cars in US MPG (miles per US gallon) from
// https://www.fueleconomy.gov/feg/download.shtml
// save the data in file called us.csv

// 3) Load the imperial.csv and us.csv files using FSharp.Data.CsvProvider<>

type imperialType = FSharp.Data.CsvProvider<"./imperial.csv">
let imperialData = imperialType.Load("./imperial.csv")


type usType =  FSharp.Data.CsvProvider<"./us.csv">
let usData = usType.Load("./us.csv")

// 4) Write a function to convert the appropriate mpg data into
//    litres per 100 km using the functions defined in Q1.

let convertToMgpUK (f: decimal)  = System.Convert.ToDouble(f) * 1.0<mpgUK>
//function to convert uk data and sort by float<litresPer100>
let convertImperialDataToLitresPer100km (data: imperialType) = data.Rows |> Seq.map (fun row  ->row.ImperialCombined |> convertToMgpUK |> convertMilesUKtoLitres) //|> Seq.sortBy (fun x -> (fst x)) //,row.Model
//converted data
let convertedImperialData = convertImperialDataToLitresPer100km imperialData

let convertToMpgUS (f: int)  = System.Convert.ToDouble(f) * 1.0<mpgUS>
//function to convert us data, filtering and sort
let convertUSDataToLitresPer100km (data: usType)= data.Rows|> Seq.map (fun row  -> (row.``City MPG`` |> convertToMpgUS |> convertMilesUStoLitres))//, row.``Comb CO2``)) |> Seq.sortBy (fun x -> (fst x))
//converted data
let convertedUSData = convertUSDataToLitresPer100km usData
// 5) Display the converted data in an appropriate chart (select the type that is most 
//    appropriate for displaying the data).

#r @"..\packages\FSharp.Charting.0.90.14\lib\net40\FSharp.Charting.dll"

open FSharp.Charting
open FSharp.Charting.ChartTypes
//
let imperialCarModels = imperialData.Rows |> Seq.map (fun row  ->row.Model)

//Chart.Line(convertedImperialData,"Imperial Data").ShowChart()
let c1 = Chart.Line (convertedImperialData, Title = "Imperial data",Labels = imperialCarModels,YTitle = "Model Number", XTitle = "Liters/100km")
c1.ShowChart()

//let imperialChart = Chart.Line(convertImp,Name="Liters per 100 km from Imperial galons",Title="L/100km",Labels=imperialCarModels ,Color=System.Drawing.Color.PaleVioletRed,XTitle="Car Number",YTitle="L/100km")
let usCarModels = usData.Rows |> Seq.map (fun row  ->row.Model)

let c2 = Chart.Line (convertedUSData, Title = "US Data",Labels = usCarModels, YTitle = "Model Number", XTitle = "Liters/100km")
c2.ShowChart()

// 6) Combine the data from 2 data sources into a single chart. Add appropriate titles and
//    legends. 

Chart.Combine(
   [ Chart.Line (convertedImperialData, Title = "Imperial data",Labels = imperialCarModels,YTitle = "Model Number", XTitle = "Liters/100km")
     Chart.Line (convertedUSData, Title = "US Data",Labels = usCarModels, YTitle = "Model Number", XTitle = "Liters/100km") ]).ShowChart()
