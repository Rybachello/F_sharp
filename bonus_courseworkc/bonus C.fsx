(*** hide ***)
#load "packages/FsLab/Themes/DefaultWhite.fsx"
#load "packages/FsLab/FsLab.fsx"
(**

  ITT8060 Advanced Programming 2016  
  Department of Computer Science  
  Tallinn University of Technology  
  Bonus C Coursework : Data analysis using FsLab   
  Name: Mykola Rybak  
  Student ID: myryba  

  What is Birth rate?
  

  The birth rate (technically, births/population rate) is the total number of live births per
  1,000 of a population in a year. The rate of births in a population is calculated in several ways:    
      
  -live births from a universal registration system for births, deaths, and marriages;
   
  -population counts from a census;      
    
  -estimation through specialized demographic techniques.       

  The birth rate (along with mortality and migration rate) are used to calculate population growth.
  The crude birth rate is the number of live births per 1,000 people per year.Another term used 
  interchangeably with birth rate is natality. When the crude death rate is subtracted from the 
  crude birth rate, the result is the rate of natural increase (RNI). This is equal to the rate of 
  population change (excluding migration). The total (crude) birth rate (which includes all births)—typically 
  indicated as births per 1,000 population—is distinguished from an age-specific rate (the number of births
  per 1,000 persons in an age group).

*)
(*** define-output:loading ***)
open FSharp.Data
open Deedle
open System.IO
open System
open XPlot.GoogleCharts
open XPlot.GoogleCharts.Deedle

let wb = WorldBankData.GetDataContext()

let birthRateIn2014 =
 [ for c in wb.Countries do     
   let birthRate = c.Indicators.``Birth rate, crude (per 1,000 people)``.[2014]   
   if not(Double.IsNaN(birthRate)) then
        yield c.Name, birthRate
 ]

let options = 
    Options(
        title = "Birth rate, crude (per 1,000 people)",
        seriesType = "line",
        colorAxis=ColorAxis(colors=[|"#6CC627";"#DB9B3B";"#DB7532";"#DD5321";"#E00B00"|])
    )


(*** define-output:chart ***)
Chart.Geo(birthRateIn2014)
    |> Chart.WithLabels ["Name"; "Birth rate in %"]
    |> Chart.WithOptions options
(*** include-it:chart ***)
(**
  From the chart it is clear see that the highest rate in African countries.This is part of
  the fertility-income paradox, as these countries are very poor, and it may seem counter-intuitive
  for families there to have so many children. The inverse relationship between income and fertility
  has been termed a demographic-economic "paradox" by the notion that greater means would enable the
  production of more offspring as suggested by the influential Thomas Malthus  
  
  To represent the countries with the highest rate, need to sort and convert to series: 
*)
let topRate = birthRateIn2014 |> List.sortBy snd |> List.rev  |> List.toSeq |> series
(*** define-output:grid ***)
topRate
|> Series.sort
|> Series.rev
|> Series.take 10
|> Chart.Column
|> Chart.WithTitle "The top birth rate countries"
(*** include-it:grid ***)

(** 

*)

(**
 Summary

 The Fslab package is very useful and powerful tool that can help you to see resulting 
 time series, data frames, matrices, vectors and charts as nicely pretty printed HTML objects.
*)