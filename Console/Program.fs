open SharpEstimate.Domain

/// Estimates building project
let material = Central { Name = "Bangkok Tile"; Unit = "piece"; UnitCost = 42M }
let work = 
    { Name = "Tile"; Quantity = 2M; Material = material; MaterialUnitCost = 5M; LaborUnitCost = 2.5M }
printfn "Cost of work is %M." (Project.calcCost work) 

let lookupFactorF directCost =
    Range { Low = 10M; LowFactorF = 1.1M; High = 20M; HighFactorF = 1.2M } 

let building = 
    Building { Name = "Building 1"; BoQ = seq { work }; LookupFactorF = lookupFactorF }
let directCost = Project.sumDirectCost building
printfn "Direct Cost is %M." directCost

printfn "FactorF is %M." (FactorF.calcFactorF lookupFactorF directCost)

printfn "Estimate is %M." (Project.estimate building)