type MaterialInfo =
    { Name : string 
      Unit : string
      UnitCost : decimal }

type Material =
    | Central of MaterialInfo
    | Provincial of MaterialInfo
    | Investigated of MaterialInfo

type CostOfWork =
    { Name : string 
      Quantity : decimal 
      Material : Material
      MaterialUnitCost : decimal
      LaborUnitCost : decimal }

type BoQ = CostOfWork seq

type ProjectInfo =
    { Name : string
      BoQ : BoQ }

type Project =
    | Building of ProjectInfo 
    | Road of ProjectInfo 
    | Bridge of ProjectInfo 
    | Irrigation of ProjectInfo 

type FactorFRange = 
    { Low : decimal 
      LowFactorF : decimal 
      High : decimal 
      HighFactorF : decimal }

type LookupFactorFResult =
    | Single of FactorF:decimal
    | Range of Range:FactorFRange

type LookupFactorF = decimal -> LookupFactorFResult 

let toProjectInfo  =
    function  
    | Building info -> info
    | Road info -> info 
    | Bridge info -> info
    | Irrigation info -> info 

[<Literal>]
let Bangkok = "10"
[<Literal>]
let Nonthaburi = "12"
[<Literal>]
let Patumthani = "13"
[<Literal>]
let SamutPrakarn = "11"

let CentralProvinces = [|Bangkok; Nonthaburi; Patumthani; SamutPrakarn|]

let searchCentralMaterials estimatedMonth query =
    [ Central { Name = "Bangkok Tile"; Unit = "piece"; UnitCost = 42M } ]

let searchProvincialMaterials provinceCode estimatedMonth query =
    [ Provincial { Name = "Korat Tile"; Unit = "piece"; UnitCost = 55M } ]

let searchNearbyProvincialMaterials provinceCode estimatedMonth query =
    [ Provincial { Name = "Khonkaen Tile"; Unit = "piece"; UnitCost = 67M } ]

let findNearestProvice provinceCode =
    (int provinceCode) + 1 |> string

let searchMaterials centralSearch provinceSearch provinceCode estimatedMonth query =
    let isCentral = CentralProvinces |> Array.contains provinceCode
    let nearestProvince = findNearestProvice provinceCode
    let nearbySearch = provinceSearch nearestProvince
    if isCentral then 
        centralSearch estimatedMonth query, nearbySearch estimatedMonth query
    else 
        provinceSearch provinceCode estimatedMonth query, nearbySearch  estimatedMonth query

// Test search materials
searchMaterials searchCentralMaterials searchProvincialMaterials Bangkok  1 "Tile"

let calcCost work = 
    (work.Quantity * work.MaterialUnitCost) + (work.Quantity * work.LaborUnitCost)

let sumDirectCost project =
    let info = project |> toProjectInfo
    (0M, info.BoQ)
    ||> Seq.fold (fun acc work -> acc + (calcCost work) )

let singleLookup : LookupFactorF =
    fun directCost ->
        Single 1.1M

let rangeLookup : LookupFactorF =
    fun directCost ->
        Range { Low = 10M; LowFactorF = 1.1M; High = 20M; HighFactorF = 1.2M } 

let calcFactorF (lookupFn:LookupFactorF) directCost =
    let calcProportion range = 
        (directCost - range.Low) / (range.High - range.Low) 
        |> (*) (range.HighFactorF - range.LowFactorF)
        |> (+) range.LowFactorF

    match (lookupFn directCost) with
    | Single factorF -> factorF
    | Range range -> calcProportion range 

// Test lookup and calculate factor f
calcFactorF ( fun _ -> Single 1.0M) 100M
calcFactorF ( fun _ -> Range { Low = 10M; LowFactorF = 1.1M; High = 20M; HighFactorF = 1.2M } ) 15M

let calcBuidingFactorF = calcFactorF rangeLookup  
let calcRoadFactorF = calcFactorF rangeLookup 
let calcBridgeFactorF = calcFactorF rangeLookup 
let calcIrrigationFactorF = calcFactorF rangeLookup 

let transportationCost projectInfo = 0M
let operationCost projectInfo = 0M
let irrigationCost projectInfo = 0M

let estimate project = 
    let directCost = sumDirectCost project
    match project with 
    | Building _ ->  directCost * (calcBuidingFactorF directCost) 
    | Road info -> directCost  * (calcRoadFactorF directCost) + (transportationCost info) + (operationCost info)
    | Bridge info -> directCost  * (calcBridgeFactorF directCost) + (transportationCost info) + (operationCost info)
    | Irrigation info -> directCost  * (calcIrrigationFactorF directCost) + (transportationCost info) + (operationCost info) + (irrigationCost info)

/// Test calcuate cost of work
let material = Central { Name = "Bangkok Tile"; Unit = "piece"; UnitCost = 42M }
let work = 
    { Name = "Tile"; Quantity = 2M; Material = material; MaterialUnitCost = 5M; LaborUnitCost = 2.5M }
(calcCost work) = 15M

/// Estimates building project
let building = Building { Name = "Building 1"; BoQ = seq { work }}
let directCost = sumDirectCost building
calcBuidingFactorF directCost
estimate building
