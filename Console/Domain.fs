namespace SharpEstimate.Domain

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

type FactorFRange = 
   { Low : decimal 
     LowFactorF : decimal 
     High : decimal 
     HighFactorF : decimal }

type LookupFactorFResult =
    | Single of FactorF:decimal
    | Range of Range:FactorFRange

type LookupFactorF = decimal -> LookupFactorFResult 

type ProjectInfo =
    { Name : string
      LookupFactorF : LookupFactorF
      BoQ : BoQ }

type Project =
    | Building of ProjectInfo 
    | Road of ProjectInfo 
    | Bridge of ProjectInfo 
    | Irrigation of ProjectInfo 

[<AutoOpen>]
module Material =
    [<Literal>]
    let Bangkok = "10"
    [<Literal>]
    let Nonthaburi = "12"
    [<Literal>]
    let Patumthani = "13"
    [<Literal>]
    let SamutPrakarn = "11"

    let CentralProvinces = [|Bangkok; Nonthaburi; Patumthani; SamutPrakarn|]

module FactorF =
    let calcFactorF (lookupFn:LookupFactorF) directCost =
        let calcProportion range = 
            (directCost - range.Low) / (range.High - range.Low) 
            |> (*) (range.HighFactorF - range.LowFactorF)
            |> (+) range.LowFactorF

        match (lookupFn directCost) with
        | Single factorF -> factorF
        | Range range -> calcProportion range 


module Project =
    let private toProjectInfo  =
        function  
        | Building info -> info
        | Road info -> info 
        | Bridge info -> info
        | Irrigation info -> info 

    let calcCost work = 
        (work.Quantity * work.MaterialUnitCost) + (work.Quantity * work.LaborUnitCost)

    let sumDirectCost project =
        let info = project |> toProjectInfo
        (0M, info.BoQ)
        ||> Seq.fold (fun acc work -> acc + (calcCost work) )

    let transportationCost projectInfo = 0M
    let operationCost projectInfo = 0M
    let irrigationCost projectInfo = 0M

    let estimate project = 
        let directCost = sumDirectCost project
        let info = project |> toProjectInfo
        let indirectCost = directCost * (FactorF.calcFactorF info.LookupFactorF directCost) 
        match project with 
        | Building info ->  indirectCost 
        | Road info -> indirectCost + (transportationCost info) + (operationCost info)
        | Bridge infol -> indirectCost + (transportationCost info) + (operationCost info)
        | Irrigation info -> indirectCost + (transportationCost info) + (operationCost info) + (irrigationCost info)
