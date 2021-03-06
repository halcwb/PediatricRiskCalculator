


module PRISM =

    open System

    type Item =
        | BloodPressure
        | Temperature
        | MentalStatus
        | HeartRate
        | Creatinine
        | Urea
        | ProthPT
        | ProthPTT
        | Pupils
        | Ph
        | TotalCO2
        | PCO2
        | PaO2
        | Glucose
        | Potassium
        | WBC
        | Platelets


    type AgePRISM3 = | Neonate | Infant | Child | Adolescent | AllMinNeonate | AnyAge


    type AgePRISM4 = | TwoWeeks | OneMonth | OneYear | EightTeen | UnknownAge


    type AdmissionSource =
        | Recovery
        | AnotherHospital
        | InHospital
        | EmergencyUnit
        | UnknownAdmissionSource


    type Mapping =
        {
            Item : Item
            AgeType : AgePRISM3
            LowPoints : int
            MidPoints : int
            HighPoints : int
            LowRange : float
            MidRangeLow : float option
            MidRangeHigh : float Option
            HighRange : float
        }


    let ageMap = function
    | TwoWeeks  -> 1.311
    | OneMonth  -> 0.968
    | OneYear   -> 0.357
    | EightTeen
    | UnknownAge -> 0.


    let admissionMap = function
    | EmergencyUnit -> 0.693
    | AnotherHospital -> 1.012 
    | InHospital ->  1.626
    | Recovery 
    | UnknownAdmissionSource  -> 0.


    //maps
    let mappings =
        [
            // blood pressures
            { Item = BloodPressure; AgeType = Neonate;       LowPoints = 7; MidPoints = 3; HighPoints = 0;  LowRange = 40.;  MidRangeLow = Some 40.;  MidRangeHigh = Some 55.;  HighRange = 55. }
            { Item = BloodPressure; AgeType = Infant;        LowPoints = 7; MidPoints = 3; HighPoints = 0;  LowRange = 45.;  MidRangeLow = Some 45.;  MidRangeHigh = Some 65.;  HighRange = 65. }
            { Item = BloodPressure; AgeType = Child;         LowPoints = 7; MidPoints = 3; HighPoints = 0;  LowRange = 55.;  MidRangeLow = Some 55.;  MidRangeHigh = Some 75.;  HighRange = 75. }
            { Item = BloodPressure; AgeType = Adolescent;    LowPoints = 7; MidPoints = 3; HighPoints = 0;  LowRange = 65.;  MidRangeLow = Some 65.;  MidRangeHigh = Some 85.;  HighRange = 85. }
            // temparature
            { Item = Temperature;   AgeType = Neonate;       LowPoints = 3; MidPoints = 0; HighPoints = 3;  LowRange = 33.;  MidRangeLow = Some 33.;  MidRangeHigh = Some 40.;  HighRange = 40. } 
            { Item = Temperature;   AgeType = Infant;        LowPoints = 3; MidPoints = 0; HighPoints = 3;  LowRange = 33.;  MidRangeLow = Some 33.;  MidRangeHigh = Some 40.;  HighRange = 40. } 
            { Item = Temperature;   AgeType = Child;         LowPoints = 3; MidPoints = 0; HighPoints = 3;  LowRange = 33.;  MidRangeLow = Some 33.;  MidRangeHigh = Some 40.;  HighRange = 40. }
            { Item = Temperature;   AgeType = Adolescent;    LowPoints = 3; MidPoints = 0; HighPoints = 3;  LowRange = 33.;  MidRangeLow = Some 33.;  MidRangeHigh = Some 40.;  HighRange = 40. }
            // mental status
            { Item = MentalStatus;  AgeType = Neonate;       LowPoints = 0; MidPoints = 5; HighPoints = 0;  LowRange = 1.;   MidRangeLow = None;      MidRangeHigh = None;      HighRange = 7. } 
            { Item = MentalStatus;  AgeType = Infant;        LowPoints = 0; MidPoints = 5; HighPoints = 0;  LowRange = 1.;   MidRangeLow = None;      MidRangeHigh = None;      HighRange = 7. } 
            { Item = MentalStatus;  AgeType = Child;         LowPoints = 0; MidPoints = 5; HighPoints = 0;  LowRange = 1.;   MidRangeLow = None;      MidRangeHigh = None;      HighRange = 7. } 
            { Item = MentalStatus;  AgeType = Adolescent;    LowPoints = 0; MidPoints = 5; HighPoints = 0;  LowRange = 1.;   MidRangeLow = None;      MidRangeHigh = None;      HighRange = 7. }
            // heart rate
            { Item = HeartRate;     AgeType = Neonate;       LowPoints = 0; MidPoints = 3; HighPoints = 4;  LowRange = 215.; MidRangeLow = Some 215.; MidRangeHigh = Some 225.; HighRange = 225. } 
            { Item = HeartRate;     AgeType = Infant;        LowPoints = 0; MidPoints = 3; HighPoints = 4;  LowRange = 215.; MidRangeLow = Some 215.; MidRangeHigh = Some 225.; HighRange = 225. } 
            { Item = HeartRate;     AgeType = Child;         LowPoints = 0; MidPoints = 3; HighPoints = 4;  LowRange = 185.; MidRangeLow = Some 185.; MidRangeHigh = Some 205.; HighRange = 205. } 
            { Item = HeartRate;     AgeType = Adolescent;    LowPoints = 0; MidPoints = 3; HighPoints = 4;  LowRange = 145.; MidRangeLow = Some 145.; MidRangeHigh = Some 155.; HighRange = 155. }
            // creatinine
            { Item = Creatinine;    AgeType = Neonate;       LowPoints = 0; MidPoints = 0; HighPoints = 2;  LowRange = 0.;   MidRangeLow = Some 0.;   MidRangeHigh = Some 0.;   HighRange = 75. } 
            { Item = Creatinine;    AgeType = Infant;        LowPoints = 0; MidPoints = 0; HighPoints = 2;  LowRange = 0.;   MidRangeLow = Some 0.;   MidRangeHigh = Some 0.;   HighRange = 80. } 
            { Item = Creatinine;    AgeType = Child;         LowPoints = 0; MidPoints = 0; HighPoints = 2;  LowRange = 0.;   MidRangeLow = Some 0.;   MidRangeHigh = Some 0.;   HighRange = 80. } 
            { Item = Creatinine;    AgeType = Adolescent;    LowPoints = 0; MidPoints = 0; HighPoints = 2;  LowRange = 0.;   MidRangeLow = Some 0.;   MidRangeHigh = Some 0.;   HighRange = 115. }
            // urea
            { Item = Urea;          AgeType = Neonate;       LowPoints = 0; MidPoints = 0; HighPoints = 3;  LowRange = 0.;   MidRangeLow = Some 0.;   MidRangeHigh = Some 0.;   HighRange = 4.3 } 
            { Item = Urea;          AgeType = AllMinNeonate; LowPoints = 0; MidPoints = 0; HighPoints = 3;  LowRange = 0.;   MidRangeLow = Some 0.;   MidRangeHigh = Some 0.;   HighRange = 5.4 }
            // prothPT
            { Item = ProthPT;       AgeType = Neonate;       LowPoints = 0; MidPoints = 0; HighPoints = 3;  LowRange = 0.;   MidRangeLow = Some 0.;   MidRangeHigh = Some 0.;   HighRange = 22.0 } 
            { Item = ProthPT;       AgeType = AllMinNeonate; LowPoints = 0; MidPoints = 0; HighPoints = 3;  LowRange = 0.;   MidRangeLow = Some 0.;   MidRangeHigh = Some 0.;   HighRange = 22.0 }           
            // prothPTT.
            { Item = ProthPTT;      AgeType = Neonate;       LowPoints = 0; MidPoints = 0; HighPoints = 3;  LowRange = 0.;   MidRangeLow = Some 0.;   MidRangeHigh = Some 0.;   HighRange = 85.0 } 
            { Item = ProthPTT;      AgeType = AllMinNeonate; LowPoints = 0; MidPoints = 0; HighPoints = 3;  LowRange = 0.;   MidRangeLow = Some 0.;   MidRangeHigh = Some 0.;   HighRange = 57.0 }  
            // pupils
            { Item = Pupils;        AgeType = AnyAge;        LowPoints = 0; MidPoints = 7; HighPoints = 11; LowRange = 0.;   MidRangeLow = Some 1.;   MidRangeHigh = Some 1.;   HighRange = 2.}
            // pHl
            { Item = Ph;            AgeType = AnyAge;        LowPoints = 0; MidPoints = 2; HighPoints = 3; LowRange = 7.47;  MidRangeLow = Some 7.48; MidRangeHigh = Some 7.55; HighRange = 7.55 }
            // tCO2
            { Item = TotalCO2;      AgeType = AnyAge;        LowPoints = 0; MidPoints = 0; HighPoints = 4; LowRange = 34.0;  MidRangeLow = Some 34.0; MidRangeHigh = Some 34.0; HighRange = 34.0 }
            // pCO2
            { Item = PCO2;          AgeType = AnyAge;        LowPoints = 0; MidPoints = 1; HighPoints = 3; LowRange = 50.0;  MidRangeLow = Some 50.0; MidRangeHigh = Some 75.0; HighRange = 75.0 }
            // pAO2
            { Item = PaO2;          AgeType = AnyAge;        LowPoints = 6; MidPoints = 3; HighPoints = 0; LowRange = 42.0;  MidRangeLow = Some 42.0; MidRangeHigh = Some 49.9; HighRange = 49.9 }
            // glu
            { Item = Glucose;       AgeType = AnyAge;        LowPoints = 0; MidPoints = 0; HighPoints = 2; LowRange = 0.;    MidRangeLow = Some 0.;   MidRangeHigh = Some 0.;   HighRange = 11.0 }
            // pot
            { Item = Potassium;     AgeType = AnyAge;        LowPoints = 0; MidPoints = 0; HighPoints = 3; LowRange = 0.;    MidRangeLow = Some 0.;   MidRangeHigh = Some 0.;   HighRange = 6.9 }
            // wbc
            { Item = WBC;           AgeType = AnyAge;        LowPoints = 0; MidPoints = 0; HighPoints = 4; LowRange = 0.;    MidRangeLow = Some 0.;   MidRangeHigh = Some 0.;   HighRange = 3000. }
            // wbc
            { Item = Platelets;     AgeType = AnyAge;        LowPoints = 2; MidPoints = 4; HighPoints = 5; LowRange = 50.;   MidRangeLow = Some 100.; MidRangeHigh = None;      HighRange = 200. }
        ]

    type Score = NonNeuro of int | Neuro of int

    type Value = NoValue | OneValue of float | TwoValues of float * float

    type ItemCalculator = AgePRISM3 -> Value -> Score

    type Input =
        {
            Age : DateTime option
            SystolicBloodPressure : Value
            Temperature : Value
            MentalStatus : Value
            HeartRate : Value
            PupilsFixed : Value
            PH : Value
            TotalCO2 : Value
            PCO2 : Value
            PaO2 : Value
            Glucose : Value
            Potassium : Value
            Creatinine : Value
            Urea : Value
            WhiteBloodCount : Value
            PT : Value
            PTT : Value
            Platelets : Value
            AdmissionSource : AdmissionSource
            CPR24HourBefore : bool
            Cancer : bool
            LowRiskPrimary : bool //Endocrine, hematologic, musculoskeletal, and renal systems of primary dysfunction are defined as ‘low risk.’
        }

    let input =
        {
            Age = None
            SystolicBloodPressure = NoValue
            Temperature = NoValue
            MentalStatus = NoValue
            HeartRate = NoValue
            PupilsFixed = NoValue
            PH = NoValue
            TotalCO2 = NoValue
            PCO2 = NoValue
            PaO2 = NoValue
            Glucose = NoValue
            Potassium = NoValue
            Creatinine = NoValue
            Urea = NoValue
            WhiteBloodCount = NoValue
            PT = NoValue
            PTT = NoValue
            Platelets = NoValue
            AdmissionSource = UnknownAdmissionSource
            CPR24HourBefore = false
            Cancer = false
            LowRiskPrimary = true
        }

    let mapPRISM3Age a =
        let diff (a : DateTime) = (DateTime.Now - a).TotalDays |> int
        match a with
        | None   -> AnyAge
        | Some a when a |> diff <= 30 -> Neonate
        | Some a when a |> diff <= 2 * 365 -> Infant
        | Some a when a |> diff <= 12 * 365 -> Child
        | Some _ -> Adolescent
      

    let mapPRISM4Age a =
        let diff (a : DateTime) = (DateTime.Now - a).TotalDays |> int
        match a with
        | a when a |> diff <= 14 -> TwoWeeks
        | a when a |> diff <= 2 * 365 -> OneMonth
        | a when a |> diff <= 12 * 365 -> OneYear
        | _ -> EightTeen


    let eqsAge a2 a1 =
        match a2 with
        | AnyAge -> true
        | AllMinNeonate -> a1 <> Neonate 
        | _ -> a1 = a2


    let calculators : (Item * ItemCalculator) list =
        // calculate the value for
        // item i, age a, value v
        // using function f
        let inline calc i a v f =
            mappings
            |> List.filter (fun x -> x.Item = i && a |> eqsAge x.AgeType)
            |> function
            | [m] ->
                printfn "going to calculate %A using mapping %A" v m
                f v m
            | _   ->
                sprintf "no mapping for %A %A" i a
                |> failwith
            |> fun s ->
                if i = MentalStatus then s |> Neuro
                else s |> NonNeuro
        // calculator for one value
        let calcOne i a v f =
            match v with
            | NoValue    -> 0 |> NonNeuro
            | OneValue v -> calc i a v f
            | TwoValues _ ->
                sprintf "expected one value but got %A" v
                |> failwith
        // calculator for two values
        let calcTwo i a v f =
            match v with
            | NoValue    -> 0 |> NonNeuro
            | OneValue v -> 
                sprintf "expected two values but got %A" v
                |> failwith
            | TwoValues (v1, v2) -> calc i a (v1, v2) f
        // generic calculator
        let genCalc v m =
            if v > m.HighRange then m.HighPoints;
            else 0
            
        // list of item and calculators
        [
            BloodPressure,
            fun a v ->
                fun v m ->
                    match m.MidRangeLow, m.MidRangeHigh with
                    | _ when (v < m.LowRange)                -> m.LowPoints
                    | Some l, Some h when (v >= l && v <= h) -> m.MidPoints
                    | _ when (v > m.HighRange)               -> m.HighPoints
                    | _ -> 0
                |> calcOne BloodPressure a v

            Temperature,
            fun a v ->
                fun (v1, v2) m ->
                    match m.MidRangeLow, m.MidRangeHigh with
                    | _ when (v1 < m.LowRange || v2 > m.HighRange) -> m.LowPoints
                    | _ -> m.MidPoints
                |> calcTwo Temperature a v

            MentalStatus,
            fun a v ->
                fun v m ->
                    if v >= m.LowRange && v <= m.HighRange then m.MidPoints
                    else 0
                |> calcOne MentalStatus a v

            HeartRate,
            fun a v ->
                fun v m ->
                    match m.MidRangeLow, m.MidRangeHigh with
                    | _ when v < m.LowRange                -> m.LowPoints
                    | Some l, Some h when v >= l && v <= h -> m.MidPoints
                    | _ when v > m.HighRange               -> m.HighPoints
                    | _ -> 0
                |> calcOne HeartRate a v

            Pupils,
            fun a v ->
                fun v m ->
                    match v with
                    | _ when v = 1. -> m.MidPoints
                    | _ when v = 2. -> m.HighPoints
                    | _ -> 0
                |> calcOne Pupils a v

            TotalCO2,
            fun a v ->
                fun (_, v2) m ->
                    match m.MidRangeLow with
                    | Some l when v2 > l -> m.HighPoints
                    | _ -> 0
                |> calcTwo TotalCO2 a v

            Ph,
            fun a v ->
                fun (_, v2) m ->
                    match m.MidRangeLow, m.MidRangeHigh with
                    | Some l, Some h when v2 >= l && v2 <= h -> m.MidPoints
                    | _ when v2 > m.HighRange -> m.HighPoints
                    | _ -> 0
                |> calcTwo Ph a v

            PCO2,
            fun a v ->
                fun v m ->
                    match v with
                    | _ when v >= m.LowRange && v <= m.HighRange -> m.MidPoints
                    | _ when v > m.HighRange                     -> m.HighPoints
                    | _ -> 0
                |> calcOne PCO2 a v

            PaO2,
            fun a v ->
                fun v m ->
                    match m.MidRangeLow, m.MidRangeHigh with
                    | _ when v < m.LowRange                -> m.LowPoints
                    | Some l, Some h when v >= l && v <= h -> m.MidPoints
                    | _ -> 0
                |> calcOne PaO2 a v

            Glucose,
            fun a v ->
                genCalc
                |> calcOne Glucose a v

            Potassium,
            fun a v ->
                genCalc
                |> calcOne Potassium a v

            Creatinine,
            fun a v ->
                genCalc
                |> calcOne Creatinine a v

            Urea,
            fun a v ->
                genCalc
                |> calcOne Urea a v

            WBC,
            fun a v ->
                fun v m ->
                    if v < m.HighRange then m.HighPoints
                    else 0
                |> calcOne WBC a v

            ProthPT,
            fun a v ->
                genCalc
                |> calcOne ProthPT a v

            ProthPTT,
            fun a v ->
                genCalc
                |> calcOne ProthPTT a v

            Platelets,
            fun a v ->
                fun v m ->
                    match m.MidRangeLow with
                    | _ when v < m.LowRange   -> m.HighPoints
                    | Some l when v < l       -> m.MidPoints
                    | _ when v <= m.HighRange -> m.LowPoints
                    | _ -> 0
                |> calcOne Platelets a v

        ]


    //if(rspt === rsptt){ 			
    //	setSubScore(clpt.lblId, rspt);
    //	setSubScore(clptt.lblId, 0);
    //	return;
    //}
    //if(rspt > rsptt) {
    //	setSubScore(clpt.lblId, rspt); 
    //	setSubScore(clptt.lblId, 0); 
    //	return;
    //}
    //if(rspt < rsptt) {
    //	setSubScore(clptt.lblId, rsptt);
    //	setSubScore(clpt.lblId, 0);
    //	return;
    let calculateCoagulation sPT sPTT =
        printfn "calculating coagulation %A and %A" sPT sPTT
        match sPT, sPTT with
        | NonNeuro rspt, NonNeuro rsptt when rspt > rsptt -> sPT
        | NonNeuro rspt, NonNeuro rsptt when rspt < rsptt -> sPTT
        | _ -> sPT            

    //function calAcidosisPh(lowestpH, lowestTotalcO2){                
    //    if(lowestpH < 7 || lowestTotalcO2 < 5) return 6;
    //    if(lowestpH <= 7.28 || lowestTotalcO2 <= 16.9) return 2;
    //    return 0;
    //}
    let calculateAcidosisPh pH tCO2 =
        match pH, tCO2 with
        | TwoValues(pHl, _), TwoValues(tCO2l, _) ->
            if pHl < 7. || tCO2l < 5. then 6
            elif pHl <= 7.28 || tCO2l <= 16.9 then 2
            else 0
        | _ -> 0
        |> NonNeuro

  
    let calcItem a item v =
        calculators
        |> List.tryFind (fst >> ((=) item))
        |> function
        | Some c ->
            c
            |> snd
            |> fun f ->
                f a v
        | None ->
            sprintf "no calculator for %A" item
            |> failwith


    let calcScore (input : Input) =
        let calc = input.Age |> mapPRISM3Age |> calcItem
        [
            input.SystolicBloodPressure |> calc BloodPressure 
            input.Creatinine            |> calc Creatinine 
            input.Glucose               |> calc Glucose 
            input.HeartRate             |> calc HeartRate 
            input.MentalStatus          |> calc MentalStatus 
            input.PaO2                  |> calc PaO2 
            input.PCO2                  |> calc PCO2 
            input.PH                    |> calc Ph 
            input.Platelets             |> calc Platelets 
            input.Potassium             |> calc Potassium 
            input.PupilsFixed           |> calc Pupils 
            input.Temperature           |> calc Temperature
            input.TotalCO2              |> calc TotalCO2
            input.Urea                  |> calc Urea
            input.WhiteBloodCount       |> calc WBC
            calculateAcidosisPh input.PH input.TotalCO2
            calculateCoagulation (input.PT |> calc ProthPT) (input.PTT |> calc ProthPTT)
        ]
        |> List.fold (fun acc s ->
            match acc with
            | Neuro n1, NonNeuro n2 ->
                match s with
                | Neuro n    -> Neuro (n + n1), NonNeuro n2
                | NonNeuro n -> Neuro n1,       NonNeuro (n + n2)
            | _ ->
                sprintf "invalid acc %A" acc
                |> failwith                 
        ) (Neuro 0, NonNeuro 0)


    let calcProbability input s =
        match input.Age with
        | None ->
            printfn "Cannot calculate probability without age for score: %A" s
            None
        | Some a ->
            let a = a |> mapPRISM4Age
            match s with
            | Neuro n1, NonNeuro n2 ->
                [
                    a |> ageMap
                    input.AdmissionSource |> admissionMap
                    if input.CPR24HourBefore then 1.082 else 0.
                    if input.Cancer then 0.766 else 0.
                    if input.LowRiskPrimary then -1.697 else 0.
                    (n1 |> float) * 0.197
                    (n2 |> float) * 0.163
                    -5.776
                ]
                |> List.reduce (+)
                |> fun x ->
                    Math.Exp(x) / (1. + Math.Exp(x))
                |> Some
            | _ ->
                printfn "not a valid score: %A" s
                None

    let calculate input =
        input
        |> calcScore
        |> fun s -> printfn "calculated score: %A" s; s
        |> calcProbability input
        

open System
open PRISM

input
|> fun i ->
    { i with
        Age = DateTime.Now.AddDays(-31.) |> Some
        SystolicBloodPressure = 60. |> OneValue
        PT = 120. |> OneValue
        MentalStatus = 7. |> OneValue
        AdmissionSource = EmergencyUnit
    }
|> calculate