namespace Shared

module PIM =

    open System

    type PIM =
        { Urgency: AdmissionUrgency
          Recovery: bool
          RiskDiagnosis: RiskDiagnosis list
          CardiacByPass: bool
          CardiacNonByPass: bool
          NonCardiacProcedure: bool
          Ventilated: bool
          AdmissionPupils: PupilResponse
          PaO2: float option
          FiO2: float option
          BaseExcess: float option
          SystolicBloodPressure: float option }

    and AdmissionUrgency =
        | Elective
        | NotElective
        | UnknownUrgency

    and AdmissionType =
        | Medical
        | Surgery
        | DOA
        | UnknownAdmissionType

    and PupilResponse =
        | FixedDilated
        | NormalPupils
        | UnknownPupils

    and RiskDiagnosis =
        | Asthma
        | BoneMarrowTransplant
        | Bronchiolitis
        | CardiacArrest
        | CardiomyopathyOrMyocarditis
        | CerebralHemorrhage
        | Croup
        | DiabeticKetoacidosis
        | HIVPositive
        | HypoplasticLeftHeartSyndrome
        | LeukemiaorLymphoma
        | LiverFailure
        | NecrotizingEnterocolitis
        | NeurodegenerativeDisorder
        | ObstructiveSleepApnea
        | SeizureDisorder
        | SevereCombinedImmuneDeficiency

    let pim =
        { Urgency = UnknownUrgency
          Recovery = false
          RiskDiagnosis = []
          CardiacByPass = false
          CardiacNonByPass = false
          NonCardiacProcedure = false
          Ventilated = false
          AdmissionPupils = UnknownPupils
          PaO2 = None
          FiO2 = None
          BaseExcess = None
          SystolicBloodPressure = None }

    // PIM2
    //High Risk Diagnoses include:
    //Cardiac Arrest
    //Cardiomyopathy or Myocarditis
    //Severe Combined Immunodeficiency (SCID)
    //Hypoplastic Left Heart Syndrome (HLHS)
    //Leukemia or Lymphoma after first induction of chemotherapy
    //Liver Failure as primary ICU admission reason
    //HIV Positive
    //Spontaneous Cerebral Hemorrhage (from any cause, except intracranial bleeding related to head trauma)
    //Neurodegenerative Disorder
    let pim2HighRisk =
        [ CardiacArrest
          CardiomyopathyOrMyocarditis
          SevereCombinedImmuneDeficiency
          HypoplasticLeftHeartSyndrome
          LeukemiaorLymphoma
          LiverFailure
          HIVPositive
          CerebralHemorrhage
          NeurodegenerativeDisorder ]
    //Low Risk Diagnoses include:
    //Asthma
    //Bronchiolitis
    //Croup
    //Obstructive Sleep Apnea (OSA)
    //Diabetic Ketoacidosis (DKA)
    let pim2LowRisk =
        [ Asthma
          Bronchiolitis
          Croup
          ObstructiveSleepApnea
          DiabeticKetoacidosis ]
    // PIM3
    // Low-risk diagnosis:
    //  asthma,
    //  bronchiolitis,
    //  croup,
    //  obstructive sleep apnea,
    //  diabetic ketoacidosis,
    //  seizure disorder.
    let pim3LowRisk =
        [ Asthma
          Bronchiolitis
          Croup
          ObstructiveSleepApnea
          DiabeticKetoacidosis
          SeizureDisorder ]
    // High-risk diagnosis:
    //  spontaneous cerebral hemorrhage,
    //  cardiomyopathy or myocarditis,
    //  hypoplastic left heart syndrome,
    //  neurodegenerative disorder,
    //  necrotizing enterocolitis.
    let pim3HighRisk =
        [ CerebralHemorrhage
          CardiomyopathyOrMyocarditis
          HypoplasticLeftHeartSyndrome
          NeurodegenerativeDisorder
          NecrotizingEnterocolitis ]
    // Very high-risk diagnosis:
    //  cardiac arrest preceding ICU admission,
    //  severe combined immune deficiency,
    //  leukemia or lymphoma after first induction,
    //  bone marrow transplant recipient,
    //  liver failure.
    let pim3VeryHighRisk =
        [ CardiacArrest
          SevereCombinedImmuneDeficiency
          LeukemiaorLymphoma
          BoneMarrowTransplant
          LiverFailure ]

    let calcRiskFromScore score = Math.Exp(score) / (1. + Math.Exp(score))


    // PIM2 score =
    //    -0.9282(Elective) +
    //    -1.0244(Recovery) +
    //    0.7507(Bypass) +
    //    1.6829(High-Risk) +
    //    -1.577(Low-Risk) +
    //    3.0791(Pupils) +
    //    1.3352(Ventilator) +
    //    0.01395(absolute value of SBP-120) +
    //    0.1040(absolute value of base excess) +
    //    0.2888(100 x FiO2 / PaO2) +
    //    -4.8841
    let calculatePIM2 (pim: PIM) =
        let mapHighRisk rd =
            pim2HighRisk
            |> List.exists (fun d -> rd |> List.exists ((=) d))
            |> fun b -> if b then 1.6829 |> Some else None

        let mapLowRisk rd =
            pim2LowRisk
            |> List.exists (fun d -> rd |> List.exists ((=) d))
            |> fun b -> if b then -1.577 |> Some else None

        let paO2 = pim.PaO2 |> Option.defaultValue 0.

        [ if pim.Urgency = Elective then -0.9282 else 0.
          // recovery
          if pim.Recovery then -1.0244 else 0.
          // bypass
          if pim.CardiacByPass then 0.7507 else 0.

          // take the max risk value
          [ pim.RiskDiagnosis |> mapHighRisk
            // lowRisc
            pim.RiskDiagnosis |> mapLowRisk ]
          |> List.filter Option.isSome
          |> List.map Option.get
          |> function
          | [] -> 0.
          | xs -> xs |> List.max

          // pupils =
          pim.AdmissionPupils
          |> function
          | FixedDilated -> 3.0791
          | _ -> 0.
          // vent
          if pim.Ventilated then 1.3352 else 0.
          // SBP
          (((pim.SystolicBloodPressure
             |> Option.defaultValue 120.)
            - 120.)
           |> Math.Abs)
          * 0.01395
          // be
          (pim.BaseExcess
           |> Option.defaultValue 0.
           |> Math.Abs)
          * 0.1040
          // fiO2 =
          if paO2 > 0. then
              (((pim.FiO2 |> Option.defaultValue 0.) * 100.)
               / paO2)
              * 0.2888
          else
              0.

          -4.8841 ]
        |> List.mapi (fun i v ->
            printfn "%i %f" i v
            v)
        |> List.reduce (+)
        |> fun score ->
            printfn "PIM 2 score: %f" score
            calcRiskFromScore score

    // PIM3 score =
    //  (3.8233 × pupillary reaction) +
    //  (−0.5378 × elective admission) +
    //  (0.9763 × mechanical ventilation) +
    //  (0.0671 × [absolute {base excess}]) +
    //  (−0.0431 × SBP) + (0.1716 × [SBP^2/1,000]) +
    //  (0.4214 × [{FiO2 × 100}/PaO2]) +
    //  (-1.2246 × bypass cardiac procedure) +
    //  (-0.8762 × non-bypass cardiac procedure) +
    //  (-1.5164 × noncardiac procedure) +
    //  (1.6225 × very high-risk diagnosis) +
    //  (1.0725 × high-risk diagnosis)
    //  (-2.1766 × low-risk diagnosis) +
    //  −1.7928
    let calculatePIM3 (pim: PIM) =
        let mapVeryHighRisk rd =
            pim3VeryHighRisk
            |> List.exists (fun d -> rd |> List.exists ((=) d))
            |> fun b -> if b then 1.6225 |> Some else None

        let mapHighRisk rd =
            pim3HighRisk
            |> List.exists (fun d -> rd |> List.exists ((=) d))
            |> fun b -> if b then 1.0725 |> Some else None

        let mapLowRisk rd =
            pim3LowRisk
            |> List.exists (fun d -> rd |> List.exists ((=) d))
            |> fun b -> if b then -2.1766 |> Some else None

        let paO2 = pim.PaO2 |> Option.defaultValue 0.

        let sbp =
            pim.SystolicBloodPressure
            |> Option.defaultValue 120.

        [ if pim.Urgency = Elective then -0.5378 else 0.
          // recovery
          if pim.Recovery && (not pim.CardiacByPass) then -0.8762 else 0.
          // bypass
          if pim.CardiacByPass then -1.2246 else 0.
          // no bypass
          if pim.CardiacNonByPass then -0.8762 else 0.
          // non cardiac
          if pim.NonCardiacProcedure then -1.5164 else 0.

          // take the max risk diagnosis
          [ pim.RiskDiagnosis |> mapVeryHighRisk
            // highRisc
            pim.RiskDiagnosis |> mapHighRisk
            // lowRisc
            pim.RiskDiagnosis |> mapLowRisk ]
          |> List.filter Option.isSome
          |> List.map Option.get
          |> function
          | [] -> 0.
          | xs -> xs |> List.max

          // pupils =
          pim.AdmissionPupils
          |> function
          | FixedDilated -> 3.8233
          | _ -> 0.
          // vent
          if pim.Ventilated then 0.9763 else 0.
          // SBP
          (-0.0431 * sbp) + (0.1716 * ((sbp ** 2.) / 1000.))
          // be
          (pim.BaseExcess
           |> Option.defaultValue 0.
           |> Math.Abs)
          * 0.0671
          // fiO2 =
          if paO2 > 0. then
              (((pim.FiO2 |> Option.defaultValue 0.) * 100.)
               / paO2)
              * 0.4214
          else
              0.

          -1.7928 ]
        |> List.mapi (fun i v ->
            printfn "%i %f" i v
            v)
        |> List.reduce (+)
        |> fun score ->
            printfn "PIM 3 score: %f" score
            calcRiskFromScore score




    let riskDiagnoses =
        [
            "None"
            "Asthma"
            "Bone Marrow Transplant"
            "Bronchiolitis"
            "Cardiac Arrest"
            "Cardiomyopathy Or Myocarditis"
            "Cerebral Hemorrhage"
            "Croup"
            "Diabetic Ketoacidosis"
            "HIV Positive"
            "Hypoplastic Left Heart Syndrome"
            "Leukemia Or Lymphoma"
            "Liver Failure"
            "Necrotizing Enterocolitis"
            "Neurodegenerative Disorder"
            "Obstructive Sleep Apnea"
            "Seizure Disorder"
            "Severe CombinedImmune Deficiency"
        ]

    let diagnosisMapping =
        [
            Asthma , riskDiagnoses.[1]
            BoneMarrowTransplant , riskDiagnoses.[2]
            Bronchiolitis , riskDiagnoses.[3]
            CardiacArrest , riskDiagnoses.[4]
            CardiomyopathyOrMyocarditis , riskDiagnoses.[5]
            CerebralHemorrhage , riskDiagnoses.[6]
            Croup , riskDiagnoses.[7]
            DiabeticKetoacidosis , riskDiagnoses.[8]
            HIVPositive , riskDiagnoses.[9]
            HypoplasticLeftHeartSyndrome , riskDiagnoses.[10]
            LeukemiaorLymphoma , riskDiagnoses.[11]
            LiverFailure , riskDiagnoses.[12]
            NecrotizingEnterocolitis , riskDiagnoses.[13]
            NeurodegenerativeDisorder , riskDiagnoses.[14]
            ObstructiveSleepApnea , riskDiagnoses.[15]
            SeizureDisorder , riskDiagnoses.[16]
            SevereCombinedImmuneDeficiency , riskDiagnoses.[17]
        ]


    let riskDiagnosesToString = function
        | [d] ->
            diagnosisMapping
            |> List.tryFind (fst >> ((=) d))
            |> function
            | Some (_, s) -> s
            | None -> ""
        | _ -> ""


    let stringToRiskDiagnoses s =
        diagnosisMapping
        |> List.tryFind (snd >> ((=) s))
        |> function
        | Some (d, _) -> [ d ]
        | None -> []


    let riskDiagnosesToIndex d =
        riskDiagnoses
        |> List.tryFindIndex ((=) (d |> riskDiagnosesToString))
        |> function
        | Some i -> i
        | None   -> 0
