

module Patient =

    open System

    type Value = NoValue | OneValue of float | TwoValues of float * float

    type AdmissionSource =
        | Recovery
        | AnotherHospital
        | InHospital
        | EmergencyUnit
        | UnknownAdmissionSource

    type AdmissionUrgency = | Elective | NotElective | UnknownUrgency

    type PupilResponse = TwoFixedDilated | OneFixedDilated | NormalPupils | UnknownPupils

    type RiskDiagnosis =
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


    type Patient =
        {
            Age : DateTime option
            AdmissionSource : AdmissionSource
            Urgency : AdmissionUrgency
            CPR24HourBefore : bool
            CardiacByPass : bool
            Ventilated : bool
            Cancer : bool
            RiskDiagnosis : RiskDiagnosis list
            LowRiskPrimary : bool //Endocrine, hematologic, musculoskeletal, and renal systems of primary dysfunction are defined as ‘low risk.’
            SystolicBloodPressure : float option
            HeartRate : int option
            TemperatureHigh : float option
            TemparatureLow : float option
            MentalStatus : int option
            AdmissionPupils : PupilResponse
            PHLow : float option
            PHHigh : float option
            TotalCO2High : float option
            TotalCO2Low : float option
            PCO2 : float option
            PaO2 : float option
            FiO2 : float option
            BaseExcess : float option
            Glucose : float option
            Potassium : float option
            Creatinine : float option
            Urea : float option
            WhiteBloodCount : float option
            PT : float option
            PTT : float option
            Platelets : float option
        }

    let patient =
        {
            Age = None
            AdmissionSource = UnknownAdmissionSource
            Urgency = UnknownUrgency
            CPR24HourBefore = false
            CardiacByPass = false
            Ventilated = false
            Cancer = false
            RiskDiagnosis = []
            LowRiskPrimary = false //Endocrine, hematologic, musculoskeletal, and renal systems of primary dysfunction are defined as ‘low risk.’
            SystolicBloodPressure = None
            HeartRate = None
            TemperatureHigh = None
            TemparatureLow = None
            MentalStatus = None
            AdmissionPupils = UnknownPupils
            PHLow = None
            PHHigh = None
            TotalCO2High = None
            TotalCO2Low = None
            PCO2 = None
            PaO2 = None
            FiO2 = None
            BaseExcess = None
            Glucose = None
            Potassium = None
            Creatinine = None
            Urea = None
            WhiteBloodCount = None
            PT = None
            PTT = None
            Platelets = None
        }
