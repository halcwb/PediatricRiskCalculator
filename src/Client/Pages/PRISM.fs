namespace Pages

module PRISM =

    open System

    open Elmish
    open Elmish.React
    open Fable.React
    open Fable.React.Props
    open Fetch.Types
    open Thoth.Fetch
    open Thoth.Json
    open Feliz
    open Feliz.UseElmish
    open Feliz.MaterialUI
    open Fable.MaterialUI.Icons
    open Fable.Core.JsInterop
    open Shared
    open Utils


    let parseValue s =
        s
        |> tryParseFloat


    let sourceMapping =
        [
            "unknown", PRISM.UnknownAdmissionSource
            "emergency unit", PRISM.EmergencyUnit
            "another hospital", PRISM.AnotherHospital
            "in hospital", PRISM.InHospital
            "recovery", PRISM.Recovery
        ]


    type PRISM = PRISM.Input


    type State = PRISM


    type Msg =
        | BirthDate of DateTime option
        | BloodPressureLow of string
        | TemperatureLow of string
        | TemperatureHigh of string
        | MentalStatus of string
        | HeartRateHigh of string
        | CreatinineHigh of string
        | UreaHigh of string
        | ProthPTHigh of string
        | ProthPTTHigh of string
        | Pupils of string
        | PhLow of string
        | PhHigh of string
        | TotalCO2Low of string
        | TotalCO2High of string
        | PCO2High of string
        | PaO2Low of string
        | GlucoseHigh of string
        | PotassiumHigh of string
        | WBCLow of string
        | PlateletsLow of string
        | AdmissionSource of string
        | CPR24HourBefore of bool
        | Cancer of bool
        | LowRisk of bool


    let init () : State * Cmd<Msg> =
        let initialModel = PRISM.input
        initialModel, Cmd.none


    let update (msg : Msg) (state : State) : State * Cmd<Msg> =
        let parseOne s =
                s
                |> tryParseFloat
                |> function
                | Some v -> v |> PRISM.OneValue
                | None   -> PRISM.NoValue

        let parseTwoFst v2 s =
            s
            |> tryParseFloat
            |> function
            | Some v1 -> (v1, v2) |> PRISM.TwoValues
            | None   -> PRISM.NoValue

        let parseTwoSnd v1 s =
            s
            |> tryParseFloat
            |> function
            | Some v2 -> (v1, v2) |> PRISM.TwoValues
            | None   -> PRISM.NoValue

        match msg with
        | BirthDate dt ->
            printfn "received birthdate %A" dt
            { state with
                Age = dt
            }
            , Cmd.none

        | MentalStatus s ->
            { state with
                MentalStatus = s |> parseOne
            }, Cmd.none

        | Pupils s ->
            { state with
                PupilsFixed = s |> parseOne
            }, Cmd.none

        | BloodPressureLow s ->
            { state with
                SystolicBloodPressure = s |> parseOne
            }, Cmd.none

        | TemperatureLow s ->
            { state with
                Temperature =
                    match state.Temperature with
                    | PRISM.NoValue
                    | PRISM.OneValue _        -> s |> parseTwoFst 0.
                    | PRISM.TwoValues (_, v2) -> s |> parseTwoFst v2
            }, Cmd.none

        | TemperatureHigh s ->
            { state with
                Temperature =
                    match state.Temperature with
                    | PRISM.NoValue
                    | PRISM.OneValue _        -> s |> parseTwoSnd 0.
                    | PRISM.TwoValues (v1, _) -> s |> parseTwoSnd v1
            }, Cmd.none

        | HeartRateHigh s ->
            { state with
                HeartRate = s |> parseOne
            }, Cmd.none

        | PhLow s ->
            { state with
                PH =
                    match state.PH with
                    | PRISM.NoValue
                    | PRISM.OneValue _        -> s |> parseTwoFst 0.
                    | PRISM.TwoValues (_, v2) -> s |> parseTwoFst v2
            }, Cmd.none

        | PhHigh s ->
            { state with
                PH =
                    match state.PH with
                    | PRISM.NoValue
                    | PRISM.OneValue _        -> s |> parseTwoSnd 0.
                    | PRISM.TwoValues (v1, _) -> s |> parseTwoSnd v1
            }, Cmd.none

        | TotalCO2Low s ->
            { state with
                TotalCO2 =
                    match state.TotalCO2 with
                    | PRISM.NoValue
                    | PRISM.OneValue _        -> s |> parseTwoFst 0.
                    | PRISM.TwoValues (_, v2) -> s |> parseTwoFst v2
            }, Cmd.none

        | TotalCO2High s ->
            { state with
                TotalCO2 =
                    match state.TotalCO2 with
                    | PRISM.NoValue
                    | PRISM.OneValue _        -> s |> parseTwoSnd 0.
                    | PRISM.TwoValues (v1, _) -> s |> parseTwoSnd v1
            }, Cmd.none

        | PCO2High s ->
            { state with
                PCO2 = s |> parseOne
            }, Cmd.none

        | PaO2Low s ->
            { state with
                PaO2 = s |> parseOne
            }, Cmd.none

        | GlucoseHigh s ->
            { state with
                Glucose = s |> parseOne
            }, Cmd.none

        | PotassiumHigh s ->
            { state with
                Potassium = s |> parseOne
            }, Cmd.none

        | CreatinineHigh s ->
            { state with
                Creatinine = s |> parseOne
            }, Cmd.none

        | UreaHigh s ->
            { state with
                Urea = s |> parseOne
            }, Cmd.none

        | WBCLow s ->
            { state with
                WhiteBloodCount = s |> parseOne
            }, Cmd.none

        | ProthPTHigh s ->
            { state with
                PT = s |> parseOne
            }, Cmd.none

        | ProthPTTHigh s ->
            { state with
                PTT = s |> parseOne
            }, Cmd.none

        | PlateletsLow s ->
            { state with
                Platelets = s |> parseOne
            }, Cmd.none

        | AdmissionSource s ->
            { state with
                AdmissionSource =
                    match sourceMapping |> List.tryFind (fst >> (=) s) with
                    | Some (_, m) -> m
                    | None        -> state.AdmissionSource
            }, Cmd.none

        | CPR24HourBefore b ->
            {
                state with CPR24HourBefore = b
            }, Cmd.none

        | Cancer b ->
            {
                state with Cancer = b
            }, Cmd.none

        | LowRisk b ->
            {
                state with LowRiskPrimary = b
            }, Cmd.none



    let createCheckBox  (lbl : string) (dispatch : bool -> unit) =
        Mui.formControlLabel [
            formControlLabel.label lbl
            formControlLabel.control (Mui.checkbox [
                prop.onChange dispatch
            ])
        ]



    let textFields (state: State) dispatch =
        let props  = Components.NumericInput.defaultProps
        let render = Components.NumericInput.renderWithProps

        let tempHighMin =
            match state.Temperature with
            | PRISM.TwoValues(v1, _) -> Some v1
            | _ -> Some 30.

        let tempLowMax =
            match state.Temperature with
            | PRISM.TwoValues(_, v2) when v2 > 0. -> Some v2
            | _ -> Some 42.

        let phHighMin =
            match state.PH with
            | PRISM.TwoValues(v1, _) -> Some v1
            | _ -> Some 6.8

        let phLowMax =
            match state.PH with
            | PRISM.TwoValues(_, v2) when v2 > 0. -> Some v2
            | _ -> Some 7.6

        let totalCO2HighMin =
            match state.TotalCO2 with
            | PRISM.TwoValues(v1, _) -> Some v1
            | _ -> Some 0.

        let totalCO2LowMax =
            match state.TotalCO2 with
            | PRISM.TwoValues(_, v2) when v2 > 0. -> Some v2
            | _ -> Some 40.

        [
            "Systolic RR"
            , {| props with max = Some 300.; label = "Low"; adorn =  "mmHg"; dispatch =  BloodPressureLow >> dispatch |} |> render
            , Html.none

            "Temperature"
            , {| props with min = Some 30.; max = tempLowMax; step = 0.1; label = "Low"; adorn =  "°C"; dispatch =  TemperatureLow >> dispatch |} |> render
            , {| props with min = tempHighMin; max = Some 42.; step = 0.1; label = "High"; adorn =  "°C"; dispatch =  TemperatureHigh >> dispatch |} |> render

            "HeartRate"
            , {| props with min = Some 30.; max = Some 220.; label = "High"; adorn =  "beats/min"; dispatch =  HeartRateHigh >> dispatch |} |> render
            , Html.none

            "pH"
            , {| props with min = Some 6.8; max = phLowMax; step = 0.01; label = "Low"; adorn =  ""; dispatch =  PhLow   >> dispatch |} |> render
            , {| props with min = phHighMin; max = Some 7.6; step = 0.01; label = "High"; adorn =  ""; dispatch =  PhHigh >> dispatch |} |> render

            "Bicarbonate"
            , {| props with max = totalCO2LowMax; label = "Low"; adorn =  "mmol/L"; dispatch =  TotalCO2Low >> dispatch|} |> render
            , {| props with min = totalCO2HighMin; max = Some 40.; label = "High"; adorn =  "mmol/L"; dispatch =  TotalCO2High >> dispatch|} |> render

            "PCO2"
            , {| props with min = Some 4.; max = Some 120.; label = "High"; adorn =  "mmHg"; dispatch =  PCO2High >> dispatch |} |> render
            , Html.none

            "PaO2"
            , {| props with min = Some 10.; max = Some 300.; label = "Low"; adorn =  "mmHg"; dispatch =  PaO2Low >> dispatch |} |> render
            , Html.none

            "Glucose"
            , {| props with min = Some 0.5; max = Some 100.; step = 0.1; label = "High"; adorn =  "mmol/L"; dispatch =  GlucoseHigh >> dispatch |} |> render
            , Html.none

            "Potassium"
            , {| props with min = Some 1.; max = Some 10.; step = 0.1;  label = "High"; adorn =  "mmol/L"; dispatch =  PotassiumHigh >> dispatch |} |> render
            , Html.none

            "Creatinine"
            , {| props with max = Some 300.; label = "High"; adorn =  "µmol/L"; dispatch =  CreatinineHigh >> dispatch |} |> render
            , Html.none

            "Urea"
            , {| props with max = Some 50.; label = "High"; adorn =  "mmol/L"; dispatch =  UreaHigh >> dispatch |} |> render
            , Html.none

            "White Blood Count"
            , {| props with max = Some 1000.; label = "Low"; adorn =  "10^9/L"; dispatch =  WBCLow >> dispatch |} |> render
            , Html.none

            "PT"
            , {| props with max = Some 50.; label = "High"; adorn =  "secs"; dispatch =  ProthPTHigh >> dispatch |} |> render
            , Html.none

            "APTT"
            , {| props with max = Some 100.; label = "High"; adorn =  "secs"; dispatch =  ProthPTTHigh >> dispatch |} |> render
            , Html.none

            "Platelets"
            , {| props with max = Some 1000.; label = "Low"; adorn =  "10^9/L"; dispatch =  PlateletsLow >> dispatch |} |> render
            , Html.none
        ]
        |> List.map (fun (t, c1, c2) ->
            Mui.tableRow [
                tableRow.children [
                    Mui.tableCell [
                        prop.style [
                            style.paddingRight 5
                            style.verticalAlign.bottom
                        ]
                        tableCell.children [
                            Mui.typography [
                                typography.variant.body2
                                prop.text t
                            ]
                        ]
                    ]
                    Mui.tableCell [ prop.style [ style.paddingRight 15 ]; tableCell.children [ c1 ] ]
                    Mui.tableCell [ prop.style []; tableCell.children [ c2 ] ]
                ]
            ]
        )


    let printCalculation (className: string) (state : State) =
        Html.div [
            prop.className className
            prop.children [
                Mui.typography [
                    let s =
                        state
                        |> PRISM.calculate
                        |> function
                        | Some v -> (sprintf "PRISM IV estimated mortality: %.2f %%" (v * 100.))
                        | None   -> "Cannot calculate PRISM IV"
                    typography.variant.h6
                    prop.text s
                ]
            ]
        ]


    let useStyles = Styles.makeStyles(fun styles theme ->
        {|
            print = styles.create [
                style.marginBottom (theme.spacing 1)
                style.paddingTop (theme.spacing 1)
                style.color (theme.palette.primary.dark)
            ]

            form = styles.create [
            ]

            div = styles.create [
                style.marginBottom (theme.spacing 1)
            ]
        |}
    )


    let private comp =
        React.functionComponent("prism", fun () ->
            let state, dispatch = React.useElmish(init, update, [||])

            let classes = useStyles ()

            let textFields = textFields state dispatch
            let checkboxes =
                [
                    "CPR 24 hours before", CPR24HourBefore >> dispatch
                    "Cancer", Cancer >> dispatch
                    "Low Risk Diagnosis", LowRisk >> dispatch
                ]
                |> List.map (fun (l, d) -> createCheckBox l d)

            Html.form [
                prop.className classes.form

                prop.children [
                    state |> printCalculation classes.print

                    Mui.formGroup [

                        formGroup.children [

                            Components.DatePicker.render "Birth Date" (BirthDate >> dispatch)

                            Html.div [ prop.className classes.div ]
                            Html.div [ prop.className classes.div ]

                            {|
                                dispatch = AdmissionSource >> dispatch
                                label = "Admission Source"
                                value = sourceMapping |> List.find (snd >> (=) state.AdmissionSource) |> fst
                                items = sourceMapping |> List.map fst
                            |}
                            |> Components.ComboBox.render

                            Html.div [ prop.className classes.div ]
                            Html.div [ prop.className classes.div ]

                            {|
                                dispatch = MentalStatus >> dispatch
                                label = "Mental Status (Glascow Coma Score)"
                                value =
                                    [ 15 .. -1 .. 3 ]
                                    |> List.map string
                                    |> List.tryFind (fun n ->
                                        match state.MentalStatus with
                                        | PRISM.OneValue v -> v |> int |> string = n
                                        | _ -> false
                                    )
                                    |> Option.defaultValue ""
                                items = [ 15 .. -1 .. 3 ] |> List.map string
                            |}
                            |> Components.ComboBox.render

                            Html.div [ prop.className classes.div ]
                            Html.div [ prop.className classes.div ]

                            {|
                                dispatch = Pupils >> dispatch
                                label = "Fixed pupils (> 3 mm), 0, 1 or 2"
                                value =
                                    [ 0 .. 2 ]
                                    |> List.map string
                                    |> List.tryFind (fun n ->
                                        match state.PupilsFixed with
                                        | PRISM.OneValue v -> v |> int |> string = n
                                        | _ -> false
                                    )
                                    |> Option.defaultValue ""
                                items = [ 0 .. 2 ] |> List.map string
                            |}
                            |> Components.ComboBox.render

                            Html.div [ prop.className classes.div ]

                            for el in checkboxes do el

                            Mui.table [
                                table.padding.none
                                table.children [ Mui.tableBody textFields ]
                            ]


                        ]
                    ]

                ]

            ]
        )

    let render = comp