namespace Pages

module PIM =

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


    type PIM = PIM.PIM


    type State = { PIM: PIM }


    type Msg =
        | ElectiveAdmission of bool
        | Recovery of bool
        | Procedure of string
        | PupillaryResponse of bool
        | MechanicalVentilation of bool
        | RiskDiagnoses of string
        | SystolicBloodPressure of string
        | BaseExcess of string
        | FiO2 of string
        | PaO2 of string


    let init () : State * Cmd<Msg> =
        let initialModel = { PIM = PIM.pim }
        initialModel, Cmd.none


    let update (msg : Msg) (model : State) : State * Cmd<Msg> =
        match msg with
        | ElectiveAdmission b ->
            { model with
                PIM = {
                    model.PIM with
                        Urgency = if b then PIM.Elective else PIM.NotElective
                }
            }, Cmd.none
        | Recovery b ->
            { model with
                PIM = {
                    model.PIM with
                        Recovery = b
                }
            }, Cmd.none
        | Procedure s ->
            printfn "switch to procedure: %s" s
            match s with
            | _ when s = "CardiacByPass" ->
                { model with
                    PIM = {
                        model.PIM with
                            CardiacByPass = true
                            CardiacNonByPass = false
                            NonCardiacProcedure = false

                    }
                }, Cmd.none
            | _ when s = "CardiacNonByPass" ->
                { model with
                    PIM = {
                        model.PIM with
                            CardiacByPass = false
                            CardiacNonByPass = true
                            NonCardiacProcedure = false

                    }
                }, Cmd.none
            | _ when s = "NonCardiacProcedure" ->
                { model with
                    PIM = {
                        model.PIM with
                            CardiacByPass = false
                            CardiacNonByPass = false
                            NonCardiacProcedure = true

                    }
                }, Cmd.none
            | _  ->
                { model with
                    PIM = {
                        model.PIM with
                            CardiacByPass = false
                            CardiacNonByPass = false
                            NonCardiacProcedure = false

                    }
                }, Cmd.none
        | PupillaryResponse b ->
            { model with
                PIM = {
                    model.PIM with
                        AdmissionPupils = if b then PIM.FixedDilated else PIM.NormalPupils
                }
            }, Cmd.none
        | MechanicalVentilation b ->
            { model with
                PIM = {
                    model.PIM with
                        Ventilated = b
                }
            }, Cmd.none
        | RiskDiagnoses d ->
                { model with
                    PIM = {
                        model.PIM with
                            RiskDiagnosis = d |> PIM.stringToRiskDiagnoses
                    }
                }, Cmd.none

        | SystolicBloodPressure s ->
            { model  with
                PIM = {
                    model.PIM with
                        SystolicBloodPressure = s |> tryParseFloat
                }
            }, Cmd.none

        | BaseExcess s ->
            { model  with
                PIM = {
                    model.PIM with
                        BaseExcess = s |> tryParseFloat
                }
            }, Cmd.none

        | FiO2 s ->
            { model  with
                PIM = {
                    model.PIM with
                        FiO2 =
                            s
                            |> tryParseFloat
                            |> Option.bind (fun v ->
                                    match v with
                                    | _ when v < 21.  -> 0.21
                                    | _ when v > 100. -> 1.0
                                    | _ -> v / 100.
                                    |> Some
                                )
                }
            }, Cmd.none

        | PaO2 s ->
            { model  with
                PIM = {
                    model.PIM with
                        PaO2 = s |> tryParseFloat
                }
            }, Cmd.none


    let checkBoxes =
        [
            ElectiveAdmission, "Elective Admission"
            Recovery, "Recovery Post-Procedure"
            PupillaryResponse, "Lack of pupillary response"
            MechanicalVentilation, "Mechanical Ventilation"
        ]


    let createCheckBox (dispatch : Msg -> unit) (lbl : string) (msg : bool -> Msg) =
        Mui.formControlLabel [
            formControlLabel.label lbl
            formControlLabel.control (Mui.checkbox [
                prop.onChange (msg >> dispatch)
            ])
        ]



    let createTextField (dispatch : Msg -> unit)  (lbl : string) (adorn : string)  (msg: string -> Msg) =
        Mui.textField [
            prop.style [
                style.marginTop 20

            ]
            textField.label lbl
            textField.type' "number"
            textField.onChange (msg >> dispatch)
            textField.InputProps [
                input.endAdornment (
                    Mui.inputAdornment [
                        inputAdornment.position.end'
                        inputAdornment.children adorn
                    ]
                )
            ]
        ]


    let textFields dispatch =
        [
            {| label = "First Systolic BloodPressure"; adorn =  "mmHg"; dispatch =  SystolicBloodPressure >> dispatch |}
            {| label = "Base Excess (artillary or capillary)"; adorn =  "mEg/L or mmol/L"; dispatch =  BaseExcess >> dispatch |}
            {| label = "FiO2 during first ABG"; adorn =  "% O2"; dispatch =  FiO2 >> dispatch |}
            {| label = "PaO2 during first ABG"; adorn =  "mmHg"; dispatch =  PaO2 >> dispatch |}
        ]
        |> List.map Components.NumericInput.render



    let printPIMMortality (className: string) (model : State) =
        Html.div [
            prop.className className
            prop.children [
                Mui.typography [
                    let s = model.PIM |> PIM.calculatePIM2
                    typography.variant.h6
                    (sprintf "PIM2 estimated mortality: %.2f %%" (s * 100.)) |> prop.text
                ]
                Mui.typography [
                    let s = model.PIM |> PIM.calculatePIM3
                    typography.variant.h6
                    (sprintf "PIM3 estimated mortality: %.2f %%" (s * 100.)) |> prop.text
                ]

            ]
        ]


    let createRadioButtons (dispatch : Msg -> unit) (msg : string -> Msg) =
        let buttons = [
            {| value = "NoProcedure";         label = "No Procedure/Unknown" |}
            {| value = "CardiacBypass";       label = "Cardiac Bypass Procedure" |}
            {| value = "CardiacNonBypass";    label = "Cardiac Procedure Without Bypass" |}
            {| value = "NonCardiacProcedure"; label = "Non Cardiac Procedure" |}
        ]

        Components.RadioButtons.render ({| title = "Procedure"; buttons = buttons; dispatch = (msg >> dispatch) |})


    let useStyles = Styles.makeStyles(fun styles theme ->
        {|
            print = styles.create [
                style.marginBottom (theme.spacing 2)
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
        React.functionComponent("pim", fun () ->
            let model, dispatch = React.useElmish(init, update, [||])

            let classes = useStyles ()

            let createCheckBox = createCheckBox dispatch
            let textFields = textFields dispatch
            let radiobuttons = createRadioButtons dispatch Procedure


            Html.form [
                prop.className classes.form

                prop.children [
                    model |> printPIMMortality classes.print

                    Mui.formGroup [

                        formGroup.children [
                            for (c, lbl) in checkBoxes do
                                createCheckBox lbl c

                            radiobuttons
                            // This is to push the formControl a bit down
                            Html.div [ prop.className classes.div ]

                            {|
                                value = model.PIM.RiskDiagnosis |> PIM.riskDiagnosesToString
                                items = PIM.riskDiagnoses
                                label = "Risk Diagnosis"
                                dispatch = RiskDiagnoses >> dispatch
                            |}
                            |> Components.ComboBox.render

                            // list the textfields
                            for e in textFields do e

                        ]
                    ]

                ]

            ]
        )

    let render = comp