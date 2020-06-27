module Client

open System

open Elmish
open Elmish.React
open Fable.React
open Fable.React.Props
open Fetch.Types
open Thoth.Fetch
open Thoth.Json
open Feliz
open Feliz.MaterialUI
open Fable.MaterialUI.Icons
open Fable.Core.JsInterop
open Shared
open Utils

type PIM = PIM.PIM


// The model holds data that you want to keep track of while the application is running
// in this case, we are keeping track of a counter
// we mark it as optional, because initially it will not be available from the client
// the initial value will be requested from server
type Model = { PIM: PIM }

// The Msg type defines what events/actions can occur while the application is running
// the state of the application changes *only* in reaction to these events
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


// defines the initial state and initial command (= side-effect) of the application
let init () : Model * Cmd<Msg> =
    let initialModel = { PIM = PIM.pim }
    initialModel, Cmd.none

// The update function computes the next state of the application based on the current state and the incoming events/messages
// It can also run side-effects (encoded as commands) like calling the server via Http.
// these commands in turn, can dispatch messages to which the update function will react.
let update (msg : Msg) (model : Model) : Model * Cmd<Msg> =
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
                        |> Option.bind (fun v -> v / 100. |> Some)
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


let createMenuItem (i : int) (txt : string) =
    Mui.menuItem [
        prop.value i
        prop.text txt
    ]


let riskDiagnosisMenuItems =
    PIM.riskDiagnoses
    |> List.mapi createMenuItem


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
        "First Systolic BloodPressure", "mmHg", SystolicBloodPressure
        "Base Excess (artillary or capillary)", "mEg/L or mmol/L", BaseExcess
        "FiO2 during first ABG", "% O2", FiO2
        "PaO2 during first ABG", "mmHg", PaO2
    ]
    |> List.map (fun (lbl, adorn, msg) -> createTextField dispatch lbl adorn msg)


let printPIMMortality (model : Model) =
    Html.div [
        prop.style [
            style.marginBottom 20
            style.paddingTop 20
        ]
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
    Mui.formControl [
        formControl.component' "fieldset"
        prop.style [
            style.marginTop 20
            style.marginBottom 20
        ]
        formControl.children [
            Mui.formLabel [
                formLabel.component' "legend"
                prop.text "Procedure"
            ]
            Mui.radioGroup [
//                radioGroup.value "NoProcedure"
                radioGroup.onChange (msg >> dispatch)
                radioGroup.children [
                    Mui.formControlLabel [
                        formControlLabel.control (Mui.radio [])
                        formControlLabel.value "NoProcedure"
                        formControlLabel.label "No Procedure/Unknown"
                    ]
                    Mui.formControlLabel [
                        formControlLabel.control (Mui.radio [])
                        formControlLabel.value "CardiacBypass"
                        formControlLabel.label "Cardiac Bypass Procedure"
                    ]
                    Mui.formControlLabel [
                        formControlLabel.control (Mui.radio [])
                        formControlLabel.value "CardiacNonBypass"
                        formControlLabel.label "Cardiac Procedure Without Bypass"
                    ]
                    Mui.formControlLabel [
                        formControlLabel.control (Mui.radio [])
                        formControlLabel.value "NonCardiacProcedure"
                        formControlLabel.label "Non Cardiac Procedure"
                    ]
                ]

            ]
        ]
    ]

let view (model : Model) (dispatch : Msg -> unit) =
    let createCheckBox = createCheckBox dispatch
    let textFields = textFields dispatch
    let createRadioButtons = createRadioButtons dispatch Procedure

    Mui.container [
        container.component' "main"
        container.maxWidth.sm
        prop.children [
            Mui.appBar [
                prop.style [
                    style.display.flex
                    style.flexDirection.row
                    style.padding 10
                ]
                appBar.children [
                    Mui.typography [
                        typography.variant.h6
                        prop.text "Informedica Pediatric Risk Calculator"
                    ]
                ]
            ]

            Html.form [
                prop.style [
                    style.marginTop 80
                ]

                prop.children [
                    model |> printPIMMortality

                    Mui.formGroup [
                        prop.style [
                            style.paddingBottom 10
                        ]

                        formGroup.children [
                            for (c, lbl) in checkBoxes do
                                createCheckBox lbl c

                            createRadioButtons
                            // This is to push the formControl a bit down
                            Html.div [ prop.style [ style.marginBottom 20 ] ]

                            Mui.formControl [
                                Mui.inputLabel "Risk Diagnosis"
                                Mui.select [
                                    select.value (model.PIM.RiskDiagnosis |> PIM.riskDiagnosesToIndex)
                                    select.onChange (fun e ->
                                        PIM.riskDiagnoses.[e]
                                        |> RiskDiagnoses
                                        |> dispatch
                                     )
                                    prop.children [
                                        for mi in riskDiagnosisMenuItems do mi
                                    ]
                                ]
                            ]

                            for f in textFields do f

                        ]
                    ]

                ]

            ]
        ]
    ]


#if DEBUG
open Elmish.Debug
open Elmish.HMR
#endif

Program.mkProgram init update view
#if DEBUG
|> Program.withConsoleTrace
#endif
|> Program.withReactBatched "elmish-app"
#if DEBUG
|> Program.withDebugger
#endif
|> Program.run
