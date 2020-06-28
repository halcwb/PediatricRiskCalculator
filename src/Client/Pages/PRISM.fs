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


    type PRISM = PRISM.Input


    type State = { PRISM: PRISM }


    type Msg =
        | BirthDate of DateTime option
        | SystolicBloodPressure of string


    let init () : State * Cmd<Msg> =
        let initialModel = { PRISM = PRISM.input }
        initialModel, Cmd.none


    let update (msg : Msg) (state : State) : State * Cmd<Msg> =
        match msg with
        | BirthDate dt ->
            printfn "received birthdate %A" dt
            state, Cmd.none
        | SystolicBloodPressure s ->
            { state with
                PRISM = {
                    state.PRISM with
                        SystolicBloodPressure =
                            s
                            |> tryParseFloat
                            |> function
                            | Some v -> v |> PRISM.OneValue
                            | None   -> PRISM.NoValue
                }
            }, Cmd.none


    let textFields dispatch =
        [
            {| label = "Systolic BloodPressure"; adorn =  "mmHg"; dispatch =  SystolicBloodPressure >> dispatch |}
            {| label = "Temperature"; adorn =  "Celsius"; dispatch =  SystolicBloodPressure >> dispatch |}
            {| label = "HeartRate"; adorn =  "/min"; dispatch =  SystolicBloodPressure >> dispatch |}
            {| label = "pH"; adorn =  ""; dispatch =  SystolicBloodPressure >> dispatch |}
            {| label = "Total CO2"; adorn =  "mmHg"; dispatch =  SystolicBloodPressure >> dispatch |}
            {| label = "PCO2"; adorn =  "mmHg"; dispatch =  SystolicBloodPressure >> dispatch |}
            {| label = "PaO2"; adorn =  "mmHg"; dispatch =  SystolicBloodPressure >> dispatch |}
            {| label = "Glucose"; adorn =  "mmol/L"; dispatch =  SystolicBloodPressure >> dispatch |}
            {| label = "Potassium"; adorn =  "mmol/L"; dispatch =  SystolicBloodPressure >> dispatch |}
            {| label = "Urea"; adorn =  "mmol/L"; dispatch =  SystolicBloodPressure >> dispatch |}
            {| label = "White Blood Count"; adorn =  "10^9/L"; dispatch =  SystolicBloodPressure >> dispatch |}
            {| label = "PT"; adorn =  "s"; dispatch =  SystolicBloodPressure >> dispatch |}
            {| label = "APTT"; adorn =  "s"; dispatch =  SystolicBloodPressure >> dispatch |}
            {| label = "Platelets"; adorn =  "mmHg"; dispatch =  SystolicBloodPressure >> dispatch |}
        ]
        |> List.map Components.NumericInput.render



    let printCalculation (className: string) (model : State) =
        Html.div [
            prop.className className
            prop.children [
                Mui.typography [
                    let s =
                        model.PRISM
                        |> PRISM.calculate
                        |> function
                        | Some v -> (sprintf "PRISM estimated mortality: %.2f %%" (v * 100.))
                        | None   -> "Cannot calculate PRISM"
                    typography.variant.h6
                    prop.text s
                ]
            ]
        ]


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
        React.functionComponent("prism", fun () ->
            let model, dispatch = React.useElmish(init, update, [||])

            let classes = useStyles ()

            let textFields = textFields dispatch


            Html.form [
                prop.className classes.form

                prop.children [
                    model |> printCalculation classes.print

                    Mui.formGroup [

                        formGroup.children [

                            Components.DatePicker.render "Birth Date" (BirthDate >> dispatch)

                            // list the textfields
                            for e in textFields do e

                        ]
                    ]

                ]

            ]
        )

    let render = comp