namespace Components


module NumericLowHighInput =

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


    let useStyles = Styles.makeStyles (fun styles theme ->
        {|
            grid = styles.create [
                style.padding (theme.spacing 1)
            ]
        |}
    )

    let private comp =
        React.functionComponent("numeric-lowhigh-input", fun (props: {| label : string; adorn : string ; dispatch : string -> unit; low : bool; high: bool  |}) ->
            let classes = useStyles()

            let input =
                match props.low, props.high with
                | true, false ->
                    {|
                        label = "Low"
                        adorn = props.adorn
                        dispatch = props.dispatch
                    |}
                    |> Components.NumericInput.render, Html.none

            Mui.table [
                Mui.tableBody [
                    Mui.tableCell [
                        Mui.typography [ prop.text props.label ]
                    ]

                    Mui.tableCell [ input |> fst ]
                    Mui.tableCell [ input |> snd ]
                ]
            ]
        )


    let render props = comp(props)