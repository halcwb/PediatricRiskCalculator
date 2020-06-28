namespace Components


module NumericInput =
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


    let useStyles = Styles.makeStyles(fun styles theme ->
        {|
            field = styles.create [
                style.marginTop (theme.spacing 1)
            ]

            input = styles.create [
                style.color (theme.palette.primary.main)
            ]
        |}
    )

    let private comp =
        React.functionComponent ("numericinput", fun (props : {| label : string; adorn : string; dispatch : string -> unit |}) ->
            let classes = useStyles ()

            Mui.textField [
                prop.className classes.field

                textField.label props.label
                textField.type' "number"
                textField.onChange props.dispatch
                textField.InputProps [
                    // sets the color of the input value
                    prop.className classes.input
                    // adds a unit (adornment) to a value
                    input.endAdornment (
                        Mui.inputAdornment [
                            inputAdornment.position.end'
                            inputAdornment.children props.adorn
                        ]
                    )
                ]
            ]
        )


    let render props = comp props
