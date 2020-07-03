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
                style.minWidth (theme.spacing 2)
                style.marginTop (theme.spacing 1)
            ]

            input = styles.create [
                style.color (theme.palette.primary.main)
            ]

            label = styles.create [
                style.fontSize (theme.typography.fontSize)
            ]
        |}
    )


    type Props =
        {|
            min : float option
            max : float option
            step : float
            label : string
            adorn : string
            dispatch : string -> unit
        |}


    let defaultProps =
        {|
            min = None
            max = None
            step = 1.
            label = ""
            adorn = ""
            dispatch = (fun (s: string) -> ())
        |}


    let private comp =
        React.functionComponent ("numericinput", fun (props : Props) ->
            let classes = useStyles ()

            Mui.textField [
                prop.className classes.field

                textField.label props.label

                textField.InputLabelProps [
                   prop.className classes.label
                ]

                textField.type' "number"
                textField.onChange props.dispatch
                textField.size.small
                textField.InputProps [
                    input.inputProps [
                        prop.step props.step
                        match props.min with
                        | Some m -> prop.min m
                        | None   -> prop.min 0.
                        match props.max with
                        | Some m -> prop.max m
                        | None   -> ()
                    ]

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


    let render label adorn dispatch =
        comp ({| defaultProps with
                    label = label
                    adorn = adorn
                    dispatch = dispatch |})


    let renderWithProps props = comp(props)
