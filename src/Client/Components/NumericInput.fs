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
    open Thoth.Elmish
    open Feliz
    open Feliz.UseElmish
    open Feliz.MaterialUI
    open Fable.MaterialUI.Icons
    open Fable.Core.JsInterop


    /// The debouncer keeps tracks of the bounce time
    /// before the UserInput is actually used.
    type State =
        {
            Debouncer : Debouncer.State
            UserInput : string
        }


    type Msg =
        | DebouncerSelfMsg of Debouncer.SelfMessage<Msg>
        | ChangeValue of string
        | EndOfInput


    let private init () =
        {
            Debouncer = Debouncer.create()
            UserInput = ""
        }, Cmd.none


    let private update dispatch msg state =
        match msg with
        | ChangeValue s ->
            let debouncerModel, debouncerCmd =
                state.Debouncer
                |> Debouncer.bounce (TimeSpan.FromSeconds 0.5) "endofinput" EndOfInput
            { state with
                UserInput = s
                Debouncer = debouncerModel }
            , Cmd.map DebouncerSelfMsg debouncerCmd

        | DebouncerSelfMsg debouncerMsg ->
            let debouncerModel, debouncerCmd = Debouncer.update debouncerMsg state.Debouncer
            { state with Debouncer = debouncerModel }, debouncerCmd
        // End of user input has reached so know actually dispatch the
        // input to the dispatch function
        | EndOfInput ->
            state, Cmd.ofSub (fun _ -> state.UserInput |> dispatch)


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
            let _, dispatch = React.useElmish(init, update props.dispatch, [||])
            let classes = useStyles ()

            Mui.textField [
                prop.className classes.field

                textField.label props.label

                textField.InputLabelProps [
                   prop.className classes.label
                ]

                textField.type' "number"
                textField.onChange (ChangeValue >> dispatch)
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
