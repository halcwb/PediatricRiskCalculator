namespace Components

module DatePicker =

    open Elmish
    open Feliz
    open Feliz.UseElmish
    open Feliz.MaterialUI
    open Feliz.MaterialUI.Pickers
    open System


    type State = DateTime option


    type Msg =
        | PickedDate of DateTime option


    let init () = None, Cmd.none


    let update dispatch msg state =
        match msg with
        | PickedDate dt -> dt, Cmd.ofSub (fun _ -> dt |> dispatch)


    let useStyles = Styles.makeStyles (fun styles theme ->
        {|
            picker = styles.create [
                style.color (theme.palette.primary.main)
            ]
        |}
    )

    type Props =
        {|
            label : string
            dispatch : DateTime option -> unit
        |}


    let private comp =
        React.functionComponent("datepicker", fun (props : Props) ->
            let state, dispatch = React.useElmish(init, update props.dispatch, [||])
            let classes  = useStyles()
            printfn "datepicker state: %A" state

            Mui.pickerUtilsProvider [
                Mui.keyboardDatePicker [
                    keyboardDatePicker.label props.label
                    keyboardDatePicker.clearable true
                    keyboardDatePicker.maxDate DateTime.Now
                    keyboardDatePicker.minDate (DateTime.Now.AddYears(-18))

                    match state with
                    | Some  _ -> keyboardDatePicker.value state
                    | None    -> keyboardDatePicker.value null

                    keyboardDatePicker.onChange (fun dt _ -> dt |> PickedDate |> dispatch)
                    keyboardDatePicker.format "dd-MMMM-yyyy"
                    keyboardDatePicker.inputProps [
                        prop.className classes.picker
                    ]
                ]
            ]
        )


    let render label dispatch = comp ({| label = label; dispatch = dispatch |})
