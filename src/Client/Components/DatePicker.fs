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
        | PickedDate dt -> dt |> Some, Cmd.ofSub (fun _ -> dt |> dispatch)


    let useStyles = Styles.makeStyles (fun styles theme ->
        {|
            picker = styles.create [
                style.color (theme.palette.primary.main)
            ]
        |}

    )


    let private comp =
        React.functionComponent("datepicker", fun (props : {| label : string; dispatch : DateTime option -> unit |}) ->
            let state, dispatch = React.useElmish(init, update props.dispatch, [||])
            let classes  = useStyles()

            Mui.pickerUtilsProvider [
                Mui.keyboardDatePicker [
                    keyboardDatePicker.label props.label
                    keyboardDatePicker.clearable true
                    match state with
                    | Some dt -> keyboardDatePicker.value dt
                    | None    -> ()

                    keyboardDatePicker.onChange (fun dt _ -> dt |> PickedDate |> dispatch)
                    keyboardDatePicker.maxDate DateTime.Now
                    keyboardDatePicker.format "dd-MMMM-yyyy"
                    keyboardDatePicker.inputProps [
                        prop.className classes.picker
                    ]
                ]
            ]
        )


    let render label dispatch = comp ({| label = label; dispatch = dispatch |})
