namespace Components


module RadioButtons =
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
            fieldset = styles.create [
                style.marginTop (theme.spacing 3)
                style.marginBottom (theme.spacing 3)
            ]
        |}
    )


    type Props =
        {|
            Title: string
            Items : {| value: string; label: string |} list
            Dispatch: string -> unit
        |}


    let private comp =
        React.functionComponent("radiobuttons", fun (props: Props) ->
            let classes = useStyles ()

            Mui.formControl [
                formControl.component' "fieldset"
                prop.className classes.fieldset
                formControl.children [
                    Mui.formLabel [
                        formLabel.component' "legend"
                        prop.text props.Title
                    ]
                    Mui.radioGroup [
                        // hook up the dispatch
                        radioGroup.onChange (props.Dispatch)
                        // create the radiobuttons
                        props.Items
                        |> List.map (fun b ->
                            Mui.formControlLabel [
                                formControlLabel.control (Mui.radio [])
                                formControlLabel.value b.value
                                formControlLabel.label b.label
                            ]
                        )

                        |> radioGroup.children
                    ]
                ]
            ]

        )

    let render title dispatch items =
        let items =
            items
            |> List.map (fun (v, l) ->
                {| value = v; label = l |}
            )
        comp ({| Title = title; Dispatch = dispatch; Items = items |})