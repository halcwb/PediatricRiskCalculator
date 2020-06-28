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

    let render =
        React.functionComponent("radiobuttons", fun (props: {|  title: string; buttons : {| value: string; label: string |} list; dispatch: string -> unit |}) ->
            let classes = useStyles ()

            Mui.formControl [
                formControl.component' "fieldset"
                prop.className classes.fieldset
                formControl.children [
                    Mui.formLabel [
                        formLabel.component' "legend"
                        prop.text props.title
                    ]
                    Mui.radioGroup [
                        // hook up the dispatch
                        radioGroup.onChange (props.dispatch)
                        // create the radiobuttons
                        props.buttons
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