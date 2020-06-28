module App

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


type State = PIM


type Msg = Started


let init () = PIM, Cmd.none


let update msg state =
    match msg with
    | Started -> state, Cmd.none


let useStyles = Styles.makeStyles(fun styles theme ->
    {|
        appBar = styles.create [
            style.display.flex
            style.flexDirection.row
            style.padding (theme.spacing 2)
        ]

        page = styles.create [
            style.marginTop (theme.spacing 10)
        ]
    |}
)


let private comp =
    React.functionComponent("app", fun () ->
        let state, dispatch = React.useElmish(init, update, [||])
        let classes = useStyles ()

        Mui.container [
            container.component' "main"
            container.maxWidth.sm
            prop.children [
                Mui.appBar [
                    prop.className classes.appBar
                    appBar.children [
                        Mui.typography [
                            typography.variant.h6
                            prop.text "Informedica Pediatric Risk Calculator"
                        ]
                    ]
                ]

                Mui.container [
                    prop.className classes.page
                    prop.children [
                        match state with
                        | PIM -> Pages.PIM.render ()
                    ]

                ]
            ]
        ]
    )


let render _ _ = comp ()
