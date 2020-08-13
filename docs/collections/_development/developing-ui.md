---
title: UI - HTML pages
order: 91
---

This doc covers page creation, html rendering and event handling.
- See also [UI - CSS and assets](./developing-ui-css.html)
- See also [UI - Java script](./developing-ui-js.html)
- See also [Html forms](./developing-ui-forms.html)

## TL;DR

- To create a new page in abapGit you subclass `ZCL_ABAPGIT_GUI_PAGE` and redefine `RENDER_CONTENT` method *(also consider separating components that implement `ZIF_ABAPGIT_GUI_RENDERABLE` directly, this will probably become the primary approach in future)*
- Use `ZCL_ABAPGIT_HTML` to collect HTML content - method `add` accepts strings, string_tables and instances of `ZCL_ABAPGIT_HTML`
- Use `ZCL_ABAPGIT_HTML=>ICON` to render icons
- Use `ZCL_ABAPGIT_HTML=>A` to render anchors, don't render them manually `<a>...</a>`
- Please, please, care about usability, content readability and style in general :pray: ;)
- Check `ZCL_ABAPGIT_GUI_CHUNK_LIB` for some existing html chunks like `render_error`
- To register postponed html parts, scripts and hotkeys - access corresponding methods via `gui_services` method of `zcl_abapgit_gui_component`

## GUI components

abapGit UI is based on HTML and `CL_GUI_HTML_VIEWER`. Main parts are:

- *ZCL_ABAPGIT_GUI* - the class which initializes `CL_GUI_HTML_VIEWER` and manages page stack
- *ZCL_ABAPGIT_GUI_ASSET_MANAGER* - manages static assets like images, css, js code and fonts
- *ZCL_ABAPGIT_HTML* - helper for HTML accumulation and rendering
- *ZCL_ABAPGIT_GUI_ROUTER* - abapGit specific global event handling, main to route between the pages or run globally defined actions like repo installation
- *ZCL_ABAPGIT_GUI_PAGE* - base class for pages. It renders typical html headers and abapGit related java scripts. ~~So in most cases you probably just want to subclass it and render just the content~~
- *ZCL_ABAPGIT_GUI_COMPONENT* - base class for gui components. Gives access to `gui_services` to register postponed html parts, scripts and hotkeys. Usually, it is a good idea to subclass from it, if you want to use these features.
- *ZIF_ABAPGIT_GUI_RENDERABLE* - interface which a renderable component must expose to be able to interact with `ZCL_ABAPGIT_GUI`
- *ZIF_ABAPGIT_GUI_EVENT_HANDLER* - interface which a component must expose to be able to register itself as a event handler in `ZCL_ABAPGIT_GUI`
- *ZIF_ABAPGIT_GUI_HOTKEYS* - interface which a component must expose to be able to register hotkey actions

## Rendering content

An example of `RENDER_CONTENT` (or any other helper method with HTML output)

```abap
METHOD render_content.

    CREATE OBJECT ro_html.

    ro_html->add( '<div>' ).
    ro_html->add( '<h1>My content</h1>' ).
    ro_html->add_icon( 'star/error' ).
    ro_html->add_a(
        iv_txt = 'click me'
        iv_act = 'some_event_handled_in_abap' ).
    ro_html->add( render_some_complex_stuff( ) ).
    ro_html->add( '</div>' ).

ENDMETHOD.
```

### Html helper

`ro_html` which is the instance of `ZCL_ABAPGIT_HTML` is helper tool for html rendering. It accumulates html content and then can output it with `render` method. It has a couple of important methods:

- **ADD** - adds a chunk to accumulated HTML. You can pass a string or another `ZCL_ABAPGIT_HTML` instance. In the example above `render_some_stuff` may either return a string or have the same pattern as `render_content` (retuning `ZCL_ABAPGIT_HTML` instance)
- **ADD_ICON and ICON** - renders an icon. abapGit uses web-fonts to render icons (see [adding icons](./development/adding-icons.html)). The method accepts the icon name and a css-class name which represents a color separated by '/'. E.g.  in the example above it will render 'star' icon and assign 'error' css class to it which has red color in abapGit styes. The method has it's static brother `ZCL_ABAPGIT_HTML=>ICON` which is more convenient in some cases and just returns a rendered html string.
- **ADD_A and A** - renders a link (anchor) (`A` - static method). It is strongly suggested that you use this method instead of rendering `<a>` tags directly. Params:
    - `IV_TXT` - text to be rendered inside anchor
    - `IV_TYP` - type of action done on click. 3 options: 
        - `zif_abapgit_html=>c_action_type-url`- direct link to an url,
        - `...-sapevent` (the default) - pass an event to sap handler,
        - `...-onclick` - call a JS function,
        - `...-dummy` - just render an anchor but no action
    - `IV_ACT` - depending on the type should be either URL or sapevent name or JS function to call 
    - `IV_OPT` - `zif_abapgit_html=>c_html_opt-strong` or `...-cancel` or `...-crossout` - some semantic predefined styles to add to the link
    - `IV_CLASS` - additional css class, if needed
    - `IV_STYLE` - additional direct styles to use (generally discouraged, please use css classes instead)
    - `IV_ID` - id of the anchor (may be needed for JS code)
- **SET_TITLE** - the method is used for debug purposes for postponed html parts. As it is not visible which class registered a part, so title can be used to specify the origin.

## Renderables

Sub-classing `ZCL_ABAPGIT_GUI_PAGE` is not the only way to render the content. You may want to separate some visual component which is not a page e.g. `ZCL_ABAPGIT_GUI_VIEW_REPO` is a class like that. In essence you have to implement `ZIF_ABAPGIT_GUI_RENDERABLE` and it's method - `render`. Then you can reuse it or even pass directly to GUI class as a page to render.

It makes sense to also subclass your component from `ZCL_ABAPGIT_GUI_COMPONENT`. This class has a protected `gui_services` method returning the singleton instance of `ZIF_ABAPGIT_GUI_SERVICES`. The gui services are good for:
- registering self as an event handler (`register_event_handler`). Importantly, later registered event handlers have higher priority (processing is done from bottom to top)
- accessing hotkey services (`get_hotkeys_ctl`) - to register own hotkeys for the page (hotkeys are combines from the whole component stack)
- registering postponed html parts (`get_html_parts`)

## Postponed HTML parts

Components may have postponed parts, e.g. scripts or hidden forms. These chunks may need to be rendered in another location of the page (e.g. scripts are rendered at the end). There is a mechanism to enable it:

```abap
    " ... somewhere within render
    gui_services( )->get_html_parts( )->add_part(
      iv_collection = c_html_parts-scripts
      ii_part       = render_my_scripts( ) ).
```
where `render_my_scripts( )` must return an instance of `ZCL_ABAPGIT_HTML`.

Currently 2 collections are supported out of the box - scripts and hidden_forms (see definition of `zcl_abapgit_gui_component`). Scripts rendered after the page body. Hidden forms right before end of the body. But this does not limit you to these categories only - you may register own collections to exchange postponed parts between components supported by you. Collection is just a named list of `ZCL_ABAPGIT_HTML` instances.

## Router and event handlers

To process sapevents in abap the component (page) must implement `ZIF_ABAPGIT_GUI_EVENT_HANDLER=>on_event`. It has the same importing params as `sapevent` handler of `cl_gui_html_viewer`, please refer SAP official documentation for param meaning and detail. For the exporting params see below.

Events can be processed on 2 levels - in page/component **or** in the router. On new event:
- the GUI goes through event handlers stack - list of components that registered themselves as event handlers during rendering via `gui_services`
- the processing is done from the last registered handler to the first one (stack top to bottom)
- the first event handler that returns "handled" status breaks the cycle (see below how this is indicated)
- if the event was not handled by the handlers in the stack the event would be passed to the router

Router (`ZCL_ABAPGIT_GUI_ROUTER`) is the class which handle global abapGit commands like opening specific pages and actions like repo installation/deletion.

In order to indicate the result of event handling an `on_event` implementation must return `ev_state` (element of `zcl_abapgit_gui=>c_event_state`) and, optionally, `ei_page`:

- `not_handled` (same as `initial`) - event was not handled, process by next handler (e.g. the router)
- `re_render` - just re-render the current page (probably internal state of the page object was changed so the visualization should too)
- `new_page` - render `ei_page`
- `go_back` - render previous page in the call stack (e.g. user pressed F3)
- `no_more_act` - action was handled, no further processing required, and in particular **no re-rendering**
- `new_page_w_bookmark` - `ei_page` and put a bookmark - allows to use `go_back_to_bookmark` action that will skip all the page stack till the first bookmark
- `new_page_replacing` - `ei_page` and replace the current page in stack (so that F3 returns to the parent of the current page)
- `go_back_to_bookmark` - go back and skip all the page stack till the first bookmark (works with `new_page_w_bookmark`)

## Hotkey

TODO ...

In a nutshell:

```abap
  " somewhere within render
  gui_services( )->get_hotkeys_ctl( )->register_hotkeys( me ).
```

The component must implement `zif_abapgit_gui_hotkeys` and return list of keys, their human readable meaning and corresponding event to invoke.

```abap
  METHOD zif_abapgit_gui_hotkeys~get_hotkey_actions.

    DATA ls_hotkey_action LIKE LINE OF rt_hotkey_actions.

    ls_hotkey_action-ui_component = 'Stage'. " <<< This is to define origin of hotkeys

    ls_hotkey_action-description  = |Patch|. " <<< Human readable description
    ls_hotkey_action-action       = zif_abapgit_definitions=>c_action-go_patch. " <<< abapgit-wide action to open patch page
    ls_hotkey_action-hotkey       = |p|.     " <<< Key
    INSERT ls_hotkey_action INTO TABLE rt_hotkey_actions.

    ls_hotkey_action-description  = |Diff|.
    ls_hotkey_action-action       = zif_abapgit_definitions=>c_action-go_diff.
    ls_hotkey_action-hotkey       = |d|.
    INSERT ls_hotkey_action INTO TABLE rt_hotkey_actions.

  ENDMETHOD.
```
