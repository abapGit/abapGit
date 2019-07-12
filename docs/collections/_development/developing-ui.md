---
title: UI - HTML pages
order: 91
---

This doc covers page creation, html rendering and event handling. See also [UI - CSS and assets](./developing-ui-css.html).

## TL;DR

- To create a new page in abapGit you subclass `ZCL_ABAPGIT_GUI_PAGE` and redefine `RENDER_CONTENT` method
- Use `ZCL_ABAPGIT_HTML` to collect HTML content
- Use `ZCL_ABAPGIT_HTML=>ICON` to render icons
- Use `ZCL_ABAPGIT_HTML=>A` to render anchors, don't render them manually `<a>...</a>`
- Please, please, care about usability, content readability and style in general :pray: ;)
- Check `ZCL_ABAPGIT_GUI_CHUNK_LIB` for some existing html chunks like `render_error`

## GUI components

abapGit UI is based on HTML and `CL_GUI_HTML_VIEWER`. Main parts are:

- ZCL_ABAPGIT_GUI - the class which initializes `CL_GUI_HTML_VIEWER` and manages page stack
- ZCL_ABAPGIT_GUI_ASSET_MANAGER - manages static assets like images, css, js code and fonts
- ZCL_ABAPGIT_HTML - helper for HTML accumulation and rendering
- ZCL_ABAPGIT_GUI_ROUTER - abapGit specific global event handling, main to route between the pages or run globally defined actions like repo installation
- ZCL_ABAPGIT_GUI_PAGE - base class for pages. It renders typical html headers and abapGit related java scripts. So in most cases you probably just want to subclass it and render just the content

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

## Renderables

Sub-classing `ZCL_ABAPGIT_GUI_PAGE` is not the only way to render the content. You may want to separate some visual component which is not a page e.g. `ZCL_ABAPGIT_GUI_VIEW_REPO` is a class like that. In essence you have to implement `ZIF_ABAPGIT_GUI_RENDERABLE` and it's method - `render`. Then you can reuse it or even pass directly to GUI class as a page to render.

## Router and event handlers

To process sapevents in abap the component (page) must implement `ZIF_ABAPGIT_GUI_EVENT_HANDLER=>on_event`. It has the same importing params as `sapevent` handler of `cl_gui_html_viewer`, please refer SAP official documentation for param meaning and detail. For the exporting params see below.

Events can be processed on 2 levels - in page **or** in the router. If an event occures, the GUI checks if the current page implements `ZIF_ABAPGIT_GUI_EVENT_HANDLER` and if so calls it. If the event was not handled by the page (see below how this is indicated) the event is passed to the router.

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
