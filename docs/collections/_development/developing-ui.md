---
title: Developing UI
order: 91
---

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

## Asset manager

`ZCL_ABAPGIT_GUI_ASSET_MANAGER` class is responsible for managing static assets. Very briefly: relevant assets must be registered in the asset manager instance during GUI initiation so that they can be used in the browser UI. The registration happens in `ZCL_ABAPGIT_UI_FACTORY=>INIT_ASSET_MANAGER`. Here is an abstract from the method for example:

```abap
DEFINE _inline.
    APPEND &1 TO lt_inline. " <<< THIS IS USED TO INCLUDE ASSET IN-CODE WITH ABAPMERGE
END-OF-DEFINITION.

DATA lt_inline TYPE string_table.

CLEAR lt_inline.
" @@abapmerge include zabapgit_css_common.w3mi.data.css > _inline '$$'.
ro_asset_man->register_asset(
    iv_url       = 'css/common.css'         " <<< PATH TO THE ASSET FROM HTML, WHICH IS ALSO IT'S UNIQUE NAME
    iv_type      = 'text/css'               " <<< CONTENT TYPE OF THE ASSET
    iv_mime_name = 'ZABAPGIT_CSS_COMMON'    " <<< MIME OBJECT NAME
    iv_inline    = concat_lines_of( table = lt_inline sep = cl_abap_char_utilities=>newline ) ).

CLEAR lt_inline.
" @@abapmerge include-base64 zabapgit_icon_font.w3mi.data.woff > _inline '$$'. " <<< THE FILE BINARY !!!
ro_asset_man->register_asset(
    iv_url       = 'font/ag-icons.woff'
    iv_type      = 'font/woff'
    iv_mime_name = 'ZABAPGIT_ICON_FONT'
    iv_base64    = concat_lines_of( table = lt_inline ) ).

" see https://github.com/larshp/abapGit/issues/201 for source SVG
ro_asset_man->register_asset(
    iv_url       = 'img/logo'
    iv_type      = 'image/png'
    iv_base64    =
        'iVBORw0KGgoAAAANSUhENCSVQICAgIfAhkiAAA...'.

```

There are several ways to store content of a static asset in abapGit.

1. Pass the asset inline. e.g. the logo at the end is a PNG image. It is encoded as BASE64 and passed as `iv_base64` param
2. Inline can be also a text then should be passed with `iv_inline`
3. Read from a MIME object - if inline is not passed, the assets falls back to the MIME

The tricky thing is that abapGit can be either installed as a development version, deploying all the MIME objects in particular **or** as a single file. This single file must contain all the assets (images, css, js and fonts) **in-code**. This is enabled by **abapmerge** tool. Consider the `css/common.css` registration above. 

- First, `lt_inline` is cleared. And in the development version of abapGit it is then just passed to `register_asset` being initial. The asset manager is thus falls back to `ZABAPGIT_CSS_COMMON` MIME object (which is conveniently deployed in case of dev version).
- in case of one-file abapGit version there is no MIME object. However, `@@abapmerge include` statement is processed by abapmerge and the file `zabapgit_css_common.w3mi.data.css` is included to the code line by line in form of `_inline '$$'`, where `$$` is the text file line. Thus, at the moment of `register_asset` the content of `lt_inline` is **not** initial and takes the priority over the missing MIME.

Note: for the binary files, like fonts, use `@@abapmerge include-base64` pragma.
