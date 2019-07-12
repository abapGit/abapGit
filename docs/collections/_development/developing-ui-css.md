---
title: UI - CSS and assets
order: 92
---

This doc covers asset management, css processing and recommended asset development flow. See also the [UI - HTML pages](./developing-ui.html).

## TL;DR

- If you add an asset - code it in `ZCL_ABAPGIT_UI_FACTORY=>INIT_ASSET_MANAGER`
- There are 3 main CSS sheets to mind about: `ag-icons.css` with icon definitions, `common.css` with layouts (don't use for colors!) and `theme-default.css` for colors
- Custom themes (dark and belize) are combined with default so that a missing attributes are inherited from the default one
- abapGit uses internal CSS preprocessing to support CSS variables (which are otherwise not supported by IE - undercover browser of SAP GUI)
- Convenient way for changing and uploading css and other assets is [W3MIPOLLER](https://github.com/sbcgua/abap_w3mi_poller)


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

### Ensuring working of one-file version of abapGit (abapmerge)

The tricky thing is that abapGit can be either installed as a development version, deploying all the MIME objects in particular **or** as a single file. This single file must contain all the assets (images, css, js and fonts) **in-code**. This is enabled by **abapmerge** tool. Consider the `css/common.css` registration above. 

- First, `lt_inline` is cleared. And in the development version of abapGit it is then just passed to `register_asset` being initial. The asset manager is thus falls back to `ZABAPGIT_CSS_COMMON` MIME object (which is conveniently deployed in case of dev version).
- In case of one-file abapGit version there is no MIME object. However, `@@abapmerge include` statement is processed by abapmerge and the file `zabapgit_css_common.w3mi.data.css` is included to the code line by line in form of `_inline '$$'`, where `$$` is the text file line. Thus, at the moment of `register_asset` the content of `lt_inline` is **not** initial and takes the priority over the missing MIME.

Note: for the binary files, like fonts, use `@@abapmerge include-base64` pragma.

## CSS structure and themes

abapGit uses several CSS sheets to style it's visual design:

- `ag-icons.css` (`ZABAPGIT_ICON_FONT_CSS`) - defines the icons. Also read in [Adding icons](./adding-icons.html)
- `common.css` (`ZABAPGIT_CSS_COMMON`) - main CSS sheet which defines the layout. **Please don't define colors in it**
- `theme-default.css` (`ZABAPGIT_CSS_THEME_DEFAULT`) - default color scheme
- `theme-belize-blue.css` and `theme-dark.css` ( `ZABAPGIT_CSS_THEME_BELIZE_BLUE` and `ZABAPGIT_CSS_THEME_DARK` ) - custom color schemes.

A regular page loads: icons, common, default theme and, optionally, one of custom themes. So the resulting style is defined by compination of them. **Importantly** custom themes take the default one as the basis, so colors and variables **not** defined explicitly in the theme will be takes from the default one.

### CSS variables support

Internet explorer - which is the undercover browser component of SAP GUI - does not support CSS variables which are quite useful for e.g. color definitions. However, abapGit preprocesses `theme-*` files, detecting the variables and applying them explicitly to other attributes and classes.

Internally this is done by a combination of `ZCL_ABAPGIT_GUI_HTML_PROCESSOR` and `ZCL_ABAPGIT_GUI_CSS_PROCESSOR`. The first one detects CSS links in html head, the second merges them into `bundle.css` which is then re-linked from html head instead of `theme-*`.

**Debugging note**: the links to the replaced CSS files remain in the html head, they are just commented. So if you what to edit UI files locally, just uncomment them and comment the `bundle.css` link.

```html
  <head>
    ...
    <link rel="stylesheet" type="text/css" href="css/common.css">
    <link rel="stylesheet" type="text/css" href="css/ag-icons.css">
    <!-- by AG HTML preprocessor <link rel="stylesheet" type="text/css" href="css/theme-default.css">-->
    <!-- UNCOMMENT THIS ^^^^^^^^^^^^^^^^^^ -->
    <!-- by AG HTML preprocessor --><link rel="stylesheet" type="text/css" href="css/bundle.css">
    <!-- COMMENT THIS ^^^^^^^^^^^^^^^^^^ -->
  </head>
```

## Recommended asset development flow

In order to edit CSS files you have to download them to the frontend, edit, debug in IE or Chrome Devtools and upload them back. Doing so via SMW0 may be inconvenient for multiple assets (main css + themes + js). One of the solution is to use [W3MIPOLLER](https://github.com/sbcgua/abap_w3mi_poller). The idea of the tool is to define connection between MIME asset and a frontend file and then monitor file changes - as soon as you save the file, the poller detects it and automatically uploads to the MIME storage.

![](../img/w3mimepoller-1.png)

In the selection screen you define one or several pairs MIME-to-File. You can also save them as variants. Choose the right option at the bottom:

- Just start polling - just start detection of changes
- Download before polling - take existing MIME object and overwrite the files - useful for initial setup or after remote AG changes
- Upload before polling - overwrites MIME object with existing files

Just a handy recommendation: if you save a variant, save it with "just start polling", not to overwrite something occasionally.

Run the program. After the initial action (if chosen) it will start monitoring file changes - leave it running. Eventually, after file was modified, it will report the upload at the screen.

![](../img/w3mimepoller-2.png)
