---
title: UI - HTML pages
order: 94
---

This doc covers html form component in abapGit UI. See also the [UI - HTML pages](./developing-ui.html).

## General

There is a helper class to render html forms - `zcl_abapgit_html_form`. To see an example - open the online repo creation dialog (code: class `zcl_abapgit_gui_page_addonline`). Typical usage:
- create the form
- add fields one by one
- for complex fields (radio) - add `options` right after the field
- on render - pass `zcl_abapgit_string_map` instance of values and, optionally, a map of validation results (see below)
- fields can be required (`iv_required = abap_true`)
- fields can have tooltips (`iv_hint = 'help for the field'`)
- text fields may have placeholders (`iv_placeholder = '...'`)
- text fields may have side-actions - button next to them - passing current form state to abap for additional logic and re-render
- a form may have one or more *commands* at the bottom. *Main* ones (`iv_is_main = abap_true`) - will be highlighted

```abap

DATA lo_form TYPE REF TO zcl_abapgit_html_form.

lo_form = zcl_abapgit_html_form=>create( iv_form_id = 'add-repo-online-form' ).
lo_form->text(
    iv_name        = c_id-package
    iv_side_action = c_event-choose_package
    iv_required    = abap_true
    iv_label       = 'Package'
    iv_hint        = 'SAP package for the code (should be a dedicated one)'
    iv_placeholder = 'Z... / $...' ).
lo_form->radio(
    iv_name        = c_id-folder_logic
    iv_default_value = zif_abapgit_dot_abapgit=>c_folder_logic-prefix
    iv_label       = 'Folder logic'
    iv_hint        = 'Define how package folders are named in the repo' ).
lo_form->option(
    iv_label       = 'Prefix'
    iv_value       = zif_abapgit_dot_abapgit=>c_folder_logic-prefix ).
lo_form->option(
    iv_label       = 'Full'
    iv_value       = zif_abapgit_dot_abapgit=>c_folder_logic-full ).
lo_form->checkbox(
    iv_name        = c_id-ignore_subpackages
    iv_label       = 'Ignore subpackages'
    iv_hint        = 'Syncronize root package only' ).

lo_form->command(
    iv_label       = 'Clone online repo'
    iv_is_main     = abap_true
    iv_action      = c_event-add_online_repo ).

ro_html->add( lo_form->render(
    iv_form_class     = 'dialog w600px m-em5-sides margin-v1'
    io_values         = mo_form_data
    io_validation_log = mo_validation_log ) ).

```

## Values and validation

The class is designed so that it's instance is more a declaration of the form, that can be created once and then rendered each time with different values. On render you pass CSS class of the form, values and optionally validation results.

Values is an instance of `zcl_abapgit_string_map` - key value map, where key must be same as field name (thus it's recommended to define names as constants). E.g.

```abap
lo_form->text(
    iv_name        = c_id-package " <<<<< NAME
    iv_label       = 'Package' ).
...
DATA mo_form_data TYPE REF TO zcl_abapgit_string_map.
CREATE OBJECT mo_form_data
mo_form_data->set(
    iv_key = c_id-package
    iv_val = 'XYZ' ).             " <<<<< VALUE
...
lo_form->render(
    iv_form_class = 'dialog w600px m-em5-sides margin-v1'
    io_values     = mo_form_data ).
```

If validation log is passed, then it's checked for a non-empty value of field name, and renders it as the error message.

```abap
lo_form->text(
    iv_name        = c_id-package " <<<<< NAME
    iv_label       = 'Package' ).
...
DATA mo_validation_log TYPE REF TO zcl_abapgit_string_map.
CREATE OBJECT mo_validation_log
mo_form_data->set(
    iv_key = c_id-package          " <<<<< SAME NAME
    iv_val = 'OMG! It crushed!' ). " <<<<< ERROR MESSAGE
...
lo_form->render(
    iv_form_class     = 'dialog w600px m-em5-sides margin-v1'
    io_values         = mo_form_data
    io_validation_log = mo_validation_log ) ). " <<< ERRORS HERE
```