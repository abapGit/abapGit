CLASS zcl_abapgit_gui_buttons DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    CLASS-METHODS new_online
      RETURNING VALUE(rv_html_string) TYPE string.

    CLASS-METHODS new_offline
      RETURNING VALUE(rv_html_string) TYPE string.

    CLASS-METHODS advanced
      RETURNING VALUE(rv_html_string) TYPE string.

    CLASS-METHODS help
      RETURNING VALUE(rv_html_string) TYPE string.

    CLASS-METHODS repo_list
      RETURNING VALUE(rv_html_string) TYPE string.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_abapgit_gui_buttons IMPLEMENTATION.

  METHOD new_online.
    rv_html_string = `<i class="icon icon-cloud-upload-alt"></i> New Online`.
  ENDMETHOD.

  METHOD new_offline.
    rv_html_string = `<i class="icon icon-plug"></i> New Offline`.
  ENDMETHOD.

  METHOD advanced.
    rv_html_string = `<i class="icon icon-tools-solid"></i>`.
  ENDMETHOD.

  METHOD help.
    rv_html_string = `<i class="icon icon-question-circle-solid"></i>`.
  ENDMETHOD.

  METHOD repo_list.
    rv_html_string = `<i class="icon icon-bars"></i> Repository list`.
  ENDMETHOD.

ENDCLASS.
