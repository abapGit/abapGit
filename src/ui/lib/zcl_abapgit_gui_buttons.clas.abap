CLASS zcl_abapgit_gui_buttons DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    CLASS-METHODS new_online
      RETURNING VALUE(rv_html_string) TYPE string.

    CLASS-METHODS flow
      RETURNING VALUE(rv_html_string) TYPE string.

    CLASS-METHODS new_offline
      RETURNING VALUE(rv_html_string) TYPE string.

    CLASS-METHODS advanced
      RETURNING VALUE(rv_html_string) TYPE string.

    CLASS-METHODS help
      RETURNING VALUE(rv_html_string) TYPE string.

    CLASS-METHODS repo_list
      RETURNING VALUE(rv_html_string) TYPE string.

    CLASS-METHODS settings
      RETURNING VALUE(rv_html_string) TYPE string.

    CLASS-METHODS experimental
      RETURNING VALUE(rv_html_string) TYPE string.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_abapgit_gui_buttons IMPLEMENTATION.


  METHOD advanced.
    rv_html_string = zcl_abapgit_html=>icon(
      iv_name = 'tools-solid'
      iv_hint = 'Utilities' ).
  ENDMETHOD.


  METHOD experimental.
    rv_html_string = zcl_abapgit_html=>icon(
      iv_name = 'vial-solid/red'
      iv_hint = 'Experimental Features are Enabled' ).
  ENDMETHOD.


  METHOD help.
    rv_html_string = zcl_abapgit_html=>icon(
      iv_name = 'question-circle-solid'
      iv_hint = 'Help' ).
  ENDMETHOD.


  METHOD new_offline.
    rv_html_string = zcl_abapgit_html=>icon( 'plug' ) && ' New Offline'.
  ENDMETHOD.


  METHOD new_online.
    rv_html_string = zcl_abapgit_html=>icon( 'cloud-upload-alt' ) && ' New Online'.
  ENDMETHOD.


  METHOD flow.
    rv_html_string = zcl_abapgit_html=>icon( 'flow' ) && ' Flow'.
  ENDMETHOD.


  METHOD repo_list.
    rv_html_string = zcl_abapgit_html=>icon( 'bars' ) && ' Repository List'.
  ENDMETHOD.


  METHOD settings.
    rv_html_string = zcl_abapgit_html=>icon( 'cog' ) && ' Global Settings'.
  ENDMETHOD.
ENDCLASS.
