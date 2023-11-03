CLASS zcl_abapgit_html_action_utils DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    CLASS-METHODS jump_encode
      IMPORTING
        !iv_obj_type     TYPE tadir-object
        !iv_obj_name     TYPE tadir-obj_name
        !iv_filename     TYPE string OPTIONAL
      RETURNING
        VALUE(rv_string) TYPE string .
    CLASS-METHODS dir_encode
      IMPORTING
        !iv_path         TYPE string
      RETURNING
        VALUE(rv_string) TYPE string .
    CLASS-METHODS file_encode
      IMPORTING
        !iv_key          TYPE zif_abapgit_persistence=>ty_repo-key
        !ig_file         TYPE any
        !iv_extra        TYPE clike OPTIONAL
      RETURNING
        VALUE(rv_string) TYPE string .
    CLASS-METHODS obj_encode
      IMPORTING
        !iv_key          TYPE zif_abapgit_persistence=>ty_repo-key
        !ig_object       TYPE any
      RETURNING
        VALUE(rv_string) TYPE string .
    CLASS-METHODS dbkey_encode
      IMPORTING
        !is_key          TYPE zif_abapgit_persistence=>ty_content
      RETURNING
        VALUE(rv_string) TYPE string .
  PROTECTED SECTION.
  PRIVATE SECTION.

    CLASS-METHODS add_field
      IMPORTING
        !iv_name  TYPE string
        !ig_field TYPE any
      CHANGING
        !ct_field TYPE tihttpnvp .

    CLASS-METHODS fields_to_string
      IMPORTING
        !it_fields       TYPE tihttpnvp
      RETURNING
        VALUE(rv_string) TYPE string.
ENDCLASS.



CLASS ZCL_ABAPGIT_HTML_ACTION_UTILS IMPLEMENTATION.


  METHOD add_field.

    DATA ls_field LIKE LINE OF ct_field.

    FIELD-SYMBOLS <lg_src> TYPE any.

    ls_field-name = iv_name.

    CASE cl_abap_typedescr=>describe_by_data( ig_field )->kind.
      WHEN cl_abap_typedescr=>kind_elem.
        ls_field-value = ig_field.
      WHEN cl_abap_typedescr=>kind_struct.
        ASSIGN COMPONENT iv_name OF STRUCTURE ig_field TO <lg_src>.
        ASSERT <lg_src> IS ASSIGNED.
        ls_field-value = <lg_src>.
      WHEN OTHERS.
        ASSERT 0 = 1.
    ENDCASE.

    APPEND ls_field TO ct_field.

  ENDMETHOD.


  METHOD dbkey_encode.

    DATA lt_fields TYPE tihttpnvp.

    add_field( EXPORTING iv_name = 'TYPE'
                         ig_field = is_key-type CHANGING ct_field = lt_fields ).
    add_field( EXPORTING iv_name = 'VALUE'
                         ig_field = is_key-value CHANGING ct_field = lt_fields ).

    rv_string = fields_to_string( lt_fields ).

  ENDMETHOD.


  METHOD dir_encode.

    DATA lt_fields TYPE tihttpnvp.
    add_field( EXPORTING iv_name = 'PATH'
                         ig_field = iv_path CHANGING ct_field = lt_fields ).
    rv_string = fields_to_string( lt_fields ).

  ENDMETHOD.


  METHOD fields_to_string.

* There is no equivalent to cl_http_utility=>fields_to_string released in ABAP Cloud,
* see cl_web_http_utility

    DATA lt_tab   TYPE STANDARD TABLE OF string WITH DEFAULT KEY.
    DATA lv_str   TYPE string.
    DATA ls_field LIKE LINE OF it_fields.

    LOOP AT it_fields INTO ls_field.
      ls_field-value = cl_http_utility=>escape_url( ls_field-value ).
      lv_str = ls_field-name && '=' && ls_field-value.
      APPEND lv_str TO lt_tab.
    ENDLOOP.
    rv_string = concat_lines_of(
      table = lt_tab
      sep   = '&' ).

  ENDMETHOD.


  METHOD file_encode.

    DATA lt_fields TYPE tihttpnvp.


    add_field(
      EXPORTING
        iv_name  = 'KEY'
        ig_field = iv_key
      CHANGING
        ct_field = lt_fields ).

    add_field(
      EXPORTING
        iv_name  = 'PATH'
        ig_field = ig_file
      CHANGING
        ct_field = lt_fields ).

    add_field(
      EXPORTING
        iv_name  = 'FILENAME'
        ig_field = ig_file
      CHANGING
        ct_field = lt_fields ).

    IF iv_extra IS SUPPLIED.
      add_field(
        EXPORTING
          iv_name  = 'EXTRA'
          ig_field = iv_extra
        CHANGING
          ct_field = lt_fields ).
    ENDIF.

    rv_string = fields_to_string( lt_fields ).

  ENDMETHOD.


  METHOD jump_encode.

    DATA lt_fields TYPE tihttpnvp.


    add_field( EXPORTING iv_name = 'TYPE'
                         ig_field = iv_obj_type CHANGING ct_field = lt_fields ).
    add_field( EXPORTING iv_name = 'NAME'
                         ig_field = iv_obj_name CHANGING ct_field = lt_fields ).

    IF iv_filename IS NOT INITIAL.
      add_field( EXPORTING iv_name = 'FILE'
                           ig_field = iv_filename CHANGING ct_field = lt_fields ).
    ENDIF.

    rv_string = fields_to_string( lt_fields ).

  ENDMETHOD.


  METHOD obj_encode.

    DATA lt_fields TYPE tihttpnvp.


    add_field( EXPORTING iv_name = 'KEY'
                         ig_field = iv_key CHANGING ct_field = lt_fields ).
    add_field( EXPORTING iv_name = 'OBJ_TYPE'
                         ig_field = ig_object CHANGING ct_field = lt_fields ).
    add_field( EXPORTING iv_name = 'OBJ_NAME'
                         ig_field = ig_object CHANGING ct_field = lt_fields ).

    rv_string = fields_to_string( lt_fields ).

  ENDMETHOD.
ENDCLASS.
