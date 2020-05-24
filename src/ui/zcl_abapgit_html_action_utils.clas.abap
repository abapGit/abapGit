CLASS zcl_abapgit_html_action_utils DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    CLASS-METHODS parse_fields
      IMPORTING
        !iv_string       TYPE clike
      RETURNING
        VALUE(rt_fields) TYPE tihttpnvp .
    CLASS-METHODS parse_fields_upper_case_name
      IMPORTING
        !iv_string       TYPE clike
      RETURNING
        VALUE(rt_fields) TYPE tihttpnvp .
    CLASS-METHODS get_field
      IMPORTING
        !iv_name   TYPE string
        !it_field  TYPE tihttpnvp
        !iv_decode TYPE abap_bool DEFAULT abap_false
      CHANGING
        !cg_field  TYPE any .
    CLASS-METHODS jump_encode
      IMPORTING
        !iv_obj_type     TYPE tadir-object
        !iv_obj_name     TYPE tadir-obj_name
      RETURNING
        VALUE(rv_string) TYPE string .
    CLASS-METHODS jump_decode
      IMPORTING
        !iv_string   TYPE clike
      EXPORTING
        !ev_obj_type TYPE tadir-object
        !ev_obj_name TYPE tadir-obj_name
      RAISING
        zcx_abapgit_exception .
    CLASS-METHODS dir_encode
      IMPORTING
        !iv_path         TYPE string
      RETURNING
        VALUE(rv_string) TYPE string .
    CLASS-METHODS dir_decode
      IMPORTING
        !iv_string     TYPE clike
      RETURNING
        VALUE(rv_path) TYPE string
      RAISING
        zcx_abapgit_exception .
    CLASS-METHODS file_encode
      IMPORTING
        !iv_key          TYPE zif_abapgit_persistence=>ty_repo-key
        !ig_file         TYPE any
      RETURNING
        VALUE(rv_string) TYPE string .
    CLASS-METHODS obj_encode
      IMPORTING
        !iv_key          TYPE zif_abapgit_persistence=>ty_repo-key
        !ig_object       TYPE any
      RETURNING
        VALUE(rv_string) TYPE string .
    CLASS-METHODS file_obj_decode
      IMPORTING
        !iv_string TYPE clike
      EXPORTING
        !ev_key    TYPE zif_abapgit_persistence=>ty_repo-key
        !eg_file   TYPE any                                    "assuming ty_file
        !eg_object TYPE any                      "assuming ty_item
      RAISING
        zcx_abapgit_exception .
    CLASS-METHODS dbkey_encode
      IMPORTING
        !is_key          TYPE zif_abapgit_persistence=>ty_content
      RETURNING
        VALUE(rv_string) TYPE string .
    CLASS-METHODS dbkey_decode
      IMPORTING
        !iv_string    TYPE clike
      RETURNING
        VALUE(rs_key) TYPE zif_abapgit_persistence=>ty_content .
    CLASS-METHODS stage_decode
      IMPORTING
        !iv_getdata TYPE clike
      EXPORTING
        !ev_key     TYPE zif_abapgit_persistence=>ty_repo-key
        !ev_seed    TYPE string
      RAISING
        zcx_abapgit_exception .
  PROTECTED SECTION.
  PRIVATE SECTION.

    CLASS-METHODS field_keys_to_upper
      CHANGING
        !ct_fields TYPE tihttpnvp .
    CLASS-METHODS add_field
      IMPORTING
        !iv_name  TYPE string
        !ig_field TYPE any
      CHANGING
        !ct_field TYPE tihttpnvp .
    CLASS-METHODS unescape
      IMPORTING
        !iv_string       TYPE string
      RETURNING
        VALUE(rv_string) TYPE string .
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


  METHOD dbkey_decode.

    DATA: lt_fields TYPE tihttpnvp.

    lt_fields = parse_fields_upper_case_name( cl_http_utility=>unescape_url( |{ iv_string }| ) ).

    get_field( EXPORTING iv_name = 'TYPE'  it_field = lt_fields CHANGING cg_field = rs_key-type ).
    get_field( EXPORTING iv_name = 'VALUE' it_field = lt_fields CHANGING cg_field = rs_key-value ).

  ENDMETHOD.


  METHOD dbkey_encode.

    DATA: lt_fields TYPE tihttpnvp.

    add_field( EXPORTING iv_name = 'TYPE'  ig_field = is_key-type CHANGING ct_field = lt_fields ).
    add_field( EXPORTING iv_name = 'VALUE' ig_field = is_key-value CHANGING ct_field = lt_fields ).

    rv_string = cl_http_utility=>if_http_utility~fields_to_string( lt_fields ).

  ENDMETHOD.


  METHOD dir_decode.

    DATA: lt_fields TYPE tihttpnvp.

    lt_fields = parse_fields( iv_string ).
    get_field( EXPORTING iv_name = 'PATH' it_field = lt_fields CHANGING cg_field = rv_path ).

  ENDMETHOD.


  METHOD dir_encode.

    DATA: lt_fields TYPE tihttpnvp.
    add_field( EXPORTING iv_name = 'PATH' ig_field = iv_path CHANGING ct_field = lt_fields ).
    rv_string = cl_http_utility=>if_http_utility~fields_to_string( lt_fields ).

  ENDMETHOD.


  METHOD field_keys_to_upper.

    FIELD-SYMBOLS <ls_field> LIKE LINE OF ct_fields.

    LOOP AT ct_fields ASSIGNING <ls_field>.
      <ls_field>-name = to_upper( <ls_field>-name ).
    ENDLOOP.

  ENDMETHOD.


  METHOD file_encode.

    DATA: lt_fields TYPE tihttpnvp.


    add_field( EXPORTING iv_name = 'KEY'      ig_field = iv_key CHANGING ct_field = lt_fields ).
    add_field( EXPORTING iv_name = 'PATH'     ig_field = ig_file CHANGING ct_field = lt_fields ).
    add_field( EXPORTING iv_name = 'FILENAME' ig_field = ig_file CHANGING ct_field = lt_fields ).

    rv_string = cl_http_utility=>if_http_utility~fields_to_string( lt_fields ).

  ENDMETHOD.


  METHOD file_obj_decode.

    DATA: lt_fields TYPE tihttpnvp.

    ASSERT eg_file IS SUPPLIED OR eg_object IS SUPPLIED OR ev_key IS SUPPLIED.

    CLEAR: ev_key, eg_file, eg_object.
    lt_fields = parse_fields_upper_case_name( iv_string ).

    get_field( EXPORTING iv_name = 'KEY'      it_field = lt_fields CHANGING cg_field = ev_key ).

    IF eg_file IS SUPPLIED.
      get_field( EXPORTING iv_name = 'PATH'     it_field = lt_fields CHANGING cg_field = eg_file ).
      get_field( EXPORTING iv_name = 'FILENAME' it_field = lt_fields iv_decode = abap_true
                 CHANGING cg_field = eg_file  ).
    ENDIF.

    IF eg_object IS SUPPLIED.
      get_field( EXPORTING iv_name = 'OBJ_TYPE' it_field = lt_fields CHANGING cg_field = eg_object ).
      get_field( EXPORTING iv_name = 'OBJ_NAME' it_field = lt_fields iv_decode = abap_true
                 CHANGING cg_field = eg_object ).
    ENDIF.

  ENDMETHOD.


  METHOD get_field.

    DATA: lv_value TYPE string.

    FIELD-SYMBOLS: <ls_field> LIKE LINE OF it_field,
                   <lg_dest>  TYPE any.


    READ TABLE it_field ASSIGNING <ls_field> WITH KEY name = iv_name.
    IF sy-subrc IS NOT INITIAL.
      RETURN.
    ENDIF.

    lv_value = <ls_field>-value.

    IF iv_decode = abap_true.
      lv_value = cl_http_utility=>unescape_url( escaped = lv_value ).
    ENDIF.

    CASE cl_abap_typedescr=>describe_by_data( cg_field )->kind.
      WHEN cl_abap_typedescr=>kind_elem.
        cg_field = lv_value.
      WHEN cl_abap_typedescr=>kind_struct.
        ASSIGN COMPONENT iv_name OF STRUCTURE cg_field TO <lg_dest>.
        ASSERT <lg_dest> IS ASSIGNED.
        <lg_dest> = lv_value.
      WHEN OTHERS.
        ASSERT 0 = 1.
    ENDCASE.

  ENDMETHOD.


  METHOD jump_decode.

    DATA: lt_fields TYPE tihttpnvp.

    lt_fields = parse_fields( iv_string ).

    get_field( EXPORTING iv_name = 'TYPE' it_field = lt_fields CHANGING cg_field = ev_obj_type ).
    get_field( EXPORTING iv_name = 'NAME' it_field = lt_fields CHANGING cg_field = ev_obj_name ).

  ENDMETHOD.


  METHOD jump_encode.

    DATA: lt_fields TYPE tihttpnvp.


    add_field( EXPORTING iv_name = 'TYPE' ig_field = iv_obj_type CHANGING ct_field = lt_fields ).
    add_field( EXPORTING iv_name = 'NAME' ig_field = iv_obj_name CHANGING ct_field = lt_fields ).

    rv_string = cl_http_utility=>if_http_utility~fields_to_string( lt_fields ).

  ENDMETHOD.


  METHOD obj_encode.

    DATA: lt_fields TYPE tihttpnvp.


    add_field( EXPORTING iv_name = 'KEY'      ig_field = iv_key CHANGING ct_field = lt_fields ).
    add_field( EXPORTING iv_name = 'OBJ_TYPE' ig_field = ig_object CHANGING ct_field = lt_fields ).
    add_field( EXPORTING iv_name = 'OBJ_NAME' ig_field = ig_object CHANGING ct_field = lt_fields ).

    rv_string = cl_http_utility=>if_http_utility~fields_to_string( lt_fields ).

  ENDMETHOD.


  METHOD parse_fields.

    DATA: lt_substrings TYPE STANDARD TABLE OF string WITH DEFAULT KEY,
          ls_field      LIKE LINE OF rt_fields.

    FIELD-SYMBOLS: <lv_substring> LIKE LINE OF lt_substrings.


    SPLIT iv_string AT '&' INTO TABLE lt_substrings.

    LOOP AT lt_substrings ASSIGNING <lv_substring>.

      CLEAR: ls_field.

      ls_field-name = substring_before( val = <lv_substring>
                                     sub = '=' ).
      ls_field-name = unescape( ls_field-name ).

      ls_field-value = substring_after( val = <lv_substring>
                                     sub = '=' ).
      ls_field-value = unescape( ls_field-value ).

      INSERT ls_field INTO TABLE rt_fields.

    ENDLOOP.

  ENDMETHOD.


  METHOD parse_fields_upper_case_name.

    rt_fields = parse_fields( iv_string ).
    field_keys_to_upper( CHANGING ct_fields = rt_fields ).

  ENDMETHOD.


  METHOD stage_decode.

    DATA: lt_fields TYPE tihttpnvp.

    lt_fields = parse_fields_upper_case_name( iv_getdata ).

    get_field( EXPORTING iv_name = 'KEY'  it_field = lt_fields CHANGING cg_field = ev_key ).
    get_field( EXPORTING iv_name = 'SEED' it_field = lt_fields CHANGING cg_field = ev_seed ).

    ASSERT NOT ev_key IS INITIAL.

  ENDMETHOD.


  METHOD unescape.
* do not use cl_http_utility as it does strange things with the encoding
    rv_string = iv_string.

* todo, more to be added here
    REPLACE ALL OCCURRENCES OF '%3F' IN rv_string WITH '?'.
    REPLACE ALL OCCURRENCES OF '%3D' IN rv_string WITH '='.

  ENDMETHOD.
ENDCLASS.
