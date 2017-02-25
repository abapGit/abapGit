*&---------------------------------------------------------------------*
*&  Include           ZABAPGIT_HTML_ACTION_UTILS
*&---------------------------------------------------------------------*

*----------------------------------------------------------------------*
*       CLASS lcl_html_action_utils DEFINITION
*----------------------------------------------------------------------*
CLASS lcl_html_action_utils DEFINITION FINAL.
  PUBLIC SECTION.

    CLASS-METHODS field_keys_to_upper
      CHANGING ct_fields TYPE tihttpnvp.

    CLASS-METHODS add_field
      IMPORTING name TYPE string
                iv   TYPE any
      CHANGING  ct   TYPE tihttpnvp.

    CLASS-METHODS get_field
      IMPORTING name TYPE string
                it   TYPE tihttpnvp
      CHANGING  cv   TYPE any.

    CLASS-METHODS jump_encode
      IMPORTING iv_obj_type      TYPE tadir-object
                iv_obj_name      TYPE tadir-obj_name
      RETURNING VALUE(rv_string) TYPE string.

    CLASS-METHODS jump_decode
      IMPORTING iv_string   TYPE clike
      EXPORTING ev_obj_type TYPE tadir-object
                ev_obj_name TYPE tadir-obj_name
      RAISING   lcx_exception.

    CLASS-METHODS dir_encode
      IMPORTING iv_path          TYPE string
      RETURNING VALUE(rv_string) TYPE string.

    CLASS-METHODS dir_decode
      IMPORTING iv_string      TYPE clike
      RETURNING VALUE(rv_path) TYPE string
      RAISING   lcx_exception.

    CLASS-METHODS file_encode
      IMPORTING iv_key           TYPE lcl_persistence_repo=>ty_repo-key
                ig_file          TYPE any "assuming ty_file
      RETURNING VALUE(rv_string) TYPE string.

    CLASS-METHODS obj_encode
      IMPORTING iv_key           TYPE lcl_persistence_repo=>ty_repo-key
                ig_object        TYPE any "assuming ty_item
      RETURNING VALUE(rv_string) TYPE string.

    CLASS-METHODS file_obj_decode
      IMPORTING iv_string TYPE clike
      EXPORTING ev_key    TYPE lcl_persistence_repo=>ty_repo-key
                eg_file   TYPE any "assuming ty_file
                eg_object TYPE any "assuming ty_item
      RAISING   lcx_exception.

    CLASS-METHODS dbkey_encode
      IMPORTING is_key           TYPE lcl_persistence_db=>ty_content
      RETURNING VALUE(rv_string) TYPE string.

    CLASS-METHODS dbkey_decode
      IMPORTING iv_string     TYPE clike
      RETURNING VALUE(rs_key) TYPE lcl_persistence_db=>ty_content.

    CLASS-METHODS dbcontent_decode
      IMPORTING it_postdata       TYPE cnht_post_data_tab
      RETURNING VALUE(rs_content) TYPE lcl_persistence_db=>ty_content.

    CLASS-METHODS parse_commit_request
      IMPORTING it_postdata      TYPE cnht_post_data_tab
      EXPORTING es_fields        TYPE any.

    CLASS-METHODS decode_bg_update
      IMPORTING iv_getdata       TYPE clike
      RETURNING VALUE(rs_fields) TYPE lcl_persistence_background=>ty_background.


ENDCLASS.       "lcl_html_action_utils DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_html_action_utils IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS lcl_html_action_utils IMPLEMENTATION.

  METHOD field_keys_to_upper.

    FIELD-SYMBOLS <field> LIKE LINE OF ct_fields.

    LOOP AT ct_fields ASSIGNING <field>.
      <field>-name = to_upper( <field>-name ).
    ENDLOOP.

  ENDMETHOD.  "field_keys_to_upper

  METHOD add_field.

    DATA ls_field LIKE LINE OF ct.

    FIELD-SYMBOLS <src> TYPE any.

    ls_field-name = name.

    CASE cl_abap_typedescr=>describe_by_data( iv )->kind.
      WHEN cl_abap_typedescr=>kind_elem.
        ls_field-value = iv.
      WHEN cl_abap_typedescr=>kind_struct.
        ASSIGN COMPONENT name OF STRUCTURE iv TO <src>.
        ASSERT <src> IS ASSIGNED.
        ls_field-value = <src>.
      WHEN OTHERS.
        ASSERT 0 = 1.
    ENDCASE.

    APPEND ls_field TO ct.

  ENDMETHOD.  "add_field

  METHOD get_field.

    FIELD-SYMBOLS: <ls_field> LIKE LINE OF it,
                   <dest>     TYPE any.


    READ TABLE it ASSIGNING <ls_field> WITH KEY name = name.
    IF sy-subrc IS NOT INITIAL.
      RETURN.
    ENDIF.

    CASE cl_abap_typedescr=>describe_by_data( cv )->kind.
      WHEN cl_abap_typedescr=>kind_elem.
        cv = <ls_field>-value.
      WHEN cl_abap_typedescr=>kind_struct.
        ASSIGN COMPONENT name OF STRUCTURE cv TO <dest>.
        ASSERT <dest> IS ASSIGNED.
        <dest> = <ls_field>-value.
      WHEN OTHERS.
        ASSERT 0 = 1.
    ENDCASE.

  ENDMETHOD.  "get_field

  METHOD jump_encode.

    DATA: lt_fields TYPE tihttpnvp.


    add_field( EXPORTING name = 'TYPE' iv = iv_obj_type CHANGING ct = lt_fields ).
    add_field( EXPORTING name = 'NAME' iv = iv_obj_name CHANGING ct = lt_fields ).

    rv_string = cl_http_utility=>if_http_utility~fields_to_string( lt_fields ).

  ENDMETHOD.                    "jump_encode

  METHOD jump_decode.

    DATA: lt_fields TYPE tihttpnvp.


    lt_fields = cl_http_utility=>if_http_utility~string_to_fields( |{ iv_string }| ).

    get_field( EXPORTING name = 'TYPE' it = lt_fields CHANGING cv = ev_obj_type ).
    get_field( EXPORTING name = 'NAME' it = lt_fields CHANGING cv = ev_obj_name ).

  ENDMETHOD.                    "jump_decode

  METHOD dir_encode.

    DATA: lt_fields TYPE tihttpnvp.
    add_field( EXPORTING name = 'PATH' iv = iv_path CHANGING ct = lt_fields ).
    rv_string = cl_http_utility=>if_http_utility~fields_to_string( lt_fields ).

  ENDMETHOD.                    "dir_encode

  METHOD dir_decode.

    DATA: lt_fields TYPE tihttpnvp.
    lt_fields = cl_http_utility=>if_http_utility~string_to_fields( |{ iv_string }| ).
    get_field( EXPORTING name = 'PATH' it = lt_fields CHANGING cv = rv_path ).

  ENDMETHOD.                    "dir_decode

  METHOD file_encode.

    DATA: lt_fields TYPE tihttpnvp.


    add_field( EXPORTING name = 'KEY'      iv = iv_key CHANGING ct = lt_fields ).
    add_field( EXPORTING name = 'PATH'     iv = ig_file CHANGING ct = lt_fields ).
    add_field( EXPORTING name = 'FILENAME' iv = ig_file CHANGING ct = lt_fields ).

    rv_string = cl_http_utility=>if_http_utility~fields_to_string( lt_fields ).

  ENDMETHOD.                    "file_encode

  METHOD obj_encode.

    DATA: lt_fields TYPE tihttpnvp.


    add_field( EXPORTING name = 'KEY'      iv = iv_key CHANGING ct = lt_fields ).
    add_field( EXPORTING name = 'OBJ_TYPE' iv = ig_object CHANGING ct = lt_fields ).
    add_field( EXPORTING name = 'OBJ_NAME' iv = ig_object CHANGING ct = lt_fields ).

    rv_string = cl_http_utility=>if_http_utility~fields_to_string( lt_fields ).

  ENDMETHOD.                    "obj_encode

  METHOD file_obj_decode.

    DATA: lt_fields TYPE tihttpnvp.

    ASSERT eg_file IS SUPPLIED OR eg_object IS SUPPLIED.

    CLEAR: ev_key, eg_file, eg_object.
    lt_fields = cl_http_utility=>if_http_utility~string_to_fields( |{ iv_string }| ).
    field_keys_to_upper( CHANGING ct_fields = lt_fields ).

    get_field( EXPORTING name = 'KEY'      it = lt_fields CHANGING cv = ev_key ).

    IF eg_file IS SUPPLIED.
      get_field( EXPORTING name = 'PATH'     it = lt_fields CHANGING cv = eg_file ).
      get_field( EXPORTING name = 'FILENAME' it = lt_fields CHANGING cv = eg_file ).
    ENDIF.

    IF eg_object IS SUPPLIED.
      get_field( EXPORTING name = 'OBJ_TYPE' it = lt_fields CHANGING cv = eg_object ).
      get_field( EXPORTING name = 'OBJ_NAME' it = lt_fields CHANGING cv = eg_object ).
    ENDIF.

  ENDMETHOD.                    "file_decode

  METHOD dbkey_encode.

    DATA: lt_fields TYPE tihttpnvp.

    add_field( EXPORTING name = 'TYPE'  iv = is_key-type CHANGING ct = lt_fields ).
    add_field( EXPORTING name = 'VALUE' iv = is_key-value CHANGING ct = lt_fields ).

    rv_string = cl_http_utility=>if_http_utility~fields_to_string( lt_fields ).

  ENDMETHOD.                    "dbkey_encode

  METHOD dbkey_decode.

    DATA: lt_fields TYPE tihttpnvp.

    lt_fields = cl_http_utility=>if_http_utility~string_to_fields( |{ iv_string }| ).
    field_keys_to_upper( CHANGING ct_fields = lt_fields ).

    get_field( EXPORTING name = 'TYPE'  it = lt_fields CHANGING cv = rs_key-type ).
    get_field( EXPORTING name = 'VALUE' it = lt_fields CHANGING cv = rs_key-value ).

  ENDMETHOD.                    "dbkey_decode

  METHOD dbcontent_decode.

    DATA: lt_fields TYPE tihttpnvp,
          lv_string TYPE string.


    CONCATENATE LINES OF it_postdata INTO lv_string.
    rs_content = dbkey_decode( lv_string ).

    lt_fields  = cl_http_utility=>if_http_utility~string_to_fields( lv_string ).
    field_keys_to_upper( CHANGING ct_fields = lt_fields ).

    get_field( EXPORTING name = 'XMLDATA' it = lt_fields CHANGING cv = rs_content-data_str ).
    IF rs_content-data_str(1) <> '<' AND rs_content-data_str+1(1) = '<'. " Hmmm ???
      rs_content-data_str = rs_content-data_str+1.
*    ELSE.
*      CLEAR rs_content-data_str.
    ENDIF.

  ENDMETHOD.                    "dbcontent_decode

  METHOD parse_commit_request.

    CONSTANTS: lc_replace TYPE string VALUE '<<new>>'.

    DATA: lv_string TYPE string,
          lt_fields TYPE tihttpnvp.

    FIELD-SYMBOLS <body> TYPE string.

    CLEAR es_fields.

    CONCATENATE LINES OF it_postdata INTO lv_string.
    REPLACE ALL OCCURRENCES OF gc_newline IN lv_string WITH lc_replace.
    lt_fields = cl_http_utility=>if_http_utility~string_to_fields( lv_string ).
    field_keys_to_upper( CHANGING ct_fields = lt_fields ).

    get_field( EXPORTING name = 'COMMITTER_NAME'  it = lt_fields CHANGING cv = es_fields ).
    get_field( EXPORTING name = 'COMMITTER_EMAIL' it = lt_fields CHANGING cv = es_fields ).
    get_field( EXPORTING name = 'AUTHOR_NAME'     it = lt_fields CHANGING cv = es_fields ).
    get_field( EXPORTING name = 'AUTHOR_EMAIL'    it = lt_fields CHANGING cv = es_fields ).
    get_field( EXPORTING name = 'COMMENT'         it = lt_fields CHANGING cv = es_fields ).
    get_field( EXPORTING name = 'BODY'            it = lt_fields CHANGING cv = es_fields ).

    ASSIGN COMPONENT 'BODY' OF STRUCTURE es_fields TO <body>.
    ASSERT <body> IS ASSIGNED.
    REPLACE ALL OCCURRENCES OF lc_replace IN <body> WITH gc_newline.

    ASSERT es_fields IS NOT INITIAL.

  ENDMETHOD.                    "parse_commit_request

  METHOD decode_bg_update.

    DATA: lt_fields TYPE tihttpnvp.


    lt_fields = cl_http_utility=>if_http_utility~string_to_fields( |{ iv_getdata }| ).
    field_keys_to_upper( CHANGING ct_fields = lt_fields ).

    get_field( EXPORTING name = 'METHOD'   it = lt_fields CHANGING cv = rs_fields ).
    get_field( EXPORTING name = 'USERNAME' it = lt_fields CHANGING cv = rs_fields ).
    get_field( EXPORTING name = 'PASSWORD' it = lt_fields CHANGING cv = rs_fields ).
    get_field( EXPORTING name = 'AMETHOD'  it = lt_fields CHANGING cv = rs_fields ).
    get_field( EXPORTING name = 'ANAME'    it = lt_fields CHANGING cv = rs_fields ).
    get_field( EXPORTING name = 'AMAIL'    it = lt_fields CHANGING cv = rs_fields ).

    ASSERT NOT rs_fields IS INITIAL.

  ENDMETHOD.  "decode_bg_update

ENDCLASS.       "lcl_html_action_utils IMPLEMENTATION
