CLASS zcl_abapgit_html_action_utils DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    CLASS-METHODS field_keys_to_upper
      CHANGING
        !ct_fields TYPE tihttpnvp .
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
    CLASS-METHODS add_field
      IMPORTING
        !name TYPE string
        !iv   TYPE any
      CHANGING
        !ct   TYPE tihttpnvp .
    CLASS-METHODS get_field
      IMPORTING
        !name TYPE string
        !it   TYPE tihttpnvp
      CHANGING
        !cv   TYPE any .
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
        !ig_file         TYPE any                             "assuming ty_file
      RETURNING
        VALUE(rv_string) TYPE string .
    CLASS-METHODS obj_encode
      IMPORTING
        !iv_key          TYPE zif_abapgit_persistence=>ty_repo-key
        !ig_object       TYPE any                         "assuming ty_item
      RETURNING
        VALUE(rv_string) TYPE string .
    CLASS-METHODS file_obj_decode
      IMPORTING
        !iv_string TYPE clike
      EXPORTING
        !ev_key    TYPE zif_abapgit_persistence=>ty_repo-key
        !eg_file   TYPE any                "assuming ty_file
        !eg_object TYPE any            "assuming ty_item
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
    CLASS-METHODS dbcontent_decode
      IMPORTING
        !it_postdata      TYPE cnht_post_data_tab
      RETURNING
        VALUE(rs_content) TYPE zif_abapgit_persistence=>ty_content .
    CLASS-METHODS parse_commit_request
      IMPORTING
        !it_postdata TYPE cnht_post_data_tab
      EXPORTING
        !es_fields   TYPE any .
    CLASS-METHODS stage_decode
      IMPORTING
        !iv_getdata TYPE clike
      EXPORTING
        !ev_key     TYPE zif_abapgit_persistence=>ty_repo-key
        !ev_seed    TYPE string
      RAISING
        zcx_abapgit_exception .
  PRIVATE SECTION.
    CLASS-METHODS unescape
      IMPORTING iv_string        TYPE string
      RETURNING VALUE(rv_string) TYPE string.

ENDCLASS.



CLASS ZCL_ABAPGIT_HTML_ACTION_UTILS IMPLEMENTATION.


  METHOD add_field.

    DATA ls_field LIKE LINE OF ct.

    FIELD-SYMBOLS <lg_src> TYPE any.

    ls_field-name = name.

    CASE cl_abap_typedescr=>describe_by_data( iv )->kind.
      WHEN cl_abap_typedescr=>kind_elem.
        ls_field-value = iv.
      WHEN cl_abap_typedescr=>kind_struct.
        ASSIGN COMPONENT name OF STRUCTURE iv TO <lg_src>.
        ASSERT <lg_src> IS ASSIGNED.
        ls_field-value = <lg_src>.
      WHEN OTHERS.
        ASSERT 0 = 1.
    ENDCASE.

    APPEND ls_field TO ct.

  ENDMETHOD.  "add_field


  METHOD dbcontent_decode.

    DATA: lt_fields TYPE tihttpnvp,
          lv_string TYPE string.


    CONCATENATE LINES OF it_postdata INTO lv_string.

    lv_string = cl_http_utility=>unescape_url( lv_string ).

    rs_content = dbkey_decode( lv_string ).

    lt_fields = parse_fields_upper_case_name( lv_string ).

    get_field( EXPORTING name = 'XMLDATA' it = lt_fields CHANGING cv = rs_content-data_str ).
    IF rs_content-data_str(1) <> '<' AND rs_content-data_str+1(1) = '<'. " Hmmm ???
      rs_content-data_str = rs_content-data_str+1.
    ENDIF.

  ENDMETHOD.                    "dbcontent_decode


  METHOD dbkey_decode.

    DATA: lt_fields TYPE tihttpnvp.

    lt_fields = parse_fields_upper_case_name( cl_http_utility=>unescape_url( |{ iv_string }| ) ).

    get_field( EXPORTING name = 'TYPE'  it = lt_fields CHANGING cv = rs_key-type ).
    get_field( EXPORTING name = 'VALUE' it = lt_fields CHANGING cv = rs_key-value ).

  ENDMETHOD.                    "dbkey_decode


  METHOD dbkey_encode.

    DATA: lt_fields TYPE tihttpnvp.

    add_field( EXPORTING name = 'TYPE'  iv = is_key-type CHANGING ct = lt_fields ).
    add_field( EXPORTING name = 'VALUE' iv = is_key-value CHANGING ct = lt_fields ).

    rv_string = cl_http_utility=>if_http_utility~fields_to_string( lt_fields ).

  ENDMETHOD.                    "dbkey_encode


  METHOD dir_decode.

    DATA: lt_fields TYPE tihttpnvp.

    lt_fields = parse_fields( iv_string ).
    get_field( EXPORTING name = 'PATH' it = lt_fields CHANGING cv = rv_path ).

  ENDMETHOD.                    "dir_decode


  METHOD dir_encode.

    DATA: lt_fields TYPE tihttpnvp.
    add_field( EXPORTING name = 'PATH' iv = iv_path CHANGING ct = lt_fields ).
    rv_string = cl_http_utility=>if_http_utility~fields_to_string( lt_fields ).

  ENDMETHOD.                    "dir_encode


  METHOD field_keys_to_upper.

    FIELD-SYMBOLS <ls_field> LIKE LINE OF ct_fields.

    LOOP AT ct_fields ASSIGNING <ls_field>.
      <ls_field>-name = to_upper( <ls_field>-name ).
    ENDLOOP.

  ENDMETHOD.


  METHOD file_encode.

    DATA: lt_fields TYPE tihttpnvp.


    add_field( EXPORTING name = 'KEY'      iv = iv_key CHANGING ct = lt_fields ).
    add_field( EXPORTING name = 'PATH'     iv = ig_file CHANGING ct = lt_fields ).
    add_field( EXPORTING name = 'FILENAME' iv = ig_file CHANGING ct = lt_fields ).

    rv_string = cl_http_utility=>if_http_utility~fields_to_string( lt_fields ).

  ENDMETHOD.                    "file_encode


  METHOD file_obj_decode.

    DATA: lt_fields TYPE tihttpnvp.

    ASSERT eg_file IS SUPPLIED OR eg_object IS SUPPLIED.

    CLEAR: ev_key, eg_file, eg_object.
    lt_fields = parse_fields_upper_case_name( iv_string ).

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


  METHOD get_field.

    FIELD-SYMBOLS: <ls_field> LIKE LINE OF it,
                   <lg_dest>  TYPE any.


    READ TABLE it ASSIGNING <ls_field> WITH KEY name = name.
    IF sy-subrc IS NOT INITIAL.
      RETURN.
    ENDIF.

    CASE cl_abap_typedescr=>describe_by_data( cv )->kind.
      WHEN cl_abap_typedescr=>kind_elem.
        cv = <ls_field>-value.
      WHEN cl_abap_typedescr=>kind_struct.
        ASSIGN COMPONENT name OF STRUCTURE cv TO <lg_dest>.
        ASSERT <lg_dest> IS ASSIGNED.
        <lg_dest> = <ls_field>-value.
      WHEN OTHERS.
        ASSERT 0 = 1.
    ENDCASE.

  ENDMETHOD.  "get_field


  METHOD jump_decode.

    DATA: lt_fields TYPE tihttpnvp.

    lt_fields = parse_fields( iv_string ).

    get_field( EXPORTING name = 'TYPE' it = lt_fields CHANGING cv = ev_obj_type ).
    get_field( EXPORTING name = 'NAME' it = lt_fields CHANGING cv = ev_obj_name ).

  ENDMETHOD.                    "jump_decode


  METHOD jump_encode.

    DATA: lt_fields TYPE tihttpnvp.


    add_field( EXPORTING name = 'TYPE' iv = iv_obj_type CHANGING ct = lt_fields ).
    add_field( EXPORTING name = 'NAME' iv = iv_obj_name CHANGING ct = lt_fields ).

    rv_string = cl_http_utility=>if_http_utility~fields_to_string( lt_fields ).

  ENDMETHOD.                    "jump_encode


  METHOD obj_encode.

    DATA: lt_fields TYPE tihttpnvp.


    add_field( EXPORTING name = 'KEY'      iv = iv_key CHANGING ct = lt_fields ).
    add_field( EXPORTING name = 'OBJ_TYPE' iv = ig_object CHANGING ct = lt_fields ).
    add_field( EXPORTING name = 'OBJ_NAME' iv = ig_object CHANGING ct = lt_fields ).

    rv_string = cl_http_utility=>if_http_utility~fields_to_string( lt_fields ).

  ENDMETHOD.                    "obj_encode


  METHOD parse_commit_request.

    CONSTANTS: lc_replace TYPE string VALUE '<<new>>'.

    DATA: lv_string TYPE string,
          lt_fields TYPE tihttpnvp.

    FIELD-SYMBOLS <lv_body> TYPE string.

    CLEAR es_fields.

    CONCATENATE LINES OF it_postdata INTO lv_string.
    REPLACE ALL OCCURRENCES OF zif_abapgit_definitions=>gc_crlf    IN lv_string WITH lc_replace.
    REPLACE ALL OCCURRENCES OF zif_abapgit_definitions=>gc_newline IN lv_string WITH lc_replace.
    lt_fields = parse_fields_upper_case_name( lv_string ).

    get_field( EXPORTING name = 'COMMITTER_NAME'  it = lt_fields CHANGING cv = es_fields ).
    get_field( EXPORTING name = 'COMMITTER_EMAIL' it = lt_fields CHANGING cv = es_fields ).
    get_field( EXPORTING name = 'AUTHOR_NAME'     it = lt_fields CHANGING cv = es_fields ).
    get_field( EXPORTING name = 'AUTHOR_EMAIL'    it = lt_fields CHANGING cv = es_fields ).
    get_field( EXPORTING name = 'COMMENT'         it = lt_fields CHANGING cv = es_fields ).
    get_field( EXPORTING name = 'BODY'            it = lt_fields CHANGING cv = es_fields ).

    ASSIGN COMPONENT 'BODY' OF STRUCTURE es_fields TO <lv_body>.
    ASSERT <lv_body> IS ASSIGNED.
    REPLACE ALL OCCURRENCES OF lc_replace IN <lv_body> WITH zif_abapgit_definitions=>gc_newline.

  ENDMETHOD.                    "parse_commit_request


  METHOD parse_fields.

    DATA: lt_substrings TYPE stringtab,
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

  ENDMETHOD.  " parse_fields.


  METHOD stage_decode.

    DATA: lt_fields TYPE tihttpnvp.

    lt_fields = parse_fields_upper_case_name( iv_getdata ).

    get_field( EXPORTING name = 'KEY'  it = lt_fields CHANGING cv = ev_key ).
    get_field( EXPORTING name = 'SEED' it = lt_fields CHANGING cv = ev_seed ).

    ASSERT NOT ev_key IS INITIAL.

  ENDMETHOD.  " stage_decode.


  METHOD unescape.
* do not use cl_http_utility as it does strange things with the encoding
    rv_string = iv_string.

* todo, more to be added here
    REPLACE ALL OCCURRENCES OF '%3F' IN rv_string WITH '?'.

  ENDMETHOD.
ENDCLASS.
