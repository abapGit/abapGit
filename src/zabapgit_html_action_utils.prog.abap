*&---------------------------------------------------------------------*
*&  Include           ZABAPGIT_HTML_ACTION_UTILS
*&---------------------------------------------------------------------*

*----------------------------------------------------------------------*
*       CLASS lcl_html_action_utils DEFINITION
*----------------------------------------------------------------------*
CLASS lcl_html_action_utils DEFINITION FINAL.
  PUBLIC SECTION.

    TYPES: BEGIN OF ty_commit_fields, "TODO refactor ! Move to normal place
             repo_key TYPE lcl_persistence_repo=>ty_repo-key,
             username TYPE string,
             email    TYPE string,
             comment  TYPE string,
             body     TYPE string,
           END OF ty_commit_fields.

    CLASS-METHODS jump_encode
      IMPORTING iv_obj_type      TYPE tadir-object
                iv_obj_name      TYPE tadir-obj_name
      RETURNING VALUE(rv_string) TYPE string.

    CLASS-METHODS jump_decode
      IMPORTING iv_string   TYPE clike
      EXPORTING ev_obj_type TYPE tadir-object
                ev_obj_name TYPE tadir-obj_name
      RAISING   lcx_exception.

    CLASS-METHODS file_encode
      IMPORTING iv_key           TYPE lcl_persistence_repo=>ty_repo-key
                ig_file          TYPE any "ty_repo_file
      RETURNING VALUE(rv_string) TYPE string.

    CLASS-METHODS file_decode
      IMPORTING iv_string TYPE clike
      EXPORTING ev_key    TYPE lcl_persistence_repo=>ty_repo-key
                eg_file   TYPE any "ty_repo_file
      RAISING   lcx_exception.

    CLASS-METHODS dbkey_encode
      IMPORTING is_key           TYPE lcl_persistence_db=>ty_content
      RETURNING VALUE(rv_string) TYPE string.

    CLASS-METHODS dbkey_decode
      IMPORTING iv_string     TYPE clike
      RETURNING VALUE(rs_key) TYPE lcl_persistence_db=>ty_content.

    CLASS-METHODS parse_commit_request
      IMPORTING it_postdata      TYPE cnht_post_data_tab
      RETURNING VALUE(rs_fields) TYPE ty_commit_fields.

    CLASS-METHODS repo_key_encode
      IMPORTING iv_key           TYPE lcl_persistence_repo=>ty_repo-key
      RETURNING VALUE(rv_string) TYPE string.

ENDCLASS.       "lcl_html_action_utils DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_html_action_utils IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS lcl_html_action_utils IMPLEMENTATION.

  METHOD jump_encode.

    DATA: lt_fields TYPE tihttpnvp,
          ls_field  LIKE LINE OF lt_fields.


    ls_field-name = 'TYPE'.
    ls_field-value = iv_obj_type.
    APPEND ls_field TO lt_fields.

    ls_field-name = 'NAME'.
    ls_field-value = iv_obj_name.
    APPEND ls_field TO lt_fields.

    rv_string = cl_http_utility=>if_http_utility~fields_to_string( lt_fields ).

  ENDMETHOD.                    "jump_encode

  METHOD jump_decode.

    DATA: lt_fields TYPE tihttpnvp,
          lv_string TYPE string.

    FIELD-SYMBOLS: <ls_field> LIKE LINE OF lt_fields.


    lv_string = iv_string.     " type conversion
    lt_fields = cl_http_utility=>if_http_utility~string_to_fields( lv_string ).

    READ TABLE lt_fields ASSIGNING <ls_field> WITH KEY name = 'TYPE'.
    IF sy-subrc = 0.
      ev_obj_type = <ls_field>-value.
    ELSE.
      CLEAR ev_obj_type.
    ENDIF.

    READ TABLE lt_fields ASSIGNING <ls_field> WITH KEY name = 'NAME'.
    IF sy-subrc = 0.
      ev_obj_name = <ls_field>-value.
    ELSE.
      CLEAR ev_obj_name.
    ENDIF.

  ENDMETHOD.                    "jump_decode

  METHOD file_encode.

    DATA: lt_fields TYPE tihttpnvp,
          ls_field  LIKE LINE OF lt_fields.

    FIELD-SYMBOLS <lv_field> TYPE string.

    ls_field-name = 'KEY'.
    ls_field-value = iv_key.
    APPEND ls_field TO lt_fields.

    ls_field-name = 'PATH'.
    ASSIGN COMPONENT ls_field-name OF STRUCTURE ig_file TO <lv_field>.
    ASSERT <lv_field> IS ASSIGNED.
    ls_field-value = <lv_field>.
    APPEND ls_field TO lt_fields.

    ls_field-name = 'FILENAME'.
    ASSIGN COMPONENT ls_field-name OF STRUCTURE ig_file TO <lv_field>.
    ASSERT <lv_field> IS ASSIGNED.
    ls_field-value = <lv_field>.
    APPEND ls_field TO lt_fields.

    rv_string = cl_http_utility=>if_http_utility~fields_to_string( lt_fields ).

  ENDMETHOD.                    "file_encode

  METHOD file_decode.

    DATA: lt_fields TYPE tihttpnvp,
          lv_string TYPE string.

    FIELD-SYMBOLS: <ls_field> LIKE LINE OF lt_fields,
                   <lv_field> TYPE string.

    CLEAR: ev_key, eg_file.
    lv_string = iv_string.     " type conversion
    lt_fields = cl_http_utility=>if_http_utility~string_to_fields( lv_string ).

    READ TABLE lt_fields ASSIGNING <ls_field> WITH KEY name = 'KEY'.
    IF sy-subrc = 0.
      ev_key = <ls_field>-value.
    ENDIF.

    READ TABLE lt_fields ASSIGNING <ls_field> WITH KEY name = 'PATH'.
    IF sy-subrc = 0.
      ASSIGN COMPONENT 'PATH' OF STRUCTURE eg_file TO <lv_field>.
      ASSERT <lv_field> IS ASSIGNED.
      <lv_field> = <ls_field>-value.
    ENDIF.

    READ TABLE lt_fields ASSIGNING <ls_field> WITH KEY name = 'FILENAME'.
    IF sy-subrc = 0.
      ASSIGN COMPONENT 'FILENAME' OF STRUCTURE eg_file TO <lv_field>.
      ASSERT <lv_field> IS ASSIGNED.
      <lv_field> = <ls_field>-value.
    ENDIF.

  ENDMETHOD.                    "file_decode

  METHOD dbkey_encode.

    DATA: lt_fields TYPE tihttpnvp,
          ls_field  LIKE LINE OF lt_fields.

    ls_field-name = 'TYPE'.
    ls_field-value = is_key-type.
    APPEND ls_field TO lt_fields.

    ls_field-name = 'VALUE'.
    ls_field-value = is_key-value.
    APPEND ls_field TO lt_fields.

    rv_string = cl_http_utility=>if_http_utility~fields_to_string( lt_fields ).

  ENDMETHOD.                    "dbkey_encode

  METHOD dbkey_decode.

    DATA: lt_fields TYPE tihttpnvp,
          lv_string TYPE string.

    FIELD-SYMBOLS: <ls_field> LIKE LINE OF lt_fields.

    lv_string = iv_string.     " type conversion
    lt_fields = cl_http_utility=>if_http_utility~string_to_fields( lv_string ).

    READ TABLE lt_fields ASSIGNING <ls_field> WITH KEY name = 'TYPE'.
    IF sy-subrc = 0.
      rs_key-type = <ls_field>-value.
    ENDIF.

    READ TABLE lt_fields ASSIGNING <ls_field> WITH KEY name = 'VALUE'.
    IF sy-subrc = 0.
      rs_key-value = <ls_field>-value.
    ENDIF.

  ENDMETHOD.                    "dbkey_decode

  METHOD parse_commit_request.

    CONSTANTS: lc_replace TYPE string VALUE '<<new>>'.

    DATA: lv_string TYPE string,
          lt_fields TYPE tihttpnvp.

    FIELD-SYMBOLS: <ls_field> LIKE LINE OF lt_fields.


    CONCATENATE LINES OF it_postdata INTO lv_string.

    REPLACE ALL OCCURRENCES OF gc_newline IN lv_string WITH lc_replace.

    lt_fields = cl_http_utility=>if_http_utility~string_to_fields( lv_string ).

    READ TABLE lt_fields ASSIGNING <ls_field> WITH KEY name = 'key' ##NO_TEXT.
    ASSERT sy-subrc = 0.
    rs_fields-repo_key = <ls_field>-value.

    READ TABLE lt_fields ASSIGNING <ls_field> WITH KEY name = 'username' ##NO_TEXT.
    ASSERT sy-subrc = 0.
    rs_fields-username = <ls_field>-value.

    READ TABLE lt_fields ASSIGNING <ls_field> WITH KEY name = 'email' ##NO_TEXT.
    ASSERT sy-subrc = 0.
    rs_fields-email = <ls_field>-value.

    READ TABLE lt_fields ASSIGNING <ls_field> WITH KEY name = 'comment' ##NO_TEXT.
    ASSERT sy-subrc = 0.
    rs_fields-comment = <ls_field>-value.

    READ TABLE lt_fields ASSIGNING <ls_field> WITH KEY name = 'body' ##NO_TEXT.
    ASSERT sy-subrc = 0.
    rs_fields-body = <ls_field>-value.
    REPLACE ALL OCCURRENCES OF lc_replace IN rs_fields-body WITH gc_newline.

  ENDMETHOD.                    "parse_commit_request

  METHOD repo_key_encode.

    DATA: lt_fields TYPE tihttpnvp,
          ls_field  LIKE LINE OF lt_fields.

    ls_field-name = 'KEY'.
    ls_field-value = iv_key.
    APPEND ls_field TO lt_fields.

    rv_string = cl_http_utility=>if_http_utility~fields_to_string( lt_fields ).

  ENDMETHOD.                    "repo_key_encode

ENDCLASS.       "lcl_html_action_utils IMPLEMENTATION