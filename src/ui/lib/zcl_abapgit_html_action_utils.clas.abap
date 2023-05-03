CLASS zcl_abapgit_html_action_utils DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    CLASS-METHODS parse_post_form_data
      IMPORTING
        !it_post_data    TYPE zif_abapgit_html_viewer=>ty_post_data
        !iv_upper_cased  TYPE abap_bool DEFAULT abap_false
      RETURNING
        VALUE(rt_fields) TYPE tihttpnvp .
    CLASS-METHODS parse_fields
      IMPORTING
        !iv_string       TYPE clike
        !iv_upper_cased  TYPE abap_bool DEFAULT abap_false
      RETURNING
        VALUE(rt_fields) TYPE tihttpnvp .
    CLASS-METHODS parse_fields_upper_case_name
      IMPORTING
        !iv_string       TYPE clike
      RETURNING
        VALUE(rt_fields) TYPE tihttpnvp .
    CLASS-METHODS translate_postdata
      IMPORTING
        !it_postdata     TYPE zif_abapgit_html_viewer=>ty_post_data
      RETURNING
        VALUE(rv_string) TYPE string .

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
      RETURNING
        VALUE(rv_string) TYPE string .
    CLASS-METHODS obj_encode
      IMPORTING
        !iv_key          TYPE zif_abapgit_persistence=>ty_repo-key
        !ig_object       TYPE any
      RETURNING
        VALUE(rv_string) TYPE string .

    CLASS-METHODS class_constructor.
    CLASS-METHODS dbkey_encode
      IMPORTING
        !is_key          TYPE zif_abapgit_persistence=>ty_content
      RETURNING
        VALUE(rv_string) TYPE string .

  PROTECTED SECTION.
  PRIVATE SECTION.
    CLASS-DATA gv_non_breaking_space TYPE string.

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



CLASS zcl_abapgit_html_action_utils IMPLEMENTATION.


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


  METHOD class_constructor.

    CONSTANTS lc_nbsp TYPE xstring VALUE 'C2A0'. " &nbsp;

    TRY.
        gv_non_breaking_space = zcl_abapgit_convert=>xstring_to_string_utf8( lc_nbsp ).
      CATCH zcx_abapgit_exception.
        ASSERT 0 = 1.
    ENDTRY.

  ENDMETHOD.


  METHOD dbkey_encode.

    DATA: lt_fields TYPE tihttpnvp.

    add_field( EXPORTING iv_name = 'TYPE'
                         ig_field = is_key-type CHANGING ct_field = lt_fields ).
    add_field( EXPORTING iv_name = 'VALUE'
                         ig_field = is_key-value CHANGING ct_field = lt_fields ).

    rv_string = cl_http_utility=>fields_to_string( lt_fields ).

  ENDMETHOD.


  METHOD dir_encode.

    DATA: lt_fields TYPE tihttpnvp.
    add_field( EXPORTING iv_name = 'PATH'
                         ig_field = iv_path CHANGING ct_field = lt_fields ).
    rv_string = cl_http_utility=>fields_to_string( lt_fields ).

  ENDMETHOD.


  METHOD field_keys_to_upper.

    FIELD-SYMBOLS <ls_field> LIKE LINE OF ct_fields.

    LOOP AT ct_fields ASSIGNING <ls_field>.
      <ls_field>-name = to_upper( <ls_field>-name ).
    ENDLOOP.

  ENDMETHOD.


  METHOD file_encode.

    DATA: lt_fields TYPE tihttpnvp.


    add_field( EXPORTING iv_name = 'KEY'
                         ig_field = iv_key CHANGING ct_field = lt_fields ).
    add_field( EXPORTING iv_name = 'PATH'
                         ig_field = ig_file CHANGING ct_field = lt_fields ).
    add_field( EXPORTING iv_name = 'FILENAME'
                         ig_field = ig_file CHANGING ct_field = lt_fields ).

    rv_string = cl_http_utility=>fields_to_string( lt_fields ).

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


  METHOD jump_encode.

    DATA: lt_fields TYPE tihttpnvp.


    add_field( EXPORTING iv_name = 'TYPE'
                         ig_field = iv_obj_type CHANGING ct_field = lt_fields ).
    add_field( EXPORTING iv_name = 'NAME'
                         ig_field = iv_obj_name CHANGING ct_field = lt_fields ).

    IF iv_filename IS NOT INITIAL.
      add_field( EXPORTING iv_name = 'FILE'
                           ig_field = iv_filename CHANGING ct_field = lt_fields ).
    ENDIF.

    rv_string = cl_http_utility=>fields_to_string( lt_fields ).

  ENDMETHOD.


  METHOD obj_encode.

    DATA: lt_fields TYPE tihttpnvp.


    add_field( EXPORTING iv_name = 'KEY'
                         ig_field = iv_key CHANGING ct_field = lt_fields ).
    add_field( EXPORTING iv_name = 'OBJ_TYPE'
                         ig_field = ig_object CHANGING ct_field = lt_fields ).
    add_field( EXPORTING iv_name = 'OBJ_NAME'
                         ig_field = ig_object CHANGING ct_field = lt_fields ).

    rv_string = cl_http_utility=>fields_to_string( lt_fields ).

  ENDMETHOD.


  METHOD parse_fields.

    DATA:
      lt_substrings TYPE string_table,
      ls_field      LIKE LINE OF rt_fields.

    FIELD-SYMBOLS <lv_substring> LIKE LINE OF lt_substrings.

    SPLIT iv_string AT '&' INTO TABLE lt_substrings.

    LOOP AT lt_substrings ASSIGNING <lv_substring>.

      CLEAR ls_field.
      " On attempt to change unescaping -> run unit tests to check !

      " Unescape name and value separately
      ls_field-name = unescape( substring_before(
        val = <lv_substring>
        sub = '=' ) ).

      ls_field-value = unescape( substring_after(
        val = <lv_substring>
        sub = '=' ) ).

      IF ls_field IS INITIAL. " Not a field with proper structure
        CONTINUE.
      ENDIF.

      APPEND ls_field TO rt_fields.

    ENDLOOP.

    IF iv_upper_cased = abap_true.
      field_keys_to_upper( CHANGING ct_fields = rt_fields ).
    ENDIF.

  ENDMETHOD.


  METHOD parse_fields_upper_case_name.

    rt_fields = parse_fields(
      iv_string      = iv_string
      iv_upper_cased = abap_true ).

  ENDMETHOD.


  METHOD parse_post_form_data.

    DATA lv_serialized_post_data TYPE string.

    lv_serialized_post_data = translate_postdata( it_post_data ).
    IF iv_upper_cased = abap_true.
      rt_fields = parse_fields_upper_case_name( lv_serialized_post_data ).
    ELSE.
      rt_fields = parse_fields( lv_serialized_post_data ).
    ENDIF.

  ENDMETHOD.


  METHOD translate_postdata.

    DATA: lt_post_data       TYPE zif_abapgit_html_viewer=>ty_post_data,
          ls_last_line       LIKE LINE OF it_postdata,
          lv_last_line_index TYPE i.

    IF it_postdata IS INITIAL.
      RETURN. "Nothing to do
    ENDIF.

    lt_post_data = it_postdata.

    "Save the last line for separate merge, because we don't need its trailing spaces
    WHILE ls_last_line IS INITIAL.
      lv_last_line_index = lines( lt_post_data ).
      READ TABLE lt_post_data INTO ls_last_line INDEX lv_last_line_index.
      "Avoid trailing null values (see isssue #4832)
      "todo, keep until SAP GUI for Java is fixed (remove on 2022-12-31)
      ls_last_line = replace( val  = ls_last_line
                              sub  = zcl_abapgit_git_utils=>get_null( )
                              with = space
                              occ  = 0 ).
      DELETE lt_post_data INDEX lv_last_line_index.
    ENDWHILE.

    CONCATENATE LINES OF lt_post_data INTO rv_string
      IN CHARACTER MODE RESPECTING BLANKS.
    CONCATENATE rv_string ls_last_line INTO rv_string
      IN CHARACTER MODE.

  ENDMETHOD.


  METHOD unescape.

* do not use cl_http_utility as it does strange things with the encoding
    rv_string = iv_string.

* todo, more to be added here
    REPLACE ALL OCCURRENCES OF '%3A' IN rv_string WITH ':' IGNORING CASE.
    REPLACE ALL OCCURRENCES OF '%3F' IN rv_string WITH '?' IGNORING CASE.
    REPLACE ALL OCCURRENCES OF '%3D' IN rv_string WITH '=' IGNORING CASE.
    REPLACE ALL OCCURRENCES OF '%2F' IN rv_string WITH '/' IGNORING CASE.
    REPLACE ALL OCCURRENCES OF '%25' IN rv_string WITH '%' IGNORING CASE.
    REPLACE ALL OCCURRENCES OF '%26' IN rv_string WITH '&' IGNORING CASE.
    REPLACE ALL OCCURRENCES OF gv_non_breaking_space IN rv_string WITH ` `.

  ENDMETHOD.
ENDCLASS.
