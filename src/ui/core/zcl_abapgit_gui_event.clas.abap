CLASS zcl_abapgit_gui_event DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_abapgit_gui_event .

    CLASS-METHODS class_constructor .

    CLASS-METHODS new
      IMPORTING
        !ii_gui_services   TYPE REF TO zif_abapgit_gui_services OPTIONAL
        !iv_action         TYPE clike
        !iv_getdata        TYPE clike OPTIONAL
        !it_postdata       TYPE zif_abapgit_html_viewer=>ty_post_data OPTIONAL
      RETURNING
        VALUE(ro_instance) TYPE REF TO zcl_abapgit_gui_event.
    METHODS constructor
      IMPORTING
        !ii_gui_services TYPE REF TO zif_abapgit_gui_services OPTIONAL
        !iv_action       TYPE clike
        !iv_getdata      TYPE clike OPTIONAL
        !it_postdata     TYPE zif_abapgit_html_viewer=>ty_post_data OPTIONAL .
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA mo_query TYPE REF TO zcl_abapgit_string_map.
    DATA mo_form_data TYPE REF TO zcl_abapgit_string_map.

    CLASS-DATA gv_non_breaking_space TYPE string .

    TYPES: BEGIN OF ty_name_value,
             name  TYPE string,
             value TYPE string,
           END OF ty_name_value.
    TYPES ty_name_value_tt TYPE STANDARD TABLE OF ty_name_value WITH DEFAULT KEY.

    METHODS fields_to_map
      IMPORTING
        it_fields            TYPE ty_name_value_tt
      RETURNING
        VALUE(ro_string_map) TYPE REF TO zcl_abapgit_string_map
      RAISING
        zcx_abapgit_exception.

    CLASS-METHODS parse_post_form_data
      IMPORTING
        !it_post_data    TYPE zif_abapgit_html_viewer=>ty_post_data
        !iv_upper_cased  TYPE abap_bool DEFAULT abap_false
      RETURNING
        VALUE(rt_fields) TYPE ty_name_value_tt .
    CLASS-METHODS parse_fields
      IMPORTING
        !iv_string       TYPE clike
        !iv_upper_cased  TYPE abap_bool DEFAULT abap_false
      RETURNING
        VALUE(rt_fields) TYPE ty_name_value_tt .

    CLASS-METHODS parse_fields_upper_case_name
      IMPORTING
        !iv_string       TYPE clike
      RETURNING
        VALUE(rt_fields) TYPE ty_name_value_tt .

    CLASS-METHODS translate_postdata
      IMPORTING
        !it_postdata     TYPE zif_abapgit_html_viewer=>ty_post_data
      RETURNING
        VALUE(rv_string) TYPE string .

    CLASS-METHODS field_keys_to_upper
      CHANGING
        !ct_fields TYPE ty_name_value_tt .
    CLASS-METHODS unescape
      IMPORTING
        !iv_string       TYPE string
      RETURNING
        VALUE(rv_string) TYPE string .

ENDCLASS.



CLASS zcl_abapgit_gui_event IMPLEMENTATION.


  METHOD constructor.

    " Edge Webview control returns upper case action but abapGit requires lower case (#4841)
    zif_abapgit_gui_event~mi_gui_services = ii_gui_services.
    zif_abapgit_gui_event~mv_action       = to_lower( iv_action ).
    zif_abapgit_gui_event~mv_getdata      = iv_getdata.
    zif_abapgit_gui_event~mt_postdata     = it_postdata.

    IF ii_gui_services IS BOUND.
      zif_abapgit_gui_event~mv_current_page_name = ii_gui_services->get_current_page_name( ).
    ENDIF.

  ENDMETHOD.


  METHOD fields_to_map.
    FIELD-SYMBOLS <ls_field> LIKE LINE OF it_fields.

    CREATE OBJECT ro_string_map EXPORTING iv_case_insensitive = abap_true.
    LOOP AT it_fields ASSIGNING <ls_field>.
      ro_string_map->set(
        iv_key = <ls_field>-name
        iv_val = <ls_field>-value ).
    ENDLOOP.
  ENDMETHOD.


  METHOD new.
    CREATE OBJECT ro_instance
      EXPORTING
        ii_gui_services = ii_gui_services
        iv_action       = iv_action
        iv_getdata      = iv_getdata
        it_postdata     = it_postdata.
  ENDMETHOD.


  METHOD zif_abapgit_gui_event~form_data.

    IF mo_form_data IS NOT BOUND.
      mo_form_data = fields_to_map( parse_post_form_data( zif_abapgit_gui_event~mt_postdata ) ).
      mo_form_data->freeze( ).
    ENDIF.
    ro_string_map = mo_form_data.

  ENDMETHOD.


  METHOD zif_abapgit_gui_event~query.

    IF mo_query IS NOT BOUND.
      mo_query = fields_to_map( parse_fields( zif_abapgit_gui_event~mv_getdata ) ).
      mo_query->freeze( ).
    ENDIF.
    ro_string_map = mo_query.

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
      DELETE lt_post_data INDEX lv_last_line_index.
    ENDWHILE.

    CONCATENATE LINES OF lt_post_data INTO rv_string
      IN CHARACTER MODE RESPECTING BLANKS.
    CONCATENATE rv_string ls_last_line INTO rv_string
      IN CHARACTER MODE.

  ENDMETHOD.


  METHOD field_keys_to_upper.

    FIELD-SYMBOLS <ls_field> LIKE LINE OF ct_fields.

    LOOP AT ct_fields ASSIGNING <ls_field>.
      <ls_field>-name = to_upper( <ls_field>-name ).
    ENDLOOP.

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


  METHOD class_constructor.

    CONSTANTS lc_nbsp TYPE xstring VALUE 'C2A0'. " &nbsp;

    TRY.
        gv_non_breaking_space = zcl_abapgit_convert=>xstring_to_string_utf8( lc_nbsp ).
      CATCH zcx_abapgit_exception.
        ASSERT 0 = 1.
    ENDTRY.

  ENDMETHOD.
ENDCLASS.
