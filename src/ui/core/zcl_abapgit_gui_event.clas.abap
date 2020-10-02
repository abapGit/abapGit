CLASS zcl_abapgit_gui_event DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_abapgit_gui_event .

    METHODS constructor
      IMPORTING
        !ii_gui_services TYPE REF TO zif_abapgit_gui_services OPTIONAL
        !iv_action       TYPE clike
        !iv_getdata      TYPE clike OPTIONAL
        !it_postdata     TYPE cnht_post_data_tab OPTIONAL .
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA mo_query TYPE REF TO zcl_abapgit_string_map.
    DATA mo_query_upper_cased TYPE REF TO zcl_abapgit_string_map.
    DATA mo_form_data TYPE REF TO zcl_abapgit_string_map.
    DATA mo_form_data_upper_cased TYPE REF TO zcl_abapgit_string_map.

    METHODS fields_to_map
      IMPORTING
        it_fields            TYPE tihttpnvp
      RETURNING
        VALUE(ro_string_map) TYPE REF TO zcl_abapgit_string_map
      RAISING
        zcx_abapgit_exception.

    METHODS fields_to_map_macro
      IMPORTING
        it_fields      TYPE tihttpnvp
        iv_upper_cased TYPE abap_bool
      CHANGING
        co_string_map_lower TYPE REF TO zcl_abapgit_string_map
        co_string_map_upper TYPE REF TO zcl_abapgit_string_map
        co_string_map_return TYPE REF TO zcl_abapgit_string_map
      RAISING
        zcx_abapgit_exception.

ENDCLASS.



CLASS ZCL_ABAPGIT_GUI_EVENT IMPLEMENTATION.


  METHOD constructor.

    zif_abapgit_gui_event~mi_gui_services = ii_gui_services.
    zif_abapgit_gui_event~mv_action       = iv_action.
    zif_abapgit_gui_event~mv_getdata      = iv_getdata.
    zif_abapgit_gui_event~mt_postdata     = it_postdata.

  ENDMETHOD.


  METHOD fields_to_map.
    FIELD-SYMBOLS <ls_field> LIKE LINE OF it_fields.

    CREATE OBJECT ro_string_map.
    LOOP AT it_fields ASSIGNING <ls_field>.
      ro_string_map->set(
        iv_key = <ls_field>-name
        iv_val = <ls_field>-value ).
    ENDLOOP.
  ENDMETHOD.


  METHOD fields_to_map_macro.

    FIELD-SYMBOLS <lo_map> TYPE REF TO zcl_abapgit_string_map.

    IF iv_upper_cased = abap_true.
      ASSIGN co_string_map_upper TO <lo_map>.
    ELSE.
      ASSIGN co_string_map_lower TO <lo_map>.
    ENDIF.

    IF <lo_map> IS NOT BOUND.
      <lo_map> = fields_to_map( it_fields ).
      <lo_map>->freeze( ).
    ENDIF.
    co_string_map_return = <lo_map>.

  ENDMETHOD.


  METHOD zif_abapgit_gui_event~form_data.

    fields_to_map_macro(
      EXPORTING
        iv_upper_cased = iv_upper_cased
        it_fields      = zcl_abapgit_html_action_utils=>parse_post_form_data(
          it_post_data   = zif_abapgit_gui_event~mt_postdata
          iv_upper_cased = iv_upper_cased )
      CHANGING
        co_string_map_upper  = mo_form_data_upper_cased
        co_string_map_lower  = mo_form_data
        co_string_map_return = ro_string_map ).

  ENDMETHOD.


  METHOD zif_abapgit_gui_event~query.

    fields_to_map_macro(
      EXPORTING
        iv_upper_cased = iv_upper_cased
        it_fields      = zcl_abapgit_html_action_utils=>parse_fields(
          iv_string      = zif_abapgit_gui_event~mv_getdata
          iv_upper_cased = iv_upper_cased )
      CHANGING
        co_string_map_upper  = mo_query_upper_cased
        co_string_map_lower  = mo_query
        co_string_map_return = ro_string_map ).

  ENDMETHOD.
ENDCLASS.
