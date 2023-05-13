CLASS zcl_abapgit_gui_in_page_modal DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    CONSTANTS:
      BEGIN OF c_event,
          ok     TYPE string VALUE 'popup-ok',
          cancel TYPE string VALUE 'popup-cancel',
        END OF c_event .

    METHODS constructor
      IMPORTING
        !iv_form_id TYPE string
        !iv_class TYPE string DEFAULT 'modal-dialog'
        !io_form TYPE REF TO zcl_abapgit_html_form .
    METHODS set_dimensions
      IMPORTING
        !iv_width TYPE i OPTIONAL
        !iv_height TYPE i OPTIONAL .
    METHODS render
      RETURNING
        VALUE(ri_html) TYPE REF TO zif_abapgit_html
      RAISING
        zcx_abapgit_exception .
    METHODS normalize
      IMPORTING
        !io_form_data TYPE REF TO zcl_abapgit_string_map
      RAISING
        zcx_abapgit_exception .
    METHODS validate
      RAISING
        zcx_abapgit_exception .
    METHODS clear
      RAISING
        zcx_abapgit_exception .
    METHODS is_valid
      RETURNING
        VALUE(rv_valid) TYPE string .
    METHODS get_value
      IMPORTING
        !iv_key TYPE string
      RETURNING
        VALUE(rv_val) TYPE string .
    METHODS get_values
      IMPORTING
        !iv_key TYPE string
      RETURNING
        VALUE(rt_val) TYPE string_table .
    METHODS get_form_id
      RETURNING
        VALUE(rv_id) TYPE string .
  PROTECTED SECTION.

    METHODS add_validation_error
      IMPORTING
        !iv_key  TYPE string
        !iv_text TYPE string
      RAISING
        zcx_abapgit_exception.

    METHODS get_form_values
      IMPORTING
        !iv_val       TYPE string
      RETURNING
        VALUE(rt_val) TYPE string_table.

  PRIVATE SECTION.

    DATA:
      BEGIN OF ms_form,
        id     TYPE string,
        class  TYPE string,
        width  TYPE i,
        height TYPE i,
      END OF ms_form.

    DATA mo_form TYPE REF TO zcl_abapgit_html_form.
    DATA mo_form_util TYPE REF TO zcl_abapgit_html_form_utils.
    DATA mo_form_data TYPE REF TO zcl_abapgit_string_map.
    DATA mo_validation_log TYPE REF TO zcl_abapgit_string_map.

ENDCLASS.



CLASS ZCL_ABAPGIT_GUI_IN_PAGE_MODAL IMPLEMENTATION.


  METHOD add_validation_error.
    mo_validation_log->set(
      iv_key = |{ ms_form-id }-{ iv_key }|
      iv_val = iv_text ).
  ENDMETHOD.


  METHOD clear.
    mo_form_data->clear( ).
  ENDMETHOD.


  METHOD constructor.
    ms_form-id     = iv_form_id.
    ms_form-class  = iv_class.
    ms_form-width  = 680.
    ms_form-height = 300.

    mo_form        = io_form.
    mo_form_util   = zcl_abapgit_html_form_utils=>create( io_form ).

    CREATE OBJECT mo_form_data.
    CREATE OBJECT mo_validation_log.
  ENDMETHOD.


  METHOD get_form_id.
    rv_id = ms_form-id.
  ENDMETHOD.


  METHOD get_form_values.
    " Redefine to return multiple values
    INSERT iv_val INTO TABLE rt_val.
  ENDMETHOD.


  METHOD get_value.
    " Returns single form value
    rv_val = mo_form_data->get( |{ ms_form-id }-{ iv_key }| ).
  ENDMETHOD.


  METHOD get_values.
    " Return multiple values based on form input
    rt_val = get_form_values( get_value( iv_key ) ).
  ENDMETHOD.


  METHOD is_valid.
    rv_valid = mo_validation_log->is_empty( ).
  ENDMETHOD.


  METHOD normalize.
    IF mo_form IS INITIAL.
      RETURN.
    ENDIF.

    mo_form_data = mo_form_util->normalize( io_form_data ).
  ENDMETHOD.


  METHOD render.

    DATA lv_style TYPE string.

    CREATE OBJECT ri_html TYPE zcl_abapgit_html.

    IF mo_form IS INITIAL.
      RETURN.
    ENDIF.

    lv_style = |width:{ ms_form-width }px; height:{ ms_form-height }px;|.

    ri_html->add( |<div class="modal" id="{ ms_form-id }" style="{ lv_style }">| ).
    ri_html->add( |<div class="modal-guts">| ).
    ri_html->add( mo_form->render(
                    iv_form_class     = ms_form-class
                    io_values         = mo_form_data
                    io_validation_log = mo_validation_log ) ).
    ri_html->add( |</div>| ).
    ri_html->add( |</div>| ).
    ri_html->add( |<div class="modal-overlay" id="{ ms_form-id }-overlay"></div>| ).

    mo_validation_log->clear( ).

  ENDMETHOD.


  METHOD set_dimensions.
    IF iv_width IS NOT INITIAL.
      ms_form-width  = iv_width.
    ENDIF.
    IF iv_height IS NOT INITIAL.
      ms_form-height = iv_height.
    ENDIF.
  ENDMETHOD.


  METHOD validate.
    " Redefine to add own validation rules
    mo_validation_log = mo_form_util->validate( mo_form_data ).
  ENDMETHOD.
ENDCLASS.
