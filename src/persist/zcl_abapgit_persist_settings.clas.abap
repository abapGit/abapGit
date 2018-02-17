CLASS zcl_abapgit_persist_settings DEFINITION
  PUBLIC
  CREATE PRIVATE .

  PUBLIC SECTION.

    METHODS modify
      IMPORTING
        !io_settings TYPE REF TO zcl_abapgit_settings
      RAISING
        zcx_abapgit_exception .
    METHODS read
      RETURNING
        VALUE(ro_settings) TYPE REF TO zcl_abapgit_settings .
    CLASS-METHODS get_instance
      RETURNING
        VALUE(ro_settings) TYPE REF TO zcl_abapgit_persist_settings .
  PRIVATE SECTION.

    DATA mo_settings TYPE REF TO zcl_abapgit_settings .
    CLASS-DATA go_persist TYPE REF TO zcl_abapgit_persist_settings .
ENDCLASS.



CLASS ZCL_ABAPGIT_PERSIST_SETTINGS IMPLEMENTATION.


  METHOD get_instance.

    IF go_persist IS NOT BOUND.
      CREATE OBJECT go_persist.
    ENDIF.
    ro_settings = go_persist.

  ENDMETHOD.


  METHOD modify.

    DATA: lv_settings TYPE string.


    lv_settings = io_settings->get_settings_xml( ).

    zcl_abapgit_persistence_db=>get_instance( )->modify(
      iv_type       = zcl_abapgit_persistence_db=>c_type_settings
      iv_value      = ''
      iv_data       = lv_settings ).

    " Settings have been modified: Update Buffered Settings
    IF mo_settings IS BOUND.
      mo_settings->set_xml_settings( lv_settings ).
    ENDIF.

  ENDMETHOD.


  METHOD read.

    IF mo_settings IS BOUND.
      " Return Buffered Settings
      ro_settings = mo_settings.
      RETURN.
    ENDIF.

    " Settings have changed or have not yet been loaded
    CREATE OBJECT ro_settings.

    TRY.

        ro_settings->set_xml_settings(
          zcl_abapgit_persistence_db=>get_instance( )->read(
            iv_type  = zcl_abapgit_persistence_db=>c_type_settings
            iv_value = '' ) ).

      CATCH zcx_abapgit_not_found zcx_abapgit_exception.

        ro_settings->set_defaults( ).

    ENDTRY.

    mo_settings = ro_settings.

  ENDMETHOD.
ENDCLASS.
