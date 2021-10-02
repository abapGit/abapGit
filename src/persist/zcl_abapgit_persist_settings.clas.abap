CLASS zcl_abapgit_persist_settings DEFINITION
  PUBLIC
  CREATE PRIVATE
  GLOBAL FRIENDS zcl_abapgit_persist_factory .

  PUBLIC SECTION.

    INTERFACES zif_abapgit_persist_settings .
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA mo_settings TYPE REF TO zcl_abapgit_settings .

ENDCLASS.



CLASS ZCL_ABAPGIT_PERSIST_SETTINGS IMPLEMENTATION.


  METHOD zif_abapgit_persist_settings~modify.

    DATA: lv_settings      TYPE string,
          ls_user_settings TYPE zif_abapgit_definitions=>ty_s_user_settings.


    lv_settings = io_settings->get_settings_xml( ).

    zcl_abapgit_persistence_db=>get_instance( )->modify(
      iv_type       = zcl_abapgit_persistence_db=>c_type_settings
      iv_value      = ''
      iv_data       = lv_settings ).

    ls_user_settings = io_settings->get_user_settings( ).

    zcl_abapgit_persistence_user=>get_instance( )->set_settings( ls_user_settings ).

    " Settings have been modified: Update Buffered Settings
    IF mo_settings IS BOUND.
      mo_settings->set_xml_settings( lv_settings ).
      mo_settings->set_user_settings( ls_user_settings ).
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_persist_settings~read.

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

        ro_settings->set_user_settings( zcl_abapgit_persistence_user=>get_instance( )->get_settings( ) ).

      CATCH zcx_abapgit_not_found zcx_abapgit_exception.

        ro_settings->set_defaults( ).

    ENDTRY.

    mo_settings = ro_settings.

  ENDMETHOD.
ENDCLASS.
