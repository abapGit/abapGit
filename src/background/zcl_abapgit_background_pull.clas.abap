CLASS zcl_abapgit_background_pull DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_abapgit_background .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_ABAPGIT_BACKGROUND_PULL IMPLEMENTATION.


  METHOD zif_abapgit_background~get_description.

    rv_description = 'Automatic pull'.

  ENDMETHOD.


  METHOD zif_abapgit_background~get_settings.
    RETURN.
  ENDMETHOD.


  METHOD zif_abapgit_background~run.

    DATA: ls_checks             TYPE zif_abapgit_definitions=>ty_deserialize_checks,
          lo_settings           TYPE REF TO zcl_abapgit_settings,
          lv_activation_setting TYPE zif_abapgit_definitions=>ty_s_user_settings-activate_wo_popup.

    FIELD-SYMBOLS: <ls_overwrite> LIKE LINE OF ls_checks-overwrite.


    ls_checks = io_repo->deserialize_checks( ).

    LOOP AT ls_checks-overwrite ASSIGNING <ls_overwrite>.
      <ls_overwrite>-decision = zif_abapgit_definitions=>c_yes.
    ENDLOOP.

    lo_settings = zcl_abapgit_persist_factory=>get_settings( )->read( ).
    lv_activation_setting = lo_settings->get_activate_wo_popup( ).

    lo_settings->set_activate_wo_popup( abap_true ).


    " pass decisions to delete
    zcl_abapgit_services_repo=>delete_unnecessary_objects(
      io_repo   = io_repo
      is_checks = ls_checks
      ii_log    = ii_log ).

    io_repo->deserialize( is_checks = ls_checks
                          ii_log    = ii_log ).

    lo_settings->set_activate_wo_popup( lv_activation_setting ).

  ENDMETHOD.
ENDCLASS.
