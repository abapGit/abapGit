CLASS zcl_abapgit_services_basis DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    CLASS-METHODS create_package
      IMPORTING
        iv_prefill_package TYPE devclass OPTIONAL
      RETURNING
        VALUE(rv_package)  TYPE devclass
      RAISING
        zcx_abapgit_exception.
    CLASS-METHODS test_changed_by
      RAISING
        zcx_abapgit_exception.
    CLASS-METHODS run_performance_test
      RAISING
        zcx_abapgit_exception.
    CLASS-METHODS open_ie_devtools
      RAISING
        zcx_abapgit_exception.

  PROTECTED SECTION.
  PRIVATE SECTION.
    CLASS-METHODS raise_error_if_package_exists
      IMPORTING
        iv_devclass TYPE scompkdtln-devclass
      RAISING
        zcx_abapgit_exception.
ENDCLASS.



CLASS ZCL_ABAPGIT_SERVICES_BASIS IMPLEMENTATION.


  METHOD create_package.

    DATA ls_package_data TYPE scompkdtln.
    DATA lv_create       TYPE abap_bool.
    DATA li_popup        TYPE REF TO zif_abapgit_popups.

    ls_package_data-devclass = to_upper( iv_prefill_package ).

    raise_error_if_package_exists( ls_package_data-devclass ).

    li_popup = zcl_abapgit_ui_factory=>get_popups( ).

    li_popup->popup_to_create_package(
      IMPORTING
        es_package_data = ls_package_data
        ev_create       = lv_create ).

    IF lv_create = abap_true.
      zcl_abapgit_factory=>get_sap_package( ls_package_data-devclass )->create( ls_package_data ).
      rv_package = ls_package_data-devclass.
      COMMIT WORK.
    ENDIF.

  ENDMETHOD.


  METHOD open_ie_devtools.
    DATA: lv_system_directory TYPE string,
          lv_exe_full_path    TYPE string.

    IF zcl_abapgit_ui_factory=>get_gui_functions( )->is_sapgui_for_windows( ) = abap_false.
      zcx_abapgit_exception=>raise( |IE DevTools not supported on frontend OS| ).
    ENDIF.

    cl_gui_frontend_services=>get_system_directory(
      CHANGING
        system_directory     = lv_system_directory
      EXCEPTIONS
        cntl_error           = 1
        error_no_gui         = 2
        not_supported_by_gui = 3
        OTHERS               = 4 ).
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( |Error from GET_SYSTEM_DIRECTORY sy-subrc: { sy-subrc }| ).
    ENDIF.

    cl_gui_cfw=>flush( ).

    lv_exe_full_path = lv_system_directory && `\F12\IEChooser.exe`.

    cl_gui_frontend_services=>execute(
      EXPORTING
        application            = lv_exe_full_path
      EXCEPTIONS
        cntl_error             = 1
        error_no_gui           = 2
        bad_parameter          = 3
        file_not_found         = 4
        path_not_found         = 5
        file_extension_unknown = 6
        error_execute_failed   = 7
        synchronous_failed     = 8
        not_supported_by_gui   = 9
        OTHERS                 = 10 ).
    IF sy-subrc <> 0.
      " IEChooser is only available on Windows 10
      zcx_abapgit_exception=>raise( |Error from EXECUTE sy-subrc: { sy-subrc }| ).
    ENDIF.
  ENDMETHOD.


  METHOD raise_error_if_package_exists.

    IF iv_devclass IS INITIAL.
      RETURN.
    ENDIF.

    IF zcl_abapgit_factory=>get_sap_package( iv_devclass )->exists( ) = abap_true.
      " Package &1 already exists
      MESSAGE e042(pak) INTO sy-msgli WITH iv_devclass.
      zcx_abapgit_exception=>raise_t100( ).
    ENDIF.

  ENDMETHOD.


  METHOD run_performance_test.
    DATA: lo_performance                TYPE REF TO zcl_abapgit_performance_test,
          lv_package                    TYPE devclass,
          lv_include_sub_packages       TYPE abap_bool VALUE abap_true,
          lv_serialize_master_lang_only TYPE abap_bool VALUE abap_true,
          lt_object_type_filter         TYPE zif_abapgit_definitions=>ty_object_type_range,
          lt_object_name_filter         TYPE zif_abapgit_definitions=>ty_object_name_range,
          lt_result                     TYPE zcl_abapgit_performance_test=>ty_results,
          lo_alv                        TYPE REF TO cl_salv_table,
          lx_salv_error                 TYPE REF TO cx_salv_error,
          lv_current_repo               TYPE zif_abapgit_persistence=>ty_value,
          lo_runtime_column             TYPE REF TO cl_salv_column,
          lo_seconds_column             TYPE REF TO cl_salv_column,
          li_popups                     TYPE REF TO zif_abapgit_popups.

    TRY.
        lv_current_repo = zcl_abapgit_persistence_user=>get_instance( )->get_repo_show( ).
        IF lv_current_repo IS NOT INITIAL.
          lv_package = zcl_abapgit_repo_srv=>get_instance( )->get( lv_current_repo )->get_package( ).
        ENDIF.
      CATCH zcx_abapgit_exception ##NO_HANDLER.
    ENDTRY.

    li_popups = zcl_abapgit_ui_factory=>get_popups( ).
    li_popups->popup_perf_test_parameters(
      IMPORTING
        et_object_type_filter         = lt_object_type_filter
        et_object_name_filter         = lt_object_name_filter
      CHANGING
        cv_package                    = lv_package
        cv_include_sub_packages       = lv_include_sub_packages
        cv_serialize_master_lang_only = lv_serialize_master_lang_only ).

    CREATE OBJECT lo_performance
      EXPORTING
        iv_package                    = lv_package
        iv_include_sub_packages       = lv_include_sub_packages
        iv_serialize_master_lang_only = lv_serialize_master_lang_only.


    lo_performance->set_object_type_filter( lt_object_type_filter ).
    lo_performance->set_object_name_filter( lt_object_name_filter ).

    lo_performance->run_measurement( ).

    lt_result = lo_performance->get_result( ).

    TRY.
        cl_salv_table=>factory(
          IMPORTING
            r_salv_table = lo_alv
          CHANGING
            t_table      = lt_result ).
        lo_alv->get_functions( )->set_all( ).
        lo_alv->get_display_settings( )->set_list_header( 'Serialization Performance Test Results' ).
        lo_runtime_column = lo_alv->get_columns( )->get_column( 'RUNTIME' ).
        lo_runtime_column->set_medium_text( 'Runtime' ).
        lo_runtime_column->set_visible( abap_false ).
        lo_seconds_column = lo_alv->get_columns( )->get_column( 'SECONDS' ).
        lo_seconds_column->set_medium_text( 'Seconds' ).
        lo_alv->get_columns( )->set_count_column( 'COUNTER' ).
        lo_alv->get_aggregations( )->add_aggregation( lo_runtime_column->get_columnname( ) ).
        lo_alv->get_aggregations( )->add_aggregation( lo_seconds_column->get_columnname( ) ).
        lo_alv->set_screen_popup(
          start_column = 1
          end_column   = 180
          start_line   = 1
          end_line     = 25 ).
        lo_alv->display( ).
      CATCH cx_salv_error INTO lx_salv_error.
        zcx_abapgit_exception=>raise(
          iv_text     = lx_salv_error->get_text( )
          ix_previous = lx_salv_error ).
    ENDTRY.
  ENDMETHOD.


  METHOD test_changed_by.

    DATA ls_tadir TYPE zif_abapgit_definitions=>ty_tadir.
    DATA ls_item  TYPE zif_abapgit_definitions=>ty_item.
    DATA lv_user  TYPE xubname.

    ls_tadir = zcl_abapgit_ui_factory=>get_popups( )->popup_object( ).
    IF ls_tadir IS INITIAL.
      RETURN.
    ENDIF.

    ls_item-obj_type = ls_tadir-object.
    ls_item-obj_name = ls_tadir-obj_name.

    lv_user = zcl_abapgit_objects=>changed_by( ls_item ).

    MESSAGE lv_user TYPE 'S'.

  ENDMETHOD.
ENDCLASS.
