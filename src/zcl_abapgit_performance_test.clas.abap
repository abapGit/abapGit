"! Performance test run
CLASS zcl_abapgit_performance_test DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    TYPES:
      BEGIN OF ty_result,
        pgmid    TYPE pgmid,
        object   TYPE trobjtype,
        obj_name TYPE sobj_name,
        devclass TYPE devclass,
        counter  TYPE i,
        runtime  TYPE i,
        seconds  TYPE p LENGTH 16 DECIMALS 6,
      END OF ty_result,
      ty_results TYPE STANDARD TABLE OF ty_result WITH KEY pgmid object obj_name.
    METHODS:
      constructor IMPORTING iv_package                    TYPE devclass
                            iv_include_sub_packages       TYPE abap_bool DEFAULT abap_true
                            iv_serialize_master_lang_only TYPE abap_bool DEFAULT abap_true,
      set_object_type_filter IMPORTING it_object_type_range TYPE zif_abapgit_definitions=>ty_object_type_range,
      set_object_name_filter IMPORTING it_object_name_range TYPE zif_abapgit_definitions=>ty_object_name_range,
      get_object_type_filter RETURNING VALUE(rt_object_type_range) TYPE zif_abapgit_definitions=>ty_object_type_range,
      get_object_name_filter RETURNING VALUE(rt_object_name_range) TYPE zif_abapgit_definitions=>ty_object_name_range,
      run_measurement RAISING zcx_abapgit_exception,
      get_result RETURNING VALUE(rt_result) TYPE ty_results.
  PROTECTED SECTION.
  PRIVATE SECTION.
    METHODS:
      select_tadir_entries RETURNING VALUE(rt_tadir) TYPE zif_abapgit_definitions=>ty_tadir_tt
                           RAISING   zcx_abapgit_exception.
    DATA:
      mv_package                    TYPE devclass,
      mv_include_sub_packages       TYPE abap_bool,
      mv_serialize_master_lang_only TYPE abap_bool,
      BEGIN OF ms_filter_parameters,
        object_type_range TYPE zif_abapgit_definitions=>ty_object_type_range,
        object_name_range TYPE zif_abapgit_definitions=>ty_object_name_range,
      END OF ms_filter_parameters,
      mt_result TYPE ty_results.
ENDCLASS.



CLASS ZCL_ABAPGIT_PERFORMANCE_TEST IMPLEMENTATION.


  METHOD constructor.
    mv_package = iv_package.
    mv_include_sub_packages = iv_include_sub_packages.
    mv_serialize_master_lang_only = iv_serialize_master_lang_only.
  ENDMETHOD.


  METHOD get_object_name_filter.
    rt_object_name_range = ms_filter_parameters-object_name_range.
  ENDMETHOD.


  METHOD get_object_type_filter.
    rt_object_type_range = ms_filter_parameters-object_type_range.
  ENDMETHOD.


  METHOD get_result.
    rt_result = mt_result.
  ENDMETHOD.


  METHOD run_measurement.
    DATA: li_actual_progress TYPE REF TO zif_abapgit_progress,
          lt_tadir           TYPE zif_abapgit_definitions=>ty_tadir_tt,
          lt_tadir_single    TYPE zif_abapgit_definitions=>ty_tadir_tt,
          lo_serializer      TYPE REF TO zcl_abapgit_serialize,
          lv_start_runtime   TYPE i,
          lv_end_runtime     TYPE i,
          lx_exception       TYPE REF TO zcx_abapgit_exception,
          lo_dummy_progress  TYPE REF TO lcl_dummy_progress.
    FIELD-SYMBOLS: <ls_tadir>  TYPE zif_abapgit_definitions=>ty_tadir,
                   <ls_result> TYPE ty_result.

    CLEAR mt_result.

    li_actual_progress = zcl_abapgit_progress=>get_instance( 1 ).
    CREATE OBJECT lo_dummy_progress.
    zcl_abapgit_progress=>set_instance( lo_dummy_progress ).

    TRY.
        lt_tadir = select_tadir_entries( ).

        CREATE OBJECT lo_serializer
          EXPORTING
            iv_serialize_master_lang_only = mv_serialize_master_lang_only.

        LOOP AT lt_tadir ASSIGNING <ls_tadir>.
          INSERT <ls_tadir> INTO TABLE lt_tadir_single.

          GET RUN TIME FIELD lv_start_runtime.

          lo_serializer->serialize(
            it_tadir            = lt_tadir_single
            iv_force_sequential = abap_true ).

          GET RUN TIME FIELD lv_end_runtime.

          APPEND INITIAL LINE TO mt_result ASSIGNING <ls_result>.
          <ls_result>-pgmid = <ls_tadir>-pgmid.
          <ls_result>-object = <ls_tadir>-object.
          <ls_result>-obj_name = <ls_tadir>-obj_name.
          <ls_result>-devclass = <ls_tadir>-devclass.
          <ls_result>-runtime = lv_end_runtime - lv_start_runtime.
          <ls_result>-seconds = <ls_result>-runtime / 1000000.

          CLEAR lt_tadir_single.
        ENDLOOP.

      CATCH zcx_abapgit_exception INTO lx_exception.
        zcl_abapgit_progress=>set_instance( li_actual_progress ).
        RAISE EXCEPTION lx_exception.
    ENDTRY.
  ENDMETHOD.


  METHOD select_tadir_entries.
    rt_tadir = zcl_abapgit_factory=>get_tadir( )->read(
      iv_package            = mv_package
      iv_ignore_subpackages = boolc( mv_include_sub_packages = abap_false ) ).

    DELETE rt_tadir WHERE object NOT IN ms_filter_parameters-object_type_range
                       OR obj_name NOT IN ms_filter_parameters-object_name_range.
  ENDMETHOD.


  METHOD set_object_name_filter.
    ms_filter_parameters-object_name_range = it_object_name_range.
  ENDMETHOD.


  METHOD set_object_type_filter.
    ms_filter_parameters-object_type_range = it_object_type_range.
  ENDMETHOD.
ENDCLASS.
