CLASS zcl_abapgit_object_sxci DEFINITION PUBLIC INHERITING FROM zcl_abapgit_objects_super FINAL.

  PUBLIC SECTION.
    INTERFACES zif_abapgit_object.

  PRIVATE SECTION.
    TYPES: BEGIN OF ty_classic_badi_implementation,
             implementation_data TYPE impl_data,
             function_codes      TYPE seex_fcode_table,
             control_composites  TYPE seex_coco_table,
             customer_includes   TYPE seex_table_table,
             screens             TYPE seex_screen_table,
             filters             TYPE seex_filter_table,
           END OF ty_classic_badi_implementation.
ENDCLASS.



CLASS ZCL_ABAPGIT_OBJECT_SXCI IMPLEMENTATION.


  METHOD zif_abapgit_object~changed_by.

    rv_user = c_user_unknown.

  ENDMETHOD.


  METHOD zif_abapgit_object~compare_to_remote_version.

    CREATE OBJECT ro_comparison_result TYPE zcl_abapgit_comparison_null.

  ENDMETHOD.


  METHOD zif_abapgit_object~delete.

    DATA: lv_implementation_name TYPE rsexscrn-imp_name.

    lv_implementation_name = ms_item-obj_name.

    CALL FUNCTION 'SXO_IMPL_DELETE'
      EXPORTING
        imp_name           = lv_implementation_name
        no_dialog          = abap_true
      EXCEPTIONS
        imp_not_existing   = 1
        action_canceled    = 2
        access_failure     = 3
        data_inconsistency = 4
        OTHERS             = 5.

    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( 'error from SXO_IMPL_DELETE' ).
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_object~deserialize.

    DATA: ls_badi_definition             TYPE badi_data,
          lo_filter_object               TYPE REF TO cl_badi_flt_struct,
          lo_filter_values_object        TYPE REF TO cl_badi_flt_values_alv,
          lv_korrnum                     TYPE trkorr,
          lv_filter_type_enhanceability  TYPE rsexscrn-flt_ext,
          lv_package                     TYPE devclass,
          ls_classic_badi_implementation TYPE ty_classic_badi_implementation.

    io_xml->read(
      EXPORTING
        iv_name = 'SXCI'
      CHANGING
        cg_data = ls_classic_badi_implementation ).

    CALL FUNCTION 'SXO_BADI_READ'
      EXPORTING
        exit_name    = ls_classic_badi_implementation-implementation_data-exit_name
      IMPORTING
        badi         = ls_badi_definition
        filter_obj   = lo_filter_object
      EXCEPTIONS
        read_failure = 1
        OTHERS       = 2.

    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( 'error from SXO_BADI_READ' ).
    ENDIF.

    lv_package = iv_package.

    CREATE OBJECT lo_filter_values_object
      EXPORTING
        filter_object = lo_filter_object
        filter_values = ls_classic_badi_implementation-filters.

    CALL FUNCTION 'SXO_IMPL_SAVE'
      EXPORTING
        impl             = ls_classic_badi_implementation-implementation_data
        flt_ext          = lv_filter_type_enhanceability
        filter_val_obj   = lo_filter_values_object
        genflag          = abap_true
        no_dialog        = abap_true
      TABLES
        fcodes_to_insert = ls_classic_badi_implementation-function_codes
        cocos_to_insert  = ls_classic_badi_implementation-control_composites
        intas_to_insert  = ls_classic_badi_implementation-customer_includes
        sscrs_to_insert  = ls_classic_badi_implementation-screens
      CHANGING
        korrnum          = lv_korrnum
        devclass         = lv_package
      EXCEPTIONS
        save_failure     = 1
        action_canceled  = 2
        OTHERS           = 3.

    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( 'error from SXO_IMPL_SAVE' ).
    ENDIF.

    CALL FUNCTION 'SXO_IMPL_ACTIVE'
      EXPORTING
        imp_name                  = ls_classic_badi_implementation-implementation_data-imp_name
        no_dialog                 = abap_true
      EXCEPTIONS
        badi_not_existing         = 1
        imp_not_existing          = 2
        already_active            = 3
        data_inconsistency        = 4
        activation_not_admissable = 5
        action_canceled           = 6
        access_failure            = 7
        OTHERS                    = 8.

    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( 'error from SXO_IMPL_ACTIVE' ).
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_object~exists.

    DATA: lv_implementation_name TYPE rsexscrn-imp_name.

    lv_implementation_name = ms_item-obj_name.

    CALL FUNCTION 'SXV_IMP_EXISTS'
      EXPORTING
        imp_name           = lv_implementation_name
      EXCEPTIONS
        not_existing       = 1
        data_inconsistency = 2
        OTHERS             = 3.

    rv_bool = boolc( sy-subrc = 0 ).

  ENDMETHOD.


  METHOD zif_abapgit_object~get_metadata.

    rs_metadata = get_metadata( ).

  ENDMETHOD.


  METHOD zif_abapgit_object~has_changed_since.

    rv_changed = abap_true.

  ENDMETHOD.


  METHOD zif_abapgit_object~jump.

    CALL FUNCTION 'RS_TOOL_ACCESS'
      EXPORTING
        operation           = 'SHOW'
        object_name         = ms_item-obj_name
        object_type         = ms_item-obj_type
        in_new_window       = abap_true
      EXCEPTIONS
        not_executed        = 1
        invalid_object_type = 2
        OTHERS              = 3.

    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( 'error from RS_TOOL_ACCESS' ).
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_object~serialize.

    DATA: lv_implementation_name         TYPE rsexscrn-imp_name,
          lv_exit_name                   TYPE rsexscrn-exit_name,
          lo_filter_object               TYPE REF TO cl_badi_flt_struct,
          ls_badi_definition             TYPE badi_data,
          lo_filter_values_object        TYPE REF TO cl_badi_flt_values_alv,
          lt_methods                     TYPE seex_mtd_table,
          ls_classic_badi_implementation TYPE ty_classic_badi_implementation.

    lv_implementation_name = ms_item-obj_name.

    CALL FUNCTION 'SXV_EXIT_FOR_IMP'
      EXPORTING
        imp_name           = lv_implementation_name
      IMPORTING
        exit_name          = lv_exit_name
      TABLES
        filters            = ls_classic_badi_implementation-filters
      EXCEPTIONS
        data_inconsistency = 1
        OTHERS             = 2.

    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( 'error from SXV_EXIT_FOR_IMP' ).
    ENDIF.

    CALL FUNCTION 'SXO_BADI_READ'
      EXPORTING
        exit_name    = lv_exit_name
      IMPORTING
        badi         = ls_badi_definition
        filter_obj   = lo_filter_object
      TABLES
        fcodes       = ls_classic_badi_implementation-function_codes
        cocos        = ls_classic_badi_implementation-control_composites
        intas        = ls_classic_badi_implementation-customer_includes
        scrns        = ls_classic_badi_implementation-screens
        methods      = lt_methods
      EXCEPTIONS
        read_failure = 1
        OTHERS       = 2.

    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( 'error from SXO_BADI_READ' ).
    ENDIF.

    CALL FUNCTION 'SXO_IMPL_FOR_BADI_READ'
      EXPORTING
        imp_name                    = lv_implementation_name
        exit_name                   = lv_exit_name
        inter_name                  = ls_badi_definition-inter_name
        filter_obj                  = lo_filter_object
        no_create_filter_values_obj = abap_true
      IMPORTING
        impl                        = ls_classic_badi_implementation-implementation_data
        filter_values_obj           = lo_filter_values_object
      TABLES
        fcodes                      = ls_classic_badi_implementation-function_codes
        cocos                       = ls_classic_badi_implementation-control_composites
        intas                       = ls_classic_badi_implementation-customer_includes
        scrns                       = ls_classic_badi_implementation-screens
      CHANGING
        methods                     = lt_methods
      EXCEPTIONS
        read_failure                = 1
        OTHERS                      = 2.

    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( 'error from SXO_IMPL_FOR_BADI_READ' ).
    ENDIF.

    CLEAR: ls_classic_badi_implementation-implementation_data-aname,
           ls_classic_badi_implementation-implementation_data-adate,
           ls_classic_badi_implementation-implementation_data-atime,
           ls_classic_badi_implementation-implementation_data-uname,
           ls_classic_badi_implementation-implementation_data-udate,
           ls_classic_badi_implementation-implementation_data-utime,
           ls_classic_badi_implementation-implementation_data-active.

    io_xml->add( iv_name = 'SXCI'
                 ig_data = ls_classic_badi_implementation ).

  ENDMETHOD.
ENDCLASS.
