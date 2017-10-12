*&---------------------------------------------------------------------*
*& Include zabapgit_object_sxci
*&---------------------------------------------------------------------*

CLASS lcl_object_sxci DEFINITION INHERITING FROM lcl_objects_super FINAL.

  PUBLIC SECTION.
    INTERFACES lif_object.

ENDCLASS.

CLASS lcl_object_sxci IMPLEMENTATION.

  METHOD lif_object~has_changed_since.

    rv_changed = abap_true.

  ENDMETHOD.

  METHOD lif_object~changed_by.

    rv_user = c_user_unknown.

  ENDMETHOD.

  METHOD lif_object~get_metadata.

    rs_metadata = get_metadata( ).

  ENDMETHOD.

  METHOD lif_object~exists.

    DATA: lv_imp_name TYPE rsexscrn-imp_name.

    CALL FUNCTION 'SXV_IMP_EXISTS'
      EXPORTING
        imp_name           = lv_imp_name
      EXCEPTIONS
        not_existing       = 1
        data_inconsistency = 2
        OTHERS             = 3.

    rv_bool = boolc( sy-subrc = 0 ).

  ENDMETHOD.

  METHOD lif_object~serialize.

    DATA: lv_imp_name          TYPE rsexscrn-imp_name,
          lv_exit_name         TYPE rsexscrn-exit_name,
          lo_filter_obj        TYPE REF TO cl_badi_flt_struct,
          lv_ext_clname        TYPE seoclsname,
          ls_badi              TYPE badi_data,
          ls_impl              TYPE impl_data,
          lv_mast_langu        TYPE sy-langu,
          lo_filter_values_obj TYPE REF TO cl_badi_flt_values_alv,
          lt_fcodes            TYPE seex_fcode_table,
          lt_cocos             TYPE seex_coco_table,
          lt_intas             TYPE seex_table_table,
          lt_scrns             TYPE seex_screen_table,
          lt_methods           TYPE seex_mtd_table,
          lt_filters           TYPE seex_filter_table.

    lv_imp_name = ms_item-obj_name.

    CALL FUNCTION 'SXV_EXIT_FOR_IMP'
      EXPORTING
        imp_name           = lv_imp_name
      IMPORTING
        exit_name          = lv_exit_name
      TABLES
        filters            = lt_filters
      EXCEPTIONS
        data_inconsistency = 1
        OTHERS             = 2.

    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( 'error from SXV_EXIT_FOR_IMP' ).
    ENDIF.

    CALL FUNCTION 'SXO_BADI_READ'
      EXPORTING
        exit_name    = lv_exit_name    " Enhancement Name
      IMPORTING
        badi         = ls_badi
*       mast_langu   =     " SAP R/3 System, Current Language
        ext_clname   = lv_ext_clname    " Object Type Name
        filter_obj   = lo_filter_obj
*      TABLES
*       fcodes       =
*       cocos        =
*       intas        =
*       scrns        =
*       methods      =
*       inactive_tabstrips =
      EXCEPTIONS
        read_failure = 1
        OTHERS       = 2.

    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( 'error from SXV_EXIT_FOR_IMP' ).
    ENDIF.

    CALL FUNCTION 'SXO_IMPL_FOR_BADI_READ'
      EXPORTING
        imp_name          = lv_imp_name     " Implementation name for an enhancement
        exit_name         = lv_exit_name    " Enhancement Name
*       maint_langu       = SY-LANGU    " SAP R/3 System, Current Language
        inter_name        = ls_badi-inter_name     " Interface Name
        filter_obj        = lo_filter_obj    " Manage Filter Type Structures for Business Add-Ins
*       no_create_filter_values_obj =
      IMPORTING
        impl              = ls_impl
        mast_langu        = lv_mast_langu
        filter_values_obj = lo_filter_values_obj    " Manage Filter Values in ALV Grid for Business Add-Ins
      TABLES
        fcodes            = lt_fcodes
        cocos             = lt_cocos
        intas             = lt_intas
        scrns             = lt_scrns
      CHANGING
        methods           = lt_methods
      EXCEPTIONS
        read_failure      = 1
        OTHERS            = 2.

    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( 'error from SXV_EXIT_FOR_IMP' ).
    ENDIF.

    CLEAR: ls_impl-aname,
           ls_impl-adate,
           ls_impl-atime,
           ls_impl-uname,
           ls_impl-udate,
           ls_impl-utime.

    io_xml->add( iv_name = 'SXCI'
                 ig_data = ls_impl ).

    io_xml->add( iv_name = 'FILTERS'
                 ig_data = lt_filters ).

  ENDMETHOD.

  METHOD lif_object~deserialize.

    DATA: ls_impl           TYPE impl_data,
          lt_filters        TYPE seex_filter_table,
          ls_badi           TYPE badi_data,
          lv_mast_langu     TYPE sy-langu,
          lv_ext_clname     TYPE seoclsname,
          lo_filter_obj     TYPE REF TO cl_badi_flt_struct,
          lo_filter_val_obj TYPE REF TO cl_badi_flt_values_alv,
          lv_korrnum        TYPE trkorr,
          flt_ext           TYPE rsexscrn-flt_ext,
          lv_package        TYPE devclass.

    io_xml->read(
      EXPORTING
        iv_name = 'SXCI'
      CHANGING
        cg_data = ls_impl ).

    io_xml->read(
      EXPORTING
        iv_name = 'FILTERS'
      CHANGING
        cg_data = lt_filters ).

    CALL FUNCTION 'SXO_BADI_READ'
      EXPORTING
        exit_name    = ls_impl-exit_name    " Enhancement Name
      IMPORTING
        badi         = ls_badi
*       mast_langu   =     " SAP R/3 System, Current Language
        ext_clname   = lv_ext_clname    " Object Type Name
        filter_obj   = lo_filter_obj
*      TABLES
*       fcodes       =
*       cocos        =
*       intas        =
*       scrns        =
*       methods      =
*       inactive_tabstrips =
      EXCEPTIONS
        read_failure = 1
        OTHERS       = 2.

    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( 'error from SXO_BADI_READ' ).
    ENDIF.

    lv_package = iv_package.

    CREATE OBJECT lo_filter_val_obj
      EXPORTING
        filter_object = lo_filter_obj    " Manage Filter Type Structures for Business Add-Ins
        filter_values = lt_filters    " Filter Values
*       for_flt_val_creation   =     " Create Filter Values in Dialog Box
*       for_overview  =     " Overview Display of All Implementations
*       filter_values_for_over =     " Filter Values with All Implementations
*       fieldcatalog  =     " Field Catalog for List Viewer Control
      .

    CALL FUNCTION 'SXO_IMPL_SAVE'
      EXPORTING
        impl            = ls_impl
        flt_ext         = flt_ext    " Alternative
*       flt_type        =     " Data Element (Semantic Domain)
*       maint_langu     = SY-LANGU    " SAP R/3 System, Current Language
        filter_val_obj  = lo_filter_val_obj    " Manage Filter Values in ALV Grid for Business Add-Ins
        genflag         = abap_true     " Generation Flag
        no_dialog       = abap_true     " No dialogs
*  IMPORTING
*       mast_langu      =     " SAP R/3 System, Current Language
*  TABLES
*       fcodes_to_insert =
*       cocos_to_insert =
*       intas_to_insert =
*       sscrs_to_insert =
      CHANGING
        korrnum         = lv_korrnum
        devclass        = lv_package    " Development class for Change and Transport Organizer
      EXCEPTIONS
        save_failure    = 1
        action_canceled = 2
        OTHERS          = 3.

    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( 'error from SXO_IMPL_SAVE' ).
    ENDIF.

    CALL FUNCTION 'SXO_IMPL_ACTIVE'
      EXPORTING
        imp_name                  = ls_impl-imp_name    " Implementation name for an enhancement
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

  METHOD lif_object~delete.

    DATA: lv_imp_name TYPE rsexscrn-imp_name.

    lv_imp_name = ms_item-obj_name.

    CALL FUNCTION 'SXO_IMPL_DELETE'
      EXPORTING
        imp_name           = lv_imp_name
        no_dialog          = abap_true
*      CHANGING
*       korr_num           =     " Correction Number
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


  METHOD lif_object~jump.

    " SXO_IMPL_SHOW

    CALL FUNCTION 'RS_TOOL_ACCESS'
      EXPORTING
        operation           = 'SHOW'
        object_name         = ms_item-obj_name    " Object Name
        object_type         = ms_item-obj_type    " Object Type
        in_new_window       = abap_true
      EXCEPTIONS
        not_executed        = 1
        invalid_object_type = 2
        OTHERS              = 3.

    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( 'error from RS_TOOL_ACCESS' ).
    ENDIF.

  ENDMETHOD.

  METHOD lif_object~compare_to_remote_version.

    CREATE OBJECT ro_comparison_result TYPE lcl_comparison_null.

  ENDMETHOD.

ENDCLASS.
