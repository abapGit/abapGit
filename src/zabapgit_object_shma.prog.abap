*&---------------------------------------------------------------------*
*&  Include  zabapgit_object_shma
*&---------------------------------------------------------------------*

CLASS lcl_object_shma DEFINITION INHERITING FROM lcl_objects_super FINAL.

  PUBLIC SECTION.
    INTERFACES lif_object.

  PRIVATE SECTION.
    TYPES: db_startup_table     TYPE STANDARD TABLE OF shma_start      WITH DEFAULT KEY,
           db_startup_table_rts TYPE STANDARD TABLE OF shma_start_rts  WITH DEFAULT KEY.

    TYPES:
      BEGIN OF change_info,
        defined     TYPE shma_chg_info,
        rts         TYPE shma_chg_info,
        startup     TYPE shma_chg_info,
        startup_rts TYPE shma_chg_info,
      END OF change_info.

ENDCLASS.

CLASS lcl_object_shma IMPLEMENTATION.

  METHOD lif_object~has_changed_since.

    rv_changed = abap_true.

  ENDMETHOD.

  METHOD lif_object~changed_by.

    rv_user = c_user_unknown.

  ENDMETHOD.

  METHOD lif_object~get_metadata.

    rs_metadata = get_metadata( ).
    rs_metadata-delete_tadir = abap_true.

  ENDMETHOD.

  METHOD lif_object~exists.

    DATA: clskey TYPE seoclskey.

    clskey = ms_item-obj_name.

    CALL FUNCTION 'SEO_CLASS_EXISTENCE_CHECK'
      EXPORTING
        clskey        = clskey
      EXCEPTIONS
        not_specified = 1
        not_existing  = 2
        is_interface  = 3
        no_text       = 4
        inconsistent  = 5
        OTHERS        = 6.

    rv_bool = boolc( sy-subrc = 0 ).

  ENDMETHOD.

  METHOD lif_object~serialize.

    DATA: area_name           TYPE shm_area_name,
          area_attributes     TYPE shma_attributes,
          area_attributes_rts TYPE shma_rts_table,
          oo_attributes       TYPE seoo_attributes_r,
          startup_list        TYPE db_startup_table,
          startup_list_rts    TYPE db_startup_table_rts,
          change_info         TYPE change_info,
          tadir_shma          TYPE tadir,
          tadir_class         TYPE tadir.

    area_name = ms_item-obj_name.

    TRY.
        CALL METHOD ('\PROGRAM=SAPLSHMA\CLASS=LCL_SHMA_HELPER')=>('READ_AREA_ATTRIBUTES_ALL')
          EXPORTING
            area_name           = area_name
          IMPORTING
            area_attributes     = area_attributes
            area_attributes_rts = area_attributes_rts
            oo_attributes       = oo_attributes
            startup_list        = startup_list
            startup_list_rts    = startup_list_rts
            change_info         = change_info
            tadir_shma          = tadir_shma
            tadir_class         = tadir_class.

        io_xml->add( iv_name = 'AREA_ATTRIBUTES'
                     ig_data = area_attributes ).

        io_xml->add( iv_name = 'AREA_ATTRIBUTES_RTS'
                     ig_data = area_attributes_rts ).

        io_xml->add( iv_name = 'OO_ATTRIBUTES'
                     ig_data = oo_attributes ).

        io_xml->add( iv_name = 'STARTUP_LIST'
                     ig_data = startup_list ).

        io_xml->add( iv_name = 'STARTUP_LIST_RTS'
                     ig_data = startup_list_rts ).

        io_xml->add( iv_name = 'CHANGE_INFO'
                     ig_data = change_info ).

        io_xml->add( iv_name = 'TADIR_SHMA'
                     ig_data = tadir_shma ).

        io_xml->add( iv_name = 'TADIR_CLASS'
                     ig_data = tadir_class ).

      CATCH cx_root INTO DATA(error).
        lcx_exception=>raise( |Error serializing SHMA { ms_item-obj_name }| ).
    ENDTRY.

  ENDMETHOD.

  METHOD lif_object~deserialize.

    DATA: area_name           TYPE shm_area_name,
          area_attributes     TYPE shma_attributes,
          area_attributes_rts TYPE shma_rts_table,
          oo_attributes       TYPE seoo_attributes_r,
          startup_list        TYPE db_startup_table,
          startup_list_rts    TYPE db_startup_table_rts,
          change_info         TYPE change_info,
          tadir_shma          TYPE tadir,
          tadir_class         TYPE tadir.

    area_name = ms_item-obj_name.

    io_xml->read(
      EXPORTING
        iv_name = 'AREA_ATTRIBUTES'
      CHANGING
        cg_data = area_attributes ).

    io_xml->read(
      EXPORTING
        iv_name = 'STARTUP_LIST'
      CHANGING
        cg_data = startup_list ).

    TRY.
        CALL METHOD ('\PROGRAM=SAPLSHMA\CLASS=LCL_SHMA_HELPER')=>('INSERT_AREA')
          EXPORTING
            area_name 			= area_name
            attributes			= area_attributes
            startup_list		= startup_list
            force_overwrite = abap_true.

      CATCH cx_root INTO DATA(error).
        lcx_exception=>raise( |Error serializing SHMA { ms_item-obj_name }| ).
    ENDTRY.

  ENDMETHOD.

  METHOD lif_object~delete.

    DATA: area_name TYPE shm_area_name.

    area_name = ms_item-obj_name.

    TRY.
        CALL FUNCTION 'SHMA_DELETE_AREA'
          EXPORTING
            area_name = area_name.

      CATCH cx_root INTO DATA(error).
        lcx_exception=>raise( |Error deleting SHMA { ms_item-obj_name }| ).
    ENDTRY.

  ENDMETHOD.

  METHOD lif_object~jump.

    DATA: ls_bcdata TYPE bdcdata,
          lt_bcdata TYPE STANDARD TABLE OF bdcdata.

    ls_bcdata-program  = 'SAPLSHMA'.
    ls_bcdata-dynpro   = '0100'.
    ls_bcdata-dynbegin = 'X'.
    APPEND ls_bcdata TO lt_bcdata.

    CLEAR ls_bcdata.
    ls_bcdata-fnam = 'SHMA_ATTRIBUTES-AREA_NAME'.
    ls_bcdata-fval = ms_item-obj_name.
    APPEND ls_bcdata TO lt_bcdata.

    CLEAR ls_bcdata.
    ls_bcdata-fnam = 'BDC_OKCODE'.
    ls_bcdata-fval = '=SHOW'.
    APPEND ls_bcdata TO lt_bcdata.

    CALL FUNCTION 'ABAP4_CALL_TRANSACTION'
      STARTING NEW TASK 'GIT'
      EXPORTING
        tcode     = 'SHMA'
        mode_val  = 'E'
      TABLES
        using_tab = lt_bcdata
      EXCEPTIONS
        OTHERS    = 1.

    IF sy-subrc <> 0.
      lcx_exception=>raise( 'error from ABAP4_CALL_TRANSACTION, SHMA' ).
    ENDIF.

  ENDMETHOD.

  METHOD lif_object~compare_to_remote_version.

    CREATE OBJECT ro_comparison_result TYPE lcl_comparison_null.

  ENDMETHOD.

ENDCLASS.
