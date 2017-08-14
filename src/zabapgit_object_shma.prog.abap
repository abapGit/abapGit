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

  ENDMETHOD.

  METHOD lif_object~exists.

    DATA: area_name TYPE shm_area_name.

    SELECT SINGLE area_name
           FROM shma_attributes
           INTO area_name
           WHERE area_name = ms_item-obj_name.

    rv_bool = boolc( sy-subrc = 0 ).

  ENDMETHOD.

  METHOD lif_object~serialize.

    DATA: area_name       TYPE shm_area_name,
          area_attributes TYPE shma_attributes,
          startup_list    TYPE db_startup_table.

    area_name = ms_item-obj_name.

    TRY.
        CALL METHOD ('\PROGRAM=SAPLSHMA\CLASS=LCL_SHMA_HELPER')=>('READ_AREA_ATTRIBUTES_ALL')
          EXPORTING
            area_name       = area_name
          IMPORTING
            area_attributes = area_attributes
            startup_list    = startup_list.

        CLEAR: area_attributes-chg_user,
               area_attributes-chg_date,
               area_attributes-chg_time,
               area_attributes-cls_gen_user ,
               area_attributes-cls_gen_date ,
               area_attributes-cls_gen_time.

        io_xml->add( iv_name = 'AREA_ATTRIBUTES'
                     ig_data = area_attributes ).

        io_xml->add( iv_name = 'STARTUP_LIST'
                     ig_data = startup_list ).

      CATCH cx_root INTO DATA(error).
        lcx_exception=>raise( |Error serializing SHMA { ms_item-obj_name }| ).
    ENDTRY.

  ENDMETHOD.

  METHOD lif_object~deserialize.

    DATA: area_name       TYPE shm_area_name,
          area_attributes TYPE shma_attributes,
          startup_list    TYPE db_startup_table.

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
            area_name           = area_name
            attributes          = area_attributes
            startup_list        = startup_list
            force_overwrite     = abap_true
            no_class_generation = abap_true
            silent_mode         = abap_true.


      CATCH cx_root INTO DATA(error).
        lcx_exception=>raise( |Error serializing SHMA { ms_item-obj_name }| ).
    ENDTRY.

  ENDMETHOD.

  METHOD lif_object~delete.

    " We can't use FM SHMA_DELETE_AREA because it depends
    " on the corresponding class, but in abapGit it has its own
    " lifecycle. Therefore we have to reimplement most of the
    " FMs logic

    CONSTANTS:
        c_request_delete TYPE i VALUE '4'.

    DATA: request   TYPE i,
          area_name TYPE shm_area_name,
          order     TYPE e070-trkorr,
          korrnum   TYPE tadir-korrnum,
          objname   TYPE tadir-obj_name,
          task      TYPE e070-trkorr,
          append    TYPE abap_bool,
          tadir     TYPE tadir,
          tdevc     TYPE tdevc,
          lo_cts_if TYPE REF TO object.

    area_name = ms_item-obj_name.

    TRY.
        CALL FUNCTION 'ENQUEUE_E_SHM_AREA'
          EXPORTING
            mode_shma_attributes = 'E'
            area_name            = area_name
            x_area_name          = ' '
            _scope               = '2'
            _wait                = ' '
            _collect             = ' '
          EXCEPTIONS
            foreign_lock         = 1
            system_failure       = 2
            OTHERS               = 3.

        IF sy-subrc <> 0.
          lcx_exception=>raise( |Error deleting SHMA { ms_item-obj_name }| ).
        ENDIF.

        CALL METHOD ('\PROGRAM=SAPMSHM_MONITOR\CLASS=LCL_SHMM')=>('FREE_AREA_BY_NAME')
          EXPORTING
            area_name     = area_name
            affect_server = cl_shm_area=>affect_all_servers.

        CREATE OBJECT lo_cts_if TYPE ('\FUNCTION-POOL=SHMA\CLASS=LCL_CTS_INTERFACE')
          EXPORTING
            area = area_name.

        CALL METHOD lo_cts_if->('CHECK_AREA')
          EXPORTING
            request     = c_request_delete
          IMPORTING
            access_mode = request
            appendable  = append.

        IF request <> c_request_delete.
          lcx_exception=>raise( |Error deleting SHMA { ms_item-obj_name }| ).
        ENDIF.

        CALL METHOD lo_cts_if->('INSERT_AREA')
          EXPORTING
            request = c_request_delete
          IMPORTING
            order   = order
            task    = task.

        DELETE FROM shma_attributes  WHERE area_name = area_name.
        DELETE FROM shma_start       WHERE area_name = area_name.

        korrnum = order.
        objname = area_name.

        CALL FUNCTION 'TR_TADIR_INTERFACE'
          EXPORTING
            wi_read_only      = abap_true
            wi_tadir_pgmid    = 'R3TR'
            wi_tadir_object   = 'SHMA'
            wi_tadir_obj_name = objname
          IMPORTING
            new_tadir_entry   = tadir
          EXCEPTIONS
            OTHERS            = 0.

        CALL FUNCTION 'TR_DEVCLASS_GET'
          EXPORTING
            iv_devclass = tadir-devclass
          IMPORTING
            es_tdevc    = tdevc
          EXCEPTIONS
            OTHERS      = 1.

        IF  sy-subrc = 0 AND tdevc-korrflag IS INITIAL.

          " TADIR entries for local objects must be deleted 'by hand'

          CALL FUNCTION 'TR_TADIR_INTERFACE'
            EXPORTING
              wi_test_modus         = abap_false
              wi_delete_tadir_entry = abap_true
              wi_tadir_pgmid        = 'R3TR'
              wi_tadir_object       = 'SHMA'
              wi_tadir_obj_name     = objname
              wi_tadir_korrnum      = korrnum
            EXCEPTIONS
              OTHERS                = 0.

          CALL FUNCTION 'TR_TADIR_INTERFACE'
            EXPORTING
              wi_test_modus         = abap_false
              wi_delete_tadir_entry = abap_true
              wi_tadir_pgmid        = 'R3TR'
              wi_tadir_object       = 'CLAS'
              wi_tadir_obj_name     = objname
              wi_tadir_korrnum      = korrnum
            EXCEPTIONS
              OTHERS                = 0.

        ENDIF.

        CALL METHOD ('\PROGRAM=SAPLSHMA\CLASS=LCL_SHMA_HELPER')=>('DELETE_RUNTIME_SETTINGS')
          EXPORTING
            area_name = area_name.

        CALL FUNCTION 'DEQUEUE_E_SHM_AREA'
          EXPORTING
            mode_shma_attributes = 'E'
            area_name            = area_name
            x_area_name          = ' '
            _scope               = '3'
            _synchron            = ' '
            _collect             = ' '.

      CATCH cx_root.
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
