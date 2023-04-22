CLASS zcl_abapgit_object_shma DEFINITION PUBLIC INHERITING FROM zcl_abapgit_objects_super FINAL.

  PUBLIC SECTION.
    INTERFACES zif_abapgit_object.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_abapgit_object_shma IMPLEMENTATION.


  METHOD zif_abapgit_object~changed_by.

    SELECT SINGLE chg_user
      FROM shma_attributes
      INTO rv_user
      WHERE area_name = ms_item-obj_name.
    IF sy-subrc <> 0.
      rv_user = c_user_unknown.
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_object~delete.

    " We can't use FM SHMA_DELETE_AREA because it depends
    " on the corresponding class, but in abapGit it has its own
    " lifecycle. Therefore we have to reimplement most of the
    " FMs logic

    CONSTANTS: lc_request_delete TYPE i VALUE 4.

    DATA: lv_request   TYPE i,
          lv_area_name TYPE shm_area_name,
          lv_order     TYPE e070-trkorr,
          lv_korrnum   TYPE tadir-korrnum,
          lv_objname   TYPE tadir-obj_name,
          lv_task      TYPE e070-trkorr,
          lv_append    TYPE abap_bool,
          ls_tadir     TYPE tadir,
          ls_tdevc     TYPE tdevc,
          lo_cts_if    TYPE REF TO object.

    lv_area_name = ms_item-obj_name.

    TRY.
        CALL FUNCTION 'ENQUEUE_E_SHM_AREA'
          EXPORTING
            mode_shma_attributes = 'E'
            area_name            = lv_area_name
            x_area_name          = ' '
            _scope               = '2'
            _wait                = ' '
            _collect             = ' '
          EXCEPTIONS
            foreign_lock         = 1
            system_failure       = 2
            OTHERS               = 3.

        IF sy-subrc <> 0.
          zcx_abapgit_exception=>raise_t100( ).
        ENDIF.

        CALL METHOD ('\PROGRAM=SAPMSHM_MONITOR\CLASS=LCL_SHMM')=>('FREE_AREA_BY_NAME')
          EXPORTING
            area_name     = lv_area_name
            affect_server = cl_shm_area=>affect_all_servers.

        CREATE OBJECT lo_cts_if TYPE ('\FUNCTION-POOL=SHMA\CLASS=LCL_CTS_INTERFACE')
          EXPORTING
            area = lv_area_name.

        CALL METHOD lo_cts_if->('CHECK_AREA')
          EXPORTING
            request     = lc_request_delete
          IMPORTING
            access_mode = lv_request
            appendable  = lv_append.

        IF lv_request <> lc_request_delete.
          zcx_abapgit_exception=>raise( |Error deleting SHMA { ms_item-obj_name }| ).
        ENDIF.

        CALL METHOD lo_cts_if->('INSERT_AREA')
          EXPORTING
            request = lc_request_delete
          IMPORTING
            order   = lv_order
            task    = lv_task.

        DELETE FROM shma_attributes  WHERE area_name = lv_area_name.
        DELETE FROM shma_start       WHERE area_name = lv_area_name.

        lv_korrnum = lv_order.
        lv_objname = lv_area_name.

        CALL FUNCTION 'TR_TADIR_INTERFACE'
          EXPORTING
            wi_read_only      = abap_true
            wi_tadir_pgmid    = 'R3TR'
            wi_tadir_object   = 'SHMA'
            wi_tadir_obj_name = lv_objname
          IMPORTING
            new_tadir_entry   = ls_tadir
          EXCEPTIONS
            OTHERS            = 0.

        CALL FUNCTION 'TR_DEVCLASS_GET'
          EXPORTING
            iv_devclass = ls_tadir-devclass
          IMPORTING
            es_tdevc    = ls_tdevc
          EXCEPTIONS
            OTHERS      = 1.

        IF sy-subrc = 0 AND ls_tdevc-korrflag IS INITIAL.

          " TADIR entries for local objects must be deleted 'by hand'

          CALL FUNCTION 'TR_TADIR_INTERFACE'
            EXPORTING
              wi_test_modus         = abap_false
              wi_delete_tadir_entry = abap_true
              wi_tadir_pgmid        = 'R3TR'
              wi_tadir_object       = 'SHMA'
              wi_tadir_obj_name     = lv_objname
              wi_tadir_korrnum      = lv_korrnum
            EXCEPTIONS
              OTHERS                = 0.

        ENDIF.

        CALL METHOD ('\PROGRAM=SAPLSHMA\CLASS=LCL_SHMA_HELPER')=>('DELETE_RUNTIME_SETTINGS')
          EXPORTING
            area_name = lv_area_name.

        CALL FUNCTION 'DEQUEUE_E_SHM_AREA'
          EXPORTING
            mode_shma_attributes = 'E'
            area_name            = lv_area_name
            x_area_name          = ' '
            _scope               = '3'
            _synchron            = ' '
            _collect             = ' '.

      CATCH cx_root.
        zcx_abapgit_exception=>raise( |Error deleting SHMA { ms_item-obj_name }| ).
    ENDTRY.

  ENDMETHOD.


  METHOD zif_abapgit_object~deserialize.

    DATA: lv_area_name       TYPE shm_area_name,
          ls_area_attributes TYPE shma_attributes.

    lv_area_name = ms_item-obj_name.

    io_xml->read(
      EXPORTING
        iv_name = 'AREA_ATTRIBUTES'
      CHANGING
        cg_data = ls_area_attributes ).

    tadir_insert( iv_package ).

    TRY.
        CALL METHOD ('\PROGRAM=SAPLSHMA\CLASS=LCL_SHMA_HELPER')=>('INSERT_AREA')
          EXPORTING
            area_name           = lv_area_name
            attributes          = ls_area_attributes
            force_overwrite     = abap_true
            no_class_generation = abap_false
            silent_mode         = abap_true.

      CATCH cx_root.
        zcx_abapgit_exception=>raise( |Error serializing SHMA { ms_item-obj_name }| ).
    ENDTRY.

  ENDMETHOD.


  METHOD zif_abapgit_object~exists.

    DATA: lv_area_name TYPE shm_area_name.

    SELECT SINGLE area_name
           FROM shma_attributes
           INTO lv_area_name
           WHERE area_name = ms_item-obj_name.

    rv_bool = boolc( sy-subrc = 0 ).

  ENDMETHOD.


  METHOD zif_abapgit_object~get_comparator.
    RETURN.
  ENDMETHOD.


  METHOD zif_abapgit_object~get_deserialize_order.
    RETURN.
  ENDMETHOD.


  METHOD zif_abapgit_object~get_deserialize_steps.
    APPEND zif_abapgit_object=>gc_step_id-abap TO rt_steps.
  ENDMETHOD.


  METHOD zif_abapgit_object~get_metadata.

    rs_metadata = get_metadata( ).

  ENDMETHOD.


  METHOD zif_abapgit_object~is_active.
    rv_active = is_active( ).
  ENDMETHOD.


  METHOD zif_abapgit_object~is_locked.
    rv_is_locked = abap_false.
  ENDMETHOD.


  METHOD zif_abapgit_object~jump.

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

    zcl_abapgit_ui_factory=>get_gui_jumper( )->jump_batch_input(
      iv_tcode   = 'SHMA'
      it_bdcdata = lt_bcdata ).

    rv_exit = abap_true.

  ENDMETHOD.


  METHOD zif_abapgit_object~map_filename_to_object.
    RETURN.
  ENDMETHOD.


  METHOD zif_abapgit_object~map_object_to_filename.
    RETURN.
  ENDMETHOD.


  METHOD zif_abapgit_object~serialize.

    DATA: lv_area_name       TYPE shm_area_name,
          ls_area_attributes TYPE shma_attributes.

    lv_area_name = ms_item-obj_name.

    TRY.
        CALL METHOD ('\PROGRAM=SAPLSHMA\CLASS=LCL_SHMA_HELPER')=>('READ_AREA_ATTRIBUTES_ALL')
          EXPORTING
            area_name       = lv_area_name
          IMPORTING
            area_attributes = ls_area_attributes.

        CLEAR: ls_area_attributes-chg_user,
               ls_area_attributes-chg_date,
               ls_area_attributes-chg_time,
               ls_area_attributes-cls_gen_user,
               ls_area_attributes-cls_gen_date,
               ls_area_attributes-cls_gen_time.

        io_xml->add( iv_name = 'AREA_ATTRIBUTES'
                     ig_data = ls_area_attributes ).

      CATCH cx_root.
        zcx_abapgit_exception=>raise( |Error serializing SHMA { ms_item-obj_name }| ).
    ENDTRY.

  ENDMETHOD.
ENDCLASS.
