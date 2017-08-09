*&---------------------------------------------------------------------*
*&  Include  zabapgit_object_jobd
*&---------------------------------------------------------------------*

CLASS lcl_object_jobd DEFINITION INHERITING FROM lcl_objects_super FINAL.

  PUBLIC SECTION.
    INTERFACES lif_object.

ENDCLASS.

CLASS lcl_object_jobd IMPLEMENTATION.

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

    TRY.
        cl_jr_jd_manager=>check_jd_existence(
          EXPORTING
            im_jd_name     = |{ ms_item-obj_name }|
          IMPORTING
            ex_is_existing = rv_bool ).

      CATCH cx_root.
        lcx_exception=>raise( |JOBD not supported| ).
    ENDTRY.

  ENDMETHOD.

  METHOD lif_object~serialize.

    DATA: ls_job_definition TYPE cl_jr_job_definition=>ty_job_definition.

    TRY.
        cl_jr_jd_manager=>get_instance(
          EXPORTING
            im_jd_name      = |{ ms_item-obj_name }|
          IMPORTING
            ex_jd_obj       = DATA(lo_job_definition) ).

        lo_job_definition->get_jd_attributes(
          IMPORTING
            ex_jd_attributes = ls_job_definition ).

        CLEAR: ls_job_definition-jdpackage,
               ls_job_definition-btcjob_user,
               ls_job_definition-owner,
               ls_job_definition-created_date,
               ls_job_definition-created_time,
               ls_job_definition-changed_date,
               ls_job_definition-changed_time.

        io_xml->add( iv_name = 'JOBD'
                     ig_data = ls_job_definition ).

      CATCH cx_root.
        lcx_exception=>raise( |Error serializing JOBD| ).
    ENDTRY.

  ENDMETHOD.

  METHOD lif_object~deserialize.

    DATA: ls_job_definition TYPE cl_jr_job_definition=>ty_job_definition.

    TRY.
        io_xml->read(
          EXPORTING
            iv_name = 'JOBD'
          CHANGING
            cg_data = ls_job_definition ).

        cl_jr_jd_manager=>create_instance(
          EXPORTING
            im_jd_name = |{ ms_item-obj_name }|
          IMPORTING
            ex_jd_obj  = DATA(lo_job_definition) ).

        ls_job_definition-jdpackage = iv_package.

        lo_job_definition->create_jd( ls_job_definition ).

      CATCH cx_root.
        lcx_exception=>raise( |Error deserializing JOBD| ).
    ENDTRY.

    lcl_objects_activation=>add_item( ms_item ).

  ENDMETHOD.

  METHOD lif_object~delete.

    TRY.
        cl_jr_jd_manager=>get_instance(
          EXPORTING
            im_jd_name = |{ ms_item-obj_name }|
          IMPORTING
            ex_jd_obj  = DATA(lo_job_definition) ).

        lo_job_definition->delete_jd( ).

      CATCH cx_root.
        lcx_exception=>raise( |Error deleting JOBD| ).
    ENDTRY.

  ENDMETHOD.

  METHOD lif_object~jump.

    DATA: obj_name TYPE e071-obj_name.

    obj_name = ms_item-obj_name.

    CALL FUNCTION 'TR_OBJECT_JUMP_TO_TOOL'
      EXPORTING
        iv_pgmid          = 'R3TR'
        iv_object         = ms_item-obj_type
        iv_obj_name       = obj_name
        iv_action         = 'SHOW'
      EXCEPTIONS
        jump_not_possible = 1
        OTHERS            = 2.

    IF sy-subrc <> 0.
      lcx_exception=>raise( |Error from TR_OBJECT_JUMP_TO_TOOL, JOBD| ).
    ENDIF.

  ENDMETHOD.

  METHOD lif_object~compare_to_remote_version.

    CREATE OBJECT ro_comparison_result TYPE lcl_comparison_null.

  ENDMETHOD.

ENDCLASS.
