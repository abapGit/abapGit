*&---------------------------------------------------------------------*
*&  Include           ZABAPGIT_UNIT_TEST_TRANSPORT
*&---------------------------------------------------------------------*


CLASS ltcl_transport_objects DEFINITION FOR TESTING.
  PRIVATE SECTION.
    METHODS:
      add_new_to_local_files         FOR TESTING RAISING cx_static_check,
      modified_to_new_local_files    FOR TESTING RAISING cx_static_check,
      transport_not_in_repository    FOR TESTING RAISING cx_static_check,
      object_not_in_local_files      FOR TESTING RAISING cx_static_check,
      cant_be_added_with_del_flag    FOR TESTING RAISING cx_static_check,
      cant_be_modified_with_del_flag FOR TESTING RAISING cx_static_check,
      deleted_to_removed_files       FOR TESTING RAISING cx_static_check,
      shouldnt_remove_no_delflag FOR TESTING RAISING cx_static_check,
      should_add_all_local_files FOR TESTING RAISING cx_static_check,
      should_delete_all_related  FOR TESTING RAISING cx_static_check,
      setup,
      given_the_transport_object
        IMPORTING iv_obj_name TYPE string
                  iv_obj_type TYPE string
                  iv_delflag  TYPE abap_bool OPTIONAL,
      given_the_object_status
        IMPORTING
          iv_obj_name TYPE string OPTIONAL
          iv_obj_type TYPE string OPTIONAL
          iv_filename TYPE string OPTIONAL
          iv_path     TYPE string OPTIONAL
          iv_lstate   TYPE char1,
      given_the_local_file
        IMPORTING iv_obj_name          TYPE string
                  iv_obj_type          TYPE string
                  iv_filename          TYPE string
                  iv_path              TYPE string
                  iv_data              TYPE string
        RETURNING VALUE(rs_local_file) TYPE ty_file_item,
      when_staging
        RAISING lcx_exception,
      then_file_should_be_added
        IMPORTING
          is_local_file TYPE ty_file_item,
      then_it_should_raise_exception
        IMPORTING
          with_text TYPE string,
      then_it_should_remove_at_stage
        IMPORTING
          iv_filename TYPE string
          iv_path     TYPE string.

    DATA: mo_transport_objects TYPE REF TO lcl_transport_objects,
          mt_transport_objects TYPE scts_tadir,
          mt_object_statuses   TYPE ty_results_tt,
          ms_stage_objects     TYPE ty_stage_files,
          mo_stage             TYPE REF TO lcl_stage.
ENDCLASS.

CLASS ltcl_transport_objects IMPLEMENTATION.
  METHOD setup.
    CREATE OBJECT mo_stage
      EXPORTING
        iv_branch_name = 'A_branch_name'
        iv_branch_sha1 = 'Branch_SH1'.
  ENDMETHOD.
  METHOD add_new_to_local_files.
    DATA ls_local_file TYPE ty_file_item.

    given_the_transport_object(
      iv_obj_name   = 'CL_FOO'
      iv_obj_type   = 'CLAS' ).

    given_the_object_status(
      iv_obj_name   = 'CL_FOO'
      iv_obj_type   = 'CLAS'
      iv_filename   = 'CL_FOO.abap'
      iv_lstate     = gc_state-added ).

    ls_local_file = given_the_local_file(
      iv_obj_name = 'CL_FOO'
      iv_obj_type = 'CLAS'
      iv_filename = 'CL_FOO.abap'
      iv_path     = '/path'
      iv_data     = 'data' ).

    when_staging( ).

    then_file_should_be_added( ls_local_file ).
  ENDMETHOD.
  METHOD modified_to_new_local_files.
    DATA ls_local_file TYPE ty_file_item.
    given_the_transport_object(
      iv_obj_name   = 'CL_FOO'
      iv_obj_type   = 'CLAS' ).

    given_the_object_status(
      iv_obj_name   = 'CL_FOO'
      iv_obj_type   = 'CLAS'
      iv_filename   = 'CL_FOO.abap'
      iv_lstate     = gc_state-modified ).

    ls_local_file = given_the_local_file(
      iv_obj_name = 'CL_FOO'
      iv_obj_type = 'CLAS'
      iv_filename = 'CL_FOO.abap'
      iv_path     = '/path'
      iv_data     = 'data' ).

    when_staging( ).

    then_file_should_be_added( ls_local_file ).
  ENDMETHOD.
  METHOD should_add_all_local_files.
    "Not only .abap, but also .xml and other includes
    DATA ls_abap_local_file TYPE ty_file_item.
    DATA ls_xml_local_file  TYPE ty_file_item.

    given_the_transport_object(
      iv_obj_name   = 'CL_FOO'
      iv_obj_type   = 'CLAS' ).

    given_the_object_status(
      iv_obj_name   = 'CL_FOO'
      iv_obj_type   = 'CLAS'
      iv_filename   = 'CL_FOO.abap'
      iv_lstate     = gc_state-modified ).

    given_the_object_status(
      iv_obj_name   = 'CL_FOO'
      iv_obj_type   = 'CLAS'
      iv_filename   = 'CL_FOO.xml'
      iv_lstate     = gc_state-modified ).

    ls_abap_local_file = given_the_local_file(
      iv_obj_name = 'CL_FOO'
      iv_obj_type = 'CLAS'
      iv_filename = 'CL_FOO.abap'
      iv_path     = '/path'
      iv_data     = 'data' ).

    ls_xml_local_file = given_the_local_file(
      iv_obj_name = 'CL_FOO'
      iv_obj_type = 'CLAS'
      iv_filename = 'CL_FOO.xml'
      iv_path     = '/path'
      iv_data     = 'data' ).

    when_staging( ).

    then_file_should_be_added( ls_abap_local_file ).
    then_file_should_be_added( ls_xml_local_file ).
  ENDMETHOD.
  METHOD transport_not_in_repository.
    given_the_transport_object(
      iv_obj_name   = 'CL_A_CLASS_NOT_IN_REPO'
      iv_obj_type   = 'CLAS' ).

    given_the_object_status(
      iv_obj_name   = 'CL_FOO'
      iv_obj_type   = 'CLAS'
      iv_lstate     = gc_state-added ).

    then_it_should_raise_exception(
      with_text = 'Object CL_A_CLASS_NOT_IN_REPO not found in the local repository files' ).
  ENDMETHOD.

  METHOD object_not_in_local_files.

    given_the_transport_object(
      iv_obj_name   = 'CL_FOO'
      iv_obj_type   = 'CLAS' ).

    given_the_object_status(
      iv_obj_name   = 'CL_FOO'
      iv_obj_type   = 'CLAS'
      iv_lstate     = gc_state-added ).

    given_the_local_file(
      iv_obj_name = 'CL_ANOTHER_LOCAL_FILE'
      iv_obj_type = 'CLAS'
      iv_filename = 'CL_FOO.abap'
      iv_path     = '/path'
      iv_data     = 'data' ).

    then_it_should_raise_exception(
      with_text = 'Object CL_FOO not found in the local repository files' ).
  ENDMETHOD.

  METHOD cant_be_added_with_del_flag.
    given_the_transport_object(
      iv_obj_name   = 'CL_FOO'
      iv_obj_type   = 'CLAS'
      iv_delflag    = abap_true ).

    given_the_object_status(
      iv_obj_name   = 'CL_FOO'
      iv_obj_type   = 'CLAS'
      iv_lstate     = gc_state-added ).

    then_it_should_raise_exception(
      with_text = 'Object CL_FOO should be added/modified, but has deletion flag in transport' ).
  ENDMETHOD.

  METHOD cant_be_modified_with_del_flag.
    given_the_transport_object(
      iv_obj_name   = 'CL_FOO'
      iv_obj_type   = 'CLAS'
      iv_delflag    = abap_true ).

    given_the_object_status(
      iv_obj_name   = 'CL_FOO'
      iv_obj_type   = 'CLAS'
      iv_lstate     = gc_state-modified ).

    then_it_should_raise_exception(
      with_text = 'Object CL_FOO should be added/modified, but has deletion flag in transport' ).
  ENDMETHOD.
  METHOD deleted_to_removed_files.
    given_the_transport_object(
     iv_obj_name   = 'CL_FOO'
     iv_obj_type   = 'CLAS'
     iv_delflag    = abap_true ).

    given_the_object_status(
      iv_obj_name   = 'CL_FOO'
      iv_obj_type   = 'CLAS'
      iv_filename   = 'CL_FOO.abap'
      iv_path       = '/a_path'
      iv_lstate     = gc_state-deleted ).

    when_staging( ).

    then_it_should_remove_at_stage(
      iv_filename = 'CL_FOO.abap'
      iv_path     = '/a_path' ).
  ENDMETHOD.

  METHOD should_delete_all_related.
    "i.e. Should also delete the XMLs related to the transport objects
    given_the_transport_object(
     iv_obj_name   = 'CL_FOO'
     iv_obj_type   = 'CLAS'
     iv_delflag    = abap_true ).

    given_the_object_status(
      iv_obj_name   = 'CL_FOO'
      iv_obj_type   = 'CLAS'
      iv_filename   = 'CL_FOO.abap'
      iv_path       = '/a_path'
      iv_lstate     = gc_state-deleted ).

    given_the_object_status(
      iv_obj_name   = 'CL_FOO'
      iv_obj_type   = 'CLAS'
      iv_filename   = 'CL_FOO.xml'
      iv_path       = '/a_path'
      iv_lstate     = gc_state-deleted ).

    when_staging( ).

    then_it_should_remove_at_stage(
      iv_filename = 'CL_FOO.abap'
      iv_path     = '/a_path' ).

    then_it_should_remove_at_stage(
      iv_filename = 'CL_FOO.xml'
      iv_path     = '/a_path' ).
  ENDMETHOD.

  METHOD shouldnt_remove_no_delflag.
    given_the_transport_object(
       iv_obj_name   = 'CL_FOO'
       iv_obj_type   = 'CLAS'
       iv_delflag    = abap_false ).

    given_the_object_status(
      iv_obj_name   = 'CL_FOO'
      iv_obj_type   = 'CLAS'
      iv_filename   = 'CL_FOO.abap'
      iv_path       = '/a_path'
      iv_lstate     = gc_state-deleted ).

    then_it_should_raise_exception(
         with_text = 'Object CL_FOO should be removed, but has NO deletion flag in transport' ).
  ENDMETHOD.

  METHOD given_the_transport_object.
    DATA ls_transport_object TYPE tadir.
    ls_transport_object-obj_name = iv_obj_name.
    ls_transport_object-object   = iv_obj_type.
    ls_transport_object-delflag  = iv_delflag.
    APPEND ls_transport_object TO mt_transport_objects.
  ENDMETHOD.

  METHOD given_the_object_status.
    DATA ls_object_status TYPE ty_result.
    ls_object_status-obj_name = iv_obj_name.
    ls_object_status-obj_type = iv_obj_type.
    ls_object_status-filename = iv_filename.
    ls_object_status-path     = iv_path.
    ls_object_status-lstate   = iv_lstate.
    APPEND ls_object_status  TO mt_object_statuses.
  ENDMETHOD.

  METHOD given_the_local_file.
    rs_local_file-item-obj_name = iv_obj_name.
    rs_local_file-item-obj_type = iv_obj_type.
    rs_local_file-file-filename = iv_filename.
    rs_local_file-file-path     = iv_path.
    rs_local_file-file-data     = iv_data.
    APPEND rs_local_file TO ms_stage_objects-local.
  ENDMETHOD.

  METHOD when_staging.
    CREATE OBJECT mo_transport_objects
      EXPORTING
        it_transport_objects = mt_transport_objects.
    mo_transport_objects->to_stage(
      io_stage           = mo_stage
      is_stage_objects   = ms_stage_objects
      it_object_statuses = mt_object_statuses ).
  ENDMETHOD.

  METHOD then_file_should_be_added.
    DATA: lt_staged_objects TYPE lcl_stage=>ty_stage_tt.
    lt_staged_objects = mo_stage->get_all( ).

    READ TABLE lt_staged_objects TRANSPORTING NO FIELDS
    WITH KEY file-filename = is_local_file-file-filename
            file-path      = is_local_file-file-path
            file-data      = is_local_file-file-data
            method         = lcl_stage=>c_method-add.
    IF sy-subrc <> 0.
      cl_abap_unit_assert=>fail( |Object { is_local_file-file-filename } not added to stage| ).
    ENDIF.
  ENDMETHOD.

  METHOD then_it_should_raise_exception.
    DATA: lo_exception TYPE REF TO lcx_exception.
    TRY.
        when_staging( ).
        cl_abap_unit_assert=>fail( 'Should have raised exception').
      CATCH lcx_exception INTO lo_exception.
        cl_abap_unit_assert=>assert_equals(
          act = lo_exception->mv_text
          exp = with_text ).
    ENDTRY.
  ENDMETHOD.

  METHOD then_it_should_remove_at_stage.
    DATA: lt_staged_objects TYPE lcl_stage=>ty_stage_tt.
    lt_staged_objects = mo_stage->get_all( ).

    READ TABLE lt_staged_objects TRANSPORTING NO FIELDS
    WITH KEY file-filename  = iv_filename
             file-path      = iv_path.
    IF sy-subrc <> 0.
      cl_abap_unit_assert=>fail( |Object { iv_filename } not removed in stage| ).
    ENDIF.
  ENDMETHOD.

ENDCLASS.
