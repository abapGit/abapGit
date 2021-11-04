CLASS zcl_abapgit_object_chko DEFINITION
  PUBLIC
  INHERITING FROM zcl_abapgit_objects_super
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_abapgit_object.
    ALIASES mo_files FOR zif_abapgit_object~mo_files.
protected section.
  PRIVATE SECTION.

    TYPES:
      BEGIN OF ty_content,
        category           TYPE string,
        implementing_class TYPE string,
        remote_enabled     TYPE flag,
        parameters         TYPE cl_chko_db_api=>ty_parameters,
      END OF ty_content .
    TYPES:
      BEGIN OF ty_chko.
        INCLUDE TYPE if_aff_types=>ty_aff_header.
    TYPES:
        content TYPE ty_content,
      END OF ty_chko .
ENDCLASS.



CLASS ZCL_ABAPGIT_OBJECT_CHKO IMPLEMENTATION.


  METHOD zif_abapgit_object~changed_by.
    DATA(chko_db_api) = NEW cl_chko_db_api( ).
    DATA(chko_header) = chko_db_api->get_header( name     = CONV #( ms_item-obj_name )
                                                 version  = 'I' ).
    IF chko_header IS INITIAL.
      chko_header = chko_db_api->get_header( name     = CONV #( ms_item-obj_name )
                                                   version  = 'A' ).
    ENDIF.
    rv_user = chko_header-changed_by.
  ENDMETHOD.


  METHOD zif_abapgit_object~delete.
    DATA(chko_db_api) = NEW cl_chko_db_api( ).
    chko_db_api->delete(
      name    = CONV #( ms_item-obj_name )
      version = 'I'
    ).
    chko_db_api->delete(
      name    = CONV #( ms_item-obj_name )
      version = 'A'
    ).
  ENDMETHOD.


  method zif_abapgit_object~deserialize.

    data properties type if_aff_chko_v1=>ty_main.

    try.
        data(json_as_xstring) = mo_files->read_raw( iv_ext = 'json' ) ##NO_TEXT.

        new zcl_abapgit_ajson_cnt_handler( )->deserialize( exporting content = json_as_xstring importing data = properties ).

        properties-header-abap_language_version = if_abap_language_version=>gc_version-sap_cloud_platform.

        data(persistence) = new lcl_chko_aff_persistence( ).
        persistence->save_content(
          data     = properties
          object   = new cl_aff_obj( package = ms_item-devclass name = conv #( ms_item-obj_name ) type = ms_item-obj_type )
          language = mv_language
          version  = 'I'
          saved_by = sy-uname ).
      catch cx_aff_root into data(exception).
        ii_log->add_exception(
            ix_exc  = exception
            is_item = ms_item ).
    endtry.

  endmethod.


  METHOD zif_abapgit_object~exists.
    rv_bool = NEW cl_chko_db_api( )->exists( name = CONV #( ms_item-obj_name ) ).
  ENDMETHOD.


  METHOD zif_abapgit_object~get_comparator.
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
    DATA(lock_object) = |{ ms_item-obj_type }{ ms_item-obj_name }*|.

    rv_is_locked = exists_a_lock_entry_for( iv_lock_object = 'ESWB_EO'
                                            iv_argument    = CONV #( lock_object ) ).
  ENDMETHOD.


  METHOD zif_abapgit_object~jump.
  ENDMETHOD.


  method zif_abapgit_object~serialize.
    data hex type xstring.

    try.
        data properties type if_aff_chko_v1=>ty_main.
        data(persistence) = new lcl_chko_aff_persistence( ).
        persistence->get_content(
          exporting
            object   = new cl_aff_obj( package = ms_item-devclass name = conv #( ms_item-obj_name ) type = ms_item-obj_type )
            language = mv_language
            version  = 'A'
          importing
            data     = properties ).


        hex = new zcl_abapgit_ajson_cnt_handler( )->serialize( data = properties ).
        mo_files->add_raw( iv_ext = 'json' iv_data = hex ).

      catch cx_aff_root into data(exception).
*        ii_log->add_exception(
*          ix_exc  = exception
*          is_item = ms_item ).
      catch zcx_abapgit_ajson_error into data(exception_ajson).
*        ii_log->add_exception(
*         ix_exc  = exception_ajson
*         is_item = ms_item ).
    endtry.

  endmethod.
ENDCLASS.
