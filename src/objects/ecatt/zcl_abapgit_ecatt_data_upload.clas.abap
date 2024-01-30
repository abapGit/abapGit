CLASS zcl_abapgit_ecatt_data_upload DEFINITION
  PUBLIC
  INHERITING FROM cl_apl_ecatt_data_upload
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES:
      zif_abapgit_ecatt_upload.

    METHODS upload REDEFINITION.

  PROTECTED SECTION.
    METHODS:
      upload_data_from_stream REDEFINITION.

  PRIVATE SECTION.
    DATA: mv_external_xml TYPE xstring,
          BEGIN OF ms_current_object,
            s_obj_type TYPE etobj_type,
            d_obj_name TYPE etobjdname,
            d_obj_ver  TYPE etobjdver,
          END OF ms_current_object,
          mx_ecatt_apl TYPE REF TO cx_ecatt_apl.
    METHODS on_ev_object_saved FOR EVENT ev_object_saved OF cl_apl_ecatt_object IMPORTING ex_ecatt_object.
ENDCLASS.



CLASS zcl_abapgit_ecatt_data_upload IMPLEMENTATION.


  METHOD upload.
    SET HANDLER on_ev_object_saved FOR ALL INSTANCES.

    ms_current_object-s_obj_type = ch_object-s_obj_type.
    ms_current_object-d_obj_name = ch_object-d_obj_name.
    ms_current_object-d_obj_ver = ch_object-d_obj_ver.

    TRY.
        super->upload( CHANGING ch_object = ch_object ).
        SET HANDLER on_ev_object_saved FOR ALL INSTANCES ACTIVATION abap_false.
      CLEANUP.
        SET HANDLER on_ev_object_saved FOR ALL INSTANCES ACTIVATION abap_false.
    ENDTRY.

    IF mx_ecatt_apl IS BOUND.
      raise_upload_exception( previous = mx_ecatt_apl ).
    ENDIF.
  ENDMETHOD.


  METHOD upload_data_from_stream.

    " Downport
    template_over_all = zcl_abapgit_ecatt_helper=>upload_data_from_stream( mv_external_xml ).

  ENDMETHOD.


  METHOD zif_abapgit_ecatt_upload~set_stream_for_upload.

    " downport from CL_ABAPGIT_ECATT_DATA_UPLOAD SET_STREAM_FOR_UPLOAD
    mv_external_xml = iv_xml.

  ENDMETHOD.


  METHOD on_ev_object_saved.
    DATA lo_ecatt_td TYPE REF TO cl_apl_ecatt_test_data.

    " Trickery to remove any local variants that do not exist on the remote on pull.

    SET HANDLER on_ev_object_saved FOR ALL INSTANCES ACTIVATION abap_false.

    TRY.
        IF ex_ecatt_object->object_type <> ms_current_object-s_obj_type OR
           ex_ecatt_object->object_name <> ms_current_object-d_obj_name OR
           ex_ecatt_object->object_version <> ms_current_object-d_obj_ver.
          CREATE OBJECT mx_ecatt_apl
            EXPORTING
              textid    = cx_ecatt_apl=>any_text
              free_text = 'Unexpected object in save sequence'.
          RETURN.
        ENDIF.

        lo_ecatt_td ?= ex_ecatt_object.
        lo_ecatt_td->params->delete_variants( '*' ).
        TRY.
            CALL METHOD ('GET_VARIANTS_FROM_DOM_NEW')
              EXPORTING
                im_params = lo_ecatt_td->params.
          CATCH cx_sy_dyn_call_error.
            get_variants_from_dom( lo_ecatt_td->params ).
        ENDTRY.
        lo_ecatt_td->save( ).
      CATCH cx_ecatt_apl INTO mx_ecatt_apl.
        RETURN.
    ENDTRY.
  ENDMETHOD.
ENDCLASS.
