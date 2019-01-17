CLASS zcl_abapgit_ecatt_data_downl DEFINITION
  PUBLIC
  INHERITING FROM cl_apl_ecatt_data_download
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS get_xml_stream
      RETURNING
        VALUE(rv_xml_stream) TYPE xstring .
    METHODS get_xml_stream_size
      RETURNING
        VALUE(rv_xml_stream_size) TYPE int4 .

    METHODS download
        REDEFINITION .
  PROTECTED SECTION.
    METHODS:
      download_data REDEFINITION.

  PRIVATE SECTION.

    DATA mv_xml_stream TYPE xstring .
    DATA mv_xml_stream_size TYPE int4 .
ENDCLASS.



CLASS ZCL_ABAPGIT_ECATT_DATA_DOWNL IMPLEMENTATION.


  METHOD download.

    " Downport

    DATA: lv_partyp TYPE string.

    load_help = im_load_help.

    TRY.
        cl_apl_ecatt_object=>show_object(
          EXPORTING
            im_obj_type = im_object_type
            im_name     = im_object_name
            im_version  = im_object_version
          IMPORTING
            re_object   = ecatt_object ).
      CATCH cx_ecatt INTO ex_ecatt.
        RETURN.
    ENDTRY.

    typ = im_object_type.

    lv_partyp = cl_apl_ecatt_const=>params_type_par.

    ecatt_data ?= ecatt_object.
    set_attributes_to_template( ).
    get_general_params_data( im_params = ecatt_data->params
                             im_ptyp   = lv_partyp ).

    LOOP AT parm INTO wa_parm.
      set_general_params_data_to_dom( ).
      IF NOT wa_parm-val_type IS INITIAL.
        set_deep_stru_to_dom( ecatt_data->params ).
        set_deep_data_to_dom( im_params = ecatt_data->params
                              im_pindex = wa_parm-pindex ).
      ENDIF.
    ENDLOOP.

* MS180406
    set_var_mode_to_dom( ).
* ENDMS180406
    set_variants_to_dom( ecatt_data->params ).

    download_data( ).

  ENDMETHOD.


  METHOD download_data.

    " Downport

    zcl_abapgit_ecatt_helper=>download_data(
      EXPORTING
        ii_template_over_all = template_over_all
      IMPORTING
        ev_xml_stream        = mv_xml_stream
        ev_xml_stream_size   = mv_xml_stream_size ).

  ENDMETHOD.


  METHOD get_xml_stream.

    rv_xml_stream = mv_xml_stream.

  ENDMETHOD.


  METHOD get_xml_stream_size.

    rv_xml_stream_size = mv_xml_stream_size.

  ENDMETHOD.
ENDCLASS.
