CLASS zcl_abapgit_ecatt_config_downl DEFINITION
  PUBLIC
  INHERITING FROM cl_apl_ecatt_config_download
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS:
      download REDEFINITION,

      get_xml_stream
        RETURNING
          VALUE(rv_xml_stream) TYPE xstring,

      get_xml_stream_size
        RETURNING
          VALUE(rv_xml_stream_size) TYPE int4.

  PROTECTED SECTION.
    METHODS:
      download_data REDEFINITION.

  PRIVATE SECTION.
    DATA:
      mv_xml_stream      TYPE xstring,
      mv_xml_stream_size TYPE int4.

ENDCLASS.



CLASS ZCL_ABAPGIT_ECATT_CONFIG_DOWNL IMPLEMENTATION.


  METHOD download.

    " Downport

    DATA: lv_partyp TYPE string.

    load_help = im_load_help.
    typ = im_object_type.

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

    lv_partyp = cl_apl_ecatt_const=>params_type_par.

    set_attributes_to_template( ).
    ecatt_config ?= ecatt_object.
    set_ecatt_objects_to_template( ).
* MS180406
    set_var_mode_to_dom( ).
* ENDMS180406
    get_general_params_data( im_params = ecatt_config->params
                             im_ptyp   = lv_partyp ).
    LOOP AT parm INTO wa_parm.
      set_general_params_data_to_dom( ).
      IF NOT wa_parm-val_type IS INITIAL.
        set_deep_stru_to_dom( ecatt_config->params ).
        set_deep_data_to_dom( im_params = ecatt_config->params
                              im_pindex = wa_parm-pindex ).
      ENDIF.
    ENDLOOP.

    set_variants_to_dom( ecatt_config->params ).

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
