CLASS zcl_abapgit_ecatt_sp_upload DEFINITION
  PUBLIC
  INHERITING FROM cl_apl_ecatt_upload
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES:
      zif_abapgit_ecatt_upload.

    METHODS:
      upload
        REDEFINITION.

  PROTECTED SECTION.
    METHODS:
      upload_data_from_stream REDEFINITION,

      get_ecatt_sp
        RAISING
          cx_ecatt_apl .

  PRIVATE SECTION.
    DATA: mv_external_xml TYPE xstring.

ENDCLASS.



CLASS zcl_abapgit_ecatt_sp_upload IMPLEMENTATION.


  METHOD get_ecatt_sp.

    " downport

    DATA: li_ixml               TYPE REF TO if_ixml,
          li_section            TYPE REF TO if_ixml_element,
          li_dom                TYPE REF TO if_ixml_document,
          li_root               TYPE REF TO if_ixml_node,
          lv_start_profile      TYPE etxml_line_str,
          lv_exception_occurred TYPE etonoff,
          lo_ecatt_sp           TYPE REF TO object.

    FIELD-SYMBOLS: <lg_ecatt_object> TYPE any.

    TRY.
        li_section = template_over_all->find_from_name_ns( 'START_PROFILE' ).

        IF NOT li_section IS INITIAL.
          li_ixml = cl_ixml=>create( ).
          li_dom  = li_ixml->create_document( ).
          li_root ?= li_section->clone( ).
          li_dom->append_child( new_child = li_root ).
          CALL FUNCTION 'SDIXML_DOM_TO_XML'
            EXPORTING
              document      = li_dom
            IMPORTING
              xml_as_string = lv_start_profile.

          ASSIGN ('ECATT_OBJECT') TO <lg_ecatt_object>.
          ASSERT sy-subrc = 0.

          lo_ecatt_sp = <lg_ecatt_object>.

          CALL METHOD lo_ecatt_sp->('SET_SP_ATTRIBUTES')
            EXPORTING
              i_sp_xml = lv_start_profile.

        ENDIF.
      CATCH cx_ecatt_apl.
        lv_exception_occurred = 'X'.
    ENDTRY.

    IF lv_exception_occurred = 'X'.
      raise_upload_exception( previous = exception_to_raise ).
    ENDIF.
  ENDMETHOD.


  METHOD upload.

    " We inherit from CL_APL_ECATT_UPLOAD because CL_APL_ECATT_SP_UPLOAD
    " doesn't exist in 702

    " Downport

    "26.03.2013

    DATA: lx_ecatt    TYPE REF TO cx_ecatt_apl,
          lv_exists   TYPE etonoff,
          lv_exc_occ  TYPE etonoff,
          ls_tadir    TYPE tadir,
          lo_ecatt_sp TYPE REF TO object.

    FIELD-SYMBOLS: <lg_ecatt_sp> TYPE any,
                   <lg_d_akh>    TYPE data,
                   <lg_i_akh>    TYPE data.

    TRY.
        ch_object-i_devclass = ch_object-d_devclass.

        ASSIGN COMPONENT 'D_AKH' OF STRUCTURE ch_object
               TO <lg_d_akh>. " doesn't exist in 702
        ASSIGN COMPONENT 'I_AKH' OF STRUCTURE ch_object
               TO <lg_i_akh>. " doesn't exist in 702
        IF <lg_d_akh> IS ASSIGNED AND <lg_i_akh> IS ASSIGNED.
          <lg_i_akh> = <lg_d_akh>.
        ENDIF.

        super->upload( CHANGING ch_object = ch_object ).

        upload_data_from_stream( ch_object-filename ).

      CATCH cx_ecatt_apl INTO lx_ecatt.
        IF template_over_all IS INITIAL.
          RAISE EXCEPTION lx_ecatt.
        ELSE.
          lv_exc_occ = 'X'.
        ENDIF.
    ENDTRY.

    TRY.
        CALL METHOD ('GET_ATTRIBUTES_FROM_DOM_NEW') " doesn't exist in 720
          CHANGING
            ch_object = ch_object.
      CATCH cx_ecatt_apl INTO lx_ecatt.
        lv_exc_occ = 'X'.
    ENDTRY.

    ASSIGN ecatt_object TO <lg_ecatt_sp>.
    ASSERT sy-subrc = 0.

    lo_ecatt_sp = <lg_ecatt_sp>.

    TRY.
        get_ecatt_sp( ).
      CATCH cx_ecatt_apl INTO lx_ecatt.
        lv_exc_occ = 'X'.
    ENDTRY.

    TRY.
        lv_exists = cl_apl_ecatt_object=>existence_check_object(
                      im_name               = ch_object-d_obj_name
                      im_version            = ch_object-d_obj_ver
                      im_obj_type           = ch_object-s_obj_type
                      im_exists_any_version = 'X' ).

        IF lv_exists = space.
          CALL METHOD lo_ecatt_sp->('SET_TADIR_FOR_NEW_OBJECT')
            EXPORTING
              im_tadir_for_new_object = tadir_preset.
        ENDIF.
      CATCH cx_ecatt.
        CLEAR lv_exists.
    ENDTRY.

    TRY.
        CALL METHOD lo_ecatt_sp->('SAVE')
          EXPORTING
            im_do_commit = 'X'.
      CATCH cx_ecatt_apl INTO lx_ecatt.
        lv_exc_occ = 'X'.
    ENDTRY.
* Devesh,C5129871  18.07.2011  Releasing enqueu after uploading
*begin
    TRY.
        ecatt_object->close_object( im_suppress_events = 'X' ).
      CATCH cx_ecatt_apl INTO lx_ecatt.
    ENDTRY.
*end
*     get devclass from existing object
    TRY.
        cl_apl_ecatt_object=>get_tadir_entry(
          EXPORTING im_obj_name = ch_object-d_obj_name
                    im_obj_type = ch_object-s_obj_type
          IMPORTING ex_tadir = ls_tadir ).

        ch_object-d_devclass = ls_tadir-devclass.

      CATCH cx_ecatt.
        CLEAR ls_tadir.
    ENDTRY.
    IF lv_exc_occ = 'X'.
      raise_upload_exception( previous = lx_ecatt ).
    ENDIF.

  ENDMETHOD.


  METHOD upload_data_from_stream.

    " Downport
    template_over_all = zcl_abapgit_ecatt_helper=>upload_data_from_stream( mv_external_xml ).

  ENDMETHOD.


  METHOD zif_abapgit_ecatt_upload~set_stream_for_upload.

    " downport from CL_APL_ECATT_START_PROFIL SET_STREAM_FOR_UPLOAD
    mv_external_xml = iv_xml.

  ENDMETHOD.
ENDCLASS.
