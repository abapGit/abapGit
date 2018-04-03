CLASS zcl_abapgit_object_ectd DEFINITION
  PUBLIC
  INHERITING FROM zcl_abapgit_objects_super
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_abapgit_object .

    ALIASES mo_files
      FOR zif_abapgit_object~mo_files .

    METHODS:
      constructor
        IMPORTING
          !is_item     TYPE zif_abapgit_definitions=>ty_item
          !iv_language TYPE spras .

  PRIVATE SECTION.
    TYPES: BEGIN OF ty_last_changed,
             luser TYPE xubname,
             ldate TYPE datum,
             ltime TYPE uzeit,
           END OF ty_last_changed.

    CONSTANTS:
      co_default_version TYPE etobj_ver VALUE '1' ##NO_TEXT,
      BEGIN OF co_name,
        version  TYPE string VALUE 'VERSION' ##NO_TEXT,
        versions TYPE string VALUE 'VERSIONS' ##NO_TEXT,
      END OF co_name.

    DATA:
      mv_object_name TYPE etobj_name.

    METHODS:
      clear_attributes
        CHANGING
          ci_document TYPE REF TO if_ixml_document,

      clear_elements
        CHANGING
          ci_document TYPE REF TO if_ixml_document,

      get_version_from_node
        IMPORTING
          io_node           TYPE REF TO if_ixml_node
        RETURNING
          VALUE(rv_version) TYPE string,

      deserialize_version
        IMPORTING
          io_version_node TYPE REF TO if_ixml_node
          iv_package      TYPE devclass
        RAISING
          zcx_abapgit_exception,

      serialize_version
        IMPORTING
          is_version_info  TYPE etversinfo
        CHANGING
          co_versions_node TYPE REF TO if_ixml_element
        RAISING
          cx_ecatt,

      get_changed_date
        IMPORTING
          ii_document            TYPE REF TO if_ixml_document
        RETURNING
          VALUE(rv_changed_date) TYPE d,

      get_changed_time
        IMPORTING
          ii_document            TYPE REF TO if_ixml_document
        RETURNING
          VALUE(rv_changed_time) TYPE t,

      get_changed_by_user
        IMPORTING
          ii_document               TYPE REF TO if_ixml_document
        RETURNING
          VALUE(rv_changed_by_user) TYPE xubname,

      get_change_information
        IMPORTING
          is_version_info              TYPE etversinfo
        RETURNING
          VALUE(rs_change_information) TYPE ty_last_changed
        RAISING
          cx_ecatt_apl,

      is_change_more_recent_than
        IMPORTING
          is_currently_changed            TYPE zcl_abapgit_object_ectd=>ty_last_changed
          is_last_changed                 TYPE zcl_abapgit_object_ectd=>ty_last_changed
        RETURNING
          VALUE(rv_is_change_more_recent) TYPE abap_bool.

ENDCLASS.



CLASS zcl_abapgit_object_ectd IMPLEMENTATION.


  METHOD clear_attributes.

    DATA: lo_element TYPE REF TO if_ixml_element.

    lo_element = ci_document->find_from_name( |ECTD| ).
    lo_element->set_attribute(  name  = |SAPRL|
                                value = || ).
    lo_element->set_attribute(  name  = |DOWNLOADDATE|
                                value = || ).
    lo_element->set_attribute(  name  = |DOWNLOADTIME|
                                value = || ).

  ENDMETHOD.


  METHOD clear_elements.

    DATA: lo_element TYPE REF TO if_ixml_element.

    lo_element = ci_document->find_from_name( |FUSER| ).
    lo_element->set_value( || ).

    lo_element = ci_document->find_from_name( |FDATE| ).
    lo_element->set_value( || ).

    lo_element = ci_document->find_from_name( |LUSER| ).
    lo_element->set_value( || ).

    lo_element = ci_document->find_from_name( |LDATE| ).
    lo_element->set_value( || ).

    lo_element = ci_document->find_from_name( |LTIME| ).
    lo_element->set_value( || ).

    lo_element = ci_document->find_from_name( |TWB_RESP| ).
    lo_element->set_value( || ).

    lo_element = ci_document->find_from_name( |DEVCLASS| ).
    lo_element->set_value( || ).

    lo_element = ci_document->find_from_name( |TADIR_RESP| ).
    lo_element->set_value( || ).

    lo_element = ci_document->find_from_name( |VAR_EXT_PATH| ).
    lo_element->set_value( || ).

  ENDMETHOD.


  METHOD constructor.

    super->constructor( is_item     = is_item
                        iv_language = iv_language ).

    mv_object_name = ms_item-obj_name.

  ENDMETHOD.


  METHOD deserialize_version.

    DATA: ls_object   TYPE etmobjects,
          lo_upload   TYPE REF TO cl_apl_ecatt_data_upload,
          lv_xml      TYPE xstring,
          lv_text     TYPE string,
          li_document TYPE REF TO if_ixml_document,
          lv_version  TYPE string,
          lx_error    TYPE REF TO cx_ecatt.

    lv_version = get_version_from_node( io_version_node ).

    IF lv_version IS INITIAL.
      RETURN.
    ENDIF.

    CREATE OBJECT lo_upload.

    li_document = cl_ixml=>create( )->create_document( ).
    li_document->append_child( io_version_node->get_first_child( ) ).

    lv_xml = cl_ixml_80_20=>render_to_xstring( li_document ).

    lo_upload->set_stream_for_upload( lv_xml ).

    ls_object-d_obj_name  = mv_object_name.
    ls_object-s_obj_type  = ms_item-obj_type.
    ls_object-d_devclass  = iv_package.
    ls_object-d_obj_ver   = lv_version.
    ls_object-d_overwrite = abap_true.

    TRY.
        lo_upload->upload(
          EXPORTING
            im_commit_flag = abap_true
          CHANGING
            ch_object      = ls_object ).

      CATCH cx_ecatt INTO lx_error.
        lv_text = lx_error->get_text( ).
        zcx_abapgit_exception=>raise( lv_text ).
    ENDTRY.

  ENDMETHOD.


  METHOD get_changed_by_user.

    rv_changed_by_user = ii_document->find_from_name( 'LUSER' )->get_value( ).

  ENDMETHOD.


  METHOD get_changed_date.

    DATA: lv_changed_date_external TYPE string.

    lv_changed_date_external = ii_document->find_from_name( 'LDATE' )->get_value( ).

    CALL FUNCTION 'CONVERSION_EXIT_RSDAT_INPUT'
      EXPORTING
        input        = lv_changed_date_external
      IMPORTING
        output       = rv_changed_date
      EXCEPTIONS
        invalid_date = 1
        OTHERS       = 2.

    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

  ENDMETHOD.


  METHOD get_changed_time.

    DATA: lv_changed_time_external TYPE string.

    lv_changed_time_external =  ii_document->find_from_name( 'LTIME' )->get_value( ).

    CALL FUNCTION 'CONVERSION_EXIT_TIMLO_INPUT'
      EXPORTING
        input       = lv_changed_time_external
      IMPORTING
        output      = rv_changed_time
      EXCEPTIONS
        wrong_input = 1
        OTHERS      = 2.

    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

  ENDMETHOD.


  METHOD get_change_information.

    DATA: li_document TYPE REF TO if_ixml_document,
          lo_download TYPE REF TO cl_apl_ecatt_data_download,
          lv_xml      TYPE xstring.

    CREATE OBJECT lo_download.

    lo_download->build_xml_of_object(
      EXPORTING
        im_object_name    = mv_object_name
        im_object_version = is_version_info-version
        im_object_type    = ms_item-obj_type
      IMPORTING
        ex_xml_stream     = lv_xml ).

    li_document = cl_ixml_80_20=>parse_to_document( stream_xstring = lv_xml ).

    rs_change_information-ldate = get_changed_date( li_document ).
    rs_change_information-ltime = get_changed_time( li_document ).
    rs_change_information-luser = get_changed_by_user( li_document ).

  ENDMETHOD.


  METHOD get_version_from_node.

    TRY.
        rv_version = io_node->get_first_child(
                           )->get_first_child(
                           )->get_first_child(
                           )->get_first_child(
                           )->get_value( ).

      CATCH cx_sy_ref_is_initial.
        RETURN.
    ENDTRY.

  ENDMETHOD.


  METHOD is_change_more_recent_than.

    IF ( is_currently_changed-ldate > is_last_changed-ldate )
      OR (     is_currently_changed-ldate = is_last_changed-ldate
           AND is_currently_changed-ltime > is_last_changed-ltime ).

      rv_is_change_more_recent = abap_true.

    ENDIF.

  ENDMETHOD.


  METHOD serialize_version.

    DATA: li_document TYPE REF TO if_ixml_document,
          lo_download TYPE REF TO cl_apl_ecatt_data_download,
          lv_xml      TYPE xstring,
          lo_node     TYPE REF TO if_ixml_element.

    CREATE OBJECT lo_download.

    lo_download->build_xml_of_object(
      EXPORTING
        im_object_name    = mv_object_name
        im_object_version = is_version_info-version
        im_object_type    = ms_item-obj_type
      IMPORTING
        ex_xml_stream     = lv_xml ).

    li_document = cl_ixml_80_20=>parse_to_document( stream_xstring = lv_xml ).

    clear_attributes(
      CHANGING
        ci_document = li_document ).

    clear_elements(
      CHANGING
        ci_document = li_document ).

    lo_node = li_document->create_element( co_name-version ).
    lo_node->append_child( li_document->get_root_element( ) ).

    co_versions_node->append_child( lo_node ).

  ENDMETHOD.


  METHOD zif_abapgit_object~changed_by.

    DATA: ls_last_changed      TYPE ty_last_changed,
          ls_currently_changed TYPE ty_last_changed,
          lt_version_info      TYPE etversinfo_tabtype,
          lx_error             TYPE REF TO cx_ecatt,
          lv_text              TYPE string.

    FIELD-SYMBOLS: <ls_version_info> LIKE LINE OF lt_version_info.

    TRY.
        cl_apl_ecatt_object=>get_version_info_object(
          EXPORTING
            im_name          = mv_object_name
            im_obj_type      = ms_item-obj_type
          IMPORTING
            ex_version_info  = lt_version_info  ).

        LOOP AT lt_version_info ASSIGNING <ls_version_info>.

          ls_currently_changed = get_change_information( <ls_version_info> ).

          IF is_change_more_recent_than( is_currently_changed = ls_currently_changed
                                         is_last_changed      = ls_last_changed ) = abap_true.

            ls_last_changed = ls_currently_changed.

          ENDIF.

        ENDLOOP.

      CATCH cx_ecatt INTO lx_error.
        lv_text = lx_error->get_text( ).
        MESSAGE lv_text TYPE 'S' DISPLAY LIKE 'E'.
    ENDTRY.

    IF ls_last_changed-luser IS NOT INITIAL.

      rv_user = ls_last_changed-luser.

    ELSE.

      rv_user = c_user_unknown.

    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_object~compare_to_remote_version.
    CREATE OBJECT ro_comparison_result TYPE zcl_abapgit_comparison_null.
  ENDMETHOD.


  METHOD zif_abapgit_object~delete.

    TRY.
        cl_apl_ecatt_object=>delete_object( im_obj_type            = ms_item-obj_type
                                            im_name                = mv_object_name
                                            " we have to supply a version, so let's use the default version
                                            " and delete them all
                                            im_version             = co_default_version
                                            im_delete_all_versions = abap_true ).

      CATCH cx_ecatt_apl INTO DATA(error).
        zcx_abapgit_exception=>raise( error->get_text( ) ).
    ENDTRY.

  ENDMETHOD.


  METHOD zif_abapgit_object~deserialize.

    DATA: li_document         TYPE REF TO if_ixml_document,
          li_versions         TYPE REF TO if_ixml_node_collection,
          li_version_iterator TYPE REF TO if_ixml_node_iterator,
          lo_version_node     TYPE REF TO if_ixml_node.

    li_document = io_xml->get_raw( ).

    li_versions = li_document->get_elements_by_tag_name( depth = 0
                                                         name  = co_name-version ).

    li_version_iterator = li_versions->create_iterator( ).

    DO.
      lo_version_node = li_version_iterator->get_next( ).

      IF lo_version_node IS NOT BOUND.
        EXIT.
      ENDIF.

      deserialize_version( io_version_node = lo_version_node
                           iv_package      = iv_package ).

    ENDDO.

  ENDMETHOD.


  METHOD zif_abapgit_object~exists.

    TRY.
        rv_bool = cl_apl_ecatt_object=>existence_check_object( im_name               = mv_object_name
                                                               im_version            = co_default_version
                                                               im_obj_type           = ms_item-obj_type
                                                               im_exists_any_version = abap_true ).

      CATCH cx_ecatt.
        RETURN.
    ENDTRY.

  ENDMETHOD.


  METHOD zif_abapgit_object~get_metadata.
    rs_metadata = get_metadata( ).
  ENDMETHOD.


  METHOD zif_abapgit_object~has_changed_since.
    rv_changed = abap_true.
  ENDMETHOD.


  METHOD zif_abapgit_object~jump.

    CALL FUNCTION 'RS_TOOL_ACCESS'
      EXPORTING
        operation           = 'SHOW'
        object_name         = ms_item-obj_name
        object_type         = ms_item-obj_type
        in_new_window       = abap_true
      EXCEPTIONS
        not_executed        = 1
        invalid_object_type = 2
        OTHERS              = 3.

    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( |Error { sy-subrc } from RS_TOOL_ACCESS | ).
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_object~serialize.

    DATA:
      lt_version_info  TYPE etversinfo_tabtype,
      li_document      TYPE REF TO if_ixml_document,
      lo_versions_node TYPE REF TO if_ixml_element,
      lv_text          TYPE string,
      lx_error         TYPE REF TO cx_ecatt.

    FIELD-SYMBOLS: <ls_version_info> TYPE etversinfo.

    TRY.
        cl_apl_ecatt_object=>get_version_info_object(
          EXPORTING
            im_name          = mv_object_name
            im_obj_type      = ms_item-obj_type
          IMPORTING
            ex_version_info  = lt_version_info  ).

        li_document = cl_ixml=>create( )->create_document( ).

        lo_versions_node = li_document->create_element( co_name-versions ).

        LOOP AT lt_version_info ASSIGNING <ls_version_info>.

          serialize_version(
            EXPORTING
              is_version_info = <ls_version_info>
            CHANGING
              co_versions_node = lo_versions_node ).

        ENDLOOP.

        li_document->append_child( lo_versions_node ).

        io_xml->set_raw( li_document->get_root_element( ) ).

      CATCH cx_ecatt INTO lx_error.
        lv_text = lx_error->get_text( ).
        MESSAGE lv_text TYPE 'S' DISPLAY LIKE 'E'.
    ENDTRY.

  ENDMETHOD.
ENDCLASS.
