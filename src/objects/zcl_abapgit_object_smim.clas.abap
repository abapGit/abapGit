CLASS zcl_abapgit_object_smim DEFINITION PUBLIC INHERITING FROM zcl_abapgit_objects_super FINAL.

  PUBLIC SECTION.
    INTERFACES zif_abapgit_object.
  PROTECTED SECTION.
  PRIVATE SECTION.

    CONSTANTS c_prop_abap_langu_vers TYPE string VALUE 'MIME_ABAP_LANGUAGE_VRS'.

    TYPES:
      BEGIN OF ty_extra,
        file_name             TYPE string,
        mimetype              TYPE string,
        description           TYPE string,
        abap_language_version TYPE uccheck,
        parent_folder_id      TYPE skwf_io-objid,
      END OF ty_extra.

    METHODS get_filename
      IMPORTING iv_url             TYPE skwf_url
      RETURNING VALUE(rv_filename) TYPE string.

    METHODS find_content
      IMPORTING iv_url            TYPE skwf_url
                iv_filename       TYPE string
      RETURNING VALUE(rv_content) TYPE xstring
      RAISING   zcx_abapgit_exception.

    METHODS build_filename
      IMPORTING iv_filename        TYPE string
      RETURNING VALUE(rv_filename) TYPE string.

    METHODS get_url_for_io
      EXPORTING ev_url       TYPE skwf_url
                ev_is_folder TYPE abap_bool
                es_io        TYPE skwf_io
      RAISING   zcx_abapgit_not_found
                zcx_abapgit_exception.

    METHODS get_properties
      IMPORTING is_loio  TYPE skwf_io
      CHANGING  cs_extra TYPE ty_extra
      RAISING   zcx_abapgit_exception.

    METHODS set_properties
      IMPORTING is_loio  TYPE skwf_io
                is_extra TYPE ty_extra
      RAISING   zcx_abapgit_exception.

    METHODS get_filename_and_mimetype
      IMPORTING is_loio  TYPE skwf_io
      CHANGING  cs_extra TYPE ty_extra
      RAISING   zcx_abapgit_exception.

    METHODS set_filename_and_mimetype
      IMPORTING is_loio  TYPE skwf_io
                is_extra TYPE ty_extra
      RAISING   zcx_abapgit_exception.

ENDCLASS.



CLASS zcl_abapgit_object_smim IMPLEMENTATION.


  METHOD build_filename.

    CONCATENATE ms_item-obj_name ms_item-obj_type iv_filename INTO rv_filename SEPARATED BY '.'.
    TRANSLATE rv_filename TO LOWER CASE.

  ENDMETHOD.


  METHOD find_content.

    DATA: lv_filename TYPE string,
          lt_files    TYPE zif_abapgit_git_definitions=>ty_files_tt.

    FIELD-SYMBOLS: <ls_file> LIKE LINE OF lt_files.

    lt_files = mo_files->get_files( ).

    READ TABLE lt_files ASSIGNING <ls_file>
        WITH KEY file
        COMPONENTS filename = iv_filename.
    IF sy-subrc <> 0.
      " Fallback to getting file name from URL
      lv_filename = build_filename( get_filename( iv_url ) ).

      READ TABLE lt_files ASSIGNING <ls_file>
          WITH KEY file
          COMPONENTS filename = lv_filename.
      IF sy-subrc <> 0.
        zcx_abapgit_exception=>raise( 'SMIM, file not found' ).
      ENDIF.
    ENDIF.

    rv_content = <ls_file>-data.

  ENDMETHOD.


  METHOD get_filename.

    DATA: lv_lines   TYPE i,
          lt_strings TYPE TABLE OF string.


    SPLIT iv_url AT '/' INTO TABLE lt_strings.
    lv_lines = lines( lt_strings ).
    ASSERT lv_lines > 0.
    READ TABLE lt_strings INDEX lv_lines INTO rv_filename.
    ASSERT sy-subrc = 0.

  ENDMETHOD.


  METHOD get_filename_and_mimetype.

    DATA ls_phio TYPE skwf_io.
    DATA lt_phios TYPE STANDARD TABLE OF skwf_io WITH DEFAULT KEY.

    " Get file name with extension which is important for importing object correctly
    CALL FUNCTION 'SKWF_LOIO_ALL_PHIOS_GET'
      EXPORTING
        loio  = is_loio
      TABLES
        phios = lt_phios.

    LOOP AT lt_phios INTO ls_phio.
      SELECT SINGLE file_name mimetype FROM smimphf INTO CORRESPONDING FIELDS OF cs_extra
        WHERE langu = sy-langu AND loio_id = is_loio-objid AND phio_id = ls_phio-objid.
      IF sy-subrc = 0.
        EXIT.
      ENDIF.
    ENDLOOP.

    IF cs_extra-mimetype IS INITIAL OR cs_extra-file_name IS INITIAL.
      zcx_abapgit_exception=>raise( 'File name or mimetype not found' ).
    ENDIF.

  ENDMETHOD.


  METHOD get_properties.

    DATA ls_loio_prop TYPE sdokpropty.
    DATA lt_loio_props TYPE STANDARD TABLE OF sdokpropty WITH DEFAULT KEY.

    CALL FUNCTION 'SKWF_IO_PROPERTIES_GET'
      EXPORTING
        io                = is_loio
      TABLES
        properties_result = lt_loio_props.

    READ TABLE lt_loio_props INTO ls_loio_prop WITH KEY name = skwfc_prop_description.
    IF sy-subrc = 0.
      cs_extra-description = ls_loio_prop-value.
    ENDIF.

    READ TABLE lt_loio_props INTO ls_loio_prop WITH KEY name = skwfc_prop_folder_id.
    IF sy-subrc = 0.
      cs_extra-parent_folder_id = ls_loio_prop-value.
    ENDIF.

    READ TABLE lt_loio_props INTO ls_loio_prop WITH KEY name = c_prop_abap_langu_vers.
    IF sy-subrc = 0.
      cs_extra-abap_language_version = ls_loio_prop-value.
      clear_abap_language_version( CHANGING cv_abap_language_version = cs_extra-abap_language_version ).
    ENDIF.

  ENDMETHOD.


  METHOD get_url_for_io.

    DATA ls_smimloio TYPE smimloio.

    CLEAR: ev_url, ev_is_folder, es_io.

    SELECT SINGLE * FROM smimloio INTO ls_smimloio
      WHERE loio_id = ms_item-obj_name.                 "#EC CI_GENBUFF
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_abapgit_not_found.
    ENDIF.

    IF ls_smimloio-lo_class = wbmr_c_skwf_folder_class.
      ev_is_folder = abap_true.
      es_io-objtype = skwfc_obtype_folder.
    ELSE.
      es_io-objtype = skwfc_obtype_loio.
    ENDIF.
    es_io-class = ls_smimloio-lo_class.
    es_io-objid = ls_smimloio-loio_id.

    CALL FUNCTION 'SKWF_NMSPC_IO_ADDRESS_GET'
      EXPORTING
        io  = es_io
      IMPORTING
        url = ev_url.

  ENDMETHOD.


  METHOD set_filename_and_mimetype.

    DATA ls_phio TYPE skwf_io.
    DATA lt_phios TYPE STANDARD TABLE OF skwf_io WITH DEFAULT KEY.

    IF is_extra-mimetype IS INITIAL OR is_extra-file_name IS INITIAL.
      RETURN.
    ENDIF.

    CALL FUNCTION 'SKWF_LOIO_ALL_PHIOS_GET'
      EXPORTING
        loio  = is_loio
      TABLES
        phios = lt_phios.

    LOOP AT lt_phios INTO ls_phio.
      UPDATE smimphf SET mimetype = is_extra-mimetype file_name = is_extra-file_name
        WHERE langu = sy-langu AND loio_id = is_loio-objid AND phio_id = ls_phio-objid.
      IF sy-subrc <> 0.
        zcx_abapgit_exception=>raise( 'Error updating file name and mimetype' ).
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD set_properties.

    DATA ls_property TYPE sdokpropty.
    DATA lt_properties TYPE STANDARD TABLE OF sdokpropty WITH DEFAULT KEY.
    DATA lv_abap_language_version TYPE uccheck.

    ls_property-name  = skwfc_prop_description.
    ls_property-value = is_extra-description.
    INSERT ls_property INTO TABLE lt_properties.

    lv_abap_language_version = is_extra-abap_language_version.
    set_abap_language_version( CHANGING cv_abap_language_version = lv_abap_language_version ).

    ls_property-name  = c_prop_abap_langu_vers.
    ls_property-value = lv_abap_language_version.
    INSERT ls_property INTO TABLE lt_properties.

    CALL FUNCTION 'SKWF_IO_PROPERTIES_SET'
      EXPORTING
        io         = is_loio
      TABLES
        properties = lt_properties.

  ENDMETHOD.


  METHOD zif_abapgit_object~changed_by.

    DATA: lv_loio TYPE sdok_docid.


    lv_loio = ms_item-obj_name.

    SELECT SINGLE chng_user FROM smimloio INTO rv_user
      WHERE loio_id = lv_loio.                          "#EC CI_GENBUFF
    IF sy-subrc <> 0 OR rv_user IS INITIAL.
      SELECT SINGLE crea_user FROM smimloio INTO rv_user
        WHERE loio_id = lv_loio.                        "#EC CI_GENBUFF
      IF sy-subrc <> 0 OR rv_user IS INITIAL.
        rv_user = c_user_unknown.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_object~delete.

    DATA: li_api TYPE REF TO if_mr_api,
          lv_url TYPE skwf_url.


    TRY.
        get_url_for_io( IMPORTING ev_url = lv_url ).
      CATCH zcx_abapgit_not_found.
        " Deleted already (maybe by "folder with children") but record deletion in transport
        corr_insert( iv_package ).
        RETURN.
    ENDTRY.

    li_api = cl_mime_repository_api=>if_mr_api~get_api( ).
    li_api->delete(
      EXPORTING
        i_url              = lv_url
        i_delete_children  = abap_true
      EXCEPTIONS
        parameter_missing  = 1
        error_occured      = 2
        cancelled          = 3
        permission_failure = 4
        not_found          = 5
        OTHERS             = 6 ).
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise_t100( ).
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_object~deserialize.

    DATA: lv_url      TYPE skwf_url,
          lv_folder   TYPE abap_bool,
          lv_mimetype TYPE string,
          lv_content  TYPE xstring,
          ls_extra    TYPE ty_extra,
          ls_loio     TYPE skwf_io,
          li_api      TYPE REF TO if_mr_api.

    li_api = cl_mime_repository_api=>if_mr_api~get_api( ).
    ls_loio-objid = ms_item-obj_name.

    io_xml->read( EXPORTING iv_name = 'URL'
                  CHANGING  cg_data = lv_url ).
    io_xml->read( EXPORTING iv_name = 'FOLDER'
                  CHANGING  cg_data = lv_folder ).
    io_xml->read( EXPORTING iv_name = 'CLASS'
                  CHANGING  cg_data = ls_loio-class ).
    io_xml->read( EXPORTING iv_name = 'EXTRA'
                  CHANGING  cg_data = ls_extra ).

    IF lv_folder = abap_true.
      ls_loio-objtype = skwfc_obtype_folder.

      li_api->create_folder(
        EXPORTING
          i_url              = lv_url
          i_language         = mv_language
          i_dev_package      = iv_package
          i_folder_loio      = ls_loio
        EXCEPTIONS
          parameter_missing  = 1
          error_occured      = 2
          cancelled          = 3
          permission_failure = 4
          folder_exists      = 5
          OTHERS             = 6 ).
      IF sy-subrc <> 5 AND sy-subrc <> 0.
        zcx_abapgit_exception=>raise_t100( ).
      ENDIF.
    ELSE.
      ls_loio-objtype = skwfc_obtype_loio.

      lv_content = find_content(
        iv_url      = lv_url
        iv_filename = ls_extra-file_name ).

      " This PUT is using function SDOK_MIMETYPE_GET to derive the mimetype from the file extension of the URL
      " If there's no extension, it defaults to 'application/octet-stream'. Therefore, the correct file name
      li_api->put(
        EXPORTING
          i_url                   = lv_url
          i_content               = lv_content
          i_dev_package           = iv_package
          i_new_loio              = ls_loio
          i_language              = mv_language
        EXCEPTIONS
          parameter_missing       = 1
          error_occured           = 2
          cancelled               = 3
          permission_failure      = 4
          data_inconsistency      = 5
          new_loio_already_exists = 6
          is_folder               = 7
          OTHERS                  = 8 ).
      IF sy-subrc <> 0.
        zcx_abapgit_exception=>raise_t100( ).
      ENDIF.

      set_filename_and_mimetype(
        is_loio  = ls_loio
        is_extra = ls_extra ).
    ENDIF.

    set_properties(
      is_loio  = ls_loio
      is_extra = ls_extra ).

  ENDMETHOD.


  METHOD zif_abapgit_object~exists.

    DATA: lv_loio TYPE sdok_docid.


    lv_loio = ms_item-obj_name.

    SELECT SINGLE loio_id FROM smimloio INTO lv_loio
      WHERE loio_id = lv_loio.                          "#EC CI_GENBUFF
    rv_bool = boolc( sy-subrc = 0 ).

  ENDMETHOD.


  METHOD zif_abapgit_object~get_comparator.
    RETURN.
  ENDMETHOD.


  METHOD zif_abapgit_object~get_deserialize_order.
    " TODO: Use parent folder ID here (#4783)
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
    " Covered by ZCL_ABAPGIT_OBJECTS=>JUMP
  ENDMETHOD.


  METHOD zif_abapgit_object~map_filename_to_object.
    RETURN.
  ENDMETHOD.


  METHOD zif_abapgit_object~map_object_to_filename.
    RETURN.
  ENDMETHOD.


  METHOD zif_abapgit_object~serialize.

    DATA: lv_url     TYPE skwf_url,
          lv_folder  TYPE abap_bool,
          ls_file    TYPE zif_abapgit_git_definitions=>ty_file,
          lv_content TYPE xstring,
          ls_extra   TYPE ty_extra,
          li_api     TYPE REF TO if_mr_api,
          ls_loio    TYPE skwf_io.

    TRY.
        get_url_for_io(
          IMPORTING
            ev_url       = lv_url
            ev_is_folder = lv_folder
            es_io        = ls_loio ).
      CATCH zcx_abapgit_not_found.
        RETURN.
    ENDTRY.

    IF lv_folder = abap_false.
      li_api = cl_mime_repository_api=>if_mr_api~get_api( ).

      li_api->get(
        EXPORTING
          i_url              = lv_url
        IMPORTING
          e_content          = lv_content
        EXCEPTIONS
          parameter_missing  = 1
          error_occured      = 2
          not_found          = 3
          permission_failure = 4
          OTHERS             = 5 ).
      IF sy-subrc <> 0 AND sy-subrc <> 2 AND sy-subrc <> 3.
        zcx_abapgit_exception=>raise_t100( ).
      ENDIF.

      get_filename_and_mimetype(
        EXPORTING
          is_loio  = ls_loio
        CHANGING
          cs_extra = ls_extra ).

      CLEAR ls_file.
      ls_file-filename = build_filename( ls_extra-file_name ).
      ls_file-path     = '/'.
      ls_file-data     = lv_content.
      mo_files->add( ls_file ).
    ENDIF.

    get_properties(
      EXPORTING
        is_loio  = ls_loio
      CHANGING
        cs_extra = ls_extra ).

    io_xml->add( iv_name = 'URL'
                 ig_data = lv_url ).
    io_xml->add( iv_name = 'FOLDER'
                 ig_data = lv_folder ).
    io_xml->add( iv_name = 'CLASS'
                 ig_data = ls_loio-class ).
    io_xml->add( iv_name = 'EXTRA'
                 ig_data = ls_extra ).

  ENDMETHOD.
ENDCLASS.
