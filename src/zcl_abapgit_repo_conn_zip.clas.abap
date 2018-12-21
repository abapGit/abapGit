CLASS zcl_abapgit_repo_conn_zip DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_abapgit_repo_connector .

    CLASS-METHODS create
      IMPORTING
        iv_path TYPE string
      RETURNING
        VALUE(ro_connector) TYPE REF TO zcl_abapgit_repo_conn_zip
      RAISING
        zcx_abapgit_exception.


  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA mv_filepath TYPE string.
    DATA mv_blob TYPE xstring.

    CLASS-METHODS parse_filename
      IMPORTING
        iv_str      TYPE string
      EXPORTING
        ev_path     TYPE string
        ev_filename TYPE string
      RAISING
        zcx_abapgit_exception.

    CLASS-METHODS normalize_path
      CHANGING
        ct_files TYPE zif_abapgit_definitions=>ty_files_tt
      RAISING
        zcx_abapgit_exception.


ENDCLASS.



CLASS ZCL_ABAPGIT_REPO_CONN_ZIP IMPLEMENTATION.


  METHOD zif_abapgit_repo_connector~fetch.

    DATA:
      lo_zip  TYPE REF TO cl_abap_zip,
      lv_data TYPE xstring.

    FIELD-SYMBOLS:
      <ls_zipfile> TYPE cl_abap_zip=>t_file,
      <ls_file>    LIKE LINE OF et_files.

    CLEAR: et_files, et_objects, ev_branch_sha1.

    CREATE OBJECT lo_zip.
    lo_zip->load(
      EXPORTING
        zip             = mv_blob
      EXCEPTIONS
        zip_parse_error = 1
        OTHERS          = 2 ).
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( 'error from zip' ).
    ENDIF.

    LOOP AT lo_zip->files ASSIGNING <ls_zipfile>.

      lo_zip->get(
        EXPORTING
          name                    = <ls_zipfile>-name
        IMPORTING
          content                 = lv_data
        EXCEPTIONS
          zip_index_error         = 1
          zip_decompression_error = 2
          OTHERS                  = 3 ).
      IF sy-subrc <> 0.
        zcx_abapgit_exception=>raise( 'error from zip get' ).
      ENDIF.

      APPEND INITIAL LINE TO et_files ASSIGNING <ls_file>.

      parse_filename(
        EXPORTING
          iv_str      = <ls_zipfile>-name
        IMPORTING
          ev_path     = <ls_file>-path
          ev_filename = <ls_file>-filename ).

      <ls_file>-data = lv_data.
      <ls_file>-sha1 = zcl_abapgit_hash=>sha1(
        iv_type = zif_abapgit_definitions=>c_type-blob
        iv_data = <ls_file>-data ).

    ENDLOOP.

    DELETE et_files WHERE filename IS INITIAL.

    normalize_path( CHANGING ct_files = et_files ).

  ENDMETHOD.


  METHOD zif_abapgit_repo_connector~push.

    zcx_abapgit_exception=>raise( 'push is not supported for zip connector' ).

  ENDMETHOD.


  METHOD parse_filename.

    IF iv_str CA '/'.
      FIND REGEX '(.*/)(.*)' IN iv_str
        SUBMATCHES ev_path ev_filename.
      IF sy-subrc <> 0.
        zcx_abapgit_exception=>raise( 'Malformed path' ).
      ENDIF.
      IF ev_path <> '/'.
        CONCATENATE '/' ev_path INTO ev_path.
      ENDIF.
    ELSE.
      ev_path = '/'.
      ev_filename = iv_str.
    ENDIF.
    TRANSLATE ev_filename TO LOWER CASE.

  ENDMETHOD.

  METHOD normalize_path.
* removes first folder from path if needed

    DATA: lt_split  TYPE TABLE OF string,
          lv_needed TYPE abap_bool,
          lv_length TYPE i,
          lv_split  LIKE LINE OF lt_split.

    FIELD-SYMBOLS: <ls_file> LIKE LINE OF ct_files.


    READ TABLE ct_files INDEX 1 ASSIGNING <ls_file>.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    SPLIT <ls_file>-path AT '/' INTO TABLE lt_split.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.
    READ TABLE lt_split INDEX 2 INTO lv_split.
    IF sy-subrc <> 0 OR strlen( lv_split ) = 0.
      RETURN.
    ENDIF.

    CONCATENATE '/' lv_split '/*' INTO lv_split.

    lv_needed = abap_true.
    LOOP AT ct_files ASSIGNING <ls_file>.
      IF NOT <ls_file>-path CP lv_split.
        lv_needed = abap_false.
        EXIT. " current loop
      ENDIF.
    ENDLOOP.

    IF lv_needed = abap_true.
      lv_length = strlen( lv_split ) - 2.
      LOOP AT ct_files ASSIGNING <ls_file>.
        <ls_file>-path = <ls_file>-path+lv_length.
      ENDLOOP.
    ENDIF.

  ENDMETHOD.

  METHOD create.

    CREATE OBJECT ro_connector.
    ro_connector->mv_filepath = iv_path.
    ro_connector->mv_blob     = zcl_abapgit_fs=>file_upload( iv_path ).

  ENDMETHOD.

ENDCLASS.
