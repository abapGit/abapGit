CLASS zcl_abapgit_tar DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    CLASS-METHODS decode
      IMPORTING
        !iv_tar                 TYPE xstring
        !iv_require_repo_marker TYPE abap_bool DEFAULT abap_false
      RETURNING
        VALUE(rt_files)         TYPE zif_abapgit_git_definitions=>ty_files_tt
      RAISING
        zcx_abapgit_exception .

  PRIVATE SECTION.
    TYPES:
      BEGIN OF ty_location,
        path     TYPE string,
        filename TYPE string,
      END OF ty_location .

    CONSTANTS:
      c_block_size       TYPE i VALUE 512,
      c_max_archive_size TYPE i VALUE 52428800,
      c_max_file_size    TYPE i VALUE 10485760,
      c_max_entries      TYPE i VALUE 10000.

    CLASS-METHODS read_text_field
      IMPORTING
        !iv_data       TYPE xstring
        !iv_offset     TYPE i
        !iv_length     TYPE i
      RETURNING
        VALUE(rv_text) TYPE string
      RAISING
        zcx_abapgit_exception .
    CLASS-METHODS read_octal_field
      IMPORTING
        !iv_data        TYPE xstring
        !iv_offset      TYPE i
        !iv_length      TYPE i
        !iv_maximum     TYPE i
      RETURNING
        VALUE(rv_value) TYPE i
      RAISING
        zcx_abapgit_exception .
    CLASS-METHODS validate_header
      IMPORTING
        !iv_tar    TYPE xstring
        !iv_offset TYPE i
      RAISING
        zcx_abapgit_exception .
    CLASS-METHODS is_zero_block
      IMPORTING
        !iv_data       TYPE xstring
        !iv_offset     TYPE i
      RETURNING
        VALUE(rv_zero) TYPE abap_bool .
    CLASS-METHODS normalize_path
      IMPORTING
        !iv_name           TYPE string
        !iv_prefix         TYPE string
        !iv_is_directory   TYPE abap_bool
      RETURNING
        VALUE(rs_location) TYPE ty_location
      RAISING
        zcx_abapgit_exception .
ENDCLASS.



CLASS zcl_abapgit_tar IMPLEMENTATION.

  METHOD decode.

    DATA: lv_archive_length TYPE i,
          lv_offset         TYPE i,
          lv_payload_offset TYPE i,
          lv_payload_length TYPE i,
          lv_size           TYPE i,
          lv_entries        TYPE i,
          lv_tar_type       TYPE string,
          lv_name           TYPE string,
          lv_prefix         TYPE string,
          ls_location       TYPE ty_location,
          lv_data           TYPE xstring,
          lv_zero           TYPE abap_bool,
          lv_byte           TYPE x LENGTH 1,
          lv_has_root_marker TYPE abap_bool.

    FIELD-SYMBOLS <ls_file> LIKE LINE OF rt_files.

    lv_archive_length = xstrlen( iv_tar ).
    IF lv_archive_length < c_block_size * 2 OR
       lv_archive_length > c_max_archive_size.
      zcx_abapgit_exception=>raise(
        |TAR archive size must be between 1024 bytes and { c_max_archive_size } bytes| ).
    ENDIF.

    lv_offset = 0.
    WHILE lv_offset <= lv_archive_length - c_block_size.

      IF is_zero_block( iv_data   = iv_tar
                        iv_offset = lv_offset ) = abap_true.
        IF lv_offset > lv_archive_length - c_block_size * 2 OR
           is_zero_block( iv_data   = iv_tar
                          iv_offset = lv_offset + c_block_size ) = abap_false.
          zcx_abapgit_exception=>raise( 'TAR archive has an invalid end marker' ).
        ENDIF.

        lv_offset = lv_offset + c_block_size * 2.
        WHILE lv_offset < lv_archive_length.
          lv_byte = iv_tar+lv_offset(1).
          IF lv_byte <> '00'.
            zcx_abapgit_exception=>raise( 'TAR archive contains data after its end marker' ).
          ENDIF.
          lv_offset = lv_offset + 1.
        ENDWHILE.

        IF iv_require_repo_marker = abap_true AND lv_has_root_marker = abap_false.
          zcx_abapgit_exception=>raise( 'TAR archive must contain .abapgit.xml at its root' ).
        ENDIF.
        RETURN.
      ENDIF.

      IF lv_archive_length - lv_offset < c_block_size.
        zcx_abapgit_exception=>raise( 'TAR archive contains a truncated header' ).
      ENDIF.

      validate_header( iv_tar    = iv_tar
                       iv_offset = lv_offset ).

      lv_entries = lv_entries + 1.
      IF lv_entries > c_max_entries.
        zcx_abapgit_exception=>raise( |TAR archive exceeds the limit of { c_max_entries } entries| ).
      ENDIF.

      lv_name = read_text_field( iv_data   = iv_tar
                                 iv_offset = lv_offset
                                 iv_length = 100 ).
      lv_prefix = read_text_field( iv_data   = iv_tar
                                   iv_offset = lv_offset + 345
                                   iv_length = 155 ).
      lv_tar_type = read_text_field( iv_data   = iv_tar
                                     iv_offset = lv_offset + 156
                                     iv_length = 1 ).

      IF lv_name IS INITIAL.
        zcx_abapgit_exception=>raise( 'TAR entry has an empty name' ).
      ENDIF.

      IF lv_tar_type IS INITIAL OR lv_tar_type = '0'.
        CLEAR lv_zero.
      ELSEIF lv_tar_type = '5'.
        lv_zero = abap_true.
      ELSE.
        zcx_abapgit_exception=>raise( |TAR entry type "{ lv_tar_type }" is not supported| ).
      ENDIF.

      lv_size = read_octal_field(
        iv_data    = iv_tar
        iv_offset  = lv_offset + 124
        iv_length  = 12
        iv_maximum = c_max_file_size ).

      IF lv_zero = abap_true AND lv_size <> 0.
        zcx_abapgit_exception=>raise( 'TAR directory entries must be empty' ).
      ENDIF.

      ls_location = normalize_path(
        iv_name         = lv_name
        iv_prefix       = lv_prefix
        iv_is_directory = lv_zero ).

      lv_payload_offset = lv_offset + c_block_size.
      lv_payload_length = ( ( lv_size + c_block_size - 1 ) DIV c_block_size ) * c_block_size.
      IF lv_payload_offset > lv_archive_length OR
         lv_payload_length > lv_archive_length - lv_payload_offset.
        zcx_abapgit_exception=>raise( |TAR entry "{ lv_name }" has a truncated payload| ).
      ENDIF.

      IF lv_zero = abap_false.
        CLEAR lv_data.
        IF lv_size > 0.
          lv_data = iv_tar+lv_payload_offset(lv_size).
        ENDIF.

        READ TABLE rt_files TRANSPORTING NO FIELDS
          WITH TABLE KEY file_path COMPONENTS
            path     = ls_location-path
            filename = ls_location-filename.
        IF sy-subrc = 0.
          zcx_abapgit_exception=>raise(
            |TAR archive contains duplicate file "{ ls_location-path }{ ls_location-filename }"| ).
        ENDIF.

        APPEND INITIAL LINE TO rt_files ASSIGNING <ls_file>.
        <ls_file>-path = ls_location-path.
        <ls_file>-filename = ls_location-filename.
        <ls_file>-data = lv_data.
        <ls_file>-sha1 = zcl_abapgit_hash=>sha1_blob( lv_data ).

        IF ls_location-path = '/' AND ls_location-filename = '.abapgit.xml'.
          lv_has_root_marker = abap_true.
        ENDIF.
      ENDIF.

      lv_offset = lv_payload_offset + lv_payload_length.
    ENDWHILE.

    zcx_abapgit_exception=>raise( 'TAR archive is missing its end marker' ).

  ENDMETHOD.


  METHOD read_text_field.

    DATA: lv_field_length TYPE i,
          lv_index        TYPE i,
          lv_byte         TYPE x LENGTH 1,
          lv_field        TYPE xstring.

    IF iv_offset < 0 OR iv_length < 0 OR
       iv_offset > xstrlen( iv_data ) OR
       iv_length > xstrlen( iv_data ) - iv_offset.
      zcx_abapgit_exception=>raise( 'TAR header field is outside the archive' ).
    ENDIF.

    lv_field_length = 0.
    DO iv_length TIMES.
      lv_index = iv_offset + sy-index - 1.
      lv_byte = iv_data+lv_index(1).
      IF lv_byte = '00'.
        EXIT.
      ENDIF.
      lv_field_length = lv_field_length + 1.
    ENDDO.

    IF lv_field_length > 0.
      lv_field = iv_data+iv_offset(lv_field_length).
      rv_text = zcl_abapgit_convert=>xstring_to_string_utf8_raw( lv_field ).
    ENDIF.

  ENDMETHOD.


  METHOD read_octal_field.

    DATA: lv_text      TYPE string,
          lv_digit     TYPE c LENGTH 1,
          lv_digit_int TYPE i,
          lv_index     TYPE i.

    lv_text = read_text_field( iv_data   = iv_data
                               iv_offset = iv_offset
                               iv_length = iv_length ).

    WHILE lv_text IS NOT INITIAL.
      IF lv_text+0(1) <> ` `.
        EXIT.
      ENDIF.
      lv_text = lv_text+1.
    ENDWHILE.
    WHILE lv_text IS NOT INITIAL.
      lv_index = strlen( lv_text ) - 1.
      IF lv_text+lv_index(1) <> ` `.
        EXIT.
      ENDIF.
      lv_text = lv_text(lv_index).
    ENDWHILE.

    IF lv_text IS INITIAL.
      zcx_abapgit_exception=>raise( 'TAR numeric field is empty' ).
    ENDIF.
    IF lv_text CS ` `.
      zcx_abapgit_exception=>raise( 'TAR numeric field contains an embedded space' ).
    ENDIF.

    rv_value = 0.
    DO strlen( lv_text ) TIMES.
      lv_index = sy-index - 1.
      lv_digit = lv_text+lv_index(1).
      IF lv_digit < '0' OR lv_digit > '7'.
        zcx_abapgit_exception=>raise( 'TAR header contains a non-octal numeric field' ).
      ENDIF.
      lv_digit_int = lv_digit.
      IF rv_value > ( iv_maximum - lv_digit_int ) DIV 8.
        zcx_abapgit_exception=>raise( 'TAR numeric field exceeds its supported limit' ).
      ENDIF.
      rv_value = rv_value * 8 + lv_digit_int.
    ENDDO.

  ENDMETHOD.


  METHOD validate_header.

    DATA: lv_expected TYPE i,
          lv_actual   TYPE i,
          lv_index    TYPE i,
          lv_byte     TYPE xstring.

    IF read_text_field( iv_data   = iv_tar
                        iv_offset = iv_offset + 257
                        iv_length = 6 ) <> 'ustar' OR
       read_text_field( iv_data   = iv_tar
                        iv_offset = iv_offset + 263
                        iv_length = 2 ) <> '00'.
      zcx_abapgit_exception=>raise( 'TAR entry is not POSIX USTAR format' ).
    ENDIF.

    lv_expected = read_octal_field(
      iv_data    = iv_tar
      iv_offset  = iv_offset + 148
      iv_length  = 8
      iv_maximum = 131072 ).

    lv_actual = 0.
    DO c_block_size TIMES.
      lv_index = iv_offset + sy-index - 1.
      IF lv_index >= iv_offset + 148 AND lv_index < iv_offset + 156.
        lv_actual = lv_actual + 32.
      ELSE.
        lv_byte = iv_tar+lv_index(1).
        lv_actual = lv_actual + zcl_abapgit_convert=>xstring_to_int( lv_byte ).
      ENDIF.
    ENDDO.

    IF lv_expected <> lv_actual.
      zcx_abapgit_exception=>raise( 'TAR header checksum does not match' ).
    ENDIF.

  ENDMETHOD.


  METHOD is_zero_block.

    DATA: lv_index TYPE i,
          lv_byte  TYPE x LENGTH 1.

    rv_zero = abap_true.
    DO c_block_size TIMES.
      lv_index = iv_offset + sy-index - 1.
      lv_byte = iv_data+lv_index(1).
      IF lv_byte <> '00'.
        rv_zero = abap_false.
        RETURN.
      ENDIF.
    ENDDO.

  ENDMETHOD.


  METHOD normalize_path.

    DATA: lv_path      TYPE string,
          lv_component TYPE string,
          lv_folder     TYPE string,
          lv_length     TYPE i,
          lv_index      TYPE i,
          lv_char       TYPE c LENGTH 1,
          lt_components TYPE string_table.

    lv_path = iv_name.
    IF iv_prefix IS NOT INITIAL.
      CONCATENATE iv_prefix lv_path INTO lv_path SEPARATED BY '/'.
    ENDIF.

    DO strlen( lv_path ) TIMES.
      lv_index = sy-index - 1.
      lv_char = lv_path+lv_index(1).
      IF lv_char < space.
        zcx_abapgit_exception=>raise( 'TAR entry path contains a control character' ).
      ENDIF.
    ENDDO.

    WHILE lv_path IS NOT INITIAL.
      IF strlen( lv_path ) < 2.
        EXIT.
      ENDIF.
      IF lv_path+0(2) <> './'.
        EXIT.
      ENDIF.
      lv_path = lv_path+2.
    ENDWHILE.

    IF iv_is_directory = abap_true AND
       ( lv_path IS INITIAL OR lv_path = '.' ).
      rs_location-path = '/'.
      RETURN.
    ENDIF.

    IF lv_path IS INITIAL.
      zcx_abapgit_exception=>raise( 'TAR entry has an absolute or empty path' ).
    ENDIF.
    IF lv_path+0(1) = '/' OR lv_path CA '\'.
      zcx_abapgit_exception=>raise( 'TAR entry has an absolute or empty path' ).
    ENDIF.

    lv_length = strlen( lv_path ).
    IF lv_path CS ':'.
      zcx_abapgit_exception=>raise( 'TAR entry contains a drive path' ).
    ENDIF.

    IF iv_is_directory = abap_true.
      lv_length = strlen( lv_path ).
      IF lv_length > 0.
        lv_length = lv_length - 1.
        IF lv_path+lv_length(1) = '/'.
          lv_path = lv_path(lv_length).
        ENDIF.
      ENDIF.
    ELSE.
      lv_length = strlen( lv_path ) - 1.
      IF lv_path+lv_length(1) = '/'.
        zcx_abapgit_exception=>raise( 'TAR file path must not end with a separator' ).
      ENDIF.
    ENDIF.

    IF lv_path IS INITIAL.
      zcx_abapgit_exception=>raise( 'TAR entry has an empty path' ).
    ENDIF.

    SPLIT lv_path AT '/' INTO TABLE lt_components.
    LOOP AT lt_components INTO lv_component.
      IF lv_component IS INITIAL OR lv_component = '.' OR lv_component = '..'.
        zcx_abapgit_exception=>raise( 'TAR entry path contains an invalid segment' ).
      ENDIF.

      IF sy-tabix = lines( lt_components ).
        IF iv_is_directory = abap_false.
          rs_location-filename = lv_component.
        ENDIF.
      ELSE.
        CONCATENATE lv_folder lv_component '/' INTO lv_folder.
      ENDIF.
    ENDLOOP.

    IF iv_is_directory = abap_false.
      TRANSLATE rs_location-filename TO LOWER CASE.
      rs_location-path = |/{ lv_folder }|.
    ENDIF.

  ENDMETHOD.

ENDCLASS.
