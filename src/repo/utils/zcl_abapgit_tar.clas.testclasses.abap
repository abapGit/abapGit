CLASS ltcl_tar DEFINITION FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS FINAL.

  PRIVATE SECTION.
    TYPES:
      BEGIN OF ty_entry,
        name   TYPE string,
        prefix TYPE string,
        data   TYPE xstring,
        type   TYPE c LENGTH 1,
      END OF ty_entry.
    TYPES ty_entry_tt TYPE STANDARD TABLE OF ty_entry WITH DEFAULT KEY.

    CONSTANTS c_block_size TYPE i VALUE 512.

    METHODS decode_snapshot FOR TESTING RAISING zcx_abapgit_exception.
    METHODS reject_bad_checksum FOR TESTING RAISING zcx_abapgit_exception.
    METHODS reject_traversal FOR TESTING RAISING zcx_abapgit_exception.
    METHODS reject_control_character FOR TESTING RAISING zcx_abapgit_exception.
    METHODS reject_unsupported_entry FOR TESTING RAISING zcx_abapgit_exception.
    METHODS reject_missing_repo_marker FOR TESTING RAISING zcx_abapgit_exception.
    METHODS reject_duplicate_paths FOR TESTING RAISING zcx_abapgit_exception.
    METHODS reject_trailing_separator FOR TESTING RAISING zcx_abapgit_exception.
    METHODS reject_truncated_end FOR TESTING RAISING zcx_abapgit_exception.
    METHODS make_archive
      IMPORTING
        it_entries        TYPE ty_entry_tt
      RETURNING
        VALUE(rv_archive) TYPE xstring
      RAISING
        zcx_abapgit_exception.
    METHODS make_header
      IMPORTING
        is_entry         TYPE ty_entry
      RETURNING
        VALUE(rv_header) TYPE xstring
      RAISING
        zcx_abapgit_exception.
    METHODS set_field
      IMPORTING
        iv_offset TYPE i
        iv_length TYPE i
        iv_value  TYPE xstring
      CHANGING
        cv_data   TYPE xstring
      RAISING
        zcx_abapgit_exception.
    METHODS octal_text
      IMPORTING
        iv_number      TYPE i
        iv_width       TYPE i
      RETURNING
        VALUE(rv_text) TYPE string.
    METHODS assert_rejected
      IMPORTING
        iv_archive             TYPE xstring
        iv_require_repo_marker TYPE abap_bool DEFAULT abap_false.
ENDCLASS.



CLASS ltcl_tar IMPLEMENTATION.

  METHOD decode_snapshot.

    DATA: lt_entries TYPE ty_entry_tt,
          lt_files   TYPE zif_abapgit_git_definitions=>ty_files_tt,
          ls_entry   TYPE ty_entry,
          ls_file    TYPE zif_abapgit_git_definitions=>ty_file,
          lv_archive TYPE xstring.

    ls_entry-name = './'.
    ls_entry-type = '5'.
    APPEND ls_entry TO lt_entries.

    CLEAR ls_entry.
    ls_entry-name = '.abapgit.xml'.
    ls_entry-data = '3C783E3C2F783E'.
    APPEND ls_entry TO lt_entries.

    CLEAR ls_entry.
    ls_entry-name = 'binary.bin'.
    ls_entry-prefix = 'src'.
    ls_entry-data = '00FF2E41'.
    APPEND ls_entry TO lt_entries.

    CLEAR ls_entry.
    ls_entry-name = './src/empty.txt'.
    APPEND ls_entry TO lt_entries.

    CLEAR ls_entry.
    ls_entry-name = 'src/'.
    ls_entry-type = '5'.
    APPEND ls_entry TO lt_entries.

    lv_archive = make_archive( lt_entries ).
    lt_files = zcl_abapgit_tar=>decode(
      iv_tar                 = lv_archive
      iv_require_repo_marker = abap_true ).

    cl_abap_unit_assert=>assert_equals(
      act = lines( lt_files )
      exp = 3 ).

    READ TABLE lt_files INTO ls_file
      WITH TABLE KEY file_path COMPONENTS
        path     = '/src/'
        filename = 'binary.bin'.
    cl_abap_unit_assert=>assert_subrc( exp = 0 ).
    cl_abap_unit_assert=>assert_equals(
      act = ls_file-data
      exp = '00FF2E41' ).
    cl_abap_unit_assert=>assert_equals(
      act = ls_file-sha1
      exp = zcl_abapgit_hash=>sha1_blob( ls_file-data ) ).

    READ TABLE lt_files INTO ls_file
      WITH TABLE KEY file_path COMPONENTS
        path     = '/src/'
        filename = 'empty.txt'.
    cl_abap_unit_assert=>assert_subrc( exp = 0 ).
    cl_abap_unit_assert=>assert_initial( ls_file-data ).

  ENDMETHOD.


  METHOD reject_bad_checksum.

    DATA: lt_entries TYPE ty_entry_tt,
          ls_entry   TYPE ty_entry,
          lv_archive TYPE xstring,
          lv_value   TYPE xstring.

    ls_entry-name = 'file.txt'.
    ls_entry-data = '41'.
    APPEND ls_entry TO lt_entries.
    lv_archive = make_archive( lt_entries ).

    lv_value = '42'.
    set_field( EXPORTING iv_offset = 0
                         iv_length = 1
                         iv_value  = lv_value
               CHANGING  cv_data   = lv_archive ).
    assert_rejected( lv_archive ).

  ENDMETHOD.


  METHOD reject_traversal.

    DATA: lt_entries TYPE ty_entry_tt,
          ls_entry   TYPE ty_entry.

    ls_entry-name = '../escape.txt'.
    ls_entry-data = '41'.
    APPEND ls_entry TO lt_entries.
    assert_rejected( make_archive( lt_entries ) ).

  ENDMETHOD.


  METHOD reject_control_character.

    DATA: lt_entries TYPE ty_entry_tt,
          ls_entry   TYPE ty_entry.

    ls_entry-name = 'bad' && cl_abap_char_utilities=>newline && 'name'.
    ls_entry-data = '41'.
    APPEND ls_entry TO lt_entries.
    assert_rejected( make_archive( lt_entries ) ).

  ENDMETHOD.


  METHOD reject_unsupported_entry.

    DATA: lt_entries TYPE ty_entry_tt,
          ls_entry   TYPE ty_entry.

    ls_entry-name = 'link'.
    ls_entry-type = '2'.
    APPEND ls_entry TO lt_entries.
    assert_rejected( make_archive( lt_entries ) ).

  ENDMETHOD.


  METHOD reject_missing_repo_marker.

    DATA: lt_entries TYPE ty_entry_tt,
          ls_entry   TYPE ty_entry.

    ls_entry-name = 'file.txt'.
    ls_entry-data = '41'.
    APPEND ls_entry TO lt_entries.
    assert_rejected(
      iv_archive             = make_archive( lt_entries )
      iv_require_repo_marker = abap_true ).

  ENDMETHOD.


  METHOD reject_duplicate_paths.

    DATA: lt_entries TYPE ty_entry_tt,
          ls_entry   TYPE ty_entry.

    ls_entry-name = './src/file.txt'.
    ls_entry-data = '41'.
    APPEND ls_entry TO lt_entries.

    CLEAR ls_entry.
    ls_entry-name = 'src/file.txt'.
    ls_entry-data = '42'.
    APPEND ls_entry TO lt_entries.

    assert_rejected( make_archive( lt_entries ) ).

  ENDMETHOD.


  METHOD reject_trailing_separator.

    DATA: lt_entries TYPE ty_entry_tt,
          ls_entry   TYPE ty_entry.

    ls_entry-name = 'file/'.
    ls_entry-data = '41'.
    APPEND ls_entry TO lt_entries.

    assert_rejected( make_archive( lt_entries ) ).

  ENDMETHOD.


  METHOD reject_truncated_end.

    DATA: lt_entries TYPE ty_entry_tt,
          ls_entry   TYPE ty_entry,
          lv_archive TYPE xstring,
          lv_size    TYPE i.

    ls_entry-name = 'file.txt'.
    ls_entry-data = '41'.
    APPEND ls_entry TO lt_entries.
    lv_archive = make_archive( lt_entries ).

    lv_size = xstrlen( lv_archive ) - c_block_size.
    lv_archive = lv_archive+0(lv_size).
    assert_rejected( lv_archive ).

  ENDMETHOD.


  METHOD make_archive.

    DATA: ls_entry       TYPE ty_entry,
          lv_header      TYPE xstring,
          lv_data_length TYPE i,
          lv_padding     TYPE i,
          lv_zero        TYPE x LENGTH 1 VALUE '00'.

    FIELD-SYMBOLS <ls_entry> LIKE LINE OF it_entries.

    LOOP AT it_entries ASSIGNING <ls_entry>.
      ls_entry = <ls_entry>.
      lv_header = make_header( ls_entry ).
      CONCATENATE rv_archive lv_header INTO rv_archive IN BYTE MODE.
      IF ls_entry-data IS NOT INITIAL.
        CONCATENATE rv_archive ls_entry-data INTO rv_archive IN BYTE MODE.
      ENDIF.

      lv_data_length = xstrlen( ls_entry-data ).
      lv_padding = ( c_block_size - lv_data_length MOD c_block_size ) MOD c_block_size.
      DO lv_padding TIMES.
        CONCATENATE rv_archive lv_zero INTO rv_archive IN BYTE MODE.
      ENDDO.
    ENDLOOP.

    DO c_block_size * 2 TIMES.
      CONCATENATE rv_archive lv_zero INTO rv_archive IN BYTE MODE.
    ENDDO.

  ENDMETHOD.


  METHOD make_header.

    DATA: lv_zero       TYPE x LENGTH 1 VALUE '00',
          lv_space      TYPE x LENGTH 1 VALUE '20',
          lv_sum        TYPE i,
          lv_index      TYPE i,
          lv_byte       TYPE xstring,
          lv_value      TYPE xstring,
          lv_text       TYPE string,
          lv_octal      TYPE string,
          lv_checksum   TYPE xstring,
          lv_size       TYPE i.

    DO c_block_size TIMES.
      CONCATENATE rv_header lv_zero INTO rv_header IN BYTE MODE.
    ENDDO.

    lv_value = zcl_abapgit_convert=>string_to_xstring_utf8( is_entry-name ).
    set_field( EXPORTING iv_offset = 0
                         iv_length = 100
                         iv_value  = lv_value
               CHANGING  cv_data   = rv_header ).

    lv_value = zcl_abapgit_convert=>string_to_xstring_utf8( '0000644' ).
    set_field( EXPORTING iv_offset = 100
                         iv_length = 8
                         iv_value  = lv_value
               CHANGING  cv_data   = rv_header ).

    lv_value = zcl_abapgit_convert=>string_to_xstring_utf8( '0000000' ).
    set_field( EXPORTING iv_offset = 108
                         iv_length = 8
                         iv_value  = lv_value
               CHANGING  cv_data   = rv_header ).
    set_field( EXPORTING iv_offset = 116
                         iv_length = 8
                         iv_value  = lv_value
               CHANGING  cv_data   = rv_header ).

    lv_size = xstrlen( is_entry-data ).
    lv_text = octal_text( iv_number = lv_size
                          iv_width  = 11 ).
    lv_value = zcl_abapgit_convert=>string_to_xstring_utf8( lv_text ).
    set_field( EXPORTING iv_offset = 124
                         iv_length = 12
                         iv_value  = lv_value
               CHANGING  cv_data   = rv_header ).

    lv_value = zcl_abapgit_convert=>string_to_xstring_utf8( '00000000000' ).
    set_field( EXPORTING iv_offset = 136
                         iv_length = 12
                         iv_value  = lv_value
               CHANGING  cv_data   = rv_header ).

    DO 8 TIMES.
      CONCATENATE lv_checksum lv_space INTO lv_checksum IN BYTE MODE.
    ENDDO.
    set_field( EXPORTING iv_offset = 148
                         iv_length = 8
                         iv_value  = lv_checksum
               CHANGING  cv_data   = rv_header ).

    lv_text = is_entry-type.
    IF is_entry-type IS INITIAL.
      lv_text = '0'.
    ENDIF.
    lv_value = zcl_abapgit_convert=>string_to_xstring_utf8( lv_text ).
    set_field( EXPORTING iv_offset = 156
                         iv_length = 1
                         iv_value  = lv_value
               CHANGING  cv_data   = rv_header ).

    lv_value = zcl_abapgit_convert=>string_to_xstring_utf8( 'ustar' ).
    set_field( EXPORTING iv_offset = 257
                         iv_length = 6
                         iv_value  = lv_value
               CHANGING  cv_data   = rv_header ).
    lv_value = zcl_abapgit_convert=>string_to_xstring_utf8( '00' ).
    set_field( EXPORTING iv_offset = 263
                         iv_length = 2
                         iv_value  = lv_value
               CHANGING  cv_data   = rv_header ).

    IF is_entry-prefix IS NOT INITIAL.
      lv_value = zcl_abapgit_convert=>string_to_xstring_utf8( is_entry-prefix ).
      set_field( EXPORTING iv_offset = 345
                           iv_length = 155
                           iv_value  = lv_value
                 CHANGING  cv_data   = rv_header ).
    ENDIF.

    lv_sum = 0.
    DO c_block_size TIMES.
      lv_index = sy-index - 1.
      IF lv_index >= 148 AND lv_index < 156.
        lv_sum = lv_sum + 32.
      ELSE.
        lv_byte = rv_header+lv_index(1).
        lv_sum = lv_sum + zcl_abapgit_convert=>xstring_to_int( lv_byte ).
      ENDIF.
    ENDDO.

    lv_octal = octal_text( iv_number = lv_sum
                           iv_width  = 6 ).
    lv_text = lv_octal && `  `.
    lv_checksum = zcl_abapgit_convert=>string_to_xstring_utf8( lv_text ).
    set_field( EXPORTING iv_offset = 148
                         iv_length = 8
                         iv_value  = lv_checksum
               CHANGING  cv_data   = rv_header ).

  ENDMETHOD.


  METHOD set_field.

    DATA: lv_field         TYPE xstring,
          lv_zero          TYPE x LENGTH 1 VALUE '00',
          lv_before        TYPE xstring,
          lv_after         TYPE xstring,
          lv_result        TYPE xstring,
          lv_original_size TYPE i,
          lv_after_offset  TYPE i.

    lv_original_size = xstrlen( cv_data ).
    IF iv_offset < 0 OR iv_length < 0 OR
       iv_offset > lv_original_size OR
       iv_length > lv_original_size - iv_offset.
      zcx_abapgit_exception=>raise( 'Test TAR field is outside its header buffer' ).
    ENDIF.

    IF xstrlen( iv_value ) > iv_length.
      zcx_abapgit_exception=>raise( 'Test TAR field does not fit in its header slot' ).
    ENDIF.

    lv_field = iv_value.
    DO iv_length - xstrlen( lv_field ) TIMES.
      CONCATENATE lv_field lv_zero INTO lv_field IN BYTE MODE.
    ENDDO.

    IF iv_offset > 0.
      lv_before = cv_data+0(iv_offset).
    ENDIF.
    lv_after_offset = iv_offset + iv_length.
    IF lv_after_offset < lv_original_size.
      lv_after = cv_data+lv_after_offset.
    ENDIF.

    CONCATENATE lv_before lv_field lv_after INTO lv_result IN BYTE MODE.
    cv_data = lv_result.

  ENDMETHOD.


  METHOD octal_text.

    DATA: lv_number TYPE i,
          lv_digit  TYPE i,
          lv_char   TYPE c LENGTH 1.

    lv_number = iv_number.
    WHILE lv_number > 0.
      lv_digit = lv_number MOD 8.
      lv_char = lv_digit.
      rv_text = lv_char && rv_text.
      lv_number = lv_number DIV 8.
    ENDWHILE.

    IF rv_text IS INITIAL.
      rv_text = '0'.
    ENDIF.

    WHILE strlen( rv_text ) < iv_width.
      rv_text = '0' && rv_text.
    ENDWHILE.

  ENDMETHOD.


  METHOD assert_rejected.

    TRY.
        zcl_abapgit_tar=>decode(
          iv_tar                 = iv_archive
          iv_require_repo_marker = iv_require_repo_marker ).
        cl_abap_unit_assert=>fail( ).
      CATCH zcx_abapgit_exception.
        RETURN.
    ENDTRY.

  ENDMETHOD.

ENDCLASS.
