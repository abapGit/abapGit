CLASS lcl_checksum_serializer DEFINITION
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    CONSTANTS c_splitter TYPE string VALUE `|`.
    CONSTANTS c_root TYPE string VALUE `@`.

    CLASS-METHODS serialize
      IMPORTING
        it_checksums     TYPE zif_abapgit_persistence=>ty_local_checksum_tt
      RETURNING
        VALUE(rv_string) TYPE string.

    CLASS-METHODS deserialize
      IMPORTING
        iv_string           TYPE string
      RETURNING
        VALUE(rt_checksums) TYPE zif_abapgit_persistence=>ty_local_checksum_tt.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS lcl_checksum_serializer IMPLEMENTATION.


  METHOD deserialize.

    DATA lt_buf_tab TYPE string_table.
    DATA lv_buf TYPE string.
    DATA lt_checksums LIKE rt_checksums.

    FIELD-SYMBOLS <ls_cs> LIKE LINE OF lt_checksums.
    FIELD-SYMBOLS <ls_file> LIKE LINE OF <ls_cs>-files.

    SPLIT iv_string AT |\n| INTO TABLE lt_buf_tab.

    LOOP AT lt_buf_tab INTO lv_buf.
      CHECK lv_buf IS NOT INITIAL. " In fact this is a bug ... it cannot be empty, maybe raise

      IF lv_buf+0(1) = '/'.
        IF <ls_cs> IS NOT ASSIGNED.
          " Incorrect checksums structure, maybe raise, though it is not critical for execution
          RETURN.
        ENDIF.

        APPEND INITIAL LINE TO <ls_cs>-files ASSIGNING <ls_file>.
        SPLIT lv_buf AT c_splitter INTO <ls_file>-path <ls_file>-filename <ls_file>-sha1.

        IF <ls_file>-path IS INITIAL OR <ls_file>-filename IS INITIAL OR <ls_file>-sha1 IS INITIAL.
          " Incorrect checksums struture, maybe raise, though it is not critical for execution
          RETURN.
        ENDIF.
      ELSEIF lv_buf = c_root. " Root
        APPEND INITIAL LINE TO lt_checksums ASSIGNING <ls_cs>. " Empty item
      ELSE.
        APPEND INITIAL LINE TO lt_checksums ASSIGNING <ls_cs>.
        SPLIT lv_buf AT c_splitter INTO <ls_cs>-item-obj_type <ls_cs>-item-obj_name <ls_cs>-item-devclass.

        IF <ls_cs>-item-obj_type IS INITIAL OR <ls_cs>-item-obj_name IS INITIAL OR <ls_cs>-item-devclass IS INITIAL.
          " Incorrect checksums structure, maybe raise, though it is not critical for execution
          RETURN.
        ENDIF.

      ENDIF.
    ENDLOOP.

    rt_checksums = lt_checksums.

  ENDMETHOD.


  METHOD serialize.

    DATA lt_buf_tab TYPE string_table.
    DATA lv_buf TYPE string.
    DATA lt_checksums_sorted TYPE zif_abapgit_persistence=>ty_local_checksum_by_item_tt.

    FIELD-SYMBOLS <ls_cs> LIKE LINE OF it_checksums.
    FIELD-SYMBOLS <ls_file> LIKE LINE OF <ls_cs>-files.

    lt_checksums_sorted = it_checksums.

    LOOP AT lt_checksums_sorted ASSIGNING <ls_cs>.

      IF lines( <ls_cs>-files ) = 0.
        CONTINUE.
      ENDIF.

      IF <ls_cs>-item-obj_type IS NOT INITIAL.
        CONCATENATE <ls_cs>-item-obj_type <ls_cs>-item-obj_name <ls_cs>-item-devclass
          INTO lv_buf
          SEPARATED BY c_splitter.
      ELSE.
        lv_buf = c_root.
      ENDIF.
      APPEND lv_buf TO lt_buf_tab.

      LOOP AT <ls_cs>-files ASSIGNING <ls_file>.

        CONCATENATE <ls_file>-path <ls_file>-filename <ls_file>-sha1
          INTO lv_buf
          SEPARATED BY c_splitter.
        APPEND lv_buf TO lt_buf_tab.

      ENDLOOP.

    ENDLOOP.

    rv_string = concat_lines_of(
      table = lt_buf_tab
      sep   = |\n| ).

  ENDMETHOD.
ENDCLASS.

**********************************************************************
* UPDATE CALCULATOR
**********************************************************************

CLASS lcl_update_calculator DEFINITION
  FINAL
  CREATE PUBLIC.
  PUBLIC SECTION.

    CLASS-METHODS calculate_updated
      IMPORTING
        it_updated_files TYPE zif_abapgit_git_definitions=>ty_file_signatures_tt
        it_current_checksums TYPE zif_abapgit_persistence=>ty_local_checksum_tt
        it_local_files TYPE zif_abapgit_definitions=>ty_files_item_tt
      RETURNING
        VALUE(rt_checksums) TYPE zif_abapgit_persistence=>ty_local_checksum_tt.

  PRIVATE SECTION.

    CLASS-METHODS process_updated_files
      CHANGING
        ct_update_index TYPE zif_abapgit_git_definitions=>ty_file_signatures_ts
        ct_checksums    TYPE zif_abapgit_persistence=>ty_local_checksum_by_item_tt.

    CLASS-METHODS add_new_files
      IMPORTING
        it_local        TYPE zif_abapgit_definitions=>ty_files_item_tt
        it_update_index TYPE zif_abapgit_git_definitions=>ty_file_signatures_ts
      CHANGING
        ct_checksums    TYPE zif_abapgit_persistence=>ty_local_checksum_by_item_tt.

ENDCLASS.

CLASS lcl_update_calculator IMPLEMENTATION.

  METHOD calculate_updated.

    DATA lt_update_index TYPE zif_abapgit_git_definitions=>ty_file_signatures_ts.
    DATA lt_checksums_sorted TYPE zif_abapgit_persistence=>ty_local_checksum_by_item_tt.

    lt_checksums_sorted = it_current_checksums.
    lt_update_index     = it_updated_files.

    process_updated_files(
      CHANGING
        ct_update_index = lt_update_index
        ct_checksums    = lt_checksums_sorted ).

    add_new_files(
      EXPORTING
        it_update_index = lt_update_index
        it_local        = it_local_files
      CHANGING
        ct_checksums    = lt_checksums_sorted ).

    rt_checksums = lt_checksums_sorted.

  ENDMETHOD.

  METHOD process_updated_files.

    DATA lv_cs_row  TYPE i.
    DATA lv_file_row  TYPE i.

    FIELD-SYMBOLS <ls_checksum>  LIKE LINE OF ct_checksums.
    FIELD-SYMBOLS <ls_file>      LIKE LINE OF <ls_checksum>-files.
    FIELD-SYMBOLS <ls_new_state> LIKE LINE OF ct_update_index.

    " Loop through current checksum state, update sha1 for common files

    LOOP AT ct_checksums ASSIGNING <ls_checksum>.
      lv_cs_row = sy-tabix.

      LOOP AT <ls_checksum>-files ASSIGNING <ls_file>.
        lv_file_row = sy-tabix.

        READ TABLE ct_update_index ASSIGNING <ls_new_state>
          WITH KEY
            path     = <ls_file>-path
            filename = <ls_file>-filename.
        IF sy-subrc <> 0.
          CONTINUE. " Missing in updated files -> nothing to update, skip
        ENDIF.

        IF <ls_new_state>-sha1 IS INITIAL. " Empty input sha1 is a deletion marker
          DELETE <ls_checksum>-files INDEX lv_file_row.
        ELSE.
          <ls_file>-sha1 = <ls_new_state>-sha1.  " Update sha1
          CLEAR <ls_new_state>-sha1.             " Mark as processed
        ENDIF.
      ENDLOOP.

      IF lines( <ls_checksum>-files ) = 0. " Remove empty objects
        DELETE ct_checksums INDEX lv_cs_row.
      ENDIF.
    ENDLOOP.

    DELETE ct_update_index WHERE sha1 IS INITIAL. " Remove processed

  ENDMETHOD.

  METHOD add_new_files.

    DATA lt_local_sorted TYPE zif_abapgit_definitions=>ty_files_item_by_file_tt.
    DATA ls_checksum LIKE LINE OF ct_checksums.
    FIELD-SYMBOLS <ls_checksum> LIKE LINE OF ct_checksums.
    FIELD-SYMBOLS <ls_new_file> LIKE LINE OF it_update_index.
    FIELD-SYMBOLS <ls_local>    LIKE LINE OF lt_local_sorted.

    lt_local_sorted = it_local.

    " Add new files - not deleted and not marked as processed
    LOOP AT it_update_index ASSIGNING <ls_new_file>.

      READ TABLE lt_local_sorted ASSIGNING <ls_local>
        WITH KEY
          file-path     = <ls_new_file>-path
          file-filename = <ls_new_file>-filename.
      IF sy-subrc <> 0.
        " The file should be in locals, however:
        " if the deserialization fails, the local file might not be there
        " in this case no new CS added, and the file will appear to be remote+new
        CONTINUE.
      ENDIF.

      READ TABLE ct_checksums ASSIGNING <ls_checksum>
        WITH KEY
          item-obj_type = <ls_local>-item-obj_type
          item-obj_name = <ls_local>-item-obj_name.
      IF sy-subrc <> 0.
        MOVE-CORRESPONDING <ls_local>-item TO ls_checksum-item.
        INSERT ls_checksum INTO TABLE ct_checksums ASSIGNING <ls_checksum>.
      ENDIF.

      APPEND <ls_new_file> TO <ls_checksum>-files.
    ENDLOOP.

  ENDMETHOD.

ENDCLASS.
