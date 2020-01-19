CLASS zcl_abapgit_file_filter DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    CLASS-METHODS:
      register
        IMPORTING
          iv_id          TYPE string
          io_file_filter TYPE REF TO zif_abapgit_file_filter,

      filter_remote_files
        CHANGING
          ct_files TYPE zif_abapgit_definitions=>ty_files_tt,

      filter_local_files
        CHANGING
          ct_files TYPE zif_abapgit_definitions=>ty_files_item_tt.

  PRIVATE SECTION.
    TYPES:
      BEGIN OF ty_filter,
        id     TYPE string,
        filter TYPE REF TO zif_abapgit_file_filter,
      END OF ty_filter,
      tty_file_filters TYPE SORTED TABLE OF ty_filter
                            WITH UNIQUE KEY id.

    CLASS-DATA:
      gt_file_filters TYPE tty_file_filters.

ENDCLASS.


CLASS zcl_abapgit_file_filter IMPLEMENTATION.

  METHOD register.

    DATA: ls_filter LIKE LINE OF gt_file_filters.

    READ TABLE gt_file_filters TRANSPORTING NO FIELDS
                               WITH TABLE KEY id = iv_id.
    IF sy-subrc <> 0.
      ls_filter-id     = iv_id.
      ls_filter-filter = io_file_filter.
      INSERT ls_filter INTO TABLE gt_file_filters.
    ENDIF.

  ENDMETHOD.


  METHOD filter_remote_files.

    DATA: li_exit TYPE REF TO zif_abapgit_exit.
    FIELD-SYMBOLS: <ls_file_filter> LIKE LINE OF gt_file_filters.

    LOOP AT gt_file_filters ASSIGNING <ls_file_filter>.
      <ls_file_filter>-filter->filter_remote_files(
        CHANGING
          ct_files = ct_files ).
    ENDLOOP.

    li_exit = zcl_abapgit_exit=>get_instance( ).
    li_exit->filter_remote_files(
      CHANGING
        ct_files = ct_files ).

  ENDMETHOD.


  METHOD filter_local_files.

    DATA: li_exit TYPE REF TO zif_abapgit_exit.
    FIELD-SYMBOLS: <ls_file_filter> LIKE LINE OF gt_file_filters.

    LOOP AT gt_file_filters ASSIGNING <ls_file_filter>.
      <ls_file_filter>-filter->filter_local_files(
        CHANGING
          ct_files = ct_files ).
    ENDLOOP.

    li_exit = zcl_abapgit_exit=>get_instance( ).
    li_exit->filter_local_files(
      CHANGING
        ct_files = ct_files ).

  ENDMETHOD.

ENDCLASS.
