CLASS zcl_abapgit_repo_filter DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS apply
      IMPORTING
        it_filter TYPE zif_abapgit_definitions=>ty_tadir_tt
      CHANGING
        ct_tadir  TYPE zif_abapgit_definitions=>ty_tadir_tt .

    METHODS apply_object_filter
      IMPORTING
        it_filter   TYPE zif_abapgit_definitions=>ty_tadir_tt
        io_dot      TYPE REF TO zcl_abapgit_dot_abapgit OPTIONAL
        iv_devclass TYPE devclass OPTIONAL
      CHANGING
        ct_files    TYPE zif_abapgit_git_definitions=>ty_files_tt
      RAISING
        zcx_abapgit_exception.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_abapgit_repo_filter IMPLEMENTATION.


  METHOD apply.

    DATA: lt_filter TYPE SORTED TABLE OF zif_abapgit_definitions=>ty_tadir
                      WITH NON-UNIQUE KEY object obj_name,
          lv_index  TYPE i.

    FIELD-SYMBOLS: <ls_tadir> LIKE LINE OF ct_tadir.

    IF lines( it_filter ) = 0.
      RETURN.
    ENDIF.

    lt_filter = it_filter.

* this is another loop at TADIR, but typically the filter is blank
    LOOP AT ct_tadir ASSIGNING <ls_tadir>.
      lv_index = sy-tabix.
      READ TABLE lt_filter TRANSPORTING NO FIELDS WITH KEY object = <ls_tadir>-object
                                                           obj_name = <ls_tadir>-obj_name
                                                  BINARY SEARCH.
      IF sy-subrc <> 0.
        DELETE ct_tadir INDEX lv_index.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD apply_object_filter.
    DATA lr_file TYPE REF TO zif_abapgit_git_definitions=>ty_file.
    DATA ls_item TYPE zif_abapgit_definitions=>ty_item.
    DATA ls_tadir TYPE zif_abapgit_definitions=>ty_tadir.
    DATA lt_tadir TYPE zif_abapgit_definitions=>ty_tadir_tt.
    DATA lt_filter TYPE SORTED TABLE OF zif_abapgit_definitions=>ty_tadir
                      WITH NON-UNIQUE KEY object obj_name.

    lt_filter = it_filter.

    LOOP AT ct_files REFERENCE INTO lr_file.
      IF lr_file->filename = zif_abapgit_definitions=>c_dot_abapgit.
        CONTINUE.
      ENDIF.

      zcl_abapgit_filename_logic=>file_to_object(
        EXPORTING
          iv_filename = lr_file->filename
          iv_path     = lr_file->path
          iv_devclass = iv_devclass
          io_dot      = io_dot
        IMPORTING
          es_item     = ls_item ).

      CLEAR lt_tadir.
      CLEAR ls_tadir.

      ls_tadir-object = ls_item-obj_type.
      ls_tadir-obj_name = ls_item-obj_name.
      ls_tadir-devclass = ls_item-devclass.

      INSERT ls_tadir INTO TABLE lt_tadir.

      READ TABLE lt_filter TRANSPORTING NO FIELDS
      WITH KEY object = ls_tadir-object
               obj_name = ls_tadir-obj_name
      BINARY SEARCH.

      IF sy-subrc <> 0.
        DELETE ct_files.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.
ENDCLASS.
