CLASS zcl_abapgit_aunit_objects DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.

    "! Objects of the repository to pass to ABAP Unit, includes are skipped
    "! as they cannot be run on their own
    CLASS-METHODS get
      IMPORTING
        !ii_repo        TYPE REF TO zif_abapgit_repo
      RETURNING
        VALUE(rt_tadir) TYPE zif_abapgit_definitions=>ty_tadir_tt
      RAISING
        zcx_abapgit_exception.

  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES ty_programs_tt TYPE SORTED TABLE OF progname WITH UNIQUE KEY table_line.

    CLASS-METHODS select_includes
      IMPORTING
        !it_tadir          TYPE zif_abapgit_definitions=>ty_tadir_tt
      RETURNING
        VALUE(rt_includes) TYPE ty_programs_tt.

ENDCLASS.



CLASS zcl_abapgit_aunit_objects IMPLEMENTATION.


  METHOD get.

    DATA lt_includes TYPE ty_programs_tt.

    FIELD-SYMBOLS <ls_tadir> LIKE LINE OF rt_tadir.

    rt_tadir = ii_repo->get_tadir_objects( ).

    lt_includes = select_includes( rt_tadir ).

    LOOP AT rt_tadir ASSIGNING <ls_tadir> WHERE object = 'PROG'.
      READ TABLE lt_includes WITH TABLE KEY table_line = <ls_tadir>-obj_name TRANSPORTING NO FIELDS.
      IF sy-subrc = 0.
        DELETE rt_tadir.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD select_includes.

    DATA lt_programs TYPE ty_programs_tt.
    DATA lv_program  TYPE progname.

    FIELD-SYMBOLS <ls_tadir> LIKE LINE OF it_tadir.

    LOOP AT it_tadir ASSIGNING <ls_tadir> WHERE object = 'PROG'.
      lv_program = <ls_tadir>-obj_name.
      INSERT lv_program INTO TABLE lt_programs.
    ENDLOOP.

    IF lt_programs IS INITIAL.
      RETURN.
    ENDIF.

    SELECT progname FROM reposrc
      INTO TABLE rt_includes
      FOR ALL ENTRIES IN lt_programs
      WHERE progname = lt_programs-table_line
      AND r3state = 'A'
      AND subc = 'I'.                                   "#EC CI_SUBRC

  ENDMETHOD.
ENDCLASS.
