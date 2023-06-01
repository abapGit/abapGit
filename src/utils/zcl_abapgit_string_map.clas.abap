CLASS zcl_abapgit_string_map DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES:
      BEGIN OF ty_entry,
        k TYPE string,
        v TYPE string,
      END OF ty_entry .
    TYPES:
      ty_entries TYPE SORTED TABLE OF ty_entry WITH UNIQUE KEY k .

    DATA mt_entries TYPE ty_entries READ-ONLY .

    CLASS-METHODS create
      IMPORTING
        !iv_case_insensitive TYPE abap_bool DEFAULT abap_false
      RETURNING
        VALUE(ro_instance)   TYPE REF TO zcl_abapgit_string_map .
    METHODS constructor
      IMPORTING
        !iv_case_insensitive TYPE abap_bool DEFAULT abap_false .
    METHODS get
      IMPORTING
        !iv_key       TYPE string
      RETURNING
        VALUE(rv_val) TYPE string .
    METHODS has
      IMPORTING
        !iv_key       TYPE string
      RETURNING
        VALUE(rv_has) TYPE abap_bool .
    METHODS set
      IMPORTING
        !iv_key       TYPE string
        !iv_val       TYPE csequence OPTIONAL
      RETURNING
        VALUE(ro_map) TYPE REF TO zcl_abapgit_string_map
      RAISING
        zcx_abapgit_exception .
    METHODS size
      RETURNING
        VALUE(rv_size) TYPE i .
    METHODS is_empty
      RETURNING
        VALUE(rv_yes) TYPE abap_bool .
    METHODS delete
      IMPORTING
        !iv_key TYPE string
      RAISING
        zcx_abapgit_exception .
    METHODS clear
      RAISING
        zcx_abapgit_exception .
    METHODS to_abap
      CHANGING
        !cs_container TYPE any
      RAISING
        zcx_abapgit_exception .
    METHODS strict
      IMPORTING
        !iv_strict         TYPE abap_bool DEFAULT abap_true
      RETURNING
        VALUE(ro_instance) TYPE REF TO zcl_abapgit_string_map .
    METHODS freeze .
    METHODS merge
      IMPORTING
        !io_string_map TYPE REF TO zcl_abapgit_string_map
      RETURNING
        VALUE(ro_instance) TYPE REF TO zcl_abapgit_string_map
      RAISING
        zcx_abapgit_exception .

  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA mv_read_only TYPE abap_bool.
    DATA mv_is_strict TYPE abap_bool.
    DATA mv_case_insensitive TYPE abap_bool.

ENDCLASS.



CLASS ZCL_ABAPGIT_STRING_MAP IMPLEMENTATION.


  METHOD clear.
    IF mv_read_only = abap_true.
      zcx_abapgit_exception=>raise( 'Cannot clear. This string map is immutable' ).
    ENDIF.
    CLEAR mt_entries.
  ENDMETHOD.


  METHOD constructor.
    mv_is_strict = abap_true.
    mv_case_insensitive = iv_case_insensitive.
  ENDMETHOD.


  METHOD create.
    CREATE OBJECT ro_instance
      EXPORTING
        iv_case_insensitive = iv_case_insensitive.
  ENDMETHOD.


  METHOD delete.

    IF mv_read_only = abap_true.
      zcx_abapgit_exception=>raise( 'Cannot delete. This string map is immutable' ).
    ENDIF.

    DELETE mt_entries WHERE k = iv_key.

  ENDMETHOD.


  METHOD freeze.
    mv_read_only = abap_true.
  ENDMETHOD.


  METHOD get.

    DATA lv_key LIKE iv_key.
    FIELD-SYMBOLS <ls_entry> LIKE LINE OF mt_entries.

    IF mv_case_insensitive = abap_true.
      lv_key = to_upper( iv_key ).
    ELSE.
      lv_key = iv_key.
    ENDIF.

    READ TABLE mt_entries ASSIGNING <ls_entry> WITH KEY k = lv_key.
    IF sy-subrc IS INITIAL.
      rv_val = <ls_entry>-v.
    ENDIF.

  ENDMETHOD.


  METHOD has.

    READ TABLE mt_entries TRANSPORTING NO FIELDS WITH KEY k = iv_key.
    rv_has = boolc( sy-subrc IS INITIAL ).

  ENDMETHOD.


  METHOD is_empty.
    rv_yes = boolc( lines( mt_entries ) = 0 ).
  ENDMETHOD.


  METHOD merge.

    FIELD-SYMBOLS <ls_entry> LIKE LINE OF mt_entries.

    LOOP AT io_string_map->mt_entries ASSIGNING <ls_entry>.
      set(
        iv_key = <ls_entry>-k
        iv_val = <ls_entry>-v ).
    ENDLOOP.

  ENDMETHOD.


  METHOD set.

    DATA lv_key LIKE iv_key.
    DATA ls_entry LIKE LINE OF mt_entries.
    FIELD-SYMBOLS <ls_entry> LIKE LINE OF mt_entries.

    IF mv_read_only = abap_true.
      zcx_abapgit_exception=>raise( 'Cannot set. This string map is immutable' ).
    ENDIF.

    IF mv_case_insensitive = abap_true.
      lv_key = to_upper( iv_key ).
    ELSE.
      lv_key = iv_key.
    ENDIF.

    READ TABLE mt_entries ASSIGNING <ls_entry> WITH KEY k = lv_key.
    IF sy-subrc IS INITIAL.
      <ls_entry>-v = iv_val.
    ELSE.
      ls_entry-k = lv_key.
      ls_entry-v = iv_val.
      INSERT ls_entry INTO TABLE mt_entries.
    ENDIF.

    ro_map = me.

  ENDMETHOD.


  METHOD size.

    rv_size = lines( mt_entries ).

  ENDMETHOD.


  METHOD strict.
    mv_is_strict = iv_strict.
    ro_instance = me.
  ENDMETHOD.


  METHOD to_abap.

    DATA lo_type TYPE REF TO cl_abap_typedescr.
    DATA lv_field TYPE string.
    FIELD-SYMBOLS <ls_entry> LIKE LINE OF mt_entries.
    FIELD-SYMBOLS <lv_val> TYPE any.

    lo_type = cl_abap_typedescr=>describe_by_data( cs_container ).
    IF lo_type->type_kind <> cl_abap_typedescr=>typekind_struct1
      AND lo_type->type_kind <> cl_abap_typedescr=>typekind_struct2.
      zcx_abapgit_exception=>raise( 'Only structures supported' ).
    ENDIF.

    LOOP AT mt_entries ASSIGNING <ls_entry>.
      lv_field = to_upper( <ls_entry>-k ).
      ASSIGN COMPONENT lv_field OF STRUCTURE cs_container TO <lv_val>.
      IF sy-subrc = 0.
        " TODO check target type ?
        <lv_val> = <ls_entry>-v.
      ELSEIF mv_is_strict = abap_false.
        CONTINUE.
      ELSE.
        zcx_abapgit_exception=>raise( |Component { lv_field } not found in target| ).
      ENDIF.
    ENDLOOP.

  ENDMETHOD.
ENDCLASS.
