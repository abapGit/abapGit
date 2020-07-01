CLASS zcl_abapgit_string_map DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES:
      BEGIN OF ty_entry,
        k TYPE string,
        v TYPE string,
      END OF ty_entry,
      tty_entries TYPE STANDARD TABLE OF ty_entry WITH KEY k,
      tts_entries TYPE SORTED TABLE OF ty_entry WITH UNIQUE KEY k.

    CLASS-METHODS create
      RETURNING
        VALUE(ro_instance) TYPE REF TO zcl_abapgit_string_map.

    METHODS get
      IMPORTING
        iv_key        TYPE string
      RETURNING
        VALUE(rv_val) TYPE string.

    METHODS has
      IMPORTING
        iv_key        TYPE string
      RETURNING
        VALUE(rv_has) TYPE abap_bool.

    METHODS set
      IMPORTING
        iv_key TYPE string
        iv_val TYPE string OPTIONAL.

    METHODS size
      RETURNING
        VALUE(rv_size) TYPE i.

    METHODS is_empty
      RETURNING
        VALUE(rv_yes) TYPE abap_bool.

    METHODS delete
      IMPORTING
        iv_key TYPE string.

    METHODS clear.

    METHODS to_abap
      CHANGING
        !cs_container TYPE any
      RAISING
        zcx_abapgit_exception.

  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA mt_entries TYPE tts_entries.

ENDCLASS.



CLASS ZCL_ABAPGIT_STRING_MAP IMPLEMENTATION.


  METHOD clear.
    CLEAR mt_entries.
  ENDMETHOD.


  METHOD create.
    CREATE OBJECT ro_instance.
  ENDMETHOD.


  METHOD delete.

    DELETE mt_entries WHERE k = iv_key.

  ENDMETHOD.


  METHOD get.

    FIELD-SYMBOLS <ls_entry> LIKE LINE OF mt_entries.
    READ TABLE mt_entries ASSIGNING <ls_entry> WITH KEY k = iv_key.
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


  METHOD set.

    DATA ls_entry LIKE LINE OF mt_entries.
    FIELD-SYMBOLS <ls_entry> LIKE LINE OF mt_entries.

    READ TABLE mt_entries ASSIGNING <ls_entry> WITH KEY k = iv_key.
    IF sy-subrc IS INITIAL.
      <ls_entry>-v = iv_val.
    ELSE.
      ls_entry-k = iv_key.
      ls_entry-v = iv_val.
      INSERT ls_entry INTO TABLE mt_entries.
    ENDIF.

  ENDMETHOD.


  METHOD size.

    rv_size = lines( mt_entries ).

  ENDMETHOD.


  METHOD to_abap.

    DATA lo_type TYPE REF TO cl_abap_typedescr.
    DATA lo_struc TYPE REF TO cl_abap_structdescr.
    DATA lv_field TYPE string.
    FIELD-SYMBOLS <ls_entry> LIKE LINE OF mt_entries.
    FIELD-SYMBOLS <lv_val> TYPE any.

    lo_type = cl_abap_typedescr=>describe_by_data( cs_container ).
    IF lo_type->type_kind <> cl_abap_typedescr=>typekind_struct1
      AND lo_type->type_kind <> cl_abap_typedescr=>typekind_struct2.
      zcx_abapgit_exception=>raise( 'Only structures supported' ).
    ENDIF.

    lo_struc ?= lo_type.
    LOOP AT mt_entries ASSIGNING <ls_entry>.
      lv_field = to_upper( <ls_entry>-k ).
      ASSIGN COMPONENT lv_field OF STRUCTURE cs_container TO <lv_val>.
      IF sy-subrc = 0.
        " TODO check target type ?
        <lv_val> = <ls_entry>-v.
      ELSE.
        zcx_abapgit_exception=>raise( |Component { lv_field } not found in target| ).
      ENDIF.
    ENDLOOP.

  ENDMETHOD.
ENDCLASS.
