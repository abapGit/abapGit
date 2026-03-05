CLASS zcl_abapgit_ajson_ref_init_lib DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.

    CLASS-METHODS create_path_refs_init
      IMPORTING
        !it_data_refs       TYPE zif_abapgit_ajson_ref_init=>tty_data_refs
      RETURNING
        VALUE(ri_refs_init) TYPE REF TO zif_abapgit_ajson_ref_init
      RAISING
        zcx_abapgit_ajson_error.

ENDCLASS.



CLASS zcl_abapgit_ajson_ref_init_lib IMPLEMENTATION.


  METHOD create_path_refs_init.
    CREATE OBJECT ri_refs_init TYPE lcl_path_refs_init
      EXPORTING
        it_data_refs = it_data_refs.
  ENDMETHOD.
ENDCLASS.
