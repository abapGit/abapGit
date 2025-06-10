CLASS lcl_filter DEFINITION FINAL.
  PUBLIC SECTION.
    INTERFACES zif_abapgit_object_filter.

    METHODS constructor
      IMPORTING
        it_filter TYPE zif_abapgit_definitions=>ty_tadir_tt.

  PRIVATE SECTION.
    DATA mt_filter TYPE zif_abapgit_definitions=>ty_tadir_tt.
ENDCLASS.

CLASS lcl_filter IMPLEMENTATION.
  METHOD constructor.
    mt_filter = it_filter.
  ENDMETHOD.

  METHOD zif_abapgit_object_filter~get_filter.
    rt_filter = mt_filter.
  ENDMETHOD.
ENDCLASS.
