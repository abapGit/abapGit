CLASS lcl_filter DEFINITION FINAL.
  PUBLIC SECTION.
    INTERFACES zif_abapgit_object_filter.

    METHODS constructor
      IMPORTING
        is_item TYPE zif_abapgit_definitions=>ty_item.

  PRIVATE SECTION.
    DATA mt_filter TYPE zif_abapgit_definitions=>ty_tadir_tt.
ENDCLASS.

CLASS lcl_filter IMPLEMENTATION.
  METHOD constructor.
    DATA ls_filter TYPE zif_abapgit_definitions=>ty_tadir.
    ls_filter-object   = is_item-obj_type.
    ls_filter-obj_name = is_item-obj_name.
    INSERT ls_filter INTO TABLE mt_filter.
  ENDMETHOD.

  METHOD zif_abapgit_object_filter~get_filter.
    rt_filter = mt_filter.
  ENDMETHOD.
ENDCLASS.
