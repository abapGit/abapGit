CLASS lcl_object_bobf DEFINITION INHERITING FROM lcl_saplink_adapter.
  PUBLIC SECTION.
    METHODS constructor
      IMPORTING
        is_item TYPE ty_item.
ENDCLASS.

CLASS lcl_object_bobf IMPLEMENTATION.
  METHOD constructor.
    super->constructor(
        iv_saplink_classname = 'ZSAPLINK_BOPF'
        is_item              = is_item
     ).
  ENDMETHOD.
ENDCLASS.