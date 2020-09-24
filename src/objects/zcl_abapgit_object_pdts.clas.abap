CLASS zcl_abapgit_object_pdts DEFINITION
  PUBLIC
  INHERITING FROM zcl_abapgit_objects_super
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
methods foo.

ENDCLASS.



CLASS zcl_abapgit_object_pdts IMPLEMENTATION.


  METHOD foo.

    DATA: li_element        TYPE REF TO if_ixml_node,
          li_child_iterator TYPE REF TO if_ixml_node_iterator.

    lcl_abapgit_object_pdts_helper=>bar( ).

    li_element->remove_node( ).
    li_child_iterator->reset( ).

  ENDMETHOD.

ENDCLASS.
