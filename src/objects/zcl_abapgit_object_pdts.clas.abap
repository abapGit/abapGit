CLASS zcl_abapgit_object_pdts DEFINITION
  PUBLIC
  INHERITING FROM zcl_abapgit_objects_super
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zif_abapgit_object.
    ALIASES mo_files FOR zif_abapgit_object~mo_files.

  PRIVATE SECTION.

ENDCLASS.



CLASS zcl_abapgit_object_pdts IMPLEMENTATION.


  METHOD zif_abapgit_object~serialize.

    DATA: li_element        TYPE REF TO if_ixml_node,
          li_child_iterator TYPE REF TO if_ixml_node_iterator.

          li_element->remove_node( ).
          li_child_iterator->reset( ).

  ENDMETHOD.


  METHOD zif_abapgit_object~deserialize.

    lcl_abapgit_object_pdts_helper=>foo( ).

  ENDMETHOD.


  METHOD zif_abapgit_object~delete.
  ENDMETHOD.


  METHOD zif_abapgit_object~exists.
  ENDMETHOD.


  METHOD zif_abapgit_object~is_locked.
   ENDMETHOD.


  METHOD zif_abapgit_object~is_active.
  ENDMETHOD.


  METHOD zif_abapgit_object~changed_by.
  ENDMETHOD.


  METHOD zif_abapgit_object~jump.
  ENDMETHOD.


  METHOD zif_abapgit_object~get_metadata.
  ENDMETHOD.


  METHOD zif_abapgit_object~get_comparator.
  ENDMETHOD.


  METHOD zif_abapgit_object~get_deserialize_steps.
  ENDMETHOD.

ENDCLASS.
