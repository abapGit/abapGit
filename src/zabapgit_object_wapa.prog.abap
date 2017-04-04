*&---------------------------------------------------------------------*
*&  Include           ZABAPGIT_OBJECT_WAPA
*&---------------------------------------------------------------------*

*----------------------------------------------------------------------*
*       CLASS lcl_object_wapa DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_object_wapa DEFINITION INHERITING FROM lcl_objects_super FINAL.

  PUBLIC SECTION.
    INTERFACES lif_object.
    ALIASES mo_files FOR lif_object~mo_files.

ENDCLASS.                    "lcl_object_TRAN DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_object_wapa IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_object_wapa IMPLEMENTATION.

  METHOD lif_object~has_changed_since.
    rv_changed = abap_true.
  ENDMETHOD.  "lif_object~has_changed_since

  METHOD lif_object~changed_by.
* todo
    rv_user = c_user_unknown.
  ENDMETHOD.

  METHOD lif_object~get_metadata.
    rs_metadata = get_metadata( ).
  ENDMETHOD.                    "lif_object~get_metadata

  METHOD lif_object~exists.

* todo
*    rv_bool = boolc( sy-subrc = 0 ).

  ENDMETHOD.                    "lif_object~exists

  METHOD lif_object~jump.

* todo

  ENDMETHOD.                    "jump

  METHOD lif_object~delete.

* todo

  ENDMETHOD.                    "delete

  METHOD lif_object~deserialize.

* todo

  ENDMETHOD.                    "deserialize

  METHOD lif_object~serialize.

* todo

  ENDMETHOD.                    "serialize

  METHOD lif_object~compare_to_remote_version.
    CREATE OBJECT ro_comparison_result TYPE lcl_null_comparison_result.
  ENDMETHOD.

ENDCLASS.                    "lcl_object_tran IMPLEMENTATION
