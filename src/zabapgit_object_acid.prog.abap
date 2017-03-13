*&---------------------------------------------------------------------*
*&  Include           ZABAPGIT_OBJECT_ACID
*&---------------------------------------------------------------------*

*----------------------------------------------------------------------*
*       CLASS lcl_object_acid DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_object_acid DEFINITION INHERITING FROM lcl_objects_super FINAL.

  PUBLIC SECTION.
    INTERFACES lif_object.
    ALIASES mo_files FOR lif_object~mo_files.

  PRIVATE SECTION.
    METHODS: create_object
      RETURNING VALUE(ro_aab) TYPE REF TO cl_aab_id
      RAISING   lcx_exception.

ENDCLASS.                    "lcl_object_acid DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_object_acid IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_object_acid IMPLEMENTATION.

  METHOD lif_object~has_changed_since.
    rv_changed = abap_true.
  ENDMETHOD.  "lif_object~has_changed_since

  METHOD lif_object~get_metadata.
    rs_metadata = get_metadata( ).
  ENDMETHOD.                    "lif_object~get_metadata

  METHOD lif_object~changed_by.
* looks like "changed by user" is not stored in the database
    rv_user = c_user_unknown.
  ENDMETHOD.

  METHOD create_object.

    DATA: lv_name TYPE aab_id_name.


    lv_name = ms_item-obj_name.

    CREATE OBJECT ro_aab
      EXPORTING
        im_name          = lv_name
      EXCEPTIONS
        name_not_allowed = 1
        OTHERS           = 2.
    IF sy-subrc <> 0.
      lcx_exception=>raise( 'error creating CL_AAB_ID object' ).
    ENDIF.

  ENDMETHOD.                    "create_object

  METHOD lif_object~serialize.

    DATA: lo_aab         TYPE REF TO cl_aab_id,
          lv_description TYPE aab_id_descript.


    IF lif_object~exists( ) = abap_false.
      RETURN.
    ENDIF.

    lo_aab = create_object( ).

    lo_aab->get_descript(
      IMPORTING ex_descript = lv_description
      EXCEPTIONS no_description_found = 1 ).

    io_xml->add( iv_name = 'DESCRIPTION'
                 ig_data = lv_description ).

  ENDMETHOD.                    "lif_object~serialize

  METHOD lif_object~deserialize.

    DATA: lv_description TYPE aab_id_descript,
          lo_aab         TYPE REF TO cl_aab_id.


    io_xml->read( EXPORTING iv_name = 'DESCRIPTION'
                  CHANGING cg_data = lv_description ).

    lo_aab = create_object( ).
    lo_aab->enqueue( ).
    lo_aab->set_descript( lv_description ).
    tadir_insert( iv_package ).
    lo_aab->save( ).

  ENDMETHOD.                    "lif_object~deserialize

  METHOD lif_object~delete.

    DATA: lo_aab TYPE REF TO cl_aab_id.


    lo_aab = create_object( ).
    lo_aab->enqueue( ).
    lo_aab->delete(
      EXCEPTIONS
        prop_error       = 1
        propt_error      = 2
        act_error        = 3
        cts_error        = 4
        cts_devclass     = 5
        id_not_found     = 6
        no_authorization = 7
        id_still_used    = 8
        where_used_error = 9
        OTHERS           = 10 ).
    IF sy-subrc <> 0.
      lcx_exception=>raise( 'error deleting ACID object' ).
    ENDIF.
    lo_aab->dequeue( ).

  ENDMETHOD.                    "lif_object~delete

  METHOD lif_object~exists.

    DATA: lv_state TYPE flag,
          lo_aab   TYPE REF TO cl_aab_id.


    lo_aab = create_object( ).

    lo_aab->get_state(
      IMPORTING
        ex_state = lv_state ).
    rv_bool = boolc( lv_state = abap_true ).

  ENDMETHOD.                    "lif_object~exists

  METHOD lif_object~jump.

    CALL FUNCTION 'RS_TOOL_ACCESS'
      EXPORTING
        operation     = 'SHOW'
        object_name   = ms_item-obj_name
        object_type   = 'ACID'
        in_new_window = abap_true.

  ENDMETHOD.                    "lif_object~jump

  METHOD lif_object~compare_to_remote_version.
    CREATE OBJECT ro_comparison_result TYPE lcl_null_comparison_result.
  ENDMETHOD.

ENDCLASS.                    "lcl_object_acid IMPLEMENTATION
