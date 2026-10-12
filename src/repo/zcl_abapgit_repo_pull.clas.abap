CLASS zcl_abapgit_repo_pull DEFINITION
  PUBLIC
  CREATE PRIVATE
  GLOBAL FRIENDS zcl_abapgit_factory .

  PUBLIC SECTION.

    INTERFACES zif_abapgit_repo_pull .

    METHODS constructor
      IMPORTING
        !ii_repo TYPE REF TO zif_abapgit_repo .
  PROTECTED SECTION.

    " Allows testing the pull sequence without deleting SAP objects
    METHODS delete_tadir
      IMPORTING
        !it_tadir  TYPE zif_abapgit_definitions=>ty_tadir_tt
        !is_checks TYPE zif_abapgit_definitions=>ty_delete_checks
        !ii_log    TYPE REF TO zif_abapgit_log
      RAISING
        zcx_abapgit_exception .
  PRIVATE SECTION.

    DATA mi_repo TYPE REF TO zif_abapgit_repo .

    METHODS delete_objects
      IMPORTING
        !is_checks TYPE zif_abapgit_definitions=>ty_deserialize_checks
        !ii_log    TYPE REF TO zif_abapgit_log
      RAISING
        zcx_abapgit_exception .
    CLASS-METHODS get_objects_to_delete
      IMPORTING
        !is_checks      TYPE zif_abapgit_definitions=>ty_deserialize_checks
      RETURNING
        VALUE(rt_tadir) TYPE zif_abapgit_definitions=>ty_tadir_tt .
ENDCLASS.



CLASS zcl_abapgit_repo_pull IMPLEMENTATION.


  METHOD constructor.

    mi_repo = ii_repo.

  ENDMETHOD.


  METHOD delete_objects.

    DATA:
      ls_checks TYPE zif_abapgit_definitions=>ty_delete_checks,
      lt_tadir  TYPE zif_abapgit_definitions=>ty_tadir_tt.

    lt_tadir = get_objects_to_delete( is_checks ).

    " todo, check if object type supports deletion of parts to avoid deleting complete object

    IF lines( lt_tadir ) > 0.
      ls_checks-transport = is_checks-transport.

      delete_tadir( it_tadir  = lt_tadir
                    is_checks = ls_checks
                    ii_log    = ii_log ).

      mi_repo->refresh( iv_drop_log = abap_false ).
    ENDIF.

  ENDMETHOD.


  METHOD delete_tadir.

    zcl_abapgit_objects=>delete( it_tadir  = it_tadir
                                 is_checks = is_checks
                                 ii_log    = ii_log ).

  ENDMETHOD.


  METHOD get_objects_to_delete.

    DATA ls_tadir TYPE zif_abapgit_definitions=>ty_tadir.

    FIELD-SYMBOLS <ls_overwrite> LIKE LINE OF is_checks-overwrite.
    FIELD-SYMBOLS <ls_tabl_data> LIKE LINE OF is_checks-delete_tabl_with_data.

    " get confirmed deletions
    LOOP AT is_checks-overwrite ASSIGNING <ls_overwrite>
      WHERE ( action = zif_abapgit_objects=>c_deserialize_action-delete
      OR action = zif_abapgit_objects=>c_deserialize_action-delete_add )
      AND decision = zif_abapgit_definitions=>c_yes.

      " a table that contains data needs its own confirmation
      READ TABLE is_checks-delete_tabl_with_data ASSIGNING <ls_tabl_data>
        WITH KEY object_type_and_name COMPONENTS
          obj_type = <ls_overwrite>-obj_type
          obj_name = <ls_overwrite>-obj_name.
      IF sy-subrc = 0 AND <ls_tabl_data>-decision = zif_abapgit_definitions=>c_no.
        CONTINUE.
      ENDIF.

      ls_tadir-pgmid    = 'R3TR'.
      ls_tadir-object   = <ls_overwrite>-obj_type.
      ls_tadir-obj_name = <ls_overwrite>-obj_name.
      ls_tadir-devclass = <ls_overwrite>-devclass.
      INSERT ls_tadir INTO TABLE rt_tadir.

    ENDLOOP.

  ENDMETHOD.


  METHOD zif_abapgit_repo_pull~checks.

    rs_checks = mi_repo->deserialize_checks( ).

  ENDMETHOD.


  METHOD zif_abapgit_repo_pull~pull.

    IF ii_log IS BOUND.
      ri_log = ii_log.
    ELSE.
      ri_log = mi_repo->create_new_log( 'Pull Log' ).
    ENDIF.

    " pass decisions to delete
    delete_objects(
      is_checks = is_checks
      ii_log    = ri_log ).

    " pass decisions to deserialize
    mi_repo->deserialize(
      is_checks = is_checks
      ii_log    = ri_log ).

  ENDMETHOD.
ENDCLASS.
