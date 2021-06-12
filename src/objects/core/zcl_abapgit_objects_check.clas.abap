CLASS zcl_abapgit_objects_check DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    CLASS-METHODS deserialize_checks
      IMPORTING
        !io_repo         TYPE REF TO zcl_abapgit_repo
      RETURNING
        VALUE(rs_checks) TYPE zif_abapgit_definitions=>ty_deserialize_checks
      RAISING
        zcx_abapgit_exception .
    CLASS-METHODS checks_adjust
      IMPORTING
        !io_repo    TYPE REF TO zcl_abapgit_repo
        !is_checks  TYPE zif_abapgit_definitions=>ty_deserialize_checks
      CHANGING
        !ct_results TYPE zif_abapgit_definitions=>ty_results_tt
      RAISING
        zcx_abapgit_exception .
  PROTECTED SECTION.

  PRIVATE SECTION.

    CLASS-METHODS warning_overwrite_adjust
      IMPORTING
        !it_overwrite TYPE zif_abapgit_definitions=>ty_overwrite_tt
      CHANGING
        !ct_results   TYPE zif_abapgit_definitions=>ty_results_tt
      RAISING
        zcx_abapgit_exception.
    CLASS-METHODS warning_overwrite_find
      IMPORTING
        !it_results         TYPE zif_abapgit_definitions=>ty_results_tt
      RETURNING
        VALUE(rt_overwrite) TYPE zif_abapgit_definitions=>ty_overwrite_tt.
    CLASS-METHODS warning_package_adjust
      IMPORTING
        !io_repo      TYPE REF TO zcl_abapgit_repo
        !it_overwrite TYPE zif_abapgit_definitions=>ty_overwrite_tt
      CHANGING
        !ct_results   TYPE zif_abapgit_definitions=>ty_results_tt
      RAISING
        zcx_abapgit_exception.
    CLASS-METHODS warning_package_find
      IMPORTING
        !it_results         TYPE zif_abapgit_definitions=>ty_results_tt
        !io_repo            TYPE REF TO zcl_abapgit_repo
      RETURNING
        VALUE(rt_overwrite) TYPE zif_abapgit_definitions=>ty_overwrite_tt
      RAISING
        zcx_abapgit_exception.
ENDCLASS.



CLASS zcl_abapgit_objects_check IMPLEMENTATION.


  METHOD checks_adjust.

    warning_overwrite_adjust(
      EXPORTING
        it_overwrite = is_checks-overwrite
      CHANGING
        ct_results   = ct_results ).

    warning_package_adjust(
      EXPORTING
        io_repo      = io_repo
        it_overwrite = is_checks-warning_package
      CHANGING
        ct_results   = ct_results ).

  ENDMETHOD.


  METHOD deserialize_checks.

    DATA: lt_results TYPE zif_abapgit_definitions=>ty_results_tt,
          li_package TYPE REF TO zif_abapgit_sap_package.

    " get unfiltered status to evaluate properly which warnings are required
    lt_results = zcl_abapgit_file_status=>status( io_repo ).

    rs_checks-overwrite = warning_overwrite_find( lt_results ).

    rs_checks-warning_package = warning_package_find(
      io_repo    = io_repo
      it_results = lt_results ).

    IF lines( lt_results ) > 0.
      li_package = zcl_abapgit_factory=>get_sap_package( io_repo->get_package( ) ).
      rs_checks-transport-required = li_package->are_changes_recorded_in_tr_req( ).
      IF NOT rs_checks-transport-required IS INITIAL.
        rs_checks-transport-type = li_package->get_transport_type( ).
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD warning_overwrite_adjust.

    DATA: lt_overwrite LIKE it_overwrite,
          ls_overwrite LIKE LINE OF lt_overwrite.

    FIELD-SYMBOLS: <ls_overwrite> LIKE LINE OF lt_overwrite.


* make sure to get the current status, as something might have changed in the meanwhile
    lt_overwrite = warning_overwrite_find( ct_results ).

    LOOP AT lt_overwrite ASSIGNING <ls_overwrite>.

      READ TABLE it_overwrite INTO ls_overwrite
                              WITH TABLE KEY object_type_and_name
                              COMPONENTS obj_type = <ls_overwrite>-obj_type
                                         obj_name = <ls_overwrite>-obj_name.
      IF sy-subrc <> 0 OR ls_overwrite-decision IS INITIAL.
        zcx_abapgit_exception=>raise( |Overwrite { <ls_overwrite>-obj_type } {
          <ls_overwrite>-obj_name } undecided| ).
      ENDIF.

      IF ls_overwrite-decision = zif_abapgit_definitions=>gc_no.
        DELETE ct_results WHERE
          obj_type = <ls_overwrite>-obj_type AND
          obj_name = <ls_overwrite>-obj_name.
        ASSERT sy-subrc = 0.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.


  METHOD warning_overwrite_find.

    DATA:
      lv_status  TYPE c LENGTH 2,
      lt_changes TYPE STANDARD TABLE OF zif_abapgit_definitions=>ty_overwrite WITH DEFAULT KEY.

    FIELD-SYMBOLS:
      <ls_result>  LIKE LINE OF it_results,
      <ls_changes> LIKE LINE OF lt_changes.

    " collect all actions for object that have been changed
    LOOP AT it_results ASSIGNING <ls_result> WHERE NOT obj_type IS INITIAL.

      APPEND INITIAL LINE TO lt_changes ASSIGNING <ls_changes>.
      MOVE-CORRESPONDING <ls_result> TO <ls_changes>.

      CONCATENATE <ls_result>-lstate <ls_result>-rstate INTO lv_status RESPECTING BLANKS.

      CASE lv_status.
        WHEN '  '. " no changes
          <ls_changes>-action = zif_abapgit_objects=>c_deserialize_action-none.
        WHEN ' A' OR 'D ' OR 'DM'. " added remotely or deleted locally
          <ls_changes>-action = zif_abapgit_objects=>c_deserialize_action-add.
          <ls_changes>-icon   = icon_create.
          <ls_changes>-text   = 'Add local object'.
        WHEN 'A ' OR ' D' OR 'MD'. " added locally or deleted remotely
          <ls_changes>-action = zif_abapgit_objects=>c_deserialize_action-delete.
          <ls_changes>-icon   = icon_delete.
          <ls_changes>-text   = 'Delete local object'.
        WHEN 'M ' OR 'MM'. " modified locally
          <ls_changes>-action = zif_abapgit_objects=>c_deserialize_action-overwrite.
          <ls_changes>-icon   = icon_change.
          <ls_changes>-text   = 'Overwrite local object'.
        WHEN ' M'. " modified only remotely
          <ls_changes>-action = zif_abapgit_objects=>c_deserialize_action-update.
          <ls_changes>-icon   = icon_change.
          <ls_changes>-text   = 'Update local object'.
        WHEN OTHERS.
          ASSERT 0 = 1.
      ENDCASE.

    ENDLOOP.

    " Remove duplicate actions
    SORT lt_changes.
    DELETE ADJACENT DUPLICATES FROM lt_changes.

    " Check if deletions are for complete object or just a part
    LOOP AT lt_changes ASSIGNING <ls_changes> WHERE action = zif_abapgit_objects=>c_deserialize_action-delete.

      LOOP AT lt_changes TRANSPORTING NO FIELDS
        WHERE obj_type = <ls_changes>-obj_type AND obj_name = <ls_changes>-obj_name
          AND action <> zif_abapgit_objects=>c_deserialize_action-delete.
        EXIT.
      ENDLOOP.
      IF sy-subrc = 0.
        " There's some other action, so object will be recreated after deletion
        <ls_changes>-action = zif_abapgit_objects=>c_deserialize_action-delete_add.
        <ls_changes>-icon   = icon_adopt.
        <ls_changes>-text   = 'Delete and recreate local object'.
      ENDIF.

    ENDLOOP.

    DELETE lt_changes WHERE action = zif_abapgit_objects=>c_deserialize_action-none.

    " If there are multiple changes in an object, keep highest priority action
    SORT lt_changes BY obj_type obj_name action DESCENDING.
    DELETE ADJACENT DUPLICATES FROM lt_changes COMPARING obj_type obj_name.

    rt_overwrite = lt_changes.

  ENDMETHOD.


  METHOD warning_package_adjust.

    DATA: lt_overwrite LIKE it_overwrite,
          ls_overwrite LIKE LINE OF lt_overwrite.

    FIELD-SYMBOLS: <ls_overwrite> LIKE LINE OF lt_overwrite.


* make sure to get the current status, as something might have changed in the meanwhile
    lt_overwrite = warning_package_find(
      it_results   = ct_results
      io_repo      = io_repo ).

    LOOP AT lt_overwrite ASSIGNING <ls_overwrite>.

      READ TABLE it_overwrite INTO ls_overwrite
                              WITH TABLE KEY object_type_and_name
                              COMPONENTS obj_type = <ls_overwrite>-obj_type
                                         obj_name = <ls_overwrite>-obj_name.
      IF sy-subrc <> 0 OR ls_overwrite-decision IS INITIAL.
        zcx_abapgit_exception=>raise( |Overwrite of package { <ls_overwrite>-obj_type } {
          <ls_overwrite>-obj_name } undecided| ).
      ENDIF.

      IF ls_overwrite-decision = zif_abapgit_definitions=>gc_no.
        DELETE ct_results WHERE
          obj_type = <ls_overwrite>-obj_type AND
          obj_name = <ls_overwrite>-obj_name.
        ASSERT sy-subrc = 0.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.


  METHOD warning_package_find.

    DATA: lv_package          TYPE devclass,
          lt_overwrite_unique TYPE HASHED TABLE OF zif_abapgit_definitions=>ty_overwrite
                                  WITH UNIQUE KEY obj_type obj_name devclass,
          ls_overwrite        LIKE LINE OF rt_overwrite,
          ls_tadir            TYPE zif_abapgit_definitions=>ty_tadir.

    DATA: lo_folder_logic TYPE REF TO zcl_abapgit_folder_logic.

    FIELD-SYMBOLS: <ls_result> LIKE LINE OF it_results.

    lo_folder_logic = zcl_abapgit_folder_logic=>get_instance( ).
    LOOP AT it_results ASSIGNING <ls_result> WHERE match IS INITIAL.

      lv_package = lo_folder_logic->path_to_package(
        iv_top  = io_repo->get_package( )
        io_dot  = io_repo->get_dot_abapgit( )
        iv_path = <ls_result>-path
        iv_create_if_not_exists = abap_false ).

      ls_tadir = zcl_abapgit_factory=>get_tadir( )->read_single(
        iv_object   = <ls_result>-obj_type
        iv_obj_name = <ls_result>-obj_name ).

      IF NOT ls_tadir IS INITIAL AND ls_tadir-devclass <> lv_package.
* overwriting object from different package than expected
        CLEAR ls_overwrite.
        ls_overwrite-obj_type = <ls_result>-obj_type.
        ls_overwrite-obj_name = <ls_result>-obj_name.
        ls_overwrite-devclass = ls_tadir-devclass.
        INSERT ls_overwrite INTO TABLE lt_overwrite_unique.
      ENDIF.

    ENDLOOP.

    rt_overwrite = lt_overwrite_unique.

  ENDMETHOD.
ENDCLASS.
