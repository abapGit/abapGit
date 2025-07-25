CLASS zcl_abapgit_objects_check DEFINITION
  PUBLIC
  CREATE PUBLIC.

  PUBLIC SECTION.

    CLASS-METHODS class_constructor.

    CLASS-METHODS deserialize_checks
      IMPORTING
        !ii_repo         TYPE REF TO zif_abapgit_repo
      RETURNING
        VALUE(rs_checks) TYPE zif_abapgit_definitions=>ty_deserialize_checks
      RAISING
        zcx_abapgit_exception.

    CLASS-METHODS checks_adjust
      IMPORTING
        !ii_repo    TYPE REF TO zif_abapgit_repo
        !is_checks  TYPE zif_abapgit_definitions=>ty_deserialize_checks
      CHANGING
        !ct_results TYPE zif_abapgit_definitions=>ty_results_tt
      RAISING
        zcx_abapgit_exception.

  PROTECTED SECTION.
  PRIVATE SECTION.

    CLASS-DATA gi_exit TYPE REF TO zif_abapgit_exit.

    CLASS-METHODS adjust_result
      IMPORTING
        !iv_txt           TYPE string
        !it_overwrite_old TYPE zif_abapgit_definitions=>ty_overwrite_tt
        !it_overwrite_new TYPE zif_abapgit_definitions=>ty_overwrite_tt
      CHANGING
        !ct_results       TYPE zif_abapgit_definitions=>ty_results_tt
      RAISING
        zcx_abapgit_exception.

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
        !ii_repo      TYPE REF TO zif_abapgit_repo
        !it_overwrite TYPE zif_abapgit_definitions=>ty_overwrite_tt
      CHANGING
        !ct_results   TYPE zif_abapgit_definitions=>ty_results_tt
      RAISING
        zcx_abapgit_exception.

    CLASS-METHODS warning_package_find
      IMPORTING
        !it_results         TYPE zif_abapgit_definitions=>ty_results_tt
        !ii_repo            TYPE REF TO zif_abapgit_repo
      RETURNING
        VALUE(rt_overwrite) TYPE zif_abapgit_definitions=>ty_overwrite_tt
      RAISING
        zcx_abapgit_exception.

    CLASS-METHODS warning_data_loss_adjust
      IMPORTING
        !ii_repo      TYPE REF TO zif_abapgit_repo
        !it_overwrite TYPE zif_abapgit_definitions=>ty_overwrite_tt
      CHANGING
        !ct_results   TYPE zif_abapgit_definitions=>ty_results_tt
      RAISING
        zcx_abapgit_exception.

    CLASS-METHODS warning_data_loss_find
      IMPORTING
        !ii_repo            TYPE REF TO zif_abapgit_repo
        !it_results         TYPE zif_abapgit_definitions=>ty_results_tt
      RETURNING
        VALUE(rt_overwrite) TYPE zif_abapgit_definitions=>ty_overwrite_tt
      RAISING
        zcx_abapgit_exception.

    CLASS-METHODS determine_transport_request
      IMPORTING
        ii_repo                     TYPE REF TO zif_abapgit_repo
        iv_transport_type           TYPE zif_abapgit_definitions=>ty_transport_type
      RETURNING
        VALUE(rv_transport_request) TYPE trkorr.

    CLASS-METHODS check_multiple_files
      IMPORTING
        !it_results TYPE zif_abapgit_definitions=>ty_results_tt
      RAISING
        zcx_abapgit_exception.

ENDCLASS.



CLASS zcl_abapgit_objects_check IMPLEMENTATION.


  METHOD adjust_result.

    DATA ls_overwrite TYPE zif_abapgit_definitions=>ty_overwrite.

    FIELD-SYMBOLS <ls_overwrite> TYPE zif_abapgit_definitions=>ty_overwrite.

    LOOP AT it_overwrite_new ASSIGNING <ls_overwrite>.

      READ TABLE it_overwrite_old INTO ls_overwrite WITH TABLE KEY object_type_and_name
        COMPONENTS obj_type = <ls_overwrite>-obj_type obj_name = <ls_overwrite>-obj_name.
      IF sy-subrc <> 0 OR ls_overwrite-decision IS INITIAL.
        zcx_abapgit_exception=>raise( |{ iv_txt } { <ls_overwrite>-obj_type } { <ls_overwrite>-obj_name } undecided| ).
      ENDIF.

      IF ls_overwrite-decision = zif_abapgit_definitions=>c_no.
        DELETE ct_results WHERE obj_type = <ls_overwrite>-obj_type AND obj_name = <ls_overwrite>-obj_name.
        ASSERT sy-subrc = 0.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.


  METHOD checks_adjust.

    " Make sure to get the current status, as something might have changed in the meanwhile
    " - Remove objects from results that were deselected in confirmation popup
    " - Raise exception if an object has no decision of what to do

    warning_overwrite_adjust(
      EXPORTING
        it_overwrite = is_checks-overwrite
      CHANGING
        ct_results   = ct_results ).

    warning_package_adjust(
      EXPORTING
        ii_repo      = ii_repo
        it_overwrite = is_checks-warning_package
      CHANGING
        ct_results   = ct_results ).

    warning_data_loss_adjust(
      EXPORTING
        ii_repo      = ii_repo
        it_overwrite = is_checks-data_loss
      CHANGING
        ct_results   = ct_results ).

  ENDMETHOD.


  METHOD check_multiple_files.

    DATA:
      lv_msg      TYPE string,
      lv_lstate   TYPE c LENGTH 2,
      lv_rstate   TYPE c LENGTH 2,
      lt_res_sort LIKE it_results,
      ls_result   LIKE LINE OF it_results.

    FIELD-SYMBOLS <ls_result> LIKE LINE OF it_results.

    lt_res_sort = it_results.
    SORT lt_res_sort BY filename ASCENDING.

    " Prevent pulling if there is more than one file with the same name
    LOOP AT lt_res_sort ASSIGNING <ls_result>
      WHERE obj_type <> 'DEVC' AND packmove = abap_false AND filename IS NOT INITIAL.
      " Changing package and object at the same time is ok (state: Add + Delete)
      CONCATENATE <ls_result>-lstate ls_result-lstate INTO lv_lstate RESPECTING BLANKS.
      CONCATENATE <ls_result>-rstate ls_result-rstate INTO lv_rstate RESPECTING BLANKS.
      IF <ls_result>-filename = ls_result-filename AND
        lv_lstate <> 'AD' AND lv_lstate <> 'DA' AND lv_rstate <> 'AD' AND lv_rstate <> 'DA'.
        lv_msg = |Pull not possible since there are multiple files with same filename, { <ls_result>-filename }.|
          && | Keep one of the files and delete the other in the repository.|.
        zcx_abapgit_exception=>raise( lv_msg ).
      ENDIF.
      MOVE-CORRESPONDING <ls_result> TO ls_result.
    ENDLOOP.

  ENDMETHOD.


  METHOD class_constructor.

    gi_exit = zcl_abapgit_exit=>get_instance( ).

  ENDMETHOD.


  METHOD deserialize_checks.

    DATA: lt_results TYPE zif_abapgit_definitions=>ty_results_tt,
          li_package TYPE REF TO zif_abapgit_sap_package.

    " get unfiltered status to evaluate properly which warnings are required
    lt_results = zcl_abapgit_repo_status=>calculate( ii_repo ).

    check_multiple_files( lt_results ).

    rs_checks-overwrite = warning_overwrite_find( lt_results ).

    rs_checks-warning_package = warning_package_find(
      ii_repo    = ii_repo
      it_results = lt_results ).

    rs_checks-data_loss = warning_data_loss_find(
      ii_repo    = ii_repo
      it_results = lt_results ).

    IF lines( lt_results ) > 0.
      li_package = zcl_abapgit_factory=>get_sap_package( ii_repo->get_package( ) ).
      rs_checks-transport-required = li_package->are_changes_recorded_in_tr_req( ).
      IF NOT rs_checks-transport-required IS INITIAL.
        rs_checks-transport-type = li_package->get_transport_type( ).
        rs_checks-transport-transport = determine_transport_request(
                                            ii_repo           = ii_repo
                                            iv_transport_type = rs_checks-transport-type ).
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD determine_transport_request.

    " Use transport from repo settings if maintained, or determine via user exit.
    " If transport keeps empty here, it'll requested later via popup.
    rv_transport_request = ii_repo->get_local_settings( )-transport_request.

    gi_exit->determine_transport_request(
      EXPORTING
        ii_repo              = ii_repo
        iv_transport_type    = iv_transport_type
      CHANGING
        cv_transport_request = rv_transport_request ).

  ENDMETHOD.


  METHOD warning_data_loss_adjust.

    DATA lt_overwrite LIKE it_overwrite.

    lt_overwrite = warning_data_loss_find(
      it_results = ct_results
      ii_repo    = ii_repo ).

    adjust_result(
      EXPORTING
        iv_txt           = 'Potential data loss for'
        it_overwrite_old = it_overwrite
        it_overwrite_new = lt_overwrite
      CHANGING
        ct_results       = ct_results ).

  ENDMETHOD.


  METHOD warning_data_loss_find.

    DATA:
      ls_item       TYPE zif_abapgit_definitions=>ty_item,
      li_comparator TYPE REF TO zif_abapgit_comparator,
      ls_overwrite  LIKE LINE OF rt_overwrite,
      lv_result     TYPE string.

    FIELD-SYMBOLS <ls_result> LIKE LINE OF it_results.

    " For optimal performance, we limit here by object type since we know that only TABL has a comparator
    " If there are other object types in the future, extend the where clause or remove the check on object type.
    LOOP AT it_results ASSIGNING <ls_result>
      WHERE match IS INITIAL
        AND packmove IS INITIAL
        AND filename CP '*.xml'
        AND obj_type = 'TABL' ##PRIMKEY[SEC_KEY].

      CLEAR ls_item.
      MOVE-CORRESPONDING <ls_result> TO ls_item.

      li_comparator = zcl_abapgit_objects_compare=>get_comparator( ls_item ).
      IF NOT li_comparator IS BOUND.
        RETURN.
      ENDIF.

      lv_result = zcl_abapgit_objects_compare=>get_result(
        ii_comparator = li_comparator
        iv_filename   = <ls_result>-filename
        it_local      = ii_repo->get_files_local( )
        it_remote     = ii_repo->get_files_remote( iv_ignore_files = abap_true ) ).

      IF lv_result IS NOT INITIAL.
        READ TABLE rt_overwrite TRANSPORTING NO FIELDS WITH KEY object_type_and_name COMPONENTS
          obj_type = <ls_result>-obj_type
          obj_name = <ls_result>-obj_name.
        IF sy-subrc <> 0.
          CLEAR ls_overwrite.
          MOVE-CORRESPONDING <ls_result> TO ls_overwrite.
          ls_overwrite-devclass = <ls_result>-package.
          ls_overwrite-action   = zif_abapgit_objects=>c_deserialize_action-data_loss.
          ls_overwrite-icon     = icon_warning.
          ls_overwrite-text     = lv_result.
          INSERT ls_overwrite INTO TABLE rt_overwrite.
        ENDIF.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.


  METHOD warning_overwrite_adjust.

    DATA lt_overwrite LIKE it_overwrite.

    lt_overwrite = warning_overwrite_find( ct_results ).

    adjust_result(
      EXPORTING
        iv_txt           = 'Overwrite of object'
        it_overwrite_old = it_overwrite
        it_overwrite_new = lt_overwrite
      CHANGING
        ct_results       = ct_results ).

  ENDMETHOD.


  METHOD warning_overwrite_find.

    DATA:
      ls_item    TYPE zif_abapgit_definitions=>ty_item,
      lv_status  TYPE c LENGTH 2,
      lt_changes TYPE STANDARD TABLE OF zif_abapgit_definitions=>ty_overwrite WITH DEFAULT KEY.

    FIELD-SYMBOLS:
      <ls_result>  LIKE LINE OF it_results,
      <ls_changes> LIKE LINE OF lt_changes.

    " collect all actions for object that have been changed
    LOOP AT it_results ASSIGNING <ls_result> WHERE NOT obj_type IS INITIAL.

      APPEND INITIAL LINE TO lt_changes ASSIGNING <ls_changes>.
      MOVE-CORRESPONDING <ls_result> TO <ls_changes>.
      <ls_changes>-devclass = <ls_result>-package.
      MOVE-CORRESPONDING <ls_changes> TO ls_item.

      IF <ls_result>-packmove = abap_true.
        <ls_changes>-action = zif_abapgit_objects=>c_deserialize_action-packmove.
        <ls_changes>-icon   = icon_package_standard.
        <ls_changes>-text   = 'Change package assignment'.
      ELSEIF zcl_abapgit_objects=>is_supported( ls_item ) = abap_false
        AND ls_item-obj_type <> zif_abapgit_data_config=>c_data_type-tabu.
        <ls_changes>-action = zif_abapgit_objects=>c_deserialize_action-no_support.
        <ls_changes>-icon   = icon_no_status.
        <ls_changes>-text   = 'Object type not supported'.
      ELSE.
        CONCATENATE <ls_result>-lstate <ls_result>-rstate INTO lv_status RESPECTING BLANKS.
        <ls_changes>-state = lv_status.
        REPLACE ALL OCCURRENCES OF ` ` IN <ls_changes>-state WITH '_'.

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
      ENDIF.

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

    DATA lt_overwrite LIKE it_overwrite.

    lt_overwrite = warning_package_find(
      it_results = ct_results
      ii_repo    = ii_repo ).

    adjust_result(
      EXPORTING
        iv_txt           = 'Overwrite of package'
        it_overwrite_old = it_overwrite
        it_overwrite_new = lt_overwrite
      CHANGING
        ct_results       = ct_results ).

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
    LOOP AT it_results ASSIGNING <ls_result> WHERE match IS INITIAL AND packmove IS INITIAL.

      lv_package = lo_folder_logic->path_to_package(
        iv_top  = ii_repo->get_package( )
        io_dot  = ii_repo->get_dot_abapgit( )
        iv_path = <ls_result>-path
        iv_create_if_not_exists = abap_false ).

      ls_tadir = zcl_abapgit_factory=>get_tadir( )->read_single(
        iv_object   = <ls_result>-obj_type
        iv_obj_name = <ls_result>-obj_name ).

      IF NOT ls_tadir IS INITIAL AND ls_tadir-devclass <> lv_package.
        " overwriting object from different package than expected
        CLEAR ls_overwrite.
        CONCATENATE <ls_result>-lstate <ls_result>-rstate INTO ls_overwrite-state RESPECTING BLANKS.
        REPLACE ALL OCCURRENCES OF ` ` IN ls_overwrite-state WITH '_'.
        ls_overwrite-obj_type = <ls_result>-obj_type.
        ls_overwrite-obj_name = <ls_result>-obj_name.
        ls_overwrite-devclass = ls_tadir-devclass.
        ls_overwrite-action   = zif_abapgit_objects=>c_deserialize_action-overwrite.
        ls_overwrite-icon     = icon_change.
        ls_overwrite-text     = 'Overwrite local object'.
        INSERT ls_overwrite INTO TABLE lt_overwrite_unique.
      ENDIF.

    ENDLOOP.

    rt_overwrite = lt_overwrite_unique.

  ENDMETHOD.
ENDCLASS.
