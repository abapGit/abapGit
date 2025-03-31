CLASS zcl_abapgit_gui_page_chg_pckg DEFINITION
  PUBLIC
  INHERITING FROM zcl_abapgit_gui_component
  FINAL
  CREATE PRIVATE.

  PUBLIC SECTION.

    INTERFACES zif_abapgit_gui_event_handler.
    INTERFACES zif_abapgit_gui_renderable.

    CLASS-METHODS create
      IMPORTING
        !io_repo       TYPE REF TO zcl_abapgit_repo
      RETURNING
        VALUE(ri_page) TYPE REF TO zif_abapgit_gui_renderable
      RAISING
        zcx_abapgit_exception.

    METHODS constructor
      IMPORTING
        !io_repo TYPE REF TO zcl_abapgit_repo
      RAISING
        zcx_abapgit_exception.

  PROTECTED SECTION.
  PRIVATE SECTION.

    CONSTANTS:
      BEGIN OF c_id,
        old_package TYPE string VALUE 'old-package',
        new_package TYPE string VALUE 'new-package',
        remove_old  TYPE string VALUE 'remove-old',
      END OF c_id.

    CONSTANTS:
      BEGIN OF c_event,
        change_package     TYPE string VALUE 'change-package',
        choose_package_old TYPE string VALUE 'choose-package-old',
        choose_package_new TYPE string VALUE 'choose-package-new',
      END OF c_event.

    TYPES:
      BEGIN OF ty_map,
        old_package TYPE devclass,
        new_package TYPE devclass,
      END OF ty_map,
      ty_mapping TYPE STANDARD TABLE OF ty_map WITH KEY old_package.

    DATA mo_repo TYPE REF TO zcl_abapgit_repo.
    DATA mo_form TYPE REF TO zcl_abapgit_html_form.
    DATA mo_form_data TYPE REF TO zcl_abapgit_string_map.
    DATA mo_validation_log TYPE REF TO zcl_abapgit_string_map.
    DATA mo_form_util TYPE REF TO zcl_abapgit_html_form_utils.

    METHODS get_form_schema
      RETURNING
        VALUE(ro_form) TYPE REF TO zcl_abapgit_html_form.

    METHODS init_form
      RAISING
        zcx_abapgit_exception.

    METHODS validate_form
      IMPORTING
        !io_form_data            TYPE REF TO zcl_abapgit_string_map
      RETURNING
        VALUE(ro_validation_log) TYPE REF TO zcl_abapgit_string_map
      RAISING
        zcx_abapgit_exception.

    METHODS change_package
      IMPORTING
        !iv_old_package TYPE devclass
        !iv_new_package TYPE devclass
        !iv_remove_old  TYPE abap_bool
      RAISING
        zcx_abapgit_exception.

    METHODS get_package_mapping
      IMPORTING
        !iv_old_package   TYPE devclass
        !iv_new_package   TYPE devclass
      RETURNING
        VALUE(rt_mapping) TYPE ty_mapping
      RAISING
        zcx_abapgit_exception.

    METHODS create_package_hierarchy
      IMPORTING
        !it_mapping TYPE ty_mapping
      RAISING
        zcx_abapgit_exception.

    METHODS delete_packages
      IMPORTING
        !it_tadir TYPE zif_abapgit_definitions=>ty_tadir_tt
      RAISING
        zcx_abapgit_exception.

    METHODS change_package_assignment
      IMPORTING
        !it_mapping TYPE ty_mapping
        !it_tadir   TYPE zif_abapgit_definitions=>ty_tadir_tt
      RAISING
        zcx_abapgit_exception.

    METHODS update_sotr_package_assignment
      IMPORTING
        !it_mapping TYPE ty_mapping
      RAISING
        zcx_abapgit_exception.

    METHODS update_repo_persistence
      IMPORTING
        !iv_old_package TYPE devclass
        !iv_new_package TYPE devclass
      RAISING
        zcx_abapgit_exception.

    METHODS update_repo_checksums
      IMPORTING
        !it_mapping TYPE ty_mapping
      RAISING
        zcx_abapgit_exception.
ENDCLASS.



CLASS zcl_abapgit_gui_page_chg_pckg IMPLEMENTATION.


  METHOD change_package.

    DATA:
      lt_mapping TYPE ty_mapping,
      lt_tadir   TYPE zif_abapgit_definitions=>ty_tadir_tt.

    ASSERT iv_old_package <> iv_new_package.

    lt_mapping = get_package_mapping(
      iv_old_package = iv_old_package
      iv_new_package = iv_new_package ).

    lt_tadir = mo_repo->get_tadir_objects( ).

    create_package_hierarchy( lt_mapping ).

    change_package_assignment(
      it_mapping = lt_mapping
      it_tadir   = lt_tadir ).

    update_sotr_package_assignment( lt_mapping ).

    update_repo_persistence(
      iv_old_package = iv_old_package
      iv_new_package = iv_new_package ).

    update_repo_checksums( lt_mapping ).

    IF iv_remove_old = abap_true.
      delete_packages( lt_tadir ).
    ENDIF.

    zcl_abapgit_repo_srv=>get_instance( )->init( ).

  ENDMETHOD.


  METHOD change_package_assignment.

    DATA ls_tadir LIKE LINE OF it_tadir.

    FIELD-SYMBOLS <ls_map> LIKE LINE OF it_mapping.

    " TODO: Transportable packages (add to transport)
    LOOP AT it_tadir INTO ls_tadir WHERE object <> 'DEVC' AND object <> 'NSPC'.
      READ TABLE it_mapping ASSIGNING <ls_map> WITH KEY old_package = ls_tadir-devclass.
      ASSERT sy-subrc = 0.

      ls_tadir-devclass = <ls_map>-new_package.

      CALL FUNCTION 'TR_TADIR_INTERFACE'
        EXPORTING
          wi_tadir_pgmid    = ls_tadir-pgmid
          wi_tadir_object   = ls_tadir-object
          wi_tadir_obj_name = ls_tadir-obj_name
          wi_tadir_devclass = ls_tadir-devclass
          wi_test_modus     = abap_false
        EXCEPTIONS
          OTHERS            = 1.
      IF sy-subrc <> 0.
        zcx_abapgit_exception=>raise_t100( ).
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD constructor.

    super->constructor( ).
    mo_repo = io_repo.
    CREATE OBJECT mo_validation_log.
    CREATE OBJECT mo_form_data.
    mo_form = get_form_schema( ).
    mo_form_util = zcl_abapgit_html_form_utils=>create( mo_form ).

    IF mo_repo->get_dot_abapgit( )->get_folder_logic( ) <> zif_abapgit_dot_abapgit=>c_folder_logic-prefix.
      zcx_abapgit_exception=>raise( 'Feature is only supported repositories with prefix folder logic' ).
    ENDIF.

    IF zcl_abapgit_factory=>get_cts_api( )->is_chrec_possible_for_package( mo_repo->get_package( ) ) = abap_true.
      zcx_abapgit_exception=>raise( 'Feature is only supported local packages (no transport)' ).
    ENDIF.

  ENDMETHOD.


  METHOD create.

    DATA lo_component TYPE REF TO zcl_abapgit_gui_page_chg_pckg.

    CREATE OBJECT lo_component
      EXPORTING
        io_repo = io_repo.

    ri_page = zcl_abapgit_gui_page_hoc=>create(
      iv_page_title      = 'Change Repository Package'
      ii_child_component = lo_component ).

  ENDMETHOD.


  METHOD create_package_hierarchy.

    DATA:
      ls_package TYPE zif_abapgit_sap_package=>ty_create,
      ls_map     TYPE ty_map.

    FIELD-SYMBOLS <ls_map> LIKE LINE OF it_mapping.

    LOOP AT it_mapping ASSIGNING <ls_map>.
      ls_package = zcl_abapgit_factory=>get_sap_package( <ls_map>-old_package )->get( ).

      ls_package-devclass = <ls_map>-new_package.
      ls_package-as4user  = sy-uname.

      READ TABLE it_mapping INTO ls_map WITH KEY old_package = ls_package-parentcl.
      IF sy-subrc = 0.
        ls_package-parentcl = ls_map-new_package.
      ENDIF.

      zcl_abapgit_factory=>get_sap_package( ls_map-new_package )->create( ls_package ).
    ENDLOOP.

    " TODO: Transportable packages (add to transport and tadir)

  ENDMETHOD.


  METHOD delete_packages.

    DATA lt_tadir LIKE it_tadir.

    lt_tadir = it_tadir.

    DELETE lt_tadir WHERE object <> 'DEVC'.

    SORT lt_tadir DESCENDING BY obj_name.

    zcl_abapgit_objects=>delete( lt_tadir ).

  ENDMETHOD.


  METHOD get_form_schema.

    ro_form = zcl_abapgit_html_form=>create( iv_form_id = 'change-package' ).

    ro_form->text(
      iv_name        = c_id-new_package
      iv_label       = 'New Package'
      iv_required    = abap_true
      iv_upper_case  = abap_true
      iv_side_action = c_event-choose_package_new
      iv_max         = 30
    )->text(
      iv_name        = c_id-old_package
      iv_label       = 'Old Package'
      iv_readonly    = abap_true
      iv_required    = abap_true
      iv_upper_case  = abap_true
      iv_side_action = c_event-choose_package_old
      iv_max         = 30
    )->checkbox(
      iv_name        = c_id-remove_old
      iv_label       = 'Delete Old Packages'
    )->command(
      iv_label       = 'Change Package'
      iv_action      = c_event-change_package
      iv_cmd_type    = zif_abapgit_html_form=>c_cmd_type-input_main
    )->command(
      iv_label       = 'Back'
      iv_action      = zif_abapgit_definitions=>c_action-go_back ).

  ENDMETHOD.


  METHOD get_package_mapping.

    DATA:
      lt_old_packages TYPE zif_abapgit_sap_package=>ty_devclass_tt,
      lv_old_package  TYPE string,
      lv_new_package  TYPE string.

    FIELD-SYMBOLS <ls_map> LIKE LINE OF rt_mapping.

    IF mo_repo->get_local_settings( )-ignore_subpackages = abap_false.
      lt_old_packages = zcl_abapgit_factory=>get_sap_package( iv_old_package )->list_subpackages( ).
    ENDIF.
    INSERT iv_old_package INTO TABLE lt_old_packages.

    " Check new package names and make sure they don't exists
    LOOP AT lt_old_packages INTO lv_old_package.
      lv_new_package = lv_old_package.
      REPLACE FIRST OCCURRENCE OF iv_old_package IN lv_new_package WITH iv_new_package.
      IF strlen( lv_new_package ) > 30.
        zcx_abapgit_exception=>raise( |Package { lv_new_package } longer than 30 characters| ).
      ENDIF.
      IF zcl_abapgit_factory=>get_sap_package( |{ lv_new_package }| )->exists( ) = abap_true.
        zcx_abapgit_exception=>raise( |Package { lv_new_package } already exists| ).
      ENDIF.
      APPEND INITIAL LINE TO rt_mapping ASSIGNING <ls_map>.
      <ls_map>-old_package = lv_old_package.
      <ls_map>-new_package = lv_new_package.
    ENDLOOP.

    SORT rt_mapping.

  ENDMETHOD.


  METHOD init_form.

    mo_form_data->set(
      iv_key = c_id-old_package
      iv_val = mo_repo->get_package( ) ).

    mo_form_data->set(
      iv_key = c_id-remove_old
      iv_val = abap_true ).

  ENDMETHOD.


  METHOD update_repo_checksums.

    DATA:
      lv_key       TYPE zif_abapgit_persistence=>ty_repo-key,
      lo_checksums TYPE REF TO zcl_abapgit_repo_checksums,
      lt_checksums TYPE zif_abapgit_persistence=>ty_local_checksum_tt.

    FIELD-SYMBOLS:
      <ls_checksum> LIKE LINE OF lt_checksums,
      <ls_map>      LIKE LINE OF it_mapping.

    lv_key = mo_repo->get_key( ).

    CREATE OBJECT lo_checksums EXPORTING iv_repo_key = lv_key.

    lt_checksums = lo_checksums->zif_abapgit_repo_checksums~get( ).

    LOOP AT lt_checksums ASSIGNING <ls_checksum> WHERE item-devclass IS NOT INITIAL.
      READ TABLE it_mapping ASSIGNING <ls_map> WITH KEY old_package = <ls_checksum>-item-devclass.
      ASSERT sy-subrc = 0.

      <ls_checksum>-item-devclass = <ls_map>-new_package.
    ENDLOOP.

    lo_checksums->force_write( lt_checksums ).

  ENDMETHOD.


  METHOD update_repo_persistence.

    DATA:
      lv_key         TYPE zif_abapgit_persistence=>ty_repo-key,
      lo_persist     TYPE REF TO zif_abapgit_persist_repo,
      ls_repo_data   TYPE zif_abapgit_persistence=>ty_repo,
      ls_meta        TYPE zif_abapgit_persistence=>ty_repo_xml,
      ls_change_mask TYPE zif_abapgit_persistence=>ty_repo_meta_mask.

    lv_key = mo_repo->get_key( ).
    lo_persist = zcl_abapgit_persist_factory=>get_repo( ).

    TRY.
        ls_repo_data = lo_persist->read( lv_key ).
      CATCH zcx_abapgit_not_found.
        zcx_abapgit_exception=>raise( 'Repository not found' ).
    ENDTRY.

    ASSERT ls_repo_data-package = iv_old_package.

    ls_repo_data-package = iv_new_package.

    MOVE-CORRESPONDING ls_repo_data TO ls_meta.
    ls_change_mask-package = abap_true.

    lo_persist->update_metadata(
      iv_key         = lv_key
      is_meta        = ls_meta
      is_change_mask = ls_change_mask ).

  ENDMETHOD.


  METHOD update_sotr_package_assignment.

    FIELD-SYMBOLS <ls_map> LIKE LINE OF it_mapping.

    LOOP AT it_mapping ASSIGNING <ls_map>.
      zcl_abapgit_sotr_handler=>change_sotr_package(
        iv_old_package = <ls_map>-old_package
        iv_new_package = <ls_map>-new_package ).
    ENDLOOP.

  ENDMETHOD.


  METHOD validate_form.

    DATA lv_new_package TYPE devclass.

    ro_validation_log = mo_form_util->validate( io_form_data ).

    lv_new_package = io_form_data->get( c_id-new_package ).

    IF lv_new_package IS NOT INITIAL AND lv_new_package = io_form_data->get( c_id-old_package ).
      ro_validation_log->set(
        iv_key = c_id-new_package
        iv_val = 'New package must be different from old package' ).
    ENDIF.

    IF zcl_abapgit_factory=>get_cts_api( )->is_chrec_possible_for_package( lv_new_package ) = abap_true.
      ro_validation_log->set(
        iv_key = c_id-new_package
        iv_val = 'Feature is only supported local packages (no transport)' ).
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_gui_event_handler~on_event.

    mo_form_data = mo_form_util->normalize( ii_event->form_data( ) ).

    CASE ii_event->mv_action.
      WHEN c_event-change_package.

        mo_validation_log = validate_form( mo_form_data ).

        IF mo_validation_log->is_empty( ) = abap_false.
          rs_handled-state = zcl_abapgit_gui=>c_event_state-re_render.
        ELSE.
          change_package(
            iv_old_package = |{ mo_form_data->get( c_id-old_package ) }|
            iv_new_package = |{ mo_form_data->get( c_id-new_package ) }|
            iv_remove_old  = |{ mo_form_data->get( c_id-remove_old ) }| ).

          MESSAGE 'Package successfully changed' TYPE 'S'.
          rs_handled-state = zcl_abapgit_gui=>c_event_state-go_back.
        ENDIF.

      WHEN c_event-choose_package_old.

        mo_form_data->set(
          iv_key = c_id-old_package
          iv_val = zcl_abapgit_ui_factory=>get_popups( )->popup_search_help( 'TDEVC-DEVCLASS' ) ).

        IF mo_form_data->get( c_id-old_package ) IS NOT INITIAL.
          rs_handled-state = zcl_abapgit_gui=>c_event_state-re_render.
        ELSE.
          rs_handled-state = zcl_abapgit_gui=>c_event_state-no_more_act.
        ENDIF.

      WHEN c_event-choose_package_new.

        mo_form_data->set(
          iv_key = c_id-new_package
          iv_val = zcl_abapgit_ui_factory=>get_popups( )->popup_search_help( 'TDEVC-DEVCLASS' ) ).

        IF mo_form_data->get( c_id-new_package ) IS NOT INITIAL.
          rs_handled-state = zcl_abapgit_gui=>c_event_state-re_render.
        ELSE.
          rs_handled-state = zcl_abapgit_gui=>c_event_state-no_more_act.
        ENDIF.
    ENDCASE.

  ENDMETHOD.


  METHOD zif_abapgit_gui_renderable~render.

    init_form( ).

    register_handlers( ).

    ri_html = zcl_abapgit_html=>create( ).

    ri_html->add( mo_form->render(
      io_values         = mo_form_data
      io_validation_log = mo_validation_log ) ).

  ENDMETHOD.
ENDCLASS.
