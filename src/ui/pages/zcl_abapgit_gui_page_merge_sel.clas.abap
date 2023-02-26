CLASS zcl_abapgit_gui_page_merge_sel DEFINITION
  PUBLIC
  INHERITING FROM zcl_abapgit_gui_component
  FINAL
  CREATE PRIVATE.

  PUBLIC SECTION.

    INTERFACES zif_abapgit_gui_event_handler.
    INTERFACES zif_abapgit_gui_renderable.

    CLASS-METHODS create
      IMPORTING
        !ii_repo       TYPE REF TO zif_abapgit_repo
      RETURNING
        VALUE(ri_page) TYPE REF TO zif_abapgit_gui_renderable
      RAISING
        zcx_abapgit_exception.

    METHODS constructor
      IMPORTING
        !ii_repo TYPE REF TO zif_abapgit_repo
      RAISING
        zcx_abapgit_exception.

  PROTECTED SECTION.
  PRIVATE SECTION.

    CONSTANTS:
      BEGIN OF c_id,
        branches TYPE string VALUE 'branches',
        source   TYPE string VALUE 'source',
        target   TYPE string VALUE 'target',
      END OF c_id.

    CONSTANTS:
      BEGIN OF c_event,
        merge TYPE string VALUE 'merge',
      END OF c_event.

    DATA mo_form TYPE REF TO zcl_abapgit_html_form.
    DATA mo_form_data TYPE REF TO zcl_abapgit_string_map.
    DATA mo_form_util TYPE REF TO zcl_abapgit_html_form_utils.
    DATA mo_repo TYPE REF TO zcl_abapgit_repo_online.
    DATA mt_branches TYPE zif_abapgit_git_definitions=>ty_git_branch_list_tt.

    METHODS read_branches
      RAISING
        zcx_abapgit_exception.

    METHODS get_form_schema
      RETURNING
        VALUE(ro_form) TYPE REF TO zcl_abapgit_html_form
      RAISING
        zcx_abapgit_exception.
ENDCLASS.



CLASS ZCL_ABAPGIT_GUI_PAGE_MERGE_SEL IMPLEMENTATION.


  METHOD constructor.

    super->constructor( ).
    CREATE OBJECT mo_form_data.
    mo_repo ?= ii_repo.

    read_branches( ).

    mo_form = get_form_schema( ).
    mo_form_util = zcl_abapgit_html_form_utils=>create( mo_form ).

  ENDMETHOD.


  METHOD create.

    DATA lo_component TYPE REF TO zcl_abapgit_gui_page_merge_sel.

    CREATE OBJECT lo_component
      EXPORTING
        ii_repo = ii_repo.

    ri_page = zcl_abapgit_gui_page_hoc=>create(
      iv_page_title      = 'Merge Branches'
      ii_child_component = lo_component ).

  ENDMETHOD.


  METHOD get_form_schema.

    FIELD-SYMBOLS <ls_branch> LIKE LINE OF mt_branches.

    ro_form = zcl_abapgit_html_form=>create(
                iv_form_id   = 'merge-branches-form'
                iv_help_page = 'https://docs.abapgit.org/' ). " todo, add docs

    ro_form->start_group(
      iv_name  = c_id-branches
      iv_label = 'Branch Selection'
      iv_hint  = 'Select the branches that should be merged'
    )->radio(
      iv_name          = c_id-source
      iv_label         = 'Source Branch'
      iv_default_value = substring(
                           val = mo_repo->get_selected_branch( )
                           off = 11 )
      iv_condense      = abap_true ).

    LOOP AT mt_branches ASSIGNING <ls_branch>.
      ro_form->option(
        iv_label = <ls_branch>-display_name
        iv_value = <ls_branch>-display_name ).
    ENDLOOP.

    ro_form->radio(
      iv_name     = c_id-target
      iv_label    = 'Target Branch'
      iv_condense = abap_true ).

    LOOP AT mt_branches ASSIGNING <ls_branch>.
      ro_form->option(
        iv_label = <ls_branch>-display_name
        iv_value = <ls_branch>-display_name ).
    ENDLOOP.

    ro_form->command(
      iv_label    = 'Preview Merge'
      iv_cmd_type = zif_abapgit_html_form=>c_cmd_type-input_main
      iv_action   = c_event-merge
    )->command(
      iv_label    = 'Back'
      iv_action   = zif_abapgit_definitions=>c_action-go_back ).

  ENDMETHOD.


  METHOD read_branches.

    DATA lo_branches TYPE REF TO zcl_abapgit_git_branch_list.

    lo_branches = zcl_abapgit_git_transport=>branches( mo_repo->get_url( ) ).
    mt_branches = lo_branches->get_branches_only( ).

    DELETE mt_branches WHERE name = zif_abapgit_definitions=>c_head_name.

  ENDMETHOD.


  METHOD zif_abapgit_gui_event_handler~on_event.

    DATA lo_merge TYPE REF TO zcl_abapgit_gui_page_merge.

    mo_form_data = mo_form_util->normalize( ii_event->form_data( ) ).

    CASE ii_event->mv_action.
      WHEN zif_abapgit_definitions=>c_action-go_back.
        rs_handled-state = zcl_abapgit_gui=>c_event_state-go_back.

      WHEN c_event-merge.
        IF mo_form_data->get( c_id-source ) = mo_form_data->get( c_id-target ).
          zcx_abapgit_exception=>raise( 'Select different branches' ).
        ENDIF.

        CREATE OBJECT lo_merge
          EXPORTING
            io_repo   = mo_repo
            iv_source = mo_form_data->get( c_id-source )
            iv_target = mo_form_data->get( c_id-target ).

        rs_handled-page = lo_merge.
        rs_handled-state = zcl_abapgit_gui=>c_event_state-new_page.

    ENDCASE.

  ENDMETHOD.


  METHOD zif_abapgit_gui_renderable~render.

    register_handlers( ).

    CREATE OBJECT ri_html TYPE zcl_abapgit_html.

    ri_html->add( `<div class="repo">` ).

    ri_html->add( zcl_abapgit_gui_chunk_lib=>render_repo_top(
                    io_repo               = mo_repo
                    iv_show_commit        = abap_false
                    iv_interactive_branch = abap_false ) ).

    ri_html->add( mo_form->render( io_values = mo_form_data ) ).

    ri_html->add( `</div>` ).

  ENDMETHOD.
ENDCLASS.
