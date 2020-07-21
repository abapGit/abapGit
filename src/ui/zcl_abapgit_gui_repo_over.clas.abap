CLASS zcl_abapgit_gui_repo_over DEFINITION
  PUBLIC
  INHERITING FROM zcl_abapgit_gui_component
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_abapgit_gui_renderable .
    DATA: mv_order_by         TYPE string READ-ONLY.

    METHODS constructor
      RAISING zcx_abapgit_exception.
    METHODS set_order_by
      IMPORTING
        iv_order_by TYPE string.
    METHODS set_order_direction
      IMPORTING
        iv_order_descending TYPE abap_bool.

    METHODS set_filter
      IMPORTING
        it_postdata TYPE cnht_post_data_tab.

    METHODS has_favorites
      RETURNING VALUE(rv_has_favorites) TYPE abap_bool.

  PROTECTED SECTION.


  PRIVATE SECTION.
    TYPES:
      BEGIN OF ty_overview,
        favorite        TYPE string,
        type            TYPE string,
        key             TYPE string,
        name            TYPE string,
        url             TYPE string,
        package         TYPE string,
        branch          TYPE string,
        created_by      TYPE string,
        created_at      TYPE string,
        deserialized_by TYPE string,
        deserialized_at TYPE string,
      END OF ty_overview,
      tty_overview TYPE STANDARD TABLE OF ty_overview
                   WITH NON-UNIQUE DEFAULT KEY.
    CONSTANTS:
      BEGIN OF c_action,
        select       TYPE string VALUE 'select',
        apply_filter TYPE string VALUE 'apply_filter',
      END OF c_action .

    DATA: mv_order_descending TYPE abap_bool,
          mv_filter           TYPE string,
          mv_time_zone        TYPE timezone,
          mt_col_spec         TYPE zif_abapgit_definitions=>tty_col_spec,
          mt_overview         TYPE tty_overview.

    METHODS: render_text_input
      IMPORTING iv_name        TYPE string
                iv_label       TYPE string
                iv_value       TYPE string OPTIONAL
                iv_max_length  TYPE string OPTIONAL
      RETURNING VALUE(ri_html) TYPE REF TO zif_abapgit_html,

      apply_filter
        CHANGING
          ct_overview TYPE tty_overview,

      map_repo_list_to_overview
        IMPORTING
          it_repo_list       TYPE zif_abapgit_persistence=>tt_repo
        RETURNING
          VALUE(rt_overview) TYPE tty_overview
        RAISING
          zcx_abapgit_exception,

      render_table_header
        IMPORTING
          ii_html TYPE REF TO zif_abapgit_html,

      render_table
        IMPORTING
          ii_html     TYPE REF TO zif_abapgit_html
          it_overview TYPE tty_overview,

      render_table_body
        IMPORTING
          ii_html     TYPE REF TO zif_abapgit_html
          it_overview TYPE tty_overview,

      render_header_bar
        IMPORTING
          ii_html TYPE REF TO zif_abapgit_html,

      apply_order_by
        CHANGING ct_overview TYPE tty_overview,

      _add_column
        IMPORTING
          iv_tech_name    TYPE string OPTIONAL
          iv_display_name TYPE string OPTIONAL
          iv_css_class    TYPE string OPTIONAL
          iv_add_tz       TYPE abap_bool OPTIONAL
          iv_title        TYPE string OPTIONAL.

    METHODS render_scripts
      RETURNING
        VALUE(ri_html) TYPE REF TO zif_abapgit_html
      RAISING
        zcx_abapgit_exception.
ENDCLASS.



CLASS zcl_abapgit_gui_repo_over IMPLEMENTATION.


  METHOD apply_filter.

    IF mv_filter IS NOT INITIAL.

      DELETE ct_overview WHERE key             NS mv_filter
                           AND name            NS mv_filter
                           AND url             NS mv_filter
                           AND package         NS mv_filter
                           AND branch          NS mv_filter
                           AND created_by      NS mv_filter
                           AND created_at      NS mv_filter
                           AND deserialized_by NS mv_filter
                           AND deserialized_at NS mv_filter.

    ENDIF.

  ENDMETHOD.


  METHOD apply_order_by.

    DATA:
      lt_sort TYPE abap_sortorder_tab,
      ls_sort LIKE LINE OF lt_sort.

    ls_sort-name = 'FAVORITE'.
    ls_sort-descending = abap_true.
    ls_sort-astext = abap_true.
    INSERT ls_sort INTO TABLE lt_sort.

    IF mv_order_by IS NOT INITIAL.

      ls_sort-name       = mv_order_by.
      ls_sort-descending = mv_order_descending.
      ls_sort-astext     = abap_true.
      INSERT ls_sort INTO TABLE lt_sort.
    ENDIF.

    SORT ct_overview BY (lt_sort).


  ENDMETHOD.


  METHOD constructor.

    super->constructor( ).
    mv_order_by = |NAME|.

    CALL FUNCTION 'GET_SYSTEM_TIMEZONE'
      IMPORTING
        timezone            = mv_time_zone
      EXCEPTIONS
        customizing_missing = 1
        OTHERS              = 2.
    ASSERT sy-subrc = 0.

  ENDMETHOD.


  METHOD has_favorites.
    READ TABLE mt_overview WITH KEY favorite = abap_true TRANSPORTING NO FIELDS.
    IF sy-subrc = 0.
      rv_has_favorites = abap_true.
    ENDIF.
  ENDMETHOD.


  METHOD map_repo_list_to_overview.

    DATA: ls_overview LIKE LINE OF rt_overview,
          lo_repo_srv TYPE REF TO zcl_abapgit_repo,
          lv_date     TYPE d,
          lv_time     TYPE t.

    FIELD-SYMBOLS: <ls_repo> LIKE LINE OF it_repo_list.


    LOOP AT it_repo_list ASSIGNING <ls_repo>.

      CLEAR: ls_overview.
      lo_repo_srv = zcl_abapgit_repo_srv=>get_instance( )->get( <ls_repo>-key ).

      ls_overview-favorite   = zcl_abapgit_persistence_user=>get_instance(
        )->is_favorite_repo( <ls_repo>-key ).
      ls_overview-type       = <ls_repo>-offline.
      ls_overview-key        = <ls_repo>-key.
      ls_overview-name       = lo_repo_srv->get_name( ).
      ls_overview-url        = <ls_repo>-url.
      ls_overview-package    = <ls_repo>-package.
      ls_overview-branch     = zcl_abapgit_git_branch_list=>get_display_name( <ls_repo>-branch_name ).
      ls_overview-created_by = <ls_repo>-created_by.

      IF <ls_repo>-created_at IS NOT INITIAL.
        CONVERT TIME STAMP <ls_repo>-created_at
                TIME ZONE mv_time_zone
                INTO DATE lv_date
                     TIME lv_time.

        ls_overview-created_at = |{ lv_date DATE = USER } { lv_time TIME = USER }|.
      ENDIF.

      ls_overview-deserialized_by = <ls_repo>-deserialized_by.

      IF <ls_repo>-deserialized_at IS NOT INITIAL.
        CONVERT TIME STAMP <ls_repo>-deserialized_at
                TIME ZONE mv_time_zone
                INTO DATE lv_date
                     TIME lv_time.

        ls_overview-deserialized_at = |{ lv_date DATE = USER } { lv_time TIME = USER }|.
      ENDIF.

      INSERT ls_overview INTO TABLE rt_overview.

    ENDLOOP.

  ENDMETHOD.


  METHOD render_header_bar.

    ii_html->add( |<div class="form-container">| ).

    ii_html->add( |<form class="inline" method="post" action="sapevent:{ c_action-apply_filter }">| ).

    ii_html->add( render_text_input(
      iv_name  = |filter|
      iv_label = |Filter: |
      iv_value = mv_filter ) ).
    ii_html->add( |<input type="submit" class="hidden-submit">| ).
    ii_html->add( |</form>| ).

    ii_html->add( zcl_abapgit_html=>a(
      iv_txt = '<i id="icon-filter-favorite" class="icon icon-check"></i> Only favorites'
      iv_act = |gHelper.toggleRepoListFavorites()|
      iv_typ = zif_abapgit_html=>c_action_type-onclick ) ).

    ii_html->add( `<span class="separator">|</span>` ).

    ii_html->add( zcl_abapgit_html=>a(
      iv_txt = '<i id="icon-filter-detail" class="icon icon-check"></i> Detail'
      iv_act = |gHelper.toggleRepoListDetail()|
      iv_typ = zif_abapgit_html=>c_action_type-onclick ) ).

    ii_html->add( |</div>| ).

  ENDMETHOD.


  METHOD render_scripts.

    CREATE OBJECT ri_html TYPE zcl_abapgit_html.

    ri_html->set_title( cl_abap_typedescr=>describe_by_object_ref( me )->get_relative_name( ) ).
    ri_html->add( 'setInitialFocus("filter");' ).
    ri_html->add( 'var gHelper = new RepoOverViewHelper();' ).

  ENDMETHOD.


  METHOD render_table.

    ii_html->add( |<div class="db_list repo-overview">| ).
    ii_html->add( |<table class="db_tab w100">| ).

    render_table_header( ii_html ).
    render_table_body( ii_html     = ii_html
                       it_overview = it_overview ).

    ii_html->add( |</table>| ).
    ii_html->add( |</div>| ).

  ENDMETHOD.


  METHOD render_table_body.

    CONSTANTS: lc_separator TYPE string VALUE `<span class="separator">|</span>`.

    DATA:
      lv_type_icon           TYPE string,
      lv_favorite_icon       TYPE string,
      lv_favorite_class      TYPE string,
      lv_package_jump_data   TYPE string,
      lv_package_obj_name    TYPE sobj_name,
      lv_stage_link          TYPE string,
      lv_patch_link          TYPE string,
      lv_code_inspector_link TYPE string.

    FIELD-SYMBOLS: <ls_overview> LIKE LINE OF it_overview.

    ii_html->add( '<tbody>' ).

    LOOP AT it_overview ASSIGNING <ls_overview>.

      IF <ls_overview>-type = abap_true.
        lv_type_icon = 'plug/darkgrey'.
      ELSE.
        lv_type_icon = 'cloud-upload-alt/darkgrey'.
      ENDIF.

      IF <ls_overview>-favorite = abap_true.
        lv_favorite_icon = 'star/blue'.
        lv_favorite_class = 'favorite'.
      ELSE.
        lv_favorite_icon = 'star/grey'.
        lv_favorite_class = ''.
      ENDIF.

      ii_html->add( |<tr class="repo { lv_favorite_class }">| ).
      ii_html->add( |<td class="wmin">| ).
      ii_html->add_a( iv_act = |{ zif_abapgit_definitions=>c_action-repo_toggle_fav }?{ <ls_overview>-key }|
                      iv_txt = zcl_abapgit_html=>icon( iv_name  = lv_favorite_icon
                                                       iv_class = 'pad-sides'
                                                       iv_hint  = 'Click to toggle favorite' ) ).
      ii_html->add( |</td>| ).
      ii_html->add( |<td class="wmin">{ zcl_abapgit_html=>icon( lv_type_icon )  }</td>| ).

      ii_html->add( |<td>{ zcl_abapgit_html=>a( iv_txt = <ls_overview>-name
                                                iv_act = |{ c_action-select }?{ <ls_overview>-key }| ) }</td>| ).

      IF <ls_overview>-type = abap_false.
        ii_html->add( |<td>{ ii_html->a( iv_txt = <ls_overview>-url
                                         iv_act = |{ zif_abapgit_definitions=>c_action-url }?|
                                               && |{ <ls_overview>-url }| ) }</td>| ).
      ELSE.
        ii_html->add( |<td></td>| ).
      ENDIF.

      lv_package_obj_name = <ls_overview>-package.
      lv_package_jump_data = zcl_abapgit_html_action_utils=>jump_encode(
        iv_obj_type = 'DEVC'
        iv_obj_name = lv_package_obj_name ).

      ii_html->add( |<td>{ ii_html->a(
          iv_txt = <ls_overview>-package
          iv_act = |{ zif_abapgit_definitions=>c_action-jump }?{ lv_package_jump_data }| ) }</td>| ).

      ii_html->add( |<td>{ ii_html->a(
        iv_txt = <ls_overview>-branch
        iv_act = |{ zif_abapgit_definitions=>c_action-git_branch_switch }?{ <ls_overview>-key }| ) }</td>| ).

      ii_html->add( |<td class="ro-detail">{ <ls_overview>-deserialized_by }</td>| ).
      ii_html->add( |<td class="ro-detail">{ <ls_overview>-deserialized_at }</td>| ).
      ii_html->add( |<td class="ro-detail">{ <ls_overview>-created_by }</td>| ).
      ii_html->add( |<td class="ro-detail">{ <ls_overview>-created_at }</td>| ).
      ii_html->add( |<td class="ro-detail">{ <ls_overview>-key }</td>| ).

      ii_html->add( |<td class='ro-action'> | ).

      lv_stage_link = ii_html->a(
        iv_txt = |Stage|
        iv_act = |{ zif_abapgit_definitions=>c_action-go_stage }?{ <ls_overview>-key } | ).

      lv_patch_link = ii_html->a(
        iv_txt = |Patch|
        iv_act = |{ zif_abapgit_definitions=>c_action-go_patch }?{ <ls_overview>-key } | ).

      lv_code_inspector_link = ii_html->a(
        iv_txt = |Code inspector|
        iv_act = |{ zif_abapgit_definitions=>c_action-repo_code_inspector }?{ <ls_overview>-key } | ).

      ii_html->add( lv_code_inspector_link && lc_separator && lv_stage_link && lc_separator && lv_patch_link ).

      ii_html->add( |</td>| ).

      ii_html->add( |<td class='ro-go'><span>{
                zcl_abapgit_html=>a(
                  iv_txt = `&rsaquo;`
                  iv_act = |{ c_action-select }?{ <ls_overview>-key }| ) }</span></td>| ).

      ii_html->add( |</tr>| ).

    ENDLOOP.

    ii_html->add( |</tbody>| ).

  ENDMETHOD.


  METHOD render_table_header.

    CLEAR mt_col_spec.

    _add_column(
      iv_tech_name = 'FAVORITE'
      iv_css_class = 'wmin' ).

    _add_column(
      iv_tech_name = 'TYPE'
      iv_css_class = 'wmin' ).

    _add_column(
      iv_tech_name = 'NAME'
      iv_display_name = 'Name' ).

    _add_column(
      iv_tech_name = 'URL'
      iv_display_name = 'Url' ).

    _add_column(
      iv_tech_name = 'PACKAGE'
      iv_display_name = 'Package' ).

    _add_column(
      iv_tech_name = 'BRANCH'
      iv_display_name = 'Branch' ).

    _add_column(
      iv_tech_name = 'DESERIALIZED_BY'
      iv_display_name = 'Deserialized by'
      iv_css_class = 'ro-detail' ).

    _add_column(
      iv_tech_name = 'DESERIALIZED_AT'
      iv_display_name = 'Deserialized at'
      iv_css_class = 'ro-detail'
      iv_add_tz = abap_true ).

    _add_column(
      iv_tech_name = 'CREATED_BY'
      iv_display_name = 'Created by'
      iv_css_class = 'ro-detail' ).


    _add_column(
      iv_tech_name = 'CREATED_TAT'
      iv_display_name = 'Created at'
      iv_css_class = 'ro-detail'
      iv_add_tz = abap_true ).

    _add_column(
      iv_tech_name = 'KEY'
      iv_display_name = 'Key'
      iv_css_class = 'ro-detail' ).

    _add_column(
      iv_tech_name = 'ACTION'
      iv_display_name = 'Action'
      iv_css_class = 'ro-action' ).

    _add_column(
      iv_tech_name = 'GO'
      iv_css_class = 'ro-go' ).

    ii_html->add( |<thead>| ).
    ii_html->add( |<tr>| ).

    ii_html->add( zcl_abapgit_gui_chunk_lib=>render_order_by_header_cells(
                      it_col_spec         = mt_col_spec
                      iv_order_by         = mv_order_by
                      iv_order_descending = mv_order_descending ) ).

    ii_html->add( '</tr>' ).
    ii_html->add( '</thead>' ).

  ENDMETHOD.


  METHOD render_text_input.

    DATA lv_attrs TYPE string.

    CREATE OBJECT ri_html TYPE zcl_abapgit_html.

    IF iv_value IS NOT INITIAL.
      lv_attrs = | value="{ iv_value }"|.
    ENDIF.

    IF iv_max_length IS NOT INITIAL.
      lv_attrs = | maxlength="{ iv_max_length }"|.
    ENDIF.

    ri_html->add( |<label for="{ iv_name }">{ iv_label }</label>| ).
    ri_html->add( |<input id="{ iv_name }" name="{ iv_name }" type="text"{ lv_attrs }>| ).

  ENDMETHOD.


  METHOD set_filter.

    FIELD-SYMBOLS: <lv_postdata> LIKE LINE OF it_postdata.

    READ TABLE it_postdata ASSIGNING <lv_postdata>
                           INDEX 1.
    IF sy-subrc = 0.
      FIND FIRST OCCURRENCE OF REGEX `filter=(.*)`
           IN <lv_postdata>
           SUBMATCHES mv_filter.
    ENDIF.

    mv_filter = condense( mv_filter ).

  ENDMETHOD.


  METHOD set_order_by.
    mv_order_by = iv_order_by.
  ENDMETHOD.


  METHOD set_order_direction.
    mv_order_descending = iv_order_descending.
  ENDMETHOD.


  METHOD zif_abapgit_gui_renderable~render.

    mt_overview = map_repo_list_to_overview( zcl_abapgit_persist_factory=>get_repo( )->list( ) ).
    apply_order_by( CHANGING ct_overview = mt_overview ).
    apply_filter( CHANGING ct_overview = mt_overview ).

    CREATE OBJECT ri_html TYPE zcl_abapgit_html.

    render_header_bar( ri_html ).
    render_table( ii_html     = ri_html
                  it_overview = mt_overview ).

    register_deferred_script( render_scripts( ) ).

  ENDMETHOD.


  METHOD _add_column.

    FIELD-SYMBOLS <ls_col> LIKE LINE OF mt_col_spec.
    APPEND INITIAL LINE TO mt_col_spec ASSIGNING <ls_col>.
    <ls_col>-display_name = iv_display_name.
    <ls_col>-tech_name = iv_tech_name.
    <ls_col>-title = iv_title.
    <ls_col>-css_class = iv_css_class.
    <ls_col>-add_tz = iv_add_tz.
  ENDMETHOD.

ENDCLASS.
