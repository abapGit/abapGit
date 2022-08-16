CLASS zcl_abapgit_gui_page_runit DEFINITION
  PUBLIC
  INHERITING FROM zcl_abapgit_gui_page
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS constructor
      IMPORTING
        !iv_devclass TYPE devclass
      RAISING
        zcx_abapgit_exception .

    METHODS:
      zif_abapgit_gui_event_handler~on_event
        REDEFINITION.

  PROTECTED SECTION.
    METHODS render_content
        REDEFINITION .
  PRIVATE SECTION.

    DATA mv_devclass TYPE devclass .

    METHODS build_tadir
      RETURNING
        VALUE(rt_tadir) TYPE sabp_t_tadir_keys
      RAISING
        zcx_abapgit_exception .
    METHODS run
      RETURNING
        VALUE(ro_result) TYPE REF TO cl_saunit_internal_result
      RAISING
        zcx_abapgit_exception .
ENDCLASS.



CLASS ZCL_ABAPGIT_GUI_PAGE_RUNIT IMPLEMENTATION.


  METHOD build_tadir.

    DATA lt_tadir TYPE zif_abapgit_definitions=>ty_tadir_tt.
    DATA ls_tadir LIKE LINE OF lt_tadir.
    DATA ls_row   LIKE LINE OF rt_tadir.

    lt_tadir = zcl_abapgit_factory=>get_tadir( )->read(
      iv_package            = mv_devclass
      iv_only_local_objects = abap_true ).

    LOOP AT lt_tadir INTO ls_tadir.
      CLEAR ls_row.
      ls_row-obj_type = ls_tadir-object.
      ls_row-obj_name = ls_tadir-obj_name.
      APPEND ls_row TO rt_tadir.
    ENDLOOP.

  ENDMETHOD.


  METHOD constructor.
    super->constructor( ).

    ms_control-page_title = |Run Unit Tests { iv_devclass }|.

    mv_devclass = iv_devclass.
  ENDMETHOD.


  METHOD render_content.

    DATA lo_result      TYPE REF TO cl_saunit_internal_result.
    DATA lv_program_ndx TYPE i.
    DATA lv_class_ndx   TYPE i.
    DATA lv_method_ndx  TYPE i.
    DATA lv_text        TYPE string.
    DATA lv_count       TYPE i.
    DATA ls_program     LIKE LINE OF lo_result->f_task_data-programs.
    DATA ls_class       LIKE LINE OF ls_program-classes.
    DATA ls_method      LIKE LINE OF ls_class-methods.


    CREATE OBJECT ri_html TYPE zcl_abapgit_html.

    ri_html->add( '<div class="repo">' ).

    lo_result = run( ).

    LOOP AT lo_result->f_task_data-alerts_by_indicies INTO DATA(ls_alert_by_index).
      LOOP AT ls_alert_by_index-alerts INTO DATA(ls_alert) WHERE kind = 'F'.
        LOOP AT ls_alert-header-params INTO DATA(lv_params).
          lv_text = lv_params.
        ENDLOOP.
        ri_html->add( |<span class="boxed red-filled-set">{ lv_text }</span><br>| ).
        lv_count = lv_count + 1.
      ENDLOOP.
    ENDLOOP.

    ri_html->add( |<b>{ lv_count } Errors</b><br>| ).

    ri_html->add( |<hr><table>| ).

    LOOP AT lo_result->f_task_data-programs INTO ls_program.
      lv_program_ndx = sy-tabix.
      ri_html->add( |<tr><td>{ ls_program-info-key-obj_type } { ls_program-info-key-obj_name }</td><td></td></tr>| ).
      LOOP AT ls_program-classes INTO ls_class.
        lv_class_ndx = sy-tabix.

        ri_html->add( |<tr><td>&nbsp;&nbsp;&nbsp;&nbsp;{ ls_class-info-name }</td><td></td></tr>| ).
        LOOP AT ls_class-methods INTO ls_method.
          lv_method_ndx = sy-tabix.

          CLEAR lv_text.
          READ TABLE lo_result->f_task_data-alerts_by_indicies WITH KEY
            program_ndx = lv_program_ndx
            class_ndx = lv_class_ndx
            method_ndx = lv_method_ndx
            INTO ls_alert_by_index.
          IF sy-subrc = 0.
            LOOP AT ls_alert_by_index-alerts INTO ls_alert.
              LOOP AT ls_alert-header-params INTO lv_params.
                lv_text = lv_params.
              ENDLOOP.
            ENDLOOP.
          ENDIF.

          IF lv_text IS INITIAL.
            lv_text = |<span class="boxed green-filled-set">PASSED</span>|.
          ELSE.
            lv_text = |<span class="boxed red-filled-set">{ lv_text }</span>|.
          ENDIF.

          ri_html->add( |<tr><td>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;{ ls_method-info-name }</td><td>{ lv_text }</td></tr>| ).
        ENDLOOP.
      ENDLOOP.
    ENDLOOP.

    ri_html->add( '</table></div>' ).

  ENDMETHOD.


  METHOD run.

    DATA lo_passport TYPE REF TO object.
    DATA lo_runner   TYPE REF TO cl_aucv_test_runner_abstract.
    DATA li_result   TYPE REF TO if_saunit_internal_result.
    DATA lt_keys     TYPE sabp_t_tadir_keys.
    DATA lo_result   TYPE REF TO cl_saunit_internal_result.

    lt_keys = build_tadir( ).

    CALL METHOD ('\PROGRAM=SAPLSAUCV_GUI_RUNNER\CLASS=PASSPORT')=>get
      RECEIVING
        result = lo_passport.

    lo_runner = cl_aucv_test_runner_standard=>create( lo_passport ).

    lo_runner->run_for_program_keys(
      EXPORTING
        i_limit_on_duration_category = '36' " long
        i_limit_on_risk_level        = '33' " critical
        i_program_keys               = lt_keys
      IMPORTING
        e_aunit_result               = li_result ).

    ro_result ?= li_result.

  ENDMETHOD.


  METHOD zif_abapgit_gui_event_handler~on_event.
    RETURN.
  ENDMETHOD.
ENDCLASS.
