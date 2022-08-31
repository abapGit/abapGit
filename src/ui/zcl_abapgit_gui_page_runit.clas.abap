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

    TYPES: BEGIN OF ty_key,
             obj_name TYPE tadir-obj_name,
             obj_type TYPE tadir-object,
           END OF ty_key.
    TYPES ty_keys_tt TYPE STANDARD TABLE OF ty_key WITH DEFAULT KEY.

    DATA mv_devclass TYPE devclass .

    METHODS build_tadir
      RETURNING
        VALUE(rt_tadir) TYPE ty_keys_tt
      RAISING
        zcx_abapgit_exception .
    METHODS run
      RETURNING
        VALUE(ro_result) TYPE REF TO object
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

    DATA lo_result         TYPE REF TO object.
    DATA lv_program_ndx    TYPE i.
    DATA lv_class_ndx      TYPE i.
    DATA lv_method_ndx     TYPE i.
    DATA lv_text           TYPE string.
    DATA lv_count          TYPE i.

    DATA ls_program        TYPE if_saunit_internal_result_type=>ty_s_program.
    DATA ls_class          LIKE LINE OF ls_program-classes.
    DATA ls_method         LIKE LINE OF ls_class-methods.

    DATA ls_alert_by_index TYPE if_saunit_internal_result_type=>ty_s_alerts_by_index.
    DATA ls_alert          LIKE LINE OF ls_alert_by_index-alerts.
    DATA lv_params         TYPE string. "LIKE LINE OF ls_alert-header-params.

    FIELD-SYMBOLS <ls_task_data>      TYPE any.
    FIELD-SYMBOLS <lt_programs>       TYPE ANY TABLE.
    FIELD-SYMBOLS <ls_alert_by_index> TYPE any.
    FIELD-SYMBOLS <lt_indices>        TYPE ANY TABLE.
    FIELD-SYMBOLS <lt_alerts>         TYPE ANY TABLE.
    FIELD-SYMBOLS <ls_alert> TYPE any.
    FIELD-SYMBOLS <lt_params> TYPE string_table.


    CREATE OBJECT ri_html TYPE zcl_abapgit_html.

    ri_html->add( '<div class="repo">' ).

    lo_result = run( ).

    ASSIGN lo_result->('F_TASK_DATA') TO <ls_task_data>.
    ASSIGN COMPONENT 'ALERTS_BY_INDICIES' OF STRUCTURE <ls_task_data> TO <lt_indices>.
    ASSIGN COMPONENT 'PROGRAMS' OF STRUCTURE <ls_task_data> TO <lt_programs>.

    LOOP AT <lt_indices> ASSIGNING <ls_alert_by_index>.
      ASSIGN COMPONENT 'ALERTS' OF STRUCTURE <ls_alert_by_index> TO <lt_alerts>.
      LOOP AT <lt_alerts> ASSIGNING <ls_alert> WHERE ('KIND = ''F'' OR KIND = ''S''').  " check level=F(ail?) instead?
        ASSIGN COMPONENT 'HEADER-PARAMS' OF STRUCTURE <ls_alert> TO <lt_params>.
        LOOP AT <lt_params> INTO lv_params.
          lv_text = lv_params.
        ENDLOOP.
        ri_html->add( |<span class="boxed red-filled-set">{ lv_text }</span><br>| ).
        lv_count = lv_count + 1.
      ENDLOOP.
    ENDLOOP.

    ri_html->add( |<b>{ lv_count } Errors</b><br>| ).

    ri_html->add( |<hr><table>| ).

    LOOP AT <lt_programs> INTO ls_program.
      lv_program_ndx = sy-tabix.
      ri_html->add( |<tr><td>{ ls_program-info-key-obj_type } {
        ls_program-info-key-obj_name }</td><td></td></tr>| ).
      LOOP AT ls_program-classes INTO ls_class.
        lv_class_ndx = sy-tabix.

        ri_html->add( |<tr><td>&nbsp;&nbsp;&nbsp;&nbsp;{ ls_class-info-name }</td><td></td></tr>| ).
        LOOP AT ls_class-methods INTO ls_method.
          lv_method_ndx = sy-tabix.

          CLEAR lv_text.
          READ TABLE <lt_indices> WITH KEY
            ('PROGRAM_NDX') = lv_program_ndx
            ('CLASS_NDX') = lv_class_ndx
            ('METHOD_NDX') = lv_method_ndx
            ASSIGNING <ls_alert_by_index>.
          IF sy-subrc = 0.
            ASSIGN COMPONENT 'ALERTS' OF STRUCTURE <ls_alert_by_index> TO <lt_alerts>.
            LOOP AT <lt_alerts> ASSIGNING <ls_alert>.
              ASSIGN COMPONENT 'HEADER-PARAMS' OF STRUCTURE <ls_alert> TO <lt_params>.
              LOOP AT <lt_params> INTO lv_params.
                lv_text = lv_params.
              ENDLOOP.
            ENDLOOP.
          ENDIF.

          IF lv_text IS INITIAL.
            lv_text = |<span class="boxed green-filled-set">PASSED</span>|.
          ELSE.
            lv_text = |<span class="boxed red-filled-set">{ lv_text }</span>|.
          ENDIF.

          ri_html->add( |<tr><td>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;{
            ls_method-info-name }</td><td>{ lv_text }</td></tr>| ).
        ENDLOOP.
      ENDLOOP.
    ENDLOOP.

    ri_html->add( '</table></div>' ).

  ENDMETHOD.


  METHOD run.

    DATA lo_passport TYPE REF TO object.
    DATA lo_runner   TYPE REF TO object.
    DATA lt_keys     TYPE ty_keys_tt.
    DATA li_result TYPE REF TO data.
    FIELD-SYMBOLS <li_result> TYPE any.

    lt_keys = build_tadir( ).

    CALL METHOD ('\PROGRAM=SAPLSAUCV_GUI_RUNNER\CLASS=PASSPORT')=>get
      RECEIVING
        result = lo_passport.

    lo_runner = cl_aucv_test_runner_standard=>create( lo_passport ).

    CREATE DATA li_result TYPE REF TO ('IF_SAUNIT_INTERNAL_RESULT').
    ASSIGN li_result->* TO <li_result>.

    CALL METHOD lo_runner->('RUN_FOR_PROGRAM_KEYS')
      EXPORTING
        i_limit_on_duration_category = '36' " long
        i_limit_on_risk_level        = '33' " critical
        i_program_keys               = lt_keys
      IMPORTING
        e_aunit_result               = <li_result>.

    ro_result = <li_result>.

  ENDMETHOD.


  METHOD zif_abapgit_gui_event_handler~on_event.
    RETURN.
  ENDMETHOD.
ENDCLASS.
