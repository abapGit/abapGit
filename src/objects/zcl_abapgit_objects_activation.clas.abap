CLASS zcl_abapgit_objects_activation DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    CLASS-METHODS add
      IMPORTING
        !iv_type   TYPE trobjtype
        !iv_name   TYPE clike
        !iv_delete TYPE abap_bool DEFAULT abap_false
      RAISING
        zcx_abapgit_exception .
    CLASS-METHODS add_item
      IMPORTING
        !is_item TYPE zif_abapgit_definitions=>ty_item
      RAISING
        zcx_abapgit_exception .
    CLASS-METHODS activate
      IMPORTING
        !iv_ddic TYPE abap_bool DEFAULT abap_false
      RAISING
        zcx_abapgit_exception .
    CLASS-METHODS clear .
  PROTECTED SECTION.
  PRIVATE SECTION.

    CLASS-DATA:
      gt_classes TYPE STANDARD TABLE OF seoclsname WITH DEFAULT KEY .
    CLASS-DATA:
      gt_objects TYPE TABLE OF dwinactiv .

    CLASS-METHODS update_where_used .
    CLASS-METHODS use_new_activation_logic
      RETURNING
        VALUE(rv_use_new_activation_logic) TYPE abap_bool .
    CLASS-METHODS activate_new
      IMPORTING
        !iv_ddic TYPE abap_bool DEFAULT abap_false
      RAISING
        zcx_abapgit_exception .
    CLASS-METHODS activate_old
      IMPORTING
        !iv_ddic TYPE abap_bool DEFAULT abap_false
      RAISING
        zcx_abapgit_exception .
    CLASS-METHODS activate_ddic
      RAISING
        zcx_abapgit_exception .
    CLASS-METHODS show_activation_errors
      IMPORTING
        !iv_logname TYPE ddmass-logname
      RAISING
        zcx_abapgit_exception .
ENDCLASS.



CLASS ZCL_ABAPGIT_OBJECTS_ACTIVATION IMPLEMENTATION.


  METHOD activate.

    IF use_new_activation_logic( ) = abap_true.
      activate_new( iv_ddic ).
    ELSE.
      activate_old( iv_ddic ).
    ENDIF.

    update_where_used( ).

  ENDMETHOD.


  METHOD activate_ddic.

    DATA: lt_gentab     TYPE STANDARD TABLE OF dcgentb,
          lv_rc         TYPE sy-subrc,
          ls_gentab     LIKE LINE OF lt_gentab,
          lt_deltab     TYPE STANDARD TABLE OF dcdeltb,
          lt_action_tab TYPE STANDARD TABLE OF dctablres,
          lv_logname    TYPE ddmass-logname.

    FIELD-SYMBOLS: <ls_object> LIKE LINE OF gt_objects.


    LOOP AT gt_objects ASSIGNING <ls_object>.
      ls_gentab-tabix = sy-tabix.
      ls_gentab-type = <ls_object>-object.
      ls_gentab-name = <ls_object>-obj_name.
      IF ls_gentab-type = 'INDX'.
        CALL FUNCTION 'DD_E071_TO_DD'
          EXPORTING
            object   = <ls_object>-object
            obj_name = <ls_object>-obj_name
          IMPORTING
            name     = ls_gentab-name
            id       = ls_gentab-indx.
      ENDIF.
      INSERT ls_gentab INTO TABLE lt_gentab.
    ENDLOOP.

    IF lt_gentab IS NOT INITIAL.

      lv_logname = |ABAPGIT_{ sy-datum }_{ sy-uzeit }|.

      CALL FUNCTION 'DD_MASS_ACT_C3'
        EXPORTING
          ddmode         = 'O'
          medium         = 'T' " transport order
          device         = 'T' " saves to table DDRPH?
          version        = 'M' " activate newest
          logname        = lv_logname
          write_log      = abap_true
          log_head_tail  = abap_true
          t_on           = space
          prid           = 1
        IMPORTING
          act_rc         = lv_rc
        TABLES
          gentab         = lt_gentab
          deltab         = lt_deltab
          cnvtab         = lt_action_tab
        EXCEPTIONS
          access_failure = 1
          no_objects     = 2
          locked         = 3
          internal_error = 4
          OTHERS         = 5.

      IF sy-subrc <> 0.
        zcx_abapgit_exception=>raise( 'error from DD_MASS_ACT_C3' ).
      ENDIF.

      IF lv_rc > 0.
        show_activation_errors( lv_logname ).
      ENDIF.

    ENDIF.

  ENDMETHOD.


  METHOD activate_new.

    DATA: li_progress TYPE REF TO zif_abapgit_progress.

    IF gt_objects IS INITIAL.
      RETURN.
    ENDIF.

    li_progress = zcl_abapgit_progress=>get_instance( 100 ).

    IF iv_ddic = abap_true.

      li_progress->show( iv_current = 98
                         iv_text    = 'Activating DDIC' ).

      activate_ddic( ).

    ELSE.

      li_progress->show( iv_current = 98
                         iv_text    = 'Activating non DDIC' ).

      activate_old( ).

    ENDIF.

  ENDMETHOD.


  METHOD activate_old.

    DATA: lv_popup TYPE abap_bool,
          lv_no_ui TYPE abap_bool.

    IF gt_objects IS NOT INITIAL.

      IF zcl_abapgit_ui_factory=>get_gui_functions( )->gui_is_available( ) = abap_true.
        IF zcl_abapgit_persist_settings=>get_instance( )->read( )->get_activate_wo_popup( ) = abap_true.
          lv_popup = abap_false.
        ELSE.
          lv_popup = abap_true.
        ENDIF.
      ELSE.
        lv_popup = abap_false.
      ENDIF.

      lv_no_ui = boolc( lv_popup = abap_false ).

      TRY.
          CALL FUNCTION 'RS_WORKING_OBJECTS_ACTIVATE'
            EXPORTING
              activate_ddic_objects  = iv_ddic
              with_popup             = lv_popup
              ui_decoupled           = lv_no_ui
            TABLES
              objects                = gt_objects
            EXCEPTIONS
              excecution_error       = 1
              cancelled              = 2
              insert_into_corr_error = 3
              OTHERS                 = 4 ##SUBRC_OK.
        CATCH cx_sy_dyn_call_param_not_found.
          CALL FUNCTION 'RS_WORKING_OBJECTS_ACTIVATE'
            EXPORTING
              activate_ddic_objects  = iv_ddic
              with_popup             = lv_popup
            TABLES
              objects                = gt_objects
            EXCEPTIONS
              excecution_error       = 1
              cancelled              = 2
              insert_into_corr_error = 3
              OTHERS                 = 4 ##SUBRC_OK.
      ENDTRY.
      CASE sy-subrc.
        WHEN 1 OR 3 OR 4.
          zcx_abapgit_exception=>raise_t100( ).
        WHEN 2.
          zcx_abapgit_exception=>raise( 'Activation cancelled. Check the inactive objects.' ).
      ENDCASE.

    ENDIF.

  ENDMETHOD.


  METHOD add.

* function group SEWORKINGAREA
* function module RS_INSERT_INTO_WORKING_AREA
* class CL_WB_ACTIVATION_WORK_AREA

    FIELD-SYMBOLS: <ls_object> TYPE dwinactiv.

    IF iv_type = 'CLAS'.
      APPEND iv_name TO gt_classes.
    ELSE.
      APPEND INITIAL LINE TO gt_objects ASSIGNING <ls_object>.
      <ls_object>-object     = iv_type.
      <ls_object>-obj_name   = iv_name.
      <ls_object>-delet_flag = iv_delete.
    ENDIF.

  ENDMETHOD.


  METHOD add_item.
    add( iv_type = is_item-obj_type
         iv_name = is_item-obj_name ).
  ENDMETHOD.


  METHOD clear.
    CLEAR gt_objects.
    CLEAR gt_classes.
  ENDMETHOD.


  METHOD show_activation_errors.

    DATA: lt_lines      TYPE STANDARD TABLE OF trlog,
          lv_logname_db TYPE ddprh-protname,
          li_log        TYPE REF TO zif_abapgit_log.

    FIELD-SYMBOLS: <ls_line> LIKE LINE OF lt_lines.


    lv_logname_db = iv_logname.

    CALL FUNCTION 'TR_READ_LOG'
      EXPORTING
        iv_log_type   = 'DB'
        iv_logname_db = lv_logname_db
      TABLES
        et_lines      = lt_lines
      EXCEPTIONS
        invalid_input = 1
        access_error  = 2
        OTHERS        = 3.

    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( 'error from TR_READ_LOG' ).
    ENDIF.

    DELETE lt_lines WHERE severity <> 'E'.

    CREATE OBJECT li_log TYPE zcl_abapgit_log.

    LOOP AT lt_lines ASSIGNING <ls_line>.
      li_log->add( <ls_line>-line ).
    ENDLOOP.

    IF li_log->count( ) > 0.
      zcl_abapgit_log_viewer=>show_log( iv_header_text = 'Activation Errors'
                                        ii_log         = li_log ).
    ENDIF.

  ENDMETHOD.


  METHOD update_where_used.

    DATA: lv_class    LIKE LINE OF gt_classes,
          lo_cross    TYPE REF TO cl_wb_crossreference,
          lv_include  TYPE programm,
          li_progress TYPE REF TO zif_abapgit_progress.


    li_progress = zcl_abapgit_progress=>get_instance( lines( gt_classes ) ).

    LOOP AT gt_classes INTO lv_class.
      IF sy-tabix MOD 20 = 0.
        li_progress->show(
          iv_current = sy-tabix
          iv_text    = 'Updating where-used lists' ).
      ENDIF.

      lv_include = cl_oo_classname_service=>get_classpool_name( lv_class ).

      CREATE OBJECT lo_cross
        EXPORTING
          p_name    = lv_include
          p_include = lv_include.

      lo_cross->index_actualize( ).
    ENDLOOP.

  ENDMETHOD.


  METHOD use_new_activation_logic.

    CALL FUNCTION 'FUNCTION_EXISTS'
      EXPORTING
        funcname           = 'DD_MASS_ACT_C3'
      EXCEPTIONS
        function_not_exist = 1
        OTHERS             = 2.
    IF sy-subrc = 0.
      rv_use_new_activation_logic = abap_true.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
