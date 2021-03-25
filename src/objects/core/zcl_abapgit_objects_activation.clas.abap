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

    TYPES:
      BEGIN OF ty_classes,
        object  TYPE trobjtype,
        clsname TYPE seoclsname,
      END OF ty_classes.

    CLASS-DATA:
      gt_classes TYPE STANDARD TABLE OF ty_classes WITH DEFAULT KEY .
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
    CLASS-METHODS is_ddic_type
      IMPORTING
        !iv_obj_type     TYPE trobjtype
      RETURNING
        VALUE(rv_result) TYPE abap_bool .
ENDCLASS.



CLASS zcl_abapgit_objects_activation IMPLEMENTATION.


  METHOD activate.

    " Make sure that all changes are committed since any activation error will lead to a rollback
    COMMIT WORK AND WAIT.

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
      " Filter types supported by mass activation
      IF is_ddic_type( <ls_object>-object ) = abap_false.
        CONTINUE.
      ENDIF.
      ls_gentab-tabix = sy-tabix.
      ls_gentab-type = <ls_object>-object.
      ls_gentab-name = <ls_object>-obj_name.
      IF ls_gentab-type = 'INDX' OR ls_gentab-type = 'XINX' OR ls_gentab-type = 'MCID'.
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
          ddmode         = 'O'         " activate changes in Original System
          frcact         = abap_true   " force Activation
          medium         = 'T'         " transport order
          device         = 'T'         " saves to table DDRPH?
          version        = 'M'         " activate newest version
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
        zcx_abapgit_exception=>raise_t100( ).
      ENDIF.

      IF lv_rc > 0.
        show_activation_errors( lv_logname ).
      ENDIF.

      " Remove objects from activation queue to avoid double activation in activate_old
      LOOP AT lt_gentab INTO ls_gentab.
        DELETE gt_objects WHERE object = ls_gentab-type AND obj_name = ls_gentab-name.
      ENDLOOP.
      DELETE gt_objects WHERE object = 'INDX' OR object = 'XINX' OR object = 'MCID'.

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

    FIELD-SYMBOLS: <ls_object>  TYPE dwinactiv,
                   <ls_classes> LIKE LINE OF gt_classes.

    IF iv_type = 'CLAS' OR iv_type = 'INTF'.
      APPEND INITIAL LINE TO gt_classes ASSIGNING <ls_classes>.
      <ls_classes>-object  = iv_type.
      <ls_classes>-clsname = iv_name.
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


  METHOD is_ddic_type.

    " Determine if object can be handled by mass activation (see RADMASUTC form ma_tab_check)

    CONSTANTS:
      lc_domain     TYPE c LENGTH 9  VALUE 'DOMA DOMD',
      lc_types      TYPE c LENGTH 50 VALUE 'DTEL DTED TABL TABD SQLT SQLD TTYP TTYD VIEW VIED',
      lc_technset   TYPE c LENGTH 24 VALUE 'TABT VIET SQTT INDX XINX',
      lc_f4_objects TYPE c LENGTH 35 VALUE 'SHLP SHLD MCOB MCOD MACO MACD MCID',
      lc_enqueue    TYPE c LENGTH 9  VALUE 'ENQU ENQD',
      lc_sqsc       TYPE c LENGTH 4  VALUE 'SQSC',
      lc_stob       TYPE c LENGTH 4  VALUE 'STOB',
      lc_ntab       TYPE c LENGTH 14 VALUE 'NTTT NTTB NTDT',
      lc_ddls       TYPE c LENGTH 4  VALUE 'DDLS',
      lc_switches   TYPE c LENGTH 24 VALUE 'SF01 SF02 SFSW SFBS SFBF',
      lc_enhd       TYPE c LENGTH 4  VALUE 'ENHD'.

    rv_result = abap_true.
    IF lc_domain   NS iv_obj_type AND lc_types      NS iv_obj_type AND
       lc_technset NS iv_obj_type AND lc_f4_objects NS iv_obj_type AND
       lc_enqueue  NS iv_obj_type AND lc_sqsc       NS iv_obj_type AND
       lc_stob     NS iv_obj_type AND lc_ntab       NS iv_obj_type AND
       lc_ddls     NS iv_obj_type AND
       lc_switches NS iv_obj_type AND iv_obj_type <> lc_enhd.
      rv_result = abap_false.
    ENDIF.

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
      zcx_abapgit_exception=>raise_t100( ).
    ENDIF.

    DELETE lt_lines WHERE severity <> 'E'.

    CREATE OBJECT li_log TYPE zcl_abapgit_log.
    li_log->set_title( 'Activation Errors' ).

    LOOP AT lt_lines ASSIGNING <ls_line>.
      li_log->add( <ls_line>-line ).
    ENDLOOP.

    IF li_log->count( ) > 0.
      zcl_abapgit_log_viewer=>show_log( li_log ).
    ENDIF.

  ENDMETHOD.


  METHOD update_where_used.

    DATA: ls_class    LIKE LINE OF gt_classes,
          lo_cross    TYPE REF TO cl_wb_crossreference,
          lv_include  TYPE programm,
          li_progress TYPE REF TO zif_abapgit_progress.


    li_progress = zcl_abapgit_progress=>get_instance( lines( gt_classes ) ).

    LOOP AT gt_classes INTO ls_class.
      IF sy-tabix MOD 20 = 0.
        li_progress->show(
          iv_current = sy-tabix
          iv_text    = 'Updating where-used lists' ).
      ENDIF.

      CASE ls_class-object.
        WHEN 'CLAS'.
          lv_include = cl_oo_classname_service=>get_classpool_name( ls_class-clsname ).
        WHEN 'INTF'.
          lv_include = cl_oo_classname_service=>get_interfacepool_name( ls_class-clsname ).
      ENDCASE.

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
