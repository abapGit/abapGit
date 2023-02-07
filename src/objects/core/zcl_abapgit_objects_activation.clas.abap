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
        !ii_log  TYPE REF TO zif_abapgit_log
      RAISING
        zcx_abapgit_exception .
    CLASS-METHODS clear .
    CLASS-METHODS is_ddic_type
      IMPORTING
        !iv_obj_type     TYPE trobjtype
      RETURNING
        VALUE(rv_result) TYPE abap_bool .
    CLASS-METHODS is_active
      IMPORTING
        !is_item         TYPE zif_abapgit_definitions=>ty_item
      RETURNING
        VALUE(rv_active) TYPE abap_bool
      RAISING
        zcx_abapgit_exception.
  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES:
      BEGIN OF ty_classes,
        object  TYPE trobjtype,
        clsname TYPE seoclsname,
      END OF ty_classes.

    CONSTANTS:
      c_domain     TYPE c LENGTH 9  VALUE 'DOMA DOMD',
      c_types      TYPE c LENGTH 50 VALUE 'DTEL DTED TABL TABD SQLT SQLD TTYP TTYD VIEW VIED',
      c_technset   TYPE c LENGTH 24 VALUE 'TABT VIET SQTT INDX XINX',
      c_f4_objects TYPE c LENGTH 35 VALUE 'SHLP SHLD MCOB MCOD MACO MACD MCID',
      c_enqueue    TYPE c LENGTH 9  VALUE 'ENQU ENQD',
      c_sqsc       TYPE c LENGTH 4  VALUE 'SQSC',
      c_stob       TYPE c LENGTH 4  VALUE 'STOB',
      c_ntab       TYPE c LENGTH 14 VALUE 'NTTT NTTB NTDT',
      c_ddls       TYPE c LENGTH 24 VALUE 'DDLS DRUL DTDC DTEB',
      c_switches   TYPE c LENGTH 24 VALUE 'SF01 SF02 SFSW SFBS SFBF',
      c_para       TYPE c LENGTH 4  VALUE 'PARA', " can be referenced by DTEL
      c_enhd       TYPE c LENGTH 4  VALUE 'ENHD'.

    CLASS-DATA:
      gt_classes TYPE STANDARD TABLE OF ty_classes WITH DEFAULT KEY .
    CLASS-DATA:
      gt_objects TYPE TABLE OF dwinactiv .

    CLASS-METHODS update_where_used
      IMPORTING
        !ii_log TYPE REF TO zif_abapgit_log.
    CLASS-METHODS use_new_activation_logic
      RETURNING
        VALUE(rv_use_new_activation_logic) TYPE abap_bool .
    CLASS-METHODS activate_new
      IMPORTING
        !iv_ddic TYPE abap_bool DEFAULT abap_false
        !ii_log  TYPE REF TO zif_abapgit_log
      RAISING
        zcx_abapgit_exception .
    CLASS-METHODS activate_old
      IMPORTING
        !iv_ddic TYPE abap_bool DEFAULT abap_false
        !ii_log  TYPE REF TO zif_abapgit_log
      RAISING
        zcx_abapgit_exception .
    CLASS-METHODS activate_ddic
      IMPORTING
        !ii_log TYPE REF TO zif_abapgit_log
      RAISING
        zcx_abapgit_exception .
    CLASS-METHODS add_errors_and_warnings_to_log
      IMPORTING
        !iv_logname TYPE ddmass-logname
        !ii_log     TYPE REF TO zif_abapgit_log
      RAISING
        zcx_abapgit_exception .
    CLASS-METHODS add_activation_errors_to_log
      IMPORTING
        !io_checklist       TYPE REF TO cl_wb_checklist
        !ii_log             TYPE REF TO zif_abapgit_log
      RETURNING
        VALUE(rv_try_again) TYPE abap_bool
      RAISING
        zcx_abapgit_exception .
    CLASS-METHODS is_non_ddic_active
      IMPORTING
        !is_item         TYPE zif_abapgit_definitions=>ty_item
      RETURNING
        VALUE(rv_active) TYPE abap_bool
      RAISING
        zcx_abapgit_exception .
    CLASS-METHODS is_ddic_active
      IMPORTING
        !is_item         TYPE zif_abapgit_definitions=>ty_item
      RETURNING
        VALUE(rv_active) TYPE abap_bool
      RAISING
        zcx_abapgit_exception .
    CLASS-METHODS get_ddic_type
      IMPORTING
        !iv_obj_type TYPE clike
        !iv_obj_name TYPE clike
      EXPORTING
        !ev_type     TYPE ddobjtyp
        !ev_name     TYPE ddobjname
        !ev_id       TYPE ddobjectid
      RAISING
        zcx_abapgit_exception.
ENDCLASS.



CLASS zcl_abapgit_objects_activation IMPLEMENTATION.


  METHOD activate.

    " Make sure that all changes are committed since any activation error will lead to a rollback
    COMMIT WORK AND WAIT.

    IF use_new_activation_logic( ) = abap_true.
      activate_new(
        iv_ddic = iv_ddic
        ii_log  = ii_log ).
    ELSE.
      activate_old(
        iv_ddic = iv_ddic
        ii_log  = ii_log ).
    ENDIF.

    update_where_used( ii_log ).

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

      get_ddic_type(
        EXPORTING
          iv_obj_type = <ls_object>-object
          iv_obj_name = <ls_object>-obj_name
        IMPORTING
          ev_type     = ls_gentab-type
          ev_name     = ls_gentab-name
          ev_id       = ls_gentab-indx ).

      INSERT ls_gentab INTO TABLE lt_gentab.
    ENDLOOP.

    IF lt_gentab IS NOT INITIAL.

      lv_logname = |ABAPGIT_{ sy-datum }_{ sy-uzeit }|.

      IF lines( lt_gentab ) = 1.
        ii_log->add_info( |> Mass activating 1 DDIC object| ).
      ELSE.
        ii_log->add_info( |> Mass activating { lines( lt_gentab ) } DDIC objects| ).
      ENDIF.
      ii_log->add_info( |Log name: { lv_logname }| ).

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
        add_errors_and_warnings_to_log(
          iv_logname = lv_logname
          ii_log     = ii_log ).
      ENDIF.

      IF lv_rc > 4.
        zcx_abapgit_exception=>raise( 'Activation cancelled. Check the inactive objects.' ).
      ENDIF.

      " Remove objects from activation queue to avoid double activation in activate_old
      LOOP AT lt_gentab INTO ls_gentab.
        DELETE gt_objects WHERE object = ls_gentab-type AND obj_name = ls_gentab-name.
      ENDLOOP.
      DELETE gt_objects WHERE object = 'INDX' OR object = 'XINX' OR object = 'MCID'.

    ENDIF.

  ENDMETHOD.


  METHOD activate_new.

    IF gt_objects IS INITIAL.
      RETURN.
    ENDIF.

    IF iv_ddic = abap_true.

      activate_ddic( ii_log ).

    ELSE.

      activate_old( ii_log ).

    ENDIF.

  ENDMETHOD.


  METHOD activate_old.

    DATA:
      lv_popup     TYPE abap_bool,
      lv_no_ui     TYPE abap_bool,
      lv_try_again TYPE abap_bool,
      lv_msg       TYPE string,
      lo_checklist TYPE REF TO cl_wb_checklist.

    IF gt_objects IS NOT INITIAL.

      IF zcl_abapgit_ui_factory=>get_frontend_services( )->gui_is_available( ) = abap_true.
        IF zcl_abapgit_persist_factory=>get_settings( )->read( )->get_activate_wo_popup( ) = abap_true.
          lv_popup = abap_false.
        ELSE.
          lv_popup = abap_true.
        ENDIF.
      ELSE.
        lv_popup = abap_false.
      ENDIF.

      lv_no_ui = boolc( lv_popup = abap_false ).

      IF iv_ddic = abap_true.
        lv_msg = |(with DDIC)|.
      ENDIF.
      IF lines( gt_objects ) = 1.
        ii_log->add_info( |> Activating 1 object { lv_msg }| ).
      ELSE.
        ii_log->add_info( |> Activating { lines( gt_objects ) } objects { lv_msg }| ).
      ENDIF.

      TRY.
          CALL FUNCTION 'RS_WORKING_OBJECTS_ACTIVATE'
            EXPORTING
              activate_ddic_objects  = iv_ddic
              with_popup             = lv_popup
              ui_decoupled           = lv_no_ui
            IMPORTING
              p_checklist            = lo_checklist
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
            IMPORTING
              p_checklist            = lo_checklist
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
          lv_msg = 'Check the log and inactive objects.'.
          IF lv_popup = abap_false.
            lv_try_again = add_activation_errors_to_log(
              ii_log       = ii_log
              io_checklist = lo_checklist ).
            IF lv_try_again = abap_true.
              lv_msg = 'Turn on "Activation Popup" in "Personal Settings" and try again'.
            ENDIF.
          ENDIF.
          zcx_abapgit_exception=>raise( |Activation cancelled. { lv_msg }| ).
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


  METHOD add_activation_errors_to_log.

    DATA:
      ls_item    TYPE zif_abapgit_definitions=>ty_item,
      lt_message TYPE swbme_error_tab.

    FIELD-SYMBOLS:
      <lv_msg>     TYPE string,
      <ls_message> LIKE LINE OF lt_message.

    io_checklist->get_error_messages( IMPORTING p_error_tab = lt_message ).

    LOOP AT lt_message ASSIGNING <ls_message> WHERE mtype = 'E'.
      " When activting without popup, includes used in multiple main programs cause error
      " Run again WITH activation popup (see abapGit, Personal Settings)
      IF <ls_message>-message-msgid = 'EU' AND <ls_message>-message-msgno = '404'.
        rv_try_again = abap_true.
      ENDIF.
      CLEAR ls_item.
      IF strlen( <ls_message>-object_text ) > 5.
        ls_item-obj_type = <ls_message>-object_text(4).
        ls_item-obj_name = <ls_message>-object_text+5(*).
      ELSE.
        ls_item-obj_name = <ls_message>-show_req->object_name.
      ENDIF.
      LOOP AT <ls_message>-mtext ASSIGNING <lv_msg>.
        ii_log->add_error(
          iv_msg  = <lv_msg>
          is_item = ls_item ).
      ENDLOOP.
    ENDLOOP.

  ENDMETHOD.


  METHOD add_errors_and_warnings_to_log.

    DATA: lt_lines      TYPE STANDARD TABLE OF trlog,
          lv_logname_db TYPE ddprh-protname.

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

    " Only error messsages
    DELETE lt_lines WHERE severity <> 'E'
                      AND severity <> 'W'.
    " Remove "Return code..." message
    DELETE lt_lines WHERE class = 'D0' AND number = '319'.

    LOOP AT lt_lines ASSIGNING <ls_line>.
      ii_log->add( iv_msg  = <ls_line>-line
                   iv_type = <ls_line>-severity ).
    ENDLOOP.

    ii_log->add_info( |View complete activation log in program RSPUTPRT (type D, log name { iv_logname })| ).

  ENDMETHOD.


  METHOD add_item.
    add( iv_type = is_item-obj_type
         iv_name = is_item-obj_name ).
  ENDMETHOD.


  METHOD clear.
    CLEAR gt_objects.
    CLEAR gt_classes.
  ENDMETHOD.


  METHOD get_ddic_type.

    DATA lv_obj_name TYPE e071-obj_name.

    ev_type = iv_obj_type.

    IF ev_type = 'INDX' OR ev_type = 'XINX' OR ev_type = 'MCID'.
      lv_obj_name = iv_obj_name. "cast

      CALL FUNCTION 'DD_E071_TO_DD'
        EXPORTING
          object        = ev_type
          obj_name      = lv_obj_name
        IMPORTING
          name          = ev_name
          id            = ev_id
        EXCEPTIONS
          illegal_input = 1
          OTHERS        = 2.
      IF sy-subrc <> 0.
        zcx_abapgit_exception=>raise_t100( ).
      ENDIF.
    ELSE.
      ev_name = iv_obj_name.
    ENDIF.

  ENDMETHOD.


  METHOD is_active.

    " Checks if object is active or not
    "
    " Note: If object does not exist, this method returns true
    " is_not_inactive might be a better name but we avoid the double negative

    IF is_ddic_type( is_item-obj_type ) = abap_true
      AND c_para     NS is_item-obj_type
      AND c_switches NS is_item-obj_type.
      rv_active = is_ddic_active( is_item ).
    ELSE.
      rv_active = is_non_ddic_active( is_item ).
    ENDIF.

  ENDMETHOD.


  METHOD is_ddic_active.

    DATA:
      lv_type  TYPE ddobjtyp,
      lv_name  TYPE ddobjname,
      lv_id    TYPE ddobjectid,
      lv_state TYPE ddgotstate.

    get_ddic_type(
      EXPORTING
        iv_obj_type = is_item-obj_type
        iv_obj_name = is_item-obj_name
      IMPORTING
        ev_type     = lv_type
        ev_name     = lv_name
        ev_id       = lv_id ).

    " Check if an inactive version of the DDIC object exists
    " state = 'A' checks if an active version exists but does not detect new or modified objects
    " state = 'M' checks for all possible versions so we can find out if an inactive one exists
    " See documentation of the function module
    CALL FUNCTION 'DDIF_STATE_GET'
      EXPORTING
        type          = lv_type
        name          = lv_name
        id            = lv_id
        state         = 'M'
      IMPORTING
        gotstate      = lv_state
      EXCEPTIONS
        illegal_input = 1
        OTHERS        = 2.

    rv_active = boolc( sy-subrc = 0 AND ( lv_state = '' OR lv_state = 'A' ) ).

  ENDMETHOD.


  METHOD is_ddic_type.

    " Determine if object can be handled by mass activation (see RADMASUTC form ma_tab_check)

    rv_result = abap_true.

    IF c_domain   NS iv_obj_type AND c_types      NS iv_obj_type AND
       c_technset NS iv_obj_type AND c_f4_objects NS iv_obj_type AND
       c_enqueue  NS iv_obj_type AND c_sqsc       NS iv_obj_type AND
       c_stob     NS iv_obj_type AND c_ntab       NS iv_obj_type AND
       c_ddls     NS iv_obj_type AND c_para       NS iv_obj_type AND
       c_switches NS iv_obj_type AND iv_obj_type <> c_enhd.
      rv_result = abap_false.
    ENDIF.

  ENDMETHOD.


  METHOD is_non_ddic_active.

    DATA:
      lt_messages TYPE STANDARD TABLE OF sprot_u WITH DEFAULT KEY,
      ls_e071     TYPE e071,
      lt_e071     TYPE STANDARD TABLE OF e071 WITH DEFAULT KEY.

    ls_e071-object   = is_item-obj_type.
    ls_e071-obj_name = is_item-obj_name.
    INSERT ls_e071 INTO TABLE lt_e071.

    CALL FUNCTION 'RS_INACTIVE_OBJECTS_WARNING'
      EXPORTING
        suppress_protocol         = abap_false
        with_program_includes     = abap_false
        suppress_dictionary_check = abap_false
      TABLES
        p_e071                    = lt_e071
        p_xmsg                    = lt_messages.

    rv_active = boolc( lt_messages IS INITIAL ).

  ENDMETHOD.


  METHOD update_where_used.

    DATA: ls_class   LIKE LINE OF gt_classes,
          lo_cross   TYPE REF TO cl_wb_crossreference,
          ls_item    TYPE zif_abapgit_definitions=>ty_item,
          lv_msg     TYPE string,
          lv_error   TYPE c LENGTH 1,
          lv_include TYPE programm.

    LOOP AT gt_classes INTO ls_class.
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

      lo_cross->index_actualize( IMPORTING p_error = lv_error ).

      IF lv_error = abap_true.
        ls_item-obj_type = ls_class-object.
        ls_item-obj_name = ls_class-clsname.
        lv_msg = |Error updating where-used list for { ls_item-obj_type } { ls_item-obj_name }.|
          && | Check for syntax errors|.
        ii_log->add(
          iv_msg  = lv_msg
          is_item = ls_item ).
      ENDIF.
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
