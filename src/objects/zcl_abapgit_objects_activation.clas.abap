CLASS zcl_abapgit_objects_activation DEFINITION PUBLIC CREATE PUBLIC.

  PUBLIC SECTION.
    CLASS-METHODS add
      IMPORTING iv_type   TYPE trobjtype
                iv_name   TYPE clike
                iv_delete TYPE abap_bool DEFAULT abap_false
      RAISING   zcx_abapgit_exception.

    CLASS-METHODS add_item
      IMPORTING is_item TYPE zif_abapgit_definitions=>ty_item
      RAISING   zcx_abapgit_exception.

    CLASS-METHODS activate
      IMPORTING iv_ddic TYPE abap_bool DEFAULT abap_false
      RAISING   zcx_abapgit_exception.

    CLASS-METHODS clear.

  PRIVATE SECTION.

    CLASS-DATA:
      gt_classes TYPE STANDARD TABLE OF seoclsname WITH DEFAULT KEY,
      gt_objects TYPE TABLE OF dwinactiv.

    CLASS-METHODS update_where_used .
    CLASS-METHODS fix_class_methods
      IMPORTING
        !iv_obj_name TYPE trobj_name
      CHANGING
        !ct_objects  TYPE dwinactiv_tab .
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
          ls_gentab     LIKE LINE OF lt_gentab,
          lv_rc         TYPE sy-subrc,
          lt_deltab     TYPE STANDARD TABLE OF dcdeltb,
          lt_action_tab TYPE STANDARD TABLE OF dctablres,
          lv_logname    TYPE ddmass-logname,
          lv_errmsg(255) TYPE c.

    FIELD-SYMBOLS: <ls_object> LIKE LINE OF gt_objects.

    LOOP AT gt_objects ASSIGNING <ls_object>.

      ls_gentab-name = <ls_object>-obj_name.
      ls_gentab-type = <ls_object>-object.
      INSERT ls_gentab INTO TABLE lt_gentab.

      CALL FUNCTION 'RS_CORR_INSERT'
        EXPORTING
          object              = <ls_object>-obj_name
          object_class        = <ls_object>-object
          global_lock         = abap_true
        EXCEPTIONS
          cancelled           = 1
          permission_failure  = 2
          unknown_objectclass = 3
          OTHERS              = 4.

      IF sy-subrc <> 0.
        CONCATENATE 'error from RS_CORR_INSERT for' <ls_object>-object <ls_object>-obj_name
            INTO lv_errmsg SEPARATED BY space.

        zcx_abapgit_exception=>raise( lv_errmsg ).
      ENDIF.

    ENDLOOP.

    IF lt_gentab IS NOT INITIAL.

      lv_logname = |ABAPGIT_{ sy-datum }_{ sy-uzeit }|.

      CALL FUNCTION 'DD_MASS_ACT_C3'
        EXPORTING
          ddmode         = 'C'
          medium         = 'T'
          device         = 'T'
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

    DATA: lo_progress TYPE REF TO zcl_abapgit_progress.

    IF gt_objects IS INITIAL.
      RETURN.
    ENDIF.

    CREATE OBJECT lo_progress
      EXPORTING
        iv_total = 100.

    IF iv_ddic = abap_true.

      lo_progress->show( iv_current = 98
                         iv_text    = 'Activating DDIC' ).

      activate_ddic( ).

    ELSE.

      lo_progress->show( iv_current = 98
                         iv_text    = 'Activating non DDIC' ).

      activate_old( ).

    ENDIF.

  ENDMETHOD.


  METHOD activate_old.

    DATA: lv_popup TYPE abap_bool.

    IF gt_objects IS NOT INITIAL.

      CALL FUNCTION 'GUI_IS_AVAILABLE'
        IMPORTING
          return = lv_popup.

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
          OTHERS                 = 4.
      IF sy-subrc <> 0.
        zcx_abapgit_exception=>raise( 'error from RS_WORKING_OBJECTS_ACTIVATE' ).
      ENDIF.

    ENDIF.

  ENDMETHOD.


  METHOD add.

* function group SEWORKINGAREA
* function module RS_INSERT_INTO_WORKING_AREA
* class CL_WB_ACTIVATION_WORK_AREA

    DATA: lt_objects  TYPE dwinactiv_tab,
          lv_obj_name TYPE dwinactiv-obj_name.

    FIELD-SYMBOLS: <ls_object> LIKE LINE OF lt_objects.


    lv_obj_name = iv_name.

    CASE iv_type.
      WHEN 'CLAS'.
        APPEND iv_name TO gt_classes.
      WHEN 'WDYN'.
* todo, move this to the object type include instead
        CALL FUNCTION 'RS_INACTIVE_OBJECTS_IN_OBJECT'
          EXPORTING
            obj_name         = lv_obj_name
            object           = iv_type
          TABLES
            inactive_objects = lt_objects
          EXCEPTIONS
            object_not_found = 1
            OTHERS           = 2.
        IF sy-subrc <> 0.
          zcx_abapgit_exception=>raise( 'Error from RS_INACTIVE_OBJECTS_IN_OBJECT' ).
        ENDIF.

*        IF iv_type = 'CLAS'.
*          fix_class_methods( EXPORTING iv_obj_name = lv_obj_name
*                             CHANGING ct_objects = lt_objects ).
*        ENDIF.

        LOOP AT lt_objects ASSIGNING <ls_object>.
          <ls_object>-delet_flag = iv_delete.
        ENDLOOP.

        APPEND LINES OF lt_objects TO gt_objects.
      WHEN OTHERS.
        APPEND INITIAL LINE TO gt_objects ASSIGNING <ls_object>.
        <ls_object>-object     = iv_type.
        <ls_object>-obj_name   = lv_obj_name.
        <ls_object>-delet_flag = iv_delete.
    ENDCASE.

  ENDMETHOD.


  METHOD add_item.
    add( iv_type = is_item-obj_type
         iv_name = is_item-obj_name ).
  ENDMETHOD.


  METHOD clear.
    CLEAR gt_objects.
    CLEAR gt_classes.
  ENDMETHOD.


  METHOD fix_class_methods.
* function module RS_WORKING_OBJECTS_ACTIVATE assumes that
* METH lines contains spaces between class and method name
* however, classes named with 30 characters
* eg. ZCL_CLAS_TESTTESTTESTTESTTESTT
* this will not be true, so find all the method includes instead

* TODO, this class is obsolete with new CLAS deserialization logic

    DATA: lt_methods TYPE seop_methods_w_include,
          lv_class   TYPE seoclsname.

    FIELD-SYMBOLS: <ls_method> LIKE LINE OF lt_methods,
                   <ls_object> LIKE LINE OF ct_objects.


    lv_class = iv_obj_name.

    cl_oo_classname_service=>get_all_method_includes(
      EXPORTING
        clsname            = lv_class
      RECEIVING
        result             = lt_methods
      EXCEPTIONS
        class_not_existing = 1
        OTHERS             = 2 ).
    ASSERT sy-subrc = 0.
    DELETE ct_objects WHERE object = 'METH'.
    LOOP AT lt_methods ASSIGNING <ls_method>.
      APPEND INITIAL LINE TO ct_objects ASSIGNING <ls_object>.
      <ls_object>-object = 'METH'.
      <ls_object>-obj_name = <ls_method>-incname.
    ENDLOOP.

  ENDMETHOD.


  METHOD show_activation_errors.

    DATA: lt_lines      TYPE STANDARD TABLE OF trlog,
          lv_logname_db TYPE ddprh-protname,
          lo_log        TYPE REF TO zcl_abapgit_log.

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

    CREATE OBJECT lo_log.

    LOOP AT lt_lines ASSIGNING <ls_line>.
      lo_log->add( <ls_line>-line ).
    ENDLOOP.

    lo_log->show( ).

  ENDMETHOD.


  METHOD update_where_used.

    DATA: lv_class    LIKE LINE OF gt_classes,
          lo_cross    TYPE REF TO cl_wb_crossreference,
          lv_include  TYPE programm,
          lo_progress TYPE REF TO zcl_abapgit_progress.


    CREATE OBJECT lo_progress
      EXPORTING
        iv_total = lines( gt_classes ).

    LOOP AT gt_classes INTO lv_class.
      IF sy-tabix MOD 20 = 0.
        lo_progress->show(
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

    IF zcl_abapgit_persist_settings=>get_instance( )->read( )->get_experimental_features( ) = abap_true.

      CALL FUNCTION 'FUNCTION_EXISTS'
        EXPORTING
          funcname           = 'DD_MASS_ACT_C3'    " Name of Function Module
        EXCEPTIONS
          function_not_exist = 1
          OTHERS             = 2.

      IF sy-subrc = 0.
        rv_use_new_activation_logic = abap_true.
      ENDIF.

    ENDIF.

  ENDMETHOD.
ENDCLASS.
