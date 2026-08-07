CLASS zcl_abapgit_object_acgr DEFINITION
  PUBLIC
  INHERITING FROM zcl_abapgit_objects_super
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zif_abapgit_object.

  PROTECTED SECTION.
    METHODS get_generic
      RETURNING
        VALUE(ro_generic) TYPE REF TO zcl_abapgit_objects_generic
      RAISING
        zcx_abapgit_exception.

  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_abapgit_object_acgr IMPLEMENTATION.
  METHOD get_generic.
    CREATE OBJECT ro_generic
      EXPORTING
        is_item     = ms_item
        iv_language = mv_language.
  ENDMETHOD.


  METHOD zif_abapgit_object~changed_by.
    SELECT SINGLE change_usr
      FROM agr_define
      INTO rv_user
      WHERE agr_name EQ ms_item-obj_name.
    IF sy-subrc NE 0 OR rv_user IS INITIAL.
      rv_user = c_user_unknown.
    ENDIF.
  ENDMETHOD.


  METHOD zif_abapgit_object~delete.
    DATA: lv_activity_group TYPE agr_name,
          lv_error_flag     TYPE char01,
          lv_text           TYPE string,
          lv_message        TYPE string,
          lt_messages       TYPE STANDARD TABLE OF sprot_u.

    FIELD-SYMBOLS <ls_message> TYPE sprot_u.

    lv_activity_group = ms_item-obj_name.
    CALL FUNCTION 'PRGN_ACTIVITY_GROUP_DELETE'
      EXPORTING
        activity_group                = lv_activity_group
        enqueue_and_transport         = 'X'
        show_dialog                   = space
        request                       = iv_transport
      IMPORTING
        error_flag                    = lv_error_flag
      TABLES
        messages                      = lt_messages
      EXCEPTIONS
        not_authorized                = 1
        transport_check_problem       = 2
        transport_canceled_or_problem = 3
        one_or_more_users_enqueued    = 4
        foreign_lock                  = 5
        user_cancels_action           = 6
        child_agr_exists              = 7
        deletion_in_target_cancelled  = 8
        tech_error                    = 9
        hr_error                      = 10
        OTHERS                        = 11.
    IF sy-subrc NE 0 OR lv_error_flag NE space.
      LOOP AT lt_messages ASSIGNING <ls_message> WHERE severity EQ 'E'.
        MESSAGE ID <ls_message>-ag TYPE 'E' NUMBER <ls_message>-msgnr
                WITH <ls_message>-var1 <ls_message>-var2
                     <ls_message>-var3 <ls_message>-var4
                INTO lv_text.
        IF lv_message IS INITIAL.
          lv_message = lv_text.
        ELSE.
          lv_message = |{ lv_message }; { lv_text }|.
        ENDIF.
      ENDLOOP.
      IF lv_message IS INITIAL.
        lv_message = |subrc { sy-subrc }, error_flag { lv_error_flag }|.
      ENDIF.
      zcx_abapgit_exception=>raise( |Error deleting role { ms_item-obj_name }: { lv_message }| ).
    ENDIF.
  ENDMETHOD.


  METHOD zif_abapgit_object~deserialize.
    DATA: lt_tables TYPE STANDARD TABLE OF objsl-tobj_name,
          lv_table  TYPE objsl-tobj_name,
          lv_role   TYPE agr_name.

    lv_role = ms_item-obj_name.

    "The generic delete() constrains language-dependent tables (AGR_TEXTS,
    "AGR_HIERT, AGR_MINIT, AGR_TCDTXT) to the login language, but serialize
    "exports all languages. Pre-clear every role table before the generic
    "import runs to avoid leftover rows collide on INSERT.
    SELECT DISTINCT tobj_name
      FROM objsl
      INTO TABLE lt_tables
      WHERE objectname EQ ms_item-obj_type
        AND objecttype EQ 'L'
        AND tobject    EQ 'TABU'
      ORDER BY tobj_name.
    LOOP AT lt_tables INTO lv_table.
      DELETE FROM (lv_table) WHERE agr_name EQ lv_role.
    ENDLOOP.

    get_generic( )->deserialize( iv_package   = iv_package
                                 io_xml       = io_xml
                                 iv_transport = iv_transport ).
  ENDMETHOD.


  METHOD zif_abapgit_object~exists.
    rv_bool = get_generic( )->exists( ).
  ENDMETHOD.


  METHOD zif_abapgit_object~get_comparator.
    RETURN.
  ENDMETHOD.


  METHOD zif_abapgit_object~get_deserialize_order.
    RETURN.
  ENDMETHOD.


  METHOD zif_abapgit_object~get_deserialize_steps.
    APPEND zif_abapgit_object=>gc_step_id-abap TO rt_steps.
  ENDMETHOD.


  METHOD zif_abapgit_object~get_metadata.
    rs_metadata = get_metadata( ).
  ENDMETHOD.


  METHOD zif_abapgit_object~is_active.
    rv_active = is_active( ).
  ENDMETHOD.


  METHOD zif_abapgit_object~is_locked.
    rv_is_locked = exists_a_lock_entry_for( iv_lock_object = 'E_AGR_DEF'
                                            iv_argument    = |{ ms_item-obj_name }|
                                            iv_prefix      = |{ sy-mandt }| ).
  ENDMETHOD.


  METHOD zif_abapgit_object~jump.
    DATA lv_activity_group TYPE agr_name.

    lv_activity_group = ms_item-obj_name.

    CALL FUNCTION 'PRGN_SHOW_EDIT_AGR'
      EXPORTING
        agr_name      = lv_activity_group
        mode          = 'A'
      EXCEPTIONS
        agr_not_found = 1
        no_auth       = 2
        OTHERS        = 3.
    IF sy-subrc NE 0.
      zcx_abapgit_exception=>raise( |Cannot open role { ms_item-obj_name } in PFCG| ).
    ENDIF.

    rv_exit = abap_true.
  ENDMETHOD.


  METHOD zif_abapgit_object~map_filename_to_object.
    RETURN.
  ENDMETHOD.


  METHOD zif_abapgit_object~map_object_to_filename.
    RETURN.
  ENDMETHOD.


  METHOD zif_abapgit_object~serialize.
    DATA: lt_tables       TYPE STANDARD TABLE OF objsl-tobj_name,
          lt_audit_fields TYPE STANDARD TABLE OF fieldname,
          lv_table        TYPE objsl-tobj_name,
          lv_field        TYPE fieldname,
          lv_role         TYPE agr_name,
          lr_data         TYPE REF TO data.

    FIELD-SYMBOLS: <lt_data> TYPE STANDARD TABLE,
                   <ls_row>  TYPE any,
                   <lv_val>  TYPE any.

    lv_role = ms_item-obj_name.

    APPEND 'MANDT'      TO lt_audit_fields.
    APPEND 'CREATE_USR' TO lt_audit_fields.
    APPEND 'CREATE_DAT' TO lt_audit_fields.
    APPEND 'CREATE_TIM' TO lt_audit_fields.
    APPEND 'CREATE_TMP' TO lt_audit_fields.
    APPEND 'CHANGE_USR' TO lt_audit_fields.
    APPEND 'CHANGE_DAT' TO lt_audit_fields.
    APPEND 'CHANGE_TIM' TO lt_audit_fields.
    APPEND 'CHANGE_TMP' TO lt_audit_fields.

    "Read tables directly instead of the generic serializer: its BEFORE_EXP( )
    "calls FM PRGN_BEFORE_EXP_ACTGROUP_ACGR that prompts for a transport request and aborts.
    SELECT DISTINCT tobj_name
      FROM objsl
      INTO TABLE lt_tables
      WHERE objectname EQ ms_item-obj_type
        AND objecttype EQ 'L'
        AND tobject    EQ 'TABU'
      ORDER BY tobj_name.
    LOOP AT lt_tables INTO lv_table.
      CREATE DATA lr_data TYPE STANDARD TABLE OF (lv_table).
      ASSIGN lr_data->* TO <lt_data>.

      SELECT * FROM (lv_table)
        INTO TABLE <lt_data>
        WHERE agr_name EQ lv_role
        ORDER BY PRIMARY KEY.

      LOOP AT <lt_data> ASSIGNING <ls_row>.
        LOOP AT lt_audit_fields INTO lv_field.
          ASSIGN COMPONENT lv_field OF STRUCTURE <ls_row> TO <lv_val>.
          IF sy-subrc EQ 0.
            CLEAR <lv_val>.
          ENDIF.
        ENDLOOP.
      ENDLOOP.

      io_xml->add( iv_name = lv_table
                   ig_data = <lt_data> ).
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.
