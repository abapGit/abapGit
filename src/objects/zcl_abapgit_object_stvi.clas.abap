CLASS zcl_abapgit_object_stvi DEFINITION
  PUBLIC
  INHERITING FROM zcl_abapgit_objects_super
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_abapgit_object .
  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES:
      BEGIN OF ty_transaction_variant,
        shdtvciu   TYPE shdtvciu,
        shdttciu   TYPE STANDARD TABLE OF shdttciu   WITH DEFAULT KEY,
        shdfvguicu TYPE STANDARD TABLE OF shdfvguicu WITH DEFAULT KEY,
        shdtvsvciu TYPE STANDARD TABLE OF shdtvsvciu WITH DEFAULT KEY,
      END OF ty_transaction_variant.
ENDCLASS.



CLASS zcl_abapgit_object_stvi IMPLEMENTATION.


  METHOD zif_abapgit_object~changed_by.

    DATA: lv_transaction_variant TYPE utcvariant.

    lv_transaction_variant = ms_item-obj_name.

    SELECT SINGLE chuser
    FROM shdtvciu
    INTO rv_user
    WHERE tcvariant = lv_transaction_variant.
    IF sy-subrc <> 0
    OR rv_user IS INITIAL.
      rv_user = c_user_unknown.
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_object~delete.

    DATA: lv_transaction_variant TYPE tcvariant.

    lv_transaction_variant = ms_item-obj_name.

    CALL FUNCTION 'RS_HDSYS_DELETE_VARIANT'
      EXPORTING
        tcvariant                 = lv_transaction_variant
        i_flag_client_independent = abap_true
      EXCEPTIONS
        variant_enqueued          = 1
        no_correction             = 2
        OTHERS                    = 3.
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise_t100( ).
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_object~deserialize.

    DATA: ls_transaction_variant TYPE ty_transaction_variant.

    io_xml->read(
      EXPORTING
        iv_name = 'STVI'
      CHANGING
        cg_data = ls_transaction_variant ).

    CALL FUNCTION 'ENQUEUE_ESTCVARCIU'
      EXPORTING
        tcvariant = ls_transaction_variant-shdtvciu-tcvariant
      EXCEPTIONS
        OTHERS    = 1.
    IF sy-subrc <> 0.
      MESSAGE e413(ms) WITH ls_transaction_variant-shdtvciu-tcvariant INTO zcx_abapgit_exception=>null.
      zcx_abapgit_exception=>raise_t100( ).
    ENDIF.

    corr_insert( iv_package = iv_package ).

*   Populate user details
    ls_transaction_variant-shdtvciu-crdate = sy-datum.
    ls_transaction_variant-shdtvciu-cruser = sy-uname.
    ls_transaction_variant-shdtvciu-chdate = sy-datum.
    ls_transaction_variant-shdtvciu-chuser = sy-uname.

    MODIFY shdtvciu   FROM ls_transaction_variant-shdtvciu.
    MODIFY shdttciu   FROM TABLE ls_transaction_variant-shdttciu[].
    INSERT shdfvguicu FROM TABLE ls_transaction_variant-shdfvguicu[] ACCEPTING DUPLICATE KEYS.
    INSERT shdtvsvciu FROM TABLE ls_transaction_variant-shdtvsvciu[] ACCEPTING DUPLICATE KEYS.

    CALL FUNCTION 'DEQUEUE_ESTCVARCIU'
      EXPORTING
        tcvariant = ls_transaction_variant-shdtvciu-tcvariant.

  ENDMETHOD.


  METHOD zif_abapgit_object~exists.

    DATA: lv_transaction_variant TYPE tcvariant.

    lv_transaction_variant = ms_item-obj_name.

    CALL FUNCTION 'RS_HDSYS_EXIST_CHECK_VARIANT'
      EXPORTING
        tcvariant                 = lv_transaction_variant
        i_flag_client_independent = abap_true
      EXCEPTIONS
        no_variant                = 1
        OTHERS                    = 2.
    IF sy-subrc = 0.
      rv_bool = abap_true.
    ENDIF.

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

    rv_is_locked = abap_false.

  ENDMETHOD.


  METHOD zif_abapgit_object~jump.
  ENDMETHOD.


  METHOD zif_abapgit_object~map_filename_to_object.
    RETURN.
  ENDMETHOD.


  METHOD zif_abapgit_object~map_object_to_filename.
    RETURN.
  ENDMETHOD.


  METHOD zif_abapgit_object~serialize.

    DATA: ls_transaction_variant TYPE ty_transaction_variant.

    ls_transaction_variant-shdtvciu-tcvariant = ms_item-obj_name.

    CALL FUNCTION 'RS_HDSYS_READ_TC_VARIANT_DB'
      EXPORTING
        tcvariant               = ls_transaction_variant-shdtvciu-tcvariant
        flag_client_independent = abap_true
      IMPORTING
        header_tcvariant        = ls_transaction_variant-shdtvciu
      TABLES
        screen_variants         = ls_transaction_variant-shdtvsvciu[]
        inactive_functions      = ls_transaction_variant-shdfvguicu[]
      EXCEPTIONS
        no_variant              = 1
        OTHERS                  = 2.
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise_t100( ).
    ENDIF.

*   Clear all user details
    CLEAR: ls_transaction_variant-shdtvciu-crdate,
           ls_transaction_variant-shdtvciu-cruser,
           ls_transaction_variant-shdtvciu-chdate,
           ls_transaction_variant-shdtvciu-chuser.

    SELECT *
      FROM shdttciu
      INTO TABLE ls_transaction_variant-shdttciu[]
      WHERE tcvariant = ls_transaction_variant-shdtvciu-tcvariant
      ORDER BY PRIMARY KEY.

    io_xml->add( iv_name = 'STVI'
                 ig_data = ls_transaction_variant ).

  ENDMETHOD.
ENDCLASS.
