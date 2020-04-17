class ZCL_ABAPGIT_OBJECT_SCVI definition
  public
  inheriting from ZCL_ABAPGIT_OBJECTS_SUPER
  final
  create public .

public section.

  interfaces ZIF_ABAPGIT_OBJECT .
  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES:
      BEGIN OF ty_screen_variant,
        header                   TYPE shdsvci,
        texts                    TYPE STANDARD TABLE OF shdsvtxci  WITH DEFAULT KEY,
        field_contents           TYPE STANDARD TABLE OF shdsvfvci  WITH DEFAULT KEY,
        guixt_scripts_attributes TYPE STANDARD TABLE OF shdguixt   WITH DEFAULT KEY,
        guixt_scripts_code       TYPE STANDARD TABLE OF shdgxtcode WITH DEFAULT KEY,
      END OF ty_screen_variant .
ENDCLASS.



CLASS ZCL_ABAPGIT_OBJECT_SCVI IMPLEMENTATION.


  METHOD zif_abapgit_object~changed_by.

    DATA: lv_screen_variant TYPE scvariant.

    lv_screen_variant = ms_item-obj_name.

    SELECT SINGLE chuser
    FROM shdsvci
    INTO rv_user
    WHERE scvariant = lv_screen_variant.
    IF sy-subrc NE 0
    OR rv_user IS INITIAL.
      rv_user = c_user_unknown.
    ENDIF. " IF sy-subrc NE 0

  ENDMETHOD.


  METHOD zif_abapgit_object~delete.

    DATA: lv_screen_variant TYPE scvariant.

    lv_screen_variant = ms_item-obj_name.

    CALL FUNCTION 'RS_HDSYS_DELETE_SC_VARIANT'
      EXPORTING
        scvariant        = lv_screen_variant
      EXCEPTIONS
        variant_enqueued = 1
        no_correction    = 2
        scvariant_used   = 3
        OTHERS           = 4.
    IF sy-subrc NE 0.
      zcx_abapgit_exception=>raise_t100( ).
    ENDIF. " IF sy-subrc NE 0

  ENDMETHOD.


  METHOD zif_abapgit_object~deserialize.

    DATA: lo_screen_variant TYPE REF TO zcl_abapgit_objects_generic.

    DATA: ls_screen_variant TYPE ty_screen_variant,
          ls_item           TYPE zif_abapgit_definitions=>ty_item.

    DATA: lv_text TYPE natxt.

    io_xml->read(
      EXPORTING
        iv_name = 'SCVI'
      CHANGING
        cg_data = ls_screen_variant ).

    ls_item-obj_type = 'SCVI'.
    ls_item-obj_name = ls_screen_variant-header-scvariant.
    ls_item-devclass = iv_package.

    CREATE OBJECT lo_screen_variant
      EXPORTING
        is_item = ls_item.

    CALL FUNCTION 'ENQUEUE_ESSCVARCIU'
      EXPORTING
        scvariant = ls_screen_variant-header-scvariant
      EXCEPTIONS
        OTHERS    = 01.
    IF sy-subrc NE 0.
      MESSAGE e413(ms) WITH ls_screen_variant-header-scvariant INTO lv_text.
      zcx_abapgit_exception=>raise_t100( ).
    ENDIF. " IF sy-subrc NE 0

    lo_screen_variant->deserialize(
      EXPORTING
        iv_package = iv_package
        io_xml     = io_xml ).

    CALL FUNCTION 'DEQUEUE_ESSCVARCIU'
      EXPORTING
        scvariant = ls_screen_variant-header-scvariant.

  ENDMETHOD.


  METHOD zif_abapgit_object~exists.

    DATA: lo_screen_variant TYPE REF TO zcl_abapgit_objects_generic.

    CREATE OBJECT lo_screen_variant
      EXPORTING
        is_item = ms_item.

    rv_bool = lo_screen_variant->exists( ).

  ENDMETHOD.


  METHOD zif_abapgit_object~get_comparator.
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

    rv_is_locked = abap_false. " ' '

  ENDMETHOD.


  METHOD zif_abapgit_object~jump.

    zcx_abapgit_exception=>raise( |TODO: Jump| ).

  ENDMETHOD.


  METHOD zif_abapgit_object~serialize.

    DATA: ls_screen_variant TYPE ty_screen_variant.

    ls_screen_variant-header-scvariant = ms_item-obj_name.

    CALL FUNCTION 'RS_HDSYS_READ_SC_VARIANT_DB'
      EXPORTING
        scvariant        = ls_screen_variant-header-scvariant
      IMPORTING
        header_scvariant = ls_screen_variant-header
      TABLES
        values_scvariant = ls_screen_variant-field_contents[]
        guixt_scripts    = ls_screen_variant-guixt_scripts_attributes[]
      EXCEPTIONS
        no_variant       = 1
        OTHERS           = 2.
    IF sy-subrc NE 0.
      zcx_abapgit_exception=>raise_t100( ).
    ENDIF. " IF sy-subrc NE 0

    SELECT *
    FROM shdsvtxci
    INTO TABLE ls_screen_variant-texts[]
    WHERE scvariant = ls_screen_variant-header-scvariant.

    SELECT *
    FROM shdgxtcode
    INTO TABLE ls_screen_variant-guixt_scripts_code[]
    WHERE scvariant = ls_screen_variant-header-scvariant.

    io_xml->add( iv_name = 'SCVI'
                 ig_data = ls_screen_variant ).

  ENDMETHOD.
ENDCLASS.
