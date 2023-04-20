CLASS zcl_abapgit_object_enhc DEFINITION
  PUBLIC
  INHERITING FROM zcl_abapgit_objects_super.

  PUBLIC SECTION.
    INTERFACES zif_abapgit_object.

    METHODS:
      constructor
        IMPORTING
          is_item     TYPE zif_abapgit_definitions=>ty_item
          iv_language TYPE spras.
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA:
      mv_composite_id TYPE enhcompositename.

ENDCLASS.



CLASS zcl_abapgit_object_enhc IMPLEMENTATION.


  METHOD constructor.

    super->constructor( is_item     = is_item
                        iv_language = iv_language ).

    mv_composite_id = ms_item-obj_name.

  ENDMETHOD.


  METHOD zif_abapgit_object~changed_by.

    SELECT SINGLE changedby INTO rv_user FROM enhcompheader
      WHERE enhcomposite = ms_item-obj_name AND version = 'A'.
    IF sy-subrc <> 0.
      rv_user = c_user_unknown.
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_object~delete.

    DATA: lx_enh_root   TYPE REF TO cx_enh_root,
          li_enh_object TYPE REF TO if_enh_object.

    TRY.
        li_enh_object = cl_enh_factory=>load_enhancement_composite(
          name = mv_composite_id
          lock = abap_true ).

        li_enh_object->delete( nevertheless_delete = abap_true
                               run_dark            = abap_true ).
        li_enh_object->unlock( ).

      CATCH cx_enh_root INTO lx_enh_root.
        zcx_abapgit_exception=>raise_with_text( lx_enh_root ).
    ENDTRY.

  ENDMETHOD.


  METHOD zif_abapgit_object~deserialize.

    DATA: lx_enh_root         TYPE REF TO cx_enh_root,
          li_enh_composite    TYPE REF TO if_enh_composite,
          lv_package          TYPE devclass,
          lt_composite_childs TYPE enhcompositename_it,
          lt_enh_childs       TYPE enhname_it,
          lv_longtext_id      TYPE enhdocuobject,
          lv_vers             TYPE enhcompheader-version,
          lv_shorttext        TYPE string.

    FIELD-SYMBOLS: <lv_composite_child> TYPE enhcompositename,
                   <lv_enh_child>       LIKE LINE OF lt_enh_childs.

    lv_package = iv_package.


    io_xml->read( EXPORTING iv_name = 'SHORTTEXT'
                  CHANGING  cg_data = lv_shorttext ).
    io_xml->read( EXPORTING iv_name = 'COMPOSITE_CHILDS'
                  CHANGING  cg_data = lt_composite_childs ).
    io_xml->read( EXPORTING iv_name = 'ENH_CHILDS'
                  CHANGING  cg_data = lt_enh_childs ).
    io_xml->read( EXPORTING iv_name = 'LONGTEXT_ID'
                  CHANGING  cg_data = lv_longtext_id ).

    SELECT SINGLE version FROM enhcompheader INTO lv_vers WHERE enhcomposite = ms_item-obj_name.
    IF sy-subrc = 0.
      " If object exists already, then set TADIR entry to deleted
      " otherwise create_enhancement_composite will fail
      CALL FUNCTION 'TR_TADIR_INTERFACE'
        EXPORTING
          wi_test_modus     = abap_false
          wi_tadir_pgmid    = 'R3TR'
          wi_tadir_object   = ms_item-obj_type
          wi_tadir_obj_name = ms_item-obj_name
          iv_delflag        = abap_true
        EXCEPTIONS
          OTHERS            = 1.
      IF sy-subrc <> 0.
        zcx_abapgit_exception=>raise_t100( ).
      ENDIF.
    ENDIF.

    TRY.
        cl_enh_factory=>create_enhancement_composite(
          EXPORTING
            name      = mv_composite_id
            run_dark  = abap_true
          IMPORTING
            composite = li_enh_composite
          CHANGING
            devclass  = lv_package ).

        li_enh_composite->if_enh_object_docu~set_shorttext( lv_shorttext ).

        LOOP AT lt_composite_childs ASSIGNING <lv_composite_child>.
          li_enh_composite->add_composite_child( <lv_composite_child> ).
        ENDLOOP.

        LOOP AT lt_enh_childs ASSIGNING <lv_enh_child>.
          li_enh_composite->add_enh_child( <lv_enh_child> ).
        ENDLOOP.

        li_enh_composite->set_longtext_id( lv_longtext_id ).

        li_enh_composite->if_enh_object~save( ).
        li_enh_composite->if_enh_object~activate( ).
        li_enh_composite->if_enh_object~unlock( ).

      CATCH cx_enh_root INTO lx_enh_root.
        zcx_abapgit_exception=>raise_with_text( lx_enh_root ).
    ENDTRY.

  ENDMETHOD.


  METHOD zif_abapgit_object~exists.

    TRY.
        cl_enh_factory=>load_enhancement_composite(
          name = mv_composite_id
          lock = abap_false ).
        rv_bool = abap_true.
      CATCH cx_enh_root.
        rv_bool = abap_false.
    ENDTRY.

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

    DATA: lv_argument TYPE seqg3-garg.

    lv_argument = |{ mv_composite_id }|.
    OVERLAY lv_argument WITH '                                  '.
    lv_argument = |{ lv_argument }*|.

    rv_is_locked = exists_a_lock_entry_for( iv_lock_object = |E_ENHANCE|
                                            iv_argument    = lv_argument ).

  ENDMETHOD.


  METHOD zif_abapgit_object~jump.
    " Covered by ZCL_ABAPGIT_OBJECTS=>JUMP
  ENDMETHOD.


  METHOD zif_abapgit_object~map_filename_to_object.
    RETURN.
  ENDMETHOD.


  METHOD zif_abapgit_object~map_object_to_filename.
    RETURN.
  ENDMETHOD.


  METHOD zif_abapgit_object~serialize.

    DATA: lx_enh_root         TYPE REF TO cx_enh_root,
          li_enh_composite    TYPE REF TO if_enh_composite,
          lt_composite_childs TYPE enhcompositename_it,
          lt_enh_childs       TYPE enhname_it,
          lv_longtext_id      TYPE enhdocuobject,
          lv_shorttext        TYPE string.

    TRY.
        li_enh_composite = cl_enh_factory=>load_enhancement_composite(
          name = mv_composite_id
          lock = abap_false ).

        lv_shorttext = li_enh_composite->if_enh_object_docu~get_shorttext( ).

        lt_composite_childs = li_enh_composite->get_composite_childs( ).
        lt_enh_childs       = li_enh_composite->get_enh_childs( ).
        lv_longtext_id      = li_enh_composite->get_longtext_id( ).

        io_xml->add( iv_name = 'SHORTTEXT'
                     ig_data = lv_shorttext ).
        io_xml->add( iv_name = 'COMPOSITE_CHILDS'
                     ig_data = lt_composite_childs ).
        io_xml->add( iv_name = 'ENH_CHILDS'
                     ig_data = lt_enh_childs ).
        io_xml->add( iv_name = 'LONGTEXT_ID'
                     ig_data = lv_longtext_id ).

      CATCH cx_enh_root INTO lx_enh_root.
        zcx_abapgit_exception=>raise_with_text( lx_enh_root ).
    ENDTRY.

  ENDMETHOD.
ENDCLASS.
