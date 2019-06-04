CLASS zcl_abapgitp_object_by_sobj DEFINITION
  PUBLIC
  INHERITING FROM zcl_abapgitp_object
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_abapgitp_plugin .

    CLASS-METHODS class_constructor .

    METHODS get_supported_obj_types
        REDEFINITION .
  PROTECTED SECTION.
  PRIVATE SECTION.

    METHODS get_tlogo_bridge
      RETURNING VALUE(ro_tlogo_bridge) TYPE REF TO lcl_tlogo_bridge
      RAISING   zcx_abapgitp_object.

    DATA mo_tlogo_bridge TYPE REF TO lcl_tlogo_bridge.

    CLASS-DATA gt_supported_obj_types TYPE objtyptable.
ENDCLASS.



CLASS ZCL_ABAPGITP_OBJECT_BY_SOBJ IMPLEMENTATION.


  METHOD class_constructor.

    DATA: lt_all_objectname TYPE STANDARD TABLE OF objh-objectname WITH DEFAULT KEY,
          lv_objectname     LIKE LINE OF lt_all_objectname,
          lv_obj_type       LIKE LINE OF gt_supported_obj_types.


    gv_serializer_classname = 'ZCL_ABAPGIT_OBJECT_BY_SOBJ'.
    gv_serializer_version   = '1.0'.

    SELECT objectname FROM objh
      INTO TABLE lt_all_objectname
      WHERE objecttype = 'L'.                           "#EC CI_GENBUFF

    LOOP AT lt_all_objectname INTO lv_objectname.
      IF strlen( lv_objectname ) <= 4.
        lv_obj_type = lv_objectname.
        INSERT lv_obj_type INTO TABLE gt_supported_obj_types.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD get_supported_obj_types.
    rt_obj_type = gt_supported_obj_types.
  ENDMETHOD.


  METHOD get_tlogo_bridge.
    IF mo_tlogo_bridge IS INITIAL.

      TRY.
          DATA lx_bridge_creation TYPE REF TO lcx_obj_exception.
          CREATE OBJECT mo_tlogo_bridge
            EXPORTING
              iv_object      = mv_obj_type
              iv_object_name = mv_obj_name.
        CATCH lcx_obj_exception INTO lx_bridge_creation.
          RAISE EXCEPTION TYPE zcx_abapgitp_object
            EXPORTING
              text     = lx_bridge_creation->get_text( )
              previous = lx_bridge_creation.
      ENDTRY.
    ENDIF.

    ro_tlogo_bridge = mo_tlogo_bridge.
  ENDMETHOD.


  METHOD zif_abapgitp_plugin~delete.

    get_tlogo_bridge( )->delete_object_on_db( ).

  ENDMETHOD.


  METHOD zif_abapgitp_plugin~deserialize.
    DATA lo_object_container TYPE REF TO lcl_abapgit_xml_container.
    DATA lx_obj_exception  TYPE REF TO lcx_obj_exception.

    CREATE OBJECT lo_object_container.

    lo_object_container->set_xml_input( io_xml ).

    TRY.
        get_tlogo_bridge( )->import_object( lo_object_container ).
      CATCH lcx_obj_exception INTO lx_obj_exception.
        RAISE EXCEPTION TYPE zcx_abapgitp_object
          EXPORTING
            text     = |{ mv_obj_type } { mv_obj_name }: {
                        lx_obj_exception->get_error_text( ) }|
            previous = lx_obj_exception.
    ENDTRY.

    me->create_tadir_entry( iv_package ).
  ENDMETHOD.


  METHOD zif_abapgitp_plugin~exists.
    TRY.
        rv_bool = get_tlogo_bridge( )->instance_exists( ).
      CATCH lcx_obj_exception.
        rv_bool = abap_false.
    ENDTRY.
  ENDMETHOD.


  METHOD zif_abapgitp_plugin~get_metadata.
    rs_metadata = get_metadata( ).
    rs_metadata-delete_tadir = abap_true.
  ENDMETHOD.


  METHOD zif_abapgitp_plugin~jump.
    RETURN.
  ENDMETHOD.


  METHOD zif_abapgitp_plugin~serialize.
    DATA lo_object_container TYPE REF TO lcl_abapgit_xml_container.
    DATA lx_obj_exception  TYPE REF TO lcx_obj_exception.

    TRY.
        IF get_tlogo_bridge( )->instance_exists( ) = abap_true.

          CREATE OBJECT lo_object_container.
          lo_object_container->set_xml_output( io_xml ).

          get_tlogo_bridge( )->export_object( lo_object_container ).

        ENDIF. "No else needed - if the object does not exist, we'll not serialize anything
      CATCH lcx_obj_exception INTO lx_obj_exception.
        RAISE EXCEPTION TYPE zcx_abapgitp_object
          EXPORTING
            text     = lx_obj_exception->get_text( )
            previous = lx_obj_exception.
    ENDTRY.
  ENDMETHOD.
ENDCLASS.
