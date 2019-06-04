CLASS zcl_abapgitp_object DEFINITION
  PUBLIC
  ABSTRACT
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES:
      BEGIN OF ty_metadata,
        class   TYPE string,
        version TYPE string,
      END OF ty_metadata .

    METHODS set_item
      IMPORTING
        !iv_obj_type TYPE tadir-object
        !iv_obj_name TYPE tadir-obj_name .
    METHODS get_supported_obj_types
          ABSTRACT
      RETURNING
        VALUE(rt_obj_type) TYPE objtyptable .
    METHODS wrap_serialize
      IMPORTING
        !io_xml TYPE REF TO object .
    METHODS wrap_deserialize
      IMPORTING
        !io_xml     TYPE REF TO object
        !iv_package TYPE devclass
      RAISING
        zcx_abapgitp_object .
  PROTECTED SECTION.
    CLASS-DATA   gv_serializer_classname TYPE string.
    CLASS-DATA   gv_serializer_version   TYPE string.

    DATA mv_obj_type TYPE tadir-object.
    DATA mv_obj_name TYPE tadir-obj_name .

    METHODS create_tadir_entry
      IMPORTING
        iv_package TYPE devclass
      RAISING
        zcx_abapgitp_object.

    METHODS get_metadata
      RETURNING VALUE(rs_metadata) TYPE zif_abapgitp_plugin=>ty_metadata.
  PRIVATE SECTION.

    METHODS change_object_directory_entry
      IMPORTING
        iv_delete  TYPE abap_bool
        iv_package TYPE devclass
      RAISING
        zcx_abapgitp_object.
ENDCLASS.



CLASS ZCL_ABAPGITP_OBJECT IMPLEMENTATION.


  METHOD change_object_directory_entry.

    DATA: lv_tadir_object   TYPE trobjtype,
          lv_exception_text TYPE string.


    lv_tadir_object = mv_obj_type.

    CALL FUNCTION 'TR_TADIR_INTERFACE'
      EXPORTING
        wi_test_modus                  = abap_false
        wi_delete_tadir_entry          = iv_delete    " X - delete object directory entry
        wi_tadir_pgmid                 = 'R3TR'    " Input for TADIR field PGMID
        wi_tadir_object                = lv_tadir_object    " Input for TADIR field OBJECT
        wi_tadir_obj_name              = mv_obj_name    " Input for TADIR field OBJ_NAME
        wi_tadir_devclass              = iv_package
        iv_delflag                     = abap_false
      EXCEPTIONS
        tadir_entry_not_existing       = 1
        tadir_entry_ill_type           = 2
        no_systemname                  = 3
        no_systemtype                  = 4
        original_system_conflict       = 5
        object_reserved_for_devclass   = 6
        object_exists_global           = 7
        object_exists_local            = 8
        object_is_distributed          = 9
        obj_specification_not_unique   = 10
        no_authorization_to_delete     = 11
        devclass_not_existing          = 12
        simultanious_set_remove_repair = 13
        order_missing                  = 14
        no_modification_of_head_syst   = 15
        pgmid_object_not_allowed       = 16
        masterlanguage_not_specified   = 17
        devclass_not_specified         = 18
        specify_owner_unique           = 19
        loc_priv_objs_no_repair        = 20
        gtadir_not_reached             = 21
        object_locked_for_order        = 22
        change_of_class_not_allowed    = 23
        no_change_from_sap_to_tmp      = 24
        OTHERS                         = 25.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO lv_exception_text.
      RAISE EXCEPTION TYPE zcx_abapgitp_object
        EXPORTING
          text = lv_exception_text.
    ENDIF.
  ENDMETHOD.


  METHOD CREATE_TADIR_ENTRY.
    me->change_object_directory_entry( iv_package = iv_package
                                       iv_delete  = abap_false ).
  ENDMETHOD.


  METHOD GET_METADATA.
    ASSERT gv_serializer_classname IS NOT INITIAL. "needs to be provided in class-constructor of the inheriting class
    rs_metadata-class = gv_serializer_classname.
    rs_metadata-version = gv_serializer_version. "optional
  ENDMETHOD.


  METHOD SET_ITEM.

    mv_obj_type = iv_obj_type.
    mv_obj_name = iv_obj_name.

  ENDMETHOD.


  METHOD wrap_deserialize.
    CALL METHOD me->('ZIF_ABAPGITP_PLUGIN~DESERIALIZE')
      EXPORTING
        io_xml     = zcl_abapgitp_xml_factory=>wrap_xml_input( io_xml )
        iv_package = iv_package.
  ENDMETHOD.


  METHOD wrap_serialize.
*    This method wraps the interface method in order to have a typed signature at the plugin-interface
*    while at the same time keeping the de-coupling of the ABAPGit Report and the plugins
    CALL METHOD me->('ZIF_ABAPGITP_PLUGIN~SERIALIZE')
      EXPORTING
        io_xml = zcl_abapgitp_xml_factory=>wrap_xml_output( io_xml ).
  ENDMETHOD.
ENDCLASS.
