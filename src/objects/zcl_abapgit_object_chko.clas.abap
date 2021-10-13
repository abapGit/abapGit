CLASS zcl_abapgit_object_chko DEFINITION
  PUBLIC
  INHERITING FROM zcl_abapgit_objects_super
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_abapgit_object.
    ALIASES mo_files FOR zif_abapgit_object~mo_files.
  PRIVATE SECTION.

    TYPES:
      BEGIN OF ty_content,
        category           TYPE string,
        implementing_class TYPE string,
        remote_enabled     TYPE flag,
        parameters         TYPE cl_chko_db_api=>ty_parameters,
      END OF ty_content .
    TYPES:
      BEGIN OF ty_chko.
        INCLUDE TYPE if_aff_types=>ty_aff_header.
    TYPES:
        content TYPE ty_content,
      END OF ty_chko .
ENDCLASS.



CLASS ZCL_ABAPGIT_OBJECT_CHKO IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_ABAPGIT_OBJECT_CHKO->ZIF_ABAPGIT_OBJECT~CHANGED_BY
* +-------------------------------------------------------------------------------------------------+
* | [<-()] RV_USER                        TYPE        XUBNAME
* | [!CX!] ZCX_ABAPGIT_EXCEPTION
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD zif_abapgit_object~changed_by.
    DATA(chko_db_api) = NEW cl_chko_db_api( ).
    DATA(chko_header) = chko_db_api->get_header( name     = CONV #( ms_item-obj_name )
                                                 version  = 'I' ).
    IF chko_header IS INITIAL.
      chko_header = chko_db_api->get_header( name     = CONV #( ms_item-obj_name )
                                                   version  = 'A' ).
    ENDIF.
    rv_user = chko_header-changed_by.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_ABAPGIT_OBJECT_CHKO->ZIF_ABAPGIT_OBJECT~DELETE
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_PACKAGE                     TYPE        DEVCLASS
* | [!CX!] ZCX_ABAPGIT_EXCEPTION
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD zif_abapgit_object~delete.
    DATA(chko_db_api) = NEW cl_chko_db_api( ).
    chko_db_api->delete(
      name    = CONV #( ms_item-obj_name )
      version = 'I'
    ).
    chko_db_api->delete(
      name    = CONV #( ms_item-obj_name )
      version = 'A'
    ).
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_ABAPGIT_OBJECT_CHKO->ZIF_ABAPGIT_OBJECT~DESERIALIZE
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_PACKAGE                     TYPE        DEVCLASS
* | [--->] IO_XML                         TYPE REF TO ZCL_ABAPGIT_XML_INPUT
* | [--->] IV_STEP                        TYPE        TY_DESERIALIZATION_STEP
* | [!CX!] ZCX_ABAPGIT_EXCEPTION
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD zif_abapgit_object~deserialize.

    DATA properties TYPE ty_chko.

    TRY.
        DATA(json_as_xstring) = mo_files->read_raw( iv_ext = 'json' ) ##NO_TEXT.
        DATA(content_handler_json) = cl_aff_content_handler_factory=>get_handler_for_json( simple_transformation = 'ZSATC_CHKO_JSON' ). "TODO replace SATC_CHKO_JSON
        content_handler_json->deserialize(
          EXPORTING
            content = json_as_xstring
          IMPORTING
            data    = properties ).

        properties-header-abap_langu_version = if_abap_language_version=>gc_version-sap_cloud_platform.

        DATA(persistence) = NEW zcl_chko_aff_persistence( ).
        persistence->save_content(
          data     = properties
          object   = NEW cl_aff_obj( package = ms_item-devclass name = CONV #( ms_item-obj_name ) type = ms_item-obj_type )
          language = mv_language
          version  = 'I'
          saved_by = sy-uname ).
      CATCH cx_aff_root INTO DATA(exception).

    ENDTRY.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_ABAPGIT_OBJECT_CHKO->ZIF_ABAPGIT_OBJECT~EXISTS
* +-------------------------------------------------------------------------------------------------+
* | [<-()] RV_BOOL                        TYPE        ABAP_BOOL
* | [!CX!] ZCX_ABAPGIT_EXCEPTION
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD zif_abapgit_object~exists.
    rv_bool = NEW cl_chko_db_api( )->exists( name = CONV #( ms_item-obj_name ) ).
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_ABAPGIT_OBJECT_CHKO->ZIF_ABAPGIT_OBJECT~GET_COMPARATOR
* +-------------------------------------------------------------------------------------------------+
* | [<-()] RI_COMPARATOR                  TYPE REF TO ZIF_ABAPGIT_COMPARATOR
* | [!CX!] ZCX_ABAPGIT_EXCEPTION
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD zif_abapgit_object~get_comparator.
    RETURN.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_ABAPGIT_OBJECT_CHKO->ZIF_ABAPGIT_OBJECT~GET_DESERIALIZE_STEPS
* +-------------------------------------------------------------------------------------------------+
* | [<-()] RT_STEPS                       TYPE        TY_DESERIALIZATION_STEP_TT
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD zif_abapgit_object~get_deserialize_steps.
    APPEND if_abapgit_object=>gc_step_id-abap TO rt_steps.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_ABAPGIT_OBJECT_CHKO->ZIF_ABAPGIT_OBJECT~GET_METADATA
* +-------------------------------------------------------------------------------------------------+
* | [<-()] RS_METADATA                    TYPE        ZIF_ABAPGIT_DEFINITIONS=>TY_METADATA
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD zif_abapgit_object~get_metadata.
    rs_metadata = get_metadata( ).
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_ABAPGIT_OBJECT_CHKO->ZIF_ABAPGIT_OBJECT~IS_ACTIVE
* +-------------------------------------------------------------------------------------------------+
* | [<-()] RV_ACTIVE                      TYPE        ABAP_BOOL
* | [!CX!] ZCX_ABAPGIT_EXCEPTION
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD zif_abapgit_object~is_active.
    rv_active = is_active( ).
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_ABAPGIT_OBJECT_CHKO->ZIF_ABAPGIT_OBJECT~IS_LOCKED
* +-------------------------------------------------------------------------------------------------+
* | [<-()] RV_IS_LOCKED                   TYPE        ABAP_BOOL
* | [!CX!] ZCX_ABAPGIT_EXCEPTION
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD zif_abapgit_object~is_locked.
    DATA(lock_object) = |{ ms_item-obj_type }{ ms_item-obj_name }*|.

    rv_is_locked = exists_a_lock_entry_for( iv_lock_object = 'ESWB_EO'
                                            iv_argument    = CONV #( lock_object ) ).
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_ABAPGIT_OBJECT_CHKO->ZIF_ABAPGIT_OBJECT~JUMP
* +-------------------------------------------------------------------------------------------------+
* | [!CX!] ZCX_ABAPGIT_EXCEPTION
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD zif_abapgit_object~jump.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_ABAPGIT_OBJECT_CHKO->ZIF_ABAPGIT_OBJECT~SERIALIZE
* +-------------------------------------------------------------------------------------------------+
* | [--->] IO_XML                         TYPE REF TO ZCL_ABAPGIT_XML_OUTPUT
* | [--->] II_LOG                         TYPE REF TO ZIF_ABAPGIT_LOG
* | [!CX!] ZCX_ABAPGIT_EXCEPTION
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD zif_abapgit_object~serialize.
    TRY.

        DATA(db_api) = NEW cl_chko_db_api( ).

        DATA object TYPE REF TO if_aff_obj.
        object = NEW cl_aff_obj( package = ms_item-devclass name = CONV #( ms_item-obj_name ) type = ms_item-obj_type ).
        DATA(chko_name) = object->get_name( ).
        DATA(master_language) = cl_aff_object_utility=>get_master_language_from_tadir( object ).

        DATA(chko_header) = db_api->get_header( name     = CONV #( chko_name )
                                                version  = 'A'
                                                language = mv_language ).
        DATA(chko_content) = db_api->get_content( name     = CONV #( chko_name )
                                                  version  = 'A'
                                                  language = mv_language ).

        DATA(properties) = VALUE ty_chko(
          header-schema              = `http://sap.com/schema/chko.json` ##NO_TEXT
          header-description         = chko_header-description
          header-master_language     = master_language
          header-abap_langu_version  = chko_header-abap_language_version
          content-category           = chko_content-category-name
          content-implementing_class = chko_content-implementing_class-name
          content-parameters         = chko_content-parameters
          content-remote_enabled     = chko_content-remote_enabled ).

        DATA json_as_xstring TYPE xstring.

        "todo: ajson parser

        mo_files->add_raw( iv_ext = 'json' iv_data = json_as_xstring ).

      CATCH cx_aff_root INTO DATA(exception).
        ii_log->add_exception(
          ix_exc  = exception
          is_item = ms_item ).
    ENDTRY.
  ENDMETHOD.
ENDCLASS.
