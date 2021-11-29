CLASS zcl_abapgit_object_common_aff DEFINITION
  PUBLIC
  ABSTRACT
  INHERITING FROM zcl_abapgit_objects_super
  CREATE PUBLIC.

  PUBLIC SECTION.

    INTERFACES zif_abapgit_object
      ABSTRACT METHODS changed_by.

    ALIASES mo_files FOR zif_abapgit_object~mo_files.
  PROTECTED SECTION.
  PRIVATE SECTION.

ENDCLASS.



CLASS ZCL_ABAPGIT_OBJECT_COMMON_AFF IMPLEMENTATION.


  METHOD zif_abapgit_object~delete.

    DATA: lr_intf_aff_obj    TYPE REF TO data,
          lr_intf_aff_log    TYPE REF TO data,
          lo_handler_factory TYPE REF TO object,
          lo_object_handler  TYPE REF TO object,
          lo_object_aff      TYPE REF TO object,
          lo_aff_factory     TYPE REF TO object,
          lv_name            TYPE c LENGTH 120,
          lx_error           TYPE REF TO cx_root,
          lo_aff_log         TYPE REF TO object.

    FIELD-SYMBOLS: <lg_intf_aff_obj> TYPE any,
                   <lg_intf_aff_log> TYPE any.


    lv_name = ms_item-obj_name.

    TRY.
        CREATE OBJECT lo_handler_factory TYPE ('CL_AFF_OBJECT_HANDLER_FACTORY').
        CALL METHOD lo_handler_factory->('IF_AFF_OBJECT_HANDLER_FACTORY~GET_OBJECT_HANDLER')
          EXPORTING
            object_type = ms_item-obj_type
          RECEIVING
            result      = lo_object_handler.

        CREATE OBJECT lo_object_aff TYPE ('CL_AFF_OBJ')
           EXPORTING
             package = iv_package
             name    = lv_name
             type    = ms_item-obj_type.

        CREATE DATA lr_intf_aff_obj TYPE REF TO ('IF_AFF_OBJ').
        ASSIGN lr_intf_aff_obj->* TO <lg_intf_aff_obj>.
        <lg_intf_aff_obj> ?= lo_object_aff.

        CREATE OBJECT lo_aff_factory TYPE ('CL_AFF_FACTORY').
        CALL METHOD lo_aff_factory->('CREATE_LOG')
          RECEIVING
            result = lo_aff_log.

        CREATE DATA lr_intf_aff_log TYPE REF TO ('IF_AFF_LOG').
        ASSIGN lr_intf_aff_log->* TO  <lg_intf_aff_log>.
        <lg_intf_aff_log> ?= lo_aff_log.

        CALL METHOD lo_object_handler->('IF_AFF_OBJECT_HANDLER~DELETE')
          EXPORTING
            object = <lg_intf_aff_obj>
            log    = <lg_intf_aff_log>.

      CATCH cx_root INTO lx_error.
        zcx_abapgit_exception=>raise_with_text( lx_error ).
    ENDTRY.
  ENDMETHOD.


  METHOD zif_abapgit_object~deserialize.

    DATA: lr_intf_aff_obj         TYPE REF TO data,
          lr_intf_aff_file        TYPE REF TO data,
          lr_intf_files_container TYPE REF TO data,
          lr_intf_aff_log         TYPE REF TO data,
          lr_intf_aff_settings    TYPE REF TO data,
          lo_handler_factory      TYPE REF TO object,
          lo_object_handler       TYPE REF TO object,
          lo_object_aff           TYPE REF TO object,
          lo_object_json_file     TYPE REF TO object,
          lo_files_container      TYPE REF TO object,
          lo_settings             TYPE REF TO object,
          lo_aff_log              TYPE REF TO object,
          lo_aff_factory          TYPE REF TO object.

    DATA: lv_json_as_xstring TYPE xstring,
          lx_exception       TYPE REF TO cx_static_check,
          lv_name            TYPE c LENGTH 120.

    FIELD-SYMBOLS: <lg_intf_aff_obj>         TYPE any,
                   <lg_intf_aff_file>        TYPE any,
                   <lg_intf_files_container> TYPE any,
                   <lg_intf_aff_log>         TYPE any,
                   <lg_intf_aff_settings>    TYPE any.


    lv_json_as_xstring = mo_files->read_raw( iv_ext = 'json' ).

    lv_name = ms_item-obj_name.

    " beyond here there will be dragons....
    TRY.
        CREATE OBJECT lo_handler_factory TYPE ('CL_AFF_OBJECT_HANDLER_FACTORY').

        CALL METHOD lo_handler_factory->('IF_AFF_OBJECT_HANDLER_FACTORY~GET_OBJECT_HANDLER')
          EXPORTING
            object_type = ms_item-obj_type
          RECEIVING
            result      = lo_object_handler.

        CREATE OBJECT lo_object_json_file TYPE ('CL_AFF_FILE')
          EXPORTING
            name    = |{ to_lower( lv_name ) }.{ to_lower( ms_item-obj_type ) }.json|
            content = lv_json_as_xstring.

        CREATE OBJECT lo_object_aff TYPE ('CL_AFF_OBJ')
          EXPORTING
            package = ms_item-devclass
            name    = lv_name
            type    = ms_item-obj_type.

        CREATE DATA lr_intf_aff_obj TYPE REF TO ('IF_AFF_OBJ').
        ASSIGN lr_intf_aff_obj->* TO <lg_intf_aff_obj>.
        <lg_intf_aff_obj> ?= lo_object_aff.

        CREATE OBJECT lo_files_container TYPE ('CL_AFF_FILES_CONTAINER')
          EXPORTING
            object = <lg_intf_aff_obj>.

        CREATE OBJECT lo_settings TYPE ('CL_AFF_SETTINGS_DESERIALIZE')
          EXPORTING
            version  = 'I'
            language = mv_language
            user     = sy-uname.

        CREATE DATA lr_intf_aff_file TYPE REF TO ('IF_AFF_FILE').
        ASSIGN lr_intf_aff_file->* TO <lg_intf_aff_file>.
        <lg_intf_aff_file> ?= lo_object_json_file.

        CALL METHOD lo_files_container->('ADD_FILE')
          EXPORTING
            file = <lg_intf_aff_file>.

        CREATE OBJECT lo_aff_factory TYPE ('CL_AFF_FACTORY').
        CALL METHOD lo_aff_factory->('CREATE_LOG')
          RECEIVING
            result = lo_aff_log.

        CREATE DATA lr_intf_files_container TYPE REF TO ('IF_AFF_FILES_CONTAINER').
        ASSIGN lr_intf_files_container->* TO <lg_intf_files_container>.
        <lg_intf_files_container> ?= lo_files_container.

        CREATE DATA lr_intf_aff_log TYPE REF TO ('IF_AFF_LOG').
        ASSIGN lr_intf_aff_log->* TO  <lg_intf_aff_log>.
        <lg_intf_aff_log> ?= lo_aff_log.

        CREATE DATA lr_intf_aff_settings TYPE REF TO ('IF_AFF_SETTINGS_DESERIALIZE').
        ASSIGN lr_intf_aff_settings->* TO <lg_intf_aff_settings>.
        <lg_intf_aff_settings> ?= lo_settings.

        CALL METHOD lo_object_handler->('IF_AFF_OBJECT_HANDLER~DESERIALIZE')
          EXPORTING
            files_container = <lg_intf_files_container>
            log             = <lg_intf_aff_log>
            settings        = <lg_intf_aff_settings>.

      CATCH cx_static_check INTO lx_exception.
        " to do: is this the right exception handling?
        ii_log->add_exception(
            ix_exc  = lx_exception
            is_item = ms_item ).
    ENDTRY.
  ENDMETHOD.


  METHOD zif_abapgit_object~exists.
    DATA: lr_intf_aff_obj    TYPE REF TO data,
          lo_handler_factory TYPE REF TO object,
          lo_object_handler  TYPE REF TO object,
          lo_object_aff      TYPE REF TO object,
          lv_name            TYPE c LENGTH 120,
          lx_error           TYPE REF TO cx_root.

    FIELD-SYMBOLS: <lg_intf_aff_obj> TYPE any.

    lv_name = ms_item-obj_name.

    TRY.
        CREATE OBJECT lo_handler_factory TYPE ('CL_AFF_OBJECT_HANDLER_FACTORY').

        CALL METHOD lo_handler_factory->('IF_AFF_OBJECT_HANDLER_FACTORY~GET_OBJECT_HANDLER')
          EXPORTING
            object_type = ms_item-obj_type
          RECEIVING
            result      = lo_object_handler.

        CREATE OBJECT lo_object_aff TYPE ('CL_AFF_OBJ')
           EXPORTING
             package = ms_item-devclass
             name    = lv_name
             type    = ms_item-obj_type.

        CREATE DATA lr_intf_aff_obj TYPE REF TO ('IF_AFF_OBJ').
        ASSIGN lr_intf_aff_obj->* TO <lg_intf_aff_obj>.
        <lg_intf_aff_obj> ?= lo_object_aff.

        CALL METHOD lo_object_handler->('IF_AFF_OBJECT_HANDLER~EXISTS')
          EXPORTING
            object = <lg_intf_aff_obj>
          RECEIVING
            result = rv_bool.

      CATCH cx_root INTO lx_error.
        zcx_abapgit_exception=>raise_with_text( lx_error ).
    ENDTRY.

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
    DATA lv_lock_object   TYPE string.
    DATA lv_argument      TYPE seqg3-garg.

    lv_lock_object = |{ ms_item-obj_type }{ ms_item-obj_name }*|.
    lv_argument  = lv_lock_object.

    rv_is_locked = exists_a_lock_entry_for( iv_lock_object = 'ESWB_EO'
                                            iv_argument    = lv_argument  ).
  ENDMETHOD.


  METHOD zif_abapgit_object~jump.
  ENDMETHOD.


  METHOD zif_abapgit_object~serialize.

    DATA: lr_intf_aff_obj      TYPE REF TO data,
          lr_intf_aff_log      TYPE REF TO data,
          lr_intf_aff_settings TYPE REF TO data,
          lo_handler_factory   TYPE REF TO object,
          lo_object_handler    TYPE REF TO object,
          lo_object_aff        TYPE REF TO object,
          lo_object_json_file  TYPE REF TO object,
          lo_files_container   TYPE REF TO object,
          lo_settings          TYPE REF TO object,
          lo_aff_log           TYPE REF TO object,
          lo_aff_factory       TYPE REF TO object.

    DATA: lv_json_as_xstring TYPE xstring,
          lx_exception       TYPE REF TO cx_root,
          lv_name            TYPE c LENGTH 120.

    FIELD-SYMBOLS: <lg_intf_aff_obj>      TYPE any,
                   <lg_intf_aff_log>      TYPE any,
                   <lg_intf_aff_settings> TYPE any.


    lv_name = ms_item-obj_name.

    TRY.
        CREATE OBJECT lo_handler_factory TYPE ('CL_AFF_OBJECT_HANDLER_FACTORY').

        CALL METHOD lo_handler_factory->('IF_AFF_OBJECT_HANDLER_FACTORY~GET_OBJECT_HANDLER')
          EXPORTING
            object_type = ms_item-obj_type
          RECEIVING
            result      = lo_object_handler.

        CREATE OBJECT lo_object_aff TYPE ('CL_AFF_OBJ')
           EXPORTING
             package = ms_item-devclass
             name    = lv_name
             type    = ms_item-obj_type.

        CREATE OBJECT lo_settings TYPE ('CL_AFF_SETTINGS_SERIALIZE')
          EXPORTING
            version = 'A'
            language = mv_language.

        CREATE OBJECT lo_aff_factory TYPE ('CL_AFF_FACTORY').
        CALL METHOD lo_aff_factory->('CREATE_LOG')
          RECEIVING
            result = lo_aff_log.

        CREATE DATA lr_intf_aff_log TYPE REF TO ('IF_AFF_LOG').
        ASSIGN lr_intf_aff_log->* TO <lg_intf_aff_log>.
        <lg_intf_aff_log> ?= lo_aff_log.

        CREATE DATA lr_intf_aff_settings TYPE REF TO ('IF_AFF_SETTINGS_SERIALIZE').
        ASSIGN lr_intf_aff_settings->* TO <lg_intf_aff_settings>.
        <lg_intf_aff_settings> ?= lo_settings.

        CREATE DATA lr_intf_aff_obj TYPE REF TO ('IF_AFF_OBj').
        ASSIGN lr_intf_aff_obj->* TO <lg_intf_aff_obj>.
        <lg_intf_aff_obj> ?= lo_object_aff.

        CALL METHOD lo_object_handler->('IF_AFF_OBJECT_HANDLER~SERIALIZE')
          EXPORTING
            object   = <lg_intf_aff_obj>
            log      = <lg_intf_aff_log>
            settings = <lg_intf_aff_settings>
          RECEIVING
            result   = lo_files_container.

        CALL METHOD lo_files_container->('IF_AFF_FILES_CONTAINER~GET_FILE')
          EXPORTING
            name   = |{ to_lower( lv_name ) }.{ to_lower( ms_item-obj_type ) }.json|
          RECEIVING
            result = lo_object_json_file.

        CALL METHOD lo_object_json_file->('IF_AFF_FILE~GET_CONTENT')
          RECEIVING
            result = lv_json_as_xstring.

        mo_files->add_raw( iv_ext = 'json'
                           iv_data = lv_json_as_xstring ).

      CATCH cx_root INTO lx_exception.
        zcx_abapgit_exception=>raise_with_text( lx_exception ).
    ENDTRY.

  ENDMETHOD.
ENDCLASS.
