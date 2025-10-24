"! Provides common functionality for the abapGit integration of objects based on ABAP File Formats (AFF).
"! It inherits from {@link ZCL_ABAPGIT_OBJECTS_SUPER} and implements the interface {@link ZIF_ABAPGIT_OBJECT}.
"! <br/><br/>
"! Each subclass must implement at least the abstract method ZIF_ABAPGIT_OBJECT~CHANGED_BY.
"! In case you deal with a DDIC object, the methods GET_METADATA and ZIF_ABAPGIT_OBJECT~GET_DESERIALIZE_STEPS
"! have to be redefined in the subclass.
"! <br/><br/>
"! In case the object has an additional file to the json file, the subclass needs
"! to redefine the method GET_ADDITIONAL_EXTENSIONS.
"! <br/><br/>
"! This common class fully relies on the implementation of the object-specific AFF handler
"! (it inherits from {@link CL_AFF_OBJECT_HANDLER}).
"! Precisely, for the existence check-, serialization-, deserialization- or deletion-functionality, the corresponding
"! AFF object handler method is called.
CLASS zcl_abapgit_object_common_aff DEFINITION
  PUBLIC
  INHERITING FROM zcl_abapgit_objects_super
  ABSTRACT
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_abapgit_object
      ABSTRACT METHODS changed_by .

    METHODS constructor
      IMPORTING
        !is_item        TYPE zif_abapgit_definitions=>ty_item
        !iv_language    TYPE spras
        !io_files       TYPE REF TO zcl_abapgit_objects_files OPTIONAL
        !io_i18n_params TYPE REF TO zcl_abapgit_i18n_params OPTIONAL
      RAISING
        zcx_abapgit_type_not_supported.

  PROTECTED SECTION.
    TYPES: BEGIN OF ty_extension_mapper_pair,
             "! file extension
             extension        TYPE string,
             "! instance of {@link CL_AFF_FILE_NAME_MAPPER} providing file names for file extensions
             file_name_mapper TYPE REF TO object,
           END OF ty_extension_mapper_pair,
           ty_extension_mapper_pairs TYPE STANDARD TABLE OF ty_extension_mapper_pair WITH DEFAULT KEY.

    "! Delivers other file extensions than json to be considered at serialize or deserialize of an object
    METHODS get_additional_extensions
      RETURNING VALUE(rv_additional_extensions) TYPE ty_extension_mapper_pairs ##NEEDED.

    "! Delivers an instance of AFF object handler ({@link IF_AFF_OBJECT_HANDLER})
    METHODS get_object_handler
      RETURNING
        VALUE(ro_object_handler) TYPE REF TO object
      RAISING
        zcx_abapgit_exception.


    METHODS create_aff_setting_deserialize FINAL
      RETURNING
        VALUE(ro_settings_deserialize) TYPE REF TO object
      RAISING
        zcx_abapgit_exception.

  PRIVATE SECTION.
    METHODS is_file_empty
      IMPORTING
        io_object_json_file TYPE REF TO object
      RETURNING
        VALUE(rv_is_empty)  TYPE abap_bool.

    CLASS-METHODS remove_abap_language_version
      IMPORTING
        iv_json_as_xstring               TYPE xstring
      RETURNING
        VALUE(rv_json_as_xstring_wo_alv) TYPE xstring
      RAISING
        zcx_abapgit_exception.

ENDCLASS.



CLASS zcl_abapgit_object_common_aff IMPLEMENTATION.


  METHOD constructor.

    DATA:
      lv_is_supported TYPE abap_bool,
      lo_handler      TYPE REF TO object.

    super->constructor(
      is_item        = is_item
      iv_language    = iv_language
      io_files       = io_files
      io_i18n_params = io_i18n_params ).

    " Check if AFF handler exists and if object type is registered and supported
    TRY.
        lo_handler = get_object_handler( ).

        lv_is_supported = zcl_abapgit_aff_factory=>get_registry( )->is_supported_object_type( is_item-obj_type ).
      CATCH cx_root.
        lv_is_supported = abap_false.
    ENDTRY.

    IF lv_is_supported IS INITIAL.
      RAISE EXCEPTION TYPE zcx_abapgit_type_not_supported EXPORTING obj_type = is_item-obj_type.
    ENDIF.

  ENDMETHOD.


  METHOD create_aff_setting_deserialize.
    IF ms_item-abap_language_version <> zcl_abapgit_abap_language_vers=>c_any_abap_language_version AND
       ms_item-abap_language_version <> zcl_abapgit_abap_language_vers=>c_no_abap_language_version.
      TRY.
          CREATE OBJECT ro_settings_deserialize TYPE ('CL_AFF_SETTINGS_DESERIALIZE')
            EXPORTING
              version               = 'A'
              language              = mv_language
              user                  = sy-uname
              abap_language_version = ms_item-abap_language_version.
        CATCH cx_root.
          zcx_abapgit_exception=>raise( |System does not supported ABAP language version for AFF| ).
      ENDTRY.
    ELSE.
      CREATE OBJECT ro_settings_deserialize TYPE ('CL_AFF_SETTINGS_DESERIALIZE')
        EXPORTING
          version               = 'A'
          language              = mv_language
          user                  = sy-uname.
    ENDIF.
  ENDMETHOD.


  METHOD get_additional_extensions.
    RETURN.
  ENDMETHOD.


  METHOD get_object_handler.

    DATA lo_handler_factory TYPE REF TO object.

    CREATE OBJECT lo_handler_factory TYPE ('CL_AFF_OBJECT_HANDLER_FACTORY').

    CALL METHOD lo_handler_factory->('IF_AFF_OBJECT_HANDLER_FACTORY~GET_OBJECT_HANDLER')
      EXPORTING
        object_type = ms_item-obj_type
      RECEIVING
        result      = ro_object_handler.

  ENDMETHOD.


  METHOD is_file_empty.

    CALL METHOD io_object_json_file->('IF_AFF_FILE~IS_DELETION')
      RECEIVING
        result = rv_is_empty.

  ENDMETHOD.


  METHOD remove_abap_language_version.
    DATA lv_json TYPE string.
    DATA lv_json_wo_alv TYPE string.
    DATA li_json TYPE REF TO zif_abapgit_ajson.

    lv_json = zcl_abapgit_convert=>xstring_to_string_utf8( iv_json_as_xstring ).

    TRY.
        li_json = zcl_abapgit_ajson=>parse( iv_json            = lv_json
                                            iv_keep_item_order = abap_true ).
        li_json->delete( '/header/abapLanguageVersion' ).
        lv_json_wo_alv = li_json->stringify( 2 ).

        rv_json_as_xstring_wo_alv = zcl_abapgit_convert=>string_to_xstring_utf8( lv_json_wo_alv ).

      CATCH zcx_abapgit_ajson_error.
        rv_json_as_xstring_wo_alv = iv_json_as_xstring.
    ENDTRY.

  ENDMETHOD.


  METHOD zif_abapgit_object~delete.

    DATA: lr_intf_aff_obj   TYPE REF TO data,
          lr_intf_aff_log   TYPE REF TO data,
          lr_messages       TYPE REF TO data,
          lo_object_handler TYPE REF TO object,
          lo_object_aff     TYPE REF TO object,
          lo_aff_factory    TYPE REF TO object,
          lv_name           TYPE c LENGTH 120,
          lx_error          TYPE REF TO cx_root,
          lo_aff_log        TYPE REF TO object.

    FIELD-SYMBOLS: <ls_intf_aff_obj> TYPE any,
                   <ls_intf_aff_log> TYPE any,
                   <ls_messages>     TYPE ANY TABLE,
                   <ls_message>      TYPE any,
                   <ls_msg>          TYPE symsg.

    lv_name = ms_item-obj_name.

    TRY.
        lo_object_handler = get_object_handler( ).

        CREATE OBJECT lo_object_aff TYPE ('CL_AFF_OBJ')
           EXPORTING
             package = iv_package
             name    = lv_name
             type    = ms_item-obj_type.

        CREATE DATA lr_intf_aff_obj TYPE REF TO ('IF_AFF_OBJ').
        ASSIGN lr_intf_aff_obj->* TO <ls_intf_aff_obj>.
        <ls_intf_aff_obj> ?= lo_object_aff.

        CREATE OBJECT lo_aff_factory TYPE ('CL_AFF_FACTORY').
        CALL METHOD lo_aff_factory->('CREATE_LOG')
          RECEIVING
            result = lo_aff_log.

        CREATE DATA lr_intf_aff_log TYPE REF TO ('IF_AFF_LOG').
        ASSIGN lr_intf_aff_log->* TO <ls_intf_aff_log>.
        <ls_intf_aff_log> ?= lo_aff_log.

        CALL METHOD lo_object_handler->('IF_AFF_OBJECT_HANDLER~DELETE')
          EXPORTING
            object = <ls_intf_aff_obj>
            log    = <ls_intf_aff_log>.

        CREATE DATA lr_messages TYPE ('IF_AFF_LOG=>TT_LOG_OUT').
        ASSIGN lr_messages->* TO <ls_messages>.

        CALL METHOD lo_aff_log->('IF_AFF_LOG~GET_MESSAGES')
          RECEIVING
            messages = <ls_messages>.

        LOOP AT <ls_messages> ASSIGNING <ls_message>.
          ASSIGN COMPONENT 'MESSAGE' OF STRUCTURE <ls_message> TO <ls_msg>.
          IF <ls_msg>-msgty = 'E'.
            zcx_abapgit_exception=>raise_t100(
              iv_msgid = <ls_msg>-msgid
              iv_msgno = <ls_msg>-msgno
              iv_msgv1 = <ls_msg>-msgv1
              iv_msgv2 = <ls_msg>-msgv2
              iv_msgv3 = <ls_msg>-msgv3
              iv_msgv4 = <ls_msg>-msgv4 ).
          ENDIF.
        ENDLOOP.

        tadir_delete( ).

      CATCH cx_root INTO lx_error.
        zcx_abapgit_exception=>raise_with_text( lx_error ).
    ENDTRY.
  ENDMETHOD.


  METHOD zif_abapgit_object~deserialize.

    DATA: lr_intf_aff_obj          TYPE REF TO data,
          lr_intf_aff_file         TYPE REF TO data,
          lr_intf_files_container  TYPE REF TO data,
          lr_intf_aff_log          TYPE REF TO data,
          lr_intf_aff_settings     TYPE REF TO data,
          lo_object_handler        TYPE REF TO object,
          lo_object_aff            TYPE REF TO object,
          lo_object_json_file      TYPE REF TO object,
          lo_object_file           TYPE REF TO object,
          lo_files_container       TYPE REF TO object,
          lo_settings              TYPE REF TO object,
          lo_aff_log               TYPE REF TO object,
          lo_aff_factory           TYPE REF TO object,
          lr_messages              TYPE REF TO data,
          lv_json_as_xstring       TYPE xstring,
          lx_exception             TYPE REF TO cx_root,
          lv_file_name             TYPE string,
          lo_file_name_mapper      TYPE REF TO object,
          lv_name                  TYPE c LENGTH 120,
          lv_file_as_xstring       TYPE xstring,
          ls_additional_extensions TYPE ty_extension_mapper_pairs.

    FIELD-SYMBOLS: <ls_intf_aff_obj>          TYPE any,
                   <ls_intf_aff_file>         TYPE any,
                   <ls_intf_files_container>  TYPE any,
                   <ls_intf_aff_log>          TYPE any,
                   <ls_intf_aff_settings>     TYPE any,
                   <ls_messages>              TYPE ANY TABLE,
                   <ls_message>               TYPE any,
                   <ls_text>                  TYPE any,
                   <ls_type>                  TYPE any,
                   <ls_msg>                   TYPE symsg,
                   <ls_extension_mapper_pair> LIKE LINE OF ls_additional_extensions.

    lv_json_as_xstring = mo_files->read_raw( 'json' ).
    lv_name = ms_item-obj_name.

    " beyond here there will be dragons....
    TRY.
        lo_object_handler = get_object_handler( ).

        CREATE OBJECT lo_object_aff TYPE ('CL_AFF_OBJ')
          EXPORTING
            package = ms_item-devclass
            name    = lv_name
            type    = ms_item-obj_type.

        CREATE DATA lr_intf_aff_obj TYPE REF TO ('IF_AFF_OBJ').
        ASSIGN lr_intf_aff_obj->* TO <ls_intf_aff_obj>.
        <ls_intf_aff_obj> ?= lo_object_aff.

        CREATE OBJECT lo_files_container TYPE ('CL_AFF_FILES_CONTAINER')
          EXPORTING
            object = <ls_intf_aff_obj>.

        CALL METHOD ('CL_AFF_FILE_NAME_MAPPER')=>for_json
          RECEIVING
            result = lo_file_name_mapper.

        CALL METHOD lo_file_name_mapper->('IF_AFF_FILE_NAME_MAPPER~GET_FILE_NAME_FROM_OBJECT')
          EXPORTING
            object = <ls_intf_aff_obj>
          RECEIVING
            result = lv_file_name.

        lo_settings = create_aff_setting_deserialize( ).

        CREATE OBJECT lo_object_json_file TYPE ('CL_AFF_FILE')
          EXPORTING
            name    = lv_file_name
            content = lv_json_as_xstring.

        CREATE DATA lr_intf_aff_file TYPE REF TO ('IF_AFF_FILE').
        ASSIGN lr_intf_aff_file->* TO <ls_intf_aff_file>.
        <ls_intf_aff_file> ?= lo_object_json_file.

        CALL METHOD lo_files_container->('ADD_FILE')
          EXPORTING
            file = <ls_intf_aff_file>.

        ls_additional_extensions = get_additional_extensions( ).

        LOOP AT ls_additional_extensions ASSIGNING <ls_extension_mapper_pair>.

          lv_file_as_xstring = mo_files->read_raw( <ls_extension_mapper_pair>-extension ).

          CALL METHOD <ls_extension_mapper_pair>-file_name_mapper->('IF_AFF_FILE_NAME_MAPPER~GET_FILE_NAME_FROM_OBJECT')
            EXPORTING
              object = <ls_intf_aff_obj>
            RECEIVING
              result = lv_file_name.

          CREATE OBJECT lo_object_file TYPE ('CL_AFF_FILE')
          EXPORTING
            name    = lv_file_name
            content = lv_file_as_xstring.

          CREATE DATA lr_intf_aff_file TYPE REF TO ('IF_AFF_FILE').
          ASSIGN lr_intf_aff_file->* TO <ls_intf_aff_file>.
          <ls_intf_aff_file> ?= lo_object_file.

          CALL METHOD lo_files_container->('ADD_FILE')
            EXPORTING
              file = <ls_intf_aff_file>.

        ENDLOOP.

        CREATE OBJECT lo_aff_factory TYPE ('CL_AFF_FACTORY').
        CALL METHOD lo_aff_factory->('CREATE_LOG')
          RECEIVING
            result = lo_aff_log.

        CREATE DATA lr_intf_files_container TYPE REF TO ('IF_AFF_FILES_CONTAINER').
        ASSIGN lr_intf_files_container->* TO <ls_intf_files_container>.
        <ls_intf_files_container> ?= lo_files_container.

        CREATE DATA lr_intf_aff_log TYPE REF TO ('IF_AFF_LOG').
        ASSIGN lr_intf_aff_log->* TO <ls_intf_aff_log>.
        <ls_intf_aff_log> ?= lo_aff_log.

        CREATE DATA lr_intf_aff_settings TYPE REF TO ('IF_AFF_SETTINGS_DESERIALIZE').
        ASSIGN lr_intf_aff_settings->* TO <ls_intf_aff_settings>.
        <ls_intf_aff_settings> ?= lo_settings.

        CALL METHOD lo_object_handler->('IF_AFF_OBJECT_HANDLER~DESERIALIZE')
          EXPORTING
            files_container = <ls_intf_files_container>
            log             = <ls_intf_aff_log>
            settings        = <ls_intf_aff_settings>.

        CREATE DATA lr_messages TYPE ('IF_AFF_LOG=>TT_LOG_OUT').
        ASSIGN lr_messages->* TO <ls_messages>.

        CALL METHOD lo_aff_log->('IF_AFF_LOG~GET_MESSAGES')
          RECEIVING
            messages = <ls_messages>.

        LOOP AT <ls_messages> ASSIGNING <ls_message>.
          ASSIGN COMPONENT 'MESSAGE' OF STRUCTURE <ls_message> TO <ls_msg>.
          ASSIGN COMPONENT 'TEXT' OF STRUCTURE <ls_message> TO <ls_text>.
          ASSIGN COMPONENT 'TYPE' OF STRUCTURE <ls_message> TO <ls_type>.
          ii_log->add(
            iv_msg  = <ls_text>
            iv_type = <ls_type>
            is_item = ms_item ).

          IF <ls_msg>-msgty = 'E'.
            zcx_abapgit_exception=>raise_t100(
              iv_msgid = <ls_msg>-msgid
              iv_msgno = <ls_msg>-msgno
              iv_msgv1 = <ls_msg>-msgv1
              iv_msgv2 = <ls_msg>-msgv2
              iv_msgv3 = <ls_msg>-msgv3
              iv_msgv4 = <ls_msg>-msgv4 ).
          ENDIF.
        ENDLOOP.

        IF is_active( ) = abap_false.
          " as DDIC-object e.g. are not deserialized in active state, activation must be performed
          zcl_abapgit_objects_activation=>add_item( ms_item ).
        ENDIF.

        tadir_insert( ms_item-devclass ).

      CATCH cx_root INTO lx_exception.
        ii_log->add_error( is_item = ms_item
                           iv_msg  = 'Error at deserialize' ).
        ii_log->add_exception(
          ix_exc  = lx_exception
          is_item = ms_item ).
    ENDTRY.
  ENDMETHOD.


  METHOD zif_abapgit_object~exists.
    DATA: lr_intf_aff_obj   TYPE REF TO data,
          lo_object_handler TYPE REF TO object,
          lo_object_aff     TYPE REF TO object,
          lv_name           TYPE c LENGTH 120,
          lx_error          TYPE REF TO cx_root.

    FIELD-SYMBOLS: <ls_intf_aff_obj> TYPE any.

    lv_name = ms_item-obj_name.

    TRY.
        lo_object_handler = get_object_handler( ).

        CREATE OBJECT lo_object_aff TYPE ('CL_AFF_OBJ')
           EXPORTING
             package = ms_item-devclass
             name    = lv_name
             type    = ms_item-obj_type.

        CREATE DATA lr_intf_aff_obj TYPE REF TO ('IF_AFF_OBJ').
        ASSIGN lr_intf_aff_obj->* TO <ls_intf_aff_obj>.
        <ls_intf_aff_obj> ?= lo_object_aff.

        CALL METHOD lo_object_handler->('IF_AFF_OBJECT_HANDLER~EXISTS')
          EXPORTING
            object = <ls_intf_aff_obj>
          RECEIVING
            result = rv_bool.

      CATCH cx_root INTO lx_error.
        " return false instead of raising exception, because abapGit assumes
        " that raising exception = existing object
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
    DATA lv_lock_object   TYPE string.
    DATA lv_argument      TYPE seqg3-garg.

    lv_lock_object = |{ ms_item-obj_type }{ ms_item-obj_name }*|.
    lv_argument  = lv_lock_object.

    rv_is_locked = exists_a_lock_entry_for( iv_lock_object = 'ESWB_EO'
                                            iv_argument    = lv_argument ).
  ENDMETHOD.


  METHOD zif_abapgit_object~jump.
    RETURN.
  ENDMETHOD.


  METHOD zif_abapgit_object~map_filename_to_object.
    RETURN.
  ENDMETHOD.


  METHOD zif_abapgit_object~map_object_to_filename.
    RETURN.
  ENDMETHOD.


  METHOD zif_abapgit_object~serialize.
    DATA: lr_intf_aff_obj           TYPE REF TO data,
          lr_intf_aff_log           TYPE REF TO data,
          lr_intf_aff_settings      TYPE REF TO data,
          lr_messages               TYPE REF TO data,
          lo_object_handler         TYPE REF TO object,
          lo_object_aff             TYPE REF TO object,
          lo_object_json_file       TYPE REF TO object,
          lo_files_container        TYPE REF TO object,
          lo_settings               TYPE REF TO object,
          lo_aff_log                TYPE REF TO object,
          lo_aff_factory            TYPE REF TO object,
          lo_object_file            TYPE REF TO object,
          lv_json_as_xstring        TYPE xstring,
          lv_json_as_xstring_wo_alv TYPE xstring,
          lx_exception              TYPE REF TO cx_root,
          lv_name                   TYPE c LENGTH 120,
          lv_file_name              TYPE string,
          lo_file_name_mapper       TYPE REF TO object,
          ls_additional_extensions  TYPE ty_extension_mapper_pairs,
          lv_file_as_xstring        TYPE xstring.

    FIELD-SYMBOLS: <ls_intf_aff_obj>          TYPE any,
                   <ls_intf_aff_log>          TYPE any,
                   <ls_intf_aff_settings>     TYPE any,
                   <ls_messages>              TYPE ANY TABLE,
                   <ls_message>               TYPE any,
                   <ls_msg>                   TYPE symsg,
                   <ls_extension_mapper_pair> LIKE LINE OF ls_additional_extensions.

    lv_name = ms_item-obj_name.

    TRY.
        lo_object_handler = get_object_handler( ).

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
        ASSIGN lr_intf_aff_log->* TO <ls_intf_aff_log>.
        <ls_intf_aff_log> ?= lo_aff_log.

        CREATE DATA lr_intf_aff_settings TYPE REF TO ('IF_AFF_SETTINGS_SERIALIZE').
        ASSIGN lr_intf_aff_settings->* TO <ls_intf_aff_settings>.
        <ls_intf_aff_settings> ?= lo_settings.

        CREATE DATA lr_intf_aff_obj TYPE REF TO ('IF_AFF_OBJ').
        ASSIGN lr_intf_aff_obj->* TO <ls_intf_aff_obj>.
        <ls_intf_aff_obj> ?= lo_object_aff.

        CALL METHOD lo_object_handler->('IF_AFF_OBJECT_HANDLER~SERIALIZE')
          EXPORTING
            object   = <ls_intf_aff_obj>
            log      = <ls_intf_aff_log>
            settings = <ls_intf_aff_settings>
          RECEIVING
            result   = lo_files_container.

        CREATE DATA lr_messages TYPE ('IF_AFF_LOG=>TT_LOG_OUT').
        ASSIGN lr_messages->* TO <ls_messages>.

        CALL METHOD lo_aff_log->('IF_AFF_LOG~GET_MESSAGES')
          RECEIVING
            messages = <ls_messages>.

        LOOP AT <ls_messages> ASSIGNING <ls_message>.
          ASSIGN COMPONENT 'MESSAGE' OF STRUCTURE <ls_message> TO <ls_msg>.
          IF <ls_msg>-msgty = 'E'.
            zcx_abapgit_exception=>raise_t100(
              iv_msgid = <ls_msg>-msgid
              iv_msgno = <ls_msg>-msgno
              iv_msgv1 = <ls_msg>-msgv1
              iv_msgv2 = <ls_msg>-msgv2
              iv_msgv3 = <ls_msg>-msgv3
              iv_msgv4 = <ls_msg>-msgv4 ).
          ENDIF.
        ENDLOOP.

        CALL METHOD ('CL_AFF_FILE_NAME_MAPPER')=>for_json
          RECEIVING
            result = lo_file_name_mapper.

        CALL METHOD lo_file_name_mapper->('IF_AFF_FILE_NAME_MAPPER~GET_FILE_NAME_FROM_OBJECT')
          EXPORTING
            object = <ls_intf_aff_obj>
          RECEIVING
            result = lv_file_name.

        CALL METHOD lo_files_container->('IF_AFF_FILES_CONTAINER~GET_FILE')
          EXPORTING
            name   = lv_file_name
          RECEIVING
            result = lo_object_json_file.

        " avoid to serialize empty content (object was never activated, exists inactive only).
        IF is_file_empty( lo_object_json_file ) = abap_true.
          MESSAGE s821(eu) WITH lv_name INTO zcx_abapgit_exception=>null.
          zcx_abapgit_exception=>raise_t100( ).
        ENDIF.

        CALL METHOD lo_object_json_file->('IF_AFF_FILE~GET_CONTENT')
          RECEIVING
            result = lv_json_as_xstring.

        " Only remove ABAP language version if repository is set to ignore it
        IF ms_item-abap_language_version = zcl_abapgit_abap_language_vers=>c_no_abap_language_version.
          lv_json_as_xstring_wo_alv = remove_abap_language_version( lv_json_as_xstring ).
          mo_files->add_raw(
            iv_ext  = 'json'
            iv_data = lv_json_as_xstring_wo_alv ).
        ELSE.
          mo_files->add_raw(
            iv_ext  = 'json'
            iv_data = lv_json_as_xstring ).
        ENDIF.

        ls_additional_extensions = get_additional_extensions( ).

        LOOP AT ls_additional_extensions ASSIGNING <ls_extension_mapper_pair>.

          CALL METHOD <ls_extension_mapper_pair>-file_name_mapper->('IF_AFF_FILE_NAME_MAPPER~GET_FILE_NAME_FROM_OBJECT')
            EXPORTING
              object = <ls_intf_aff_obj>
            RECEIVING
              result = lv_file_name.

          CALL METHOD lo_files_container->('IF_AFF_FILES_CONTAINER~GET_FILE')
            EXPORTING
              name   = lv_file_name
            RECEIVING
              result = lo_object_file.

          CALL METHOD lo_object_file->('IF_AFF_FILE~GET_CONTENT')
            RECEIVING
              result = lv_file_as_xstring.

          mo_files->add_raw(
            iv_ext  = <ls_extension_mapper_pair>-extension
            iv_data = lv_file_as_xstring ).

        ENDLOOP.

      CATCH cx_root INTO lx_exception.
        zcx_abapgit_exception=>raise_with_text( lx_exception ).
    ENDTRY.
  ENDMETHOD.
ENDCLASS.
