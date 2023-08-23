    CLASS zcl_abapgit_objects_api DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

      PUBLIC SECTION.

        TYPES: BEGIN OF ty_web_file,
                 path        TYPE string,
                 filename    TYPE string,
                 base64_data TYPE string,
               END OF ty_web_file.

        TYPES: BEGIN OF ty_web_file_item,
                 web_file TYPE ty_web_file,
                 item     TYPE zif_abapgit_definitions=>ty_item,
               END OF ty_web_file_item.

        TYPES ty_web_files_item_tt TYPE STANDARD TABLE OF ty_web_file_item WITH DEFAULT KEY .

        CLASS-METHODS serialize_web_objects
          IMPORTING
            iv_top_package    TYPE devclass
            is_dot_data       TYPE zif_abapgit_dot_abapgit=>ty_dot_abapgit OPTIONAL
            it_objs           TYPE zif_abapgit_definitions=>ty_obj_tt
          EXPORTING
            ei_log            TYPE REF TO zif_abapgit_log
            et_web_files_item TYPE ty_web_files_item_tt
          RAISING
            zcx_abapgit_exception .

        CLASS-METHODS serialize_objects
          IMPORTING
            iv_top_package TYPE devclass
            is_dot_data    TYPE zif_abapgit_dot_abapgit=>ty_dot_abapgit OPTIONAL
            it_objs        TYPE zif_abapgit_definitions=>ty_obj_tt
          EXPORTING
            ei_log         TYPE REF TO zif_abapgit_log
            et_files       TYPE zif_abapgit_definitions=>ty_files_item_tt
          RAISING
            zcx_abapgit_exception .

        CLASS-METHODS deserialize_objects
          IMPORTING iv_top_package        TYPE devclass
                    is_dot_data           TYPE zif_abapgit_dot_abapgit=>ty_dot_abapgit OPTIONAL
                    it_remote             TYPE zif_abapgit_git_definitions=>ty_files_tt OPTIONAL
                    it_objs_del           TYPE zif_abapgit_definitions=>ty_obj_tt OPTIONAL
                    iv_transport          TYPE trkorr OPTIONAL
                    iv_conf_diff_wo_warn  TYPE abap_bool DEFAULT abap_false
                    iv_main_language_only TYPE abap_bool DEFAULT abap_false
          EXPORTING ei_log                TYPE REF TO zif_abapgit_log
                    et_accessed_files     TYPE zif_abapgit_git_definitions=>ty_file_signatures_tt
          RAISING   zcx_abapgit_exception.


      PROTECTED SECTION.
      PRIVATE SECTION.

        CLASS-METHODS get_dot
          IMPORTING is_dot_data   TYPE zif_abapgit_dot_abapgit=>ty_dot_abapgit OPTIONAL
          RETURNING VALUE(ro_dot) TYPE REF TO zcl_abapgit_dot_abapgit.


        CLASS-METHODS get_filter
          IMPORTING it_objs          TYPE zif_abapgit_definitions=>ty_obj_tt
          RETURNING VALUE(rt_filter) TYPE zif_abapgit_definitions=>ty_tadir_tt.


        CLASS-METHODS get_filter_deserialize
          IMPORTING it_objs_del      TYPE zif_abapgit_definitions=>ty_obj_tt OPTIONAL
                    it_remote        TYPE zif_abapgit_git_definitions=>ty_files_tt OPTIONAL
                    iv_top_package   TYPE devclass
                    io_dot           TYPE REF TO zcl_abapgit_dot_abapgit
          RETURNING VALUE(rt_filter) TYPE zif_abapgit_definitions=>ty_tadir_tt
          RAISING   zcx_abapgit_exception .

        CLASS-METHODS conv_files_to_web_files_item
          IMPORTING it_files                 TYPE zif_abapgit_definitions=>ty_files_item_tt
          RETURNING VALUE(rt_web_files_item) TYPE ty_web_files_item_tt
          RAISING   zcx_abapgit_exception .

        CLASS-METHODS delete_unnecessary_objects
          IMPORTING
            ii_log    TYPE REF TO zif_abapgit_log
            is_checks TYPE zif_abapgit_definitions=>ty_deserialize_checks
          RAISING
            zcx_abapgit_exception .

        CLASS-METHODS deserialize_objects_checks
          IMPORTING iv_top_package TYPE devclass
                    io_dot         TYPE REF TO zcl_abapgit_dot_abapgit
                    it_filter      TYPE zif_abapgit_definitions=>ty_tadir_tt
                    it_remote      TYPE zif_abapgit_git_definitions=>ty_files_tt OPTIONAL
                    iv_transport   TYPE trkorr OPTIONAL
                    ii_log         TYPE REF TO zif_abapgit_log
          EXPORTING es_checks      TYPE zif_abapgit_definitions=>ty_deserialize_checks
                    et_results     TYPE zif_abapgit_definitions=>ty_results_tt
          RAISING   zcx_abapgit_exception.

    ENDCLASS.


    CLASS zcl_abapgit_objects_api IMPLEMENTATION.

      METHOD serialize_objects.

        DATA lo_dot TYPE REF TO zcl_abapgit_dot_abapgit.
        DATA lt_filter TYPE zif_abapgit_definitions=>ty_tadir_tt.
        DATA lo_serialize TYPE REF TO zcl_abapgit_serialize.

        CLEAR ei_log.
        CLEAR et_files.

        IF iv_top_package IS INITIAL.
          zcx_abapgit_exception=>raise( |Please enter top package| ).
        ENDIF.

        lo_dot = get_dot( is_dot_data ).

        lt_filter = get_filter( it_objs = it_objs  ).

        CREATE OBJECT ei_log TYPE zcl_abapgit_log.

        CREATE OBJECT lo_serialize
          EXPORTING
            io_dot_abapgit = lo_dot.

        et_files =  lo_serialize->files_local(
          EXPORTING
            iv_package     = iv_top_package
            ii_log         = ei_log
            it_filter      = lt_filter ).

        "We want to handle only objects so delete the not relevant files
        DELETE et_files WHERE file-filename = zif_abapgit_definitions=>c_dot_abapgit
                           OR file-filename = zif_abapgit_apack_definitions=>c_dot_apack_manifest.
      ENDMETHOD.

      METHOD get_dot.
        "Get abapGit. Dot use default values if a field is not provided

        DATA ls_dot_new_data TYPE zif_abapgit_dot_abapgit=>ty_dot_abapgit.
        DATA lo_dot_default TYPE REF TO zcl_abapgit_dot_abapgit.
        lo_dot_default = zcl_abapgit_dot_abapgit=>build_default( ).

        ls_dot_new_data = lo_dot_default->get_data( ).

        IF NOT is_dot_data-master_language IS INITIAL.
          ls_dot_new_data-master_language = is_dot_data-master_language.
        ENDIF.

        ls_dot_new_data-use_lxe = is_dot_data-use_lxe.

        IF NOT is_dot_data-i18n_languages IS INITIAL.
          ls_dot_new_data-i18n_languages = is_dot_data-i18n_languages.
        ENDIF.

        IF NOT is_dot_data-starting_folder IS INITIAL.
          ls_dot_new_data-starting_folder = is_dot_data-starting_folder.
        ENDIF.

        IF NOT is_dot_data-folder_logic IS INITIAL.
          ls_dot_new_data-folder_logic = is_dot_data-folder_logic.
        ENDIF.

        IF NOT is_dot_data-ignore IS INITIAL.
          ls_dot_new_data-ignore = is_dot_data-ignore.
        ENDIF.

        IF NOT is_dot_data-requirements IS INITIAL.
          ls_dot_new_data-requirements = is_dot_data-requirements.
        ENDIF.

        IF NOT is_dot_data-version_constant IS INITIAL.
          ls_dot_new_data-version_constant = is_dot_data-version_constant.
        ENDIF.

        IF NOT is_dot_data-abap_language_version IS INITIAL.
          ls_dot_new_data-abap_language_version = is_dot_data-abap_language_version.
        ENDIF.

        CREATE OBJECT ro_dot
          EXPORTING
            is_data = ls_dot_new_data.

      ENDMETHOD.

      METHOD get_filter.
        DATA lr_obj TYPE REF TO  zif_abapgit_definitions=>ty_obj.
        DATA ls_filter TYPE zif_abapgit_definitions=>ty_tadir.

        LOOP AT it_objs REFERENCE INTO lr_obj.
          ls_filter-object = lr_obj->obj_type.
          ls_filter-obj_name = lr_obj->obj_name.
          INSERT ls_filter INTO TABLE rt_filter.
        ENDLOOP.

      ENDMETHOD.

      METHOD serialize_web_objects.
        DATA lt_files  TYPE zif_abapgit_definitions=>ty_files_item_tt.
        serialize_objects(
          EXPORTING
            iv_top_package = iv_top_package
            it_objs        = it_objs
            is_dot_data    = is_dot_data
          IMPORTING
            ei_log         = ei_log
            et_files       = lt_files ).

        et_web_files_item = conv_files_to_web_files_item( it_files = lt_files ).

      ENDMETHOD.

      METHOD conv_files_to_web_files_item.
        DATA lr_files TYPE REF TO zif_abapgit_definitions=>ty_file_item.
        DATA ls_web_file_item TYPE ty_web_file_item.
        DATA lv_data TYPE string.


        LOOP AT it_files REFERENCE INTO lr_files.
          CLEAR ls_web_file_item.
          ls_web_file_item-item = lr_files->item.
          ls_web_file_item-web_file-filename = lr_files->file-filename.
          ls_web_file_item-web_file-path = lr_files->file-path.
          lv_data = zcl_abapgit_convert=>xstring_to_string_utf8( lr_files->file-data ).

          cl_http_utility=>encode_base64( EXPORTING
                                            unencoded = lv_data
                                          RECEIVING
                                            encoded   = ls_web_file_item-web_file-base64_data ).

          INSERT ls_web_file_item INTO TABLE rt_web_files_item.

        ENDLOOP.

      ENDMETHOD.

      METHOD deserialize_objects.

        DATA lo_dot TYPE REF TO zcl_abapgit_dot_abapgit.
        DATA lt_filter TYPE zif_abapgit_definitions=>ty_tadir_tt.
        DATA ls_checks TYPE zif_abapgit_definitions=>ty_deserialize_checks.
        DATA lr_overwrite TYPE REF TO zif_abapgit_definitions=>ty_overwrite.
        DATA lt_results  TYPE zif_abapgit_definitions=>ty_results_tt.
        DATA lt_deserialized_objects TYPE zcl_abapgit_objects=>ty_deserialization_tt.

        IF iv_top_package IS INITIAL.
          zcx_abapgit_exception=>raise( |Please enter top package| ).
        ENDIF.

        lo_dot = get_dot( is_dot_data ).

        CREATE OBJECT ei_log TYPE zcl_abapgit_log.

        lt_filter = get_filter_deserialize(
            it_objs_del    = it_objs_del
            it_remote      = it_remote
            iv_top_package = iv_top_package
            io_dot         = lo_dot ).


        deserialize_objects_checks(
          EXPORTING
            iv_top_package = iv_top_package
            io_dot         = lo_dot
            it_filter      = lt_filter
            it_remote      = it_remote
            iv_transport   = iv_transport
            ii_log         = ei_log
          IMPORTING
            es_checks      = ls_checks
            et_results     = lt_results ).

        "We want to handle only objects so delete the not relevant files
        DELETE lt_results WHERE filename = zif_abapgit_definitions=>c_dot_abapgit
                           OR filename = zif_abapgit_apack_definitions=>c_dot_apack_manifest.

        "Set all overwrite decisions to true (without gui)
        LOOP AT ls_checks-overwrite REFERENCE INTO lr_overwrite.
          lr_overwrite->decision = zif_abapgit_definitions=>c_yes.
        ENDLOOP.

        "Delete unnecesary objects
        delete_unnecessary_objects(
          EXPORTING
            ii_log    = ei_log
            is_checks = ls_checks ).

        lt_results = zcl_abapgit_file_deserialize=>prioritize_deser(
          EXPORTING
            ii_log     = ei_log
            it_results = lt_results ).


        lt_deserialized_objects = zcl_abapgit_objects=>get_deserialized_objects(
            it_remote   = it_remote
            it_results  = lt_results ).

        zcl_abapgit_objects=>deserialize_central(
          EXPORTING
            iv_top_package    = iv_top_package
            iv_transport            = ls_checks-transport-transport
            iv_main_language_only   = iv_main_language_only
            io_dot                  = lo_dot
            ii_log                  = ei_log
            iv_conf_diff_wo_warn    = abap_true
          IMPORTING
            et_accessed_files       = et_accessed_files
          CHANGING
            ct_deserialized_objects = lt_deserialized_objects ).

      ENDMETHOD.

      METHOD delete_unnecessary_objects.

        DATA:
          ls_checks TYPE zif_abapgit_definitions=>ty_delete_checks,
          ls_tadir  TYPE zif_abapgit_definitions=>ty_tadir,
          lt_tadir  TYPE zif_abapgit_definitions=>ty_tadir_tt.

        FIELD-SYMBOLS <ls_overwrite> LIKE LINE OF is_checks-overwrite.

        " get confirmed deletions
        LOOP AT is_checks-overwrite ASSIGNING <ls_overwrite>
          WHERE ( action = zif_abapgit_objects=>c_deserialize_action-delete
          OR action = zif_abapgit_objects=>c_deserialize_action-delete_add )
          AND decision = zif_abapgit_definitions=>c_yes.

          ls_tadir-pgmid    = 'R3TR'.
          ls_tadir-object   = <ls_overwrite>-obj_type.
          ls_tadir-obj_name = <ls_overwrite>-obj_name.
          ls_tadir-devclass = <ls_overwrite>-devclass.
          INSERT ls_tadir INTO TABLE lt_tadir.

        ENDLOOP.

        " delete objects
        IF lines( lt_tadir ) > 0.
          ls_checks-transport = is_checks-transport.

          zcl_abapgit_objects=>delete( it_tadir  = lt_tadir
                                       is_checks = ls_checks
                                       ii_log    = ii_log ).

        ENDIF.

      ENDMETHOD.

      METHOD get_filter_deserialize.
        DATA lt_objs  TYPE zif_abapgit_definitions=>ty_obj_tt.
        DATA ls_obj TYPE zif_abapgit_definitions=>ty_obj.
        DATA ls_item  TYPE zif_abapgit_definitions=>ty_item.
        DATA lr_remote  TYPE REF TO  zif_abapgit_git_definitions=>ty_file.

        lt_objs = it_objs_del.

        "Get Objects from remote files
        LOOP AT it_remote REFERENCE INTO lr_remote.

          zcl_abapgit_filename_logic=>file_to_object(
                EXPORTING
                  iv_filename = lr_remote->filename
                  iv_path     = lr_remote->path
                  io_dot      = io_dot
                  iv_devclass = iv_top_package
                IMPORTING
                  es_item     = ls_item ).

          ls_obj-obj_name = ls_item-obj_name.
          ls_obj-obj_type = ls_item-obj_type.

          INSERT ls_obj INTO TABLE lt_objs.

        ENDLOOP.

        SORT lt_objs.
        DELETE ADJACENT DUPLICATES FROM lt_objs.
        IF lt_objs IS INITIAL.
          zcx_abapgit_exception=>raise( |Please enter the objects to delete or the modified files| ).
        ENDIF.

        "Use filter to delete only relevant objects
        rt_filter = get_filter( it_objs = lt_objs  ).

      ENDMETHOD.

      METHOD deserialize_objects_checks.
        DATA lt_requirements TYPE zif_abapgit_dot_abapgit=>ty_requirement_tt.

        zcl_abapgit_objects_check=>deserialize_checks_wo_repo(
               EXPORTING
                 iv_top_package = iv_top_package
                 io_dot         = io_dot
                 it_remote      = it_remote
                 it_filter      = it_filter
                 iv_transport   = iv_transport
                 ii_log         = ii_log
               IMPORTING
                 es_checks      = es_checks
                 et_results     = et_results ).

        IF es_checks-overwrite IS INITIAL.
          zcx_abapgit_exception=>raise(
            'There is nothing to deserialize. The local state completely matches the remote repository.' ).
        ENDIF.

        lt_requirements = io_dot->get_data( )-requirements.
        es_checks-requirements-met = zcl_abapgit_requirement_helper=>is_requirements_met( lt_requirements ).

        IF es_checks-requirements-met = zif_abapgit_definitions=>c_no.
          zcx_abapgit_exception=>raise( 'Requirements not met' ).
        ENDIF.

        IF es_checks-transport-required = abap_true AND es_checks-transport-transport IS INITIAL.
          zcx_abapgit_exception=>raise( |No transport request was supplied| ).
        ENDIF.
      ENDMETHOD.

    ENDCLASS.
