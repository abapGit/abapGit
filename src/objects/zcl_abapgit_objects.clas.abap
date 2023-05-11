CLASS zcl_abapgit_objects DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES:
      ty_types_tt TYPE SORTED TABLE OF tadir-object WITH UNIQUE KEY table_line .
    TYPES:
      BEGIN OF ty_serialization,
        files TYPE zif_abapgit_git_definitions=>ty_files_tt,
        item  TYPE zif_abapgit_definitions=>ty_item,
      END OF ty_serialization .

    CLASS-METHODS serialize
      IMPORTING
        !is_item                 TYPE zif_abapgit_definitions=>ty_item
        !iv_language             TYPE spras
        !iv_main_language_only   TYPE abap_bool DEFAULT abap_false
        !it_translation_langs    TYPE zif_abapgit_definitions=>ty_languages OPTIONAL
        !iv_use_lxe              TYPE abap_bool DEFAULT abap_false
      RETURNING
        VALUE(rs_files_and_item) TYPE ty_serialization
      RAISING
        zcx_abapgit_exception .
    CLASS-METHODS deserialize
      IMPORTING
        !io_repo                 TYPE REF TO zcl_abapgit_repo
        !is_checks               TYPE zif_abapgit_definitions=>ty_deserialize_checks
        !ii_log                  TYPE REF TO zif_abapgit_log
      RETURNING
        VALUE(rt_accessed_files) TYPE zif_abapgit_git_definitions=>ty_file_signatures_tt
      RAISING
        zcx_abapgit_exception .
    CLASS-METHODS deserialize_checks
      IMPORTING
        !io_repo         TYPE REF TO zcl_abapgit_repo
      RETURNING
        VALUE(rs_checks) TYPE zif_abapgit_definitions=>ty_deserialize_checks
      RAISING
        zcx_abapgit_exception .
    CLASS-METHODS delete
      IMPORTING
        !it_tadir  TYPE zif_abapgit_definitions=>ty_tadir_tt
        !is_checks TYPE zif_abapgit_definitions=>ty_delete_checks OPTIONAL
        !ii_log    TYPE REF TO zif_abapgit_log OPTIONAL
      RAISING
        zcx_abapgit_exception .
    CLASS-METHODS jump
      IMPORTING
        !is_item        TYPE zif_abapgit_definitions=>ty_item
        !is_sub_item    TYPE zif_abapgit_definitions=>ty_item OPTIONAL
        !iv_filename    TYPE string OPTIONAL
        !iv_line_number TYPE i OPTIONAL
      RAISING
        zcx_abapgit_exception .
    CLASS-METHODS changed_by
      IMPORTING
        !is_item       TYPE zif_abapgit_definitions=>ty_item
        !is_sub_item   TYPE zif_abapgit_definitions=>ty_item OPTIONAL
        !iv_filename   TYPE string OPTIONAL
      RETURNING
        VALUE(rv_user) TYPE syuname .
    CLASS-METHODS is_supported
      IMPORTING
        !is_item        TYPE zif_abapgit_definitions=>ty_item
        !iv_native_only TYPE abap_bool DEFAULT abap_false
      RETURNING
        VALUE(rv_bool)  TYPE abap_bool .
    CLASS-METHODS is_type_supported
      IMPORTING
        !iv_obj_type   TYPE zif_abapgit_definitions=>ty_item-obj_type
      RETURNING
        VALUE(rv_bool) TYPE abap_bool .
    CLASS-METHODS exists
      IMPORTING
        !is_item       TYPE zif_abapgit_definitions=>ty_item
      RETURNING
        VALUE(rv_bool) TYPE abap_bool .
    CLASS-METHODS supported_list
      RETURNING
        VALUE(rt_types) TYPE ty_types_tt .
    CLASS-METHODS is_active
      IMPORTING
        !is_item         TYPE zif_abapgit_definitions=>ty_item
      RETURNING
        VALUE(rv_active) TYPE abap_bool
      RAISING
        zcx_abapgit_exception .
  PROTECTED SECTION.

  PRIVATE SECTION.

    TYPES:
      BEGIN OF ty_supported_types,
        obj_type  TYPE tadir-object,
        supported TYPE abap_bool,
      END OF ty_supported_types.

    TYPES: ty_supported_types_tt TYPE SORTED TABLE OF ty_supported_types WITH UNIQUE KEY obj_type.

    TYPES:
      BEGIN OF ty_obj_serializer_item,
        item     TYPE zif_abapgit_definitions=>ty_item,
        metadata TYPE zif_abapgit_definitions=>ty_metadata,
      END OF ty_obj_serializer_item .
    TYPES:
      ty_obj_serializer_map TYPE SORTED TABLE OF ty_obj_serializer_item WITH UNIQUE KEY item .

    CLASS-DATA gt_obj_serializer_map TYPE ty_obj_serializer_map .
    CLASS-DATA gt_supported_obj_types TYPE ty_supported_types_tt .
    CLASS-DATA gv_supported_obj_types_loaded TYPE abap_bool .

    CLASS-METHODS check_duplicates
      IMPORTING
        !it_files TYPE zif_abapgit_git_definitions=>ty_files_tt
      RAISING
        zcx_abapgit_exception .
    CLASS-METHODS class_name
      IMPORTING
        !is_item             TYPE zif_abapgit_definitions=>ty_item
      RETURNING
        VALUE(rv_class_name) TYPE string .
    CLASS-METHODS update_package_tree
      IMPORTING
        !iv_package TYPE devclass .
    CLASS-METHODS delete_object
      IMPORTING
        !iv_package   TYPE devclass
        !is_item      TYPE zif_abapgit_definitions=>ty_item
        !iv_transport TYPE trkorr
      RAISING
        zcx_abapgit_exception .
    CLASS-METHODS compare_remote_to_local
      IMPORTING
        !ii_object TYPE REF TO zif_abapgit_object
        !it_remote TYPE zif_abapgit_git_definitions=>ty_files_tt
        !is_result TYPE zif_abapgit_definitions=>ty_result
        !ii_log    TYPE REF TO zif_abapgit_log
      RAISING
        zcx_abapgit_exception .
    CLASS-METHODS deserialize_steps
      IMPORTING
        !it_steps     TYPE zif_abapgit_objects=>ty_step_data_tt
        !ii_log       TYPE REF TO zif_abapgit_log
        !iv_transport TYPE trkorr
      CHANGING
        !ct_files     TYPE zif_abapgit_git_definitions=>ty_file_signatures_tt
      RAISING
        zcx_abapgit_exception .
    CLASS-METHODS deserialize_objects
      IMPORTING
        !is_step      TYPE zif_abapgit_objects=>ty_step_data
        !ii_log       TYPE REF TO zif_abapgit_log
        !iv_transport TYPE trkorr
      CHANGING
        !ct_files     TYPE zif_abapgit_git_definitions=>ty_file_signatures_tt
      RAISING
        zcx_abapgit_exception .
    CLASS-METHODS check_objects_locked
      IMPORTING
        !iv_language TYPE spras
        !it_items    TYPE zif_abapgit_definitions=>ty_items_tt
      RAISING
        zcx_abapgit_exception .
    CLASS-METHODS create_object
      IMPORTING
        !is_item        TYPE zif_abapgit_definitions=>ty_item
        !iv_language    TYPE spras
        !is_metadata    TYPE zif_abapgit_definitions=>ty_metadata OPTIONAL
        !iv_native_only TYPE abap_bool DEFAULT abap_false
      RETURNING
        VALUE(ri_obj)   TYPE REF TO zif_abapgit_object
      RAISING
        zcx_abapgit_exception .
    CLASS-METHODS map_tadir_to_items
      IMPORTING
        !it_tadir       TYPE zif_abapgit_definitions=>ty_tadir_tt
      RETURNING
        VALUE(rt_items) TYPE zif_abapgit_definitions=>ty_items_tt .
    CLASS-METHODS map_results_to_items
      IMPORTING
        !it_results     TYPE zif_abapgit_definitions=>ty_results_tt
      RETURNING
        VALUE(rt_items) TYPE zif_abapgit_definitions=>ty_items_tt .
    CLASS-METHODS get_deserialize_steps
      RETURNING
        VALUE(rt_steps) TYPE zif_abapgit_objects=>ty_step_data_tt .
    CLASS-METHODS check_main_package
      IMPORTING
        !iv_package  TYPE devclass
        !iv_obj_type TYPE tadir-object
      RAISING
        zcx_abapgit_exception .
    CLASS-METHODS change_package_assignments
      IMPORTING
        !is_item TYPE zif_abapgit_definitions=>ty_item
        !ii_log  TYPE REF TO zif_abapgit_log .
    CLASS-METHODS determine_i18n_params
      IMPORTING
        !io_dot                TYPE REF TO zcl_abapgit_dot_abapgit
        !iv_main_language_only TYPE abap_bool
      RETURNING
        VALUE(rs_i18n_params)  TYPE zif_abapgit_definitions=>ty_i18n_params
      RAISING
        zcx_abapgit_exception.
    CLASS-METHODS get_extra_from_filename
      IMPORTING
        !iv_filename    TYPE string
      RETURNING
        VALUE(rv_extra) TYPE string.
ENDCLASS.



CLASS zcl_abapgit_objects IMPLEMENTATION.


  METHOD changed_by.

    DATA: li_obj TYPE REF TO zif_abapgit_object.

    " For unsupported objects, return empty string
    IF is_type_supported( is_item-obj_type ) = abap_false.
      RETURN.
    ENDIF.

    TRY.
        li_obj = create_object( is_item     = is_item
                                iv_language = zif_abapgit_definitions=>c_english ).

        rv_user = li_obj->changed_by( get_extra_from_filename( iv_filename ) ).
      CATCH zcx_abapgit_exception ##NO_HANDLER.
        " Ignore errors
    ENDTRY.

    IF rv_user IS INITIAL.
      " Eg. ".abapgit.xml" file
      rv_user = zcl_abapgit_objects_super=>c_user_unknown.
    ENDIF.

  ENDMETHOD.


  METHOD change_package_assignments.

    CALL FUNCTION 'TR_TADIR_INTERFACE'
      EXPORTING
        wi_tadir_pgmid    = 'R3TR'
        wi_tadir_object   = is_item-obj_type
        wi_tadir_obj_name = is_item-obj_name
        wi_tadir_devclass = is_item-devclass
        wi_test_modus     = abap_false
      EXCEPTIONS
        OTHERS            = 1.
    IF sy-subrc = 0.
      ii_log->add_success( iv_msg  = |Object { is_item-obj_name } assigned to package { is_item-devclass }|
                           is_item = is_item ).
    ELSE.
      ii_log->add_error( iv_msg  = |Package change of object { is_item-obj_name } failed|
                         is_item = is_item ).
    ENDIF.

  ENDMETHOD.


  METHOD check_duplicates.

    DATA: lt_files          TYPE zif_abapgit_git_definitions=>ty_files_tt,
          lv_path           TYPE string,
          lv_filename       TYPE string,
          lt_duplicates     TYPE STANDARD TABLE OF string WITH DEFAULT KEY,
          lv_duplicates     LIKE LINE OF lt_duplicates,
          lv_all_duplicates TYPE string.

    FIELD-SYMBOLS: <ls_file> LIKE LINE OF it_files.

    lt_files = it_files.
    SORT lt_files BY path ASCENDING filename ASCENDING.

    LOOP AT lt_files ASSIGNING <ls_file>.
      IF lv_path = <ls_file>-path AND lv_filename = <ls_file>-filename.
        CONCATENATE <ls_file>-path <ls_file>-filename INTO lv_duplicates.
        APPEND lv_duplicates TO lt_duplicates.
      ENDIF.
      lv_path = <ls_file>-path.
      lv_filename = <ls_file>-filename.
    ENDLOOP.

    IF lt_duplicates IS NOT INITIAL.
      CONCATENATE LINES OF lt_duplicates INTO lv_all_duplicates SEPARATED BY `, `.
      zcx_abapgit_exception=>raise( |Duplicates: { lv_all_duplicates }| ).
    ENDIF.

  ENDMETHOD.


  METHOD check_main_package.

    " check package restrictions, closed package, descriptive or
    " functional package
    cl_pak_object_types=>check_object_type(
      EXPORTING
        i_working_mode         = 'I'
        i_package_name         = iv_package
        i_pgmid                = 'R3TR'
        i_object_type          = iv_obj_type
      EXCEPTIONS
        wrong_object_type      = 1
        package_not_extensible = 2
        package_not_loaded     = 3
        OTHERS                 = 4 ).
    CASE sy-subrc.
      WHEN 0.
        RETURN.
      WHEN 2.
        zcx_abapgit_exception=>raise( |Object type { iv_obj_type } not allowed for package { iv_package }| ).
      WHEN OTHERS.
        zcx_abapgit_exception=>raise_t100(  ).
    ENDCASE.

  ENDMETHOD.


  METHOD check_objects_locked.

    DATA: li_obj TYPE REF TO zif_abapgit_object.

    FIELD-SYMBOLS: <ls_item> LIKE LINE OF it_items.

    LOOP AT it_items ASSIGNING <ls_item>.

      " You should remember that we ignore not supported objects here,
      " because otherwise the process aborts which is not desired
      IF is_type_supported( <ls_item>-obj_type ) = abap_false.
        CONTINUE.
      ENDIF.

      li_obj = create_object( is_item     = <ls_item>
                              iv_language = iv_language ).

      IF li_obj->is_locked( ) = abap_true.
        zcx_abapgit_exception=>raise( |Object { <ls_item>-obj_type } { <ls_item>-obj_name } |
                                   && |is locked. Action not possible.| ).
      ENDIF.

    ENDLOOP.

  ENDMETHOD.


  METHOD class_name.

    CONCATENATE 'ZCL_ABAPGIT_OBJECT_' is_item-obj_type INTO rv_class_name.

  ENDMETHOD.


  METHOD compare_remote_to_local.
* this method is used for comparing local with remote objects
* before pull, this is useful eg. when overwriting a TABL object.
* only the main XML file is used for comparison

    DATA: ls_remote_file    TYPE zif_abapgit_git_definitions=>ty_file,
          li_remote_version TYPE REF TO zif_abapgit_xml_input,
          lv_count          TYPE i,
          ls_result         TYPE zif_abapgit_comparator=>ty_result,
          lv_answer         TYPE string,
          li_comparator     TYPE REF TO zif_abapgit_comparator,
          ls_item           TYPE zif_abapgit_definitions=>ty_item.

    FIND ALL OCCURRENCES OF '.' IN is_result-filename MATCH COUNT lv_count.

    IF is_result-filename CS '.XML' AND lv_count = 2.
      IF ii_object->exists( ) = abap_false.
        RETURN.
      ENDIF.

      READ TABLE it_remote WITH KEY file
        COMPONENTS filename = is_result-filename INTO ls_remote_file.
      IF sy-subrc <> 0. "if file does not exist in remote, we don't need to validate
        RETURN.
      ENDIF.

      li_comparator = ii_object->get_comparator( ).
      IF NOT li_comparator IS BOUND.
        RETURN.
      ENDIF.

      CREATE OBJECT li_remote_version
        TYPE zcl_abapgit_xml_input
        EXPORTING
          iv_xml      = zcl_abapgit_convert=>xstring_to_string_utf8( ls_remote_file-data )
          iv_filename = ls_remote_file-filename.

      ls_result = li_comparator->compare( ii_remote = li_remote_version
                                          ii_log = ii_log ).
      IF ls_result-text IS INITIAL.
        RETURN.
      ENDIF.

      "log comparison result
      ls_item-obj_type = is_result-obj_type.
      ls_item-obj_name = is_result-obj_name.
      ii_log->add_warning( iv_msg = ls_result-text
                           is_item = ls_item ).

      "continue or abort?
      IF zcl_abapgit_ui_factory=>get_frontend_services( )->gui_is_available( ) = abap_true.
        lv_answer = zcl_abapgit_ui_factory=>get_popups( )->popup_to_confirm(
          iv_titlebar              = 'Warning'
          iv_text_question         = ls_result-text
          iv_text_button_1         = 'Pull Anyway'
          iv_icon_button_1         = 'ICON_OKAY'
          iv_text_button_2         = 'Cancel'
          iv_icon_button_2         = 'ICON_CANCEL'
          iv_default_button        = '2'
          iv_display_cancel_button = abap_false ).

        IF lv_answer = '2'.
          zcx_abapgit_exception=>raise( |Deserialization for object { is_result-obj_name } | &
                                        |(type { is_result-obj_type }) aborted by user| ).
        ENDIF.
      ELSE.
        zcx_abapgit_exception=>raise( |Deserialization for object { is_result-obj_name } | &
                                      |(type { is_result-obj_type }) aborted, user descision required| ).
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD create_object.

    DATA: lv_message            TYPE string,
          lv_class_name         TYPE string,
          ls_obj_serializer_map LIKE LINE OF gt_obj_serializer_map.


    READ TABLE gt_obj_serializer_map
      INTO ls_obj_serializer_map WITH KEY item = is_item.
    IF sy-subrc = 0.
      lv_class_name = ls_obj_serializer_map-metadata-class.
    ELSEIF is_metadata IS NOT INITIAL.
*        Metadata is provided only on deserialization
*        Once this has been triggered, the same deserializer shall be used
*        for subsequent processes.
*        Thus, buffer the metadata afterwards
      ls_obj_serializer_map-item      = is_item.
      ls_obj_serializer_map-metadata  = is_metadata.
      INSERT ls_obj_serializer_map INTO TABLE gt_obj_serializer_map.
      lv_class_name = is_metadata-class.
    ELSE.
      lv_class_name = class_name( is_item ).
    ENDIF.

    REPLACE FIRST OCCURRENCE OF 'LCL' IN lv_class_name WITH 'ZCL_ABAPGIT'.

    IF zcl_abapgit_factory=>get_environment( )->is_merged( ) = abap_true.
      " Prevent accidental usage of object handlers in the developer version
      lv_class_name = |\\PROGRAM={ sy-repid }\\CLASS={ lv_class_name }|.
    ENDIF.

    TRY.
        CREATE OBJECT ri_obj TYPE (lv_class_name)
          EXPORTING
            is_item     = is_item
            iv_language = iv_language.
      CATCH cx_sy_create_object_error.
        lv_message = |Object type { is_item-obj_type } is not supported by this system|.
        IF iv_native_only = abap_false.
          TRY. " 2nd step, try looking for plugins
              CREATE OBJECT ri_obj TYPE zcl_abapgit_objects_bridge
                EXPORTING
                  is_item = is_item.
            CATCH cx_sy_create_object_error.
              zcx_abapgit_exception=>raise( lv_message ).
          ENDTRY.
        ELSE. " No native support? -> fail
          zcx_abapgit_exception=>raise( lv_message ).
        ENDIF.
    ENDTRY.

  ENDMETHOD.


  METHOD delete.

    DATA: ls_item     TYPE zif_abapgit_definitions=>ty_item,
          li_progress TYPE REF TO zif_abapgit_progress,
          lt_tadir    LIKE it_tadir,
          lt_deleted  LIKE it_tadir,
          lt_items    TYPE zif_abapgit_definitions=>ty_items_tt,
          lx_error    TYPE REF TO zcx_abapgit_exception,
          lv_count    TYPE i.

    FIELD-SYMBOLS: <ls_tadir> LIKE LINE OF it_tadir.

    IF it_tadir IS INITIAL.
      RETURN.
    ENDIF.

    lt_tadir = it_tadir.

    IF ii_log IS BOUND.
      IF lines( lt_tadir ) = 1.
        ii_log->add_info( |>>> Deleting 1 object| ).
      ELSE.
        ii_log->add_info( |>>> Deleting { lines( lt_tadir ) } objects| ).
      ENDIF.
    ENDIF.

    IF is_checks-transport-required = abap_true.
      zcl_abapgit_default_transport=>get_instance( )->set( is_checks-transport-transport ).
    ENDIF.

    TRY.
        zcl_abapgit_dependencies=>resolve( CHANGING ct_tadir = lt_tadir ).

        li_progress = zcl_abapgit_progress=>get_instance( lines( lt_tadir ) ).

        lt_items = map_tadir_to_items( lt_tadir ).

        check_objects_locked( iv_language = zif_abapgit_definitions=>c_english
                              it_items    = lt_items ).

      CATCH zcx_abapgit_exception INTO lx_error.
        zcl_abapgit_default_transport=>get_instance( )->reset( ).
        RAISE EXCEPTION lx_error.
    ENDTRY.

    lv_count = 1.
    DO.
      CLEAR lt_deleted.
      LOOP AT lt_tadir ASSIGNING <ls_tadir>.
        li_progress->show( iv_current = lv_count
                           iv_text    = |Delete { <ls_tadir>-obj_name }| ).

        CLEAR ls_item.
        ls_item-obj_type = <ls_tadir>-object.
        ls_item-obj_name = <ls_tadir>-obj_name.

        TRY.
            delete_object(
              iv_package   = <ls_tadir>-devclass
              is_item      = ls_item
              iv_transport = is_checks-transport-transport ).

            INSERT <ls_tadir> INTO TABLE lt_deleted.
            DELETE lt_tadir.
            lv_count = lv_count + 1.

            " make sure to save object deletions
            COMMIT WORK.

            IF ii_log IS BOUND.
              ii_log->add_info( iv_msg  = |Object { ls_item-obj_type } { ls_item-obj_name } deleted|
                                is_item = ls_item ).
            ENDIF.

          CATCH zcx_abapgit_exception INTO lx_error.
            IF ii_log IS BOUND.
              ii_log->add_exception( ix_exc  = lx_error
                                     is_item = ls_item ).
              ii_log->add_error( iv_msg  = |Deletion of object { ls_item-obj_name } failed|
                                 is_item = ls_item ).
            ENDIF.
        ENDTRY.

      ENDLOOP.

      " Exit if done or nothing else was deleted
      IF lines( lt_tadir ) = 0 OR lines( lt_deleted ) = 0.
        EXIT.
      ENDIF.
    ENDDO.

    zcl_abapgit_default_transport=>get_instance( )->reset( ).

    IF lx_error IS BOUND AND lines( lt_tadir ) > 0.
      zcx_abapgit_exception=>raise( 'Error during uninstall. Check the log.' ).
    ENDIF.

    li_progress->off( ).

  ENDMETHOD.


  METHOD delete_object.

    DATA: li_obj TYPE REF TO zif_abapgit_object.

    " Nothing to do for unsupported objects
    IF is_type_supported( is_item-obj_type ) = abap_false.
      RETURN.
    ENDIF.

    li_obj = create_object( is_item     = is_item
                            iv_language = zif_abapgit_definitions=>c_english ).

    li_obj->delete( iv_package   = iv_package
                    iv_transport = iv_transport ).

  ENDMETHOD.


  METHOD deserialize.

    DATA: ls_item     TYPE zif_abapgit_definitions=>ty_item,
          li_obj      TYPE REF TO zif_abapgit_object,
          lt_remote   TYPE zif_abapgit_git_definitions=>ty_files_tt,
          lv_package  TYPE devclass,
          lo_files    TYPE REF TO zcl_abapgit_objects_files,
          ls_metadata TYPE zif_abapgit_definitions=>ty_metadata,
          lo_xml      TYPE REF TO zif_abapgit_xml_input,
          lt_results  TYPE zif_abapgit_definitions=>ty_results_tt,
          li_progress TYPE REF TO zif_abapgit_progress,
          lv_path     TYPE string,
          lt_items    TYPE zif_abapgit_definitions=>ty_items_tt,
          lt_steps_id TYPE zif_abapgit_definitions=>ty_deserialization_step_tt,
          lt_steps    TYPE zif_abapgit_objects=>ty_step_data_tt,
          lx_exc      TYPE REF TO zcx_abapgit_exception.
    DATA lo_folder_logic TYPE REF TO zcl_abapgit_folder_logic.
    DATA ls_i18n_params TYPE zif_abapgit_definitions=>ty_i18n_params.
    DATA lo_timer TYPE REF TO zcl_abapgit_timer.

    FIELD-SYMBOLS: <ls_result>  TYPE zif_abapgit_definitions=>ty_result,
                   <lv_step_id> TYPE LINE OF zif_abapgit_definitions=>ty_deserialization_step_tt,
                   <ls_step>    TYPE LINE OF zif_abapgit_objects=>ty_step_data_tt,
                   <ls_deser>   TYPE LINE OF zif_abapgit_objects=>ty_deserialization_tt.

    lt_steps = get_deserialize_steps( ).

    lv_package = io_repo->get_package( ).

    IF is_checks-transport-required = abap_true.
      zcl_abapgit_default_transport=>get_instance( )->set( is_checks-transport-transport ).
    ENDIF.

    zcl_abapgit_objects_activation=>clear( ).

    lt_remote = io_repo->get_files_remote( iv_ignore_files = abap_true ).

    lt_results = zcl_abapgit_file_deserialize=>get_results(
      io_repo = io_repo
      ii_log = ii_log ).

    IF lt_results IS INITIAL.
      RETURN.
    ENDIF.

    zcl_abapgit_objects_check=>checks_adjust(
      EXPORTING
        io_repo    = io_repo
        is_checks  = is_checks
      CHANGING
        ct_results = lt_results ).

    li_progress = zcl_abapgit_progress=>get_instance( lines( lt_results ) ).

    lt_items = map_results_to_items( lt_results ).

    lo_timer = zcl_abapgit_timer=>create(
      iv_text  = 'Deserialize:'
      iv_count = lines( lt_items ) )->start( ).

    check_objects_locked( iv_language = io_repo->get_dot_abapgit( )->get_main_language( )
                          it_items    = lt_items ).

    ls_i18n_params = determine_i18n_params(
      io_dot = io_repo->get_dot_abapgit( )
      iv_main_language_only = io_repo->get_local_settings( )-main_language_only ).

    IF lines( lt_items ) = 1.
      ii_log->add_info( |>>> Deserializing 1 object| ).
    ELSE.
      ii_log->add_info( |>>> Deserializing { lines( lt_items ) } objects| ).
    ENDIF.

    lo_folder_logic = zcl_abapgit_folder_logic=>get_instance( ).
    LOOP AT lt_results ASSIGNING <ls_result>.
      li_progress->show( iv_current = sy-tabix
                         iv_text    = |Prepare Deserialize: { <ls_result>-obj_type } { <ls_result>-obj_name }| ).

      CLEAR ls_item.
      ls_item-obj_type = <ls_result>-obj_type.
      ls_item-obj_name = <ls_result>-obj_name.

      "error handling & logging added
      TRY.
          IF ls_item-obj_type <> 'NSPC'.
            " If package does not exist yet, it will be created with this call
            lv_package = lo_folder_logic->path_to_package(
              iv_top  = io_repo->get_package( )
              io_dot  = io_repo->get_dot_abapgit( )
              iv_path = <ls_result>-path ).

            check_main_package(
              iv_package  = lv_package
              iv_obj_type = ls_item-obj_type ).
          ENDIF.

          IF ls_item-obj_type = 'DEVC'.
            " Packages have the same filename across different folders. The path needs to be supplied
            " to find the correct file.
            lv_path = <ls_result>-path.
          ENDIF.

          ls_item-devclass = lv_package.

          IF <ls_result>-packmove = abap_true.
            " Move object to new package
            change_package_assignments( is_item = ls_item
                                        ii_log  = ii_log ).
            " No other changes required
            CONTINUE.
          ENDIF.

          " Create or update object
          CREATE OBJECT lo_files
            EXPORTING
              is_item = ls_item
              iv_path = lv_path.

          lo_files->set_files( lt_remote ).

          IF lo_files->is_json_metadata( ) = abap_false.
            "analyze XML in order to instantiate the proper serializer
            lo_xml = lo_files->read_xml( ).
            lo_xml->i18n_params( ls_i18n_params ).
            ls_metadata = lo_xml->get_metadata( ).
          ELSE.
            " there's no XML and metadata for JSON format
            CLEAR: lo_xml, ls_metadata.
          ENDIF.

          li_obj = create_object( is_item     = ls_item
                                  iv_language = io_repo->get_dot_abapgit( )->get_main_language( )
                                  is_metadata = ls_metadata ).

          compare_remote_to_local(
            ii_object = li_obj
            it_remote = lt_remote
            is_result = <ls_result>
            ii_log    = ii_log ).

          li_obj->mo_files = lo_files.

          "get required steps for deserialize the object
          lt_steps_id = li_obj->get_deserialize_steps( ).

          LOOP AT lt_steps_id ASSIGNING <lv_step_id>.
            READ TABLE lt_steps WITH KEY step_id = <lv_step_id> ASSIGNING <ls_step>.
            ASSERT sy-subrc = 0.
            IF <lv_step_id> = zif_abapgit_object=>gc_step_id-ddic AND
               zcl_abapgit_objects_activation=>is_ddic_type( ls_item-obj_type ) = abap_false.
              " DDIC only for DDIC objects
              zcx_abapgit_exception=>raise( |Step { <lv_step_id> } is only for DDIC objects| ).
            ENDIF.
            APPEND INITIAL LINE TO <ls_step>-objects ASSIGNING <ls_deser>.
            <ls_deser>-item    = ls_item.
            <ls_deser>-obj     = li_obj.
            <ls_deser>-xml     = lo_xml.
            <ls_deser>-package = lv_package.
          ENDLOOP.

          CLEAR: lv_path, lv_package.

        CATCH zcx_abapgit_exception INTO lx_exc.
          ii_log->add_exception( ix_exc = lx_exc
                                 is_item = ls_item ).
          ii_log->add_error( iv_msg = |Import of object { ls_item-obj_name } failed|
                             is_item = ls_item ).
          "object should not be part of any deserialization step
          CONTINUE.
      ENDTRY.

    ENDLOOP.

    li_progress->off( ).

    "run deserialize for all steps and its objects
    deserialize_steps(
      EXPORTING
        it_steps     = lt_steps
        ii_log       = ii_log
        iv_transport = is_checks-transport-transport
      CHANGING
        ct_files     = rt_accessed_files ).

    update_package_tree( io_repo->get_package( ) ).

    zcl_abapgit_default_transport=>get_instance( )->reset( ).

    lo_timer->end( abap_true ).

  ENDMETHOD.


  METHOD deserialize_checks.

    rs_checks = zcl_abapgit_objects_check=>deserialize_checks( io_repo ).

  ENDMETHOD.


  METHOD deserialize_objects.

    DATA: li_progress TYPE REF TO zif_abapgit_progress,
          li_exit     TYPE REF TO zif_abapgit_exit,
          lx_exc      TYPE REF TO zcx_abapgit_exception.

    FIELD-SYMBOLS: <ls_obj> LIKE LINE OF is_step-objects.


    zcl_abapgit_objects_activation=>clear( ).

    ii_log->add_success( |>> Step { is_step-order } - { is_step-descr }| ).

    li_progress = zcl_abapgit_progress=>get_instance( lines( is_step-objects ) ).

    LOOP AT is_step-objects ASSIGNING <ls_obj>.
      li_progress->show(
        iv_current = sy-tabix
        iv_text    = |Step { is_step-order } - { is_step-descr }:| &&
                     | { <ls_obj>-item-obj_type } { <ls_obj>-item-obj_name }| ).

      TRY.
          <ls_obj>-obj->deserialize( iv_package   = <ls_obj>-package
                                     io_xml       = <ls_obj>-xml
                                     iv_step      = is_step-step_id
                                     ii_log       = ii_log
                                     iv_transport = iv_transport ).
          APPEND LINES OF <ls_obj>-obj->mo_files->get_accessed_files( ) TO ct_files.

          ii_log->add_success( iv_msg = |Object { <ls_obj>-item-obj_name } imported|
                               is_item = <ls_obj>-item ).

        CATCH zcx_abapgit_exception INTO lx_exc.
          ii_log->add_exception( ix_exc = lx_exc
                                 is_item = <ls_obj>-item ).
          ii_log->add_error( iv_msg = |Import of object { <ls_obj>-item-obj_name } failed|
                             is_item = <ls_obj>-item ).
      ENDTRY.

    ENDLOOP.

    li_progress->show( iv_current = lines( is_step-objects )
                       iv_text    = |Step { is_step-order } - Activating Objects| ).

    CASE is_step-step_id.
      WHEN zif_abapgit_object=>gc_step_id-ddic.
        zcl_abapgit_objects_activation=>activate(
          iv_ddic = abap_true
          ii_log  = ii_log ).
      WHEN zif_abapgit_object=>gc_step_id-abap.
        zcl_abapgit_objects_activation=>activate(
          iv_ddic = abap_false
          ii_log  = ii_log ).
      WHEN zif_abapgit_object=>gc_step_id-late.
        " late can have both DDIC (like TABL with REF TO) and non-DDIC objects
        zcl_abapgit_objects_activation=>activate(
          iv_ddic = abap_true
          ii_log  = ii_log ).
        zcl_abapgit_objects_activation=>activate(
          iv_ddic = abap_false
          ii_log  = ii_log ).
    ENDCASE.

    li_progress->off( ).

*   Call postprocessing
    li_exit = zcl_abapgit_exit=>get_instance( ).

    li_exit->deserialize_postprocess( is_step = is_step
                                      ii_log  = ii_log ).

  ENDMETHOD.


  METHOD deserialize_steps.

    FIELD-SYMBOLS <ls_step> LIKE LINE OF it_steps.

    LOOP AT it_steps ASSIGNING <ls_step>.
      deserialize_objects(
        EXPORTING
          is_step      = <ls_step>
          ii_log       = ii_log
          iv_transport = iv_transport
        CHANGING
          ct_files     = ct_files ).
    ENDLOOP.

    SORT ct_files BY path ASCENDING filename ASCENDING.
    DELETE ADJACENT DUPLICATES FROM ct_files. " Just in case

  ENDMETHOD.


  METHOD determine_i18n_params.

    " TODO: unify with ZCL_ABAPGIT_SERIALIZE=>DETERMINE_I18N_PARAMS, same code

    IF io_dot IS BOUND.
      rs_i18n_params-main_language         = io_dot->get_main_language( ).
      rs_i18n_params-use_lxe               = io_dot->use_lxe( ).
      rs_i18n_params-main_language_only    = iv_main_language_only.
      rs_i18n_params-translation_languages = zcl_abapgit_lxe_texts=>get_translation_languages(
        iv_main_language  = io_dot->get_main_language( )
        it_i18n_languages = io_dot->get_i18n_languages( ) ).
    ENDIF.

    IF rs_i18n_params-main_language IS INITIAL.
      rs_i18n_params-main_language = sy-langu.
    ENDIF.

  ENDMETHOD.


  METHOD exists.

    DATA: li_obj TYPE REF TO zif_abapgit_object.

    " Might be called for objects without tadir entry
    IF is_item IS INITIAL.
      RETURN.
    ENDIF.

    " For unsupported objects, assume object exists
    IF is_type_supported( is_item-obj_type ) = abap_false.
      rv_bool = abap_true.
      RETURN.
    ENDIF.

    TRY.
        li_obj = create_object( is_item     = is_item
                                iv_language = zif_abapgit_definitions=>c_english ).

        rv_bool = li_obj->exists( ).
      CATCH zcx_abapgit_exception.
        " Ignore errors and assume the object exists
        rv_bool = abap_true.
    ENDTRY.

  ENDMETHOD.


  METHOD get_deserialize_steps.
    FIELD-SYMBOLS: <ls_step> TYPE LINE OF zif_abapgit_objects=>ty_step_data_tt.

    APPEND INITIAL LINE TO rt_steps ASSIGNING <ls_step>.
    <ls_step>-step_id      = zif_abapgit_object=>gc_step_id-early.
    <ls_step>-descr        = 'Pre-process Objects'.
    <ls_step>-syntax_check = abap_false.
    <ls_step>-order        = 1.

    APPEND INITIAL LINE TO rt_steps ASSIGNING <ls_step>.
    <ls_step>-step_id      = zif_abapgit_object=>gc_step_id-ddic.
    <ls_step>-descr        = 'Deserialize DDIC Objects'.
    <ls_step>-syntax_check = abap_false.
    <ls_step>-order        = 2.

    APPEND INITIAL LINE TO rt_steps ASSIGNING <ls_step>.
    <ls_step>-step_id      = zif_abapgit_object=>gc_step_id-abap.
    <ls_step>-descr        = 'Deserialize non-DDIC Objects'.
    <ls_step>-syntax_check = abap_false.
    <ls_step>-order        = 3.

    APPEND INITIAL LINE TO rt_steps ASSIGNING <ls_step>.
    <ls_step>-step_id      = zif_abapgit_object=>gc_step_id-late.
    <ls_step>-descr        = 'Post-process Objects'.
    <ls_step>-syntax_check = abap_true.
    <ls_step>-order        = 4.

    SORT rt_steps BY order. " ensure correct processing order
  ENDMETHOD.


  METHOD get_extra_from_filename.

    IF iv_filename IS NOT INITIAL.
      FIND REGEX '\..*\.([\-a-z0-9_%]*)\.' IN iv_filename SUBMATCHES rv_extra.
      IF sy-subrc = 0.
        rv_extra = cl_http_utility=>unescape_url( rv_extra ).
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD is_active.

    DATA: li_obj TYPE REF TO zif_abapgit_object.

    " For unsupported objects, assume active state
    IF is_type_supported( is_item-obj_type ) = abap_false.
      rv_active = abap_true.
      RETURN.
    ENDIF.

    TRY.
        li_obj = create_object( is_item     = is_item
                                iv_language = zif_abapgit_definitions=>c_english ).

        rv_active = li_obj->is_active( ).
      CATCH cx_sy_dyn_call_illegal_method
            cx_sy_ref_is_initial
            zcx_abapgit_exception.
        " Ignore errors and assume active state
        rv_active = abap_true.
    ENDTRY.

  ENDMETHOD.


  METHOD is_supported.

    TRY.
        create_object( is_item        = is_item
                       iv_language    = zif_abapgit_definitions=>c_english
                       iv_native_only = iv_native_only ).
        rv_bool = abap_true.
      CATCH zcx_abapgit_exception.
        rv_bool = abap_false.
    ENDTRY.

  ENDMETHOD.


  METHOD is_type_supported.

    DATA: ls_item               TYPE zif_abapgit_definitions=>ty_item,
          ls_supported_obj_type TYPE ty_supported_types.

    FIELD-SYMBOLS <ls_supported_obj_type> TYPE ty_supported_types.

    IF iv_obj_type IS INITIAL.
      " empty object type should never exist
      RETURN.
    ENDIF.

    READ TABLE gt_supported_obj_types
      ASSIGNING <ls_supported_obj_type>
      WITH KEY obj_type = iv_obj_type.

    IF sy-subrc <> 0.

      ls_item-obj_type = iv_obj_type.

      ls_supported_obj_type-obj_type  = iv_obj_type.
      ls_supported_obj_type-supported = is_supported( ls_item ).

      INSERT ls_supported_obj_type INTO TABLE gt_supported_obj_types.

      rv_bool = ls_supported_obj_type-supported.
      RETURN.

    ENDIF.

    rv_bool = <ls_supported_obj_type>-supported.

  ENDMETHOD.


  METHOD jump.

    DATA: li_obj  TYPE REF TO zif_abapgit_object,
          lv_exit TYPE abap_bool.

    " Nothing to do for unsupported objects
    IF is_type_supported( is_item-obj_type ) = abap_false.
      zcx_abapgit_exception=>raise( |Object type { is_item-obj_type } is not supported by this system| ).
    ENDIF.

    " Nothing to do if object does not exist
    li_obj = create_object( is_item     = is_item
                            iv_language = zif_abapgit_definitions=>c_english ).

    IF li_obj->exists( ) = abap_false.
      zcx_abapgit_exception=>raise( |Object { is_item-obj_type } { is_item-obj_name } doesn't exist| ).
    ENDIF.

    " First priority object-specific handler
    lv_exit = li_obj->jump( get_extra_from_filename( iv_filename ) ).

    IF lv_exit = abap_false.
      " Open object in new window with generic jumper
      lv_exit = zcl_abapgit_ui_factory=>get_gui_jumper( )->jump(
        is_item        = is_item
        is_sub_item    = is_sub_item
        iv_line_number = iv_line_number ).
    ENDIF.

    IF lv_exit = abap_false.
      zcx_abapgit_exception=>raise( |Jump to { is_item-obj_type } { is_item-obj_name } not possible| ).
    ENDIF.

  ENDMETHOD.


  METHOD map_results_to_items.

    DATA: ls_item LIKE LINE OF rt_items.
    FIELD-SYMBOLS: <ls_result> TYPE zif_abapgit_definitions=>ty_result.

    LOOP AT it_results ASSIGNING <ls_result>.

      ls_item-devclass = <ls_result>-package.
      ls_item-obj_type = <ls_result>-obj_type.
      ls_item-obj_name = <ls_result>-obj_name.
      INSERT ls_item INTO TABLE rt_items.

    ENDLOOP.

  ENDMETHOD.


  METHOD map_tadir_to_items.

    DATA: ls_item LIKE LINE OF rt_items.
    FIELD-SYMBOLS: <ls_tadir> TYPE zif_abapgit_definitions=>ty_tadir.

    LOOP AT it_tadir ASSIGNING <ls_tadir>.

      ls_item-devclass = <ls_tadir>-devclass.
      ls_item-obj_type = <ls_tadir>-object.
      ls_item-obj_name = <ls_tadir>-obj_name.
      INSERT ls_item INTO TABLE rt_items.

    ENDLOOP.

  ENDMETHOD.


  METHOD serialize.

    DATA: li_obj         TYPE REF TO zif_abapgit_object,
          lx_error       TYPE REF TO zcx_abapgit_exception,
          li_xml         TYPE REF TO zif_abapgit_xml_output,
          lo_files       TYPE REF TO zcl_abapgit_objects_files,
          ls_i18n_params TYPE zif_abapgit_definitions=>ty_i18n_params.

    FIELD-SYMBOLS: <ls_file> LIKE LINE OF rs_files_and_item-files.

    rs_files_and_item-item = is_item.

    IF is_type_supported( rs_files_and_item-item-obj_type ) = abap_false.
      zcx_abapgit_exception=>raise( |Object type ignored, not supported: {
        rs_files_and_item-item-obj_type }-{
        rs_files_and_item-item-obj_name }| ).
    ENDIF.

    CREATE OBJECT lo_files
      EXPORTING
        is_item = rs_files_and_item-item.

    li_obj = create_object( is_item     = rs_files_and_item-item
                            iv_language = iv_language ).

    li_obj->mo_files = lo_files.

    CREATE OBJECT li_xml TYPE zcl_abapgit_xml_output.

    ls_i18n_params-main_language         = iv_language.
    ls_i18n_params-main_language_only    = iv_main_language_only.
    ls_i18n_params-translation_languages = it_translation_langs.
    ls_i18n_params-use_lxe               = iv_use_lxe.

    li_xml->i18n_params( ls_i18n_params ).

    TRY.
        li_obj->serialize( li_xml ).
      CATCH zcx_abapgit_exception INTO lx_error.
        rs_files_and_item-item-inactive = boolc( li_obj->is_active( ) = abap_false ).
        RAISE EXCEPTION lx_error.
    ENDTRY.

    IF lo_files->is_json_metadata( ) = abap_false.
      lo_files->add_xml( ii_xml      = li_xml
                         is_metadata = li_obj->get_metadata( ) ).
    ENDIF.

    rs_files_and_item-files = lo_files->get_files( ).

    check_duplicates( rs_files_and_item-files ).

    rs_files_and_item-item-inactive = boolc( li_obj->is_active( ) = abap_false ).

    LOOP AT rs_files_and_item-files ASSIGNING <ls_file>.
      <ls_file>-sha1 = zcl_abapgit_hash=>sha1_blob( <ls_file>-data ).
    ENDLOOP.

  ENDMETHOD.


  METHOD supported_list.

    DATA lt_objects            TYPE STANDARD TABLE OF ko100.
    DATA ls_item               TYPE zif_abapgit_definitions=>ty_item.
    DATA ls_supported_obj_type TYPE ty_supported_types.
    DATA lt_types              TYPE zif_abapgit_exit=>ty_object_types.
    DATA lv_type               LIKE LINE OF lt_types.
    DATA li_exit               TYPE REF TO zif_abapgit_exit.

    FIELD-SYMBOLS <ls_object> LIKE LINE OF lt_objects.
    FIELD-SYMBOLS <ls_supported_obj_type> TYPE ty_supported_types.

    IF gv_supported_obj_types_loaded = abap_true.
      LOOP AT gt_supported_obj_types ASSIGNING <ls_supported_obj_type> WHERE supported = abap_true.
        INSERT <ls_supported_obj_type>-obj_type INTO TABLE rt_types.
      ENDLOOP.
      RETURN.
    ENDIF.

    " delete content because it might be filled already by method IS_TYPE_SUPPORTED
    CLEAR gt_supported_obj_types.

    CALL FUNCTION 'TR_OBJECT_TABLE'
      TABLES
        wt_object_text = lt_objects
      EXCEPTIONS
        OTHERS         = 1 ##FM_SUBRC_OK.

    LOOP AT lt_objects ASSIGNING <ls_object> WHERE pgmid = 'R3TR'.
      INSERT <ls_object>-object INTO TABLE lt_types.
    ENDLOOP.

    li_exit = zcl_abapgit_exit=>get_instance( ).
    li_exit->change_supported_object_types( CHANGING ct_types = lt_types ).

    LOOP AT lt_types INTO lv_type.
      ls_item-obj_type = lv_type.

      ls_supported_obj_type-obj_type  = lv_type.
      ls_supported_obj_type-supported = is_supported( ls_item ).

      INSERT ls_supported_obj_type INTO TABLE gt_supported_obj_types.

      IF ls_supported_obj_type-supported = abap_true.
        INSERT ls_supported_obj_type-obj_type INTO TABLE rt_types.
      ENDIF.
    ENDLOOP.

    gv_supported_obj_types_loaded = abap_true.

  ENDMETHOD.


  METHOD update_package_tree.

    DATA: lt_packages TYPE zif_abapgit_sap_package=>ty_devclass_tt,
          lv_package  LIKE LINE OF lt_packages,
          lv_tree     TYPE dirtree-tname.


    lt_packages = zcl_abapgit_factory=>get_sap_package( iv_package )->list_subpackages( ).
    APPEND iv_package TO lt_packages.

    LOOP AT lt_packages INTO lv_package.
* update package tree for SE80
      lv_tree = 'EU_' && lv_package.
      CALL FUNCTION 'WB_TREE_ACTUALIZE'
        EXPORTING
          tree_name              = lv_tree
          without_crossreference = abap_true
          with_tcode_index       = abap_true.
    ENDLOOP.

  ENDMETHOD.
ENDCLASS.
