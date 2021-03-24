CLASS zcl_abapgit_objects DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES:
      ty_types_tt TYPE SORTED TABLE OF tadir-object WITH UNIQUE KEY table_line .
    TYPES:
      BEGIN OF ty_serialization,
        files TYPE zif_abapgit_definitions=>ty_files_tt,
        item  TYPE zif_abapgit_definitions=>ty_item,
      END OF ty_serialization .

    CLASS-METHODS serialize
      IMPORTING
        !is_item                       TYPE zif_abapgit_definitions=>ty_item
        !iv_language                   TYPE spras
        !iv_serialize_master_lang_only TYPE abap_bool DEFAULT abap_false
        !it_translation_langs          TYPE zif_abapgit_definitions=>ty_languages OPTIONAL
      RETURNING
        VALUE(rs_files_and_item)       TYPE ty_serialization
      RAISING
        zcx_abapgit_exception .
    CLASS-METHODS deserialize
      IMPORTING
        !io_repo                 TYPE REF TO zcl_abapgit_repo
        !is_checks               TYPE zif_abapgit_definitions=>ty_deserialize_checks
        !ii_log                  TYPE REF TO zif_abapgit_log
      RETURNING
        VALUE(rt_accessed_files) TYPE zif_abapgit_definitions=>ty_file_signatures_tt
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
        !is_item         TYPE zif_abapgit_definitions=>ty_item
        !iv_line_number  TYPE i OPTIONAL
        !iv_sub_obj_name TYPE zif_abapgit_definitions=>ty_item-obj_name OPTIONAL
        !iv_sub_obj_type TYPE zif_abapgit_definitions=>ty_item-obj_type OPTIONAL
      RAISING
        zcx_abapgit_exception .
    CLASS-METHODS changed_by
      IMPORTING
        !is_item       TYPE zif_abapgit_definitions=>ty_item
      RETURNING
        VALUE(rv_user) TYPE xubname
      RAISING
        zcx_abapgit_exception .
    CLASS-METHODS is_supported
      IMPORTING
        !is_item        TYPE zif_abapgit_definitions=>ty_item
        !iv_native_only TYPE abap_bool DEFAULT abap_false
      RETURNING
        VALUE(rv_bool)  TYPE abap_bool .
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
      BEGIN OF ty_obj_serializer_item,
        item     TYPE zif_abapgit_definitions=>ty_item,
        metadata TYPE zif_abapgit_definitions=>ty_metadata,
      END OF ty_obj_serializer_item .
    TYPES:
      ty_obj_serializer_map TYPE SORTED TABLE OF ty_obj_serializer_item WITH UNIQUE KEY item .

    CLASS-DATA gt_obj_serializer_map TYPE ty_obj_serializer_map .
    CLASS-DATA gt_supported_obj_types TYPE ty_types_tt .

    CLASS-METHODS check_duplicates
      IMPORTING
        !it_files TYPE zif_abapgit_definitions=>ty_files_tt
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
        !iv_package TYPE devclass
        !is_item    TYPE zif_abapgit_definitions=>ty_item
      RAISING
        zcx_abapgit_exception .
    CLASS-METHODS compare_remote_to_local
      IMPORTING
        !ii_object TYPE REF TO zif_abapgit_object
        !it_remote TYPE zif_abapgit_definitions=>ty_files_tt
        !is_result TYPE zif_abapgit_definitions=>ty_result
        !ii_log    TYPE REF TO zif_abapgit_log
      RAISING
        zcx_abapgit_exception .
    CLASS-METHODS deserialize_objects
      IMPORTING
        !is_step  TYPE zif_abapgit_objects=>ty_step_data
        !ii_log   TYPE REF TO zif_abapgit_log
      CHANGING
        !ct_files TYPE zif_abapgit_definitions=>ty_file_signatures_tt
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
ENDCLASS.



CLASS zcl_abapgit_objects IMPLEMENTATION.


  METHOD changed_by.

    DATA: li_obj TYPE REF TO zif_abapgit_object.

    IF is_item IS NOT INITIAL.
      li_obj = create_object( is_item     = is_item
                              iv_language = zif_abapgit_definitions=>c_english ).
      rv_user = li_obj->changed_by( ).
    ENDIF.

    IF rv_user IS INITIAL.
* eg. ".abapgit.xml" file
      rv_user = zcl_abapgit_objects_super=>c_user_unknown.
    ENDIF.

  ENDMETHOD.


  METHOD check_duplicates.

    DATA: lt_files          TYPE zif_abapgit_definitions=>ty_files_tt,
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
      IF is_supported( <ls_item> ) = abap_false.
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

    DATA: ls_remote_file      TYPE zif_abapgit_definitions=>ty_file,
          li_remote_version   TYPE REF TO zif_abapgit_xml_input,
          lv_count            TYPE i,
          ls_result           TYPE zif_abapgit_comparator=>ty_result,
          lv_answer           TYPE string,
          li_comparator       TYPE REF TO zif_abapgit_comparator,
          lv_gui_is_available TYPE abap_bool,
          ls_item             TYPE zif_abapgit_definitions=>ty_item.

    FIND ALL OCCURRENCES OF '.' IN is_result-filename MATCH COUNT lv_count.

    IF is_result-filename CS '.XML' AND lv_count = 2.
      IF ii_object->exists( ) = abap_false.
        RETURN.
      ENDIF.

      READ TABLE it_remote WITH KEY filename = is_result-filename INTO ls_remote_file.
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
      lv_gui_is_available = zcl_abapgit_ui_factory=>get_gui_functions( )->gui_is_available( ).
      IF lv_gui_is_available = abap_true.
        CALL FUNCTION 'POPUP_TO_CONFIRM'
          EXPORTING
            titlebar              = 'Warning'
            text_question         = ls_result-text
            text_button_1         = 'Abort'
            icon_button_1         = 'ICON_CANCEL'
            text_button_2         = 'Pull anyway'
            icon_button_2         = 'ICON_OKAY'
            default_button        = '2'
            display_cancel_button = abap_false
          IMPORTING
            answer                = lv_answer
          EXCEPTIONS
            text_not_found        = 1
            OTHERS                = 2.
        IF sy-subrc <> 0 OR lv_answer = 1.
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
*        Metadata is provided only on serialization
*        Once this has been triggered, the same serializer shall be used
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

    TRY.
        CREATE OBJECT ri_obj TYPE (lv_class_name)
          EXPORTING
            is_item     = is_item
            iv_language = iv_language.
      CATCH cx_sy_create_object_error.
        lv_message = |Object type { is_item-obj_type } not supported, serialize|.
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

    lt_tadir = it_tadir.

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
              iv_package = <ls_tadir>-devclass
              is_item    = ls_item ).

            INSERT <ls_tadir> INTO TABLE lt_deleted.
            DELETE lt_tadir.
            lv_count = lv_count + 1.

            " make sure to save object deletions
            COMMIT WORK.
          CATCH zcx_abapgit_exception INTO lx_error.
            IF ii_log IS BOUND.
              ii_log->add_exception( ix_exc  = lx_error
                                     is_item = ls_item ).
              ii_log->add_error( iv_msg = |Deletion of object { ls_item-obj_name } failed|
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


    IF is_supported( is_item ) = abap_true.
      li_obj = create_object( is_item     = is_item
                              iv_language = zif_abapgit_definitions=>c_english ).

      li_obj->delete( iv_package ).

      IF li_obj->get_metadata( )-delete_tadir = abap_true.
        CALL FUNCTION 'TR_TADIR_INTERFACE'
          EXPORTING
            wi_delete_tadir_entry = abap_true
            wi_tadir_pgmid        = 'R3TR'
            wi_tadir_object       = is_item-obj_type
            wi_tadir_obj_name     = is_item-obj_name
            wi_test_modus         = abap_false
          EXCEPTIONS
            OTHERS                = 1 ##FM_SUBRC_OK.

        " We deliberately ignore the subrc, because throwing an exception would
        " break the deletion of lots of object types. On the other hand we have
        " to catch the exceptions because otherwise messages would directly be issued
        " by the function module and change the control flow. Thus breaking the
        " deletion of TOBJ and other object types.
        " TODO: This is not very clean and has to be improved in the future. See PR 2741.

      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD deserialize.

    DATA: ls_item     TYPE zif_abapgit_definitions=>ty_item,
          li_obj      TYPE REF TO zif_abapgit_object,
          lt_remote   TYPE zif_abapgit_definitions=>ty_files_tt,
          lv_package  TYPE devclass,
          lo_files    TYPE REF TO zcl_abapgit_objects_files,
          lo_xml      TYPE REF TO zif_abapgit_xml_input,
          lt_results  TYPE zif_abapgit_definitions=>ty_results_tt,
          li_progress TYPE REF TO zif_abapgit_progress,
          lv_path     TYPE string,
          lt_items    TYPE zif_abapgit_definitions=>ty_items_tt,
          lt_steps_id TYPE zif_abapgit_definitions=>ty_deserialization_step_tt,
          lt_steps    TYPE zif_abapgit_objects=>ty_step_data_tt,
          lx_exc      TYPE REF TO zcx_abapgit_exception.
    DATA: lo_folder_logic TYPE REF TO zcl_abapgit_folder_logic.

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

    lt_remote = io_repo->get_files_remote( ).

    lt_results = zcl_abapgit_file_deserialize=>get_results(
      io_repo = io_repo
      ii_log = ii_log ).

    zcl_abapgit_objects_check=>checks_adjust(
      EXPORTING
        io_repo    = io_repo
        is_checks  = is_checks
      CHANGING
        ct_results = lt_results ).

    li_progress = zcl_abapgit_progress=>get_instance( lines( lt_results ) ).

    lt_items = map_results_to_items( lt_results ).

    check_objects_locked( iv_language = io_repo->get_dot_abapgit( )->get_master_language( )
                          it_items    = lt_items ).

    lo_folder_logic = zcl_abapgit_folder_logic=>get_instance( ).
    LOOP AT lt_results ASSIGNING <ls_result>.
      li_progress->show( iv_current = sy-tabix
                         iv_text    = |Deserialize { <ls_result>-obj_name }| ).

      CLEAR ls_item.
      ls_item-obj_type = <ls_result>-obj_type.
      ls_item-obj_name = <ls_result>-obj_name.

      "error handling & logging added
      TRY.
          " If package does not exist yet, it will be created with this call
          lv_package = lo_folder_logic->path_to_package(
            iv_top  = io_repo->get_package( )
            io_dot  = io_repo->get_dot_abapgit( )
            iv_path = <ls_result>-path ).

          check_main_package(
            iv_package  = lv_package
            iv_obj_type = ls_item-obj_type ).

          IF ls_item-obj_type = 'DEVC'.
            " Packages have the same filename across different folders. The path needs to be supplied
            " to find the correct file.
            lv_path = <ls_result>-path.
          ENDIF.

          CREATE OBJECT lo_files
            EXPORTING
              is_item = ls_item
              iv_path = lv_path.
          lo_files->set_files( lt_remote ).

          "analyze XML in order to instantiate the proper serializer
          lo_xml = lo_files->read_xml( ).

          li_obj = create_object( is_item     = ls_item
                                  iv_language = io_repo->get_dot_abapgit( )->get_master_language( )
                                  is_metadata = lo_xml->get_metadata( ) ).

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
            IF <ls_step>-is_ddic = abap_true AND li_obj->get_metadata( )-ddic = abap_false.
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

    "run deserialize for all steps and it's objects
    SORT lt_steps BY order.
    LOOP AT lt_steps ASSIGNING <ls_step>.
      deserialize_objects( EXPORTING is_step = <ls_step>
                                     ii_log  = ii_log
                           CHANGING ct_files = rt_accessed_files ).
    ENDLOOP.

    update_package_tree( io_repo->get_package( ) ).

    SORT rt_accessed_files BY path ASCENDING filename ASCENDING.
    DELETE ADJACENT DUPLICATES FROM rt_accessed_files. " Just in case

    zcl_abapgit_default_transport=>get_instance( )->reset( ).

    li_progress->off( ).

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

    li_progress = zcl_abapgit_progress=>get_instance( lines( is_step-objects ) ).

    LOOP AT is_step-objects ASSIGNING <ls_obj>.
      li_progress->show(
        iv_current = sy-tabix
        iv_text    = |Deserialize { is_step-descr } - { <ls_obj>-item-obj_name }| ).

      TRY.
          <ls_obj>-obj->deserialize( iv_package = <ls_obj>-package
                                     io_xml     = <ls_obj>-xml
                                     iv_step    = is_step-step_id
                                     ii_log     = ii_log ).
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

    CASE is_step-step_id.
      WHEN zif_abapgit_object=>gc_step_id-ddic.
        zcl_abapgit_objects_activation=>activate( is_step-is_ddic ).
      WHEN zif_abapgit_object=>gc_step_id-abap.
        zcl_abapgit_objects_activation=>activate( is_step-is_ddic ).
      WHEN zif_abapgit_object=>gc_step_id-late.
        " late can have both DDIC (like TABL with REF TO) and non-DDIC objects
        zcl_abapgit_objects_activation=>activate( abap_true ).
        zcl_abapgit_objects_activation=>activate( abap_false ).
    ENDCASE.

*   Call postprocessing
    li_exit = zcl_abapgit_exit=>get_instance( ).

    li_exit->deserialize_postprocess( is_step = is_step
                                      ii_log  = ii_log ).

  ENDMETHOD.


  METHOD exists.

    DATA: li_obj TYPE REF TO zif_abapgit_object.


    TRY.
        li_obj = create_object( is_item = is_item
                                iv_language = zif_abapgit_definitions=>c_english ).
        rv_bool = li_obj->exists( ).
      CATCH zcx_abapgit_exception.
* ignore all errors and assume the object exists
        rv_bool = abap_true.
    ENDTRY.

  ENDMETHOD.


  METHOD get_deserialize_steps.
    FIELD-SYMBOLS: <ls_step>    TYPE LINE OF zif_abapgit_objects=>ty_step_data_tt.

    APPEND INITIAL LINE TO rt_steps ASSIGNING <ls_step>.
    <ls_step>-step_id      = zif_abapgit_object=>gc_step_id-ddic.
    <ls_step>-descr        = 'Import DDIC objects'.
    <ls_step>-is_ddic      = abap_true.
    <ls_step>-syntax_check = abap_false.
    <ls_step>-order        = 1.

    APPEND INITIAL LINE TO rt_steps ASSIGNING <ls_step>.
    <ls_step>-step_id      = zif_abapgit_object=>gc_step_id-abap.
    <ls_step>-descr        = 'Import objects main'.
    <ls_step>-is_ddic      = abap_false.
    <ls_step>-syntax_check = abap_false.
    <ls_step>-order        = 2.

    APPEND INITIAL LINE TO rt_steps ASSIGNING <ls_step>.
    <ls_step>-step_id      = zif_abapgit_object=>gc_step_id-late.
    <ls_step>-descr        = 'Import late objects'.
    <ls_step>-is_ddic      = abap_false.
    <ls_step>-syntax_check = abap_true.
    <ls_step>-order        = 3.
  ENDMETHOD.


  METHOD is_active.

    DATA: li_object TYPE REF TO zif_abapgit_object.

    TRY.
        li_object = create_object( is_item     = is_item
                                   iv_language = sy-langu ).

        rv_active = li_object->is_active( ).
      CATCH cx_sy_dyn_call_illegal_method
            cx_sy_ref_is_initial
            zcx_abapgit_exception.
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


  METHOD jump.

    DATA: li_obj              TYPE REF TO zif_abapgit_object,
          lv_adt_jump_enabled TYPE abap_bool.

    li_obj = create_object( is_item     = is_item
                            iv_language = zif_abapgit_definitions=>c_english ).

    IF li_obj->exists( ) = abap_false.
      zcx_abapgit_exception=>raise( |Object { is_item-obj_type } { is_item-obj_name } doesn't exist| ).
    ENDIF.

    lv_adt_jump_enabled = zcl_abapgit_persist_settings=>get_instance( )->read( )->get_adt_jump_enabled( ).

    IF lv_adt_jump_enabled = abap_true.

      TRY.
          zcl_abapgit_objects_super=>jump_adt(
            iv_obj_name     = is_item-obj_name
            iv_obj_type     = is_item-obj_type
            iv_sub_obj_name = iv_sub_obj_name
            iv_sub_obj_type = iv_sub_obj_type
            iv_line_number  = iv_line_number ).
        CATCH zcx_abapgit_exception.
          li_obj->jump( ).
      ENDTRY.

    ELSEIF iv_line_number IS NOT INITIAL
        AND iv_sub_obj_type IS NOT INITIAL
        AND iv_sub_obj_name IS NOT INITIAL.

      " For the line navigation we have to supply the sub object type (i_sub_obj_type).
      " If we use is_item-obj_type it navigates only to the object.

      CALL FUNCTION 'RS_TOOL_ACCESS'
        EXPORTING
          operation           = 'SHOW'
          object_name         = is_item-obj_name
          object_type         = iv_sub_obj_type
          include             = iv_sub_obj_name
          position            = iv_line_number
          in_new_window       = abap_true
        EXCEPTIONS
          not_executed        = 1
          invalid_object_type = 2
          OTHERS              = 3.

      IF sy-subrc <> 0.
        zcx_abapgit_exception=>raise_t100( ).
      ENDIF.

    ELSE.
      li_obj->jump( ).
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
          li_xml         TYPE REF TO zif_abapgit_xml_output,
          lo_files       TYPE REF TO zcl_abapgit_objects_files,
          ls_i18n_params TYPE zif_abapgit_definitions=>ty_i18n_params.

    FIELD-SYMBOLS: <ls_file> LIKE LINE OF rs_files_and_item-files.

    rs_files_and_item-item = is_item.

    IF is_supported( rs_files_and_item-item ) = abap_false.
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
    ls_i18n_params-main_language_only    = iv_serialize_master_lang_only.
    ls_i18n_params-translation_languages = it_translation_langs.

    li_xml->i18n_params( ls_i18n_params ).

    li_obj->serialize( li_xml ).
    lo_files->add_xml( ii_xml      = li_xml
                       is_metadata = li_obj->get_metadata( ) ).

    rs_files_and_item-files = lo_files->get_files( ).

    check_duplicates( rs_files_and_item-files ).

    rs_files_and_item-item-inactive = boolc( li_obj->is_active( ) = abap_false ).

    LOOP AT rs_files_and_item-files ASSIGNING <ls_file>.
      <ls_file>-sha1 = zcl_abapgit_hash=>sha1_blob( <ls_file>-data ).
    ENDLOOP.

  ENDMETHOD.


  METHOD supported_list.

    DATA: lt_objects TYPE STANDARD TABLE OF ko100,
          ls_item    TYPE zif_abapgit_definitions=>ty_item.

    FIELD-SYMBOLS <ls_object> LIKE LINE OF lt_objects.

    IF gt_supported_obj_types IS NOT INITIAL.
      rt_types = gt_supported_obj_types.
      RETURN.
    ENDIF.

    CALL FUNCTION 'TR_OBJECT_TABLE'
      TABLES
        wt_object_text = lt_objects
      EXCEPTIONS
        OTHERS         = 1 ##FM_SUBRC_OK.

    LOOP AT lt_objects ASSIGNING <ls_object> WHERE pgmid = 'R3TR'.
      ls_item-obj_type = <ls_object>-object.

      IF is_supported( ls_item ) = abap_true.
        INSERT <ls_object>-object INTO TABLE rt_types.
      ENDIF.
    ENDLOOP.
    gt_supported_obj_types = rt_types.

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
