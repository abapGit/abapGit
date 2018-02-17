CLASS zcl_abapgit_objects DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES:
      ty_types_tt TYPE STANDARD TABLE OF tadir-object WITH DEFAULT KEY .
    TYPES:
      BEGIN OF ty_deserialization,
        obj     TYPE REF TO zif_abapgit_object,
        xml     TYPE REF TO zcl_abapgit_xml_input,
        package TYPE devclass,
        item    TYPE zif_abapgit_definitions=>ty_item,
      END OF ty_deserialization .
    TYPES:
      ty_deserialization_tt TYPE STANDARD TABLE OF ty_deserialization WITH DEFAULT KEY .

    CLASS-METHODS serialize
      IMPORTING
        !is_item        TYPE zif_abapgit_definitions=>ty_item
        !iv_language    TYPE spras
        !io_log         TYPE REF TO zcl_abapgit_log OPTIONAL
      RETURNING
        VALUE(rt_files) TYPE zif_abapgit_definitions=>ty_files_tt
      RAISING
        zcx_abapgit_exception .
    CLASS-METHODS deserialize
      IMPORTING
        !io_repo                 TYPE REF TO zcl_abapgit_repo
      RETURNING
        VALUE(rt_accessed_files) TYPE zif_abapgit_definitions=>ty_file_signatures_tt
      RAISING
        zcx_abapgit_exception .
    CLASS-METHODS delete
      IMPORTING
        !it_tadir TYPE zif_abapgit_definitions=>ty_tadir_tt
      RAISING
        zcx_abapgit_exception .
    CLASS-METHODS jump
      IMPORTING
        !is_item TYPE zif_abapgit_definitions=>ty_item
      RAISING
        zcx_abapgit_exception .
    CLASS-METHODS changed_by
      IMPORTING
        !is_item       TYPE zif_abapgit_definitions=>ty_item
      RETURNING
        VALUE(rv_user) TYPE xubname
      RAISING
        zcx_abapgit_exception .
    CLASS-METHODS has_changed_since
      IMPORTING
        !is_item          TYPE zif_abapgit_definitions=>ty_item
        !iv_timestamp     TYPE timestamp
      RETURNING
        VALUE(rv_changed) TYPE abap_bool
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
  PRIVATE SECTION.

    CLASS-METHODS check_duplicates
      IMPORTING it_files TYPE zif_abapgit_definitions=>ty_files_tt
      RAISING   zcx_abapgit_exception.

    CLASS-METHODS create_object
      IMPORTING is_item        TYPE zif_abapgit_definitions=>ty_item
                iv_language    TYPE spras
                is_metadata    TYPE zif_abapgit_definitions=>ty_metadata OPTIONAL
                iv_native_only TYPE abap_bool DEFAULT abap_false
      RETURNING VALUE(ri_obj)  TYPE REF TO zif_abapgit_object
      RAISING   zcx_abapgit_exception.

    CLASS-METHODS
      prioritize_deser
        IMPORTING it_results        TYPE zif_abapgit_definitions=>ty_results_tt
        RETURNING VALUE(rt_results) TYPE zif_abapgit_definitions=>ty_results_tt.

    CLASS-METHODS class_name
      IMPORTING is_item              TYPE zif_abapgit_definitions=>ty_item
      RETURNING VALUE(rv_class_name) TYPE string.

    CLASS-METHODS warning_overwrite
      CHANGING ct_results TYPE zif_abapgit_definitions=>ty_results_tt
      RAISING  zcx_abapgit_exception.

    CLASS-METHODS warning_package
      IMPORTING is_item          TYPE zif_abapgit_definitions=>ty_item
                iv_package       TYPE devclass
      RETURNING VALUE(rv_cancel) TYPE abap_bool
      RAISING   zcx_abapgit_exception.

    CLASS-METHODS update_package_tree
      IMPORTING iv_package TYPE devclass.

    CLASS-METHODS delete_obj
      IMPORTING is_item TYPE zif_abapgit_definitions=>ty_item
      RAISING   zcx_abapgit_exception.

    CLASS-METHODS compare_remote_to_local
      IMPORTING
        io_object TYPE REF TO zif_abapgit_object
        it_remote TYPE zif_abapgit_definitions=>ty_files_tt
        is_result TYPE zif_abapgit_definitions=>ty_result
      RAISING
        zcx_abapgit_exception.

    CLASS-METHODS deserialize_objects
      IMPORTING it_objects TYPE ty_deserialization_tt
                iv_ddic    TYPE abap_bool DEFAULT abap_false
                iv_descr   TYPE string
      CHANGING  ct_files   TYPE zif_abapgit_definitions=>ty_file_signatures_tt
      RAISING   zcx_abapgit_exception.

ENDCLASS.



CLASS ZCL_ABAPGIT_OBJECTS IMPLEMENTATION.


  METHOD changed_by.

    DATA: li_obj TYPE REF TO zif_abapgit_object.


    IF is_item IS INITIAL.
* eg. ".abapgit.xml" file
      rv_user = zcl_abapgit_objects_super=>c_user_unknown.
    ELSE.
      li_obj = create_object( is_item     = is_item
                              iv_language = zif_abapgit_definitions=>gc_english ).
      rv_user = li_obj->changed_by( ).
    ENDIF.

    ASSERT NOT rv_user IS INITIAL.

* todo, fallback to looking at transports if rv_user = 'UNKNOWN'?

  ENDMETHOD.


  METHOD check_duplicates.

    DATA: lt_files TYPE zif_abapgit_definitions=>ty_files_tt.


    lt_files[] = it_files[].
    SORT lt_files BY path ASCENDING filename ASCENDING.
    DELETE ADJACENT DUPLICATES FROM lt_files COMPARING path filename.
    IF lines( lt_files ) <> lines( it_files ).
      zcx_abapgit_exception=>raise( 'Duplicates' ).
    ENDIF.

  ENDMETHOD.


  METHOD class_name.

    CONCATENATE 'ZCL_ABAPGIT_OBJECT_' is_item-obj_type INTO rv_class_name. "#EC NOTEXT

  ENDMETHOD.                    "class_name


  METHOD compare_remote_to_local.
* this method is used for comparing local with remote objects
* before pull, this is useful eg. when overwriting a TABL object.
* only the main XML file is used for comparison

    DATA: ls_remote_file       TYPE zif_abapgit_definitions=>ty_file,
          lo_remote_version    TYPE REF TO zcl_abapgit_xml_input,
          lv_count             TYPE i,
          li_comparison_result TYPE REF TO zif_abapgit_comparison_result.


    FIND ALL OCCURRENCES OF '.' IN is_result-filename MATCH COUNT lv_count.

    IF is_result-filename CS '.XML' AND lv_count = 2.
      IF io_object->exists( ) = abap_false.
        RETURN.
      ENDIF.

      READ TABLE it_remote WITH KEY filename = is_result-filename INTO ls_remote_file.

      "if file does not exist in remote, we don't need to validate
      IF sy-subrc = 0.
        CREATE OBJECT lo_remote_version
          EXPORTING
            iv_xml = zcl_abapgit_convert=>xstring_to_string_utf8( ls_remote_file-data ).
        li_comparison_result = io_object->compare_to_remote_version( lo_remote_version ).
        li_comparison_result->show_confirmation_dialog( ).

        IF li_comparison_result->is_result_complete_halt( ) = abap_true.
          zcx_abapgit_exception=>raise( 'Deserialization aborted by user' ).
        ENDIF.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD create_object.

    TYPES: BEGIN OF ty_obj_serializer_map,
             item     LIKE is_item,
             metadata LIKE is_metadata,
           END OF ty_obj_serializer_map.

    STATICS st_obj_serializer_map
      TYPE SORTED TABLE OF ty_obj_serializer_map WITH UNIQUE KEY item.

    DATA: lv_message            TYPE string,
          lv_class_name         TYPE string,
          ls_obj_serializer_map LIKE LINE OF st_obj_serializer_map.


    READ TABLE st_obj_serializer_map
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
      INSERT ls_obj_serializer_map INTO TABLE st_obj_serializer_map.

      lv_class_name = is_metadata-class.
    ELSE.
      lv_class_name = class_name( is_item ).
    ENDIF.

*    IF zcl_abapgit_persist_settings=>get_instance( )->read( )->get_experimental_features( ) = abap_true
*        AND is_item-obj_type = 'CLAS'.
*      lv_class_name = 'LCL_OBJECT_CLAS_NEW'.
*    ENDIF.

    REPLACE FIRST OCCURRENCE OF 'LCL' IN lv_class_name WITH 'ZCL_ABAPGIT'.

    TRY.
        CREATE OBJECT ri_obj TYPE (lv_class_name)
          EXPORTING
            is_item     = is_item
            iv_language = iv_language.
      CATCH cx_sy_create_object_error.
        lv_message = |Object type { is_item-obj_type } not supported, serialize|. "#EC NOTEXT
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

  ENDMETHOD.                    "create_object


  METHOD delete.

    DATA: ls_item     TYPE zif_abapgit_definitions=>ty_item,
          lo_progress TYPE REF TO zcl_abapgit_progress,
          lt_tadir    LIKE it_tadir.

    FIELD-SYMBOLS: <ls_tadir> LIKE LINE OF it_tadir.

    lt_tadir[] = it_tadir[].

    zcl_abapgit_dependencies=>resolve( CHANGING ct_tadir = lt_tadir ).

    CREATE OBJECT lo_progress
      EXPORTING
        iv_total = lines( lt_tadir ).

    LOOP AT lt_tadir ASSIGNING <ls_tadir>.
      lo_progress->show( iv_current = sy-tabix
                         iv_text    = |Delete { <ls_tadir>-obj_name }| ) ##NO_TEXT.

      CLEAR ls_item.
      ls_item-obj_type = <ls_tadir>-object.
      ls_item-obj_name = <ls_tadir>-obj_name.
      delete_obj( ls_item ).

* make sure to save object deletions
      COMMIT WORK.
    ENDLOOP.

  ENDMETHOD.                    "delete


  METHOD delete_obj.

    DATA: li_obj TYPE REF TO zif_abapgit_object.


    IF is_supported( is_item ) = abap_true.
      li_obj = create_object( is_item     = is_item
                              iv_language = zif_abapgit_definitions=>gc_english ).

      li_obj->delete( ).

      IF li_obj->get_metadata( )-delete_tadir = abap_true.
        CALL FUNCTION 'TR_TADIR_INTERFACE'
          EXPORTING
            wi_delete_tadir_entry = abap_true
            wi_tadir_pgmid        = 'R3TR'
            wi_tadir_object       = is_item-obj_type
            wi_tadir_obj_name     = is_item-obj_name
            wi_test_modus         = abap_false.
      ENDIF.
    ENDIF.

  ENDMETHOD.                    "delete


  METHOD deserialize.

    DATA: ls_item     TYPE zif_abapgit_definitions=>ty_item,
          lv_cancel   TYPE abap_bool,
          li_obj      TYPE REF TO zif_abapgit_object,
          lt_remote   TYPE zif_abapgit_definitions=>ty_files_tt,
          lv_package  TYPE devclass,
          lo_files    TYPE REF TO zcl_abapgit_objects_files,
          lo_xml      TYPE REF TO zcl_abapgit_xml_input,
          lt_results  TYPE zif_abapgit_definitions=>ty_results_tt,
          lt_ddic     TYPE TABLE OF ty_deserialization,
          lt_rest     TYPE TABLE OF ty_deserialization,
          lt_late     TYPE TABLE OF ty_deserialization,
          lo_progress TYPE REF TO zcl_abapgit_progress,
          lv_path     TYPE string.

    FIELD-SYMBOLS: <ls_result> TYPE zif_abapgit_definitions=>ty_result,
                   <ls_deser>  LIKE LINE OF lt_late.

    lv_package = io_repo->get_package( ).

    zcl_abapgit_default_task=>get_instance( )->set( lv_package ).

    zcl_abapgit_objects_activation=>clear( ).

    lt_remote = io_repo->get_files_remote( ).

    lt_results = zcl_abapgit_file_status=>status( io_repo ).
    DELETE lt_results WHERE match = abap_true.     " Full match
    SORT lt_results BY obj_type ASCENDING obj_name ASCENDING.
    DELETE ADJACENT DUPLICATES FROM lt_results COMPARING obj_type obj_name.

    lt_results = prioritize_deser( lt_results ).

    warning_overwrite( CHANGING ct_results = lt_results ).

    CREATE OBJECT lo_progress
      EXPORTING
        iv_total = lines( lt_results ).

    LOOP AT lt_results ASSIGNING <ls_result> WHERE obj_type IS NOT INITIAL
        AND NOT ( lstate = zif_abapgit_definitions=>gc_state-added AND rstate IS INITIAL ).
      lo_progress->show( iv_current = sy-tabix
                         iv_text    = |Deserialize { <ls_result>-obj_name }| ) ##NO_TEXT.

      CLEAR ls_item.
      ls_item-obj_type = <ls_result>-obj_type.
      ls_item-obj_name = <ls_result>-obj_name.
* handle namespaces
      REPLACE ALL OCCURRENCES OF '#' IN ls_item-obj_name WITH '/'.

      lv_package = zcl_abapgit_folder_logic=>path_to_package(
        iv_top  = io_repo->get_package( )
        io_dot  = io_repo->get_dot_abapgit( )
        iv_path = <ls_result>-path ).

      lv_cancel = warning_package( is_item    = ls_item
                                   iv_package = lv_package ).
      IF lv_cancel = abap_true.
        zcx_abapgit_exception=>raise( 'cancelled' ).
      ENDIF.

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

* Analyze XML in order to instantiate the proper serializer
      lo_xml = lo_files->read_xml( ).

      li_obj = create_object( is_item     = ls_item
                              iv_language = io_repo->get_master_language( )
                              is_metadata = lo_xml->get_metadata( ) ).

      compare_remote_to_local(
        io_object = li_obj
        it_remote = lt_remote
        is_result = <ls_result> ).

      li_obj->mo_files = lo_files.

      IF li_obj->get_metadata( )-late_deser = abap_true.
        APPEND INITIAL LINE TO lt_late ASSIGNING <ls_deser>.
      ELSEIF li_obj->get_metadata( )-ddic = abap_true.
        APPEND INITIAL LINE TO lt_ddic ASSIGNING <ls_deser>.
      ELSE.
        APPEND INITIAL LINE TO lt_rest ASSIGNING <ls_deser>.
      ENDIF.
      <ls_deser>-item    = ls_item.
      <ls_deser>-obj     = li_obj.
      <ls_deser>-xml     = lo_xml.
      <ls_deser>-package = lv_package.

      CLEAR: lv_path, lv_package.
    ENDLOOP.

    deserialize_objects( EXPORTING it_objects = lt_ddic
                                   iv_ddic    = abap_true
                                   iv_descr   = 'DDIC'
                         CHANGING ct_files = rt_accessed_files ).

    deserialize_objects( EXPORTING it_objects = lt_rest
                                   iv_descr   = 'Objects'
                         CHANGING ct_files = rt_accessed_files ).

    deserialize_objects( EXPORTING it_objects = lt_late
                                   iv_descr   = 'Late'
                         CHANGING ct_files = rt_accessed_files ).

    update_package_tree( io_repo->get_package( ) ).

    SORT rt_accessed_files BY path ASCENDING filename ASCENDING.
    DELETE ADJACENT DUPLICATES FROM rt_accessed_files. " Just in case

    zcl_abapgit_default_task=>get_instance( )->reset( ).

  ENDMETHOD.                    "deserialize


  METHOD deserialize_objects.

    DATA: lo_progress TYPE REF TO zcl_abapgit_progress.

    FIELD-SYMBOLS: <ls_obj> LIKE LINE OF it_objects.


    zcl_abapgit_objects_activation=>clear( ).

    CREATE OBJECT lo_progress
      EXPORTING
        iv_total = lines( it_objects ).

    LOOP AT it_objects ASSIGNING <ls_obj>.
      lo_progress->show(
        iv_current = sy-tabix
        iv_text    = |Deserialize { iv_descr } - { <ls_obj>-item-obj_name }| ) ##NO_TEXT.

      <ls_obj>-obj->deserialize( iv_package = <ls_obj>-package
                                 io_xml     = <ls_obj>-xml ).
      APPEND LINES OF <ls_obj>-obj->mo_files->get_accessed_files( ) TO ct_files.
    ENDLOOP.

    zcl_abapgit_objects_activation=>activate( iv_ddic ).

  ENDMETHOD.


  METHOD exists.

    DATA: li_obj TYPE REF TO zif_abapgit_object.


    TRY.
        li_obj = create_object( is_item = is_item
                                iv_language = zif_abapgit_definitions=>gc_english ).
        rv_bool = li_obj->exists( ).
      CATCH zcx_abapgit_exception.
* ignore all errors and assume the object exists
        rv_bool = abap_true.
    ENDTRY.

  ENDMETHOD.                    "exists


  METHOD has_changed_since.
    rv_changed = abap_true. " Assume changed

    IF is_supported( is_item ) = abap_false.
      RETURN. " Will requre serialize which will log the error
    ENDIF.

    rv_changed = create_object(
      is_item     = is_item
      iv_language = zif_abapgit_definitions=>gc_english )->has_changed_since( iv_timestamp ).

  ENDMETHOD.  "has_changed_since


  METHOD is_supported.

    TRY.
        create_object( is_item        = is_item
                       iv_language    = zif_abapgit_definitions=>gc_english
                       iv_native_only = iv_native_only ).
        rv_bool = abap_true.
      CATCH zcx_abapgit_exception.
        rv_bool = abap_false.
    ENDTRY.

  ENDMETHOD.                    "is_supported


  METHOD jump.

    DATA: li_obj              TYPE REF TO zif_abapgit_object,
          lv_adt_jump_enabled TYPE abap_bool.

    li_obj = create_object( is_item     = is_item
                            iv_language = zif_abapgit_definitions=>gc_english ).

    lv_adt_jump_enabled = zcl_abapgit_persist_settings=>get_instance( )->read( )->get_adt_jump_enabled( ).

    IF lv_adt_jump_enabled = abap_true.
      TRY.
          zcl_abapgit_objects_super=>jump_adt(
            i_obj_name = is_item-obj_name
            i_obj_type = is_item-obj_type ).
        CATCH zcx_abapgit_exception.
          li_obj->jump( ).
      ENDTRY.
    ELSE.
      li_obj->jump( ).
    ENDIF.

  ENDMETHOD.                    "jump


  METHOD prioritize_deser.

    FIELD-SYMBOLS: <ls_result> LIKE LINE OF it_results.

* XSLT has to be handled before CLAS/PROG
    LOOP AT it_results ASSIGNING <ls_result> WHERE obj_type = 'XSLT'.
      APPEND <ls_result> TO rt_results.
    ENDLOOP.

* PROG before internet services, as the services might use the screens
    LOOP AT it_results ASSIGNING <ls_result> WHERE obj_type = 'PROG'.
      APPEND <ls_result> TO rt_results.
    ENDLOOP.

* ISAP has to be handled before ISRP
    LOOP AT it_results ASSIGNING <ls_result> WHERE obj_type = 'IASP'.
      APPEND <ls_result> TO rt_results.
    ENDLOOP.

* PINF has to be handled before DEVC for package interface usage
    LOOP AT it_results ASSIGNING <ls_result> WHERE obj_type = 'PINF'.
      APPEND <ls_result> TO rt_results.
    ENDLOOP.

    LOOP AT it_results ASSIGNING <ls_result>
        WHERE obj_type <> 'IASP'
        AND obj_type <> 'PROG'
        AND obj_type <> 'XSLT'
        AND obj_type <> 'PINF'.
      APPEND <ls_result> TO rt_results.
    ENDLOOP.

  ENDMETHOD.                    "prioritize_deser


  METHOD serialize.

    DATA: li_obj   TYPE REF TO zif_abapgit_object,
          lo_xml   TYPE REF TO zcl_abapgit_xml_output,
          lo_files TYPE REF TO zcl_abapgit_objects_files.


    IF is_supported( is_item ) = abap_false.
      IF NOT io_log IS INITIAL.
        io_log->add( iv_msg = |Object type ignored, not supported: { is_item-obj_type
                       }-{ is_item-obj_name }|
                     iv_type = 'E' ).
      ENDIF.
      RETURN.
    ENDIF.

    CREATE OBJECT lo_files
      EXPORTING
        is_item = is_item.

    li_obj = create_object( is_item = is_item
                            iv_language = iv_language ).
    li_obj->mo_files = lo_files.
    CREATE OBJECT lo_xml.
    li_obj->serialize( lo_xml ).
    lo_files->add_xml( io_xml      = lo_xml
                       is_metadata = li_obj->get_metadata( ) ).

    rt_files = lo_files->get_files( ).

    check_duplicates( rt_files ).

  ENDMETHOD.                    "serialize


  METHOD supported_list.

    DATA: lv_type  LIKE LINE OF rt_types,
          lt_snode TYPE TABLE OF snode.

    FIELD-SYMBOLS: <ls_snode> LIKE LINE OF lt_snode.


    CALL FUNCTION 'WB_TREE_ACTUALIZE'
      EXPORTING
        tree_name              = 'PG_ZABAPGIT'
        without_crossreference = abap_true
        with_tcode_index       = abap_true
      TABLES
        p_tree                 = lt_snode.

    DELETE lt_snode WHERE type <> 'OPL'
      OR name NP 'LCL_OBJECT_++++'.

    LOOP AT lt_snode ASSIGNING <ls_snode>.
      lv_type = <ls_snode>-name+11.
      APPEND lv_type TO rt_types.
    ENDLOOP.

  ENDMETHOD.                    "supported_list


  METHOD update_package_tree.

    DATA: lt_packages TYPE zif_abapgit_sap_package=>ty_devclass_tt,
          lv_package  LIKE LINE OF lt_packages,
          lv_tree     TYPE dirtree-tname.


    lt_packages = zcl_abapgit_sap_package=>get( iv_package )->list_subpackages( ).
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

  ENDMETHOD.                    "update_package_tree


  METHOD warning_overwrite.

    DATA: lt_results_overwrite   LIKE ct_results,
          lt_confirmed_overwrite LIKE ct_results,
          lt_columns             TYPE stringtab.

    FIELD-SYMBOLS: <ls_result>  LIKE LINE OF ct_results.

    LOOP AT ct_results ASSIGNING <ls_result>
        WHERE NOT obj_type IS INITIAL.

      IF <ls_result>-lstate IS NOT INITIAL
          AND <ls_result>-lstate <> zif_abapgit_definitions=>gc_state-deleted
          AND NOT ( <ls_result>-lstate = zif_abapgit_definitions=>gc_state-added
          AND <ls_result>-rstate IS INITIAL ).

        "current object has been modified locally, add to table for popup
        APPEND <ls_result> TO lt_results_overwrite.
      ENDIF.

    ENDLOOP.

    IF lines( lt_results_overwrite ) > 0.

      INSERT `OBJ_TYPE` INTO TABLE lt_columns.
      INSERT `OBJ_NAME` INTO TABLE lt_columns.

      "all returned objects will be overwritten
      zcl_abapgit_popups=>popup_to_select_from_list(
        EXPORTING
          it_list               = lt_results_overwrite
          i_header_text         = |The following Objects have been modified locally.|
                              && | Select the Objects which should be overwritten.|
          i_select_column_text  = 'Overwrite?'
          it_columns_to_display = lt_columns
        IMPORTING
          et_list               = lt_confirmed_overwrite ).

      LOOP AT lt_results_overwrite ASSIGNING <ls_result>.
        READ TABLE lt_confirmed_overwrite TRANSPORTING NO FIELDS
             WITH KEY obj_type = <ls_result>-obj_type
                      obj_name = <ls_result>-obj_name.
        IF sy-subrc <> 0.
          DELETE TABLE ct_results FROM <ls_result>.
        ENDIF.
      ENDLOOP.
    ENDIF.

  ENDMETHOD.


  METHOD warning_package.

    DATA: lv_question TYPE c LENGTH 200,
          lv_answer   TYPE c,
          ls_tadir    TYPE tadir.


    ls_tadir = zcl_abapgit_tadir=>read_single(
      iv_object   = is_item-obj_type
      iv_obj_name = is_item-obj_name ).

    IF NOT ls_tadir IS INITIAL AND ls_tadir-devclass <> iv_package.
      CONCATENATE 'Overwrite object' is_item-obj_type is_item-obj_name
        'from package' ls_tadir-devclass
        INTO lv_question SEPARATED BY space.                "#EC NOTEXT

      lv_answer = zcl_abapgit_popups=>popup_to_confirm(
        titlebar              = 'Warning'
        text_question         = lv_question
        text_button_1         = 'Ok'
        icon_button_1         = 'ICON_DELETE'
        text_button_2         = 'Cancel'
        icon_button_2         = 'ICON_CANCEL'
        default_button        = '2'
        display_cancel_button = abap_false ).               "#EC NOTEXT

      IF lv_answer = '2'.
        rv_cancel = abap_true.
      ENDIF.

    ENDIF.

  ENDMETHOD.                    "check_warning
ENDCLASS.
