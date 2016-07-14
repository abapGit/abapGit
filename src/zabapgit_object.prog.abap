*&---------------------------------------------------------------------*
*&  Include           ZABAPGIT_OBJECT
*&---------------------------------------------------------------------*

*----------------------------------------------------------------------*
*       CLASS lcl_objects IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_objects IMPLEMENTATION.

  METHOD warning_overwrite.

    DATA: lv_index    TYPE i,
          lv_answer   TYPE c,
          lv_question TYPE string,
          lt_before   TYPE lcl_persistence_repo=>ty_local_checksum_tt,
          lt_current  TYPE lcl_persistence_repo=>ty_local_checksum_tt.

    FIELD-SYMBOLS: <ls_before>  LIKE LINE OF lt_before,
                   <ls_current> LIKE LINE OF lt_current,
                   <ls_result>  LIKE LINE OF ct_results.


    lt_before = io_repo->get_local_checksums( ).
    lt_current = io_repo->build_local_checksums( ).

    LOOP AT ct_results ASSIGNING <ls_result>.
      lv_index = sy-tabix.

      READ TABLE lt_before ASSIGNING <ls_before>
        WITH KEY item-obj_type = <ls_result>-obj_type
        item-obj_name = <ls_result>-obj_name.
      IF sy-subrc <> 0.
        CONTINUE.
      ENDIF.

      READ TABLE lt_current ASSIGNING <ls_current>
        WITH KEY item-obj_type = <ls_result>-obj_type
        item-obj_name = <ls_result>-obj_name.
      IF sy-subrc <> 0.
        CONTINUE.
      ENDIF.

      IF <ls_before>-sha1 <> <ls_current>-sha1.
        lv_question = |It looks like object { <ls_result>-obj_type
          } { <ls_result>-obj_name
          } has been modified locally, overwrite object?|.

        CALL FUNCTION 'POPUP_TO_CONFIRM'
          EXPORTING
            titlebar              = 'Warning'
            text_question         = lv_question
            display_cancel_button = abap_false
          IMPORTING
            answer                = lv_answer
          EXCEPTIONS
            text_not_found        = 1
            OTHERS                = 2 ##NO_TEXT.
        IF sy-subrc <> 0.
          _raise 'error from POPUP_TO_CONFIRM'.
        ENDIF.

        IF lv_answer = '2'.
          DELETE ct_results INDEX lv_index.
        ENDIF.

      ENDIF.

    ENDLOOP.

  ENDMETHOD.

  METHOD warning_package.

    DATA: lv_question TYPE c LENGTH 200,
          lv_answer   TYPE c,
          ls_tadir    TYPE tadir.


    ls_tadir = lcl_tadir=>read_single( iv_object   = is_item-obj_type
                                       iv_obj_name = is_item-obj_name ).
    IF NOT ls_tadir IS INITIAL AND ls_tadir-devclass <> iv_package.
      CONCATENATE 'Overwrite object' is_item-obj_type is_item-obj_name
        'from package' ls_tadir-devclass
        INTO lv_question SEPARATED BY space.                "#EC NOTEXT

      CALL FUNCTION 'POPUP_TO_CONFIRM'
        EXPORTING
          titlebar              = 'Warning'
          text_question         = lv_question
          text_button_1         = 'Ok'
          icon_button_1         = 'ICON_DELETE'
          text_button_2         = 'Cancel'
          icon_button_2         = 'ICON_CANCEL'
          default_button        = '2'
          display_cancel_button = abap_false
        IMPORTING
          answer                = lv_answer
        EXCEPTIONS
          text_not_found        = 1
          OTHERS                = 2.                        "#EC NOTEXT
      IF sy-subrc <> 0.
        _raise 'error from POPUP_TO_CONFIRM'.
      ENDIF.

      IF lv_answer = '2'.
        rv_cancel = abap_true.
      ENDIF.

    ENDIF.

  ENDMETHOD.                    "check_warning

  METHOD update_package_tree.

    DATA: lt_packages TYPE lcl_sap_package=>ty_devclass_tt,
          lv_package  LIKE LINE OF lt_packages,
          lv_tree     TYPE dirtree-tname.


    lt_packages = lcl_sap_package=>list_subpackages( iv_package ).
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

    TRY.
        CREATE OBJECT ri_obj TYPE (lv_class_name)
          EXPORTING
            is_item = is_item
            iv_language = iv_language.
      CATCH cx_sy_create_object_error.
        TRY.
* 2nd step, try looking for plugins
            CREATE OBJECT ri_obj TYPE lcl_objects_bridge
              EXPORTING
                is_item = is_item.
          CATCH cx_sy_create_object_error.
            CONCATENATE 'Object type' is_item-obj_type 'not supported, serialize'
              INTO lv_message
              SEPARATED BY space.                           "#EC NOTEXT
            _raise lv_message.
        ENDTRY.
    ENDTRY.

  ENDMETHOD.                    "create_object

  METHOD is_supported.

    TRY.
        create_object( is_item = is_item
                       iv_language = gc_english ).
        rv_bool = abap_true.
      CATCH lcx_exception.
        rv_bool = abap_false.
    ENDTRY.

  ENDMETHOD.                    "is_supported

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

  METHOD exists.

    DATA: li_obj TYPE REF TO lif_object.


    TRY.
        li_obj = create_object( is_item = is_item
                                iv_language = gc_english ).
        rv_bool = li_obj->exists( ).
      CATCH lcx_exception.
* ignore all errors and assume the object exists
        rv_bool = abap_true.
    ENDTRY.

  ENDMETHOD.                    "exists

  METHOD path_to_package.

    DATA: lv_length TYPE i,
          lv_path   TYPE string.


    lv_length = strlen( iv_start ) - 1.
    lv_path = iv_path+lv_length.

    CONCATENATE iv_top lv_path INTO rv_package.

    TRANSLATE rv_package USING '/_'.

    lv_length = strlen( rv_package ) - 1.

    rv_package = rv_package(lv_length).

    TRANSLATE rv_package TO UPPER CASE.

    IF lcl_sap_package=>exists( rv_package ) = abap_false.
      lcl_sap_package=>create_child( iv_parent = iv_top
                                     iv_child = rv_package ).
    ENDIF.

  ENDMETHOD.

  METHOD class_name.

    CONCATENATE 'LCL_OBJECT_' is_item-obj_type INTO rv_class_name. "#EC NOTEXT

  ENDMETHOD.                    "class_name

  METHOD jump.

    DATA: li_obj TYPE REF TO lif_object.


    li_obj = create_object( is_item     = is_item
                            iv_language = gc_english ).

    li_obj->jump( ).

  ENDMETHOD.                    "jump

  METHOD changed_by.

    DATA: li_obj TYPE REF TO lif_object.


    li_obj = create_object( is_item     = is_item
                            iv_language = gc_english ).

    rv_user = li_obj->changed_by( ).

    ASSERT NOT rv_user IS INITIAL.

* todo, fallback to looking at transports if rv_user = 'UNKNOWN'?

  ENDMETHOD.

  METHOD delete.

    DATA: ls_item  TYPE ty_item,
          lt_tadir LIKE it_tadir.

    FIELD-SYMBOLS: <ls_tadir> LIKE LINE OF it_tadir.


* misuse field KORRNUM to fix deletion sequence

    lt_tadir[] = it_tadir[].

    LOOP AT lt_tadir ASSIGNING <ls_tadir>.
      CASE <ls_tadir>-object.
        WHEN 'IATU'.
          <ls_tadir>-korrnum = '5500'.
        WHEN 'IARP'.
          <ls_tadir>-korrnum = '5510'.
        WHEN 'IASP'.
          <ls_tadir>-korrnum = '5520'.
        WHEN 'SUSC'.
          <ls_tadir>-korrnum = '5000'.
        WHEN 'TTYP' OR 'TABL' OR 'VIEW'.
          <ls_tadir>-korrnum = '7000'.
        WHEN 'DTEL'.
          <ls_tadir>-korrnum = '8000'.
        WHEN 'DOMA'.
          <ls_tadir>-korrnum = '9000'.
        WHEN 'PROG'.
* delete includes after main programs
          SELECT COUNT(*) FROM reposrc
            WHERE progname = <ls_tadir>-obj_name
            AND r3state = 'A'
            AND subc = 'I'.
          IF sy-subrc = 0.
            <ls_tadir>-korrnum = '2000'.
          ELSE.
            <ls_tadir>-korrnum = '1000'.
          ENDIF.
        WHEN OTHERS.
          <ls_tadir>-korrnum = '1000'.
      ENDCASE.
    ENDLOOP.

    resolve_ddic( CHANGING ct_tadir = lt_tadir ).

    SORT lt_tadir BY korrnum ASCENDING.

    LOOP AT lt_tadir ASSIGNING <ls_tadir>.
      lcl_progress=>show( iv_key     = 'Delete'
                          iv_current = sy-tabix
                          iv_total   = lines( lt_tadir )
                          iv_text    = <ls_tadir>-obj_name ) ##NO_TEXT.

      CLEAR ls_item.
      ls_item-obj_type = <ls_tadir>-object.
      ls_item-obj_name = <ls_tadir>-obj_name.
      delete_obj( ls_item ).
    ENDLOOP.

  ENDMETHOD.                    "delete

  METHOD resolve_ddic.
* this will make sure the deletion sequence of structures/tables work
* in case they have dependencies with .INCLUDE

    TYPES: BEGIN OF ty_edge,
             from TYPE ty_item,
             to   TYPE ty_item,
           END OF ty_edge.

    DATA: lt_nodes        TYPE TABLE OF ty_item,
          lt_edges        TYPE TABLE OF ty_edge,
          lt_findstrings  TYPE TABLE OF rsfind,
          lv_plus         TYPE i VALUE 1,
          lv_find_obj_cls TYPE euobj-id,
          lv_index        TYPE i,
          lv_before       TYPE i,
          lt_founds       TYPE TABLE OF rsfindlst,
          lt_scope        TYPE STANDARD TABLE OF seu_obj.

    FIELD-SYMBOLS: <ls_tadir> LIKE LINE OF ct_tadir,
                   <ls_edge>  LIKE LINE OF lt_edges,
                   <ls_found> LIKE LINE OF lt_founds,
                   <ls_node>  LIKE LINE OF lt_nodes.


* build nodes
    LOOP AT ct_tadir ASSIGNING <ls_tadir>
        WHERE object = 'TABL'
        OR object = 'TTYP'.
      APPEND INITIAL LINE TO lt_nodes ASSIGNING <ls_node>.
      <ls_node>-obj_name = <ls_tadir>-obj_name.
      <ls_node>-obj_type = <ls_tadir>-object.
    ENDLOOP.

    APPEND 'TABL' TO lt_scope.
    APPEND 'STRU' TO lt_scope.
    APPEND 'TTYP' TO lt_scope.

* build edges
    LOOP AT lt_nodes ASSIGNING <ls_node>.

      CLEAR lt_findstrings.
      APPEND <ls_node>-obj_name TO lt_findstrings.
      lv_find_obj_cls = <ls_node>-obj_type.

      CALL FUNCTION 'RS_EU_CROSSREF'
        EXPORTING
          i_find_obj_cls           = lv_find_obj_cls
        TABLES
          i_findstrings            = lt_findstrings
          o_founds                 = lt_founds
          i_scope_object_cls       = lt_scope
        EXCEPTIONS
          not_executed             = 1
          not_found                = 2
          illegal_object           = 3
          no_cross_for_this_object = 4
          batch                    = 5
          batchjob_error           = 6
          wrong_type               = 7
          object_not_exist         = 8
          OTHERS                   = 9.
      IF sy-subrc <> 0.
        CONTINUE.
      ENDIF.

      LOOP AT lt_founds ASSIGNING <ls_found>.
        APPEND INITIAL LINE TO lt_edges ASSIGNING <ls_edge>.
        <ls_edge>-from = <ls_node>.

        <ls_edge>-to-obj_name   = <ls_found>-object.
        CASE <ls_found>-object_cls.
          WHEN 'DS'
              OR 'DT'.
            <ls_edge>-to-obj_type = 'TABL'.
          WHEN 'DA'.
            <ls_edge>-to-obj_type = 'TTYP'.
          WHEN OTHERS.
            _raise 'resolve_ddic, unknown object_cls'.
        ENDCASE.
      ENDLOOP.

    ENDLOOP.

    DO.
      lv_before = lines( lt_nodes ).
      LOOP AT lt_nodes ASSIGNING <ls_node>.
        lv_index = sy-tabix.
        READ TABLE lt_edges WITH KEY
          from-obj_name = <ls_node>-obj_name
          from-obj_type = <ls_node>-obj_type
          TRANSPORTING NO FIELDS.
        IF sy-subrc <> 0.
          LOOP AT ct_tadir ASSIGNING <ls_tadir>
              WHERE obj_name = <ls_node>-obj_name
              AND object = <ls_node>-obj_type.
            <ls_tadir>-korrnum = <ls_tadir>-korrnum + lv_plus.
            CONDENSE <ls_tadir>-korrnum.
          ENDLOOP.
          DELETE lt_edges
            WHERE to-obj_name = <ls_node>-obj_name
            AND to-obj_type = <ls_node>-obj_type.
          DELETE lt_nodes INDEX lv_index.
          EXIT. " make sure the sequence is fixed
        ENDIF.
      ENDLOOP.
      IF lv_before = lines( lt_nodes ).
        EXIT.
      ENDIF.
      lv_plus = lv_plus + 1.
    ENDDO.

  ENDMETHOD.                    "resolve_ddic

  METHOD delete_obj.

    DATA: li_obj TYPE REF TO lif_object.


    IF is_supported( is_item ) = abap_true.
      li_obj = create_object( is_item     = is_item
                              iv_language = gc_english ).
      li_obj->delete( ).
    ENDIF.

  ENDMETHOD.                    "delete

  METHOD serialize.

    DATA: li_obj   TYPE REF TO lif_object,
          lo_xml   TYPE REF TO lcl_xml_output,
          lo_files TYPE REF TO lcl_objects_files.


    IF is_supported( is_item ) = abap_false.
      IF NOT io_log IS INITIAL.
        io_log->add( iv_msgv1 = 'Object type ignored, not supported:'
                     iv_msgv2 = is_item-obj_type
                     iv_msgv3 = '-'
                     iv_msgv4 = is_item-obj_name ) ##no_text.
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

  METHOD check_duplicates.

    DATA: lt_files TYPE ty_files_tt.


    lt_files[] = it_files[].
    SORT lt_files BY path ASCENDING filename ASCENDING.
    DELETE ADJACENT DUPLICATES FROM lt_files COMPARING path filename.
    IF lines( lt_files ) <> lines( it_files ).
      _raise 'Duplicates'.
    ENDIF.

  ENDMETHOD.

  METHOD prioritize_deser.

    FIELD-SYMBOLS: <ls_result> LIKE LINE OF it_results.

* PROG before internet services, as the services might use the screens
    LOOP AT it_results ASSIGNING <ls_result> WHERE obj_type = 'PROG'.
      APPEND <ls_result> TO rt_results.
    ENDLOOP.

* ISAP has to be handled before ISRP
    LOOP AT it_results ASSIGNING <ls_result> WHERE obj_type = 'IASP'.
      APPEND <ls_result> TO rt_results.
    ENDLOOP.

    LOOP AT it_results ASSIGNING <ls_result>
        WHERE obj_type <> 'IASP' AND obj_type <> 'PROG'.
      APPEND <ls_result> TO rt_results.
    ENDLOOP.

  ENDMETHOD.                    "prioritize_deser

  METHOD deserialize.

    TYPES: BEGIN OF ty_late,
             obj     TYPE REF TO lif_object,
             xml     TYPE REF TO lcl_xml_input,
             package TYPE devclass,
           END OF ty_late.

    DATA: ls_item    TYPE ty_item,
          lv_cancel  TYPE abap_bool,
          li_obj     TYPE REF TO lif_object,
          lt_remote  TYPE ty_files_tt,
          lv_package TYPE devclass,
          lo_files   TYPE REF TO lcl_objects_files,
          lo_xml     TYPE REF TO lcl_xml_input,
          lt_results TYPE ty_results_tt,
          lt_late    TYPE TABLE OF ty_late.

    FIELD-SYMBOLS: <ls_result> LIKE LINE OF lt_results,
                   <ls_late>   LIKE LINE OF lt_late.


    lcl_objects_activation=>clear( ).

    lt_remote = io_repo->get_files_remote( ).

    lt_results = lcl_file_status=>status( io_repo ).
    DELETE lt_results WHERE match = abap_true.
    SORT lt_results BY obj_type ASCENDING obj_name ASCENDING.
    DELETE ADJACENT DUPLICATES FROM lt_results COMPARING obj_type obj_name.

    lt_results = prioritize_deser( lt_results ).

    warning_overwrite( EXPORTING io_repo = io_repo
                       CHANGING ct_results = lt_results ).

    LOOP AT lt_results ASSIGNING <ls_result>.
      lcl_progress=>show( iv_key     = 'Deserialize'
                          iv_current = sy-tabix
                          iv_total   = lines( lt_results )
                          iv_text    = <ls_result>-obj_name ) ##NO_TEXT.

      CLEAR ls_item.
      ls_item-obj_type = <ls_result>-obj_type.
      ls_item-obj_name = <ls_result>-obj_name.
* handle namespaces
      REPLACE ALL OCCURRENCES OF '#' IN ls_item-obj_name WITH '/'.

      lv_package = path_to_package(
        iv_top   = io_repo->get_package( )
        iv_start = io_repo->get_dot_abapgit( )->get_starting_folder( )
        iv_path  = <ls_result>-path ).

      lv_cancel = warning_package( is_item    = ls_item
                                   iv_package = lv_package ).
      IF lv_cancel = abap_true.
        _raise 'cancelled'.
      ENDIF.

      CREATE OBJECT lo_files
        EXPORTING
          is_item = ls_item.
      lo_files->set_files( lt_remote ).

* Analyze XML in order to instantiate the proper serializer
      lo_xml = lo_files->read_xml( ).

      li_obj = create_object( is_item     = ls_item
                              iv_language = io_repo->get_master_language( )
                              is_metadata = lo_xml->get_metadata( ) ).

      li_obj->mo_files = lo_files.

      IF li_obj->get_metadata( )-late_deser = abap_true.
        APPEND INITIAL LINE TO lt_late ASSIGNING <ls_late>.
        <ls_late>-obj = li_obj.
        <ls_late>-xml = lo_xml.
        <ls_late>-package = lv_package.
        CONTINUE.
      ENDIF.

      li_obj->deserialize( iv_package = lv_package
                           io_xml     = lo_xml ).

    ENDLOOP.

    lcl_objects_activation=>activate( ).

    LOOP AT lt_late ASSIGNING <ls_late>.
      <ls_late>-obj->deserialize( iv_package = <ls_late>-package
                                  io_xml     = <ls_late>-xml ).
    ENDLOOP.

    update_package_tree( io_repo->get_package( ) ).

  ENDMETHOD.                    "deserialize

ENDCLASS.                    "lcl_objects IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_object_clas DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_object_clas DEFINITION INHERITING FROM lcl_objects_program.

  PUBLIC SECTION.
    INTERFACES lif_object.
    ALIASES mo_files FOR lif_object~mo_files.

  PRIVATE SECTION.
    DATA mv_skip_testclass TYPE abap_bool.

    METHODS deserialize_abap
      IMPORTING io_xml     TYPE REF TO lcl_xml_input
                iv_package TYPE devclass
      RAISING   lcx_exception.

    METHODS deserialize_textpool
      IMPORTING io_xml TYPE REF TO lcl_xml_input
      RAISING   lcx_exception.

    METHODS deserialize_docu
      IMPORTING io_xml TYPE REF TO lcl_xml_input
      RAISING   lcx_exception.

    METHODS serialize_abap_old
      IMPORTING is_clskey        TYPE seoclskey
      RETURNING VALUE(rt_source) TYPE ty_string_tt
      RAISING   lcx_exception.

    METHODS deserialize_abap_source_old
      IMPORTING is_clskey TYPE seoclskey
                it_source TYPE ty_string_tt
      RAISING   lcx_exception.

    METHODS deserialize_abap_source_new
      IMPORTING is_clskey TYPE seoclskey
                it_source TYPE ty_string_tt
      RAISING   lcx_exception
                cx_sy_dyn_call_error.

    METHODS serialize_abap_new
      IMPORTING is_clskey        TYPE seoclskey
      RETURNING VALUE(rt_source) TYPE ty_string_tt
      RAISING   lcx_exception
                cx_sy_dyn_call_error.

    METHODS serialize_locals_imp
      IMPORTING is_clskey        TYPE seoclskey
      RETURNING VALUE(rt_source) TYPE ty_string_tt
      RAISING   lcx_exception.

    METHODS serialize_locals_def
      IMPORTING is_clskey        TYPE seoclskey
      RETURNING VALUE(rt_source) TYPE ty_string_tt
      RAISING   lcx_exception.

    METHODS read_include
      IMPORTING is_clskey        TYPE seoclskey
                iv_type          TYPE seop_include_ext_app
      RETURNING VALUE(rt_source) TYPE seop_source_string.

    METHODS serialize_testclasses
      IMPORTING is_clskey        TYPE seoclskey
      RETURNING VALUE(rt_source) TYPE ty_string_tt
      RAISING   lcx_exception.

    METHODS serialize_macros
      IMPORTING is_clskey        TYPE seoclskey
      RETURNING VALUE(rt_source) TYPE ty_string_tt
      RAISING   lcx_exception.

    METHODS serialize_xml
      IMPORTING io_xml TYPE REF TO lcl_xml_output
      RAISING   lcx_exception.

    METHODS remove_signatures
      CHANGING ct_source TYPE ty_string_tt.

    METHODS reduce
      CHANGING ct_source TYPE ty_string_tt.

ENDCLASS.                    "lcl_object_dtel DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_object_intf DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_object_intf DEFINITION INHERITING FROM lcl_object_clas FINAL.
* todo, CLAS + INTF to be refactored, see:
* https://github.com/larshp/abapGit/issues/21
ENDCLASS.                    "lcl_object_intf DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_object_clas IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_object_clas IMPLEMENTATION.

  METHOD lif_object~get_metadata.
    rs_metadata = get_metadata( ).
  ENDMETHOD.                    "lif_object~get_metadata

  METHOD lif_object~changed_by.
* todo, not sure this is correct, to be tested
    SELECT SINGLE changedby FROM seoclassdf INTO rv_user
      WHERE clsname = ms_item-obj_name
      AND version = '1'.                                "#EC CI_GENBUFF
    IF sy-subrc = 0 AND rv_user IS INITIAL.
      SELECT SINGLE author FROM seoclassdf INTO rv_user
        WHERE clsname = ms_item-obj_name
        AND version = '1'.                              "#EC CI_GENBUFF
    ENDIF.
    IF sy-subrc <> 0.
      rv_user = c_user_unknown.
    ENDIF.
  ENDMETHOD.

  METHOD lif_object~exists.

    DATA: ls_clskey TYPE seoclskey.


    ls_clskey-clsname = ms_item-obj_name.

    CALL FUNCTION 'SEO_CLASS_EXISTENCE_CHECK'
      EXPORTING
        clskey        = ls_clskey
      EXCEPTIONS
        not_specified = 1
        not_existing  = 2
        is_interface  = 3
        no_text       = 4
        inconsistent  = 5
        OTHERS        = 6.
    rv_bool = boolc( sy-subrc <> 2 ).

  ENDMETHOD.                    "lif_object~exists

  METHOD lif_object~jump.

    CALL FUNCTION 'RS_TOOL_ACCESS'
      EXPORTING
        operation     = 'SHOW'
        object_name   = ms_item-obj_name
        object_type   = 'CLAS'
        in_new_window = abap_true.

  ENDMETHOD.                    "jump

  METHOD lif_object~delete.

    DATA: ls_clskey TYPE seoclskey.


    ls_clskey-clsname = ms_item-obj_name.

    CASE ms_item-obj_type.
      WHEN 'CLAS'.
        CALL FUNCTION 'SEO_CLASS_DELETE_COMPLETE'
          EXPORTING
            clskey       = ls_clskey
          EXCEPTIONS
            not_existing = 1
            is_interface = 2
            db_error     = 3
            no_access    = 4
            other        = 5
            OTHERS       = 6.
        IF sy-subrc <> 0.
          _raise 'Error from SEO_CLASS_DELETE_COMPLETE'.
        ENDIF.
      WHEN 'INTF'.
        CALL FUNCTION 'SEO_INTERFACE_DELETE_COMPLETE'
          EXPORTING
            intkey       = ls_clskey
          EXCEPTIONS
            not_existing = 1
            is_class     = 2
            db_error     = 3
            no_access    = 4
            other        = 5
            OTHERS       = 6.
        IF sy-subrc <> 0.
          _raise 'Error from SEO_INTERFACE_DELETE_COMPLETE'.
        ENDIF.
      WHEN OTHERS.
        _raise 'class delete, unknown type'.
    ENDCASE.

  ENDMETHOD.                    "delete

  METHOD reduce.

    DATA: lv_source LIKE LINE OF ct_source,
          lv_found  TYPE sap_bool.


* skip files that only contain the standard comments
    lv_found = abap_false.
    LOOP AT ct_source INTO lv_source.
      IF strlen( lv_source ) >= 3 AND lv_source(3) <> '*"*'.
        lv_found = abap_true.
      ENDIF.
    ENDLOOP.
    IF lv_found = abap_false.
      CLEAR ct_source[].
    ENDIF.

  ENDMETHOD.                    "reduce

  METHOD serialize_locals_imp.

    rt_source = read_include( is_clskey = is_clskey
                              iv_type = seop_ext_class_locals_imp ).

    reduce( CHANGING ct_source = rt_source ).

  ENDMETHOD.                    "serialize_local

  METHOD serialize_locals_def.

    rt_source = read_include( is_clskey = is_clskey
                              iv_type = seop_ext_class_locals_def ).

    reduce( CHANGING ct_source = rt_source ).

  ENDMETHOD.                    "serialize_locals_def

  METHOD read_include.

    DATA: ls_include TYPE progstruc.


    ASSERT iv_type = seop_ext_class_locals_def
      OR iv_type = seop_ext_class_locals_imp
      OR iv_type = seop_ext_class_macros
      OR iv_type = seop_ext_class_testclasses.

    ls_include-rootname = is_clskey-clsname.
    TRANSLATE ls_include-rootname USING ' ='.
    ls_include-categorya = iv_type(1).
    ls_include-codea = iv_type+1(4).

* it looks like there is an issue in function module SEO_CLASS_GET_INCLUDE_SOURCE
* on 750 kernels, where the READ REPORT without STATE addition does not
* return the active version, this method is a workaround for this issue
    READ REPORT ls_include INTO rt_source STATE 'A'.

  ENDMETHOD.

  METHOD serialize_testclasses.

    DATA: lv_line1 LIKE LINE OF rt_source,
          lv_line2 LIKE LINE OF rt_source.


    rt_source = read_include( is_clskey = is_clskey
                              iv_type = seop_ext_class_testclasses ).

* when creating classes in Eclipse it automatically generates the
* testclass include, but it is not needed, so skip to avoid
* creating an extra file in the repository.
* Also remove it if the content is manually removed, but
* the class still thinks it contains tests
    mv_skip_testclass = abap_false.
    IF lines( rt_source ) = 2.
      READ TABLE rt_source INDEX 1 INTO lv_line1.
      ASSERT sy-subrc = 0.
      READ TABLE rt_source INDEX 2 INTO lv_line2.
      ASSERT sy-subrc = 0.
      IF lv_line1(3) = '*"*' AND lv_line2 IS INITIAL.
        mv_skip_testclass = abap_true.
      ENDIF.
    ELSEIF lines( rt_source ) = 1.
      READ TABLE rt_source INDEX 1 INTO lv_line1.
      ASSERT sy-subrc = 0.
      IF lv_line1(3) = '*"*' OR lv_line1 IS INITIAL.
        mv_skip_testclass = abap_true.
      ENDIF.
    ELSEIF lines( rt_source ) = 0.
      mv_skip_testclass = abap_true.
    ENDIF.

  ENDMETHOD.                    "serialize_test

  METHOD serialize_macros.

    rt_source = read_include( is_clskey = is_clskey
                              iv_type = seop_ext_class_macros ).

    reduce( CHANGING ct_source = rt_source ).

  ENDMETHOD.                    "serialize_macro

  METHOD serialize_abap_old.
* for old ABAP AS versions
    DATA: lo_source TYPE REF TO cl_oo_source.


    CREATE OBJECT lo_source
      EXPORTING
        clskey             = is_clskey
      EXCEPTIONS
        class_not_existing = 1
        OTHERS             = 2.
    IF sy-subrc <> 0.
      _raise 'error from CL_OO_SOURCE'.
    ENDIF.

    lo_source->read( 'A' ).
    rt_source = lo_source->get_old_source( ).
    remove_signatures( CHANGING ct_source = rt_source ).

  ENDMETHOD.                    "serialize_abap

  METHOD serialize_abap_new.

    DATA: lo_source   TYPE REF TO object,
          lo_instance TYPE REF TO object.

* do not call the class/methods statically, as it will
* give syntax errors on old versions
    CALL METHOD ('CL_OO_FACTORY')=>('CREATE_INSTANCE')
      RECEIVING
        result = lo_instance.

    CALL METHOD lo_instance->('CREATE_CLIF_SOURCE')
      EXPORTING
        clif_name = is_clskey-clsname
        version   = 'A'
      RECEIVING
        result    = lo_source.

    CALL METHOD lo_source->('GET_SOURCE')
      IMPORTING
        source = rt_source.

  ENDMETHOD.

  METHOD remove_signatures.

* signatures messes up in CL_OO_SOURCE when deserializing and serializing
* within same session

    DATA: lv_begin  TYPE string,
          lv_end    TYPE string,
          lv_remove TYPE sap_bool,
          lv_source LIKE LINE OF ct_source.


    CONCATENATE '* <SIGNATURE>------------------------------------'
      '---------------------------------------------------+'
      INTO lv_begin.

    CONCATENATE '* +------------------------------------------------'
      '--------------------------------------</SIGNATURE>'
      INTO lv_end.

    lv_remove = abap_false.
    LOOP AT ct_source INTO lv_source.
      IF lv_source = lv_begin.
        lv_remove = abap_true.
      ENDIF.
      IF lv_remove = abap_true.
        DELETE ct_source INDEX sy-tabix.
      ENDIF.
      IF lv_source = lv_end.
        lv_remove = abap_false.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.                    "remove_signatures

  METHOD lif_object~serialize.

    DATA: lt_source TYPE seop_source_string,
          ls_clskey TYPE seoclskey.


    ls_clskey-clsname = ms_item-obj_name.

    IF lif_object~exists( ) = abap_false.
      RETURN.
    ENDIF.

    CALL FUNCTION 'SEO_BUFFER_REFRESH'
      EXPORTING
        version = seoc_version_active
        force   = seox_true.
    CALL FUNCTION 'SEO_BUFFER_REFRESH'
      EXPORTING
        version = seoc_version_inactive
        force   = seox_true.

    TRY.
        lt_source = serialize_abap_new( ls_clskey ).
      CATCH cx_sy_dyn_call_error.
        lt_source = serialize_abap_old( ls_clskey ).
    ENDTRY.

    mo_files->add_abap( lt_source ).

    IF ms_item-obj_type = 'CLAS'.
      lt_source = serialize_locals_def( ls_clskey ).
      IF NOT lt_source[] IS INITIAL.
        mo_files->add_abap( iv_extra = 'locals_def'
                            it_abap  = lt_source ).         "#EC NOTEXT
      ENDIF.

      lt_source = serialize_locals_imp( ls_clskey ).
      IF NOT lt_source[] IS INITIAL.
        mo_files->add_abap( iv_extra = 'locals_imp'
                            it_abap  = lt_source ).         "#EC NOTEXT
      ENDIF.

      lt_source = serialize_testclasses( ls_clskey ).
      IF NOT lt_source[] IS INITIAL AND mv_skip_testclass = abap_false.
        mo_files->add_abap( iv_extra = 'testclasses'
                            it_abap  = lt_source ).         "#EC NOTEXT
      ENDIF.

      lt_source = serialize_macros( ls_clskey ).
      IF NOT lt_source[] IS INITIAL.
        mo_files->add_abap( iv_extra = 'macros'
                            it_abap  = lt_source ).         "#EC NOTEXT
      ENDIF.
    ENDIF.

    serialize_xml( io_xml ).

  ENDMETHOD.                    "serialize

  METHOD serialize_xml.

    DATA: ls_vseoclass  TYPE vseoclass,
          lv_cp         TYPE program,
          lt_tpool      TYPE textpool_table,
          lv_object     TYPE dokhl-object,
          lv_state      TYPE dokhl-dokstate,
          ls_vseointerf TYPE vseointerf,
          ls_clskey     TYPE seoclskey,
          lt_lines      TYPE tlinetab.


    ls_clskey-clsname = ms_item-obj_name.

    CALL FUNCTION 'SEO_CLIF_GET'
      EXPORTING
        cifkey       = ls_clskey
        version      = seoc_version_active
      IMPORTING
        class        = ls_vseoclass
        interface    = ls_vseointerf
      EXCEPTIONS
        not_existing = 1
        deleted      = 2
        model_only   = 3
        OTHERS       = 4.
    IF sy-subrc = 1.
      RETURN. " in case only inactive version exists
    ELSEIF sy-subrc <> 0.
      _raise 'error from seo_clif_get'.
    ENDIF.

    CLEAR: ls_vseoclass-uuid,
           ls_vseoclass-author,
           ls_vseoclass-createdon,
           ls_vseoclass-changedby,
           ls_vseoclass-changedon,
           ls_vseoclass-r3release,
           ls_vseoclass-chgdanyby,
           ls_vseoclass-chgdanyon.

    IF mv_skip_testclass = abap_true.
      CLEAR ls_vseoclass-with_unit_tests.
    ENDIF.

    CLEAR: ls_vseointerf-uuid,
           ls_vseointerf-author,
           ls_vseointerf-createdon,
           ls_vseointerf-changedby,
           ls_vseointerf-changedon,
           ls_vseointerf-r3release.

    CASE ms_item-obj_type.
      WHEN 'CLAS'.
        io_xml->add( iv_name = 'VSEOCLASS'
                     ig_data = ls_vseoclass ).

        lv_cp = cl_oo_classname_service=>get_classpool_name( ls_clskey-clsname ).
        READ TEXTPOOL lv_cp INTO lt_tpool LANGUAGE mv_language. "#EC CI_READ_REP
        io_xml->add( iv_name = 'TPOOL'
                     ig_data = add_tpool( lt_tpool ) ).
      WHEN 'INTF'.
        io_xml->add( iv_name = 'VSEOINTERF'
                     ig_data = ls_vseointerf ).
      WHEN OTHERS.
        ASSERT 0 = 1.
    ENDCASE.

    lv_object = ls_clskey-clsname.
    CALL FUNCTION 'DOCU_GET'
      EXPORTING
        id                = 'CL'
        langu             = mv_language
        object            = lv_object
      IMPORTING
        dokstate          = lv_state
      TABLES
        line              = lt_lines
      EXCEPTIONS
        no_docu_on_screen = 1
        no_docu_self_def  = 2
        no_docu_temp      = 3
        ret_code          = 4
        OTHERS            = 5.
    IF sy-subrc = 0 AND lv_state = 'R'.
      io_xml->add( iv_name = 'LINES'
                   ig_data = lt_lines ).
    ENDIF.

  ENDMETHOD.                    "serialize_xml

  METHOD lif_object~deserialize.

* function group SEOK
* function group SEOQ
* function group SEOP
* class CL_OO_CLASSNAME_SERVICE
* class CL_OO_SOURCE

    deserialize_abap( io_xml     = io_xml
                      iv_package = iv_package ).

    IF ms_item-obj_type = 'CLAS'.
      deserialize_textpool( io_xml ).
    ENDIF.

    deserialize_docu( io_xml ).

  ENDMETHOD.                    "deserialize

  METHOD deserialize_docu.

    DATA: lt_lines  TYPE tlinetab,
          lv_object TYPE dokhl-object.


    io_xml->read( EXPORTING iv_name = 'LINES'
                  CHANGING cg_data = lt_lines ).

    IF lt_lines[] IS INITIAL.
      RETURN.
    ENDIF.

    lv_object = ms_item-obj_name.
    CALL FUNCTION 'DOCU_UPD'
      EXPORTING
        id       = 'CL'
        langu    = mv_language
        object   = lv_object
      TABLES
        line     = lt_lines
      EXCEPTIONS
        ret_code = 1
        OTHERS   = 2.
    IF sy-subrc <> 0.
      _raise 'error from DOCU_UPD'.
    ENDIF.

  ENDMETHOD.                    "deserialize_doku

  METHOD deserialize_textpool.

    DATA: lv_cp        TYPE program,
          lv_clsname   TYPE seoclsname,
          lt_tpool_ext TYPE ty_tpool_tt,
          lt_tpool     TYPE textpool_table.


    io_xml->read( EXPORTING iv_name = 'TPOOL'
                  CHANGING cg_data = lt_tpool_ext ).
    lt_tpool = read_tpool( lt_tpool_ext ).

    IF lt_tpool[] IS INITIAL.
      RETURN.
    ENDIF.

    lv_clsname = ms_item-obj_name.
    lv_cp = cl_oo_classname_service=>get_classpool_name( lv_clsname ).

    INSERT TEXTPOOL lv_cp
      FROM lt_tpool
      LANGUAGE mv_language
      STATE 'I'.
    IF sy-subrc <> 0.
      _raise 'error from INSERT TEXTPOOL'.
    ENDIF.

    lcl_objects_activation=>add( iv_type = 'REPT'
                                 iv_name = lv_cp ).

  ENDMETHOD.                    "deserialize_textpool

  METHOD deserialize_abap.

    DATA: ls_vseoclass   TYPE vseoclass,
          ls_vseointerf  TYPE vseointerf,
          lt_source      TYPE seop_source_string,
          lt_locals_def  TYPE seop_source_string,
          lt_locals_imp  TYPE seop_source_string,
          lt_locals_mac  TYPE seop_source_string,
          lt_testclasses TYPE seop_source_string,
          ls_clskey      TYPE seoclskey.


    lt_source = mo_files->read_abap( ).

    lt_locals_def = mo_files->read_abap( iv_extra = 'locals_def'
                                         iv_error = abap_false ). "#EC NOTEXT

    lt_locals_imp = mo_files->read_abap( iv_extra = 'locals_imp'
                                         iv_error = abap_false ). "#EC NOTEXT

    lt_locals_mac = mo_files->read_abap( iv_extra = 'macros'
                                         iv_error = abap_false ). "#EC NOTEXT

    lt_testclasses = mo_files->read_abap( iv_extra = 'testclasses'
                                          iv_error = abap_false ). "#EC NOTEXT

    ls_clskey-clsname = ms_item-obj_name.


    CASE ms_item-obj_type.
      WHEN 'CLAS'.
        io_xml->read( EXPORTING iv_name = 'VSEOCLASS'
                      CHANGING cg_data = ls_vseoclass ).

        CALL FUNCTION 'SEO_CLASS_CREATE_COMPLETE'
          EXPORTING
            devclass        = iv_package
            overwrite       = seox_true
          CHANGING
            class           = ls_vseoclass
          EXCEPTIONS
            existing        = 1
            is_interface    = 2
            db_error        = 3
            component_error = 4
            no_access       = 5
            other           = 6
            OTHERS          = 7.
        IF sy-subrc <> 0.
          _raise 'error from SEO_CLASS_CREATE_COMPLETE'.
        ENDIF.

      WHEN 'INTF'.
        io_xml->read( EXPORTING iv_name = 'VSEOINTERF'
                      CHANGING cg_data = ls_vseointerf ).

        CALL FUNCTION 'SEO_INTERFACE_CREATE_COMPLETE'
          EXPORTING
            devclass        = iv_package
            overwrite       = seox_true
          CHANGING
            interface       = ls_vseointerf
          EXCEPTIONS
            existing        = 1
            is_class        = 2
            db_error        = 3
            component_error = 4
            no_access       = 5
            other           = 6
            OTHERS          = 7.
        IF sy-subrc <> 0.
          _raise 'Error from SEO_INTERFACE_CREATE_COMPLETE'.
        ENDIF.

      WHEN OTHERS.
        ASSERT 0 = 1.
    ENDCASE.

    IF ms_item-obj_type = 'CLAS'.
      CALL FUNCTION 'SEO_CLASS_GENERATE_LOCALS'
        EXPORTING
          clskey                 = ls_clskey
          force                  = seox_true
          locals_def             = lt_locals_def
          locals_imp             = lt_locals_imp
          locals_mac             = lt_locals_mac
          locals_testclasses     = lt_testclasses
        EXCEPTIONS
          not_existing           = 1
          model_only             = 2
          locals_not_generated   = 3
          locals_not_initialised = 4
          OTHERS                 = 5.
      IF sy-subrc <> 0.
        _raise 'error from generate_locals'.
      ENDIF.
    ENDIF.

    TRY.
        deserialize_abap_source_new(
          is_clskey = ls_clskey
          it_source = lt_source ).
      CATCH cx_sy_dyn_call_error.
        deserialize_abap_source_old(
          is_clskey = ls_clskey
          it_source = lt_source ).
    ENDTRY.

    lcl_objects_activation=>add_item( ms_item ).

  ENDMETHOD.                    "deserialize

  METHOD deserialize_abap_source_old.
* for backwards compatability down to 702

    DATA: lo_source TYPE REF TO cl_oo_source.


    CREATE OBJECT lo_source
      EXPORTING
        clskey             = is_clskey
      EXCEPTIONS
        class_not_existing = 1
        OTHERS             = 2.
    IF sy-subrc <> 0.
      _raise 'error from CL_OO_SOURCE'.
    ENDIF.

    TRY.
        lo_source->access_permission( seok_access_modify ).
        lo_source->set_source( it_source ).
        lo_source->save( ).
        lo_source->access_permission( seok_access_free ).
      CATCH cx_oo_access_permission.
        _raise 'permission error'.
      CATCH cx_oo_source_save_failure.
        _raise 'save failure'.
    ENDTRY.

  ENDMETHOD.

  METHOD deserialize_abap_source_new.

    DATA: lo_factory TYPE REF TO object,
          lo_source  TYPE REF TO object.


    CALL METHOD ('CL_OO_FACTORY')=>('CREATE_INSTANCE')
      RECEIVING
        result = lo_factory.

    CALL METHOD lo_factory->('CREATE_CLIF_SOURCE')
      EXPORTING
        clif_name = is_clskey-clsname
      RECEIVING
        result    = lo_source.

    TRY.
        CALL METHOD lo_source->('IF_OO_CLIF_SOURCE~LOCK').
      CATCH cx_oo_access_permission.
        _raise 'source_new, access permission exception'.
    ENDTRY.

    CALL METHOD lo_source->('IF_OO_CLIF_SOURCE~SET_SOURCE')
      EXPORTING
        source = it_source.

    CALL METHOD lo_source->('IF_OO_CLIF_SOURCE~SAVE').

    CALL METHOD lo_source->('IF_OO_CLIF_SOURCE~UNLOCK').

  ENDMETHOD.

ENDCLASS.                    "lcl_object_CLAS IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_object_fugr DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_object_fugr DEFINITION INHERITING FROM lcl_objects_program FINAL.

  PUBLIC SECTION.
    INTERFACES lif_object.
    ALIASES mo_files FOR lif_object~mo_files.

  PRIVATE SECTION.
    TYPES: ty_rs38l_incl_tt TYPE STANDARD TABLE OF rs38l_incl WITH DEFAULT KEY.

    TYPES: BEGIN OF ty_function,
             funcname      TYPE rs38l_fnam,
             include       TYPE progname,
             global_flag   TYPE rs38l-global,
             remote_call   TYPE rs38l-remote,
             update_task   TYPE rs38l-utask,
             short_text    TYPE tftit-stext,
             remote_basxml TYPE rs38l-basxml_enabled,
             import        TYPE STANDARD TABLE OF rsimp WITH DEFAULT KEY,
             changing      TYPE STANDARD TABLE OF rscha WITH DEFAULT KEY,
             export        TYPE STANDARD TABLE OF rsexp WITH DEFAULT KEY,
             tables        TYPE STANDARD TABLE OF rstbl WITH DEFAULT KEY,
             exception     TYPE STANDARD TABLE OF rsexc WITH DEFAULT KEY,
             documentation TYPE STANDARD TABLE OF rsfdo WITH DEFAULT KEY,
           END OF ty_function.

    TYPES: ty_function_tt TYPE STANDARD TABLE OF ty_function WITH DEFAULT KEY.

    METHODS main_name
      RETURNING VALUE(rv_program) TYPE program
      RAISING   lcx_exception.

    METHODS functions
      RETURNING VALUE(rt_functab) TYPE ty_rs38l_incl_tt
      RAISING   lcx_exception.

    METHODS includes
      RETURNING VALUE(rt_includes) TYPE rso_t_objnm
      RAISING   lcx_exception.

    METHODS serialize_functions
      RETURNING VALUE(rt_functions) TYPE ty_function_tt
      RAISING   lcx_exception.

    METHODS deserialize_functions
      IMPORTING it_functions TYPE ty_function_tt
      RAISING   lcx_exception.

    METHODS serialize_xml
      IMPORTING io_xml TYPE REF TO lcl_xml_output
      RAISING   lcx_exception.

    METHODS deserialize_xml
      IMPORTING io_xml     TYPE REF TO lcl_xml_input
                iv_package TYPE devclass
      RAISING   lcx_exception.

    METHODS serialize_includes
      RAISING lcx_exception.

    METHODS deserialize_includes
      IMPORTING io_xml     TYPE REF TO lcl_xml_input
                iv_package TYPE devclass
      RAISING   lcx_exception.

*    METHODS deserialize_dynpros
*      IMPORTING it_dynpros TYPE ty_dynpro_tt
*      RAISING   lcx_exception.
*
*    METHODS deserialize_cua
*      IMPORTING is_cua TYPE ty_cua
*      RAISING   lcx_exception.

ENDCLASS.                    "lcl_object_fugr DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_object_dtel IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_object_fugr IMPLEMENTATION.

* function group SEUF
* function group SIFP
* function group SUNI

  METHOD lif_object~changed_by.
    rv_user = c_user_unknown. " todo
  ENDMETHOD.

  METHOD lif_object~get_metadata.
    rs_metadata = get_metadata( ).
  ENDMETHOD.                    "lif_object~get_metadata

  METHOD lif_object~exists.

    DATA: lv_pool  TYPE tlibg-area.


    lv_pool = ms_item-obj_name.
    CALL FUNCTION 'RS_FUNCTION_POOL_EXISTS'
      EXPORTING
        function_pool   = lv_pool
      EXCEPTIONS
        pool_not_exists = 1.
    rv_bool = boolc( sy-subrc <> 1 ).

  ENDMETHOD.                    "lif_object~exists

  METHOD deserialize_functions.

    DATA: lv_include TYPE rs38l-include,
          lv_area    TYPE rs38l-area,
          lt_source  TYPE TABLE OF abaptxt255.

    FIELD-SYMBOLS: <ls_func> LIKE LINE OF it_functions.


    LOOP AT it_functions ASSIGNING <ls_func>.

      lt_source = mo_files->read_abap( iv_extra = <ls_func>-funcname ).

      lv_area = ms_item-obj_name.

      CALL FUNCTION 'FUNCTION_EXISTS'
        EXPORTING
          funcname           = <ls_func>-funcname
        IMPORTING
          include            = lv_include
        EXCEPTIONS
          function_not_exist = 1.
      IF sy-subrc = 0.
* delete the function module to make sure the parameters are updated
* havent found a nice way to update the paramters
        CALL FUNCTION 'FUNCTION_DELETE'
          EXPORTING
            funcname                 = <ls_func>-funcname
            suppress_success_message = abap_true
          EXCEPTIONS
            error_message            = 1
            OTHERS                   = 2.
        IF sy-subrc <> 0.
          _raise 'error from FUNCTION_DELETE'.
        ENDIF.
      ENDIF.

      CALL FUNCTION 'RS_FUNCTIONMODULE_INSERT'
        EXPORTING
          funcname                = <ls_func>-funcname
          function_pool           = lv_area
          interface_global        = <ls_func>-global_flag
          remote_call             = <ls_func>-remote_call
          short_text              = <ls_func>-short_text
*         NAMESPACE               = ' ' todo
          remote_basxml_supported = <ls_func>-remote_basxml
        IMPORTING
          function_include        = lv_include
        TABLES
          import_parameter        = <ls_func>-import
          export_parameter        = <ls_func>-export
          tables_parameter        = <ls_func>-tables
          changing_parameter      = <ls_func>-changing
          exception_list          = <ls_func>-exception
          parameter_docu          = <ls_func>-documentation
        EXCEPTIONS
          double_task             = 1
          error_message           = 2
          function_already_exists = 3
          invalid_function_pool   = 4
          invalid_name            = 5
          too_many_functions      = 6
          no_modify_permission    = 7
          no_show_permission      = 8
          enqueue_system_failure  = 9
          canceled_in_corr        = 10
          OTHERS                  = 11.
      IF sy-subrc <> 0.
        _raise 'error from RS_FUNCTIONMODULE_INSERT'.
      ENDIF.

      INSERT REPORT lv_include FROM lt_source.

*      lcl_objects_activation=>add( iv_type = 'FUNC'
*                                   iv_name = <ls_func>-funcname ).

    ENDLOOP.

  ENDMETHOD.                    "deserialize_functions

  METHOD deserialize_includes.

    DATA: lo_xml       TYPE REF TO lcl_xml_input,
          ls_progdir   TYPE ty_progdir,
          lt_includes  TYPE rso_t_objnm,
          lt_tpool     TYPE textpool_table,
          lt_tpool_ext TYPE ty_tpool_tt,
          lt_source    TYPE TABLE OF abaptxt255.

    FIELD-SYMBOLS: <lv_include> LIKE LINE OF lt_includes.


    io_xml->read( EXPORTING iv_name = 'INCLUDES'
                  CHANGING cg_data = lt_includes ).

    LOOP AT lt_includes ASSIGNING <lv_include>.

      lt_source = mo_files->read_abap( iv_extra = <lv_include> ).

      lo_xml = mo_files->read_xml( <lv_include> ).

      lo_xml->read( EXPORTING iv_name = 'PROGDIR'
                    CHANGING cg_data = ls_progdir ).

      lo_xml->read( EXPORTING iv_name = 'TPOOL'
                    CHANGING cg_data = lt_tpool_ext ).
      lt_tpool = read_tpool( lt_tpool_ext ).

      deserialize_program( is_progdir = ls_progdir
                           it_source  = lt_source
                           it_tpool   = lt_tpool
                           iv_package = iv_package ).

    ENDLOOP.

  ENDMETHOD.                    "deserialize_includes

  METHOD deserialize_xml.

    DATA: lv_complete  TYPE rs38l-area,
          lv_namespace TYPE rs38l-namespace,
          lv_areat     TYPE tlibt-areat,
          lv_stext     TYPE tftit-stext,
          lv_group     TYPE rs38l-area.


    lv_complete = ms_item-obj_name.

    CALL FUNCTION 'FUNCTION_INCLUDE_SPLIT'
      EXPORTING
        complete_area                = lv_complete
      IMPORTING
        namespace                    = lv_namespace
        group                        = lv_group
      EXCEPTIONS
        include_not_exists           = 1
        group_not_exists             = 2
        no_selections                = 3
        no_function_include          = 4
        no_function_pool             = 5
        delimiter_wrong_position     = 6
        no_customer_function_group   = 7
        no_customer_function_include = 8
        reserved_name_customer       = 9
        namespace_too_long           = 10
        area_length_error            = 11
        OTHERS                       = 12.
    IF sy-subrc <> 0.
      _raise 'error from FUNCTION_INCLUDE_SPLIT'.
    ENDIF.

    io_xml->read( EXPORTING iv_name = 'AREAT'
                  CHANGING cg_data = lv_areat ).
    lv_stext = lv_areat.

    CALL FUNCTION 'RS_FUNCTION_POOL_INSERT'
      EXPORTING
        function_pool           = lv_group
        short_text              = lv_stext
        namespace               = lv_namespace
        devclass                = iv_package
      EXCEPTIONS
        name_already_exists     = 1
        name_not_correct        = 2
        function_already_exists = 3
        invalid_function_pool   = 4
        invalid_name            = 5
        too_many_functions      = 6
        no_modify_permission    = 7
        no_show_permission      = 8
        enqueue_system_failure  = 9
        canceled_in_corr        = 10
        undefined_error         = 11
        OTHERS                  = 12.
    IF sy-subrc <> 0 AND sy-subrc <> 1 AND sy-subrc <> 3.
* todo, change description
      _raise 'error from RS_FUNCTION_POOL_INSERT'.
    ENDIF.

  ENDMETHOD.                    "deserialize_xml

  METHOD serialize_xml.

    DATA: lt_functab  TYPE ty_rs38l_incl_tt,
          lt_includes TYPE rso_t_objnm,
          lv_areat    TYPE tlibt-areat.


    SELECT SINGLE areat INTO lv_areat
      FROM tlibt
      WHERE spras = mv_language
      AND area = ms_item-obj_name.                      "#EC CI_GENBUFF
    IF sy-subrc <> 0.
      _raise 'not found in TLIBT'.
    ENDIF.

    lt_functab = functions( ).
    lt_includes = includes( ).

* todo, dynpros

    io_xml->add( iv_name = 'AREAT'
                 ig_data = lv_areat ).
    io_xml->add( iv_name = 'INCLUDES'
                 ig_data = lt_includes ).

  ENDMETHOD.                    "serialize_xml

  METHOD includes.

    DATA: lv_program TYPE program,
          lv_cnam    TYPE reposrc-cnam,
          lv_tabix   LIKE sy-tabix,
          lt_functab TYPE ty_rs38l_incl_tt.

    FIELD-SYMBOLS: <lv_include> LIKE LINE OF rt_includes,
                   <ls_func>    LIKE LINE OF lt_functab.


    lv_program = main_name( ).
    lt_functab = functions( ).

    CALL FUNCTION 'RS_GET_ALL_INCLUDES'
      EXPORTING
        program      = lv_program
*       WITH_RESERVED_INCLUDES =
*       WITH_CLASS_INCLUDES    = ' ' hmm, todo
      TABLES
        includetab   = rt_includes
      EXCEPTIONS
        not_existent = 1
        no_program   = 2
        OTHERS       = 3.
    IF sy-subrc <> 0.
      _raise 'Error from RS_GET_ALL_INCLUDES'.
    ENDIF.

    LOOP AT lt_functab ASSIGNING <ls_func>.
      DELETE TABLE rt_includes FROM <ls_func>-include.
    ENDLOOP.


    LOOP AT rt_includes ASSIGNING <lv_include>.
      lv_tabix = sy-tabix.

* skip SAP standard includes
      SELECT SINGLE cnam FROM reposrc INTO lv_cnam
        WHERE progname = <lv_include>
        AND r3state = 'A'
        AND cnam = 'SAP'.
      IF sy-subrc = 0.
        DELETE rt_includes INDEX lv_tabix.
        CONTINUE.
      ENDIF.

* also make sure the include exists
      SELECT SINGLE cnam FROM reposrc INTO lv_cnam
        WHERE progname = <lv_include>
        AND r3state = 'A'.
      IF sy-subrc <> 0.
        DELETE rt_includes INDEX lv_tabix.
      ENDIF.

    ENDLOOP.

    APPEND lv_program TO rt_includes.

  ENDMETHOD.                    "includes

  METHOD functions.

    DATA: lv_area TYPE rs38l-area.


    lv_area = ms_item-obj_name.

    CALL FUNCTION 'RS_FUNCTION_POOL_CONTENTS'
      EXPORTING
        function_pool           = lv_area
      TABLES
        functab                 = rt_functab
      EXCEPTIONS
        function_pool_not_found = 1
        OTHERS                  = 2.
    IF sy-subrc <> 0.
      _raise 'Error from RS_FUNCTION_POOL_CONTENTS'.
    ENDIF.

  ENDMETHOD.                    "functions

  METHOD main_name.

    DATA: lv_area      TYPE rs38l-area,
          lv_namespace TYPE rs38l-namespace,
          lv_group     TYPE rs38l-area.


    lv_area = ms_item-obj_name.

    CALL FUNCTION 'FUNCTION_INCLUDE_SPLIT'
      EXPORTING
        complete_area                = lv_area
      IMPORTING
        namespace                    = lv_namespace
        group                        = lv_group
      EXCEPTIONS
        include_not_exists           = 1
        group_not_exists             = 2
        no_selections                = 3
        no_function_include          = 4
        no_function_pool             = 5
        delimiter_wrong_position     = 6
        no_customer_function_group   = 7
        no_customer_function_include = 8
        reserved_name_customer       = 9
        namespace_too_long           = 10
        area_length_error            = 11
        OTHERS                       = 12.
    IF sy-subrc <> 0.
      _raise 'Error from FUNCTION_INCLUDE_SPLIT'.
    ENDIF.

    CONCATENATE lv_namespace 'SAPL' lv_group INTO rv_program.

  ENDMETHOD.                    "main_name

  METHOD serialize_functions.

    DATA:
      lt_source     TYPE TABLE OF rssource,
      lt_functab    TYPE ty_rs38l_incl_tt,
      lt_new_source TYPE rsfb_source.

    FIELD-SYMBOLS: <ls_func> LIKE LINE OF lt_functab,
                   <ls_ret>  LIKE LINE OF rt_functions.


    lt_functab = functions( ).

    LOOP AT lt_functab ASSIGNING <ls_func>.
* fm RPY_FUNCTIONMODULE_READ does not support source code
* lines longer than 72 characters
      APPEND INITIAL LINE TO rt_functions ASSIGNING <ls_ret>.
      MOVE-CORRESPONDING <ls_func> TO <ls_ret>.

      CALL FUNCTION 'RPY_FUNCTIONMODULE_READ_NEW'
        EXPORTING
          functionname            = <ls_func>-funcname
        IMPORTING
          global_flag             = <ls_ret>-global_flag
          remote_call             = <ls_ret>-remote_call
          update_task             = <ls_ret>-update_task
          short_text              = <ls_ret>-short_text
          remote_basxml_supported = <ls_ret>-remote_basxml
        TABLES
          import_parameter        = <ls_ret>-import
          changing_parameter      = <ls_ret>-changing
          export_parameter        = <ls_ret>-export
          tables_parameter        = <ls_ret>-tables
          exception_list          = <ls_ret>-exception
          documentation           = <ls_ret>-documentation
          source                  = lt_source
        CHANGING
          new_source              = lt_new_source
        EXCEPTIONS
          error_message           = 1
          function_not_found      = 2
          invalid_name            = 3
          OTHERS                  = 4.
      IF sy-subrc <> 0.
        _raise 'Error from RPY_FUNCTIONMODULE_READ_NEW'.
      ENDIF.

      IF NOT lt_new_source IS INITIAL.
        mo_files->add_abap( iv_extra = <ls_func>-funcname
                            it_abap  = lt_new_source ).
      ELSE.
        mo_files->add_abap( iv_extra = <ls_func>-funcname
                            it_abap  = lt_source ).
      ENDIF.

    ENDLOOP.

  ENDMETHOD.                    "serialize_functions

  METHOD serialize_includes.

    DATA: lt_includes TYPE rso_t_objnm.

    FIELD-SYMBOLS: <lv_include> LIKE LINE OF lt_includes.


    lt_includes = includes( ).

    LOOP AT lt_includes ASSIGNING <lv_include>.

* todo, filename is not correct, a include can be used in several programs
      serialize_program( is_item    = ms_item
                         io_files   = mo_files
                         iv_program = <lv_include>
                         iv_extra   = <lv_include> ).

    ENDLOOP.

  ENDMETHOD.                    "serialize_includes

  METHOD lif_object~serialize.

    DATA: lt_functions    TYPE ty_function_tt,
          ls_progdir      TYPE ty_progdir,
          lv_program_name TYPE programm,
          lt_dynpros      TYPE ty_dynpro_tt,
          ls_cua          TYPE ty_cua.

    IF lif_object~exists( ) = abap_false.
      RETURN.
    ENDIF.

    serialize_xml( io_xml ).

    lt_functions = serialize_functions( ).
    io_xml->add( iv_name = 'FUNCTIONS'
                 ig_data = lt_functions ).

    serialize_includes( ).

    lv_program_name = main_name( ).
    ls_progdir = read_progdir( lv_program_name ).

    IF ls_progdir-subc = 'F'.
      lt_dynpros = serialize_dynpros( lv_program_name ).
      io_xml->add( iv_name = 'DYNPROS'
                   ig_data = lt_dynpros ).

      ls_cua = serialize_cua( lv_program_name ).
      io_xml->add( iv_name = 'CUA'
                   ig_data = ls_cua ).
    ENDIF.

  ENDMETHOD.                    "serialize

  METHOD lif_object~deserialize.

    DATA: lv_program_name TYPE programm,
          lt_functions    TYPE ty_function_tt,
          lt_dynpros      TYPE ty_dynpro_tt,
          ls_cua          TYPE ty_cua.


    deserialize_xml(
      io_xml     = io_xml
      iv_package = iv_package ).

    io_xml->read( EXPORTING iv_name = 'FUNCTIONS'
                  CHANGING cg_data = lt_functions ).
    deserialize_functions( lt_functions ).

    deserialize_includes(
      io_xml     = io_xml
      iv_package = iv_package ).

    lv_program_name = main_name( ).

    io_xml->read( EXPORTING iv_name = 'DYNPROS'
                  CHANGING cg_data = lt_dynpros ).
    deserialize_dynpros( it_dynpros = lt_dynpros ).

    io_xml->read( EXPORTING iv_name = 'CUA'
                  CHANGING cg_data = ls_cua ).
    deserialize_cua( iv_program_name = lv_program_name
                     is_cua = ls_cua ).

  ENDMETHOD.                    "deserialize

  METHOD lif_object~delete.

    DATA: lv_area TYPE rs38l-area.


    lv_area = ms_item-obj_name.

    CALL FUNCTION 'RS_FUNCTION_POOL_DELETE'
      EXPORTING
        area                   = lv_area
        suppress_popups        = abap_true
        skip_progress_ind      = abap_true
      EXCEPTIONS
        canceled_in_corr       = 1
        enqueue_system_failure = 2
        function_exist         = 3
        not_executed           = 4
        no_modify_permission   = 5
        no_show_permission     = 6
        permission_failure     = 7
        pool_not_exist         = 8
        cancelled              = 9
        OTHERS                 = 10.
    IF sy-subrc <> 0.
      _raise 'error from RS_FUNCTION_POOL_DELETE'.
    ENDIF.

  ENDMETHOD.                    "delete

  METHOD lif_object~jump.

    CALL FUNCTION 'RS_TOOL_ACCESS'
      EXPORTING
        operation     = 'SHOW'
        object_name   = ms_item-obj_name
        object_type   = 'FUGR'
        in_new_window = abap_true.

  ENDMETHOD.                    "jump

ENDCLASS.                    "lcl_object_fugr IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_object_prog DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_object_prog DEFINITION INHERITING FROM lcl_objects_program FINAL.

  PUBLIC SECTION.
    INTERFACES lif_object.
    ALIASES mo_files FOR lif_object~mo_files.

  PRIVATE SECTION.

    METHODS deserialize_textpool
      IMPORTING it_tpool TYPE textpool_table
      RAISING   lcx_exception.

ENDCLASS.                    "lcl_object_prog DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_object_prog IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_object_prog IMPLEMENTATION.

  METHOD lif_object~changed_by.
    SELECT SINGLE unam FROM reposrc INTO rv_user
      WHERE progname = ms_item-obj_name
      AND r3state = 'A'.
    IF sy-subrc <> 0.
      rv_user = c_user_unknown.
    ENDIF.
  ENDMETHOD.

  METHOD lif_object~get_metadata.
    rs_metadata = get_metadata( ).
  ENDMETHOD.                    "lif_object~get_metadata

  METHOD lif_object~exists.

    DATA: lv_progname TYPE reposrc-progname.


    SELECT SINGLE progname FROM reposrc INTO lv_progname
      WHERE progname = ms_item-obj_name
      AND r3state = 'A'.
    rv_bool = boolc( sy-subrc = 0 ).

  ENDMETHOD.                    "lif_object~exists

  METHOD lif_object~jump.

    CALL FUNCTION 'RS_TOOL_ACCESS'
      EXPORTING
        operation     = 'SHOW'
        object_name   = ms_item-obj_name
        object_type   = 'PROG'
        in_new_window = abap_true.

  ENDMETHOD.                    "jump

  METHOD lif_object~delete.

    DATA: lv_program LIKE sy-repid.


    lv_program = ms_item-obj_name.

    CALL FUNCTION 'RS_DELETE_PROGRAM'
      EXPORTING
        program            = lv_program
        suppress_popup     = abap_true
      EXCEPTIONS
        enqueue_lock       = 1
        object_not_found   = 2
        permission_failure = 3
        reject_deletion    = 4
        OTHERS             = 5.
    IF sy-subrc <> 0.
      _raise 'error from RS_DELETE_PROGRAM'.
    ENDIF.

  ENDMETHOD.                    "delete

  METHOD deserialize_textpool.

    READ TABLE it_tpool WITH KEY id = 'R' TRANSPORTING NO FIELDS.
    IF ( sy-subrc = 0 AND lines( it_tpool ) = 1 ) OR lines( it_tpool ) = 0.
      RETURN. " no action for includes
    ENDIF.

    INSERT TEXTPOOL ms_item-obj_name
      FROM it_tpool
      LANGUAGE mv_language
      STATE 'I'.
    IF sy-subrc <> 0.
      _raise 'error from INSERT TEXTPOOL'.
    ENDIF.

    lcl_objects_activation=>add( iv_type = 'REPT'
                                 iv_name = ms_item-obj_name ).

  ENDMETHOD.                    "deserialize_textpool

  METHOD lif_object~serialize.

    serialize_program( io_xml   = io_xml
                       is_item  = ms_item
                       io_files = mo_files ).

  ENDMETHOD.                    "lif_serialize~serialize

  METHOD lif_object~deserialize.

    DATA: lv_program_name TYPE programm,
          ls_progdir      TYPE ty_progdir,
          lt_tpool        TYPE textpool_table,
          lt_dynpros      TYPE ty_dynpro_tt,
          lt_tpool_ext    TYPE ty_tpool_tt,
          ls_cua          TYPE ty_cua,
          lt_source       TYPE abaptxt255_tab.

    lv_program_name = ms_item-obj_name.

    lt_source = mo_files->read_abap( ).

    io_xml->read( EXPORTING iv_name = 'TPOOL'
                  CHANGING cg_data = lt_tpool_ext ).
    lt_tpool = read_tpool( lt_tpool_ext ).

    io_xml->read( EXPORTING iv_name = 'PROGDIR'
                  CHANGING cg_data = ls_progdir ).
    deserialize_program( is_progdir = ls_progdir
                         it_source  = lt_source
                         it_tpool   = lt_tpool
                         iv_package = iv_package ).

    io_xml->read( EXPORTING iv_name = 'DYNPROS'
                  CHANGING cg_data = lt_dynpros ).
    deserialize_dynpros( it_dynpros = lt_dynpros ).

    io_xml->read( EXPORTING iv_name = 'CUA'
                  CHANGING cg_data = ls_cua ).
    deserialize_cua( iv_program_name = lv_program_name
                     is_cua = ls_cua ).

    deserialize_textpool( lt_tpool ).

  ENDMETHOD.                    "lif_serialize~deserialize

ENDCLASS.                    "lcl_object_prog IMPLEMENTATION