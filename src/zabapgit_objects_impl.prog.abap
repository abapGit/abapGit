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

        lv_answer = lcl_popups=>popup_to_confirm(
          titlebar              = 'Warning'
          text_question         = lv_question
          display_cancel_button = abap_false
        ).  "#EC NOTEXT

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

      lv_answer = lcl_popups=>popup_to_confirm(
        titlebar              = 'Warning'
        text_question         = lv_question
        text_button_1         = 'Ok'
        icon_button_1         = 'ICON_DELETE'
        text_button_2         = 'Cancel'
        icon_button_2         = 'ICON_CANCEL'
        default_button        = '2'
        display_cancel_button = abap_false
      ).  "#EC NOTEXT

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
            lcx_exception=>raise( lv_message ).
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


    IF is_item IS INITIAL.
* eg. ".abapgit.xml" file
      rv_user = lcl_objects_super=>c_user_unknown.
    ELSE.
      li_obj = create_object( is_item     = is_item
                              iv_language = gc_english ).
      rv_user = li_obj->changed_by( ).
    ENDIF.

    ASSERT NOT rv_user IS INITIAL.

* todo, fallback to looking at transports if rv_user = 'UNKNOWN'?

  ENDMETHOD.

  METHOD delete.

    DATA: ls_item     TYPE ty_item,
          lv_tabclass TYPE dd02l-tabclass,
          lt_tadir    LIKE it_tadir.

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
          SELECT SINGLE tabclass FROM dd02l
            INTO lv_tabclass
            WHERE tabname = <ls_tadir>-obj_name
            AND as4local = 'A'
            AND as4vers = '0000'.
          IF sy-subrc = 0 AND lv_tabclass = 'APPEND'.
* delete append structures before database tables
            <ls_tadir>-korrnum = '6500'.
          ELSE.
            <ls_tadir>-korrnum = '7000'.
          ENDIF.
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
            lcx_exception=>raise( 'resolve_ddic, unknown object_cls' ).
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
      lcx_exception=>raise( 'Duplicates' ).
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
        lcx_exception=>raise( 'cancelled' ).
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