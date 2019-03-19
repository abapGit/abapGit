CLASS zcl_abapgit_dependencies DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    CONSTANTS:
      gc_korrnum TYPE i VALUE 1000000.


    CLASS-METHODS resolve
      CHANGING
        !ct_tadir TYPE zif_abapgit_definitions=>ty_tadir_tt
      RAISING
        zcx_abapgit_exception .
  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES:
      BEGIN OF ty_dependency,
        depname  TYPE dd02l-tabname,
        deptyp   TYPE c LENGTH 4,
        deplocal TYPE dd02l-as4local,
        refname  TYPE dd02l-tabname,
        reftyp   TYPE c LENGTH 4,
        kind     TYPE c LENGTH 1,
      END OF ty_dependency .
    TYPES:
      tty_dedenpency TYPE STANDARD TABLE OF ty_dependency
                                 WITH NON-UNIQUE DEFAULT KEY .
    TYPES:
      BEGIN OF ty_item,
        obj_type TYPE tadir-object,
        obj_name TYPE tadir-obj_name,
        devclass TYPE devclass,
      END OF ty_item .

    TYPES: BEGIN OF ty_edge,
             from TYPE ty_item,
             to   TYPE ty_item,
           END OF ty_edge,
           ty_edge_tt TYPE STANDARD TABLE OF ty_edge.

    CLASS-METHODS resolve_edges_generic
      IMPORTING
        !is_tadir       TYPE zif_abapgit_definitions=>ty_tadir
        !it_tadir_scope TYPE  zif_abapgit_definitions=>ty_tadir_tt
      CHANGING
        !ct_edges       TYPE ty_edge_tt
      RAISING
        zcx_abapgit_exception .

    CLASS-METHODS resolve_edges_ddls
      IMPORTING
        !is_tadir       TYPE zif_abapgit_definitions=>ty_tadir
        !it_tadir_scope TYPE  zif_abapgit_definitions=>ty_tadir_tt
      CHANGING
        !ct_edges       TYPE ty_edge_tt
      RAISING
        zcx_abapgit_exception .

    CLASS-METHODS resolve_edges_devc
      IMPORTING
        !is_tadir       TYPE zif_abapgit_definitions=>ty_tadir
        !it_tadir_scope TYPE  zif_abapgit_definitions=>ty_tadir_tt
      CHANGING
        !ct_edges       TYPE ty_edge_tt
      RAISING
        zcx_abapgit_exception .

    CLASS-METHODS resolve_edges
      IMPORTING
        !it_tadir TYPE zif_abapgit_definitions=>ty_tadir_tt
      CHANGING
        !ct_edges TYPE ty_edge_tt
      RAISING
        zcx_abapgit_exception .


    CLASS-METHODS resolve_basic_edges
      IMPORTING
        !it_tadir TYPE zif_abapgit_definitions=>ty_tadir_tt
      CHANGING
        !ct_edges TYPE ty_edge_tt

      RAISING
        zcx_abapgit_exception .
    CLASS-METHODS get_ddls_dependencies
      IMPORTING
        iv_ddls_name         TYPE tadir-obj_name
      RETURNING
        VALUE(rt_dependency) TYPE tty_dedenpency .

ENDCLASS.



CLASS ZCL_ABAPGIT_DEPENDENCIES IMPLEMENTATION.


  METHOD get_ddls_dependencies.

    TYPES: BEGIN OF ty_ddls_name.
        INCLUDE TYPE ddsymtab.
    TYPES: END OF ty_ddls_name.

    TYPES: tty_ddls_names TYPE STANDARD TABLE OF ty_ddls_name
                               WITH NON-UNIQUE DEFAULT KEY.

    DATA: lt_ddls_name TYPE tty_ddls_names,
          ls_ddls_name LIKE LINE OF lt_ddls_name.

    ls_ddls_name-name = iv_ddls_name.
    INSERT ls_ddls_name INTO TABLE lt_ddls_name.

    PERFORM ('DDLS_GET_DEP') IN PROGRAM ('RADMASDL')
                             TABLES lt_ddls_name rt_dependency.

  ENDMETHOD.


  METHOD resolve.

    DATA: lv_tabclass TYPE dd02l-tabclass.

    FIELD-SYMBOLS: <ls_tadir> LIKE LINE OF ct_tadir.

    DATA:
          lt_edges TYPE ty_edge_tt.


    resolve_edges( EXPORTING it_tadir = ct_tadir
                   CHANGING ct_edges = lt_edges ).


    DATA: lt_nodes        TYPE TABLE OF ty_item,
          lt_findstrings  TYPE TABLE OF rsfind,
          lv_plus         TYPE i VALUE 1,
          lv_find_obj_cls TYPE euobj-id,
          lv_index        TYPE i,
          lv_before       TYPE i,
          lt_founds       TYPE TABLE OF rsfindlst,
          lt_scope        TYPE STANDARD TABLE OF seu_obj,
          lt_dependency   TYPE tty_dedenpency.

    FIELD-SYMBOLS: <ls_tadir_ddls>      TYPE zif_abapgit_definitions=>ty_tadir,
                   <ls_dependency>      TYPE ty_dependency,
                   <ls_tadir_dependent> TYPE zif_abapgit_definitions=>ty_tadir,
                   <ls_edge>            LIKE LINE OF lt_edges,
                   <ls_found>           LIKE LINE OF lt_founds.

    " get blocks in scope
    DATA(lt_blcok_scope) = ct_tadir.
    SORT lt_blcok_scope BY korrnum.
    DELETE ADJACENT DUPLICATES FROM lt_blcok_scope COMPARING korrnum.

    DO.

      " prevent endless loop
      READ TABLE lt_blcok_scope INTO DATA(ls_blcok_scope) INDEX 1.

      " sort must working for char vfield avoid numers 1 10 2 ....
      lv_plus = '1000000'.

      DO.
        " prevent endless loop
*      lv_before = lines( ct_tadir ).
        DATA(lv_plus_old) = lv_plus.


        LOOP AT ct_tadir ASSIGNING <ls_tadir> WHERE korrnum = ls_blcok_scope-korrnum.
          lv_index = sy-tabix.


          READ TABLE lt_edges WITH KEY
            from-obj_name = <ls_tadir>-obj_name
            from-obj_type = <ls_tadir>-object
            TRANSPORTING NO FIELDS.

          IF sy-subrc <> 0.
*          LOOP AT ct_tadir ASSIGNING <ls_tadir>
*              WHERE obj_name = <ls_node>-obj_name
*              AND object = <ls_node>-obj_type.
            <ls_tadir>-korrnum = <ls_tadir>-korrnum + lv_plus.
            CONDENSE <ls_tadir>-korrnum.
*          ENDLOOP.

            DELETE lt_edges
              WHERE to-obj_name = <ls_tadir>-obj_name
                AND to-obj_type = <ls_tadir>-object.

            " Next deletion step
            lv_plus = lv_plus + 1.

            EXIT. " make sure the sequence is fixed
*            else.
*              <ls_tadir>-korrnum = ls_blcok_scope-korrnum.
          ENDIF.
        ENDLOOP.

        " prevent endless loop
        IF lv_plus = lv_plus_old.
          EXIT.
        ENDIF.
      ENDDO.

      " prevent endless loop
      DELETE lt_blcok_scope INDEX 1.
      IF lt_blcok_scope IS INITIAL.
        EXIT.
      ENDIF.

    ENDDO.

*    resolve_ddic( CHANGING ct_tadir = ct_tadir ).

*    resolve_packages( CHANGING ct_tadir = ct_tadir ).

    SORT ct_tadir BY korrnum ASCENDING.

  ENDMETHOD.


  METHOD resolve_basic_edges.
* misuse field KORRNUM to fix deletion sequence

    DATA lt_basic_stack TYPE stringtab.

    " Deleta later ehen al dependencies are handeled by the object class
    APPEND 'DEVC' TO lt_basic_stack.
    APPEND 'DOMA' TO lt_basic_stack.
    APPEND 'PARA' TO lt_basic_stack.
    APPEND 'DTEL' TO lt_basic_stack.
    APPEND 'TABL' TO lt_basic_stack.
    APPEND 'VIEW' TO lt_basic_stack.
    APPEND 'TTYP' TO lt_basic_stack.
    APPEND 'DCLS' TO lt_basic_stack.
    APPEND 'AUTH' TO lt_basic_stack.
    APPEND 'IASP' TO lt_basic_stack.
    APPEND 'IARP' TO lt_basic_stack.
    APPEND 'IATU' TO lt_basic_stack.
    APPEND 'SUSC' TO lt_basic_stack.
    APPEND 'SUSO' TO lt_basic_stack.
    APPEND 'ACID' TO lt_basic_stack.
    APPEND 'IDOC' TO lt_basic_stack.
    APPEND 'IEXT' TO lt_basic_stack.

* build nodes
    LOOP AT it_tadir ASSIGNING FIELD-SYMBOL(<ls_tadir>).
      READ TABLE lt_basic_stack WITH KEY table_line = <ls_tadir>-object TRANSPORTING NO FIELDS.
      IF sy-subrc = '0'.

        data(lv_tabix) = sy-tabix.
      ELSE.
        lv_tabix = lines( lt_basic_stack ) + 1.
      ENDIF.

      DO.
        lv_tabix = lv_tabix - 1.
        IF lv_tabix = 0.
          EXIT.
        ELSE.
          READ TABLE lt_basic_stack INTO DATA(lv_edge_obj) INDEX lv_tabix.
        ENDIF.

        " collect the edges
        LOOP AT it_tadir ASSIGNING FIELD-SYMBOL(<ls_tadir_depend>) WHERE object = lv_edge_obj.

          APPEND INITIAL LINE TO ct_edges ASSIGNING FIELD-SYMBOL(<ls_edge>).
          <ls_edge>-from-obj_name = <ls_tadir>-obj_name.
          <ls_edge>-from-obj_type = <ls_tadir>-object.
          <ls_edge>-to-obj_name = <ls_tadir_depend>-obj_name.
          <ls_edge>-to-obj_type = <ls_tadir_depend>-object.

        ENDLOOP.

      ENDDO.

    ENDLOOP.

**    " Build some globa "genral" edges based on direct/Indirect dependencies
**    LOOP AT ct_tadir ASSIGNING <ls_tadir>.
**      CASE <ls_tadir>-object.
**        WHEN 'DEVC'.
**          <ls_tadir>-korrnum = gc_korrnum * 90.
**
**        WHEN 'DOMA'.
**          <ls_tadir>-korrnum = gc_korrnum * 89.
**
**        WHEN 'PARA'.
*** PARA after DTEL
**          <ls_tadir>-korrnum = gc_korrnum * 81.
**
**        WHEN 'DTEL'.
**          <ls_tadir>-korrnum = gc_korrnum * 80.
**
**        WHEN 'TTYP' OR 'TABL' OR 'VIEW'.
**          <ls_tadir>-korrnum = gc_korrnum * 75.
**
*** AUTH after DCLS
**        WHEN 'DCLS'.
**          <ls_tadir>-korrnum = gc_korrnum * 71.
**
**        WHEN 'AUTH'.
**          <ls_tadir>-korrnum = gc_korrnum * 70.
**
**        WHEN 'IASP'.
**          <ls_tadir>-korrnum = gc_korrnum * '55.2'.
**        WHEN 'IARP'.
**          <ls_tadir>-korrnum = gc_korrnum * '55.1'.
**        WHEN 'IATU'.
**          <ls_tadir>-korrnum = gc_korrnum * 55.
**
**        WHEN 'SUSC'.
**          <ls_tadir>-korrnum = gc_korrnum * 50.
**
**        WHEN 'SUSO'.
**          <ls_tadir>-korrnum = gc_korrnum * 49.
**
**
**
*** ACID after PROG/FUGR/CLAS
**        WHEN 'ACID'.
**          <ls_tadir>-korrnum = gc_korrnum * 30.
**
**        WHEN 'IDOC'.
**          <ls_tadir>-korrnum = gc_korrnum * 20.
**
**        WHEN 'IEXT'.
**          <ls_tadir>-korrnum = gc_korrnum * 15.
**
**        WHEN "'PROG' OR
**              others.
**          <ls_tadir>-korrnum = gc_korrnum * 10.
**      ENDCASE.
*    ENDLOOP.
  ENDMETHOD.


  METHOD resolve_edges.

    resolve_basic_edges( EXPORTING it_tadir = it_tadir
                          CHANGING ct_edges = ct_edges ).


* build nodes
    LOOP AT it_tadir ASSIGNING FIELD-SYMBOL(<ls_tadir>).

      CASE <ls_tadir>-object.
        WHEN 'DDLS'.
          resolve_edges_ddls( EXPORTING is_tadir = <ls_tadir>
                                        it_tadir_scope = it_tadir
                               CHANGING ct_edges = ct_edges ).
        WHEN 'DEVC'.
          resolve_edges_devc( EXPORTING is_tadir = <ls_tadir>
                                        it_tadir_scope = it_tadir
                               CHANGING ct_edges = ct_edges ).
**        WHEN 'TTYP' OR 'TABL' OR 'VIEW'.
**          SELECT SINGLE tabclass FROM dd02l
**            INTO lv_tabclass
**            WHERE tabname = <ls_tadir>-obj_name
**            AND as4local = 'A'
**            AND as4vers = '0000'.
**          IF sy-subrc = 0 AND lv_tabclass = 'APPEND'.
*** delete append structures before database tables
**            <ls_tadir>-korrnum = gc_korrnum * 65.
**          ELSE.
**            <ls_tadir>-korrnum = gc_korrnum * 75.
**          ENDIF.
**        WHEN 'PROG'.
*** delete includes after main programs
**          SELECT COUNT(*) FROM reposrc
**            WHERE progname = <ls_tadir>-obj_name
**            AND r3state = 'A'
**            AND subc = 'I'.
**          IF sy-subrc = 0.
**            <ls_tadir>-korrnum = gc_korrnum * 20.
**          ELSE.
**            <ls_tadir>-korrnum = gc_korrnum * 10.
**          ENDIF.
        WHEN OTHERS.
          resolve_edges_generic( EXPORTING is_tadir = <ls_tadir>
                                           it_tadir_scope = it_tadir
                                  CHANGING ct_edges = ct_edges ).
      ENDCASE.
    ENDLOOP.

  ENDMETHOD.


  METHOD resolve_edges_ddls.
* build DDLS edges
* build DDLS edges

    DATA:
              lt_dependency   TYPE tty_dedenpency.

    FIELD-SYMBOLS: <ls_tadir_ddls>      TYPE zif_abapgit_definitions=>ty_tadir,
                   <ls_dependency>      TYPE ty_dependency,
                   <ls_edge>            LIKE LINE OF ct_edges,
                   <ls_tadir_dependent> TYPE zif_abapgit_definitions=>ty_tadir.

*    CLEAR: lt_dependency.

*    APPEND INITIAL LINE TO lt_nodes ASSIGNING <ls_node>.
*    <ls_node>-obj_name = <ls_tadir_ddls>-obj_name.
*    <ls_node>-obj_type = <ls_tadir_ddls>-object.

    lt_dependency = get_ddls_dependencies( <ls_tadir_ddls>-obj_name ).

    LOOP AT lt_dependency ASSIGNING <ls_dependency>
                          WHERE deptyp = 'DDLS'
                          AND   refname = <ls_tadir_ddls>-obj_name.

      READ TABLE it_tadir_scope ASSIGNING <ls_tadir_dependent>
                          WITH KEY pgmid    = 'R3TR'
                                   object   = 'DDLS'
                                   obj_name = <ls_dependency>-depname
                          BINARY SEARCH.
      CHECK sy-subrc = 0.

      APPEND INITIAL LINE TO ct_edges ASSIGNING <ls_edge>.
      <ls_edge>-from-obj_name = is_tadir-obj_name.
      <ls_edge>-from-obj_type = is_tadir-object.
      <ls_edge>-to-obj_name = <ls_dependency>-depname.
      <ls_edge>-to-obj_type = 'DDLS'.

    ENDLOOP.
  ENDMETHOD.


  METHOD resolve_edges_devc.

    DATA: lt_subpackages TYPE zif_abapgit_sap_package=>ty_devclass_tt.

    FIELD-SYMBOLS: "<ls_tadir>            LIKE LINE OF ct_tadir,
      <lv_subpackage>       LIKE LINE OF lt_subpackages,
      <ls_edge>             LIKE LINE OF ct_edges,
      <ls_tadir_subpackage> LIKE LINE OF it_tadir_scope.

    " List subpackage before corresponding superpackage

    lt_subpackages = zcl_abapgit_factory=>get_sap_package( |{ is_tadir-obj_name }| )->list_subpackages( ).

    LOOP AT lt_subpackages ASSIGNING <lv_subpackage>.

      READ TABLE it_tadir_scope ASSIGNING <ls_tadir_subpackage>
                          WITH KEY object   = 'DEVC'
                                   obj_name = <lv_subpackage>.
      IF sy-subrc = 0.
**          <ls_tadir_subpackage>-korrnum = condense( |{ <ls_tadir_subpackage>-korrnum - 1 }| ).

        APPEND INITIAL LINE TO ct_edges ASSIGNING <ls_edge>.
        <ls_edge>-from-obj_name = is_tadir-obj_name.
        <ls_edge>-from-obj_type = is_tadir-object.
        <ls_edge>-to-obj_name = <lv_subpackage>.
        <ls_edge>-to-obj_type = 'DEVC'.

      ENDIF.

    ENDLOOP.
  ENDMETHOD.


  METHOD resolve_edges_generic.
* this will make sure the deletion sequence of structures/tables work
* in case they have dependencies with .INCLUDE

    DATA: lt_nodes        TYPE TABLE OF ty_item,
          lt_edges        TYPE TABLE OF ty_edge,
          lt_findstrings  TYPE TABLE OF rsfind,
          lv_plus         TYPE i VALUE 1,
          lv_find_obj_cls TYPE euobj-id,
          lv_index        TYPE i,
          lv_before       TYPE i,
          lt_founds       TYPE TABLE OF rsfindlst,
          lt_scope        TYPE STANDARD TABLE OF seu_obj,
          lt_dependency   TYPE tty_dedenpency.

    FIELD-SYMBOLS: <ls_tadir_ddls>      TYPE zif_abapgit_definitions=>ty_tadir,
                   <ls_dependency>      TYPE ty_dependency,
                   <ls_tadir_dependent> TYPE zif_abapgit_definitions=>ty_tadir,
                   <ls_edge>            LIKE LINE OF lt_edges,
                   <ls_found>           LIKE LINE OF lt_founds,
                   <ls_node>            LIKE LINE OF lt_nodes.

* build nodes
    APPEND INITIAL LINE TO lt_nodes ASSIGNING <ls_node>.
    <ls_node>-obj_name = is_tadir-obj_name.
    <ls_node>-obj_type = is_tadir-object.

    " Only find object that cause troubls
*    APPEND 'TABL' TO lt_scope.
*    APPEND 'STRU' TO lt_scope.
*    APPEND 'TTYP' TO lt_scope.
*    APPEND 'DTEL' TO lt_scope.
*    APPEND 'SHLP' TO lt_scope.
**********************************************************************
    APPEND 'DH' TO lt_scope.
    APPEND 'DE' TO lt_scope.
    APPEND 'DT' TO lt_scope.
    APPEND 'DTF' TO lt_scope.
    APPEND 'DSF' TO lt_scope.
    APPEND 'L' TO lt_scope.
    APPEND 'P' TO lt_scope.
    APPEND 'PS' TO lt_scope.
    APPEND 'KI' TO lt_scope.
**********************************************************************

* build edges
    LOOP AT lt_nodes ASSIGNING <ls_node>.

      CLEAR lt_findstrings.
      APPEND <ls_node>-obj_name TO lt_findstrings.
      lv_find_obj_cls = <ls_node>-obj_type.

      CALL FUNCTION 'RS_EU_CROSSREF'
        EXPORTING
          i_find_obj_cls           = lv_find_obj_cls
          no_dialog                = abap_true
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

**********************************************************************
        DATA:
          ls_object_key       TYPE cts_object_key,
          lv_r3tr_object      TYPE trobjtype,
          lv_limu_object      TYPE trobjtype,
          lt_r3tr_object_keys TYPE cts_object_keys.

        IF strlen( <ls_found>-object_cls ) < 4.
          CALL FUNCTION 'GET_TADIR_TYPE_FROM_WB_TYPE'
            EXPORTING
              wb_objtype        = CONV seu_objtyp( <ls_found>-object_cls )
            IMPORTING
              transport_objtype = lv_r3tr_object.

          <ls_edge>-to-obj_type   = lv_r3tr_object.
          IF <ls_found>-encl_objec IS INITIAL.
            <ls_edge>-to-obj_name  = <ls_found>-object.
          ELSE.
            <ls_edge>-to-obj_name    = <ls_found>-encl_objec.
          ENDIF.
**          ENDIF.

        ELSE.
          BREAK-POINT.
        ENDIF.
**********************************************************************

      ENDLOOP.

    ENDLOOP.

    LOOP AT lt_nodes ASSIGNING <ls_node>
                     WHERE obj_type = 'TABL'.

      DATA(otto) = CAST cl_abap_structdescr( cl_abap_structdescr=>describe_by_name( <ls_node>-obj_name ) ).
      DATA(lt_incl) = otto->get_components( ).

      LOOP AT lt_incl ASSIGNING FIELD-SYMBOL(<ls_karl>) WHERE  as_include = abap_true .

        APPEND INITIAL LINE TO lt_edges ASSIGNING <ls_edge>.
        <ls_edge>-from = <ls_node>.
        <ls_edge>-to-obj_type   = 'TABL'.
        <ls_edge>-to-obj_name  = <ls_karl>-name.


      ENDLOOP.

*      lt_incl
    ENDLOOP.






  ENDMETHOD.
ENDCLASS.
