CLASS zcl_abapgit_dependencies DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

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
      ty_dedenpencies TYPE STANDARD TABLE OF ty_dependency
                                 WITH NON-UNIQUE DEFAULT KEY .
    TYPES:
      BEGIN OF ty_item,
        obj_type TYPE tadir-object,
        obj_name TYPE tadir-obj_name,
        devclass TYPE devclass,
      END OF ty_item .

    CLASS-METHODS resolve_ddic
      CHANGING
        !ct_tadir TYPE zif_abapgit_definitions=>ty_tadir_tt
      RAISING
        zcx_abapgit_exception .
    CLASS-METHODS get_ddls_dependencies
      IMPORTING
        iv_ddls_name         TYPE tadir-obj_name
      RETURNING
        VALUE(rt_dependency) TYPE ty_dedenpencies.
    CLASS-METHODS resolve_packages
      CHANGING
        ct_tadir TYPE zif_abapgit_definitions=>ty_tadir_tt.
ENDCLASS.



CLASS zcl_abapgit_dependencies IMPLEMENTATION.


  METHOD get_ddls_dependencies.

    DATA: lt_ddls_name TYPE TABLE OF ddsymtab,
          ls_ddls_name TYPE ddsymtab.

    ls_ddls_name-name = iv_ddls_name.
    INSERT ls_ddls_name INTO TABLE lt_ddls_name.

    PERFORM ('DDLS_GET_DEP') IN PROGRAM ('RADMASDL')
                             TABLES lt_ddls_name rt_dependency.

  ENDMETHOD.


  METHOD resolve.

    DATA: lv_tabclass TYPE dd02l-tabclass.

    FIELD-SYMBOLS: <ls_tadir> LIKE LINE OF ct_tadir.

    " misuse field KORRNUM to fix deletion sequence
    " higher value means later deletion

    LOOP AT ct_tadir ASSIGNING <ls_tadir>.
      CASE <ls_tadir>-object.
        WHEN 'DEVC'.
          " Packages last
          <ls_tadir>-korrnum = '999000'.
        WHEN 'DOMA'.
          <ls_tadir>-korrnum = '900000'.
        WHEN 'SPRX'.
          <ls_tadir>-korrnum = '850000'.
        WHEN 'WEBI'.
          <ls_tadir>-korrnum = '840000'.
        WHEN 'PARA'.
          " PARA after DTEL
          <ls_tadir>-korrnum = '810000'.
        WHEN 'DTEL'.
          <ls_tadir>-korrnum = '800000'.
        WHEN 'SHLP'.
          " SHLP after TABL
          <ls_tadir>-korrnum = '760000'.
        WHEN 'TTYP' OR 'TABL' OR 'VIEW'.
          SELECT SINGLE tabclass FROM dd02l
            INTO lv_tabclass
            WHERE tabname = <ls_tadir>-obj_name
            AND as4local = 'A'
            AND as4vers = '0000'.
          IF sy-subrc = 0 AND lv_tabclass = 'APPEND'.
            " delete append structures before database tables
            <ls_tadir>-korrnum = '730000'.
          ELSE.
            <ls_tadir>-korrnum = '750000'.
          ENDIF.
        WHEN 'ENQU'.
          " ENQU before TABL
          <ls_tadir>-korrnum = '725000'.
        WHEN 'DDLS'.
          " DDLS after DCLS but before other DDIC
          <ls_tadir>-korrnum = '720000'.
        WHEN 'DDLX'.
          " DDLX before DDLS
          <ls_tadir>-korrnum = '719000'.
        WHEN 'AUTH'.
          " AUTH after DCLS
          <ls_tadir>-korrnum = '715000'.
        WHEN 'SUSH'.
          " SUSH after SUSC
          <ls_tadir>-korrnum = '712000'.
        WHEN 'SUSC'.
          " SUSC after SUSO
          <ls_tadir>-korrnum = '711000'.
        WHEN 'SUSO'.
          " SUSO after DCLS
          <ls_tadir>-korrnum = '710000'.
        WHEN 'DCLS'.
          " AUTH and SUSO after DCLS
          <ls_tadir>-korrnum = '705000'.
        WHEN 'IASP'.
          <ls_tadir>-korrnum = '552000'.
        WHEN 'IARP'.
          <ls_tadir>-korrnum = '551000'.
        WHEN 'IATU'.
          <ls_tadir>-korrnum = '550000'.
        WHEN 'ACID'.
          " ACID after PROG/FUGR/CLAS
          <ls_tadir>-korrnum = '300000'.
        WHEN 'FUGR'.
          <ls_tadir>-korrnum = '260000'.
        WHEN 'PROG'.
          " delete includes after main programs
          SELECT COUNT(*) FROM reposrc
            WHERE progname = <ls_tadir>-obj_name
            AND r3state = 'A'
            AND subc = 'I'.
          IF sy-subrc = 0.
            <ls_tadir>-korrnum = '250000'.
          ELSE.
            <ls_tadir>-korrnum = '240000'.
          ENDIF.
        WHEN 'INTF'.
          <ls_tadir>-korrnum = '230000'.
        WHEN 'CLAS'.
          <ls_tadir>-korrnum = '220000'.
        WHEN 'IDOC'.
          <ls_tadir>-korrnum = '200000'.
        WHEN 'IOBJ'.
          <ls_tadir>-korrnum = '195000'.
        WHEN 'ODSO'.
          <ls_tadir>-korrnum = '190000'.
        WHEN 'WDCA'.
          <ls_tadir>-korrnum = '174000'.
        WHEN 'WDYA'.
          <ls_tadir>-korrnum = '173000'.
        WHEN 'WDCC'.
          <ls_tadir>-korrnum = '172000'.
        WHEN 'WDYN'.
          <ls_tadir>-korrnum = '171000'.
        WHEN 'IEXT'.
          <ls_tadir>-korrnum = '150000'.
        WHEN 'SAPC'.
          " SAPC after SICF
          <ls_tadir>-korrnum = '140000'.
        WHEN 'PINF'.
          " PINF before exposed objects
          <ls_tadir>-korrnum = '130000'.
        WHEN OTHERS.
          <ls_tadir>-korrnum = '100000'.
      ENDCASE.
    ENDLOOP.

    resolve_ddic( CHANGING ct_tadir = ct_tadir ).
    resolve_packages( CHANGING ct_tadir = ct_tadir ).

    SORT ct_tadir BY korrnum ASCENDING.

  ENDMETHOD.


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
          lt_scope        TYPE STANDARD TABLE OF seu_obj,
          lt_dependency   TYPE ty_dedenpencies.

    FIELD-SYMBOLS: <ls_tadir_ddls>      TYPE zif_abapgit_definitions=>ty_tadir,
                   <ls_dependency>      TYPE ty_dependency,
                   <ls_tadir_dependent> TYPE zif_abapgit_definitions=>ty_tadir,
                   <ls_tadir>           LIKE LINE OF ct_tadir,
                   <ls_edge>            LIKE LINE OF lt_edges,
                   <ls_found>           LIKE LINE OF lt_founds,
                   <ls_node>            LIKE LINE OF lt_nodes.

    " build nodes
    LOOP AT ct_tadir ASSIGNING <ls_tadir>
        WHERE object = 'TABL'
        OR object = 'VIEW'
        OR object = 'TTYP'.
      APPEND INITIAL LINE TO lt_nodes ASSIGNING <ls_node>.
      <ls_node>-obj_name = <ls_tadir>-obj_name.
      <ls_node>-obj_type = <ls_tadir>-object.
    ENDLOOP.

    APPEND 'TABL' TO lt_scope.
    APPEND 'VIEW' TO lt_scope.
    APPEND 'STRU' TO lt_scope.
    APPEND 'TTYP' TO lt_scope.

    " build edges
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
          WHEN 'DV'.
            <ls_edge>-to-obj_type = 'VIEW'.
          WHEN 'DA'.
            <ls_edge>-to-obj_type = 'TTYP'.
          WHEN OTHERS.
            zcx_abapgit_exception=>raise( 'resolve_ddic, unknown object_cls' ).
        ENDCASE.
      ENDLOOP.

    ENDLOOP.

    " build DDLS edges
    SORT ct_tadir. "binary search
    LOOP AT ct_tadir ASSIGNING <ls_tadir_ddls>
                     WHERE object = 'DDLS'.

      CLEAR: lt_dependency.

      APPEND INITIAL LINE TO lt_nodes ASSIGNING <ls_node>.
      <ls_node>-obj_name = <ls_tadir_ddls>-obj_name.
      <ls_node>-obj_type = <ls_tadir_ddls>-object.

      lt_dependency = get_ddls_dependencies( <ls_tadir_ddls>-obj_name ).

      LOOP AT lt_dependency ASSIGNING <ls_dependency>
                            WHERE deptyp = 'DDLS'
                            AND refname = <ls_tadir_ddls>-obj_name.

        READ TABLE ct_tadir ASSIGNING <ls_tadir_dependent>
                            WITH KEY pgmid    = 'R3TR'
                                     object   = 'DDLS'
                                     obj_name = <ls_dependency>-depname
                            BINARY SEARCH.
        CHECK sy-subrc = 0.

        APPEND INITIAL LINE TO lt_edges ASSIGNING <ls_edge>.
        <ls_edge>-from = <ls_node>.
        <ls_edge>-to-obj_name = <ls_dependency>-depname.
        <ls_edge>-to-obj_type = 'DDLS'.

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

  ENDMETHOD.


  METHOD resolve_packages.

    DATA: lt_subpackages TYPE zif_abapgit_sap_package=>ty_devclass_tt.

    FIELD-SYMBOLS: <ls_tadir>            LIKE LINE OF ct_tadir,
                   <lv_subpackage>       LIKE LINE OF lt_subpackages,
                   <ls_tadir_subpackage> LIKE LINE OF ct_tadir.

    " List subpackage before corresponding superpackage

    LOOP AT ct_tadir ASSIGNING <ls_tadir>
                     WHERE object = 'DEVC'.

      lt_subpackages = zcl_abapgit_factory=>get_sap_package( |{ <ls_tadir>-obj_name }| )->list_subpackages( ).

      LOOP AT lt_subpackages ASSIGNING <lv_subpackage>.

        READ TABLE ct_tadir ASSIGNING <ls_tadir_subpackage>
                            WITH KEY object   = 'DEVC'
                                     obj_name = <lv_subpackage>.
        IF sy-subrc = 0.
          <ls_tadir_subpackage>-korrnum = condense( |{ <ls_tadir_subpackage>-korrnum - 1 }| ).
        ENDIF.

      ENDLOOP.

    ENDLOOP.

  ENDMETHOD.
ENDCLASS.
