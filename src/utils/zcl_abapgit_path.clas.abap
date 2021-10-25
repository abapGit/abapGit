CLASS zcl_abapgit_path DEFINITION
  PUBLIC FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.

    CLASS-METHODS split_file_location
      IMPORTING iv_fullpath TYPE string
      EXPORTING ev_path     TYPE string
                ev_filename TYPE string.

    CLASS-METHODS is_root
      IMPORTING iv_path       TYPE string
      RETURNING VALUE(rv_yes) TYPE abap_bool.

    CLASS-METHODS is_subdir
      IMPORTING iv_path       TYPE string
                iv_parent     TYPE string
      RETURNING VALUE(rv_yes) TYPE abap_bool.

    CLASS-METHODS change_dir
      IMPORTING iv_cur_dir     TYPE string
                iv_cd          TYPE string
      RETURNING VALUE(rv_path) TYPE string.

    CLASS-METHODS get_filename_from_syspath
      IMPORTING iv_path            TYPE string
      RETURNING VALUE(rv_filename) TYPE string.

ENDCLASS.



CLASS ZCL_ABAPGIT_PATH IMPLEMENTATION.


  METHOD change_dir.

    DATA: lv_last TYPE i,
          lv_temp TYPE string.

    lv_last = strlen( iv_cur_dir ) - 1.

    IF iv_cd = '' OR iv_cd = '.'. " No change
      rv_path = iv_cur_dir.
    ELSEIF iv_cd+0(1) = '/'.      " Absolute path
      rv_path = iv_cd.
    ELSEIF iv_cd = '..'.          " CD back
      IF iv_cur_dir = '/' OR iv_cur_dir = ''. " Back from root = root
        rv_path = iv_cur_dir.
      ELSE.
        lv_temp = reverse( iv_cur_dir ).
        IF lv_temp+0(1) = '/'.
          SHIFT lv_temp BY 1 PLACES LEFT.
        ENDIF.
        SHIFT lv_temp UP TO '/' LEFT.
        rv_path = reverse( lv_temp ).
      ENDIF.
    ELSEIF iv_cur_dir+lv_last(1) = '/'.  " Append cd to cur_dir separated by /
      rv_path = iv_cur_dir && iv_cd.
    ELSE.
      rv_path = iv_cur_dir && '/' && iv_cd.
    ENDIF.

    " TODO: improve logic and cases

  ENDMETHOD.


  METHOD get_filename_from_syspath.

    DATA: lv_split TYPE c LENGTH 1,
          lv_index TYPE i,
          lt_split TYPE TABLE OF string.

    " filename | c:\filename | /dir/filename | \\server\filename
    IF iv_path CA '/'.
      lv_split = '/'.
    ELSE.
      lv_split = '\'.
    ENDIF.

    SPLIT iv_path AT lv_split INTO TABLE lt_split.

    lv_index = lines( lt_split ).

    READ TABLE lt_split INDEX lv_index INTO rv_filename.

  ENDMETHOD.


  METHOD is_root.
    rv_yes = boolc( iv_path = '/' ).
  ENDMETHOD.


  METHOD is_subdir.

    DATA lv_len  TYPE i.
    DATA lv_last TYPE i.

    lv_len  = strlen( iv_parent ).
    lv_last = lv_len - 1.
    rv_yes  = boolc( strlen( iv_path ) > lv_len
                 AND iv_path+0(lv_len) = iv_parent
                 AND ( iv_parent+lv_last(1) = '/' OR iv_path+lv_len(1) = '/' ) ).

  ENDMETHOD.


  METHOD split_file_location.

    DATA: lv_cnt TYPE i,
          lv_len TYPE i.

    FIND FIRST OCCURRENCE OF REGEX '^/(.*/)?' IN iv_fullpath
      MATCH COUNT lv_cnt
      MATCH LENGTH lv_len.

    IF lv_cnt > 0.
      ev_path     = iv_fullpath+0(lv_len).
      ev_filename = iv_fullpath+lv_len.
    ELSE.
      CLEAR ev_path.
      ev_filename = iv_fullpath.
    ENDIF.

    ev_filename = cl_http_utility=>unescape_url( escaped = ev_filename ).

  ENDMETHOD.
ENDCLASS.
