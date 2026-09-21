CLASS zcl_abapgit_historical_tabl DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_abapgit_historical_object .

    METHODS constructor
      IMPORTING
        is_tadir TYPE zif_abapgit_definitions=>ty_tadir .

  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA ms_tadir TYPE zif_abapgit_definitions=>ty_tadir .

    METHODS determine_parts
      RETURNING
        VALUE(rt_parts) TYPE zif_abapgit_historical_object=>ty_parts_tt .

    METHODS read_tabd
      IMPORTING
        is_vrsd            TYPE zif_abapgit_historical_object=>ty_vrsd
      RETURNING
        VALUE(rs_internal) TYPE zif_abapgit_object_tabl=>ty_internal
      RAISING
        zcx_abapgit_exception .

    METHODS is_supported
      IMPORTING
        is_internal         TYPE zif_abapgit_object_tabl=>ty_internal
        iv_versno           TYPE vrsd-versno
      RETURNING
        VALUE(rv_supported) TYPE abap_bool
      RAISING
        zcx_abapgit_exception .

    METHODS map_to_aff
      IMPORTING
        is_internal   TYPE zif_abapgit_object_tabl=>ty_internal
      RETURNING
        VALUE(rs_aff) TYPE zif_abapgit_aff_tabl_v1=>ty_main
      RAISING
        zcx_abapgit_exception .

    METHODS serialize_aff
      IMPORTING
        is_internal    TYPE zif_abapgit_object_tabl=>ty_internal
      RETURNING
        VALUE(rv_json) TYPE string
      RAISING
        zcx_abapgit_exception .

    METHODS serialize_ddic
      IMPORTING
        is_internal   TYPE zif_abapgit_object_tabl=>ty_internal
      RETURNING
        VALUE(rv_ddl) TYPE string
      RAISING
        zcx_abapgit_exception .
ENDCLASS.



CLASS ZCL_ABAPGIT_HISTORICAL_TABL IMPLEMENTATION.


  METHOD constructor.

    ms_tadir = is_tadir.

  ENDMETHOD.


  METHOD determine_parts.

    APPEND VALUE #(
      objtype  = 'TABD'
      objname  = ms_tadir-obj_name
      type     = ms_tadir-object
      name     = ms_tadir-obj_name
      devclass = ms_tadir-devclass ) TO rt_parts.

  ENDMETHOD.


  METHOD is_supported.

    DATA lv_where TYPE string.


    lv_where = |table { ms_tadir-obj_name } version { iv_versno }|.

* structures, append structures, pooled and cluster tables and IDoc segment
* tables cannot be described in DDL, they are skipped instead of failing the
* extraction of the surrounding transport
    IF is_internal-dd02v-tabclass <> 'TRANSP'.
      RETURN.
    ENDIF.

* the remaining checks mirror what zcl_abapgit_object_tabl_ddl refuses, they
* only exist to name the object and version in the message
    IF is_internal-dd02v-contflag IS INITIAL.
      zcx_abapgit_exception=>raise( |Delivery class is missing in { lv_where }| ).
    ENDIF.
    IF is_internal-dd02v-exclass NOT BETWEEN '0' AND '4'.
      zcx_abapgit_exception=>raise( |Unsupported enhancement category { is_internal-dd02v-exclass } in { lv_where }| ).
    ENDIF.
    IF is_internal-dd02v-authclass IS NOT INITIAL
        AND is_internal-dd02v-authclass <> '01'
        AND is_internal-dd02v-authclass <> '02'
        AND is_internal-dd02v-authclass <> '10'.
      zcx_abapgit_exception=>raise( |Unsupported activation type { is_internal-dd02v-authclass } in { lv_where }| ).
    ENDIF.
    IF is_internal-dd02v-mainflag IS NOT INITIAL
        AND is_internal-dd02v-mainflag <> abap_true
        AND is_internal-dd02v-mainflag <> 'N'.
      zcx_abapgit_exception=>raise( |Unsupported data maintenance { is_internal-dd02v-mainflag } in { lv_where }| ).
    ENDIF.

    rv_supported = abap_true.

  ENDMETHOD.


  METHOD map_to_aff.

    rs_aff-format_version = '1'.

    rs_aff-header-description = is_internal-dd02v-ddtext.
    IF is_internal-dd02v-ddlanguage IS INITIAL.
      zcx_abapgit_exception=>raise( |Original language is missing in table { ms_tadir-obj_name }| ).
    ENDIF.
    rs_aff-header-original_language = is_internal-dd02v-ddlanguage.
    rs_aff-header-abap_language_version = zif_abapgit_aff_types_v1=>co_abap_language_version-standard.

  ENDMETHOD.


  METHOD read_tabd.

    DATA lt_dd02v  TYPE STANDARD TABLE OF dd02v WITH DEFAULT KEY.
    DATA lt_dd08v  TYPE STANDARD TABLE OF dd08v WITH DEFAULT KEY.
    DATA lt_dd35v  TYPE STANDARD TABLE OF dd35v WITH DEFAULT KEY.
    DATA lt_dd03v  TYPE STANDARD TABLE OF dd03v WITH DEFAULT KEY.
    DATA lt_dd05v  TYPE STANDARD TABLE OF dd05v WITH DEFAULT KEY.
    DATA lt_dd36v  TYPE STANDARD TABLE OF dd36v WITH DEFAULT KEY.
    DATA lt_dd02tv TYPE STANDARD TABLE OF dd02tv WITH DEFAULT KEY.
    DATA lt_dd03tv TYPE STANDARD TABLE OF dd03tv WITH DEFAULT KEY.
    DATA lt_dd08tv TYPE STANDARD TABLE OF dd08tv WITH DEFAULT KEY.
    DATA ls_dd03p  TYPE dd03p.
    DATA ls_dd05m  TYPE dd05m.
    DATA ls_dd36m  TYPE dd36m.


    CALL FUNCTION 'SVRS_GET_VERSION_TABD_40'
      EXPORTING
        object_name           = is_vrsd-objname
        versno                = is_vrsd-versno
      TABLES
        dd02tv_tab            = lt_dd02tv
        dd02v_tab             = lt_dd02v
        dd03tv_tab            = lt_dd03tv
        dd03v_tab             = lt_dd03v
        dd05v_tab             = lt_dd05v
        dd08tv_tab            = lt_dd08tv
        dd08v_tab             = lt_dd08v
        dd35v_tab             = lt_dd35v
        dd36v_tab             = lt_dd36v
      EXCEPTIONS
        no_version            = 1
        system_failure        = 2
        communication_failure = 3
        OTHERS                = 4.
    IF sy-subrc = 1.
      RETURN.
    ELSEIF sy-subrc <> 0.
      zcx_abapgit_exception=>raise(
        |Unable to read historical TABL { is_vrsd-objname } version { is_vrsd-versno }, subrc { sy-subrc }| ).
    ENDIF.

    READ TABLE lt_dd02v INTO rs_internal-dd02v INDEX 1.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.
    IF rs_internal-dd02v-tabname IS INITIAL.
      rs_internal-dd02v-tabname = ms_tadir-obj_name.
    ENDIF.

* only the original language is extracted, translations are out of scope
    IF rs_internal-dd02v-masterlang IS NOT INITIAL.
      rs_internal-dd02v-ddlanguage = rs_internal-dd02v-masterlang.
    ENDIF.
    READ TABLE lt_dd02tv INTO DATA(ls_dd02tv)
      WITH KEY ddlanguage = rs_internal-dd02v-ddlanguage.
    IF sy-subrc <> 0 AND rs_internal-dd02v-ddlanguage IS INITIAL.
      READ TABLE lt_dd02tv INTO ls_dd02tv INDEX 1.
    ENDIF.
    IF sy-subrc = 0.
      IF rs_internal-dd02v-ddlanguage IS INITIAL.
        rs_internal-dd02v-ddlanguage = ls_dd02tv-ddlanguage.
      ENDIF.
      IF ls_dd02tv-ddtext IS NOT INITIAL.
        rs_internal-dd02v-ddtext = ls_dd02tv-ddtext.
      ENDIF.
    ENDIF.

* the version reader returns the dictionary view structures, the DDL serializer
* works on the prepared ones, the shared components are named alike. Nothing is
* sorted on the way, the serializer emits fields, foreign keys and value help
* parameters in the order they arrive
    LOOP AT lt_dd03v INTO DATA(ls_dd03v).
      CLEAR ls_dd03p.
      MOVE-CORRESPONDING ls_dd03v TO ls_dd03p.
      READ TABLE lt_dd03tv INTO DATA(ls_dd03tv)
        WITH KEY fieldname  = ls_dd03p-fieldname
                 ddlanguage = rs_internal-dd02v-ddlanguage.
      IF sy-subrc = 0.
        ls_dd03p-ddtext = ls_dd03tv-ddtext.
      ENDIF.
      APPEND ls_dd03p TO rs_internal-dd03p.
    ENDLOOP.

    LOOP AT lt_dd05v INTO DATA(ls_dd05v).
      CLEAR ls_dd05m.
      MOVE-CORRESPONDING ls_dd05v TO ls_dd05m.
      APPEND ls_dd05m TO rs_internal-dd05m.
    ENDLOOP.

    LOOP AT lt_dd36v INTO DATA(ls_dd36v).
      CLEAR ls_dd36m.
      MOVE-CORRESPONDING ls_dd36v TO ls_dd36m.
      APPEND ls_dd36m TO rs_internal-dd36m.
    ENDLOOP.

* the foreign key labels are kept in a text table of their own
    LOOP AT lt_dd08v INTO DATA(ls_dd08v).
      READ TABLE lt_dd08tv INTO DATA(ls_dd08tv)
        WITH KEY fieldname  = ls_dd08v-fieldname
                 ddlanguage = rs_internal-dd02v-ddlanguage.
      IF sy-subrc = 0.
        ls_dd08v-ddtext = ls_dd08tv-ddtext.
      ENDIF.
      APPEND ls_dd08v TO rs_internal-dd08v.
    ENDLOOP.

    rs_internal-dd35v = lt_dd35v.

  ENDMETHOD.


  METHOD serialize_aff.

    DATA(ls_aff) = map_to_aff( is_internal ).

    TRY.
        DATA(lv_json) = NEW zcl_abapgit_json_handler( )->serialize( ls_aff ).
      CATCH cx_root INTO DATA(lx_error).
        zcx_abapgit_exception=>raise_with_text( lx_error ).
    ENDTRY.

    rv_json = zcl_abapgit_convert=>xstring_to_string_utf8( lv_json ).

  ENDMETHOD.


  METHOD serialize_ddic.

* abapGit owns the DDL format, this only hands it the historical definition
    rv_ddl = NEW zcl_abapgit_object_tabl_ddl( )->serialize( is_internal ).

  ENDMETHOD.


  METHOD zif_abapgit_historical_object~build_files.

    DATA(lt_vrsd) = zcl_abapgit_historical_source=>read_versions(
      it_parts   = determine_parts( )
      iv_korrnum = iv_korrnum ).

    SORT lt_vrsd BY objtype versno DESCENDING.
    READ TABLE lt_vrsd INTO DATA(ls_vrsd) WITH KEY objtype = 'TABD'.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    DATA(ls_internal) = read_tabd( ls_vrsd ).
    IF ls_internal-dd02v IS INITIAL.
      RETURN.
    ENDIF.

    IF is_supported( is_internal = ls_internal
                     iv_versno   = ls_vrsd-versno ) = abap_false.
      RETURN.
    ENDIF.

    APPEND VALUE #(
      filename = |{ to_lower( ms_tadir-obj_name ) }.tabl.json|
      source   = serialize_aff( ls_internal ) ) TO rt_files.

    APPEND VALUE #(
      filename = |{ to_lower( ms_tadir-obj_name ) }.tabl.ddic|
      source   = serialize_ddic( ls_internal ) ) TO rt_files.

  ENDMETHOD.


  METHOD zif_abapgit_historical_object~build_deleted_files.

    APPEND VALUE #(
      filename = |{ to_lower( ms_tadir-obj_name ) }.tabl.json|
      deleted  = abap_true ) TO rt_files.

    APPEND VALUE #(
      filename = |{ to_lower( ms_tadir-obj_name ) }.tabl.ddic|
      deleted  = abap_true ) TO rt_files.

  ENDMETHOD.
ENDCLASS.
