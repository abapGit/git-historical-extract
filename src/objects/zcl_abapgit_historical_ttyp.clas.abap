CLASS zcl_abapgit_historical_ttyp DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_abapgit_historical_object .

    METHODS constructor
      IMPORTING
        is_tadir TYPE zif_abapgit_definitions=>ty_tadir .

  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES:
      BEGIN OF ty_ttyp,
        dd40v TYPE dd40v,
        dd42v TYPE dd42v_tab,
        dd43v TYPE dd43v_tab,
      END OF ty_ttyp .

    DATA ms_tadir TYPE zif_abapgit_definitions=>ty_tadir .

    METHODS determine_parts
      RETURNING
        VALUE(rt_parts) TYPE zif_abapgit_historical_object=>ty_parts_tt .

    METHODS read_ttyd
      IMPORTING
        is_vrsd        TYPE zif_abapgit_historical_object=>ty_vrsd
      RETURNING
        VALUE(rs_ttyp) TYPE ty_ttyp
      RAISING
        zcx_abapgit_exception .

    METHODS normalize
      IMPORTING
        is_ttyp        TYPE ty_ttyp
      RETURNING
        VALUE(rs_ttyp) TYPE ty_ttyp
      RAISING
        zcx_abapgit_exception .

    METHODS serialize_xml
      IMPORTING
        is_ttyp       TYPE ty_ttyp
      RETURNING
        VALUE(rv_xml) TYPE string
      RAISING
        zcx_abapgit_exception .
ENDCLASS.



CLASS ZCL_ABAPGIT_HISTORICAL_TTYP IMPLEMENTATION.


  METHOD constructor.

    ms_tadir = is_tadir.

  ENDMETHOD.


  METHOD determine_parts.

    APPEND VALUE #(
      objtype  = 'TTYD'
      objname  = ms_tadir-obj_name
      type     = ms_tadir-object
      name     = ms_tadir-obj_name
      devclass = ms_tadir-devclass ) TO rt_parts.

  ENDMETHOD.


  METHOD normalize.

    FIELD-SYMBOLS <lv_value> TYPE any.


    rs_ttyp = is_ttyp.

    IF rs_ttyp-dd40v-ddlanguage IS INITIAL.
      zcx_abapgit_exception=>raise( |Original language is missing in table type { ms_tadir-obj_name }| ).
    ENDIF.

* the same fields abapGit clears when it serializes a table type
    CLEAR: rs_ttyp-dd40v-as4user,
           rs_ttyp-dd40v-as4date,
           rs_ttyp-dd40v-as4time.

    IF rs_ttyp-dd40v-rowkind IS NOT INITIAL.
      CLEAR rs_ttyp-dd40v-typelen.
    ENDIF.

    ASSIGN COMPONENT 'ACTFLAG' OF STRUCTURE rs_ttyp-dd40v TO <lv_value>.
    IF sy-subrc = 0.
      CLEAR <lv_value>.
    ENDIF.

* the primary key comes first, its fields carry no secondary key name
    SORT rs_ttyp-dd42v BY seckeyname keyfdpos.
    SORT rs_ttyp-dd43v BY seckeyname.

  ENDMETHOD.


  METHOD read_ttyd.

    DATA lt_dd40v  TYPE STANDARD TABLE OF dd40v WITH DEFAULT KEY.
    DATA lt_dd40tv TYPE STANDARD TABLE OF dd40tv WITH DEFAULT KEY.
    DATA lt_dd43tv TYPE STANDARD TABLE OF dd43t WITH DEFAULT KEY.


    CALL FUNCTION 'SVRS_GET_VERSION_TTYD_40'
      EXPORTING
        object_name           = is_vrsd-objname
        versno                = is_vrsd-versno
      TABLES
        dd40tv_tab            = lt_dd40tv
        dd40v_tab             = lt_dd40v
        dd42v_tab             = rs_ttyp-dd42v
        dd43tv_tab            = lt_dd43tv
        dd43v_tab             = rs_ttyp-dd43v
      EXCEPTIONS
        no_version            = 1
        system_failure        = 2
        communication_failure = 3
        OTHERS                = 4.
    IF sy-subrc = 1.
      CLEAR rs_ttyp.
      RETURN.
    ELSEIF sy-subrc <> 0.
      zcx_abapgit_exception=>raise(
        |Unable to read historical TTYP { is_vrsd-objname } version { is_vrsd-versno }, subrc { sy-subrc }| ).
    ENDIF.

    READ TABLE lt_dd40v INTO rs_ttyp-dd40v INDEX 1.
    IF sy-subrc <> 0.
      CLEAR rs_ttyp.
      RETURN.
    ENDIF.
    IF rs_ttyp-dd40v-typename IS INITIAL.
      rs_ttyp-dd40v-typename = ms_tadir-obj_name.
    ENDIF.

* only the original language is extracted, translations are out of scope
    READ TABLE lt_dd40tv INTO DATA(ls_dd40tv)
      WITH KEY ddlanguage = rs_ttyp-dd40v-ddlanguage.
    IF sy-subrc <> 0 AND rs_ttyp-dd40v-ddlanguage IS INITIAL.
      READ TABLE lt_dd40tv INTO ls_dd40tv INDEX 1.
    ENDIF.
    IF sy-subrc = 0.
      IF rs_ttyp-dd40v-ddlanguage IS INITIAL.
        rs_ttyp-dd40v-ddlanguage = ls_dd40tv-ddlanguage.
      ENDIF.
      rs_ttyp-dd40v-ddtext = ls_dd40tv-ddtext.
    ENDIF.

* the secondary key descriptions are kept in a text table of their own, a key
* without a description in the original language keeps no translated text either
    LOOP AT rs_ttyp-dd43v ASSIGNING FIELD-SYMBOL(<ls_dd43v>).
      READ TABLE lt_dd43tv INTO DATA(ls_dd43tv)
        WITH KEY seckeyname = <ls_dd43v>-seckeyname
                 ddlanguage = rs_ttyp-dd40v-ddlanguage.
      IF sy-subrc = 0.
        <ls_dd43v>-ddlanguage = ls_dd43tv-ddlanguage.
        <ls_dd43v>-keydescription = ls_dd43tv-ddtext.
      ELSEIF <ls_dd43v>-ddlanguage <> rs_ttyp-dd40v-ddlanguage.
        CLEAR: <ls_dd43v>-ddlanguage,
               <ls_dd43v>-keydescription.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD serialize_xml.

* abapGit has no ABAP file format for table types, the file is written exactly
* as zcl_abapgit_object_ttyp serializes it
    DATA(li_xml) = CAST zif_abapgit_xml_output( NEW zcl_abapgit_xml_output( ) ).

    li_xml->add( iv_name = 'DD40V'
                 ig_data = is_ttyp-dd40v ).
    li_xml->add( iv_name = 'DD42V'
                 ig_data = is_ttyp-dd42v ).
    li_xml->add( iv_name = 'DD43V'
                 ig_data = is_ttyp-dd43v ).

    rv_xml = li_xml->render( is_metadata = VALUE #(
      class   = 'LCL_OBJECT_TTYP'
      version = 'v1.0.0' ) ).

    REPLACE FIRST OCCURRENCE
      OF REGEX '<\?xml version="1\.0" encoding="[\w-]+"\?>'
      IN rv_xml
      WITH '<?xml version="1.0" encoding="utf-8"?>' ##REGEX_POSIX.
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( |XML declaration missing in table type { ms_tadir-obj_name }| ).
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_historical_object~build_files.

    DATA(lt_vrsd) = zcl_abapgit_historical_source=>read_versions(
      it_parts   = determine_parts( )
      iv_korrnum = iv_korrnum ).

    SORT lt_vrsd BY objtype versno DESCENDING.
    READ TABLE lt_vrsd INTO DATA(ls_vrsd) WITH KEY objtype = 'TTYD'.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    DATA(ls_ttyp) = read_ttyd( ls_vrsd ).
    IF ls_ttyp-dd40v IS INITIAL.
      RETURN.
    ENDIF.

    APPEND VALUE #(
      filename = |{ to_lower( ms_tadir-obj_name ) }.ttyp.xml|
      source   = serialize_xml( normalize( ls_ttyp ) ) ) TO rt_files.

  ENDMETHOD.


  METHOD zif_abapgit_historical_object~build_deleted_files.

    APPEND VALUE #(
      filename = |{ to_lower( ms_tadir-obj_name ) }.ttyp.xml|
      deleted  = abap_true ) TO rt_files.

  ENDMETHOD.
ENDCLASS.
