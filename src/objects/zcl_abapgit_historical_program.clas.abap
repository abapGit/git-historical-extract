CLASS zcl_abapgit_historical_program DEFINITION
  PUBLIC
  CREATE PUBLIC .

* the program attributes and text pool as abapGit's PROG and FUGR serializers write them
  PUBLIC SECTION.

* the text pool of a REPT version in the given language, empty when the version does not exist
    CLASS-METHODS read_textpool
      IMPORTING
        is_vrsd         TYPE zif_abapgit_historical_object=>ty_vrsd
        iv_language     TYPE sy-langu
      RETURNING
        VALUE(rt_tpool) TYPE textpool_table
      RAISING
        zcx_abapgit_exception .

    CLASS-METHODS normalize_progdir
      IMPORTING
        is_progdir        TYPE zif_abapgit_sap_report=>ty_progdir
      RETURNING
        VALUE(rs_progdir) TYPE zif_abapgit_sap_report=>ty_progdir .

    CLASS-METHODS normalize_textpool
      IMPORTING
        it_tpool        TYPE textpool_table
      RETURNING
        VALUE(rt_tpool) TYPE textpool_table .

    CLASS-METHODS add_program
      IMPORTING
        ii_xml     TYPE REF TO zif_abapgit_xml_output
        is_progdir TYPE zif_abapgit_sap_report=>ty_progdir
        it_tpool   TYPE textpool_table
      RAISING
        zcx_abapgit_exception .

* renders with the utf-8 declaration abapGit writes, the include files of a function group
* are written without serializer metadata
    CLASS-METHODS render
      IMPORTING
        ii_xml        TYPE REF TO zif_abapgit_xml_output
        is_metadata   TYPE zif_abapgit_definitions=>ty_metadata OPTIONAL
        iv_name       TYPE csequence
      RETURNING
        VALUE(rv_xml) TYPE string
      RAISING
        zcx_abapgit_exception .

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_ABAPGIT_HISTORICAL_PROGRAM IMPLEMENTATION.


  METHOD add_program.

    DATA lt_tpool TYPE zif_abapgit_lang_definitions=>ty_tpool_tt.


    ii_xml->add( iv_name = 'PROGDIR'
                 ig_data = is_progdir ).

* a selection text keeps its dictionary reference flag in the first eight characters,
* abapGit moves them to SPLIT
    LOOP AT it_tpool INTO DATA(ls_tpool).
      INSERT INITIAL LINE INTO TABLE lt_tpool ASSIGNING FIELD-SYMBOL(<ls_tpool>).
      MOVE-CORRESPONDING ls_tpool TO <ls_tpool>.
      IF <ls_tpool>-id = 'S'.
        <ls_tpool>-split = <ls_tpool>-entry.
        <ls_tpool>-entry = <ls_tpool>-entry+8.
      ENDIF.
    ENDLOOP.
    ii_xml->add( iv_name = 'TPOOL'
                 ig_data = lt_tpool ).

  ENDMETHOD.


  METHOD normalize_progdir.

    rs_progdir = is_progdir.

* the same fields abapGit clears when it reads the program attributes
    CLEAR: rs_progdir-edtx,
           rs_progdir-cnam,
           rs_progdir-cdat,
           rs_progdir-unam,
           rs_progdir-udat,
           rs_progdir-levl,
           rs_progdir-vern,
           rs_progdir-rmand,
           rs_progdir-sdate,
           rs_progdir-stime,
           rs_progdir-idate,
           rs_progdir-itime,
           rs_progdir-varcl,
           rs_progdir-state.

  ENDMETHOD.


  METHOD normalize_textpool.

    rt_tpool = it_tpool.

* abapGit drops a title without text, and the text pool comes sorted as READ TEXTPOOL returns it
    DELETE rt_tpool WHERE id = 'R' AND key IS INITIAL AND length = 0.
    SORT rt_tpool BY id key.

  ENDMETHOD.


  METHOD read_textpool.

    DATA lt_textpoolt TYPE STANDARD TABLE OF textpoolt WITH DEFAULT KEY.

    FIELD-SYMBOLS <lv_language> TYPE any.


    CALL FUNCTION 'SVRS_GET_VERSION_REPT_40'
      EXPORTING
        object_name           = is_vrsd-objname
        versno                = is_vrsd-versno
      TABLES
        repot_tab             = lt_textpoolt
      EXCEPTIONS
        no_version            = 1
        system_failure        = 2
        communication_failure = 3
        OTHERS                = 4.
    IF sy-subrc = 1.
      RETURN.
    ELSEIF sy-subrc <> 0.
      zcx_abapgit_exception=>raise(
        |Unable to read historical REPT { is_vrsd-objname } version { is_vrsd-versno }, subrc { sy-subrc }| ).
    ENDIF.

* only the original language is extracted, translations are out of scope. The language is
* read dynamically, it is not confirmed on a system that TEXTPOOLT carries one
    LOOP AT lt_textpoolt ASSIGNING FIELD-SYMBOL(<ls_textpoolt>).
      ASSIGN COMPONENT 'LANGUAGE' OF STRUCTURE <ls_textpoolt> TO <lv_language>.
      IF sy-subrc = 0 AND <lv_language> <> iv_language.
        CONTINUE.
      ENDIF.
      INSERT CORRESPONDING textpool( <ls_textpoolt> ) INTO TABLE rt_tpool.
    ENDLOOP.

  ENDMETHOD.


  METHOD render.

    rv_xml = ii_xml->render( is_metadata = is_metadata ).

    REPLACE FIRST OCCURRENCE
      OF REGEX '<\?xml version="1\.0" encoding="[\w-]+"\?>'
      IN rv_xml
      WITH '<?xml version="1.0" encoding="utf-8"?>' ##REGEX_POSIX.
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( |XML declaration missing in { iv_name }| ).
    ENDIF.

  ENDMETHOD.
ENDCLASS.
