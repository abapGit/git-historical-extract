CLASS zcl_abapgit_historical_prog DEFINITION
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
      BEGIN OF ty_prog,
        progdir TYPE zif_abapgit_sap_report=>ty_progdir,
        tpool   TYPE textpool_table,
      END OF ty_prog .

    DATA ms_tadir TYPE zif_abapgit_definitions=>ty_tadir .

    METHODS determine_parts
      RETURNING
        VALUE(rt_parts) TYPE zif_abapgit_historical_object=>ty_parts_tt .

    METHODS get_file_name
      IMPORTING
        iv_ext             TYPE string
      RETURNING
        VALUE(rv_filename) TYPE string .

    METHODS read_rept
      IMPORTING
        is_vrsd         TYPE zif_abapgit_historical_object=>ty_vrsd
        iv_language     TYPE sy-langu
      RETURNING
        VALUE(rt_tpool) TYPE textpool_table
      RAISING
        zcx_abapgit_exception .

    METHODS normalize
      IMPORTING
        is_prog        TYPE ty_prog
      RETURNING
        VALUE(rs_prog) TYPE ty_prog
      RAISING
        zcx_abapgit_exception .

    METHODS serialize_xml
      IMPORTING
        is_prog       TYPE ty_prog
      RETURNING
        VALUE(rv_xml) TYPE string
      RAISING
        zcx_abapgit_exception .
ENDCLASS.



CLASS ZCL_ABAPGIT_HISTORICAL_PROG IMPLEMENTATION.


  METHOD constructor.

    ms_tadir = is_tadir.

  ENDMETHOD.


  METHOD determine_parts.

* screens (DYNP), the GUI status (CUAD), documentation and variants are not extracted yet
    LOOP AT VALUE zif_abapgit_historical_object=>ty_parts_tt(
        ( objtype = 'REPS' )
        ( objtype = 'REPT' ) ) INTO DATA(ls_part).
      ls_part-objname = ms_tadir-obj_name.
      ls_part-type = ms_tadir-object.
      ls_part-name = ms_tadir-obj_name.
      ls_part-devclass = ms_tadir-devclass.
      INSERT ls_part INTO TABLE rt_parts.
    ENDLOOP.

  ENDMETHOD.


  METHOD get_file_name.

    rv_filename = |{ to_lower( ms_tadir-obj_name ) }.prog.{ iv_ext }|.

  ENDMETHOD.


  METHOD normalize.

    rs_prog = is_prog.

    IF rs_prog-progdir-rload IS INITIAL.
      zcx_abapgit_exception=>raise( |Original language is missing in program { ms_tadir-obj_name }| ).
    ENDIF.
    IF rs_prog-progdir-name IS INITIAL.
      rs_prog-progdir-name = ms_tadir-obj_name.
    ENDIF.

* the same fields abapGit clears when it reads the program attributes
    CLEAR: rs_prog-progdir-edtx,
           rs_prog-progdir-cnam,
           rs_prog-progdir-cdat,
           rs_prog-progdir-unam,
           rs_prog-progdir-udat,
           rs_prog-progdir-levl,
           rs_prog-progdir-vern,
           rs_prog-progdir-rmand,
           rs_prog-progdir-sdate,
           rs_prog-progdir-stime,
           rs_prog-progdir-idate,
           rs_prog-progdir-itime,
           rs_prog-progdir-varcl,
           rs_prog-progdir-state.

* abapGit drops a title without text, and the text pool comes sorted as READ TEXTPOOL returns it
    DELETE rs_prog-tpool WHERE id = 'R' AND key IS INITIAL AND length = 0.
    SORT rs_prog-tpool BY id key.

  ENDMETHOD.


  METHOD read_rept.

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


  METHOD serialize_xml.

    DATA lt_tpool TYPE zif_abapgit_lang_definitions=>ty_tpool_tt.


* the file is written as zcl_abapgit_object_prog serializes it, the screens and the
* GUI status which abapGit adds for executable programs and module pools are not extracted
    DATA(li_xml) = CAST zif_abapgit_xml_output( NEW zcl_abapgit_xml_output( ) ).

    li_xml->add( iv_name = 'PROGDIR'
                 ig_data = is_prog-progdir ).

* a selection text keeps its dictionary reference flag in the first eight characters,
* abapGit moves them to SPLIT
    LOOP AT is_prog-tpool INTO DATA(ls_tpool).
      INSERT INITIAL LINE INTO TABLE lt_tpool ASSIGNING FIELD-SYMBOL(<ls_tpool>).
      MOVE-CORRESPONDING ls_tpool TO <ls_tpool>.
      IF <ls_tpool>-id = 'S'.
        <ls_tpool>-split = <ls_tpool>-entry.
        <ls_tpool>-entry = <ls_tpool>-entry+8.
      ENDIF.
    ENDLOOP.
    li_xml->add( iv_name = 'TPOOL'
                 ig_data = lt_tpool ).

    rv_xml = li_xml->render( is_metadata = VALUE #(
      class   = 'LCL_OBJECT_PROG'
      version = 'v1.0.0' ) ).

    REPLACE FIRST OCCURRENCE
      OF REGEX '<\?xml version="1\.0" encoding="[\w-]+"\?>'
      IN rv_xml
      WITH '<?xml version="1.0" encoding="utf-8"?>' ##REGEX_POSIX.
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( |XML declaration missing in program { ms_tadir-obj_name }| ).
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_historical_object~build_files.

    DATA ls_prog TYPE ty_prog.


* a transport usually carries only the parts that changed, so the program is rebuilt from the
* newest version of every part as of this transport
    DATA(lt_snapshot) = zcl_abapgit_historical_source=>read_snapshot(
      it_parts   = determine_parts( )
      iv_korrnum = iv_korrnum ).
    IF NOT line_exists( lt_snapshot[ korrnum = iv_korrnum ] ).
      RETURN.
    ENDIF.

* the source version carries the program attributes too, without it there is no program
    READ TABLE lt_snapshot INTO DATA(ls_reps) WITH KEY objtype = 'REPS'.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.
    DATA(ls_program) = zcl_abapgit_historical_source=>read_program( ls_reps ).
    IF ls_program IS INITIAL.
      RETURN.
    ENDIF.

    ls_prog-progdir = CORRESPONDING #( ls_program-trdir ).

    READ TABLE lt_snapshot INTO DATA(ls_rept) WITH KEY objtype = 'REPT'.
    IF sy-subrc = 0 AND ls_prog-progdir-rload IS NOT INITIAL.
      ls_prog-tpool = read_rept(
        is_vrsd     = ls_rept
        iv_language = ls_prog-progdir-rload ).
    ENDIF.

* abapGit ends every source file with a newline
    INSERT VALUE #(
      filename = get_file_name( 'abap' )
      source   = |{ ls_program-source }\n| ) INTO TABLE rt_files.
    INSERT VALUE #(
      filename = get_file_name( 'xml' )
      source   = serialize_xml( normalize( ls_prog ) ) ) INTO TABLE rt_files.

  ENDMETHOD.


  METHOD zif_abapgit_historical_object~build_deleted_files.

    INSERT VALUE #(
      filename = get_file_name( 'abap' )
      deleted  = abap_true ) INTO TABLE rt_files.
    INSERT VALUE #(
      filename = get_file_name( 'xml' )
      deleted  = abap_true ) INTO TABLE rt_files.

  ENDMETHOD.
ENDCLASS.
