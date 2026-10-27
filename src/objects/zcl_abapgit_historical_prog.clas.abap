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

    METHODS get_original_language
      IMPORTING
        is_progdir         TYPE zif_abapgit_sap_report=>ty_progdir
      RETURNING
        VALUE(rv_language) TYPE sy-langu .

    METHODS normalize
      IMPORTING
        is_prog        TYPE ty_prog
      RETURNING
        VALUE(rs_prog) TYPE ty_prog .

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


  METHOD get_original_language.

* a program without original language is taken as English
    rv_language = is_progdir-rload.
    IF rv_language IS INITIAL.
      rv_language = 'E'.
    ENDIF.

  ENDMETHOD.


  METHOD normalize.

    rs_prog-progdir = zcl_abapgit_historical_program=>normalize_progdir( is_prog-progdir ).
    rs_prog-tpool = zcl_abapgit_historical_program=>normalize_textpool( is_prog-tpool ).

    rs_prog-progdir-rload = get_original_language( rs_prog-progdir ).
    IF rs_prog-progdir-name IS INITIAL.
      rs_prog-progdir-name = ms_tadir-obj_name.
    ENDIF.

  ENDMETHOD.


  METHOD serialize_xml.

* the file is written as zcl_abapgit_object_prog serializes it, the screens and the
* GUI status which abapGit adds for executable programs and module pools are not extracted
    DATA(li_xml) = CAST zif_abapgit_xml_output( NEW zcl_abapgit_xml_output( ) ).

    zcl_abapgit_historical_program=>add_program(
      ii_xml     = li_xml
      is_progdir = is_prog-progdir
      it_tpool   = is_prog-tpool ).

    rv_xml = zcl_abapgit_historical_program=>render(
      ii_xml      = li_xml
      is_metadata = VALUE #(
        class   = 'LCL_OBJECT_PROG'
        version = 'v1.0.0' )
      iv_name     = |program { ms_tadir-obj_name }| ).

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
    IF sy-subrc = 0.
      ls_prog-tpool = zcl_abapgit_historical_program=>read_textpool(
        is_vrsd     = ls_rept
        iv_language = get_original_language( ls_prog-progdir ) ).
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
