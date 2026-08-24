CLASS zcl_abapgit_historical_domd DEFINITION
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

    METHODS read_domd
      IMPORTING
        is_vrsd TYPE zif_abapgit_historical_object=>ty_vrsd .
ENDCLASS.



CLASS ZCL_ABAPGIT_HISTORICAL_DOMD IMPLEMENTATION.


  METHOD constructor.

    ms_tadir = is_tadir.

  ENDMETHOD.


  METHOD determine_parts.

    APPEND VALUE #(
      objtype  = 'DOMD'
      objname  = ms_tadir-obj_name
      type     = ms_tadir-object
      name     = ms_tadir-obj_name
      devclass = ms_tadir-devclass ) TO rt_parts.

  ENDMETHOD.


  METHOD read_domd.

    DATA lt_dd01v  TYPE STANDARD TABLE OF dd01v WITH DEFAULT KEY.
    DATA lt_dd01tv TYPE STANDARD TABLE OF dd01tv WITH DEFAULT KEY.
    DATA lt_dd07v  TYPE STANDARD TABLE OF dd07v WITH DEFAULT KEY.
    DATA lt_dd07tv TYPE STANDARD TABLE OF dd07tv WITH DEFAULT KEY.

    CALL FUNCTION 'SVRS_GET_VERSION_DOMD_40'
      EXPORTING
        object_name = is_vrsd-objname
        versno      = is_vrsd-versno
      TABLES
        dd01v_tab   = lt_dd01v
        dd07v_tab   = lt_dd07v
        dd01tv_tab  = lt_dd01tv
        dd07tv_tab  = lt_dd07tv
      EXCEPTIONS
        no_version  = 01.

* todo, convert the dictionary structures into the abapGit XML representation

  ENDMETHOD.


  METHOD zif_abapgit_historical_object~build_files.

    DATA(lt_vrsd) = zcl_abapgit_historical_source=>read_versions(
      it_parts   = determine_parts( )
      iv_korrnum = iv_korrnum ).

    LOOP AT lt_vrsd INTO DATA(ls_vrsd).
      read_domd( ls_vrsd ).
    ENDLOOP.

* todo, serializing the domain into a file is not implemented yet

  ENDMETHOD.
ENDCLASS.
