CLASS zcl_abapgit_historical_source DEFINITION PUBLIC.
  PUBLIC SECTION.

    CLASS-METHODS read_versions
      IMPORTING
        it_parts       TYPE zif_abapgit_historical_object=>ty_parts_tt
        iv_korrnum     TYPE vrsd-korrnum
      RETURNING
        VALUE(rt_vrsd) TYPE zif_abapgit_historical_object=>ty_vrsd_tt .

    CLASS-METHODS read_reps
      IMPORTING
        is_vrsd          TYPE zif_abapgit_historical_object=>ty_vrsd
      RETURNING
        VALUE(rv_source) TYPE string .

  PROTECTED SECTION.
ENDCLASS.

CLASS zcl_abapgit_historical_source IMPLEMENTATION.
  METHOD read_reps.

    DATA lt_repos TYPE STANDARD TABLE OF abaptxt255 WITH EMPTY KEY.
    DATA lt_trdir TYPE STANDARD TABLE OF trdir WITH EMPTY KEY.

* note that this function module returns the full 255 character width source code
* plus works for multiple object types
    CALL FUNCTION 'SVRS_GET_REPS_FROM_OBJECT'
      EXPORTING
        object_name = is_vrsd-objname
        object_type = is_vrsd-objtype
        versno      = is_vrsd-versno
      TABLES
        repos_tab   = lt_repos
        trdir_tab   = lt_trdir
      EXCEPTIONS
        no_version  = 1
        OTHERS      = 2.
    IF sy-subrc = 0.
      rv_source = concat_lines_of( table = lt_repos
                                   sep   = |\n| ).
    ENDIF.

  ENDMETHOD.

  METHOD read_versions.

    IF lines( it_parts ) = 0.
      RETURN.
    ENDIF.

    SELECT objtype, objname, versno, korrnum, author, datum, zeit
      FROM vrsd INTO CORRESPONDING FIELDS OF TABLE @rt_vrsd
      FOR ALL ENTRIES IN @it_parts
      WHERE objtype = @it_parts-objtype
      AND objname = @it_parts-objname
      AND korrnum = @iv_korrnum
      ORDER BY PRIMARY KEY.

  ENDMETHOD.

ENDCLASS.
