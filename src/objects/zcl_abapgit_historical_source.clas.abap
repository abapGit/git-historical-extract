CLASS zcl_abapgit_historical_source DEFINITION PUBLIC.
  PUBLIC SECTION.

    CLASS-METHODS read_versions
      IMPORTING
        it_parts       TYPE zif_abapgit_historical_object=>ty_parts_tt
        iv_korrnum     TYPE vrsd-korrnum
      RETURNING
        VALUE(rt_vrsd) TYPE zif_abapgit_historical_object=>ty_vrsd_tt .

* the newest version of each part as of the release of the transport, for objects whose
* transports only carry the parts that changed
    CLASS-METHODS read_snapshot
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

  METHOD read_snapshot.

    TYPES:
      BEGIN OF ty_row,
        objtype TYPE vrsd-objtype,
        objname TYPE vrsd-objname,
        versno  TYPE vrsd-versno,
        korrnum TYPE vrsd-korrnum,
        author  TYPE vrsd-author,
        datum   TYPE vrsd-datum,
        zeit    TYPE vrsd-zeit,
        as4date TYPE e070-as4date,
        as4time TYPE e070-as4time,
      END OF ty_row.
    DATA lt_rows TYPE SORTED TABLE OF ty_row WITH NON-UNIQUE KEY objtype objname versno.

    IF lines( it_parts ) = 0.
      RETURN.
    ENDIF.

    SELECT SINGLE as4date, as4time FROM e070 INTO @DATA(ls_transport)
      WHERE trkorr = @iv_korrnum.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

* versions without a released request, like local ones, never reached the repository
    SELECT vrsd~objtype, vrsd~objname, vrsd~versno, vrsd~korrnum, vrsd~author, vrsd~datum, vrsd~zeit,
        e070~as4date, e070~as4time
      FROM vrsd INNER JOIN e070 ON e070~trkorr = vrsd~korrnum
      INTO TABLE @lt_rows
      FOR ALL ENTRIES IN @it_parts
      WHERE vrsd~objtype = @it_parts-objtype
      AND vrsd~objname = @it_parts-objname.

* same order as the extraction processes transports in, so later transports never leak in
    LOOP AT lt_rows INTO DATA(ls_row).
      IF ls_row-as4date > ls_transport-as4date
          OR ( ls_row-as4date = ls_transport-as4date AND ls_row-as4time > ls_transport-as4time )
          OR ( ls_row-as4date = ls_transport-as4date AND ls_row-as4time = ls_transport-as4time
            AND ls_row-korrnum > iv_korrnum ).
        CONTINUE.
      ENDIF.
* rows come in ascending version order, so the last row kept per part is its newest version
      READ TABLE rt_vrsd ASSIGNING FIELD-SYMBOL(<ls_vrsd>)
        WITH KEY objtype = ls_row-objtype objname = ls_row-objname.
      IF sy-subrc <> 0.
        APPEND INITIAL LINE TO rt_vrsd ASSIGNING <ls_vrsd>.
      ENDIF.
      <ls_vrsd> = CORRESPONDING #( ls_row ).
    ENDLOOP.

  ENDMETHOD.

ENDCLASS.
