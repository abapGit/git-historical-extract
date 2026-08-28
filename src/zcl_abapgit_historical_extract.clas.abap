CLASS zcl_abapgit_historical_extract DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES ty_trkorr_range TYPE RANGE OF e070-trkorr .
    TYPES ty_object_range TYPE RANGE OF tadir-object .

    METHODS run
      IMPORTING
        it_transports TYPE ty_trkorr_range
        it_object     TYPE ty_object_range
        iv_url        TYPE string
        iv_branch     TYPE string
        iv_skip_git   TYPE abap_bool
      RAISING
        zcx_abapgit_exception .
  PROTECTED SECTION.

  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_ABAPGIT_HISTORICAL_EXTRACT IMPLEMENTATION.


  METHOD run.

    TYPES:
      BEGIN OF ty_transport,
        trkorr TYPE e070-trkorr,
      END OF ty_transport,
      BEGIN OF ty_deleted_object,
        request  TYPE e070-strkorr,
        object   TYPE e071-object,
        obj_name TYPE e071-obj_name,
      END OF ty_deleted_object.

    DATA lt_files           TYPE zif_abapgit_historical_extract=>ty_files_tt.
    DATA lt_trkorr          TYPE STANDARD TABLE OF ty_transport WITH EMPTY KEY.
    DATA lt_deleted_objects TYPE SORTED TABLE OF ty_deleted_object
      WITH UNIQUE KEY request object obj_name.
    DATA lt_deleted_files   TYPE zif_abapgit_historical_extract=>ty_files_tt.

    SELECT trkorr FROM e070
      INTO TABLE @lt_trkorr
      WHERE trkorr IN @it_transports
      AND trstatus = @zif_abapgit_cts_api=>c_transport_status-released
      AND trfunction = @zif_abapgit_cts_api=>c_transport_type-wb_request
      AND strkorr = ''
      ORDER BY PRIMARY KEY.

    SELECT e071~trkorr, e071~object, e071~obj_name
      FROM e071
      INTO TABLE @lt_deleted_objects
      WHERE e071~trkorr IN @it_transports
        AND e071~pgmid = 'R3TR'
        AND e071~object IN @it_object
        AND e071~objfunc = 'D'.

    LOOP AT lt_trkorr INTO DATA(ls_trkorr).
      IF sy-tabix MOD 10 = 0.
        cl_progress_indicator=>progress_indicate(
          i_text               = |Processing transport { ls_trkorr-trkorr }, { sy-tabix }/{ lines( lt_trkorr ) }|
          i_processed          = sy-tabix
          i_total              = lines( lt_trkorr )
          i_output_immediately = abap_true ).
      ENDIF.

      DATA(lt_list) = zcl_abapgit_factory=>get_cts_api( )->list_r3tr_by_request( ls_trkorr-trkorr ).

      CLEAR lt_files.
      LOOP AT lt_list INTO DATA(ls_list) WHERE object IN it_object.
        IF line_exists( lt_deleted_objects[
              request  = ls_trkorr-trkorr
              object   = ls_list-object
              obj_name = ls_list-obj_name ] ).
          CONTINUE.
        ENDIF.

        DATA(lt_obj_files) = zcl_abapgit_historical_objects=>read(
          iv_objtype = ls_list-object
          iv_objname = CONV #( ls_list-obj_name )
          iv_korrnum = ls_trkorr-trkorr ).
        INSERT LINES OF lt_obj_files INTO TABLE lt_files.
      ENDLOOP.

      LOOP AT lt_deleted_objects INTO DATA(ls_deleted_object)
          WHERE request = ls_trkorr-trkorr.
        lt_deleted_files = zcl_abapgit_historical_objects=>read_deleted(
          iv_objtype = ls_deleted_object-object
          iv_objname = ls_deleted_object-obj_name(110) ).
        LOOP AT lt_deleted_files INTO DATA(ls_deleted_file).
          IF NOT line_exists( lt_files[ filename = ls_deleted_file-filename ] ).
            APPEND ls_deleted_file TO lt_files.
          ENDIF.
        ENDLOOP.
      ENDLOOP.

      IF iv_skip_git = abap_false.
        zcl_abapgit_historical_git=>push(
          iv_trkorr = ls_trkorr-trkorr
          it_files  = lt_files
          iv_url    = iv_url
          iv_branch = iv_branch ).
      ENDIF.
    ENDLOOP.

  ENDMETHOD.
ENDCLASS.
