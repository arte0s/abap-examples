*&---------------------------------------------------------------------*
*& Report ZTMP_SAO_GROUP_BY_HEADERS
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT ztmp_sao_group_by_headers.

*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
CLASS lcl_main DEFINITION.

  PUBLIC SECTION.

    CLASS-METHODS:
      meth1.

ENDCLASS.

*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
CLASS lcl_main IMPLEMENTATION.

  METHOD meth1.

*&---------------------------------------------------------------------*
*& TYPES
*&---------------------------------------------------------------------*
    TYPES: BEGIN OF ts_group_key,
             carrid   TYPE scarr-carrid,
             carrname TYPE scarr-carrname,
           END OF ts_group_key.

    TYPES: BEGIN OF ts_totals,
             fltime   TYPE spfli-fltime,
             distance TYPE spfli-distance,
           END OF ts_totals.

    TYPES: BEGIN OF ts_header.
             INCLUDE TYPE ts_group_key AS group_key.
             INCLUDE TYPE ts_totals AS totals.
           TYPES:
                  END OF ts_header.

*&---------------------------------------------------------------------*
*& POSITIONS TYPES
*&---------------------------------------------------------------------*
    TYPES: BEGIN OF ts_position,
             index    TYPE sy-index,
             carrid   TYPE spfli-carrid,
             connid   TYPE spfli-connid,
             fltime   TYPE spfli-fltime,
             distance TYPE spfli-distance,
           END OF ts_position.

    TYPES tt_position TYPE STANDARD TABLE OF ts_position WITH EMPTY KEY.

    TYPES tth_position TYPE HASHED TABLE OF ts_position WITH UNIQUE KEY carrid connid.

*&---------------------------------------------------------------------*
*& INPUT MERGE TYPES
*&---------------------------------------------------------------------*
    TYPES: BEGIN OF ts_merge,
             carrid   TYPE ts_header-carrid,
             carrname TYPE ts_header-carrname,
             connid   TYPE ts_position-connid,
             fltime   TYPE ts_position-fltime,
             distance TYPE ts_position-distance,
           END OF ts_merge.

    TYPES tth_merge TYPE HASHED TABLE OF ts_merge WITH UNIQUE KEY carrid connid.

*&---------------------------------------------------------------------*
*& OUTPUT GROUP TYPES
*&---------------------------------------------------------------------*
    TYPES: BEGIN OF ts_gheader.
             INCLUDE TYPE ts_header AS header.
           TYPES:
                    group_index TYPE int4,
                    group_size  TYPE int4,
                  END OF ts_gheader.

    TYPES: BEGIN OF ts_group.
             INCLUDE TYPE ts_gheader AS gheader.
           TYPES:
                    positions TYPE tth_position,
                  END OF ts_group.

    TYPES tth_group TYPE HASHED TABLE OF ts_group WITH UNIQUE KEY header-group_key.

*&---------------------------------------------------------------------*
*& GET DATA
*&---------------------------------------------------------------------*
    DATA(lt_merged) = VALUE tth_merge( ).

    SELECT
        a~carrid,
        a~carrname,
        f~connid,
        f~fltime,
        f~distance
      FROM spfli AS f
      INNER JOIN scarr AS a
        ON a~carrid = f~carrid
      INTO CORRESPONDING FIELDS OF TABLE @lt_merged.

*&---------------------------------------------------------------------*
*& GET GROUPS
*&---------------------------------------------------------------------*
    DATA(lt_group) = VALUE tth_group(

      FOR GROUPS ls_group OF ls_merged IN lt_merged INDEX INTO l_ind

        GROUP BY (
          group_key   = VALUE ts_group_key(
            carrid   = ls_merged-carrid
            carrname = ls_merged-carrname
          )
          group_index = GROUP INDEX
          group_size  = GROUP SIZE
        )

        ( VALUE #( "VALUE #( ... ) is needed here to use LET ... IN operator
              group_index = ls_group-group_index
              group_size  = ls_group-group_size
              group_key   = ls_group-group_key
              totals      = REDUCE #(
                INIT ls_totals TYPE ts_totals
                FOR ls_grp IN GROUP ls_group
                NEXT ls_totals = VALUE #(
                  fltime   = ls_totals-fltime + ls_grp-fltime
                  distance = ls_totals-distance + ls_grp-distance
                )
              )
              positions   = REDUCE #(
                INIT
                  lt_pos TYPE tt_position
***                  ls_totals TYPE ts_totals
                FOR ls_pos_merged IN GROUP ls_group
                NEXT
                  lt_pos = VALUE #( BASE lt_pos (
                    index    = COND #( WHEN lt_pos IS INITIAL THEN 1 ELSE lt_pos[ lines( lt_pos ) ]-index + 1 )
                    carrid   = ls_pos_merged-carrid
                    connid   = ls_pos_merged-connid
                    fltime   = ls_pos_merged-fltime
                    distance = ls_pos_merged-distance
                  )
                )
              )
        ) )
      ).

*&---------------------------------------------------------------------*
*& DISPLAY RESULTS
*&---------------------------------------------------------------------*
    DATA(lo_out) = cl_demo_output=>new( )->write( lt_merged ).

    LOOP AT lt_group INTO DATA(ls_g).

      lo_out->begin_section( ls_g-gheader-carrname
        )->write( ls_g-gheader
        )->write( ls_g-positions
        )->end_section( ).
    ENDLOOP.

    lo_out->display( ).

  ENDMETHOD.

ENDCLASS.

START-OF-SELECTION.

  lcl_main=>meth1( ).
