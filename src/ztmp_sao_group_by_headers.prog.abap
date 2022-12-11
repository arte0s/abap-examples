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

    TYPES: BEGIN OF ts_header.
             INCLUDE TYPE ts_group_key AS group_key.
           TYPES:
                    fltime   TYPE spfli-fltime,
                    distance TYPE spfli-distance,
                  END OF ts_header.

    TYPES: BEGIN OF ts_position,
             index    TYPE sy-index,
             carrid   TYPE spfli-carrid,
             connid   TYPE spfli-connid,
             fltime   TYPE spfli-fltime,
             distance TYPE spfli-distance,
           END OF ts_position.

    TYPES tts_position TYPE STANDARD TABLE OF ts_position WITH EMPTY KEY.

    TYPES tth_position TYPE HASHED TABLE OF ts_position WITH UNIQUE KEY carrid connid.

    TYPES: BEGIN OF ts_grp,
             group_index TYPE int4,
             group_size  TYPE int4.
             INCLUDE TYPE ts_header AS header.
           TYPES:
                  END OF ts_grp.

    TYPES: BEGIN OF ts_group.
             INCLUDE TYPE ts_grp AS group.
           TYPES:
                    positions TYPE tth_position,
                  END OF ts_group.

    TYPES: BEGIN OF ts_merge,
             carrid   TYPE ts_header-carrid,
             carrname TYPE ts_header-carrname,
             connid   TYPE ts_position-connid,
             fltime   TYPE ts_position-fltime,
             distance TYPE ts_position-distance,
           END OF ts_merge.

    TYPES tts_merge TYPE STANDARD TABLE OF ts_merge WITH EMPTY KEY.

    TYPES tth_merge TYPE HASHED TABLE OF ts_merge WITH UNIQUE KEY carrid connid.

    TYPES tts_group TYPE STANDARD TABLE OF ts_group WITH EMPTY KEY.

    TYPES tth_group TYPE HASHED TABLE OF ts_group WITH UNIQUE KEY carrid.

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
    DATA(lt_group) = VALUE tts_group(

      FOR GROUPS ls_group OF ls_merged IN lt_merged INDEX INTO l_ind

        GROUP BY (
          group_index = GROUP INDEX
          group_size  = GROUP SIZE
          group_key   = VALUE ts_group_key(
            carrid   = ls_merged-carrid
            carrname = ls_merged-carrname
          )
        )

        ( VALUE #( "VALUE #( ... ) is needed here to use LET ... IN operator

            LET
              l_total_time = REDUCE ts_position-fltime(
                INIT l_time TYPE ts_position-fltime
                FOR ls_time_merged IN GROUP ls_group
                NEXT l_time = l_time + ls_time_merged-fltime
              )

              l_total_distance = REDUCE ts_position-fltime(
                INIT l_distance TYPE ts_position-fltime
                FOR ls_dist_merged IN GROUP ls_group
                NEXT l_distance = l_distance + ls_dist_merged-distance
              )

            IN
              group_index = ls_group-group_index
              group_size  = ls_group-group_size
              group_key   = ls_group-group_key
              fltime      = l_total_time
              distance    = l_total_distance
***              positions   = VALUE #( FOR ls_pos_merged IN GROUP ls_group INDEX INTO l_pos_index "INDEX INTO doesn't work!
              positions   = REDUCE #(
                INIT lt_pos TYPE tts_position
                FOR ls_pos_merged IN GROUP ls_group
                NEXT lt_pos = VALUE #( BASE lt_pos
                  (
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

      lo_out->begin_section( ls_g-group-carrname
        )->write( ls_g-group
        )->write( ls_g-positions
        )->end_section( ).
    ENDLOOP.

    lo_out->display( ).

  ENDMETHOD.

ENDCLASS.

START-OF-SELECTION.

  lcl_main=>meth1( ).