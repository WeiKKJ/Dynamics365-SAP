*&---------------------------------------------------------------------*
*& Include ZSDCRM001_SUB_DATA
*&---------------------------------------------------------------------*

*&SPWIZARD: FUNCTION CODES FOR TABSTRIP 'SO'
CONSTANTS: BEGIN OF C_SO,
             TAB1 LIKE SY-UCOMM VALUE 'SO_FC1',
             TAB2 LIKE SY-UCOMM VALUE 'SO_FC2',
             TAB3 LIKE SY-UCOMM VALUE 'SO_FC3',
             TAB4 LIKE SY-UCOMM VALUE 'SO_FC4',
             TAB5 LIKE SY-UCOMM VALUE 'SO_FC5',
           END OF C_SO.
*&SPWIZARD: DATA FOR TABSTRIP 'SO'
CONTROLS:  SO TYPE TABSTRIP.
DATA:      BEGIN OF G_SO,
             SUBSCREEN   LIKE SY-DYNNR,
             PROG        LIKE SY-REPID VALUE 'ZSDCRM001',
             PRESSED_TAB LIKE SY-UCOMM VALUE C_SO-TAB1,
           END OF G_SO.
