*&---------------------------------------------------------------------*
*& Include ZSDCRM001_SUB_PAI
*&---------------------------------------------------------------------*

*&SPWIZARD: INPUT MODULE FOR TS 'SO'. DO NOT CHANGE THIS LINE!
*&SPWIZARD: GETS ACTIVE TAB
MODULE SO_ACTIVE_TAB_GET INPUT.
  OK_CODE = SY-UCOMM.
  CASE OK_CODE.
    WHEN C_SO-TAB1.
      G_SO-PRESSED_TAB = C_SO-TAB1.
    WHEN C_SO-TAB2.
      G_SO-PRESSED_TAB = C_SO-TAB2.
    WHEN C_SO-TAB3.
      G_SO-PRESSED_TAB = C_SO-TAB3.
    WHEN C_SO-TAB4.
      G_SO-PRESSED_TAB = C_SO-TAB4.
    WHEN C_SO-TAB5.
      G_SO-PRESSED_TAB = C_SO-TAB5.
    WHEN OTHERS.
*&SPWIZARD:      DO NOTHING
  ENDCASE.
ENDMODULE.
