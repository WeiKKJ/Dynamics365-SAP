*----------------------------------------------------------------------*
***INCLUDE ZSDCRM001_PAI_0900.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0900  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0900 INPUT.
  save_ok = ok_code.
  CLEAR ok_code.
  CASE save_ok.
    WHEN 'BACK' OR 'EXIT' OR 'CANCEL' OR 'QX'.
      LEAVE TO SCREEN 0.
    WHEN 'DCLICK'.
      DATA:lv_name TYPE char20.
      "获取光标焦点
      GET CURSOR FIELD lv_name.
      CASE lv_name.
        WHEN 'GS_OUT-KUNNR'.
*          PERFORM frm_skip_bp .
        WHEN 'GS_OUT-VBELN'.
*          SET PARAMETER ID 'VL' FIELD gs_head-vbeln_vl .
*          CALL TRANSACTION 'VL03N' AND SKIP FIRST SCREEN.
        WHEN OTHERS.
      ENDCASE.
  ENDCASE.
ENDMODULE.
