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
  CLEAR:ok_code,lv_name.
  CASE save_ok.
    WHEN 'BACK' OR 'EXIT' OR 'CANCEL' OR 'QX'.
      PERFORM ezsdr IN PROGRAM saplzfg_crm USING 'X' gs_out-new_contractid CHANGING msg.
      LEAVE TO SCREEN 0.
*    WHEN 'ENTE'.
*      GET CURSOR FIELD lv_name.
    WHEN 'DCLICK'.
      GET CURSOR FIELD lv_name.
      ASSIGN (lv_name) TO FIELD-SYMBOL(<lv_name>).
      CHECK sy-subrc EQ 0.
      CASE lv_name.
        WHEN 'GS_OUT-KUNNR'.
*          PERFORM frm_skip_bp .
        WHEN 'GS_OUT-VBELN'.
          PERFORM va03 IN PROGRAM zpubform IF FOUND USING <lv_name>.
        WHEN OTHERS.
      ENDCASE.
      UNASSIGN <lv_name>.
    WHEN 'P'.
      CLEAR:gs_out-province_des,gs_out-city,gs_out-city_des,gs_out-county,gs_out-county_des.
      SELECT SINGLE zname FROM ztsd226 WHERE zid = @gs_out-province INTO @gs_out-province_des.
    WHEN 'CI'.
      CLEAR:gs_out-city_des,gs_out-county,gs_out-county_des.
      SELECT SINGLE zname FROM ztsd226 WHERE zid = @gs_out-city INTO @gs_out-city_des.
    WHEN 'CO'.
      CLEAR:gs_out-county_des.
      SELECT SINGLE zname FROM ztsd226 WHERE zid = @gs_out-county INTO @gs_out-county_des.
    WHEN 'SO'.
      PERFORM so.
    WHEN 'ZC'.
      PERFORM zc.
  ENDCASE.
ENDMODULE.
*&---------------------------------------------------------------------*
*& Form so
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM so .
  DATA:action    TYPE ze_so_action,
       rtype     TYPE bapi_mtype,
       rtmsg     TYPE bapi_msg,
       vbeln     TYPE vbeln,
       data      TYPE zscrm_so,
       gt_itemso TYPE zlcrm_so_item,
       gs_itemso LIKE LINE OF gt_itemso.
  DATA:gtcrmitems TYPE TABLE OF ztcrm_so_item,
       gscrmitems LIKE LINE OF gtcrmitems.
  CLEAR ret2.
  IF line_exists( gt_item[ matnr = '' ] ).
    MESSAGE '请填写物料号' TYPE 'E'.
  ENDIF.
  LOOP AT gt_item TRANSPORTING NO FIELDS WHERE action NE ''.
    EXIT.
  ENDLOOP.
  IF sy-subrc NE 0.
    MESSAGE '明细行均已处理完毕' TYPE 'E'.
  ENDIF.
  IF gs_out-vbeln IS INITIAL.
    action = 'I'.
  ELSE.
    action = 'U'.
  ENDIF.
  MOVE-CORRESPONDING gs_out TO data.
  LOOP AT gt_item INTO DATA(wa) WHERE action NE 'D'.
    MOVE-CORRESPONDING wa TO gs_itemso.
    APPEND gs_itemso TO gt_itemso.
  ENDLOOP.
  IF sy-subrc NE 0 AND  action = 'I'.
    DELETE FROM ztcrm_so_head WHERE new_contractid = @gs_out-new_contractid.
    DELETE FROM ztcrm_so_item WHERE new_contractid = @gs_out-new_contractid.
    COMMIT WORK.
    MESSAGE '处理成功' TYPE 'S'.
    PERFORM getdata.
    LEAVE TO SCREEN 0.
  ENDIF.
  data-items = gt_itemso.
  CALL FUNCTION 'ZFM_CRM_SO'
    EXPORTING
      action = action
    IMPORTING
      rtype  = rtype
      rtmsg  = rtmsg
      vbeln  = vbeln
    CHANGING
      data   = data.
  PERFORM ezsdr IN PROGRAM saplzfg_crm USING 'X' gs_out-new_contractid CHANGING msg.
  PERFORM inmsg(zpubform) TABLES ret2 USING '' rtype '' rtmsg(50) rtmsg+50(50) rtmsg+100(50) rtmsg+150(50).
  IF rtype = 'S'.
    gs_out-vbeln = vbeln.
    gs_out-state = '已处理'.
    MODIFY TABLE gt_out FROM gs_out.
    UPDATE ztcrm_so_head
    SET vbeln = @vbeln,province = @gs_out-province,city = @gs_out-city,county = @gs_out-county
    WHERE new_contractid = @gs_out-new_contractid.

    LOOP AT gt_item INTO wa.
      IF wa-action EQ 'D'.
        DELETE FROM ztcrm_so_item
        WHERE new_contractid = @gs_out-new_contractid AND new_contractdetailid = @wa-new_contractdetailid.
        DELETE TABLE gt_item FROM wa.
      ELSE.
        UPDATE ztcrm_so_item
        SET action = '',matnr = @wa-matnr,posnr = @wa-posnr
        WHERE new_contractid = @gs_out-new_contractid AND new_contractdetailid = @wa-new_contractdetailid.
      ENDIF.
      CLEAR wa-action.
      PERFORM set_style USING wa-matnr CHANGING wa.
      MODIFY gt_item FROM wa.
    ENDLOOP.
    COMMIT WORK.
    PERFORM crm_so_sync CHANGING rtype rtmsg.
    PERFORM inmsg(zpubform) TABLES ret2 USING '' rtype '' rtmsg(50) rtmsg+50(50) rtmsg+100(50) rtmsg+150(50).
  ENDIF.
  PERFORM showmsg(zpubform) TABLES ret2.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form crm_so_sync
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- RTYPE
*&      <-- RTMSG
*&---------------------------------------------------------------------*
FORM crm_so_sync  CHANGING p_rtype
                           p_rtmsg.
  DATA:data         TYPE zscrm_sosync,
       gt_main_data TYPE zlcrm_somaindata,
       gs_main_data LIKE LINE OF gt_main_data,
       gt_details   TYPE zlcrm_sodetails,
       gs_details   LIKE LINE OF gt_details.
  CLEAR:p_rtype,p_rtmsg.
  data-func = 'ContractInfoSync'.
  CLEAR:gs_main_data,gt_main_data.
  gs_main_data-contractid = gs_out-new_contractid.
  gs_main_data-code = gs_out-vbeln.
  CLEAR:gt_details.
  LOOP AT gt_item INTO DATA(item).
    CLEAR:gs_details.
    gs_details-contractdetailid = item-new_contractdetailid.
    gs_details-code = item-posnr.
    gs_details-product = item-matnr.
    APPEND gs_details TO gt_details.
  ENDLOOP.
  gs_main_data-details = gt_details.
  APPEND gs_main_data TO gt_main_data.
  data-main_data = gt_main_data.

  CALL FUNCTION 'ZFM_CRM_SO_SYNC'
    EXPORTING
      data  = data
    IMPORTING
      rtype = p_rtype
      rtmsg = p_rtmsg
*     CRM_RETURN       =
*     INSTR =
*     OUTSTR           =
*     STATUS           =
*     MSG   =
    .

ENDFORM.
*&---------------------------------------------------------------------*
*& Form zc
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM zc .
  DATA:wa_head TYPE ztcrm_so_head,
       lt_item TYPE TABLE OF ztcrm_so_item.
  MOVE-CORRESPONDING gs_out TO wa_head.
  LOOP AT gt_item INTO gs_item.
    INSERT INITIAL LINE INTO TABLE lt_item ASSIGNING FIELD-SYMBOL(<lt_item>).
    MOVE-CORRESPONDING gs_item TO <lt_item>.
  ENDLOOP.
  IF sy-subrc EQ 0.
    MODIFY ztcrm_so_head FROM wa_head.
    MODIFY ztcrm_so_item FROM TABLE lt_item.
    COMMIT WORK.
    MESSAGE '暂存成功' TYPE 'S'.
  ENDIF.
ENDFORM.
