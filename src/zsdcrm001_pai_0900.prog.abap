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
  DATA:BEGIN OF w_del,
         vbeln_ht TYPE vbeln,
         posnr_ht TYPE posnr,
         menge_fh TYPE vbap-kwmeng,
         menge_th TYPE vbap-kwmeng,
         menge    TYPE vbap-kwmeng,
         kzwi1_bc TYPE vbap-kzwi1,
       END OF w_del,
       t_del LIKE TABLE OF w_del.
  DATA:answer   TYPE c,
       t_spopli TYPE TABLE OF spopli.
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
    "根据合同查发货
    SELECT
      vbfa~vbelv AS vbeln_ht,
      vbfa~posnv AS posnr_ht,
      vbfa~vbeln AS vbeln_fh,
      vbfa~posnn AS posnr_fh,
      vbap~kwmeng AS menge_fh
      FROM @gt_item AS i
      JOIN vbfa ON i~posnr = vbfa~posnv
      JOIN vbap ON vbfa~vbeln = vbap~vbeln AND vbfa~posnn = vbap~posnr
      WHERE vbfa~vbelv = @gs_out-vbeln
      AND vbfa~vbtyp_n = 'C'
      AND i~action EQ 'D'
      ORDER BY vbfa~vbelv,vbfa~posnv
      INTO TABLE @DATA(t_fh)
      .
    "根据发货查退货
    SELECT
      fh~vbeln_ht,
      fh~posnr_ht,
*    vbfa~vbeln AS vbeln_th,
*    vbfa~posnn AS posnr_th,
      SUM( vbap~kwmeng ) AS menge_th
      FROM @t_fh AS fh
      JOIN vbfa ON fh~vbeln_fh = vbfa~vbelv AND posnr_fh = vbfa~posnv
      JOIN vbap ON vbfa~vbeln = vbap~vbeln AND vbfa~posnn = vbap~posnr
      WHERE vbfa~vbtyp_n = 'H'
      GROUP BY fh~vbeln_ht,fh~posnr_ht
      ORDER BY fh~vbeln_ht,fh~posnr_ht
      INTO TABLE @DATA(t_th)
      .
    "根据发货查补差
    SELECT
      fh~vbeln_ht,
      fh~posnr_ht,
*    vbfa~vbeln AS vbeln_bc,
*    vbfa~posnn AS posnr_bc,
      SUM( vbap~kzwi1 ) AS kzwi1_bc
      FROM @t_fh AS fh
      JOIN vbfa ON fh~vbeln_fh = vbfa~vbelv AND posnr_fh = vbfa~posnv
      JOIN vbap ON vbfa~vbeln = vbap~vbeln AND vbfa~posnn = vbap~posnr
      WHERE vbfa~vbtyp_n IN ('K','L')
      GROUP BY fh~vbeln_ht,fh~posnr_ht
      ORDER BY fh~vbeln_ht,fh~posnr_ht
      INTO TABLE @DATA(t_bc)
      .
    LOOP AT t_fh ASSIGNING FIELD-SYMBOL(<t_fh>) GROUP BY ( vbeln_ht = <t_fh>-vbeln_ht  posnr_ht = <t_fh>-posnr_ht
      index = GROUP INDEX size = GROUP SIZE
      ) ASSIGNING FIELD-SYMBOL(<group>).
      APPEND INITIAL LINE TO t_del ASSIGNING FIELD-SYMBOL(<t_del>).
      <t_del>-vbeln_ht = <group>-vbeln_ht.
      <t_del>-posnr_ht = <group>-posnr_ht.
      LOOP AT GROUP <group> ASSIGNING FIELD-SYMBOL(<mem>).
        <t_del>-menge_fh += <mem>-menge_fh.
      ENDLOOP.
      READ TABLE t_th ASSIGNING FIELD-SYMBOL(<t_th>) WITH KEY vbeln_ht = <group>-vbeln_ht posnr_ht = <group>-posnr_ht BINARY SEARCH.
      IF sy-subrc EQ 0.
        <t_del>-menge_th = <t_th>-menge_th.
      ENDIF.
      READ TABLE t_bc ASSIGNING FIELD-SYMBOL(<t_bc>) WITH KEY vbeln_ht = <group>-vbeln_ht posnr_ht = <group>-posnr_ht BINARY SEARCH.
      IF sy-subrc EQ 0.
        <t_del>-kzwi1_bc = <t_bc>-kzwi1_bc.
      ENDIF.
      <t_del>-menge = <t_del>-menge_fh - <t_del>-menge_th.
    ENDLOOP.
    LOOP AT t_del ASSIGNING <t_del>.
      APPEND INITIAL LINE TO t_spopli ASSIGNING FIELD-SYMBOL(<t_spopli>).
      IF <t_del>-kzwi1_bc NE 0 OR <t_del>-menge NE 0.
        <t_spopli>-varoption = |{ <t_del>-posnr_ht }行无法打作废标志，已发货量{ <t_del>-menge_fh } 已退货量{ <t_del>-menge_th } 已补差量{ <t_del>-kzwi1_bc }|.
        <t_spopli>-selflag   = ''    .
        <t_spopli>-inactive  = 'X'    .
      ELSE.
        <t_spopli>-varoption = |{ <t_del>-posnr_ht }行法直接删除，已发货量{ <t_del>-menge_fh } 已退货量{ <t_del>-menge_th } 已补差量{ <t_del>-kzwi1_bc }是否进行作废标志|.
        <t_spopli>-selflag   = 'X'    .
        <t_spopli>-inactive  = ''    .
      ENDIF.
    ENDLOOP.
    IF t_spopli IS NOT INITIAL.
      CLEAR answer.
      CALL FUNCTION 'POPUP_TO_DECIDE_LIST'
        EXPORTING
*         CURSORLINE         = 1
          mark_flag          = 'X'
          mark_max           = lines( t_spopli )
*         START_COL          = 0
*         START_ROW          = 0
          textline1          = '以下合同无法直接删除，请选择要打作废标志的行明细'(t01)
*         TEXTLINE2          = ' '
*         TEXTLINE3          = ' '
          titel              = '请确认'
*         DISPLAY_ONLY       = ' '
        IMPORTING
          answer             = answer
        TABLES
          t_spopli           = t_spopli
        EXCEPTIONS
          not_enough_answers = 1
          too_much_answers   = 2
          too_much_marks     = 3
          OTHERS             = 4.
      IF sy-subrc EQ 0 AND answer NE 'A'.
        LOOP AT t_spopli ASSIGNING <t_spopli> WHERE selflag   = 'X'.
          READ TABLE gt_item ASSIGNING FIELD-SYMBOL(<ww>) WITH KEY posnr = <t_spopli>-varoption(6).
          IF sy-subrc EQ 0.
            <ww>-zhtmxzf = 'X'.
            UPDATE ztcrm_so_item SET zhtmxzf = 'X',action = '' WHERE new_contractid = @gs_out-new_contractid AND new_contractdetailid = @<ww>-new_contractdetailid.
          ENDIF.
          UPDATE vbap SET zhtmxzf = 'X' WHERE vbeln = gs_out-vbeln AND posnr = <t_spopli>-varoption(6).
        ENDLOOP.
        IF sy-subrc EQ 0.
          COMMIT WORK AND WAIT.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.
  MOVE-CORRESPONDING gs_out TO data.
  LOOP AT gt_item INTO DATA(wa) WHERE action NE 'D'.
    MOVE-CORRESPONDING wa TO gs_itemso.
    APPEND gs_itemso TO gt_itemso.
  ENDLOOP.
  IF sy-subrc NE 0 .
    IF action = 'I'.
      DELETE FROM ztcrm_so_head WHERE new_contractid = @gs_out-new_contractid.
      DELETE FROM ztcrm_so_item WHERE new_contractid = @gs_out-new_contractid.
      COMMIT WORK.
      MESSAGE '处理成功' TYPE 'S'.
    ELSE.
      LOOP AT gt_item TRANSPORTING NO FIELDS WHERE zhtmxzf = ''.
        EXIT.
      ENDLOOP.
      IF sy-subrc EQ 0.
        action = 'D'.
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
        IF rtype = 'S'.
          DELETE FROM ztcrm_so_head WHERE new_contractid = @gs_out-new_contractid.
          DELETE FROM ztcrm_so_item WHERE new_contractid = @gs_out-new_contractid.
          COMMIT WORK.
        ENDIF.
        MESSAGE rtmsg TYPE rtype.
      ELSE.
        MESSAGE '处理成功' TYPE 'S'.
      ENDIF.
    ENDIF.
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
