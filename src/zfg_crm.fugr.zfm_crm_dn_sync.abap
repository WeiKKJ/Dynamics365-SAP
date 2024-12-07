FUNCTION zfm_crm_dn_sync .
*"----------------------------------------------------------------------
*"*"本地接口：
*"  IMPORTING
*"     VALUE(TCODE) TYPE  SY-TCODE OPTIONAL
*"     VALUE(IT_XLIKP) TYPE  SHP_LIKP_T OPTIONAL
*"     VALUE(IT_YLIKP) TYPE  SHP_YLIKP_T OPTIONAL
*"     VALUE(TRTYP) TYPE  T180-TRTYP OPTIONAL
*"  EXPORTING
*"     VALUE(RTYPE) TYPE  BAPI_MTYPE
*"     VALUE(RTMSG) TYPE  BAPI_MSG
*"     VALUE(CRM_RETURN) TYPE  ZSCRM_RETURN
*"     VALUE(INSTR) TYPE  STRING
*"     VALUE(OUTSTR) TYPE  STRING
*"     VALUE(STATUS) TYPE  I
*"     VALUE(MSG) TYPE  STRING
*"     VALUE(SECDS) TYPE  ZILOGSECDS
*"  CHANGING
*"     VALUE(DATA) TYPE  ZSCRM_DN OPTIONAL
*"----------------------------------------------------------------------
  zfmdatasave1 'ZFM_CRM_DN_SYNC'.
  zfmdatasave2 'B'.
  COMMIT WORK.
  DATA:t1 TYPE i,
       t2 TYPE i.
  CLEAR:rtype,rtmsg,crm_return,instr,outstr,crmurl,crmappId,header,data.
  IF it_xlikp IS INITIAL.
    fillmsg 'E' '请传入[IT_XLIKP]数据后再调用本接口'.
  ENDIF.
  PERFORM getdata(zpub_data) USING header_gd-name CHANGING crmurl."配置化
  IF crmurl IS INITIAL.
    rtmsg = |请先在ZMM000维护CRM销售合同关闭接口的键[{ header_gd-name }]的值：CRM接口地址|.
    fillmsg 'E' rtmsg.
  ENDIF.
  PERFORM getdata(zpub_data) USING 'CRMAPPID' CHANGING crmappid."配置化
  IF crmappid IS INITIAL.
    rtmsg = |请先在ZMM000维护CRM授权许可的Key键[CRMAPPID]的值：授权许可的Key|.
    fillmsg 'E' rtmsg.
  ENDIF.

  INSERT INITIAL LINE INTO TABLE header ASSIGNING FIELD-SYMBOL(<header>).
  <header>-name    = 'appId' .
  <header>-value   = crmappid .
  <header>-cdata   = '' .
  <header>-xdata   = '' .
*  READ TABLE it_xlikp INTO DATA(v_xlikp) INDEX 1.
  " 交货单明细  14.09.2024 11:46:43 by kkw
  GET RUN TIME FIELD t1.
  DO.
    SELECT
      likp~vbeln,
      likp~bldat,
      likp~wadat_ist,
      likp~kunnr,
      likp~wbstk,
      lips~posnr,
      lips~matnr,
      lips~lfimg,
      lips~vrkme
      FROM likp
      JOIN lips ON likp~vbeln = lips~vbeln
      FOR ALL ENTRIES IN @it_xlikp
      WHERE likp~vbeln = @it_xlikp-vbeln
      AND lips~matnr <> 'M0518012864'
      INTO TABLE @DATA(lt_dn)
      .
    IF sy-subrc EQ 0.
      EXIT.
      SORT lt_dn BY vbeln posnr.
    ENDIF.
    GET RUN TIME FIELD t2.
    secds = ( t2 - t1 ) / 1000000.
    IF secds GT 5.
      rtmsg = |等待时间{ secds }秒未取到交货单数据|.
      fillmsg 'E' rtmsg.
    ENDIF.
  ENDDO.
  " 找交货单对应的销售合同  14.09.2024 11:46:55 by kkw
  IF lt_dn IS NOT INITIAL.
    SELECT DISTINCT
      vbfa~vbeln,
      vbfa~posnn AS posnr,
*& kkw 31571 子 SAP-CRM对接 / 3.5.2-3.2.2 交货单创建&更新&删除-20241121新加逻辑：
      CASE WHEN vbak~new_contractid IS INITIAL THEN ztcrm_so_init~new_contractid ELSE vbak~new_contractid END AS new_contractid,
      CASE WHEN vbak~new_contractid IS INITIAL THEN ztcrm_so_init~new_contractdetailid ELSE vbap~new_contractdetailid END AS new_contractdetailid,
*& End  31571 子 SAP-CRM对接 / 3.5.2-3.2.2 交货单创建&更新&删除-20241121新加逻辑： 21.11.2024 14:36:53
      vbfa~vbtyp_v
      FROM vbfa
      JOIN vbap ON vbfa~vbelv = vbap~vbeln AND vbfa~posnv = vbap~posnr
      JOIN vbak ON vbap~vbeln = vbak~vbeln
      JOIN @lt_dn AS lt_dn ON vbfa~vbeln = lt_dn~vbeln AND vbfa~posnn = lt_dn~posnr
      LEFT JOIN ztcrm_so_init ON vbap~vbeln = ztcrm_so_init~vbeln AND vbap~posnr = ztcrm_so_init~posnr
      WHERE vbfa~vbtyp_v IN ( 'G','H' )
      INTO TABLE @DATA(lt_vbfa)
      .
    SORT lt_vbfa BY vbtyp_v vbeln posnr.
  ENDIF.

  data-func = 'DeliverySync'.
  LOOP AT lt_dn ASSIGNING FIELD-SYMBOL(<lt_dn>) GROUP BY ( vbeln = <lt_dn>-vbeln bldat = <lt_dn>-bldat
    wadat_ist = <lt_dn>-wadat_ist kunnr = <lt_dn>-kunnr wbstk = <lt_dn>-wbstk
    index = GROUP INDEX size = GROUP SIZE
     ) ASSIGNING FIELD-SYMBOL(<group>).
    READ TABLE lt_vbfa ASSIGNING FIELD-SYMBOL(<lt_vbfav>) WITH KEY vbtyp_v = 'G' vbeln = <group>-vbeln BINARY SEARCH.
    IF sy-subrc EQ 0.
      IF <lt_vbfav>-new_contractdetailid IS INITIAL.
        CONTINUE.
      ENDIF.
    ELSE.
      CONTINUE.
    ENDIF.

    INSERT INITIAL LINE INTO TABLE data-main_data ASSIGNING FIELD-SYMBOL(<main_data>).
    <main_data>-name           = <lt_vbfav>-vbeln .
    <main_data>-date           = <group>-bldat .
    <main_data>-deliverytime   = <group>-wadat_ist .
*    IF tcode = 'VL09' AND ( <group>-wadat_ist IS INITIAL OR <group>-wadat_ist = '00000000' ).
    IF ( <group>-wadat_ist IS INITIAL OR <group>-wadat_ist = '00000000' ).
*      SELECT SINGLE budat
*       FROM mkpf
*        WHERE mkpf~le_vbeln = @<lt_vbfav>-vbeln
*        AND mkpf~tcode2 = @sy-tcode
*        INTO @<main_data>-deliverytime.
      <main_data>-deliverytime           = <group>-bldat .
    ENDIF.
    CASE <group>-wbstk .
      WHEN 'A'.
        <main_data>-deliverystatus = 1 .
      WHEN 'B'.
        <main_data>-deliverystatus = 2 .
      WHEN 'C'.
        <main_data>-deliverystatus = 3 .
    ENDCASE.
*    <main_data>-accountid      = <group>-kunnr .
*    SELECT SINGLE
*      b~smtp_addr
*      FROM but021_fs AS a
*      JOIN adr6 AS b
*      ON a~addrnumber = b~addrnumber
*      WHERE a~partner = @<group>-kunnr
*      INTO @DATA(smtp_addr)
*      .
*    IF cl_abap_matcher=>matches( pattern = `^[a-zA-Z0-9._%+-]+@crm\.com$` text = to_lower( smtp_addr ) ) = abap_true.
*      DATA(len) = strlen( smtp_addr ) - 8.
*      <main_data>-accountid      = smtp_addr(len) .
*    ELSE.
*      <main_data>-accountid      = <group>-kunnr.
*    ENDIF.
    <main_data>-accountid      = <group>-kunnr.
    LOOP AT GROUP <group> ASSIGNING FIELD-SYMBOL(<mem>).
      READ TABLE lt_vbfa ASSIGNING FIELD-SYMBOL(<lt_vbfa>) WITH KEY vbtyp_v = 'G' vbeln = <mem>-vbeln posnr = <mem>-posnr BINARY SEARCH.
      IF sy-subrc EQ 0.
        IF <lt_vbfa>-new_contractdetailid IS INITIAL.
*          rtmsg = |发货单[{ <group>-vbeln }]，行[{ <mem>-posnr }]不是经由CRM创建，状态变更未同步CRM|.
          CONTINUE.
        ENDIF.
      ELSE.
        CONTINUE.
      ENDIF.
      INSERT INITIAL LINE INTO TABLE <main_data>-deliverydetail ASSIGNING FIELD-SYMBOL(<deliverydetail>).
      <deliverydetail>-name               = <mem>-posnr .
      <deliverydetail>-productid          = <mem>-matnr .
      READ TABLE lt_vbfa ASSIGNING FIELD-SYMBOL(<lt_vbfh>) WITH KEY vbtyp_v = 'H' vbeln = <mem>-vbeln posnr = <mem>-posnr BINARY SEARCH.
      IF sy-subrc EQ 0.
        <deliverydetail>-quantity           = <mem>-lfimg * -1.
      ELSE.
        <deliverydetail>-quantity           = <mem>-lfimg .
      ENDIF.
      <deliverydetail>-vrkme              = <mem>-vrkme .
      <main_data>-contractid     = <lt_vbfa>-new_contractid.
      <deliverydetail>-contractdetailid   = <lt_vbfa>-new_contractdetailid .
    ENDLOOP.
  ENDLOOP.
  IF data-main_data IS INITIAL.
    rtmsg = '查无数据'.
    fillmsg 'E' rtmsg.
  ENDIF.
  instr = /ui2/cl_json=>serialize( data = data compress = abap_false pretty_name = /ui2/cl_json=>pretty_mode-camel_case ).
  CALL METHOD zcl_dingtalk=>create_http_client
    EXPORTING
      input     = instr
      url       = crmurl
*     username  =
*     password  =
      reqmethod = 'POST'
*     http1_1   = ABAP_TRUE
*     proxy     =
*     bodytype  = 'JSON'
      header    = header
    IMPORTING
      output    = outstr
      rtmsg     = msg
      status    = status.
  /ui2/cl_json=>deserialize( EXPORTING json = outstr  pretty_name = /ui2/cl_json=>pretty_mode-camel_case CHANGING data = crm_return ).
  IF crm_return-_is_success = abap_true.
    rtype = 'S'.
    rtmsg = '同步CRM成功'.
  ELSE.
    rtype = 'E'.
    rtmsg = |同步CRM失败，状态码：[{ status }]，http reason：[{ msg }]，CRM返回：[{ crm_return-_error_message }]|.
  ENDIF.


  zfmdatasave2 'R'.




ENDFUNCTION.
