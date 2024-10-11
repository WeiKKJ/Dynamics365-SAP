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
  READ TABLE it_xlikp INTO DATA(v_xlikp) INDEX 1.
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
*        LEFT JOIN vbuk ON likp~vbeln = vbuk~vbeln
      WHERE likp~vbeln = @v_xlikp-vbeln
      ORDER BY likp~vbeln,lips~posnr
      INTO TABLE @DATA(lt_dn)
      .
    IF sy-subrc EQ 0.
      EXIT.
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
    SELECT
      vbfa~vbeln,
      vbfa~posnn AS posnr,
      vbak~new_contractid,
      vbap~new_contractdetailid
      FROM vbfa
      JOIN vbak ON vbfa~vbelv = vbak~vbeln
      JOIN vbap ON vbak~vbeln = vbap~vbeln
      FOR ALL ENTRIES IN @lt_dn
      WHERE vbfa~vbeln = @lt_dn-vbeln
      AND vbfa~posnn = @lt_dn-posnr
      AND vbfa~vbtyp_v = 'G'
      INTO TABLE @DATA(lt_vbfa)
      .
    SORT lt_vbfa BY vbeln posnr.
  ENDIF.

  data-func = 'DeliverySync'.
  LOOP AT lt_dn ASSIGNING FIELD-SYMBOL(<lt_dn>) GROUP BY ( vbeln = <lt_dn>-vbeln bldat = <lt_dn>-bldat
    wadat_ist = <lt_dn>-wadat_ist kunnr = <lt_dn>-kunnr wbstk = <lt_dn>-wbstk
    index = GROUP INDEX size = GROUP SIZE
     ) ASSIGNING FIELD-SYMBOL(<group>).
    READ TABLE lt_vbfa ASSIGNING FIELD-SYMBOL(<lt_vbfav>) WITH KEY vbeln = <group>-vbeln BINARY SEARCH.
    IF sy-subrc EQ 0.
      IF <lt_vbfav>-new_contractdetailid IS INITIAL.
        CONTINUE.
      ENDIF.
    ELSE.
      CONTINUE.
    ENDIF.

    INSERT INITIAL LINE INTO TABLE data-main_data ASSIGNING FIELD-SYMBOL(<main_data>).
    <main_data>-name           = <group>-vbeln .
    <main_data>-date           = <group>-bldat .
    <main_data>-deliverytime   = <group>-wadat_ist .
    <main_data>-deliverystatus = <group>-wbstk .
*    <main_data>-accountid      = <group>-kunnr .
    SELECT SINGLE
      b~smtp_addr
      FROM but021_fs AS a
      JOIN adr6 AS b
      ON a~addrnumber = b~addrnumber
      WHERE a~partner = @<group>-kunnr
      INTO @DATA(smtp_addr)
      .
    IF cl_abap_matcher=>matches( pattern = `^[a-zA-Z0-9._%+-]+@crm\.com$` text = to_lower( smtp_addr ) ) = abap_true.
      DATA(len) = strlen( smtp_addr ) - 8.
      <main_data>-accountid      = <group>-kunnr(len) .
    ELSE.
      <main_data>-accountid      = smtp_addr .
    ENDIF.

    LOOP AT GROUP <group> ASSIGNING FIELD-SYMBOL(<mem>).
      READ TABLE lt_vbfa ASSIGNING FIELD-SYMBOL(<lt_vbfa>) WITH KEY vbeln = <mem>-vbeln posnr = <mem>-posnr BINARY SEARCH.
      IF sy-subrc NE 0.
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
      <deliverydetail>-quantity           = <mem>-lfimg .
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
