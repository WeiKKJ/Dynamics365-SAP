FUNCTION zfm_crm_so_contractclose.
*"----------------------------------------------------------------------
*"*"本地接口：
*"  IMPORTING
*"     VALUE(DATA) TYPE  ZSMAINDATA
*"  EXPORTING
*"     VALUE(RTYPE) TYPE  BAPI_MTYPE
*"     VALUE(RTMSG) TYPE  BAPI_MSG
*"     VALUE(CRM_RETURN) TYPE  ZSCRM_RETURN
*"     VALUE(INSTR) TYPE  STRING
*"     VALUE(OUTSTR) TYPE  STRING
*"     VALUE(STATUS) TYPE  I
*"     VALUE(MSG) TYPE  STRING
*"----------------------------------------------------------------------
  zfmdatasave1 'ZFM_CRM_SO_CONTRACTCLOSE'.
  zfmdatasave2 'B'.
  COMMIT WORK.

  CLEAR:rtype,rtmsg,crm_return,instr,outstr,crmurl,crmappId,header.
  IF data IS INITIAL.
    fillmsg 'E' '请传入DATA数据后再调用本接口'.
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
    rtmsg = |同步CRM失败，状态码：{ status }，http reason：{ msg }，CRM返回：{ crm_return-_error_message }|.
  ENDIF.


  zfmdatasave2 'R'.




ENDFUNCTION.
