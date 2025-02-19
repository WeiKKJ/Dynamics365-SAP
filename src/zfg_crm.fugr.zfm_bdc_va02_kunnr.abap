FUNCTION zfm_bdc_va02_kunnr.
*"----------------------------------------------------------------------
*"*"本地接口：
*"  IMPORTING
*"     VALUE(CTU) LIKE  APQI-PUTACTIVE DEFAULT 'X'
*"     VALUE(MODE) LIKE  APQI-PUTACTIVE DEFAULT 'N'
*"     VALUE(UPDATE) LIKE  APQI-PUTACTIVE DEFAULT 'L'
*"     VALUE(GROUP) LIKE  APQI-GROUPID OPTIONAL
*"     VALUE(USER) LIKE  APQI-USERID OPTIONAL
*"     VALUE(KEEP) LIKE  APQI-QERASE OPTIONAL
*"     VALUE(HOLDDATE) LIKE  APQI-STARTDATE OPTIONAL
*"     VALUE(NODATA) LIKE  APQI-PUTACTIVE DEFAULT '/'
*"     VALUE(VBELN_001) LIKE  BDCDATA-FVAL DEFAULT 'VBELN'
*"     VALUE(BSTKD_002) LIKE  BDCDATA-FVAL OPTIONAL
*"     VALUE(KUNNR_003) LIKE  BDCDATA-FVAL DEFAULT 'KUNNRAG'
*"     VALUE(KUNNR_004) LIKE  BDCDATA-FVAL DEFAULT 'KUNNRWE'
*"     VALUE(GUEBG_005) LIKE  BDCDATA-FVAL OPTIONAL
*"     VALUE(GUEEN_006) LIKE  BDCDATA-FVAL OPTIONAL
*"     VALUE(PRSDT_007) LIKE  BDCDATA-FVAL OPTIONAL
*"     VALUE(VSBED_008) LIKE  BDCDATA-FVAL OPTIONAL
*"     VALUE(BSTKD_009) LIKE  BDCDATA-FVAL OPTIONAL
*"     VALUE(KUNNR_010) LIKE  BDCDATA-FVAL DEFAULT 'KUNNRAG'
*"     VALUE(KUNNR_011) LIKE  BDCDATA-FVAL DEFAULT 'KUNNRWE'
*"     VALUE(GUEBG_012) LIKE  BDCDATA-FVAL OPTIONAL
*"     VALUE(GUEEN_013) LIKE  BDCDATA-FVAL OPTIONAL
*"     VALUE(PRSDT_014) LIKE  BDCDATA-FVAL OPTIONAL
*"     VALUE(VSBED_015) LIKE  BDCDATA-FVAL OPTIONAL
*"  EXPORTING
*"     VALUE(SUBRC) LIKE  SYST-SUBRC
*"  TABLES
*"      MESSTAB STRUCTURE  BDCMSGCOLL OPTIONAL
*"----------------------------------------------------------------------

  subrc = 0.

  PERFORM bdc_nodata      USING nodata.

  PERFORM open_group      USING group user keep holddate ctu.

  PERFORM bdc_dynpro      USING 'SAPMV45A' '0102'.
  PERFORM bdc_field       USING 'BDC_CURSOR'
                                'VBAK-VBELN'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                                '=ENT2'.
  PERFORM bdc_field       USING 'VBAK-VBELN'
                                vbeln_001.
  PERFORM bdc_dynpro      USING 'SAPMV45A' '4001'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                                '/00'.
*perform bdc_field       using 'VBKD-BSTKD'
*                              BSTKD_002.
  PERFORM bdc_field       USING 'BDC_CURSOR'
                                'KUAGV-KUNNR'.
  PERFORM bdc_field       USING 'KUAGV-KUNNR'
                                kunnr_003.
  PERFORM bdc_field       USING 'KUWEV-KUNNR'
                                kunnr_004.
*perform bdc_field       using 'VBAK-GUEBG'
*                              GUEBG_005.
*perform bdc_field       using 'VBAK-GUEEN'
*                              GUEEN_006.
*perform bdc_field       using 'VBKD-PRSDT'
*                              PRSDT_007.
*perform bdc_field       using 'VBAK-VSBED'
*                              VSBED_008.
  PERFORM bdc_dynpro      USING 'SAPMV45A' '4001'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                                '=SICH'.
*perform bdc_field       using 'VBKD-BSTKD'
*                              BSTKD_009.
  PERFORM bdc_field       USING 'KUAGV-KUNNR'
                                kunnr_010.
  PERFORM bdc_field       USING 'KUWEV-KUNNR'
                                kunnr_011.
*perform bdc_field       using 'VBAK-GUEBG'
*                              GUEBG_012.
*perform bdc_field       using 'VBAK-GUEEN'
*                              GUEEN_013.
*perform bdc_field       using 'VBKD-PRSDT'
*                              PRSDT_014.
*perform bdc_field       using 'VBAK-VSBED'
*                              VSBED_015.
  PERFORM bdc_field       USING 'BDC_CURSOR'
                                'RV45A-MABNR(01)'.
  PERFORM bdc_dynpro      USING 'SAPLSPO2' '0100'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                                '=OPT1'.
  PERFORM bdc_transaction TABLES messtab
  USING                         'VA02'
                                ctu
                                mode
                                update.
  IF sy-subrc <> 0.
    subrc = sy-subrc.
    EXIT.
  ENDIF.

  PERFORM close_group USING     ctu.





ENDFUNCTION.
INCLUDE bdcrecxy .
