REPORT ZP2P_WITHOLDING_TAX_AUTOUPD_V3.

DATA: LV_STR TYPE STRING.
CONSTANTS: LC_ZP2P_LWT_V3 TYPE TVARVC-NAME VALUE 'ZP2P_LWT_V3'.

"Get variant details.
SELECT SINGLE FROM TVARVC
         FIELDS LOW
         WHERE NAME =  @LC_ZP2P_LWT_V3
         INTO @DATA(LV_NO_RANGE).

IF LV_NO_RANGE IS NOT   INITIAL.

  LV_STR = |{ LV_NO_RANGE }{ TEXT-001 }|.

  "Get V2 and V3 data
  SELECT FROM LFA1 AS A INNER JOIN LFB1 AS B
         ON A~LIFNR = B~LIFNR
       FIELDS A~LIFNR,
              A~STCD3,
              A~J_1IPANNO,
              A~KONZS,
              B~BUKRS
        WHERE A~LIFNR LIKE @LV_STR "'73%'
          AND A~STCD3 IS NOT INITIAL
          AND A~J_1IPANNO IS NOT INITIAL
          AND A~KONZS IS NOT INITIAL
        INTO  TABLE @DATA(LT_LFA1_V3).

  IF LT_LFA1_V3 IS NOT INITIAL.

    SORT LT_LFA1_V3 BY KONZS LIFNR BUKRS.

    LOOP AT LT_LFA1_V3 INTO DATA(LS_LFA1_V3).

      "Check for V2 vendor.
      ZCL_WITHOLDING_TAX_AUTOUPD=>CHECK_FOR_V2_VENDOR(
        EXPORTING
          IV_KONZS    = LS_LFA1_V3-KONZS
          IV_BUKRS    = LS_LFA1_V3-BUKRS
        IMPORTING
          ET_LFBW_V2  = DATA(LT_LFBW_V2)
          EV_COUNT_V2 = DATA(LV_COUNT_V2)
      ).

      "Check for V3 Vendor
      ZCL_WITHOLDING_TAX_AUTOUPD=>CHECK_FOR_V3_VENDOR(
        EXPORTING
          IV_LIFNR    = LS_LFA1_V3-LIFNR
          IV_BUKRS    = LS_LFA1_V3-BUKRS
        IMPORTING
          ET_LFBW_V3  = DATA(LT_LFBW_V3)
          EV_COUNT_V3 = DATA(LV_COUNT_V3)
      ).

      "Check if V3 does not have the data from V2
      IF LV_COUNT_V3 LT LV_COUNT_V2.

        "Get only V2 data not in V3
        ZCL_WITHOLDING_TAX_AUTOUPD=>FILTER_V3_USING_V2(
                            EXPORTING
                              IV_LIFNR       = LS_LFA1_V3-LIFNR
                              IT_LFBW_V2     = LT_LFBW_V2
                              IT_LFBW_V3     = LT_LFBW_V3
                            IMPORTING
                              ET_V3_FILTERED = DATA(LT_V3_FILTERED)
                                                      ).

        IF LT_V3_FILTERED IS NOT INITIAL.

          "Udpate LFBW table
          ZCL_WITHOLDING_TAX_AUTOUPD=>UPDATE_TABLE(
                             EXPORTING
                               IV_TABLE       = ZCL_WITHOLDING_TAX_AUTOUPD=>LC_LFBW
                               IT_V3_FILTERED = LT_V3_FILTERED
                               IV_V2          = LS_LFA1_V3-KONZS
                                                   ).

        ENDIF. "LT_V3_FILTERED
      ENDIF. "COUNT

      CLEAR: LS_LFA1_V3.
      CLEAR: LV_COUNT_V2, LV_COUNT_V3.
      REFRESH: LT_LFBW_V2, LT_LFBW_V3.

    ENDLOOP. "LT_LFA1_V3

  ELSE.
    MESSAGE TEXT-002 TYPE SY-ABCDE+4(1).
  ENDIF." LT_LFA1_V3
ELSE.
  MESSAGE TEXT-003 TYPE SY-ABCDE+4(1).

ENDIF. "LV_NO_RANGE