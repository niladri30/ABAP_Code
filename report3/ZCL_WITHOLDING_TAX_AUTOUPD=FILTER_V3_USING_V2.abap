  METHOD FILTER_V3_USING_V2.


    DATA(LT_V2) = IT_LFBW_V2.

    "Remove data from V2 which already exists in V3
    LOOP AT IT_LFBW_V3 ASSIGNING FIELD-SYMBOL(<FS_V3>).

      DELETE LT_V2 WHERE  MANDT      = <FS_V3>-MANDT
*                     AND  LIFNR      = <FS_V3>-LIFNR
                     AND  BUKRS      = <FS_V3>-BUKRS
                     AND  WITHT      = <FS_V3>-WITHT
                     AND  WT_SUBJCT  = <FS_V3>-WT_SUBJCT
                     AND  QSREC      = <FS_V3>-QSREC
                     AND  WT_WTSTCD  = <FS_V3>-WT_WTSTCD
                     AND  WT_WITHCD  = <FS_V3>-WT_WITHCD
                     AND  WT_EXNR    = <FS_V3>-WT_EXNR
                     AND  WT_EXRT    = <FS_V3>-WT_EXRT
                     AND  WT_EXDF    = <FS_V3>-WT_EXDF
                     AND  WT_EXDT    = <FS_V3>-WT_EXDT
                     AND  WT_WTEXRS  = <FS_V3>-WT_WTEXRS.

    ENDLOOP. "IT_LFBW_V3

    IF LT_V2 IS NOT INITIAL.

      ET_V3_FILTERED = VALUE #( FOR <FS_TEMP> IN LT_V2
                                  (
                                   MANDT      = <FS_TEMP>-MANDT
                                   LIFNR      = IV_LIFNR
                                   BUKRS      = <FS_TEMP>-BUKRS
                                   WITHT      = <FS_TEMP>-WITHT
                                   WT_SUBJCT  = <FS_TEMP>-WT_SUBJCT
                                   QSREC      = <FS_TEMP>-QSREC
                                   WT_WTSTCD  = <FS_TEMP>-WT_WTSTCD
                                   WT_WITHCD  = <FS_TEMP>-WT_WITHCD
                                   WT_EXNR    = <FS_TEMP>-WT_EXNR
                                   WT_EXRT    = <FS_TEMP>-WT_EXRT
                                   WT_EXDF    = <FS_TEMP>-WT_EXDF
                                   WT_EXDT    = <FS_TEMP>-WT_EXDT
                                   WT_WTEXRS  = <FS_TEMP>-WT_WTEXRS
                                  )
                              ).

    ENDIF. "LT_V2
    REFRESH: LT_V2.

  ENDMETHOD.