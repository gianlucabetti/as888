000100090901     D*------------------
000200160920     D* PASSAGGIO PARAMETRI A UBRTVSPE
000300090901     D*------------------
000400160920     D iRTVSPE_Opz     S              3A   inz
000500160920     D iRTVSPE_AAS     S                   inz like(tasAAS)
000600160920     D iRTVSPE_LNP     S                   inz like(tasLNP)
000700160920     D iRTVSPE_NRS     S                   inz like(tasNRS)
000800160920     D iRTVSPE_NSP     S                   inz like(tasNSP)
000900160920     D oRTVSPE_TASDS E DS                  inz extname(TITAS00F)
001000160920     D oRTVSPE_ARBDS E DS                  inz extname(FNARB00F)
001100160920     D oRTVSPE_BLPDS E DS                  inz extname(FNBLP00F)
001200160920     D oRTVSPE_Esito   S              3A   inz
