000100090901     D*------------------
000200160920     D* PASSAGGIO PARAMETRI A UBRTVSPE
000300090901     D*------------------
000400160922     D iRTVSPE_Opz     S              3A
000500160922     D iRTVSPE_AAS     S                   like(tasAAS)
000600160922     D iRTVSPE_LNP     S                   like(tasLNP)
000700160922     D iRTVSPE_NRS     S                   like(tasNRS)
000800160922     D iRTVSPE_NSP     S                   like(tasNSP)
000900160922     D oRTVSPE_TASDS E DS                  extname(TITAS00F)
001000160922     D oRTVSPE_ARBDS E DS                  extname(FNARB00F)
001100160922     D oRTVSPE_BLPDS E DS                  extname(FNBLP00F)
001200160922     D oRTVSPE_Esito   S              3A
