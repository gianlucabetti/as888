000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200900606?     *--------------------------------------------------------------*
000300030326      * TNSC78R                                                      *
000400930510      *        TRASMISSIONE                                          *
000500030326      *        FIVABCAF A 001                                        *
000600900606?     *--------------------------------------------------------------*
001300030326     D DSCMZ         E DS                  INZ
001900030326     d KPJBA         E DS
001901030326     d tibs55ds      E DS
003500971121?     *--------------------------------------------------------------*
003502030326     C     *ENTRY        PLIST
003503030326     C                   PARM                    KPJBA
003504030326?     *--------------------------------------------------------------*
007800020805     C                   CLEAR                   TIBS55ds
007900971121     C                   MOVEL     'L'           I50TLA
008000971121     C                   MOVEL     KNSIF         I50PSI
008100030326     C                   MOVEL     046           I50PPO
008200030326     C                   MOVEL     001           I50APO
008300020805     C                   CALL      'TIBS55R'
008400020805     C                   PARM                    TIBS55ds
008401030326     C* GUARDO SE IL MEMBRO è VUOTO - FIVABCAF
008402030326     C                   MOVEL(P)  'FIVABCAF'    FILE             10
008403030326     C                   MOVEL(p)  'FIVABCAF'    MBR              10
008404030326     C                   MOVEL(p)  O50PLG        LIBR             10
008405030326     C                   CALL      'TRUL51C'
008406030326     C                   PARM                    LIBR
008407030326     C                   PARM                    FILE
008408030326     C                   PARM                    MBR
008409030326     C                   PARM                    NRR              10 0
008410030326      *
008411030326     c                   if        nrr > 0
008412030326     C                   CLEAR                   DsCmz
008413030326     C                   MOVEL     O50LCL        CMZLCL            1
008414030326     C                   MOVE      '001'         CMZDST
008415030326     C                   MOVEL     O50PLG        CMZLBD
008416030326     C                   MOVEL     O50ALG        CMZLBA
008417030326     C                   MOVEL     O50ASI        CMZSYF
008418030326     C                   MOVEL     'FIVABCAF'    CMZFLD           10
008419030326     C                   MOVEL     'FIVAB00F'    CMZFLA           10
008420030326     C                   MOVE      '0000'        CMZORI
008421030326     C                   MOVE      '0000000'     CMZDTI
008422030326     C                   MOVE      'A'           CMZADD
008423030326     C                   MOVE      'S'           CMZCLR
008424030326     C                   MOVE      'S'           CMZFTR
008425030326     C                   MOVE      *BLANK        CMZERR
008426030326     C                   MOVE      'N'           CMZURG
008427030326     C                   MOVEL     'FIVABCAF'    CMZMBD
008428030326     C                   MOVEL     'FIVAB00F'    CMZMBA
008429030326     C                   move      nrr           CMZNRR
008430030326     C                   CLEAR                   W236
008431030326      *
008432030326     C                   CALL      'TIBS53C'
008433030326     C                   PARM                    CMZLCL
008434030326     C                   PARM                    CMZDST
008435030326     C                   PARM                    CMZFLD
008436030326     C                   PARM                    CMZMBD
008437030326     C                   PARM                    CMZLBD
008438030326     C                   PARM                    CMZFLA
008439030326     C                   PARM                    CMZMBA
008440030326     C                   PARM                    CMZLBA
008441030326     C                   PARM                    CMZORI
008442030326     C                   PARM                    CMZDTI
008443030326     C                   PARM                    CMZURG
008444030326     C                   PARM                    CMZADD
008445030326     C                   PARM                    CMZCLR
008446030326     C                   PARM                    CMZFTR
008447030326     C                   PARM                    CMZERR
008448030326     C                   PARM                    CMZAZN
008449030326     C                   PARM                    CMZSYF
008450030326     C                   PARM                    CMZNRR
008451030326     C                   PARM                    CMZPRR
008452030326     C                   PARM                    W236            236
008453030326     c                   end
008454030331     C* GUARDO SE IL MEMBRO è VUOTO - FIVADCAF
008455030331     C                   MOVEL(P)  'FIVADCAF'    FILE             10
008456030331     C                   MOVEL(p)  'FIVADCAF'    MBR              10
008457030331     C                   MOVEL(p)  O50PLG        LIBR             10
008458030331     C                   CALL      'TRUL51C'
008459030331     C                   PARM                    LIBR
008460030331     C                   PARM                    FILE
008461030331     C                   PARM                    MBR
008462030331     C                   PARM                    NRR              10 0
008463030331      *
008464030331     c                   if        nrr > 0
008465030331     C                   CLEAR                   DsCmz
008466030331     C                   MOVEL     O50LCL        CMZLCL            1
008467030331     C                   MOVE      '001'         CMZDST
008468030331     C                   MOVEL     O50PLG        CMZLBD
008469030331     C                   MOVEL     O50ALG        CMZLBA
008470030331     C                   MOVEL     O50ASI        CMZSYF
008471030331     C                   MOVEL     'FIVADCAF'    CMZFLD           10
008472030331     C                   MOVEL     'FIVAD00F'    CMZFLA           10
008473030331     C                   MOVE      '0000'        CMZORI
008474030331     C                   MOVE      '0000000'     CMZDTI
008475030331     C                   MOVE      'A'           CMZADD
008476030331     C                   MOVE      'S'           CMZCLR
008477030331     C                   MOVE      'S'           CMZFTR
008478030331     C                   MOVE      *BLANK        CMZERR
008479030331     C                   MOVE      'N'           CMZURG
008480030331     C                   MOVEL     'FIVADCAF'    CMZMBD
008481030331     C                   MOVEL     'FIVAD00F'    CMZMBA
008482030331     C                   move      nrr           CMZNRR
008483030331     C                   CLEAR                   W236
008484030331      *
008485030331     C                   CALL      'TIBS53C'
008486030331     C                   PARM                    CMZLCL
008487030331     C                   PARM                    CMZDST
008488030331     C                   PARM                    CMZFLD
008489030331     C                   PARM                    CMZMBD
008490030331     C                   PARM                    CMZLBD
008491030331     C                   PARM                    CMZFLA
008492030331     C                   PARM                    CMZMBA
008493030331     C                   PARM                    CMZLBA
008494030331     C                   PARM                    CMZORI
008495030331     C                   PARM                    CMZDTI
008496030331     C                   PARM                    CMZURG
008497030331     C                   PARM                    CMZADD
008498030331     C                   PARM                    CMZCLR
008499030331     C                   PARM                    CMZFTR
008500030331     C                   PARM                    CMZERR
008501030331     C                   PARM                    CMZAZN
008502030331     C                   PARM                    CMZSYF
008503030331     C                   PARM                    CMZNRR
008504030331     C                   PARM                    CMZPRR
008505030331     C                   PARM                    W236            236
008506030331     c                   end
008507030326     C                   SETON                                        LR
