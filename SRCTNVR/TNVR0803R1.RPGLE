000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000400030804
000500030805     FTITAS00F  UP   E             DISK
000600030805     FTITA730C  UF A E           K DISK
000700030804
000701030805     D SVI             S              1    DIM(20)                              SALVO SIGLE VARIE
000702030805     D VAI             S             11  3 DIM(20)                              SALVO IMPORTI VARIE
000703030805     D SVo             S              1    DIM(20)                              SALVO SIGLE VARIE
000704030805     D VAo             S             11  3 DIM(20)                              SALVO IMPORTI VARIE
000705030805
000800961108     C*
000801030805     C                   EVAL      *IN99 = *On
000802030805     C*
000900980702     C     KTAS          KLIST
001000030804     C                   KFLD                    TASAAS
001100030804     C                   KFLD                    TAsLNP
001200030804     C                   KFLD                    TAsNRS
001300030804     C                   KFLD                    TAsNSP
001400030804     C                   KFLD                    TAsTBL
002100030804
002600030805    1C                   IF        TASFIV > 900   and
002601030805     C                             (TASdFT = 20030731 or tasdFT = 20030801)
002602030805
002603030805    2c                   IF        TASSV1 = '&' or tassv2 = '&'
002604030805     c                                          or tassv3 <> *blanks
002700030804
002701030805     C                   CLEAR                   WTA7              1
002702030805     C                   CLEAR                   SVI
002703030805     C                   CLEAR                   VAI
002704030805     C                   CLEAR                   SVO
002705030805     C                   CLEAR                   VAO
002706030805
002707030805     c                   IF        TASSV1 <> *blanks
002708030805     c                   movel     TASSV1        svI(1)
002709030805     c                   movel     TASVA1        vaI(1)
002710030805     c                   ENDIF
002711030805
002712030805     c                   IF        TASSV2 <> *blanks
002713030805     c                   movel     TASSV2        svI(2)
002714030805     c                   movel     TASVA2        vaI(2)
002715030805     c                   ENDIF
002716030805
002717030805     c                   IF        TASSV3 <> *blanks
002718030805     c                   movel     TASSV3        svI(3)
002719030805     c                   movel     TASVA3        vaI(3)
002720030805     C                   Z-ADD     3             X
002721030805     c     ktas          setll     tita730c
002722030805     c     ktas          reade     tita730c
002723030805     c                   DOW       NOT %eof(TITA730C)
002724030805     C                   EVAL      WTA7 = 'S'
002725030805     C                   ADD       1             X                 2 0
002726030805     c                   movel     TA7SVN        svI(X)
002727030805     c                   movel     TA7VAN        vaI(X)
002728030805     c     ktas          reade     tita730c
002729030805     C                   ENDDO
002730030805     c                   ENDIF
005000030804
005001030805     C                   Z-ADD     1             YY                2 0
005004030805     C     '&'           LOOKUP    SVI(YY)                                01
005006030805
005007030805      * TROVATA VARIA '&'
005008030805    3C                   IF        *IN01 = *ON
005009030805      * PULISCO VARIE SU TITAS
005010030805     c                   CLEAR                   TASSV1
005011030805     c                   CLEAR                   TASVA1
005012030805     c                   CLEAR                   TASSV2
005013030805     c                   CLEAR                   TASVA2
005014030805     c                   CLEAR                   TASSV3
005015030805     c                   CLEAR                   TASVA3
005016030805      * SE PRESENTE TITA7 LO CANCELLO
005017030805     C                   IF        WTA7 = 'S'  AND *IN99 = *ON
005018030805     c                   eval      *in32 = *off
005019030805     c     ktas          setll     tita730c
005020030805     c     ktas          reade     tita730c                               32
005021030805     c                   DOw       *in32 = *off
005022030805     c                   DELETE    tita730c
005023030805     c     ktas          reade     tita730c                               32
005024030805     C                   ENDDO
005025030805     C                   ENDIF
005026030805      **
005027030805     C                   Z-ADD     0             XX                2 0
005028030805    4C     1             DO        20            WW                2 0
005029030805    5C                   IF        SVI(WW) <> '&' AND  SVI(WW) <> 'Z'
005030030805     C                                            AND  SVI(WW) <> *BLANKS
005031030805     c                   ADD       1             XX
005032030805     c                   Z-ADD     VAI(WW)       VAO(XX)
005033030805     c                   MOVEL     SVI(WW)       SVO(XX)
005034030805     C                   SELECT
005035030805     C                   WHEN      XX = 1
005036030805     c                   Z-ADD     VAI(WW)       TASVA1
005037030805     c                   MOVEL     SVI(WW)       TASSV1
005038030805     C                   WHEN      XX = 2
005039030805     c                   Z-ADD     VAI(WW)       TASVA2
005040030805     c                   MOVEL     SVI(WW)       TASSV2
005041030805     C                   WHEN      XX = 3
005042030805     c                   Z-ADD     VAI(WW)       TASVA3
005043030805     c                   MOVEL     SVI(WW)       TASSV3
005044030805     C                   OTHER
005045030805     c                   Z-ADD     VAI(WW)       TA7VAN
005046030805     c                   MOVEL     SVI(WW)       TA7SVN
005047030805     c                   Z-ADD     TASAAS        TA7AAS
005048030805     c                   Z-ADD     TASLNP        TA7LNP
005049030805     c                   Z-ADD     TASNRS        TA7NRS
005050030805     c                   Z-ADD     TASNSP        TA7NSP
005051030805     c                   MOVEL     TASTBL        TA7TBL
005052030805     C   99              WRITE     TITA7000
005053030805     C                   ENDSL
005054030805    5C                   ENDIF
005055030805    4C                   ENDDO
005056030805
005057030805     C                   CLEAR                   TASIMV
005058030805     C   99              UPDATE    TITAS000
005059030805    3C                   ENDIF
005060030805    2C                   ENDIF
005061030805    1C                   ENDIF
