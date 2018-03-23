000100000414
000200000414     H DECEDIT('0,') DATEDIT(*DMY.)
000300000414
000400030716     D*------------
000500030716     D* PARAMETRI DI CHIAMATA
000600030716     D*------------
000700030716     D IN_Volume       S             30  9
000800030716     D IN_Step         S             30  9
000900030717     D IN_FlgInt       S              1
001000030717     D OUT_LatoL       S             30  9
001100030717     D OUT_LatoH       S             30  9
001200030717     D OUT_LatoW       S             30  9
001300030716     D OUT_Errore      S              1
001400030716
001500030716     D*------------
001600030716     D* VARIABILI DI WRK
001700030716     D*------------
001800030716     D wflgCiclo       S              1    INZ('0')
001900030716     D wLato           S             30  9 INZ(*zeros)
002000030717     D wLatoL          S             30  9 INZ(*zeros)
002100030717     D wLatoH          S             30  9 INZ(*zeros)
002200030717     D wLatoW          S             30  9 INZ(*zeros)
002300030716     D wLatoSav        S             30  9 INZ(*zeros)
002400030716     D wVolume         S             30  9 INZ(*zeros)
002500030716     D wVolumeSav      S             30  9 INZ(*zeros)
002600030717     D wVolumeInt      S             30  0 INZ(*zeros)
002700030717     D wVolumeIntA     S             30    INZ(*blanks)
002800030717     D wScalaVol       S              2  0 INZ(*zeros)
002900030716
003000030716
003100000414     C*------------------------------------------------------------------------*
003200000414     C* MAIN LINE
003300000414     C*------------------------------------------------------------------------*
003400030717     C*
003500030717     C* Elaboro solo se nn già rilevati errori
003600030717     C                   IF        OUT_Errore <> '1'
003700030717     C*
003800030717     C* 1° giro x calcolare il lato cubico
003900000414     C*
004000030716     C* Procedo calcolando il lato x approssimazione
004100030716     C                   DOW       wflgCiclo = '0'
004200030716     C                   EVAL      wVolume = wLato * wLato * wLato
004300030716     C                   IF        wVolume <= IN_Volume
004400030716     C                   EVAL      wLatoSav = wLato
004500030716     C                   EVAL      wVolumeSav = wVolume
004600030716     C                   EVAL      wLato = wLato + IN_Step
004700030716     C                   ELSE
004800030716     C                   EVAL      wflgCiclo = '1'
004900030716     C                   ENDIF
005000030716     C                   ENDDO
005100030717     C*
005200030717     C* 2° giro partendo dal lato cubico temgo constanti 1 dimensione, la 2° la valorizzo alla metà del lato cubico
005300030717     C* e vario la 3° x approssimare il volume
005400030717     C* in input con maggior precisione
005500030717     C                   EVAL      wLatoL = %int(wLatoSav)
005600030717     C                   EVAL      wLatoH = %int(wLatoSav/2)
005700030717     C                   EVAL      wLatoW = 1
005800030717     C                   EVAL      wLatoW = IN_Volume**(1/3)
005900030717     C*
006000030717     C* Procedo calcolando il 3° lato x approssimazione
006100030717     C                   EVAL      wFlgCiclo = '0'
006200030717     C                   DOW       wflgCiclo = '0'
006300030717     C                   EVAL      wVolume = wLatoL * wLatoH * wLatoW
006400030717     C                   IF        wVolume <= IN_Volume
006500030717     C                   EVAL      wLatoSav = wLatoW
006600030717     C                   EVAL      wVolumeSav = wVolume
006700030717     C                   EVAL      wLatoW = wLatoW + IN_Step
006800030717     C                   ELSE
006900030717     C                   EVAL      wflgCiclo = '1'
007000030717     C                   ENDIF
007100030717     C                   ENDDO
007200030717     C
007300030717     C
007400030717     C
007500030716     C*
007600030716     C* Al termine del ciclo valorizzo i parametri d output
007700030717     C                   EVAL      OUT_LatoL = wLatoL
007800030717     C                   EVAL      OUT_LatoH = wLatoH
007900030717     C                   EVAL      OUT_LatoW = wLatoW
008000030716     C                   EVAL      OUT_Errore = *blanks
008100030717     C*
008200030717     C                   ENDIF
008300000414     C*
008400000414     C                   SETON                                        LR
008500030717     C*------------------------------------------------------------------------*
008600000411
008700030717
008800030717     C*------------------------------------------------------------------------*
008900030717     C* CHKVOL - ROUTINE CHE DETERMINA L'ORDINE D GRANDEZZA DEL VOLUME X OTTIMIZZARE IL CALCOLO DEL LATO CUBICO
009000030717     C*------------------------------------------------------------------------*
009100030717     C     CHKVOL        BEGSR
009200030717     C*
009300030717     C* Determino il numero d cifre valorizzate del volume in input
009400030717     C                   Z-ADD     IN_Volume     wVolumeInt
009500030717     C                   EVAL      wVolumeIntA = %editc(wVolumeInt:'4')
009600030717     C                   EVAL      wScalaVol = %len(%trim(wVolumeIntA))
009700030717     C*
009800030717     C* Determino l'ordine d grandezza d partenza del lato cubico da calcolare
009900030717     C                   IF        wScalaVol = 0
010000030717     C                   EVAL      OUT_LatoL   = *zeros
010100030717     C                   EVAL      OUT_LatoH   = *zeros
010200030717     C                   EVAL      OUT_LatoW   = *zeros
010300030717     C                   EVAL      OUT_Errore = '1'
010400030717     C                   ENDIF
010500030717     C                   IF        wScalaVol >= 1 AND wScalaVol <= 3
010600030717     C                   EVAL      wLato = 1
010700030717     C                   ENDIF
010800030717     C                   IF        wScalaVol >= 4 AND wScalaVol <= 6
010900030717     C                   EVAL      wLato = 10
011000030717     C                   ENDIF
011100030717     C                   IF        wScalaVol >= 7 AND wScalaVol <= 9
011200030717     C                   EVAL      wLato = 100
011300030717     C                   ENDIF
011400030717     C                   IF        wScalaVol >= 10 AND wScalaVol <= 12
011500030717     C                   EVAL      wLato = 1000
011600030717     C                   ENDIF
011700030717     C                   IF        wScalaVol >= 13 AND wScalaVol <= 15
011800030717     C                   EVAL      wLato = 10000
011900030717     C                   ENDIF
012000030717     C                   IF        wScalaVol >= 16 AND wScalaVol <= 18
012100030717     C                   EVAL      wLato = 100000
012200030717     C                   ENDIF
012300030717     C                   IF        wScalaVol >= 19 AND wScalaVol <= 21
012400030717     C                   EVAL      wLato = 1000000
012500030717     C                   ENDIF
012600030717     C                   IF        wScalaVol >= 22
012700030717     C                   EVAL      wLato = 10000000
012800030717     C                   ENDIF
012900030717     C*
013000030717     C                   ENDSR
013100030717     C*------------------------------------------------------------------------*
013200030717
013300030717
013400000414     C*------------------------------------------------------------------------*
013500000414     C* *INZSR - OPERAZIONI INIZIALI
013600000414     C*------------------------------------------------------------------------*
013700000414     C     *INZSR        BEGSR
013800030716     C*
013900030716     C* Ricevimento parametri
014000000414     C     *ENTRY        PLIST
014100030716     C                   PARM                    IN_Volume
014200030716     C                   PARM                    IN_Step
014300030717     C                   PARM                    IN_FlgInt
014400030717     C                   PARM                    OUT_LatoL
014500030717     C                   PARM                    OUT_LatoH
014600030717     C                   PARM                    OUT_LatoW
014700030716     C                   PARM                    OUT_Errore
014800011213     C*
014900030717     C* Eseguo subito alcune considerazioni sullo step d incremento x il calcolo del lato cubico
015000030716     C                   IF        IN_Step = *zeros
015100030716     C                   EVAL      IN_Step = 0,1
015200030716     C                   ENDIF
015300030717     C*
015400030717     C* Eseguo subito alcune considerazioni sul valore iniziale da attribuire al lato cubico da calcolare
015500030717     C                   EXSR      CHKVOL
015600030716     C*
015700000414     C                   ENDSR
