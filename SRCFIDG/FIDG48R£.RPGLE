000100950315     H DECEDIT('0,') DATEDIT(*DMY.)
000200950315      *---------*----------------------------------------------------*
000300080728      * FIDG48r                                                      *
000400950315      *---------*                                                    *
000500080728      *         - esiti pda dettaglio colli -                        *
000600000426      *                                                              *
000700950315      *--------------------------------------------------------------*
000800080728     FFidg48d   CF   E             WORKSTN SFILE(LR53S01:NRR3)
000900080728     fazorg01l  if   e           k disk
001000080730     ffipcd01l  if   e           k disk    usropn
001100080730     ffipcd03l  if   e           k disk    usropn rename(fipcd000:fipcd003)
001200080707     ftabel00f  if   e           k disk
001300950320      *----------------------------------------------------------------*
001400950320      *    s c h i e r e                                               *
001500950320      *----------------------------------------------------------------*
001600950315     D KPJBA         E DS
001700080728     D fidg46ds      E DS
001800080728     D fidg47ds      E DS
001900080707     D fipcdcedds    E DS
002000080728     D ds3e          e ds
002100080728     D tibs34ds      E DS                  inz
002200080728     D ddatiute      e ds
002300080728     D azuteds       e ds                  extname(AZUTE00F)
002400950315      *---------------------------------------------------------------*
002500950315      * FLUSSO PRINCIPALE                                             *
002600950315      *---------------------------------------------------------------*
002700950320     C*
002800080728     C*  Caricamento subfile
002900080508     C                   EXSR      INZS01
003000080728     c*  Se il subfile è vuoto esco
003100080526     C     NRR3          IFEQ      0
003200080728     c                   seton                                        lr
003300080728     c                   return
003400080526     C                   END
003500080701     c*
003600080728     C                   Z-ADD     Dg46aasi      v3Caas
003700080728     C                   Z-ADD     Dg46lnpi      v3Clnp
003800080728     C                   Z-ADD     Dg46nrsi      v3Cnrs
003900080728     C                   Z-ADD     Dg46nspi      v3Cnsp
004000080512     C                   Z-ADD     1             NRR3
004100080508     C                   WRITE     LR53Z01
004200950315      *  Loop gestione subfile degli errori
004300080508     C                   DO        *hival
004400080508      *
004500080508     C                   EXFMT     LR53C01
004600080508      *  Fine Lavoro
004700080508     C     *INKC         IFEQ      '1'
004800080728     C                   MOVEL     '1'           dg46erro
004900080508     C                   leave
005000080508     C                   END
005100080508      *  Guida
005200080711     C     *INKL         IFEQ      '1'
005300080728     C                   MOVEL     '2'           dg46erro
005400080508     C                   leave
005500080508     C                   END
005600080508      *  Loop di controlli
005700080508     C                   EXSR      CTR01
005800080508     c
005900080508     C                   ENDdo
006000080508     c*
006100080728     c                   movel     fidg46ds      kpjbu
006200080728     c                   seton                                        lr
006300950317      *---------------------------------------------------------------*
006400950317      * Gestione videata subfile errori                               *
006500950317      *---------------------------------------------------------------*
006600080508     C     INZS01        BEGSR
006700080508     C                   SETOFF                                       202190
006800080508     C                   WRITE     LR53C01
006900950317     C                   SETON                                        2021
007000080512     C                   Z-ADD     0             NRR3              4 0
007100080728     c*
007200080730     c                   if        dg46ndci <> 0
007300080730     c     kpcd3         setll     fipcd03l
007400080730     c                   else
007500080805     c     kpcd1         setll     fipcd01l
007600080730     c                   end
007700080710     c                   do        *hival
007800080730     c                   if        dg46ndci <> 0
007900080730     c     kpcd3         reade     fipcd03l
008000080729     c                   if        %eof(fipcd03l)
008100080710     c                   leave
008200080710     c                   end
008300080730     c                   else
008400080805     c     kpcd1         reade     fipcd01l
008500080730     c                   if        %eof(fipcd01l)
008600080730     c                   leave
008700080730     c                   end
008800080730     c                   end
008900080730     C                   Z-ADD     pcdfls        v3Cfls
009000080730     C                   Z-ADD     pcdlna        v3Clna
009100080730     C                   Z-ADD     pcdnrser      v3Cnrser
009200080730     C                   Z-ADD     pcdnsc        v3Cnsc
009300080730     C                   Z-ADD     pcdfgs        v3Cfgs
009400080730     C                   Z-ADD     pcdndc        v3Cndc
009500080807     C                   Z-ADD     pcdcodaut     v3Ccodaut
009600080710     c                   eval      fipcdcedds = pcddati
009700080730     c                   select
009800080730     c                   when      §PCDSPUCON = 'S'
009900080728     c                   eval      v3cspucon ='SI'
010000080730     c                   when      §PCDSPUCON = 'A'
010100080730     c                   eval      v3cspucon ='An'
010200080730     c                   other
010300080728     c                   clear                   v3cspucon
010400080730     c                   endsl
010500080728     c                   if        §PCDSPUspc = 'S'
010600080728     c                   eval      v3cspuspc ='SI'
010700080728     c                   else
010800080728     c                   clear                   v3cspuspc
010900080728     c                   end
011000080729     c                   if        §pcdcancon <> ' '
011100080729     c                   movel(p)  §pcdcancon    tblkey
011200080728     c     ktab          chain     tabel00f
011300080728     c                   if        not %found(tabel00f) or
011400080728     c                             (%found(tabel00f) and tblflg <> ' ')
011500080728     c                   clear                   v3dcaucon
011600080728     c                   else
011700080728     c                   movel(p)  tbluni        ds3e
011800120806     c                   movel(p)  §3ede12       v3dcaucon
011900080728     c                   end
012000080728     c                   else
012100080728     c                   clear                   v3dcaucon
012200080728     c                   end
012300080730     c                   if        §pcdflgspc ='S'
012400080730     c                   eval      v3cflgspc ='SI'
012500080730     c                   else
012600080730     c                   eval      v3cflgspc ='NO'
012700080730     c                   end
012800080728     C                   ADD       1             NRR3
012900080728     C                   WRITE     LR53S01
013000080728     c                   enddo
013100080728     c*
013200080707     C                   ENDSR
013300080508      *---------------------------------------------------------------*
013400080508      *  Loop di errori  subfile errori                               *
013500080508      *---------------------------------------------------------------*
013600080509     C     CTR01         BEGSR
013700080715     c                   setoff                                       2840
013800950315      *
013900080509     C                   READC     LR53S01                                32
014000980325      *
014100950317     C     *IN32         DOWEQ     '0'
014200950320     C     *IN28         ANDEQ     '0'
014300080704     c*
014400080701     c                   select
014500080728      *  segnacollo
014600080728     C                   when      v3csce = 'S'
014700080728     C                   clear                   fidg47ds
014800080730     C                   Z-ADD     V3Clna        dg47fgsi
014900080730     C                   Z-ADD     V3Cndc        dg47ndci
015000080807     C                   Z-ADD     V3Ccodaut     dg47codaut
015100080730     C                   Z-ADD     V3CAAS        dg47AASi
015200080730     C                   Z-ADD     V3CLNP        dg47LNPi
015300080728     C                   Z-ADD     V3CNSP        dg47NSPi
015400080728     C                   Z-ADD     V3CNRS        dg47NRSi
015500080728     C                   Z-ADD     V3CNsc        dg47Nsci
015600080728     C                   MOVEL(P)  fidg47ds      KPJBU
015700080728     C                   CALL      'FIDG47R'
015800080701     c                   PARM                    KPJBA
015900080728     c                   movel     kpjbu         fidg47ds
016000080730     c                   if        dg47erro <> ' ' and dg47erro <> '2'
016100080728     c                   eval      dg46erro = dg47erro
016200080730     c                   movel(p)  fidg46ds      kpjbu
016300080728     c                   seton                                        lr
016400080728     c                   return
016500080728     c                   end
016600080730     c                   eval      v3csce = ' '
016700080509     C                   ENDsl
016800980325      *  Aggiorno subfile
016900950320     C                   SETON                                        22
017000080707     C                   UPDATE    LR53s01
017100950320     C                   SETOFF                                       22
017200080711     C  N28              READC     LR53S01                                32
017300080509     C                   ENDdo
017400080715     c*
017500080715     c                   z-add     1             nrr3
017600950317     C*
017700950317     C                   ENDSR
017800080509     C*-----------------------------------------------------*
017900080509     C     *INZSR        BEGSR
018000080509     C*-----------------------------------------------------*
018100080508      *
018200080508     C     *ENTRY        PLIST
018300080728     C                   PARM                    kpjba
018400080728     c                   movel     kpjbu         fidg46ds
018500080730     c                   eval      nomPGM = 'FIDG48R'
018600080730     c                   if        dg46ndci <> 0
018700080730     c                   open      fipcd03l
018800080730     c                   else
018900080730     c                   open      fipcd01l
019000080730     c                   end
019100080515     C* Decodifica Elaboratore
019200080515     c     *dtaara       define    §azute        azuteds
019300080515     c     *dtaara       define    §datiute      ddatiute
019400080515     C                   in(E)     *dtaara
019500080515     C                   IF        %Error  or  RSUT = *blanks
019600080515     C                   call      'TIBS34R'
019700080515     C                   parm                    Tibs34Ds
019800080515     C                   in        *dtaara
019900080515     c                   ENDIF
020000080730     c     kpcd3         klist
020100080728     c                   kfld                    dg46fgsi
020200080728     c                   kfld                    dg46ndci
020300080805     c                   kfld                    dg46codaut
020400080729     c                   kfld                    pcdtrd
020500080728     c                   kfld                    dg46aasi
020600080728     c                   kfld                    dg46lnpi
020700080728     c                   kfld                    dg46nrsi
020800080728     c                   kfld                    dg46nspi
020900080730     c     kpcd1         klist
021000080730     c                   kfld                    dg46aasi
021100080730     c                   kfld                    dg46lnpi
021200080730     c                   kfld                    dg46nrsi
021300080730     c                   kfld                    dg46nspi
021400080730     c                   kfld                    pcdtrd
021500080707     c     ktab          klist
021600080707     c                   kfld                    TBLKUT
021700080707     c                   kfld                    TBLCOD
021800080707     c                   kfld                    TBLKEY
021900080707     c                   eval      tblkut = 1
022000080728     c                   eval      tblcod = '3E'
022100080730     c                   eval      pcdtrd = 'CED'
022200080515     c*
022300080509     C                   ENDSR
022400080515
