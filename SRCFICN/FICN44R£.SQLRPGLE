000100990504     H DEBUG DECEDIT('0,') DATEDIT(*DMY/)
000200060125      *****************************************************************
000300160126      * Il pgm scrive i record di rettifica per le filiali in cui è stato
000400160126      * attivato il bonus per gli autisti
000500060125      *
000600060125      *****************************************************************
000700160125     FTntbe01l  IF   E           K DISK
000800160126     FTntbe02l  IF   E           K DISK    rename(tntbe000:tntbe2) prefix(x)
000900160125     Ffntna00f  o    E             DISK
001000160125     Ffnrna00f  o    E             DISK
001100160125     Ffnfna00f  o    E             DISK
001200030429     F*---------------------------------------------------------------------------------
001300000607     D kpjba         e ds
001400060125     D fiftt         E DS                  extname(fiftt00f)
001500160125     D Dbon          E DS
001600160125     D Dtce          E DS
001700160125     D trul33ds      E DS
001800160127     D autisti         s              3  0
001900160127     D import          s              9  2
002000160126     D padmin          s              7  0
002100160126     D padmax          s              7  0
002200060125     d dataoggi        s               d   datfmt(*eur)
002300060125     d data_gg1        s               d   datfmt(*iso)
002400020205     c**********************************************************************
002500160126     C*
002600160126     C                   move      'BON'         tbecod
002700160126     c     'BON'         setll     tntbe01l
002800160126     c                   do        *hival
002900160126     c     'BON'         reade     tntbe01l
003000160126     c                   if        %eof(tntbe01l)
003100160126     c                   leave
003200160126     c                   endif
003300160126     c                   movel     tbeuni        dbon
003400160126     C                   move      'TCE'         tbecod
003500160126     c                   movel(p)  §boncaus      xTbeke1
003600160126     c     ktbe          chain     tntbe02l
003700160126     c                   if        %found(tntbe02l)
003800160126     c                   movel     xtbeuni       dtce
003900160126     c                   endif
004000160126      *
004100160126     c                   move      *zeros        padmin
004200170111     c                   movel     tbeke1        padmin
004300160126     c                   z-add     9999999       padmax
004400170111     c                   movel     tbeke1        padmax
004500160127     c                   z-add     0             autisti
004600160127     c                   z-add     0             import
004700160126      *
004800060125     c* Lettura fifTT00F x FGS/PDR/DDC
004900160126     C/EXEC SQL
005000160128     C+ DECLARE A1 CURSOR FOR SELECT fttddc, count(*)
005100160126     C+  FROM fiftt00f WHERE
005200160127     C+  fttfvl = 'C' and fttdcv <> 0 and ftttsr = ' ' and fttftr = ' '
005300160127     C+  and fttpdr between :padmin and :padmax  and ftttbn > 0
005400170111     C+  and fttddc >= :§bondti and fttddc <= :§bondtf
005500160128     C+  Group by fttddc
005600160126     C/END-EXEC
005700020328
005800020328     C/EXEC SQL
005900020328     C+ OPEN A1
006000020328     C/END-EXEC
006100020328     C                   DOU       SqlCod <> 0
006200020328     C/EXEC SQL
006300160128     C+ FETCH NEXT FROM A1 INTO :fttddc, :autisti
006400020328     C/END-EXEC
006500020328     C                   SELECT
006600060125     c*
006700020328     C                   WHEN      SqlCod = 0
006800160128     c     autisti       mult      §bonimp       import
006900160128     c                   exsr      scriviRet
007000060125     C*
007100060125      **
007200060125      * ? EoF
007300020328     C                   WHEN      SqlCod = 100
007400160126     c                   leave
007500060125      **
007600060125      * ? Errori
007700020328     C                   WHEN      SqlCod <> 0
007800160126     c                   leave
007900060125      **
008000020328     C                   ENDSL
008100060125      **
008200020328     C                   ENDDO
008300020328     C/EXEC SQL
008400020328     C+ CLOSE A1
008500020328     C/END-EXEC
008600160126     c                   enddo
008700030429      *
008800000607     c                   seton                                        lr
008900160125     C**********************************************************************
009000160125     C** scrittura file rettifiche
009100160125     C**********************************************************************
009200160125     C     scriviRet     BEGSR
009300160125     c                   exsr      repnum
009400160126     c                   z-add     o33nrf        tnanric
009500170111     c                   movel     tbeke1        TNAFILa
009600160201      * inserisco al data solo se minore di oggi in modo da rendere la rettifica non modificabile
009700160201     c                   if        fttddc < oggi
009800160125     c                   move      fttddc        tnadtaf
009900160201     c                   else
010000160201     c                   move      ieri          tnadtaf
010100160201     c                   endif
010200160201      *
010300160127     c                   z-add     import        TNAIMP
010400160126     c                   move      §tcTSR        TNATSR
010500160125     c                   z-add     10            tnafase
010600160126     c                   z-add     §bonfild      TNAFILd
010700160125     c                   move      §boncaus      TNACAUS
010800160125     c                   move      fttddc        TNADTAc
010900160125     c                   move      knmus         TNAuten
011000160127     c                   eval      tnanota = 'Numero di autisti conteggiati '
011100160127     c                             + %editc(autisti:'Z')
011200160125     c                   write     fntna000
011300160125     c                   exsr      storicofasi
011400160125     c                   if        §tcece = 'S'
011500160125     c                   z-add     tnanric       rnanric
011600160125     c                   move      tnadtac       rnadtac
011700160125     c                   move      tnatsr        rnatsr
011800160125     c                   if        §tcecon = 'N'
011900160125      * scrittura record con importo negativo su filiale richiedente
012000160125     c                   eval      rnaimp = (tnaimp * -1)
012100160125     c                   move      tnafila       rnafil
012200160125     c                   write     fnrna000
012300160125     c                   endif
012400160125      * scrittura record con importo positivo su filiale addebito
012500160125     c                   if        §tctpa = 'C'
012600160125     c                   eval      rnaimp = tnaimp
012700160125     c                   move      tnafild       rnafil
012800160125     c                   write     fnrna000
012900160125     c                   endif
013000160125     c                   endif
013100160125     C                   endsr
013200160125     C**********************************************************************
013300160125     C** inserisce lo storico delle fasi
013400160125     C**********************************************************************
013500160125     C     storicofasi   BEGSR
013600160125     c                   z-add     tNANRIC       FNANRIC
013700160126     c                   move      tNAFILa       FNAFIL
013800160125     c                   move      tNAFASE       FNAFASE
013900160201     c                   move      tnadtaf       FNADTAAG
014000160125     c                   z-add     oracor        FNAORAAG
014100160125     c                   move      tNANOTA       FNANOTA
014200160125     c                   move      knmus         FNAUTE
014300160125     c                   write     fnfna000
014400160125     C                   endsr
014500160125     c**********************************************************************
014600160125     c* reperimento numero viaggio
014700160125     c**********************************************************************
014800160125     C     repnum        BEGSR
014900160125     c                   clear                   trul33ds
015000160125     c                   move      'L'           i33tla
015100160125     c                   z-add     90            i33cnu
015200160125     c                   z-add     1             i33num
015300160125     c                   movel     trul33ds      kpjbu
015400160125     c                   call      'TRUL33R'
015500160125     c                   PARM                    kpjba
015600160125     c                   movel     kpjbu         trul33ds
015700160125     C                   ENDSR
015800020205     C**-------------------------------------------------------------**
015900000607     c     *inzsr        begsr
016000060125     C**-------------------------------------------------------------**
016100000607     c     *entry        plist
016200020205     c                   parm                    kpjba
016300030429      *----------*
016400030429     C* GIRO DATA ODIERNA
016500030429     C                   TIME                    W0140            14 0
016600030429     C                   MOVE      W0140         UDATE8            8 0
016700160126     C                   MOVEl     W0140         oracor            6 0
016800060125     C     *eur          movel     UDATE8        dataoggi
016900160201     c                   move      dataoggi      data_gg1
017000160201     c                   move      data_gg1      oggi              8 0
017100060125     C     dataoggi      subdur    1:*d          data_gg1
017200160201     c                   move      data_gg1      ieri              8 0
017300060125      *
017400160125     C* Definisco chiavi di accesso
017500160125     C     Ktbe          KLIST
017600160126     C                   KFLD                    xtbecod
017700160126     C                   KFLD                    tbelin
017800160126     C                   KFLD                    tbesif
017900160126     C                   KFLD                    xtbeke1
018000160126     C                   move      'TCE'         xtbecod
018100071017      *
018200020402     c*
018300020402     c                   endsr
