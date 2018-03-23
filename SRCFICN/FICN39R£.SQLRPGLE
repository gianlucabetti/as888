000100990504     H DEBUG DECEDIT('0,') DATEDIT(*DMY/)
000200060125      *****************************************************************
000300160126      * Il pgm scrive i record di rettifica per le filiali in cui è stato
000400161013      * attivato il BOI  per gli autisti
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
001500161013     D DBOI          E DS
001600160125     D Dtce          E DS
001700160125     D trul33ds      E DS
001800030429      *
001900160127     D autisti         s              3  0
002000160127     D import          s              9  2
002100160126     D padmin          s              7  0
002200160126     D padmax          s              7  0
002300060125     d dataoggi        s               d   datfmt(*eur)
002400060125     d data_gg1        s               d   datfmt(*iso)
002500020205     c**********************************************************************
002600160126     C*
002700161013     C                   move      'BOI'         tbecod
002800161013     c     'BOI'         setll     tntbe01l
002900160126     c                   do        *hival
003000161013     c     'BOI'         reade     tntbe01l
003100160126     c                   if        %eof(tntbe01l)
003200160126     c                   leave
003300160126     c                   endif
003400161013     c                   movel     tbeuni        dBOI
003500160126     C                   move      'TCE'         tbecod
003600161013     c                   movel(p)  §BOIcaus      xTbeke1
003700160126     c     ktbe          chain     tntbe02l
003800160126     c                   if        %found(tntbe02l)
003900160126     c                   movel     xtbeuni       dtce
004000160126     c                   endif
004100160126      *
004200160126     c                   move      *zeros        padmin
004300170111     c                   movel     tbeke1        padmin
004400160126     c                   z-add     9999999       padmax
004500170111     c                   movel     tbeke1        padmax
004600160127     c                   z-add     0             autisti
004700160127     c                   z-add     0             import
004800160126      *
004900060125     c* Lettura fifTT00F x FGS/PDR/DDC
005000160126     C/EXEC SQL
005100161013     C+ DECLARE A1 CURSOR FOR
005200161013     C+  SELECT FTTDDC, COUNT(*), SUM(A) A           FROM
005300170111     C+  (SELECT FTTDDC,
005400161013     C+  CASE WHEN FTTTBN < :§BOIIMP THEN FTTTBN ELSE :§BOIIMP END AS A
005500161013     C+  FROM FIFTT00F WHERE
005600161013     C+  fttfvl = 'C' and fttdcv <> 0 and ftttsr = ' ' and fttftr = ' '
005700161013     C+  and fttpdr between :padmin and :padmax  and fttTBN > 0
005800170111     C+  and fttddc >= :§BOIdti and fttddc <= :§boidtf) AS TABELLA
005900161013     C+  Group by fttddc
006000160126     C/END-EXEC
006100020328
006200020328     C/EXEC SQL
006300020328     C+ OPEN A1
006400020328     C/END-EXEC
006500020328     C                   DOU       SqlCod <> 0
006600020328     C/EXEC SQL
006700161007     C+ FETCH NEXT FROM A1 INTO :fttddc, :autisti, :import
006800020328     C/END-EXEC
006900020328     C                   SELECT
007000060125     c*
007100020328     C                   WHEN      SqlCod = 0
007200160128     c                   exsr      scriviRet
007300060125     C*
007400060125      **
007500060125      * ? EoF
007600020328     C                   WHEN      SqlCod = 100
007700160126     c                   leave
007800060125      **
007900060125      * ? Errori
008000020328     C                   WHEN      SqlCod <> 0
008100160126     c                   leave
008200060125      **
008300020328     C                   ENDSL
008400060125      **
008500020328     C                   ENDDO
008600020328     C/EXEC SQL
008700020328     C+ CLOSE A1
008800020328     C/END-EXEC
008900160126     c                   enddo
009000030429      *
009100000607     c                   seton                                        lr
009200160125     C**********************************************************************
009300160125     C** scrittura file rettifiche
009400160125     C**********************************************************************
009500160125     C     scriviRet     BEGSR
009600160125     c                   exsr      repnum
009700160126     c                   z-add     o33nrf        tnanric
009800170111     c                   MOVEL     tbeke1        TNAFILa
009900160201      * inserisco al data solo se minore di oggi in modo da rendere la rettifica non modificabile
010000160201     c                   if        fttddc < oggi
010100160125     c                   move      fttddc        tnadtaf
010200160201     c                   else
010300160201     c                   move      ieri          tnadtaf
010400160201     c                   endif
010500160127     c                   z-add     import        TNAIMP
010600160126     c                   move      §tcTSR        TNATSR
010700160125     c                   z-add     10            tnafase
010800161013     c                   z-add     §BOIfild      TNAFILd
010900161013     c                   move      §BOIcaus      TNACAUS
011000160125     c                   move      fttddc        TNADTAc
011100160125     c                   move      knmus         TNAuten
011200160127     c                   eval      tnanota = 'Numero di autisti conteggiati '
011300160127     c                             + %editc(autisti:'Z')
011400160125     c                   write     fntna000
011500160125     c                   exsr      storicofasi
011600160125     c                   if        §tcece = 'S'
011700160125     c                   z-add     tnanric       rnanric
011800160125     c                   move      tnadtac       rnadtac
011900160125     c                   move      tnatsr        rnatsr
012000160125     c                   if        §tcecon = 'N'
012100160125      * scrittura record con importo negativo su filiale richiedente
012200160125     c                   eval      rnaimp = (tnaimp * -1)
012300160125     c                   move      tnafila       rnafil
012400160125     c                   write     fnrna000
012500160125     c                   endif
012600160125      * scrittura record con importo positivo su filiale addebito
012700160125     c                   if        §tctpa = 'C'
012800160125     c                   eval      rnaimp = tnaimp
012900160125     c                   move      tnafild       rnafil
013000160125     c                   write     fnrna000
013100160125     c                   endif
013200160125     c                   endif
013300160125     C                   endsr
013400160125     C**********************************************************************
013500160125     C** inserisce lo storico delle fasi
013600160125     C**********************************************************************
013700160125     C     storicofasi   BEGSR
013800160125     c                   z-add     tNANRIC       FNANRIC
013900160126     c                   move      tNAFILa       FNAFIL
014000160125     c                   move      tNAFASE       FNAFASE
014100160201     c                   move      tnadtaf       FNADTAAG
014200160125     c                   z-add     oracor        FNAORAAG
014300160125     c                   move      tNANOTA       FNANOTA
014400160125     c                   move      knmus         FNAUTE
014500160125     c                   write     fnfna000
014600160125     C                   endsr
014700160125     c**********************************************************************
014800160125     c* reperimento numero viaggio
014900160125     c**********************************************************************
015000160125     C     repnum        BEGSR
015100160125     c                   clear                   trul33ds
015200160125     c                   move      'L'           i33tla
015300160125     c                   z-add     90            i33cnu
015400160125     c                   z-add     1             i33num
015500160125     c                   movel     trul33ds      kpjbu
015600160125     c                   call      'TRUL33R'
015700160125     c                   PARM                    kpjba
015800160125     c                   movel     kpjbu         trul33ds
015900160125     C                   ENDSR
016000020205     C**-------------------------------------------------------------**
016100000607     c     *inzsr        begsr
016200060125     C**-------------------------------------------------------------**
016300000607     c     *entry        plist
016400020205     c                   parm                    kpjba
016500030429      *----------*
016600030429     C* GIRO DATA ODIERNA
016700030429     C                   TIME                    W0140            14 0
016800030429     C                   MOVE      W0140         UDATE8            8 0
016900160126     C                   MOVEl     W0140         oracor            6 0
017000060125     C     *eur          movel     UDATE8        dataoggi
017100160201     c                   move      dataoggi      data_gg1
017200160201     c                   move      data_gg1      oggi              8 0
017300060125     C     dataoggi      subdur    1:*d          data_gg1
017400160201     c                   move      data_gg1      ieri              8 0
017500060125      *
017600160125     C* Definisco chiavi di accesso
017700160125     C     Ktbe          KLIST
017800160126     C                   KFLD                    xtbecod
017900160126     C                   KFLD                    tbelin
018000160126     C                   KFLD                    tbesif
018100160126     C                   KFLD                    xtbeke1
018200160126     C                   move      'TCE'         xtbecod
018300071017      *
018400020402     c*
018500020402     c                   endsr
