000100990504     H DEBUG DECEDIT('0,') DATEDIT(*DMY/)
000200060125      *****************************************************************
000300160126      * Il pgm scrive i record di rettifica per le filiali in cui è stato
000400161007      * attivato il MGA  per gli autisti
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
001500161007     D Dmga          E DS
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
002700161007     C                   move      'MGA'         tbecod
002800161007     c     'MGA'         setll     tntbe01l
002900160126     c                   do        *hival
003000161007     c     'MGA'         reade     tntbe01l
003100160126     c                   if        %eof(tntbe01l)
003200160126     c                   leave
003300160126     c                   endif
003400161007     c                   movel     tbeuni        dmga
003500160126     C                   move      'TCE'         tbecod
003600161007     c                   movel(p)  §mgacaus      xTbeke1
003700160126     c     ktbe          chain     tntbe02l
003800160126     c                   if        %found(tntbe02l)
003900160126     c                   movel     xtbeuni       dtce
004000160126     c                   endif
004100160126      *
004200160126     c                   move      *zeros        padmin
004300170111     c                   movel     tbeke1        padmin
004301180207     c                   z-add     padmin        padmax
004400180207     c                   z-add     9999          padmax
004600160127     c                   z-add     0             autisti
004700160127     c                   z-add     0             import
004800160126      *
004900060125     c* Lettura fifTT00F x FGS/PDR/DDC
005000160126     C/EXEC SQL
005100161007     C+ DECLARE A1 CURSOR FOR SELECT fttddc, count(*), sum(fttmnt)
005200160126     C+  FROM fiftt00f WHERE
005300160127     C+  fttfvl = 'C' and fttdcv <> 0 and ftttsr = ' ' and fttftr = ' '
005400161007     C+  and fttpdr between :padmin and :padmax  and fttmnt > 0
005500170111     C+  and fttddc >= :§mgadti and fttddc <= :§mgadtf
005600160128     C+  Group by fttddc
005700160126     C/END-EXEC
005800020328
005900020328     C/EXEC SQL
006000020328     C+ OPEN A1
006100020328     C/END-EXEC
006200020328     C                   DOU       SqlCod <> 0
006300020328     C/EXEC SQL
006400161007     C+ FETCH NEXT FROM A1 INTO :fttddc, :autisti, :import
006500020328     C/END-EXEC
006600020328     C                   SELECT
006700060125     c*
006800020328     C                   WHEN      SqlCod = 0
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
010200161010      * testo importo massimo applicabile
010300161010     c                   if        import <= §mgaimp
010400160127     c                   z-add     import        TNAIMP
010500161010     c                   else
010600161010     c                   z-add     §mgaimp       tnaimp
010700161010     c                   endif
010800161010      *
010900160126     c                   move      §tcTSR        TNATSR
011000160125     c                   z-add     10            tnafase
011100161007     c                   z-add     §mgafild      TNAFILd
011200161007     c                   move      §mgacaus      TNACAUS
011300160125     c                   move      fttddc        TNADTAc
011400160125     c                   move      knmus         TNAuten
011500160127     c                   eval      tnanota = 'Numero di autisti conteggiati '
011600160127     c                             + %editc(autisti:'Z')
011700160125     c                   write     fntna000
011800160125     c                   exsr      storicofasi
011900160125     c                   if        §tcece = 'S'
012000160125     c                   z-add     tnanric       rnanric
012100160125     c                   move      tnadtac       rnadtac
012200160125     c                   move      tnatsr        rnatsr
012300160125     c                   if        §tcecon = 'N'
012400160125      * scrittura record con importo negativo su filiale richiedente
012500160125     c                   eval      rnaimp = (tnaimp * -1)
012600160125     c                   move      tnafila       rnafil
012700160125     c                   write     fnrna000
012800160125     c                   endif
012900160125      * scrittura record con importo positivo su filiale addebito
013000160125     c                   if        §tctpa = 'C'
013100160125     c                   eval      rnaimp = tnaimp
013200160125     c                   move      tnafild       rnafil
013300160125     c                   write     fnrna000
013400160125     c                   endif
013500160125     c                   endif
013600160125     C                   endsr
013700160125     C**********************************************************************
013800160125     C** inserisce lo storico delle fasi
013900160125     C**********************************************************************
014000160125     C     storicofasi   BEGSR
014100160125     c                   z-add     tNANRIC       FNANRIC
014200160126     c                   move      tNAFILa       FNAFIL
014300160125     c                   move      tNAFASE       FNAFASE
014400160201     c                   move      tnadtaf       FNADTAAG
014500160125     c                   z-add     oracor        FNAORAAG
014600160125     c                   move      tNANOTA       FNANOTA
014700160125     c                   move      knmus         FNAUTE
014800160125     c                   write     fnfna000
014900160125     C                   endsr
015000160125     c**********************************************************************
015100160125     c* reperimento numero viaggio
015200160125     c**********************************************************************
015300160125     C     repnum        BEGSR
015400160125     c                   clear                   trul33ds
015500160125     c                   move      'L'           i33tla
015600160125     c                   z-add     90            i33cnu
015700160125     c                   z-add     1             i33num
015800160125     c                   movel     trul33ds      kpjbu
015900160125     c                   call      'TRUL33R'
016000160125     c                   PARM                    kpjba
016100160125     c                   movel     kpjbu         trul33ds
016200160125     C                   ENDSR
016300020205     C**-------------------------------------------------------------**
016400000607     c     *inzsr        begsr
016500060125     C**-------------------------------------------------------------**
016600000607     c     *entry        plist
016700020205     c                   parm                    kpjba
016800030429      *----------*
016900030429     C* GIRO DATA ODIERNA
017000030429     C                   TIME                    W0140            14 0
017100030429     C                   MOVE      W0140         UDATE8            8 0
017200160126     C                   MOVEl     W0140         oracor            6 0
017300060125     C     *eur          movel     UDATE8        dataoggi
017400160201     c                   move      dataoggi      data_gg1
017500160201     c                   move      data_gg1      oggi              8 0
017600060125     C     dataoggi      subdur    1:*d          data_gg1
017700160201     c                   move      data_gg1      ieri              8 0
017800060125      *
017900160125     C* Definisco chiavi di accesso
018000160125     C     Ktbe          KLIST
018100160126     C                   KFLD                    xtbecod
018200160126     C                   KFLD                    tbelin
018300160126     C                   KFLD                    tbesif
018400160126     C                   KFLD                    xtbeke1
018500160126     C                   move      'TCE'         xtbecod
018600071017      *
018700020402     c*
018800020402     c                   endsr
