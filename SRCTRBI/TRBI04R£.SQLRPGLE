000100990504     H DEBUG DECEDIT('0,') DATEDIT(*DMY/)
000200060125      *****************************************************************
000300161025      * Il pgm scrive i record di valorizzazione autisti per giornata
000400060125      *
000500060125      *****************************************************************
000600160125     FTntbe01l  IF   E           K DISK
000700161025     Ffitgt01l  IF   E           K DISK
000800161025     Ffifpt02l  IF   E           K DISK
000900161025     Fbitvl00f  o    E             DISK
001000030429     F*---------------------------------------------------------------------------------
001100000607     D kpjba         e ds
001200161026     dparam            ds
001300161026     d datap                          8  0
001400060125     D fiftt         E DS                  extname(fiftt00f)
001500161025     D Dboi          E DS
001600161025     D Dbon          E DS
001700161007     D Dmga          E DS
001800160125     D Dtce          E DS
001900030429      *
002000060125     d dataoggi        s               d   datfmt(*eur)
002100060125     d data_gg1        s               d   datfmt(*iso)
002200161025     d tmp             s             17
002300161025     d exfttfgs        s              3  0
002400161025     d exfttddc        s              8  0
002500020205     c**********************************************************************
002600160126     C*
002700161026     c* Lettura fifTT00F con data zero quindi tutto lo sflaggato
002800161025     c                   if        datap = 0
002900161026     C/EXEC SQL
003000161026     C+ DECLARE A1 CURSOR FOR SELECT * from fiftt00f where
003100161026     C+  fttfvl = 'C' and fttftr = ' ' and ftttsr = ' '
003200161026     C+  order by fttpdr
003300161026     C/END-EXEC
003400020328
003500020328     C/EXEC SQL
003600020328     C+ OPEN A1
003700020328     C/END-EXEC
003800020328     C                   DOU       SqlCod <> 0
003900020328     C/EXEC SQL
004000161025     C+ FETCH NEXT FROM A1 INTO :fiftt
004100020328     C/END-EXEC
004200020328     C                   SELECT
004300060125     c*
004400020328     C                   WHEN      SqlCod = 0
004500161026     c                   clear                   bitvl000
004600161026      * verifica attivazioni aggiunte tariffe per filiale
004700161026     c                   if        fttfgs <> exfttfgs or
004800161026     c                             fttddc <> exfttddc
004900161026     c                   exsr      RepFilAtt
005000161026     c                   z-add     fttfgs        exfttfgs
005100161026     c                   z-add     fttddc        exfttddc
005200161026     c                   endif
005300161025      *Reperimento dati tariffa autista
005400161025     c                   exsr      RepTipTar
005500161025      *
005600161025     c                   exsr      scriviTvl
005700060125      **
005800060125      * ? EoF
005900020328     C                   WHEN      SqlCod = 100
006000160126     c                   leave
006100060125      **
006200060125      * ? Errori
006300020328     C                   WHEN      SqlCod <> 0
006400160126     c                   leave
006500060125      **
006600020328     C                   ENDSL
006700060125      **
006800020328     C                   ENDDO
006900020328     C/EXEC SQL
007000020328     C+ CLOSE A1
007100020328     C/END-EXEC
007200161026     c* Lettura fifTT00F con data quindi seleziono tutto il convalidato di quella data
007300161026     c                   else
007400161026     C/EXEC SQL
007500161026     C+ DECLARE B1 CURSOR FOR SELECT * from fiftt00f where
007600161026     C+  fttfvl = 'C' and fttdcv = :datap and ftttsr = ' '
007700161026     C+  order by fttpdr
007800161026     C/END-EXEC
007900161026     C/EXEC SQL
008000161026     C+ OPEN B1
008100161026     C/END-EXEC
008200161026     C                   DOU       SqlCod <> 0
008300161026     C/EXEC SQL
008400161026     C+ FETCH NEXT FROM B1 INTO :fiftt
008500161026     C/END-EXEC
008600161026     C                   SELECT
008700161026     c*
008800161026     C                   WHEN      SqlCod = 0
008900161026     c                   clear                   bitvl000
009000161026      * verifica attivazioni aggiunte tariffe per filiale
009100161026     c                   if        fttfgs <> exfttfgs or
009200161026     c                             fttddc <> exfttddc
009300161026     c                   exsr      RepFilAtt
009400161026     c                   z-add     fttfgs        exfttfgs
009500161026     c                   z-add     fttddc        exfttddc
009600161026     c                   endif
009700161026      *Reperimento dati tariffa autista
009800161026     c                   exsr      RepTipTar
009900161026      *
010000161026     c                   exsr      scriviTvl
010100161026      **
010200161026      * ? EoF
010300161026     C                   WHEN      SqlCod = 100
010400161026     c                   leave
010500161026      **
010600161026      * ? Errori
010700161026     C                   WHEN      SqlCod <> 0
010800161026     c                   leave
010900161026      **
011000161026     C                   ENDSL
011100161026      **
011200161026     C                   ENDDO
011300161026     C/EXEC SQL
011400161026     C+ CLOSE B1
011500161026     C/END-EXEC
011600161026     c                   endif
011700030429      *
011800000607     c                   seton                                        lr
011900161025      * ?___________________________________________________________________
012000161025     c     RepFilAtt     begsr
012100161025      * ?___________________________________________________________________
012200161025     c                   eval      TVLACBON = 'N'
012300161025     c                   eval      TVLACBOI = 'N'
012400161025     c                   eval      TVLACMGA = 'N'
012500161025     c                   clear                   dmga
012600161025     c                   clear                   dbon
012700161025     c                   clear                   dboi
012800161025     C                   movel     fttfgs        tbeke1
012900161025     C                   move      'MGA'         tbecod
013000161025     c     ktbe          chain     tntbe01l
013100161025     c                   if        %found(tntbe01l)
013200161025     c                   movel     tbeuni        dmga
013300161025     c                   if        §mgadti <= fttddc
013400161025     c                   eval      TVlacmga = 'S'
013500161025     c                   endif
013600161025     c                   endif
013700161025      *
013800161025     C                   move      'BON'         tbecod
013900161025     c     ktbe          chain     tntbe01l
014000161025     c                   if        %found(tntbe01l)
014100161025     c                   movel     tbeuni        dbon
014200161025     c                   if        §bondti <= fttddc
014300161025     c                   eval      TVlacbon = 'S'
014400161025     c                   endif
014500161025     c                   endif
014600161025      *
014700161025     C                   move      'BOI'         tbecod
014800161025     c     ktbe          chain     tntbe01l
014900161025     c                   if        %found(tntbe01l)
015000161025     c                   movel     tbeuni        dboi
015100161025     c                   if        §boidti <= fttddc
015200161025     c                   eval      TVlacboi = 'S'
015300161025     c                   endif
015400161025     c                   endif
015500161025      *
015600161025     c                   endsr
015700161025      * ?___________________________________________________________________
015800161025     c     RepTipTar     begsr
015900161025      * ?___________________________________________________________________
016000161025     c                   eval      TVLTPTAR = 'S'
016100161025      *
016200161025     c                   eval        TGTPDR = fttpdr
016300161025     c                   eval        TGTSML = ' '
016400161025      *
016500161025     c                   move      'N'           stessa_tariffa    1
016600161025      *
016700161025     c     ktgt          setll     fiTGT01l
016800161025     c     ktgt          reade     fiTGT01l
016900161025     c                   dow       not %EoF(fiTGT01l)
017000161025     c                   if        tgtDDT <= fttddc and
017100161025     c                             tgtDST >= fttddc
017200161025      * Se utilizzata un'unica tariffa all'interno del periodo selezionato
017300161025     c                   move      ' '           stessa_tariffa
017400161025     c                   leave
017500161025     c                   end
017600161025     c     ktgt          reade     fiTGT01l
017700161025     c                   end
017800161025      *
017900161025      *   Se i calcoli sono basati sulla stessa tariffa,
018000161025      *  Controllo se ci sono tariffe a Giornata
018100161025     c                   move      'N'           No_Tariffa        1
018200161025      *
018300161025     c                   if          stessa_tariffa = 'N'
018400161025     c                   move      ' '           No_Tariffa
018500161025      *
018600161025     c                   elseIf      stessa_tariffa = ' '
018700161025      *
018800161025     c                   eval           fPTPDR  = TGTpdr
018900161025     c                   eval           fPTSML  = ' '
019000161025      *
019100161025      *  Nell'ambito dello stesso progressivo
019200161025      *   Quali tariffe ci sono a giornata ?
019300161025     c     kfpt          setll     fifPT02l
019400161025     c     kfpt          reade     fifPT02l
019500161025      *
019600161025     c                   dow       not %EoF(fifPT02l)
019700161025      *
019800161025      *  solo per il progressivo
019900161025     c                   if             fPTPRG  = TGTprg
020000161025      * Tariffa non trovata in assoluto perchè eliminata
020100161025     c                   move      ' '           No_Tariffa
020200161025      *
020300161025      *   solo per tariffe a giornata
020400161025     c                   if        fPTTSR  = 'G' and fPTCTR  = 999
020500161025     c                   if        fptCTD = 'TIG'
020600161025     c                   z-add     fptATA        tvltig
020700161025     c                   eval      TVLTPTAR = 'N'
020800161025      *
020900161025     c                   elseIf    fptCTD = 'TMG'
021000161025     c                   z-add     fptATA        tvlTMG
021100161025     c                   eval      TVLTPTAR = 'N'
021200161025      *
021300161025     c                   elseIf    fptCTD = 'MGA'
021400161025     c                   z-add     fptATA        tvlMGA
021500161025      *
021600161025     c                   elseIf    fptCTD = 'BON'
021700161025     c                   z-add     fptATA        tvlBON
021800161025      *
021900161025     c                   end
022000161025      *
022100161025     c                   end
022200161025     c                   end
022300161025      *
022400161025     c     kfpt          reade     fifPT02l
022500161025     c                   end
022600161025      *
022700161025     c                   end
022800161025     c                   eval      tvlnotar = no_tariffa
022900161025     c                   endsr
023000161025     C*_____________________________________________________________________
023100161025     C** scrittura testate valorizzazioni
023200161025     C*_____________________________________________________________________
023300161025     C     scriviTvl     BEGSR
023400161026     c                   eval      TVLFGS   =    fttFGS
023500161026     c                   eval      TVLDFV   =    fttDdc
023600161026     c                   eval      TVLPDR   =    fttPDR
023700161026     c                   eval      TVLBONF  =    §bonimp
023800161026     c                   eval      TVLTBN    =   fttTBN
023900161026     c                   eval      TVLMNT    =   fttMNT
024000161026     c                   eval      TVLITT    =   fttITT
024100161026     c                   eval      TVLITA    =   fttITA
024200161025     c                   eval      tvldatora =   Tmp
024300161025     c
024400161025     c
024500161025     c                   write     bitvl000
024600160125     C                   endsr
024700020205     C**-------------------------------------------------------------**
024800000607     c     *inzsr        begsr
024900060125     C**-------------------------------------------------------------**
025000000607     c     *entry        plist
025100020205     c                   parm                    kpjba
025200161026     c                   if        kpjbu = *blank
025300161026     c                   clear                   param
025400161026     c                   else
025500161026     c                   movel     kpjbu         param
025600161026     c                   endif
025700030429      *----------*
025800030429     C* GIRO DATA ODIERNA
025900030429     C                   TIME                    W0140            14 0
026000030429     C                   MOVE      W0140         UDATE8            8 0
026100160126     C                   MOVEl     W0140         oracor            6 0
026200060125     C     *eur          movel     UDATE8        dataoggi
026300160201     c                   move      dataoggi      data_gg1
026400160201     c                   move      data_gg1      oggi              8 0
026500060125     C     dataoggi      subdur    1:*d          data_gg1
026600160201     c                   move      data_gg1      ieri              8 0
026700060125      *
026800160125     C* Definisco chiavi di accesso
026900160125     C     Ktbe          KLIST
027000161025     C                   KFLD                    tbecod
027100161025     C                   KFLD                    tbeke1
027200161025      *
027300161025     c     ktgt          klist
027400161025     c                   kFLD                    TGTPDR
027500161025     c                   kFLD                    TGTSML
027600161025      *
027700161025     c     kfpt          klist
027800161025     c                   kFLD                    fPTPDR
027900161025     c                   kFLD                    fPTSML
028000020402     c*
028100020402     c                   endsr
