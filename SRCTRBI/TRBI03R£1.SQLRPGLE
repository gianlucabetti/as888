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
001400170330     d datap2                         8  0
001500060125     D fiftt         E DS                  extname(fiftt00f)
001600161025     D Dboi          E DS
001700161025     D Dbon          E DS
001800161007     D Dmga          E DS
001900160125     D Dtce          E DS
002000030429      *
002100060125     d dataoggi        s               d   datfmt(*eur)
002200060125     d data_gg1        s               d   datfmt(*iso)
002300161025     d tmp             s             17
002400161025     d exfttfgs        s              3  0
002500161025     d exfttddc        s              8  0
002600161026     d cmga            s              1
002700161026     d cbon            s              1
002800161026     d cboi            s              1
002900020205     c**********************************************************************
003000160126     C*
003100161026     c* Lettura fifTT00F con data zero quindi tutto lo sflaggato
003200161025     c                   if        datap = 0
003300161026     C/EXEC SQL
003400161026     C+ DECLARE A1 CURSOR FOR SELECT * from fiftt00f where
003500161026     C+  fttfvl = 'C' and fttftr = ' ' and ftttsr = ' '
003600161026     C+  order by fttpdr
003700161026     C/END-EXEC
003800020328
003900020328     C/EXEC SQL
004000020328     C+ OPEN A1
004100020328     C/END-EXEC
004200020328     C                   DOU       SqlCod <> 0
004300020328     C/EXEC SQL
004400161025     C+ FETCH NEXT FROM A1 INTO :fiftt
004500020328     C/END-EXEC
004600020328     C                   SELECT
004700060125     c*
004800020328     C                   WHEN      SqlCod = 0
004900161026     c                   clear                   bitvl000
005000161026      * verifica attivazioni aggiunte tariffe per filiale
005100161026     c                   if        fttfgs <> exfttfgs or
005200161026     c                             fttddc <> exfttddc
005300161026     c                   exsr      RepFilAtt
005400161026     c                   z-add     fttfgs        exfttfgs
005500161026     c                   z-add     fttddc        exfttddc
005600161026     c                   endif
005700161025      *Reperimento dati tariffa autista
005800161025     c                   exsr      RepTipTar
005900161025      *
006000161025     c                   exsr      scriviTvl
006100060125      **
006200060125      * ? EoF
006300020328     C                   WHEN      SqlCod = 100
006400160126     c                   leave
006500060125      **
006600060125      * ? Errori
006700020328     C                   WHEN      SqlCod <> 0
006800160126     c                   leave
006900060125      **
007000020328     C                   ENDSL
007100060125      **
007200020328     C                   ENDDO
007300020328     C/EXEC SQL
007400020328     C+ CLOSE A1
007500020328     C/END-EXEC
007600161028     c* Lettura fifTT00F con data quindi seleziono tutte le distinte di quella data
007700161028     c* che abbiano il flag di trasmesso diverso da blank
007800161026     c                   else
007900170420      * ?effettua una delete preventiva per non scrivere rekord doppi nel periodo da elaborare
008000170420     C/EXEC SQL
008100170420     C+ delete from bitvl00F where fttddc between :datap and :datap2
008200170420     C/END-EXEC
008300170420      *
008400161026     C/EXEC SQL
008500161026     C+ DECLARE B1 CURSOR FOR SELECT * from fiftt00f where
008600170330     C+  fttfvl = 'C'  and fttftr <> ' ' and ftttsr = ' ' and
008700170330     C+  fttddc between :datap and :datap2
008800161026     C+  order by fttpdr
008900161026     C/END-EXEC
009000161026     C/EXEC SQL
009100161026     C+ OPEN B1
009200161026     C/END-EXEC
009300161026     C                   DOU       SqlCod <> 0
009400161026     C/EXEC SQL
009500161026     C+ FETCH NEXT FROM B1 INTO :fiftt
009600161026     C/END-EXEC
009700161026     C                   SELECT
009800161026     c*
009900161026     C                   WHEN      SqlCod = 0
010000161026     c                   clear                   bitvl000
010100161026      * verifica attivazioni aggiunte tariffe per filiale
010200161026     c                   if        fttfgs <> exfttfgs or
010300161026     c                             fttddc <> exfttddc
010400161026     c                   exsr      RepFilAtt
010500161026     c                   z-add     fttfgs        exfttfgs
010600161026     c                   z-add     fttddc        exfttddc
010700161026     c                   endif
010800161026      *Reperimento dati tariffa autista
010900161026     c                   exsr      RepTipTar
011000161026      *
011100161026     c                   exsr      scriviTvl
011200161026      **
011300161026      * ? EoF
011400161026     C                   WHEN      SqlCod = 100
011500161026     c                   leave
011600161026      **
011700161026      * ? Errori
011800161026     C                   WHEN      SqlCod <> 0
011900161026     c                   leave
012000161026      **
012100161026     C                   ENDSL
012200161026      **
012300161026     C                   ENDDO
012400161026     C/EXEC SQL
012500161026     C+ CLOSE B1
012600161026     C/END-EXEC
012700161026     c                   endif
012800030429      *
012900000607     c                   seton                                        lr
013000161025      * ?___________________________________________________________________
013100161025     c     RepFilAtt     begsr
013200161025      * ?___________________________________________________________________
013300161026     c                   eval      CBON = 'N'
013400161026     c                   eval      CBOI = 'N'
013500161026     c                   eval      CMGA = 'N'
013600161025     c                   clear                   dmga
013700161025     c                   clear                   dbon
013800161025     c                   clear                   dboi
013900161025     C                   movel     fttfgs        tbeke1
014000161025     C                   move      'MGA'         tbecod
014100161025     c     ktbe          chain     tntbe01l
014200161025     c                   if        %found(tntbe01l)
014300161025     c                   movel     tbeuni        dmga
014400161025     c                   if        §mgadti <= fttddc
014500161026     c                   eval      cmga = 'S'
014600161026     c                   else
014700161026     c                   clear                   dmga
014800161025     c                   endif
014900161025     c                   endif
015000161025      *
015100161025     C                   move      'BON'         tbecod
015200161025     c     ktbe          chain     tntbe01l
015300161025     c                   if        %found(tntbe01l)
015400161025     c                   movel     tbeuni        dbon
015500161025     c                   if        §bondti <= fttddc
015600161026     c                   eval      cbon = 'S'
015700161026     c                   else
015800161026     c                   clear                   dbon
015900161025     c                   endif
016000161025     c                   endif
016100161025      *
016200161025     C                   move      'BOI'         tbecod
016300161025     c     ktbe          chain     tntbe01l
016400161025     c                   if        %found(tntbe01l)
016500161025     c                   movel     tbeuni        dboi
016600161025     c                   if        §boidti <= fttddc
016700161026     c                   eval      cboi = 'S'
016800161026     c                   else
016900161026     c                   clear                   dboi
017000161025     c                   endif
017100161025     c                   endif
017200161025      *
017300161025     c                   endsr
017400161025      * ?___________________________________________________________________
017500161025     c     RepTipTar     begsr
017600161025      * ?___________________________________________________________________
017700161025     c                   eval      TVLTPTAR = 'S'
017800161025      *
017900161025     c                   eval        TGTPDR = fttpdr
018000161025     c                   eval        TGTSML = ' '
018100161025      *
018200161025     c                   move      'N'           stessa_tariffa    1
018300161025      *
018400161025     c     ktgt          setll     fiTGT01l
018500161025     c     ktgt          reade     fiTGT01l
018600161025     c                   dow       not %EoF(fiTGT01l)
018700161025     c                   if        tgtDDT <= fttddc and
018800161025     c                             tgtDST >= fttddc
018900161025      * Se utilizzata un'unica tariffa all'interno del periodo selezionato
019000161025     c                   move      ' '           stessa_tariffa
019100161025     c                   leave
019200161025     c                   end
019300161025     c     ktgt          reade     fiTGT01l
019400161025     c                   end
019500161025      *
019600161025      *   Se i calcoli sono basati sulla stessa tariffa,
019700161025      *  Controllo se ci sono tariffe a Giornata
019800161025     c                   move      'N'           No_Tariffa        1
019900161025      *
020000161025     c                   if          stessa_tariffa = 'N'
020100161025     c                   move      ' '           No_Tariffa
020200161025      *
020300161025     c                   elseIf      stessa_tariffa = ' '
020400161025      *
020500161025     c                   eval           fPTPDR  = TGTpdr
020600161025     c                   eval           fPTSML  = ' '
020700161025      *
020800161025      *  Nell'ambito dello stesso progressivo
020900161025      *   Quali tariffe ci sono a giornata ?
021000161025     c     kfpt          setll     fifPT02l
021100161025     c     kfpt          reade     fifPT02l
021200161025      *
021300161025     c                   dow       not %EoF(fifPT02l)
021400161025      *
021500161025      *  solo per il progressivo
021600161025     c                   if             fPTPRG  = TGTprg
021700161025      * Tariffa non trovata in assoluto perchè eliminata
021800161025     c                   move      ' '           No_Tariffa
021900161025      *
022000161025      *   solo per tariffe a giornata
022100161025     c                   if        fPTTSR  = 'G' and fPTCTR  = 999
022200161025     c                   if        fptCTD = 'TIG'
022300161025     c                   z-add     fptATA        tvltig
022400161025     c                   eval      TVLTPTAR = 'N'
022500161025      *
022600161025     c                   elseIf    fptCTD = 'TMG'
022700161025     c                   z-add     fptATA        tvlTMG
022800161025     c                   eval      TVLTPTAR = 'N'
022900161025      *
023000161025     c                   elseIf    fptCTD = 'MGA'
023100161025     c                   z-add     fptATA        tvlMGA
023200161025      *
023300161025     c                   elseIf    fptCTD = 'BON'
023400161025     c                   z-add     fptATA        tvlBON
023500161025      *
023600161025     c                   end
023700161025      *
023800161025     c                   end
023900161025     c                   end
024000161025      *
024100161025     c     kfpt          reade     fifPT02l
024200161025     c                   end
024300161025      *
024400161025     c                   end
024500161025     c                   eval      tvlnotar = no_tariffa
024600161025     c                   endsr
024700161025     C*_____________________________________________________________________
024800161025     C** scrittura testate valorizzazioni
024900161025     C*_____________________________________________________________________
025000161025     C     scriviTvl     BEGSR
025100161026     c                   eval      TVLACBON = cbon
025200161026     c                   eval      TVLACBOI = cboi
025300161026     c                   eval      TVLACMGA = cmga
025400161028     c                   eval      TVLFGS   =  fttFGS
025500161028     c                   eval      TVLDFV   =  fttDdc
025600161028     c                   eval      TVLPDR   =  fttPDR
025700161028     c                   eval      TVLBONF  =  §bonimp
025800161028     c                   eval      TVLTBN   =  fttTBN
025900161028     c                   eval      TVLMNT   =  fttMNT
026000161028     c                   eval      TVLITT   =  fttITT
026100161028     c                   eval      TVLITA   =  fttITA
026200161104     c                   eval      tvltpe   =  ftttpe
026300161104     c                   eval      tvltim   =  ftttim
026400161122     c                   eval      tmp= %char(%timestamp: *iso0)
026500161025     c                   eval      tvldatora =   Tmp
026600161027      *
026700161025     c                   write     bitvl000
026800160125     C                   endsr
026900020205     C**-------------------------------------------------------------**
027000000607     c     *inzsr        begsr
027100060125     C**-------------------------------------------------------------**
027200000607     c     *entry        plist
027300020205     c                   parm                    kpjba
027400161026     c                   if        kpjbu = *blank
027500161026     c                   clear                   param
027600161026     c                   else
027700161026     c                   movel     kpjbu         param
027800161026     c                   endif
027900030429      *----------*
028000030429     C* GIRO DATA ODIERNA
028100030429     C                   TIME                    W0140            14 0
028200030429     C                   MOVE      W0140         UDATE8            8 0
028300160126     C                   MOVEl     W0140         oracor            6 0
028400060125     C     *eur          movel     UDATE8        dataoggi
028500160201     c                   move      dataoggi      data_gg1
028600160201     c                   move      data_gg1      oggi              8 0
028700060125     C     dataoggi      subdur    1:*d          data_gg1
028800160201     c                   move      data_gg1      ieri              8 0
028900060125      *
029000160125     C* Definisco chiavi di accesso
029100160125     C     Ktbe          KLIST
029200161025     C                   KFLD                    tbecod
029300161025     C                   KFLD                    tbeke1
029400161025      *
029500161025     c     ktgt          klist
029600161025     c                   kFLD                    TGTPDR
029700161025     c                   kFLD                    TGTSML
029800161027      *
029900161025     c     kfpt          klist
030000161025     c                   kFLD                    fPTPDR
030100161025     c                   kFLD                    fPTSML
030200020402     c*
030300020402     c                   endsr
030400161123      * ?------------------------------------------------------------------ */
030500161123      *?      X non bloccare in nessun caso il traduttore CLIENTI
030600161123      * ?------------------------------------------------------------------ */
030700161123     C     *pssr         BEGSR
030800161123     C
030900161123     c                   seton                                        LR
031000161123     c                   return
031100161123     C
031200161123     C                   ENDSR
031300161123      * ?------------------------------------------------------------------ */
