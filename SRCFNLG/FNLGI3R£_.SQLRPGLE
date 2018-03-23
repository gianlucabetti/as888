000100160819     H*------------------------------------------------------------------------*
000200170522     H* Giacenze Righe senza RIAPERTURA x misurare i tempi in IMA e su IMG
000300160819     H*------------------------------------------------------------------------*
000400170512     ftitas30c  if   e           k disk
000500170323     fazorg01l  if   e           k disk
000600160822     FFNevb01L  IF   E           K DISK
000700160822     FTABEL00F  IF   E           K DISK    usropn
000800161005     Ftntbe01l  IF   E           K DISK
000900170519     FWFIMAG0F  O  a E             DISK
001000170519     FTNIMAG1L  uF a E           K DISK
001100170511     D*------------------
001200170511     D* PASSAGGIO PARAMETRI
001300170511     D*------------------
001400170512     D KPJBA         E DS
001500170512     D FNLGI2ds      E DS
001600170512     D tigcpDS       E DS                  extname(tigcp00f)
001700170512     D*
001800170512     D WrkSqlCmd       S           1024
001900170511     D*
002000160822     D codntc          S              3    DIM(250)
002100070629
002200160822     D ds2a          E DS
002300170424     D ds2g2         E DS
002400170323     D og150         E DS
002500170323     D fnlv55ds      E DS
002600170519     d xgiolavds     e ds
002700170518
002800160822     D WDAT8           DS
002900160822     D  DATADA                 1      8  0
003000160822     D  DATAA                  9     16  0
003100160822     D  GGL                   17     21  0
003200170518
003300160822     D tfptfa          DS
003400160822     D  p_tfp                  1      3  0
003500160822     D  p_tfa                  4      6  0
003600170518
003700160927     d waddgg          s              3  0
003800170518
003900161005     d ktcod           s                   like(tbecod) inz('FLG')
004000161005     d ktke1           s                   like(tbeke1)
004100160926     d Datasys         s               d   datfmt(*iso) inz(*sys)
004200170512     d Data_ISO        s               d   datfmt(*iso)
004300170519      **-- campi chiave
004400170519     d  data8          s              8  0
004500170519     d  fil3           s              3  0
004600170519      *
004700170519     D*______________________________________________
004800170518     D Contatori       DS
004900170518     D  bolla__BASE                   8  0
005000170518     D  colli__BASE                   8  0
005100170518     D  peso___BASE                  13  1
005200170518     D  volume_BASE                  13  3
005300170518     D  bolla__A                      8  0
005400170518     D  colli__A                      8  0
005500170518     D  peso___A                     13  1
005600170518     D  volume_A                     13  3
005700170518     D  bolla__B                      8  0
005800170518     D  colli__B                      8  0
005900170518     D  peso___B                     13  1
006000170518     D  volume_B                     13  3
006100170518     D  bolla__C                      8  0
006200170518     D  colli__C                      8  0
006300170518     D  peso___C                     13  1
006400170518     D  volume_C                     13  3
006500170518     D  bolla__DE                     8  0
006600170518     D  colli__DE                     8  0
006700170518     D  peso___DE                    13  1
006800170518     D  volume_DE                    13  3
006900170518     D  bolla__F                      8  0
007000170518     D  colli__F                      8  0
007100170518     D  peso___F                     13  1
007200170518     D  volume_F                     13  3
007300170518     D  bolla__G                      8  0
007400170518     D  colli__G                      8  0
007500170518     D  peso___G                     13  1
007600170518     D  volume_G                     13  3
007700170518     D  bolla__H                      8  0
007800170518     D  colli__H                      8  0
007900170518     D  peso___H                     13  1
008000170518     D  volume_H                     13  3
008100170518     D  bolla__I                      8  0
008200170518     D  colli__I                      8  0
008300170518     D  peso___I                     13  1
008400170518     D  volume_I                     13  3
008500170518     D  bolla__J                      8  0
008600170518     D  colli__J                      8  0
008700170518     D  peso___J                     13  1
008800170518     D  volume_J                     13  3
008900170518     D  bolla__K                      8  0
009000170518     D  colli__K                      8  0
009100170518     D  peso___K                     13  1
009200170518     D  volume_K                     13  3
009300170519      *
009400170512     C*---------------------------------------------------------------*
009500170512     C     *ENTRY        PLIST
009600170512     C                   PARM                    KPJBA
009700170512     C                   MOVEL     KPJBU         FNLGI2DS
009800170512     C                   SETON                                        LR
009900170512     C*---------------------------------------------------------------*
010000170512     c* Carico i codici di mancata consegna
010100170512     c                   eval      kcod='2A'
010200170512     c                   eval      codut=1
010300170512     c                   clear                   xx
010400170512     c                   open      tabel00f
010500170512      **
010600170512     c     ktab          setll     tabel00f
010700170512     c     ktab          reade     tabel00f
010800170512     c                   dow       not %eof(tabel00f)
010900170512     c                   eval      ds2a=tbluni
011000170512     c                   if        §2antc='S'  or (§2atip<>'§'
011100170512     c                             and §2atip<>'S')
011200170512     c                   add       1             xx                3 0
011300170512     c                   movel     tblkey        codntc(xx)
011400170512     c                   endif
011500170512     c     ktab          reade     tabel00f
011600170512     c                   enddo
011700170512     c
011800170512     c* chaino DS2G per il peso limite sopra il quale va subito in IMG
011900170512     c                   clear                   ds2g2
012000170512     c                   eval      kcod='2G'
012100170512     c                   eval      kkey='2       '
012200170512     c     ktab2         chain     tabel00f
012300170512     c                   if        %found(tabel00f)
012400170512     c                   eval      ds2g2=tbluni
012500170512     c                   else
012600170512     c                   eval      §2g2kgimg=50
012700170512     c                   endif
012800170512     c
012900170512     c                   close     tabel00f
013000170512     c
013100170518     c                   clear                   wfgs              3 0
013200170518     c                   clear                   picking           1
013300170512      *
013400170518     C* DEFINIZIONE CHIAVI -------------------------------------------------
013500170512     C     Ktab          KLIST
013600170512     C                   KFLD                    codut             1 0
013700170512     C                   KFLD                    kcod              2
013800170512      *
013900170512     C     Ktab2         KLIST
014000170512     C                   KFLD                    codut             1 0
014100170512     C                   KFLD                    kcod              2
014200170512     C                   KFLD                    kkey              8
014300170512      *
014400170512     C     Ktbe          KLIST
014500170512     C                   KFLD                    ktcod
014600170512     C                   KFLD                    ktke1
014700170512     C*
014800170512     C     Karrivo       KLIST
014900170512     C                   KFLD                    GCPAASa
015000170512     C                   KFLD                    GCPLNPa
015100170512     C                   KFLD                    GCPNRSa
015200170512     C                   KFLD                    GCPNSPa
015300170519      *
015400170519     C     KDataFil      KLIST
015500170526     C                   KFLD                    data8
015600170526     C                   KFLD                    fil3
015700170519      *
015800170512?     *--------------------------------------------------------------*
015900170512     C*   data di sistema
016000170512     c                   movel     datasys       dateu             8 0
016100170512     C*
016200170512?     *--------------------------------------------------------------*
016300170519     C/EXEC SQL
016400170519     C+ DELETE FROM WFIMAG0F
016500170519     C/END-EXEC
016600170519     C*
016700170512     C/EXEC SQL
016800170526     C+ DELETE FROM TNIMAG0F
016900170512     C/END-EXEC
017000170512     C*
017100170512     c                   clear                   almenoUno         1
017200170512     C*
017300170512     c* ---------------------
017400170512      **** con SQL lettura delle GIACENZE senza RIAPERTURA
017500170512     C                   EVAL      WrkSqlCmd ='SELECT * FROM tigcp00f '     +
017600170512     C                             ' WHERE GCPRIAP = '' '' and '            +
017700170512     C                             ' GCPAGC * 10000 + GCPMGC between '      +
017800170512     C                              %editc(I2DADATA:'X') + ' and '          +
017900170526     C                               %editc(I2ADATA:'X') + ' and ('         +
018000170526     C                             %editc(I2FILIALE:'X') + ' = 0 or '       +
018100170526     C                             %editc(I2FILIALE:'X') + ' = GCPLNAA) '   +
018200170526     C                             ' ORDER BY GCPAGC, GCPMGC '
018300170512     c* ---------------------
018400170512     C/EXEC SQL
018500170512     C+ PREPARE S1 FROM :WrkSqlCmd
018600170512     C/END-EXEC
018700170512
018800170512     C/EXEC SQL
018900170512     C+ DECLARE A1 CURSOR FOR S1
019000170512     C/END-EXEC
019100170512
019200170512     C/EXEC SQL
019300170512     C+ OPEN A1
019400170512     C/END-EXEC
019500170512      * Forzo la stampa del JOBLOG.
019600170512     C                   If        SqlCod < 0
019700170512     C                   CALL      'X66CHGJOB'
019800170512     C                   RETURN
019900170512     C                   End
020000170512      *>>>>>>>>>>>>>
020100170512     C                   DOU       SqlCod <> 0
020200170512     C/EXEC SQL
020300170512     C+ FETCH NEXT FROM A1 INTO :tigcpDS
020400170512     C/END-EXEC
020500170512     C                   SELECT
020600170512     c*
020700170512     c* a fine file Totali x rotture
020800170512     C                   WHEN      SqlCod = 100
020900170519     c                   if        almenouno ='S'
021000170512     c                   exsr      TOTals
021100170512     c                   else
021200170512     c                   end
021300170512     c*
021400170512     c                   leave
021500170512     **   ERRORI
021600170512     C                   WHEN      SqlCod < 0
021700170512     C                   seton                                        H1
021800170512     c                   goto      fine
021900170512     c*
022000170512     C                   OTHER
022100170512     c*
022200170512      ** calcola data ELABORAZIONE
022300170512     c                   exsr      calc_DATA
022400170512     c*
022500170512     c* controlla se il record di giacenza è da considerare
022600170519     c                   clear                   rec_OK            1
022700170512     c                   exsr      check_REC
022800170512      **
022900170512      **  Se il record è buono lo elabora in dettaglio
023000170519     c                   if          rec_OK <> 'N'
023100170518     c* imposta Date di riferimento per il calcolo
023200170518     c                   exsr      DATE_riferim
023300170512     c* DETTAGLIO
023400170512     c                   exsr      dettaglio
023500170519     c* record di controllo
023600170519     c                   exsr      controlla
023700170519     c* aggiorna il file di Statistica
023800170519     c                   exsr      aggiorna
023900170519     c                   move      'S'           almenouno
024000170512     c                   end
024100170512     c*
024200170512     C                   ENDSL
024300170512
024400170512     C                   ENDDO
024500170512      *>>>>>>>>>>>>>
024600170512     C/EXEC SQL
024700170512     C+ CLOSE A1
024800170512     C/END-EXEC
024900170512     c*
025000170512     C* totali generali
025100170519     c                   if        almenouno ='S'
025200170512     c                   end
025300170512      *
025400170512     c     fine          tag
025500170512     C                   Return
025600170512?     *--------------------------------------------------------------*
025700170512      * Calcola DATA Elaborazione
025800170512?     *--------------------------------------------------------------*
025900170512     c     Calc_DATA     begsr
026000170511      *
026100170518     c                   clear                   data_INIelab      8 0
026200170522     c                   clear                   data_OPNgiac      8 0
026300170523     c                   clear                   data_CLOgiac      8 0
026400170523     c                   clear                   data_EVEnto       8 0
026500170525     c                   clear                   data_MICelab      8 0
026600170512      *
026700170512      * intanto come prima cosa  imposta come DATA ELABORAZIONE
026800170512      *     la  DATA APERTURA GIACENZA
026900170522     c                   eval       data_INIelab =
027000170512     c                                    gcpAGC * 10000 + gcpMGC
027100170522     c                   eval      data_OPNgiac = data_INIelab
027200170523     c                   eval      data_CLOgiac =  GCPDCG
027300170512      **
027400170512      * poi bisogna vedere se l'EVENTO di MANCATA CONSEGNA faccia
027500170512      *  considerare diversamente la DATA ELABORAZIONE aggiungendo
027600170512      *   un giorno al calcolo.
027700170512      **
027800170512     c     karrivo       setgt     fnevb01l
027900170512     c     karrivo       readpe    fnevb01l
028000170512    1c                   dow       not %eof(fnevb01l)
028100170525      *
028200170525      * Prende l'ultimo MIC letto prima dell'eventuale stato di Mancata
028300170525      * consegna.
028400170525    3c                   if        evbCEV = 'MIC'
028500170525     c                   eval        data_MICelab =evbDEV
028600170525    5c                   endif
028700170512
028800170512    2c                   if        %lookup(evbcev:codntc)>0
028900170512     c                             and %subst(evbnot:8:7)>*zeros
029000170525      *-
029100170512     c* trovato evento di mancata consegna --> verifico la data rispetto
029200170512     c*  alla data giacenza
029300170518    3c                   if        data_INIelab  = evbDEV
029400170523    3c                   eval      data_EVEnto = evbDEV
029500170523     c*  +1gg. lavorativo
029600170523     C                   eval      fil3  =  gcpLNAA
029700170523     c                   clear                   xgiolavds
029800170523     c                   eval      IXGLDATA = data_INIelab
029900170523     c                   eval      IXGLfil  = fil3
030000170523     c                   eval      IXGLpa   ='A'
030100170525     c                   eval      IXGLadd  ='S'
030200170523     c                   eval      IXGLgg   = 1
030300170523     c                   eval      IXGLlav  ='S'
030400170523     c                   call      'XGIOLAV'
030500170523     c                   parm                    xgiolavds
030600170523    5c                   if        oxglerr=' '
030700170523     c                   eval      data_INIelab = oxgldata
030800170523    5c                   endif
030900170524      **
031000170524     c                   leave
031100170512    3c                   endif
031200170523      **
031300170512    2c                   endif
031400170512     c
031500170512     c     karrivo       readpe    fnevb01l
031600170512    1c                   enddo
031700170512      *
031800170512     c                   endsr
031900170512?     *--------------------------------------------------------------*
032000170512      * Check se l record letto è da scartare
032100170512?     *--------------------------------------------------------------*
032200170512     c     Check_REC     begsr
032300170512      *****
032400170512      * (1)
032500170512      *****    Se fosse presente un periodo di sospensione e la data
032600170512      *  ELABORAZIONE fosse compresa nell'intervallo -> il RECORD è da SCARTARE
032700170512     c                   if            §2G2DIMAi >  0  and
032800170518     c                             data_INIelab  >= §2G2DIMAi and
032900170518     c                             data_INIelab  <= §2G2DIMAf
033000170512     c                   eval             rec_OK = 'N'
033100170512     c                   leavesr
033200170512    1c                   endif
033300170512      *****
033400170512      * (2)
033500170512      *****    Se la Filiale NON ha il PICKING -> il RECORD è da SCARTARE
033600170512     c                   exsr      FIL_fa_il_Pick
033700170512     c                   if        picking = 'N'
033800170512     c                   eval             rec_OK = 'N'
033900170512     c                   leavesr
034000170512    1c                   endif
034100170512      *****
034200170512      * (3)
034300170512      *****    Alcune Filiali non hanno IMA -> il RECORD è da SCARTARE
034400170512     c* prima controllo lnp poi ccm
034500170512     c                   eval      ktke1=%editc(GCPLNPA:'X')
034600170512     c     ktbe          chain     tntbe01l
034700170512     c                   if        not %found(tntbe01l) or tbeatb<>' '
034800170512     c* alcuni clienti vanno subito a img
034900170512     c                   eval      ktke1=%editc(GCPSCM:'X')
035000170512     c     ktbe          chain     tntbe01l
035100170512     c                   endif
035200170512     c                   if        %found(tntbe01l) and tbeatb=' '
035300170512     c                   eval             rec_OK = 'N'
035400170512     c                   leavesr
035500170512    1c                   endif
035600170512      *****
035700170512      * (4)
035800170512      *****    Se il peso superiore alla 2G -> il RECORD è da SCARTARE
035900170512     c                   clear                   Peso_Sped         7 1
036000170518     c                   clear                   Vol_Sped          5 3
036100170512     c     karrivo       chain     titas30c
036200170512      **
036300170512    1c                   if        %found(titas30c)
036400170518      * prende il peso
036500170518     c                   eval      Peso_Sped  = taspkf
036600170518     c                   if          taspkc > 0 and tasncp = tasncl
036700170518     c                                or
036800170518     c                               taspkc > 0 and tasncp < tasncl
036900170518     c                               and taspkc > taspkf
037000170512     c                   eval      Peso_Sped  = taspkc
037100170518     c                   end
037200170518      ** se supera esce
037300170518    1c                   if        Peso_Sped > §2g2kgimg
037400170518     c                   eval             rec_OK = 'N'
037500170518     c                   leavesr
037600170518    1c                   endif
037700170518     **
037800170518      * prende il volume
037900170518     c                   eval      Vol_Sped  = tasvlf
038000170518     c                   if          tasvlc > 0 and tasncr = tasncl
038100170518     c                                or
038200170518     c                               tasvlc > 0 and tasncr < tasncl
038300170518     c                               and tasvlc > tasvlf
038400170518     c                   eval      Vol_Sped  = tasvlc
038500170518     c                   end
038600170518      **
038700170512    1c                   endif
038800170512      *
038900170512     c                   endsr
039000170512?     *--------------------------------------------------------------*
039100170512     C     FIL_fa_il_PickBEGSR
039200170512?     *--------------------------------------------------------------*
039300170512      **  La Filiale GESTISCE il PICKING?
039400170512      *
039500170512     c                   clear                   picking
039600170512     c                   movel     GCPFGC        wfgs              3 0
039700170512      * tramite Terminal
039800170512     c                   clear                   fnlv55ds
039900170512     c                   eval      d55lin=GCPFGC
040000170512     c                   eval      d55tpt='6'
040100170512     c                   movel     dateu         d55drf
040200170512     c                   call      'FNLV55R'
040300170512     c                   parm                    fnlv55ds
040400170512     c                   movel     d55tfa        wfgs
040500170512     c
040600170512     c     wfgs          chain     azorg01l
040700170512     c                   movel     orgdf0        og150
040800170512     c                   eval      picking=§OGPCK
040900170512     c                   if        picking=' '
041000170512     c                   eval      picking='N'
041100170512     c                   endif
041200170512     c*
041300170512     c                   ENDSR
041400170512?     *--------------------------------------------------------------*
041500170518      * imposta le date di RIFERIMENTO
041600170512?     *--------------------------------------------------------------*
041700170518     c     DATE_riferim  begsr
041800170512      *
041900170518      *------------
042000170518      ** calcola data FINE ELABORAZIONE
042100170518     c                   clear                   data_FINEelab     8 0
042200170522     c                   clear                   data_Consegna     8 0
042300170518      *
042400170518      *  chiamo data fine calcolo, la data chiusura giacenza se PRG <>0,
042500170518      *        altrimenti DCM da ARB se consegnata
042600170518     c                   if        gcpFRG > 0
042700170525     c                              or
042800170525     c                             tasCCA <> *blank
042900170518     c                   eval      data_FINEelab = GCPDCG
043000170518     c                   else
043100170523      *
043200170518      ** se c'è la data Consegna su TITAS
043300170518     c                   if        tasDCM > 0
043400170525      *
043500170518     c                   eval      data_FINEelab = tasDCM
043600170522     c                   eval      data_Consegna = tasDCM
043700170523     **
043800170523     *** toglie   1gg. per i calcoli a seguire
043900170523     C                   eval      fil3  =  gcpLNAA
044000170523     c                   clear                   xgiolavds
044100170523     c                   eval      IXGLDATA = data_FINEelab
044200170523     c                   eval      IXGLfil  = fil3
044300170523     c                   eval      IXGLpa   ='A'
044400170525     c                   eval      IXGLsub  ='S'
044500170523     c                   eval      IXGLgg   = 1
044600170523     c                   eval      IXGLlav  ='S'
044700170523     c                   call      'XGIOLAV'
044800170523     c                   parm                    xgiolavds
044900170523    5c                   if        oxglerr=' '
045000170523     c                   eval      data_FINEelab = oxgldata
045100170523    5c                   endif
045200170518     c                   end
045300170518      *
045400170525     c                   endif
045500170518      *------------
045600170518      ** data ENTRATA MAGAZZINO GIACENZE
045700170518     c                   clear                   data_ENTR_IMG     8 0
045800170518      *
045900170518     c                   if         GCPDLMA > 0 and  GCPDLMA<99999999
046000170518     c                   eval      data_ENTR_IMG = GCPDLMA
046100170518     c                   end
046200170518      * >>>>>>>>>>>>
046300170518      *
046400170518     c                   ENDSR
046500170512?     *--------------------------------------------------------------*
046600170525      * DATA_con_MIC
046700170512?     *--------------------------------------------------------------*
046800170525     c     DATA_con_MIC  begsr
046900170518      *
047000170525      * sottra 1 gg. data Fine Calcolo
047100170525     c                   clear                   xgiolavds
047200170525     c                   eval      IXGLDATA = data_FINEelab
047300170525     c                   eval      IXGLfil  = fil3
047400170525     c                   eval      IXGLpa   ='A'
047500170525     c                   eval      IXGLsub  ='S'
047600170525     c                   eval      IXGLgg   = 1
047700170525     c                   eval      IXGLlav  ='S'
047800170525     c                   call      'XGIOLAV'
047900170525     c                   parm                    xgiolavds
048000170525      *
048100170525    5c                   if        oxglerr=' '
048200170525    5c                               and
048300170525     c                             data_INIelab <= oxgldata
048400170525     c                   eval      data_FINEelab = oxgldata
048500170525    5c                   endif
048600170525      *
048700170525     c                   ENDSR
048800170525?     *--------------------------------------------------------------*
048900170525      * Dettaglio
049000170525?     *--------------------------------------------------------------*
049100170525     c     Dettaglio     begsr
049200170525      *
049300170525      *---------------- ------------------------ -----------------
049400170518      *  se si devono fare dei TOTALI
049500170518      *     a rottura di qualche chiave .... lo si deve fare qui
049600170518      *       e a EoF.
049700170518     c                   exsr      TOTals
049800170518      *---------------- ------------------------ -----------------
049900170518      *  Pulisce i contatori x la singola riga
050000170518     c                   clear                   contatori
050100170518      *
050200170518     c                   z-add     1             bolla__BASE
050300170518     c                   z-add     TASncl        colli__BASE
050400170518     c                   z-add     Peso_Sped     peso___BASE
050500170518     c                   z-add     Vol_Sped      volume_BASE
050600170518      *
050700170518      *  SE NON ENTRATO in IMG quindi ANCORA in IMA
0508001705180    c                   IF          data_ENTR_IMG = 0
050900170518      *
051000170518      *  Se data di FINE non c'è
051100170518      **     oltre 2 gg. NON essendoci la data di FINE
0512001705181    c                   if        data_FINEelab = 0
051300170518      **   '"> 2 IMA"
051400170518     c                   z-add     bolla__BASE   bolla__DE
051500170518     c                   z-add     colli__BASE   colli__DE
051600170518     c                   z-add     peso___BASE   peso___DE
051700170518     c                   z-add     volume_BASE   volume_DE
051800170518      *
0519001705181x   c                   else
052000170525      **
052100170525      ** attenzione se c'era un MIC contestuale alla data FINE
052200170525     c                   if        data_FINEelab = data_MICelab
052300170525     c                              and
052400170525     c                             data_FINEelab > data_INIelab
052500170525      ** sottraggo 1gg.data fine
0526001705251x   c                   exsr      DATA_con_MIC
052700170525     c                   else
052800170525     c                   eval      data_MICelab = 0
052900170525     c                   end
053000170525      **
053100170525      * calcola sempre quindi i giorni trascorsi
053200170525     c                   clear                   wdat8
053300170525     c                   clear                   tfptfa
053400170525     c                   movel     data_INIelab  datada
053500170525     c                   movel     data_FINEelab dataa
053600170525     c                   z-add     GCPLNAA       p_tfa
053700170525     c                   call      'XSRLAV8'
053800170525     c                   parm                    wdat8
053900170525     c                   parm                    tfptfa
054000170525      **
054100170518      * Con data FINE
054200170518      *  Se inizia e finisce nello stesso giorno
054300170523      *  o causa evento (ha aggiunto un giorno) quindi il giorno prima
0544001705232    c                   if        data_FINEelab <= data_INIelab
054500170525     c                               and
054600170525     c                             data_Consegna > 0
054700170518      * "risolte subito"
054800170518     c                   z-add     bolla__BASE   bolla__A
054900170518     c                   z-add     colli__BASE   colli__A
055000170518     c                   z-add     peso___BASE   peso___A
055100170518     c                   z-add     volume_BASE   volume_A
0552001705182x   c                   else
055300170518      **  se 1 gg.
055400170518      **  "+IMA -IMG" (1° gg)
055500170518     c                   if        GGL = 1
055600170518     c                   z-add     bolla__BASE   bolla__B
055700170518     c                   z-add     colli__BASE   colli__B
055800170518     c                   z-add     peso___BASE   peso___B
055900170518     c                   z-add     volume_BASE   volume_B
056000170518      **  se 2 gg.
056100170518      **  "+IMA -IMG" (2° gg)
056200170518     c                   elseif    GGL = 2
056300170518     c                   z-add     bolla__BASE   bolla__C
056400170518     c                   z-add     colli__BASE   colli__C
056500170518     c                   z-add     peso___BASE   peso___C
056600170518     c                   z-add     volume_BASE   volume_C
056700170518      **  se oltre 2 gg.
056800170518      **  "> 2 IMA" ciò si verifica per 2 motivi:
056900170518      **  1.  DLO accetta di non portare in IMG se già immesse dispo in ARRIVO.
057000170518      **  2.  "Anomalia di procedura".
057100170518     c                   elseif    GGL > 2
057200170518     c                   z-add     bolla__BASE   bolla__DE
057300170518     c                   z-add     colli__BASE   colli__DE
057400170518     c                   z-add     peso___BASE   peso___DE
057500170518     c                   z-add     volume_BASE   volume_DE
057600170518     c                   endif
057700170518      **
0578001705182e   c                   end
0579001705181e   c                   endif
058000170518      *
0581001705180x   C                   ELSE
058200170518      * Se ENTRATO in IMG
058300170518      **
058400170518      * calcola i giorni trascorsi
058500170518     c                   clear                   wdat8
058600170518     c                   clear                   tfptfa
058700170518     c                   movel     data_INIelab  datada
058800170518     c                   movel     data_ENTR_IMG dataa
058900170518     c                   z-add     GCPLNAA       p_tfa
059000170518     c                   call      'XSRLAV8'
059100170518     c                   parm                    wdat8
059200170518     c                   parm                    tfptfa
059300170518      **  se 3 gg.
059400170518      ** "+2 IMA" (3° gg)
059500170518     c                   if        GGL = 3
059600170518     c                   z-add     bolla__BASE   bolla__F
059700170518     c                   z-add     colli__BASE   colli__F
059800170518     c                   z-add     peso___BASE   peso___F
059900170518     c                   z-add     volume_BASE   volume_F
060000170518      **  se <3 gg.
060100170518      ** "variazione di peso o forzata IMG o tabella 2G periodo sospensione"
060200170518     c                   elseif    GGL < 3
060300170518     c                   z-add     bolla__BASE   bolla__G
060400170518     c                   z-add     colli__BASE   colli__G
060500170518     c                   z-add     peso___BASE   peso___G
060600170518     c                   z-add     volume_BASE   volume_G
060700170518      **  se >3 gg.
060800170518      ** "anomalia di procedura"
060900170518     c                   elseif    GGL > 3
061000170518     c                   z-add     bolla__BASE   bolla__H
061100170518     c                   z-add     colli__BASE   colli__H
061200170518     c                   z-add     peso___BASE   peso___H
061300170518     c                   z-add     volume_BASE   volume_H
061400170518     c                   endif
061500170518      **
0616001705180e   C                   END
061700170518      *
061800170518      *  ULTERIORI CONSIDERAZIONI
061900170518      *   Occorre calcolare inoltre le spedizioni
062000170518      *    che giornalmente vengono "imate"
062100170518      *
062200170518      *   nel campo spediz con ima 1° gg "data partenza del calcolo + 1"
062300170518     c                   z-add     bolla__B      bolla__I
062400170518     c                   z-add     colli__B      colli__I
062500170518     c                   z-add     peso___B      peso___I
062600170518     c                   z-add     volume_B      volume_I
062700170518      *
062800170518      *   nel campo spediz con ima 1° gg "data partenza del calcolo + 1"
062900170518      *      e
063000170518      *   nel campo spediz con ima 2° gg "data calcolo + 2"
063100170518     c                   z-add     bolla__C      bolla__J
063200170518     c                   z-add     colli__C      colli__J
063300170518     c                   z-add     peso___C      peso___J
063400170518     c                   z-add     volume_C      volume_J
063500170518      *
063600170518      *  nel campo spediz con ima 1° gg "data partenza del calcolo + 1"
063700170518      *     e
063800170518      *  nel campo spediz con ima 2° gg "data calcolo + 2"
063900170518      *     e
064000170518      *  nel campo spediz con ima oltre 2 gg in tutti i giorni fino
064100170518      *  alla "data fine calcolo -1 gg"
064200170518      *  (se <> 0 oppure fino alla data di finale del range elaborazione..)
064300170518     c                   z-add     bolla__DE     bolla__K
064400170518     c                   z-add     colli__DE     colli__K
064500170518     c                   z-add     peso___DE     peso___K
064600170518     c                   z-add     volume_DE     volume_K
064700170518      *
064800170512     c                   endsr
064900170512?     *--------------------------------------------------------------*
065000170519      * Controlla scrivendo il record sul WFIMAG0F
065100170518?     *--------------------------------------------------------------*
065200170519     c     Controlla     begsr
065300170518      *
065400170519     c                   clear                   wfIMAG00
065500170519     c                   eval       WFAAAS    = gcpAAS
065600170519     c                   eval       WFANGC    = gcpNGC
065700170523     c                   eval       WFAPRG    = gcpFRG
065800170519     c                   eval       WFAAASA   = gcpAASA
065900170519     c                   eval       WFALNPA   = gcpLNPA
066000170519     c                   eval       WFANRSA   = gcpNRSA
066100170519     c                   eval       WFANSPA   = gcpNSPA
066200170519     c                   eval       WFAGFGS   = gcpLNAA
066300170525     c                   eval       WFACCA    = tasCCA
066400170522     c                   eval       WFADOPN   = data_OPNgiac
066500170522     c                   eval       WFADINI   = data_INIelab
066600170519     c                   eval       WFADFIN   = data_FINEelab
066700170525     c                   eval       WFADMIC   = data_MICelab
066800170523     c                   eval       WFADEVE   = data_EVEnto
066900170523     c                   eval       WFADCHI   = data_CLOgiac
067000170522     c                   eval       WFADDCM   = data_Consegna
067100170519     c                   eval       WFADENT   = data_ENTR_IMG
067200170519     c                   eval       WFAFLDA   = bolla__A
067300170519     c                   eval       WFAFLDB   = bolla__B
067400170519     c                   eval       WFAFLDC   = bolla__C
067500170519     c                   eval       WFAFLDDE  = bolla__DE
067600170519     c                   eval       WFAFLDF   = bolla__F
067700170519     c                   eval       WFAFLDG   = bolla__G
067800170519     c                   eval       WFAFLDH   = bolla__H
067900170519     c                   write     wfIMAG00
068000170518      *
068100170518     c                   endsr
068200170518?     *--------------------------------------------------------------*
068300170519      * aggiorna il D.B.
068400170519?     *--------------------------------------------------------------*
068500170519     c     Aggiorna      begsr
068600170519      *
068700170519      * con DATA e FILIALE arrivo
068800170519     C                   eval      data8 =  data_INIelab
068900170519     C                   eval      fil3  =  gcpLNAA
069000170519     c     kDataFil      chain     tnIMAG1L
069100170519      *
069200170519     c                   if        %Found(tnIMAG1L)
069300170519      *  AGGIORNA
069400170519     C                   add       bolla__BASE   IMAGNBL
069500170519     C                   add       colli__BASE   IMAGNCL
069600170519     C                   add       peso___BASE   IMAGPKL
069700170519     C                   add       volume_BASE   IMAGVLU
069800170519     C                   add       bolla__A      IMAGNBLR
069900170519     C                   add       colli__A      IMAGNCLR
070000170519     C                   add       peso___A      IMAGPKLR
070100170519     C                   add       volume_A      IMAGVLUR
070200170519     C                   add       bolla__B      IMAGNBL1R
070300170519     C                   add       colli__B      IMAGNCL1R
070400170519     C                   add       peso___B      IMAGPKL1R
070500170519     C                   add       volume_B      IMAGVLU1R
070600170519     C                   add       bolla__C      IMAGNBL2R
070700170519     C                   add       colli__C      IMAGNCL2R
070800170519     C                   add       peso___C      IMAGPKL2R
070900170519     C                   add       volume_C      IMAGVLU2R
071000170519     C                   add       bolla__DE     IMAGBAPR
071100170519     C                   add       colli__DE     IMAGCAPR
071200170519     C                   add       peso___DE     IMAGPAPR
071300170519     C                   add       volume_DE     IMAGVAPR
071400170519     C                   add       bolla__F      IMAGNBLG
071500170519     C                   add       colli__F      IMAGNCLG
071600170519     C                   add       peso___F      IMAGPKLG
071700170519     C                   add       volume_F      IMAGVLUG
071800170519     C                   add       bolla__G      IMAGBGP
071900170519     C                   add       colli__G      IMAGCGP
072000170519     C                   add       peso___G      IMAGPGP
072100170519     C                   add       volume_G      IMAGVGP
072200170519     C                   add       bolla__H      IMAGBGD
072300170519     C                   add       colli__H      IMAGCGD
072400170519     C                   add       peso___H      IMAGPGD
072500170519     C                   add       volume_H      IMAGVGD
072600170519     c                   update    tnIMAG00
072700170519      *
072800170519     c                   else
072900170519      *  SCRIVE
073000170519      *    il record per la DATA Elaborazione
073100170519     c                   clear                   tnIMAG00
073200170519     C                   eval      IMAGDTA = data_INIelab
073300170519     C                   eval      IMAGFGS = gcpLNAA
073400170519      *
073500170519     C                   eval      IMAGNBL    =  bolla__BASE
073600170519     C                   eval      IMAGNCL    =  colli__BASE
073700170519     C                   eval      IMAGPKL    =  peso___BASE
073800170519     C                   eval      IMAGVLU    =  volume_BASE
073900170519     C                   eval      IMAGICE    =  0
074000170519     C                   eval      IMAGNBLR   =  bolla__A
074100170519     C                   eval      IMAGNCLR   =  colli__A
074200170519     C                   eval      IMAGPKLR   =  peso___A
074300170519     C                   eval      IMAGVLUR   =  volume_A
074400170519     C                   eval      IMAGNBL1R  =  bolla__B
074500170519     C                   eval      IMAGNCL1R  =  colli__B
074600170519     C                   eval      IMAGPKL1R  =  peso___B
074700170519     C                   eval      IMAGVLU1R  =  volume_B
074800170519     C                   eval      IMAGNBL2R  =  bolla__C
074900170519     C                   eval      IMAGNCL2R  =  colli__C
075000170519     C                   eval      IMAGPKL2R  =  peso___C
075100170519     C                   eval      IMAGVLU2R  =  volume_C
075200170519     C                   eval      IMAGBAPR   =  bolla__DE
075300170519     C                   eval      IMAGCAPR   =  colli__DE
075400170519     C                   eval      IMAGPAPR   =  peso___DE
075500170519     C                   eval      IMAGVAPR   =  volume_DE
075600170519     C                   eval      IMAGNBLG   =  bolla__F
075700170519     C                   eval      IMAGNCLG   =  colli__F
075800170519     C                   eval      IMAGPKLG   =  peso___F
075900170519     C                   eval      IMAGVLUG   =  volume_F
076000170519     C                   eval      IMAGBGP    =  bolla__G
076100170519     C                   eval      IMAGCGP    =  colli__G
076200170519     C                   eval      IMAGPGP    =  peso___G
076300170519     C                   eval      IMAGVGP    =  volume_G
076400170519     C                   eval      IMAGBGD    =  bolla__H
076500170519     C                   eval      IMAGCGD    =  colli__H
076600170519     C                   eval      IMAGPGD    =  peso___H
076700170519     C                   eval      IMAGVGD    =  volume_H
076800170519     C                   eval      IMAGNBL1   =  0
076900170519     C                   eval      IMAGNCL1   =  0
077000170519     C                   eval      IMAGPKL1   =  0
077100170519     C                   eval      IMAGVLU1   =  0
077200170519     C                   eval      IMAGNBL2   =  0
077300170519     C                   eval      IMAGNCL2   =  0
077400170519     C                   eval      IMAGPKL2   =  0
077500170519     C                   eval      IMAGVLU2   =  0
077600170519     C                   eval      IMAGNBLO   =  0
077700170519     C                   eval      IMAGNCLO   =  0
077800170519     C                   eval      IMAGPKLO   =  0
077900170519     C                   eval      IMAGVLUO   =  0
078000170519     c                   write     tnIMAG00
078100170519     c                   end
078200170519      *
078300170519      *  Contatori aggiunti I,J,K
078400170519     c                   exsr      AltriContatori
078500170519      *
078600170519     c                   endsr
078700170519?     *--------------------------------------------------------------*
078800170519      *  deve aggiornare altri contatori su altre date
078900170519?     *--------------------------------------------------------------*
079000170519     c     AltriContatoriBEGsr
079100170519      *
079200170519      *   campi per il calcolo
079300170519     c                   clear                   data_elab_1       8 0
079400170519     c                   clear                   data_elab_2       8 0
079500170519      **
079600170526     c* data Inizio
079700170526     c                   eval      data_elab_1 = data8
079800170519      **
079900170526     c* data Inizio +1gg. lavorativo
080000170519     c                   clear                   xgiolavds
080100170519     c                   eval      IXGLDATA = data_elab_1
080200170519     c                   eval      IXGLfil  = fil3
080300170519     c                   eval      IXGLpa   ='A'
080400170519     c                   eval      IXGLadd  ='S'
080500170519     c                   eval      IXGLgg   = 1
080600170519     c                   eval      IXGLlav  ='S'
080700170519     c                   call      'XGIOLAV'
080800170519     c                   parm                    xgiolavds
080900170519    5c                   if        oxglerr=' '
081000170519     c                   eval      data_elab_2 = oxgldata
081100170519    5c                   endif
081200170519      *----------------
081300170519      *  su 1gg successivo al giorno dell'INI_elaborazione
081400170519     c                   if         bolla__B  > 0      or
081500170519     c                              bolla__C  > 0      or
081600170519     c                              bolla__DE > 0
081700170519     c                   exsr      Dopo_1_giorno
081800170519     c                   end
081900170519      *
082000170519      *  su 2gg successivi al giorno dell'INI_elaborazione
082100170519     c                   if         bolla__C  > 0      or
082200170519     c                              bolla__DE > 0
082300170519     c                   exsr      Dopo_2_giorni
082400170519     c                   end
082500170519      *
082600170519      *  su  gg successivi al 2°giorno dell'INI_elaborazione
082700170519     c                   if         bolla__DE > 0
082800170519     c                   exsr      Oltre_2_giorni
082900170519     c                   end
083000170519      *
083100170519     c                   endsr
083200170519?     *--------------------------------------------------------------*
083300170519      *  dopo 1 giorno dalla data elaborazione
083400170519?     *--------------------------------------------------------------*
083500170519     c     dopo_1_giorno BEGSR
083600170519      *
083700170519      * con DATA e FILIALE arrivo
083800170519     C                   eval      data8 =  data_elab_1
083900170519     c     kDataFil      chain     tnIMAG1L
084000170519      *
084100170519     c                   if        %Found(tnIMAG1L)
084200170519      *  AGGIORNA
084300170519     C                   add       bolla__BASE   IMAGNBL1
084400170519     C                   add       colli__BASE   IMAGNCL1
084500170519     C                   add       peso___BASE   IMAGPKL1
084600170519     C                   add       volume_BASE   IMAGVLU1
084700170519     c                   update    tnIMAG00
084800170519      *
084900170519     c                   else
085000170519      *  SCRIVE
085100170519      *    il record per la DATA Elaborazione
085200170519     c                   clear                   tnIMAG00
085300170519     C                   eval      IMAGDTA = data_elab_1
085400170519     C                   eval      IMAGFGS = gcpLNAA
085500170519     C                   add       bolla__BASE   IMAGNBL1
085600170519     C                   add       colli__BASE   IMAGNCL1
085700170519     C                   add       peso___BASE   IMAGPKL1
085800170519     C                   add       volume_BASE   IMAGVLU1
085900170519      *
086000170519     c                   write     tnIMAG00
086100170519     c                   end
086200170519      *
086300170519     c                   endsr
086400170519?     *--------------------------------------------------------------*
086500170519      *  dopo 2 giorni dalla data elaborazione
086600170519?     *--------------------------------------------------------------*
086700170519     c     dopo_2_giorni BEGSR
086800170519      *
086900170519      * con DATA e FILIALE arrivo
087000170519     C                   eval      data8 =  data_elab_2
087100170519     c     kDataFil      chain     tnIMAG1L
087200170519      *
087300170519     c                   if        %Found(tnIMAG1L)
087400170519      *  AGGIORNA
087500170519     C                   add       bolla__BASE   IMAGNBL2
087600170519     C                   add       colli__BASE   IMAGNCL2
087700170519     C                   add       peso___BASE   IMAGPKL2
087800170519     C                   add       volume_BASE   IMAGVLU2
087900170519     c                   update    tnIMAG00
088000170519      *
088100170519     c                   else
088200170519      *  SCRIVE
088300170519      *    il record per la DATA Elaborazione
088400170519     c                   clear                   tnIMAG00
088500170519     C                   eval      IMAGDTA = data_elab_2
088600170519     C                   eval      IMAGFGS = gcpLNAA
088700170519     C                   add       bolla__BASE   IMAGNBL2
088800170519     C                   add       colli__BASE   IMAGNCL2
088900170519     C                   add       peso___BASE   IMAGPKL2
089000170519     C                   add       volume_BASE   IMAGVLU2
089100170519      *
089200170519     c                   write     tnIMAG00
089300170519     c                   end
089400170519      *
089500170519     c                   endsr
089600170519?     *--------------------------------------------------------------*
089700170519      * oltre 2 giorni dalla data elaborazione
089800170519?     *--------------------------------------------------------------*
089900170519     c     oltre_2_giorniBEGSR
090000170519      *
090100170526      * Imposta il Giorno LIMITE a cui arrivare
090200170519     c                   clear                   data_LIMITE       8 0
090300170526      **
090400170526      ** se c'è la data fine
090500170519     c                   if        data_FINEelab > 0
090600170526     c                   eval      data_LIMITE = data_FINEelab
090700170526      ** altrimenti
090800170526      **  prende la data di fine range passata in KPJBU
090900170519     c                   else
091000170519     c                   eval      data_LIMITE = i2aDATA
091100170519     c                   end
091200170526      *
091300170519      **  Da data 2 giorni dopo
091400170519     c                   eval      data8 = data_elab_2
091500170519      **
091600170519     c     altro_giorno  tag
091700170519     c*  +1gg. lavorativo
091800170519     c                   clear                   xgiolavds
091900170519     c                   eval      IXGLDATA = data8
092000170519     c                   eval      IXGLfil  = fil3
092100170519     c                   eval      IXGLpa   ='A'
092200170519     c                   eval      IXGLadd  ='S'
092300170519     c                   eval      IXGLgg   = 1
092400170519     c                   eval      IXGLlav  ='S'
092500170519     c                   call      'XGIOLAV'
092600170519     c                   parm                    xgiolavds
092700170519    5c                   if        oxglerr=' '
092800170519     c                   eval      data8 = oxgldata
092900170519    5c                   endif
093000170519      **
093100170519      ** supera la data limite deve USCIRE!!!!!!!
093200170519    5c                   if        data8 > data_LIMITE
093300170519     c                   LEAVESR
093400170519     c                   end
093500170519      **
093600170519      * con DATA e FILIALE arrivo
093700170519     c     kDataFil      chain     tnIMAG1L
093800170519      *
093900170519     c                   if        %Found(tnIMAG1L)
094000170519      *  AGGIORNA
094100170519     C                   add       bolla__BASE   IMAGNBLO
094200170519     C                   add       colli__BASE   IMAGNCLO
094300170519     C                   add       peso___BASE   IMAGPKLO
094400170519     C                   add       volume_BASE   IMAGVLUO
094500170519     c                   update    tnIMAG00
094600170519      *
094700170519     c                   else
094800170519      *  SCRIVE
094900170519      *    il record per la DATA Elaborazione
095000170519     c                   clear                   tnIMAG00
095100170519     C                   eval      IMAGDTA = data8
095200170519     C                   eval      IMAGFGS = gcpLNAA
095300170519     C                   add       bolla__BASE   IMAGNBLO
095400170519     C                   add       colli__BASE   IMAGNCLO
095500170519     C                   add       peso___BASE   IMAGPKLO
095600170519     C                   add       volume_BASE   IMAGVLUO
095700170519      *
095800170519     c                   write     tnIMAG00
095900170519     c                   end
096000170519      *
096100170519     c                   goto      altro_giorno
096200170519      *
096300170519     c                   endsr
096400170525?     *--------------------------------------------------------------*
096500170525      * TOTALI
096600170525?     *--------------------------------------------------------------*
096700170525     c     TOTALS        begsr
096800170525      *
096900170525      *  se ci fossero delle chiavi x rotture di livello totalizzazioni
097000170525      *     questa è la routine da utilizzare
097100170525      *
097200170525     c                   endsr
097300170519?     *--------------------------------------------------------------*
