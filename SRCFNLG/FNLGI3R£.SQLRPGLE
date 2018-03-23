000100160819     H*------------------------------------------------------------------------*
000200170522     H* Giacenze Righe senza RIAPERTURA x misurare i tempi in IMA e su IMG
000300160819     H*------------------------------------------------------------------------*
000400170512     ftitas30c  if   e           k disk
000500170323     fazorg01l  if   e           k disk
000600160822     FFNevb01L  IF   E           K DISK
000700160822     FTABEL00F  IF   E           K DISK    usropn
000800161005     Ftntbe01l  IF   E           K DISK
000900170519     FWFIMAG0F  O  a E             DISK
001000170607     FTNIMAG0L  uF a E           K DISK    usropn
001001170607     FTNIMAG5L  uF a E           K DISK    usropn
001002170607     F
001100170526     FECevd05L  IF   E           K DISK
001200170511     D*------------------
001300170511     D* PASSAGGIO PARAMETRI
001400170511     D*------------------
001500170512     D KPJBA         E DS
001600170512     D FNLGI2ds      E DS
001700170512     D tigcpDS       E DS                  extname(tigcp00f)
001800170512     D*
001900170512     D WrkSqlCmd       S           1024
002000170511     D*
002100160822     D codntc          S              3    DIM(250)
002200070629
002300160822     D ds2a          E DS
002400170424     D ds2g2         E DS
002500170323     D og150         E DS
002600170323     D fnlv55ds      E DS
002700170519     d xgiolavds     e ds
002800170518
002900160822     D WDAT8           DS
003000160822     D  DATADA                 1      8  0
003100160822     D  DATAA                  9     16  0
003200160822     D  GGL                   17     21  0
003300170518
003400160822     D tfptfa          DS
003500160822     D  p_tfp                  1      3  0
003600160822     D  p_tfa                  4      6  0
003700170518
003800160927     d waddgg          s              3  0
003900170518
004000161005     d ktcod           s                   like(tbecod) inz('FLG')
004100161005     d ktke1           s                   like(tbeke1)
004200160926     d Datasys         s               d   datfmt(*iso) inz(*sys)
004300170512     d Data_ISO        s               d   datfmt(*iso)
004400170519      **-- campi chiave
004500170519     d  data8          s              8  0
004600170519     d  fil3           s              3  0
004700170529     d  pesoKG         s             13  3
004800170529     d  Scaglione      s             13  3
004900170519      *
005000170519     D*______________________________________________
005100170518     D Contatori       DS
005200170518     D  bolla__BASE                   8  0
005300170518     D  colli__BASE                   8  0
005400170518     D  peso___BASE                  13  1
005500170518     D  volume_BASE                  13  3
005600170529     D  GSosta_BASE                   7  0
005700170518     D  bolla__A                      8  0
005800170518     D  colli__A                      8  0
005900170518     D  peso___A                     13  1
006000170518     D  volume_A                     13  3
006100170518     D  bolla__B                      8  0
006200170518     D  colli__B                      8  0
006300170518     D  peso___B                     13  1
006400170518     D  volume_B                     13  3
006500170518     D  bolla__C                      8  0
006600170518     D  colli__C                      8  0
006700170518     D  peso___C                     13  1
006800170518     D  volume_C                     13  3
006900170518     D  bolla__DE                     8  0
007000170518     D  colli__DE                     8  0
007100170518     D  peso___DE                    13  1
007200170518     D  volume_DE                    13  3
007300170518     D  bolla__F                      8  0
007400170518     D  colli__F                      8  0
007500170518     D  peso___F                     13  1
007600170518     D  volume_F                     13  3
007700170518     D  bolla__G                      8  0
007800170518     D  colli__G                      8  0
007900170518     D  peso___G                     13  1
008000170518     D  volume_G                     13  3
008100170518     D  bolla__H                      8  0
008200170518     D  colli__H                      8  0
008300170518     D  peso___H                     13  1
008400170518     D  volume_H                     13  3
008500170518     D  bolla__I                      8  0
008600170518     D  colli__I                      8  0
008700170518     D  peso___I                     13  1
008800170518     D  volume_I                     13  3
008900170518     D  bolla__J                      8  0
009000170518     D  colli__J                      8  0
009100170518     D  peso___J                     13  1
009200170518     D  volume_J                     13  3
009300170518     D  bolla__K                      8  0
009400170518     D  colli__K                      8  0
009500170518     D  peso___K                     13  1
009600170518     D  volume_K                     13  3
009700170519      *
009800170512     C*---------------------------------------------------------------*
009900170512     C     *ENTRY        PLIST
010000170512     C                   PARM                    KPJBA
010100170512     C                   MOVEL     KPJBU         FNLGI2DS
010200170512     C                   SETON                                        LR
010300170512     C*---------------------------------------------------------------*
010400170512     c* Carico i codici di mancata consegna
010500170512     c                   eval      kcod='2A'
010600170512     c                   eval      codut=1
010700170512     c                   clear                   xx
010800170512     c                   open      tabel00f
010900170512      **
011000170512     c     ktab          setll     tabel00f
011100170512     c     ktab          reade     tabel00f
011200170512     c                   dow       not %eof(tabel00f)
011300170512     c                   eval      ds2a=tbluni
011400170512     c                   if        §2antc='S'  or (§2atip<>'§'
011500170512     c                             and §2atip<>'S')
011600170512     c                   add       1             xx                3 0
011700170512     c                   movel     tblkey        codntc(xx)
011800170512     c                   endif
011900170512     c     ktab          reade     tabel00f
012000170512     c                   enddo
012100170512     c
012200170512     c* chaino DS2G per il peso limite sopra il quale va subito in IMG
012300170512     c                   clear                   ds2g2
012400170512     c                   eval      kcod='2G'
012500170512     c                   eval      kkey='2       '
012600170512     c     ktab2         chain     tabel00f
012700170512     c                   if        %found(tabel00f)
012800170512     c                   eval      ds2g2=tbluni
012900170512     c                   else
013000170512     c                   eval      §2g2kgimg=50
013100170512     c                   endif
013200170512     c
013300170512     c                   close     tabel00f
013400170512     c
013500170518     c                   clear                   wfgs              3 0
013600170518     c                   clear                   picking           1
013700170512      *
013800170518     C* DEFINIZIONE CHIAVI -------------------------------------------------
013900170512     C     Ktab          KLIST
014000170512     C                   KFLD                    codut             1 0
014100170512     C                   KFLD                    kcod              2
014200170512      *
014300170512     C     Ktab2         KLIST
014400170512     C                   KFLD                    codut             1 0
014500170512     C                   KFLD                    kcod              2
014600170512     C                   KFLD                    kkey              8
014700170512      *
014800170512     C     Ktbe          KLIST
014900170512     C                   KFLD                    ktcod
015000170512     C                   KFLD                    ktke1
015100170512     C*
015200170512     C     Karrivo       KLIST
015300170512     C                   KFLD                    GCPAASa
015400170512     C                   KFLD                    GCPLNPa
015500170512     C                   KFLD                    GCPNRSa
015600170512     C                   KFLD                    GCPNSPa
015700170519      *
015800170519     C     KDataFil      KLIST
015900170526     C                   KFLD                    data8
016000170526     C                   KFLD                    fil3
016100170526      *
016200170526     C     KVOCI         KLIST
016300170526     C                   KFLD                    EVDVOC
016400170526     C                   KFLD                    EVDTTV
016500170526     C                   KFLD                    EVDFT1
016600170519      *
016700170512?     *--------------------------------------------------------------*
016800170512     C*   data di sistema
016900170512     c                   movel     datasys       dateu             8 0
017000170512     C*
017100170512?     *--------------------------------------------------------------*
017200170519     C/EXEC SQL
017300170519     C+ DELETE FROM WFIMAG0F
017400170519     C/END-EXEC
017500170519     C*
017600170512     C/EXEC SQL
017700170526     C+ DELETE FROM TNIMAG0F
017800170512     C/END-EXEC
017801170607     C*
017802170607     C/EXEC SQL
017803170607     C+ DELETE FROM TNIMAG5F
017804170607     C/END-EXEC
017900170607     c                   open      TNIMAG0L
017901170607     c                   open      TNIMAG5L
018000170512     C*
018100170512     c                   clear                   almenoUno         1
018200170512     C*
018300170512     c* ---------------------
018400170512      **** con SQL lettura delle GIACENZE senza RIAPERTURA
018500170512     C                   EVAL      WrkSqlCmd ='SELECT * FROM tigcp00f '     +
018600170512     C                             ' WHERE GCPRIAP = '' '' and '            +
018700170512     C                             ' GCPAGC * 10000 + GCPMGC between '      +
018800170512     C                              %editc(I2DADATA:'X') + ' and '          +
018900170526     C                               %editc(I2ADATA:'X') + ' and ('         +
019000170526     C                             %editc(I2FILIALE:'X') + ' = 0 or '       +
019100170526     C                             %editc(I2FILIALE:'X') + ' = GCPLNAA) '   +
019200170526     C                             ' ORDER BY GCPAGC, GCPMGC '
019300170512     c* ---------------------
019400170512     C/EXEC SQL
019500170512     C+ PREPARE S1 FROM :WrkSqlCmd
019600170512     C/END-EXEC
019700170512
019800170512     C/EXEC SQL
019900170512     C+ DECLARE A1 CURSOR FOR S1
020000170512     C/END-EXEC
020100170512
020200170512     C/EXEC SQL
020300170512     C+ OPEN A1
020400170512     C/END-EXEC
020500170512      * Forzo la stampa del JOBLOG.
020600170512     C                   If        SqlCod < 0
020700170512     C                   CALL      'X66CHGJOB'
020800170512     C                   RETURN
020900170512     C                   End
021000170512      *>>>>>>>>>>>>>
021100170512     C                   DOU       SqlCod <> 0
021200170512     C/EXEC SQL
021300170512     C+ FETCH NEXT FROM A1 INTO :tigcpDS
021400170512     C/END-EXEC
021500170512     C                   SELECT
021600170512     c*
021700170512     c* a fine file Totali x rotture
021800170512     C                   WHEN      SqlCod = 100
021900170512     c                   leave
022000170512     **   ERRORI
022100170512     C                   WHEN      SqlCod < 0
022200170512     C                   seton                                        H1
022300170512     c                   goto      fine
022400170512     c*
022500170512     C                   OTHER
022600170512     c*
022700170512     c* controlla se il record di giacenza è da considerare
022800170519     c                   clear                   rec_OK            1
022900170512     c                   exsr      check_REC
023000170529     c*
023100170529      ** calcola data ELABORAZIONE
023200170529     c                   if          rec_OK <> 'N'
023300170529     c                   exsr      calc_DATAINI
023400170529     c                   end
023500170512      **
023600170512      **  Se il record è buono lo elabora in dettaglio
023700170519     c                   if          rec_OK <> 'N'
023800170529      **
023900170518     c* imposta Date di riferimento per il calcolo
024000170529     c                   exsr      DATE_diRIFER
024100170512     c* DETTAGLIO
024200170512     c                   exsr      dettaglio
024300170519     c* record di controllo
024400170519     c                   exsr      controlla
024500170529      **
024600170519     c* aggiorna il file di Statistica
024700170519     c                   exsr      aggiorna
024800170519     c                   move      'S'           almenouno
024900170529      **
025000170512     c                   end
025100170512     c*
025200170512     C                   ENDSL
025300170512
025400170512     C                   ENDDO
025500170512      *>>>>>>>>>>>>>
025600170512     C/EXEC SQL
025700170512     C+ CLOSE A1
025800170512     C/END-EXEC
025900170607     c                   close     TNIMAG0L
025901170607     c                   close     TNIMAG5L
026000170529      *>>>>>>>>>>>>>
026100170512     C* totali generali
026200170526     c                   if            almenouno ='S'
026300170526     c                   exsr      Importo_VOCI
026400170512     c                   end
026500170512      *
026600170512     c     fine          tag
026700170512     C                   Return
026800170529?     *--------------------------------------------------------------*
026900170529      * Check se l record letto è da scartare
027000170529?     *--------------------------------------------------------------*
027100170529     c     Check_REC     begsr
027200170530      *****
027300170530      * (0)
027400170530      *****    C'è un errore che andrà a posto (Michele lo sta sistemando)
027500170530     c*  quindi per ora escludiamo le giacenze in fase 40 e senza disposizioni
027600170530     c*  del MITTENTE. (questo filtro diventerà inutile)
027700170530     c                   if        GCPFAS =40 and GCPDDM =0
027800170530     c                   eval             rec_OK = 'N'
027900170530     c                   leavesr
028000170530    1c                   endif
028100170529      *****
028200170529      * (1)
028300170529      *****    Se la Filiale NON ha il PICKING -> il RECORD è da SCARTARE
028400170529     c                   exsr      FIL_fa_il_Pick
028500170529     c                   if        picking = 'N'
028600170529     c                   eval             rec_OK = 'N'
028700170529     c                   leavesr
028800170529    1c                   endif
028900170529      *****
029000170529      * (2)
029100170529      *****    Alcune Filiali non hanno IMA -> il RECORD è da SCARTARE
029200170529     c* prima controllo lnp poi ccm
029300170529     c                   eval      ktke1=%editc(GCPLNPA:'X')
029400170529     c     ktbe          chain     tntbe01l
029500170529     c                   if        not %found(tntbe01l) or tbeatb<>' '
029600170529     c* alcuni clienti vanno subito a img
029700170529     c                   eval      ktke1=%editc(GCPSCM:'X')
029800170529     c     ktbe          chain     tntbe01l
029900170529     c                   endif
030000170529     c                   if        %found(tntbe01l) and tbeatb=' '
030100170529     c                   eval             rec_OK = 'N'
030200170529     c                   leavesr
030300170529    1c                   endif
030400170529      *****
030500170529      * (3)
030600170529      *****    Se il peso superiore alla 2G -> il RECORD è da SCARTARE
030700170529     c                   clear                   Peso_Sped         7 1
030800170529     c                   clear                   Vol_Sped          5 3
030900170529     c     karrivo       chain     titas30c
031000170529      **
031100170529    1c                   if        %found(titas30c)
031200170529      * prende il peso
031300170529     c                   eval      Peso_Sped  = taspkf
031400170529     c                   if          taspkc > 0 and tasncp = tasncl
031500170529     c                                or
031600170529     c                               taspkc > 0 and tasncp < tasncl
031700170529     c                               and taspkc > taspkf
031800170529     c                   eval      Peso_Sped  = taspkc
031900170529     c                   end
032000170529      ** se supera esce
032100170529    1c                   if        Peso_Sped > §2g2kgimg
032200170529     c                   eval             rec_OK = 'N'
032300170529     c                   leavesr
032400170529    1c                   endif
032500170529     **
032600170529      * prende il volume
032700170529     c                   eval      Vol_Sped  = tasvlf
032800170529     c                   if          tasvlc > 0 and tasncr = tasncl
032900170529     c                                or
033000170529     c                               tasvlc > 0 and tasncr < tasncl
033100170529     c                               and tasvlc > tasvlf
033200170529     c                   eval      Vol_Sped  = tasvlc
033300170529     c                   end
033400170529      **
033500170529    1c                   endif
033600170529      *
033700170529     c                   endsr
033800170529?     *--------------------------------------------------------------*
033900170529     C     FIL_fa_il_PickBEGSR
034000170529?     *--------------------------------------------------------------*
034100170529      **  La Filiale GESTISCE il PICKING?
034200170529      *
034300170529     c                   clear                   picking
034400170529     c                   movel     GCPFGC        wfgs              3 0
034500170530     c                   clear                   desfil           30
034600170529      * tramite Terminal
034700170529     c                   clear                   fnlv55ds
034800170529     c                   eval      d55lin=GCPFGC
034900170529     c                   eval      d55tpt='6'
035000170529     c                   movel     dateu         d55drf
035100170529     c                   call      'FNLV55R'
035200170529     c                   parm                    fnlv55ds
035300170529     c                   movel     d55tfa        wfgs
035400170529     c
035500170529     c     wfgs          chain     azorg01l
035600170530     c                   if        %Found(azorg01l)
035700170529     c                   movel     orgdf0        og150
035800170530     c                   movel     orgdes        desfil
035900170529     c                   eval      picking=§OGPCK
036000170529     c                   if        picking=' '
036100170529     c                   eval      picking='N'
036200170529     c                   endif
036300170530     c                   endif
036400170529     c*
036500170529     c                   ENDSR
036600170512?     *--------------------------------------------------------------*
036700170512      * Calcola DATA Elaborazione
036800170512?     *--------------------------------------------------------------*
036900170529     c     Calc_DATAINI  begsr
037000170511      *
037100170518     c                   clear                   data_INIelab      8 0
037200170522     c                   clear                   data_OPNgiac      8 0
037300170523     c                   clear                   data_CLOgiac      8 0
037400170523     c                   clear                   data_EVEnto       8 0
037500170525     c                   clear                   data_MICelab      8 0
037600170529     c                   clear                   data_Consegna     8 0
037700170529     c                   eval      data_Consegna = tasDCM
037800170512      *
037900170512      * intanto come prima cosa  imposta come DATA ELABORAZIONE
038000170512      *     la  DATA APERTURA GIACENZA
038100170529     c                   eval      data_OPNgiac = gcpAGC
038200170529     c                                          * 10000 + gcpMGC
038300170529     c                   eval      data_INIelab = data_OPNgiac
038400170523     c                   eval      data_CLOgiac =  GCPDCG
038500170529      **-----
038600170529      **
038700170512      * poi bisogna vedere se l'EVENTO di MANCATA CONSEGNA faccia
038800170512      *  considerare diversamente la DATA ELABORAZIONE aggiungendo
038900170512      *   un giorno al calcolo.
039000170512      **
039100170512     c     karrivo       setgt     fnevb01l
039200170512     c     karrivo       readpe    fnevb01l
039300170512    1c                   dow       not %eof(fnevb01l)
039400170525      *
039500170525      * Prende l'ultimo MIC letto prima dell'eventuale stato di Mancata
039600170525      * consegna.
039700170529     c                   if        evbCEV = 'MIC'
039800170525     c                   eval        data_MICelab =evbDEV
039900170529     c                   endif
040000170512
040100170512    2c                   if        %lookup(evbcev:codntc)>0
040200170512     c                             and %subst(evbnot:8:7)>*zeros
040300170525      *-
040400170512     c* trovato evento di mancata consegna --> verifico la data rispetto
040500170512     c*  alla data giacenza
040600170518    3c                   if        data_INIelab  = evbDEV
040700170529     c                   eval      data_EVEnto = evbDEV
040800170523     c*  +1gg. lavorativo
040900170523     C                   eval      fil3  =  gcpLNAA
041000170523     c                   clear                   xgiolavds
041100170523     c                   eval      IXGLDATA = data_INIelab
041200170523     c                   eval      IXGLfil  = fil3
041300170523     c                   eval      IXGLpa   ='A'
041400170525     c                   eval      IXGLadd  ='S'
041500170523     c                   eval      IXGLgg   = 1
041600170523     c                   eval      IXGLlav  ='S'
041700170523     c                   call      'XGIOLAV'
041800170523     c                   parm                    xgiolavds
041900170529     c                   if        oxglerr=' '
042000170523     c                   eval      data_INIelab = oxgldata
042100170529     c                   endif
042200170524      **
042300170524     c                   leave
042400170512    3c                   endif
042500170523      **
042600170512    2c                   endif
042700170512     c
042800170512     c     karrivo       readpe    fnevb01l
042900170512    1c                   enddo
043000170529      **
043100170529      **-----
043200170529      *****
043300170529      * (4)
043400170529      *****    Se fosse presente un periodo di sospensione e la data
043500170529      *  ELABORAZIONE fosse compresa nell'intervallo -> il RECORD è da SCARTARE
043600170529     c                   if            §2G2DIMAi >  0  and
043700170529     c                             data_INIelab  >= §2G2DIMAi and
043800170529     c                             data_INIelab  <= §2G2DIMAf
043900170529     c                   eval             rec_OK = 'N'
044000170529     c                   leavesr
044100170529     c                   endif
044200170512      *
044300170512     c                   endsr
044400170512?     *--------------------------------------------------------------*
044500170518      * imposta le date di RIFERIMENTO
044600170512?     *--------------------------------------------------------------*
044700170529     c     DATE_diRIFER  begsr
044800170512      *
044900170518      *------------
045000170518      ** calcola data FINE ELABORAZIONE
045100170518     c                   clear                   data_FINEelab     8 0
045200170518      *
045300170518      *  chiamo data fine calcolo, la data chiusura giacenza se PRG <>0,
045400170518      *        altrimenti DCM da ARB se consegnata
045500170518     c                   if        gcpFRG > 0
045600170525     c                              or
045700170525     c                             tasCCA <> *blank
045800170529     c                   eval        data_FINEelab = data_CLOgiac
045900170518     c                   else
046000170523      *
046100170518      ** se c'è la data Consegna su TITAS
046200170529     c                   if        data_Consegna > 0
046300170525      *
046400170529     c                   eval      data_FINEelab = data_Consegna
046500170523     **
046600170523     *** toglie   1gg. per i calcoli a seguire
046700170529     c                   if          data_FINEelab > data_INIelab
046800170523     C                   eval      fil3  =  gcpLNAA
046900170523     c                   clear                   xgiolavds
047000170523     c                   eval      IXGLDATA = data_FINEelab
047100170523     c                   eval      IXGLfil  = fil3
047200170523     c                   eval      IXGLpa   ='A'
047300170525     c                   eval      IXGLsub  ='S'
047400170523     c                   eval      IXGLgg   = 1
047500170523     c                   eval      IXGLlav  ='S'
047600170523     c                   call      'XGIOLAV'
047700170523     c                   parm                    xgiolavds
047800170523    5c                   if        oxglerr=' '
047900170529    5c                               and
048000170529     c                             data_INIelab <= oxgldata
048100170523     c                   eval      data_FINEelab = oxgldata
048200170523    5c                   endif
048300170518     c                   end
048400170529     c                   end
048500170518      *
048600170525     c                   endif
048700170518      *------------
048800170518      ** data ENTRATA MAGAZZINO GIACENZE
048900170518     c                   clear                   data_ENTR_IMG     8 0
049000170518      *
049100170518     c                   if         GCPDLMA > 0 and  GCPDLMA<99999999
049200170518     c                   eval      data_ENTR_IMG = GCPDLMA
049300170518     c                   end
049400170518      *
049500170518     c                   ENDSR
049600170512?     *--------------------------------------------------------------*
049700170525      * DATA_con_MIC
049800170512?     *--------------------------------------------------------------*
049900170525     c     DATA_con_MIC  begsr
050000170518      *
050100170525      * sottra 1 gg. data Fine Calcolo
050200170525     c                   clear                   xgiolavds
050300170525     c                   eval      IXGLDATA = data_FINEelab
050400170525     c                   eval      IXGLfil  = fil3
050500170525     c                   eval      IXGLpa   ='A'
050600170525     c                   eval      IXGLsub  ='S'
050700170525     c                   eval      IXGLgg   = 1
050800170525     c                   eval      IXGLlav  ='S'
050900170525     c                   call      'XGIOLAV'
051000170525     c                   parm                    xgiolavds
051100170525      *
051200170525    5c                   if        oxglerr=' '
051300170525    5c                               and
051400170525     c                             data_INIelab <= oxgldata
051500170525     c                   eval      data_FINEelab = oxgldata
051600170525    5c                   endif
051700170525      *
051800170525     c                   ENDSR
051900170525?     *--------------------------------------------------------------*
052000170525      * Dettaglio
052100170525?     *--------------------------------------------------------------*
052200170525     c     Dettaglio     begsr
052300170525      *
052400170518      *  Pulisce i contatori x la singola riga
052500170518     c                   clear                   contatori
052600170518      *
052700170518     c                   z-add     1             bolla__BASE
052800170518     c                   z-add     TASncl        colli__BASE
052900170518     c                   z-add     Peso_Sped     peso___BASE
053000170518     c                   z-add     Vol_Sped      volume_BASE
053100170529      *  solo se NON negativi
053200170601     c                   clear                   GSosta_BASE
053300170529     c                   if           TASggs >0
053400170529     c                   z-add     TASggs        GSosta_BASE
053500170529     c                   end
053600170518      *
053700170518      *  SE NON ENTRATO in IMG quindi ANCORA in IMA
0538001705180    c                   IF          data_ENTR_IMG = 0
053900170518      *
054000170518      *  Se data di FINE non c'è
054100170518      **     oltre 2 gg. NON essendoci la data di FINE
0542001705181    c                   if        data_FINEelab = 0
054300170518      **   '"> 2 IMA"
054400170518     c                   z-add     bolla__BASE   bolla__DE
054500170518     c                   z-add     colli__BASE   colli__DE
054600170518     c                   z-add     peso___BASE   peso___DE
054700170518     c                   z-add     volume_BASE   volume_DE
054800170530      *
054900170530      *  azzera il MIC x non fare confusione
055000170530     c                   eval      data_MICelab = 0
055100170530      *
0552001705181x   c                   else
055300170525      **
055400170525      ** attenzione se c'era un MIC contestuale alla data FINE
055500170525     c                   if        data_FINEelab = data_MICelab
055600170525     c                              and
055700170525     c                             data_FINEelab > data_INIelab
055800170525      ** sottraggo 1gg.data fine
0559001705251x   c                   exsr      DATA_con_MIC
056000170525     c                   else
056100170525     c                   eval      data_MICelab = 0
056200170525     c                   end
056300170529      **
056400170529      ** FORZATURA:
056500170529      **  X parare casi pietosi dove le PRECEDENTI considerazioni
056600170529      **  mi portano ad avere una data CHIUSURA precedente ad APERTURA
056700170529      **   ILLOGICO
056800170529      **  allora si deve imporre la data APERTURA in apertura e chiusura
056900170529     c                   if        data_FINEelab < data_INIelab
057000170529     c                   eval      data_INIelab  = data_OPNgiac
057100170529     c                   eval      data_FINEelab = data_OPNgiac
057200170529     c                   end
057300170529      **
057400170529      **
057500170525      * calcola sempre quindi i giorni trascorsi
057600170525     c                   clear                   wdat8
057700170525     c                   clear                   tfptfa
057800170525     c                   movel     data_INIelab  datada
057900170525     c                   movel     data_FINEelab dataa
058000170525     c                   z-add     GCPLNAA       p_tfa
058100170525     c                   call      'XSRLAV8'
058200170525     c                   parm                    wdat8
058300170525     c                   parm                    tfptfa
058400170525      **
058500170518      * Con data FINE
058600170518      *  Se inizia e finisce nello stesso giorno
058700170523      *  o causa evento (ha aggiunto un giorno) quindi il giorno prima
0588001705232    c                   if        data_FINEelab <= data_INIelab
058900170525     c                               and
059000170525     c                             data_Consegna > 0
059100170518      * "risolte subito"
059200170518     c                   z-add     bolla__BASE   bolla__A
059300170518     c                   z-add     colli__BASE   colli__A
059400170518     c                   z-add     peso___BASE   peso___A
059500170518     c                   z-add     volume_BASE   volume_A
0596001705182x   c                   else
059700170518      **  se 1 gg.
059800170518      **  "+IMA -IMG" (1° gg)
059900170518     c                   if        GGL = 1
060000170518     c                   z-add     bolla__BASE   bolla__B
060100170518     c                   z-add     colli__BASE   colli__B
060200170518     c                   z-add     peso___BASE   peso___B
060300170518     c                   z-add     volume_BASE   volume_B
060400170518      **  se 2 gg.
060500170518      **  "+IMA -IMG" (2° gg)
060600170518     c                   elseif    GGL = 2
060700170518     c                   z-add     bolla__BASE   bolla__C
060800170518     c                   z-add     colli__BASE   colli__C
060900170518     c                   z-add     peso___BASE   peso___C
061000170518     c                   z-add     volume_BASE   volume_C
061100170518      **  se oltre 2 gg.
061200170518      **  "> 2 IMA" ciò si verifica per 2 motivi:
061300170518      **  1.  DLO accetta di non portare in IMG se già immesse dispo in ARRIVO.
061400170518      **  2.  "Anomalia di procedura".
061500170518     c                   elseif    GGL > 2
061600170518     c                   z-add     bolla__BASE   bolla__DE
061700170518     c                   z-add     colli__BASE   colli__DE
061800170518     c                   z-add     peso___BASE   peso___DE
061900170518     c                   z-add     volume_BASE   volume_DE
062000170518     c                   endif
062100170518      **
0622001705182e   c                   end
0623001705181e   c                   endif
062400170518      *
0625001705180x   C                   ELSE
062600170518      * Se ENTRATO in IMG
062700170518      **
062800170518      * calcola i giorni trascorsi
062900170518     c                   clear                   wdat8
063000170518     c                   clear                   tfptfa
063100170518     c                   movel     data_INIelab  datada
063200170518     c                   movel     data_ENTR_IMG dataa
063300170518     c                   z-add     GCPLNAA       p_tfa
063400170518     c                   call      'XSRLAV8'
063500170518     c                   parm                    wdat8
063600170518     c                   parm                    tfptfa
063700170518      **  se 3 gg.
063800170518      ** "+2 IMA" (3° gg)
063900170518     c                   if        GGL = 3
064000170518     c                   z-add     bolla__BASE   bolla__F
064100170518     c                   z-add     colli__BASE   colli__F
064200170518     c                   z-add     peso___BASE   peso___F
064300170518     c                   z-add     volume_BASE   volume_F
064400170518      **  se <3 gg.
064500170518      ** "variazione di peso o forzata IMG o tabella 2G periodo sospensione"
064600170518     c                   elseif    GGL < 3
064700170518     c                   z-add     bolla__BASE   bolla__G
064800170518     c                   z-add     colli__BASE   colli__G
064900170518     c                   z-add     peso___BASE   peso___G
065000170518     c                   z-add     volume_BASE   volume_G
065100170518      **  se >3 gg.
065200170518      ** "anomalia di procedura"
065300170518     c                   elseif    GGL > 3
065400170518     c                   z-add     bolla__BASE   bolla__H
065500170518     c                   z-add     colli__BASE   colli__H
065600170518     c                   z-add     peso___BASE   peso___H
065700170518     c                   z-add     volume_BASE   volume_H
065800170518     c                   endif
065900170518      **
0660001705180e   C                   END
066100170518      *
066200170518      *  ULTERIORI CONSIDERAZIONI
066300170518      *   Occorre calcolare inoltre le spedizioni
066400170518      *    che giornalmente vengono "imate"
066500170518      *
066600170518      *   nel campo spediz con ima 1° gg "data partenza del calcolo + 1"
066700170518     c                   z-add     bolla__B      bolla__I
066800170518     c                   z-add     colli__B      colli__I
066900170518     c                   z-add     peso___B      peso___I
067000170518     c                   z-add     volume_B      volume_I
067100170518      *
067200170518      *   nel campo spediz con ima 1° gg "data partenza del calcolo + 1"
067300170518      *      e
067400170518      *   nel campo spediz con ima 2° gg "data calcolo + 2"
067500170518     c                   z-add     bolla__C      bolla__J
067600170518     c                   z-add     colli__C      colli__J
067700170518     c                   z-add     peso___C      peso___J
067800170518     c                   z-add     volume_C      volume_J
067900170518      *
068000170518      *  nel campo spediz con ima 1° gg "data partenza del calcolo + 1"
068100170518      *     e
068200170518      *  nel campo spediz con ima 2° gg "data calcolo + 2"
068300170518      *     e
068400170518      *  nel campo spediz con ima oltre 2 gg in tutti i giorni fino
068500170518      *  alla "data fine calcolo -1 gg"
068600170518      *  (se <> 0 oppure fino alla data di finale del range elaborazione..)
068700170518     c                   z-add     bolla__DE     bolla__K
068800170518     c                   z-add     colli__DE     colli__K
068900170518     c                   z-add     peso___DE     peso___K
069000170518     c                   z-add     volume_DE     volume_K
069100170518      *
069200170512     c                   endsr
069300170512?     *--------------------------------------------------------------*
069400170519      * Controlla scrivendo il record sul WFIMAG0F
069500170518?     *--------------------------------------------------------------*
069600170519     c     Controlla     begsr
069700170518      *
069800170519     c                   clear                   wfIMAG00
069900170519     c                   eval       WFAAAS    = gcpAAS
070000170519     c                   eval       WFANGC    = gcpNGC
070100170523     c                   eval       WFAPRG    = gcpFRG
070200170519     c                   eval       WFAAASA   = gcpAASA
070300170519     c                   eval       WFALNPA   = gcpLNPA
070400170519     c                   eval       WFANRSA   = gcpNRSA
070500170519     c                   eval       WFANSPA   = gcpNSPA
070600170519     c                   eval       WFAGFGS   = gcpLNAA
070700170525     c                   eval       WFACCA    = tasCCA
070800170601     c                   eval       WFAGGS    = GSosta_BASE
070900170522     c                   eval       WFADOPN   = data_OPNgiac
071000170522     c                   eval       WFADINI   = data_INIelab
071100170519     c                   eval       WFADFIN   = data_FINEelab
071200170525     c                   eval       WFADMIC   = data_MICelab
071300170523     c                   eval       WFADEVE   = data_EVEnto
071400170523     c                   eval       WFADCHI   = data_CLOgiac
071500170522     c                   eval       WFADDCM   = data_Consegna
071600170519     c                   eval       WFADENT   = data_ENTR_IMG
071700170519     c                   eval       WFAFLDA   = bolla__A
071800170519     c                   eval       WFAFLDB   = bolla__B
071900170519     c                   eval       WFAFLDC   = bolla__C
072000170519     c                   eval       WFAFLDDE  = bolla__DE
072100170519     c                   eval       WFAFLDF   = bolla__F
072200170519     c                   eval       WFAFLDG   = bolla__G
072300170519     c                   eval       WFAFLDH   = bolla__H
072400170519     c                   write     wfIMAG00
072500170518      *
072600170518     c                   endsr
072700170518?     *--------------------------------------------------------------*
072800170519      * aggiorna il D.B.
072900170519?     *--------------------------------------------------------------*
073000170519     c     Aggiorna      begsr
073100170519      *
073200170519      * con DATA e FILIALE arrivo
073300170519     C                   eval      data8 =  data_INIelab
073400170519     C                   eval      fil3  =  gcpLNAA
073500170607     c     kDataFil      chain     tnIMAG0L
073600170519      *
073700170607     c                   if        %Found(tnIMAG0L)
073800170519      *  AGGIORNA
073900170519     C                   add       bolla__BASE   IMAGNBL
074000170519     C                   add       colli__BASE   IMAGNCL
074100170519     C                   add       peso___BASE   IMAGPKL
074200170519     C                   add       volume_BASE   IMAGVLU
074300170601     C******             add       GSosta_BASE   IMAGGGS
074400170519     C                   add       bolla__A      IMAGNBLR
074500170519     C                   add       colli__A      IMAGNCLR
074600170519     C                   add       peso___A      IMAGPKLR
074700170519     C                   add       volume_A      IMAGVLUR
074800170519     C                   add       bolla__B      IMAGNBL1R
074900170519     C                   add       colli__B      IMAGNCL1R
075000170519     C                   add       peso___B      IMAGPKL1R
075100170519     C                   add       volume_B      IMAGVLU1R
075200170519     C                   add       bolla__C      IMAGNBL2R
075300170519     C                   add       colli__C      IMAGNCL2R
075400170519     C                   add       peso___C      IMAGPKL2R
075500170519     C                   add       volume_C      IMAGVLU2R
075600170519     C                   add       bolla__DE     IMAGBAPR
075700170519     C                   add       colli__DE     IMAGCAPR
075800170519     C                   add       peso___DE     IMAGPAPR
075900170519     C                   add       volume_DE     IMAGVAPR
076000170519     C                   add       bolla__F      IMAGNBLG
076100170519     C                   add       colli__F      IMAGNCLG
076200170519     C                   add       peso___F      IMAGPKLG
076300170519     C                   add       volume_F      IMAGVLUG
076400170519     C                   add       bolla__G      IMAGBGP
076500170519     C                   add       colli__G      IMAGCGP
076600170519     C                   add       peso___G      IMAGPGP
076700170519     C                   add       volume_G      IMAGVGP
076800170519     C                   add       bolla__H      IMAGBGD
076900170519     C                   add       colli__H      IMAGCGD
077000170519     C                   add       peso___H      IMAGPGD
077100170519     C                   add       volume_H      IMAGVGD
077200170519     c                   update    tnIMAG00
077300170519      *
077400170519     c                   else
077500170519      *  SCRIVE
077600170519      *    il record per la DATA Elaborazione
077700170519     c                   clear                   tnIMAG00
077800170519     C                   eval      IMAGDTA = data_INIelab
077900170519     C                   eval      IMAGFGS = gcpLNAA
078000170530     c                   eval      IMAGFDES  = DESfil
078100170519      *
078200170519     C                   eval      IMAGNBL    =  bolla__BASE
078300170519     C                   eval      IMAGNCL    =  colli__BASE
078400170519     C                   eval      IMAGPKL    =  peso___BASE
078500170519     C                   eval      IMAGVLU    =  volume_BASE
078600170601     C*******            eval      IMAGGGS    =  GSosta_BASE
078700170519     C                   eval      IMAGICE    =  0
078800170519     C                   eval      IMAGNBLR   =  bolla__A
078900170519     C                   eval      IMAGNCLR   =  colli__A
079000170519     C                   eval      IMAGPKLR   =  peso___A
079100170519     C                   eval      IMAGVLUR   =  volume_A
079200170519     C                   eval      IMAGNBL1R  =  bolla__B
079300170519     C                   eval      IMAGNCL1R  =  colli__B
079400170519     C                   eval      IMAGPKL1R  =  peso___B
079500170519     C                   eval      IMAGVLU1R  =  volume_B
079600170519     C                   eval      IMAGNBL2R  =  bolla__C
079700170519     C                   eval      IMAGNCL2R  =  colli__C
079800170519     C                   eval      IMAGPKL2R  =  peso___C
079900170519     C                   eval      IMAGVLU2R  =  volume_C
080000170519     C                   eval      IMAGBAPR   =  bolla__DE
080100170519     C                   eval      IMAGCAPR   =  colli__DE
080200170519     C                   eval      IMAGPAPR   =  peso___DE
080300170519     C                   eval      IMAGVAPR   =  volume_DE
080400170519     C                   eval      IMAGNBLG   =  bolla__F
080500170519     C                   eval      IMAGNCLG   =  colli__F
080600170519     C                   eval      IMAGPKLG   =  peso___F
080700170519     C                   eval      IMAGVLUG   =  volume_F
080800170519     C                   eval      IMAGBGP    =  bolla__G
080900170519     C                   eval      IMAGCGP    =  colli__G
081000170519     C                   eval      IMAGPGP    =  peso___G
081100170519     C                   eval      IMAGVGP    =  volume_G
081200170519     C                   eval      IMAGBGD    =  bolla__H
081300170519     C                   eval      IMAGCGD    =  colli__H
081400170519     C                   eval      IMAGPGD    =  peso___H
081500170519     C                   eval      IMAGVGD    =  volume_H
082800170519     c                   write     tnIMAG00
082900170519     c                   end
083000170519      *
083100170519      *  Contatori aggiunti I,J,K
083200170519     c                   exsr      AltriContatori
083300170519      *
083400170519     c                   endsr
083500170519?     *--------------------------------------------------------------*
083600170519      *  deve aggiornare altri contatori su altre date
083700170519?     *--------------------------------------------------------------*
083800170519     c     AltriContatoriBEGsr
083900170519      *
084000170519      *   campi per il calcolo
084100170519     c                   clear                   data_elab_1       8 0
084200170519     c                   clear                   data_elab_2       8 0
084300170519      **
084400170526     c* data Inizio
084500170526     c                   eval      data_elab_1 = data8
084600170519      **
084700170526     c* data Inizio +1gg. lavorativo
084800170519     c                   clear                   xgiolavds
084900170519     c                   eval      IXGLDATA = data_elab_1
085000170519     c                   eval      IXGLfil  = fil3
085100170519     c                   eval      IXGLpa   ='A'
085200170519     c                   eval      IXGLadd  ='S'
085300170519     c                   eval      IXGLgg   = 1
085400170519     c                   eval      IXGLlav  ='S'
085500170519     c                   call      'XGIOLAV'
085600170519     c                   parm                    xgiolavds
085700170519    5c                   if        oxglerr=' '
085800170519     c                   eval      data_elab_2 = oxgldata
085900170519    5c                   endif
086000170519      *----------------
086100170519      *  su 1gg successivo al giorno dell'INI_elaborazione
086200170519     c                   if         bolla__B  > 0      or
086300170519     c                              bolla__C  > 0      or
086400170519     c                              bolla__DE > 0
086500170519     c                   exsr      Dopo_1_giorno
086600170519     c                   end
086700170519      *
086800170519      *  su 2gg successivi al giorno dell'INI_elaborazione
086900170519     c                   if         bolla__C  > 0      or
087000170519     c                              bolla__DE > 0
087100170519     c                   exsr      Dopo_2_giorni
087200170519     c                   end
087300170519      *
087400170519      *  su  gg successivi al 2°giorno dell'INI_elaborazione
087500170519     c                   if         bolla__DE > 0
087600170519     c                   exsr      Oltre_2_giorni
087700170519     c                   end
087800170519      *
087900170519     c                   endsr
088000170519?     *--------------------------------------------------------------*
088100170519      *  dopo 1 giorno dalla data elaborazione
088200170519?     *--------------------------------------------------------------*
088300170519     c     dopo_1_giorno BEGSR
088400170519      *
088500170519      * con DATA e FILIALE arrivo
088600170519     C                   eval      data8 =  data_elab_1
088700170607     c     kDataFil      chain     tnIMAG5L
088800170519      *
088900170607     c                   if        %Found(tnIMAG0L)
089000170519      *  AGGIORNA
089100170519     C                   add       bolla__BASE   IMAGNBL1
089200170519     C                   add       colli__BASE   IMAGNCL1
089300170519     C                   add       peso___BASE   IMAGPKL1
089400170519     C                   add       volume_BASE   IMAGVLU1
089500170607     c                   update    tnIMAG50
089600170519      *
089700170519     c                   else
089800170519      *  SCRIVE
089900170519      *    il record per la DATA Elaborazione
090000170607     c                   clear                   tnIMAG50
090100170519     C                   eval      IMAGDTA = data_elab_1
090200170519     C                   eval      IMAGFGS = gcpLNAA
090300170530     c     IMAGFGS       chain     azorg01l
090400170530     c                   if        %Found(azorg01l)
090500170530     c                   movel     orgdes        IMAGFdes
090600170530     c                   end
090700170519     C                   add       bolla__BASE   IMAGNBL1
090800170519     C                   add       colli__BASE   IMAGNCL1
090900170519     C                   add       peso___BASE   IMAGPKL1
091000170519     C                   add       volume_BASE   IMAGVLU1
091100170519      *
091200170607     c                   write     tnIMAG50
091300170519     c                   end
091400170519      *
091500170519     c                   endsr
091600170519?     *--------------------------------------------------------------*
091700170519      *  dopo 2 giorni dalla data elaborazione
091800170519?     *--------------------------------------------------------------*
091900170519     c     dopo_2_giorni BEGSR
092000170519      *
092100170519      * con DATA e FILIALE arrivo
092200170519     C                   eval      data8 =  data_elab_2
092300170607     c     kDataFil      chain     tnIMAG5L
092400170519      *
092500170607     c                   if        %Found(tnIMAG5L)
092600170519      *  AGGIORNA
092700170519     C                   add       bolla__BASE   IMAGNBL2
092800170519     C                   add       colli__BASE   IMAGNCL2
092900170519     C                   add       peso___BASE   IMAGPKL2
093000170519     C                   add       volume_BASE   IMAGVLU2
093100170607     c                   update    tnIMAG50
093200170519      *
093300170519     c                   else
093400170519      *  SCRIVE
093500170519      *    il record per la DATA Elaborazione
093600170607     c                   clear                   tnIMAG50
093700170519     C                   eval      IMAGDTA = data_elab_2
093800170519     C                   eval      IMAGFGS = gcpLNAA
093900170530     c     IMAGFGS       chain     azorg01l
094000170530     c                   if        %Found(azorg01l)
094100170530     c                   movel     orgdes        IMAGFdes
094200170530     c                   end
094300170519     C                   add       bolla__BASE   IMAGNBL2
094400170519     C                   add       colli__BASE   IMAGNCL2
094500170519     C                   add       peso___BASE   IMAGPKL2
094600170519     C                   add       volume_BASE   IMAGVLU2
094700170519      *
094800170607     c                   write     tnIMAG50
094900170519     c                   end
095000170519      *
095100170519     c                   endsr
095200170519?     *--------------------------------------------------------------*
095300170519      * oltre 2 giorni dalla data elaborazione
095400170519?     *--------------------------------------------------------------*
095500170519     c     oltre_2_giorniBEGSR
095600170519      *
095700170526      * Imposta il Giorno LIMITE a cui arrivare
095800170519     c                   clear                   data_LIMITE       8 0
095900170526      **
096000170526      ** se c'è la data fine
096100170519     c                   if        data_FINEelab > 0
096200170526     c                   eval      data_LIMITE = data_FINEelab
096300170526      ** altrimenti
096400170526      **  prende la data di fine range passata in KPJBU
096500170519     c                   else
096600170519     c                   eval      data_LIMITE = i2aDATA
096700170519     c                   end
096800170526      *
096900170519      **  Da data 2 giorni dopo
097000170519     c                   eval      data8 = data_elab_2
097100170519      **
097200170519     c     altro_giorno  tag
097300170519     c*  +1gg. lavorativo
097400170519     c                   clear                   xgiolavds
097500170519     c                   eval      IXGLDATA = data8
097600170519     c                   eval      IXGLfil  = fil3
097700170519     c                   eval      IXGLpa   ='A'
097800170519     c                   eval      IXGLadd  ='S'
097900170519     c                   eval      IXGLgg   = 1
098000170519     c                   eval      IXGLlav  ='S'
098100170519     c                   call      'XGIOLAV'
098200170519     c                   parm                    xgiolavds
098300170519    5c                   if        oxglerr=' '
098400170519     c                   eval      data8 = oxgldata
098500170519    5c                   endif
098600170519      **
098700170519      ** supera la data limite deve USCIRE!!!!!!!
098800170519    5c                   if        data8 > data_LIMITE
098900170519     c                   LEAVESR
099000170519     c                   end
099100170519      **
099200170519      * con DATA e FILIALE arrivo
099300170607     c     kDataFil      chain     tnIMAG5L
099400170519      *
099500170607     c                   if        %Found(tnIMAG5L)
099600170519      *  AGGIORNA
099700170519     C                   add       bolla__BASE   IMAGNBLO
099800170519     C                   add       colli__BASE   IMAGNCLO
099900170519     C                   add       peso___BASE   IMAGPKLO
100000170519     C                   add       volume_BASE   IMAGVLUO
100100170607     c                   update    tnIMAG50
100200170519      *
100300170519     c                   else
100400170519      *  SCRIVE
100500170519      *    il record per la DATA Elaborazione
100600170607     c                   clear                   tnIMAG50
100700170519     C                   eval      IMAGDTA = data8
100800170519     C                   eval      IMAGFGS = gcpLNAA
100900170530     c     IMAGFGS       chain     azorg01l
101000170530     c                   if        %Found(azorg01l)
101100170530     c                   movel     orgdes        IMAGFdes
101200170530     c                   end
101300170519     C                   add       bolla__BASE   IMAGNBLO
101400170519     C                   add       colli__BASE   IMAGNCLO
101500170519     C                   add       peso___BASE   IMAGPKLO
101600170519     C                   add       volume_BASE   IMAGVLUO
101700170519      *
101800170607     c                   write     tnIMAG50
101900170519     c                   end
102000170519      *
102100170519     c                   goto      altro_giorno
102200170519      *
102300170519     c                   endsr
102400170525?     *--------------------------------------------------------------*
102500170526      * IMPORTO VOCI 262+267
102600170525?     *--------------------------------------------------------------*
102700170526     c     Importo_VOCI  begsr
102800170526      *
102900170607     c                   open      TNIMAG0L
103000170526      *
103100170526      *  rilegge tutto e aggiorna il valore del costo con le VOCI
103200170607     c                   read      TNIMAG0L
103300170607     c                   dow       not %EoF(TNIMAG0L)
103400170526      **
103500170526      **   ci sono delle spedizioni da valorizzare
103600170526     c                   if        IMAGNBLR  > 0 or
103700170526     c                             IMAGNBL1R > 0 or
103800170526     c                             IMAGNBL2R > 0 or
103900170526     c                              IMAGBAPR > 0
104000170601      **
104100170529      **  Peso Movimentato totale
104200170529     c                   eval      pesoKG = IMAGPKLR  + IMAGPKL1R
104300170529     c                                    + IMAGPKL2R + IMAGPAPR
104400170529      **
104500170529      *   tariffa a peso della voce 262
104600170526     c                   clear                   tariffa_262      13 5
104700170529     C                   z-add     262           EVDVOC
104800170529     C                   z-add     PesoKG        Scaglione
104900170529     c                   exsr      TARIFFA_VOCE
105000170529     c                   z-add     tariffa       tariffa_262
105100170601      **
105200170601      **  Rileva i giorni di sosta per il calcolo della voce 267
105300170601     c                   clear                   GSosta_BASE
105400170601     C/EXEC SQL
105500170601     C+ SELECT sum(WFAGGS) INTO :GSosta_BASE
105600170601     C+    FROM wfimag0f WHERE
105700170601     C+ WFAGFGS = :IMAGFGS and
105800170601     C+ WFADINI = :IMAGDTA
105900170601     C/END-EXEC
106000170601      **
106100170601      **  tariffa per i GIORNI di SOSTA della voce 267
106200170529     c                   clear                   tariffa_267      13 5
106300170529     C                   z-add     267           EVDVOC
106400170601     C                   z-add     GSosta_BASE   Scaglione
106500170529     c                   exsr      TARIFFA_VOCE
106600170529     c                   z-add     tariffa       tariffa_267
106700170529      **
106800170529      **  Peso + Gg.Sosta totali x Filiale/giorno
106900170529     c                   eval      IMAGice = PesoKG * tariffa_262
107000170601     c                                + GSosta_BASE * tariffa_267
107100170526      **
107200170529     c                   update    TNIMAG00
107300170526     c                   end
107400170526      **
107500170526      *
107600170607     c                   read      TNIMAG0L
107700170526     c                   enddo
107800170525      *
107900170525     c                   endsr
108000170526?     *--------------------------------------------------------------*
108100170529      *   CERCA la tariffa della VOCE
108200170519?     *--------------------------------------------------------------*
108300170529     c     TARIFFA_VOCE  begsr
108400170526      *
108500170526      *-- di FILIALE
108600170526     c                   clear                   trovato_EVD       1
108700170529     c                   clear                   tariffa          13 5
108800170526     C                   move      'F'           EVDTTV
108900170526     C                   z-add     IMAGFGS       EVDFT1
109000170529      *--
109100170526      ** tenta max 2 volte
109200170526     c                   do        2             volte             3 0
109300170526      *--
109400170526     C     KVOCI         setll     ECEVD05L
109500170526     C     KVOCI         reade     ECEVD05L
109600170526     c                   dow       not %EoF(ECEVD05L)
109700170529      * in vigore
109800170526     c                   if        imagDTA >= EVDDDT and
109900170526     c                             imagDTA <= EVDDST
110000170529      **  x Scaglione
110100170529     c                   if        Scaglione >= EVDSGI and
110200170529     c                             Scaglione <= EVDSGF
110300170529      **                                        *-------*
110400170529     c                   z-add     EVDITR        tariffa
110500170529      **                                        *--------*
110600170529     c                   eval        trovato_EVD ='S'
110700170526     c                   end
110800170529     c                   end
110900170526      *
111000170526     C     KVOCI         reade     ECEVD05L
111100170526     c                   enddo
111200170526      *-- di CARTELLO
111300170526     c                   if          trovato_EVD <>*blank
111400170526     c                   LEAVE
111500170526     c                   else
111600170526     C                   move      'C'           EVDTTV
111700170526     C                   z-add     0             EVDFT1
111800170526     c                   end
111900170526      *
112000170526     c                   enddo
112100170526      *
112200170526     c                   endsr
112300170526?     *--------------------------------------------------------------*
