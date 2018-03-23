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
001100170607     FTNIMAG5L  uF a E           K DISK    usropn
001200170607     F
001300170526     FECevd05L  IF   E           K DISK
001400170511     D*------------------
001500170511     D* PASSAGGIO PARAMETRI
001600170511     D*------------------
001700170512     D KPJBA         E DS
001800170512     D FNLGI2ds      E DS
001900170512     D tigcpDS       E DS                  extname(tigcp00f)
002000170512     D*
002100170512     D WrkSqlCmd       S           1024
002200170511     D*
002300160822     D codntc          S              3    DIM(250)
002400070629
002500160822     D ds2a          E DS
002600170424     D ds2g2         E DS
002700170323     D og150         E DS
002800170323     D fnlv55ds      E DS
002900170519     d xgiolavds     e ds
003000170518
003100160822     D WDAT8           DS
003200160822     D  DATADA                 1      8  0
003300160822     D  DATAA                  9     16  0
003400160822     D  GGL                   17     21  0
003500170518
003600160822     D tfptfa          DS
003700160822     D  p_tfp                  1      3  0
003800160822     D  p_tfa                  4      6  0
003900170518
004000160927     d waddgg          s              3  0
004100170518
004200161005     d ktcod           s                   like(tbecod) inz('FLG')
004300161005     d ktke1           s                   like(tbeke1)
004400160926     d Datasys         s               d   datfmt(*iso) inz(*sys)
004500170512     d Data_ISO        s               d   datfmt(*iso)
004600170519      **-- campi chiave
004700170519     d  data8          s              8  0
004800170519     d  fil3           s              3  0
004900170529     d  pesoKG         s             13  3
005000170529     d  Scaglione      s             13  3
005001170616     d NR_Spedizioni   s              9  0
005100170519      *
005200170519     D*______________________________________________
005300170518     D Contatori       DS
005400170518     D  bolla__BASE                   8  0
005500170518     D  colli__BASE                   8  0
005600170518     D  peso___BASE                  13  1
005700170518     D  volume_BASE                  13  3
005800170529     D  GSosta_BASE                   7  0
005900170518     D  bolla__A                      8  0
006000170518     D  colli__A                      8  0
006100170518     D  peso___A                     13  1
006200170518     D  volume_A                     13  3
006300170518     D  bolla__B                      8  0
006400170518     D  colli__B                      8  0
006500170518     D  peso___B                     13  1
006600170518     D  volume_B                     13  3
006700170518     D  bolla__C                      8  0
006800170518     D  colli__C                      8  0
006900170518     D  peso___C                     13  1
007000170518     D  volume_C                     13  3
007100170518     D  bolla__DE                     8  0
007200170518     D  colli__DE                     8  0
007300170518     D  peso___DE                    13  1
007400170518     D  volume_DE                    13  3
007500170518     D  bolla__F                      8  0
007600170518     D  colli__F                      8  0
007700170518     D  peso___F                     13  1
007800170518     D  volume_F                     13  3
007900170518     D  bolla__G                      8  0
008000170518     D  colli__G                      8  0
008100170518     D  peso___G                     13  1
008200170518     D  volume_G                     13  3
008300170518     D  bolla__H                      8  0
008400170518     D  colli__H                      8  0
008500170518     D  peso___H                     13  1
008600170518     D  volume_H                     13  3
008700170518     D  bolla__I                      8  0
008800170518     D  colli__I                      8  0
008900170518     D  peso___I                     13  1
009000170518     D  volume_I                     13  3
009100170518     D  bolla__J                      8  0
009200170518     D  colli__J                      8  0
009300170518     D  peso___J                     13  1
009400170518     D  volume_J                     13  3
009500170518     D  bolla__K                      8  0
009600170518     D  colli__K                      8  0
009700170518     D  peso___K                     13  1
009800170518     D  volume_K                     13  3
009900170519      *
010000170512     C*---------------------------------------------------------------*
010100170512     C     *ENTRY        PLIST
010200170512     C                   PARM                    KPJBA
010300170512     C                   MOVEL     KPJBU         FNLGI2DS
010400170512     C                   SETON                                        LR
010500170512     C*---------------------------------------------------------------*
010600170607      * Inizia l'analisi da meno 1 settimana
010700170607     c                   movel     i2DaData      Data_ISO
010800170607     c                   subdur    7:*d          data_iso
010900170607     c                   movel     Data_ISO      Da_meno7gg        8 0
011000170607     C*---------------------------------------------------------------*
011100170512     c* Carico i codici di mancata consegna
011200170512     c                   eval      kcod='2A'
011300170512     c                   eval      codut=1
011400170512     c                   clear                   xx
011500170512     c                   open      tabel00f
011600170512      **
011700170512     c     ktab          setll     tabel00f
011800170512     c     ktab          reade     tabel00f
011900170512     c                   dow       not %eof(tabel00f)
012000170512     c                   eval      ds2a=tbluni
012100170512     c                   if        §2antc='S'  or (§2atip<>'§'
012200170512     c                             and §2atip<>'S')
012300170512     c                   add       1             xx                3 0
012400170512     c                   movel     tblkey        codntc(xx)
012500170512     c                   endif
012600170512     c     ktab          reade     tabel00f
012700170512     c                   enddo
012800170512     c
012900170512     c* chaino DS2G per il peso limite sopra il quale va subito in IMG
013000170512     c                   clear                   ds2g2
013100170512     c                   eval      kcod='2G'
013200170512     c                   eval      kkey='2       '
013300170512     c     ktab2         chain     tabel00f
013400170512     c                   if        %found(tabel00f)
013500170512     c                   eval      ds2g2=tbluni
013600170512     c                   else
013700170512     c                   eval      §2g2kgimg=50
013800170512     c                   endif
013900170512     c
014000170512     c                   close     tabel00f
014100170512     c
014200170518     c                   clear                   wfgs              3 0
014300170518     c                   clear                   picking           1
014400170512      *
014500170518     C* DEFINIZIONE CHIAVI -------------------------------------------------
014600170512     C     Ktab          KLIST
014700170512     C                   KFLD                    codut             1 0
014800170512     C                   KFLD                    kcod              2
014900170512      *
015000170512     C     Ktab2         KLIST
015100170512     C                   KFLD                    codut             1 0
015200170512     C                   KFLD                    kcod              2
015300170512     C                   KFLD                    kkey              8
015400170512      *
015500170512     C     Ktbe          KLIST
015600170512     C                   KFLD                    ktcod
015700170512     C                   KFLD                    ktke1
015800170512     C*
015900170512     C     Karrivo       KLIST
016000170512     C                   KFLD                    GCPAASa
016100170512     C                   KFLD                    GCPLNPa
016200170512     C                   KFLD                    GCPNRSa
016300170512     C                   KFLD                    GCPNSPa
016400170519      *
016500170519     C     KDataFil      KLIST
016600170526     C                   KFLD                    data8
016700170526     C                   KFLD                    fil3
016800170526      *
016900170526     C     KVOCI         KLIST
017000170526     C                   KFLD                    EVDVOC
017100170526     C                   KFLD                    EVDTTV
017200170526     C                   KFLD                    EVDFT1
017300170519      *
017400170512?     *--------------------------------------------------------------*
017500170512     C*   data di sistema
017600170512     c                   movel     datasys       dateu             8 0
017700170512     C*
017800170512?     *--------------------------------------------------------------*
017900170519     C/EXEC SQL
018000170519     C+ DELETE FROM WFIMAG0F
018100170519     C/END-EXEC
018200170519     C*
018300170512     C/EXEC SQL
018400170526     C+ DELETE FROM TNIMAG0F
018500170512     C/END-EXEC
018600170607     C*
018700170607     C/EXEC SQL
018800170607     C+ DELETE FROM TNIMAG5F
018900170607     C/END-EXEC
019000170607     c                   open      TNIMAG0L
019100170607     c                   open      TNIMAG5L
019200170512     C*
019300170512     c                   clear                   almenoUno         1
019400170512     C*
019500170512     c* ---------------------
019600170512      **** con SQL lettura delle GIACENZE senza RIAPERTURA
019700170512     C                   EVAL      WrkSqlCmd ='SELECT * FROM tigcp00f '     +
019800170512     C                             ' WHERE GCPRIAP = '' '' and '            +
019900170512     C                             ' GCPAGC * 10000 + GCPMGC between '      +
020000170607     C                             %editc(Da_Meno7gg:'X') + ' and '         +
020100170607     C                                %editc(I2ADATA:'X') + ' and ('        +
020200170526     C                             %editc(I2FILIALE:'X') + ' = 0 or '       +
020300170526     C                             %editc(I2FILIALE:'X') + ' = GCPLNAA) '   +
020400170526     C                             ' ORDER BY GCPAGC, GCPMGC '
020500170512     c* ---------------------
020600170512     C/EXEC SQL
020700170512     C+ PREPARE S1 FROM :WrkSqlCmd
020800170512     C/END-EXEC
020900170512
021000170512     C/EXEC SQL
021100170512     C+ DECLARE A1 CURSOR FOR S1
021200170512     C/END-EXEC
021300170512
021400170512     C/EXEC SQL
021500170512     C+ OPEN A1
021600170512     C/END-EXEC
021700170512      * Forzo la stampa del JOBLOG.
021800170512     C                   If        SqlCod < 0
021900170512     C                   CALL      'X66CHGJOB'
022000170512     C                   RETURN
022100170512     C                   End
022200170512      *>>>>>>>>>>>>>
022300170512     C                   DOU       SqlCod <> 0
022400170512     C/EXEC SQL
022500170512     C+ FETCH NEXT FROM A1 INTO :tigcpDS
022600170512     C/END-EXEC
022700170512     C                   SELECT
022800170512     c*
022900170512     c* a fine file Totali x rotture
023000170512     C                   WHEN      SqlCod = 100
023100170512     c                   leave
023200170512     **   ERRORI
023300170512     C                   WHEN      SqlCod < 0
023400170512     C                   seton                                        H1
023500170512     c                   goto      fine
023600170512     c*
023700170512     C                   OTHER
023800170512     c*
023900170512     c* controlla se il record di giacenza è da considerare
024000170519     c                   clear                   rec_OK            1
024100170512     c                   exsr      check_REC
024200170529     c*
024300170529      ** calcola data ELABORAZIONE
024400170529     c                   if          rec_OK <> 'N'
024500170529     c                   exsr      calc_DATAINI
024600170529     c                   end
024700170512      **
024800170512      **  Se il record è buono lo elabora in dettaglio
024900170519     c                   if          rec_OK <> 'N'
025000170529      **
025100170518     c* imposta Date di riferimento per il calcolo
025200170529     c                   exsr      DATE_diRIFER
025300170512     c* DETTAGLIO
025400170512     c                   exsr      dettaglio
025500170519     c* record di controllo
025600170519     c                   exsr      controlla
025700170529      **
025800170519     c* aggiorna il file di Statistica
025900170519     c                   exsr      aggiorna
026000170519     c                   move      'S'           almenouno
026100170529      **
026200170512     c                   end
026300170512     c*
026400170512     C                   ENDSL
026500170512
026600170512     C                   ENDDO
026700170512      *>>>>>>>>>>>>>
026800170512     C/EXEC SQL
026900170512     C+ CLOSE A1
027000170512     C/END-EXEC
027100170607     c                   close     TNIMAG0L
027200170607     c                   close     TNIMAG5L
027300170529      *>>>>>>>>>>>>>
027400170512     C* totali generali
027500170526     c                   if            almenouno ='S'
027600170526     c                   exsr      Importo_VOCI
027700170512     c                   end
027800170512      *
027900170512     c     fine          tag
028000170512     C                   Return
028100170529?     *--------------------------------------------------------------*
028200170529      * Check se l record letto è da scartare
028300170529?     *--------------------------------------------------------------*
028400170529     c     Check_REC     begsr
028500170530      *****
028600170530      * (0)
028700170530      *****    C'è un errore che andrà a posto (Michele lo sta sistemando)
028800170530     c*  quindi per ora escludiamo le giacenze in fase 40 e senza disposizioni
028900170530     c*  del MITTENTE. (questo filtro diventerà inutile)
029000170530     c                   if        GCPFAS =40 and GCPDDM =0
029100170530     c                   eval             rec_OK = 'N'
029200170530     c                   leavesr
029300170530    1c                   endif
029400170529      *****
029500170529      * (1)
029600170529      *****    Se la Filiale NON ha il PICKING -> il RECORD è da SCARTARE
029700170529     c                   exsr      FIL_fa_il_Pick
029800170529     c                   if        picking = 'N'
029900170529     c                   eval             rec_OK = 'N'
030000170529     c                   leavesr
030100170529    1c                   endif
030200170529      *****
030300170529      * (2)
030400170529      *****    Alcune Filiali non hanno IMA -> il RECORD è da SCARTARE
030500170529     c* prima controllo lnp poi ccm
030600170529     c                   eval      ktke1=%editc(GCPLNPA:'X')
030700170529     c     ktbe          chain     tntbe01l
030800170529     c                   if        not %found(tntbe01l) or tbeatb<>' '
030900170529     c* alcuni clienti vanno subito a img
031000170529     c                   eval      ktke1=%editc(GCPSCM:'X')
031100170529     c     ktbe          chain     tntbe01l
031200170529     c                   endif
031300170529     c                   if        %found(tntbe01l) and tbeatb=' '
031400170529     c                   eval             rec_OK = 'N'
031500170529     c                   leavesr
031600170529    1c                   endif
031700170529      *****
031800170529      * (3)
031900170529      *****    Se il peso superiore alla 2G -> il RECORD è da SCARTARE
032000170529     c                   clear                   Peso_Sped         7 1
032100170529     c                   clear                   Vol_Sped          5 3
032200170529     c     karrivo       chain     titas30c
032300170529      **
032400170529    1c                   if        %found(titas30c)
032500170529      * prende il peso
032600170529     c                   eval      Peso_Sped  = taspkf
032700170529     c                   if          taspkc > 0 and tasncp = tasncl
032800170529     c                                or
032900170529     c                               taspkc > 0 and tasncp < tasncl
033000170529     c                               and taspkc > taspkf
033100170529     c                   eval      Peso_Sped  = taspkc
033200170529     c                   end
033300170529      ** se supera esce
033400170529    1c                   if        Peso_Sped > §2g2kgimg
033500170529     c                   eval             rec_OK = 'N'
033600170529     c                   leavesr
033700170529    1c                   endif
033800170529     **
033900170529      * prende il volume
034000170529     c                   eval      Vol_Sped  = tasvlf
034100170529     c                   if          tasvlc > 0 and tasncr = tasncl
034200170529     c                                or
034300170529     c                               tasvlc > 0 and tasncr < tasncl
034400170529     c                               and tasvlc > tasvlf
034500170529     c                   eval      Vol_Sped  = tasvlc
034600170529     c                   end
034700170529      **
034800170529    1c                   endif
034900170529      *
035000170529     c                   endsr
035100170529?     *--------------------------------------------------------------*
035200170529     C     FIL_fa_il_PickBEGSR
035300170529?     *--------------------------------------------------------------*
035400170529      **  La Filiale GESTISCE il PICKING?
035500170529      *
035600170529     c                   clear                   picking
035700170529     c                   movel     GCPFGC        wfgs              3 0
035800170530     c                   clear                   desfil           30
035900170529      * tramite Terminal
036000170529     c                   clear                   fnlv55ds
036100170529     c                   eval      d55lin=GCPFGC
036200170529     c                   eval      d55tpt='6'
036300170529     c                   movel     dateu         d55drf
036400170529     c                   call      'FNLV55R'
036500170529     c                   parm                    fnlv55ds
036600170529     c                   movel     d55tfa        wfgs
036700170529     c
036800170529     c     wfgs          chain     azorg01l
036900170530     c                   if        %Found(azorg01l)
037000170529     c                   movel     orgdf0        og150
037100170530     c                   movel     orgdes        desfil
037200170529     c                   eval      picking=§OGPCK
037300170529     c                   if        picking=' '
037400170529     c                   eval      picking='N'
037500170529     c                   endif
037600170530     c                   endif
037700170529     c*
037800170529     c                   ENDSR
037900170512?     *--------------------------------------------------------------*
038000170512      * Calcola DATA Elaborazione
038100170512?     *--------------------------------------------------------------*
038200170529     c     Calc_DATAINI  begsr
038300170511      *
038400170518     c                   clear                   data_INIelab      8 0
038500170522     c                   clear                   data_OPNgiac      8 0
038600170523     c                   clear                   data_CLOgiac      8 0
038700170523     c                   clear                   data_EVEnto       8 0
038800170525     c                   clear                   data_MICelab      8 0
038900170529     c                   clear                   data_Consegna     8 0
039000170529     c                   eval      data_Consegna = tasDCM
039100170512      *
039200170512      * intanto come prima cosa  imposta come DATA ELABORAZIONE
039300170512      *     la  DATA APERTURA GIACENZA
039400170529     c                   eval      data_OPNgiac = gcpAGC
039500170529     c                                          * 10000 + gcpMGC
039600170529     c                   eval      data_INIelab = data_OPNgiac
039700170523     c                   eval      data_CLOgiac =  GCPDCG
039800170529      **-----
039900170529      **
040000170512      * poi bisogna vedere se l'EVENTO di MANCATA CONSEGNA faccia
040100170512      *  considerare diversamente la DATA ELABORAZIONE aggiungendo
040200170512      *   un giorno al calcolo.
040300170512      **
040400170512     c     karrivo       setgt     fnevb01l
040500170512     c     karrivo       readpe    fnevb01l
040600170512    1c                   dow       not %eof(fnevb01l)
040700170525      *
040800170525      * Prende l'ultimo MIC letto prima dell'eventuale stato di Mancata
040900170525      * consegna.
041000170529     c                   if        evbCEV = 'MIC'
041100170525     c                   eval        data_MICelab =evbDEV
041200170529     c                   endif
041300170512
041400170512    2c                   if        %lookup(evbcev:codntc)>0
041500170512     c                             and %subst(evbnot:8:7)>*zeros
041600170525      *-
041700170512     c* trovato evento di mancata consegna --> verifico la data rispetto
041800170512     c*  alla data giacenza
041900170518    3c                   if        data_INIelab  = evbDEV
042000170529     c                   eval      data_EVEnto = evbDEV
042100170523     c*  +1gg. lavorativo
042200170523     C                   eval      fil3  =  gcpLNAA
042300170523     c                   clear                   xgiolavds
042400170523     c                   eval      IXGLDATA = data_INIelab
042500170523     c                   eval      IXGLfil  = fil3
042600170523     c                   eval      IXGLpa   ='A'
042700170525     c                   eval      IXGLadd  ='S'
042800170523     c                   eval      IXGLgg   = 1
042900170523     c                   eval      IXGLlav  ='S'
043000170523     c                   call      'XGIOLAV'
043100170523     c                   parm                    xgiolavds
043101170609      ** Al momento si sospende l'aggiunta di 1gg. x aver trovato
043102170609      ** un evento di mancata consegna
043200170529     c                   if        oxglerr=' '
043300170609     c******             eval      data_INIelab = oxgldata
043400170529     c                   endif
043500170524      **
043600170524     c                   leave
043700170512    3c                   endif
043800170523      **
043900170512    2c                   endif
044000170512     c
044100170512     c     karrivo       readpe    fnevb01l
044200170512    1c                   enddo
044300170529      **
044400170529      **-----
044500170529      *****
044600170529      * (4)
044700170529      *****    Se fosse presente un periodo di sospensione e la data
044800170529      *  ELABORAZIONE fosse compresa nell'intervallo -> il RECORD è da SCARTARE
044900170529     c                   if            §2G2DIMAi >  0  and
045000170529     c                             data_INIelab  >= §2G2DIMAi and
045100170529     c                             data_INIelab  <= §2G2DIMAf
045200170529     c                   eval             rec_OK = 'N'
045300170529     c                   leavesr
045400170529     c                   endif
045500170512      *
045600170512     c                   endsr
045700170512?     *--------------------------------------------------------------*
045800170518      * imposta le date di RIFERIMENTO
045900170512?     *--------------------------------------------------------------*
046000170529     c     DATE_diRIFER  begsr
046100170512      *
046200170518      *------------
046300170518      ** calcola data FINE ELABORAZIONE
046400170518     c                   clear                   data_FINEelab     8 0
046500170518      *
046600170518      *  chiamo data fine calcolo, la data chiusura giacenza se PRG <>0,
046700170518      *        altrimenti DCM da ARB se consegnata
046800170518     c                   if        gcpFRG > 0
046900170525     c                              or
047000170525     c                             tasCCA <> *blank
047100170529     c                   eval        data_FINEelab = data_CLOgiac
047200170518     c                   else
047300170523      *
047400170518      ** se c'è la data Consegna su TITAS
047500170529     c                   if        data_Consegna > 0
047600170525      *
047700170529     c                   eval      data_FINEelab = data_Consegna
047800170523     **
047900170523     *** toglie   1gg. per i calcoli a seguire
048000170529     c                   if          data_FINEelab > data_INIelab
048100170523     C                   eval      fil3  =  gcpLNAA
048200170523     c                   clear                   xgiolavds
048300170523     c                   eval      IXGLDATA = data_FINEelab
048400170523     c                   eval      IXGLfil  = fil3
048500170523     c                   eval      IXGLpa   ='A'
048600170525     c                   eval      IXGLsub  ='S'
048700170523     c                   eval      IXGLgg   = 1
048800170523     c                   eval      IXGLlav  ='S'
048900170523     c                   call      'XGIOLAV'
049000170523     c                   parm                    xgiolavds
049100170523    5c                   if        oxglerr=' '
049200170529    5c                               and
049300170529     c                             data_INIelab <= oxgldata
049400170523     c                   eval      data_FINEelab = oxgldata
049500170523    5c                   endif
049600170518     c                   end
049700170529     c                   end
049800170518      *
049900170525     c                   endif
050000170518      *------------
050100170518      ** data ENTRATA MAGAZZINO GIACENZE
050200170518     c                   clear                   data_ENTR_IMG     8 0
050300170518      *
050400170518     c                   if         GCPDLMA > 0 and  GCPDLMA<99999999
050500170518     c                   eval      data_ENTR_IMG = GCPDLMA
050600170518     c                   end
050700170518      *
050800170518     c                   ENDSR
050900170512?     *--------------------------------------------------------------*
051000170525      * DATA_con_MIC
051100170512?     *--------------------------------------------------------------*
051200170525     c     DATA_con_MIC  begsr
051300170518      *
051400170525      * sottra 1 gg. data Fine Calcolo
051500170525     c                   clear                   xgiolavds
051600170525     c                   eval      IXGLDATA = data_FINEelab
051700170525     c                   eval      IXGLfil  = fil3
051800170525     c                   eval      IXGLpa   ='A'
051900170525     c                   eval      IXGLsub  ='S'
052000170525     c                   eval      IXGLgg   = 1
052100170525     c                   eval      IXGLlav  ='S'
052200170525     c                   call      'XGIOLAV'
052300170525     c                   parm                    xgiolavds
052400170525      *
052500170525    5c                   if        oxglerr=' '
052600170525    5c                               and
052700170525     c                             data_INIelab <= oxgldata
052800170525     c                   eval      data_FINEelab = oxgldata
052900170525    5c                   endif
053000170525      *
053100170525     c                   ENDSR
053200170525?     *--------------------------------------------------------------*
053300170525      * Dettaglio
053400170525?     *--------------------------------------------------------------*
053500170525     c     Dettaglio     begsr
053600170525      *
053700170518      *  Pulisce i contatori x la singola riga
053800170518     c                   clear                   contatori
053900170518      *
054000170518     c                   z-add     1             bolla__BASE
054100170518     c                   z-add     TASncl        colli__BASE
054200170518     c                   z-add     Peso_Sped     peso___BASE
054300170518     c                   z-add     Vol_Sped      volume_BASE
054400170529      *  solo se NON negativi
054500170601     c                   clear                   GSosta_BASE
054600170529     c                   if           TASggs >0
054700170529     c                   z-add     TASggs        GSosta_BASE
054800170529     c                   end
054900170518      *
055000170518      *  SE NON ENTRATO in IMG quindi ANCORA in IMA
0551001705180    c                   IF          data_ENTR_IMG = 0
055200170518      *
055300170518      *  Se data di FINE non c'è
055400170518      **     oltre 2 gg. NON essendoci la data di FINE
0555001705181    c                   if        data_FINEelab = 0
055600170518      **   '"> 2 IMA"
055700170518     c                   z-add     bolla__BASE   bolla__DE
055800170518     c                   z-add     colli__BASE   colli__DE
055900170518     c                   z-add     peso___BASE   peso___DE
056000170518     c                   z-add     volume_BASE   volume_DE
056100170530      *
056200170530      *  azzera il MIC x non fare confusione
056300170530     c                   eval      data_MICelab = 0
056400170530      *
0565001705181x   c                   else
056600170525      **
056700170525      ** attenzione se c'era un MIC contestuale alla data FINE
056800170525     c                   if        data_FINEelab = data_MICelab
056900170525     c                              and
057000170525     c                             data_FINEelab > data_INIelab
057100170525      ** sottraggo 1gg.data fine
0572001705251x   c                   exsr      DATA_con_MIC
057300170525     c                   else
057400170525     c                   eval      data_MICelab = 0
057500170525     c                   end
057600170529      **
057700170529      ** FORZATURA:
057800170529      **  X parare casi pietosi dove le PRECEDENTI considerazioni
057900170529      **  mi portano ad avere una data CHIUSURA precedente ad APERTURA
058000170529      **   ILLOGICO
058100170529      **  allora si deve imporre la data APERTURA in apertura e chiusura
058200170529     c                   if        data_FINEelab < data_INIelab
058300170529     c                   eval      data_INIelab  = data_OPNgiac
058400170529     c                   eval      data_FINEelab = data_OPNgiac
058500170529     c                   end
058600170529      **
058700170529      **
058800170525      * calcola sempre quindi i giorni trascorsi
058900170525     c                   clear                   wdat8
059000170525     c                   clear                   tfptfa
059100170525     c                   movel     data_INIelab  datada
059200170525     c                   movel     data_FINEelab dataa
059300170525     c                   z-add     GCPLNAA       p_tfa
059400170525     c                   call      'XSRLAV8'
059500170525     c                   parm                    wdat8
059600170525     c                   parm                    tfptfa
059700170525      **
059800170518      * Con data FINE
059900170518      *  Se inizia e finisce nello stesso giorno
060000170523      *  o causa evento (ha aggiunto un giorno) quindi il giorno prima
0601001705232    c                   if        data_FINEelab <= data_INIelab
060200170525     c                               and
060300170525     c                             data_Consegna > 0
060400170518      * "risolte subito"
060500170518     c                   z-add     bolla__BASE   bolla__A
060600170518     c                   z-add     colli__BASE   colli__A
060700170518     c                   z-add     peso___BASE   peso___A
060800170518     c                   z-add     volume_BASE   volume_A
0609001705182x   c                   else
061000170518      **  se 1 gg.
061100170518      **  "+IMA -IMG" (1° gg)
061200170518     c                   if        GGL = 1
061300170518     c                   z-add     bolla__BASE   bolla__B
061400170518     c                   z-add     colli__BASE   colli__B
061500170518     c                   z-add     peso___BASE   peso___B
061600170518     c                   z-add     volume_BASE   volume_B
061700170518      **  se 2 gg.
061800170518      **  "+IMA -IMG" (2° gg)
061900170518     c                   elseif    GGL = 2
062000170518     c                   z-add     bolla__BASE   bolla__C
062100170518     c                   z-add     colli__BASE   colli__C
062200170518     c                   z-add     peso___BASE   peso___C
062300170518     c                   z-add     volume_BASE   volume_C
062400170518      **  se oltre 2 gg.
062500170518      **  "> 2 IMA" ciò si verifica per 2 motivi:
062600170518      **  1.  DLO accetta di non portare in IMG se già immesse dispo in ARRIVO.
062700170518      **  2.  "Anomalia di procedura".
062800170518     c                   elseif    GGL > 2
062900170518     c                   z-add     bolla__BASE   bolla__DE
063000170518     c                   z-add     colli__BASE   colli__DE
063100170518     c                   z-add     peso___BASE   peso___DE
063200170518     c                   z-add     volume_BASE   volume_DE
063300170518     c                   endif
063400170518      **
0635001705182e   c                   end
0636001705181e   c                   endif
063700170518      *
0638001705180x   C                   ELSE
063900170518      * Se ENTRATO in IMG
064000170518      **
064100170518      * calcola i giorni trascorsi
064200170518     c                   clear                   wdat8
064300170518     c                   clear                   tfptfa
064400170518     c                   movel     data_INIelab  datada
064500170518     c                   movel     data_ENTR_IMG dataa
064600170518     c                   z-add     GCPLNAA       p_tfa
064700170518     c                   call      'XSRLAV8'
064800170518     c                   parm                    wdat8
064900170518     c                   parm                    tfptfa
065000170518      **  se 3 gg.
065100170518      ** "+2 IMA" (3° gg)
065200170518     c                   if        GGL = 3
065300170518     c                   z-add     bolla__BASE   bolla__F
065400170518     c                   z-add     colli__BASE   colli__F
065500170518     c                   z-add     peso___BASE   peso___F
065600170518     c                   z-add     volume_BASE   volume_F
065700170518      **  se <3 gg.
065800170518      ** "variazione di peso o forzata IMG o tabella 2G periodo sospensione"
065900170518     c                   elseif    GGL < 3
066000170518     c                   z-add     bolla__BASE   bolla__G
066100170518     c                   z-add     colli__BASE   colli__G
066200170518     c                   z-add     peso___BASE   peso___G
066300170518     c                   z-add     volume_BASE   volume_G
066400170518      **  se >3 gg.
066500170518      ** "anomalia di procedura"
066600170518     c                   elseif    GGL > 3
066700170518     c                   z-add     bolla__BASE   bolla__H
066800170518     c                   z-add     colli__BASE   colli__H
066900170518     c                   z-add     peso___BASE   peso___H
067000170518     c                   z-add     volume_BASE   volume_H
067100170518     c                   endif
067200170518      **
0673001705180e   C                   END
067400170518      *
067500170518      *  ULTERIORI CONSIDERAZIONI
067600170518      *   Occorre calcolare inoltre le spedizioni
067700170518      *    che giornalmente vengono "imate"
067800170518      *
067900170518      *   nel campo spediz con ima 1° gg "data partenza del calcolo + 1"
068000170518     c                   z-add     bolla__B      bolla__I
068100170518     c                   z-add     colli__B      colli__I
068200170518     c                   z-add     peso___B      peso___I
068300170518     c                   z-add     volume_B      volume_I
068400170518      *
068500170518      *   nel campo spediz con ima 1° gg "data partenza del calcolo + 1"
068600170518      *      e
068700170518      *   nel campo spediz con ima 2° gg "data calcolo + 2"
068800170518     c                   z-add     bolla__C      bolla__J
068900170518     c                   z-add     colli__C      colli__J
069000170518     c                   z-add     peso___C      peso___J
069100170518     c                   z-add     volume_C      volume_J
069200170518      *
069300170518      *  nel campo spediz con ima 1° gg "data partenza del calcolo + 1"
069400170518      *     e
069500170518      *  nel campo spediz con ima 2° gg "data calcolo + 2"
069600170518      *     e
069700170518      *  nel campo spediz con ima oltre 2 gg in tutti i giorni fino
069800170518      *  alla "data fine calcolo -1 gg"
069900170518      *  (se <> 0 oppure fino alla data di finale del range elaborazione..)
070000170518     c                   z-add     bolla__DE     bolla__K
070100170518     c                   z-add     colli__DE     colli__K
070200170518     c                   z-add     peso___DE     peso___K
070300170518     c                   z-add     volume_DE     volume_K
070400170518      *
070500170512     c                   endsr
070600170512?     *--------------------------------------------------------------*
070700170519      * Controlla scrivendo il record sul WFIMAG0F
070800170518?     *--------------------------------------------------------------*
070900170519     c     Controlla     begsr
071000170518      *
071100170519     c                   clear                   wfIMAG00
071200170519     c                   eval       WFAAAS    = gcpAAS
071300170519     c                   eval       WFANGC    = gcpNGC
071400170523     c                   eval       WFAPRG    = gcpFRG
071500170519     c                   eval       WFAAASA   = gcpAASA
071600170519     c                   eval       WFALNPA   = gcpLNPA
071700170519     c                   eval       WFANRSA   = gcpNRSA
071800170519     c                   eval       WFANSPA   = gcpNSPA
071900170519     c                   eval       WFAGFGS   = gcpLNAA
072000170525     c                   eval       WFACCA    = tasCCA
072100170601     c                   eval       WFAGGS    = GSosta_BASE
072200170522     c                   eval       WFADOPN   = data_OPNgiac
072300170522     c                   eval       WFADINI   = data_INIelab
072400170519     c                   eval       WFADFIN   = data_FINEelab
072500170525     c                   eval       WFADMIC   = data_MICelab
072600170523     c                   eval       WFADEVE   = data_EVEnto
072700170523     c                   eval       WFADCHI   = data_CLOgiac
072800170522     c                   eval       WFADDCM   = data_Consegna
072900170519     c                   eval       WFADENT   = data_ENTR_IMG
073000170519     c                   eval       WFAFLDA   = bolla__A
073100170519     c                   eval       WFAFLDB   = bolla__B
073200170519     c                   eval       WFAFLDC   = bolla__C
073300170519     c                   eval       WFAFLDDE  = bolla__DE
073400170519     c                   eval       WFAFLDF   = bolla__F
073500170519     c                   eval       WFAFLDG   = bolla__G
073600170519     c                   eval       WFAFLDH   = bolla__H
073700170519     c                   write     wfIMAG00
073800170518      *
073900170518     c                   endsr
074000170518?     *--------------------------------------------------------------*
074100170519      * aggiorna il D.B.
074200170519?     *--------------------------------------------------------------*
074300170519     c     Aggiorna      begsr
074400170519      *
074500170519      * con DATA e FILIALE arrivo
074600170519     C                   eval      data8 =  data_INIelab
074700170519     C                   eval      fil3  =  gcpLNAA
074800170607      **
074900170607      ** scrive solo se nel range immesso a video
075000170607     c                   IF        data8 >= i2DaData and
075100170607     c                             data8 <= i2aData
075200170607      **
075300170607     c     kDataFil      chain     tnIMAG0L
075400170519      *
075500170607     c                   if        %Found(tnIMAG0L)
075600170519      *  AGGIORNA
075700170519     C                   add       bolla__BASE   IMAGNBL
075800170519     C                   add       colli__BASE   IMAGNCL
075900170519     C                   add       peso___BASE   IMAGPKL
076000170519     C                   add       volume_BASE   IMAGVLU
076100170601     C******             add       GSosta_BASE   IMAGGGS
076200170519     C                   add       bolla__A      IMAGNBLR
076300170519     C                   add       colli__A      IMAGNCLR
076400170519     C                   add       peso___A      IMAGPKLR
076500170519     C                   add       volume_A      IMAGVLUR
076600170519     C                   add       bolla__B      IMAGNBL1R
076700170519     C                   add       colli__B      IMAGNCL1R
076800170519     C                   add       peso___B      IMAGPKL1R
076900170519     C                   add       volume_B      IMAGVLU1R
077000170519     C                   add       bolla__C      IMAGNBL2R
077100170519     C                   add       colli__C      IMAGNCL2R
077200170519     C                   add       peso___C      IMAGPKL2R
077300170519     C                   add       volume_C      IMAGVLU2R
077400170519     C                   add       bolla__DE     IMAGBAPR
077500170519     C                   add       colli__DE     IMAGCAPR
077600170519     C                   add       peso___DE     IMAGPAPR
077700170519     C                   add       volume_DE     IMAGVAPR
077800170519     C                   add       bolla__F      IMAGNBLG
077900170519     C                   add       colli__F      IMAGNCLG
078000170519     C                   add       peso___F      IMAGPKLG
078100170519     C                   add       volume_F      IMAGVLUG
078200170519     C                   add       bolla__G      IMAGBGP
078300170519     C                   add       colli__G      IMAGCGP
078400170519     C                   add       peso___G      IMAGPGP
078500170519     C                   add       volume_G      IMAGVGP
078600170519     C                   add       bolla__H      IMAGBGD
078700170519     C                   add       colli__H      IMAGCGD
078800170519     C                   add       peso___H      IMAGPGD
078900170519     C                   add       volume_H      IMAGVGD
079000170519     c                   update    tnIMAG00
079100170519      *
079200170519     c                   else
079300170519      *  SCRIVE
079400170519      *    il record per la DATA Elaborazione
079500170519     c                   clear                   tnIMAG00
079600170519     C                   eval      IMAGDTA = data_INIelab
079700170519     C                   eval      IMAGFGS = gcpLNAA
079800170530     c                   eval      IMAGFDES  = DESfil
079900170519      *
080000170519     C                   eval      IMAGNBL    =  bolla__BASE
080100170519     C                   eval      IMAGNCL    =  colli__BASE
080200170519     C                   eval      IMAGPKL    =  peso___BASE
080300170519     C                   eval      IMAGVLU    =  volume_BASE
080400170601     C*******            eval      IMAGGGS    =  GSosta_BASE
080500170519     C                   eval      IMAGICE    =  0
080600170519     C                   eval      IMAGNBLR   =  bolla__A
080700170519     C                   eval      IMAGNCLR   =  colli__A
080800170519     C                   eval      IMAGPKLR   =  peso___A
080900170519     C                   eval      IMAGVLUR   =  volume_A
081000170519     C                   eval      IMAGNBL1R  =  bolla__B
081100170519     C                   eval      IMAGNCL1R  =  colli__B
081200170519     C                   eval      IMAGPKL1R  =  peso___B
081300170519     C                   eval      IMAGVLU1R  =  volume_B
081400170519     C                   eval      IMAGNBL2R  =  bolla__C
081500170519     C                   eval      IMAGNCL2R  =  colli__C
081600170519     C                   eval      IMAGPKL2R  =  peso___C
081700170519     C                   eval      IMAGVLU2R  =  volume_C
081800170519     C                   eval      IMAGBAPR   =  bolla__DE
081900170519     C                   eval      IMAGCAPR   =  colli__DE
082000170519     C                   eval      IMAGPAPR   =  peso___DE
082100170519     C                   eval      IMAGVAPR   =  volume_DE
082200170519     C                   eval      IMAGNBLG   =  bolla__F
082300170519     C                   eval      IMAGNCLG   =  colli__F
082400170519     C                   eval      IMAGPKLG   =  peso___F
082500170519     C                   eval      IMAGVLUG   =  volume_F
082600170519     C                   eval      IMAGBGP    =  bolla__G
082700170519     C                   eval      IMAGCGP    =  colli__G
082800170519     C                   eval      IMAGPGP    =  peso___G
082900170519     C                   eval      IMAGVGP    =  volume_G
083000170519     C                   eval      IMAGBGD    =  bolla__H
083100170519     C                   eval      IMAGCGD    =  colli__H
083200170519     C                   eval      IMAGPGD    =  peso___H
083300170519     C                   eval      IMAGVGD    =  volume_H
083400170519     c                   write     tnIMAG00
083500170519     c                   end
083600170607     c                   endIF
083700170519      *
083800170519      *  Contatori aggiunti I,J,K
083900170519     c                   exsr      AltriContatori
084000170519      *
084100170519     c                   endsr
084200170519?     *--------------------------------------------------------------*
084300170519      *  deve aggiornare altri contatori su altre date
084400170519?     *--------------------------------------------------------------*
084500170519     c     AltriContatoriBEGsr
084600170519      *
084700170519      *   campi per il calcolo
084800170519     c                   clear                   data_elab_1       8 0
084900170519     c                   clear                   data_elab_2       8 0
085000170519      **
085100170526     c* data Inizio
085200170526     c                   eval      data_elab_1 = data8
085300170519      **
085400170526     c* data Inizio +1gg. lavorativo
085500170519     c                   clear                   xgiolavds
085600170519     c                   eval      IXGLDATA = data_elab_1
085700170519     c                   eval      IXGLfil  = fil3
085800170519     c                   eval      IXGLpa   ='A'
085900170519     c                   eval      IXGLadd  ='S'
086000170519     c                   eval      IXGLgg   = 1
086100170519     c                   eval      IXGLlav  ='S'
086200170519     c                   call      'XGIOLAV'
086300170519     c                   parm                    xgiolavds
086400170519    5c                   if        oxglerr=' '
086500170519     c                   eval      data_elab_2 = oxgldata
086600170519    5c                   endif
086700170519      *----------------
086800170519      *  su 1gg successivo al giorno dell'INI_elaborazione
086900170519     c                   if         bolla__B  > 0      or
087000170519     c                              bolla__C  > 0      or
087100170519     c                              bolla__DE > 0
087200170519     c                   exsr      Dopo_1_giorno
087300170519     c                   end
087400170519      *
087500170519      *  su 2gg successivi al giorno dell'INI_elaborazione
087600170519     c                   if         bolla__C  > 0      or
087700170519     c                              bolla__DE > 0
087800170519     c                   exsr      Dopo_2_giorni
087900170519     c                   end
088000170519      *
088100170519      *  su  gg successivi al 2°giorno dell'INI_elaborazione
088200170519     c                   if         bolla__DE > 0
088300170519     c                   exsr      Oltre_2_giorni
088400170519     c                   end
088500170519      *
088600170519     c                   endsr
088700170519?     *--------------------------------------------------------------*
088800170519      *  dopo 1 giorno dalla data elaborazione
088900170519?     *--------------------------------------------------------------*
089000170519     c     dopo_1_giorno BEGSR
089100170519      *
089200170519      * con DATA e FILIALE arrivo
089300170519     C                   eval      data8 =  data_elab_1
089400170607      **
089500170607      ** scrive solo se nel range immesso a video
089600170607     c                   IF        data8 >= i2DaData and
089700170607     c                             data8 <= i2aData
089800170607      **
089900170607     c     kDataFil      chain     tnIMAG5L
090000170519      *
090100170607     c                   if        %Found(tnIMAG5L)
090200170519      *  AGGIORNA
090300170519     C                   add       bolla__BASE   IMAGNBL1
090400170519     C                   add       colli__BASE   IMAGNCL1
090500170519     C                   add       peso___BASE   IMAGPKL1
090600170519     C                   add       volume_BASE   IMAGVLU1
090700170607     c                   update    tnIMAG50
090800170519      *
090900170519     c                   else
091000170519      *  SCRIVE
091100170519      *    il record per la DATA Elaborazione
091200170607     c                   clear                   tnIMAG50
091300170519     C                   eval      IMAGDTA = data_elab_1
091400170519     C                   eval      IMAGFGS = gcpLNAA
091500170530     c     IMAGFGS       chain     azorg01l
091600170530     c                   if        %Found(azorg01l)
091700170530     c                   movel     orgdes        IMAGFdes
091800170530     c                   end
091900170519     C                   add       bolla__BASE   IMAGNBL1
092000170519     C                   add       colli__BASE   IMAGNCL1
092100170519     C                   add       peso___BASE   IMAGPKL1
092200170519     C                   add       volume_BASE   IMAGVLU1
092300170519      *
092400170607     c                   write     tnIMAG50
092500170519     c                   end
092600170519      *
092700170607     c                   endIF
092800170607      *
092900170519     c                   endsr
093000170519?     *--------------------------------------------------------------*
093100170519      *  dopo 2 giorni dalla data elaborazione
093200170519?     *--------------------------------------------------------------*
093300170519     c     dopo_2_giorni BEGSR
093400170519      *
093500170519      * con DATA e FILIALE arrivo
093600170519     C                   eval      data8 =  data_elab_2
093700170607      **
093800170607      ** scrive solo se nel range immesso a video
093900170607     c                   IF        data8 >= i2DaData and
094000170607     c                             data8 <= i2aData
094100170607      **
094200170607     c     kDataFil      chain     tnIMAG5L
094300170519      *
094400170607     c                   if        %Found(tnIMAG5L)
094500170519      *  AGGIORNA
094600170519     C                   add       bolla__BASE   IMAGNBL2
094700170519     C                   add       colli__BASE   IMAGNCL2
094800170519     C                   add       peso___BASE   IMAGPKL2
094900170519     C                   add       volume_BASE   IMAGVLU2
095000170607     c                   update    tnIMAG50
095100170519      *
095200170519     c                   else
095300170519      *  SCRIVE
095400170519      *    il record per la DATA Elaborazione
095500170607     c                   clear                   tnIMAG50
095600170519     C                   eval      IMAGDTA = data_elab_2
095700170519     C                   eval      IMAGFGS = gcpLNAA
095800170530     c     IMAGFGS       chain     azorg01l
095900170530     c                   if        %Found(azorg01l)
096000170530     c                   movel     orgdes        IMAGFdes
096100170530     c                   end
096200170519     C                   add       bolla__BASE   IMAGNBL2
096300170519     C                   add       colli__BASE   IMAGNCL2
096400170519     C                   add       peso___BASE   IMAGPKL2
096500170519     C                   add       volume_BASE   IMAGVLU2
096600170519      *
096700170607     c                   write     tnIMAG50
096800170519     c                   end
096900170519      *
097000170607     c                   endIF
097100170607      *
097200170519     c                   endsr
097300170519?     *--------------------------------------------------------------*
097400170519      * oltre 2 giorni dalla data elaborazione
097500170519?     *--------------------------------------------------------------*
097600170519     c     oltre_2_giorniBEGSR
097700170519      *
097800170526      * Imposta il Giorno LIMITE a cui arrivare
097900170519     c                   clear                   data_LIMITE       8 0
098000170526      **
098100170526      ** se c'è la data fine
098200170519     c                   if        data_FINEelab > 0
098300170526     c                   eval      data_LIMITE = data_FINEelab
098400170526      ** altrimenti
098500170526      **  prende la data di fine range passata in KPJBU
098600170519     c                   else
098700170519     c                   eval      data_LIMITE = i2aDATA
098800170519     c                   end
098900170526      *
099000170519      **  Da data 2 giorni dopo
099100170519     c                   eval      data8 = data_elab_2
099200170519      **
099300170519     c     altro_giorno  tag
099400170519     c*  +1gg. lavorativo
099500170519     c                   clear                   xgiolavds
099600170519     c                   eval      IXGLDATA = data8
099700170519     c                   eval      IXGLfil  = fil3
099800170519     c                   eval      IXGLpa   ='A'
099900170519     c                   eval      IXGLadd  ='S'
100000170519     c                   eval      IXGLgg   = 1
100100170519     c                   eval      IXGLlav  ='S'
100200170519     c                   call      'XGIOLAV'
100300170519     c                   parm                    xgiolavds
100400170519    5c                   if        oxglerr=' '
100500170519     c                   eval      data8 = oxgldata
100600170519    5c                   endif
100700170519      **
100800170519      ** supera la data limite deve USCIRE!!!!!!!
100900170519    5c                   if        data8 > data_LIMITE
101000170519     c                   LEAVESR
101100170519     c                   end
101200170519      **
101300170607      ** scrive solo se nel range immesso a video
101400170607     c                   IF        data8 >= i2DaData and
101500170607     c                             data8 <= i2aData
101600170607      **
101700170519      * con DATA e FILIALE arrivo
101800170607     c     kDataFil      chain     tnIMAG5L
101900170519      *
102000170607     c                   if        %Found(tnIMAG5L)
102100170519      *  AGGIORNA
102200170519     C                   add       bolla__BASE   IMAGNBLO
102300170519     C                   add       colli__BASE   IMAGNCLO
102400170519     C                   add       peso___BASE   IMAGPKLO
102500170519     C                   add       volume_BASE   IMAGVLUO
102600170607     c                   update    tnIMAG50
102700170519      *
102800170519     c                   else
102900170519      *  SCRIVE
103000170519      *    il record per la DATA Elaborazione
103100170607     c                   clear                   tnIMAG50
103200170519     C                   eval      IMAGDTA = data8
103300170519     C                   eval      IMAGFGS = gcpLNAA
103400170530     c     IMAGFGS       chain     azorg01l
103500170530     c                   if        %Found(azorg01l)
103600170530     c                   movel     orgdes        IMAGFdes
103700170530     c                   end
103800170519     C                   add       bolla__BASE   IMAGNBLO
103900170519     C                   add       colli__BASE   IMAGNCLO
104000170519     C                   add       peso___BASE   IMAGPKLO
104100170519     C                   add       volume_BASE   IMAGVLUO
104200170519      *
104300170607     c                   write     tnIMAG50
104400170519     c                   end
104500170519      *
104600170607     c                   endIF
104700170607      *
104800170519     c                   goto      altro_giorno
104900170519      *
105000170519     c                   endsr
105100170525?     *--------------------------------------------------------------*
105200170526      * IMPORTO VOCI 262+267
105300170525?     *--------------------------------------------------------------*
105400170526     c     Importo_VOCI  begsr
105500170526      *
105600170607     c                   open      TNIMAG0L
105700170526      *
105800170526      *  rilegge tutto e aggiorna il valore del costo con le VOCI
105900170607     c                   read      TNIMAG0L
106000170607     c                   dow       not %EoF(TNIMAG0L)
106100170526      **
106200170526      **   ci sono delle spedizioni da valorizzare
106300170526     c                   if        IMAGNBLR  > 0 or
106400170526     c                             IMAGNBL1R > 0 or
106500170526     c                             IMAGNBL2R > 0 or
106600170526     c                              IMAGBAPR > 0
106700170601      **
106701170616      **  Rileva i giorni di sosta per il calcolo della voce 267
106702170616     c                   clear                   GSosta_BASE
106703170616     c                   clear                   NR_Spedizioni
106704170616     c
106705170616     C/EXEC SQL
106706170616     C+ SELECT sum(WFAGGS),
106708170616     C+        sum(WFAFLDA+ WFAFLDB+ WFAFLDC+ WFAFLDDE)
106709170616     C+   INTO :GSosta_BASE, :NR_Spedizioni
106710170616     C+   FROM wfimag0f
106711170616     C+  WHERE
106713170616     C+ WFAGFGS = :IMAGFGS and
106714170616     C+ WFADINI = :IMAGDTA
106715170616     C+     and WFAPRG =  0
106716170616     C/END-EXEC
106717170616      **  imposto un peso per reperire la tariffa
106718170616      *     (tanto è unico quindi metto 100)
106719170616     c                   eval      pesoKG = 100
106800170529      **  Peso Movimentato totale
106900170616     c******                   eval      pesoKG = IMAGPKLR  + IMAGPKL1R
107000170616     c******                                    + IMAGPKL2R + IMAGPAPR
107100170529      **
107200170529      *   tariffa a peso della voce 262
107300170526     c                   clear                   tariffa_262      13 5
107400170529     C                   z-add     262           EVDVOC
107500170529     C                   z-add     PesoKG        Scaglione
107600170529     c                   exsr      TARIFFA_VOCE
107700170529     c                   z-add     tariffa       tariffa_262
107800170601      **
108800170601      **  tariffa per i GIORNI di SOSTA della voce 267
108900170529     c                   clear                   tariffa_267      13 5
109000170529     C                   z-add     267           EVDVOC
109100170601     C                   z-add     GSosta_BASE   Scaglione
109200170529     c                   exsr      TARIFFA_VOCE
109300170529     c                   z-add     tariffa       tariffa_267
109400170529      **
109500170529      **  Peso + Gg.Sosta totali x Filiale/giorno
109600170616     c                   eval      IMAGice = NR_Spedizioni * tariffa_262 * 100
109700170601     c                                + GSosta_BASE * tariffa_267
109800170526      **
109900170529     c                   update    TNIMAG00
110000170526     c                   end
110100170526      **
110200170526      *
110300170607     c                   read      TNIMAG0L
110400170526     c                   enddo
110500170525      *
110600170525     c                   endsr
110700170526?     *--------------------------------------------------------------*
110800170529      *   CERCA la tariffa della VOCE
110900170519?     *--------------------------------------------------------------*
111000170529     c     TARIFFA_VOCE  begsr
111100170526      *
111200170526      *-- di FILIALE
111300170526     c                   clear                   trovato_EVD       1
111400170529     c                   clear                   tariffa          13 5
111500170526     C                   move      'F'           EVDTTV
111600170526     C                   z-add     IMAGFGS       EVDFT1
111700170529      *--
111800170526      ** tenta max 2 volte
111900170526     c                   do        2             volte             3 0
112000170526      *--
112100170526     C     KVOCI         setll     ECEVD05L
112200170526     C     KVOCI         reade     ECEVD05L
112300170526     c                   dow       not %EoF(ECEVD05L)
112400170529      * in vigore
112500170526     c                   if        imagDTA >= EVDDDT and
112600170526     c                             imagDTA <= EVDDST
112700170529      **  x Scaglione
112800170529     c                   if        Scaglione >= EVDSGI and
112900170529     c                             Scaglione <= EVDSGF
113000170529      **                                        *-------*
113100170529     c                   z-add     EVDITR        tariffa
113200170529      **                                        *--------*
113300170529     c                   eval        trovato_EVD ='S'
113400170526     c                   end
113500170529     c                   end
113600170526      *
113700170526     C     KVOCI         reade     ECEVD05L
113800170526     c                   enddo
113900170526      *-- di CARTELLO
114000170526     c                   if          trovato_EVD <>*blank
114100170526     c                   LEAVE
114200170526     c                   else
114300170526     C                   move      'C'           EVDTTV
114400170526     C                   z-add     0             EVDFT1
114500170526     c                   end
114600170526      *
114700170526     c                   enddo
114800170526      *
114900170526     c                   endsr
115000170526?     *--------------------------------------------------------------*
