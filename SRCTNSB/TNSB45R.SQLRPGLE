000100060105     h Decedit('0,') Datedit(*dmy.) Option(*nodebugio)
000200060105
000300060105      *------------------------------------------------------------------------*
000400060105      *           Controllo manca Tariffa Filiale / Sede                       *
000500060105      *------------------------------------------------------------------------*
000600060105
000700060105      *------------------------------------------------------------------------*
000800060105      *   N O T E
000900060105      *------------------------------------------------------------------------*
001000060105      *
001100060105      *------------------------------------------------------------------------*
001200060105
001300060105     fAzorg01l  if   e           k Disk
001400060105     fCnaco00f  if   e           k Disk
001500060105     fCnind00f  if   e           k Disk
001600060105     fCnclp00f  if   e           k Disk
001700060120     fFiar531c  if   e           k Disk    usropn
001800060105     fFnevb04l  if   e           k Disk
001900130912     fAZCMM01L  if   e           k Disk
002000060126     fTabel00f  if   e           k Disk
002100060120     fTitas38c  if   e           k Disk    usropn
002200060105     f                                     ignore(Titas010)
002300060105     f                                     ignore(TitasP00)
002400060120     fTitas02l  if   e           k Disk    rename(Titas000:Titas2) usropn
002500060120     fTita730c  if   e           k Disk    usropn
002600060120     fTita430c  if   e           k Disk    usropn
002700060120     fTitaa30c  if   e           k Disk    usropn
002800060120     fTncsb03l  if   e           k Disk    usropn
002900060105
003000120217      //?File Manca Tariffa
003100120217     fTfmtc01l  uf a e           k Disk    usropn
003200120228     fWfmtc00f  o    e             Disk    usropn
003300130108
003400130108      //?Fatturato Giornaliero
003500160121     fTFFGC01L  uf a e           k Disk
003600161007
003700161007      //?Fatturato Giornaliero per Cliente
003800161007     fTFFGK01L  uf a e           k Disk
003900060105
004000060105      *------------------------------------------------------------------------*
004100060105      *   C A M P I   D I   L A V O R O
004200060105      *------------------------------------------------------------------------*
004300060105     d dateu           s              8  0
004400060105     d w0140           s             14  0
004500060105     d wdata           s              8  0
004600060111     d com04           s              4  0
004700060125     d monovaria       s              2
004800060111     d Yy              s              3  0
004900060112     d Kk              s              3  0
005000060117     d wunim           s             10  1
005100060117     d wunis           s             10  1
005200060117     d wunimt          s             11  1
005300060111     d Rec_ok          s              1                                         ' '=ok 'N'=No ok
005400060111     d TasPoKsc        s              3
005500060202     d SavPo           s              3  0
005600060202     d SavCar          s              3  0
005700060116     d Tipo_tar        s              1
005800060119     d Wtibs69r        s              1
005900060119     d Wtnsf20r        s              1
006000151001     d Wtnsf22r        s              1
006100060105     d codut           s              1  0 Inz(1)
006200120217     d wOggi_60        s              8  0
006300060111
006400060120     d wlib            s             10
006500061012     d wlibc           s              9
006600120313     d wlibw           s              9
006700120402     d comman          s             91
006800120402     d lenght          s             15  5 inz(91)
006900060120
007000060113     d w_dtas          s                   like(dtas)
007100060109     d Tabcod          s                   like(Tblcod)
007200060109     d Tabkey          s                   like(Tblkey)
007300060109     d KtasAAS         s                   like(Tasaas)
007400060109     d KtasMGS         s                   like(Tasmgs)
007500060109     d KtasKSC         s                   like(Tasksc)
007600060119     d KeyTrc          s                   like(Ta4trc)
007700060112     d SavKSC          s                   like(Tasksc)
007800060112     d SavRAG          s                   like(Mtcrag)
007900060112     d SavBAP          s                   like(Mtcbap)
008000060112     d SavCas          s                   like(Mtccas)
008100060112     d SavVca          s                   like(Mtcvca)
008200060112     d SavRma          s                   like(MtcRma)
008300060119     d SavPro          s                   like(MtcPro)
008400060123     d SavAge          s                   like(MtcAge)
008500060119     d §Tasrma         s                   like(MtcRma)
008600060112     d AnaERR          s                   like(Mtcerr)
008700060113     d kAr5Trd         s                   Like(Ar5Trd)
008800060113     d Wcev            s                   like(evbcev) inz('RIC')
008900090327     d StpNcl          s                   like(TasNcl)
009000120220     d wMTCtrc         s                   like(MTCtrc)
009100120307     d wMTCerr         s                   like(MTCerr)
009200120322     d wMTCesc         s                   like(MTCesc)
009300120322     d wMTCimp         s                   like(MTCimp)
009400120322     d wMTCias         s                   like(MTCias)
009500120322     d wMTCvas         s                   like(MTCvas)
009600120322     d wMTCass         s                   like(MTCass)
009700120322     d wMTCinm         s                   like(MTCinm)
009800120217
009900120222     d wDaMenu         s               n   inz(*off)
010000120224     d wDaSB48         s               n   inz(*off)
010100120220     d wEndMtc         s               n   inz(*off)
010200121003     d wProva          s               n   inz(*off)
010300120220     d wRcdMtc         s               n   inz(*off)
010400120224     d wSB48           s               n   inz(*off)
010500120413     d wVisto          s               n   inz(*off)
010600130108
010700130108     d wDataISO        s               d   datfmt(*iso)
010800130108     d wDataAl         s              8s 0
010900130108     d wDataDa         s              8s 0
011000130109     d wFIL            s              3s 0
011100130109     d wTassa          s               n   inz(*off)
011200130109     d wManca          s               n   inz(*off)
011300120307
011400120307       // -?Stringa SQL da eseguire
011500120307     d wSQL            s           2048    Varying        inz
011600060105
011700060105      *------------------------------------------------------------------------*
011800060105      *   S C H I E R E
011900060105      *------------------------------------------------------------------------*
012000060105
012100060110      * Errori manca tariffa
012200120907     d err             s             49    dim(15) ctdata perrcd(1)
012300060110      * Ovrprtf
012400060110     d cmd             s             80    dim(1) ctdata perrcd(1)
012500060120      * Ovrdbf
012600060126     d cmdt            s             60    dim(7) ctdata perrcd(1)
012700060120      * Crtdupobj
012800120224     d cmdc            s             90    dim(5) ctdata perrcd(1)              Cpyf Crtdupobj
012900120402     d cmdw            s             91    dim(5) ctdata perrcd(1)              Cpyf Crtdupobj
013000060110
013100060105      * P.O. da elaborare
013200060111     d Skpo            s              3    dim(250) inz(*Zeros)
013300060110      * Tipi bolla non soggette a tassazione
013400060110     d tbo             s              2    dim(30)
013500060110      * Codici tassazione
013600060110     d cts             s              2    dim(600)
013700060110      * Province legate ai Codici tassazione
013800060110     d cpr             s              2    dim(600)
013900060110      * Codici bolla che prevedono la tassazione di una varia fissa
014000060110     d cbo             s              2    dim(50)
014100060110      * Sigla varia legata a codici bolla CBO
014200060110     d cbv             s              1    dim(50)
014300060113      * Copia di DtasV
014400060113     d  w_tsv          s              1    dim(20)
014500060113     d  w_tva          s             11  3 dim(20)
014600060113     d  w_tev          s              1    dim(20)
014700141211
014800141211       //?Sk part.consegna con abilitazione PIN Code
014900141211     d skGMA           s              2a   dim(10)
015000060116
015100060116     d dataiso         s               d   datfmt(*iso)
015200060113
015300060105
015400060105      *------------------------------------------------------------------------*
015500060105      *   D S   I N T E R N E / E S T E R N E
015600060105      *------------------------------------------------------------------------*
015700060105
015800060116     d                 ds
015900060116     d  wdataf                 1      8  0
016000060116     d  waaf                   1      4  0
016100060116     d  wmmf                   5      6  0
016200060116     d  waammf                 1      6  0
016300060116     d  wggf                   7      8  0
016400060116     d                 ds
016500060116     d  waammi                 1      6  0
016600060116     d  waai                   1      4  0
016700060116     d  wmmi                   5      6  0
016800060116
016900060105     d                 ds
017000060105     d  TasDsp                 1      8  0
017100060105     d  TasAas                 1      4  0
017200060105     d  TasMgs                 5      8  0
017300060105
017400060105     d wlbdat          ds                  Inz
017500060105     d  g02dat                 1      8  0
017600060105     d  g02inv                 9     16  0
017700060105     d  g02err                17     17
017800060105     d  g02tgi                18     22  0
017900060119
018000060119      *
018100060119     d Paramt          ds
018200060119     d  parca                256    256
018300060105
018400060105     d Tnsb44ds      e ds                  inz
018500060105
018600060105     d Trul06ds      e ds
018700060105     d  l1                     1     90  0 Dim(30)
018800060105     d AzuteDs       e ds                  ExtName(AzUte00F)
018900060105     d DDatiUte      e ds
019000060105     d Tibs34Ds      e ds
019100060105     d Tibs02ds      e ds
019200060116     d Tise70ds      e ds
019300060119     d Trul31Ds      e ds
019400060119
019500060119     d Tibs69ds      e ds                  inz
019600060119     d DS_cnaco      e ds                  extname(Cnaco00f)
019700060119     d DS_cnind      e ds                  extname(Cnind00f)
019800060119     d DS_cnclp      e ds                  extname(Cnclp00f)
019900060119     d DS_fncls      e ds                  extname(Fncls00f)
020000120220
020100120220      // File TFMTC x sql
020200120220     d TFMTCDS       e ds                  extname(TFMTC00F)
020300060119
020400060119      * Ds x controllo limite importo da assicurare
020500060119     d Trul22Ds      e ds
020600060119      * Ds x tabella LIA
020700060119     d Dlia          e ds
020800060119
020900060113
021000060113     d dAr5Ban       e ds
021100060113     d dAr5Bnb       e ds
021200151127
021300151127      // - Rcd GEN file FIAR5
021400151127     d dAR5gen       e ds                  inz
021500060113
021600060109     d Dged          e ds
021700060109     d Dgei          e ds
021800060109     d Dstb          e ds                  occurs(30) inz
021900060110     d Dsct          e ds
022000060110     d Ds3a          e ds
022100060113     d Dscc          e ds
022200120328     d dIMP          e ds
022300130109
022400130109      // - Tabella 05 = Codici Area
022500130109     d ds05          e ds                  inz
022600130109      // - Tabella 17 = Direzioni commerciali
022700130109     d ds17          e ds                  inz
022800060202
022900060202     d Og141         e ds
023000070518     d Og143         e ds
023100060202     d Og144         e ds
023200060113
023300110701      ** DS del tasflo del file TITAS00F
023400110701     d DTASFLO       E DS
023500110701
023600060113     d Dtas          e ds                  prefix(§)
023700060926      *
023800060926     d DTAS01        E DS
023900130108
024000130108       //?Ds tabella RPF (data ultima spedizione fatturata)
024100130108     d dRPFfatgio    e ds
024200141211
024300141211       //?Ds tabella 7R - part.consegna
024400141211     d ds7R          e ds
024500130108
024600060113      *
024700060113     d Dtasv         e ds                  prefix(§)
024800060113     d   Tsv                   1     20    dim(20)                              Dtasv: Sigla Varia
024900060113     d   Tva                  21    140P 3 dim(20)                              Dtasv: Imp. Varia
025000060113     d   Tev                 141    160    dim(20)                              Dtasv: Esenz. Varia
025100060113      *
025200151001     d Dtaspes       e ds                  inz
025300060105
025400060105     d Kpjba         e ds
025500120217
025600120217      //---------------------------------------------------------------
025700120217      //?Definizione key-list.
025800120217      //---------------------------------------------------------------
025900120217
026000120217       // - File TFMTC01L
026100120220     d k06tfmtc      e ds                  extname(TFMTC01L:*key)
026200120217     d                                     prefix(k_)
026300120220
026400120220       // - File TITAS02L
026500120220     d k07titas      e ds                  extname(TITAS02L:*key)
026600120220     d                                     prefix(k_)
026700060105
026800060105      *------------------------------------------------------------------------*
026900060105      *   I N D I C A T O R I
027000060105      *------------------------------------------------------------------------*
027100060203
027200060203      * 05    Sede
027300060203      * 10    Bollo monovaria
027400060203      * 20    Stampa Imponibile
027500060203
027600060203      * 30    Errore generico
027700060203      * 31    Errore generico
027800060203      * 32    Errore generico
027900060203      * 51    Errore generico
028000060105      * 90    Errore generico
028100060203      * 99    Errore generico
028200060105
028300060105      *------------------------------------------------------------------------*
028400060105      * CICLO PRINCIPALE
028500060105      *------------------------------------------------------------------------*
028600060105
028700060110      * carico le tabelle ed imposto i punti operativi da elaborare
028800060110     c                   exsr      Sr_imposta
028900120217
029000120220      /free
029100120309
029200120309         exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
029300120309
029400120402       //?Pulizia dei dati da rielaborare e rielaborazione dei dati rimasti
029500120416       //?sul file TFMTC00F  (in caso di lancio schedulato SB47)
029600130108       //?pulizia anche dei dati relativi al fatturato giornaliero
029700120222       //?Se non è elaborazione da Menù
029800120222         IF  not wDaMenu;
029900120224         //?Se non è elaborazione da SB48
030000120224           IF  not wDaSB48;
030100120222         //?Pulisco (dal file in QTEMP) i dati che verranno rielaborati
030200120222         //?e le segnalazioni ormai obsolete (+ vecchie di 60 gg.)
030300130108         //?Pulisco anche i dati relativi al Fatturato Giornaliero
030400120224             exsr Pulisci_File;
030500120222         //?Rielaboro quello rimasto nel file in QTEMP
030600120306             exsr Rielabora_File;
030700120306             wRcdMtc = *off;
030800120306           ENDIF;
030900120222         ENDIF;
031000120402
031100120402       //?Rielaborazione dei dati presenti su TFMTC00F
031200120306       //?Se è elaborazione da SB48
031300120306         IF  wDaSB48;
031400120307         //?Preparo stringa SQL
031500120307           exsr PrepSql;
031600120306         //?Elaboro il file TFMTC con le richieste fatte a video
031700120306           exsr Elabora_TFMTC;
031800120306           wRcdMtc = *off;
031900120306         ENDIF;
032000120220      /end-free
032100120217
032200120416      *?Elaborazione della bolle da TITAS (lancio schedulato o stampa da menù)
032300120306      *?Se non è elaborazione da SB48
032400120306     c                   IF        not wDaSB48
032500060110     c                   exsr      Sr_Elabora
032600120306     c                   ENDIF
032700120402
032800120402      * stampa
032900120402      *?SOLO se è da menu
033000120416 NEW c                   IF        wDaMenu = *on
033100060206     c                   exsr      Sr_Stampa
033200120220     c                   ENDIF
033300120402
033400120220      /free
033500120402       //?Salvataggio del membro nel file WFMTC00F
033600120402       //?SOLO se elaborazione mensile/settimanale e NON da SB48
033700120402 NEW     IF  D44gio = *blanks and not wDaSB48
033800120402             and D44crf = 'S';
033900120402           exsr Salva_FileWF;
034000120402         ENDIF;
034100120402
034200120402       //?Copio TFMTC00F dalla QTEMP alla buona
034300120402       //?se non è da SB48 o da menù
034400120306 NEW     IF  not wDaMenu and not wDaSB48;
034500120221           exsr Copia_File;
034600120224         ENDIF;
034700120220      /end-free
034800060119
034900060119      * Chiudo i programmi di utilità
035000060119    1c                   IF        Wtnsf20r = *blanks
035100060119     c                   movel     'C'           §tastla                         'C'=CHIUDI
035200060119     c                   call      'TNSF20R'                            99
035300060119     c                   parm                    Kpjba
035400060119     c                   parm                    Dtas
035500060119     c                   parm                    Dtasv
035600060119    1c                   Endif
035700151001    1c                   IF        Wtnsf22r = *blanks
035800151001     c                   movel     'C'           tasptla                         'C'=CHIUDI
035900151001     c                   call      'TNSF22R'                            99
036000151001     c                   parm                    Kpjba
036100151001     c                   parm                    Dtas
036200151001     c                   parm                    Dtasv
036300151001     c                   parm                    dtaspes
036400151001    1c                   Endif
036500060119      *
036600060119     C                   IF        Wtibs69r  = 'S'
036700060119     C                   EVAL      I69TLA  = 'C'
036800060119     C                   CALL      'TIBS69R'
036900060119     C                   PARM                    TIBS69DS
037000060119     C                   ENDIF
037100060119      *
037200060117      *
037300060117     c                   seton                                        lr
037400120217
037500120217      /free
037600120217       //--------------------------------------------------------------
037700120217       //?Pulisco i dati che verranno rielaborati
037800120217       //--------------------------------------------------------------
037900120217       BEGSR Pulisci_File;
038000120217
038100120217         //?Se elaborazione giornaliera devo cancellare i dati "M"
038200120217         //?dei gg che verranno elaborati
038300120217         IF  D44gio = 'S';
038400120217           exec sql
038500120217             DELETE  from  QTEMP/TFMTC00F
038600120217             WHERE   MTCtrc = 'M' and
038700120217                     MTCdsp between :D44dsd and :D44dsa;
038800120217         ENDIF;
038900120217
039000120217         //?Se elaborazione settimanale/mensile devo cancellare tutti i dati "M"
039100120416         //?e i dati 'S' o 'V'  più vecchi di 60 gg. da oggi
039200120308         IF  D44gio = *blanks;
039300120217           exec sql
039400120217             DELETE  from  QTEMP/TFMTC00F
039500120217             WHERE   MTCtrc = 'M';
039600120217
039700120217           exec sql
039800120217             DELETE  from  QTEMP/TFMTC00F
039900120416             WHERE   MTCtrc in('S', 'V') and
040000120217                     MTCdsp < :wOggi_60;
040100120217         ENDIF;
040200130108
040300130108         //?Devo pulire anche i dati relativi al fatturato giornaliero
040400130108         //?Pulisco in base alle date calcolate all'inizio del pgm
040500130108         exec sql
040600160121           DELETE  from  TFFGC00F
040700130108           WHERE   FGCdata between :wDataDa and :wDataAl;
040800161007
040900161007         //?Devo pulire anche i dati relativi al fatturato giornaliero KSC
041000161007         //?Pulisco in base alle date calcolate all'inizio del pgm
041100161007         exec sql
041200161007           DELETE  from  TFFGK00F
041300161007           WHERE   FGKdata between :wDataDa and :wDataAl;
041400120217
041500120217       ENDSR;
041600120217
041700120217       //--------------------------------------------------------------
041800120217       //?Rielaboro i dati rimasti sul file TFMTC
041900120217       //--------------------------------------------------------------
042000120217       BEGSR Rielabora_File;
042100120217
042200120217         //?Leggo TFMTC
042300120220         wEndMtc = *off;
042400120217         exec sql
042500120217          DECLARE MTC cursor for
042600120220          SELECT  * from QTEMP/TFMTC00F
042700120217          WHERE MTCtrc = 'M'
042800120220          ORDER by MTCaas, MTCdsp, MTCksc;
042900120217
043000120217         exec sql
043100120217          OPEN MTC;
043200120217          IF SqlCode < 0;
043300120220            wEndMtc = *on;
043400120217          ENDIF;
043500120217
043600120220         DOW not wEndMtc;
043700120217           exec sql
043800120217            FETCH next from MTC into :TFMTCds;
043900120217            IF SqlCod = 100 or SqlCod < 0;
044000120220              wEndMtc = *on;
044100120217              leave;
044200120217            ENDIF;
044300120220
044400120220         //?Imposto flag che sto leggendo rcd già esistente su TFMTC
044500120220           wRcdMtc = *on;
044600120307         //?Pulisco il campo che identifica se segnalazione o manca tariffa
044700120307           clear wMTCtrc;
044800120322         //?Pulisco il campo di dell'errore
044900120322           clear wMTCerr;
045000120220
045100120220         //?Aggancio TITAS
045200120220           k_TASksc = MTCksc;
045300120220           k_TAStbl = MTCtbl;
045400120220           k_TASaas = MTCaas;
045500120220           k_TASmgs = %dec(%subst(%editc(MTCdsp:'X'):5:4):4:0);
045600120220           k_TASlnp = MTClnp;
045700120220           k_TASnrs = MTCnrs;
045800120220           k_TASnsp = MTCnsp;
045900120220           chain %kds(k07titas) TITAS02L;
046000120224
046100120224         //?Se non trovo la bolla su TITAS02L vuol dire che è stata contabilizzata
046200120224         //?quindi non c'è più il manca tariffa
046300120224           IF  not %found(TITAS02L);
046400120224
046500120224         //?Se trovo la bolla allora procedo con i controlli del manca tariffa
046600120224           ELSE;
046700120309
046800120312             com04 = %dec(%subst(%editc(TASksc:'X'):4:4):4:0);
046900120217
047000120224           //?Controllo se OK Anagrafica cliente
047100120224             exsr sr_Anagra;
047200120220
047300120224           //?Tasso se no 8888 e se no errore in Anagra
047400120322             IF com04 <> 8888 and wMTCerr = *blanks;
047500120224               exsr sr_Tassaz;
047600120224             ENDIF;
047700120224
047800120224           ENDIF;
047900120220
048000120220         //?Se arrivo a questo punto con il campo errore a blank
048100120307         //?e il campo di work che pilota se segnalazione o manca tariffa a blank
048200120307         //?(perchè se impostato ha già elaborato il rcd nella SR_SCRIVEERR)
048300120322           IF  wMTCerr = *blanks and wMTCtrc = *blanks;
048400120416         //?se ho trovato che c'è già un Visto non devo fare niente
048500120416         //?e leggo il rcd successivo
048600120416             IF  wVisto;
048700120416               iter;
048800120416             ENDIF;
048900120416         //?altrimenti vuol dire che non c'è più manca tariffa quindi cancello il rcd
049000120220             k_MTCtrc = MTCtrc;
049100120220             k_MTCaas = MTCaas;
049200120220             k_MTClnp = MTClnp;
049300120220             k_MTCnrs = MTCnrs;
049400120220             k_MTCnsp = MTCnsp;
049500120220             k_MTCtbl = MTCtbl;
049600120307             chain(e) %kds(k06tfmtc) TFMTC01L;
049700120307             IF  %error;
049800120307               iter;
049900120307             ENDIF;
050000120220             IF  %found(TFMTC01L);
050100120220               delete  TFMTC000;
050200120220             ENDIF;
050300120220           ENDIF;
050400120217
050500120217         ENDDO;
050600120217
050700120217         exec sql close MTC;
050800120217
050900120217       ENDSR;
051000120307
051100120307       //--------------------------------------------------------------
051200120307       //?Preparazione stringa SQL.
051300120307       //--------------------------------------------------------------
051400120307       BEGSR PrepSql;
051500120307
051600120307         wSQL = 'select * from TFMTC00F where MTCtrc = ''M''';
051700120307
051800120307       //?Se richiesta filiale a video
051900120307         IF D44fil > 0;
052000120307           wSQL += ' and MTCpoc = ' + %editc(D44fil:'X');
052100120307         ENDIF;
052200120307       //?Se richiesto commerciale a video
052300120307         IF D44ccm > 0;
052400120307           wSQL += ' and MTCage = ' + %editc(D44ccm:'X');
052500120307         ENDIF;
052600120307       //?Se richiesto cliente a video
052700120307         IF D44cli > 0;
052800120307           wSQL += ' and MTCksc = ' + %editc(D44cli:'X');
052900120307         ENDIF;
053000120307
053100120307          wSQL += ' ORDER by MTCaas, MTCdsp, MTCksc';
053200120307
053300120307       ENDSR;
053400120306
053500120306       //--------------------------------------------------------------
053600120306       //?Elaboro i dati presenti sul file TFMTC
053700120306       //--------------------------------------------------------------
053800120306       BEGSR Elabora_TFMTC;
053900120306
054000120306         //?Leggo TFMTC
054100120306         wEndMtc = *off;
054200120307         exec sql prepare A1 from :wSQL;
054300120307         exec sql declare MTC48 cursor for A1;
054400120307         exec sql open MTC48;
054500120306
054600120306          IF SqlCode < 0;
054700120306            wEndMtc = *on;
054800120306          ENDIF;
054900120306
055000120306         DOW not wEndMtc;
055100120306           exec sql
055200120306            FETCH next from MTC48 into :TFMTCds;
055300120306            IF SqlCod = 100 or SqlCod < 0;
055400120306              wEndMtc = *on;
055500120306              leave;
055600120306            ENDIF;
055700120306
055800120306         //?Imposto flag che sto leggendo rcd già esistente su TFMTC
055900120306           wRcdMtc = *on;
056000120307         //?Pulisco il campo che identifica se segnalazione o manca tariffa
056100120307           clear wMTCtrc;
056200120322         //?Pulisco il campo di dell'errore
056300120322           clear wMTCerr;
056400120306
056500120306         //?Aggancio TITAS
056600120306           k_TASksc = MTCksc;
056700120306           k_TAStbl = MTCtbl;
056800120306           k_TASaas = MTCaas;
056900120306           k_TASmgs = %dec(%subst(%editc(MTCdsp:'X'):5:4):4:0);
057000120306           k_TASlnp = MTClnp;
057100120306           k_TASnrs = MTCnrs;
057200120306           k_TASnsp = MTCnsp;
057300120306           chain %kds(k07titas) TITAS02L;
057400120306
057500120306         //?Se non trovo la bolla su TITAS02L vuol dire che è stata contabilizzata
057600120306         //?quindi non c'è più il manca tariffa
057700120306           IF  not %found(TITAS02L);
057800120306
057900120306         //?Se trovo la bolla allora procedo con i controlli del manca tariffa
058000120306           ELSE;
058100120312
058200120312             com04 = %dec(%subst(%editc(TASksc:'X'):4:4):4:0);
058300120306
058400120306           //?Controllo se OK Anagrafica cliente
058500120306             exsr sr_Anagra;
058600120306
058700120306           //?Tasso se no 8888 e se no errore in Anagra
058800120322             IF com04 <> 8888 and wMTCerr = *blanks;
058900120306               exsr sr_Tassaz;
059000120306             ENDIF;
059100120306
059200120306           ENDIF;
059300120306
059400120306         //?Se arrivo a questo punto con il campo errore a blank
059500120307         //?e il campo di work che pilota se segnalazione o manca tariffa a blank
059600120307         //?(perchè se impostato ha già elaborato il rcd nella SR_SCRIVEERR)
059700120322           IF  wMTCerr = *blanks and wMTCtrc = *blanks;
059800120416         //?se ho trovato che c'è già un Visto non devo fare niente
059900120416         //?e leggo il rcd successivo
060000120416             IF  wVisto;
060100120416               iter;
060200120416             ENDIF;
060300120416         //?altrimenti vuol dire che non c'è più manca tariffa quindi cancello il rcd
060400120306             k_MTCtrc = MTCtrc;
060500120306             k_MTCaas = MTCaas;
060600120306             k_MTClnp = MTClnp;
060700120306             k_MTCnrs = MTCnrs;
060800120306             k_MTCnsp = MTCnsp;
060900120306             k_MTCtbl = MTCtbl;
061000120307             chain(e) %kds(k06tfmtc) TFMTC01L;
061100120307             IF  %error;
061200120307               iter;
061300120307             ENDIF;
061400120306             IF  %found(TFMTC01L);
061500120306               delete  TFMTC000;
061600120306             ENDIF;
061700120306           ENDIF;
061800120306
061900120306         ENDDO;
062000120306
062100120306         exec sql close MTC48;
062200120306
062300120306       ENDSR;
062400120217
062500120217      /end-free
062600120217
062700060110      *------------------------------------------------------------------------*
062800060110      * ELABORAZIONE MANCA TARIFFA
062900060110      *------------------------------------------------------------------------*
063000060110     c     SR_Elabora    BegSr
063100060110
063200060110      * lettera del file TITAS in base alle richieste a video
063300060111     c                   SELECT
063400060111
063500060110      * Codice cliente  valorizzato
063600060119     c                   When      D44cli > 0
063700060112     c                   z-add     1             yy
063800060119     c                   eval      KTasKsc = D44cli
063900060111     c                   exsr      SR_Lettura02
064000060110      * Cliente non valorizzato
064100060201      * Po o area valorizzati
064200060201      * elaboro i P.O. della schiera
064300060201     c                   When      D44are > *zeros or  D44fil > *zeros
064400060111
064500060110     c                   z-add     1             yy
064600060111     c                   dow       Skpo(yy) > *zeros
064700060110      *
064800060110     c                   clear                   Ktasksc
064900060111     c                   movel     Skpo(yy)      Ktasksc
065000060111     c                   exsr      SR_Lettura02
065100060120     c                   add       1             yy
065200060110     c                   enddo
065300060111
065400060111     c                   OTHER
065500060111
065600060111      * preparo la chiave
065700060111      * Data spedizione
065800060111     c                   movel     D44Dsd        KTasAas
065900060111     c                   move      D44Dsd        KTasMgs
066000060111
066100060111     c                   exsr      SR_Lettura38
066200060110
066300060111     c                   ENDSL
066400060110
066500060110     c                   Endsr
066600060110
066700060110      *------------------------------------------------------------------------*
066800060111      * LETTURA TITAS38C
066900060110      *------------------------------------------------------------------------*
067000060111     c     SR_Lettura38  BegSr
067100060110
067200060111     c     Keytas38      setll     Titas38c
067300060110
067400060110     c                   Do        *hival
067500060110     c                   Read      Titas38c
067600060110
067700060110     c                   If        %eof(Titas38c)
067800060110     c                   leave
067900060110     c                   endif
068000130109
068100130109      /free
068200130109       //?Resetto i flag che indicano se spedizione tassata o manca tariffa
068300130109       //?da memorizzare nel fatturato giornaliero
068400130109        wTassa = *off;
068500130109        wManca = *off;
068600130109      /end-free
068700130109
068800060123      * Verifico se già contabilizzato da escludere
068900060123     c                   If        tasfic <> ' '
069000130109      /free
069100130109       //?Spedizione contabilizzata da calcolare nel fatturato giornaliero
069200130109       //?Elaboro solo le bolle con data spedizione nel range di date
069300130109       //?calcolato all'inizio del pgm
069400130110         IF  (TASaas * 10000 + TASmgs) >= wDataDa and
069500130110             (TASaas * 10000 + TASmgs) <= wDataAl;
069600130116           kk = 1;
069700130116           kk = %lookup(TAStbl : TBO);
069800130116           IF  kk > 0;
069900130116             %occur(dsTB) = kk;
070000130116             IF  §TBfcb = '1';
070100130116               exsr FatGioTas;
070200130116             ENDIF;
070300130116           ENDIF;
070400130109         ENDIF;
070500130109      /end-free
070600060123     c                   iter
070700060123     c                   endif
070800060110      * Verifico la data spedizione se maggiore della data ultima da elaborare vado a fine
070900060119     c                   If        tasdsp > D44dsa
071000060110     c                   leave
071100060110     c                   endif
071200060111      * Controllo manca tariffa
071300060111     c                   Exsr      SR_Tassa
071400130109
071500130109      /free
071600130109       //?Calcolo il fatturato giornaliero
071700130109       //?solo le bolle con data spedizione nel range di date
071800130109       //?calcolato all'inizio del pgm
071900130110         IF  (TASaas * 10000 + TASmgs) >= wDataDa and
072000130110             (TASaas * 10000 + TASmgs) <= wDataAl;
072100130109           SELECT;
072200130109         //?Spedizione prepagata
072300130109             WHEN  com04 = 8888 and TASdft > 0 and §TBfcb = '1';
072400130109              exsr FatGioTas;
072500130116         //?Spedizione con ksc 8888 non tassata conto come manca tariffa
072600130116             WHEN  com04 = 8888 and TASdft = 0 and §TBfcb = '1';
072700130116               clear dTASv;
072800130116               clear dTAS;
072900130116               §TASksc = TASksc;
073000130116               §TASdsp = TASaas * 10000 + TASmgs;
073100130116               wManca = *on;
073200130116               exsr agg_TFFGC;
073300130116         //?Spedizione in assegnato non tassata non la conteggio
073400130116             WHEN  com04 = 9999 and TASdft = 0;
073500130121         //?Spedizione in assegnato tassata
073600130121             WHEN  com04 = 9999 and TASdft > 0 and §TBfcb = '1';
073700130121              exsr FatGioTas;
073800130109         //?Manca tariffa da anagrafica
073900130109         //?Manca tariffa per errore in TNSF20R
074000130109         //?Manca tariffa per IMF
074100130109             WHEN  (Anaerr <> *blanks or §TASerr <> *blanks or wMTCimp = 'IMF')
074200130109                    and §TBfcb = '1';
074300130109               clear dTASv;
074400130109               clear dTAS;
074500130109               §TASksc = TASksc;
074600130109               §TASdsp = TASaas * 10000 + TASmgs;
074700130109               wManca = *on;
074800130109               exsr agg_TFFGC;
074900130109         //?Bolla Tassata da calcolare nel fatturato giornaliero
075000130111             WHEN  §TBfcb = '1';
075100130111               wTassa = *on;
075200130111               exsr agg_TFFGC;
075300130109           ENDSL;
075400130109         ENDIF;
075500130109      /end-free
075600060111
075700060111     c                   enddo
075800060111
075900060111     c                   endsr
076000060111
076100060111      *------------------------------------------------------------------------*
076200060111      * LETTURA TITAS02L
076300060111      *------------------------------------------------------------------------*
076400060111     c     SR_Lettura02  BegSr
076500060111
076600060111     c     Keytas02      setll     Titas02l
076700060111
076800060111     c                   Do        *hival
076900060111      * se è stato richiesto un codice cliente leggo tutti i record di quel codice
077000060119     c                   If        D44cli > 0
077100060111     c     Keytas02      Reade     Titas02l
077200060111     c                   else
077300060111      * se sto leggendo per PO
077400060111     c                   Read      Titas02l
077500060111     c                   endif
077600060111
077700060111     c                   If        %eof(Titas02l)
077800060111     c                   leave
077900060111     c                   endif
078000060111      * se sto leggendo per P.O. al cambio P.O. vado a fine lettura
078100060119     c                   If        D44cli = *zeros and tasksc <> savksc
078200060111     c                   movel     tasksc        taspoksc
078300060111     c                   if        Skpo(yy) <> taspoksc
078400060111     c                   leave
078500060111     c                   endif
078600060111     c                   endif
078700060111      * Verifico la data spedizione se non è compresa nei limiti leggo il successivo
078800060119     c                   If        tasdsp > D44dsa  or  tasdsp < D44dsd
078900060111     c                   iter
079000060111     c                   endif
079100060111      * Controllo manca tariffa
079200060111     c                   Exsr      SR_Tassa
079300060111
079400060111     c                   enddo
079500060111
079600060111     c                   endsr
079700060111
079800060111      *------------------------------------------------------------------------*
079900060111      * TASSAZIONE DELLA SINGOLA SPEDIZIONE
080000060111      *------------------------------------------------------------------------*
080100060111     c     SR_Tassa      BegSr
080200060111
080300120220     c                   clear                   tfmtc000
080400120228     c                   clear                   Wfmtc000
080500120307     c                   clear                   wMTCtrc
080600120322     c                   clear                   wMTCerr
080700060111     c                   clear                   rec_ok
080800060120      * ESCLUDO clienti  8888 e data fattura maggiore di zero o 9999
080900060111     c                   move      Tasksc        com04
081000060120     c                   If        (com04 = 8888 and tasdft > 0) or
081100060120     c                             com04 = 9999
081200060111     c                   eval      rec_ok = 'N'
081300060111     c                   endif
081400060111      * ESCLUDO le bolle da non tassare
081500060112     c                   z-add     1             kk
081600060112     c     tastbl        lookup    tbo(kk)                                30
081700060111     c                   if        %found
081800060112     c     kk            occur     dstb
081900060111      * verifico se bolla tassabile
082000060111     c                   if        §tbfcb <> '1'
082100060111     c                   eval      rec_ok = 'N'
082200060111     c                   endif
082300060111     c                   endif
082400060111
082500060112      * se record valido
082600060111     c                   If        Rec_ok = ' '
082700060112      *    controllo il cliente
082800060112     c                   exsr      Sr_anagra
082900060124      *    controllo la spedizione se diverso da 8888
083000060124     c                   if        Com04 <> 8888
083100120221      *    e se non ha avuto errori di anagrafica
083200120322     c                             and wMTCerr = *blanks
083300060113     c                   exsr      Sr_Tassaz
083400060124     c                   endif
083500060119
083600060117
083700060120     c                   endif
083800060120
083900060117     c                   Endsr
084000060112      *------------------------------------------------------------------------*
084100060112      * Controlla Anagrafica Clienti
084200060112      *------------------------------------------------------------------------*
084300060112     c     SR_anagra     BegSr
084400060112
084500060112      * Solo a cambio Cliente
084600060112
084700060112    0c                   If        TasKsc <> Savksc
084800060202      * recupero P.O. e Area
084900060202     c                   movel     TasKsc        Savpo
085000120423     c                   clear                   og143
085100060202     c     Savpo         Chain     Azorg01l
085200060202     c                   if        %found(azorg01l)
085300060202     c                   eval      Savcar = orgcar
085400120423     c                   eval      og143 = orgde3
085500060202     c                   else
085600060202     c                   clear                   savcar
085700060202     c                   endif
085800060112
085900060112     c                   movel     Tasksc        Savksc
086000060112     c                   clear                   AnaErr
086100060123      * se cliente 8888 stampo  direttamente l'errore  manca codice cliente
086200060123    1c                   If        Com04 = 8888
086300060124     c                   clear                   Savksc
086400060123      * recupero la descrizione del mittente
086500060123     c                   movel     'M'           Keytrc
086600060123     c     keyta430      chain     titaa30C
086700060123     c                   if        %found(titaa30c)
086800060123     c                   movel(P)  TaaRsc        Savrag
086900060123     c                   endif
087000060123
087100060123      * scrittura dell'errore
087200120322     c                   Movel     Err(12)       wMtcerr
087300060123     c                   clear                   Savbap
087400060123     c                   clear                   Savcas
087500060123     c                   clear                   Savvca
087600060123     c                   clear                   Savrma
087700060123     c                   clear                   Savage
087800120220     c                   eval      wMTCtrc = 'M'
087900060123     c                   Exsr      SR_ScriveErr
088000060123   x1c                   else
088100060112      * Aggancio Anagrafiche
088200060112     c     KeyAco        Chain     Cnaco000
088300060112     c     KeyAco        Chain     Cnclp000
088400060112     c     Keyaco        setll     Cnind000
088500060112      **
088600060123    2c                   If        %found(Cnaco00f)
088700060112     c                   movel     AcoRag        Savrag
088800060112     c                   Else
088900060112     c                   clear                   Savrag
089000060123    2c                   Endif
089100060123    2c                   If        %found(Cnclp00f)
089200060123     c                   movel     Clpage        Savage
089300060123     c                   Else
089400060123     c                   clear                   Savage
089500060123    2c                   Endif
089600120402
089700120402      /free
089800120402       //?Recupero la filiale dell'agente unificante
089900120412       //?e memorizzo anche l'agente unificante
090000120423          IF  §ogntw <> 'EEX' and §ogntw <> 'FED' and §ogntw <> 'DPD';
090100120402          IF  savage > 0;
090200130912            chain (SavAge) AZCMM000;
090300130912            IF  %found(AZCMM01L);
090400130912              savpo = %dec(%subst(%editc(CMMuni:'X'):1:3):3:0);
090500130912              savage = CMMuni;
090600120402            ENDIF;
090700120402          ENDIF;
090800120423          ENDIF;
090900120402      /end-free
091000120402
091100060123    2c                   SELECT
091200060112
091300060112      ** se non trovate anagrafiche
091400060112     c                   When      not %found(Cnaco00f) or not %found(Cnclp00f)
091500060112     c                             or not %equal(Cnind00f)
091600060112      * Manca anagrafica Cliente
091700060112     c                   movel     err(1)        AnaErr
091800060112
091900060112
092000060112      * Anagrafica Cleinte Annullata
092100060112
092200060123     c                   When      Acoflg = '*'
092300060112     c                   movel     Err(2)        AnaErr
092400060112
092500060112      * verifico frequenza fattura
092600060112
092700060112     c                   When      ClpTft <> 0 and ClpTft <> 1 and
092800060112     c                             ClpTft <> 9
092900060112     c                   Movel     Err(8)        AnaErr
093000060112
093100060112      * Manca Sottoconto Intestazione Fattura
093200060112
093300060123     c                   When      ClpScf = 0
093400060112     c                   movel     Err(3)        AnaErr
093500060112
093600060112      * Sottoconto Intestazione Fattura
093700060112
093800060123     c                   When      ClpScf > 0
093900060123    3c                   IF        ClpScf <> CLPKSC
094000060112
094100060112     c                   clear                   Tibs69ds
094200060112     c                   movel     clpksc        i69kac
094300060112
094400060112      * Controllo  Anagrafica
094500060112
094600060112     c                   Call      'TIBS69R'
094700060112     c                   Parm                    tibs69DS
094800060112     c                   Parm                    DS_cnaco
094900060112     c                   Parm                    DS_cnind
095000060112     c                   Parm                    DS_cnclp
095100060112     c                   Parm                    DS_fncls
095200060112
095300060112     c                   eval      wtibs69r = 'S'
095400060112
095500060112      * Manca Sottoconto Intestazione Fattura
095600060112
095700060123    4c                   if        O69Err = '1'
095800060112     c                   movel     Err(3)        AnaErr
095900060123   x4c                   else
096000060112
096100060112      * Sottoconto intestazione fattura annullato
096200060112
096300060123    5c                   If        Acoflg = '*'
096400060112     c                   movel     Err(4)        Anaerr
096500060123    5c                   endif
096600060112
096700060123    4c                   endif
096800060123    3c                   endif
096900060112
097000060112
097100060123    2c                   ENDSL
097200060123
097300060123    1c                   Endif
097400060112
097500060123    0c                   Endif
097600060112
097700060112      *
097800060112     c                   If        Anaerr <> *Blanks
097900120220     c                   eval      wMTCtrc = 'M'
098000120322     c                   Movel     Anaerr        wMtcerr
098100060112     c                   clear                   Savbap
098200060112     c                   clear                   Savcas
098300060112     c                   clear                   Savvca
098400060112     c                   clear                   Savrma
098500060112     c                   Exsr      SR_ScriveErr
098600060112     c                   endif
098700060112      *
098800060112     c                   Endsr
098900060113      *------------------------------------------------------------------------*
099000060113      * Controlla Tassazione della spediizone
099100060113      *------------------------------------------------------------------------*
099200060113     c     SR_tassaz     BegSr
099300060113
099400120322     c                   clear                   wMTCesc
099500120322     c                   clear                   wMTCimp
099600120322     c                   clear                   wMTCias
099700120322     c                   clear                   wMTCvas
099800120322     c                   clear                   wMTCass
099900120322     c                   clear                   wMTCinm
100000120322     c                   clear                   wMtcerr
100100090327     c                   clear                   stpncl
100200120416     c                   eval      wVisto = *off
100300060119
100400060119      * se richiesta stampa dell'imponibile recupero il riferimento alfanumerico
100500060119      * dal tita4 tipo record 'A'
100600060119
100700060119     c                   clear                   §tasrma
100800060119     c                   if        D44fsi = 'S'
100900060119     c                   eval      keytrc = 'A'
101000060119     c     Keyta430      chain     tita430C
101100060119     c                   if        %found(tita430c)
101200060119     c                   movel     ta4not        §tasrma
101300060119     c                   endif
101400060119     c                   endif
101500060119     c
101600060113      *  Valorizzo   D T A S V
101700060113
101800060113     c                   clear                   Dtasv
101900060113      * recupero le prime tre varie da titas
102000060113     c                   movel     Tassv1        Tsv(1)
102100060113     c                   z-add     Tasva1        Tva(1)
102200060113
102300060113     c                   movel     Tassv2        Tsv(2)
102400060113     c                   z-add     Tasva2        Tva(2)
102500060113
102600060113     c                   movel     Tassv3        Tsv(3)
102700060113     c                   z-add     Tasva3        Tva(3)
102800060113
102900060113      * se la terza varia è piena verifico in Tita7 se ce ne sono altre
103000060113     c                   If        Tassv3 <> *blanks
103100060113     c                   z-add     4             kk
103200060113     c     Keyta730      setll     Tita730c
103300060113     c     Keyta730      reade     Tita730c
103400060113     c                   Dow       not %eof(Tita730c)
103500060113     c                             and kk < 20                                  al massimo 20 varie
103600060113     c                   add       1             kk
103700060113     c                   movel     Ta7svn        Tsv(kk)
103800060113     c                   z-add     Ta7van        Tva(kk)
103900060113     c     Keyta730      reade     Tita730c
104000060113     c                   enddo
104100060113     c                   endif
104200060113      *
104300060113      * Carico esenzioni varie per DTASV
104400060113     c                   clear                   kk
104500060113     c                   movel     'CC'          Tabcod
104600060113     c                   movel(P)  'VARIE'       Tabkey
104700060113      *
104800060113     c                   Dow       kk < 20
104900060113     c                   add       1             kk
105000060113     c                   If        Tsv(kk) = *blanks
105100060113     c                   add       50            kk
105200060113     c                   else
105300060113     c                   move      Tsv(kk)       Tabkey
105400060113     c     keytab        chain     tabel00f
105500060113     c                   if        %found(tabel00f)
105600060113     c                   movel     tbluni        Dscc
105700060113     c                   movel     §cccei        Tev(kk)
105800060113     c                   endif
105900060113     c                   endif
106000060113      *
106100060113     c                   enddo
106200060113
106300060113      *  Valorizzo   D T A S
106400060113
106500060113     c                   clear                   DTAS
106600161012     c                   clear                   DTASPES
106700060113     c                   z-add     Tasksc        §Tasksc
106800060113     c                   z-add     Taspkf        §Taspkf
106900060113     c                   z-add     Tasvlf        §Tasvlf
107000160530     c                   z-add     Tasvlc        §Tasvlc
107100160530     c                   z-add     Tasncr        §Tasncr
107200160530     c                   move      Tasfvf        §Tasfvf
107300160530     c                   z-add     Taspkb        §Taspkb
107400060113     c                   z-add     Tasncl        §Tasncl
107500160530     c                   z-add     Tasncl        §Tasnclb
107600060113     c                   movel     Tascts        §Tascts
107700060113     c                   movel     Tasctr        §Tasctr
107800060113     c                   z-add     Tasmgs        §Tasdsp
107900060113     c                   movel     Tasaas        §Tasdsp
108000160122      * modifico per il recupero della tariffa di cartello con la data del giorno di tassazione
108100160122     c                   movel     dateu         §Tasdct
108200060113     c                   movel     Tascad        §Tascad
108300060113     c                   movel     Tasfin        §Tasfin
108400060113     c                   movel     Tascam        §Tascam
108500060113     c                   movel     Tasfap        §Tasfap
108600060113     c                   movel     Tastsp        §Tastsp
108700060113     c                   movel     Tasftc        §Tastc1
108800060113     c                   movel     Tastc2        §Tastc2
108900060113     c                   movel     Tasdiv        §Tasdiv
109000060113      *
109100060113      * aggancio il contrassegno.
109200060113      * se trovato verifico che il 1° carattere del tipo bolla corrisponda
109300060113     c                   if        tasfca <> *blanks
109400060113     c     Keytncsb      chain     Tncsb03l
109500060113     c                   if        %found(Tncsb03l)   and
109600060113     c                             Tastbl = csbtbl
109700161012     c                   movel     csbsta        §Tassta
109800060113     c                   z-add     Csbcas        §Tascas                         Contrassegno
109900060113     c                   movel     Csbvca        §Tasvca                         Valuta
110000060113     c                   z-add     Csbcmb        §Tascmb                         Cambio Divisa
110100061016     c****               movel     Csbtpa        §Tastic                         Tipo Incasso
110200060113     c                   if        csbfus <> *blanks
110300060113     c                   move      Csbfus        §tastic
110400060113     c                   else
110500060113     c                   move      Csbtpi        §Tastic                         Tipo Incasso
110600060113     c                   endif
110700060113     c                   endif
110800060113     c                   endif
110900060113
111000060113     c                   movel     Tasvas        §Tasvas
111100060113     c                   movel     Tasfdn        §Tasfdn                         Merce Port.Mag.
111200060113     c                   movel     Tasmct        §Tasmct                         Cod.Tassaz.Mittente
111300060113     c                   z-add     Taslnp        §Taslnp
111400060113     c                   z-add     Taslna        §Taslna
111500060113     c                   movel     Tastbl        §Tastbl
111600060113     c                   z-add     Tasias        §Tasias                         Importo assicurazio
111700060113     c                   z-add     Tasqft        §Tasqft                         Qta' Fatturabile
111800060113     c                   z-add     Taspor        §Taspor                         Trasporto
111900060113     c                   z-add     tasimv        §Tasimv                         Tot.Imponibile
112000060113
112100060113     c                   if        Tasll1 = 'S' or Tasll1 = 'C'
112200060113     c                   movel     'S'           §Tasddt                         flag si/no ddt
112300060113     c                   else
112400060113     c                   clear                   §Tasddt
112500060113     c                   endif
112600060926     c*****              movel     tasfbr        §tasflo
112700060926     c                   clear                   dtas01
112800060926     c                   movel     tasfbr        §asfbr
112900060926     c                   movel     tascca        §ascca
113000110701      * valorizzo flag email al destinatario
113100110701     c                   movel     tasflo        dtasflo
113200110701     c                   move      §floemd       §asemd
113300141211      /free
113400151127       //?Imposto se Prenotazione Ritiro Telefonico
113500151127         §TASprt = §FLOado;
113600151127
113700141211       //?Imposto se part.consegna con PinCode
113800141211         IF  TASgma <> *blanks and %lookup (TASgma:skGMA) > 0;
113900141211           §ASpin = 'S';
114000141211         ENDIF;
114100151127
114200151127       //?Imposto se Stampa Packing List
114300151127         clear dAR5gen;
114400151127         chain (TASaas:TASlnp:TASnrs:TASnsp:'GEN') FIAR531C;
114500151127         IF  %found(FIAR531C);
114600151127           dAR5gen = AR5uni;
114700151127         ENDIF;
114800151127         IF  §AR5als = 'S' or §AR5alx = 'S';
114900151127           §TASspl = 'S';
115000151127         ENDIF;
115100141211      /end-free
115200110701
115300060926     c                   eval      §tasflo = dtas01
115400060113
115500060113     c                   clear                   Kpjbu
115600060125     c                   clear                   Monovaria
115700060113
115800060113      * Le bolle con totale imponibile le controllo per controllare l'assicurazione
115900060113     c                   clear                   Parca
116000060113     c                   setoff                                       10
116100060113    1c                   If        Tasimv > *zeros
116200060113     c                   movel     'R'           §Tassva
116300060113     c                   movel     'S'           Parca
116400060113     c                   else
116500060113     c                   z-add     1             kk
116600060113     c     tascbo        lookup    cbo(kk)                                31
116700060113     c                   if        *in31
116800060113     c                   eval      §tassva=cBV(kk)
116900060125     c                   If        cbv(kk) <> ' '
117000060125     c                   eval      monovaria = 'SI'
117100060113     c                   endif
117200060125     c                   endif
117300060113
117400060113     c                   if        §tassva<>*blanks
117500060113     c                   z-add     1             kk
117600060113     c     §tassva       lookup    tsv(kk)                                10
117700060113      * se la varia c'è ed è valorizzata e imponibile = 0 devo richiamare la tassazione
117800060113     c                   If        *In10 and tva(kk) > 0 and TasImv = 0
117900060113     c                   Eval      *In10 = *Off
118000060113     c                   EndIf
118100060113     c                   Endif
118200060113
118300060113    1c                   Endif
118400060113
118500060113      * imposto peso vdl e colli rilevati
118600060113     c                   z-add     tasncp        §tasncp
118700060113     c                   z-add     taspkc        §taspkc
118800060113     c                   clear                   §taspkcN
118900060113
119000060113      * Bancali
119100060113     c                   If        %Subst(TasGva:1:1) ='B'
119200060113     c                   Clear                   dAr5Ban
119300060113     c                   Eval      kAr5Trd = 'BAN'
119400060113     c     keyFiar5      Chain     Fiar531c
119500060113     c                   If        %Found(Fiar531c)
119600060113     c                   Movel     Ar5Uni        dAr5Ban
119700060113     c                   Eval      §TasBan = §Ar5Nba + §ar5Nb2
119800060113     c                   EndIf
119900060113     c                   EndIf
120000060113      * Colli originali
120100060113     c                   If        %Subst(TasGva:1:1) ='O'
120200060113     c                   Clear                   dAr5Bnb
120300060113     c                   Eval      kAr5Trd = 'BNB'
120400060113     c     keyFiar5      Chain     Fiar531c
120500060113     c                   If        %Found(Fiar531c)
120600060113     c                   Movel     Ar5Uni        dAr5Bnb
120700060113     c                   Eval      §TasNcl = §Ar5bCor
120800090327      * mi salvo i colli da mettere nel file manca tariffa
120900090327     c                   eval      stpNcl = §ar5bCor
121000060113     c                   EndIf
121100060113     c                   EndIf
121200060113     c
121300060113      * Se bolla di reso e tassata fino all'imponibile con varia 'N' 88888888
121400060113      * prima di tassare la bolla devo tassare la varia 'N'
121500060113     c                   if        tasimv = 0 and tasfbr = 'S'
121600060116     c                   exsr      Sr_tassan
121700060113     c                   endif
121800060113
121900060113     c                   clear                   Kpjbu
122000060113
122100060113     c                   movel     paramt        Kpjbu
122200060113      * verifico se esiste la varia & con importo uguale a 8888888 e se si passo al
122300060113      * TNSF20R il flag di calcolo varia &
122400060113      *
122500060113     c                   setoff                                       51
122600060113     c                   z-add     1             kk
122700060113     c     '&'           lookup    tsv(kk)                                51
122800060113     c                   if        *in51 and tva(kk) = 88888888
122900060113     c                   eval      §tasfcaa = 'S'
123000060113     c                   clear                   tsv(kk)
123100060113     c                   clear                   tva(kk)
123200060113     c                   endif
123300060113      * verifico se devo calcolare l'addebito di lasciato avviso
123400060113      *
123500060113      * verifico se c'è evento
123600060113     c     KeyEvb        chain     Fnevb04l
123700060113
123800060113     c                   If        %found(Fnevb04l)
123900060113     c                   eval      §tasric = 'S'
124000060113     c                   endif
124100060113
124200151001      * imposto dati
124300151001     c                   eval      tasppkb = taspkb
124400160504     c                   eval      taspfvf = TASfvf
124500160504     c                   eval      taspncr = TASncr
124600060113
124700060113     c                   if        §tassva=*blanks or not *in10
124800151001     c                   Call      'TNSF22R'
124900060113     c                   Parm                    Kpjba
125000060113     c                   Parm                    Dtas
125100060113     c                   Parm                    Dtasv
125200151001     c                   parm                    dtaspes
125300060119
125400151001     c                   eval      Wtnsf22r = '1'
125500060119
125600060113     c                   endif
125700060113
125800060113      * stampo e uso il peso usato in fatturazione
125900151013     c                   If        §Tasspc <> ' '
126000060113      * mi salvo il peso da fatturare per stamparlo in fattura
126100151013     c                   eval      taspkf=§TaspkcN
126200151013     c                   endif
126300060116      * Se tassando la bolla ho errori oppure non ho errori ma devo stampare l'imponibile
126400060116      * scrivo il record sul file work
126500060116
126600060119    1c                   If        (§taserr = ' ' and d44fsi = 'S') or
126700060116     c                              §taserr <> ' '
126800060116     c                   eval      Savbap = §tasbap
126900060116     c                   eval      Savcas = §Tascas
127000060116     c                   eval      Savvca = §Tasvca
127100060116     c                   eval      Savrma = §Tasrma
127200120322     c                   movel     §Tasmsg       wMtcerr
127300060208
127400060223     c                   If        d44fsi = 'S' and §taserr = ' '
127500120322     c                   eval      wmtcimp = 'IMP'
127600060208     c                   endif
127700060208
127800060223      * valorizzo i campi dell'imponibile e della divisa
127900060123     c                   z-add     §tasimv       tasimv
128000060123     c                   eval      tasdiv = §tasdiv
128100060123
128200120220     c                   eval      wMTCtrc = 'M'
128300060116     c                   Exsr      SR_ScriveErr
128400060116
128500060119   x1c                   else
128600120413
128700120413      * controllo se imponibile maggiore del limite massimo fatturabile
128800120413      * da tabella GEI
128900120413    2c                   If        §tasimv > §geimf
129000120413     c                   eval      wMTCtrc = 'M'
129100120413     c                   eval      wmtcerr = err(7)
129200120413     c                   eval      wmtcimp = 'IMF'
129300120413      * valorizzo i campi dell'imponibile e della divisa
129400120413     c                   z-add     §tasimv       tasimv
129500120413     c                   eval      tasdiv = §tasdiv
129600120413     c                   Exsr      SR_ScriveErr
129700120413     c                   leavesr
129800120413    2c                   endif
129900120907       //?Controllo se imponibile a zero
130000120910       //?non se bolla monovaria
130100120910    2c                   If        §tasimv = 0 and monovaria <> 'SI'
130200120907     c                   eval      wMTCtrc = 'M'
130300120910     c                   eval      wmtcerr = err(15)
130400120907     c                   eval      wmtcimp = 'IMF'
130500120907      * valorizzo i campi dell'imponibile e della divisa
130600120907     c                   z-add     §tasimv       tasimv
130700120907     c                   eval      tasdiv = §tasdiv
130800120907     c                   Exsr      SR_ScriveErr
130900120907     c                   leavesr
131000120907    2c                   endif
131100060116
131200060116      * Se tassando la bolla non ho errori e non devo stampare l'imponibile
131300120328      * controllo se imponibile nei limiti indicati nelle tabelle
131400120328       //?limite imponibile bolla + peso da tabella IMP
131500120416       //?peso fino a kg. e imponibile > di xxx
131600120328    2c                   IF        TASpkf <= §IMPpeso and §tasimv > §IMPimp1
131700060116      * verifico se imponibile inserito  manualmente
131800060116     c                   if        tasimv > 0
131900120322     c                   eval      wmtcinm = 'M'
132000060116     c                   endif
132100120326      * valorizzo i campi dell'imponibile e della divisa
132200120326     c                   z-add     §tasimv       tasimv
132300120326     c                   eval      tasdiv = §tasdiv
132400120326     c                   eval      wmtcimp = 'IMP'
132500120326     c                   eval      wMTCtrc = 'M'
132600120326     c                   eval      wmtcerr = err(14)
132700120326     c                   eval      %subst(wmtcerr:28:14) =
132800120329     c                             %trim(%editc(§IMPimp1:'4'))
132900120326     c                   Exsr      SR_ScriveErr
133000120416       //?Se torna che la bolla non ha già il Visto vado alla fine dei controlli perchè il rcd
133100120416       //?è già stato scritto nella routine scriveerr
133200120416     c                   IF        not wVisto
133300120326     c                   leavesr
133400120416       //?Se torna che la bolla ha già il Visto devo andare avanti con gli altri controlli
133500120416       //?per cercare eventuali altri errori da scrivere
133600120416     c                   ELSE
133700120416     c                   clear                   wmtcimp
133800120416     c                   clear                   wMTCtrc
133900120416     c                   clear                   wMTCerr
134000120413     c                   ENDIF
134100060119    2c                   endif
134200120416
134300120328       //?limite imponibile bolla + peso da tabella IMP
134400120416       //?peso oltre kg. e imponibile > di xx
134500120328    2c                   IF        TASpkf > §IMPpeso and §tasimv > §IMPimp2
134600120328      * verifico se imponibile inserito  manualmente
134700120328     c                   if        tasimv > 0
134800120328     c                   eval      wmtcinm = 'M'
134900120328     c                   endif
135000120328      * valorizzo i campi dell'imponibile e della divisa
135100120328     c                   z-add     §tasimv       tasimv
135200120328     c                   eval      tasdiv = §tasdiv
135300120328     c                   eval      wmtcimp = 'IMP'
135400120328     c                   eval      wMTCtrc = 'M'
135500120328     c                   eval      wmtcerr = err(14)
135600120328     c                   eval      %subst(wmtcerr:28:14) =
135700120329     c                             %trim(%editc(§IMPimp2:'4'))
135800120328     c                   Exsr      SR_ScriveErr
135900120416       //?Se torna che la bolla non ha già il Visto vado alla fine dei controlli perchè il rcd
136000120416       //?è già stato scritto nella routine scriveerr
136100120416     c                   IF        not wVisto
136200120413     c                   leavesr
136300120416       //?Se torna che la bolla ha già il Visto devo andare avanti con gli altri controlli
136400120416       //?per cercare eventuali altri errori da scrivere
136500120416     c                   ELSE
136600120416     c                   clear                   wmtcimp
136700120416     c                   clear                   wMTCtrc
136800120416     c                   clear                   wMTCerr
136900120413     c                   ENDIF
137000120328    2c                   endif
137100120328
137200060117      * verifico per le tariffe a collo il peso del collo
137300130913      * e se è superiore di oltre  5 volte alla media del cliente lo segnalo
137400060117      * non viene controllato se bolla con varia "O"
137500130913      * e peso bolla minore di un kg (xchè non significativo)
137600060117     c                   movel     tasctr        Tipo_tar
137700060119    2c                   If        Tipo_tar = '1' and tasimv = 0 and
137800090327     c                             %Subst(TasGva:1:1) <>'O'
137900130913     c                             and taspkf >  1
138000060117      * richiamo il tise70r x recuperare i dati degli ultimi 3 mesi
138100060117     c                   ExSr      Sr_tise
138200060117      * Calcolo la media del cliente
138300060117     c                   Exsr      Sr_Calmed
138400060117      * se 08 acceso scrivo
138500060117     c                   If        *in08
138600120321     c                   eval      wMTCtrc = 'M'
138700120322     c                   eval      wmtcesc = '!!!'
138800120322     c                   eval      wmtcerr = err(13)
138900120322     c                   Exsr      SR_ScriveErr
139000120322     c                   leavesr
139100060117     c                   Endif
139200060119    2c                   Endif
139300060117
139400140402      * recupero il tipo bolla
139500140402      /free
139600140402        kk = %lookup(TAStbl:tbo);
139700140402        IF  kk > 0;
139800140402          %occur(dstb) = kk;
139900140402        ENDIF;
140000140402      /end-free
140100140402
140200060117      * se imponibile in bolla uguale a zero e
140300060117      * tariffa bloccata oppure franco e tariffa
140400060125      * oppure franco e tariffa valida solo in arrivo e non è bolla monovaria
140500060125      * oppure assegnato e tariffa valida solo in partenza e non è bolla monovaria
140600060125      * segnalo l'errore
140700060117
140800060119    2c                   If        Tasimv = 0 and §tasbap <> ' '
140900060125     c                   If        §tbtpo = 'F' and §tasbap = 'A'  and
141000060125     c                             monovaria <> 'SI'
141100120402     c                   eval      wMTCtrc = 'M'
141200120322     c                   movel     err(5)        wMtcerr
141300120329     c                   Exsr      SR_ScriveErr
141400120329     c                   leavesr
141500060117     c                   endif
141600060125     c                   If        §tbtpo = 'A' and §tasbap = 'P'  and
141700060125     c                             monovaria <> 'SI'
141800120402     c                   eval      wMTCtrc = 'M'
141900120322     c                   movel     err(9)        wMtcerr
142000120329     c                   Exsr      SR_ScriveErr
142100120329     c                   leavesr
142200060117     c                   endif
142300060119     c                   If        §tasbap = 'B'
142400120402     c                   eval      wMTCtrc = 'M'
142500120322     c                   movel     err(10)       wMtcerr
142600120329     c                   Exsr      SR_ScriveErr
142700120329     c                   leavesr
142800060119     c                   endif
142900060119    2c                   endif
143000120329
143100060119      * Controllo il limite dell'importo da assicurare
143200070518    2c                   If         TasIas <> *Zeros or §TasIal <> *Zeros
143300060119     c                   Clear                   Trul22Ds
143400060119     c                   Eval      I22Cbo = TasCbo
143500060119     c                   Eval      I22Tsp = TasTsp
143600060119     c                   Eval      I22Lnp = TasLnp
143700060119     c                   Eval      I22Nzm = TasNzm
143800060119     c                   Eval      I22Lna = TasLna
143900060119     c                   Eval      I22Nzd = TasNzd
144000060119     c                   Eval      I22Ksc = TasKsc
144100060119     c                   Movel     TasCtr        I22Ctr
144200070518    3c                   If        TasIas <> *Zeros
144300060119     c                   Eval      I22Imp = TasIas
144400060119     c                   Eval      I22Div = TasVas
144500070518    3c                   EndIf
144600070518    3c                   If        §TasIal <> *Zeros
144700060119     c                   Eval      I22Imp = §TasIal
144800060119     c                   Eval      I22Div = §TasDiv
144900070518    3c                   EndIf
145000060119     c                   Call      'TRUL22R'
145100060119     c                   Parm                    trul22ds
145200060119      * controllo che non sia maggiore del limite previsto
145300060119      * non forzabile
145400070518    3c                   If        O22Fx2 <> *blanks
145500120322     c                   Z-Add     I22Imp        wMtcIas
145600120322     c                   Movel     I22Div        wMtcVas
145700120322     c                   Eval      wMtcAss = 'ASS'
145800070518      * verifico se la spedizione con mandato assicurativo è :
145900070518      * - porto assegnato
146000070518      * - Euroexpress IMPORT
146100070518      * non segnalo l'errore
146200070518    4c                   If        §task88 = 'N' and §Tbtpo = 'A'
146300070523     c     taslna        chain     azorg01l
146400070518    5c                   if        %found(azorg01l) and orgfl1 = ' '            italia
146500070523     c     taslnp        chain     azorg01l
146600070518    6c                   if        %found(azorg01l) and orgfl1 = 'E'            Estero
146700070518     c                   eval      og143 = orgde3
146800070518    7c                   if        §ogntw = 'EEX'
146900120322     c                   clear                   wMtcAss
147000120322     c                   clear                   wMtcIas
147100120322     c                   clear                   wMtcVas
147200070518    7c                   EndIf
147300070518    6c                   EndIf
147400070518    5c                   EndIf
147500070518    4c                   EndIf
147600070518    3c                   EndIf
147700060119    2c                   EndIf
147800060119
147900060119      * se devo scrivere una segnalazione di imponibile anomalo  o assicurazione  o
148000060119      * errori
148100120413     c                   If        wmtcimp = 'IMP' or wMtcAss = 'ASS' or
148200120322     c                             wmtcerr <> *blanks or wmtcesc = '!!!'
148300060119
148400060116     c                   eval      Savbap = §tasbap
148500060116     c                   eval      Savcas = §Tascas
148600060116     c                   eval      Savvca = §Tasvca
148700060116     c                   eval      Savrma = §Tasrma
148800060123      * valorizzo i campi del'imponibile e della divisa
148900060123
149000060123     c                   z-add     §tasimv       tasimv
149100060123     c                   eval      tasdiv = §tasdiv
149200120220
149300120220      /free
149400120220       //?Imposto il tipo record 'S' se non è un manca tariffa
149500120220       //?il manca tariffa lo imposto a 'M' appena lo trovo
149600120220         IF  wMTCtrc = *blanks;
149700120220           wMTCtrc = 'S';
149800120220         ENDIF;
149900120220      /end-free
150000060123
150100060116     c                   Exsr      SR_ScriveErr
150200060119
150300060116     c                   endif
150400060119
150500060119     c                   endif
150600060119      *
150700060113     c                   endsr
150800060113
150900060113      *-----------------------------------------------------------
151000060113      * Tasso la Varia 'N' 88888888
151100060113      *-----------------------------------------------------------
151200060116     c     SR_Tassan     begsr
151300060113
151400060113      * verifico se c'è la varia 'N' 88888888
151500060113     c                   eval      kk = 1
151600060113     c     'N'           lookup    tsv(kk)                                32
151700060113     c                   if        *in32 and tva(kk) = 88888888
151800060113      * richiamo il programma di tassazione
151900060113      * senza passare le varie e il porto
152000060113     c                   movea     tsv           w_tsv
152100060113     c                   movea     tva           w_tva
152200060113     c                   movea     tev           w_tev
152300060113     c                   eval      w_dtas = dtas
152400110701
152500110701      * tolgo il flag dell'invio mail al destinatario nell'eventualità ci fosse
152600110701     c                   eval      dtas01 = §tasflo
152700110701     c                   clear                   §asemd
152800141211      /free
152900141211       //?Pulisco flag PinCode
153000141211         clear §ASpin;
153100141211      /end-free
153200110701     c                   eval      §tasflo = dtas01
153300160113
153400160113      * tolgo flag per calcolo  Diritto di chiamata prenotazione  ritiro e packing list
153500160113      *
153600160113     c                   clear                   §TASPRT
153700160113     c                   clear                   §TASSPL
153800160113
153900060113
154000060113     c                   clear                   dtasv
154100060113     c                   clear                   §tasdiv
154200060113     c                   clear                   §taspor
154300060113
154400060113     c                   clear                   kpjbu
154500060113     c                   call      'TNSF20R'
154600060113     c                   parm                    kpjba
154700060113     c                   parm                    dtas
154800060113     c                   parm                    dtasv
154900060113      * valorizzo la varia 'N' 88888888
155000060113     c                   if        §taserr = ' '
155100060116     c                   z-add     §tasimv       w_tva(kk)
155200060113     c                   endif
155300060113      * ripristino le ds salvate prima
155400060113     c                   movea     w_tsv         tsv
155500060113     c                   movea     w_tva         tva
155600060113     c                   movea     w_tev         tev
155700060113     c                   eval      dtas = w_dtas
155800060113     c                   endif
155900060113
156000060113     c                   endsr
156100060117      *------------------------------------------------------------------------*
156200060116      *    richiamo il tise70r x recuperare i dati
156300060117      *------------------------------------------------------------------------*
156400060116     c     Sr_Tise       BegSr
156500060116
156600060116     c                   move      Tasaas        waaf
156700060116     c                   movel     Tasmgs        wmmf
156800060116     c                   move      01            wggf
156900060116
157000060116     c     *iso          move      wdataf        dataiso
157100060116     c                   subdur    2:*m          dataiso
157200060116     c                   extrct    dataiso:*m    wmmi
157300060116     c                   extrct    dataiso:*y    waai
157400060116
157500060116     c                   clear                   TISE70DS
157600060116     c                   eval      I70Tla = 'S'
157700060116     c                   eval      I70Tle = 'ST'
157800060116     c                   eval      I70Ksc = Tasksc
157900171130     c                   eval      I70ctr = %editc(TASctr:'X')
158000060116     c                   eval      I70Daf = waammf
158100060116     c                   eval      I70Dai = waammi
158200060116
158300060116     c                   call      'TISE70R'
158400060116     c                   parm                    Tise70ds
158500060116
158600060116     c                   EndSr
158700060117      *------------------------------------------------------------------------*
158800060117      *    Calcolo la media del cliente
158900060117      *------------------------------------------------------------------------*
159000060119     c     Sr_Calmed     Begsr
159100060117
159200070704     c                   clear                   *in08
159300120402
159400120402       //?Il controllo lo faccio solo se raggruppamento bolla (§TBRBL) é NORMALE (N)
159500120402      /free
159600120402        kk = %lookup(TAStbl:tbo);
159700120402        IF  kk > 0;
159800120402          %occur(dstb) = kk;
159900120402          IF  §TBrbl <> 'N';
160000120402            leavesr;
160100120402          ENDIF;
160200120402        ENDIF;
160300160518
160400160518       //?e se il n. di spedizioni è > di 50
160500160518       //?come da indicazione di Bocchi
160600160518        IF  O70nsp <= 50;
160700160518          leavesr;
160800160518        ENDIF;
160900120402      /end-free
161000120402
161100060117      * Peso unitario della media del cliente
161200060117
161300060116     c                   If        o70nco > *Zeros
161400060117     c     o70pkg        div       o70nco        wunim
161500060117      * peso unitario della spedizione
161600090327     c     TasPKF        div(h)    Tasncl        wunis
161700090324      * si è deciso di modificare il controllo del peso 10 volte superiore
161800090324      * alla media dell cliente con solo 5 volte superiore
161900090324     c**** Wunim         Mult      10            Wunimt
162000090324     c     Wunim         Mult      5             Wunimt
162100060117
162200060117     c                   eval      *in08 =(Wunis >= wunimt)
162300060117
162400070704     c                   EndIf
162500060117     c                   Endsr
162600060117
162700060112      *------------------------------------------------------------------------*
162800060112      * Scrittura file di lavoro
162900060112      *------------------------------------------------------------------------*
163000060112     c     SR_ScriveErr  BegSr
163100120220
163200120220      /free
163300120413         wVisto = *off;
163400120326       //?Se devo scrivere un manca tariffa per IMP > di §geimv prima verifico che
163500120326       //?non sia già stato vistato dall'utente, lo cerco quindi con tipo rcd V=OK da utente
163600120326         IF  wmtcimp = 'IMP' and wmtctrc = 'M';
163700120326           k_MTCtrc = 'V';
163800120413           k_MTCaas = TASaas;
163900120413           k_MTClnp = TASlnp;
164000120413           k_MTCnrs = TASnrs;
164100120413           k_MTCnsp = TASnsp;
164200120413           k_MTCtbl = TAStbl;
164300120326           chain(n) %kds(k06tfmtc) TFMTC01L;
164400120326           IF  %found(TFMTC01L);
164500120413             wVisto = *on;
164600120326             leavesr;
164700120326           ENDIF;
164800120326         ENDIF;
164900120326
165000120307       //?Se sto rielaborando un rcd di Manca tariffa già presente sul file
165100120307         IF  wRcdMtc;
165200120307           k_MTCtrc = MTCtrc;
165300120307           k_MTCaas = MTCaas;
165400120307           k_MTClnp = MTClnp;
165500120307           k_MTCnrs = MTCnrs;
165600120307           k_MTCnsp = MTCnsp;
165700120307           k_MTCtbl = MTCtbl;
165800120307           chain(e) %kds(k06tfmtc) TFMTC01L;
165900120307           IF  %error;
166000120307             leavesr;
166100120307           ENDIF;
166200120307           IF  %found(TFMTC01L);
166300120307           //?se il campo di work che pilota se segnalazione o manca tariffa è = 'S'
166400120307           //?da manca tariffa è diventato segnalazione quindi cancello il rcd di manca tariffa
166500120307             IF  wMTCtrc = 'S';
166600120307               delete  TFMTC000;
166700120307             ENDIF;
166800120307           //?se il campo di work che pilota se segnalazione o manca tariffa è = 'M'
166900120307           //?è ancora manca tariffa quindi aggiorno il rcd di manca tariffa
167000120307             IF  wMTCtrc = 'M';
167100120307               exsr MemDati;
167200120307               update  TFMTC000;
167300120307               leavesr;
167400120307             ENDIF;
167500120307           ENDIF;
167600120307         ENDIF;
167700120307         //?Se è una segnalazione verifico se non è già presente sul file
167800120307         IF  wMTCtrc = 'S';
167900120307           k_MTCtrc = wMTCtrc;
168000120308           k_MTCaas = TASaas;
168100120308           k_MTClnp = TASlnp;
168200120308           k_MTCnrs = TASnrs;
168300120308           k_MTCnsp = TASnsp;
168400120308           k_MTCtbl = TAStbl;
168500120307           chain(n) %kds(k06tfmtc) TFMTC01L;
168600120307         //?Se è già presente non faccio niente perchè non è da riscrivere
168700120307           IF  %found(TFMTC01L);
168800120307             //?Scrivo il WF
168900120307             //?se non è richiamato da TNSB48
169000120307             IF  not wDaSB48;
169100120307               exsr wrtWF;
169200120307             ENDIF;
169300120307             leavesr;
169400120307           ENDIF;
169500120307         ENDIF;
169600120307
169700120307       //?Scrivo nuovo record
169800120307         MTCtrc = wMTCtrc;
169900120307         MTCaas = TASaas;
170000120307         MTClnp = TASlnp;
170100120307         MTCnrs = TASnrs;
170200120307         MTCnsp = TASnsp;
170300120307         MTCtbl = TAStbl;
170400120307         MTCDSP = TASaas * 10000;
170500120307         MTCdsp += TASmgs;
170600120307         MTCksc = TASksc;
170700120322         clear MTCdstp;
170800120307         exsr MemDati;
170900120307         write  TFMTC000;
171000120307         //?Scrivo il WF
171100120307         //?se non è richiamato da TNSB48
171200120307         IF  not wDaSB48;
171300120307           exsr wrtWF;
171400120307         ENDIF;
171500060112
171600120220       ENDSR;
171700120220
171800120220       //--------------------------------------------------------------
171900120220       //?Memorizzo i dati su TFMTC
172000120220       //--------------------------------------------------------------
172100120220       BEGSR MemDati;
172200120220
172300120220       //?Reperisco la provincia del codice tassazione
172400120220         clear savpro;
172500120220         kk = %lookup(TAScts:cts);
172600120220         IF  kk > 0;
172700120220           savpro = cpr(kk);
172800120220         ENDIF;
172900120220
173000120220         MTCpoc = Savpo;
173100120220         MTCcar = Savcar;
173200120220         MTCrag = Savrag;
173300120220         MTCage = Savage;
173400120220
173500120220       //?verifico se il campo colli originali da stampare è valorizzato
173600120220         IF  StpNcl <> 0 and StpNcl <> Tasncl;
173700120220           MTCNcl = StpNcl;
173800120322           %Subst(wMtcEsc:1:1) = 'o';
173900120220         ELSE;
174000120220           MTCncl = Tasncl;
174100120220         ENDIF;
174200120220
174300120220         MTCpkf = Taspkf;
174400120220         MTCvlf = Tasvlf;
174500120220         MTCfap = Tasfap;
174600120220         MTClna = Taslna;
174700120220         MTCcts = Tascts;
174800120220         MTCfin = Tasfin;
174900120220         MTCpro = Savpro;
175000120220         MTCtsp = Tastsp;
175100120220         MTCctr = Tasctr;
175200120220         MTCbap = Savbap;
175300120220         MTCdiv = Tasdiv;
175400120220         MTCpor = Taspor;
175500120220         MTCimv = Tasimv;
175600120220         MTCcas = Savcas;
175700120220         MTCvca = Savvca;
175800120220         MTCrmn = Tasrmn;
175900120220         MTCrma = Savrma;
176000120322         MTCerr = wMTCerr;
176100120322         MTCesc = wMTCesc;
176200120322         MTCimp = wMTCimp;
176300120322         MTCias = wMTCias;
176400120322         MTCvas = wMTCvas;
176500120322         MTCass = wMTCass;
176600120322         MTCinm = wMTCinm;
176700120220
176800120220       ENDSR;
176900120228
177000120228       //--------------------------------------------------------------
177100120228       //?Scrivo il file WFTMC
177200120228       //--------------------------------------------------------------
177300120228       BEGSR wrtWF;
177400120329
177500120329       //?Se errore IMP ed elaborazione giornaliera non la scrivo sul WF
177600120329       //?così non viene stampata tutti i giorni
177700120329         IF  wMTCtrc = 'M' and wMTCimp = 'IMP' and d44gio = 'S';
177800120329           leavesr;
177900120329         ENDIF;
178000120228
178100120228       //?Reperisco la provincia del codice tassazione
178200120228         clear savpro;
178300120228         kk = %lookup(TAScts:cts);
178400120228         IF  kk > 0;
178500120228           savpro = cpr(kk);
178600120228         ENDIF;
178700120228
178800120228         MTCdel = dateu;
178900120228         MTCksc = TASksc;
179000120228         MTCpoc = Savpo;
179100120228         MTCcar = Savcar;
179200120228         MTCage = Savage;
179300120228         MTCrag = Savrag;
179400120228         MTCaas = TASaas;
179500120228         MTClnp = TASlnp;
179600120228         MTCnrs = TASnrs;
179700120228         MTCnsp = TASnsp;
179800120228         MTCtbl = TAStbl;
179900120228         MTCDSP = TASaas * 10000;
180000120228         MTCdsp += TASmgs;
180100120228
180200120228       //?verifico se il campo colli originali da stampare è valorizzato
180300120228         IF  StpNcl <> 0 and StpNcl <> Tasncl;
180400120228           MTCNcl = StpNcl;
180500120322           %Subst(wMtcEsc:1:1) = 'o';
180600120228         ELSE;
180700120228           MTCncl = Tasncl;
180800120228         ENDIF;
180900120228
181000120228         MTCpkf = Taspkf;
181100120228         MTCvlf = Tasvlf;
181200120228         MTCfap = Tasfap;
181300120228         MTClna = Taslna;
181400120228         MTCcts = Tascts;
181500120228         MTCfin = Tasfin;
181600120228         MTCpro = Savpro;
181700120228         MTCtsp = Tastsp;
181800120228         MTCctr = Tasctr;
181900120228         MTCbap = Savbap;
182000120228         MTCdiv = Tasdiv;
182100120228         MTCpor = Taspor;
182200120228         MTCimv = Tasimv;
182300120228         MTCcas = Savcas;
182400120228         MTCvca = Savvca;
182500120228         MTCrmn = Tasrmn;
182600120228         MTCrma = Savrma;
182700120322         MTCerr = wMTCerr;
182800120322         MTCesc = wMTCesc;
182900120322         MTCimp = wMTCimp;
183000120322         MTCias = wMTCias;
183100120322         MTCvas = wMTCvas;
183200120322         MTCass = wMTCass;
183300120322         MTCinm = wMTCinm;
183400120322
183500120228         write  WFMTC000;
183600120228
183700120228       ENDSR;
183800120220
183900120220      /end-free
184000060126
184100060126      *------------------------------------------------------------------------*
184200060126      * Stampa manca tariffa
184300060126      *------------------------------------------------------------------------*
184400060126     c     SR_Stampa     BegSr
184500060206
184600060206      * richiamo il programma di stampa
184700060206     c                   clear                   kpjbu
184800060206     c                   movel     tnsb44ds      kpjbu
184900060206     c                   Call      'TNSB46R'
185000060206     c                   Parm                    Kpjba
185100060206
185200060206     c                   Endsr
185300120220
185400120220       //--------------------------------------------------------------
185500120220       //?Copio il file dalla QTEMP alla BUONA
185600120220       //--------------------------------------------------------------
185700120220     c     Copia_File    BEGSR
185800120220
185900120220      *?Chiudo TFMTC01L in QTEMP
186000120220     c                   close     TFMTC01L
186100121003
186200121003      /free
186300121003       //?Cancello i dati presenti sul file TFMTC00F della BUONA
186400121003       //?lo faccio con SQL così anche se impegnato vado avanti lo stesso
186500121003         IF  not wProva;
186600121003           exec sql
186700121003             DELETE  from  UNITRAGRU/TFMTC00F;
186800121003         ELSE;
186900121003           exec sql
187000121003             DELETE  from  UNITRAGRP/TFMTC00F;
187100121003         ENDIF;
187200121003      /end-free
187300120220      *?Copio il file TFMTC00F dalla QTEMP alla BUONA
187400120220     c                   clear                   comman
187500120220     c                   movel(p)  cmdc(3)       comman
187600120220     c                   Eval      %Subst(comman:38:9) = wlibc
187700120220     c                   call      'QCMDEXC'
187800120220     c                   parm                    comman
187900120220     c                   parm                    lenght
188000120220
188100120220     c                   ENDSR
188200120402
188300120402       //--------------------------------------------------------------
188400120402       //?Salvo il file WFMTC00F in un nuovo membro
188500120402       //--------------------------------------------------------------
188600120402     c     Salva_FileWF  BEGSR
188700120402
188800120402      *?Chiudo WFMTC00F in QTEMP
188900120402     c                   close     WFMTC00F
189000120402      *?Salvo il membro
189100120402     c                   clear                   comman
189200120402     c                   movel(p)  cmdw(5)       comman
189300120402     c                   Eval      %Subst(comman:64:10) = D44mbr
189400120402     c                   call      'QCMDEXC'
189500120402     c                   parm                    comman
189600120402     c                   parm                    lenght
189700120402
189800120402     c                   ENDSR
189900130108
190000130108      /free
190100130108       //--------------------------------------------------------------
190200130109       //?Bolle già contabilizzate o prepagati per Fatturato Giornaliero.
190300130109       //?dati da TITAS.
190400130108       //--------------------------------------------------------------
190500130109       BEGSR  FatGioTas;
190600130108
190700130108       //?Visto che bolla già contabilizzata non devo controllare niente
190800130108       //?e nemmeno tassare, prendo i dati da TITAS
190900130108       //?valorizzando le DS DTAS e DTASV
191000130108         clear dTASv;
191100130108         TSV(1) = TASsv1;
191200130108         TVA(1) = TASva1;
191300130108         TSV(2) = TASsv2;
191400130108         TVA(2) = TASva2;
191500130108         TSV(3) = TASsv3;
191600130108         TVA(3) = TASva3;
191700130108       //?Se la terza varia è piena cerco su TITA7 eventuali altre varie
191800130109         IF  TASsv3 <> *blanks;
191900130108           kk = 4;
192000130108           setll (TASaas:TASlnp:TASnrs:TASnsp:TAStbl) TITA730C;
192100130108           reade (TASaas:TASlnp:TASnrs:TASnsp:TAStbl) TITA730C;
192200130108           DOW  not %eof(TITA730C) and kk < 20;
192300130108             kk += 1;
192400130108             TSV(kk) = TA7svn;
192500130108             TVA(kk) = TA7van;
192600130108             reade (TASaas:TASlnp:TASnrs:TASnsp:TAStbl) TITA730C;
192700130108           ENDDO;
192800130108         ENDIF;
192900130108
193000130108         clear dTAS;
193100130108         §TASksc = TASksc;
193200130108         §TASdsp = TASaas * 10000 + TASmgs;
193300130108         §TASpor = TASpor;
193400130108         §TASimv = TASimv;
193500130109         wTassa = *on;
193600130108
193700130108         exsr agg_TFFGC;
193800130108
193900130108       ENDSR;
194000130109
194100130109       //--------------------------------------------------------------
194200130109       //?Scrivo/Aggiorno fatturato giornaliero.
194300130109       //--------------------------------------------------------------
194400130109       BEGSR  agg_TFFGC;
194500130109
194600130109         wFIL = %dec(%subst(%editc(§TASksc:'X'):1:3):3:0);
194700160121         chain (§TASdsp:wFIL) TFFGC01L;
194800130109
194900130109       //?Se c'è già un rcd aggiorno con i dati variabili
195000160121         IF  %found(TFFGC01L);
195100130109           exsr imposta_TFFGC;
195200130109           update TFFGC000;
195300130109         ENDIF;
195400130109
195500130109       //?Se rcd nuovo prima imposto i dati fissi + key poi i dati variabili
195600160121         IF  not %found(TFFGC01L);
195700130109           clear TFFGC000;
195800130109           FGCDATA = §TASdsp;
195900130109           FGCFIL  = wFIL;
196000130109
196100130109         //?valorizzo area e distretto + decodifiche
196200130109           chain wFIL AZORG01L;
196300130109           IF  %found(AZORG01L);
196400130109             FGCfild = ORGdes;
196500130109             FGCdit  = ORGfl3;
196600130109             FGCare  = ORGcar;
196700130109           ENDIF;
196800130109           clear ds05;
196900130109           TABcod = '05';
197000130109           TABkey = %editc(FGCare:'X');
197100130109           chain (codut:TABcod:TABkey) TABEL00F;
197200130109           IF  %found(TABEL00F);
197300130109             ds05 = TBLuni;
197400130109           ENDIF;
197500130109           FGCared = §05des;
197600130109           clear ds17;
197700130109           TABcod = '17';
197800130109           TABkey = FGCdit;
197900130109           chain (codut:TABcod:TABkey) TABEL00F;
198000130109           IF  %found( TABEL00F );
198100130109             ds17 = TBLuni;
198200130109           ENDIF;
198300130109           FGCditd = §17des;
198400130201
198500130201           FGCdtela = dateu;
198600130109
198700130109         //?valorizzo i dati variabili
198800130109           exsr imposta_TFFGC;
198900130109           write TFFGC000;
199000130109         ENDIF;
199100161007
199200161007         exsr agg_TFFGK;
199300130109
199400130109       ENDSR;
199500130109
199600130109       //--------------------------------------------------------------
199700130109       //?Imposto dati per il file Fatturato Giornaliero.
199800130109       //--------------------------------------------------------------
199900130109       BEGSR  imposta_TFFGC;
200000130109
200100130109       //?spedizioni tassate
200200130109         IF  wTassa;
200300130109           FGCtost += 1;
200400130109         ENDIF;
200500130109
200600130109       //?spedizioni manca tariffa
200700130109         IF  wManca;
200800130109           FGCtosm += 1;
200900130109         ENDIF;
201000130109
201100130109       //?porto e imponibile
201200130109         FGCtpor += §TASpor;
201300130109         FGCtimp += §TASimv;
201400130109
201500130109       //?varie
201600130109         kk = 1;
201700130109         FOR kk by 1 to %elem(TSV);
201800130109           SELECT;
201900130109             WHEN  TSV(kk) = '2 ';
202000130109               FGCva2 += TVA(kk);
202100130109             WHEN  TSV(kk) = '5 ';
202200130109               FGCva5 += TVA(kk);
202300130109             WHEN  TSV(kk) = '& ';
202400130109               FGCvapa += TVA(kk);
202500130109             WHEN  TSV(kk) = '* ';
202600130109               FGCvadir += TVA(kk);
202700130109             WHEN  TSV(kk) = '+ ';
202800130109               FGCvasca += TVA(kk);
202900130109             WHEN  TSV(kk) = '= ';
203000130109               FGCvarba += TVA(kk);
203100130109             WHEN  TSV(kk) = 'a ';
203200130109               FGCvaaa += TVA(kk);
203300130109             WHEN  TSV(kk) = 'A ';
203400130109               FGCvaa += TVA(kk);
203500130109             WHEN  TSV(kk) = 'b ';
203600130109               FGCvabb += TVA(kk);
203700130109             WHEN  TSV(kk) = 'B ';
203800130109               FGCvab += TVA(kk);
203900130109             WHEN  TSV(kk) = 'c ';
204000130109               FGCvacc += TVA(kk);
204100130109             WHEN  TSV(kk) = 'C ';
204200130109               FGCvac += TVA(kk);
204300130109             WHEN  TSV(kk) = 'd ';
204400130109               FGCvadd += TVA(kk);
204500130109             WHEN  TSV(kk) = 'D ';
204600130109               FGCvad += TVA(kk);
204700130109             WHEN  TSV(kk) = 'e ';
204800130109               FGCvaee += TVA(kk);
204900130109             WHEN  TSV(kk) = 'f ';
205000130109               FGCvaff += TVA(kk);
205100130109             WHEN  TSV(kk) = 'F ';
205200130109               FGCvaf += TVA(kk);
205300130109             WHEN  TSV(kk) = 'G ';
205400130109               FGCvag += TVA(kk);
205500130109             WHEN  TSV(kk) = 'h ';
205600130109               FGCvahh += TVA(kk);
205700130109             WHEN  TSV(kk) = 'H ';
205800130109               FGCvah += TVA(kk);
205900130109             WHEN  TSV(kk) = 'I ';
206000130109               FGCvai += TVA(kk);
206100130109             WHEN  TSV(kk) = 'J ';
206200130109               FGCvaj += TVA(kk);
206300130109             WHEN  TSV(kk) = 'K ';
206400130109               FGCvak += TVA(kk);
206500130109             WHEN  TSV(kk) = 'L ';
206600130109               FGCval += TVA(kk);
206700130109             WHEN  TSV(kk) = 'm ';
206800130109               FGCvamm += TVA(kk);
206900130109             WHEN  TSV(kk) = 'M ';
207000130109               FGCvam += TVA(kk);
207100130109             WHEN  TSV(kk) = 'N ';
207200130109               FGCvan += TVA(kk);
207300130109             WHEN  TSV(kk) = 'O ';
207400130109               FGCvao += TVA(kk);
207500130109             WHEN  TSV(kk) = 'P ';
207600130109               FGCvap += TVA(kk);
207700130109             WHEN  TSV(kk) = 'Q ';
207800130109               FGCvaq += TVA(kk);
207900130109             WHEN  TSV(kk) = 'R ';
208000130109               FGCvar += TVA(kk);
208100130109             WHEN  TSV(kk) = 'S ';
208200130109               FGCvas += TVA(kk);
208300130109             WHEN  TSV(kk) = 'T ';
208400130109               FGCvat += TVA(kk);
208500130109             WHEN  TSV(kk) = 'U ';
208600130109               FGCvau += TVA(kk);
208700130109             WHEN  TSV(kk) = 'W ';
208800130109               FGCvaw += TVA(kk);
208900130109             WHEN  TSV(kk) = 'X ';
209000130109               FGCvax += TVA(kk);
209100130109             WHEN  TSV(kk) = 'Y ';
209200130109               FGCvay += TVA(kk);
209300130109             WHEN  TSV(kk) = 'Z ';
209400130109               FGCvaz += TVA(kk);
209500160121             WHEN  TSV(kk) = 't ';
209600160121               FGCvapr += TVA(kk);
209700160121             WHEN  TSV(kk) = 'k ';
209800160121               FGCvapk += TVA(kk);
209900130109           ENDSL;
210000130109         ENDFOR;
210100130109
210200130109       ENDSR;
210300161007
210400161007       //--------------------------------------------------------------
210500161007       //?Scrivo/Aggiorno fatturato giornaliero per KSC
210600161007       //--------------------------------------------------------------
210700161007       BEGSR agg_TFFGK;
210800161007
210900161007         chain (§TASdsp:§TASksc) TFFGK01L;
211000161007
211100161007       //?Se c'è già un rcd aggiorno con i dati variabili
211200161007         IF  %found(TFFGK01L);
211300161007           exsr imposta_TFFGK;
211400161007           update TFFGK000;
211500161007         ENDIF;
211600161007
211700161007       //?Se rcd nuovo prima imposto i dati fissi + key poi i dati variabili
211800161007         IF  not %found(TFFGK01L);
211900161007           clear TFFGK000;
212000161007           FGKDATA = §TASdsp;
212100161007           FGKKSC  = §TASksc;
212200161007
212300161007         //?valorizzo area e distretto + decodifiche
212400161007           chain wFIL AZORG01L;
212500161007           IF  %found(AZORG01L);
212600161007             FGKdit  = ORGfl3;
212700161007             FGKare  = ORGcar;
212800161007           ENDIF;
212900161007           clear ds05;
213000161007           TABcod = '05';
213100161007           TABkey = %editc(FGKare:'X');
213200161007           chain (codut:TABcod:TABkey) TABEL00F;
213300161007           IF  %found(TABEL00F);
213400161007             ds05 = TBLuni;
213500161007           ENDIF;
213600161007           FGKared = §05des;
213700161007           clear ds17;
213800161007           TABcod = '17';
213900161007           TABkey = FGKdit;
214000161007           chain (codut:TABcod:TABkey) TABEL00F;
214100161007           IF  %found( TABEL00F );
214200161007             ds17 = TBLuni;
214300161007           ENDIF;
214400161007           FGKditd = §17des;
214500161007
214600161007           FGKdtela = dateu;
214700161007
214800161007         //?valorizzo i dati variabili
214900161007           exsr imposta_TFFGK;
215000161007           write TFFGK000;
215100161007         ENDIF;
215200161007
215300161007       ENDSR;
215400161007
215500161007       //--------------------------------------------------------------
215600161007       //?Imposto dati per il file Fatturato Giornaliero X KSC
215700161007       //--------------------------------------------------------------
215800161007       BEGSR imposta_TFFGK;
215900161007
216000161007       //?spedizioni tassate
216100161007         IF  wTassa;
216200161007           FGKtost += 1;
216300161007         ENDIF;
216400161007
216500161007       //?spedizioni manca tariffa
216600161007         IF  wManca;
216700161007           FGKtosm += 1;
216800161007         ENDIF;
216900161007
217000161007       //?porto e imponibile
217100161007         FGKtpor += §TASpor;
217200161007         FGKtimp += §TASimv;
217300161007
217400161007       //?varie
217500161007         kk = 1;
217600161007         FOR kk by 1 to %elem(TSV);
217700161007           SELECT;
217800161007             WHEN  TSV(kk) = '2 ';
217900161007               FGKva2 += TVA(kk);
218000161007             WHEN  TSV(kk) = '5 ';
218100161007               FGKva5 += TVA(kk);
218200161007             WHEN  TSV(kk) = '& ';
218300161007               FGKvapa += TVA(kk);
218400161007             WHEN  TSV(kk) = '* ';
218500161007               FGKvadir += TVA(kk);
218600161007             WHEN  TSV(kk) = '+ ';
218700161007               FGKvasca += TVA(kk);
218800161007             WHEN  TSV(kk) = '= ';
218900161007               FGKvarba += TVA(kk);
219000161007             WHEN  TSV(kk) = 'a ';
219100161007               FGKvaaa += TVA(kk);
219200161007             WHEN  TSV(kk) = 'A ';
219300161007               FGKvaa += TVA(kk);
219400161007             WHEN  TSV(kk) = 'b ';
219500161007               FGKvabb += TVA(kk);
219600161007             WHEN  TSV(kk) = 'B ';
219700161007               FGKvab += TVA(kk);
219800161007             WHEN  TSV(kk) = 'c ';
219900161007               FGKvacc += TVA(kk);
220000161007             WHEN  TSV(kk) = 'C ';
220100161007               FGKvac += TVA(kk);
220200161007             WHEN  TSV(kk) = 'd ';
220300161007               FGKvadd += TVA(kk);
220400161007             WHEN  TSV(kk) = 'D ';
220500161007               FGKvad += TVA(kk);
220600161007             WHEN  TSV(kk) = 'e ';
220700161007               FGKvaee += TVA(kk);
220800161007             WHEN  TSV(kk) = 'f ';
220900161007               FGKvaff += TVA(kk);
221000161007             WHEN  TSV(kk) = 'F ';
221100161007               FGKvaf += TVA(kk);
221200161007             WHEN  TSV(kk) = 'G ';
221300161007               FGKvag += TVA(kk);
221400161007             WHEN  TSV(kk) = 'h ';
221500161007               FGKvahh += TVA(kk);
221600161007             WHEN  TSV(kk) = 'H ';
221700161007               FGKvah += TVA(kk);
221800161007             WHEN  TSV(kk) = 'I ';
221900161007               FGKvai += TVA(kk);
222000161007             WHEN  TSV(kk) = 'J ';
222100161007               FGKvaj += TVA(kk);
222200161007             WHEN  TSV(kk) = 'K ';
222300161007               FGKvak += TVA(kk);
222400161007             WHEN  TSV(kk) = 'L ';
222500161007               FGKval += TVA(kk);
222600161007             WHEN  TSV(kk) = 'm ';
222700161007               FGKvamm += TVA(kk);
222800161007             WHEN  TSV(kk) = 'M ';
222900161007               FGKvam += TVA(kk);
223000161007             WHEN  TSV(kk) = 'N ';
223100161007               FGKvan += TVA(kk);
223200161007             WHEN  TSV(kk) = 'O ';
223300161007               FGKvao += TVA(kk);
223400161007             WHEN  TSV(kk) = 'P ';
223500161007               FGKvap += TVA(kk);
223600161007             WHEN  TSV(kk) = 'Q ';
223700161007               FGKvaq += TVA(kk);
223800161007             WHEN  TSV(kk) = 'R ';
223900161007               FGKvar += TVA(kk);
224000161007             WHEN  TSV(kk) = 'S ';
224100161007               FGKvas += TVA(kk);
224200161007             WHEN  TSV(kk) = 'T ';
224300161007               FGKvat += TVA(kk);
224400161007             WHEN  TSV(kk) = 'U ';
224500161007               FGKvau += TVA(kk);
224600161007             WHEN  TSV(kk) = 'W ';
224700161007               FGKvaw += TVA(kk);
224800161007             WHEN  TSV(kk) = 'X ';
224900161007               FGKvax += TVA(kk);
225000161007             WHEN  TSV(kk) = 'Y ';
225100161007               FGKvay += TVA(kk);
225200161007             WHEN  TSV(kk) = 'Z ';
225300161007               FGKvaz += TVA(kk);
225400161007             WHEN  TSV(kk) = 't ';
225500161007               FGKvapr += TVA(kk);
225600161007             WHEN  TSV(kk) = 'k ';
225700161007               FGKvapk += TVA(kk);
225800161007           ENDSL;
225900161007         ENDFOR;
226000130108
226100161007       ENDSR;
226200161007
226300130108      /end-free
226400060112
226500060109      *------------------------------------------------------------------------*
226600060109      * IMPOSTAZIONI INIZIALI
226700060109      *------------------------------------------------------------------------*
226800060109     c     SR_Imposta    BegSr
226900060109
227000060105      * Utente di sede
227100060110     c                   If        DutPou = 46
227200060105     c                   Eval      *In05 = *On
227300060110     c                   EndIf
227400060203
227500060203      * verifico se devo stampare il totale imponibile
227600060203     c                   eval      *in20 = (d44fsi = 'S')
227700060203
227800060105
227900060105     c                   SELECT
228000060105      *
228100060105      * se codice cliente valorizzato valorizzo il p.o. da elaborare con quello del cliente
228200060119     c                   When      D44cli <> *zeros
228300120416     c                   eval      skpo(1) = %subst(%editc(d44cli:'X'):1:3)
228400060105      *
228500060105      * verifico se valorizzata la sola linea valorizzo i p.o. da elaborare se stessa e la sua  £1
228600060105      *
228700060105     c                   When      D44Fil <> *zeros
228800060105     c                   clear                   TRUL31DS
228900060105     c                   eval      I31abi = 'PO'
229000060105     c                   eval      I31cpo = D44Fil
229100060105     c                   call      'TRUL31R'
229200060105     c                   parm                    KPJBA
229300060105     c                   parm                    TRUL31DS
229400060111     c                   if        O31pog > *zeros
229500060111     c                   movea     O31pog        Skpo
229600060111     c                   else
229700060119     c                   movel     D44fil        Skpo(1)
229800060111     c                   endif
229900060105
230000060105      *
230100060201      * verifico se valorizzata l'area
230200060105      *
230300060201     c                   When      D44Are <> *zeros
230400060201      * Reperimento dei P.O. dell'area
230500060105     c                   clear                   TRUL31DS
230600060201     c                   eval      I31abi = 'RA'
230700060201     c                   eval      I31car = D44Are
230800060105     c                   call      'TRUL31R'
230900060105     c                   parm                    KPJBA
231000060105     c                   parm                    TRUL31DS
231100060111     c                   if        O31pog > *zeros
231200060111     c                   movea     O31pog        Skpo
231300060111     c                   endif
231400060105
231500060109     c                   other
231600060109      * Reperimento dei P.O. dell'Azienda
231700060109     c                   clear                   TRUL31DS
231800060109     c                   eval      I31abi = 'AZ'
231900060109     c                   call      'TRUL31R'
232000060109     c                   parm                    KPJBA
232100060109     c                   parm                    TRUL31DS
232200060111     c                   if        O31pog > *zeros
232300060111     c                   movea     O31pog        Skpo
232400060111     c                   endif
232500060109
232600060111     c                   endsl
232700060109
232800060109      * Recupero la moneta di conto
232900060109     c                   clear                   Tibs02ds
233000060109     c                   clear                   Dged
233100060109
233200060109     c                   Movel     'C'           T02mod
233300060109     c                   Movel     KNSIF         T02sif
233400060109     c                   Movel     'GED'         T02cod
233500060109     c                   Movel     '1'           T02ke1
233600060109      *
233700060109     c                   Call      'TIBS02R'
233800060109     c                   Parm                    Kpjba
233900060109     c                   Parm                    Tibs02ds
234000060109      *
234100060109     c                   If        T02err = *blanks
234200060109     c                   Movel     T02uni        Dged
234300060109     c                   Endif
234400060109
234500060109      * Ricerca degli importi standard nella moneta di conto
234600060109
234700060109     c                   Clear                   Tibs02ds
234800060109     c                   Clear                   Dgei
234900060109     c                   Movel     'L'           T02tla
235000060109     c                   Movel     'C'           T02mod
235100060109     c                   Movel     KNSIF         T02sif
235200060109     c                   Movel     'GEI'         T02cod
235300060109     c                   Movel     §GEDCN        T02ke1
235400060109
235500060109     c                   Call      'TIBS02R'
235600060109     c                   Parm                    Kpjba
235700060109     c                   Parm                    Tibs02ds
235800060109
235900060109     c                   If        T02err =  *blanks
236000060109     c                   Movel     T02uni        Dgei
236100120328     c                   Endif
236200120328
236300120328      *?Cerco il limite imponibile + peso tabella IMP
236400120328     c                   clear                   TIBS02DS
236500120328     c                   clear                   dIMP
236600120328     c                   eval      T02tla = 'L'
236700120328     c                   eval      T02mod = 'C'
236800120328     c                   eval      T02sif = KNSIF
236900120328     c                   eval      T02cod = 'IMP'
237000120328     c                   eval      T02ke1 = '001'
237100120328     c                   call      'TIBS02R'
237200120328     c                   parm                    kpjba
237300120328     c                   parm                    TIBS02DS
237400120328     c                   IF        T02err = *blanks
237500120328     c                   eval      dIMP = T02uni
237600120328     c                   ENDIF
237700130108
237800130108      *?Cerco data ultima spedizione fatturata tabella RPF
237900130116     c                   IF        not wDaMenu and
238000130116     c                             not wDASB48
238100130108     c                   clear                   TIBS02DS
238200130108     c                   clear                   dRPFfatgio
238300130108     c                   eval      T02tla = 'L'
238400130108     c                   eval      T02mod = 'C'
238500130108     c                   eval      T02sif = KNSIF
238600130108     c                   eval      T02cod = 'RPF'
238700130108     c                   eval      T02ke1 = 'FATTGIO'
238800130108     c                   call      'TIBS02R'
238900130108     c                   parm                    kpjba
239000130108     c                   parm                    TIBS02DS
239100130108     c                   IF        T02err = *blanks
239200130108     c                   eval      dRPFfatgio = T02uni
239300130108     c                   ENDIF
239400130116     c                   ENDIF
239500060109
239600060109      * Carico tipi bolla
239700060109
239800060109     c                   Movel     'TB'          Tabcod
239900060112     c                   z-add     1             kk
240000060109     c     KeyTab2       setll     Tabel00f
240100060109     c                   Do        *hival
240200060109     c     KeyTab2       Reade     Tabel00f
240300060109
240400060109    1c                   If        %eof(Tabel00f)
240500060109     c                   leave
240600060109     c                   endif
240700060109
240800060109    1c                   If        tblflg = ' '
240900060112     c     kk            Occur     Dstb
241000060109     c                   Movel     Tbluni        Dstb
241100060112     c                   Movel     Tblkey        Tbo(kk)
241200060112     c                   add       1             kk
241300060109    2c                   endif
241400060109
241500060109    1c                   Enddo
241600060110
241700060110      * Carico le province dei codici tassazione
241800060110
241900060110     c                   Movel     'CT'          Tabcod
242000060112     c                   z-add     1             kk
242100060110     c     KeyTab2       setll     Tabel00f
242200060110     c                   Do        *hival
242300060110     c     KeyTab2       Reade     Tabel00f
242400060110
242500060110    1c                   If        %eof(Tabel00f)
242600060110     c                   leave
242700060110     c                   endif
242800060110
242900060110    1c                   If        tblflg = ' '
243000060110     c                   Movel     Tbluni        Dsct
243100060112     c                   Movel     Tblkey        Cts(kk)
243200060112     c                   Movel     §ctprv        Cpr(kk)
243300060112     c                   add       1             kk
243400060110    2c                   endif
243500060110
243600060110    1c                   Enddo
243700060110
243800060110      * carico cod.bolla che prevedono la tassazione di una sola varia
243900060110
244000060110     c                   Movel     '3A'          Tabcod
244100060112     c                   z-add     1             kk
244200060110     c     KeyTab2       setll     Tabel00f
244300060110     c                   Do        *hival
244400060110     c     KeyTab2       Reade     Tabel00f
244500060110
244600060110    1c                   If        %eof(Tabel00f)
244700060110     c                   leave
244800060110     c                   endif
244900060110
245000060110    1c                   If        tblflg = ' '
245100060110     c                   Movel     Tbluni        Ds3a
245200060110
245300060110     c                   If        §3asva <> *blanks
245400060112     c                   Movel     Tblkey        Cbo(kk)
245500060112     c                   Movel     §3asva        Cbv(kk)
245600060112     c                   add       1             kk
245700060110     c                   endif
245800060110
245900060110    2c                   endif
246000060110
246100060110    1c                   Enddo
246200060109
246300060119      * Ricerco il limite standard dell'importo da assicurare
246400060119     c                   Clear                   Tibs02ds
246500060119     c                   Clear                   Dlia
246600060119     c                   Eval      T02Mod = 'C'
246700060119     c                   Eval      T02Sif = Knsif
246800060119     c                   Eval      T02Cod = 'LIA'
246900060119     c                   Movel     '001'         T02Ke1
247000060119     c                   Call      'TIBS02R'
247100060119     c                   Parm                    Kpjba
247200060119     c                   Parm                    Tibs02ds
247300060119     c                   If        T02Err = *Blanks
247400060119     c                   Movel     T02Uni        Dlia
247500060119     c                   EndIf
247600060119
247700130108      /free
247800130108       //?Per Fatturato giornaliero
247900130108       //?devo controllare la data DAL con la data ultima spedizione fatturata
248000130108       //?se data DAL minore/uguale devo impostare come data DAL, per fatturato giornaliero,
248100130108       //?la data ultima spedizione fatturata + 1 gg
248200130108       //?come data AL va bene quella passata dal pgm di lancio
248300130116       IF  not wDaMenu and not wDASB48;
248400130116         IF  D44dsd <= §RPFfatdsp;
248500130116           wDataISO = %date(§RPFfatdsp);
248600130116           wDataISO = wDataISO + %days(1);
248700130116           wDataDa  = %dec(wDataISO);
248800130116         ELSE;
248900130116           wDataDa  = D44dsd;
249000130116         ENDIF;
249100130116         wDataAl  = D44dsa;
249200130116       ENDIF;
249300141211
249400141211       //?Carico part.consegna con abilitazione PinCode
249500141211         TABcod = '7R';
249600141211         clear kk;
249700141211         setll (Codut:TABcod) TABEL00F;
249800141211         reade (Codut:TABcod) TABEL00F;
249900141211         DOW  not %eof(TABEL00F);
250000141211           IF  TBLflg = *blanks;
250100141211             ds7R = TBLuni;
250200141211             IF  §7Rpincode = 'S';
250300141211               kk += 1;
250400141211               skGMA(kk) = TBLkey;
250500141211             ENDIF;
250600141211           ENDIF;
250700141211           reade (Codut:TABcod) TABEL00F;
250800141211         ENDDO;
250900141211
251000130108      /end-free
251100060105
251200060109     c                   endsr
251300060109      *------------------------------------------------------------------------*
251400060109      * ROUTINE INIZIALE
251500060109      *------------------------------------------------------------------------*
251600060109     c     *Inzsr        BegSr
251700060109
251800060109     c     *entry        plist
251900060109     c                   parm                    Kpjba
252000060119     c                   eval      Tnsb44ds = Kpjbu
252100060109      * Reperisco data corrente
252200060109     c                   time                    w0140
252300060109     c                   move      w0140         wData
252400060202
252500060109     c                   clear                   WLBdat
252600060109     c                   eval      G02dat = wData
252700060109     c                   call      'XSRDA8'
252800060109     c                   parm                    WLBdat
252900060109     c                   z-add     G02inv        DateU
253000060109
253100060109     c     *dtaara       define    §azute        azuteds
253200060109     c     *dtaara       define    §datiute      ddatiute
253300060109     c                   in(E)     *dtaara
253400060109     c                   If        %error  or RSUT = *blanks
253500060109     c                   CLEAR                   tibs34ds
253600060109     c                   CALL      'TIBS34R'
253700060109     c                   PARM                    tibs34ds
253800060109     c                   in        *dtaara
253900060109     c                   EndIf
254000060120
254100060206      * Se sono in ambiente buono - GAITRAGRU
254200060208     c                   If        knsif = 'FILTRA201 ' or
254300060208     c                             knsif = 'GAITRA201 '
254400121003     c                   eval      wProva = *off
254500060206     c                   Eval      wlib = 'GAITRAGRU '
254600120220     c                   Eval      wlibc= 'UNITRAGRU'
254700120313     c                   Eval      wlibw= 'GAITRAAZM'
254800060206      *  Se sono in ambiente di prova - GAITRAGRPS
254900060206     c                   Else
255000121003     c                   eval      wProva = *on
255100060206     c                   Eval      wlib = 'GAITRAGRPS'
255200121003     c                   Eval      wlibc= 'UNITRAGRP'
255300121003     c                   Eval      wlibw= 'GAITRAAZP'
255400060206     c                   EndIf
255500060120      * Se non sono in sede
255600060120     c                   If        Dutpou <> 46
255700060120
255800060120      *  titas38c
255900060120     c                   Clear                   comman
256000060120     c                   Movel(p)  cmdt(1)       comman
256100060120     c                   Eval      %Subst(comman:30:10) = wlib
256200060120     c                   Eval      comman =
256300060120     c                             %trim(comman) + '/TITAS38C)'
256400060120     c                   Call      'QCMDEXC'                            99
256500060120     c                   Parm                    comman
256600060120     c                   Parm                    lenght
256700060120     c  n99              Open      Titas38c
256800060120      *  titas02l
256900060120     c                   Clear                   comman
257000060120     c                   Movel(p)  cmdt(2)       comman
257100060120     c                   Eval      %Subst(comman:30:10) = wlib
257200060120     c                   Eval      comman =
257300060120     c                             %trim(comman) + '/TITAS02L)'
257400060120     c                   Call      'QCMDEXC'                            99
257500060120     c                   Parm                    comman
257600060120     c                   Parm                    lenght
257700060120     c  n99              Open      Titas02l
257800060120      *  tita730c
257900060120     c                   Clear                   comman
258000060120     c                   Movel(p)  cmdt(3)       comman
258100060120     c                   Eval      %Subst(comman:30:10) = wlib
258200060120     c                   Eval      comman =
258300060120     c                             %trim(comman) + '/TITA730C)'
258400060120     c                   Call      'QCMDEXC'                            99
258500060120     c                   Parm                    comman
258600060120     c                   Parm                    lenght
258700060120     c  n99              Open      Tita730c
258800060120      *  tita430c
258900060120     c                   Clear                   comman
259000060120     c                   Movel(p)  cmdt(4)       comman
259100060120     c                   Eval      %Subst(comman:30:10) = wlib
259200060120     c                   Eval      comman =
259300060120     c                             %trim(comman) + '/TITA430C)'
259400060120     c                   Call      'QCMDEXC'                            99
259500060120     c                   Parm                    comman
259600060120     c                   Parm                    lenght
259700060120     c  n99              Open      Tita430c
259800060120      *  tncsb03l
259900060120     c                   Clear                   comman
260000060120     c                   Movel(p)  cmdt(5)       comman
260100060120     c                   Eval      %Subst(comman:30:10) = wlib
260200060120     c                   Eval      comman =
260300060120     c                             %trim(comman) + '/TNCSB03L)'
260400060120     c                   Call      'QCMDEXC'                            99
260500060120     c                   Parm                    comman
260600060120     c                   Parm                    lenght
260700060120     c  n99              Open      Tncsb03l
260800060120      *  Fiar531c
260900060120     c                   Clear                   comman
261000060120     c                   Movel(p)  cmdt(6)       comman
261100060120     c                   Eval      %Subst(comman:30:10) = wlib
261200060120     c                   Eval      comman =
261300060120     c                             %trim(comman) + '/FIAR531C)'
261400060120     c                   Call      'QCMDEXC'                            99
261500060120     c                   Parm                    comman
261600060120     c                   Parm                    lenght
261700060120     c  n99              Open      Fiar531c
261800060120      *  titaa30c
261900060120     c                   Clear                   comman
262000060120     c                   Movel(p)  cmdt(7)       comman
262100060120     c                   Eval      %Subst(comman:30:10) = wlib
262200060120     c                   Eval      comman =
262300060120     c                             %trim(comman) + '/TITAA30C)'
262400060120     c                   Call      'QCMDEXC'                            99
262500060120     c                   Parm                    comman
262600060120     c                   Parm                    lenght
262700060120     c  n99              Open      Titaa30c
262800060123
262900060123      * se sono in sede faccio solo open dei file
263000060123
263100060123     c                   else
263200060123
263300060123     c                   Open      Titas38c
263400060123     c                   Open      Titas02l
263500060123     c                   Open      Tita730c
263600060123     c                   Open      Tita430c
263700060123     c                   Open      Tncsb03l
263800060123     c                   Open      Fiar531c
263900060123     c                   Open      Titaa30c
264000060123
264100060123     c                   EndIf
264200120306
264300120306      *?Controllo se è lancio da menù
264400120308     c                   eval      wDaMenu = (D44gio = 'M')
264500120306
264600120306      *?Controllo se è lancio da TNSB48R Gestione Manca Tariffa Tasto F5-Ritassa
264700120306     c                   eval      wDaSB48 = (D44SB48 = 'S')
264800120228      *
264900120228      * file di lavoro wfmtc se sono in filiale duplico in QTEMP
265000120306      *?e se NON richiamato da TNSB48
265100120228     c                   If        1 = 1
265200120306     c                             and not wDaSB48
265300120228     c                   clear                   comman
265400120228     c                   movel(p)  cmdw(1)       comman
265500120313     c                   Eval      %Subst(comman:15:9) = wlibw
265600120228     c                   call      'QCMDEXC'
265700120228     c                   parm                    comman
265800120228     c                   parm                    lenght
265900120228
266000120228     c                   clear                   comman
266100120228     c                   movel(p)  cmdw(2)       comman
266200120313     c                   Eval      %Subst(comman:34:9) = wlibw
266300120228     c                   call      'QCMDEXC'
266400120228     c                   parm                    comman
266500120228     c                   parm                    lenght
266600120228
266700120228     c                   clear                   comman
266800120228     c                   movel(p)  cmdw(3)       comman
266900120228     c                   call      'QCMDEXC'
267000120228     c                   parm                    comman
267100120228     c                   parm                    lenght
267200120228
267300120228     c                   endif
267400120228      * apro il file di comodo per la stampa
267500120306      *?se NON richiamato da TNSB48
267600120306     c                   IF        not wDaSB48
267700120228     c                   open      wfmtc00f
267800120306     c                   ENDIF
267900120228
268000120222      *?Se è lancio da menù devo duplicare il file in QTEMP VUOTO
268100120222     c                   IF        wDaMenu
268200120222     c                   clear                   comman
268300120222     c                   movel(p)  cmdc(4)       comman
268400120308     c                   Eval      %Subst(comman:34:9) = wlibc
268500120222     c                   call      'QCMDEXC'
268600120222     c                   parm                    comman
268700120222     c                   parm                    lenght
268800120222      *?Se è schedulato devo copiare il file in QTEMP PIENO
268900120222     c                   ELSE
269000120306      *?se NON richiamato da TNSB48
269100120306     c                   IF        not wDaSB48
269200120222      *?Copio il file TFMTC00F in QTEMP
269300120217     c                   clear                   comman
269400120217     c                   movel(p)  cmdc(1)       comman
269500120217     c                   Eval      %Subst(comman:15:9) = wlibc
269600120217     c                   call      'QCMDEXC'
269700120217     c                   parm                    comman
269800120217     c                   parm                    lenght
269900120306     c                   ENDIF
270000120222     c                   ENDIF
270100120222
270200120217      *?Duplico TFMTC01L in QTEMP
270300120306      *?se NON richiamato da TNSB48
270400120306     c                   IF        not wDaSB48
270500120217     c                   clear                   comman
270600120217     c                   movel(p)  cmdc(2)       comman
270700120217     c                   Eval      %Subst(comman:34:9) = wlibc
270800120217     c                   call      'QCMDEXC'
270900120217     c                   parm                    comman
271000120217     c                   parm                    lenght
271100120306     c                   ENDIF
271200120306      *?Apro TFMTC01L
271300120217     c                   open      TFMTC01L
271400120217
271500120217      /free
271600120217         //?Calcolo oggi - 60 gg.
271700120221           dataiso = %date(dateu);
271800120221           dataiso = dataiso - %days(60);
271900120221           wOggi_60 = %dec(dataiso);
272000120217      /end-free
272100120217
272200060202      *
272300060105      *
272400060105      *------------------------------------------------------------------------*
272500060105      * KLIST
272600060105      *------------------------------------------------------------------------*
272700060105
272800060105      *---- File Cnaco00f
272900060105
273000060105     c     KeyAco        Klist
273100060105     c                   Kfld                    Codut
273200060105     c                   Kfld                    DutKci
273300060105     c                   Kfld                    TasKsc
273400060105
273500060105      *---- File Titas38c
273600060105
273700060111     c     KeyTas38      Klist
273800060105     c                   Kfld                    KTasAas
273900060105     c                   Kfld                    KTasMgs
274000060113
274100060111      *---- File Titas02l
274200060111
274300060111     c     KeyTas02      Klist
274400060111     c                   Kfld                    KTasksc
274500060113
274600060113      *---- File Tita730C
274700060113
274800060113     c     KeyTa730      Klist
274900060113     c                   Kfld                    Tasaas
275000060113     c                   Kfld                    Taslnp
275100060113     c                   Kfld                    Tasnrs
275200060113     c                   Kfld                    Tasnsp
275300060113     c                   Kfld                    Tastbl
275400060119
275500060119      *---- File Tita430C
275600060119
275700060119     c     KeyTa430      Klist
275800060119     c                   Kfld                    Tasaas
275900060119     c                   Kfld                    Taslnp
276000060119     c                   Kfld                    Tasnrs
276100060119     c                   Kfld                    Tasnsp
276200060119     c                   Kfld                    Keytrc
276300060113
276400060113      *---- File Tncsb03l
276500060113
276600060113     c     KeyTncsb      Klist
276700060113     c                   Kfld                    Tasaas
276800060113     c                   Kfld                    Taslnp
276900060113     c                   Kfld                    Tasnrs
277000060113     c                   Kfld                    Tasnsp
277100060113
277200060113      *---- File Fiar531c
277300060113
277400060113     c     keyFiar5      Klist
277500060113     c                   Kfld                    TasAas
277600060113     c                   Kfld                    TasLnp
277700060113     c                   Kfld                    TasNrs
277800060113     c                   Kfld                    TasNsp
277900060113     c                   Kfld                    kAr5Trd
278000060113
278100060113      *---- File Fnevb00f
278200060113
278300060113     c     KeyEvb        Klist
278400060113     c                   Kfld                    Tasaas
278500060113     c                   Kfld                    Taslnp
278600060113     c                   Kfld                    Tasnrs
278700060113     c                   Kfld                    Tasnsp
278800060113     c                   Kfld                    Wcev
278900060109
279000060109      *---- File Tabel00f
279100060109
279200060109     c     KeyTab        Klist
279300060109     c                   Kfld                    Codut
279400060109     c                   Kfld                    Tabcod
279500060109     c                   Kfld                    Tabkey
279600060109
279700060109     c     KeyTab2       Klist
279800060109     c                   Kfld                    Codut
279900060109     c                   Kfld                    Tabcod
280000060105      *
280100060105     c                   EndSr
280200960924**   ERR
280300060120Manca Anagrafica Cliente
280400060120Anagrafica Cliente Annullata
280500060120Manca sottoconto Intest.Fattura
280600060120Sottoconto Intest.Fatt. Annullato
280700060117Tariffa valida solo in ARRIVO
280800060120Manca Tariffa
280900060120Imp.Maggiore del limite Fatturabile
281000060120Tipo fattura errato
281100060117Tariffa valida solo in PARTENZA
281200060117Tariffa BLOCCATA
281300060120Imp. d'assicurare maggiore del limite
281400060120Manca codice cliente
281500180314Peso/Collo superiore 5 volte media tariffa cliente
281600120326Imponibile maggiore di EUR xxxxxxxxxx,xxx
281700120907Imponibile a ZERO
281800050711** cmd ovrprtf
281900050712OVRPRTF PRTF1982 OUTQ(xxxxxxxxxx) USRDTA('MancaTarif') SAVE(*YES) HOLD(*YES)       1
282000060120** cmdt ovrdbf
282100060120OVRDBF FILE(TITAS38C) TOFILE(
282200060120OVRDBF FILE(TITAS02L) TOFILE(
282300060120OVRDBF FILE(TITA730C) TOFILE(
282400060120OVRDBF FILE(TITA430C) TOFILE(
282500060120OVRDBF FILE(TNCSB03L) TOFILE(
282600060120OVRDBF FILE(FIAR531C) TOFILE(
282700060120OVRDBF FILE(TITAA30C) TOFILE(
282800060120** cmdc crtdupobj
282900120217CPYF FROMFILE(         /TFMTC00F) TOFILE(QTEMP/TFMTC00F) CRTFILE(*YES)
283000120217CRTDUPOBJ OBJ(TFMTC01L) FROMLIB(          ) OBJTYPE(*FILE) TOLIB(QTEMP)
283100121003CPYF FROMFILE(QTEMP/TFMTC00F) TOFILE(         /TFMTC00F) MBROPT(*ADD)
283200120222CRTDUPOBJ OBJ(TFMTC00F) FROMLIB(          ) OBJTYPE(*FILE) TOLIB(QTEMP)
283300120224DLTF FILE(QTEMP/TFMTC*)
283400120228** cmdw crtdupobj
283500120228CPYF FROMFILE(         /WFMTC00F) TOFILE(QTEMP/WFMTC00F) FROMMBR(WFMTC00F) CRTFILE(*YES)
283600120228CRTDUPOBJ OBJ(WFMTC01L) FROMLIB(          ) OBJTYPE(*FILE) TOLIB(QTEMP)
283700120228CLRPFM FILE(QTEMP/WFMTC00F)
283800120229DLTF FILE(QTEMP/WFMTC*)
283900120402CPYF FROMFILE(QTEMP/WFMTC00F) TOFILE(GAITRAAZM/WFMTC00F) TOMBR(          ) MBROPT(*REPLACE)
