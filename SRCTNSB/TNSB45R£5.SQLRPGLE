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
106600060113     c                   z-add     Tasksc        §Tasksc
106700060113     c                   z-add     Taspkf        §Taspkf
106800060113     c                   z-add     Tasvlf        §Tasvlf
106900160530     c                   z-add     Tasvlc        §Tasvlc
107000160530     c                   z-add     Tasncr        §Tasncr
107100160530     c                   move      Tasfvf        §Tasfvf
107200160530     c                   z-add     Taspkb        §Taspkb
107300060113     c                   z-add     Tasncl        §Tasncl
107400160530     c                   z-add     Tasncl        §Tasnclb
107500060113     c                   movel     Tascts        §Tascts
107600060113     c                   movel     Tasctr        §Tasctr
107700060113     c                   z-add     Tasmgs        §Tasdsp
107800060113     c                   movel     Tasaas        §Tasdsp
107900160122      * modifico per il recupero della tariffa di cartello con la data del giorno di tassazione
108000160122     c                   movel     dateu         §Tasdct
108100060113     c                   movel     Tascad        §Tascad
108200060113     c                   movel     Tasfin        §Tasfin
108300060113     c                   movel     Tascam        §Tascam
108400060113     c                   movel     Tasfap        §Tasfap
108500060113     c                   movel     Tastsp        §Tastsp
108600060113     c                   movel     Tasftc        §Tastc1
108700060113     c                   movel     Tastc2        §Tastc2
108800060113     c                   movel     Tasdiv        §Tasdiv
108900060113      *
109000060113      * aggancio il contrassegno.
109100060113      * se trovato verifico che il 1° carattere del tipo bolla corrisponda
109200060113     c                   if        tasfca <> *blanks
109300060113     c     Keytncsb      chain     Tncsb03l
109400060113     c                   if        %found(Tncsb03l)   and
109500060113     c                             Tastbl = csbtbl
109600060113     c                   z-add     Csbcas        §Tascas                         Contrassegno
109700060113     c                   movel     Csbvca        §Tasvca                         Valuta
109800060113     c                   z-add     Csbcmb        §Tascmb                         Cambio Divisa
109900061016     c****               movel     Csbtpa        §Tastic                         Tipo Incasso
110000060113     c                   if        csbfus <> *blanks
110100060113     c                   move      Csbfus        §tastic
110200060113     c                   else
110300060113     c                   move      Csbtpi        §Tastic                         Tipo Incasso
110400060113     c                   endif
110500060113     c                   endif
110600060113     c                   endif
110700060113
110800060113     c                   movel     Tasvas        §Tasvas
110900060113     c                   movel     Tasfdn        §Tasfdn                         Merce Port.Mag.
111000060113     c                   movel     Tasmct        §Tasmct                         Cod.Tassaz.Mittente
111100060113     c                   z-add     Taslnp        §Taslnp
111200060113     c                   z-add     Taslna        §Taslna
111300060113     c                   movel     Tastbl        §Tastbl
111400060113     c                   z-add     Tasias        §Tasias                         Importo assicurazio
111500060113     c                   z-add     Tasqft        §Tasqft                         Qta' Fatturabile
111600060113     c                   z-add     Taspor        §Taspor                         Trasporto
111700060113     c                   z-add     tasimv        §Tasimv                         Tot.Imponibile
111800060113
111900060113     c                   if        Tasll1 = 'S' or Tasll1 = 'C'
112000060113     c                   movel     'S'           §Tasddt                         flag si/no ddt
112100060113     c                   else
112200060113     c                   clear                   §Tasddt
112300060113     c                   endif
112400060926     c*****              movel     tasfbr        §tasflo
112500060926     c                   clear                   dtas01
112600060926     c                   movel     tasfbr        §asfbr
112700060926     c                   movel     tascca        §ascca
112800110701      * valorizzo flag email al destinatario
112900110701     c                   movel     tasflo        dtasflo
113000110701     c                   move      §floemd       §asemd
113100141211      /free
113200151127       //?Imposto se Prenotazione Ritiro Telefonico
113300151127         §TASprt = §FLOado;
113400151127
113500141211       //?Imposto se part.consegna con PinCode
113600141211         IF  TASgma <> *blanks and %lookup (TASgma:skGMA) > 0;
113700141211           §ASpin = 'S';
113800141211         ENDIF;
113900151127
114000151127       //?Imposto se Stampa Packing List
114100151127         clear dAR5gen;
114200151127         chain (TASaas:TASlnp:TASnrs:TASnsp:'GEN') FIAR531C;
114300151127         IF  %found(FIAR531C);
114400151127           dAR5gen = AR5uni;
114500151127         ENDIF;
114600151127         IF  §AR5als = 'S' or §AR5alx = 'S';
114700151127           §TASspl = 'S';
114800151127         ENDIF;
114900141211      /end-free
115000110701
115100060926     c                   eval      §tasflo = dtas01
115200060113
115300060113     c                   clear                   Kpjbu
115400060125     c                   clear                   Monovaria
115500060113
115600060113      * Le bolle con totale imponibile le controllo per controllare l'assicurazione
115700060113     c                   clear                   Parca
115800060113     c                   setoff                                       10
115900060113    1c                   If        Tasimv > *zeros
116000060113     c                   movel     'R'           §Tassva
116100060113     c                   movel     'S'           Parca
116200060113     c                   else
116300060113     c                   z-add     1             kk
116400060113     c     tascbo        lookup    cbo(kk)                                31
116500060113     c                   if        *in31
116600060113     c                   eval      §tassva=cBV(kk)
116700060125     c                   If        cbv(kk) <> ' '
116800060125     c                   eval      monovaria = 'SI'
116900060113     c                   endif
117000060125     c                   endif
117100060113
117200060113     c                   if        §tassva<>*blanks
117300060113     c                   z-add     1             kk
117400060113     c     §tassva       lookup    tsv(kk)                                10
117500060113      * se la varia c'è ed è valorizzata e imponibile = 0 devo richiamare la tassazione
117600060113     c                   If        *In10 and tva(kk) > 0 and TasImv = 0
117700060113     c                   Eval      *In10 = *Off
117800060113     c                   EndIf
117900060113     c                   Endif
118000060113
118100060113    1c                   Endif
118200060113
118300060113      * imposto peso vdl e colli rilevati
118400060113     c                   z-add     tasncp        §tasncp
118500060113     c                   z-add     taspkc        §taspkc
118600060113     c                   clear                   §taspkcN
118700060113
118800060113      * Bancali
118900060113     c                   If        %Subst(TasGva:1:1) ='B'
119000060113     c                   Clear                   dAr5Ban
119100060113     c                   Eval      kAr5Trd = 'BAN'
119200060113     c     keyFiar5      Chain     Fiar531c
119300060113     c                   If        %Found(Fiar531c)
119400060113     c                   Movel     Ar5Uni        dAr5Ban
119500060113     c                   Eval      §TasBan = §Ar5Nba + §ar5Nb2
119600060113     c                   EndIf
119700060113     c                   EndIf
119800060113      * Colli originali
119900060113     c                   If        %Subst(TasGva:1:1) ='O'
120000060113     c                   Clear                   dAr5Bnb
120100060113     c                   Eval      kAr5Trd = 'BNB'
120200060113     c     keyFiar5      Chain     Fiar531c
120300060113     c                   If        %Found(Fiar531c)
120400060113     c                   Movel     Ar5Uni        dAr5Bnb
120500060113     c                   Eval      §TasNcl = §Ar5bCor
120600090327      * mi salvo i colli da mettere nel file manca tariffa
120700090327     c                   eval      stpNcl = §ar5bCor
120800060113     c                   EndIf
120900060113     c                   EndIf
121000060113     c
121100060113      * Se bolla di reso e tassata fino all'imponibile con varia 'N' 88888888
121200060113      * prima di tassare la bolla devo tassare la varia 'N'
121300060113     c                   if        tasimv = 0 and tasfbr = 'S'
121400060116     c                   exsr      Sr_tassan
121500060113     c                   endif
121600060113
121700060113     c                   clear                   Kpjbu
121800060113
121900060113     c                   movel     paramt        Kpjbu
122000060113      * verifico se esiste la varia & con importo uguale a 8888888 e se si passo al
122100060113      * TNSF20R il flag di calcolo varia &
122200060113      *
122300060113     c                   setoff                                       51
122400060113     c                   z-add     1             kk
122500060113     c     '&'           lookup    tsv(kk)                                51
122600060113     c                   if        *in51 and tva(kk) = 88888888
122700060113     c                   eval      §tasfcaa = 'S'
122800060113     c                   clear                   tsv(kk)
122900060113     c                   clear                   tva(kk)
123000060113     c                   endif
123100060113      * verifico se devo calcolare l'addebito di lasciato avviso
123200060113      *
123300060113      * verifico se c'è evento
123400060113     c     KeyEvb        chain     Fnevb04l
123500060113
123600060113     c                   If        %found(Fnevb04l)
123700060113     c                   eval      §tasric = 'S'
123800060113     c                   endif
123900060113
124000151001      * imposto dati
124100151001     c                   eval      tasppkb = taspkb
124200160504     c                   eval      taspfvf = TASfvf
124300160504     c                   eval      taspncr = TASncr
124400060113
124500060113     c                   if        §tassva=*blanks or not *in10
124600151001     c                   Call      'TNSF22R'
124700060113     c                   Parm                    Kpjba
124800060113     c                   Parm                    Dtas
124900060113     c                   Parm                    Dtasv
125000151001     c                   parm                    dtaspes
125100060119
125200151001     c                   eval      Wtnsf22r = '1'
125300060119
125400060113     c                   endif
125500060113
125600060113      * stampo e uso il peso usato in fatturazione
125700151013     c                   If        §Tasspc <> ' '
125800060113      * mi salvo il peso da fatturare per stamparlo in fattura
125900151013     c                   eval      taspkf=§TaspkcN
126000151013     c                   endif
126100060116      * Se tassando la bolla ho errori oppure non ho errori ma devo stampare l'imponibile
126200060116      * scrivo il record sul file work
126300060116
126400060119    1c                   If        (§taserr = ' ' and d44fsi = 'S') or
126500060116     c                              §taserr <> ' '
126600060116     c                   eval      Savbap = §tasbap
126700060116     c                   eval      Savcas = §Tascas
126800060116     c                   eval      Savvca = §Tasvca
126900060116     c                   eval      Savrma = §Tasrma
127000120322     c                   movel     §Tasmsg       wMtcerr
127100060208
127200060223     c                   If        d44fsi = 'S' and §taserr = ' '
127300120322     c                   eval      wmtcimp = 'IMP'
127400060208     c                   endif
127500060208
127600060223      * valorizzo i campi dell'imponibile e della divisa
127700060123     c                   z-add     §tasimv       tasimv
127800060123     c                   eval      tasdiv = §tasdiv
127900060123
128000120220     c                   eval      wMTCtrc = 'M'
128100060116     c                   Exsr      SR_ScriveErr
128200060116
128300060119   x1c                   else
128400120413
128500120413      * controllo se imponibile maggiore del limite massimo fatturabile
128600120413      * da tabella GEI
128700120413    2c                   If        §tasimv > §geimf
128800120413     c                   eval      wMTCtrc = 'M'
128900120413     c                   eval      wmtcerr = err(7)
129000120413     c                   eval      wmtcimp = 'IMF'
129100120413      * valorizzo i campi dell'imponibile e della divisa
129200120413     c                   z-add     §tasimv       tasimv
129300120413     c                   eval      tasdiv = §tasdiv
129400120413     c                   Exsr      SR_ScriveErr
129500120413     c                   leavesr
129600120413    2c                   endif
129700120907       //?Controllo se imponibile a zero
129800120910       //?non se bolla monovaria
129900120910    2c                   If        §tasimv = 0 and monovaria <> 'SI'
130000120907     c                   eval      wMTCtrc = 'M'
130100120910     c                   eval      wmtcerr = err(15)
130200120907     c                   eval      wmtcimp = 'IMF'
130300120907      * valorizzo i campi dell'imponibile e della divisa
130400120907     c                   z-add     §tasimv       tasimv
130500120907     c                   eval      tasdiv = §tasdiv
130600120907     c                   Exsr      SR_ScriveErr
130700120907     c                   leavesr
130800120907    2c                   endif
130900060116
131000060116      * Se tassando la bolla non ho errori e non devo stampare l'imponibile
131100120328      * controllo se imponibile nei limiti indicati nelle tabelle
131200120328       //?limite imponibile bolla + peso da tabella IMP
131300120416       //?peso fino a kg. e imponibile > di xxx
131400120328    2c                   IF        TASpkf <= §IMPpeso and §tasimv > §IMPimp1
131500060116      * verifico se imponibile inserito  manualmente
131600060116     c                   if        tasimv > 0
131700120322     c                   eval      wmtcinm = 'M'
131800060116     c                   endif
131900120326      * valorizzo i campi dell'imponibile e della divisa
132000120326     c                   z-add     §tasimv       tasimv
132100120326     c                   eval      tasdiv = §tasdiv
132200120326     c                   eval      wmtcimp = 'IMP'
132300120326     c                   eval      wMTCtrc = 'M'
132400120326     c                   eval      wmtcerr = err(14)
132500120326     c                   eval      %subst(wmtcerr:28:14) =
132600120329     c                             %trim(%editc(§IMPimp1:'4'))
132700120326     c                   Exsr      SR_ScriveErr
132800120416       //?Se torna che la bolla non ha già il Visto vado alla fine dei controlli perchè il rcd
132900120416       //?è già stato scritto nella routine scriveerr
133000120416     c                   IF        not wVisto
133100120326     c                   leavesr
133200120416       //?Se torna che la bolla ha già il Visto devo andare avanti con gli altri controlli
133300120416       //?per cercare eventuali altri errori da scrivere
133400120416     c                   ELSE
133500120416     c                   clear                   wmtcimp
133600120416     c                   clear                   wMTCtrc
133700120416     c                   clear                   wMTCerr
133800120413     c                   ENDIF
133900060119    2c                   endif
134000120416
134100120328       //?limite imponibile bolla + peso da tabella IMP
134200120416       //?peso oltre kg. e imponibile > di xx
134300120328    2c                   IF        TASpkf > §IMPpeso and §tasimv > §IMPimp2
134400120328      * verifico se imponibile inserito  manualmente
134500120328     c                   if        tasimv > 0
134600120328     c                   eval      wmtcinm = 'M'
134700120328     c                   endif
134800120328      * valorizzo i campi dell'imponibile e della divisa
134900120328     c                   z-add     §tasimv       tasimv
135000120328     c                   eval      tasdiv = §tasdiv
135100120328     c                   eval      wmtcimp = 'IMP'
135200120328     c                   eval      wMTCtrc = 'M'
135300120328     c                   eval      wmtcerr = err(14)
135400120328     c                   eval      %subst(wmtcerr:28:14) =
135500120329     c                             %trim(%editc(§IMPimp2:'4'))
135600120328     c                   Exsr      SR_ScriveErr
135700120416       //?Se torna che la bolla non ha già il Visto vado alla fine dei controlli perchè il rcd
135800120416       //?è già stato scritto nella routine scriveerr
135900120416     c                   IF        not wVisto
136000120413     c                   leavesr
136100120416       //?Se torna che la bolla ha già il Visto devo andare avanti con gli altri controlli
136200120416       //?per cercare eventuali altri errori da scrivere
136300120416     c                   ELSE
136400120416     c                   clear                   wmtcimp
136500120416     c                   clear                   wMTCtrc
136600120416     c                   clear                   wMTCerr
136700120413     c                   ENDIF
136800120328    2c                   endif
136900120328
137000060117      * verifico per le tariffe a collo il peso del collo
137100130913      * e se è superiore di oltre  5 volte alla media del cliente lo segnalo
137200060117      * non viene controllato se bolla con varia "O"
137300130913      * e peso bolla minore di un kg (xchè non significativo)
137400060117     c                   movel     tasctr        Tipo_tar
137500060119    2c                   If        Tipo_tar = '1' and tasimv = 0 and
137600090327     c                             %Subst(TasGva:1:1) <>'O'
137700130913     c                             and taspkf >  1
137800060117      * richiamo il tise70r x recuperare i dati degli ultimi 3 mesi
137900060117     c                   ExSr      Sr_tise
138000060117      * Calcolo la media del cliente
138100060117     c                   Exsr      Sr_Calmed
138200060117      * se 08 acceso scrivo
138300060117     c                   If        *in08
138400120321     c                   eval      wMTCtrc = 'M'
138500120322     c                   eval      wmtcesc = '!!!'
138600120322     c                   eval      wmtcerr = err(13)
138700120322     c                   Exsr      SR_ScriveErr
138800120322     c                   leavesr
138900060117     c                   Endif
139000060119    2c                   Endif
139100060117
139200140402      * recupero il tipo bolla
139300140402      /free
139400140402        kk = %lookup(TAStbl:tbo);
139500140402        IF  kk > 0;
139600140402          %occur(dstb) = kk;
139700140402        ENDIF;
139800140402      /end-free
139900140402
140000060117      * se imponibile in bolla uguale a zero e
140100060117      * tariffa bloccata oppure franco e tariffa
140200060125      * oppure franco e tariffa valida solo in arrivo e non è bolla monovaria
140300060125      * oppure assegnato e tariffa valida solo in partenza e non è bolla monovaria
140400060125      * segnalo l'errore
140500060117
140600060119    2c                   If        Tasimv = 0 and §tasbap <> ' '
140700060125     c                   If        §tbtpo = 'F' and §tasbap = 'A'  and
140800060125     c                             monovaria <> 'SI'
140900120402     c                   eval      wMTCtrc = 'M'
141000120322     c                   movel     err(5)        wMtcerr
141100120329     c                   Exsr      SR_ScriveErr
141200120329     c                   leavesr
141300060117     c                   endif
141400060125     c                   If        §tbtpo = 'A' and §tasbap = 'P'  and
141500060125     c                             monovaria <> 'SI'
141600120402     c                   eval      wMTCtrc = 'M'
141700120322     c                   movel     err(9)        wMtcerr
141800120329     c                   Exsr      SR_ScriveErr
141900120329     c                   leavesr
142000060117     c                   endif
142100060119     c                   If        §tasbap = 'B'
142200120402     c                   eval      wMTCtrc = 'M'
142300120322     c                   movel     err(10)       wMtcerr
142400120329     c                   Exsr      SR_ScriveErr
142500120329     c                   leavesr
142600060119     c                   endif
142700060119    2c                   endif
142800120329
142900060119      * Controllo il limite dell'importo da assicurare
143000070518    2c                   If         TasIas <> *Zeros or §TasIal <> *Zeros
143100060119     c                   Clear                   Trul22Ds
143200060119     c                   Eval      I22Cbo = TasCbo
143300060119     c                   Eval      I22Tsp = TasTsp
143400060119     c                   Eval      I22Lnp = TasLnp
143500060119     c                   Eval      I22Nzm = TasNzm
143600060119     c                   Eval      I22Lna = TasLna
143700060119     c                   Eval      I22Nzd = TasNzd
143800060119     c                   Eval      I22Ksc = TasKsc
143900060119     c                   Movel     TasCtr        I22Ctr
144000070518    3c                   If        TasIas <> *Zeros
144100060119     c                   Eval      I22Imp = TasIas
144200060119     c                   Eval      I22Div = TasVas
144300070518    3c                   EndIf
144400070518    3c                   If        §TasIal <> *Zeros
144500060119     c                   Eval      I22Imp = §TasIal
144600060119     c                   Eval      I22Div = §TasDiv
144700070518    3c                   EndIf
144800060119     c                   Call      'TRUL22R'
144900060119     c                   Parm                    trul22ds
145000060119      * controllo che non sia maggiore del limite previsto
145100060119      * non forzabile
145200070518    3c                   If        O22Fx2 <> *blanks
145300120322     c                   Z-Add     I22Imp        wMtcIas
145400120322     c                   Movel     I22Div        wMtcVas
145500120322     c                   Eval      wMtcAss = 'ASS'
145600070518      * verifico se la spedizione con mandato assicurativo è :
145700070518      * - porto assegnato
145800070518      * - Euroexpress IMPORT
145900070518      * non segnalo l'errore
146000070518    4c                   If        §task88 = 'N' and §Tbtpo = 'A'
146100070523     c     taslna        chain     azorg01l
146200070518    5c                   if        %found(azorg01l) and orgfl1 = ' '            italia
146300070523     c     taslnp        chain     azorg01l
146400070518    6c                   if        %found(azorg01l) and orgfl1 = 'E'            Estero
146500070518     c                   eval      og143 = orgde3
146600070518    7c                   if        §ogntw = 'EEX'
146700120322     c                   clear                   wMtcAss
146800120322     c                   clear                   wMtcIas
146900120322     c                   clear                   wMtcVas
147000070518    7c                   EndIf
147100070518    6c                   EndIf
147200070518    5c                   EndIf
147300070518    4c                   EndIf
147400070518    3c                   EndIf
147500060119    2c                   EndIf
147600060119
147700060119      * se devo scrivere una segnalazione di imponibile anomalo  o assicurazione  o
147800060119      * errori
147900120413     c                   If        wmtcimp = 'IMP' or wMtcAss = 'ASS' or
148000120322     c                             wmtcerr <> *blanks or wmtcesc = '!!!'
148100060119
148200060116     c                   eval      Savbap = §tasbap
148300060116     c                   eval      Savcas = §Tascas
148400060116     c                   eval      Savvca = §Tasvca
148500060116     c                   eval      Savrma = §Tasrma
148600060123      * valorizzo i campi del'imponibile e della divisa
148700060123
148800060123     c                   z-add     §tasimv       tasimv
148900060123     c                   eval      tasdiv = §tasdiv
149000120220
149100120220      /free
149200120220       //?Imposto il tipo record 'S' se non è un manca tariffa
149300120220       //?il manca tariffa lo imposto a 'M' appena lo trovo
149400120220         IF  wMTCtrc = *blanks;
149500120220           wMTCtrc = 'S';
149600120220         ENDIF;
149700120220      /end-free
149800060123
149900060116     c                   Exsr      SR_ScriveErr
150000060119
150100060116     c                   endif
150200060119
150300060119     c                   endif
150400060119      *
150500060113     c                   endsr
150600060113
150700060113      *-----------------------------------------------------------
150800060113      * Tasso la Varia 'N' 88888888
150900060113      *-----------------------------------------------------------
151000060116     c     SR_Tassan     begsr
151100060113
151200060113      * verifico se c'è la varia 'N' 88888888
151300060113     c                   eval      kk = 1
151400060113     c     'N'           lookup    tsv(kk)                                32
151500060113     c                   if        *in32 and tva(kk) = 88888888
151600060113      * richiamo il programma di tassazione
151700060113      * senza passare le varie e il porto
151800060113     c                   movea     tsv           w_tsv
151900060113     c                   movea     tva           w_tva
152000060113     c                   movea     tev           w_tev
152100060113     c                   eval      w_dtas = dtas
152200110701
152300110701      * tolgo il flag dell'invio mail al destinatario nell'eventualità ci fosse
152400110701     c                   eval      dtas01 = §tasflo
152500110701     c                   clear                   §asemd
152600141211      /free
152700141211       //?Pulisco flag PinCode
152800141211         clear §ASpin;
152900141211      /end-free
153000110701     c                   eval      §tasflo = dtas01
153100160113
153200160113      * tolgo flag per calcolo  Diritto di chiamata prenotazione  ritiro e packing list
153300160113      *
153400160113     c                   clear                   §TASPRT
153500160113     c                   clear                   §TASSPL
153600160113
153700060113
153800060113     c                   clear                   dtasv
153900060113     c                   clear                   §tasdiv
154000060113     c                   clear                   §taspor
154100060113
154200060113     c                   clear                   kpjbu
154300060113     c                   call      'TNSF20R'
154400060113     c                   parm                    kpjba
154500060113     c                   parm                    dtas
154600060113     c                   parm                    dtasv
154700060113      * valorizzo la varia 'N' 88888888
154800060113     c                   if        §taserr = ' '
154900060116     c                   z-add     §tasimv       w_tva(kk)
155000060113     c                   endif
155100060113      * ripristino le ds salvate prima
155200060113     c                   movea     w_tsv         tsv
155300060113     c                   movea     w_tva         tva
155400060113     c                   movea     w_tev         tev
155500060113     c                   eval      dtas = w_dtas
155600060113     c                   endif
155700060113
155800060113     c                   endsr
155900060117      *------------------------------------------------------------------------*
156000060116      *    richiamo il tise70r x recuperare i dati
156100060117      *------------------------------------------------------------------------*
156200060116     c     Sr_Tise       BegSr
156300060116
156400060116     c                   move      Tasaas        waaf
156500060116     c                   movel     Tasmgs        wmmf
156600060116     c                   move      01            wggf
156700060116
156800060116     c     *iso          move      wdataf        dataiso
156900060116     c                   subdur    2:*m          dataiso
157000060116     c                   extrct    dataiso:*m    wmmi
157100060116     c                   extrct    dataiso:*y    waai
157200060116
157300060116     c                   clear                   TISE70DS
157400060116     c                   eval      I70Tla = 'S'
157500060116     c                   eval      I70Tle = 'ST'
157600060116     c                   eval      I70Ksc = Tasksc
157700060116     c                   eval      I70Daf = waammf
157800060116     c                   eval      I70Dai = waammi
157900060116
158000060116     c                   call      'TISE70R'
158100060116     c                   parm                    Tise70ds
158200060116
158300060116     c                   EndSr
158400060117      *------------------------------------------------------------------------*
158500060117      *    Calcolo la media del cliente
158600060117      *------------------------------------------------------------------------*
158700060119     c     Sr_Calmed     Begsr
158800060117
158900070704     c                   clear                   *in08
159000120402
159100120402       //?Il controllo lo faccio solo se raggruppamento bolla (§TBRBL) é NORMALE (N)
159200120402      /free
159300120402        kk = %lookup(TAStbl:tbo);
159400120402        IF  kk > 0;
159500120402          %occur(dstb) = kk;
159600120402          IF  §TBrbl <> 'N';
159700120402            leavesr;
159800120402          ENDIF;
159900120402        ENDIF;
160000160518
160100160518       //?e se il n. di spedizioni è > di 50
160200160518       //?come da indicazione di Bocchi
160300160518        IF  O70nsp <= 50;
160400160518          leavesr;
160500160518        ENDIF;
160600120402      /end-free
160700120402
160800060117      * Peso unitario della media del cliente
160900060117
161000060116     c                   If        o70nco > *Zeros
161100060117     c     o70pkg        div       o70nco        wunim
161200060117      * peso unitario della spedizione
161300090327     c     TasPKF        div(h)    Tasncl        wunis
161400090324      * si è deciso di modificare il controllo del peso 10 volte superiore
161500090324      * alla media dell cliente con solo 5 volte superiore
161600090324     c**** Wunim         Mult      10            Wunimt
161700090324     c     Wunim         Mult      5             Wunimt
161800060117
161900060117     c                   eval      *in08 =(Wunis >= wunimt)
162000060117
162100070704     c                   EndIf
162200060117     c                   Endsr
162300060117
162400060112      *------------------------------------------------------------------------*
162500060112      * Scrittura file di lavoro
162600060112      *------------------------------------------------------------------------*
162700060112     c     SR_ScriveErr  BegSr
162800120220
162900120220      /free
163000120413         wVisto = *off;
163100120326       //?Se devo scrivere un manca tariffa per IMP > di §geimv prima verifico che
163200120326       //?non sia già stato vistato dall'utente, lo cerco quindi con tipo rcd V=OK da utente
163300120326         IF  wmtcimp = 'IMP' and wmtctrc = 'M';
163400120326           k_MTCtrc = 'V';
163500120413           k_MTCaas = TASaas;
163600120413           k_MTClnp = TASlnp;
163700120413           k_MTCnrs = TASnrs;
163800120413           k_MTCnsp = TASnsp;
163900120413           k_MTCtbl = TAStbl;
164000120326           chain(n) %kds(k06tfmtc) TFMTC01L;
164100120326           IF  %found(TFMTC01L);
164200120413             wVisto = *on;
164300120326             leavesr;
164400120326           ENDIF;
164500120326         ENDIF;
164600120326
164700120307       //?Se sto rielaborando un rcd di Manca tariffa già presente sul file
164800120307         IF  wRcdMtc;
164900120307           k_MTCtrc = MTCtrc;
165000120307           k_MTCaas = MTCaas;
165100120307           k_MTClnp = MTClnp;
165200120307           k_MTCnrs = MTCnrs;
165300120307           k_MTCnsp = MTCnsp;
165400120307           k_MTCtbl = MTCtbl;
165500120307           chain(e) %kds(k06tfmtc) TFMTC01L;
165600120307           IF  %error;
165700120307             leavesr;
165800120307           ENDIF;
165900120307           IF  %found(TFMTC01L);
166000120307           //?se il campo di work che pilota se segnalazione o manca tariffa è = 'S'
166100120307           //?da manca tariffa è diventato segnalazione quindi cancello il rcd di manca tariffa
166200120307             IF  wMTCtrc = 'S';
166300120307               delete  TFMTC000;
166400120307             ENDIF;
166500120307           //?se il campo di work che pilota se segnalazione o manca tariffa è = 'M'
166600120307           //?è ancora manca tariffa quindi aggiorno il rcd di manca tariffa
166700120307             IF  wMTCtrc = 'M';
166800120307               exsr MemDati;
166900120307               update  TFMTC000;
167000120307               leavesr;
167100120307             ENDIF;
167200120307           ENDIF;
167300120307         ENDIF;
167400120307         //?Se è una segnalazione verifico se non è già presente sul file
167500120307         IF  wMTCtrc = 'S';
167600120307           k_MTCtrc = wMTCtrc;
167700120308           k_MTCaas = TASaas;
167800120308           k_MTClnp = TASlnp;
167900120308           k_MTCnrs = TASnrs;
168000120308           k_MTCnsp = TASnsp;
168100120308           k_MTCtbl = TAStbl;
168200120307           chain(n) %kds(k06tfmtc) TFMTC01L;
168300120307         //?Se è già presente non faccio niente perchè non è da riscrivere
168400120307           IF  %found(TFMTC01L);
168500120307             //?Scrivo il WF
168600120307             //?se non è richiamato da TNSB48
168700120307             IF  not wDaSB48;
168800120307               exsr wrtWF;
168900120307             ENDIF;
169000120307             leavesr;
169100120307           ENDIF;
169200120307         ENDIF;
169300120307
169400120307       //?Scrivo nuovo record
169500120307         MTCtrc = wMTCtrc;
169600120307         MTCaas = TASaas;
169700120307         MTClnp = TASlnp;
169800120307         MTCnrs = TASnrs;
169900120307         MTCnsp = TASnsp;
170000120307         MTCtbl = TAStbl;
170100120307         MTCDSP = TASaas * 10000;
170200120307         MTCdsp += TASmgs;
170300120307         MTCksc = TASksc;
170400120322         clear MTCdstp;
170500120307         exsr MemDati;
170600120307         write  TFMTC000;
170700120307         //?Scrivo il WF
170800120307         //?se non è richiamato da TNSB48
170900120307         IF  not wDaSB48;
171000120307           exsr wrtWF;
171100120307         ENDIF;
171200060112
171300120220       ENDSR;
171400120220
171500120220       //--------------------------------------------------------------
171600120220       //?Memorizzo i dati su TFMTC
171700120220       //--------------------------------------------------------------
171800120220       BEGSR MemDati;
171900120220
172000120220       //?Reperisco la provincia del codice tassazione
172100120220         clear savpro;
172200120220         kk = %lookup(TAScts:cts);
172300120220         IF  kk > 0;
172400120220           savpro = cpr(kk);
172500120220         ENDIF;
172600120220
172700120220         MTCpoc = Savpo;
172800120220         MTCcar = Savcar;
172900120220         MTCrag = Savrag;
173000120220         MTCage = Savage;
173100120220
173200120220       //?verifico se il campo colli originali da stampare è valorizzato
173300120220         IF  StpNcl <> 0 and StpNcl <> Tasncl;
173400120220           MTCNcl = StpNcl;
173500120322           %Subst(wMtcEsc:1:1) = 'o';
173600120220         ELSE;
173700120220           MTCncl = Tasncl;
173800120220         ENDIF;
173900120220
174000120220         MTCpkf = Taspkf;
174100120220         MTCvlf = Tasvlf;
174200120220         MTCfap = Tasfap;
174300120220         MTClna = Taslna;
174400120220         MTCcts = Tascts;
174500120220         MTCfin = Tasfin;
174600120220         MTCpro = Savpro;
174700120220         MTCtsp = Tastsp;
174800120220         MTCctr = Tasctr;
174900120220         MTCbap = Savbap;
175000120220         MTCdiv = Tasdiv;
175100120220         MTCpor = Taspor;
175200120220         MTCimv = Tasimv;
175300120220         MTCcas = Savcas;
175400120220         MTCvca = Savvca;
175500120220         MTCrmn = Tasrmn;
175600120220         MTCrma = Savrma;
175700120322         MTCerr = wMTCerr;
175800120322         MTCesc = wMTCesc;
175900120322         MTCimp = wMTCimp;
176000120322         MTCias = wMTCias;
176100120322         MTCvas = wMTCvas;
176200120322         MTCass = wMTCass;
176300120322         MTCinm = wMTCinm;
176400120220
176500120220       ENDSR;
176600120228
176700120228       //--------------------------------------------------------------
176800120228       //?Scrivo il file WFTMC
176900120228       //--------------------------------------------------------------
177000120228       BEGSR wrtWF;
177100120329
177200120329       //?Se errore IMP ed elaborazione giornaliera non la scrivo sul WF
177300120329       //?così non viene stampata tutti i giorni
177400120329         IF  wMTCtrc = 'M' and wMTCimp = 'IMP' and d44gio = 'S';
177500120329           leavesr;
177600120329         ENDIF;
177700120228
177800120228       //?Reperisco la provincia del codice tassazione
177900120228         clear savpro;
178000120228         kk = %lookup(TAScts:cts);
178100120228         IF  kk > 0;
178200120228           savpro = cpr(kk);
178300120228         ENDIF;
178400120228
178500120228         MTCdel = dateu;
178600120228         MTCksc = TASksc;
178700120228         MTCpoc = Savpo;
178800120228         MTCcar = Savcar;
178900120228         MTCage = Savage;
179000120228         MTCrag = Savrag;
179100120228         MTCaas = TASaas;
179200120228         MTClnp = TASlnp;
179300120228         MTCnrs = TASnrs;
179400120228         MTCnsp = TASnsp;
179500120228         MTCtbl = TAStbl;
179600120228         MTCDSP = TASaas * 10000;
179700120228         MTCdsp += TASmgs;
179800120228
179900120228       //?verifico se il campo colli originali da stampare è valorizzato
180000120228         IF  StpNcl <> 0 and StpNcl <> Tasncl;
180100120228           MTCNcl = StpNcl;
180200120322           %Subst(wMtcEsc:1:1) = 'o';
180300120228         ELSE;
180400120228           MTCncl = Tasncl;
180500120228         ENDIF;
180600120228
180700120228         MTCpkf = Taspkf;
180800120228         MTCvlf = Tasvlf;
180900120228         MTCfap = Tasfap;
181000120228         MTClna = Taslna;
181100120228         MTCcts = Tascts;
181200120228         MTCfin = Tasfin;
181300120228         MTCpro = Savpro;
181400120228         MTCtsp = Tastsp;
181500120228         MTCctr = Tasctr;
181600120228         MTCbap = Savbap;
181700120228         MTCdiv = Tasdiv;
181800120228         MTCpor = Taspor;
181900120228         MTCimv = Tasimv;
182000120228         MTCcas = Savcas;
182100120228         MTCvca = Savvca;
182200120228         MTCrmn = Tasrmn;
182300120228         MTCrma = Savrma;
182400120322         MTCerr = wMTCerr;
182500120322         MTCesc = wMTCesc;
182600120322         MTCimp = wMTCimp;
182700120322         MTCias = wMTCias;
182800120322         MTCvas = wMTCvas;
182900120322         MTCass = wMTCass;
183000120322         MTCinm = wMTCinm;
183100120322
183200120228         write  WFMTC000;
183300120228
183400120228       ENDSR;
183500120220
183600120220      /end-free
183700060126
183800060126      *------------------------------------------------------------------------*
183900060126      * Stampa manca tariffa
184000060126      *------------------------------------------------------------------------*
184100060126     c     SR_Stampa     BegSr
184200060206
184300060206      * richiamo il programma di stampa
184400060206     c                   clear                   kpjbu
184500060206     c                   movel     tnsb44ds      kpjbu
184600060206     c                   Call      'TNSB46R'
184700060206     c                   Parm                    Kpjba
184800060206
184900060206     c                   Endsr
185000120220
185100120220       //--------------------------------------------------------------
185200120220       //?Copio il file dalla QTEMP alla BUONA
185300120220       //--------------------------------------------------------------
185400120220     c     Copia_File    BEGSR
185500120220
185600120220      *?Chiudo TFMTC01L in QTEMP
185700120220     c                   close     TFMTC01L
185800121003
185900121003      /free
186000121003       //?Cancello i dati presenti sul file TFMTC00F della BUONA
186100121003       //?lo faccio con SQL così anche se impegnato vado avanti lo stesso
186200121003         IF  not wProva;
186300121003           exec sql
186400121003             DELETE  from  UNITRAGRU/TFMTC00F;
186500121003         ELSE;
186600121003           exec sql
186700121003             DELETE  from  UNITRAGRP/TFMTC00F;
186800121003         ENDIF;
186900121003      /end-free
187000120220      *?Copio il file TFMTC00F dalla QTEMP alla BUONA
187100120220     c                   clear                   comman
187200120220     c                   movel(p)  cmdc(3)       comman
187300120220     c                   Eval      %Subst(comman:38:9) = wlibc
187400120220     c                   call      'QCMDEXC'
187500120220     c                   parm                    comman
187600120220     c                   parm                    lenght
187700120220
187800120220     c                   ENDSR
187900120402
188000120402       //--------------------------------------------------------------
188100120402       //?Salvo il file WFMTC00F in un nuovo membro
188200120402       //--------------------------------------------------------------
188300120402     c     Salva_FileWF  BEGSR
188400120402
188500120402      *?Chiudo WFMTC00F in QTEMP
188600120402     c                   close     WFMTC00F
188700120402      *?Salvo il membro
188800120402     c                   clear                   comman
188900120402     c                   movel(p)  cmdw(5)       comman
189000120402     c                   Eval      %Subst(comman:64:10) = D44mbr
189100120402     c                   call      'QCMDEXC'
189200120402     c                   parm                    comman
189300120402     c                   parm                    lenght
189400120402
189500120402     c                   ENDSR
189600130108
189700130108      /free
189800130108       //--------------------------------------------------------------
189900130109       //?Bolle già contabilizzate o prepagati per Fatturato Giornaliero.
190000130109       //?dati da TITAS.
190100130108       //--------------------------------------------------------------
190200130109       BEGSR  FatGioTas;
190300130108
190400130108       //?Visto che bolla già contabilizzata non devo controllare niente
190500130108       //?e nemmeno tassare, prendo i dati da TITAS
190600130108       //?valorizzando le DS DTAS e DTASV
190700130108         clear dTASv;
190800130108         TSV(1) = TASsv1;
190900130108         TVA(1) = TASva1;
191000130108         TSV(2) = TASsv2;
191100130108         TVA(2) = TASva2;
191200130108         TSV(3) = TASsv3;
191300130108         TVA(3) = TASva3;
191400130108       //?Se la terza varia è piena cerco su TITA7 eventuali altre varie
191500130109         IF  TASsv3 <> *blanks;
191600130108           kk = 4;
191700130108           setll (TASaas:TASlnp:TASnrs:TASnsp:TAStbl) TITA730C;
191800130108           reade (TASaas:TASlnp:TASnrs:TASnsp:TAStbl) TITA730C;
191900130108           DOW  not %eof(TITA730C) and kk < 20;
192000130108             kk += 1;
192100130108             TSV(kk) = TA7svn;
192200130108             TVA(kk) = TA7van;
192300130108             reade (TASaas:TASlnp:TASnrs:TASnsp:TAStbl) TITA730C;
192400130108           ENDDO;
192500130108         ENDIF;
192600130108
192700130108         clear dTAS;
192800130108         §TASksc = TASksc;
192900130108         §TASdsp = TASaas * 10000 + TASmgs;
193000130108         §TASpor = TASpor;
193100130108         §TASimv = TASimv;
193200130109         wTassa = *on;
193300130108
193400130108         exsr agg_TFFGC;
193500130108
193600130108       ENDSR;
193700130109
193800130109       //--------------------------------------------------------------
193900130109       //?Scrivo/Aggiorno fatturato giornaliero.
194000130109       //--------------------------------------------------------------
194100130109       BEGSR  agg_TFFGC;
194200130109
194300130109         wFIL = %dec(%subst(%editc(§TASksc:'X'):1:3):3:0);
194400160121         chain (§TASdsp:wFIL) TFFGC01L;
194500130109
194600130109       //?Se c'è già un rcd aggiorno con i dati variabili
194700160121         IF  %found(TFFGC01L);
194800130109           exsr imposta_TFFGC;
194900130109           update TFFGC000;
195000130109         ENDIF;
195100130109
195200130109       //?Se rcd nuovo prima imposto i dati fissi + key poi i dati variabili
195300160121         IF  not %found(TFFGC01L);
195400130109           clear TFFGC000;
195500130109           FGCDATA = §TASdsp;
195600130109           FGCFIL  = wFIL;
195700130109
195800130109         //?valorizzo area e distretto + decodifiche
195900130109           chain wFIL AZORG01L;
196000130109           IF  %found(AZORG01L);
196100130109             FGCfild = ORGdes;
196200130109             FGCdit  = ORGfl3;
196300130109             FGCare  = ORGcar;
196400130109           ENDIF;
196500130109           clear ds05;
196600130109           TABcod = '05';
196700130109           TABkey = %editc(FGCare:'X');
196800130109           chain (codut:TABcod:TABkey) TABEL00F;
196900130109           IF  %found(TABEL00F);
197000130109             ds05 = TBLuni;
197100130109           ENDIF;
197200130109           FGCared = §05des;
197300130109           clear ds17;
197400130109           TABcod = '17';
197500130109           TABkey = FGCdit;
197600130109           chain (codut:TABcod:TABkey) TABEL00F;
197700130109           IF  %found( TABEL00F );
197800130109             ds17 = TBLuni;
197900130109           ENDIF;
198000130109           FGCditd = §17des;
198100130201
198200130201           FGCdtela = dateu;
198300130109
198400130109         //?valorizzo i dati variabili
198500130109           exsr imposta_TFFGC;
198600130109           write TFFGC000;
198700130109         ENDIF;
198800161007
198900161007         exsr agg_TFFGK;
199000130109
199100130109       ENDSR;
199200130109
199300130109       //--------------------------------------------------------------
199400130109       //?Imposto dati per il file Fatturato Giornaliero.
199500130109       //--------------------------------------------------------------
199600130109       BEGSR  imposta_TFFGC;
199700130109
199800130109       //?spedizioni tassate
199900130109         IF  wTassa;
200000130109           FGCtost += 1;
200100130109         ENDIF;
200200130109
200300130109       //?spedizioni manca tariffa
200400130109         IF  wManca;
200500130109           FGCtosm += 1;
200600130109         ENDIF;
200700130109
200800130109       //?porto e imponibile
200900130109         FGCtpor += §TASpor;
201000130109         FGCtimp += §TASimv;
201100130109
201200130109       //?varie
201300130109         kk = 1;
201400130109         FOR kk by 1 to %elem(TSV);
201500130109           SELECT;
201600130109             WHEN  TSV(kk) = '2 ';
201700130109               FGCva2 += TVA(kk);
201800130109             WHEN  TSV(kk) = '5 ';
201900130109               FGCva5 += TVA(kk);
202000130109             WHEN  TSV(kk) = '& ';
202100130109               FGCvapa += TVA(kk);
202200130109             WHEN  TSV(kk) = '* ';
202300130109               FGCvadir += TVA(kk);
202400130109             WHEN  TSV(kk) = '+ ';
202500130109               FGCvasca += TVA(kk);
202600130109             WHEN  TSV(kk) = '= ';
202700130109               FGCvarba += TVA(kk);
202800130109             WHEN  TSV(kk) = 'a ';
202900130109               FGCvaaa += TVA(kk);
203000130109             WHEN  TSV(kk) = 'A ';
203100130109               FGCvaa += TVA(kk);
203200130109             WHEN  TSV(kk) = 'b ';
203300130109               FGCvabb += TVA(kk);
203400130109             WHEN  TSV(kk) = 'B ';
203500130109               FGCvab += TVA(kk);
203600130109             WHEN  TSV(kk) = 'c ';
203700130109               FGCvacc += TVA(kk);
203800130109             WHEN  TSV(kk) = 'C ';
203900130109               FGCvac += TVA(kk);
204000130109             WHEN  TSV(kk) = 'd ';
204100130109               FGCvadd += TVA(kk);
204200130109             WHEN  TSV(kk) = 'D ';
204300130109               FGCvad += TVA(kk);
204400130109             WHEN  TSV(kk) = 'e ';
204500130109               FGCvaee += TVA(kk);
204600130109             WHEN  TSV(kk) = 'f ';
204700130109               FGCvaff += TVA(kk);
204800130109             WHEN  TSV(kk) = 'F ';
204900130109               FGCvaf += TVA(kk);
205000130109             WHEN  TSV(kk) = 'G ';
205100130109               FGCvag += TVA(kk);
205200130109             WHEN  TSV(kk) = 'h ';
205300130109               FGCvahh += TVA(kk);
205400130109             WHEN  TSV(kk) = 'H ';
205500130109               FGCvah += TVA(kk);
205600130109             WHEN  TSV(kk) = 'I ';
205700130109               FGCvai += TVA(kk);
205800130109             WHEN  TSV(kk) = 'J ';
205900130109               FGCvaj += TVA(kk);
206000130109             WHEN  TSV(kk) = 'K ';
206100130109               FGCvak += TVA(kk);
206200130109             WHEN  TSV(kk) = 'L ';
206300130109               FGCval += TVA(kk);
206400130109             WHEN  TSV(kk) = 'm ';
206500130109               FGCvamm += TVA(kk);
206600130109             WHEN  TSV(kk) = 'M ';
206700130109               FGCvam += TVA(kk);
206800130109             WHEN  TSV(kk) = 'N ';
206900130109               FGCvan += TVA(kk);
207000130109             WHEN  TSV(kk) = 'O ';
207100130109               FGCvao += TVA(kk);
207200130109             WHEN  TSV(kk) = 'P ';
207300130109               FGCvap += TVA(kk);
207400130109             WHEN  TSV(kk) = 'Q ';
207500130109               FGCvaq += TVA(kk);
207600130109             WHEN  TSV(kk) = 'R ';
207700130109               FGCvar += TVA(kk);
207800130109             WHEN  TSV(kk) = 'S ';
207900130109               FGCvas += TVA(kk);
208000130109             WHEN  TSV(kk) = 'T ';
208100130109               FGCvat += TVA(kk);
208200130109             WHEN  TSV(kk) = 'U ';
208300130109               FGCvau += TVA(kk);
208400130109             WHEN  TSV(kk) = 'W ';
208500130109               FGCvaw += TVA(kk);
208600130109             WHEN  TSV(kk) = 'X ';
208700130109               FGCvax += TVA(kk);
208800130109             WHEN  TSV(kk) = 'Y ';
208900130109               FGCvay += TVA(kk);
209000130109             WHEN  TSV(kk) = 'Z ';
209100130109               FGCvaz += TVA(kk);
209200160121             WHEN  TSV(kk) = 't ';
209300160121               FGCvapr += TVA(kk);
209400160121             WHEN  TSV(kk) = 'k ';
209500160121               FGCvapk += TVA(kk);
209600130109           ENDSL;
209700130109         ENDFOR;
209800130109
209900130109       ENDSR;
210000161007
210100161007       //--------------------------------------------------------------
210200161007       //?Scrivo/Aggiorno fatturato giornaliero per KSC
210300161007       //--------------------------------------------------------------
210400161007       BEGSR agg_TFFGK;
210500161007
210600161007         chain (§TASdsp:§TASksc) TFFGK01L;
210700161007
210800161007       //?Se c'è già un rcd aggiorno con i dati variabili
210900161007         IF  %found(TFFGK01L);
211000161007           exsr imposta_TFFGK;
211100161007           update TFFGK000;
211200161007         ENDIF;
211300161007
211400161007       //?Se rcd nuovo prima imposto i dati fissi + key poi i dati variabili
211500161007         IF  not %found(TFFGK01L);
211600161007           clear TFFGK000;
211700161007           FGKDATA = §TASdsp;
211800161007           FGKKSC  = §TASksc;
211900161007
212000161007         //?valorizzo area e distretto + decodifiche
212100161007           chain wFIL AZORG01L;
212200161007           IF  %found(AZORG01L);
212300161007             FGKdit  = ORGfl3;
212400161007             FGKare  = ORGcar;
212500161007           ENDIF;
212600161007           clear ds05;
212700161007           TABcod = '05';
212800161007           TABkey = %editc(FGKare:'X');
212900161007           chain (codut:TABcod:TABkey) TABEL00F;
213000161007           IF  %found(TABEL00F);
213100161007             ds05 = TBLuni;
213200161007           ENDIF;
213300161007           FGKared = §05des;
213400161007           clear ds17;
213500161007           TABcod = '17';
213600161007           TABkey = FGKdit;
213700161007           chain (codut:TABcod:TABkey) TABEL00F;
213800161007           IF  %found( TABEL00F );
213900161007             ds17 = TBLuni;
214000161007           ENDIF;
214100161007           FGKditd = §17des;
214200161007
214300161007           FGKdtela = dateu;
214400161007
214500161007         //?valorizzo i dati variabili
214600161007           exsr imposta_TFFGK;
214700161007           write TFFGK000;
214800161007         ENDIF;
214900161007
215000161007       ENDSR;
215100161007
215200161007       //--------------------------------------------------------------
215300161007       //?Imposto dati per il file Fatturato Giornaliero X KSC
215400161007       //--------------------------------------------------------------
215500161007       BEGSR imposta_TFFGK;
215600161007
215700161007       //?spedizioni tassate
215800161007         IF  wTassa;
215900161007           FGKtost += 1;
216000161007         ENDIF;
216100161007
216200161007       //?spedizioni manca tariffa
216300161007         IF  wManca;
216400161007           FGKtosm += 1;
216500161007         ENDIF;
216600161007
216700161007       //?porto e imponibile
216800161007         FGKtpor += §TASpor;
216900161007         FGKtimp += §TASimv;
217000161007
217100161007       //?varie
217200161007         kk = 1;
217300161007         FOR kk by 1 to %elem(TSV);
217400161007           SELECT;
217500161007             WHEN  TSV(kk) = '2 ';
217600161007               FGKva2 += TVA(kk);
217700161007             WHEN  TSV(kk) = '5 ';
217800161007               FGKva5 += TVA(kk);
217900161007             WHEN  TSV(kk) = '& ';
218000161007               FGKvapa += TVA(kk);
218100161007             WHEN  TSV(kk) = '* ';
218200161007               FGKvadir += TVA(kk);
218300161007             WHEN  TSV(kk) = '+ ';
218400161007               FGKvasca += TVA(kk);
218500161007             WHEN  TSV(kk) = '= ';
218600161007               FGKvarba += TVA(kk);
218700161007             WHEN  TSV(kk) = 'a ';
218800161007               FGKvaaa += TVA(kk);
218900161007             WHEN  TSV(kk) = 'A ';
219000161007               FGKvaa += TVA(kk);
219100161007             WHEN  TSV(kk) = 'b ';
219200161007               FGKvabb += TVA(kk);
219300161007             WHEN  TSV(kk) = 'B ';
219400161007               FGKvab += TVA(kk);
219500161007             WHEN  TSV(kk) = 'c ';
219600161007               FGKvacc += TVA(kk);
219700161007             WHEN  TSV(kk) = 'C ';
219800161007               FGKvac += TVA(kk);
219900161007             WHEN  TSV(kk) = 'd ';
220000161007               FGKvadd += TVA(kk);
220100161007             WHEN  TSV(kk) = 'D ';
220200161007               FGKvad += TVA(kk);
220300161007             WHEN  TSV(kk) = 'e ';
220400161007               FGKvaee += TVA(kk);
220500161007             WHEN  TSV(kk) = 'f ';
220600161007               FGKvaff += TVA(kk);
220700161007             WHEN  TSV(kk) = 'F ';
220800161007               FGKvaf += TVA(kk);
220900161007             WHEN  TSV(kk) = 'G ';
221000161007               FGKvag += TVA(kk);
221100161007             WHEN  TSV(kk) = 'h ';
221200161007               FGKvahh += TVA(kk);
221300161007             WHEN  TSV(kk) = 'H ';
221400161007               FGKvah += TVA(kk);
221500161007             WHEN  TSV(kk) = 'I ';
221600161007               FGKvai += TVA(kk);
221700161007             WHEN  TSV(kk) = 'J ';
221800161007               FGKvaj += TVA(kk);
221900161007             WHEN  TSV(kk) = 'K ';
222000161007               FGKvak += TVA(kk);
222100161007             WHEN  TSV(kk) = 'L ';
222200161007               FGKval += TVA(kk);
222300161007             WHEN  TSV(kk) = 'm ';
222400161007               FGKvamm += TVA(kk);
222500161007             WHEN  TSV(kk) = 'M ';
222600161007               FGKvam += TVA(kk);
222700161007             WHEN  TSV(kk) = 'N ';
222800161007               FGKvan += TVA(kk);
222900161007             WHEN  TSV(kk) = 'O ';
223000161007               FGKvao += TVA(kk);
223100161007             WHEN  TSV(kk) = 'P ';
223200161007               FGKvap += TVA(kk);
223300161007             WHEN  TSV(kk) = 'Q ';
223400161007               FGKvaq += TVA(kk);
223500161007             WHEN  TSV(kk) = 'R ';
223600161007               FGKvar += TVA(kk);
223700161007             WHEN  TSV(kk) = 'S ';
223800161007               FGKvas += TVA(kk);
223900161007             WHEN  TSV(kk) = 'T ';
224000161007               FGKvat += TVA(kk);
224100161007             WHEN  TSV(kk) = 'U ';
224200161007               FGKvau += TVA(kk);
224300161007             WHEN  TSV(kk) = 'W ';
224400161007               FGKvaw += TVA(kk);
224500161007             WHEN  TSV(kk) = 'X ';
224600161007               FGKvax += TVA(kk);
224700161007             WHEN  TSV(kk) = 'Y ';
224800161007               FGKvay += TVA(kk);
224900161007             WHEN  TSV(kk) = 'Z ';
225000161007               FGKvaz += TVA(kk);
225100161007             WHEN  TSV(kk) = 't ';
225200161007               FGKvapr += TVA(kk);
225300161007             WHEN  TSV(kk) = 'k ';
225400161007               FGKvapk += TVA(kk);
225500161007           ENDSL;
225600161007         ENDFOR;
225700130108
225800161007       ENDSR;
225900161007
226000130108      /end-free
226100060112
226200060109      *------------------------------------------------------------------------*
226300060109      * IMPOSTAZIONI INIZIALI
226400060109      *------------------------------------------------------------------------*
226500060109     c     SR_Imposta    BegSr
226600060109
226700060105      * Utente di sede
226800060110     c                   If        DutPou = 46
226900060105     c                   Eval      *In05 = *On
227000060110     c                   EndIf
227100060203
227200060203      * verifico se devo stampare il totale imponibile
227300060203     c                   eval      *in20 = (d44fsi = 'S')
227400060203
227500060105
227600060105     c                   SELECT
227700060105      *
227800060105      * se codice cliente valorizzato valorizzo il p.o. da elaborare con quello del cliente
227900060119     c                   When      D44cli <> *zeros
228000120416     c                   eval      skpo(1) = %subst(%editc(d44cli:'X'):1:3)
228100060105      *
228200060105      * verifico se valorizzata la sola linea valorizzo i p.o. da elaborare se stessa e la sua  £1
228300060105      *
228400060105     c                   When      D44Fil <> *zeros
228500060105     c                   clear                   TRUL31DS
228600060105     c                   eval      I31abi = 'PO'
228700060105     c                   eval      I31cpo = D44Fil
228800060105     c                   call      'TRUL31R'
228900060105     c                   parm                    KPJBA
229000060105     c                   parm                    TRUL31DS
229100060111     c                   if        O31pog > *zeros
229200060111     c                   movea     O31pog        Skpo
229300060111     c                   else
229400060119     c                   movel     D44fil        Skpo(1)
229500060111     c                   endif
229600060105
229700060105      *
229800060201      * verifico se valorizzata l'area
229900060105      *
230000060201     c                   When      D44Are <> *zeros
230100060201      * Reperimento dei P.O. dell'area
230200060105     c                   clear                   TRUL31DS
230300060201     c                   eval      I31abi = 'RA'
230400060201     c                   eval      I31car = D44Are
230500060105     c                   call      'TRUL31R'
230600060105     c                   parm                    KPJBA
230700060105     c                   parm                    TRUL31DS
230800060111     c                   if        O31pog > *zeros
230900060111     c                   movea     O31pog        Skpo
231000060111     c                   endif
231100060105
231200060109     c                   other
231300060109      * Reperimento dei P.O. dell'Azienda
231400060109     c                   clear                   TRUL31DS
231500060109     c                   eval      I31abi = 'AZ'
231600060109     c                   call      'TRUL31R'
231700060109     c                   parm                    KPJBA
231800060109     c                   parm                    TRUL31DS
231900060111     c                   if        O31pog > *zeros
232000060111     c                   movea     O31pog        Skpo
232100060111     c                   endif
232200060109
232300060111     c                   endsl
232400060109
232500060109      * Recupero la moneta di conto
232600060109     c                   clear                   Tibs02ds
232700060109     c                   clear                   Dged
232800060109
232900060109     c                   Movel     'C'           T02mod
233000060109     c                   Movel     KNSIF         T02sif
233100060109     c                   Movel     'GED'         T02cod
233200060109     c                   Movel     '1'           T02ke1
233300060109      *
233400060109     c                   Call      'TIBS02R'
233500060109     c                   Parm                    Kpjba
233600060109     c                   Parm                    Tibs02ds
233700060109      *
233800060109     c                   If        T02err = *blanks
233900060109     c                   Movel     T02uni        Dged
234000060109     c                   Endif
234100060109
234200060109      * Ricerca degli importi standard nella moneta di conto
234300060109
234400060109     c                   Clear                   Tibs02ds
234500060109     c                   Clear                   Dgei
234600060109     c                   Movel     'L'           T02tla
234700060109     c                   Movel     'C'           T02mod
234800060109     c                   Movel     KNSIF         T02sif
234900060109     c                   Movel     'GEI'         T02cod
235000060109     c                   Movel     §GEDCN        T02ke1
235100060109
235200060109     c                   Call      'TIBS02R'
235300060109     c                   Parm                    Kpjba
235400060109     c                   Parm                    Tibs02ds
235500060109
235600060109     c                   If        T02err =  *blanks
235700060109     c                   Movel     T02uni        Dgei
235800120328     c                   Endif
235900120328
236000120328      *?Cerco il limite imponibile + peso tabella IMP
236100120328     c                   clear                   TIBS02DS
236200120328     c                   clear                   dIMP
236300120328     c                   eval      T02tla = 'L'
236400120328     c                   eval      T02mod = 'C'
236500120328     c                   eval      T02sif = KNSIF
236600120328     c                   eval      T02cod = 'IMP'
236700120328     c                   eval      T02ke1 = '001'
236800120328     c                   call      'TIBS02R'
236900120328     c                   parm                    kpjba
237000120328     c                   parm                    TIBS02DS
237100120328     c                   IF        T02err = *blanks
237200120328     c                   eval      dIMP = T02uni
237300120328     c                   ENDIF
237400130108
237500130108      *?Cerco data ultima spedizione fatturata tabella RPF
237600130116     c                   IF        not wDaMenu and
237700130116     c                             not wDASB48
237800130108     c                   clear                   TIBS02DS
237900130108     c                   clear                   dRPFfatgio
238000130108     c                   eval      T02tla = 'L'
238100130108     c                   eval      T02mod = 'C'
238200130108     c                   eval      T02sif = KNSIF
238300130108     c                   eval      T02cod = 'RPF'
238400130108     c                   eval      T02ke1 = 'FATTGIO'
238500130108     c                   call      'TIBS02R'
238600130108     c                   parm                    kpjba
238700130108     c                   parm                    TIBS02DS
238800130108     c                   IF        T02err = *blanks
238900130108     c                   eval      dRPFfatgio = T02uni
239000130108     c                   ENDIF
239100130116     c                   ENDIF
239200060109
239300060109      * Carico tipi bolla
239400060109
239500060109     c                   Movel     'TB'          Tabcod
239600060112     c                   z-add     1             kk
239700060109     c     KeyTab2       setll     Tabel00f
239800060109     c                   Do        *hival
239900060109     c     KeyTab2       Reade     Tabel00f
240000060109
240100060109    1c                   If        %eof(Tabel00f)
240200060109     c                   leave
240300060109     c                   endif
240400060109
240500060109    1c                   If        tblflg = ' '
240600060112     c     kk            Occur     Dstb
240700060109     c                   Movel     Tbluni        Dstb
240800060112     c                   Movel     Tblkey        Tbo(kk)
240900060112     c                   add       1             kk
241000060109    2c                   endif
241100060109
241200060109    1c                   Enddo
241300060110
241400060110      * Carico le province dei codici tassazione
241500060110
241600060110     c                   Movel     'CT'          Tabcod
241700060112     c                   z-add     1             kk
241800060110     c     KeyTab2       setll     Tabel00f
241900060110     c                   Do        *hival
242000060110     c     KeyTab2       Reade     Tabel00f
242100060110
242200060110    1c                   If        %eof(Tabel00f)
242300060110     c                   leave
242400060110     c                   endif
242500060110
242600060110    1c                   If        tblflg = ' '
242700060110     c                   Movel     Tbluni        Dsct
242800060112     c                   Movel     Tblkey        Cts(kk)
242900060112     c                   Movel     §ctprv        Cpr(kk)
243000060112     c                   add       1             kk
243100060110    2c                   endif
243200060110
243300060110    1c                   Enddo
243400060110
243500060110      * carico cod.bolla che prevedono la tassazione di una sola varia
243600060110
243700060110     c                   Movel     '3A'          Tabcod
243800060112     c                   z-add     1             kk
243900060110     c     KeyTab2       setll     Tabel00f
244000060110     c                   Do        *hival
244100060110     c     KeyTab2       Reade     Tabel00f
244200060110
244300060110    1c                   If        %eof(Tabel00f)
244400060110     c                   leave
244500060110     c                   endif
244600060110
244700060110    1c                   If        tblflg = ' '
244800060110     c                   Movel     Tbluni        Ds3a
244900060110
245000060110     c                   If        §3asva <> *blanks
245100060112     c                   Movel     Tblkey        Cbo(kk)
245200060112     c                   Movel     §3asva        Cbv(kk)
245300060112     c                   add       1             kk
245400060110     c                   endif
245500060110
245600060110    2c                   endif
245700060110
245800060110    1c                   Enddo
245900060109
246000060119      * Ricerco il limite standard dell'importo da assicurare
246100060119     c                   Clear                   Tibs02ds
246200060119     c                   Clear                   Dlia
246300060119     c                   Eval      T02Mod = 'C'
246400060119     c                   Eval      T02Sif = Knsif
246500060119     c                   Eval      T02Cod = 'LIA'
246600060119     c                   Movel     '001'         T02Ke1
246700060119     c                   Call      'TIBS02R'
246800060119     c                   Parm                    Kpjba
246900060119     c                   Parm                    Tibs02ds
247000060119     c                   If        T02Err = *Blanks
247100060119     c                   Movel     T02Uni        Dlia
247200060119     c                   EndIf
247300060119
247400130108      /free
247500130108       //?Per Fatturato giornaliero
247600130108       //?devo controllare la data DAL con la data ultima spedizione fatturata
247700130108       //?se data DAL minore/uguale devo impostare come data DAL, per fatturato giornaliero,
247800130108       //?la data ultima spedizione fatturata + 1 gg
247900130108       //?come data AL va bene quella passata dal pgm di lancio
248000130116       IF  not wDaMenu and not wDASB48;
248100130116         IF  D44dsd <= §RPFfatdsp;
248200130116           wDataISO = %date(§RPFfatdsp);
248300130116           wDataISO = wDataISO + %days(1);
248400130116           wDataDa  = %dec(wDataISO);
248500130116         ELSE;
248600130116           wDataDa  = D44dsd;
248700130116         ENDIF;
248800130116         wDataAl  = D44dsa;
248900130116       ENDIF;
249000141211
249100141211       //?Carico part.consegna con abilitazione PinCode
249200141211         TABcod = '7R';
249300141211         clear kk;
249400141211         setll (Codut:TABcod) TABEL00F;
249500141211         reade (Codut:TABcod) TABEL00F;
249600141211         DOW  not %eof(TABEL00F);
249700141211           IF  TBLflg = *blanks;
249800141211             ds7R = TBLuni;
249900141211             IF  §7Rpincode = 'S';
250000141211               kk += 1;
250100141211               skGMA(kk) = TBLkey;
250200141211             ENDIF;
250300141211           ENDIF;
250400141211           reade (Codut:TABcod) TABEL00F;
250500141211         ENDDO;
250600141211
250700130108      /end-free
250800060105
250900060109     c                   endsr
251000060109      *------------------------------------------------------------------------*
251100060109      * ROUTINE INIZIALE
251200060109      *------------------------------------------------------------------------*
251300060109     c     *Inzsr        BegSr
251400060109
251500060109     c     *entry        plist
251600060109     c                   parm                    Kpjba
251700060119     c                   eval      Tnsb44ds = Kpjbu
251800060109      * Reperisco data corrente
251900060109     c                   time                    w0140
252000060109     c                   move      w0140         wData
252100060202
252200060109     c                   clear                   WLBdat
252300060109     c                   eval      G02dat = wData
252400060109     c                   call      'XSRDA8'
252500060109     c                   parm                    WLBdat
252600060109     c                   z-add     G02inv        DateU
252700060109
252800060109     c     *dtaara       define    §azute        azuteds
252900060109     c     *dtaara       define    §datiute      ddatiute
253000060109     c                   in(E)     *dtaara
253100060109     c                   If        %error  or RSUT = *blanks
253200060109     c                   CLEAR                   tibs34ds
253300060109     c                   CALL      'TIBS34R'
253400060109     c                   PARM                    tibs34ds
253500060109     c                   in        *dtaara
253600060109     c                   EndIf
253700060120
253800060206      * Se sono in ambiente buono - GAITRAGRU
253900060208     c                   If        knsif = 'FILTRA201 ' or
254000060208     c                             knsif = 'GAITRA201 '
254100121003     c                   eval      wProva = *off
254200060206     c                   Eval      wlib = 'GAITRAGRU '
254300120220     c                   Eval      wlibc= 'UNITRAGRU'
254400120313     c                   Eval      wlibw= 'GAITRAAZM'
254500060206      *  Se sono in ambiente di prova - GAITRAGRPS
254600060206     c                   Else
254700121003     c                   eval      wProva = *on
254800060206     c                   Eval      wlib = 'GAITRAGRPS'
254900121003     c                   Eval      wlibc= 'UNITRAGRP'
255000121003     c                   Eval      wlibw= 'GAITRAAZP'
255100060206     c                   EndIf
255200060120      * Se non sono in sede
255300060120     c                   If        Dutpou <> 46
255400060120
255500060120      *  titas38c
255600060120     c                   Clear                   comman
255700060120     c                   Movel(p)  cmdt(1)       comman
255800060120     c                   Eval      %Subst(comman:30:10) = wlib
255900060120     c                   Eval      comman =
256000060120     c                             %trim(comman) + '/TITAS38C)'
256100060120     c                   Call      'QCMDEXC'                            99
256200060120     c                   Parm                    comman
256300060120     c                   Parm                    lenght
256400060120     c  n99              Open      Titas38c
256500060120      *  titas02l
256600060120     c                   Clear                   comman
256700060120     c                   Movel(p)  cmdt(2)       comman
256800060120     c                   Eval      %Subst(comman:30:10) = wlib
256900060120     c                   Eval      comman =
257000060120     c                             %trim(comman) + '/TITAS02L)'
257100060120     c                   Call      'QCMDEXC'                            99
257200060120     c                   Parm                    comman
257300060120     c                   Parm                    lenght
257400060120     c  n99              Open      Titas02l
257500060120      *  tita730c
257600060120     c                   Clear                   comman
257700060120     c                   Movel(p)  cmdt(3)       comman
257800060120     c                   Eval      %Subst(comman:30:10) = wlib
257900060120     c                   Eval      comman =
258000060120     c                             %trim(comman) + '/TITA730C)'
258100060120     c                   Call      'QCMDEXC'                            99
258200060120     c                   Parm                    comman
258300060120     c                   Parm                    lenght
258400060120     c  n99              Open      Tita730c
258500060120      *  tita430c
258600060120     c                   Clear                   comman
258700060120     c                   Movel(p)  cmdt(4)       comman
258800060120     c                   Eval      %Subst(comman:30:10) = wlib
258900060120     c                   Eval      comman =
259000060120     c                             %trim(comman) + '/TITA430C)'
259100060120     c                   Call      'QCMDEXC'                            99
259200060120     c                   Parm                    comman
259300060120     c                   Parm                    lenght
259400060120     c  n99              Open      Tita430c
259500060120      *  tncsb03l
259600060120     c                   Clear                   comman
259700060120     c                   Movel(p)  cmdt(5)       comman
259800060120     c                   Eval      %Subst(comman:30:10) = wlib
259900060120     c                   Eval      comman =
260000060120     c                             %trim(comman) + '/TNCSB03L)'
260100060120     c                   Call      'QCMDEXC'                            99
260200060120     c                   Parm                    comman
260300060120     c                   Parm                    lenght
260400060120     c  n99              Open      Tncsb03l
260500060120      *  Fiar531c
260600060120     c                   Clear                   comman
260700060120     c                   Movel(p)  cmdt(6)       comman
260800060120     c                   Eval      %Subst(comman:30:10) = wlib
260900060120     c                   Eval      comman =
261000060120     c                             %trim(comman) + '/FIAR531C)'
261100060120     c                   Call      'QCMDEXC'                            99
261200060120     c                   Parm                    comman
261300060120     c                   Parm                    lenght
261400060120     c  n99              Open      Fiar531c
261500060120      *  titaa30c
261600060120     c                   Clear                   comman
261700060120     c                   Movel(p)  cmdt(7)       comman
261800060120     c                   Eval      %Subst(comman:30:10) = wlib
261900060120     c                   Eval      comman =
262000060120     c                             %trim(comman) + '/TITAA30C)'
262100060120     c                   Call      'QCMDEXC'                            99
262200060120     c                   Parm                    comman
262300060120     c                   Parm                    lenght
262400060120     c  n99              Open      Titaa30c
262500060123
262600060123      * se sono in sede faccio solo open dei file
262700060123
262800060123     c                   else
262900060123
263000060123     c                   Open      Titas38c
263100060123     c                   Open      Titas02l
263200060123     c                   Open      Tita730c
263300060123     c                   Open      Tita430c
263400060123     c                   Open      Tncsb03l
263500060123     c                   Open      Fiar531c
263600060123     c                   Open      Titaa30c
263700060123
263800060123     c                   EndIf
263900120306
264000120306      *?Controllo se è lancio da menù
264100120308     c                   eval      wDaMenu = (D44gio = 'M')
264200120306
264300120306      *?Controllo se è lancio da TNSB48R Gestione Manca Tariffa Tasto F5-Ritassa
264400120306     c                   eval      wDaSB48 = (D44SB48 = 'S')
264500120228      *
264600120228      * file di lavoro wfmtc se sono in filiale duplico in QTEMP
264700120306      *?e se NON richiamato da TNSB48
264800120228     c                   If        1 = 1
264900120306     c                             and not wDaSB48
265000120228     c                   clear                   comman
265100120228     c                   movel(p)  cmdw(1)       comman
265200120313     c                   Eval      %Subst(comman:15:9) = wlibw
265300120228     c                   call      'QCMDEXC'
265400120228     c                   parm                    comman
265500120228     c                   parm                    lenght
265600120228
265700120228     c                   clear                   comman
265800120228     c                   movel(p)  cmdw(2)       comman
265900120313     c                   Eval      %Subst(comman:34:9) = wlibw
266000120228     c                   call      'QCMDEXC'
266100120228     c                   parm                    comman
266200120228     c                   parm                    lenght
266300120228
266400120228     c                   clear                   comman
266500120228     c                   movel(p)  cmdw(3)       comman
266600120228     c                   call      'QCMDEXC'
266700120228     c                   parm                    comman
266800120228     c                   parm                    lenght
266900120228
267000120228     c                   endif
267100120228      * apro il file di comodo per la stampa
267200120306      *?se NON richiamato da TNSB48
267300120306     c                   IF        not wDaSB48
267400120228     c                   open      wfmtc00f
267500120306     c                   ENDIF
267600120228
267700120222      *?Se è lancio da menù devo duplicare il file in QTEMP VUOTO
267800120222     c                   IF        wDaMenu
267900120222     c                   clear                   comman
268000120222     c                   movel(p)  cmdc(4)       comman
268100120308     c                   Eval      %Subst(comman:34:9) = wlibc
268200120222     c                   call      'QCMDEXC'
268300120222     c                   parm                    comman
268400120222     c                   parm                    lenght
268500120222      *?Se è schedulato devo copiare il file in QTEMP PIENO
268600120222     c                   ELSE
268700120306      *?se NON richiamato da TNSB48
268800120306     c                   IF        not wDaSB48
268900120222      *?Copio il file TFMTC00F in QTEMP
269000120217     c                   clear                   comman
269100120217     c                   movel(p)  cmdc(1)       comman
269200120217     c                   Eval      %Subst(comman:15:9) = wlibc
269300120217     c                   call      'QCMDEXC'
269400120217     c                   parm                    comman
269500120217     c                   parm                    lenght
269600120306     c                   ENDIF
269700120222     c                   ENDIF
269800120222
269900120217      *?Duplico TFMTC01L in QTEMP
270000120306      *?se NON richiamato da TNSB48
270100120306     c                   IF        not wDaSB48
270200120217     c                   clear                   comman
270300120217     c                   movel(p)  cmdc(2)       comman
270400120217     c                   Eval      %Subst(comman:34:9) = wlibc
270500120217     c                   call      'QCMDEXC'
270600120217     c                   parm                    comman
270700120217     c                   parm                    lenght
270800120306     c                   ENDIF
270900120306      *?Apro TFMTC01L
271000120217     c                   open      TFMTC01L
271100120217
271200120217      /free
271300120217         //?Calcolo oggi - 60 gg.
271400120221           dataiso = %date(dateu);
271500120221           dataiso = dataiso - %days(60);
271600120221           wOggi_60 = %dec(dataiso);
271700120217      /end-free
271800120217
271900060202      *
272000060105      *
272100060105      *------------------------------------------------------------------------*
272200060105      * KLIST
272300060105      *------------------------------------------------------------------------*
272400060105
272500060105      *---- File Cnaco00f
272600060105
272700060105     c     KeyAco        Klist
272800060105     c                   Kfld                    Codut
272900060105     c                   Kfld                    DutKci
273000060105     c                   Kfld                    TasKsc
273100060105
273200060105      *---- File Titas38c
273300060105
273400060111     c     KeyTas38      Klist
273500060105     c                   Kfld                    KTasAas
273600060105     c                   Kfld                    KTasMgs
273700060113
273800060111      *---- File Titas02l
273900060111
274000060111     c     KeyTas02      Klist
274100060111     c                   Kfld                    KTasksc
274200060113
274300060113      *---- File Tita730C
274400060113
274500060113     c     KeyTa730      Klist
274600060113     c                   Kfld                    Tasaas
274700060113     c                   Kfld                    Taslnp
274800060113     c                   Kfld                    Tasnrs
274900060113     c                   Kfld                    Tasnsp
275000060113     c                   Kfld                    Tastbl
275100060119
275200060119      *---- File Tita430C
275300060119
275400060119     c     KeyTa430      Klist
275500060119     c                   Kfld                    Tasaas
275600060119     c                   Kfld                    Taslnp
275700060119     c                   Kfld                    Tasnrs
275800060119     c                   Kfld                    Tasnsp
275900060119     c                   Kfld                    Keytrc
276000060113
276100060113      *---- File Tncsb03l
276200060113
276300060113     c     KeyTncsb      Klist
276400060113     c                   Kfld                    Tasaas
276500060113     c                   Kfld                    Taslnp
276600060113     c                   Kfld                    Tasnrs
276700060113     c                   Kfld                    Tasnsp
276800060113
276900060113      *---- File Fiar531c
277000060113
277100060113     c     keyFiar5      Klist
277200060113     c                   Kfld                    TasAas
277300060113     c                   Kfld                    TasLnp
277400060113     c                   Kfld                    TasNrs
277500060113     c                   Kfld                    TasNsp
277600060113     c                   Kfld                    kAr5Trd
277700060113
277800060113      *---- File Fnevb00f
277900060113
278000060113     c     KeyEvb        Klist
278100060113     c                   Kfld                    Tasaas
278200060113     c                   Kfld                    Taslnp
278300060113     c                   Kfld                    Tasnrs
278400060113     c                   Kfld                    Tasnsp
278500060113     c                   Kfld                    Wcev
278600060109
278700060109      *---- File Tabel00f
278800060109
278900060109     c     KeyTab        Klist
279000060109     c                   Kfld                    Codut
279100060109     c                   Kfld                    Tabcod
279200060109     c                   Kfld                    Tabkey
279300060109
279400060109     c     KeyTab2       Klist
279500060109     c                   Kfld                    Codut
279600060109     c                   Kfld                    Tabcod
279700060105      *
279800060105     c                   EndSr
279900960924**   ERR
280000060120Manca Anagrafica Cliente
280100060120Anagrafica Cliente Annullata
280200060120Manca sottoconto Intest.Fattura
280300060120Sottoconto Intest.Fatt. Annullato
280400060117Tariffa valida solo in ARRIVO
280500060120Manca Tariffa
280600060120Imp.Maggiore del limite Fatturabile
280700060120Tipo fattura errato
280800060117Tariffa valida solo in PARTENZA
280900060117Tariffa BLOCCATA
281000060120Imp. d'assicurare maggiore del limite
281100060120Manca codice cliente
281200120322Peso/Collo superiore 5 volte media del cliente
281300120326Imponibile maggiore di EUR xxxxxxxxxx,xxx
281400120907Imponibile a ZERO
281500050711** cmd ovrprtf
281600050712OVRPRTF PRTF1982 OUTQ(xxxxxxxxxx) USRDTA('MancaTarif') SAVE(*YES) HOLD(*YES)       1
281700060120** cmdt ovrdbf
281800060120OVRDBF FILE(TITAS38C) TOFILE(
281900060120OVRDBF FILE(TITAS02L) TOFILE(
282000060120OVRDBF FILE(TITA730C) TOFILE(
282100060120OVRDBF FILE(TITA430C) TOFILE(
282200060120OVRDBF FILE(TNCSB03L) TOFILE(
282300060120OVRDBF FILE(FIAR531C) TOFILE(
282400060120OVRDBF FILE(TITAA30C) TOFILE(
282500060120** cmdc crtdupobj
282600120217CPYF FROMFILE(         /TFMTC00F) TOFILE(QTEMP/TFMTC00F) CRTFILE(*YES)
282700120217CRTDUPOBJ OBJ(TFMTC01L) FROMLIB(          ) OBJTYPE(*FILE) TOLIB(QTEMP)
282800121003CPYF FROMFILE(QTEMP/TFMTC00F) TOFILE(         /TFMTC00F) MBROPT(*ADD)
282900120222CRTDUPOBJ OBJ(TFMTC00F) FROMLIB(          ) OBJTYPE(*FILE) TOLIB(QTEMP)
283000120224DLTF FILE(QTEMP/TFMTC*)
283100120228** cmdw crtdupobj
283200120228CPYF FROMFILE(         /WFMTC00F) TOFILE(QTEMP/WFMTC00F) FROMMBR(WFMTC00F) CRTFILE(*YES)
283300120228CRTDUPOBJ OBJ(WFMTC01L) FROMLIB(          ) OBJTYPE(*FILE) TOLIB(QTEMP)
283400120228CLRPFM FILE(QTEMP/WFMTC00F)
283500120229DLTF FILE(QTEMP/WFMTC*)
283600120402CPYF FROMFILE(QTEMP/WFMTC00F) TOFILE(GAITRAAZM/WFMTC00F) TOMBR(          ) MBROPT(*REPLACE)
