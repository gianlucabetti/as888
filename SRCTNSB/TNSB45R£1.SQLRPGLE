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
003600060105
003700060105      *------------------------------------------------------------------------*
003800060105      *   C A M P I   D I   L A V O R O
003900060105      *------------------------------------------------------------------------*
004000060105     d dateu           s              8  0
004100060105     d w0140           s             14  0
004200060105     d wdata           s              8  0
004300060111     d com04           s              4  0
004400060125     d monovaria       s              2
004500060111     d Yy              s              3  0
004600060112     d Kk              s              3  0
004700060117     d wunim           s             10  1
004800060117     d wunis           s             10  1
004900060117     d wunimt          s             11  1
005000060111     d Rec_ok          s              1                                         ' '=ok 'N'=No ok
005100060111     d TasPoKsc        s              3
005200060202     d SavPo           s              3  0
005300060202     d SavCar          s              3  0
005400060116     d Tipo_tar        s              1
005500060119     d Wtibs69r        s              1
005600060119     d Wtnsf20r        s              1
005700151001     d Wtnsf22r        s              1
005800060105     d codut           s              1  0 Inz(1)
005900120217     d wOggi_60        s              8  0
006000060111
006100060120     d wlib            s             10
006200061012     d wlibc           s              9
006300120313     d wlibw           s              9
006400120402     d comman          s             91
006500120402     d lenght          s             15  5 inz(91)
006600060120
006700060113     d w_dtas          s                   like(dtas)
006800060109     d Tabcod          s                   like(Tblcod)
006900060109     d Tabkey          s                   like(Tblkey)
007000060109     d KtasAAS         s                   like(Tasaas)
007100060109     d KtasMGS         s                   like(Tasmgs)
007200060109     d KtasKSC         s                   like(Tasksc)
007300060119     d KeyTrc          s                   like(Ta4trc)
007400060112     d SavKSC          s                   like(Tasksc)
007500060112     d SavRAG          s                   like(Mtcrag)
007600060112     d SavBAP          s                   like(Mtcbap)
007700060112     d SavCas          s                   like(Mtccas)
007800060112     d SavVca          s                   like(Mtcvca)
007900060112     d SavRma          s                   like(MtcRma)
008000060119     d SavPro          s                   like(MtcPro)
008100060123     d SavAge          s                   like(MtcAge)
008200060119     d §Tasrma         s                   like(MtcRma)
008300060112     d AnaERR          s                   like(Mtcerr)
008400060113     d kAr5Trd         s                   Like(Ar5Trd)
008500060113     d Wcev            s                   like(evbcev) inz('RIC')
008600090327     d StpNcl          s                   like(TasNcl)
008700120220     d wMTCtrc         s                   like(MTCtrc)
008800120307     d wMTCerr         s                   like(MTCerr)
008900120322     d wMTCesc         s                   like(MTCesc)
009000120322     d wMTCimp         s                   like(MTCimp)
009100120322     d wMTCias         s                   like(MTCias)
009200120322     d wMTCvas         s                   like(MTCvas)
009300120322     d wMTCass         s                   like(MTCass)
009400120322     d wMTCinm         s                   like(MTCinm)
009500120217
009600120222     d wDaMenu         s               n   inz(*off)
009700120224     d wDaSB48         s               n   inz(*off)
009800120220     d wEndMtc         s               n   inz(*off)
009900121003     d wProva          s               n   inz(*off)
010000120220     d wRcdMtc         s               n   inz(*off)
010100120224     d wSB48           s               n   inz(*off)
010200120413     d wVisto          s               n   inz(*off)
010300130108
010400130108     d wDataISO        s               d   datfmt(*iso)
010500130108     d wDataAl         s              8s 0
010600130108     d wDataDa         s              8s 0
010700130109     d wFIL            s              3s 0
010800130109     d wTassa          s               n   inz(*off)
010900130109     d wManca          s               n   inz(*off)
011000120307
011100120307       // -?Stringa SQL da eseguire
011200120307     d wSQL            s           2048    Varying        inz
011300060105
011400060105      *------------------------------------------------------------------------*
011500060105      *   S C H I E R E
011600060105      *------------------------------------------------------------------------*
011700060105
011800060110      * Errori manca tariffa
011900120907     d err             s             49    dim(15) ctdata perrcd(1)
012000060110      * Ovrprtf
012100060110     d cmd             s             80    dim(1) ctdata perrcd(1)
012200060120      * Ovrdbf
012300060126     d cmdt            s             60    dim(7) ctdata perrcd(1)
012400060120      * Crtdupobj
012500120224     d cmdc            s             90    dim(5) ctdata perrcd(1)              Cpyf Crtdupobj
012600120402     d cmdw            s             91    dim(5) ctdata perrcd(1)              Cpyf Crtdupobj
012700060110
012800060105      * P.O. da elaborare
012900060111     d Skpo            s              3    dim(250) inz(*Zeros)
013000060110      * Tipi bolla non soggette a tassazione
013100060110     d tbo             s              2    dim(30)
013200060110      * Codici tassazione
013300060110     d cts             s              2    dim(600)
013400060110      * Province legate ai Codici tassazione
013500060110     d cpr             s              2    dim(600)
013600060110      * Codici bolla che prevedono la tassazione di una varia fissa
013700060110     d cbo             s              2    dim(50)
013800060110      * Sigla varia legata a codici bolla CBO
013900060110     d cbv             s              1    dim(50)
014000060113      * Copia di DtasV
014100060113     d  w_tsv          s              1    dim(20)
014200060113     d  w_tva          s             11  3 dim(20)
014300060113     d  w_tev          s              1    dim(20)
014400141211
014500141211       //?Sk part.consegna con abilitazione PIN Code
014600141211     d skGMA           s              2a   dim(10)
014700060116
014800060116     d dataiso         s               d   datfmt(*iso)
014900060113
015000060105
015100060105      *------------------------------------------------------------------------*
015200060105      *   D S   I N T E R N E / E S T E R N E
015300060105      *------------------------------------------------------------------------*
015400060105
015500060116     d                 ds
015600060116     d  wdataf                 1      8  0
015700060116     d  waaf                   1      4  0
015800060116     d  wmmf                   5      6  0
015900060116     d  waammf                 1      6  0
016000060116     d  wggf                   7      8  0
016100060116     d                 ds
016200060116     d  waammi                 1      6  0
016300060116     d  waai                   1      4  0
016400060116     d  wmmi                   5      6  0
016500060116
016600060105     d                 ds
016700060105     d  TasDsp                 1      8  0
016800060105     d  TasAas                 1      4  0
016900060105     d  TasMgs                 5      8  0
017000060105
017100060105     d wlbdat          ds                  Inz
017200060105     d  g02dat                 1      8  0
017300060105     d  g02inv                 9     16  0
017400060105     d  g02err                17     17
017500060105     d  g02tgi                18     22  0
017600060119
017700060119      *
017800060119     d Paramt          ds
017900060119     d  parca                256    256
018000060105
018100060105     d Tnsb44ds      e ds                  inz
018200060105
018300060105     d Trul06ds      e ds
018400060105     d  l1                     1     90  0 Dim(30)
018500060105     d AzuteDs       e ds                  ExtName(AzUte00F)
018600060105     d DDatiUte      e ds
018700060105     d Tibs34Ds      e ds
018800060105     d Tibs02ds      e ds
018900060116     d Tise70ds      e ds
019000060119     d Trul31Ds      e ds
019100060119
019200060119     d Tibs69ds      e ds                  inz
019300060119     d DS_cnaco      e ds                  extname(Cnaco00f)
019400060119     d DS_cnind      e ds                  extname(Cnind00f)
019500060119     d DS_cnclp      e ds                  extname(Cnclp00f)
019600060119     d DS_fncls      e ds                  extname(Fncls00f)
019700120220
019800120220      // File TFMTC x sql
019900120220     d TFMTCDS       e ds                  extname(TFMTC00F)
020000060119
020100060119      * Ds x controllo limite importo da assicurare
020200060119     d Trul22Ds      e ds
020300060119      * Ds x tabella LIA
020400060119     d Dlia          e ds
020500060119
020600060113
020700060113     d dAr5Ban       e ds
020800060113     d dAr5Bnb       e ds
020900151127
021000151127      // - Rcd GEN file FIAR5
021100151127     d dAR5gen       e ds                  inz
021200060113
021300060109     d Dged          e ds
021400060109     d Dgei          e ds
021500060109     d Dstb          e ds                  occurs(30) inz
021600060110     d Dsct          e ds
021700060110     d Ds3a          e ds
021800060113     d Dscc          e ds
021900120328     d dIMP          e ds
022000130109
022100130109      // - Tabella 05 = Codici Area
022200130109     d ds05          e ds                  inz
022300130109      // - Tabella 17 = Direzioni commerciali
022400130109     d ds17          e ds                  inz
022500060202
022600060202     d Og141         e ds
022700070518     d Og143         e ds
022800060202     d Og144         e ds
022900060113
023000110701      ** DS del tasflo del file TITAS00F
023100110701     d DTASFLO       E DS
023200110701
023300060113     d Dtas          e ds                  prefix(§)
023400060926      *
023500060926     d DTAS01        E DS
023600130108
023700130108       //?Ds tabella RPF (data ultima spedizione fatturata)
023800130108     d dRPFfatgio    e ds
023900141211
024000141211       //?Ds tabella 7R - part.consegna
024100141211     d ds7R          e ds
024200130108
024300060113      *
024400060113     d Dtasv         e ds                  prefix(§)
024500060113     d   Tsv                   1     20    dim(20)                              Dtasv: Sigla Varia
024600060113     d   Tva                  21    140P 3 dim(20)                              Dtasv: Imp. Varia
024700060113     d   Tev                 141    160    dim(20)                              Dtasv: Esenz. Varia
024800060113      *
024900151001     d Dtaspes       e ds                  inz
025000060105
025100060105     d Kpjba         e ds
025200120217
025300120217      //---------------------------------------------------------------
025400120217      //?Definizione key-list.
025500120217      //---------------------------------------------------------------
025600120217
025700120217       // - File TFMTC01L
025800120220     d k06tfmtc      e ds                  extname(TFMTC01L:*key)
025900120217     d                                     prefix(k_)
026000120220
026100120220       // - File TITAS02L
026200120220     d k07titas      e ds                  extname(TITAS02L:*key)
026300120220     d                                     prefix(k_)
026400060105
026500060105      *------------------------------------------------------------------------*
026600060105      *   I N D I C A T O R I
026700060105      *------------------------------------------------------------------------*
026800060203
026900060203      * 05    Sede
027000060203      * 10    Bollo monovaria
027100060203      * 20    Stampa Imponibile
027200060203
027300060203      * 30    Errore generico
027400060203      * 31    Errore generico
027500060203      * 32    Errore generico
027600060203      * 51    Errore generico
027700060105      * 90    Errore generico
027800060203      * 99    Errore generico
027900060105
028000060105      *------------------------------------------------------------------------*
028100060105      * CICLO PRINCIPALE
028200060105      *------------------------------------------------------------------------*
028300060105
028400060110      * carico le tabelle ed imposto i punti operativi da elaborare
028500060110     c                   exsr      Sr_imposta
028600120217
028700120220      /free
028800120309
028900120309         exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
029000120309
029100120402       //?Pulizia dei dati da rielaborare e rielaborazione dei dati rimasti
029200120416       //?sul file TFMTC00F  (in caso di lancio schedulato SB47)
029300130108       //?pulizia anche dei dati relativi al fatturato giornaliero
029400120222       //?Se non è elaborazione da Menù
029500120222         IF  not wDaMenu;
029600120224         //?Se non è elaborazione da SB48
029700120224           IF  not wDaSB48;
029800120222         //?Pulisco (dal file in QTEMP) i dati che verranno rielaborati
029900120222         //?e le segnalazioni ormai obsolete (+ vecchie di 60 gg.)
030000130108         //?Pulisco anche i dati relativi al Fatturato Giornaliero
030100120224             exsr Pulisci_File;
030200120222         //?Rielaboro quello rimasto nel file in QTEMP
030300120306             exsr Rielabora_File;
030400120306             wRcdMtc = *off;
030500120306           ENDIF;
030600120222         ENDIF;
030700120402
030800120402       //?Rielaborazione dei dati presenti su TFMTC00F
030900120306       //?Se è elaborazione da SB48
031000120306         IF  wDaSB48;
031100120307         //?Preparo stringa SQL
031200120307           exsr PrepSql;
031300120306         //?Elaboro il file TFMTC con le richieste fatte a video
031400120306           exsr Elabora_TFMTC;
031500120306           wRcdMtc = *off;
031600120306         ENDIF;
031700120220      /end-free
031800120217
031900120416      *?Elaborazione della bolle da TITAS (lancio schedulato o stampa da menù)
032000120306      *?Se non è elaborazione da SB48
032100120306     c                   IF        not wDaSB48
032200060110     c                   exsr      Sr_Elabora
032300120306     c                   ENDIF
032400120402
032500120402      * stampa
032600120402      *?SOLO se è da menu
032700120416 NEW c                   IF        wDaMenu = *on
032800060206     c                   exsr      Sr_Stampa
032900120220     c                   ENDIF
033000120402
033100120220      /free
033200120402       //?Salvataggio del membro nel file WFMTC00F
033300120402       //?SOLO se elaborazione mensile/settimanale e NON da SB48
033400120402 NEW     IF  D44gio = *blanks and not wDaSB48
033500120402             and D44crf = 'S';
033600120402           exsr Salva_FileWF;
033700120402         ENDIF;
033800120402
033900120402       //?Copio TFMTC00F dalla QTEMP alla buona
034000120402       //?se non è da SB48 o da menù
034100120306 NEW     IF  not wDaMenu and not wDaSB48;
034200120221           exsr Copia_File;
034300120224         ENDIF;
034400120220      /end-free
034500060119
034600060119      * Chiudo i programmi di utilità
034700060119    1c                   IF        Wtnsf20r = *blanks
034800060119     c                   movel     'C'           §tastla                         'C'=CHIUDI
034900060119     c                   call      'TNSF20R'                            99
035000060119     c                   parm                    Kpjba
035100060119     c                   parm                    Dtas
035200060119     c                   parm                    Dtasv
035300060119    1c                   Endif
035400151001    1c                   IF        Wtnsf22r = *blanks
035500151001     c                   movel     'C'           tasptla                         'C'=CHIUDI
035600151001     c                   call      'TNSF22R'                            99
035700151001     c                   parm                    Kpjba
035800151001     c                   parm                    Dtas
035900151001     c                   parm                    Dtasv
036000151001     c                   parm                    dtaspes
036100151001    1c                   Endif
036200060119      *
036300060119     C                   IF        Wtibs69r  = 'S'
036400060119     C                   EVAL      I69TLA  = 'C'
036500060119     C                   CALL      'TIBS69R'
036600060119     C                   PARM                    TIBS69DS
036700060119     C                   ENDIF
036800060119      *
036900060117      *
037000060117     c                   seton                                        lr
037100120217
037200120217      /free
037300120217       //--------------------------------------------------------------
037400120217       //?Pulisco i dati che verranno rielaborati
037500120217       //--------------------------------------------------------------
037600120217       BEGSR Pulisci_File;
037700120217
037800120217         //?Se elaborazione giornaliera devo cancellare i dati "M"
037900120217         //?dei gg che verranno elaborati
038000120217         IF  D44gio = 'S';
038100120217           exec sql
038200120217             DELETE  from  QTEMP/TFMTC00F
038300120217             WHERE   MTCtrc = 'M' and
038400120217                     MTCdsp between :D44dsd and :D44dsa;
038500120217         ENDIF;
038600120217
038700120217         //?Se elaborazione settimanale/mensile devo cancellare tutti i dati "M"
038800120416         //?e i dati 'S' o 'V'  più vecchi di 60 gg. da oggi
038900120308         IF  D44gio = *blanks;
039000120217           exec sql
039100120217             DELETE  from  QTEMP/TFMTC00F
039200120217             WHERE   MTCtrc = 'M';
039300120217
039400120217           exec sql
039500120217             DELETE  from  QTEMP/TFMTC00F
039600120416             WHERE   MTCtrc in('S', 'V') and
039700120217                     MTCdsp < :wOggi_60;
039800120217         ENDIF;
039900130108
040000130108         //?Devo pulire anche i dati relativi al fatturato giornaliero
040100130108         //?Pulisco in base alle date calcolate all'inizio del pgm
040200130108         exec sql
040300160121           DELETE  from  TFFGC00F
040400130108           WHERE   FGCdata between :wDataDa and :wDataAl;
040500120217
040600120217       ENDSR;
040700120217
040800120217       //--------------------------------------------------------------
040900120217       //?Rielaboro i dati rimasti sul file TFMTC
041000120217       //--------------------------------------------------------------
041100120217       BEGSR Rielabora_File;
041200120217
041300120217         //?Leggo TFMTC
041400120220         wEndMtc = *off;
041500120217         exec sql
041600120217          DECLARE MTC cursor for
041700120220          SELECT  * from QTEMP/TFMTC00F
041800120217          WHERE MTCtrc = 'M'
041900120220          ORDER by MTCaas, MTCdsp, MTCksc;
042000120217
042100120217         exec sql
042200120217          OPEN MTC;
042300120217          IF SqlCode < 0;
042400120220            wEndMtc = *on;
042500120217          ENDIF;
042600120217
042700120220         DOW not wEndMtc;
042800120217           exec sql
042900120217            FETCH next from MTC into :TFMTCds;
043000120217            IF SqlCod = 100 or SqlCod < 0;
043100120220              wEndMtc = *on;
043200120217              leave;
043300120217            ENDIF;
043400120220
043500120220         //?Imposto flag che sto leggendo rcd già esistente su TFMTC
043600120220           wRcdMtc = *on;
043700120307         //?Pulisco il campo che identifica se segnalazione o manca tariffa
043800120307           clear wMTCtrc;
043900120322         //?Pulisco il campo di dell'errore
044000120322           clear wMTCerr;
044100120220
044200120220         //?Aggancio TITAS
044300120220           k_TASksc = MTCksc;
044400120220           k_TAStbl = MTCtbl;
044500120220           k_TASaas = MTCaas;
044600120220           k_TASmgs = %dec(%subst(%editc(MTCdsp:'X'):5:4):4:0);
044700120220           k_TASlnp = MTClnp;
044800120220           k_TASnrs = MTCnrs;
044900120220           k_TASnsp = MTCnsp;
045000120220           chain %kds(k07titas) TITAS02L;
045100120224
045200120224         //?Se non trovo la bolla su TITAS02L vuol dire che è stata contabilizzata
045300120224         //?quindi non c'è più il manca tariffa
045400120224           IF  not %found(TITAS02L);
045500120224
045600120224         //?Se trovo la bolla allora procedo con i controlli del manca tariffa
045700120224           ELSE;
045800120309
045900120312             com04 = %dec(%subst(%editc(TASksc:'X'):4:4):4:0);
046000120217
046100120224           //?Controllo se OK Anagrafica cliente
046200120224             exsr sr_Anagra;
046300120220
046400120224           //?Tasso se no 8888 e se no errore in Anagra
046500120322             IF com04 <> 8888 and wMTCerr = *blanks;
046600120224               exsr sr_Tassaz;
046700120224             ENDIF;
046800120224
046900120224           ENDIF;
047000120220
047100120220         //?Se arrivo a questo punto con il campo errore a blank
047200120307         //?e il campo di work che pilota se segnalazione o manca tariffa a blank
047300120307         //?(perchè se impostato ha già elaborato il rcd nella SR_SCRIVEERR)
047400120322           IF  wMTCerr = *blanks and wMTCtrc = *blanks;
047500120416         //?se ho trovato che c'è già un Visto non devo fare niente
047600120416         //?e leggo il rcd successivo
047700120416             IF  wVisto;
047800120416               iter;
047900120416             ENDIF;
048000120416         //?altrimenti vuol dire che non c'è più manca tariffa quindi cancello il rcd
048100120220             k_MTCtrc = MTCtrc;
048200120220             k_MTCaas = MTCaas;
048300120220             k_MTClnp = MTClnp;
048400120220             k_MTCnrs = MTCnrs;
048500120220             k_MTCnsp = MTCnsp;
048600120220             k_MTCtbl = MTCtbl;
048700120307             chain(e) %kds(k06tfmtc) TFMTC01L;
048800120307             IF  %error;
048900120307               iter;
049000120307             ENDIF;
049100120220             IF  %found(TFMTC01L);
049200120220               delete  TFMTC000;
049300120220             ENDIF;
049400120220           ENDIF;
049500120217
049600120217         ENDDO;
049700120217
049800120217         exec sql close MTC;
049900120217
050000120217       ENDSR;
050100120307
050200120307       //--------------------------------------------------------------
050300120307       //?Preparazione stringa SQL.
050400120307       //--------------------------------------------------------------
050500120307       BEGSR PrepSql;
050600120307
050700120307         wSQL = 'select * from TFMTC00F where MTCtrc = ''M''';
050800120307
050900120307       //?Se richiesta filiale a video
051000120307         IF D44fil > 0;
051100120307           wSQL += ' and MTCpoc = ' + %editc(D44fil:'X');
051200120307         ENDIF;
051300120307       //?Se richiesto commerciale a video
051400120307         IF D44ccm > 0;
051500120307           wSQL += ' and MTCage = ' + %editc(D44ccm:'X');
051600120307         ENDIF;
051700120307       //?Se richiesto cliente a video
051800120307         IF D44cli > 0;
051900120307           wSQL += ' and MTCksc = ' + %editc(D44cli:'X');
052000120307         ENDIF;
052100120307
052200120307          wSQL += ' ORDER by MTCaas, MTCdsp, MTCksc';
052300120307
052400120307       ENDSR;
052500120306
052600120306       //--------------------------------------------------------------
052700120306       //?Elaboro i dati presenti sul file TFMTC
052800120306       //--------------------------------------------------------------
052900120306       BEGSR Elabora_TFMTC;
053000120306
053100120306         //?Leggo TFMTC
053200120306         wEndMtc = *off;
053300120307         exec sql prepare A1 from :wSQL;
053400120307         exec sql declare MTC48 cursor for A1;
053500120307         exec sql open MTC48;
053600120306
053700120306          IF SqlCode < 0;
053800120306            wEndMtc = *on;
053900120306          ENDIF;
054000120306
054100120306         DOW not wEndMtc;
054200120306           exec sql
054300120306            FETCH next from MTC48 into :TFMTCds;
054400120306            IF SqlCod = 100 or SqlCod < 0;
054500120306              wEndMtc = *on;
054600120306              leave;
054700120306            ENDIF;
054800120306
054900120306         //?Imposto flag che sto leggendo rcd già esistente su TFMTC
055000120306           wRcdMtc = *on;
055100120307         //?Pulisco il campo che identifica se segnalazione o manca tariffa
055200120307           clear wMTCtrc;
055300120322         //?Pulisco il campo di dell'errore
055400120322           clear wMTCerr;
055500120306
055600120306         //?Aggancio TITAS
055700120306           k_TASksc = MTCksc;
055800120306           k_TAStbl = MTCtbl;
055900120306           k_TASaas = MTCaas;
056000120306           k_TASmgs = %dec(%subst(%editc(MTCdsp:'X'):5:4):4:0);
056100120306           k_TASlnp = MTClnp;
056200120306           k_TASnrs = MTCnrs;
056300120306           k_TASnsp = MTCnsp;
056400120306           chain %kds(k07titas) TITAS02L;
056500120306
056600120306         //?Se non trovo la bolla su TITAS02L vuol dire che è stata contabilizzata
056700120306         //?quindi non c'è più il manca tariffa
056800120306           IF  not %found(TITAS02L);
056900120306
057000120306         //?Se trovo la bolla allora procedo con i controlli del manca tariffa
057100120306           ELSE;
057200120312
057300120312             com04 = %dec(%subst(%editc(TASksc:'X'):4:4):4:0);
057400120306
057500120306           //?Controllo se OK Anagrafica cliente
057600120306             exsr sr_Anagra;
057700120306
057800120306           //?Tasso se no 8888 e se no errore in Anagra
057900120322             IF com04 <> 8888 and wMTCerr = *blanks;
058000120306               exsr sr_Tassaz;
058100120306             ENDIF;
058200120306
058300120306           ENDIF;
058400120306
058500120306         //?Se arrivo a questo punto con il campo errore a blank
058600120307         //?e il campo di work che pilota se segnalazione o manca tariffa a blank
058700120307         //?(perchè se impostato ha già elaborato il rcd nella SR_SCRIVEERR)
058800120322           IF  wMTCerr = *blanks and wMTCtrc = *blanks;
058900120416         //?se ho trovato che c'è già un Visto non devo fare niente
059000120416         //?e leggo il rcd successivo
059100120416             IF  wVisto;
059200120416               iter;
059300120416             ENDIF;
059400120416         //?altrimenti vuol dire che non c'è più manca tariffa quindi cancello il rcd
059500120306             k_MTCtrc = MTCtrc;
059600120306             k_MTCaas = MTCaas;
059700120306             k_MTClnp = MTClnp;
059800120306             k_MTCnrs = MTCnrs;
059900120306             k_MTCnsp = MTCnsp;
060000120306             k_MTCtbl = MTCtbl;
060100120307             chain(e) %kds(k06tfmtc) TFMTC01L;
060200120307             IF  %error;
060300120307               iter;
060400120307             ENDIF;
060500120306             IF  %found(TFMTC01L);
060600120306               delete  TFMTC000;
060700120306             ENDIF;
060800120306           ENDIF;
060900120306
061000120306         ENDDO;
061100120306
061200120306         exec sql close MTC48;
061300120306
061400120306       ENDSR;
061500120217
061600120217      /end-free
061700120217
061800060110      *------------------------------------------------------------------------*
061900060110      * ELABORAZIONE MANCA TARIFFA
062000060110      *------------------------------------------------------------------------*
062100060110     c     SR_Elabora    BegSr
062200060110
062300060110      * lettera del file TITAS in base alle richieste a video
062400060111     c                   SELECT
062500060111
062600060110      * Codice cliente  valorizzato
062700060119     c                   When      D44cli > 0
062800060112     c                   z-add     1             yy
062900060119     c                   eval      KTasKsc = D44cli
063000060111     c                   exsr      SR_Lettura02
063100060110      * Cliente non valorizzato
063200060201      * Po o area valorizzati
063300060201      * elaboro i P.O. della schiera
063400060201     c                   When      D44are > *zeros or  D44fil > *zeros
063500060111
063600060110     c                   z-add     1             yy
063700060111     c                   dow       Skpo(yy) > *zeros
063800060110      *
063900060110     c                   clear                   Ktasksc
064000060111     c                   movel     Skpo(yy)      Ktasksc
064100060111     c                   exsr      SR_Lettura02
064200060120     c                   add       1             yy
064300060110     c                   enddo
064400060111
064500060111     c                   OTHER
064600060111
064700060111      * preparo la chiave
064800060111      * Data spedizione
064900060111     c                   movel     D44Dsd        KTasAas
065000060111     c                   move      D44Dsd        KTasMgs
065100060111
065200060111     c                   exsr      SR_Lettura38
065300060110
065400060111     c                   ENDSL
065500060110
065600060110     c                   Endsr
065700060110
065800060110      *------------------------------------------------------------------------*
065900060111      * LETTURA TITAS38C
066000060110      *------------------------------------------------------------------------*
066100060111     c     SR_Lettura38  BegSr
066200060110
066300060111     c     Keytas38      setll     Titas38c
066400060110
066500060110     c                   Do        *hival
066600060110     c                   Read      Titas38c
066700060110
066800060110     c                   If        %eof(Titas38c)
066900060110     c                   leave
067000060110     c                   endif
067100130109
067200130109      /free
067300130109       //?Resetto i flag che indicano se spedizione tassata o manca tariffa
067400130109       //?da memorizzare nel fatturato giornaliero
067500130109        wTassa = *off;
067600130109        wManca = *off;
067700130109      /end-free
067800130109
067900060123      * Verifico se già contabilizzato da escludere
068000060123     c                   If        tasfic <> ' '
068100130109      /free
068200130109       //?Spedizione contabilizzata da calcolare nel fatturato giornaliero
068300130109       //?Elaboro solo le bolle con data spedizione nel range di date
068400130109       //?calcolato all'inizio del pgm
068500130110         IF  (TASaas * 10000 + TASmgs) >= wDataDa and
068600130110             (TASaas * 10000 + TASmgs) <= wDataAl;
068700130116           kk = 1;
068800130116           kk = %lookup(TAStbl : TBO);
068900130116           IF  kk > 0;
069000130116             %occur(dsTB) = kk;
069100130116             IF  §TBfcb = '1';
069200130116               exsr FatGioTas;
069300130116             ENDIF;
069400130116           ENDIF;
069500130109         ENDIF;
069600130109      /end-free
069700060123     c                   iter
069800060123     c                   endif
069900060110      * Verifico la data spedizione se maggiore della data ultima da elaborare vado a fine
070000060119     c                   If        tasdsp > D44dsa
070100060110     c                   leave
070200060110     c                   endif
070300060111      * Controllo manca tariffa
070400060111     c                   Exsr      SR_Tassa
070500130109
070600130109      /free
070700130109       //?Calcolo il fatturato giornaliero
070800130109       //?solo le bolle con data spedizione nel range di date
070900130109       //?calcolato all'inizio del pgm
071000130110         IF  (TASaas * 10000 + TASmgs) >= wDataDa and
071100130110             (TASaas * 10000 + TASmgs) <= wDataAl;
071200130109           SELECT;
071300130109         //?Spedizione prepagata
071400130109             WHEN  com04 = 8888 and TASdft > 0 and §TBfcb = '1';
071500130109              exsr FatGioTas;
071600130116         //?Spedizione con ksc 8888 non tassata conto come manca tariffa
071700130116             WHEN  com04 = 8888 and TASdft = 0 and §TBfcb = '1';
071800130116               clear dTASv;
071900130116               clear dTAS;
072000130116               §TASksc = TASksc;
072100130116               §TASdsp = TASaas * 10000 + TASmgs;
072200130116               wManca = *on;
072300130116               exsr agg_TFFGC;
072400130116         //?Spedizione in assegnato non tassata non la conteggio
072500130116             WHEN  com04 = 9999 and TASdft = 0;
072600130121         //?Spedizione in assegnato tassata
072700130121             WHEN  com04 = 9999 and TASdft > 0 and §TBfcb = '1';
072800130121              exsr FatGioTas;
072900130109         //?Manca tariffa da anagrafica
073000130109         //?Manca tariffa per errore in TNSF20R
073100130109         //?Manca tariffa per IMF
073200130109             WHEN  (Anaerr <> *blanks or §TASerr <> *blanks or wMTCimp = 'IMF')
073300130109                    and §TBfcb = '1';
073400130109               clear dTASv;
073500130109               clear dTAS;
073600130109               §TASksc = TASksc;
073700130109               §TASdsp = TASaas * 10000 + TASmgs;
073800130109               wManca = *on;
073900130109               exsr agg_TFFGC;
074000130109         //?Bolla Tassata da calcolare nel fatturato giornaliero
074100130111             WHEN  §TBfcb = '1';
074200130111               wTassa = *on;
074300130111               exsr agg_TFFGC;
074400130109           ENDSL;
074500130109         ENDIF;
074600130109      /end-free
074700060111
074800060111     c                   enddo
074900060111
075000060111     c                   endsr
075100060111
075200060111      *------------------------------------------------------------------------*
075300060111      * LETTURA TITAS02L
075400060111      *------------------------------------------------------------------------*
075500060111     c     SR_Lettura02  BegSr
075600060111
075700060111     c     Keytas02      setll     Titas02l
075800060111
075900060111     c                   Do        *hival
076000060111      * se è stato richiesto un codice cliente leggo tutti i record di quel codice
076100060119     c                   If        D44cli > 0
076200060111     c     Keytas02      Reade     Titas02l
076300060111     c                   else
076400060111      * se sto leggendo per PO
076500060111     c                   Read      Titas02l
076600060111     c                   endif
076700060111
076800060111     c                   If        %eof(Titas02l)
076900060111     c                   leave
077000060111     c                   endif
077100060111      * se sto leggendo per P.O. al cambio P.O. vado a fine lettura
077200060119     c                   If        D44cli = *zeros and tasksc <> savksc
077300060111     c                   movel     tasksc        taspoksc
077400060111     c                   if        Skpo(yy) <> taspoksc
077500060111     c                   leave
077600060111     c                   endif
077700060111     c                   endif
077800060111      * Verifico la data spedizione se non è compresa nei limiti leggo il successivo
077900060119     c                   If        tasdsp > D44dsa  or  tasdsp < D44dsd
078000060111     c                   iter
078100060111     c                   endif
078200060111      * Controllo manca tariffa
078300060111     c                   Exsr      SR_Tassa
078400060111
078500060111     c                   enddo
078600060111
078700060111     c                   endsr
078800060111
078900060111      *------------------------------------------------------------------------*
079000060111      * TASSAZIONE DELLA SINGOLA SPEDIZIONE
079100060111      *------------------------------------------------------------------------*
079200060111     c     SR_Tassa      BegSr
079300060111
079400120220     c                   clear                   tfmtc000
079500120228     c                   clear                   Wfmtc000
079600120307     c                   clear                   wMTCtrc
079700120322     c                   clear                   wMTCerr
079800060111     c                   clear                   rec_ok
079900060120      * ESCLUDO clienti  8888 e data fattura maggiore di zero o 9999
080000060111     c                   move      Tasksc        com04
080100060120     c                   If        (com04 = 8888 and tasdft > 0) or
080200060120     c                             com04 = 9999
080300060111     c                   eval      rec_ok = 'N'
080400060111     c                   endif
080500060111      * ESCLUDO le bolle da non tassare
080600060112     c                   z-add     1             kk
080700060112     c     tastbl        lookup    tbo(kk)                                30
080800060111     c                   if        %found
080900060112     c     kk            occur     dstb
081000060111      * verifico se bolla tassabile
081100060111     c                   if        §tbfcb <> '1'
081200060111     c                   eval      rec_ok = 'N'
081300060111     c                   endif
081400060111     c                   endif
081500060111
081600060112      * se record valido
081700060111     c                   If        Rec_ok = ' '
081800060112      *    controllo il cliente
081900060112     c                   exsr      Sr_anagra
082000060124      *    controllo la spedizione se diverso da 8888
082100060124     c                   if        Com04 <> 8888
082200120221      *    e se non ha avuto errori di anagrafica
082300120322     c                             and wMTCerr = *blanks
082400060113     c                   exsr      Sr_Tassaz
082500060124     c                   endif
082600060119
082700060117
082800060120     c                   endif
082900060120
083000060117     c                   Endsr
083100060112      *------------------------------------------------------------------------*
083200060112      * Controlla Anagrafica Clienti
083300060112      *------------------------------------------------------------------------*
083400060112     c     SR_anagra     BegSr
083500060112
083600060112      * Solo a cambio Cliente
083700060112
083800060112    0c                   If        TasKsc <> Savksc
083900060202      * recupero P.O. e Area
084000060202     c                   movel     TasKsc        Savpo
084100120423     c                   clear                   og143
084200060202     c     Savpo         Chain     Azorg01l
084300060202     c                   if        %found(azorg01l)
084400060202     c                   eval      Savcar = orgcar
084500120423     c                   eval      og143 = orgde3
084600060202     c                   else
084700060202     c                   clear                   savcar
084800060202     c                   endif
084900060112
085000060112     c                   movel     Tasksc        Savksc
085100060112     c                   clear                   AnaErr
085200060123      * se cliente 8888 stampo  direttamente l'errore  manca codice cliente
085300060123    1c                   If        Com04 = 8888
085400060124     c                   clear                   Savksc
085500060123      * recupero la descrizione del mittente
085600060123     c                   movel     'M'           Keytrc
085700060123     c     keyta430      chain     titaa30C
085800060123     c                   if        %found(titaa30c)
085900060123     c                   movel(P)  TaaRsc        Savrag
086000060123     c                   endif
086100060123
086200060123      * scrittura dell'errore
086300120322     c                   Movel     Err(12)       wMtcerr
086400060123     c                   clear                   Savbap
086500060123     c                   clear                   Savcas
086600060123     c                   clear                   Savvca
086700060123     c                   clear                   Savrma
086800060123     c                   clear                   Savage
086900120220     c                   eval      wMTCtrc = 'M'
087000060123     c                   Exsr      SR_ScriveErr
087100060123   x1c                   else
087200060112      * Aggancio Anagrafiche
087300060112     c     KeyAco        Chain     Cnaco000
087400060112     c     KeyAco        Chain     Cnclp000
087500060112     c     Keyaco        setll     Cnind000
087600060112      **
087700060123    2c                   If        %found(Cnaco00f)
087800060112     c                   movel     AcoRag        Savrag
087900060112     c                   Else
088000060112     c                   clear                   Savrag
088100060123    2c                   Endif
088200060123    2c                   If        %found(Cnclp00f)
088300060123     c                   movel     Clpage        Savage
088400060123     c                   Else
088500060123     c                   clear                   Savage
088600060123    2c                   Endif
088700120402
088800120402      /free
088900120402       //?Recupero la filiale dell'agente unificante
089000120412       //?e memorizzo anche l'agente unificante
089100120423          IF  §ogntw <> 'EEX' and §ogntw <> 'FED' and §ogntw <> 'DPD';
089200120402          IF  savage > 0;
089300130912            chain (SavAge) AZCMM000;
089400130912            IF  %found(AZCMM01L);
089500130912              savpo = %dec(%subst(%editc(CMMuni:'X'):1:3):3:0);
089600130912              savage = CMMuni;
089700120402            ENDIF;
089800120402          ENDIF;
089900120423          ENDIF;
090000120402      /end-free
090100120402
090200060123    2c                   SELECT
090300060112
090400060112      ** se non trovate anagrafiche
090500060112     c                   When      not %found(Cnaco00f) or not %found(Cnclp00f)
090600060112     c                             or not %equal(Cnind00f)
090700060112      * Manca anagrafica Cliente
090800060112     c                   movel     err(1)        AnaErr
090900060112
091000060112
091100060112      * Anagrafica Cleinte Annullata
091200060112
091300060123     c                   When      Acoflg = '*'
091400060112     c                   movel     Err(2)        AnaErr
091500060112
091600060112      * verifico frequenza fattura
091700060112
091800060112     c                   When      ClpTft <> 0 and ClpTft <> 1 and
091900060112     c                             ClpTft <> 9
092000060112     c                   Movel     Err(8)        AnaErr
092100060112
092200060112      * Manca Sottoconto Intestazione Fattura
092300060112
092400060123     c                   When      ClpScf = 0
092500060112     c                   movel     Err(3)        AnaErr
092600060112
092700060112      * Sottoconto Intestazione Fattura
092800060112
092900060123     c                   When      ClpScf > 0
093000060123    3c                   IF        ClpScf <> CLPKSC
093100060112
093200060112     c                   clear                   Tibs69ds
093300060112     c                   movel     clpksc        i69kac
093400060112
093500060112      * Controllo  Anagrafica
093600060112
093700060112     c                   Call      'TIBS69R'
093800060112     c                   Parm                    tibs69DS
093900060112     c                   Parm                    DS_cnaco
094000060112     c                   Parm                    DS_cnind
094100060112     c                   Parm                    DS_cnclp
094200060112     c                   Parm                    DS_fncls
094300060112
094400060112     c                   eval      wtibs69r = 'S'
094500060112
094600060112      * Manca Sottoconto Intestazione Fattura
094700060112
094800060123    4c                   if        O69Err = '1'
094900060112     c                   movel     Err(3)        AnaErr
095000060123   x4c                   else
095100060112
095200060112      * Sottoconto intestazione fattura annullato
095300060112
095400060123    5c                   If        Acoflg = '*'
095500060112     c                   movel     Err(4)        Anaerr
095600060123    5c                   endif
095700060112
095800060123    4c                   endif
095900060123    3c                   endif
096000060112
096100060112
096200060123    2c                   ENDSL
096300060123
096400060123    1c                   Endif
096500060112
096600060123    0c                   Endif
096700060112
096800060112      *
096900060112     c                   If        Anaerr <> *Blanks
097000120220     c                   eval      wMTCtrc = 'M'
097100120322     c                   Movel     Anaerr        wMtcerr
097200060112     c                   clear                   Savbap
097300060112     c                   clear                   Savcas
097400060112     c                   clear                   Savvca
097500060112     c                   clear                   Savrma
097600060112     c                   Exsr      SR_ScriveErr
097700060112     c                   endif
097800060112      *
097900060112     c                   Endsr
098000060113      *------------------------------------------------------------------------*
098100060113      * Controlla Tassazione della spediizone
098200060113      *------------------------------------------------------------------------*
098300060113     c     SR_tassaz     BegSr
098400060113
098500120322     c                   clear                   wMTCesc
098600120322     c                   clear                   wMTCimp
098700120322     c                   clear                   wMTCias
098800120322     c                   clear                   wMTCvas
098900120322     c                   clear                   wMTCass
099000120322     c                   clear                   wMTCinm
099100120322     c                   clear                   wMtcerr
099200090327     c                   clear                   stpncl
099300120416     c                   eval      wVisto = *off
099400060119
099500060119      * se richiesta stampa dell'imponibile recupero il riferimento alfanumerico
099600060119      * dal tita4 tipo record 'A'
099700060119
099800060119     c                   clear                   §tasrma
099900060119     c                   if        D44fsi = 'S'
100000060119     c                   eval      keytrc = 'A'
100100060119     c     Keyta430      chain     tita430C
100200060119     c                   if        %found(tita430c)
100300060119     c                   movel     ta4not        §tasrma
100400060119     c                   endif
100500060119     c                   endif
100600060119     c
100700060113      *  Valorizzo   D T A S V
100800060113
100900060113     c                   clear                   Dtasv
101000060113      * recupero le prime tre varie da titas
101100060113     c                   movel     Tassv1        Tsv(1)
101200060113     c                   z-add     Tasva1        Tva(1)
101300060113
101400060113     c                   movel     Tassv2        Tsv(2)
101500060113     c                   z-add     Tasva2        Tva(2)
101600060113
101700060113     c                   movel     Tassv3        Tsv(3)
101800060113     c                   z-add     Tasva3        Tva(3)
101900060113
102000060113      * se la terza varia è piena verifico in Tita7 se ce ne sono altre
102100060113     c                   If        Tassv3 <> *blanks
102200060113     c                   z-add     4             kk
102300060113     c     Keyta730      setll     Tita730c
102400060113     c     Keyta730      reade     Tita730c
102500060113     c                   Dow       not %eof(Tita730c)
102600060113     c                             and kk < 20                                  al massimo 20 varie
102700060113     c                   add       1             kk
102800060113     c                   movel     Ta7svn        Tsv(kk)
102900060113     c                   z-add     Ta7van        Tva(kk)
103000060113     c     Keyta730      reade     Tita730c
103100060113     c                   enddo
103200060113     c                   endif
103300060113      *
103400060113      * Carico esenzioni varie per DTASV
103500060113     c                   clear                   kk
103600060113     c                   movel     'CC'          Tabcod
103700060113     c                   movel(P)  'VARIE'       Tabkey
103800060113      *
103900060113     c                   Dow       kk < 20
104000060113     c                   add       1             kk
104100060113     c                   If        Tsv(kk) = *blanks
104200060113     c                   add       50            kk
104300060113     c                   else
104400060113     c                   move      Tsv(kk)       Tabkey
104500060113     c     keytab        chain     tabel00f
104600060113     c                   if        %found(tabel00f)
104700060113     c                   movel     tbluni        Dscc
104800060113     c                   movel     §cccei        Tev(kk)
104900060113     c                   endif
105000060113     c                   endif
105100060113      *
105200060113     c                   enddo
105300060113
105400060113      *  Valorizzo   D T A S
105500060113
105600060113     c                   clear                   DTAS
105700060113     c                   z-add     Tasksc        §Tasksc
105800060113     c                   z-add     Taspkf        §Taspkf
105900060113     c                   z-add     Tasvlf        §Tasvlf
106000060113     c                   z-add     Tasncl        §Tasncl
106100060113     c                   movel     Tascts        §Tascts
106200060113     c                   movel     Tasctr        §Tasctr
106300060113     c                   z-add     Tasmgs        §Tasdsp
106400060113     c                   movel     Tasaas        §Tasdsp
106401160122      * modifico per il recupero della tariffa di cartello con la data del giorno di tassazione
106500160122     c                   movel     dateu         §Tasdct
106600060113     c                   movel     Tascad        §Tascad
106700060113     c                   movel     Tasfin        §Tasfin
106800060113     c                   movel     Tascam        §Tascam
106900060113     c                   movel     Tasfap        §Tasfap
107000060113     c                   movel     Tastsp        §Tastsp
107100060113     c                   movel     Tasftc        §Tastc1
107200060113     c                   movel     Tastc2        §Tastc2
107300060113     c                   movel     Tasdiv        §Tasdiv
107400060113      *
107500060113      * aggancio il contrassegno.
107600060113      * se trovato verifico che il 1° carattere del tipo bolla corrisponda
107700060113     c                   if        tasfca <> *blanks
107800060113     c     Keytncsb      chain     Tncsb03l
107900060113     c                   if        %found(Tncsb03l)   and
108000060113     c                             Tastbl = csbtbl
108100060113     c                   z-add     Csbcas        §Tascas                         Contrassegno
108200060113     c                   movel     Csbvca        §Tasvca                         Valuta
108300060113     c                   z-add     Csbcmb        §Tascmb                         Cambio Divisa
108400061016     c****               movel     Csbtpa        §Tastic                         Tipo Incasso
108500060113     c                   if        csbfus <> *blanks
108600060113     c                   move      Csbfus        §tastic
108700060113     c                   else
108800060113     c                   move      Csbtpi        §Tastic                         Tipo Incasso
108900060113     c                   endif
109000060113     c                   endif
109100060113     c                   endif
109200060113
109300060113     c                   movel     Tasvas        §Tasvas
109400060113     c                   movel     Tasfdn        §Tasfdn                         Merce Port.Mag.
109500060113     c                   movel     Tasmct        §Tasmct                         Cod.Tassaz.Mittente
109600060113     c                   z-add     Taslnp        §Taslnp
109700060113     c                   z-add     Taslna        §Taslna
109800060113     c                   movel     Tastbl        §Tastbl
109900060113     c                   z-add     Tasias        §Tasias                         Importo assicurazio
110000060113     c                   z-add     Tasqft        §Tasqft                         Qta' Fatturabile
110100060113     c                   z-add     Taspor        §Taspor                         Trasporto
110200060113     c                   z-add     tasimv        §Tasimv                         Tot.Imponibile
110300060113
110400060113     c                   if        Tasll1 = 'S' or Tasll1 = 'C'
110500060113     c                   movel     'S'           §Tasddt                         flag si/no ddt
110600060113     c                   else
110700060113     c                   clear                   §Tasddt
110800060113     c                   endif
110900060926     c*****              movel     tasfbr        §tasflo
111000060926     c                   clear                   dtas01
111100060926     c                   movel     tasfbr        §asfbr
111200060926     c                   movel     tascca        §ascca
111300110701      * valorizzo flag email al destinatario
111400110701     c                   movel     tasflo        dtasflo
111500110701     c                   move      §floemd       §asemd
111600141211      /free
111700151127       //?Imposto se Prenotazione Ritiro Telefonico
111800151127         §TASprt = §FLOado;
111900151127
112000141211       //?Imposto se part.consegna con PinCode
112100141211         IF  TASgma <> *blanks and %lookup (TASgma:skGMA) > 0;
112200141211           §ASpin = 'S';
112300141211         ENDIF;
112400151127
112500151127       //?Imposto se Stampa Packing List
112600151127         clear dAR5gen;
112700151127         chain (TASaas:TASlnp:TASnrs:TASnsp:'GEN') FIAR531C;
112800151127         IF  %found(FIAR531C);
112900151127           dAR5gen = AR5uni;
113000151127         ENDIF;
113100151127         IF  §AR5als = 'S' or §AR5alx = 'S';
113200151127           §TASspl = 'S';
113300151127         ENDIF;
113400141211      /end-free
113500110701
113600060926     c                   eval      §tasflo = dtas01
113700060113
113800060113     c                   clear                   Kpjbu
113900060125     c                   clear                   Monovaria
114000060113
114100060113      * Le bolle con totale imponibile le controllo per controllare l'assicurazione
114200060113     c                   clear                   Parca
114300060113     c                   setoff                                       10
114400060113    1c                   If        Tasimv > *zeros
114500060113     c                   movel     'R'           §Tassva
114600060113     c                   movel     'S'           Parca
114700060113     c                   else
114800060113     c                   z-add     1             kk
114900060113     c     tascbo        lookup    cbo(kk)                                31
115000060113     c                   if        *in31
115100060113     c                   eval      §tassva=cBV(kk)
115200060125     c                   If        cbv(kk) <> ' '
115300060125     c                   eval      monovaria = 'SI'
115400060113     c                   endif
115500060125     c                   endif
115600060113
115700060113     c                   if        §tassva<>*blanks
115800060113     c                   z-add     1             kk
115900060113     c     §tassva       lookup    tsv(kk)                                10
116000060113      * se la varia c'è ed è valorizzata e imponibile = 0 devo richiamare la tassazione
116100060113     c                   If        *In10 and tva(kk) > 0 and TasImv = 0
116200060113     c                   Eval      *In10 = *Off
116300060113     c                   EndIf
116400060113     c                   Endif
116500060113
116600060113    1c                   Endif
116700060113
116800060113      * imposto peso vdl e colli rilevati
116900060113     c                   z-add     tasncp        §tasncp
117000060113     c                   z-add     taspkc        §taspkc
117100060113     c                   clear                   §taspkcN
117200060113
117300060113      * Bancali
117400060113     c                   If        %Subst(TasGva:1:1) ='B'
117500060113     c                   Clear                   dAr5Ban
117600060113     c                   Eval      kAr5Trd = 'BAN'
117700060113     c     keyFiar5      Chain     Fiar531c
117800060113     c                   If        %Found(Fiar531c)
117900060113     c                   Movel     Ar5Uni        dAr5Ban
118000060113     c                   Eval      §TasBan = §Ar5Nba + §ar5Nb2
118100060113     c                   EndIf
118200060113     c                   EndIf
118300060113      * Colli originali
118400060113     c                   If        %Subst(TasGva:1:1) ='O'
118500060113     c                   Clear                   dAr5Bnb
118600060113     c                   Eval      kAr5Trd = 'BNB'
118700060113     c     keyFiar5      Chain     Fiar531c
118800060113     c                   If        %Found(Fiar531c)
118900060113     c                   Movel     Ar5Uni        dAr5Bnb
119000060113     c                   Eval      §TasNcl = §Ar5bCor
119100090327      * mi salvo i colli da mettere nel file manca tariffa
119200090327     c                   eval      stpNcl = §ar5bCor
119300060113     c                   EndIf
119400060113     c                   EndIf
119500060113     c
119600060113      * Se bolla di reso e tassata fino all'imponibile con varia 'N' 88888888
119700060113      * prima di tassare la bolla devo tassare la varia 'N'
119800060113     c                   if        tasimv = 0 and tasfbr = 'S'
119900060116     c                   exsr      Sr_tassan
120000060113     c                   endif
120100060113
120200060113     c                   clear                   Kpjbu
120300060113
120400060113     c                   movel     paramt        Kpjbu
120500060113      * verifico se esiste la varia & con importo uguale a 8888888 e se si passo al
120600060113      * TNSF20R il flag di calcolo varia &
120700060113      *
120800060113     c                   setoff                                       51
120900060113     c                   z-add     1             kk
121000060113     c     '&'           lookup    tsv(kk)                                51
121100060113     c                   if        *in51 and tva(kk) = 88888888
121200060113     c                   eval      §tasfcaa = 'S'
121300060113     c                   clear                   tsv(kk)
121400060113     c                   clear                   tva(kk)
121500060113     c                   endif
121600060113      * verifico se devo calcolare l'addebito di lasciato avviso
121700060113      *
121800060113      * verifico se c'è evento
121900060113     c     KeyEvb        chain     Fnevb04l
122000060113
122100060113     c                   If        %found(Fnevb04l)
122200060113     c                   eval      §tasric = 'S'
122300060113     c                   endif
122400060113
122500151001      * imposto dati
122600151001     c                   eval      tasppkb = taspkb
122700060113
122800060113     c                   if        §tassva=*blanks or not *in10
122900151001     c                   Call      'TNSF22R'
123000060113     c                   Parm                    Kpjba
123100060113     c                   Parm                    Dtas
123200060113     c                   Parm                    Dtasv
123300151001     c                   parm                    dtaspes
123400060119
123500151001     c                   eval      Wtnsf22r = '1'
123600060119
123700060113     c                   endif
123800060113
123900060113      * stampo e uso il peso usato in fatturazione
124000151013     c                   If        §Tasspc <> ' '
124100060113      * mi salvo il peso da fatturare per stamparlo in fattura
124200151013     c                   eval      taspkf=§TaspkcN
124300151013     c                   endif
124400060116      * Se tassando la bolla ho errori oppure non ho errori ma devo stampare l'imponibile
124500060116      * scrivo il record sul file work
124600060116
124700060119    1c                   If        (§taserr = ' ' and d44fsi = 'S') or
124800060116     c                              §taserr <> ' '
124900060116     c                   eval      Savbap = §tasbap
125000060116     c                   eval      Savcas = §Tascas
125100060116     c                   eval      Savvca = §Tasvca
125200060116     c                   eval      Savrma = §Tasrma
125300120322     c                   movel     §Tasmsg       wMtcerr
125400060208
125500060223     c                   If        d44fsi = 'S' and §taserr = ' '
125600120322     c                   eval      wmtcimp = 'IMP'
125700060208     c                   endif
125800060208
125900060223      * valorizzo i campi dell'imponibile e della divisa
126000060123     c                   z-add     §tasimv       tasimv
126100060123     c                   eval      tasdiv = §tasdiv
126200060123
126300120220     c                   eval      wMTCtrc = 'M'
126400060116     c                   Exsr      SR_ScriveErr
126500060116
126600060119   x1c                   else
126700120413
126800120413      * controllo se imponibile maggiore del limite massimo fatturabile
126900120413      * da tabella GEI
127000120413    2c                   If        §tasimv > §geimf
127100120413     c                   eval      wMTCtrc = 'M'
127200120413     c                   eval      wmtcerr = err(7)
127300120413     c                   eval      wmtcimp = 'IMF'
127400120413      * valorizzo i campi dell'imponibile e della divisa
127500120413     c                   z-add     §tasimv       tasimv
127600120413     c                   eval      tasdiv = §tasdiv
127700120413     c                   Exsr      SR_ScriveErr
127800120413     c                   leavesr
127900120413    2c                   endif
128000120907       //?Controllo se imponibile a zero
128100120910       //?non se bolla monovaria
128200120910    2c                   If        §tasimv = 0 and monovaria <> 'SI'
128300120907     c                   eval      wMTCtrc = 'M'
128400120910     c                   eval      wmtcerr = err(15)
128500120907     c                   eval      wmtcimp = 'IMF'
128600120907      * valorizzo i campi dell'imponibile e della divisa
128700120907     c                   z-add     §tasimv       tasimv
128800120907     c                   eval      tasdiv = §tasdiv
128900120907     c                   Exsr      SR_ScriveErr
129000120907     c                   leavesr
129100120907    2c                   endif
129200060116
129300060116      * Se tassando la bolla non ho errori e non devo stampare l'imponibile
129400120328      * controllo se imponibile nei limiti indicati nelle tabelle
129500120328       //?limite imponibile bolla + peso da tabella IMP
129600120416       //?peso fino a kg. e imponibile > di xxx
129700120328    2c                   IF        TASpkf <= §IMPpeso and §tasimv > §IMPimp1
129800060116      * verifico se imponibile inserito  manualmente
129900060116     c                   if        tasimv > 0
130000120322     c                   eval      wmtcinm = 'M'
130100060116     c                   endif
130200120326      * valorizzo i campi dell'imponibile e della divisa
130300120326     c                   z-add     §tasimv       tasimv
130400120326     c                   eval      tasdiv = §tasdiv
130500120326     c                   eval      wmtcimp = 'IMP'
130600120326     c                   eval      wMTCtrc = 'M'
130700120326     c                   eval      wmtcerr = err(14)
130800120326     c                   eval      %subst(wmtcerr:28:14) =
130900120329     c                             %trim(%editc(§IMPimp1:'4'))
131000120326     c                   Exsr      SR_ScriveErr
131100120416       //?Se torna che la bolla non ha già il Visto vado alla fine dei controlli perchè il rcd
131200120416       //?è già stato scritto nella routine scriveerr
131300120416     c                   IF        not wVisto
131400120326     c                   leavesr
131500120416       //?Se torna che la bolla ha già il Visto devo andare avanti con gli altri controlli
131600120416       //?per cercare eventuali altri errori da scrivere
131700120416     c                   ELSE
131800120416     c                   clear                   wmtcimp
131900120416     c                   clear                   wMTCtrc
132000120416     c                   clear                   wMTCerr
132100120413     c                   ENDIF
132200060119    2c                   endif
132300120416
132400120328       //?limite imponibile bolla + peso da tabella IMP
132500120416       //?peso oltre kg. e imponibile > di xx
132600120328    2c                   IF        TASpkf > §IMPpeso and §tasimv > §IMPimp2
132700120328      * verifico se imponibile inserito  manualmente
132800120328     c                   if        tasimv > 0
132900120328     c                   eval      wmtcinm = 'M'
133000120328     c                   endif
133100120328      * valorizzo i campi dell'imponibile e della divisa
133200120328     c                   z-add     §tasimv       tasimv
133300120328     c                   eval      tasdiv = §tasdiv
133400120328     c                   eval      wmtcimp = 'IMP'
133500120328     c                   eval      wMTCtrc = 'M'
133600120328     c                   eval      wmtcerr = err(14)
133700120328     c                   eval      %subst(wmtcerr:28:14) =
133800120329     c                             %trim(%editc(§IMPimp2:'4'))
133900120328     c                   Exsr      SR_ScriveErr
134000120416       //?Se torna che la bolla non ha già il Visto vado alla fine dei controlli perchè il rcd
134100120416       //?è già stato scritto nella routine scriveerr
134200120416     c                   IF        not wVisto
134300120413     c                   leavesr
134400120416       //?Se torna che la bolla ha già il Visto devo andare avanti con gli altri controlli
134500120416       //?per cercare eventuali altri errori da scrivere
134600120416     c                   ELSE
134700120416     c                   clear                   wmtcimp
134800120416     c                   clear                   wMTCtrc
134900120416     c                   clear                   wMTCerr
135000120413     c                   ENDIF
135100120328    2c                   endif
135200120328
135300060117      * verifico per le tariffe a collo il peso del collo
135400130913      * e se è superiore di oltre  5 volte alla media del cliente lo segnalo
135500060117      * non viene controllato se bolla con varia "O"
135600130913      * e peso bolla minore di un kg (xchè non significativo)
135700060117     c                   movel     tasctr        Tipo_tar
135800060119    2c                   If        Tipo_tar = '1' and tasimv = 0 and
135900090327     c                             %Subst(TasGva:1:1) <>'O'
136000130913     c                             and taspkf >  1
136100060117      * richiamo il tise70r x recuperare i dati degli ultimi 3 mesi
136200060117     c                   ExSr      Sr_tise
136300060117      * Calcolo la media del cliente
136400060117     c                   Exsr      Sr_Calmed
136500060117      * se 08 acceso scrivo
136600060117     c                   If        *in08
136700120321     c                   eval      wMTCtrc = 'M'
136800120322     c                   eval      wmtcesc = '!!!'
136900120322     c                   eval      wmtcerr = err(13)
137000120322     c                   Exsr      SR_ScriveErr
137100120322     c                   leavesr
137200060117     c                   Endif
137300060119    2c                   Endif
137400060117
137500140402      * recupero il tipo bolla
137600140402      /free
137700140402        kk = %lookup(TAStbl:tbo);
137800140402        IF  kk > 0;
137900140402          %occur(dstb) = kk;
138000140402        ENDIF;
138100140402      /end-free
138200140402
138300060117      * se imponibile in bolla uguale a zero e
138400060117      * tariffa bloccata oppure franco e tariffa
138500060125      * oppure franco e tariffa valida solo in arrivo e non è bolla monovaria
138600060125      * oppure assegnato e tariffa valida solo in partenza e non è bolla monovaria
138700060125      * segnalo l'errore
138800060117
138900060119    2c                   If        Tasimv = 0 and §tasbap <> ' '
139000060125     c                   If        §tbtpo = 'F' and §tasbap = 'A'  and
139100060125     c                             monovaria <> 'SI'
139200120402     c                   eval      wMTCtrc = 'M'
139300120322     c                   movel     err(5)        wMtcerr
139400120329     c                   Exsr      SR_ScriveErr
139500120329     c                   leavesr
139600060117     c                   endif
139700060125     c                   If        §tbtpo = 'A' and §tasbap = 'P'  and
139800060125     c                             monovaria <> 'SI'
139900120402     c                   eval      wMTCtrc = 'M'
140000120322     c                   movel     err(9)        wMtcerr
140100120329     c                   Exsr      SR_ScriveErr
140200120329     c                   leavesr
140300060117     c                   endif
140400060119     c                   If        §tasbap = 'B'
140500120402     c                   eval      wMTCtrc = 'M'
140600120322     c                   movel     err(10)       wMtcerr
140700120329     c                   Exsr      SR_ScriveErr
140800120329     c                   leavesr
140900060119     c                   endif
141000060119    2c                   endif
141100120329
141200060119      * Controllo il limite dell'importo da assicurare
141300070518    2c                   If         TasIas <> *Zeros or §TasIal <> *Zeros
141400060119     c                   Clear                   Trul22Ds
141500060119     c                   Eval      I22Cbo = TasCbo
141600060119     c                   Eval      I22Tsp = TasTsp
141700060119     c                   Eval      I22Lnp = TasLnp
141800060119     c                   Eval      I22Nzm = TasNzm
141900060119     c                   Eval      I22Lna = TasLna
142000060119     c                   Eval      I22Nzd = TasNzd
142100060119     c                   Eval      I22Ksc = TasKsc
142200060119     c                   Movel     TasCtr        I22Ctr
142300070518    3c                   If        TasIas <> *Zeros
142400060119     c                   Eval      I22Imp = TasIas
142500060119     c                   Eval      I22Div = TasVas
142600070518    3c                   EndIf
142700070518    3c                   If        §TasIal <> *Zeros
142800060119     c                   Eval      I22Imp = §TasIal
142900060119     c                   Eval      I22Div = §TasDiv
143000070518    3c                   EndIf
143100060119     c                   Call      'TRUL22R'
143200060119     c                   Parm                    trul22ds
143300060119      * controllo che non sia maggiore del limite previsto
143400060119      * non forzabile
143500070518    3c                   If        O22Fx2 <> *blanks
143600120322     c                   Z-Add     I22Imp        wMtcIas
143700120322     c                   Movel     I22Div        wMtcVas
143800120322     c                   Eval      wMtcAss = 'ASS'
143900070518      * verifico se la spedizione con mandato assicurativo è :
144000070518      * - porto assegnato
144100070518      * - Euroexpress IMPORT
144200070518      * non segnalo l'errore
144300070518    4c                   If        §task88 = 'N' and §Tbtpo = 'A'
144400070523     c     taslna        chain     azorg01l
144500070518    5c                   if        %found(azorg01l) and orgfl1 = ' '            italia
144600070523     c     taslnp        chain     azorg01l
144700070518    6c                   if        %found(azorg01l) and orgfl1 = 'E'            Estero
144800070518     c                   eval      og143 = orgde3
144900070518    7c                   if        §ogntw = 'EEX'
145000120322     c                   clear                   wMtcAss
145100120322     c                   clear                   wMtcIas
145200120322     c                   clear                   wMtcVas
145300070518    7c                   EndIf
145400070518    6c                   EndIf
145500070518    5c                   EndIf
145600070518    4c                   EndIf
145700070518    3c                   EndIf
145800060119    2c                   EndIf
145900060119
146000060119      * se devo scrivere una segnalazione di imponibile anomalo  o assicurazione  o
146100060119      * errori
146200120413     c                   If        wmtcimp = 'IMP' or wMtcAss = 'ASS' or
146300120322     c                             wmtcerr <> *blanks or wmtcesc = '!!!'
146400060119
146500060116     c                   eval      Savbap = §tasbap
146600060116     c                   eval      Savcas = §Tascas
146700060116     c                   eval      Savvca = §Tasvca
146800060116     c                   eval      Savrma = §Tasrma
146900060123      * valorizzo i campi del'imponibile e della divisa
147000060123
147100060123     c                   z-add     §tasimv       tasimv
147200060123     c                   eval      tasdiv = §tasdiv
147300120220
147400120220      /free
147500120220       //?Imposto il tipo record 'S' se non è un manca tariffa
147600120220       //?il manca tariffa lo imposto a 'M' appena lo trovo
147700120220         IF  wMTCtrc = *blanks;
147800120220           wMTCtrc = 'S';
147900120220         ENDIF;
148000120220      /end-free
148100060123
148200060116     c                   Exsr      SR_ScriveErr
148300060119
148400060116     c                   endif
148500060119
148600060119     c                   endif
148700060119      *
148800060113     c                   endsr
148900060113
149000060113      *-----------------------------------------------------------
149100060113      * Tasso la Varia 'N' 88888888
149200060113      *-----------------------------------------------------------
149300060116     c     SR_Tassan     begsr
149400060113
149500060113      * verifico se c'è la varia 'N' 88888888
149600060113     c                   eval      kk = 1
149700060113     c     'N'           lookup    tsv(kk)                                32
149800060113     c                   if        *in32 and tva(kk) = 88888888
149900060113      * richiamo il programma di tassazione
150000060113      * senza passare le varie e il porto
150100060113     c                   movea     tsv           w_tsv
150200060113     c                   movea     tva           w_tva
150300060113     c                   movea     tev           w_tev
150400060113     c                   eval      w_dtas = dtas
150500110701
150600110701      * tolgo il flag dell'invio mail al destinatario nell'eventualità ci fosse
150700110701     c                   eval      dtas01 = §tasflo
150800110701     c                   clear                   §asemd
150900141211      /free
151000141211       //?Pulisco flag PinCode
151100141211         clear §ASpin;
151200141211      /end-free
151300110701     c                   eval      §tasflo = dtas01
151301160113
151302160113      * tolgo flag per calcolo  Diritto di chiamata prenotazione  ritiro e packing list
151303160113      *
151304160113     c                   clear                   §TASPRT
151305160113     c                   clear                   §TASSPL
151306160113
151400060113
151500060113     c                   clear                   dtasv
151600060113     c                   clear                   §tasdiv
151700060113     c                   clear                   §taspor
151800060113
151900060113     c                   clear                   kpjbu
152000060113     c                   call      'TNSF20R'
152100060113     c                   parm                    kpjba
152200060113     c                   parm                    dtas
152300060113     c                   parm                    dtasv
152400060113      * valorizzo la varia 'N' 88888888
152500060113     c                   if        §taserr = ' '
152600060116     c                   z-add     §tasimv       w_tva(kk)
152700060113     c                   endif
152800060113      * ripristino le ds salvate prima
152900060113     c                   movea     w_tsv         tsv
153000060113     c                   movea     w_tva         tva
153100060113     c                   movea     w_tev         tev
153200060113     c                   eval      dtas = w_dtas
153300060113     c                   endif
153400060113
153500060113     c                   endsr
153600060117      *------------------------------------------------------------------------*
153700060116      *    richiamo il tise70r x recuperare i dati
153800060117      *------------------------------------------------------------------------*
153900060116     c     Sr_Tise       BegSr
154000060116
154100060116     c                   move      Tasaas        waaf
154200060116     c                   movel     Tasmgs        wmmf
154300060116     c                   move      01            wggf
154400060116
154500060116     c     *iso          move      wdataf        dataiso
154600060116     c                   subdur    2:*m          dataiso
154700060116     c                   extrct    dataiso:*m    wmmi
154800060116     c                   extrct    dataiso:*y    waai
154900060116
155000060116     c                   clear                   TISE70DS
155100060116     c                   eval      I70Tla = 'S'
155200060116     c                   eval      I70Tle = 'ST'
155300060116     c                   eval      I70Ksc = Tasksc
155400060116     c                   eval      I70Daf = waammf
155500060116     c                   eval      I70Dai = waammi
155600060116
155700060116     c                   call      'TISE70R'
155800060116     c                   parm                    Tise70ds
155900060116
156000060116     c                   EndSr
156100060117      *------------------------------------------------------------------------*
156200060117      *    Calcolo la media del cliente
156300060117      *------------------------------------------------------------------------*
156400060119     c     Sr_Calmed     Begsr
156500060117
156600070704     c                   clear                   *in08
156700120402
156800120402       //?Il controllo lo faccio solo se raggruppamento bolla (§TBRBL) é NORMALE (N)
156900120402      /free
157000120402        kk = %lookup(TAStbl:tbo);
157100120402        IF  kk > 0;
157200120402          %occur(dstb) = kk;
157300120402          IF  §TBrbl <> 'N';
157400120402            leavesr;
157500120402          ENDIF;
157600120402        ENDIF;
157700120402      /end-free
157800120402
157900060117      * Peso unitario della media del cliente
158000060117
158100060116     c                   If        o70nco > *Zeros
158200060117     c     o70pkg        div       o70nco        wunim
158300060117      * peso unitario della spedizione
158400090327     c     TasPKF        div(h)    Tasncl        wunis
158500090324      * si è deciso di modificare il controllo del peso 10 volte superiore
158600090324      * alla media dell cliente con solo 5 volte superiore
158700090324     c**** Wunim         Mult      10            Wunimt
158800090324     c     Wunim         Mult      5             Wunimt
158900060117
159000060117     c                   eval      *in08 =(Wunis >= wunimt)
159100060117
159200070704     c                   EndIf
159300060117     c                   Endsr
159400060117
159500060112      *------------------------------------------------------------------------*
159600060112      * Scrittura file di lavoro
159700060112      *------------------------------------------------------------------------*
159800060112     c     SR_ScriveErr  BegSr
159900120220
160000120220      /free
160100120413         wVisto = *off;
160200120326       //?Se devo scrivere un manca tariffa per IMP > di §geimv prima verifico che
160300120326       //?non sia già stato vistato dall'utente, lo cerco quindi con tipo rcd V=OK da utente
160400120326         IF  wmtcimp = 'IMP' and wmtctrc = 'M';
160500120326           k_MTCtrc = 'V';
160600120413           k_MTCaas = TASaas;
160700120413           k_MTClnp = TASlnp;
160800120413           k_MTCnrs = TASnrs;
160900120413           k_MTCnsp = TASnsp;
161000120413           k_MTCtbl = TAStbl;
161100120326           chain(n) %kds(k06tfmtc) TFMTC01L;
161200120326           IF  %found(TFMTC01L);
161300120413             wVisto = *on;
161400120326             leavesr;
161500120326           ENDIF;
161600120326         ENDIF;
161700120326
161800120307       //?Se sto rielaborando un rcd di Manca tariffa già presente sul file
161900120307         IF  wRcdMtc;
162000120307           k_MTCtrc = MTCtrc;
162100120307           k_MTCaas = MTCaas;
162200120307           k_MTClnp = MTClnp;
162300120307           k_MTCnrs = MTCnrs;
162400120307           k_MTCnsp = MTCnsp;
162500120307           k_MTCtbl = MTCtbl;
162600120307           chain(e) %kds(k06tfmtc) TFMTC01L;
162700120307           IF  %error;
162800120307             leavesr;
162900120307           ENDIF;
163000120307           IF  %found(TFMTC01L);
163100120307           //?se il campo di work che pilota se segnalazione o manca tariffa è = 'S'
163200120307           //?da manca tariffa è diventato segnalazione quindi cancello il rcd di manca tariffa
163300120307             IF  wMTCtrc = 'S';
163400120307               delete  TFMTC000;
163500120307             ENDIF;
163600120307           //?se il campo di work che pilota se segnalazione o manca tariffa è = 'M'
163700120307           //?è ancora manca tariffa quindi aggiorno il rcd di manca tariffa
163800120307             IF  wMTCtrc = 'M';
163900120307               exsr MemDati;
164000120307               update  TFMTC000;
164100120307               leavesr;
164200120307             ENDIF;
164300120307           ENDIF;
164400120307         ENDIF;
164500120307         //?Se è una segnalazione verifico se non è già presente sul file
164600120307         IF  wMTCtrc = 'S';
164700120307           k_MTCtrc = wMTCtrc;
164800120308           k_MTCaas = TASaas;
164900120308           k_MTClnp = TASlnp;
165000120308           k_MTCnrs = TASnrs;
165100120308           k_MTCnsp = TASnsp;
165200120308           k_MTCtbl = TAStbl;
165300120307           chain(n) %kds(k06tfmtc) TFMTC01L;
165400120307         //?Se è già presente non faccio niente perchè non è da riscrivere
165500120307           IF  %found(TFMTC01L);
165600120307             //?Scrivo il WF
165700120307             //?se non è richiamato da TNSB48
165800120307             IF  not wDaSB48;
165900120307               exsr wrtWF;
166000120307             ENDIF;
166100120307             leavesr;
166200120307           ENDIF;
166300120307         ENDIF;
166400120307
166500120307       //?Scrivo nuovo record
166600120307         MTCtrc = wMTCtrc;
166700120307         MTCaas = TASaas;
166800120307         MTClnp = TASlnp;
166900120307         MTCnrs = TASnrs;
167000120307         MTCnsp = TASnsp;
167100120307         MTCtbl = TAStbl;
167200120307         MTCDSP = TASaas * 10000;
167300120307         MTCdsp += TASmgs;
167400120307         MTCksc = TASksc;
167500120322         clear MTCdstp;
167600120307         exsr MemDati;
167700120307         write  TFMTC000;
167800120307         //?Scrivo il WF
167900120307         //?se non è richiamato da TNSB48
168000120307         IF  not wDaSB48;
168100120307           exsr wrtWF;
168200120307         ENDIF;
168300060112
168400120220       ENDSR;
168500120220
168600120220       //--------------------------------------------------------------
168700120220       //?Memorizzo i dati su TFMTC
168800120220       //--------------------------------------------------------------
168900120220       BEGSR MemDati;
169000120220
169100120220       //?Reperisco la provincia del codice tassazione
169200120220         clear savpro;
169300120220         kk = %lookup(TAScts:cts);
169400120220         IF  kk > 0;
169500120220           savpro = cpr(kk);
169600120220         ENDIF;
169700120220
169800120220         MTCpoc = Savpo;
169900120220         MTCcar = Savcar;
170000120220         MTCrag = Savrag;
170100120220         MTCage = Savage;
170200120220
170300120220       //?verifico se il campo colli originali da stampare è valorizzato
170400120220         IF  StpNcl <> 0 and StpNcl <> Tasncl;
170500120220           MTCNcl = StpNcl;
170600120322           %Subst(wMtcEsc:1:1) = 'o';
170700120220         ELSE;
170800120220           MTCncl = Tasncl;
170900120220         ENDIF;
171000120220
171100120220         MTCpkf = Taspkf;
171200120220         MTCvlf = Tasvlf;
171300120220         MTCfap = Tasfap;
171400120220         MTClna = Taslna;
171500120220         MTCcts = Tascts;
171600120220         MTCfin = Tasfin;
171700120220         MTCpro = Savpro;
171800120220         MTCtsp = Tastsp;
171900120220         MTCctr = Tasctr;
172000120220         MTCbap = Savbap;
172100120220         MTCdiv = Tasdiv;
172200120220         MTCpor = Taspor;
172300120220         MTCimv = Tasimv;
172400120220         MTCcas = Savcas;
172500120220         MTCvca = Savvca;
172600120220         MTCrmn = Tasrmn;
172700120220         MTCrma = Savrma;
172800120322         MTCerr = wMTCerr;
172900120322         MTCesc = wMTCesc;
173000120322         MTCimp = wMTCimp;
173100120322         MTCias = wMTCias;
173200120322         MTCvas = wMTCvas;
173300120322         MTCass = wMTCass;
173400120322         MTCinm = wMTCinm;
173500120220
173600120220       ENDSR;
173700120228
173800120228       //--------------------------------------------------------------
173900120228       //?Scrivo il file WFTMC
174000120228       //--------------------------------------------------------------
174100120228       BEGSR wrtWF;
174200120329
174300120329       //?Se errore IMP ed elaborazione giornaliera non la scrivo sul WF
174400120329       //?così non viene stampata tutti i giorni
174500120329         IF  wMTCtrc = 'M' and wMTCimp = 'IMP' and d44gio = 'S';
174600120329           leavesr;
174700120329         ENDIF;
174800120228
174900120228       //?Reperisco la provincia del codice tassazione
175000120228         clear savpro;
175100120228         kk = %lookup(TAScts:cts);
175200120228         IF  kk > 0;
175300120228           savpro = cpr(kk);
175400120228         ENDIF;
175500120228
175600120228         MTCdel = dateu;
175700120228         MTCksc = TASksc;
175800120228         MTCpoc = Savpo;
175900120228         MTCcar = Savcar;
176000120228         MTCage = Savage;
176100120228         MTCrag = Savrag;
176200120228         MTCaas = TASaas;
176300120228         MTClnp = TASlnp;
176400120228         MTCnrs = TASnrs;
176500120228         MTCnsp = TASnsp;
176600120228         MTCtbl = TAStbl;
176700120228         MTCDSP = TASaas * 10000;
176800120228         MTCdsp += TASmgs;
176900120228
177000120228       //?verifico se il campo colli originali da stampare è valorizzato
177100120228         IF  StpNcl <> 0 and StpNcl <> Tasncl;
177200120228           MTCNcl = StpNcl;
177300120322           %Subst(wMtcEsc:1:1) = 'o';
177400120228         ELSE;
177500120228           MTCncl = Tasncl;
177600120228         ENDIF;
177700120228
177800120228         MTCpkf = Taspkf;
177900120228         MTCvlf = Tasvlf;
178000120228         MTCfap = Tasfap;
178100120228         MTClna = Taslna;
178200120228         MTCcts = Tascts;
178300120228         MTCfin = Tasfin;
178400120228         MTCpro = Savpro;
178500120228         MTCtsp = Tastsp;
178600120228         MTCctr = Tasctr;
178700120228         MTCbap = Savbap;
178800120228         MTCdiv = Tasdiv;
178900120228         MTCpor = Taspor;
179000120228         MTCimv = Tasimv;
179100120228         MTCcas = Savcas;
179200120228         MTCvca = Savvca;
179300120228         MTCrmn = Tasrmn;
179400120228         MTCrma = Savrma;
179500120322         MTCerr = wMTCerr;
179600120322         MTCesc = wMTCesc;
179700120322         MTCimp = wMTCimp;
179800120322         MTCias = wMTCias;
179900120322         MTCvas = wMTCvas;
180000120322         MTCass = wMTCass;
180100120322         MTCinm = wMTCinm;
180200120322
180300120228         write  WFMTC000;
180400120228
180500120228       ENDSR;
180600120220
180700120220      /end-free
180800060126
180900060126      *------------------------------------------------------------------------*
181000060126      * Stampa manca tariffa
181100060126      *------------------------------------------------------------------------*
181200060126     c     SR_Stampa     BegSr
181300060206
181400060206      * richiamo il programma di stampa
181500060206     c                   clear                   kpjbu
181600060206     c                   movel     tnsb44ds      kpjbu
181700060206     c                   Call      'TNSB46R'
181800060206     c                   Parm                    Kpjba
181900060206
182000060206     c                   Endsr
182100120220
182200120220       //--------------------------------------------------------------
182300120220       //?Copio il file dalla QTEMP alla BUONA
182400120220       //--------------------------------------------------------------
182500120220     c     Copia_File    BEGSR
182600120220
182700120220      *?Chiudo TFMTC01L in QTEMP
182800120220     c                   close     TFMTC01L
182900121003
183000121003      /free
183100121003       //?Cancello i dati presenti sul file TFMTC00F della BUONA
183200121003       //?lo faccio con SQL così anche se impegnato vado avanti lo stesso
183300121003         IF  not wProva;
183400121003           exec sql
183500121003             DELETE  from  UNITRAGRU/TFMTC00F;
183600121003         ELSE;
183700121003           exec sql
183800121003             DELETE  from  UNITRAGRP/TFMTC00F;
183900121003         ENDIF;
184000121003      /end-free
184100120220      *?Copio il file TFMTC00F dalla QTEMP alla BUONA
184200120220     c                   clear                   comman
184300120220     c                   movel(p)  cmdc(3)       comman
184400120220     c                   Eval      %Subst(comman:38:9) = wlibc
184500120220     c                   call      'QCMDEXC'
184600120220     c                   parm                    comman
184700120220     c                   parm                    lenght
184800120220
184900120220     c                   ENDSR
185000120402
185100120402       //--------------------------------------------------------------
185200120402       //?Salvo il file WFMTC00F in un nuovo membro
185300120402       //--------------------------------------------------------------
185400120402     c     Salva_FileWF  BEGSR
185500120402
185600120402      *?Chiudo WFMTC00F in QTEMP
185700120402     c                   close     WFMTC00F
185800120402      *?Salvo il membro
185900120402     c                   clear                   comman
186000120402     c                   movel(p)  cmdw(5)       comman
186100120402     c                   Eval      %Subst(comman:64:10) = D44mbr
186200120402     c                   call      'QCMDEXC'
186300120402     c                   parm                    comman
186400120402     c                   parm                    lenght
186500120402
186600120402     c                   ENDSR
186700130108
186800130108      /free
186900130108       //--------------------------------------------------------------
187000130109       //?Bolle già contabilizzate o prepagati per Fatturato Giornaliero.
187100130109       //?dati da TITAS.
187200130108       //--------------------------------------------------------------
187300130109       BEGSR  FatGioTas;
187400130108
187500130108       //?Visto che bolla già contabilizzata non devo controllare niente
187600130108       //?e nemmeno tassare, prendo i dati da TITAS
187700130108       //?valorizzando le DS DTAS e DTASV
187800130108         clear dTASv;
187900130108         TSV(1) = TASsv1;
188000130108         TVA(1) = TASva1;
188100130108         TSV(2) = TASsv2;
188200130108         TVA(2) = TASva2;
188300130108         TSV(3) = TASsv3;
188400130108         TVA(3) = TASva3;
188500130108       //?Se la terza varia è piena cerco su TITA7 eventuali altre varie
188600130109         IF  TASsv3 <> *blanks;
188700130108           kk = 4;
188800130108           setll (TASaas:TASlnp:TASnrs:TASnsp:TAStbl) TITA730C;
188900130108           reade (TASaas:TASlnp:TASnrs:TASnsp:TAStbl) TITA730C;
189000130108           DOW  not %eof(TITA730C) and kk < 20;
189100130108             kk += 1;
189200130108             TSV(kk) = TA7svn;
189300130108             TVA(kk) = TA7van;
189400130108             reade (TASaas:TASlnp:TASnrs:TASnsp:TAStbl) TITA730C;
189500130108           ENDDO;
189600130108         ENDIF;
189700130108
189800130108         clear dTAS;
189900130108         §TASksc = TASksc;
190000130108         §TASdsp = TASaas * 10000 + TASmgs;
190100130108         §TASpor = TASpor;
190200130108         §TASimv = TASimv;
190300130109         wTassa = *on;
190400130108
190500130108         exsr agg_TFFGC;
190600130108
190700130108       ENDSR;
190800130109
190900130109       //--------------------------------------------------------------
191000130109       //?Scrivo/Aggiorno fatturato giornaliero.
191100130109       //--------------------------------------------------------------
191200130109       BEGSR  agg_TFFGC;
191300130109
191400130109         wFIL = %dec(%subst(%editc(§TASksc:'X'):1:3):3:0);
191500160121         chain (§TASdsp:wFIL) TFFGC01L;
191600130109
191700130109       //?Se c'è già un rcd aggiorno con i dati variabili
191800160121         IF  %found(TFFGC01L);
191900130109           exsr imposta_TFFGC;
192000130109           update TFFGC000;
192100130109         ENDIF;
192200130109
192300130109       //?Se rcd nuovo prima imposto i dati fissi + key poi i dati variabili
192400160121         IF  not %found(TFFGC01L);
192500130109           clear TFFGC000;
192600130109           FGCDATA = §TASdsp;
192700130109           FGCFIL  = wFIL;
192800130109
192900130109         //?valorizzo area e distretto + decodifiche
193000130109           chain wFIL AZORG01L;
193100130109           IF  %found(AZORG01L);
193200130109             FGCfild = ORGdes;
193300130109             FGCdit  = ORGfl3;
193400130109             FGCare  = ORGcar;
193500130109           ENDIF;
193600130109           clear ds05;
193700130109           TABcod = '05';
193800130109           TABkey = %editc(FGCare:'X');
193900130109           chain (codut:TABcod:TABkey) TABEL00F;
194000130109           IF  %found(TABEL00F);
194100130109             ds05 = TBLuni;
194200130109           ENDIF;
194300130109           FGCared = §05des;
194400130109           clear ds17;
194500130109           TABcod = '17';
194600130109           TABkey = FGCdit;
194700130109           chain (codut:TABcod:TABkey) TABEL00F;
194800130109           IF  %found( TABEL00F );
194900130109             ds17 = TBLuni;
195000130109           ENDIF;
195100130109           FGCditd = §17des;
195200130201
195300130201           FGCdtela = dateu;
195400130109
195500130109         //?valorizzo i dati variabili
195600130109           exsr imposta_TFFGC;
195700130109           write TFFGC000;
195800130109         ENDIF;
195900130109
196000130109       ENDSR;
196100130109
196200130109       //--------------------------------------------------------------
196300130109       //?Imposto dati per il file Fatturato Giornaliero.
196400130109       //--------------------------------------------------------------
196500130109       BEGSR  imposta_TFFGC;
196600130109
196700130109       //?spedizioni tassate
196800130109         IF  wTassa;
196900130109           FGCtost += 1;
197000130109         ENDIF;
197100130109
197200130109       //?spedizioni manca tariffa
197300130109         IF  wManca;
197400130109           FGCtosm += 1;
197500130109         ENDIF;
197600130109
197700130109       //?porto e imponibile
197800130109         FGCtpor += §TASpor;
197900130109         FGCtimp += §TASimv;
198000130109
198100130109       //?varie
198200130109         kk = 1;
198300130109         FOR kk by 1 to %elem(TSV);
198400130109           SELECT;
198500130109             WHEN  TSV(kk) = '2 ';
198600130109               FGCva2 += TVA(kk);
198700130109             WHEN  TSV(kk) = '5 ';
198800130109               FGCva5 += TVA(kk);
198900130109             WHEN  TSV(kk) = '& ';
199000130109               FGCvapa += TVA(kk);
199100130109             WHEN  TSV(kk) = '* ';
199200130109               FGCvadir += TVA(kk);
199300130109             WHEN  TSV(kk) = '+ ';
199400130109               FGCvasca += TVA(kk);
199500130109             WHEN  TSV(kk) = '= ';
199600130109               FGCvarba += TVA(kk);
199700130109             WHEN  TSV(kk) = 'a ';
199800130109               FGCvaaa += TVA(kk);
199900130109             WHEN  TSV(kk) = 'A ';
200000130109               FGCvaa += TVA(kk);
200100130109             WHEN  TSV(kk) = 'b ';
200200130109               FGCvabb += TVA(kk);
200300130109             WHEN  TSV(kk) = 'B ';
200400130109               FGCvab += TVA(kk);
200500130109             WHEN  TSV(kk) = 'c ';
200600130109               FGCvacc += TVA(kk);
200700130109             WHEN  TSV(kk) = 'C ';
200800130109               FGCvac += TVA(kk);
200900130109             WHEN  TSV(kk) = 'd ';
201000130109               FGCvadd += TVA(kk);
201100130109             WHEN  TSV(kk) = 'D ';
201200130109               FGCvad += TVA(kk);
201300130109             WHEN  TSV(kk) = 'e ';
201400130109               FGCvaee += TVA(kk);
201500130109             WHEN  TSV(kk) = 'f ';
201600130109               FGCvaff += TVA(kk);
201700130109             WHEN  TSV(kk) = 'F ';
201800130109               FGCvaf += TVA(kk);
201900130109             WHEN  TSV(kk) = 'G ';
202000130109               FGCvag += TVA(kk);
202100130109             WHEN  TSV(kk) = 'h ';
202200130109               FGCvahh += TVA(kk);
202300130109             WHEN  TSV(kk) = 'H ';
202400130109               FGCvah += TVA(kk);
202500130109             WHEN  TSV(kk) = 'I ';
202600130109               FGCvai += TVA(kk);
202700130109             WHEN  TSV(kk) = 'J ';
202800130109               FGCvaj += TVA(kk);
202900130109             WHEN  TSV(kk) = 'K ';
203000130109               FGCvak += TVA(kk);
203100130109             WHEN  TSV(kk) = 'L ';
203200130109               FGCval += TVA(kk);
203300130109             WHEN  TSV(kk) = 'm ';
203400130109               FGCvamm += TVA(kk);
203500130109             WHEN  TSV(kk) = 'M ';
203600130109               FGCvam += TVA(kk);
203700130109             WHEN  TSV(kk) = 'N ';
203800130109               FGCvan += TVA(kk);
203900130109             WHEN  TSV(kk) = 'O ';
204000130109               FGCvao += TVA(kk);
204100130109             WHEN  TSV(kk) = 'P ';
204200130109               FGCvap += TVA(kk);
204300130109             WHEN  TSV(kk) = 'Q ';
204400130109               FGCvaq += TVA(kk);
204500130109             WHEN  TSV(kk) = 'R ';
204600130109               FGCvar += TVA(kk);
204700130109             WHEN  TSV(kk) = 'S ';
204800130109               FGCvas += TVA(kk);
204900130109             WHEN  TSV(kk) = 'T ';
205000130109               FGCvat += TVA(kk);
205100130109             WHEN  TSV(kk) = 'U ';
205200130109               FGCvau += TVA(kk);
205300130109             WHEN  TSV(kk) = 'W ';
205400130109               FGCvaw += TVA(kk);
205500130109             WHEN  TSV(kk) = 'X ';
205600130109               FGCvax += TVA(kk);
205700130109             WHEN  TSV(kk) = 'Y ';
205800130109               FGCvay += TVA(kk);
205900130109             WHEN  TSV(kk) = 'Z ';
206000130109               FGCvaz += TVA(kk);
206001160121             WHEN  TSV(kk) = 't ';
206002160121               FGCvapr += TVA(kk);
206003160121             WHEN  TSV(kk) = 'k ';
206004160121               FGCvapk += TVA(kk);
206100130109           ENDSL;
206200130109         ENDFOR;
206300130109
206400130109       ENDSR;
206500130108
206600130108      /end-free
206700060112
206800060109      *------------------------------------------------------------------------*
206900060109      * IMPOSTAZIONI INIZIALI
207000060109      *------------------------------------------------------------------------*
207100060109     c     SR_Imposta    BegSr
207200060109
207300060105      * Utente di sede
207400060110     c                   If        DutPou = 46
207500060105     c                   Eval      *In05 = *On
207600060110     c                   EndIf
207700060203
207800060203      * verifico se devo stampare il totale imponibile
207900060203     c                   eval      *in20 = (d44fsi = 'S')
208000060203
208100060105
208200060105     c                   SELECT
208300060105      *
208400060105      * se codice cliente valorizzato valorizzo il p.o. da elaborare con quello del cliente
208500060119     c                   When      D44cli <> *zeros
208600120416     c                   eval      skpo(1) = %subst(%editc(d44cli:'X'):1:3)
208700060105      *
208800060105      * verifico se valorizzata la sola linea valorizzo i p.o. da elaborare se stessa e la sua  £1
208900060105      *
209000060105     c                   When      D44Fil <> *zeros
209100060105     c                   clear                   TRUL31DS
209200060105     c                   eval      I31abi = 'PO'
209300060105     c                   eval      I31cpo = D44Fil
209400060105     c                   call      'TRUL31R'
209500060105     c                   parm                    KPJBA
209600060105     c                   parm                    TRUL31DS
209700060111     c                   if        O31pog > *zeros
209800060111     c                   movea     O31pog        Skpo
209900060111     c                   else
210000060119     c                   movel     D44fil        Skpo(1)
210100060111     c                   endif
210200060105
210300060105      *
210400060201      * verifico se valorizzata l'area
210500060105      *
210600060201     c                   When      D44Are <> *zeros
210700060201      * Reperimento dei P.O. dell'area
210800060105     c                   clear                   TRUL31DS
210900060201     c                   eval      I31abi = 'RA'
211000060201     c                   eval      I31car = D44Are
211100060105     c                   call      'TRUL31R'
211200060105     c                   parm                    KPJBA
211300060105     c                   parm                    TRUL31DS
211400060111     c                   if        O31pog > *zeros
211500060111     c                   movea     O31pog        Skpo
211600060111     c                   endif
211700060105
211800060109     c                   other
211900060109      * Reperimento dei P.O. dell'Azienda
212000060109     c                   clear                   TRUL31DS
212100060109     c                   eval      I31abi = 'AZ'
212200060109     c                   call      'TRUL31R'
212300060109     c                   parm                    KPJBA
212400060109     c                   parm                    TRUL31DS
212500060111     c                   if        O31pog > *zeros
212600060111     c                   movea     O31pog        Skpo
212700060111     c                   endif
212800060109
212900060111     c                   endsl
213000060109
213100060109      * Recupero la moneta di conto
213200060109     c                   clear                   Tibs02ds
213300060109     c                   clear                   Dged
213400060109
213500060109     c                   Movel     'C'           T02mod
213600060109     c                   Movel     KNSIF         T02sif
213700060109     c                   Movel     'GED'         T02cod
213800060109     c                   Movel     '1'           T02ke1
213900060109      *
214000060109     c                   Call      'TIBS02R'
214100060109     c                   Parm                    Kpjba
214200060109     c                   Parm                    Tibs02ds
214300060109      *
214400060109     c                   If        T02err = *blanks
214500060109     c                   Movel     T02uni        Dged
214600060109     c                   Endif
214700060109
214800060109      * Ricerca degli importi standard nella moneta di conto
214900060109
215000060109     c                   Clear                   Tibs02ds
215100060109     c                   Clear                   Dgei
215200060109     c                   Movel     'L'           T02tla
215300060109     c                   Movel     'C'           T02mod
215400060109     c                   Movel     KNSIF         T02sif
215500060109     c                   Movel     'GEI'         T02cod
215600060109     c                   Movel     §GEDCN        T02ke1
215700060109
215800060109     c                   Call      'TIBS02R'
215900060109     c                   Parm                    Kpjba
216000060109     c                   Parm                    Tibs02ds
216100060109
216200060109     c                   If        T02err =  *blanks
216300060109     c                   Movel     T02uni        Dgei
216400120328     c                   Endif
216500120328
216600120328      *?Cerco il limite imponibile + peso tabella IMP
216700120328     c                   clear                   TIBS02DS
216800120328     c                   clear                   dIMP
216900120328     c                   eval      T02tla = 'L'
217000120328     c                   eval      T02mod = 'C'
217100120328     c                   eval      T02sif = KNSIF
217200120328     c                   eval      T02cod = 'IMP'
217300120328     c                   eval      T02ke1 = '001'
217400120328     c                   call      'TIBS02R'
217500120328     c                   parm                    kpjba
217600120328     c                   parm                    TIBS02DS
217700120328     c                   IF        T02err = *blanks
217800120328     c                   eval      dIMP = T02uni
217900120328     c                   ENDIF
218000130108
218100130108      *?Cerco data ultima spedizione fatturata tabella RPF
218200130116     c                   IF        not wDaMenu and
218300130116     c                             not wDASB48
218400130108     c                   clear                   TIBS02DS
218500130108     c                   clear                   dRPFfatgio
218600130108     c                   eval      T02tla = 'L'
218700130108     c                   eval      T02mod = 'C'
218800130108     c                   eval      T02sif = KNSIF
218900130108     c                   eval      T02cod = 'RPF'
219000130108     c                   eval      T02ke1 = 'FATTGIO'
219100130108     c                   call      'TIBS02R'
219200130108     c                   parm                    kpjba
219300130108     c                   parm                    TIBS02DS
219400130108     c                   IF        T02err = *blanks
219500130108     c                   eval      dRPFfatgio = T02uni
219600130108     c                   ENDIF
219700130116     c                   ENDIF
219800060109
219900060109      * Carico tipi bolla
220000060109
220100060109     c                   Movel     'TB'          Tabcod
220200060112     c                   z-add     1             kk
220300060109     c     KeyTab2       setll     Tabel00f
220400060109     c                   Do        *hival
220500060109     c     KeyTab2       Reade     Tabel00f
220600060109
220700060109    1c                   If        %eof(Tabel00f)
220800060109     c                   leave
220900060109     c                   endif
221000060109
221100060109    1c                   If        tblflg = ' '
221200060112     c     kk            Occur     Dstb
221300060109     c                   Movel     Tbluni        Dstb
221400060112     c                   Movel     Tblkey        Tbo(kk)
221500060112     c                   add       1             kk
221600060109    2c                   endif
221700060109
221800060109    1c                   Enddo
221900060110
222000060110      * Carico le province dei codici tassazione
222100060110
222200060110     c                   Movel     'CT'          Tabcod
222300060112     c                   z-add     1             kk
222400060110     c     KeyTab2       setll     Tabel00f
222500060110     c                   Do        *hival
222600060110     c     KeyTab2       Reade     Tabel00f
222700060110
222800060110    1c                   If        %eof(Tabel00f)
222900060110     c                   leave
223000060110     c                   endif
223100060110
223200060110    1c                   If        tblflg = ' '
223300060110     c                   Movel     Tbluni        Dsct
223400060112     c                   Movel     Tblkey        Cts(kk)
223500060112     c                   Movel     §ctprv        Cpr(kk)
223600060112     c                   add       1             kk
223700060110    2c                   endif
223800060110
223900060110    1c                   Enddo
224000060110
224100060110      * carico cod.bolla che prevedono la tassazione di una sola varia
224200060110
224300060110     c                   Movel     '3A'          Tabcod
224400060112     c                   z-add     1             kk
224500060110     c     KeyTab2       setll     Tabel00f
224600060110     c                   Do        *hival
224700060110     c     KeyTab2       Reade     Tabel00f
224800060110
224900060110    1c                   If        %eof(Tabel00f)
225000060110     c                   leave
225100060110     c                   endif
225200060110
225300060110    1c                   If        tblflg = ' '
225400060110     c                   Movel     Tbluni        Ds3a
225500060110
225600060110     c                   If        §3asva <> *blanks
225700060112     c                   Movel     Tblkey        Cbo(kk)
225800060112     c                   Movel     §3asva        Cbv(kk)
225900060112     c                   add       1             kk
226000060110     c                   endif
226100060110
226200060110    2c                   endif
226300060110
226400060110    1c                   Enddo
226500060109
226600060119      * Ricerco il limite standard dell'importo da assicurare
226700060119     c                   Clear                   Tibs02ds
226800060119     c                   Clear                   Dlia
226900060119     c                   Eval      T02Mod = 'C'
227000060119     c                   Eval      T02Sif = Knsif
227100060119     c                   Eval      T02Cod = 'LIA'
227200060119     c                   Movel     '001'         T02Ke1
227300060119     c                   Call      'TIBS02R'
227400060119     c                   Parm                    Kpjba
227500060119     c                   Parm                    Tibs02ds
227600060119     c                   If        T02Err = *Blanks
227700060119     c                   Movel     T02Uni        Dlia
227800060119     c                   EndIf
227900060119
228000130108      /free
228100130108       //?Per Fatturato giornaliero
228200130108       //?devo controllare la data DAL con la data ultima spedizione fatturata
228300130108       //?se data DAL minore/uguale devo impostare come data DAL, per fatturato giornaliero,
228400130108       //?la data ultima spedizione fatturata + 1 gg
228500130108       //?come data AL va bene quella passata dal pgm di lancio
228600130116       IF  not wDaMenu and not wDASB48;
228700130116         IF  D44dsd <= §RPFfatdsp;
228800130116           wDataISO = %date(§RPFfatdsp);
228900130116           wDataISO = wDataISO + %days(1);
229000130116           wDataDa  = %dec(wDataISO);
229100130116         ELSE;
229200130116           wDataDa  = D44dsd;
229300130116         ENDIF;
229400130116         wDataAl  = D44dsa;
229500130116       ENDIF;
229600141211
229700141211       //?Carico part.consegna con abilitazione PinCode
229800141211         TABcod = '7R';
229900141211         clear kk;
230000141211         setll (Codut:TABcod) TABEL00F;
230100141211         reade (Codut:TABcod) TABEL00F;
230200141211         DOW  not %eof(TABEL00F);
230300141211           IF  TBLflg = *blanks;
230400141211             ds7R = TBLuni;
230500141211             IF  §7Rpincode = 'S';
230600141211               kk += 1;
230700141211               skGMA(kk) = TBLkey;
230800141211             ENDIF;
230900141211           ENDIF;
231000141211           reade (Codut:TABcod) TABEL00F;
231100141211         ENDDO;
231200141211
231300130108      /end-free
231400060105
231500060109     c                   endsr
231600060109      *------------------------------------------------------------------------*
231700060109      * ROUTINE INIZIALE
231800060109      *------------------------------------------------------------------------*
231900060109     c     *Inzsr        BegSr
232000060109
232100060109     c     *entry        plist
232200060109     c                   parm                    Kpjba
232300060119     c                   eval      Tnsb44ds = Kpjbu
232400060109      * Reperisco data corrente
232500060109     c                   time                    w0140
232600060109     c                   move      w0140         wData
232700060202
232800060109     c                   clear                   WLBdat
232900060109     c                   eval      G02dat = wData
233000060109     c                   call      'XSRDA8'
233100060109     c                   parm                    WLBdat
233200060109     c                   z-add     G02inv        DateU
233300060109
233400060109     c     *dtaara       define    §azute        azuteds
233500060109     c     *dtaara       define    §datiute      ddatiute
233600060109     c                   in(E)     *dtaara
233700060109     c                   If        %error  or RSUT = *blanks
233800060109     c                   CLEAR                   tibs34ds
233900060109     c                   CALL      'TIBS34R'
234000060109     c                   PARM                    tibs34ds
234100060109     c                   in        *dtaara
234200060109     c                   EndIf
234300060120
234400060206      * Se sono in ambiente buono - GAITRAGRU
234500060208     c                   If        knsif = 'FILTRA201 ' or
234600060208     c                             knsif = 'GAITRA201 '
234700121003     c                   eval      wProva = *off
234800060206     c                   Eval      wlib = 'GAITRAGRU '
234900120220     c                   Eval      wlibc= 'UNITRAGRU'
235000120313     c                   Eval      wlibw= 'GAITRAAZM'
235100060206      *  Se sono in ambiente di prova - GAITRAGRPS
235200060206     c                   Else
235300121003     c                   eval      wProva = *on
235400060206     c                   Eval      wlib = 'GAITRAGRPS'
235500121003     c                   Eval      wlibc= 'UNITRAGRP'
235600121003     c                   Eval      wlibw= 'GAITRAAZP'
235700060206     c                   EndIf
235800060120      * Se non sono in sede
235900060120     c                   If        Dutpou <> 46
236000060120
236100060120      *  titas38c
236200060120     c                   Clear                   comman
236300060120     c                   Movel(p)  cmdt(1)       comman
236400060120     c                   Eval      %Subst(comman:30:10) = wlib
236500060120     c                   Eval      comman =
236600060120     c                             %trim(comman) + '/TITAS38C)'
236700060120     c                   Call      'QCMDEXC'                            99
236800060120     c                   Parm                    comman
236900060120     c                   Parm                    lenght
237000060120     c  n99              Open      Titas38c
237100060120      *  titas02l
237200060120     c                   Clear                   comman
237300060120     c                   Movel(p)  cmdt(2)       comman
237400060120     c                   Eval      %Subst(comman:30:10) = wlib
237500060120     c                   Eval      comman =
237600060120     c                             %trim(comman) + '/TITAS02L)'
237700060120     c                   Call      'QCMDEXC'                            99
237800060120     c                   Parm                    comman
237900060120     c                   Parm                    lenght
238000060120     c  n99              Open      Titas02l
238100060120      *  tita730c
238200060120     c                   Clear                   comman
238300060120     c                   Movel(p)  cmdt(3)       comman
238400060120     c                   Eval      %Subst(comman:30:10) = wlib
238500060120     c                   Eval      comman =
238600060120     c                             %trim(comman) + '/TITA730C)'
238700060120     c                   Call      'QCMDEXC'                            99
238800060120     c                   Parm                    comman
238900060120     c                   Parm                    lenght
239000060120     c  n99              Open      Tita730c
239100060120      *  tita430c
239200060120     c                   Clear                   comman
239300060120     c                   Movel(p)  cmdt(4)       comman
239400060120     c                   Eval      %Subst(comman:30:10) = wlib
239500060120     c                   Eval      comman =
239600060120     c                             %trim(comman) + '/TITA430C)'
239700060120     c                   Call      'QCMDEXC'                            99
239800060120     c                   Parm                    comman
239900060120     c                   Parm                    lenght
240000060120     c  n99              Open      Tita430c
240100060120      *  tncsb03l
240200060120     c                   Clear                   comman
240300060120     c                   Movel(p)  cmdt(5)       comman
240400060120     c                   Eval      %Subst(comman:30:10) = wlib
240500060120     c                   Eval      comman =
240600060120     c                             %trim(comman) + '/TNCSB03L)'
240700060120     c                   Call      'QCMDEXC'                            99
240800060120     c                   Parm                    comman
240900060120     c                   Parm                    lenght
241000060120     c  n99              Open      Tncsb03l
241100060120      *  Fiar531c
241200060120     c                   Clear                   comman
241300060120     c                   Movel(p)  cmdt(6)       comman
241400060120     c                   Eval      %Subst(comman:30:10) = wlib
241500060120     c                   Eval      comman =
241600060120     c                             %trim(comman) + '/FIAR531C)'
241700060120     c                   Call      'QCMDEXC'                            99
241800060120     c                   Parm                    comman
241900060120     c                   Parm                    lenght
242000060120     c  n99              Open      Fiar531c
242100060120      *  titaa30c
242200060120     c                   Clear                   comman
242300060120     c                   Movel(p)  cmdt(7)       comman
242400060120     c                   Eval      %Subst(comman:30:10) = wlib
242500060120     c                   Eval      comman =
242600060120     c                             %trim(comman) + '/TITAA30C)'
242700060120     c                   Call      'QCMDEXC'                            99
242800060120     c                   Parm                    comman
242900060120     c                   Parm                    lenght
243000060120     c  n99              Open      Titaa30c
243100060123
243200060123      * se sono in sede faccio solo open dei file
243300060123
243400060123     c                   else
243500060123
243600060123     c                   Open      Titas38c
243700060123     c                   Open      Titas02l
243800060123     c                   Open      Tita730c
243900060123     c                   Open      Tita430c
244000060123     c                   Open      Tncsb03l
244100060123     c                   Open      Fiar531c
244200060123     c                   Open      Titaa30c
244300060123
244400060123     c                   EndIf
244500120306
244600120306      *?Controllo se è lancio da menù
244700120308     c                   eval      wDaMenu = (D44gio = 'M')
244800120306
244900120306      *?Controllo se è lancio da TNSB48R Gestione Manca Tariffa Tasto F5-Ritassa
245000120306     c                   eval      wDaSB48 = (D44SB48 = 'S')
245100120228      *
245200120228      * file di lavoro wfmtc se sono in filiale duplico in QTEMP
245300120306      *?e se NON richiamato da TNSB48
245400120228     c                   If        1 = 1
245500120306     c                             and not wDaSB48
245600120228     c                   clear                   comman
245700120228     c                   movel(p)  cmdw(1)       comman
245800120313     c                   Eval      %Subst(comman:15:9) = wlibw
245900120228     c                   call      'QCMDEXC'
246000120228     c                   parm                    comman
246100120228     c                   parm                    lenght
246200120228
246300120228     c                   clear                   comman
246400120228     c                   movel(p)  cmdw(2)       comman
246500120313     c                   Eval      %Subst(comman:34:9) = wlibw
246600120228     c                   call      'QCMDEXC'
246700120228     c                   parm                    comman
246800120228     c                   parm                    lenght
246900120228
247000120228     c                   clear                   comman
247100120228     c                   movel(p)  cmdw(3)       comman
247200120228     c                   call      'QCMDEXC'
247300120228     c                   parm                    comman
247400120228     c                   parm                    lenght
247500120228
247600120228     c                   endif
247700120228      * apro il file di comodo per la stampa
247800120306      *?se NON richiamato da TNSB48
247900120306     c                   IF        not wDaSB48
248000120228     c                   open      wfmtc00f
248100120306     c                   ENDIF
248200120228
248300120222      *?Se è lancio da menù devo duplicare il file in QTEMP VUOTO
248400120222     c                   IF        wDaMenu
248500120222     c                   clear                   comman
248600120222     c                   movel(p)  cmdc(4)       comman
248700120308     c                   Eval      %Subst(comman:34:9) = wlibc
248800120222     c                   call      'QCMDEXC'
248900120222     c                   parm                    comman
249000120222     c                   parm                    lenght
249100120222      *?Se è schedulato devo copiare il file in QTEMP PIENO
249200120222     c                   ELSE
249300120306      *?se NON richiamato da TNSB48
249400120306     c                   IF        not wDaSB48
249500120222      *?Copio il file TFMTC00F in QTEMP
249600120217     c                   clear                   comman
249700120217     c                   movel(p)  cmdc(1)       comman
249800120217     c                   Eval      %Subst(comman:15:9) = wlibc
249900120217     c                   call      'QCMDEXC'
250000120217     c                   parm                    comman
250100120217     c                   parm                    lenght
250200120306     c                   ENDIF
250300120222     c                   ENDIF
250400120222
250500120217      *?Duplico TFMTC01L in QTEMP
250600120306      *?se NON richiamato da TNSB48
250700120306     c                   IF        not wDaSB48
250800120217     c                   clear                   comman
250900120217     c                   movel(p)  cmdc(2)       comman
251000120217     c                   Eval      %Subst(comman:34:9) = wlibc
251100120217     c                   call      'QCMDEXC'
251200120217     c                   parm                    comman
251300120217     c                   parm                    lenght
251400120306     c                   ENDIF
251500120306      *?Apro TFMTC01L
251600120217     c                   open      TFMTC01L
251700120217
251800120217      /free
251900120217         //?Calcolo oggi - 60 gg.
252000120221           dataiso = %date(dateu);
252100120221           dataiso = dataiso - %days(60);
252200120221           wOggi_60 = %dec(dataiso);
252300120217      /end-free
252400120217
252500060202      *
252600060105      *
252700060105      *------------------------------------------------------------------------*
252800060105      * KLIST
252900060105      *------------------------------------------------------------------------*
253000060105
253100060105      *---- File Cnaco00f
253200060105
253300060105     c     KeyAco        Klist
253400060105     c                   Kfld                    Codut
253500060105     c                   Kfld                    DutKci
253600060105     c                   Kfld                    TasKsc
253700060105
253800060105      *---- File Titas38c
253900060105
254000060111     c     KeyTas38      Klist
254100060105     c                   Kfld                    KTasAas
254200060105     c                   Kfld                    KTasMgs
254300060113
254400060111      *---- File Titas02l
254500060111
254600060111     c     KeyTas02      Klist
254700060111     c                   Kfld                    KTasksc
254800060113
254900060113      *---- File Tita730C
255000060113
255100060113     c     KeyTa730      Klist
255200060113     c                   Kfld                    Tasaas
255300060113     c                   Kfld                    Taslnp
255400060113     c                   Kfld                    Tasnrs
255500060113     c                   Kfld                    Tasnsp
255600060113     c                   Kfld                    Tastbl
255700060119
255800060119      *---- File Tita430C
255900060119
256000060119     c     KeyTa430      Klist
256100060119     c                   Kfld                    Tasaas
256200060119     c                   Kfld                    Taslnp
256300060119     c                   Kfld                    Tasnrs
256400060119     c                   Kfld                    Tasnsp
256500060119     c                   Kfld                    Keytrc
256600060113
256700060113      *---- File Tncsb03l
256800060113
256900060113     c     KeyTncsb      Klist
257000060113     c                   Kfld                    Tasaas
257100060113     c                   Kfld                    Taslnp
257200060113     c                   Kfld                    Tasnrs
257300060113     c                   Kfld                    Tasnsp
257400060113
257500060113      *---- File Fiar531c
257600060113
257700060113     c     keyFiar5      Klist
257800060113     c                   Kfld                    TasAas
257900060113     c                   Kfld                    TasLnp
258000060113     c                   Kfld                    TasNrs
258100060113     c                   Kfld                    TasNsp
258200060113     c                   Kfld                    kAr5Trd
258300060113
258400060113      *---- File Fnevb00f
258500060113
258600060113     c     KeyEvb        Klist
258700060113     c                   Kfld                    Tasaas
258800060113     c                   Kfld                    Taslnp
258900060113     c                   Kfld                    Tasnrs
259000060113     c                   Kfld                    Tasnsp
259100060113     c                   Kfld                    Wcev
259200060109
259300060109      *---- File Tabel00f
259400060109
259500060109     c     KeyTab        Klist
259600060109     c                   Kfld                    Codut
259700060109     c                   Kfld                    Tabcod
259800060109     c                   Kfld                    Tabkey
259900060109
260000060109     c     KeyTab2       Klist
260100060109     c                   Kfld                    Codut
260200060109     c                   Kfld                    Tabcod
260300060105      *
260400060105     c                   EndSr
260500960924**   ERR
260600060120Manca Anagrafica Cliente
260700060120Anagrafica Cliente Annullata
260800060120Manca sottoconto Intest.Fattura
260900060120Sottoconto Intest.Fatt. Annullato
261000060117Tariffa valida solo in ARRIVO
261100060120Manca Tariffa
261200060120Imp.Maggiore del limite Fatturabile
261300060120Tipo fattura errato
261400060117Tariffa valida solo in PARTENZA
261500060117Tariffa BLOCCATA
261600060120Imp. d'assicurare maggiore del limite
261700060120Manca codice cliente
261800120322Peso/Collo superiore 5 volte media del cliente
261900120326Imponibile maggiore di EUR xxxxxxxxxx,xxx
262000120907Imponibile a ZERO
262100050711** cmd ovrprtf
262200050712OVRPRTF PRTF1982 OUTQ(xxxxxxxxxx) USRDTA('MancaTarif') SAVE(*YES) HOLD(*YES)       1
262300060120** cmdt ovrdbf
262400060120OVRDBF FILE(TITAS38C) TOFILE(
262500060120OVRDBF FILE(TITAS02L) TOFILE(
262600060120OVRDBF FILE(TITA730C) TOFILE(
262700060120OVRDBF FILE(TITA430C) TOFILE(
262800060120OVRDBF FILE(TNCSB03L) TOFILE(
262900060120OVRDBF FILE(FIAR531C) TOFILE(
263000060120OVRDBF FILE(TITAA30C) TOFILE(
263100060120** cmdc crtdupobj
263200120217CPYF FROMFILE(         /TFMTC00F) TOFILE(QTEMP/TFMTC00F) CRTFILE(*YES)
263300120217CRTDUPOBJ OBJ(TFMTC01L) FROMLIB(          ) OBJTYPE(*FILE) TOLIB(QTEMP)
263400121003CPYF FROMFILE(QTEMP/TFMTC00F) TOFILE(         /TFMTC00F) MBROPT(*ADD)
263500120222CRTDUPOBJ OBJ(TFMTC00F) FROMLIB(          ) OBJTYPE(*FILE) TOLIB(QTEMP)
263600120224DLTF FILE(QTEMP/TFMTC*)
263700120228** cmdw crtdupobj
263800120228CPYF FROMFILE(         /WFMTC00F) TOFILE(QTEMP/WFMTC00F) FROMMBR(WFMTC00F) CRTFILE(*YES)
263900120228CRTDUPOBJ OBJ(WFMTC01L) FROMLIB(          ) OBJTYPE(*FILE) TOLIB(QTEMP)
264000120228CLRPFM FILE(QTEMP/WFMTC00F)
264100120229DLTF FILE(QTEMP/WFMTC*)
264200120402CPYF FROMFILE(QTEMP/WFMTC00F) TOFILE(GAITRAAZM/WFMTC00F) TOMBR(          ) MBROPT(*REPLACE)
