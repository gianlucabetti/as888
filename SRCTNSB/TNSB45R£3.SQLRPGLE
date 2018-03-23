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
106500160122      * modifico per il recupero della tariffa di cartello con la data del giorno di tassazione
106600160122     c                   movel     dateu         §Tasdct
106700060113     c                   movel     Tascad        §Tascad
106800060113     c                   movel     Tasfin        §Tasfin
106900060113     c                   movel     Tascam        §Tascam
107000060113     c                   movel     Tasfap        §Tasfap
107100060113     c                   movel     Tastsp        §Tastsp
107200060113     c                   movel     Tasftc        §Tastc1
107300060113     c                   movel     Tastc2        §Tastc2
107400060113     c                   movel     Tasdiv        §Tasdiv
107500060113      *
107600060113      * aggancio il contrassegno.
107700060113      * se trovato verifico che il 1° carattere del tipo bolla corrisponda
107800060113     c                   if        tasfca <> *blanks
107900060113     c     Keytncsb      chain     Tncsb03l
108000060113     c                   if        %found(Tncsb03l)   and
108100060113     c                             Tastbl = csbtbl
108200060113     c                   z-add     Csbcas        §Tascas                         Contrassegno
108300060113     c                   movel     Csbvca        §Tasvca                         Valuta
108400060113     c                   z-add     Csbcmb        §Tascmb                         Cambio Divisa
108500061016     c****               movel     Csbtpa        §Tastic                         Tipo Incasso
108600060113     c                   if        csbfus <> *blanks
108700060113     c                   move      Csbfus        §tastic
108800060113     c                   else
108900060113     c                   move      Csbtpi        §Tastic                         Tipo Incasso
109000060113     c                   endif
109100060113     c                   endif
109200060113     c                   endif
109300060113
109400060113     c                   movel     Tasvas        §Tasvas
109500060113     c                   movel     Tasfdn        §Tasfdn                         Merce Port.Mag.
109600060113     c                   movel     Tasmct        §Tasmct                         Cod.Tassaz.Mittente
109700060113     c                   z-add     Taslnp        §Taslnp
109800060113     c                   z-add     Taslna        §Taslna
109900060113     c                   movel     Tastbl        §Tastbl
110000060113     c                   z-add     Tasias        §Tasias                         Importo assicurazio
110100060113     c                   z-add     Tasqft        §Tasqft                         Qta' Fatturabile
110200060113     c                   z-add     Taspor        §Taspor                         Trasporto
110300060113     c                   z-add     tasimv        §Tasimv                         Tot.Imponibile
110400060113
110500060113     c                   if        Tasll1 = 'S' or Tasll1 = 'C'
110600060113     c                   movel     'S'           §Tasddt                         flag si/no ddt
110700060113     c                   else
110800060113     c                   clear                   §Tasddt
110900060113     c                   endif
111000060926     c*****              movel     tasfbr        §tasflo
111100060926     c                   clear                   dtas01
111200060926     c                   movel     tasfbr        §asfbr
111300060926     c                   movel     tascca        §ascca
111400110701      * valorizzo flag email al destinatario
111500110701     c                   movel     tasflo        dtasflo
111600110701     c                   move      §floemd       §asemd
111700141211      /free
111800151127       //?Imposto se Prenotazione Ritiro Telefonico
111900151127         §TASprt = §FLOado;
112000151127
112100141211       //?Imposto se part.consegna con PinCode
112200141211         IF  TASgma <> *blanks and %lookup (TASgma:skGMA) > 0;
112300141211           §ASpin = 'S';
112400141211         ENDIF;
112500151127
112600151127       //?Imposto se Stampa Packing List
112700151127         clear dAR5gen;
112800151127         chain (TASaas:TASlnp:TASnrs:TASnsp:'GEN') FIAR531C;
112900151127         IF  %found(FIAR531C);
113000151127           dAR5gen = AR5uni;
113100151127         ENDIF;
113200151127         IF  §AR5als = 'S' or §AR5alx = 'S';
113300151127           §TASspl = 'S';
113400151127         ENDIF;
113500141211      /end-free
113600110701
113700060926     c                   eval      §tasflo = dtas01
113800060113
113900060113     c                   clear                   Kpjbu
114000060125     c                   clear                   Monovaria
114100060113
114200060113      * Le bolle con totale imponibile le controllo per controllare l'assicurazione
114300060113     c                   clear                   Parca
114400060113     c                   setoff                                       10
114500060113    1c                   If        Tasimv > *zeros
114600060113     c                   movel     'R'           §Tassva
114700060113     c                   movel     'S'           Parca
114800060113     c                   else
114900060113     c                   z-add     1             kk
115000060113     c     tascbo        lookup    cbo(kk)                                31
115100060113     c                   if        *in31
115200060113     c                   eval      §tassva=cBV(kk)
115300060125     c                   If        cbv(kk) <> ' '
115400060125     c                   eval      monovaria = 'SI'
115500060113     c                   endif
115600060125     c                   endif
115700060113
115800060113     c                   if        §tassva<>*blanks
115900060113     c                   z-add     1             kk
116000060113     c     §tassva       lookup    tsv(kk)                                10
116100060113      * se la varia c'è ed è valorizzata e imponibile = 0 devo richiamare la tassazione
116200060113     c                   If        *In10 and tva(kk) > 0 and TasImv = 0
116300060113     c                   Eval      *In10 = *Off
116400060113     c                   EndIf
116500060113     c                   Endif
116600060113
116700060113    1c                   Endif
116800060113
116900060113      * imposto peso vdl e colli rilevati
117000060113     c                   z-add     tasncp        §tasncp
117100060113     c                   z-add     taspkc        §taspkc
117200060113     c                   clear                   §taspkcN
117300060113
117400060113      * Bancali
117500060113     c                   If        %Subst(TasGva:1:1) ='B'
117600060113     c                   Clear                   dAr5Ban
117700060113     c                   Eval      kAr5Trd = 'BAN'
117800060113     c     keyFiar5      Chain     Fiar531c
117900060113     c                   If        %Found(Fiar531c)
118000060113     c                   Movel     Ar5Uni        dAr5Ban
118100060113     c                   Eval      §TasBan = §Ar5Nba + §ar5Nb2
118200060113     c                   EndIf
118300060113     c                   EndIf
118400060113      * Colli originali
118500060113     c                   If        %Subst(TasGva:1:1) ='O'
118600060113     c                   Clear                   dAr5Bnb
118700060113     c                   Eval      kAr5Trd = 'BNB'
118800060113     c     keyFiar5      Chain     Fiar531c
118900060113     c                   If        %Found(Fiar531c)
119000060113     c                   Movel     Ar5Uni        dAr5Bnb
119100060113     c                   Eval      §TasNcl = §Ar5bCor
119200090327      * mi salvo i colli da mettere nel file manca tariffa
119300090327     c                   eval      stpNcl = §ar5bCor
119400060113     c                   EndIf
119500060113     c                   EndIf
119600060113     c
119700060113      * Se bolla di reso e tassata fino all'imponibile con varia 'N' 88888888
119800060113      * prima di tassare la bolla devo tassare la varia 'N'
119900060113     c                   if        tasimv = 0 and tasfbr = 'S'
120000060116     c                   exsr      Sr_tassan
120100060113     c                   endif
120200060113
120300060113     c                   clear                   Kpjbu
120400060113
120500060113     c                   movel     paramt        Kpjbu
120600060113      * verifico se esiste la varia & con importo uguale a 8888888 e se si passo al
120700060113      * TNSF20R il flag di calcolo varia &
120800060113      *
120900060113     c                   setoff                                       51
121000060113     c                   z-add     1             kk
121100060113     c     '&'           lookup    tsv(kk)                                51
121200060113     c                   if        *in51 and tva(kk) = 88888888
121300060113     c                   eval      §tasfcaa = 'S'
121400060113     c                   clear                   tsv(kk)
121500060113     c                   clear                   tva(kk)
121600060113     c                   endif
121700060113      * verifico se devo calcolare l'addebito di lasciato avviso
121800060113      *
121900060113      * verifico se c'è evento
122000060113     c     KeyEvb        chain     Fnevb04l
122100060113
122200060113     c                   If        %found(Fnevb04l)
122300060113     c                   eval      §tasric = 'S'
122400060113     c                   endif
122500060113
122600151001      * imposto dati
122700151001     c                   eval      tasppkb = taspkb
122800160504     c                   eval      taspfvf = TASfvf
122900160504     c                   eval      taspncr = TASncr
123000060113
123100060113     c                   if        §tassva=*blanks or not *in10
123200151001     c                   Call      'TNSF22R'
123300060113     c                   Parm                    Kpjba
123400060113     c                   Parm                    Dtas
123500060113     c                   Parm                    Dtasv
123600151001     c                   parm                    dtaspes
123700060119
123800151001     c                   eval      Wtnsf22r = '1'
123900060119
124000060113     c                   endif
124100060113
124200060113      * stampo e uso il peso usato in fatturazione
124300151013     c                   If        §Tasspc <> ' '
124400060113      * mi salvo il peso da fatturare per stamparlo in fattura
124500151013     c                   eval      taspkf=§TaspkcN
124600151013     c                   endif
124700060116      * Se tassando la bolla ho errori oppure non ho errori ma devo stampare l'imponibile
124800060116      * scrivo il record sul file work
124900060116
125000060119    1c                   If        (§taserr = ' ' and d44fsi = 'S') or
125100060116     c                              §taserr <> ' '
125200060116     c                   eval      Savbap = §tasbap
125300060116     c                   eval      Savcas = §Tascas
125400060116     c                   eval      Savvca = §Tasvca
125500060116     c                   eval      Savrma = §Tasrma
125600120322     c                   movel     §Tasmsg       wMtcerr
125700060208
125800060223     c                   If        d44fsi = 'S' and §taserr = ' '
125900120322     c                   eval      wmtcimp = 'IMP'
126000060208     c                   endif
126100060208
126200060223      * valorizzo i campi dell'imponibile e della divisa
126300060123     c                   z-add     §tasimv       tasimv
126400060123     c                   eval      tasdiv = §tasdiv
126500060123
126600120220     c                   eval      wMTCtrc = 'M'
126700060116     c                   Exsr      SR_ScriveErr
126800060116
126900060119   x1c                   else
127000120413
127100120413      * controllo se imponibile maggiore del limite massimo fatturabile
127200120413      * da tabella GEI
127300120413    2c                   If        §tasimv > §geimf
127400120413     c                   eval      wMTCtrc = 'M'
127500120413     c                   eval      wmtcerr = err(7)
127600120413     c                   eval      wmtcimp = 'IMF'
127700120413      * valorizzo i campi dell'imponibile e della divisa
127800120413     c                   z-add     §tasimv       tasimv
127900120413     c                   eval      tasdiv = §tasdiv
128000120413     c                   Exsr      SR_ScriveErr
128100120413     c                   leavesr
128200120413    2c                   endif
128300120907       //?Controllo se imponibile a zero
128400120910       //?non se bolla monovaria
128500120910    2c                   If        §tasimv = 0 and monovaria <> 'SI'
128600120907     c                   eval      wMTCtrc = 'M'
128700120910     c                   eval      wmtcerr = err(15)
128800120907     c                   eval      wmtcimp = 'IMF'
128900120907      * valorizzo i campi dell'imponibile e della divisa
129000120907     c                   z-add     §tasimv       tasimv
129100120907     c                   eval      tasdiv = §tasdiv
129200120907     c                   Exsr      SR_ScriveErr
129300120907     c                   leavesr
129400120907    2c                   endif
129500060116
129600060116      * Se tassando la bolla non ho errori e non devo stampare l'imponibile
129700120328      * controllo se imponibile nei limiti indicati nelle tabelle
129800120328       //?limite imponibile bolla + peso da tabella IMP
129900120416       //?peso fino a kg. e imponibile > di xxx
130000120328    2c                   IF        TASpkf <= §IMPpeso and §tasimv > §IMPimp1
130100060116      * verifico se imponibile inserito  manualmente
130200060116     c                   if        tasimv > 0
130300120322     c                   eval      wmtcinm = 'M'
130400060116     c                   endif
130500120326      * valorizzo i campi dell'imponibile e della divisa
130600120326     c                   z-add     §tasimv       tasimv
130700120326     c                   eval      tasdiv = §tasdiv
130800120326     c                   eval      wmtcimp = 'IMP'
130900120326     c                   eval      wMTCtrc = 'M'
131000120326     c                   eval      wmtcerr = err(14)
131100120326     c                   eval      %subst(wmtcerr:28:14) =
131200120329     c                             %trim(%editc(§IMPimp1:'4'))
131300120326     c                   Exsr      SR_ScriveErr
131400120416       //?Se torna che la bolla non ha già il Visto vado alla fine dei controlli perchè il rcd
131500120416       //?è già stato scritto nella routine scriveerr
131600120416     c                   IF        not wVisto
131700120326     c                   leavesr
131800120416       //?Se torna che la bolla ha già il Visto devo andare avanti con gli altri controlli
131900120416       //?per cercare eventuali altri errori da scrivere
132000120416     c                   ELSE
132100120416     c                   clear                   wmtcimp
132200120416     c                   clear                   wMTCtrc
132300120416     c                   clear                   wMTCerr
132400120413     c                   ENDIF
132500060119    2c                   endif
132600120416
132700120328       //?limite imponibile bolla + peso da tabella IMP
132800120416       //?peso oltre kg. e imponibile > di xx
132900120328    2c                   IF        TASpkf > §IMPpeso and §tasimv > §IMPimp2
133000120328      * verifico se imponibile inserito  manualmente
133100120328     c                   if        tasimv > 0
133200120328     c                   eval      wmtcinm = 'M'
133300120328     c                   endif
133400120328      * valorizzo i campi dell'imponibile e della divisa
133500120328     c                   z-add     §tasimv       tasimv
133600120328     c                   eval      tasdiv = §tasdiv
133700120328     c                   eval      wmtcimp = 'IMP'
133800120328     c                   eval      wMTCtrc = 'M'
133900120328     c                   eval      wmtcerr = err(14)
134000120328     c                   eval      %subst(wmtcerr:28:14) =
134100120329     c                             %trim(%editc(§IMPimp2:'4'))
134200120328     c                   Exsr      SR_ScriveErr
134300120416       //?Se torna che la bolla non ha già il Visto vado alla fine dei controlli perchè il rcd
134400120416       //?è già stato scritto nella routine scriveerr
134500120416     c                   IF        not wVisto
134600120413     c                   leavesr
134700120416       //?Se torna che la bolla ha già il Visto devo andare avanti con gli altri controlli
134800120416       //?per cercare eventuali altri errori da scrivere
134900120416     c                   ELSE
135000120416     c                   clear                   wmtcimp
135100120416     c                   clear                   wMTCtrc
135200120416     c                   clear                   wMTCerr
135300120413     c                   ENDIF
135400120328    2c                   endif
135500120328
135600060117      * verifico per le tariffe a collo il peso del collo
135700130913      * e se è superiore di oltre  5 volte alla media del cliente lo segnalo
135800060117      * non viene controllato se bolla con varia "O"
135900130913      * e peso bolla minore di un kg (xchè non significativo)
136000060117     c                   movel     tasctr        Tipo_tar
136100060119    2c                   If        Tipo_tar = '1' and tasimv = 0 and
136200090327     c                             %Subst(TasGva:1:1) <>'O'
136300130913     c                             and taspkf >  1
136400060117      * richiamo il tise70r x recuperare i dati degli ultimi 3 mesi
136500060117     c                   ExSr      Sr_tise
136600060117      * Calcolo la media del cliente
136700060117     c                   Exsr      Sr_Calmed
136800060117      * se 08 acceso scrivo
136900060117     c                   If        *in08
137000120321     c                   eval      wMTCtrc = 'M'
137100120322     c                   eval      wmtcesc = '!!!'
137200120322     c                   eval      wmtcerr = err(13)
137300120322     c                   Exsr      SR_ScriveErr
137400120322     c                   leavesr
137500060117     c                   Endif
137600060119    2c                   Endif
137700060117
137800140402      * recupero il tipo bolla
137900140402      /free
138000140402        kk = %lookup(TAStbl:tbo);
138100140402        IF  kk > 0;
138200140402          %occur(dstb) = kk;
138300140402        ENDIF;
138400140402      /end-free
138500140402
138600060117      * se imponibile in bolla uguale a zero e
138700060117      * tariffa bloccata oppure franco e tariffa
138800060125      * oppure franco e tariffa valida solo in arrivo e non è bolla monovaria
138900060125      * oppure assegnato e tariffa valida solo in partenza e non è bolla monovaria
139000060125      * segnalo l'errore
139100060117
139200060119    2c                   If        Tasimv = 0 and §tasbap <> ' '
139300060125     c                   If        §tbtpo = 'F' and §tasbap = 'A'  and
139400060125     c                             monovaria <> 'SI'
139500120402     c                   eval      wMTCtrc = 'M'
139600120322     c                   movel     err(5)        wMtcerr
139700120329     c                   Exsr      SR_ScriveErr
139800120329     c                   leavesr
139900060117     c                   endif
140000060125     c                   If        §tbtpo = 'A' and §tasbap = 'P'  and
140100060125     c                             monovaria <> 'SI'
140200120402     c                   eval      wMTCtrc = 'M'
140300120322     c                   movel     err(9)        wMtcerr
140400120329     c                   Exsr      SR_ScriveErr
140500120329     c                   leavesr
140600060117     c                   endif
140700060119     c                   If        §tasbap = 'B'
140800120402     c                   eval      wMTCtrc = 'M'
140900120322     c                   movel     err(10)       wMtcerr
141000120329     c                   Exsr      SR_ScriveErr
141100120329     c                   leavesr
141200060119     c                   endif
141300060119    2c                   endif
141400120329
141500060119      * Controllo il limite dell'importo da assicurare
141600070518    2c                   If         TasIas <> *Zeros or §TasIal <> *Zeros
141700060119     c                   Clear                   Trul22Ds
141800060119     c                   Eval      I22Cbo = TasCbo
141900060119     c                   Eval      I22Tsp = TasTsp
142000060119     c                   Eval      I22Lnp = TasLnp
142100060119     c                   Eval      I22Nzm = TasNzm
142200060119     c                   Eval      I22Lna = TasLna
142300060119     c                   Eval      I22Nzd = TasNzd
142400060119     c                   Eval      I22Ksc = TasKsc
142500060119     c                   Movel     TasCtr        I22Ctr
142600070518    3c                   If        TasIas <> *Zeros
142700060119     c                   Eval      I22Imp = TasIas
142800060119     c                   Eval      I22Div = TasVas
142900070518    3c                   EndIf
143000070518    3c                   If        §TasIal <> *Zeros
143100060119     c                   Eval      I22Imp = §TasIal
143200060119     c                   Eval      I22Div = §TasDiv
143300070518    3c                   EndIf
143400060119     c                   Call      'TRUL22R'
143500060119     c                   Parm                    trul22ds
143600060119      * controllo che non sia maggiore del limite previsto
143700060119      * non forzabile
143800070518    3c                   If        O22Fx2 <> *blanks
143900120322     c                   Z-Add     I22Imp        wMtcIas
144000120322     c                   Movel     I22Div        wMtcVas
144100120322     c                   Eval      wMtcAss = 'ASS'
144200070518      * verifico se la spedizione con mandato assicurativo è :
144300070518      * - porto assegnato
144400070518      * - Euroexpress IMPORT
144500070518      * non segnalo l'errore
144600070518    4c                   If        §task88 = 'N' and §Tbtpo = 'A'
144700070523     c     taslna        chain     azorg01l
144800070518    5c                   if        %found(azorg01l) and orgfl1 = ' '            italia
144900070523     c     taslnp        chain     azorg01l
145000070518    6c                   if        %found(azorg01l) and orgfl1 = 'E'            Estero
145100070518     c                   eval      og143 = orgde3
145200070518    7c                   if        §ogntw = 'EEX'
145300120322     c                   clear                   wMtcAss
145400120322     c                   clear                   wMtcIas
145500120322     c                   clear                   wMtcVas
145600070518    7c                   EndIf
145700070518    6c                   EndIf
145800070518    5c                   EndIf
145900070518    4c                   EndIf
146000070518    3c                   EndIf
146100060119    2c                   EndIf
146200060119
146300060119      * se devo scrivere una segnalazione di imponibile anomalo  o assicurazione  o
146400060119      * errori
146500120413     c                   If        wmtcimp = 'IMP' or wMtcAss = 'ASS' or
146600120322     c                             wmtcerr <> *blanks or wmtcesc = '!!!'
146700060119
146800060116     c                   eval      Savbap = §tasbap
146900060116     c                   eval      Savcas = §Tascas
147000060116     c                   eval      Savvca = §Tasvca
147100060116     c                   eval      Savrma = §Tasrma
147200060123      * valorizzo i campi del'imponibile e della divisa
147300060123
147400060123     c                   z-add     §tasimv       tasimv
147500060123     c                   eval      tasdiv = §tasdiv
147600120220
147700120220      /free
147800120220       //?Imposto il tipo record 'S' se non è un manca tariffa
147900120220       //?il manca tariffa lo imposto a 'M' appena lo trovo
148000120220         IF  wMTCtrc = *blanks;
148100120220           wMTCtrc = 'S';
148200120220         ENDIF;
148300120220      /end-free
148400060123
148500060116     c                   Exsr      SR_ScriveErr
148600060119
148700060116     c                   endif
148800060119
148900060119     c                   endif
149000060119      *
149100060113     c                   endsr
149200060113
149300060113      *-----------------------------------------------------------
149400060113      * Tasso la Varia 'N' 88888888
149500060113      *-----------------------------------------------------------
149600060116     c     SR_Tassan     begsr
149700060113
149800060113      * verifico se c'è la varia 'N' 88888888
149900060113     c                   eval      kk = 1
150000060113     c     'N'           lookup    tsv(kk)                                32
150100060113     c                   if        *in32 and tva(kk) = 88888888
150200060113      * richiamo il programma di tassazione
150300060113      * senza passare le varie e il porto
150400060113     c                   movea     tsv           w_tsv
150500060113     c                   movea     tva           w_tva
150600060113     c                   movea     tev           w_tev
150700060113     c                   eval      w_dtas = dtas
150800110701
150900110701      * tolgo il flag dell'invio mail al destinatario nell'eventualità ci fosse
151000110701     c                   eval      dtas01 = §tasflo
151100110701     c                   clear                   §asemd
151200141211      /free
151300141211       //?Pulisco flag PinCode
151400141211         clear §ASpin;
151500141211      /end-free
151600110701     c                   eval      §tasflo = dtas01
151700160113
151800160113      * tolgo flag per calcolo  Diritto di chiamata prenotazione  ritiro e packing list
151900160113      *
152000160113     c                   clear                   §TASPRT
152100160113     c                   clear                   §TASSPL
152200160113
152300060113
152400060113     c                   clear                   dtasv
152500060113     c                   clear                   §tasdiv
152600060113     c                   clear                   §taspor
152700060113
152800060113     c                   clear                   kpjbu
152900060113     c                   call      'TNSF20R'
153000060113     c                   parm                    kpjba
153100060113     c                   parm                    dtas
153200060113     c                   parm                    dtasv
153300060113      * valorizzo la varia 'N' 88888888
153400060113     c                   if        §taserr = ' '
153500060116     c                   z-add     §tasimv       w_tva(kk)
153600060113     c                   endif
153700060113      * ripristino le ds salvate prima
153800060113     c                   movea     w_tsv         tsv
153900060113     c                   movea     w_tva         tva
154000060113     c                   movea     w_tev         tev
154100060113     c                   eval      dtas = w_dtas
154200060113     c                   endif
154300060113
154400060113     c                   endsr
154500060117      *------------------------------------------------------------------------*
154600060116      *    richiamo il tise70r x recuperare i dati
154700060117      *------------------------------------------------------------------------*
154800060116     c     Sr_Tise       BegSr
154900060116
155000060116     c                   move      Tasaas        waaf
155100060116     c                   movel     Tasmgs        wmmf
155200060116     c                   move      01            wggf
155300060116
155400060116     c     *iso          move      wdataf        dataiso
155500060116     c                   subdur    2:*m          dataiso
155600060116     c                   extrct    dataiso:*m    wmmi
155700060116     c                   extrct    dataiso:*y    waai
155800060116
155900060116     c                   clear                   TISE70DS
156000060116     c                   eval      I70Tla = 'S'
156100060116     c                   eval      I70Tle = 'ST'
156200060116     c                   eval      I70Ksc = Tasksc
156300060116     c                   eval      I70Daf = waammf
156400060116     c                   eval      I70Dai = waammi
156500060116
156600060116     c                   call      'TISE70R'
156700060116     c                   parm                    Tise70ds
156800060116
156900060116     c                   EndSr
157000060117      *------------------------------------------------------------------------*
157100060117      *    Calcolo la media del cliente
157200060117      *------------------------------------------------------------------------*
157300060119     c     Sr_Calmed     Begsr
157400060117
157500070704     c                   clear                   *in08
157600120402
157700120402       //?Il controllo lo faccio solo se raggruppamento bolla (§TBRBL) é NORMALE (N)
157800120402      /free
157900120402        kk = %lookup(TAStbl:tbo);
158000120402        IF  kk > 0;
158100120402          %occur(dstb) = kk;
158200120402          IF  §TBrbl <> 'N';
158300120402            leavesr;
158400120402          ENDIF;
158500120402        ENDIF;
158600160518
158700160518       //?e se il n. di spedizioni è > di 50
158800160518       //?come da indicazione di Bocchi
158900160518        IF  O70nsp <= 50;
159000160518          leavesr;
159100160518        ENDIF;
159200120402      /end-free
159300120402
159400060117      * Peso unitario della media del cliente
159500060117
159600060116     c                   If        o70nco > *Zeros
159700060117     c     o70pkg        div       o70nco        wunim
159800060117      * peso unitario della spedizione
159900090327     c     TasPKF        div(h)    Tasncl        wunis
160000090324      * si è deciso di modificare il controllo del peso 10 volte superiore
160100090324      * alla media dell cliente con solo 5 volte superiore
160200090324     c**** Wunim         Mult      10            Wunimt
160300090324     c     Wunim         Mult      5             Wunimt
160400060117
160500060117     c                   eval      *in08 =(Wunis >= wunimt)
160600060117
160700070704     c                   EndIf
160800060117     c                   Endsr
160900060117
161000060112      *------------------------------------------------------------------------*
161100060112      * Scrittura file di lavoro
161200060112      *------------------------------------------------------------------------*
161300060112     c     SR_ScriveErr  BegSr
161400120220
161500120220      /free
161600120413         wVisto = *off;
161700120326       //?Se devo scrivere un manca tariffa per IMP > di §geimv prima verifico che
161800120326       //?non sia già stato vistato dall'utente, lo cerco quindi con tipo rcd V=OK da utente
161900120326         IF  wmtcimp = 'IMP' and wmtctrc = 'M';
162000120326           k_MTCtrc = 'V';
162100120413           k_MTCaas = TASaas;
162200120413           k_MTClnp = TASlnp;
162300120413           k_MTCnrs = TASnrs;
162400120413           k_MTCnsp = TASnsp;
162500120413           k_MTCtbl = TAStbl;
162600120326           chain(n) %kds(k06tfmtc) TFMTC01L;
162700120326           IF  %found(TFMTC01L);
162800120413             wVisto = *on;
162900120326             leavesr;
163000120326           ENDIF;
163100120326         ENDIF;
163200120326
163300120307       //?Se sto rielaborando un rcd di Manca tariffa già presente sul file
163400120307         IF  wRcdMtc;
163500120307           k_MTCtrc = MTCtrc;
163600120307           k_MTCaas = MTCaas;
163700120307           k_MTClnp = MTClnp;
163800120307           k_MTCnrs = MTCnrs;
163900120307           k_MTCnsp = MTCnsp;
164000120307           k_MTCtbl = MTCtbl;
164100120307           chain(e) %kds(k06tfmtc) TFMTC01L;
164200120307           IF  %error;
164300120307             leavesr;
164400120307           ENDIF;
164500120307           IF  %found(TFMTC01L);
164600120307           //?se il campo di work che pilota se segnalazione o manca tariffa è = 'S'
164700120307           //?da manca tariffa è diventato segnalazione quindi cancello il rcd di manca tariffa
164800120307             IF  wMTCtrc = 'S';
164900120307               delete  TFMTC000;
165000120307             ENDIF;
165100120307           //?se il campo di work che pilota se segnalazione o manca tariffa è = 'M'
165200120307           //?è ancora manca tariffa quindi aggiorno il rcd di manca tariffa
165300120307             IF  wMTCtrc = 'M';
165400120307               exsr MemDati;
165500120307               update  TFMTC000;
165600120307               leavesr;
165700120307             ENDIF;
165800120307           ENDIF;
165900120307         ENDIF;
166000120307         //?Se è una segnalazione verifico se non è già presente sul file
166100120307         IF  wMTCtrc = 'S';
166200120307           k_MTCtrc = wMTCtrc;
166300120308           k_MTCaas = TASaas;
166400120308           k_MTClnp = TASlnp;
166500120308           k_MTCnrs = TASnrs;
166600120308           k_MTCnsp = TASnsp;
166700120308           k_MTCtbl = TAStbl;
166800120307           chain(n) %kds(k06tfmtc) TFMTC01L;
166900120307         //?Se è già presente non faccio niente perchè non è da riscrivere
167000120307           IF  %found(TFMTC01L);
167100120307             //?Scrivo il WF
167200120307             //?se non è richiamato da TNSB48
167300120307             IF  not wDaSB48;
167400120307               exsr wrtWF;
167500120307             ENDIF;
167600120307             leavesr;
167700120307           ENDIF;
167800120307         ENDIF;
167900120307
168000120307       //?Scrivo nuovo record
168100120307         MTCtrc = wMTCtrc;
168200120307         MTCaas = TASaas;
168300120307         MTClnp = TASlnp;
168400120307         MTCnrs = TASnrs;
168500120307         MTCnsp = TASnsp;
168600120307         MTCtbl = TAStbl;
168700120307         MTCDSP = TASaas * 10000;
168800120307         MTCdsp += TASmgs;
168900120307         MTCksc = TASksc;
169000120322         clear MTCdstp;
169100120307         exsr MemDati;
169200120307         write  TFMTC000;
169300120307         //?Scrivo il WF
169400120307         //?se non è richiamato da TNSB48
169500120307         IF  not wDaSB48;
169600120307           exsr wrtWF;
169700120307         ENDIF;
169800060112
169900120220       ENDSR;
170000120220
170100120220       //--------------------------------------------------------------
170200120220       //?Memorizzo i dati su TFMTC
170300120220       //--------------------------------------------------------------
170400120220       BEGSR MemDati;
170500120220
170600120220       //?Reperisco la provincia del codice tassazione
170700120220         clear savpro;
170800120220         kk = %lookup(TAScts:cts);
170900120220         IF  kk > 0;
171000120220           savpro = cpr(kk);
171100120220         ENDIF;
171200120220
171300120220         MTCpoc = Savpo;
171400120220         MTCcar = Savcar;
171500120220         MTCrag = Savrag;
171600120220         MTCage = Savage;
171700120220
171800120220       //?verifico se il campo colli originali da stampare è valorizzato
171900120220         IF  StpNcl <> 0 and StpNcl <> Tasncl;
172000120220           MTCNcl = StpNcl;
172100120322           %Subst(wMtcEsc:1:1) = 'o';
172200120220         ELSE;
172300120220           MTCncl = Tasncl;
172400120220         ENDIF;
172500120220
172600120220         MTCpkf = Taspkf;
172700120220         MTCvlf = Tasvlf;
172800120220         MTCfap = Tasfap;
172900120220         MTClna = Taslna;
173000120220         MTCcts = Tascts;
173100120220         MTCfin = Tasfin;
173200120220         MTCpro = Savpro;
173300120220         MTCtsp = Tastsp;
173400120220         MTCctr = Tasctr;
173500120220         MTCbap = Savbap;
173600120220         MTCdiv = Tasdiv;
173700120220         MTCpor = Taspor;
173800120220         MTCimv = Tasimv;
173900120220         MTCcas = Savcas;
174000120220         MTCvca = Savvca;
174100120220         MTCrmn = Tasrmn;
174200120220         MTCrma = Savrma;
174300120322         MTCerr = wMTCerr;
174400120322         MTCesc = wMTCesc;
174500120322         MTCimp = wMTCimp;
174600120322         MTCias = wMTCias;
174700120322         MTCvas = wMTCvas;
174800120322         MTCass = wMTCass;
174900120322         MTCinm = wMTCinm;
175000120220
175100120220       ENDSR;
175200120228
175300120228       //--------------------------------------------------------------
175400120228       //?Scrivo il file WFTMC
175500120228       //--------------------------------------------------------------
175600120228       BEGSR wrtWF;
175700120329
175800120329       //?Se errore IMP ed elaborazione giornaliera non la scrivo sul WF
175900120329       //?così non viene stampata tutti i giorni
176000120329         IF  wMTCtrc = 'M' and wMTCimp = 'IMP' and d44gio = 'S';
176100120329           leavesr;
176200120329         ENDIF;
176300120228
176400120228       //?Reperisco la provincia del codice tassazione
176500120228         clear savpro;
176600120228         kk = %lookup(TAScts:cts);
176700120228         IF  kk > 0;
176800120228           savpro = cpr(kk);
176900120228         ENDIF;
177000120228
177100120228         MTCdel = dateu;
177200120228         MTCksc = TASksc;
177300120228         MTCpoc = Savpo;
177400120228         MTCcar = Savcar;
177500120228         MTCage = Savage;
177600120228         MTCrag = Savrag;
177700120228         MTCaas = TASaas;
177800120228         MTClnp = TASlnp;
177900120228         MTCnrs = TASnrs;
178000120228         MTCnsp = TASnsp;
178100120228         MTCtbl = TAStbl;
178200120228         MTCDSP = TASaas * 10000;
178300120228         MTCdsp += TASmgs;
178400120228
178500120228       //?verifico se il campo colli originali da stampare è valorizzato
178600120228         IF  StpNcl <> 0 and StpNcl <> Tasncl;
178700120228           MTCNcl = StpNcl;
178800120322           %Subst(wMtcEsc:1:1) = 'o';
178900120228         ELSE;
179000120228           MTCncl = Tasncl;
179100120228         ENDIF;
179200120228
179300120228         MTCpkf = Taspkf;
179400120228         MTCvlf = Tasvlf;
179500120228         MTCfap = Tasfap;
179600120228         MTClna = Taslna;
179700120228         MTCcts = Tascts;
179800120228         MTCfin = Tasfin;
179900120228         MTCpro = Savpro;
180000120228         MTCtsp = Tastsp;
180100120228         MTCctr = Tasctr;
180200120228         MTCbap = Savbap;
180300120228         MTCdiv = Tasdiv;
180400120228         MTCpor = Taspor;
180500120228         MTCimv = Tasimv;
180600120228         MTCcas = Savcas;
180700120228         MTCvca = Savvca;
180800120228         MTCrmn = Tasrmn;
180900120228         MTCrma = Savrma;
181000120322         MTCerr = wMTCerr;
181100120322         MTCesc = wMTCesc;
181200120322         MTCimp = wMTCimp;
181300120322         MTCias = wMTCias;
181400120322         MTCvas = wMTCvas;
181500120322         MTCass = wMTCass;
181600120322         MTCinm = wMTCinm;
181700120322
181800120228         write  WFMTC000;
181900120228
182000120228       ENDSR;
182100120220
182200120220      /end-free
182300060126
182400060126      *------------------------------------------------------------------------*
182500060126      * Stampa manca tariffa
182600060126      *------------------------------------------------------------------------*
182700060126     c     SR_Stampa     BegSr
182800060206
182900060206      * richiamo il programma di stampa
183000060206     c                   clear                   kpjbu
183100060206     c                   movel     tnsb44ds      kpjbu
183200060206     c                   Call      'TNSB46R'
183300060206     c                   Parm                    Kpjba
183400060206
183500060206     c                   Endsr
183600120220
183700120220       //--------------------------------------------------------------
183800120220       //?Copio il file dalla QTEMP alla BUONA
183900120220       //--------------------------------------------------------------
184000120220     c     Copia_File    BEGSR
184100120220
184200120220      *?Chiudo TFMTC01L in QTEMP
184300120220     c                   close     TFMTC01L
184400121003
184500121003      /free
184600121003       //?Cancello i dati presenti sul file TFMTC00F della BUONA
184700121003       //?lo faccio con SQL così anche se impegnato vado avanti lo stesso
184800121003         IF  not wProva;
184900121003           exec sql
185000121003             DELETE  from  UNITRAGRU/TFMTC00F;
185100121003         ELSE;
185200121003           exec sql
185300121003             DELETE  from  UNITRAGRP/TFMTC00F;
185400121003         ENDIF;
185500121003      /end-free
185600120220      *?Copio il file TFMTC00F dalla QTEMP alla BUONA
185700120220     c                   clear                   comman
185800120220     c                   movel(p)  cmdc(3)       comman
185900120220     c                   Eval      %Subst(comman:38:9) = wlibc
186000120220     c                   call      'QCMDEXC'
186100120220     c                   parm                    comman
186200120220     c                   parm                    lenght
186300120220
186400120220     c                   ENDSR
186500120402
186600120402       //--------------------------------------------------------------
186700120402       //?Salvo il file WFMTC00F in un nuovo membro
186800120402       //--------------------------------------------------------------
186900120402     c     Salva_FileWF  BEGSR
187000120402
187100120402      *?Chiudo WFMTC00F in QTEMP
187200120402     c                   close     WFMTC00F
187300120402      *?Salvo il membro
187400120402     c                   clear                   comman
187500120402     c                   movel(p)  cmdw(5)       comman
187600120402     c                   Eval      %Subst(comman:64:10) = D44mbr
187700120402     c                   call      'QCMDEXC'
187800120402     c                   parm                    comman
187900120402     c                   parm                    lenght
188000120402
188100120402     c                   ENDSR
188200130108
188300130108      /free
188400130108       //--------------------------------------------------------------
188500130109       //?Bolle già contabilizzate o prepagati per Fatturato Giornaliero.
188600130109       //?dati da TITAS.
188700130108       //--------------------------------------------------------------
188800130109       BEGSR  FatGioTas;
188900130108
189000130108       //?Visto che bolla già contabilizzata non devo controllare niente
189100130108       //?e nemmeno tassare, prendo i dati da TITAS
189200130108       //?valorizzando le DS DTAS e DTASV
189300130108         clear dTASv;
189400130108         TSV(1) = TASsv1;
189500130108         TVA(1) = TASva1;
189600130108         TSV(2) = TASsv2;
189700130108         TVA(2) = TASva2;
189800130108         TSV(3) = TASsv3;
189900130108         TVA(3) = TASva3;
190000130108       //?Se la terza varia è piena cerco su TITA7 eventuali altre varie
190100130109         IF  TASsv3 <> *blanks;
190200130108           kk = 4;
190300130108           setll (TASaas:TASlnp:TASnrs:TASnsp:TAStbl) TITA730C;
190400130108           reade (TASaas:TASlnp:TASnrs:TASnsp:TAStbl) TITA730C;
190500130108           DOW  not %eof(TITA730C) and kk < 20;
190600130108             kk += 1;
190700130108             TSV(kk) = TA7svn;
190800130108             TVA(kk) = TA7van;
190900130108             reade (TASaas:TASlnp:TASnrs:TASnsp:TAStbl) TITA730C;
191000130108           ENDDO;
191100130108         ENDIF;
191200130108
191300130108         clear dTAS;
191400130108         §TASksc = TASksc;
191500130108         §TASdsp = TASaas * 10000 + TASmgs;
191600130108         §TASpor = TASpor;
191700130108         §TASimv = TASimv;
191800130109         wTassa = *on;
191900130108
192000130108         exsr agg_TFFGC;
192100130108
192200130108       ENDSR;
192300130109
192400130109       //--------------------------------------------------------------
192500130109       //?Scrivo/Aggiorno fatturato giornaliero.
192600130109       //--------------------------------------------------------------
192700130109       BEGSR  agg_TFFGC;
192800130109
192900130109         wFIL = %dec(%subst(%editc(§TASksc:'X'):1:3):3:0);
193000160121         chain (§TASdsp:wFIL) TFFGC01L;
193100130109
193200130109       //?Se c'è già un rcd aggiorno con i dati variabili
193300160121         IF  %found(TFFGC01L);
193400130109           exsr imposta_TFFGC;
193500130109           update TFFGC000;
193600130109         ENDIF;
193700130109
193800130109       //?Se rcd nuovo prima imposto i dati fissi + key poi i dati variabili
193900160121         IF  not %found(TFFGC01L);
194000130109           clear TFFGC000;
194100130109           FGCDATA = §TASdsp;
194200130109           FGCFIL  = wFIL;
194300130109
194400130109         //?valorizzo area e distretto + decodifiche
194500130109           chain wFIL AZORG01L;
194600130109           IF  %found(AZORG01L);
194700130109             FGCfild = ORGdes;
194800130109             FGCdit  = ORGfl3;
194900130109             FGCare  = ORGcar;
195000130109           ENDIF;
195100130109           clear ds05;
195200130109           TABcod = '05';
195300130109           TABkey = %editc(FGCare:'X');
195400130109           chain (codut:TABcod:TABkey) TABEL00F;
195500130109           IF  %found(TABEL00F);
195600130109             ds05 = TBLuni;
195700130109           ENDIF;
195800130109           FGCared = §05des;
195900130109           clear ds17;
196000130109           TABcod = '17';
196100130109           TABkey = FGCdit;
196200130109           chain (codut:TABcod:TABkey) TABEL00F;
196300130109           IF  %found( TABEL00F );
196400130109             ds17 = TBLuni;
196500130109           ENDIF;
196600130109           FGCditd = §17des;
196700130201
196800130201           FGCdtela = dateu;
196900130109
197000130109         //?valorizzo i dati variabili
197100130109           exsr imposta_TFFGC;
197200130109           write TFFGC000;
197300130109         ENDIF;
197400130109
197500130109       ENDSR;
197600130109
197700130109       //--------------------------------------------------------------
197800130109       //?Imposto dati per il file Fatturato Giornaliero.
197900130109       //--------------------------------------------------------------
198000130109       BEGSR  imposta_TFFGC;
198100130109
198200130109       //?spedizioni tassate
198300130109         IF  wTassa;
198400130109           FGCtost += 1;
198500130109         ENDIF;
198600130109
198700130109       //?spedizioni manca tariffa
198800130109         IF  wManca;
198900130109           FGCtosm += 1;
199000130109         ENDIF;
199100130109
199200130109       //?porto e imponibile
199300130109         FGCtpor += §TASpor;
199400130109         FGCtimp += §TASimv;
199500130109
199600130109       //?varie
199700130109         kk = 1;
199800130109         FOR kk by 1 to %elem(TSV);
199900130109           SELECT;
200000130109             WHEN  TSV(kk) = '2 ';
200100130109               FGCva2 += TVA(kk);
200200130109             WHEN  TSV(kk) = '5 ';
200300130109               FGCva5 += TVA(kk);
200400130109             WHEN  TSV(kk) = '& ';
200500130109               FGCvapa += TVA(kk);
200600130109             WHEN  TSV(kk) = '* ';
200700130109               FGCvadir += TVA(kk);
200800130109             WHEN  TSV(kk) = '+ ';
200900130109               FGCvasca += TVA(kk);
201000130109             WHEN  TSV(kk) = '= ';
201100130109               FGCvarba += TVA(kk);
201200130109             WHEN  TSV(kk) = 'a ';
201300130109               FGCvaaa += TVA(kk);
201400130109             WHEN  TSV(kk) = 'A ';
201500130109               FGCvaa += TVA(kk);
201600130109             WHEN  TSV(kk) = 'b ';
201700130109               FGCvabb += TVA(kk);
201800130109             WHEN  TSV(kk) = 'B ';
201900130109               FGCvab += TVA(kk);
202000130109             WHEN  TSV(kk) = 'c ';
202100130109               FGCvacc += TVA(kk);
202200130109             WHEN  TSV(kk) = 'C ';
202300130109               FGCvac += TVA(kk);
202400130109             WHEN  TSV(kk) = 'd ';
202500130109               FGCvadd += TVA(kk);
202600130109             WHEN  TSV(kk) = 'D ';
202700130109               FGCvad += TVA(kk);
202800130109             WHEN  TSV(kk) = 'e ';
202900130109               FGCvaee += TVA(kk);
203000130109             WHEN  TSV(kk) = 'f ';
203100130109               FGCvaff += TVA(kk);
203200130109             WHEN  TSV(kk) = 'F ';
203300130109               FGCvaf += TVA(kk);
203400130109             WHEN  TSV(kk) = 'G ';
203500130109               FGCvag += TVA(kk);
203600130109             WHEN  TSV(kk) = 'h ';
203700130109               FGCvahh += TVA(kk);
203800130109             WHEN  TSV(kk) = 'H ';
203900130109               FGCvah += TVA(kk);
204000130109             WHEN  TSV(kk) = 'I ';
204100130109               FGCvai += TVA(kk);
204200130109             WHEN  TSV(kk) = 'J ';
204300130109               FGCvaj += TVA(kk);
204400130109             WHEN  TSV(kk) = 'K ';
204500130109               FGCvak += TVA(kk);
204600130109             WHEN  TSV(kk) = 'L ';
204700130109               FGCval += TVA(kk);
204800130109             WHEN  TSV(kk) = 'm ';
204900130109               FGCvamm += TVA(kk);
205000130109             WHEN  TSV(kk) = 'M ';
205100130109               FGCvam += TVA(kk);
205200130109             WHEN  TSV(kk) = 'N ';
205300130109               FGCvan += TVA(kk);
205400130109             WHEN  TSV(kk) = 'O ';
205500130109               FGCvao += TVA(kk);
205600130109             WHEN  TSV(kk) = 'P ';
205700130109               FGCvap += TVA(kk);
205800130109             WHEN  TSV(kk) = 'Q ';
205900130109               FGCvaq += TVA(kk);
206000130109             WHEN  TSV(kk) = 'R ';
206100130109               FGCvar += TVA(kk);
206200130109             WHEN  TSV(kk) = 'S ';
206300130109               FGCvas += TVA(kk);
206400130109             WHEN  TSV(kk) = 'T ';
206500130109               FGCvat += TVA(kk);
206600130109             WHEN  TSV(kk) = 'U ';
206700130109               FGCvau += TVA(kk);
206800130109             WHEN  TSV(kk) = 'W ';
206900130109               FGCvaw += TVA(kk);
207000130109             WHEN  TSV(kk) = 'X ';
207100130109               FGCvax += TVA(kk);
207200130109             WHEN  TSV(kk) = 'Y ';
207300130109               FGCvay += TVA(kk);
207400130109             WHEN  TSV(kk) = 'Z ';
207500130109               FGCvaz += TVA(kk);
207600160121             WHEN  TSV(kk) = 't ';
207700160121               FGCvapr += TVA(kk);
207800160121             WHEN  TSV(kk) = 'k ';
207900160121               FGCvapk += TVA(kk);
208000130109           ENDSL;
208100130109         ENDFOR;
208200130109
208300130109       ENDSR;
208400130108
208500130108      /end-free
208600060112
208700060109      *------------------------------------------------------------------------*
208800060109      * IMPOSTAZIONI INIZIALI
208900060109      *------------------------------------------------------------------------*
209000060109     c     SR_Imposta    BegSr
209100060109
209200060105      * Utente di sede
209300060110     c                   If        DutPou = 46
209400060105     c                   Eval      *In05 = *On
209500060110     c                   EndIf
209600060203
209700060203      * verifico se devo stampare il totale imponibile
209800060203     c                   eval      *in20 = (d44fsi = 'S')
209900060203
210000060105
210100060105     c                   SELECT
210200060105      *
210300060105      * se codice cliente valorizzato valorizzo il p.o. da elaborare con quello del cliente
210400060119     c                   When      D44cli <> *zeros
210500120416     c                   eval      skpo(1) = %subst(%editc(d44cli:'X'):1:3)
210600060105      *
210700060105      * verifico se valorizzata la sola linea valorizzo i p.o. da elaborare se stessa e la sua  £1
210800060105      *
210900060105     c                   When      D44Fil <> *zeros
211000060105     c                   clear                   TRUL31DS
211100060105     c                   eval      I31abi = 'PO'
211200060105     c                   eval      I31cpo = D44Fil
211300060105     c                   call      'TRUL31R'
211400060105     c                   parm                    KPJBA
211500060105     c                   parm                    TRUL31DS
211600060111     c                   if        O31pog > *zeros
211700060111     c                   movea     O31pog        Skpo
211800060111     c                   else
211900060119     c                   movel     D44fil        Skpo(1)
212000060111     c                   endif
212100060105
212200060105      *
212300060201      * verifico se valorizzata l'area
212400060105      *
212500060201     c                   When      D44Are <> *zeros
212600060201      * Reperimento dei P.O. dell'area
212700060105     c                   clear                   TRUL31DS
212800060201     c                   eval      I31abi = 'RA'
212900060201     c                   eval      I31car = D44Are
213000060105     c                   call      'TRUL31R'
213100060105     c                   parm                    KPJBA
213200060105     c                   parm                    TRUL31DS
213300060111     c                   if        O31pog > *zeros
213400060111     c                   movea     O31pog        Skpo
213500060111     c                   endif
213600060105
213700060109     c                   other
213800060109      * Reperimento dei P.O. dell'Azienda
213900060109     c                   clear                   TRUL31DS
214000060109     c                   eval      I31abi = 'AZ'
214100060109     c                   call      'TRUL31R'
214200060109     c                   parm                    KPJBA
214300060109     c                   parm                    TRUL31DS
214400060111     c                   if        O31pog > *zeros
214500060111     c                   movea     O31pog        Skpo
214600060111     c                   endif
214700060109
214800060111     c                   endsl
214900060109
215000060109      * Recupero la moneta di conto
215100060109     c                   clear                   Tibs02ds
215200060109     c                   clear                   Dged
215300060109
215400060109     c                   Movel     'C'           T02mod
215500060109     c                   Movel     KNSIF         T02sif
215600060109     c                   Movel     'GED'         T02cod
215700060109     c                   Movel     '1'           T02ke1
215800060109      *
215900060109     c                   Call      'TIBS02R'
216000060109     c                   Parm                    Kpjba
216100060109     c                   Parm                    Tibs02ds
216200060109      *
216300060109     c                   If        T02err = *blanks
216400060109     c                   Movel     T02uni        Dged
216500060109     c                   Endif
216600060109
216700060109      * Ricerca degli importi standard nella moneta di conto
216800060109
216900060109     c                   Clear                   Tibs02ds
217000060109     c                   Clear                   Dgei
217100060109     c                   Movel     'L'           T02tla
217200060109     c                   Movel     'C'           T02mod
217300060109     c                   Movel     KNSIF         T02sif
217400060109     c                   Movel     'GEI'         T02cod
217500060109     c                   Movel     §GEDCN        T02ke1
217600060109
217700060109     c                   Call      'TIBS02R'
217800060109     c                   Parm                    Kpjba
217900060109     c                   Parm                    Tibs02ds
218000060109
218100060109     c                   If        T02err =  *blanks
218200060109     c                   Movel     T02uni        Dgei
218300120328     c                   Endif
218400120328
218500120328      *?Cerco il limite imponibile + peso tabella IMP
218600120328     c                   clear                   TIBS02DS
218700120328     c                   clear                   dIMP
218800120328     c                   eval      T02tla = 'L'
218900120328     c                   eval      T02mod = 'C'
219000120328     c                   eval      T02sif = KNSIF
219100120328     c                   eval      T02cod = 'IMP'
219200120328     c                   eval      T02ke1 = '001'
219300120328     c                   call      'TIBS02R'
219400120328     c                   parm                    kpjba
219500120328     c                   parm                    TIBS02DS
219600120328     c                   IF        T02err = *blanks
219700120328     c                   eval      dIMP = T02uni
219800120328     c                   ENDIF
219900130108
220000130108      *?Cerco data ultima spedizione fatturata tabella RPF
220100130116     c                   IF        not wDaMenu and
220200130116     c                             not wDASB48
220300130108     c                   clear                   TIBS02DS
220400130108     c                   clear                   dRPFfatgio
220500130108     c                   eval      T02tla = 'L'
220600130108     c                   eval      T02mod = 'C'
220700130108     c                   eval      T02sif = KNSIF
220800130108     c                   eval      T02cod = 'RPF'
220900130108     c                   eval      T02ke1 = 'FATTGIO'
221000130108     c                   call      'TIBS02R'
221100130108     c                   parm                    kpjba
221200130108     c                   parm                    TIBS02DS
221300130108     c                   IF        T02err = *blanks
221400130108     c                   eval      dRPFfatgio = T02uni
221500130108     c                   ENDIF
221600130116     c                   ENDIF
221700060109
221800060109      * Carico tipi bolla
221900060109
222000060109     c                   Movel     'TB'          Tabcod
222100060112     c                   z-add     1             kk
222200060109     c     KeyTab2       setll     Tabel00f
222300060109     c                   Do        *hival
222400060109     c     KeyTab2       Reade     Tabel00f
222500060109
222600060109    1c                   If        %eof(Tabel00f)
222700060109     c                   leave
222800060109     c                   endif
222900060109
223000060109    1c                   If        tblflg = ' '
223100060112     c     kk            Occur     Dstb
223200060109     c                   Movel     Tbluni        Dstb
223300060112     c                   Movel     Tblkey        Tbo(kk)
223400060112     c                   add       1             kk
223500060109    2c                   endif
223600060109
223700060109    1c                   Enddo
223800060110
223900060110      * Carico le province dei codici tassazione
224000060110
224100060110     c                   Movel     'CT'          Tabcod
224200060112     c                   z-add     1             kk
224300060110     c     KeyTab2       setll     Tabel00f
224400060110     c                   Do        *hival
224500060110     c     KeyTab2       Reade     Tabel00f
224600060110
224700060110    1c                   If        %eof(Tabel00f)
224800060110     c                   leave
224900060110     c                   endif
225000060110
225100060110    1c                   If        tblflg = ' '
225200060110     c                   Movel     Tbluni        Dsct
225300060112     c                   Movel     Tblkey        Cts(kk)
225400060112     c                   Movel     §ctprv        Cpr(kk)
225500060112     c                   add       1             kk
225600060110    2c                   endif
225700060110
225800060110    1c                   Enddo
225900060110
226000060110      * carico cod.bolla che prevedono la tassazione di una sola varia
226100060110
226200060110     c                   Movel     '3A'          Tabcod
226300060112     c                   z-add     1             kk
226400060110     c     KeyTab2       setll     Tabel00f
226500060110     c                   Do        *hival
226600060110     c     KeyTab2       Reade     Tabel00f
226700060110
226800060110    1c                   If        %eof(Tabel00f)
226900060110     c                   leave
227000060110     c                   endif
227100060110
227200060110    1c                   If        tblflg = ' '
227300060110     c                   Movel     Tbluni        Ds3a
227400060110
227500060110     c                   If        §3asva <> *blanks
227600060112     c                   Movel     Tblkey        Cbo(kk)
227700060112     c                   Movel     §3asva        Cbv(kk)
227800060112     c                   add       1             kk
227900060110     c                   endif
228000060110
228100060110    2c                   endif
228200060110
228300060110    1c                   Enddo
228400060109
228500060119      * Ricerco il limite standard dell'importo da assicurare
228600060119     c                   Clear                   Tibs02ds
228700060119     c                   Clear                   Dlia
228800060119     c                   Eval      T02Mod = 'C'
228900060119     c                   Eval      T02Sif = Knsif
229000060119     c                   Eval      T02Cod = 'LIA'
229100060119     c                   Movel     '001'         T02Ke1
229200060119     c                   Call      'TIBS02R'
229300060119     c                   Parm                    Kpjba
229400060119     c                   Parm                    Tibs02ds
229500060119     c                   If        T02Err = *Blanks
229600060119     c                   Movel     T02Uni        Dlia
229700060119     c                   EndIf
229800060119
229900130108      /free
230000130108       //?Per Fatturato giornaliero
230100130108       //?devo controllare la data DAL con la data ultima spedizione fatturata
230200130108       //?se data DAL minore/uguale devo impostare come data DAL, per fatturato giornaliero,
230300130108       //?la data ultima spedizione fatturata + 1 gg
230400130108       //?come data AL va bene quella passata dal pgm di lancio
230500130116       IF  not wDaMenu and not wDASB48;
230600130116         IF  D44dsd <= §RPFfatdsp;
230700130116           wDataISO = %date(§RPFfatdsp);
230800130116           wDataISO = wDataISO + %days(1);
230900130116           wDataDa  = %dec(wDataISO);
231000130116         ELSE;
231100130116           wDataDa  = D44dsd;
231200130116         ENDIF;
231300130116         wDataAl  = D44dsa;
231400130116       ENDIF;
231500141211
231600141211       //?Carico part.consegna con abilitazione PinCode
231700141211         TABcod = '7R';
231800141211         clear kk;
231900141211         setll (Codut:TABcod) TABEL00F;
232000141211         reade (Codut:TABcod) TABEL00F;
232100141211         DOW  not %eof(TABEL00F);
232200141211           IF  TBLflg = *blanks;
232300141211             ds7R = TBLuni;
232400141211             IF  §7Rpincode = 'S';
232500141211               kk += 1;
232600141211               skGMA(kk) = TBLkey;
232700141211             ENDIF;
232800141211           ENDIF;
232900141211           reade (Codut:TABcod) TABEL00F;
233000141211         ENDDO;
233100141211
233200130108      /end-free
233300060105
233400060109     c                   endsr
233500060109      *------------------------------------------------------------------------*
233600060109      * ROUTINE INIZIALE
233700060109      *------------------------------------------------------------------------*
233800060109     c     *Inzsr        BegSr
233900060109
234000060109     c     *entry        plist
234100060109     c                   parm                    Kpjba
234200060119     c                   eval      Tnsb44ds = Kpjbu
234300060109      * Reperisco data corrente
234400060109     c                   time                    w0140
234500060109     c                   move      w0140         wData
234600060202
234700060109     c                   clear                   WLBdat
234800060109     c                   eval      G02dat = wData
234900060109     c                   call      'XSRDA8'
235000060109     c                   parm                    WLBdat
235100060109     c                   z-add     G02inv        DateU
235200060109
235300060109     c     *dtaara       define    §azute        azuteds
235400060109     c     *dtaara       define    §datiute      ddatiute
235500060109     c                   in(E)     *dtaara
235600060109     c                   If        %error  or RSUT = *blanks
235700060109     c                   CLEAR                   tibs34ds
235800060109     c                   CALL      'TIBS34R'
235900060109     c                   PARM                    tibs34ds
236000060109     c                   in        *dtaara
236100060109     c                   EndIf
236200060120
236300060206      * Se sono in ambiente buono - GAITRAGRU
236400060208     c                   If        knsif = 'FILTRA201 ' or
236500060208     c                             knsif = 'GAITRA201 '
236600121003     c                   eval      wProva = *off
236700060206     c                   Eval      wlib = 'GAITRAGRU '
236800120220     c                   Eval      wlibc= 'UNITRAGRU'
236900120313     c                   Eval      wlibw= 'GAITRAAZM'
237000060206      *  Se sono in ambiente di prova - GAITRAGRPS
237100060206     c                   Else
237200121003     c                   eval      wProva = *on
237300060206     c                   Eval      wlib = 'GAITRAGRPS'
237400121003     c                   Eval      wlibc= 'UNITRAGRP'
237500121003     c                   Eval      wlibw= 'GAITRAAZP'
237600060206     c                   EndIf
237700060120      * Se non sono in sede
237800060120     c                   If        Dutpou <> 46
237900060120
238000060120      *  titas38c
238100060120     c                   Clear                   comman
238200060120     c                   Movel(p)  cmdt(1)       comman
238300060120     c                   Eval      %Subst(comman:30:10) = wlib
238400060120     c                   Eval      comman =
238500060120     c                             %trim(comman) + '/TITAS38C)'
238600060120     c                   Call      'QCMDEXC'                            99
238700060120     c                   Parm                    comman
238800060120     c                   Parm                    lenght
238900060120     c  n99              Open      Titas38c
239000060120      *  titas02l
239100060120     c                   Clear                   comman
239200060120     c                   Movel(p)  cmdt(2)       comman
239300060120     c                   Eval      %Subst(comman:30:10) = wlib
239400060120     c                   Eval      comman =
239500060120     c                             %trim(comman) + '/TITAS02L)'
239600060120     c                   Call      'QCMDEXC'                            99
239700060120     c                   Parm                    comman
239800060120     c                   Parm                    lenght
239900060120     c  n99              Open      Titas02l
240000060120      *  tita730c
240100060120     c                   Clear                   comman
240200060120     c                   Movel(p)  cmdt(3)       comman
240300060120     c                   Eval      %Subst(comman:30:10) = wlib
240400060120     c                   Eval      comman =
240500060120     c                             %trim(comman) + '/TITA730C)'
240600060120     c                   Call      'QCMDEXC'                            99
240700060120     c                   Parm                    comman
240800060120     c                   Parm                    lenght
240900060120     c  n99              Open      Tita730c
241000060120      *  tita430c
241100060120     c                   Clear                   comman
241200060120     c                   Movel(p)  cmdt(4)       comman
241300060120     c                   Eval      %Subst(comman:30:10) = wlib
241400060120     c                   Eval      comman =
241500060120     c                             %trim(comman) + '/TITA430C)'
241600060120     c                   Call      'QCMDEXC'                            99
241700060120     c                   Parm                    comman
241800060120     c                   Parm                    lenght
241900060120     c  n99              Open      Tita430c
242000060120      *  tncsb03l
242100060120     c                   Clear                   comman
242200060120     c                   Movel(p)  cmdt(5)       comman
242300060120     c                   Eval      %Subst(comman:30:10) = wlib
242400060120     c                   Eval      comman =
242500060120     c                             %trim(comman) + '/TNCSB03L)'
242600060120     c                   Call      'QCMDEXC'                            99
242700060120     c                   Parm                    comman
242800060120     c                   Parm                    lenght
242900060120     c  n99              Open      Tncsb03l
243000060120      *  Fiar531c
243100060120     c                   Clear                   comman
243200060120     c                   Movel(p)  cmdt(6)       comman
243300060120     c                   Eval      %Subst(comman:30:10) = wlib
243400060120     c                   Eval      comman =
243500060120     c                             %trim(comman) + '/FIAR531C)'
243600060120     c                   Call      'QCMDEXC'                            99
243700060120     c                   Parm                    comman
243800060120     c                   Parm                    lenght
243900060120     c  n99              Open      Fiar531c
244000060120      *  titaa30c
244100060120     c                   Clear                   comman
244200060120     c                   Movel(p)  cmdt(7)       comman
244300060120     c                   Eval      %Subst(comman:30:10) = wlib
244400060120     c                   Eval      comman =
244500060120     c                             %trim(comman) + '/TITAA30C)'
244600060120     c                   Call      'QCMDEXC'                            99
244700060120     c                   Parm                    comman
244800060120     c                   Parm                    lenght
244900060120     c  n99              Open      Titaa30c
245000060123
245100060123      * se sono in sede faccio solo open dei file
245200060123
245300060123     c                   else
245400060123
245500060123     c                   Open      Titas38c
245600060123     c                   Open      Titas02l
245700060123     c                   Open      Tita730c
245800060123     c                   Open      Tita430c
245900060123     c                   Open      Tncsb03l
246000060123     c                   Open      Fiar531c
246100060123     c                   Open      Titaa30c
246200060123
246300060123     c                   EndIf
246400120306
246500120306      *?Controllo se è lancio da menù
246600120308     c                   eval      wDaMenu = (D44gio = 'M')
246700120306
246800120306      *?Controllo se è lancio da TNSB48R Gestione Manca Tariffa Tasto F5-Ritassa
246900120306     c                   eval      wDaSB48 = (D44SB48 = 'S')
247000120228      *
247100120228      * file di lavoro wfmtc se sono in filiale duplico in QTEMP
247200120306      *?e se NON richiamato da TNSB48
247300120228     c                   If        1 = 1
247400120306     c                             and not wDaSB48
247500120228     c                   clear                   comman
247600120228     c                   movel(p)  cmdw(1)       comman
247700120313     c                   Eval      %Subst(comman:15:9) = wlibw
247800120228     c                   call      'QCMDEXC'
247900120228     c                   parm                    comman
248000120228     c                   parm                    lenght
248100120228
248200120228     c                   clear                   comman
248300120228     c                   movel(p)  cmdw(2)       comman
248400120313     c                   Eval      %Subst(comman:34:9) = wlibw
248500120228     c                   call      'QCMDEXC'
248600120228     c                   parm                    comman
248700120228     c                   parm                    lenght
248800120228
248900120228     c                   clear                   comman
249000120228     c                   movel(p)  cmdw(3)       comman
249100120228     c                   call      'QCMDEXC'
249200120228     c                   parm                    comman
249300120228     c                   parm                    lenght
249400120228
249500120228     c                   endif
249600120228      * apro il file di comodo per la stampa
249700120306      *?se NON richiamato da TNSB48
249800120306     c                   IF        not wDaSB48
249900120228     c                   open      wfmtc00f
250000120306     c                   ENDIF
250100120228
250200120222      *?Se è lancio da menù devo duplicare il file in QTEMP VUOTO
250300120222     c                   IF        wDaMenu
250400120222     c                   clear                   comman
250500120222     c                   movel(p)  cmdc(4)       comman
250600120308     c                   Eval      %Subst(comman:34:9) = wlibc
250700120222     c                   call      'QCMDEXC'
250800120222     c                   parm                    comman
250900120222     c                   parm                    lenght
251000120222      *?Se è schedulato devo copiare il file in QTEMP PIENO
251100120222     c                   ELSE
251200120306      *?se NON richiamato da TNSB48
251300120306     c                   IF        not wDaSB48
251400120222      *?Copio il file TFMTC00F in QTEMP
251500120217     c                   clear                   comman
251600120217     c                   movel(p)  cmdc(1)       comman
251700120217     c                   Eval      %Subst(comman:15:9) = wlibc
251800120217     c                   call      'QCMDEXC'
251900120217     c                   parm                    comman
252000120217     c                   parm                    lenght
252100120306     c                   ENDIF
252200120222     c                   ENDIF
252300120222
252400120217      *?Duplico TFMTC01L in QTEMP
252500120306      *?se NON richiamato da TNSB48
252600120306     c                   IF        not wDaSB48
252700120217     c                   clear                   comman
252800120217     c                   movel(p)  cmdc(2)       comman
252900120217     c                   Eval      %Subst(comman:34:9) = wlibc
253000120217     c                   call      'QCMDEXC'
253100120217     c                   parm                    comman
253200120217     c                   parm                    lenght
253300120306     c                   ENDIF
253400120306      *?Apro TFMTC01L
253500120217     c                   open      TFMTC01L
253600120217
253700120217      /free
253800120217         //?Calcolo oggi - 60 gg.
253900120221           dataiso = %date(dateu);
254000120221           dataiso = dataiso - %days(60);
254100120221           wOggi_60 = %dec(dataiso);
254200120217      /end-free
254300120217
254400060202      *
254500060105      *
254600060105      *------------------------------------------------------------------------*
254700060105      * KLIST
254800060105      *------------------------------------------------------------------------*
254900060105
255000060105      *---- File Cnaco00f
255100060105
255200060105     c     KeyAco        Klist
255300060105     c                   Kfld                    Codut
255400060105     c                   Kfld                    DutKci
255500060105     c                   Kfld                    TasKsc
255600060105
255700060105      *---- File Titas38c
255800060105
255900060111     c     KeyTas38      Klist
256000060105     c                   Kfld                    KTasAas
256100060105     c                   Kfld                    KTasMgs
256200060113
256300060111      *---- File Titas02l
256400060111
256500060111     c     KeyTas02      Klist
256600060111     c                   Kfld                    KTasksc
256700060113
256800060113      *---- File Tita730C
256900060113
257000060113     c     KeyTa730      Klist
257100060113     c                   Kfld                    Tasaas
257200060113     c                   Kfld                    Taslnp
257300060113     c                   Kfld                    Tasnrs
257400060113     c                   Kfld                    Tasnsp
257500060113     c                   Kfld                    Tastbl
257600060119
257700060119      *---- File Tita430C
257800060119
257900060119     c     KeyTa430      Klist
258000060119     c                   Kfld                    Tasaas
258100060119     c                   Kfld                    Taslnp
258200060119     c                   Kfld                    Tasnrs
258300060119     c                   Kfld                    Tasnsp
258400060119     c                   Kfld                    Keytrc
258500060113
258600060113      *---- File Tncsb03l
258700060113
258800060113     c     KeyTncsb      Klist
258900060113     c                   Kfld                    Tasaas
259000060113     c                   Kfld                    Taslnp
259100060113     c                   Kfld                    Tasnrs
259200060113     c                   Kfld                    Tasnsp
259300060113
259400060113      *---- File Fiar531c
259500060113
259600060113     c     keyFiar5      Klist
259700060113     c                   Kfld                    TasAas
259800060113     c                   Kfld                    TasLnp
259900060113     c                   Kfld                    TasNrs
260000060113     c                   Kfld                    TasNsp
260100060113     c                   Kfld                    kAr5Trd
260200060113
260300060113      *---- File Fnevb00f
260400060113
260500060113     c     KeyEvb        Klist
260600060113     c                   Kfld                    Tasaas
260700060113     c                   Kfld                    Taslnp
260800060113     c                   Kfld                    Tasnrs
260900060113     c                   Kfld                    Tasnsp
261000060113     c                   Kfld                    Wcev
261100060109
261200060109      *---- File Tabel00f
261300060109
261400060109     c     KeyTab        Klist
261500060109     c                   Kfld                    Codut
261600060109     c                   Kfld                    Tabcod
261700060109     c                   Kfld                    Tabkey
261800060109
261900060109     c     KeyTab2       Klist
262000060109     c                   Kfld                    Codut
262100060109     c                   Kfld                    Tabcod
262200060105      *
262300060105     c                   EndSr
262400960924**   ERR
262500060120Manca Anagrafica Cliente
262600060120Anagrafica Cliente Annullata
262700060120Manca sottoconto Intest.Fattura
262800060120Sottoconto Intest.Fatt. Annullato
262900060117Tariffa valida solo in ARRIVO
263000060120Manca Tariffa
263100060120Imp.Maggiore del limite Fatturabile
263200060120Tipo fattura errato
263300060117Tariffa valida solo in PARTENZA
263400060117Tariffa BLOCCATA
263500060120Imp. d'assicurare maggiore del limite
263600060120Manca codice cliente
263700120322Peso/Collo superiore 5 volte media del cliente
263800120326Imponibile maggiore di EUR xxxxxxxxxx,xxx
263900120907Imponibile a ZERO
264000050711** cmd ovrprtf
264100050712OVRPRTF PRTF1982 OUTQ(xxxxxxxxxx) USRDTA('MancaTarif') SAVE(*YES) HOLD(*YES)       1
264200060120** cmdt ovrdbf
264300060120OVRDBF FILE(TITAS38C) TOFILE(
264400060120OVRDBF FILE(TITAS02L) TOFILE(
264500060120OVRDBF FILE(TITA730C) TOFILE(
264600060120OVRDBF FILE(TITA430C) TOFILE(
264700060120OVRDBF FILE(TNCSB03L) TOFILE(
264800060120OVRDBF FILE(FIAR531C) TOFILE(
264900060120OVRDBF FILE(TITAA30C) TOFILE(
265000060120** cmdc crtdupobj
265100120217CPYF FROMFILE(         /TFMTC00F) TOFILE(QTEMP/TFMTC00F) CRTFILE(*YES)
265200120217CRTDUPOBJ OBJ(TFMTC01L) FROMLIB(          ) OBJTYPE(*FILE) TOLIB(QTEMP)
265300121003CPYF FROMFILE(QTEMP/TFMTC00F) TOFILE(         /TFMTC00F) MBROPT(*ADD)
265400120222CRTDUPOBJ OBJ(TFMTC00F) FROMLIB(          ) OBJTYPE(*FILE) TOLIB(QTEMP)
265500120224DLTF FILE(QTEMP/TFMTC*)
265600120228** cmdw crtdupobj
265700120228CPYF FROMFILE(         /WFMTC00F) TOFILE(QTEMP/WFMTC00F) FROMMBR(WFMTC00F) CRTFILE(*YES)
265800120228CRTDUPOBJ OBJ(WFMTC01L) FROMLIB(          ) OBJTYPE(*FILE) TOLIB(QTEMP)
265900120228CLRPFM FILE(QTEMP/WFMTC00F)
266000120229DLTF FILE(QTEMP/WFMTC*)
266100120402CPYF FROMFILE(QTEMP/WFMTC00F) TOFILE(GAITRAAZM/WFMTC00F) TOMBR(          ) MBROPT(*REPLACE)
