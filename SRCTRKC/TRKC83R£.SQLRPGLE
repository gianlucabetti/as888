000100150326      //---------------------------------------------------------------
000200150401      //?TRKC83R - Elimina i clienti che sono diventati figli
000300150326      //---------------------------------------------------------------
000400150326     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000500150326     h dftactgrp(*no) actgrp(*caller)
000600150326
000700150326      //---------------------------------------------------------------
000800150326      //?Dichiarazione file.
000900150326      //---------------------------------------------------------------
001000161123     fTICMI01L  if   e           k disk
001100150408
001200150408     fTRKC83P   o    e             printer oflind(*in90)
001300150326
001400150326      //---------------------------------------------------------------
001500150326      //?Definizione costanti.
001600150326      //---------------------------------------------------------------
001700150326     d FaseObjFine     c                   const(' 30')
001800150327     d FaseObjTf       c                   const(' TF')
001900150327     d FaseObjTtr      c                   const(' TR')
002000150326
002100150326      //---------------------------------------------------------------
002200150326      //?Definizione schiere.
002300150326      //---------------------------------------------------------------
002400150326
002500150326      //---------------------------------------------------------------
002600150326      //?Definizione aree dati.
002700150326      //---------------------------------------------------------------
002800150326      // - Dati utente
002900150326     d §AzUte        e ds                  extname(AZUTE00F)
003000150326     d                                     dtaara
003100150326     d §DatiUte      e ds                  extname(dDatiUte)
003200150326     d                                     dtaara
003300150326
003400150326      //---------------------------------------------------------------
003500150326      //?Definizione strutture dati.
003600150326      //---------------------------------------------------------------
003700150326      // - Parametri ricevuti
003800150326     d KPJBA         e ds
003900150326
004000150326      // - Ricerca Unificante Padre?
004100150326     d TIBS10DS      e ds                  inz
004200150326     d  sk_D10skc             21   5520  0 inz  dim(500)
004300150326
004400150326      // - Reperimento dati utente
004500150326     d TIBS34DS      e ds
004600161123
004700161123      // - Reperimento dati Anagrafica Clienti
004800161123      /copy gaitrasrc/srcprotopi,TIBS69R
004900150326
005000150326      // - File TICMC00F
005100150326     d TICMC00F      e ds                  extname(TICMC00F)
005200150401
005300150401      // - Cancella clienti in campagna
005400150401     d TRKC77DS      e ds
005500150326
005600150326      //---------------------------------------------------------------
005700150326      //?Definizione variabili globali.
005800150326      //---------------------------------------------------------------
005900150326      // - Flags booleani
006000150408     d Stampa          s               n   inz(*off)
006100150326     d wEoF            s               n   inz(*off)
006200150408     d wTesta          s               n   inz(*off)
006300150326
006400150326      // - Indice di schiera
006500150326     d xx              s              3s 0 inz
006600150326
006700150326      // - Campi di comodo
006800161123     d CampagnaA       s                   like(CMIncm) inz(5)
006900161123     d CampagnaB       s                   like(CMIncm) inz(6)
007000161124     d Data_EUR        s               d   datfmt(*eur)
007100161124     d Data_ISO        s               d   datfmt(*iso)
007200150326     d Oggi            s              8s 0 inz
007300150326
007400150326      //---------------------------------------------------------------
007500150326      //?Definizione Procedure usate.
007600150326      //---------------------------------------------------------------
007700150401      // - Cancello clienti in campagna
007800150401     d TRKC77R         pr                  extpgm('TRKC77R')
007900150401     d  kpjba                              likeds(kpjba)
008000150401     d  trkc77ds                           likeds(TRKC77DS)
008100150326
008200150326      //---------------------------------------------------------------
008300150326      //?Definizione Prototipi.
008400150326      //---------------------------------------------------------------
008500150326      /copy gaitrasrc/srcprotopr,TIBS10R
008600150326      /copy gaitrasrc/srcprotopr,TIBS34R
008700161123      /copy gaitrasrc/srcprotopr,TIBS69R
008800150326      /copy gaitrasrc/srcprotopr,SYSTEM
008900150326
009000150326      //---------------------------------------------------------------
009100150326      //?Definizione key-list.
009200150326      //---------------------------------------------------------------
009300150326
009400150326      //---------------------------------------------------------------
009500150326      //?M A I N - L I N E
009600150326      //---------------------------------------------------------------
009700150326
009800150326     c     *Entry        plist
009900150326     c                   parm                    kpjba
010000150326
010100150326      /free
010200150326
010300150326       //?Operazioni iniziali
010400150326       exsr RoutInz;
010500150326
010600150326       //?Elaborazione
010700150326       exsr Elabora;
010800150326
010900150326       //?Operazioni finali
011000150326       exsr RoutEnd;
011100150326
011200150326       //--------------------------------------------------------------
011300150326       //?Operazioni iniziali.
011400150326       //--------------------------------------------------------------
011500150326       BEGSR RoutInz;
011600150326
011700150326         exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
011800150408
011900150408       //?Stampa
012000150408         IF  %subst(kpjbu:1:2) = 'SI';
012100150408           Stampa = *on;
012200150408         ELSE;
012300150408           Stampa = *off;
012400150408         ENDIF;
012500150326
012600150326       //?Imposto oggi
012700150326         Oggi = %dec(%date());
012800150326
012900150326       //?Reperimento dati job
013000150326         exsr DatiJob;
013100150326
013200150326       ENDSR;
013300150326
013400150326       //--------------------------------------------------------------
013500150326       //?Reperimento Dati del job (Utente/Operativi).
013600150326       //--------------------------------------------------------------
013700150326       BEGSR DatiJob;
013800150326
013900150326         in(E) §AzUte;
014000150326         IF  not %error;
014100150326           in(E) §DatiUte;
014200150326         ENDIF;
014300150326         IF  %error or RSut = *blanks;
014400150326           clear TIBS34ds;
014500150326           tibs34r(tibs34ds);
014600150326           in §AzUte;
014700150326           in §DatiUte;
014800150326         ENDIF;
014900150326
015000150326       ENDSR;
015100150326
015200150326       //--------------------------------------------------------------
015300150326       //?Elaborazione.
015400150326       //--------------------------------------------------------------
015500150326       BEGSR Elabora;
015600150326
015700150326       //?Dichiarazione cursore
015800150326         exec sql
015900150326         DECLARE CMC cursor for
016000150401         SELECT * from TICMC00F
016100150401         WHERE CMCufe <> ' 90'
016200161123           and (CMCncm = :CampagnaA or CMCncm = :CampagnaB);
016300150401
016400150326         //?Apertura del cursore
016500150326         exec sql open CMC;
016600150326
016700150326         IF sqlcode < 0;
016800150326           exec sql close CMC;
016900150326           leavesr;
017000150326         ENDIF;
017100150326
017200150326         DOW  not wEoF;
017300150326           exec sql
017400150326           fetch next from CMC into :TICMC00F;
017500150326           IF  sqlcod = 100 or sqlcod < 0;
017600150326             wEoF = *on;
017700150326             leave;
017800150326           ENDIF;
017900150401
018000150401       //?Cerco il papa del codice in elaborazione
018100150401           clear TIBS10DS;
018200150401           D10tle = 'ST';
018300150401           D10paf = 'P';
018400150401           D10cod = CMCksu;
018500150401           D10drf = %dec(%date());
018600150401           GetKSCfamily ( TIBS10ds : 'A' );
018700150401       //?Se torna errore leggo nuovo KSU
018800150401           IF  D10err <> *blanks;
018900150401             iter;
019000150401           ENDIF;
019100150401       //?Se Papa uguale al KSU leggo nuovo KSU
019200150401           IF  D10cop = CMCksu;
019300150401             iter;
019400150401           ENDIF;
019500150401       //?Se arrivo qua vol dire che
019600150401       //?è un Unificante presente in campagna
019700150401       //?ma che è diventato figlio di un altro Unificante
019800150401       //?quindi lo devo cancellare
019900150408           IF  Stampa;
020000150408             exsr StampaDel;
020100150408           ELSE;
020200150401           clear TRKC77DS;
020300150401           IKC77ncm  = CMCncm;
020400150401           IKC77ksu  = CMCksu;
020500150401           IKC77ksc  = CMCksc;
020600150401           IKC77cpo  = CMCcpo;
020700150401           TRKC77R (kpjba:TRKC77DS);
020800150408           ENDIF;
020900150326
021000150326         ENDDO;
021100150326
021200150326         exec sql close CMC;
021300150326
021400150326       ENDSR;
021500150408
021600150408       //--------------------------------------------------------------
021700150408       //?Stampo i codici che dovrei eliminare.
021800150408       //--------------------------------------------------------------
021900150408       BEGSR StampaDel;
022000161124
022100161124         NomePgm = 'TRKC83R';
022200161124         Data_EUR = %date(Oggi:*ISO);
022300161124         DataEla = %dec(Data_EUR);
022400161124         Oraela = %dec(%time());
022500150408
022600150408       //?Testata
022700150408         IF  not wTesta or *in90;
022800150408           write TRKC83T;
022900150408           wTesta = *on;
023000150408           *in90 = *off;
023100150408         ENDIF;
023200161123
023300161123       //?Cerco ragione Sociale e importanza cliente Cliente in campagna
023400161123         clear TIBS69DS;
023500161123         clear ACOrag;
023600161123         clear CLPclv;
023700161123         I69kac = CMCksu;
023800161123         I69kcp = CMCksu;
023900161123         TIBS69R (tibs69ds:ds_cnaco:ds_cnind:ds_cnclp:ds_fncls);
024000161123         ragksc = ACOrag;
024100161123         clvksc = CLPclv;
024200161123       //?Cerco fatturato Cliente in campagna
024300161123         clear ricksc;
024400161123         chain (CMCncm:CMCksu) TICMI01L;
024500161123         IF  %found(TICMI01L);
024600161123           ricksc = CMIric;
024700161123         ENDIF;
024800161123
024900161123       //?Cerco ragione Sociale e importanza cliente Nuovo Papà
025000161123         clear TIBS69DS;
025100161123         clear ACOrag;
025200161123         clear CLPclv;
025300161123         I69kac = D10cop;
025400161123         I69kcp = D10cop;
025500161123         TIBS69R (tibs69ds:ds_cnaco:ds_cnind:ds_cnclp:ds_fncls);
025600161123         ragksu = ACOrag;
025700161123         clvksu = CLPclv;
025800161123
025900161123       //?Verifico se il nuovo papà è in campagna
026000161123       //?se presente recupero anche il fatturato
026100161123         clear ncmksu;
026200161123         clear ricksu;
026300161123         chain (CampagnaA:I69kac) TICMI01L;
026400161123         IF  %found(TICMI01L);
026500161123           ncmksu = CampagnaA;
026600161123           ricksu = CMIric;
026700161123         ENDIF;
026800161123         IF  ncmksu = 0;
026900161123           chain (CampagnaB:I69kac) TICMI01L;
027000161123           IF  %found(TICMI01L);
027100161123             ncmksu = CampagnaB;
027200161123             ricksu = CMIric;
027300161123           ENDIF;
027400161123         ENDIF;
027500150408
027600150408       //?Dettaglio
027700150408         write TRKC83D;
027800150408
027900150408       ENDSR;
028000150326
028100150326       //--------------------------------------------------------------
028200150326       //?Operazioni finali.
028300150326       //--------------------------------------------------------------
028400150326       BEGSR RoutEnd;
028500150326
028600150326         *inLR = *on;
028700150326         return;
028800150326
028900150326       ENDSR;
029000150326
029100150326      /end-free
