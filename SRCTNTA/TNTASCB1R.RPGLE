000100170125      //--------------------------------------------------------------
000200170125      //?TNTASCB1R - Crea WrkF Conteggio KSC per supporto da TITAS
000300170125      //--------------------------------------------------------------
000400170125     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000500170125     h dftactgrp(*no) actgrp(*caller)
000600160805
000700170125      //---------------------------------------------------------------
000800170125      //?Dichiarazione file.
000900170125      //---------------------------------------------------------------
001000170125      // - Organigramma?
001100160805     fAZORG01L  if   e           k disk
001200170125
001300170125      // - Tabella
001400170125     fTABEL00F  if   e           k disk
001500170126
001600170126      // - Bolle sede
001700170126     fTITAS38C  if   e           k disk
001800160805
001900170125      // - Work File
002000170126     fWFSCB01L  uf a e           k disk    usropn
002100160805     f                                     extfile(wFLib)
002200160805
002300170125      //--------------------------------------------------------------
002400170125      //?Definizione costanti.                                        ?
002500170125      //--------------------------------------------------------------
002600160805
002700170125      //--------------------------------------------------------------
002800170125      //?Definizione schiere.                                         ?
002900170125      //--------------------------------------------------------------
003000160805
003100170125      //--------------------------------------------------------------
003200170125      //?Definizione aree dati.                                       ?
003300170125      //--------------------------------------------------------------
003400170125      // - Dati utente?
003500160805     d §AzUte        e ds                  extname(AZUTE00F)
003600160805     d                                     dtaara
003700160805     d §DatiUte      e ds                  extname(dDatiUte)
003800160805     d                                     dtaara
003900160805
004000170125      //--------------------------------------------------------------
004100170125      //?Definizione strutture dati.                                  ?
004200170125      //--------------------------------------------------------------
004300170125      // - Parametri ricevuti
004400160805     d KPJBA         e ds
004500160805
004600170125      // - Parametri per Reperimento dati utente?
004700160805     d TIBS34ds      e ds
004800170125
004900170125      // - Parametri ricevuti
005000170125     d TNTASCBDS     e ds
005100170125     d  AASI                   1      4s 0
005200170125     d  MGSI                   5      8s 0
005300170125
005400170125      // - Tabella 3C
005500170125     d ds3C          e ds
005600160805
005700170125      //--------------------------------------------------------------
005800170125      //?Definizione variabili globali.                               ?
005900170125      //--------------------------------------------------------------
006000170125      // - Flags booleani
006100170125     d EoF             s               n   inz(*off)
006200170125     d Fine            s               n   inz(*off)
006300160805
006400170125      // - Nome libreria del work-file WFSCB00F?
006500160805     d wFLib           s             21    inz
006600160805
006700170125      // - Campi per richiamo QCMDEXC
006800170125     d cmd             s            128    inz
006900170125
007000170125       // - Stringa SQL da eseguire
007100170125     d wSQL            s           4096    Varying        inz
007200170125
007300170125      // - Campi di comodo
007400170125     d DataBolla       s              8s 0 inz
007500170126     d savKSC          s                   like(TASksc) inz
007600170126     d savLNA          s                   like(TASlna) inz
007700170126     d totNSP          s              9s 0 inz
007800170126     d wLNPSede        s                   like(TASlnp) inz(102)
007900170126     d wNRSzero        s                   like(TASnrs) inz
008000160805
008100170125      //--------------------------------------------------------------
008200170125      //?Definizione prototipi procedure e relativi parametri.        ?
008300170125      //--------------------------------------------------------------
008400170125      /copy gaitrasrc/srcprotopr,SYSTEM
008500170125      /copy gaitrasrc/srcprotopr,TIBS34R
008600160805
008700170125      //--------------------------------------------------------------
008800170125      //?Definizione key-list.                                        ?
008900170125      //--------------------------------------------------------------
009000170125      // - File TABEL00F
009100170125     d k03tabel      e ds                  extname(TABEL00F:*key)
009200170125     d                                     prefix(k_)
009300160805
009400170125      //--------------------------------------------------------------
009500170125      //?M A I N - L I N E                                            ?
009600170125      //--------------------------------------------------------------
009700160805
009800160805     c     *Entry        plist
009900160805     c                   parm                    KPJBA
010000160805
010100160805      /free
010200160805
010300170125      //?Operazioni iniziali
010400170125       exsr RoutInz;
010500170125
010600170125      //?Elabora
010700170125       exsr Elabora;
010800170125
010900170125      //?Operazioni finali
011000170125       exsr RoutEnd;
011100160805
011200170125      //--------------------------------------------------------------
011300170125      //?Operazioni iniziali.
011400170125      //--------------------------------------------------------------
011500170125       BEGSR RoutInz;
011600170125
011700170125       //?Reperimento dati job
011800170125         exsr DatiJob;
011900170125
012000170125       //?Imposto quanto ricevuto da chiamante
012100170125         TNTASCBDS = kpjbu;
012200170125
012300170125       //?Pulizia file di work
012400170125         cmd = 'CLRPFM FILE(';
012500170125         IF  %subst(knsif : 7 : 1) = 'P';
012600170125           cmd = %trimr(cmd) + 'GAITRAAZP';
012700170126           wFLib = 'GAITRAAZP/WFSCB01L';
012800170125         ELSE;
012900170125           cmd = %trimr(cmd) + 'GAITRAAZM';
013000170126           wFLib = 'GAITRAAZM/WFSCB01L';
013100170125         ENDIF;
013200170126         cmd = %trimr(cmd) + '/WFSCB00F)';
013300170125         ExecuteCommand (cmd);
013400170126         open  WFSCB01L;
013500170125
013600170125       //?Imposto key fissa tabella 3C
013700170125         k_TBLkut = 1;
013800170125         k_TBLcod = '3C';
013900170125
014000170125       ENDSR;
014100170125
014200170125      //--------------------------------------------------------------
014300170125      //?Reperimento Dati del job (Utente/Operativi).
014400170125      //--------------------------------------------------------------
014500170125       BEGSR DatiJob;
014600170125
014700170125         in(E) §AzUte;
014800170125         IF  not %error;
014900170125           in(E) §DatiUte;
015000170125         ENDIF;
015100170125         IF  %error or RSut = *blanks;
015200170125           clear TIBS34ds;
015300170125           tibs34r(tibs34ds);
015400170125           in §AzUte;
015500170125           in §DatiUte;
015600170125         ENDIF;
015700170125
015800170125       ENDSR;
015900160805
016000170125      //--------------------------------------------------------------
016100170125      //?Elaborazione principale.
016200170125      //--------------------------------------------------------------
016300170125       BEGSR Elabora;
016400170125
016500170125       //?Leggo file bolle per il periodo richiesto
016600170125       //?Solo bolle di tipo '1'
016700170125       //?Solo bolle con serie
016800170126         setll (AASI:MGSI) TITAS38C;
016900170126         DOW  not EoF;
017000170126           read  TITAS38C;
017100170126
017200170126         //?Fine file
017300170126           IF  %eof(TITAS38C);
017400170126             leave;
017500170126           ENDIF;
017600170125
017700170126         //?Fino alla data richiesta
017800170125           DataBolla = TASaas * 10000 + TASmgs;
017900170125           IF  DataBolla > ISCBdataF;
018000170125             leave;
018100170125           ENDIF;
018200170126
018300170127         //?Solo bolle 'F1'
018400170127           IF  TAStbl <> 'F1';
018500170126             iter;
018600170126           ENDIF;
018700170126
018800170126         //?Solo bolle con serie
018900170126           IF  TASnrs = 0;
019000170126             iter;
019100170126           ENDIF;
019200170126
019300170126         //?No LNP 102 (bolle generate da EDP)
019400170126           IF  TASlnp = 102;
019500170126             iter;
019600170126           ENDIF;
019700170126
019800170126         //?Se richiesta solo la LNA impostata nel lancio
019900170126           IF  ISCBlna <> 999 and TASlna <> ISCBlna;
020000170126             iter;
020100170126           ENDIF;
020200170125
020300170126         //?A cambio cliente
020400170126         //?aggancio la tabella 3C x recuperare il supporto
020500170126         //?e scrivo il file
020600170125           IF  TASksc <> savKSC;
020700170127             IF  savKSC > 0;
020800170126               exsr WriteFile;
020900170126             ENDIF;
021000170126             savKSC = TASksc;
021100170127             IF  ISCBlna = 999;
021200170127               savLNA = TASLNA;
021300170127             ENDIF;
021400170125           ENDIF;
021500170126
021600170126         //?Se richieste tutte le LNA
021700170126           IF  ISCBlna = 999;
021800170126           //?A cambio LNA
021900170126           //?scrivo il file
022000170126             IF  TASlna <> savLNA;
022100170126               IF  savLNA > 0;
022200170126                 exsr WriteFile;
022300170126               ENDIF;
022400170126               savLNA = TASlna;
022500170126             ENDIF;
022600170126           ENDIF;
022700170126         //?Se richieste una LNA la imposto nel campo SAV
022800170126           IF  ISCBlna <> 999;
022900170126             savLNA = ISCBlna;
023000170126           ENDIF;
023100170126
023200170126         //?Conto
023300170126           totNSP += 1;
023400170125
023500170125         ENDDO;
023600170127
023700170127       //?devo aggiornare anche l'ultimo KSC che ho letto
023800170127         IF  savKSC > 0;
023900170127           exsr WriteFile;
024000170127         ENDIF;
024100170125
024200170125       ENDSR;
024300170126
024400170126      //--------------------------------------------------------------
024500170126      //?Scrive file di WORK.
024600170126      //--------------------------------------------------------------
024700170126       BEGSR WriteFile;
024800170127
024900170127          clear ds3C;
025000170127          k_TBLkey = %editc(SAVksc:'X');
025100170127          chain %kds(K03tabel) TABEL00F;
025200170127          IF  %found(TABEL00F) and TBLflg = *blanks;
025300170127            ds3C = TBLuni;
025400170127          ENDIF;
025500170126
025600170126         chain (savKSC:savLNA) WFSCB01L;
025700170126         IF  not %found(WFSCB01L);
025800170126           clear WFSCB000;
025900170126           SCBdatai = ISCBdataI;
026000170126           SCBdataf = ISCBdataF;
026100170126           SCBksc = savKSC;
026200170126           SCBlna = savLNA;
026300170126         ENDIF;
026400170126         SCBnsp += totNSP;
026500170126         SELECT;
026600170126         WHEN  §3Ccba = 'EDI  ';
026700170126           SCBedi += totNSP;
026800170126         WHEN  §3Ccba = 'EMAIL';
026900170126           SCBemail += totNSP;
027000170126         WHEN  §3Ccba = 'ESYSP';
027100170126           SCBesysp += totNSP;
027200170126         WHEN  §3Ccba = 'ESVAL';
027300170126           SCBesval += totNSP;
027400170126         WHEN  §3Ccba = 'ESWEB';
027500170126           SCBesweb += totNSP;
027600170126         WHEN  §3Ccba = 'FTP  ';
027700170126           SCBftp += totNSP;
027800170126         WHEN  §3Ccba = 'SNA  ';
027900170126           SCBsna += totNSP;
028000170126         WHEN  §3Ccba = 'WW   ';
028100170126           SCBww += totNSP;
028200170127         OTHER;
028300170127           SCBno3c += totNSP;
028400170126         ENDSL;
028500170126         IF  not %found(WFSCB01L);
028600170126           write WFSCB000;
028700170126         ENDIF;
028800170126         IF  %found(WFSCB01L);
028900170126           update WFSCB000;
029000170126         ENDIF;
029100170126         clear totNSP;
029200170126
029300170126       ENDSR;
029400170125
029500170125      //--------------------------------------------------------------
029600170125      //?Operazioni finali.
029700170125      //--------------------------------------------------------------
029800170125       BEGSR RoutEnd;
029900170125
030000170125         *inLR = *on;
030100170125         return;
030200170125
030300170125       ENDSR;
030400170125
030500170125      /end-free
030600170125
