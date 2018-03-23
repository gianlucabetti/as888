000100120920     h DFTACTGRP(*NO) ACTGRP(*NEW) BNDDIR('TRUL')
000200120920
000300120920       //-----------------------------------------------------------------------
000400120920       //
000500120920       // Controllo File LOG PDA - Max Record per Distinta
000600120920       //
000700120920       //-----------------------------------------------------------------------
000800120920
000900120920     fPRTEMAIL  o    f  132        printer oflind(*inof) usropn
001000120920
001100120920       //--------------------------------------------------------------
001200120920       //?Definizione costanti.                                        ?
001300120920       //--------------------------------------------------------------
001400120920     d C_Dominio       c                   const('@brt.it')
001500120920
001600120920     d C_Sede          c                   const('046')
001700120920
001800120920      // - Comando di override al PrtF
001900120920     d C_CmdOvrPrtF    c                   const('OVRPRTF +
002000120920     d                                           file(PRTEMAIL) +
002100120920     d                                           pagesize(66 132) +
002200120920     d                                           lpi(6) cpi(10) +
002300120920     d                                           ovrscope(*actgrpdfn) +
002400120920     d                                           ')
002500120920     d C_CmdDltOvr     c                   const('DLTOVR +
002600120920     d                                            file(PRTEMAIL) +
002700120920     d                                            lvl(*actgrpdfn)')
002800120920
002900120920      //---------------------------------------------------------------
003000120920      //?Definizione schiere.
003100120920      //---------------------------------------------------------------
003200120920
003300120920       //--------------------------------------------------------------
003400120920       //?Definizione strutture dati.                                  ?
003500120920       //--------------------------------------------------------------
003600120920     d kpjba         e ds
003700120920
003800120920     d FIPLG00F      e ds                  extname(FIPLG00F)
003900120920
004000120920      // - Gestione tabelle: controllo e ricerca
004100120920     d TIBS02ds      e ds                  inz
004200120920     d   T02tla      e                     inz('L')
004300120920     d   T02mod      e                     inz('C')
004400120920     d   T02cod      e                     inz('MRA')
004500120920
004600120920      // - Recupera dati utente
004700120920     d TIBS34ds      e ds
004800120920     d dUTE01        e ds
004900120920
005000120920      // - Tabella "MRA" = Bart-Mailing - Danni
005100120920     d dMRAdan       e ds                  inz
005200120920
005300120920      // - Parametri x Ridefinizione dati utente estesi per mailing PDF
005400120920     d TRTCM1ds      e ds                  inz
005500120920      //   ·§CM1mitt = Indirizzo e-mail del mittente
005600120920     d   §CM1mitt    e                     inz('ced@brt.it')
005700120920      //   ·§CM1dst  = Indirizzo e-mail del destinatario
005800120920     d   §CM1dst     e                     inz('monica.beghelli@brt.it')
005900120920      //   ·§CM1tips = Tipo lettera via e-mail:
006000120920      //    "LET" = testo allegato in corpo con logo
006100120920      //            (richiede righe libere iniziali per il logo)
006200120920      //    "COR" = testo integrato senza logo
006300120920      //            (non consente né UNDERLINE né HIGHLIGHT)
006400120920     d   §CM1tips    e                     inz('COR')
006500120920      //   ·§CM1po   = Filiale
006600120920     d   §CM1po      e                     inz('046')
006700120920      //   ·§CM1var  = Oggetto e-mail
006800120920     d   §CM1var     e                     inz('*OBJM*+
006900120920     d                                     Errore in ricezione dati da +
007000120920     d                                     PDA')
007100120920      //   ·§CM1sts  = Stato
007200120920     d   §CM1sts     e                     inz(*off)
007300120920      //   ·§CM1sts  = Id processo
007400120920     d   §CM1idp     e                     inz('2')
007500120920
007600120920      // - Status
007700120920     d Psds           sds
007800120920     d   SDSpgm          *proc
007900120920     d   SDSjob              244    253
008000120920     d   SDSuse              254    263
008100120920
008200120920      //---------------------------------------------------------------
008300120920      //?Definizione aree dati.
008400120920      //---------------------------------------------------------------
008500120920
008600120920      // - Dati utente
008700120920     d §AzUte        e ds                  extname(AZUTE00F)
008800120920     d                                     dtaara
008900120920     d §DatiUte      e ds                  extname(dDatiUte)
009000120920     d                                     dtaara
009100120920
009200120920       //--------------------------------------------------------------
009300120920       //?Definizione variabili.                                       ?
009400120920       //--------------------------------------------------------------
009500120920     d cmd             s            512a   varying
009600120920     d conta           s             10i 0
009700120920
009800120920       // Flag Booleani
009900120920     d wEoF            s               n   inz('0')
010000120921     d wInvia          s               n   inz('0')
010100120920
010200120920      // - Campi di stampa
010300120920     d O_testo         s            132    inz
010400120920     d wOggi           s              8  0
010500120920
010600120920       //--------------------------------------------------------------
010700120920       //?Definizione procedure.
010800120920       //--------------------------------------------------------------
010900120920
011000120920      // - Gestione tabelle
011100120920     d tibs02r         pr                  extpgm('TIBS02R')
011200120920     d  kpjba                              likeds(KPJBA)    const
011300120920     d  tibs02ds                           likeds(TIBS02ds)
011400120920
011500120920       //--------------------------------------------------------------
011600120920       //?Definizione prototipi.
011700120920       //--------------------------------------------------------------
011800120920      /COPY GAITRASRC/SRCPROTOPR,SYSTEM
011900120920
012000120920       // - Reperimento dati utente
012100120920      /copy gaitrasrc/srcprotopr,tibs34r
012200120920
012300120920      //---------------------------------------------------------------
012400120920      //?Definizione key-list.
012500120920      //---------------------------------------------------------------
012600120920
012700120920       // - File TABEL00F
012800120920     d k03tabel      e ds                  extname(TABEL00F:*key)
012900120920     d                                     prefix(k_)
013000120920     d                                     inz
013100120920
013200120920       //--------------------------------------------------------------
013300120920       //?M A I N - L I N E                                            ?
013400120920       //--------------------------------------------------------------
013500120920
013600120920     c     *entry        plist
013700120920     c                   parm                    kpjba
013800120920
013900120920      /free
014000120920
014100120920       *inlr = *on;
014200120920
014300120920       //?Recupero i dati del job
014400120920       exsr DatiJob;
014500120920
014600120920       //?Calcolo oggi
014700120920       wOggi = %dec(%date());
014800120920
014900120920       exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
015000120920
015100120920       //?Reperimento tabella MRA
015200120920       clear dMRAdan;
015300120920       clear kpjbu;
015400120920       reset tibs02ds;
015500120920       T02ke1 = SDSpgm;
015600120920       tibs02r(kpjba:tibs02ds);
015700120920       IF T02err = *blank;
015800120920         dMRAdan = T02uni;
015900120920       ENDIF;
016000120920
016100120920       //?Controllo se per distinta ci sono più di 1500 rcd
016200120920       //?nel caso mail di allert a ced
016300120920       exec sql
016400120920         DECLARE PLG cursor for
016500120920         SELECT count(*) as conta, PLGiddisp, PLGcodaut,
016600120920                                   PLGfgs, PLGndc
016700120920         FROM FIPLG00F
016800120921         WHERE substr(PLGdatora, 1, 8) = :wOggi
016900120920         GROUP BY PLGiddisp, PLGcodaut,
017000120920                  PLGfgs, PLGndc
017100120921         HAVING count(*) > 1500;
017200120920
017300120920       //?Apertura del cursore
017400120920       exec sql
017500120920         open PLG;
017600120920
017700120920       IF sqlcode < 0;
017800120920         wEoF = *on;
017900120920       ENDIF;
018000120920
018100120920       DOW not wEoF;
018200120920         exec sql
018300120920           FETCH next from PLG into :conta, :PLGiddisp, :PLGcodaut,
018400120920                                            :PLGfgs, :PLGndc;
018500120920         IF sqlcod = 100 or sqlcod < 0;
018600120920           wEof = *on;
018700120920           leave;
018800120920         ENDIF;
018900120921
019000120921       //?Apro il file per la mail a CED
019100120921         IF  not wInvia;
019200120921           exsr Open_PrtF;
019300120921           wInvia = *on;
019400120921         ENDIF;
019500120920
019600120920       //?Scrivo mail
019700120921         clear O_testo;
019800120921         %subst(O_testo:1:35) = PLGiddisp;
019900120921         %subst(O_testo:37:7) = %editc(PLGcodaut:'X');
020000120921         %subst(O_testo:55:3) = %editc(PLGfgs:'X');
020100120921         %subst(O_testo:63:6) = %editc(PLGndc:'X');
020200120920         except PrtDet;
020300120920
020400120920       ENDDO;
020500120920
020600120920       exec sql
020700120920         close PLG;
020800120920
020900120920       //?Chiudo la mail a CED
021000120920       exsr Close_PrtF;
021100120920
021200120920       exsr Uscita;
021300120920
021400120920       //--------------------------------------------------------------
021500120920       //?Apro file di stampa per invio mail a CED.
021600120920       //--------------------------------------------------------------
021700120920       BEGSR Open_PrtF;
021800120920
021900120920       //?Override al file di stampa per impostarvi i dati per
022000120920       //?l'invio via e-mail
022100120920         exsr OVR_PrtF;
022200120920
022300120920         open PrtEMAIL;
022400120920
022500120920       //?Stampa una testata se NON è richiesta la e-mail
022600120920         IF  §MRAdreg =  *blank;
022700120920           O_testo = SDSuse + ' - ' + SDSpgm
022800120920                   + ' - ' + %editc(wOggi:'X')
022900120920                   + ' - *REM* ' + %subst(§CM1var : 7 : 70);
023000120920           except PrtDet;
023100120920           clear O_testo;
023200120920           except PrtDet;
023300120920           except PrtDet;
023400120920         ENDIF;
023500120920
023600120920       //?Stampa testo iniziale della mail
023700120920         clear O_testo;
023800120920         except PrtDet;
023900120920
024000120920         O_testo = 'Per i seguenti Dispositivi abbiamo ricevuto più ' +
024100121120                   'di 1500 rcd di LOG, VERIFICARE i LOG e contattare';
024200120920         except PrtDet;
024300120920
024400121120         O_testo = 'IMMEDIATAMENTE la filiale di competenza per far ' +
024500121120                   'resettare il PDA indicato: facendo estrarre la';
024600120920         except PrtDet;
024700121120
024800121120         O_testo = 'batteria per più di 5 minuti per poi reinserirla.';
024900121120         except PrtDet;
025000120920
025100120920         clear O_testo;
025200120920         except PrtDet;
025300120920
025400120920       //?Stampa intestazione
025500120921         %subst(O_testo:1:11)  = 'Dispositivo';
025600120921         %subst(O_testo:37:17) = 'Autotrasportatore';
025700120921         %subst(O_testo:55:7)  = 'Filiale';
025800120921         %subst(O_testo:63:8)  = 'Distinta';
025900120920         except PrtDet;
026000120920         clear O_testo;
026100120920         except PrtDet;
026200120920
026300120920       ENDSR;
026400120920
026500120920       //--------------------------------------------------------------
026600120920       //?Override al file di stampa per impostarvi i dati per
026700120920       //?l'invio via e-mail + stampa inizio e-mail
026800120920       //--------------------------------------------------------------
026900120920       BEGSR OVR_PrtF;
027000120920
027100120920         reset TRTCM1ds;
027200120920
027300120920         IF  §MRAdreg <> *blank;
027400120920           §CM1mitt = %trim(§MRAdmitt);
027500120920           §CM1dst  = §MRAddest;
027600120920           §CM1tips = §MRAdreg;
027700120920           §CM1po   = C_Sede;
027800120920           §CM1var  = '*OBJM*' + §MRAddes;
027900120920           §CM1idp  = §MRAdidpro;
028000120920           cmd = C_CmdOvrPrtF
028100120920               + ' outq(' + %trim(§MRAdoutqi) + ')'
028200120920               + ' usrdfndta(''' + TRTCM1ds + ''')';
028300120920         ELSE;
028400120920           cmd = C_CmdOvrPrtF;
028500120920         ENDIF;
028600120920
028700120920         IF  ExecuteCommand( cmd ) = 0 and
028800120920             §MRAdreg <> *blanks;
028900120920         ENDIF;
029000120920
029100120920       ENDSR;
029200120920
029300120920       //--------------------------------------------------------------
029400120920       //?Chiudo il file di stampa per invio mail a RA.
029500120920       //--------------------------------------------------------------
029600120920       BEGSR Close_PrtF;
029700120920
029800120920         IF %open(PrtEMAIL);
029900120920
030000120920       //?Stampo le righe finali della mail.
030100120920           clear O_testo;
030200120920           except PrtDet;
030300120920
030400120920       //?Chiusura dello spool
030500120920           clear O_testo;
030600120920           except PrtDet;
030700120920           except PrtDet;
030800120920           O_testo = '***   Fine Lista   ***';
030900120920           except PrtDet;
031000120920
031100120920           close PrtEMAIL;
031200120920
031300120920       //?Elimina Override
031400120920           cmd = C_CmdDltOvr;
031500120920           ExecuteCommand( cmd );
031600120920         ENDIF;
031700120920
031800120920       ENDSR;
031900120920
032000120920       //--------------------------------------------------------------
032100120920       //?Reperimento Dati del job (Utente/Operativi).
032200120920       //--------------------------------------------------------------
032300120920       BEGSR DatiJob;
032400120920
032500120920         in(E) §AzUte;
032600120920         if NOT %error;
032700120920           in(E) §DatiUte;
032800120920         endif;
032900120920         if %error or RSut = *blanks;
033000120920           clear TIBS34ds;
033100120920           tibs34r(tibs34ds);
033200120920           in §AzUte;
033300120920           in §DatiUte;
033400120920         endif;
033500120920
033600120920       ENDSR;
033700120920
033800120920       //--------------------------------------------------------------
033900120920       //?Uscita dal pgm.
034000120920       //--------------------------------------------------------------
034100120920       BEGSR Uscita;
034200120920
034300120920         return;
034400120920
034500120920       ENDSR;
034600120920
034700120920      /end-free
034800120920
034900120920      //---------------------------------------------------------------
035000120920      //?Spool di stampa (per e-mail).
035100120920      //---------------------------------------------------------------
035200120920     oPrtEMAIL  e            PRTdet      1
035300120920     o                       O_testo
