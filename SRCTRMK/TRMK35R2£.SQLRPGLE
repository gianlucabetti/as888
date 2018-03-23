000100100413      //---------------------------------------------------------------
000200110513      //?TRMK35R1 - Crea attività 70 su potenziali codificati   SPOT
000300100413      //---------------------------------------------------------------
000400100413
000500100413     h decedit('0,') datedit(*ymd/) option(*nodebugio)
000600100413
000700100413      //---------------------------------------------------------------
000800100413      //?Dichiarazione file.
000900100413      //---------------------------------------------------------------
001000110517
001100110517      // - File Organigramma
001200110517     fAZORG01L  if   e           k disk
001300110517
001400110516      // - File attività
001500110428     fTIATC05L  if   e           k disk    rename(TIATC000:TIATC05)
001600110517
001700110516      // - File di controllo
001800110516     fwfcad70t  o    e             disk    rename(wfcad70t:wfcad7000)
001900100413
002000100413      //---------------------------------------------------------------
002100100413      //?Definizione costanti.
002200100413      //---------------------------------------------------------------
002300100413
002400100413      //---------------------------------------------------------------
002500100413      //?Definizione schiere.
002600100413      //---------------------------------------------------------------
002700090715
002800100413      //---------------------------------------------------------------
002900100413      //?Definizione aree dati.
003000100413      //---------------------------------------------------------------
003100100413
003200100413      //---------------------------------------------------------------
003300100413      //?Definizione strutture dati.
003400100413      //---------------------------------------------------------------
003500100415
003600110131      // - Parametri ricevuti
003700100415     d KPJBA         e ds
003800110211
003900110211      // ds x ATCflo
004000110211     d dATC01        e ds
004100110517
004200110517      // ds x ORGde3
004300110517     d OG143         e ds
004400100726
004500100726      // File potenziali
004600100726     d TNCPOds       e ds                  extname(TNCPO00F)
004700110131
004800110131      // - Controllo se potenziale lavora
004900110131     d TRMK04ds      e ds                  inz
005000110329     d TRMK85ds      e ds                  inz
005100110131
005200110131      // - Aggiunge/Toglie gg/mm dalla data
005300110131     d XGIOLAVds     e ds                  inz
005400100415
005500100413      //---------------------------------------------------------------
005600100413      //?Definizione variabili globali.
005700100413      //---------------------------------------------------------------
005800100413
005900100413      // - Flags booleani
006000100413     d $End            s               n   inz(*off)
006100110131
006200110131      // - Campi di comodo
006300110426     d NrAtt           s              5i 0
006400110131     d wDad            s                   like(ATCdad)
006500110513     d wgg             s                   like(IXGLgg)
006600100413
006700100413      //---------------------------------------------------------------
006800100413      //?Definizione procedure esterne.
006900100413      //---------------------------------------------------------------
007000110131
007100110131      // - Controllo se potenziale lavora
007200110131     d Trmk04R         pr                  extpgm('TRMK04R')
007300110131     d  KPJBA                              likeds(KPJBA)
007400110131     d  TRMK04ds                           likeds(TRMK04ds)
007500110329     d  TRMK85ds                           likeds(TRMK85ds) options(*nopass)
007600110131
007700110131      // - Aggiunge/Toglie gg/mm dalla data
007800110131     d Xgiolav         pr                  extpgm('XGIOLAV')
007900110131     d  xgiolavds                          likeds(xgiolavds)
008000100413
008100100413      //---------------------------------------------------------------
008200110131      //?Prototipi
008300100413      //---------------------------------------------------------------
008400100414
008500100413
008600100413      //---------------------------------------------------------------
008700100413      //?Definizione key-list.
008800100413      //---------------------------------------------------------------
008900110517
009000110517      // - Organigramma
009100110517     d k01azorg      e ds                  extname(AZORG01L:*key)
009200110517     d                                     prefix(k_)
009300110517     d                                     inz
009400100413
009500100413      //---------------------------------------------------------------
009600100413      //?Riepilogo indicatori.
009700100413      //---------------------------------------------------------------
009800100413
009900100413      //---------------------------------------------------------------
010000100413      //?M A I N - L I N E
010100100413      //---------------------------------------------------------------
010200100413
010300100413     c     *Entry        plist
010400110131     c                   parm                    kpjba
010500100413
010600100413      /free
010700100413
010800100413       //?Operazioni iniziali
010900100513       exsr RoutInz;
011000100413
011100100726       //?Elaboro
011200100726       exsr Elabora;
011300100413
011400100413       //?Operazioni finali
011500100513       exsr RoutEnd;
011600100413
011700100413       //--------------------------------------------------------------
011800100413       //?Operazioni iniziali.
011900100413       //--------------------------------------------------------------
012000100513       BEGSR RoutInz;
012100100726
012200100726         exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
012300100413
012400100413       ENDSR;
012500100413
012600100413       //--------------------------------------------------------------
012700110131       //?Elabora.
012800100413       //--------------------------------------------------------------
012900100513       BEGSR Elabora;
013000100413
013100100413         $End = *off;
013200100413
013300110131       //?Estraggo solo i potenziali codificati
013400100413         exec sql
013500100726         DECLARE CPO cursor for
013600110516         SELECT  TNCPO01L.*
013700110428         FROM TNCPO01L
013800110513         WHERE CPOfls = 'C'
013900100726         ORDER by CPOcpo;
014000100413
014100100726         exec sql OPEN CPO;
014200100413
014300100413         DOW  not $End;
014400110516           exec sql FETCH next from CPO into :TNCPOds;
014500100413           IF  sqlcod = 100 or sqlcod < 0;
014600100413             $End = *on;
014700100413             leave;
014800100413           ENDIF;
014900100422
015000110131         //?Controllo se potenziale lavora o no
015100110131           clear TRMK04DS;
015200110131           IMK04cpo = CPOcpo;
015300110131           trmk04r (kpjba: TRMK04DS);
015400110131           IF  OMK04err <> *blanks;
015500110131             iter;
015600110131           ENDIF;
015700110517
015800110517         //?Controllo se il cliente che torna è un ntw LOG o XXX
015900110517         //?In questo caso non devo creare l'attività
016000110517           clear og143;
016100110517           k_ORGfil = %dec(%subst(%editc(OMK04ksc:'X'):1:3):3:0);
016200110517           chain %kds(k01azorg) AZORG01L;
016300110517           IF  %found(AZORG01L);
016400110517             og143 = ORGde3;
016500110517           ENDIF;
016600110517           IF  §OGntw = 'LOG' or §OGntw = 'XXX';
016700110517             iter;
016800110517           ENDIF;
016900110426
017000110426         //?Controllo se il potenziale ha già un'attività 70/75 da eseguire
017100110426           clear NrAtt;
017200110428           setll (CPOcpo:0:0) TIATC05L;
017300110428           reade (CPOcpo:0:0) TIATC05L;
017400110428           DOW not %eof(TIATC05L);
017500110428             IF  ATCcad = '70' or ATCcad = '75';
017600110428               NrAtt = 1;
017700110428               leave;
017800110428             ENDIF;
017900110428             reade (CPOcpo:0:0) TIATC05L;
018000110428           ENDDO;
018100100726
018200110512         //?Se NON lavora creo attività 70
018300110512         //?se c'è il commerciale sul potenziale
018400110512         //?se non c'è già un'attività 70/75 da eseguire
018500110426           IF  OMK04lav = 'NO' and OMK04cmm > 0 and NrAtt = 0;
018600100726             exsr Scrivi;
018700100726           ENDIF;
018800100413         ENDDO;
018900100413
019000100726         exec sql CLOSE CPO;
019100100413
019200100413       ENDSR;
019300100413
019400100413       //--------------------------------------------------------------
019500100726       //?Scrivo nuova attività.
019600100413       //--------------------------------------------------------------
019700100413       BEGSR Scrivi;
019800100422
019900100422         clear wDad;
020000110513
020100110513       //?Creo l'attività al 1° gg. lavorativo successivo all'elaborazione
020200110513         wgg = 1;
020300110513
020400110513       //?Se cliente C o senza codice importanza cliente
020500110513       //?creo l'attività in base a quanti gg non lavora
020600110513       //?per i clienti C in tabella il NON lavora è a 120 gg
020700110513         IF  OMK04clv = 'C' or OMK04clv = *blanks;
020800110513       //?Se cliente C o senza codice importanza cliente
020900110513           SELECT;
021000110513       //?non lavora da oltre 4 mesi e fino a 9 mesi creo l'attività a 30 gg.
021100110513             WHEN  OMK04gg <= 270;
021200110513               wgg = 30;
021300110513       //?non lavora da oltre 9 mesi e fino a 18 mesi creo l'attività a 60 gg.
021400110513             WHEN  OMK04gg <= 540;
021500110513               wgg = 60;
021600110513       //?non lavora da oltre 18 mesi creo l'attività a 90 gg.
021700110513             OTHER;
021800110513               wgg = 90;
021900110513           ENDSL;
022000110513         ENDIF;
022100110513
022200110131       //?Calcolo la data attività
022300110131         clear XGIOLAVDS;
022400110131         IXGLdata = %dec(%date());
022500110131         IXGLadd  = 'S';
022600110207         IXGLgg   = wgg;
022700110131         IXGLpa   = 'P';
022800110513         IF  wgg = 1;
022900110513           IXGLlav  = 'S';
023000110513         ENDIF;
023100110223         IXGLfil  = %dec(%subst(%editc(OMK04cmm:'X'):1:3):3:0);
023200110131         XGIOLAV (xgiolavds);
023300110131         IF  OXGLerr = *blanks;
023400110131           wDad = OXGLdata;
023500110131         ELSE;
023600110131           wDad = %dec(%date());
023700110131         ENDIF;
023800110131       //?Scrivo la nuova attività
023900110513         exsr Scrivi_TIATC;
024000100413
024100100413       ENDSR;
024200100415
024300100415       //--------------------------------------------------------------
024400100415       //?Scrivo file TIATC.
024500100415       //--------------------------------------------------------------
024600100415       BEGSR Scrivi_TIATC;
024700110516
024800110516         clear wfcad7000;
024900110516         ATCcpo  = CPOcpo;
025000110516         ATCksc  = OMK04ksc;
025100110516         ATCcad  = '70';
025200110516         ATCdad  = wDad;
025300110516         ATChda  = 103000;
025400110516         ATCcmm  = OMK04cmm;
025500110516         ATCdim  = %dec(%date());
025600110516         ATChim  = %dec(%time());
025700110516         ATCpri  = 'BATCH';
025800110516         write WFCAD7000;
025900100415
026000100415       ENDSR;
026100100413
026200100413       //--------------------------------------------------------------
026300100413       //?Operazioni finali.
026400100413       //--------------------------------------------------------------
026500100513       BEGSR RoutEnd;
026600100413
026700100413         *inLR = *on;
026800100413         return;
026900100413
027000100413       ENDSR;
027100100413
027200100413      /end-free
