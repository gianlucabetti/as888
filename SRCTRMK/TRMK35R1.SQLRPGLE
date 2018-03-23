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
001400110517      // - File attività
001500100415     fTIATC00F  o    e             disk
001600110428     fTIATC05L  if   e           k disk    rename(TIATC000:TIATC05)
001700100413
001800100413      //---------------------------------------------------------------
001900100413      //?Definizione costanti.
002000100413      //---------------------------------------------------------------
002100100413
002200100413      //---------------------------------------------------------------
002300100413      //?Definizione schiere.
002400100413      //---------------------------------------------------------------
002500090715
002600100413      //---------------------------------------------------------------
002700100413      //?Definizione aree dati.
002800100413      //---------------------------------------------------------------
002900100413
003000100413      //---------------------------------------------------------------
003100100413      //?Definizione strutture dati.
003200100413      //---------------------------------------------------------------
003300100415
003400110131      // - Parametri ricevuti
003500100415     d KPJBA         e ds
003600110211
003700110211      // ds x ATCflo
003800110211     d dATC01        e ds
003900110517
004000110517      // ds x ORGde3
004100110517     d OG143         e ds
004200100726
004300100726      // File potenziali
004400100726     d TNCPOds       e ds                  extname(TNCPO00F)
004500110131
004600110131      // - Controllo se potenziale lavora
004700110131     d TRMK04ds      e ds                  inz
004800110329     d TRMK85ds      e ds                  inz
004900100415
005000100415      // - Ricerca ultimo numero attività
005100100415     d TRUL33ds      e ds                  inz
005200110131
005300110131      // - Aggiunge/Toglie gg/mm dalla data
005400110131     d XGIOLAVds     e ds                  inz
005500100415
005600100413      //---------------------------------------------------------------
005700100413      //?Definizione variabili globali.
005800100413      //---------------------------------------------------------------
005900100413
006000100413      // - Flags booleani
006100100413     d $End            s               n   inz(*off)
006200110131
006300110131      // - Campi di comodo
006400110426     d NrAtt           s              5i 0
006500110131     d wDad            s                   like(ATCdad)
006600110513     d wgg             s                   like(IXGLgg)
006700100413
006800100413      //---------------------------------------------------------------
006900100413      //?Definizione procedure esterne.
007000100413      //---------------------------------------------------------------
007100110131
007200110131      // - Controllo se potenziale lavora
007300110131     d Trmk04R         pr                  extpgm('TRMK04R')
007400110131     d  KPJBA                              likeds(KPJBA)
007500110131     d  TRMK04ds                           likeds(TRMK04ds)
007600110329     d  TRMK85ds                           likeds(TRMK85ds) options(*nopass)
007700110131
007800110131      // - Aggiunge/Toglie gg/mm dalla data
007900110131     d Xgiolav         pr                  extpgm('XGIOLAV')
008000110131     d  xgiolavds                          likeds(xgiolavds)
008100100413
008200100413      //---------------------------------------------------------------
008300110131      //?Prototipi
008400100413      //---------------------------------------------------------------
008500100414
008600100415      /copy gaitrasrc/srcprotopr,trul33r
008700100413
008800100413      //---------------------------------------------------------------
008900100413      //?Definizione key-list.
009000100413      //---------------------------------------------------------------
009100100415
009200110517      // - Organigramma
009300110517     d k01azorg      e ds                  extname(AZORG01L:*key)
009400110517     d                                     prefix(k_)
009500110517     d                                     inz
009600100413
009700100413      //---------------------------------------------------------------
009800100413      //?Riepilogo indicatori.
009900100413      //---------------------------------------------------------------
010000100413
010100100413      //---------------------------------------------------------------
010200100413      //?M A I N - L I N E
010300100413      //---------------------------------------------------------------
010400100413
010500100413     c     *Entry        plist
010600110131     c                   parm                    kpjba
010700100413
010800100413      /free
010900100413
011000100413       //?Operazioni iniziali
011100100513       exsr RoutInz;
011200100413
011300100726       //?Elaboro
011400100726       exsr Elabora;
011500100413
011600100413       //?Operazioni finali
011700100513       exsr RoutEnd;
011800100413
011900100413       //--------------------------------------------------------------
012000100413       //?Operazioni iniziali.
012100100413       //--------------------------------------------------------------
012200100513       BEGSR RoutInz;
012300100726
012400100726         exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
012500100413
012600100413       ENDSR;
012700100413
012800100413       //--------------------------------------------------------------
012900110131       //?Elabora.
013000100413       //--------------------------------------------------------------
013100100513       BEGSR Elabora;
013200100413
013300100413         $End = *off;
013400100413
013500110131       //?Estraggo solo i potenziali codificati
013600100413         exec sql
013700100726         DECLARE CPO cursor for
013800110218         SELECT  CPOcpo, CPOfls, CPOflt
013900110428         FROM TNCPO01L
014000110513         WHERE CPOfls = 'C'
014100100726         ORDER by CPOcpo;
014200100413
014300100726         exec sql OPEN CPO;
014400100413
014500100413         DOW  not $End;
014600110218           exec sql FETCH next from CPO into :CPOCPO,
014700110211                                             :CPOFLS, :CPOFLT;
014800100413           IF  sqlcod = 100 or sqlcod < 0;
014900100413             $End = *on;
015000100413             leave;
015100100413           ENDIF;
015200100422
015300110131         //?Controllo se potenziale lavora o no
015400110131           clear TRMK04DS;
015500110131           IMK04cpo = CPOcpo;
015600110131           trmk04r (kpjba: TRMK04DS);
015700110131           IF  OMK04err <> *blanks;
015800110131             iter;
015900110131           ENDIF;
016000110517
016100110517         //?Controllo se il cliente che torna è un ntw LOG o XXX
016200110517         //?In questo caso non devo creare l'attività
016300110517           clear og143;
016400110517           k_ORGfil = %dec(%subst(%editc(OMK04ksc:'X'):1:3):3:0);
016500110517           chain %kds(k01azorg) AZORG01L;
016600110517           IF  %found(AZORG01L);
016700110517             og143 = ORGde3;
016800110517           ENDIF;
016900110517           IF  §OGntw = 'LOG' or §OGntw = 'XXX';
017000110517             iter;
017100110517           ENDIF;
017200110426
017300110426         //?Controllo se il potenziale ha già un'attività 70/75 da eseguire
017400110426           clear NrAtt;
017500110428           setll (CPOcpo:0:0) TIATC05L;
017600110428           reade (CPOcpo:0:0) TIATC05L;
017700110428           DOW not %eof(TIATC05L);
017800110428             IF  ATCcad = '70' or ATCcad = '75';
017900110428               NrAtt = 1;
018000110428               leave;
018100110428             ENDIF;
018200110428             reade (CPOcpo:0:0) TIATC05L;
018300110428           ENDDO;
018400100726
018500110512         //?Se NON lavora creo attività 70
018600110512         //?se c'è il commerciale sul potenziale
018700110512         //?se non c'è già un'attività 70/75 da eseguire
018800110426           IF  OMK04lav = 'NO' and OMK04cmm > 0 and NrAtt = 0;
018900100726             exsr Scrivi;
019000100726           ENDIF;
019100100413         ENDDO;
019200100413
019300100726         exec sql CLOSE CPO;
019400100413
019500100413       ENDSR;
019600100413
019700100413       //--------------------------------------------------------------
019800100726       //?Scrivo nuova attività.
019900100413       //--------------------------------------------------------------
020000100413       BEGSR Scrivi;
020100100422
020200100422         clear wDad;
020300100415
020400100726       //?Stacco nuovo numero attività
020500110513         exsr Stacca_Numero;
020600110513
020700110513       //?Creo l'attività al 1° gg. lavorativo successivo all'elaborazione
020800110513         wgg = 1;
020900110513
021000110513       //?Se cliente C o senza codice importanza cliente
021100110513       //?creo l'attività in base a quanti gg non lavora
021200110513       //?per i clienti C in tabella il NON lavora è a 120 gg
021300160926         //*·IF  OMK04clv = 'C' or OMK04clv = *blanks;
021400160926         IF  OMK04clv = 'C' or OMK04clv = 'E' or
021500160926             OMK04clv = *blanks;
021600110513       //?Se cliente C o senza codice importanza cliente
021700110513           SELECT;
021800110513       //?non lavora da oltre 4 mesi e fino a 9 mesi creo l'attività a 30 gg.
021900110513             WHEN  OMK04gg <= 270;
022000110513               wgg = 30;
022100110513       //?non lavora da oltre 9 mesi e fino a 18 mesi creo l'attività a 60 gg.
022200110513             WHEN  OMK04gg <= 540;
022300110513               wgg = 60;
022400110513       //?non lavora da oltre 18 mesi creo l'attività a 90 gg.
022500110513             OTHER;
022600110513               wgg = 90;
022700110513           ENDSL;
022800110513         ENDIF;
022900110513
023000110131       //?Calcolo la data attività
023100110131         clear XGIOLAVDS;
023200110131         IXGLdata = %dec(%date());
023300110131         IXGLadd  = 'S';
023400110207         IXGLgg   = wgg;
023500110131         IXGLpa   = 'P';
023600110513         IF  wgg = 1;
023700110513           IXGLlav  = 'S';
023800110513         ENDIF;
023900110223         IXGLfil  = %dec(%subst(%editc(OMK04cmm:'X'):1:3):3:0);
024000110131         XGIOLAV (xgiolavds);
024100110131         IF  OXGLerr = *blanks;
024200110131           wDad = OXGLdata;
024300110131         ELSE;
024400110131           wDad = %dec(%date());
024500110131         ENDIF;
024600110131       //?Scrivo la nuova attività
024700110513         exsr Scrivi_TIATC;
024800100413
024900100413       ENDSR;
025000100415
025100100415       //--------------------------------------------------------------
025200100415       //?Stacco nuovo numero attività.
025300100415       //--------------------------------------------------------------
025400100415       BEGSR Stacca_Numero;
025500100415
025600100415         clear TRUL33ds;
025700100415         I33tla = 'L';
025800100415         I33ope = 0;
025900100415         I33cnu = 070;
026000100415         I33num = 1;
026100100415         I33aaa = *year;
026200100415         kpjbu  = TRUL33ds;
026300100415         trul33r (kpjba);
026400100415         TRUL33ds = kpjbu;
026500100415
026600100415       ENDSR;
026700100415
026800100415       //--------------------------------------------------------------
026900100415       //?Scrivo file TIATC.
027000100415       //--------------------------------------------------------------
027100100415       BEGSR Scrivi_TIATC;
027200100415
027300110211         clear dATC01;
027400100415         clear TIATC000;
027500110131         ATCtat  = 'T';
027600100415         ATCatn  = O33Nrf;
027700100415         ATCatnp = 1;
027800110131         ATCcpo  = CPOcpo;
027900110207         ATCksc  = OMK04ksc;
028000110131         ATCcad  = '70';
028100110131         ATCdad  = wDad;
028200100422         ATChda  = 103000;
028300110218         ATCcmm  = OMK04cmm;
028400110218         ATCco3  = OMK04cmm;
028500110131         ATCcnw = 'C';
028600100415         ATCdim  = %dec(%date());
028700100415         ATChim  = %dec(%time());
028800100415         ATCpri  = 'BATCH';
028900110211         §ATCcapo1 = CPOfls;
029000110211         ATCflo = dATC01;
029100100415         write TIATC000;
029200100415
029300100415       ENDSR;
029400100413
029500100413       //--------------------------------------------------------------
029600100413       //?Operazioni finali.
029700100413       //--------------------------------------------------------------
029800100513       BEGSR RoutEnd;
029900100413
030000100413         *inLR = *on;
030100100413         return;
030200100413
030300100413       ENDSR;
030400100413
030500100413      /end-free
