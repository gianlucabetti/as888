000100180117     h dftactgrp(*no) actgrp(*caller)
000200180117      //-------------------------------------------------------------------------
000300180117      //?TRULDVDR: Scrive file TIDVD00F
000400180117      //-------------------------------------------------------------------------
000500180117
000600180117      //-------------------------------------------------------------------------
000700180117      //?Dichiarazione file.
000800180117      //-------------------------------------------------------------------------
000900180117     fTIDVD00F  o    e             disk
001000180117
001100180117      //---------------------------------------------------------------
001200180117      //?Definizione costanti.
001300180117      //---------------------------------------------------------------
001400180117
001500180117      //---------------------------------------------------------------
001600180117      //?Definizione strutture dati.
001700180117      //---------------------------------------------------------------
001800180117     d kpjba         e ds                  inz
001900180117
002000180117      // - Ricerca/Controllo tabelle
002100180117     d TIBS02DS      e ds                  qualified inz
002200180117
002300180117      // - Tabella DVD
002400180117     d dDVD          e ds                  qualified inz
002500180117
002600180117      // - Passaggio dati
002700180117     d TRULDVDDS     e ds                  qualified
002800180117
002900180117      // - Numeratore
003000180117     d TRUL33DS      e ds                  qualified inz
003100180117
003200180117      //---------------------------------------------------------------
003300180117      //?Definizione variabili globali.
003400180117      //---------------------------------------------------------------
003500180117      // - Flags booleani
003600180117     d Fine            s               n   inz(*off)
003700180117
003800180117      //---------------------------------------------------------------
003900180117      //?Programmi richiamati
004000180117      //---------------------------------------------------------------
004100180117
004200180117      //---------------------------------------------------------------
004300180117      //?Prototipi.
004400180117      //---------------------------------------------------------------
004500180117      /copy gaitrasrc/srcprotopr,tibs02r
004600180117      /copy gaitrasrc/srcprotopr,trul33r
004700180117
004800180117      //---------------------------------------------------------------
004900180117      //?M A I N - L I N E
005000180117      //---------------------------------------------------------------
005100180117     c     *Entry        plist
005200180117     c                   parm                    TRULDVDDS
005300180117
005400180117       //?Operazioni iniziali
005500180117       exsr RoutInz;
005600180117
005700180117       //?Elaborazione dati
005800180117       IF  not Fine;
005900180117         exsr Elabora;
006000180117       ENDIF;
006100180117
006200180117       //?Operazioni finali
006300180117       exsr RoutEnd;
006400180117
006500180117       //--------------------------------------------------------------
006600180117       //?Operazioni iniziali.
006700180117       //--------------------------------------------------------------
006800180117       BEGSR RoutInz;
006900180117
007000180117       //?Controllo se ci sono tutti i parametri obbligatori
007100180117         IF  truldvdds.codtask = *blanks;
007200180117           truldvdds.esito = '1';
007300180117           truldvdds.errore = 'Codice TASK non passato';
007400180117           Fine = *on;
007500180117           leavesr;
007600180117         ENDIF;
007601180214         IF  %subst(truldvdds.codtask:1:3) = 'POD' and
007602180213             truldvdds.filiale = *zeros;
007603180213           truldvdds.esito = '1';
007604180213           truldvdds.errore = 'Codice filiale non passato';
007605180213           Fine = *on;
007606180213           leavesr;
007608180213         ENDIF;
007609180213         IF  truldvdds.path = *blanks;
007610180213           truldvdds.esito = '1';
007611180213           truldvdds.errore = 'Percorso non passato';
007612180213           Fine = *on;
007613180213           leavesr;
007614180213
007615180213         ENDIF;
007800180213         IF  truldvdds.text1 = *blanks or truldvdds.text2 = *blanks or
007900180117             truldvdds.text3 = *blanks;
008000180117           truldvdds.esito = '1';
008100180213           truldvdds.errore = 'Testo Cover non passato';
008200180117           Fine = *on;
008300180117           leavesr;
008400180117         ENDIF;
008401180213         IF  truldvdds.copies = *zeros;
008402180213           truldvdds.copies = 1;
008403180213         ENDIF;
008500180117
008600180117       ENDSR;
008700180117
008800180117       //--------------------------------------------------------------
008900180117       //?Elaborazione.
009000180117       //--------------------------------------------------------------
009100180117       BEGSR Elabora;
009200180117
009300180117         //Come prima cosa stacco il numero ID
009400180117         clear trul33ds;
009500180117         trul33ds.I33OPE = *zeros;
009600180117         trul33ds.I33CNU = 800;
009700180117         trul33ds.I33NUM = 1;
009800180117         KPJBU = trul33ds;
009900180117         trul33r (KPJBA);
010000180117         trul33ds = KPJBU;
010100180117         IF  trul33ds.O33err <> *zeros;
010200180117           truldvdds.esito = '1';
010300180117           truldvdds.errore = 'Errore numeratore 800';
010400180117           Fine = *on;
010500180117           leavesr;
010600180117         ENDIF;
010700180117
010800180117         //Poi aggancio la tabella DVD per i dati fissi legati al codice TASK
010900180117         clear TIBS02DS;
011000180117         clear dDVD;
011100180117         tibs02ds.T02mod = 'C';
011200180117         tibs02ds.T02cod = 'DVD';
011300180117         tibs02ds.T02ke1 = %subst(truldvdds.codtask:1:3);
011400180117         TNTBE_RicercaControllo (kpjba:tibs02ds);
011500180117         IF  tibs02ds.T02err <> *blanks;
011600180117           truldvdds.esito = '1';
011700180117           truldvdds.errore = 'Codice TASK errato';
011800180117           Fine = *on;
011900180117           leavesr;
012000180117         ENDIF;
012100180117         dDVD = tibs02ds.T02uni;
012200180117
012300180117         clear TIDVD000;
012400180117         DVDidtask = trul33ds.O33nrf;
012500180117         DVDcodtask = truldvdds.codtask;
012600180117         DVDcover = ddvd.§DVDCOVER;
012700180117         DVDfiliale = truldvdds.filiale;
012800180117         DVDcopies = truldvdds.copies;
012900180117         DVDroot = ddvd.§DVDROOT;
013000180117         DVDpath = truldvdds.path;
013100180117         DVDdataimm = %timestamp();
013200180117         DVDdtaburn = *loval;
013300180117         DVDtext1 = truldvdds.text1;
013400180117         DVDtext2 = truldvdds.text2;
013500180117         DVDtext3 = truldvdds.text3;
013501180124         DVDkeyas = truldvdds.keyas;
013600180117
013700180117         //tolgo dal campo TEXT1 (ragione sociale) eventuali apici
013701180124         IF  %scan('''':DVDtext1) > *zeros;
013800180124           DVDtext1 = %replace(' ':DVDtext1:%scan('''':DVDtext1));
013803180124         ENDIF;
013900180117
014000180117         write TIDVD000;
014100180117
014200180117       ENDSR;
014300180117
014400180117       //--------------------------------------------------------------
014500180117       //?Operazioni finali.
014600180117       //--------------------------------------------------------------
014700180117       BEGSR RoutEnd;
014800180117
014900180117         *inLR = *on;
015000180117         return;
015100180117
015200180117       ENDSR;
