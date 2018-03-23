000100120621       //==============================================================
000200120621       //?TEST - STAMPA COPIA LDV X SCANNERIZZAZIONE DOCUMENTI CLIENTI ?
000300120621       //==============================================================
000400120621
000500120621       //--------------------------------------------------------------
000600120621       //?Parametri di compilazione (vedi *cmd UBCRTOBJ).              ?
000700120621       //--------------------------------------------------------------
000800120621
000900120622     /*PRM  dbgview(*source)
001000120622     /*CMD  ovrdbf file(TITAS30C) tofile(GAITRAGRPS/TITAS30C) +
001100120622     /*CMD         ovrscope(*calllvl)
001200120622     /*END  dltovr file(TITAS30C) lvl(*)
001300120621     /*END
001400120621
001500120621       //--------------------------------------------------------------
001600120621       //?Specifiche di controllo.                                     ?
001700120621       //--------------------------------------------------------------
001800120621
001900120621     h decedit('0,') datedit(*dmy/) option(*nodebugio)
002000120621     h dftactgrp(*no)
002100120621     h bnddir('TRUL')
002200120621
002300120621       //--------------------------------------------------------------
002400120621       //?Dichiarazione file.
002500120621       //--------------------------------------------------------------
002600120621
002700120621       // -?Archivio Bolle Sede?
002800120622     fTITAS30C  if   e           k disk    extfile(wLibTITAS)
002900120622     f                                     usropn
003000120621
003100120621       // -?Video?
003200120621     fTNSB25TSTDcf   e             workstn
003300120621     f                                     indds(IndDspF)
003400120621     f                                     infds(InfDspF)
003500120621
003600120621       //--------------------------------------------------------------
003700120621       //?Definizione costanti.
003800120621       //--------------------------------------------------------------
003900120621
004000120621       // -?Tasti funzionali a video?
004100120621     d c_F01           c                   const(x'31')
004200120621     d c_F02           c                   const(x'32')
004300120621     d c_F03           c                   const(x'33')
004400120621     d c_F04           c                   const(x'34')
004500120621     d c_F05           c                   const(x'35')
004600120621     d c_F06           c                   const(x'36')
004700120621     d c_F07           c                   const(x'37')
004800120621     d c_F08           c                   const(x'38')
004900120621     d c_F09           c                   const(x'39')
005000120621     d c_F10           c                   const(x'3A')
005100120621     d c_F11           c                   const(x'3B')
005200120621     d c_F12           c                   const(x'3C')
005300120621     d c_F13           c                   const(x'B1')
005400120621     d c_F14           c                   const(x'B2')
005500120621     d c_F15           c                   const(x'B3')
005600120621     d c_F16           c                   const(x'B4')
005700120621     d c_F17           c                   const(x'B5')
005800120621     d c_F18           c                   const(x'B6')
005900120621     d c_F19           c                   const(x'B7')
006000120621     d c_F20           c                   const(x'B8')
006100120621     d c_F21           c                   const(x'B9')
006200120621     d c_F22           c                   const(x'BA')
006300120621     d c_F23           c                   const(x'BB')
006400120621     d c_F24           c                   const(x'BC')
006500120621     d c_Enter         c                   const(x'F1')
006600120621     d c_RollDown      c                   const(x'F4')
006700120621     d c_RollUp        c                   const(x'F5')
006800120621
006900120621       //--------------------------------------------------------------
007000120621       //?Definizione schiere.
007100120621       //--------------------------------------------------------------
007200120621
007300120621       // -?Messaggi di errore?
007400120621     d $Msg            s             78    dim( 4) ctdata perrcd(1)
007500120621
007600120621       //--------------------------------------------------------------
007700120621       //?Definizione aree dati.
007800120621       //--------------------------------------------------------------
007900120621
008000120621       // -?Dati utente?
008100120621     d §AzUte        e ds                  extname(AZUTE00F)
008200120621     d                                     dtaara
008300120621     d §DatiUte      e ds                  extname(dDatiUte)
008400120621     d                                     dtaara
008500120621
008600120621       //--------------------------------------------------------------
008700120621       //?Definizione strutture dati.
008800120621       //--------------------------------------------------------------
008900120621
009000120621       // -?Status ds?
009100120621     d Status         sds
009200120621     d  SDSpgm           *proc
009300120621     d  SDSprm           *parms
009400120621     d  SDSusr               254    263
009500120621
009600120621       // -?InfDS?
009700120621     d InfDspF         ds
009800120621     d  dsp_aid              369    369a
009900120621
010000120621       // -?Indicatori su DspF?
010100120621     d IndDspF         ds
010200120621         // -?Emissione messaggio di errore?
010300120621     d   ErrMessage                    n   overlay(IndDspF : 28)
010400120621         // -?Posizionamento cursore & Segnalazione errore?
010500120621     d   PosCurSped                    n   overlay(IndDspF : 50)
010600120621         // -?Riemissione videata?
010700120621     d   ErrGenerico                   n   overlay(IndDspF : 99)
010800120621
010900120621       // -?Parametro Numero Spedizione?
011000120621     d ds_Spedizione   ds            16    inz
011100120621     d   V1Caas                       4s 0 inz
011200120621     d   V1Clnp                       3s 0 inz
011300120621     d   V1Cnrs                       2s 0 inz
011400120621     d   V1Cnsp                       7s 0 inz
011500120621
011600120621       // -?Parametri ricevuti?
011700120621     d KPJBA         e ds
011800120621
011900120621       //--------------------------------------------------------------
012000120621       //?Definizione variabili globali.
012100120621       //--------------------------------------------------------------
012200120621
012300120621       // -?Flags booleani?
012400120621     d $Fine           s               n   inz(*off)
012500120621     d $InzD01         s               n   inz(*on)
012600120622
012700120622       // -?Nomi estesi Libreria/File dei files?
012800120622     d wLibTITAS       s             21a   inz
012900120621
013000120621       // -?Campi di comodo?
013100120622     d SAVfl1          s                   inz(*off)  like(V1Cfl1)
013200120621
013300120621       //--------------------------------------------------------------
013400120621       //?Definizione procedure usate.
013500120621       //--------------------------------------------------------------
013600120621
013700120621       // -?Reperimento dati utente?
013800120621     d TIBS34ds      e ds
013900120621      /copy gaitrasrc/srcProtoPR,TIBS34R
014000120621
014100120621       // -?Interrogazione Bolle Sede?
014200120622     d TNSB50ds      e ds                  inz
014300120622     d tnsb50c         pr                  extpgm('TNSB50C')
014400120621     d   kpjba                             likeds(KPJBA)
014500120621
014600120621       // -?Stampa copia LdV?
014700120621     d TNSB25ds      e ds                  inz
014800120622     d   D25ges      e                     inz('T')
014900120625      /copy gaitrasrc/srcProtoPR,TNSB25R
015000120621
015100120621       //--------------------------------------------------------------
015200120621       //?Definizione key-list.
015300120621       //--------------------------------------------------------------
015400120621
015500120621       // -?File TITAS30C?
015600120621     d k05titas30    e ds                  extname(TITAS30C : *key)
015700120621     d                                     prefix(k_)
015800120621     d                                     inz
015900120621
016000120621       //--------------------------------------------------------------
016100120621       //?Riepilogo indicatori.
016200120621       //--------------------------------------------------------------
016300120621
016400120621     c     *Entry        plist
016500120621     c                   parm                    KPJBA
016600120621
016700120621      /free
016800120621
016900120621       // -?Start?
017000120621       *inLR = *on;
017100120621
017200120621       // -?Elab?
017300120621       dow  $Fine = *off;
017400120621         exsr  sr_GesD01;
017500120621       enddo;
017600120621
017700120621       // -?End?
017800120621       exsr  sr_EndPgm;
017900120621       return;
018000120621
018100120621       //--------------------------------------------------------------
018200120621       //?Operazioni iniziali
018300120621       //--------------------------------------------------------------
018400120621       BEGSR  *InzSR;
018500120621
018600120621         // -?Reperimento dati job?
018700120621         exsr  sr_DatiJob;
018800120621
018900120621         // -?Impostazione nome programma a video?
019000120621         VTDpgm = SDSpgm;
019100120621
019200120621       ENDSR;
019300120621
019400120621       //--------------------------------------------------------------
019500120621       //?Reperimento Dati del job (Utente/Operativi).
019600120621       //--------------------------------------------------------------
019700120621       BEGSR  sr_DatiJob;
019800120621
019900120621         in(E) §AzUte;
020000120621         if NOT %error;
020100120621           in(E) §DatiUte;
020200120621         endif;
020300120621         if %error or RSut = *blanks;
020400120621           clear TIBS34ds;
020500120621           tibs34r(tibs34ds);
020600120621           in §AzUte;
020700120621           in §DatiUte;
020800120621         endif;
020900120621
021000120621       ENDSR;
021100120621
021200120621       //--------------------------------------------------------------
021300120621       //?Gestione videata D01
021400120621       //--------------------------------------------------------------
021500120621       BEGSR  sr_GesD01;
021600120621
021700120621         // -?Inizializzazione videata?
021800120621         if  $InzD01 = *on;
021900120621           SAVfl1 = V1Cfl1;
022000120621           clear  SB25D01;
022100120621           V1Caas   = *year;
022200120621           if  SAVfl1 = *blank;
022300120622             V1Cfl1 = *off;
022400120621           else;
022500120621             V1Cfl1 = SAVfl1;
022600120621           endif;
022700120621           $InzD01  = *off;
022800120621         endif;
022900120621
023000120621         // -?Verifica correttezza del sistema informativo?
023100120621         if  %subst(knsif : 1 : 6) <> 'FILTRA';
023200120621           ErrMessage  = *on;
023300120621           ErrGenerico = *on;
023400120621           V1Dmsg = $Msg(01);
023500120621         endif;
023600120621
023700120621         // -?Emissione videata?
023800120621         write SB25T01;
023900120621         write SB25Z01;
024000120621         exfmt SB25D01;
024100120621
024200120621         reset ErrMessage;
024300120621         reset ErrGenerico;
024400120621         clear V1Dmsg;
024500120621
024600120621         clear V1Dtxt;
024700120621
024800120621         SELECT;
024900120621
025000120621           // -?Sistema Informativo errato?
025100120621           WHEN  %subst(knsif : 1 : 6) <> 'FILTRA';
025200120621             $Fine = *on;
025300120621
025400120621           // -?F3=Fine?
025500120621           WHEN  dsp_aid = c_F03;
025600120621             $Fine = *on;
025700120621
025800120621           // -?F4=Interrogazione bolle sede?
025900120621           WHEN  dsp_aid = c_F04;
026000120621             exsr  sr_Call_TNSB50R;
026100120621
026200120621           // -?F8=Chiusura spool?
026300120621           WHEN  dsp_aid = c_F08;
026400120621             exsr  sr_EndPgm;
026500120621
026600120621           // -?Invio o F6=Conferma?
026700120621           OTHER;
026800120621             exsr  sr_CtrD01;
026900120621             select;
027000120621               // -?rilevato errore?
027100120621               when  ErrGenerico = *on;
027200120621               // -?non premuto F6?
027300120621               when  dsp_aid <> c_F06;
027400120621               // -?selezionata una singola spedizione?
027500120621               when  V1Clnp  <> *zero   and
027600120621                     V1Cnsp  <> *zero;
027700120621                 exsr  sr_Call_TNSB25R;
027800120621                 if  Not ErrGenerico;
027900120621                   reset $InzD01;
028000120621                 endif;
028100120621             endsl;
028200120621
028300120621         ENDSL;
028400120621
028500120621       ENDSR;
028600120621
028700120621       //--------------------------------------------------------------
028800120621       //?Richiamo pgm. Interrogazione Bolle Arrivi.
028900120621       //--------------------------------------------------------------
029000120621       BEGSR  sr_Call_TNSB50R;
029100120621
029200120622         clear  TNSB50ds;
029300120622         I50op0 = 'B01';
029400120621         kpjbu = TNSB50ds;
029500120621
029600120622         tnsb50c (kpjba);
029700120621
029800120621         TNSB50ds = kpjbu;
029900120621
030000120621         if  O50F03 = *on  or
030100120621             O50F12 = *on  or
030200120621             O50err = *on;
030300120621           if  O50msg <> *blank;
030400120621             ErrMessage  = *on;
030500120621             ErrGenerico = *on;
030600120621             V1Dmsg = O50msg;
030700120621           endif;
030800120621           leavesr;
030900120621         endif;
031000120621
031100120621         clear ds_Spedizione;
031200120621         V1Caas = D50aas;
031300120621         V1Clnp = D50lnp;
031400120621         V1Cnrs = D50nrs;
031500120621         V1Cnsp = D50nsp;
031600120621
031700120621       ENDSR;
031800120621
031900120621       //--------------------------------------------------------------
032000120621       //?Richiamo stored procedure per immagini LdV
032100120621       //--------------------------------------------------------------
032200120621       BEGSR  sr_Call_TNSB25R;
032300120621
032400120622         reset   tnsb25ds;
032500120621         D25aas = V1Caas;
032600120621         D25lnp = V1Clnp;
032700120621         D25nrs = V1Cnrs;
032800120621         D25nsp = V1Cnsp;
032900120621         D25fl1 = V1Cfl1;
033000120621
033100120621         tnsb25r (tnsb25ds);
033200120621
033300120621         V1Dtxt = 'Generata stampa.';
033400120621         write  Protect;
033500120621         exfmt  SB25D02;
033600120621
033700120621         // -?F3=Fine?
033800120621         if  dsp_aid = c_F03;
033900120621           $Fine = *on;
034000120621           leavesr;
034100120621         endif;
034200120621
034300120621       ENDSR;
034400120621
034500120621       //--------------------------------------------------------------
034600120621       //?Controllo dati in videata D01
034700120621       //--------------------------------------------------------------
034800120621       BEGSR  sr_CtrD01;
034900120621
035000120621         %subst(IndDspF : 50) = *off;
035100120621
035200120621         select;
035300120621           when  V1Caas  = *zero;
035400120621             V1Caas  = *year;
035500120621           when  V1Caas  > 100;
035600120621           when  V1Caas  > 50;
035700120621             V1Caas += 1900;
035800120621           when  V1Caas <= 50;
035900120621             V1Caas += 2000;
036000120621         endsl;
036100120621
036200120621         SELECT;
036300120621
036400120621           // -?Nessuna spedizione inserita?
036500120621           WHEN  V1Clnp = *zero   and
036600120621                 V1Cnrs = *zero   and
036700120621                 V1Cnsp = *zero;
036800120621             ErrMessage  = *on;
036900120621             ErrGenerico = *on;
037000120621             PosCurSped  = *on;
037100120621             V1Dmsg = $Msg(02);
037200120621             leavesr;
037300120621
037400120621           // -?Selezionata una singola spedizione?
037500120621           WHEN  V1Clnp <> *zero   or
037600120621                 V1Cnrs <> *zero   or
037700120621                 V1Cnsp <> *zero;
037800120621             k_TASaas = V1Caas;
037900120621             k_TASlnp = V1Clnp;
038000120621             k_TASnrs = V1Cnrs;
038100120621             k_TASnsp = V1Cnsp;
038200120621             if  not %open(TITAS30C);
038300120622               if  %subst ( KNSIF : 7 : 1 ) = 'P';
038400120622                 wLibTITAS = 'GAITRAGRPS/TITAS30C';
038500120622               else;
038600120622                 wLibTITAS = 'GAITRAGRU/TITAS30C';
038700120622               endif;
038800120621               open  TITAS30C;
038900120621             endif;
039000120621             chain  %kds(K05titas30 : 4)  TITAS30C;
039100120621             if  not %found(TITAS30C);
039200120621               ErrMessage  = *on;
039300120621               ErrGenerico = *on;
039400120621               PosCurSped  = *on;
039500120621               V1Dmsg = $Msg(03);
039600120621               leavesr;
039700120621             endif;
039800120621
039900120621         ENDSL;
040000120621
040100120621       ENDSR;
040200120621
040300120621       //--------------------------------------------------------------
040400120621       //?Fine: chiusura dei files di stampa
040500120621       //--------------------------------------------------------------
040600120621       BEGSR  sr_EndPgm;
040700120621
040800120621         clear   tnsb25ds;
040900120621         D25tla = 'C';
041000120621         tnsb25r (tnsb25ds);
041100120621
041200120621       ENDSR;
041300120621
041400120621      /end-free
041500120621
041600120621** - $Msg:?Messaggi di Errore?-----------------------------------------------*
041700120621Programma NON eseguibile nel sistema informativo di SEDE. "Invio" per uscire.
041800120621Nessuna parzializzazione inserita
041900120621Spedizione inesistente
042000120621Programma NON previsto
