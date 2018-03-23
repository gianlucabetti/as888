000100100929       //==============================================================
000200100929       //?G E N E R A Z I O N E    B O Z Z A   I M M A G I N E   L D V?
000300100929       //?                       P E R   P D A                        ?
000400100929       //==============================================================
000500110517
000600110517       //--------------------------------------------------------------
000700110517       //?Parametri di compilazione (vedi *cmd UBCRTOBJ).              ?
000800110517       //--------------------------------------------------------------
000900110517
001000110517     /*END
001100100928
001200100928       //--------------------------------------------------------------
001300100928       //?Specifiche di controllo.                                     ?
001400100928       //--------------------------------------------------------------
001500030623
001600100422     h decedit('0,') datedit(*dmy/) option(*nodebugio)
001700100422     h dftactgrp(*no)
001800030623
001900100928       //--------------------------------------------------------------
002000100928       //?Dichiarazione file.                                          ?
002100100928       //--------------------------------------------------------------
002200030623
002300110517     fFISP30IMD cf   e             workstn
002400110517     f                                     indds(IndDspF)
002500110517     f                                     infds(InfDspF)
002600110517
002700110517     fFISP30IMP o    e             printer usropn
002800110517     f*//                                  infds(Info_Pda)
002900081024     f                                     rename(SP30BOX1:PDA_BOX1)
003000081024     f                                     rename(SP30LDV1:PDA_LDV1)
003100081024     f                                     rename(SP30SEP :PDA_SEP)
003200081024     f                                     rename(SP30NUL :PDA_NUL)
003300081024     f                                     rename(SP30AR53:PDA_AR53)
003400081024     f                                     rename(SP30DPD :PDA_DPD)
003500100429     f                                     rename(SP30FED :PDA_FED)
003600100929     f                                     rename(SP30ROW :PDA_ROW)
003700100929     f                                     rename(SP30R30 :PDA_R30)
003800030623
003900100928       //--------------------------------------------------------------
004000100928       //?Definizione costanti.                                        ?
004100100928       //--------------------------------------------------------------
004200030623
004300100929     d c_Char          c                   const('*...+....1....+....2+
004400100929     d                                            ....+....3....+....4+
004500100929     d                                            ....+....5....+....6+
004600100929     d                                            ....+....7....+....8+
004700100929     d                                            ....+....9....+...10+
004800100929     d                                            ....+...11....+...12')
004900100929
005000110516     d*// $Bartolini      c                   const('BARTOLINI S.p.A.')
005100110516     d $BRT            c                   const('BRT S.p.A.')
005200060912     d $Intest1        c                   const('Sede Operativa ed Am-
005300060912     d                                     ministrativa:')
005400060912     d $Intest2        c                   const('via E. Mattei, 42 -
005500060912     d                                     - 40138 BOLOGNA')
005600060912     d $Intest3        c                   const('Sede Legale: Piazza -
005700060912     d                                     Diaz, 7 - 20123 Milano')
005800060912     d $Intest4x       c                   const('P.IVA 03987100371')
005900070215     d $Intest4        c                   const('C.F. e P.IVA 04507990150')
006000070215     d $Intest5x       c                   const('Capitale Sociale Eur-
006100060912     d                                     o 100.000.000 int. vers.')
006200070215     d $Intest5        c                   const(' Capitale Sociale Eu-
006300070215     d                                     ro 24.000.000 int. vers.')
006400060912     d $Intest6        c                   const('info on-line')
006500120124     d $Intest7        c                   const('www.brt.it')
006600100929
006700100929     d c_PMxA          c                   const('PROMEMORIA PER L''AU-
006800100929     d                                     TOTRASPORTATORE')
006900100929
007000100929       // -?Numero di righe da gestire per la LdV?
007100100929     d c_Row           c                   const(36)
007200100929     d c_Row_B         c                   const(49)
007300100929     d c_Row_Fed       c                   const(10)
007400110517
007500110517       // -?Tasti funzionali a video?
007600110517     d c_F01           c                   const(x'31')
007700110517     d c_F02           c                   const(x'32')
007800110517     d c_F03           c                   const(x'33')
007900110517     d c_F04           c                   const(x'34')
008000110517     d c_F05           c                   const(x'35')
008100110517     d c_F06           c                   const(x'36')
008200110517     d c_F07           c                   const(x'37')
008300110517     d c_F08           c                   const(x'38')
008400110517     d c_F09           c                   const(x'39')
008500110517     d c_F10           c                   const(x'3A')
008600110517     d c_F11           c                   const(x'3B')
008700110517     d c_F12           c                   const(x'3C')
008800110517     d c_F13           c                   const(x'B1')
008900110517     d c_F14           c                   const(x'B2')
009000110517     d c_F15           c                   const(x'B3')
009100110517     d c_F16           c                   const(x'B4')
009200110517     d c_F17           c                   const(x'B5')
009300110517     d c_F18           c                   const(x'B6')
009400110517     d c_F19           c                   const(x'B7')
009500110517     d c_F20           c                   const(x'B8')
009600110517     d c_F21           c                   const(x'B9')
009700110517     d c_F22           c                   const(x'BA')
009800110517     d c_F23           c                   const(x'BB')
009900110517     d c_F24           c                   const(x'BC')
010000110517     d c_Enter         c                   const(x'F1')
010100110517     d c_RollDown      c                   const(x'F4')
010200110517     d c_RollUp        c                   const(x'F5')
010300100928
010400100928       //--------------------------------------------------------------
010500100928       //?Definizione schiere.                                         ?
010600100928       //--------------------------------------------------------------
010700100928
010800100928
010900100928       //--------------------------------------------------------------
011000100928       //?Definizione aree dati.                                       ?
011100100928       //--------------------------------------------------------------
011200100928
011300100928       // -?Dati utente?
011400100928     d §AzUte        e ds                  extname(AZUTE00F)
011500100928     d                                     dtaara
011600100928     d §DatiUte      e ds                  extname(dDatiUte)
011700100928     d                                     dtaara
011800100928
011900100928       //--------------------------------------------------------------
012000100928       //?Definizione strutture dati.                                  ?
012100100928       //--------------------------------------------------------------
012200100928
012300100928       // -?Status ds?
012400030623     d Status         sds           333
012500100929     d  SDSpgm           *proc
012600110517     d*//  SDSprm           *parms
012700100929     d  SDSdta               191    198
012800100929     d  SDSjob               244    253
012900030623     d  SDSusr               254    263
013000110517
013100110517       // -?InfDS?
013200110517     d InfDspF         ds
013300110517     d   dsp_aid             369    369a
013400110517
013500110517       // -?Indicatori su DspF?
013600110517     d IndDspF         ds                  inz
013700110517         // -?Abilitazione tasti funzionali?
013800110517         // -?Emissione messaggio di errore?
013900110517     d   ErrMessage                    n   overlay(IndDspF : 28)
014000110517         // -?Indicatori per Attibuti di visualizzazione?
014100110517         // -?Posizionamento cursore & Segnalazione errore?
014200110517         // -?Riemissione videata?
014300110517     d   ErrGenerico                   n   overlay(IndDspF : 99)
014400100928
014500100928       // -?InfDS?
014600110517     d*// Info_Pda        ds
014700110517     d*//  CurLinePda           367    368i 0
014800110517
014900110517       // -?Dati da restituire?                ?
015000110518     d $LdV_ds         ds           123    occurs(c_Row_B)  inz
015100110517     d   LdvPrg                       3s 0 inz
015200110517     d   LdvTxt                     120    inz
015300100928
015400100928       // -?Parametri per Reperimento dati utente?
015500100928     d TIBS34ds      e ds                  inz
015600100928
015700100928       // -?KPJBA?
015800100928     d KPJBA         e ds
015900100929
016000100929       // -?Records in output?                ?
016100100929       // ·? 1ª riga?
016200100929     d ds_01         e ds                  inz  extname(FISP3001ds)
016300100929     d                                          prefix(P : 3)
016400100929       // ·? 2ª riga (vuota)?
016500100929       // ·? 3ª riga?
016600100929     d ds_03         e ds                  inz  extname(FISP3003ds)
016700100929     d                                          prefix(P : 3)
016800100929       // ·? 4ª riga?
016900100929     d ds_04         e ds                  inz  extname(FISP3004ds)
017000100929     d                                          prefix(P : 3)
017100100929       // ·? 5ª riga?
017200100929     d ds_05         e ds                  inz  extname(FISP3005ds)
017300100929     d                                          prefix(P : 3)
017400100929       // ·? 6ª riga (vuota)?
017500100929       // ·? 7ª riga?
017600100929     d ds_07         e ds                  inz  extname(FISP3007ds)
017700100929     d                                          prefix(P : 3)
017800100929       // ·? 8ª riga (vuota)?
017900100929       // ·? 9ª riga?
018000100929     d ds_09         e ds                  inz  extname(FISP3009ds)
018100100929     d                                          prefix(P : 3)
018200100929       // ·?10ª riga?
018300100929     d ds_10         e ds                  inz  extname(FISP3010ds)
018400100929     d                                          prefix(P : 3)
018500100929       // ·?11ª riga?
018600100929     d ds_11         e ds                  inz  extname(FISP3011ds)
018700100929     d                                          prefix(P : 3)
018800100929       // ·?12ª riga?
018900100929     d ds_12         e ds                  inz  extname(FISP3012ds)
019000100929     d                                          prefix(P : 3)
019100100929       // ·?13ª riga?
019200100929     d ds_13         e ds                  inz  extname(FISP3013ds)
019300100929     d                                          prefix(P : 3)
019400100929       // ·?14ª riga?
019500100929     d ds_14         e ds                  inz  extname(FISP3014ds)
019600100929     d                                          prefix(P : 3)
019700140113     d ds_140        e ds                  inz  extname(FISP30140s)
019800140113     d                                          prefix(P : 3)
019900100929       // ·?15ª riga?
020000110511     d ds_15         e ds                  inz  extname(FISP3015ds)
020100100929     d                                          prefix(P : 3)
020200100929       // ·?16ª riga?
020300110511     d ds_16         e ds                  inz  extname(FISP3016ds)
020400100929     d                                          prefix(P : 3)
020500100929       // ·?17ª riga?
020600110511     d ds_17         e ds                  inz  extname(FISP3017ds)
020700100929     d                                          prefix(P : 3)
020800100929       // ·?18ª riga?
020900110511     d ds_18         e ds                  inz  extname(FISP3018ds)
021000100929     d                                          prefix(P : 3)
021100140113     d ds_180        e ds                  inz  extname(FISP30180s)
021200140113     d                                          prefix(P : 3)
021300100929       // ·?19ª riga?
021400100929     d ds_19         e ds                  inz  extname(FISP3019ds)
021500100929     d                                          prefix(P : 3)
021600100929       // ·?20ª riga?
021700100929     d ds_20         e ds                  inz  extname(FISP3020ds)
021800100929     d                                          prefix(P : 3)
021900100929       // ·?21ª riga?
022000110511     d ds_21         e ds                  inz  extname(FISP3021ds)
022100100929     d                                          prefix(P : 3)
022200100929       // ·?22ª riga?
022300110511     d ds_22         e ds                  inz  extname(FISP3022ds)
022400100929     d                                          prefix(P : 3)
022500100929       // ·?23ª riga?
022600110511     d ds_23         e ds                  inz  extname(FISP3023ds)
022700100929     d                                          prefix(P : 3)
022800100929       // ·?24ª riga?
022900110511     d ds_24         e ds                  inz  extname(FISP3024ds)
023000100929     d                                          prefix(P : 3)
023100100929       // ·?25ª riga?
023200100929     d ds_25         e ds                  inz  extname(FISP3025ds)
023300100929     d                                          prefix(P : 3)
023400100929       // ·?26ª riga (vuota)?
023500100929       // ·?27ª riga?
023600100929     d ds_27         e ds                  inz  extname(FISP3027ds)
023700100929     d                                          prefix(P : 3)
023800100929       // ·?28ª riga?
023900100929     d ds_28         e ds                  inz  extname(FISP3028ds)
024000100929     d                                          prefix(P : 3)
024100100929       // ·?29ª riga?
024200100929     d ds_29         e ds                  inz  extname(FISP3029ds)
024300100929     d                                          prefix(P : 3)
024400100929       // ·?30ª riga?
024500100929     d ds_30         e ds                  inz  extname(FISP3030ds)
024600100929     d                                          prefix(P : 3)
024700110517       // ·?31ª riga?
024800110517     d ds_31         e ds                  inz  extname(FISP3031ds)
024900110517     d                                          prefix(P : 3)
025000100929       // ·?32ª riga?
025100100929     d ds_32         e ds                  inz  extname(FISP3032ds)
025200100929     d                                          prefix(P : 3)
025300100929       // ·?33ª riga?
025400100929     d ds_33         e ds                  inz  extname(FISP3033ds)
025500100929     d                                          prefix(P : 3)
025600100929       // ·?34ª riga?
025700100929     d ds_34         e ds                  inz  extname(FISP3034ds)
025800100929     d                                          prefix(P : 3)
025900100929       // ·?35ª riga (vuota)?
026000100929       // ·?36ª riga (vuota)?
026100100929       // ·?37ª riga e successive (Promemoria per AUT)?
026200100929     d ds_37         e ds                  inz  extname(FISP3037ds)
026300100929     d                                          prefix(P : 3)
026400100929       // ·?Righe x FedEx?
026500100929     d ds_Fed        e ds                  inz  extname(FISP304Fds)
026600100929     d                                          prefix(P : 3)
026700100928
026800100928       //--------------------------------------------------------------
026900100928       //?Definizione variabili globali.                               ?
027000100928       //--------------------------------------------------------------
027100110517
027200110517       // -?Flags booleani?
027300110517     d $Fine           s               n   inz
027400110518     d $InzD01         s               n   inz(*on)
027500100928
027600100928       // -?Indici di schiera?
027700100928     d xx              s              3  0 inz
027800100929     d wx              s              3  0 inz
027900100929
028000100929       // -?Campi di comodo?
028100100929     d w_Row           s              3  0 inz(c_Row)
028200100929     d Data_EUR        s               d   datfmt(*eur)   inz(*loval)
028300100928
028400100928       //--------------------------------------------------------------
028500100928       //?Definizione prototipi procedure.                             ?
028600100928       //--------------------------------------------------------------
028700100928
028800100928       // -?Reperimento dati utente?
028900100928      /copy gaitrasrc/srcProtoPR,TIBS34R
029000100928
029100100928
029200100928       //--------------------------------------------------------------
029300100928       //?Definizione key-list.                                        ?
029400100928       //--------------------------------------------------------------
029500100928
029600030623
029700100928       //==============================================================
029800100928       //?Indicatori usati :                                           ?
029900100928       //--------------------------------------------------------------
030000100928       // 20    - Stampa CONTRASSEGNO
030100100928       // 21    - Stampa TOTALE DA ESIGERE ALLA CONSEGNA / AL RITIRO
030200100928       // 35    - Richiesta FIRMA, DATA e ORA   in 1ª copia
030300100928       //         per ricevuta della merce senza riserve
030400100928       //         e indica "TOTALE DA ESIGERE ALLA CONSEGNA"
030500100928       // 72    - Trattasi di BOLLA FedEx
030600100928       //==============================================================
030700100928
030800100928       //--------------------------------------------------------------
030900110517       //?Specifiche di calcolo.                                       ?
031000100928       //--------------------------------------------------------------
031100030623
031200030623     c     *Entry        plist
031300110517     c                   parm                    KPJBA
031400110517
031500110517     c*//                parm                    £Mod              1
031600110517     c*//                parm                    £Dati             1
031700100928
031800100929       // £Mod  - Tipo modulo: " " = Immagine vuota  x PDA
031900100929       //                      "B" = Immagine vuota  x PDA con box "promemoria"
032000100929       //                      "D" = Immagine vuota  x PDA con logo DPD
032100100929       //                      "F" = Immagine vuota  x PDA con logo FedEx
032200110516       // £Dati - Dati richiesti: "X" = Tutte le righe "*...+....1....+'
032300110516       //                         "W" = Dati di test...
032400100928
032500100928      /free
032600100928
032700110517       // -?Operazioni iniziali?
032800110517       exsr  sr_RoutInz;
032900110517
033000110517       // -?Gestione videata?
033100110517       DoW  $Fine = *off;
033200110517         exsr  sr_GesD01;
033300110517       EndDo;
033400100929
033500110517       // -?Operazioni finali?
033600100929       return;
033700110517
033800110517       //--------------------------------------------------------------
033900110517       //?Routine Iniziale.                                            ?
034000110517       //--------------------------------------------------------------
034100110517       BEGSR  sr_RoutInz;
034200110517
034300110517         *inLR = *on;
034400110517
034500110517         in(e) §AzUte;
034600110517         if NOT %error;
034700110517           in(e) §DatiUte;
034800110517         endif;
034900110517         if %error or RSut = *blank;
035000110517           tibs34r ( tibs34ds );
035100110517           in §AzUte;
035200110517           in §DatiUte;
035300110517         endif;
035400110517
035500110517         Data_Eur = %date();
035600110517
035700110517       ENDSR;
035800110517
035900110517       //--------------------------------------------------------------
036000110517       //?Gestione videata D01 (unica).                                ?
036100110517       //--------------------------------------------------------------
036200110517       BEGSR  sr_GesD01;
036300110517
036400110517         // -?Inizializzazione videata?
036500110517         if  $InzD01 = *on;
036600110518           VTDpgm = SDSpgm;
036700110517           clear  £Mod;
036800110517           //clear  £Dati;
036900110517           $InzD01 = *off;
037000110517         endif;
037100110517
037200110517         // -?Emissione videata?
037300110517         exfmt  SP3ID01;
037400110517
037500110517         clear  V1Dmsg;
037600110517         reset  ErrMessage;
037700110517         reset  ErrGenerico;
037800110517
037900110517         SELECT;
038000110517
038100110517           // -?F3=Fine?
038200110517           WHEN  dsp_aid = c_F03;
038300110517             $Fine = *on;
038400110517
038500110517           // -?Invio?
038600110517           OTHER;
038700110518             clear  V1Dtxt;
038800110517             exsr sr_CtrD01;
038900110517             Select;
039000110517               When  ErrGenerico;
039100110517                 leavesr;
039200110517               When  dsp_aid <> c_F06;
039300110517                 leavesr;
039400110517               Other;
039500110517                 exsr  sr_Stampa;
039600110517                 $InzD01 = *on;
039700110517             EndSl;
039800110517
039900110517         ENDSL;
040000110517
040100110517       ENDSR;
040200110517
040300110517       //--------------------------------------------------------------
040400110517       //?Stampa singola immagine.                                     ?
040500110517       //--------------------------------------------------------------
040600110517       BEGSR  sr_CtrD01;
040700110517
040800110517         %subst(IndDspF : 50) = *off;
040900110517
041000110517       ENDSR;
041100110517
041200110517       //--------------------------------------------------------------
041300110517       //?Stampa singola immagine.                                     ?
041400110517       //--------------------------------------------------------------
041500110517       BEGSR  sr_Stampa;
041600110517
041700110517         // -?Pulizia del PRTF?
041800110517         exsr  sr_Clr_PRTF;
041900110517
042000110517         // -?Stampa?
042100110517         exsr  sr_LdV_Vuota;
042200110517
042300110517         //if  %parms() > 1   and   %addr(£Dati) <> *null
042400110517         //                   and   £Dati <> *blank;
042500110517         if  £Dati <> *blank;
042600110517           exsr  sr_LdV_Piena;
042700110517         endif;
042800110517
042900110517         exsr  sr_Prt_LdV;
043000110518
043100110518         V1Dtxt = 'Stampata "LDV di base"';
043200110518         select;
043300110518           when  £Mod = *blank;
043400110518             V1Dtxt = %trimr(V1Dtxt) + ' standard';
043500110518           when  £Mod = 'B';
043600110518             V1Dtxt = %trimr(V1Dtxt) + ' con box aggiuntivo';
043700110518           when  £Mod = 'D';
043800110518             V1Dtxt = %trimr(V1Dtxt) + ' con logo DPD';
043900110518           when  £Mod = 'F';
044000110518             V1Dtxt = %trimr(V1Dtxt) + ' con logo e box FedEx';
044100110518         endsl;
044200110518         select;
044300110518           when  £Dati = *blank;
044400110518             V1Dtxt = %trimr(V1Dtxt) + ' vuoto';
044500110518           when  £Dati = 'W';
044600110518             V1Dtxt = %trimr(V1Dtxt) + ' con dati di prova';
044700110518           when  £Dati = 'X';
044800110518             V1Dtxt = %trimr(V1Dtxt) + ' con righe complete';
044900110518         endsl;
045000110517
045100110517       ENDSR;
045200030912
045300100928       //--------------------------------------------------------------
045400100928       //?Pulizia del PrtF                                             ?
045500100928       //--------------------------------------------------------------
045600100929       BEGSR  sr_Clr_PRTF;
045700100928
045800110518         // -?Apertura PrtF?
045900081024         open   FISP30IMP;
046000081024
046100110518         // -?Pulizia PrtF?
046200081024         clear  PDA_BOX1;
046300110518         //clear  PDA_LDV1;        (senza campi)?
046400110518         //clear  PDA_SEP;         (senza campi)?
046500110518         //clear  PDA_NUL;         (senza campi)?
046600081024         clear  PDA_AR53;
046700081024         clear  PDA_DPD;
046800100429         clear  PDA_FED;
046900100928
047000100928         %subst(*in : 01) = *off;
047100100929
047200110518         clear  ds_01;
047300110518         //clear  ds_02;           (riga vuota)?
047400110518         clear  ds_03;
047500110518         clear  ds_04;
047600110518         clear  ds_05;
047700110518         //clear  ds_06;           (riga vuota)?
047800110518         clear  ds_07;
047900110518         //clear  ds_08;           (riga vuota)?
048000110518         clear  ds_09;
048100110518         clear  ds_10;
048200110518         clear  ds_11;
048300110518         clear  ds_12;
048400110518         clear  ds_13;
048500110518         clear  ds_14;
048600110518         clear  ds_15;
048700110518         clear  ds_16;
048800110518         clear  ds_17;
048900110518         clear  ds_18;
049000110518         clear  ds_19;
049100110518         clear  ds_20;
049200110518         clear  ds_21;
049300110518         clear  ds_22;
049400110518         clear  ds_23;
049500110518         clear  ds_24;
049600110518         clear  ds_25;
049700110518         //clear  ds_26;           (riga vuota)?
049800110518         clear  ds_27;
049900110518         clear  ds_28;
050000110518         clear  ds_29;
050100110518         clear  ds_30;
050200110518         clear  ds_31;
050300110518         clear  ds_32;
050400110518         clear  ds_33;
050500110518         clear  ds_34;
050600110518         clear  ds_37;
050700110518         clear  ds_Fed;
050800110518
050900110518         // -?Pulizia schiera con dati in stampa?
051000110518         For  wx = 1  To  c_Row_B;
051100110518           %occur($LdV_ds) = wx;
051200110518           clear  $LdV_ds;
051300110518         EndFor;
051400110518         clear  wx;
051500100928
051600100928       ENDSR;
051700080725
051800100928       //--------------------------------------------------------------
051900100928       //?Stampa di LdV vuota per l'immagine del PDA                   ?
052000100928       //--------------------------------------------------------------
052100100929       BEGSR  sr_LdV_Vuota;
052200080725
052300100928         // -?Indicatori utilizzati:?
052400100929         exsr  sr_Set_IND;
052500080725
052600100928         // -?Costanti: dati Bartolini S.p.A.?
052700110516         //TBAR  = $Bartolini;
052800110516         TBAR  = $BRT;
052900080725         TBA1  = $Intest1;
053000080725         TBA2  = $Intest2;
053100080725         TBA3  = $Intest3;
053200080725         TBA4  = $Intest4;
053300080725         TBA5  = $Intest5;
053400080725         TBA7  = $Intest7;
053500100929
053600100929       ENDSR;
053700100929
053800100929       //--------------------------------------------------------------
053900100929       //?Impostazione indicatori utilizzati                           ?
054000100929       //--------------------------------------------------------------
054100100929       BEGSR  sr_Set_IND;
054200100929
054300100929         // -?Box contrassegno (se non DPD)?
054400110517         //*in20 = (%parms() = *zero  or   %addr(£Mod) =  *null  or
054500110517         //        (%parms() > *zero  and  %addr(£Mod) <> *null
054600110517         //                           and  £Mod <> 'D'));
054700110517         *in20 = (£Mod <> 'D');
054800100929
054900100929         // -?Box totale da esigere?...alla consegna (mai al ritiro)?
055000100929         *in21 = *in20;
055100100929
055200100929         // -?Richiesta nome e cognome, firma, data e ora consegna?
055300100929         //  ?nella 1ª copia?...alla consegna (mai al ritiro)?
055400110517         //*in35 = (%parms() = *zero  or   %addr(£Mod) =  *null  or
055500110517         //        (%parms() > *zero  and  %addr(£Mod) <> *null
055600110517         //                           and   £Mod <> 'F'));
055700110517         *in35 = (£Mod <> 'F');
055800100929         *in36 = *off;
055900100929
056000100929         // -?Non stampa del barcode?
056100100929         *in50 = *off;
056200100929
056300100929         select;
056400100929
056500100929           // -?x DPD?
056600110517           ////when  %parms() > *zero  and  %addr(£Mod) <> *null
056700110517           ////                        and  £Mod = 'D';
056800110517           //when  £Mod = 'D';
056900100929           //  *in71 = *on;
057000100929
057100100929           // -?x FedEx?
057200110517           //when  %parms() > *zero  and  %addr(£Mod) <> *null
057300110517           //                        and  £Mod = 'F';
057400110517           when  £Mod = 'F';
057500100929             *in72 = *on;
057600100929             w_Row = c_Row_B;
057700100929
057800100929           // -?x EuroExpress?
057900110517           ////when  %parms() > *zero  and  %addr(£Mod) <> *null
058000110517           ////                        and  £Mod = 'E';
058100110517           //when  £Mod = 'E';
058200100929           //  *in73 = *on;
058300100929
058400110516           // -?x BRT?
058500100929           //other;
058600100929           //  *in70 = *on;
058700100929
058800100929         endsl;
058900100929
059000100929       ENDSR;
059100100929
059200100929       //--------------------------------------------------------------
059300100929       //?Riempimento del box "Promemoria per l'autotrasportore"
059400100929       //--------------------------------------------------------------
059500100929       BEGSR  sr_LdV_Piena;
059600100929
059700100929         select;
059800100929
059900100929           when  £Dati = 'X';
060000100929             ds_01 = c_Char;
060100110517             //ds_02 = c_Char;
060200100929             ds_03 = c_Char;
060300100929             ds_04 = c_Char;
060400100929             ds_05 = c_Char;
060500110517             //ds_06 = c_Char;
060600100929             ds_07 = c_Char;
060700110517             //ds_08 = c_Char;
060800100929             ds_09 = c_Char;
060900100929             ds_10 = c_Char;
061000100929             ds_11 = c_Char;
061100100929             ds_12 = c_Char;
061200100929             ds_13 = c_Char;
061300100929             ds_14 = c_Char;
061400100929             ds_15 = c_Char;
061500100929             ds_16 = c_Char;
061600100929             ds_17 = c_Char;
061700100929             ds_18 = c_Char;
061800100929             ds_19 = c_Char;
061900100929             ds_20 = c_Char;
062000100929             ds_21 = c_Char;
062100100929             ds_22 = c_Char;
062200100929             ds_23 = c_Char;
062300100929             ds_24 = c_Char;
062400100929             ds_25 = c_Char;
062500110517             //ds_26 = c_Char;
062600100929             ds_27 = c_Char;
062700100929             ds_28 = c_Char;
062800100929             ds_29 = c_Char;
062900100929             ds_30 = c_Char;
063000110517             ds_31 = c_Char;
063100100929             ds_32 = c_Char;
063200100929             ds_33 = c_Char;
063300100929             ds_34 = c_Char;
063400110517             //ds_35 = c_Char;
063500110517             //ds_36 = c_Char;
063600110705             //ds_37 = c_Char;
063700100929             ds_Fed = c_Char;
063800100929
063900100929           when  £Dati = 'W';
064000100929             Pccm = '1234567';
064100100929             Pctf = '123';
064200100929             Psop = c_Char;
064300100929             Pddt = 'DDT sul collo';
064400100929             Prsm = c_Char;
064500100929             Pinm = c_Char;
064600100929             Pcam = c_Char;
064700100929             Plom = c_Char;
064800100929             Pprm = c_Char;
064900100929             Pnzm = c_Char;
065000100929             Pkmo = 'Mitt.Originale: ';
065100100929             Prmo = c_Char;
065200100929             Pccd = '7654321';
065300100929             Pcta = '987';
065400100929             Plna = '789';
065500100929             Prmn = c_Char;
065600100929             Prsd = c_Char;
065700100929             Prma = c_Char;
065800100929             Plnp = c_Char;
065900100929             Pnrs = c_Char;
066000100929             Pnsp = c_Char;
066100100929             Pdspa = c_Char;
066200100929             Prse = c_Char;
066300100929             Pind = c_Char;
066400100929             Pcad = c_Char;
066500100929             Plod = c_Char;
066600100929             Pprd = c_Char;
066700100929             Pnzd = c_Char;
066800100929             Ppkl = 'PACKING-LIST';
066900100929             Ptel = c_Char;
067000100929             Pnom = c_Char;
067100100929             If  *in20;
067200100929               Pkim = 'Imponibile';
067300100929               Pimp = c_Char;
067400100929               Pkie = 'I.v.a./Ese';
067500100929               Piva = c_Char;
067600100929               Pktf = 'Tot. Fatt.';
067700100929               Pift = c_Char;
067800100929               Pdiv = c_Char;
067900100929               Pkft = 'Fatt.';
068000100929               Pnft = '123456';
068100100929               Pkdf = ' del ';
068200110517               //Pdft = '12/34/5678';
068300100929               Pdft = %editc( %dec(Data_EUR) : 'Y' );
068400100929             EndIf;
068500100929             Pncla = c_Char;
068600100929             Ppkfa = c_Char;
068700100929             Pvlfa = c_Char;
068800100929             Pfvf = c_Char;
068900110517             //Pdso = c_Char;
069000100929             Pds1 = c_Char;
069100100929             Pds2 = c_Char;
069200100929             Pnp1 = c_Char;
069300100929             Pnp2 = c_Char;
069400100929             Pnp3 = c_Char;
069500100929             Pnp4 = c_Char;
069600100929             Pnp5 = c_Char;
069700100929             Pnp6 = c_Char;
069800100929             Ppad = 'PRIVATO    APPUNTAMENTO';
069900100929             If  *in20;
070000100929               Pcti = c_Char;
070100100929               Pdca = c_Char;
070200100929               Pcas = c_Char;
070300100929               Ptic = c_Char;
070400110511               //Pkic = 'INTESTARE A:';
070500110511               //Pica = c_Char;
070600100929             EndIf;
070700100929             Pnat = c_Char;
070800100929             Ptdr = c_Char;
070900100929             Pznc = '98';
071000100929             Pdis = 'D';
071100100929             Pass = 'A';
071200100929             Past = '*';
071300100929             Pbva = 'V';
071400110517             //Pdcr = '12/34/5678';
071500100929             Pdcr = %editc( %dec(Data_EUR) : 'Y' );
071600100929             Phcr = '12:34';
071700100929             Ppos = '123';
071800100929             Psrs = '12';
071900100929             Pgc1 = c_Char;
072000100929             Pgc2 = c_Char;
072100100929             If  *in20;
072200100929               Pdie = c_Char;
072300100929               Pies = c_Char;
072400100929               Pdi1 = c_Char;
072500100929               Pie1 = c_Char;
072600100929             EndIf;
072700100929             Pnsx = 'NON SEQUENZIALI';
072800100929             Pkrm = 'PER RICEVUTA DELLA MERCE E DEL DOCUMENTO DI +
072900100929                     TRASPORTO SENZA RISERVE';
073000100929             Pnux = 'NON UTILIZZARE PER LA CONSEGNA';
073100100929             Pcp1 = c_Char;
073200100929             Pcp2 = c_Char;
073300100929             Pkcn = c_Char;
073400110517             //Pkcd = '12/34/5678  12:34';
073500100929             Pkcd = %editc( %dec(Data_EUR) : 'Y' ) + '  '
073600100929                  + %char( %dec( %subdt( %time() : *h ) ) )
073700100929                  + ':'
073800100929                  + %char( %dec( %subdt( %time() : *mn ) ) );
073900100929
074000100929         endsl;
074100100929
074200100929       ENDSR;
074300100929
074400100929       //--------------------------------------------------------------
074500100929       //?Stampa di LdV per l'immagine del PDA                         ?
074600100929       //--------------------------------------------------------------
074700100929       BEGSR  sr_Prt_LdV;
074800100929
074900100929         write  PDA_LDV1;
075000100929         write  PDA_BOX1;
075100100929
075200100929         select;
075300100929
075400100929           // -?Nulla?
075500110517           //when  %parms() = *zero   or  %addr(£Mod) = *null;
075600110517           when  £Mod = *blank;
075700100929
075800100929           // -?Promemoria per l'autotrasportatore?
075900110517           //when  %parms() > *zero  and  %addr(£Mod) <> *null
076000110517           //                        and  £Mod = 'B';
076100110517           when  £Mod = 'B';
076200100929             write  PDA_SEP;
076300100929             write  PDA_AR53;
076400100929
076500100929           // -?DPD?
076600110517           //when  %parms() > *zero  and  %addr(£Mod) <> *null
076700110517           //                        and  £Mod = 'D';
076800110517           when  £Mod = 'D';
076900100929             write  PDA_DPD;
077000100929
077100100929           // -?FedEx?
077200110517           //when  %parms() > *zero  and  %addr(£Mod) <> *null
077300110517           //                        and  £Mod = 'F';
077400110517           when  £Mod = 'F';
077500100929             write  PDA_FED;
077600100929
077700100929         endsl;
077800100929
077900100929         // -?Dati di prova:?
078000110517         //IF  %parms() > 1   and   %addr(£Dati) <> *null
078100110517         //                   and   £Dati <> *blank;
078200110517         IF  £Dati <> *blank;
078300100929
078400100929           // -?Impostazione dati per immagine LdV?
078500100929           exsr  sr_Rec_LdV;
078600100929
078700110518           // -?STAMPA?
078800100929           For  wx = 1  To  w_Row;
078900110518             %occur($LdV_ds) = wx;
079000100929             §Row = 0,2 + (LdVPrg / 10);
079100100929             pTxt = LdVTxt + ' ' + %editc( LdvPrg : 'K' );
079200110517             if  LdVprg <> 30;
079300100929               write  PDA_ROW;
079400100929             else;
079500100929               write  PDA_R30;
079600100929             endif;
079700100929           EndFor;
079800100929
079900100929         ENDIF;
080000100929
080100110518         // -?Chiusura PrtF?
080200100929         close  FISP30IMP;
080300100929
080400100929       ENDSR;
080500100929
080600100929       //--------------------------------------------------------------
080700100929       //?Preparazione dati di output relativi alla Lettera di Vettura
080800100929       //--------------------------------------------------------------
080900100929       BEGSR  sr_Rec_LdV;
081000100929
081100110518         // -?Impostazione in output delle righe dell'area immagine LdV?
081200110518         FOR  wx = 1  TO  c_Row;
081300100929
081400110518           %occur($LdV_ds) = wx;
081500100929
081600110518           LdvPrg  = wx;
081700100929           select;
081800100929
081900100929             // -? 1ª riga?
082000100929             when  wx = 1;
082100100929               LdvTxt = ds_01;
082200100929
082300100929             // -? 2ª riga (vuota)?
082400100929             when  wx = 2;
082500110518               clear  LdvTxt;
082600100929
082700100929             // -? 3ª riga?
082800100929             when  wx = 3;
082900100929               LdvTxt = ds_03;
083000100929
083100100929             // -? 4ª riga?
083200100929             when  wx = 4;
083300100929               LdvTxt = ds_04;
083400100929
083500100929             // -? 5ª riga?
083600100929             when  wx = 5;
083700100929               LdvTxt = ds_05;
083800100929
083900100929             // -? 6ª riga (vuota)?
084000100929             when  wx = 6;
084100110518               clear  LdvTxt;
084200100929
084300100929             // -? 7ª riga?
084400100929             when  wx = 7;
084500100929               LdvTxt = ds_07;
084600100929
084700100929             // -? 8ª riga (vuota)?
084800100929             when  wx = 8;
084900110518               clear  LdvTxt;
085000100929
085100100929             // -? 9ª riga?
085200100929             when  wx = 9;
085300100929               LdvTxt = ds_09;
085400100929
085500100929             // -?10ª riga?
085600100929             when  wx = 10;
085700100929               LdvTxt = ds_10;
085800100929
085900100929             // -?11ª riga?
086000100929             when  wx = 11;
086100100929               LdvTxt = ds_11;
086200100929
086300100929             // -?12ª riga?
086400100929             when  wx = 12;
086500100929               LdvTxt = ds_12;
086600100929
086700100929             // -?13ª riga?
086800100929             when  wx = 13;
086900100929               LdvTxt = ds_13;
087000100929
087100100929             // -?14ª riga?
087200100929             when  wx = 14;
087300100929               LdvTxt = ds_14;
087400100929
087500100929             // -?15ª riga?
087600100929             when  wx = 15;
087700100929               LdvTxt = ds_15;
087800100929
087900100929             // -?16ª riga?
088000100929             when  wx = 16;
088100100929               LdvTxt = ds_16;
088200100929
088300100929             // -?17ª riga?
088400100929             when  wx = 17;
088500100929               LdvTxt = ds_17;
088600100929
088700100929             // -?18ª riga?
088800100929             when  wx = 18;
088900100929               LdvTxt = ds_18;
089000100929
089100100929             // -?19ª riga?
089200100929             when  wx = 19;
089300100929               LdvTxt = ds_19;
089400100929
089500100929             // -?20ª riga?
089600100929             when  wx = 20;
089700100929               LdvTxt = ds_20;
089800100929
089900100929             // -?21ª riga?
090000100929             when  wx = 21;
090100100929               LdvTxt  = ds_21;
090200100929
090300100929             // -?22ª riga?
090400100929             when  wx = 22;
090500100929               LdvTxt = ds_22;
090600100929
090700100929             // -?23ª riga?
090800100929             when  wx = 23;
090900100929               LdvTxt = ds_23;
091000100929
091100100929             // -?24ª riga?
091200100929             when  wx = 24;
091300100929               LdvTxt = ds_24;
091400100929
091500100929             // -?25ª riga?
091600100929             when  wx = 25;
091700100929               LdvTxt = ds_25;
091800100929
091900100929             // -?26ª riga (vuota)?
092000100929             when  wx = 26;
092100110518               clear  LdvTxt;
092200100929
092300100929             // -?27ª riga?
092400100929             when  wx = 27;
092500100929               LdvTxt = ds_27;
092600100929
092700100929             // -?28ª riga?
092800100929             when  wx = 28;
092900100929               LdvTxt = ds_28;
093000100929
093100100929             // -?29ª riga?
093200100929             when  wx = 29;
093300100929               LdvTxt = ds_29;
093400100929
093500100929             // -?30ª riga?
093600100929             when  wx = 30;
093700100929               LdvTxt = ds_30;
093800100929
093900100929             // -?31ª riga (vuota)?
094000100929             when  wx = 31;
094100110517               LdvTxt = ds_31;
094200100929
094300100929             // -?32ª riga?
094400100929             when  wx = 32;
094500100929               LdvTxt = ds_32;
094600100929
094700100929             // -?33ª riga?
094800100929             when  wx = 33;
094900100929               LdvTxt = ds_33;
095000100929
095100100929             // -?34ª riga?
095200100929             when  wx = 34;
095300100929               LdvTxt = ds_34;
095400100929
095500100929             // -?35ª riga (vuota)?
095600100929             when  wx = 35;
095700110518               clear  LdvTxt;
095800100929
095900100929             // -?36ª riga (vuota)?
096000100929             when  wx = 36;
096100110518               clear  LdvTxt;
096200100929
096300100929           endsl;
096400100929
096500100929         ENDFOR;
096600100929
096700100929
096800100929         select;
096900100929
097000100929           // -?Area box "Promemoria per l'autotrasportatore"?
097100110517           //when  %parms() > *zero  and  %addr(£Mod) <> *null
097200110517           //                        and  £Mod = 'B';
097300110517           when  £Mod = 'B';
097400110518             exsr  sr_LdVbase01;
097500100929
097600100929           // -?Area dati "DPD"?
097700110517           //when  %parms() > *zero  and  %addr(£Mod) <> *null
097800110517           //                        and  £Mod = 'D';
097900110517           when  £Mod = 'D';
098000110518             exsr  sr_LdVbaseDPD;
098100100929
098200100929           // -?Area dati "FedEx"?
098300110517           //when  %parms() > *zero  and  %addr(£Mod) <> *null
098400110517           //                        and  £Mod = 'F';
098500110517           when  £Mod = 'F';
098600110518             exsr  sr_LdVbaseFED;
098700100929
098800100929         endsl;
098900100929
099000100929         // -?Vuoti finali?
099100100929         For  wx = wx+1  To  w_Row;
099200110518           %occur($LdV_ds) = wx;
099300110518           LdvPrg = wx;
099400110518           clear  LdvTxt;
099500100929         EndFor;
099600100929
099700100929       ENDSR;
099800100929
099900100929       //--------------------------------------------------------------
100000100929       //?Riempimento del box "Promemoria per l'autotrasportore"
100100100929       //--------------------------------------------------------------
100200100929       BEGSR  sr_LdVbase01;
100300100929
100400100929         w_Row  = c_Row_B;
100500100929
100600100929         // -?Intestazione box "Promemoria per l'autotrasportatore"?
100700100929         // ·?37ª riga?
100800100929         //wx += 1;          (già incrementato dal ciclo "FOR" precedente)
100900110518         %occur($LdV_ds) = wx;
101000110518         LdvPrg  = wx;
101100100929         Ppma    = '                             '
101200100929                 + c_PMxA;
101300100929         LdvTxt  = ds_37;
101400100929         // ·?38ª riga?
101500100929         wx += 1;
101600110518         %occur($LdV_ds) = wx;
101700110518         LdvPrg  = wx;
101800100929         Ppma    = *all'=';
101900100929         LdvTxt  = ds_37;
102000100929
102100100929         // -?Impostazione segnalazione in output?
102200100929
102300100929         // -?39ª riga & successive?
102400110705         DoW  wx < c_Row_B;
102500110705
102600110705           wx += 1;
102700110705           %occur($LdV_ds) = wx;
102800110705           LdvPrg  = wx;
102900110705           clear  ds_37;
103000110705
103100110705           select;
103200110705             when  £Dati = 'X';
103300110705               Ppma = c_Char;
103400110705             when  £Dati = 'W'  and  wx <= 40;
103500110705               Ppma   = 'Consegnati colli';
103600110705               if  wx = 39;
103700110705                 Ppma = %trimr(Ppma)
103800110705                      + ' ....5';
103900110705               else;
104000110705                 Ppma = %trimr(Ppma)
104100110705                      + ' ....1';
104200110705               endif;
104300110705               Ppma   = %trimr(Ppma)
104400110705                      + ' in data '
104500110705                      + %editc( %dec(Data_EUR) : 'Y' );
104600110705           endsl;
104700110705           LdvTxt  = ds_37;
104800110705
104900110705         EndDo;
105000100929
105100100929       ENDSR;
105200100929
105300100929       //--------------------------------------------------------------
105400100929       //?Preparazione dati x spedizione "DPD"
105500100929       //--------------------------------------------------------------
105600100929       BEGSR  sr_LdVbaseDPD;
105700100929
105800100929         // -?Impostazione data?
105900110518         %occur($LdV_ds) = 19;
106000100929         %subst(LdVTxt : 94) = 'Date ' +
106100100929                               %editc( %dec(Data_EUR) : 'Y' );
106200100929
106300100929         // -?Impostazione numero colli?
106400110518         %occur($LdV_ds) = 20;
106500100929         %subst(LdVTxt : 94) = 'Parcel N.' + '....5';
106600100929
106700100929         // -?Impostazione numero parcel?
106800110518         %occur($LdV_ds) = 21;
106900100929         %subst(LdVTxt : 94) = '*...+....1...|';
107000100929
107100100929         // -?"Decodifica" del vecchio barcode?
107200110518         %occur($LdV_ds) = 22;
107300100929         %subst(LdVTxt : 94) = *blank;
107400100929         %subst(LdVTxt : 94) = '01 ' + 'ACBD'
107500100929                             + ' '   + '5678901234';
107600100929
107700100929         // -?Impostazione costanti  (Luciano ha detto di NON metterle)?
107800110518         //%occur($LdV_ds) = 23;
107900100929         //%subst(LdVTxt : 94) = *blank;
108000110518         //%occur($LdV_ds) = 24;
108100100929         //%subst(LdVTxt : 94) = 'Controllare la corrispon-';
108200110518         //%occur($LdV_ds) = 25;
108300100929         //%subst(LdVTxt : 94) = 'denza del destinatario con';
108400110518         //%occur($LdV_ds) = 26;
108500100929         //%subst(LdVTxt : 94) = 'l''indirizzo dell''etichetta';
108600110518         //%occur($LdV_ds) = 27;
108700100929         //%subst(LdVTxt : 94) = 'DPD.';
108800100929
108900100929       ENDSR;
109000100929
109100100929       //--------------------------------------------------------------
109200100929       //?Preparazione dati x spedizione "FedEx"
109300100929       //--------------------------------------------------------------
109400100929       BEGSR  sr_LdVbaseFED;
109500100929
109600100929         w_Row  = c_Row_B;
109700100929
109800100929         // -?Impostazione numeratore fino alla 39ª riga?
109900100929         For  wx = wx  To  (w_Row - c_Row_Fed);
110000110518           %occur($LdV_ds) = wx;
110100110518           LdvPrg  = wx;
110200100929         EndFor;
110300100929
110400100929         // -?40ª riga & successive?
110500100929         clear  xx;
110600100929         FOR  wx = (w_Row - c_Row_Fed + 1)  TO w_Row;
110700100929
110800110518           %occur($LdV_ds) = wx;
110900110518           LdvPrg  = wx;
111000100929
111100100929           // -?Caricamento AWB FedEx?
111200100929           xx += 1;
111300100929           Pfedkcn = 'EDPXX....1..';
111400100929           Pptn    = '210987654321';
111500100929           Pfedkcd = %editc( %dec(Data_EUR) : 'Y' ) + '  '
111600100929                   + %char( %dec( %subdt( %time() : *h ) ) )
111700100929                   + ':'
111800100929                   + %char( %dec( %subdt( %time() : *mn ) ) );
111900100929           Psts    = '..1';
112000100929           LdvTxt  = ds_Fed;
112100100929           leave;
112200100929
112300100929         ENDFOR;
112400100929
112500100929       ENDSR;
