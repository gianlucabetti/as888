000100091228       //==============================================================
000200180202       // Pgm di aggiornamento peso bollettato su EDIVAB
000300091228       //==============================================================
000400091228
000500091228       //--------------------------------------------------------------
000600121106       // Parametri di compilazione (vedi *cmd UBCRTOBJ).              ?
000700091228       //--------------------------------------------------------------
000800091228
000900100325     /*PRM dbgview(*source)
001000091223     /*END
001100160202     **
001200160202     ** ISTRUZIONI PER LA COMPILAZIONE
001300160202     **
001400180202     ** 1. Creare il pgm (Opz 14 PDM)
001500160202     **
001600091228
001700091228       //--------------------------------------------------------------
001800121106       // Specifiche di controllo.
001900091228       //--------------------------------------------------------------
002000091223
002100091223     h decedit('0,') datedit(*dmy/) option(*nodebugio)
002200091223     h alwnull(*inputonly)
002300091223
002400091223       //--------------------------------------------------------------
002500121106       // Dichiarazione file.
002600091223       //--------------------------------------------------------------
002700100112
002800180202     FEDIVAB2L  UF   E           K DISK
002900091223
003000091223       //--------------------------------------------------------------
003100121106       // Definizione costanti.
003200091223       //--------------------------------------------------------------
003300100302
003400091223
003500091223       //--------------------------------------------------------------
003600121106       // Definizione schiere.
003700091223       //--------------------------------------------------------------
003800091223
003900091223
004000091223       //--------------------------------------------------------------
004100121106       // Definizione aree dati.
004200091223       //--------------------------------------------------------------
004300091223
004400091223
004500140724       //--------------------------------------------------------------
004600140724       // Definizione strutture dati.
004700140724       //--------------------------------------------------------------
004800140724
004900121106       // - Status
005000110516     d Psds           sds
005100110516     d   SDSpgm          *proc
005200121106     d   JobName             244    253                                         Job name
005300121106     d   JobUser             254    263                                         User name
005400121106     d   JobNumber           264    269s 0                                      Job number
005500160202
005600091223       //--------------------------------------------------------------
005700121106       // Definizione variabili.
005800091223       //--------------------------------------------------------------
005900091223
006000121106       // - Parametri ricevuti:
006100180202     d i_Opz           s              1a
006200160202       // tipo lancio: 'C'=LR   *blank=RT
006300180202     d i_TLa           s              1a
006400180202     d i_AAS           s              4s 0
006500180202     d i_LNP           s              3s 0
006600180202     d i_NRS           s              2s 0
006700180202     d i_NSP           s              7s 0
006800180202     d i_PKB           s              7s 1
006900160202       // esito chiamata: 0=OK   1=ERR INPUT   2=KO
007000180202     d o_Esito         s              1s 0
007100121106
007200121106       // Campi di comodo
007300121106     d wDate           s              8  0 inz
007400091223
007500091223       //--------------------------------------------------------------
007600121106       // Definizione prototipi procedure.
007700091223       //--------------------------------------------------------------
007800130307
007900091223       //--------------------------------------------------------------
008000121106       // Definizione key-list.
008100091223       //--------------------------------------------------------------
008200091223
008300180202       // - File EDIVAB2L
008400180202     d k04edivab2    e ds                  extname(EDIVAB2L : *key)
008500140724     d                                     prefix(k_)   inz
008600091223
008700091223       //--------------------------------------------------------------
008800121107       // Definizione parametri procedura.
008900091223       //--------------------------------------------------------------
009000091223
009100091223     c     *Entry        plist
009200180202     c                   parm                    i_Opz
009300180202     c                   parm                    i_TLa
009400180202     c                   parm                    i_AAS
009500180202     c                   parm                    i_LNP
009600180202     c                   parm                    i_NRS
009700180202     c                   parm                    i_NSP
009800180202     c                   parm                    i_PKB
009900180202     c                   parm                    o_Esito
010000091223
010100130307      /free
010200091223
010300091223       //--------------------------------------------------------------
010400121106       // M A I N - L I N E
010500091223       //--------------------------------------------------------------
010600140806
010700130307       // Operazioni iniziali?
010800130307       exsr sr_RoutInz;
010900130307
011000140724       // attivit� richiesta dal chiamante se non chiamato per chiusura
011100180202       if i_TLa = *blank;
011200180202         // imposto come se non avessi trovato
011300180202         o_Esito = 2;
011400180202         K_VABAAS = i_AAS;
011500180202         K_VABLNP = i_LNP;
011600180202         K_VABNRS = i_NRS;
011700180202         K_VABNSP = i_NSP;
011800180202         chain %kds(K04EDIVAB2) EDIVAB00 ;
011900180202         If  %found;
012000180202           VABPKB = i_PKB;
012100180202           o_Esito = 0;
012200180202           update EDIVAB00;
012300140724         endif;
012400140724       endif;
012500091223
012600121106       // Operazioni finali?
012700091223       exsr sr_RoutEnd;
012800091223
012900091223       //--------------------------------------------------------------
013000121107       // Operazioni iniziali.
013100091223       //--------------------------------------------------------------
013200091223       BEGSR  sr_RoutInz;
013300121108
013400121108         // Reperimento data odierna (fmt aaaa/mm/gg)
013500121108         wDate = %dec( %date() );
013600160202
013700160202         // Inizializzazione parametri di output
013800180202         clear o_Esito   ;
013900160202
014000160202         // Controllo i parametri di input
014100160202         exsr sr_CtrlPIn;
014200160202
014300160202       ENDSR;
014400160202
014500160202       //--------------------------------------------------------------
014600160202       // Controllo i parametri di input
014700160202       //--------------------------------------------------------------
014800160202       BEGSR  sr_CtrlPIn;
014900140724
015000160202         // L'opzione in questo caso non serve'
015100160202
015200160202         // Tipo lancio deve essere blank o 'C'
015300180202         if i_TLa <> *blank and i_TLa <>'C';
015400180202           // esco dando errore parametri input
015500180202           o_Esito = 1;
015600160202           exsr sr_RoutEnd;
015700140724         endif;
015800160202
015900160205         // Anno spedizione deve essere valorizzato (se non chiusura)
016000180202         if i_AAS <= 0 and i_TLa <>'C';
016100180202           // esco dando errore parametri input
016200180202           o_Esito = 1;
016300160202           exsr sr_RoutEnd;
016400160202         endif;
016500160202
016600160205         // Linea di partenza deve essere valorizzato (se non chiusura)
016700180202         if i_LNP <= 0 and i_TLa <>'C';
016800180202           // esco dando errore parametri input
016900180202           o_Esito = 1;
017000160202           exsr sr_RoutEnd;
017100160202         endif;
017200160202
017300160205         // Nr. Spedizione deve essere valorizzato (se non chiusura)
017400180202         if i_NSP <= 0 and i_TLa <>'C';
017500180202           // esco dando errore parametri input
017600180202           o_Esito = 1;
017700160202           exsr sr_RoutEnd;
017800160202         endif;
017900091223
018000091223       ENDSR;
018100121106
018200091223       //--------------------------------------------------------------
018300121107       // Operazioni finali.
018400091223       //--------------------------------------------------------------
018500091223       BEGSR  sr_RoutEnd;
018600140724
018700140724         // Chiusura pgm
018800180202         if i_TLa = 'C';
018900140724           *inlr = *on;
019000140724         else;
019100140724           *inrt = *on;
019200140724         endif;
019300140724         return;
019400091223
019500091223       ENDSR;
019600091223
019700091223      /end-free
