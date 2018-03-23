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
002300180206     H DFTACTGRP(*NO) BNDDIR('UBRTVNETA') ACTGRP(*CALLER)
002400091223
002500091223       //--------------------------------------------------------------
002600121106       // Dichiarazione file.
002700091223       //--------------------------------------------------------------
002800100112
002900180206     FEDIVAB2L  UF   E           K DISK    usropn
003000180206     F                                     extfile(LibFileEDIVB)
003100091223
003200091223       //--------------------------------------------------------------
003300121106       // Definizione costanti.
003400091223       //--------------------------------------------------------------
003500100302
003600091223
003700091223       //--------------------------------------------------------------
003800121106       // Definizione schiere.
003900091223       //--------------------------------------------------------------
004000091223
004100091223
004200091223       //--------------------------------------------------------------
004300121106       // Definizione aree dati.
004400091223       //--------------------------------------------------------------
004500091223
004600091223
004700140724       //--------------------------------------------------------------
004800140724       // Definizione strutture dati.
004900140724       //--------------------------------------------------------------
005000140724
005100121106       // - Status
005200110516     d Psds           sds
005300110516     d   SDSpgm          *proc
005400121106     d   JobName             244    253                                         Job name
005500121106     d   JobUser             254    263                                         User name
005600121106     d   JobNumber           264    269s 0                                      Job number
005700160202
005800091223       //--------------------------------------------------------------
005900121106       // Definizione variabili.
006000091223       //--------------------------------------------------------------
006100091223
006200121106       // - Parametri ricevuti:
006300180202     d i_Opz           s              1a
006400160202       // tipo lancio: 'C'=LR   *blank=RT
006500180202     d i_TLa           s              1a
006600180202     d i_AAS           s              4s 0
006700180202     d i_LNP           s              3s 0
006800180202     d i_NRS           s              2s 0
006900180202     d i_NSP           s              7s 0
007000180202     d i_PKB           s              7s 1
007100180306     d i_NCL           s              5s 0
007200160202       // esito chiamata: 0=OK   1=ERR INPUT   2=KO
007300180202     d o_Esito         s              1s 0
007400121106
007500121106       // Campi di comodo
007600121106     d wDate           s              8  0 inz
007700180206
007800180206     D LibFileEDIVB    s             21A   inz
007900180206     D currSysNeta     s              8A   inz('*NULL')
008000091223
008100091223       //--------------------------------------------------------------
008200121106       // Definizione prototipi procedure.
008300091223       //--------------------------------------------------------------
008400180206     D/COPY GAITRASRC/SRCPROTOPR,UBRTVNETA
008500180206     D/COPY GAITRASRC/SRCPROTOPI,UBRTVNETA
008600130307
008700091223       //--------------------------------------------------------------
008800121106       // Definizione key-list.
008900091223       //--------------------------------------------------------------
009000091223
009100180202       // - File EDIVAB2L
009200180202     d k04edivab2    e ds                  extname(EDIVAB2L : *key)
009300140724     d                                     prefix(k_)   inz
009400091223
009500091223       //--------------------------------------------------------------
009600121107       // Definizione parametri procedura.
009700091223       //--------------------------------------------------------------
009800091223
009900091223     c     *Entry        plist
010000180202     c                   parm                    i_Opz
010100180202     c                   parm                    i_TLa
010200180202     c                   parm                    i_AAS
010300180202     c                   parm                    i_LNP
010400180202     c                   parm                    i_NRS
010500180202     c                   parm                    i_NSP
010600180202     c                   parm                    i_PKB
010700180306     c                   parm                    i_NCL
010800180202     c                   parm                    o_Esito
010900091223
011000130307      /free
011100091223
011200091223       //--------------------------------------------------------------
011300121106       // M A I N - L I N E
011400091223       //--------------------------------------------------------------
011500140806
011600130307       // Operazioni iniziali?
011700130307       exsr sr_RoutInz;
011800130307
011900140724       // attività richiesta dal chiamante se non chiamato per chiusura
012000180202       if i_TLa = *blank;
012100180202         // imposto come se non avessi trovato
012200180202         o_Esito = 2;
012300180202         K_VABAAS = i_AAS;
012400180202         K_VABLNP = i_LNP;
012500180202         K_VABNRS = i_NRS;
012600180202         K_VABNSP = i_NSP;
012700180202         chain %kds(K04EDIVAB2) EDIVAB00 ;
012800180202         If  %found;
012900180202           VABPKB = i_PKB;
013000180306           VABNCL = i_NCL;
013100180202           o_Esito = 0;
013200180202           update EDIVAB00;
013300140724         endif;
013400140724       endif;
013500091223
013600121106       // Operazioni finali?
013700091223       exsr sr_RoutEnd;
013800091223
013900091223       //--------------------------------------------------------------
014000121107       // Operazioni iniziali.
014100091223       //--------------------------------------------------------------
014200091223       BEGSR  sr_RoutInz;
014300121108
014400121108         // Reperimento data odierna (fmt aaaa/mm/gg)
014500121108         wDate = %dec( %date() );
014600180206
014700180206     C*
014800180206     C* Reperisco il sistema AS/400 corrente
014900180206     C                   callp     UBRTVNETA_Rtv(currSysNeta)
015000180206     C                   if        %subst(currSysNeta:1:6) = 'SETRAS'
015100180206     C                   eval      LibFileEDIVB = 'FILTRAGRU/EDIVAB2L'
015200180206     C                   else
015300180206     C                   eval      LibFileEDIVB = 'GAITRAGRPS/EDIVAB2L'
015400180206     C                   endif
015500180206         // apro il file la prima volta
015600180206         if not %open(EDIVAB2L);
015700180206           open EDIVAB2L;
015800180206         endif;
015900160202
016000160202         // Inizializzazione parametri di output
016100180202         clear o_Esito   ;
016200160202
016300160202         // Controllo i parametri di input
016400160202         exsr sr_CtrlPIn;
016500160202
016600160202       ENDSR;
016700160202
016800160202       //--------------------------------------------------------------
016900160202       // Controllo i parametri di input
017000160202       //--------------------------------------------------------------
017100160202       BEGSR  sr_CtrlPIn;
017200140724
017300160202         // L'opzione in questo caso non serve'
017400160202
017500160202         // Tipo lancio deve essere blank o 'C'
017600180202         if i_TLa <> *blank and i_TLa <>'C';
017700180202           // esco dando errore parametri input
017800180202           o_Esito = 1;
017900160202           exsr sr_RoutEnd;
018000140724         endif;
018100160202
018200160205         // Anno spedizione deve essere valorizzato (se non chiusura)
018300180202         if i_AAS <= 0 and i_TLa <>'C';
018400180202           // esco dando errore parametri input
018500180202           o_Esito = 1;
018600160202           exsr sr_RoutEnd;
018700160202         endif;
018800160202
018900160205         // Linea di partenza deve essere valorizzato (se non chiusura)
019000180202         if i_LNP <= 0 and i_TLa <>'C';
019100180202           // esco dando errore parametri input
019200180202           o_Esito = 1;
019300160202           exsr sr_RoutEnd;
019400160202         endif;
019500160202
019600160205         // Nr. Spedizione deve essere valorizzato (se non chiusura)
019700180202         if i_NSP <= 0 and i_TLa <>'C';
019800180202           // esco dando errore parametri input
019900180202           o_Esito = 1;
020000160202           exsr sr_RoutEnd;
020100160202         endif;
020200091223
020300091223       ENDSR;
020400121106
020500091223       //--------------------------------------------------------------
020600121107       // Operazioni finali.
020700091223       //--------------------------------------------------------------
020800091223       BEGSR  sr_RoutEnd;
020900140724
021000140724         // Chiusura pgm
021100180202         if i_TLa = 'C';
021200140724           *inlr = *on;
021300140724         else;
021400140724           *inrt = *on;
021500140724         endif;
021600140724         return;
021700091223
021800091223       ENDSR;
021900091223
022000091223      /end-free
