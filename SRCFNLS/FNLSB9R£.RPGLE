000100090116       //--------------------------------------------------------------
000200090116       //?     STAMPA  DOCUMENTO  DI  RITIRO  SU  STAMPANTE  LASER     ?
000300090116       //--------------------------------------------------------------
000400090116       //
000500090116       //?NOTA BENE:?TUTTI i dati da stampare sono ricevuti in input.
000600090116       //            NON è richiesta alcuna decodifica.
000700090116       //
000800090116       //--------------------------------------------------------------
000900100414
001000100414       //--------------------------------------------------------------
001100100414       //?Parametri di compilazione (vedi *cmd UBCRTOBJ).              ?
001200100414       //--------------------------------------------------------------
001300100414
001400100414     /*PRM dbgview(*source)
001500100414     /*END
001600100414
001700100414       //--------------------------------------------------------------
001800100414       //?Specifiche di controllo.                                     ?
001900100414       //--------------------------------------------------------------
002000090116
002100090116     h decedit('0,') datedit(*dmy/) option(*nodebugio)
002200090116     h dftactgrp(*no) actgrp(*caller)
002300090116
002400090116       //--------------------------------------------------------------
002500090116       //?Dichiarazione file.                                          ?
002600090116       //--------------------------------------------------------------
002700100721
002800100721       // -?Tabelle?
002900100721     fTABEL00F  if   e           k disk
003000090116
003100100414       // -?Numeratori?
003200090116     fFLNUF01L  Uf   e           k disk
003300090116
003400100414       // -?Printer File?
003500100722     fFNLSB9P   o    e             printer usropn
003600090116     f                                     infds(InfPrtF)
003700090116
003800090116       //--------------------------------------------------------------
003900090116       //?Definizione costanti.                                        ?
004000090116       //--------------------------------------------------------------
004100090116
004200120329       // -?Intestazione BRT?
004300110503     d $BRT            c                   const('BRT S.p.A.')
004400090116     d $Intest1        c                   const('via E. Mattei, 42 - +
004500110503     d                                           40138 Bologna')
004600090116     d $Intest2        c                   const('C.F. - P.IVA e Reg. +
004700090116     d                                           Impr. BO 04507990150')
004800090116     d $Intest3        c                   const(' Capitale Sociale Eu+
004900090116     d                                           ro 24.000.000 int. ve+
005000090116     d                                           rs.')
005100090116     d $Intest4        c                   const('info on-line')
005200120329     d $Intest5        c                   const('www.brt.it')
005300090116
005400100414       // -?Costanti nel Documento di Trasporto?
005500090116     d c_Ref           c                   const('(REFERENTE)')
005600090116     d c_Tel           c                   const('(TELEFONO)')
005700090116     d c_Ind           c                   const('(INDIRIZZO)')
005800090116     d c_Cap           c                   const('(CAP)')
005900090116     d c_Loc           c                   const('(LOCALITÀ)')
006000090116     d c_Prv           c                   const('(PROV / NAZIONE)')
006100090116
006200100414       // -?Numero dei Box per Pagina "A4"?
006300090116     d $BxP            c                   const(4)
006400110503
006500110503       // -?DIMENSIONI & POSIZIONI:?
006600090116
006700100414       // -?Spessore delle linee in inches/pollici (*memo)?
006800090116     d c_LineNarrow    c                   const(0.008)
006900090116     d c_LineMedium    c                   const(0.017)
007000090116     d c_LineWide      c                   const(0.025)
007100090116
007200100414       // -?Posizioni "base" per:?
007300120329       //  ?- logo BRT in Pagina "A4"?
007400120329     d $PS0            c                   const(0.250)
007500090116
007600120329       //  ?- intestazione "BRT S.p.A."?
007700090116     d c_PosTBar       c                   const(0.370)
007800090116     d c_PosTBa1       c                   const(0.470)
007900090116     d c_PosTBa2       c                   const(0.570)
008000090116     d c_PosTBa3       c                   const(0.660)
008100090116     d c_PosTBa4       c                   const(0.770)
008200090116
008300100414       //  ?-?bordi orizzontali superiori box?
008400100414       //    ?(distanza dal lato superiore dell'A4)?
008500100414       //  ?N.B. - c_PosBoxA => posizione iniziale della linea?
008600100414       //                      ?orizzontale Superiore?
008700100414       //       ?- c_PosBoxB => posizione iniziale della linea?
008800100414       //                      ?orizzontale Inferiore?
008900090116     d c_PosBoxA       c                   const(0.200)
009000090116     d c_PosBoxB       c                   const(2.825)
009100090116
009200100414       //  ?- altre linee orizzontali?
009300090116     d c_PosLO1        c                   const(1.320)
009400090116     d c_PosLO2        c                   const(2.520)
009500090116     d c_PosLO3        c                   const(0.860)
009600090116     d c_PosLO4        c                   const(1.120)
009700090116     d c_PosLO5        c                   const(1.250)
009800090116     d c_PosLO6        c                   const(1.460)
009900090116     d c_PosLO7        c                   const(1.680)
010000090116     d c_PosLO8        c                   const(1.900)
010100090116     d c_PosLO9        c                   const(2.120)
010200090116     d c_PosLO0        c                   const(2.350)
010300100721
010400120329       // -?Dimensioni base per il simil-logo BRT?
010500120329      /copy gaitrasrc/srcTNSY,LogoBRT_R1
010600090116
010700090116       //--------------------------------------------------------------
010800090116       //?Definizione schiere.                                         ?
010900090116       //--------------------------------------------------------------
011000090116
011100100414       // -?Giorni della settimana e relativa decodifica?
011200090116     d $GgS            s              1    dim(9)  ctdata  perrcd(1)
011300090116     d $Gio            s              5    dim(9)  alt($GgS)
011400100722
011500100722       // -?Tipi servizio stampati?
011600100722     d $5Ec            s                   like(DB0tsp)   inz
011700100722     d                                     dim(2)
011800100722     d $5Ed            s                   like(§5Ed08)   inz
011900100722     d                                     dim(2)
012000090116
012100090116       //--------------------------------------------------------------
012200090116       //?Definizione aree dati.                                       ?
012300090116       //--------------------------------------------------------------
012400090116
012500100414       // -?Dati utente?
012600090116     d §AzUte        e ds                  extname(AZUTE00F)
012700090116     d                                     dtaara
012800090116     d §DatiUte      e ds                  extname(dDatiUte)
012900090116     d                                     dtaara
013000090116
013100090116       //--------------------------------------------------------------
013200090116       //?Definizione strutture dati.                                  ?
013300090116       //--------------------------------------------------------------
013400090116
013500100414       // -?Status?
013600090116     d Psds           sds
013700090116     d   SDSpgm          *proc
013800090116     d***SDSprm          *parms
013900090116     d   SDSusr              254    263
014000090116
014100100414       // -?InfDS?
014200090116     d InfPrtF         ds
014300090116     d   CurLine             367    368i 0
014400090116
014500100414       // -?Parametri ricevuti: dati Documento di Ritiro da stampare?
014600090116     d FNLSB5ds      e ds
014700100721
014800100721       // -?Tabella "5E" = Tipi Servizio?
014900100721     d ds5E          e ds                  inz
015000090116
015100090116       //--------------------------------------------------------------
015200090116       //?Definizione variabili globali.                               ?
015300090116       //--------------------------------------------------------------
015400090116
015500100414       // -?Indici di schiera?
015600110119     d xx              s              4  0 inz
015700090116     d yy              s              3  0 inz
015800090116
015900100414       // -?Variabili per il calcolo delle posizioni per box in A4?
016000090116     d wNumDoc         s              5  0 inz
016100090116     d wSeqDoc         s              1  0 inz
016200090116     d wPosIni         s              5  3 inz
016300090116
016400100414       // -?Campi di comodo?
016500090116     d wDateEUR        s               d   datfmt(*eur)   inz(*loval)
016600090116     d wDateDMY        s               d   datfmt(*dmy)   inz(*loval)
016700090116     d w0080           s              8  0 inz
016800090116
016900090116       //--------------------------------------------------------------
017000090116       //?Definizione procedure esterne.                               ?
017100090116       //--------------------------------------------------------------
017200090116
017300100414       // -?Reperimento dati utente?
017400110503     d TIBS34ds      e ds
017500100414      /copy gaitrasrc/srcProtoPR,TIBS34R
017600090116
017700100414       // -?Decodifica consegna richiesta?
017800110503     d KPJBA         e ds                  inz
017900110503     d FNLV80ds      e ds                  inz
018000110503     d  ILV80tla     e                     inz('L')
018100090116     d fnlv80r         pr                  extpgm('FNLV80R')
018200090116     d  kpjba                              likeds(KPJBA)
018300090116     d  fnlv80ds                           likeds(FNLV80ds)
018400090116
018500090116       //--------------------------------------------------------------
018600090116       //?Definizione key-list.                                        ?
018700090116       //--------------------------------------------------------------
018800090116
018900100721       // -?File TABEL00F?
019000100721     d k03tabel00    e ds                  extname(TABEL00F : *key)
019100100721     d                                     prefix(k_)   inz
019200090116
019300090116       //---------------------------------------------------------------
019400090116       //?Riepilogo indicatori.                                        ?
019500090116       //---------------------------------------------------------------
019600090116       //  41  - Posizionamento iniziale al 1° modulo in A4
019700090116       //  42  - Posizionamento iniziale al 2° modulo in A4
019800090116       //  43  - Posizionamento iniziale al 3° modulo in A4
019900090116       //  44  - Posizionamento iniziale al 4° modulo in A4
020000090116       //  51  - No dati del Mittente    : richiesto inserimento a mano
020100090116       //  52  - No dati del Destinatario: richiesto inserimento a mano
020200090116       //---------------------------------------------------------------
020300090116
020400110503       //--------------------------------------------------------------
020500110503       //?Specifiche di calcolo.                                       ?
020600110503       //--------------------------------------------------------------
020700090116
020800090116     c     *Entry        plist
020900090116     c                   parm                    FNLSB5ds
021000090116
021100090116      /free
021200090116
021300100414       // -?Operazioni iniziali?
021400100414       exsr  sr_RoutInz;
021500090116
021600100414       // -?Stampa Documento di Ritiro?
021700090116       if  DB0ris = 'M';
021800090116         exsr  sr_PrtDocRit;
021900090116       endif;
022000090116
022100100414       // -?Operazioni finali?
022200090116       exsr  sr_RoutEnd;
022300090116
022400090116       //--------------------------------------------------------------
022500090116       //?Subroutine iniziale.                                         ?
022600090116       //--------------------------------------------------------------
022700090116       BEGSR  *InzSR;
022800090116
022900100414         // -?Reperimento Dati del job (Utente/Operativi)?
023000090116         exsr sr_DatiJob;
023100090116
023200100414         // -?Composizione KPJBA (per FNL800R)?
023300090116         KNMUS  = SDSusr;
023400090119
023500100414         // -?Apertura del file di stampa?
023600100722         open  FNLSB9P;
023700100721
023800100722         // -?Decodifica tipi servizio?
023900100722         clear  $5Ec;
024000100722         clear  $5Ed;
024100100721         k_TBLkut = 1;
024200100721         k_TBLcod = '5E';
024300100722         // · ·?Decodifica tipo servizio "E" (default)?
024400100721         k_TBLkey = 'E';
024500100722         xx = 1;
024600100722         exsr  sr_Tab5E;
024700100722         // · ·?Decodifica tipo servizio "H" (default)?
024800100721         k_TBLkey = 'H';
024900100722         xx = 2;
025000100722         exsr  sr_Tab5E;
025100100722         // · ·?Decodifica tipo servizio ricevuto (se non "E" né "H")?
025200100722         if  DB0tsp <> *blank   and
025300100722             %lookup( DB0tsp : $5Ec ) = *zero;
025400100722           k_TBLkey = DB0tsp;
025500100722           xx = 2;
025600100722           exsr  sr_Tab5E;
025700100722         endif;
025800090116
025900090116       ENDSR;
026000090116
026100090116       //--------------------------------------------------------------
026200090116       //?Reperimento Dati del job (Utente/Operativi).                 ?
026300090116       //--------------------------------------------------------------
026400090116       BEGSR  sr_DatiJob;
026500090116
026600090116         in(E) §AzUte;
026700090116         if NOT %error;
026800090116           in(E) §DatiUte;
026900090116         endif;
027000090116         if %error or RSut = *blanks;
027100090116           clear TIBS34ds;
027200090116           tibs34r(tibs34ds);
027300090116           in §AzUte;
027400090116           in §DatiUte;
027500090116         endif;
027600090116
027700090116       ENDSR;
027800100722
027900100722       //--------------------------------------------------------------
028000100722       //?Reperimento Dati del job (Utente/Operativi).                 ?
028100100722       //--------------------------------------------------------------
028200100722       BEGSR  sr_Tab5E;
028300100722
028400100722         $5Ec(xx) = k_TBLkey;
028500100722
028600100722         chain  %kds( k03tabel00 )  TABEL;
028700100722
028800100722         if  %found(TABEL00F);
028900100722
029000100722           ds5E = TBLuni;
029100100722
029200100722           // -?Decodifica sempre il tipo servizio ricevuto?
029300100722           //  ?perchè tanto il controllo sulla "stampabilità"?
029400100722           //  ?l'ha già eseguito il chiamante (FNLS27R).?
029500100722           //if  §5Efub = 'S';
029600100722             $5Ed(xx) = §5Ed08;
029700100722           //endif;
029800100722
029900100722         endif;
030000100722
030100100722       ENDSR;
030200090116
030300090116       //--------------------------------------------------------------
030400090116       //?Operazioni iniziali.                                         ?
030500090116       //--------------------------------------------------------------
030600090116       BEGSR  sr_RoutInz;
030700090116
030800100414         // -?Verifica se richiesta la chiusura in "LR"?
030900100414         //?(Tipo lancio, DB0tla: "C" -           Chiude con LR?
031000100414         //                      ?"L" - Elabora e chiude con LR?
031100100414         //                      ?" " - Elabora e chiude in RETURN)?
031200090116         if  DB0tla <> *blank;
031300090116           *inLR = *on;
031400100414           // -?Verifica se richiesta solo la chiusura del pgm.?
031500090116           if  DB0tla = 'C';
031600090116             exsr  sr_RoutEnd;
031700090116           endif;
031800090116         endif;
031900090116
032000100414         // -?Verifica se richiesta la chiusura e la riapertura del?
032100100414         //  ?file di stampa?
032200090116         if  DB0fl3 = '1';
032300100722           close  FNLSB9P;
032400100722           open   FNLSB9P;
032500090116           clear  Page;
032600090116         endif;
032700090116
032800090116       ENDSR;
032900090116
033000090116       //--------------------------------------------------------------
033100090116       //?Stampa singolo Documento di Ritiro                           ?
033200090116       //--------------------------------------------------------------
033300090116       BEGSR  sr_PrtDocRit;
033400090116
033500100414         // -?Pulizia del PrtF?
033600090116         exsr  sr_ClrPrtF;
033700090116
033800100414         // -?Impostazione dei campi di stampa con quelli ricevuti?
033900090116         exsr  sr_PrepPrt;
034000090116
034100100414         // -?Stampa del Documento di Ritiro?
034200100414         //  ?(nel numero di copie richiesto)?
034300090116         For  xx = 1  To  DB0ctr;
034400090116
034500100414           // -?Deve stampare il?riferimento numerico aggiornato?
034600100414           //  ?di volta in volta su FLNUF.?
034700090116           chain  (*zero : 020 : DB0lnp : *zero)  FLNUF;
034800090116
034900090116           if  %found(FLNUF01L);
035000090116
035100100414             // -?Aggiornamento numeratore?
035200090116             NUFnum += 1;
035300100414             //____________
035400090116             Update  FLNUF;
035500090116             //¯¯¯¯¯¯¯¯¯¯¯¯
035600090116
035700100414             // -?Stampa documento di ritiro?
035800090116             exsr  sr_PosizBox;
035900090116             exsr  sr_Stampa;
036000090116
036100090116           endif;
036200090116
036300090116         EndFor;
036400090116
036500090116       ENDSR;
036600090116
036700090116       //--------------------------------------------------------------
036800090116       //?Pulizia dei campi di stampa                                  ?
036900090116       //--------------------------------------------------------------
037000090116       BEGSR  sr_ClrPrtF;
037100090116
037200100722         clear  LSB9Pos;
037300100722         //clear  LSB9Nul;            (no campi);
037400100722         clear  LSB9RowNr;
037500120329         clear  LSB9LogBrt;
037600100722         clear  LSB9Box;
037700100722         clear  LSB9ManD;
037800100722         clear  LSB9Txt;
037900090116
038000090116         *in = *off;
038100090116
038200090116       ENDSR;
038300090116
038400090116       //--------------------------------------------------------------
038500090116       //?Impostazione campi di stampa con quelli ricevuti nella DS    ?
038600090116       //--------------------------------------------------------------
038700090116       BEGSR  sr_PrepPRT;
038800090116
038900100414         // -?Numero documento?
039000100414         //  ?(NON ancora reperito né aggiornato)?
039100090119         //Pndc    = NUFnum;
039200090116
039300100414         // -?Data   documento?
039400090116         if  DB0aas <> *zero   and  DB0mgs <> *zero;
039500090116           w0080 = (DB0aas * 10000) + DB0mgs;
039600090116           wDateEUR = %date(w0080 : *iso);
039700090116           Pddc     = %dec( wDateEUR );
039800090116         endif;
039900090116
040000100414         // -?Mittente?
040100090116         if  DB0ccm <> *zero;
040200090116           Pmitt00 = %editc( DB0ccm : 'X' );
040300090116         endif;
040400090116         Pmitt02 = DB0rsm;
040500090116         Pmitt04 = DB0inm;
040600090116         Pmitt06 = %trimr( DB0cam )  +  ' ' +  %trimr( DB0lom );
040700090116         if  DB0nzm = *blank;
040800090116           %subst( Pmitt06 : 47 ) = DB0prm;
040900090116         else;
041000090116           %subst( Pmitt06 : 47 ) = DB0nzm;
041100090116         endif;
041200100721         // -?Mittente originale?
041300090116         if  DB0rmo <> *blank;
041400090116           Pmitt08 =  'Mitt.Orig.: '   +   DB0rmo;
041500090116         endif;
041600100721         // -?Richiesta immissione dati "a mano"?
041700090116         if  Pmitt00 = *blank   and   Pmitt01 = *blank   and
041800090116             Pmitt02 = *blank   and   Pmitt03 = *blank   and
041900090116             Pmitt04 = *blank   and   Pmitt05 = *blank   and
042000090119             Pmitt06 = *blank;
042100090116           Pmitt01 = *all'.';
042200090116           Pmitt02 = *all'.';
042300090116           Pmitt03 = *all'.';
042400090116           Pmitt04 = *all'.';
042500090116           Pmitt05 = *all'.';
042600090116           Pmitt06 = *all'.';
042700090116         endif;
042800090119         if  Pmitt07 = *blank   and   Pmitt08 = *blank;
042900090119           Pmitt07 = *all'.';
043000090119           Pmitt08 = *all'.';
043100090119           Kref    = c_Ref;
043200090119           Ktel    = c_Tel;
043300090119         endif;
043400090116
043500100414         // -?Destinatario?
043600090116         if  DB0ccd <> *zero;
043700090116           Pdest00 = %editc( DB0ccd : 'X' );
043800090116         endif;
043900090116         Pdest02 = DB0rsd;
044000090116         Pdest04 = DB0rd2;
044100090116         Pdest06 = DB0ind;
044200090116         Pdest08 = %trimr( DB0cad )  +  ' '  +  DB0lod;
044300090116         if  DB0nzd =  *blank;
044400090116           %subst( Pdest06 : 47 ) = DB0prd;
044500090116         else;
044600090116           %subst( Pdest06 : 47 ) = DB0nzd;
044700090116         endif;
044800100721         // -?Richiesta immissione dati "a mano"?
044900090116         if  Pdest00 = *blank   and   Pdest01 = *blank   and
045000090116             Pdest02 = *blank   and   Pdest03 = *blank   and
045100090116             Pdest04 = *blank   and   Pdest05 = *blank   and
045200090116             Pdest06 = *blank   and   Pdest07 = *blank   and
045300090116             Pdest08 = *blank   and   Pdest09 = *blank;
045400090116           Pdest01 = *all'.';
045500090116           Pdest02 = *all'.';
045600090116           Pdest03 = *all'.';
045700090116           Pdest04 = *all'.';
045800090116           Pdest05 = *all'.';
045900090116           Pdest06 = *all'.';
046000090116           Pdest07 = *all'.';
046100090116           Pdest08 = *all'.';
046200090116           Pdest09 = *all'.';
046300090119           Kref    = c_Ref;
046400090119           Ktel    = c_Tel;
046500090119           Kind    = c_Ind;
046600090119           Kcap    = c_Cap;
046700090119           Kloc    = c_Loc;
046800090119           Kprv    = c_Prv;
046900090116         endif;
047000090116
047100100414         // -?Resa: Franco / Assegnato?
047200090116         select;
047300100721           // -?(non indicato)?
047400090116           when  DB0cbo = *blank;
047500100721           // -?porto assegnato?
047600090116           when  %subst( DB0tb1 : 1 : 1 ) = 'A'  or  DB0tb2 <> *blank;
047700090116             Pxas  = 'X';
047800100721           // -?porto franco?
047900090116           other;
048000090116             Pxfr  = 'X';
048100090116         endsl;
048200090116
048300100722         // -?Tipo servizio (espresso/h10:30)?
048400100721         select;
048500100722           when  DB0fst = 'S'   and   DB0tsp = $5Ec(1);
048600100722             Pxs1  = 'X';
048700100722           when  DB0fst = 'S'   and   DB0tsp = $5Ec(2);
048800100722             Pxs2  = 'X';
048900100721         endsl;
049000090116
049100100414         // -?Fermo deposito?
049200090116         if  DB0ffd <> *blank;
049300090116           Pxfd  = 'X';
049400090116         endif;
049500090116
049600100414         // -?Consegna richiesta?
049700100721         // ·?IL / PRIMA DEL/LE / DOPO IL/LE?
049800090116         if  DB0dcr > *zero   or   Db0hcr > *zero;
049900090116           clear  FNLV80ds;
050000090116           ILV80aas = DB0aas;
050100090116           ILV80lnp = DB0lnp;
050200090116           ILV80nrs = DB0nrs;
050300090116           ILV80nsp = DB0nsp;
050400090116           ILV80mgs = DB0mgs;
050500090116           ILV80tc1 = DB0tc1;
050600090116           ILV80tc2 = DB0tc2;
050700090116           ILV80dcr = DB0dcr;
050800090116           ILV80hcr = DB0hcr;
050900090116           ILV80tcr = DB0tcr;
051000090116           fnlv80r( kpjba : FNLV80ds );
051100090116           Select;
051200100721             // ·?errore?
051300090116             When  OLV80err <> *blank;
051400100721             // ·?consegna tassativa (per ora solo ai supermercati)?
051500090116             When  OLV80tat =  'S';
051600090116             //Pcot = OLV80tcrd;
051700090116               Ptdr = 'IL';
051800100721             // ·?prima/dopo il/delle?
051900090116             Other;
052000090116               Ptdr = OLV80tcrd;
052100090116           Endsl;
052200090116         ENDIF;
052300100721         // ·?DATA consegna richiesta?
052400090116         if  DB0dcr <> *zero;
052500090116           wDateDMY = %date( DB0dcr : *iso );
052600090116           Pdcr     = %dec( wDateDMY );
052700090116         endif;
052800100721         // ·?ORA consegna richiesta?
052900090116         if  DB0hcr <> *zero;
053000090116           Phcr  = %editw( DB0hcr : '0 :  ' );
053100090116         endif;
053200090116
053300100414         // -?1° turno di chiusura?
053400090116         IF  DB0gc1 <> *blank;
053500090116           if  %subst( DB0gc1 : 1 : 1 ) = *blank;
053600100721             // ·?Nessun giorno specificato - Solo "M" o "P"?
053700090116             if  DB0gc1 = ' M';
053800090116               Pgc1 = %triml( $Gio(8) );
053900090116             else;
054000090116               Pgc1 = %triml( $Gio(9) );
054100090116             endif;
054200090116           else;
054300100721             // ·?Giorno specificato?
054400090116             yy = %lookup( %subst( DB0gc1 : 1 : 1 ) : $GgS );
054500090116             if  yy > *zero;
054600090116               Pgc1 = $Gio(yy);
054700090116             endif;
054800100721             // +?"M" o "P"?
054900090116             select;
055000090116               when  %subst( DB0gc1 : 2 : 1 ) = 'M';
055100090116                 Pgc1 = %trimr( Pgc1 )  +  %trim( $Gio(8) );
055200090116               when  %subst( DB0gc1 : 2 : 1 ) = 'P';
055300090116                 Pgc1 = %trimr( Pgc1 )  +  %trim( $Gio(9) );
055400090116             endsl;
055500090116           endif;
055600090116         ENDIF;
055700090116
055800100414         // -?2° turno di chiusura?
055900090116         IF  DB0gc2 <> *blank;
056000090116           if  %subst( DB0gc2 : 1 : 1 ) = *blank;
056100100721             // ·?Nessun giorno specificato - Solo "M" o "P"?
056200090116             if  DB0gc2 = ' M';
056300090116               Pgc2 = %triml( $Gio(8) );
056400090116             else;
056500090116               Pgc2 = %triml( $Gio(9) );
056600090116             endif;
056700090116           else;
056800100721             // ·?Giorno specificato?
056900090116             yy = %lookup( %subst( DB0gc2 : 1 : 1 ) : $GgS );
057000090116             if  yy > *zero;
057100090116               Pgc2 = $Gio(yy);
057200090116             endif;
057300100721             // +?"M" o "P"?
057400090116             select;
057500090116               when  %subst( DB0gc2 : 2 : 1 ) = 'M';
057600090116                 Pgc2 = %trimr( Pgc2 )  +  %trim( $Gio(8) );
057700090116               when  %subst( DB0gc2 : 2 : 1 ) = 'P';
057800090116                 Pgc2 = %trimr( Pgc2 )  +  %trim( $Gio(9) );
057900090116             endsl;
058000090116           endif;
058100090116         ENDIF;
058200090116
058300100414         // -?Valore da assicurare e relativa divisa?
058400090116         if  DB0ias <> *zero;
058500100722           Pdas  = DB0vas;
058600100722           Pias  = DB0ias;
058700090116         endif;
058800090116
058900100414         // -?Contrassegno e relativa divisa?
059000090116         if  DB0cas <> *zero;
059100100722           Pdca  = DB0vca;
059200100722           Pcas  = DB0cas;
059300090116         endif;
059400090116
059500100414         // -?Colli: numero, peso, volume, flag tipo-volume?
059600100722         Pncl  = DB0ncl;
059700100722         Ppkf  = DB0pkf;
059800100722         Pvlf  = DB0vlf;
059900090116
060000100414         // -?Natura merce?
060100100722         Pnat  = DB0nas;
060200090116
060300100414         // -?Note?
060400100722         Pnp1  = %trim(DB0nt1);
060500090116
060600090116
060700100414         // -Costanti in stampa?
060800100722         //Kacc  = c_Accettaz;
060900100722         //Kfir  = c_Firma;
061000100722         //Kdat  = c_Data;
061100100721
061200100722         kXS1  = $5Ed(1);     // - Espresso/Priority
061300100722         kXS2  = $5Ed(2);     // - h 10:30
061400090116
061500090116       ENDSR;
061600090116
061700090116       //--------------------------------------------------------------
061800090116       //?Posizionamento del box all'interno della pag. A4             ?
061900090116       //--------------------------------------------------------------
062000090116       BEGSR  sr_PosizBox;
062100090116
062200100414         // -?Calcolo la posizione x box all'interno della pagina "A4"?
062300090116         wNumDoc += 1;
062400090116         wSeqDoc = %rem( wNumDoc : $BxP );
062500090116
062600090116         if  wSeqDoc = *zero;
062700090116           wSeqDoc = 4;
062800090116         endif;
062900090116
063000100414         // -?Posizionamento nella pagina A4 in base al n° Doc. in?
063100100414         //  ?stampa?
063200090116         *in41 = (wSeqDoc = 1);
063300090116         *in42 = (wSeqDoc = 2);
063400090116         *in43 = (wSeqDoc = 3);
063500090116         *in44 = (wSeqDoc = 4);
063600100722         write  LSB9Pos;
063700090116
063800100414         // -?Imposto le posizioni nella pagina in corso di stampa?
063900090116         wPosIni = (CurLine / 9) - 0.120;
064000090116
064100120329         // -?Simil-Logo BRT?
064200100721         §PlblS  = wPosIni + $PS0;
064300120329         §PlblVS = 7.539;
064400120329         exsr  sr_SimilLogoBRT_;
064500090116
064600120329         // -?Impotazione dati BRT S.p.A.?
064700110503         TBAR  = $BRT;
064800090116         TBA1  = $Intest1;
064900090116         TBA2  = $Intest2;
065000090116         TBA3  = $Intest3;
065100090116         TBA4  = $Intest4;
065200090116         TBA5  = $Intest5;
065300090116         §PC0  = wPosIni + c_PosTBar;
065400090116         §PC1  = wPosIni + c_PosTBa1;
065500090116         §PC2  = wPosIni + c_PosTBa2;
065600090116         §PC3  = wPosIni + c_PosTBa3;
065700090116         §PC4  = wPosIni + c_PosTBa4;
065800090116
065900100414         // -?Box generale?
066000090116         §BoxA = wPosIni + c_PosBoxA;
066100090116         §BoxB = wPosIni + c_PosBoxB;
066200090116
066300100414         // -?Linee orizzontali?
066400090116         §LO1  = wPosIni + c_PosLO1;
066500090116         §LO2  = wPosIni + c_PosLO2;
066600090116         §LO3  = wPosIni + c_PosLO3;
066700090116         §LO4  = wPosIni + c_PosLO4;
066800090116         §LO5  = wPosIni + c_PosLO5;
066900090116         §LO6  = wPosIni + c_PosLO6;
067000090116         §LO7  = wPosIni + c_PosLO7;
067100090116         §LO8  = wPosIni + c_PosLO8;
067200090116         §LO9  = wPosIni + c_PosLO9;
067300090116         §LO0  = wPosIni + c_PosLO0;
067400090116         §LOA  = §BoxB - 0.080;
067500090116
067600100414         // -?Linee verticali?
067700090116         §LV1  = §BoxB - §BoxA;
067800090116         §LV2  = §LO7  - §LO3;
067900100721         §LV3  = §LO8  - §LO7;
068000100721         §LV4  = §LO9  - §LO8;
068100090116
068200100414         // -?Mini-Box interni?
068300090116         §Box1a = §LO4 + 0.020;
068400090116         §Box1b = §Box1a + 0.100;
068500100721         §Box2a = §LO8 + ( ( c_PosLo9 - c_PosLO8 - 0.100 ) / 2 );
068600100721         §Box2b = §Box2a + 0.100;
068700090116
068800100414         // -?Costanti nel box?
068900100721         §pkXFD = §Box2b - 0.015;
069000090116         §pkFIR = §LOA + 0.062;
069100090116
069200090116       ENDSR;
069300100721
069400110503       //--------------------------------------------------------------
069500120329       //?Posizionamento del simil-logo BRT (unico)                    ?
069600110503       //--------------------------------------------------------------
069700120329      /copy gaitrasrc/srcTNSY,LogoBRT_R2
069800090116
069900090116       //--------------------------------------------------------------
070000090116       //?Stampa (Documento di Ritiro)                               ?
070100090116       //--------------------------------------------------------------
070200090116       BEGSR  sr_Stampa;
070300090119
070400100414         // -?Numero documento?
070500100721         //  ?(unico campo NON ancora impostato)?
070600100721         Pndc  = NUFnum;
070700090116
070800100721         // -?Impostazione indicatori?
070900090116         *in51 = (Pmitt00 = *blank   and
071000090116                  Pmitt01 = *all'.'  and
071100090116                  Pmitt02 = *all'.'  and
071200090116                  Pmitt03 = *all'.'  and
071300090116                  Pmitt04 = *all'.'  and
071400090116                  Pmitt05 = *all'.'  and
071500090119                  Pmitt06 = *all'.');
071600090119         *in52 = (Pmitt07 = *all'.'  and
071700090119                  Pmitt08 = *all'.');
071800090119         *in53 = (Pdest00 = *blank   and
071900090116                  Pdest01 = *all'.'  and
072000090116                  Pdest02 = *all'.'  and
072100090116                  Pdest03 = *all'.'  and
072200090116                  Pdest04 = *all'.'  and
072300090116                  Pdest05 = *all'.'  and
072400090116                  Pdest06 = *all'.'  and
072500090119                  Pdest07 = *all'.'  and
072600090119                  Pdest08 = *all'.'  and
072700090119                  Pdest09 = *all'.');
072800090116
072900100721         // -?Stampa box?
073000100722         write  LSB9Box;
073100090116
073200120329         // -?Stampa logo BRT?
073300120329         write  LSB9LogBrt;
073400090119
073500100721         // -?Stampa indicazione dei dati da inserire a mano?
073600090119         if  *in53;
073700090119           §pkInd = ((Curline + 16) / 9) + 0.055;
073800090119           §pkLoc = ((Curline + 18) / 9) + 0.055;
073900090119           §pkRef = ((Curline + 21) / 9) + 0.055;
074000100722           write  LSB9Mand;
074100090119         endif;
074200090116
074300100721         // -?Stampa testo?
074400100722         write  LSB9Txt;
074500090116
074600090116       ENDSR;
074700090116
074800090116       //--------------------------------------------------------------
074900090116       //?Operazioni finali.                                         ?
075000090116       //--------------------------------------------------------------
075100090116       BEGSR  sr_RoutEnd;
075200090116
075300100721         // -?Chiusura file di stampa?
075400100414         //   (Tipo lancio, DB0tla: "C" -           Chiude con LR
075500100414         //                         "L" - Elabora e chiude con LR
075600100414         //                         " " - Elabora e chiude in RETURN)
075700090119         if  DB0tla <> *blank;
075800100722           close  FNLSB9P;
075900090119         endif;
076000090116
076100100721         // -?Chiusura pgm?
076200090116         return;
076300090116
076400090116       ENDSR;
076500090116
076600090116       //--------------------------------------------------------------
076700090116       //?Schiere a tempo di compilazione.                             ?
076800090116       //--------------------------------------------------------------
076900090116
077000100414** -?$GgS/$Gio?
0771000901161LUN
0772000901162MAR
0773000901163MER
0774000901164GIO
0775000901165VEN
0776000901166SAB
0777000901167DOM
077800090116M   -MATT.
077900090116P   -POM.
