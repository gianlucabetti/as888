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
004400130214     d $Intest1        c                   const('Sede Operativa ed Am-
004500130214     d                                     ministrativa:')
004600130214     d $Intest2        c                   const('via E. Mattei, 42 -
004700130214     d                                     - 40138 Bologna')
004800130214     d $Intest3        c                   const('Sede Legale: Piazza -
004900130214     d                                     Diaz, 7 - 20123 Milano')
005000130214     d $Intest4        c                   const('C.F. e P.IVA 04507990150')
005100130214     d $Intest5        c                   const(' Capitale Sociale Eu-
005200130214     d                                     ro 24.000.000 int. vers.')
005300130214     d $Intest6        c                   const('info on-line')
005400130214     d $Intest7_brt    c                   const('www.brt.it')
005500090116
005600100414       // -?Costanti nel Documento di Trasporto?
005700090116     d c_Ref           c                   const('(REFERENTE)')
005800090116     d c_Tel           c                   const('(TELEFONO)')
005900090116     d c_Ind           c                   const('(INDIRIZZO)')
006000090116     d c_Cap           c                   const('(CAP)')
006100090116     d c_Loc           c                   const('(LOCALITÀ)')
006200090116     d c_Prv           c                   const('(PROV / NAZIONE)')
006300090116
006400100414       // -?Numero dei Box per Pagina "A4"?
006500090116     d $BxP            c                   const(4)
006600110503
006700110503       // -?DIMENSIONI & POSIZIONI:?
006800090116
006900100414       // -?Spessore delle linee in inches/pollici (*memo)?
007000090116     d c_LineNarrow    c                   const(0.008)
007100090116     d c_LineMedium    c                   const(0.017)
007200090116     d c_LineWide      c                   const(0.025)
007300090116
007400100414       // -?Posizioni "base" per:?
007500120329       //  ?- logo BRT in Pagina "A4"?
007600120329     d $PS0            c                   const(0.250)
007700090116
007800120329       //  ?- intestazione "BRT S.p.A."?
007900130214     d c_PosTBar       c                   const(0.320)
008000130214     d c_PosTBa1       c                   const(0.400)
008100130214     d c_PosTBa2       c                   const(0.480)
008200130214     d c_PosTBa3       c                   const(0.550)
008300130214     d c_PosTBa4       c                   const(0.640)
008400130214     d c_PosTBa5       c                   const(0.710)
008500130214     d c_PosTBa6       c                   const(0.800)
008600090116
008700100414       //  ?-?bordi orizzontali superiori box?
008800100414       //    ?(distanza dal lato superiore dell'A4)?
008900100414       //  ?N.B. - c_PosBoxA => posizione iniziale della linea?
009000100414       //                      ?orizzontale Superiore?
009100100414       //       ?- c_PosBoxB => posizione iniziale della linea?
009200100414       //                      ?orizzontale Inferiore?
009300090116     d c_PosBoxA       c                   const(0.200)
009400090116     d c_PosBoxB       c                   const(2.825)
009500090116
009600100414       //  ?- altre linee orizzontali?
009700090116     d c_PosLO1        c                   const(1.320)
009800090116     d c_PosLO2        c                   const(2.520)
009900090116     d c_PosLO3        c                   const(0.860)
010000090116     d c_PosLO4        c                   const(1.120)
010100090116     d c_PosLO5        c                   const(1.250)
010200090116     d c_PosLO6        c                   const(1.460)
010300090116     d c_PosLO7        c                   const(1.680)
010400090116     d c_PosLO8        c                   const(1.900)
010500090116     d c_PosLO9        c                   const(2.120)
010600090116     d c_PosLO0        c                   const(2.350)
010700100721
010800120329       // -?Dimensioni base per il simil-logo BRT?
010900120329      /copy gaitrasrc/srcTNSY,LogoBRT_R1
011000090116
011100090116       //--------------------------------------------------------------
011200090116       //?Definizione schiere.                                         ?
011300090116       //--------------------------------------------------------------
011400090116
011500100414       // -?Giorni della settimana e relativa decodifica?
011600090116     d $GgS            s              1    dim(9)  ctdata  perrcd(1)
011700090116     d $Gio            s              5    dim(9)  alt($GgS)
011800100722
011900100722       // -?Tipi servizio stampati?
012000100722     d $5Ec            s                   like(DB0tsp)   inz
012100100722     d                                     dim(2)
012200100722     d $5Ed            s                   like(§5Ed08)   inz
012300100722     d                                     dim(2)
012400090116
012500090116       //--------------------------------------------------------------
012600090116       //?Definizione aree dati.                                       ?
012700090116       //--------------------------------------------------------------
012800090116
012900100414       // -?Dati utente?
013000090116     d §AzUte        e ds                  extname(AZUTE00F)
013100090116     d                                     dtaara
013200090116     d §DatiUte      e ds                  extname(dDatiUte)
013300090116     d                                     dtaara
013400090116
013500090116       //--------------------------------------------------------------
013600090116       //?Definizione strutture dati.                                  ?
013700090116       //--------------------------------------------------------------
013800090116
013900100414       // -?Status?
014000090116     d Psds           sds
014100090116     d   SDSpgm          *proc
014200090116     d***SDSprm          *parms
014300090116     d   SDSusr              254    263
014400090116
014500100414       // -?InfDS?
014600090116     d InfPrtF         ds
014700090116     d   CurLine             367    368i 0
014800090116
014900100414       // -?Parametri ricevuti: dati Documento di Ritiro da stampare?
015000090116     d FNLSB5ds      e ds
015100100721
015200100721       // -?Tabella "5E" = Tipi Servizio?
015300100721     d ds5E          e ds                  inz
015400090116
015500090116       //--------------------------------------------------------------
015600090116       //?Definizione variabili globali.                               ?
015700090116       //--------------------------------------------------------------
015800090116
015900100414       // -?Indici di schiera?
016000110119     d xx              s              4  0 inz
016100090116     d yy              s              3  0 inz
016200090116
016300100414       // -?Variabili per il calcolo delle posizioni per box in A4?
016400090116     d wNumDoc         s              5  0 inz
016500090116     d wSeqDoc         s              1  0 inz
016600090116     d wPosIni         s              5  3 inz
016700090116
016800100414       // -?Campi di comodo?
016900090116     d wDateEUR        s               d   datfmt(*eur)   inz(*loval)
017000090116     d wDateDMY        s               d   datfmt(*dmy)   inz(*loval)
017100090116     d w0080           s              8  0 inz
017200090116
017300090116       //--------------------------------------------------------------
017400090116       //?Definizione procedure esterne.                               ?
017500090116       //--------------------------------------------------------------
017600090116
017700100414       // -?Reperimento dati utente?
017800110503     d TIBS34ds      e ds
017900100414      /copy gaitrasrc/srcProtoPR,TIBS34R
018000090116
018100100414       // -?Decodifica consegna richiesta?
018200110503     d KPJBA         e ds                  inz
018300110503     d FNLV80ds      e ds                  inz
018400110503     d  ILV80tla     e                     inz('L')
018500090116     d fnlv80r         pr                  extpgm('FNLV80R')
018600090116     d  kpjba                              likeds(KPJBA)
018700090116     d  fnlv80ds                           likeds(FNLV80ds)
018800090116
018900090116       //--------------------------------------------------------------
019000090116       //?Definizione key-list.                                        ?
019100090116       //--------------------------------------------------------------
019200090116
019300100721       // -?File TABEL00F?
019400100721     d k03tabel00    e ds                  extname(TABEL00F : *key)
019500100721     d                                     prefix(k_)   inz
019600090116
019700090116       //---------------------------------------------------------------
019800090116       //?Riepilogo indicatori.                                        ?
019900090116       //---------------------------------------------------------------
020000090116       //  41  - Posizionamento iniziale al 1° modulo in A4
020100090116       //  42  - Posizionamento iniziale al 2° modulo in A4
020200090116       //  43  - Posizionamento iniziale al 3° modulo in A4
020300090116       //  44  - Posizionamento iniziale al 4° modulo in A4
020400090116       //  51  - No dati del Mittente    : richiesto inserimento a mano
020500090116       //  52  - No dati del Destinatario: richiesto inserimento a mano
020600090116       //---------------------------------------------------------------
020700090116
020800110503       //--------------------------------------------------------------
020900110503       //?Specifiche di calcolo.                                       ?
021000110503       //--------------------------------------------------------------
021100090116
021200090116     c     *Entry        plist
021300090116     c                   parm                    FNLSB5ds
021400090116
021500090116      /free
021600090116
021700100414       // -?Operazioni iniziali?
021800100414       exsr  sr_RoutInz;
021900090116
022000100414       // -?Stampa Documento di Ritiro?
022100090116       if  DB0ris = 'M';
022200090116         exsr  sr_PrtDocRit;
022300090116       endif;
022400090116
022500100414       // -?Operazioni finali?
022600090116       exsr  sr_RoutEnd;
022700090116
022800090116       //--------------------------------------------------------------
022900090116       //?Subroutine iniziale.                                         ?
023000090116       //--------------------------------------------------------------
023100090116       BEGSR  *InzSR;
023200090116
023300100414         // -?Reperimento Dati del job (Utente/Operativi)?
023400090116         exsr sr_DatiJob;
023500090116
023600100414         // -?Composizione KPJBA (per FNL800R)?
023700090116         KNMUS  = SDSusr;
023800090119
023900100414         // -?Apertura del file di stampa?
024000100722         open  FNLSB9P;
024100100721
024200100722         // -?Decodifica tipi servizio?
024300100722         clear  $5Ec;
024400100722         clear  $5Ed;
024500100721         k_TBLkut = 1;
024600100721         k_TBLcod = '5E';
024700100722         // · ·?Decodifica tipo servizio "E" (default)?
024800100721         k_TBLkey = 'E';
024900100722         xx = 1;
025000100722         exsr  sr_Tab5E;
025100100722         // · ·?Decodifica tipo servizio "H" (default)?
025200100721         k_TBLkey = 'H';
025300100722         xx = 2;
025400100722         exsr  sr_Tab5E;
025500100722         // · ·?Decodifica tipo servizio ricevuto (se non "E" né "H")?
025600100722         if  DB0tsp <> *blank   and
025700100722             %lookup( DB0tsp : $5Ec ) = *zero;
025800100722           k_TBLkey = DB0tsp;
025900100722           xx = 2;
026000100722           exsr  sr_Tab5E;
026100100722         endif;
026200090116
026300090116       ENDSR;
026400090116
026500090116       //--------------------------------------------------------------
026600090116       //?Reperimento Dati del job (Utente/Operativi).                 ?
026700090116       //--------------------------------------------------------------
026800090116       BEGSR  sr_DatiJob;
026900090116
027000090116         in(E) §AzUte;
027100090116         if NOT %error;
027200090116           in(E) §DatiUte;
027300090116         endif;
027400090116         if %error or RSut = *blanks;
027500090116           clear TIBS34ds;
027600090116           tibs34r(tibs34ds);
027700090116           in §AzUte;
027800090116           in §DatiUte;
027900090116         endif;
028000090116
028100090116       ENDSR;
028200100722
028300100722       //--------------------------------------------------------------
028400100722       //?Reperimento Dati del job (Utente/Operativi).                 ?
028500100722       //--------------------------------------------------------------
028600100722       BEGSR  sr_Tab5E;
028700100722
028800100722         $5Ec(xx) = k_TBLkey;
028900100722
029000100722         chain  %kds( k03tabel00 )  TABEL;
029100100722
029200100722         if  %found(TABEL00F);
029300100722
029400100722           ds5E = TBLuni;
029500100722
029600100722           // -?Decodifica sempre il tipo servizio ricevuto?
029700100722           //  ?perchè tanto il controllo sulla "stampabilità"?
029800100722           //  ?l'ha già eseguito il chiamante (FNLS27R).?
029900100722           //if  §5Efub = 'S';
030000100722             $5Ed(xx) = §5Ed08;
030100100722           //endif;
030200100722
030300100722         endif;
030400100722
030500100722       ENDSR;
030600090116
030700090116       //--------------------------------------------------------------
030800090116       //?Operazioni iniziali.                                         ?
030900090116       //--------------------------------------------------------------
031000090116       BEGSR  sr_RoutInz;
031100090116
031200100414         // -?Verifica se richiesta la chiusura in "LR"?
031300100414         //?(Tipo lancio, DB0tla: "C" -           Chiude con LR?
031400100414         //                      ?"L" - Elabora e chiude con LR?
031500100414         //                      ?" " - Elabora e chiude in RETURN)?
031600090116         if  DB0tla <> *blank;
031700090116           *inLR = *on;
031800100414           // -?Verifica se richiesta solo la chiusura del pgm.?
031900090116           if  DB0tla = 'C';
032000090116             exsr  sr_RoutEnd;
032100090116           endif;
032200090116         endif;
032300090116
032400100414         // -?Verifica se richiesta la chiusura e la riapertura del?
032500100414         //  ?file di stampa?
032600090116         if  DB0fl3 = '1';
032700100722           close  FNLSB9P;
032800100722           open   FNLSB9P;
032900090116           clear  Page;
033000090116         endif;
033100090116
033200090116       ENDSR;
033300090116
033400090116       //--------------------------------------------------------------
033500090116       //?Stampa singolo Documento di Ritiro                           ?
033600090116       //--------------------------------------------------------------
033700090116       BEGSR  sr_PrtDocRit;
033800090116
033900100414         // -?Pulizia del PrtF?
034000090116         exsr  sr_ClrPrtF;
034100090116
034200100414         // -?Impostazione dei campi di stampa con quelli ricevuti?
034300090116         exsr  sr_PrepPrt;
034400090116
034500100414         // -?Stampa del Documento di Ritiro?
034600100414         //  ?(nel numero di copie richiesto)?
034700090116         For  xx = 1  To  DB0ctr;
034800090116
034900100414           // -?Deve stampare il?riferimento numerico aggiornato?
035000100414           //  ?di volta in volta su FLNUF.?
035100090116           chain  (*zero : 020 : DB0lnp : *zero)  FLNUF;
035200090116
035300090116           if  %found(FLNUF01L);
035400090116
035500100414             // -?Aggiornamento numeratore?
035600090116             NUFnum += 1;
035700100414             //____________
035800090116             Update  FLNUF;
035900090116             //¯¯¯¯¯¯¯¯¯¯¯¯
036000090116
036100100414             // -?Stampa documento di ritiro?
036200090116             exsr  sr_PosizBox;
036300090116             exsr  sr_Stampa;
036400090116
036500090116           endif;
036600090116
036700090116         EndFor;
036800090116
036900090116       ENDSR;
037000090116
037100090116       //--------------------------------------------------------------
037200090116       //?Pulizia dei campi di stampa                                  ?
037300090116       //--------------------------------------------------------------
037400090116       BEGSR  sr_ClrPrtF;
037500090116
037600100722         clear  LSB9Pos;
037700100722         //clear  LSB9Nul;            (no campi);
037800100722         clear  LSB9RowNr;
037900120329         clear  LSB9LogBrt;
038000100722         clear  LSB9Box;
038100100722         clear  LSB9ManD;
038200100722         clear  LSB9Txt;
038300090116
038400090116         *in = *off;
038500090116
038600090116       ENDSR;
038700090116
038800090116       //--------------------------------------------------------------
038900090116       //?Impostazione campi di stampa con quelli ricevuti nella DS    ?
039000090116       //--------------------------------------------------------------
039100090116       BEGSR  sr_PrepPRT;
039200090116
039300100414         // -?Numero documento?
039400100414         //  ?(NON ancora reperito né aggiornato)?
039500090119         //Pndc    = NUFnum;
039600090116
039700100414         // -?Data   documento?
039800090116         if  DB0aas <> *zero   and  DB0mgs <> *zero;
039900090116           w0080 = (DB0aas * 10000) + DB0mgs;
040000090116           wDateEUR = %date(w0080 : *iso);
040100090116           Pddc     = %dec( wDateEUR );
040200090116         endif;
040300090116
040400100414         // -?Mittente?
040500090116         if  DB0ccm <> *zero;
040600090116           Pmitt00 = %editc( DB0ccm : 'X' );
040700090116         endif;
040800090116         Pmitt02 = DB0rsm;
040900090116         Pmitt04 = DB0inm;
041000090116         Pmitt06 = %trimr( DB0cam )  +  ' ' +  %trimr( DB0lom );
041100090116         if  DB0nzm = *blank;
041200090116           %subst( Pmitt06 : 47 ) = DB0prm;
041300090116         else;
041400090116           %subst( Pmitt06 : 47 ) = DB0nzm;
041500090116         endif;
041600100721         // -?Mittente originale?
041700090116         if  DB0rmo <> *blank;
041800090116           Pmitt08 =  'Mitt.Orig.: '   +   DB0rmo;
041900090116         endif;
042000100721         // -?Richiesta immissione dati "a mano"?
042100090116         if  Pmitt00 = *blank   and   Pmitt01 = *blank   and
042200090116             Pmitt02 = *blank   and   Pmitt03 = *blank   and
042300090116             Pmitt04 = *blank   and   Pmitt05 = *blank   and
042400090119             Pmitt06 = *blank;
042500090116           Pmitt01 = *all'.';
042600090116           Pmitt02 = *all'.';
042700090116           Pmitt03 = *all'.';
042800090116           Pmitt04 = *all'.';
042900090116           Pmitt05 = *all'.';
043000090116           Pmitt06 = *all'.';
043100090116         endif;
043200090119         if  Pmitt07 = *blank   and   Pmitt08 = *blank;
043300090119           Pmitt07 = *all'.';
043400090119           Pmitt08 = *all'.';
043500090119           Kref    = c_Ref;
043600090119           Ktel    = c_Tel;
043700090119         endif;
043800090116
043900100414         // -?Destinatario?
044000090116         if  DB0ccd <> *zero;
044100090116           Pdest00 = %editc( DB0ccd : 'X' );
044200090116         endif;
044300090116         Pdest02 = DB0rsd;
044400090116         Pdest04 = DB0rd2;
044500090116         Pdest06 = DB0ind;
044600090116         Pdest08 = %trimr( DB0cad )  +  ' '  +  DB0lod;
044700090116         if  DB0nzd =  *blank;
044800090116           %subst( Pdest06 : 47 ) = DB0prd;
044900090116         else;
045000090116           %subst( Pdest06 : 47 ) = DB0nzd;
045100090116         endif;
045200100721         // -?Richiesta immissione dati "a mano"?
045300090116         if  Pdest00 = *blank   and   Pdest01 = *blank   and
045400090116             Pdest02 = *blank   and   Pdest03 = *blank   and
045500090116             Pdest04 = *blank   and   Pdest05 = *blank   and
045600090116             Pdest06 = *blank   and   Pdest07 = *blank   and
045700090116             Pdest08 = *blank   and   Pdest09 = *blank;
045800090116           Pdest01 = *all'.';
045900090116           Pdest02 = *all'.';
046000090116           Pdest03 = *all'.';
046100090116           Pdest04 = *all'.';
046200090116           Pdest05 = *all'.';
046300090116           Pdest06 = *all'.';
046400090116           Pdest07 = *all'.';
046500090116           Pdest08 = *all'.';
046600090116           Pdest09 = *all'.';
046700090119           Kref    = c_Ref;
046800090119           Ktel    = c_Tel;
046900090119           Kind    = c_Ind;
047000090119           Kcap    = c_Cap;
047100090119           Kloc    = c_Loc;
047200090119           Kprv    = c_Prv;
047300090116         endif;
047400090116
047500100414         // -?Resa: Franco / Assegnato?
047600090116         select;
047700100721           // -?(non indicato)?
047800090116           when  DB0cbo = *blank;
047900100721           // -?porto assegnato?
048000090116           when  %subst( DB0tb1 : 1 : 1 ) = 'A'  or  DB0tb2 <> *blank;
048100090116             Pxas  = 'X';
048200100721           // -?porto franco?
048300090116           other;
048400090116             Pxfr  = 'X';
048500090116         endsl;
048600090116
048700100722         // -?Tipo servizio (espresso/h10:30)?
048800100721         select;
048900100722           when  DB0fst = 'S'   and   DB0tsp = $5Ec(1);
049000100722             Pxs1  = 'X';
049100100722           when  DB0fst = 'S'   and   DB0tsp = $5Ec(2);
049200100722             Pxs2  = 'X';
049300100721         endsl;
049400090116
049500100414         // -?Fermo deposito?
049600090116         if  DB0ffd <> *blank;
049700090116           Pxfd  = 'X';
049800090116         endif;
049900090116
050000100414         // -?Consegna richiesta?
050100100721         // ·?IL / PRIMA DEL/LE / DOPO IL/LE?
050200090116         if  DB0dcr > *zero   or   Db0hcr > *zero;
050300090116           clear  FNLV80ds;
050400090116           ILV80aas = DB0aas;
050500090116           ILV80lnp = DB0lnp;
050600090116           ILV80nrs = DB0nrs;
050700090116           ILV80nsp = DB0nsp;
050800090116           ILV80mgs = DB0mgs;
050900090116           ILV80tc1 = DB0tc1;
051000090116           ILV80tc2 = DB0tc2;
051100090116           ILV80dcr = DB0dcr;
051200090116           ILV80hcr = DB0hcr;
051300090116           ILV80tcr = DB0tcr;
051400090116           fnlv80r( kpjba : FNLV80ds );
051500090116           Select;
051600100721             // ·?errore?
051700090116             When  OLV80err <> *blank;
051800100721             // ·?consegna tassativa (per ora solo ai supermercati)?
051900090116             When  OLV80tat =  'S';
052000090116             //Pcot = OLV80tcrd;
052100090116               Ptdr = 'IL';
052200100721             // ·?prima/dopo il/delle?
052300090116             Other;
052400090116               Ptdr = OLV80tcrd;
052500090116           Endsl;
052600090116         ENDIF;
052700100721         // ·?DATA consegna richiesta?
052800090116         if  DB0dcr <> *zero;
052900090116           wDateDMY = %date( DB0dcr : *iso );
053000090116           Pdcr     = %dec( wDateDMY );
053100090116         endif;
053200100721         // ·?ORA consegna richiesta?
053300090116         if  DB0hcr <> *zero;
053400090116           Phcr  = %editw( DB0hcr : '0 :  ' );
053500090116         endif;
053600090116
053700100414         // -?1° turno di chiusura?
053800090116         IF  DB0gc1 <> *blank;
053900090116           if  %subst( DB0gc1 : 1 : 1 ) = *blank;
054000100721             // ·?Nessun giorno specificato - Solo "M" o "P"?
054100090116             if  DB0gc1 = ' M';
054200090116               Pgc1 = %triml( $Gio(8) );
054300090116             else;
054400090116               Pgc1 = %triml( $Gio(9) );
054500090116             endif;
054600090116           else;
054700100721             // ·?Giorno specificato?
054800090116             yy = %lookup( %subst( DB0gc1 : 1 : 1 ) : $GgS );
054900090116             if  yy > *zero;
055000090116               Pgc1 = $Gio(yy);
055100090116             endif;
055200100721             // +?"M" o "P"?
055300090116             select;
055400090116               when  %subst( DB0gc1 : 2 : 1 ) = 'M';
055500090116                 Pgc1 = %trimr( Pgc1 )  +  %trim( $Gio(8) );
055600090116               when  %subst( DB0gc1 : 2 : 1 ) = 'P';
055700090116                 Pgc1 = %trimr( Pgc1 )  +  %trim( $Gio(9) );
055800090116             endsl;
055900090116           endif;
056000090116         ENDIF;
056100090116
056200100414         // -?2° turno di chiusura?
056300090116         IF  DB0gc2 <> *blank;
056400090116           if  %subst( DB0gc2 : 1 : 1 ) = *blank;
056500100721             // ·?Nessun giorno specificato - Solo "M" o "P"?
056600090116             if  DB0gc2 = ' M';
056700090116               Pgc2 = %triml( $Gio(8) );
056800090116             else;
056900090116               Pgc2 = %triml( $Gio(9) );
057000090116             endif;
057100090116           else;
057200100721             // ·?Giorno specificato?
057300090116             yy = %lookup( %subst( DB0gc2 : 1 : 1 ) : $GgS );
057400090116             if  yy > *zero;
057500090116               Pgc2 = $Gio(yy);
057600090116             endif;
057700100721             // +?"M" o "P"?
057800090116             select;
057900090116               when  %subst( DB0gc2 : 2 : 1 ) = 'M';
058000090116                 Pgc2 = %trimr( Pgc2 )  +  %trim( $Gio(8) );
058100090116               when  %subst( DB0gc2 : 2 : 1 ) = 'P';
058200090116                 Pgc2 = %trimr( Pgc2 )  +  %trim( $Gio(9) );
058300090116             endsl;
058400090116           endif;
058500090116         ENDIF;
058600090116
058700100414         // -?Valore da assicurare e relativa divisa?
058800090116         if  DB0ias <> *zero;
058900100722           Pdas  = DB0vas;
059000100722           Pias  = DB0ias;
059100090116         endif;
059200090116
059300100414         // -?Contrassegno e relativa divisa?
059400090116         if  DB0cas <> *zero;
059500100722           Pdca  = DB0vca;
059600100722           Pcas  = DB0cas;
059700090116         endif;
059800090116
059900100414         // -?Colli: numero, peso, volume, flag tipo-volume?
060000100722         Pncl  = DB0ncl;
060100100722         Ppkf  = DB0pkf;
060200100722         Pvlf  = DB0vlf;
060300090116
060400100414         // -?Natura merce?
060500100722         Pnat  = DB0nas;
060600090116
060700100414         // -?Note?
060800100722         Pnp1  = %trim(DB0nt1);
060900090116
061000090116
061100100414         // -Costanti in stampa?
061200100722         //Kacc  = c_Accettaz;
061300100722         //Kfir  = c_Firma;
061400100722         //Kdat  = c_Data;
061500100721
061600100722         kXS1  = $5Ed(1);     // - Espresso/Priority
061700100722         kXS2  = $5Ed(2);     // - h 10:30
061800090116
061900090116       ENDSR;
062000090116
062100090116       //--------------------------------------------------------------
062200090116       //?Posizionamento del box all'interno della pag. A4             ?
062300090116       //--------------------------------------------------------------
062400090116       BEGSR  sr_PosizBox;
062500090116
062600100414         // -?Calcolo la posizione x box all'interno della pagina "A4"?
062700090116         wNumDoc += 1;
062800090116         wSeqDoc = %rem( wNumDoc : $BxP );
062900090116
063000090116         if  wSeqDoc = *zero;
063100090116           wSeqDoc = 4;
063200090116         endif;
063300090116
063400100414         // -?Posizionamento nella pagina A4 in base al n° Doc. in?
063500100414         //  ?stampa?
063600090116         *in41 = (wSeqDoc = 1);
063700090116         *in42 = (wSeqDoc = 2);
063800090116         *in43 = (wSeqDoc = 3);
063900090116         *in44 = (wSeqDoc = 4);
064000100722         write  LSB9Pos;
064100090116
064200100414         // -?Imposto le posizioni nella pagina in corso di stampa?
064300090116         wPosIni = (CurLine / 9) - 0.120;
064400090116
064500120329         // -?Simil-Logo BRT?
064600100721         §PlblS  = wPosIni + $PS0;
064700120329         §PlblVS = 7.539;
064800120329         exsr  sr_SimilLogoBRT_;
064900090116
065000120329         // -?Impotazione dati BRT S.p.A.?
065100130214         TBAR  = $BRT;                   // - BRT S.p.A.
065200130214         TBA1  = $Intest1;               // - "Sede Operativa ed Amministrativa"
065300130214         TBA2  = $Intest2;               // - Indirizzo (BO)
065400130214         //TBA3  = $Intest3;               // - Sede legale: indirizzo
065500130214         TBA4  = $Intest4;               // - C.F. / P.I.
065600130214         TBA5  = $Intest5;               // - Capitale sociale
065700130214         //TBA6  = $Intest6;               // - "info on-line"
065800130214         TBA7  = $Intest7_BRT;           // - sito internet
065900090116         §PC0  = wPosIni + c_PosTBar;
066000090116         §PC1  = wPosIni + c_PosTBa1;
066100090116         §PC2  = wPosIni + c_PosTBa2;
066200090116         §PC3  = wPosIni + c_PosTBa3;
066300090116         §PC4  = wPosIni + c_PosTBa4;
066400130214         §PC5  = wPosIni + c_PosTBa5;
066500130214         §PC6  = wPosIni + c_PosTBa6;
066600090116
066700100414         // -?Box generale?
066800090116         §BoxA = wPosIni + c_PosBoxA;
066900090116         §BoxB = wPosIni + c_PosBoxB;
067000090116
067100100414         // -?Linee orizzontali?
067200090116         §LO1  = wPosIni + c_PosLO1;
067300090116         §LO2  = wPosIni + c_PosLO2;
067400090116         §LO3  = wPosIni + c_PosLO3;
067500090116         §LO4  = wPosIni + c_PosLO4;
067600090116         §LO5  = wPosIni + c_PosLO5;
067700090116         §LO6  = wPosIni + c_PosLO6;
067800090116         §LO7  = wPosIni + c_PosLO7;
067900090116         §LO8  = wPosIni + c_PosLO8;
068000090116         §LO9  = wPosIni + c_PosLO9;
068100090116         §LO0  = wPosIni + c_PosLO0;
068200090116         §LOA  = §BoxB - 0.080;
068300090116
068400100414         // -?Linee verticali?
068500090116         §LV1  = §BoxB - §BoxA;
068600090116         §LV2  = §LO7  - §LO3;
068700100721         §LV3  = §LO8  - §LO7;
068800100721         §LV4  = §LO9  - §LO8;
068900090116
069000100414         // -?Mini-Box interni?
069100090116         §Box1a = §LO4 + 0.020;
069200090116         §Box1b = §Box1a + 0.100;
069300100721         §Box2a = §LO8 + ( ( c_PosLo9 - c_PosLO8 - 0.100 ) / 2 );
069400100721         §Box2b = §Box2a + 0.100;
069500090116
069600100414         // -?Costanti nel box?
069700100721         §pkXFD = §Box2b - 0.015;
069800090116         §pkFIR = §LOA + 0.062;
069900090116
070000090116       ENDSR;
070100100721
070200110503       //--------------------------------------------------------------
070300120329       //?Posizionamento del simil-logo BRT (unico)                    ?
070400110503       //--------------------------------------------------------------
070500120329      /copy gaitrasrc/srcTNSY,LogoBRT_R2
070600090116
070700090116       //--------------------------------------------------------------
070800090116       //?Stampa (Documento di Ritiro)                               ?
070900090116       //--------------------------------------------------------------
071000090116       BEGSR  sr_Stampa;
071100090119
071200100414         // -?Numero documento?
071300100721         //  ?(unico campo NON ancora impostato)?
071400100721         Pndc  = NUFnum;
071500090116
071600100721         // -?Impostazione indicatori?
071700090116         *in51 = (Pmitt00 = *blank   and
071800090116                  Pmitt01 = *all'.'  and
071900090116                  Pmitt02 = *all'.'  and
072000090116                  Pmitt03 = *all'.'  and
072100090116                  Pmitt04 = *all'.'  and
072200090116                  Pmitt05 = *all'.'  and
072300090119                  Pmitt06 = *all'.');
072400090119         *in52 = (Pmitt07 = *all'.'  and
072500090119                  Pmitt08 = *all'.');
072600090119         *in53 = (Pdest00 = *blank   and
072700090116                  Pdest01 = *all'.'  and
072800090116                  Pdest02 = *all'.'  and
072900090116                  Pdest03 = *all'.'  and
073000090116                  Pdest04 = *all'.'  and
073100090116                  Pdest05 = *all'.'  and
073200090116                  Pdest06 = *all'.'  and
073300090119                  Pdest07 = *all'.'  and
073400090119                  Pdest08 = *all'.'  and
073500090119                  Pdest09 = *all'.');
073600090116
073700100721         // -?Stampa box?
073800100722         write  LSB9Box;
073900090116
074000120329         // -?Stampa logo BRT?
074100120329         write  LSB9LogBrt;
074200090119
074300100721         // -?Stampa indicazione dei dati da inserire a mano?
074400090119         if  *in53;
074500090119           §pkInd = ((Curline + 16) / 9) + 0.055;
074600090119           §pkLoc = ((Curline + 18) / 9) + 0.055;
074700090119           §pkRef = ((Curline + 21) / 9) + 0.055;
074800100722           write  LSB9Mand;
074900090119         endif;
075000090116
075100100721         // -?Stampa testo?
075200100722         write  LSB9Txt;
075300090116
075400090116       ENDSR;
075500090116
075600090116       //--------------------------------------------------------------
075700090116       //?Operazioni finali.                                         ?
075800090116       //--------------------------------------------------------------
075900090116       BEGSR  sr_RoutEnd;
076000090116
076100100721         // -?Chiusura file di stampa?
076200100414         //   (Tipo lancio, DB0tla: "C" -           Chiude con LR
076300100414         //                         "L" - Elabora e chiude con LR
076400100414         //                         " " - Elabora e chiude in RETURN)
076500090119         if  DB0tla <> *blank;
076600100722           close  FNLSB9P;
076700090119         endif;
076800090116
076900100721         // -?Chiusura pgm?
077000090116         return;
077100090116
077200090116       ENDSR;
077300090116
077400090116       //--------------------------------------------------------------
077500090116       //?Schiere a tempo di compilazione.                             ?
077600090116       //--------------------------------------------------------------
077700090116
077800100414** -?$GgS/$Gio?
0779000901161LUN
0780000901162MAR
0781000901163MER
0782000901164GIO
0783000901165VEN
0784000901166SAB
0785000901167DOM
078600090116M   -MATT.
078700090116P   -POM.
