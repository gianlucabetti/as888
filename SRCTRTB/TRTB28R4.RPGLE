000100120612       //==============================================================
000200120612       //?Assegnazione nuovo Unificante in tab. "3C".                  ?
000300120612       //==============================================================
000400120612
000500120612       //--------------------------------------------------------------
000600120612       //?Parametri di compilazione (vedi *cmd UBCRTOBJ).              ?
000700120612       //--------------------------------------------------------------
000800120612
000900120612     /*END
001000120612
001100120612       //--------------------------------------------------------------
001200120612       //?Specifiche di controllo.                                     ?
001300120612       //--------------------------------------------------------------
001400120612
001500120612     h decedit('0,') datedit(*dmy/) option(*nodebugio)
001600121008     h dftactgrp(*no)
001700121008     h bnddir('UBBNDDIR')
001800121128       // »?Deve?essere specificato DFTACTGRP(*NO) per un prototipo che?
001900121128       //  ?non ha la parola chiave EXTPGM.?
002000120612       //  ?(vedi API QSNDRTVMOD - Retrieve Display Mode)?
002100120612
002200120612       //--------------------------------------------------------------
002300120612       //?Dichiarazione file.
002400120612       //--------------------------------------------------------------
002500120612
002600120612       // -?Tabella "3C"?
002700120618     fTABEL00F  Uf   e           k disk
002800120618     f                                     extfile(wLibFile1)  usropn
002900120618     fTNTBE01L  Uf   e           k disk
003000120618     f                                     extfile(wLibFile2)  usropn
003100120612
003200120612       // -?Video?
003300120806     fTRTB28D4  cf   e             workstn usropn
003400120612     f                                     indds(IndDspF)
003500120612     f                                     infds(InfDspF)
003600120612
003700120612       //--------------------------------------------------------------
003800120612       //?Definizione costanti.                                        ?
003900120612       //--------------------------------------------------------------
004000120612
004100120612       // -?Codice Utente / Lingua italiana  in tabelle su TABEL00F?
004200120612     d c_Kut           c                   const(1)
004300120612
004400120612       // -?Codice tabella in gestione?
004500120612     d c_Tab           c                   const('3C')
004600121008
004700121008       // -?Costante Supporto "EasyWEB"?
004800121008     d c_EasyWEB       c                   const('ESWEB')
004900120612
005000120612       // -?Costanti per la definizione delle schiere con i nomi?
005100120612       //  ?degli iSeries da elaborare e delle relative librerie?
005200120612     d c_NrSyst        c                   const(2)
005300120612     d c_NrLibr        c                   const(2)
005400120612
005500120612       // -?Tasti funzionali a video?
005600120612     d c_F01           c                   const(x'31')
005700120612     d c_F02           c                   const(x'32')
005800120612     d c_F03           c                   const(x'33')
005900120612     d c_F04           c                   const(x'34')
006000120612     d c_F05           c                   const(x'35')
006100120612     d c_F06           c                   const(x'36')
006200120612     d c_F07           c                   const(x'37')
006300120612     d c_F08           c                   const(x'38')
006400120612     d c_F09           c                   const(x'39')
006500120612     d c_F10           c                   const(x'3A')
006600120612     d c_F11           c                   const(x'3B')
006700120612     d c_F12           c                   const(x'3C')
006800120612     d c_F13           c                   const(x'B1')
006900120612     d c_F14           c                   const(x'B2')
007000120612     d c_F15           c                   const(x'B3')
007100120612     d c_F16           c                   const(x'B4')
007200120612     d c_F17           c                   const(x'B5')
007300120612     d c_F18           c                   const(x'B6')
007400120612     d c_F19           c                   const(x'B7')
007500120612     d c_F20           c                   const(x'B8')
007600120612     d c_F21           c                   const(x'B9')
007700120612     d c_F22           c                   const(x'BA')
007800120612     d c_F23           c                   const(x'BB')
007900120612     d c_F24           c                   const(x'BC')
008000120612     d c_Enter         c                   const(x'F1')
008100120612     d c_RollDown      c                   const(x'F4')
008200120612     d c_RollUp        c                   const(x'F5')
008300120612
008400120612       // -?Costante per controllo "caratteri solo numerici"?
008500120612     d c_Digits        c                   const('0123456789')
008600120612
008700120612       //--------------------------------------------------------------
008800120612       //?Definizione schiere.                                         ?
008900120612       //--------------------------------------------------------------
009000120612
009100120612       // -?iSeries  &  Librerie con entrambi i file tabelle?
009200120612     d $iSystem        s                   like(currSysNetA)
009300120612     d                                     dim(c_NrSyst)
009400120612     d                                     ctdata   perrcd( 1)
009500120612     d $Libraries      s                   like(ds_Libr)
009600120612     d                                     dim(c_NrSyst)
009700120612     d                                     alt($iSystem)
009800120612
009900120612       // -?Messaggi di errore?
010000121008     d $Msg            s             78    dim(13) ctdata perrcd(1)
010100120612
010200120612       //--------------------------------------------------------------
010300120612       //?Definizione aree dati.                                       ?
010400120612       //--------------------------------------------------------------
010500120612
010600120612       // -?Dati utente?
010700120612     d §AzUte        e ds                  extname(AZUTE00F)
010800120612     d                                     dtaara
010900120612     d §DatiUte      e ds                  extname(dDatiUte)
011000120612     d                                     dtaara
011100120612
011200120612       //--------------------------------------------------------------
011300120612       //?Definizione strutture dati.                                  ?
011400120612       //--------------------------------------------------------------
011500120612
011600120612       // -?Status ds?
011700120612     d Status         sds
011800120612     d   SDSpgm          *proc
011900120612
012000120612       // -?Indicatori su DspF?
012100120612     d IndDspF         ds
012200121008         // ·?Emissione messaggio di errore?
012300120612     d   ErrMessage                    n   overlay(IndDspF : 28)
012400121008         // ·?Condzionamento tipo di emissione?
012500120612     d   xDs4                          n   overlay(IndDspF : 34)
012600121008         // ·?Posizionamento cursore?
012700120618     d   PosCurKSU                     n   overlay(IndDspF : 51)
012800121008         // ·?Riemissione videata?
012900120612     d   ErrGenerico                   n   overlay(IndDspF : 99)
013000120612
013100120612       // -?InfDS?
013200120612     d InfDspF         ds
013300120612     d   dsp_aid             369    369a
013400120612
013500120612       // -?Ridefinizione elenco librerie in elaborare le tabelle?
013600120612     d ds_Libr         ds                  inz
013700120612     d   $Libr                       10    dim(c_NrLibr) inz
013800120612
013900120612       // -?Parametri ricevuti?
014000120612     d KPJBA         e ds
014100121128     d TRTB28ds4     e ds                              inz
014200121128         // ·?IN - Codice Cliente Unificante Attuale?
014300121128     d*//Itb28ksuA                    7s 0             inz
014400121128         // ·?OUT - Codice Cliente Unificante Nuovo?
014500121128     d*//Otb28ksuN                    7s 0             inz
014600121128         // ·?OUT - Tasto funzionale di uscita?
014700121128         //        ?(03=F3, 06=F6)?
014800121128     d*//Otb28fxx                     2a               inz
014900121128         // ·?OUT - Flag di errore?
015000121128     d*//Otb28err                     1a               inz
015100121128         // ·?OUT - Messaggio di errore?
015200121128     d*//Otb28msg                    78a               inz
015300120612
015400120612       // -?Tab. "3C" = Invio dati -BOLLETTAZIONE-?
015500121008     d ds3CunifOld   e ds                  extname(ds3C)
015600120612     d                                     qualified   inz
015700121008     d ds3CunifNew   e ds                  extname(ds3C)
015800121008     d                                     qualified   inz
015900120612     d ds3C          e ds                  qualified   inz
016000121008     d d3EWunifOld   e ds                  extname(d3EW)
016100120618     d                                     qualified   inz
016200121128     d d3EW          e ds                  qualified   inz
016300120612
016400120612       //--------------------------------------------------------------
016500120612       //?Definizione variabili.                                       ?
016600120612       //--------------------------------------------------------------
016700120612
016800120612       // -?Flags booleani?
016900120612     d $Fine           s               n   inz
017000120612     d $InzW01         s               n   inz(*on)
017100120618     d $Bypass_01      s               n   inz
017200121008     d $Found_TIVSS    s               n   inz
017300120612
017400120612       // -?Indici di schiera?
017500120612     d xx              s              3  0 inz
017600120612
017700120612       // -?Variabili per la gestione del video?
017800120612     d $Video          s              2    inz('W1')
017900121128
018000121128       // -?Variabili per il controllo delle variazioni?
018100121128     d SaveW2ksu       s                   like(W2Cksu)  inz
018200120612
018300120612       // -?Nome del sistema?
018400120612     d currSysNeta     s              8a   inz
018500120612
018600120612       // -?Nome esteso Libreria/File del file tabella?
018700120618     d wLibFile1       s             21a   inz
018800120618     d wLibFile2       s             21a   inz
018900120612
019000120612       // -?Campi di comodo?
019100120612     d w_iSystem       s              1  0 inz
019200120612     d w_SisInf        s              3  0 inz
019300121008     d wDate           s              8  0 inz
019400120612
019500120612       //--------------------------------------------------------------
019600120612       //?Definizione prototipi procedure & relativi parametri.        ?
019700120612       //--------------------------------------------------------------
019800120612
019900120612       // -?Reperimento dati utente?
020000120612     d TIBS34ds      e ds                  inz
020100120612      /copy gaitrasrc/srcProtoPR,TIBS34R
020200120612
020300120612       // -?Reperimento legami in tab. "3C"?
020400120612      /copy gaitrasrc/srcProtoPI,UBLEG3C
020500120612      /copy gaitrasrc/srcProtoPR,UBLEG3C
020600120612
020700120612       // -?Interrogazione tabella "3C" (oltre 9.999 record)?
020800120612     d trtb28r0        pr                  extpgm('TRTB28R0')
020900120612     d   kpjba                             likeds(KPJBA)
021000120618
021100120618       // -?Parametri per verifica abilitazione STRATEGI?
021200120618      /copy gaitrasrc/srcProtoPI,TIS7701R
021300121008     d TIVSSds       e ds                  extname(TIVSS00F) qualified
021400120618     d TIVSSds2      e ds                  extname(TIVSS00F) qualified
021500120618       // -?Verifica abilitazione STRATEGI?
021600120618      /copy gaitrasrc/srcProtoPR,TIS7701R
021700120612
021800120612       // -?Reperimento NETA sistema AS/400 corrente?
021900120612      /copy gaitrasrc/srcProtoPR,UBRTVNETA
022000120612
022100120612       // -?API QSNDRTVMOD (Retrieve Display Mode)?
022200120612     d QdspMode        s              1    inz
022300120612      /copy gaitrasrc/srcProtoPR,QSNRTVMOD
022400120612
022500120612       // -?API QCAPCMD (Process Commands)?
022600120612     d Qcmd            s           2048    inz  varying
022700120612      /copy qSysInc/qRpgleSrc,QCAPCMD
022800120612      /copy gaitrasrc/srcProtoPR,QCAPCMD
022900120612
023000120612       // -?Parametri gestione errori API.?
023100120612      /copy qsysinc/qrpglesrc,QUSEC
023200120612
023300120612       //--------------------------------------------------------------
023400120612       //?Definizione key-list.                                        ?
023500120612       //--------------------------------------------------------------
023600120612
023700120612       // -?File TABEL00F?
023800120612     d k03tabel00    e ds                  extname(TABEL00F : *key)
023900120612     d                                     prefix(k_)   inz
024000121008
024100121008       // -?File TNTBE01L?
024200121008     d k05tntbe01    e ds                  extname(TNTBE01L : *key)
024300121008     d                                     prefix(k_)   inz
024400120612
024500120612       //--------------------------------------------------------------
024600120612       //?M A I N - L I N E                                            ?
024700120612       //--------------------------------------------------------------
024800120612
024900120612     c     *Entry        plist
025000120612     c                   parm                    KPJBA
025100120612
025200120612      /free
025300120612
025400120612       // -?Operazioni iniziali?
025500120612       exsr  sr_RoutInz;
025600120612
025700120612       // -?Gestione window?
025800120612       DoW  Not $Fine;
025900120612         select;
026000120612           when  $Video = 'W1';
026100120612             exsr  sr_GesW01;
026200120612           other;
026300120612             $Fine = *on;
026400120612         endsl;
026500120612       EndDo;
026600120612
026700120612       // -?Operazioni finali?
026800120612       exsr  sr_RoutEnd;
026900120612
027000120612       //--------------------------------------------------------------
027100120612       //?Operazioni iniziali.                                         ?
027200120612       //--------------------------------------------------------------
027300120612       BEGSR  sr_RoutInz;
027400120612
027500120612         *inLR = *on;
027600120612
027700120612         // -?Impostazione parametri ricevuti?
027800120612         if  KPJBU <> *blank;
027900121008           TRTB28ds4 = KPJBU;
028000120612         endif;
028100120612         clear  Otb28ksuN;
028200120612         clear  Otb28fxx;
028300120612         Otb28err = *off;
028400120612         clear  Otb28msg;
028500120612
028600120612         // -?Verifica del sistema AS/400 corrente?
028700120612         if  UBRTVNETA_Rtv(currSysNeta) <> *zero;
028800120612           Otb28err = *on;
028900120612           Otb28msg = $Msg(01);
029000120612           $Fine = *on;
029100120612           leavesr;
029200120612         endif;
029300120612
029400120612         // -?Impostazione elenco librerie in cui gestire le tabelle?
029500120612         //  ?(a seconda del sistema in cui si stà lavorando)?
029600120612         w_iSystem = %lookup ( %subst ( currSysNeta : 1 : 6 ) : $iSystem );
029700120612         if  w_iSystem = *zero;
029800120612           Otb28err = *on;
029900120612           Otb28msg = $Msg(02);
030000120612           $Fine = *on;
030100120612           leavesr;
030200120612         endif;
030300120612
030400120612         // -?Impostazione campi "fissi"?
030500121008         clear  k03tabel00;
030600120612         k_TBLkut = c_Kut;
030700120612         k_TBLcod = c_Tab;
030800121008         clear  k05tntbe01;
030900121008         k_TBEcod = '3EW';
031000120612
031100120612         // -?Reperimento dati del lavoro?
031200120612         exsr  sr_DatiJob;
031300121008
031400121008         // -?Reperimento data odierna?
031500121008         wDate = %int( %subst( %char( %dec( %timestamp() ) )
031600121008                               : 1 : 8 ) );
031700120612
031800120612         // -?Reperimento formato attuale display?
031900120612         clear  TB28P01;
032000120612         RetrieveDisplayMode ( QdspMode : *omit : *omit );
032100120612         xDs4 = (QdspMode = '4');
032200120806         open  TRTB28D4;
032300120612
032400120612         // -?Posizionamento window?
032500120612         //   -?Prima prova con le posizioni ricevute (SE ricevute)?
032600120612         //if  Itb28rig <> *zero   and   Itb28col <> *zero;
032700120612         //  W1rig = Itb28rig;
032800120612         //  W1col = Itb28col;
032900120612         //  write(e)  TB28P01;
033000120612         //endif;
033100120612         //   -?In caso di errore (o SE NON ricevute posizioni):?
033200120612         //    ?imposta le posizioni di default (in basso a sx)?
033300120612         //if  %error              or
033400120612         //    Itb28rig = *zero    or    Itb28col = *zero;
033500120612           if  Not xDs4;
033600120612             W1rig = 14;
033700120612           else;
033800120612             W1rig = 18;
033900120612           endif;
034000120612           W1col = 2;
034100120612         //endif;
034200120612
034300120612       ENDSR;
034400120612
034500120612       //--------------------------------------------------------------
034600120612       //?Reperimento Dati del job (Utente/Operativi).                 ?
034700120612       //--------------------------------------------------------------
034800120612       BEGSR  sr_DatiJob;
034900120612
035000120612         in(E) §AzUte;
035100120612         if NOT %error;
035200120612           in(E) §DatiUte;
035300120612         endif;
035400120612         if %error or RSut = *blanks;
035500120612           clear TIBS34ds;
035600120612           tibs34r(tibs34ds);
035700120612           in §AzUte;
035800120612           in §DatiUte;
035900120612         endif;
036000120612
036100120612       ENDSR;
036200120612
036300120612       //--------------------------------------------------------------
036400120612       //?Gestione videata W01.                                        ?
036500120612       //--------------------------------------------------------------
036600120612       BEGSR  sr_GesW01;
036700120612
036800120612         // -?Inizializzazione videata?
036900120612         if  $InzW01;
037000120612           exsr  sr_InzW01;
037100120612           $InzW01 = *off;
037200120612         endif;
037300120612
037400120612         // -?Emissione Piede (window)?
037500120612         write  TB28P01;
037600120612
037700120612         // -?Emissione videata?
037800120612         exfmt  TB28W01;
037900120612
038000120612         clear  W1Dmsg;
038100120612         clear  errMessage;
038200120612         clear  errGenerico;
038300120612
038400120612         Select;
038500120612
038600120612           // -?Errore => uscita?
038700120612           When  $Fine;
038800120612
038900120612           // -?F3=Fine?
039000120612           When  dsp_aid = c_F03;
039100120612             exsr  sr_F03W01;
039200120612
039300120612           // -?F12=Ritorno?
039400120612           When  dsp_aid = c_F12;
039500120612             exsr  sr_F12W01;
039600120612
039700120612           // -?Invio / F6=Conferma?
039800120612           Other;
039900120612             exsr  sr_CtrW01;
040000120612             select;
040100120612               when  ErrGenerico;
040200120612                 leavesr;
040300120612               when  dsp_aid = c_F06;
040400120612                 exsr  sr_F06W01;
040500120612             endsl;
040600120612
040700120612         EndSl;
040800120612
040900120612       ENDSR;
041000120612
041100120612       //--------------------------------------------------------------
041200121008       //?Inizializzazione videata W01.                                ?
041300120612       //--------------------------------------------------------------
041400120612       BEGSR  sr_InzW01;
041500120612
041600120612         // -?Apertura file TABEL00F del 1° S.I. (sede)?
041700120612         if  w_SisInf <> 1   or   Not %open(TABEL00F);
041800120612           w_SisInf = 1;
041900120612           exsr  sr_OpenFileTab;
042000120612         endif;
042100120612
042200120612         // -?Impostazione a video parametri ricevuti?
042300120612         clear  TB28W01;
042400121205         W0Npgm = SDSpgm;
042500120612         if  Itb28ksuA <= *zero;
042600120612           Otb28err = *on;
042700120612           Otb28msg = $Msg(03);
042800120612           $Fine  = *on;
042900121008           W1Dmsg = %trimr($Msg(03)) +
043000121008                    ' - Premere "Invio" per uscire.';
043100120612           ErrGenerico = *on;
043200120612           ErrMessage  = *on;
043300120612           leavesr;
043400120612         endif;
043500120612
043600120612         W1Cksu = Itb28ksuA;
043700120612
043800120612         // -?Decodifica dell'Unificante attuale?
043900121008         clear  ds3CunifOld;
044000120612         k_TBLkey = %editc( W1Cksu : 'X' );
044100120612         chain(n)  %kds(k03tabel00)  TABEL;
044200120612         if  %found(TABEL00F)   and   TBLflg = *blank;
044300121008           ds3CunifOld = TBLuni;
044400121008           W1Dksu = ds3CunifOld.§3Crag;
044500121008         else;
044600121008           Otb28err = *on;
044700121008           Otb28msg = $Msg(04);
044800121008           $Fine = *on;
044900121008           W1Dmsg = %trimr($Msg(04)) +
045000121008                    ' - Premere "Invio" per uscire.';
045100121008           ErrGenerico = *on;
045200121008           ErrMessage  = *on;
045300121008           leavesr;
045400120612         endif;
045500121008
045600121008         // -?Reperim. dati EasySped-WEB dell'Unificante attuale?
045700121008         clear  $Found_TIVSS;
045800121008         clear  rpyEsito;
045900121008         clear  d3EWunifOld;
046000121008         If  ds3CunifOld.§3Ccba = c_EasyWEB;
046100121008
046200121008           // -?Apertura files tabelle di Sede?
046300121008           if  w_SisInf <> 1   or   Not %open(TNTBE01L);
046400121008             w_SisInf = 1;
046500121008             exsr  sr_OpenFileTbe;
046600121008           endif;
046700121008
046800121008           // -?Reperimento chiave completa per tab. "3EW"?
046900121008           rqsKSU = '0' + %editc( W1Cksu : 'X' );
047000121008           clear  rpyEsito;
047100121008           Selettore_Record_TIVSS ( 'GETRCDVSS' : '0' : rqsKSU : 'EW' :
047200121008                                    rqsSUN : *zero : TIVSSds :
047300121008                                    rpyEsito );
047400121008           $Found_TIVSS = (rpyEsito = *zero  and  TIVSSds.VSSksu > *zero);
047500121008
047600121008           // -?Reperim. dati EasySped-WEB dell'Unificante attuale?
047700121008           k_TBEke1 = TIVSSds.VSSksu;
047800121008           if  $Found_TIVSS;
047900121008             k_TBEke2 = TIVSSds.VSSsun;
048000121008             chain  %kds(k05tntbe01 : 3)  TNTBE000;
048100121008           else;
048200121008             chain  %kds(k05tntbe01 : 2)  TNTBE000;
048300121008           endif;
048400121008           if  %found(TNTBE01L)   and   TBEatb = *blank;
048500121008             d3EWunifOld = TBEuni;
048600121008           endif;
048700121008
048800121008         EndIf;
048900120618
049000120612         // -?Reperimento famiglia completa dell'Unificante attuale?
049100120612         ubLeg3C_Rtv ( W1Cksu :
049200120612                       ubLeg3C_FlgPF :
049300120612                       ubLeg3C_Padre :
049400120612                       ubLeg3C_SKC :
049500120612                       ubLeg3C_SKEW );
049600120612
049700120612       ENDSR;
049800120612
049900120612       //--------------------------------------------------------------
050000121008       //?Controllo videata W01.                                       ?
050100120612       //--------------------------------------------------------------
050200120612       BEGSR  sr_CtrW01;
050300120612
050400120612         %subst(IndDspF : 50) = *off;
050500120612
050600120612         // -?Ricerche?
050700120612         exsr  sr_Search;
050800120612         if  ErrGenerico;
050900120612           leavesr;
051000120612         endif;
051100120612
051200121008         // -?CONTROLLO CODICE NUOVO UNIFICANTE:?
051300121008         clear  W2Dksu;
051400120612         Select;
051500121008           // ·?Codice nuovo Unificante mancante?
051600120612           When  W2Cksu = *blank  or  W2Cksu = *zero;
051700121008             W1Dmsg = $Msg(05);
051800120612             PosCurKSU   = *on;
051900120612             ErrGenerico = *on;
052000120612             ErrMessage  = *on;
052100120612             leavesr;
052200121008           // ·?Codice nuovo Unificante NON numerico?
052300120612           When  %check( c_Digits : W2Cksu ) > *zero;
052400121008             W1Dmsg = $Msg(06);
052500120612             PosCurKSU   = *on;
052600120612             ErrMessage  = *on;
052700120612             ErrGenerico = *on;
052800120612             leavesr;
052900120612           // ·?Codice nuovo Unificante uguale all'attuale?
053000120612           When  %editc( W1Cksu : 'X' ) = W2Cksu;
053100121008             W2Dksu = W1Dksu;
053200121008             W1Dmsg = $Msg(07);
053300120612             PosCurKSU   = *on;
053400120612             ErrMessage  = *on;
053500120612             ErrGenerico = *on;
053600120612             leavesr;
053700120612         EndSl;
053800120612         // ·?Codice nuovo Unificante errato?
053900120612         k_TBLkey = W2Cksu;
054000120618         chain(n)  %kds(k03tabel00)  TABEL;
054100120612         if  Not %found(TABEL00F)  or  TBLflg <> *blank;
054200121008           W1Dmsg = $Msg(08);
054300120612           PosCurKSU   = *on;
054400120612           ErrMessage  = *on;
054500120612           ErrGenerico = *on;
054600120612           leavesr;
054700120618         else;
054800121008           ds3CunifNew = TBLuni;
054900121008           W2Dksu = ds3CunifNew.§3Crag;
055000120612         endif;
055100121008
055200120612         // ·?Codice nuovo Unificante NON nella famiglia dell'attuale?
055300121205         //  ?(se NON EasySped-WEB: serie e range DEVONO rimanere?
055400121205         //  ?quelli attuali)?
055500121205         If  ds3CunifOld.§3Ccba <> c_EasyWEB  and
055600121205             %lookup( %int(W2Cksu) : sch_SKC ) = *zero;
055700121205           W1Dmsg = $Msg(09);
055800121205           PosCurKSU   = *on;
055900121205           ErrMessage  = *on;
056000121205           ErrGenerico = *on;
056100121205           leavesr;
056200121205         endif;
056300121008
056400121008         // ·?Codice nuovo Unificante già figlio di altro Unificante?
056500121205         if  %lookup( %int(W2Cksu) : sch_SKC ) = *zero  and
056600121205             %int(W2Cksu) <> ds3CunifNew.§3Ccks;
056700121205           W1Dmsg = $Msg(10);
056800121205           PosCurKSU   = *on;
056900121205           ErrMessage  = *on;
057000121205           ErrGenerico = *on;
057100121205           leavesr;
057200121205         endif;
057300121008
057400120618         // ·?Codice nuovo Unificante NON ancora abilitato ad EasyWEB?
057500121008         IF  ds3CunifOld.§3Ccba = c_EasyWEB;
057600121008           If  ds3CunifNew.§3Ccba <> c_EasyWEB;
057700121205             W1Dmsg = %trimr($Msg(11)) + ds3CunifNew.§3Ccba + '"';
057800120618             PosCurKSU   = *on;
057900120618             ErrMessage  = *on;
058000120618             ErrGenerico = *on;
058100120618             leavesr;
058200120618           Else;
058300120618             clear  rpyEsito;
058400121008             rqsKSU = '0' + W2Cksu;
058500120618             Selettore_Record_TIVSS ( 'GETRCDVSS' : '0' : rqsKSU : 'EW' :
058600120618                                      rqsSUN : *zero : TIVSSds2 :
058700120618                                      rpyEsito );
058800121008             if  rpyEsito <> *zero  or  TIVSSds2.VSSksu = *zero;
058900121008               W1Dmsg = $Msg(12);
059000120618               PosCurKSU   = *on;
059100120618               ErrMessage  = *on;
059200120618               ErrGenerico = *on;
059300120618               leavesr;
059400120618             endif;
059500120618           EndIf;
059600120618         ENDIF;
059700120618
059800120618         // -?AVVISO: occorrerà disabilitare ad EasySped-WEB (su?
059900120618         //          ?STRATEGI) l'unificante attuale?
060000121128         if  W2Cksu <> SaveW2ksu;
060100121128           SaveW2ksu = W2Cksu;
060200121128           clear  $Bypass_01;
060300121128         endif;
060400121128         If  ds3CunifOld.§3Ccba = c_EasyWEB  and  Not $Bypass_01;
060500120618           $Bypass_01 = *on;
060600121008           if  rqsKSU <> '0' + W2Cksu;
060700121008             clear  rpyEsito;
060800121008             rqsKSU = '0' + W2Cksu;
060900121008             Selettore_Record_TIVSS ( 'GETRCDVSS' : '0' : rqsKSU : 'EW' :
061000121008                                      rqsSUN : *zero : TIVSSds2 :
061100121008                                      rpyEsito );
061200121008           endif;
061300121008           if  rpyEsito <> *zero  or  TIVSSds2.VSSksu = *zero;
061400121008             W1Dmsg = $Msg(13);
061500120618             PosCurKSU   = *on;
061600120618             ErrMessage  = *on;
061700120618             ErrGenerico = *on;
061800120618             leavesr;
061900120618           endif;
062000120618         EndIf;
062100120612
062200120612       ENDSR;
062300120612
062400120612       //--------------------------------------------------------------
062500121008       //?Gestione ricerche in videata W01.
062600120612       //--------------------------------------------------------------
062700120612       BEGSR  sr_Search;
062800120612
062900120612         Select;
063000120612
063100120612           // -?Codice Cliente?
063200120612           When  %scan( '?' : W2Cksu ) > *zero;
063300120612             kpjbu = '      ' + %editc( W1Cksu : 'X' );
063400120612             trtb28r0 ( kpjba );
063500120612             W2Cksu = kpjbu;
063600120612             clear  W2Dksu;
063700120612             ErrGenerico = *on;
063800120612             leavesr;
063900120612
064000120612           EndSl;
064100120612
064200120612       ENDSR;
064300121008
064400121008       //--------------------------------------------------------------
064500121008       //?Gestione tasto funzionale F3 da videata W01:                 ?
064600121008       //?F3=Fine                                                      ?
064700121008       //--------------------------------------------------------------
064800121008       BEGSR  sr_F03W01;
064900121008
065000121008         // -?Restituzione tasto funzionale premuto?
065100121008         Otb28fxx = '03';
065200121008
065300121008         // -?Chiusura del *pgm?
065400121008         $Fine = *on;
065500121008
065600121008       ENDSR;
065700121008
065800121008       //--------------------------------------------------------------
065900121008       //?Gestione tasto funzionale F12 da videata W01:                ?
066000121008       //?F12=Ritorno                                                  ?
066100121008       //--------------------------------------------------------------
066200121008       BEGSR  sr_F12W01;
066300121008
066400121008         // -?Restituzione tasto funzionale premuto?
066500121008         Otb28fxx = '12';
066600121008
066700121008         // -?Chiusura del *pgm?
066800121008         $Fine = *on;
066900121008
067000121008       ENDSR;
067100121008
067200121008       //--------------------------------------------------------------
067300121008       //?Gestione tasto funzionale F6 da videata W01:                 ?
067400121008       //?F6=Conferma                                                  ?
067500121008       //--------------------------------------------------------------
067600121008       BEGSR  sr_F06W01;
067700121008
067800121008         // -?Ciclo di elaborazione per ogni sistema informativo?
067900121008         For  w_SisInf = 1  To  %elem($Libr);
068000121008
068100121008           // -?Aggiornamento tab. "3C"?
068200121008           exsr  sr_UpdTABEL;
068300121008
068400121008           // -?Annullamento tab. "3EW" del vecchio unificante?
068500121008           if  ds3CunifOld.§3Ccba = c_EasyWEB   and
068600121008               %found(TNTBE01L)   and   TBEatb = *blank;
068700121008             exsr  sr_UpdTNTBE;
068800121008           endif;
068900121008
069000121008         EndFor;
069100121008
069200121008         // -?Restituzione tasto funzionale premuto?
069300121008         Otb28ksuN = %int(W2Cksu);
069400121008         Otb28fxx  = '06';
069500121008
069600121008         // -?Chiusura del *pgm?
069700121008         $Fine = *on;
069800121008
069900121008       ENDSR;
070000120612
070100120612       //--------------------------------------------------------------
070200120618       //?Aggiornamento file TABEL00F (cod. unificante in tab. "3C").  ?
070300120612       //--------------------------------------------------------------
070400120612       BEGSR  sr_UpdTABEL;
070500121008
070600121008         // -?Apertura dell'archivio TABEL00F?
070700121008         if  w_SisInf > 1;
070800121008           exsr  sr_OpenFileTab;
070900121008         endif;
071000120612
071100120612         // -?Ciclo di elaborazione clienti con l'unificante attuale?
071200120612         For  xx = 1  To  %elem(sch_SKC);
071300121008
071400121008           // ·?Il "vecchio" unificante lo si lascia così?
071500121204           //if  sch_SKC(xx) = W1Cksu;
071600121204           //  iter;
071700121204           //endif;
071800121204           // ·?Ma perchè????
071900121205           //  ?Già gestita la DISallocazione del cliente in?
072000121205           //  ?manutenzione della tab. "3C".?
072100121129
072200121129           // ·?Finiti (già elaborati tutti) i membri della famiglia?
072300121129           if  sch_SKC(xx) = *zero;
072400121129             leave;
072500121129           endif;
072600120612
072700120612           // ·?Aggancio record da aggiornare?
072800120612           //  ?(dopo aver impostato la libreria del file da aggiornare)?
072900120612           k_TBLkey = %editc( sch_SKC(xx) : 'X' );
073000120612           chain  %kds(k03tabel00)  TABEL;
073100120612
073200120612           if  Not  %found(TABEL00F);
073300120612             iter;
073400120612           endif;
073500120612
073600120612           ds3C = TBLuni;
073700120612           ds3C.§3Ccks = %int(W2Cksu);
073800121205           // -?Se ESWEB: Serie (e range segnacolli) del padre?
073900121205           if  ds3C.§3Ccba = c_EasyWEB;
074000121205             ds3C.§3Cnrs = ds3CunifNew.§3Cnrs;
074100121205           endif;
074200121128           // -?Impostazione eventuali dati mancanti?
074300120612           Select;
074400121008             When  k_TBLkey = W2Cksu  and  ds3C.§3Cnrs > *zero
074500121008                                      and  ds3C.§3Cbac = *blank;
074600121008               if  ds3CunifOld.§3Cbac <> *blank;
074700121008                 ds3C.§3Cbac = ds3CunifOld.§3Cbac;
074800121008               else;
074900121008                 ds3C.§3Cbac = 'EMAIL';
075000121008               endif;
075100120612             When  k_TBLkey <> W2Cksu;
075200120612               clear  ds3C.§3Cbac;
075300120612           EndSl;
075400120612           TBLuni = ds3C;
075500120612           //____________
075600120612           update  TABEL;
075700120612           //¯¯¯¯¯¯¯¯¯¯¯¯
075800120612
075900120612         EndFor;
076000120612
076100120612       ENDSR;
076200120618
076300120618       //--------------------------------------------------------------
076400121008       //?Aggiornamento file TNTBE00F (vecchio unificante in tab."3EW")?
076500120618       //--------------------------------------------------------------
076600120618       BEGSR  sr_UpdTNTBE;
076700120618
076800121008         // -?Apertura dell'archivio TNTBE01L?
076900120618         if  w_SisInf > 1;
077000120618           exsr  sr_OpenFileTbe;
077100121008         endif;
077200121008
077300121008         // -?Reperimento dati da aggiornare?
077400121128         clear  d3EW;
077500121205         k_TBEke1 = TIVSSds.VSSksu;
077600121205         k_TBEke2 = TIVSSds.VSSsun;
077700121008         chain  %kds(k05tntbe01 : 3)  TNTBE000;
077800121008         if  %found(TNTBE01L)   and   TBEatb = *blank;
077900121128           //TBEatb = 'A';
078000121128           ////_______________
078100121128           //update  TNTBE000;
078200121128           ////¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
078300121128           d3EW = TBEuni;
078400121128           if  d3EW.§3EWfaa = *blank;
078500121128             d3EW.§3EWfaa = 'A';
078600121128             d3EW.§3EWdaa = %editc( wDate : 'X' );
078700121128             TBEuni  = d3EW;
078800121128             //_______________
078900121128             update  TNTBE000;
079000121128             //¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
079100121128           else;
079200121128             unlock  TNTBE01L;
079300121128           endif;
079400121024         else;
079500121024           unlock  TNTBE01L;
079600121008         endif;
079700120618
079800120618       ENDSR;
079900120612
080000120612       //--------------------------------------------------------------
080100120618       //?Apertura del file TABEL00F nel sistema informativo impostato.?
080200120612       //--------------------------------------------------------------
080300120612       BEGSR  sr_OpenFileTab;
080400120612
080500120612         // -?Chiusura (eventuale) archivio?
080600120612         if  %open(TABEL00F);
080700120612           close  TABEL00F;
080800120612         endif;
080900120612
081000120618         // -?Apertura archivio?
081100120618         ds_Libr   = $Libraries(w_iSystem);
081200120618         wLibFile1 = %trimr( $Libr(w_SisInf) ) + '/' + 'TABEL00F';
081300120612         open  TABEL00F;
081400120612
081500120612       ENDSR;
081600120618
081700120618       //--------------------------------------------------------------
081800120618       //?Apertura del file TNTBE00F nel sistema informativo impostato.?
081900120618       //--------------------------------------------------------------
082000120618       BEGSR  sr_OpenFileTbe;
082100120618
082200120618         // -?Chiusura (eventuale) archivio?
082300120618         if  %open(TNTBE01L);
082400120618           close  TNTBE01L;
082500120618         endif;
082600120618
082700120618         // -?Apertura archivio?
082800120618         ds_Libr   = $Libraries(w_iSystem);
082900120618         wLibFile2 = %trimr( $Libr(w_SisInf) ) + '/' + 'TNTBE01L';
083000120618         open  TNTBE01L;
083100120618
083200120618       ENDSR;
083300120612
083400120612       //--------------------------------------------------------------
083500120612       //?Operazioni finali.                                           ?
083600120612       //--------------------------------------------------------------
083700120612       BEGSR  sr_RoutEnd;
083800120612
083900120612         // -?Chiusura applicazioni?
084000120612         ubLeg3C_Close ();
084100120618         Selettore_Record_TIVSS ( 'SETONLR' );
084200120612
084300120612         // -?Restituzione parametri?
084400121008         kpjbu = TRTB28ds4;
084500120612
084600120612         // -?Uscita?
084700120612         return;
084800120612
084900120612       ENDSR;
085000120612
085100120612      /end-free
085200120612
085300120612       //--------------------------------------------------------------
085400120612       //?Schiere a tempo di compilazione.                             ?
085500120612       //--------------------------------------------------------------
085600120612
085700120612** -?$iSystem / $Libraries:?Sistemi AS/400 & Librerie con il file TABEL00F?
085800120612SETRAS  GAITRAGRU FILTRAGRU
085900120612AS888   GAITRAGRPSFILTRAGRPF
086000120612** -?$Msg:?Messaggi di Errore?-----------------------------------------------*
086100120612Sistema IBM non riconosciuto                                                   1
086200120612Sistema operativo non riconosciuto                                             2
086300120612Codice unificante attuale obbligatorio                                         3
086400121008Codice unificante attuale errato                                               4
086500121008Codice nuovo unificante obbligatorio                                           5
086600121008Codice nuovo unificante NON numerico                                           6
086700121008Codice nuovo unificante uguale a quello attuale                                7
086800121008Codice nuovo unificante errato                                                 8
086900121008Il nuovo unificante non risulta nella famiglia dell'attuale                    9
087000121008Il nuovo unificante è anch'esso figlio                                        10
087100121205Il nuovo unificante NON ha il "Supporto Cliente a BRT" = "ESWEB", ma "        11
087200121008Il nuovo unificante va PRIMA abilitato ad EasySped-WEB su STRATEGI            12
087300121008AVVISO: occorre disabilitare ad EasySped-WEB su STRATEGI l'unificante attuale 13
