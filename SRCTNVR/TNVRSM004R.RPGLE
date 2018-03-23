000100090415       //--------------------------------------------------------------
000200090415       //?TNVRSM004R - Sistemazione ordinante in FNORM da TIORI        ?
000300090415       //--------------------------------------------------------------
000400090415
000500090415     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000600090415
000700090415       //--------------------------------------------------------------
000800090415       //?Dichiarazione file.                                          ?
000900090415       //--------------------------------------------------------------
001000090415
001100090416       // - File ORM di partenza
001200090415     fTIORI02L  if   e           k disk
001300090415     fFNACR01L  if   e           k disk
001400090415
001500090416       // - File ORM da controllare ed eventualmente aggiornare
001600090415     fFNORM01L  Uf   e           k disk
001700090416
001800090416       // - Videata di partenza
001900090416     fTNVRSM004Dcf   e             workstn
002000090416     f                                     indds(IndDspF)
002100090416     f                                     infds(InfDspF)
002200090415
002300090415     fQSYSPRT   o    f  132        printer oflind(*inOF)
002400090415
002500090415       //--------------------------------------------------------------
002600090415       //?Definizione costanti.                                        ?
002700090415       //--------------------------------------------------------------
002800090415
002900090416       // - Tasti funzionali a video
003000090416     d c_F01           c                   const(x'31')
003100090416     d c_F02           c                   const(x'32')
003200090416     d c_F03           c                   const(x'33')
003300090416     d c_F05           c                   const(x'35')
003400090416     d c_F06           c                   const(x'36')
003500090416     d c_F07           c                   const(x'37')
003600090416     d c_F08           c                   const(x'38')
003700090416     d c_F09           c                   const(x'39')
003800090416     d c_F10           c                   const(x'3A')
003900090416     d c_F12           c                   const(x'3C')
004000090416     d c_F13           c                   const(x'B1')
004100090416     d c_F14           c                   const(x'B2')
004200090416     d c_F15           c                   const(x'B3')
004300090416     d c_F16           c                   const(x'B4')
004400090416     d c_F17           c                   const(x'B5')
004500090416     d c_F18           c                   const(x'B6')
004600090416     d c_F19           c                   const(x'B7')
004700090416     d c_F20           c                   const(x'B8')
004800090416     d c_F21           c                   const(x'B9')
004900090416     d c_F22           c                   const(x'BA')
005000090416     d c_F23           c                   const(x'BB')
005100090416     d c_F24           c                   const(x'BC')
005200090416     d c_Enter         c                   const(x'F1')
005300090416     d c_RollDown      c                   const(x'F4')
005400090416     d c_RollUp        c                   const(x'F5')
005500090415
005600090415       //--------------------------------------------------------------
005700090415       //?Definizione schiere.                                         ?
005800090415       //--------------------------------------------------------------
005900090415
006000090416       // - ID Clienti da controllare
006100090416     d $IdC            s              8    dim(13)  ctdata  perrcd(1)
006200090415
006300090415       // - Messaggi di errore
006400090415     d $Msg            s             78    dim( 5)  ctdata  perrcd(1)
006500090415
006600090415       //--------------------------------------------------------------
006700090415       //?Definizione aree dati.                                       ?
006800090415       //--------------------------------------------------------------
006900090415
007000090415       // - Dati utente
007100090415     d §AzUte        e ds                  extname(AZUTE00F)
007200090415     d                                     dtaara
007300090415     d §DatiUte      e ds                  extname(dDatiUte)
007400090415     d                                     dtaara
007500090415
007600090415       //--------------------------------------------------------------
007700090415       //?Definizione strutture dati.                                  ?
007800090415       //--------------------------------------------------------------
007900090415
008000090415       // - Status
008100090415     d Status         sds
008200090415     d   SDSpgm          *proc
008300090416
008400090416       // - InfDS
008500090416     d InfDspF         ds
008600090416     d   dsp_aid             369    369a
008700090416
008800090416       // - Indicatori su DspF
008900090416     d IndDspF         ds                  inz
009000090416       //   · Emissione messaggio di errore
009100090416     d   ErrMessage                    n   overlay(IndDspF : 28)
009200090416       //   · Riemissione videata
009300090416     d   ErrGenerico                   n   overlay(IndDspF : 99)
009400090416
009500090416       // - Strutture di comodo
009600090416     d ds_VidIdCli     ds                  inz
009700090416     d   VidIC1                            inz
009800090416     d   VidIC2                            inz
009900090416     d   VidIC3                            inz
010000090416     d   VidIC4                            inz
010100090416     d   VidIC5                            inz
010200090416     d   VidIC6                            inz
010300090416     d   VidIC7                            inz
010400090416     d   VidIC8                            inz
010500090416     d   VidIC9                            inz
010600090416     d   VidICA                            inz
010700090416     d   VidICB                            inz
010800090416     d   VidICC                            inz
010900090416     d   VidICD                            inz
011000090416     d   VidICE                            inz
011100090416     d   VidICF                            inz
011200090416     d  $IdCli                 1    120    inz  dim(15)
011300090416
011400090415       // - Parametri ricevuti
011500090415     d KPJBA         e ds
011600090415
011700090415       // - Reperimento dati utente
011800090415     d TIBS34ds      e ds
011900090415
012000090415       //--------------------------------------------------------------
012100090415       //?Definizione variabili globali.                               ?
012200090415       //--------------------------------------------------------------
012300090415
012400090415       // - Flags booleani
012500090415     d $Upd            s               n   inz
012600090415
012700090415       // - Indici di schiera
012800090415     d xx              s              3  0 inz
012900090415     d xe              s              3  0 inz
013000090415
013100090415       // - Campi di comodo
013200090415     d wTime           s              6  0 inz
013300090415     d wMsg            s             80    inz
013400090415
013500090415       //--------------------------------------------------------------
013600090415       //?Definizione procedure usate.                                 ?
013700090415       //--------------------------------------------------------------
013800090415
013900090415       // - Reperimento dati utente
014000090415      /copy gaitrasrc/srcProtoPR,TIBS34R
014100090415
014200090415       //--------------------------------------------------------------
014300090415       //?Definizione key-list.                                        ?
014400090415       //--------------------------------------------------------------
014500090415
014600090415       // - file TIORI02L
014700090415     d k03tiori02    e ds                  extname(TIORI02L : *key)
014800090415     d                                     inz     prefix(k_)
014900090415
015000090415       // - file FNORM01L
015100090415     d k04fnorm01    e ds                  extname(FNORM01L : *key)
015200090415     d                                     inz     prefix(k_)
015300090415
015400090415       // - file FNACR01L
015500090415     d k01fnacr01    e ds                  extname(FNACR01L : *key)
015600090415     d                                     inz     prefix(k_)
015700090415
015800090415       //--------------------------------------------------------------
015900090415       //?Riepilogo indicatori usati.                                  ?
016000090415       //--------------------------------------------------------------
016100090416       // OF - OverFlow
016200090415       //--------------------------------------------------------------
016300090415
016400090415       //--------------------------------------------------------------
016500090415       //?M A I N - L I N E.                                           ?
016600090415       //--------------------------------------------------------------
016700090415
016800090415     c     *Entry        plist
016900090415     c                   parm                    KPJBA
017000090415
017100090415      /free
017200090415
017300090415       //?Operazioni iniziali?
017400090415       exsr  sr_RoutInz;
017500090415
017600090415       //?Ciclo di lettura file ORM da clienti via internet (TIORI)?
017700090416       FOR  xx = 1  TO  %elem($IdCli);
017800090416
017900090416         if  $IdCli(xx) <= *zero;
018000090416           iter;
018100090416         endif;
018200090415
018300090416         k_ORIidc = $IdCli(xx);
018400090415         clear  k_ORIder;
018500090415         clear  k_ORIoer;
018600090415         setll  %kds(k03tiori02)  TIORI000;
018700090415         reade  %kds(k03tiori02 : 1)  TIORI000;
018800090415
018900090415         DoW  NOT %eof(TIORI02L);
019000090415
019100090415           // - Elaborazione singolo record di TIORI
019200090415           if  VAOatb = *blank;
019300090415             exsr  sr_Elab;
019400090415           endif;
019500090415
019600090415           // - Lettura record successivo di TIORI
019700090415           reade  %kds(k03tiori02 : 1)  TIORI000;
019800090415
019900090415         EndDo;
020000090415
020100090415       ENDFOR;
020200090415
020300090415       //?Operazioni finali?
020400090415       exsr  sr_RoutEnd;
020500090415
020600090415       //--------------------------------------------------------------
020700090415       //?Operazioni iniziali.                                         ?
020800090415       //--------------------------------------------------------------
020900090415       BEGSR  sr_RoutInz;
021000090415
021100090415         *inLR = *on;
021200090415
021300090415         // - Reperimento dati job
021400090415         exsr  sr_DatiJob;
021500090416
021600090416         // Gestione filtro di lancio
021700090416         exsr  sr_GesW01;
021800090415
021900090415       ENDSR;
022000090416
022100090416       //--------------------------------------------------------------
022200090416       //?Gestione filtro di lancio.                                   ?
022300090416       //--------------------------------------------------------------
022400090416       BEGSR  sr_GesW01;
022500090416
022600090416         // - Inizializzazione videata
022700090416         T1Cpgm = SDSpgm;
022800090416         VIDlst = 'S';
022900090416         $IdCli = $IdC;
023000090416
023100090416         // - Emissione window per richiedere l'aggiornamento di FNORM
023200090416         DoU  dsp_aid <> c_Enter;
023300090416           write  TNVRSM4W;
023400090416           exfmt  TNVRSM4D;
023500090416           clear  ErrMessage;
023600090416           clear  ErrGenerico;
023700090416           clear  V1Dmsg;
023800090416         EndDo;
023900090416
024000090416         // - F3 = Fine
024100090416         if  dsp_aid = c_F03;
024200090416           exsr  sr_RoutEnd;
024300090416         endif;
024400090416
024500090416         // - F6 = Conferma
024600090416         $Upd = (VIDlst = *blank);
024700090416
024800090416         // - Reperimento ora corrente
024900090416         wTime = %dec( %time() );
025000090416
025100090416         // - Stampa 1ª testata
025200090416         except  PRTtxt;
025300090416         *inOF = *off;
025400090416
025500090416       ENDSR;
025600090415
025700090415       //--------------------------------------------------------------
025800090415       //?Reperimento Dati del job (Utente/Operativi).                 ?
025900090415       //--------------------------------------------------------------
026000090415       BEGSR  sr_DatiJob;
026100090415
026200090415         in(E) §AzUte;
026300090415         if NOT %error;
026400090415           in(E) §DatiUte;
026500090415         endif;
026600090415         if %error or RSut = *blanks;
026700090415           clear TIBS34ds;
026800090415           tibs34r(tibs34ds);
026900090415           in §AzUte;
027000090415           in §DatiUte;
027100090415         endif;
027200090415
027300090415       ENDSR;
027400090415
027500090415       //--------------------------------------------------------------
027600090415       //?Elaborazione singolo record del file TIORI                   ?
027700090415       //--------------------------------------------------------------
027800090415       BEGSR  sr_Elab;
027900090415
028000090415         // -?Errore: codice ordinante mancante su TIORI?
028100090415         if  VAOcor = *zero;
028200090415           xe = 1;
028300090415           exsr  sr_PrtErr;
028400090415           leavesr;
028500090415         endif;
028600090415
028700090415         // -?Aggancio Ordine Ritiro Merce abbinato?
028800090415         k_ORMpoe = ORIpoe;
028900090415         k_ORMnsr = ORInsr;
029000090415         k_ORMnor = ORInor;
029100090415         k_ORMnrv = ORInrv;
029200090415         chain  %kds(k04fnorm01)  FNORM000;
029300090415
029400090415
029500090415         SELECT;
029600090415
029700090415           // -?Errore: non trovato O.R.M.?
029800090415           when  Not %found(FNORM01L);
029900090415             xe = 2;
030000090415             exsr  sr_PrtErr;
030100090415             leavesr;
030200090415           when  ORMatb <> *blank;
030300090415             unlock  FNORM01L;
030400090416             xe = 3;
030500090415             exsr  sr_PrtErr;
030600090415             leavesr;
030700090415
030800090415           // -?Errore: codice ordinante mancante su FNORM?
030900090415           when  ORMcor = *zero;
031000090415             if  $Upd = *on;
031100090415               exsr  sr_UpdORM;
031200090415             else;
031300090415               unlock  FNORM01L;
031400090416               xe = 4;
031500090416               exsr  sr_PrtErr;
031600090416             endif;
031700090415             leavesr;
031800090415
031900090415           // -?Errore: codice ordinante diverso (TIORI/FNORM)?
032000090415           when  VAOcor <> ORMcor;
032100090415             unlock  FNORM01L;
032200090415             xe = 5;
032300090415             exsr  sr_PrtErr;
032400090415             leavesr;
032500090415
032600090415           other;
032700090415             unlock  FNORM01L;
032800090415             if  Not $Upd;
032900090415               xe = *zero;
033000090415               exsr  sr_PrtErr;
033100090415               leavesr;
033200090415             endif;
033300090415
033400090415         endsl;
033500090415
033600090415       ENDSR;
033700090415
033800090415       //--------------------------------------------------------------
033900090415       //?Aggiornamento ordinante in FNORM                             ?
034000090415       //--------------------------------------------------------------
034100090415       BEGSR  sr_UpdORM;
034200090415
034300090415         // -?Reperimento dati anagrafici da FNACR?
034400090415         k_ACRcro = VAOcor;
034500090415         chain  %kds(k01fnacr01)  FNACR000;
034600090415
034700090415         // -?Aggiornamento ordinante in FNORM?
034800090415         ORMcor = VAOcor;
034900090415         if  %found(FNACR01L)   and   ACRatb = *blank;
035000090415           ORMrso = ACRrsr;
035100090415           ORMino = ACRinr;
035200090415           ORMcao = ACRcar;
035300090415           ORMloo = ACRlor;
035400090415           ORMpro = ACRprr;
035500090415           ORMnao = ACRnar;
035600090415         endif;
035700090415         //_______________
035800090415         update  FNORM000;
035900090415         //¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
036000090415
036100090415       ENDSR;
036200090415
036300090415       //--------------------------------------------------------------
036400090415       //?Stampa errore e/o aggiornamebnto da fare                     ?
036500090415       //--------------------------------------------------------------
036600090415       BEGSR  sr_PrtErr;
036700090415
036800090415         if  xe = *zero;
036900090416           wMsg = '=> ORDINANTE valorizzato e corrispondente';
037000090415         else;
037100090415           wMsg = $Msg(xe);
037200090416           if  xe = 5;
037300090416             wMsg = %trimr(wMsg) + ' ' + %editc(ORMcor : 'X');
037400090416           endif;
037500090415         endif;
037600090415
037700090415         if  *inOF;
037800090415           except  PRTtxt;
037900090415           *inOF = *off;
038000090415         endif;
038100090415
038200090415         except  PRTdet;
038300090415
038400090415       ENDSR;
038500090415
038600090415       //--------------------------------------------------------------
038700090415       //?Operazioni finali.                                           ?
038800090415       //--------------------------------------------------------------
038900090415       BEGSR  sr_RoutEnd;
039000090415
039100090415         // - Stampa "fine lista"
039200090416         if  dsp_aid <> c_F03;
039300090416           except  PrtEND;
039400090416         endif;
039500090415
039600090415         return;
039700090415
039800090415       ENDSR;
039900090415
040000090415      /end-free
040100090415
040200090415       //--------------------------------------------------------------
040300090415       //?Specifiche di output.                                        ?
040400090415       //--------------------------------------------------------------
040500090415
040600090415     oQSYSPRT   e            PrtTXT            2
040700090415     o                       RsUt
040800090415     o                                        +   6 'LISTA DISCORDANZE-
040900090415     o                                               RILEVATE TRA TIOR-
041000090416     o                                              I E FNORM'
041100090415     o                       SDSpgm           +   6
041200090415     o                       *date         y  +   3
041300090415     o          e            PrtTXT      0
041400090415     o                                        +  26 'LISTA DISCORDANZE-
041500090415     o                                               RILEVATE TRA TIOR-
041600090416     o                                              I E FNORM'
041700090415     o          e            PrtTXT      1
041800090415     o                       KNSIF
041900090415     o                       KNMUS            +   1
042000090416     o                                        +   5 '------------------
042100090415     o                                              -------------------
042200090416     o                                              ---------'
042300090415     o                                        +   6 'Pag.'
042400090415     o                       Page          z  +   0
042500090415     o                       wTime            +   5 '  :  :  '
042600090415     o          e            PrtTXT      2
042700090415     o                                              'ID cliente'
042800090415     o                                        +   3 'Riferimento    '
042900090415     o                                        +   3 'O.R.M.           '
043000090415     o                                        +   3 'Messaggio        -
043100090415     o                                                                '
043200090415     o          e            PrtTXT      1  1
043300090415     o                                              '=========='
043400090415     o                                        +   3 '==============='
043500090415     o                                        +   3 '================='
043600090415     o                                        +   3 '=================-
043700090415     o                                              =================='
043800090415     o          e            PrtDET      1
043900090415     o                       ORIidc
044000090415     o                       VAOrfa           +   5
044100090415     o                       ORIpoe           +   3
044200090415     o                       ORInsr        z  +   1
044300090415     o                       ORInor        z  +   1
044400090415     o                       ORInrv        z  +   1
044500090415     o                       wMsg             +   3
044600090415     o          e            PrtEND      2
044700090416     o                                        +  26 '***  Fine  Lista -
044800090415     o                                               ***'
044900090415     o          e            PrtEND      0
045000090416     o                                        +  26 '***  Fine  Lista -
045100090415     o                                               ***'
045200090415
045300090415       //--------------------------------------------------------------
045400090415       //?Definizione schiere a tempo di compilazione.                 ?
045500090415       //--------------------------------------------------------------
045600090415
045700090416** - $IdC
04580009041500480934
04590009041500593812
04600009041500622269
04610009041500821240
04620009041500990637
04630009041501161538
04640009041502101096
04650009041502494193
04660009041502751097
04670009041502751098
04680009041502771235
04690009041502801066
04700009041502801067
047100090415** - $Msg
047200090415Errore: codice ordinante mancante su TIORI                                     1
047300090415Errore: non trovato O.R.M. in FNORM                                            2
047400090415Errore: O.R.M. annullato in FNORM                                              3
047500090416Errore: codice ordinante mancante su FNORM => da impostare                     4
047600090416Errore: codice ordinante diverso su FNORM:                                     5
