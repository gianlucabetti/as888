000100130723       //==============================================================
000200150518       //?Gestione File FIBSP00F - Abilitazione clienti e filiali alla ?
000300150518       //?                         sospensione  bolle
000400130723       //==============================================================
000500130723
000600130723       //--------------------------------------------------------------
000700130723       //?Parametri di compilazione (vedi *cmd UBCRTOBJ).              ?
000800130723       //--------------------------------------------------------------
000900130723
001000130723     /*PRM  dbgview(*source)
001100130723     /*END
001200130723
001300130723       //--------------------------------------------------------------
001400130723       //?Specifiche di controllo.                                     ?
001500130723       //--------------------------------------------------------------
001600130723
001700130723     h decedit('0,') datedit(*dmy/) option(*nodebugio)
001800130723     h dftactgrp(*no)
001900150519     h bnddir('TRUL')
002000130723
002100130723       //--------------------------------------------------------------
002200130723       //?Dichiarazione file.                                          ?
002300130723       //--------------------------------------------------------------
002400130723
002500130723       // -?Organigramma?
002600130723     fAZORG01L  if   e           k disk
002700130723
002800150518       // -?File clienti/filiali abilitati
002900150519     fFIBSP01L  Uf A e           k disk
003000150519     fFIBSP02L  Uf A e           k disk    rename(fibsp000:fibsp02)
003100150505
003200150505       // -?Anagrafica Aut
003300150505     fFIAPD01L  If   e           k disk
003400130723
003500130723       // -?Video?
003600150526     ffnlsBSPD  cf   e             workstn
003700130723     f                                     indds(IndDspF)
003800130723     f                                     infds(InfDspF)
003900130723
004000130723       //--------------------------------------------------------------
004100130723       //?Definizione costanti.                                        ?
004200130723       //--------------------------------------------------------------
004300130723
004400130723       // -?Costante per controllo "caratteri solo numerici"?
004500130723     d c_Digits        c                   const('0123456789')
004600131212
004700131212       // -?Costanti per la definizione delle schiere con i nomi?
004800130723
004900130723       // -?Tasti funzionali a video?
005000130723     d c_F01           c                   const(x'31')
005100130723     d c_F02           c                   const(x'32')
005200130723     d c_F03           c                   const(x'33')
005300130723     d c_F04           c                   const(x'34')
005400130723     d c_F05           c                   const(x'35')
005500130723     d c_F06           c                   const(x'36')
005600130723     d c_F07           c                   const(x'37')
005700130723     d c_F08           c                   const(x'38')
005800130723     d c_F09           c                   const(x'39')
005900130723     d c_F10           c                   const(x'3A')
006000130723     d c_F11           c                   const(x'3B')
006100130723     d c_F12           c                   const(x'3C')
006200130723     d c_F13           c                   const(x'B1')
006300130723     d c_F14           c                   const(x'B2')
006400130723     d c_F15           c                   const(x'B3')
006500130723     d c_F16           c                   const(x'B4')
006600130723     d c_F17           c                   const(x'B5')
006700130723     d c_F18           c                   const(x'B6')
006800130723     d c_F19           c                   const(x'B7')
006900130723     d c_F20           c                   const(x'B8')
007000130723     d c_F21           c                   const(x'B9')
007100130723     d c_F22           c                   const(x'BA')
007200130723     d c_F23           c                   const(x'BB')
007300130723     d c_F24           c                   const(x'BC')
007400130723     d c_Enter         c                   const(x'F1')
007500130723     d c_RollDown      c                   const(x'F4')
007600130723     d c_RollUp        c                   const(x'F5')
007700130723
007800130723       //--------------------------------------------------------------
007900130723       //?Definizione schiere.                                         ?
008000130723       //--------------------------------------------------------------
008100130723
008200130723       // -?Messaggi di errore?
008300151030     d $Msg            s             78    dim(19)  ctdata  perrcd( 1)
008400130723
008500130723       //--------------------------------------------------------------
008600130723       //?Definizione aree dati.                                       ?
008700130723       //--------------------------------------------------------------
008800130723
008900130723       // -?Dati utente?
009000130723     d §AzUte        e ds                  extname(AZUTE00F)
009100130723     d                                     dtaara
009200130723     d §DatiUte      e ds                  extname(dDatiUte)
009300130723     d                                     dtaara
009400130723
009500130723       //--------------------------------------------------------------
009600130723       //?Definizione strutture dati.                                  ?
009700130723       //--------------------------------------------------------------
009800130723
009900130723       // -?Status ds?
010000130723     d Status         sds
010100130723     d   SDSpgm          *proc
010200130723
010300130723       // -?InfDS?
010400130723     d InfDspF         ds
010500130723     d   dsp_aid             369    369a
010600130723
010700130723       // -?Indicatori su DspF?
010800130723     d IndDspF         ds                  inz
010900130723         // -?Abilitazione tasti funzionali?
011000130723     d   F5Attivo                      n   overlay(IndDspF : 05)
011100130723     d   F6Attivo                      n   overlay(IndDspF : 06)
011200151102     d   F7Attivo                      n   overlay(IndDspF : 07)
011300150526     d   F10Attivo                     n   overlay(IndDspF : 10)
011400130724     d   F12Attivo                     n   overlay(IndDspF : 12)
011500130723     d   F16Attivo                     n   overlay(IndDspF : 16)
011600150520         // -?Indicatori di gestione del subfile?
011700130723         // -?Emissione messaggio di errore?
011800130723     d   ErrMessage                    n   overlay(IndDspF : 28)
011900130723         // -?Indicatori per Attibuti di visualizzazione?
012000150526     d   $Ricercaksc                   n   overlay(IndDspF : 27)
012100151030        // - Indicatore di protezione campi
012200151030     d  Protlnpb                       n   overlay(IndDspF : 40)
012300130723         // -?Posizionamento cursore & segnalazione errore?
012400150518     d   PosCurLnp                     n   overlay(IndDspF : 51)
012500130723     d   PosCurKE2F                    n   overlay(IndDspF : 52)
012600150519     d   PosCurKsc                     n   overlay(IndDspF : 53)
012700130723     d   PosCurDES                     n   overlay(IndDspF : 54)
012800130723     d   PosCurSOD                     n   overlay(IndDspF : 55)
012900150504     d   PosCurLNPB                    n   overlay(IndDspF : 56)
013000150504     d   PosCurPDR                     n   overlay(IndDspF : 57)
013100150505     d   PosCurlp1                     n   overlay(IndDspF : 61)
013200150505     d   PosCurlp2                     n   overlay(IndDspF : 62)
013300150505     d   PosCurlp3                     n   overlay(IndDspF : 63)
013400150505     d   PosCurlp4                     n   overlay(IndDspF : 64)
013500150505     d   PosCurlp5                     n   overlay(IndDspF : 65)
013600150505     d   PosCurlp6                     n   overlay(IndDspF : 66)
013700150505     d   PosCurlp7                     n   overlay(IndDspF : 67)
013800150505     d   PosCurlp8                     n   overlay(IndDspF : 68)
013900150505     d   PosCurlp9                     n   overlay(IndDspF : 69)
014000150505     d   PosCurlp10                    n   overlay(IndDspF : 70)
014100150505     d   PosCurlp11                    n   overlay(IndDspF : 71)
014200150505     d   PosCurlp12                    n   overlay(IndDspF : 72)
014300150505     d   PosCurlp13                    n   overlay(IndDspF : 73)
014400150505     d   PosCurlp14                    n   overlay(IndDspF : 74)
014500150518     d   PosCurlp15                    n   overlay(IndDspF : 75)
014600150518     d   PosCurlp16                    n   overlay(IndDspF : 76)
014700150518     d   PosCurlp17                    n   overlay(IndDspF : 77)
014800150518     d   PosCurlp18                    n   overlay(IndDspF : 78)
014900150518     d   PosCurlp19                    n   overlay(IndDspF : 79)
015000150518     d   PosCurlp20                    n   overlay(IndDspF : 80)
015100150518     d   PosCurlp21                    n   overlay(IndDspF : 81)
015200150518     d   PosCurlp22                    n   overlay(IndDspF : 82)
015300150518     d   PosCurlp23                    n   overlay(IndDspF : 83)
015400150518     d   PosCurlp24                    n   overlay(IndDspF : 84)
015500150518     d   PosCurlp25                    n   overlay(IndDspF : 85)
015600150518     d   PosCurlp26                    n   overlay(IndDspF : 86)
015700150518     d   PosCurlp27                    n   overlay(IndDspF : 87)
015800150518     d   PosCurlp28                    n   overlay(IndDspF : 88)
015900150518     d   PosCurlp29                    n   overlay(IndDspF : 89)
016000150518     d   PosCurlp30                    n   overlay(IndDspF : 90)
016100130723         // -?Riemissione videata?
016200130723     d   ErrGenerico                   n   overlay(IndDspF : 99)
016300131212
016400150519     d campolpd02      ds            90
016500150504     d  v1clp1
016600150504     d  v1clp2
016700150504     d  v1clp3
016800150504     d  v1clp4
016900150504     d  v1clp5
017000150504     d  v1clp6
017100150504     d  v1clp7
017200150504     d  v1clp8
017300150504     d  v1clp9
017400150504     d  v1clp10
017500150504     d  v1clp11
017600150504     d  v1clp12
017700150504     d  v1clp13
017800150504     d  v1clp14
017900150518     d  v1clp15
018000150518     d  v1clp16
018100150518     d  v1clp17
018200150518     d  v1clp18
018300150518     d  v1clp19
018400150518     d  v1clp20
018500150518     d  v1clp21
018600150518     d  v1clp22
018700150518     d  v1clp23
018800150518     d  v1clp24
018900150518     d  v1clp25
019000150518     d  v1clp26
019100150518     d  v1clp27
019200150518     d  v1clp28
019300150518     d  v1clp29
019400150518     d  v1clp30
019500150518     d  slp                           3  0 dim(30) overlay(campolpd02)
019600130723
019700130723       // -?Parametri ricevuti?
019800130723     d KPJBA         e ds
019900130723
020000130723
020100130723       // -?Tabelle usate:?
020200130724
020300130724       // -?148ª descrizione dell'organigramma?
020400130724     d Og148         e ds                  inz  qualified
020500150908       // -?143ª descrizione dell'organigramma?
020600150908     d Og143         e ds                  inz
020700150520
020800150520     D fnlv55ds      E DS
020900150525     D fnlsbsp1ds    E DS
021000130723
021100130723       //--------------------------------------------------------------
021200130723       //?Definizione variabili globali.                               ?
021300130723       //--------------------------------------------------------------
021400130723
021500130723       // -?Flags booleani?
021600130723     d $Fine           s               n   inz
021700130723     d $InzD01         s               n   inz(*on)
021800130723     d $InzD01b        s               n   inz(*on)
021900130723     d $InzD02         s               n   inz(*on)
022000150519     d $InzD03         s               n   inz(*on)
022100130723     d $InzW01         s               n   inz(*on)
022200130723     d $Immissione     s               n   inz
022300130723     d $Protect        s               n   inz
022400130723
022500130723       // -?Variabili per la gestione del video?
022600130723     d $Video          s              2    inz('D1')
022700131212
022800131212       // -?Nome esteso Libreria/File del file tabella?
022900131212     d wLibFile        s             21a   inz
023000130723
023100130723       // -?Campi di comodo?
023200131212     d wDate           s              8  0 inz
023300130723     d wDate_EUR       s               d   datfmt(*eur)   inz(*loval)
023400160112     d datacur         s               d   datfmt(*iso)
023500150505     d w_numlnp        s              2  0 inz
023600150505     d wpdr            s                   like(apdpdr)
023700151030     d wntw            s                   like(§ogntw)
023800150505     d i               s              2  0 inz
023900150526     d iv              s              2  0 inz
024000150526     d ix              s              2  0 inz
024100150519     d w0030           s              3  0 inz
024200150519     d W0070           s              7  0 inz
024300150520     d s01nrr          s              4  0
024400150526     d wAbi            s                   like(UTEaut)  inz
024500151102     d wforzlnpb       s              1
024600151102     d wmsgSbm         s              1
024700151102     d*TotFil          s              3  0
024800151102     d*TotFilN         s              3  0
024900130723
025000130723       //--------------------------------------------------------------
025100130723       //?Definizione prototipi procedure.                             ?
025200130723       //--------------------------------------------------------------
025300130723
025400130723       // -?Reperimento dati utente?
025500130723     d TIBS34ds      e ds
025600130723      /copy gaitrasrc/srcProtoPR,TIBS34R
025700130724
025800130724       // -?Ricerca filiale in organigramma?
025900130724      /copy gaitrasrc/srcProtoPI,TNSD24R
026000130724      /copy gaitrasrc/srcProtoPR,TNSD24R
026100130724
026200130724       // -?Caricamento filiali gestite?
026300130724     d TRUL31ds      e ds                  inz
026400130724     d   $POg                 10    759    inz   dim(250)
026500130724      /copy gaitrasrc/srcProtoPr,TRUL31R
026600130724
026700130724       // -?Interrogazione sottoconti?
026800130724      /copy gaitrasrc/srcProtoPR,XALFA3BRDS
026900130724      /copy gaitrasrc/srcProtoPR,XALFA3BR
027000130723
027100130723       // -?Controllo/Decodifica cliente?
027200130723      /copy gaitrasrc/srcProtoPI,TIBS69R
027300130723      /copy gaitrasrc/srcProtoPR,TIBS69R
027400150520      // - Reperimento terminal di arrivo/partenza
027500150520     d fnlv55r         pr                  extpgm('FNLV55R')
027600150520     d  fnlv55ds                           likeds(FNLV55ds)
027700150525      // -
027800150525     d fnlsbsp1r       pr                  extpgm('FNLSBSP1R')
027900150525     d  kpjba                              likeds(kpjba)
028000151102      // - Sottomissione lavoro batch
028100151102     d bch10           pr                  extpgm('BCH10')
028200151102     d  kpjba                              likeds(KPJBA)
028300130723
028400130723       //--------------------------------------------------------------
028500130723       //?Definizione key-list.                                        ?
028600130723       //--------------------------------------------------------------
028700130723
028800130723       //--------------------------------------------------------------
028900130723       //?Riepilogo indicatori utilizzati.                             ?
029000130723       //--------------------------------------------------------------
029100130723       //--------------------------------------------------------------
029200130723
029300130723       //--------------------------------------------------------------
029400130723       //?M A I N - L I N E                                            ?
029500130723       //--------------------------------------------------------------
029600130723
029700130723     c     *Entry        plist
029800130723     c                   parm                    KPJBA
029900130723
030000130723      /free
030100151211         exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
030200130723
030300130723       // -?Operazioni iniziali?
030400130723       exsr  sr_RoutInz;
030500130723
030600130723       // -?Ciclo di gestione del file video?
030700130723       DOW  $Fine = *off;
030800130723
030900130723         select;
031000130723
031100130723           // -?Richiesta chiave di accesso?
031200130723           when $Video = 'D1';
031300130723             exsr  sr_GesD01;
031400150518
031500150518           when $Video = 'D2';
031600150518             exsr  sr_GesD02 ;
031700150518
031800150518
031900150518           when $Video = '3A';
032000150518             exsr  sr_GesD03a;
032100150518
032200150518           when $Video = '3B';
032300150518             exsr  sr_GesD03b;
032400130723
032500130723           // -?? ? ??
032600130723           other;
032700130723             $Fine = *on;
032800130723
032900130723         endsl;
033000130723
033100130723       ENDDO;
033200130723
033300130723       // -?Operazioni finali?
033400130723       exsr  sr_RoutEnd;
033500130723
033600130723       //--------------------------------------------------------------
033700130723       //?Operazioni iniziali.                                         ?
033800130723       //--------------------------------------------------------------
033900130723       BEGSR  sr_RoutInz;
034000130723
034100130723         *inLR = *on;
034200130723
034300130723         // -?Reperimento dati job?
034400130723         exsr  sr_DatiJob;
034500130723
034600130723         // -?Impostazione nome programma a video?
034700130723         V1Tpgm = SDSpgm;
034800131212
034900131212         // -?Reperimento data odierna?
035000131212         wDate = %int( %subst( %char( %dec( %timestamp() ) )
035100131212                               : 1 : 8 ) );
035200130723
035300130723       ENDSR;
035400130723
035500130723       //--------------------------------------------------------------
035600130723       //?Reperimento Dati del job (Utente/Operativi).                 ?
035700130723       //--------------------------------------------------------------
035800130723       BEGSR  sr_DatiJob;
035900130723
036000130723         in(E) §AzUte;
036100130723         if NOT %error;
036200130723           in(E) §DatiUte;
036300130723         endif;
036400130723         if %error or RSut = *blanks;
036500130723           clear TIBS34ds;
036600130723           tibs34r ( tibs34ds );
036700130723           in §AzUte;
036800130723           in §DatiUte;
036900130723         endif;
037000150526         select;
037100150526            when uteaut=*blanks;
037200150526            if   dutlpo= '1';
037300150526               wabi='TP';
037400150526            endif;
037500150526            if   dutlpo= '2' ;
037600150526               wabi='PO';
037700150526            endif;
037800150526            other;
037900150526               wabi=uteaut;
038000150526         endsl;
038100150526         clear  trul31ds;
038200150526         I31abi = wabi ;
038300150526         I31cdi = DUTdis ;
038400150526         I31car = DUTare ;
038500150526         I31cpo = DUTpou  ;
038600150526         TRUL31R   (KPJBA:trul31ds)   ;
038700130723
038800130723       ENDSR;
038900130723
039000130723       //--------------------------------------------------------------
039100130723       //?Gestione videata D01.                                        ?
039200130723       //--------------------------------------------------------------
039300130723       BEGSR  sr_GesD01;
039400130723
039500130723         // -?Inizializzazione videata?
039600130723         if  $InzD01 = *on;
039700150518           clear  TBBSPD01;
039800130723           $InzD01 = *off;
039900130723         endif;
040000130723         clear  V1Topz;
040100130724
040200130724         F12Attivo = *off;
040300151102         F7Attivo = *off;
040400150526         F10Attivo = *on;
040500130723
040600130723         // -?Emissione videata?
040700130723         write  TBBSPT01;
040800130723         write  TBBSPP01;
040900130723         exfmt  TBBSPD01;
041000130723
041100130723         clear  V1Dmsg;
041200130723         reset  ErrMessage;
041300130723         reset  ErrGenerico;
041400130723
041500130723         SELECT;
041600130723
041700130723           // -?F3=Fine?
041800130723           WHEN  dsp_aid = c_F03;
041900130723             $Fine = *on;
042000150526           // -?F10=Ricerca alfabetica
042100150526           WHEN  dsp_aid = c_F10;
042200150526             $ricercaksc=*on;
042300130723
042400130723           // -?Invio?
042500130723           OTHER;
042600130723             exsr  sr_CtrD01;
042700130723             if  ErrGenerico = *off;
042800150518                 $Video   = 'D2';
042900150518                 $InzD02  = *on;
043000130723             endif;
043100130723
043200130723         ENDSL;
043300130723
043400130723       ENDSR;
043500130723
043600130723       //--------------------------------------------------------------
043700130723       //?Inizializzazione videata D01.                                ?
043800130723       //--------------------------------------------------------------
043900130723       BEGSR  sr_InzD01;
044000130723
044100130723
044200130723
044300130723       ENDSR;
044400130723
044500130723       //--------------------------------------------------------------
044600130723       //?Controllo dati immessi nella videata D01.                    ?
044700130723       //--------------------------------------------------------------
044800130723       BEGSR  sr_CtrD01;
044900130723
045000151030         clear wntw;
045100151030         clear wforzlnpb;
045200130723         // -?Spegnimento indicatori di posizionamento cursore?
045300130723         %subst(IndDspF : 28) = *off;
045400150526         // -?Ricerca alfabetica codice cliente
045500150526         if (v1cksc=*zeros or v1cksc=*blanks)
045600150526                          and $ricercaksc=*on and v1dksc<>*blanks ;
045700150526             xParDut = RSut;
045800150526             xParKut = 2;
045900150526             xParRag = V1Dksc ;
046000150526             xParKcc = DUTkci;
046100150526             //xParSta = *zero;     //?Anche clienti bloccati o annullati?
046200150526             xParSta = 9;         //?Solo clienti attivi?
046300150526             //clear  xParFlr;      //?Filiali in cui cercare (max 30)?
046400150526             //xParFlr = V1Cke1;    //?Filiali in cui cercare (max 30)?
046500150526             xParFlr = O31pog;    //?Filiali in cui cercare (max 30)?
046600150526             clear  xParDit;
046700150526             xParNum = 1;
046800150526             clear  xParKcm;
046900150526             clear  xParKsm;
047000150526             clear  xParKdm;
047100150526             clear  xParEsci;
047200150526             clear  xParErr;
047300150526             clear  xParIva;
047400150526             clear  xParCdf;
047500150526             xAlfa3Br ( xParDut : xParKut : xParRag : xParKcc :
047600150526                        xParSta : xParFlr : xParDit : xParNum :
047700150526                        xParKcm : xParKsm : xParKdm : xParEsci :
047800150526                        xParErr : xParIva : xParCdf );
047900150526             if  xParSta <> -1   and   xParErr = *blank;
048000150526               V1Cksc  = xParKsm;
048100150526               V1Dksc  = xParRag;
048200150526               $Ricercaksc  = *off;
048300150526             endif;
048400150526             ErrGenerico = *on;
048500150526             leavesr;
048600150526         endif;
048700130723
048800150518         // -?Linea di Partenza OPPURE Codice Cliente
048900150518         if (v1clnp>*zeros and v1cksc>*zeros) or
049000150518            ((v1clnp=*zeros or v1clnp=*blanks) and
049100150518             (v1cksc=*zeros or v1cksc=*blanks) ) ;
049200150518            V1Dmsg = $Msg(01);
049300150518            PosCurLnp=*on  ;
049400150518            ErrMessage  = *on;
049500150518            ErrGenerico = *on;
049600150518            leavesr;
049700150518         endif;
049800150518         // -?Controlli Linea di Partenza
049900150518         if v1clnp<>*blanks and v1clnp<>*zeros ;
050000150518         // -?Ricerca?
050100150518            If  %scan( '?' : V1Clnp ) > *zero;
050200150525              clear fnlsbsp1ds;
050300150525              ibsptps='P';
050400150525              kpjbu=fnlsbsp1ds;
050500150525              fnlsbsp1r(kpjba);
050600150525              fnlsbsp1ds=kpjbu;
050700150525              eval v1clnp=%editc(obsplin:'X');
050800150518              ErrGenerico=*on;
050900150518              PosCurLnp=*on  ;
051000150518              leavesr;
051100150518            EndIf;
051200150518       // Deve essere numerico
051300150518           if    %check( c_Digits : v1clnp ) > *zero;
051400150518            V1Dmsg = $Msg(04);
051500150518             PosCurLnp = *on;
051600150518             ErrGenerico = *on;
051700150518             ErrMessage =  *on;
051800150518             leavesr;
051900150518           endif;
052000150518        // controlla esistenza del codice
052100150518           if v1clnp <>*zeros;
052200150518              chain  (%int(V1Clnp))  AZORG;
052300150518
052400150518              if  NOT %found(AZORG01L)  or  ORGfva <> *blank or orgfag='V';
052500150519                 V1dmsg = $Msg(04);
052600150518                 PosCurLnp = *on;
052700150518                 ErrGenerico = *on;
052800150518                 ErrMessage =  *on;
052900150518                 leavesr;
053000150518              endif;
053100150518
053200150518              v1dlnp  = ORGdes;
053300151030              og143   = orgde3;
053400151030              wntw    = §ogntw;
053500150518           endif;
053600150518         endif;
053700150518         // -?Controlli Codice Cliente
053800150518         if v1cksc<>*blanks and v1cksc<>*zeros ;
053900150518         // -?Ricerca?
054000150518            If  %scan( '?' : V1Cksc ) > *zero;
054100150525              clear fnlsbsp1ds;
054200150525              ibsptps='C';
054300150525              kpjbu=fnlsbsp1ds;
054400150525              fnlsbsp1r(kpjba);
054500150525              fnlsbsp1ds=kpjbu;
054600150525              eval v1cksc=%editc(obspcli:'X');
054700150518              ErrGenerico=*on;
054800150518              PosCurKsc=*on  ;
054900150518              leavesr;
055000150518            EndIf;
055100150518       // Deve essere numerico
055200150518           if    %check( c_Digits : v1cksc) > *zero;
055300150518            V1Dmsg = $Msg(06);
055400150518             PosCurKsc = *on;
055500150518             ErrGenerico = *on;
055600150518             ErrMessage =  *on;
055700150518             leavesr;
055800150518           endif;
055900150518        // controlla esistenza del codice
056000150518           if v1cksc <>*zeros;
056100150519              clear tibs69ds;
056200150518              clear DS_cnaco;
056300150518              clear DS_cnind;
056400150518              clear DS_cnclp;
056500150518              clear DS_fncls;
056600150518              I69kac=%int(v1cksc) ;
056700150518              tibs69r(tibs69ds:DS_cnaco:DS_cnind:DS_cnclp:DS_fncls);
056800150518              if o69err= *on    ;
056900150519                 V1dmsg = $Msg(06);
057000150518                 PosCurKsc = *on;
057100150518                 ErrGenerico = *on;
057200150518                 ErrMessage =  *on;
057300150518                 leavesr;
057400150518              endif;
057500150518
057600150518              v1dksc  = acorag;
057700150518           endif;
057800150518         endif;
057900130723
058000130723       ENDSR;
058100150518       //--------------------------------------------------------------
058200150518       //?Gestione videata D02                                 ?
058300150518       //--------------------------------------------------------------
058400150518       BEGSR  sr_GesD02;
058500150521
058600150521         F12Attivo = *on;
058700151102         F7Attivo = *on;
058800150526         F10Attivo = *off;
058900150521
059000150518         // -?Inizializzazione videata?
059100150518         if  $InzD02 = *on;
059200150519           reset  $Immissione;
059300150519         // -?Pulizia videata?
059400150518           clear  TBBSPD02;
059500150518           clear  V1Topz;
059600150518           if v1clnp>*zeros;
059700150519               v1cdes=v1dlnp;
059800150518               w0030=%int(v1clnp);
059900150518               chain  w0030 fibsp01l;
060000150518           else;
060100150518               w0070=%int(v1cksc);
060200150519               v1cdes=v1dksc;
060300150518               chain  w0070 fibsp02l;
060400150518           endif;
060500150518           if %found   ;
060600150518               v1topz='   MODIFICA    ';
060700150518               v1cdes=bspdes   ;
060800150518               V1CKLNPB=bspfpa   ;
060900150518           else;
061000150519               v1topz='  INSERIMENTO  ';
061100150519               $Immissione = *on;
061200160112               datacur=(%date());
061300160112               eval %subst(v1cdes:30:10)=
061400160112                                 %editw(%dec(datacur :*eur):'  /  /    ');
061500150518           endif;
061600151030         // Proteggo campo "Cliente con colli in part. da più Filiali"
061700151102         // e spengo indicatore di pos. cursore
061800151030           Protlnpb= *on;
061900151102           poscurlnpb= *off;
062000150518           $InzD02 = *off;
062100150518         endif;
062200150518         // -?Emissione videata?
062300150518         write  TBBSPT01;
062400150518         if v1clnp>*zeros ;
062500150518            write  TBBSPd01f;
062600150518         else;
062700150518            write  TBBSPd01C;
062800150518         endif;
062900150519         write  TBBSPP01;
063000150518         exfmt  TBBSPD02;
063100150518
063200150518         clear  V1Dmsg;
063300150518         reset  ErrMessage;
063400150518         reset  ErrGenerico;
063500150518
063600150518         SELECT;
063700150518
063800150518           // -?F3=Fine?
063900150518           WHEN  dsp_aid = c_F03;
064000150518             $Fine = *on;
064100150518           // -?F12=Ritorno
064200150518           WHEN  dsp_aid = c_F12;
064300150518             $video = 'D1';
064400151030           // -?F7 =Modifica --> sproteggo campo opzione "colli in part.da +fil"
064500151030           WHEN  dsp_aid = c_F07;
064600151030             protlnpb=*off;
064700151102             poscurlnpb= *on;
064800150518
064900150518           // -?Invio?
065000150518           OTHER;
065100150520              if v1cdes=*blanks;
065200150520                 v1cdes=bspdes;
065300150520              endif;
065400151030           // Erore forzabile
065500151030           // Se gestione per linea non DPD e inserito "S" nell'opzione
065600151030              if not *in40 and v1cklnpB='S' and  v1clnp>*zeros and wntw<>'DPD'
065700151030              and wforzlnpb=' ';
065800151030                 wforzlnpb='1';
065900151030                 poscurlnpb= *on ;
066000151030                 ErrMessage  = *on;
066100151030                 ErrGenerico = *on;
066200151030                 v1dmsg=$msg(19);
066300151030              endif;
066400151030              if ErrGenerico = *off;
066500151030                 if V1CKLNPB='S';
066600151030                    $Video   = '3B';
066700151030                 else;
066800151030                    $Video   = '3A';
066900151030                 endif;
067000150518                 $InzD03  = *on;
067100151030              endif;
067200150518
067300150518         ENDSL;
067400150518       endsr;
067500150519       //--------------------------------------------------------------
067600150519       //?Gestione videata D03A
067700150519       //--------------------------------------------------------------
067800150519       BEGSR  sr_GesD03A;
067900150521
068000150525         //f16attivo=*off;
068100150521
068200150519         // -?Inizializ.videata?
068300150519         if  $InzD03  = *on;
068400150519           clear  TBBSPD03A;
068500150525           f16attivo=*off;
068600150519           if $immissione = *off;
068700150521              f16attivo=*on;
068800150519              v1csod=BSPDCRP;
068900150519              v1caut=BSPaut ;
069000150520              if %check(c_digits:bsplin)>0 or v1clnp>*zeros;
069100150519                 campolpd02=*zeros;
069200150519              else;
069300150519                 campolpd02 = bsplin;
069400150519              endif;
069500150519           endif;
069600150519           $InzD03  = *off;
069700150519         endif;
069800150519         // -?Emissione videata?
069900150519         write  TBBSPT01;
070000150519         if v1clnp>*zeros;
070100150519            write TBBSPD01F;
070200150519         else;
070300150519            write TBBSPD01C;
070400150519         endif;
070500150519         write  TBBSPD02O;
070600150519         write  TBBSPP02 ;
070700150520         if v1clnp>*zeros;
070800150520            exfmt  TBBSPD03c;
070900150520         else;
071000150520            exfmt  TBBSPD03A;
071100150520         endif;
071200150519
071300150519         clear  V1Dmsg;
071400150519         reset  ErrMessage;
071500150519         reset  ErrGenerico;
071600150519
071700150519         SELECT;
071800150519
071900150519           // -?F3=Fine?
072000150519           WHEN  dsp_aid = c_F03;
072100150519             $Fine = *on;
072200150519           // -?F12=Ritorno
072300150519           WHEN  dsp_aid = c_F12;
072400150519             $video = 'D2';
072500150519
072600150519           // -?Invio?
072700150519           OTHER;
072800150519             exsr  sr_CtrD03A;
072900150519             if  ErrGenerico       ;
073000150519                leavesr;
073100150519             endif;
073200150519             if  dsp_aid = c_F06   or
073300150519                 dsp_aid = c_F16;
073400150519                 exsr  sr_scrivi;
073500150519                 $Video  = 'D1';
073600150519                 $InzD01 = *on;
073700150519             endif;
073800150519
073900150519         ENDSL;
074000150519       endsr;
074100150519       //--------------------------------------------------------------
074200150519       //?Gestione videata D03B  - Clienti con colli in part. da + fil
074300150519       //--------------------------------------------------------------
074400150519       BEGSR  sr_GesD03B;
074500150519         if  $InzD03  = *on;
074600150519           clear  TBBSPD03B;
074700150525           f16attivo=*off;
074800151102         //clear TotFil;
074900150519           if $immissione = *off;
075000150521              f16attivo=*on ;
075100150519              v1cpdr=BSPpdr ;
075200151214              v1caut=BSPaut ;
075300150519              if %check(c_digits:bsplin)>0;
075400150519                 campolpd02=*zeros;
075500150519              else;
075600150519                 campolpd02 = bsplin;
075700151102         // Memorizzo il numero delle filiali già presenti
075800151102         //      TotFil=%lookup(000:slp);
075900151102         //      if TotFil>0;
076000151102         //         TotFil-=1;
076100151102         //      endif;
076200150519              endif;
076300150519           endif;
076400150519           $InzD03  = *off;
076500150519         endif;
076600150519         // -?Emissione videata?
076700150519         write  TBBSPT01;
076800150519         if v1clnp>*zeros;
076900150519            write TBBSPD01F;
077000150519         else;
077100150519            write TBBSPD01C;
077200150519         endif;
077300150519         write  TBBSPD02O;
077400150519         write  TBBSPP02 ;
077500150519         exfmt  TBBSPD03B;
077600150519
077700150519         clear  V1Dmsg;
077800150519         reset  ErrMessage;
077900150519         reset  ErrGenerico;
078000150519
078100150519         SELECT;
078200150519
078300150519           // -?F3=Fine?
078400150519           WHEN  dsp_aid = c_F03;
078500150519             $Fine = *on;
078600150519           // -?F12=Ritorno
078700150519           WHEN  dsp_aid = c_F12;
078800150519             $video = 'D2';
078900150519
079000150519           // -?Invio?
079100150519           OTHER;
079200150519             exsr  sr_CtrD03B;
079300150519             if  ErrGenerico       ;
079400150519                leavesr;
079500150519             endif;
079600150519             if  dsp_aid = c_F06   or
079700150519                 dsp_aid = c_F16;
079800150519                 exsr  sr_Scrivi;
079900150519                 $Video  = 'D1';
080000150519                 $InzD01 = *on;
080100150519             endif;
080200150519
080300150519         ENDSL;
080400150519       endsr;
080500150519       //--------------------------------------------------------------
080600150519       //?Controllo dati videata    .                                  ?
080700150519       //--------------------------------------------------------------
080800150519       BEGSR  sr_CtrD03A;
080900150520
081000150521         // -?Spegnimento indicatori di posizionamento cursore?
081100150521         %subst(IndDspF : 28) = *off;
081200150521
081300150520       // Controllo dei terminal abilitati non vale in caso di inserimento
081400150520       // per linea di partenza. Quindi controllo solo in caso di gestione per
081500150520       // Codice cliente
081600150520
081700150520       IF V1cksc>*zeros;
081800150520       // terminal di partenza abilitati
081900150520                  clear w_numlnp;
082000150520            for i=1 to %elem(slp);
082100150520               if slp(i) >0;
082200150520                  reset fnlv55ds;
082300150520                  D55lin = slp(i) ;
082400150520                  d55drf = wdate  ;
082500150520                  d55tpt = 'P';
082600150520                  callp fnlv55r(fnlv55ds);
082700150520                  if d55err <> ' ';
082800150520                       V1Dmsg = $Msg(14);
082900150520                       %subst(IndDspF : i+60 : 1) = *on;
083000150520                       ErrMessage  = *on;
083100150520                       ErrGenerico = *on;
083200150520                       leavesr;
083300150520                  endif;
083400150520         //       Errore se ripetuto
083500150520                       if %lookup(slp(i):slp:1:i-1)>0;
083600150520                          V1Dmsg = $Msg(16);
083700150520                          %subst(IndDspF : i+60 : 1) = *on;
083800150520                          ErrMessage  = *on;
083900150520                          ErrGenerico = *on;
084000150520                          leavesr;
084100150520                       endif ;
084200150520                  w_numlnp+=1;
084300150520               endif;
084400150520            endfor;
084500150520         // - errore se non immesso almeno un tfp
084600150520              if w_numlnp=0 ;
084700150520                       V1Dmsg = $Msg(15);
084800150520                       PosCurLp1   = *on;
084900150520                       ErrMessage  = *on;
085000150520                       ErrGenerico = *on;
085100150520                       leavesr;
085200150520              endif;
085300150520       endif;
085400150519       ENDSR;
085500131212
085600150519       //--------------------------------------------------------------
085700150519       //?Controllo dati videata    .                                  ?
085800150519       //--------------------------------------------------------------
085900150519       BEGSR  sr_CtrD03B;
086000150521
086100150521         // -?Spegnimento indicatori di posizionamento cursore?
086200150521         %subst(IndDspF : 28) = *off;
086300150521
086400150521         // -?Controllo lnp abilitate ?
086500150521                  clear w_numlnp;
086600150521            for i=1 to %elem(slp);
086700150521               if slp(i) >0;
086800150521                  chain  slp(i)  AZORG;
086900150521                  if   not %found(azorg01l) or
087000150521                       (orgfag<>'F' and orgfag<>'A') or
087100150521                       orgfva<>*blanks;
087200150521                       V1Dmsg = $Msg(04);
087300150521                       %subst(IndDspF : i+60 : 1) = *on;
087400150521                       ErrMessage  = *on;
087500150521                       ErrGenerico = *on;
087600150521                       leavesr;
087700150521                  endif;
087800150521         //       Errore se lnp ripetuta
087900150521                       if %lookup(slp(i):slp:1:i-1)>0;
088000150521                          V1Dmsg = $Msg(13);
088100150521                          %subst(IndDspF : i+60 : 1) = *on;
088200150521                          ErrMessage  = *on;
088300150521                          ErrGenerico = *on;
088400150521                          leavesr;
088500150521                       endif ;
088600150521                  w_numlnp+=1;
088700150521               endif;
088800150521            endfor;
088900150521         // - errore se non immesse almeno due lnp
089000150521              if w_numlnp<=1;
089100150521                       V1Dmsg = $Msg(11);
089200150521                       PosCurLp1   = *on;
089300150521                       ErrMessage  = *on;
089400150521                       ErrGenerico = *on;
089500150521                       leavesr;
089600150521              endif;
089700150521         // - codice aut deve esistere per ogni lnp immessa
089800150908         //   Se record per lnp (no cod. cliente) il codice aut deve
089900150908         //   esistere per la "fil.valida come lnp bolla" corrispondente
090000150908         //   ad ogni lnp immessa
090100150521            for i=1 to %elem(slp);
090200150521               if slp(i) >0;
090300150908                if v1clnp>*zeros;
090400150908                  clear orgde3;
090500150908                  chain slp(i) azorg01l;
090600150908                  og143=orgde3  ;
090700150908                  if §oglnpb>*zeros ;
090800150908                     wpdr=%int(§oglnpb)*10000+v1cpdr;
090900150908                     chain ('A': wpdr) fiapd01l;
091000150908                     if not %found(fiapd01l) or apdfcm= *blanks;
091100150908                       V1Dmsg = $Msg(18);
091200150908                       v1dmsg = %trim(v1dmsg)+' '+ §oglnpb            ;
091300150908                       PosCurPDR   = *on;
091400150908                       ErrMessage  = *on;
091500150908                       ErrGenerico = *on;
091600150908                       leavesr;
091700150908                     endif;
091800150908                  else;
091900150908                       V1Dmsg = $Msg(17);
092000150908                       v1dmsg = %trim(v1dmsg)+' '+ %editc(slp(i):'X') ;
092100150908                       PosCurPDR   = *on;
092200150908                       ErrMessage  = *on;
092300150908                       ErrGenerico = *on;
092400150908                       leavesr;
092500150908                  endif;
092600150908                else;
092700150521                  wpdr=slp(i)*10000+v1cpdr;
092800150521                  chain ('A': wpdr) fiapd01l;
092900150521                  if not %found(fiapd01l) or apdfcm= *blanks;
093000150521                       V1Dmsg = $Msg(10);
093100150521                       v1dmsg = %trim(v1dmsg)+' '+ %editc(slp(i):'X') ;
093200150521                       PosCurPDR   = *on;
093300150521                       ErrMessage  = *on;
093400150521                       ErrGenerico = *on;
093500150521                       leavesr;
093600150521                  endif;
093700150908                endif;
093800150521               endif;
093900150521            endfor;
094000150519       ENDSR;
094100150519       //--------------------------------------------------------------
094200150519       //?Controllo dati videata    .                                  ?
094300150519       //--------------------------------------------------------------
094400150519       BEGSR  sr_scrivi ;
094500150519         // -?Aggiornamento FIBSP     ?
094600150519         SELECT;
094700150519
094800150519
094900150519           // -?F16=Annullamento?
095000150519           WHEN  dsp_aid = c_F16 ;
095100150519               //_______________
095200150519               if v1clnp>*zeros;
095300150519                  delete  fibsp000;
095400150519               else;
095500151211          // se presenti,
095600151211          // prima cancello tutti i record del cliente su FIBSP10F
095700151211                  w0070=%int(v1cksc);
095800151211                  exec sql delete from fibsp10f where bs1ksc=:w0070;
095900151211
096000150519                  delete  fibsp02 ;
096100150519               endif;
096200150519               //¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
096300150519
096400150519           // -?Immissione o Modifica?
096500150519           OTHER;
096600150520             exsr  sr_RieBsp   ;
096700150519             // -?Aggiornamento record?
096800150519             if  $immissione=*off;
096900150519               if v1clnp>*zeros;
097000150519                  //_____________
097100150519                  UPDATE fibsp000;
097200150519                  //¯¯¯¯¯¯¯¯¯¯¯¯¯
097300150519               else;
097400150519                  //_____________
097500150519                  UPDATE fibsp02 ;
097600150519                  //¯¯¯¯¯¯¯¯¯¯¯¯¯
097700150519               endif;
097800150519             else;
097900150519               if v1clnp>*zeros;
098000150519                  //_____________
098100150519                  WRITE  fibsp000;
098200150519                  //¯¯¯¯¯¯¯¯¯¯¯¯¯
098300150519               else;
098400150519                  //_____________
098500150519                  WRITE  fibsp02 ;
098600150519                  //¯¯¯¯¯¯¯¯¯¯¯¯¯
098700150519               endif;
098800150519             endif;
098900150519
099000150519         ENDSL;
099100150519
099200150519       ENDSR;
099300150519       //--------------------------------------------------------------
099400150519       //?                                                             ?
099500150519       //--------------------------------------------------------------
099600150519       BEGSR  sr_RieBsp ;
099700151102        clear wMsgSbm;
099800150520        if $immissione=*on;
099900150520           clear fibsp000;
100000150520           if v1clnp>*zeros;
100100150520              bspfil=%int(v1clnp);
100200150520           else;
100300150520              bspksc=%int(v1cksc);
100400150520           endif;
100500150520        endif;
100600150519        BSPDES=v1cdes   ;
100700150519        BSPFPA=v1cklnpb ;
100800150526       // Compatto l'elenco delle filiali inserite mettendo vicine quelle piene
100900150526        clear iv;
101000151102       //clear TotFilN;
101100150526        for i=1 to %elem(slp);
101200150526       // Primo elemento vuoto
101300150526           if slp(i)=0 and iv=0;
101400150526              iv=I;
101500150526           endif;
101600150526       // Primo elemento pieno dopo il vuoto
101700150526           if slp(i)>0 and iv>0;
101800150526       // Schifto verso sinisitra
101900150526              %subarr(slp:iv)=%subarr(slp:i);
102000150526       // Pulisco gli ultimi elementi in numero uguale al
102100150526       // numero di elementi che ho ricoperto perchè vuoti
102200150526              ix=%elem(slp)-(i-iv);
102300150526              %subarr(slp:ix+1)=*zeros;
102400150526              i=iv;
102500150526              iv=0;
102600150526           endif;
102700151102       // Memorizzo il totale delle filiali inserite
102800151102       //  if slp(i)>0;
102900151102       //     TotFilN=i;
103000151102       //  endif;
103100150526        endfor;
103200151102       // Se in partenza da più filiali verifico se aggiunte nuove linee
103300151102        if BSPFPA='S' and  bsplin<>campolpd02;
103400151102           eval wmsgSbm='S';
103500151102        endif;
103600150519        BSPLIN=campolpd02;
103700151214        BSPAUT  = v1caut ;
103800150519        if bspfpa='S';
103900150519           BSPPDR = v1cpdr;
104000151214       //    clear bspaut;
104100150521           clear bspdcrp;
104200150519        else;
104300150519           BSPDCRP =  v1csod;
104400151214       //  BSPAUT  = v1caut ;
104500150521           clear bsppdr;
104600150519        endif;
104700150519        BSPDUV  = wdate                ;
104800150519       endsr;
104900130723       //--------------------------------------------------------------
105000130723       //?Operazioni finali.                                           ?
105100130723       //--------------------------------------------------------------
105200130723       BEGSR  sr_RoutEnd;
105300130723
105400151102         // Se aggiunte nuove filiali emissione window di
105500151102         // richiesta per sottomissione duplica fisgn per
105600151102         // recuperare anche i record già presenti
105700151102         // Solo per EDP
105800151102         if   wmsgSbm='S' and %subst(knmus:1:3)='EDP';
105900151102             $fine=*off;
106000151102             dow $fine=*off;
106100151102                exfmt tbbspw01;
106200151102                if  dsp_aid = c_F06;
106300151102                   if w01sbm='S';
106400151102                      kcoaz='LBSP';
106500151102                      BCH10(kpjba);
106600151102                   endif;
106700151102                      $fine=*on;
106800151102                endif ;
106900151102             enddo;
107000151102         endif;
107100130723         // -?Chiusura funzioni precedentemente aperte?
107200130723
107300130723         reset  TIBS69ds;
107400130723         tibs69r( tibs69ds :
107500130723                  ds_cnaco : ds_cnind : ds_cnclp : ds_fncls );
107600130723
107700130723         //  ?Uscita?
107800130723         return;
107900130723
108000130723       ENDSR;
108100130723
108200130723      /end-free
108300130723
108400130723       //--------------------------------------------------------------
108500130723       //?Definizione schiere a tempo di compilazione.                 ?
108600130723       //--------------------------------------------------------------
108700130723** -?$Msg:?Messaggi di Errore?-----------------------------------------------*
108800150521Immettere la Linea di Partenza OPPURE il Codice Cliente
108900130723Ammessi solo caratteri numerici
109000130723Filiale obbligatoria
109100130726Filiale errata
109200130723Cliente obbligatorio
109300130723Cliente errato
109400130726Descrizione obbligatoria
109500130723Valore non ammesso nel campo: immettere " " o "S"
109600150504Scelta incompatibile con la richiesta di non gestire nella Sospensione automatic
109700150908Codice Aut. inesistente o non valido per la linea di part.
109800150505Inserire almeno due "Filiali abilitate"
109900150505Richiesta valida solo se Cliente con colli i partenza da più Filiali='S'
110000150505Filiale già immessa
110100150520Terminal di Partenza errato
110200150520Immettere almeno un Terminal di Partenza
110300150520Terminal di Partenza già presente
110400150908Manca indicazione "Fil. valida come LnP Bolla" su organigramma per l.part.
110500150908Codice Aut. inesistente o non valido per la "Fil. valida come LnP Bolla"
110600151030Richiesta partenza da più filiali per Network non DPD. ENTER per Forzare
