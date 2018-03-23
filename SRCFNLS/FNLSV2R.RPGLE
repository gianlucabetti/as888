000100080807     H DECEDIT('0,') DATEDIT(*ymd/)
000200080827     H* FNLSV2R *-----------------------------------------------------*
000300080909     H*   Controllo documenti da restituire al mittente
000400930126     H*---------------------------------------------------------------*
000500080827     FFNLsv2d   CF   E             WORKSTN indds(IndDspF)
000600080807     f                                     infds(InfDspF)
000700080319     Ftabel00f  IF   E           K DISK
000800080807     Fcnaco00f  IF   E           K DISK
000900080902     Fazorg01l  IF   E           K DISK
001000080319     d*-----------------
001100080807      // - Tasti funzionali a video
001200080807     d c_F01           c                   const(x'31')
001300080807     d c_F02           c                   const(x'32')
001400080807     d c_F03           c                   const(x'33')
001500080807     d c_F05           c                   const(x'35')
001600080807     d c_F06           c                   const(x'36')
001700080807     d c_F07           c                   const(x'37')
001800080807     d c_F08           c                   const(x'38')
001900080807     d c_F09           c                   const(x'39')
002000080807     d c_F10           c                   const(x'3A')
002100080807     d c_F12           c                   const(x'3C')
002200080807     d c_F13           c                   const(x'B1')
002300080807     d c_F14           c                   const(x'B2')
002400080807     d c_F15           c                   const(x'B3')
002500080807     d c_F16           c                   const(x'B4')
002600080807     d c_F17           c                   const(x'B5')
002700080807     d c_F18           c                   const(x'B6')
002800080807     d c_F19           c                   const(x'B7')
002900080807     d c_F20           c                   const(x'B8')
003000080807     d c_F21           c                   const(x'B9')
003100080807     d c_F22           c                   const(x'BA')
003200080807     d c_F23           c                   const(x'BB')
003300080807     d c_F24           c                   const(x'BC')
003400080807     d InfDspF         ds
003500080807     d  dsp_aid              369    369a
003600080807     d  sfl_rrn              376    377i 0
003700080807     d  min_nrr              378    379i 0
003800080807     d  num_rcds             380    381i 0
003900080807
004000080807      // - Indicatori su DspF
004100080807     d IndDspF         ds
004200080807        // - Indicatori di errore
004300080807     d  errMessage                    1n   overlay(IndDspF : 28)
004400080807     d  errGenerico                   1n   overlay(IndDspF : 99)
004500080807        // - Indicatori vari
004600080807     d  ErrKSC                        1n   overlay(IndDspF : 50)
004700080827     d  ErrINV                        1n   overlay(IndDspF : 51)
004800080827     d  ErrINVRD                      1n   overlay(IndDspF : 52)
004900080827     d  ErrINVRa                      1n   overlay(IndDspF : 53)
005000080827     d  ErrDSD                        1n   overlay(IndDspF : 54)
005100080827     d  ErrDSA                        1n   overlay(IndDspF : 55)
005200080902     d  ErrLNP                        1n   overlay(IndDspF : 56)
005300110617     d  Erremail                      1n   overlay(IndDspF : 57)
005400080807
005500080807     d*-----------------
005600080826     D MSG             S             78    DIM(14) CTDATA PERRCD(1)
005700080827     D SKI7R           S              2    DIM(50)
005800080807     d*-----------------
005900911010     D WLBDAT          DS
006000941221     D  G02DAT                 1      8  0
006100941221     D  G02INV                 9     16  0
006200941221     D  G02ERR                17     17
006300941221     D  G02TGI                18     22  0
006400080828
006500080828     D fnlsv2ds        DS
006600080828     D  v1cinv                 1      8  0
006700080828     D  cominv                 9     16  0
006800080828     D  v1cinvrd              17     24  0
006900080828     D  cominvrd              25     32  0
007000080828     D  v1cinvra              33     40  0
007100080828     D  cominvra              41     48  0
007200080901     D  SIRICEVUTI             1     48
007300080828     D  v1cdsd                49     56  0
007400080828     D  comdsd                57     64  0
007500080828     D  v1cdsa                65     72  0
007600080828     D  comdsa                73     80  0
007700080901     D  NORICEVUTI            49     80
007800080829     D  v1cksc                81     87  0
007900080829     D  v1dksc                88    112
008000080902     D  v1clnp               113    115  0
008100080902     D  v1dlnp               116    140
008200110610     D  v1csfile             141    141
008300110617     D  v1cemail             142    171
008400080319     d
008500040604     D KPJBA         E DS
008600080807     D tibs34ds      E DS
008700080319     D ds7N          E DS
008800111228     D dsemail       E DS
008900110805     D** dcli          E DS
009000080807     D ds7PQRS       E DS
009100080807     d kscabi                         2    DIM(30)  overlay(ds7cdp)
009200080319     d
009300080807     d DS7R          e ds
009400080826      // - Ricerca/Controllo tabelle
009500080826     d TIBS02ds      e ds                  inz
009600040614     D*
009700080319     d Dataiso         s               d   datfmt(*iso)
009800080826     d timeiso         s               t   timfmt(*iso)
009900080319     d Dataeur         s               d   datfmt(*eur)
010000080807     D W007A           S              7
010100080807     D $video          S              2    inz('D1')
010200080807     D $inzd01         S              1
010300080807     d $Fine           s               n   inz(*off)
010400080807     d Primavolta      s              1    inz
010500080807     D XX              S              3  0 inz
010600080807     D Indx            S              3  0 inz
010700080901     D SALVASI         S                   LIKE(SIRICEVUTI)
010800080901     D SALVANO         S                   LIKE(NORICEVUTI)
010900120424     d Diff            s              4  0
011000120424     d gg_speD         s                   like(g02tgi)
011100120424     d gg_speA         s                   like(g02tgi)
011200120424     d
011300080807     d
011400080807     d  xpardut        s             30
011500080807     d  xparkut        s              1  0
011600080807     d  xparrag        s             48
011700080807     d  xparkcc        s              4  0
011800080807     d  xparsta        s              1  0
011900080807     d  xparflr        s             90
012000080807     d  xpardit        s              3
012100080807     d  xparnum        s              2  0
012200080807     d  xparkcm        s             80
012300080807     d  xparksm        s            140
012400080807     d  xparkdm        s             60
012500080807     d  xparesci       s              1
012600080807     d  xparerr        s              2
012700080807     d  xpariva        s             16
012800080807     d  xparcdf        s             16
012900080807      //---------------------------------------------------------------
013000080807      //?Definizione aree dati.
013100080807      //---------------------------------------------------------------
013200080807
013300080807      // - Dati utente
013400080807     d §AzUte        e ds                  extname(AZUTE00F)
013500080807     d                                     dtaara
013600080807     d §DatiUte      e ds                  extname(dDatiUte)
013700080807     d                                     dtaara
013800080807      //---------------------------------------------------------------
013900080807      //?Definizione procedure usate.
014000080807      //---------------------------------------------------------------
014100080807      /copy gaitrasrc/srcprotopr,XALFA3BR
014200080807      /copy gaitrasrc/srcprotopr,tibs34r
014300080807      /copy gaitrasrc/srcprotopr,trtb70r
014400080826      /copy gaitrasrc/srcprotopr,tibs02r
014500080827      /copy gaitrasrc/srcprotopr,xsrda8
014600111228      /copy gaitrasrc/srcprotopr,xemail
014700000000     C*---------------------------------------------------------------*
014800911010     C* INDICATORI
014900911010     C*---------------------------------------------------------------*
015000951115     C*  28    - EMETTE CAMPO MESSAGGI
015100040607     C*  40    - ERRORE
015200911010     C*  90    - ERRORE GENERICO
015300911010     C*---------------------------------------------------------------*
015400000000     C     *ENTRY        PLIST
015500000000     C                   PARM                    KPJBA
015600080807
015700080807      /free
015800080807
015900080807       // Operazioni iniziali
016000080807       exsr RoutInz;
016100080807
016200080807       // Gestione video
016300080807       DOW $Fine = *off;
016400080807         select;
016500080807
016600080807         // Videata di selezioni
016700080807           when $Video = 'D1';
016800080807             exsr GesD01;
016900080807
017000080807           other   ;
017100080807             $Fine = *on;
017200080807         endsl;
017300080807       ENDDO;
017400080807
017500080807       // Operazioni finali
017600080807       exsr RoutEnd;
017700080807
017800080807       //--------------------------------------------------------------
017900080807       //?Operazioni iniziali.
018000080807       //--------------------------------------------------------------
018100080807       BEGSR RoutInz;
018200080807       $inzD01=*on;
018300080807
018400080807       // Solo la prima volta
018500080807 1     if primavolta=' '   ;
018600080807
018700080807         // Reperimento dati job
018800080807         exsr DatiJob;
018900080807
019000080807         // Carico tutte le particolarità consegne che prevedono
019100080807         //  la restituzione della BAM
019200080807
019300080807         clear XX   ;
019400080807         setll (1:'7R')  tabel00f    ;
019500080807         reade (1:'7R')  tabel00f    ;
019600080807
019700080807         dow not %eof(tabel00f)  ;
019800080807
019900080807         if tblflg=' '   ;
020000080807           ds7r=tbluni   ;
020100080807           if §7r1bf='B' ;
020200080807            xx=xx+1   ;
020300080807            SKI7r(xx)=tblkey   ;
020400080807           endif     ;
020500080807         endif           ;
020600080807
020700080807         reade (1:'7R')  tabel00f    ;
020800080807         enddo     ;
020900080807 1       endif    ;
021000080807
021100080807         ENDSR;
021200080807
021300080807       //--------------------------------------------------------------
021400080807       //?Reperimento Dati del job (Utente/Operativi).
021500080807       //--------------------------------------------------------------
021600080807       BEGSR DatiJob;
021700080807
021800080807         in(E) §AzUte;
021900080807         if NOT %error;
022000080807           in(E) §DatiUte;
022100080807         endif;
022200080807         if %error or RSut = *blanks;
022300080807           clear TIBS34ds;
022400080807           tibs34r(tibs34ds);
022500080807           in §AzUte;
022600080807           in §DatiUte;
022700080807         endif;
022800080807
022900080807       ENDSR;
023000080807       //--------------------------------------------------------------
023100080807       //?Gestione video 1
023200080807       //--------------------------------------------------------------
023300080807       BEGSR GesD01;
023400080807
023500080807         // Inizializzazione videata
023600080807         if  $Inzd01 = *on;
023700080807            exsr InzD01;
023800080807            $Inzd01  = *off;
023900080807         endif;
024000080807
024100080807         // Emissione videata
024200080827         exfmt  lsv2d01;
024300080807
024400080807         reset errMessage;
024500080807         reset errGenerico;
024600080807         clear V1cmsg;
024700080807         errksc=*off     ;
024800080827         errinv=*off     ;
024900080827         errinvrd=*off     ;
025000080827         errinvra=*off     ;
025100080827         errdsd=*off     ;
025200080827         errdsa=*off     ;
025300080902         errlnp=*off     ;
025400110617         erremail=*off   ;
025500080807
025600080807 1       SELECT;
025700080807
025800080807         // - F3=Fine
025900080807 1         WHEN  dsp_aid = c_F03;
026000080807            $Fine = *on;
026100080807
026200080807 x1      // Invio
026300080807           OTHER;
026400080807             exsr ContrD01 ;
026500080807 2           if  errGenerico = *on;
026600080807               leavesr;
026700080807 2           endif;
026800080828
026900080828           // F6- Conferma
027000080828 1         if  dsp_aid = c_F06;
027100080828           exsr Conferma      ;
027200080828           $Inzd01 = *on;
027300080828           endif              ;
027400080807
027500080807 1       ENDSL;
027600080807
027700080807       ENDSR;
027800080807       //--------------------------------------------------------------
027900080807       //?controlli video1
028000080807       //--------------------------------------------------------------
028100080807       BEGSR ContrD01 ;
028200080807       select    ;
028300080807       when v1cksc=0 and v1dksc=*blanks               ;
028400080807           errksc=*on;
028500080807           v1cmsg = Msg(01);
028600080807           errMessage  = *on;
028700080807           errGenerico = *on;
028800080807           leavesr;
028900080807
029000080807       when v1cksc=0 and v1dksc<>*blanks   ;
029100080807       xparsta=9   ;
029200080807       xpardut=rsut   ;
029300080807       clear xparrag   ;
029400080807       xparrag  =v1dksc  ;
029500080807       xparkcc=dutkci    ;
029600080807       clear xparflr  ;
029700080807       clear xpardit  ;
029800080807       xparnum=1     ;
029900080807       clear xparkcm ;
030000080807       clear xparksm ;
030100080807       clear xparkdm ;
030200080807       clear xparesci;
030300080807       clear xparerr ;
030400080807       clear xpariva ;
030500080807       clear xparcdf ;
030600080807       xalfa3br (xpardut:xparkut:xparrag:xparkcc:xparsta:xparflr:xpardit
030700080807                 :xparnum:xparkcm:xparksm:xparkdm:xparesci:xparerr:xpariva
030800080807                 :xparcdf)             ;
030900080807       if xparsta<>-1  ;
031000080807       w007a=XPARKSM  ;
031100080807       V1CKSC=%int(W007A)  ;
031200080807       v1dksc=xparrag   ;
031300080807       else   ;
031400080807       clear v1dksc    ;
031500080807       endif    ;
031600080807       ErrGenerico=*on   ;
031700080807       leavesr   ;
031800080807
031900080807       other     ;
032000080807       chain   (1:dutkci:v1cksc)   cnaco000   ;
032100080807       if   not %found(cnaco00f) or acoflg<>' '    ;
032200080807           errksc=*on;
032300080807           v1cmsg = Msg(02);
032400080807           errMessage  = *on;
032500080807           errGenerico = *on;
032600080807           clear v1dksc    ;
032700080807           leavesr;
032800080807       endif   ;
032900080807
033000080807       v1dksc=acorag   ;
033100080807       ENDSL   ;
033200080807
033300080807       // Verifico se il cliente prevede la particolarità giusta
033400080807       clear ds7pqrs   ;
033500080807       ds7ksc=v1cksc   ;
033600080826       ds7ges='O'      ;
033700080807       ds7ric='S'      ;
033800080807       kpjbu=ds7pqrs   ;
033900080807       TRTB70R (KPJBA)   ;
034000080807       ds7pqrs=kpjbu     ;
034100080807
034200080807       xx=1   ;
034300080807       clear Indx    ;
034400080807       dow kscabi(xx)<>*blanks  and Indx=0;
034500080807       Indx=%lookup(kscabi(xx):SKI7R)  ;
034600080807       xx=xx+1  ;
034700080807       enddo    ;
034800080807
034900080807       if Indx=0   ;
035000080807           errksc=*on;
035100080807           v1cmsg = Msg(03);
035200080807           errMessage  = *on;
035300080807           errGenerico = *on;
035400080807           leavesr;
035500080807       endif       ;
035600080827
035700080902       // controllo la linea di partenza immessa
035800080902       clear v1dlnp    ;
035900080902       if v1clnp>0        ;
036000080902       chain  v1clnp   azorg01l   ;
036100080903        if not %found(azorg01l) or orgfva<>' ' or (orgfag<>'A' and
036200080903                                                   orgfag<>'F') ;
036300080902           errlnp=*on;
036400080902           v1cmsg = Msg(10);
036500080902           errMessage  = *on;
036600080902           errGenerico = *on;
036700080902           leavesr;
036800080902        endif   ;
036900080903        v1dlnp=orgdes    ;
037000080902       endif   ;
037100080902
037200080827       // immettere almeno una data
037300080827       if v1cinv=0 and v1cinvrd=0 and v1cinvra=0 and v1cdsd=0 and
037400080827          v1cdsa=0    ;
037500080827           errinv=*on;
037600080827           v1cmsg = Msg(04);
037700080827           errMessage  = *on;
037800080827           errGenerico = *on;
037900080827           leavesr;
038000080827       endif          ;
038100080828
038200080828       // Data stampa e data ristampa inventario non possono
038300080828       //   essere messe contemporaneamente
038400080828       if v1cinv>0 and (v1cinvRD>0 or v1cinvra>0)   ;
038500080828           errinv=*on;
038600080828           v1cmsg = Msg(08);
038700080828           errMessage  = *on;
038800080828           errGenerico = *on;
038900080828           leavesr;
039000080828       endif                      ;
039100080827
039200080827       // Data inventario
039300080827       clear cominv     ;
039400080827       if v1cinv>0      ;
039500080828         g02dat=v1cinv    ;
039600080828         EXSR ContrDATA ;
039700080828
039800080828         if errMessage=*on ;
039900080828           errinv=*on;
040000080828           leavesr;
040100080828          else               ;
040200080828          v1cinv=g02dat      ;
040300080828          cominv=g02inv      ;
040400080828          endif            ;
040500080827       endif            ;
040600080828
040700080828       // Data ristampa inventario  DAL
040800080828       clear cominvRD   ;
040900080828       if v1cinvRD>0      ;
041000080828         g02dat=v1cinvRD  ;
041100080828         EXSR ContrDATA ;
041200080828
041300080828         if errMessage=*on ;
041400080828           errinvRD=*on;
041500080828           leavesr;
041600080828          else               ;
041700080828          v1cinvRD=g02dat      ;
041800080828          cominvRD=g02inv      ;
041900080828          endif            ;
042000080828       endif            ;
042100080828
042200080828       // Data ristampa inventario  AL
042300080828       clear cominvRA   ;
042400080828       if v1cinvRA>0      ;
042500080828         g02dat=v1cinvRA  ;
042600080828         EXSR ContrDATA ;
042700080828
042800080828         if errMessage=*on ;
042900080828           errinvRA=*on;
043000080828           leavesr;
043100080828          else               ;
043200080828          v1cinvRA=g02dat      ;
043300080828          cominvRA=g02inv      ;
043400080828          endif            ;
043500080828
043600080828           // Data AL no < data DAL
043700080828          if cominvra<cominvrd    ;
043800080828           errinvra=*on;
043900080828           v1cmsg = Msg(07);
044000080828           errMessage  = *on;
044100080828           errGenerico = *on;
044200080828           leavesr;
044300080828           endif                   ;
044400080828       endif            ;
044500080828
044600080828       // Data spedizione  DAL
044700080828       clear comdsd     ;
044800080828       if v1cdsd  >0      ;
044900080828         g02dat=v1cdsd    ;
045000080828         EXSR ContrDATA ;
045100080828
045200080828         if errMessage=*on ;
045300080828           errdsd  =*on;
045400080828           leavesr;
045500080828          else               ;
045600080828          v1cdsd  =g02dat      ;
045700080828          comdsd  =g02inv      ;
045800120424          gg_speD =g02tgi      ;
045900080828          endif            ;
046000080828       endif            ;
046100080828
046200080828       // Data spedizione  AL
046300080828       clear comdsa     ;
046400080828       if v1cdsa>0      ;
046500080828         g02dat=v1cdsa    ;
046600080828         EXSR ContrDATA ;
046700080828
046800080828         if errMessage=*on ;
046900080828           errdsa  =*on;
047000080828           leavesr;
047100080828          else               ;
047200080828          v1cdsa  =g02dat      ;
047300080828          comdsa  =g02inv      ;
047400120515          gg_speA =g02tgi      ;
047500080828          endif            ;
047600080828
047700080828           // Data AL no < data DAL
047800080828          if comdsa  <comdsd      ;
047900080828           errdsa  =*on;
048000080828           v1cmsg = Msg(07);
048100080828           errMessage  = *on;
048200080828           errGenerico = *on;
048300080828           leavesr;
048400080828           endif                   ;
048500120424
048600120424          // non si può elaborare più di un mese
048700120424          Diff=gg_spea - gg_sped  ;
048800120424 2        if Diff>45  ;
048900120424           errdsd  =*on;
049000120424           v1cmsg = Msg(13);
049100120424           errMessage  = *on;
049200120424           errGenerico = *on;
049300120424           leavesr;
049400120424 2        endif  ;
049500080828       endif            ;
049600080828
049700110805        // Se richiesto le non invetariate per data spedizione, il cliente
049800110805        //  deve essere abilitato dalla tabella DCLI
049900110805        // if v1cdsd>0 or v1cdsa>0 ;
050000110805        //  clear TIBS02ds;
050100110805        //  T02sif = knsif;
050200110805        //  T02cod = 'CLI';
050300110805        //  T02ke1 = %editc(v1cksc:'X') ;
050400110805        //  T02mod = 'C'  ;
050500110805        //  TNTBE_RicercaControllo  (kpjba : tibs02ds);
050600080828
050700110805 2      //  if t02err=' '  ;
050800110805        //  dcli=t02uni    ;
050900110805        //  else ;
051000110805        //  clear DCLI    ;
051100110805 2      //  endif   ;
051200080828
051300110805         // if §cliidt=' '   ;
051400110805         //   errdsd  =*on;
051500110805         //  v1cmsg = Msg(09);
051600110805         //  errMessage  = *on;
051700110805         //  errGenerico = *on;
051800110805         //  leavesr;
051900110805         // endif            ;
052000080828
052100110805       // endif    ;
052200110617       // Se richiesto file devo chiedere le non inventariate
052300110613       if v1csfile='S' and v1cdsd=0  ;
052400110613           errdsd  =*on;
052500110613           v1cmsg = Msg(11);
052600110613           errMessage  = *on;
052700110613           errGenerico = *on;
052800110613           leavesr;
052900110613       endif   ;
053000110617
053100110617       // Se file anche indirizzo e.mail e viceversa
053200110617       if (v1csfile=' ' and v1cemail<>*blanks)
053300110617          or (v1csfile<>' ' and v1cemail= *blanks)  ;
053400110617           erremail=*on;
053500110617           v1cmsg = Msg(12);
053600110617           errMessage  = *on;
053700110617           errGenerico = *on;
053800110617           leavesr;
053900110617       endif   ;
054000110613
054100111228       // controllo formale indirizzo email
054200111228       if v1cemail<>*blanks   ;
054300111228       clear dsemail  ;
054400120302       §EMLINDI=%trim(v1cemail)+'@brt.it'   ;
054500111228       callp XEMAIL   (dsemail)  ;
054600111228         if §EMLERRO<>*blanks and §EMLERRO<> '0';
054700111228             erremail=*on;
054800111228             v1cmsg = §emlmsgo   ;
054900111228             errMessage  = *on;
055000111228             errGenerico = *on;
055100111228             leavesr;
055200111228         endif   ;
055300111228       endif   ;
055400111228
055500080807       ENDSR   ;
055600080807       //--------------------------------------------------------------
055700080807       //?Inizializzazione video1
055800080807       //--------------------------------------------------------------
055900080807       BEGSR InzD01;
056000080807         clear v1cksc;
056100080807         clear v1dksc;
056200080827         clear v1cinv;
056300080827         clear v1cinvrd;
056400080827         clear v1cinvra;
056500080827         clear v1cdsd  ;
056600080827         clear v1cdsa  ;
056700080903         clear v1clnp  ;
056800080903         clear v1dlnp  ;
056900110617         clear v1csfile;
057000110617         clear v1cemail;
057100080807       ENDSR  ;
057200080828       //--------------------------------------------------------------
057300080828       //?controllo della Data
057400080828       //--------------------------------------------------------------
057500080828         BEGSR  CONTRData  ;
057600080828
057700080828         clear g02err     ;
057800080828         callp xsrda8 (wlbdat)  ;
057900080828          if g02err='1'      ;
058000080828           v1cmsg = Msg(05);
058100080828           errMessage  = *on;
058200080828           errGenerico = *on;
058300080828           leavesr;
058400080828          endif            ;
058500080828
058600080828        // Non deve essere > di oggi
058700080828        if g02inv>*date       ;
058800080828           v1cmsg = Msg(06);
058900080828           errMessage  = *on;
059000080828           errGenerico = *on;
059100080828           leavesr;
059200080828        endif                 ;
059300080828        ENDSR   ;
059400080807       //--------------------------------------------------------------
059500080807       //?Operazioni finali.
059600080807       //--------------------------------------------------------------
059700080807       BEGSR RoutEnd;
059800080807
059900080807         *inLR = *on;
060000080807         return;
060100080807
060200080807       ENDSR;
060300080807
060400080828       //--------------------------------------------------------------
060500080828       //?conferma elaborazione
060600080828       //--------------------------------------------------------------
060700080807      /end-free
060800080828     c     Conferma      BEGSR
060900080901     c                   if        SIricevuti>*zeros
061000080901     c                   eval      salvano=noricevuti
061100080902     c                   movel     *zeros        NOricevuti
061200080901     c
061300080828     c                   clear                   kpjbu
061400080828     c                   movel     fnlsv2ds      kpjbu
061500080828     c                   movel     'LSV3'        Kcoaz
061600120410     C                   CALL      'BCH10'
061700120410     C***                CALL      'FNLSV3R'
061800080828     C                   PARM                    KPJBA
061900080901     c
062000080901     c                   eval      NOricevuti=salvaNO
062100080901     c                   endif
062200080901
062300080901     c                   if        NOricevuti>*zeros
062400080901     c                   eval      salvaSI=SIricevuti
062500080902     c                   movel     *zeros        SIricevuti
062600080901     c
062700080901     c                   clear                   kpjbu
062800080901     c                   movel     fnlsv2ds      kpjbu
062900080901     c                   movel     'LSV3'        Kcoaz
063000110617     C                   CALL      'BCH10'
063100110617     C***                CALL      'FNLSV3R'
063200080901     C                   PARM                    KPJBA
063300080901     c
063400080901     c                   eval      SIricevuti=salvaSI
063500080901     c                   endif
063600080828     c
063700080828     c                   ENDSR
063800080807
063900000606**  MSG
064000080826Immettere il codice Cliente Mittente                                           1
064100080826Codice Cliente Mittente inesistente                                            2
064200080909Il cliente non ha una particolarità che prevede la restituzione dei documenti  3
064300080827Immettere almeno una data di stampa o ristampa
064400080827Data Errata
064500080827La data non deve essere maggiore di oggi
064600080828Data AL non può essere minore della data DAL
064700080828Impossibile effettuare contemporanemanete stampa e ristampa inventario
064800080828Per il cliente non è stata attivata la lista delle non inventariate:TEL al CED
064900110617Linea di partenza errata                                                      10
065000110617File richiedibile soltanto per le bolle non inventariate                      11
065100110617Se richiesto file immettere anche indirizzo e-mail e viceversa                12
065200120424Non è possibile richiedere più di un mese e mezzo                             13
