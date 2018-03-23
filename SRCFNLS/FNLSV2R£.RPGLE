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
010900080807     d
011000080807     d  xpardut        s             30
011100080807     d  xparkut        s              1  0
011200080807     d  xparrag        s             48
011300080807     d  xparkcc        s              4  0
011400080807     d  xparsta        s              1  0
011500080807     d  xparflr        s             90
011600080807     d  xpardit        s              3
011700080807     d  xparnum        s              2  0
011800080807     d  xparkcm        s             80
011900080807     d  xparksm        s            140
012000080807     d  xparkdm        s             60
012100080807     d  xparesci       s              1
012200080807     d  xparerr        s              2
012300080807     d  xpariva        s             16
012400080807     d  xparcdf        s             16
012500080807      //---------------------------------------------------------------
012600080807      //?Definizione aree dati.
012700080807      //---------------------------------------------------------------
012800080807
012900080807      // - Dati utente
013000080807     d §AzUte        e ds                  extname(AZUTE00F)
013100080807     d                                     dtaara
013200080807     d §DatiUte      e ds                  extname(dDatiUte)
013300080807     d                                     dtaara
013400080807      //---------------------------------------------------------------
013500080807      //?Definizione procedure usate.
013600080807      //---------------------------------------------------------------
013700080807      /copy gaitrasrc/srcprotopr,XALFA3BR
013800080807      /copy gaitrasrc/srcprotopr,tibs34r
013900080807      /copy gaitrasrc/srcprotopr,trtb70r
014000080826      /copy gaitrasrc/srcprotopr,tibs02r
014100080827      /copy gaitrasrc/srcprotopr,xsrda8
014200111228      /copy gaitrasrc/srcprotopr,xemail
014300000000     C*---------------------------------------------------------------*
014400911010     C* INDICATORI
014500911010     C*---------------------------------------------------------------*
014600951115     C*  28    - EMETTE CAMPO MESSAGGI
014700040607     C*  40    - ERRORE
014800911010     C*  90    - ERRORE GENERICO
014900911010     C*---------------------------------------------------------------*
015000000000     C     *ENTRY        PLIST
015100000000     C                   PARM                    KPJBA
015200080807
015300080807      /free
015400080807
015500080807       // Operazioni iniziali
015600080807       exsr RoutInz;
015700080807
015800080807       // Gestione video
015900080807       DOW $Fine = *off;
016000080807         select;
016100080807
016200080807         // Videata di selezioni
016300080807           when $Video = 'D1';
016400080807             exsr GesD01;
016500080807
016600080807           other   ;
016700080807             $Fine = *on;
016800080807         endsl;
016900080807       ENDDO;
017000080807
017100080807       // Operazioni finali
017200080807       exsr RoutEnd;
017300080807
017400080807       //--------------------------------------------------------------
017500080807       //?Operazioni iniziali.
017600080807       //--------------------------------------------------------------
017700080807       BEGSR RoutInz;
017800080807       $inzD01=*on;
017900080807
018000080807       // Solo la prima volta
018100080807 1     if primavolta=' '   ;
018200080807
018300080807         // Reperimento dati job
018400080807         exsr DatiJob;
018500080807
018600080807         // Carico tutte le particolarità consegne che prevedono
018700080807         //  la restituzione della BAM
018800080807
018900080807         clear XX   ;
019000080807         setll (1:'7R')  tabel00f    ;
019100080807         reade (1:'7R')  tabel00f    ;
019200080807
019300080807         dow not %eof(tabel00f)  ;
019400080807
019500080807         if tblflg=' '   ;
019600080807           ds7r=tbluni   ;
019700080807           if §7r1bf='B' ;
019800080807            xx=xx+1   ;
019900080807            SKI7r(xx)=tblkey   ;
020000080807           endif     ;
020100080807         endif           ;
020200080807
020300080807         reade (1:'7R')  tabel00f    ;
020400080807         enddo     ;
020500080807 1       endif    ;
020600080807
020700080807         ENDSR;
020800080807
020900080807       //--------------------------------------------------------------
021000080807       //?Reperimento Dati del job (Utente/Operativi).
021100080807       //--------------------------------------------------------------
021200080807       BEGSR DatiJob;
021300080807
021400080807         in(E) §AzUte;
021500080807         if NOT %error;
021600080807           in(E) §DatiUte;
021700080807         endif;
021800080807         if %error or RSut = *blanks;
021900080807           clear TIBS34ds;
022000080807           tibs34r(tibs34ds);
022100080807           in §AzUte;
022200080807           in §DatiUte;
022300080807         endif;
022400080807
022500080807       ENDSR;
022600080807       //--------------------------------------------------------------
022700080807       //?Gestione video 1
022800080807       //--------------------------------------------------------------
022900080807       BEGSR GesD01;
023000080807
023100080807         // Inizializzazione videata
023200080807         if  $Inzd01 = *on;
023300080807            exsr InzD01;
023400080807            $Inzd01  = *off;
023500080807         endif;
023600080807
023700080807         // Emissione videata
023800080827         exfmt  lsv2d01;
023900080807
024000080807         reset errMessage;
024100080807         reset errGenerico;
024200080807         clear V1cmsg;
024300080807         errksc=*off     ;
024400080827         errinv=*off     ;
024500080827         errinvrd=*off     ;
024600080827         errinvra=*off     ;
024700080827         errdsd=*off     ;
024800080827         errdsa=*off     ;
024900080902         errlnp=*off     ;
025000110617         erremail=*off   ;
025100080807
025200080807 1       SELECT;
025300080807
025400080807         // - F3=Fine
025500080807 1         WHEN  dsp_aid = c_F03;
025600080807            $Fine = *on;
025700080807
025800080807 x1      // Invio
025900080807           OTHER;
026000080807             exsr ContrD01 ;
026100080807 2           if  errGenerico = *on;
026200080807               leavesr;
026300080807 2           endif;
026400080828
026500080828           // F6- Conferma
026600080828 1         if  dsp_aid = c_F06;
026700080828           exsr Conferma      ;
026800080828           $Inzd01 = *on;
026900080828           endif              ;
027000080807
027100080807 1       ENDSL;
027200080807
027300080807       ENDSR;
027400080807       //--------------------------------------------------------------
027500080807       //?controlli video1
027600080807       //--------------------------------------------------------------
027700080807       BEGSR ContrD01 ;
027800080807       select    ;
027900080807       when v1cksc=0 and v1dksc=*blanks               ;
028000080807           errksc=*on;
028100080807           v1cmsg = Msg(01);
028200080807           errMessage  = *on;
028300080807           errGenerico = *on;
028400080807           leavesr;
028500080807
028600080807       when v1cksc=0 and v1dksc<>*blanks   ;
028700080807       xparsta=9   ;
028800080807       xpardut=rsut   ;
028900080807       clear xparrag   ;
029000080807       xparrag  =v1dksc  ;
029100080807       xparkcc=dutkci    ;
029200080807       clear xparflr  ;
029300080807       clear xpardit  ;
029400080807       xparnum=1     ;
029500080807       clear xparkcm ;
029600080807       clear xparksm ;
029700080807       clear xparkdm ;
029800080807       clear xparesci;
029900080807       clear xparerr ;
030000080807       clear xpariva ;
030100080807       clear xparcdf ;
030200080807       xalfa3br (xpardut:xparkut:xparrag:xparkcc:xparsta:xparflr:xpardit
030300080807                 :xparnum:xparkcm:xparksm:xparkdm:xparesci:xparerr:xpariva
030400080807                 :xparcdf)             ;
030500080807       if xparsta<>-1  ;
030600080807       w007a=XPARKSM  ;
030700080807       V1CKSC=%int(W007A)  ;
030800080807       v1dksc=xparrag   ;
030900080807       else   ;
031000080807       clear v1dksc    ;
031100080807       endif    ;
031200080807       ErrGenerico=*on   ;
031300080807       leavesr   ;
031400080807
031500080807       other     ;
031600080807       chain   (1:dutkci:v1cksc)   cnaco000   ;
031700080807       if   not %found(cnaco00f) or acoflg<>' '    ;
031800080807           errksc=*on;
031900080807           v1cmsg = Msg(02);
032000080807           errMessage  = *on;
032100080807           errGenerico = *on;
032200080807           clear v1dksc    ;
032300080807           leavesr;
032400080807       endif   ;
032500080807
032600080807       v1dksc=acorag   ;
032700080807       ENDSL   ;
032800080807
032900080807       // Verifico se il cliente prevede la particolarità giusta
033000080807       clear ds7pqrs   ;
033100080807       ds7ksc=v1cksc   ;
033200080826       ds7ges='O'      ;
033300080807       ds7ric='S'      ;
033400080807       kpjbu=ds7pqrs   ;
033500080807       TRTB70R (KPJBA)   ;
033600080807       ds7pqrs=kpjbu     ;
033700080807
033800080807       xx=1   ;
033900080807       clear Indx    ;
034000080807       dow kscabi(xx)<>*blanks  and Indx=0;
034100080807       Indx=%lookup(kscabi(xx):SKI7R)  ;
034200080807       xx=xx+1  ;
034300080807       enddo    ;
034400080807
034500080807       if Indx=0   ;
034600080807           errksc=*on;
034700080807           v1cmsg = Msg(03);
034800080807           errMessage  = *on;
034900080807           errGenerico = *on;
035000080807           leavesr;
035100080807       endif       ;
035200080827
035300080902       // controllo la linea di partenza immessa
035400080902       clear v1dlnp    ;
035500080902       if v1clnp>0        ;
035600080902       chain  v1clnp   azorg01l   ;
035700080903        if not %found(azorg01l) or orgfva<>' ' or (orgfag<>'A' and
035800080903                                                   orgfag<>'F') ;
035900080902           errlnp=*on;
036000080902           v1cmsg = Msg(10);
036100080902           errMessage  = *on;
036200080902           errGenerico = *on;
036300080902           leavesr;
036400080902        endif   ;
036500080903        v1dlnp=orgdes    ;
036600080902       endif   ;
036700080902
036800080827       // immettere almeno una data
036900080827       if v1cinv=0 and v1cinvrd=0 and v1cinvra=0 and v1cdsd=0 and
037000080827          v1cdsa=0    ;
037100080827           errinv=*on;
037200080827           v1cmsg = Msg(04);
037300080827           errMessage  = *on;
037400080827           errGenerico = *on;
037500080827           leavesr;
037600080827       endif          ;
037700080828
037800080828       // Data stampa e data ristampa inventario non possono
037900080828       //   essere messe contemporaneamente
038000080828       if v1cinv>0 and (v1cinvRD>0 or v1cinvra>0)   ;
038100080828           errinv=*on;
038200080828           v1cmsg = Msg(08);
038300080828           errMessage  = *on;
038400080828           errGenerico = *on;
038500080828           leavesr;
038600080828       endif                      ;
038700080827
038800080827       // Data inventario
038900080827       clear cominv     ;
039000080827       if v1cinv>0      ;
039100080828         g02dat=v1cinv    ;
039200080828         EXSR ContrDATA ;
039300080828
039400080828         if errMessage=*on ;
039500080828           errinv=*on;
039600080828           leavesr;
039700080828          else               ;
039800080828          v1cinv=g02dat      ;
039900080828          cominv=g02inv      ;
040000080828          endif            ;
040100080827       endif            ;
040200080828
040300080828       // Data ristampa inventario  DAL
040400080828       clear cominvRD   ;
040500080828       if v1cinvRD>0      ;
040600080828         g02dat=v1cinvRD  ;
040700080828         EXSR ContrDATA ;
040800080828
040900080828         if errMessage=*on ;
041000080828           errinvRD=*on;
041100080828           leavesr;
041200080828          else               ;
041300080828          v1cinvRD=g02dat      ;
041400080828          cominvRD=g02inv      ;
041500080828          endif            ;
041600080828       endif            ;
041700080828
041800080828       // Data ristampa inventario  AL
041900080828       clear cominvRA   ;
042000080828       if v1cinvRA>0      ;
042100080828         g02dat=v1cinvRA  ;
042200080828         EXSR ContrDATA ;
042300080828
042400080828         if errMessage=*on ;
042500080828           errinvRA=*on;
042600080828           leavesr;
042700080828          else               ;
042800080828          v1cinvRA=g02dat      ;
042900080828          cominvRA=g02inv      ;
043000080828          endif            ;
043100080828
043200080828           // Data AL no < data DAL
043300080828          if cominvra<cominvrd    ;
043400080828           errinvra=*on;
043500080828           v1cmsg = Msg(07);
043600080828           errMessage  = *on;
043700080828           errGenerico = *on;
043800080828           leavesr;
043900080828           endif                   ;
044000080828       endif            ;
044100080828
044200080828       // Data spedizione  DAL
044300080828       clear comdsd     ;
044400080828       if v1cdsd  >0      ;
044500080828         g02dat=v1cdsd    ;
044600080828         EXSR ContrDATA ;
044700080828
044800080828         if errMessage=*on ;
044900080828           errdsd  =*on;
045000080828           leavesr;
045100080828          else               ;
045200080828          v1cdsd  =g02dat      ;
045300080828          comdsd  =g02inv      ;
045400080828          endif            ;
045500080828       endif            ;
045600080828
045700080828       // Data spedizione  AL
045800080828       clear comdsa     ;
045900080828       if v1cdsa>0      ;
046000080828         g02dat=v1cdsa    ;
046100080828         EXSR ContrDATA ;
046200080828
046300080828         if errMessage=*on ;
046400080828           errdsa  =*on;
046500080828           leavesr;
046600080828          else               ;
046700080828          v1cdsa  =g02dat      ;
046800080828          comdsa  =g02inv      ;
046900080828          endif            ;
047000080828
047100080828           // Data AL no < data DAL
047200080828          if comdsa  <comdsd      ;
047300080828           errdsa  =*on;
047400080828           v1cmsg = Msg(07);
047500080828           errMessage  = *on;
047600080828           errGenerico = *on;
047700080828           leavesr;
047800080828           endif                   ;
047900080828       endif            ;
048000080828
048100110805        // Se richiesto le non invetariate per data spedizione, il cliente
048200110805        //  deve essere abilitato dalla tabella DCLI
048300110805        // if v1cdsd>0 or v1cdsa>0 ;
048400110805        //  clear TIBS02ds;
048500110805        //  T02sif = knsif;
048600110805        //  T02cod = 'CLI';
048700110805        //  T02ke1 = %editc(v1cksc:'X') ;
048800110805        //  T02mod = 'C'  ;
048900110805        //  TNTBE_RicercaControllo  (kpjba : tibs02ds);
049000080828
049100110805 2      //  if t02err=' '  ;
049200110805        //  dcli=t02uni    ;
049300110805        //  else ;
049400110805        //  clear DCLI    ;
049500110805 2      //  endif   ;
049600080828
049700110805         // if §cliidt=' '   ;
049800110805         //   errdsd  =*on;
049900110805         //  v1cmsg = Msg(09);
050000110805         //  errMessage  = *on;
050100110805         //  errGenerico = *on;
050200110805         //  leavesr;
050300110805         // endif            ;
050400080828
050500110805       // endif    ;
050600110617       // Se richiesto file devo chiedere le non inventariate
050700110613       if v1csfile='S' and v1cdsd=0  ;
050800110613           errdsd  =*on;
050900110613           v1cmsg = Msg(11);
051000110613           errMessage  = *on;
051100110613           errGenerico = *on;
051200110613           leavesr;
051300110613       endif   ;
051400110617
051500110617       // Se file anche indirizzo e.mail e viceversa
051600110617       if (v1csfile=' ' and v1cemail<>*blanks)
051700110617          or (v1csfile<>' ' and v1cemail= *blanks)  ;
051800110617           erremail=*on;
051900110617           v1cmsg = Msg(12);
052000110617           errMessage  = *on;
052100110617           errGenerico = *on;
052200110617           leavesr;
052300110617       endif   ;
052400110613
052500111228       // controllo formale indirizzo email
052600111228       if v1cemail<>*blanks   ;
052700111228       clear dsemail  ;
052800120302       §EMLINDI=%trim(v1cemail)+'@brt.it'   ;
052900111228       callp XEMAIL   (dsemail)  ;
053000111228         if §EMLERRO<>*blanks and §EMLERRO<> '0';
053100111228             erremail=*on;
053200111228             v1cmsg = §emlmsgo   ;
053300111228             errMessage  = *on;
053400111228             errGenerico = *on;
053500111228             leavesr;
053600111228         endif   ;
053700111228       endif   ;
053800111228
053900080807       ENDSR   ;
054000080807       //--------------------------------------------------------------
054100080807       //?Inizializzazione video1
054200080807       //--------------------------------------------------------------
054300080807       BEGSR InzD01;
054400080807         clear v1cksc;
054500080807         clear v1dksc;
054600080827         clear v1cinv;
054700080827         clear v1cinvrd;
054800080827         clear v1cinvra;
054900080827         clear v1cdsd  ;
055000080827         clear v1cdsa  ;
055100080903         clear v1clnp  ;
055200080903         clear v1dlnp  ;
055300110617         clear v1csfile;
055400110617         clear v1cemail;
055500080807       ENDSR  ;
055600080828       //--------------------------------------------------------------
055700080828       //?controllo della Data
055800080828       //--------------------------------------------------------------
055900080828         BEGSR  CONTRData  ;
056000080828
056100080828         clear g02err     ;
056200080828         callp xsrda8 (wlbdat)  ;
056300080828          if g02err='1'      ;
056400080828           v1cmsg = Msg(05);
056500080828           errMessage  = *on;
056600080828           errGenerico = *on;
056700080828           leavesr;
056800080828          endif            ;
056900080828
057000080828        // Non deve essere > di oggi
057100080828        if g02inv>*date       ;
057200080828           v1cmsg = Msg(06);
057300080828           errMessage  = *on;
057400080828           errGenerico = *on;
057500080828           leavesr;
057600080828        endif                 ;
057700080828        ENDSR   ;
057800080807       //--------------------------------------------------------------
057900080807       //?Operazioni finali.
058000080807       //--------------------------------------------------------------
058100080807       BEGSR RoutEnd;
058200080807
058300080807         *inLR = *on;
058400080807         return;
058500080807
058600080807       ENDSR;
058700080807
058800080828       //--------------------------------------------------------------
058900080828       //?conferma elaborazione
059000080828       //--------------------------------------------------------------
059100080807      /end-free
059200080828     c     Conferma      BEGSR
059300080901     c                   if        SIricevuti>*zeros
059400080901     c                   eval      salvano=noricevuti
059500080902     c                   movel     *zeros        NOricevuti
059600080901     c
059700080828     c                   clear                   kpjbu
059800080828     c                   movel     fnlsv2ds      kpjbu
059900080828     c                   movel     'LSV3'        Kcoaz
060000080901     C                   CALL      'BCH10'
060100110616     C***                CALL      'FNLSV3R'
060200080828     C                   PARM                    KPJBA
060300080901     c
060400080901     c                   eval      NOricevuti=salvaNO
060500080901     c                   endif
060600080901
060700080901     c                   if        NOricevuti>*zeros
060800080901     c                   eval      salvaSI=SIricevuti
060900080902     c                   movel     *zeros        SIricevuti
061000080901     c
061100080901     c                   clear                   kpjbu
061200080901     c                   movel     fnlsv2ds      kpjbu
061300080901     c                   movel     'LSV3'        Kcoaz
061400110617     C                   CALL      'BCH10'
061500110617     C***                CALL      'FNLSV3R'
061600080901     C                   PARM                    KPJBA
061700080901     c
061800080901     c                   eval      SIricevuti=salvaSI
061900080901     c                   endif
062000080828     c
062100080828     c                   ENDSR
062200080807
062300000606**  MSG
062400080826Immettere il codice Cliente Mittente                                           1
062500080826Codice Cliente Mittente inesistente                                            2
062600080909Il cliente non ha una particolarità che prevede la restituzione dei documenti  3
062700080827Immettere almeno una data di stampa o ristampa
062800080827Data Errata
062900080827La data non deve essere maggiore di oggi
063000080828Data AL non può essere minore della data DAL
063100080828Impossibile effettuare contemporanemanete stampa e ristampa inventario
063200080828Per il cliente non è stata attivata la lista delle non inventariate:TEL al CED
063300110617Linea di partenza errata                                                      10
063400110617File richiedibile soltanto per le bolle non inventariate                      11
063500110617Se richiesto file immettere anche indirizzo e-mail e viceversa                12
