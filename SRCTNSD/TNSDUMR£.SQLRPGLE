000100080530      *PARMS DFTACTGRP(*NO) ACTGRP(QILE) OPTION(*NOXREF) DATEDIT(*yMd.)
000200080206      //--------------------------------------------------------------
000300100901      //?TNSDUMR - Stampa inVII/rientri  GIORNALIERI UNITÀ EDP  - esegue -
000400080206      //--------------------------------------------------------------
000500080206
000600080206     h decedit('0,') datedit(*ymd/) option(*nodebugio)
000700080206
000800080206      //---------------------------------------------------------------
000900080206      //?Dichiarazione file.
001000080206      //---------------------------------------------------------------
001100100902     FUNana02L  iF   E           K DISK
001200100312     FAZORG21l  IF   E           K DISK
001300100312     fazorg01l  if   e           k disk
001400100312     fkpprf01l  if   e           k disk
001500100826     fcnaco00f  if   e           k disk
001600100902     FTNSDUMP   O    E             PRINTER oflind(*in90)
001700100917     FFNLV22P   O    F  132        PRINTER usropn
001800080206      //---------------------------------------------------------------
001900080206      //?Definizione schiere.
002000080206      //---------------------------------------------------------------
002100100917     d OVRP            s             91    dim(1) ctdata perrcd(1)
002200080206      //---------------------------------------------------------------
002300080206      //?Definizione aree dati.
002400080206      //---------------------------------------------------------------
002500080206      // - Dati utente
002600080206     d §AzUte        e ds                  extname(AZUTE00F)
002700080206     d                                     dtaara
002800080206     d §DatiUte      e ds                  extname(dDatiUte)
002900080206     d                                     dtaara
003000080206
003100080206      //---------------------------------------------------------------
003200080206      //?Definizione strutture dati.
003300080206      //---------------------------------------------------------------
003400080206
003500080206      // - Parametri ricevuti
003600080206     d KPJBA         e ds
003700080206
003800080206      // - Reperimento dati utente
003900080206     d TIBS34ds      e ds
004000080206     d dUte01        e ds
004100080702     d DLAT          e ds
004200100927     d og143         e ds
004300080702
004400100901     d tnsdulds      e ds
004500100917     d dsetiunit     e ds
004600100902     d ungiods       e ds                  extname(ungio00f)
004700080206
004800080206      // - Ricerca/Controllo tabelle
004900080206     d TIBS02ds      e ds                  inz
005000100316
005100100316     d wlbdat          ds                  inz
005200100316     d  g02dat                 1      8  0
005300100316     d  g02inv                 9     16  0
005400100316     d  g02err                17     17
005500100316     d  g02tgi                18     22  0
005600100826
005700100827      // ricerca filiale su organigramma
005800100827     d   pInFIL        S              3
005900100827     d   pInFAG        S              1
006000100827     d   pInDES        S             25
006100100827     d   pInDIT        S              3
006200100316
006300080206      //---------------------------------------------------------------
006400080206      //?Definizione variabili globali.
006500080206      //---------------------------------------------------------------
006600080206
006700080206      // - Flags booleani
006800080208     d $Fine           s               n   inz(*off)
006900100902     d ErrGenerico     s               n   inz(*off)
007000080206
007100100901     d Indx            s              3  0 inz
007200100901     d w1cdtconf       s              8  0 inz
007300100831     d w1cdtm          s              8  0 inz
007400100316     d wazorg          s              1    Inz(' ')
007500100316     d wmag            s              3
007600100316     d wdmag           s             30
007700100708     d wtip            s              2
007800100708     d wdtip           s             30
007900100827     d Datapiu10       s              8  0
008000100901     d Dataoggi        s              8  0
008100100917     d comman          s            100
008200100917     d lung            s             15  5
008300100920     d dmag25          s             25
008400101011     d PosM            s              2  0
008500101011     d LungM           s              2  0
008600080414
008700100901     d wrkgetlista     s           4096    varying
008800100901
008900100315     d Dataeur         s               d   datfmt(*eur)
009000100315     d Dataiso         s               d   datfmt(*iso)
009100080605     d Dataymd         s               d   datfmt(*ymd)
009200080605     d Datadmy         s               d   datfmt(*dmy)
009300100901     d Datasys         s               d   inz(*sys)
009400080604     d
009500080605     d                 DS
009600080605     d  AA                     1      4  0
009700080605     d  MM                     5      6  0
009800080605     d  GG                     7      8  0
009900080605     d DATA                    1      8  0
010000081009     d
010100080206      //---------------------------------------------------------------
010200080206      //?Definizione procedure usate.
010300080206      //---------------------------------------------------------------
010400100901      /copy gaitrasrc/srcprotopr,tibs02r
010500080414      /copy gaitrasrc/srcprotopr,tibs34r
010600100316      /copy gaitrasrc/srcprotopr,xsrda8
010700100917      /copy gaitrasrc/srcprotopr,qcmdexc
010800100917      /copy gaitrasrc/srcprotopr,fnlvuezs
010900080206
011000080206      //---------------------------------------------------------------
011100080206      //?Riepilogo indicatori.
011200080206      //---------------------------------------------------------------
011300080207      // - Comuni
011400080207      // 28    : Emissione messaggio di errore a video
011500080207      // - C01/S01
011600080207      // 30    : Condiziona SFLDSP    (*not)
011700080207      // 31    : Condiziona SFLDSPCTL (*not)
011800080207      // 30+31 : Condiziona SFLCLR
011900100824      // 32    : Condiziona SFLNXTCHG
012000080207      // 50    : Errore: Opzione errata
012100080207      // - D01
012200080207      // - Comuni
012300080207      // 99    : Generico di Errore
012400080206      //---------------------------------------------------------------
012500080206
012600080206      //---------------------------------------------------------------
012700080206      //?M A I N - L I N E
012800080206      //---------------------------------------------------------------
012900080206
013000080627     c     *Entry        plist
013100080206     c                   parm                    KPJBA
013200100901     c                   movel     kpjbu         tnsdulds
013300080702
013400080206      /free
013500080206
013600080206       // Operazioni iniziali
013700080206       exsr RoutInz;
013800100917       if $Fine=*off ;
013900081215
014000100901       // Prepara SQL con le selezioni previste a filtro
014100100901       exsr PrepaSQL    ;
014200080206
014300100902       // Lettura record selezionati
014400100902       exsr LeggiSQL    ;
014500100917       endif  ;
014600100902
014700080206       // Operazioni finali
014800080206       exsr RoutEnd;
014900080206
015000080206       //--------------------------------------------------------------
015100080206       //?Operazioni iniziali.
015200080206       //--------------------------------------------------------------
015300080206       BEGSR RoutInz;
015400100902       *in90  =*on;
015500080206
015600080206         // Reperimento dati job
015700100902         exsr DatiJob;
015800100826
015900100902       // Udate
016000100826       dataiso=datasys ;
016100100831       dataeur=dataiso ;
016200100831       Dataoggi=%dec(dataeur) ;
016300100902
016400100902       // PREPARA TESTAta con selezioni
016500100902       if §sdulcconf='S'   ;
016600100902        t1dconf='ancora da confermare alla data'   ;
016700100902        dataiso=%date(§sduldtm) ;
016800100902        dataeur=dataiso ;
016900100902        t1cdtm  =%dec(dataeur) ;
017000100902       else   ;
017100100902        t1dconf='      già confermati in  data'   ;
017200100902        dataiso=%date(§sduldtcon) ;
017300100902        dataeur=dataiso ;
017400100902        t1cdtm  =%dec(dataeur) ;
017500100902       endif  ;
017600100902
017700100902       // Ufficio
017800140124       select ;
017900140124       when §sdulcuf='MAG'          ;
018000101008         t1dcuf=' MAG=Mag. C.E.D. ' ;
018100140124       when §sdulcuf='TLC'          ;
018200140124         t1dcuf=' TLC=Mag. T.L.C. ' ;
018300140124       other ;
018400101008         t1dcuf='FOR=Mag. ACQUISTI' ;
018500140124       endsl  ;
018600101005
018700101005       // Ordinamento
018800101005       if §sdulord='D'  ;
018900101005       t1dord='Data / Ora IMMISSIONE'  ;
019000101005       *in10=*on  ;
019100101005       else  ;
019200101005       t1dord='Magazzino DESTINAZIONE, tipo Unità';
019300101005       *in11=*on  ;
019400101005       endif ;
019500100902
019600100902       // Tipo invio /rientro
019700100902       select  ;
019800100902       when §sdulinvr='I'   ;
019900100902        t1dinvr=' I=Invii '  ;
020000100902       when §sdulinvr='R'  ;
020100100902        t1dinvr='R=Rientro da fornitore'  ;
020200100902       when §sdulinvr='F'  ;
020300100902        t1dinvr='F=Rientro da filiale  '  ;
020400101013       when §sdulinvr='S'  ;
020500101013        t1dinvr='S=Movimenti da Sede  '  ;
020600100902        endsl  ;
020700100902
020800100902       // Magazzino destinazione
020900100902       if §sdulmag>0  ;
021000100902        t1cmag=§sdulmag  ;
021100100902        wazorg='E'     ;
021200100902        wmag=%editc(t1cmag:'X')  ;
021300100902        exsr  chkmag   ;
021400100902        t1dmag=wdmag  ;
021500100902       endif  ;
021600100902
021700100902       // Magazzino correlato
021800100902       if §sdulmgc>0  ;
021900100902        t1cmgc=§sdulmgc  ;
022000100902        wazorg='E'   ;
022100100902        wmag=%editc(t1cmgc:'X')  ;
022200100902        exsr  chkmag   ;
022300100902        t1dmgc=wdmag  ;
022400100902       endif   ;
022500100902
022600100902       t1cute=§sdulute  ;
022700100917
022800100917       // data / ora immissione
022900100917       if §sduldim>0  ;
023000100917        dataiso=%date(§sduldim) ;
023100100917        dataeur=dataiso ;
023200100917        t1cdim  =%dec(dataeur) ;
023300100917        t1chim=§sdulhim  ;
023400100917       endif  ;
023500100917
023600100917       // Se stampa etichette    eseguo ovrprtf fnlv22p e apro il file
023700100917       if §sdultips='E'   ;
023800100917         comman=ovrp(1)  ;
023900100917         lung=91   ;
024000100917         %subst(comman:28:10)=§sdulpre  ;
024100100917
024200100917         callp(e)  QCMDEXC (comman:lung)   ;
024300100917         if not %error  ;
024400100917          open fnlv22p ;
024500100917         else  ;
024600100917          $Fine=*on  ;
024700100917         endif  ;
024800100917
024900100917       endif  ;
025000100917
025100080627         ENDSR;
025200080206
025300080206       //--------------------------------------------------------------
025400080206       //?Reperimento Dati del job (Utente/Operativi).
025500080206       //--------------------------------------------------------------
025600080206       BEGSR DatiJob;
025700080206
025800080206         in(E) §AzUte;
025900080206         if NOT %error;
026000080206           in(E) §DatiUte;
026100080206         endif;
026200080206         if %error or RSut = *blanks;
026300080206           clear TIBS34ds;
026400080206           tibs34r(tibs34ds);
026500080206           in §AzUte;
026600080206           in §DatiUte;
026700080206         endif;
026800080206
026900080206       ENDSR;
027000100901       //--------------------------------------------------------------
027100100902       //?Prepara SQL con le selezioni del filtro
027200100901       //--------------------------------------------------------------
027300100901       BEGSR PrepaSQL ;
027400100901
027500100902         wrkgetlista='SELECT * from UNGIO00F WHERE ungcuf=''' +§sdulcuf + '''';
027600100902         if §sdulinvr<>' ' ;
027700100902         wrkgetlista =%trim(wrkgetlista) + ' and unginvr='''+§sdulinvr+'''' ;
027800100902         endif  ;
027900100902         if §sdulcconf= 'S' ;
028000100902         wrkgetlista =%trim(wrkgetlista) + ' and ungfmv ='' '''          ;
028100100902         endif  ;
028200100902         if §sduldtm >0     ;
028300100902         wrkgetlista =%trim(wrkgetlista)+' and ungdtm<='+%editc(§sduldtm:'X') ;
028400100902         endif  ;
028500100902         if §sduldtcon > 0     ;
028600100902         wrkgetlista =%trim(wrkgetlista)+' and ungfmv=''S'' and ungdtm= ' +
028700100902           %editc(§sduldtcon:'X')   ;
028800100902         endif  ;
028900100902         if §sdulmag >0    ;
029000100902         wrkgetlista =%trim(wrkgetlista)+
029100100902         ' and ungmag =' +%editc(§sdulmag:'X');
029200100902         endif  ;
029300100902         if §sdulmgc >0    ;
029400100902         wrkgetlista =%trim(wrkgetlista)+
029500100902         ' and ungmgc =' +%editc(§sdulmgc:'X');
029600100902         endif  ;
029700100902         if §sdulute <>*blanks  ;
029800100902         wrkgetlista =%trim(wrkgetlista) + ' and ungute=''' +§sdulute+'''' ;
029900100902         endif  ;
030000100917         if §sduldim   > 0     ;
030100100917         wrkgetlista =%trim(wrkgetlista)+' and ungdim>= ' +
030200100917           %editc(§sduldim:'X') + ' and unghim>= ' +  %editc(§sdulhim:'X')  ;
030300100917         endif  ;
030400100917
030500101005         if §sdulord='D'  ;
030600100902         wrkgetlista =%trim(wrkgetlista) + ' order by ungdim, unghim '   ;
030700101005         else  ;
030800101005         wrkgetlista =%trim(wrkgetlista) + ' order by ungmag, ungtip ';
030900101005         endif  ;
031000100902
031100100901         exec sql   PREPARE s1 FROM :wrkgetlista   ;
031200100901
031300100901         exec sql   DECLARE a1 cursor FOR  s1   ;
031400100901
031500100901         exec sql OPEN a1   ;
031600100901
031700100901         ENDSR    ;
031800100902       //--------------------------------------------------------------
031900100902       //?Leggi record selezionati con SQL
032000100902       //--------------------------------------------------------------
032100100902       BEGSR LeggiSQL ;
032200100902
032300100902         clear sqlcod   ;
032400100902
032500100902         dow sqlcod=0   ;
032600100902
032700100902         exec sql FETCH NEXT  FROM a1 INTO: ungiods ;
032800100902
032900100902         select ;
033000100902         when sqlcod=100 ;
033100100902
033200100902         when sqlcod<0   ;
033300100902         *inh1=*on   ;
033400100902
033500100902         other   ;
033600100917         exsr StampaRec ;
033700100902         endsl  ;
033800100902
033900100902         enddo   ;
034000100902
034100100902         exec sql CLOSE a1   ;
034200100902
034300100902       ENDSR    ;
034400100902       //--------------------------------------------------------------
034500100902       //?Stampa record selezionato
034600100902       //--------------------------------------------------------------
034700100902       BEGSR StampaREC              ;
034800100902       // Immissione
034900100902       dataiso=%date(ungdim)    ;
035000100902       datadmy=dataiso        ;
035100100902       s1cdim=%dec(datadmy)   ;
035200100902
035300100902       s1chim=unghim   ;
035400100902       wazorg='E'   ;
035500100902       s1cmag=ungmag  ;
035600100902       wmag=%editc(s1cmag:'X')  ;
035700100902       exsr  chkmag   ;
035800100902       s1dmag=wdmag  ;
035900100920       dmag25=wdmag  ;
036000100902
036100100902       // Magazzino correlato
036200100902       clear s1cmgc  ;
036300100902       clear s1dmgc  ;
036400100902       if ungmgc>0  ;
036500100902        s1cmgc=ungmgc    ;
036600101011       // non decodifico per gli invii
036700101011       if unginvr<>'I'   ;
036800100902        wazorg='E'   ;
036900100902        wmag=%editc(s1cmgc:'X')  ;
037000100902        exsr  chkmag   ;
037100101011        s1dmgc='-'+ wdmag  ;
037200100902       endif   ;
037300101011       endif   ;
037400100902
037500100902       // Tipo unità
037600100902       clear s1ctip  ;
037700100902       clear s1dtip  ;
037800100902       clear s1cmod  ;
037900100902       s1cmat=ungmat  ;
038000100902 1     if ungtip>0  ;
038100100902       s1ctip=%editc(ungtip:'X')   ;
038200100902         wtip=s1ctip   ;
038300100902         exsr chktip   ;
038400100902         s1dtip=wdtip  ;
038500101011
038600100902
038700100902 2     if ungmat<>*blanks  ;
038800100902       chain (ungtip:ungmat)  unana02l    ;
038900100902 3      if %found(unana02l)  ;
039000100902         s1cmod=unamod  ;
039100100902        else  ;
039200100902         s1cmod=*all'?'  ;
039300100902 3      endif  ;
039400100902 2     endif  ;
039500100902 1     endif  ;
039600101011
039700101011         // Se tipo = 99 unisco descrizione a quantità
039800101011         if ungtip=99  or ungqta>1  ;
039900101011           %subst(s1dtip:14:2)=%editc(ungqta:'X')  ;
040000101011           %subst(s1dtip:09:4)=' Qta'    ;
040100101011         endif   ;
040200100902
040300100902       // unità in
040400100902       clear s1dtru   ;
040500100902       if ungtru='A' ;
040600100902       s1dtru='Aumento ' ;
040700100902       endif  ;
040800100902       if ungtru='S' ;
040900100902       s1dtru='Sostituz' ;
041000100902       endif  ;
041100100902
041200100902       // Tipo unità di reso
041300100902       clear s1ctir  ;
041400101011       // clear s1dtir  ;
041500100902 1     if ungtir>0  ;
041600100902       s1ctir=%editc(ungtir:'X')   ;
041700101011         // wtip=s1ctir   ;
041800101011         // exsr chktip   ;
041900101011         // s1dtir=wdtip  ;
042000100902 1     endif  ;
042100100902
042200101011
042300101011        clear    s1cmatr  ;
042400101011        if ungmatr<>*blanks   ;
042500101011         LungM=%checkr(' ':ungmatr)  ;
042600101011         posM=15-lungM+1;
042700101011         %subst(s1cmatr:PosM:lungM)=ungmatr ;
042800101011        endif  ;
042900101011
043000101011       // s1cute =ungute  ;
043100100902       s1cdes=ungdes  ;
043200100902       s1cnot=ungnot  ;
043300100902       s1cksc=ungksc  ;
043400100902       clear s1dksc  ;
043500100902       if ungksc>0  ;
043600100902           chain   (1:dutkci:ungksc) cnaco00f   ;
043700100902 4         if %found(cnaco00f)   ;
043800101011             s1dksc='-'+ acorag  ;
043900100902             else  ;
044000101011             s1dksc= *all'?'  ;
044100100902           endif  ;
044200100902       endif  ;
044300101006
044400101006       // Materiale preparato
044500101006       clear s1dfpr  ;
044600101006       if ungfpr='P'  ;
044700101011        s1dfpr='M.PREPARATO ' ;
044800101006        endif  ;
044900100902
045000100917         // S t a m p a   E l e n c o
045100100917         if §sdultips='T'  ;
045200100917
045300100902       // testata
045400100902       if *in90    ;
045500100902       write sdumpt1      ;
045600100902       *in90 =*off  ;
045700100902       endif ;
045800100902
045900100902       write sdumpd1      ;
046000100917
046100100917       else  ;
046200100917         // S t a m p a   E t i c h e t t a
046300100917         clear dsetiunit   ;
046400100917         §eunitmag=%editc(s1cmag:'X') ;
046500100920         §eunitdmag=dmag25             ;
046600100917         §eunitksc =%editc(s1cksc:'X') ;
046700100917         §eunitdksc=s1dksc             ;
046800101028
046900101028         select   ;
047000101028         // Imposto il campo NOTE
047100101028         when ungnot<>*blanks  ;
047200100920         §eunitnot =ungnot             ;
047300101028         //   se vuoto e tipo 99 -->quantità + descrizione
047400101028         when ungtip=99  ;
047500101006         §eunitnot=%editc(ungqta:'X') + ' ' + ungdes   ;
047600101028
047700101028         //   se vuoto e tipo <> 99 -->solo descrizione
047800101028         when ungdes<>*blanks  ;
047900101028         §eunitnot =ungdes             ;
048000101028         other   ;
048100101028
048200101028         //   se vuota anche descrizione tipo e decodifica
048300101028         §eunitnot=%editc(ungtip:'X') + '-' + s1dtip   ;
048400101028         endsl  ;
048500101028
048600100917         callp FNLVUEZS  (dsetiunit) ;
048700100917       endif ;
048800100902
048900100902       ENDSR    ;
049000100902       //--------------------------------------------------------------
049100100902       //?controllo Magazzino
049200100902       //--------------------------------------------------------------
049300100902        BEGSR ChkMag                 ;
049400100902        clear WDMAG    ;
049500100902
049600100902       eval   orgfil=%int(wmag)  ;
049700100902
049800100902       if   wazorg='2 '  or wazorg='E ';
049900100902       chain   orgfil   azorg21l   ;
050000100927       if %found(azorg21l) and (orgfag<>' ') ;
050100100902       wdmag=orgdes  ;
050200100902       else   ;
050300100902       ErrGenerico=*on   ;
050400100902       endif    ;
050500100902       endif    ;
050600100902
050700100902       if wazorg='0 '  or (wazorg='E ' and ErrGenerico=*on )  ;
050800100902
050900100902         chain   orgfil  azorg01l   ;
051000100927         if %found(azorg01l) ;
051100100927         og143=orgde3  ;
051200100927         else ;
051300100927         clear  og143  ;
051400100927         clear orgfag  ;
051500100927         endif ;
051600100902
051700100927        if §ogntw='LOG' or orgfag='A' or orgfag='F'   ;
051800100902         wdmag=orgdes  ;
051900100902         ErrGenerico=*off  ;
052000100902         else     ;
052100100902         ErrGenerico=*on   ;
052200100902        endif    ;
052300100902       endif   ;
052400100902
052500100902         if        wazorg<>'0  ' and wazorg<>'2  ' and  wazorg<>'E  '
052600100902                   and wazorg<>*blanks and wmag<>wazorg       ;
052700100902         ErrGenerico=*on   ;
052800100902         endif    ;
052900100902
053000100902         if ErrGenerico =*on ;
053100100902         wdmag=*all'?'   ;
053200100902         endif  ;
053300100902
053400100902          ENDSR    ;
053500100902       //--------------------------------------------------------------
053600100902       //?controllo Tipo Unità
053700100902       //--------------------------------------------------------------
053800100902        BEGSR ChkTip                 ;
053900100902        clear WDTIP    ;
054000100902        indx=%scan('?':wtip )   ;
054100100902
054200100902        if Indx>0             ;
054300100902         clear tibs02ds  ;
054400100902         t02tla=' '      ;
054500100902         t02mod='R'      ;
054600100902         t02sif=knsif    ;
054700100902         t02cod='UNT'    ;
054800100902         TNTBE_RicercaControllo  (kpjba : tibs02ds);
054900100902         if t02ke1<>*blanks;
055000100902         wtip =t02ke1    ;
055100100902         wdtip=t02uni    ;
055200100902         else   ;
055300100902         clear  wtip ;
055400100902         endif  ;
055500100902
055600100902        ErrGenerico=*on   ;
055700100902        leavesr  ;
055800100902        endif   ;
055900100902
056000100902         clear tibs02ds  ;
056100100902         t02tla=' '      ;
056200100902         t02mod='C'      ;
056300100902         t02sif=knsif    ;
056400100902         t02cod='UNT'    ;
056500100902         t02ke1=wtip     ;
056600100902         TNTBE_RicercaControllo  (kpjba : tibs02ds);
056700100902
056800100902         if  t02err<>*blanks ;
056900100902         ErrGenerico=*on  ;
057000100902         wdtip=*all'?'   ;
057100100902         else  ;
057200100902         wdtip=t02uni    ;
057300100902         endif    ;
057400100902
057500100902          ENDSR    ;
057600100902       //--------------------------------------------------------------
057700100902       //?Operazioni finali.
057800100902       //--------------------------------------------------------------
057900100902       BEGSR RoutEnd;
058000100902
058100100917         if §sdultips='T'  ;
058200100917          write sdumpFI  ;
058300100917         endif  ;
058400100902
058500100920         if %open(fnlv22p)  ;
058600100920         close fnlv22p ;
058700100920         endif  ;
058800100920
058900100902         *inLR = *on;
059000100902
059100100902         return;
059200100902
059300100902       ENDSR;
059400100902
059500100902      /end-free
059600100902
059700100917**
059800100917OVRPRTF FILE(FNLV22P) OUTQ(xxxxxxxxxx) FORMTYPE('ETCH_EDP') USRDTA('Unità EDP') SHARE(*YES)
