000100080206      //--------------------------------------------------------------
000200100901      //?TNSDUMR - Stampa inVII/rientri  GIORNALIERI UNITÀ EDP  - esegue -
000300080206      //--------------------------------------------------------------
000400080206
000500080206     h decedit('0,') datedit(*ymd/) option(*nodebugio)
000600080206
000700080206      //---------------------------------------------------------------
000800080206      //?Dichiarazione file.
000900080206      //---------------------------------------------------------------
001000100902     FUNana02L  iF   E           K DISK
001100100312     FAZORG21l  IF   E           K DISK
001200100312     fazorg01l  if   e           k disk
001300100312     fkpprf01l  if   e           k disk
001400100826     fcnaco00f  if   e           k disk
001500100902     FTNSDUMP   O    E             PRINTER oflind(*in90)
001600100917     FFNLV22P   O    F  132        PRINTER usropn
001700080206      //---------------------------------------------------------------
001800080206      //?Definizione schiere.
001900080206      //---------------------------------------------------------------
002000100917     d OVRP            s             91    dim(1) ctdata perrcd(1)
002100080206      //---------------------------------------------------------------
002200080206      //?Definizione aree dati.
002300080206      //---------------------------------------------------------------
002400080206      // - Dati utente
002500080206     d §AzUte        e ds                  extname(AZUTE00F)
002600080206     d                                     dtaara
002700080206     d §DatiUte      e ds                  extname(dDatiUte)
002800080206     d                                     dtaara
002900080206
003000080206      //---------------------------------------------------------------
003100080206      //?Definizione strutture dati.
003200080206      //---------------------------------------------------------------
003300080206
003400080206      // - Parametri ricevuti
003500080206     d KPJBA         e ds
003600080206
003700080206      // - Reperimento dati utente
003800080206     d TIBS34ds      e ds
003900080206     d dUte01        e ds
004000080702     d DLAT          e ds
004100100927     d og143         e ds
004200080702
004300100901     d tnsdulds      e ds
004400100917     d dsetiunit     e ds
004500100902     d ungiods       e ds                  extname(ungio00f)
004600080206
004700080206      // - Ricerca/Controllo tabelle
004800080206     d TIBS02ds      e ds                  inz
004900100316
005000100316     d wlbdat          ds                  inz
005100100316     d  g02dat                 1      8  0
005200100316     d  g02inv                 9     16  0
005300100316     d  g02err                17     17
005400100316     d  g02tgi                18     22  0
005500100826
005600100827      // ricerca filiale su organigramma
005700100827     d   pInFIL        S              3
005800100827     d   pInFAG        S              1
005900100827     d   pInDES        S             25
006000100827     d   pInDIT        S              3
006100100316
006200080206      //---------------------------------------------------------------
006300080206      //?Definizione variabili globali.
006400080206      //---------------------------------------------------------------
006500080206
006600080206      // - Flags booleani
006700080208     d $Fine           s               n   inz(*off)
006800100902     d ErrGenerico     s               n   inz(*off)
006900080206
007000100901     d Indx            s              3  0 inz
007100100901     d w1cdtconf       s              8  0 inz
007200100831     d w1cdtm          s              8  0 inz
007300100316     d wazorg          s              1    Inz(' ')
007400100316     d wmag            s              3
007500100316     d wdmag           s             30
007600100708     d wtip            s              2
007700100708     d wdtip           s             30
007800100827     d Datapiu10       s              8  0
007900100901     d Dataoggi        s              8  0
008000100917     d comman          s            100
008100100917     d lung            s             15  5
008200100920     d dmag25          s             25
008300101011     d PosM            s              2  0
008400101011     d LungM           s              2  0
008500080414
008600100901     d wrkgetlista     s           4096    varying
008700100901
008800100315     d Dataeur         s               d   datfmt(*eur)
008900100315     d Dataiso         s               d   datfmt(*iso)
009000080605     d Dataymd         s               d   datfmt(*ymd)
009100080605     d Datadmy         s               d   datfmt(*dmy)
009200100901     d Datasys         s               d   inz(*sys)
009300080604     d
009400080605     d                 DS
009500080605     d  AA                     1      4  0
009600080605     d  MM                     5      6  0
009700080605     d  GG                     7      8  0
009800080605     d DATA                    1      8  0
009900081009     d
010000080206      //---------------------------------------------------------------
010100080206      //?Definizione procedure usate.
010200080206      //---------------------------------------------------------------
010300100901      /copy gaitrasrc/srcprotopr,tibs02r
010400080414      /copy gaitrasrc/srcprotopr,tibs34r
010500100316      /copy gaitrasrc/srcprotopr,xsrda8
010600100917      /copy gaitrasrc/srcprotopr,qcmdexc
010700100917      /copy gaitrasrc/srcprotopr,fnlvuezs
010800080206
010900080206      //---------------------------------------------------------------
011000080206      //?Riepilogo indicatori.
011100080206      //---------------------------------------------------------------
011200080207      // - Comuni
011300080207      // 28    : Emissione messaggio di errore a video
011400080207      // - C01/S01
011500080207      // 30    : Condiziona SFLDSP    (*not)
011600080207      // 31    : Condiziona SFLDSPCTL (*not)
011700080207      // 30+31 : Condiziona SFLCLR
011800100824      // 32    : Condiziona SFLNXTCHG
011900080207      // 50    : Errore: Opzione errata
012000080207      // - D01
012100080207      // - Comuni
012200080207      // 99    : Generico di Errore
012300080206      //---------------------------------------------------------------
012400080206
012500080206      //---------------------------------------------------------------
012600080206      //?M A I N - L I N E
012700080206      //---------------------------------------------------------------
012800080206
012900080627     c     *Entry        plist
013000080206     c                   parm                    KPJBA
013100100901     c                   movel     kpjbu         tnsdulds
013200080702
013300080206      /free
013400080206
013500080206       // Operazioni iniziali
013600080206       exsr RoutInz;
013700100917       if $Fine=*off ;
013800081215
013900100901       // Prepara SQL con le selezioni previste a filtro
014000100901       exsr PrepaSQL    ;
014100080206
014200100902       // Lettura record selezionati
014300100902       exsr LeggiSQL    ;
014400100917       endif  ;
014500100902
014600080206       // Operazioni finali
014700080206       exsr RoutEnd;
014800080206
014900080206       //--------------------------------------------------------------
015000080206       //?Operazioni iniziali.
015100080206       //--------------------------------------------------------------
015200080206       BEGSR RoutInz;
015300100902       *in90  =*on;
015400080206
015500080206         // Reperimento dati job
015600100902         exsr DatiJob;
015700100826
015800100902       // Udate
015900100826       dataiso=datasys ;
016000100831       dataeur=dataiso ;
016100100831       Dataoggi=%dec(dataeur) ;
016200100902
016300100902       // PREPARA TESTAta con selezioni
016400100902       if §sdulcconf='S'   ;
016500100902        t1dconf='ancora da confermare alla data'   ;
016600100902        dataiso=%date(§sduldtm) ;
016700100902        dataeur=dataiso ;
016800100902        t1cdtm  =%dec(dataeur) ;
016900100902       else   ;
017000100902        t1dconf='      già confermati in  data'   ;
017100100902        dataiso=%date(§sduldtcon) ;
017200100902        dataeur=dataiso ;
017300100902        t1cdtm  =%dec(dataeur) ;
017400100902       endif  ;
017500100902
017600100902       // Ufficio
017700140124       select ;
017800140124       when §sdulcuf='MAG'          ;
017900101008         t1dcuf=' MAG=Mag. C.E.D. ' ;
018000140124       when §sdulcuf='TLC'          ;
018100140124         t1dcuf=' TLC=Mag. T.L.C. ' ;
018200140124       other ;
018300101008         t1dcuf='FOR=Mag. ACQUISTI' ;
018400140124       endsl  ;
018500101005
018600101005       // Ordinamento
018700101005       if §sdulord='D'  ;
018800101005       t1dord='Data / Ora IMMISSIONE'  ;
018900101005       *in10=*on  ;
019000101005       else  ;
019100101005       t1dord='Magazzino DESTINAZIONE, tipo Unità';
019200101005       *in11=*on  ;
019300101005       endif ;
019400100902
019500100902       // Tipo invio /rientro
019600100902       select  ;
019700100902       when §sdulinvr='I'   ;
019800100902        t1dinvr=' I=Invii '  ;
019900100902       when §sdulinvr='R'  ;
020000100902        t1dinvr='R=Rientro da fornitore'  ;
020100100902       when §sdulinvr='F'  ;
020200100902        t1dinvr='F=Rientro da filiale  '  ;
020300101013       when §sdulinvr='S'  ;
020400101013        t1dinvr='S=Movimenti da Sede  '  ;
020500100902        endsl  ;
020600100902
020700100902       // Magazzino destinazione
020800100902       if §sdulmag>0  ;
020900100902        t1cmag=§sdulmag  ;
021000100902        wazorg='E'     ;
021100100902        wmag=%editc(t1cmag:'X')  ;
021200100902        exsr  chkmag   ;
021300100902        t1dmag=wdmag  ;
021400100902       endif  ;
021500100902
021600100902       // Magazzino correlato
021700100902       if §sdulmgc>0  ;
021800100902        t1cmgc=§sdulmgc  ;
021900100902        wazorg='E'   ;
022000100902        wmag=%editc(t1cmgc:'X')  ;
022100100902        exsr  chkmag   ;
022200100902        t1dmgc=wdmag  ;
022300100902       endif   ;
022400100902
022500100902       t1cute=§sdulute  ;
022600100917
022700100917       // data / ora immissione
022800100917       if §sduldim>0  ;
022900100917        dataiso=%date(§sduldim) ;
023000100917        dataeur=dataiso ;
023100100917        t1cdim  =%dec(dataeur) ;
023200100917        t1chim=§sdulhim  ;
023300100917       endif  ;
023400100917
023500100917       // Se stampa etichette    eseguo ovrprtf fnlv22p e apro il file
023600100917       if §sdultips='E'   ;
023700100917         comman=ovrp(1)  ;
023800100917         lung=91   ;
023900100917         %subst(comman:28:10)=§sdulpre  ;
024000100917
024100100917         callp(e)  QCMDEXC (comman:lung)   ;
024200100917         if not %error  ;
024300100917          open fnlv22p ;
024400100917         else  ;
024500100917          $Fine=*on  ;
024600100917         endif  ;
024700100917
024800100917       endif  ;
024900100917
025000080627         ENDSR;
025100080206
025200080206       //--------------------------------------------------------------
025300080206       //?Reperimento Dati del job (Utente/Operativi).
025400080206       //--------------------------------------------------------------
025500080206       BEGSR DatiJob;
025600080206
025700080206         in(E) §AzUte;
025800080206         if NOT %error;
025900080206           in(E) §DatiUte;
026000080206         endif;
026100080206         if %error or RSut = *blanks;
026200080206           clear TIBS34ds;
026300080206           tibs34r(tibs34ds);
026400080206           in §AzUte;
026500080206           in §DatiUte;
026600080206         endif;
026700080206
026800080206       ENDSR;
026900100901       //--------------------------------------------------------------
027000100902       //?Prepara SQL con le selezioni del filtro
027100100901       //--------------------------------------------------------------
027200100901       BEGSR PrepaSQL ;
027300100901
027400100902         wrkgetlista='SELECT * from UNGIO00F WHERE ungcuf=''' +§sdulcuf + '''';
027500100902         if §sdulinvr<>' ' ;
027600100902         wrkgetlista =%trim(wrkgetlista) + ' and unginvr='''+§sdulinvr+'''' ;
027700100902         endif  ;
027800100902         if §sdulcconf= 'S' ;
027900100902         wrkgetlista =%trim(wrkgetlista) + ' and ungfmv ='' '''          ;
028000100902         endif  ;
028100100902         if §sduldtm >0     ;
028200100902         wrkgetlista =%trim(wrkgetlista)+' and ungdtm<='+%editc(§sduldtm:'X') ;
028300100902         endif  ;
028400100902         if §sduldtcon > 0     ;
028500100902         wrkgetlista =%trim(wrkgetlista)+' and ungfmv=''S'' and ungdtm= ' +
028600100902           %editc(§sduldtcon:'X')   ;
028700100902         endif  ;
028800100902         if §sdulmag >0    ;
028900100902         wrkgetlista =%trim(wrkgetlista)+
029000100902         ' and ungmag =' +%editc(§sdulmag:'X');
029100100902         endif  ;
029200100902         if §sdulmgc >0    ;
029300100902         wrkgetlista =%trim(wrkgetlista)+
029400100902         ' and ungmgc =' +%editc(§sdulmgc:'X');
029500100902         endif  ;
029600100902         if §sdulute <>*blanks  ;
029700100902         wrkgetlista =%trim(wrkgetlista) + ' and ungute=''' +§sdulute+'''' ;
029800100902         endif  ;
029900100917         if §sduldim   > 0     ;
030000100917         wrkgetlista =%trim(wrkgetlista)+' and ungdim>= ' +
030100100917           %editc(§sduldim:'X') + ' and unghim>= ' +  %editc(§sdulhim:'X')  ;
030200100917         endif  ;
030300100917
030400101005         if §sdulord='D'  ;
030500100902         wrkgetlista =%trim(wrkgetlista) + ' order by ungdim, unghim '   ;
030600101005         else  ;
030700101005         wrkgetlista =%trim(wrkgetlista) + ' order by ungmag, ungtip ';
030800101005         endif  ;
030900100902
031000100901         exec sql   PREPARE s1 FROM :wrkgetlista   ;
031100100901
031200100901         exec sql   DECLARE a1 cursor FOR  s1   ;
031300100901
031400100901         exec sql OPEN a1   ;
031500100901
031600100901         ENDSR    ;
031700100902       //--------------------------------------------------------------
031800100902       //?Leggi record selezionati con SQL
031900100902       //--------------------------------------------------------------
032000100902       BEGSR LeggiSQL ;
032100100902
032200100902         clear sqlcod   ;
032300100902
032400100902         dow sqlcod=0   ;
032500100902
032600100902         exec sql FETCH NEXT  FROM a1 INTO: ungiods ;
032700100902
032800100902         select ;
032900100902         when sqlcod=100 ;
033000100902
033100100902         when sqlcod<0   ;
033200100902         *inh1=*on   ;
033300100902
033400100902         other   ;
033500100917         exsr StampaRec ;
033600100902         endsl  ;
033700100902
033800100902         enddo   ;
033900100902
034000100902         exec sql CLOSE a1   ;
034100100902
034200100902       ENDSR    ;
034300100902       //--------------------------------------------------------------
034400100902       //?Stampa record selezionato
034500100902       //--------------------------------------------------------------
034600100902       BEGSR StampaREC              ;
034700100902       // Immissione
034800100902       dataiso=%date(ungdim)    ;
034900100902       datadmy=dataiso        ;
035000100902       s1cdim=%dec(datadmy)   ;
035100100902
035200100902       s1chim=unghim   ;
035300100902       wazorg='E'   ;
035400100902       s1cmag=ungmag  ;
035500100902       wmag=%editc(s1cmag:'X')  ;
035600100902       exsr  chkmag   ;
035700100902       s1dmag=wdmag  ;
035800100920       dmag25=wdmag  ;
035900100902
036000100902       // Magazzino correlato
036100100902       clear s1cmgc  ;
036200100902       clear s1dmgc  ;
036300100902       if ungmgc>0  ;
036400100902        s1cmgc=ungmgc    ;
036500101011       // non decodifico per gli invii
036600101011       if unginvr<>'I'   ;
036700100902        wazorg='E'   ;
036800100902        wmag=%editc(s1cmgc:'X')  ;
036900100902        exsr  chkmag   ;
037000101011        s1dmgc='-'+ wdmag  ;
037100100902       endif   ;
037200101011       endif   ;
037300100902
037400100902       // Tipo unità
037500100902       clear s1ctip  ;
037600100902       clear s1dtip  ;
037700100902       clear s1cmod  ;
037800100902       s1cmat=ungmat  ;
037900100902 1     if ungtip>0  ;
038000100902       s1ctip=%editc(ungtip:'X')   ;
038100100902         wtip=s1ctip   ;
038200100902         exsr chktip   ;
038300100902         s1dtip=wdtip  ;
038400101011
038500100902
038600100902 2     if ungmat<>*blanks  ;
038700100902       chain (ungtip:ungmat)  unana02l    ;
038800100902 3      if %found(unana02l)  ;
038900100902         s1cmod=unamod  ;
039000100902        else  ;
039100100902         s1cmod=*all'?'  ;
039200100902 3      endif  ;
039300100902 2     endif  ;
039400100902 1     endif  ;
039500101011
039600101011         // Se tipo = 99 unisco descrizione a quantità
039700101011         if ungtip=99  or ungqta>1  ;
039800101011           %subst(s1dtip:14:2)=%editc(ungqta:'X')  ;
039900101011           %subst(s1dtip:09:4)=' Qta'    ;
040000101011         endif   ;
040100100902
040200100902       // unità in
040300100902       clear s1dtru   ;
040400100902       if ungtru='A' ;
040500100902       s1dtru='Aumento ' ;
040600100902       endif  ;
040700100902       if ungtru='S' ;
040800100902       s1dtru='Sostituz' ;
040900100902       endif  ;
041000100902
041100100902       // Tipo unità di reso
041200100902       clear s1ctir  ;
041300101011       // clear s1dtir  ;
041400100902 1     if ungtir>0  ;
041500100902       s1ctir=%editc(ungtir:'X')   ;
041600101011         // wtip=s1ctir   ;
041700101011         // exsr chktip   ;
041800101011         // s1dtir=wdtip  ;
041900100902 1     endif  ;
042000100902
042100101011
042200101011        clear    s1cmatr  ;
042300101011        if ungmatr<>*blanks   ;
042400101011         LungM=%checkr(' ':ungmatr)  ;
042500101011         posM=15-lungM+1;
042600101011         %subst(s1cmatr:PosM:lungM)=ungmatr ;
042700101011        endif  ;
042800101011
042900101011       // s1cute =ungute  ;
043000100902       s1cdes=ungdes  ;
043100100902       s1cnot=ungnot  ;
043200100902       s1cksc=ungksc  ;
043300100902       clear s1dksc  ;
043400100902       if ungksc>0  ;
043500100902           chain   (1:dutkci:ungksc) cnaco00f   ;
043600100902 4         if %found(cnaco00f)   ;
043700101011             s1dksc='-'+ acorag  ;
043800100902             else  ;
043900101011             s1dksc= *all'?'  ;
044000100902           endif  ;
044100100902       endif  ;
044200101006
044300101006       // Materiale preparato
044400101006       clear s1dfpr  ;
044500101006       if ungfpr='P'  ;
044600101011        s1dfpr='M.PREPARATO ' ;
044700101006        endif  ;
044800100902
044900100917         // S t a m p a   E l e n c o
045000100917         if §sdultips='T'  ;
045100100917
045200100902       // testata
045300100902       if *in90    ;
045400100902       write sdumpt1      ;
045500100902       *in90 =*off  ;
045600100902       endif ;
045700100902
045800100902       write sdumpd1      ;
045900100917
046000100917       else  ;
046100100917         // S t a m p a   E t i c h e t t a
046200100917         clear dsetiunit   ;
046300100917         §eunitmag=%editc(s1cmag:'X') ;
046400100920         §eunitdmag=dmag25             ;
046500100917         §eunitksc =%editc(s1cksc:'X') ;
046600100917         §eunitdksc=s1dksc             ;
046700101028
046800101028         select   ;
046900101028         // Imposto il campo NOTE
047000101028         when ungnot<>*blanks  ;
047100100920         §eunitnot =ungnot             ;
047200101028         //   se vuoto e tipo 99 -->quantità + descrizione
047300101028         when ungtip=99  ;
047400101006         §eunitnot=%editc(ungqta:'X') + ' ' + ungdes   ;
047500101028
047600101028         //   se vuoto e tipo <> 99 -->solo descrizione
047700101028         when ungdes<>*blanks  ;
047800101028         §eunitnot =ungdes             ;
047900101028         other   ;
048000101028
048100101028         //   se vuota anche descrizione tipo e decodifica
048200101028         §eunitnot=%editc(ungtip:'X') + '-' + s1dtip   ;
048300101028         endsl  ;
048400101028
048500100917         callp FNLVUEZS  (dsetiunit) ;
048600100917       endif ;
048700100902
048800100902       ENDSR    ;
048900100902       //--------------------------------------------------------------
049000100902       //?controllo Magazzino
049100100902       //--------------------------------------------------------------
049200100902        BEGSR ChkMag                 ;
049300100902        clear WDMAG    ;
049400100902
049500100902       eval   orgfil=%int(wmag)  ;
049600100902
049700100902       if   wazorg='2 '  or wazorg='E ';
049800100902       chain   orgfil   azorg21l   ;
049900100927       if %found(azorg21l) and (orgfag<>' ') ;
050000100902       wdmag=orgdes  ;
050100100902       else   ;
050200100902       ErrGenerico=*on   ;
050300100902       endif    ;
050400100902       endif    ;
050500100902
050600100902       if wazorg='0 '  or (wazorg='E ' and ErrGenerico=*on )  ;
050700100902
050800100902         chain   orgfil  azorg01l   ;
050900100927         if %found(azorg01l) ;
051000100927         og143=orgde3  ;
051100100927         else ;
051200100927         clear  og143  ;
051300100927         clear orgfag  ;
051400100927         endif ;
051500100902
051600100927        if §ogntw='LOG' or orgfag='A' or orgfag='F'   ;
051700100902         wdmag=orgdes  ;
051800100902         ErrGenerico=*off  ;
051900100902         else     ;
052000100902         ErrGenerico=*on   ;
052100100902        endif    ;
052200100902       endif   ;
052300100902
052400100902         if        wazorg<>'0  ' and wazorg<>'2  ' and  wazorg<>'E  '
052500100902                   and wazorg<>*blanks and wmag<>wazorg       ;
052600100902         ErrGenerico=*on   ;
052700100902         endif    ;
052800100902
052900100902         if ErrGenerico =*on ;
053000100902         wdmag=*all'?'   ;
053100100902         endif  ;
053200100902
053300100902          ENDSR    ;
053400100902       //--------------------------------------------------------------
053500100902       //?controllo Tipo Unità
053600100902       //--------------------------------------------------------------
053700100902        BEGSR ChkTip                 ;
053800100902        clear WDTIP    ;
053900100902        indx=%scan('?':wtip )   ;
054000100902
054100100902        if Indx>0             ;
054200100902         clear tibs02ds  ;
054300100902         t02tla=' '      ;
054400100902         t02mod='R'      ;
054500100902         t02sif=knsif    ;
054600100902         t02cod='UNT'    ;
054700100902         TNTBE_RicercaControllo  (kpjba : tibs02ds);
054800100902         if t02ke1<>*blanks;
054900100902         wtip =t02ke1    ;
055000100902         wdtip=t02uni    ;
055100100902         else   ;
055200100902         clear  wtip ;
055300100902         endif  ;
055400100902
055500100902        ErrGenerico=*on   ;
055600100902        leavesr  ;
055700100902        endif   ;
055800100902
055900100902         clear tibs02ds  ;
056000100902         t02tla=' '      ;
056100100902         t02mod='C'      ;
056200100902         t02sif=knsif    ;
056300100902         t02cod='UNT'    ;
056400100902         t02ke1=wtip     ;
056500100902         TNTBE_RicercaControllo  (kpjba : tibs02ds);
056600100902
056700100902         if  t02err<>*blanks ;
056800100902         ErrGenerico=*on  ;
056900100902         wdtip=*all'?'   ;
057000100902         else  ;
057100100902         wdtip=t02uni    ;
057200100902         endif    ;
057300100902
057400100902          ENDSR    ;
057500100902       //--------------------------------------------------------------
057600100902       //?Operazioni finali.
057700100902       //--------------------------------------------------------------
057800100902       BEGSR RoutEnd;
057900100902
058000100917         if §sdultips='T'  ;
058100100917          write sdumpFI  ;
058200100917         endif  ;
058300100902
058400100920         if %open(fnlv22p)  ;
058500100920         close fnlv22p ;
058600100920         endif  ;
058700100920
058800100902         *inLR = *on;
058900100902
059000100902         return;
059100100902
059200100902       ENDSR;
059300100902
059400100902      /end-free
059500100902
059600100917**
059700100917OVRPRTF FILE(FNLV22P) OUTQ(xxxxxxxxxx) FORMTYPE('ETCH_EDP') USRDTA('Unità EDP') SHARE(*YES)
