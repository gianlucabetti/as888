000100001027     H DECEDIT('0,') DATEDIT(*DMY.)
000200110912      * FNLSW2R *----------------------------------------------------------*
000300110912      *   Invio anomalie ai clienti mittenti
000400080319      *--------------------------------------------------------------------*
000500111202     Fcnaco00f  IF   E           K DISK
000600111202     Ffnblp01l  IF   E           K DISK
000700110922     Ffnblt27l  IF   E           K DISK
000800110913     Fazorg01l  IF   E           K DISK
000900080327     Ftabel00f  IF   E           K DISK
001000110926     Ffnanm02l  uF   E           K DISK    prefix(a_)
001100110913     FwfANC01L  uF a E           K DISK
001200050705      *
001300110916     d*  linee partenza selezionate prese una sola volta
001400110913     D lnpSel          S              3  0 DIM(20)
001500110916     D*  linee partenze selezionate con relativo terminal partenza
001600110922     D tfpLnp          S              6  0 DIM(20)
001700110916     D* LNP + 5F  CHE devo leggere
001800110916     D LP4             S              3  0 DIM(199)
001900110916     D*
002000110916     D FAS             S              5  0 DIM($MaxElem)
002100080328     d
002200110916     D DSLAS           DS            87    OCCURS($MaxElem)
002300110916     D  LAS                    1     87
002400110916     D                                     DIM(29)
002500110913     d KPJBA         e ds
002600080328     d tibs34ds      e ds                  inz
002700080410     d tibs02ds      e ds                  inz
002800080328     d AzuteDs       e ds                  ExtName(AzUte00F) dtaara(§azute)
002900080328     d DDatiUte      e ds                  dtaara(§datiute)
003000110914     d fnanmds       e ds                  ExtName(fnanm00f)
003100110916     d ds5e          e ds
003200110916     d fnlv55ds      e ds
003300111116     d fnlr44ds      e ds
003400111116     d fnlr34ds      e ds
003500111117     d trulNRSds     E DS
003600161221       // -?Legami clienti
003700161221     d TIBS10DS      e ds                  INZ(*EXTDFT)
003800161221     d  skc                          11    dim(500) overlay(d10skc)
003900080829     d
004000110913     D fnlsw1ds        DS
004100110913     D  COMdchd                1      8  0
004200110914     D  COMdcha                9     16  0
004300110914     D  LNP                   17     56P 0
004400110914     D                                     DIM(20)
004500110914     D  CLI                   57    136P 0
004600110914     D                                     DIM(20)
004700110914     D  NRS                  137    176
004800110914     D                                     DIM(20)
004900110914     D  videmail             177    201
005000111116     D  parGEST              202    202
005100161221     d* vidok    usato in fnlsw7r
005200161221     D  vidok                203    203
005300161221     d* videveok usato in fnlsw7r
005400161221     D  videveok             204    204
005500161221     d* vidcons  usato in fnlsw7r
005600161221     D  vidcons              205    205
005700161221     d* UNIF usato oer ora solo qui
005800161222     D  UNIF                 206    225    dim(20)
005900110914     d
006000110914     d
006100110617     D  pInFILE        s             10
006200110617     D  pInIDJOB       s             26
006300110617     D  pInPWDXLS      s             15
006400110617     D  pInNOMXLS      s             78
006500111206     D  pInEMLVAR      s             80
006600110617     D  pInEMLDEST     s            121
006700120504     D  pInMRAKE2      s             15    inz
006800110617     D  pOutESITO      s              1
006900111013     d wdao            s              8  0
007000111013     d wdch            s              8  0
007100111013     d dateu           s              8  0
007200110613     d ora_u           s              6  0
007300110913     d xx              s              3  0
007400110913     d yy              s              3  0
007500110916     d ee              s              3  0
007600110916     d zz              s              3  0
007700161221     d ww              s              3  0
007800110916     d x               s              3  0
007900110916     d y               s              3  0
008000110913     d Indx            s              3  0
008100110916     d ktfp            s              3  0
008200110916     d savtfp          s              3  0
008300110913     d Wccm            s              7  0
008400111202     d oggcli          s              7  0
008500110916     d W0030           s              3  0
008600110916     d WLPSR           s              5  0
008700111116     d anmrrn          s              9  0
008800161221     d wcod11          s             11  0
008900161221     d wcliente        s             11
009000110913     d alfanrs         s              2
009100110913     d Primo           s              1
009200110913     d Wok             s              1
009300110914     d comcaa          s                   like(anmcaa)
009400110916     D WMAX            S                   LIKE(TBLKEY) inz
009500110914     d descaa          s             25
009600110613     d wlib            s             10    inz('GAITRAGRPS')
009700080828     d wrkgetlista     s           4096    varying
009800110613     d Datasys         s               d   datfmt(*iso)  inz(*sys)
009900110613     d Ora_sys         s               t   timfmt(*iso)  inz(*sys)
010000110613     d Ora_iso         s               t   timfmt(*iso)
010100110613     d Dataiso         s               d   datfmt(*iso)
010200111013     d Dataeur         s               d   datfmt(*eur)
010300080327     d Datadmy         s               d   datfmt(*dmy)
010400110916
010500110916     d $MaxElem        c                   const(998)
010600080829     d
010700080317      *------------------------------------------------------------------------*
010800080317      *   P R O T O T I P I
010900080317      *------------------------------------------------------------------------*
011000080829      /copy gaitrasrc/srcprotopr,tibs34r
011100110617      /copy gaitrasrc/srcprotopr,trtcm8r1
011200110916      /copy gaitrasrc/srcprotopr,fnlv55r
011300111116      /copy gaitrasrc/srcprotopr,fnlr44r
011400111117      /copy gaitrasrc/srcprotopr,trulnrs
011500161221      /copy gaitrasrc/srcprotopr,TIBS10R
011600001027     C*---------------------------------------------------------------*
011700001027     C     *ENTRY        PLIST
011800080325     C                   PARM                    kpjba
011900110913     c                   movel     kpjbu         fnlsw1ds
012000080317      /free
012100080828
012200110913       // Linee partenza selezionate
012300110913          EXSR  PrepaLNP      ;
012400110914
012500110913       // Prepara SQL
012600110914          EXSR SQLINV115      ;
012700080902
012800110913       //  Esegui SQL
012900110914          EXSR SQLEseg115   ;
013000110617
013100110916       // Carico i terminal per i manca record bolla
013200111201       //          eseguo solo se non ho premuto F12 in gestione
013300111201
013400111201   0      if  parGEST='I'  or wuscit=*blanks  ;
013500111201
013600110916          EXSR carTFP   ;
013700110916
013800110916          sorta tfplnp  ;
013900110916          eval ee= 1 ;
014000110916  1       dow  ee<=20  ;
014100110916
014200110916  2       if tfplnp(ee)>0  ;
014300110916          ktfp=tfplnp(ee)/1000  ;
014400110916  3       if ktfp<>savtfp  ;
014500110916
014600110916          // Carico tabella 5F confeabili dai terminal di linee selez.
014700110916          exsr   carfas   ;
014800110916
014900110916          // carico le linee da leggere più le confermabili dal relativo
015000110916          //   terminal partenza da tabella 5F
015100110916          EXSR   PrepaLP4    ;
015200110916
015300110916
015400110916       // Prepara SQL per anomalia manca record
015500110914          EXSR SQLINV105      ;
015600110914
015700110914       //  Esegui SQL
015800110914          EXSR SQLEseg105   ;
015900110916
016000110916        savtfp=ktfp   ;
016100110916 3      endif  ;
016200110916 2      endif  ;
016300110914
016400110916        ee=ee+1  ;
016500110916 1      enddo  ;
016600110914
016700111116        // se sono in gestione emetto videata
016800111206 1      if ParGEST='G'   ;
016900111116
017000111206 2      if   wuscit=*blanks  ;
017100111116          clear    anmrrn  ;
017200111116          exsr    RicLR44  ;
017300111206 2      endif   ;
017400111116
017500111206 x1     else  ;
017600111116
017700110617        // Se scritto almeno un record, invio all'indirizzo e-mail
017800110913          pInFILE='WFANC00F'   ;
017900110617          pInIDJOB=knmeb       ;
018000111202          // Nell'oggetto imposto il primo codice cliente richiesto
018100111206  2       if  oggcli>0   ;
018200111202          chain (1:dutkci:oggcli) cnaco00f  ;
018300111206  3       if not %found(cnaco00f)  ;
018400111202            acorag=*all'?'  ;
018500111206  3       endif  ;
018600111202
018700111207            pinEMLVAR='*OBJM* Anomalie Cliente: '+%editc(oggcli:'X')+ '-'+
018800111207                       %subst(acorag:1:15)  ;
018900111206  x2      else  ;
019000111202          // Imposto la prima lnp/serie richiesta   ;
019100111207           pinEMLVAR='*OBJM* Anomalie Lnp/Ser:'+%editc(lnp(1):'X')+' '+
019200111202                                         nrs(1)      ;
019300111206  2       endif  ;
019400111206
019500120305          pInEMLDEST=%trim(videmail)+'@brt.it' ;
019600110617          callp trtcm8r1 (pInfile:pInIDJOB:pInPWDXLS:pInNOMXLS:pInEMLDEST:
019700120504                          pInEMLVAR:pInMRAKE2:pOutESITO) ;
019800080901
0199001112061       endif  ;
0200001112010       endif  ;
020100080829
020200080317        *inlr ='1'   ;
020300080828
020400110913       //-------------------------------------------------------------------
020500110913       BEGSR  PrepaLNP               ;
020600110913
020700110913       clear  LnpSel  ;
020800110913       xx= 1 ;
020900110913       yy= 1 ;
021000110913
021100110913       dow xx <=20  ;
021200110913        if lnp(xx)>0  ;
021300110913         Indx=%lookup(lnp(xx):LnpSel)  ;
021400110913         if Indx=0  ;
021500110913           eval lnpSel(yy)=lnp(xx)  ;
021600110913           yy=yy+1   ;
021700110913         endif  ;
021800110913
021900110913        endif  ;
022000110913
022100110913        xx=xx+1   ;
022200110913        enddo     ;
022300110913
022400111202        // Memorizzo nel campo oggcli il codice cliente che devo
022500111202        //  mettere nell'ogggetto della mail
022600111202        clear oggcli  ;
022700111202        if cli(1)>0   ;
022800111202          oggcli=cli(1)  ;
022900111202        endif  ;
023000111202
023100110913       ENDSR  ;
023200080901       //-------------------------------------------------------------------
023300110914       BEGSR  SQLINV115              ;
023400110913       clear primo  ;
023500110914       comcaa=115   ;
023600110913
023700110929        clear wrkgetlista  ;
023800111116        wrkgetlista ='select rrn(fnanm00f), fnanm00f.*                 +
023900111116                               from fnanm00f where anmcaa = ' +
024000110914                     %editc(comcaa:'X') +
024100110914                     ' and anmcch<>''AN'' and (anmdch=0'     ;
024200080901
024300110913        // NON ho richiesto anche le chiuse
024400110929        if comdchd=0    ;
024500110929           wrkgetlista=wrkgetlista + ')' ;
024600110929        else  ;
024700110929           wrkgetlista=wrkgetlista + ' or ('  ;
024800110929
024900110929        // Se richiesta elaborazione a oggi leggo anche
025000110929        //  anomalie chiuse ancora non trasmesse
025100110929
025200110929        if comdcha=dateu   ;
025300110929           wrkgetlista=%trim(wrkgetlista) +
025400110929                                    'anmdch>0 and anmft1='' '') or (';
025500110929           endif  ;
025600110929
025700110929           wrkgetlista=wrkgetlista + 'anmdch>0 and anmft1=''T'' and +
025800110929                                anmdtr>=' +%editc(comdchd:'X')+
025900110929                   ' and anmdtr<=' + %editc(comdcha:'X') +'))';
026000110929
026100110929        endif ;
026200110929
026300110913
026400110913        wrkgetlista =%trim(wrkgetlista) + ' and anmlnp in('  ;
026500110913        xx= 1   ;
026600110913        dow  xx <=20   ;
026700110913        if lnpSel(xx) > 0 ;
026800110913
026900110913        if Primo<>' '  ;
027000110913          wrkgetlista =%trim(wrkgetlista) + ','      ;
027100110913        endif  ;
027200110913
027300110913        wrkgetlista =%trim(wrkgetlista) + ' ' +%editc(lnpSel(xx):'X') ;
027400110913        eval Primo='S'  ;
027500110913        endif  ;
027600110913        xx=xx+1  ;
027700110913        enddo  ;
027800110913
027900111013        wrkgetlista =%trim(wrkgetlista) + ') order by anmdao'  ;
028000110913
028100080902
028200080901        ENDSR     ;
028300110914       //-------------------------------------------------------------------
028400110914       BEGSR  SQLINV105              ;
028500110914       clear primo  ;
028600110914       comcaa=105   ;
028700110916
028800110929        clear wrkgetlista  ;
028900111116        wrkgetlista ='select rrn(fnanm00f), fnanm00f.*                 +
029000111116                       from fnanm00f where anmcaa = ' +
029100110914                     %editc(comcaa:'X') +
029200110914                     ' and anmcch<>''AN'' and (anmdch=0'     ;
029300110914
029400110914        // NON ho richiesto anche le chiuse
029500110914        if comdchd=0    ;
029600110914           wrkgetlista=wrkgetlista + ')' ;
029700110914        else  ;
029800110929           wrkgetlista=wrkgetlista + ' or ('  ;
029900110929
030000110929        // Se richeista elaborazione a oggi leggo anche
030100110929        //  anomalie chiuse ancora non trasmesse
030200110929
030300110929        if comdcha=dateu   ;
030400110929           wrkgetlista=wrkgetlista + 'anmdch>0 and anmft1='' '') or (';
030500110929           endif  ;
030600110929
030700110929           wrkgetlista=wrkgetlista + 'anmdch>0 and anmft1=''T'' and +
030800110929                                anmdtr>=' +%editc(comdchd:'X')+
030900110929                   ' and anmdtr<=' + %editc(comdcha:'X') +'))';
031000110914
031100110914        endif ;
031200110914
031300110916        wrkgetlista =%trim(wrkgetlista) + ' and anmfls in('  ;
031400110914        xx= 1   ;
031500110916        dow  xx <=199  ;
031600110916        if LP4(xx) > 0 ;
031700110914
031800110914        if Primo<>' '  ;
031900110914          wrkgetlista =%trim(wrkgetlista) + ','      ;
032000110914        endif  ;
032100110914
032200110916        wrkgetlista =%trim(wrkgetlista) + ' ' +%editc(LP4(xx):'X') ;
032300110914        eval Primo='S'  ;
032400110914        endif  ;
032500110914        xx=xx+1  ;
032600110914        enddo  ;
032700110914
032800110914        wrkgetlista =%trim(wrkgetlista) + ')'  ;
032900110914
033000110916        // Selezione per ANMCDU
033100111013        wrkgetlista =%trim(wrkgetlista) + ' and anmcdu=' + %editc(ktfp:'X') +
033200111013         ' order by anmdao'  ;
033300110914
033400110914        ENDSR     ;
033500080829       //-------------------------------------------------------------------
033600110914       BEGSR  SQLEseg115           ;
033700110914
033800110914       exsr DecoAnom ;
033900110914
034000080828       EXEC sql  prepare s1 from : wrkgetlista ;
034100080828       EXEC sql  declare a1 cursor for s1      ;
034200080828       EXEC sql  open    a1                    ;
034300080828
034400111116       EXEC sql fetch next from a1 into :anmrrn, :fnanmds  ;
034500080828
034600080828       dow sqlcod=0    ;
034700080828
034800110916       exsr Elabora115 ;
034900110913
035000110913       // se record anomalia nelle selezioni fatte --> scrivo il file
035100110913       if   Wok='S'  ;
035200110913       exsr scrivifile  ;
035300110913       endif  ;
035400110913
035500111116       if wuscit=*blanks  ;
035600111116        EXEC sql fetch next from a1 into :anmrrn, :fnanmds  ;
035700111116       else  ;
035800111116
035900111116        sqlcod=100  ;
036000111116       endif ;
036100080829
036200080828       enddo           ;
036300080828
036400080829       EXEC sql  CLOSE   a1                    ;
036500080829
036600080828       ENDSR     ;
036700110916       //-------------------------------------------------------------------
036800110916       BEGSR  SQLEseg105           ;
036900110916
037000110916       exsr DecoAnom ;
037100110916
037200110916
037300110916       EXEC sql  prepare s2 from : wrkgetlista ;
037400110916       EXEC sql  declare a2 cursor for s2      ;
037500110916       EXEC sql  open    a2                    ;
037600110916
037700111116       EXEC sql fetch next from a2 into :anmrrn, :fnanmds  ;
037800110916
037900110916       dow sqlcod=0    ;
038000110916
038100110916       exsr Elabora105 ;
038200110916
038300111230       // Se serie 00 non la inviio/gestisco
038400111230
038500110916       // se record anomalia nelle selezioni fatte --> scrivo il file
038600111230       if   Wok='S'  and anmnrs>0  ;
038700111230
038800110922       // clearo i dati che non posso impostare nel file dalla bolla
038900110922       clear blprmn  ;
039000110922       clear blprma  ;
039100110922       clear blptsp  ;
039200110922       clear wccm    ;
039300110922
039400111013       // se si tratta di anomalia chiusa --> cerco la bolla
039500110922       if anmdch>0  and anmcch<>'AN'  ;
039600110922        chain  (anmfls:anmlna:anmnrs:anmscn) fnblt27l  ;
0397001109222         if %found(fnblt27l)  ;
039800110922           chain  (bltaas:bltlnp:bltnrs:bltnsp) fnblp01l  ;
0399001109222         anmaas=bltaas        ;
0400001109222         anmlnp=bltlnp       ;
0401001109222         anmnsp=bltnsp        ;
040200110922          endif  ;
040300110922       endif  ;
040400110922
040500110916       exsr scrivifile  ;
040600110916       endif  ;
040700110916
040800111116       if wuscit=*blanks  ;
040900111116        EXEC sql fetch next from a2 into :anmrrn, :fnanmds  ;
041000111116       else  ;
041100111116        sqlcod=100  ;
041200111116       endif  ;
041300110916
041400110916       enddo           ;
041500110916
041600110922       EXEC sql  CLOSE   a2                    ;
041700110916
041800110916       ENDSR     ;
041900110914       //-------------------------------------------------------------------
042000110914       BEGSR DecoAnom  ;
042100110914       clear tblkey ;
042200110914       eval tblkey=%editc(comcaa:'X')  ;
042300110914
042400110914       // decodifico tipo anomalia e causale chiusura
042500110914       chain (1:'7C':tblkey) tabel00f   ;
042600110914       if %found(tabel00f)  ;
042700110914       descaa =tbluni   ;
042800110914       endif   ;
042900110914       ENDSR  ;
043000110916       //-------------------------------------------------------------------
043100110916       BEGSR  carTFP                 ;
043200110916       eval yy=1  ;
043300110916
043400110916       dow lnpSel(YY) > 0  ;
043500110916       clear fnlv55ds ;
043600110916       d55tpt='P'  ;
043700110922       d55drf=dateu;
043800110916       d55lin=lnpSel(yy)   ;
043900110916       callp FNLV55R (fnlv55ds)  ;
044000110916
044100110916       tfplnp(yy)=(d55tfp*1000)+lnpsel(yy)  ;
044200110916       yy=yy+1  ;
044300110916
044400110916       enddo ;
044500110916       ENDSR  ;
044600080327       //-------------------------------------------------------------------
044700110916       BEGSR  Elabora115      ;
044800110913        wOK=' '  ;
044900110913
045000110913       // se presente numero spedizione chaino la bolla es eseguo control
045100110913       //  li delle parzializzazioni
0452001109131      if anmnsp>0  ;
045300110913        chain  (anmaas:anmlnp:anmnrs:anmnsp) fnblp01l  ;
0454001109132       if %found(fnblp01l)  ;
045500110913
045600110913        // controllo cliente e serie
0457001109133       if blpccm>0  ;
045800110913         wccm= blpccm  ;
045900110913        else  ;
046000110913         wccm=blpksc  ;
0461001109133       endif  ;
046200110913
046300110913        xx= 1 ;
046400110913
046500110913 3      dow xx<= 20  ;
046600110913 4      if lnp(xx) >0  ;
046700161221
046800161221        // Se ho richiesto l'unificante, carico tutti i codici
046900161221           clear tibs10ds ;
047000170112           wcod11=wccm      ;
047100161221
047200161221        if cli(xx)>0 and unif(xx)='U'  ;
047300161221
047400170112           d10tle='ST'  ;
047500161221           d10paf='F'    ;     // verifico se è padre
047600161221           d10cod=cli(xx)       ;
047700161221           TIBS10R (tibs10ds)   ;
047800161221  1        if d10err<>*blanks   ;
047900161221           // Se errore verifico se è figlio
048000161221              clear tibs10ds ;
048100170112              d10tle='ST'        ;
048200161221              d10paf='P'    ;
048300161221              d10cod=cli(xx)       ;
048400161221              TIBS10R (tibs10ds)   ;
048500161221           endif;
048600161221
048700161221  2        if d10err<>*blanks   ;
048800161221             // Imposto nella skiera e come padre solo se stesso
048900161221             skc(1)= '0000'+%editc(cli(xx):'X');
049000161221             d10cop=d10cod      ;
049100161221  2        endif                ;
049200161221        endif  ;
049300110913
049400161221 5      if cli(XX)=0 or wccm= cli(xx)
049500161221        or  %lookup(%editc(wcod11:'X'):skc)>0  ;
049600110913
049700110914        if   nrs(xx)='99'   ;
049800110914         WOK='S'  ;
049900110914          leavesr  ;
050000110914        endif   ;
050100110914
050200110913 6      if   nrs(XX)='SI' and blpnrs>0    ;
050300110913         WOK='S'  ;
050400110913          leavesr  ;
050500110913 x6     else  ;
050600110913
050700110913        alfanrs=%editc(blpnrs:'X')  ;
050800110913 7       if alfanrs= nrs(xx)  ;
050900110913         WOK='S'  ;
051000110913          leavesr  ;
051100110913 7       endif  ;
051200110913 6      endif  ;
051300110913
051400110913 5      endif  ;
051500110913 4      endif  ;
051600110913
051700110913        xx=xx+1  ;
051800110913 3      enddo  ;
051900110913
052000110913 2      endif  ;
052100110913 1      endif  ;
052200080327
052300080325       ENDSR                  ;
052400110916       //-------------------------------------------------------------------
052500110916       BEGSR  Elabora105      ;
052600110916        wOK=' '  ;
052700110916
052800110916        xx=1  ;
052900110916 1      dow   Wok=' '     ;
053000110922        xx=%lookup(anmfls:lnp:xx) ;
053100110916 2      if xx > 0   ;
053200110916          exsr CtrSerie  ;
053300110916          xx=xx+1   ;
053400110916         else  ;
053500110916          leave ;
053600110916 2      endif  ;
053700110916 1      enddo  ;
053800110916
053900110916 1      if wok=' ' and anmnrs>0   ;
054000110916         wlpsr=(anmfls*100)+anmnrs  ;
054100110916         y =1  ;
054200110916         Indx=%lookup(wlpsr:fas) ;
054300110916 2       if Indx>0    ;
054400110916
054500110916      /end-free
054600110916     C     Indx          OCCUR     DSLAS
054700110916     c                   seton                                        36
054800110916     C*
054900110916     C                   Z-ADD     1             YY
055000110916   3 C     *IN36         DOWEQ     *ON
055100110916     C     LAS(YY)       ANDGT     *ZEROS
055200110916     C                   MOVE      LAS(YY)       W0030
055300110916     C                   Z-ADD     1             Xx
055400110916   4 C                   DOW       Wok=' '
055500110916     C*
055600110922     c                   eval      xx  =%lookup(w0030:lnp:xx)
055700110916   5 c                   if        xx >0
055800110916     C                   EXSR      CtrSERIE
055900110916     c                   eval      xx=  xx + 1
056000110916  x5 c                   else
056100110916     c                   leave
056200110916   5 c                   endif
056300110916     c
056400110916   4 C                   ENDDO
056500110916     C*
056600110916     c                   if        wok='S'
056700110916     C                   SETOFF                                       36
056800110916     c                   endif
056900110916     c
057000110916     C                   ADD       1             YY
057100110916   3 C                   ENDDO
057200110916      /free
057300110916
057400110916   2   endif   ;
057500110916   1   endif   ;
057600110916
057700110916       ENDSR                  ;
057800110916       //-------------------------------------------------------------------
057900110916       BEGSR  ctrSerie               ;
058000110916       select ;
058100110916       when  nrs(XX)= '99' ;
058200110916       wok='S'  ;
058300110916
058400110916       when  nrs(XX)= 'SI'  ;
058500110916       if anmnrs>0  ;
058600110916         wok='S'  ;
058700110916       endif  ;
058800110916       other  ;
058900110916       alfanrs=%editc(anmnrs:'X')  ;
059000110916       if nrs(xx)=alfanrs  ;
059100110916         wok='S'  ;
059200110916       endif  ;
059300110916       endsl  ;
059400110916
059500161221       // se c'e' serie e cod cliente, verifico anche il range di segnacolli
059600161221
059700161221       if Wok='S'  and  anmnrs>0  and cli(xx)>0  and unif(xx)=' '  ;
059800111117          clear   trulnrsds  ;
059900111117          inrsCCM = cli(xx)  ;
060000111117          inrsNRS = anmnrs ;
060100111117          inrsFLS = anmfls ;
060200111117          inrsNCD = anmscn ;
060300111117          callp TRULNRSR (trulnrsds) ;
060400111117          if onrserr<>' '  ;
060500111117           clear wok  ;
060600111117          endif  ;
060700111117       endif  ;
060800161221
060900161221       if Wok='S'  and  anmnrs>0  and cli(xx)>0  and unif(xx)='U'  ;
061000161221       // carico gli unificanti
061100161221        // Se ho richiesto l'unificante, carico tutti i codici
061200161221           clear tibs10ds ;
061300161221           wcod11=cli(xx)   ;
061400161221
061500170112           d10tle='ST'  ;
061600161221           d10paf='F'    ;     // verifico se è padre
061700161221           d10cod=cli(xx)       ;
061800161221           TIBS10R (tibs10ds)   ;
061900161221  1        if d10err<>*blanks   ;
062000161221           // Se errore verifico se è figlio
062100161221              clear tibs10ds ;
062200170112              d10tle='ST'        ;
062300161221              d10paf='P'    ;
062400161221              d10cod=cli(xx)       ;
062500161221              TIBS10R (tibs10ds)   ;
062600161221           endif;
062700161221
062800161221  2        if d10err<>*blanks   ;
062900161221             // Imposto nella skiera e come padre solo se stesso
063000161221             skc(1)= '0000'+%editc(cli(xx):'X');
063100161221             d10cop=d10cod      ;
063200161221  2        endif                ;
063300161221
063400161221          ww=1   ;
063500161221          dow skc(ww) >*zeros   ;
063600161221          clear   trulnrsds  ;
063700161221          inrsCCM = %int(%subst(skc(ww):5:7))  ;
063800161221          inrsNRS = anmnrs ;
063900161221          inrsFLS = anmfls ;
064000161221          inrsNCD = anmscn ;
064100161221          callp TRULNRSR (trulnrsds) ;
064200161221          if onrserr= ' '  ;
064300161221           leave ;
064400161221          endif  ;
064500161221
064600161221          ww=ww+1  ;
064700161221          enddo  ;
064800161221
064900161221      // e non ne ho trovato nemmeno uno valido  -> errore sulla serie
065000161221          if onrserr<>' '    ;
065100161221           clear wok  ;
065200161221         endif  ;
065300161221       endif  ;
065400111117
065500111117       if Wok='S'   ;
065600110916       clear Indx   ;
065700110916       endif   ;
065800110916
065900110916       ENDSR                  ;
066000110913       //-------------------------------------------------------------------
066100110922       BEGSR  Scrivifile             ;
066200110926
066300110926       // Escludo le anomalie chiuse se non trasmesse
066400110926
066500110926       if anmdch=0 or anmdtr>0  ;
066600110926
066700111116       // Per gestione richiamo fnlr44r
066800111116       if parGEST='G'  ;
066900111116        exsr    RicLR44  ;
067000111116       else   ;
067100111116
067200111116       // Per invio wcrivo file di work
067300110913       clear wfanc000  ;
067400110913
067500110913       ancidute=knmeb   ;
067600111013       // anceut=knmus   ;
067700110913       ancepo=dutpou  ;
067800111013       //ancedt=dateu   ;
067900111013       //anceor=ora_u   ;
068000110913
068100110913       ancaas=anmaas  ;
068200110913       anclnp=anmlnp  ;
068300111013       ancfls=anmfls  ;
068400110913       ancnrs=anmnrs  ;
068500110913       ancnsp=anmnsp  ;
068600110913       anclna=anmlna  ;
068700110913       ancscn=anmscn  ;
068800110913       anccaa=anmcaa  ;
068900110914       ancdcaa=descaa   ;
069000111013       wdch=anmdch  ;
069100111013
069200111013       if anmdch>0  ;
069300111013         dataeur=%date(anmdch:*iso)    ;
069400111013         ancdch=%char(dataeur) ;
069500111013       endif  ;
069600111013
069700111013       wdao=anmdao  ;
069800111013       dataeur=%date(anmdao:*iso)    ;
069900111013       ancdao=%char(dataeur) ;
070000110914       if anmcch<>*blanks  ;
070100110914        clear tblkey ;
070200110914        tblkey=anmcch ;
070300110914        // decodifico tipo anomalia e causale chiusura
070400110914        chain (1:'7E':tblkey) tabel00f   ;
070500110914        if %found(tabel00f)  ;
070600110914         ancdcch=tbluni   ;
070700110914        endif   ;
070800110914       endif   ;
070900110913
071000110922       // solo per anomalia 115 e 105 chiusa
071100110913       ancrmn=blprmn   ;
071200110914       ancrma=blprma   ;
071300110913       ancccm=wccm     ;
071400111202       if wccm>0 and oggcli=0  ;
071500111202        oggcli=wccm   ;
071600111202       endif  ;
071700110916
071800110916       // Descrizione tipo servizio
071900110916       if blptsp<>*blanks  ;
072000110916        clear tblkey ;
072100110916        tblkey=blptsp ;
072200110916        chain (1:'5E':tblkey) tabel00f   ;
072300110916        if %found(tabel00f)  ;
072400110916         ds5e   =tbluni   ;
072500110916         ancdtsp=§5edes   ;
072600110916        endif   ;
072700110929        endif   ;
072800110916
072900110913       write  wfanc000 ;
073000110913
073100110926       // Aggiorno l'anomalia con la data trasmissione
073200111116       //  se  sono in Trasmissione
073300111116
073400111116 1     if parGEST='I'   ;
073500110926
073600111013       chain (ancfls:anclna:ancnrs:ancscn:anccaa:wdao) fnanm02l  ;
073700111116 2     if %found(fnanm02l) and a_anmcdu=anmcdu  ;
073800110929
073900111116 3       if anccaa=105 or a_anmnsp=ancnsp ;
074000110929
074100110929         // Se aperta aggiorno falg e data
074200111116 4        if wdch=0      ;
074300110929           a_anmdtr=dateu  ;
074400110929           a_anmft1='T'    ;
074500111116 x4       else  ;
074600111116 5          if a_anmft1=' ' ;
074700110929              a_anmdtr=dateu  ;
074800110929              a_anmft1='T'    ;
074900111116 5          endif  ;
075000111116 4        endif  ;
075100110929
075200110926          update fnanm000 ;
075300111116 3       endif  ;
075400111116 2     endif  ;
075500111116 1     endif  ;
075600110926
075700110926       endif  ;
075800111116       endif  ;
075900110926
076000110913       ENDSR                  ;
076100080327       //-------------------------------------------------------------------
076200080327       BEGSR  *INZSR                 ;
076300080328
076400080328       in(E) *dtaara   ;
076500080328       if    %error or rsut  =*blanks  ;
076600080328       callp TIBS34R (TIBS34DS)        ;
076700080328       in    *dtaara   ;
076800080328       endif                           ;
076900080829
077000110613       // imposto data e ora del giorno
077100110613       dataiso=datasys  ;
077200110613       dateu=%dec(dataiso)   ;
077300110616
077400110613       ora_iso=ora_sys  ;
077500110613       ora_u=%dec(ora_iso)   ;
077600080829
077700110913       // Se richiesto file pulisco il lancio precedente per filiale
077800110913       setll (dutpou)   wfanc01l   ;
077900110913       reade (dutpou)   wfanc01l   ;
078000110616
078100110913       dow  not %eof(wfanc01l)   ;
078200110913       delete wfanc000  ;
078300110616
078400110913       reade (dutpou)   wfanc01l   ;
078500110616       enddo  ;
078600110616
078700110616
078800080328       ENDSR                           ;
078900080317      /end-free
079000110916       //-------------------------------------------------------------------
079100110916     c     CARFAS        BEGSR
079200110916     c
079300110916     C     KTAB          KLIST
079400110916     C                   KFLD                    CODUT             1 0
079500110916     C                   KFLD                    COD               2
079600110916     C                   KFLD                    KEY               8
079700110916     C     KTAB2         KLIST
079800110916     C                   KFLD                    CODUT
079900110916     C                   KFLD                    COD
080000110916     c                   z-add     1             codut
080100110916     C***
080200110916     C* CARICO TABELLA FILIALI GESTITE da tfp
080300110916     c*
080400110916     c                   clear                   FAS
080500110916     c                   clear                   LAS
080600110916     C                   Z-ADD     1             X
080700110916     c*
080800110916     C                   MOVEL     '5F'          COD
080900110916     C                   MOVEL(P)  ktfp          KEY
081000110916     C                   MOVE      *ALL'9'       WMAX
081100110916     C                   MOVEL     ktfp          WMAX
081200110916     C     KTAB          SETLL     TABEL00F
081300110916     C     KTAB2         READE     TABEL00F                               31
081400110916     C     *IN31         DOWEQ     *OFF
081500110916     C     TBLKEY        ANDLE     WMAX
081600110916     C     TBLFLG        IFEQ      ' '
081700110916     C                   MOVE      TBLKEY        FAS(X)
081800110916     C     X             OCCUR     DSLAS
081900110916     C                   MOVEA     TBLUNI        LAS
082000110916     C                   ADD       1             X
082100110916    4C     X             IFGE      $MaxElem
082200110916     C                   SETON                                        31
082300110916    4C                   ENDIF
082400110916     C                   END
082500110916     C  N31KTAB2         READE     TABEL00F                               31
082600110916     C                   ENDDO
082700110916     c                   ENDSR
082800110916       //-------------------------------------------------------------------
082900110916     C     PrepaLP4      BEGSR
083000110916     C                   CLEAR                   LP4
083100110916     c                   move      tfplnp(ee)    lp4(1)
083200110916     C                   Z-ADD     2             Y
083300110916     c                   eval      zz=ee+1
083400110916     c* Se non è l'ultimo record letto, cerco altre lnp selezionate
083500110916     c*  per lo stesso terminal di partenza
083600110916    1c                   if        zz<=20
083700110916     c                   movel     tfplnp(zz)    elatfp            3 0
083800110916     c
083900110916    2c                   dow       zz<=20 and elatfp=ktfp
084000110916     c                   move      tfplnp(zz)    lp4(Y)
084100110916     c                   add       1             y
084200110916     c
084300110916     c                   add       1             zz
084400110916     c
084500110916    3c                   if        zz<=20
084600110916     c                   movel     tfplnp(zz)    elatfp
084700110916    3c                   endif
084800110916    2c                   enddo
084900110916     c
085000110916    1c                   endif
085100110916     c
085200110916     c
085300110916     C* DELLA SKIERA FAS CARICO SOLO QUELLE RELATIVE ALLE LINEE
085400110916     C*  ELABORATE
085500110916     C                   Z-ADD     1             X
085600110916     C     000           LOOKUP    LP4(X)                                 32
085700110916     C* ACCODO LA SKIERA FAS DELLE ALTRE FILIALI GESTITE
085800110916     C*  COME LNP SEGNACOLLO
085900110916    1C     *IN32         IFEQ      *ON
086000110916     C                   Z-ADD     1             Y
086100110916     C*
086200110916    2C     FAS(Y)        DOWGT     *ZEROS
086300110916     C     Y             ANDLE     $MaxElem
086400110916     C     Y             OCCUR     DSLAS
086500110916     C                   Z-ADD     1             YY
086600110916    3C     YY            DOWLE     87
086700110916     C     LAS(YY)       ANDGT     *ZEROS
086800110916     C                   MOVE      LAS(YY)       W0030
086900110916     C     W0030         LOOKUP    LP4                                    32
087000110916    4C     *IN32         IFEQ      *ON
087100110916     C                   MOVEL     FAS(Y)        W0030
087200110916     C     W0030         LOOKUP    LP4                                    32
087300110916     C  N32              z-add     W0030         LP4(X)
087400110916     C  N32              ADD       1             X
087500110916    4C                   END
087600110916     C                   ADD       1             YY
087700110916    3C                   ENDDO
087800110916     C                   ADD       1             Y
087900110916    2C                   ENDDO
088000110916     C*
088100110916    1C                   ENDIF
088200110916     C*
088300110916     C                   ENDSR
088400111116     c*----------------------------------------------------------------------
088500111116     c*  richiamo fnlr44r
088600111116     c*----------------------------------------------------------------------
088700111116     c     RicLR44       BEGSR
088800111116     c                   clear                   wuscit
088900111116     C                   CLEAR                   fnlr44ds
089000111116     C                   CLEAR                   fnlr34ds
089100111116     c
089200111116     C                   MOVEL     'P'           D34APA
089300111116     c                   if        anmrrn>0
089400111116     C                   Z-ADD     ANMrrn        D44NRR
089500111116     c                   else
089600111116     c                   eval      d34end='S'
089700111116     c                   endif
089800111116     c
089900111116     C                   MOVEL     fnanmds       D44ANM
090000111116     C                   MOVEL     'G'           D44MOD
090100111116     C*
090200111116     C                   MOVEL     fnlr44ds      KPJBU
090300111116     C                   CALL      'FNLR44R'
090400111116     C                   PARM                    KPJBA
090500111116     C                   PARM                    fnlr34ds
090600111116     c
090700111116     C                   MOVEL     KPJBU         fnlr44ds
090800111116     c
090900111116     C* SE PREMUTO F3 F12 O RILETTURA
091000111116    4C     D34F03        IFEQ      'S'
091100111116     C     D34F12        OREQ      'S'
091200111116     C     D34RIL        OREQ      'S'
091300111116     C                   MOVEL     'S'           WUSCIT            1
091400111116    3C                   ENDIF
091500111116     c
091600111116     C                   ENDSR
