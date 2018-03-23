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
003600080829     d
003700110913     D fnlsw1ds        DS
003800110913     D  COMdchd                1      8  0
003900110914     D  COMdcha                9     16  0
004000110914     D  LNP                   17     56P 0
004100110914     D                                     DIM(20)
004200110914     D  CLI                   57    136P 0
004300110914     D                                     DIM(20)
004400110914     D  NRS                  137    176
004500110914     D                                     DIM(20)
004600110914     D  videmail             177    201
004700111116     D  parGEST              202    202
004800110914     d
004900110914     d
005000110617     D  pInFILE        s             10
005100110617     D  pInIDJOB       s             26
005200110617     D  pInPWDXLS      s             15
005300110617     D  pInNOMXLS      s             78
005400111206     D  pInEMLVAR      s             80
005500110617     D  pInEMLDEST     s            121
005600110617     D  pOutESITO      s              1
005700111013     d wdao            s              8  0
005800111013     d wdch            s              8  0
005900111013     d dateu           s              8  0
006000110613     d ora_u           s              6  0
006100110913     d xx              s              3  0
006200110913     d yy              s              3  0
006300110916     d ee              s              3  0
006400110916     d zz              s              3  0
006500110916     d x               s              3  0
006600110916     d y               s              3  0
006700110913     d Indx            s              3  0
006800110916     d ktfp            s              3  0
006900110916     d savtfp          s              3  0
007000110913     d Wccm            s              7  0
007100111202     d oggcli          s              7  0
007200110916     d W0030           s              3  0
007300110916     d WLPSR           s              5  0
007400111116     d anmrrn          s              9  0
007500110913     d alfanrs         s              2
007600110913     d Primo           s              1
007700110913     d Wok             s              1
007800110914     d comcaa          s                   like(anmcaa)
007900110916     D WMAX            S                   LIKE(TBLKEY) inz
008000110914     d descaa          s             25
008100110613     d wlib            s             10    inz('GAITRAGRPS')
008200080828     d wrkgetlista     s           4096    varying
008300110613     d Datasys         s               d   datfmt(*iso)  inz(*sys)
008400110613     d Ora_sys         s               t   timfmt(*iso)  inz(*sys)
008500110613     d Ora_iso         s               t   timfmt(*iso)
008600110613     d Dataiso         s               d   datfmt(*iso)
008700111013     d Dataeur         s               d   datfmt(*eur)
008800080327     d Datadmy         s               d   datfmt(*dmy)
008900110916
009000110916     d $MaxElem        c                   const(998)
009100080829     d
009200080317      *------------------------------------------------------------------------*
009300080317      *   P R O T O T I P I
009400080317      *------------------------------------------------------------------------*
009500080829      /copy gaitrasrc/srcprotopr,tibs34r
009600110617      /copy gaitrasrc/srcprotopr,trtcm8r1
009700110916      /copy gaitrasrc/srcprotopr,fnlv55r
009800111116      /copy gaitrasrc/srcprotopr,fnlr44r
009900111117      /copy gaitrasrc/srcprotopr,trulnrs
010000001027     C*---------------------------------------------------------------*
010100001027     C     *ENTRY        PLIST
010200080325     C                   PARM                    kpjba
010300110913     c                   movel     kpjbu         fnlsw1ds
010400080317      /free
010500080828
010600110913       // Linee partenza selezionate
010700110913          EXSR  PrepaLNP      ;
010800110914
010900110913       // Prepara SQL
011000110914          EXSR SQLINV115      ;
011100080902
011200110913       //  Esegui SQL
011300110914          EXSR SQLEseg115   ;
011400110617
011500110916       // Carico i terminal per i manca record bolla
011600111201       //          eseguo solo se non ho premuto F12 in gestione
011700111201
011800111201   0      if  parGEST='I'  or wuscit=*blanks  ;
011900111201
012000110916          EXSR carTFP   ;
012100110916
012200110916          sorta tfplnp  ;
012300110916          eval ee= 1 ;
012400110916  1       dow  ee<=20  ;
012500110916
012600110916  2       if tfplnp(ee)>0  ;
012700110916          ktfp=tfplnp(ee)/1000  ;
012800110916  3       if ktfp<>savtfp  ;
012900110916
013000110916          // Carico tabella 5F confeabili dai terminal di linee selez.
013100110916          exsr   carfas   ;
013200110916
013300110916          // carico le linee da leggere più le confermabili dal relativo
013400110916          //   terminal partenza da tabella 5F
013500110916          EXSR   PrepaLP4    ;
013600110916
013700110916
013800110916       // Prepara SQL per anomalia manca record
013900110914          EXSR SQLINV105      ;
014000110914
014100110914       //  Esegui SQL
014200110914          EXSR SQLEseg105   ;
014300110916
014400110916        savtfp=ktfp   ;
014500110916 3      endif  ;
014600110916 2      endif  ;
014700110914
014800110916        ee=ee+1  ;
014900110916 1      enddo  ;
015000110914
015100111116        // se sono in gestione emetto videata
015200111206 1      if ParGEST='G'   ;
015300111116
015400111206 2      if   wuscit=*blanks  ;
015500111116          clear    anmrrn  ;
015600111116          exsr    RicLR44  ;
015700111206 2      endif   ;
015800111116
015900111206 x1     else  ;
016000111116
016100110617        // Se scritto almeno un record, invio all'indirizzo e-mail
016200110913          pInFILE='WFANC00F'   ;
016300110617          pInIDJOB=knmeb       ;
016400111202          // Nell'oggetto imposto il primo codice cliente richiesto
016500111206  2       if  oggcli>0   ;
016600111202          chain (1:dutkci:oggcli) cnaco00f  ;
016700111206  3       if not %found(cnaco00f)  ;
016800111202            acorag=*all'?'  ;
016900111206  3       endif  ;
017000111202
017100111207            pinEMLVAR='*OBJM* Anomalie Cliente: '+%editc(oggcli:'X')+ '-'+
017200111207                       %subst(acorag:1:15)  ;
017300111206  x2      else  ;
017400111202          // Imposto la prima lnp/serie richiesta   ;
017500111207           pinEMLVAR='*OBJM* Anomalie Lnp/Ser:'+%editc(lnp(1):'X')+' '+
017600111202                                         nrs(1)      ;
017700111206  2       endif  ;
017800111206
017900120305          pInEMLDEST=%trim(videmail)+'@brt.it' ;
018000110617          callp trtcm8r1 (pInfile:pInIDJOB:pInPWDXLS:pInNOMXLS:pInEMLDEST:
018100111202                          pInEMLVAR:pOutESITO) ;
018200080901
0183001112061       endif  ;
0184001112010       endif  ;
018500080829
018600080317        *inlr ='1'   ;
018700080828
018800110913       //-------------------------------------------------------------------
018900110913       BEGSR  PrepaLNP               ;
019000110913
019100110913       clear  LnpSel  ;
019200110913       xx= 1 ;
019300110913       yy= 1 ;
019400110913
019500110913       dow xx <=20  ;
019600110913        if lnp(xx)>0  ;
019700110913         Indx=%lookup(lnp(xx):LnpSel)  ;
019800110913         if Indx=0  ;
019900110913           eval lnpSel(yy)=lnp(xx)  ;
020000110913           yy=yy+1   ;
020100110913         endif  ;
020200110913
020300110913        endif  ;
020400110913
020500110913        xx=xx+1   ;
020600110913        enddo     ;
020700110913
020800111202        // Memorizzo nel campo oggcli il codice cliente che devo
020900111202        //  mettere nell'ogggetto della mail
021000111202        clear oggcli  ;
021100111202        if cli(1)>0   ;
021200111202          oggcli=cli(1)  ;
021300111202        endif  ;
021400111202
021500110913       ENDSR  ;
021600080901       //-------------------------------------------------------------------
021700110914       BEGSR  SQLINV115              ;
021800110913       clear primo  ;
021900110914       comcaa=115   ;
022000110913
022100110929        clear wrkgetlista  ;
022200111116        wrkgetlista ='select rrn(fnanm00f), fnanm00f.*                 +
022300111116                               from fnanm00f where anmcaa = ' +
022400110914                     %editc(comcaa:'X') +
022500110914                     ' and anmcch<>''AN'' and (anmdch=0'     ;
022600080901
022700110913        // NON ho richiesto anche le chiuse
022800110929        if comdchd=0    ;
022900110929           wrkgetlista=wrkgetlista + ')' ;
023000110929        else  ;
023100110929           wrkgetlista=wrkgetlista + ' or ('  ;
023200110929
023300110929        // Se richiesta elaborazione a oggi leggo anche
023400110929        //  anomalie chiuse ancora non trasmesse
023500110929
023600110929        if comdcha=dateu   ;
023700110929           wrkgetlista=%trim(wrkgetlista) +
023800110929                                    'anmdch>0 and anmft1='' '') or (';
023900110929           endif  ;
024000110929
024100110929           wrkgetlista=wrkgetlista + 'anmdch>0 and anmft1=''T'' and +
024200110929                                anmdtr>=' +%editc(comdchd:'X')+
024300110929                   ' and anmdtr<=' + %editc(comdcha:'X') +'))';
024400110929
024500110929        endif ;
024600110929
024700110913
024800110913        wrkgetlista =%trim(wrkgetlista) + ' and anmlnp in('  ;
024900110913        xx= 1   ;
025000110913        dow  xx <=20   ;
025100110913        if lnpSel(xx) > 0 ;
025200110913
025300110913        if Primo<>' '  ;
025400110913          wrkgetlista =%trim(wrkgetlista) + ','      ;
025500110913        endif  ;
025600110913
025700110913        wrkgetlista =%trim(wrkgetlista) + ' ' +%editc(lnpSel(xx):'X') ;
025800110913        eval Primo='S'  ;
025900110913        endif  ;
026000110913        xx=xx+1  ;
026100110913        enddo  ;
026200110913
026300111013        wrkgetlista =%trim(wrkgetlista) + ') order by anmdao'  ;
026400110913
026500080902
026600080901        ENDSR     ;
026700110914       //-------------------------------------------------------------------
026800110914       BEGSR  SQLINV105              ;
026900110914       clear primo  ;
027000110914       comcaa=105   ;
027100110916
027200110929        clear wrkgetlista  ;
027300111116        wrkgetlista ='select rrn(fnanm00f), fnanm00f.*                 +
027400111116                       from fnanm00f where anmcaa = ' +
027500110914                     %editc(comcaa:'X') +
027600110914                     ' and anmcch<>''AN'' and (anmdch=0'     ;
027700110914
027800110914        // NON ho richiesto anche le chiuse
027900110914        if comdchd=0    ;
028000110914           wrkgetlista=wrkgetlista + ')' ;
028100110914        else  ;
028200110929           wrkgetlista=wrkgetlista + ' or ('  ;
028300110929
028400110929        // Se richeista elaborazione a oggi leggo anche
028500110929        //  anomalie chiuse ancora non trasmesse
028600110929
028700110929        if comdcha=dateu   ;
028800110929           wrkgetlista=wrkgetlista + 'anmdch>0 and anmft1='' '') or (';
028900110929           endif  ;
029000110929
029100110929           wrkgetlista=wrkgetlista + 'anmdch>0 and anmft1=''T'' and +
029200110929                                anmdtr>=' +%editc(comdchd:'X')+
029300110929                   ' and anmdtr<=' + %editc(comdcha:'X') +'))';
029400110914
029500110914        endif ;
029600110914
029700110916        wrkgetlista =%trim(wrkgetlista) + ' and anmfls in('  ;
029800110914        xx= 1   ;
029900110916        dow  xx <=199  ;
030000110916        if LP4(xx) > 0 ;
030100110914
030200110914        if Primo<>' '  ;
030300110914          wrkgetlista =%trim(wrkgetlista) + ','      ;
030400110914        endif  ;
030500110914
030600110916        wrkgetlista =%trim(wrkgetlista) + ' ' +%editc(LP4(xx):'X') ;
030700110914        eval Primo='S'  ;
030800110914        endif  ;
030900110914        xx=xx+1  ;
031000110914        enddo  ;
031100110914
031200110914        wrkgetlista =%trim(wrkgetlista) + ')'  ;
031300110914
031400110916        // Selezione per ANMCDU
031500111013        wrkgetlista =%trim(wrkgetlista) + ' and anmcdu=' + %editc(ktfp:'X') +
031600111013         ' order by anmdao'  ;
031700110914
031800110914        ENDSR     ;
031900080829       //-------------------------------------------------------------------
032000110914       BEGSR  SQLEseg115           ;
032100110914
032200110914       exsr DecoAnom ;
032300110914
032400080828       EXEC sql  prepare s1 from : wrkgetlista ;
032500080828       EXEC sql  declare a1 cursor for s1      ;
032600080828       EXEC sql  open    a1                    ;
032700080828
032800111116       EXEC sql fetch next from a1 into :anmrrn, :fnanmds  ;
032900080828
033000080828       dow sqlcod=0    ;
033100080828
033200110916       exsr Elabora115 ;
033300110913
033400110913       // se record anomalia nelle selezioni fatte --> scrivo il file
033500110913       if   Wok='S'  ;
033600110913       exsr scrivifile  ;
033700110913       endif  ;
033800110913
033900111116       if wuscit=*blanks  ;
034000111116        EXEC sql fetch next from a1 into :anmrrn, :fnanmds  ;
034100111116       else  ;
034200111116
034300111116        sqlcod=100  ;
034400111116       endif ;
034500080829
034600080828       enddo           ;
034700080828
034800080829       EXEC sql  CLOSE   a1                    ;
034900080829
035000080828       ENDSR     ;
035100110916       //-------------------------------------------------------------------
035200110916       BEGSR  SQLEseg105           ;
035300110916
035400110916       exsr DecoAnom ;
035500110916
035600110916
035700110916       EXEC sql  prepare s2 from : wrkgetlista ;
035800110916       EXEC sql  declare a2 cursor for s2      ;
035900110916       EXEC sql  open    a2                    ;
036000110916
036100111116       EXEC sql fetch next from a2 into :anmrrn, :fnanmds  ;
036200110916
036300110916       dow sqlcod=0    ;
036400110916
036500110916       exsr Elabora105 ;
036600110916
036700111230       // Se serie 00 non la inviio/gestisco
036800111230
036900110916       // se record anomalia nelle selezioni fatte --> scrivo il file
037000111230       if   Wok='S'  and anmnrs>0  ;
037100111230
037200110922       // clearo i dati che non posso impostare nel file dalla bolla
037300110922       clear blprmn  ;
037400110922       clear blprma  ;
037500110922       clear blptsp  ;
037600110922       clear wccm    ;
037700110922
037800111013       // se si tratta di anomalia chiusa --> cerco la bolla
037900110922       if anmdch>0  and anmcch<>'AN'  ;
038000110922        chain  (anmfls:anmlna:anmnrs:anmscn) fnblt27l  ;
0381001109222         if %found(fnblt27l)  ;
038200110922           chain  (bltaas:bltlnp:bltnrs:bltnsp) fnblp01l  ;
0383001109222         anmaas=bltaas        ;
0384001109222         anmlnp=bltlnp       ;
0385001109222         anmnsp=bltnsp        ;
038600110922          endif  ;
038700110922       endif  ;
038800110922
038900110916       exsr scrivifile  ;
039000110916       endif  ;
039100110916
039200111116       if wuscit=*blanks  ;
039300111116        EXEC sql fetch next from a2 into :anmrrn, :fnanmds  ;
039400111116       else  ;
039500111116        sqlcod=100  ;
039600111116       endif  ;
039700110916
039800110916       enddo           ;
039900110916
040000110922       EXEC sql  CLOSE   a2                    ;
040100110916
040200110916       ENDSR     ;
040300110914       //-------------------------------------------------------------------
040400110914       BEGSR DecoAnom  ;
040500110914       clear tblkey ;
040600110914       eval tblkey=%editc(comcaa:'X')  ;
040700110914
040800110914       // decodifico tipo anomalia e causale chiusura
040900110914       chain (1:'7C':tblkey) tabel00f   ;
041000110914       if %found(tabel00f)  ;
041100110914       descaa =tbluni   ;
041200110914       endif   ;
041300110914       ENDSR  ;
041400110916       //-------------------------------------------------------------------
041500110916       BEGSR  carTFP                 ;
041600110916       eval yy=1  ;
041700110916
041800110916       dow lnpSel(YY) > 0  ;
041900110916       clear fnlv55ds ;
042000110916       d55tpt='P'  ;
042100110922       d55drf=dateu;
042200110916       d55lin=lnpSel(yy)   ;
042300110916       callp FNLV55R (fnlv55ds)  ;
042400110916
042500110916       tfplnp(yy)=(d55tfp*1000)+lnpsel(yy)  ;
042600110916       yy=yy+1  ;
042700110916
042800110916       enddo ;
042900110916       ENDSR  ;
043000080327       //-------------------------------------------------------------------
043100110916       BEGSR  Elabora115      ;
043200110913        wOK=' '  ;
043300110913
043400110913       // se presente numero spedizione chaino la bolla es eseguo control
043500110913       //  li delle parzializzazioni
0436001109131      if anmnsp>0  ;
043700110913        chain  (anmaas:anmlnp:anmnrs:anmnsp) fnblp01l  ;
0438001109132       if %found(fnblp01l)  ;
043900110913
044000110913        // controllo cliente e serie
0441001109133       if blpccm>0  ;
044200110913         wccm= blpccm  ;
044300110913        else  ;
044400110913         wccm=blpksc  ;
0445001109133       endif  ;
044600110913
044700110913        xx= 1 ;
044800110913
044900110913 3      dow xx<= 20  ;
045000110913 4      if lnp(xx) >0  ;
045100110913
045200110914 5      if cli(XX)=0 or wccm= cli(xx)  ;
045300110913
045400110914        if   nrs(xx)='99'   ;
045500110914         WOK='S'  ;
045600110914          leavesr  ;
045700110914        endif   ;
045800110914
045900110913 6      if   nrs(XX)='SI' and blpnrs>0    ;
046000110913         WOK='S'  ;
046100110913          leavesr  ;
046200110913 x6     else  ;
046300110913
046400110913        alfanrs=%editc(blpnrs:'X')  ;
046500110913 7       if alfanrs= nrs(xx)  ;
046600110913         WOK='S'  ;
046700110913          leavesr  ;
046800110913 7       endif  ;
046900110913 6      endif  ;
047000110913
047100110913 5      endif  ;
047200110913 4      endif  ;
047300110913
047400110913        xx=xx+1  ;
047500110913 3      enddo  ;
047600110913
047700110913 2      endif  ;
047800110913 1      endif  ;
047900080327
048000080325       ENDSR                  ;
048100110916       //-------------------------------------------------------------------
048200110916       BEGSR  Elabora105      ;
048300110916        wOK=' '  ;
048400110916
048500110916        xx=1  ;
048600110916 1      dow   Wok=' '     ;
048700110922        xx=%lookup(anmfls:lnp:xx) ;
048800110916 2      if xx > 0   ;
048900110916          exsr CtrSerie  ;
049000110916          xx=xx+1   ;
049100110916         else  ;
049200110916          leave ;
049300110916 2      endif  ;
049400110916 1      enddo  ;
049500110916
049600110916 1      if wok=' ' and anmnrs>0   ;
049700110916         wlpsr=(anmfls*100)+anmnrs  ;
049800110916         y =1  ;
049900110916         Indx=%lookup(wlpsr:fas) ;
050000110916 2       if Indx>0    ;
050100110916
050200110916      /end-free
050300110916     C     Indx          OCCUR     DSLAS
050400110916     c                   seton                                        36
050500110916     C*
050600110916     C                   Z-ADD     1             YY
050700110916   3 C     *IN36         DOWEQ     *ON
050800110916     C     LAS(YY)       ANDGT     *ZEROS
050900110916     C                   MOVE      LAS(YY)       W0030
051000110916     C                   Z-ADD     1             Xx
051100110916   4 C                   DOW       Wok=' '
051200110916     C*
051300110922     c                   eval      xx  =%lookup(w0030:lnp:xx)
051400110916   5 c                   if        xx >0
051500110916     C                   EXSR      CtrSERIE
051600110916     c                   eval      xx=  xx + 1
051700110916  x5 c                   else
051800110916     c                   leave
051900110916   5 c                   endif
052000110916     c
052100110916   4 C                   ENDDO
052200110916     C*
052300110916     c                   if        wok='S'
052400110916     C                   SETOFF                                       36
052500110916     c                   endif
052600110916     c
052700110916     C                   ADD       1             YY
052800110916   3 C                   ENDDO
052900110916      /free
053000110916
053100110916   2   endif   ;
053200110916   1   endif   ;
053300110916
053400110916       ENDSR                  ;
053500110916       //-------------------------------------------------------------------
053600110916       BEGSR  ctrSerie               ;
053700110916       select ;
053800110916       when  nrs(XX)= '99' ;
053900110916       wok='S'  ;
054000110916
054100110916       when  nrs(XX)= 'SI'  ;
054200110916       if anmnrs>0  ;
054300110916         wok='S'  ;
054400110916       endif  ;
054500110916       other  ;
054600110916       alfanrs=%editc(anmnrs:'X')  ;
054700110916       if nrs(xx)=alfanrs  ;
054800110916         wok='S'  ;
054900110916       endif  ;
055000110916       endsl  ;
055100110916
055200111117       // se c'e' serioe e cod cliente, verifico anche il range di segnacolli
055300111117       if Wok='S'  and  anmnrs>0  and cli(xx)>0  ;
055400111117          clear   trulnrsds  ;
055500111117          inrsCCM = cli(xx)  ;
055600111117          inrsNRS = anmnrs ;
055700111117          inrsFLS = anmfls ;
055800111117          inrsNCD = anmscn ;
055900111117          callp TRULNRSR (trulnrsds) ;
056000111117          if onrserr<>' '  ;
056100111117           clear wok  ;
056200111117          endif  ;
056300111117       endif  ;
056400111117
056500111117       if Wok='S'   ;
056600110916       clear Indx   ;
056700110916       endif   ;
056800110916
056900110916       ENDSR                  ;
057000110913       //-------------------------------------------------------------------
057100110922       BEGSR  Scrivifile             ;
057200110926
057300110926       // Escludo le anomalie chiuse se non trasmesse
057400110926
057500110926       if anmdch=0 or anmdtr>0  ;
057600110926
057700111116       // Per gestione richiamo fnlr44r
057800111116       if parGEST='G'  ;
057900111116        exsr    RicLR44  ;
058000111116       else   ;
058100111116
058200111116       // Per invio wcrivo file di work
058300110913       clear wfanc000  ;
058400110913
058500110913       ancidute=knmeb   ;
058600111013       // anceut=knmus   ;
058700110913       ancepo=dutpou  ;
058800111013       //ancedt=dateu   ;
058900111013       //anceor=ora_u   ;
059000110913
059100110913       ancaas=anmaas  ;
059200110913       anclnp=anmlnp  ;
059300111013       ancfls=anmfls  ;
059400110913       ancnrs=anmnrs  ;
059500110913       ancnsp=anmnsp  ;
059600110913       anclna=anmlna  ;
059700110913       ancscn=anmscn  ;
059800110913       anccaa=anmcaa  ;
059900110914       ancdcaa=descaa   ;
060000111013       wdch=anmdch  ;
060100111013
060200111013       if anmdch>0  ;
060300111013         dataeur=%date(anmdch:*iso)    ;
060400111013         ancdch=%char(dataeur) ;
060500111013       endif  ;
060600111013
060700111013       wdao=anmdao  ;
060800111013       dataeur=%date(anmdao:*iso)    ;
060900111013       ancdao=%char(dataeur) ;
061000110914       if anmcch<>*blanks  ;
061100110914        clear tblkey ;
061200110914        tblkey=anmcch ;
061300110914        // decodifico tipo anomalia e causale chiusura
061400110914        chain (1:'7E':tblkey) tabel00f   ;
061500110914        if %found(tabel00f)  ;
061600110914         ancdcch=tbluni   ;
061700110914        endif   ;
061800110914       endif   ;
061900110913
062000110922       // solo per anomalia 115 e 105 chiusa
062100110913       ancrmn=blprmn   ;
062200110914       ancrma=blprma   ;
062300110913       ancccm=wccm     ;
062400111202       if wccm>0 and oggcli=0  ;
062500111202        oggcli=wccm   ;
062600111202       endif  ;
062700110916
062800110916       // Descrizione tipo servizio
062900110916       if blptsp<>*blanks  ;
063000110916        clear tblkey ;
063100110916        tblkey=blptsp ;
063200110916        chain (1:'5E':tblkey) tabel00f   ;
063300110916        if %found(tabel00f)  ;
063400110916         ds5e   =tbluni   ;
063500110916         ancdtsp=§5edes   ;
063600110916        endif   ;
063700110929        endif   ;
063800110916
063900110913       write  wfanc000 ;
064000110913
064100110926       // Aggiorno l'anomalia con la data trasmissione
064200111116       //  se  sono in Trasmissione
064300111116
064400111116 1     if parGEST='I'   ;
064500110926
064600111013       chain (ancfls:anclna:ancnrs:ancscn:anccaa:wdao) fnanm02l  ;
064700111116 2     if %found(fnanm02l) and a_anmcdu=anmcdu  ;
064800110929
064900111116 3       if anccaa=105 or a_anmnsp=ancnsp ;
065000110929
065100110929         // Se aperta aggiorno falg e data
065200111116 4        if wdch=0      ;
065300110929           a_anmdtr=dateu  ;
065400110929           a_anmft1='T'    ;
065500111116 x4       else  ;
065600111116 5          if a_anmft1=' ' ;
065700110929              a_anmdtr=dateu  ;
065800110929              a_anmft1='T'    ;
065900111116 5          endif  ;
066000111116 4        endif  ;
066100110929
066200110926          update fnanm000 ;
066300111116 3       endif  ;
066400111116 2     endif  ;
066500111116 1     endif  ;
066600110926
066700110926       endif  ;
066800111116       endif  ;
066900110926
067000110913       ENDSR                  ;
067100080327       //-------------------------------------------------------------------
067200080327       BEGSR  *INZSR                 ;
067300080328
067400080328       in(E) *dtaara   ;
067500080328       if    %error or rsut  =*blanks  ;
067600080328       callp TIBS34R (TIBS34DS)        ;
067700080328       in    *dtaara   ;
067800080328       endif                           ;
067900080829
068000110613       // imposto data e ora del giorno
068100110613       dataiso=datasys  ;
068200110613       dateu=%dec(dataiso)   ;
068300110616
068400110613       ora_iso=ora_sys  ;
068500110613       ora_u=%dec(ora_iso)   ;
068600080829
068700110913       // Se richiesto file pulisco il lancio precedente per filiale
068800110913       setll (dutpou)   wfanc01l   ;
068900110913       reade (dutpou)   wfanc01l   ;
069000110616
069100110913       dow  not %eof(wfanc01l)   ;
069200110913       delete wfanc000  ;
069300110616
069400110913       reade (dutpou)   wfanc01l   ;
069500110616       enddo  ;
069600110616
069700110616
069800080328       ENDSR                           ;
069900080317      /end-free
070000110916       //-------------------------------------------------------------------
070100110916     c     CARFAS        BEGSR
070200110916     c
070300110916     C     KTAB          KLIST
070400110916     C                   KFLD                    CODUT             1 0
070500110916     C                   KFLD                    COD               2
070600110916     C                   KFLD                    KEY               8
070700110916     C     KTAB2         KLIST
070800110916     C                   KFLD                    CODUT
070900110916     C                   KFLD                    COD
071000110916     c                   z-add     1             codut
071100110916     C***
071200110916     C* CARICO TABELLA FILIALI GESTITE da tfp
071300110916     c*
071400110916     c                   clear                   FAS
071500110916     c                   clear                   LAS
071600110916     C                   Z-ADD     1             X
071700110916     c*
071800110916     C                   MOVEL     '5F'          COD
071900110916     C                   MOVEL(P)  ktfp          KEY
072000110916     C                   MOVE      *ALL'9'       WMAX
072100110916     C                   MOVEL     ktfp          WMAX
072200110916     C     KTAB          SETLL     TABEL00F
072300110916     C     KTAB2         READE     TABEL00F                               31
072400110916     C     *IN31         DOWEQ     *OFF
072500110916     C     TBLKEY        ANDLE     WMAX
072600110916     C     TBLFLG        IFEQ      ' '
072700110916     C                   MOVE      TBLKEY        FAS(X)
072800110916     C     X             OCCUR     DSLAS
072900110916     C                   MOVEA     TBLUNI        LAS
073000110916     C                   ADD       1             X
073100110916    4C     X             IFGE      $MaxElem
073200110916     C                   SETON                                        31
073300110916    4C                   ENDIF
073400110916     C                   END
073500110916     C  N31KTAB2         READE     TABEL00F                               31
073600110916     C                   ENDDO
073700110916     c                   ENDSR
073800110916       //-------------------------------------------------------------------
073900110916     C     PrepaLP4      BEGSR
074000110916     C                   CLEAR                   LP4
074100110916     c                   move      tfplnp(ee)    lp4(1)
074200110916     C                   Z-ADD     2             Y
074300110916     c                   eval      zz=ee+1
074400110916     c* Se non è l'ultimo record letto, cerco altre lnp selezionate
074500110916     c*  per lo stesso terminal di partenza
074600110916    1c                   if        zz<=20
074700110916     c                   movel     tfplnp(zz)    elatfp            3 0
074800110916     c
074900110916    2c                   dow       zz<=20 and elatfp=ktfp
075000110916     c                   move      tfplnp(zz)    lp4(Y)
075100110916     c                   add       1             y
075200110916     c
075300110916     c                   add       1             zz
075400110916     c
075500110916    3c                   if        zz<=20
075600110916     c                   movel     tfplnp(zz)    elatfp
075700110916    3c                   endif
075800110916    2c                   enddo
075900110916     c
076000110916    1c                   endif
076100110916     c
076200110916     c
076300110916     C* DELLA SKIERA FAS CARICO SOLO QUELLE RELATIVE ALLE LINEE
076400110916     C*  ELABORATE
076500110916     C                   Z-ADD     1             X
076600110916     C     000           LOOKUP    LP4(X)                                 32
076700110916     C* ACCODO LA SKIERA FAS DELLE ALTRE FILIALI GESTITE
076800110916     C*  COME LNP SEGNACOLLO
076900110916    1C     *IN32         IFEQ      *ON
077000110916     C                   Z-ADD     1             Y
077100110916     C*
077200110916    2C     FAS(Y)        DOWGT     *ZEROS
077300110916     C     Y             ANDLE     $MaxElem
077400110916     C     Y             OCCUR     DSLAS
077500110916     C                   Z-ADD     1             YY
077600110916    3C     YY            DOWLE     87
077700110916     C     LAS(YY)       ANDGT     *ZEROS
077800110916     C                   MOVE      LAS(YY)       W0030
077900110916     C     W0030         LOOKUP    LP4                                    32
078000110916    4C     *IN32         IFEQ      *ON
078100110916     C                   MOVEL     FAS(Y)        W0030
078200110916     C     W0030         LOOKUP    LP4                                    32
078300110916     C  N32              z-add     W0030         LP4(X)
078400110916     C  N32              ADD       1             X
078500110916    4C                   END
078600110916     C                   ADD       1             YY
078700110916    3C                   ENDDO
078800110916     C                   ADD       1             Y
078900110916    2C                   ENDDO
079000110916     C*
079100110916    1C                   ENDIF
079200110916     C*
079300110916     C                   ENDSR
079400111116     c*----------------------------------------------------------------------
079500111116     c*  richiamo fnlr44r
079600111116     c*----------------------------------------------------------------------
079700111116     c     RicLR44       BEGSR
079800111116     c                   clear                   wuscit
079900111116     C                   CLEAR                   fnlr44ds
080000111116     C                   CLEAR                   fnlr34ds
080100111116     c
080200111116     C                   MOVEL     'P'           D34APA
080300111116     c                   if        anmrrn>0
080400111116     C                   Z-ADD     ANMrrn        D44NRR
080500111116     c                   else
080600111116     c                   eval      d34end='S'
080700111116     c                   endif
080800111116     c
080900111116     C                   MOVEL     fnanmds       D44ANM
081000111116     C                   MOVEL     'G'           D44MOD
081100111116     C*
081200111116     C                   MOVEL     fnlr44ds      KPJBU
081300111116     C                   CALL      'FNLR44R'
081400111116     C                   PARM                    KPJBA
081500111116     C                   PARM                    fnlr34ds
081600111116     c
081700111116     C                   MOVEL     KPJBU         fnlr44ds
081800111116     c
081900111116     C* SE PREMUTO F3 F12 O RILETTURA
082000111116    4C     D34F03        IFEQ      'S'
082100111116     C     D34F12        OREQ      'S'
082200111116     C     D34RIL        OREQ      'S'
082300111116     C                   MOVEL     'S'           WUSCIT            1
082400111116    3C                   ENDIF
082500111116     c
082600111116     C                   ENDSR
