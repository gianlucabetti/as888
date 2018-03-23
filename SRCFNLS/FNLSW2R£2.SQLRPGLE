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
005600120504     D  pInMRAKE2      s             15    inz
005700110617     D  pOutESITO      s              1
005800111013     d wdao            s              8  0
005900111013     d wdch            s              8  0
006000111013     d dateu           s              8  0
006100110613     d ora_u           s              6  0
006200110913     d xx              s              3  0
006300110913     d yy              s              3  0
006400110916     d ee              s              3  0
006500110916     d zz              s              3  0
006600110916     d x               s              3  0
006700110916     d y               s              3  0
006800110913     d Indx            s              3  0
006900110916     d ktfp            s              3  0
007000110916     d savtfp          s              3  0
007100110913     d Wccm            s              7  0
007200111202     d oggcli          s              7  0
007300110916     d W0030           s              3  0
007400110916     d WLPSR           s              5  0
007500111116     d anmrrn          s              9  0
007600110913     d alfanrs         s              2
007700110913     d Primo           s              1
007800110913     d Wok             s              1
007900110914     d comcaa          s                   like(anmcaa)
008000110916     D WMAX            S                   LIKE(TBLKEY) inz
008100110914     d descaa          s             25
008200110613     d wlib            s             10    inz('GAITRAGRPS')
008300080828     d wrkgetlista     s           4096    varying
008400110613     d Datasys         s               d   datfmt(*iso)  inz(*sys)
008500110613     d Ora_sys         s               t   timfmt(*iso)  inz(*sys)
008600110613     d Ora_iso         s               t   timfmt(*iso)
008700110613     d Dataiso         s               d   datfmt(*iso)
008800111013     d Dataeur         s               d   datfmt(*eur)
008900080327     d Datadmy         s               d   datfmt(*dmy)
009000110916
009100110916     d $MaxElem        c                   const(998)
009200080829     d
009300080317      *------------------------------------------------------------------------*
009400080317      *   P R O T O T I P I
009500080317      *------------------------------------------------------------------------*
009600080829      /copy gaitrasrc/srcprotopr,tibs34r
009700110617      /copy gaitrasrc/srcprotopr,trtcm8r1
009800110916      /copy gaitrasrc/srcprotopr,fnlv55r
009900111116      /copy gaitrasrc/srcprotopr,fnlr44r
010000111117      /copy gaitrasrc/srcprotopr,trulnrs
010100001027     C*---------------------------------------------------------------*
010200001027     C     *ENTRY        PLIST
010300080325     C                   PARM                    kpjba
010400110913     c                   movel     kpjbu         fnlsw1ds
010500080317      /free
010600080828
010700110913       // Linee partenza selezionate
010800110913          EXSR  PrepaLNP      ;
010900110914
011000110913       // Prepara SQL
011100110914          EXSR SQLINV115      ;
011200080902
011300110913       //  Esegui SQL
011400110914          EXSR SQLEseg115   ;
011500110617
011600110916       // Carico i terminal per i manca record bolla
011700111201       //          eseguo solo se non ho premuto F12 in gestione
011800111201
011900111201   0      if  parGEST='I'  or wuscit=*blanks  ;
012000111201
012100110916          EXSR carTFP   ;
012200110916
012300110916          sorta tfplnp  ;
012400110916          eval ee= 1 ;
012500110916  1       dow  ee<=20  ;
012600110916
012700110916  2       if tfplnp(ee)>0  ;
012800110916          ktfp=tfplnp(ee)/1000  ;
012900110916  3       if ktfp<>savtfp  ;
013000110916
013100110916          // Carico tabella 5F confeabili dai terminal di linee selez.
013200110916          exsr   carfas   ;
013300110916
013400110916          // carico le linee da leggere più le confermabili dal relativo
013500110916          //   terminal partenza da tabella 5F
013600110916          EXSR   PrepaLP4    ;
013700110916
013800110916
013900110916       // Prepara SQL per anomalia manca record
014000110914          EXSR SQLINV105      ;
014100110914
014200110914       //  Esegui SQL
014300110914          EXSR SQLEseg105   ;
014400110916
014500110916        savtfp=ktfp   ;
014600110916 3      endif  ;
014700110916 2      endif  ;
014800110914
014900110916        ee=ee+1  ;
015000110916 1      enddo  ;
015100110914
015200111116        // se sono in gestione emetto videata
015300111206 1      if ParGEST='G'   ;
015400111116
015500111206 2      if   wuscit=*blanks  ;
015600111116          clear    anmrrn  ;
015700111116          exsr    RicLR44  ;
015800111206 2      endif   ;
015900111116
016000111206 x1     else  ;
016100111116
016200110617        // Se scritto almeno un record, invio all'indirizzo e-mail
016300110913          pInFILE='WFANC00F'   ;
016400110617          pInIDJOB=knmeb       ;
016500111202          // Nell'oggetto imposto il primo codice cliente richiesto
016600111206  2       if  oggcli>0   ;
016700111202          chain (1:dutkci:oggcli) cnaco00f  ;
016800111206  3       if not %found(cnaco00f)  ;
016900111202            acorag=*all'?'  ;
017000111206  3       endif  ;
017100111202
017200111207            pinEMLVAR='*OBJM* Anomalie Cliente: '+%editc(oggcli:'X')+ '-'+
017300111207                       %subst(acorag:1:15)  ;
017400111206  x2      else  ;
017500111202          // Imposto la prima lnp/serie richiesta   ;
017600111207           pinEMLVAR='*OBJM* Anomalie Lnp/Ser:'+%editc(lnp(1):'X')+' '+
017700111202                                         nrs(1)      ;
017800111206  2       endif  ;
017900111206
018000120305          pInEMLDEST=%trim(videmail)+'@brt.it' ;
018100110617          callp trtcm8r1 (pInfile:pInIDJOB:pInPWDXLS:pInNOMXLS:pInEMLDEST:
018200120504                          pInEMLVAR:pInMRAKE2:pOutESITO) ;
018300080901
0184001112061       endif  ;
0185001112010       endif  ;
018600080829
018700080317        *inlr ='1'   ;
018800080828
018900110913       //-------------------------------------------------------------------
019000110913       BEGSR  PrepaLNP               ;
019100110913
019200110913       clear  LnpSel  ;
019300110913       xx= 1 ;
019400110913       yy= 1 ;
019500110913
019600110913       dow xx <=20  ;
019700110913        if lnp(xx)>0  ;
019800110913         Indx=%lookup(lnp(xx):LnpSel)  ;
019900110913         if Indx=0  ;
020000110913           eval lnpSel(yy)=lnp(xx)  ;
020100110913           yy=yy+1   ;
020200110913         endif  ;
020300110913
020400110913        endif  ;
020500110913
020600110913        xx=xx+1   ;
020700110913        enddo     ;
020800110913
020900111202        // Memorizzo nel campo oggcli il codice cliente che devo
021000111202        //  mettere nell'ogggetto della mail
021100111202        clear oggcli  ;
021200111202        if cli(1)>0   ;
021300111202          oggcli=cli(1)  ;
021400111202        endif  ;
021500111202
021600110913       ENDSR  ;
021700080901       //-------------------------------------------------------------------
021800110914       BEGSR  SQLINV115              ;
021900110913       clear primo  ;
022000110914       comcaa=115   ;
022100110913
022200110929        clear wrkgetlista  ;
022300111116        wrkgetlista ='select rrn(fnanm00f), fnanm00f.*                 +
022400111116                               from fnanm00f where anmcaa = ' +
022500110914                     %editc(comcaa:'X') +
022600110914                     ' and anmcch<>''AN'' and (anmdch=0'     ;
022700080901
022800110913        // NON ho richiesto anche le chiuse
022900110929        if comdchd=0    ;
023000110929           wrkgetlista=wrkgetlista + ')' ;
023100110929        else  ;
023200110929           wrkgetlista=wrkgetlista + ' or ('  ;
023300110929
023400110929        // Se richiesta elaborazione a oggi leggo anche
023500110929        //  anomalie chiuse ancora non trasmesse
023600110929
023700110929        if comdcha=dateu   ;
023800110929           wrkgetlista=%trim(wrkgetlista) +
023900110929                                    'anmdch>0 and anmft1='' '') or (';
024000110929           endif  ;
024100110929
024200110929           wrkgetlista=wrkgetlista + 'anmdch>0 and anmft1=''T'' and +
024300110929                                anmdtr>=' +%editc(comdchd:'X')+
024400110929                   ' and anmdtr<=' + %editc(comdcha:'X') +'))';
024500110929
024600110929        endif ;
024700110929
024800110913
024900110913        wrkgetlista =%trim(wrkgetlista) + ' and anmlnp in('  ;
025000110913        xx= 1   ;
025100110913        dow  xx <=20   ;
025200110913        if lnpSel(xx) > 0 ;
025300110913
025400110913        if Primo<>' '  ;
025500110913          wrkgetlista =%trim(wrkgetlista) + ','      ;
025600110913        endif  ;
025700110913
025800110913        wrkgetlista =%trim(wrkgetlista) + ' ' +%editc(lnpSel(xx):'X') ;
025900110913        eval Primo='S'  ;
026000110913        endif  ;
026100110913        xx=xx+1  ;
026200110913        enddo  ;
026300110913
026400111013        wrkgetlista =%trim(wrkgetlista) + ') order by anmdao'  ;
026500110913
026600080902
026700080901        ENDSR     ;
026800110914       //-------------------------------------------------------------------
026900110914       BEGSR  SQLINV105              ;
027000110914       clear primo  ;
027100110914       comcaa=105   ;
027200110916
027300110929        clear wrkgetlista  ;
027400111116        wrkgetlista ='select rrn(fnanm00f), fnanm00f.*                 +
027500111116                       from fnanm00f where anmcaa = ' +
027600110914                     %editc(comcaa:'X') +
027700110914                     ' and anmcch<>''AN'' and (anmdch=0'     ;
027800110914
027900110914        // NON ho richiesto anche le chiuse
028000110914        if comdchd=0    ;
028100110914           wrkgetlista=wrkgetlista + ')' ;
028200110914        else  ;
028300110929           wrkgetlista=wrkgetlista + ' or ('  ;
028400110929
028500110929        // Se richeista elaborazione a oggi leggo anche
028600110929        //  anomalie chiuse ancora non trasmesse
028700110929
028800110929        if comdcha=dateu   ;
028900110929           wrkgetlista=wrkgetlista + 'anmdch>0 and anmft1='' '') or (';
029000110929           endif  ;
029100110929
029200110929           wrkgetlista=wrkgetlista + 'anmdch>0 and anmft1=''T'' and +
029300110929                                anmdtr>=' +%editc(comdchd:'X')+
029400110929                   ' and anmdtr<=' + %editc(comdcha:'X') +'))';
029500110914
029600110914        endif ;
029700110914
029800110916        wrkgetlista =%trim(wrkgetlista) + ' and anmfls in('  ;
029900110914        xx= 1   ;
030000110916        dow  xx <=199  ;
030100110916        if LP4(xx) > 0 ;
030200110914
030300110914        if Primo<>' '  ;
030400110914          wrkgetlista =%trim(wrkgetlista) + ','      ;
030500110914        endif  ;
030600110914
030700110916        wrkgetlista =%trim(wrkgetlista) + ' ' +%editc(LP4(xx):'X') ;
030800110914        eval Primo='S'  ;
030900110914        endif  ;
031000110914        xx=xx+1  ;
031100110914        enddo  ;
031200110914
031300110914        wrkgetlista =%trim(wrkgetlista) + ')'  ;
031400110914
031500110916        // Selezione per ANMCDU
031600111013        wrkgetlista =%trim(wrkgetlista) + ' and anmcdu=' + %editc(ktfp:'X') +
031700111013         ' order by anmdao'  ;
031800110914
031900110914        ENDSR     ;
032000080829       //-------------------------------------------------------------------
032100110914       BEGSR  SQLEseg115           ;
032200110914
032300110914       exsr DecoAnom ;
032400110914
032500080828       EXEC sql  prepare s1 from : wrkgetlista ;
032600080828       EXEC sql  declare a1 cursor for s1      ;
032700080828       EXEC sql  open    a1                    ;
032800080828
032900111116       EXEC sql fetch next from a1 into :anmrrn, :fnanmds  ;
033000080828
033100080828       dow sqlcod=0    ;
033200080828
033300110916       exsr Elabora115 ;
033400110913
033500110913       // se record anomalia nelle selezioni fatte --> scrivo il file
033600110913       if   Wok='S'  ;
033700110913       exsr scrivifile  ;
033800110913       endif  ;
033900110913
034000111116       if wuscit=*blanks  ;
034100111116        EXEC sql fetch next from a1 into :anmrrn, :fnanmds  ;
034200111116       else  ;
034300111116
034400111116        sqlcod=100  ;
034500111116       endif ;
034600080829
034700080828       enddo           ;
034800080828
034900080829       EXEC sql  CLOSE   a1                    ;
035000080829
035100080828       ENDSR     ;
035200110916       //-------------------------------------------------------------------
035300110916       BEGSR  SQLEseg105           ;
035400110916
035500110916       exsr DecoAnom ;
035600110916
035700110916
035800110916       EXEC sql  prepare s2 from : wrkgetlista ;
035900110916       EXEC sql  declare a2 cursor for s2      ;
036000110916       EXEC sql  open    a2                    ;
036100110916
036200111116       EXEC sql fetch next from a2 into :anmrrn, :fnanmds  ;
036300110916
036400110916       dow sqlcod=0    ;
036500110916
036600110916       exsr Elabora105 ;
036700110916
036800111230       // Se serie 00 non la inviio/gestisco
036900111230
037000110916       // se record anomalia nelle selezioni fatte --> scrivo il file
037100111230       if   Wok='S'  and anmnrs>0  ;
037200111230
037300110922       // clearo i dati che non posso impostare nel file dalla bolla
037400110922       clear blprmn  ;
037500110922       clear blprma  ;
037600110922       clear blptsp  ;
037700110922       clear wccm    ;
037800110922
037900111013       // se si tratta di anomalia chiusa --> cerco la bolla
038000110922       if anmdch>0  and anmcch<>'AN'  ;
038100110922        chain  (anmfls:anmlna:anmnrs:anmscn) fnblt27l  ;
0382001109222         if %found(fnblt27l)  ;
038300110922           chain  (bltaas:bltlnp:bltnrs:bltnsp) fnblp01l  ;
0384001109222         anmaas=bltaas        ;
0385001109222         anmlnp=bltlnp       ;
0386001109222         anmnsp=bltnsp        ;
038700110922          endif  ;
038800110922       endif  ;
038900110922
039000110916       exsr scrivifile  ;
039100110916       endif  ;
039200110916
039300111116       if wuscit=*blanks  ;
039400111116        EXEC sql fetch next from a2 into :anmrrn, :fnanmds  ;
039500111116       else  ;
039600111116        sqlcod=100  ;
039700111116       endif  ;
039800110916
039900110916       enddo           ;
040000110916
040100110922       EXEC sql  CLOSE   a2                    ;
040200110916
040300110916       ENDSR     ;
040400110914       //-------------------------------------------------------------------
040500110914       BEGSR DecoAnom  ;
040600110914       clear tblkey ;
040700110914       eval tblkey=%editc(comcaa:'X')  ;
040800110914
040900110914       // decodifico tipo anomalia e causale chiusura
041000110914       chain (1:'7C':tblkey) tabel00f   ;
041100110914       if %found(tabel00f)  ;
041200110914       descaa =tbluni   ;
041300110914       endif   ;
041400110914       ENDSR  ;
041500110916       //-------------------------------------------------------------------
041600110916       BEGSR  carTFP                 ;
041700110916       eval yy=1  ;
041800110916
041900110916       dow lnpSel(YY) > 0  ;
042000110916       clear fnlv55ds ;
042100110916       d55tpt='P'  ;
042200110922       d55drf=dateu;
042300110916       d55lin=lnpSel(yy)   ;
042400110916       callp FNLV55R (fnlv55ds)  ;
042500110916
042600110916       tfplnp(yy)=(d55tfp*1000)+lnpsel(yy)  ;
042700110916       yy=yy+1  ;
042800110916
042900110916       enddo ;
043000110916       ENDSR  ;
043100080327       //-------------------------------------------------------------------
043200110916       BEGSR  Elabora115      ;
043300110913        wOK=' '  ;
043400110913
043500110913       // se presente numero spedizione chaino la bolla es eseguo control
043600110913       //  li delle parzializzazioni
0437001109131      if anmnsp>0  ;
043800110913        chain  (anmaas:anmlnp:anmnrs:anmnsp) fnblp01l  ;
0439001109132       if %found(fnblp01l)  ;
044000110913
044100110913        // controllo cliente e serie
0442001109133       if blpccm>0  ;
044300110913         wccm= blpccm  ;
044400110913        else  ;
044500110913         wccm=blpksc  ;
0446001109133       endif  ;
044700110913
044800110913        xx= 1 ;
044900110913
045000110913 3      dow xx<= 20  ;
045100110913 4      if lnp(xx) >0  ;
045200110913
045300110914 5      if cli(XX)=0 or wccm= cli(xx)  ;
045400110913
045500110914        if   nrs(xx)='99'   ;
045600110914         WOK='S'  ;
045700110914          leavesr  ;
045800110914        endif   ;
045900110914
046000110913 6      if   nrs(XX)='SI' and blpnrs>0    ;
046100110913         WOK='S'  ;
046200110913          leavesr  ;
046300110913 x6     else  ;
046400110913
046500110913        alfanrs=%editc(blpnrs:'X')  ;
046600110913 7       if alfanrs= nrs(xx)  ;
046700110913         WOK='S'  ;
046800110913          leavesr  ;
046900110913 7       endif  ;
047000110913 6      endif  ;
047100110913
047200110913 5      endif  ;
047300110913 4      endif  ;
047400110913
047500110913        xx=xx+1  ;
047600110913 3      enddo  ;
047700110913
047800110913 2      endif  ;
047900110913 1      endif  ;
048000080327
048100080325       ENDSR                  ;
048200110916       //-------------------------------------------------------------------
048300110916       BEGSR  Elabora105      ;
048400110916        wOK=' '  ;
048500110916
048600110916        xx=1  ;
048700110916 1      dow   Wok=' '     ;
048800110922        xx=%lookup(anmfls:lnp:xx) ;
048900110916 2      if xx > 0   ;
049000110916          exsr CtrSerie  ;
049100110916          xx=xx+1   ;
049200110916         else  ;
049300110916          leave ;
049400110916 2      endif  ;
049500110916 1      enddo  ;
049600110916
049700110916 1      if wok=' ' and anmnrs>0   ;
049800110916         wlpsr=(anmfls*100)+anmnrs  ;
049900110916         y =1  ;
050000110916         Indx=%lookup(wlpsr:fas) ;
050100110916 2       if Indx>0    ;
050200110916
050300110916      /end-free
050400110916     C     Indx          OCCUR     DSLAS
050500110916     c                   seton                                        36
050600110916     C*
050700110916     C                   Z-ADD     1             YY
050800110916   3 C     *IN36         DOWEQ     *ON
050900110916     C     LAS(YY)       ANDGT     *ZEROS
051000110916     C                   MOVE      LAS(YY)       W0030
051100110916     C                   Z-ADD     1             Xx
051200110916   4 C                   DOW       Wok=' '
051300110916     C*
051400110922     c                   eval      xx  =%lookup(w0030:lnp:xx)
051500110916   5 c                   if        xx >0
051600110916     C                   EXSR      CtrSERIE
051700110916     c                   eval      xx=  xx + 1
051800110916  x5 c                   else
051900110916     c                   leave
052000110916   5 c                   endif
052100110916     c
052200110916   4 C                   ENDDO
052300110916     C*
052400110916     c                   if        wok='S'
052500110916     C                   SETOFF                                       36
052600110916     c                   endif
052700110916     c
052800110916     C                   ADD       1             YY
052900110916   3 C                   ENDDO
053000110916      /free
053100110916
053200110916   2   endif   ;
053300110916   1   endif   ;
053400110916
053500110916       ENDSR                  ;
053600110916       //-------------------------------------------------------------------
053700110916       BEGSR  ctrSerie               ;
053800110916       select ;
053900110916       when  nrs(XX)= '99' ;
054000110916       wok='S'  ;
054100110916
054200110916       when  nrs(XX)= 'SI'  ;
054300110916       if anmnrs>0  ;
054400110916         wok='S'  ;
054500110916       endif  ;
054600110916       other  ;
054700110916       alfanrs=%editc(anmnrs:'X')  ;
054800110916       if nrs(xx)=alfanrs  ;
054900110916         wok='S'  ;
055000110916       endif  ;
055100110916       endsl  ;
055200110916
055300111117       // se c'e' serioe e cod cliente, verifico anche il range di segnacolli
055400111117       if Wok='S'  and  anmnrs>0  and cli(xx)>0  ;
055500111117          clear   trulnrsds  ;
055600111117          inrsCCM = cli(xx)  ;
055700111117          inrsNRS = anmnrs ;
055800111117          inrsFLS = anmfls ;
055900111117          inrsNCD = anmscn ;
056000111117          callp TRULNRSR (trulnrsds) ;
056100111117          if onrserr<>' '  ;
056200111117           clear wok  ;
056300111117          endif  ;
056400111117       endif  ;
056500111117
056600111117       if Wok='S'   ;
056700110916       clear Indx   ;
056800110916       endif   ;
056900110916
057000110916       ENDSR                  ;
057100110913       //-------------------------------------------------------------------
057200110922       BEGSR  Scrivifile             ;
057300110926
057400110926       // Escludo le anomalie chiuse se non trasmesse
057500110926
057600110926       if anmdch=0 or anmdtr>0  ;
057700110926
057800111116       // Per gestione richiamo fnlr44r
057900111116       if parGEST='G'  ;
058000111116        exsr    RicLR44  ;
058100111116       else   ;
058200111116
058300111116       // Per invio wcrivo file di work
058400110913       clear wfanc000  ;
058500110913
058600110913       ancidute=knmeb   ;
058700111013       // anceut=knmus   ;
058800110913       ancepo=dutpou  ;
058900111013       //ancedt=dateu   ;
059000111013       //anceor=ora_u   ;
059100110913
059200110913       ancaas=anmaas  ;
059300110913       anclnp=anmlnp  ;
059400111013       ancfls=anmfls  ;
059500110913       ancnrs=anmnrs  ;
059600110913       ancnsp=anmnsp  ;
059700110913       anclna=anmlna  ;
059800110913       ancscn=anmscn  ;
059900110913       anccaa=anmcaa  ;
060000110914       ancdcaa=descaa   ;
060100111013       wdch=anmdch  ;
060200111013
060300111013       if anmdch>0  ;
060400111013         dataeur=%date(anmdch:*iso)    ;
060500111013         ancdch=%char(dataeur) ;
060600111013       endif  ;
060700111013
060800111013       wdao=anmdao  ;
060900111013       dataeur=%date(anmdao:*iso)    ;
061000111013       ancdao=%char(dataeur) ;
061100110914       if anmcch<>*blanks  ;
061200110914        clear tblkey ;
061300110914        tblkey=anmcch ;
061400110914        // decodifico tipo anomalia e causale chiusura
061500110914        chain (1:'7E':tblkey) tabel00f   ;
061600110914        if %found(tabel00f)  ;
061700110914         ancdcch=tbluni   ;
061800110914        endif   ;
061900110914       endif   ;
062000110913
062100110922       // solo per anomalia 115 e 105 chiusa
062200110913       ancrmn=blprmn   ;
062300110914       ancrma=blprma   ;
062400110913       ancccm=wccm     ;
062500111202       if wccm>0 and oggcli=0  ;
062600111202        oggcli=wccm   ;
062700111202       endif  ;
062800110916
062900110916       // Descrizione tipo servizio
063000110916       if blptsp<>*blanks  ;
063100110916        clear tblkey ;
063200110916        tblkey=blptsp ;
063300110916        chain (1:'5E':tblkey) tabel00f   ;
063400110916        if %found(tabel00f)  ;
063500110916         ds5e   =tbluni   ;
063600110916         ancdtsp=§5edes   ;
063700110916        endif   ;
063800110929        endif   ;
063900110916
064000110913       write  wfanc000 ;
064100110913
064200110926       // Aggiorno l'anomalia con la data trasmissione
064300111116       //  se  sono in Trasmissione
064400111116
064500111116 1     if parGEST='I'   ;
064600110926
064700111013       chain (ancfls:anclna:ancnrs:ancscn:anccaa:wdao) fnanm02l  ;
064800111116 2     if %found(fnanm02l) and a_anmcdu=anmcdu  ;
064900110929
065000111116 3       if anccaa=105 or a_anmnsp=ancnsp ;
065100110929
065200110929         // Se aperta aggiorno falg e data
065300111116 4        if wdch=0      ;
065400110929           a_anmdtr=dateu  ;
065500110929           a_anmft1='T'    ;
065600111116 x4       else  ;
065700111116 5          if a_anmft1=' ' ;
065800110929              a_anmdtr=dateu  ;
065900110929              a_anmft1='T'    ;
066000111116 5          endif  ;
066100111116 4        endif  ;
066200110929
066300110926          update fnanm000 ;
066400111116 3       endif  ;
066500111116 2     endif  ;
066600111116 1     endif  ;
066700110926
066800110926       endif  ;
066900111116       endif  ;
067000110926
067100110913       ENDSR                  ;
067200080327       //-------------------------------------------------------------------
067300080327       BEGSR  *INZSR                 ;
067400080328
067500080328       in(E) *dtaara   ;
067600080328       if    %error or rsut  =*blanks  ;
067700080328       callp TIBS34R (TIBS34DS)        ;
067800080328       in    *dtaara   ;
067900080328       endif                           ;
068000080829
068100110613       // imposto data e ora del giorno
068200110613       dataiso=datasys  ;
068300110613       dateu=%dec(dataiso)   ;
068400110616
068500110613       ora_iso=ora_sys  ;
068600110613       ora_u=%dec(ora_iso)   ;
068700080829
068800110913       // Se richiesto file pulisco il lancio precedente per filiale
068900110913       setll (dutpou)   wfanc01l   ;
069000110913       reade (dutpou)   wfanc01l   ;
069100110616
069200110913       dow  not %eof(wfanc01l)   ;
069300110913       delete wfanc000  ;
069400110616
069500110913       reade (dutpou)   wfanc01l   ;
069600110616       enddo  ;
069700110616
069800110616
069900080328       ENDSR                           ;
070000080317      /end-free
070100110916       //-------------------------------------------------------------------
070200110916     c     CARFAS        BEGSR
070300110916     c
070400110916     C     KTAB          KLIST
070500110916     C                   KFLD                    CODUT             1 0
070600110916     C                   KFLD                    COD               2
070700110916     C                   KFLD                    KEY               8
070800110916     C     KTAB2         KLIST
070900110916     C                   KFLD                    CODUT
071000110916     C                   KFLD                    COD
071100110916     c                   z-add     1             codut
071200110916     C***
071300110916     C* CARICO TABELLA FILIALI GESTITE da tfp
071400110916     c*
071500110916     c                   clear                   FAS
071600110916     c                   clear                   LAS
071700110916     C                   Z-ADD     1             X
071800110916     c*
071900110916     C                   MOVEL     '5F'          COD
072000110916     C                   MOVEL(P)  ktfp          KEY
072100110916     C                   MOVE      *ALL'9'       WMAX
072200110916     C                   MOVEL     ktfp          WMAX
072300110916     C     KTAB          SETLL     TABEL00F
072400110916     C     KTAB2         READE     TABEL00F                               31
072500110916     C     *IN31         DOWEQ     *OFF
072600110916     C     TBLKEY        ANDLE     WMAX
072700110916     C     TBLFLG        IFEQ      ' '
072800110916     C                   MOVE      TBLKEY        FAS(X)
072900110916     C     X             OCCUR     DSLAS
073000110916     C                   MOVEA     TBLUNI        LAS
073100110916     C                   ADD       1             X
073200110916    4C     X             IFGE      $MaxElem
073300110916     C                   SETON                                        31
073400110916    4C                   ENDIF
073500110916     C                   END
073600110916     C  N31KTAB2         READE     TABEL00F                               31
073700110916     C                   ENDDO
073800110916     c                   ENDSR
073900110916       //-------------------------------------------------------------------
074000110916     C     PrepaLP4      BEGSR
074100110916     C                   CLEAR                   LP4
074200110916     c                   move      tfplnp(ee)    lp4(1)
074300110916     C                   Z-ADD     2             Y
074400110916     c                   eval      zz=ee+1
074500110916     c* Se non è l'ultimo record letto, cerco altre lnp selezionate
074600110916     c*  per lo stesso terminal di partenza
074700110916    1c                   if        zz<=20
074800110916     c                   movel     tfplnp(zz)    elatfp            3 0
074900110916     c
075000110916    2c                   dow       zz<=20 and elatfp=ktfp
075100110916     c                   move      tfplnp(zz)    lp4(Y)
075200110916     c                   add       1             y
075300110916     c
075400110916     c                   add       1             zz
075500110916     c
075600110916    3c                   if        zz<=20
075700110916     c                   movel     tfplnp(zz)    elatfp
075800110916    3c                   endif
075900110916    2c                   enddo
076000110916     c
076100110916    1c                   endif
076200110916     c
076300110916     c
076400110916     C* DELLA SKIERA FAS CARICO SOLO QUELLE RELATIVE ALLE LINEE
076500110916     C*  ELABORATE
076600110916     C                   Z-ADD     1             X
076700110916     C     000           LOOKUP    LP4(X)                                 32
076800110916     C* ACCODO LA SKIERA FAS DELLE ALTRE FILIALI GESTITE
076900110916     C*  COME LNP SEGNACOLLO
077000110916    1C     *IN32         IFEQ      *ON
077100110916     C                   Z-ADD     1             Y
077200110916     C*
077300110916    2C     FAS(Y)        DOWGT     *ZEROS
077400110916     C     Y             ANDLE     $MaxElem
077500110916     C     Y             OCCUR     DSLAS
077600110916     C                   Z-ADD     1             YY
077700110916    3C     YY            DOWLE     87
077800110916     C     LAS(YY)       ANDGT     *ZEROS
077900110916     C                   MOVE      LAS(YY)       W0030
078000110916     C     W0030         LOOKUP    LP4                                    32
078100110916    4C     *IN32         IFEQ      *ON
078200110916     C                   MOVEL     FAS(Y)        W0030
078300110916     C     W0030         LOOKUP    LP4                                    32
078400110916     C  N32              z-add     W0030         LP4(X)
078500110916     C  N32              ADD       1             X
078600110916    4C                   END
078700110916     C                   ADD       1             YY
078800110916    3C                   ENDDO
078900110916     C                   ADD       1             Y
079000110916    2C                   ENDDO
079100110916     C*
079200110916    1C                   ENDIF
079300110916     C*
079400110916     C                   ENDSR
079500111116     c*----------------------------------------------------------------------
079600111116     c*  richiamo fnlr44r
079700111116     c*----------------------------------------------------------------------
079800111116     c     RicLR44       BEGSR
079900111116     c                   clear                   wuscit
080000111116     C                   CLEAR                   fnlr44ds
080100111116     C                   CLEAR                   fnlr34ds
080200111116     c
080300111116     C                   MOVEL     'P'           D34APA
080400111116     c                   if        anmrrn>0
080500111116     C                   Z-ADD     ANMrrn        D44NRR
080600111116     c                   else
080700111116     c                   eval      d34end='S'
080800111116     c                   endif
080900111116     c
081000111116     C                   MOVEL     fnanmds       D44ANM
081100111116     C                   MOVEL     'G'           D44MOD
081200111116     C*
081300111116     C                   MOVEL     fnlr44ds      KPJBU
081400111116     C                   CALL      'FNLR44R'
081500111116     C                   PARM                    KPJBA
081600111116     C                   PARM                    fnlr34ds
081700111116     c
081800111116     C                   MOVEL     KPJBU         fnlr44ds
081900111116     c
082000111116     C* SE PREMUTO F3 F12 O RILETTURA
082100111116    4C     D34F03        IFEQ      'S'
082200111116     C     D34F12        OREQ      'S'
082300111116     C     D34RIL        OREQ      'S'
082400111116     C                   MOVEL     'S'           WUSCIT            1
082500111116    3C                   ENDIF
082600111116     c
082700111116     C                   ENDSR
