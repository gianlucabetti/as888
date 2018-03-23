000100001006     H DECEDIT('0,') DATEDIT(*YMD.)
000200001006
000300001006      *****************************************************************
000400001006      *                                                               *
000500010605      *       STATISTICHE ORM                                         *
000600001006      *                                                               *
000700001006      *****************************************************************
000800001006
000900001010      ****************************************************************
001000001010      *  RIEPILOGO INDICATORI
001100001010      ****************************************************************
001200010618      * 06 - P.O. da elaborare protetto
001300010618      * 12 - Sono in sede
001400010705      * 13 - Utente EDP
001500001010      * 28 - ERRORE GENERICO DSPF
001600001010      * 30 - Comodo
001700010606      * 40 - ERRORE p.o. da elaborare
001800010618      * 41 - ERRORE data elaborazione da
001900010618      * 42 - ERRORE data elaborazione al
002000001010      ****************************************************************
002100001010
002200010605     fazorg01l  if   e           k disk
002300010618     ftabel00f  if   e           k disk
002400010618     fazcln01l  if   e           k disk
002500010605     ffior40d   cf   e             workstn
002600001010
002700001010      *------------------------------------------------------------------------*
002800010625     d worgfel         s                   like(orgfel)
002900010618
003000010618     d kcod            s                   like(tblcod)
003100010618     d kkey            s                   like(tblkey)
003200010618
003300010618     d ktfp            s                   like(clntfp)
003400010618     d ktfa            s                   like(clntfa)
003500010618     d kann            s                   like(clnann)
003600010618     d kmes            s                   like(clnmes)
003700030321
003800030321     d sav_g02tgi      s                   like(g02tgi)
003900010606
004000010618     d wcdtda          s              8  0
004100010618     d wcdtal          s              8  0
004200010618     d da_azorg        s                   like(orgfil)
004300001009
004400010625     d w0030           s              3  0
004500010625     d w0140           s             14  0
004600010605     d wdtgio          s              8  0
004700010618     d dataoggi        s              8  0
004800010622     d dtpuliaammgg    s              8  0
004900010622     d dtpuliggmmaa    s              8  0
005000010618     d wgiorno         s              2  0
005100010618     d wgiolav         s              5  0
005200010622     d datamsg         s             10
005300010705
005400010705     d wprofilo        s              3
005500010618
005600010618     d wdatada         s               d   datfmt(*iso)
005700010618     d wdataal         s               d   datfmt(*iso)
005800001006
005900001010      *  titolo videata (lunghezza massima 34)
006000010605     d tit_a           c                   const('***  Stampa Statistiche O.R.M-
006100010605     d                                     . ***')
006200001031
006300001031      *   S C H I E R E
006400010625     d msg             s             78    dim(9) ctdata perrcd(1)              msg video
006500001006
006600001010      *   D S   I N T E R N E / E S T E R N E
006700001009
006800010605     d wlbdat          ds                  inz
006900010605     d  g02dat                 1      8  0
007000010605     d  g02inv                 9     16  0
007100010605     d  g02err                17     17
007200010605     d  g02tgi                18     22  0
007300010618
007400010618     d wgidat          ds                  inz
007500010618     d  giodat                 1      8  0
007600010618     d  gioinv                 9     16  0
007700010618     d  giotgi                17     21  0
007800010618
007900010622     d datapul         ds                  inz
008000010622     d  giorno                 1      2
008100010622     d  mese                   3      4
008200010622     d  anno                   5      8
008300010622
008400010618     d clnmat          ds
008500010618     d  mat                    1     31    dim(31)
008600010618     d clnpom          ds
008700010618     d  pom                    1     31    dim(31)
008800001009
008900010605     d fior40ds      e ds
009000010618     d ds5as1        e ds
009100010618     d ds5a2         e ds
009200070418     d  §5astb2      e                     extfld(§5astb)
009300170322
009400170322      // - Reperimento dati utente
009500170322     d TIBS34DS      e ds
009600170322
009700170322      // - Reperimento filiali gestite dall'utente
009800170322     d TRUL31DS      e ds
009900170322     d  skpoel                10    759    dim(250)
010000001010
010100010605     d kpjba         e ds
010200001010      *
010300010605     d                sds
010400010605     d  vtcpgm                 1     10
010500170322
010600170322      //---------------------------------------------------------------
010700170322      //?Definizione aree dati.
010800170322      //---------------------------------------------------------------
010900170322      // - Dati utente
011000170322     d §AzUte        e ds                  extname(AZUTE00F)
011100170322     d                                     dtaara
011200170322     d §DatiUte      e ds                  extname(dDatiUte)
011300170322     d                                     dtaara
011400170322
011500170322      //---------------------------------------------------------------
011600170322      //?Definizione Prototipi.
011700170322      //---------------------------------------------------------------
011800170322      /copy gaitrasrc/srcprotopr,TIBS34R
011900170322
012000170322      // - Caricamento Filiali in gestione
012100170322     d TRUL31R         pr                  extpgm('TRUL31R')
012200170322     d  kpjba                              likeds(kpjba)
012300170322     d  trul31ds                           likeds(trul31ds)
012400001010
012500001006      *------------------------------------------------------------------------*
012600010108
012700010618      * emissione Videata
012800010108
012900010618      * pulizia Videata
013000010618     c                   if        *in28 = *off
013100010606     c                   exsr      Sr_Pulvid
013200010618     c                   endif
013300010606
013400010606     c                   do        *hival
013500010108
013600010605     c                   exfmt     or40d01
013700010108
013800010108      * F3=Fine
013900010605     c   kc              leave
014000010605     c                   setoff                                       28
014100010109
014200010618      * controlli
014300010605     c                   exsr      Sr_Contr
014400010109
014500010109      * F6=Elabora
014600010618     c                   if        *inkf = *on and *in28 = *off
014700010618     c                   exsr      Sr_Contr
014800010618     c  n28              exsr      Sr_Elabora
014900010618     c  n28              leave
015000010618     c                   endif
015100010108
015200010605     c   28              iter
015300010108
015400010605     c                   enddo
015500001010
015600010605     c                   eval      *inlr = *on
015700010108      *****************************************************************
015800010109      * PULIZIA VIDEATA
015900010108      *****************************************************************
016000010605     c     Sr_Pulvid     BEGSR
016100010108
016200010605     c                   clear                   v1cdtda
016300010605     c                   clear                   v1cdtal
016400010618     c                   eval      v1ctpst = 'A'
016500010620     c                   eval      v1cstpc = 'N'
016600010608     c                   eval      v1ctpel = 'S'
016700010705     c                   eval      v1cedp  = 'N'
016800010108
016900010605     c                   endsr
017000010108      **********************************************************************
017100010618      * CONTROLLO I DATI DELLA VIDEATA
017200010108      **********************************************************************
017300010618     c     Sr_Contr      begsr
017400010618
017500010618      * reset indicatori
017600010618     c                   setoff                                       404142
017700010622     c                   setoff                                       43
017800010618
017900010622      * p.o. da elaborare
018000010618     c                   exsr      Sr_Contrpo
018100010618     c                   if        *in28 = *on
018200010618     c                   goto      endsrcontr
018300010618     c                   endif
018400010618
018500010622      * date
018600010618     c                   exsr      Sr_Contrdate
018700010618     c                   if        *in28 = *on
018800010618     c                   goto      endsrcontr
018900010618     c                   endif
019000010622
019100010622      * richiesta lista di controllo
019200010622     c                   if        v1cpoel = 999 and
019300010622     c                             v1cstpc = 'S'
019400010622     c                   seton                                        43  28
019500010622     c                   movel     msg(9)        v1cmsg
019600010622     c                   goto      endsrcontr
019700010622     c                   endif
019800010618
019900010618     c     endsrcontr    endsr
020000010618      **********************************************************************
020100010618      * CONTROLLO IL P.O. DA ELABORARE
020200010618      **********************************************************************
020300010618     c     Sr_Contrpo    begsr
020400010108
020500010618     c                   setoff                                       40
020600010622     c                   clear                   v1dpoel
020700010622
020800010622     c                   if        v1cpoel = 999
020900010622     c                   clear                   v1dpoel
021000170322     c                   eval      v1dpoel = 'Tutte le filiali'
021100010622     c                   goto      endsrctpo
021200010622     c                   endif
021300010622
021400010622      * non deve essere a zero
021500010622     c                   if        v1cpoel = *zeros
021600010622     c                   seton                                        40  28
021700010622     c                   movel     msg(1)        v1cmsg
021800010622     c                   goto      endsrctpo
021900010622     c                   endif
022000010108
022100010625      * deve essere caricato nella sk
022200170322      /free
022300170322        IF  %lookup(%editc(V1Cpoel:'X'):skpoel) = 0;
022400170322      /end-free
022500010618     c                   seton                                        40  28
022600010618     c                   movel     msg(1)        v1cmsg
022700010618     c                   else
022800010108
022900010618      * decodifica e controlla la validità
023000010618     c     v1cpoel       chain     azorg01L
023100010618     c                   if        not%found(azorg01l) or
023200010618     c                             ORGfva <> *blanks
023300010618     c                   seton                                        40  28
023400010618     c                   movel     msg(1)        v1cmsg
023500010618     c                   else
023600010618     c                   if        orgfag <> 'A' and orgfag <> 'F'
023700010618     c                   seton                                        40  28
023800010618     c                   movel     msg(1)        v1cmsg
023900010618     c                   endif
024000010618     c                   movel     orgDES        v1dpoel
024100010618     c                   endif
024200010618     c                   endif
024300010108
024400010622     c     endsrctpo     endsr
024500010618      **********************************************************************
024600010618      * CONTROLLO LE DATE
024700010618      **********************************************************************
024800010618     c     Sr_Contrdate  begsr
024900010618
025000010618     c                   setoff                                       4142
025100010618
025200010618      * data 'da' deve essere impostata
025300010618     c                   if        v1cdtda = *zeros
025400010618     c                   seton                                        41  28
025500010618     c                   movel     msg(2)        v1cmsg
025600010618     c                   goto      endsrctdate
025700010618     c                   endif
025800010618
025900010618      * controllo la data inserita 'da'
026000010618     c                   clear                   wcdtda
026100010618     c                   clear                   wlbdat
026200010618     c                   z-add     v1cdtda       G02dat
026300010618     c                   call      'XSRDA8'
026400010618     c                   parm                    wlbdat
026500010618     c                   if        G02err = '1'
026600010618     c                   eval      *in41 = *ON
026700010618     c                   eval      *in28 = *ON
026800010618     c                   movel     msg(3)        v1cmsg
026900010618     c                   goto      endsrctdate
027000010618     c                   else
027100010618     c                   z-add     G02dat        v1cdtda
027200010618     c                   z-add     G02inv        wcdtda
027300010618     c                   endif
027400010618
027500010618      * non può essere >= ad oggi
027600010618     c                   if        wcdtda >= dataoggi
027700010618     c                   seton                                        41  28
027800010618     c                   movel     msg(4)        v1cmsg
027900010618     c                   goto      endsrctdate
028000010618     c                   endif
028100010618
028200010618      * deve essere > della data di pulizia ORM
028300010622     c                   if        wcdtda <= dtpuliaammgg
028400010618     c                   seton                                        41  28
028500010618     c                   movel     msg(5)        v1cmsg
028600010622     c                   movel     dtpuliggmmaa  datapul
028700010622     c                   eval      datamsg = %trim (giorno + '/' +
028800010622     c                             mese + '/' + anno)
028900010622     c                   eval      %subst(v1cmsg: 62: 10) = datamsg
029000010618     c                   goto      endsrctdate
029100010618     c                   endif
029200010618
029300010618      * deve essere un giorno feriale
029400010618     c     *iso          movel     wcdtda        wdatada
029500010618     c                   extrct    wdatada:*y    kann
029600010618     c                   extrct    wdatada:*m    kmes
029700010618     c                   extrct    wdatada:*d    wgiorno
029800011217     c                   clear                   ktfa
029900010618     c                   if        v1cpoel = 999
030000011217     c                   movel     simfel        ktfp
030100010618     c                   else
030200011217     c                   eval      ktfp = v1cpoel
030300010618     c                   endif
030400010618     c     kazcln        chain     azcln01l
030500010618     c                   if        %found(azcln01l) and
030600010618     c                             (MAT(wgiorno) =  'F'
030700010618     c                                    or  POM(wgiorno) =  'F')
030800010618     c                   seton                                        41  28
030900010622     c                   movel     msg(6)        v1cmsg
031000010618     c                   goto      endsrctdate
031100010618     c                   endif
031200010621      * BLOCCO
031300010621      * non deve essere inferiore a giugno 2001
031400010621     c                   if        kann = 2001 and kmes < 06
031500010621     c                   seton                                        41  28
031600010622     c                   movel     msg(8)        v1cmsg
031700010621     c                   goto      endsrctdate
031800010621     c                   endif
031900010618
032000010618      * data 'al' se non impostata è = a data 'da'
032100010618     c                   if        v1cdtal = *zeros
032200010618     c                   eval      v1cdtal = v1cdtda
032300010618     c                   eval      wcdtal = wcdtda
032400010618     c                   goto      endsrctdate
032500010618
032600010618     c                   else
032700010621
032800010618      * controllo la data inserita 'al'
032900010618     c                   clear                   wcdtal
033000010618     c                   clear                   wlbdat
033100010618     c                   z-add     v1cdtal       G02dat
033200010618     c                   call      'XSRDA8'
033300010618     c                   parm                    wlbdat
033400010618     c                   if        G02err = '1'
033500010618     c                   eval      *in42 = *ON
033600010618     c                   eval      *in28 = *ON
033700010622     c                   movel     msg(3)        v1cmsg
033800010618     c                   goto      endsrctdate
033900010618     c                   else
034000010618     c                   z-add     G02dat        v1cdtal
034100010618     c                   z-add     G02inv        wcdtal
034200010618     c                   endif
034300010618
034400010618      * non può essere >= ad oggi
034500030314      * tranne per sede
034600010618     c                   if        wcdtal >= dataoggi
034700030314     c                             and *in12 = *off
034800010618     c                   seton                                        42  28
034900010622     c                   movel     msg(4)        v1cmsg
035000010618     c                   goto      endsrctdate
035100010618     c                   endif
035200010618
035300010618      * deve essere > della data di pulizia ORM
035400010622     c                   if        wcdtal <= dtpuliaammgg
035500010618     c                   seton                                        42  28
035600010622     c                   movel     msg(5)        v1cmsg
035700010622     c                   movel     dtpuliggmmaa  datapul
035800010622     c                   eval      datamsg = %trim (giorno + '/' +
035900010622     c                             mese + '/' + anno)
036000010622     c                   eval      %subst(v1cmsg: 62: 10) = datamsg
036100010618     c                   goto      endsrctdate
036200010618     c                   endif
036300010621
036400010618      * deve essere un giorno feriale
036500010618     c     *iso          movel     wcdtal        wdataal
036600010618     c                   extrct    wdataal:*y    kann
036700010618     c                   extrct    wdataal:*m    kmes
036800010618     c                   extrct    wdataal:*d    wgiorno
036900011217     c                   clear                   ktfa
037000010618     c                   if        v1cpoel = 999
037100011217     c                   movel     simfel        ktfp
037200010618     c                   else
037300011217     c                   eval      ktfp = v1cpoel
037400010618     c                   endif
037500010618     c     kazcln        chain     azcln01l
037600010618     c                   if        %found(azcln01l) and
037700010618     c                             (MAT(wgiorno) =  'F'
037800010618     c                                    or  POM(wgiorno) =  'F')
037900010618     c                   seton                                        42  28
038000010622     c                   movel     msg(6)        v1cmsg
038100010618     c                   goto      endsrctdate
038200010618     c                   endif
038300010621
038400010621      * BLOCCO
038500010621      * non deve essere inferiore a giugno 2001
038600010621     c                   if        kann = 2001 and kmes < 06
038700010621     c                   seton                                        42  28
038800010622     c                   movel     msg(8)        v1cmsg
038900010621     c                   goto      endsrctdate
039000010621     c                   endif
039100010618
039200010618      * range
039300010618     c                   if        wcdtda > wcdtal
039400010618     c                   seton                                        41  28
039500010622     c                   movel     msg(3)        v1cmsg
039600010618     c                   goto      endsrctdate
039700010618     c                   endif
039800010618
039900010618      * range massimo di 31 gg
040000010726      * dal 26/07/01 range massimo di 7 gg (Bocchi)
040100030307      * 07/03/03 - NICOLA per sede il lancio deve essere possibile per 31 gg
040200010618     c     wdataal       subdur    wdatada       wgiolav:*d
040300010703     c                   eval      wgiolav = wgiolav + 1
040400030307     c                   if        wgiolav > 7 and Not *In12
040500010622     c                   movel     msg(7)        v1cmsg
040600030307     c                   Eval      %Subst(V1cmsg:50:2) = ' 7'
040700010618     c                   seton                                        41  28
040800010618     c                   goto      endsrctdate
040900010618     c                   endif
041000030307     c                   if        wgiolav > 31 and *In12
041100030307     c                   movel     msg(7)        v1cmsg
041200030307     c                   Eval      %Subst(V1cmsg:50:2) = '31'
041300030307     c                   seton                                        41  28
041400030307     c                   goto      endsrctdate
041500030307     c                   endif
041600010618
041700010618     c                   endif
041800010618
041900010618     c     endsrctdate   endsr
042000010606      **********************************************************************
042100010606      * ELABORA
042200010606      **********************************************************************
042300010606     C     Sr_Elabora    begsr
042400010606
042500010606     c                   eval      i40poel = v1cpoel
042600010606
042700010618     c                   eval      i40dtda = wcdtda
042800010618     c                   eval      i40dtal = wcdtal
042900010606
043000010606     c                   eval      i40tpel = v1ctpel
043100010620     c                   eval      i40stpc = v1cstpc
043200010618     c                   eval      i40tpst = v1ctpst
043300010705     c                   eval      i40edp  = v1cedp
043400010621
043500010621     c                   clear                   kpjbu
043600010621     c                   movel     fior40ds      kpjbu
043700010606
043800010618     c                   move      'OR41'        kcoaz
043900011217     c******             call      'FIOR41R'
044000030307     c                   If        *In12
044100090116     c                   eval      kqebi = 'KCEPICO '
044200030307     c                   call      'BCH09'
044300030307     c                   parm                    kpjba
044400030307     c                   EndIf
044500011217     c                   call      'BCH10'
044600030307     c                   parm                    kpjba
044700010606
044800010618     c                   endsr
044900001010      *****************************************************************
045000001010      * ROUTINE INIZIALE
045100001010      *****************************************************************
045200010618     c     *INZSR        begsr
045300001009
045400010618     c     *entry        plist
045500010618     c                   parm                    kpjba
045600010618
045700170322     c                   Z-ADD     1             CODUT             1 0
045800010618
045900010618     c     ktab          klist
046000010618     c                   kfld                    codut
046100010618     c                   kfld                    kcod
046200010618     c                   kfld                    kkey
046300010618
046400010618     c     kazcln        klist
046500010618     c                   kfld                    ktfp
046600010618     c                   kfld                    ktfa
046700010618     c                   kfld                    kann
046800010618     c                   kfld                    kmes
046900001011
047000010618     c                   movel     tit_a         vtctit
047100010618     c                   clear                   fior40ds
047200010625     c                   clear                   v1cpoel
047300010625     c                   clear                   v1dpoel
047400170322
047500170322      /free
047600170322       //?Reperimento dati job
047700170322         in(E) §AzUte;
047800170322         IF  NOT %error;
047900170322           in(E) §DatiUte;
048000170322         ENDIF;
048100170322         IF  %error or RSut = *blanks;
048200170322           clear TIBS34ds;
048300170322           tibs34r(tibs34ds);
048400170322           in §AzUte;
048500170322           in §DatiUte;
048600170322         ENDIF;
048700170322      /end-free
048800010618
048900010618      * da sede non imposto il p.o. da elaborare
049000010625     c                   IF        simfel = *zeros
049100010618     c                   eval      *in12 = *on
049200010618     c                   eval      i40sede = 'S'
049300010625      *
049400010625     c                   ELSE
049500010625      *
049600010618      * da filiale imposto il p.o. da elaborare
049700010618     c                   eval      i40sede = 'F'
049800010108
049900170322     c                   if        dutlpo = '2' or dutlpo = *blanks
050000170322     c                   movel     dutpou        V1Cpoel
050100020808     c                   eval      *in06 = *On
050200010618     c                   else
050300010618     c                   movel     SIMFEL        V1Cpoel
050400010618     c                   endif
050500010625      *
050600010625     c     V1Cpoel       chain     azorg01L
050700010625     c                   IF        not%found(azorg01l) or
050800010625     c                             orgfva <> *blanks   or
050900010625     c                             (orgfag <> 'A' and orgfag <> 'F')
051000010625     c                   eval      *in28 = *on
051100010625     c                   eval      *in40 = *on
051200010625     c                   movel     msg(1)        v1cmsg
051300010625     c                   ELSE
051400010618      * decodifico p.o. da elaborare
051500010618     c                   movel     orgDES        v1dpoel
051600010625     c                   ENDIF
051700010618     c                   endif
051800010705
051900010705      * se profilo EDP richiedo la stampa di controllo
052000010705     c                   movel     knmus         wprofilo
052100010705     c                   eval      *in13 = (wprofilo = 'EDP')
052200001009
052300001023      * reperisce data e ora
052400010618     c                   TIME                    W0140
052500001010      * UDATE IN GGMMAAAA
052600010618     c                   MOVE      W0140         WDTGIO
052700001010      * UDATE IN AAAAMMGG
052800010618     c                   Z-ADD     WDTGIO        G02DAT
052900010618     c                   MOVEL     *BLANK        G02ERR
053000010618     c                   CALL      'XSRDA8'
053100010618     c                   PARM                    WLBDAT
053200010618     c                   Z-ADD     G02INV        dataoggi
053300030321     c                   z-add     g02tgi        sav_g02tgi
053400010618
053500170322      /free
053600170322       //?Carico le filiali con abilitazione 'RC' Raggruppamento Centralino
053700170322       //?se utente di filiale
053800170322         IF  not *in12;
053900170322           clear TRUL31DS;
054000170322           I31abi = 'RC';
054100170322           I31cdi = DUTdis;
054200170322           I31car = DUTare;
054300170322           I31cpo = DUTpou;
054400170322           TRUL31R (kpjba:trul31ds);
054500170322           IF  O31pog <= *zeros;
054600170322             *in28 = *on;
054700170322             *in40 = *on;
054800170322             V1Cmsg = msg(1);
054900170322           ENDIF;
055000170322       //?Carico le tutte le filiali
055100170322       //?se utente di sede
055200170322         ELSE;
055300170322           clear TRUL31DS;
055400170322           I31abi = 'AZ';
055500170322           I31cdi = DUTdis;
055600170322           I31car = DUTare;
055700170322           I31cpo = DUTpou;
055800170322           TRUL31R (kpjba:trul31ds);
055900170322           IF  O31pog <= *zeros;
056000170322             *in28 = *on;
056100170322             *in40 = *on;
056200170322             V1Cmsg = msg(1);
056300170322           ENDIF;
056400170322         ENDIF;
056500170322      /end-free
056600010618
056700010618      * cerco data limite lancio elaborazione
056800010618      * per sede
056900010625     c                   IF        *in12 = *on
057000010618     c                   clear                   ds5as1
057100010618     c                   movel     '5A'          kcod
057200010618     c                   movel     'SEDE1'       kkey
057300010618     c     ktab          chain     tabel00f
057400010618     c                   if        %found(tabel00f) and tblflg = *blanks
057500010618     c                   movel     tbluni        ds5as1
057600010625     c                   z-add     §5aors        W0030
057700010618     c                   endif
057800010625      *
057900010625     c                   ELSE
058000010625      * per filiale
058100010625     c                   clear                   ds5a2
058200010625     c                   movel     '5A'          kcod
058300010625     c                   movel     '2       '    kkey
058400010625     c     ktab          chain     tabel00f
058500010625     c                   if        %found(tabel00f) and tblflg = *blanks
058600010625     c                   movel     tbluni        ds5a2
058700010625     c                   z-add     §5aorf        W0030
058800010625     c                   endif
058900010625      *
059000010625     c                   ENDIF
059100010618
059200010618      * Calcolo data
059300030321     c     sav_g02tgi    sub       W0030         giotgi
059400010618     c                   call      'XSRGI8'
059500010618     c                   parm                    wgidat
059600010622     c                   z-add     giodat        dtpuliggmmaa
059700010618
059800010618      * Giro la data
059900010622     c                   if        dtpuliggmmaa > 0
060000010622     c                   move      dtpuliggmmaa  g02dat
060100010618     c                   movel     *blanks       g02err
060200010618     c                   call      'XSRDA8'
060300010618     c                   parm                    wlbdat
060400010618     c                   if        g02err = '1'
060500010625     c                   clear                   dtpuliggmmaa
060600010618     c                   else
060700010622     c                   z-add     g02inv        dtpuliaammgg
060800010618     c                   endif
060900010618     c                   endif
061000030321
061100030321      * se sono in sede
061200030321      * cerco anche quella dei p.o. per elaborare solo gli ORM presenti nei p.o.
061300030321     c                   IF        *in12 = *on
061400030321     c                   clear                   ds5a2
061500030321     c                   movel     '5A'          kcod
061600030321     c                   movel     '2       '    kkey
061700030321     c     ktab          chain     tabel00f
061800030321     c                   if        %found(tabel00f) and tblflg = *blanks
061900030321     c                   movel     tbluni        ds5a2
062000030321     c                   z-add     §5aorf        W0030
062100030321     c                   endif
062200030321      * Calcolo data
062300030321     c     sav_g02tgi    sub       W0030         giotgi
062400030321     c                   call      'XSRGI8'
062500030321     c                   parm                    wgidat
062600030321     c                   z-add     giodat        i40dtpo
062700030321
062800030321      * Giro la data
062900030321     c                   if        i40dtpo > 0
063000030321     c                   move      i40dtpo       g02dat
063100030321     c                   movel     *blanks       g02err
063200030321     c                   call      'XSRDA8'
063300030321     c                   parm                    wlbdat
063400030321     c                   if        g02err = '1'
063500030321     c                   clear                   i40dtpo
063600030321     c                   else
063700030321     c                   z-add     g02inv        i40dtpo
063800030321     c                   endif
063900030321     c                   endif
064000030321     c                   endif
064100010618
064200010618     c                   endsr
064300001010      *---------------------------------------------------------------------------------------------
064400001031** MSG  Lungh. 78                                                            *
064500070418Filiale non gestita o annullata                                                     1
064600010622Inserire la data elaborazione                                                       2
064700010622Data elaborazione errata                                                            3
064800010622Data elaborazione deve essere inferiore alla data odierna                           4
064900010622Data elaborazione deve essere maggiore alla data pulizia ORM                        5
065000010622Data elaborazione deve essere un giorno feriale                                     6
065100030307Non è possibile elaborare un periodo superiore a xx giorni                          7
065200010622Data elaborazione deve essere maggiore del 31 Maggio 2001                           8
065300070418La lista di controllo può essere richiesta se si elabora una sola filiale           9
