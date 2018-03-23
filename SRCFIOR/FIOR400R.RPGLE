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
002500151105     ffior400d  cf   e             workstn
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
004000010618     d xa              s              5i 0 inz(*zeros)                          ind.sk po da elab.
004100020808     d xb              s              5i 0 inz(*zeros)                          ind.sk po TRUL26
004200010618     d wcdtda          s              8  0
004300010618     d wcdtal          s              8  0
004400010618     d da_azorg        s                   like(orgfil)
004500001009
004600010625     d w0030           s              3  0
004700010625     d w0140           s             14  0
004800010605     d wdtgio          s              8  0
004900010618     d dataoggi        s              8  0
005000010622     d dtpuliaammgg    s              8  0
005100010622     d dtpuliggmmaa    s              8  0
005200010618     d wgiorno         s              2  0
005300010618     d wgiolav         s              5  0
005400010622     d datamsg         s             10
005500010705
005600010705     d wprofilo        s              3
005700010618
005800010618     d wdatada         s               d   datfmt(*iso)
005900010618     d wdataal         s               d   datfmt(*iso)
006000001006
006100001010      *  titolo videata (lunghezza massima 34)
006200010605     d tit_a           c                   const('***  Stampa Statistiche O.R.M-
006300010605     d                                     . ***')
006400001031
006500001031      *   S C H I E R E
006600020808     d skpoel          s              3  0 dim(250)                             po da elab.
006700010625     d msg             s             78    dim(9) ctdata perrcd(1)              msg video
006800010605     d l1              s              3  0 dim(30)                              p.o.gestiti
006900001006
007000001010      *   D S   I N T E R N E / E S T E R N E
007100001009
007200010605     d wlbdat          ds                  inz
007300010605     d  g02dat                 1      8  0
007400010605     d  g02inv                 9     16  0
007500010605     d  g02err                17     17
007600010605     d  g02tgi                18     22  0
007700010618
007800010618     d wgidat          ds                  inz
007900010618     d  giodat                 1      8  0
008000010618     d  gioinv                 9     16  0
008100010618     d  giotgi                17     21  0
008200010618
008300010622     d datapul         ds                  inz
008400010622     d  giorno                 1      2
008500010622     d  mese                   3      4
008600010622     d  anno                   5      8
008700010622
008800010618     d clnmat          ds
008900010618     d  mat                    1     31    dim(31)
009000010618     d clnpom          ds
009100010618     d  pom                    1     31    dim(31)
009200001009
009300151105     d fior400ds     e ds
009400010618     d ds5as1        e ds
009500010618     d ds5a2         e ds
009600070418     d  §5astb2      e                     extfld(§5astb)
009700010605     d ut§dse0f      e ds
009800010605     d cncr80        e ds
009900010108
010000020808     d trul26ds      e ds
010100020808     d  skpo                  11    760  0 dim(250)
010200020808     d  skpot                761   1510  0 dim(250)
010300001010
010400010605     d kpjba         e ds
010500001010      *
010600010605     d                sds
010700010605     d  vtcpgm                 1     10
010800001010
010900001006      *------------------------------------------------------------------------*
011000010108
011100010618      * emissione Videata
011200010108
011300010618      * pulizia Videata
011400010618     c                   if        *in28 = *off
011500010606     c                   exsr      Sr_Pulvid
011600010618     c                   endif
011700010606
011800010606     c                   do        *hival
011900010108
012000010605     c                   exfmt     or40d01
012100010108
012200010108      * F3=Fine
012300010605     c   kc              leave
012400010605     c                   setoff                                       28
012500010109
012600010618      * controlli
012700010605     c                   exsr      Sr_Contr
012800010109
012900010109      * F6=Elabora
013000010618     c                   if        *inkf = *on and *in28 = *off
013100010618     c                   exsr      Sr_Contr
013200010618     c  n28              exsr      Sr_Elabora
013300010618     c  n28              leave
013400010618     c                   endif
013500010108
013600010605     c   28              iter
013700010108
013800010605     c                   enddo
013900001010
014000010605     c                   eval      *inlr = *on
014100010108      *****************************************************************
014200010109      * PULIZIA VIDEATA
014300010108      *****************************************************************
014400010605     c     Sr_Pulvid     BEGSR
014500010108
014600010605     c                   clear                   v1cdtda
014700010605     c                   clear                   v1cdtal
014800010108
014900010605     c                   endsr
015000010108      **********************************************************************
015100010618      * CONTROLLO I DATI DELLA VIDEATA
015200010108      **********************************************************************
015300010618     c     Sr_Contr      begsr
015400010618
015500010618      * reset indicatori
015600010618     c                   setoff                                       404142
015700010622     c                   setoff                                       43
015800010618
015900010622      * p.o. da elaborare
016000010618     c                   exsr      Sr_Contrpo
016100010618     c                   if        *in28 = *on
016200010618     c                   goto      endsrcontr
016300010618     c                   endif
016400010618
016500010622      * date
016600010618     c                   exsr      Sr_Contrdate
016700010618     c                   if        *in28 = *on
016800010618     c                   goto      endsrcontr
016900010618     c                   endif
017000010622
017100010618
017200010618     c     endsrcontr    endsr
017300010618      **********************************************************************
017400010618      * CONTROLLO IL P.O. DA ELABORARE
017500010618      **********************************************************************
017600010618     c     Sr_Contrpo    begsr
017700010108
017800010618     c                   setoff                                       40
017900010622     c                   clear                   v1dpoel
018000010622
018100010622     c                   if        v1cpoel = 999
018200010622     c                   clear                   v1dpoel
018300010622     c                   eval      v1dpoel = 'Tutti i p.o. dell''AS'
018400010622     c                   goto      endsrctpo
018500010622     c                   endif
018600010622
018700010622      * non deve essere a zero
018800010622     c                   if        v1cpoel = *zeros
018900010622     c                   seton                                        40  28
019000010622     c                   movel     msg(1)        v1cmsg
019100010622     c                   goto      endsrctpo
019200010622     c                   endif
019300010108
019400010625      * deve essere caricato nella sk
019500010618     c     v1cpoel       lookup    skpoel                                 30
019600010618     c                   if        *in30 = *off
019700010618     c                   seton                                        40  28
019800010618     c                   movel     msg(1)        v1cmsg
019900010618     c                   else
020000010108
020100010618      * decodifica e controlla la validità
020200010618     c     v1cpoel       chain     azorg01L
020300010618     c                   if        not%found(azorg01l) or
020400010618     c                             ORGfva <> *blanks
020500010618     c                   seton                                        40  28
020600010618     c                   movel     msg(1)        v1cmsg
020700010618     c                   else
020800010618     c                   if        orgfag <> 'A' and orgfag <> 'F'
020900010618     c                   seton                                        40  28
021000010618     c                   movel     msg(1)        v1cmsg
021100010618     c                   endif
021200010618     c                   movel     orgDES        v1dpoel
021300010618     c                   endif
021400010618     c                   endif
021500010108
021600010622     c     endsrctpo     endsr
021700010618      **********************************************************************
021800010618      * CONTROLLO LE DATE
021900010618      **********************************************************************
022000010618     c     Sr_Contrdate  begsr
022100010618
022200010618     c                   setoff                                       4142
022300010618
022400010618      * data 'da' deve essere impostata
022500010618     c                   if        v1cdtda = *zeros
022600010618     c                   seton                                        41  28
022700010618     c                   movel     msg(2)        v1cmsg
022800010618     c                   goto      endsrctdate
022900010618     c                   endif
023000010618
023100010618      * controllo la data inserita 'da'
023200010618     c                   clear                   wcdtda
023300010618     c                   clear                   wlbdat
023400010618     c                   z-add     v1cdtda       G02dat
023500010618     c                   call      'XSRDA8'
023600010618     c                   parm                    wlbdat
023700010618     c                   if        G02err = '1'
023800010618     c                   eval      *in41 = *ON
023900010618     c                   eval      *in28 = *ON
024000010618     c                   movel     msg(3)        v1cmsg
024100010618     c                   goto      endsrctdate
024200010618     c                   else
024300010618     c                   z-add     G02dat        v1cdtda
024400010618     c                   z-add     G02inv        wcdtda
024500010618     c                   endif
024600010618
024700010618      * non può essere >= ad oggi
024800010618     c                   if        wcdtda >= dataoggi
024900010618     c                   seton                                        41  28
025000010618     c                   movel     msg(4)        v1cmsg
025100010618     c                   goto      endsrctdate
025200010618     c                   endif
025300010618
025400010618      * deve essere > della data di pulizia ORM
025500010622     c                   if        wcdtda <= dtpuliaammgg
025600010618     c                   seton                                        41  28
025700010618     c                   movel     msg(5)        v1cmsg
025800010622     c                   movel     dtpuliggmmaa  datapul
025900010622     c                   eval      datamsg = %trim (giorno + '/' +
026000010622     c                             mese + '/' + anno)
026100010622     c                   eval      %subst(v1cmsg: 62: 10) = datamsg
026200010618     c                   goto      endsrctdate
026300010618     c                   endif
026400010618
026500010618      * deve essere un giorno feriale
026600010618     c     *iso          movel     wcdtda        wdatada
026700010618     c                   extrct    wdatada:*y    kann
026800010618     c                   extrct    wdatada:*m    kmes
026900010618     c                   extrct    wdatada:*d    wgiorno
027000011217     c                   clear                   ktfa
027100010618     c                   if        v1cpoel = 999
027200011217     c                   movel     simfel        ktfp
027300010618     c                   else
027400011217     c                   eval      ktfp = v1cpoel
027500010618     c                   endif
027600010618     c     kazcln        chain     azcln01l
027700010618     c                   if        %found(azcln01l) and
027800010618     c                             (MAT(wgiorno) =  'F'
027900010618     c                                    or  POM(wgiorno) =  'F')
028000010618     c                   seton                                        41  28
028100010622     c                   movel     msg(6)        v1cmsg
028200010618     c                   goto      endsrctdate
028300010618     c                   endif
028400010621      * BLOCCO
028500010621      * non deve essere inferiore a giugno 2001
028600010621     c                   if        kann = 2001 and kmes < 06
028700010621     c                   seton                                        41  28
028800010622     c                   movel     msg(8)        v1cmsg
028900010621     c                   goto      endsrctdate
029000010621     c                   endif
029100010618
029200010618      * data 'al' se non impostata è = a data 'da'
029300010618     c                   if        v1cdtal = *zeros
029400010618     c                   eval      v1cdtal = v1cdtda
029500010618     c                   eval      wcdtal = wcdtda
029600010618     c                   goto      endsrctdate
029700010618
029800010618     c                   else
029900010621
030000010618      * controllo la data inserita 'al'
030100010618     c                   clear                   wcdtal
030200010618     c                   clear                   wlbdat
030300010618     c                   z-add     v1cdtal       G02dat
030400010618     c                   call      'XSRDA8'
030500010618     c                   parm                    wlbdat
030600010618     c                   if        G02err = '1'
030700010618     c                   eval      *in42 = *ON
030800010618     c                   eval      *in28 = *ON
030900010622     c                   movel     msg(3)        v1cmsg
031000010618     c                   goto      endsrctdate
031100010618     c                   else
031200010618     c                   z-add     G02dat        v1cdtal
031300010618     c                   z-add     G02inv        wcdtal
031400010618     c                   endif
031500010618
031600010618      * non può essere >= ad oggi
031700030314      * tranne per sede
031800010618     c                   if        wcdtal >= dataoggi
031900030314     c                             and *in12 = *off
032000010618     c                   seton                                        42  28
032100010622     c                   movel     msg(4)        v1cmsg
032200010618     c                   goto      endsrctdate
032300010618     c                   endif
032400010618
032500010618      * deve essere > della data di pulizia ORM
032600010622     c                   if        wcdtal <= dtpuliaammgg
032700010618     c                   seton                                        42  28
032800010622     c                   movel     msg(5)        v1cmsg
032900010622     c                   movel     dtpuliggmmaa  datapul
033000010622     c                   eval      datamsg = %trim (giorno + '/' +
033100010622     c                             mese + '/' + anno)
033200010622     c                   eval      %subst(v1cmsg: 62: 10) = datamsg
033300010618     c                   goto      endsrctdate
033400010618     c                   endif
033500010621
033600010618      * deve essere un giorno feriale
033700010618     c     *iso          movel     wcdtal        wdataal
033800010618     c                   extrct    wdataal:*y    kann
033900010618     c                   extrct    wdataal:*m    kmes
034000010618     c                   extrct    wdataal:*d    wgiorno
034100011217     c                   clear                   ktfa
034200010618     c                   if        v1cpoel = 999
034300011217     c                   movel     simfel        ktfp
034400010618     c                   else
034500011217     c                   eval      ktfp = v1cpoel
034600010618     c                   endif
034700010618     c     kazcln        chain     azcln01l
034800010618     c                   if        %found(azcln01l) and
034900010618     c                             (MAT(wgiorno) =  'F'
035000010618     c                                    or  POM(wgiorno) =  'F')
035100010618     c                   seton                                        42  28
035200010622     c                   movel     msg(6)        v1cmsg
035300010618     c                   goto      endsrctdate
035400010618     c                   endif
035500010621
035600010621      * BLOCCO
035700010621      * non deve essere inferiore a giugno 2001
035800010621     c                   if        kann = 2001 and kmes < 06
035900010621     c                   seton                                        42  28
036000010622     c                   movel     msg(8)        v1cmsg
036100010621     c                   goto      endsrctdate
036200010621     c                   endif
036300010618
036400010618      * range
036500010618     c                   if        wcdtda > wcdtal
036600010618     c                   seton                                        41  28
036700010622     c                   movel     msg(3)        v1cmsg
036800010618     c                   goto      endsrctdate
036900010618     c                   endif
037000010618
037100010618      * range massimo di 31 gg
037200010726      * dal 26/07/01 range massimo di 7 gg (Bocchi)
037300030307      * 07/03/03 - NICOLA per sede il lancio deve essere possibile per 31 gg
037400010618     c     wdataal       subdur    wdatada       wgiolav:*d
037500010703     c                   eval      wgiolav = wgiolav + 1
037600030307     c                   if        wgiolav > 7 and Not *In12
037700010622     c                   movel     msg(7)        v1cmsg
037800030307     c                   Eval      %Subst(V1cmsg:50:2) = ' 7'
037900010618     c                   seton                                        41  28
038000010618     c                   goto      endsrctdate
038100010618     c                   endif
038200030307     c                   if        wgiolav > 31 and *In12
038300030307     c                   movel     msg(7)        v1cmsg
038400030307     c                   Eval      %Subst(V1cmsg:50:2) = '31'
038500030307     c                   seton                                        41  28
038600030307     c                   goto      endsrctdate
038700030307     c                   endif
038800010618
038900010618     c                   endif
039000010618
039100010618     c     endsrctdate   endsr
039200010606      **********************************************************************
039300010606      * ELABORA
039400010606      **********************************************************************
039500010606     C     Sr_Elabora    begsr
039600010606
039700010606     c                   eval      i40poel = v1cpoel
039800010606
039900010618     c                   eval      i40dtda = wcdtda
040000010618     c                   eval      i40dtal = wcdtal
040100010621
040200010621     c                   clear                   kpjbu
040300151105     c                   movel     fior400ds     kpjbu
040400151106
040500151106     c                   IF        knmus = 'EDPMB'
040600151106     c                   call      'FIOR410C'
040700151106     c                   parm                    kpjba
040800151106     c                   ELSE
040900010606
041000151106     c                   move      'ORA1'        kcoaz
041100030307     c                   If        *In12
041200090116     c                   eval      kqebi = 'KCEPICO '
041300030307     c                   call      'BCH09'
041400030307     c                   parm                    kpjba
041500030307     c                   EndIf
041600011217     c                   call      'BCH10'
041700030307     c                   parm                    kpjba
041800151106     c                   ENDIF
041900010606
042000010618     c                   endsr
042100001010      *****************************************************************
042200001010      * ROUTINE INIZIALE
042300001010      *****************************************************************
042400010618     c     *INZSR        begsr
042500001009
042600010618     c     *entry        plist
042700010618     c                   parm                    kpjba
042800010618
042900010618     c                   Z-ADD     1             CODUT
043000010618     c                   CALL      'X§PARUT'
043100010618     c                   PARM                    UT§DSE0F
043200010618     c                   MOVEL     RAGUT         RSUT
043300010618     c                   MOVEL     REC80         CNCR80
043400010618
043500010618     c     ktab          klist
043600010618     c                   kfld                    codut
043700010618     c                   kfld                    kcod
043800010618     c                   kfld                    kkey
043900010618
044000010618     c     kazcln        klist
044100010618     c                   kfld                    ktfp
044200010618     c                   kfld                    ktfa
044300010618     c                   kfld                    kann
044400010618     c                   kfld                    kmes
044500001011
044600010618     c                   movel     tit_a         vtctit
044700151105     c                   clear                   fior400ds
044800010625     c                   clear                   v1cpoel
044900010625     c                   clear                   v1dpoel
045000010618
045100010618      * da sede non imposto il p.o. da elaborare
045200010625     c                   IF        simfel = *zeros
045300010618     c                   eval      *in12 = *on
045400010625      *
045500010625     c                   ELSE
045600010625      *
045700010618      * da filiale imposto il p.o. da elaborare
045800010108
045900020502     c                   if        simtpp = '2' or simtpp = *blanks
046000020502     c                   movel     simpou        V1Cpoel
046100020808     c                   eval      *in06 = *On
046200010618     c                   else
046300010618     c                   movel     SIMFEL        V1Cpoel
046400010618     c                   endif
046500010625      *
046600010625     c     V1Cpoel       chain     azorg01L
046700010625     c                   IF        not%found(azorg01l) or
046800010625     c                             orgfva <> *blanks   or
046900010625     c                             (orgfag <> 'A' and orgfag <> 'F')
047000010625     c                   eval      *in28 = *on
047100010625     c                   eval      *in40 = *on
047200010625     c                   movel     msg(1)        v1cmsg
047300010625     c                   ELSE
047400010618      * decodifico p.o. da elaborare
047500010618     c                   movel     orgDES        v1dpoel
047600010625     c                   ENDIF
047700010618     c                   endif
047800010705
047900010705      * se profilo EDP richiedo la stampa di controllo
048000010705     c                   movel     knmus         wprofilo
048100010705     c                   eval      *in13 = (wprofilo = 'EDP')
048200001009
048300001023      * reperisce data e ora
048400010618     c                   TIME                    W0140
048500001010      * UDATE IN GGMMAAAA
048600010618     c                   MOVE      W0140         WDTGIO
048700001010      * UDATE IN AAAAMMGG
048800010618     c                   Z-ADD     WDTGIO        G02DAT
048900010618     c                   MOVEL     *BLANK        G02ERR
049000010618     c                   CALL      'XSRDA8'
049100010618     c                   PARM                    WLBDAT
049200010618     c                   Z-ADD     G02INV        dataoggi
049300030321     c                   z-add     g02tgi        sav_g02tgi
049400010618
049500020808     c                   z-add     1             xa
049600010606
049700020808      * Ricerco tutti i terminal di partenza
049800020808     c                   Clear                   Trul26Ds
049900020808     c                   Eval      I26tpt = 'P'
050000020808     c                   Eval      I26tla = 'L'
050100020808     c                   Call      'TRUL26R'
050200020808     c                   Parm                    Trul26Ds
050300020808      * carico i p.o. da elaborare dell'AS in un sk
050400020808 b1  c                   Do        250           Xb
050500020808     c                   If        Skpot(Xb) = *Zeros
050600020808     c                   Leave
050700020808     c                   EndIf
050800020808      * Per la filiale devo caricare solo quelli che hanno il terminal di partenza = simfel
050900020808 b2  c                   If        Not *In12
051000020808     c                   If        Skpot(Xb) = Simfel
051100020808     c                   Eval      Skpoel(Xa) = Skpo(Xb)
051200020808     c                   Add       1             Xa
051300020808     c                   endif
051400020808      * Per la Sede se lancio tutti i p.o. carico tutti
051500020808 x2  c                   Else
051600020808     c                   Eval      Skpoel(Xb) = Skpo(Xb)
051700020808 e2  c                   EndIf
051800020808
051900020808 e1  c                   EndDo
052000010618
052100010618      * cerco data limite lancio elaborazione
052200010618      * per sede
052300010625     c                   IF        *in12 = *on
052400010618     c                   clear                   ds5as1
052500010618     c                   movel     '5A'          kcod
052600010618     c                   movel     'SEDE1'       kkey
052700010618     c     ktab          chain     tabel00f
052800010618     c                   if        %found(tabel00f) and tblflg = *blanks
052900010618     c                   movel     tbluni        ds5as1
053000010625     c                   z-add     §5aors        W0030
053100010618     c                   endif
053200010625      *
053300010625     c                   ELSE
053400010625      * per filiale
053500010625     c                   clear                   ds5a2
053600010625     c                   movel     '5A'          kcod
053700010625     c                   movel     '2       '    kkey
053800010625     c     ktab          chain     tabel00f
053900010625     c                   if        %found(tabel00f) and tblflg = *blanks
054000010625     c                   movel     tbluni        ds5a2
054100010625     c                   z-add     §5aorf        W0030
054200010625     c                   endif
054300010625      *
054400010625     c                   ENDIF
054500010618
054600010618      * Calcolo data
054700030321     c     sav_g02tgi    sub       W0030         giotgi
054800010618     c                   call      'XSRGI8'
054900010618     c                   parm                    wgidat
055000010622     c                   z-add     giodat        dtpuliggmmaa
055100010618
055200010618      * Giro la data
055300010622     c                   if        dtpuliggmmaa > 0
055400010622     c                   move      dtpuliggmmaa  g02dat
055500010618     c                   movel     *blanks       g02err
055600010618     c                   call      'XSRDA8'
055700010618     c                   parm                    wlbdat
055800010618     c                   if        g02err = '1'
055900010625     c                   clear                   dtpuliggmmaa
056000010618     c                   else
056100010622     c                   z-add     g02inv        dtpuliaammgg
056200010618     c                   endif
056300010618     c                   endif
056400030321
056500030321      * se sono in sede
056600030321      * cerco anche quella dei p.o. per elaborare solo gli ORM presenti nei p.o.
056700030321     c                   IF        *in12 = *on
056800030321     c                   clear                   ds5a2
056900030321     c                   movel     '5A'          kcod
057000030321     c                   movel     '2       '    kkey
057100030321     c     ktab          chain     tabel00f
057200030321     c                   if        %found(tabel00f) and tblflg = *blanks
057300030321     c                   movel     tbluni        ds5a2
057400030321     c                   z-add     §5aorf        W0030
057500030321     c                   endif
057600030321      * Calcolo data
057700030321     c     sav_g02tgi    sub       W0030         giotgi
057800030321     c                   call      'XSRGI8'
057900030321     c                   parm                    wgidat
058000030321     c                   z-add     giodat        i40dtpo
058100030321
058200030321      * Giro la data
058300030321     c                   if        i40dtpo > 0
058400030321     c                   move      i40dtpo       g02dat
058500030321     c                   movel     *blanks       g02err
058600030321     c                   call      'XSRDA8'
058700030321     c                   parm                    wlbdat
058800030321     c                   if        g02err = '1'
058900030321     c                   clear                   i40dtpo
059000030321     c                   else
059100030321     c                   z-add     g02inv        i40dtpo
059200030321     c                   endif
059300030321     c                   endif
059400030321     c                   endif
059500010618
059600010618     c                   endsr
059700001010      *---------------------------------------------------------------------------------------------
059800001031** MSG  Lungh. 78                                                            *
059900070418Filiale non gestita o annullata                                                     1
060000010622Inserire la data elaborazione                                                       2
060100010622Data elaborazione errata                                                            3
060200010622Data elaborazione deve essere inferiore alla data odierna                           4
060300010622Data elaborazione deve essere maggiore alla data pulizia ORM                        5
060400010622Data elaborazione deve essere un giorno feriale                                     6
060500030307Non è possibile elaborare un periodo superiore a xx giorni                          7
060600010622Data elaborazione deve essere maggiore del 31 Maggio 2001                           8
060700070418La lista di controllo può essere richiesta se si elabora una sola filiale           9
