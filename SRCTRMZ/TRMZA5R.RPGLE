000100981019     H DEBUG DECEDIT('0,') DATEDIT(*DMY.)
000200000000     H*--------------------------------------------------------------*
000300160212     Ftrmza5d   CF   E             wORKSTN usropn
000400030630     Faitra02l  if   e           k disk
000500090625     Faitrs01l  if   e           k disk
000600160209     Faitrc01l  if   e           k disk
000700050623     Fazorg01l  if   e           k disk    rename(azorg:azorg1)
000800050622     Fazorg02l  if   e           k disk
000900160209     Fwaitramf  uf a e           k disk    usropn
001000050801     ftabel00f  if   e           k disk
001100050805     Fqsysprt   O    F  132        PRINTER prtctl(prtds)
001200020304     dkpjba          e ds
001300030630     d Tibs36Ds      e ds
001400081204     D tibs02ds      E DS
001500160210     D trmz70s1ds    E DS
001600160210     D dtrsflr1      E DS
001700160210     D dtrafiller    E DS
001800081204
001900160210     d param           ds
002000160210     d   parfil                       3
002100020404     d prtds           ds
002200020404     d   spab                         3  0
002300020404     d   spaa                         3  0
002400020404     d   skab                         3  0
002500020404     d   skaa                         3  0
002600020404     d   line                         3  0
002700160211     d alert1          s              1
002800160211     d alert2          s              1
002900160211     d alert3          s              1
003000160211     d alert4          s              1
003100160210     d anom7           s              1
003200160210     d anom15          s              1
003300050623     d kfil            s                   like(orgfil)
003400050803     d exfil           s                   like(orgfil)
003500160211     d extrc           s                   like(tranrc)
003600160210     d datal15         s              8  0
003700160210     d datal7          s              8  0
003800160210     d dataisow        s               d   datfmt(*iso)
003900160210     d dataiso         s               d   datfmt(*iso)
004000160210     d dataeur         s               d   datfmt(*eur)
004100160210     d datadurc        s               d   datfmt(*eur)
004200160210     d dataunil        s               d   datfmt(*eur)
004300160210     d datarps         s               d   datfmt(*eur)
004400160210     d datasps         s               d   datfmt(*eur)
004500050802     d coman           s             66
004600050802     d luncmd          s             15  5
004700050802     D cmd1            S              1    DIM(66) CTDATA PERRCD(66)
004800050802     D cmd2            S              1    DIM(66) CTDATA PERRCD(66)
004900050802     D cmd3            S              1    DIM(66) CTDATA PERRCD(66)
005000000000     C*---------------------------------------------------------------*
005100020304      *
005200160212     c                   if        param = *blank
005300160212     c                   open      trmza5d
005400160212     c                   endif
005500160212      *
005600031105     c                   do        *hival
005700160210     c                   if        param = *blank
005800031105     c                   exfmt     video1
005900031105     c                   clear                   $msg
006000031105     c   kc              leave
006100031105     c                   exsr      controv1
006200031105     c   96              iter
006300160210     c                   else
006400160212     c                   movel     parfil        v1cfil
006500160210     c                   eval      *inkf = '1'
006600160210     c                   endif
006700160210      *
006800050623     c                   if        *inkf
006900050802      *clear file
007000050802     c                   movea     cmd1          coman
007100050802     c                   exsr      comandi
007200050623     c                   exsr      elabora
007300050805     c                   leave
007400050623     c                   endif
007500050805     c                   enddo
007600050802     c                   exsr      waitra
007700050802      *elimina file in qtemp x la filiale
007800051017     c                   movea     cmd3          coman
007900051017     c                   exsr      comandi
008000050802
008100020308     c                   seton                                        lr
008200050622     c*-------------------------------------------------
008300050622     c     elabora       begsr
008400050622     c*-------------------------------------------------
008500160210     c                   if        v1cfil <> 046
008600050623     c                   move      v1cfil        kfil
008700050622     c                   exsr      aitra
008800050622     c                   else
008900160210     c                   clear                   kfil
009000050622     c                   exsr      aitra
009100160211     c                   endif
009200050622     c                   endsr
009300990113     c*-------------------------------------------------
009400030630     c     aitra         begsr
009500990113     c*-------------------------------------------------
009600050623     c     kfil          setll     aitra02l
009700981029     c                   do        *hival
009800160209     c                   read      aitra02l                               98
009900031105     c   98              leave
010000160209     c                   if        kfil > 0 and trafil <> kfil
010100160209     C                   leave
010200160209     C                   endif
010300160210      *
010400160211     c                   if        traann <> *blank or trasco <> *blank or
010500160210     c                             tracor <> *blank
010600160210     c                   iter
010700160210     c                   endif
010800160210      *
010900160209     c                   movel     trafiller     dtrafiller
011000160212     c                   clear                   tradurc
011100160212     c                   clear                   traivas
011200090625     c     tranrc        chain     aitrs01l
011300090625     c                   if        %found(aitrs01l)
011400160209     c                   movel     trsflr1       dtrsflr1
011500160210     c                   if        §trsddurc <> *blank
011600160209     c                   move      §trsddurc     tradurc
011700160217     c                   move      tradurc       dataiso
011800160218     c                   adddur    4:*m          dataiso
011900160217     c                   move      dataiso       tradurc
012000160209     c                   endif
012100090625     c                   endif
012200160210     c                   if        §traivac <> *blank
012300160210     c                   movel     §traivac      traivas
012400160209     c     ktrc          chain     aitrc01l
012500160217     c                   if        %found(aitrc01l) and trcddurc > 0
012600160217     c                   move      trcddurc      dataiso
012700160218     c                   adddur    4:*m          dataiso
012800160217     c                   move      dataiso       tradurc
012900160209     c                   endif
013000160209     c                   endif
013100160217     c                   if        traitaest <> 'I' and
013200160503     c                             tradtrps > 0 and tradtrps < 20391231
013300160210     c                   move      TRADTRPS      dataiso
013400160511     c                   adddur    12:*m         dataiso
013500160210     c                   move      dataiso       tradtrps
013600160210     c                   endif
013700050801      * scrive W.F
013800160209     c     trafil        chain     azorg01l
013900050801     c                   move      orgcar        traarea
014000050801     c                   move      orgfl3        tradist
014100160210     C                   exsr      RagSocCont
014200160210     c                   eval      trasocl = RagSocKsc
014300160209     c                   write     waitram
014400050623     c                   enddo
014500050623
014600020304     c                   endsr
014700981029      *
014800050802     c*-------------------------------------------------
014900050802     c     waitra        begsr
015000050802     c*-------------------------------------------------
015100160209     c     *loval        setll     waitramf
015200050802     c                   do        *hival
015300160209     c                   read      waitramf                               98
015400050802     c   98              leave
015500160210     c                   clear                   anom7
015600160210     c                   clear                   anom15
015700160211     c                   clear                   alert1
015800160211     c                   clear                   alert2
015900160211     c                   clear                   alert3
016000160211     c                   clear                   alert4
016100160212     c                   clear                   datadurc
016200160212     c                   clear                   dataunil
016300160212     c                   clear                   datarps
016400160212     c                   clear                   datasps
016500160212     c                   clear                   dataiso
016600160212      * autista
016700160212     c                   if        trakau > 0
016800160212     c                   z-add     trakau        codaut            7 0
016900160212     c                   else
017000160212     c                   z-add     trabpt        codaut
017100160212     c                   endif
017200050803      * rottura filiale
017300050803     c                   if        trafil  <> exfil
017400160212     c     trafil        chain     azorg01l
017500160211     c                   except    testa
017600160210     c                   except    testafil
017700050803     c                   move      trafil        exfil
017800160210     c                   endif
017900050803      *
018000050802     c                   if        line > 60
018100160211     c                   except    testa
018200160210     c                   except    testafil
018300050802     c                   endif
018400160211      * controllo riga fornitore con limiti 7 o 15 giorni per data DURC
018500160211     c                   if        tranrc  <> extrc
018600160211     c                   z-add     tranrc        extrc
018700160217     c                   if        tradurc > 0 and tradurc > wdatg
018800160217      *scade entro 7g
018900160211     c                   if        tradurc < datal7
019000160210     c                   movel     tradurc       dataiso
019100160210     c                   move      dataiso       datadurc
019200160211     c                   move      'B'           alert1
019300160210     c                   except    for7
019400160211     c                   else
019500160217      *scade entro 15g
019600160211     c                   if        tradurc < datal15
019700160217     c                   movel     tradurc       dataiso
019800160210     c                   move      dataiso       datadurc
019900160211     c                   move      'A'           alert1
020000160210     c                   except    for15
020100160211     c                   endif
020200160211     c                   endif
020300160211     c                   else
020400160217      *scaduto
020500160217     c                   if        tradurc > 0
020600160217     c                   movel     tradurc       dataiso
020700160217     c                   move      dataiso       datadurc
020800160217     c                   else
020900160217      *zero
021000160211     c                   clear                   datadurc
021100160217     c                   endif
021200160211     c                   move      'C'           alert1
021300160211     c                   except    for7
021400160211     c                   endif
021500160211     c                   endif
021600160217      * controllo riga autista se presente campo traivas trattasi di consorziata
021700160212     c                   setoff                                       30
021800160212     c                   if        traivas <> *blank
021900160217     c                   if        tradurc > 0 and tradurc > wdatg
022000160512     c                   movel     tradurc       dataiso
022100160512     c                   move      dataiso       datadurc
022200160212     c                   if        tradurc < datal7
022300160212     c                   movel     tradurc       dataiso
022400160212     c                   move      dataiso       datadurc
022500160212     c                   move      'B'           alert1
022600160212     c                   move      'X'           anom7
022700160512     c                   seton                                        30
022800160212     c                   else
022900160212     c                   if        tradurc < datal15
023000160212     c                   movel     tradurc       dataiso
023100160212     c                   move      dataiso       datadurc
023200160212     c                   move      'A'           alert1
023300160212     c                   move      'X'           anom15
023400160512     c                   seton                                        30
023500160212     c                   endif
023600160212     c                   endif
023700160212     c                   else
023800160217     c                   if        tradurc > 0
023900160217     c                   movel     tradurc       dataiso
024000160217     c                   move      dataiso       datadurc
024100160217     c                   else
024200160217     c                   clear                   datadurc
024300160217     c                   endif
024400160218     c                   move      'C'           alert1
024500160212     c                   move      'X'           anom7
024600160512     c                   seton                                        30
024700160212     c                   endif
024800160212     c                   endif
024900160217      * dipendente
025000160212     c                   setoff                                       31
0251001605061    c                   if        tradipa <> 'A'
025200160506     c                   if        traunil <> 99991231
0253001602172    c                   if        traunil > 0 and traunil > wdatg
0254001602123    c                   if        traunil < datal7
025500160210     c                   movel     traunil       dataiso
025600160210     c                   move      dataiso       dataunil
025700160211     c                   move      'B'           alert2
025800160211     c                   move      'X'           anom7
025900160512     c                   seton                                        31
026000160212x3   c                   else
0261001602124    c                   if        traunil < datal15  and
026200160210     c                             traunil > 0
026300160210     c                   movel     traunil       dataiso
026400160210     c                   move      dataiso       dataunil
026500160211     c                   move      'A'           alert2
026600160210     c                   move      'X'           anom15
026700160512     c                   seton                                        31
026800160212e4   c                   endif
026900160212e3   c                   endif
027000160212x2   c                   else
027100160217     c                   if        traunil > 0
027200160217     c                   movel     traunil       dataiso
027300160217     c                   move      dataiso       dataunil
027400160217     c                   else
027500160217     c                   clear                   dataunil
027600160217     c                   endif
027700160211     c                   move      'C'           alert2
027800160211     c                   move      'X'           anom7
027900160512     c                   seton                                        31
028000160212e2   c                   endif
028100160212e1   c                   endif
028200160506e1   c                   endif
028300160210      * controllo riga autista estero per permessi di soggiorno 7 o 15 giorni
028400160511     c                   setoff                                       32
0285001605111    c                   if        traitaest <> 'I'
028600160511     c                   if        tradtsps <> 99991231
028700160217      * data permesso di soggiorno
0288001605062    c                   if        TRADTSPS > 0 and tradtsps > wdatg
0289001605063    c                   if        tradtsps < datal7
029000160217     c                   movel     tradtsps      dataiso
029100160217     c                   move      dataiso       datasps
029200160217     c                   move      'B'           alert3
029300160217     c                   move      'X'           anom7
029400160512     c                   seton                                        32
029500160506x3   c                   else
0296001605064    c                   if        tradtsps < datal15
029700160217     c                   movel     tradtsps      dataiso
029800160217     c                   move      dataiso       datasps
029900160217     c                   move      'A'           alert3
030000160217     c                   move      'X'           anom15
030100160512     c                   seton                                        32
030200160506e4   c                   endif
030300160506e3   c                   endif
030400160506x2   c                   else
0305001605063    c                   if        tradtsps > 0
030600160217     c                   movel     tradtsps      dataiso
030700160217     c                   move      dataiso       datasps
030800160506x3   c                   else
030900160217     c                   clear                   datasps
031000160506e3   c                   endif
031100160217     c                   move      'C'           alert3
031200160217     c                   move      'X'           anom7
031300160512     c                   seton                                        32
031400160506e2   c                   endif
031500160217      *data richiesta permesso di soggiorno/rinnovo
0316001605062    c                   if        TRADTSPS = 0 or  tradtsps < wdatg
0317001602173    c                   if        tradtrps > 0 and tradtrps > wdatg
031800160511     c                   movel     tradtrps      dataiso
031900160511     c                   move      dataiso       datarps
0320001602114    c                   if        tradtrps < datal7
032100160210     c                   movel     tradtrps      dataiso
032200160210     c                   move      dataiso       datarps
032300160211     c                   move      'B'           alert4
032400160210     c                   move      'X'           anom7
032500160211x4   c                   else
0326001602115    c                   if        tradtrps < datal15
032700160210     c                   movel     tradtrps      dataiso
032800160210     c                   move      dataiso       datarps
032900160211     c                   move      'A'           alert4
033000160210     c                   move      'X'           anom15
033100160211e5   c                   endif
033200160211e4   c                   endif
033300160211x3   c                   else
033400160217     c                   if        tradtrps > 0
033500160217     c                   movel     tradtrps      dataiso
033600160217     c                   move      dataiso       datarps
033700160217     c                   else
033800160210     c                   clear                   datarps
033900160217     c                   endif
034000160211     c                   move      'C'           alert4
034100160211     c                   move      'X'           anom7
034200160506e3   c                   endif
034300160506e2   c                   endif
034400160509e2   c                   endif
034500160506e1   c                   endif
034600160217      * stampa anomalie
034700160210     c                   if        anom7 <> *BLANK
034800160210     C                   except    sette
034900160506     c                   clear                   anom7
035000160210     c                   else
035100160210     c                   if        anom15 <> *BLANK
035200160210     C                   except    quindici
035300160506     c                   clear                   anom15
035400160210     c                   endif
035500160210     c                   endif
035600160210      *
035700050802     c                   enddo
035800050802     c                   endsr
035900050802      *
036000160210      * ?___________________________________________________________________
036100160210     C     RagSocCont    BegSR
036200160210      * ?___________________________________________________________________
036300160210      *
036400160210     C                   clear                   trmz70s1ds
036500160210     C                   movel(p)  TRsiva        PartitaIVA                     Input
036600160210     C                   movel(p)  'F'           SottoNatur                     Input "F/C"
036700160210     C                   movel(p)  TRasocg       Societa                        Input/Output
036800160210     C                   movel(p)  TRaFIL        Unita                          Input/Output
036900160210      *
037000160210     c                   call      'TRMZ70SR1'
037100160210     C                   parm                    trmz70s1ds
037200160210     c*
037300160210     C                   ENDSR
037400031105     c*-------------------------------------------------
037500031105     c     controv1      begsr
037600031105     c*-------------------------------------------------
037700031105     c                   clear                   v1dfil
037800050803     c                   setoff                                       969799
037900051122     c                   setoff                                       959493
038000051122     c                   setoff                                       92
038100031105     c                   if        v1cfil > 0
038200050623     c     v1cfil        chain     azorg01l
038300050623     c                   if        %found(azorg01l)
038400031105     c                   movel     orgdes        v1dfil
038500031105     c                   else
038600031105     c                   seton                                        9796
038700051122     c                   goto      endcontr
038800031105     c                   endif
038900031105     c                   endif
039000160210     c                   if        v1cfil = *zeros
039100031105     c                   seton                                        9996
039200031105     c                   endif
039300160211     c*
039400051122     c     endcontr      endsr
039500050802     c*-------------------------------------------------
039600050802     c     comandi       begsr
039700050802     c*-------------------------------------------------
039800160209     C                   close     waitramf
039900050802      *
040000050802     C                   z-add     66            luncmd
040100050802     C                   CALL      'QCMDEXC'
040200050802     C                   parm                    coman
040300050802     C                   parm                    luncmd
040400050802      *
040500160209     C  nkc              OPEN      waitramf
040600050802     c                   endsr
040700011026     c*-------------------------------------------------
040800011026     c     *inzsr        begsr
040900011026     c*-------------------------------------------------
041000020304     c     *entry        plist
041100020304     c                   parm                    kpjba
041200160210     c                   movel     kpjbu         param
041300160210     c
041400050801
041500160209     c     ktrc          klist
041600160209     c                   kfld                    trsiva
041700160209     c                   kfld                    traivas
041800050801     c
041900050801
042000020218     C                   time                    w0120            14 0
042100020218     C                   move      w0120         wdat              8 0
042200160210     c                   move      wdat          dataeur
042300160210     c                   move      dataeur       dataiso
042400160212     c                   move      dataiso       wdatg             8 0
042500160210     c     dataiso       adddur    15:*d         dataisow
042600160210     c                   move      dataisow      datal15
042700160210     c     dataiso       adddur    7:*d          dataisow
042800160210     c                   move      dataisow      datal7
042900020404     c                   z-add     99            line
043000030630      *profilo di filiale estraggo solo quelli relativi al P.O.
043100030630     C                   CLEAR                   Tibs36Ds
043200030630     C                   EVAL      I36ute = knmus
043300030630     C                   CALL      'TIBS36R'
043400030630     C                   PARM                    tibs36ds
043500051017      * x filiale duplica file di work in qtemp
043600051017     c                   movea     cmd2          coman
043700051017     c                   exsr      comandi
043800030630     c                   if        o36pou <> 046
043900050805     c                   move      o36pou        v1cfil
044000160211     c                   seton                                        80
044100030630     c                   endif
044200050802
044300050805     c                   if        v1cfil <> 0
044400031105     c     v1cfil        chain     azorg01l
044500031105     c                   if        %found(azorg01l)
044600031105     c                   movel     orgdes        v1dfil
044700031105     c                   endif
044800050805     c                   endif
044900011026     c                   endsr
045000020404     C*--------------------------------------------------------------------
045100160210     Oqsysprt   E            testa          1 02
045200160209     o                                           10 'TRMZA5R'
045300050802     o                       knmus               22
045400160209     o                                        +   1 'Lista società e autisti no-
045500160209     o                                              n in regola con i documenti'
045600050802     o                       udate         y  +   3
045700050802     O                                        +   1 'Pag.'
045800050802     O                       page          z  +   1
045900160211     O          E            testa       1
046000160211     o                                           20 'Legenda Anomalie:'
046100160211     o                                           21 'A'
046200160217     o                                           50 'Documento scade entro 15gg'
046300160211     O          E            testa       1
046400160211     o                                           21 'B'
046500160217     o                                           50 'Documento scade entro 7g  '
046600160211     O          E            testa       1
046700160211     o                                           21 'C'
046800160217     o                                           50 'Manca la data / Scaduto   '
046900160210     O          E            testa       2
047000160211     o                                           15 'Società/Autista'
047100160215     o                                           47 'P.I.V.A. Cons.'
047200160210     o                                           60 'Data Durc'
047300160210     o                                           75 'Data Unilav'
047400160210     o                                           90 'Data P.Sogg'
047500160210     o                                          105 'Rich P.Sogg'
047600160217     o                                          120 'E1 E2 E3 E4'
047700160215     O          E            testa       0
047800160215     o                                           15 'Società/Autista'
047900160215     o                                           47 'P.I.V.A. Cons.'
048000160215     o                                           60 'Data Durc'
048100160215     o                                           75 'Data Unilav'
048200160215     o                                           90 'Data P.Sogg'
048300160215     o                                          105 'Rich P.Sogg'
048400160217     o                                          120 'E1 E2 E3 E4'
048500160215     O          E            testa       0
048600160215     o                                           15 'Società/Autista'
048700160215     o                                           47 'P.I.V.A. Cons.'
048800160215     o                                           60 'Data Durc'
048900160215     o                                           75 'Data Unilav'
049000160215     o                                           90 'Data P.Sogg'
049100160215     o                                          105 'Rich P.Sogg'
049200160217     o                                          120 'E1 E2 E3 E4'
049300160210      *filiale
049400050803     O          E            testafil    2  1
049500160212     o                                            7 'Filiale'
049600160212     o                       trafil           +   3
049700160210     o                       orgdes           +   2
049800160210      *fornitore
049900160215     O          E            for7        1
050000160215     o                       TRASOCl             44
050100160215     o                       datadurc            60
050200160217     o                       alert1             111
050300160215     O          E            for7        0
050400160215     o                       TRASOCl             44
050500160215     o                       datadurc            60
050600160215     O          E            for7        0
050700160215     o                       TRASOCl             44
050800160215     o                       datadurc            60
050900160215     O          E            for15       1
051000160215     o                       TRASOCl             44
051100160215     o                       datadurc            60
051200160217     o                       alert1             111
051300160215     O          E            for15       0
051400160215     o                       TRASOCl             44
051500160215     o                       datadurc            60
051600160215     O          E            for15       0
051700160215     o                       TRASOCl             44
051800160215     o                       datadurc            60
051900160210      *autista
052000160210     O          E            sette       1
052100160212     o                       codaut               7
052200160212     o                       TRAaut              33
052300160212     o               30      TRAivas             49
052400160212     o               30      datadurc            60
052500160212     o               31      dataunil            75
052600160212     o               32      datasps             90
052700160212     o               32      datarps            105
052800160217     o               30      alert1             111
052900160217     o               31      alert2             114
053000160217     o               32      alert3             117
053100160217     o               32      alert4             120
053200160210     O          E            quindici    1
053300160212     o                       codaut               7
053400160212     o                       TRAaut              33
053500160212     o               30      TRAivas             49
053600160212     o               30      datadurc            60
053700160212     o               31      dataunil            75
053800160212     o               32      datasps             90
053900160212     o               32      datarps            105
054000160217     o               30      alert1             111
054100160217     o               31      alert2             114
054200160217     o               32      alert3             117
054300160217     o               32      alert4             120
054400160210      *________________________________________________________________
054500050802**  CMD1
054600160209CLRPFM    FILE(WAITRAMF)
054700050802**  CMD2
054800160209CRTDUPOBJ OBJ(WAITRAMF) FROMLIB(*LIBL) OBJTYPE(*FILE) TOLIB(QTEMP)
054900050802**  CMD3
055000160209DLTF FILE(QTEMP/WAITRAMF)
