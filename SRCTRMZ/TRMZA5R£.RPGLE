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
013400160217     c                   adddur    8:*m          dataiso
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
021900160212     c                   seton                                        30
022000160217     c                   if        tradurc > 0 and tradurc > wdatg
022100160212     c                   if        tradurc < datal7
022200160212     c                   movel     tradurc       dataiso
022300160212     c                   move      dataiso       datadurc
022400160212     c                   move      'B'           alert1
022500160212     c                   move      'X'           anom7
022600160212     c                   else
022700160212     c                   if        tradurc < datal15
022800160212     c                   movel     tradurc       dataiso
022900160212     c                   move      dataiso       datadurc
023000160212     c                   move      'A'           alert1
023100160212     c                   move      'X'           anom15
023200160212     c                   endif
023300160212     c                   endif
023400160212     c                   else
023500160217     c                   if        tradurc > 0
023600160217     c                   movel     tradurc       dataiso
023700160217     c                   move      dataiso       datadurc
023800160217     c                   else
023900160217     c                   clear                   datadurc
024000160217     c                   endif
024100160218     c                   move      'C'           alert1
024200160212     c                   move      'X'           anom7
024300160212     c                   endif
024400160212     c                   endif
024500160217      * dipendente
024600160212     c                   setoff                                       31
0247001602171    c                   if        tradipa <> 'A'
024800160212     c                   seton                                        31
0249001602172    c                   if        traunil > 0 and traunil > wdatg
0250001602123    c                   if        traunil < datal7
025100160210     c                   movel     traunil       dataiso
025200160210     c                   move      dataiso       dataunil
025300160211     c                   move      'B'           alert2
025400160211     c                   move      'X'           anom7
025500160212x3   c                   else
0256001602124    c                   if        traunil < datal15  and
025700160210     c                             traunil > 0
025800160210     c                   movel     traunil       dataiso
025900160210     c                   move      dataiso       dataunil
026000160211     c                   move      'A'           alert2
026100160210     c                   move      'X'           anom15
026200160212e4   c                   endif
026300160212e3   c                   endif
026400160212x2   c                   else
026500160217     c                   if        traunil > 0
026600160217     c                   movel     traunil       dataiso
026700160217     c                   move      dataiso       dataunil
026800160217     c                   else
026900160217     c                   clear                   dataunil
027000160217     c                   endif
027100160211     c                   move      'C'           alert2
027200160211     c                   move      'X'           anom7
027300160212e2   c                   endif
027400160212e1   c                   endif
027500160210      * controllo riga autista estero per permessi di soggiorno 7 o 15 giorni
027600160212     c                   setoff                                       32
027700160217     c                   if        traitaest <> 'I'
027800160212     c                   seton                                        32
027900160217      * data permesso di soggiorno
0280001602171    c                   if        TRADTSPS > 0 and tradtsps > wdatg
0281001602174    c                   if        tradtsps < datal7
028200160217     c                   movel     tradtsps      dataiso
028300160217     c                   move      dataiso       datasps
028400160217     c                   move      'B'           alert3
028500160217     c                   move      'X'           anom7
028600160217x4   c                   else
0287001602175    c                   if        tradtsps < datal15
028800160217     c                   movel     tradtsps      dataiso
028900160217     c                   move      dataiso       datasps
029000160217     c                   move      'A'           alert3
029100160217     c                   move      'X'           anom15
029200160217e5   c                   endif
029300160217e4   c                   endif
029400160217x3   c                   else
029500160217     c                   if        tradtsps > 0
029600160217     c                   movel     tradtsps      dataiso
029700160217     c                   move      dataiso       datasps
029800160217     c                   else
029900160217     c                   clear                   datasps
030000160217     c                   endif
030100160217     c                   move      'C'           alert3
030200160217     c                   move      'X'           anom7
030300160217e3   c                   endif
030400160217e2   c                   endif
030500160217      *data richiesta permesso di soggiorno/rinnovo
0306001602171    c                   if        TRADTSPS = 0 or  tradtsps < wdatg
0307001602173    c                   if        tradtrps > 0 and tradtrps > wdatg
0308001602114    c                   if        tradtrps < datal7
030900160210     c                   movel     tradtrps      dataiso
031000160210     c                   move      dataiso       datarps
031100160211     c                   move      'B'           alert4
031200160210     c                   move      'X'           anom7
031300160211x4   c                   else
0314001602115    c                   if        tradtrps < datal15
031500160210     c                   movel     tradtrps      dataiso
031600160210     c                   move      dataiso       datarps
031700160211     c                   move      'A'           alert4
031800160210     c                   move      'X'           anom15
031900160211e5   c                   endif
032000160211e4   c                   endif
032100160211x3   c                   else
032200160217     c                   if        tradtrps > 0
032300160217     c                   movel     tradtrps      dataiso
032400160217     c                   move      dataiso       datarps
032500160217     c                   else
032600160210     c                   clear                   datarps
032700160217     c                   endif
032800160211     c                   move      'C'           alert4
032900160211     c                   move      'X'           anom7
033000160218e3   c                   endif
033100160217e3   c                   endif
033200160217      * stampa anomalie
033300160210     c                   if        anom7 <> *BLANK
033400160210     C                   except    sette
033500160210     c                   else
033600160210     c                   if        anom15 <> *BLANK
033700160210     C                   except    quindici
033800160210     c                   endif
033900160210     c                   endif
034000160210      *
034100050802     c                   enddo
034200050802     c                   endsr
034300050802      *
034400160210      * ?___________________________________________________________________
034500160210     C     RagSocCont    BegSR
034600160210      * ?___________________________________________________________________
034700160210      *
034800160210     C                   clear                   trmz70s1ds
034900160210     C                   movel(p)  TRsiva        PartitaIVA                     Input
035000160210     C                   movel(p)  'F'           SottoNatur                     Input "F/C"
035100160210     C                   movel(p)  TRasocg       Societa                        Input/Output
035200160210     C                   movel(p)  TRaFIL        Unita                          Input/Output
035300160210      *
035400160210     c                   call      'TRMZ70SR1'
035500160210     C                   parm                    trmz70s1ds
035600160210     c*
035700160210     C                   ENDSR
035800031105     c*-------------------------------------------------
035900031105     c     controv1      begsr
036000031105     c*-------------------------------------------------
036100031105     c                   clear                   v1dfil
036200050803     c                   setoff                                       969799
036300051122     c                   setoff                                       959493
036400051122     c                   setoff                                       92
036500031105     c                   if        v1cfil > 0
036600050623     c     v1cfil        chain     azorg01l
036700050623     c                   if        %found(azorg01l)
036800031105     c                   movel     orgdes        v1dfil
036900031105     c                   else
037000031105     c                   seton                                        9796
037100051122     c                   goto      endcontr
037200031105     c                   endif
037300031105     c                   endif
037400160210     c                   if        v1cfil = *zeros
037500031105     c                   seton                                        9996
037600031105     c                   endif
037700160211     c*
037800051122     c     endcontr      endsr
037900050802     c*-------------------------------------------------
038000050802     c     comandi       begsr
038100050802     c*-------------------------------------------------
038200160209     C                   close     waitramf
038300050802      *
038400050802     C                   z-add     66            luncmd
038500050802     C                   CALL      'QCMDEXC'
038600050802     C                   parm                    coman
038700050802     C                   parm                    luncmd
038800050802      *
038900160209     C  nkc              OPEN      waitramf
039000050802     c                   endsr
039100011026     c*-------------------------------------------------
039200011026     c     *inzsr        begsr
039300011026     c*-------------------------------------------------
039400020304     c     *entry        plist
039500020304     c                   parm                    kpjba
039600160210     c                   movel     kpjbu         param
039700160210     c
039800050801
039900160209     c     ktrc          klist
040000160209     c                   kfld                    trsiva
040100160209     c                   kfld                    traivas
040200050801     c
040300050801
040400020218     C                   time                    w0120            14 0
040500020218     C                   move      w0120         wdat              8 0
040600160210     c                   move      wdat          dataeur
040700160210     c                   move      dataeur       dataiso
040800160212     c                   move      dataiso       wdatg             8 0
040900160210     c     dataiso       adddur    15:*d         dataisow
041000160210     c                   move      dataisow      datal15
041100160210     c     dataiso       adddur    7:*d          dataisow
041200160210     c                   move      dataisow      datal7
041300020404     c                   z-add     99            line
041400030630      *profilo di filiale estraggo solo quelli relativi al P.O.
041500030630     C                   CLEAR                   Tibs36Ds
041600030630     C                   EVAL      I36ute = knmus
041700030630     C                   CALL      'TIBS36R'
041800030630     C                   PARM                    tibs36ds
041900051017      * x filiale duplica file di work in qtemp
042000051017     c                   movea     cmd2          coman
042100051017     c                   exsr      comandi
042200030630     c                   if        o36pou <> 046
042300050805     c                   move      o36pou        v1cfil
042400160211     c                   seton                                        80
042500030630     c                   endif
042600050802
042700050805     c                   if        v1cfil <> 0
042800031105     c     v1cfil        chain     azorg01l
042900031105     c                   if        %found(azorg01l)
043000031105     c                   movel     orgdes        v1dfil
043100031105     c                   endif
043200050805     c                   endif
043300011026     c                   endsr
043400020404     C*--------------------------------------------------------------------
043500160210     Oqsysprt   E            testa          1 02
043600160209     o                                           10 'TRMZA5R'
043700050802     o                       knmus               22
043800160209     o                                        +   1 'Lista società e autisti no-
043900160209     o                                              n in regola con i documenti'
044000050802     o                       udate         y  +   3
044100050802     O                                        +   1 'Pag.'
044200050802     O                       page          z  +   1
044300160211     O          E            testa       1
044400160211     o                                           20 'Legenda Anomalie:'
044500160211     o                                           21 'A'
044600160217     o                                           50 'Documento scade entro 15gg'
044700160211     O          E            testa       1
044800160211     o                                           21 'B'
044900160217     o                                           50 'Documento scade entro 7g  '
045000160211     O          E            testa       1
045100160211     o                                           21 'C'
045200160217     o                                           50 'Manca la data / Scaduto   '
045300160210     O          E            testa       2
045400160211     o                                           15 'Società/Autista'
045500160215     o                                           47 'P.I.V.A. Cons.'
045600160210     o                                           60 'Data Durc'
045700160210     o                                           75 'Data Unilav'
045800160210     o                                           90 'Data P.Sogg'
045900160210     o                                          105 'Rich P.Sogg'
046000160217     o                                          120 'E1 E2 E3 E4'
046100160215     O          E            testa       0
046200160215     o                                           15 'Società/Autista'
046300160215     o                                           47 'P.I.V.A. Cons.'
046400160215     o                                           60 'Data Durc'
046500160215     o                                           75 'Data Unilav'
046600160215     o                                           90 'Data P.Sogg'
046700160215     o                                          105 'Rich P.Sogg'
046800160217     o                                          120 'E1 E2 E3 E4'
046900160215     O          E            testa       0
047000160215     o                                           15 'Società/Autista'
047100160215     o                                           47 'P.I.V.A. Cons.'
047200160215     o                                           60 'Data Durc'
047300160215     o                                           75 'Data Unilav'
047400160215     o                                           90 'Data P.Sogg'
047500160215     o                                          105 'Rich P.Sogg'
047600160217     o                                          120 'E1 E2 E3 E4'
047700160210      *filiale
047800050803     O          E            testafil    2  1
047900160212     o                                            7 'Filiale'
048000160212     o                       trafil           +   3
048100160210     o                       orgdes           +   2
048200160210      *fornitore
048300160215     O          E            for7        1
048400160215     o                       TRASOCl             44
048500160215     o                       datadurc            60
048600160217     o                       alert1             111
048700160215     O          E            for7        0
048800160215     o                       TRASOCl             44
048900160215     o                       datadurc            60
049000160215     O          E            for7        0
049100160215     o                       TRASOCl             44
049200160215     o                       datadurc            60
049300160215     O          E            for15       1
049400160215     o                       TRASOCl             44
049500160215     o                       datadurc            60
049600160217     o                       alert1             111
049700160215     O          E            for15       0
049800160215     o                       TRASOCl             44
049900160215     o                       datadurc            60
050000160215     O          E            for15       0
050100160215     o                       TRASOCl             44
050200160215     o                       datadurc            60
050300160210      *autista
050400160210     O          E            sette       1
050500160212     o                       codaut               7
050600160212     o                       TRAaut              33
050700160212     o               30      TRAivas             49
050800160212     o               30      datadurc            60
050900160212     o               31      dataunil            75
051000160212     o               32      datasps             90
051100160212     o               32      datarps            105
051200160217     o               30      alert1             111
051300160217     o               31      alert2             114
051400160217     o               32      alert3             117
051500160217     o               32      alert4             120
051600160210     O          E            quindici    1
051700160212     o                       codaut               7
051800160212     o                       TRAaut              33
051900160212     o               30      TRAivas             49
052000160212     o               30      datadurc            60
052100160212     o               31      dataunil            75
052200160212     o               32      datasps             90
052300160212     o               32      datarps            105
052400160217     o               30      alert1             111
052500160217     o               31      alert2             114
052600160217     o               32      alert3             117
052700160217     o               32      alert4             120
052800160210      *________________________________________________________________
052900050802**  CMD1
053000160209CLRPFM    FILE(WAITRAMF)
053100050802**  CMD2
053200160209CRTDUPOBJ OBJ(WAITRAMF) FROMLIB(*LIBL) OBJTYPE(*FILE) TOLIB(QTEMP)
053300050802**  CMD3
053400160209DLTF FILE(QTEMP/WAITRAMF)
