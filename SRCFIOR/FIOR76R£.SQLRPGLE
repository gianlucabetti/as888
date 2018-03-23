000100070910     h DECEDIT('0,') DATEDIT(*DMY.) option(*nodebugio)
000200070910      * FIOR76R *----------------------------------------------------*
000300070910      *         Stampa assegnazione ritiri in distinta
000400070910      *--------------------------------------------------------------*
000500070213     Ffidst08l  if   e           k disk
000600021203     Ffiapd01l  if   e           k disk
000700030723     Fazorg01l  if   e           k disk
000800070907     Ffidst51l  if   e           k disk
000900070907     Ffidst52l  if   e           k disk    rename(fidst500:fidst52)
001000070214     Ffigrg03l  if   e           k disk
001100070214     Fprtf198   O    F  198        PRINTER prtctl(prtds)
001200020304     dkpjba          e ds
001300030505     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
001400030505     D CNCR80        E DS
001500070911     D fior76ds      e DS
001600070911     D fior64ds      e DS
001700070911     D og147         E DS
001800070911     d fnorgds       e ds                  extname(fnorg00f)
001900070911     d fnormds       e ds                  extname(fnorm00f)
002000070706     d dgrgflo       e ds
002100020404     d prtds           ds
002200020404     d   spab                         3  0
002300020404     d   spaa                         3  0
002400020404     d   skab                         3  0
002500020404     d   skaa                         3  0
002600020404     d   line                         3  0
002700070911     d rrnorg          s              9  0
002800070911     d rrnorm          s              9  0
002900030724     d primo           s              1
003000020305     d dataiso         s               d   datfmt(*iso)
003100020305     d dataeur         s               d   datfmt(*eur)
003200070214     d diffpkl         s                   like(dstpkl)
003300070214     d diffvlu         s                   like(dstvlu)
003400070214     d diffnftl        s                   like(dstnftl)
003500070221     d diffnbl         s                   like(dstnbl)
003600070214     d desgir          s                   like(grgdes)
003700070912     d exorgcgi        s                   like(orgcgi)
003800070510     d totpkl          s                   like(dstpkl) inz
003900070510     d totvlu          s              7  3 inz
004000070911     d totorm          s              5  0 inz
004100070510     d totcol          s              5  0 inz
004200070510     d ttotpkl         s                   like(dstpkl) inz
004300070510     d ttotvlu         s              7  3 inz
004400070911     d ttotorm         s              5  0 inz
004500070510     d ttotcol         s              5  0 inz
004600070510     d wpes            s                   like(dstpkl) inz
004700070510     d wvol            s                   like(dstvlu) inz
004800070215     d dis             s              6  0 dim(5)
004900070215     d pta             s              2  0 dim(5)
005000070215     d cn              s              1  0
005100070510     d tdstpkla        s                   like(dstpkla) inz
005200070629     d tdstvlua        s              7  3 inz
005300070510     d tdststpa        s                   like(dststpa) inz
005400070510     d tdstnbla        s                   like(dstnbla) inz
005500000000     C*---------------------------------------------------------------*
005600020304      *
005700070907     c                   if        or76orma = 'S'
005800070213     c                   exsr      fidst
005900070329     c                   endif
006000070214     c                   z-add     99            line
006100070907     c                   if        or76noas = 'S'
006200070911     c                   exsr      fnorg
006300070329     c                   endif
006400070215     c                   seton                                        lr
006500990113     c*-------------------------------------------------
006600070213     c     fidst         begsr
006700990113     c*-------------------------------------------------
006800070213     c     kdst          setll     fidst08l
006900981029     c                   do        *hival
007000070213     c     kdst          reade     fidst08l
007100070213     c                   if        %eof(fidst08l)
007200070213     c                   leave
007300070213     c                   endif
007400070213      * richiesta una sola distinta
007500070329      * oppure distinta annullata
007600070907     c                   if        or76dis <> 0 and dstnfv <> or76dis
007700070329     c                             or dstatb = 'A'
007800031114     c                   iter
007900031114     c                   endif
008000070618      * richieste solo distinte PARCEL/ oppure solo messaggerie
008100070907     c                   if        (or76pame = 'P' and dsttpm <>' ') or
008200070907     c                             (or76pame = 'M' and dsttpm <>'M')
008300070618     c                   iter
008400070618     c                   endif
008500070830      * richieste solo distinte mattina e pomeriggio
008600070907     c                   if        (or76FPPI = 'P' and dstfpp <>'P') or
008700070907     c                             (or76FPPI = 'M' and dstfpp = 'P')
008800070830     c                   iter
008900070830     c                   endif
009000070618      *
009100070213     c     kapd          chain     fiapd01l                           97
009200070213      *se l'anagrafica non corrisponde non stampo la decodifica
009300070213     c   97              clear                   despdr           25
009400070213     c  n97              movel     apdrsc        despdr
009500070214      *reperisco descrizione primo giro distinta
009600070214     c                   clear                   desgir
009700070907     c     kdst5         chain     fidst51l                           97
009800070907     c                   if        %found(fidst51l)
009900070214     c     kgrg          setll     figrg03l
010000070214     c                   do        *hival
010100070214     c     kgrg          reade     figrg03l
010200070214     c                   if        %eof(figrg03l)
010300070214     c                   leave
010400070214     c                   endif
010500070706     c                   eval      dgrgflo = grgflo
010600070214     c                   if        dstdfv >= grgdde and dstdfv <= grgdscad
010700070706      * verifico tipo utilizzo giro
010800071011     c                             and (§grgtug = 'R' or §grgtug = 'E')
010900070214     c                   movel     grgdes        desgir
011000070214     c                   endif
011100070214     c                   enddo
011200070214     c                   endif
011300070214
011400030724     c                   if        line > 60
011500030724     c                   except    testata
011600030724     c                   endif
011700070214      * calcolo le differenze
011800070214     c                   clear                   diffpkl
011900070214     c                   clear                   diffvlu
012000070214     c                   clear                   diffnftl
012100070221     c                   clear                   diffnbl
012200070907     c     dstrpkla      sub       dstrpkl       diffpkl
012300070907     c     dstrvlua      sub       dstrvlu       diffvlu
012400070907     c     dstrstpa      sub       dstrnftl      diffnftl
012500070330
012600070330     c     diffpkl       comp      0                                  30
012700070330     c     diffvlu       comp      0                                  31
012800070330     c     diffnftl      comp      0                                  32
012900070510      * totalizzo
013000070907     c                   add       dstrpkla      tdstpkla
013100070907     c                   add       dstrvlua      tdstvlua
013200070907     c                   add       dstrstpa      tdststpa
013300070907     c                   add       dstrnbla      tdstnbla
013400070214     c
013500070213     c                   except    detta
013600030724     c
013700020304     c                   enddo
013800070510     c                   except    totalia
013900020304
014000020304     c                   endsr
014100981029      *
014200070214     c*-------------------------------------------------
014300070911     c     fnorg         begsr
014400070214     c*-------------------------------------------------
014500070911      * se il p.o. è in parallelo testo la distinta su ORG altrimenti
014600070911      * se il p.o. è partito testo la distinta anche su ORM
014700070911      *
014800070911      *
014900070911     c                   if        ogddan > wdymd
015000070911     C/EXEC SQL
015100070911     C+ DECLARE A1 CURSOR FOR SELECT rrn(fnorg01l), rrn(fnorm01l),
015200070911     C+ fnorg01l.*, fnorm01l.*
015300070911     C+ FROM fnorg01l join fnorm01l on ormpoe=orgpoe
015400070911     C+ and ormnsr=orgnsr and ormnor=orgnor and ormnrv=orgnrv
015500070917     C+ WHERE orgndc = 0 and ormpor=:or76fil
015600070911     C+ ORDER BY
015700070911     C+ Fnorg01l.orgpocgi, Fnorg01l.orgcgi
015800070911     C/END-EXEC
015900070911
016000070911     C/EXEC SQL
016100070911     C+ OPEN a1
016200070911     C/END-EXEC
016300070911     c                   else
016400070911     c* oltre a ORMNDC = 0 testo anche ORGNDC=0 per evitare di rielaborare
016500070911     c* gli ORM  già inseriti nelle precedenti distinte
016600070911     C/EXEC SQL
016700070911     C+ DECLARE B1 CURSOR FOR SELECT rrn(fnorg01l), rrn(fnorm01l),
016800070911     C+ fnorg01l.*, fnorm01l.*
016900070911     C+ FROM fnorg01l join fnorm01l on ormpoe=orgpoe
017000070911     C+ and ormnsr=orgnsr and ormnor=orgnor and ormnrv=orgnrv
017100070917     C+ WHERE orgndc = 0 and ormpor=:or76fil
017200071105     C+ and ormndc = 0 and ormeti <> ' ' ORDER BY
017300070911     C+ Fnorg01l.orgpocgi, Fnorg01l.orgcgi
017400070911     C/END-EXEC
017500070911
017600070911     C/EXEC SQL
017700070911     C+ OPEN b1
017800070911     C/END-EXEC
017900070911     c                   end
018000070911
018100071010do  1C                   do        *hival
018200070911
018300070911     c                   if        ogddan > wdymd
018400070911     C/EXEC SQL
018500070911     C+ FETCH NEXT FROM a1 INTO :rrnorg, :rrnorm, :fnorgds, :fnormds
018600070911     C/END-EXEC
018700070911     c                   else
018800070911     C/EXEC SQL
018900070911     C+ FETCH NEXT FROM b1 INTO :rrnorg, :rrnorm, :fnorgds, :fnormds
019000070911     C/END-EXEC
019100070911     c                   end
019200070911
019300070911     c                   select
019400070911     c                   when      sqlcod = 100
019500070911     c                   leave
019600070911     c                   WHEN      SqlCod < 0
019700070911     c                   if        sqlcod <> -913
019800070911     c                   seton                                        H1
019900070911     c                   end
020000070911     c                   other
020100070911     c                   exsr      gesorm
020200070911     c                   endsl
020300070911
020400071010e   1c                   enddo
020500070911
020600070911     c                   if        ogddan > wdymd
020700070911     C/EXEC SQL
020800070911     C+ CLOSE a1
020900070911     C/END-EXEC
021000070911     c                   else
021100070911     C/EXEC SQL
021200070911     C+ CLOSE b1
021300070911     C/END-EXEC
021400070911     c                   end
021500070912
021600070912     c                   exsr      ultimo
021700071010      *
021800071010      * Chiusura prtf di dettaglio ORM da assegnare
021900071011     c                   if        OR76detta = 'S'
022000071011     c                   movel(p)  FIOR76ds      KPJBU
022100071010     c*** NO 2° parm:    clear                   FNORMds
022200071010     c*** NO 3° parm:    clear                   FNORGds
022300071010     c                   call      'FIOR76R1'
022400071010     c                   parm                    KPJBA
022500071010     c*** NO 2° parm:    parm                    FNORMds
022600071010     c*** NO 3° parm:    parm                    FNORGds
022700071011     c                   endif
022800070330     c
022900070911     c                   endsr
023000070911      *
023100070911     c*-------------------------------------------------
023200070911     c     gesorm        begsr
023300070911     c*-------------------------------------------------
023400070911     c                   exsr      buono
023500070911     c                   if        or64asso = 'N'
023600070912     c                   leavesr
023700070214     c                   endif
023800070214      * primo rekord
023900070214     c                   if        primo = *blank
024000070912     c                   move      orgcgi        exorgcgi
024100070214     c                   move      'X'           primo
024200070214     c                   endif
024300071010if  1c                   if        exorgcgi = orgcgi
024400070911     c                   add       orgpkg        totpkl
024500070911     c                   add       orgvlm        totvlu
024600070911     c                   add       1             totorm
024700071010x   1c                   else
024800070214      *reperisco descrizione primo giro distinta
024900070214     c                   clear                   desgir
025000070214     c     kgrg2         setll     figrg03l
025100071010do  2c                   do        *hival
025200070214     c     kgrg2         reade     figrg03l
025300070214     c                   if        %eof(figrg03l)
025400070214     c                   leave
025500070214     c                   endif
025600070706     c                   eval      dgrgflo = grgflo
025700070214     c                   if        datk >= grgdde and datk <= grgdscad
025800070706      * verifico tipo utilizzo giro
025900071011     c                             and (§grgtug = 'R' or §grgtug = 'E')
026000070214     c                   movel     grgdes        desgir
026100070214     c                   endif
026200071010e   2c                   enddo
026300070214
026400070215      *reperisco distinte con giro nella giornata in esame
026500070215     c                   clear                   dis
026600070215     c                   clear                   pta
026700070215     c                   z-add     0             cn
026800070911     c     kdst52        setll     fidst52l
026900071010do  2c                   do        *hival
027000070911     c     kdst52        reade     fidst52l
027100070911     c                   if        %eof(fidst52l)
027200070215     c                   leave
027300070215     c                   endif
027400070329      * scarta distinte annullate
027500070911     c                   if        dst5atb = 'A'
027600070329     c                   iter
027700070329     c                   endif
027800070215     c                   add       1             cn
027900070215     c                   if        cn > 5
028000070215     c                   leave
028100070215     c                   endif
028200070911     c                   z-add     dst5nfv       dis(cn)
028300070911     c                   z-add     dst5prgg      pta(cn)
028400071010e   2c                   enddo
028500070215
028600070214     c                   if        line > 60
028700070214     c                   except    testatab
028800070214     c                   endif
028900070214     c                   except    dettab
029000070214     c                   add       totpkl        ttotpkl
029100070214     c                   add       totvlu        ttotvlu
029200070911     c                   add       totorm        ttotorm
029300070214      * memorizza valori prima bolla del nuovo giro
029400070912     c                   move      orgcgi        exorgcgi
029500070911     c                   z-add     orgpkg        totpkl
029600070911     c                   z-add     orgvlm        totvlu
029700070911     c                   z-add     1             totorm
029800071010e   1c                   endif
029900071010      *
030000071010      * Richiesta stampa del dettaglio ORM
030100071010     c                   if        OR76detta = 'S'
030200071010     c                   exsr      CallFIOR76R1
030300071010     c                   endif
030400071010      *
030500070214     c                   endsr
030600070912      *
030700070912      *-----------------------------------------------
030800070912     C     ultimo        begsr
030900070912      *-----------------------------------------------
031000070912      * ultima riga
031100070912      *reperisco descrizione ultimo giro distinta
031200070912     c                   clear                   desgir
031300070912     c     kgrg2         setll     figrg03l
031400070912     c                   do        *hival
031500070912     c     kgrg2         reade     figrg03l
031600070912     c                   if        %eof(figrg03l)
031700070912     c                   leave
031800070912     c                   endif
031900070912     c                   if        datk >= grgdde and datk <= grgdscad
032000070912     c                   movel     grgdes        desgir
032100070912     c                   endif
032200070912     c                   enddo
032300070912
032400070912      *reperisco distinte con giro nella giornata in esame
032500070912     c                   clear                   dis
032600070912     c                   clear                   pta
032700070912     c                   z-add     0             cn
032800070912     c     kdst52        setll     fidst52l
032900070912     c                   do        *hival
033000070912     c     kdst52        reade     fidst52l
033100070912     c                   if        %eof(fidst52l)
033200070912     c                   leave
033300070912     c                   endif
033400070912      * scarta distinte annullate
033500070912     c                   if        dst5atb = 'A'
033600070912     c                   iter
033700070912     c                   endif
033800070912     c                   add       1             cn
033900070912     c                   if        cn > 5
034000070912     c                   leave
034100070912     c                   endif
034200070912     c                   z-add     dst5nfv       dis(cn)
034300070912     c                   z-add     dst5prgg      pta(cn)
034400070912     c                   enddo
034500070912
034600070912
034700070912     c                   if        line > 60
034800070912     c                   except    testatab
034900070912     c                   endif
035000070912     c                   except    dettab
035100070912     c                   add       totpkl        ttotpkl
035200070912     c                   add       totvlu        ttotvlu
035300070912     c                   add       totorm        ttotorm
035400070912     c                   except    totali
035500070912
035600070912     c                   endsr
035700070214      *
035800070214      *-----------------------------------------------
035900070911     C     buono         begsr
036000070214      *-----------------------------------------------
036100070911      * richiamo pgm per esito ritirabilità
036200070911      * per controllare se ORM assegnabile richiamo pgm esterno
036300070911     c                   clear                   fior64ds
036400070911     c                   eval      or64poei = ormpoe
036500070911     c                   eval      or64nsri = ormnsr
036600070911     c                   eval      or64nori = ormnor
036700070911     c                   eval      or64nrvi = ormnrv
036800070911      * deve essere impostata la data
036900070911     c                   eval      or64dfvi = datk
037000070911     c                   eval      or64dari = 'S'
037100070911     c                   if        wdymd < ogddaN
037200070911     c                   eval      or64ndci = 'G'
037300070911     c                   eval      or64etii = 'S'
037400070911     c                   else
037500070911     c                   eval      or64ndci = 'E'
037600070911     c                   endif
037700070911     c                   call      'FIOR64R'
037800070911     c                   parm                    kpjba
037900070911     c                   parm                    fior64ds
038000070911
038100070214     C                   ENDSR
038200071010      *-------------------------------------------------
038300071010      * Stampa dettaglio ORM da assegnare
038400071010      *-------------------------------------------------
038500071010     c     CallFIOR76R1  begsr
038600071010      *
038700071010     c                   movel(p)  FIOR76ds      KPJBU
038800071010      *
038900071010     c                   call      'FIOR76R1'
039000071010     c                   parm                    KPJBA
039100071010     c                   parm                    FNORMds
039200071010     c                   parm                    FNORGds
039300071010      *
039400071010     c                   endsr
039500011026     c*-------------------------------------------------
039600011026     c     *inzsr        begsr
039700011026     c*-------------------------------------------------
039800020304     c     *entry        plist
039900020304     c                   parm                    kpjba
040000070911     c                   movel     kpjbu         fior76ds
040100070329      * inizializzo parametri di stampa elenchi a 'S' se non impostati
040200070911     c                   if        or76orma = *blank
040300070911     c                   move      'S'           or76orma
040400070329     c                   endif
040500070911     c                   if        or76noas = *blank
040600070911     c                   move      'S'           or76noas
040700070329     c                   endif
040800070329      *
040900020218     C                   time                    w0120            14 0
041000020218     C                   move      w0120         wdat              8 0
041100070222     C                   movel     w0120         wora              6 0
041200070403     c                   move      wdat          dataeur
041300070403     c                   move      dataeur       dataiso
041400070403     c                   move      dataiso       wdymd             8 0
041500070403
041600020404     c                   z-add     99            line
041700070213     c     kdst          klist
041800070213     c                   kfld                    filk              3 0
041900070213     c                   kfld                    catk              1 0
042000070213     c                   kfld                    datk              8 0
042100070911     c                   z-add     or76fil       filk
042200070213     c                   z-add     4             catk
042300070911     c                   z-add     or76dat       datk
042400070213
042500070911     c     kdst5         klist
042600070214     c                   kfld                    dstfgs
042700070214     c                   kfld                    dstnfv
042800070214     c                   kfld                    dstdfv
042900070214
043000070911     c     kdst52        klist
043100070215     c                   kfld                    filk
043200070215     c                   kfld                    datk
043300070223     c                   kfld                    filk
043400070912     c                   kfld                    exorgcgi
043500070215
043600070214     c     kgrg          klist
043700070911     c                   kfld                    dst5fgs
043800070911     c                   kfld                    dst5cgi
043900070214
044000070214     c     kgrg2         klist
044100070214     c                   kfld                    filk
044200070912     c                   kfld                    exorgcgi
044300070214
044400070213     c     kapd          klist
044500070213     c                   kfld                    tipk              1
044600070213     c                   kfld                    dstpdr
044700070213     c                   movel     'A'           tipk
044800030725
044900070214
045000030505     C                   Z-ADD     1             CODUT
045100030505     C                   CALL      'X§PARUT'
045200030505     C                   PARM                    UT§DSE
045300030505     C                   MOVEL     RAGUT         RSUT             20
045400030505     C                   MOVEL     REC80         CNCR80
045500070213     c                   move      datk          dataiso
045600070213     c                   move      dataiso       dataeur
045700070222     c                   move      *all'_'       separa          192
045800070213      * decodifica p.o.
045900070403     c                   clear                   desfil
046000070403     c                   clear                   ogddaN            8 0
046100070403     c     filk          chain     azorg01l
046200070403     c                   if        %found(azorg01l)
046300070911     c                   movel     orgde7        og147
046400070911     c                   if        §ogddao <> ' '
046500070911     c                   move      §ogddao       ogddaN
046600070403     c                   endif
046700070403     c                   movel     orgdes        desfil           20
046800070403     c                   endif
046900070403
047000011026     c                   endsr
047100020404     C*--------------------------------------------------------------------
047200070214     Oprtf198   E            testata        1 02
047300070222     o                       rsut                21
047400070222     o                       knmus               33
047500070327     o                                          100 'Situazione Assegnazione'
047600070912     o                                        +   2 'Ritiri Distinte del'
047700070215     o                       dataeur          +   2
047800070912     o                                          153 'FIOR76R'
047900070510     o                       wdat          y    170
048000070510     o                       wora               181 '  :  :  '
048100070222     o                                          190 'Pag.'
048200070222     O                       page          z    196
048300070223     O          E            testata     1
048400070223     o                                           13 'P.O Gestione'
048500070223     o                       filk             +   2
048600070223     o                       desfil           +   2
048700070223
048800070222     O          E            testata     1
048900070222     o                       separa             192
049000070223     O          E            testata     1
049100070223     o                                           73 '|'
049200070330     o                                           84 'TOLLERANZA'
049300070912     o                                          100 '   ORM    '
049400070223     o                                          102 '|'
049500070917     o                                          120 'FERMATE'
049600070223     o                                          132 '|'
049700070223     o                                          150 'VOLUME'
049800070223     o                                          162 '|'
049900070223     o                                          179 'PESO '
050000070223     o                                          192 '|'
050100070223     O          E            testata     1
050200070213     o                                           10 'Distinta'
050300070213     o                                        +   2 'Autotrasportatore'
050400070213     o                                           41 'M/P'
050500070228     o                                           60 'Descrizione 1°giro'
050600070222     o                                           73 '|'
050700070330     o*                                          80 'Nr. Max'
050800070330     o                                           80 ' %  Mag'
050900070912     o                                          100 'Assegnati'
051000070330     o*                                         101 'Differ.'
051100070222     o                                          102 '|'
051200070223     o                                          110 'Nr. Max '
051300070223     o                                          121 'Assegnate'
051400070223     o                                          131 'Differ.'
051500070222     o                                          132 '|'
051600070223     o                                          140 'Mc. Max '
051700070223     o                                          151 'Assegnati'
051800070223     o                                          161 'Differ.'
051900070222     o                                          162 '|'
052000070223     o                                          171 'Kg. Max '
052100070223     o                                          181 'Assegnati'
052200070223     o                                          191 'Differ.'
052300070222     o                                          192 '|'
052400070222     O          E            testata     1
052500070222     o                       separa             192
052600070222     O          E            detta       1
052700070222     o                                           73 '|'
052800070222     o                                          102 '|'
052900070222     o                                          132 '|'
053000070222     o                                          162 '|'
053100070222     o                                          192 '|'
053200070222     O          E            detta       1
053300070213     o                       dstnfv        z      9
053400070213     o                       despdr              37
053500070213     o                       dstfpp              40
053600070214     o                       desgir              72
053700070222     o                                           73 '|'
053800070912     o                       dstrtol       2     81
053900070912     o                       dstrnbla      2    100
054000070222     o                                          102 '|'
054100070912     o                       dstrnftl      z    111
054200070912     o                       dstrstpa      z    121
054300070912     o                       diffnftl      k    131
054400070222     o                                          132 '|'
054500070912     o                       dstrvlu       2    141
054600070912     o                       dstrvlua      2    151
054700070912     o                       diffvlu       k    161
054800070222     o                                          162 '|'
054900070912     o                       dstrpkl       2    171
055000070912     o                       dstrpkla      2    181
055100070221     o                       diffpkl       k    191
055200070222     o                                          192 '|'
055300070215     O          E            detta       0
055400070330     o               32      diffnftl      k    131
055500070330     o               31      diffvlu       k    161
055600070330     o               30      diffpkl       k    191
055700070215     O          E            detta       0
055800070330     o               32      diffnftl      k    131
055900070330     o               31      diffvlu       k    161
056000070330     o               30      diffpkl       k    191
056100070510     O          E            totalia     1
056200070510     o                                           73 '|'
056300070510     o                                          102 '|'
056400070510     o                                          132 '|'
056500070510     o                                          162 '|'
056600070510     o                                          192 '|'
056700070510     O          E            totalia     1
056800070912     o                                           30 'Totale ORM  -
056900070912     o                                              assegnati '
057000070510     o                                           73 '|'
057100070510     o                       tdstnbla      2    100
057200070510     o                                          102 '|'
057300070510     o                       tdststpa      z    121
057400070510     o                                          132 '|'
057500070510     o                       tdstvlua      2    151
057600070510     o                                          162 '|'
057700070510     o                       tdstpkla      2    181
057800070510     o                                          192 '|'
057900070510     O          E            totalia     0
058000070912     o                                           30 'Totale ORM  -
058100070912     o                                              assegnati '
058200070510     o                                           73 '|'
058300070510     o                       tdstnbla      2    100
058400070510     o                                          102 '|'
058500070510     o                       tdststpa      z    121
058600070510     o                                          132 '|'
058700070510     o                       tdstvlua      2    151
058800070510     o                                          162 '|'
058900070510     o                       tdstpkla      2    181
059000070510     o                                          192 '|'
059100070510     O          E            totalia     0
059200070912     o                                           30 'Totale ORM  -
059300070912     o                                              assegnati '
059400070510     o                                           73 '|'
059500070510     o                       tdstnbla      2    100
059600070510     o                                          102 '|'
059700070510     o                       tdststpa      z    121
059800070510     o                                          132 '|'
059900070510     o                       tdstvlua      2    151
060000070510     o                                          162 '|'
060100070510     o                       tdstpkla      2    181
060200070510     o                                          192 '|'
060300070214      *__totali Giri non asssegnati______________________________
060400070214     O          E            testatab       1 02
060500070223     o                       rsut                21
060600070215     o                       knmus            +   2
060700070912     o                                          110 'ORM da ritirare non assegn-
060800070912     o                                              ati alle'
060900070215     o                                        +   2 'Distinte del'
061000070215     o                       dataeur          +   2
061100070912     o                                          150 'FIOR76R'
061200070510     o                       wdat          y    170
061300070510     o                       wora               181 '  :  :  '
061400070222     o                                          190 'Pag.'
061500070222     O                       page          z    196
061600070223     O          E            testatab    1
061700070223     o                                           13 'P.O Gestione'
061800070223     o                       filk             +   2
061900070223     o                       desfil           +   2
062000070223
062100070222     O          E            testatab    1
062200070222     o                       separa             192
062300070214     O          E            testatab    2
062400070214     o                                            5 'Giro'
062500070214     o                                           23 'Descrizione'
062600070912     o                                           53 'Tot ORM '
062700070912     o*                                          63 'Tot Colli'
062800070222     o                                           73 'Tot Vol.'
062900070222     o                                           83 'Tot Peso'
063000070222     o                                        +  30 'Distinte in cui il giro -
063100070215     o                                              e'' stato pianificato'
063200070215     O          E            testatab    1
063300070222     o                                           85 '|'
063400070215     o                                           93 'Numero'
063500070215     o                                        +   1 'Ptà'
063600070222     o                                        +   1 '|'
063700070215     o                                        +   3 'Numero'
063800070215     o                                        +   1 'Ptà'
063900070222     o                                        +   1 '|'
064000070215     o                                        +   3 'Numero'
064100070215     o                                        +   1 'Ptà'
064200070222     o                                        +   1 '|'
064300070215     o                                        +   3 'Numero'
064400070215     o                                        +   1 'Ptà'
064500070222     o                                        +   1 '|'
064600070215     o                                        +   3 'Numero'
064700070215     o                                        +   1 'Ptà'
064800070222     o                                        +   1 '|'
064900070222     O          E            testatab    1
065000070222     o                       separa             192
065100070222     O          E            dettab      1
065200070222     o                                           85 '|'
065300070222     o                                        +  13 '|'
065400070222     o                                        +  14 '|'
065500070222     o                                        +  14 '|'
065600070222     o                                        +  14 '|'
065700070222     o                                        +  14 '|'
065800070222     O          E            dettab      1
065900070912     o                       exorgcgi            11
066000070214     o                       desgir              42
066100070912     o                       totorm        2     53
066200070912     o*                      totcol        2     63
066300070222     o                       totvlu        2     73
066400070222     o                       totpkl        2     83
066500070222     o                                           85 '|'
066600070222     o                       dis(1)        z  +   2
066700070215     o                       pta(1)        z  +   2
066800070222     o                                        +   1 '|'
066900070215     o                       dis(2)        z  +   3
067000070215     o                       pta(2)        z  +   2
067100070222     o                                        +   1 '|'
067200070215     o                       dis(3)        z  +   3
067300070215     o                       pta(3)        z  +   2
067400070222     o                                        +   1 '|'
067500070215     o                       dis(4)        z  +   3
067600070215     o                       pta(4)        z  +   2
067700070222     o                                        +   1 '|'
067800070215     o                       dis(5)        z  +   3
067900070215     o                       pta(5)        z  +   2
068000070222     o                                        +   1 '|'
068100070214     O          E            totali      2
068200070912     o                                           33 'Totale ORM non -
068300070912     o                                              assegnati '
068400070912     o                       ttotorm       2     53
068500070912     o*                      ttotcol       2     63
068600070222     o                       ttotvlu       2     73
068700070222     o                       ttotpkl       2     83
