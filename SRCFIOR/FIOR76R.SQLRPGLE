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
001100170728     Fprtf198   O    F  198        PRINTER
001200170728     f                                     usropn
001300170728     F                                     prtctl(prtds)
001400020304     dkpjba          e ds
001500030505     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
001600030505     D CNCR80        E DS
001700070911     D fior76ds      e DS
001800070911     D fior64ds      e DS
001900070911     D og147         E DS
002000070911     d fnorgds       e ds                  extname(fnorg00f)
002100070911     d fnormds       e ds                  extname(fnorm00f)
002200070706     d dgrgflo       e ds
002300020404     d prtds           ds
002400020404     d   spab                         3  0
002500020404     d   spaa                         3  0
002600020404     d   skab                         3  0
002700020404     d   skaa                         3  0
002800020404     d   line                         3  0
002900070911     d rrnorg          s              9  0
003000070911     d rrnorm          s              9  0
003100030724     d primo           s              1
003200020305     d dataiso         s               d   datfmt(*iso)
003300020305     d dataeur         s               d   datfmt(*eur)
003400070214     d diffpkl         s                   like(dstpkl)
003500070214     d diffvlu         s                   like(dstvlu)
003600070214     d diffnftl        s                   like(dstnftl)
003700070221     d diffnbl         s                   like(dstnbl)
003800070214     d desgir          s                   like(grgdes)
003900070912     d exorgcgi        s                   like(orgcgi)
004000070510     d totpkl          s                   like(dstpkl) inz
004100070510     d totvlu          s              7  3 inz
004200070911     d totorm          s              5  0 inz
004300070510     d totcol          s              5  0 inz
004400070510     d ttotpkl         s                   like(dstpkl) inz
004500070510     d ttotvlu         s              7  3 inz
004600070911     d ttotorm         s              5  0 inz
004700070510     d ttotcol         s              5  0 inz
004800070510     d wpes            s                   like(dstpkl) inz
004900070510     d wvol            s                   like(dstvlu) inz
005000070215     d dis             s              6  0 dim(5)
005100070215     d pta             s              2  0 dim(5)
005200070215     d cn              s              1  0
005300070510     d tdstpkla        s                   like(dstpkla) inz
005400070629     d tdstvlua        s              7  3 inz
005500070510     d tdststpa        s                   like(dststpa) inz
005600070510     d tdstnbla        s                   like(dstnbla) inz
005700170728     D CMD             S             80    DIM(2) CTDATA PERRCD(1)              OVRPRTF
005800000000     C*---------------------------------------------------------------*
005900020304      *
006000070907     c                   if        or76orma = 'S'
006100070213     c                   exsr      fidst
006200070329     c                   endif
006300070214     c                   z-add     99            line
006400070907     c                   if        or76noas = 'S'
006500070911     c                   exsr      fnorg
006600070329     c                   endif
006700170728      *
006800170728     C* ESEGUO L'OVERRIDE DEL FILE DI STAMPA
006900170728     C                   close     prtf198
007000170728     C                   MOVEL(p)  CMD(2)        COMMAN           80
007100170728     C                   Z-ADD     80            LUNG             15 5
007200170728     C                   CALL      'QCMDEXC'
007300170728     C                   PARM                    COMMAN
007400170728     C                   PARM                    LUNG
007500170728
007600070215     c                   seton                                        lr
007700990113     c*-------------------------------------------------
007800070213     c     fidst         begsr
007900990113     c*-------------------------------------------------
008000070213     c     kdst          setll     fidst08l
008100981029     c                   do        *hival
008200070213     c     kdst          reade     fidst08l
008300070213     c                   if        %eof(fidst08l)
008400070213     c                   leave
008500070213     c                   endif
008600070213      * richiesta una sola distinta
008700070329      * oppure distinta annullata
008800070907     c                   if        or76dis <> 0 and dstnfv <> or76dis
008900070329     c                             or dstatb = 'A'
009000031114     c                   iter
009100031114     c                   endif
009200070618      * richieste solo distinte PARCEL/ oppure solo messaggerie
009300070907     c                   if        (or76pame = 'P' and dsttpm <>' ') or
009400070907     c                             (or76pame = 'M' and dsttpm <>'M')
009500070618     c                   iter
009600070618     c                   endif
009700070830      * richieste solo distinte mattina e pomeriggio
009800070907     c                   if        (or76FPPI = 'P' and dstfpp <>'P') or
009900070907     c                             (or76FPPI = 'M' and dstfpp = 'P')
010000070830     c                   iter
010100070830     c                   endif
010200070618      *
010300070213     c     kapd          chain     fiapd01l                           97
010400070213      *se l'anagrafica non corrisponde non stampo la decodifica
010500070213     c   97              clear                   despdr           25
010600070213     c  n97              movel     apdrsc        despdr
010700070214      *reperisco descrizione primo giro distinta
010800070214     c                   clear                   desgir
010900070907     c     kdst5         chain     fidst51l                           97
011000070907     c                   if        %found(fidst51l)
011100070214     c     kgrg          setll     figrg03l
011200070214     c                   do        *hival
011300070214     c     kgrg          reade     figrg03l
011400070214     c                   if        %eof(figrg03l)
011500070214     c                   leave
011600070214     c                   endif
011700070706     c                   eval      dgrgflo = grgflo
011800070214     c                   if        dstdfv >= grgdde and dstdfv <= grgdscad
011900070706      * verifico tipo utilizzo giro
012000071011     c                             and (§grgtug = 'R' or §grgtug = 'E')
012100070214     c                   movel     grgdes        desgir
012200070214     c                   endif
012300070214     c                   enddo
012400070214     c                   endif
012500070214
012600030724     c                   if        line > 60
012700030724     c                   except    testata
012800030724     c                   endif
012900070214      * calcolo le differenze
013000070214     c                   clear                   diffpkl
013100070214     c                   clear                   diffvlu
013200070214     c                   clear                   diffnftl
013300070221     c                   clear                   diffnbl
013400070907     c     dstrpkla      sub       dstrpkl       diffpkl
013500070907     c     dstrvlua      sub       dstrvlu       diffvlu
013600070907     c     dstrstpa      sub       dstrnftl      diffnftl
013700070330
013800070330     c     diffpkl       comp      0                                  30
013900070330     c     diffvlu       comp      0                                  31
014000070330     c     diffnftl      comp      0                                  32
014100070510      * totalizzo
014200070907     c                   add       dstrpkla      tdstpkla
014300070907     c                   add       dstrvlua      tdstvlua
014400070907     c                   add       dstrstpa      tdststpa
014500070907     c                   add       dstrnbla      tdstnbla
014600070214     c
014700070213     c                   except    detta
014800030724     c
014900020304     c                   enddo
015000070510     c                   except    totalia
015100020304
015200020304     c                   endsr
015300981029      *
015400070214     c*-------------------------------------------------
015500070911     c     fnorg         begsr
015600070214     c*-------------------------------------------------
015700070911      * se il p.o. è in parallelo testo la distinta su ORG altrimenti
015800070911      * se il p.o. è partito testo la distinta anche su ORM
015900070911      *
016000070911      *
016100070911     c                   if        ogddan > wdymd
016200070911     C/EXEC SQL
016300070911     C+ DECLARE A1 CURSOR FOR SELECT rrn(fnorg01l), rrn(fnorm01l),
016400070911     C+ fnorg01l.*, fnorm01l.*
016500070911     C+ FROM fnorg01l join fnorm01l on ormpoe=orgpoe
016600070911     C+ and ormnsr=orgnsr and ormnor=orgnor and ormnrv=orgnrv
016700070917     C+ WHERE orgndc = 0 and ormpor=:or76fil
016800070911     C+ ORDER BY
016900070911     C+ Fnorg01l.orgpocgi, Fnorg01l.orgcgi
017000070911     C/END-EXEC
017100070911
017200070911     C/EXEC SQL
017300070911     C+ OPEN a1
017400070911     C/END-EXEC
017500070911     c                   else
017600070911     c* oltre a ORMNDC = 0 testo anche ORGNDC=0 per evitare di rielaborare
017700070911     c* gli ORM  già inseriti nelle precedenti distinte
017800070911     C/EXEC SQL
017900070911     C+ DECLARE B1 CURSOR FOR SELECT rrn(fnorg01l), rrn(fnorm01l),
018000070911     C+ fnorg01l.*, fnorm01l.*
018100070911     C+ FROM fnorg01l join fnorm01l on ormpoe=orgpoe
018200070911     C+ and ormnsr=orgnsr and ormnor=orgnor and ormnrv=orgnrv
018300070917     C+ WHERE orgndc = 0 and ormpor=:or76fil
018400071105     C+ and ormndc = 0 and ormeti <> ' ' ORDER BY
018500070911     C+ Fnorg01l.orgpocgi, Fnorg01l.orgcgi
018600070911     C/END-EXEC
018700070911
018800070911     C/EXEC SQL
018900070911     C+ OPEN b1
019000070911     C/END-EXEC
019100070911     c                   end
019200070911
019300071010do  1C                   do        *hival
019400070911
019500070911     c                   if        ogddan > wdymd
019600070911     C/EXEC SQL
019700070911     C+ FETCH NEXT FROM a1 INTO :rrnorg, :rrnorm, :fnorgds, :fnormds
019800070911     C/END-EXEC
019900070911     c                   else
020000070911     C/EXEC SQL
020100070911     C+ FETCH NEXT FROM b1 INTO :rrnorg, :rrnorm, :fnorgds, :fnormds
020200070911     C/END-EXEC
020300070911     c                   end
020400070911
020500070911     c                   select
020600070911     c                   when      sqlcod = 100
020700070911     c                   leave
020800070911     c                   WHEN      SqlCod < 0
020900070911     c                   if        sqlcod <> -913
021000070911     c                   seton                                        H1
021100070911     c                   end
021200070911     c                   other
021300070911     c                   exsr      gesorm
021400070911     c                   endsl
021500070911
021600071010e   1c                   enddo
021700070911
021800070911     c                   if        ogddan > wdymd
021900070911     C/EXEC SQL
022000070911     C+ CLOSE a1
022100070911     C/END-EXEC
022200070911     c                   else
022300070911     C/EXEC SQL
022400070911     C+ CLOSE b1
022500070911     C/END-EXEC
022600070911     c                   end
022700070912
022800070912     c                   exsr      ultimo
022900071010      *
023000071010      * Chiusura prtf di dettaglio ORM da assegnare
023100071011     c                   if        OR76detta = 'S'
023200071011     c                   movel(p)  FIOR76ds      KPJBU
023300071010     c*** NO 2° parm:    clear                   FNORMds
023400071010     c*** NO 3° parm:    clear                   FNORGds
023500071010     c                   call      'FIOR76R1'
023600071010     c                   parm                    KPJBA
023700071010     c*** NO 2° parm:    parm                    FNORMds
023800071010     c*** NO 3° parm:    parm                    FNORGds
023900071011     c                   endif
024000070330     c
024100070911     c                   endsr
024200070911      *
024300070911     c*-------------------------------------------------
024400070911     c     gesorm        begsr
024500070911     c*-------------------------------------------------
024600070911     c                   exsr      buono
024700070911     c                   if        or64asso = 'N'
024800070912     c                   leavesr
024900070214     c                   endif
025000070214      * primo rekord
025100070214     c                   if        primo = *blank
025200070912     c                   move      orgcgi        exorgcgi
025300070214     c                   move      'X'           primo
025400070214     c                   endif
025500071010if  1c                   if        exorgcgi = orgcgi
025600070911     c                   add       orgpkg        totpkl
025700070911     c                   add       orgvlm        totvlu
025800070911     c                   add       1             totorm
025900071010x   1c                   else
026000070214      *reperisco descrizione primo giro distinta
026100070214     c                   clear                   desgir
026200070214     c     kgrg2         setll     figrg03l
026300071010do  2c                   do        *hival
026400070214     c     kgrg2         reade     figrg03l
026500070214     c                   if        %eof(figrg03l)
026600070214     c                   leave
026700070214     c                   endif
026800070706     c                   eval      dgrgflo = grgflo
026900070214     c                   if        datk >= grgdde and datk <= grgdscad
027000070706      * verifico tipo utilizzo giro
027100071011     c                             and (§grgtug = 'R' or §grgtug = 'E')
027200070214     c                   movel     grgdes        desgir
027300070214     c                   endif
027400071010e   2c                   enddo
027500070214
027600070215      *reperisco distinte con giro nella giornata in esame
027700070215     c                   clear                   dis
027800070215     c                   clear                   pta
027900070215     c                   z-add     0             cn
028000070911     c     kdst52        setll     fidst52l
028100071010do  2c                   do        *hival
028200070911     c     kdst52        reade     fidst52l
028300070911     c                   if        %eof(fidst52l)
028400070215     c                   leave
028500070215     c                   endif
028600070329      * scarta distinte annullate
028700070911     c                   if        dst5atb = 'A'
028800070329     c                   iter
028900070329     c                   endif
029000070215     c                   add       1             cn
029100070215     c                   if        cn > 5
029200070215     c                   leave
029300070215     c                   endif
029400070911     c                   z-add     dst5nfv       dis(cn)
029500070911     c                   z-add     dst5prgg      pta(cn)
029600071010e   2c                   enddo
029700070215
029800070214     c                   if        line > 60
029900070214     c                   except    testatab
030000070214     c                   endif
030100070214     c                   except    dettab
030200070214     c                   add       totpkl        ttotpkl
030300070214     c                   add       totvlu        ttotvlu
030400070911     c                   add       totorm        ttotorm
030500070214      * memorizza valori prima bolla del nuovo giro
030600070912     c                   move      orgcgi        exorgcgi
030700070911     c                   z-add     orgpkg        totpkl
030800070911     c                   z-add     orgvlm        totvlu
030900070911     c                   z-add     1             totorm
031000071010e   1c                   endif
031100071010      *
031200071010      * Richiesta stampa del dettaglio ORM
031300071010     c                   if        OR76detta = 'S'
031400071010     c                   exsr      CallFIOR76R1
031500071010     c                   endif
031600071010      *
031700070214     c                   endsr
031800070912      *
031900070912      *-----------------------------------------------
032000070912     C     ultimo        begsr
032100070912      *-----------------------------------------------
032200070912      * ultima riga
032300070912      *reperisco descrizione ultimo giro distinta
032400070912     c                   clear                   desgir
032500070912     c     kgrg2         setll     figrg03l
032600070912     c                   do        *hival
032700070912     c     kgrg2         reade     figrg03l
032800070912     c                   if        %eof(figrg03l)
032900070912     c                   leave
033000070912     c                   endif
033100070912     c                   if        datk >= grgdde and datk <= grgdscad
033200070912     c                   movel     grgdes        desgir
033300070912     c                   endif
033400070912     c                   enddo
033500070912
033600070912      *reperisco distinte con giro nella giornata in esame
033700070912     c                   clear                   dis
033800070912     c                   clear                   pta
033900070912     c                   z-add     0             cn
034000070912     c     kdst52        setll     fidst52l
034100070912     c                   do        *hival
034200070912     c     kdst52        reade     fidst52l
034300070912     c                   if        %eof(fidst52l)
034400070912     c                   leave
034500070912     c                   endif
034600070912      * scarta distinte annullate
034700070912     c                   if        dst5atb = 'A'
034800070912     c                   iter
034900070912     c                   endif
035000070912     c                   add       1             cn
035100070912     c                   if        cn > 5
035200070912     c                   leave
035300070912     c                   endif
035400070912     c                   z-add     dst5nfv       dis(cn)
035500070912     c                   z-add     dst5prgg      pta(cn)
035600070912     c                   enddo
035700070912
035800070912
035900070912     c                   if        line > 60
036000070912     c                   except    testatab
036100070912     c                   endif
036200070912     c                   except    dettab
036300070912     c                   add       totpkl        ttotpkl
036400070912     c                   add       totvlu        ttotvlu
036500070912     c                   add       totorm        ttotorm
036600070912     c                   except    totali
036700070912
036800070912     c                   endsr
036900070214      *
037000070214      *-----------------------------------------------
037100070911     C     buono         begsr
037200070214      *-----------------------------------------------
037300070911      * richiamo pgm per esito ritirabilità
037400070911      * per controllare se ORM assegnabile richiamo pgm esterno
037500070911     c                   clear                   fior64ds
037600070911     c                   eval      or64poei = ormpoe
037700070911     c                   eval      or64nsri = ormnsr
037800070911     c                   eval      or64nori = ormnor
037900070911     c                   eval      or64nrvi = ormnrv
038000070911      * deve essere impostata la data
038100070911     c                   eval      or64dfvi = datk
038200070911     c                   eval      or64dari = 'S'
038300070911     c                   if        wdymd < ogddaN
038400070911     c                   eval      or64ndci = 'G'
038500070911     c                   eval      or64etii = 'S'
038600070911     c                   else
038700070911     c                   eval      or64ndci = 'E'
038800070911     c                   endif
038900070911     c                   call      'FIOR64R'
039000070911     c                   parm                    kpjba
039100070911     c                   parm                    fior64ds
039200070911
039300070214     C                   ENDSR
039400071010      *-------------------------------------------------
039500071010      * Stampa dettaglio ORM da assegnare
039600071010      *-------------------------------------------------
039700071010     c     CallFIOR76R1  begsr
039800071010      *
039900071010     c                   movel(p)  FIOR76ds      KPJBU
040000071010      *
040100071010     c                   call      'FIOR76R1'
040200071010     c                   parm                    KPJBA
040300071010     c                   parm                    FNORMds
040400071010     c                   parm                    FNORGds
040500071010      *
040600071010     c                   endsr
040700011026     c*-------------------------------------------------
040800011026     c     *inzsr        begsr
040900011026     c*-------------------------------------------------
041000020304     c     *entry        plist
041100020304     c                   parm                    kpjba
041200070911     c                   movel     kpjbu         fior76ds
041300070329      * inizializzo parametri di stampa elenchi a 'S' se non impostati
041400070911     c                   if        or76orma = *blank
041500070911     c                   move      'S'           or76orma
041600070329     c                   endif
041700070911     c                   if        or76noas = *blank
041800070911     c                   move      'S'           or76noas
041900070329     c                   endif
042000070329      *
042100020218     C                   time                    w0120            14 0
042200020218     C                   move      w0120         wdat              8 0
042300070222     C                   movel     w0120         wora              6 0
042400070403     c                   move      wdat          dataeur
042500070403     c                   move      dataeur       dataiso
042600070403     c                   move      dataiso       wdymd             8 0
042700070403
042800020404     c                   z-add     99            line
042900070213     c     kdst          klist
043000070213     c                   kfld                    filk              3 0
043100070213     c                   kfld                    catk              1 0
043200070213     c                   kfld                    datk              8 0
043300070911     c                   z-add     or76fil       filk
043400070213     c                   z-add     4             catk
043500070911     c                   z-add     or76dat       datk
043600070213
043700070911     c     kdst5         klist
043800070214     c                   kfld                    dstfgs
043900070214     c                   kfld                    dstnfv
044000070214     c                   kfld                    dstdfv
044100070214
044200070911     c     kdst52        klist
044300070215     c                   kfld                    filk
044400070215     c                   kfld                    datk
044500070223     c                   kfld                    filk
044600070912     c                   kfld                    exorgcgi
044700070215
044800070214     c     kgrg          klist
044900070911     c                   kfld                    dst5fgs
045000070911     c                   kfld                    dst5cgi
045100070214
045200070214     c     kgrg2         klist
045300070214     c                   kfld                    filk
045400070912     c                   kfld                    exorgcgi
045500070214
045600070213     c     kapd          klist
045700070213     c                   kfld                    tipk              1
045800070213     c                   kfld                    dstpdr
045900070213     c                   movel     'A'           tipk
046000030725
046100070214
046200030505     C                   Z-ADD     1             CODUT
046300030505     C                   CALL      'X§PARUT'
046400030505     C                   PARM                    UT§DSE
046500030505     C                   MOVEL     RAGUT         RSUT             20
046600030505     C                   MOVEL     REC80         CNCR80
046700070213     c                   move      datk          dataiso
046800070213     c                   move      dataiso       dataeur
046900070222     c                   move      *all'_'       separa          192
047000070213      * decodifica p.o.
047100070403     c                   clear                   desfil
047200070403     c                   clear                   ogddaN            8 0
047300070403     c     filk          chain     azorg01l
047400070403     c                   if        %found(azorg01l)
047500070911     c                   movel     orgde7        og147
047600070911     c                   if        §ogddao <> ' '
047700070911     c                   move      §ogddao       ogddaN
047800070403     c                   endif
047900070403     c                   movel     orgdes        desfil           20
048000070403     c                   endif
048100170728
048200170728     C* ESEGUO L'OVERRIDE DEL FILE DI STAMPA
048300170728     C                   MOVEL(p)  CMD(1)        COMMAN           80
048400170728     C                   Z-ADD     80            LUNG             15 5
048500170728     C                   CALL      'QCMDEXC'
048600170728     C                   PARM                    COMMAN
048700170728     C                   PARM                    LUNG
048800170728     C                   OPEN      prtf198
048900070403
049000011026     c                   endsr
049100020404     C*--------------------------------------------------------------------
049200070214     Oprtf198   E            testata        1 02
049300070222     o                       rsut                21
049400070222     o                       knmus               33
049500070327     o                                          100 'Situazione Assegnazione'
049600070912     o                                        +   2 'Ritiri Distinte del'
049700070215     o                       dataeur          +   2
049800070912     o                                          153 'FIOR76R'
049900070510     o                       wdat          y    170
050000070510     o                       wora               181 '  :  :  '
050100070222     o                                          190 'Pag.'
050200070222     O                       page          z    196
050300070223     O          E            testata     1
050400070223     o                                           13 'P.O Gestione'
050500070223     o                       filk             +   2
050600070223     o                       desfil           +   2
050700070223
050800070222     O          E            testata     1
050900070222     o                       separa             192
051000070223     O          E            testata     1
051100070223     o                                           73 '|'
051200070330     o                                           84 'TOLLERANZA'
051300070912     o                                          100 '   ORM    '
051400070223     o                                          102 '|'
051500070917     o                                          120 'FERMATE'
051600070223     o                                          132 '|'
051700070223     o                                          150 'VOLUME'
051800070223     o                                          162 '|'
051900070223     o                                          179 'PESO '
052000070223     o                                          192 '|'
052100070223     O          E            testata     1
052200070213     o                                           10 'Distinta'
052300070213     o                                        +   2 'Autotrasportatore'
052400070213     o                                           41 'M/P'
052500070228     o                                           60 'Descrizione 1°giro'
052600070222     o                                           73 '|'
052700070330     o*                                          80 'Nr. Max'
052800070330     o                                           80 ' %  Mag'
052900070912     o                                          100 'Assegnati'
053000070330     o*                                         101 'Differ.'
053100070222     o                                          102 '|'
053200070223     o                                          110 'Nr. Max '
053300070223     o                                          121 'Assegnate'
053400070223     o                                          131 'Differ.'
053500070222     o                                          132 '|'
053600070223     o                                          140 'Mc. Max '
053700070223     o                                          151 'Assegnati'
053800070223     o                                          161 'Differ.'
053900070222     o                                          162 '|'
054000070223     o                                          171 'Kg. Max '
054100070223     o                                          181 'Assegnati'
054200070223     o                                          191 'Differ.'
054300070222     o                                          192 '|'
054400070222     O          E            testata     1
054500070222     o                       separa             192
054600070222     O          E            detta       1
054700070222     o                                           73 '|'
054800070222     o                                          102 '|'
054900070222     o                                          132 '|'
055000070222     o                                          162 '|'
055100070222     o                                          192 '|'
055200070222     O          E            detta       1
055300070213     o                       dstnfv        z      9
055400070213     o                       despdr              37
055500070213     o                       dstfpp              40
055600070214     o                       desgir              72
055700070222     o                                           73 '|'
055800070912     o                       dstrtol       2     81
055900070912     o                       dstrnbla      2    100
056000070222     o                                          102 '|'
056100070912     o                       dstrnftl      z    111
056200070912     o                       dstrstpa      z    121
056300070912     o                       diffnftl      k    131
056400070222     o                                          132 '|'
056500070912     o                       dstrvlu       2    141
056600070912     o                       dstrvlua      2    151
056700070912     o                       diffvlu       k    161
056800070222     o                                          162 '|'
056900070912     o                       dstrpkl       2    171
057000070912     o                       dstrpkla      2    181
057100070221     o                       diffpkl       k    191
057200070222     o                                          192 '|'
057300070215     O          E            detta       0
057400070330     o               32      diffnftl      k    131
057500070330     o               31      diffvlu       k    161
057600070330     o               30      diffpkl       k    191
057700070215     O          E            detta       0
057800070330     o               32      diffnftl      k    131
057900070330     o               31      diffvlu       k    161
058000070330     o               30      diffpkl       k    191
058100070510     O          E            totalia     1
058200070510     o                                           73 '|'
058300070510     o                                          102 '|'
058400070510     o                                          132 '|'
058500070510     o                                          162 '|'
058600070510     o                                          192 '|'
058700070510     O          E            totalia     1
058800070912     o                                           30 'Totale ORM  -
058900070912     o                                              assegnati '
059000070510     o                                           73 '|'
059100070510     o                       tdstnbla      2    100
059200070510     o                                          102 '|'
059300070510     o                       tdststpa      z    121
059400070510     o                                          132 '|'
059500070510     o                       tdstvlua      2    151
059600070510     o                                          162 '|'
059700070510     o                       tdstpkla      2    181
059800070510     o                                          192 '|'
059900070510     O          E            totalia     0
060000070912     o                                           30 'Totale ORM  -
060100070912     o                                              assegnati '
060200070510     o                                           73 '|'
060300070510     o                       tdstnbla      2    100
060400070510     o                                          102 '|'
060500070510     o                       tdststpa      z    121
060600070510     o                                          132 '|'
060700070510     o                       tdstvlua      2    151
060800070510     o                                          162 '|'
060900070510     o                       tdstpkla      2    181
061000070510     o                                          192 '|'
061100070510     O          E            totalia     0
061200070912     o                                           30 'Totale ORM  -
061300070912     o                                              assegnati '
061400070510     o                                           73 '|'
061500070510     o                       tdstnbla      2    100
061600070510     o                                          102 '|'
061700070510     o                       tdststpa      z    121
061800070510     o                                          132 '|'
061900070510     o                       tdstvlua      2    151
062000070510     o                                          162 '|'
062100070510     o                       tdstpkla      2    181
062200070510     o                                          192 '|'
062300070214      *__totali Giri non asssegnati______________________________
062400070214     O          E            testatab       1 02
062500070223     o                       rsut                21
062600070215     o                       knmus            +   2
062700070912     o                                          110 'ORM da ritirare non assegn-
062800070912     o                                              ati alle'
062900070215     o                                        +   2 'Distinte del'
063000070215     o                       dataeur          +   2
063100070912     o                                          150 'FIOR76R'
063200070510     o                       wdat          y    170
063300070510     o                       wora               181 '  :  :  '
063400070222     o                                          190 'Pag.'
063500070222     O                       page          z    196
063600070223     O          E            testatab    1
063700070223     o                                           13 'P.O Gestione'
063800070223     o                       filk             +   2
063900070223     o                       desfil           +   2
064000070223
064100070222     O          E            testatab    1
064200070222     o                       separa             192
064300070214     O          E            testatab    2
064400070214     o                                            5 'Giro'
064500070214     o                                           23 'Descrizione'
064600070912     o                                           53 'Tot ORM '
064700070912     o*                                          63 'Tot Colli'
064800070222     o                                           73 'Tot Vol.'
064900070222     o                                           83 'Tot Peso'
065000070222     o                                        +  30 'Distinte in cui il giro -
065100070215     o                                              e'' stato pianificato'
065200070215     O          E            testatab    1
065300070222     o                                           85 '|'
065400070215     o                                           93 'Numero'
065500070215     o                                        +   1 'Ptà'
065600070222     o                                        +   1 '|'
065700070215     o                                        +   3 'Numero'
065800070215     o                                        +   1 'Ptà'
065900070222     o                                        +   1 '|'
066000070215     o                                        +   3 'Numero'
066100070215     o                                        +   1 'Ptà'
066200070222     o                                        +   1 '|'
066300070215     o                                        +   3 'Numero'
066400070215     o                                        +   1 'Ptà'
066500070222     o                                        +   1 '|'
066600070215     o                                        +   3 'Numero'
066700070215     o                                        +   1 'Ptà'
066800070222     o                                        +   1 '|'
066900070222     O          E            testatab    1
067000070222     o                       separa             192
067100070222     O          E            dettab      1
067200070222     o                                           85 '|'
067300070222     o                                        +  13 '|'
067400070222     o                                        +  14 '|'
067500070222     o                                        +  14 '|'
067600070222     o                                        +  14 '|'
067700070222     o                                        +  14 '|'
067800070222     O          E            dettab      1
067900070912     o                       exorgcgi            11
068000070214     o                       desgir              42
068100070912     o                       totorm        2     53
068200070912     o*                      totcol        2     63
068300070222     o                       totvlu        2     73
068400070222     o                       totpkl        2     83
068500070222     o                                           85 '|'
068600070222     o                       dis(1)        z  +   2
068700070215     o                       pta(1)        z  +   2
068800070222     o                                        +   1 '|'
068900070215     o                       dis(2)        z  +   3
069000070215     o                       pta(2)        z  +   2
069100070222     o                                        +   1 '|'
069200070215     o                       dis(3)        z  +   3
069300070215     o                       pta(3)        z  +   2
069400070222     o                                        +   1 '|'
069500070215     o                       dis(4)        z  +   3
069600070215     o                       pta(4)        z  +   2
069700070222     o                                        +   1 '|'
069800070215     o                       dis(5)        z  +   3
069900070215     o                       pta(5)        z  +   2
070000070222     o                                        +   1 '|'
070100070214     O          E            totali      2
070200070912     o                                           33 'Totale ORM non -
070300070912     o                                              assegnati '
070400070912     o                       ttotorm       2     53
070500070912     o*                      ttotcol       2     63
070600070222     o                       ttotvlu       2     73
070700070222     o                       ttotpkl       2     83
070800170728**
070900170728OVRPRTF FILE(PRTF198) HOLD(*YES)
071000170728DLTOVR FILE(PRTF198)
