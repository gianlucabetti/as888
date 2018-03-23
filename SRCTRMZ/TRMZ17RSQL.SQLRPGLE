000100011210      *PARMS DFTACTGRP(*YES)
000200011210      *PARMS COMMIT(*NONE) DATFMT(*ISO)
000300011210      *PARMS CLOSQLCSR(*ENDMOD)
000400011210      *PARMS DYNUSRPRF(*OWNER)
000500011219     H DECEDIT('0,') DATEDIT(*DMY.)
000600011206     Fmzamh01l  IF   E           k DISK
000700141203     Fwrkame1l  IF   E           k DISK
000800011210     Fmzamh04l  IF   E           k DISK    RENAME(MZAMH000:MZAMH004)
000900141203     Fwrkame4l  IF   E           k DISK    RENAME(MZAME000:MZAME004)
001000160902     FmZimp01l  IF   E           k DISK
001100011205     FAZORG01L  IF   E           K DISK
001200141120     FTABEL00F  IF   E           K DISK
001300160518     FTntbe01l  IF   E           K DISK
001400141121     Fwmzame0F  uf a E             DISK    usropn
001500011205     FPRTF198   O    F  198        PRINTER OFLIND(*INOF)
001600011210     D WrkSqlCmd       S           1024
001700011210     D data1           S              8  0
001800011210     D data2           S              8  0
001900011210     D CON             S              5  0 DIM(12)
002000141121     D CONr            S              5  0
002100141121     D CONt            S              9  0 DIM(12)
002200020717     D QTA             S                   DIM(12) like(impqta)
002300020717     D KM              S                   DIM(12) like(impkm)
002400020710     D KMM             S              5  0 DIM(12)
002500141121     D KMMr            S              5  0
002600141121     D KMMt            S              9  0 DIM(12)
002700011205     D MED             S              4  1 DIM(12)
002800011205     D MEM             S              4  1 DIM(12)
002900141121     D MEmr            S              4  1
003000141121     D MEmt            S              4  1 DIM(12)
003100141125     D cat             S              2    DIM(20) inz
003200011205     D st              S              3    DIM(12) CTDATA PERRCD(1)
003300141120     d file            s             10
003400141120     D cmd1            S              1    DIM(99) CTDATA PERRCD(99)
003500141120     D cmd2            S              1    DIM(99) CTDATA PERRCD(99)
003600141120     D cmd3            S              1    DIM(99) CTDATA PERRCD(99)
003700141120     D cmd4            S              1    DIM(99) CTDATA PERRCD(99)
003800141120     D cmd5            S              1    DIM(99) CTDATA PERRCD(99)
003900141120     D cmd6            S              1    DIM(99) CTDATA PERRCD(99)
004000020710     D*impkms          S                   like(impkm)
004100011205     D dtaiso          S               d   datfmt(*iso)
004200011205     D dalame          S               d   datfmt(*iso)
004300011205     D dalames         S               d   datfmt(*eur)
004400011206     D dtaeur          S               d   datfmt(*eur)
004500141121     d coman           s             99
004600141121     d luncmd          s             15  5
004700011205     d param           ds
004800011205     d anno                           4  0
004900011205     d mese                           2  0
005000011205     d vscef                          1
005100011205     d vscec                          1
005200011205     d vscea                          1
005300011205     d vscet                          1
005400011205     d vfil                           3  0
005500141125     d vcat                          20
005600011205     d vaus                           2
005700011205     d vtga                           2
005800011205     d vtgn                           9
005900141125     d vvend                          1
006000160810     d vkmdal                         6  0
006100160810     d vkmal                          6  0
006200160810     d vcaut                          2
006300160902     d vfun                           5
006400160810      *
006500011205     D KPJBA         E DS
006600011205     D UTEDSE        E DS                  EXTNAME(UTEDSE0F)
006700011205     D wrkame0f      E DS
006800011205     D mzame         E DS                  extname(wrkame0f) prefix($)
006900011205     D DSXN          E DS
007000011207     D DSXH          E DS
007100160518     D  dmau         E DS
007200160518     D  dcau         E DS
007300011205     c* Lettura WRKAME00F
007400011210     C/EXEC SQL
007500011210     C+ PREPARE S1 FROM :WrkSqlCmd
007600011210     C/END-EXEC
007700011210
007800011210     C/EXEC SQL
007900011210     C+ DECLARE A1 CURSOR FOR S1
008000011210     C/END-EXEC
008100011210
008200011210     C/EXEC SQL
008300011210     C+ OPEN A1
008400011210     C/END-EXEC
008500011210
008600011210     C                   DOU       SqlCod <> 0
008700011205     C/EXEC SQL
008800011205     C+ FETCH NEXT FROM A1 INTO :mzame
008900011205     C/END-EXEC
009000011205     c*
009100011210     C                   SELECT
009200011210     C                   WHEN      SqlCod = 0
009300011210     c                   if        (vscea <> *blanks or vscet <> *blanks)
009400011210     c                             and $amedal>data2
009500011210     c                   iter
009600011210     c                   end
009700141125     c                   if        vcat <> *blank
009800141125     c     $amecat       lookup    cat                                    89
009900141216     c  n89              iter
010000141125     c                   endif
010100160902      *
010200011205     C                   EXSR      rotture
010300160902      * verifica funzione se richiesta
010400160902     c                   if        vfun <> *blank  and
010500160902     c                             Vfun <> §xnfun
010600160902     c                   iter
010700160902     c                   endif
010800011212     c* salto le persone generiche
010900011212     c                   if        vscea <> *blanks and §xnper = 'G'
011000011212     c                   iter
011100011212     c                   end
011200011205     C                   EXSR      calcoli
011300011207     c                   if        vscea <> ' ' or vscet <> ' '
011400011206     C                   MOVEL     $AMEdal       ameals
011500011205     C                   ENDif
011600011210     C                   WHEN      SqlCod = 100
011700011210     C                   WHEN      SqlCod <> 0
011800011210      * Forzo la stampa del JOBLOG.
011900011210     C                   CALL      'X66CHGJOB'
012000011210     C                   ENDSL
012100011205     C                   ENDDO
012200011210
012300011205     C/EXEC SQL
012400011205     C+ CLOSE A1
012500011205     C/END-EXEC
012600011205     c*
012700141120     c*                  endif
012800141121      *stampa riepilogo azindale
012900141121      * calcolo media generale
013000141121     C                   do        12            b                 2 0
013100141121     C                   Z-ADD     0             SV092
013200141121     c                   if        kmmt(b) = 0
013300141121     c                   iter
013400141121     c                   end
013500141121     C     cont(b)       DIV       kmmt(b)       comodo           13 5
013600141121     C     comodo        MULT      100           comodo
013700141121     C                   Z-ADD     comodo        memt(b)
013800141121     c                   enddo
013900141121     c                   xfoot     kmmt          kmmtt             9 0
014000141121     c                   xfoot     cont          contt             9 0
014100141121     c                   if        kmmtt > 0
014200141121     c     contt         div       kmmtt         comodo
014300141121     c     comodo        mult      100           comodo
014400141121     c                   z-add     comodo        memtt             4 1
014500141121     c                   endif
014600141121      *
014700141121     c                   except    totale
014800141121      *
014900141120      * operazioni di fine duplica da file in qtemp a libreria utente
015000141120      *verifica se esiste file in libreria di wrk per utente
015100141120     c                   movea     cmd4          coman
015200141120     c                   eval      %subst(coman:22:10) = file
015300141121     c                   exsr      comandi
015400141120      *se trova il file esegue copy file altrimenti lo crea
015500141120     c                   if        *in34
015600141120     c                   movea     cmd5          coman
015700141120     c                   eval      %subst(coman:78:10) = file
015800141121     c                   exsr      comandi
015900141120     c                   else
016000141120     c                   movea     cmd6          coman
016100141120     c                   eval      %subst(coman:41:10) = file
016200141121     c                   exsr      comandi
016300141121     c                   endif
016400011205     C                   seton                                        lr
016500011204     C******************************************************
016600011204     C     calcoli       BEGSR
016700011205     C******************************************************
016800011205     c                   exsr      srini
016900011205     c*
017000141205     c                   clear                   consumo           1
017100011206     C                   Z-ADD     0             FILAA
017200011206     C                   SETOFF                                       61
017300011205     c                   move      $amedal       dalame
017400011205     c                   move      dalame        dalames
017500011205     C                   move      data1         DATA
017600011210     c                   if        (vscea <> *blanks or vscet <> *blanks)
017700011210     c                             and $amedal > data1
017800011205     c                   move      $amedal       data
017900011205     c                   end
018000011210     C     KIMP          SETLL     MZIMP000
018100011204     C                   do        *hival
018200011210     C     KIMP1         READE     MZIMP000                               02
018300011204     C   02              leave
018400011205     c                   if        impdat > data2
018500011205     c                   leave
018600011205     c                   end
018700011207     c                   if        impdat >= ameals and (vscea <> *blanks or
018800011207     c                             vscet <> *blanks)
018900011205     c                   leave
019000011205     c                   end
019100160810      * verifico i km se richiesti e fuori range
019200160902     c*                  if        vkmdal <> 0
019300160902     c*                  if        impkm < vkmdal or impkm > vkmal
019400160902     c*                  iter
019500160902     c*                  end
019600160902     c*                  end
019700141219     c* se non ho i consumi non conteggio nemmeno i km
019800141219     c                   if        impqta = 0
019900141219     c                   iter
020000141219     c                   end
020100141219     c*
020200011205     c                   move      impdat        dtaiso
020300011205     C                   extrct    dtaiso:*m     mm                2 0
020400011205     C                   extrct    dtaiso:*y     aa                4 0
020500011205     C*ROTTURA anno stampo riga
020600011205     C                   IF        anno = 0 and aa  <> filaa and filaa <> 0
020700011205     c                   exsr      srsta
020800011205     c                   exsr      srini
020900011206     C                   SETON                                        61
021000011205     c                   end
021100011205     C                   z-add     aa            filaa             4 0
021200011205     C*
021300141205     c                   eval      consumo = '1'
021400011204     C                   ADD       IMPQTA        QTA(mm)
021500160902     C                   add       impkm         km(mm)
021600011204     c                   enddo
021700141205      * se non ha consumi forzo la rottura abblencando autista
021800141205     c                   if        consumo = *blank
021900141205     c                   move      '??'          filcau
022000141205     c                   move      '??'          filcaT
022100141205     c                   endif
022200011205     c* stampa riga
022300011205     c                   exsr      srsta
022400011204     C                   ENDSR
022500011205     C******************************************************
022600011205      * stampa riga
022700011205     C******************************************************
022800011205     C     srsta         BEGSR
022900011207     c                   if        vscea <> ' ' or vscet <> *blanks
023000011206     c                   move      ameals        dtaiso
023100011206     c                   move      dtaiso        dtaeur
023200011206     c                   end
023300011205     C*CALCOLO MEDIA DINAMICA E MEDIA MENSILE
023400020717     C                   XFOOT     QTA           TOTQTA           11 2
023500011205     c                   if        totqta <> 0
023600011205     C                   EXSR      MEDDIN
023700011205     C                   XFOOT     CON           TOTCON            9 0
023800011205     c                   if        totcon <> 0
023900011205     C                   EXSR      stampa
024000011205     C                   ENDIF
024100011205     C                   ENDIF
024200011205     C                   ENDSR
024300011205     C******************************************************
024400011205      * Rottura chiave.
024500011205     C******************************************************
024600011205     C     Rotture       BEGSR
024700011219     c                   setoff                                       66
024800011212     c                   do
024900011205     C*ROTTURA FILIALE
025000011205     C   55$AMEFIL       IFNE      FILPRE
025100011205     C     $AMEFIL       CHAIN     AZORG                              10
025200011205     C                   MOVEL     *BLANKS       DESFIL           20
025300011205     C  N10              MOVEL     ORGDES        DESFIL
025400011220     c                   if        vscef = 'P' or filpre = 0
025500011205     C                   Z-ADD     70            CONRIG            3 0
025600011219     c                   else
025700011219     c                   seton                                        66
025800011219     C                   END
025900011205     C                   MOVEL     $AMEFIL       FILPRE            3 0
026000011205     C                   END
026100011205     C*ROTTURA CATEGORIA
026200011205     C   56$AMECAT       IFNE      FILCAT
026300011205     C                   MOVEL     'XA'          COD
026400011205     C                   MOVEL     *BLANKS       KEY
026500011205     C                   MOVEL     $AMECAT       KEY
026600011205     C     KTAB          CHAIN     TABEL                              10
026700011205     C                   MOVEL     *BLANKS       DESCAT           20
026800011205     C  N10              MOVEL     TBLUNI        DESCAT
026900011220     c                   if        vscec = 'P' or filcat = *blanks
027000011205     C                   Z-ADD     70            CONRIG
027100011219     c                   else
027200011219     c                   seton                                        66
027300011219     C                   END
027400011205     C                   MOVEL     $AMECAT       FILCAT            2
027500011205     C                   END
027600011207     C** TABELLA CONSUMI
027700011212     C                   MOVEL     'XH'          COD
027800011212     C                   MOVEL     *BLANKS       KEY
027900011212     C                   MOVEL     $AMEUSI       KEY
028000011212     C     KTAB          CHAIN     TABEL                              10
028100011212     C   10              CLEAR                   DSXH
028200011212     C  N10              MOVEL     TBLUNI        DSXH
028300011212     C                   MOVEL     §XHMED        DESMED            4 1
028400011212     C     DESMED        MULT(H)   0,15          W010             10 1
028500011212     C     DESMED        ADD(H)    W010          DESMED
028600011207     C** TABELLA AUTISTA
028700011207     C                   MOVEL     'XN'          COD
028800011207     C                   MOVEL     *BLANKS       KEY
028900011207     C                   MOVEL     $AMECAU       KEY
029000011207     C     KTAB          CHAIN     TABEL                              10
029100011207     C   10              CLEAR                   DSXN
029200011207     C  N10              MOVEL     TBLUNI        DSXN
029300011207     C                   MOVEL     §XNDES        DESAUT           20
029400011207     C                   MOVEL     §XNDES        DESA30           30
029500011212     c* salto le persone generiche
029600011212     c                   if        vscea <> *blanks and §xnper = 'G'
029700011212     c                   leave
029800011212     c                   end
029900011210     C*ROTTURA AUTISTA/TARGA
030000011207     c                   if        vscea <> *blanks or vscet <> *blanks
030100011207     C                   IF        $AMECAU <> filcau   OR
030200011207     C                             $AMETGA <> filTGA   OR
030300011207     C                             $AMETGN <> filTGN
030400011210     c                   move      99991231      ameals            8 0
030500011207     c* CALCOLO FINO A CHE DATA L'AUTISTA HA TENUTO L'AUTO
030600011207     C* prima cerco la data in cui l'auto ha cambiato autista
030700011210     c                   move      99991231      amealn            8 0
030800011206     c     $amenrf       setll     mzamh01l
030900011206     c                   do        *hival
031000011206     c     $amenrf       reade     mzamh01l                               10
031100011206     c   10              leave
031200011210     c                   if        amhtip = 'AU' and amhdal < amealn
031300130411     c                             and amhdal >=$amedal and amhcau <> $amecau
031400011210     c                   move      amhdal        amealn
031500011206     c                   end
031600011206     c                   enddo
031700011207     c* controllo anche la data dell'anagrafico perchè potrebbe essere mino-
031800011207     c* re di quella dello storico
031900141203     c     $amenrf       chain     wrkame1l
032000141203     c                   if        %found(wrkame1l) and amecau <> $amecau
032100011210     c                             and amedal < amealn and
032200011210     c                             amedal >= $amedal
032300011210     c                   move      amedal        amealn
032400011206     c                   end
032500011211     C* se l'autista è persona generica salto il controllo altrimenti
032600011211     C* cerco la data in cui l'autista ha cambato auto
032700011210     c                   move      99991231      ameala            8 0
032800011211     c                   if        §xnper <> 'G'
032900011210     c     KCAU          setgt     mzamh04l
033000011210     c                   do        *hival
033100011210     c     $amecau       reade     mzamh04l                               10
033200011210     c   10              leave
033300011210     c                   if        amhtip = 'AU' and amhdal < ameala
033400130411     c                             and amhdal >=$amedal and amhnrf <> $amenrf
033500011210     c                   move      amhdal        ameala
033600011210     C                   LEAVE
033700011210     c                   end
033800011210     c                   enddo
033900011210     c* controllo anche la data dell'anagrafico perchè potrebbe essere mino-
034000011210     c* re di quella dello storico
034100141203     c     KCAU          setgt     wrkame4l
034200011211     c                   do        *hival
034300141203     c     $amecau       reade     wrkame4l                               10
034400011211     c   10              leave
034500011211     c                   if        amenrf <> $amenrf
034600011210     c                             and amedal < ameala and
034700011210     c                             amedal >= $amedal
034800011210     c                   move      amedal        ameala
034900011211     C                   LEAVE
035000011210     c                   end
035100011211     c                   endDO
035200011211     c                   end
035300011210     c* metto la data più piccola
035400011210     c                   if        ameala < amealn
035500011210     c                   move      ameala        ameals
035600011210     c                   else
035700011210     c                   move      amealn        ameals
035800011210     c                   end
035900011210     C*
036000011207     c                   end
036100011207     c*
036200011219     c                   if        vscea <> ' ' and $amecau <> filcau
036300011220     c                   if        vscea = 'P' or filcau = *blanks
036400011205     C                   Z-ADD     70            CONRIG
036500011219     c                   else
036600011219     c                   seton                                        66
036700011219     C                   END
036800011205     C                   MOVEL     $AMECAU       FILCAU            2
036900011207     C                   END
037000011207     C                   END
037100011205     C*ROTTURA TARGA
037200011207     C                   if        VSCET <> ' ' and (FILtgn <> $ametgn or
037300011210     C                             $ametga <> filtga)
037400011220     c                   if        vscet = 'P' or filtgn = *blanks
037500011205     C                   Z-ADD     70            CONRIG
037600011219     c                   else
037700011219     c                   seton                                        66
037800011219     C                   END
037900011205     C                   MOVEL     $AMEtga       FILtga            2
038000011205     C                   MOVEL     $AMEtgn       FILtgn            9
038100011205     C                   END
038200011205      *
038300011212     C                   ENDdo
038400011205     C                   ENDSR
038500011204     C******************************************************
038600011204     C** CALCOLO MEDIA DINAMICA
038700011204     C******************************************************
038800011204     C     MEDDIN        BEGSR
038900011204     C                   Z-ADD     0             SV092
039000011204     C**----------------------
039100011204     C** CALCOLO MEDIA MENSILE
039200011204     C**----------------------
039300011205     C                   do        12            a                 2 0
039400011204     C                   Z-ADD     0             SV092
039500011205     c                   if        km(a) = 0
039600011206     c                   iter
039700011205     c                   end
039800020710     c                   z-add     km(a)         kmm(a)
039900020710     C     QTA(a)        DIV       km(a)         SV092            11 5
040000011204     C     SV092         MULT      100           SV092
040100011204     C                   Z-ADD     SV092         TOTMEM            4 1
040200011204     C                   Z-ADD     TOTMEM        MEM(a)
040300020717     C                   eval(h)   con(a) = QTA(A)
040400011205     C                   Z-ADD     DESMED        MED(a)
040500011204     c                   enddo
040600011204     C                   ENDSR
040700011205     C******************************************************
040800011205     C     srini         BEGSR
040900011205     C******************************************************
041000011205     C                   Z-ADD     0             QTA
041100011205     C                   Z-ADD     0             CON
041200020710     C                   Z-ADD     0             KMM
041300011205     C                   Z-ADD     0             KM
041400011205     C                   Z-ADD     0             MED
041500011205     C                   Z-ADD     0             MEM
041600011205     C                   ENDSR
041700011204     C******************************************************
041800011205     C** STAMPA
041900011205     C******************************************************
042000011204     C     STAMPA        BEGSR
042100011219     c                   if        *in66
042200011219     c                   add       3             conrig
042300011219     c                   if        conrig < 50
042400011219     C                   EXCEPT    testa1
042500011219     c                   else
042600011219     c                   z-add     70            conrig
042700011219     c                   end
042800011219     c                   setoff                                       66
042900011219     c                   end
043000011204     C     CONRIG        IFGT      60
043100011204     C                   EXCEPT    TESTA
043200011204     C                   Z-ADD     6             CONRIG
043300011204     C                   END
043400141120     c                   exsr      scriviWF
043500141121     c                   xfoot     con           conr
043600141121     c                   xfoot     kmm           kmmr
043700141121     c                   add       con           cont
043800141121     c                   add       kmm           kmmt
043900141121     c                   if        kmmr > 0
044000141121     c     conr          div       kmmr          comodo
044100141121     c     comodo        mult      100           comodo
044200141121     c                   z-add     comodo        memr
044300141121     c                   else
044400141121     c                   clear                   memr
044500141121     c                   endif
044600160518     c                   clear                   $amemaud         20
044700160518     c                   movel     'MAU'         tbecod
044800160518     c                   movel(p)  $amemau       tbeke1
044900160518     c     ktbe          chain     tntbe01l
045000160518     c                   if        %found(tntbe01l)
045100160518     c                   movel     tbeuni        dmau
045200160518     c                   movel     §maudes       $amemaud
045300160518     c                   endif
045400160905     c                   clear                   $amecautd        14
045500160518     c                   movel     'CAU'         tbecod
045600160518     c                   movel(p)  $amecaut      tbeke1
045700160518     c     ktbe          chain     tntbe01l
045800160518     c                   if        %found(tntbe01l)
045900160518     c                   movel     tbeuni        dcau
046000160518     c                   movel     §caudes       $amecautd
046100160518     c                   endif
046200011219     C                   EXCEPT    DETAIL
046300011204     C                   ADD       6             CONRIG
046400011204     C                   ENDSR
046500141120     c*_____________________________________________________
046600141120     c     scriviWF      begsr
046700141120     c*_____________________________________________________
046800141120     c                   eval      WAMENRF  =  $AMENRF
046900141120     c                   eval      WAMETGA  =  $AMETGA
047000141120     c                   eval      WAMETGN  =  $AMETGN
047100141120     c                   eval      WAMETLA  =  $AMETLA
047200141120     c                   eval      WAMECAT  =  $AMECAT
047300141205     C                   MOVEL     'XA'          COD
047400141205     C                   MOVEL     *BLANKS       KEY
047500141205     C                   MOVEL     $AMECAT       KEY
047600141205     C     KTAB          CHAIN     TABEL                              10
047700141205     C                   MOVEL     *BLANKS       wamecatd
047800141205     C  N10              MOVEL     TBLUNI        wamecatd
047900141120     c                   eval      WAMEFIL  =  $AMEFIL
048000141205     C     $AMEFIL       CHAIN     AZORG                              10
048100141205     C                   MOVEL     *BLANKS       wamefild
048200141205     C  N10              MOVEL     ORGDES        wamefild
048300160518     c*                  eval      WAMEMAR  =  $AMEMAR
048400160518     c*                  eval      WAMETIP  =  $AMETIP
048500160518     c                   eval      WAMECAU  =  $AMECAU
048600160518     c                   eval      WAMEmau  =  $AMEmau
048700160518     c                   eval      WAMECAUt =  $AMECAUt
048800141120     c                   eval      WAMECAUD =  desaut
048900141120     c                   eval      WAMEDAL  =  $AMEDAL
049000141120     c                   eval      WAMEAL   =  ameals
049100141120     c                   eval      WAMEANNO =  filaa
049200141120     c                   eval      WAMCON1  =  con(1)
049300141120     c                   eval      WAMCON2  =  con(2)
049400141120     c                   eval      WAMCON3  =  con(3)
049500141120     c                   eval      WAMCON4  =  con(4)
049600141120     c                   eval      WAMCON5  =  con(5)
049700141120     c                   eval      WAMCON6  =  con(6)
049800141120     c                   eval      WAMCON7  =  con(7)
049900141120     c                   eval      WAMCON8  =  con(8)
050000141120     c                   eval      WAMCON9  =  con(9)
050100141120     c                   eval      WAMCON10 =  con(10)
050200141120     c                   eval      WAMCON11 =  con(11)
050300141120     c                   eval      WAMCON12 =  con(12)
050400141125     c                   eval      WAMCmm1  =  mem(1)
050500141125     c                   eval      WAMCmm2  =  mem(2)
050600141125     c                   eval      WAMCmm3  =  mem(3)
050700141125     c                   eval      WAMCmm4  =  mem(4)
050800141125     c                   eval      WAMCmm5  =  mem(5)
050900141125     c                   eval      WAMCmm6  =  mem(6)
051000141125     c                   eval      WAMCmm7  =  mem(7)
051100141125     c                   eval      WAMCmm8  =  mem(8)
051200141125     c                   eval      WAMCmm9  =  mem(9)
051300141125     c                   eval      WAMCmm10 =  mem(10)
051400141125     c                   eval      WAMCmm11 =  mem(11)
051500141125     c                   eval      WAMCmm12 =  mem(12)
051600141120     c                   eval      WAMC4R1  =  med(1)
051700141120     c                   eval      WAMC4R2  =  med(2)
051800141120     c                   eval      WAMC4R3  =  med(3)
051900141120     c                   eval      WAMC4R4  =  med(4)
052000141120     c                   eval      WAMC4R5  =  med(5)
052100141120     c                   eval      WAMC4R6  =  med(6)
052200141120     c                   eval      WAMC4R7  =  med(7)
052300141120     c                   eval      WAMC4R8  =  med(8)
052400141120     c                   eval      WAMC4R9  =  med(9)
052500141120     c                   eval      WAMC4R10 =  med(10)
052600141120     c                   eval      WAMC4R11 =  med(11)
052700141120     c                   eval      WAMC4R12 =  med(12)
052800141120     c                   eval      WAMKMP1  =  kmm(1)
052900141120     c                   eval      WAMKMP2  =  kmm(2)
053000141120     c                   eval      WAMKMP3  =  kmm(3)
053100141120     c                   eval      WAMKMP4  =  kmm(4)
053200141120     c                   eval      WAMKMP5  =  kmm(5)
053300141120     c                   eval      WAMKMP6  =  kmm(6)
053400141120     c                   eval      WAMKMP7  =  kmm(7)
053500141120     c                   eval      WAMKMP8  =  kmm(8)
053600141120     c                   eval      WAMKMP9  =  kmm(9)
053700141120     c                   eval      WAMKMP10 =  kmm(10)
053800141120     c                   eval      WAMKMP11 =  kmm(11)
053900141120     c                   eval      WAMKMP12 =  kmm(12)
054000141205     c                   eval      wameute  =  knmus
054100141205     c                   eval      wamedat  =  oradat
054200141125     c                   write     WMZAME0
054300141120     c                   endsr
054400141120     c*-------------------------------------------------
054500141121     c     comandi       begsr
054600141120     c*-------------------------------------------------
054700141120      *
054800141120     C                   z-add     99            luncmd
054900141120     C                   CALL      'QCMDEXC'                            34
055000141120     C                   parm                    coman
055100141120     C                   parm                    luncmd
055200141120      *
055300141120     c                   endsr
055400141120     c*_____________________________________________________
055500011205      * Ricezione e controllo parametri.
055600011205     C     *inzsr        BEGSR
055700141121     c*_____________________________________________________
055800011205     C     *ENTRY        PLIST
055900011205     C                   PARM                    KPJBA
056000011205     C                   Z-ADD     1             CODUT
056100011205     C                   CALL      'XPARUT'
056200011205     C                   PARM                    UTEDSE
056300011205     C                   MOVEL     RAGUT         RSUT             20
056400011205     c                   movel     kpjbu         param
056500141205     c                   time                    oradat           14 0
056600141121     C* data inizio
056700011205     c                   if        anno <> 0
056800011205     C                   MOVEl     anno          DATAX             6 0
056900011205     c                   else
057000011205     C                   movel     0001          DATAX
057100011205     c                   end
057200011205     c                   if        mese = 0
057300011205     c                   move      01            datax
057400011205     c                   else
057500011205     C                   MOVE      mese          DATAX
057600011205     c                   end
057700011205     C                   movel     datax         DATA1
057800011205     C                   MOVE      01            DATA1
057900011205     C* data fine
058000011205     c                   if        anno <> 0
058100011205     C                   MOVEl     anno          DATAX             6 0
058200011205     c                   else
058300011205     C                   movel     9999          DATAX
058400011205     c                   end
058500011205     c                   if        mese = 0
058600011205     c                   move      12            datax
058700011205     c                   else
058800011205     C                   MOVE      mese          DATAX
058900011205     c                   end
059000011205     C                   movel     datax         DATA2
059100011205     C                   MOVE      31            DATA2
059200141120      *elimina file in qtemp x la filiale
059300141120     c                   eval      file = 'AX' + knmus
059400141120      *
059500141121     C                   close     wmzame0f
059600141120     c                   movea     cmd2          coman
059700141120     c                   exsr      comandi
059800141121     C                   open      wmzame0f
059900011210     C* POSIZIONAMENTO SU MZAMH00F                         *
060000011210     C     KCAU          KLIST
060100011210     C                   KFLD                    $AMECAU
060200011210     C                   KFLD                    $AMEDAL
060300011210     C* POSIZIONAMENTO SU MZIMP000                         *
060400011210     C     KIMP          KLIST
060500011210     C                   KFLD                    $AMENRF
060600011210     C                   KFLD                    INTER             2
060700011210     C                   KFLD                    DATA              8 0
060800141121     C                   MOVEL     '£1'          INTER
060900011205     C     KIMP1         KLIST
061000011205     C                   KFLD                    $AMENRF
061100011205     C                   KFLD                    INTER             2
061200011205     C* POSIZIONAMENTO TABELLE                             *
061300011205     C     KTAB          KLIST
061400011205     C                   KFLD                    CODUT
061500011205     C                   KFLD                    COD               2
061600011205     C                   KFLD                    KEY               8
061700160518      *
061800160518     C     KTbe          KLIST                                                  *
061900160518     C                   KFLD                    tbecod                         *
062000160518     C                   KFLD                    tbeke1                         *
062100011205     c*
062200011205     C     VSCEF         IFNE      *BLANKS
062300011205     C                   SETON                                        55
062400011205     C                   END
062500011205     C     VSCEc         IFNE      *BLANKS
062600011205     C                   SETON                                        56
062700011205     C                   END
062800011205     C     VSCEa         IFNE      *BLANKS
062900011205     C                   SETON                                        57
063000011205     C                   END
063100011205     C     VSCEt         IFNE      *BLANKS
063200011205     C                   SETON                                        58
063300011205     C                   END
063400011205     c     anno          comp      0                                      59
063500011205     c     mese          comp      0                                      60
063600141125     c* categorie scelte
063700141125     c                   if        vcat <> *blank
063800141125     c                   movea     vcat          cat
063900141125     c                   endif
064000011210     c* preparo istruzione sql
064100011210     c                   exsr      srsql
064200011205     C                   ENDSR
064300011210     c**********************************************************************
064400011210     c     srsql         begsr
064500011210     c**********************************************************************
064600011210     C                   EVAL      WrkSqlCmd
064700011210     C                             =
064800011210     C                             'SELECT'
064900011210     C                             +
065000011210     C                             ' * FROM WRKAME0F '
065100011210     C                             +
065200011210     C                             ' WHERE AMENRF <> 0'
065300011210     c*scelta filiale
065400011210     C                   IF        vfil <> 0
065500011210     C                   EVAL      WrkSqlCmd
065600011210     C                             =
065700011210     C                             %TRIMR(WrkSqlCmd)
065800011210     C                             +
065900011210     C                             ' AND AMEFIL = ' + %editc(vfil:'Z')
066000011210     C                   endif
066100160810     c*scelto marca auto
066200160810     C                   IF        vcaut <> ' '
066300160810     C                   EVAL      WrkSqlCmd
066400160810     C                             =
066500160810     C                             %TRIMR(WrkSqlCmd)
066600160810     C                             +
066700160810     C                             ' AND AMECAUT = ' + '''' + VCAUT + ''''
066800160810     C                   endif
066900011210     c*scelta autista
067000011210     C                   IF        vaus <> ' '
067100011210     C                   EVAL      WrkSqlCmd
067200011210     C                             =
067300011210     C                             %TRIMR(WrkSqlCmd)
067400011210     C                             +
067500011210     C                             ' AND AMECAU = ' + '''' + VAUS + ''''
067600011210     C                   endif
067700011210     c*scelta targa
067800011210     C                   IF        vtga <> ' '
067900011210     C                   EVAL      WrkSqlCmd
068000011210     C                             =
068100011210     C                             %TRIMR(WrkSqlCmd)
068200011210     C                             +
068300011210     C                             ' AND AMETGA = ' + '''' + VTGA + ''''
068400011210     C                             +
068500011210     C                             ' AND AMETGN = ' + '''' + VTGN + ''''
068600011210     C                   endif
068700011210     c* ordinamento
068800011210     C                   EVAL      WrkSqlCmd
068900011210     C                             =
069000011210     C                             %TRIMR(WrkSqlCmd)
069100011210     C                             +
069200011210     C                             ' ORDER BY'
069300011210     c                   select
069400011210     c*x filiale
069500011210     C                   when      vscef <> ' '
069600011210     C                   EVAL      WrkSqlCmd
069700011210     C                             =
069800011210     C                             %TRIMR(WrkSqlCmd)
069900011210     C                             +
070000011210     C                             ' AMEFIL, AMECAT'
070100011210     c*x categoria
070200011210     C                   when      vscec <> ' '
070300011210     C                   EVAL      WrkSqlCmd
070400011210     C                             =
070500011210     C                             %TRIMR(WrkSqlCmd)
070600011210     C                             +
070700011210     C                             ' AMECAT'
070800011210     c*x autista
070900011210     C                   when      vscea <> ' '
071000011210     C                   EVAL      WrkSqlCmd
071100011210     C                             =
071200011210     C                             %TRIMR(WrkSqlCmd)
071300011210     C                             +
071400011210     C                             ' AMECAU, AMEDAL DESC, AMETGA, AMETGN'
071500011210     c*x targa
071600011210     C                   when      vscet <> ' '
071700011210     C                   EVAL      WrkSqlCmd
071800011210     C                             =
071900011210     C                             %TRIMR(WrkSqlCmd)
072000011210     C                             +
072100011210     C                             ' AMETGA, AMETGN, AMECAU, AMEDAL DESC'
072200011210     C                   endsl
072300011210     c*
072400011210     C                   ENDSR
072500011205     C*-----------------------------------------------------
072600011205     OPRTF198   E            TESTA            02
072700011205     O                       RSUT                20
072800011205     O                                           68 'CONSUMI MENSILI'
072900011211     O              n59                          73 'ANNO'
073000011211     O              n59      anno                78
073100011211     O              n60                          83 'MESE'
073200011211     O              n60      mese                86
073300011212     O                                          112 'TRMZ17RSQL'
073400011205     O                       *date              127 '  /  /    '
073500011205     O                       PAGE          Z    132
073600011205     O          E            TESTA             4
073700160518     O               55                          10 'Filiale '
073800011205     O               55      $AMEFIL             19
073900011205     O               55      DESFIL              40
074000011205     O               56                           9 'CATEGORIA'
074100011205     O               56      $AMECAT             12
074200011205     O               56      DESCAT              33
074300011205     O               57                           7 'AUTISTA'
074400011205     O               57      $AMECAu             10
074500011207     O               57      desa30              41
074600011205     O               58                           5 'TARGA'
074700011205     O               58      $AMEtga              8
074800011205     O               58      $AMEtgn             18
074900011205     O          E            TESTA             6
075000011206     O                                            7 'N.RIF.'
075100011205     O              n58                          13 'TARGA'
075200011207     O              n58                          28 'AUTISTA'
075300011207     O               58                          15 'AUTISTA'
075400011207     O                                           47 'MARCA'
075500011207     O                                           60 'TIPO'
075600011207     O                                           81 'ANNO'
075700011205     O                       ST(1)               96
075800011205     O                       ST(2)              102
075900011205     O                       ST(3)              108
076000011205     O                       ST(4)              114
076100011205     O                       ST(5)              120
076200011205     O                       ST(6)              126
076300011205     O                       ST(7)              132
076400011205     O                       ST(8)              138
076500011205     O                       ST(9)              144
076600011205     O                       ST(10)             150
076700011205     O                       ST(11)             156
076800011205     O                       ST(12)             162
076900141121     o                                          172 'Totale'
077000011219     O          E            TESTA1      2
077100160518     O               55                          10 'Filiale '
077200011219     O               55      $AMEFIL             19
077300011219     O               55      DESFIL              40
077400011219     O               56                           9 'CATEGORIA'
077500011219     O               56      $AMECAT             12
077600011219     O               56      DESCAT              33
077700011219     O               57                           7 'AUTISTA'
077800011219     O               57      $AMECAu             10
077900011219     O               57      desa30              41
078000011219     O               58                           5 'TARGA'
078100011219     O               58      $AMEtga              8
078200011219     O               58      $AMEtgn             18
078300011205     C*-----------------------------------------------------
078400011205     C* STAMPA DETTAGLIO
078500011205     C*-----------------------------------------------------
078600011207     o* per tutti
078700011207     O          E            DETAIL      1
078800011207     O              n61      $AMEnrf       z      7
078900160518     O*             n61      $AMEMAR             55
079000160518     O*             n61      $AMETIP             76
079100160518     O              n61      $AMEcautd           55
079200160518     O              n61      $AMEmaud            76
079300011207     O                       FILAA         4     81
079400011207     O                                           91 'CON.CARB.'
079500011207     O                       CON(1)        Z     96
079600011207     O                       CON(2)        Z    102
079700011207     O                       CON(3)        Z    108
079800011207     O                       CON(4)        Z    114
079900011207     O                       CON(5)        Z    120
080000011207     O                       CON(6)        Z    126
080100011207     O                       CON(7)        Z    132
080200011207     O                       CON(8)        Z    138
080300011207     O                       CON(9)        Z    144
080400011207     O                       CON(10)       Z    150
080500011207     O                       CON(11)       Z    156
080600011207     O                       CON(12)       Z    162
080700141126     O                       CONr          2    172
080800011207     o* filiale
080900011207     O          E   n61 55   DETAIL      0
081000011207     O                       $AMETGA             10
081100011207     O                       $AMETGN             20
081200011207     O                       DESAUT              41
081300011207     o* categoria
081400011207     O          E   n61 56   DETAIL      0
081500011207     O                       $AMETGA             10
081600011207     O                       $AMETGN             20
081700011207     O                       DESAUT              41
081800011207     o* autista
081900011207     O          E   n61 57   DETAIL      0
082000011207     O                       $AMETGA             10
082100011207     O                       $AMETGN             20
082200011207     o                                           24 'DAL'
082300011207     o                       dalames             35
082400011207     o* targa
082500011207     O          E   n61 58   DETAIL      0
082600011207     O                       DESA30              38
082700011207     o* per tutti
082800011207     O          E            DETAIL      1
082900011207     O                                           91 'MEDIA MEN.'
083000011207     O                       MEM(1)        2     96
083100011207     O                       MEM(2)        2    102
083200011207     O                       MEM(3)        2    108
083300011207     O                       MEM(4)        2    114
083400011207     O                       MEM(5)        2    120
083500011207     O                       MEM(6)        2    126
083600011207     O                       MEM(7)        2    132
083700011207     O                       MEM(8)        2    138
083800011207     O                       MEM(9)        2    144
083900011207     O                       MEM(10)       2    150
084000011207     O                       MEM(11)       2    156
084100011207     O                       MEM(12)       2    162
084200011207     c* per filiale e categoria
084300011207     O          E   n61n57n58DETAIL      0
084400011207     o                                           24 'DAL'
084500011207     o                       dalames             35
084600011207     c* per autista
084700011207     O          E   n61 57   DETAIL      0
084800011207     o                                           23 'AL'
084900011207     o                       dtaeur              35
085000011207     c* per targa
085100011207     O          E   n61 58   DETAIL      0
085200011207     o                                           11 'DAL'
085300011207     o                       dalames             22
085400011207     o                                           25 'AL'
085500011207     o                       DTAEUR              36
085600141121     O          E            DETAIL      1  1
085700011207     O                                           91 'MEDIA 4 R.'
085800011205     O                       MED(1)        2     96
085900011205     O                       MED(2)        2    102
086000011205     O                       MED(3)        2    108
086100011205     O                       MED(4)        2    114
086200011205     O                       MED(5)        2    120
086300011205     O                       MED(6)        2    126
086400011205     O                       MED(7)        2    132
086500011205     O                       MED(8)        2    138
086600011205     O                       MED(9)        2    144
086700011205     O                       MED(10)       2    150
086800011205     O                       MED(11)       2    156
086900011205     O                       MED(12)       2    162
087000011205     O          E            DETAIL      1  1
087100011207     O                                           91 'KM PERCOSI'
087200020710     O                       KMM(1)        Z     96
087300020710     O                       KMM(2)        Z    102
087400020710     O                       KMM(3)        Z    108
087500020710     O                       KMM(4)        Z    114
087600020710     O                       KMM(5)        Z    120
087700020710     O                       KMM(6)        Z    126
087800020710     O                       KMM(7)        Z    132
087900020710     O                       KMM(8)        Z    138
088000020710     O                       KMM(9)        Z    144
088100020710     O                       KMM(10)       Z    150
088200020710     O                       KMM(11)       Z    156
088300020710     O                       KMM(12)       Z    162
088400141126     O                       KMMr          2    172
088500141121      *totale aziendale
088600141121     O          E            totale           02
088700141121     O                       RSUT                20
088800141121     O                                           68 'CONSUMI MENSILI'
088900141121     O                                           78 'TOTALI'
089000141121     O                                           88 'AZIENDA'
089100141121     O                                          112 'TRMZ17RSQL'
089200141121     O                       *date              127 '  /  /    '
089300141121     O                       PAGE          Z    132
089400141121     O          E            Totale            6
089500141121     O                                           15 'TOTALI GENERALI'
089600141121     O                                           30 'Gennaio'
089700141121     O                                           45 'Febbraio'
089800141121     O                                           60 'Marzo'
089900141121     O                                           75 'Aprile'
090000141121     O                                           90 'Maggio'
090100141121     O                                          105 'Giugno'
090200141121     O                                          120 'Luglio'
090300141121     O                                          135 'Agosto'
090400141121     O                                          150 'Settembre'
090500141121     O                                          165 'Ottobre'
090600141121     O                                          180 'Novembre'
090700141121     O                                          195 'Dicembre'
090800141121     O          E            totale      2
090900141121     O                                           16 'Litri Carburante'
091000141121     O                       CONt(1)       2     30
091100141121     O                       CONt(2)       2     45
091200141121     O                       CONt(3)       2     60
091300141121     O                       CONt(4)       2     75
091400141121     O                       CONt(5)       2     90
091500141121     O                       CONt(6)       2    105
091600141121     O                       CONt(7)       2    120
091700141121     O                       CONt(8)       2    135
091800141121     O                       CONt(9)       2    150
091900141121     O                       CONt(10)      2    165
092000141121     O                       CONt(11)      2    180
092100141121     O                       CONt(12)      2    195
092200141121     O          E            totale      2
092300141121     O                                           15 'Km. Percorsi   '
092400141121     O                       kmmt(1)       2     30
092500141121     O                       kmmt(2)       2     45
092600141121     O                       kmmt(3)       2     60
092700141121     O                       kmmt(4)       2     75
092800141121     O                       kmmt(5)       2     90
092900141121     O                       kmmt(6)       2    105
093000141121     O                       kmmt(7)       2    120
093100141121     O                       kmmt(8)       2    135
093200141121     O                       kmmt(9)       2    150
093300141121     O                       kmmt(10)      2    165
093400141121     O                       kmmt(11)      2    180
093500141121     O                       kmmt(12)      2    195
093600141121     O          E            totale      2
093700141121     O                                           15 'Media l./100 km'
093800141121     O                       memt(1)       2     30
093900141121     O                       memt(2)       2     45
094000141121     O                       memt(3)       2     60
094100141121     O                       memt(4)       2     75
094200141121     O                       memt(5)       2     90
094300141121     O                       memt(6)       2    105
094400141121     O                       memt(7)       2    120
094500141121     O                       memt(8)       2    135
094600141121     O                       memt(9)       2    150
094700141121     O                       memt(10)      2    165
094800141121     O                       memt(11)      2    180
094900141121     O                       memt(12)      2    195
095000141205     O          E            totale      3
095100141205     O                                           25 'Generale Aziendale'
095200141205     O                                           44 'Litri Carburante'
095300141205     O                                           62 'Km Totali'
095400141205     O                                           77 'Media'
095500141121     O          E            totale      2
095600141205     O                       contt         2     44
095700141205     O                                        +   2 ':'
095800141205     O                       kmmtt         2     62
095900141121     O                                        +   1 'x 100  = '
096000141121     O                       memtt         2  +   1
096100141121**
096200011205GEN
096300011205FEB
096400011205MAR
096500011205APR
096600011205MAG
096700011205GIU
096800011205LUG
096900011205AGO
097000011205SET
097100011205OTT
097200011205NOV
097300011205DIC
097400141120**  CMD1
097500141120CLRPFM    FILE(WMZAME0F)
097600141120**  CMD2
097700141120CRTDUPOBJ OBJ(WMZAME0F) FROMLIB(*LIBL) OBJTYPE(*FILE) TOLIB(QTEMP)
097800141120**  CMD3
097900141120DLTF FILE(QTEMP/WMZAME0F)
098000141120**  CMD4
098100141120CHKOBJ  OBJ(WRKQUERY/xxxxxxxxxx) OBJTYPE(*FILE)
098200141120**  CMD5
098300141120CRTDUPOBJ OBJ(WMZAME0F) FROMLIB(*LIBL) OBJTYPE(*FILE) TOLIB(WRKQUERY) NEWOBJ(xxxxxxxxxx) DATA(*YES)
098400141120**  CMD6
098500141120CPYF FROMFILE(WMZAME0F) TOFILE(WRKQUERY/xxxxxxxxxx) MBROPT(*REPLACE)
