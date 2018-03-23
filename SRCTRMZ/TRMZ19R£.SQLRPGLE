000100011214      *PARMS DFTACTGRP(*YES)
000200011214      *PARMS COMMIT(*NONE) DATFMT(*ISO)
000300011214      *PARMS CLOSQLCSR(*ENDMOD)
000400011214      *PARMS DYNUSRPRF(*OWNER)
000500000000     H DECEDIT('0,') DATEDIT(*DMY.)
000600000000     F*          ANOMALIE DI PICCOLA MNUTENZIONE           *
000700000000     F*----------------------------------------------------*
000800011214     FMZIMP01L  IF   E           K DISK
000900011214     FMZtim02L  IF   E           K DISK
001000000000     FTABEL00F  IF   E           K DISK
001100160517     FTntbe01l  IF   E           K DISK
001200910208     FAZORG01L  IF   E           K DISK
001300000000     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOA)
001400000000     D*--------------------------------------------------------------*
001500910213     D S               S             66    DIM(12) CTDATA PERRCD(1)             STAMPA
001600011214     D KEY             S              6    DIM(5)                               CAMPI PER KEYFLD
001700011214     D PARAM           DS
001800011214     D  SCAT                          1
001900011214     D  SDIT                          1
002000011214     D  SFIL                          1
002100011214     D  keyfld                       30
002200011214     D  ANNOV                         4  0
002300011214     D  MESEV                         2  0
002400011214     D  VTGA                          2
002500011214     D  VTGN                          9
002600011214     D  VIMM                          4  0
002700011214     D  VCAT                          2
002800160517     D  Vcaut                         2
002900160517     D  Vmau                          3
003000011214     D  VDIT                          3
003100011214     D  VFIL                          3  0
003200011214     D  VCDC                          8
003300160517     D  dmau         E DS
003400160517     D  dcau         E DS
003500020705     D ut§dse        E DS                  extname(ut§dse0f)
003600000000     D KPJBA         E DS
003700011214     d dtaiso          s               d
003800011214     d dtaeur          s               d   datfmt(*eur)
003900011214     d dtaiso1         s               d
004000011214     d dtaiso2         s               d
004100011214     D WrkSqlCmd       S           1024
004200011214     D mzame         E DS                  extname(mzame00f)
004300011214     C*----------------------------------------------------*
004400011214     C* MAIN LINE
004500011214     C*
004600011214     C*  PER FILIALE -- PER OGNI AUTOMEZZO
004700011214     C*      PER OGNI INTERVENTO
004800020730     C*         LEGGO MZIMP01L
004900011214     C*           PER OGNI COPPIA DI INTERVENTI CALCOLO LO SCOSTAMENTO
005000011214     C*           RISPETTO ALLO STANDARD
005100011214     C*           SE SUPERA LA TOLLERANZA STAMPO ANOMALIA
005200011214     C*-----------------------------------------------------*
005300011214     c                   exsr      srsql
005400011214      *
005500011214     c* Lettura WRKAME00F
005600011214     C/EXEC SQL
005700011214     C+ PREPARE S1 FROM :WrkSqlCmd
005800011214     C/END-EXEC
005900011214
006000011214     C/EXEC SQL
006100011214     C+ DECLARE A1 CURSOR FOR S1
006200011214     C/END-EXEC
006300011214
006400011214     C/EXEC SQL
006500011214     C+ OPEN A1
006600011214     C/END-EXEC
006700011214
006800011214     C                   DOU       SqlCod <> 0
006900011214     C/EXEC SQL
007000011214     C+ FETCH NEXT FROM A1 INTO :mzame
007100011214     C/END-EXEC
007200011214     C                   SELECT
007300011214     C                   WHEN      SqlCod = 0
007400011214     C                   EXSR      SRROT
007500011214     C                   EXSR      SRcalcol
007600011214     C*
007700011214     C                   WHEN      SqlCod = 100
007800011214     C                   WHEN      SqlCod <> 0
007900011214      * Forzo la stampa del JOBLOG.
008000011214     C                   CALL      'X66CHGJOB'
008100011214     C                   ENDSL
008200011214     C                   ENDDO
008300011214
008400011214     C/EXEC SQL
008500011214     C+ CLOSE A1
008600011214     C/END-EXEC
008700011214     c*
008800011214     C                   seton                                        lr
008900011214     c**********************************************************************
009000011214     c     srcalcol      begsr
009100011214     c**********************************************************************
009200011214     C* LEGGO LA TABELLA INTERVENTI
009300011214     C     AMEUTI        IFNE      *BLANK
009400011214     C                   MOVE      AMEUTI        TIMMAN
009500011214     C     KTIM          SETLL     MZTIM000
009600011214     C                   DO        *HIVAL
009700011214     C     KTIM          READE     MZTIM000                               27
009800011214     C   27              LEAVE
009900020730     C* ESCLUDE GLI INTERVENTI DI PIENO CARBURANTE
010000020730     C                   IF        TIMINT = '£1' OR TIMINT = 'CB'
010100020730     C                   ITER
010200020730     C                   END
010300020730     C*
010400011214     C                   SETOFF                                           50
010500011214     C                   Z-ADD     0             SV1DAT            8 0
010600011214     C                   Z-ADD     0             SV1KM
010700011214     C                   Z-ADD     AMENRF        IMPNRF
010800011214     C                   MOVE      TIMINT        IMPINT
010900011214     C                   Z-ADD     DATA1         IMPDAT
011000011214      * RICERCO L'ULTIMO INTERVENTO PRIMA DELLA DATA INIZIO
011100020730      * SE LO TROVO ACCENDO 50
011200011214     C     KIMP          SETLL     MZIMP000
011300011214     C                   READP     MZIMP000                               21
011400011214     C  N21IMPNRF        COMP      AMENRF                             2121
011500011214     C  N21IMPINT        COMP      TIMINT                             2121
011600011214     C                   IF        NOT *IN21
011700011214     C                   Z-ADD     IMPDAT        SV1DAT
011800011214     C                   Z-ADD     IMPKM         SV1KM             7 0
011900011214     C                   SETON                                            50
012000011214     C                   END
012100011214     C                   Z-ADD     AMENRF        IMPNRF
012200011214     C                   MOVE      TIMINT        IMPINT
012300011214     C                   Z-ADD     DATA1         IMPDAT
012400011214     C     KIMP          SETLL     MZIMP000
012500011214     C     KIMP1         READE     MZIMP000                               26
012600011214     C* se non trovo l'ultimo intervento prima della data inizio e non
012700011214     c* trovo nessun intervento dalla data di inizio emetto la segnalazione
012800011214     c* di dato mai pervenuto
012900011214     C                   IF        *IN21 AND *IN26
013000011214     C                   SETON                                        66
013100011214     C                   MOVE      'XM'          TBLCOD
013200011214     C                   MOVE      *BLANK        TBLKEY
013300011214     C                   MOVEL     TIMINT        TBLKEY
013400011214     C     KTAB          CHAIN     TABEL                              47
013500011214     C                   MOVE      *BLANK        INTDES
013600011214     C  N47              MOVEL     TBLUNI        INTDES           19
013700011214     C     TBLFLG        IFEQ      *BLANKS
013800011214     C   OA              EXCEPT    TESTA
013900011214     c                   setoff                                       oa
014000160517     c                   clear                   amemaud          30
014100160517     c                   movel     'MAU'         tbecod
014200160517     c                   movel(p)  amemau        tbeke1
014300160517     c     ktbe          chain     tntbe01l
014400160517     c                   if        %found(tntbe01l)
014500160517     c                   movel     tbeuni        dmau
014600160517     c                   movel     §maudes       amemaud
014700160517     c                   endif
014800160517     c                   clear                   amecautd         30
014900160517     c                   movel     'CAU'         tbecod
015000160517     c                   movel(p)  amecaut       tbeke1
015100160517     c     ktbe          chain     tntbe01l
015200160517     c                   if        %found(tntbe01l)
015300160517     c                   movel     tbeuni        dcau
015400160517     c                   movel     §caudes       amecautd
015500160517     c                   endif
015600011214     C                   EXCEPT    RIGA
015700011214     C                   END
015800011214     C                   SETOFF                                       66
015900011214     C                   END
016000011214     C* se ho trovo l'ultimo intervento PRIMA della data inizio ma non ho
016100011214     c* trovo nessun intervento DALLA data di inizio emetto la segnalazione
016200011214     c* che non è stato effettuato nel periodo scelto
016300011214     C                   IF        NOT *IN21 AND *IN26
016400020730     C                   EXSR      SRCARB
016500020730     C                   END
016600011214      * LEGGO I SUCCESSIVI INTERVENTI
016700011214     C     *IN26         DOWNE     '1'
016800011214     C     IMPDAT        ANDLE     DATA2
016900011214     C                   SETOFF                                           80
017000011214     c* salvo il primo intervento che leggo se non ho trovato
017100011214     C* l'ultimo intervento PRIMA della data inizio
017200011214     C                   if        not *in50
017300011214     C                   Z-ADD     IMPDAT        SV1DAT            8 0
017400011214     C                   Z-ADD     IMPKM         SV1KM             7 0
017500011214     C                   SETON                                          7950
017600011214     C                   ELSE
017700011214     C                   SETON                                        80
017800011214     C                   Z-ADD     IMPDAT        SV2DAT            8 0
017900011214     C                   Z-ADD     IMPKM         SV2KM             7 0
018000011214     C                   EXSR      CTRL
018100011214     C                   Z-ADD     SV2DAT        SV1DAT
018200011214     C                   Z-ADD     SV2KM         SV1KM
018300011214     C                   END
018400011214     C     KIMP1         READE     MZIMP000                               26
018500011214     C  N26              END
018600011214     C* SE C'E' UN SOLO INTERVENTO DI QUEL TIPO CONTROLLA SE NE TROVA ALTRI
018700011214     C* E SE CI SONO, NON INDICA ALCUNA ANOMALIA
018800011214     C                   IF        *IN79 AND NOT *IN80
018900020730     C                   EXSR      SRCARB
019000020730     C                   SETOFF                                       79
019100011214     C                   END
019200011214     C                   ENDDO
019300011214     C                   END
019400011214     C                   EXCEPT    SALTO
019500011214     c                   endsr
019600011214     C*---------------------------------------------------
019700020730     C     SRCARB        BEGSR
019800011214     C*---------------------------------------------------
019900020730     C                   Z-ADD     data2         sv2DAT            8 0
020000020730     C* SE UNITÀ DI MISURA  È KM CONTROLLO IL CARBURANTE
020100020730     C     TIMUNI        IFEQ      'KM'
020200020730     C                   Z-ADD     AMENRF        IMPNRF
020300020730     C                   Z-ADD     data2         IMPDAT
020400020801     c* provo con carburante manuale
020500020730     C                   MOVE      '£1'          IMPINT
020600020730     C     KIMP          SETGT     MZIMP000
020700020730     C     KIMP1         READPE    MZIMP000
020800020730     C                   IF        %FOUND(MZIMP01L) AND
020900020730     C                             IMPDAT <= DATA2
021000020730     C                             AND IMPDAT >= sv1dat
021100020730     C                   Z-ADD     IMPKM         SV2KM
021200020730     C                   Z-ADD     IMPDAT        sv2DAT
021300020730     C                   SETON                                            81
021400020731     C                   End
021500020801     c* provo con carburante agip
021600020730     C                   MOVE      'CB'          IMPINT
021700020730     C     KIMP          SETGT     MZIMP000
021800020730     C     KIMP1         READPE    MZIMP000
021900020730     C                   IF        %FOUND(MZIMP01L) AND
022000020730     C                             IMPDAT <= DATA2
022100020730     C                             AND IMPDAT >= sv1dat
022200020731     C                             AND IMPkm > sv2km
022300020730     C                   Z-ADD     IMPKM         SV2KM
022400020730     C                   Z-ADD     IMPDAT        sv2DAT
022500020730     C                   SETON                                            81
022600020730     C                   END
022700020730     C                   ELSE
022800020730     C                   Z-ADD     0             SV2KM
022900020730     C                   SETON                                            81
023000020730     C                   END
023100020730     C* SE TROVATO FACCIO IL CONFRONTO
023200020730     C                   IF        *IN81
023300020730     C                   EXSR      CTRL
023400020730     C                   SETOFF                                           81
023500020730     C                   END
023600020730     c                   endsr
023700020730     C*---------------------------------------------------
023800020730     C     SRROT         BEGSR
023900020730     C*---------------------------------------------------
024000011214      * ROTTURA DI FILIALE
024100011214     C     AMEFIL        IFNE      SAVFIL
024200011214      * DECODIFICA FILIALE
024300011214     C     AMEFIL        CHAIN     AZORG                              21
024400011214     C                   MOVEL     *BLANKS       DESFIL           20
024500011214     C  N21              MOVEL     ORGDES        DESFIL
024600011214     C                   SETON                                            OA
024700011214     C                   Z-ADD     AMEFIL        SAVFIL            3 0
024800011214     C                   END
024900011214     C                   ENDSR
025000000000     C*---------------------------------------------------
025100000000     C*   CTRL
025200000000     C*        CONTROLLA SCOSTAMENTI E STAMPA
025300000000     C*---------------------------------------------------
025400000000     C     CTRL          BEGSR
025500011214     c                   move      sv1dat        dtaiso1
025600011214     c                   move      sv2dat        dtaiso2
025700011214      * CALCOLO DIFFERENZE IN GG MM O KM
025800000000     C     TIMUNI        IFEQ      'GG'
025900011214     C     dtaiso2       subdur    dtaiso1       difgg:*d          7 0
026000011214     c                   z-add     difgg         diffe
026100000000     C                   END
026200000000      *
026300000000     C     TIMUNI        IFEQ      'KM'
026400000000     C     SV2KM         SUB       SV1KM         DIFFE             7 0
026500000000     C                   END
026600000000      *
026700000000     C     TIMUNI        IFEQ      'MM'
026800011214     C     dtaiso2       subdur    dtaiso1       difmm:*m          7 0
026900011214     c                   z-add     difmm         diffe
027000000000     C                   END
027100000000      *
027200000000     C     DIFFE         IFLT      0
027300910211     C     TIMUNI        IFEQ      'KM'
027400910211     C                   Z-ADD     0             DIFFE
027500910211     C                   ELSE
027600910211     C                   Z-SUB     DIFFE         DIFFE
027700910211     C                   END
027800000000     C                   END
027900000000      *
028000910211     C                   Z-ADD     0             SCOST
028100000000     C     DIFFE         IFGT      TIMPER
028200000000     C*
028300000000     C     DIFFE         SUB       TIMPER        SCOST             9 2
028400000000     C                   DIV       TIMPER        SCOST
028500000000     C                   MULT      100           SCOST
028600000000      *
028700000000     C     SCOST         IFGT      TIMTOL
028800000000     C* DECODIFICA INTERVENTO
028900000000     C                   MOVE      'XM'          TBLCOD
029000000000     C                   MOVE      *BLANK        TBLKEY
029100000000     C                   MOVEL     TIMINT        TBLKEY
029200910207     C     KTAB          CHAIN     TABEL                              47
029300000000     C                   MOVE      *BLANK        INTDES
029400910207     C  N47              MOVEL     TBLUNI        INTDES           19
029500000000      *
029600011214     C                   MOVE      DTAISO1       DTAEUR
029700011214     C                   MOVE      DTAEUR        D1GMA             8 0
029800000000      *
029900011214     C                   MOVE      DTAISO2       DTAEUR
030000011214     C                   MOVE      DTAEUR        D2GMA             8 0
030100000000      *
030200910208     C     TBLFLG        IFEQ      *BLANKS
030300000000     C   OA              EXCEPT    TESTA
030400011214     c                   setoff                                       oa
030500160517     c                   clear                   amemaud          30
030600160517     c                   movel     'MAU'         tbecod
030700160517     c                   movel(p)  amemau        tbeke1
030800160517     c     ktbe          chain     tntbe01l
030900160517     c                   if        %found(tntbe01l)
031000160517     c                   movel     tbeuni        dmau
031100160517     c                   movel     §maudes       amemaud
031200160517     c                   endif
031300160517     c                   clear                   amecautd         30
031400160517     c                   movel     'CAU'         tbecod
031500160517     c                   movel(p)  amecaut       tbeke1
031600160517     c     ktbe          chain     tntbe01l
031700160517     c                   if        %found(tntbe01l)
031800160517     c                   movel     tbeuni        dcau
031900160517     c                   movel     §caudes       amecautd
032000160517     c                   endif
032100000000     C                   EXCEPT    RIGA
032200910208     C                   END
032300000000     C                   END
032400000000     C                   END
032500000000     C                   ENDSR
032600011214     c**********************************************************************
032700011214     c     srsql         begsr
032800011214     c**********************************************************************
032900011214     C                   EVAL      WrkSqlCmd
033000011214     C                             =
033100011214     C                             'SELECT'
033200011214     C                             +
033300011214     C                             ' * FROM MZAME00F '
033400011214     C                             +
033500011214     C                             ' WHERE AMENRF <> 0'
033600011214     c*scelta filiale
033700011214     C                   IF        vfil <> 0
033800011214     C                   EVAL      WrkSqlCmd
033900011214     C                             =
034000011214     C                             %TRIMR(WrkSqlCmd)
034100011214     C                             +
034200011214     C                             ' AND AMEFIL = ' + %editc(vfil:'Z')
034300011214     C                   endif
034400011214     c*scelta ditta
034500011214     C                   IF        vdit <> *blanks
034600011214     C                   EVAL      WrkSqlCmd
034700011214     C                             =
034800011214     C                             %TRIMR(WrkSqlCmd)
034900011214     C                             +
035000011214     C                             ' AND AMEDIT = ' + '''' + VDIT + ''''
035100011214     C                   endif
035200011214     c*scelta cdc
035300011214     C                   IF        vcdc <> *blanks
035400011214     C                   EVAL      WrkSqlCmd
035500011214     C                             =
035600011214     C                             %TRIMR(WrkSqlCmd)
035700011214     C                             +
035800011214     C                             ' AND AMECDC = ' + '''' + Vcdc + ''''
035900011214     C                   endif
036000011214     c*scelta categoria
036100011214     C                   IF        vcat <> ' '
036200050526     c                   if        %subst(vcat: 2: 1) = '*'
036300050526     c                   movel     vcat          comcat            1
036400050526     C                   EVAL      WrkSqlCmd
036500050526     C                             =
036600050526     C                             %TRIMR(WrkSqlCmd)
036700050526     C                             +
036800050526     C                             ' AND substr(AMECAT, 1, 1) = '
036900050526     C                             + '''' + COMCAT + ''''
037000050526     c                   else
037100011214     C                   EVAL      WrkSqlCmd
037200011214     C                             =
037300011214     C                             %TRIMR(WrkSqlCmd)
037400011214     C                             +
037500011214     C                             ' AND AMECAT = ' + '''' + VCAT + ''''
037600011214     C                   endif
037700050526     C                   endif
037800011214     c*scelta immatricolazione
037900011214     C                   IF        vimm <> 0
038000011214     C                   EVAL      WrkSqlCmd
038100011214     C                             =
038200011214     C                             %TRIMR(WrkSqlCmd)
038300011214     C                             +
038400011214     C                             ' AND AMEIMM = ' + %editc(vimm:'Z')
038500011214     C                   endif
038600011214     c*scelta targa
038700011214     C                   IF        vtga <> ' '
038800011214     C                   EVAL      WrkSqlCmd
038900011214     C                             =
039000011214     C                             %TRIMR(WrkSqlCmd)
039100011214     C                             +
039200011214     C                             ' AND AMETGA = ' + '''' + VTGA + ''''
039300011214     C                             +
039400011214     C                             ' AND AMETGN = ' + '''' + VTGN + ''''
039500011214     C                   endif
039600020206     c*scelta marca
039700160517     C                   IF        vcaut <> ' '
039800160517     C                   EVAL      WrkSqlCmd
039900160517     C                             =
040000160517     C                             %TRIMR(WrkSqlCmd)
040100160517     C                             +
040200160517     C                             ' AND AMECAUT = ' + '''' + VCAUT + ''''
040300160517     C                   endif
040400020206     c*scelta tipo
040500160517     C                   IF        vmau <> ' '
040600160517     C                   EVAL      WrkSqlCmd
040700160517     C                             =
040800160517     C                             %TRIMR(WrkSqlCmd)
040900160517     C                             +
041000160517     C                             ' AND AMEMAU = ' + '''' + VMAU + ''''
041100160517     C                   endif
041200020206     c*
041300011214     C                   EVAL      WrkSqlCmd
041400011214     C                             =
041500011214     C                             %TRIMR(WrkSqlCmd)
041600011214     C                             +
041700011214     C                             ' ORDER BY '
041800011214     c* ORDINAMENTO
041900011214     C                   EVAL      WrkSqlCmd
042000011214     C                             =
042100011214     C                             %TRIMR(WrkSqlCmd)
042200011214     C                             + ' ' + key(1)
042300011214     c     2             do        5             aa                2 0
042400011214     c                   if        key(AA) <> 'AMENRF'
042500011214     C                   EVAL      WrkSqlCmd
042600011214     C                             =
042700011214     C                             %TRIMR(WrkSqlCmd)
042800011214     c                             +
042900011214     C                             ',' + key(aa)
043000011214     c                   end
043100011214     c                   enddo
043200011214     c                   if        key(1) <> 'AMENRF'
043300011214     C                   EVAL      WrkSqlCmd
043400011214     C                             =
043500011214     C                             %TRIMR(WrkSqlCmd)
043600011214     C                             +
043700011214     C                             ', AMENRF'
043800011214     c                   end
043900011214     c*
044000011214     C                   ENDSR
044100011214     C**********************************************************************
044200011214     C     *INZSR        BEGSR
044300011214     C**********************************************************************
044400011214     C     *ENTRY        PLIST
044500011214     C                   PARM                    KPJBA
044600011214     c                   movel     kpjbu         param
044700011214     C                   MOVEA     KEYFLD        KEY
044800011214     C                   Z-ADD     1             CODUT
044900020705     C                   CALL      'X§PARUT'
045000020705     C                   PARM                    UT§DSE
045100011214     C                   MOVEL     RAGUT         RSUT             20
045200011214     C                   MOVEL     RAGUT         RSUT2            25
045300011214     C* LA DATA DEL GIORNO LA PRENDO DA TIME
045400011214     C                   TIME                    W0140            14 0
045500011214     C                   MOVE      W0140         UDATE8            8 0
045600011214     C*----------------------------------------------------*
045700011214     C* POSIZIONAMENTO SU MZIMP000                         *
045800011214     C     KIMP          KLIST                                                  *
045900011214     C                   KFLD                    IMPNRF                         *
046000011214     C                   KFLD                    IMPINT                         *
046100011214     C                   KFLD                    IMPDAT                         *
046200011214     C     KIMP1         KLIST                                                  *
046300011214     C                   KFLD                    IMPNRF                         *
046400011214     C                   KFLD                    IMPINT                         *
046500011214     C* POSIZIONAMENTO SU MZTIM000                         *
046600011214     C     KTIM          KLIST                                                  *
046700011214     C                   KFLD                    TIMTIP                         *
046800011214     C                   KFLD                    TIMMAN                         *
046900011214     C                   MOVE      'P'           TIMTIP
047000011214     C* POSIZIONAMENTO TABELLE                             *
047100011214     C     KTAB          KLIST                                                  *
047200011214     C                   KFLD                    CODUT                          *
047300011214     C                   KFLD                    TBLCOD                         *
047400011214     C                   KFLD                    TBLKEY                         *
047500160517      *
047600160517     C     KTbe          KLIST                                                  *
047700160517     C                   KFLD                    tbecod                         *
047800160517     C                   KFLD                    tbeke1                         *
047900011214     C* data inizio
048000011214     c                   if        annoV <> 0
048100011214     C                   MOVEl     annoV         DATAX             6 0
048200011214     c                   else
048300011214     C                   movel     0001          DATAX
048400011214     c                   end
048500011214     c                   if        meseV = 0
048600011214     c                   move      01            datax
048700011214     c                   else
048800011214     C                   MOVE      meseV         DATAX
048900011214     c                   end
049000011214     C                   movel     datax         DATA1             8 0
049100011214     C                   MOVE      01            DATA1
049200011214     C                   MOVE      data1         Dtaiso
049300011214     C                   MOVE      dtaiso        Dtaeur
049400011214     C                   MOVE      dtaeur        Dat1              8 0
049500011214     C* data fine
049600011214     c                   if        annoV <> 0
049700011214     C                   MOVEl     annoV         DATAX             6 0
049800011214     c                   else
049900011214     C                   movel     9999          DATAX
050000011214     c                   end
050100011214     c                   if        meseV = 0
050200011214     c                   move      12            datax
050300011214     c                   else
050400011214     C                   MOVE      meseV         DATAX
050500011214     c                   end
050600011214     C                   movel     datax         DATA2             8 0
050700011214     C                   MOVE      01            DATA2
050800011214     c                   if        data2 = 99991201
050900011214     c                   move      31            data2
051000011214     c                   move      data2         DTAISO
051100011214     c                   else
051200011214     c                   move      data2         DTAISO
051300011214     c                   adddur    1:*m          dtaiso
051400011214     c                   subdur    1:*d          dtaiso
051500011214     c                   move      dtaiso        data2
051600011214     c                   end
051700011214     C                   MOVE      dtaiso        Dtaeur
051800011214     C                   MOVE      dtaeur        Dat2              8 0
051900011214     C                   ENDSR
052000000000     C***------------------------------------------------***
052100000000     C* STAMPA TESTATA
052200000000     C*-----------------------------------------------------
052300000000     OQSYSPRT   E            TESTA            01
052400000000     O                       S(1)                66
052500000000     O                       S(2)               132
052600000000     O                       RSUT                20
052700910206     O                                           80 'DAL'
052800011214     O                       DAT1                91 '  /  /    '
052900910206     O                                           95 'AL'
053000011214     O                       DAT2               106 '  /  /    '
053100961029     O                       UDATE8             120 '  /  /    '
053200000000     O                       PAGE          Z    132
053300000000     O          E            TESTA            02
053400000000     O                       S(3)                66
053500000000     O                       S(4)               132
053600000000     O          E            TESTA            03
053700000000     O                       S(9)                66
053800000000     O                       S(10)              132
053900000000     O          E            TESTA            05
054000160517     O                                           10 'Filiale '
054100961029     O                       AMEFIL              22
054200961029     O                       DESFIL              44
054300000000     O          E            TESTA            06
054400000000     O                       S(5)                66
054500000000     O                       S(6)               132
054600000000     O          E            TESTA            07
054700011214     O                       S(7)                66
054800011214     O                       S(8)               132
054900000000     O          E            TESTA            08 09
055000000000     O                       S(5)                66
055100000000     O                       S(6)               132
055200000000     O*-----------------------------------------------------
055300000000     O* STAMPA RIGHE
055400000000     O*-----------------------------------------------------
055500000000     O          E            RIGA        1
055600000000     O                       AMENRF        Z      9
055700910205     O                       AMETGA              12
055800910205     O                       AMETGN              22
055900160517     O                       AMEcaut             25
056000160517     O                       AMEcautd            57
056100011214     O                       INTDES              77
056200011214     O                  66                      104 'DATO MAI PERVENUTO      '
056300011214     O                 N66   D1GMA         Y     88
056400011214     O                 N66   D2GMA         Y     99
056500011214     O                  81                      100 '*'
056600011214     O                 N66   SV1KM         2    110
056700011214     O                 N66   SV2KM         2    120
056800011214     O                  81                      121 '*'
056900011214     O                  81                      130 'NON FATTO'
057000160517     O          E            RIGA        1
057100160517     O                       AMEmau              25
057200160517     O                       AMEmaud             57
057300910131     O          E            SALTO       2
057400910131     O                                            9 ' '
057500910213**
057600000000*-DESDIT-----------**             **------------------------------  S1
057700910205----**                                      XX/XX/XX      Pag. XXX  S2
057800000000                                  ** ANOMALIE DI PICCOLA MANUTENZI  S3
057900011214ONE **                                          PGM.:   TRMZ19R     S4
058000011214+--------+------------+----------------------------------+--------  S5
058100011214-----------+----------+----------+----------+---------+             S6
058200011214' N° RIF.'  TARGA     'MARCA         TIPO                ' INTERVE  S7
058300011214NTO        ' EGUITO IL'  E IL    '  AL KM.  ' E AL KM '             S8
058400000000                                  **------------------------------  S9
058500000000----**                                                             S10
058600000000  XXXXXXX XX XXXXXXXXX XXXXXXXXXX XXXXXXX  XXXXXXXXXXXXXXXXXXX
058700000000XX/XX/XX   XX/XX/XX    XXXXXXX   XXXXXXX
