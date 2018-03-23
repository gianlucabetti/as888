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
001600160810     D KEY             S              7    DIM(5)                               CAMPI PER KEYFLD
001700011214     D PARAM           DS
001800011214     D  SCAT                          1
001900011214     D  SDIT                          1
002000011214     D  SFIL                          1
002100160810     D  keyfld                       35
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
003300160810     D  Vdadug                        8  0
003400160810     D  Valdug                        8  0
003500160810     D  Vusodg                        8  0
003600160810     D  Vusoag                        8  0
003700160517     D  dmau         E DS
003800160517     D  dcau         E DS
003900020705     D ut§dse        E DS                  extname(ut§dse0f)
004000000000     D KPJBA         E DS
004100011214     d dtaiso          s               d
004200011214     d dtaeur          s               d   datfmt(*eur)
004300011214     d dtaiso1         s               d
004400011214     d dtaiso2         s               d
004500160810     D*-------------
004600160810     D WLBDA8          DS
004700160810     D  G02DAT                 1      8  0
004800160810     D  G02INV                 9     16  0
004900160810     D  G02ERR                17     17
005000160810     D  G02TGI                18     22  0
005100011214     D WrkSqlCmd       S           1024
005200011214     D mzame         E DS                  extname(mzame00f)
005300011214     C*----------------------------------------------------*
005400011214     C* MAIN LINE
005500011214     C*
005600011214     C*  PER FILIALE -- PER OGNI AUTOMEZZO
005700011214     C*      PER OGNI INTERVENTO
005800020730     C*         LEGGO MZIMP01L
005900011214     C*           PER OGNI COPPIA DI INTERVENTI CALCOLO LO SCOSTAMENTO
006000011214     C*           RISPETTO ALLO STANDARD
006100011214     C*           SE SUPERA LA TOLLERANZA STAMPO ANOMALIA
006200011214     C*-----------------------------------------------------*
006300011214     c                   exsr      srsql
006400011214      *
006500011214     c* Lettura WRKAME00F
006600011214     C/EXEC SQL
006700011214     C+ PREPARE S1 FROM :WrkSqlCmd
006800011214     C/END-EXEC
006900011214
007000011214     C/EXEC SQL
007100011214     C+ DECLARE A1 CURSOR FOR S1
007200011214     C/END-EXEC
007300011214
007400011214     C/EXEC SQL
007500011214     C+ OPEN A1
007600011214     C/END-EXEC
007700011214
007800011214     C                   DOU       SqlCod <> 0
007900011214     C/EXEC SQL
008000011214     C+ FETCH NEXT FROM A1 INTO :mzame
008100011214     C/END-EXEC
008200011214     C                   SELECT
008300011214     C                   WHEN      SqlCod = 0
008400011214     C                   EXSR      SRROT
008500011214     C                   EXSR      SRcalcol
008600011214     C*
008700011214     C                   WHEN      SqlCod = 100
008800011214     C                   WHEN      SqlCod <> 0
008900011214      * Forzo la stampa del JOBLOG.
009000011214     C                   CALL      'X66CHGJOB'
009100011214     C                   ENDSL
009200011214     C                   ENDDO
009300011214
009400011214     C/EXEC SQL
009500011214     C+ CLOSE A1
009600011214     C/END-EXEC
009700011214     c*
009800011214     C                   seton                                        lr
009900011214     c**********************************************************************
010000011214     c     srcalcol      begsr
010100011214     c**********************************************************************
010200011214     C* LEGGO LA TABELLA INTERVENTI
010300011214     C     AMEUTI        IFNE      *BLANK
010400011214     C                   MOVE      AMEUTI        TIMMAN
010500011214     C     KTIM          SETLL     MZTIM000
010600011214     C                   DO        *HIVAL
010700011214     C     KTIM          READE     MZTIM000                               27
010800011214     C   27              LEAVE
010900020730     C* ESCLUDE GLI INTERVENTI DI PIENO CARBURANTE
011000020730     C                   IF        TIMINT = '£1' OR TIMINT = 'CB'
011100020730     C                   ITER
011200020730     C                   END
011300020730     C*
011400011214     C                   SETOFF                                           50
011500011214     C                   Z-ADD     0             SV1DAT            8 0
011600011214     C                   Z-ADD     0             SV1KM
011700011214     C                   Z-ADD     AMENRF        IMPNRF
011800011214     C                   MOVE      TIMINT        IMPINT
011900011214     C                   Z-ADD     DATA1         IMPDAT
012000011214      * RICERCO L'ULTIMO INTERVENTO PRIMA DELLA DATA INIZIO
012100020730      * SE LO TROVO ACCENDO 50
012200011214     C     KIMP          SETLL     MZIMP000
012300011214     C                   READP     MZIMP000                               21
012400011214     C  N21IMPNRF        COMP      AMENRF                             2121
012500011214     C  N21IMPINT        COMP      TIMINT                             2121
012600011214     C                   IF        NOT *IN21
012700011214     C                   Z-ADD     IMPDAT        SV1DAT
012800011214     C                   Z-ADD     IMPKM         SV1KM             7 0
012900011214     C                   SETON                                            50
013000011214     C                   END
013100011214     C                   Z-ADD     AMENRF        IMPNRF
013200011214     C                   MOVE      TIMINT        IMPINT
013300011214     C                   Z-ADD     DATA1         IMPDAT
013400011214     C     KIMP          SETLL     MZIMP000
013500011214     C     KIMP1         READE     MZIMP000                               26
013600011214     C* se non trovo l'ultimo intervento prima della data inizio e non
013700011214     c* trovo nessun intervento dalla data di inizio emetto la segnalazione
013800011214     c* di dato mai pervenuto
013900011214     C                   IF        *IN21 AND *IN26
014000011214     C                   SETON                                        66
014100011214     C                   MOVE      'XM'          TBLCOD
014200011214     C                   MOVE      *BLANK        TBLKEY
014300011214     C                   MOVEL     TIMINT        TBLKEY
014400011214     C     KTAB          CHAIN     TABEL                              47
014500011214     C                   MOVE      *BLANK        INTDES
014600011214     C  N47              MOVEL     TBLUNI        INTDES           19
014700011214     C     TBLFLG        IFEQ      *BLANKS
014800011214     C   OA              EXCEPT    TESTA
014900011214     c                   setoff                                       oa
015000160517     c                   clear                   amemaud          30
015100160517     c                   movel     'MAU'         tbecod
015200160517     c                   movel(p)  amemau        tbeke1
015300160517     c     ktbe          chain     tntbe01l
015400160517     c                   if        %found(tntbe01l)
015500160517     c                   movel     tbeuni        dmau
015600160517     c                   movel     §maudes       amemaud
015700160517     c                   endif
015800160517     c                   clear                   amecautd         30
015900160517     c                   movel     'CAU'         tbecod
016000160517     c                   movel(p)  amecaut       tbeke1
016100160517     c     ktbe          chain     tntbe01l
016200160517     c                   if        %found(tntbe01l)
016300160517     c                   movel     tbeuni        dcau
016400160517     c                   movel     §caudes       amecautd
016500160517     c                   endif
016600011214     C                   EXCEPT    RIGA
016700011214     C                   END
016800011214     C                   SETOFF                                       66
016900011214     C                   END
017000011214     C* se ho trovo l'ultimo intervento PRIMA della data inizio ma non ho
017100011214     c* trovo nessun intervento DALLA data di inizio emetto la segnalazione
017200011214     c* che non è stato effettuato nel periodo scelto
017300011214     C                   IF        NOT *IN21 AND *IN26
017400020730     C                   EXSR      SRCARB
017500020730     C                   END
017600011214      * LEGGO I SUCCESSIVI INTERVENTI
017700011214     C     *IN26         DOWNE     '1'
017800011214     C     IMPDAT        ANDLE     DATA2
017900011214     C                   SETOFF                                           80
018000011214     c* salvo il primo intervento che leggo se non ho trovato
018100011214     C* l'ultimo intervento PRIMA della data inizio
018200011214     C                   if        not *in50
018300011214     C                   Z-ADD     IMPDAT        SV1DAT            8 0
018400011214     C                   Z-ADD     IMPKM         SV1KM             7 0
018500011214     C                   SETON                                          7950
018600011214     C                   ELSE
018700011214     C                   SETON                                        80
018800011214     C                   Z-ADD     IMPDAT        SV2DAT            8 0
018900011214     C                   Z-ADD     IMPKM         SV2KM             7 0
019000011214     C                   EXSR      CTRL
019100011214     C                   Z-ADD     SV2DAT        SV1DAT
019200011214     C                   Z-ADD     SV2KM         SV1KM
019300011214     C                   END
019400011214     C     KIMP1         READE     MZIMP000                               26
019500011214     C  N26              END
019600011214     C* SE C'E' UN SOLO INTERVENTO DI QUEL TIPO CONTROLLA SE NE TROVA ALTRI
019700011214     C* E SE CI SONO, NON INDICA ALCUNA ANOMALIA
019800011214     C                   IF        *IN79 AND NOT *IN80
019900020730     C                   EXSR      SRCARB
020000020730     C                   SETOFF                                       79
020100011214     C                   END
020200011214     C                   ENDDO
020300011214     C                   END
020400011214     C                   EXCEPT    SALTO
020500011214     c                   endsr
020600011214     C*---------------------------------------------------
020700020730     C     SRCARB        BEGSR
020800011214     C*---------------------------------------------------
020900020730     C                   Z-ADD     data2         sv2DAT            8 0
021000020730     C* SE UNITÀ DI MISURA  È KM CONTROLLO IL CARBURANTE
021100020730     C     TIMUNI        IFEQ      'KM'
021200020730     C                   Z-ADD     AMENRF        IMPNRF
021300020730     C                   Z-ADD     data2         IMPDAT
021400020801     c* provo con carburante manuale
021500020730     C                   MOVE      '£1'          IMPINT
021600020730     C     KIMP          SETGT     MZIMP000
021700020730     C     KIMP1         READPE    MZIMP000
021800020730     C                   IF        %FOUND(MZIMP01L) AND
021900020730     C                             IMPDAT <= DATA2
022000020730     C                             AND IMPDAT >= sv1dat
022100020730     C                   Z-ADD     IMPKM         SV2KM
022200020730     C                   Z-ADD     IMPDAT        sv2DAT
022300020730     C                   SETON                                            81
022400020731     C                   End
022500020801     c* provo con carburante agip
022600020730     C                   MOVE      'CB'          IMPINT
022700020730     C     KIMP          SETGT     MZIMP000
022800020730     C     KIMP1         READPE    MZIMP000
022900020730     C                   IF        %FOUND(MZIMP01L) AND
023000020730     C                             IMPDAT <= DATA2
023100020730     C                             AND IMPDAT >= sv1dat
023200020731     C                             AND IMPkm > sv2km
023300020730     C                   Z-ADD     IMPKM         SV2KM
023400020730     C                   Z-ADD     IMPDAT        sv2DAT
023500020730     C                   SETON                                            81
023600020730     C                   END
023700020730     C                   ELSE
023800020730     C                   Z-ADD     0             SV2KM
023900020730     C                   SETON                                            81
024000020730     C                   END
024100020730     C* SE TROVATO FACCIO IL CONFRONTO
024200020730     C                   IF        *IN81
024300020730     C                   EXSR      CTRL
024400020730     C                   SETOFF                                           81
024500020730     C                   END
024600020730     c                   endsr
024700020730     C*---------------------------------------------------
024800020730     C     SRROT         BEGSR
024900020730     C*---------------------------------------------------
025000011214      * ROTTURA DI FILIALE
025100011214     C     AMEFIL        IFNE      SAVFIL
025200011214      * DECODIFICA FILIALE
025300011214     C     AMEFIL        CHAIN     AZORG                              21
025400011214     C                   MOVEL     *BLANKS       DESFIL           20
025500011214     C  N21              MOVEL     ORGDES        DESFIL
025600011214     C                   SETON                                            OA
025700011214     C                   Z-ADD     AMEFIL        SAVFIL            3 0
025800011214     C                   END
025900011214     C                   ENDSR
026000000000     C*---------------------------------------------------
026100000000     C*   CTRL
026200000000     C*        CONTROLLA SCOSTAMENTI E STAMPA
026300000000     C*---------------------------------------------------
026400000000     C     CTRL          BEGSR
026500011214     c                   move      sv1dat        dtaiso1
026600011214     c                   move      sv2dat        dtaiso2
026700011214      * CALCOLO DIFFERENZE IN GG MM O KM
026800000000     C     TIMUNI        IFEQ      'GG'
026900011214     C     dtaiso2       subdur    dtaiso1       difgg:*d          7 0
027000011214     c                   z-add     difgg         diffe
027100000000     C                   END
027200000000      *
027300000000     C     TIMUNI        IFEQ      'KM'
027400000000     C     SV2KM         SUB       SV1KM         DIFFE             7 0
027500000000     C                   END
027600000000      *
027700000000     C     TIMUNI        IFEQ      'MM'
027800011214     C     dtaiso2       subdur    dtaiso1       difmm:*m          7 0
027900011214     c                   z-add     difmm         diffe
028000000000     C                   END
028100000000      *
028200000000     C     DIFFE         IFLT      0
028300910211     C     TIMUNI        IFEQ      'KM'
028400910211     C                   Z-ADD     0             DIFFE
028500910211     C                   ELSE
028600910211     C                   Z-SUB     DIFFE         DIFFE
028700910211     C                   END
028800000000     C                   END
028900000000      *
029000910211     C                   Z-ADD     0             SCOST
029100000000     C     DIFFE         IFGT      TIMPER
029200000000     C*
029300000000     C     DIFFE         SUB       TIMPER        SCOST             9 2
029400000000     C                   DIV       TIMPER        SCOST
029500000000     C                   MULT      100           SCOST
029600000000      *
029700000000     C     SCOST         IFGT      TIMTOL
029800000000     C* DECODIFICA INTERVENTO
029900000000     C                   MOVE      'XM'          TBLCOD
030000000000     C                   MOVE      *BLANK        TBLKEY
030100000000     C                   MOVEL     TIMINT        TBLKEY
030200910207     C     KTAB          CHAIN     TABEL                              47
030300000000     C                   MOVE      *BLANK        INTDES
030400910207     C  N47              MOVEL     TBLUNI        INTDES           19
030500000000      *
030600011214     C                   MOVE      DTAISO1       DTAEUR
030700011214     C                   MOVE      DTAEUR        D1GMA             8 0
030800000000      *
030900011214     C                   MOVE      DTAISO2       DTAEUR
031000011214     C                   MOVE      DTAEUR        D2GMA             8 0
031100000000      *
031200910208     C     TBLFLG        IFEQ      *BLANKS
031300000000     C   OA              EXCEPT    TESTA
031400011214     c                   setoff                                       oa
031500160517     c                   clear                   amemaud          30
031600160517     c                   movel     'MAU'         tbecod
031700160517     c                   movel(p)  amemau        tbeke1
031800160517     c     ktbe          chain     tntbe01l
031900160517     c                   if        %found(tntbe01l)
032000160517     c                   movel     tbeuni        dmau
032100160517     c                   movel     §maudes       amemaud
032200160517     c                   endif
032300160517     c                   clear                   amecautd         30
032400160517     c                   movel     'CAU'         tbecod
032500160517     c                   movel(p)  amecaut       tbeke1
032600160517     c     ktbe          chain     tntbe01l
032700160517     c                   if        %found(tntbe01l)
032800160517     c                   movel     tbeuni        dcau
032900160517     c                   movel     §caudes       amecautd
033000160517     c                   endif
033100000000     C                   EXCEPT    RIGA
033200910208     C                   END
033300000000     C                   END
033400000000     C                   END
033500000000     C                   ENDSR
033600011214     c**********************************************************************
033700011214     c     srsql         begsr
033800011214     c**********************************************************************
033900011214     C                   EVAL      WrkSqlCmd
034000011214     C                             =
034100011214     C                             'SELECT'
034200011214     C                             +
034300011214     C                             ' * FROM MZAME00F '
034400011214     C                             +
034500011214     C                             ' WHERE AMENRF <> 0'
034600011214     c*scelta filiale
034700011214     C                   IF        vfil <> 0
034800011214     C                   EVAL      WrkSqlCmd
034900011214     C                             =
035000011214     C                             %TRIMR(WrkSqlCmd)
035100011214     C                             +
035200011214     C                             ' AND AMEFIL = ' + %editc(vfil:'Z')
035300011214     C                   endif
035400011214     c*scelta ditta
035500011214     C                   IF        vdit <> *blanks
035600011214     C                   EVAL      WrkSqlCmd
035700011214     C                             =
035800011214     C                             %TRIMR(WrkSqlCmd)
035900011214     C                             +
036000011214     C                             ' AND AMEDIT = ' + '''' + VDIT + ''''
036100011214     C                   endif
036200011214     c*scelta cdc
036300011214     C                   IF        vcdc <> *blanks
036400011214     C                   EVAL      WrkSqlCmd
036500011214     C                             =
036600011214     C                             %TRIMR(WrkSqlCmd)
036700011214     C                             +
036800011214     C                             ' AND AMECDC = ' + '''' + Vcdc + ''''
036900011214     C                   endif
037000011214     c*scelta categoria
037100011214     C                   IF        vcat <> ' '
037200050526     c                   if        %subst(vcat: 2: 1) = '*'
037300050526     c                   movel     vcat          comcat            1
037400050526     C                   EVAL      WrkSqlCmd
037500050526     C                             =
037600050526     C                             %TRIMR(WrkSqlCmd)
037700050526     C                             +
037800050526     C                             ' AND substr(AMECAT, 1, 1) = '
037900050526     C                             + '''' + COMCAT + ''''
038000050526     c                   else
038100011214     C                   EVAL      WrkSqlCmd
038200011214     C                             =
038300011214     C                             %TRIMR(WrkSqlCmd)
038400011214     C                             +
038500011214     C                             ' AND AMECAT = ' + '''' + VCAT + ''''
038600011214     C                   endif
038700050526     C                   endif
038800011214     c*scelta immatricolazione
038900011214     C                   IF        vimm <> 0
039000011214     C                   EVAL      WrkSqlCmd
039100011214     C                             =
039200011214     C                             %TRIMR(WrkSqlCmd)
039300011214     C                             +
039400011214     C                             ' AND AMEIMM = ' + %editc(vimm:'Z')
039500011214     C                   endif
039600160810     c*scelta data ultima revisione
039700160810     C                   IF        vdadug <> 0
039800160810     C                   EVAL      WrkSqlCmd
039900160810     C                             =
040000160810     C                             %TRIMR(WrkSqlCmd)
040100160810     C                             +
040200160810     C                             ' AND AMEDUR between ' + %editc(vdadug:'Z')
040300160810     C                             + ' AND ' + %editc(valdug:'Z')
040400160810     C                   endif
040500160810     c*scelta data in uso dal ...
040600160810     C                   IF        vusodg <> 0
040700160810     C                   EVAL      WrkSqlCmd
040800160810     C                             =
040900160810     C                             %TRIMR(WrkSqlCmd)
041000160810     C                             +
041100160810     C                             ' AND AMEDAL between ' + %editc(vusodg:'Z')
041200160810     C                             + ' AND ' + %editc(vusoag:'Z')
041300160810     C                   endif
041400011214     c*scelta targa
041500011214     C                   IF        vtga <> ' '
041600011214     C                   EVAL      WrkSqlCmd
041700011214     C                             =
041800011214     C                             %TRIMR(WrkSqlCmd)
041900011214     C                             +
042000011214     C                             ' AND AMETGA = ' + '''' + VTGA + ''''
042100011214     C                             +
042200011214     C                             ' AND AMETGN = ' + '''' + VTGN + ''''
042300011214     C                   endif
042400020206     c*scelta marca
042500160517     C                   IF        vcaut <> ' '
042600160517     C                   EVAL      WrkSqlCmd
042700160517     C                             =
042800160517     C                             %TRIMR(WrkSqlCmd)
042900160517     C                             +
043000160517     C                             ' AND AMECAUT = ' + '''' + VCAUT + ''''
043100160517     C                   endif
043200020206     c*scelta tipo
043300160517     C                   IF        vmau <> ' '
043400160517     C                   EVAL      WrkSqlCmd
043500160517     C                             =
043600160517     C                             %TRIMR(WrkSqlCmd)
043700160517     C                             +
043800160517     C                             ' AND AMEMAU = ' + '''' + VMAU + ''''
043900160517     C                   endif
044000020206     c*
044100011214     C                   EVAL      WrkSqlCmd
044200011214     C                             =
044300011214     C                             %TRIMR(WrkSqlCmd)
044400011214     C                             +
044500011214     C                             ' ORDER BY '
044600011214     c* ORDINAMENTO
044700011214     C                   EVAL      WrkSqlCmd
044800011214     C                             =
044900011214     C                             %TRIMR(WrkSqlCmd)
045000011214     C                             + ' ' + key(1)
045100011214     c     2             do        5             aa                2 0
045200011214     c                   if        key(AA) <> 'AMENRF'
045300011214     C                   EVAL      WrkSqlCmd
045400011214     C                             =
045500011214     C                             %TRIMR(WrkSqlCmd)
045600011214     c                             +
045700011214     C                             ',' + key(aa)
045800011214     c                   end
045900011214     c                   enddo
046000011214     c                   if        key(1) <> 'AMENRF'
046100011214     C                   EVAL      WrkSqlCmd
046200011214     C                             =
046300011214     C                             %TRIMR(WrkSqlCmd)
046400011214     C                             +
046500011214     C                             ', AMENRF'
046600011214     c                   end
046700011214     c*
046800011214     C                   ENDSR
046900011214     C**********************************************************************
047000011214     C     *INZSR        BEGSR
047100011214     C**********************************************************************
047200011214     C     *ENTRY        PLIST
047300011214     C                   PARM                    KPJBA
047400011214     c                   movel     kpjbu         param
047500011214     C                   MOVEA     KEYFLD        KEY
047600011214     C                   Z-ADD     1             CODUT
047700020705     C                   CALL      'X§PARUT'
047800020705     C                   PARM                    UT§DSE
047900011214     C                   MOVEL     RAGUT         RSUT             20
048000011214     C                   MOVEL     RAGUT         RSUT2            25
048100011214     C* LA DATA DEL GIORNO LA PRENDO DA TIME
048200011214     C                   TIME                    W0140            14 0
048300011214     C                   MOVE      W0140         UDATE8            8 0
048400160810      * verifico presenza date periodi selezione
048500160810     c                   clear                   vdadur            8 0
048600160810     c                   if        vdadug > 0
048700160810     C                   MOVE      Vdadug        G02inv
048800160810     C                   MOVEL     '3'           G02ERR
048900160810     C                   CALL      'XSRDA8'
049000160810     C                   PARM                    WLBDA8
049100160810     C     G02ERR        IFne      '1'
049200160810     c                   move      g02dat        vdadur
049300160810     C                   END
049400160810     c                   endif
049500160810     c                   clear                   valdur            8 0
049600160810     c                   if        valdug > 0
049700160810     C                   MOVE      Valdug        G02inv
049800160810     C                   MOVEL     '3'           G02ERR
049900160810     C                   CALL      'XSRDA8'
050000160810     C                   PARM                    WLBDA8
050100160810     C     G02ERR        IFne      '1'
050200160810     c                   move      g02dat        valdur
050300160810     C                   END
050400160810     c                   endif
050500160810     c                   clear                   vusodal           8 0
050600160810     c                   if        vusodg > 0
050700160810     C                   MOVE      Vusodg        G02inv
050800160810     C                   MOVEL     '3'           G02ERR
050900160810     C                   CALL      'XSRDA8'
051000160810     C                   PARM                    WLBDA8
051100160810     C     G02ERR        IFne      '1'
051200160810     c                   move      g02dat        vusodal
051300160810     C                   END
051400160810     c                   endif
051500160810     c                   clear                   vusoal            8 0
051600160810     c                   if        vusoag > 0
051700160810     C                   MOVE      Vusoag        G02inv
051800160810     C                   MOVEL     '3'           G02ERR
051900160810     C                   CALL      'XSRDA8'
052000160810     C                   PARM                    WLBDA8
052100160810     C     G02ERR        IFne      '1'
052200160810     c                   move      g02dat        vusoal
052300160810     C                   END
052400160810     c                   endif
052500011214     C*----------------------------------------------------*
052600011214     C* POSIZIONAMENTO SU MZIMP000                         *
052700011214     C     KIMP          KLIST                                                  *
052800011214     C                   KFLD                    IMPNRF                         *
052900011214     C                   KFLD                    IMPINT                         *
053000011214     C                   KFLD                    IMPDAT                         *
053100011214     C     KIMP1         KLIST                                                  *
053200011214     C                   KFLD                    IMPNRF                         *
053300011214     C                   KFLD                    IMPINT                         *
053400011214     C* POSIZIONAMENTO SU MZTIM000                         *
053500011214     C     KTIM          KLIST                                                  *
053600011214     C                   KFLD                    TIMTIP                         *
053700011214     C                   KFLD                    TIMMAN                         *
053800011214     C                   MOVE      'P'           TIMTIP
053900011214     C* POSIZIONAMENTO TABELLE                             *
054000011214     C     KTAB          KLIST                                                  *
054100011214     C                   KFLD                    CODUT                          *
054200011214     C                   KFLD                    TBLCOD                         *
054300011214     C                   KFLD                    TBLKEY                         *
054400160517      *
054500160517     C     KTbe          KLIST                                                  *
054600160517     C                   KFLD                    tbecod                         *
054700160517     C                   KFLD                    tbeke1                         *
054800011214     C* data inizio
054900011214     c                   if        annoV <> 0
055000011214     C                   MOVEl     annoV         DATAX             6 0
055100011214     c                   else
055200011214     C                   movel     0001          DATAX
055300011214     c                   end
055400011214     c                   if        meseV = 0
055500011214     c                   move      01            datax
055600011214     c                   else
055700011214     C                   MOVE      meseV         DATAX
055800011214     c                   end
055900011214     C                   movel     datax         DATA1             8 0
056000011214     C                   MOVE      01            DATA1
056100011214     C                   MOVE      data1         Dtaiso
056200011214     C                   MOVE      dtaiso        Dtaeur
056300011214     C                   MOVE      dtaeur        Dat1              8 0
056400011214     C* data fine
056500011214     c                   if        annoV <> 0
056600011214     C                   MOVEl     annoV         DATAX             6 0
056700011214     c                   else
056800011214     C                   movel     9999          DATAX
056900011214     c                   end
057000011214     c                   if        meseV = 0
057100011214     c                   move      12            datax
057200011214     c                   else
057300011214     C                   MOVE      meseV         DATAX
057400011214     c                   end
057500011214     C                   movel     datax         DATA2             8 0
057600011214     C                   MOVE      01            DATA2
057700011214     c                   if        data2 = 99991201
057800011214     c                   move      31            data2
057900011214     c                   move      data2         DTAISO
058000011214     c                   else
058100011214     c                   move      data2         DTAISO
058200011214     c                   adddur    1:*m          dtaiso
058300011214     c                   subdur    1:*d          dtaiso
058400011214     c                   move      dtaiso        data2
058500011214     c                   end
058600011214     C                   MOVE      dtaiso        Dtaeur
058700011214     C                   MOVE      dtaeur        Dat2              8 0
058800011214     C                   ENDSR
058900000000     C***------------------------------------------------***
059000000000     C* STAMPA TESTATA
059100000000     C*-----------------------------------------------------
059200000000     OQSYSPRT   E            TESTA            01
059300000000     O                       S(1)                66
059400000000     O                       S(2)               132
059500000000     O                       RSUT                20
059600910206     O                                           80 'DAL'
059700011214     O                       DAT1                91 '  /  /    '
059800910206     O                                           95 'AL'
059900011214     O                       DAT2               106 '  /  /    '
060000961029     O                       UDATE8             120 '  /  /    '
060100000000     O                       PAGE          Z    132
060200000000     O          E            TESTA            02
060300000000     O                       S(3)                66
060400000000     O                       S(4)               132
060500000000     O          E            TESTA            03
060600000000     O                       S(9)                66
060700000000     O                       S(10)              132
060800000000     O          E            TESTA            05
060900160517     O                                           10 'Filiale '
061000961029     O                       AMEFIL              22
061100961029     O                       DESFIL              44
061200000000     O          E            TESTA            06
061300000000     O                       S(5)                66
061400000000     O                       S(6)               132
061500000000     O          E            TESTA            07
061600011214     O                       S(7)                66
061700011214     O                       S(8)               132
061800000000     O          E            TESTA            08 09
061900000000     O                       S(5)                66
062000000000     O                       S(6)               132
062100000000     O*-----------------------------------------------------
062200000000     O* STAMPA RIGHE
062300000000     O*-----------------------------------------------------
062400000000     O          E            RIGA        1
062500000000     O                       AMENRF        Z      9
062600910205     O                       AMETGA              12
062700910205     O                       AMETGN              22
062800160517     O                       AMEcaut             25
062900160517     O                       AMEcautd            57
063000011214     O                       INTDES              77
063100011214     O                  66                      104 'DATO MAI PERVENUTO      '
063200011214     O                 N66   D1GMA         Y     88
063300011214     O                 N66   D2GMA         Y     99
063400011214     O                  81                      100 '*'
063500011214     O                 N66   SV1KM         2    110
063600011214     O                 N66   SV2KM         2    120
063700011214     O                  81                      121 '*'
063800011214     O                  81                      130 'NON FATTO'
063900160517     O          E            RIGA        1
064000160517     O                       AMEmau              25
064100160517     O                       AMEmaud             57
064200910131     O          E            SALTO       2
064300910131     O                                            9 ' '
064400910213**
064500000000*-DESDIT-----------**             **------------------------------  S1
064600910205----**                                      XX/XX/XX      Pag. XXX  S2
064700000000                                  ** ANOMALIE DI PICCOLA MANUTENZI  S3
064800011214ONE **                                          PGM.:   TRMZ19R     S4
064900011214+--------+------------+----------------------------------+--------  S5
065000011214-----------+----------+----------+----------+---------+             S6
065100011214' N° RIF.'  TARGA     'MARCA         TIPO                ' INTERVE  S7
065200011214NTO        ' EGUITO IL'  E IL    '  AL KM.  ' E AL KM '             S8
065300000000                                  **------------------------------  S9
065400000000----**                                                             S10
065500000000  XXXXXXX XX XXXXXXXXX XXXXXXXXXX XXXXXXX  XXXXXXXXXXXXXXXXXXX
065600000000XX/XX/XX   XX/XX/XX    XXXXXXX   XXXXXXX
