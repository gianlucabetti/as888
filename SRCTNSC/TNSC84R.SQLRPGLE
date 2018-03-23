000100910802     H DECEDIT('0,') DATEDIT(*DMY.)
000200910320     H*----------*                                                   *
000300941026     H* TNAS84R  *---------------------------------------------------*
000400910320     H*----------*                                                   *
000500910802     H*           - ESTRAZIONE CONTRASSEGNI                          *
000600910320     H*--------------------------------------------------------------*
000700941026     FAZORG01L  IF   F 5000     2PIDISK    KEYLOC(4)
000800941026     FTABEL00F  IF   E           K DISK
000900950720     FTNCSB09L  IF   E           K DISK
001000120216     FTNCSm03L  IF   E           K DISK
001100120216     FTNSC84P   O    E             PRINTER OFLIND(*IN01)
001200941026     D*--------------------------------------------------------------*
001300941026     D CCV             S              3    DIM(100)
001400000110     D DVA             S              1    DIM(100)
001500000110     D DSBS02        E DS                  EXTNAME(TIBS02DS)
001600000110     D DGED          E DS
001700971230     D PARUT         E DS                  EXTNAME(UT§DSE0F)
001800910802     D KPJBA         E DS
001900941027     D TNSC84        E DS                  EXTNAME(TNSC84DS)
002000941026     D WLBDA8          DS
002100941026     D  G02DAT                 1      8  0
002200941026     D  G02INV                 9     16  0
002300941026     D  G02ERR                17     17
002400941026     D  G02TGI                18     22  0
002500941026     D DSCV          E DS
002600120216     d totidc          s             15  3
002700120216     d div             s                   like(csmdiv)
002800120216     d conta           s                   like(totcmv)
002900941026     I*--------------------------------------------------------------*
003000941026     IAZORG01L  AA
003100941026     I                             P    4    5 0ORGFIL
003200941026     I                                 14   33  ORGDES
003300910321     C***------------------------------------------***
003400941026     C*   OPERAZIONI INIZIALI
003500941026     C***------------------------------------------***
003600950720     C     KCSB          CHAIN     TNCSB09L                           30
003700950720     C     *IN30         IFEQ      '0'
003800950720     C* Inizio Distinta
003900950720     C                   EXSR      INZDIS
004000120216     C*  Loop lettura singola distinta con singoli assegni
004100950720     C     *IN30         DOWEQ     '0'
004200950720     C                   EXSR      DETDIS
004300950720     C     KCSB          READE     TNCSB09L                               30
004400950720     C                   END
004500120216     c* stampo assegni multipli
004600120216     c                   exsr      srasstot
004700950720     C* Fine Distinta: Stampa totali
004800120216     C                   IF        *in01
004900120216     C                   write     TESTA
005000120216     C                   write     TESDIS
005100950720     C                   END
005200120216     C                   write     TOTAL
005300950720     C                   END
005400941028     C*
005500910321     C                   SETON                                        LR
005600941027     C***------------------------------------------***
005700941027     C* INIZIO DISTINTA
005800941027     C***------------------------------------------***
005900941027     C     INZDIS        BEGSR
006000941027     C*
006100941027     C*  Decodifica filiale che ha inviato la distinta
006200950720     C                   MOVE      CSBRGF        ORGFIL
006300941027     C                   MOVE      *BLANKS       ORGDES
006400941027     C     ORGFIL        CHAIN     AZORG01L                           15
006500941027     C                   MOVEL     ORGDES        DES              20
006600941027     C*  Inverto la data della distinta
006700950720     C                   Z-ADD     CSBDDC        G02INV
006800941027     C                   Z-ADD     0             G02DAT
006900941027     C                   MOVEL     '3'           G02ERR
007000941027     C                   CALL      'XSRDA8'
007100941027     C                   PARM                    WLBDA8
007200941027     C                   Z-ADD     G02DAT        WDDC              8 0
007300941027     C*  Inverto la data arrivo distinta in sede
007400950720     C                   Z-ADD     CSBUDA        G02INV
007500941027     C                   Z-ADD     0             G02DAT
007600941027     C                   MOVEL     '3'           G02ERR
007700941027     C                   CALL      'XSRDA8'
007800941027     C                   PARM                    WLBDA8
007900950720     C                   Z-ADD     G02DAT        UDA               8 0
008000941027     C*  Azzero variabili totale
008100941027     C                   Z-ADD     0             TOT              14 3
008200000110     C                   Z-ADD     0             TOTE             14 3
008300941027     C                   Z-ADD     0             TOTV             14 3
008400941027     C                   Z-ADD     0             TOTM             14 3
008500000110     C                   Z-ADD     0             TOTME            14 3
008600941027     C                   Z-ADD     0             TOTMV            14 3
008700120216     C                   Z-ADD     0             TOTMa            14 3
008800120216     C                   Z-ADD     0             TOTMEa           14 3
008900120216     C                   Z-ADD     0             TOTMVa           14 3
009000120216     C                   Z-ADD     0             TOTcM
009100120216     C                   Z-ADD     0             TOTcME
009200120216     C                   Z-ADD     0             TOTcMV
009300120216     C                   Z-ADD     0             TOTcMa
009400120216     C                   Z-ADD     0             TOTcMEa
009500120216     C                   Z-ADD     0             TOTcMVa
009600941027     C*
009700941028     C* Fine Distinta: Stampa totali
009800120216     C                   IF        *in01
009900120216     C                   write     TESTA
010000120216     c                   setoff                                       01
010100941028     C                   END
010200120216     C                   write     TESDIS
010300941027     C*
010400941027     C*  Imposto variabili rottura livello
010500941027     C                   Z-ADD     DDC           WSVDDC            8 0
010600950720     C                   Z-ADD     CSBNDT        WSVNDT            7 0
010700950720     C                   Z-ADD     CSBRGF        WSVRGF            3 0
010800941027     C*
010900941027     C                   ENDSR
011000941027     C***------------------------------------------***
011100941027     C* DETTAGLIO DISTINTA
011200941027     C***------------------------------------------***
011300941027     C     DETDIS        BEGSR
011400941027     C*
011500120216     c                   setoff                                       44
011600120216     c                   z-add     csbcas        cas
011700120216     c                   z-add     csbcas        idc
011800941027     C* Inverto la data della spedizione
011900950720     C                   MOVEL     CSBAAS        G02INV
012000950720     C                   MOVE      CSBMGS        G02INV
012100941026     C                   Z-ADD     0             G02DAT
012200941026     C                   MOVEL     '3'           G02ERR
012300941026     C                   CALL      'XSRDA8'
012400941026     C                   PARM                    WLBDA8
012500950720     C                   Z-ADD     G02DAT        DSP               8 0
012600941027     C*  Ricerco Tipo valuta
012700941026     C                   MOVEL     'N'           VALEST            1
012800950720     C     CSBVCA        IFNE      *BLANKS
012900011002     C     CSBVCA        ANDNE     §GEDCN
013000941028     C                   Z-ADD     1             X
013100950720     C     CSBVCA        LOOKUP    CCV(X)                                 32
013200000110     C     DVA(X)        IFNE      'S'
013300000110     C     *IN32         ANDEQ     '1'
013400941026     C                   MOVEL     'S'           VALEST
013500941026     C                   END
013600941026     C                   END
013700941026     C*
013800950720     C     CSBTPI        IFNE      'M'
013900941027     C*  Bartolini
014000000110     C     CSBVCA        IFEQ      *BLANKS
014100011002     C     CSBVCA        OREQ      §GEDCN
014200120216     C                   ADD       CSBCAS        TOT
014300941026     C                   ELSE
014400000110     C     VALEST        IFEQ      'S'
014500000110     C                   ADD       CSBCAS        TOTV
014600000110     C                   ELSE
014700000110     C                   ADD       CSBCAS        TOTE
014800941026     C                   END
014900000110     C                   END
015000941026     C*
015100941026     C                   ELSE
015200941027     C*  Mittente
015300000110     C     CSBVCA        IFEQ      *BLANKS
015400011002     C     CSBVCA        OREQ      §GEDCN
015500120220     c                   if        %subst(csbnra: 10: 1)=' '
015600120216     C                   ADD       CSBCAS        TOTMa
015700120216     C                   ADD       1             TOTcMa
015800120216     c                   end
015900120216     C                   ADD       CSBCAS        TOTM
016000120216     C                   ADD       1             TOTcM
016100000110     C                   ELSE
016200941026     C     VALEST        IFEQ      'S'
016300120220     c                   if        %subst(csbnra: 10: 1)=' '
016400120216     C                   ADD       CSBCAS        TOTMVa
016500120216     C                   ADD       1             TOTcMVa
016600120216     C                   END
016700120216     C                   ADD       CSBCAS        TOTMV
016800120216     C                   ADD       1             TOTcMV
016900941026     C                   ELSE
017000120220     c                   if        %subst(csbnra: 10: 1)=' '
017100120216     C                   ADD       CSBCAS        TOTMVa
017200120216     C                   ADD       1             TOTcMVa
017300910802     C                   END
017400120216     C                   ADD       CSBCAS        TOTMV
017500120216     C                   ADD       1             TOTcMV
017600120216     C                   END
017700000110     C                   END
017800941026     C*
017900941026     C                   END
018000941027     C*  Stampa
018100120220     c                   if        %subst(csbnra: 10: 1)=' '
018200120216     C                   MOVE      CSBNRA        nra
018300120216     C                   MOVE      CSBtpi        tpi
018400120216     C                   MOVE      CSBtpa        tpa
018500120216     C* Inverto la data dell' assegno
018600120216     C                   Z-ADD     CSBDTE        G02INV
018700120216     C                   Z-ADD     0             G02DAT
018800120216     C                   MOVEL     '3'           G02ERR
018900120216     C                   CALL      'XSRDA8'
019000120216     C                   PARM                    WLBDA8
019100120216     C                   Z-ADD     G02DAT        DTE               8 0
019200120216     C                   MOVE      CSBABI        ABI
019300120216     C                   MOVE      CSBCAB        CAB
019400120216     C     *IN01         IFEQ      '1'
019500120216     C                   write     TESTA
019600120216     C                   write     TESDIS
019700120216     c                   setoff                                       01
019800120216     C                   END
019900120216     C                   write     DET
020000120216     c                   else
020100120216     c                   exsr      srass
020200120216     c                   end
020300941027     C*
020400941027     C                   ENDSR
020500120216      *---------------------------------------------------------------*
020600120216      *  stampa rif assegni
020700120216      *---------------------------------------------------------------*
020800120216     C     srass         BEGSR
020900120216     C*
021000120216     c                   seton                                        44
021100120216     c     csbnra        setll     tncsm03l
021200120216     c                   do        *hival
021300120216     c     csbnra        reade     tncsm03l
021400120216     c                   if        %eof(tncsm03l)
021500120216     c                   leave
021600120216     c                   end
021700120216     C                   z-add     CSmidc        idc
021800120216     C                   MOVE      CSmNRA        nra
021900120216     C                   Z-ADD     CSmDTE        G02INV
022000120216     C                   Z-ADD     0             G02DAT
022100120216     C                   MOVEL     '3'           G02ERR
022200120216     C                   CALL      'XSRDA8'
022300120216     C                   PARM                    WLBDA8
022400120216     C                   Z-ADD     G02DAT        DTE               8 0
022500120216     C                   MOVE      CSmABI        ABI
022600120216     C                   MOVE      CSmCAB        CAB
022700120216     C                   MOVEL     CSmTPA        tpa
022800120216     C                   MOVE      CSmTPI        tpi
022900120216     C     *IN01         IFEQ      '1'
023000120216     C                   write     TESTA
023100120216     C                   write     TESDIS
023200120216     c                   setoff                                       01
023300120216     C                   END
023400120216     C                   write     DET
023500120216     c                   enddo
023600120216     c                   endsr
023700120216      *---------------------------------------------------------------*
023800120216      *  totale multiassegni
023900120216      *---------------------------------------------------------------*
024000120216     C     srasstot      BEGSR
024100120216     c                   clear                   totidc
024200120216     c                   clear                   div
024300120216     C/EXEC SQL
024400120216     C+   declare b1 cursor for
024500120216     C+   select  sum(csmidc), csmdiv, count(*)
024600120216     C+   from (SELECT DISTINCT csbnra from tncsb00f
024700120216     C+   where csbrgf = :kRGF and csbndt = :kNDT and csbddc =
024800120220     C+   :kDDC and substr(csbnra, 10, 1) <> ' ' and csbabi = 0 and
024900120216     C+   csbtpi = 'M'
025000120216     C+   group by csbnra) as pippo join tncsm00f a on
025100120216     C+   pippo.csbnra = a.csmkey
025200120216     C+   group by csmdiv
025300120216     C/END-EXEC
025400120216
025500120216     C/EXEC SQL
025600120216     C+ OPEN b1
025700120216     C/END-EXEC
025800120216
025900120216     C                   do        *hival
026000120216
026100120216     C/EXEC SQL
026200120216     C+ FETCH NEXT FROM b1 INTO :totidc, :div, :conta
026300120216     C/END-EXEC
026400120216     C                   select
026500120216     C                   when      sqlcod = 100 or sqlcod < 0
026600120216     c                   leave
026700120216     C                   WHEN      SqlCod = 0
026800120216     C     div           IFEQ      *BLANKS
026900120216     C     div           OREQ      §GEDCN
027000120216     C                   ADD       totidc        TOTMa
027100120216     C                   ADD       conta         TOTcMa
027200120216     C                   ELSE
027300120216     C                   Z-ADD     1             X
027400120216     C     div           LOOKUP    CCV(X)                                 32
027500120216     C     DVA(X)        IFNE      'S'
027600120216     C     *IN32         ANDEQ     '1'
027700120216     C                   ADD       totidc        TOTMVa
027800120216     C                   ADD       conta         TOTcMVa
027900120216     C                   ELSE
028000120216     C                   ADD       totidc        TOTMEa
028100120216     C                   ADD       conta         TOTcMEa
028200120216     C                   END
028300120216     C                   END
028400120216     c                   endsl
028500120216     c                   enddo
028600120216     C/EXEC SQL
028700120216     C+ CLOSE b1
028800120216     C/END-EXEC
028900120216     c                   endsr
029000941026     C*-------------------------------------------------------------------*
029100941026     C*  OPERAZIONI INIZIALI
029200941026     C*-------------------------------------------------------------------*
029300941026     C     *INZSR        BEGSR
029400941026      *
029500941026     C     *ENTRY        PLIST
029600941026     C                   PARM                    KPJBA
029700941027     C                   MOVEL     KPJBU         TNSC84
029800971230     C*
029900971230     C                   Z-ADD     1             CODUT
030000971230     C                   CALL      'X§PARUT'
030100971230     C                   PARM                    PARUT
030200971230     C                   MOVEL     RAGUT         RSUT             20
030300120216     c                   seton                                        01
030400941026     C*
030500941026     C*  CHIAVI
030600941026     C     KTAB          KLIST
030700941026     C                   KFLD                    KKUT
030800941026     C                   KFLD                    KCOD
030900950720     C     KCSB          KLIST
031000950720     C                   KFLD                    KRGF
031100950720     C                   KFLD                    KNDT
031200950720     C                   KFLD                    KDDC
031300941026     C*
031400941026     C*  DEFINIZIONE VARIABILI
031500941026     C     *LIKE         DEFINE    TBLKUT        KKUT
031600941026     C     *LIKE         DEFINE    TBLCOD        KCOD
031700950720     C     *LIKE         DEFINE    CSBRGF        KRGF
031800950720     C     *LIKE         DEFINE    CSBNDT        KNDT
031900950720     C     *LIKE         DEFINE    CSBDDC        KDDC
032000941026     C*
032100941026     C                   TIME                    WHHDA1           12 0
032200941026     C                   MOVE      WHHDA1        WOGGI             6 0
032300941026     C*
032400941026     C                   Z-ADD     0             DSP               8 0
032500941026     C                   Z-ADD     0             DTE               8 0
032600941026     C                   Z-ADD     0             DDC               8 0
032700941026     C                   Z-ADD     0             UDA               8 0
032800120216     C                   MOVE      *ALL'-'       TRA
032900120216     C                   MOVE      *ALL'-'       TRA1
033000000110     C*----------------------------------------------------*
033100000110     C*  RECUPERO DIVISA MONETA CORRENTE
033200000110     C                   CLEAR                   DGED
033300000110     C                   CLEAR                   DSBS02
033400000110     C                   MOVEL     'C'           T02MOD
033500000110     C                   MOVEL     KNSIF         T02SIF
033600000110     C                   MOVEL     'GED'         T02COD
033700000110     C                   MOVEL     '1'           T02KE1
033800000110     C                   CALL      'TIBS02R'
033900000110     C                   PARM                    KPJBA
034000000110     C                   PARM                    DSBS02
034100000110     C*
034200000110    3C     T02ERR        IFEQ      ' '
034300000110     C                   MOVEL     T02UNI        DGED
034400011002     C                   MOVEL     §GEDCN        DIVCO1            3
034500011002     C                   MOVEL     §GEDCN        DIVCO2            3
034600120216     C                   MOVEL     §GEDCN        DIVCO2a           3
034700000110    3C                   ENDIF
034800000110     C*----------------------------------------------------*
034900941026     C* Carico tabella cambi/divise
035000941026     C                   Z-ADD     0             X                 3 0
035100941026     C                   Z-ADD     1             KKUT
035200941026     C                   MOVEL     'CV'          KCOD
035300941026     C     KTAB          CHAIN     TABEL                              31
035400941026     C     *IN31         DOWEQ     '0'
035500941028     C     X             ANDLT     100
035600941026     C     TBLFLG        IFEQ      ' '
035700010619     C                   MOVEL     TBLKEY        WDIV              3
035800941026     C                   ADD       1             X
035900941026     C                   MOVEL     TBLKEY        CCV(X)
036000941026     C                   MOVEL     TBLUNI        DSCV
036100000110     C                   MOVEL     §CVDVA        DVA(X)
036200000110     C     §CVDVA        IFEQ      'S'
036300011002     C     WDIV          ANDNE     §GEDCN
036400000110     C                   MOVEL     WDIV          DIVAL1            3
036500000110     C                   MOVEL     WDIV          DIVAL2            3
036600120216     C                   MOVEL     WDIV          DIVAL2a           3
036700000110     C                   END
036800941026     C                   END
036900941026     C     KTAB          READE     TABEL                                  31
037000941026     C                   END
037100941028     C*
037200950720     C                   Z-ADD     D84DDC        KDDC
037300950720     C                   Z-ADD     D84NDT        KNDT
037400950720     C                   Z-ADD     D84RGF        KRGF
037500950720     C*
037600941026     C                   ENDSR
