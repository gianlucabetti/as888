000100011213      *PARMS DFTACTGRP(*YES)
000200011213      *PARMS COMMIT(*NONE) DATFMT(*ISO)
000300011213      *PARMS CLOSQLCSR(*ENDMOD)
000400011213      *PARMS DYNUSRPRF(*OWNER)
000500000000     H DECEDIT('0,') DATEDIT(*DMY.)
000600910221     F*                   STAMPA SCHEDA TECNICA            *
000700000000     F*                   ----------------                 *
000800000000     F*----------------------------------------------------*
000900011213     FmzIMP01L  IF   E           K DISK
001000011213     FmzIGM01L  IF   E           K DISK
001100011213     FmzNOT01L  IF   E           K DISK
001200910220     FAZORG01L  IF   E           K DISK
001300910221     FTABEL00F  IF   E           K DISK
001400160517     FTntbe01l  IF   E           K DISK
001500011213     FansIF01L  IF   E           K DISK
001600000000     FQSYSPRT   O    F  132        PRINTER
001700160809     D SEL             S              1  0 DIM(11)                              ORDINAMENTO
001800160809     D KEY             S              7    DIM(5)                               CAMPI PER KEYFLD
001900011213     D DTAISO          S               D
002000011213     D DTAEUR          S               D   DATFMT(*EUR)
002100000000     D*----------------------------------------------------*
002200011213     D PARAM           DS
002300011213     D  SCAT                          1
002400011213     D  SDIT                          1
002500011213     D  SFIL                          1
002600160809     D  keyfld                       35
002700011213     D  ANNOV                         4  0
002800011213     D  MESEV                         2  0
002900011213     D  VTGA                          2
003000011213     D  VTGN                          9
003100011213     D  VIMM                          4  0
003200011213     D  VCAT                          2
003300160517     D  Vcaut                         2
003400160517     D  Vmau                          3
003500011213     D  VDIT                          3
003600011213     D  VFIL                          3  0
003700011213     D  VCDC                          8
003800160809     D  Vdadug                        8  0
003900160809     D  Valdug                        8  0
004000160809     D  Vusodg                        8  0
004100160809     D  Vusoag                        8  0
004200160517     D  dmau         E DS
004300160517     D  dcau         E DS
004400000000     D*
004500000000     D KPJBA         E DS
004600000000     D  LIBSYS                92    100
004700000000     D  MSGERR               453    502
004800910218     D                 DS
004900910218     D  DATO                   1      4  0
005000910218     D  MESE                   1      2  0
005100910218     D  ANNO                   3      4  0
005200000000     D UTEDSE        E DS                  EXTNAME(UTEDSE0F)
005300000000     D  TCU                  398    697
005400000000     D                                     DIM(50)                              TAB FLAG CNT
005500000000     D  KCU                  698    847P 0
005600000000     D                                     DIM(50)                              TAB CAPO CONTO
005700000000     D                                     PACKEVEN
005800000000     D  DTU                  848    895P 0
005900000000     D                                     DIM(12)                              DATE PARAM
006000000000     D                                     PACKEVEN
006100160809     D*-------------
006200160809     D WLBDA8          DS
006300160809     D  G02DAT                 1      8  0
006400160809     D  G02INV                 9     16  0
006500160809     D  G02ERR                17     17
006600160809     D  G02TGI                18     22  0
006700011213     D WrkSqlCmd       S           1024
006800011213     D mzame         E DS                  extname(mzame00f)
006900000000     C*----------------------------------------------------*
007000011213     c                   exsr      srsql
007100011213     C* DECODIFICA DITTA
007200011213     c                   if        not *inlr
007300011213     c* Lettura WRKAME00F
007400011213     C/EXEC SQL
007500011213     C+ PREPARE S1 FROM :WrkSqlCmd
007600011213     C/END-EXEC
007700011213
007800011213     C/EXEC SQL
007900011213     C+ DECLARE A1 CURSOR FOR S1
008000011213     C/END-EXEC
008100011213
008200011213     C/EXEC SQL
008300011213     C+ OPEN A1
008400011213     C/END-EXEC
008500011213
008600011213     C                   DOU       SqlCod <> 0
008700011213     C/EXEC SQL
008800011213     C+ FETCH NEXT FROM A1 INTO :mzame
008900011213     C/END-EXEC
009000011213     c*
009100011213     C                   SELECT
009200011213     C                   WHEN      SqlCod = 0
009300011213     c                   exsr      srdecod
009400011213     c                   exsr      srcalcol
009500011213     C                   WHEN      SqlCod = 100
009600011213     C                   WHEN      SqlCod <> 0
009700011213      * Forzo la stampa del JOBLOG.
009800011213     C                   CALL      'X66CHGJOB'
009900011213     C                   ENDSL
010000011213     C                   ENDDO
010100011213
010200011213     C/EXEC SQL
010300011213     C+ CLOSE A1
010400011213     C/END-EXEC
010500011213     c                   end
010600011213     c*
010700011213     C                   seton                                        lr
010800011213     C**----------------------
010900011213     C** decodifiche
011000011213     C**----------------------
011100011213     C     srdecod       BEGSR
011200011213     C     AMEDIT        CHAIN     ansif01L
011300011213     C                   if        %found(ansif01l)
011400011213     C                   MOVEL     SIFDESBREV    DITDES           20
011500011213     C                   ELSE
011600011213     C                   MOVEL     *ALL'?'       DITDES           20
011700011213     C                   END
011800011213     C*
011900011213     C                   MOVE      'XA'          COD
012000011213     C                   MOVEL     AMECAT        KEYt
012100011213     C     KTAB          CHAIN     TABEL                              88
012200011213     C  N88              MOVEL     TBLUNI        DESCAT           20
012300011213     C   88              MOVE      *ALL'?'       DESCAT
012400011213     C* SISTEMAZIONE MESE/ANNO IMMATR.
012500011213     C                   Z-ADD     AMEIMM        DATO
012600011213     C                   Z-ADD     MESE          APPO              2 0
012700011213     C                   Z-ADD     ANNO          MESE
012800011213     C                   Z-ADD     APPO          ANNO
012900011213     C* DECODIFICA FILIALE DOTAZIONE
013000011213     C     AMEFIL        CHAIN     AZORG                              94
013100011213     C  N94              MOVEL     ORGDES        DESFIL           20
013200011213     C   94              MOVE      *ALL'?'       DESFIL
013300011213     C* DECODIFICA AUTISTA
013400011213     C                   MOVE      'XN'          COD
013500011213     C                   MOVEL     AMECAU        KEYt
013600011213     C     KTAB          CHAIN     TABEL                              93
013700011213     C  N93              MOVEL     TBLUNI        DESAUT           20
013800011213     C   93              MOVE      *BLANKS       DESAUT
013900011213     C* GIRA DATA PROSSIMA REVISIONE
014000011221     c                   if        amedur <> 0
014100011213     C                   MOVE      AMEDUR        DTAISO
014200011213     C                   MOVE      DTAISO        DTAEUR
014300011213     C                   MOVE      DTAEUR        datain            8 0
014400011221     c                   else
014500011221     c                   z-add     0             datain
014600011221     c                   end
014700011213     C* TROVA KM PERCORSI SU ULTIMO CONSUMO IMMESSO
014800020801     C* consumi manuali
014900011213     C                   MOVE      '£1'          INT
015000011213     C     KGAI          SETGT     mzIMP000
015100011213     C                   READP     mzIMP000                               92
015200011213     C     IMPNRF        IFEQ      AMENRF
015300011213     C                   Z-ADD     IMPKM         KILOM             6 0
015400011213     C                   ELSE
015500011213     C                   Z-ADD     0             KILOM
015600011213     C                   END
015700020801     C* agip
015800020731     C                   MOVE      'CB'          INT
015900020731     C     KGAI          SETGT     mzIMP000
016000020731     C                   READP     mzIMP000                               92
016100020731     C                   IF        AMENRF = IMPNRF AND IMPKM > KILOM
016200020731     C                   Z-ADD     IMPKM         KILOM             6 0
016300020731     C                   END
016400011213     C* TROVA ULTIMO CAMBIO OLIO
016500011213     C                   MOVE      'CO'          INT
016600011213     C     KGAI          SETGT     mzIMP000
016700011213     C                   READP     mzIMP000                               92
016800011213     C     IMPNRF        IFEQ      AMENRF
016900011213     C                   Z-ADD     IMPKM         KMOIL             6 0
017000011213     C                   ELSE
017100011213     C                   Z-ADD     0             KMOIL
017200011213     C                   END
017300160517     C                   Z-ADD     21            CONRIG
017400160517     c                   clear                   amemaud          35
017500160517     c                   movel     'MAU'         tbecod
017600160517     c                   movel(p)  amemau        tbeke1
017700160517     c     ktbe          chain     tntbe01l
017800160517     c                   if        %found(tntbe01l)
017900160517     c                   movel     tbeuni        dmau
018000160517     c                   movel     §maudes       amemaud
018100160517     c                   endif
018200160517     c                   clear                   amecautd         35
018300160517     c                   movel     'CAU'         tbecod
018400160517     c                   movel(p)  amecaut       tbeke1
018500160517     c     ktbe          chain     tntbe01l
018600160517     c                   if        %found(tntbe01l)
018700160517     c                   movel     tbeuni        dcau
018800160517     c                   movel     §caudes       amecautd
018900160517     c                   endif
019000011214     C                   EXCEPT    TESTA
019100011213     c                   endsr
019200011213     C**----------------------
019300011213     C** calcoli
019400011213     C**----------------------
019500011213     C     srcalcol      BEGSR
019600011213     C* STAMPA TUTTE LE GRANDI MANUTENZIONI DELL'AUTOMEZZO
019700011213     C                   SETOFF                                       33
019800011213     C                   Z-ADD     0             XIN               3 0
019900011213     C     AMENRF        SETLL     mzIGM000                           90
020000011213     C     *IN90         DOWEQ     '0'
020100011213     C     AMENRF        READE     mzIGM000                               90
020200011213     C  N90              DO
020300011213     C                   ADD       1             XIN
020400011213     C* DECODIFICA L'INTERVENTO
020500011213     C                   MOVE      'XE'          COD
020600011213     C                   MOVEL     IGMINT        KEYt
020700011213     C     KTAB          CHAIN     TABEL                              89
020800011213     C  N89              MOVEL     TBLUNI        DESINT           25
020900011213     C   89              MOVE      *ALL'?'       DESINT
021000011213     C* DESCRIZIONE OFFICINA
021100011213     C                   MOVEL     IGMOFF        OFFI             20
021200011213     C* GIRA LA DATA INTERVENTO
021300011213     C                   move      IGMDAT        Dtaiso
021400011213     C                   move      Dtaiso        dtaeur
021500011213     C                   move      Dtaeur        datain
021600011213     C*
021700011213     C     CONRIG        IFGT      51
021800011213     C                   EXCEPT    TESTA1
021900011213     C                   Z-ADD     8             CONRIG
022000011213     C                   END
022100011213     C                   EXCEPT    DETAIL
022200011213     C                   ADD       2             CONRIG            2 0
022300011213     C* CARICA E STAMPA EVENTUALI NOTE AGGIUNTIVE
022400011213     C     KNOT          SETLL     mzNOT000                           16
022500011213     C     *IN16         DOWEQ     '0'
022600011213     C     KNOT          READE     mzNOT000                               16
022700011213     C  N16              DO
022800011213     C     CONRIG        IFGT      51
022900011213     C                   EXCEPT    TESTA
023000011213     C                   Z-ADD     23            CONRIG
023100011213     C                   END
023200011213     C                   EXCEPT    DENOT
023300011213     C                   ADD       1             CONRIG
023400011213     C                   END
023500011213     C                   END
023600011213     C*
023700011213     C                   END
023800011213     C                   END
023900011213     C     XIN           COMP      0                                      33
024000011213     C   33              EXCEPT    DETAIL
024100011213     c                   endsr
024200011213     c**********************************************************************
024300011213     c     srsql         begsr
024400011213     c**********************************************************************
024500011213     C                   EVAL      WrkSqlCmd
024600011213     C                             =
024700011213     C                             'SELECT'
024800011213     C                             +
024900011213     C                             ' * FROM MZAME00F '
025000011213     C                             +
025100011213     C                             ' WHERE AMENRF <> 0'
025200011213     c*scelta filiale
025300011213     C                   IF        vfil <> 0
025400011213     C                   EVAL      WrkSqlCmd
025500011213     C                             =
025600011213     C                             %TRIMR(WrkSqlCmd)
025700011213     C                             +
025800011213     C                             ' AND AMEFIL = ' + %editc(vfil:'Z')
025900011213     C                   endif
026000011213     c*scelta ditta
026100011213     C                   IF        vdit <> *blanks
026200011213     C                   EVAL      WrkSqlCmd
026300011213     C                             =
026400011213     C                             %TRIMR(WrkSqlCmd)
026500011213     C                             +
026600011213     C                             ' AND AMEDIT = ' + '''' + VDIT + ''''
026700011213     C                   endif
026800011213     c*scelta cdc
026900011213     C                   IF        vcdc <> *blanks
027000011213     C                   EVAL      WrkSqlCmd
027100011213     C                             =
027200011213     C                             %TRIMR(WrkSqlCmd)
027300011213     C                             +
027400011213     C                             ' AND AMECDC = ' + '''' + Vcdc + ''''
027500011213     C                   endif
027600011213     c*scelta categoria
027700011213     C                   IF        vcat <> ' '
027800050526     c                   if        %subst(vcat: 2: 1) = '*'
027900050526     c                   movel     vcat          comcat            1
028000050526     C                   EVAL      WrkSqlCmd
028100050526     C                             =
028200050526     C                             %TRIMR(WrkSqlCmd)
028300050526     C                             +
028400050526     C                             ' AND substr(AMECAT, 1, 1) = '
028500050526     C                             + '''' + COMCAT + ''''
028600050526     c                   else
028700011213     C                   EVAL      WrkSqlCmd
028800011213     C                             =
028900011213     C                             %TRIMR(WrkSqlCmd)
029000011213     C                             +
029100011213     C                             ' AND AMECAT = ' + '''' + VCAT + ''''
029200011213     C                   endif
029300050526     C                   endif
029400011213     c*scelta immatricolazione
029500011213     C                   IF        vimm <> 0
029600011213     C                   EVAL      WrkSqlCmd
029700011213     C                             =
029800011213     C                             %TRIMR(WrkSqlCmd)
029900011213     C                             +
030000011213     C                             ' AND AMEIMM = ' + %editc(vimm:'Z')
030100011213     C                   endif
030200160809     c*scelta data ultima revisione
030300160809     C                   IF        vdadug <> 0
030400160809     C                   EVAL      WrkSqlCmd
030500160809     C                             =
030600160809     C                             %TRIMR(WrkSqlCmd)
030700160809     C                             +
030800160809     C                             ' AND AMEDUR between ' + %editc(vdadug:'Z')
030900160809     C                             + ' AND ' + %editc(valdug:'Z')
031000160809     C                   endif
031100160809     c*scelta data in uso dal ...
031200160809     C                   IF        vusodg <> 0
031300160809     C                   EVAL      WrkSqlCmd
031400160809     C                             =
031500160809     C                             %TRIMR(WrkSqlCmd)
031600160809     C                             +
031700160809     C                             ' AND AMEDAL between ' + %editc(vusodg:'Z')
031800160809     C                             + ' AND ' + %editc(vusoag:'Z')
031900160809     C                   endif
032000011213     c*scelta targa
032100011213     C                   IF        vtga <> ' '
032200011213     C                   EVAL      WrkSqlCmd
032300011213     C                             =
032400011213     C                             %TRIMR(WrkSqlCmd)
032500011213     C                             +
032600011213     C                             ' AND AMETGA = ' + '''' + VTGA + ''''
032700011213     C                             +
032800011213     C                             ' AND AMETGN = ' + '''' + VTGN + ''''
032900011213     C                   endif
033000020206     c*scelta marca
033100160517     C                   IF        vcaut <> ' '
033200160517     C                   EVAL      WrkSqlCmd
033300160517     C                             =
033400160517     C                             %TRIMR(WrkSqlCmd)
033500160517     C                             +
033600160517     C                             ' AND AMECAUT = ' + '''' + VCAUT + ''''
033700160517     C                   endif
033800020206     c*scelta marca e tipo
033900160517     C                   IF        vmau <> ' '
034000160517     C                   EVAL      WrkSqlCmd
034100160517     C                             =
034200160517     C                             %TRIMR(WrkSqlCmd)
034300160517     C                             +
034400160517     C                             ' AND AMEMAU = ' + '''' + VMAU + ''''
034500160517     C                   endif
034600020206     c*
034700011213     C                   EVAL      WrkSqlCmd
034800011213     C                             =
034900011213     C                             %TRIMR(WrkSqlCmd)
035000011213     C                             +
035100011213     C                             ' ORDER BY '
035200011213     c* ORDINAMENTO
035300011213     C                   EVAL      WrkSqlCmd
035400011213     C                             =
035500011213     C                             %TRIMR(WrkSqlCmd)
035600011213     C                             + ' ' + key(1)
035700011213     c     2             do        5             aa                2 0
035800011213     c                   if        key(AA) <> 'AMENRF'
035900011213     C                   EVAL      WrkSqlCmd
036000011213     C                             =
036100011213     C                             %TRIMR(WrkSqlCmd)
036200011213     c                             +
036300011213     C                             ',' + key(aa)
036400011213     c                   end
036500011213     c                   enddo
036600011213     c                   if        key(1) <> 'AMENRF'
036700011213     C                   EVAL      WrkSqlCmd
036800011213     C                             =
036900011213     C                             %TRIMR(WrkSqlCmd)
037000011213     C                             +
037100011213     C                             ', AMENRF'
037200011213     c                   end
037300011213     c*
037400011213     C                   ENDSR
037500011213     C*---------------------------------------------------------------*
037600011213     C     *INZSR        BEGSR
037700011213     C     *ENTRY        PLIST
037800011213     C                   PARM                    KPJBA
037900011213     C                   Z-ADD     1             CODUT
038000011213     C                   CALL      'XPARUT'
038100011213     C                   PARM                    UTEDSE
038200011213     C     CODUT         ifeq      -1
038300011213     c                   seton                                        lr
038400011213     c                   end
038500011213     C                   MOVEL     RAGUT         RSUT             20
038600011213     C                   MOVEL     RAGUT         RSUT2            25
038700011213     C* LA DATA DEL GIORNO LA PRENDO DA TIME
038800011213     C                   TIME                    W0140            14 0
038900011213     C                   MOVE      W0140         UDATE8            8 0
039000011213     C*
039100011213     C     KTAB          KLIST
039200011213     C                   KFLD                    CODUT
039300011213     C                   KFLD                    COD               2
039400011213     C                   KFLD                    KEYt              8
039500160517      *
039600160517     C     KTbe          KLIST                                                  *
039700160517     C                   KFLD                    tbecod                         *
039800160517     C                   KFLD                    tbeke1                         *
039900011213     C* POSIZIONAMENTO FILE NOTE                           *
040000011213     C     KNOT          KLIST                                                  *
040100011213     C                   KFLD                    NOTTIP                         *
040200011213     C                   KFLD                    IGMNOT                         *
040300011213     C     KGAI          KLIST
040400011213     C                   KFLD                    AMENRF
040500011213     C                   KFLD                    INT               2
040600011213     C                   MOVE      *ALL'-'       TRA1             78
040700011213     C                   MOVEl     KPJBU         PARAM
040800011213     C                   MOVEA     KEYFLD        KEY
040900160809      * verifico presenza date periodi selezione
041000160809     c                   clear                   vdadur            8 0
041100160809     c                   if        vdadug > 0
041200160809     C                   MOVE      Vdadug        G02inv
041300160809     C                   MOVEL     '3'           G02ERR
041400160809     C                   CALL      'XSRDA8'
041500160809     C                   PARM                    WLBDA8
041600160809     C     G02ERR        IFne      '1'
041700160809     c                   move      g02dat        vdadur
041800160809     C                   END
041900160809     c                   endif
042000160809     c                   clear                   valdur            8 0
042100160809     c                   if        valdug > 0
042200160809     C                   MOVE      Valdug        G02inv
042300160809     C                   MOVEL     '3'           G02ERR
042400160809     C                   CALL      'XSRDA8'
042500160809     C                   PARM                    WLBDA8
042600160809     C     G02ERR        IFne      '1'
042700160809     c                   move      g02dat        valdur
042800160809     C                   END
042900160809     c                   endif
043000160809     c                   clear                   vusodal           8 0
043100160809     c                   if        vusodg > 0
043200160809     C                   MOVE      Vusodg        G02inv
043300160809     C                   MOVEL     '3'           G02ERR
043400160809     C                   CALL      'XSRDA8'
043500160809     C                   PARM                    WLBDA8
043600160809     C     G02ERR        IFne      '1'
043700160809     c                   move      g02dat        vusodal
043800160809     C                   END
043900160809     c                   endif
044000160809     c                   clear                   vusoal            8 0
044100160809     c                   if        vusoag > 0
044200160809     C                   MOVE      Vusoag        G02inv
044300160809     C                   MOVEL     '3'           G02ERR
044400160809     C                   CALL      'XSRDA8'
044500160809     C                   PARM                    WLBDA8
044600160809     C     G02ERR        IFne      '1'
044700160809     c                   move      g02dat        vusoal
044800160809     C                   END
044900160809     c                   endif
045000011213     C                   ENDSR
045100000000     C*-----------------------------------------------------
045200000000     C* STAMPA TESTATA
045300000000     C*-----------------------------------------------------
045400910222     OQSYSPRT   E            TESTA            02
045500011213     O                                            7 'TRMZ34R'
045600910222     O                                           52 'SCHEDA TECNICA AUTOVEICO'
045700910222     O                                           54 'LO'
045800961029     O                       UDATE8              78 '  /  /    '
045900910222     O          E            TESTA            03
046000910222     O                                           52 '------------------------'
046100910222     O                                           54 '--'
046200910222     O          E            TESTA            05
046300910222     O                                            7 'N° Rif:'
046400910221     O                       AMENRF        Z     15
046500910222     O                                           26 'Targa:'
046600910221     O                       AMETGA              29
046700910221     O                       AMETGN              39
046800011213     O                                           53 'Ditta:'
046900910221     O                       DITDES              75
047000910220     O*
047100910222     O          E            TESTA            07
047200910222     O                                            6 'Marca:'
047300160517     O                       AMEcaut              9
047400160517     O                       AMEcautd            47
047500011213     O                                           53 'Cat.:'
047600011213     O                       DESCAT              75
047700160517     O          E            TESTA            09
047800160517     O                                            5 'Tipo:'
047900160517     O                       AMEmau               9
048000160517     O                       AMEmaud             47
048100160517     O          E            TESTA            11
048200011213     O                                            5 'Alim:'
048300011213     O                       AMEALI               7
048400011213     O                                           15 'Cilin:'
048500011213     O                       AMECIL              26
048600011213     O                                           35 'Immatr:'
048700011213     O                       DATO                40 '  /  '
048800011213     O                                           53 'P.O.:'
048900011213     O                       DESFIL              75
049000160517     O          E            TESTA            13
049100910222     O                                            8 'Autista:'
049200910221     O                       DESAUT              29
049300011213     O                                           53 'Ultima revisione:'
049400011213     O                       DATAIN              64 '  /  /    '
049500160517     O          E            TESTA            15
049600910222     O                       TRA1                78
049700160517     O          E            TESTA            17
049800910222     O                                           13 'Km. percorsi:'
049900910221     O                       KILOM               21 '   .   '
050000910222     O                                           55 'Ultimo cambio olio Km.:'
050100910222     O                       KMOIL               65 '   .   '
050200160517     O          E            TESTA            19
050300910222     O                       TRA1                78
050400160517     O          E            TESTA            21
050500910222     O                                           24 'Interventi di riparazion'
050600910222     O                                           48 'e    Data         Km    '
050700910222     O                                           72 '  Officina              '
050800910221     O          E            DETAIL      2
050900910221     O               33                          35 '*** Nessun Intervento ef'
051000910221     O               33                          59 'fettuato ***            '
051100910222     O              N33      DESINT              25
051200011213     O              N33      DATAIN              36 '  /  /    '
051300910221     O              N33      IGMKM               47 ' .   .   '
051400910222     O              N33      OFFI                70
051500910222     O          E            DENOT       1
051600910222     O                       NOTDES              50
051700910502     O          E            TESTA1           02
051800011213     O                                            7 'TRMZ34R'
051900910502     O                                           52 'SCHEDA TECNICA AUTOVEICO'
052000910502     O                                           54 'LO'
052100910502     O                                           78 'Continua'
052200910502     O          E            TESTA1           03
052300910502     O                                           52 '------------------------'
052400910502     O                                           54 '--'
052500910502     O          E            TESTA1           05
052600910502     O                                            7 'N° Rif:'
052700910502     O                       AMENRF        Z     15
052800910502     O                                           26 'Targa:'
052900910502     O                       AMETGA              29
053000910502     O                       AMETGN              39
053100910502     O                                           49 'Ditta:'
053200910502     O                       DITDES              75
053300910502     O          E            TESTA1           07
053400910502     O                                           52 '------------------------'
053500910502     O                                           54 '--'
053600910502     O          E            TESTA1           08
053700910502     O                                           24 'Interventi di riparazion'
053800910502     O                                           48 'e    Data         Km    '
053900910502     O                                           72 '  Officina              '
054000910502     O*
