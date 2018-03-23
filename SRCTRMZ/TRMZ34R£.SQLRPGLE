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
001700011213     D SEL             S              1  0 DIM(9)                               ORDINAMENTO
001800011213     D KEY             S              6    DIM(5)                               CAMPI PER KEYFLD
001900011213     D DTAISO          S               D
002000011213     D DTAEUR          S               D   DATFMT(*EUR)
002100000000     D*----------------------------------------------------*
002200011213     D PARAM           DS
002300011213     D  SCAT                          1
002400011213     D  SDIT                          1
002500011213     D  SFIL                          1
002600011213     D  keyfld                       30
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
003800160517     D  dmau         E DS
003900160517     D  dcau         E DS
004000000000     D*
004100000000     D KPJBA         E DS
004200000000     D  LIBSYS                92    100
004300000000     D  MSGERR               453    502
004400910218     D                 DS
004500910218     D  DATO                   1      4  0
004600910218     D  MESE                   1      2  0
004700910218     D  ANNO                   3      4  0
004800000000     D UTEDSE        E DS                  EXTNAME(UTEDSE0F)
004900000000     D  TCU                  398    697
005000000000     D                                     DIM(50)                              TAB FLAG CNT
005100000000     D  KCU                  698    847P 0
005200000000     D                                     DIM(50)                              TAB CAPO CONTO
005300000000     D                                     PACKEVEN
005400000000     D  DTU                  848    895P 0
005500000000     D                                     DIM(12)                              DATE PARAM
005600000000     D                                     PACKEVEN
005700011213     D WrkSqlCmd       S           1024
005800011213     D mzame         E DS                  extname(mzame00f)
005900000000     C*----------------------------------------------------*
006000011213     c                   exsr      srsql
006100011213     C* DECODIFICA DITTA
006200011213     c                   if        not *inlr
006300011213     c* Lettura WRKAME00F
006400011213     C/EXEC SQL
006500011213     C+ PREPARE S1 FROM :WrkSqlCmd
006600011213     C/END-EXEC
006700011213
006800011213     C/EXEC SQL
006900011213     C+ DECLARE A1 CURSOR FOR S1
007000011213     C/END-EXEC
007100011213
007200011213     C/EXEC SQL
007300011213     C+ OPEN A1
007400011213     C/END-EXEC
007500011213
007600011213     C                   DOU       SqlCod <> 0
007700011213     C/EXEC SQL
007800011213     C+ FETCH NEXT FROM A1 INTO :mzame
007900011213     C/END-EXEC
008000011213     c*
008100011213     C                   SELECT
008200011213     C                   WHEN      SqlCod = 0
008300011213     c                   exsr      srdecod
008400011213     c                   exsr      srcalcol
008500011213     C                   WHEN      SqlCod = 100
008600011213     C                   WHEN      SqlCod <> 0
008700011213      * Forzo la stampa del JOBLOG.
008800011213     C                   CALL      'X66CHGJOB'
008900011213     C                   ENDSL
009000011213     C                   ENDDO
009100011213
009200011213     C/EXEC SQL
009300011213     C+ CLOSE A1
009400011213     C/END-EXEC
009500011213     c                   end
009600011213     c*
009700011213     C                   seton                                        lr
009800011213     C**----------------------
009900011213     C** decodifiche
010000011213     C**----------------------
010100011213     C     srdecod       BEGSR
010200011213     C     AMEDIT        CHAIN     ansif01L
010300011213     C                   if        %found(ansif01l)
010400011213     C                   MOVEL     SIFDESBREV    DITDES           20
010500011213     C                   ELSE
010600011213     C                   MOVEL     *ALL'?'       DITDES           20
010700011213     C                   END
010800011213     C*
010900011213     C                   MOVE      'XA'          COD
011000011213     C                   MOVEL     AMECAT        KEYt
011100011213     C     KTAB          CHAIN     TABEL                              88
011200011213     C  N88              MOVEL     TBLUNI        DESCAT           20
011300011213     C   88              MOVE      *ALL'?'       DESCAT
011400011213     C* SISTEMAZIONE MESE/ANNO IMMATR.
011500011213     C                   Z-ADD     AMEIMM        DATO
011600011213     C                   Z-ADD     MESE          APPO              2 0
011700011213     C                   Z-ADD     ANNO          MESE
011800011213     C                   Z-ADD     APPO          ANNO
011900011213     C* DECODIFICA FILIALE DOTAZIONE
012000011213     C     AMEFIL        CHAIN     AZORG                              94
012100011213     C  N94              MOVEL     ORGDES        DESFIL           20
012200011213     C   94              MOVE      *ALL'?'       DESFIL
012300011213     C* DECODIFICA AUTISTA
012400011213     C                   MOVE      'XN'          COD
012500011213     C                   MOVEL     AMECAU        KEYt
012600011213     C     KTAB          CHAIN     TABEL                              93
012700011213     C  N93              MOVEL     TBLUNI        DESAUT           20
012800011213     C   93              MOVE      *BLANKS       DESAUT
012900011213     C* GIRA DATA PROSSIMA REVISIONE
013000011221     c                   if        amedur <> 0
013100011213     C                   MOVE      AMEDUR        DTAISO
013200011213     C                   MOVE      DTAISO        DTAEUR
013300011213     C                   MOVE      DTAEUR        datain            8 0
013400011221     c                   else
013500011221     c                   z-add     0             datain
013600011221     c                   end
013700011213     C* TROVA KM PERCORSI SU ULTIMO CONSUMO IMMESSO
013800020801     C* consumi manuali
013900011213     C                   MOVE      '£1'          INT
014000011213     C     KGAI          SETGT     mzIMP000
014100011213     C                   READP     mzIMP000                               92
014200011213     C     IMPNRF        IFEQ      AMENRF
014300011213     C                   Z-ADD     IMPKM         KILOM             6 0
014400011213     C                   ELSE
014500011213     C                   Z-ADD     0             KILOM
014600011213     C                   END
014700020801     C* agip
014800020731     C                   MOVE      'CB'          INT
014900020731     C     KGAI          SETGT     mzIMP000
015000020731     C                   READP     mzIMP000                               92
015100020731     C                   IF        AMENRF = IMPNRF AND IMPKM > KILOM
015200020731     C                   Z-ADD     IMPKM         KILOM             6 0
015300020731     C                   END
015400011213     C* TROVA ULTIMO CAMBIO OLIO
015500011213     C                   MOVE      'CO'          INT
015600011213     C     KGAI          SETGT     mzIMP000
015700011213     C                   READP     mzIMP000                               92
015800011213     C     IMPNRF        IFEQ      AMENRF
015900011213     C                   Z-ADD     IMPKM         KMOIL             6 0
016000011213     C                   ELSE
016100011213     C                   Z-ADD     0             KMOIL
016200011213     C                   END
016300160517     C                   Z-ADD     21            CONRIG
016400160517     c                   clear                   amemaud          35
016500160517     c                   movel     'MAU'         tbecod
016600160517     c                   movel(p)  amemau        tbeke1
016700160517     c     ktbe          chain     tntbe01l
016800160517     c                   if        %found(tntbe01l)
016900160517     c                   movel     tbeuni        dmau
017000160517     c                   movel     §maudes       amemaud
017100160517     c                   endif
017200160517     c                   clear                   amecautd         35
017300160517     c                   movel     'CAU'         tbecod
017400160517     c                   movel(p)  amecaut       tbeke1
017500160517     c     ktbe          chain     tntbe01l
017600160517     c                   if        %found(tntbe01l)
017700160517     c                   movel     tbeuni        dcau
017800160517     c                   movel     §caudes       amecautd
017900160517     c                   endif
018000011214     C                   EXCEPT    TESTA
018100011213     c                   endsr
018200011213     C**----------------------
018300011213     C** calcoli
018400011213     C**----------------------
018500011213     C     srcalcol      BEGSR
018600011213     C* STAMPA TUTTE LE GRANDI MANUTENZIONI DELL'AUTOMEZZO
018700011213     C                   SETOFF                                       33
018800011213     C                   Z-ADD     0             XIN               3 0
018900011213     C     AMENRF        SETLL     mzIGM000                           90
019000011213     C     *IN90         DOWEQ     '0'
019100011213     C     AMENRF        READE     mzIGM000                               90
019200011213     C  N90              DO
019300011213     C                   ADD       1             XIN
019400011213     C* DECODIFICA L'INTERVENTO
019500011213     C                   MOVE      'XE'          COD
019600011213     C                   MOVEL     IGMINT        KEYt
019700011213     C     KTAB          CHAIN     TABEL                              89
019800011213     C  N89              MOVEL     TBLUNI        DESINT           25
019900011213     C   89              MOVE      *ALL'?'       DESINT
020000011213     C* DESCRIZIONE OFFICINA
020100011213     C                   MOVEL     IGMOFF        OFFI             20
020200011213     C* GIRA LA DATA INTERVENTO
020300011213     C                   move      IGMDAT        Dtaiso
020400011213     C                   move      Dtaiso        dtaeur
020500011213     C                   move      Dtaeur        datain
020600011213     C*
020700011213     C     CONRIG        IFGT      51
020800011213     C                   EXCEPT    TESTA1
020900011213     C                   Z-ADD     8             CONRIG
021000011213     C                   END
021100011213     C                   EXCEPT    DETAIL
021200011213     C                   ADD       2             CONRIG            2 0
021300011213     C* CARICA E STAMPA EVENTUALI NOTE AGGIUNTIVE
021400011213     C     KNOT          SETLL     mzNOT000                           16
021500011213     C     *IN16         DOWEQ     '0'
021600011213     C     KNOT          READE     mzNOT000                               16
021700011213     C  N16              DO
021800011213     C     CONRIG        IFGT      51
021900011213     C                   EXCEPT    TESTA
022000011213     C                   Z-ADD     23            CONRIG
022100011213     C                   END
022200011213     C                   EXCEPT    DENOT
022300011213     C                   ADD       1             CONRIG
022400011213     C                   END
022500011213     C                   END
022600011213     C*
022700011213     C                   END
022800011213     C                   END
022900011213     C     XIN           COMP      0                                      33
023000011213     C   33              EXCEPT    DETAIL
023100011213     c                   endsr
023200011213     c**********************************************************************
023300011213     c     srsql         begsr
023400011213     c**********************************************************************
023500011213     C                   EVAL      WrkSqlCmd
023600011213     C                             =
023700011213     C                             'SELECT'
023800011213     C                             +
023900011213     C                             ' * FROM MZAME00F '
024000011213     C                             +
024100011213     C                             ' WHERE AMENRF <> 0'
024200011213     c*scelta filiale
024300011213     C                   IF        vfil <> 0
024400011213     C                   EVAL      WrkSqlCmd
024500011213     C                             =
024600011213     C                             %TRIMR(WrkSqlCmd)
024700011213     C                             +
024800011213     C                             ' AND AMEFIL = ' + %editc(vfil:'Z')
024900011213     C                   endif
025000011213     c*scelta ditta
025100011213     C                   IF        vdit <> *blanks
025200011213     C                   EVAL      WrkSqlCmd
025300011213     C                             =
025400011213     C                             %TRIMR(WrkSqlCmd)
025500011213     C                             +
025600011213     C                             ' AND AMEDIT = ' + '''' + VDIT + ''''
025700011213     C                   endif
025800011213     c*scelta cdc
025900011213     C                   IF        vcdc <> *blanks
026000011213     C                   EVAL      WrkSqlCmd
026100011213     C                             =
026200011213     C                             %TRIMR(WrkSqlCmd)
026300011213     C                             +
026400011213     C                             ' AND AMECDC = ' + '''' + Vcdc + ''''
026500011213     C                   endif
026600011213     c*scelta categoria
026700011213     C                   IF        vcat <> ' '
026800050526     c                   if        %subst(vcat: 2: 1) = '*'
026900050526     c                   movel     vcat          comcat            1
027000050526     C                   EVAL      WrkSqlCmd
027100050526     C                             =
027200050526     C                             %TRIMR(WrkSqlCmd)
027300050526     C                             +
027400050526     C                             ' AND substr(AMECAT, 1, 1) = '
027500050526     C                             + '''' + COMCAT + ''''
027600050526     c                   else
027700011213     C                   EVAL      WrkSqlCmd
027800011213     C                             =
027900011213     C                             %TRIMR(WrkSqlCmd)
028000011213     C                             +
028100011213     C                             ' AND AMECAT = ' + '''' + VCAT + ''''
028200011213     C                   endif
028300050526     C                   endif
028400011213     c*scelta immatricolazione
028500011213     C                   IF        vimm <> 0
028600011213     C                   EVAL      WrkSqlCmd
028700011213     C                             =
028800011213     C                             %TRIMR(WrkSqlCmd)
028900011213     C                             +
029000011213     C                             ' AND AMEIMM = ' + %editc(vimm:'Z')
029100011213     C                   endif
029200011213     c*scelta targa
029300011213     C                   IF        vtga <> ' '
029400011213     C                   EVAL      WrkSqlCmd
029500011213     C                             =
029600011213     C                             %TRIMR(WrkSqlCmd)
029700011213     C                             +
029800011213     C                             ' AND AMETGA = ' + '''' + VTGA + ''''
029900011213     C                             +
030000011213     C                             ' AND AMETGN = ' + '''' + VTGN + ''''
030100011213     C                   endif
030200020206     c*scelta marca
030300160517     C                   IF        vcaut <> ' '
030400160517     C                   EVAL      WrkSqlCmd
030500160517     C                             =
030600160517     C                             %TRIMR(WrkSqlCmd)
030700160517     C                             +
030800160517     C                             ' AND AMECAUT = ' + '''' + VCAUT + ''''
030900160517     C                   endif
031000020206     c*scelta marca e tipo
031100160517     C                   IF        vmau <> ' '
031200160517     C                   EVAL      WrkSqlCmd
031300160517     C                             =
031400160517     C                             %TRIMR(WrkSqlCmd)
031500160517     C                             +
031600160517     C                             ' AND AMEMAU = ' + '''' + VMAU + ''''
031700160517     C                   endif
031800020206     c*
031900011213     C                   EVAL      WrkSqlCmd
032000011213     C                             =
032100011213     C                             %TRIMR(WrkSqlCmd)
032200011213     C                             +
032300011213     C                             ' ORDER BY '
032400011213     c* ORDINAMENTO
032500011213     C                   EVAL      WrkSqlCmd
032600011213     C                             =
032700011213     C                             %TRIMR(WrkSqlCmd)
032800011213     C                             + ' ' + key(1)
032900011213     c     2             do        5             aa                2 0
033000011213     c                   if        key(AA) <> 'AMENRF'
033100011213     C                   EVAL      WrkSqlCmd
033200011213     C                             =
033300011213     C                             %TRIMR(WrkSqlCmd)
033400011213     c                             +
033500011213     C                             ',' + key(aa)
033600011213     c                   end
033700011213     c                   enddo
033800011213     c                   if        key(1) <> 'AMENRF'
033900011213     C                   EVAL      WrkSqlCmd
034000011213     C                             =
034100011213     C                             %TRIMR(WrkSqlCmd)
034200011213     C                             +
034300011213     C                             ', AMENRF'
034400011213     c                   end
034500011213     c*
034600011213     C                   ENDSR
034700011213     C*---------------------------------------------------------------*
034800011213     C     *INZSR        BEGSR
034900011213     C     *ENTRY        PLIST
035000011213     C                   PARM                    KPJBA
035100011213     C                   Z-ADD     1             CODUT
035200011213     C                   CALL      'XPARUT'
035300011213     C                   PARM                    UTEDSE
035400011213     C     CODUT         ifeq      -1
035500011213     c                   seton                                        lr
035600011213     c                   end
035700011213     C                   MOVEL     RAGUT         RSUT             20
035800011213     C                   MOVEL     RAGUT         RSUT2            25
035900011213     C* LA DATA DEL GIORNO LA PRENDO DA TIME
036000011213     C                   TIME                    W0140            14 0
036100011213     C                   MOVE      W0140         UDATE8            8 0
036200011213     C*
036300011213     C     KTAB          KLIST
036400011213     C                   KFLD                    CODUT
036500011213     C                   KFLD                    COD               2
036600011213     C                   KFLD                    KEYt              8
036700160517      *
036800160517     C     KTbe          KLIST                                                  *
036900160517     C                   KFLD                    tbecod                         *
037000160517     C                   KFLD                    tbeke1                         *
037100011213     C* POSIZIONAMENTO FILE NOTE                           *
037200011213     C     KNOT          KLIST                                                  *
037300011213     C                   KFLD                    NOTTIP                         *
037400011213     C                   KFLD                    IGMNOT                         *
037500011213     C     KGAI          KLIST
037600011213     C                   KFLD                    AMENRF
037700011213     C                   KFLD                    INT               2
037800011213     C                   MOVE      *ALL'-'       TRA1             78
037900011213     C                   MOVEl     KPJBU         PARAM
038000011213     C                   MOVEA     KEYFLD        KEY
038100011213     C                   ENDSR
038200000000     C*-----------------------------------------------------
038300000000     C* STAMPA TESTATA
038400000000     C*-----------------------------------------------------
038500910222     OQSYSPRT   E            TESTA            02
038600011213     O                                            7 'TRMZ34R'
038700910222     O                                           52 'SCHEDA TECNICA AUTOVEICO'
038800910222     O                                           54 'LO'
038900961029     O                       UDATE8              78 '  /  /    '
039000910222     O          E            TESTA            03
039100910222     O                                           52 '------------------------'
039200910222     O                                           54 '--'
039300910222     O          E            TESTA            05
039400910222     O                                            7 'N° Rif:'
039500910221     O                       AMENRF        Z     15
039600910222     O                                           26 'Targa:'
039700910221     O                       AMETGA              29
039800910221     O                       AMETGN              39
039900011213     O                                           53 'Ditta:'
040000910221     O                       DITDES              75
040100910220     O*
040200910222     O          E            TESTA            07
040300910222     O                                            6 'Marca:'
040400160517     O                       AMEcaut              9
040500160517     O                       AMEcautd            47
040600011213     O                                           53 'Cat.:'
040700011213     O                       DESCAT              75
040800160517     O          E            TESTA            09
040900160517     O                                            5 'Tipo:'
041000160517     O                       AMEmau               9
041100160517     O                       AMEmaud             47
041200160517     O          E            TESTA            11
041300011213     O                                            5 'Alim:'
041400011213     O                       AMEALI               7
041500011213     O                                           15 'Cilin:'
041600011213     O                       AMECIL              26
041700011213     O                                           35 'Immatr:'
041800011213     O                       DATO                40 '  /  '
041900011213     O                                           53 'P.O.:'
042000011213     O                       DESFIL              75
042100160517     O          E            TESTA            13
042200910222     O                                            8 'Autista:'
042300910221     O                       DESAUT              29
042400011213     O                                           53 'Ultima revisione:'
042500011213     O                       DATAIN              64 '  /  /    '
042600160517     O          E            TESTA            15
042700910222     O                       TRA1                78
042800160517     O          E            TESTA            17
042900910222     O                                           13 'Km. percorsi:'
043000910221     O                       KILOM               21 '   .   '
043100910222     O                                           55 'Ultimo cambio olio Km.:'
043200910222     O                       KMOIL               65 '   .   '
043300160517     O          E            TESTA            19
043400910222     O                       TRA1                78
043500160517     O          E            TESTA            21
043600910222     O                                           24 'Interventi di riparazion'
043700910222     O                                           48 'e    Data         Km    '
043800910222     O                                           72 '  Officina              '
043900910221     O          E            DETAIL      2
044000910221     O               33                          35 '*** Nessun Intervento ef'
044100910221     O               33                          59 'fettuato ***            '
044200910222     O              N33      DESINT              25
044300011213     O              N33      DATAIN              36 '  /  /    '
044400910221     O              N33      IGMKM               47 ' .   .   '
044500910222     O              N33      OFFI                70
044600910222     O          E            DENOT       1
044700910222     O                       NOTDES              50
044800910502     O          E            TESTA1           02
044900011213     O                                            7 'TRMZ34R'
045000910502     O                                           52 'SCHEDA TECNICA AUTOVEICO'
045100910502     O                                           54 'LO'
045200910502     O                                           78 'Continua'
045300910502     O          E            TESTA1           03
045400910502     O                                           52 '------------------------'
045500910502     O                                           54 '--'
045600910502     O          E            TESTA1           05
045700910502     O                                            7 'N° Rif:'
045800910502     O                       AMENRF        Z     15
045900910502     O                                           26 'Targa:'
046000910502     O                       AMETGA              29
046100910502     O                       AMETGN              39
046200910502     O                                           49 'Ditta:'
046300910502     O                       DITDES              75
046400910502     O          E            TESTA1           07
046500910502     O                                           52 '------------------------'
046600910502     O                                           54 '--'
046700910502     O          E            TESTA1           08
046800910502     O                                           24 'Interventi di riparazion'
046900910502     O                                           48 'e    Data         Km    '
047000910502     O                                           72 '  Officina              '
047100910502     O*
