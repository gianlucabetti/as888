000100011127     H DECEDIT('0,') DATEDIT(*DMY.)
000200020116
000300011127      *--------------------------------------------------------------*
000400020206      * TIBS15R * (RI)CREO NUMERATORI DI GRUPPO PER ANNO NUOVO       *
000500011127      *--------------------------------------------------------------*
000600020116
000700011127     FAZNUM01L  UF A E           K DISK
000800011127     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
000900011127
001000011127     D SK              S             66    DIM(8) CTDATA PERRCD(1)              RIGHE STAMPA
001100011127      *
001200011127     D KPJBA         E DS
001300011127     D PARAM           DS
001400020213     D  CODSOC                 1      3
001500020213     D  CODNUM                 4      6  0
001600020213     D  DELNEW                 7      7
001700011127      *
001800011127     D                 DS                  INZ
001900011127     D DATE                    1      8  0
002000011127     D  UDAMM                  3      4  0
002100011127     D  UDAAA                  5      8  0
002200011127     D                 DS                  INZ
002300011127     D NEWDAT                  1      8  0
002400011127     D  NEWAAS                 1      4  0
002500011127     D  NEWMG                  5      8  0
002600020116      *
002700011127     D WLBDAT          DS                  INZ
002800011127     D  G02DAT                 1      8  0
002900011127     D  G02INV                 9     16  0
003000011127     D  G02ERR                17     17
003100011127     D  G02TGI                18     22  0
003200011127      *
003300011127     D DCNU          E DS                  INZ
003400011127     D TIBS02DS      E DS                  INZ
003500011127     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
003600011127     D CNCR80        E DS
003700011127      *
003800011127     D OLDAAS          s              4  0 INZ
003900011127     D DELAAS          s              4  0 INZ
004000011127     D SAVcnu          s              3  0 INZ
004100011127
004200011127      *****************************************************************
004300011127
004400020206     C     K5NUMnew      KLIST
004500020213     C                   KFLD                    CODsoc
004600020116     C                   KFLD                    NEWAAS
004700020116     C                   KFLD                    NUMCNU
004800020116     C                   KFLD                    NUMPO1
004900020116     C                   KFLD                    NUMPO2
005000020206     C     K5NUMold      KLIST
005100020213     C                   KFLD                    CODsoc
005200020116     C                   KFLD                    OLDAAS
005300020116     C                   KFLD                    NUMCNU
005400020116     C                   KFLD                    NUMPO1
005500020116     C                   KFLD                    NUMPO2
005600020206     C     K3NUMold      KLIST
005700020213     C                   KFLD                    CODsoc
005800020116     C                   KFLD                    OLDAAS
005900020116     C                   KFLD                    CODNUM
006000020206     C     K2NUMold      KLIST
006100020213     C                   KFLD                    CODsoc
006200020206     C                   KFLD                    OLDAAS
006300020116     C*
006400020116     C     *LIKE         DEFINE    NUMAAA        WAAA
006500020116     C*
006600011127     C     *ENTRY        PLIST
006700011127     C                   PARM                    KPJBA
006800011127     C                   MOVEL     KPJBU         PARAM
006900011127     C*
007000011127     C                   Z-ADD     1             CODUT
007100011127     C                   CALL      'X§PARUT'
007200011127     C                   PARM                    UT§DSE
007300011127     C                   MOVEL     REC80         CNCR80
007400011127     C*
007500020116     C                   move      *date         DATE
007600011127     C     UDAMM         IFLE      7
007700011127     C     UDAAA         SUB       1             OLDAAS
007800011127     C                   Z-ADD     UDAAA         NEWAAS
007900011127     C     UDAAA         SUB       2             DELAAS
008000011127     C                   ELSE
008100011127     C                   Z-ADD     UDAAA         OLDAAS
008200011127     C     UDAAA         ADD       1             NEWAAS
008300011127     C     UDAAA         SUB       1             DELAAS
008400011127     C                   END
008500020116     C                   Z-ADD     0101          NEWMG
008600011127     C*
008700020116     C                   movel     *on           *inOF
008800020116     C*
008900020116     C     CODNUM        IFGT      0
009000020116     C                   SETON                                        01
009100020116     C                   ENDIF
009200020116     C*
009300020116     C*--------------------------------------------------------------*
009400020116     C*
009500020116     C* Cancello numeratori vecchi (tranne quelli con anno = 0)
009600020213     C*  per la società in esame
009700020206     C*
009800020213     C     CODSOC        SETLL     AZNUM000
009900020213     C     CODSOC        READE     AZNUM000
010000011127     C*
010100020213DO  1C                   DOW       not %eof(AZNUM01L)
010200020206DO  2C     NUMaaa        IFLE      DELAAS
010300020116     C* Aggancio tabella al cambio del codice numeratore
010400020206IF  3C     NUMcnu        IFNE      SAVcnu
010500011127     C                   clear                   TIBS02DS
010600011127     C                   movel     'C'           T02mod
010700011127     C                   movel     knsif         T02sif
010800011127     C                   movel     'CNU'         T02cod
010900011127     C                   movel(p)  NUMcnu        T02ke1
011000011127     C                   call      'TIBS02R'
011100011127     C                   parm                    KPJBA
011200011127     C                   parm                    TIBS02DS
011300011127     C                   if        T02err = *blanks
011400011127     C                   movel     T02uni        DCNU
011500011127     C                   z-add     NUMcnu        SAVcnu
011600011127     C                   else
011700011127     C                   clear                   DCNU
011800020206     C                   exsr      STAMPA
011900011127     C                   endif
012000020206E   3C                   ENDIF
012100011127     C*
012200020116     C* Se ha l'anno in chiave < di quello limite: si cancella
012300020206IF  3C     §CNUfaa       IFEQ      'S'
012400020116     C     NUMaaa        ANDLE     DELAAS
012500011127     C                   DELETE    AZNUM000
012600020206E   3C                   ENDIF
012700020206E   2C                   ENDIF
012800011127     C*
012900020213     C     CODSOC        READE     AZNUM000
013000020116E   1C                   ENDDO
013100020206     C*
013200011127     C*
013300020116     C* Creo numeratori per nuovo anno
013400020206     C*
013500020116IF  1C                   if        not *in01
013600020206     C     K2NUMold      SETLL     AZNUM000
013700020206     C     K2NUMold      READE     AZNUM000                               31
013800020116X   1C                   else
013900020206     C     K3NUMold      SETLL     AZNUM000
014000020206     C     K3NUMold      READE     AZNUM000                               31
014100020116E   1C                   endif
014200011127     C*
014300020116DO  1C     *IN31         DOWEQ     *OFF
014400020116     C* Aggancio tabella al cambiamento del codice numeratore
014500020116IF  2C     NUMcnu        IFNE      SAVcnu
014600011127     C                   clear                   TIBS02DS
014700011127     C                   movel     'C'           T02mod
014800011127     C                   movel     knsif         T02sif
014900011127     C                   movel     'CNU'         T02cod
015000011127     C                   movel(p)  NUMcnu        T02ke1
015100011127     C                   call      'TIBS02R'
015200011127     C                   parm                    KPJBA
015300011127     C                   parm                    TIBS02DS
015400020116IF  3C                   if        T02err = *blanks
015500011127     C                   movel     T02uni        DCNU
015600011127     C                   z-add     NUMcnu        SAVcnu
015700020116X   3C                   else
015800011127     C                   clear                   DCNU
015900020206     C                   exsr      STAMPA
016000020116E   3C                   endif
016100020116E   2C                   ENDIF
016200020206     C*
016300011127     C*
016400011127     C* D A   R I C R E A R E
016500020206     C*
016600020116IF  2C     §CNUncr       IFNE      'S'
016700020116     C* Solo se prevede l'anno corrente
016800020116     C     §CNUfaa       ANDEQ     'S'
016900020116     C* Solo se il numeratore è ancora codificato in tab.CNU
017000020116     C     T02err        andeq     *blanks
017100011127     C*
017200020206     C* Se esiste già al nuovo anno, E SE RICHIESTO, lo cancello
017300020206     C     K5NUMnew      CHAIN     AZNUM000                           32
017400020206IF  3C     *IN32         IFEQ      *OFF
017500020206     C     DELNEW        ANDEQ     'S'
017600020206     C                   DELETE    AZNUM000
017700020206     C     K5NUMnew      CHAIN     AZNUM000                           32
017800020206E   3C                   ENDIF
017900020206     C* Se esiste già, lo lascio
018000020206IF  3C     *IN32         IFEQ      *ON
018100020206     C*
018200020206     C     K5NUMold      CHAIN     AZNUM000                           32
018300020206IF  4C     *IN32         IFEQ      *OFF
018400020131     C*
018500020226     C* G E S T I O N E   R A N G E
018600020116     C                   Z-ADD     §CNUrin       NUMrin
018700020116     C                   Z-ADD     §CNUrfi       NUMrfi
018800020116     C* NUMERATORE DA AZZERARE
018900020226IF  5C     NUMrin        IFGT      0
019000020226     C     NUMrin        SUB       §CNUinc       NUMunp
019100020226X   5C                   ELSE
019200020116     C                   Z-ADD     NUMrin        NUMunp
019300020226E   5C                   ENDIF
019400020116     C*
019500020116     C                   Z-ADD     NEWDAT        NUMdac                          DATA
019600020116     C                   Z-ADD     NEWAAS        NUMaaa                          ANNO NUOVO
019700020208     C                   clear                   NUMflo
019800020116     C                   WRITE     AZNUM000
019900020131     C*
020000020206E   4C                   ENDIF
020100020206     C*
020200020206E   3C                   ENDIF
020300020206     C*
020400020206     C* Mi riposiziono sul (vecchio) record letto
020500020206     C     K5NUMold      CHAIN     AZNUM000                           32
020600020131     C*
020700020116E   2C                   ENDIF
020800011127     C*
020900020206IF  2C                   if        not *in01
021000020206     C     K2NUMold      READE     AZNUM000                               31
021100020206X   2C                   else
021200020206     C     K3NUMold      READE     AZNUM000                               31
021300020206E   2C                   endif
021400020116E   1C                   ENDDO
021500011127     C*
021600011127     C   10              EXCEPT    PIEDI
021700011127     C                   SETON                                        LR
021800011127
021900011127      *****************************************************************
022000011127      *--- STAMPO ERRORE MANCA NUMERATORE IN TABELLA -----------------*
022100011127      *****************************************************************
022200011127     C     STAMPA        BEGSR
022300011127     C                   SETON                                        10
022400011127     C* GIRO DATA STAMPA
022500011127     C                   Z-ADD     NUMDAC        G02INV
022600011127     C                   MOVE      *ZEROS        G02DAT
022700011127     C                   MOVEL     3             G02ERR
022800011127     C                   CALL      'XSRDA8'
022900011127     C                   PARM                    WLBDAT
023000011127     C                   Z-ADD     G02DAT        STADAT            8 0
023100011127     C*
023200011127     C   OF              EXCEPT    TES
023300011127     C   OF              SETOFF                                       OF
023400011127     C*
023500011127     C                   EXCEPT    DET
023600011127     C                   ENDSR
023700011127
023800011127      *****************************************************************
023900011127
024000011127     OQSYSPRT   E            TES              02
024100011127     O                       SK(1)               66
024200011127     O                       SK(2)              132
024300011127     O                       SIMFEL              20
024400020116     O                       DATE               120 '  /  /    '
024500011127     O          E            TES              04
024600011127     O                       SK(3)               66
024700011127     O                       SK(5)              132
024800011127     O          E            DET         1
024900020206     O                       NUMSOC               5
025000020206     O                       NUMAAA              14
025100020206     O                       NUMCNU              26
025200020206     O                       NUMPO1              38
025300020206     O                       NUMUNP        Z     56
025400020206     O                       STADAT              68 '  /  /    '
025500020206     O                       NUMRIN        Z     84
025600020206     O                                           86 '/'
025700020206     O                       NUMRFI        Z    102
025800011127     O          E            PIEDI            64
025900011127     O                       SK(7)               66
026000011127     O                       SK(8)              132
026100011127     O          E            PIEDI       0  0
026200011127     O                       SK(7)               66
026300020206     O                       SK(8)              132
026400011127**
026500011127PUNTO OPERATIVO: XXX   *** STAMPA NUMERATORI ERRATI DI CUI NON TRO
026600011127VATA LA TABELLA ***
026700020206SOCIETA'  ANNO   COD.NUMERATORE   P.O.1       NUMERATORE     DATA
026800020206  XXX     XXXX         XXX         XXX   123456789012345  XX/XX/XX
026900020206          RANGE DA           RANGE A
027000020206XX XXXXXXXXXXXXXXX / XXXXXXXXXXXXXXX
027100011127           ***  SPEDIRE  CON  URGENZA  QUESTA  STAMPA  ALL'UFFICIO
027200011127  C.E.D.  IN  SEDE  ***
