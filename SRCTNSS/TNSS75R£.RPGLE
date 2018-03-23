000100000000     H*------------------------------------------------------------------------*
000200000000     H* STATISTICA TRAINI - CREA WORK FILE                                     *
000300000000     H*------------------------------------------------------------------------*
000400000000     H DECEDIT('0,') DATEDIT(*DMY.)
000500000000     F*------------------------------------------------------------------------*
000600000000     F* DATA BASE
000700000000     F*------------------------------------------------------------------------*
000800000000     F*-------------------
000900000000     F* METRICUBI CONFERMATI
001000000000     F*-------------------
001100000000     FTNTSM04L  IF   E           K DISK
001200000000     F*-------------------
001300000000     F* TRAINI CONFERMATI
001400000000     F*-------------------
001500000000     FTNTST01L  IF   E           K DISK
001600000000     F*-------------------
001700000000     F* WORK FILE
001800000000     F*-------------------
001900000000     FWFTRA01L  UF A E           K DISK
002000000000     D*------------------------------------------------------------------------*
002100000000     D* SCHIERE
002200000000     D*------------------------------------------------------------------------*
002300000000     D*------------------
002400000000     D* METRICUBI LINEA-LINEA                            - 3 DECADI -
002500000000     D*------------------
002600000000     D LGPN            S              9  0 DIM(3)                               *GIORNI LAVORATI
002700000000     D LMAN            S              9  3 DIM(3)                               *MC ASSEGNATI NORMAL
002800000000     D LMAB            S              9  3 DIM(3)                               *MC ASSEGNATI BIS
002900000000     D LMAS            S              9  3 DIM(3)                               *MC ASSGNATI SEDE
003000000000     D*------------------------------------------------------------------------*
003100000000     D* INPUT
003200000000     D*------------------------------------------------------------------------*
003300000000     D*------------------
003400000000     D* ARCHITETTURA
003500000000     D*------------------
003600000000     D KPJBA         E DS
003700000000     D*------------------
003800000000     D* PARAMETRI DI LANCIO
003900000000     D*------------------
004000000000     D PARAM           DS
004100990604     D  PARDAT                 1      6  0
004200990604     D  PARDA2                 7     12  0
004300990604     D  PARMPA                13     13
004400990604     D  PARFPC                14     14
004500990604     D  PARCRE                15     15
004600000707     D  PARPOS                16     16
004700000000     D*------------------
004800000000     D* DATA TRAINO - TNTSM
004900000000     D*------------------
005000000000     D                 DS
005100000000     D  TSMANN                 1      4  0
005200000000     D  TSMMES                 5      6  0
005300960926     D  TSMDAT                 1      6  0
005400000000     D  TSMGIO                 7      8  0
005500000000     D  TSMDTN                 1      8  0
005600000000     D*------------------
005700000000     D* DATA WORKFILE
005800000000     D*------------------
005900000000     D                 DS
006000000000     D  WFRANN                 1      4  0
006100000000     D  WFRMES                 5      6  0
006200960927     D  WFRDAT                 1      6  0
006300000000     D*------------------
006400000000     D* DEPOSITO DATA TRAINO
006500000000     D*------------------
006600000000     D                 DS
006700000000     D  DEPANN                 1      4  0
006800000000     D  DEPMES                 5      6  0
006900000000     D  DEPGIO                 7      8  0
007000000000     D  DEPDTN                 1      8  0
007100960926     D*------------------
007200960926     D* DS SCOMPOSIZIONE DATA GENERICA
007300960926     D*------------------
007400960926     D                 DS
007500960926     D  $DASEC                 1      2  0
007600960926     D  $DAANN                 3      4  0
007700960926     D  $DAAES                 1      4  0
007800960926     D  $DAMES                 5      6  0
007900960926     D  $DAGIO                 7      8  0
008000960926     D  $DATA8                 1      8  0
008100000000     C*------------------------------------------------------------------------*
008200000000     C* MAIN LINE
008300000000     C*------------------------------------------------------------------------*
008400000000     C*
008500000000     C* POSIZIONAMENTO SUL FILE E PRIMA LETTURA
008600000000     C                   EXSR      SETFIL
008700000000     C*
008800000000     C* CICLO FINO A FINE FILE
008900000000DO  1C     $FINE         DOWEQ     '0'
009000000000     C*
009100000000     C* OPERAZIONI PER NUOVA LINEA
009200000000     C                   EXSR      NEWLIN
009300000000     C*
009400000000     C* CICLO FINO A ROTTURA LINEA
009500000000DO  2C     $FINE         DOWEQ     '0'
009600000000     C     TSMTFP        ANDEQ     DEPTFP
009700000000     C     TSMTFF        ANDEQ     DEPTFF
009800000000     C*
009900000000     C* OPERAZIONI PER NUOVO MESE
010000000000     C                   EXSR      NEWMES
010100000000     C*
010200000000     C* CICLO FINO A ROTTURA PERIODO ANNO/MESE
010300000000DO  3C     $FINE         DOWEQ     '0'
010400000000     C     TSMTFP        ANDEQ     DEPTFP
010500000000     C     TSMTFF        ANDEQ     DEPTFF
010600000000     C     TSMANN        ANDEQ     DEPANN
010700000000     C     TSMMES        ANDEQ     DEPMES
010800000000     C*
010900000000     C* OPERAZIONI PER NUOVO GIORNO
011000000000     C                   EXSR      NEWGIO
011100000000     C*
011200000000     C* CICLO FINO A ROTTURA GIORNO
011300000000DO  4C     $FINE         DOWEQ     '0'
011400000000     C     TSMTFP        ANDEQ     DEPTFP
011500000000     C     TSMTFF        ANDEQ     DEPTFF
011600000000     C     TSMANN        ANDEQ     DEPANN
011700000000     C     TSMMES        ANDEQ     DEPMES
011800000000     C     TSMGIO        ANDEQ     DEPGIO
011900000000     C*
012000000000     C* MEMORIZZA I DATI DEI METRICUBI
012100000000     C                   EXSR      MEMTSM
012200000000     C*
012300000000     C* LEGGE IL RECORD SUCCESSIVO
012400000000     C                   EXSR      LETFIL
012500000000E   4C                   ENDDO                                                  *ROTTURA GIORNO
012600000000     C*
012700000000     C* MEMORIZZA I DATI DLE TRAINO
012800000000     C                   EXSR      MEMTST
012900000000E   3C                   ENDDO                                                  *ROTTURA MESE
013000000000     C*
013100000000     C* SCRIVE IL FILE DI LAVORO
013200000000     C                   EXSR      AGGWFR
013300000000E   2C                   ENDDO                                                  *ROTTURA LINEA
013400000000E   1C                   ENDDO                                                  *FINE FILE
013500000000     C*
013600000000     C                   SETON                                        LR
013700000000     C*------------------------------------------------------------------------*
013800000000     C* SETFIL - POSIZIONAMENTO E PRIMA LETTURA
013900000000     C*------------------------------------------------------------------------*
014000000000     C     SETFIL        BEGSR
014100000000     C*
014200000000     C     *LOVAL        SETLL     TNTSM04L                           99        *FINE ARCHIVIO
014300000000     C*
014400000000IF  1C     *IN99         IFEQ      *ON
014500000000     C                   MOVEL     '1'           $FINE
014600000000X   1C                   ELSE
014700000000     C                   EXSR      LETFIL                                       *LETTURA RECORD
014800000000E   1C                   ENDIF
014900000000     C*
015000000000     C                   ENDSR
015100000000     C*------------------------------------------------------------------------*
015200000000     C* LETFIL - LETTURA PROSSIMO RECORD
015300000000     C*------------------------------------------------------------------------*
015400000000     C     LETFIL        BEGSR
015500000000     C*
015600000000     C* LEGGE FINO A:
015700000000     C*     - FINE FILE
015800000000     C*     - TROVATO RECORD VALIDO
015900000000     C                   MOVEL     'N'           WRECOK
016000000000DO  1C     $FINE         DOWEQ     '0'
016100000000     C     WRECOK        ANDEQ     'N'
016200000000     C*
016300000000     C                   READ      TNTSM04L                               99
016400000000     C*
016500000000IF  2C     *IN99         IFEQ      *ON                                          *FINE ARCHIVIO
016600000000     C                   MOVEL     '1'           $FINE
016700000000X   2C                   ELSE
016800000000     C                   EXSR      CHKREC                                       *CONTROLLO RECORD
016900000000E   2C                   ENDIF
017000000000E   1C                   ENDDO
017100000000     C*
017200000000     C                   ENDSR
017300000000     C*------------------------------------------------------------------------*
017400000000     C* CHKREC - CONTROLLA VALIDITA' RECORD
017500000000     C*------------------------------------------------------------------------*
017600000000     C     CHKREC        BEGSR
017700000000     C*
017800000000     C                   MOVEL     'S'           WRECOK            1            *RECORD VALIDO
017900960927     C*
018000960927     C* CREA IL WORKFILE DAL PRIMO MESE INTERO DISPONIBILE IN GRIGLIA
018100960927     C* FINO ALLA DATA FINALE RICHIESTA A VIDEO
018200960927IF  1C     TSMDAT        IFGE      WDICRE                                       *> DATI IN GRIGLIA
018300990604     C     TSMDAT        ANDLE     PARDA2                                       *< DATA FINALE RICH.
018400960927X   1C                   ELSE
018500960927     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
018600960927E   1C                   ENDIF
018700960614     C*
018800960926     C* IL PERIODO PRIMA DI GIUGNO 96 E' ERRATO, QUINDI ESCLUDE
018900960926IF  1C     TSMDAT        IFLT      199606
019000960614     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
019100960614E   1C                   ENDIF
019200000000     C*
019300000000     C* METRICUBI ANNULLATI, ESCLUDE
019400000000IF  1C     TSMATB        IFNE      *BLANKS
019500000000     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
019600000000E   1C                   ENDIF
019700000000     C*
019800000000     C* CONTROLLA LA VALIDITA' DEL TRAINO
019900000000IF  1C     WRECOK        IFEQ      'S'
020000000000     C                   EXSR      CHKTST
020100000000E   1C                   ENDIF
020200000000     C*
020300000000     C                   ENDSR
020400000000     C*------------------------------------------------------------------------*
020500000000     C* CHKTST - COTROLLA LA VALIDITA' DEL TRAINO
020600000000     C*------------------------------------------------------------------------*
020700000000     C     CHKTST        BEGSR
020800000000     C*
020900000000     C* LEGGE IL TRAINO CORRISPONDENTE
021000000000     C                   Z-ADD     TSMTRN        KSTTRN                         NUMERO TRAINO
021100000000     C                   Z-ADD     TSMDTN        KSTDTN                         DATA TRAINO
021200000000     C     KEYTST        CHAIN     TNTST01L                           98
021300000000     C*
021400000000     C* TRANO NON TROVATO, ESCLUDE
021500000000IF  1C     *IN98         IFEQ      *ON
021600000000     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
021700000000X   1C                   ELSE
021800000000     C*
021900000000     C* TRAINO ANNULLATO, ESCLUDE
022000000000IF  2C     TSTATB        IFNE      *BLANKS
022100000000     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
022200000000E   2C                   ENDIF
022300000000     C*
022400000000     C* TRAINO NON CONFERMATO, ESCLUDE
022500000000IF  2C     TSTCNF        IFEQ      *BLANKS
022600000000     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
022700000000E   2C                   ENDIF
022800000000     C*
022900000000     C* TRAINO -SOLO ANDATA- O -CHIUSO-, ALTRIMENTI ESCLUDE
023000000000IF  2C     TSTTTR        IFEQ      'C'
023100000000     C     TSTTTR        OREQ      'S'
023200000000X   2C                   ELSE
023300000000     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
023400000000E   2C                   ENDIF
023500000000     C*
023600000000     C* CONTROLLA SE' PARTITO ALMENO UN TRAINO NORMALE
023700000000IF  2C     WRECOK        IFEQ      'S'
023800000000IF  3C     TSTBIS        IFEQ      *BLANKS
023900000000     C                   MOVEL     '1'           $NOR
024000000000E   3C                   ENDIF
024100000000E   2C                   ENDIF
024200000000E   1C                   ENDIF
024300000000     C*
024400000000     C                   ENDSR
024500000000     C*------------------------------------------------------------------------*
024600000000     C* MEMTSM - MEMORIZZA I DATI DEI METRICUBI
024700000000     C*------------------------------------------------------------------------*
024800000000     C     MEMTSM        BEGSR
024900000000     C*
025000000000     C* DEFINISCE LA DECADE DEL MESE
025100000000IF  1C     DEPGIO        IFGT      0
025200000000     C     DEPGIO        ANDLE     10
025300000000     C                   Z-ADD     1             DEPDEC            1 0          *PRIMA DECADE
025400000000E   1C                   ENDIF
025500000000IF  1C     DEPGIO        IFGT      10
025600000000     C     DEPGIO        ANDLE     20
025700000000     C                   Z-ADD     2             DEPDEC                         *SECONDA DECADE
025800000000E   1C                   ENDIF
025900000000IF  1C     DEPGIO        IFGT      20
026000000000     C                   Z-ADD     3             DEPDEC                         *TERZA DECADE
026100000000E   1C                   ENDIF
026200000000     C                   Z-ADD     DEPDEC        D                 1 0
026300000000     C***
026400000000     C* MCUBI ASSEGNATI NORMALI
026500000000     C***
026600000000     C                   ADD       TSMMCA        LMAN(D)
026700000000     C***
026800000000     C* MCUBI ASSEGNATI BIS
026900000000     C***
027000000000     C                   ADD       TSMMCB        LMAB(D)
027100000000     C***
027200000000     C* MCUBI ASSEGNATI SEDE
027300000000     C***
027400000000     C                   ADD       TSMMCT        LMAS(D)
027500000000     C*
027600000000     C                   ENDSR
027700000000     C*------------------------------------------------------------------------*
027800000000     C* MEMTST - MEMORIZZA I DATI DEL TRAINO
027900000000     C*------------------------------------------------------------------------*
028000000000     C     MEMTST        BEGSR
028100000000     C***
028200000000     C* GIORNI PARTENZE NORMALI
028300000000     C***
028400000000     C                   Z-ADD     DEPDEC        D
028500000000IF  1C     $NOR          IFEQ      '1'
028600000000     C                   ADD       1             LGPN(D)
028700000000E   1C                   ENDIF
028800000000     C*
028900000000     C                   ENDSR
029000000000     C*------------------------------------------------------------------------*
029100000000     C* AGGWFR - SCRIVE IL FILE DI LAVORO
029200000000     C*------------------------------------------------------------------------*
029300000000     C     AGGWFR        BEGSR
029400000000     C*
029500000000     C* PER CIASCUNA DECADE DEPOSITATA
029600000000DO  1C     1             DO        3             D
029700000000IF  2C     LGPN(D)       IFGT      *ZEROS                                       *GG PARTENZE NORMALI
029800000000     C     LMAN(D)       ORGT      *ZEROS                                       *MC ASSEGNATI NORMAL
029900000000     C     LMAB(D)       ORGT      *ZEROS                                       *MC ASSEGNATI BIS
030000000000     C     LMAS(D)       ORGT      *ZEROS                                       *MC ASSEGNATI SEDE
030100000000     C*
030200000000     C* LEGGE IL WORKFILE PER VERIFICARE L'ESISTENZA DEL RECORD DA SCRIVERE
030300000000     C                   Z-ADD     DEPTFP        KWRTFP                         *FILIALE PARTENZA
030400000000     C                   Z-ADD     DEPTFF        KWRTFA                         *FILIALE ARRIVO
030500000000     C                   Z-ADD     DEPANN        KWRANN                         *ANNO
030600000000     C                   Z-ADD     DEPMES        KWRMES                         *MESE
030700000000     C                   MOVEL     D             KWRDEC                         *DECADE
030800000000     C     KEYTRA        CHAIN     WFTRA01L                           98
030900000000     C* INSERIMENTO
031000000000IF  3C     *IN98         IFEQ      *ON
031100000000     C                   Z-ADD     DEPTFP        WFRTFP                         *FILIALE PARTENZA
031200000000     C                   Z-ADD     DEPTFF        WFRTFA                         *FILIALE ARRIVO
031300000000     C                   Z-ADD     DEPANN        WFRANN                         *ANNO
031400000000     C                   Z-ADD     DEPMES        WFRMES                         *MESE
031500000000     C                   MOVEL     D             WFRDEC                         *DECADE
031600000000     C                   Z-ADD     LGPN(D)       WFRGPN                         *GG PARTENZE NORMALI
031700000000     C                   Z-ADD     LMAN(D)       WFRMAN                         *MC ASSEGNATI NORMAL
031800000000     C                   Z-ADD     LMAB(D)       WFRMAB                         *MC ASSEGNATI BIS
031900000000     C                   Z-ADD     LMAS(D)       WFRMAS                         *MC ASSEGNATI SEDE
032000000000     C                   WRITE     WFTRA000
032100000000     C* AGGIORNAMENTO
032200000000X   3C                   ELSE
032300000000     C                   ADD       LGPN(D)       WFRGPN                         *GG PARTENZE NORMALI
032400000000     C                   ADD       LMAN(D)       WFRMAN                         *MC ASSEGNATI NORMAL
032500000000     C                   ADD       LMAB(D)       WFRMAB                         *MC ASSEGNATI BIS
032600000000     C                   ADD       LMAS(D)       WFRMAS                         *MC ASSEGNATI SEDE
032700000000     C                   UPDATE    WFTRA000
032800000000E   3C                   ENDIF
032900000000E   2C                   ENDIF
033000000000E   1C                   ENDDO
033100000000     C*
033200000000     C                   ENDSR
033300000000     C*------------------------------------------------------------------------*
033400000000     C* NEWLIN - OPERAZIONI PER NUOVA LINEA
033500000000     C*------------------------------------------------------------------------*
033600000000     C     NEWLIN        BEGSR
033700000000     C*
033800000000     C* MEMORIZZA LA NUOVA LINEA IN UN DEPOSITO
033900000000     C                   Z-ADD     TSMTFP        DEPTFP                         FILIALE PARTENZA
034000000000     C                   Z-ADD     TSMTFF        DEPTFF                         FILIALE ARRIVO FINAL
034100000000     C*
034200000000     C                   ENDSR
034300000000     C*------------------------------------------------------------------------*
034400000000     C* NEWMES - OPERAZIONI PER NUOVO MESE
034500000000     C*------------------------------------------------------------------------*
034600000000     C     NEWMES        BEGSR
034700000000     C*
034800000000     C* MEMORIZZA IL NUOVO MESE IN UN DEPOSITO
034900000000     C                   Z-ADD     TSMANN        DEPANN            4 0          *ANNO
035000000000     C                   Z-ADD     TSMMES        DEPMES            2 0          *MESE
035100000000     C*
035200000000     C* PULISCE I CAMPI DI MEMORIZZAZIONE DEL MESE
035300000000     C                   CLEAR                   LGPN                           *GG PARTENZE NORMALI
035400000000     C                   CLEAR                   LMAN                           *MC ASSEGNAI NORMALI
035500000000     C                   CLEAR                   LMAB                           *MC ASSEGNATI BIS
035600000000     C                   CLEAR                   LMAS                           *MC ASSEGNATI SEDE
035700000000     C*
035800000000     C                   ENDSR
035900000000     C*------------------------------------------------------------------------*
036000000000     C* NEWGIO - OPERAZIONI PER NUOVO GIORNO (O GIORNO NUOVO)
036100000000     C*------------------------------------------------------------------------*
036200000000     C     NEWGIO        BEGSR
036300000000     C*
036400000000     C* MEMORIZZA IL NUOVO GIORNO IN UN DEPOSITO
036500000000     C                   Z-ADD     TSMGIO        DEPGIO            2 0
036600000000     C*
036700000000     C* REIMPOSTA IL FLAG DI PARTENZA TRAINI NORMALI
036800000000     C                   MOVEL     '0'           $NOR
036900000000     C*
037000000000     C                   ENDSR
037100000000     C*------------------------------------------------------------------------*
037200000000     C* CANWFR - CANCELLA IL WORKFILE
037300000000     C*------------------------------------------------------------------------*
037400000000     C     CANWFR        BEGSR
037500000000     C*
037600960927     C* CANCELLA CIO'CHE SI PUO' RICREARE DALLA GRIGLIA FINO ALLA DATA RICHIESTA
037700000000     C                   READ      WFTRA01L                               98
037800000000DO  1C     *IN98         DOWEQ     *OFF
037900960927IF  2C     WFRDAT        IFGE      WDICRE                                       *> DATI IN GRIGLIA
038000990604     C     WFRDAT        ANDLE     PARDA2                                       *< DATA FINALE RICH.
038100000000     C                   DELETE    WFTRA000
038200000000E   2C                   ENDIF
038300000000     C                   READ      WFTRA01L                               98
038400000000E   1C                   ENDDO
038500000000     C*
038600000000     C                   ENDSR
038700000000     C*------------------------------------------------------------------------*
038800000000     C* *INZSR - IMPOSTAZIONI INIZIALI
038900000000     C*------------------------------------------------------------------------*
039000000000     C     *INZSR        BEGSR
039100000000     C*--------------------
039200000000     C* RICEVIMENTO PARAMETRI
039300000000     C*--------------------
039400000000     C     *ENTRY        PLIST
039500000000     C                   PARM                    KPJBA
039600000000     C                   MOVEL     KPJBU         PARAM
039700000000     C*--------------------
039800000000     C* VARIABILI RIFERITE AL DATA BASE
039900000000     C*--------------------
040000000000     C     *LIKE         DEFINE    TSTTRN        KSTTRN                         *TNTST00F
040100000000     C     *LIKE         DEFINE    TSTDTN        KSTDTN
040200000000     C     *LIKE         DEFINE    WFRTFP        KWRTFP                         *WFTRA00F
040300000000     C     *LIKE         DEFINE    WFRTFA        KWRTFA
040400000000     C     *LIKE         DEFINE    WFRANN        KWRANN
040500000000     C     *LIKE         DEFINE    WFRMES        KWRMES
040600000000     C     *LIKE         DEFINE    WFRDEC        KWRDEC
040700000000     C     *LIKE         DEFINE    TSMTFP        DEPTFP                         *DEPOSITI
040800000000     C     *LIKE         DEFINE    TSMTFF        DEPTFF
040900000000     C*--------------------
041000000000     C* CHIAVI DI LETTURA
041100000000     C*--------------------
041200000000     C* LETTURA TNTST01L - COMPLETA
041300000000     C     KEYTST        KLIST
041400000000     C                   KFLD                    KSTTRN                         *TRAINO
041500000000     C                   KFLD                    KSTDTN                         *DATA TRAINO
041600000000     C* LETTURA WFTRA01L - COMPLETA
041700000000     C     KEYTRA        KLIST
041800000000     C                   KFLD                    KWRTFP                         *FILIALE DI PARTENZA
041900000000     C                   KFLD                    KWRTFA                         *FILIALE DI ARRIVO
042000000000     C                   KFLD                    KWRANN                         *ANNO
042100000000     C                   KFLD                    KWRMES                         *MESE
042200000000     C                   KFLD                    KWRDEC                         *DECADE
042300000000     C* LETTURA WFTRA03L - COMPLETA
042400000000     C     KE3TRA        KLIST
042500000000     C                   KFLD                    KWRANN                         *ANNO
042600000000     C                   KFLD                    KWRMES                         *MESE
042700000000     C*--------------------
042800000000     C* VARIABILI DI CONTROLLO
042900000000     C*--------------------
043000000000     C                   MOVEL     '0'           $FINE             1            *FINE PGM
043100000000     C                   MOVEL     '0'           $NOR              1            *PARTENZA TRAINI NOR
043200960926     C*
043300960926     C* REPERISCE LA DATA DELL'ULTIMO TRAINO CANCELLATO IN GRIGLIA
043400960927     C     *DTAARA       DEFINE                  TNTL58            8 0
043500960926     C                   IN        TNTL58
043600960926     C*
043700960926     C* CALCOLA IL MESE SUCCESSIVO DALLA DATA X CREARE IL WORKFILE (MESE INTERO)
043800960926     C                   Z-ADD     *ZEROS        WDICRE            6 0
043900960926IF  1C     TNTL58        IFGT      *ZEROS
044000960926     C                   MOVEL     TNTL58        $DATA8
044100960926     C     $DAMES        ADD       1             $DAMES
044200960926IF  2C     $DAMES        IFEQ      13
044300960926     C                   Z-ADD     01            $DAMES
044400960926     C     $DAAES        ADD       1             $DAAES
044500960926E   2C                   ENDIF
044600960927     C                   MOVEL     $DATA8        WDICRE                         DATA INIZIO CREAZION
044700960926X   1C                   ELSE
044800960926     C                   MOVEL     '1'           $FINE                          *FINE PGM
044900960926E   1C                   ENDIF
045000000000     C*
045100000000     C* CANCELLA IL WORKFILE
045200000000     C                   EXSR      CANWFR
045300000000     C*
045400000000     C                   ENDSR
