000100960927     H*------------------------------------------------------------------------*
000200960927     H* STATISTICA RITARDI TRAINI - CREA WORKFILE
000300960927     H*------------------------------------------------------------------------*
000400960927     H DECEDIT('0,') DATEDIT(*DMY.)
000500960927     F*------------------------------------------------------------------------*
000600960927     F* DATA BASE
000700960927     F*------------------------------------------------------------------------*
000800960927     FAZORG01L  IF   E           K DISK
000900960927     FTABEL00F  IF   E           K DISK
001000960927     F*------------
001100960927     F* GRIGLIA
001200960927     F*------------
001300960927     FTNTST04L  IF   E           K DISK
001400960927     FTNTSR01L  IF   E           K DISK
001500960927     F*------------
001600960927     F* WORK FILE
001700960927     F*------------
001800960927     FWFRIT01L  UF A E           K DISK
001900960927     D*------------------------------------------------------------------------*
002000960927     D* SCHIERE
002100960927     D*------------------------------------------------------------------------*
002200960927     D*------------------
002300960927     D* ORGANIGRAMMA
002400960927     D*------------------
002500971124     D ORG             S              3  0 DIM(500)                             *FILIALE
002600971124     D ARE             S              3  0 DIM(500)                             *AREA
002700971124     D DIV             S              1    DIM(500)                             *DIVISIONE
002800960927     D*------------------
002900960927     D* CAUSALI RITARDO TRAINI
003000960927     D*------------------
003100960927     D CAU             S              1    DIM(300)                             *CODICE
003200960927     D IMP             S              1    DIM(300)                             *IMPUTAZIONE RITARDO
003300960927     D FST             S              1    DIM(300)                             *FLAG STAMPA
003400960927     D*------------------
003500960927     D* CAUSALI RITARDI
003600960927     D*------------------
003700960927     D $TCR            S              1    DIM(20)                              *TIPI CAUSALI RITARD
003800960927     D $IMP            S              1    DIM(20)                              *IMPUTAZIONE RITARDO
003900960927     D*------------------------------------------------------------------------*
004000960927     D* INPUT
004100960927     D*------------------------------------------------------------------------*
004200960927     D*------------------
004300960927     D* ARCHITETTURA
004400960927     D*------------------
004500960927     D KPJBA         E DS
004600960927     D*------------------
004700960927     D* DS PARAMETRI LANCIO
004800960927     D*------------------
004900960927     D PARAM           DS
005000990608     D  PARDAT                 1      6  0
005100990608     D  PARDA2                 7     12  0
005200990608     D  PARCRE                13     13
005300000706     D  PARPOS                14     14
005400000706     D  PARTAD                15     15
005500960927     D*------------------
005600960927     D* DS SCOMPOSIZIONE DATA TRAINO
005700960927     D*------------------
005800960927     D                 DS
005900960927     D  TSTANN                 1      4  0
006000960927     D  TSTMES                 5      6  0
006100990608     D  TSTDAT                 1      6  0
006200960927     D  TSTGIO                 7      8  0
006300960927     D  TSTDTN                 1      8  0
006400960927     D*------------------
006500960927     D* DS COMPOSIZIONE DATA WORKFILE
006600960927     D*------------------
006700960927     D                 DS
006800960927     D  WFLANN                 1      4  0
006900960927     D  WFLMES                 5      6  0
007000960927     D  WFLDAT                 1      6  0
007100960927     D*------------------
007200960927     D* DS SCOMPOSIZIONE DATA GENERICA
007300960927     D*------------------
007400960927     D                 DS
007500960927     D  $DASEC                 1      2  0
007600960927     D  $DAANN                 3      4  0
007700960927     D  $DAAES                 1      4  0
007800960927     D  $DAMES                 5      6  0
007900960927     D  $DAGIO                 7      8  0
008000960927     D  $DATA8                 1      8  0
008100960927     D*------------------
008200960927     D* DS CAUSALI RITARDO TRAINI
008300960927     D*------------------
008400960927     D DSTC          E DS
008500960927     C*------------------------------------------------------------------------*
008600960927     C* MAIN LINE
008700960927     C*------------------------------------------------------------------------*
008800960927     C*
008900960927     C* POSIZIONAMENTO SUL FILE E PRIMA LETTURA
009000960927     C                   EXSR      SETFIL
009100960927     C*
009200960927     C* CICLO FINO A FINE FILE
009300960927DO  1C     $FINE         DOWEQ     '0'
009400960927     C*
009500960927     C* SCRIVE IL FILE DI LAVORO
009600960927     C                   EXSR      AGGWFL
009700960927     C*
009800960927     C* LEGGE IL RECORD SUCCESSIVO
009900960927     C                   EXSR      LETFIL
010000960927E   1C                   ENDDO
010100960927     C*
010200960927     C                   SETON                                        LR
010300960927     C*------------------------------------------------------------------------*
010400960927     C* SETFIL - POSIZIONAMENTO E PRIMA LETTURA
010500960927     C*------------------------------------------------------------------------*
010600960927     C     SETFIL        BEGSR
010700960927     C*
010800960927     C     *LOVAL        SETLL     TNTST04L                           99        *FINE ARCHIVIO
010900960927IF  1C     *IN99         IFEQ      *ON
011000960927     C                   MOVEL     '1'           $FINE
011100960927X   1C                   ELSE
011200960927     C                   EXSR      LETFIL                                       *LETTURA RECORD
011300960927E   1C                   ENDIF
011400960927     C*
011500960927     C                   ENDSR
011600960927     C*------------------------------------------------------------------------*
011700960927     C* LETFIL - LETTURA PROSSIMO RECORD
011800960927     C*------------------------------------------------------------------------*
011900960927     C     LETFIL        BEGSR
012000960927     C*
012100960927     C* LEGGE FINO A:
012200960927     C*     - FINE FILE
012300960927     C*     - TROVATO RECORD VALIDO
012400960927     C                   MOVEL     'N'           WRECOK
012500960927DO  1C     $FINE         DOWEQ     '0'
012600960927     C     WRECOK        ANDEQ     'N'
012700960927     C*
012800960927     C                   READ      TNTST04L                               99
012900960927     C*
013000960927IF  2C     *IN99         IFEQ      *ON                                          *FINE ARCHIVIO
013100960927     C                   MOVEL     '1'           $FINE
013200960927X   2C                   ELSE
013300960927     C                   EXSR      CHKREC                                       *CONTROLLO RECORD
013400960927E   2C                   ENDIF
013500960927E   1C                   ENDDO
013600960927     C*
013700960927     C                   ENDSR
013800960927     C*------------------------------------------------------------------------*
013900960927     C* CHKREC - CONTROLLA VALIDITA' RECORD
014000960927     C*------------------------------------------------------------------------*
014100960927     C     CHKREC        BEGSR
014200960927     C*
014300960927     C                   MOVEL     'S'           WRECOK                         *RECORD VALIDO
014400960927     C*
014500960927     C* CREA IL WORKFILE DAL PRIMO MESE INTERO DISPONIBILE IN GRIGLIA
014600960927     C* FINO ALLA DATA FINALE RICHIESTA A VIDEO
014700960927IF  1C     TSTDAT        IFGE      WDICRE                                       *> DATI IN GRIGLIA
014800990608     C     TSTDAT        ANDLE     PARDA2                                       *< DATA FINALE RICH.
014900960927X   1C                   ELSE
015000960927     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
015100960927E   1C                   ENDIF
015200960927     C*
015300960927     C* IL PERIODO PRIMA DI GIUGNO 96 E' ERRATO, QUINDI ESCLUDE
015400960927IF  1C     TSTDAT        IFLT      199606
015500960927     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
015600960927E   1C                   ENDIF
015700960927     C*
015800960927     C* TRAINO ANNULLATO, ESCLUDE
015900960927IF  1C     TSTATB        IFNE      *BLANKS
016000960927     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
016100960927E   1C                   ENDIF
016200960927     C*
016300960927     C* TRAINO NON CONFERMATO, ESCLUDE
016400960927IF  1C     TSTCNF        IFEQ      *BLANKS
016500960927     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
016600960927E   1C                   ENDIF
016700960927     C***
016800960927     C* DETERMINA LA DECADE
016900960927     C***
017000960927IF  1C     TSTGIO        IFGE      1
017100960927     C     TSTGIO        ANDLE     10
017200960927     C                   MOVEL     '1'           TSTDEC            1            *PRIMA DECADE
017300960927X   1C                   ELSE
017400960927IF  2C     TSTGIO        IFGE      11
017500960927     C     TSTGIO        ANDLE     20
017600960927     C                   MOVEL     '2'           TSTDEC            1            *SECONDA DECADE
017700960927X   2C                   ELSE
017800960927     C                   MOVEL     '3'           TSTDEC            1            *TERZA DECADE
017900960927E   2C                   ENDIF
018000960927E   1C                   ENDIF
018100960927     C*
018200960927     C                   ENDSR
018300960927     C*------------------------------------------------------------------------*
018400960927     C* AGGWFL - AGGIORNA IL FILE DI LAVORO
018500960927     C*------------------------------------------------------------------------*
018600960927     C     AGGWFL        BEGSR
018700960927     C*
018800960927     C* REPERISCE I RITARDI PARTENZA E ARRIVO DEL TRAINO
018900960927     C                   CLEAR                   $TCR                           *TIPI CAUSALI RITARD
019000960927     C                   CLEAR                   $IMP                           *IMPUTAZIONE  RITARD
019100960927     C                   Z-ADD     *ZEROS        C                 4 0
019200960927     C                   EXSR      REPRIT
019300960927     C*
019400960927     C* REPERISCE LA DIVISIONE DELLA FILIALE DI PARTENZA
019500960927     C                   SETOFF                                           28
019600960927     C                   Z-ADD     1             I
019700960927     C     TSTTFP        LOOKUP    ORG(I)                                 28
019800960927IF  1C     *IN28         IFEQ      *ON
019900960927     C*
020000960927     C* LEGGE IL WORKFILE PER VERIFICARE L'ESISTENZA DEL RECORD DA SCRIVERE
020100960927     C                   MOVEL     DIV(I)        KWLDIV
020200960927     C                   Z-ADD     ARE(I)        KWLARE
020300960927     C                   Z-ADD     TSTTFP        KWLTFP
020400960927     C                   Z-ADD     TSTANN        KWLANN
020500960927     C                   Z-ADD     TSTMES        KWLMES
020600960927     C                   MOVEL     TSTDEC        KWLDEC
020700960927     C     KEYWFL        CHAIN     WFRIT01L                           98
020800960927IF  2C     *IN98         IFEQ      *ON                                          *IMMISSIONE
020900960927     C                   CLEAR                   WFRIT000                       *CANCELLA RECORD
021000960927E   2C                   ENDIF
021100960927     C* IMPOSTA I CAMPI
021200960927     C                   ADD       1             WFLPAR                         *TOTALE PARTENZE
021300960927DO  2C     1             DO        20            C
021400960927IF  3C     $TCR(C)       IFNE      *BLANKS
021500960927     C                   ADD       1             WFLRIT                         *TOTALE RITARDI
021600960927IF  4C     $IMP(C)       IFEQ      'P'
021700990817     C                   ADD       1             WFLFIL                         *COLPA FIL/ARR RITAR
021800960927E   4C                   ENDIF
021900960927IF  4C     $IMP(C)       IFEQ      'A'
022000960927     C                   ADD       1             WFLAUT                         *COLPA AUTISTA
022100960927E   4C                   ENDIF
022200960927IF  4C     $IMP(C)       IFEQ      'F'
022300960927     C                   ADD       1             WFLCFM                         *CAUSA FORZA MAGGIOR
022400960927E   4C                   ENDIF
022500960927IF  4C     $IMP(C)       IFEQ      'S'
022600960927     C                   ADD       1             WFLSED                         *TRINI SEDE
022700960927E   4C                   ENDIF
022800960927IF  4C     $IMP(C)       IFEQ      'T'
022900960927     C                   ADD       1             WFLTRS                         *TRANSITO
023000960927E   4C                   ENDIF
023100990817IF  4C     $IMP(C)       IFEQ      'O'
023200990817     C                   ADD       1             WFLFIO                         *COLPA FIL/ARR ORARI
023300990817E   4C                   ENDIF
023400960927E   3C                   ENDIF
023500960927E   2C                   ENDDO                                                  *FINE RITARDI
023600960927     C* IMMISSIONE
023700960927IF  2C     *IN98         IFEQ      *ON
023800960927     C                   MOVEL     KWLDIV        WFLDIV
023900960927     C                   Z-ADD     KWLARE        WFLARE
024000960927     C                   Z-ADD     TSTTFP        WFLTFP
024100960927     C                   Z-ADD     TSTANN        WFLANN
024200960927     C                   Z-ADD     TSTMES        WFLMES
024300960927     C                   MOVEL     TSTDEC        WFLDEC
024400960927     C                   WRITE     WFRIT000
024500960927     C* AGGIORNAMENTO
024600960927X   2C                   ELSE
024700960927     C                   UPDATE    WFRIT000
024800960927E   2C                   ENDIF
024900960927E   1C                   ENDIF
025000960927     C*
025100960927     C                   ENDSR
025200960927     C*------------------------------------------------------------------------*
025300960927     C* REPRIT - REPERISCE I RITARDI PARTENZA E ARRIVO DEL TRAINO
025400960927     C*------------------------------------------------------------------------*
025500960927     C     REPRIT        BEGSR
025600960927     C*---
025700960927     C* SE PER IL TRAINO ESISTONO + CAUSALI CON = IMPUTAZIONE, NE CONTEGGIA UNA
025800960927     C*---
025900960927     C                   Z-ADD     TSTTRN        KSRTRN                         *NUMERO TRAINO
026000960927     C                   Z-ADD     TSTDTN        KSRDTN                         *DATA TRAINO
026100960927     C     KEYTSR        SETLL     TNTSR01L
026200960927     C     KEYTSR        READE     TNTSR01L                               98
026300960927DO  1C     *IN98         DOWEQ     *OFF
026400960927IF  2C     TSRFFP        IFEQ      TSTTFP                                       *FILIALE PARTENZA
026500960927     C*  A R R I V O
026600960927IF  3C     TSRRTA        IFNE      *BLANKS                                      *IN RITARDO ARRIVO
026700960927     C*
026800960927     C* REPERISCE L'IMPUTAZIONE RITARDO ARRIVO
026900960927     C                   Z-ADD     1             I
027000960927     C                   SETOFF                                           28
027100960927     C                   MOVEL     *BLANKS       DEPIMP            1
027200960927     C                   MOVEL     *BLANKS       DEPFST            1
027300960927     C     TSRRTA        LOOKUP    CAU(I)                                 28
027400960927IF  4C     *IN28         IFEQ      *ON                                          *TROVATO
027500960927     C                   MOVEL     IMP(I)        DEPIMP                         *DEPOSITO IMPUTAZION
027600960927     C                   MOVEL     FST(I)        DEPFST                         *DEPOSITO FLAG STAMP
027700960927E   4C                   ENDIF
027800960927     C*
027900960927     C* CONTROLLA CHE LA CAUSALE RITARDO SIA DA STAMPARE
028000960927     C* CONTROLLA CHE NON SIA GIA'MEMORIZZATA UNA CAUSALE CON = IMPUTAZIONE
028100960927IF  4C     DEPFST        IFNE      'N'
028200960927     C                   SETOFF                                           28
028300960927     C     DEPIMP        LOOKUP    $IMP                                   28
028400960927IF  5C     *IN28         IFEQ      *OFF                                         *NON TROVATO
028500960927     C                   ADD       1             C
028600960927     C                   MOVEL     TSRRTA        $TCR(C)                        TIPO RITARDO ARRIVO
028700960927     C                   MOVEL     DEPIMP        $IMP(C)                        IMPUTAZIONE RITARDO
028800960927E   5C                   ENDIF
028900960927E   4C                   ENDIF
029000960927     C*  P A R T E N Z A
029100960927X   3C                   ELSE
029200960927     C*
029300960927     C* REPERISCE L'IMPUTAZIONE RITARDO PARTENZA
029400960927IF  4C     TSRRTP        IFNE      *BLANKS                                      *IN RITARDO PARTENZA
029500960927     C                   Z-ADD     1             I
029600960927     C                   SETOFF                                           28
029700960927     C                   MOVEL     *BLANKS       DEPIMP
029800960927     C                   MOVEL     *BLANKS       DEPFST
029900960927     C     TSRRTP        LOOKUP    CAU(I)                                 28
030000960927IF  5C     *IN28         IFEQ      *ON                                          *TROVATO
030100960927     C                   MOVEL     IMP(I)        DEPIMP                         *DEPOSITO IMPUTAZION
030200960927     C                   MOVEL     FST(I)        DEPFST                         *DEPOSITO FLAG STAMP
030300960927E   5C                   ENDIF
030400960927     C*
030500960927     C* CONTROLLA CHE LA CAUSALE RITARDO SIA DA STAMPARE
030600960927     C* CONTROLLA CHE NON SIA GIA'MEMORIZZATA UNA CAUSALE CON = IMPUTAZIONE
030700960927IF  5C     DEPFST        IFNE      'N'
030800960927     C                   SETOFF                                           28
030900960927     C     DEPIMP        LOOKUP    $IMP                                   28
031000960927IF  6C     *IN28         IFEQ      *OFF                                         *NON TROVATO
031100960927     C                   ADD       1             C
031200960927     C                   MOVEL     TSRRTP        $TCR(C)                        TIPO RITARDO PARTENZ
031300960927     C                   MOVEL     DEPIMP        $IMP(C)                        IMPUTAZIONE RITARDO
031400960927E   6C                   ENDIF
031500960927E   5C                   ENDIF
031600960927E   4C                   ENDIF
031700960927E   3C                   ENDIF
031800960927E   2C                   ENDIF
031900960927     C*
032000960927     C     KEYTSR        READE     TNTSR01L                               98
032100960927E   1C                   ENDDO                                                  *FINE TRATTE
032200960927     C*
032300960927     C                   ENDSR
032400960927     C*------------------------------------------------------------------------*
032500960927     C* CANWFL - CANCELLA IL FILE DI LAVORO
032600960927     C*------------------------------------------------------------------------*
032700960927     C     CANWFL        BEGSR
032800960927     C*
032900960927     C* CANCELLA CIO'CHE SI PUO' RICREARE DALLA GRIGLIA FINO ALLA DATA RICHIESTA
033000960927     C                   READ      WFRIT01L                               98
033100960927DO  1C     *IN98         DOWEQ     *OFF
033200960927IF  2C     WFLDAT        IFGE      WDICRE                                       *> DATI IN GRIGLIA
033300990608     C     WFLDAT        ANDLE     PARDA2                                       *< DATA FINALE RICH.
033400960927     C                   DELETE    WFRIT000
033500960927E   2C                   ENDIF
033600960927     C                   READ      WFRIT01L                               98
033700960927E   1C                   ENDDO
033800960927     C*
033900960927     C                   ENDSR
034000960927     C*------------------------------------------------------------------------*
034100960927     C* CARTAB - CARICA LE TABELLE
034200960927     C*------------------------------------------------------------------------*
034300960927     C     CARTAB        BEGSR
034400960927     C*--------------------
034500960927     C* FILIALI
034600960927     C*--------------------
034700960927     C                   Z-ADD     *ZEROS        I                 4 0
034800960927     C                   READ      AZORG01L                               99
034900960927DO  1C     *IN99         DOWEQ     *OFF
035000960927IF  2C     ORGFVA        IFEQ      *BLANKS
035100960927     C     ORGFAG        ANDNE     'V'
035200960927     C                   ADD       1             I
035300960927     C                   Z-ADD     ORGFIL        ORG(I)                         *CODICE
035400960927     C                   MOVEL     ORGCAR        ARE(I)                         *AREA
035500960927     C                   MOVEL     ORGFL3        DIV(I)                         *DIVISIONE
035600960927E   2C                   ENDIF
035700960927     C                   READ      AZORG01L                               99
035800960927E   1C                   ENDDO
035900960927     C*--------------------
036000960927     C* CAUSALI RITARDO TRAINI
036100960927     C*--------------------
036200960927     C                   Z-ADD     *ZEROS        I                 4 0
036300020906     C                   Z-ADD     1             KTBKUT
036400960927     C                   MOVEL     'TC'          KTBCOD
036500960927     C     KE2TAB        SETLL     TABEL00F
036600960927     C     KE2TAB        READE     TABEL00F                               99
036700960927DO  1C     *IN99         DOWEQ     *OFF
036800960927IF  2C     TBLFLG        IFEQ      *BLANKS
036900960927     C                   MOVEL(P)  TBLUNI        DSTC
037000960927     C                   ADD       1             I
037100960927     C                   MOVEL     TBLKEY        CAU(I)                         *CODICE
037200960927     C                   MOVEL     §TCFST        FST(I)                         *FLAG STAMPA
037300960927     C                   MOVEL     §TCIMP        IMP(I)                         *IMPUTAZIONE RITARDO
037400960927E   2C                   ENDIF
037500960927     C     KE2TAB        READE     TABEL00F                               99
037600960927E   1C                   ENDDO
037700960927     C*
037800960927     C                   ENDSR
037900960927     C*------------------------------------------------------------------------*
038000960927     C* *INZSR - OPERAZIONI INIZIALI
038100960927     C*------------------------------------------------------------------------*
038200960927     C     *INZSR        BEGSR
038300960927     C*--------------------
038400960927     C* RICEVIMENTO PARAMETRI
038500960927     C*--------------------
038600960927     C     *ENTRY        PLIST
038700960927     C                   PARM                    KPJBA
038800960927     C                   MOVEL     KPJBU         PARAM
038900960927     C*--------------------
039000960927     C* VARIABILI RIFERITE AL DATA BASE
039100960927     C*--------------------
039200960927     C     *LIKE         DEFINE    TBLKUT        KTBKUT                         *TABEL00F
039300960927     C     *LIKE         DEFINE    TBLCOD        KTBCOD
039400960927     C     *LIKE         DEFINE    TSRTRN        KSRTRN                         *TNTSR00F
039500960927     C     *LIKE         DEFINE    TSRDTN        KSRDTN
039600960927     C     *LIKE         DEFINE    WFLDIV        KWLDIV                         *WFRIT00F
039700960927     C     *LIKE         DEFINE    WFLARE        KWLARE
039800960927     C     *LIKE         DEFINE    WFLTFP        KWLTFP
039900960927     C     *LIKE         DEFINE    WFLANN        KWLANN
040000960927     C     *LIKE         DEFINE    WFLMES        KWLMES
040100960927     C     *LIKE         DEFINE    WFLDEC        KWLDEC
040200960927     C*--------------------
040300960927     C* CHIAVI DI LETTURA
040400960927     C*--------------------
040500960927     C* LETTURA TABEL00F - PARZIALE
040600960927     C     KE2TAB        KLIST
040700960927     C                   KFLD                    KTBKUT                         *CODICE UTENTE
040800960927     C                   KFLD                    KTBCOD                         *CODICE TABELLA
040900960927     C* LETTURA TNTSR01L - PARZIALE
041000960927     C     KEYTSR        KLIST
041100960927     C                   KFLD                    KSRTRN                         *NUMERO TRAINO
041200960927     C                   KFLD                    KSRDTN                         *DATA TRAINO
041300960927     C* LETTURA WFRIT01L - COMPLETA
041400960927     C     KEYWFL        KLIST
041500960927     C                   KFLD                    KWLDIV                         *DIVISIONE
041600960927     C                   KFLD                    KWLARE                         *AREA
041700960927     C                   KFLD                    KWLTFP                         *FILIALE PARTENZA
041800960927     C                   KFLD                    KWLANN                         *ANNO
041900960927     C                   KFLD                    KWLMES                         *MESE
042000960927     C                   KFLD                    KWLDEC                         *DECADE
042100960927     C*--------------------
042200960927     C* VARIABILI DI CONTROLLO
042300960927     C*--------------------
042400960927     C                   MOVEL     '0'           $FINE             1            *FINE PGM
042500960927     C                   MOVEL     'N'           WRECOK            1            *VALIDITA' RECORD
042600960927     C*
042700960927     C* REPERISCE LA DATA DELL'ULTIMO TRAINO CANCELLATO IN GRIGLIA
042800960927     C     *DTAARA       DEFINE                  TNTL58            8 0
042900960927     C                   IN        TNTL58
043000960927     C*
043100960927     C* CALCOLA IL MESE SUCCESSIVO DALLA DATA X CREARE IL WORKFILE (MESE INTERO)
043200960927     C                   Z-ADD     *ZEROS        WDICRE            6 0
043300960927IF  1C     TNTL58        IFGT      *ZEROS
043400960927     C                   MOVEL     TNTL58        $DATA8
043500960927     C     $DAMES        ADD       1             $DAMES
043600960927IF  2C     $DAMES        IFEQ      13
043700960927     C                   Z-ADD     01            $DAMES
043800960927     C     $DAAES        ADD       1             $DAAES
043900960927E   2C                   ENDIF
044000960927     C                   MOVEL     $DATA8        WDICRE                         DATA INIZIO CREAZION
044100960927X   1C                   ELSE
044200960927     C                   MOVEL     '1'           $FINE                          *FINE PGM
044300960927E   1C                   ENDIF
044400960927     C*
044500960927     C* CARICA LE TABELLE
044600960927     C                   EXSR      CARTAB
044700960927     C*
044800960927     C* CANCELLA IL WORKFILE
044900960927     C                   EXSR      CANWFL
045000960927     C*
045100960927     C                   ENDSR
