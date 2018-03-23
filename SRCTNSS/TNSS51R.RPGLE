000100020111     H*------------------------------------------------------------------------*
000200020111     H* CANCELLAZIONE CONTO ECONOMICO DI SEDE - ESEGUE
000300020111     H*------------------------------------------------------------------------*
000400020111     H DECEDIT('0,') DATEDIT(*DMY.)
000500020111     F*------------------------------------------------------------------------*
000600020111     F* DATA BASE
000700020111     F*------------------------------------------------------------------------*
000800020111     F*---
000900020111     F* FILES DA SALVARE
001000020111     F*---
001100020111     FECMOV01L  UF   E           K DISK
001200020111     F*---
001300020111     F* FILES SALVATI -SALxx-
001400020111     F*---
001500020111     FECMOV00F  O  A E           K DISK
001600020111     F                                     RENAME(ECMOV000:ECMOVS)
001700020111     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
001800020111     D*------------------------------------------------------------------------*
001900020111     D* SCHIERE
002000020111     D*------------------------------------------------------------------------*
002100020111     D*---
002200020111     D* RIGHE DI STAMPA
002300020111     D*---
002400020111     D RIG             S             66    DIM(10) CTDATA PERRCD(1)
002500020111     D*------------------------------------------------------------------------*
002600020111     D* INPUT
002700020111     D*------------------------------------------------------------------------*
002800020111     D*---
002900020920     D* DS REPERIMENTO DATI UTENTE
003000020111     D*---
003100020920     D TIBS34DS      E DS                                                       *Profili utente
003200020920     D DDATIUTE      E DS                                                       *Dati utente
003300020920     D AZUTEDS       E DS                  extname(AZUTE00F)                    *Utenti
003400020111     D*---
003500020111     D* DS COMPOSIZIONE DATA -ECMOV-
003600020111     D*---
003700020111     D                 DS
003800020111     D  MOVDAT                 1      6  0
003900020111     D  MOVDRG                 1      8  0
004000020111     D*---
004100020111     D* DS PARAMETRI LANCIO
004200020111     D*---
004300020111     D PARAM           DS
004400020111     D  PARDAT                 1      6  0
004500020111     D  PARDA2                 7     12  0
004600020111     D*---
004700020111     D* ARCHITETTURA
004800020111     D*---
004900020111     D KPJBA         E DS
005000020111     C*------------------------------------------------------------------------*
005100020111     C* MAIN LINES
005200020111     C*------------------------------------------------------------------------*
005300020111     C*
005400020111     C* ELABORAZIONE PER ECMOV
005500020111     C                   EXSR      ELAMOV
005600020111     C*
005700020111     C* OPERAZIONI FINALI
005800020111     C                   EXSR      FINSR
005900020111     C*
006000020111     C                   SETON                                        LR
006100020111     C*------------------------------------------------------------------------*
006200020111     C* ELAMOV - ELABORAZIONE PER ECMOV
006300020111     C*------------------------------------------------------------------------*
006400020111     C     ELAMOV        BEGSR
006500020111     C*
006600020111     C* POSIZIONAMENTO E PRIMA LETTURA
006700020111     C                   EXSR      SETMOV
006800020111     C*
006900020111     C* CICLO FINO A FINE CICLO O FINE FILE
007000020111DO  1C     $FINE         DOWEQ     'N'
007100020111     C     $FMOV         ANDEQ     'N'
007200020111     C*
007300020111     C* SCRIVE / CANCELLA RECORD
007400020111     C                   EXSR      AGGMOV
007500020111     C*
007600020111     C* LETTURA SUCESSIVA
007700020111     C                   EXSR      LETMOV
007800020111     C                   SHTDN                                        98        *CHIUSURA CONTROLLAT
007900020111E   1C                   ENDDO                                                  *FINE FILE
008000020111     C*
008100020111     C                   MOVEL     'M'           WSDX
008200020111     C                   EXSR      STATOT
008300020111     C*
008400020111     C                   ENDSR
008500020111     C*------------------------------------------------------------------------*
008600020111     C* SETMOV - POSIZIONAMENTO E PRIMA LETTURA
008700020111     C*------------------------------------------------------------------------*
008800020111     C     SETMOV        BEGSR
008900020111     C*
009000020111     C* REIMPOSTA FINE PGM
009100020111     C                   MOVEL     'N'           $FINE                          *FINE PGM
009200020111     C*
009300020111     C     *LOVAL        SETLL     ECMOV01L                           99
009400020111IF  1C     *IN99         IFEQ      *ON                                          *FILE FILE
009500020111     C                   MOVEL     'S'           $FMOV                          *FINE CICLO
009600020111X   1C                   ELSE
009700020111     C                   EXSR      LETMOV                                       *LETTURA RECORD
009800020111E   1C                   ENDIF
009900020111     C*
010000020111     C                   ENDSR
010100020111     C*------------------------------------------------------------------------*
010200020111     C* LETMOV - LETTURA PROSSIMO RECORD
010300020111     C*------------------------------------------------------------------------*
010400020111     C     LETMOV        BEGSR
010500020111     C*
010600020111     C* LETTURA FINO A:
010700020111     C*  - FINE FILE
010800020111     C*  - TROVATO RECORD VALIDO
010900020111     C                   MOVEL     'N'           WRECOK
011000020111DO  1C     $FMOV         DOWEQ     'N'
011100020111     C     WRECOK        ANDEQ     'N'
011200020111     C                   READ      ECMOV01L                               99
011300020111IF  2C     *IN99         IFEQ      *ON                                          *FINE FILE
011400020111     C                   MOVEL     'S'           $FMOV                          *FINE CICLO
011500020111X   2C                   ELSE
011600020111     C                   EXSR      CHKMOV                                       *CONTROLLA RECORD
011700020111E   2C                   ENDIF
011800020111E   1C                   ENDDO
011900020111     C*
012000020111     C                   ENDSR
012100020111     C*------------------------------------------------------------------------*
012200020111     C* CHKMOV - CONTROLLA VALIDITA' RECORD
012300020111     C*------------------------------------------------------------------------*
012400020111     C     CHKMOV        BEGSR
012500020111     C*
012600020111     C                   MOVEL     'S'           WRECOK                         *VALIDITA' RECORD
012700020111     C*
012800020111     C* PERIODO <> QUELLO RICHIESTO, ESCLUDE
012900020111IF  1C     MOVDAT        IFGE      PARDAT
013000020111     C     MOVDAT        ANDLE     PARDA2
013100020111X   1C                   ELSE
013200020111     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
013300020111E   1C                   ENDIF
013400020111     C*
013500020111     C                   ENDSR
013600020111     C*------------------------------------------------------------------------*
013700020111     C* AGGMOV - SCRIVE / CANCELLA RECORD
013800020111     C*------------------------------------------------------------------------*
013900020111     C     AGGMOV        BEGSR
014000020111     C*
014100020111     C* SCRIVE IL RECORD SUL FILE DA SALVARE
014200020111     C                   WRITE     ECMOVS                               98
014300020111     C*
014400020111     C* NO ERRORE, CANCELLA RECORD
014500020111IF  1C     *IN98         IFEQ      *OFF                                         *NO ERRORE
014600020111     C                   DELETE    ECMOV000
014700020111     C                   ADD       1             TOTMOV                         *TOT AGGIORNATI ECMO
014800020111     C*
014900020111     C* SI ERRORE, STAMPA ERRORE ED ESCE DAL CICLO
015000020111X   1C                   ELSE                                                   *ERRORE
015100020111     C                   MOVEL     'M'           WSDX
015200020111     C                   EXSR      STAERR
015300020111     C                   MOVEL     'S'           $FINE                          *FINE CICLO
015400020111E   1C                   ENDIF
015500020111     C*
015600020111     C                   ENDSR
015700020111     C*------------------------------------------------------------------------*
015800020111     C* STAERR - STAMPA ERRORE
015900020111     C*------------------------------------------------------------------------*
016000020111     C     STAERR        BEGSR
016100020111     C*
016200020111     C* FINE PAGINA, STAMPA TESTATA
016300020111IF  1C     *INOF         IFEQ      *ON
016400020111     C                   EXCEPT    TEST
016500020111     C                   SETOFF                                       OF
016600020111E   1C                   ENDIF
016700020111     C*
016800020111     C* STAMPA ERRORE PER -ECMOV-
016900020111IF  1C     WSDX          IFEQ      'M'
017000020111     C                   EXCEPT    EMOV
017100020111E   1C                   ENDIF
017200020111     C*
017300020111     C                   ENDSR
017400020111     C*------------------------------------------------------------------------*
017500020111     C* STATOT - STAMPA RIEPILOGO ELABORAZIONE
017600020111     C*------------------------------------------------------------------------*
017700020111     C     STATOT        BEGSR
017800020111     C*
017900020111     C* FINE PAGINA, STAMPA TESTATA
018000020111IF  1C     *INOF         IFEQ      *ON
018100020111     C                   EXCEPT    TEST
018200020111     C                   SETOFF                                       OF
018300020111E   1C                   ENDIF
018400020111     C*
018500020111     C* STAMPA ERRORE PER -ECMOV-
018600020111IF  1C     WSDX          IFEQ      'M'
018700020111     C                   EXCEPT    TMOV
018800020111E   1C                   ENDIF
018900020111     C*
019000020111     C                   ENDSR
019100020111     C*------------------------------------------------------------------------*
019200020111     C* FINSR - OPERAZIONI FINALI
019300020111     C*------------------------------------------------------------------------*
019400020111     C     FINSR         BEGSR
019500020111     C*
019600020111     C                   ENDSR
019700020920     C*--------------------------------------------------------------------------------------------*
019800020920     C* REPERISCE I DATI UTENTE
019900020920     C*--------------------------------------------------------------------------------------------*
020000020920     C     REPDATIUTE    BEGSR
020100020920     C*
020200020920     C* INIZIALIZZA VARIABILI DI WRK
020300020920     C                   CLEAR                   TIBS34DS
020400020920     C                   CLEAR                   AZUTEDS
020500020920     C                   CLEAR                   DDATIUTE
020600020920     C*
020700020920     C     *DTAARA       DEFINE    §azute        azuteds
020800020920     C     *DTAARA       DEFINE    §datiute      ddatiute
020900020920     C                   IN(E)     *DTAARA
021000020920if  1C                   IF        %Error
021100020920     c                   EVAL      I34Tla = 'L'
021200020920     C                   CALL      'TIBS34R'
021300020920     C                   PARM                    Tibs34Ds
021400020920     C                   IN        *DTAARA
021500020920e   1C                   ENDIF
021600020920     C*
021700020920     C* ASSEGNO LA DESCRIZIONE DEL S.I./UTENTE AL CAMPO DEL VIDEO
021800020920     C                   MOVEL(P)  RSUT          DSFIRS           20
021900020920     C*
022000020920     C                   ENDSR
022100020111     C*------------------------------------------------------------------------*
022200020111     C* *INZSR - OPERAZIONI INIZIALI
022300020111     C*------------------------------------------------------------------------*
022400020111     C     *INZSR        BEGSR
022500020111     C*---
022600020111     C* RICEVIMENTO PARAMETRI
022700020111     C*---
022800020111     C     *ENTRY        PLIST
022900020111     C                   PARM                    KPJBA
023000020111     C                   MOVEL     KPJBU         PARAM
023100020111     C*---
023200020111     C* VARIABILI DI CONTROLLO
023300020111     C*---
023400020111     C                   MOVEL     'N'           $FINE             1            *FINE PGM
023500020111     C                   MOVEL     '0'           $ERR              1            *ERRORE GENERICO
023600020111     C                   MOVEL     'N'           $FMOV             1            *FINE LETTURA ECMOV
023700020111     C                   MOVEL     'S'           WRECOK            1            *VALIDITA' RECORD
023800020111     C                   Z-ADD     *ZEROS        TOTMOV            9 0          *TOT AGGIORNATI ECMO
023900020111     C                   MOVEL     *BLANKS       WSDX              1            *ERRORE
024000020111     C                   MOVEL     '1'           *INOF                          *OVERFLOW
024100020111     C*
024200020111     C* REPERISCE I DATI DELL'UTENTE
024300020920     C                   EXSR      REPDATIUTE
024400020111     C*
024500020111     C* REPERIMENTO DATA E ORA CORRENTE
024600020111     C                   TIME                    WN14             14 0          *ORA (6)+ DATA (8)
024700020111     C                   MOVE      WN14          DATCOR            8 0          *DATA (8) IN G/M/AA
024800020111     C                   MOVEL     WN14          ORACOR            6 0          *ORA (6)
024900020111     C*
025000020111     C                   ENDSR
025100020111     O*------------------------------------------------------------------------*
025200020111     OQSYSPRT   E            TEST             02
025300020111     O                       RIG(1)              66
025400020111     O                       RIG(2)             132
025500020111     O                       DSFIRS              20
025600020111     O                       DATCOR             107 '  /  /    '
025700020111     O                       ORACOR             118 '  :  :  '
025800020111     O                       PAGE          Z    132
025900020111     O*------------------------
026000020111     O          E            TEST             03
026100020111     O                       RIG(3)              66
026200020111     O                       RIG(3)             132
026300020111     O*------------------------
026400020111     O          E            EMOV        1
026500020111     O                       RIG(4)              66
026600020111     O                       MOVSOC              19
026700020111     O                       MOVCDP              30 '    /  /  '
026800020111     O                       MOVSVO        Z     38
026900020111     O                       MOVCDC              47
027000020111     O                       MOVDA               49
027100020111     O*------------------------
027200020111     O          E            TMOV        1
027300020111     O                       RIG(5)              66
027400020111     O                       TOTMOV        Z     39
027500020111     O*------------------------------------------------------------------------*
027600020111** RIG - RIGHE DI STAMPA
027700020111XXXXXXXXXXXXXXXXXXXX    TNSS51R   * CANCELLAZIONE MOVIMENTI CONTAB1
027800020111ILI DI C/E -NEW- *             99/99/9999   66:66:66      PAG.99992
027900020111------------------------------------------------------------------3
028000020111ERRORE ECMOV -> XXX 9999/99/99 9999999 XXXXXXXX X                 4
028100020111RECORD AGGIORNATI IN ECMOV -> 999999999                           5
0282000201116
0283000201117
0284000201118
0285000201119
02860002011110
