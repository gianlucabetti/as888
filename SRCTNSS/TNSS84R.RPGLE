000100000000     H DECEDIT('0,') DATEDIT(*YMD.)
000200000000      **************************************************************************
000300000000      *
000400000000      *  Nome programma:  TNSS84R
000500000000      *  Descrizione   :  Statistica transiti per terminal di partenza/arrivo
000600000000      *                   Programma di richiesta parametri, filtro.
000700000000      *  Data creazione:  23 MAG 1994
000800000000      *
000900000000      **************************************************************************
001000000000     FTNSS84D   CF   E             WORKSTN
001100000000      *
001200000000     D MES             S              9    DIM(12) CTDATA PERRCD(1)             NOMI MENSILITA'
001300000000      *
001400000000      * DS x il passaggio parametri
001500000000     D PARAM           DS                  INZ
001600990608     D  ANNODA                 1      4  0
001700990608     D  MESEDA                 5      6  0
001800990608     D  DATADA                 1      6  0
001900990608     D  ANNOA                  7     10  0
002000990608     D  MESEA                 11     12  0
002100990608     D  DATAA                  7     12  0
002200990608     D*------------------
002300990608     D* DS DATA GENERICA(8) IN GG/MM/AAAA
002400990608     D*------------------
002500990608     D                 DS
002600990608     D  DGIOR8                 1      2  0
002700990608     D  DMESE8                 3      4  0
002800990608     D  DANNO8                 5      8  0
002900990608     D  DDATA8                 1      8  0
003000990608     D*------------------
003100990608     D* DS CONTROLLO DATA (8)
003200990608     D*------------------
003300990608     D WLBDA8          DS                  INZ
003400990608     D  G08DAT                 1      8  0
003500990608     D  G08INV                 9     16  0
003600990608     D  G08ERR                17     17
003700990608     D  G08TGI                18     22  0
003800020920     D TIBS34DS      E DS                                                       *Profili utente
003900020920     D DDATIUTE      E DS                                                       *Dati utente
004000020920     D AZUTEDS       E DS                  extname(AZUTE00F)                    *Utenti
004100000000     D KPJBA         E DS
004200000000      *
004300000000     C     *ENTRY        PLIST
004400000000     C                   PARM                    KPJBA
004500000000      *
004600000000      **************************************************************************
004700000000      *                     M A I N      L I N E
004800000000      **************************************************************************
004900990608      *
005000990608      * Reperimento dati utente
005100020920     C                   EXSR      REPDATIUTE
005200990608      *
005300990608      * reperisce la data+ora corrente
005400990608     C                   TIME                    N14              14 0          *ORA (6)+ DATA (8)
005500990608     C                   MOVE      N14           N8                8 0          *DATA (8) IN G/M/AA
005600990608     C                   Z-ADD     N8            DDATA8                         *SCOMPOSIZIONE DATA
005700000000      *
005800000000      * Se siamo a gennaio imposto l'anno precedente come date
005900990608     C     DMESE8        IFEQ      1                                            1=GENNAIO
006000000000     C                   Z-ADD     12            MESEA
006100990608     C     DANNO8        SUB       1             ANNOA
006200000000     C                   ELSE
006300990608     C     DMESE8        SUB       1             MESEA
006400990608     C                   Z-ADD     DANNO8        ANNOA
006500000000     C                   ENDIF
006600000000     C                   Z-ADD     MESEA         MESEDA
006700000000     C                   Z-ADD     ANNOA         ANNODA
006800000000      *
006900000000      * Emetto il formato fi guida
007000000000     C     EMETTO        TAG
007100000000     C                   EXFMT     SS84R1
007200000000      *
007300000000      * F3=Fine  e  F12=Ritorno
007400000000     C   KL
007500000000     COR KC              GOTO      FINE
007600000000      *
007700000000      * Eseguo i controlli sul formato video
007800000000     C                   SETOFF                                       90
007900000000     C                   EXSR      CONTR                                        CONTROLLI
008000000000      *
008100000000      * F21=Pe.Batch
008200000000     C   KV
008300000000     CANN90              DO
008400000000     C                   MOVE      '1'           KRITB
008500000000     C                   MOVE      *BLANKS       KCOAZ
008600960209     C                   MOVE      'SS85'        KCOAZ
008700000000     C                   CALL      'BCH09'
008800000000     C                   PARM                    KPJBA
008900000000     C                   ENDDO
009000000000      *
009100000000      * Per generico errore, o non conferma riemetto
009200000000     C   90
009300000000     CORNKF              GOTO      EMETTO
009400000000      *
009500000000     C                   MOVEL     PARAM         KPJBU
009600960209     C                   MOVE      'SS85'        KCOAZ
009700000000     C                   CALL      'BCH10'
009800000000     C                   PARM                    KPJBA
009900000000      *
010000000000     C     FINE          TAG
010100000000     C                   SETON                                        LR
010200020920     C*--------------------------------------------------------------------------------------------*
010300020920     C* REPERISCE I DATI UTENTE
010400020920     C*--------------------------------------------------------------------------------------------*
010500020920     C     REPDATIUTE    BEGSR
010600020920     C*
010700020920     C* INIZIALIZZA VARIABILI DI WRK
010800020920     C                   CLEAR                   TIBS34DS
010900020920     C                   CLEAR                   AZUTEDS
011000020920     C                   CLEAR                   DDATIUTE
011100020920     C*
011200020920     C     *DTAARA       DEFINE    §azute        azuteds
011300020920     C     *DTAARA       DEFINE    §datiute      ddatiute
011400020920     C                   IN(E)     *DTAARA
011500020920if  1C                   IF        %Error
011600020920     c                   EVAL      I34Tla = 'L'
011700020920     C                   CALL      'TIBS34R'
011800020920     C                   PARM                    Tibs34Ds
011900020920     C                   IN        *DTAARA
012000020920e   1C                   ENDIF
012100020920     C*
012200020920     C                   ENDSR
012300000000      **************************************************************************
012400000000      * Eseguo i controlli sul formato video di guida
012500000000      **************************************************************************
012600000000     C     CONTR         BEGSR
012700000000      *
012800000000      * La data iniziale non puo' superare quella finale
012900000000     C     ANNODA        IFGT      ANNOA
013000000000     C                   SETON                                        42  90
013100000000     C                   ENDIF
013200000000     C     ANNODA        IFEQ      ANNOA
013300000000     C     MESEDA        ANDGT     MESEA
013400000000     C                   SETON                                        42  90
013500000000     C                   ENDIF
013600000000      *
013700000000      * La differenza tra le due date non puo' superare 12 mesi
013800000000     C     ANNODA        IFLT      ANNOA
013900990608     C     ANNOA         SUB       1             $COMOD            4 0
014000000000     C     $COMOD        COMP      ANNODA                             4141      ON=DIVERSO
014100000000     C                   ENDIF
014200000000      *
014300000000     C  N41ANNODA        IFLT      ANNOA
014400000000     C     MESEA         ANDGE     MESEDA
014500000000     C                   SETON                                        41
014600000000     C                   ENDIF
014700000000     C   41              SETON                                        90
014800000000      *
014900000000      * La data finale non puo' superare "oggi"
015000990608     C     ANNOA         IFEQ      DANNO8
015100990608     C     MESEA         ANDGT     DMESE8
015200000000     C                   SETON                                        43  90
015300000000     C                   ENDIF
015400000000      *
015500000000      * Se data iniziale vuota, imposto inizio anno
015600000000     C  N90ANNODA        IFEQ      *ZERO
015700000000     C     MESEDA        ANDEQ     *ZERO
015800000000     C                   Z-ADD     ANNOA         ANNODA
015900000000     C                   Z-ADD     1             MESEDA
016000000000     C                   ENDIF
016100000000      *
016200000000     C     ANNODA        IFNE      *ZERO
016300000000     C     MESEDA        ANDEQ     *ZERO
016400000000     C                   Z-ADD     1             MESEDA
016500000000     C                   ENDIF
016600000000      *
016700000000      * Decodifico il mese iniziale
016800000000     C                   MOVE      *BLANKS       DEMEDA
016900000000     C     MESEDA        IFGT      *ZERO
017000000000     C                   Z-ADD     MESEDA        $A                2 0
017100000000     C                   MOVE      MES($A)       DEMEDA
017200000000     C                   ENDIF
017300000000      *
017400000000      * Decodifico il mese finale
017500000000     C                   MOVE      *BLANKS       DEMEA
017600000000     C     MESEA         IFGT      *ZERO
017700000000     C                   Z-ADD     MESEA         $A
017800000000     C                   MOVE      MES($A)       DEMEA
017900000000     C                   ENDIF
018000000000      *
018100000000     C                   ENDSR
018200000000      **************************************************************************
018300000000**
018400000000GENNAIO
018500000000FEBBRAIO
018600000000MARZO
018700000000APRILE
018800000000MAGGIO
018900000000GIUGNO
019000000000LUGLIO
019100000000AGOSTO
019200000000SETTEMBRE
019300000000OTTOBRE
019400000000NOVEMBRE
019500000000DICEMBRE
