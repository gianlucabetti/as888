000100970506     H*------------------------------------------------------------------------*
000200970506     H* AGGIORNA LE COMETENZE DEL WORKFILE IN BASE AL DELTA RICHIESTO          *
000300970506     H*------------------------------------------------------------------------*
000400970506     H DECEDIT('0,') DATEDIT(*DMY.)
000500970506     F*------------------------------------------------------------------------*
000600970506     F* DATA BASE
000700970506     F*------------------------------------------------------------------------*
000800011001     FTEETS01L  IF   E           K DISK
000900011001     FTEETW01L  UF   E           K DISK
001000970506     D*------------------------------------------------------------------------*
001100970506     D* SCHIERE
001200970506     D*------------------------------------------------------------------------*
001300970506     D*---
001400970506     D* SCAGLIONI
001500970506     D*---
001600970506     D SSGL            S             13  3 DIM(10)                              *SCAGLIONE SUPERIORE
001700970506     D SDEL            S              6  3 DIM(10)                              *% DELTA -CON SEGNO-
001800970506     D*------------------------------------------------------------------------*
001900970506     D* INPUT
002000970506     D*------------------------------------------------------------------------*
002100970506     D*-------------------
002200970506     D* DS PASSAGGIO PARAMETRI
002300970506     D*-------------------
002400970506     D DSTE00        E DS                  EXTNAME(TNTE00DS)
002500970506     D*-------------------
002600970506     D* INFDS - INFORMAZIONI PROGRAMMA
002700970506     D*-------------------
002800970506     D                SDS
002900980909     D  JOB                  244    253
003000980909     D  USER                 254    263
003100970506     D*-------------------
003200970506     D* ARCHITETTURA
003300970506     D*-------------------
003400970506     D KPJBA         E DS
003500970506     C*------------------------------------------------------------------------*
003600970506     C* MAIN LINE
003700970506     C*------------------------------------------------------------------------*
003800970506     C*
003900970506     C* CARICA DATI TIPO -SCAGLIONI-
004000970506     C                   EXSR      CARETS
004100970506     C*
004200970506     C* ERRORI IN ENTRATA, ESCE DAL PGM
004300970506IF  1C     $FINE         IFEQ      'N'
004400970506     C*
004500970506     C* POSIZIONAMENTO E PRIMA LETTURA -WORKFILE-
004600970506     C                   EXSR      SETFIL
004700970506     C*
004800970506     C* CICLO FINO A FINE ARCHIVIO
004900970506DO  2C     $FINE         DOWEQ     'N'
005000970506     C*
005100970506     C* OPERAZIONI PER NUOVO SCAGLIONE
005200970506     C                   EXSR      NEWSGL
005300970506     C*
005400970506     C* CICLO FINO A ROTTURA SCAGLIONE
005500970506DO  3C     $FINE         DOWEQ     'N'
005600970506     C     ETWSGL        ANDEQ     DEPSGL
005700970506     C*
005800970506     C* AGGIORNA LE COMPETENZE DEL WORKFILE
005900970506     C                   EXSR      AGGETW
006000970506     C*
006100970506     C* LETTURA SUCESSIVA
006200970506     C                   EXSR      LETFIL
006300970506E   3C                   ENDDO                                                  *FINE SCAGLIONI
006400970506E   2C                   ENDDO                                                  *FINE FILE
006500970506E   1C                   ENDIF
006600970506     C*
006700970506     C                   SETON                                        LR
006800970506     C*------------------------------------------------------------------------*
006900970506     C* SETFIL - POSIZIONAMENTO E PRIMA LETTURA -WORKFILE-
007000970506     C*------------------------------------------------------------------------*
007100970506     C     SETFIL        BEGSR
007200970506     C*
007300980909     C******************** MOVELUSER      KTWUSR           *UTENTE
007400980909     C                   MOVEL     JOB           KTWUSR                         *UTENTE
007500091123     C                   MOVEL     'S'           KTWDSF                         *DATI SEDE O FILIALE
007600980603     C                   MOVEL     D00CTO        KTWCTO                         *TARIFFA O OFFERTA
007700970506     C                   Z-ADD     D00KSC        KTWKSC                         *CLIENTE O VISITA
007800970506     C                   Z-ADD     D00CTR        KTWCTR                         *TARIFFA
007900970506     C                   Z-ADD     D00PRG        KTWPRG                         *PROGRESSIVO
008000011001     C     KEYETW        SETLL     TEETW01L                           99
008100970506IF  1C     *IN99         IFEQ      *ON                                          *FINE ARCHIVIO
008200970506     C                   MOVEL     'S'           $FINE                          *FINE PGM
008300970506X   1C                   ELSE
008400970506     C                   EXSR      LETFIL                                       *LETTURA RECORD
008500970506E   1C                   ENDIF
008600970506     C*
008700970506     C                   ENDSR
008800970506     C*------------------------------------------------------------------------*
008900970506     C* LETFIL - LETTURA SUCESSIVA -WORKFILE-
009000970506     C*------------------------------------------------------------------------*
009100970506     C     LETFIL        BEGSR
009200970506     C*
009300970506     C* LETTURA FINO A:
009400970506     C*  - FINE FILE
009500970506     C*  - TROVATO RECORD VALIDO
009600970506     C                   MOVEL     'N'           WRECOK
009700970506DO  1C     $FINE         DOWEQ     'N'
009800970506     C     WRECOK        ANDEQ     'N'
009900011001     C     KEYETW        READE     TEETW01L                               99
010000970506IF  2C     *IN99         IFEQ      *ON                                          *FINE ARCHIVIO
010100970506     C                   MOVEL     'S'           $FINE
010200970506X   2C                   ELSE
010300970506     C                   EXSR      CHKREC                                       *CONTROLLA RECORD
010400970506E   2C                   ENDIF
010500970506E   1C                   ENDDO
010600970506     C*
010700970506     C                   ENDSR
010800970506     C*------------------------------------------------------------------------*
010900970506     C* CHKREC - CONTROLLA VALIDITA' RECORD -DATI TIPO GENERALI-
011000970506     C*------------------------------------------------------------------------*
011100970506     C     CHKREC        BEGSR
011200970506     C*
011300970506     C                   MOVEL     'S'           WRECOK                         *VALIDITA' RECORD
011400970506     C*
011500970506     C* RAGGRUPPAMENTO <> RAGGRUPPAMENTO PORTO, ESCLUDE
011600971022IF  1C     ETWRDL        IFNE      001
011700971022     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
011800971022E   1C                   ENDIF
011900970506     C*
012000970506     C                   ENDSR
012100970506     C*------------------------------------------------------------------------*
012200970506     C* NEWSGL - OPERAZIONI PER NUOVO SCAGLIONE
012300970506     C*------------------------------------------------------------------------*
012400970506     C     NEWSGL        BEGSR
012500970506     C*
012600970506     C* MEMORIZZA IL NUOVO SCAGLIONE IN UN DEPOSITO
012700970506     C                   Z-ADD     ETWSGL        DEPSGL
012800970506     C*
012900970506     C* REPERISCE IL DELTA RICHIESTO DELLO SCAGLIONE
013000970506     C                   Z-ADD     1             I
013100970506     C                   SETOFF                                           28
013200970506     C     ETWSGL        LOOKUP    SSGL(I)                                28
013300970506IF  1C     *IN28         IFEQ      *ON                                          *TROVATO
013400970507     C                   Z-ADD     SDEL(I)       DELRIC           24 9
013500970506X   1C                   ELSE                                                   *NON TROVATO
013600970506     C                   Z-ADD     *ZEROS        DELRIC
013700970506E   1C                   ENDIF
013800970506     C*
013900970506     C                   ENDSR
014000970506     C*------------------------------------------------------------------------*
014100970506     C* AGGETW - AGGIORNAMENTO COMPETENZE WORKFILE
014200970506     C*------------------------------------------------------------------------*
014300970506     C     AGGETW        BEGSR
014400970506     C*
014500970506     C* RICALCOLA LE COMPETENZE IN BASE AL DELTA RICHIESTO
014600970506IF  1C     ETWCOM        IFGT      *ZEROS                                       *COMPETENZA PORTO
014700970506     C     ETWRIC        ANDGT     *ZEROS                                       *RICAVO PORTO
014800970620     C                   Z-ADD     ETWCOM        WCOM
014900970506     C                   EXSR      CALCOM
015000970506     C*
015100970506     C* AGGIORNA IL NUOVO IMPORTO COMPETENZA
015200970620IF  2C     WCOM          IFNE      ETWCOM                                       *NUOVO <> VECCHIO
015300970506     C                   Z-ADD     WCOM          ETWCOM
015400011001     C                   UPDATE    TEETW000
015500970506E   2C                   ENDIF
015600970506E   1C                   ENDIF
015700970506     C*
015800970506     C                   ENDSR
015900970506     C*------------------------------------------------------------------------*
016000970506     C* CALCOM - RICALCOLA LE COMPETENZE IN BASE AL DELTA RICHIESTO
016100970506     C*------------------------------------------------------------------------*
016200970506     C     CALCOM        BEGSR
016300970623     C*---
016400970623     C* DELTA ATTUALE --> (COMPETENZA - RICAVO) = GAP
016500970623     C*                   [(GAP x 100) : COMPETENZA] = DELTA
016600970623     C*---
016700970710     C*                               COMPETENZA
016800970623     C* TARIFFA A DELTA 0 = --------------------------------
016900970623     C*                     [1 - (DELTA DA CALCOLARE / 100)]
017000970623     C*---
017100970623     C*---
017200970709     C*                                    TARIFFA A DELTA 0
017300970623     C* TARIFFA A DELTA RICHIESTO = --------------------------------
017400970623     C*                             [1 - (DELTA DA CALCOLARE / 100)]
017500970623     C*---
017600970620     C*
017700970623     C                   Z-ADD     WCOM          DEPRIC
017800970620     C****
017900970620     C* DELTA ATTUALE
018000970620     C****
018100970602     C     ETWCOM        SUB       ETWRIC        GAP              24 9
018200970602     C     GAP           MULT      100           GAP
018300970623     C     GAP           DIV       ETWCOM        GAP
018400970620     C                   Z-ADD     GAP           DELATT           24 9
018500970620     C****
018600970623     C* TARIFFA A DELTA 0
018700970620     C****
018800970623IF  1C     DELATT        IFNE      *ZEROS
018900970623     C     DELATT        DIV       100           COMODO           24 9
019000970620     C     1             SUB       COMODO        COMODO
019100970709     C     ETWCOM        DIV       COMODO        DEPRIC           24 9          *BASE COMPETENZA
019200970623IF  2C     DEPRIC        IFLE      *ZEROS
019300970623     C                   Z-ADD     1             DEPRIC
019400970623E   2C                   ENDIF
019500970620E   1C                   ENDIF
019600970623     C****
019700970623     C* TARIFFA A DELTA RICHIESTO
019800970623     C****
019900970623IF  1C     DELRIC        IFNE      *ZEROS
020000970623     C     DELRIC        DIV       100           COMODO           24 9
020100970623     C     1             SUB       COMODO        COMODO
020200970623     C     DEPRIC        DIV       COMODO        DEPRIC           24 9
020300970623IF  2C     DEPRIC        IFLE      *ZEROS
020400970623     C                   Z-ADD     1             DEPRIC
020500970623E   2C                   ENDIF
020600970623E   1C                   ENDIF
020700970620     C*
020800970709     C                   Z-ADD     DEPRIC        WCOM
020900970506     C*
021000970602     C                   ENDSR
021100970506     C*------------------------------------------------------------------------*
021200970506     C* CARETS - CARICA I DATI TIPO -SCAGLIONI-
021300970506     C*------------------------------------------------------------------------*
021400970506     C     CARETS        BEGSR
021500970506     C*
021600970506     C                   CLEAR                   SSGL
021700970506     C                   CLEAR                   SDEL
021800970506     C                   Z-ADD     *ZEROS        I                 6 0
021900091123     C                   MOVEL     'S'           KTSDSF                         *DATI SEDE O FILIALE
022000980603     C                   MOVEL     D00CTO        KTSCTO                         *TARIFFA O OFFERTA
022100970506     C                   Z-ADD     D00KSC        KTSKSC                         *CLIENTE O VISITA
022200970506     C                   Z-ADD     D00CTR        KTSCTR                         *TARIFFA
022300970506     C                   Z-ADD     D00PRG        KTSPRG                         *PROGRESSIVO
022400970506     C                   Z-ADD     D00LNP        KTSLNP                         *LINEA DI PARTENZA
022500011001     C     KEYETS        SETLL     TEETS01L
022600011001     C     KEYETS        READE     TEETS01L                               99
022700970506DO  1C     *IN99         DOWEQ     *OFF
022800970506IF  2C     ETSATB        IFEQ      *BLANKS                                      *NO ANNULLATI
022900970506     C                   ADD       1             I
023000970506     C                   Z-ADD     ETSSGL        SSGL(I)                        *SCAGLIONE SUPERIORE
023100970506IF  3C     ETSSDE        IFEQ      '+'                                          *POSITIVO
023200970506     C                   Z-ADD     ETSPDE        SDEL(I)                        *% DELTA -CON SEGNO-
023300970506E   3C                   ENDIF
023400970506IF  3C     ETSSDE        IFEQ      '-'                                          *NEGATIVO
023500970506     C                   Z-SUB     ETSPDE        SDEL(I)                        *% DELTA -CON SEGNO-
023600970506E   3C                   ENDIF
023700970506E   2C                   ENDIF
023800011001     C     KEYETS        READE     TEETS01L                               99
023900970506E   1C                   ENDDO
024000970506     C*
024100970506     C                   ENDSR
024200970506     C*------------------------------------------------------------------------*
024300970506     C* *INZSR - OPERAZIONI INIZIALI
024400970506     C*------------------------------------------------------------------------*
024500970506     C     *INZSR        BEGSR
024600970506     C*--------------------
024700970506     C* RICEVIMENTO PARAMETRI
024800970506     C*--------------------
024900970506     C     *ENTRY        PLIST
025000970506     C                   PARM                    KPJBA
025100970506     C                   PARM                    DSTE00
025200970506     C*--------------------
025300970506     C* VARIABILI RIFERITE AL DATABASE
025400970506     C*--------------------
025500011001     C     *LIKE         DEFINE    ETSDSF        KTSDSF                         *TEETS00F
025600980603     C     *LIKE         DEFINE    ETSCTO        KTSCTO
025700970506     C     *LIKE         DEFINE    ETSKSC        KTSKSC
025800970506     C     *LIKE         DEFINE    ETSCTR        KTSCTR
025900970506     C     *LIKE         DEFINE    ETSPRG        KTSPRG
026000970506     C     *LIKE         DEFINE    ETSLNP        KTSLNP
026100011001     C     *LIKE         DEFINE    ETWUSR        KTWUSR                         *TEETW00F
026200980603     C     *LIKE         DEFINE    ETWDSF        KTWDSF
026300980603     C     *LIKE         DEFINE    ETWCTO        KTWCTO
026400970506     C     *LIKE         DEFINE    ETWKSC        KTWKSC
026500970506     C     *LIKE         DEFINE    ETWCTR        KTWCTR
026600970506     C     *LIKE         DEFINE    ETWPRG        KTWPRG
026700970506     C     *LIKE         DEFINE    ETSSGL        DEPSGL                         *DI DEPOSITO
026800970620     C     *LIKE         DEFINE    ETWCOM        WCOM                           *DI LAVORO
026900970506     C*--------------------
027000970506     C* CHIAVI DI LETTURA
027100970506     C*--------------------
027200011001     C* LETTURA TEETS01L - PARZIALE
027300970506     C     KEYETS        KLIST
027400980603     C                   KFLD                    KTSDSF                         *DATI SEDE O FILIALE
027500980603     C                   KFLD                    KTSCTO                         *TARIFFA O OFFERTA
027600970506     C                   KFLD                    KTSKSC                         *CLIENTE O VISITA
027700970506     C                   KFLD                    KTSCTR                         *TARIFFA
027800970506     C                   KFLD                    KTSPRG                         *PROGRESSIVO
027900970506     C                   KFLD                    KTSLNP                         *LINEA PARTENZA
028000011001     C* LETTURA TEETW01L - PARZIALE
028100970506     C     KEYETW        KLIST
028200970506     C                   KFLD                    KTWUSR                         *UTENTE
028300980603     C                   KFLD                    KTWDSF                         *DATI SEDE O FILIALE
028400980603     C                   KFLD                    KTWCTO                         *TARIFFA O OFFERTA
028500970506     C                   KFLD                    KTWKSC                         *CLIENTE O VISITA
028600970506     C                   KFLD                    KTWCTR                         *TARIFFA
028700970506     C                   KFLD                    KTWPRG                         *PROGRESSIVO
028800970506     C*--------------------
028900970506     C* VARIABILI DI CONTROLLO
029000970506     C*--------------------
029100970506     C                   MOVEL     'N'           $FINE             1            *FINE PROGRAMMA
029200970506     C                   MOVEL     'S'           WRECOK            1            *VALIDITA'RECORD
029300970506     C*
029400970506     C                   ENDSR
