000100000000     H*------------------------------------------------------------------------*
000200971002     H* LEGGE FNVAB00F E STAMPA SEGNACOLLI
000300000000     H*------------------------------------------------------------------------*
000400000000     H DECEDIT('0,') DATEDIT(*DMY.)
000500000000     F*------------------------------------------------------------------------*
000600000000     F* DATA BASE
000700000000     F*------------------------------------------------------------------------*
000800971219     FFNVAB00R  IF   E             DISK
000900971201     FTABEL00F  IF   E           K DISK
001000971002     FFNLV22P   O    F  132        PRINTER USROPN
001100971002     D*------------
001200971002     D* COMANDI
001300971002     D*------------
001400971002     D CMD             S             80    DIM(5) CTDATA PERRCD(1)
001500971002     D*-------------------
001600971002     D* DS PARAMETRI IN INPUT
001700971002     D*-------------------
001800971002     D TISI58        E DS                  EXTNAME(TISI58DS)
001900000000     D*-------------------
002000971002     D* DS PARAMETRI PER PGM STAMPA SEGNACOLLI
002100000000     D*-------------------
002200971002     D TISI57        E DS                  EXTNAME(TISI57DS)
002300971201     D*-------------------
002400971201     D* DS CODICI BOLLA
002500971201     D*-------------------
002600971201     D DS3A          E DS
002700000000     C*------------------------------------------------------------------------*
002800000000     C* MAIN LINE
002900000000     C*------------------------------------------------------------------------*
003000000000     C*
003100000000     C* SE NON DEVE ESSERE ESEGUITO, VA A FINE PGM
003200000000IF  1C     $FINE         IFEQ      '0'
003300000000     C*
003400000000     C* POSIZIONAMENTO SUL FILE E PRIMA LETTURA
003500971002     C                   EXSR      LETFIL
003600000000     C*
003700000000     C* CICLO FINO A FINE FILE
003800000000DO  2C     $FINE         DOWEQ     '0'
003900000000     C*
004000971002     C* STAMPA SEGNACOLLO
004100971002     C                   EXSR      STAMPA
004200000000     C*
004300000000     C* LEGGE IL RECORD SUCCESSIVO
004400000000     C                   EXSR      LETFIL
004500000000     C*
004600000000E   2C                   ENDDO
004700000000E   1C                   ENDIF
004800000000     C*
004900000000     C* OPERAZIONI DI CHIUSURA
005000000000     C                   EXSR      CHIUDE
005100000000     C*
005200000000     C                   SETON                                        LR
005300000000     C*------------------------------------------------------------------------*
005400000000     C* LETFIL - LETTURA PROSSIMO RECORD
005500000000     C*------------------------------------------------------------------------*
005600000000     C     LETFIL        BEGSR
005700000000     C*
005800000000     C* LEGGE FINO A:
005900000000     C*     - FINE FILE
006000000000     C*     - TROVATO RECORD VALIDO
006100000000     C                   MOVEL     'N'           WRECOK
006200000000DO  1C     $FINE         DOWEQ     '0'
006300000000     C     WRECOK        ANDEQ     'N'
006400971002     C                   READ      FNVAB00R                               99
006500000000IF  2C     *IN99         IFEQ      *ON
006600000000     C                   MOVEL     '1'           $FINE                          *FINE PGM
006700000000X   2C                   ELSE
006800000000     C                   EXSR      CHKREC                                       *CONTROLLA RECORD
006900000000E   2C                   ENDIF
007000000000E   1C                   ENDDO
007100960701     C*
007200000000     C                   ENDSR
007300000000     C*------------------------------------------------------------------------*
007400971002     C* CHKREC - CONTROLLA RECORD
007500000000     C*------------------------------------------------------------------------*
007600971002     C     CHKREC        BEGSR
007700000000     C*
007800000000     C                   MOVEL     'S'           WRECOK                         *RECORD VALIDO
007900000000     C*
008000000000     C                   ENDSR
008100000000     C*------------------------------------------------------------------------*
008200971002     C* STAMPA - LANCIA PGM STAMPA SEGNACOLLO
008300000000     C*------------------------------------------------------------------------*
008400971002     C     STAMPA        BEGSR
008500971201     C*
008600971201     C* RECUPERA SE FRANCO/ASSEGNATO
008700971201     C                   Z-ADD     CODUT         KTBUTE
008800971201     C                   MOVEL     '3A'          KTBCOD
008900971201     C                   MOVEL     *BLANKS       KTBKEY
009000971201     C                   MOVE      VABCBO        W001              1
009100971201     C                   MOVEL     W001          KTBKEY
009200971201     C     KEYTAB        CHAIN     TABEL00F                           99
009300971201     C     *IN99         IFEQ      '0'
009400971201     C                   MOVEL     TBLUNI        DS3A
009500971201     C                   MOVEL     §3ATB1        WTPO              1            *TIPO PORTO F/A
009600971201     C                   ENDIF
009700000000     C*
009800971002     C* SE N.SEGNACOLLO A.. UGUALE A 0, LO IMPOSTA COME N.SEGNACOLLO DA..
009900971002     C     VABNCA        IFEQ      0
010000971002     C                   Z-ADD     VABNCD        VABNCA
010100971002     C                   END
010200971002     C                   Z-ADD     VABNCD        DEPNCD            7 0
010300971002     C                   Z-ADD     0             I57NC1
010400971002     C*
010500971002     C* LANCIA PGM DI STAMPA PER TUTTI I SEGNCOLLI DELLA SPEDIZIONE
010600971002DO  1C     DEPNCD        DOWLE     VABNCA
010700971002     C                   MOVEL     *BLANKS       I57TLA                         *TIPO LANCIO
010800971002     C                   MOVEL     'N'           I57RLZ                         *RICERCA PO ARR/ZONA
010900971002     C                   MOVEL     I58TST        I57TST                         *TIPO STAMPANTE
011000971002     C                   Z-ADD     VABAAS        I57AAS                         *ANNO SPEDIZIONE
011100971002     C                   Z-ADD     VABMGS        I57MGS                         *MESE/GIORNO
011200971002     C                   MOVEL     I58RSM        I57RSM                         *RAGIONE SOC. MITTEN
011300971002     C                   MOVEL     I58NAM        I57NAM                         *NAZIONE MITTENTE
011400971002     C                   MOVEL     I58PRM        I57PRM                         *PROVINCIA MITTENTE
011500971002     C                   MOVEL     VABRSD        I57RSD                         *RAGIONE SOC. DESTIN
011600971002     C                   MOVEL     VABNZD        I57NAD                         *NAZIONE DESTINATARI
011700971002     C                   MOVEL     VABCAD        I57CAD                         *CAP DESTINATARIO
011800971002     C                   MOVEL     VABIND        I57IND                         *INDIRIZZO DESTINATA
011900971002     C                   MOVEL     VABLOD        I57LOD                         *LOCALITA' DESTINATA
012000971002     C                   MOVEL     VABPRD        I57PRD                         *PROVINCIA DESTINATA
012100971002     C                   Z-ADD     VABLNP        I57LNP                         *P.O. PARTENZA
012200971002     C                   MOVEL     VABRMA        I57CDP                         *CODICE PRODOTTO
012300971002     C                   MOVEL     VABTSP        I57TSP                         *TIPO SERVIZIO
012400971002     C                   MOVEL     VABFFD        I57FFD                         *FERMO DEPOSITO
012500971002     C                   Z-ADD     VABNRS        I57NRS                         *NUMERO SERIE SEGNAC
012600971002     C                   Z-ADD     DEPNCD        I57NCD                         *NUMERO SEGNACOLLO
012700971002     C                   ADD       1             I57NC1                         *NUM. SEGNACOLLO DI
012800971002     C                   Z-ADD     VABNCL        I57NC2                         *TOT.SEGNAC.DELLA SP
012900971002     C                   Z-ADD     VABVLB        I57VOL                         *VOLUME
013000971002     C                   MOVEL     VABFFD        I57FVR                         *FLAG VOL.BOLLETTATO
013100971002     C                   Z-ADD     VABPKB        I57PKG                         *PESO
013200971002     C                   MOVEL     *BLANKS       I57RST                         *RISTAMPA
013300971201     C                   SELECT
013400971201     C     VABCAS        WHENGT    *ZEROS
013500971201     C     WTPO          ANDNE     'A'
013600971201     C                   MOVEL     'S'           I57FCA                         *CONTRASS.
013700971201     C     VABCAS        WHENEQ    *ZEROS
013800971201     C     WTPO          ANDEQ     'A'
013900971201     C                   MOVEL     'A'           I57FCA                         *ASSEGN.
014000971201     C     VABCAS        WHENGT    *ZEROS
014100971201     C     WTPO          ANDEQ     'A'
014200971201     C                   MOVEL     'X'           I57FCA                         *CONTRASS.+ASSEGN.
014300971201     C                   OTHER
014400971201     C                   MOVEL     'N'           I57FCA                         *NO CONTR.,NO ASS.
014500971201     C                   ENDSL
014600971002     C                   Z-ADD     VABLNA        I57LNA                         *P.O. ARRIVO
014700971002     C                   Z-ADD     VABZNC        I57ZSC                         *ZONA CONSEGNA
014800971002     C                   CALL      'TISI57R'
014900971002     C                   PARM                    TISI57
015000971002     C                   ADD       1             DEPNCD                         *N.SEGNACOLLO
015100971002E   1C                   ENDDO
015200000000     C*
015300000000     C                   ENDSR
015400000000     C*------------------------------------------------------------------------*
015500000000     C* CHIUDE - OPERAZIONI DI CHIUSURA
015600000000     C*------------------------------------------------------------------------*
015700971002     C     CHIUDE        BEGSR
015800971002     C*
015900971002     C* CHIUDE FILE DI STAMPA
016000971002     C                   CLOSE     FNLV22P
016100971002     C*
016200971002     C* LANCIA TISI57R PER CHIUDERE FILE
016300971002     C                   CLEAR                   TISI57
016400971002     C                   MOVEL     'C'           I57TLA
016500971002     C                   CALL      'TISI57R'
016600971002     C                   PARM                    TISI57
016700000000     C*
016800000000     C                   ENDSR
016900000000     C*------------------------------------------------------------------------*
017000000000     C* *INZSR - OPERAZIONI INIZIALI
017100000000     C*------------------------------------------------------------------------*
017200000000     C     *INZSR        BEGSR
017300971002     C*
017400971002     C* PARAMETRI IN INPUT ALLA ROUTINE
017500971002     C     *ENTRY        PLIST
017600971002     C                   PARM                    TISI58
017700971201     C*
017800971201     C* CHIAVE LETTURA TABEL00F - COMPLETA
017900971201     C     KEYTAB        KLIST
018000971201     C                   KFLD                    KTBUTE
018100971201     C                   KFLD                    KTBCOD
018200971201     C                   KFLD                    KTBKEY
018300971201     C*
018400971201     C* DEFINISCO CAMPI CHIAVE
018500971201     C     *LIKE         DEFINE    TBLKUT        KTBUTE
018600971201     C     *LIKE         DEFINE    TBLCOD        KTBCOD
018700971201     C     *LIKE         DEFINE    TBLKEY        KTBKEY
018800971002     C*
018900000000     C* VARIABILI DI CONTROLLO
019000971201     C                   Z-ADD     1             CODUT             1 0
019100000000     C                   MOVEL     '0'           $FINE             1            *FINE PGM
019200000000     C                   MOVEL     'N'           WRECOK            1            *VALIDITA' RECORD
019300971002     C*
019400971002     C* OVRDBF PER SHARARE SPOOL FNLV22P
019500971002     C                   Z-ADD     80            LENGH            15 5
019600971002     C                   CALL      'QCAEXEC'                                    *LANCIA PGM
019700971002     C                   PARM                    CMD(1)
019800971002     C                   PARM                    LENGH
019900971002     C*
020000971002     C* APERTURA SPOOL STAMPA
020100971002     C                   OPEN      FNLV22P
020200960709     C*
020300000000     C                   ENDSR
020400000000     C*------------------------------------------------------------------------*
020500971002     O* OUTPUT FITTIZIO
020600971002     OFNLV22P   T   NLR LR               1
020700971002     O*------------------------------------------------------------------------*
020800971002** CMD - COMANDI CL
020900971124OVRPRTF FILE(FNLV22P) SHARE(*YES)
021000971002
021100971002
021200971002
021300971002
