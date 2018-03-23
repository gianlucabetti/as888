000100930128      ***********************************************************************
000200931027      *   SPALMA RETTIFICHE RICAVI E NOTE DI CREDITO SUI SALDI BOLLE        *
000300990528      *   LANCIATO DALLA STATISTICA RICAVI -> TNSS56C                       *
000400990528      *   LANCIATO DALL' AGGIORNAMENTO DA CONTABILITA' -> YCO904C           *
000500930128      ***********************************************************************
000600930128     H DECEDIT('0,') DATEDIT(*DMY.)
000700990609     FSISDF01L  IF   E           K DISK
000800931027     FTABEL00F  UF   E           K DISK
000900990609     FSISDR01L  UF   E           K DISK
001000931123     D*****************************************************************
001100931123     D* SCHIERA IMPORTI FILIALE DELL'ANNO
001200020110     D WRB             S             15  3 DIM(12)                              *RIC.BOLLE
001300020110     D WRR             S             15  3 DIM(12)                              *RETT.RIC.
001400020110     D WNC             S             15  3 DIM(12)                              *N.CRED.
001500931027     D*****************************************************************
001600010410     D DS5S          E DS
001700931122     I*****************************************************************
001800000000     C*
001900930129     C* ROUTINE INIZIALE
002000930129     C                   EXSR      RUTINZ
002100931027     C*
002200931027     C* POSIZIONAMENTO SUL FILE E PRIMA LETTURA
002300931122     C                   Z-ADD     PARANN        SDRANN
002400990609     C     KEYSDR        SETLL     SISDR01L                           99        *FINE-ARCHIV
002500990609     C  N99              READ      SISDR01L                               99
002600931027     C*
002700931027     C* CICLO FINO A FINE FILE OPPURE A FINE ANNO
002800931027     C     *IN99         DOWEQ     '0'                                          --- 1 -->
002900931122     C     SDRANN        ANDEQ     PARANN
003000931027     C*
003100931027     C* LEGGE SALDI FILIALE E CALCOLA IMPORTI DA SPALMARE
003200931027     C                   EXSR      CALCOL
003300931027     C*
003400931027     C* SE GLI IMPORTI DA SPALMARE SONO UGUALI A QUELLI GIA' PRESENTI NEL REC.
003500931027     C* NON RISCRIVO NEMMENO IL RECORD
003600931027     C     SDRIRR        IFEQ      WIRR
003700931027     C     SDRINC        ANDEQ     WINC
003800931027     C                   ELSE
003900931028     C                   Z-ADD     WIRR          SDRIRR                         *RETT.RIC.
004000931028     C                   Z-ADD     WINC          SDRINC                         *NOTE ACCRED.
004100990609     C                   UPDATE    SISDR000                                     *RISCRIVE
004200931027     C                   END
004300931027     C*
004400931027     C* LEGGE IL RECORD SUCCESSIVO
004500990609     C                   READ      SISDR01L                               99
004600931027     C                   END                                                    <-- 1 ---
004700931027     C*
004800010410     C* AGGIORNA TABELLA 5S CON FLAG DI SPALMAMENTO EFFETTUATO
004900931027     C                   EXSR      AGGTAB
005000000000     C*
005100000000     C                   SETON                                        LR
005200930208     C*----------------------------------------------------------------
005300931027     C*? CALCOL - CALCOLA IMPORTI DA SPALMARE
005400930208     C*----------------------------------------------------------------
005500931027     C     CALCOL        BEGSR
005600930624     C*
005700931027     C* AZZERA IMPORTI CALCOLO
005800020110     C                   Z-ADD     *ZEROS        WIRR             15 3
005900020110     C                   Z-ADD     *ZEROS        WINC             15 3
006000990609     C* SE E' CAMBIATA LA FILIALE LEGGO SU SISDF TUTTI I DODICI MESI DELL'ANNO
006100931123     C* E MEMORIZZO I SALDI
006200931123     C     SDRFLP        IFNE      WFIL                                         --- 1 -->
006300931123     C                   Z-ADD     SDRFLP        WFIL
006400931123     C                   DO        12            I                 6 0          --- 2 -->
006500931027     C                   Z-ADD     SDRANN        SDFANN
006600931123     C                   Z-ADD     SDRFLP        SDFFIL
006700931123     C                   Z-ADD     I             SDFMES
006800990609     C     KEYSDF        CHAIN     SISDF01L                           99
006900931123     C     *IN99         IFEQ      '0'
007000931123     C                   Z-ADD     SDFIRR        WRR(I)
007100931123     C                   Z-ADD     SDFIRB        WRB(I)
007200931123     C                   Z-ADD     SDFINC        WNC(I)
007300931123     C                   ELSE
007400931123     C                   Z-ADD     *ZEROS        WRR(I)
007500931123     C                   Z-ADD     *ZEROS        WRB(I)
007600931123     C                   Z-ADD     *ZEROS        WNC(I)
007700931123     C                   ENDIF
007800931123     C                   ENDDO                                                  <-- 2 ---
007900931123     C                   ENDIF                                                  <-- 1 ---
008000931123     C*
008100931123     C* IMPOSTO L'INDICE(MESE) PER LA SCHIERA SALDI
008200931123     C                   Z-ADD     SDRMES        I
008300931123     C*
008400931123     C* CALCOLO RETT.RICAVI SE CI SONO
008500931123     C     WRR(I)        IFNE      *ZEROS
008600931123     C     WRB(I)        ANDNE     *ZEROS
008700931029     C     SDRIRB        ANDNE     *ZEROS
008800020110     C     WRR(I)        MULT      SDRIRB        WTM213           21 3
008900020110     C     WTM213        DIV(H)    WRB(I)        WIRR
009000931027     C                   END
009100931123     C*
009200931123     C* CALCOLO NOTE CRED.  SE CI SONO
009300931123     C     WNC(I)        IFNE      *ZEROS
009400931123     C     WRB(I)        ANDNE     *ZEROS
009500931029     C     SDRIRB        ANDNE     *ZEROS
009600020110     C     WNC(I)        MULT      SDRIRB        WTM213
009700020110     C     WTM213        DIV(H)    WRB(I)        WINC
009800931027     C                   END
009900930208     C*
010000930208     C                   ENDSR
010100931027     C*-------------------------------------------------------------------
010200931027     C*? AGGTAB - AGGIORNA TABELLA DATI SALDI
010300931027     C*-------------------------------------------------------------------
010400931027     C     AGGTAB        BEGSR
010500931027     C*
010600931029     C                   Z-ADD     CODUT         KTBUTE
010700010410     C                   MOVEL     '5S'          KTBCOD
010800931029     C                   MOVEL     *BLANKS       KTBKEY
010900931029     C     KEYTAB        CHAIN     TABEL                              99
011000931029     C     *IN99         IFEQ      '0'                                          --- 1 -->
011100010410     C                   MOVEL     TBLUNI        DS5S
011200010410     C                   MOVEL     'S'           §5SFSE                         *C.ECON. SPALMATO S/
011300010410     C                   MOVEL     DS5S          TBLUNI
011400940427     C                   MOVEL     *BLANKS       TBLFTR                         *SFLEGGA X TRASMISS.
011500940427     C                   Z-ADD     *ZEROS        TBLDTR
011600931028     C                   UPDATE    TABEL
011700931027     C                   END                                                    <-- 1 ---
011800931027     C*
011900931027     C                   ENDSR
012000000000     C*----------------------------------------------------------------
012100000000     C*? RUTINZ            ROUTINE INIZIALE
012200000000     C*----------------------------------------------------------------
012300000000     C     RUTINZ        BEGSR
012400000000     C*
012500930129     C* PARAMETRI IN INPUT AL PGM
012600930218     C     *ENTRY        PLIST
012700990528     C                   PARM                    PARANN            4 0
012800931026     C*
012900931026     C* DEFINISCE LE VARIABILI DI PROGRAMMA
013000931026     C     *LIKE         DEFINE    TBLKUT        KTBUTE
013100931026     C     *LIKE         DEFINE    TBLCOD        KTBCOD
013200931026     C     *LIKE         DEFINE    TBLKEY        KTBKEY
013300930702     C*
013400930702     C* CHIAVE LETTURA TABEL00F - COMPLETA
013500930702     C     KEYTAB        KLIST
013600930702     C                   KFLD                    KTBUTE
013700930702     C                   KFLD                    KTBCOD
013800930702     C                   KFLD                    KTBKEY
013900930621     C*
014000990609     C* CHIAVE LETTURA SISDF01L - COMPLETA
014100931026     C     KEYSDF        KLIST
014200931026     C                   KFLD                    SDFANN
014300931026     C                   KFLD                    SDFMES
014400931026     C                   KFLD                    SDFFIL
014500931027     C*
014600990609     C* CHIAVE LETTURA SISDR01L - PARZIALE
014700931027     C     KEYSDR        KLIST
014800931027     C                   KFLD                    SDRANN
014900930208     C*
015000930208     C* IMPOSTO A ZERO IL FINE PGM
015100930208     C                   MOVEL     '0'           FINL00            1
015200930702     C                   Z-ADD     1             CODUT             1 0
015300931123     C                   Z-ADD     *ZEROS        WFIL              3 0
015400000000     C*
015500000000     C                   ENDSR
