000100970722     H*------------------------------------------------------------------------*
000200970723     H* SALVATAGGIO SALDI SU CASSETTA - RICHIESTA PARAMETRI                    *
000300970722     H*------------------------------------------------------------------------*
000400970722     H DECEDIT('0,') DATEDIT(*DMY.)
000500970722     F*------------------------------------------------------------------------*
000600970722     F* DATA BASE
000700970722     F*------------------------------------------------------------------------*
000800140407     FTNSS43D   CF   E             WORKSTN
000900970724     FTABEL00F  IF   E           K DISK
001000970722     D*------------------------------------------------------------------------*
001100970722     D* SCHIERE
001200140407     D ERR             S             70    DIM(4) CTDATA PERRCD(1)
001300140407     D cmd1            S              1    DIM(99) CTDATA PERRCD(99)
001400140407     D cmd2            S              1    DIM(99) CTDATA PERRCD(99)
001500140407?     *--------------------------------------------------------------*
001600140407?     *  DS                                                          *
001700140407?     *--------------------------------------------------------------*
001800140407     D KPJBA         E DS
001900140407      *
002000140407     D TIBS55        E DS                  EXTNAME(TIBS55DS)
002100140407     D TNsc64ds      E DS
002200140407     D ds5s          E DS
002300140407      *
002400140407     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
002500140407     D CNCR80        E DS
002600140407      *
002700140407     D                SDS
002800140407     D  NOMPGM                 1     10
002900140407     d coman           s             99
003000140407     d luncmd          s             15  5
003100140407     D                 DS
003200140407     D  ANNO                   1      4  0
003300140407     D  MESE                   5      6  0
003400140407     D  DATA                   1      6  0
003500140407      *---------------------------------------------------------------*
003600140407      *   C I C L O      P R I N C I P A L E                          *
003700140407      *---------------------------------------------------------------*
003800140407     C*  Inizializzazione videata
003900140407     c     keytab        klist
004000140407     c                   kfld                    tblKUT
004100140407     c                   kfld                    tblCOD
004200140407     C                   CLEAR                   DS5S
004300140407     C                   Z-ADD     1             tblKUT
004400140407     C                   MOVEL     '5S'          tblCOD
004500140407     C     KEYTAB        CHAIN     TABEL00F                           99
004600140407IF  1C     *IN99         IFEQ      *OFF
004700140407     C                   MOVEL     TBLUNI        DS5S
004800140407E   1C                   ENDIF
004900140407     C                   MOVEL     §5SDCC        DATA
005000140407     C                   move      ANNO          V1CAN2                         *ANNO FINALE
005100140407     C                   move      MESE          V1CME2                         *MESE FINALE
005200140407     c                   eval      v1ccos = 'SAL'
005300140407     C*
005400140407     C                   EXSR      INZVID
005500140407     C*
005600140407     C*  Loop
005700140407     C     £FINE         DOUEQ     'S'
005800140407     C*
005900140407     C     TIPVID        CASEQ     '1'           GESVD1
006000140407     C                   END
006100140407     C*
006200140407     C                   END
006300140407     C*
006400140407     C                   SETON                                        LR
006500140407      *---------------------------------------------------------------*
006600140407      *  INZVID: Inizializzazione parametri video                     *
006700140407      *---------------------------------------------------------------*
006800140407     C     INZVID        BEGSR
006900140407     C*
007000140407     C                   eval      V1Clib = v1ccos + v1can2  +v1cme2
007100140407     C                   clear                   V1Cnas
007200140407     C                   MOVEL     NOMPGM        §PGM
007300140407     C                   MOVEL     'SI'          V1CPUL
007400140407     C                   MOVEL     *BLANKS       $MSG
007500140407     C*
007600140407     C                   ENDSR
007700140407      *---------------------------------------------------------------*
007800140407      *  GESVD1: Gestione prima videata richiesta parametri           *
007900140407      *---------------------------------------------------------------*
008000140407     C     GESVD1        BEGSR
008100140407     C*
008200140407     C                   WRITE     ss43T01
008300140407     C                   EXFMT     ss43D01
008400140407     C                   SETOFF                                       902123
008500140407     C                   MOVEL     *BLANKS       $MSG
008600140407     C*  Fine elaborazione
008700140407     C     *INKC         IFEQ      '1'
008800140407     C                   CLEAR                   TNsc64ds
008900140407     C                   MOVEL     'S'           £FINE
009000140407     C                   GOTO      FINVD1
009100140407     C                   END
009200140407     C*  Pe.Batch
009300140407     C     *INKV         IFEQ      '1'
009400140407     C                   CALL      'BCH09'
009500140407     C                   PARM                    KPJBA
009600140407     C                   END
009700140407     C*  Controlli
009800140407     C                   EXSR      CNTVD1
009900140407     C   90              GOTO      FINVD1
010000140407     C*  conferma
010100140407     C     *INKF         IFEQ      '1'
010200140407     C                   MOVEL     'S'           £FINE
010300140407     C                   eval      V1Clib = v1ccos + v1can2  +v1cme2
010400140407     C                   MOVEL     V1Clib        D64lib
010500140407     C                   MOVEL     V1Cnas        D64nas
010600140407     C                   MOVEL     V1CPUL        D64PUL
010700140407     C                   MOVE      'SS44'        KCOAZ
010800140407     C                   MOVEL     TNsc64ds      KPJBU
010900140407     C                   CALL      'BCH10'
011000140407     C                   PARM                    KPJBA
011100140407     C                   END
011200140407     C*
011300140407     C     FINVD1        ENDSR
011400140407      *---------------------------------------------------------------*
011500140407      *  CNTVD1: Controlli prima videata                              *
011600140407      *---------------------------------------------------------------*
011700140407     C     CNTVD1        BEGSR
011800140407     C*----------------------------------------------------*
011900140407     C     V1Clib        IFEQ      *BLANKS
012000140407     C                   SETON                                        2190
012100140407     C                   MOVEL     ERR(1)        $MSG
012200140407     C                   GOTO      FINCT1
012300140407     c                   else
012400140407     c                   movea     cmd1          coman
012500140407     c                   eval      %subst(coman:12:10) = v1clib
012600140407     c                   exsr      comandi2
012700140407     c                   if        *in34
012800140407     C                   SETON                                        2190
012900140407     C                   MOVEL     ERR(2)        $MSG
013000140407     C                   GOTO      FINCT1
013100140407     C                   endif
013200140407     C                   END
013300140407     C*----------------------------------------------------*
013400140407     C     V1Cnas        IFEQ      *BLANKS
013500140407     C                   SETON                                        2390
013600140407     C                   MOVEL     ERR(3)        $MSG
013700140407     C                   GOTO      FINCT1
013800140407     c                   else
013900140407     c                   movea     cmd2          coman
014000140407     c                   eval      %subst(coman:12:10) = v1cnas
014100140407     c                   exsr      comandi2
014200140407     c                   if        *in34
014300140407     C                   SETON                                        2390
014400140407     C                   MOVEL     ERR(4)        $MSG
014500140407     C                   GOTO      FINCT1
014600140407     C                   endif
014700140407     C                   END
014800140407     C*-
014900140407     C     FINCT1        ENDSR
015000140407     c*-------------------------------------------------
015100140407     c     comandi2      begsr
015200140407     c*-------------------------------------------------
015300140407      *
015400140407     C                   z-add     99            luncmd
015500140407     C                   CALL      'QCMDEXC'                            34
015600140407     C                   parm                    coman
015700140407     C                   parm                    luncmd
015800140407      *
015900140407     c                   endsr
016000140407      *  *INZSR: OPERAZIONI INIZIALI                                  *
016100140407      *---------------------------------------------------------------*
016200140407     C     *INZSR        BEGSR
016300140407      *---------------------------------------------------------------*
016400140407     C     *ENTRY        PLIST
016500140407     C                   PARM                    KPJBA
016600140407      *---------------------------------------------------------------*
016700140407     C                   Z-ADD     1             CODUT
016800140407     C                   CALL      'X§PARUT'
016900140407     C                   PARM                    UT§DSE
017000140407     C                   MOVEL     RAGUT         RSUT             20
017100140407     C                   MOVEL     REC80         CNCR80
017200140407     C*----------------------------------------------------*
017300140407     C                   MOVEL     'N'           £FINE             1
017400140407     C                   MOVEL     '1'           TIPVID            1
017500140407     C*----------------------------------------------------*
017600140407     C                   CLEAR                   TIBS55
017700140407     C                   MOVEL     'L'           I50TLA
017800140407     C                   MOVE      SIMPOU        I50PPO
017900140407     C                   MOVE      KNSIF         I50PSI
018000140407     C                   CALL      'TIBS55R'
018100140407     C                   PARM                    TIBS55
018200140407     C*----------------------------------------------------*
018300140407     C                   ENDSR
018400140407**
018500140407Immettere nome libreria salvataggio dati
018600140407Libreria inesistente
018700140407Immettere nome unità nastri per salvataggio
018800140407Unità nastri inesistente
018900140407**  CMD1
019000140407CHKOBJ OBJ(SALaaaamm ) OBJTYPE(*LIB)
019100140407**  CMD2
019200140407CHKOBJ OBJ(TAP01     ) OBJTYPE(*DEVD)
