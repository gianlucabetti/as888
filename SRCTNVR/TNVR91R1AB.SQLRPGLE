000100000000     H*------------------------------------------------------------------------*
000200080916     H*    Esegue la somma del WADELPCE sul WADELPCT (Italia+Estero)           *
000300080916     H*      Legge il File dell'Estero e lo somma su quello del totale.        *
000400000000     H*------------------------------------------------------------------------*
000500000000     H DECEDIT('0,') DATEDIT(*DMY.)
000600000000     F*------------------------------------------------------------------------*
000700000000     F* DATA BASE
000800000000     F*------------------------------------------------------------------------*
000900081027      *
001000091209     FWADELPCN  uF   E             DISK    prefix(INN:3) usropn
001100091209     F                                     rename(WADEL00N:wadelN)
001200091209      *
001300081027     F*------------------------------------------------------------------------*
001400081027     FAZORG01L  IF   E           K DISK
001500081027     FTABEL00F  IF   E           K DISK
001600081027     D*------------------------------------------------------------------------*
001700081027     D* SCHIERE
001800081027     D*------------------------------------------------------------------------*
001900081027     D*------------------
002000081027     D* ORGANIGRAMMA
002100081027     D*------------------
002200081027     D FIL             S              3  0 DIM(500)                             *FILIALI
002300081027     D DFIL            S             20    DIM(500)
002400081027     D ARE             S              3  0 DIM(500)                             *AREE
002500081027     D DARE            S             20    DIM(500)
002600081027     D DIV             S              1    DIM(500)                             *DIVISIONI
002700081027     D DDIV            S             20    DIM(500)
002800081027     D*------------------
002900080916      * Record Nr. per agganciare il file di totale come file Fisico
003000080916     D RRN_PCT         s              9  0
003100080916      *
003200000000     D*------------------
003300000000     D* ARCHITETTURA
003400000000     D*------------------
003500000000     C*------------------------------------------------------------------------*
003600000000     C* MAIN LINE
003700000000     C*------------------------------------------------------------------------*
003800081027     C*       carica tabelle Area e Divisione x decodifica
003900081027     c                   exsr      cartab
004000081027     C*
004100081027     C* Deve impostare l'area/descrizione area dell'agente unificante sui 3 files.
004200081027     c                   exsr      AREA_AgenUnif
004300081027      *
004400081027     C                   SETON                                        LR
004500080916     C* =========================================================================
004600081027     C*  ALLA FINE DEVE AGGIORNARE L'AREA DELL'AGENTE UNIFICIANTE SUI 3 ARCHIVI
004700081027     C*------------------------------------------------------------------------*
004800081027     C     AREA_AgenUnif BEGSR
004900081027     C*
005000091209     C*----------------
005100091209     c                   open      WADELpcN
005200091209     C* ciclo completo di lettura
005300091209     C                   read      WADELpcN
005400091209     C*
005500091209     C* CICLO FINO A FINE FILE DI LAVORO
005600091209DO  1C                   DOW       not %EoF(WADELpcN)
005700091209     C*
005800091209     c                   movel     inNauC        CoDFiliale        3 0
005900091209     c                   exsr      Dec_Area
006000091209     C                   eval      inNarAU  = CodAREa
006100091209     C                   eval      inNarAUD = DesAREa
006200091209      *
006300091209      * Aggiorna l'Area dell'agente Unificante
006400091209     c                   update    wadelN
006500091209     C* FINE ANNO
006600091209     C                   read      WADELpcN
006700091209E   1C                   ENDDO
006800091209     C*
006900091209     c                   close     WADELpcN
007000091209     C*
007100081027     C                   ENDSR
007200081027     C*------------------------------------------------------------------------*
007300081027     C* CARTAB - CARICA I DATI IN TABELLE
007400081027     C*------------------------------------------------------------------------*
007500081027     C     CARTAB        BEGSR
007600081027     C*
007700081027     C* CHIAVE LETTURA AZORG01L - COMPLETA
007800081027     C     KEYORG        KLIST
007900081027     C                   KFLD                    KORFIL                         *FILIALE
008000081027     C     *LIKE         DEFINE    ORGFIL        KORFIL                         *AZORG01L
008100081027     C*
008200081027     C* CHIAVE LETTURA TABEL00F - COMPLETA
008300081027     C     KEYTAB        KLIST
008400081027     C                   KFLD                    KTBKUT                         *CODICE UTENTE
008500081027     C                   KFLD                    KTBCOD                         *CODICE TABELLA
008600081027     C                   KFLD                    KTBKEY                         *CHIAVE TABELLA
008700081027     C     *LIKE         DEFINE    TBLKUT        KTBKUT                         *TABEL00F
008800081027     C     *LIKE         DEFINE    TBLCOD        KTBCOD
008900081027     C     *LIKE         DEFINE    TBLKEY        KTBKEY
009000081027     C*
009100081027     C                   Z-ADD     *ZEROS        F                 5 0          *PER ORGANIGRAMMA
009200081027     C*--------------------
009300081027     C* ORGANIGRAMMA
009400081027     C*--------------------
009500081027     C                   Z-ADD     *ZEROS        F
009600081027     C                   Z-ADD     *ZEROS        KORFIL
009700081027      *
009800081027     C     KEYORG        SETLL     AZORG01L
009900081027     C                   READ      AZORG01L
010000081027      *
010100081027DO  1C                   DoW       not %EoF(AZORG01L)
010200081027IF  2C     ORGFVA        IFEQ      *BLANKS                                      *NO ANNULLATI
010300081027IF  3C     ORGFAG        IFNE      'V'                                          *FILIALE/AGENZIA
010400081027     C                   ADD       1             F
010500081027     C                   Z-ADD     ORGFIL        FIL(F)
010600081027     C                   MOVEL     ORGDES        DFIL(F)
010700081027     C                   Z-ADD     1             KTBKUT
010800081027     C                   MOVEL     '05'          KTBCOD
010900081027     C                   MOVEL     *BLANKS       KTBKEY
011000081027     C                   MOVEL     ORGCAR        KTBKEY
011100081027     C     KEYTAB        CHAIN     TABEL00F
011200081027IF  5C                   If        %Found(TABEL00F)
011300081027     C                   Z-ADD     ORGCAR        ARE(F)
011400081027     C                   MOVEL     TBLUNI        DARE(F)
011500081027E   5C                   ENDIF
011600081027     C                   Z-ADD     1             KTBKUT
011700081027     C                   MOVEL     '17'          KTBCOD
011800081027     C                   MOVEL     *BLANKS       KTBKEY
011900081027     C                   MOVEL     ORGFL3        KTBKEY
012000081027     C     KEYTAB        CHAIN     TABEL00F
012100081027IF  5C                   If        %Found(TABEL00F)
012200081027     C                   MOVEL     ORGFL3        DIV(F)
012300081027     C                   MOVEL     TBLUNI        DDIV(F)
012400081027E   5C                   ENDIF
012500081027E   3C                   ENDIF
012600081027E   2C                   ENDIF
012700081027     C*
012800081027     C                   READ      AZORG01L
012900081027E   1C                   ENDDO
013000081027     C*--------------------
013100081027     C                   ENDSR
013200081027     C*------------------------------------------------------------------------*
013300081027     C*? DECARE - DECODIFICA CODICE AREA
013400081027     C*          INPUT  -- DEPARE: CODICE AREA
013500081027     C*          OUTPUT -- DESARE: DESCRIZIONE AREA
013600081027     C*------------------------------------------------------------------------*
013700081027     C     Dec_Area      BEGSR
013800081027     C*
013900081027     C                   Z-ADD     1             f
014000081027     C     CodFiliale    LOOKUP    FIL(f)                                 99
014100081027IF  1C     *IN99         IFEQ      *OFF
014200081027     C                   MOVEL     *zeros        CodAREa           3 0
014300081027     C                   MOVEL     *ALL'*'       DesAREa          25
014400081027X   1C                   ELSE
014500081027     C                   MOVEL     ARE(f)        CodAREa
014600081027     C                   MOVEL     DARE(f)       DesAREa
014700081027E   1C                   ENDIF
014800081027     C*
014900081027     C                   ENDSR
015000081027     C*------------------------------------------------------------------------*
