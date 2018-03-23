000100000000     H*--------------------------------------------------------------------------------------------*
000200040406     H* Pulizia archivio TISTB00F
000300000000     H*--------------------------------------------------------------------------------------------*
000400000000     H DECEDIT('0,') DATEDIT(*DMY.)
000500000000     F*--------------------------------------------------------------------------------------------*
000600000000     F* Data base
000700000000     F*--------------------------------------------------------------------------------------------*
000800170821     Ftabel00f  IF   E           K DISK
000900040406     Ftitas30c  IF   E           K DISK
001000040406     Ftistb01l  UF   E           K DISK
001100170821     Ftistbd2i  UF   E           K DISK
001200040406     D*----------------
001300000000     D* Data structure
001400040406     D*----------------
001500000000     D kpjba         E DS                                                       *architettura
001600170821     D ds5as1        E DS
001700170821     D*----------------
001800170821     D* Variabili a programma
001900170821     D*----------------
002000170821     D FlgElab         S              1    INZ('S')                             *flag si/no elaboraz
002100170821     D datcor          S              8  0                                      *d.corrente aa/m/g
002200170821     D findata         S               D   DATFMT(*ISO) INZ(D'1999-01-01')
002300170821     D findataFin      S              8  0
002400000000     C*--------------------------------------------------------------------------------------------*
002500000000     C* Main lines
002600000000     C*--------------------------------------------------------------------------------------------*
002700170821     C*
002800170821     C* Reperisco la data limite prima del quale NON eliminare
002900170821     C                   EXSR      cartab
003000170821     C*
003100170821     C* Se tutto ok => procedo con la cancellazione fisica dei record dal file TISTB
003200170821     C                   IF        FlgElab = 'S'
003300170821     C                   EXSR      exeSTB
003400170821     C*
003500170821     C                   EXSR      exeSTBD
003600170821     C                   ENDIF
003700971230     C*
003800040406     C                   SETON                                        LR
003900000306     C*--------------------------------------------------------------------------------------------*
004000170821     C* exeSTB  - legge tutto il file TISTB e determina se pulire i record
004100000306     C*--------------------------------------------------------------------------------------------*
004200170821     C     exeSTB        BEGSR
004300000000     C*
004400040406     C* Scorro tutto il file TISTB
004500040406     C     *LOVAL        SETLL     tistb01l
004600040406     C                   IF        %found(tistb01l)
004700040406     C                   READ      tistb01l
004800040406     C                   DOW       not %eof(tistb01l)
004900170821     C*
005000170821     C* Verifico se la data spedizione è più vecchia della data limite prevista in tabella
005100170821     C                   IF        stbDSP < findataFin
005200170821     C*
005300170821     C* Verifico se spedizione ancora presente sulle bolle di sede
005400170821     C     KEYTAS_P      CHAIN     titas30c
005500170821     C                   IF        %found(titas30c)
005600170821     C                   ELSE
005700170821     C*
005800170821     C* Se spedizione non trovata => elimino record TISTB corrente
005900170821     C                   DELETE(e) tistb01l
006000170821     C                   ENDIF
006100170821     C                   ENDIF
006200170821     C*
006300040406     C                   READ      tistb01l
006400040406     C                   ENDDO
006500040406     C                   ENDIF
006600040406     C*
006700000000     C                   ENDSR
006800170821     C*--------------------------------------------------------------------------------------------*
006900170821     C* exeSTBD - legge tutto il file TISTBD e determina se pulire i record
007000170821     C*--------------------------------------------------------------------------------------------*
007100170821     C     exeSTBD       BEGSR
007200170821     C*
007300170821     C* Scorro tutto il file TISTB
007400170821     C     *LOVAL        SETLL     tistbd2i
007500170821     C                   IF        %found(tistbd2i)
007600170821     C                   READ      tistbd2i
007700170821     C                   DOW       not %eof(tistbd2i)
007800170821     C*
007900170821     C* Verifico se la data spedizione è più vecchia della data limite prevista in tabella
008000170821     C                   IF        stbDDAO < (findataFin * 1000000)
008100170821     C*
008200170821     C* Verifico se spedizione ancora presente sulle bolle di sede
008300170821     C     KEYTAS_P2     CHAIN     titas30c
008400170821     C                   IF        %found(titas30c)
008500170821     C                   ELSE
008600170821     C*
008700170821     C* Se spedizione non trovata => elimino record TISTBD corrente
008800170821     C                   DELETE(e) tistbd2i
008900170821     C                   ENDIF
009000170821     C                   ENDIF
009100170821     C*
009200170821     C                   READ      tistbd2i
009300170821     C                   ENDDO
009400170821     C                   ENDIF
009500170821     C*
009600170821     C                   ENDSR
009700170821     C*--------------------------------------------------------------------------------------------*
009800170821     C* cartab - carica le tabelle occorrenti
009900170821     C*--------------------------------------------------------------------------------------------*
010000170821     C     cartab        BEGSR
010100170821     C*
010200170821     C                   EVAL      tblkut = 1
010300170821     C                   EVAL      tblcod = '5A'
010400170821     C                   EVAL      tblkey = 'SEDE1'
010500170821     C*
010600170821     C     KEYTAB        CHAIN     tabel00f
010700170821     C                   IF        %found(tabel00f) AND
010800170821     C                             tblflg = *blanks
010900170821     C                   EVAL      ds5as1 = tbluni
011000170821     C     *ISO          MOVE      datcor        findata
011100170821     C     findata       SUBDUR    §5ASTB:*D     findata
011200170821     C     *ISO          MOVE      findata       findataFin
011300170821     C*
011400170821     C                   ELSE
011500170821     C                   EVAL      FlgElab = 'N'
011600170821     C                   ENDIF
011700170821     C*
011800170821     C                   ENDSR
011900040406     C*--------------------------------------------------------------------------------------------*
012000000000     C* *inzsr - operazioni iniziali
012100040406     C*--------------------------------------------------------------------------------------------*
012200000000     C     *inzsr        BEGSR
012300040406     C*
012400000000     C* ricevimento parametri
012500040406     C     *ENTRY        PLIST
012600040406     C                   PARM                    kpjba
012700170821     C*
012800170821     C* calcola la data corrente
012900170821     C                   z-add     *zeros        datcor
013000170821     C                   eval      datcor = %dec(%date() : *iso)
013100000000     C*
013200040406     C* definizione chiavi
013300170821     C*
013400170821     C* - TABEL00F - Completa
013500170821     C     KEYTAB        KLIST
013600170821     C                   KFLD                    tblkut
013700170821     C                   KFLD                    tblcod
013800170821     C                   KFLD                    tblkey
013900040406     C*
014000040406     C* - TITAS30C - Parziale
014100040406     C     KEYTAS_P      KLIST
014200040406     C                   KFLD                    stbaas
014300040406     C                   KFLD                    stblnp
014400040406     C                   KFLD                    stbnrs
014500040406     C                   KFLD                    stbnsp
014600170821     C*
014700170821     C* - TITAS30C - Parziale
014800170821     C     KEYTAS_P2     KLIST
014900170821     C                   KFLD                    stbdaas
015000170821     C                   KFLD                    stbdlnp
015100170821     C                   KFLD                    stbdnrs
015200170821     C                   KFLD                    stbdnsp
015300000000     C*
015400000000     C                   ENDSR
