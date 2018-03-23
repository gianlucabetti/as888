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
002400170821     d wSQL            s           2048    inz  varying
002500000000     C*--------------------------------------------------------------------------------------------*
002600000000     C* Main lines
002700000000     C*--------------------------------------------------------------------------------------------*
002800170821     C*
002900170821     C* Reperisco la data limite prima del quale NON eliminare
003000170821     C                   EXSR      cartab
003100170821     C*
003200170821     C* Se tutto ok => procedo con la cancellazione fisica dei record dal file TISTB
003300170821     C                   IF        FlgElab = 'S'
003400170821     C                   EXSR      exeVAV
003500170821     C*
003600170821     C                   EXSR      exeSTB
003700170821     C*
003800170821     C                   EXSR      exeSTBD
003900170821     C                   ENDIF
004000971230     C*
004100040406     C                   SETON                                        LR
004200000306     C*--------------------------------------------------------------------------------------------*
004300170821     C* exeSTB  - legge tutto il file TISTB e determina se pulire i record
004400000306     C*--------------------------------------------------------------------------------------------*
004500170821     C     exeSTB        BEGSR
004600000000     C*
004700040406     C* Scorro tutto il file TISTB
004800040406     C     *LOVAL        SETLL     tistb01l
004900040406     C                   IF        %found(tistb01l)
005000040406     C                   READ      tistb01l
005100040406     C                   DOW       not %eof(tistb01l)
005200170821     C*
005300170821     C* Verifico se la data spedizione è più vecchia della data limite prevista in tabella
005400170821     C                   IF        stbDSP < findataFin
005500170821     C*
005600170821     C* Verifico se spedizione ancora presente sulle bolle di sede
005700170821     C     KEYTAS_P      CHAIN     titas30c
005800170821     C                   IF        %found(titas30c)
005900170821     C                   ELSE
006000170821     C*
006100170821     C* Se spedizione non trovata => elimino record TISTB corrente
006200170821     C                   DELETE(e) tistb01l
006300170821     C                   ENDIF
006400170821     C                   ENDIF
006500170821     C*
006600040406     C                   READ      tistb01l
006700040406     C                   ENDDO
006800040406     C                   ENDIF
006900040406     C*
007000000000     C                   ENDSR
007100170821     C*--------------------------------------------------------------------------------------------*
007200170821     C* exeSTBD - legge tutto il file TISTBD e determina se pulire i record
007300170821     C*--------------------------------------------------------------------------------------------*
007400170821     C     exeSTBD       BEGSR
007500170821     C*
007600170821     C* Scorro tutto il file TISTB
007700170821     C     *LOVAL        SETLL     tistbd2i
007800170821     C                   IF        %found(tistbd2i)
007900170821     C                   READ      tistbd2i
008000170821     C                   DOW       not %eof(tistbd2i)
008100170821     C*
008200170821     C* Verifico se la data spedizione è più vecchia della data limite prevista in tabella
008300170821     C                   IF        stbDDAO < (findataFin * 1000000)
008400170821     C*
008500170821     C* Verifico se spedizione ancora presente sulle bolle di sede
008600170821     C     KEYTAS_P2     CHAIN     titas30c
008700170821     C                   IF        %found(titas30c)
008800170821     C                   ELSE
008900170821     C*
009000170821     C* Se spedizione non trovata => elimino record TISTBD corrente
009100170821     C                   DELETE(e) tistbd2i
009200170821     C                   ENDIF
009300170821     C                   ENDIF
009400170821     C*
009500170821     C                   READ      tistbd2i
009600170821     C                   ENDDO
009700170821     C                   ENDIF
009800170821     C*
009900170821     C                   ENDSR
010000170821     C*--------------------------------------------------------------------------------------------*
010100170821     C* exeVAV  - cancelola rcd di TIVAV aventi chiave1 = VASWS e data <= quella finale
010200170821     C*--------------------------------------------------------------------------------------------*
010300170821     C     exeVAV        BEGSR
010400170821     C*
010500170821         wSQL =
010600170821         'delete from TIVAV00F where VAVKEY1 = ''VASWS'' and +
010700170821          decimal(date(VAVMOM), 8, 0) <= ' +
010800170821          %editc(findataFin:'X');
010900170821
011000170821         exec sql   execute immediate :wSQL;
011100170821     C*
011200170821     C                   ENDSR
011300170821     C*--------------------------------------------------------------------------------------------*
011400170821     C* cartab - carica le tabelle occorrenti
011500170821     C*--------------------------------------------------------------------------------------------*
011600170821     C     cartab        BEGSR
011700170821     C*
011800170821     C                   EVAL      tblkut = 1
011900170821     C                   EVAL      tblcod = '5A'
012000170821     C                   EVAL      tblkey = 'SEDE1'
012100170821     C*
012200170821     C     KEYTAB        CHAIN     tabel00f
012300170821     C                   IF        %found(tabel00f) AND
012400170821     C                             tblflg = *blanks
012500170821     C                   EVAL      ds5as1 = tbluni
012600170821     C     *ISO          MOVE      datcor        findata
012700170821     C     findata       SUBDUR    §5ASTB:*D     findata
012800170821     C     *ISO          MOVE      findata       findataFin
012900170821     C*
013000170821     C                   ELSE
013100170821     C                   EVAL      FlgElab = 'N'
013200170821     C                   ENDIF
013300170821     C*
013400170821     C                   ENDSR
013500040406     C*--------------------------------------------------------------------------------------------*
013600000000     C* *inzsr - operazioni iniziali
013700040406     C*--------------------------------------------------------------------------------------------*
013800000000     C     *inzsr        BEGSR
013900040406     C*
014000000000     C* ricevimento parametri
014100040406     C     *ENTRY        PLIST
014200040406     C                   PARM                    kpjba
014300170821     C*
014400170821     C* calcola la data corrente
014500170821     C                   z-add     *zeros        datcor
014600170821     C                   eval      datcor = %dec(%date() : *iso)
014700000000     C*
014800040406     C* definizione chiavi
014900170821     C*
015000170821     C* - TABEL00F - Completa
015100170821     C     KEYTAB        KLIST
015200170821     C                   KFLD                    tblkut
015300170821     C                   KFLD                    tblcod
015400170821     C                   KFLD                    tblkey
015500040406     C*
015600040406     C* - TITAS30C - Parziale
015700040406     C     KEYTAS_P      KLIST
015800040406     C                   KFLD                    stbaas
015900040406     C                   KFLD                    stblnp
016000040406     C                   KFLD                    stbnrs
016100040406     C                   KFLD                    stbnsp
016200170821     C*
016300170821     C* - TITAS30C - Parziale
016400170821     C     KEYTAS_P2     KLIST
016500170821     C                   KFLD                    stbdaas
016600170821     C                   KFLD                    stbdlnp
016700170821     C                   KFLD                    stbdnrs
016800170821     C                   KFLD                    stbdnsp
016900000000     C*
017000000000     C                   ENDSR
