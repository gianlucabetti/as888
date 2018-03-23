000100000000     H*--------------------------------------------------------------------------------------------*
000200040406     H* Pulizia archivio TISTB00F
000300000000     H*--------------------------------------------------------------------------------------------*
000400000000     H DECEDIT('0,') DATEDIT(*DMY.)
000500000000     F*--------------------------------------------------------------------------------------------*
000600000000     F* Data base
000700000000     F*--------------------------------------------------------------------------------------------*
000800040406     Ftitas30c  IF   E           K DISK
000900040406     Ftistb01l  UF   E           K DISK
001000040406     Ftabel00f  IF   E           K DISK
001100040406     D*----------------
001200000000     D* Data structure
001300040406     D*----------------
001400000000     D wlbda8          DS                  INZ                                  *ds "XSRDA8"
001500000000     D  g08dat                        8  0                                       -data g/m/aa
001600000000     D  g08inv                        8  0                                       -data aa/m/g
001700000000     D  g08err                        1                                          -errore
001800000000     D  g08tgi                        5  0                                       -n°giorni data
001900000000     D kpjba         E DS                                                       *architettura
002000040406     D ds5as1        E DS
002100000000     D*----------------
002200040406     D* Variabili a programma
002300000000     D*----------------
002400040406     D FlgElab         S              1    INZ('S')                             *flag si/no elaboraz
002500000000     D wn14            S             14  0                                      *numerico 14
002600000000     D wn8             S              8  0                                      *numerico 8
002700000000     D datcor          S              8  0                                      *d.corrente aa/m/g
002800040406     D findata         S               D   DATFMT(*ISO) INZ(D'1999-01-01')
002900040406     D findataFin      S              8  0
003000000000     C*--------------------------------------------------------------------------------------------*
003100000000     C* Main lines
003200000000     C*--------------------------------------------------------------------------------------------*
003300020918     C*
003400040406     C* Reperisco il numero d giorni d mantanimento record in file TISTB
003500040406     C                   EXSR      cartab
003600020918     C*
003700040406     C* Se tutto ok => procedo con la cancellazione fisica dei record dal file TISTB
003800040406     C                   IF        FlgElab = 'S'
003900040406     C                   EXSR      exeelab
004000040406     C                   ENDIF
004100971230     C*
004200040406     C                   SETON                                        LR
004300000306     C*--------------------------------------------------------------------------------------------*
004400040406     C* exeelab - legge il file fisico e determina se deletare i record
004500000306     C*--------------------------------------------------------------------------------------------*
004600040406     C     exeelab       BEGSR
004700000000     C*
004800040406     C* Scorro tutto il file TISTB
004900040406     C     *LOVAL        SETLL     tistb01l
005000040406     C                   IF        %found(tistb01l)
005100040406     C                   READ      tistb01l
005200040406     C                   DOW       not %eof(tistb01l)
005300040406     C* Deleto solo se il record è già stato trasmesso al cliente
005400040406     C                   IF        stbFTR <> *blanks
005500040406     C* Aggancio la bolla sul TITAS x verificare i giorni d mantenimento record rispetto alla
005600040406     C* data consegna reale merce
005700040406     C     KEYTAS_P      SETLL     titas30c
005800040406     C                   IF        %equal(titas30c)
005900040406     C     KEYTAS_P      READE     titas30c
006000040406     C                   DOW       not %eof(titas30c)
006100040406     C                   IF        tasDCM > *zeros
006200040406     C* Verifico se la data consegna reale merce della bolla è antecedente ai giorni d mantenimento
006300040406     C* record in TISTB
006400040406     C     *ISO          MOVE      datcor        findata
006500040406     C     findata       SUBDUR    §5ASTB:*D     findata
006600040406     C     *ISO          MOVE      findata       findataFin
006700040406     C                   IF        tasDCM < findataFin
006800100215     C                   DELETE(e) tistb01l
006900040406     C                   ENDIF
007000040406     C                   ENDIF
007100040406     C     KEYTAS_P      READE     titas30c
007200040406     C                   ENDDO
007300100215     C                   ELSE
007400100215     C                   DELETE(e) tistb01l
007500040406     C                   ENDIF
007600040406     C*
007700040406     C                   ENDIF
007800040406     C                   READ      tistb01l
007900040406     C                   ENDDO
008000040406     C                   ENDIF
008100040406     C*
008200000000     C                   ENDSR
008300040406     C*--------------------------------------------------------------------------------------------*
008400000000     C* cartab - carica le tabelle occorrenti
008500040406     C*--------------------------------------------------------------------------------------------*
008600000000     C     cartab        BEGSR
008700040406     C*
008800040406     C                   EVAL      tblkut = 1
008900040406     C                   EVAL      tblcod = '5A'
009000040406     C                   EVAL      tblkey = 'SEDE1'
009100020905     C*
009200040406     C     KEYTAB        CHAIN     tabel00f
009300040406     C                   IF        %found(tabel00f) AND
009400040406     C                             tblflg = *blanks
009500040406     C                   EVAL      ds5as1 = tbluni
009600040406     C                   ELSE
009700040406     C                   EVAL      FlgElab = 'N'
009800020905     C                   ENDIF
009900000000     C*
010000000000     C                   ENDSR
010100040406     C*--------------------------------------------------------------------------------------------*
010200000000     C* *inzsr - operazioni iniziali
010300040406     C*--------------------------------------------------------------------------------------------*
010400000000     C     *inzsr        BEGSR
010500040406     C*
010600000000     C* ricevimento parametri
010700040406     C     *ENTRY        PLIST
010800040406     C                   PARM                    kpjba
010900000000     C*
011000000000     C* calcola la data corrente
011100000000     C                   TIME                    wn14                           *ora(6) + data(8)
011200000000     C                   MOVE      wn14          wn8                            *data(8) in g/m/aa
011300000000     C                   Z-ADD     wn8           g08dat
011400000000     C                   Z-ADD     *zeros        g08inv
011500000000     C                   MOVEL     '0'           g08err
011600000000     C                   CALL      'XSRDA8'
011700000000     C                   PARM                    wlbda8
011800000000     C                   Z-ADD     g08inv        datcor                         *d.corrente aa/m/g
011900000000     C*
012000040406     C* definizione chiavi
012100040406     C*
012200040406     C* - TABEL00F - Completa
012300040406     C     KEYTAB        KLIST
012400040406     C                   KFLD                    tblkut
012500040406     C                   KFLD                    tblcod
012600040406     C                   KFLD                    tblkey
012700040406     C*
012800040406     C* - TITAS30C - Parziale
012900040406     C     KEYTAS_P      KLIST
013000040406     C                   KFLD                    stbaas
013100040406     C                   KFLD                    stblnp
013200040406     C                   KFLD                    stbnrs
013300040406     C                   KFLD                    stbnsp
013400000000     C*
013500000000     C                   ENDSR
