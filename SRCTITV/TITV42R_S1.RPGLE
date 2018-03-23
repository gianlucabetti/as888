000100890921     H DECEDIT('0,') DATEDIT(*DMY.)
000200991027     H dftactgrp(*yes)
000300991027
000400030314     FTABEL00F  IF   E           K DISK
000500030314     FTITAH30C  IF   E           K DISK
000600030314     FTITA430C  IF   E           K DISK
000700030314     FFNEVB05L  IF   E           K DISK
000800080924     FTIVGDTMP  UF   E             DISK
000900030314     FfnvacfdxwtO    E             DISK
001000991027
001100080924     D fnvac00t      e ds
001200991027     D prmppt          s             50
001300991027     D prmesito        s              1
001400991027     D wrkesito        s                   like(prmesito)
001500020920     D wrkDAT          s              8
001600030314
001700030314     D ds2a          e ds
001800080903     D sk2Agiac        s              3    dim(200)
001900080903     D sk2Alav         s              3    dim(200)
002000020729
002100991027
002200991027     C                   EXSR      traduci
002300921023     C                   SETON                                        LR
002400991027
002500991027     C     traduci       BEGSR
002600991027     C*
002700080924     C                   READ      TIVGDTMP
002800080924     C                   dow       not %eof(TIVGDTMP)
002900080924     C*
003000080924     C                   EVAL      fnvac00t = vgdDTA
003100020508     C*
003200020508     C* Innanzitutto inizializzo il formato record del file tradotto
003300030314     C                   CLEAR                   FNVACFDX
003400030314     C****
003500020729     C* Quindi effettuo la traduzione vera e propria
003600030314     C****
003700030314     C*
003800030314     C* Valorizzo i campi relativi alla spedizione, ovvero uguali x tutti i colli della bolla
003900030314     C                   MOVEL     vacRMA        FDXVCSHIP
004000030314     C                   MOVE      vacRMN        FDXVCAWB
004100030314     C
004200030314     C*
004300030314     C* Effettuo considerazioni particolari relative all'esito della spedizione
004400030314     C*
004500030314     C* LASCIATO AVVISO...
004600030314     C                   IF        vacDLA > *zeros and
004700030314     C                             vacDAG = *zeros and
004800030314     C                             vacDCM = *zeros
004900030314     C                   EVAL      FDXVCSTSC = 'A'
005000030314     C                   EVAL      FDXVCSTSD = %editw(vacDLA:'        ')
005100030314     C                   ENDIF
005200030314     C*
005300030314     C* GIACENZA...
005400030314     C                   IF        vacDAG > *zeros and
005500030314     C                             vacDCM = *zeros
005600030314     C                   EVAL      FDXVCSTSC = 'G'
005700030314     C                   EVAL      FDXVCSTSD = %editw(vacDAG:'        ')
005800030314     C                   ENDIF
005900030314     C*
006000030314     C* CONSEGNATA OK...
006100030314     C                   IF        vacDCM > *zeros
006200030314     C                   IF        vacCCA = 'C' OR
006300030314     C                             vacCCA = 'A'
006400030314     C                   ELSE
006500030314     C                   IF        vacCCA = *blanks
006600030314     C                   EVAL      FDXVCSTSC = 'C'
006700030314     C                   EVAL      FDXVCSTSD = %editw(vacDCM:'        ')
006800030528     C                   EVAL      FDXVCSTST = %editw(vacHMC:'    ')
006900030528     C                   IF        %subst(FDXVCSTST:1:1) = *blanks
007000030528     C                   EVAL      %subst(FDXVCSTST:1:1) = '0'
007100030528     C                   ENDIF
007200030314     C                   ELSE
007300030314     C*
007400030314     C* CHIUSURA OK CON DIROTTAMENTO...
007500030314     C                   IF        vacCCA = '1'
007600030314     C                   EVAL      FDXVCSTSC = 'C'
007700030314     C                   EVAL      FDXVCSTSD = %editw(vacDCM:'        ')
007800030528     C                   EVAL      FDXVCSTST = %editw(vacHMC:'    ')
007900030528     C                   IF        %subst(FDXVCSTST:1:1) = *blanks
008000030528     C                   EVAL      %subst(FDXVCSTST:1:1) = '0'
008100030528     C                   ENDIF
008200030314     C                   ELSE
008300030314     C*
008400030314     C* CHIUSURA CON RESO...
008500030314     C                   IF        vacCCA = '2'
008600030314     C                   EVAL      FDXVCSTSC = 'R'
008700030314     C                   EVAL      FDXVCSTSD = %editw(vacDCM:'        ')
008800030528     C                   EVAL      FDXVCSTST = %editw(vacHMC:'    ')
008900030528     C                   IF        %subst(FDXVCSTST:1:1) = *blanks
009000030528     C                   EVAL      %subst(FDXVCSTST:1:1) = '0'
009100030528     C                   ENDIF
009200030314     C                   ELSE
009300030314     C*
009400030314     C* CHIUSURA CON ANOMALIA...
009500030314     C                   EVAL      FDXVCSTSC = 'D'
009600030314     C                   EVAL      FDXVCSTSD = %editw(vacDCM:'        ')
009700030528     C                   EVAL      FDXVCSTST = %editw(vacHMC:'    ')
009800030528     C                   IF        %subst(FDXVCSTST:1:1) = *blanks
009900030528     C                   EVAL      %subst(FDXVCSTST:1:1) = '0'
010000030528     C                   ENDIF
010100030314     C                   ENDIF
010200030314     C                   ENDIF
010300030314     C                   ENDIF
010400030314     C                   ENDIF
010500030314     C                   ENDIF
010600030407     C*
010700030407     C* In caso di bolla di "tipo" lasciato avviso reperisco l'ora dell'evento
010800030407     C                   IF        FDXVCSTSC = 'A'
010900030407     C     KEYevbP       SETLL     fnevb05l
011000030407     C     KEYevbP       READE     fnevb05l
011100030407     C                   DOW       not %eof(fnevb05l)
011200030407     C     evbCEV        LOOKUP    sk2Alav                                20
011300030407     C                   IF        *IN20 = *ON
011400030528     C                   EVAL      FDXVCSTST = %editw(evbHEV:'    ')
011500030528     C                   IF        %subst(FDXVCSTST:1:1) = *blanks
011600030528     C                   EVAL      %subst(FDXVCSTST:1:1) = '0'
011700030528     C                   ENDIF
011800030407     C                   ENDIF
011900030407     C     KEYevbP       READE     fnevb05l
012000030407     C                   ENDDO
012100030407     C                   ENDIF
012200030314     C*
012300030314     C* In caso di bolla di "tipo" giacenza verifico il tipo di giacenza nel dettaglio
012400030314     C                   IF        FDXVCSTSC = 'G'
012500030314     C     KEYevbP       SETLL     fnevb05l
012600030314     C     KEYevbP       READE     fnevb05l
012700030314     C                   DOW       not %eof(fnevb05l)
012800030314     C     evbCEV        LOOKUP    sk2Agiac                               20
012900030314     C                   IF        *IN20 = *ON
013000030314     C                   EVAL      FDXVCSTSC = evbCEV
013100030528     C                   EVAL      FDXVCSTST = %editw(evbHEV:'    ')
013200030528     C                   IF        %subst(FDXVCSTST:1:1) = *blanks
013300030528     C                   EVAL      %subst(FDXVCSTST:1:1) = '0'
013400030528     C                   ENDIF
013500030314     C                   ENDIF
013600030314     C     KEYevbP       READE     fnevb05l
013700030314     C                   ENDDO
013800030314     C                   ENDIF
013900030314     C*
014000030314     C* In caso di bolla di "tipo" consegnata reperisco il nominativo del ritirante merce in cons.
014100030314     C                   IF        FDXVCSTSC = 'C'
014200150507     C***                EVAL      FDXVCREF = 'ILLEGGIBILE'
014300030314     C                   EVAL      ta4TRC = '1'
014400030314     C     KEYta4C       CHAIN     tita430c
014500030314     C                   IF        %found(tita430c)
014600030314     C                   EVAL      FDXVCREF = ta4NOT
014700030314     C                   ENDIF
014800030314     C                   ENDIF
014900030423     C*
015000030423     C                   MOVEL     '0'           flgWRI            1
015100030314     C*
015200030314     C* Quindi aggancio il dettaglio colli
015300030314     C                   EVAL      tahTRC = 'C'
015400030411     C     KEYtahC       SETLL     titah30c
015500030314     C                   IF        %found(titah30c)
015600030411     C     KEYtahC       READE     titah30c
015700030314     C                   DOW       not %eof(titah30c)
015800030314     C                   EVAL      FDXVCMPS = tahNOT
015900030314     C                   IF        FDXVCMPS = FDXVCAWB
016000030314     C                   EVAL      FDXVCMPS = *all'0'
016100030314     C                   ENDIF
016200030314     C                   IF        FDXVCSTSC <> *blanks AND
016300030314     C                             FDXVCSTSC <> 'D'     AND
016400030314     C                             FDXVCMPS  <> *blanks
016500030314     C                   WRITE     FNVACFDX
016600030423     C                   EVAL      flgWRI = '1'
016700030314     C                   ENDIF
016800030314     C     KEYtahC       READE     titah30c
016900030314     C                   ENDDO
017000030314     C                   ENDIF
017100030423     C*
017200030423     C* Se nn trovato dettaglio colli => scrivo cmq un record
017300030423     C                   IF        flgWRI = '0'
017400030423     C                   EVAL      FDXVCMPS = *all'0'
017500030423     C                   IF        FDXVCSTSC <> *blanks AND
017600030423     C                             FDXVCSTSC <> 'D'     AND
017700030423     C                             FDXVCMPS  <> *blanks
017800030423     C                   WRITE     FNVACFDX
017900030423     C                   ENDIF
018000030423     C                   ENDIF
018100020508     C*
018200080924     C                   DELETE    TIVGD000
018300020508     C*
018400080924     C                   READ      TIVGDTMP
018500991027     C                   ENDdo
018600991027     C*
018700991027     C                   eval      wrkesito = '0'
018800991027     C*
018900910830     C                   ENDSR
019000020508
019100020508
019200030314
019300030314     C     cartab        BEGSR
019400030314     C*
019500030314     C                   clear                   sk2Agiac
019600030407     C                   clear                   sk2Alav
019700030314     C                   z-add     *zeros        i                 3 0
019800030407     C                   z-add     *zeros        j                 3 0
019900030314     C                   eval      tblKUT = 1
020000030314     C                   eval      tblCOD = '2A'
020100030314     C     KEYtabP       setll     tabel00f
020200030314     C     KEYtabP       reade     tabel00f
020300030314     C                   dow       not %eof(tabel00f)
020400030314     C                   movel(p)  tblUNI        ds2a
020500030314     C                   if        §2aFTC = 'G'
020600030314     C                   add       1             i
020700030314     C                   eval      sk2Agiac(i) = tblKEY
020800030314     C                   endif
020900030407     C                   if        §2aFTC = 'A'
021000030407     C                   add       1             j
021100030407     C                   eval      sk2Alav(j) = tblKEY
021200030407     C                   endif
021300030314     C     KEYtabP       reade     tabel00f
021400030314     C                   enddo
021500030314     C*
021600030314     C                   ENDSR
021700030314
021800020508
021900991027
022000991027      /TITLE Operazioni iniziali.
022100991027     C     *inzsr        BEGSR
022200991027     C*
022300991027     C     *ENTRY        PLIST
022400030314     C                   PARM                    prmppt
022500030314     C     wrkesito      PARM      wrkesito      prmesito
022600030314     C****
022700030314     C* Definizione chiavi
022800030314     C****
022900030314     C* Chiave su TABEL00F - parziale
023000030314     C     KEYtabP       KLIST
023100030314     C                   KFLD                    tblKUT
023200030314     C                   KFLD                    tblCOD
023300030314     C* Chiave su TITAH30C - completa
023400030314     C     KEYtahC       KLIST
023500030314     C                   KFLD                    vacAAS
023600030314     C                   KFLD                    vacLNP
023700030314     C                   KFLD                    vacNRS
023800030314     C                   KFLD                    vacNSP
023900030314     C                   KFLD                    tahTRC
024000030314     C* Chiave su TITA430C - completa
024100030314     C     KEYta4C       KLIST
024200030314     C                   KFLD                    vacAAS
024300030314     C                   KFLD                    vacLNP
024400030314     C                   KFLD                    vacNRS
024500030314     C                   KFLD                    vacNSP
024600030314     C                   KFLD                    ta4TRC
024700030314     C* Chiave su FNEVB05L - parziale
024800030314     C     KEYevbP       KLIST
024900030314     C                   KFLD                    vacAAS
025000030314     C                   KFLD                    vacLNP
025100030314     C                   KFLD                    vacNRS
025200030314     C                   KFLD                    vacNSP
025300030314     C*
025400030314     C* Carico le tabelle occorrenti
025500030314     C                   EXSR      cartab
025600030314     C*
025700991027     C                   ENDSR
