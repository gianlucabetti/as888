000100890921     H DECEDIT('0,') DATEDIT(*DMY.)
000200991027     H dftactgrp(*yes)
000300991027
000400020508     Ftita432c  iF   E           K DISK    rename(tita4000:tita4032)
000500020508     F                                     rename(tita4P00:tita4P32)
000600020508     Ftitas30c  iF   E           K DISK
000700050909     FTIVGDTMP  UF   E             DISK
000800020508     Ffnvapfdx  O    E             DISK
000900991027
001000991027     D prmppt          s             50
001100991027     D prmesito        s              1
001200991027     D wrkesito        s                   like(prmesito)
001300021015     D dataISOda       s               D   datfmt(*ISO)
001400021015     D dataISOa        s               D   datfmt(*ISO)
001500021015     D numMESI         s              5i 0
001600050909     D fnvapds       e ds                  extname(fnvap00t)
001700020508
001800020508     D*------------
001900020508     D* DS di ridefinizione chiave esterna ORM
002000020508     D*------------
002100020508     D                 DS
002200050909     D x_vapPOE                1      3  0
002300050909     D x_vapNSR                4      5  0
002400050909     D x_vapNOR                6     12  0
002500050909     D x_vapNRV               13     14  0
002600020508     D keyORM                  1     14
002700991027
002800991027     C                   EXSR      traduci
002900921023     C                   SETON                                        LR
003000991027
003100991027     C     traduci       BEGSR
003200991027     C*
003300050909     C                   READ      TIVGDTMP
003400050909     C                   DOW       not %eof(TIVGDTMP)
003500050909     C                   eval      fnvapds = vgdDTA
003600991027     C*
003700020508     C* Elaboro solo le fasi 500/600/900/999
003800020508     C                   IF        vapFAR = 500 OR
003900020508     C                             vapFAR = 600 OR
004000020508     C                             vapFAR = 900 OR
004100020508     C                             vapFAR = 999
004200020508     C*
004300020508     C* Innanzitutto inizializzo il formato record del file tradotto
004400020508     C                   CLEAR                   FNVAPxxx
004500020508     C*
004600020508     C                   IF        %subst(%trim(vapRFA):1:1) = 'P'
004700020508     C                   EVAL      fdxMSGCD = 'PUR'
004800020508     C                   ENDIF
004900020508     C                   IF        %subst(%trim(vapRFA):1:1) = 'R'
005000020508     C                   EVAL      fdxMSGCD = 'ROC'
005100020508     C                   ENDIF
005200020508     C*
005300020508     C                   EVAL      fdxREGDT = '20' + %subst(%trim(vapRFA):2:6)
005400020508     C                   EVAL      fdxPKNO  = %subst(%trim(vapRFA):8:7)
005500020508     C*
005600020508     C                   EVAL      fdxSTNCD = 'BART '
005700020508     C*
005800021004     C* Stabilisco lo status del ritiro
005900021004     C                   IF        vapFAR = 600
006000021004     C                   EVAL      fdxSTSCD = 'RE'
006100021004     C                   ELSE
006200021004     C                   IF         vapFAR = 900   OR
006300021004     C                             (vapFAR = 999  AND
006400021004     C                              vapCAR = '82')
006500021004     C                   EVAL      fdxSTSCD = 'SB'
006600021004     C                   ELSE
006700021004     C                   IF        vapCAR <> '82'
006800021004     C                   EVAL      fdxSTSCD = vapCAR
006900021004     C                   ENDIF
007000021004     C                   ENDIF
007100021004     C                   ENDIF
007200020508     C*
007300020508     C                   MOVEL     vapDAE        fdxPKDT
007400020508     C                   MOVEL     vapORE        fdxPKTM
007500021015     C*
007600021015     C* Inizializzo il flag di storicizzazione
007700021015     C                   MOVEL     'N'           flgCHK            1
007800021015     C*
007900021015     C* Se trattasi d 1 status x cui occorre reperire l'AWB => reperisco la bolla corrispondente
008000021015     C                   IF        fdxSTSCD = 'RE' OR
008100021015     C                             fdxSTSCD = 'SB'
008200020508     C*
008300020508     C* Reperisco alcuni dati direttamente dalla bolla legata al ritiro
008400020508     C                   EXSR      repTAS
008500021015     C                   ELSE
008600021015     C                   EVAL      flgCHK = 'S'
008700021015     C                   WRITE     FNVAPxxx
008800021015     C                   ENDIF
008900020508     C*
009000021015     C                   ELSE
009100021015     C                   EVAL      flgCHK = 'S'
009200020910     C                   ENDIF
009300021015     C*
009400021015     C* Se comunque la fase ORM è + vecchia d 1 mese => storicizzzo cmq
009500021015     C     *ISO          MOVE      vapDAE        dataISOda
009600021015     C                   TIME                    dataISOa
009700021015     C     dataISOa      SUBDUR    dataISOda     numMESI:*M
009800021015     C                   IF        numMESI >= 1
009900021015     C                   EVAL      flgCHK = 'S'
010000021015     C                   ENDIF
010100020910     C*
010200021015     C                   IF        flgCHK = 'S'
010300050909     C                   DELETE    TIVGD000
010400021015     C                   ENDIF
010500020508     C*
010600050909     C                   READ      TIVGDTMP
010700021015     C                   ENDDO
010800991027     C*
010900021015     C                   EVAL      wrkesito = '0'
011000991027     C*
011100910830     C                   ENDSR
011200020508
011300020508
011400020508
011500020508
011600020508      /TITLE Reperimento dati dalle bolle
011700020508     C     REPTAS        BEGSR
011800020508     C*
011900021015     C* Effettuo prima lettura (e implicitamente posizionamento) "fuori ciclo"
012000050909     C                   EVAL      x_vapPOE = vapPOE
012100050909     C                   EVAL      x_vapNSR = vapNSR
012200050909     C                   EVAL      x_vapNOR = vapNOR
012300050909     C                   EVAL      x_vapNRV = vapNRV
012400020508     C                   EVAL      ta4n14 = keyORM
012500020508     C     KEYta432      CHAIN     tita432c
012600021015     C                   IF        %found(tita432c)
012700021015     C                   DOW       not %eof(tita432c)
012800020620     C     KEYtas30      CHAIN     titas30c
012900020620     C                   IF        %found(titas30c)
013000021015     C                   EVAL      flgCHK = 'S'
013100021015     C                   MOVE(P)   tasRMN        fdxAWBNO
013200021015     C                   MOVE(P)   tasNCL        fdxPKGS
013300021015     C                   WRITE     FNVAPxxx
013400021015     C                   ENDIF
013500021015     C     KEYta432      READE     tita432c
013600021015     C                   ENDDO
013700020508     C                   ENDIF
013800020508     C*
013900020508     C                   ENDSR
014000020508
014100020508
014200020508
014300991027
014400991027      /TITLE Operazioni iniziali.
014500991027     C     *inzsr        BEGSR
014600991027     C*
014700991027     C     *ENTRY        PLIST
014800991027     C                   parm                    prmppt
014900991027     C     wrkesito      parm      wrkesito      prmesito
015000020508     C*
015100020508     C* Definizione chiavi
015200020508     C*
015300020508     C* - TITAS30C => Incompleta
015400020620     C     KEYtas30      KLIST
015500020508     C                   KFLD                    ta4AAS
015600020508     C                   KFLD                    ta4LNP
015700020508     C                   KFLD                    ta4NRS
015800020508     C                   KFLD                    ta4NSP
015900020508     C*
016000020508     C* - TITA432C => Completa
016100020508     C     KEYta432      KLIST
016200020508     C                   KFLD                    ta4N14
016300991027     C*
016400991027     C                   ENDSR
