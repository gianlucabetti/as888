000100890921     H DECEDIT('0,') DATEDIT(*DMY.)
000200991027     H dftactgrp(*yes)
000300991027
000400080924     FTIVGDTMP  uF   E             DISK
000500020603     Ffnvac1wt  O    E             DISK
000600060529     FFNLBL01L  IF   E           K DISK
000700060529     FFNLBL02L  IF   E           K DISK    rename(fnlbl000:fnlbl002)
000800060529     FTITAS30C  IF   E           K DISK
000900991027
001000080924     D fnvac00t      e ds
001100991027     D prmppt          s             50
001200991027     D prmesito        s              1
001300991027     D wrkesito        s                   like(prmesito)
001400021008     D*-------------------
001500021008     D* COSTANTI
001600021008     D*-------------------
001700021008     D charOK          C                   const('ABCDEFGHIJKLMNOPQRSTUVWXYZ-
001800021008     D                                     abcdefghijklmnopqrstuvwxyz1234567-
001900021008     D                                     890,;.:-_òçà°ù§+*èé!"£$%&/()=?ì\|')
002000021008     D*-------------------
002100021008     D* SCHIERE DI WRK
002200021008     D*-------------------
002300021008     D SKcharOK        S              1    DIM(92)
002400991027
002500021008
002600021008
002700021008     C                   MOVEA     charOK        SKcharOK
002800991027     C                   EXSR      traduci
002900921023     C                   SETON                                        LR
003000991027
003100991027     C     traduci       BEGSR
003200991027     C*
003300080924     C                   READ      TIVGDTMP
003400080924     C                   dow       not %eof(TIVGDTMP)
003500080924     C*
003600080924     C                   EVAL      fnvac00t = vgdDTA
003700060529     C*
003800060529     C                   SETOFF                                       31
003900991027     C*
004000941221     C                   MOVE      VACAAS        VHCAAS
004100941221     C                   MOVE      VACLNP        VHCLNP
004200941221     C                   MOVE      VACNRS        VHCNRS
004300941221     C                   MOVE      VACNSP        VHCNSP
004400941221     C                   MOVE      VACMGS        VHCMGS
004500941221     C                   MOVE      VACCBO        VHCCBO
004600941221     C                   MOVE      VACLNA        VHCLNA
004700941221     C                   MOVE      VACRSD        VHCRSD
004800021008     C* Effettuo considerazioni sui caratteri della Ragione Sociale Destinatario
004900021008     C     1             DO        35            x                 2 0
005000021008     C                   MOVEL     *blanks       wrkCHAR           1
005100021008     C                   EVAL      wrkCHAR = %subst(VHCRSD:x:1)
005200021008     C                   IF        wrkCHAR <> *blanks AND
005300021008     C                             wrkCHAR <> ''''
005400021008     C     wrkCHAR       LOOKUP    SKcharOK                               20
005500021008     C                   IF        *in20 = *off
005600021008     C                   EVAL      %subst(VHCRSD:x:1) = *blanks
005700021008     C                   ENDIF
005800021008     C                   ENDIF
005900021008     C                   ENDDO
006000021008     C*
006100941221     C                   MOVE      VACPRD        VHCPRD
006200941221     C                   MOVE      VACGC1        VHCGC1
006300941221     C                   MOVE      VACGC2        VHCGC2
006400941221     C                   MOVE      VACCTR        VHCCTR
006500941221     C                   MOVE      VACCTS        VHCCTS
006600941221     C                   MOVE      VACFTM        VHCFTM
006700941221     C                   MOVE      VACFIN        VHCFIN
006800941221     C                   MOVE      VACFAP        VHCFAP
006900941221     C                   MOVE      VACTSP        VHCTSP
007000020603     C                   MOVE      VACIAS        VHCIAS
007100941221     C                   MOVE      VACVAS        VHCVAS
007200941221     C                   MOVE      VACNAS        VHCNAS
007300941221     C                   MOVE      VACNCL        VHCNCL
007400020603     C                   MOVE      VACPKB        VHCPKB
007500020603     C                   MOVE      VACVLB        VHCVLB
007600020603     C                   MOVE      VACQFT        VHCQFT
007700941221     C                   MOVE      VACTIC        VHCTIC
007800020603     C                   MOVE      VACCAS        VHCCAS
007900941221     C                   MOVE      VACVCA        VHCVCA
008000941221     C                   MOVE      VACCCM        VHCCCM
008100941221     C                   MOVE      VACRMN        VHCRMN
008200941221     C                   MOVE      VACRMA        VHCRMA
008300941221     C                   MOVE      VACRMO        VHCRMO
008400941221     C                   MOVE      VACFFD        VHCFFD
008500941221     C                   MOVE      VACTCR        VHCTCR
008600941221     C                   MOVE      VACDCR        VHCDCR
008700941221     C                   MOVE      VACHCR        VHCHCR
008800941221     C                   MOVE      VACDCM        VHCDCM
008900941221     C                   MOVE      VACHMC        VHCHMC
009000941221     C                   MOVE      VACTC1        VHCTC1
009100941221     C                   MOVE      VACTC2        VHCTC2
009200941221     C                   MOVE      VACCCA        VHCCCA
009300070725     C*
009400070725     C* Sopprimo gli zeri iniziali nn significativi
009500070725     C                   EVAL      VHCRMN = %editc(VACRMN:'4')
009600070726     C                   IF        %len(%trim(VHCRMN)) = 8
009700070726     C                   EVALR     VHCRMN = '0' + %trim(VHCRMN)
009800070726     C                   ENDIF
009900060529     C*
010000060529     C* Se richiesto nei parametri traduzione effettuo le seguenti considerazioni:
010100060529     C* - no bolle figlie
010200060529     C* - no bolle mamme se CCA=1 e bolle figlie legate NN CONS. o NN RESI
010300060619     C* - no bolle mamme se CCA=2 e bolle figlie legate NN CONS. o NN RESI
010400060619     C                   MOVEL     *blanks       wTip              1
010500060619     C                   EVAL      wTip = %subst(prmppt:1:1)
010600060619     C                   IF        wTip <> *blanks
010700060619     C* Se CCA=2 e richiesta forzatura relativa nei parametri azzero la data consegna merce
010800060619     C                   IF        VHCCCA='2' and (wTip='2' or wTip='3')
010900060619     C                   MOVE      *zeros        VHCDCM
011000060619     C                   ENDIF
011100060529     C* Verifico se trattasi d bolla "figlia"
011200060529     C     KEYlbl01C     CHAIN     FNLBL01L
011300060529     C                   IF        %found(FNLBL01L)
011400060529     C                   SETON                                        31
011500060529     C                   ELSE
011600060619     C* Se CCA=1 o CCA=2 => verifico stato attuale dell'ultima bolla figlia legata
011700060619     C                   IF        (VACCCA='1' and wTip='1') or
011800060619     C                             (VACCCA='2' and wTip='2') or
011900060619     C                             ((VACCCA='1' or VACCCA='2') and wTip='3')
012000060529     C     KEYlbl01C     CHAIN     FNLBL02L
012100060529     C                   IF        %found(FNLBL02L)
012200060529     C     KEYlbl02P     SETLL     FNLBL02L
012300060529     C                   IF        %equal(FNLBL02L)
012400060529     C     KEYlbl02P     READE     FNLBL02L
012500060529     C                   DOW       not %eof(FNLBL02L)
012600060529     C     KEYlbl02P     READE     FNLBL02L
012700060529     C                   ENDDO
012800060529     C                   ENDIF
012900060529     C* A questo punto con la chiave dell'ultima bolla figlia letta verifico il suo stato attuale
013000060529     C     KEYlbl02P     CHAIN     TITAS30C
013100060529     C                   IF        %found(TITAS30C)
013200060529     C                   IF        tasDCM > *zeros AND
013300060529     C                             (tasCCA = *blanks OR tasCCA = '2')
013400060529     C* Se ultima bolla figlia chiusa con consegna OK o reso => forzo lo stato d quest'ultima
013500060529     C* sulla bolla originale
013600060529     C                   EVAL      VHCCCA = tasCCA
013700060630     C* Verifica e forzatura x casi d resi d dirottamenti
013800060630     C                   IF        tasFBR = 'S'
013900060630     C                   EVAL      VHCCCA = '2'
014000060630     C                   ENDIF
014100060529     C                   ELSE
014200060703 xxx C***                SETON                                        31
014300060529     C                   ENDIF
014400060529     C                   ENDIF
014500060529     C                   ENDIF
014600060529     C                   ENDIF
014700060529     C                   ENDIF
014800060529     C                   ENDIF
014900060529     C*
015000941221     C                   MOVE      VACDLA        VHCDLA
015100941221     C                   MOVE      VACDAG        VHCDAG
015200930409     C*
015300060529     C  N31              WRITE     FNVACAA
015400080924     C                   delete    TIVGDTMP
015500991027     C*
015600080924     C                   READ      TIVGDTMP
015700991027     C                   ENDdo
015800991027     C*
015900991027     C                   eval      wrkesito = '0'
016000991027     C*
016100910830     C                   ENDSR
016200991027
016300991027      /TITLE Operazioni iniziali.
016400991027     C     *inzsr        BEGSR
016500991027     C*
016600991027     C     *ENTRY        PLIST
016700991027     C                   parm                    prmppt
016800991027     C     wrkesito      parm      wrkesito      prmesito
016900060529     C*
017000060529     C* Chiave su FNLBL01L - completa
017100060529     C     KEYlbl01C     KLIST
017200060529     C                   KFLD                    vacAAS
017300060529     C                   KFLD                    vacLNP
017400060529     C                   KFLD                    vacNRS
017500060529     C                   KFLD                    vacNSP
017600060529     C*
017700060529     C     KEYlbl02P     KLIST
017800060529     C                   KFLD                    lblAAN
017900060529     C                   KFLD                    lblLPN
018000060529     C                   KFLD                    lblNRN
018100060529     C                   KFLD                    lblNSN
018200991027     C*
018300991027     C                   ENDSR
