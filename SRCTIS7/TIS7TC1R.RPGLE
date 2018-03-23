000100890921     H DECEDIT('0,') DATEDIT(*DMY.)
000200090326     H DFTACTGRP(*NO) ACTGRP(*CALLER)
000300991027
000400080924     Ftivgdtmp  uF   E             DISK
000500090326     Ffnvac1wt  O    E             DISK    commit
000600060529     FFNLBL01L  IF   E           K DISK
000700060529     FFNLBL02L  IF   E           K DISK    rename(fnlbl000:fnlbl002)
000800060529     FTITAS30C  IF   E           K DISK
000900991027
001000991027     D prmppt          s             50
001100991027     D prmesito        s              1
001200991027     D wrkesito        s                   like(prmesito)
001300080924     D fnvacds       e ds                  extname(fnvac00t)
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
003300080924     C                   READ      tivgdtmp
003400991027     C                   dow       not %eof
003500080924     c                   movel     vgddta        fnvacds
003600060529     C*
003700060529     C                   SETOFF                                       31
003800991027     C*
003900941221     C                   MOVE      VACAAS        VHCAAS
004000941221     C                   MOVE      VACLNP        VHCLNP
004100941221     C                   MOVE      VACNRS        VHCNRS
004200941221     C                   MOVE      VACNSP        VHCNSP
004300941221     C                   MOVE      VACMGS        VHCMGS
004400941221     C                   MOVE      VACCBO        VHCCBO
004500941221     C                   MOVE      VACLNA        VHCLNA
004600941221     C                   MOVE      VACRSD        VHCRSD
004700021008     C* Effettuo considerazioni sui caratteri della Ragione Sociale Destinatario
004800021008     C     1             DO        35            x                 2 0
004900021008     C                   MOVEL     *blanks       wrkCHAR           1
005000021008     C                   EVAL      wrkCHAR = %subst(VHCRSD:x:1)
005100021008     C                   IF        wrkCHAR <> *blanks AND
005200021008     C                             wrkCHAR <> ''''
005300021008     C     wrkCHAR       LOOKUP    SKcharOK                               20
005400021008     C                   IF        *in20 = *off
005500021008     C                   EVAL      %subst(VHCRSD:x:1) = *blanks
005600021008     C                   ENDIF
005700021008     C                   ENDIF
005800021008     C                   ENDDO
005900021008     C*
006000941221     C                   MOVE      VACPRD        VHCPRD
006100941221     C                   MOVE      VACGC1        VHCGC1
006200941221     C                   MOVE      VACGC2        VHCGC2
006300941221     C                   MOVE      VACCTR        VHCCTR
006400941221     C                   MOVE      VACCTS        VHCCTS
006500941221     C                   MOVE      VACFTM        VHCFTM
006600941221     C                   MOVE      VACFIN        VHCFIN
006700941221     C                   MOVE      VACFAP        VHCFAP
006800941221     C                   MOVE      VACTSP        VHCTSP
006900020603     C                   MOVE      VACIAS        VHCIAS
007000941221     C                   MOVE      VACVAS        VHCVAS
007100941221     C                   MOVE      VACNAS        VHCNAS
007200941221     C                   MOVE      VACNCL        VHCNCL
007300020603     C                   MOVE      VACPKB        VHCPKB
007400020603     C                   MOVE      VACVLB        VHCVLB
007500020603     C                   MOVE      VACQFT        VHCQFT
007600941221     C                   MOVE      VACTIC        VHCTIC
007700020603     C                   MOVE      VACCAS        VHCCAS
007800941221     C                   MOVE      VACVCA        VHCVCA
007900941221     C                   MOVE      VACCCM        VHCCCM
008000941221     C                   MOVE      VACRMN        VHCRMN
008100941221     C                   MOVE      VACRMA        VHCRMA
008200941221     C                   MOVE      VACRMO        VHCRMO
008300941221     C                   MOVE      VACFFD        VHCFFD
008400941221     C                   MOVE      VACTCR        VHCTCR
008500941221     C                   MOVE      VACDCR        VHCDCR
008600941221     C                   MOVE      VACHCR        VHCHCR
008700941221     C                   MOVE      VACDCM        VHCDCM
008800941221     C                   MOVE      VACHMC        VHCHMC
008900941221     C                   MOVE      VACTC1        VHCTC1
009000941221     C                   MOVE      VACTC2        VHCTC2
009100941221     C                   MOVE      VACCCA        VHCCCA
009200060529     C*
009300060529     C* Se richiesto nei parametri traduzione effettuo le seguenti considerazioni:
009400060529     C* - no bolle figlie
009500060529     C* - no bolle mamme se CCA=1 e bolle figlie legate NN CONS. o NN RESI
009600060619     C* - no bolle mamme se CCA=2 e bolle figlie legate NN CONS. o NN RESI
009700060619     C                   MOVEL     *blanks       wTip              1
009800060619     C                   EVAL      wTip = %subst(prmppt:1:1)
009900060619     C                   IF        wTip <> *blanks
010000060619     C* Se CCA=2 e richiesta forzatura relativa nei parametri azzero la data consegna merce
010100060619     C                   IF        VHCCCA='2' and (wTip='2' or wTip='3')
010200060619     C                   MOVE      *zeros        VHCDCM
010300060619     C                   ENDIF
010400060529     C* Verifico se trattasi d bolla "figlia"
010500060529     C     KEYlbl01C     CHAIN     FNLBL01L
010600060529     C                   IF        %found(FNLBL01L)
010700060529     C                   SETON                                        31
010800060529     C                   ELSE
010900060619     C* Se CCA=1 o CCA=2 => verifico stato attuale dell'ultima bolla figlia legata
011000060619     C                   IF        (VACCCA='1' and wTip='1') or
011100060619     C                             (VACCCA='2' and wTip='2') or
011200060619     C                             ((VACCCA='1' or VACCCA='2') and wTip='3')
011300060529     C     KEYlbl01C     CHAIN     FNLBL02L
011400060529     C                   IF        %found(FNLBL02L)
011500060529     C     KEYlbl02P     SETLL     FNLBL02L
011600060529     C                   IF        %equal(FNLBL02L)
011700060529     C     KEYlbl02P     READE     FNLBL02L
011800060529     C                   DOW       not %eof(FNLBL02L)
011900060529     C     KEYlbl02P     READE     FNLBL02L
012000060529     C                   ENDDO
012100060529     C                   ENDIF
012200060529     C* A questo punto con la chiave dell'ultima bolla figlia letta verifico il suo stato attuale
012300060529     C     KEYlbl02P     CHAIN     TITAS30C
012400060529     C                   IF        %found(TITAS30C)
012500060529     C                   IF        tasDCM > *zeros AND
012600060529     C                             (tasCCA = *blanks OR tasCCA = '2')
012700060529     C* Se ultima bolla figlia chiusa con consegna OK o reso => forzo lo stato d quest'ultima
012800060529     C* sulla bolla originale
012900060529     C                   EVAL      VHCCCA = tasCCA
013000060630     C* Verifica e forzatura x casi d resi d dirottamenti
013100060630     C                   IF        tasFBR = 'S'
013200060630     C                   EVAL      VHCCCA = '2'
013300060630     C                   ENDIF
013400060529     C                   ELSE
013500060703 xxx C***                SETON                                        31
013600060529     C                   ENDIF
013700060529     C                   ENDIF
013800060529     C                   ENDIF
013900060529     C                   ENDIF
014000060529     C                   ENDIF
014100060529     C                   ENDIF
014200060529     C*
014300941221     C                   MOVE      VACDLA        VHCDLA
014400941221     C                   MOVE      VACDAG        VHCDAG
014500930409     C*
014600060529     C  N31              WRITE     FNVACAA
014700080924     C                   delete    tivgd000
014800991027     C*
014900080924     C                   READ      tivgdtmp
015000991027     C                   ENDdo
015100991027     C*
015200991027     C                   eval      wrkesito = '0'
015300991027     C*
015400910830     C                   ENDSR
015500991027
015600991027      /TITLE Operazioni iniziali.
015700991027     C     *inzsr        BEGSR
015800991027     C*
015900991027     C     *ENTRY        PLIST
016000991027     C                   parm                    prmppt
016100991027     C     wrkesito      parm      wrkesito      prmesito
016200060529     C*
016300060529     C* Chiave su FNLBL01L - completa
016400060529     C     KEYlbl01C     KLIST
016500060529     C                   KFLD                    vacAAS
016600060529     C                   KFLD                    vacLNP
016700060529     C                   KFLD                    vacNRS
016800060529     C                   KFLD                    vacNSP
016900060529     C*
017000060529     C     KEYlbl02P     KLIST
017100060529     C                   KFLD                    lblAAN
017200060529     C                   KFLD                    lblLPN
017300060529     C                   KFLD                    lblNRN
017400060529     C                   KFLD                    lblNSN
017500991027     C*
017600991027     C                   ENDSR
