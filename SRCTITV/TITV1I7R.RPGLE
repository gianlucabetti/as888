000100890921     H DECEDIT('0,') DATEDIT(*DMY.)
000200991027     H dftactgrp(*yes)
000300991027
000400080924     FTIVGDTMP  uF   E             DISK
000500991027     Ffnvacwwt  O    E             DISK
000600050714     FFNLBL01L  IF   E           K DISK
000700050622     FFNLBL02L  IF   E           K DISK    rename(fnlbl000:fnlbl002)
000800050622     FTITAS30C  IF   E           K DISK
000900080910     FTIGCP51L  IF   E           K DISK
001000991027
001100080924     D fnvac00t      e ds
001200991027     D prmppt          s             50
001300991027     D prmesito        s              1
001400991027     D wrkesito        s                   like(prmesito)
001500941221     D* DS PER EDITARE I CAMPI NUMERICI IN ASCII
001600941221     D* 10+3
001700941221     D                 DS
001800941221     D  WS15                   1      1
001900941221     D  WI15                   2     11
002000991125     D  WV15                  12     12    inz(',')
002100941221     D  WD15                  13     15
002200941221     D  WASC15                 1     15
002300941221     D*  6+1
002400941221     D                 DS
002500941221     D  WS09                   1      1
002600941221     D  WI09                   2      7
002700991125     D  WV09                   8      8    inz(',')
002800941221     D  WD09                   9      9
002900941221     D  WASC09                 1      9
003000941221     D*  2+3
003100941221     D                 DS
003200941221     D  WS07                   1      1
003300941221     D  WI07                   2      3
003400991125     D  WV07                   4      4    inz(',')
003500941221     D  WD07                   5      7
003600941221     D  WASC07                 1      7
003700021007     D*-------------------
003800021007     D* COSTANTI
003900021007     D*-------------------
004000021007     D charOK          C                   const('ABCDEFGHIJKLMNOPQRSTUVWXYZ-
004100021007     D                                     abcdefghijklmnopqrstuvwxyz1234567-
004200021007     D                                     890,;.:-_òçà°ù§+*èé!"£$%&/()=?ì\|')
004300021007     D*-------------------
004400021007     D* SCHIERE DI WRK
004500021007     D*-------------------
004600021007     D SKcharOK        S              1    DIM(92)
004700021007
004800021007
004900021008     C                   MOVEA     charOK        SKcharOK
005000991027     C                   EXSR      traduci
005100921023     C                   SETON                                        LR
005200991027
005300991027     C     traduci       BEGSR
005400991027     C*
005500080924     C                   READ      TIVGDTMP
005600080924     C                   DOW       not %eof(TIVGDTMP)
005700080924     C*
005800080924     C                   EVAL      fnvac00t = vgdDTA
005900050622     C*
006000080910     C                   SETOFF                                       313233
006100080613     C*
006200080613     C* Se richiesto nei parametri d ingresso ritorno solamente le bolle originali
006300080613     C                   if        %subst(prmppt:7:1) = 'S'
006400080613     C                   if        VACNRS = *zeros
006500080613     C                   seton                                        31
006600080613     C                   endif
006700080613     C                   endif
006800080910     C*
006900080910     C* Se richiesto nei parametri d ingresso ritorno stati bolle in giacenza solo dopo
007000080910     C* chiusura della stessa
007100080910     C                   if        %subst(prmppt:8:1) = 'S'
007200080910     C                   seton                                        32
007300080910     C                   endif
007400080613     C*
007500080613     C* Se tutto ok => procedo
007600080910     C                   if        *in31 = *off
007700080910     C*
007800080910     C                   if        *in32 = *on
007900080910     C                   if        VACDAG > *zeros
008000080910     C                   eval      gcpFRG = *zeros
008100080910     C*
008200080910     C* Aggancio il file giacenze della bolla corrente
008300080910     C     KEYgcp51_P    chain     TIGCP51L
008400080910     C                   if        %found(TIGCP51L)
008500080910     C                   if        gcpFAS < 50
008600080910     C                   seton                                        31
008700080910     C                   seton                                        33
008800080910     C                   endif
008900080910     C                   endif
009000080910     C                   endif
009100080910     C                   endif
009200080910     C*
009300080910     C* Se tutto ok => procedo
009400080910     C                   if        *in31 = *off
009500991027     C*
009600941221     C                   MOVE      VACAAS        VHCAAS
009700941221     C                   MOVE      VACLNP        VHCLNP
009800941221     C                   MOVE      VACNRS        VHCNRS
009900941221     C                   MOVE      VACNSP        VHCNSP
010000941221     C                   MOVE      VACMGS        VHCMGS
010100941221     C                   MOVE      VACCBO        VHCCBO
010200941221     C                   MOVE      VACLNA        VHCLNA
010300021008     C                   MOVE      VACRSD        VHCRSD
010400021007     C* Effettuo considerazioni sui caratteri della Ragione Sociale Destinatario
010500021007     C     1             DO        35            x                 2 0
010600021007     C                   MOVEL     *blanks       wrkCHAR           1
010700021008     C                   EVAL      wrkCHAR = %subst(VHCRSD:x:1)
010800021008     C                   IF        wrkCHAR <> *blanks AND
010900021008     C                             wrkCHAR <> ''''
011000021007     C     wrkCHAR       LOOKUP    SKcharOK                               20
011100021007     C                   IF        *in20 = *off
011200021008     C                   EVAL      %subst(VHCRSD:x:1) = *blanks
011300021007     C                   ENDIF
011400021007     C                   ENDIF
011500021007     C                   ENDDO
011600021007     C*
011700941221     C                   MOVE      VACPRD        VHCPRD
011800941221     C                   MOVE      VACGC1        VHCGC1
011900941221     C                   MOVE      VACGC2        VHCGC2
012000941221     C                   MOVE      VACCTR        VHCCTR
012100941221     C                   MOVE      VACCTS        VHCCTS
012200941221     C                   MOVE      VACFTM        VHCFTM
012300941221     C                   MOVE      VACFIN        VHCFIN
012400941221     C                   MOVE      VACFAP        VHCFAP
012500941221     C                   MOVE      VACTSP        VHCTSP
012600941221     C*
012700941221     C                   MOVE      VACIAS        WD15
012800941221     C                   MOVEL     VACIAS        WI15
012900941221     C                   MOVE      WASC15        VHCIAS
013000941221     C*
013100941221     C                   MOVE      VACVAS        VHCVAS
013200941221     C                   MOVE      VACNAS        VHCNAS
013300941221     C                   MOVE      VACNCL        VHCNCL
013400941221     C*
013500941221     C                   MOVE      VACPKB        WD09
013600941221     C                   MOVEL     VACPKB        WI09
013700941221     C                   MOVE      WASC09        VHCPKB
013800941221     C*
013900941221     C                   MOVE      VACVLB        WD07
014000941221     C                   MOVEL     VACVLB        WI07
014100941221     C                   MOVE      WASC07        VHCVLB
014200941221     C*
014300941221     C                   MOVE      VACQFT        WD15
014400941221     C                   MOVEL     VACQFT        WI15
014500941221     C                   MOVE      WASC15        VHCQFT
014600941221     C*
014700941221     C                   MOVE      VACTIC        VHCTIC
014800941221     C*
014900941221     C                   MOVE      VACCAS        WD15
015000941221     C                   MOVEL     VACCAS        WI15
015100941221     C                   MOVE      WASC15        VHCCAS
015200941221     C*
015300941221     C                   MOVE      VACVCA        VHCVCA
015400941221     C                   MOVE      VACCCM        VHCCCM
015500941221     C                   MOVE      VACRMN        VHCRMN
015600941221     C                   MOVE      VACRMA        VHCRMA
015700941221     C                   MOVE      VACRMO        VHCRMO
015800941221     C                   MOVE      VACFFD        VHCFFD
015900941221     C                   MOVE      VACTCR        VHCTCR
016000941221     C                   MOVE      VACDCR        VHCDCR
016100941221     C                   MOVE      VACHCR        VHCHCR
016200941221     C                   MOVE      VACDCM        VHCDCM
016300941221     C                   MOVE      VACHMC        VHCHMC
016400941221     C                   MOVE      VACTC1        VHCTC1
016500941221     C                   MOVE      VACTC2        VHCTC2
016600941221     C                   MOVE      VACCCA        VHCCCA
016700050622     C*
016800050622     C* Se richiesto nei parametri traduzione effettuo le seguenti considerazioni:
016900050622     C* - no bolle figlie
017000050622     C* - no bolle mamme se CCA=1 e bolle figlie legate NN CONS. o NN RESI
017100060619     C* - no bolle mamme se CCA=2 e bolle figlie legate NN CONS. o NN RESI
017200060619     C                   MOVEL     *blanks       wTip              1
017300060619     C                   EVAL      wTip = %subst(prmppt:1:1)
017400060619     C                   IF        wTip <> *blanks
017500060619     C* Se CCA=2 e richiesta forzatura relativa nei parametri azzero la data consegna merce
017600060619     C                   IF        VHCCCA='2' and (wTip='2' or wTip='3')
017700060619     C                   MOVE      *zeros        VHCDCM
017800060619     C                   ENDIF
017900050622     C* Verifico se trattasi d bolla "figlia"
018000050622     C     KEYlbl01C     CHAIN     FNLBL01L
018100050622     C                   IF        %found(FNLBL01L)
018200050622     C                   SETON                                        31
018300050622     C                   ELSE
018400060619     C* Se CCA=1 o CCA=2 => verifico stato attuale dell'ultima bolla figlia legata
018500060619     C                   IF        (VACCCA='1' and wTip='1') or
018600060619     C                             (VACCCA='2' and wTip='2') or
018700060619     C                             ((VACCCA='1' or VACCCA='2') and wTip='3')
018800050622     C     KEYlbl01C     CHAIN     FNLBL02L
018900050622     C                   IF        %found(FNLBL02L)
019000050714     C     KEYlbl02P     SETLL     FNLBL02L
019100050714     C                   IF        %equal(FNLBL02L)
019200050714     C     KEYlbl02P     READE     FNLBL02L
019300050714     C                   DOW       not %eof(FNLBL02L)
019400050714     C     KEYlbl02P     READE     FNLBL02L
019500050622     C                   ENDDO
019600050714     C                   ENDIF
019700050622     C* A questo punto con la chiave dell'ultima bolla figlia letta verifico il suo stato attuale
019800050622     C     KEYlbl02P     CHAIN     TITAS30C
019900050622     C                   IF        %found(TITAS30C)
020000050622     C                   IF        tasDCM > *zeros AND
020100050622     C                             (tasCCA = *blanks OR tasCCA = '2')
020200050623     C* Se ultima bolla figlia chiusa con consegna OK o reso => forzo lo stato d quest'ultima
020300050623     C* sulla bolla originale
020400050623     C                   EVAL      VHCCCA = tasCCA
020500050622     C                   ELSE
020600050622     C                   SETON                                        31
020700050622     C                   ENDIF
020800050622     C                   ENDIF
020900050622     C                   ENDIF
021000050622     C                   ENDIF
021100050622     C                   ENDIF
021200050622     C                   ENDIF
021300050622     C*
021400941221     C                   MOVE      VACDLA        VHCDLA
021500941221     C                   MOVE      VACDAG        VHCDAG
021600930409     C*
021700050622     C  N31              WRITE     FNVACAA
021800080613     C                   endif
021900080910     C                   endif
022000080613     C*
022100080924     C  N33              delete    TIVGD000
022200991027     C*
022300080924     C                   READ      TIVGDTMP
022400991027     C                   ENDdo
022500991027     C*
022600991027     C                   eval      wrkesito = '0'
022700991027     C*
022800910830     C                   ENDSR
022900991027
023000991027      /TITLE Operazioni iniziali.
023100991027     C     *inzsr        BEGSR
023200991027     C*
023300991027     C     *ENTRY        PLIST
023400991027     C                   parm                    prmppt
023500991027     C     wrkesito      parm      wrkesito      prmesito
023600050622     C*
023700050622     C* Chiave su FNLBL01L - completa
023800050622     C     KEYlbl01C     KLIST
023900050622     C                   KFLD                    vacAAS
024000050622     C                   KFLD                    vacLNP
024100050622     C                   KFLD                    vacNRS
024200050622     C                   KFLD                    vacNSP
024300050622     C*
024400080910     C* Chiave su FNLBL02L - Parziale
024500050622     C     KEYlbl02P     KLIST
024600050622     C                   KFLD                    lblAAN
024700050622     C                   KFLD                    lblLPN
024800050622     C                   KFLD                    lblNRN
024900050622     C                   KFLD                    lblNSN
025000080910     C*
025100080910     C* Chiave su TIGCP51L - Parziale
025200080910     C     KEYgcp51_P    KLIST
025300080910     C                   KFLD                    vacAAS
025400080910     C                   KFLD                    vacLNP
025500080910     C                   KFLD                    vacNRS
025600080910     C                   KFLD                    vacNSP
025700080910     C                   KFLD                    gcpFRG
025800050622     C*
025900991027     C                   ENDSR
