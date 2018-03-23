000100890921     H DECEDIT('0,') DATEDIT(*DMY.)
000200090326     H DFTACTGRP(*NO) ACTGRP(*CALLER)
000300991027
000400130430     FAZORG01L  IF   E           K DISK
000500050720     FFNBLT01L  IF   E           K DISK
000600030924     FTITAH30C  IF   E           K DISK
000700080710     FTITAS30C  IF   E           K DISK
000800080710     FTITAT30C  IF   E           K DISK
000900080924     FTIVGDTMP  uF   E             DISK
001000090326     FTIVAWWWT  O    E             DISK    commit
001100991027
001200080924     D fnvac00t      e ds
001300991027     D prmppt          s             50
001400991027     D prmesito        s              1
001500991027     D wrkesito        s                   like(prmesito)
001600080710     D FNVACDW2T     E DS                  INZ
001700080710     C*
001800941221     D* DS PER EDITARE I CAMPI NUMERICI IN ASCII
001900941221     D* 10+3
002000941221     D                 DS
002100941221     D  WS15                   1      1
002200941221     D  WI15                   2     11
002300991125     D  WV15                  12     12    inz(',')
002400941221     D  WD15                  13     15
002500941221     D  WASC15                 1     15
002600941221     D*  6+1
002700941221     D                 DS
002800941221     D  WS09                   1      1
002900941221     D  WI09                   2      7
003000991125     D  WV09                   8      8    inz(',')
003100941221     D  WD09                   9      9
003200941221     D  WASC09                 1      9
003300941221     D*  2+3
003400941221     D                 DS
003500941221     D  WS07                   1      1
003600941221     D  WI07                   2      3
003700991125     D  WV07                   4      4    inz(',')
003800941221     D  WD07                   5      7
003900941221     D  WASC07                 1      7
004000021007     D*-------------------
004100021007     D* COSTANTI
004200021007     D*-------------------
004300021007     D charOK          C                   const('ABCDEFGHIJKLMNOPQRSTUVWXYZ-
004400021007     D                                     abcdefghijklmnopqrstuvwxyz1234567-
004500021007     D                                     890,;.:-_òçà°ù§+*èé!"£$%&/()=?ì\|')
004600021007     D*-------------------
004700021007     D* SCHIERE DI WRK
004800021007     D*-------------------
004900021007     D SKcharOK        S              1    DIM(92)
005000021007
005100021007
005200021008     C                   MOVEA     charOK        SKcharOK
005300991027     C                   EXSR      traduci
005400921023     C                   SETON                                        LR
005500991027
005600991027     C     traduci       BEGSR
005700991027     C*
005800080924     C                   READ      TIVGDTMP
005900080924     C                   dow       not %eof(TIVGDTMP)
006000080924     C*
006100080924     C                   eval      fnvac00t = vgdDTA
006200080710     C*
006300100923     C                   clear                   FNVACDW2T
006400991027     C*
006500941221     C                   MOVE      VACAAS        VHCAAS
006600941221     C                   MOVE      VACLNP        VHCLNP
006700941221     C                   MOVE      VACNRS        VHCNRS
006800941221     C                   MOVE      VACNSP        VHCNSP
006900941221     C                   MOVE      VACMGS        VHCMGS
007000941221     C                   MOVE      VACCBO        VHCCBO
007100941221     C                   MOVE      VACLNA        VHCLNA
007200021008     C                   MOVE      VACRSD        VHCRSD
007300021007     C* Effettuo considerazioni sui caratteri della Ragione Sociale Destinatario
007400021007     C     1             DO        35            x                 2 0
007500021007     C                   MOVEL     *blanks       wrkCHAR           1
007600021008     C                   EVAL      wrkCHAR = %subst(VHCRSD:x:1)
007700021008     C                   IF        wrkCHAR <> *blanks AND
007800021008     C                             wrkCHAR <> ''''
007900021007     C     wrkCHAR       LOOKUP    SKcharOK                               20
008000021007     C                   IF        *in20 = *off
008100021008     C                   EVAL      %subst(VHCRSD:x:1) = *blanks
008200021007     C                   ENDIF
008300021007     C                   ENDIF
008400021007     C                   ENDDO
008500021007     C*
008600941221     C                   MOVE      VACPRD        VHCPRD
008700941221     C                   MOVE      VACGC1        VHCGC1
008800941221     C                   MOVE      VACGC2        VHCGC2
008900941221     C                   MOVE      VACCTR        VHCCTR
009000941221     C                   MOVE      VACCTS        VHCCTS
009100941221     C                   MOVE      VACFTM        VHCFTM
009200941221     C                   MOVE      VACFIN        VHCFIN
009300941221     C                   MOVE      VACFAP        VHCFAP
009400941221     C                   MOVE      VACTSP        VHCTSP
009500941221     C*
009600941221     C                   MOVE      VACIAS        WD15
009700941221     C                   MOVEL     VACIAS        WI15
009800941221     C                   MOVE      WASC15        VHCIAS
009900941221     C*
010000941221     C                   MOVE      VACVAS        VHCVAS
010100941221     C                   MOVE      VACNAS        VHCNAS
010200941221     C                   MOVE      VACNCL        VHCNCL
010300941221     C*
010400941221     C                   MOVE      VACPKB        WD09
010500941221     C                   MOVEL     VACPKB        WI09
010600941221     C                   MOVE      WASC09        VHCPKB
010700941221     C*
010800941221     C                   MOVE      VACVLB        WD07
010900941221     C                   MOVEL     VACVLB        WI07
011000941221     C                   MOVE      WASC07        VHCVLB
011100941221     C*
011200941221     C                   MOVE      VACQFT        WD15
011300941221     C                   MOVEL     VACQFT        WI15
011400941221     C                   MOVE      WASC15        VHCQFT
011500941221     C*
011600941221     C                   MOVE      VACTIC        VHCTIC
011700941221     C*
011800941221     C                   MOVE      VACCAS        WD15
011900941221     C                   MOVEL     VACCAS        WI15
012000941221     C                   MOVE      WASC15        VHCCAS
012100941221     C*
012200941221     C                   MOVE      VACVCA        VHCVCA
012300941221     C                   MOVE      VACCCM        VHCCCM
012400941221     C                   MOVE      VACRMN        VHCRMN
012500941221     C                   MOVE      VACRMA        VHCRMA
012600941221     C                   MOVE      VACRMO        VHCRMO
012700941221     C                   MOVE      VACFFD        VHCFFD
012800941221     C                   MOVE      VACTCR        VHCTCR
012900941221     C                   MOVE      VACDCR        VHCDCR
013000941221     C                   MOVE      VACHCR        VHCHCR
013100941221     C                   MOVE      VACDCM        VHCDCM
013200941221     C                   MOVE      VACHMC        VHCHMC
013300941221     C                   MOVE      VACTC1        VHCTC1
013400941221     C                   MOVE      VACTC2        VHCTC2
013500941221     C                   MOVE      VACCCA        VHCCCA
013600941221     C                   MOVE      VACDLA        VHCDLA
013700941221     C                   MOVE      VACDAG        VHCDAG
013800130430     C                   MOVEL     '.'           VHCNUL
013900130430     C*
014000130430     C* Reperisco la descrizione della linea di arrivo
014100130430     C                   MOVEL     *blanks       wLNADES          20
014200130430     C     vacLNA        CHAIN     azorg01l
014300130430     C                   IF        %found(azorg01l)
014400130430     C                   EVAL      wLNADES = orgDES
014500130430     C                   ENDIF
014600080710     C*
014700080710     C* Aggancio innanzitutto la bolla
014800080710     C     KEYtasP       CHAIN     titas30c
014900080710     C                   if        %found(titas30c)
015000100923     C*
015100100923     C* Mi posiziono subito sul file dettagli segnacolli cliente
015200100923     C                   SETOFF                                       70
015300100923     C                   EVAL      tahTRC = 'C'
015400100923     C     KEYtahC       SETLL     titah30c
015500100923     C                   IF        %equal(titah30c)
015600100923     C                   SETON                                        70
015700100923     C                   ENDIF
015800080710     C*
015900080710     C* Effettuo considerazioni su segnacolli sequenziali e nn
016000080710     C                   if        tasFNS = 'S'
016100080710     C*
016200080710     C* Se nn sequenziali aggancio il segnacollo Bartolini (da TITAT)
016300080710     C                   exsr      exeTITAT
016400080710     C*
016500080710     C                   else
016600080710     C*
016700080710     C* Se sequenziali effettuo sviluppo da segnacollo "da" x <n> colli
016800080710     C                   exsr      exeSEGNSEQ
016900080710     C*
017000080710     C                   endif
017100080710     C                   endif
017200930409     C*
017300080924     C                   delete    TIVGD000
017400991027     C*
017500080924     C                   READ      TIVGDTMP
017600991027     C                   ENDdo
017700991027     C*
017800991027     C                   eval      wrkesito = '0'
017900991027     C*
018000910830     C                   ENDSR
018100080710
018200080710
018300080710
018400080710     C     exeTITAT      BEGSR
018500080710     C*
018600080710     C* Aggancio il segnacollo Bartolini
018700080710     C     KEYtasP       SETLL     titat30c
018800080710     C                   IF        %equal(titat30c)
018900080710     C     KEYtasP       READE     titat30c
019000080710     C                   DOW       not %eof(titat30c)
019100080710     C*
019200080710     C* Se richiesto nei parametri imposto anche il segnacollo Bartolini
019300080710     C                   IF        %subst(prmppt:1:1) = 'S'
019400080710     C                   MOVE(P)   TATNSC        VHCNNCB
019500080710     C                   ENDIF
019600080710     C*
019700080710     C* Quindi aggancio il dettaglio colli (da TITAH)
019800100923     C   70              EXSR      exeTITAH
019900080710     C*
020000130430     C                   EVAL      VAWDTA = FNVACDW2T+wLNADES+VHCNUL
020100080710     C                   WRITE     TIVAW000
020200080710     C     KEYtasP       READE     titat30c
020300080710     C                   ENDDO
020400080710     C                   ELSE
020500130430     C                   EVAL      VAWDTA = FNVACDW2T+wLNADES+VHCNUL
020600080710     C                   WRITE     TIVAW000
020700080710     C                   ENDIF
020800080710     C*
020900080710     C                   ENDSR
021000080710
021100080710
021200080710
021300080710     C     exeSEGNSEQ    BEGSR
021400080710     C*
021500080710     C                   z-add     tasNCD        wBartNcd          7 0
021600080710     C                   dow       wBartNcd <= tasNCD + tasNCL - 1
021700080710     C*
021800080710     C* Se richiesto nei parametri imposto anche il segnacollo Bartolini
021900080710     C                   IF        %subst(prmppt:1:1) = 'S'
022000080710     C                   MOVE(P)   wBartNcd      VHCNNCB
022100080710     C                   ENDIF
022200080710     C*
022300080710     C* Quindi aggancio il dettaglio colli (da TITAH)
022400100923     C   70              EXSR      exeTITAH
022500080710     C*
022600130430     C                   EVAL      VAWDTA = FNVACDW2T+wLNADES+VHCNUL
022700080710     C                   WRITE     TIVAW000
022800080710     C*
022900080710     C                   eval      wBartNcd = wBartNcd + 1
023000080710     C                   enddo
023100080710     C*
023200080710     C                   ENDSR
023300080710
023400080710
023500080710
023600080710     C     exeTITAH      BEGSR
023700080710     C*
023800100923     C* Quindi leggo il dettaglio colli corrente
023900100923     C     KEYtahC       READE     titah30c
024000080710     C                   IF        %found(titah30c)
024100080710     C                   EVAL      VHCNNCC = tahNOT
024200080710     C*
024300080710     C* X ogni segnacollo d default imposto la data consegna merce presente in testata (VAC)
024400080710     C                   MOVE      VACDCM        VHCDCM
024500080710     C*
024600080710     C* Se riesco considero la data consegna merce d ogni singolo collo
024700080710     C                   EVAL      bltNSC = tahNSC
024800080710     C     KEYbltC       CHAIN     fnblt01l
024900080710     C                   IF        %found(fnblt01l)
025000080710     C                   IF        bltDCM > *zeros
025100080710     C                   MOVE      bltDCM        VHCDCM
025200080710     C                   ENDIF
025300080710     C                   ENDIF
025400080710     C*
025500080710     C                   ENDIF
025600080710     C*
025700080710     C                   ENDSR
025800080710
025900080710
026000991027
026100991027      /TITLE Operazioni iniziali.
026200991027     C     *inzsr        BEGSR
026300991027     C*
026400991027     C     *ENTRY        PLIST
026500991027     C                   parm                    prmppt
026600991027     C     wrkesito      parm      wrkesito      prmesito
026700080710     C*
026800080710     C* Chiave su TITAS30C/TITAT30C - parziale
026900080710     C     KEYtasP       KLIST
027000080710     C                   KFLD                    vacAAS
027100080710     C                   KFLD                    vacLNP
027200080710     C                   KFLD                    vacNRS
027300080710     C                   KFLD                    vacNSP
027400030924     C*
027500030924     C* Chiave su TITAH30C - completa
027600030924     C     KEYtahC       KLIST
027700030924     C                   KFLD                    vacAAS
027800030924     C                   KFLD                    vacLNP
027900030924     C                   KFLD                    vacNRS
028000030924     C                   KFLD                    vacNSP
028100030924     C                   KFLD                    tahTRC
028200050720     C*
028300050720     C* Chiave su FNBLT01L - completa
028400050720     C     KEYbltC       KLIST
028500050720     C                   KFLD                    vacAAS
028600050720     C                   KFLD                    vacLNP
028700050720     C                   KFLD                    vacNRS
028800050720     C                   KFLD                    vacNSP
028900050720     C                   KFLD                    bltNSC
029000991027     C*
029100991027     C                   ENDSR
