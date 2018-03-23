000100890921     H DECEDIT('0,') DATEDIT(*DMY.)
000200090326     H DFTACTGRP(*NO) ACTGRP(*CALLER)
000300991027
000400050720     FFNBLT01L  IF   E           K DISK
000500030924     FTITAH30C  IF   E           K DISK
000600080710     FTITAS30C  IF   E           K DISK
000700080710     FTITAT30C  IF   E           K DISK
000800080924     FTIVGDTMP  uF   E             DISK
000900090326     FTIVAWWWT  O    E             DISK    commit
001000991027
001100080924     D fnvac00t      e ds
001200991027     D prmppt          s             50
001300991027     D prmesito        s              1
001400991027     D wrkesito        s                   like(prmesito)
001500080710     D FNVACDW2T     E DS                  INZ
001600131115     D TRUL28DS      E DS
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
013800030925     C                   MOVEL     '.'           VHCNUL
013900080710     C*
014000080710     C* Aggancio innanzitutto la bolla
014100080710     C     KEYtasP       CHAIN     titas30c
014200080710     C                   if        %found(titas30c)
014300100923     C*
014400100923     C* Mi posiziono subito sul file dettagli segnacolli cliente
014500100923     C                   SETOFF                                       70
014600100923     C                   EVAL      tahTRC = 'C'
014700100923     C     KEYtahC       SETLL     titah30c
014800100923     C                   IF        %equal(titah30c)
014900100923     C                   SETON                                        70
015000100923     C                   ENDIF
015100080710     C*
015200080710     C* Effettuo considerazioni su segnacolli sequenziali e nn
015300080710     C                   if        tasFNS = 'S'
015400080710     C*
015500080710     C* Se nn sequenziali aggancio il segnacollo Bartolini (da TITAT)
015600080710     C                   exsr      exeTITAT
015700080710     C*
015800080710     C                   else
015900080710     C*
016000080710     C* Se sequenziali effettuo sviluppo da segnacollo "da" x <n> colli
016100080710     C                   exsr      exeSEGNSEQ
016200080710     C*
016300080710     C                   endif
016400080710     C                   endif
016500930409     C*
016600080924     C                   delete    TIVGD000
016700991027     C*
016800080924     C                   READ      TIVGDTMP
016900991027     C                   ENDdo
017000991027     C*
017100991027     C                   eval      wrkesito = '0'
017200991027     C*
017300910830     C                   ENDSR
017400080710
017500080710
017600080710
017700080710     C     exeTITAT      BEGSR
017800131115     C*
017900131115     C* Inizializzo variabili di wrk
018000131115     C                   setoff                                       80
018100131115     C                   movel     *blanks       wSEGNAC_BRT      18
018200080710     C*
018300080710     C* Aggancio il segnacollo Bartolini
018400080710     C     KEYtasP       SETLL     titat30c
018500080710     C                   IF        %equal(titat30c)
018600080710     C     KEYtasP       READE     titat30c
018700080710     C                   DOW       not %eof(titat30c)
018800080710     C*
018900080710     C* Se richiesto nei parametri imposto anche il segnacollo Bartolini
019000080710     C                   IF        %subst(prmppt:1:1) = 'S'
019100080710     C                   MOVE(P)   TATNSC        VHCNNCB
019200131115     C*
019300131115     C* Se richiesto segnacollo completo => compongo
019400131115     C                   IF        %subst(prmppt:2:1) = 'S'
019500131115     C                   seton                                        80
019600131115     C*
019700131115     C* Compongo il segnacollo BRT completo (calcolando anche il check-digits)
019800131115     C                   clear                   TRUL28DS
019900131115     C                   eval      I28TLA = 'E'
020000131115     C                   eval      I28MOD = 'BAR'
020100131115     C                   eval      I28COD = %editc(tasFLS:'X')   +
020200131115     C                                      %editc(vacLNA:'X')   +
020300131115     C                                      %editc(vacNRS:'X')   +
020400171122     C***                                   %editc(wBartNcd:'X') +
020500171122     C                                      %editc(tatnsc:'X') +
020600131115     C                                      %editc(tasZNC:'X')
020700131115     C                   call      'TRUL28R1'
020800131115     C                   parm                    TRUL28DS
020900131115     C                   if        O28ERR = *blanks
021000131115     C                   eval      wSEGNAC_BRT = %trim(O28COD)
021100131115     C                   else
021200131115     C                   eval      wSEGNAC_BRT = *all'*'
021300131115     C                   endif
021400131115     C*
021500131115     C                   ENDIF
021600131115     C*
021700080710     C                   ENDIF
021800080710     C*
021900080710     C* Quindi aggancio il dettaglio colli (da TITAH)
022000100923     C   70              EXSR      exeTITAH
022100080710     C*
022200131115     C  N80              EVAL      VAWDTA = FNVACDW2T
022300131115     C   80              EVAL      VAWDTA = FNVACDW2T+wSEGNAC_BRT
022400080710     C                   WRITE     TIVAW000
022500080710     C     KEYtasP       READE     titat30c
022600080710     C                   ENDDO
022700080710     C                   ELSE
022800080710     C                   EVAL      VAWDTA = FNVACDW2T
022900080710     C                   WRITE     TIVAW000
023000080710     C                   ENDIF
023100080710     C*
023200080710     C                   ENDSR
023300080710
023400080710
023500080710
023600080710     C     exeSEGNSEQ    BEGSR
023700080710     C*
023800131115     C* Inizializzo variabili di wrk
023900131115     C                   setoff                                       80
024000131115     C                   movel     *blanks       wSEGNAC_BRT      18
024100131115     C*
024200080710     C                   z-add     tasNCD        wBartNcd          7 0
024300080710     C                   dow       wBartNcd <= tasNCD + tasNCL - 1
024400080710     C*
024500080710     C* Se richiesto nei parametri imposto anche il segnacollo Bartolini
024600080710     C                   IF        %subst(prmppt:1:1) = 'S'
024700080710     C                   MOVE(P)   wBartNcd      VHCNNCB
024800131115     C*
024900131115     C* Se richiesto segnacollo completo => compongo
025000131115     C                   IF        %subst(prmppt:2:1) = 'S'
025100131115     C                   seton                                        80
025200131115     C*
025300131115     C* Compongo il segnacollo BRT completo (calcolando anche il check-digits)
025400131115     C                   clear                   TRUL28DS
025500131115     C                   eval      I28TLA = 'E'
025600131115     C                   eval      I28MOD = 'BAR'
025700131115     C                   eval      I28COD = %editc(tasFLS:'X')   +
025800131115     C                                      %editc(vacLNA:'X')   +
025900131115     C                                      %editc(vacNRS:'X')   +
026000131115     C                                      %editc(wBartNcd:'X') +
026100131115     C                                      %editc(tasZNC:'X')
026200131115     C                   call      'TRUL28R1'
026300131115     C                   parm                    TRUL28DS
026400131115     C                   if        O28ERR = *blanks
026500131115     C                   eval      wSEGNAC_BRT = %trim(O28COD)
026600131115     C                   else
026700131115     C                   eval      wSEGNAC_BRT = *all'*'
026800131115     C                   endif
026900131115     C*
027000131115     C                   ENDIF
027100131115     C*
027200080710     C                   ENDIF
027300080710     C*
027400080710     C* Quindi aggancio il dettaglio colli (da TITAH)
027500100923     C   70              EXSR      exeTITAH
027600080710     C*
027700131115     C  N80              EVAL      VAWDTA = FNVACDW2T
027800131115     C   80              EVAL      VAWDTA = FNVACDW2T+wSEGNAC_BRT
027900080710     C                   WRITE     TIVAW000
028000080710     C*
028100080710     C                   eval      wBartNcd = wBartNcd + 1
028200080710     C                   enddo
028300080710     C*
028400080710     C                   ENDSR
028500080710
028600080710
028700080710
028800080710     C     exeTITAH      BEGSR
028900080710     C*
029000100923     C* Quindi leggo il dettaglio colli corrente
029100100923     C     KEYtahC       READE     titah30c
029200080710     C                   IF        %found(titah30c)
029300080710     C                   EVAL      VHCNNCC = tahNOT
029400080710     C*
029500080710     C* X ogni segnacollo d default imposto la data consegna merce presente in testata (VAC)
029600080710     C                   MOVE      VACDCM        VHCDCM
029700080710     C*
029800080710     C* Se riesco considero la data consegna merce d ogni singolo collo
029900080710     C                   EVAL      bltNSC = tahNSC
030000080710     C     KEYbltC       CHAIN     fnblt01l
030100080710     C                   IF        %found(fnblt01l)
030200080710     C                   IF        bltDCM > *zeros
030300080710     C                   MOVE      bltDCM        VHCDCM
030400080710     C                   ENDIF
030500080710     C                   ENDIF
030600080710     C*
030700080710     C                   ENDIF
030800080710     C*
030900080710     C                   ENDSR
031000080710
031100080710
031200991027
031300991027      /TITLE Operazioni iniziali.
031400991027     C     *inzsr        BEGSR
031500991027     C*
031600991027     C     *ENTRY        PLIST
031700991027     C                   parm                    prmppt
031800991027     C     wrkesito      parm      wrkesito      prmesito
031900080710     C*
032000080710     C* Chiave su TITAS30C/TITAT30C - parziale
032100080710     C     KEYtasP       KLIST
032200080710     C                   KFLD                    vacAAS
032300080710     C                   KFLD                    vacLNP
032400080710     C                   KFLD                    vacNRS
032500080710     C                   KFLD                    vacNSP
032600030924     C*
032700030924     C* Chiave su TITAH30C - completa
032800030924     C     KEYtahC       KLIST
032900030924     C                   KFLD                    vacAAS
033000030924     C                   KFLD                    vacLNP
033100030924     C                   KFLD                    vacNRS
033200030924     C                   KFLD                    vacNSP
033300030924     C                   KFLD                    tahTRC
033400050720     C*
033500050720     C* Chiave su FNBLT01L - completa
033600050720     C     KEYbltC       KLIST
033700050720     C                   KFLD                    vacAAS
033800050720     C                   KFLD                    vacLNP
033900050720     C                   KFLD                    vacNRS
034000050720     C                   KFLD                    vacNSP
034100050720     C                   KFLD                    bltNSC
034200991027     C*
034300991027     C                   ENDSR
