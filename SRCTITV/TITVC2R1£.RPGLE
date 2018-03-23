000100040723     H DECEDIT('0.') DATEDIT(*DMY.)
000200991027     H dftactgrp(*yes)
000300991027
000400040526     FTIVLT01L  UF A E           K DISK
000500080924     FTIVGDTMP  UF   E             DISK
000600040617     FFNVACXLS  UF A E             DISK
000700070524     Ftivawwwt  UF A E             DISK
000800991027     Ffnvacwwt  O    E             DISK
000900040526     Ftis7prgf  uf   e             disk    RENAME(tis7prgf:tis7prg0)
001000040526     F                                     PREFIX(f_)
001100040526     F                                     USROPN
001200991027
001300991027     D prmppt          s             50
001400991027     D prmesito        s              1
001500040526     D prmksc          s              7
001600040526     D prmmbr          s             10
001700991027     D wrkesito        s                   like(prmesito)
001800040526     D wISV            s              2    INZ(*blanks)
001900040526     D                 ds                  INZ
002000040526     D wrkday                  1      2s 0
002100040526     D wrkdaychr                      2    overlay(wrkday)
002200040526     D wrkoggiiso      s               d
002300040526     D wrkmese         s              2  0
002400040526     D dwlprg          s             10    INZ(*all'0')
002500040526     D wrkprg          s              8  0 INZ(*zeros)
002600040526     D mc              s              1    dim(12) ctdata perrcd(12)
002700040617
002800080924     D fnvac00t      e ds
002900040526
003000040526     D*---------------------
003100040526     D* DS "XSRDA8" - CONTROLLA DATA (8)
003200040526     D*---------------------
003300040526     D WLBDA8          DS
003400040526     D  G08DAT                 1      8  0
003500040526     D  G08INV                 9     16  0
003600040526     D  G08ERR                17     17
003700040526     D  G08TGI                18     22  0
003800040526
003900040526     D*---------------------
004000941221     D* DS PER EDITARE I CAMPI NUMERICI IN ASCII
004100941221     D* 10+3
004200941221     D                 DS
004300941221     D  WS15                   1      1
004400941221     D  WI15                   2     11
004500991125     D  WV15                  12     12    inz(',')
004600941221     D  WD15                  13     15
004700941221     D  WASC15                 1     15
004800941221     D*  6+1
004900941221     D                 DS
005000941221     D  WS09                   1      1
005100941221     D  WI09                   2      7
005200991125     D  WV09                   8      8    inz(',')
005300941221     D  WD09                   9      9
005400941221     D  WASC09                 1      9
005500941221     D*  2+3
005600941221     D                 DS
005700941221     D  WS07                   1      1
005800941221     D  WI07                   2      3
005900991125     D  WV07                   4      4    inz(',')
006000941221     D  WD07                   5      7
006100941221     D  WASC07                 1      7
006200021007     D*-------------------
006300021007     D* COSTANTI
006400021007     D*-------------------
006500021007     D charOK          C                   const('ABCDEFGHIJKLMNOPQRSTUVWXYZ-
006600021007     D                                     abcdefghijklmnopqrstuvwxyz1234567-
006700021007     D                                     890,;.:-_òçà°ù§+*èé!"£$%&/()=?ì\|')
006800021007     D*-------------------
006900021007     D* SCHIERE DI WRK
007000021007     D*-------------------
007100021007     D SKcharOK        S              1    DIM(92)
007200021007
007300021007
007400021008     C                   MOVEA     charOK        SKcharOK
007500040617     C                   EXSR      chkRECFILMBR
007600991027     C                   EXSR      traduci
007700921023     C                   SETON                                        LR
007800040617
007900040617
008000040617
008100040617
008200040617     C     chkRECFILMBR  BEGSR
008300040617     C*
008400040617     C                   READ      FNVACXLS                               55
008500070524     C                   READ      TIVAWWWT                               56
008600040617     C*
008700040617     C                   ENDSR
008800040617
008900040617
009000040617
009100991027
009200991027     C     traduci       BEGSR
009300040617     C*
009400070524     C                   EVAL      VACXDTA = '"VACAAS"'+','+
009500070524     C                                       '"VACLNP"'+','+
009600070524     C                                       '"VACNRS"'+','+
009700070524     C                                       '"VACNSP"'+','+
009800070524     C                                       '"VACMGS"'+','+
009900070524     C                                       '"VACCBO"'+','+
010000070524     C                                       '"VACLNA"'+','+
010100070524     C                                       '"VACRSD"'+','+
010200070524     C                                       '"VACPRD"'+','+
010300070524     C                                       '"VACGC1"'+','+
010400070524     C                                       '"VACGC2"'+','+
010500070524     C                                       '"VACCTR"'+','+
010600070524     C                                       '"VACCTS"'+','+
010700070524     C                                       '"VACFTM"'+','+
010800070524     C                                       '"VACFIN"'+','+
010900070524     C                                       '"VACFAP"'+','+
011000070524     C                                       '"VACTSP"'+','+
011100070524     C                                       '"VACIAS"'+','+
011200070524     C                                       '"VACVAS"'+','+
011300070524     C                                       '"VACNAS"'+','+
011400070524     C                                       '"VACNCL"'+','+
011500070524     C                                       '"VACPKB"'+','+
011600070524     C                                       '"VACVLB"'+','+
011700070524     C                                       '"VACQFT"'+','+
011800070524     C                                       '"VACTIC"'+','+
011900070524     C                                       '"VACCAS"'+','+
012000070524     C                                       '"VACVCA"'+','+
012100070524     C                                       '"VACCCM"'+','+
012200070524     C                                       '"VACRMN"'+','+
012300070524     C                                       '"VACRMA"'+','+
012400070524     C                                       '"VACRMO"'+','+
012500070524     C                                       '"VACFFD"'+','+
012600070524     C                                       '"VACTCR"'+','+
012700070524     C                                       '"VACDCR"'+','+
012800070524     C                                       '"VACHCR"'+','+
012900070524     C                                       '"VACDCM"'+','+
013000070524     C                                       '"VACHMC"'+','+
013100070524     C                                       '"VACTC1"'+','+
013200070524     C                                       '"VACTC2"'+','+
013300070524     C                                       '"VACCCA"'+','+
013400070524     C                                       '"VACDLA"'+','+
013500070524     C                                       '"VACDAG"'
013600070524     C*
013700070524     C* Se richiesto nei parametri scrivo una prima riga con l'intestazione dei campi
013800070524     C                   IF        %subst(prmppt:8:1) = 'I' AND *IN55 = *ON
013900070524     C                   WRITE     FNVACX00
014000070524     C                   ENDIF
014100070524     C*
014200070524     C* Se richiesto nei parametri scrivo una prima riga con l'intestazione dei campi
014300070524     C***                IF        %subst(prmppt:8:1) = 'I' AND *IN55 = *ON
014400070524     C***                EVAL      VAWDTA = VACXDTA
014500070524     C***                WRITE     TIVAW000
014600070524     C***                ENDIF
014700991027     C*
014800080924     C                   READ      TIVGDTMP
014900080924     C                   dow       not %eof(TIVGDTMP)
015000080924     C*
015100080924     C                   EVAL      fnvac00t = vgdDTA
015200080924     C*
015300040723     C                   CLEAR                   FNVACAA
015400040723     C                   CLEAR                   FNVACX00
015500070524     C                   CLEAR                   TIVAW000
015600991027     C*
015700941221     C                   MOVE      VACAAS        VHCAAS
015800941221     C                   MOVE      VACLNP        VHCLNP
015900941221     C                   MOVE      VACNRS        VHCNRS
016000941221     C                   MOVE      VACNSP        VHCNSP
016100941221     C                   MOVE      VACMGS        VHCMGS
016200941221     C                   MOVE      VACCBO        VHCCBO
016300941221     C                   MOVE      VACLNA        VHCLNA
016400021008     C                   MOVE      VACRSD        VHCRSD
016500021007     C* Effettuo considerazioni sui caratteri della Ragione Sociale Destinatario
016600021007     C     1             DO        35            x                 2 0
016700021007     C                   MOVEL     *blanks       wrkCHAR           1
016800021008     C                   EVAL      wrkCHAR = %subst(VHCRSD:x:1)
016900021008     C                   IF        wrkCHAR <> *blanks AND
017000021008     C                             wrkCHAR <> ''''
017100021007     C     wrkCHAR       LOOKUP    SKcharOK                               20
017200021007     C                   IF        *in20 = *off
017300021008     C                   EVAL      %subst(VHCRSD:x:1) = *blanks
017400021007     C                   ENDIF
017500021007     C                   ENDIF
017600021007     C                   ENDDO
017700021007     C*
017800941221     C                   MOVE      VACPRD        VHCPRD
017900941221     C                   MOVE      VACGC1        VHCGC1
018000941221     C                   MOVE      VACGC2        VHCGC2
018100941221     C                   MOVE      VACCTR        VHCCTR
018200941221     C                   MOVE      VACCTS        VHCCTS
018300941221     C                   MOVE      VACFTM        VHCFTM
018400941221     C                   MOVE      VACFIN        VHCFIN
018500941221     C                   MOVE      VACFAP        VHCFAP
018600941221     C                   MOVE      VACTSP        VHCTSP
018700941221     C*
018800941221     C                   MOVE      VACIAS        WD15
018900941221     C                   MOVEL     VACIAS        WI15
019000941221     C                   MOVE      WASC15        VHCIAS
019100941221     C*
019200941221     C                   MOVE      VACVAS        VHCVAS
019300941221     C                   MOVE      VACNAS        VHCNAS
019400941221     C                   MOVE      VACNCL        VHCNCL
019500941221     C*
019600941221     C                   MOVE      VACPKB        WD09
019700941221     C                   MOVEL     VACPKB        WI09
019800941221     C                   MOVE      WASC09        VHCPKB
019900941221     C*
020000941221     C                   MOVE      VACVLB        WD07
020100941221     C                   MOVEL     VACVLB        WI07
020200941221     C                   MOVE      WASC07        VHCVLB
020300941221     C*
020400941221     C                   MOVE      VACQFT        WD15
020500941221     C                   MOVEL     VACQFT        WI15
020600941221     C                   MOVE      WASC15        VHCQFT
020700941221     C*
020800941221     C                   MOVE      VACTIC        VHCTIC
020900941221     C*
021000941221     C                   MOVE      VACCAS        WD15
021100941221     C                   MOVEL     VACCAS        WI15
021200941221     C                   MOVE      WASC15        VHCCAS
021300941221     C*
021400941221     C                   MOVE      VACVCA        VHCVCA
021500941221     C                   MOVE      VACCCM        VHCCCM
021600941221     C                   MOVE      VACRMN        VHCRMN
021700941221     C                   MOVE      VACRMA        VHCRMA
021800941221     C                   MOVE      VACRMO        VHCRMO
021900941221     C                   MOVE      VACFFD        VHCFFD
022000941221     C                   MOVE      VACTCR        VHCTCR
022100941221     C                   MOVE      VACDCR        VHCDCR
022200941221     C                   MOVE      VACHCR        VHCHCR
022300941221     C                   MOVE      VACDCM        VHCDCM
022400941221     C                   MOVE      VACHMC        VHCHMC
022500941221     C                   MOVE      VACTC1        VHCTC1
022600941221     C                   MOVE      VACTC2        VHCTC2
022700941221     C                   MOVE      VACCCA        VHCCCA
022800941221     C                   MOVE      VACDLA        VHCDLA
022900941221     C                   MOVE      VACDAG        VHCDAG
023000070524     C*
023100070524     C* Se richiesto nei parametri l'editazione dei campi data => gestisco
023200070524     C                   IF        %subst(prmppt:9:1) = 'D'
023300070524     C                   EVAL      VACXDTA = %trim(%editc(VACAAS:'Q'))+','+
023400070524     C                                       %trim(%editc(VACLNP:'Q'))+','+
023500070524     C                                       %trim(%editc(VACNRS:'Q'))+','+
023600070524     C                                       %trim(%editc(VACNSP:'Q'))+','+
023700070524     C                                       %trim(%editc(VACMGS:'Q'))+','+
023800070524     C                                          '"'+%trim(VACCBO)+'"'+','+
023900070524     C                                       %trim(%editc(VACLNA:'Q'))+','+
024000070524     C                                          '"'+%trim(VACRSD)+'"'+','+
024100070524     C                                          '"'+%trim(VACPRD)+'"'+','+
024200070524     C                                          '"'+%trim(VACGC1)+'"'+','+
024300070524     C                                          '"'+%trim(VACGC2)+'"'+','+
024400070524     C                                       %trim(%editc(VACCTR:'Q'))+','+
024500070524     C                                          '"'+%trim(VACCTS)+'"'+','+
024600070524     C                                          '"'+%trim(VACFTM)+'"'+','+
024700070524     C                                          '"'+%trim(VACFIN)+'"'+','+
024800070524     C                                          '"'+%trim(VACFAP)+'"'+','+
024900070524     C                                          '"'+%trim(VACTSP)+'"'+','+
025000070524     C                                       %trim(%editc(VACIAS:'Q'))+','+
025100070524     C                                          '"'+%trim(VACVAS)+'"'+','+
025200070524     C                                          '"'+%trim(VACNAS)+'"'+','+
025300070524     C                                       %trim(%editc(VACNCL:'Q'))+','+
025400070524     C                                       %trim(%editc(VACPKB:'Q'))+','+
025500070524     C                                       %trim(%editc(VACVLB:'Q'))+','+
025600070524     C                                       %trim(%editc(VACQFT:'Q'))+','+
025700070524     C                                          '"'+%trim(VACTIC)+'"'+','+
025800070524     C                                       %trim(%editc(VACCAS:'Q'))+','+
025900070524     C                                          '"'+%trim(VACVCA)+'"'+','+
026000070524     C                                       %trim(%editc(VACCCM:'Q'))+','+
026100070524     C                                       %trim(%editc(VACRMN:'Q'))+','+
026200070524     C                                          '"'+%trim(VACRMA)+'"'+','+
026300070524     C                                          '"'+%trim(VACRMO)+'"'+','+
026400070524     C                                          '"'+%trim(VACFFD)+'"'+','+
026500070524     C                                          '"'+%trim(VACTCR)+'"'+','+
026600070524     C                              %trim(%editw(VACDCR:'    /  /  '))+','+
026700070524     C                                       %trim(%editc(VACHCR:'Q'))+','+
026800070524     C                              %trim(%editw(VACDCM:'    /  /  '))+','+
026900070524     C                                       %trim(%editc(VACHMC:'Q'))+','+
027000070524     C                                          '"'+%trim(VACTC1)+'"'+','+
027100070524     C                                          '"'+%trim(VACTC2)+'"'+','+
027200070524     C                                          '"'+%trim(VACCCA)+'"'+','+
027300070524     C                              %trim(%editw(VACDLA:'    /  /  '))+','+
027400070524     C                              %trim(%editw(VACDAG:'    /  /  '))
027500070524     C                   ELSE
027600070524     C*
027700070524     C                   EVAL      VACXDTA = %trim(%editc(VACAAS:'Q'))+','+
027800070524     C                                       %trim(%editc(VACLNP:'Q'))+','+
027900070524     C                                       %trim(%editc(VACNRS:'Q'))+','+
028000070524     C                                       %trim(%editc(VACNSP:'Q'))+','+
028100070524     C                                       %trim(%editc(VACMGS:'Q'))+','+
028200070524     C                                          '"'+%trim(VACCBO)+'"'+','+
028300070524     C                                       %trim(%editc(VACLNA:'Q'))+','+
028400070524     C                                          '"'+%trim(VACRSD)+'"'+','+
028500070524     C                                          '"'+%trim(VACPRD)+'"'+','+
028600070524     C                                          '"'+%trim(VACGC1)+'"'+','+
028700070524     C                                          '"'+%trim(VACGC2)+'"'+','+
028800070524     C                                       %trim(%editc(VACCTR:'Q'))+','+
028900070524     C                                          '"'+%trim(VACCTS)+'"'+','+
029000070524     C                                          '"'+%trim(VACFTM)+'"'+','+
029100070524     C                                          '"'+%trim(VACFIN)+'"'+','+
029200070524     C                                          '"'+%trim(VACFAP)+'"'+','+
029300070524     C                                          '"'+%trim(VACTSP)+'"'+','+
029400070524     C                                       %trim(%editc(VACIAS:'Q'))+','+
029500070524     C                                          '"'+%trim(VACVAS)+'"'+','+
029600070524     C                                          '"'+%trim(VACNAS)+'"'+','+
029700070524     C                                       %trim(%editc(VACNCL:'Q'))+','+
029800070524     C                                       %trim(%editc(VACPKB:'Q'))+','+
029900070524     C                                       %trim(%editc(VACVLB:'Q'))+','+
030000070524     C                                       %trim(%editc(VACQFT:'Q'))+','+
030100070524     C                                          '"'+%trim(VACTIC)+'"'+','+
030200070524     C                                       %trim(%editc(VACCAS:'Q'))+','+
030300070524     C                                          '"'+%trim(VACVCA)+'"'+','+
030400070524     C                                       %trim(%editc(VACCCM:'Q'))+','+
030500070524     C                                       %trim(%editc(VACRMN:'Q'))+','+
030600070524     C                                          '"'+%trim(VACRMA)+'"'+','+
030700070524     C                                          '"'+%trim(VACRMO)+'"'+','+
030800070524     C                                          '"'+%trim(VACFFD)+'"'+','+
030900070524     C                                          '"'+%trim(VACTCR)+'"'+','+
031000070524     C                                       %trim(%editc(VACDCR:'Q'))+','+
031100070524     C                                       %trim(%editc(VACHCR:'Q'))+','+
031200070524     C                                       %trim(%editc(VACDCM:'Q'))+','+
031300070524     C                                       %trim(%editc(VACHMC:'Q'))+','+
031400070524     C                                          '"'+%trim(VACTC1)+'"'+','+
031500070524     C                                          '"'+%trim(VACTC2)+'"'+','+
031600070524     C                                          '"'+%trim(VACCCA)+'"'+','+
031700070524     C                                       %trim(%editc(VACDLA:'Q'))+','+
031800070524     C                                       %trim(%editc(VACDAG:'Q'))
031900070524     C                   ENDIF
032000930409     C*
032100040526     C* Effettuo determinate considerazioni x determinare in quale file scrivere il record corrente
032200040526     C* ...se evento "MESSA IN CONSEGNA" nn scrivo
032300040526     C                   IF        VACCCA = 'C'
032400040526     C                   ELSE
032500040526     C* ...se consegna ok scrivo il record nel file FNVACWWT membro 'M0'+cliente
032600041111     C*     solo spedizioni con serie. RIC. Venerini 10/11/04
032700060323      *     TUTTE le spedizioni richiesta Peretti 23/03/2006
032800040526     C                   IF        VACDCM > *zeros   AND
032900060323     C                             (VACCCA = *blanks or VACCCA = '1')
033000060323     C**!!!                        VACnrs <> *zeros
033100040526     C                   WRITE     FNVACAA
033200040723     C                   ENDIF
033300040617     C*
033400040617     C* ...se stato intermedio scrivo il record nel file FNVACXLS membro 'M0'+cliente+'A'
033500040617     C                   WRITE     FNVACX00
033600070622     C*
033700070622     C* Scrivo anche il 3° file (richiesta del 21/05/2007 Burani Ambra x Ced063)
033800070622     C                   EVAL      VAWDTA = VACXDTA
033900070622     C                   WRITE     TIVAW000
034000040526     C                   ENDIF
034100070622     C*
034200070622     C* Scrivo sempre e cmq il 3° file "totale" (richiesta del 21/05/2007 Burani Ambra x Ced063)
034300070622     C***                EVAL      VAWDTA = VACXDTA
034400070622     C***                WRITE     TIVAW000
034500040526     C*
034600040526     C* In ogni caso storicizzo il record e deleto l'input
034700080924     C                   DELETE    TIVGD000
034800991027     C*
034900080924     C                   READ      TIVGDTMP
035000040526     C                   ENDDO
035100991027     C*
035200040526     C* Creo il record d log x il download del 2° file da invaire in altro modo
035300040526     C                   EXSR      CREALOGS
035400040526     C*
035500040526     C                   EVAL      wrkesito = '0'
035600991027     C*
035700910830     C                   ENDSR
035800040526
035900040526
036000040526
036100040526     C     CREALOGS      BEGSR
036200040526     C*
036300040526     C* Valorizzo i campi comuni a tutti i reord di log da "clonare"
036400040526     C                   CLEAR                   TIVLT000
036500040526     C                   EVAL      wISV = 'OE'
036600040526     C                   EVAL      vltKSC = '0' + prmksc
036700040526     C                   EVAL      vltKSU = '0' + prmksc
036800040526     C                   EVAL      vltMBD = prmmbr
036900040526     C                   EVAL      vltSND = 'D'
037000040526     C                   EVAL      vltFLS = 'FNVAC00S'
037100040526     C*
037200040526     C                   time                    wrkoggiiso
037300040526     C                   extrct    wrkoggiiso:*m wrkmese
037400040526     C                   extrct    wrkoggiiso:*d wrkday
037500040526     C                   eval      vltMBS =
037600040526     C                             mc(wrkmese)
037700040526     C                             + wrkdaychr
037800040526     C                             + %subst(vltKSU:2:7)
037900040526     C*
038000040526     C                   EVAL      vltDAT = DATCOR
038100040526     C                   EVAL      vltORA = ORACOR
038200040526     C                   EVAL      vltISV = wISV
038300040526     C                   EVAL      vltTIA = 'F'
038400040526     C                   EVAL      vltFL2 = '01'
038500040526     C                   EVAL      vltSTS = '0'
038600040526     C*
038700040526     C* => scrivo record di log x tipo file 01 (x ARIANNA 2001)
038800040526     C                   EXSR      calprog
038900040526     C                   EVAL      vltPRG = dwlprg
039000040526     C                   EVAL      vltTIP = '01'
039100040617     C                   EVAL      vltFLD = 'FNVACXLS'
039200040526     C                   WRITE     TIVLT000
039300070524     C*
039400070524     C* => scrivo record di log x tipo file 03 (tutti stati consegna via ftp)
039500080801     C                   EVAL      wISV = 'OB'
039600070524     C                   EXSR      calprog
039700070608     C                   EVAL      vltISV = wISV
039800070524     C                   EVAL      vltPRG = dwlprg
039900070524     C                   EVAL      vltTIP = '03'
040000070524     C                   EVAL      vltFLD = 'TIVAWWWT'
040100070524     C                   WRITE     TIVLT000
040200040526     C*
040300040526     C                   ENDSR
040400040526
040500040526
040600040526
040700040526      /TITLE Valorizzazione Progressivo Applicazione
040800040526     C     calprog       begsr
040900040526     C*
041000040526     C                   open      tis7prgf
041100040526     C*
041200040526     C                   read(e)   tis7prgf
041300040526     C                   if        not %error
041400040526     C                   eval      dwlprg = f_tis7prgf
041500040526     C*
041600040526     C                   move(p)   dwlprg        wrkprg
041700040526     C                   add       1             wrkprg
041800040526     C                   move(p)   wrkprg        dwlprg
041900040526     C                   movel     wISV          dwlprg
042000040526     C*
042100040526     C                   eval      f_tis7prgf = dwlprg
042200040526     C                   update    tis7prg0
042300040526     C                   endif
042400040526     C*
042500040526     C                   close     tis7prgf
042600040526     C*
042700040526     C                   endsr
042800040526
042900040526
043000040526
043100991027
043200991027      /TITLE Operazioni iniziali.
043300991027     C     *inzsr        BEGSR
043400040526     C*
043500040526     C     *ENTRY        PLIST
043600040526     C                   parm                    prmppt
043700040526     C     wrkesito      parm      wrkesito      prmesito
043800040526     C                   parm                    prmksc
043900040526     C                   parm                    prmmbr
044000040526     C*
044100040526     C* IMPOSTA LA DATA CORRENTE
044200040526     C                   TIME                    N14              14 0
044300040526     C                   MOVE      N14           N8                8 0
044400040526     C                   MOVEL     N14           ORACOR            6 0
044500040526     C                   Z-ADD     N8            G08DAT
044600040526     C                   Z-ADD     *ZEROS        G08INV
044700040526     C                   MOVEL     '0'           G08ERR
044800040526     C                   CALL      'XSRDA8'
044900040526     C                   PARM                    WLBDA8
045000040526     C                   Z-ADD     G08INV        DATCOR            8 0
045100991027     C*
045200991027     C                   ENDSR
045300040526** ctdata mc
045400040526ABCDEFGHILMN
