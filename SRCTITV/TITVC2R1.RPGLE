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
001100991027
001200991027     D prmppt          s             50
001300991027     D prmesito        s              1
001400040526     D prmksc          s              7
001500040526     D prmmbr          s             10
001600991027     D wrkesito        s                   like(prmesito)
001700040526     D wISV            s              2    INZ(*blanks)
001800040526     D                 ds                  INZ
001900040526     D wrkday                  1      2s 0
002000040526     D wrkdaychr                      2    overlay(wrkday)
002100040526     D wrkoggiiso      s               d
002200040526     D wrkmese         s              2  0
002300040526     D dwlprg          s             10    INZ(*all'0')
002400040526     D wrkprg          s              8  0 INZ(*zeros)
002500040526     D mc              s              1    dim(12) ctdata perrcd(12)
002600040617
002700080924     D fnvac00t      e ds
002800040526
002900040526     D*---------------------
003000040526     D* DS "XSRDA8" - CONTROLLA DATA (8)
003100040526     D*---------------------
003200040526     D WLBDA8          DS
003300040526     D  G08DAT                 1      8  0
003400040526     D  G08INV                 9     16  0
003500040526     D  G08ERR                17     17
003600040526     D  G08TGI                18     22  0
003700040526
003800040526     D*---------------------
003900941221     D* DS PER EDITARE I CAMPI NUMERICI IN ASCII
004000941221     D* 10+3
004100941221     D                 DS
004200941221     D  WS15                   1      1
004300941221     D  WI15                   2     11
004400991125     D  WV15                  12     12    inz(',')
004500941221     D  WD15                  13     15
004600941221     D  WASC15                 1     15
004700941221     D*  6+1
004800941221     D                 DS
004900941221     D  WS09                   1      1
005000941221     D  WI09                   2      7
005100991125     D  WV09                   8      8    inz(',')
005200941221     D  WD09                   9      9
005300941221     D  WASC09                 1      9
005400941221     D*  2+3
005500941221     D                 DS
005600941221     D  WS07                   1      1
005700941221     D  WI07                   2      3
005800991125     D  WV07                   4      4    inz(',')
005900941221     D  WD07                   5      7
006000941221     D  WASC07                 1      7
006100021007     D*-------------------
006200021007     D* COSTANTI
006300021007     D*-------------------
006400021007     D charOK          C                   const('ABCDEFGHIJKLMNOPQRSTUVWXYZ-
006500021007     D                                     abcdefghijklmnopqrstuvwxyz1234567-
006600021007     D                                     890,;.:-_òçà°ù§+*èé!"£$%&/()=?ì\|')
006700021007     D*-------------------
006800021007     D* SCHIERE DI WRK
006900021007     D*-------------------
007000021007     D SKcharOK        S              1    DIM(92)
007100021007
007200021007
007300021008     C                   MOVEA     charOK        SKcharOK
007400040617     C                   EXSR      chkRECFILMBR
007500991027     C                   EXSR      traduci
007600921023     C                   SETON                                        LR
007700040617
007800040617
007900040617
008000040617
008100040617     C     chkRECFILMBR  BEGSR
008200040617     C*
008300040617     C                   READ      FNVACXLS                               55
008400070524     C                   READ      TIVAWWWT                               56
008500040617     C*
008600040617     C                   ENDSR
008700040617
008800040617
008900040617
009000991027
009100991027     C     traduci       BEGSR
009200040617     C*
009300070524     C                   EVAL      VACXDTA = '"VACAAS"'+','+
009400070524     C                                       '"VACLNP"'+','+
009500070524     C                                       '"VACNRS"'+','+
009600070524     C                                       '"VACNSP"'+','+
009700070524     C                                       '"VACMGS"'+','+
009800070524     C                                       '"VACCBO"'+','+
009900070524     C                                       '"VACLNA"'+','+
010000070524     C                                       '"VACRSD"'+','+
010100070524     C                                       '"VACPRD"'+','+
010200070524     C                                       '"VACGC1"'+','+
010300070524     C                                       '"VACGC2"'+','+
010400070524     C                                       '"VACCTR"'+','+
010500070524     C                                       '"VACCTS"'+','+
010600070524     C                                       '"VACFTM"'+','+
010700070524     C                                       '"VACFIN"'+','+
010800070524     C                                       '"VACFAP"'+','+
010900070524     C                                       '"VACTSP"'+','+
011000070524     C                                       '"VACIAS"'+','+
011100070524     C                                       '"VACVAS"'+','+
011200070524     C                                       '"VACNAS"'+','+
011300070524     C                                       '"VACNCL"'+','+
011400070524     C                                       '"VACPKB"'+','+
011500070524     C                                       '"VACVLB"'+','+
011600070524     C                                       '"VACQFT"'+','+
011700070524     C                                       '"VACTIC"'+','+
011800070524     C                                       '"VACCAS"'+','+
011900070524     C                                       '"VACVCA"'+','+
012000070524     C                                       '"VACCCM"'+','+
012100070524     C                                       '"VACRMN"'+','+
012200070524     C                                       '"VACRMA"'+','+
012300070524     C                                       '"VACRMO"'+','+
012400070524     C                                       '"VACFFD"'+','+
012500070524     C                                       '"VACTCR"'+','+
012600070524     C                                       '"VACDCR"'+','+
012700070524     C                                       '"VACHCR"'+','+
012800070524     C                                       '"VACDCM"'+','+
012900070524     C                                       '"VACHMC"'+','+
013000070524     C                                       '"VACTC1"'+','+
013100070524     C                                       '"VACTC2"'+','+
013200070524     C                                       '"VACCCA"'+','+
013300070524     C                                       '"VACDLA"'+','+
013400070524     C                                       '"VACDAG"'
013500070524     C*
013600070524     C* Se richiesto nei parametri scrivo una prima riga con l'intestazione dei campi
013700070524     C                   IF        %subst(prmppt:8:1) = 'I' AND *IN55 = *ON
013800070524     C                   WRITE     FNVACX00
013900070524     C                   ENDIF
014000070524     C*
014100070524     C* Se richiesto nei parametri scrivo una prima riga con l'intestazione dei campi
014200070524     C***                IF        %subst(prmppt:8:1) = 'I' AND *IN55 = *ON
014300070524     C***                EVAL      VAWDTA = VACXDTA
014400070524     C***                WRITE     TIVAW000
014500070524     C***                ENDIF
014600991027     C*
014700080924     C                   READ      TIVGDTMP
014800080924     C                   dow       not %eof(TIVGDTMP)
014900080924     C*
015000080924     C                   EVAL      fnvac00t = vgdDTA
015100080924     C*
015200040723     C                   CLEAR                   FNVACAA
015300040723     C                   CLEAR                   FNVACX00
015400070524     C                   CLEAR                   TIVAW000
015500991027     C*
015600941221     C                   MOVE      VACAAS        VHCAAS
015700941221     C                   MOVE      VACLNP        VHCLNP
015800941221     C                   MOVE      VACNRS        VHCNRS
015900941221     C                   MOVE      VACNSP        VHCNSP
016000941221     C                   MOVE      VACMGS        VHCMGS
016100941221     C                   MOVE      VACCBO        VHCCBO
016200941221     C                   MOVE      VACLNA        VHCLNA
016300021008     C                   MOVE      VACRSD        VHCRSD
016400021007     C* Effettuo considerazioni sui caratteri della Ragione Sociale Destinatario
016500021007     C     1             DO        35            x                 2 0
016600021007     C                   MOVEL     *blanks       wrkCHAR           1
016700021008     C                   EVAL      wrkCHAR = %subst(VHCRSD:x:1)
016800021008     C                   IF        wrkCHAR <> *blanks AND
016900021008     C                             wrkCHAR <> ''''
017000021007     C     wrkCHAR       LOOKUP    SKcharOK                               20
017100021007     C                   IF        *in20 = *off
017200021008     C                   EVAL      %subst(VHCRSD:x:1) = *blanks
017300021007     C                   ENDIF
017400021007     C                   ENDIF
017500021007     C                   ENDDO
017600021007     C*
017700941221     C                   MOVE      VACPRD        VHCPRD
017800941221     C                   MOVE      VACGC1        VHCGC1
017900941221     C                   MOVE      VACGC2        VHCGC2
018000941221     C                   MOVE      VACCTR        VHCCTR
018100941221     C                   MOVE      VACCTS        VHCCTS
018200941221     C                   MOVE      VACFTM        VHCFTM
018300941221     C                   MOVE      VACFIN        VHCFIN
018400941221     C                   MOVE      VACFAP        VHCFAP
018500941221     C                   MOVE      VACTSP        VHCTSP
018600941221     C*
018700941221     C                   MOVE      VACIAS        WD15
018800941221     C                   MOVEL     VACIAS        WI15
018900941221     C                   MOVE      WASC15        VHCIAS
019000941221     C*
019100941221     C                   MOVE      VACVAS        VHCVAS
019200941221     C                   MOVE      VACNAS        VHCNAS
019300941221     C                   MOVE      VACNCL        VHCNCL
019400941221     C*
019500941221     C                   MOVE      VACPKB        WD09
019600941221     C                   MOVEL     VACPKB        WI09
019700941221     C                   MOVE      WASC09        VHCPKB
019800941221     C*
019900941221     C                   MOVE      VACVLB        WD07
020000941221     C                   MOVEL     VACVLB        WI07
020100941221     C                   MOVE      WASC07        VHCVLB
020200941221     C*
020300941221     C                   MOVE      VACQFT        WD15
020400941221     C                   MOVEL     VACQFT        WI15
020500941221     C                   MOVE      WASC15        VHCQFT
020600941221     C*
020700941221     C                   MOVE      VACTIC        VHCTIC
020800941221     C*
020900941221     C                   MOVE      VACCAS        WD15
021000941221     C                   MOVEL     VACCAS        WI15
021100941221     C                   MOVE      WASC15        VHCCAS
021200941221     C*
021300941221     C                   MOVE      VACVCA        VHCVCA
021400941221     C                   MOVE      VACCCM        VHCCCM
021500941221     C                   MOVE      VACRMN        VHCRMN
021600941221     C                   MOVE      VACRMA        VHCRMA
021700941221     C                   MOVE      VACRMO        VHCRMO
021800941221     C                   MOVE      VACFFD        VHCFFD
021900941221     C                   MOVE      VACTCR        VHCTCR
022000941221     C                   MOVE      VACDCR        VHCDCR
022100941221     C                   MOVE      VACHCR        VHCHCR
022200941221     C                   MOVE      VACDCM        VHCDCM
022300941221     C                   MOVE      VACHMC        VHCHMC
022400941221     C                   MOVE      VACTC1        VHCTC1
022500941221     C                   MOVE      VACTC2        VHCTC2
022600941221     C                   MOVE      VACCCA        VHCCCA
022700941221     C                   MOVE      VACDLA        VHCDLA
022800941221     C                   MOVE      VACDAG        VHCDAG
022900070524     C*
023000070524     C* Se richiesto nei parametri l'editazione dei campi data => gestisco
023100070524     C                   IF        %subst(prmppt:9:1) = 'D'
023200070524     C                   EVAL      VACXDTA = %trim(%editc(VACAAS:'Q'))+','+
023300070524     C                                       %trim(%editc(VACLNP:'Q'))+','+
023400070524     C                                       %trim(%editc(VACNRS:'Q'))+','+
023500070524     C                                       %trim(%editc(VACNSP:'Q'))+','+
023600070524     C                                       %trim(%editc(VACMGS:'Q'))+','+
023700070524     C                                          '"'+%trim(VACCBO)+'"'+','+
023800070524     C                                       %trim(%editc(VACLNA:'Q'))+','+
023900070524     C                                          '"'+%trim(VACRSD)+'"'+','+
024000070524     C                                          '"'+%trim(VACPRD)+'"'+','+
024100070524     C                                          '"'+%trim(VACGC1)+'"'+','+
024200070524     C                                          '"'+%trim(VACGC2)+'"'+','+
024300070524     C                                       %trim(%editc(VACCTR:'Q'))+','+
024400070524     C                                          '"'+%trim(VACCTS)+'"'+','+
024500070524     C                                          '"'+%trim(VACFTM)+'"'+','+
024600070524     C                                          '"'+%trim(VACFIN)+'"'+','+
024700070524     C                                          '"'+%trim(VACFAP)+'"'+','+
024800070524     C                                          '"'+%trim(VACTSP)+'"'+','+
024900070524     C                                       %trim(%editc(VACIAS:'Q'))+','+
025000070524     C                                          '"'+%trim(VACVAS)+'"'+','+
025100070524     C                                          '"'+%trim(VACNAS)+'"'+','+
025200070524     C                                       %trim(%editc(VACNCL:'Q'))+','+
025300070524     C                                       %trim(%editc(VACPKB:'Q'))+','+
025400070524     C                                       %trim(%editc(VACVLB:'Q'))+','+
025500070524     C                                       %trim(%editc(VACQFT:'Q'))+','+
025600070524     C                                          '"'+%trim(VACTIC)+'"'+','+
025700070524     C                                       %trim(%editc(VACCAS:'Q'))+','+
025800070524     C                                          '"'+%trim(VACVCA)+'"'+','+
025900070524     C                                       %trim(%editc(VACCCM:'Q'))+','+
026000070524     C                                       %trim(%editc(VACRMN:'Q'))+','+
026100070524     C                                          '"'+%trim(VACRMA)+'"'+','+
026200070524     C                                          '"'+%trim(VACRMO)+'"'+','+
026300070524     C                                          '"'+%trim(VACFFD)+'"'+','+
026400070524     C                                          '"'+%trim(VACTCR)+'"'+','+
026500070524     C                              %trim(%editw(VACDCR:'    /  /  '))+','+
026600070524     C                                       %trim(%editc(VACHCR:'Q'))+','+
026700070524     C                              %trim(%editw(VACDCM:'    /  /  '))+','+
026800070524     C                                       %trim(%editc(VACHMC:'Q'))+','+
026900070524     C                                          '"'+%trim(VACTC1)+'"'+','+
027000070524     C                                          '"'+%trim(VACTC2)+'"'+','+
027100070524     C                                          '"'+%trim(VACCCA)+'"'+','+
027200070524     C                              %trim(%editw(VACDLA:'    /  /  '))+','+
027300070524     C                              %trim(%editw(VACDAG:'    /  /  '))
027400070524     C                   ELSE
027500070524     C*
027600070524     C                   EVAL      VACXDTA = %trim(%editc(VACAAS:'Q'))+','+
027700070524     C                                       %trim(%editc(VACLNP:'Q'))+','+
027800070524     C                                       %trim(%editc(VACNRS:'Q'))+','+
027900070524     C                                       %trim(%editc(VACNSP:'Q'))+','+
028000070524     C                                       %trim(%editc(VACMGS:'Q'))+','+
028100070524     C                                          '"'+%trim(VACCBO)+'"'+','+
028200070524     C                                       %trim(%editc(VACLNA:'Q'))+','+
028300070524     C                                          '"'+%trim(VACRSD)+'"'+','+
028400070524     C                                          '"'+%trim(VACPRD)+'"'+','+
028500070524     C                                          '"'+%trim(VACGC1)+'"'+','+
028600070524     C                                          '"'+%trim(VACGC2)+'"'+','+
028700070524     C                                       %trim(%editc(VACCTR:'Q'))+','+
028800070524     C                                          '"'+%trim(VACCTS)+'"'+','+
028900070524     C                                          '"'+%trim(VACFTM)+'"'+','+
029000070524     C                                          '"'+%trim(VACFIN)+'"'+','+
029100070524     C                                          '"'+%trim(VACFAP)+'"'+','+
029200070524     C                                          '"'+%trim(VACTSP)+'"'+','+
029300070524     C                                       %trim(%editc(VACIAS:'Q'))+','+
029400070524     C                                          '"'+%trim(VACVAS)+'"'+','+
029500070524     C                                          '"'+%trim(VACNAS)+'"'+','+
029600070524     C                                       %trim(%editc(VACNCL:'Q'))+','+
029700070524     C                                       %trim(%editc(VACPKB:'Q'))+','+
029800070524     C                                       %trim(%editc(VACVLB:'Q'))+','+
029900070524     C                                       %trim(%editc(VACQFT:'Q'))+','+
030000070524     C                                          '"'+%trim(VACTIC)+'"'+','+
030100070524     C                                       %trim(%editc(VACCAS:'Q'))+','+
030200070524     C                                          '"'+%trim(VACVCA)+'"'+','+
030300070524     C                                       %trim(%editc(VACCCM:'Q'))+','+
030400070524     C                                       %trim(%editc(VACRMN:'Q'))+','+
030500070524     C                                          '"'+%trim(VACRMA)+'"'+','+
030600070524     C                                          '"'+%trim(VACRMO)+'"'+','+
030700070524     C                                          '"'+%trim(VACFFD)+'"'+','+
030800070524     C                                          '"'+%trim(VACTCR)+'"'+','+
030900070524     C                                       %trim(%editc(VACDCR:'Q'))+','+
031000070524     C                                       %trim(%editc(VACHCR:'Q'))+','+
031100070524     C                                       %trim(%editc(VACDCM:'Q'))+','+
031200070524     C                                       %trim(%editc(VACHMC:'Q'))+','+
031300070524     C                                          '"'+%trim(VACTC1)+'"'+','+
031400070524     C                                          '"'+%trim(VACTC2)+'"'+','+
031500070524     C                                          '"'+%trim(VACCCA)+'"'+','+
031600070524     C                                       %trim(%editc(VACDLA:'Q'))+','+
031700070524     C                                       %trim(%editc(VACDAG:'Q'))
031800070524     C                   ENDIF
031900930409     C*
032000040526     C* Effettuo determinate considerazioni x determinare in quale file scrivere il record corrente
032100040526     C* ...se evento "MESSA IN CONSEGNA" nn scrivo
032200040526     C                   IF        VACCCA = 'C'
032300040526     C                   ELSE
032400040526     C* ...se consegna ok scrivo il record nel file FNVACWWT membro 'M0'+cliente
032500041111     C*     solo spedizioni con serie. RIC. Venerini 10/11/04
032600060323      *     TUTTE le spedizioni richiesta Peretti 23/03/2006
032700040526     C                   IF        VACDCM > *zeros   AND
032800060323     C                             (VACCCA = *blanks or VACCCA = '1')
032900060323     C**!!!                        VACnrs <> *zeros
033000040526     C                   WRITE     FNVACAA
033100040723     C                   ENDIF
033200040617     C*
033300040617     C* ...se stato intermedio scrivo il record nel file FNVACXLS membro 'M0'+cliente+'A'
033400040617     C                   WRITE     FNVACX00
033500070622     C*
033600070622     C* Scrivo anche il 3° file (richiesta del 21/05/2007 Burani Ambra x Ced063)
033700070622     C                   EVAL      VAWDTA = VACXDTA
033800070622     C                   WRITE     TIVAW000
033900040526     C                   ENDIF
034000070622     C*
034100070622     C* Scrivo sempre e cmq il 3° file "totale" (richiesta del 21/05/2007 Burani Ambra x Ced063)
034200070622     C***                EVAL      VAWDTA = VACXDTA
034300070622     C***                WRITE     TIVAW000
034400040526     C*
034500040526     C* In ogni caso storicizzo il record e deleto l'input
034600080924     C                   DELETE    TIVGD000
034700991027     C*
034800080924     C                   READ      TIVGDTMP
034900040526     C                   ENDDO
035000991027     C*
035100040526     C* Creo il record d log x il download del 2° file da invaire in altro modo
035200040526     C                   EXSR      CREALOGS
035300040526     C*
035400040526     C                   EVAL      wrkesito = '0'
035500991027     C*
035600910830     C                   ENDSR
035700040526
035800040526
035900040526
036000040526     C     CREALOGS      BEGSR
036100040526     C*
036200040526     C* Valorizzo i campi comuni a tutti i reord di log da "clonare"
036300040526     C                   CLEAR                   TIVLT000
036400040526     C                   EVAL      wISV = 'OE'
036500040526     C                   EVAL      vltKSC = '0' + prmksc
036600040526     C                   EVAL      vltKSU = '0' + prmksc
036700040526     C                   EVAL      vltMBD = prmmbr
036800040526     C                   EVAL      vltSND = 'D'
036900040526     C                   EVAL      vltFLS = 'FNVAC00S'
037000040526     C*
037100040526     C                   time                    wrkoggiiso
037200040526     C                   extrct    wrkoggiiso:*m wrkmese
037300040526     C                   extrct    wrkoggiiso:*d wrkday
037400040526     C                   eval      vltMBS =
037500040526     C                             mc(wrkmese)
037600040526     C                             + wrkdaychr
037700040526     C                             + %subst(vltKSU:2:7)
037800040526     C*
037900040526     C                   EVAL      vltDAT = DATCOR
038000040526     C                   EVAL      vltORA = ORACOR
038100040526     C                   EVAL      vltISV = wISV
038200040526     C                   EVAL      vltTIA = 'F'
038300040526     C                   EVAL      vltFL2 = '01'
038400040526     C                   EVAL      vltSTS = '0'
038500040526     C*
038600040526     C* => scrivo record di log x tipo file 01 (x ARIANNA 2001)
038700040526     C                   EXSR      calprog
038800040526     C                   EVAL      vltPRG = dwlprg
038900040526     C                   EVAL      vltTIP = '01'
039000040617     C                   EVAL      vltFLD = 'FNVACXLS'
039100040526     C                   WRITE     TIVLT000
039200070524     C*
039300070524     C* => scrivo record di log x tipo file 03 (tutti stati consegna via ftp)
039400080801     C                   EVAL      wISV = 'OB'
039500070524     C                   EXSR      calprog
039600070608     C                   EVAL      vltISV = wISV
039700170324     C                   EVAL      vltPRG = f_tis7prgf
039800070524     C                   EVAL      vltTIP = '03'
039900070524     C                   EVAL      vltFLD = 'TIVAWWWT'
040000070524     C                   WRITE     TIVLT000
040100040526     C*
040200040526     C                   ENDSR
040300040526
040400040526
040500040526
040600040526      /TITLE Valorizzazione Progressivo Applicazione
040700040526     C     calprog       begsr
040800040526     C*
040900170324     C     *start        setll     tis7prgf
041000040526     C                   read(e)   tis7prgf
041100040526     C                   if        not %error
041200040526     C                   eval      dwlprg = f_tis7prgf
041300040526     C*
041400040526     C                   move(p)   dwlprg        wrkprg
041500040526     C                   add       1             wrkprg
041600040526     C                   move(p)   wrkprg        dwlprg
041700040526     C                   movel     wISV          dwlprg
041800040526     C*
041900040526     C                   eval      f_tis7prgf = dwlprg
042000040526     C                   update    tis7prg0
042100170324     C                   else
042200170324     C                   eval      f_tis7prgf = 'XX00000000'
042300040526     C                   endif
042400040526     C*
042500040526     C                   endsr
042600040526
042700040526
042800040526
042900991027
043000991027      /TITLE Operazioni iniziali.
043100991027     C     *inzsr        BEGSR
043200040526     C*
043300040526     C     *ENTRY        PLIST
043400040526     C                   parm                    prmppt
043500040526     C     wrkesito      parm      wrkesito      prmesito
043600040526     C                   parm                    prmksc
043700040526     C                   parm                    prmmbr
043800040526     C*
043900040526     C* IMPOSTA LA DATA CORRENTE
044000040526     C                   TIME                    N14              14 0
044100040526     C                   MOVE      N14           N8                8 0
044200040526     C                   MOVEL     N14           ORACOR            6 0
044300040526     C                   Z-ADD     N8            G08DAT
044400040526     C                   Z-ADD     *ZEROS        G08INV
044500040526     C                   MOVEL     '0'           G08ERR
044600040526     C                   CALL      'XSRDA8'
044700040526     C                   PARM                    WLBDA8
044800040526     C                   Z-ADD     G08INV        DATCOR            8 0
044900991027     C*
045000991027     C                   ENDSR
045100040526** ctdata mc
045200040526ABCDEFGHILMN
