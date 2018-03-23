000100040723     H DECEDIT('0.') DATEDIT(*DMY.)
000200991027     H dftactgrp(*yes)
000300991027
000400040526     FTIVLT01L  UF A E           K DISK
000500080924     FTIVGDTMP  UF   E             DISK
000600040617     FFNVACXLS  UF A E             DISK
000700991027     Ffnvacwwt  O    E             DISK
000800040526     Ftis7prgf  uf   e             disk    RENAME(tis7prgf:tis7prg0)
000900040526     F                                     PREFIX(f_)
001000040526     F                                     USROPN
001100991027
001200080924     D fnvac00t      e ds
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
002700040526
002800040526     D*---------------------
002900040526     D* DS "XSRDA8" - CONTROLLA DATA (8)
003000040526     D*---------------------
003100040526     D WLBDA8          DS
003200040526     D  G08DAT                 1      8  0
003300040526     D  G08INV                 9     16  0
003400040526     D  G08ERR                17     17
003500040526     D  G08TGI                18     22  0
003600040526
003700040526     D*---------------------
003800941221     D* DS PER EDITARE I CAMPI NUMERICI IN ASCII
003900941221     D* 10+3
004000941221     D                 DS
004100941221     D  WS15                   1      1
004200941221     D  WI15                   2     11
004300991125     D  WV15                  12     12    inz(',')
004400941221     D  WD15                  13     15
004500941221     D  WASC15                 1     15
004600941221     D*  6+1
004700941221     D                 DS
004800941221     D  WS09                   1      1
004900941221     D  WI09                   2      7
005000991125     D  WV09                   8      8    inz(',')
005100941221     D  WD09                   9      9
005200941221     D  WASC09                 1      9
005300941221     D*  2+3
005400941221     D                 DS
005500941221     D  WS07                   1      1
005600941221     D  WI07                   2      3
005700991125     D  WV07                   4      4    inz(',')
005800941221     D  WD07                   5      7
005900941221     D  WASC07                 1      7
006000021007     D*-------------------
006100021007     D* COSTANTI
006200021007     D*-------------------
006300021007     D charOK          C                   const('ABCDEFGHIJKLMNOPQRSTUVWXYZ-
006400021007     D                                     abcdefghijklmnopqrstuvwxyz1234567-
006500021007     D                                     890,;.:-_òçà°ù§+*èé!"£$%&/()=?ì\|')
006600021007     D*-------------------
006700021007     D* SCHIERE DI WRK
006800021007     D*-------------------
006900021007     D SKcharOK        S              1    DIM(92)
007000021007
007100021007
007200021008     C                   MOVEA     charOK        SKcharOK
007300040617     C                   EXSR      chkRECFILMBR
007400991027     C                   EXSR      traduci
007500921023     C                   SETON                                        LR
007600040617
007700040617
007800040617
007900040617
008000040617     C     chkRECFILMBR  BEGSR
008100040617     C*
008200040617     C                   READ      FNVACXLS                               55
008300040617     C*
008400040617     C                   ENDSR
008500040617
008600040617
008700040617
008800991027
008900991027     C     traduci       BEGSR
009000040617     C*
009100040617     C* Se richiesto nei parametri scrivo una prima riga con l'intestazione dei campi
009200040617     C                   IF        %subst(prmppt:8:1) = 'I' AND *IN55 = *ON
009300040617     C                   EVAL      VACXDTA = '"VACAAS"'+','+
009400040617     C                                       '"VACLNP"'+','+
009500040617     C                                       '"VACNRS"'+','+
009600040617     C                                       '"VACNSP"'+','+
009700040617     C                                       '"VACMGS"'+','+
009800040617     C                                       '"VACCBO"'+','+
009900040617     C                                       '"VACLNA"'+','+
010000040617     C                                       '"VACRSD"'+','+
010100040617     C                                       '"VACPRD"'+','+
010200040617     C                                       '"VACGC1"'+','+
010300040617     C                                       '"VACGC2"'+','+
010400040617     C                                       '"VACCTR"'+','+
010500040617     C                                       '"VACCTS"'+','+
010600040617     C                                       '"VACFTM"'+','+
010700040617     C                                       '"VACFIN"'+','+
010800040617     C                                       '"VACFAP"'+','+
010900040617     C                                       '"VACTSP"'+','+
011000040617     C                                       '"VACIAS"'+','+
011100040617     C                                       '"VACVAS"'+','+
011200040617     C                                       '"VACNAS"'+','+
011300040617     C                                       '"VACNCL"'+','+
011400040617     C                                       '"VACPKB"'+','+
011500040617     C                                       '"VACVLB"'+','+
011600040617     C                                       '"VACQFT"'+','+
011700040617     C                                       '"VACTIC"'+','+
011800040617     C                                       '"VACCAS"'+','+
011900040617     C                                       '"VACVCA"'+','+
012000040617     C                                       '"VACCCM"'+','+
012100040617     C                                       '"VACRMN"'+','+
012200040617     C                                       '"VACRMA"'+','+
012300040617     C                                       '"VACRMO"'+','+
012400040617     C                                       '"VACFFD"'+','+
012500040617     C                                       '"VACTCR"'+','+
012600040617     C                                       '"VACDCR"'+','+
012700040617     C                                       '"VACHCR"'+','+
012800040617     C                                       '"VACDCM"'+','+
012900040617     C                                       '"VACHMC"'+','+
013000040617     C                                       '"VACTC1"'+','+
013100040617     C                                       '"VACTC2"'+','+
013200040617     C                                       '"VACCCA"'+','+
013300040617     C                                       '"VACDLA"'+','+
013400040617     C                                       '"VACDAG"'
013500040617     C*
013600040617     C                   WRITE     FNVACX00
013700040617     C                   ENDIF
013800991027     C*
013900080924     C                   READ      TIVGDTMP
014000080924     C                   dow       not %eof(TIVGDTMP)
014100080924     C*
014200080924     C                   EVAL      fnvac00t = vgdDTA
014300080924     C*
014400040723     C                   CLEAR                   FNVACAA
014500040723     C                   CLEAR                   FNVACX00
014600991027     C*
014700941221     C                   MOVE      VACAAS        VHCAAS
014800941221     C                   MOVE      VACLNP        VHCLNP
014900941221     C                   MOVE      VACNRS        VHCNRS
015000941221     C                   MOVE      VACNSP        VHCNSP
015100941221     C                   MOVE      VACMGS        VHCMGS
015200941221     C                   MOVE      VACCBO        VHCCBO
015300941221     C                   MOVE      VACLNA        VHCLNA
015400021008     C                   MOVE      VACRSD        VHCRSD
015500021007     C* Effettuo considerazioni sui caratteri della Ragione Sociale Destinatario
015600021007     C     1             DO        35            x                 2 0
015700021007     C                   MOVEL     *blanks       wrkCHAR           1
015800021008     C                   EVAL      wrkCHAR = %subst(VHCRSD:x:1)
015900021008     C                   IF        wrkCHAR <> *blanks AND
016000021008     C                             wrkCHAR <> ''''
016100021007     C     wrkCHAR       LOOKUP    SKcharOK                               20
016200021007     C                   IF        *in20 = *off
016300021008     C                   EVAL      %subst(VHCRSD:x:1) = *blanks
016400021007     C                   ENDIF
016500021007     C                   ENDIF
016600021007     C                   ENDDO
016700021007     C*
016800941221     C                   MOVE      VACPRD        VHCPRD
016900941221     C                   MOVE      VACGC1        VHCGC1
017000941221     C                   MOVE      VACGC2        VHCGC2
017100941221     C                   MOVE      VACCTR        VHCCTR
017200941221     C                   MOVE      VACCTS        VHCCTS
017300941221     C                   MOVE      VACFTM        VHCFTM
017400941221     C                   MOVE      VACFIN        VHCFIN
017500941221     C                   MOVE      VACFAP        VHCFAP
017600941221     C                   MOVE      VACTSP        VHCTSP
017700941221     C*
017800941221     C                   MOVE      VACIAS        WD15
017900941221     C                   MOVEL     VACIAS        WI15
018000941221     C                   MOVE      WASC15        VHCIAS
018100941221     C*
018200941221     C                   MOVE      VACVAS        VHCVAS
018300941221     C                   MOVE      VACNAS        VHCNAS
018400941221     C                   MOVE      VACNCL        VHCNCL
018500941221     C*
018600941221     C                   MOVE      VACPKB        WD09
018700941221     C                   MOVEL     VACPKB        WI09
018800941221     C                   MOVE      WASC09        VHCPKB
018900941221     C*
019000941221     C                   MOVE      VACVLB        WD07
019100941221     C                   MOVEL     VACVLB        WI07
019200941221     C                   MOVE      WASC07        VHCVLB
019300941221     C*
019400941221     C                   MOVE      VACQFT        WD15
019500941221     C                   MOVEL     VACQFT        WI15
019600941221     C                   MOVE      WASC15        VHCQFT
019700941221     C*
019800941221     C                   MOVE      VACTIC        VHCTIC
019900941221     C*
020000941221     C                   MOVE      VACCAS        WD15
020100941221     C                   MOVEL     VACCAS        WI15
020200941221     C                   MOVE      WASC15        VHCCAS
020300941221     C*
020400941221     C                   MOVE      VACVCA        VHCVCA
020500941221     C                   MOVE      VACCCM        VHCCCM
020600941221     C                   MOVE      VACRMN        VHCRMN
020700941221     C                   MOVE      VACRMA        VHCRMA
020800941221     C                   MOVE      VACRMO        VHCRMO
020900941221     C                   MOVE      VACFFD        VHCFFD
021000941221     C                   MOVE      VACTCR        VHCTCR
021100941221     C                   MOVE      VACDCR        VHCDCR
021200941221     C                   MOVE      VACHCR        VHCHCR
021300941221     C                   MOVE      VACDCM        VHCDCM
021400941221     C                   MOVE      VACHMC        VHCHMC
021500941221     C                   MOVE      VACTC1        VHCTC1
021600941221     C                   MOVE      VACTC2        VHCTC2
021700941221     C                   MOVE      VACCCA        VHCCCA
021800941221     C                   MOVE      VACDLA        VHCDLA
021900941221     C                   MOVE      VACDAG        VHCDAG
022000930409     C*
022100040526     C* Effettuo determinate considerazioni x determinare in quale file scrivere il record corrente
022200040526     C* ...se evento "MESSA IN CONSEGNA" nn scrivo
022300040526     C                   IF        VACCCA = 'C'
022400040526     C                   ELSE
022500040526     C* ...se consegna ok scrivo il record nel file FNVACWWT membro 'M0'+cliente
022600041111     C*     solo spedizioni con serie. RIC. Venerini 10/11/04
022700060323      *     TUTTE le spedizioni richiesta Peretti 23/03/2006
022800040526     C                   IF        VACDCM > *zeros   AND
022900060323     C                             (VACCCA = *blanks or VACCCA = '1')
023000060323     C**!!!                        VACnrs <> *zeros
023100040526     C                   WRITE     FNVACAA
023200040723     C                   ENDIF
023300040617     C*
023400040617     C* ...se stato intermedio scrivo il record nel file FNVACXLS membro 'M0'+cliente+'A'
023500040617     C*
023600040617     C* Se richiesto nei parametri l'editazione dei campi data => gestisco
023700040617     C                   IF        %subst(prmppt:9:1) = 'D'
023800040617     C                   EVAL      VACXDTA = %trim(%editc(VACAAS:'Q'))+','+
023900040617     C                                       %trim(%editc(VACLNP:'Q'))+','+
024000040617     C                                       %trim(%editc(VACNRS:'Q'))+','+
024100040617     C                                       %trim(%editc(VACNSP:'Q'))+','+
024200040617     C                                       %trim(%editc(VACMGS:'Q'))+','+
024300040617     C                                          '"'+%trim(VACCBO)+'"'+','+
024400040617     C                                       %trim(%editc(VACLNA:'Q'))+','+
024500040617     C                                          '"'+%trim(VACRSD)+'"'+','+
024600040617     C                                          '"'+%trim(VACPRD)+'"'+','+
024700040617     C                                          '"'+%trim(VACGC1)+'"'+','+
024800040617     C                                          '"'+%trim(VACGC2)+'"'+','+
024900040617     C                                       %trim(%editc(VACCTR:'Q'))+','+
025000040617     C                                          '"'+%trim(VACCTS)+'"'+','+
025100040617     C                                          '"'+%trim(VACFTM)+'"'+','+
025200040617     C                                          '"'+%trim(VACFIN)+'"'+','+
025300040617     C                                          '"'+%trim(VACFAP)+'"'+','+
025400040617     C                                          '"'+%trim(VACTSP)+'"'+','+
025500040617     C                                       %trim(%editc(VACIAS:'Q'))+','+
025600040617     C                                          '"'+%trim(VACVAS)+'"'+','+
025700040617     C                                          '"'+%trim(VACNAS)+'"'+','+
025800040617     C                                       %trim(%editc(VACNCL:'Q'))+','+
025900040617     C                                       %trim(%editc(VACPKB:'Q'))+','+
026000040617     C                                       %trim(%editc(VACVLB:'Q'))+','+
026100040617     C                                       %trim(%editc(VACQFT:'Q'))+','+
026200040617     C                                          '"'+%trim(VACTIC)+'"'+','+
026300040617     C                                       %trim(%editc(VACCAS:'Q'))+','+
026400040617     C                                          '"'+%trim(VACVCA)+'"'+','+
026500040617     C                                       %trim(%editc(VACCCM:'Q'))+','+
026600040617     C                                       %trim(%editc(VACRMN:'Q'))+','+
026700040617     C                                          '"'+%trim(VACRMA)+'"'+','+
026800040617     C                                          '"'+%trim(VACRMO)+'"'+','+
026900040617     C                                          '"'+%trim(VACFFD)+'"'+','+
027000040617     C                                          '"'+%trim(VACTCR)+'"'+','+
027100040617     C                              %trim(%editw(VACDCR:'    /  /  '))+','+
027200040617     C                                       %trim(%editc(VACHCR:'Q'))+','+
027300040617     C                              %trim(%editw(VACDCM:'    /  /  '))+','+
027400040617     C                                       %trim(%editc(VACHMC:'Q'))+','+
027500040617     C                                          '"'+%trim(VACTC1)+'"'+','+
027600040617     C                                          '"'+%trim(VACTC2)+'"'+','+
027700040617     C                                          '"'+%trim(VACCCA)+'"'+','+
027800040617     C                              %trim(%editw(VACDLA:'    /  /  '))+','+
027900040617     C                              %trim(%editw(VACDAG:'    /  /  '))
028000040617     C                   ELSE
028100040617     C*
028200040617     C                   EVAL      VACXDTA = %trim(%editc(VACAAS:'Q'))+','+
028300040617     C                                       %trim(%editc(VACLNP:'Q'))+','+
028400040617     C                                       %trim(%editc(VACNRS:'Q'))+','+
028500040617     C                                       %trim(%editc(VACNSP:'Q'))+','+
028600040617     C                                       %trim(%editc(VACMGS:'Q'))+','+
028700040617     C                                          '"'+%trim(VACCBO)+'"'+','+
028800040617     C                                       %trim(%editc(VACLNA:'Q'))+','+
028900040617     C                                          '"'+%trim(VACRSD)+'"'+','+
029000040617     C                                          '"'+%trim(VACPRD)+'"'+','+
029100040617     C                                          '"'+%trim(VACGC1)+'"'+','+
029200040617     C                                          '"'+%trim(VACGC2)+'"'+','+
029300040617     C                                       %trim(%editc(VACCTR:'Q'))+','+
029400040617     C                                          '"'+%trim(VACCTS)+'"'+','+
029500040617     C                                          '"'+%trim(VACFTM)+'"'+','+
029600040617     C                                          '"'+%trim(VACFIN)+'"'+','+
029700040617     C                                          '"'+%trim(VACFAP)+'"'+','+
029800040617     C                                          '"'+%trim(VACTSP)+'"'+','+
029900040617     C                                       %trim(%editc(VACIAS:'Q'))+','+
030000040617     C                                          '"'+%trim(VACVAS)+'"'+','+
030100040617     C                                          '"'+%trim(VACNAS)+'"'+','+
030200040617     C                                       %trim(%editc(VACNCL:'Q'))+','+
030300040617     C                                       %trim(%editc(VACPKB:'Q'))+','+
030400040617     C                                       %trim(%editc(VACVLB:'Q'))+','+
030500040617     C                                       %trim(%editc(VACQFT:'Q'))+','+
030600040617     C                                          '"'+%trim(VACTIC)+'"'+','+
030700040617     C                                       %trim(%editc(VACCAS:'Q'))+','+
030800040617     C                                          '"'+%trim(VACVCA)+'"'+','+
030900040617     C                                       %trim(%editc(VACCCM:'Q'))+','+
031000040617     C                                       %trim(%editc(VACRMN:'Q'))+','+
031100040617     C                                          '"'+%trim(VACRMA)+'"'+','+
031200040617     C                                          '"'+%trim(VACRMO)+'"'+','+
031300040617     C                                          '"'+%trim(VACFFD)+'"'+','+
031400040617     C                                          '"'+%trim(VACTCR)+'"'+','+
031500040617     C                                       %trim(%editc(VACDCR:'Q'))+','+
031600040617     C                                       %trim(%editc(VACHCR:'Q'))+','+
031700040617     C                                       %trim(%editc(VACDCM:'Q'))+','+
031800040617     C                                       %trim(%editc(VACHMC:'Q'))+','+
031900040617     C                                          '"'+%trim(VACTC1)+'"'+','+
032000040617     C                                          '"'+%trim(VACTC2)+'"'+','+
032100040617     C                                          '"'+%trim(VACCCA)+'"'+','+
032200040617     C                                       %trim(%editc(VACDLA:'Q'))+','+
032300040617     C                                       %trim(%editc(VACDAG:'Q'))
032400040617     C                   ENDIF
032500040617     C                   WRITE     FNVACX00
032600040526     C                   ENDIF
032700040526     C*
032800040526     C* In ogni caso storicizzo il record e deleto l'input
032900080924     C                   DELETE    TIVGD000
033000991027     C*
033100080924     C                   READ      TIVGDTMP
033200040526     C                   ENDDO
033300991027     C*
033400040526     C* Creo il record d log x il download del 2° file da invaire in altro modo
033500040526     C                   EXSR      CREALOGS
033600040526     C*
033700040526     C                   EVAL      wrkesito = '0'
033800991027     C*
033900910830     C                   ENDSR
034000040526
034100040526
034200040526
034300040526     C     CREALOGS      BEGSR
034400040526     C*
034500040526     C* Valorizzo i campi comuni a tutti i reord di log da "clonare"
034600040526     C                   CLEAR                   TIVLT000
034700040526     C                   EVAL      wISV = 'OE'
034800040526     C                   EVAL      vltKSC = '0' + prmksc
034900040526     C                   EVAL      vltKSU = '0' + prmksc
035000040526     C                   EVAL      vltMBD = prmmbr
035100040526     C                   EVAL      vltSND = 'D'
035200040526     C                   EVAL      vltFLS = 'FNVAC00S'
035300040526     C*
035400040526     C                   time                    wrkoggiiso
035500040526     C                   extrct    wrkoggiiso:*m wrkmese
035600040526     C                   extrct    wrkoggiiso:*d wrkday
035700040526     C                   eval      vltMBS =
035800040526     C                             mc(wrkmese)
035900040526     C                             + wrkdaychr
036000040526     C                             + %subst(vltKSU:2:7)
036100040526     C*
036200040526     C                   EVAL      vltDAT = DATCOR
036300040526     C                   EVAL      vltORA = ORACOR
036400040526     C                   EVAL      vltISV = wISV
036500040526     C                   EVAL      vltTIA = 'F'
036600040526     C                   EVAL      vltFL2 = '01'
036700040526     C                   EVAL      vltSTS = '0'
036800040526     C*
036900040526     C* => scrivo record di log x tipo file 01 (x ARIANNA 2001)
037000040526     C                   EXSR      calprog
037100040526     C                   EVAL      vltPRG = dwlprg
037200040526     C*
037300040526     C                   EVAL      vltTIP = '01'
037400040617     C                   EVAL      vltFLD = 'FNVACXLS'
037500040526     C                   WRITE     TIVLT000
037600040526     C*
037700040526     C                   ENDSR
037800040526
037900040526
038000040526
038100040526      /TITLE Valorizzazione Progressivo Applicazione
038200040526     C     calprog       begsr
038300040526     C*
038400040526     C                   open      tis7prgf
038500040526     C*
038600040526     C                   read(e)   tis7prgf
038700040526     C                   if        not %error
038800040526     C                   eval      dwlprg = f_tis7prgf
038900040526     C*
039000040526     C                   move(p)   dwlprg        wrkprg
039100040526     C                   add       1             wrkprg
039200040526     C                   move(p)   wrkprg        dwlprg
039300040526     C                   movel     wISV          dwlprg
039400040526     C*
039500040526     C                   eval      f_tis7prgf = dwlprg
039600040526     C                   update    tis7prg0
039700040526     C                   endif
039800040526     C*
039900040526     C                   close     tis7prgf
040000040526     C*
040100040526     C                   endsr
040200040526
040300040526
040400040526
040500991027
040600991027      /TITLE Operazioni iniziali.
040700991027     C     *inzsr        BEGSR
040800040526     C*
040900040526     C     *ENTRY        PLIST
041000040526     C                   parm                    prmppt
041100040526     C     wrkesito      parm      wrkesito      prmesito
041200040526     C                   parm                    prmksc
041300040526     C                   parm                    prmmbr
041400040526     C*
041500040526     C* IMPOSTA LA DATA CORRENTE
041600040526     C                   TIME                    N14              14 0
041700040526     C                   MOVE      N14           N8                8 0
041800040526     C                   MOVEL     N14           ORACOR            6 0
041900040526     C                   Z-ADD     N8            G08DAT
042000040526     C                   Z-ADD     *ZEROS        G08INV
042100040526     C                   MOVEL     '0'           G08ERR
042200040526     C                   CALL      'XSRDA8'
042300040526     C                   PARM                    WLBDA8
042400040526     C                   Z-ADD     G08INV        DATCOR            8 0
042500991027     C*
042600991027     C                   ENDSR
042700040526** ctdata mc
042800040526ABCDEFGHILMN
