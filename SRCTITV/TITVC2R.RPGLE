000100040723     H DECEDIT('0.') DATEDIT(*DMY.)
000200991027     H dftactgrp(*yes)
000300991027
000400040526     FTIVLT01L  UF A E           K DISK
000500080924     FTIVGDTMP  UF   E             DISK
000600040617     FFNVACXLS  UF A E             DISK
000700991027     Ffnvacwwt  O    E             DISK
000800040526     Ftis7prgf  uf   e             disk    RENAME(tis7prgf:tis7prg0)
000900040526     F                                     PREFIX(f_)
001000991027
001100080924     D fnvac00t      e ds
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
002600040526
002700040526     D*---------------------
002800040526     D* DS "XSRDA8" - CONTROLLA DATA (8)
002900040526     D*---------------------
003000040526     D WLBDA8          DS
003100040526     D  G08DAT                 1      8  0
003200040526     D  G08INV                 9     16  0
003300040526     D  G08ERR                17     17
003400040526     D  G08TGI                18     22  0
003500040526
003600040526     D*---------------------
003700941221     D* DS PER EDITARE I CAMPI NUMERICI IN ASCII
003800941221     D* 10+3
003900941221     D                 DS
004000941221     D  WS15                   1      1
004100941221     D  WI15                   2     11
004200991125     D  WV15                  12     12    inz(',')
004300941221     D  WD15                  13     15
004400941221     D  WASC15                 1     15
004500941221     D*  6+1
004600941221     D                 DS
004700941221     D  WS09                   1      1
004800941221     D  WI09                   2      7
004900991125     D  WV09                   8      8    inz(',')
005000941221     D  WD09                   9      9
005100941221     D  WASC09                 1      9
005200941221     D*  2+3
005300941221     D                 DS
005400941221     D  WS07                   1      1
005500941221     D  WI07                   2      3
005600991125     D  WV07                   4      4    inz(',')
005700941221     D  WD07                   5      7
005800941221     D  WASC07                 1      7
005900021007     D*-------------------
006000021007     D* COSTANTI
006100021007     D*-------------------
006200021007     D charOK          C                   const('ABCDEFGHIJKLMNOPQRSTUVWXYZ-
006300021007     D                                     abcdefghijklmnopqrstuvwxyz1234567-
006400021007     D                                     890,;.:-_òçà°ù§+*èé!"£$%&/()=?ì\|')
006500021007     D*-------------------
006600021007     D* SCHIERE DI WRK
006700021007     D*-------------------
006800021007     D SKcharOK        S              1    DIM(92)
006900021007
007000021007
007100021008     C                   MOVEA     charOK        SKcharOK
007200040617     C                   EXSR      chkRECFILMBR
007300991027     C                   EXSR      traduci
007400921023     C                   SETON                                        LR
007500040617
007600040617
007700040617
007800040617
007900040617     C     chkRECFILMBR  BEGSR
008000040617     C*
008100040617     C                   READ      FNVACXLS                               55
008200040617     C*
008300040617     C                   ENDSR
008400040617
008500040617
008600040617
008700991027
008800991027     C     traduci       BEGSR
008900040617     C*
009000040617     C* Se richiesto nei parametri scrivo una prima riga con l'intestazione dei campi
009100040617     C                   IF        %subst(prmppt:8:1) = 'I' AND *IN55 = *ON
009200040617     C                   EVAL      VACXDTA = '"VACAAS"'+','+
009300040617     C                                       '"VACLNP"'+','+
009400040617     C                                       '"VACNRS"'+','+
009500040617     C                                       '"VACNSP"'+','+
009600040617     C                                       '"VACMGS"'+','+
009700040617     C                                       '"VACCBO"'+','+
009800040617     C                                       '"VACLNA"'+','+
009900040617     C                                       '"VACRSD"'+','+
010000040617     C                                       '"VACPRD"'+','+
010100040617     C                                       '"VACGC1"'+','+
010200040617     C                                       '"VACGC2"'+','+
010300040617     C                                       '"VACCTR"'+','+
010400040617     C                                       '"VACCTS"'+','+
010500040617     C                                       '"VACFTM"'+','+
010600040617     C                                       '"VACFIN"'+','+
010700040617     C                                       '"VACFAP"'+','+
010800040617     C                                       '"VACTSP"'+','+
010900040617     C                                       '"VACIAS"'+','+
011000040617     C                                       '"VACVAS"'+','+
011100040617     C                                       '"VACNAS"'+','+
011200040617     C                                       '"VACNCL"'+','+
011300040617     C                                       '"VACPKB"'+','+
011400040617     C                                       '"VACVLB"'+','+
011500040617     C                                       '"VACQFT"'+','+
011600040617     C                                       '"VACTIC"'+','+
011700040617     C                                       '"VACCAS"'+','+
011800040617     C                                       '"VACVCA"'+','+
011900040617     C                                       '"VACCCM"'+','+
012000040617     C                                       '"VACRMN"'+','+
012100040617     C                                       '"VACRMA"'+','+
012200040617     C                                       '"VACRMO"'+','+
012300040617     C                                       '"VACFFD"'+','+
012400040617     C                                       '"VACTCR"'+','+
012500040617     C                                       '"VACDCR"'+','+
012600040617     C                                       '"VACHCR"'+','+
012700040617     C                                       '"VACDCM"'+','+
012800040617     C                                       '"VACHMC"'+','+
012900040617     C                                       '"VACTC1"'+','+
013000040617     C                                       '"VACTC2"'+','+
013100040617     C                                       '"VACCCA"'+','+
013200040617     C                                       '"VACDLA"'+','+
013300040617     C                                       '"VACDAG"'
013400040617     C*
013500040617     C                   WRITE     FNVACX00
013600040617     C                   ENDIF
013700991027     C*
013800080924     C                   READ      TIVGDTMP
013900080924     C                   dow       not %eof(TIVGDTMP)
014000080924     C*
014100080924     C                   EVAL      fnvac00t = vgdDTA
014200080924     C*
014300040723     C                   CLEAR                   FNVACAA
014400040723     C                   CLEAR                   FNVACX00
014500991027     C*
014600941221     C                   MOVE      VACAAS        VHCAAS
014700941221     C                   MOVE      VACLNP        VHCLNP
014800941221     C                   MOVE      VACNRS        VHCNRS
014900941221     C                   MOVE      VACNSP        VHCNSP
015000941221     C                   MOVE      VACMGS        VHCMGS
015100941221     C                   MOVE      VACCBO        VHCCBO
015200941221     C                   MOVE      VACLNA        VHCLNA
015300021008     C                   MOVE      VACRSD        VHCRSD
015400021007     C* Effettuo considerazioni sui caratteri della Ragione Sociale Destinatario
015500021007     C     1             DO        35            x                 2 0
015600021007     C                   MOVEL     *blanks       wrkCHAR           1
015700021008     C                   EVAL      wrkCHAR = %subst(VHCRSD:x:1)
015800021008     C                   IF        wrkCHAR <> *blanks AND
015900021008     C                             wrkCHAR <> ''''
016000021007     C     wrkCHAR       LOOKUP    SKcharOK                               20
016100021007     C                   IF        *in20 = *off
016200021008     C                   EVAL      %subst(VHCRSD:x:1) = *blanks
016300021007     C                   ENDIF
016400021007     C                   ENDIF
016500021007     C                   ENDDO
016600021007     C*
016700941221     C                   MOVE      VACPRD        VHCPRD
016800941221     C                   MOVE      VACGC1        VHCGC1
016900941221     C                   MOVE      VACGC2        VHCGC2
017000941221     C                   MOVE      VACCTR        VHCCTR
017100941221     C                   MOVE      VACCTS        VHCCTS
017200941221     C                   MOVE      VACFTM        VHCFTM
017300941221     C                   MOVE      VACFIN        VHCFIN
017400941221     C                   MOVE      VACFAP        VHCFAP
017500941221     C                   MOVE      VACTSP        VHCTSP
017600941221     C*
017700941221     C                   MOVE      VACIAS        WD15
017800941221     C                   MOVEL     VACIAS        WI15
017900941221     C                   MOVE      WASC15        VHCIAS
018000941221     C*
018100941221     C                   MOVE      VACVAS        VHCVAS
018200941221     C                   MOVE      VACNAS        VHCNAS
018300941221     C                   MOVE      VACNCL        VHCNCL
018400941221     C*
018500941221     C                   MOVE      VACPKB        WD09
018600941221     C                   MOVEL     VACPKB        WI09
018700941221     C                   MOVE      WASC09        VHCPKB
018800941221     C*
018900941221     C                   MOVE      VACVLB        WD07
019000941221     C                   MOVEL     VACVLB        WI07
019100941221     C                   MOVE      WASC07        VHCVLB
019200941221     C*
019300941221     C                   MOVE      VACQFT        WD15
019400941221     C                   MOVEL     VACQFT        WI15
019500941221     C                   MOVE      WASC15        VHCQFT
019600941221     C*
019700941221     C                   MOVE      VACTIC        VHCTIC
019800941221     C*
019900941221     C                   MOVE      VACCAS        WD15
020000941221     C                   MOVEL     VACCAS        WI15
020100941221     C                   MOVE      WASC15        VHCCAS
020200941221     C*
020300941221     C                   MOVE      VACVCA        VHCVCA
020400941221     C                   MOVE      VACCCM        VHCCCM
020500941221     C                   MOVE      VACRMN        VHCRMN
020600941221     C                   MOVE      VACRMA        VHCRMA
020700941221     C                   MOVE      VACRMO        VHCRMO
020800941221     C                   MOVE      VACFFD        VHCFFD
020900941221     C                   MOVE      VACTCR        VHCTCR
021000941221     C                   MOVE      VACDCR        VHCDCR
021100941221     C                   MOVE      VACHCR        VHCHCR
021200941221     C                   MOVE      VACDCM        VHCDCM
021300941221     C                   MOVE      VACHMC        VHCHMC
021400941221     C                   MOVE      VACTC1        VHCTC1
021500941221     C                   MOVE      VACTC2        VHCTC2
021600941221     C                   MOVE      VACCCA        VHCCCA
021700941221     C                   MOVE      VACDLA        VHCDLA
021800941221     C                   MOVE      VACDAG        VHCDAG
021900930409     C*
022000040526     C* Effettuo determinate considerazioni x determinare in quale file scrivere il record corrente
022100040526     C* ...se evento "MESSA IN CONSEGNA" nn scrivo
022200040526     C                   IF        VACCCA = 'C'
022300040526     C                   ELSE
022400040526     C* ...se consegna ok scrivo il record nel file FNVACWWT membro 'M0'+cliente
022500041111     C*     solo spedizioni con serie. RIC. Venerini 10/11/04
022600060323      *     TUTTE le spedizioni richiesta Peretti 23/03/2006
022700040526     C                   IF        VACDCM > *zeros   AND
022800060323     C                             (VACCCA = *blanks or VACCCA = '1')
022900060323     C**!!!                        VACnrs <> *zeros
023000040526     C                   WRITE     FNVACAA
023100040723     C                   ENDIF
023200040617     C*
023300040617     C* ...se stato intermedio scrivo il record nel file FNVACXLS membro 'M0'+cliente+'A'
023400040617     C*
023500040617     C* Se richiesto nei parametri l'editazione dei campi data => gestisco
023600040617     C                   IF        %subst(prmppt:9:1) = 'D'
023700040617     C                   EVAL      VACXDTA = %trim(%editc(VACAAS:'Q'))+','+
023800040617     C                                       %trim(%editc(VACLNP:'Q'))+','+
023900040617     C                                       %trim(%editc(VACNRS:'Q'))+','+
024000040617     C                                       %trim(%editc(VACNSP:'Q'))+','+
024100040617     C                                       %trim(%editc(VACMGS:'Q'))+','+
024200040617     C                                          '"'+%trim(VACCBO)+'"'+','+
024300040617     C                                       %trim(%editc(VACLNA:'Q'))+','+
024400040617     C                                          '"'+%trim(VACRSD)+'"'+','+
024500040617     C                                          '"'+%trim(VACPRD)+'"'+','+
024600040617     C                                          '"'+%trim(VACGC1)+'"'+','+
024700040617     C                                          '"'+%trim(VACGC2)+'"'+','+
024800040617     C                                       %trim(%editc(VACCTR:'Q'))+','+
024900040617     C                                          '"'+%trim(VACCTS)+'"'+','+
025000040617     C                                          '"'+%trim(VACFTM)+'"'+','+
025100040617     C                                          '"'+%trim(VACFIN)+'"'+','+
025200040617     C                                          '"'+%trim(VACFAP)+'"'+','+
025300040617     C                                          '"'+%trim(VACTSP)+'"'+','+
025400040617     C                                       %trim(%editc(VACIAS:'Q'))+','+
025500040617     C                                          '"'+%trim(VACVAS)+'"'+','+
025600040617     C                                          '"'+%trim(VACNAS)+'"'+','+
025700040617     C                                       %trim(%editc(VACNCL:'Q'))+','+
025800040617     C                                       %trim(%editc(VACPKB:'Q'))+','+
025900040617     C                                       %trim(%editc(VACVLB:'Q'))+','+
026000040617     C                                       %trim(%editc(VACQFT:'Q'))+','+
026100040617     C                                          '"'+%trim(VACTIC)+'"'+','+
026200040617     C                                       %trim(%editc(VACCAS:'Q'))+','+
026300040617     C                                          '"'+%trim(VACVCA)+'"'+','+
026400040617     C                                       %trim(%editc(VACCCM:'Q'))+','+
026500040617     C                                       %trim(%editc(VACRMN:'Q'))+','+
026600040617     C                                          '"'+%trim(VACRMA)+'"'+','+
026700040617     C                                          '"'+%trim(VACRMO)+'"'+','+
026800040617     C                                          '"'+%trim(VACFFD)+'"'+','+
026900040617     C                                          '"'+%trim(VACTCR)+'"'+','+
027000040617     C                              %trim(%editw(VACDCR:'    /  /  '))+','+
027100040617     C                                       %trim(%editc(VACHCR:'Q'))+','+
027200040617     C                              %trim(%editw(VACDCM:'    /  /  '))+','+
027300040617     C                                       %trim(%editc(VACHMC:'Q'))+','+
027400040617     C                                          '"'+%trim(VACTC1)+'"'+','+
027500040617     C                                          '"'+%trim(VACTC2)+'"'+','+
027600040617     C                                          '"'+%trim(VACCCA)+'"'+','+
027700040617     C                              %trim(%editw(VACDLA:'    /  /  '))+','+
027800040617     C                              %trim(%editw(VACDAG:'    /  /  '))
027900040617     C                   ELSE
028000040617     C*
028100040617     C                   EVAL      VACXDTA = %trim(%editc(VACAAS:'Q'))+','+
028200040617     C                                       %trim(%editc(VACLNP:'Q'))+','+
028300040617     C                                       %trim(%editc(VACNRS:'Q'))+','+
028400040617     C                                       %trim(%editc(VACNSP:'Q'))+','+
028500040617     C                                       %trim(%editc(VACMGS:'Q'))+','+
028600040617     C                                          '"'+%trim(VACCBO)+'"'+','+
028700040617     C                                       %trim(%editc(VACLNA:'Q'))+','+
028800040617     C                                          '"'+%trim(VACRSD)+'"'+','+
028900040617     C                                          '"'+%trim(VACPRD)+'"'+','+
029000040617     C                                          '"'+%trim(VACGC1)+'"'+','+
029100040617     C                                          '"'+%trim(VACGC2)+'"'+','+
029200040617     C                                       %trim(%editc(VACCTR:'Q'))+','+
029300040617     C                                          '"'+%trim(VACCTS)+'"'+','+
029400040617     C                                          '"'+%trim(VACFTM)+'"'+','+
029500040617     C                                          '"'+%trim(VACFIN)+'"'+','+
029600040617     C                                          '"'+%trim(VACFAP)+'"'+','+
029700040617     C                                          '"'+%trim(VACTSP)+'"'+','+
029800040617     C                                       %trim(%editc(VACIAS:'Q'))+','+
029900040617     C                                          '"'+%trim(VACVAS)+'"'+','+
030000040617     C                                          '"'+%trim(VACNAS)+'"'+','+
030100040617     C                                       %trim(%editc(VACNCL:'Q'))+','+
030200040617     C                                       %trim(%editc(VACPKB:'Q'))+','+
030300040617     C                                       %trim(%editc(VACVLB:'Q'))+','+
030400040617     C                                       %trim(%editc(VACQFT:'Q'))+','+
030500040617     C                                          '"'+%trim(VACTIC)+'"'+','+
030600040617     C                                       %trim(%editc(VACCAS:'Q'))+','+
030700040617     C                                          '"'+%trim(VACVCA)+'"'+','+
030800040617     C                                       %trim(%editc(VACCCM:'Q'))+','+
030900040617     C                                       %trim(%editc(VACRMN:'Q'))+','+
031000040617     C                                          '"'+%trim(VACRMA)+'"'+','+
031100040617     C                                          '"'+%trim(VACRMO)+'"'+','+
031200040617     C                                          '"'+%trim(VACFFD)+'"'+','+
031300040617     C                                          '"'+%trim(VACTCR)+'"'+','+
031400040617     C                                       %trim(%editc(VACDCR:'Q'))+','+
031500040617     C                                       %trim(%editc(VACHCR:'Q'))+','+
031600040617     C                                       %trim(%editc(VACDCM:'Q'))+','+
031700040617     C                                       %trim(%editc(VACHMC:'Q'))+','+
031800040617     C                                          '"'+%trim(VACTC1)+'"'+','+
031900040617     C                                          '"'+%trim(VACTC2)+'"'+','+
032000040617     C                                          '"'+%trim(VACCCA)+'"'+','+
032100040617     C                                       %trim(%editc(VACDLA:'Q'))+','+
032200040617     C                                       %trim(%editc(VACDAG:'Q'))
032300040617     C                   ENDIF
032400040617     C                   WRITE     FNVACX00
032500040526     C                   ENDIF
032600040526     C*
032700040526     C* In ogni caso storicizzo il record e deleto l'input
032800080924     C                   DELETE    TIVGD000
032900991027     C*
033000080924     C                   READ      TIVGDTMP
033100040526     C                   ENDDO
033200991027     C*
033300040526     C* Creo il record d log x il download del 2° file da invaire in altro modo
033400040526     C                   EXSR      CREALOGS
033500040526     C*
033600040526     C                   EVAL      wrkesito = '0'
033700991027     C*
033800910830     C                   ENDSR
033900040526
034000040526
034100040526
034200040526     C     CREALOGS      BEGSR
034300040526     C*
034400040526     C* Valorizzo i campi comuni a tutti i reord di log da "clonare"
034500040526     C                   CLEAR                   TIVLT000
034600040526     C                   EVAL      wISV = 'OE'
034700040526     C                   EVAL      vltKSC = '0' + prmksc
034800040526     C                   EVAL      vltKSU = '0' + prmksc
034900040526     C                   EVAL      vltMBD = prmmbr
035000040526     C                   EVAL      vltSND = 'D'
035100040526     C                   EVAL      vltFLS = 'FNVAC00S'
035200040526     C*
035300040526     C                   time                    wrkoggiiso
035400040526     C                   extrct    wrkoggiiso:*m wrkmese
035500040526     C                   extrct    wrkoggiiso:*d wrkday
035600040526     C                   eval      vltMBS =
035700040526     C                             mc(wrkmese)
035800040526     C                             + wrkdaychr
035900040526     C                             + %subst(vltKSU:2:7)
036000040526     C*
036100040526     C                   EVAL      vltDAT = DATCOR
036200040526     C                   EVAL      vltORA = ORACOR
036300040526     C                   EVAL      vltISV = wISV
036400040526     C                   EVAL      vltTIA = 'F'
036500040526     C                   EVAL      vltFL2 = '01'
036600040526     C                   EVAL      vltSTS = '0'
036700040526     C*
036800040526     C* => scrivo record di log x tipo file 01 (x ARIANNA 2001)
036900040526     C                   EXSR      calprog
037000170324     C                   EVAL      vltPRG = f_tis7prgf
037100040526     C*
037200040526     C                   EVAL      vltTIP = '01'
037300040617     C                   EVAL      vltFLD = 'FNVACXLS'
037400040526     C                   WRITE     TIVLT000
037500040526     C*
037600040526     C                   ENDSR
037700040526
037800040526
037900040526
038000040526      /TITLE Valorizzazione Progressivo Applicazione
038100040526     C     calprog       begsr
038200040526     C*
038300170324     C     *start        setll     tis7prgf
038400040526     C                   read(e)   tis7prgf
038500040526     C                   if        not %error
038600040526     C                   eval      dwlprg = f_tis7prgf
038700040526     C*
038800040526     C                   move(p)   dwlprg        wrkprg
038900040526     C                   add       1             wrkprg
039000040526     C                   move(p)   wrkprg        dwlprg
039100040526     C                   movel     wISV          dwlprg
039200040526     C*
039300040526     C                   eval      f_tis7prgf = dwlprg
039400040526     C                   update    tis7prg0
039500170324     C                   else
039600170324     C                   eval      f_tis7prgf = 'XX00000000'
039700040526     C                   endif
039800040526     C*
039900040526     C                   endsr
040000040526
040100040526
040200040526
040300991027
040400991027      /TITLE Operazioni iniziali.
040500991027     C     *inzsr        BEGSR
040600040526     C*
040700040526     C     *ENTRY        PLIST
040800040526     C                   parm                    prmppt
040900040526     C     wrkesito      parm      wrkesito      prmesito
041000040526     C                   parm                    prmksc
041100040526     C                   parm                    prmmbr
041200040526     C*
041300040526     C* IMPOSTA LA DATA CORRENTE
041400040526     C                   TIME                    N14              14 0
041500040526     C                   MOVE      N14           N8                8 0
041600040526     C                   MOVEL     N14           ORACOR            6 0
041700040526     C                   Z-ADD     N8            G08DAT
041800040526     C                   Z-ADD     *ZEROS        G08INV
041900040526     C                   MOVEL     '0'           G08ERR
042000040526     C                   CALL      'XSRDA8'
042100040526     C                   PARM                    WLBDA8
042200040526     C                   Z-ADD     G08INV        DATCOR            8 0
042300991027     C*
042400991027     C                   ENDSR
042500040526** ctdata mc
042600040526ABCDEFGHILMN
