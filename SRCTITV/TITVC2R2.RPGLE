000100150813     H DFTACTGRP(*NO) ACTGRP(*CALLER)
000200040723     H DECEDIT('0.') DATEDIT(*DMY.)
000300991027
000400080924     FTIVGDTMP  UF   E             DISK
000500150813     FFNVACXLS  UF A E             DISK    usropn
000600150813     Ftivawwwt  UF A E             DISK    usropn
000700150813     Ffnvacwwt  UF A E             DISK    usropn
000800991027
000900991027     D prmppt          s             50
001000991027     D prmesito        s              1
001100040526     D prmksc          s              7
001200040526     D prmmbr          s             10
001300991027     D wrkesito        s                   like(prmesito)
001400040617
001500080924     D fnvac00t      e ds
001600040526
001700040526     D*---------------------
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
005300150813     C                   EXSR      apriFile
005400991027     C                   EXSR      traduci
005500150813     C                   EXSR      chiudiFile
005600921023     C                   SETON                                        LR
005700150813
005800150813
005900150813
006000150813     C     apriFile      BEGSR
006100150813     C*
006200150813     C                   setoff                                       414243
006300150813     C                   setoff                                       55
006400150813     C                   select
006500150813     C*
006600150813     C                   when      %subst(prmppt:10:2) = '01'
006700150813     C                   if        not %open(FNVACXLS)
006800150813     C                   seton                                        41
006900150813     C                   open      FNVACXLS
007000150813     C                   read      FNVACXLS                               55
007100150813     C                   endif
007200150813     C*
007300150813     C                   when      %subst(prmppt:10:2) = '03'
007400150813     C                   if        not %open(tivawwwt)
007500150813     C                   seton                                        42
007600150813     C                   open      tivawwwt
007700150813     C                   endif
007800150813     C*
007900150813     C                   when      %subst(prmppt:10:2) = 'VC'
008000150813     C                   if        not %open(fnvacwwt)
008100150813     C                   seton                                        43
008200150813     C                   open      fnvacwwt
008300150813     C                   endif
008400150813     C*
008500150813     C                   endsl
008600150813     C*
008700150813     C                   ENDSR
008800150813
008900150813
009000150813
009100150813     C     chiudiFile    BEGSR
009200150813     C*
009300150813     C                   select
009400150813     C*
009500150813     C                   when      %subst(prmppt:10:2) = '01'
009600150813     C                   if        %open(FNVACXLS)
009700150813     C                   close     FNVACXLS
009800150813     C                   endif
009900150813     C*
010000150813     C                   when      %subst(prmppt:10:2) = '03'
010100150813     C                   if        %open(tivawwwt)
010200150813     C                   close     tivawwwt
010300150813     C                   endif
010400150813     C*
010500150813     C                   when      %subst(prmppt:10:2) = 'VC'
010600150813     C                   if        %open(fnvacwwt)
010700150813     C                   close     fnvacwwt
010800150813     C                   endif
010900150813     C*
011000150813     C                   endsl
011100150813     C*
011200150813     C                   ENDSR
011300040617
011400040617
011500040617
011600991027     C     traduci       BEGSR
011700040617     C*
011800070524     C                   EVAL      VACXDTA = '"VACAAS"'+','+
011900070524     C                                       '"VACLNP"'+','+
012000070524     C                                       '"VACNRS"'+','+
012100070524     C                                       '"VACNSP"'+','+
012200070524     C                                       '"VACMGS"'+','+
012300070524     C                                       '"VACCBO"'+','+
012400070524     C                                       '"VACLNA"'+','+
012500070524     C                                       '"VACRSD"'+','+
012600070524     C                                       '"VACPRD"'+','+
012700070524     C                                       '"VACGC1"'+','+
012800070524     C                                       '"VACGC2"'+','+
012900070524     C                                       '"VACCTR"'+','+
013000070524     C                                       '"VACCTS"'+','+
013100070524     C                                       '"VACFTM"'+','+
013200070524     C                                       '"VACFIN"'+','+
013300070524     C                                       '"VACFAP"'+','+
013400070524     C                                       '"VACTSP"'+','+
013500070524     C                                       '"VACIAS"'+','+
013600070524     C                                       '"VACVAS"'+','+
013700070524     C                                       '"VACNAS"'+','+
013800070524     C                                       '"VACNCL"'+','+
013900070524     C                                       '"VACPKB"'+','+
014000070524     C                                       '"VACVLB"'+','+
014100070524     C                                       '"VACQFT"'+','+
014200070524     C                                       '"VACTIC"'+','+
014300070524     C                                       '"VACCAS"'+','+
014400070524     C                                       '"VACVCA"'+','+
014500070524     C                                       '"VACCCM"'+','+
014600070524     C                                       '"VACRMN"'+','+
014700070524     C                                       '"VACRMA"'+','+
014800070524     C                                       '"VACRMO"'+','+
014900070524     C                                       '"VACFFD"'+','+
015000070524     C                                       '"VACTCR"'+','+
015100070524     C                                       '"VACDCR"'+','+
015200070524     C                                       '"VACHCR"'+','+
015300070524     C                                       '"VACDCM"'+','+
015400070524     C                                       '"VACHMC"'+','+
015500070524     C                                       '"VACTC1"'+','+
015600070524     C                                       '"VACTC2"'+','+
015700070524     C                                       '"VACCCA"'+','+
015800070524     C                                       '"VACDLA"'+','+
015900070524     C                                       '"VACDAG"'
016000070524     C*
016100070524     C* Se richiesto nei parametri scrivo una prima riga con l'intestazione dei campi
016200070524     C                   IF        %subst(prmppt:8:1) = 'I' AND *IN55 = *ON
016300070524     C                   WRITE     FNVACX00
016400070524     C                   ENDIF
016500991027     C*
016600080924     C                   READ      TIVGDTMP
016700080924     C                   dow       not %eof(TIVGDTMP)
016800080924     C*
016900080924     C                   EVAL      fnvac00t = vgdDTA
017000080924     C*
017100040723     C                   CLEAR                   FNVACAA
017200040723     C                   CLEAR                   FNVACX00
017300070524     C                   CLEAR                   TIVAW000
017400991027     C*
017500941221     C                   MOVE      VACAAS        VHCAAS
017600941221     C                   MOVE      VACLNP        VHCLNP
017700941221     C                   MOVE      VACNRS        VHCNRS
017800941221     C                   MOVE      VACNSP        VHCNSP
017900941221     C                   MOVE      VACMGS        VHCMGS
018000941221     C                   MOVE      VACCBO        VHCCBO
018100941221     C                   MOVE      VACLNA        VHCLNA
018200021008     C                   MOVE      VACRSD        VHCRSD
018300021007     C* Effettuo considerazioni sui caratteri della Ragione Sociale Destinatario
018400021007     C     1             DO        35            x                 2 0
018500021007     C                   MOVEL     *blanks       wrkCHAR           1
018600021008     C                   EVAL      wrkCHAR = %subst(VHCRSD:x:1)
018700021008     C                   IF        wrkCHAR <> *blanks AND
018800021008     C                             wrkCHAR <> ''''
018900021007     C     wrkCHAR       LOOKUP    SKcharOK                               20
019000021007     C                   IF        *in20 = *off
019100021008     C                   EVAL      %subst(VHCRSD:x:1) = *blanks
019200021007     C                   ENDIF
019300021007     C                   ENDIF
019400021007     C                   ENDDO
019500021007     C*
019600941221     C                   MOVE      VACPRD        VHCPRD
019700941221     C                   MOVE      VACGC1        VHCGC1
019800941221     C                   MOVE      VACGC2        VHCGC2
019900941221     C                   MOVE      VACCTR        VHCCTR
020000941221     C                   MOVE      VACCTS        VHCCTS
020100941221     C                   MOVE      VACFTM        VHCFTM
020200941221     C                   MOVE      VACFIN        VHCFIN
020300941221     C                   MOVE      VACFAP        VHCFAP
020400941221     C                   MOVE      VACTSP        VHCTSP
020500941221     C*
020600941221     C                   MOVE      VACIAS        WD15
020700941221     C                   MOVEL     VACIAS        WI15
020800941221     C                   MOVE      WASC15        VHCIAS
020900941221     C*
021000941221     C                   MOVE      VACVAS        VHCVAS
021100941221     C                   MOVE      VACNAS        VHCNAS
021200941221     C                   MOVE      VACNCL        VHCNCL
021300941221     C*
021400941221     C                   MOVE      VACPKB        WD09
021500941221     C                   MOVEL     VACPKB        WI09
021600941221     C                   MOVE      WASC09        VHCPKB
021700941221     C*
021800941221     C                   MOVE      VACVLB        WD07
021900941221     C                   MOVEL     VACVLB        WI07
022000941221     C                   MOVE      WASC07        VHCVLB
022100941221     C*
022200941221     C                   MOVE      VACQFT        WD15
022300941221     C                   MOVEL     VACQFT        WI15
022400941221     C                   MOVE      WASC15        VHCQFT
022500941221     C*
022600941221     C                   MOVE      VACTIC        VHCTIC
022700941221     C*
022800941221     C                   MOVE      VACCAS        WD15
022900941221     C                   MOVEL     VACCAS        WI15
023000941221     C                   MOVE      WASC15        VHCCAS
023100941221     C*
023200941221     C                   MOVE      VACVCA        VHCVCA
023300941221     C                   MOVE      VACCCM        VHCCCM
023400941221     C                   MOVE      VACRMN        VHCRMN
023500941221     C                   MOVE      VACRMA        VHCRMA
023600941221     C                   MOVE      VACRMO        VHCRMO
023700941221     C                   MOVE      VACFFD        VHCFFD
023800941221     C                   MOVE      VACTCR        VHCTCR
023900941221     C                   MOVE      VACDCR        VHCDCR
024000941221     C                   MOVE      VACHCR        VHCHCR
024100941221     C                   MOVE      VACDCM        VHCDCM
024200941221     C                   MOVE      VACHMC        VHCHMC
024300941221     C                   MOVE      VACTC1        VHCTC1
024400941221     C                   MOVE      VACTC2        VHCTC2
024500941221     C                   MOVE      VACCCA        VHCCCA
024600941221     C                   MOVE      VACDLA        VHCDLA
024700941221     C                   MOVE      VACDAG        VHCDAG
024800070524     C*
024900070524     C* Se richiesto nei parametri l'editazione dei campi data => gestisco
025000070524     C                   IF        %subst(prmppt:9:1) = 'D'
025100070524     C                   EVAL      VACXDTA = %trim(%editc(VACAAS:'Q'))+','+
025200070524     C                                       %trim(%editc(VACLNP:'Q'))+','+
025300070524     C                                       %trim(%editc(VACNRS:'Q'))+','+
025400070524     C                                       %trim(%editc(VACNSP:'Q'))+','+
025500070524     C                                       %trim(%editc(VACMGS:'Q'))+','+
025600070524     C                                          '"'+%trim(VACCBO)+'"'+','+
025700070524     C                                       %trim(%editc(VACLNA:'Q'))+','+
025800070524     C                                          '"'+%trim(VACRSD)+'"'+','+
025900070524     C                                          '"'+%trim(VACPRD)+'"'+','+
026000070524     C                                          '"'+%trim(VACGC1)+'"'+','+
026100070524     C                                          '"'+%trim(VACGC2)+'"'+','+
026200070524     C                                       %trim(%editc(VACCTR:'Q'))+','+
026300070524     C                                          '"'+%trim(VACCTS)+'"'+','+
026400070524     C                                          '"'+%trim(VACFTM)+'"'+','+
026500070524     C                                          '"'+%trim(VACFIN)+'"'+','+
026600070524     C                                          '"'+%trim(VACFAP)+'"'+','+
026700070524     C                                          '"'+%trim(VACTSP)+'"'+','+
026800070524     C                                       %trim(%editc(VACIAS:'Q'))+','+
026900070524     C                                          '"'+%trim(VACVAS)+'"'+','+
027000070524     C                                          '"'+%trim(VACNAS)+'"'+','+
027100070524     C                                       %trim(%editc(VACNCL:'Q'))+','+
027200070524     C                                       %trim(%editc(VACPKB:'Q'))+','+
027300070524     C                                       %trim(%editc(VACVLB:'Q'))+','+
027400070524     C                                       %trim(%editc(VACQFT:'Q'))+','+
027500070524     C                                          '"'+%trim(VACTIC)+'"'+','+
027600070524     C                                       %trim(%editc(VACCAS:'Q'))+','+
027700070524     C                                          '"'+%trim(VACVCA)+'"'+','+
027800070524     C                                       %trim(%editc(VACCCM:'Q'))+','+
027900070524     C                                       %trim(%editc(VACRMN:'Q'))+','+
028000070524     C                                          '"'+%trim(VACRMA)+'"'+','+
028100070524     C                                          '"'+%trim(VACRMO)+'"'+','+
028200070524     C                                          '"'+%trim(VACFFD)+'"'+','+
028300070524     C                                          '"'+%trim(VACTCR)+'"'+','+
028400070524     C                              %trim(%editw(VACDCR:'    /  /  '))+','+
028500070524     C                                       %trim(%editc(VACHCR:'Q'))+','+
028600070524     C                              %trim(%editw(VACDCM:'    /  /  '))+','+
028700070524     C                                       %trim(%editc(VACHMC:'Q'))+','+
028800070524     C                                          '"'+%trim(VACTC1)+'"'+','+
028900070524     C                                          '"'+%trim(VACTC2)+'"'+','+
029000070524     C                                          '"'+%trim(VACCCA)+'"'+','+
029100070524     C                              %trim(%editw(VACDLA:'    /  /  '))+','+
029200070524     C                              %trim(%editw(VACDAG:'    /  /  '))
029300070524     C                   ELSE
029400070524     C*
029500070524     C                   EVAL      VACXDTA = %trim(%editc(VACAAS:'Q'))+','+
029600070524     C                                       %trim(%editc(VACLNP:'Q'))+','+
029700070524     C                                       %trim(%editc(VACNRS:'Q'))+','+
029800070524     C                                       %trim(%editc(VACNSP:'Q'))+','+
029900070524     C                                       %trim(%editc(VACMGS:'Q'))+','+
030000070524     C                                          '"'+%trim(VACCBO)+'"'+','+
030100070524     C                                       %trim(%editc(VACLNA:'Q'))+','+
030200070524     C                                          '"'+%trim(VACRSD)+'"'+','+
030300070524     C                                          '"'+%trim(VACPRD)+'"'+','+
030400070524     C                                          '"'+%trim(VACGC1)+'"'+','+
030500070524     C                                          '"'+%trim(VACGC2)+'"'+','+
030600070524     C                                       %trim(%editc(VACCTR:'Q'))+','+
030700070524     C                                          '"'+%trim(VACCTS)+'"'+','+
030800070524     C                                          '"'+%trim(VACFTM)+'"'+','+
030900070524     C                                          '"'+%trim(VACFIN)+'"'+','+
031000070524     C                                          '"'+%trim(VACFAP)+'"'+','+
031100070524     C                                          '"'+%trim(VACTSP)+'"'+','+
031200070524     C                                       %trim(%editc(VACIAS:'Q'))+','+
031300070524     C                                          '"'+%trim(VACVAS)+'"'+','+
031400070524     C                                          '"'+%trim(VACNAS)+'"'+','+
031500070524     C                                       %trim(%editc(VACNCL:'Q'))+','+
031600070524     C                                       %trim(%editc(VACPKB:'Q'))+','+
031700070524     C                                       %trim(%editc(VACVLB:'Q'))+','+
031800070524     C                                       %trim(%editc(VACQFT:'Q'))+','+
031900070524     C                                          '"'+%trim(VACTIC)+'"'+','+
032000070524     C                                       %trim(%editc(VACCAS:'Q'))+','+
032100070524     C                                          '"'+%trim(VACVCA)+'"'+','+
032200070524     C                                       %trim(%editc(VACCCM:'Q'))+','+
032300070524     C                                       %trim(%editc(VACRMN:'Q'))+','+
032400070524     C                                          '"'+%trim(VACRMA)+'"'+','+
032500070524     C                                          '"'+%trim(VACRMO)+'"'+','+
032600070524     C                                          '"'+%trim(VACFFD)+'"'+','+
032700070524     C                                          '"'+%trim(VACTCR)+'"'+','+
032800070524     C                                       %trim(%editc(VACDCR:'Q'))+','+
032900070524     C                                       %trim(%editc(VACHCR:'Q'))+','+
033000070524     C                                       %trim(%editc(VACDCM:'Q'))+','+
033100070524     C                                       %trim(%editc(VACHMC:'Q'))+','+
033200070524     C                                          '"'+%trim(VACTC1)+'"'+','+
033300070524     C                                          '"'+%trim(VACTC2)+'"'+','+
033400070524     C                                          '"'+%trim(VACCCA)+'"'+','+
033500070524     C                                       %trim(%editc(VACDLA:'Q'))+','+
033600070524     C                                       %trim(%editc(VACDAG:'Q'))
033700070524     C                   ENDIF
033800930409     C*
033900040526     C* Effettuo determinate considerazioni x determinare in quale file scrivere il record corrente
034000040526     C* ...se evento "MESSA IN CONSEGNA" nn scrivo
034100040526     C                   IF        VACCCA = 'C'
034200040526     C                   ELSE
034300040526     C* ...se consegna ok scrivo il record nel file FNVACWWT membro 'M0'+cliente
034400041111     C*     solo spedizioni con serie. RIC. Venerini 10/11/04
034500060323      *     TUTTE le spedizioni richiesta Peretti 23/03/2006
034600040526     C                   IF        VACDCM > *zeros   AND
034700060323     C                             (VACCCA = *blanks or VACCCA = '1')
034800060323     C**!!!                        VACnrs <> *zeros
034900150813     C   43              WRITE     FNVACAA
035000040723     C                   ENDIF
035100040617     C*
035200040617     C* ...se stato intermedio scrivo il record nel file FNVACXLS membro 'M0'+cliente+'A'
035300150813     C   41              WRITE     FNVACX00
035400070622     C*
035500070622     C* Scrivo anche il 3° file (richiesta del 21/05/2007 Burani Ambra x Ced063)
035600070622     C                   EVAL      VAWDTA = VACXDTA
035700150813     C   42              WRITE     TIVAW000
035800040526     C                   ENDIF
035900040526     C*
036000040526     C* In ogni caso storicizzo il record e deleto l'input
036100080924     C                   DELETE    TIVGD000
036200991027     C*
036300080924     C                   READ      TIVGDTMP
036400040526     C                   ENDDO
036500040526     C*
036600040526     C                   EVAL      wrkesito = '0'
036700991027     C*
036800910830     C                   ENDSR
036900040526
037000040526
037100040526
037200991027
037300991027      /TITLE Operazioni iniziali.
037400991027     C     *inzsr        BEGSR
037500040526     C*
037600040526     C     *ENTRY        PLIST
037700040526     C                   parm                    prmppt
037800040526     C     wrkesito      parm      wrkesito      prmesito
037900040526     C                   parm                    prmksc
038000040526     C                   parm                    prmmbr
038100991027     C*
038200991027     C                   ENDSR
