000100890921     H DECEDIT('0,') DATEDIT(*DMY.)
000200991027     H dftactgrp(*yes)
000300991027
000400050909     FTIVGDTMP  UF   E             DISK
000500101129     Fwacpmod2l IF   E           K DISK
000600050121     Ftivawwwt  O    E             DISK
000700991027
000800060804     D dtllso        e ds
000900991027     D prmppt          s             50
001000991027     D prmesito        s              1
001100991027     D wrkesito        s                   like(prmesito)
001200060804     D wNum11_8        s             11  8 inz
001300060804     D wAlfa11         s             11    inz
001400060804     D wSegno          s              1    inz
001500060804     D wSepDec         s              1    inz(',')
001600070112     D wRecOut         s           1024    varying
001700070907
001800070907     D*
001900070907     D aapma           s              2    inz('A''')
002000070907     D aacma           s              2    inz('À ')
002100070907     D eapma           s              2    inz('E''')
002200070907     D eacma           s              2    inz('È ')
002300070907     D iapma           s              2    inz('I''')
002400070907     D iacma           s              2    inz('Ì ')
002500070907     D oapma           s              2    inz('O''')
002600070907     D oacma           s              2    inz('Ò ')
002700070907     D uapma           s              2    inz('U''')
002800070907     D uacma           s              2    inz('Ù ')
002900070907     D aapmi           s              2    inz('a''')
003000070907     D aacmi           s              2    inz('à ')
003100070907     D eapmi           s              2    inz('e''')
003200070907     D eacmi           s              2    inz('è ')
003300070907     D iapmi           s              2    inz('i''')
003400070907     D iacmi           s              2    inz('ì ')
003500070907     D oapmi           s              2    inz('o''')
003600070907     D oacmi           s              2    inz('ò ')
003700070907     D uapmi           s              2    inz('u''')
003800070907     D uacmi           s              2    inz('ù ')
003900070907     D*
004000070907
004100070907
004200060804     C                   exsr      traduci
004300060804     C                   seton                                        LR
004400991027
004500060804
004600991027     C     traduci       BEGSR
004700991027     C*
004800060804     C                   read      TIVGDTMP
004900991027     C                   dow       not %eof
005000060804     C                   eval      dtllso = vgdDTA
005100080122     c*
005200080122     c* Forzatrura speciale x "CENTERGROSS"
005300080122     c* (in quanto nn esiste in cartografia ma nel ns cappario invece sì)
005400080122     c                   if        §tllsoPRD  = 'BO'                 AND
005500080122     c                             %trim(§tllsoLOD)  = 'CENTERGROSS' AND
005600080122     c                             %trim(§tllsoLODN) = *blanks
005700080122     C                   eval      §tllsoLODN  = 'FUNO'
005800080122     c                   endif
005900080122     c                   if        §tllsoPRD  = 'BO'                 AND
006000080122     c                             %trim(§tllsoLODN) = 'CENTERGROSS'
006100080122     C                   eval      §tllsoLODN  = 'FUNO'
006200080122     c                   endif
006300150112     c*
006400150112     c* Forzatrura speciale x "INTERPORTO BENTIVOGLIO"
006500150112     c* (in quanto nn esiste in cartografia ma nel ns cappario invece sì)
006600150112     c                   if        §tllsoPRD  = 'BO'                 AND
006700150112     c                             %trim(§tllsoLODN) = 'INTERPORTO BENTIVOGLIO'
006800150112     C                   if        %scan('INTERPORTO':§tllsoIND) = *zeros
006900150112     c                   eval      §tllsoLODN  = 'BENTIVOGLIO'
007000150112     C                   eval      §tllsoIND   = 'INTERPORTO '+%trim(§tllsoIND)
007100150112     c                   endif
007200150112     c                   endif
007300080122     c*
007400080122     c* Forzatrura speciale x "CASCINE VICA"
007500080122     c* (in quanto nn esiste in cartografia ma nel ns cappario invece sì)
007600080122     c                   if        §tllsoPRD  = 'TO'                  AND
007700080122     c                             %trim(§tllsoLOD)  = 'CASCINE VICA' AND
007800080122     c                             %trim(§tllsoLODN) = *blanks
007900080122     C                   eval      §tllsoLODN  = 'RIVOLI'
008000080122     c                   endif
008100080122     c                   if        §tllsoPRD  = 'TO'                  AND
008200080122     c                             %trim(§tllsoLODN) = 'CASCINE VICA'
008300080122     C                   eval      §tllsoLODN  = 'RIVOLI'
008400080122     c                   endif
008500150624     c*
008600150624     c* Forzatrura speciale x "BELLARIA"
008700150624     c* (in quanto nn esiste in cartografia ma nel ns cappario invece sì)
008800150624     c                   if        §tllsoPRD  = 'RN'                  AND
008900150624     c                             §tllsoCAD  = '47814'               AND
009000150624     c                             %trim(§tllsoLOD)  = 'BELLARIA'     AND
009100150624     c                             %trim(§tllsoLODN) = *blanks
009200150624     C                   eval      §tllsoLODN  = 'BELLARIA-IGEA MARINA'
009300150624     c                   endif
009400150624     c                   if        §tllsoPRD  = 'RN'                  AND
009500150624     c                             §tllsoCAD  = '47814'               AND
009600150624     c                             %trim(§tllsoLODN) = 'BELLARIA'
009700150624     C                   eval      §tllsoLODN  = 'BELLARIA-IGEA MARINA'
009800150624     c                   endif
009900080122     c*
010000080122     c* ...x ogni evenienza se provincia nn fosse valorizzata => fisso BO
010100080122     c                   select
010200080122     c                   when      §TLLSOPRD = *blanks
010300080122     c                   eval      §TLLSOPRD = 'BO'
010400080122     c                   endsl
010500080122     c*
010600080122     c* Forzatura x San Marino e Vaticano
010700080122     c                   select
010800080122     c                   when      §TLLSOPRD  = 'RM'    and
010900080122     c                             §TLLSOCAD  = '00120'
011000080122     c                   eval      §TLLSOPRD  = 'CV'
011100080122     c                   when      §TLLSOPRD  = 'RN'    and
011200080122     c                             §TLLSOCAD >= '47890' and
011300080122     c                             §TLLSOCAD <= '47899'
011400080122     c                   eval      §TLLSOPRD  = 'SM'
011500080122     c                   endsl
011600070907     c*
011700070907     c* Forzatrura speciale x lettere accentate
011800070907     c* - A' => À
011900080630     c                   dow       %scan(aapma:§TLLSOIND) > *zeros
012000070907     c                   eval      §TLLSOIND  = %replace(aacma:§TLLSOIND:
012100070907     c                                             %scan(aapma:§TLLSOIND))
012200080630     c                   enddo
012300080630     c                   dow       %scan(aapma:§TLLSOLOD) > *zeros
012400070907     c                   eval      §TLLSOLOD  = %replace(aacma:§TLLSOLOD:
012500070907     c                                             %scan(aapma:§TLLSOLOD))
012600080630     c                   enddo
012700080630     c                   dow       %scan(aapma:§TLLSOLODN) > *zeros
012800070907     c                   eval      §TLLSOLODN = %replace(aacma:§TLLSOLODN:
012900070907     c                                             %scan(aapma:§TLLSOLODN))
013000080630     c                   enddo
013100070907     c* - E' => È
013200080630     c                   dow       %scan(eapma:§TLLSOIND) > *zeros
013300070907     c                   eval      §TLLSOIND  = %replace(eacma:§TLLSOIND:
013400070907     c                                             %scan(eapma:§TLLSOIND))
013500080630     c                   enddo
013600080630     c                   dow       %scan(eapma:§TLLSOLOD) > *zeros
013700070907     c                   eval      §TLLSOLOD  = %replace(eacma:§TLLSOLOD:
013800070907     c                                             %scan(eapma:§TLLSOLOD))
013900080630     c                   enddo
014000080630     c                   dow       %scan(eapma:§TLLSOLODN) > *zeros
014100070907     c                   eval      §TLLSOLODN = %replace(eacma:§TLLSOLODN:
014200070907     c                                             %scan(eapma:§TLLSOLODN))
014300080630     c                   enddo
014400070907     c* - I' => Ì
014500080630     c                   dow       %scan(iapma:§TLLSOIND) > *zeros
014600070907     c                   eval      §TLLSOIND  = %replace(iacma:§TLLSOIND:
014700070907     c                                             %scan(iapma:§TLLSOIND))
014800080630     c                   enddo
014900080630     c                   dow       %scan(iapma:§TLLSOLOD) > *zeros
015000070907     c                   eval      §TLLSOLOD  = %replace(iacma:§TLLSOLOD:
015100070907     c                                             %scan(iapma:§TLLSOLOD))
015200080630     c                   enddo
015300080630     c                   dow       %scan(iapma:§TLLSOLODN) > *zeros
015400070907     c                   eval      §TLLSOLODN = %replace(iacma:§TLLSOLODN:
015500070907     c                                             %scan(iapma:§TLLSOLODN))
015600080630     c                   enddo
015700070907     c* - O' => Ò
015800080630     c                   dow       %scan(oapma:§TLLSOIND) > *zeros
015900070907     c                   eval      §TLLSOIND  = %replace(oacma:§TLLSOIND:
016000070907     c                                             %scan(oapma:§TLLSOIND))
016100080630     c                   enddo
016200080630     c                   dow       %scan(oapma:§TLLSOLOD) > *zeros
016300070907     c                   eval      §TLLSOLOD  = %replace(oacma:§TLLSOLOD:
016400070907     c                                             %scan(oapma:§TLLSOLOD))
016500080630     c                   enddo
016600080630     c                   dow       %scan(oapma:§TLLSOLODN) > *zeros
016700070907     c                   eval      §TLLSOLODN = %replace(oacma:§TLLSOLODN:
016800070907     c                                             %scan(oapma:§TLLSOLODN))
016900080630     c                   enddo
017000070907     c* - U' => Ù
017100080630     c                   dow       %scan(uapma:§TLLSOIND) > *zeros
017200070907     c                   eval      §TLLSOIND  = %replace(uacma:§TLLSOIND:
017300070907     c                                             %scan(uapma:§TLLSOIND))
017400080630     c                   enddo
017500080630     c                   dow       %scan(uapma:§TLLSOLOD) > *zeros
017600070907     c                   eval      §TLLSOLOD  = %replace(uacma:§TLLSOLOD:
017700070907     c                                             %scan(uapma:§TLLSOLOD))
017800080630     c                   enddo
017900080630     c                   dow       %scan(uapma:§TLLSOLODN) > *zeros
018000070907     c                   eval      §TLLSOLODN = %replace(uacma:§TLLSOLODN:
018100070907     c                                             %scan(uapma:§TLLSOLODN))
018200080630     c                   enddo
018300070907     c* - a' => à
018400080630     c                   dow       %scan(aapmi:§TLLSOIND) > *zeros
018500070907     c                   eval      §TLLSOIND  = %replace(aacmi:§TLLSOIND:
018600070907     c                                             %scan(aapmi:§TLLSOIND))
018700080630     c                   enddo
018800080630     c                   dow       %scan(aapmi:§TLLSOLOD) > *zeros
018900070907     c                   eval      §TLLSOLOD  = %replace(aacmi:§TLLSOLOD:
019000070907     c                                             %scan(aapmi:§TLLSOLOD))
019100080630     c                   enddo
019200080630     c                   dow       %scan(aapmi:§TLLSOLODN) > *zeros
019300070907     c                   eval      §TLLSOLODN = %replace(aacmi:§TLLSOLODN:
019400070907     c                                             %scan(aapmi:§TLLSOLODN))
019500080630     c                   enddo
019600070907     c* - e' => è
019700080630     c                   dow       %scan(eapmi:§TLLSOIND) > *zeros
019800070907     c                   eval      §TLLSOIND  = %replace(eacmi:§TLLSOIND:
019900070907     c                                             %scan(eapmi:§TLLSOIND))
020000080630     c                   enddo
020100080630     c                   dow       %scan(eapmi:§TLLSOLOD) > *zeros
020200070907     c                   eval      §TLLSOLOD  = %replace(eacmi:§TLLSOLOD:
020300070907     c                                             %scan(eapmi:§TLLSOLOD))
020400080630     c                   enddo
020500080630     c                   dow       %scan(eapmi:§TLLSOLODN) > *zeros
020600070907     c                   eval      §TLLSOLODN = %replace(eacmi:§TLLSOLODN:
020700070907     c                                             %scan(eapmi:§TLLSOLODN))
020800080630     c                   enddo
020900070907     c* - i' => ì
021000080630     c                   dow       %scan(iapmi:§TLLSOIND) > *zeros
021100070907     c                   eval      §TLLSOIND  = %replace(iacmi:§TLLSOIND:
021200070907     c                                             %scan(iapmi:§TLLSOIND))
021300080630     c                   enddo
021400080630     c                   dow       %scan(iapmi:§TLLSOLOD) > *zeros
021500070907     c                   eval      §TLLSOLOD  = %replace(iacmi:§TLLSOLOD:
021600070907     c                                             %scan(iapmi:§TLLSOLOD))
021700080630     c                   enddo
021800080630     c                   dow       %scan(iapmi:§TLLSOLODN) > *zeros
021900070907     c                   eval      §TLLSOLODN = %replace(iacmi:§TLLSOLODN:
022000070907     c                                             %scan(iapmi:§TLLSOLODN))
022100080630     c                   enddo
022200070907     c* - o' => ò
022300080630     c                   dow       %scan(oapmi:§TLLSOIND) > *zeros
022400070907     c                   eval      §TLLSOIND  = %replace(oacmi:§TLLSOIND:
022500070907     c                                             %scan(oapmi:§TLLSOIND))
022600080630     c                   enddo
022700080630     c                   dow       %scan(oapmi:§TLLSOLOD) > *zeros
022800070907     c                   eval      §TLLSOLOD  = %replace(oacmi:§TLLSOLOD:
022900070907     c                                             %scan(oapmi:§TLLSOLOD))
023000080630     c                   enddo
023100080630     c                   dow       %scan(oapmi:§TLLSOLODN) > *zeros
023200070907     c                   eval      §TLLSOLODN = %replace(oacmi:§TLLSOLODN:
023300070907     c                                             %scan(oapmi:§TLLSOLODN))
023400080630     c                   enddo
023500070907     c* - u' => ù
023600080630     c                   dow       %scan(uapmi:§TLLSOIND) > *zeros
023700070907     c                   eval      §TLLSOIND  = %replace(uacmi:§TLLSOIND:
023800070907     c                                             %scan(uapmi:§TLLSOIND))
023900080630     c                   enddo
024000080630     c                   dow       %scan(uapmi:§TLLSOLOD) > *zeros
024100070907     c                   eval      §TLLSOLOD  = %replace(uacmi:§TLLSOLOD:
024200070907     c                                             %scan(uapmi:§TLLSOLOD))
024300080630     c                   enddo
024400080630     c                   dow       %scan(uapmi:§TLLSOLODN) > *zeros
024500070907     c                   eval      §TLLSOLODN = %replace(uacmi:§TLLSOLODN:
024600070907     c                                             %scan(uapmi:§TLLSOLODN))
024700080630     c                   enddo
024800070913     c*
024900070913     c* Forzatrura speciale x strade statali
025000080630     c                   dow       %scan('s.s.':§TLLSOIND) > *zeros
025100070913     c                   eval      §TLLSOIND = %replace('ss  ':§TLLSOIND:
025200070913     c                                            %scan('s.s.':§TLLSOIND))
025300080630     c                   enddo
025400080630     c                   dow       %scan('S.S.':§TLLSOIND) > *zeros
025500070913     c                   eval      §TLLSOIND = %replace('SS  ':§TLLSOIND:
025600070913     c                                            %scan('S.S.':§TLLSOIND))
025700080630     c                   enddo
025800101129     C*
025900101129     C* Effettuo considerazioni relative alle modifiche CAP PT 10/2010
026000101129     C*
026100101129     C* ...solo le provincia interessate: MB / FM / BT / RN / TN / PR / AP
026200121120 xxx C***                if        §TLLSOPRD = 'MB' or
026300121228     C***                          §TLLSOPRD = 'FM' or
026400121228     C***                          §TLLSOPRD = 'BT' or
026500121228     C***                          §TLLSOPRD = 'RN' or
026600121228     C***                          §TLLSOPRD = 'TN' or
026700121228     C***                          §TLLSOPRD = 'PR' or
026800121228     C***                          §TLLSOPRD = 'AP'
026900101129     C*
027000121228     C***                eval      NEWL_PRV = §TLLSOPRD
027100121228     C***                eval      NEWL_CAP = §TLLSOCAD
027200121228     C***                if        §TLLSOLODN <> *blanks
027300121228     C***                eval      NEWL_LOC = §TLLSOLODN
027400121228     C***                else
027500121228     C***                eval      NEWL_LOC = §TLLSOLOD
027600121228     C***                endif
027700101129     C*
027800101129     C* 1° tentativo => PRV + CAP + LOC
027900121228     C***  KEYcpmod2_PCL chain     wacpmod2l
028000121228     C***                if        %found(wacpmod2l)
028100121228     C***                eval      §TLLSOPRD = OLDL_PRV
028200121228     C***                eval      §TLLSOCAD = OLDL_CAP
028300121228     C***                else
028400101129     C* 2° tentativo => PRV + CAP
028500121228     C***  KEYcpmod2_PC  chain     wacpmod2l
028600121228     C***                if        %found(wacpmod2l)
028700121228     C***                eval      §TLLSOPRD = OLDL_PRV
028800121228     C***                eval      §TLLSOCAD = OLDL_CAP
028900121228     C***                else
029000101201     C* 3° tentativo => PRV (solo x nuove provincie)
029100121120 xxx C***                if        §TLLSOPRD = 'MB' or
029200121228     C***                          §TLLSOPRD = 'FM' or
029300121228     C***                          §TLLSOPRD = 'BT'
029400121228     C***  KEYcpmod2_P   chain     wacpmod2l
029500121228     C***                if        %found(wacpmod2l)
029600121228     C***                eval      §TLLSOPRD = OLDL_PRV
029700121228     C***                endif
029800121228     C***                endif
029900121228     C***                endif
030000121228     C***                endif
030100121228     C***                endif
030200050121     C*
030300050121     C* Inizializzazione campi e variabili d out
030400060804     C                   clear                   tivaw000
030500060804     C*
030600070913     C                   eval      wRecOut = %subst(dtllso:1:157)
030700060804     C*
030800060804     C* Effettuo determinate considerazioni x portare nella forma ASCII editata con segno
030900060804     C* le coordinale geo-stazionarie
031000060804     C                   if        §tllsoXLO < *zeros
031100060804     C                   eval      wNum11_8 = %abs(§tllsoXLO)
031200060804     C                   eval      wSegno = '-'
031300060804     C                   else
031400060804     C                   eval      wNum11_8 = §tllsoXLO
031500060804     C                   eval      wSegno = *blanks
031600060804     C                   endif
031700060804     C                   move(p)   wNum11_8      wAlfa11
031800060804     C                   eval      wRecOut = wRecOut + wSegno +
031900060804     C                                       %subst(wAlfa11:1:3) + wSepDec +
032000060804     C                                       %subst(wAlfa11:4:8)
032100060804     C*
032200060804     C                   if        §tllsoYLA < *zeros
032300060804     C                   eval      wNum11_8 = %abs(§tllsoYLA)
032400060804     C                   eval      wSegno = '-'
032500060804     C                   else
032600060804     C                   eval      wNum11_8 = §tllsoYLA
032700060804     C                   eval      wSegno = *blanks
032800060804     C                   endif
032900060804     C                   move(p)   wNum11_8      wAlfa11
033000060804     C                   eval      wRecOut = wRecOut + wSegno +
033100060804     C                                       %subst(wAlfa11:1:3) + wSepDec +
033200060804     C                                       %subst(wAlfa11:4:8)
033300070112     C*
033400070112     C* Quindi proseguo con il resto dei dati
033500070112     C                   eval      wRecOut = %subst(wRecOut:1:183) +
033600070913     C                             %subst(vgdDTA:180:%len(dtllso)-180+1)
033700060804     C*
033800060804     C* Valorizzo il buffer del file d output
033900060804     C                   eval      vawdta = wRecOut
034000930409     C*
034100050121     C                   WRITE     TIVAW000
034200050909     C                   DELETE    TIVGD000
034300991027     C*
034400050909     C                   READ      TIVGDTMP
034500991027     C                   ENDdo
034600991027     C*
034700991027     C                   eval      wrkesito = '0'
034800991027     C*
034900910830     C                   ENDSR
035000060804
035100991027
035200991027      /TITLE Operazioni iniziali.
035300991027     C     *inzsr        BEGSR
035400991027     C*
035500991027     C     *ENTRY        PLIST
035600991027     C                   parm                    prmppt
035700991027     C     wrkesito      parm      wrkesito      prmesito
035800991027     C*
035900101129     C* Definizione chiave su WACPMOD2L  - Completa: PRV / CAP / LOC
036000101129     C     KEYcpmod2_PCL KLIST
036100101129     C                   KFLD                    NEWL_PRV
036200101129     C                   KFLD                    NEWL_CAP
036300101129     C                   KFLD                    NEWL_LOC
036400101129     C*
036500101129     C* Definizione chiave su WACPMOD2L  - Parziale: PRV / CAP
036600101129     C     KEYcpmod2_PC  KLIST
036700101129     C                   KFLD                    NEWL_PRV
036800101129     C                   KFLD                    NEWL_CAP
036900101129     C*
037000101129     C* Definizione chiave su WACPMOD2L  - Parziale: PRV
037100101129     C     KEYcpmod2_P   KLIST
037200101129     C                   KFLD                    NEWL_PRV
037300101129     C*
037400991027     C                   ENDSR
