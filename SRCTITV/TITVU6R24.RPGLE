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
008500080122     c*
008600080122     c* ...x ogni evenienza se provincia nn fosse valorizzata => fisso BO
008700080122     c                   select
008800080122     c                   when      §TLLSOPRD = *blanks
008900080122     c                   eval      §TLLSOPRD = 'BO'
009000080122     c                   endsl
009100080122     c*
009200080122     c* Forzatura x San Marino e Vaticano
009300080122     c                   select
009400080122     c                   when      §TLLSOPRD  = 'RM'    and
009500080122     c                             §TLLSOCAD  = '00120'
009600080122     c                   eval      §TLLSOPRD  = 'CV'
009700080122     c                   when      §TLLSOPRD  = 'RN'    and
009800080122     c                             §TLLSOCAD >= '47890' and
009900080122     c                             §TLLSOCAD <= '47899'
010000080122     c                   eval      §TLLSOPRD  = 'SM'
010100080122     c                   endsl
010200070907     c*
010300070907     c* Forzatrura speciale x lettere accentate
010400070907     c* - A' => À
010500080630     c                   dow       %scan(aapma:§TLLSOIND) > *zeros
010600070907     c                   eval      §TLLSOIND  = %replace(aacma:§TLLSOIND:
010700070907     c                                             %scan(aapma:§TLLSOIND))
010800080630     c                   enddo
010900080630     c                   dow       %scan(aapma:§TLLSOLOD) > *zeros
011000070907     c                   eval      §TLLSOLOD  = %replace(aacma:§TLLSOLOD:
011100070907     c                                             %scan(aapma:§TLLSOLOD))
011200080630     c                   enddo
011300080630     c                   dow       %scan(aapma:§TLLSOLODN) > *zeros
011400070907     c                   eval      §TLLSOLODN = %replace(aacma:§TLLSOLODN:
011500070907     c                                             %scan(aapma:§TLLSOLODN))
011600080630     c                   enddo
011700070907     c* - E' => È
011800080630     c                   dow       %scan(eapma:§TLLSOIND) > *zeros
011900070907     c                   eval      §TLLSOIND  = %replace(eacma:§TLLSOIND:
012000070907     c                                             %scan(eapma:§TLLSOIND))
012100080630     c                   enddo
012200080630     c                   dow       %scan(eapma:§TLLSOLOD) > *zeros
012300070907     c                   eval      §TLLSOLOD  = %replace(eacma:§TLLSOLOD:
012400070907     c                                             %scan(eapma:§TLLSOLOD))
012500080630     c                   enddo
012600080630     c                   dow       %scan(eapma:§TLLSOLODN) > *zeros
012700070907     c                   eval      §TLLSOLODN = %replace(eacma:§TLLSOLODN:
012800070907     c                                             %scan(eapma:§TLLSOLODN))
012900080630     c                   enddo
013000070907     c* - I' => Ì
013100080630     c                   dow       %scan(iapma:§TLLSOIND) > *zeros
013200070907     c                   eval      §TLLSOIND  = %replace(iacma:§TLLSOIND:
013300070907     c                                             %scan(iapma:§TLLSOIND))
013400080630     c                   enddo
013500080630     c                   dow       %scan(iapma:§TLLSOLOD) > *zeros
013600070907     c                   eval      §TLLSOLOD  = %replace(iacma:§TLLSOLOD:
013700070907     c                                             %scan(iapma:§TLLSOLOD))
013800080630     c                   enddo
013900080630     c                   dow       %scan(iapma:§TLLSOLODN) > *zeros
014000070907     c                   eval      §TLLSOLODN = %replace(iacma:§TLLSOLODN:
014100070907     c                                             %scan(iapma:§TLLSOLODN))
014200080630     c                   enddo
014300070907     c* - O' => Ò
014400080630     c                   dow       %scan(oapma:§TLLSOIND) > *zeros
014500070907     c                   eval      §TLLSOIND  = %replace(oacma:§TLLSOIND:
014600070907     c                                             %scan(oapma:§TLLSOIND))
014700080630     c                   enddo
014800080630     c                   dow       %scan(oapma:§TLLSOLOD) > *zeros
014900070907     c                   eval      §TLLSOLOD  = %replace(oacma:§TLLSOLOD:
015000070907     c                                             %scan(oapma:§TLLSOLOD))
015100080630     c                   enddo
015200080630     c                   dow       %scan(oapma:§TLLSOLODN) > *zeros
015300070907     c                   eval      §TLLSOLODN = %replace(oacma:§TLLSOLODN:
015400070907     c                                             %scan(oapma:§TLLSOLODN))
015500080630     c                   enddo
015600070907     c* - U' => Ù
015700080630     c                   dow       %scan(uapma:§TLLSOIND) > *zeros
015800070907     c                   eval      §TLLSOIND  = %replace(uacma:§TLLSOIND:
015900070907     c                                             %scan(uapma:§TLLSOIND))
016000080630     c                   enddo
016100080630     c                   dow       %scan(uapma:§TLLSOLOD) > *zeros
016200070907     c                   eval      §TLLSOLOD  = %replace(uacma:§TLLSOLOD:
016300070907     c                                             %scan(uapma:§TLLSOLOD))
016400080630     c                   enddo
016500080630     c                   dow       %scan(uapma:§TLLSOLODN) > *zeros
016600070907     c                   eval      §TLLSOLODN = %replace(uacma:§TLLSOLODN:
016700070907     c                                             %scan(uapma:§TLLSOLODN))
016800080630     c                   enddo
016900070907     c* - a' => à
017000080630     c                   dow       %scan(aapmi:§TLLSOIND) > *zeros
017100070907     c                   eval      §TLLSOIND  = %replace(aacmi:§TLLSOIND:
017200070907     c                                             %scan(aapmi:§TLLSOIND))
017300080630     c                   enddo
017400080630     c                   dow       %scan(aapmi:§TLLSOLOD) > *zeros
017500070907     c                   eval      §TLLSOLOD  = %replace(aacmi:§TLLSOLOD:
017600070907     c                                             %scan(aapmi:§TLLSOLOD))
017700080630     c                   enddo
017800080630     c                   dow       %scan(aapmi:§TLLSOLODN) > *zeros
017900070907     c                   eval      §TLLSOLODN = %replace(aacmi:§TLLSOLODN:
018000070907     c                                             %scan(aapmi:§TLLSOLODN))
018100080630     c                   enddo
018200070907     c* - e' => è
018300080630     c                   dow       %scan(eapmi:§TLLSOIND) > *zeros
018400070907     c                   eval      §TLLSOIND  = %replace(eacmi:§TLLSOIND:
018500070907     c                                             %scan(eapmi:§TLLSOIND))
018600080630     c                   enddo
018700080630     c                   dow       %scan(eapmi:§TLLSOLOD) > *zeros
018800070907     c                   eval      §TLLSOLOD  = %replace(eacmi:§TLLSOLOD:
018900070907     c                                             %scan(eapmi:§TLLSOLOD))
019000080630     c                   enddo
019100080630     c                   dow       %scan(eapmi:§TLLSOLODN) > *zeros
019200070907     c                   eval      §TLLSOLODN = %replace(eacmi:§TLLSOLODN:
019300070907     c                                             %scan(eapmi:§TLLSOLODN))
019400080630     c                   enddo
019500070907     c* - i' => ì
019600080630     c                   dow       %scan(iapmi:§TLLSOIND) > *zeros
019700070907     c                   eval      §TLLSOIND  = %replace(iacmi:§TLLSOIND:
019800070907     c                                             %scan(iapmi:§TLLSOIND))
019900080630     c                   enddo
020000080630     c                   dow       %scan(iapmi:§TLLSOLOD) > *zeros
020100070907     c                   eval      §TLLSOLOD  = %replace(iacmi:§TLLSOLOD:
020200070907     c                                             %scan(iapmi:§TLLSOLOD))
020300080630     c                   enddo
020400080630     c                   dow       %scan(iapmi:§TLLSOLODN) > *zeros
020500070907     c                   eval      §TLLSOLODN = %replace(iacmi:§TLLSOLODN:
020600070907     c                                             %scan(iapmi:§TLLSOLODN))
020700080630     c                   enddo
020800070907     c* - o' => ò
020900080630     c                   dow       %scan(oapmi:§TLLSOIND) > *zeros
021000070907     c                   eval      §TLLSOIND  = %replace(oacmi:§TLLSOIND:
021100070907     c                                             %scan(oapmi:§TLLSOIND))
021200080630     c                   enddo
021300080630     c                   dow       %scan(oapmi:§TLLSOLOD) > *zeros
021400070907     c                   eval      §TLLSOLOD  = %replace(oacmi:§TLLSOLOD:
021500070907     c                                             %scan(oapmi:§TLLSOLOD))
021600080630     c                   enddo
021700080630     c                   dow       %scan(oapmi:§TLLSOLODN) > *zeros
021800070907     c                   eval      §TLLSOLODN = %replace(oacmi:§TLLSOLODN:
021900070907     c                                             %scan(oapmi:§TLLSOLODN))
022000080630     c                   enddo
022100070907     c* - u' => ù
022200080630     c                   dow       %scan(uapmi:§TLLSOIND) > *zeros
022300070907     c                   eval      §TLLSOIND  = %replace(uacmi:§TLLSOIND:
022400070907     c                                             %scan(uapmi:§TLLSOIND))
022500080630     c                   enddo
022600080630     c                   dow       %scan(uapmi:§TLLSOLOD) > *zeros
022700070907     c                   eval      §TLLSOLOD  = %replace(uacmi:§TLLSOLOD:
022800070907     c                                             %scan(uapmi:§TLLSOLOD))
022900080630     c                   enddo
023000080630     c                   dow       %scan(uapmi:§TLLSOLODN) > *zeros
023100070907     c                   eval      §TLLSOLODN = %replace(uacmi:§TLLSOLODN:
023200070907     c                                             %scan(uapmi:§TLLSOLODN))
023300080630     c                   enddo
023400070913     c*
023500070913     c* Forzatrura speciale x strade statali
023600080630     c                   dow       %scan('s.s.':§TLLSOIND) > *zeros
023700070913     c                   eval      §TLLSOIND = %replace('ss  ':§TLLSOIND:
023800070913     c                                            %scan('s.s.':§TLLSOIND))
023900080630     c                   enddo
024000080630     c                   dow       %scan('S.S.':§TLLSOIND) > *zeros
024100070913     c                   eval      §TLLSOIND = %replace('SS  ':§TLLSOIND:
024200070913     c                                            %scan('S.S.':§TLLSOIND))
024300080630     c                   enddo
024400101129     C*
024500101129     C* Effettuo considerazioni relative alle modifiche CAP PT 10/2010
024600101129     C*
024700101129     C* ...solo le provincia interessate: MB / FM / BT / RN / TN / PR / AP
024800121120 xxx C***                if        §TLLSOPRD = 'MB' or
024900121228     C***                          §TLLSOPRD = 'FM' or
025000121228     C***                          §TLLSOPRD = 'BT' or
025100121228     C***                          §TLLSOPRD = 'RN' or
025200121228     C***                          §TLLSOPRD = 'TN' or
025300121228     C***                          §TLLSOPRD = 'PR' or
025400121228     C***                          §TLLSOPRD = 'AP'
025500101129     C*
025600121228     C***                eval      NEWL_PRV = §TLLSOPRD
025700121228     C***                eval      NEWL_CAP = §TLLSOCAD
025800121228     C***                if        §TLLSOLODN <> *blanks
025900121228     C***                eval      NEWL_LOC = §TLLSOLODN
026000121228     C***                else
026100121228     C***                eval      NEWL_LOC = §TLLSOLOD
026200121228     C***                endif
026300101129     C*
026400101129     C* 1° tentativo => PRV + CAP + LOC
026500121228     C***  KEYcpmod2_PCL chain     wacpmod2l
026600121228     C***                if        %found(wacpmod2l)
026700121228     C***                eval      §TLLSOPRD = OLDL_PRV
026800121228     C***                eval      §TLLSOCAD = OLDL_CAP
026900121228     C***                else
027000101129     C* 2° tentativo => PRV + CAP
027100121228     C***  KEYcpmod2_PC  chain     wacpmod2l
027200121228     C***                if        %found(wacpmod2l)
027300121228     C***                eval      §TLLSOPRD = OLDL_PRV
027400121228     C***                eval      §TLLSOCAD = OLDL_CAP
027500121228     C***                else
027600101201     C* 3° tentativo => PRV (solo x nuove provincie)
027700121120 xxx C***                if        §TLLSOPRD = 'MB' or
027800121228     C***                          §TLLSOPRD = 'FM' or
027900121228     C***                          §TLLSOPRD = 'BT'
028000121228     C***  KEYcpmod2_P   chain     wacpmod2l
028100121228     C***                if        %found(wacpmod2l)
028200121228     C***                eval      §TLLSOPRD = OLDL_PRV
028300121228     C***                endif
028400121228     C***                endif
028500121228     C***                endif
028600121228     C***                endif
028700121228     C***                endif
028800050121     C*
028900050121     C* Inizializzazione campi e variabili d out
029000060804     C                   clear                   tivaw000
029100060804     C*
029200070913     C                   eval      wRecOut = %subst(dtllso:1:157)
029300060804     C*
029400060804     C* Effettuo determinate considerazioni x portare nella forma ASCII editata con segno
029500060804     C* le coordinale geo-stazionarie
029600060804     C                   if        §tllsoXLO < *zeros
029700060804     C                   eval      wNum11_8 = %abs(§tllsoXLO)
029800060804     C                   eval      wSegno = '-'
029900060804     C                   else
030000060804     C                   eval      wNum11_8 = §tllsoXLO
030100060804     C                   eval      wSegno = *blanks
030200060804     C                   endif
030300060804     C                   move(p)   wNum11_8      wAlfa11
030400060804     C                   eval      wRecOut = wRecOut + wSegno +
030500060804     C                                       %subst(wAlfa11:1:3) + wSepDec +
030600060804     C                                       %subst(wAlfa11:4:8)
030700060804     C*
030800060804     C                   if        §tllsoYLA < *zeros
030900060804     C                   eval      wNum11_8 = %abs(§tllsoYLA)
031000060804     C                   eval      wSegno = '-'
031100060804     C                   else
031200060804     C                   eval      wNum11_8 = §tllsoYLA
031300060804     C                   eval      wSegno = *blanks
031400060804     C                   endif
031500060804     C                   move(p)   wNum11_8      wAlfa11
031600060804     C                   eval      wRecOut = wRecOut + wSegno +
031700060804     C                                       %subst(wAlfa11:1:3) + wSepDec +
031800060804     C                                       %subst(wAlfa11:4:8)
031900070112     C*
032000070112     C* Quindi proseguo con il resto dei dati
032100070112     C                   eval      wRecOut = %subst(wRecOut:1:183) +
032200070913     C                             %subst(vgdDTA:180:%len(dtllso)-180+1)
032300060804     C*
032400060804     C* Valorizzo il buffer del file d output
032500060804     C                   eval      vawdta = wRecOut
032600930409     C*
032700050121     C                   WRITE     TIVAW000
032800050909     C                   DELETE    TIVGD000
032900991027     C*
033000050909     C                   READ      TIVGDTMP
033100991027     C                   ENDdo
033200991027     C*
033300991027     C                   eval      wrkesito = '0'
033400991027     C*
033500910830     C                   ENDSR
033600060804
033700991027
033800991027      /TITLE Operazioni iniziali.
033900991027     C     *inzsr        BEGSR
034000991027     C*
034100991027     C     *ENTRY        PLIST
034200991027     C                   parm                    prmppt
034300991027     C     wrkesito      parm      wrkesito      prmesito
034400991027     C*
034500101129     C* Definizione chiave su WACPMOD2L  - Completa: PRV / CAP / LOC
034600101129     C     KEYcpmod2_PCL KLIST
034700101129     C                   KFLD                    NEWL_PRV
034800101129     C                   KFLD                    NEWL_CAP
034900101129     C                   KFLD                    NEWL_LOC
035000101129     C*
035100101129     C* Definizione chiave su WACPMOD2L  - Parziale: PRV / CAP
035200101129     C     KEYcpmod2_PC  KLIST
035300101129     C                   KFLD                    NEWL_PRV
035400101129     C                   KFLD                    NEWL_CAP
035500101129     C*
035600101129     C* Definizione chiave su WACPMOD2L  - Parziale: PRV
035700101129     C     KEYcpmod2_P   KLIST
035800101129     C                   KFLD                    NEWL_PRV
035900101129     C*
036000991027     C                   ENDSR
