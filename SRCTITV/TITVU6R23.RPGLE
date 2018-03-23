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
006300080122     c*
006400080122     c* Forzatrura speciale x "CASCINE VICA"
006500080122     c* (in quanto nn esiste in cartografia ma nel ns cappario invece sì)
006600080122     c                   if        §tllsoPRD  = 'TO'                  AND
006700080122     c                             %trim(§tllsoLOD)  = 'CASCINE VICA' AND
006800080122     c                             %trim(§tllsoLODN) = *blanks
006900080122     C                   eval      §tllsoLODN  = 'RIVOLI'
007000080122     c                   endif
007100080122     c                   if        §tllsoPRD  = 'TO'                  AND
007200080122     c                             %trim(§tllsoLODN) = 'CASCINE VICA'
007300080122     C                   eval      §tllsoLODN  = 'RIVOLI'
007400080122     c                   endif
007500080122     c*
007600080122     c* ...x ogni evenienza se provincia nn fosse valorizzata => fisso BO
007700080122     c                   select
007800080122     c                   when      §TLLSOPRD = *blanks
007900080122     c                   eval      §TLLSOPRD = 'BO'
008000080122     c                   endsl
008100080122     c*
008200080122     c* Forzatura x San Marino e Vaticano
008300080122     c                   select
008400080122     c                   when      §TLLSOPRD  = 'RM'    and
008500080122     c                             §TLLSOCAD  = '00120'
008600080122     c                   eval      §TLLSOPRD  = 'CV'
008700080122     c                   when      §TLLSOPRD  = 'RN'    and
008800080122     c                             §TLLSOCAD >= '47890' and
008900080122     c                             §TLLSOCAD <= '47899'
009000080122     c                   eval      §TLLSOPRD  = 'SM'
009100080122     c                   endsl
009200070907     c*
009300070907     c* Forzatrura speciale x lettere accentate
009400070907     c* - A' => À
009500080630     c                   dow       %scan(aapma:§TLLSOIND) > *zeros
009600070907     c                   eval      §TLLSOIND  = %replace(aacma:§TLLSOIND:
009700070907     c                                             %scan(aapma:§TLLSOIND))
009800080630     c                   enddo
009900080630     c                   dow       %scan(aapma:§TLLSOLOD) > *zeros
010000070907     c                   eval      §TLLSOLOD  = %replace(aacma:§TLLSOLOD:
010100070907     c                                             %scan(aapma:§TLLSOLOD))
010200080630     c                   enddo
010300080630     c                   dow       %scan(aapma:§TLLSOLODN) > *zeros
010400070907     c                   eval      §TLLSOLODN = %replace(aacma:§TLLSOLODN:
010500070907     c                                             %scan(aapma:§TLLSOLODN))
010600080630     c                   enddo
010700070907     c* - E' => È
010800080630     c                   dow       %scan(eapma:§TLLSOIND) > *zeros
010900070907     c                   eval      §TLLSOIND  = %replace(eacma:§TLLSOIND:
011000070907     c                                             %scan(eapma:§TLLSOIND))
011100080630     c                   enddo
011200080630     c                   dow       %scan(eapma:§TLLSOLOD) > *zeros
011300070907     c                   eval      §TLLSOLOD  = %replace(eacma:§TLLSOLOD:
011400070907     c                                             %scan(eapma:§TLLSOLOD))
011500080630     c                   enddo
011600080630     c                   dow       %scan(eapma:§TLLSOLODN) > *zeros
011700070907     c                   eval      §TLLSOLODN = %replace(eacma:§TLLSOLODN:
011800070907     c                                             %scan(eapma:§TLLSOLODN))
011900080630     c                   enddo
012000070907     c* - I' => Ì
012100080630     c                   dow       %scan(iapma:§TLLSOIND) > *zeros
012200070907     c                   eval      §TLLSOIND  = %replace(iacma:§TLLSOIND:
012300070907     c                                             %scan(iapma:§TLLSOIND))
012400080630     c                   enddo
012500080630     c                   dow       %scan(iapma:§TLLSOLOD) > *zeros
012600070907     c                   eval      §TLLSOLOD  = %replace(iacma:§TLLSOLOD:
012700070907     c                                             %scan(iapma:§TLLSOLOD))
012800080630     c                   enddo
012900080630     c                   dow       %scan(iapma:§TLLSOLODN) > *zeros
013000070907     c                   eval      §TLLSOLODN = %replace(iacma:§TLLSOLODN:
013100070907     c                                             %scan(iapma:§TLLSOLODN))
013200080630     c                   enddo
013300070907     c* - O' => Ò
013400080630     c                   dow       %scan(oapma:§TLLSOIND) > *zeros
013500070907     c                   eval      §TLLSOIND  = %replace(oacma:§TLLSOIND:
013600070907     c                                             %scan(oapma:§TLLSOIND))
013700080630     c                   enddo
013800080630     c                   dow       %scan(oapma:§TLLSOLOD) > *zeros
013900070907     c                   eval      §TLLSOLOD  = %replace(oacma:§TLLSOLOD:
014000070907     c                                             %scan(oapma:§TLLSOLOD))
014100080630     c                   enddo
014200080630     c                   dow       %scan(oapma:§TLLSOLODN) > *zeros
014300070907     c                   eval      §TLLSOLODN = %replace(oacma:§TLLSOLODN:
014400070907     c                                             %scan(oapma:§TLLSOLODN))
014500080630     c                   enddo
014600070907     c* - U' => Ù
014700080630     c                   dow       %scan(uapma:§TLLSOIND) > *zeros
014800070907     c                   eval      §TLLSOIND  = %replace(uacma:§TLLSOIND:
014900070907     c                                             %scan(uapma:§TLLSOIND))
015000080630     c                   enddo
015100080630     c                   dow       %scan(uapma:§TLLSOLOD) > *zeros
015200070907     c                   eval      §TLLSOLOD  = %replace(uacma:§TLLSOLOD:
015300070907     c                                             %scan(uapma:§TLLSOLOD))
015400080630     c                   enddo
015500080630     c                   dow       %scan(uapma:§TLLSOLODN) > *zeros
015600070907     c                   eval      §TLLSOLODN = %replace(uacma:§TLLSOLODN:
015700070907     c                                             %scan(uapma:§TLLSOLODN))
015800080630     c                   enddo
015900070907     c* - a' => à
016000080630     c                   dow       %scan(aapmi:§TLLSOIND) > *zeros
016100070907     c                   eval      §TLLSOIND  = %replace(aacmi:§TLLSOIND:
016200070907     c                                             %scan(aapmi:§TLLSOIND))
016300080630     c                   enddo
016400080630     c                   dow       %scan(aapmi:§TLLSOLOD) > *zeros
016500070907     c                   eval      §TLLSOLOD  = %replace(aacmi:§TLLSOLOD:
016600070907     c                                             %scan(aapmi:§TLLSOLOD))
016700080630     c                   enddo
016800080630     c                   dow       %scan(aapmi:§TLLSOLODN) > *zeros
016900070907     c                   eval      §TLLSOLODN = %replace(aacmi:§TLLSOLODN:
017000070907     c                                             %scan(aapmi:§TLLSOLODN))
017100080630     c                   enddo
017200070907     c* - e' => è
017300080630     c                   dow       %scan(eapmi:§TLLSOIND) > *zeros
017400070907     c                   eval      §TLLSOIND  = %replace(eacmi:§TLLSOIND:
017500070907     c                                             %scan(eapmi:§TLLSOIND))
017600080630     c                   enddo
017700080630     c                   dow       %scan(eapmi:§TLLSOLOD) > *zeros
017800070907     c                   eval      §TLLSOLOD  = %replace(eacmi:§TLLSOLOD:
017900070907     c                                             %scan(eapmi:§TLLSOLOD))
018000080630     c                   enddo
018100080630     c                   dow       %scan(eapmi:§TLLSOLODN) > *zeros
018200070907     c                   eval      §TLLSOLODN = %replace(eacmi:§TLLSOLODN:
018300070907     c                                             %scan(eapmi:§TLLSOLODN))
018400080630     c                   enddo
018500070907     c* - i' => ì
018600080630     c                   dow       %scan(iapmi:§TLLSOIND) > *zeros
018700070907     c                   eval      §TLLSOIND  = %replace(iacmi:§TLLSOIND:
018800070907     c                                             %scan(iapmi:§TLLSOIND))
018900080630     c                   enddo
019000080630     c                   dow       %scan(iapmi:§TLLSOLOD) > *zeros
019100070907     c                   eval      §TLLSOLOD  = %replace(iacmi:§TLLSOLOD:
019200070907     c                                             %scan(iapmi:§TLLSOLOD))
019300080630     c                   enddo
019400080630     c                   dow       %scan(iapmi:§TLLSOLODN) > *zeros
019500070907     c                   eval      §TLLSOLODN = %replace(iacmi:§TLLSOLODN:
019600070907     c                                             %scan(iapmi:§TLLSOLODN))
019700080630     c                   enddo
019800070907     c* - o' => ò
019900080630     c                   dow       %scan(oapmi:§TLLSOIND) > *zeros
020000070907     c                   eval      §TLLSOIND  = %replace(oacmi:§TLLSOIND:
020100070907     c                                             %scan(oapmi:§TLLSOIND))
020200080630     c                   enddo
020300080630     c                   dow       %scan(oapmi:§TLLSOLOD) > *zeros
020400070907     c                   eval      §TLLSOLOD  = %replace(oacmi:§TLLSOLOD:
020500070907     c                                             %scan(oapmi:§TLLSOLOD))
020600080630     c                   enddo
020700080630     c                   dow       %scan(oapmi:§TLLSOLODN) > *zeros
020800070907     c                   eval      §TLLSOLODN = %replace(oacmi:§TLLSOLODN:
020900070907     c                                             %scan(oapmi:§TLLSOLODN))
021000080630     c                   enddo
021100070907     c* - u' => ù
021200080630     c                   dow       %scan(uapmi:§TLLSOIND) > *zeros
021300070907     c                   eval      §TLLSOIND  = %replace(uacmi:§TLLSOIND:
021400070907     c                                             %scan(uapmi:§TLLSOIND))
021500080630     c                   enddo
021600080630     c                   dow       %scan(uapmi:§TLLSOLOD) > *zeros
021700070907     c                   eval      §TLLSOLOD  = %replace(uacmi:§TLLSOLOD:
021800070907     c                                             %scan(uapmi:§TLLSOLOD))
021900080630     c                   enddo
022000080630     c                   dow       %scan(uapmi:§TLLSOLODN) > *zeros
022100070907     c                   eval      §TLLSOLODN = %replace(uacmi:§TLLSOLODN:
022200070907     c                                             %scan(uapmi:§TLLSOLODN))
022300080630     c                   enddo
022400070913     c*
022500070913     c* Forzatrura speciale x strade statali
022600080630     c                   dow       %scan('s.s.':§TLLSOIND) > *zeros
022700070913     c                   eval      §TLLSOIND = %replace('ss  ':§TLLSOIND:
022800070913     c                                            %scan('s.s.':§TLLSOIND))
022900080630     c                   enddo
023000080630     c                   dow       %scan('S.S.':§TLLSOIND) > *zeros
023100070913     c                   eval      §TLLSOIND = %replace('SS  ':§TLLSOIND:
023200070913     c                                            %scan('S.S.':§TLLSOIND))
023300080630     c                   enddo
023400101129     C*
023500101129     C* Effettuo considerazioni relative alle modifiche CAP PT 10/2010
023600101129     C*
023700101129     C* ...solo le provincia interessate: MB / FM / BT / RN / TN / PR / AP
023800121120 xxx C***                if        §TLLSOPRD = 'MB' or
023900121228     C***                          §TLLSOPRD = 'FM' or
024000121228     C***                          §TLLSOPRD = 'BT' or
024100121228     C***                          §TLLSOPRD = 'RN' or
024200121228     C***                          §TLLSOPRD = 'TN' or
024300121228     C***                          §TLLSOPRD = 'PR' or
024400121228     C***                          §TLLSOPRD = 'AP'
024500101129     C*
024600121228     C***                eval      NEWL_PRV = §TLLSOPRD
024700121228     C***                eval      NEWL_CAP = §TLLSOCAD
024800121228     C***                if        §TLLSOLODN <> *blanks
024900121228     C***                eval      NEWL_LOC = §TLLSOLODN
025000121228     C***                else
025100121228     C***                eval      NEWL_LOC = §TLLSOLOD
025200121228     C***                endif
025300101129     C*
025400101129     C* 1° tentativo => PRV + CAP + LOC
025500121228     C***  KEYcpmod2_PCL chain     wacpmod2l
025600121228     C***                if        %found(wacpmod2l)
025700121228     C***                eval      §TLLSOPRD = OLDL_PRV
025800121228     C***                eval      §TLLSOCAD = OLDL_CAP
025900121228     C***                else
026000101129     C* 2° tentativo => PRV + CAP
026100121228     C***  KEYcpmod2_PC  chain     wacpmod2l
026200121228     C***                if        %found(wacpmod2l)
026300121228     C***                eval      §TLLSOPRD = OLDL_PRV
026400121228     C***                eval      §TLLSOCAD = OLDL_CAP
026500121228     C***                else
026600101201     C* 3° tentativo => PRV (solo x nuove provincie)
026700121120 xxx C***                if        §TLLSOPRD = 'MB' or
026800121228     C***                          §TLLSOPRD = 'FM' or
026900121228     C***                          §TLLSOPRD = 'BT'
027000121228     C***  KEYcpmod2_P   chain     wacpmod2l
027100121228     C***                if        %found(wacpmod2l)
027200121228     C***                eval      §TLLSOPRD = OLDL_PRV
027300121228     C***                endif
027400121228     C***                endif
027500121228     C***                endif
027600121228     C***                endif
027700121228     C***                endif
027800050121     C*
027900050121     C* Inizializzazione campi e variabili d out
028000060804     C                   clear                   tivaw000
028100060804     C*
028200070913     C                   eval      wRecOut = %subst(dtllso:1:157)
028300060804     C*
028400060804     C* Effettuo determinate considerazioni x portare nella forma ASCII editata con segno
028500060804     C* le coordinale geo-stazionarie
028600060804     C                   if        §tllsoXLO < *zeros
028700060804     C                   eval      wNum11_8 = %abs(§tllsoXLO)
028800060804     C                   eval      wSegno = '-'
028900060804     C                   else
029000060804     C                   eval      wNum11_8 = §tllsoXLO
029100060804     C                   eval      wSegno = *blanks
029200060804     C                   endif
029300060804     C                   move(p)   wNum11_8      wAlfa11
029400060804     C                   eval      wRecOut = wRecOut + wSegno +
029500060804     C                                       %subst(wAlfa11:1:3) + wSepDec +
029600060804     C                                       %subst(wAlfa11:4:8)
029700060804     C*
029800060804     C                   if        §tllsoYLA < *zeros
029900060804     C                   eval      wNum11_8 = %abs(§tllsoYLA)
030000060804     C                   eval      wSegno = '-'
030100060804     C                   else
030200060804     C                   eval      wNum11_8 = §tllsoYLA
030300060804     C                   eval      wSegno = *blanks
030400060804     C                   endif
030500060804     C                   move(p)   wNum11_8      wAlfa11
030600060804     C                   eval      wRecOut = wRecOut + wSegno +
030700060804     C                                       %subst(wAlfa11:1:3) + wSepDec +
030800060804     C                                       %subst(wAlfa11:4:8)
030900070112     C*
031000070112     C* Quindi proseguo con il resto dei dati
031100070112     C                   eval      wRecOut = %subst(wRecOut:1:183) +
031200070913     C                             %subst(vgdDTA:180:%len(dtllso)-180+1)
031300060804     C*
031400060804     C* Valorizzo il buffer del file d output
031500060804     C                   eval      vawdta = wRecOut
031600930409     C*
031700050121     C                   WRITE     TIVAW000
031800050909     C                   DELETE    TIVGD000
031900991027     C*
032000050909     C                   READ      TIVGDTMP
032100991027     C                   ENDdo
032200991027     C*
032300991027     C                   eval      wrkesito = '0'
032400991027     C*
032500910830     C                   ENDSR
032600060804
032700991027
032800991027      /TITLE Operazioni iniziali.
032900991027     C     *inzsr        BEGSR
033000991027     C*
033100991027     C     *ENTRY        PLIST
033200991027     C                   parm                    prmppt
033300991027     C     wrkesito      parm      wrkesito      prmesito
033400991027     C*
033500101129     C* Definizione chiave su WACPMOD2L  - Completa: PRV / CAP / LOC
033600101129     C     KEYcpmod2_PCL KLIST
033700101129     C                   KFLD                    NEWL_PRV
033800101129     C                   KFLD                    NEWL_CAP
033900101129     C                   KFLD                    NEWL_LOC
034000101129     C*
034100101129     C* Definizione chiave su WACPMOD2L  - Parziale: PRV / CAP
034200101129     C     KEYcpmod2_PC  KLIST
034300101129     C                   KFLD                    NEWL_PRV
034400101129     C                   KFLD                    NEWL_CAP
034500101129     C*
034600101129     C* Definizione chiave su WACPMOD2L  - Parziale: PRV
034700101129     C     KEYcpmod2_P   KLIST
034800101129     C                   KFLD                    NEWL_PRV
034900101129     C*
035000991027     C                   ENDSR
