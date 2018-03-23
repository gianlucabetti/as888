000100890921     H DECEDIT('0,') DATEDIT(*DMY.)
000200991027     H dftactgrp(*yes)
000300991027
000400050909     FTIVGDTMP  UF   E             DISK
000500050121     Ftivawwwt  O    E             DISK
000600991027
000700060804     D dtllso        e ds
000800160504     D dtlloo        e ds
000900160616     D dtllxo        e ds
001000991027     D prmppt          s             50
001100991027     D prmesito        s              1
001200991027     D wrkesito        s                   like(prmesito)
001300060804     D wNum11_8        s             11  8 inz
001400060804     D wAlfa11         s             11    inz
001500060804     D wSegno          s              1    inz
001600060804     D wSepDec         s              1    inz(',')
001700070112     D wRecOut         s           1024    varying
001800070907
001900070907     D*
002000070907     D aapma           s              2    inz('A''')
002100070907     D aacma           s              2    inz('À ')
002200070907     D eapma           s              2    inz('E''')
002300070907     D eacma           s              2    inz('È ')
002400070907     D iapma           s              2    inz('I''')
002500070907     D iacma           s              2    inz('Ì ')
002600070907     D oapma           s              2    inz('O''')
002700070907     D oacma           s              2    inz('Ò ')
002800070907     D uapma           s              2    inz('U''')
002900070907     D uacma           s              2    inz('Ù ')
003000070907     D aapmi           s              2    inz('a''')
003100070907     D aacmi           s              2    inz('à ')
003200070907     D eapmi           s              2    inz('e''')
003300070907     D eacmi           s              2    inz('è ')
003400070907     D iapmi           s              2    inz('i''')
003500070907     D iacmi           s              2    inz('ì ')
003600070907     D oapmi           s              2    inz('o''')
003700070907     D oacmi           s              2    inz('ò ')
003800070907     D uapmi           s              2    inz('u''')
003900070907     D uacmi           s              2    inz('ù ')
004000070907     D*
004100070907
004200070907
004300060804     C                   exsr      traduci
004400060804     C                   seton                                        LR
004500991027
004600060804
004700991027     C     traduci       BEGSR
004800991027     C*
004900060804     C                   read      TIVGDTMP
005000991027     C                   dow       not %eof
005100160504     C*
005200160504     C                   clear                   dtllso
005300160504     C                   clear                   dtlloo
005400160616     C                   clear                   dtllxo
005500160504     C*
005600160616     C* Verifico se elaborazione per bolle o per ORM o "varie"
005700160616     C* => se no bolle "overrido" i dati originali
005800160616     C                   select
005900160616     C                   when      %subst(prmppt:1:2) = 'OO'
006000160504     C                   eval      dtlloo = vgdDTA
006100160504     C                   eval      %subst(dtllso:27) = %subst(dtlloo:27)
006200160616     C*
006300160616     C                   when      %subst(prmppt:1:2) = 'XO'
006400160616     C                   eval      dtllxo = vgdDTA
006500160616     C                   eval      %subst(dtllso:27) = %subst(dtllxo:27)
006600160616     C*
006700160616     C                   other
006800160504     C                   eval      dtllso = vgdDTA
006900160616     C                   endsl
007000080122     c*
007100080122     c* Forzatrura speciale x "CENTERGROSS"
007200080122     c* (in quanto nn esiste in cartografia ma nel ns cappario invece sì)
007300080122     c                   if        §tllsoPRD  = 'BO'                 AND
007400080122     c                             %trim(§tllsoLOD)  = 'CENTERGROSS' AND
007500080122     c                             %trim(§tllsoLODN) = *blanks
007600080122     C                   eval      §tllsoLODN  = 'FUNO'
007700080122     c                   endif
007800080122     c                   if        §tllsoPRD  = 'BO'                 AND
007900080122     c                             %trim(§tllsoLODN) = 'CENTERGROSS'
008000080122     C                   eval      §tllsoLODN  = 'FUNO'
008100080122     c                   endif
008200150112     c*
008300150112     c* Forzatrura speciale x "INTERPORTO BENTIVOGLIO"
008400150112     c* (in quanto nn esiste in cartografia ma nel ns cappario invece sì)
008500150112     c                   if        §tllsoPRD  = 'BO'                 AND
008600150112     c                             %trim(§tllsoLODN) = 'INTERPORTO BENTIVOGLIO'
008700150112     C                   if        %scan('INTERPORTO':§tllsoIND) = *zeros
008800150112     c                   eval      §tllsoLODN  = 'BENTIVOGLIO'
008900150112     C                   eval      §tllsoIND   = 'INTERPORTO '+%trim(§tllsoIND)
009000150112     c                   endif
009100150112     c                   endif
009200080122     c*
009300080122     c* Forzatrura speciale x "CASCINE VICA"
009400080122     c* (in quanto nn esiste in cartografia ma nel ns cappario invece sì)
009500080122     c                   if        §tllsoPRD  = 'TO'                  AND
009600080122     c                             %trim(§tllsoLOD)  = 'CASCINE VICA' AND
009700080122     c                             %trim(§tllsoLODN) = *blanks
009800080122     C                   eval      §tllsoLODN  = 'RIVOLI'
009900080122     c                   endif
010000080122     c                   if        §tllsoPRD  = 'TO'                  AND
010100080122     c                             %trim(§tllsoLODN) = 'CASCINE VICA'
010200080122     C                   eval      §tllsoLODN  = 'RIVOLI'
010300080122     c                   endif
010400150624     c*
010500150624     c* Forzatrura speciale x "BELLARIA"
010600150624     c* (in quanto nn esiste in cartografia ma nel ns cappario invece sì)
010700150624     c                   if        §tllsoPRD  = 'RN'                  AND
010800150624     c                             §tllsoCAD  = '47814'               AND
010900150624     c                             %trim(§tllsoLOD)  = 'BELLARIA'     AND
011000150624     c                             %trim(§tllsoLODN) = *blanks
011100150624     C                   eval      §tllsoLODN  = 'BELLARIA-IGEA MARINA'
011200150624     c                   endif
011300150624     c                   if        §tllsoPRD  = 'RN'                  AND
011400150624     c                             §tllsoCAD  = '47814'               AND
011500150624     c                             %trim(§tllsoLODN) = 'BELLARIA'
011600150624     C                   eval      §tllsoLODN  = 'BELLARIA-IGEA MARINA'
011700150624     c                   endif
011800080122     c*
011900080122     c* ...x ogni evenienza se provincia nn fosse valorizzata => fisso BO
012000080122     c                   select
012100080122     c                   when      §TLLSOPRD = *blanks
012200080122     c                   eval      §TLLSOPRD = 'BO'
012300080122     c                   endsl
012400080122     c*
012500080122     c* Forzatura x San Marino e Vaticano
012600080122     c                   select
012700080122     c                   when      §TLLSOPRD  = 'RM'    and
012800080122     c                             §TLLSOCAD  = '00120'
012900080122     c                   eval      §TLLSOPRD  = 'CV'
013000080122     c                   when      §TLLSOPRD  = 'RN'    and
013100080122     c                             §TLLSOCAD >= '47890' and
013200080122     c                             §TLLSOCAD <= '47899'
013300080122     c                   eval      §TLLSOPRD  = 'SM'
013400080122     c                   endsl
013500070907     c*
013600070907     c* Forzatrura speciale x lettere accentate
013700070907     c* - A' => À
013800080630     c                   dow       %scan(aapma:§TLLSOIND) > *zeros
013900070907     c                   eval      §TLLSOIND  = %replace(aacma:§TLLSOIND:
014000070907     c                                             %scan(aapma:§TLLSOIND))
014100080630     c                   enddo
014200080630     c                   dow       %scan(aapma:§TLLSOLOD) > *zeros
014300070907     c                   eval      §TLLSOLOD  = %replace(aacma:§TLLSOLOD:
014400070907     c                                             %scan(aapma:§TLLSOLOD))
014500080630     c                   enddo
014600080630     c                   dow       %scan(aapma:§TLLSOLODN) > *zeros
014700070907     c                   eval      §TLLSOLODN = %replace(aacma:§TLLSOLODN:
014800070907     c                                             %scan(aapma:§TLLSOLODN))
014900080630     c                   enddo
015000070907     c* - E' => È
015100080630     c                   dow       %scan(eapma:§TLLSOIND) > *zeros
015200070907     c                   eval      §TLLSOIND  = %replace(eacma:§TLLSOIND:
015300070907     c                                             %scan(eapma:§TLLSOIND))
015400080630     c                   enddo
015500080630     c                   dow       %scan(eapma:§TLLSOLOD) > *zeros
015600070907     c                   eval      §TLLSOLOD  = %replace(eacma:§TLLSOLOD:
015700070907     c                                             %scan(eapma:§TLLSOLOD))
015800080630     c                   enddo
015900080630     c                   dow       %scan(eapma:§TLLSOLODN) > *zeros
016000070907     c                   eval      §TLLSOLODN = %replace(eacma:§TLLSOLODN:
016100070907     c                                             %scan(eapma:§TLLSOLODN))
016200080630     c                   enddo
016300070907     c* - I' => Ì
016400080630     c                   dow       %scan(iapma:§TLLSOIND) > *zeros
016500070907     c                   eval      §TLLSOIND  = %replace(iacma:§TLLSOIND:
016600070907     c                                             %scan(iapma:§TLLSOIND))
016700080630     c                   enddo
016800080630     c                   dow       %scan(iapma:§TLLSOLOD) > *zeros
016900070907     c                   eval      §TLLSOLOD  = %replace(iacma:§TLLSOLOD:
017000070907     c                                             %scan(iapma:§TLLSOLOD))
017100080630     c                   enddo
017200080630     c                   dow       %scan(iapma:§TLLSOLODN) > *zeros
017300070907     c                   eval      §TLLSOLODN = %replace(iacma:§TLLSOLODN:
017400070907     c                                             %scan(iapma:§TLLSOLODN))
017500080630     c                   enddo
017600070907     c* - O' => Ò
017700080630     c                   dow       %scan(oapma:§TLLSOIND) > *zeros
017800070907     c                   eval      §TLLSOIND  = %replace(oacma:§TLLSOIND:
017900070907     c                                             %scan(oapma:§TLLSOIND))
018000080630     c                   enddo
018100080630     c                   dow       %scan(oapma:§TLLSOLOD) > *zeros
018200070907     c                   eval      §TLLSOLOD  = %replace(oacma:§TLLSOLOD:
018300070907     c                                             %scan(oapma:§TLLSOLOD))
018400080630     c                   enddo
018500080630     c                   dow       %scan(oapma:§TLLSOLODN) > *zeros
018600070907     c                   eval      §TLLSOLODN = %replace(oacma:§TLLSOLODN:
018700070907     c                                             %scan(oapma:§TLLSOLODN))
018800080630     c                   enddo
018900070907     c* - U' => Ù
019000080630     c                   dow       %scan(uapma:§TLLSOIND) > *zeros
019100070907     c                   eval      §TLLSOIND  = %replace(uacma:§TLLSOIND:
019200070907     c                                             %scan(uapma:§TLLSOIND))
019300080630     c                   enddo
019400080630     c                   dow       %scan(uapma:§TLLSOLOD) > *zeros
019500070907     c                   eval      §TLLSOLOD  = %replace(uacma:§TLLSOLOD:
019600070907     c                                             %scan(uapma:§TLLSOLOD))
019700080630     c                   enddo
019800080630     c                   dow       %scan(uapma:§TLLSOLODN) > *zeros
019900070907     c                   eval      §TLLSOLODN = %replace(uacma:§TLLSOLODN:
020000070907     c                                             %scan(uapma:§TLLSOLODN))
020100080630     c                   enddo
020200070907     c* - a' => à
020300080630     c                   dow       %scan(aapmi:§TLLSOIND) > *zeros
020400070907     c                   eval      §TLLSOIND  = %replace(aacmi:§TLLSOIND:
020500070907     c                                             %scan(aapmi:§TLLSOIND))
020600080630     c                   enddo
020700080630     c                   dow       %scan(aapmi:§TLLSOLOD) > *zeros
020800070907     c                   eval      §TLLSOLOD  = %replace(aacmi:§TLLSOLOD:
020900070907     c                                             %scan(aapmi:§TLLSOLOD))
021000080630     c                   enddo
021100080630     c                   dow       %scan(aapmi:§TLLSOLODN) > *zeros
021200070907     c                   eval      §TLLSOLODN = %replace(aacmi:§TLLSOLODN:
021300070907     c                                             %scan(aapmi:§TLLSOLODN))
021400080630     c                   enddo
021500070907     c* - e' => è
021600080630     c                   dow       %scan(eapmi:§TLLSOIND) > *zeros
021700070907     c                   eval      §TLLSOIND  = %replace(eacmi:§TLLSOIND:
021800070907     c                                             %scan(eapmi:§TLLSOIND))
021900080630     c                   enddo
022000080630     c                   dow       %scan(eapmi:§TLLSOLOD) > *zeros
022100070907     c                   eval      §TLLSOLOD  = %replace(eacmi:§TLLSOLOD:
022200070907     c                                             %scan(eapmi:§TLLSOLOD))
022300080630     c                   enddo
022400080630     c                   dow       %scan(eapmi:§TLLSOLODN) > *zeros
022500070907     c                   eval      §TLLSOLODN = %replace(eacmi:§TLLSOLODN:
022600070907     c                                             %scan(eapmi:§TLLSOLODN))
022700080630     c                   enddo
022800070907     c* - i' => ì
022900080630     c                   dow       %scan(iapmi:§TLLSOIND) > *zeros
023000070907     c                   eval      §TLLSOIND  = %replace(iacmi:§TLLSOIND:
023100070907     c                                             %scan(iapmi:§TLLSOIND))
023200080630     c                   enddo
023300080630     c                   dow       %scan(iapmi:§TLLSOLOD) > *zeros
023400070907     c                   eval      §TLLSOLOD  = %replace(iacmi:§TLLSOLOD:
023500070907     c                                             %scan(iapmi:§TLLSOLOD))
023600080630     c                   enddo
023700080630     c                   dow       %scan(iapmi:§TLLSOLODN) > *zeros
023800070907     c                   eval      §TLLSOLODN = %replace(iacmi:§TLLSOLODN:
023900070907     c                                             %scan(iapmi:§TLLSOLODN))
024000080630     c                   enddo
024100070907     c* - o' => ò
024200080630     c                   dow       %scan(oapmi:§TLLSOIND) > *zeros
024300070907     c                   eval      §TLLSOIND  = %replace(oacmi:§TLLSOIND:
024400070907     c                                             %scan(oapmi:§TLLSOIND))
024500080630     c                   enddo
024600080630     c                   dow       %scan(oapmi:§TLLSOLOD) > *zeros
024700070907     c                   eval      §TLLSOLOD  = %replace(oacmi:§TLLSOLOD:
024800070907     c                                             %scan(oapmi:§TLLSOLOD))
024900080630     c                   enddo
025000080630     c                   dow       %scan(oapmi:§TLLSOLODN) > *zeros
025100070907     c                   eval      §TLLSOLODN = %replace(oacmi:§TLLSOLODN:
025200070907     c                                             %scan(oapmi:§TLLSOLODN))
025300080630     c                   enddo
025400070907     c* - u' => ù
025500080630     c                   dow       %scan(uapmi:§TLLSOIND) > *zeros
025600070907     c                   eval      §TLLSOIND  = %replace(uacmi:§TLLSOIND:
025700070907     c                                             %scan(uapmi:§TLLSOIND))
025800080630     c                   enddo
025900080630     c                   dow       %scan(uapmi:§TLLSOLOD) > *zeros
026000070907     c                   eval      §TLLSOLOD  = %replace(uacmi:§TLLSOLOD:
026100070907     c                                             %scan(uapmi:§TLLSOLOD))
026200080630     c                   enddo
026300080630     c                   dow       %scan(uapmi:§TLLSOLODN) > *zeros
026400070907     c                   eval      §TLLSOLODN = %replace(uacmi:§TLLSOLODN:
026500070907     c                                             %scan(uapmi:§TLLSOLODN))
026600080630     c                   enddo
026700070913     c*
026800070913     c* Forzatrura speciale x strade statali
026900080630     c                   dow       %scan('s.s.':§TLLSOIND) > *zeros
027000070913     c                   eval      §TLLSOIND = %replace('ss  ':§TLLSOIND:
027100070913     c                                            %scan('s.s.':§TLLSOIND))
027200080630     c                   enddo
027300080630     c                   dow       %scan('S.S.':§TLLSOIND) > *zeros
027400070913     c                   eval      §TLLSOIND = %replace('SS  ':§TLLSOIND:
027500070913     c                                            %scan('S.S.':§TLLSOIND))
027600080630     c                   enddo
027700050121     C*
027800050121     C* Inizializzazione campi e variabili d out
027900060804     C                   clear                   tivaw000
028000060804     C*
028100070913     C                   eval      wRecOut = %subst(dtllso:1:157)
028200060804     C*
028300060804     C* Effettuo determinate considerazioni x portare nella forma ASCII editata con segno
028400060804     C* le coordinale geo-stazionarie
028500060804     C                   if        §tllsoXLO < *zeros
028600060804     C                   eval      wNum11_8 = %abs(§tllsoXLO)
028700060804     C                   eval      wSegno = '-'
028800060804     C                   else
028900060804     C                   eval      wNum11_8 = §tllsoXLO
029000060804     C                   eval      wSegno = *blanks
029100060804     C                   endif
029200060804     C                   move(p)   wNum11_8      wAlfa11
029300060804     C                   eval      wRecOut = wRecOut + wSegno +
029400060804     C                                       %subst(wAlfa11:1:3) + wSepDec +
029500060804     C                                       %subst(wAlfa11:4:8)
029600060804     C*
029700060804     C                   if        §tllsoYLA < *zeros
029800060804     C                   eval      wNum11_8 = %abs(§tllsoYLA)
029900060804     C                   eval      wSegno = '-'
030000060804     C                   else
030100060804     C                   eval      wNum11_8 = §tllsoYLA
030200060804     C                   eval      wSegno = *blanks
030300060804     C                   endif
030400060804     C                   move(p)   wNum11_8      wAlfa11
030500060804     C                   eval      wRecOut = wRecOut + wSegno +
030600060804     C                                       %subst(wAlfa11:1:3) + wSepDec +
030700060804     C                                       %subst(wAlfa11:4:8)
030800070112     C*
030900070112     C* Quindi proseguo con il resto dei dati
031000070112     C                   eval      wRecOut = %subst(wRecOut:1:183) +
031100070913     C                             %subst(vgdDTA:180:%len(dtllso)-180+1)
031200160504     C*
031300160616     C* Verifico se elaborazione per bolle o per ORM o "varie"
031400160616     C* => se no bolle "overrido" indietro i dati originali
031500160616     C                   select
031600160616     C                   when      %subst(prmppt:1:2) = 'OO'
031700160616     C                   eval      %subst(wRecOut:1:26) = %subst(dtlloo:1:26)
031800160616     C*
031900160616     C                   when      %subst(prmppt:1:2) = 'XO'
032000160616     C                   eval      %subst(wRecOut:1:26) = %subst(dtllxo:1:26)
032100160616     C                   endsl
032200060804     C*
032300060804     C* Valorizzo il buffer del file d output
032400060804     C                   eval      vawdta = wRecOut
032500930409     C*
032600050121     C                   WRITE     TIVAW000
032700050909     C                   DELETE    TIVGD000
032800991027     C*
032900050909     C                   READ      TIVGDTMP
033000991027     C                   ENDdo
033100991027     C*
033200991027     C                   eval      wrkesito = '0'
033300991027     C*
033400910830     C                   ENDSR
033500060804
033600991027
033700991027      /TITLE Operazioni iniziali.
033800991027     C     *inzsr        BEGSR
033900991027     C*
034000991027     C     *ENTRY        PLIST
034100991027     C                   parm                    prmppt
034200991027     C     wrkesito      parm      wrkesito      prmesito
034300101129     C*
034400991027     C                   ENDSR
