000100890921     H DECEDIT('0,') DATEDIT(*DMY.)
000200991027     H dftactgrp(*yes)
000300991027
000400050909     FTIVGDTMP  UF   E             DISK
000500050121     Ftivawwwt  O    E             DISK
000600991027
000700060804     D dtllso        e ds
000800160504     D dtlloo        e ds
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
005000160504     C*
005100160504     C                   clear                   dtllso
005200160504     C                   clear                   dtlloo
005300160504     C*
005400160504     C* Verifico se elaborazione per bolle o per ORM
005500160504     C* => se ORM "overrido" i dati originali
005600160504     C                   if        %subst(prmppt:1:2) = 'OO'
005700160504     C                   eval      dtlloo = vgdDTA
005800160504     C                   eval      %subst(dtllso:27) = %subst(dtlloo:27)
005900160504     C                   else
006000160504     C                   eval      dtllso = vgdDTA
006100160504     C                   endif
006200080122     c*
006300080122     c* Forzatrura speciale x "CENTERGROSS"
006400080122     c* (in quanto nn esiste in cartografia ma nel ns cappario invece sì)
006500080122     c                   if        §tllsoPRD  = 'BO'                 AND
006600080122     c                             %trim(§tllsoLOD)  = 'CENTERGROSS' AND
006700080122     c                             %trim(§tllsoLODN) = *blanks
006800080122     C                   eval      §tllsoLODN  = 'FUNO'
006900080122     c                   endif
007000080122     c                   if        §tllsoPRD  = 'BO'                 AND
007100080122     c                             %trim(§tllsoLODN) = 'CENTERGROSS'
007200080122     C                   eval      §tllsoLODN  = 'FUNO'
007300080122     c                   endif
007400150112     c*
007500150112     c* Forzatrura speciale x "INTERPORTO BENTIVOGLIO"
007600150112     c* (in quanto nn esiste in cartografia ma nel ns cappario invece sì)
007700150112     c                   if        §tllsoPRD  = 'BO'                 AND
007800150112     c                             %trim(§tllsoLODN) = 'INTERPORTO BENTIVOGLIO'
007900150112     C                   if        %scan('INTERPORTO':§tllsoIND) = *zeros
008000150112     c                   eval      §tllsoLODN  = 'BENTIVOGLIO'
008100150112     C                   eval      §tllsoIND   = 'INTERPORTO '+%trim(§tllsoIND)
008200150112     c                   endif
008300150112     c                   endif
008400080122     c*
008500080122     c* Forzatrura speciale x "CASCINE VICA"
008600080122     c* (in quanto nn esiste in cartografia ma nel ns cappario invece sì)
008700080122     c                   if        §tllsoPRD  = 'TO'                  AND
008800080122     c                             %trim(§tllsoLOD)  = 'CASCINE VICA' AND
008900080122     c                             %trim(§tllsoLODN) = *blanks
009000080122     C                   eval      §tllsoLODN  = 'RIVOLI'
009100080122     c                   endif
009200080122     c                   if        §tllsoPRD  = 'TO'                  AND
009300080122     c                             %trim(§tllsoLODN) = 'CASCINE VICA'
009400080122     C                   eval      §tllsoLODN  = 'RIVOLI'
009500080122     c                   endif
009600150624     c*
009700150624     c* Forzatrura speciale x "BELLARIA"
009800150624     c* (in quanto nn esiste in cartografia ma nel ns cappario invece sì)
009900150624     c                   if        §tllsoPRD  = 'RN'                  AND
010000150624     c                             §tllsoCAD  = '47814'               AND
010100150624     c                             %trim(§tllsoLOD)  = 'BELLARIA'     AND
010200150624     c                             %trim(§tllsoLODN) = *blanks
010300150624     C                   eval      §tllsoLODN  = 'BELLARIA-IGEA MARINA'
010400150624     c                   endif
010500150624     c                   if        §tllsoPRD  = 'RN'                  AND
010600150624     c                             §tllsoCAD  = '47814'               AND
010700150624     c                             %trim(§tllsoLODN) = 'BELLARIA'
010800150624     C                   eval      §tllsoLODN  = 'BELLARIA-IGEA MARINA'
010900150624     c                   endif
011000080122     c*
011100080122     c* ...x ogni evenienza se provincia nn fosse valorizzata => fisso BO
011200080122     c                   select
011300080122     c                   when      §TLLSOPRD = *blanks
011400080122     c                   eval      §TLLSOPRD = 'BO'
011500080122     c                   endsl
011600080122     c*
011700080122     c* Forzatura x San Marino e Vaticano
011800080122     c                   select
011900080122     c                   when      §TLLSOPRD  = 'RM'    and
012000080122     c                             §TLLSOCAD  = '00120'
012100080122     c                   eval      §TLLSOPRD  = 'CV'
012200080122     c                   when      §TLLSOPRD  = 'RN'    and
012300080122     c                             §TLLSOCAD >= '47890' and
012400080122     c                             §TLLSOCAD <= '47899'
012500080122     c                   eval      §TLLSOPRD  = 'SM'
012600080122     c                   endsl
012700070907     c*
012800070907     c* Forzatrura speciale x lettere accentate
012900070907     c* - A' => À
013000080630     c                   dow       %scan(aapma:§TLLSOIND) > *zeros
013100070907     c                   eval      §TLLSOIND  = %replace(aacma:§TLLSOIND:
013200070907     c                                             %scan(aapma:§TLLSOIND))
013300080630     c                   enddo
013400080630     c                   dow       %scan(aapma:§TLLSOLOD) > *zeros
013500070907     c                   eval      §TLLSOLOD  = %replace(aacma:§TLLSOLOD:
013600070907     c                                             %scan(aapma:§TLLSOLOD))
013700080630     c                   enddo
013800080630     c                   dow       %scan(aapma:§TLLSOLODN) > *zeros
013900070907     c                   eval      §TLLSOLODN = %replace(aacma:§TLLSOLODN:
014000070907     c                                             %scan(aapma:§TLLSOLODN))
014100080630     c                   enddo
014200070907     c* - E' => È
014300080630     c                   dow       %scan(eapma:§TLLSOIND) > *zeros
014400070907     c                   eval      §TLLSOIND  = %replace(eacma:§TLLSOIND:
014500070907     c                                             %scan(eapma:§TLLSOIND))
014600080630     c                   enddo
014700080630     c                   dow       %scan(eapma:§TLLSOLOD) > *zeros
014800070907     c                   eval      §TLLSOLOD  = %replace(eacma:§TLLSOLOD:
014900070907     c                                             %scan(eapma:§TLLSOLOD))
015000080630     c                   enddo
015100080630     c                   dow       %scan(eapma:§TLLSOLODN) > *zeros
015200070907     c                   eval      §TLLSOLODN = %replace(eacma:§TLLSOLODN:
015300070907     c                                             %scan(eapma:§TLLSOLODN))
015400080630     c                   enddo
015500070907     c* - I' => Ì
015600080630     c                   dow       %scan(iapma:§TLLSOIND) > *zeros
015700070907     c                   eval      §TLLSOIND  = %replace(iacma:§TLLSOIND:
015800070907     c                                             %scan(iapma:§TLLSOIND))
015900080630     c                   enddo
016000080630     c                   dow       %scan(iapma:§TLLSOLOD) > *zeros
016100070907     c                   eval      §TLLSOLOD  = %replace(iacma:§TLLSOLOD:
016200070907     c                                             %scan(iapma:§TLLSOLOD))
016300080630     c                   enddo
016400080630     c                   dow       %scan(iapma:§TLLSOLODN) > *zeros
016500070907     c                   eval      §TLLSOLODN = %replace(iacma:§TLLSOLODN:
016600070907     c                                             %scan(iapma:§TLLSOLODN))
016700080630     c                   enddo
016800070907     c* - O' => Ò
016900080630     c                   dow       %scan(oapma:§TLLSOIND) > *zeros
017000070907     c                   eval      §TLLSOIND  = %replace(oacma:§TLLSOIND:
017100070907     c                                             %scan(oapma:§TLLSOIND))
017200080630     c                   enddo
017300080630     c                   dow       %scan(oapma:§TLLSOLOD) > *zeros
017400070907     c                   eval      §TLLSOLOD  = %replace(oacma:§TLLSOLOD:
017500070907     c                                             %scan(oapma:§TLLSOLOD))
017600080630     c                   enddo
017700080630     c                   dow       %scan(oapma:§TLLSOLODN) > *zeros
017800070907     c                   eval      §TLLSOLODN = %replace(oacma:§TLLSOLODN:
017900070907     c                                             %scan(oapma:§TLLSOLODN))
018000080630     c                   enddo
018100070907     c* - U' => Ù
018200080630     c                   dow       %scan(uapma:§TLLSOIND) > *zeros
018300070907     c                   eval      §TLLSOIND  = %replace(uacma:§TLLSOIND:
018400070907     c                                             %scan(uapma:§TLLSOIND))
018500080630     c                   enddo
018600080630     c                   dow       %scan(uapma:§TLLSOLOD) > *zeros
018700070907     c                   eval      §TLLSOLOD  = %replace(uacma:§TLLSOLOD:
018800070907     c                                             %scan(uapma:§TLLSOLOD))
018900080630     c                   enddo
019000080630     c                   dow       %scan(uapma:§TLLSOLODN) > *zeros
019100070907     c                   eval      §TLLSOLODN = %replace(uacma:§TLLSOLODN:
019200070907     c                                             %scan(uapma:§TLLSOLODN))
019300080630     c                   enddo
019400070907     c* - a' => à
019500080630     c                   dow       %scan(aapmi:§TLLSOIND) > *zeros
019600070907     c                   eval      §TLLSOIND  = %replace(aacmi:§TLLSOIND:
019700070907     c                                             %scan(aapmi:§TLLSOIND))
019800080630     c                   enddo
019900080630     c                   dow       %scan(aapmi:§TLLSOLOD) > *zeros
020000070907     c                   eval      §TLLSOLOD  = %replace(aacmi:§TLLSOLOD:
020100070907     c                                             %scan(aapmi:§TLLSOLOD))
020200080630     c                   enddo
020300080630     c                   dow       %scan(aapmi:§TLLSOLODN) > *zeros
020400070907     c                   eval      §TLLSOLODN = %replace(aacmi:§TLLSOLODN:
020500070907     c                                             %scan(aapmi:§TLLSOLODN))
020600080630     c                   enddo
020700070907     c* - e' => è
020800080630     c                   dow       %scan(eapmi:§TLLSOIND) > *zeros
020900070907     c                   eval      §TLLSOIND  = %replace(eacmi:§TLLSOIND:
021000070907     c                                             %scan(eapmi:§TLLSOIND))
021100080630     c                   enddo
021200080630     c                   dow       %scan(eapmi:§TLLSOLOD) > *zeros
021300070907     c                   eval      §TLLSOLOD  = %replace(eacmi:§TLLSOLOD:
021400070907     c                                             %scan(eapmi:§TLLSOLOD))
021500080630     c                   enddo
021600080630     c                   dow       %scan(eapmi:§TLLSOLODN) > *zeros
021700070907     c                   eval      §TLLSOLODN = %replace(eacmi:§TLLSOLODN:
021800070907     c                                             %scan(eapmi:§TLLSOLODN))
021900080630     c                   enddo
022000070907     c* - i' => ì
022100080630     c                   dow       %scan(iapmi:§TLLSOIND) > *zeros
022200070907     c                   eval      §TLLSOIND  = %replace(iacmi:§TLLSOIND:
022300070907     c                                             %scan(iapmi:§TLLSOIND))
022400080630     c                   enddo
022500080630     c                   dow       %scan(iapmi:§TLLSOLOD) > *zeros
022600070907     c                   eval      §TLLSOLOD  = %replace(iacmi:§TLLSOLOD:
022700070907     c                                             %scan(iapmi:§TLLSOLOD))
022800080630     c                   enddo
022900080630     c                   dow       %scan(iapmi:§TLLSOLODN) > *zeros
023000070907     c                   eval      §TLLSOLODN = %replace(iacmi:§TLLSOLODN:
023100070907     c                                             %scan(iapmi:§TLLSOLODN))
023200080630     c                   enddo
023300070907     c* - o' => ò
023400080630     c                   dow       %scan(oapmi:§TLLSOIND) > *zeros
023500070907     c                   eval      §TLLSOIND  = %replace(oacmi:§TLLSOIND:
023600070907     c                                             %scan(oapmi:§TLLSOIND))
023700080630     c                   enddo
023800080630     c                   dow       %scan(oapmi:§TLLSOLOD) > *zeros
023900070907     c                   eval      §TLLSOLOD  = %replace(oacmi:§TLLSOLOD:
024000070907     c                                             %scan(oapmi:§TLLSOLOD))
024100080630     c                   enddo
024200080630     c                   dow       %scan(oapmi:§TLLSOLODN) > *zeros
024300070907     c                   eval      §TLLSOLODN = %replace(oacmi:§TLLSOLODN:
024400070907     c                                             %scan(oapmi:§TLLSOLODN))
024500080630     c                   enddo
024600070907     c* - u' => ù
024700080630     c                   dow       %scan(uapmi:§TLLSOIND) > *zeros
024800070907     c                   eval      §TLLSOIND  = %replace(uacmi:§TLLSOIND:
024900070907     c                                             %scan(uapmi:§TLLSOIND))
025000080630     c                   enddo
025100080630     c                   dow       %scan(uapmi:§TLLSOLOD) > *zeros
025200070907     c                   eval      §TLLSOLOD  = %replace(uacmi:§TLLSOLOD:
025300070907     c                                             %scan(uapmi:§TLLSOLOD))
025400080630     c                   enddo
025500080630     c                   dow       %scan(uapmi:§TLLSOLODN) > *zeros
025600070907     c                   eval      §TLLSOLODN = %replace(uacmi:§TLLSOLODN:
025700070907     c                                             %scan(uapmi:§TLLSOLODN))
025800080630     c                   enddo
025900070913     c*
026000070913     c* Forzatrura speciale x strade statali
026100080630     c                   dow       %scan('s.s.':§TLLSOIND) > *zeros
026200070913     c                   eval      §TLLSOIND = %replace('ss  ':§TLLSOIND:
026300070913     c                                            %scan('s.s.':§TLLSOIND))
026400080630     c                   enddo
026500080630     c                   dow       %scan('S.S.':§TLLSOIND) > *zeros
026600070913     c                   eval      §TLLSOIND = %replace('SS  ':§TLLSOIND:
026700070913     c                                            %scan('S.S.':§TLLSOIND))
026800080630     c                   enddo
026900050121     C*
027000050121     C* Inizializzazione campi e variabili d out
027100060804     C                   clear                   tivaw000
027200060804     C*
027300070913     C                   eval      wRecOut = %subst(dtllso:1:157)
027400060804     C*
027500060804     C* Effettuo determinate considerazioni x portare nella forma ASCII editata con segno
027600060804     C* le coordinale geo-stazionarie
027700060804     C                   if        §tllsoXLO < *zeros
027800060804     C                   eval      wNum11_8 = %abs(§tllsoXLO)
027900060804     C                   eval      wSegno = '-'
028000060804     C                   else
028100060804     C                   eval      wNum11_8 = §tllsoXLO
028200060804     C                   eval      wSegno = *blanks
028300060804     C                   endif
028400060804     C                   move(p)   wNum11_8      wAlfa11
028500060804     C                   eval      wRecOut = wRecOut + wSegno +
028600060804     C                                       %subst(wAlfa11:1:3) + wSepDec +
028700060804     C                                       %subst(wAlfa11:4:8)
028800060804     C*
028900060804     C                   if        §tllsoYLA < *zeros
029000060804     C                   eval      wNum11_8 = %abs(§tllsoYLA)
029100060804     C                   eval      wSegno = '-'
029200060804     C                   else
029300060804     C                   eval      wNum11_8 = §tllsoYLA
029400060804     C                   eval      wSegno = *blanks
029500060804     C                   endif
029600060804     C                   move(p)   wNum11_8      wAlfa11
029700060804     C                   eval      wRecOut = wRecOut + wSegno +
029800060804     C                                       %subst(wAlfa11:1:3) + wSepDec +
029900060804     C                                       %subst(wAlfa11:4:8)
030000070112     C*
030100070112     C* Quindi proseguo con il resto dei dati
030200070112     C                   eval      wRecOut = %subst(wRecOut:1:183) +
030300070913     C                             %subst(vgdDTA:180:%len(dtllso)-180+1)
030400160504     C*
030500160504     C* Verifico se elaborazione per bolle o per ORM
030600160504     C* => se ORM "overrido" indietro i dati originali
030700160504     C                   if        %subst(prmppt:1:2) = 'OO'
030800160504     C                   eval      %subst(wRecOut:1:26) = %subst(dtlloo:1:26)
030900160504     C                   endif
031000060804     C*
031100060804     C* Valorizzo il buffer del file d output
031200060804     C                   eval      vawdta = wRecOut
031300930409     C*
031400050121     C                   WRITE     TIVAW000
031500050909     C                   DELETE    TIVGD000
031600991027     C*
031700050909     C                   READ      TIVGDTMP
031800991027     C                   ENDdo
031900991027     C*
032000991027     C                   eval      wrkesito = '0'
032100991027     C*
032200910830     C                   ENDSR
032300060804
032400991027
032500991027      /TITLE Operazioni iniziali.
032600991027     C     *inzsr        BEGSR
032700991027     C*
032800991027     C     *ENTRY        PLIST
032900991027     C                   parm                    prmppt
033000991027     C     wrkesito      parm      wrkesito      prmesito
033100101129     C*
033200991027     C                   ENDSR
