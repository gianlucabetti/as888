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
013500170412     c*
013600170412     c* Forzatura x Localià "alternativa" calcolata da noi
013700170412     c                   if        §TLLSOLODN <> *blanks
013800170412     c                   eval      §TLLSOLOD   = §TLLSOLODN
013900170412     c                   endif
014000070907     c*
014100070907     c* Forzatrura speciale x lettere accentate
014200070907     c* - A' => À
014300080630     c                   dow       %scan(aapma:§TLLSOIND) > *zeros
014400070907     c                   eval      §TLLSOIND  = %replace(aacma:§TLLSOIND:
014500070907     c                                             %scan(aapma:§TLLSOIND))
014600080630     c                   enddo
014700080630     c                   dow       %scan(aapma:§TLLSOLOD) > *zeros
014800070907     c                   eval      §TLLSOLOD  = %replace(aacma:§TLLSOLOD:
014900070907     c                                             %scan(aapma:§TLLSOLOD))
015000080630     c                   enddo
015100080630     c                   dow       %scan(aapma:§TLLSOLODN) > *zeros
015200070907     c                   eval      §TLLSOLODN = %replace(aacma:§TLLSOLODN:
015300070907     c                                             %scan(aapma:§TLLSOLODN))
015400080630     c                   enddo
015500070907     c* - E' => È
015600080630     c                   dow       %scan(eapma:§TLLSOIND) > *zeros
015700070907     c                   eval      §TLLSOIND  = %replace(eacma:§TLLSOIND:
015800070907     c                                             %scan(eapma:§TLLSOIND))
015900080630     c                   enddo
016000080630     c                   dow       %scan(eapma:§TLLSOLOD) > *zeros
016100070907     c                   eval      §TLLSOLOD  = %replace(eacma:§TLLSOLOD:
016200070907     c                                             %scan(eapma:§TLLSOLOD))
016300080630     c                   enddo
016400080630     c                   dow       %scan(eapma:§TLLSOLODN) > *zeros
016500070907     c                   eval      §TLLSOLODN = %replace(eacma:§TLLSOLODN:
016600070907     c                                             %scan(eapma:§TLLSOLODN))
016700080630     c                   enddo
016800070907     c* - I' => Ì
016900080630     c                   dow       %scan(iapma:§TLLSOIND) > *zeros
017000070907     c                   eval      §TLLSOIND  = %replace(iacma:§TLLSOIND:
017100070907     c                                             %scan(iapma:§TLLSOIND))
017200080630     c                   enddo
017300080630     c                   dow       %scan(iapma:§TLLSOLOD) > *zeros
017400070907     c                   eval      §TLLSOLOD  = %replace(iacma:§TLLSOLOD:
017500070907     c                                             %scan(iapma:§TLLSOLOD))
017600080630     c                   enddo
017700080630     c                   dow       %scan(iapma:§TLLSOLODN) > *zeros
017800070907     c                   eval      §TLLSOLODN = %replace(iacma:§TLLSOLODN:
017900070907     c                                             %scan(iapma:§TLLSOLODN))
018000080630     c                   enddo
018100070907     c* - O' => Ò
018200080630     c                   dow       %scan(oapma:§TLLSOIND) > *zeros
018300070907     c                   eval      §TLLSOIND  = %replace(oacma:§TLLSOIND:
018400070907     c                                             %scan(oapma:§TLLSOIND))
018500080630     c                   enddo
018600080630     c                   dow       %scan(oapma:§TLLSOLOD) > *zeros
018700070907     c                   eval      §TLLSOLOD  = %replace(oacma:§TLLSOLOD:
018800070907     c                                             %scan(oapma:§TLLSOLOD))
018900080630     c                   enddo
019000080630     c                   dow       %scan(oapma:§TLLSOLODN) > *zeros
019100070907     c                   eval      §TLLSOLODN = %replace(oacma:§TLLSOLODN:
019200070907     c                                             %scan(oapma:§TLLSOLODN))
019300080630     c                   enddo
019400070907     c* - U' => Ù
019500080630     c                   dow       %scan(uapma:§TLLSOIND) > *zeros
019600070907     c                   eval      §TLLSOIND  = %replace(uacma:§TLLSOIND:
019700070907     c                                             %scan(uapma:§TLLSOIND))
019800080630     c                   enddo
019900080630     c                   dow       %scan(uapma:§TLLSOLOD) > *zeros
020000070907     c                   eval      §TLLSOLOD  = %replace(uacma:§TLLSOLOD:
020100070907     c                                             %scan(uapma:§TLLSOLOD))
020200080630     c                   enddo
020300080630     c                   dow       %scan(uapma:§TLLSOLODN) > *zeros
020400070907     c                   eval      §TLLSOLODN = %replace(uacma:§TLLSOLODN:
020500070907     c                                             %scan(uapma:§TLLSOLODN))
020600080630     c                   enddo
020700070907     c* - a' => à
020800080630     c                   dow       %scan(aapmi:§TLLSOIND) > *zeros
020900070907     c                   eval      §TLLSOIND  = %replace(aacmi:§TLLSOIND:
021000070907     c                                             %scan(aapmi:§TLLSOIND))
021100080630     c                   enddo
021200080630     c                   dow       %scan(aapmi:§TLLSOLOD) > *zeros
021300070907     c                   eval      §TLLSOLOD  = %replace(aacmi:§TLLSOLOD:
021400070907     c                                             %scan(aapmi:§TLLSOLOD))
021500080630     c                   enddo
021600080630     c                   dow       %scan(aapmi:§TLLSOLODN) > *zeros
021700070907     c                   eval      §TLLSOLODN = %replace(aacmi:§TLLSOLODN:
021800070907     c                                             %scan(aapmi:§TLLSOLODN))
021900080630     c                   enddo
022000070907     c* - e' => è
022100080630     c                   dow       %scan(eapmi:§TLLSOIND) > *zeros
022200070907     c                   eval      §TLLSOIND  = %replace(eacmi:§TLLSOIND:
022300070907     c                                             %scan(eapmi:§TLLSOIND))
022400080630     c                   enddo
022500080630     c                   dow       %scan(eapmi:§TLLSOLOD) > *zeros
022600070907     c                   eval      §TLLSOLOD  = %replace(eacmi:§TLLSOLOD:
022700070907     c                                             %scan(eapmi:§TLLSOLOD))
022800080630     c                   enddo
022900080630     c                   dow       %scan(eapmi:§TLLSOLODN) > *zeros
023000070907     c                   eval      §TLLSOLODN = %replace(eacmi:§TLLSOLODN:
023100070907     c                                             %scan(eapmi:§TLLSOLODN))
023200080630     c                   enddo
023300070907     c* - i' => ì
023400080630     c                   dow       %scan(iapmi:§TLLSOIND) > *zeros
023500070907     c                   eval      §TLLSOIND  = %replace(iacmi:§TLLSOIND:
023600070907     c                                             %scan(iapmi:§TLLSOIND))
023700080630     c                   enddo
023800080630     c                   dow       %scan(iapmi:§TLLSOLOD) > *zeros
023900070907     c                   eval      §TLLSOLOD  = %replace(iacmi:§TLLSOLOD:
024000070907     c                                             %scan(iapmi:§TLLSOLOD))
024100080630     c                   enddo
024200080630     c                   dow       %scan(iapmi:§TLLSOLODN) > *zeros
024300070907     c                   eval      §TLLSOLODN = %replace(iacmi:§TLLSOLODN:
024400070907     c                                             %scan(iapmi:§TLLSOLODN))
024500080630     c                   enddo
024600070907     c* - o' => ò
024700080630     c                   dow       %scan(oapmi:§TLLSOIND) > *zeros
024800070907     c                   eval      §TLLSOIND  = %replace(oacmi:§TLLSOIND:
024900070907     c                                             %scan(oapmi:§TLLSOIND))
025000080630     c                   enddo
025100080630     c                   dow       %scan(oapmi:§TLLSOLOD) > *zeros
025200070907     c                   eval      §TLLSOLOD  = %replace(oacmi:§TLLSOLOD:
025300070907     c                                             %scan(oapmi:§TLLSOLOD))
025400080630     c                   enddo
025500080630     c                   dow       %scan(oapmi:§TLLSOLODN) > *zeros
025600070907     c                   eval      §TLLSOLODN = %replace(oacmi:§TLLSOLODN:
025700070907     c                                             %scan(oapmi:§TLLSOLODN))
025800080630     c                   enddo
025900070907     c* - u' => ù
026000080630     c                   dow       %scan(uapmi:§TLLSOIND) > *zeros
026100070907     c                   eval      §TLLSOIND  = %replace(uacmi:§TLLSOIND:
026200070907     c                                             %scan(uapmi:§TLLSOIND))
026300080630     c                   enddo
026400080630     c                   dow       %scan(uapmi:§TLLSOLOD) > *zeros
026500070907     c                   eval      §TLLSOLOD  = %replace(uacmi:§TLLSOLOD:
026600070907     c                                             %scan(uapmi:§TLLSOLOD))
026700080630     c                   enddo
026800080630     c                   dow       %scan(uapmi:§TLLSOLODN) > *zeros
026900070907     c                   eval      §TLLSOLODN = %replace(uacmi:§TLLSOLODN:
027000070907     c                                             %scan(uapmi:§TLLSOLODN))
027100080630     c                   enddo
027200070913     c*
027300070913     c* Forzatrura speciale x strade statali
027400080630     c                   dow       %scan('s.s.':§TLLSOIND) > *zeros
027500070913     c                   eval      §TLLSOIND = %replace('ss  ':§TLLSOIND:
027600070913     c                                            %scan('s.s.':§TLLSOIND))
027700080630     c                   enddo
027800080630     c                   dow       %scan('S.S.':§TLLSOIND) > *zeros
027900070913     c                   eval      §TLLSOIND = %replace('SS  ':§TLLSOIND:
028000070913     c                                            %scan('S.S.':§TLLSOIND))
028100080630     c                   enddo
028200050121     C*
028300050121     C* Inizializzazione campi e variabili d out
028400060804     C                   clear                   tivaw000
028500060804     C*
028600070913     C                   eval      wRecOut = %subst(dtllso:1:157)
028700060804     C*
028800060804     C* Effettuo determinate considerazioni x portare nella forma ASCII editata con segno
028900060804     C* le coordinale geo-stazionarie
029000060804     C                   if        §tllsoXLO < *zeros
029100060804     C                   eval      wNum11_8 = %abs(§tllsoXLO)
029200060804     C                   eval      wSegno = '-'
029300060804     C                   else
029400060804     C                   eval      wNum11_8 = §tllsoXLO
029500060804     C                   eval      wSegno = *blanks
029600060804     C                   endif
029700060804     C                   move(p)   wNum11_8      wAlfa11
029800060804     C                   eval      wRecOut = wRecOut + wSegno +
029900060804     C                                       %subst(wAlfa11:1:3) + wSepDec +
030000060804     C                                       %subst(wAlfa11:4:8)
030100060804     C*
030200060804     C                   if        §tllsoYLA < *zeros
030300060804     C                   eval      wNum11_8 = %abs(§tllsoYLA)
030400060804     C                   eval      wSegno = '-'
030500060804     C                   else
030600060804     C                   eval      wNum11_8 = §tllsoYLA
030700060804     C                   eval      wSegno = *blanks
030800060804     C                   endif
030900060804     C                   move(p)   wNum11_8      wAlfa11
031000060804     C                   eval      wRecOut = wRecOut + wSegno +
031100060804     C                                       %subst(wAlfa11:1:3) + wSepDec +
031200060804     C                                       %subst(wAlfa11:4:8)
031300070112     C*
031400070112     C* Quindi proseguo con il resto dei dati
031500070112     C                   eval      wRecOut = %subst(wRecOut:1:183) +
031600070913     C                             %subst(vgdDTA:180:%len(dtllso)-180+1)
031700160504     C*
031800160616     C* Verifico se elaborazione per bolle o per ORM o "varie"
031900160616     C* => se no bolle "overrido" indietro i dati originali
032000160616     C                   select
032100160616     C                   when      %subst(prmppt:1:2) = 'OO'
032200160616     C                   eval      %subst(wRecOut:1:26) = %subst(dtlloo:1:26)
032300160616     C*
032400160616     C                   when      %subst(prmppt:1:2) = 'XO'
032500160616     C                   eval      %subst(wRecOut:1:26) = %subst(dtllxo:1:26)
032600160616     C                   endsl
032700060804     C*
032800060804     C* Valorizzo il buffer del file d output
032900060804     C                   eval      vawdta = wRecOut
033000930409     C*
033100050121     C                   WRITE     TIVAW000
033200050909     C                   DELETE    TIVGD000
033300991027     C*
033400050909     C                   READ      TIVGDTMP
033500991027     C                   ENDdo
033600991027     C*
033700991027     C                   eval      wrkesito = '0'
033800991027     C*
033900910830     C                   ENDSR
034000060804
034100991027
034200991027      /TITLE Operazioni iniziali.
034300991027     C     *inzsr        BEGSR
034400991027     C*
034500991027     C     *ENTRY        PLIST
034600991027     C                   parm                    prmppt
034700991027     C     wrkesito      parm      wrkesito      prmesito
034800101129     C*
034900991027     C                   ENDSR
