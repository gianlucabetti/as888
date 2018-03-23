000100890921     H DECEDIT('0,') DATEDIT(*DMY.)
000200991027     H dftactgrp(*yes)
000300991027
000400050909     FTIVGDTMP  UF   E             DISK
000500050121     Ftivawwwt  O    E             DISK
000600991027
000700060804     D dtllso        e ds
000800991027     D prmppt          s             50
000900991027     D prmesito        s              1
001000991027     D wrkesito        s                   like(prmesito)
001100060804     D wNum11_8        s             11  8 inz
001200060804     D wAlfa11         s             11    inz
001300060804     D wSegno          s              1    inz
001400060804     D wSepDec         s              1    inz(',')
001500070112     D wRecOut         s           1024    varying
001600070907
001700070907     D*
001800070907     D aapma           s              2    inz('A''')
001900070907     D aacma           s              2    inz('À ')
002000070907     D eapma           s              2    inz('E''')
002100070907     D eacma           s              2    inz('È ')
002200070907     D iapma           s              2    inz('I''')
002300070907     D iacma           s              2    inz('Ì ')
002400070907     D oapma           s              2    inz('O''')
002500070907     D oacma           s              2    inz('Ò ')
002600070907     D uapma           s              2    inz('U''')
002700070907     D uacma           s              2    inz('Ù ')
002800070907     D aapmi           s              2    inz('a''')
002900070907     D aacmi           s              2    inz('à ')
003000070907     D eapmi           s              2    inz('e''')
003100070907     D eacmi           s              2    inz('è ')
003200070907     D iapmi           s              2    inz('i''')
003300070907     D iacmi           s              2    inz('ì ')
003400070907     D oapmi           s              2    inz('o''')
003500070907     D oacmi           s              2    inz('ò ')
003600070907     D uapmi           s              2    inz('u''')
003700070907     D uacmi           s              2    inz('ù ')
003800070907     D*
003900070907
004000070907
004100060804     C                   exsr      traduci
004200060804     C                   seton                                        LR
004300991027
004400060804
004500991027     C     traduci       BEGSR
004600991027     C*
004700060804     C                   read      TIVGDTMP
004800991027     C                   dow       not %eof
004900060804     C                   eval      dtllso = vgdDTA
005000080122     c*
005100080122     c* Forzatrura speciale x "CENTERGROSS"
005200080122     c* (in quanto nn esiste in cartografia ma nel ns cappario invece sì)
005300080122     c                   if        §tllsoPRD  = 'BO'                 AND
005400080122     c                             %trim(§tllsoLOD)  = 'CENTERGROSS' AND
005500080122     c                             %trim(§tllsoLODN) = *blanks
005600080122     C                   eval      §tllsoLODN  = 'FUNO'
005700080122     c                   endif
005800080122     c                   if        §tllsoPRD  = 'BO'                 AND
005900080122     c                             %trim(§tllsoLODN) = 'CENTERGROSS'
006000080122     C                   eval      §tllsoLODN  = 'FUNO'
006100080122     c                   endif
006200080122     c*
006300080122     c* Forzatrura speciale x "CASCINE VICA"
006400080122     c* (in quanto nn esiste in cartografia ma nel ns cappario invece sì)
006500080122     c                   if        §tllsoPRD  = 'TO'                  AND
006600080122     c                             %trim(§tllsoLOD)  = 'CASCINE VICA' AND
006700080122     c                             %trim(§tllsoLODN) = *blanks
006800080122     C                   eval      §tllsoLODN  = 'RIVOLI'
006900080122     c                   endif
007000080122     c                   if        §tllsoPRD  = 'TO'                  AND
007100080122     c                             %trim(§tllsoLODN) = 'CASCINE VICA'
007200080122     C                   eval      §tllsoLODN  = 'RIVOLI'
007300080122     c                   endif
007400080122     c*
007500080122     c* Forzatura "vecchie" provincie su "nuove" provincie
007600080122     c                   select
007700080122     c                   when      §TLLSOPRD = 'OT'
007800080122     c                   eval      §TLLSOPRD = 'SS'
007900080122     c                   when      §TLLSOPRD = 'OG'
008000080122     c                   eval      §TLLSOPRD = 'NU'
008100080122     c                   when      §TLLSOPRD = 'CI'
008200080122     c                   eval      §TLLSOPRD = 'CA'
008300080122     c                   when      §TLLSOPRD = 'MD'
008400080122     c                   eval      §TLLSOPRD = 'CA'
008500080122     c                   when      §TLLSOPRD = 'VS'
008600080122     c                   eval      §TLLSOPRD = 'CA'
008700080122     c                   when      §TLLSOPRD = 'FO'
008800080122     c                   eval      §TLLSOPRD = 'FC'
008900080122     c                   when      §TLLSOPRD = 'PS'
009000080122     c                   eval      §TLLSOPRD = 'PU'
009100080122     c* ...x ogni evenienza se provincia nn fosse valorizzata => fisso BO
009200080122     c                   when      §TLLSOPRD = *blanks
009300080122     c                   eval      §TLLSOPRD = 'BO'
009400080122     c                   endsl
009500080122     c*
009600080122     c* Forzatura x San Marino e Vaticano
009700080122     c                   select
009800080122     c                   when      §TLLSOPRD  = 'RM'    and
009900080122     c                             §TLLSOCAD  = '00120'
010000080122     c                   eval      §TLLSOPRD  = 'CV'
010100080122     c                   when      §TLLSOPRD  = 'RN'    and
010200080122     c                             §TLLSOCAD >= '47890' and
010300080122     c                             §TLLSOCAD <= '47899'
010400080122     c                   eval      §TLLSOPRD  = 'SM'
010500080122     c                   endsl
010600070907     c*
010700070907     c* Forzatrura speciale x lettere accentate
010800070907     c* - A' => À
010900070907     c                   if        %scan(aapma:§TLLSOIND) > *zeros
011000070907     c                   eval      §TLLSOIND  = %replace(aacma:§TLLSOIND:
011100070907     c                                             %scan(aapma:§TLLSOIND))
011200070907     c                   endif
011300070907     c                   if        %scan(aapma:§TLLSOLOD) > *zeros
011400070907     c                   eval      §TLLSOLOD  = %replace(aacma:§TLLSOLOD:
011500070907     c                                             %scan(aapma:§TLLSOLOD))
011600070907     c                   endif
011700070907     c                   if        %scan(aapma:§TLLSOLODN) > *zeros
011800070907     c                   eval      §TLLSOLODN = %replace(aacma:§TLLSOLODN:
011900070907     c                                             %scan(aapma:§TLLSOLODN))
012000070907     c                   endif
012100070907     c* - E' => È
012200070907     c                   if        %scan(eapma:§TLLSOIND) > *zeros
012300070907     c                   eval      §TLLSOIND  = %replace(eacma:§TLLSOIND:
012400070907     c                                             %scan(eapma:§TLLSOIND))
012500070907     c                   endif
012600070907     c                   if        %scan(eapma:§TLLSOLOD) > *zeros
012700070907     c                   eval      §TLLSOLOD  = %replace(eacma:§TLLSOLOD:
012800070907     c                                             %scan(eapma:§TLLSOLOD))
012900070907     c                   endif
013000070907     c                   if        %scan(eapma:§TLLSOLODN) > *zeros
013100070907     c                   eval      §TLLSOLODN = %replace(eacma:§TLLSOLODN:
013200070907     c                                             %scan(eapma:§TLLSOLODN))
013300070907     c                   endif
013400070907     c* - I' => Ì
013500070907     c                   if        %scan(iapma:§TLLSOIND) > *zeros
013600070907     c                   eval      §TLLSOIND  = %replace(iacma:§TLLSOIND:
013700070907     c                                             %scan(iapma:§TLLSOIND))
013800070907     c                   endif
013900070907     c                   if        %scan(iapma:§TLLSOLOD) > *zeros
014000070907     c                   eval      §TLLSOLOD  = %replace(iacma:§TLLSOLOD:
014100070907     c                                             %scan(iapma:§TLLSOLOD))
014200070907     c                   endif
014300070907     c                   if        %scan(iapma:§TLLSOLODN) > *zeros
014400070907     c                   eval      §TLLSOLODN = %replace(iacma:§TLLSOLODN:
014500070907     c                                             %scan(iapma:§TLLSOLODN))
014600070907     c                   endif
014700070907     c* - O' => Ò
014800070907     c                   if        %scan(oapma:§TLLSOIND) > *zeros
014900070907     c                   eval      §TLLSOIND  = %replace(oacma:§TLLSOIND:
015000070907     c                                             %scan(oapma:§TLLSOIND))
015100070907     c                   endif
015200070907     c                   if        %scan(oapma:§TLLSOLOD) > *zeros
015300070907     c                   eval      §TLLSOLOD  = %replace(oacma:§TLLSOLOD:
015400070907     c                                             %scan(oapma:§TLLSOLOD))
015500070907     c                   endif
015600070907     c                   if        %scan(oapma:§TLLSOLODN) > *zeros
015700070907     c                   eval      §TLLSOLODN = %replace(oacma:§TLLSOLODN:
015800070907     c                                             %scan(oapma:§TLLSOLODN))
015900070907     c                   endif
016000070907     c* - U' => Ù
016100070907     c                   if        %scan(uapma:§TLLSOIND) > *zeros
016200070907     c                   eval      §TLLSOIND  = %replace(uacma:§TLLSOIND:
016300070907     c                                             %scan(uapma:§TLLSOIND))
016400070907     c                   endif
016500070907     c                   if        %scan(uapma:§TLLSOLOD) > *zeros
016600070907     c                   eval      §TLLSOLOD  = %replace(uacma:§TLLSOLOD:
016700070907     c                                             %scan(uapma:§TLLSOLOD))
016800070907     c                   endif
016900070907     c                   if        %scan(uapma:§TLLSOLODN) > *zeros
017000070907     c                   eval      §TLLSOLODN = %replace(uacma:§TLLSOLODN:
017100070907     c                                             %scan(uapma:§TLLSOLODN))
017200070907     c                   endif
017300070907     c* - a' => à
017400070907     c                   if        %scan(aapmi:§TLLSOIND) > *zeros
017500070907     c                   eval      §TLLSOIND  = %replace(aacmi:§TLLSOIND:
017600070907     c                                             %scan(aapmi:§TLLSOIND))
017700070907     c                   endif
017800070907     c                   if        %scan(aapmi:§TLLSOLOD) > *zeros
017900070907     c                   eval      §TLLSOLOD  = %replace(aacmi:§TLLSOLOD:
018000070907     c                                             %scan(aapmi:§TLLSOLOD))
018100070907     c                   endif
018200070907     c                   if        %scan(aapmi:§TLLSOLODN) > *zeros
018300070907     c                   eval      §TLLSOLODN = %replace(aacmi:§TLLSOLODN:
018400070907     c                                             %scan(aapmi:§TLLSOLODN))
018500070907     c                   endif
018600070907     c* - e' => è
018700070907     c                   if        %scan(eapmi:§TLLSOIND) > *zeros
018800070907     c                   eval      §TLLSOIND  = %replace(eacmi:§TLLSOIND:
018900070907     c                                             %scan(eapmi:§TLLSOIND))
019000070907     c                   endif
019100070907     c                   if        %scan(eapmi:§TLLSOLOD) > *zeros
019200070907     c                   eval      §TLLSOLOD  = %replace(eacmi:§TLLSOLOD:
019300070907     c                                             %scan(eapmi:§TLLSOLOD))
019400070907     c                   endif
019500070907     c                   if        %scan(eapmi:§TLLSOLODN) > *zeros
019600070907     c                   eval      §TLLSOLODN = %replace(eacmi:§TLLSOLODN:
019700070907     c                                             %scan(eapmi:§TLLSOLODN))
019800070907     c                   endif
019900070907     c* - i' => ì
020000070907     c                   if        %scan(iapmi:§TLLSOIND) > *zeros
020100070907     c                   eval      §TLLSOIND  = %replace(iacmi:§TLLSOIND:
020200070907     c                                             %scan(iapmi:§TLLSOIND))
020300070907     c                   endif
020400070907     c                   if        %scan(iapmi:§TLLSOLOD) > *zeros
020500070907     c                   eval      §TLLSOLOD  = %replace(iacmi:§TLLSOLOD:
020600070907     c                                             %scan(iapmi:§TLLSOLOD))
020700070907     c                   endif
020800070907     c                   if        %scan(iapmi:§TLLSOLODN) > *zeros
020900070907     c                   eval      §TLLSOLODN = %replace(iacmi:§TLLSOLODN:
021000070907     c                                             %scan(iapmi:§TLLSOLODN))
021100070907     c                   endif
021200070907     c* - o' => ò
021300070907     c                   if        %scan(oapmi:§TLLSOIND) > *zeros
021400070907     c                   eval      §TLLSOIND  = %replace(oacmi:§TLLSOIND:
021500070907     c                                             %scan(oapmi:§TLLSOIND))
021600070907     c                   endif
021700070907     c                   if        %scan(oapmi:§TLLSOLOD) > *zeros
021800070907     c                   eval      §TLLSOLOD  = %replace(oacmi:§TLLSOLOD:
021900070907     c                                             %scan(oapmi:§TLLSOLOD))
022000070907     c                   endif
022100070907     c                   if        %scan(oapmi:§TLLSOLODN) > *zeros
022200070907     c                   eval      §TLLSOLODN = %replace(oacmi:§TLLSOLODN:
022300070907     c                                             %scan(oapmi:§TLLSOLODN))
022400070907     c                   endif
022500070907     c* - u' => ù
022600070907     c                   if        %scan(uapmi:§TLLSOIND) > *zeros
022700070907     c                   eval      §TLLSOIND  = %replace(uacmi:§TLLSOIND:
022800070907     c                                             %scan(uapmi:§TLLSOIND))
022900070907     c                   endif
023000070907     c                   if        %scan(uapmi:§TLLSOLOD) > *zeros
023100070907     c                   eval      §TLLSOLOD  = %replace(uacmi:§TLLSOLOD:
023200070907     c                                             %scan(uapmi:§TLLSOLOD))
023300070907     c                   endif
023400070907     c                   if        %scan(uapmi:§TLLSOLODN) > *zeros
023500070907     c                   eval      §TLLSOLODN = %replace(uacmi:§TLLSOLODN:
023600070907     c                                             %scan(uapmi:§TLLSOLODN))
023700070907     c                   endif
023800070913     c*
023900070913     c* Forzatrura speciale x strade statali
024000070913     c                   if        %scan('s.s.':§TLLSOIND) > *zeros
024100070913     c                   eval      §TLLSOIND = %replace('ss  ':§TLLSOIND:
024200070913     c                                            %scan('s.s.':§TLLSOIND))
024300070913     c                   endif
024400070913     c                   if        %scan('S.S.':§TLLSOIND) > *zeros
024500070913     c                   eval      §TLLSOIND = %replace('SS  ':§TLLSOIND:
024600070913     c                                            %scan('S.S.':§TLLSOIND))
024700070913     c                   endif
024800050121     C*
024900050121     C* Inizializzazione campi e variabili d out
025000060804     C                   clear                   tivaw000
025100060804     C*
025200070913     C                   eval      wRecOut = %subst(dtllso:1:157)
025300060804     C*
025400060804     C* Effettuo determinate considerazioni x portare nella forma ASCII editata con segno
025500060804     C* le coordinale geo-stazionarie
025600060804     C                   if        §tllsoXLO < *zeros
025700060804     C                   eval      wNum11_8 = %abs(§tllsoXLO)
025800060804     C                   eval      wSegno = '-'
025900060804     C                   else
026000060804     C                   eval      wNum11_8 = §tllsoXLO
026100060804     C                   eval      wSegno = *blanks
026200060804     C                   endif
026300060804     C                   move(p)   wNum11_8      wAlfa11
026400060804     C                   eval      wRecOut = wRecOut + wSegno +
026500060804     C                                       %subst(wAlfa11:1:3) + wSepDec +
026600060804     C                                       %subst(wAlfa11:4:8)
026700060804     C*
026800060804     C                   if        §tllsoYLA < *zeros
026900060804     C                   eval      wNum11_8 = %abs(§tllsoYLA)
027000060804     C                   eval      wSegno = '-'
027100060804     C                   else
027200060804     C                   eval      wNum11_8 = §tllsoYLA
027300060804     C                   eval      wSegno = *blanks
027400060804     C                   endif
027500060804     C                   move(p)   wNum11_8      wAlfa11
027600060804     C                   eval      wRecOut = wRecOut + wSegno +
027700060804     C                                       %subst(wAlfa11:1:3) + wSepDec +
027800060804     C                                       %subst(wAlfa11:4:8)
027900070112     C*
028000070112     C* Quindi proseguo con il resto dei dati
028100070112     C                   eval      wRecOut = %subst(wRecOut:1:183) +
028200070913     C                             %subst(vgdDTA:180:%len(dtllso)-180+1)
028300060804     C*
028400060804     C* Valorizzo il buffer del file d output
028500060804     C                   eval      vawdta = wRecOut
028600930409     C*
028700050121     C                   WRITE     TIVAW000
028800050909     C                   DELETE    TIVGD000
028900991027     C*
029000050909     C                   READ      TIVGDTMP
029100991027     C                   ENDdo
029200991027     C*
029300991027     C                   eval      wrkesito = '0'
029400991027     C*
029500910830     C                   ENDSR
029600060804
029700991027
029800991027      /TITLE Operazioni iniziali.
029900991027     C     *inzsr        BEGSR
030000991027     C*
030100991027     C     *ENTRY        PLIST
030200991027     C                   parm                    prmppt
030300991027     C     wrkesito      parm      wrkesito      prmesito
030400991027     C*
030500991027     C                   ENDSR
