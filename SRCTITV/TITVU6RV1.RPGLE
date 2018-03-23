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
005000070907     c*
005100070907     c* Forzatrura speciale x lettere accentate
005200070907     c* - A' => À
005300070907     c                   if        %scan(aapma:§TLLSOIND) > *zeros
005400070907     c                   eval      §TLLSOIND  = %replace(aacma:§TLLSOIND:
005500070907     c                                             %scan(aapma:§TLLSOIND))
005600070907     c                   endif
005700070907     c                   if        %scan(aapma:§TLLSOLOD) > *zeros
005800070907     c                   eval      §TLLSOLOD  = %replace(aacma:§TLLSOLOD:
005900070907     c                                             %scan(aapma:§TLLSOLOD))
006000070907     c                   endif
006100070907     c                   if        %scan(aapma:§TLLSOLODN) > *zeros
006200070907     c                   eval      §TLLSOLODN = %replace(aacma:§TLLSOLODN:
006300070907     c                                             %scan(aapma:§TLLSOLODN))
006400070907     c                   endif
006500070907     c* - E' => È
006600070907     c                   if        %scan(eapma:§TLLSOIND) > *zeros
006700070907     c                   eval      §TLLSOIND  = %replace(eacma:§TLLSOIND:
006800070907     c                                             %scan(eapma:§TLLSOIND))
006900070907     c                   endif
007000070907     c                   if        %scan(eapma:§TLLSOLOD) > *zeros
007100070907     c                   eval      §TLLSOLOD  = %replace(eacma:§TLLSOLOD:
007200070907     c                                             %scan(eapma:§TLLSOLOD))
007300070907     c                   endif
007400070907     c                   if        %scan(eapma:§TLLSOLODN) > *zeros
007500070907     c                   eval      §TLLSOLODN = %replace(eacma:§TLLSOLODN:
007600070907     c                                             %scan(eapma:§TLLSOLODN))
007700070907     c                   endif
007800070907     c* - I' => Ì
007900070907     c                   if        %scan(iapma:§TLLSOIND) > *zeros
008000070907     c                   eval      §TLLSOIND  = %replace(iacma:§TLLSOIND:
008100070907     c                                             %scan(iapma:§TLLSOIND))
008200070907     c                   endif
008300070907     c                   if        %scan(iapma:§TLLSOLOD) > *zeros
008400070907     c                   eval      §TLLSOLOD  = %replace(iacma:§TLLSOLOD:
008500070907     c                                             %scan(iapma:§TLLSOLOD))
008600070907     c                   endif
008700070907     c                   if        %scan(iapma:§TLLSOLODN) > *zeros
008800070907     c                   eval      §TLLSOLODN = %replace(iacma:§TLLSOLODN:
008900070907     c                                             %scan(iapma:§TLLSOLODN))
009000070907     c                   endif
009100070907     c* - O' => Ò
009200070907     c                   if        %scan(oapma:§TLLSOIND) > *zeros
009300070907     c                   eval      §TLLSOIND  = %replace(oacma:§TLLSOIND:
009400070907     c                                             %scan(oapma:§TLLSOIND))
009500070907     c                   endif
009600070907     c                   if        %scan(oapma:§TLLSOLOD) > *zeros
009700070907     c                   eval      §TLLSOLOD  = %replace(oacma:§TLLSOLOD:
009800070907     c                                             %scan(oapma:§TLLSOLOD))
009900070907     c                   endif
010000070907     c                   if        %scan(oapma:§TLLSOLODN) > *zeros
010100070907     c                   eval      §TLLSOLODN = %replace(oacma:§TLLSOLODN:
010200070907     c                                             %scan(oapma:§TLLSOLODN))
010300070907     c                   endif
010400070907     c* - U' => Ù
010500070907     c                   if        %scan(uapma:§TLLSOIND) > *zeros
010600070907     c                   eval      §TLLSOIND  = %replace(uacma:§TLLSOIND:
010700070907     c                                             %scan(uapma:§TLLSOIND))
010800070907     c                   endif
010900070907     c                   if        %scan(uapma:§TLLSOLOD) > *zeros
011000070907     c                   eval      §TLLSOLOD  = %replace(uacma:§TLLSOLOD:
011100070907     c                                             %scan(uapma:§TLLSOLOD))
011200070907     c                   endif
011300070907     c                   if        %scan(uapma:§TLLSOLODN) > *zeros
011400070907     c                   eval      §TLLSOLODN = %replace(uacma:§TLLSOLODN:
011500070907     c                                             %scan(uapma:§TLLSOLODN))
011600070907     c                   endif
011700070907     c* - a' => à
011800070907     c                   if        %scan(aapmi:§TLLSOIND) > *zeros
011900070907     c                   eval      §TLLSOIND  = %replace(aacmi:§TLLSOIND:
012000070907     c                                             %scan(aapmi:§TLLSOIND))
012100070907     c                   endif
012200070907     c                   if        %scan(aapmi:§TLLSOLOD) > *zeros
012300070907     c                   eval      §TLLSOLOD  = %replace(aacmi:§TLLSOLOD:
012400070907     c                                             %scan(aapmi:§TLLSOLOD))
012500070907     c                   endif
012600070907     c                   if        %scan(aapmi:§TLLSOLODN) > *zeros
012700070907     c                   eval      §TLLSOLODN = %replace(aacmi:§TLLSOLODN:
012800070907     c                                             %scan(aapmi:§TLLSOLODN))
012900070907     c                   endif
013000070907     c* - e' => è
013100070907     c                   if        %scan(eapmi:§TLLSOIND) > *zeros
013200070907     c                   eval      §TLLSOIND  = %replace(eacmi:§TLLSOIND:
013300070907     c                                             %scan(eapmi:§TLLSOIND))
013400070907     c                   endif
013500070907     c                   if        %scan(eapmi:§TLLSOLOD) > *zeros
013600070907     c                   eval      §TLLSOLOD  = %replace(eacmi:§TLLSOLOD:
013700070907     c                                             %scan(eapmi:§TLLSOLOD))
013800070907     c                   endif
013900070907     c                   if        %scan(eapmi:§TLLSOLODN) > *zeros
014000070907     c                   eval      §TLLSOLODN = %replace(eacmi:§TLLSOLODN:
014100070907     c                                             %scan(eapmi:§TLLSOLODN))
014200070907     c                   endif
014300070907     c* - i' => ì
014400070907     c                   if        %scan(iapmi:§TLLSOIND) > *zeros
014500070907     c                   eval      §TLLSOIND  = %replace(iacmi:§TLLSOIND:
014600070907     c                                             %scan(iapmi:§TLLSOIND))
014700070907     c                   endif
014800070907     c                   if        %scan(iapmi:§TLLSOLOD) > *zeros
014900070907     c                   eval      §TLLSOLOD  = %replace(iacmi:§TLLSOLOD:
015000070907     c                                             %scan(iapmi:§TLLSOLOD))
015100070907     c                   endif
015200070907     c                   if        %scan(iapmi:§TLLSOLODN) > *zeros
015300070907     c                   eval      §TLLSOLODN = %replace(iacmi:§TLLSOLODN:
015400070907     c                                             %scan(iapmi:§TLLSOLODN))
015500070907     c                   endif
015600070907     c* - o' => ò
015700070907     c                   if        %scan(oapmi:§TLLSOIND) > *zeros
015800070907     c                   eval      §TLLSOIND  = %replace(oacmi:§TLLSOIND:
015900070907     c                                             %scan(oapmi:§TLLSOIND))
016000070907     c                   endif
016100070907     c                   if        %scan(oapmi:§TLLSOLOD) > *zeros
016200070907     c                   eval      §TLLSOLOD  = %replace(oacmi:§TLLSOLOD:
016300070907     c                                             %scan(oapmi:§TLLSOLOD))
016400070907     c                   endif
016500070907     c                   if        %scan(oapmi:§TLLSOLODN) > *zeros
016600070907     c                   eval      §TLLSOLODN = %replace(oacmi:§TLLSOLODN:
016700070907     c                                             %scan(oapmi:§TLLSOLODN))
016800070907     c                   endif
016900070907     c* - u' => ù
017000070907     c                   if        %scan(uapmi:§TLLSOIND) > *zeros
017100070907     c                   eval      §TLLSOIND  = %replace(uacmi:§TLLSOIND:
017200070907     c                                             %scan(uapmi:§TLLSOIND))
017300070907     c                   endif
017400070907     c                   if        %scan(uapmi:§TLLSOLOD) > *zeros
017500070907     c                   eval      §TLLSOLOD  = %replace(uacmi:§TLLSOLOD:
017600070907     c                                             %scan(uapmi:§TLLSOLOD))
017700070907     c                   endif
017800070907     c                   if        %scan(uapmi:§TLLSOLODN) > *zeros
017900070907     c                   eval      §TLLSOLODN = %replace(uacmi:§TLLSOLODN:
018000070907     c                                             %scan(uapmi:§TLLSOLODN))
018100070907     c                   endif
018200070913     c*
018300070913     c* Forzatrura speciale x strade statali
018400070913     c                   if        %scan('s.s.':§TLLSOIND) > *zeros
018500070913     c                   eval      §TLLSOIND = %replace('ss  ':§TLLSOIND:
018600070913     c                                            %scan('s.s.':§TLLSOIND))
018700070913     c                   endif
018800070913     c                   if        %scan('S.S.':§TLLSOIND) > *zeros
018900070913     c                   eval      §TLLSOIND = %replace('SS  ':§TLLSOIND:
019000070913     c                                            %scan('S.S.':§TLLSOIND))
019100070913     c                   endif
019200050121     C*
019300050121     C* Inizializzazione campi e variabili d out
019400060804     C                   clear                   tivaw000
019500060804     C*
019600070913     C                   eval      wRecOut = %subst(dtllso:1:157)
019700060804     C*
019800060804     C* Effettuo determinate considerazioni x portare nella forma ASCII editata con segno
019900060804     C* le coordinale geo-stazionarie
020000060804     C                   if        §tllsoXLO < *zeros
020100060804     C                   eval      wNum11_8 = %abs(§tllsoXLO)
020200060804     C                   eval      wSegno = '-'
020300060804     C                   else
020400060804     C                   eval      wNum11_8 = §tllsoXLO
020500060804     C                   eval      wSegno = *blanks
020600060804     C                   endif
020700060804     C                   move(p)   wNum11_8      wAlfa11
020800060804     C                   eval      wRecOut = wRecOut + wSegno +
020900060804     C                                       %subst(wAlfa11:1:3) + wSepDec +
021000060804     C                                       %subst(wAlfa11:4:8)
021100060804     C*
021200060804     C                   if        §tllsoYLA < *zeros
021300060804     C                   eval      wNum11_8 = %abs(§tllsoYLA)
021400060804     C                   eval      wSegno = '-'
021500060804     C                   else
021600060804     C                   eval      wNum11_8 = §tllsoYLA
021700060804     C                   eval      wSegno = *blanks
021800060804     C                   endif
021900060804     C                   move(p)   wNum11_8      wAlfa11
022000060804     C                   eval      wRecOut = wRecOut + wSegno +
022100060804     C                                       %subst(wAlfa11:1:3) + wSepDec +
022200060804     C                                       %subst(wAlfa11:4:8)
022300070112     C*
022400070112     C* Quindi proseguo con il resto dei dati
022500070112     C                   eval      wRecOut = %subst(wRecOut:1:183) +
022600070913     C                             %subst(vgdDTA:180:%len(dtllso)-180+1)
022700060804     C*
022800060804     C* Valorizzo il buffer del file d output
022900060804     C                   eval      vawdta = wRecOut
023000930409     C*
023100050121     C                   WRITE     TIVAW000
023200050909     C                   DELETE    TIVGD000
023300991027     C*
023400050909     C                   READ      TIVGDTMP
023500991027     C                   ENDdo
023600991027     C*
023700991027     C                   eval      wrkesito = '0'
023800991027     C*
023900910830     C                   ENDSR
024000060804
024100991027
024200991027      /TITLE Operazioni iniziali.
024300991027     C     *inzsr        BEGSR
024400991027     C*
024500991027     C     *ENTRY        PLIST
024600991027     C                   parm                    prmppt
024700991027     C     wrkesito      parm      wrkesito      prmesito
024800991027     C*
024900991027     C                   ENDSR
