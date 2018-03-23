000100890921     H DECEDIT('0,') DATEDIT(*DMY.)
000200991027     H dftactgrp(*yes)
000300991027
000400050909     FTIVGDTMP  UF   E             DISK
000500101129     Fwacpmod2l IF   E           K DISK
000600111125     Ffilte01l  UF   E           K DISK
000700050121     Ftivawwwt  O    E             DISK
000800991027
000900060804     D dtllso        e ds
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
004100111124
004200111124     D*-------------------
004300111124     D* COSTANTI
004400111124     D*-------------------
004500111124     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
004600111124     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
004700111124
004800070907
004900070907
005000060804     C                   exsr      traduci
005100111125     C*
005200111125     C* Gestisco eventuale "scarto totale"
005300111125     C                   if        not *in40
005400111125     C                   move(p)   vgdPGM        wIDB              7 0
005500111125     C     wIDB          chain     filte01l
005600111125     C                   if        %found(filte01l)
005700111125     C                   eval      lteHDL  = 'ANNULLATO'
005800111125     C                   eval      lteDATU = lteDAT
005900111125     C                   eval      lteORAU = lteORA
006000111125     C                   update    filte000
006100111125     C                   endif
006200111125     C                   endif
006300111125     C*
006400060804     C                   seton                                        LR
006500111125
006600991027
006700060804
006800991027     C     traduci       BEGSR
006900991027     C*
007000111125     C                   setoff                                       40
007100111125     C                   z-add     2             wChLimite         1 0
007200111125     C*
007300060804     C                   read      TIVGDTMP
007400991027     C                   dow       not %eof
007500060804     C                   eval      dtllso = vgdDTA
007600111124     C*
007700111124     C                   z-add     1             idx               2 0
007800111124     C                   z-add     *zeros        wChOK             2 0
007900111124     C                   dow       idx < %len(%trim(§TLLSOIND)) and wChOK < 3
008000111124     C                   if        %scan(%subst(%trim(§TLLSOIND):idx:1):
008100111124     C                                    minu:1) > 1 OR
008200111124     C                             %scan(%subst(%trim(§TLLSOIND):idx:1):
008300111124     C                                    MAIU:1) > 1
008400111124     C                   eval      wChOK = wChOK + 1
008500111124     C                   endif
008600111124     C                   add       1             idx
008700111124     C                   enddo
008800111124     C*
008900111125     C* Se nn c sono almeno <n> caratteri alfabetici => escludo il record corrente
009000111125     C                   if        wChOK < wChLimite
009100111124     C                   else
009200111124     C*
009300111125     C* Se elaboro qualcosa (ovvero mando a Tellus) => tengo traccia
009400111125     C                   seton                                        40
009500080122     c*
009600080122     c* Forzatrura speciale x "CENTERGROSS"
009700080122     c* (in quanto nn esiste in cartografia ma nel ns cappario invece sì)
009800080122     c                   if        §tllsoPRD  = 'BO'                 AND
009900080122     c                             %trim(§tllsoLOD)  = 'CENTERGROSS' AND
010000080122     c                             %trim(§tllsoLODN) = *blanks
010100080122     C                   eval      §tllsoLODN  = 'FUNO'
010200080122     c                   endif
010300080122     c                   if        §tllsoPRD  = 'BO'                 AND
010400080122     c                             %trim(§tllsoLODN) = 'CENTERGROSS'
010500080122     C                   eval      §tllsoLODN  = 'FUNO'
010600080122     c                   endif
010700080122     c*
010800080122     c* Forzatrura speciale x "CASCINE VICA"
010900080122     c* (in quanto nn esiste in cartografia ma nel ns cappario invece sì)
011000080122     c                   if        §tllsoPRD  = 'TO'                  AND
011100080122     c                             %trim(§tllsoLOD)  = 'CASCINE VICA' AND
011200080122     c                             %trim(§tllsoLODN) = *blanks
011300080122     C                   eval      §tllsoLODN  = 'RIVOLI'
011400080122     c                   endif
011500080122     c                   if        §tllsoPRD  = 'TO'                  AND
011600080122     c                             %trim(§tllsoLODN) = 'CASCINE VICA'
011700080122     C                   eval      §tllsoLODN  = 'RIVOLI'
011800080122     c                   endif
011900080122     c*
012000080122     c* ...x ogni evenienza se provincia nn fosse valorizzata => fisso BO
012100080122     c                   select
012200080122     c                   when      §TLLSOPRD = *blanks
012300080122     c                   eval      §TLLSOPRD = 'BO'
012400080122     c                   endsl
012500080122     c*
012600080122     c* Forzatura x San Marino e Vaticano
012700080122     c                   select
012800080122     c                   when      §TLLSOPRD  = 'RM'    and
012900080122     c                             §TLLSOCAD  = '00120'
013000080122     c                   eval      §TLLSOPRD  = 'CV'
013100080122     c                   when      §TLLSOPRD  = 'RN'    and
013200080122     c                             §TLLSOCAD >= '47890' and
013300080122     c                             §TLLSOCAD <= '47899'
013400080122     c                   eval      §TLLSOPRD  = 'SM'
013500080122     c                   endsl
013600070907     c*
013700070907     c* Forzatrura speciale x lettere accentate
013800070907     c* - A' => À
013900080630     c                   dow       %scan(aapma:§TLLSOIND) > *zeros
014000070907     c                   eval      §TLLSOIND  = %replace(aacma:§TLLSOIND:
014100070907     c                                             %scan(aapma:§TLLSOIND))
014200080630     c                   enddo
014300080630     c                   dow       %scan(aapma:§TLLSOLOD) > *zeros
014400070907     c                   eval      §TLLSOLOD  = %replace(aacma:§TLLSOLOD:
014500070907     c                                             %scan(aapma:§TLLSOLOD))
014600080630     c                   enddo
014700080630     c                   dow       %scan(aapma:§TLLSOLODN) > *zeros
014800070907     c                   eval      §TLLSOLODN = %replace(aacma:§TLLSOLODN:
014900070907     c                                             %scan(aapma:§TLLSOLODN))
015000080630     c                   enddo
015100070907     c* - E' => È
015200080630     c                   dow       %scan(eapma:§TLLSOIND) > *zeros
015300070907     c                   eval      §TLLSOIND  = %replace(eacma:§TLLSOIND:
015400070907     c                                             %scan(eapma:§TLLSOIND))
015500080630     c                   enddo
015600080630     c                   dow       %scan(eapma:§TLLSOLOD) > *zeros
015700070907     c                   eval      §TLLSOLOD  = %replace(eacma:§TLLSOLOD:
015800070907     c                                             %scan(eapma:§TLLSOLOD))
015900080630     c                   enddo
016000080630     c                   dow       %scan(eapma:§TLLSOLODN) > *zeros
016100070907     c                   eval      §TLLSOLODN = %replace(eacma:§TLLSOLODN:
016200070907     c                                             %scan(eapma:§TLLSOLODN))
016300080630     c                   enddo
016400070907     c* - I' => Ì
016500080630     c                   dow       %scan(iapma:§TLLSOIND) > *zeros
016600070907     c                   eval      §TLLSOIND  = %replace(iacma:§TLLSOIND:
016700070907     c                                             %scan(iapma:§TLLSOIND))
016800080630     c                   enddo
016900080630     c                   dow       %scan(iapma:§TLLSOLOD) > *zeros
017000070907     c                   eval      §TLLSOLOD  = %replace(iacma:§TLLSOLOD:
017100070907     c                                             %scan(iapma:§TLLSOLOD))
017200080630     c                   enddo
017300080630     c                   dow       %scan(iapma:§TLLSOLODN) > *zeros
017400070907     c                   eval      §TLLSOLODN = %replace(iacma:§TLLSOLODN:
017500070907     c                                             %scan(iapma:§TLLSOLODN))
017600080630     c                   enddo
017700070907     c* - O' => Ò
017800080630     c                   dow       %scan(oapma:§TLLSOIND) > *zeros
017900070907     c                   eval      §TLLSOIND  = %replace(oacma:§TLLSOIND:
018000070907     c                                             %scan(oapma:§TLLSOIND))
018100080630     c                   enddo
018200080630     c                   dow       %scan(oapma:§TLLSOLOD) > *zeros
018300070907     c                   eval      §TLLSOLOD  = %replace(oacma:§TLLSOLOD:
018400070907     c                                             %scan(oapma:§TLLSOLOD))
018500080630     c                   enddo
018600080630     c                   dow       %scan(oapma:§TLLSOLODN) > *zeros
018700070907     c                   eval      §TLLSOLODN = %replace(oacma:§TLLSOLODN:
018800070907     c                                             %scan(oapma:§TLLSOLODN))
018900080630     c                   enddo
019000070907     c* - U' => Ù
019100080630     c                   dow       %scan(uapma:§TLLSOIND) > *zeros
019200070907     c                   eval      §TLLSOIND  = %replace(uacma:§TLLSOIND:
019300070907     c                                             %scan(uapma:§TLLSOIND))
019400080630     c                   enddo
019500080630     c                   dow       %scan(uapma:§TLLSOLOD) > *zeros
019600070907     c                   eval      §TLLSOLOD  = %replace(uacma:§TLLSOLOD:
019700070907     c                                             %scan(uapma:§TLLSOLOD))
019800080630     c                   enddo
019900080630     c                   dow       %scan(uapma:§TLLSOLODN) > *zeros
020000070907     c                   eval      §TLLSOLODN = %replace(uacma:§TLLSOLODN:
020100070907     c                                             %scan(uapma:§TLLSOLODN))
020200080630     c                   enddo
020300070907     c* - a' => à
020400080630     c                   dow       %scan(aapmi:§TLLSOIND) > *zeros
020500070907     c                   eval      §TLLSOIND  = %replace(aacmi:§TLLSOIND:
020600070907     c                                             %scan(aapmi:§TLLSOIND))
020700080630     c                   enddo
020800080630     c                   dow       %scan(aapmi:§TLLSOLOD) > *zeros
020900070907     c                   eval      §TLLSOLOD  = %replace(aacmi:§TLLSOLOD:
021000070907     c                                             %scan(aapmi:§TLLSOLOD))
021100080630     c                   enddo
021200080630     c                   dow       %scan(aapmi:§TLLSOLODN) > *zeros
021300070907     c                   eval      §TLLSOLODN = %replace(aacmi:§TLLSOLODN:
021400070907     c                                             %scan(aapmi:§TLLSOLODN))
021500080630     c                   enddo
021600070907     c* - e' => è
021700080630     c                   dow       %scan(eapmi:§TLLSOIND) > *zeros
021800070907     c                   eval      §TLLSOIND  = %replace(eacmi:§TLLSOIND:
021900070907     c                                             %scan(eapmi:§TLLSOIND))
022000080630     c                   enddo
022100080630     c                   dow       %scan(eapmi:§TLLSOLOD) > *zeros
022200070907     c                   eval      §TLLSOLOD  = %replace(eacmi:§TLLSOLOD:
022300070907     c                                             %scan(eapmi:§TLLSOLOD))
022400080630     c                   enddo
022500080630     c                   dow       %scan(eapmi:§TLLSOLODN) > *zeros
022600070907     c                   eval      §TLLSOLODN = %replace(eacmi:§TLLSOLODN:
022700070907     c                                             %scan(eapmi:§TLLSOLODN))
022800080630     c                   enddo
022900070907     c* - i' => ì
023000080630     c                   dow       %scan(iapmi:§TLLSOIND) > *zeros
023100070907     c                   eval      §TLLSOIND  = %replace(iacmi:§TLLSOIND:
023200070907     c                                             %scan(iapmi:§TLLSOIND))
023300080630     c                   enddo
023400080630     c                   dow       %scan(iapmi:§TLLSOLOD) > *zeros
023500070907     c                   eval      §TLLSOLOD  = %replace(iacmi:§TLLSOLOD:
023600070907     c                                             %scan(iapmi:§TLLSOLOD))
023700080630     c                   enddo
023800080630     c                   dow       %scan(iapmi:§TLLSOLODN) > *zeros
023900070907     c                   eval      §TLLSOLODN = %replace(iacmi:§TLLSOLODN:
024000070907     c                                             %scan(iapmi:§TLLSOLODN))
024100080630     c                   enddo
024200070907     c* - o' => ò
024300080630     c                   dow       %scan(oapmi:§TLLSOIND) > *zeros
024400070907     c                   eval      §TLLSOIND  = %replace(oacmi:§TLLSOIND:
024500070907     c                                             %scan(oapmi:§TLLSOIND))
024600080630     c                   enddo
024700080630     c                   dow       %scan(oapmi:§TLLSOLOD) > *zeros
024800070907     c                   eval      §TLLSOLOD  = %replace(oacmi:§TLLSOLOD:
024900070907     c                                             %scan(oapmi:§TLLSOLOD))
025000080630     c                   enddo
025100080630     c                   dow       %scan(oapmi:§TLLSOLODN) > *zeros
025200070907     c                   eval      §TLLSOLODN = %replace(oacmi:§TLLSOLODN:
025300070907     c                                             %scan(oapmi:§TLLSOLODN))
025400080630     c                   enddo
025500070907     c* - u' => ù
025600080630     c                   dow       %scan(uapmi:§TLLSOIND) > *zeros
025700070907     c                   eval      §TLLSOIND  = %replace(uacmi:§TLLSOIND:
025800070907     c                                             %scan(uapmi:§TLLSOIND))
025900080630     c                   enddo
026000080630     c                   dow       %scan(uapmi:§TLLSOLOD) > *zeros
026100070907     c                   eval      §TLLSOLOD  = %replace(uacmi:§TLLSOLOD:
026200070907     c                                             %scan(uapmi:§TLLSOLOD))
026300080630     c                   enddo
026400080630     c                   dow       %scan(uapmi:§TLLSOLODN) > *zeros
026500070907     c                   eval      §TLLSOLODN = %replace(uacmi:§TLLSOLODN:
026600070907     c                                             %scan(uapmi:§TLLSOLODN))
026700080630     c                   enddo
026800070913     c*
026900070913     c* Forzatrura speciale x strade statali
027000080630     c                   dow       %scan('s.s.':§TLLSOIND) > *zeros
027100070913     c                   eval      §TLLSOIND = %replace('ss  ':§TLLSOIND:
027200070913     c                                            %scan('s.s.':§TLLSOIND))
027300080630     c                   enddo
027400080630     c                   dow       %scan('S.S.':§TLLSOIND) > *zeros
027500070913     c                   eval      §TLLSOIND = %replace('SS  ':§TLLSOIND:
027600070913     c                                            %scan('S.S.':§TLLSOIND))
027700080630     c                   enddo
027800101129     C*
027900101129     C* Effettuo considerazioni relative alle modifiche CAP PT 10/2010
028000101129     C*
028100101129     C* ...solo le provincia interessate: MB / FM / BT / RN / TN / PR / AP
028200101129     C                   if        §TLLSOPRD = 'MB' or
028300101129     C                             §TLLSOPRD = 'FM' or
028400101129     C                             §TLLSOPRD = 'BT' or
028500101129     C                             §TLLSOPRD = 'RN' or
028600101129     C                             §TLLSOPRD = 'TN' or
028700101129     C                             §TLLSOPRD = 'PR' or
028800101129     C                             §TLLSOPRD = 'AP'
028900101129     C*
029000101129     C                   eval      NEWL_PRV = §TLLSOPRD
029100101129     C                   eval      NEWL_CAP = §TLLSOCAD
029200101129     C                   if        §TLLSOLODN <> *blanks
029300101129     C                   eval      NEWL_LOC = §TLLSOLODN
029400101129     C                   else
029500101129     C                   eval      NEWL_LOC = §TLLSOLOD
029600101129     C                   endif
029700101129     C*
029800101129     C* 1° tentativo => PRV + CAP + LOC
029900101129     C     KEYcpmod2_PCL chain     wacpmod2l
030000101129     C                   if        %found(wacpmod2l)
030100101129     C                   eval      §TLLSOPRD = OLDL_PRV
030200101129     C                   eval      §TLLSOCAD = OLDL_CAP
030300101129     C                   else
030400101129     C* 2° tentativo => PRV + CAP
030500101129     C     KEYcpmod2_PC  chain     wacpmod2l
030600101129     C                   if        %found(wacpmod2l)
030700101129     C                   eval      §TLLSOPRD = OLDL_PRV
030800101129     C                   eval      §TLLSOCAD = OLDL_CAP
030900101129     C                   else
031000101201     C* 3° tentativo => PRV (solo x nuove provincie)
031100101201     C                   if        §TLLSOPRD = 'MB' or
031200101201     C                             §TLLSOPRD = 'FM' or
031300101201     C                             §TLLSOPRD = 'BT'
031400101129     C     KEYcpmod2_P   chain     wacpmod2l
031500101129     C                   if        %found(wacpmod2l)
031600101129     C                   eval      §TLLSOPRD = OLDL_PRV
031700101129     C                   endif
031800101201     C                   endif
031900101129     C                   endif
032000101129     C                   endif
032100101129     C                   endif
032200050121     C*
032300050121     C* Inizializzazione campi e variabili d out
032400060804     C                   clear                   tivaw000
032500060804     C*
032600070913     C                   eval      wRecOut = %subst(dtllso:1:157)
032700060804     C*
032800060804     C* Effettuo determinate considerazioni x portare nella forma ASCII editata con segno
032900060804     C* le coordinale geo-stazionarie
033000060804     C                   if        §tllsoXLO < *zeros
033100060804     C                   eval      wNum11_8 = %abs(§tllsoXLO)
033200060804     C                   eval      wSegno = '-'
033300060804     C                   else
033400060804     C                   eval      wNum11_8 = §tllsoXLO
033500060804     C                   eval      wSegno = *blanks
033600060804     C                   endif
033700060804     C                   move(p)   wNum11_8      wAlfa11
033800060804     C                   eval      wRecOut = wRecOut + wSegno +
033900060804     C                                       %subst(wAlfa11:1:3) + wSepDec +
034000060804     C                                       %subst(wAlfa11:4:8)
034100060804     C*
034200060804     C                   if        §tllsoYLA < *zeros
034300060804     C                   eval      wNum11_8 = %abs(§tllsoYLA)
034400060804     C                   eval      wSegno = '-'
034500060804     C                   else
034600060804     C                   eval      wNum11_8 = §tllsoYLA
034700060804     C                   eval      wSegno = *blanks
034800060804     C                   endif
034900060804     C                   move(p)   wNum11_8      wAlfa11
035000060804     C                   eval      wRecOut = wRecOut + wSegno +
035100060804     C                                       %subst(wAlfa11:1:3) + wSepDec +
035200060804     C                                       %subst(wAlfa11:4:8)
035300070112     C*
035400070112     C* Quindi proseguo con il resto dei dati
035500070112     C                   eval      wRecOut = %subst(wRecOut:1:183) +
035600070913     C                             %subst(vgdDTA:180:%len(dtllso)-180+1)
035700060804     C*
035800060804     C* Valorizzo il buffer del file d output
035900060804     C                   eval      vawdta = wRecOut
036000930409     C*
036100050121     C                   WRITE     TIVAW000
036200050909     C                   DELETE    TIVGD000
036300111124     C*
036400111124     C                   endif
036500991027     C*
036600050909     C                   READ      TIVGDTMP
036700991027     C                   ENDdo
036800991027     C*
036900991027     C                   eval      wrkesito = '0'
037000991027     C*
037100910830     C                   ENDSR
037200060804
037300991027
037400991027      /TITLE Operazioni iniziali.
037500991027     C     *inzsr        BEGSR
037600991027     C*
037700991027     C     *ENTRY        PLIST
037800991027     C                   parm                    prmppt
037900991027     C     wrkesito      parm      wrkesito      prmesito
038000991027     C*
038100101129     C* Definizione chiave su WACPMOD2L  - Completa: PRV / CAP / LOC
038200101129     C     KEYcpmod2_PCL KLIST
038300101129     C                   KFLD                    NEWL_PRV
038400101129     C                   KFLD                    NEWL_CAP
038500101129     C                   KFLD                    NEWL_LOC
038600101129     C*
038700101129     C* Definizione chiave su WACPMOD2L  - Parziale: PRV / CAP
038800101129     C     KEYcpmod2_PC  KLIST
038900101129     C                   KFLD                    NEWL_PRV
039000101129     C                   KFLD                    NEWL_CAP
039100101129     C*
039200101129     C* Definizione chiave su WACPMOD2L  - Parziale: PRV
039300101129     C     KEYcpmod2_P   KLIST
039400101129     C                   KFLD                    NEWL_PRV
039500101129     C*
039600991027     C                   ENDSR
