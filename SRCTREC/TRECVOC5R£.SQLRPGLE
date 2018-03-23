000100010925      *PARMS DYNUSRPRF(*OWNER)
000200970526     H*------------------------------------------------------------------------*
000300000920     H DECEDIT('0,') DATEDIT(*DMY.)
000400970526     F*------------------------------------------------------------------------*
000500970526     F* DATA BASE
000600970526     F*------------------------------------------------------------------------*
000700140604     Feccdc00f  o    e             disk
000800140605     Ftabel00f  if   e           k disk
000900140605     Fecect02l  if   e           k disk
001000140604     Fecc2v30c  if   e           k disk
001100140604     Ftitas30c  if   e           k disk
001200140605     FFIAR531C  IF   E           K DISK
001300140605     D ds2k          E DS
001400140605     D DAR5TRS       E DS
001500140604     D ececdds       E DS                  extname(ececd00f)
001600140604     D ecevtds       E DS                  extname(ecevt00f)
001700140604     D eccevds       E DS                  extname(eccev00f)
001800140604     D KPJBA         E DS
001900110420     d param           ds
002000140604     d aa                      1      4s 0
002100140604     d mmi                     5      6s 0
002200140604     d mmf                     7      8s 0
002300140604     d nbc                     9     13s 0
002400140605     d voc             s              3  0 DIM(999) inz
002500140605     d ric             s              3    DIM(999) inz
002600140605     D fili            DS                  OCCURS(999) INZ
002700140605     d imp                           13  5 DIM(999) inz
002800140605     D filc            DS                  OCCURS(999) INZ
002900140605     d ncl                            7  0 DIM(999) inz
003000140605     D fils            DS                  OCCURS(999) INZ
003100140605     d nsp                            7  0 DIM(999) inz
003200140605     D filv            DS                  OCCURS(999) INZ
003300140605     d vlf                            9  3 DIM(999) inz
003400140605     D filp            DS                  OCCURS(999) INZ
003500140605     d pkf                            9  1 DIM(999) inz
003600140604     d anno            s              4  0
003700140604     d mese            s              2  0
003800140604     d com5            s              5
003900140604     d $finearb        s               n
004000140604     d pkfvoc          s                   like(cdcpkf)
004100140604     d vlfvoc          s                   like(cdcvlf)
004200140604     c* --------------------------------------------------------------------
004300140604     c*
004400050502     C/EXEC SQL
004500140604     C+ DECLARE C1 CURSOR FOR SELECT eccev00f.* FROM eccev00f WHERE
004600140604     C+ cevaas = :aa and cevmes between :mmi and :mmf ORDER BY cevaas,
004700140604     C+ cevmes
004800050502     C/END-EXEC
004900110420     c*
005000050502     C/EXEC SQL
005100050502     C+ OPEN c1
005200050502     C/END-EXEC
005300110420     c*
005400050502
005500050502     C                   DO        *HIVAL
005600050502
005700050502     C/EXEC SQL
005800140604     C+ FETCH NEXT FROM c1 INTO :eccevds
005900050502     C/END-EXEC
006000050502
006100110420     C                   IF        SQLCod = 100 or sqlcod < 0
006200050502     C                   LEAVE
006300050502     C                   ENDIF
006400140604     c* a rottura di anno e mese scarico la schiera
006500140604     c                   if        (anno <> 0 and mese <> 0) and
006600140604     c                             (cevaas <> anno or cevmes <> mese)
006700140604     c                   exsr      scrivi
006800140604     c                   end
006900140604     c                   eval      anno = cevaas
007000140604     c                   eval      mese = cevmes
007100140604     c* reperisco le informazioni da titas
007200140604     c     ktas          chain     titas30c
007300140604     c                   if        %found(titas30c)
007400140605     c                   exsr      srpesvol
007500140605     c                   clear                   dar5trs
007600140605     C     KEYAR5        CHAIN     FIAR531C
007700140605     c                   if        %found(fiar531c)
007800140605     C                   MOVEL     AR5UNI        DAR5TRS
007900140605     c                   end
008000140605     c                   EXSR      conta
008100140605     c                   end
008200050502
008300050502     C                   ENDDO
008400050502
008500050502     C/EXEC SQL
008600050502     C+ CLOSE c1
008700050502     C/END-EXEC
008800140604
008900140604     c                   if        anno <> 0 and mese <> 0
009000140604     c                   exsr      scrivi
009100140604     c                   end
009200140604     C*--------------------------------------------------------------- ---------
009300140605     C     srpesvol      BEGSR
009400140604     c* peso
009500140604IF  2C     tasPKC        IFGT      *ZEROS
009600140604     C     tasncl        ANDEQ     tasNCP
009700140604     C                   z-add     tasPKC        Pkfvoc
009800140604X   2C                   ELSE
009900140604IF  3C     tasPKF        IFGT      tasPKC
010000140604     C                   z-add     tasPKF        pkfvoc
010100140604X   3C                   ELSE
010200140604     C                   z-add     tasPKC        Pkfvoc
010300140604E   3C                   ENDIF
010400140604E   2C                   ENDIF
010500140604     c* volume
010600140604IF  1C     tasFVF        IFEQ      'T'
010700140604     C                   z-add     tasVLF        vlfvoc
010800140604X   1C                   ELSE
010900140604IF  2C     tasVLC        IFGT      *ZEROS
011000140604     C     tasNCL        ANDEQ     tasNCR
011100140604     C                   z-add     tasVLC        vlfvoc
011200140604X   2C                   ELSE
011300140604IF  3C     tasVLF        IFGT      tasVLC
011400140604     C                   z-add     tasVLF        vlfvoc
011500140604X   3C                   ELSE
011600140604     C                   z-add     tasVLC        vlfvoc
011700140604E   3C                   ENDIF
011800140604E   2C                   ENDIF
011900140604E   1C                   ENDIF
012000140605     c                   endsr
012100140605     C*--------------------------------------------------------------- ---------
012200140605     C     srric         BEGSR
012300140604     c* cdc
012400140604     c                   select
012500140604     C     $ric          WHENEQ    'LA'
012600140605     C                   MOVE      tasLNA        $FIL              3 0
012700140604     C     $RIC          WHENEQ    'LP'
012800140604     C                   MOVE      tasLNP        $FIL
012900140604     C     $RIC          WHENEQ    'TA'
013000140604     C                   MOVE      tasTFA        $FIL
013100140604     C     $RIC          WHENEQ    'TP'
013200140604     C                   MOVE      tastfp        $FIL
013300140605     C     $RIC          WHENEQ    'T1'
013400140605     C                   MOVE      §AR5flp       $FIL
013500140605     C     $RIC          WHENEQ    'T2'
013600140605     C                   MOVE      §AR5fl2       $FIL
013700140605     C     $RIC          WHENEQ    'T3'
013800140605     C                   MOVE      §AR5fl3       $FIL
013900140605     C     $RIC          WHENEQ    'T4'
014000140605     C                   MOVE      §AR5fl4       $FIL
014100140605     C     $RIC          WHENEQ    'T5'
014200140605     C                   MOVE      §AR5fl5       $FIL
014300140604     c                   other
014400140604     C                   MOVE      $ric          $FIL
014500140604E   4C                   ENDSL
014600140605     c     $fil          occur     fili
014700140605     c     $fil          occur     filc
014800140605     c     $fil          occur     fils
014900140605     c     $fil          occur     filv
015000140605     c     $fil          occur     filp
015100140604     c
015200140604     c                   endsr
015300140604     C*--------------------------------------------------------------- ---------
015400140604     c     scrivi        begsr
015500140605DO  1C     1             DO        999           f                 4 0
015600140605     C     f             OCCUR     Fili
015700140605     C     f             OCCUR     Filc
015800140605     C     f             OCCUR     Fils
015900140605     C     f             OCCUR     Filp
016000140605     C     f             OCCUR     Filv
016100140604     c                   do        999           i                 4 0
016200140604     c     voc(i)        ifne      0
016300140605     c     imp(i)        andne     0
016400140604     c                   eval      cdcaas = anno
016500140604     c                   eval      cdcmes = mese
016600140604     c                   eval      cdcvoc = voc(i)
016700140604     c                   eval      cdcimp = imp(i)
016800140605     c                   eval      cdcfil = f
016900140604     c                   eval      cdcnsp = nsp(i)
017000140604     c                   eval      cdcncl = ncl(i)
017100140604     c                   eval      cdcvlf = vlf(i)
017200140604     c                   eval      cdcpkf = pkf(i)
017300140604     c                   write     eccdc000
017400140604     c                   end
017500140604     c                   enddo
017600140605     c                   clear                   imp
017700140605     c                   clear                   nsp
017800140605     c                   clear                   ncl
017900140605     c                   clear                   vlf
018000140605     c                   clear                   pkf
018100140605     c                   enddo
018200140604     c                   endsr
018300140605     c***********************************************************
018400140605     C     conta         BEGSR
018500140605     c                   if        cevc01 = 0
018600140605     c                   leavesr
018700140605     c                   end
018800140605     c                   if        voc(cevc01) <> 0
018900140605     c                   movel     ric(cevc01)   $ric              3
019000140605     c                   exsr      srric
019100140605     c                   add       cevi01        imp(cevc01)
019200140605     c                   add       1             nsp(cevc01)
019300140605     c                   add       tasncl        ncl(cevc01)
019400140605     C                   add       Pkfvoc        pkf(cevc01)
019500140605     C                   add       vlfvoc        vlf(cevc01)
019600140605     c                   end
019700140605     c                   if        cevc02 = 0
019800140605     c                   leavesr
019900140605     c                   end
020000140605     c                   if        voc(cevc02) <> 0
020100140605     c                   movel     ric(cevc02)   $ric
020200140605     c                   exsr      srric
020300140605     c                   add       cevi02        imp(cevc02)
020400140605     c                   add       1             nsp(cevc02)
020500140605     c                   add       tasncl        ncl(cevc02)
020600140605     C                   add       Pkfvoc        pkf(cevc02)
020700140605     C                   add       vlfvoc        vlf(cevc02)
020800140605     c                   end
020900140605     c                   if        cevc03 = 0
021000140605     c                   leavesr
021100140605     c                   end
021200140605     c                   if        voc(cevc03) <> 0
021300140605     c                   movel     ric(cevc03)   $ric
021400140605     c                   exsr      srric
021500140605     c                   add       cevi03        imp(cevc03)
021600140605     c                   add       1             nsp(cevc03)
021700140605     c                   add       tasncl        ncl(cevc03)
021800140605     C                   add       Pkfvoc        pkf(cevc03)
021900140605     C                   add       vlfvoc        vlf(cevc03)
022000140605     c                   end
022100140605     c                   if        cevc04 = 0
022200140605     c                   leavesr
022300140605     c                   end
022400140605     c                   if        voc(cevc04) <> 0
022500140605     c                   movel     ric(cevc04)   $ric
022600140605     c                   exsr      srric
022700140605     c                   add       cevi04        imp(cevc04)
022800140605     c                   add       1             nsp(cevc04)
022900140605     c                   add       tasncl        ncl(cevc04)
023000140605     C                   add       Pkfvoc        pkf(cevc04)
023100140605     C                   add       vlfvoc        vlf(cevc04)
023200140605     c                   end
023300140605     c                   if        cevc05 = 0
023400140605     c                   leavesr
023500140605     c                   end
023600140605     c                   if        voc(cevc05) <> 0
023700140605     c                   movel     ric(cevc05)   $ric
023800140605     c                   exsr      srric
023900140605     c                   add       cevi05        imp(cevc05)
024000140605     c                   add       1             nsp(cevc05)
024100140605     c                   add       tasncl        ncl(cevc05)
024200140605     C                   add       Pkfvoc        pkf(cevc05)
024300140605     C                   add       vlfvoc        vlf(cevc05)
024400140605     c                   end
024500140605     c                   if        cevc06 = 0
024600140605     c                   leavesr
024700140605     c                   end
024800140605     c                   if        voc(cevc06) <> 0
024900140605     c                   movel     ric(cevc06)   $ric
025000140605     c                   exsr      srric
025100140605     c                   add       cevi06        imp(cevc06)
025200140605     c                   add       1             nsp(cevc06)
025300140605     c                   add       tasncl        ncl(cevc06)
025400140605     C                   add       Pkfvoc        pkf(cevc06)
025500140605     C                   add       vlfvoc        vlf(cevc06)
025600140605     c                   end
025700140605     c                   if        cevc07 = 0
025800140605     c                   leavesr
025900140605     c                   end
026000140605     c                   if        voc(cevc07) <> 0
026100140605     c                   movel     ric(cevc07)   $ric
026200140605     c                   exsr      srric
026300140605     c                   add       cevi07        imp(cevc07)
026400140605     c                   add       1             nsp(cevc07)
026500140605     c                   add       tasncl        ncl(cevc07)
026600140605     C                   add       Pkfvoc        pkf(cevc07)
026700140605     C                   add       vlfvoc        vlf(cevc07)
026800140605     c                   end
026900140605     c                   if        cevc08 = 0
027000140605     c                   leavesr
027100140605     c                   end
027200140605     c                   if        voc(cevc08)<> 0
027300140605     c                   movel     ric(cevc08)   $ric
027400140605     c                   exsr      srric
027500140605     c                   add       cevi08        imp(cevc08)
027600140605     c                   add       1             nsp(cevc08)
027700140605     c                   add       tasncl        ncl(cevc08)
027800140605     C                   add       Pkfvoc        pkf(cevc08)
027900140605     C                   add       vlfvoc        vlf(cevc08)
028000140605     c                   end
028100140605     c                   if        cevc09 = 0
028200140605     c                   leavesr
028300140605     c                   end
028400140605     c                   if        voc(cevc09) <> 0
028500140605     c                   movel     ric(cevc09)   $ric
028600140605     c                   exsr      srric
028700140605     c                   add       cevi09        imp(cevc09)
028800140605     c                   add       1             nsp(cevc09)
028900140605     c                   add       tasncl        ncl(cevc09)
029000140605     C                   add       Pkfvoc        pkf(cevc09)
029100140605     C                   add       vlfvoc        vlf(cevc09)
029200140605     c                   end
029300140605     c                   if        cevc10 = 0
029400140605     c                   leavesr
029500140605     c                   end
029600140605     c                   if        voc(cevc10) <> 0
029700140605     c                   movel     ric(cevc10)   $ric
029800140605     c                   exsr      srric
029900140605     c                   add       cevi10        imp(cevc10)
030000140605     c                   add       1             nsp(cevc10)
030100140605     c                   add       tasncl        ncl(cevc10)
030200140605     C                   add       Pkfvoc        pkf(cevc10)
030300140605     C                   add       vlfvoc        vlf(cevc10)
030400140605     c                   end
030500140605     c                   if        cevc11 = 0
030600140605     c                   leavesr
030700140605     c                   end
030800140605     c                   if        voc(cevc11) <> 0
030900140605     c                   movel     ric(cevc11)   $ric
031000140605     c                   exsr      srric
031100140605     c                   add       cevi11        imp(cevc11)
031200140605     c                   add       1             nsp(cevc11)
031300140605     c                   add       tasncl        ncl(cevc11)
031400140605     C                   add       Pkfvoc        pkf(cevc11)
031500140605     C                   add       vlfvoc        vlf(cevc11)
031600140605     c                   end
031700140605     c                   if        cevc12 = 0
031800140605     c                   leavesr
031900140605     c                   end
032000140605     c                   if        voc(cevc12)<> 0
032100140605     c                   movel     ric(cevc12)   $ric
032200140605     c                   exsr      srric
032300140605     c                   add       cevi12        imp(cevc12)
032400140605     c                   add       1             nsp(cevc12)
032500140605     c                   add       tasncl        ncl(cevc12)
032600140605     C                   add       Pkfvoc        pkf(cevc12)
032700140605     C                   add       vlfvoc        vlf(cevc12)
032800140605     c                   end
032900140605     c                   if        cevc13 = 0
033000140605     c                   leavesr
033100140605     c                   end
033200140605     c                   if        voc(cevc13) <> 0
033300140605     c                   movel     ric(cevc13)   $ric
033400140605     c                   exsr      srric
033500140605     c                   add       cevi13        imp(cevc13)
033600140605     c                   add       1             nsp(cevc13)
033700140605     c                   add       tasncl        ncl(cevc13)
033800140605     C                   add       Pkfvoc        pkf(cevc13)
033900140605     C                   add       vlfvoc        vlf(cevc13)
034000140605     c                   end
034100140605     c                   if        cevc14 = 0
034200140605     c                   leavesr
034300140605     c                   end
034400140605     c                   if        voc(cevc14)<> 0
034500140605     c                   movel     ric(cevc14)   $ric
034600140605     c                   exsr      srric
034700140605     c                   add       cevi14        imp(cevc14)
034800140605     c                   add       1             nsp(cevc14)
034900140605     c                   add       tasncl        ncl(cevc14)
035000140605     C                   add       Pkfvoc        pkf(cevc14)
035100140605     C                   add       vlfvoc        vlf(cevc14)
035200140605     c                   end
035300140605     c                   if        cevc15 = 0
035400140605     c                   leavesr
035500140605     c                   end
035600140605     c                   if        voc(cevc15) <> 0
035700140605     c                   movel     ric(cevc15)   $ric
035800140605     c                   exsr      srric
035900140605     c                   add       cevi15        imp(cevc15)
036000140605     c                   add       1             nsp(cevc15)
036100140605     c                   add       tasncl        ncl(cevc15)
036200140605     C                   add       Pkfvoc        pkf(cevc15)
036300140605     C                   add       vlfvoc        vlf(cevc15)
036400140605     c                   end
036500140605     c                   if        cevc16 = 0
036600140605     c                   leavesr
036700140605     c                   end
036800140605     c                   if        voc(cevc16) <> 0
036900140605     c                   movel     ric(cevc16)   $ric
037000140605     c                   exsr      srric
037100140605     c                   add       cevi16        imp(cevc16)
037200140605     c                   add       1             nsp(cevc16)
037300140605     c                   add       tasncl        ncl(cevc16)
037400140605     C                   add       Pkfvoc        pkf(cevc16)
037500140605     C                   add       vlfvoc        vlf(cevc16)
037600140605     c                   end
037700140605     c                   if        cevc17 = 0
037800140605     c                   leavesr
037900140605     c                   end
038000140605     c                   if        voc(cevc17)<> 0
038100140605     c                   movel     ric(cevc17)   $ric
038200140605     c                   exsr      srric
038300140605     c                   add       cevi17        imp(cevc17)
038400140605     c                   add       1             nsp(cevc17)
038500140605     c                   add       tasncl        ncl(cevc17)
038600140605     C                   add       Pkfvoc        pkf(cevc17)
038700140605     C                   add       vlfvoc        vlf(cevc17)
038800140605     c                   end
038900140605     c                   if        cevc18 = 0
039000140605     c                   leavesr
039100140605     c                   end
039200140605     c                   if        voc(cevc18) <> 0
039300140605     c                   movel     ric(cevc18)   $ric
039400140605     c                   exsr      srric
039500140605     c                   add       cevi18        imp(cevc18)
039600140605     c                   add       1             nsp(cevc18)
039700140605     c                   add       tasncl        ncl(cevc18)
039800140605     C                   add       Pkfvoc        pkf(cevc18)
039900140605     C                   add       vlfvoc        vlf(cevc18)
040000140605     c                   end
040100140605     c                   if        cevc19 = 0
040200140605     c                   leavesr
040300140605     c                   end
040400140605     c                   if        voc(cevc19) <> 0
040500140605     c                   movel     ric(cevc19)   $ric
040600140605     c                   exsr      srric
040700140605     c                   add       cevi19        imp(cevc19)
040800140605     c                   add       1             nsp(cevc19)
040900140605     c                   add       tasncl        ncl(cevc19)
041000140605     C                   add       Pkfvoc        pkf(cevc19)
041100140605     C                   add       vlfvoc        vlf(cevc19)
041200140605     c                   end
041300140605     c                   if        cevc20 = 0
041400140605     c                   leavesr
041500140605     c                   end
041600140605     c                   if        voc(cevc20)<> 0
041700140605     c                   movel     ric(cevc20)   $ric
041800140605     c                   exsr      srric
041900140605     c                   add       cevi20        imp(cevc20)
042000140605     c                   add       1             nsp(cevc20)
042100140605     c                   add       tasncl        ncl(cevc20)
042200140605     C                   add       Pkfvoc        pkf(cevc20)
042300140605     C                   add       vlfvoc        vlf(cevc20)
042400140605     c                   end
042500140605     c                   if        cevc21 = 0
042600140605     c                   leavesr
042700140605     c                   end
042800140605     c                   if        voc(cevc21)<> 0
042900140605     c                   movel     ric(cevc21)   $ric
043000140605     c                   exsr      srric
043100140605     c                   add       cevi21        imp(cevc21)
043200140605     c                   add       1             nsp(cevc21)
043300140605     c                   add       tasncl        ncl(cevc21)
043400140605     C                   add       Pkfvoc        pkf(cevc21)
043500140605     C                   add       vlfvoc        vlf(cevc21)
043600140605     c                   end
043700140605     c                   if        cevc22 = 0
043800140605     c                   leavesr
043900140605     c                   end
044000140605     c                   if        voc(cevc22) <> 0
044100140605     c                   movel     ric(cevc22)   $ric
044200140605     c                   exsr      srric
044300140605     c                   add       cevi22        imp(cevc22)
044400140605     c                   add       1             nsp(cevc22)
044500140605     c                   add       tasncl        ncl(cevc22)
044600140605     C                   add       Pkfvoc        pkf(cevc22)
044700140605     C                   add       vlfvoc        vlf(cevc22)
044800140605     c                   end
044900140605     c                   if        cevc23 = 0
045000140605     c                   leavesr
045100140605     c                   end
045200140605     c                   if        voc(cevc23) <> 0
045300140605     c                   movel     ric(cevc23)   $ric
045400140605     c                   exsr      srric
045500140605     c                   add       cevi23        imp(cevc23)
045600140605     c                   add       1             nsp(cevc23)
045700140605     c                   add       tasncl        ncl(cevc23)
045800140605     C                   add       Pkfvoc        pkf(cevc23)
045900140605     C                   add       vlfvoc        vlf(cevc23)
046000140605     c                   end
046100140605     c                   if        cevc24 = 0
046200140605     c                   leavesr
046300140605     c                   end
046400140605     c                   if        voc(cevc24) <> 0
046500140605     c                   movel     ric(cevc24)   $ric
046600140605     c                   exsr      srric
046700140605     c                   add       cevi24        imp(cevc24)
046800140605     c                   add       1             nsp(cevc24)
046900140605     c                   add       tasncl        ncl(cevc24)
047000140605     C                   add       Pkfvoc        pkf(cevc24)
047100140605     C                   add       vlfvoc        vlf(cevc24)
047200140605     c                   end
047300140605     c                   if        cevc25 = 0
047400140605     c                   leavesr
047500140605     c                   end
047600140605     c                   if        voc(cevc25)<> 0
047700140605     c                   movel     ric(cevc25)   $ric
047800140605     c                   exsr      srric
047900140605     c                   add       cevi25        imp(cevc25)
048000140605     c                   add       1             nsp(cevc25)
048100140605     c                   add       tasncl        ncl(cevc25)
048200140605     C                   add       Pkfvoc        pkf(cevc25)
048300140605     C                   add       vlfvoc        vlf(cevc25)
048400140605     c                   end
048500140605     c                   if        cevc26 = 0
048600140605     c                   leavesr
048700140605     c                   end
048800140605     c                   if        voc(cevc26)  <> 0
048900140605     c                   movel     ric(cevc26)   $ric
049000140605     c                   exsr      srric
049100140605     c                   add       cevi26        imp(cevc26)
049200140605     c                   add       1             nsp(cevc26)
049300140605     c                   add       tasncl        ncl(cevc26)
049400140605     C                   add       Pkfvoc        pkf(cevc26)
049500140605     C                   add       vlfvoc        vlf(cevc26)
049600140605     c                   end
049700140605     c                   if        cevc27 = 0
049800140605     c                   leavesr
049900140605     c                   end
050000140605     c                   if        voc(cevc27)<> 0
050100140605     c                   movel     ric(cevc27)   $ric
050200140605     c                   exsr      srric
050300140605     c                   add       cevi27        imp(cevc27)
050400140605     c                   add       1             nsp(cevc27)
050500140605     c                   add       tasncl        ncl(cevc27)
050600140605     C                   add       Pkfvoc        pkf(cevc27)
050700140605     C                   add       vlfvoc        vlf(cevc27)
050800140605     c                   end
050900140605     c                   if        cevc28 = 0
051000140605     c                   leavesr
051100140605     c                   end
051200140605     c                   if        voc(cevc28) <> 0
051300140605     c                   movel     ric(cevc28)   $ric
051400140605     c                   exsr      srric
051500140605     c                   add       cevi28        imp(cevc28)
051600140605     c                   add       1             nsp(cevc28)
051700140605     c                   add       tasncl        ncl(cevc28)
051800140605     C                   add       Pkfvoc        pkf(cevc28)
051900140605     C                   add       vlfvoc        vlf(cevc28)
052000140605     c                   end
052100140605     c                   if        cevc29 = 0
052200140605     c                   leavesr
052300140605     c                   end
052400140605     c                   if        voc(cevc29)<> 0
052500140605     c                   movel     ric(cevc29)   $ric
052600140605     c                   exsr      srric
052700140605     c                   add       cevi29        imp(cevc29)
052800140605     c                   add       1             nsp(cevc29)
052900140605     c                   add       tasncl        ncl(cevc29)
053000140605     C                   add       Pkfvoc        pkf(cevc29)
053100140605     C                   add       vlfvoc        vlf(cevc29)
053200140605     c                   end
053300140605     c                   if        cevc30 = 0
053400140605     c                   leavesr
053500140605     c                   end
053600140605     c                   if        voc(cevc30)  <> 0
053700140605     c                   movel     ric(cevc30)   $ric
053800140605     c                   exsr      srric
053900140605     c                   add       cevi30        imp(cevc30)
054000140605     c                   add       1             nsp(cevc30)
054100140605     c                   add       tasncl        ncl(cevc30)
054200140605     C                   add       Pkfvoc        pkf(cevc30)
054300140605     C                   add       vlfvoc        vlf(cevc30)
054400140605     c                   end
054500140605     c                   if        cevc31 = 0
054600140605     c                   leavesr
054700140605     c                   end
054800140605     c                   if        voc(cevc31)  <> 0
054900140605     c                   movel     ric(cevc31)   $ric
055000140605     c                   exsr      srric
055100140605     c                   add       cevi31        imp(cevc31)
055200140605     c                   add       1             nsp(cevc31)
055300140605     c                   add       tasncl        ncl(cevc31)
055400140605     C                   add       Pkfvoc        pkf(cevc31)
055500140605     C                   add       vlfvoc        vlf(cevc31)
055600140605     c                   end
055700140605     c                   if        cevc32 = 0
055800140605     c                   leavesr
055900140605     c                   end
056000140605     c                   if        voc(cevc32) <> 0
056100140605     c                   movel     ric(cevc32)   $ric
056200140605     c                   exsr      srric
056300140605     c                   add       cevi32        imp(cevc32)
056400140605     c                   add       1             nsp(cevc32)
056500140605     c                   add       tasncl        ncl(cevc32)
056600140605     C                   add       Pkfvoc        pkf(cevc32)
056700140605     C                   add       vlfvoc        vlf(cevc32)
056800140605     c                   end
056900140605     c                   if        cevc33 = 0
057000140605     c                   leavesr
057100140605     c                   end
057200140605     c                   if        voc(cevc33)<> 0
057300140605     c                   movel     ric(cevc33)   $ric
057400140605     c                   exsr      srric
057500140605     c                   add       cevi33        imp(cevc33)
057600140605     c                   add       1             nsp(cevc33)
057700140605     c                   add       tasncl        ncl(cevc33)
057800140605     C                   add       Pkfvoc        pkf(cevc33)
057900140605     C                   add       vlfvoc        vlf(cevc33)
058000140605     c                   end
058100140605     c                   if        cevc34 = 0
058200140605     c                   leavesr
058300140605     c                   end
058400140605     c                   if        voc(cevc34)<> 0
058500140605     c                   movel     ric(cevc34)   $ric
058600140605     c                   exsr      srric
058700140605     c                   add       cevi34        imp(cevc34)
058800140605     c                   add       1             nsp(cevc34)
058900140605     c                   add       tasncl        ncl(cevc34)
059000140605     C                   add       Pkfvoc        pkf(cevc34)
059100140605     C                   add       vlfvoc        vlf(cevc34)
059200140605     c                   end
059300140605     c                   if        cevc35 = 0
059400140605     c                   leavesr
059500140605     c                   end
059600140605     c                   if        voc(cevc35) <> 0
059700140605     c                   movel     ric(cevc35)   $ric
059800140605     c                   exsr      srric
059900140605     c                   add       cevi35        imp(cevc35)
060000140605     c                   add       1             nsp(cevc35)
060100140605     c                   add       tasncl        ncl(cevc35)
060200140605     C                   add       Pkfvoc        pkf(cevc35)
060300140605     C                   add       vlfvoc        vlf(cevc35)
060400140605     c                   end
060500140605     c                   if        cevc36 = 0
060600140605     c                   leavesr
060700140605     c                   end
060800140605     c                   if        voc(cevc36) <> 0
060900140605     c                   movel     ric(cevc36)   $ric
061000140605     c                   exsr      srric
061100140605     c                   add       cevi36        imp(cevc36)
061200140605     c                   add       1             nsp(cevc36)
061300140605     c                   add       tasncl        ncl(cevc36)
061400140605     C                   add       Pkfvoc        pkf(cevc36)
061500140605     C                   add       vlfvoc        vlf(cevc36)
061600140605     c                   end
061700140605     c                   if        cevc37 = 0
061800140605     c                   leavesr
061900140605     c                   end
062000140605     c                   if        voc(cevc37)<> 0
062100140605     c                   movel     ric(cevc37)   $ric
062200140605     c                   exsr      srric
062300140605     c                   add       cevi37        imp(cevc37)
062400140605     c                   add       1             nsp(cevc37)
062500140605     c                   add       tasncl        ncl(cevc37)
062600140605     C                   add       Pkfvoc        pkf(cevc37)
062700140605     C                   add       vlfvoc        vlf(cevc37)
062800140605     c                   end
062900140605     c                   if        cevc38 = 0
063000140605     c                   leavesr
063100140605     c                   end
063200140605     c                   if        voc(cevc38)<> 0
063300140605     c                   movel     ric(cevc38)   $ric
063400140605     c                   exsr      srric
063500140605     c                   add       cevi38        imp(cevc38)
063600140605     c                   add       1             nsp(cevc38)
063700140605     c                   add       tasncl        ncl(cevc38)
063800140605     C                   add       Pkfvoc        pkf(cevc38)
063900140605     C                   add       vlfvoc        vlf(cevc38)
064000140605     c                   end
064100140605     c                   if        cevc39 = 0
064200140605     c                   leavesr
064300140605     c                   end
064400140605     c                   if        voc(cevc39)<> 0
064500140605     c                   movel     ric(cevc39)   $ric
064600140605     c                   exsr      srric
064700140605     c                   add       cevi39        imp(cevc39)
064800140605     c                   add       1             nsp(cevc39)
064900140605     c                   add       tasncl        ncl(cevc39)
065000140605     C                   add       Pkfvoc        pkf(cevc39)
065100140605     C                   add       vlfvoc        vlf(cevc39)
065200140605     c                   end
065300140605     c                   if        cevc40 = 0
065400140605     c                   leavesr
065500140605     c                   end
065600140605     c                   if        voc(cevc40) <> 0
065700140605     c                   movel     ric(cevc40)   $ric
065800140605     c                   exsr      srric
065900140605     c                   add       cevi40        imp(cevc40)
066000140605     c                   add       1             nsp(cevc40)
066100140605     c                   add       tasncl        ncl(cevc40)
066200140605     C                   add       Pkfvoc        pkf(cevc40)
066300140605     C                   add       vlfvoc        vlf(cevc40)
066400140605     c                   end
066500140605     c                   if        cevc41 = 0
066600140605     c                   leavesr
066700140605     c                   end
066800140605     c                   if        voc(cevc41) <> 0
066900140605     c                   movel     ric(cevc41)   $ric
067000140605     c                   exsr      srric
067100140605     c                   add       cevi41        imp(cevc41)
067200140605     c                   add       1             nsp(cevc41)
067300140605     c                   add       tasncl        ncl(cevc41)
067400140605     C                   add       Pkfvoc        pkf(cevc41)
067500140605     C                   add       vlfvoc        vlf(cevc41)
067600140605     c                   end
067700140605     c                   if        cevc42 = 0
067800140605     c                   leavesr
067900140605     c                   end
068000140605     c                   if        voc(cevc42)<> 0
068100140605     c                   movel     ric(cevc42)   $ric
068200140605     c                   exsr      srric
068300140605     c                   add       cevi42        imp(cevc42)
068400140605     c                   add       1             nsp(cevc42)
068500140605     c                   add       tasncl        ncl(cevc42)
068600140605     C                   add       Pkfvoc        pkf(cevc42)
068700140605     C                   add       vlfvoc        vlf(cevc42)
068800140605     c                   end
068900140605     c                   if        cevc43 = 0
069000140605     c                   leavesr
069100140605     c                   end
069200140605     c                   if        voc(cevc43) <> 0
069300140605     c                   movel     ric(cevc43)   $ric
069400140605     c                   exsr      srric
069500140605     c                   add       cevi43        imp(cevc43)
069600140605     c                   add       1             nsp(cevc43)
069700140605     c                   add       tasncl        ncl(cevc43)
069800140605     C                   add       Pkfvoc        pkf(cevc43)
069900140605     C                   add       vlfvoc        vlf(cevc43)
070000140605     c                   end
070100140605     c                   if        cevc44 = 0
070200140605     c                   leavesr
070300140605     c                   end
070400140605     c                   if        voc(cevc44) <> 0
070500140605     c                   movel     ric(cevc44)   $ric
070600140605     c                   exsr      srric
070700140605     c                   add       cevi44        imp(cevc44)
070800140605     c                   add       1             nsp(cevc44)
070900140605     c                   add       tasncl        ncl(cevc44)
071000140605     C                   add       Pkfvoc        pkf(cevc44)
071100140605     C                   add       vlfvoc        vlf(cevc44)
071200140605     c                   end
071300140605     c                   if        cevc45 = 0
071400140605     c                   leavesr
071500140605     c                   end
071600140605     c                   if        voc(cevc45) <> 0
071700140605     c                   movel     ric(cevc45)   $ric
071800140605     c                   exsr      srric
071900140605     c                   add       cevi45        imp(cevc45)
072000140605     c                   add       1             nsp(cevc45)
072100140605     c                   add       tasncl        ncl(cevc45)
072200140605     C                   add       Pkfvoc        pkf(cevc45)
072300140605     C                   add       vlfvoc        vlf(cevc45)
072400140605     c                   end
072500140605     c                   if        cevc46 = 0
072600140605     c                   leavesr
072700140605     c                   end
072800140605     c                   if        voc(cevc46) <> 0
072900140605     c                   movel     ric(cevc46)   $ric
073000140605     c                   exsr      srric
073100140605     c                   add       cevi46        imp(cevc46)
073200140605     c                   add       1             nsp(cevc46)
073300140605     c                   add       tasncl        ncl(cevc46)
073400140605     C                   add       Pkfvoc        pkf(cevc46)
073500140605     C                   add       vlfvoc        vlf(cevc46)
073600140605     c                   end
073700140605     c                   if        cevc47 = 0
073800140605     c                   leavesr
073900140605     c                   end
074000140605     c                   if        voc(cevc47)  <> 0
074100140605     c                   movel     ric(cevc47)   $ric
074200140605     c                   exsr      srric
074300140605     c                   add       cevi47        imp(cevc47)
074400140605     c                   add       1             nsp(cevc47)
074500140605     c                   add       tasncl        ncl(cevc47)
074600140605     C                   add       Pkfvoc        pkf(cevc47)
074700140605     C                   add       vlfvoc        vlf(cevc47)
074800140605     c                   end
074900140605     c                   if        cevc48 = 0
075000140605     c                   leavesr
075100140605     c                   end
075200140605     c                   if        voc(cevc48)<> 0
075300140605     c                   movel     ric(cevc48)   $ric
075400140605     c                   exsr      srric
075500140605     c                   add       cevi48        imp(cevc48)
075600140605     c                   add       1             nsp(cevc48)
075700140605     c                   add       tasncl        ncl(cevc48)
075800140605     C                   add       Pkfvoc        pkf(cevc48)
075900140605     C                   add       vlfvoc        vlf(cevc48)
076000140605     c                   end
076100140605     c                   if        cevc49 = 0
076200140605     c                   leavesr
076300140605     c                   end
076400140605     c                   if        voc(cevc49) <> 0
076500140605     c                   movel     ric(cevc49)   $ric
076600140605     c                   exsr      srric
076700140605     c                   add       cevi49        imp(cevc49)
076800140605     c                   add       1             nsp(cevc49)
076900140605     c                   add       tasncl        ncl(cevc49)
077000140605     C                   add       Pkfvoc        pkf(cevc49)
077100140605     C                   add       vlfvoc        vlf(cevc49)
077200140605     c                   end
077300140605     c                   if        cevc50 = 0
077400140605     c                   leavesr
077500140605     c                   end
077600140605     c                   if        voc(cevc50) <> 0
077700140605     c                   movel     ric(cevc50)   $ric
077800140605     c                   exsr      srric
077900140605     c                   add       cevi50        imp(cevc50)
078000140605     c                   add       1             nsp(cevc50)
078100140605     c                   add       tasncl        ncl(cevc50)
078200140605     C                   add       Pkfvoc        pkf(cevc50)
078300140605     C                   add       vlfvoc        vlf(cevc50)
078400140605     c                   end
078500140605     c* se sono tutte piene le voci aggancio anche il c2v
078600140605     c                   if        cevc50 <> 0
078700140605     c                   exsr      src2v
078800140605     c                   end
078900140605     C                   ENDSR
079000140604     c***********************************************************
079100140604     C     src2v         BEGSR
079200140604     c     ktas          chain     ecc2v30c
079300140604     c                   if        %found(ecc2v30c)
079400140605     c                   if        C2Vc51 = 0
079500140605     c                   leavesr
079600140605     c                   end
079700140605     c                   if        voc(C2Vc51)<> 0
079800140604     c                   movel     ric(C2Vc51)   $ric
079900140604     c                   exsr      srric
080000140605     c                   add       C2Vi51        imp(C2Vc51)
080100140604     c                   add       1             nsp(C2Vc51)
080200140604     c                   add       tasncl        ncl(C2Vc51)
080300140604     C                   add       Pkfvoc        pkf(C2Vc51)
080400140604     C                   add       vlfvoc        vlf(C2Vc51)
080500140604     c                   end
080600140605     c                   if        C2Vc52 = 0
080700140605     c                   leavesr
080800140605     c                   end
080900140605     c                   if        voc(C2Vc52) <> 0
081000140604     c                   movel     ric(C2Vc52)   $ric
081100140604     c                   exsr      srric
081200140605     c                   add       C2Vi52        imp(C2Vc52)
081300140604     c                   add       1             nsp(C2Vc52)
081400140604     c                   add       tasncl        ncl(C2Vc52)
081500140604     C                   add       Pkfvoc        pkf(C2Vc52)
081600140604     C                   add       vlfvoc        vlf(C2Vc52)
081700140604     c                   end
081800140605     c                   if        C2Vc53 = 0
081900140605     c                   leavesr
082000140605     c                   end
082100140605     c                   if        voc(C2Vc53)<> 0
082200140604     c                   movel     ric(C2Vc53)   $ric
082300140604     c                   exsr      srric
082400140605     c                   add       C2Vi53        imp(C2Vc53)
082500140604     c                   add       1             nsp(C2Vc53)
082600140604     c                   add       tasncl        ncl(C2Vc53)
082700140604     C                   add       Pkfvoc        pkf(C2Vc53)
082800140604     C                   add       vlfvoc        vlf(C2Vc53)
082900140604     c                   end
083000140605     c                   if        C2Vc54 = 0
083100140605     c                   leavesr
083200140605     c                   end
083300140605     c                   if        voc(C2Vc54)<> 0
083400140604     c                   movel     ric(C2Vc54)   $ric
083500140604     c                   exsr      srric
083600140605     c                   add       C2Vi54        imp(C2Vc54)
083700140604     c                   add       1             nsp(C2Vc54)
083800140604     c                   add       tasncl        ncl(C2Vc54)
083900140604     C                   add       Pkfvoc        pkf(C2Vc54)
084000140604     C                   add       vlfvoc        vlf(C2Vc54)
084100140604     c                   end
084200140605     c                   if        C2Vc55 = 0
084300140605     c                   leavesr
084400140605     c                   end
084500140605     c                   if        voc(C2Vc55)<> 0
084600140604     c                   movel     ric(C2Vc55)   $ric
084700140604     c                   exsr      srric
084800140605     c                   add       C2Vi55        imp(C2Vc55)
084900140604     c                   add       1             nsp(C2Vc55)
085000140604     c                   add       tasncl        ncl(C2Vc55)
085100140604     C                   add       Pkfvoc        pkf(C2Vc55)
085200140604     C                   add       vlfvoc        vlf(C2Vc55)
085300140604     c                   end
085400140605     c                   if        C2Vc56 = 0
085500140605     c                   leavesr
085600140605     c                   end
085700140605     c                   if        voc(C2Vc56) <> 0
085800140604     c                   movel     ric(C2Vc56)   $ric
085900140604     c                   exsr      srric
086000140605     c                   add       C2Vi56        imp(C2Vc56)
086100140604     c                   add       1             nsp(C2Vc56)
086200140604     c                   add       tasncl        ncl(C2Vc56)
086300140604     C                   add       Pkfvoc        pkf(C2Vc56)
086400140604     C                   add       vlfvoc        vlf(C2Vc56)
086500140604     c                   end
086600140605     c                   if        C2Vc57 = 0
086700140605     c                   leavesr
086800140605     c                   end
086900140605     c                   if        voc(C2Vc57)<> 0
087000140604     c                   movel     ric(C2Vc57)   $ric
087100140604     c                   exsr      srric
087200140605     c                   add       C2Vi57        imp(C2Vc57)
087300140604     c                   add       1             nsp(C2Vc57)
087400140604     c                   add       tasncl        ncl(C2Vc57)
087500140604     C                   add       Pkfvoc        pkf(C2Vc57)
087600140604     C                   add       vlfvoc        vlf(C2Vc57)
087700140604     c                   end
087800140605     c                   if        C2Vc58 = 0
087900140605     c                   leavesr
088000140605     c                   end
088100140605     c                   if        voc(C2Vc58) <> 0
088200140604     c                   movel     ric(C2Vc58)   $ric
088300140604     c                   exsr      srric
088400140605     c                   add       C2Vi58        imp(C2Vc58)
088500140604     c                   add       1             nsp(C2Vc58)
088600140604     c                   add       tasncl        ncl(C2Vc58)
088700140604     C                   add       Pkfvoc        pkf(C2Vc58)
088800140604     C                   add       vlfvoc        vlf(C2Vc58)
088900140604     c                   end
089000140605     c                   if        C2Vc59 = 0
089100140605     c                   leavesr
089200140605     c                   end
089300140605     c                   if        voc(C2Vc59)<> 0
089400140604     c                   movel     ric(C2Vc59)   $ric
089500140604     c                   exsr      srric
089600140605     c                   add       C2Vi59        imp(C2Vc59)
089700140604     c                   add       1             nsp(C2Vc59)
089800140604     c                   add       tasncl        ncl(C2Vc59)
089900140604     C                   add       Pkfvoc        pkf(C2Vc59)
090000140604     C                   add       vlfvoc        vlf(C2Vc59)
090100140604     c                   end
090200140605     c                   if        C2Vc60 = 0
090300140605     c                   leavesr
090400140605     c                   end
090500140605     c                   if        voc(C2Vc60) <> 0
090600140604     c                   movel     ric(C2Vc60)   $ric
090700140604     c                   exsr      srric
090800140605     c                   add       C2Vi60        imp(C2Vc60)
090900140604     c                   add       1             nsp(C2Vc60)
091000140604     c                   add       tasncl        ncl(C2Vc60)
091100140604     C                   add       Pkfvoc        pkf(C2Vc60)
091200140604     C                   add       vlfvoc        vlf(C2Vc60)
091300140604     c                   end
091400140605     c                   if        C2Vc61 = 0
091500140605     c                   leavesr
091600140605     c                   end
091700140605     c                   if        voc(C2Vc61) <> 0
091800140604     c                   movel     ric(C2Vc61)   $ric
091900140604     c                   exsr      srric
092000140605     c                   add       C2Vi61        imp(C2Vc61)
092100140604     c                   add       1             nsp(C2Vc61)
092200140604     c                   add       tasncl        ncl(C2Vc61)
092300140604     C                   add       Pkfvoc        pkf(C2Vc61)
092400140604     C                   add       vlfvoc        vlf(C2Vc61)
092500140604     c                   end
092600140605     c                   if        C2Vc62 = 0
092700140605     c                   leavesr
092800140605     c                   end
092900140605     c                   if        voc(C2Vc62)<> 0
093000140604     c                   movel     ric(C2Vc62)   $ric
093100140604     c                   exsr      srric
093200140605     c                   add       C2Vi62        imp(C2Vc62)
093300140604     c                   add       1             nsp(C2Vc62)
093400140604     c                   add       tasncl        ncl(C2Vc62)
093500140604     C                   add       Pkfvoc        pkf(C2Vc62)
093600140604     C                   add       vlfvoc        vlf(C2Vc62)
093700140604     c                   end
093800140605     c                   if        C2Vc63 = 0
093900140605     c                   leavesr
094000140605     c                   end
094100140605     c                   if        voc(C2Vc63) <> 0
094200140604     c                   movel     ric(C2Vc63)   $ric
094300140604     c                   exsr      srric
094400140605     c                   add       C2Vi63        imp(C2Vc63)
094500140604     c                   add       1             nsp(C2Vc63)
094600140604     c                   add       tasncl        ncl(C2Vc63)
094700140604     C                   add       Pkfvoc        pkf(C2Vc63)
094800140604     C                   add       vlfvoc        vlf(C2Vc63)
094900140604     c                   end
095000140605     c                   if        C2Vc64 = 0
095100140605     c                   leavesr
095200140605     c                   end
095300140605     c                   if        voc(C2Vc64)<> 0
095400140604     c                   add       C2Vi64        imp(C2Vc64)
095500140604     c                   movel     ric(C2Vc64)   $ric
095600140604     c                   exsr      srric
095700140604     c                   add       1             nsp(C2Vc64)
095800140604     c                   add       tasncl        ncl(C2Vc64)
095900140604     C                   add       Pkfvoc        pkf(C2Vc64)
096000140604     C                   add       vlfvoc        vlf(C2Vc64)
096100140604     c                   end
096200140605     c                   if        C2Vc65 = 0
096300140605     c                   leavesr
096400140605     c                   end
096500140605     c                   if        voc(C2Vc65) <> 0
096600140604     c                   movel     ric(C2Vc65)   $ric
096700140604     c                   exsr      srric
096800140605     c                   add       C2Vi65        imp(C2Vc65)
096900140604     c                   add       1             nsp(C2Vc65)
097000140604     c                   add       tasncl        ncl(C2Vc65)
097100140604     C                   add       Pkfvoc        pkf(C2Vc65)
097200140604     C                   add       vlfvoc        vlf(C2Vc65)
097300140604     c                   end
097400140605     c                   if        C2Vc66 = 0
097500140605     c                   leavesr
097600140605     c                   end
097700140605     c                   if        voc(C2Vc66)<> 0
097800140604     c                   movel     ric(C2Vc66)   $ric
097900140604     c                   exsr      srric
098000140605     c                   add       C2Vi66        imp(C2Vc66)
098100140604     c                   add       1             nsp(C2Vc66)
098200140604     c                   add       tasncl        ncl(C2Vc66)
098300140604     C                   add       Pkfvoc        pkf(C2Vc66)
098400140604     C                   add       vlfvoc        vlf(C2Vc66)
098500140604     c                   end
098600140605     c                   if        C2Vc67 = 0
098700140605     c                   leavesr
098800140605     c                   end
098900140605     c                   if        voc(C2Vc67) <> 0
099000140604     c                   movel     ric(C2Vc67)   $ric
099100140604     c                   exsr      srric
099200140605     c                   add       C2Vi67        imp(C2Vc67)
099300140604     c                   add       1             nsp(C2Vc67)
099400140604     c                   add       tasncl        ncl(C2Vc67)
099500140604     C                   add       Pkfvoc        pkf(C2Vc67)
099600140604     C                   add       vlfvoc        vlf(C2Vc67)
099700140604     c                   end
099800140605     c                   if        C2Vc68 = 0
099900140605     c                   leavesr
100000140605     c                   end
100100140605     c                   if        voc(C2Vc68)<> 0
100200140604     c                   movel     ric(C2Vc68)   $ric
100300140604     c                   exsr      srric
100400140605     c                   add       C2Vi68        imp(C2Vc68)
100500140604     c                   add       1             nsp(C2Vc68)
100600140604     c                   add       tasncl        ncl(C2Vc68)
100700140604     C                   add       Pkfvoc        pkf(C2Vc68)
100800140604     C                   add       vlfvoc        vlf(C2Vc68)
100900140604     c                   end
101000140605     c                   if        C2Vc69 = 0
101100140605     c                   leavesr
101200140605     c                   end
101300140605     c                   if        voc(C2Vc69) <> 0
101400140604     c                   movel     ric(C2Vc69)   $ric
101500140604     c                   exsr      srric
101600140605     c                   add       C2Vi69        imp(C2Vc69)
101700140604     c                   add       1             nsp(C2Vc69)
101800140604     c                   add       tasncl        ncl(C2Vc69)
101900140604     C                   add       Pkfvoc        pkf(C2Vc69)
102000140604     C                   add       vlfvoc        vlf(C2Vc69)
102100140604     c                   end
102200140605     c                   if        C2Vc70 = 0
102300140605     c                   leavesr
102400140605     c                   end
102500140605     c                   if        voc(C2Vc70) <> 0
102600140604     c                   movel     ric(C2Vc70)   $ric
102700140604     c                   exsr      srric
102800140605     c                   add       C2Vi70        imp(C2Vc70)
102900140604     c                   add       1             nsp(C2Vc70)
103000140604     c                   add       tasncl        ncl(C2Vc70)
103100140604     C                   add       Pkfvoc        pkf(C2Vc70)
103200140604     C                   add       vlfvoc        vlf(C2Vc70)
103300140604     c                   end
103400140605     c                   if        C2Vc71 = 0
103500140605     c                   leavesr
103600140605     c                   end
103700140605     c                   if        voc(C2Vc71) <> 0
103800140604     c                   movel     ric(C2Vc71)   $ric
103900140604     c                   exsr      srric
104000140605     c                   add       C2Vi71        imp(C2Vc71)
104100140604     c                   add       1             nsp(C2Vc71)
104200140604     c                   add       tasncl        ncl(C2Vc71)
104300140604     C                   add       Pkfvoc        pkf(C2Vc71)
104400140604     C                   add       vlfvoc        vlf(C2Vc71)
104500140604     c                   end
104600140605     c                   if        C2Vc72 = 0
104700140605     c                   leavesr
104800140605     c                   end
104900140605     c                   if        voc(C2Vc72)<> 0
105000140604     c                   movel     ric(C2Vc72)   $ric
105100140604     c                   exsr      srric
105200140605     c                   add       C2Vi72        imp(C2Vc72)
105300140605     c                   add       1             nsp(C2Vc72)
105400140604     c                   add       tasncl        ncl(C2Vc72)
105500140604     C                   add       Pkfvoc        pkf(C2Vc72)
105600140604     C                   add       vlfvoc        vlf(C2Vc72)
105700140604     c                   end
105800140605     c                   if        C2Vc73 = 0
105900140605     c                   leavesr
106000140605     c                   end
106100140605     c                   if        voc(C2Vc73)<> 0
106200140604     c                   movel     ric(C2Vc73)   $ric
106300140604     c                   exsr      srric
106400140605     c                   add       C2Vi73        imp(C2Vc73)
106500140604     c                   add       1             nsp(C2Vc73)
106600140604     c                   add       tasncl        ncl(C2Vc73)
106700140604     C                   add       Pkfvoc        pkf(C2Vc73)
106800140604     C                   add       vlfvoc        vlf(C2Vc73)
106900140604     c                   end
107000140605     c                   if        C2Vc74 = 0
107100140605     c                   leavesr
107200140605     c                   end
107300140605     c                   if        voc(C2Vc74)<> 0
107400140604     c                   movel     ric(C2Vc74)   $ric
107500140604     c                   exsr      srric
107600140605     c                   add       C2Vi74        imp(C2Vc74)
107700140604     c                   add       1             nsp(C2Vc74)
107800140604     c                   add       tasncl        ncl(C2Vc74)
107900140604     C                   add       Pkfvoc        pkf(C2Vc74)
108000140604     C                   add       vlfvoc        vlf(C2Vc74)
108100140604     c                   end
108200140605     c                   if        C2Vc75 = 0
108300140605     c                   leavesr
108400140605     c                   end
108500140605     c                   if        voc(C2Vc75)<> 0
108600140604     c                   movel     ric(C2Vc75)   $ric
108700140604     c                   exsr      srric
108800140605     c                   add       C2Vi75        imp(C2Vc75)
108900140604     c                   add       1             nsp(C2Vc75)
109000140604     c                   add       tasncl        ncl(C2Vc75)
109100140604     C                   add       Pkfvoc        pkf(C2Vc75)
109200140604     C                   add       vlfvoc        vlf(C2Vc75)
109300140604     c                   end
109400140604     c                   end
109500140604     c                   endsr
109600140605     c**********************************************************************
109700140605     c     *inzsr        begsr
109800140605     c*
109900140605     C     *ENTRY        PLIST
110000140605     C                   PARM                    KPJBA
110100140605     C                   MOVEL     KPJBU         param
110200140605     c*
110300140605     c     ktas          klist
110400140605     c                   kfld                    cevaas
110500140605     c                   kfld                    cevlnp
110600140605     c                   kfld                    cevnrs
110700140605     c                   kfld                    cevnsp
110800140605     c                   kfld                    cevtbl
110900140605     c     ktab          klist
111000140605     c                   kfld                    tblkut
111100140605     c                   kfld                    tblcod
111200140605     c                   eval      tblkut = 1
111300140605     c                   eval      tblcod = '2K'
111400140605     C     KEYAR5        KLIST
111500140605     C                   KFLD                    cevaas
111600140605     C                   KFLD                    cevlnp
111700140605     C                   KFLD                    cevnrs
111800140605     C                   KFLD                    cevnsp
111900140605     C                   KFLD                    AR5TRD
112000140605     C                   KFLD                    tasflp
112100140605     c                   eval      ar5trd = 'TRS'
112200140605     c                   seton                                        lr
112300140605     c                   movel     aa            dtf
112400140605     c                   movel     mmf           com4              4 0
112500140605     c                   move      31            com4              4 0
112600140605     c                   move      com4          dtf               8 0
112700140605     c* se non passato il numero blocco della competenza, lo ricavo
112800140605     c* dalòla tabella
112900140605     c                   if        nbc = 0
113000140605     c     ktab          chain     tabel00f
113100140605     c                   if        %found(tabel00f)
113200140605     c                   movel     tbluni        ds2k
113300140605     c                   eval      nbc = §2knbc
113400140605     c                   else
113500140605     c                   return
113600140605     c                   end
113700140605     c                   end
113800140605     c*
113900140605     c* pulisco il file
114000140605     C/EXEC SQL
114100140605     C+ DELETE FROM eccdc00f where
114200140605     C+ cdcaas = :aa and cdcmes between :mmi and :mmf
114300140605     C/END-EXEC
114400140605      /free
114500140605       //?estraggo prima i conti per reperire il tipo CDC x ogni competenza
114600140605       //?poi memorizzo le voci con i relativi cdc
114700140605         exec sql
114800140605          declare arb cursor for
114900140605
115000140605          with
115100140605          selconti as
115200140605          (SELECT ECDCMP, ECDRIC, ECDCCF FROM ececd00f WHERE
115300140605          ECDDSO >= :dtf and ECDSEG = 'A' and ECDNBC = :nbc
115400140605          GROUP BY ECDCMP, ECDRIC, ecdccf),
115500140605
115600140605          selvoci as (select ecevt00f.*
115700140605          from ecevt00f WHERE EVTDSV >= :dtf and evttvo = 'V')
115800140605
115900140605          select evtvoc, ecdric, ecdccf from selvoci join selconti on
116000140605          evtcmp = ecdcmp;
116100140605
116200140605         exec sql open arb;
116300140605
116400140605         dow not $finearb;
116500140605          exec sql fetch next from arb into
116600140605          :evtvoc, :ecdric, :ecdccf;
116700140605       //?fine file o errore sql esco
116800140605         if sqlcod = 100 or sqlcod < 0;
116900140605           $finearb = *on;
117000140605           leave;
117100140605          endif;
117200140605           voc(evtvoc) = evtvoc;
117300140605           ric(evtvoc) = ecdric;
117400140605      /end-free
117500140605     c                   if        ecdric = ' '
117600140605     c                   movel     ecdccf        com5
117700140605     c                   move      com5          ric(evtvoc)
117800140605     c                   end
117900140605      /free
118000140605         enddo;
118100140605       //?chiudo il cursore
118200140605          exec sql close arb;
118300140605      /end-free
118400140605     c                   endsr
