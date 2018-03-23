000100010925      *PARMS DYNUSRPRF(*OWNER)
000200970526     H*------------------------------------------------------------------------*
000300000920     H DECEDIT('0,') DATEDIT(*DMY.)
000400970526     F*------------------------------------------------------------------------*
000500970526     F* DATA BASE
000600970526     F*------------------------------------------------------------------------*
000700140604     Feccdc00f  o    e             disk
000800140605     Ftabel00f  if   e           k disk
000900140604     Fecc2v30c  if   e           k disk
001000140604     Ftitas30c  if   e           k disk
001100140605     FFIAR531C  IF   E           K DISK
001200140606     FFNLBL01L  IF   E           K DISK
001300140605     D ds2k          E DS
001400140605     D DAR5TRS       E DS
001500140604     D ececdds       E DS                  extname(ececd00f)
001600140604     D ecevtds       E DS                  extname(ecevt00f)
001700140604     D eccevds       E DS                  extname(eccev00f)
001800140606     D titasds       E DS                  extname(titas00f)
001900140606     D titasdss      E DS                  extname(titas00f) prefix(s)
002000140604     D KPJBA         E DS
002100110420     d param           ds
002200140604     d aa                      1      4s 0
002300140604     d mmi                     5      6s 0
002400140604     d mmf                     7      8s 0
002500140604     d nbc                     9     13s 0
002600140605     d voc             s              3  0 DIM(999) inz
002700140605     d ric             s              3    DIM(999) inz
002800140606     d cmp             s              3  0 DIM(999) inz
002900140605     D fili            DS                  OCCURS(999) INZ
003000140605     d imp                           13  5 DIM(999) inz
003100140605     D filc            DS                  OCCURS(999) INZ
003200140605     d ncl                            7  0 DIM(999) inz
003300140605     D fils            DS                  OCCURS(999) INZ
003400140605     d nsp                            7  0 DIM(999) inz
003500140605     D filv            DS                  OCCURS(999) INZ
003600140605     d vlf                            9  3 DIM(999) inz
003700140605     D filp            DS                  OCCURS(999) INZ
003800140605     d pkf                            9  1 DIM(999) inz
003900140604     d anno            s              4  0
004000140604     d mese            s              2  0
004100140604     d com5            s              5
004200140604     d $finearb        s               n
004300140604     d pkfvoc          s                   like(cdcpkf)
004400140604     d vlfvoc          s                   like(cdcvlf)
004500140606     d wlnp            s                   like(taslnp)
004600140606     d wtfp            s                   like(tastfp)
004700140604     c* --------------------------------------------------------------------
004800140609     C     *ENTRY        PLIST
004900140609     C                   PARM                    KPJBA
005000140609     C                   MOVEL     KPJBU         param
005100140609     C* CHIAVE LETTURA FLLBL01L - COMPLETA
005200140609     C     KEYTAS        KLIST
005300140609     C                   KFLD                    TASAAS
005400140609     C                   KFLD                    TASLNP
005500140609     C                   KFLD                    TASNRS
005600140609     C                   KFLD                    TASNSP
005700140609     C                   KFLD                    TASTBL
005800140609     C     KEYLBL        KLIST
005900140609     C                   KFLD                    LBLAAN
006000140609     C                   KFLD                    LBLLPN
006100140609     C                   KFLD                    LBLNRN
006200140609     C                   KFLD                    LBLNSN
006300140609     c     ktas          klist
006400140609     c                   kfld                    cevaas
006500140609     c                   kfld                    cevlnp
006600140609     c                   kfld                    cevnrs
006700140609     c                   kfld                    cevnsp
006800140609     c                   kfld                    cevtbl
006900140609     c     ktab          klist
007000140609     c                   kfld                    tblkut
007100140609     c                   kfld                    tblcod
007200140609     c                   eval      tblkut = 1
007300140609     c                   eval      tblcod = '2K'
007400140609     C     KEYAR5        KLIST
007500140609     C                   KFLD                    cevaas
007600140609     C                   KFLD                    cevlnp
007700140609     C                   KFLD                    cevnrs
007800140609     C                   KFLD                    cevnsp
007900140609     C                   KFLD                    AR5TRD
008000140609     C                   KFLD                    tasflp
008100140609     c                   eval      ar5trd = 'TRS'
008200140609     c                   seton                                        lr
008300140609     c                   movel     aa            dtf
008400140609     c                   movel     mmf           com4              4 0
008500140609     c                   move      31            com4              4 0
008600140609     c                   move      com4          dtf               8 0
008700140609     c* se non passato il numero blocco della competenza, lo ricavo
008800140609     c* dalòla tabella
008900140609     c                   if        nbc = 0
009000140609     c     ktab          chain     tabel00f
009100140609     c                   if        %found(tabel00f)
009200140609     c                   movel     tbluni        ds2k
009300140609     c                   eval      nbc = §2knbc
009400140609     c                   else
009500140609     c                   return
009600140609     c                   end
009700140609     c                   end
009800140609     c*
009900140609     c* pulisco il file
010000140609     C/EXEC SQL
010100140609     C+ DELETE FROM eccdc00f where
010200140609     C+ cdcaas = :aa and cdcmes between :mmi and :mmf
010300140609     C/END-EXEC
010400140609      /free
010500140609       //?estraggo prima i conti per reperire il tipo CDC x ogni competenza
010600140609       //?poi memorizzo le voci con i relativi cdc
010700140609         exec sql
010800140609          declare arb cursor for
010900140609
011000140609          with
011100140609          selconti as
011200140609          (SELECT ECDCMP, ECDRIC, ECDCCF FROM ececd00f WHERE
011300140609          ECDDSO >= :dtf and ECDSEG = 'A' and ECDNBC = :nbc
011400140609          GROUP BY ECDCMP, ECDRIC, ecdccf),
011500140609
011600140609          selvoci as (select ecevt00f.*
011700140609          from ecevt00f WHERE EVTDSV >= :dtf and evttvo = 'V')
011800140609
011900140609          select evtvoc, ecdric, ecdccf, ecdcmp from selvoci join selconti
012000140609          on evtcmp = ecdcmp;
012100140609
012200140609         exec sql open arb;
012300140609
012400140609         dow not $finearb;
012500140609          exec sql fetch next from arb into
012600140609          :evtvoc, :ecdric, :ecdccf, :ecdcmp;
012700140609       //?fine file o errore sql esco
012800140609         if sqlcod = 100 or sqlcod < 0;
012900140609           $finearb = *on;
013000140609           leave;
013100140609          endif;
013200140609           voc(evtvoc) = evtvoc;
013300140609           ric(evtvoc) = ecdric;
013400140609           cmp(evtvoc) = ecdcmp;
013500140609      /end-free
013600140609     c                   if        ecdric = ' '
013700140609     c                   movel     ecdccf        com5
013800140609     c                   move      com5          ric(evtvoc)
013900140609     c                   end
014000140609      /free
014100140609         enddo;
014200140609       //?chiudo il cursore
014300140609          exec sql close arb;
014400140609      /end-free
014500140604     c*
014600050502     C/EXEC SQL
014700140604     C+ DECLARE C1 CURSOR FOR SELECT eccev00f.* FROM eccev00f WHERE
014800140604     C+ cevaas = :aa and cevmes between :mmi and :mmf ORDER BY cevaas,
014900140604     C+ cevmes
015000050502     C/END-EXEC
015100110420     c*
015200050502     C/EXEC SQL
015300050502     C+ OPEN c1
015400050502     C/END-EXEC
015500110420     c*
015600050502
015700050502     C                   DO        *HIVAL
015800050502
015900050502     C/EXEC SQL
016000140604     C+ FETCH NEXT FROM c1 INTO :eccevds
016100050502     C/END-EXEC
016200050502
016300110420     C                   IF        SQLCod = 100 or sqlcod < 0
016400050502     C                   LEAVE
016500050502     C                   ENDIF
016600140604     c* a rottura di anno e mese scarico la schiera
016700140604     c                   if        (anno <> 0 and mese <> 0) and
016800140604     c                             (cevaas <> anno or cevmes <> mese)
016900140604     c                   exsr      scrivi
017000140604     c                   end
017100140604     c                   eval      anno = cevaas
017200140604     c                   eval      mese = cevmes
017300140604     c* reperisco le informazioni da titas
017400140604     c     ktas          chain     titas30c
017500140604     c                   if        %found(titas30c)
017600140606     c                   eval      wlnp = taslnp
017700140606     c                   eval      wtfp = tastfp
017800140605     c                   exsr      srpesvol
017900140606     C* SE FLAG 'HA LEGAMI' IN TITAS = 'SI' LEGGO SUL FILE LEGAMI
018000140606     C* SE E' BOLLA DI RIENTRO NON LA CONSIDERO COME LEGATA
018100140606     C     tasfbr        IFne      'S'
018200140606     C     tasFA1        andeq     'S'
018300140606     c                   exsr      gesleg
018400140606     c                   end
018500140605     c                   clear                   dar5trs
018600140605     C     KEYAR5        CHAIN     FIAR531C
018700140605     c                   if        %found(fiar531c)
018800140605     C                   MOVEL     AR5UNI        DAR5TRS
018900140605     c                   end
019000140605     c                   EXSR      conta
019100140605     c                   end
019200050502
019300050502     C                   ENDDO
019400050502
019500050502     C/EXEC SQL
019600050502     C+ CLOSE c1
019700050502     C/END-EXEC
019800140604
019900140604     c                   if        anno <> 0 and mese <> 0
020000140604     c                   exsr      scrivi
020100140604     c                   end
020200140604     C*--------------------------------------------------------------- ---------
020300140605     C     srpesvol      BEGSR
020400140604     c* peso
020500140604IF  2C     tasPKC        IFGT      *ZEROS
020600140604     C     tasncl        ANDEQ     tasNCP
020700140604     C                   z-add     tasPKC        Pkfvoc
020800140604X   2C                   ELSE
020900140604IF  3C     tasPKF        IFGT      tasPKC
021000140604     C                   z-add     tasPKF        pkfvoc
021100140604X   3C                   ELSE
021200140604     C                   z-add     tasPKC        Pkfvoc
021300140604E   3C                   ENDIF
021400140604E   2C                   ENDIF
021500140604     c* volume
021600140604IF  1C     tasFVF        IFEQ      'T'
021700140604     C                   z-add     tasVLF        vlfvoc
021800140604X   1C                   ELSE
021900140604IF  2C     tasVLC        IFGT      *ZEROS
022000140604     C     tasNCL        ANDEQ     tasNCR
022100140604     C                   z-add     tasVLC        vlfvoc
022200140604X   2C                   ELSE
022300140604IF  3C     tasVLF        IFGT      tasVLC
022400140604     C                   z-add     tasVLF        vlfvoc
022500140604X   3C                   ELSE
022600140604     C                   z-add     tasVLC        vlfvoc
022700140604E   3C                   ENDIF
022800140604E   2C                   ENDIF
022900140604E   1C                   ENDIF
023000140605     c                   endsr
023100140605     C*--------------------------------------------------------------- ---------
023200140605     C     srric         BEGSR
023300140604     c* cdc
023400140604     c                   select
023500140604     C     $ric          WHENEQ    'LA'
023600140605     C                   MOVE      tasLNA        $FIL              3 0
023700140604     C     $RIC          WHENEQ    'LP'
023800140606     C                   MOVE      wLNP          $FIL
023900140604     C     $RIC          WHENEQ    'TA'
024000140604     C                   MOVE      tasTFA        $FIL
024100140604     C     $RIC          WHENEQ    'TP'
024200140606     C                   MOVE      wtfp          $FIL
024300140605     C     $RIC          WHENEQ    'T1'
024400140605     C                   MOVE      §AR5flp       $FIL
024500140605     C     $RIC          WHENEQ    'T2'
024600140605     C                   MOVE      §AR5fl2       $FIL
024700140605     C     $RIC          WHENEQ    'T3'
024800140605     C                   MOVE      §AR5fl3       $FIL
024900140605     C     $RIC          WHENEQ    'T4'
025000140605     C                   MOVE      §AR5fl4       $FIL
025100140605     C     $RIC          WHENEQ    'T5'
025200140605     C                   MOVE      §AR5fl5       $FIL
025300140604     c                   other
025400140604     C                   MOVE      $ric          $FIL
025500140604E   4C                   ENDSL
025600140605     c     $fil          occur     fili
025700140605     c     $fil          occur     filc
025800140605     c     $fil          occur     fils
025900140605     c     $fil          occur     filv
026000140605     c     $fil          occur     filp
026100140604     c
026200140604     c                   endsr
026300140604     C*--------------------------------------------------------------- ---------
026400140604     c     scrivi        begsr
026500140605DO  1C     1             DO        999           f                 4 0
026600140605     C     f             OCCUR     Fili
026700140605     C     f             OCCUR     Filc
026800140605     C     f             OCCUR     Fils
026900140605     C     f             OCCUR     Filp
027000140605     C     f             OCCUR     Filv
027100140604     c                   do        999           i                 4 0
027200140604     c     voc(i)        ifne      0
027300140605     c     imp(i)        andne     0
027400140604     c                   eval      cdcaas = anno
027500140604     c                   eval      cdcmes = mese
027600140604     c                   eval      cdcvoc = voc(i)
027700140606     c                   eval      cdccmp = cmp(i)
027800140604     c                   eval      cdcimp = imp(i)
027900140605     c                   eval      cdcfil = f
028000140604     c                   eval      cdcnsp = nsp(i)
028100140604     c                   eval      cdcncl = ncl(i)
028200140604     c                   eval      cdcvlf = vlf(i)
028300140604     c                   eval      cdcpkf = pkf(i)
028400140604     c                   write     eccdc000
028500140604     c                   end
028600140604     c                   enddo
028700140605     c                   clear                   imp
028800140605     c                   clear                   nsp
028900140605     c                   clear                   ncl
029000140605     c                   clear                   vlf
029100140605     c                   clear                   pkf
029200140605     c                   enddo
029300140604     c                   endsr
029400140605     c***********************************************************
029500140605     C     conta         BEGSR
029600140605     c                   if        cevc01 = 0
029700140605     c                   leavesr
029800140605     c                   end
029900140605     c                   if        voc(cevc01) <> 0
030000140605     c                   movel     ric(cevc01)   $ric              3
030100140605     c                   exsr      srric
030200140605     c                   add       cevi01        imp(cevc01)
030300140605     c                   add       1             nsp(cevc01)
030400140605     c                   add       tasncl        ncl(cevc01)
030500140605     C                   add       Pkfvoc        pkf(cevc01)
030600140605     C                   add       vlfvoc        vlf(cevc01)
030700140605     c                   end
030800140605     c                   if        cevc02 = 0
030900140605     c                   leavesr
031000140605     c                   end
031100140605     c                   if        voc(cevc02) <> 0
031200140605     c                   movel     ric(cevc02)   $ric
031300140605     c                   exsr      srric
031400140605     c                   add       cevi02        imp(cevc02)
031500140605     c                   add       1             nsp(cevc02)
031600140605     c                   add       tasncl        ncl(cevc02)
031700140605     C                   add       Pkfvoc        pkf(cevc02)
031800140605     C                   add       vlfvoc        vlf(cevc02)
031900140605     c                   end
032000140605     c                   if        cevc03 = 0
032100140605     c                   leavesr
032200140605     c                   end
032300140605     c                   if        voc(cevc03) <> 0
032400140605     c                   movel     ric(cevc03)   $ric
032500140605     c                   exsr      srric
032600140605     c                   add       cevi03        imp(cevc03)
032700140605     c                   add       1             nsp(cevc03)
032800140605     c                   add       tasncl        ncl(cevc03)
032900140605     C                   add       Pkfvoc        pkf(cevc03)
033000140605     C                   add       vlfvoc        vlf(cevc03)
033100140605     c                   end
033200140605     c                   if        cevc04 = 0
033300140605     c                   leavesr
033400140605     c                   end
033500140605     c                   if        voc(cevc04) <> 0
033600140605     c                   movel     ric(cevc04)   $ric
033700140605     c                   exsr      srric
033800140605     c                   add       cevi04        imp(cevc04)
033900140605     c                   add       1             nsp(cevc04)
034000140605     c                   add       tasncl        ncl(cevc04)
034100140605     C                   add       Pkfvoc        pkf(cevc04)
034200140605     C                   add       vlfvoc        vlf(cevc04)
034300140605     c                   end
034400140605     c                   if        cevc05 = 0
034500140605     c                   leavesr
034600140605     c                   end
034700140605     c                   if        voc(cevc05) <> 0
034800140605     c                   movel     ric(cevc05)   $ric
034900140605     c                   exsr      srric
035000140605     c                   add       cevi05        imp(cevc05)
035100140605     c                   add       1             nsp(cevc05)
035200140605     c                   add       tasncl        ncl(cevc05)
035300140605     C                   add       Pkfvoc        pkf(cevc05)
035400140605     C                   add       vlfvoc        vlf(cevc05)
035500140605     c                   end
035600140605     c                   if        cevc06 = 0
035700140605     c                   leavesr
035800140605     c                   end
035900140605     c                   if        voc(cevc06) <> 0
036000140605     c                   movel     ric(cevc06)   $ric
036100140605     c                   exsr      srric
036200140605     c                   add       cevi06        imp(cevc06)
036300140605     c                   add       1             nsp(cevc06)
036400140605     c                   add       tasncl        ncl(cevc06)
036500140605     C                   add       Pkfvoc        pkf(cevc06)
036600140605     C                   add       vlfvoc        vlf(cevc06)
036700140605     c                   end
036800140605     c                   if        cevc07 = 0
036900140605     c                   leavesr
037000140605     c                   end
037100140605     c                   if        voc(cevc07) <> 0
037200140605     c                   movel     ric(cevc07)   $ric
037300140605     c                   exsr      srric
037400140605     c                   add       cevi07        imp(cevc07)
037500140605     c                   add       1             nsp(cevc07)
037600140605     c                   add       tasncl        ncl(cevc07)
037700140605     C                   add       Pkfvoc        pkf(cevc07)
037800140605     C                   add       vlfvoc        vlf(cevc07)
037900140605     c                   end
038000140605     c                   if        cevc08 = 0
038100140605     c                   leavesr
038200140605     c                   end
038300140605     c                   if        voc(cevc08)<> 0
038400140605     c                   movel     ric(cevc08)   $ric
038500140605     c                   exsr      srric
038600140605     c                   add       cevi08        imp(cevc08)
038700140605     c                   add       1             nsp(cevc08)
038800140605     c                   add       tasncl        ncl(cevc08)
038900140605     C                   add       Pkfvoc        pkf(cevc08)
039000140605     C                   add       vlfvoc        vlf(cevc08)
039100140605     c                   end
039200140605     c                   if        cevc09 = 0
039300140605     c                   leavesr
039400140605     c                   end
039500140605     c                   if        voc(cevc09) <> 0
039600140605     c                   movel     ric(cevc09)   $ric
039700140605     c                   exsr      srric
039800140605     c                   add       cevi09        imp(cevc09)
039900140605     c                   add       1             nsp(cevc09)
040000140605     c                   add       tasncl        ncl(cevc09)
040100140605     C                   add       Pkfvoc        pkf(cevc09)
040200140605     C                   add       vlfvoc        vlf(cevc09)
040300140605     c                   end
040400140605     c                   if        cevc10 = 0
040500140605     c                   leavesr
040600140605     c                   end
040700140605     c                   if        voc(cevc10) <> 0
040800140605     c                   movel     ric(cevc10)   $ric
040900140605     c                   exsr      srric
041000140605     c                   add       cevi10        imp(cevc10)
041100140605     c                   add       1             nsp(cevc10)
041200140605     c                   add       tasncl        ncl(cevc10)
041300140605     C                   add       Pkfvoc        pkf(cevc10)
041400140605     C                   add       vlfvoc        vlf(cevc10)
041500140605     c                   end
041600140605     c                   if        cevc11 = 0
041700140605     c                   leavesr
041800140605     c                   end
041900140605     c                   if        voc(cevc11) <> 0
042000140605     c                   movel     ric(cevc11)   $ric
042100140605     c                   exsr      srric
042200140605     c                   add       cevi11        imp(cevc11)
042300140605     c                   add       1             nsp(cevc11)
042400140605     c                   add       tasncl        ncl(cevc11)
042500140605     C                   add       Pkfvoc        pkf(cevc11)
042600140605     C                   add       vlfvoc        vlf(cevc11)
042700140605     c                   end
042800140605     c                   if        cevc12 = 0
042900140605     c                   leavesr
043000140605     c                   end
043100140605     c                   if        voc(cevc12)<> 0
043200140605     c                   movel     ric(cevc12)   $ric
043300140605     c                   exsr      srric
043400140605     c                   add       cevi12        imp(cevc12)
043500140605     c                   add       1             nsp(cevc12)
043600140605     c                   add       tasncl        ncl(cevc12)
043700140605     C                   add       Pkfvoc        pkf(cevc12)
043800140605     C                   add       vlfvoc        vlf(cevc12)
043900140605     c                   end
044000140605     c                   if        cevc13 = 0
044100140605     c                   leavesr
044200140605     c                   end
044300140605     c                   if        voc(cevc13) <> 0
044400140605     c                   movel     ric(cevc13)   $ric
044500140605     c                   exsr      srric
044600140605     c                   add       cevi13        imp(cevc13)
044700140605     c                   add       1             nsp(cevc13)
044800140605     c                   add       tasncl        ncl(cevc13)
044900140605     C                   add       Pkfvoc        pkf(cevc13)
045000140605     C                   add       vlfvoc        vlf(cevc13)
045100140605     c                   end
045200140605     c                   if        cevc14 = 0
045300140605     c                   leavesr
045400140605     c                   end
045500140605     c                   if        voc(cevc14)<> 0
045600140605     c                   movel     ric(cevc14)   $ric
045700140605     c                   exsr      srric
045800140605     c                   add       cevi14        imp(cevc14)
045900140605     c                   add       1             nsp(cevc14)
046000140605     c                   add       tasncl        ncl(cevc14)
046100140605     C                   add       Pkfvoc        pkf(cevc14)
046200140605     C                   add       vlfvoc        vlf(cevc14)
046300140605     c                   end
046400140605     c                   if        cevc15 = 0
046500140605     c                   leavesr
046600140605     c                   end
046700140605     c                   if        voc(cevc15) <> 0
046800140605     c                   movel     ric(cevc15)   $ric
046900140605     c                   exsr      srric
047000140605     c                   add       cevi15        imp(cevc15)
047100140605     c                   add       1             nsp(cevc15)
047200140605     c                   add       tasncl        ncl(cevc15)
047300140605     C                   add       Pkfvoc        pkf(cevc15)
047400140605     C                   add       vlfvoc        vlf(cevc15)
047500140605     c                   end
047600140605     c                   if        cevc16 = 0
047700140605     c                   leavesr
047800140605     c                   end
047900140605     c                   if        voc(cevc16) <> 0
048000140605     c                   movel     ric(cevc16)   $ric
048100140605     c                   exsr      srric
048200140605     c                   add       cevi16        imp(cevc16)
048300140605     c                   add       1             nsp(cevc16)
048400140605     c                   add       tasncl        ncl(cevc16)
048500140605     C                   add       Pkfvoc        pkf(cevc16)
048600140605     C                   add       vlfvoc        vlf(cevc16)
048700140605     c                   end
048800140605     c                   if        cevc17 = 0
048900140605     c                   leavesr
049000140605     c                   end
049100140605     c                   if        voc(cevc17)<> 0
049200140605     c                   movel     ric(cevc17)   $ric
049300140605     c                   exsr      srric
049400140605     c                   add       cevi17        imp(cevc17)
049500140605     c                   add       1             nsp(cevc17)
049600140605     c                   add       tasncl        ncl(cevc17)
049700140605     C                   add       Pkfvoc        pkf(cevc17)
049800140605     C                   add       vlfvoc        vlf(cevc17)
049900140605     c                   end
050000140605     c                   if        cevc18 = 0
050100140605     c                   leavesr
050200140605     c                   end
050300140605     c                   if        voc(cevc18) <> 0
050400140605     c                   movel     ric(cevc18)   $ric
050500140605     c                   exsr      srric
050600140605     c                   add       cevi18        imp(cevc18)
050700140605     c                   add       1             nsp(cevc18)
050800140605     c                   add       tasncl        ncl(cevc18)
050900140605     C                   add       Pkfvoc        pkf(cevc18)
051000140605     C                   add       vlfvoc        vlf(cevc18)
051100140605     c                   end
051200140605     c                   if        cevc19 = 0
051300140605     c                   leavesr
051400140605     c                   end
051500140605     c                   if        voc(cevc19) <> 0
051600140605     c                   movel     ric(cevc19)   $ric
051700140605     c                   exsr      srric
051800140605     c                   add       cevi19        imp(cevc19)
051900140605     c                   add       1             nsp(cevc19)
052000140605     c                   add       tasncl        ncl(cevc19)
052100140605     C                   add       Pkfvoc        pkf(cevc19)
052200140605     C                   add       vlfvoc        vlf(cevc19)
052300140605     c                   end
052400140605     c                   if        cevc20 = 0
052500140605     c                   leavesr
052600140605     c                   end
052700140605     c                   if        voc(cevc20)<> 0
052800140605     c                   movel     ric(cevc20)   $ric
052900140605     c                   exsr      srric
053000140605     c                   add       cevi20        imp(cevc20)
053100140605     c                   add       1             nsp(cevc20)
053200140605     c                   add       tasncl        ncl(cevc20)
053300140605     C                   add       Pkfvoc        pkf(cevc20)
053400140605     C                   add       vlfvoc        vlf(cevc20)
053500140605     c                   end
053600140605     c                   if        cevc21 = 0
053700140605     c                   leavesr
053800140605     c                   end
053900140605     c                   if        voc(cevc21)<> 0
054000140605     c                   movel     ric(cevc21)   $ric
054100140605     c                   exsr      srric
054200140605     c                   add       cevi21        imp(cevc21)
054300140605     c                   add       1             nsp(cevc21)
054400140605     c                   add       tasncl        ncl(cevc21)
054500140605     C                   add       Pkfvoc        pkf(cevc21)
054600140605     C                   add       vlfvoc        vlf(cevc21)
054700140605     c                   end
054800140605     c                   if        cevc22 = 0
054900140605     c                   leavesr
055000140605     c                   end
055100140605     c                   if        voc(cevc22) <> 0
055200140605     c                   movel     ric(cevc22)   $ric
055300140605     c                   exsr      srric
055400140605     c                   add       cevi22        imp(cevc22)
055500140605     c                   add       1             nsp(cevc22)
055600140605     c                   add       tasncl        ncl(cevc22)
055700140605     C                   add       Pkfvoc        pkf(cevc22)
055800140605     C                   add       vlfvoc        vlf(cevc22)
055900140605     c                   end
056000140605     c                   if        cevc23 = 0
056100140605     c                   leavesr
056200140605     c                   end
056300140605     c                   if        voc(cevc23) <> 0
056400140605     c                   movel     ric(cevc23)   $ric
056500140605     c                   exsr      srric
056600140605     c                   add       cevi23        imp(cevc23)
056700140605     c                   add       1             nsp(cevc23)
056800140605     c                   add       tasncl        ncl(cevc23)
056900140605     C                   add       Pkfvoc        pkf(cevc23)
057000140605     C                   add       vlfvoc        vlf(cevc23)
057100140605     c                   end
057200140605     c                   if        cevc24 = 0
057300140605     c                   leavesr
057400140605     c                   end
057500140605     c                   if        voc(cevc24) <> 0
057600140605     c                   movel     ric(cevc24)   $ric
057700140605     c                   exsr      srric
057800140605     c                   add       cevi24        imp(cevc24)
057900140605     c                   add       1             nsp(cevc24)
058000140605     c                   add       tasncl        ncl(cevc24)
058100140605     C                   add       Pkfvoc        pkf(cevc24)
058200140605     C                   add       vlfvoc        vlf(cevc24)
058300140605     c                   end
058400140605     c                   if        cevc25 = 0
058500140605     c                   leavesr
058600140605     c                   end
058700140605     c                   if        voc(cevc25)<> 0
058800140605     c                   movel     ric(cevc25)   $ric
058900140605     c                   exsr      srric
059000140605     c                   add       cevi25        imp(cevc25)
059100140605     c                   add       1             nsp(cevc25)
059200140605     c                   add       tasncl        ncl(cevc25)
059300140605     C                   add       Pkfvoc        pkf(cevc25)
059400140605     C                   add       vlfvoc        vlf(cevc25)
059500140605     c                   end
059600140605     c                   if        cevc26 = 0
059700140605     c                   leavesr
059800140605     c                   end
059900140605     c                   if        voc(cevc26)  <> 0
060000140605     c                   movel     ric(cevc26)   $ric
060100140605     c                   exsr      srric
060200140605     c                   add       cevi26        imp(cevc26)
060300140605     c                   add       1             nsp(cevc26)
060400140605     c                   add       tasncl        ncl(cevc26)
060500140605     C                   add       Pkfvoc        pkf(cevc26)
060600140605     C                   add       vlfvoc        vlf(cevc26)
060700140605     c                   end
060800140605     c                   if        cevc27 = 0
060900140605     c                   leavesr
061000140605     c                   end
061100140605     c                   if        voc(cevc27)<> 0
061200140605     c                   movel     ric(cevc27)   $ric
061300140605     c                   exsr      srric
061400140605     c                   add       cevi27        imp(cevc27)
061500140605     c                   add       1             nsp(cevc27)
061600140605     c                   add       tasncl        ncl(cevc27)
061700140605     C                   add       Pkfvoc        pkf(cevc27)
061800140605     C                   add       vlfvoc        vlf(cevc27)
061900140605     c                   end
062000140605     c                   if        cevc28 = 0
062100140605     c                   leavesr
062200140605     c                   end
062300140605     c                   if        voc(cevc28) <> 0
062400140605     c                   movel     ric(cevc28)   $ric
062500140605     c                   exsr      srric
062600140605     c                   add       cevi28        imp(cevc28)
062700140605     c                   add       1             nsp(cevc28)
062800140605     c                   add       tasncl        ncl(cevc28)
062900140605     C                   add       Pkfvoc        pkf(cevc28)
063000140605     C                   add       vlfvoc        vlf(cevc28)
063100140605     c                   end
063200140605     c                   if        cevc29 = 0
063300140605     c                   leavesr
063400140605     c                   end
063500140605     c                   if        voc(cevc29)<> 0
063600140605     c                   movel     ric(cevc29)   $ric
063700140605     c                   exsr      srric
063800140605     c                   add       cevi29        imp(cevc29)
063900140605     c                   add       1             nsp(cevc29)
064000140605     c                   add       tasncl        ncl(cevc29)
064100140605     C                   add       Pkfvoc        pkf(cevc29)
064200140605     C                   add       vlfvoc        vlf(cevc29)
064300140605     c                   end
064400140605     c                   if        cevc30 = 0
064500140605     c                   leavesr
064600140605     c                   end
064700140605     c                   if        voc(cevc30)  <> 0
064800140605     c                   movel     ric(cevc30)   $ric
064900140605     c                   exsr      srric
065000140605     c                   add       cevi30        imp(cevc30)
065100140605     c                   add       1             nsp(cevc30)
065200140605     c                   add       tasncl        ncl(cevc30)
065300140605     C                   add       Pkfvoc        pkf(cevc30)
065400140605     C                   add       vlfvoc        vlf(cevc30)
065500140605     c                   end
065600140605     c                   if        cevc31 = 0
065700140605     c                   leavesr
065800140605     c                   end
065900140605     c                   if        voc(cevc31)  <> 0
066000140605     c                   movel     ric(cevc31)   $ric
066100140605     c                   exsr      srric
066200140605     c                   add       cevi31        imp(cevc31)
066300140605     c                   add       1             nsp(cevc31)
066400140605     c                   add       tasncl        ncl(cevc31)
066500140605     C                   add       Pkfvoc        pkf(cevc31)
066600140605     C                   add       vlfvoc        vlf(cevc31)
066700140605     c                   end
066800140605     c                   if        cevc32 = 0
066900140605     c                   leavesr
067000140605     c                   end
067100140605     c                   if        voc(cevc32) <> 0
067200140605     c                   movel     ric(cevc32)   $ric
067300140605     c                   exsr      srric
067400140605     c                   add       cevi32        imp(cevc32)
067500140605     c                   add       1             nsp(cevc32)
067600140605     c                   add       tasncl        ncl(cevc32)
067700140605     C                   add       Pkfvoc        pkf(cevc32)
067800140605     C                   add       vlfvoc        vlf(cevc32)
067900140605     c                   end
068000140605     c                   if        cevc33 = 0
068100140605     c                   leavesr
068200140605     c                   end
068300140605     c                   if        voc(cevc33)<> 0
068400140605     c                   movel     ric(cevc33)   $ric
068500140605     c                   exsr      srric
068600140605     c                   add       cevi33        imp(cevc33)
068700140605     c                   add       1             nsp(cevc33)
068800140605     c                   add       tasncl        ncl(cevc33)
068900140605     C                   add       Pkfvoc        pkf(cevc33)
069000140605     C                   add       vlfvoc        vlf(cevc33)
069100140605     c                   end
069200140605     c                   if        cevc34 = 0
069300140605     c                   leavesr
069400140605     c                   end
069500140605     c                   if        voc(cevc34)<> 0
069600140605     c                   movel     ric(cevc34)   $ric
069700140605     c                   exsr      srric
069800140605     c                   add       cevi34        imp(cevc34)
069900140605     c                   add       1             nsp(cevc34)
070000140605     c                   add       tasncl        ncl(cevc34)
070100140605     C                   add       Pkfvoc        pkf(cevc34)
070200140605     C                   add       vlfvoc        vlf(cevc34)
070300140605     c                   end
070400140605     c                   if        cevc35 = 0
070500140605     c                   leavesr
070600140605     c                   end
070700140605     c                   if        voc(cevc35) <> 0
070800140605     c                   movel     ric(cevc35)   $ric
070900140605     c                   exsr      srric
071000140605     c                   add       cevi35        imp(cevc35)
071100140605     c                   add       1             nsp(cevc35)
071200140605     c                   add       tasncl        ncl(cevc35)
071300140605     C                   add       Pkfvoc        pkf(cevc35)
071400140605     C                   add       vlfvoc        vlf(cevc35)
071500140605     c                   end
071600140605     c                   if        cevc36 = 0
071700140605     c                   leavesr
071800140605     c                   end
071900140605     c                   if        voc(cevc36) <> 0
072000140605     c                   movel     ric(cevc36)   $ric
072100140605     c                   exsr      srric
072200140605     c                   add       cevi36        imp(cevc36)
072300140605     c                   add       1             nsp(cevc36)
072400140605     c                   add       tasncl        ncl(cevc36)
072500140605     C                   add       Pkfvoc        pkf(cevc36)
072600140605     C                   add       vlfvoc        vlf(cevc36)
072700140605     c                   end
072800140605     c                   if        cevc37 = 0
072900140605     c                   leavesr
073000140605     c                   end
073100140605     c                   if        voc(cevc37)<> 0
073200140605     c                   movel     ric(cevc37)   $ric
073300140605     c                   exsr      srric
073400140605     c                   add       cevi37        imp(cevc37)
073500140605     c                   add       1             nsp(cevc37)
073600140605     c                   add       tasncl        ncl(cevc37)
073700140605     C                   add       Pkfvoc        pkf(cevc37)
073800140605     C                   add       vlfvoc        vlf(cevc37)
073900140605     c                   end
074000140605     c                   if        cevc38 = 0
074100140605     c                   leavesr
074200140605     c                   end
074300140605     c                   if        voc(cevc38)<> 0
074400140605     c                   movel     ric(cevc38)   $ric
074500140605     c                   exsr      srric
074600140605     c                   add       cevi38        imp(cevc38)
074700140605     c                   add       1             nsp(cevc38)
074800140605     c                   add       tasncl        ncl(cevc38)
074900140605     C                   add       Pkfvoc        pkf(cevc38)
075000140605     C                   add       vlfvoc        vlf(cevc38)
075100140605     c                   end
075200140605     c                   if        cevc39 = 0
075300140605     c                   leavesr
075400140605     c                   end
075500140605     c                   if        voc(cevc39)<> 0
075600140605     c                   movel     ric(cevc39)   $ric
075700140605     c                   exsr      srric
075800140605     c                   add       cevi39        imp(cevc39)
075900140605     c                   add       1             nsp(cevc39)
076000140605     c                   add       tasncl        ncl(cevc39)
076100140605     C                   add       Pkfvoc        pkf(cevc39)
076200140605     C                   add       vlfvoc        vlf(cevc39)
076300140605     c                   end
076400140605     c                   if        cevc40 = 0
076500140605     c                   leavesr
076600140605     c                   end
076700140605     c                   if        voc(cevc40) <> 0
076800140605     c                   movel     ric(cevc40)   $ric
076900140605     c                   exsr      srric
077000140605     c                   add       cevi40        imp(cevc40)
077100140605     c                   add       1             nsp(cevc40)
077200140605     c                   add       tasncl        ncl(cevc40)
077300140605     C                   add       Pkfvoc        pkf(cevc40)
077400140605     C                   add       vlfvoc        vlf(cevc40)
077500140605     c                   end
077600140605     c                   if        cevc41 = 0
077700140605     c                   leavesr
077800140605     c                   end
077900140605     c                   if        voc(cevc41) <> 0
078000140605     c                   movel     ric(cevc41)   $ric
078100140605     c                   exsr      srric
078200140605     c                   add       cevi41        imp(cevc41)
078300140605     c                   add       1             nsp(cevc41)
078400140605     c                   add       tasncl        ncl(cevc41)
078500140605     C                   add       Pkfvoc        pkf(cevc41)
078600140605     C                   add       vlfvoc        vlf(cevc41)
078700140605     c                   end
078800140605     c                   if        cevc42 = 0
078900140605     c                   leavesr
079000140605     c                   end
079100140605     c                   if        voc(cevc42)<> 0
079200140605     c                   movel     ric(cevc42)   $ric
079300140605     c                   exsr      srric
079400140605     c                   add       cevi42        imp(cevc42)
079500140605     c                   add       1             nsp(cevc42)
079600140605     c                   add       tasncl        ncl(cevc42)
079700140605     C                   add       Pkfvoc        pkf(cevc42)
079800140605     C                   add       vlfvoc        vlf(cevc42)
079900140605     c                   end
080000140605     c                   if        cevc43 = 0
080100140605     c                   leavesr
080200140605     c                   end
080300140605     c                   if        voc(cevc43) <> 0
080400140605     c                   movel     ric(cevc43)   $ric
080500140605     c                   exsr      srric
080600140605     c                   add       cevi43        imp(cevc43)
080700140605     c                   add       1             nsp(cevc43)
080800140605     c                   add       tasncl        ncl(cevc43)
080900140605     C                   add       Pkfvoc        pkf(cevc43)
081000140605     C                   add       vlfvoc        vlf(cevc43)
081100140605     c                   end
081200140605     c                   if        cevc44 = 0
081300140605     c                   leavesr
081400140605     c                   end
081500140605     c                   if        voc(cevc44) <> 0
081600140605     c                   movel     ric(cevc44)   $ric
081700140605     c                   exsr      srric
081800140605     c                   add       cevi44        imp(cevc44)
081900140605     c                   add       1             nsp(cevc44)
082000140605     c                   add       tasncl        ncl(cevc44)
082100140605     C                   add       Pkfvoc        pkf(cevc44)
082200140605     C                   add       vlfvoc        vlf(cevc44)
082300140605     c                   end
082400140605     c                   if        cevc45 = 0
082500140605     c                   leavesr
082600140605     c                   end
082700140605     c                   if        voc(cevc45) <> 0
082800140605     c                   movel     ric(cevc45)   $ric
082900140605     c                   exsr      srric
083000140605     c                   add       cevi45        imp(cevc45)
083100140605     c                   add       1             nsp(cevc45)
083200140605     c                   add       tasncl        ncl(cevc45)
083300140605     C                   add       Pkfvoc        pkf(cevc45)
083400140605     C                   add       vlfvoc        vlf(cevc45)
083500140605     c                   end
083600140605     c                   if        cevc46 = 0
083700140605     c                   leavesr
083800140605     c                   end
083900140605     c                   if        voc(cevc46) <> 0
084000140605     c                   movel     ric(cevc46)   $ric
084100140605     c                   exsr      srric
084200140605     c                   add       cevi46        imp(cevc46)
084300140605     c                   add       1             nsp(cevc46)
084400140605     c                   add       tasncl        ncl(cevc46)
084500140605     C                   add       Pkfvoc        pkf(cevc46)
084600140605     C                   add       vlfvoc        vlf(cevc46)
084700140605     c                   end
084800140605     c                   if        cevc47 = 0
084900140605     c                   leavesr
085000140605     c                   end
085100140605     c                   if        voc(cevc47)  <> 0
085200140605     c                   movel     ric(cevc47)   $ric
085300140605     c                   exsr      srric
085400140605     c                   add       cevi47        imp(cevc47)
085500140605     c                   add       1             nsp(cevc47)
085600140605     c                   add       tasncl        ncl(cevc47)
085700140605     C                   add       Pkfvoc        pkf(cevc47)
085800140605     C                   add       vlfvoc        vlf(cevc47)
085900140605     c                   end
086000140605     c                   if        cevc48 = 0
086100140605     c                   leavesr
086200140605     c                   end
086300140605     c                   if        voc(cevc48)<> 0
086400140605     c                   movel     ric(cevc48)   $ric
086500140605     c                   exsr      srric
086600140605     c                   add       cevi48        imp(cevc48)
086700140605     c                   add       1             nsp(cevc48)
086800140605     c                   add       tasncl        ncl(cevc48)
086900140605     C                   add       Pkfvoc        pkf(cevc48)
087000140605     C                   add       vlfvoc        vlf(cevc48)
087100140605     c                   end
087200140605     c                   if        cevc49 = 0
087300140605     c                   leavesr
087400140605     c                   end
087500140605     c                   if        voc(cevc49) <> 0
087600140605     c                   movel     ric(cevc49)   $ric
087700140605     c                   exsr      srric
087800140605     c                   add       cevi49        imp(cevc49)
087900140605     c                   add       1             nsp(cevc49)
088000140605     c                   add       tasncl        ncl(cevc49)
088100140605     C                   add       Pkfvoc        pkf(cevc49)
088200140605     C                   add       vlfvoc        vlf(cevc49)
088300140605     c                   end
088400140605     c                   if        cevc50 = 0
088500140605     c                   leavesr
088600140605     c                   end
088700140605     c                   if        voc(cevc50) <> 0
088800140605     c                   movel     ric(cevc50)   $ric
088900140605     c                   exsr      srric
089000140605     c                   add       cevi50        imp(cevc50)
089100140605     c                   add       1             nsp(cevc50)
089200140605     c                   add       tasncl        ncl(cevc50)
089300140605     C                   add       Pkfvoc        pkf(cevc50)
089400140605     C                   add       vlfvoc        vlf(cevc50)
089500140605     c                   end
089600140605     c* se sono tutte piene le voci aggancio anche il c2v
089700140605     c                   if        cevc50 <> 0
089800140605     c                   exsr      src2v
089900140605     c                   end
090000140605     C                   ENDSR
090100140604     c***********************************************************
090200140604     C     src2v         BEGSR
090300140604     c     ktas          chain     ecc2v30c
090400140604     c                   if        %found(ecc2v30c)
090500140605     c                   if        C2Vc51 = 0
090600140605     c                   leavesr
090700140605     c                   end
090800140605     c                   if        voc(C2Vc51)<> 0
090900140604     c                   movel     ric(C2Vc51)   $ric
091000140604     c                   exsr      srric
091100140605     c                   add       C2Vi51        imp(C2Vc51)
091200140604     c                   add       1             nsp(C2Vc51)
091300140604     c                   add       tasncl        ncl(C2Vc51)
091400140604     C                   add       Pkfvoc        pkf(C2Vc51)
091500140604     C                   add       vlfvoc        vlf(C2Vc51)
091600140604     c                   end
091700140605     c                   if        C2Vc52 = 0
091800140605     c                   leavesr
091900140605     c                   end
092000140605     c                   if        voc(C2Vc52) <> 0
092100140604     c                   movel     ric(C2Vc52)   $ric
092200140604     c                   exsr      srric
092300140605     c                   add       C2Vi52        imp(C2Vc52)
092400140604     c                   add       1             nsp(C2Vc52)
092500140604     c                   add       tasncl        ncl(C2Vc52)
092600140604     C                   add       Pkfvoc        pkf(C2Vc52)
092700140604     C                   add       vlfvoc        vlf(C2Vc52)
092800140604     c                   end
092900140605     c                   if        C2Vc53 = 0
093000140605     c                   leavesr
093100140605     c                   end
093200140605     c                   if        voc(C2Vc53)<> 0
093300140604     c                   movel     ric(C2Vc53)   $ric
093400140604     c                   exsr      srric
093500140605     c                   add       C2Vi53        imp(C2Vc53)
093600140604     c                   add       1             nsp(C2Vc53)
093700140604     c                   add       tasncl        ncl(C2Vc53)
093800140604     C                   add       Pkfvoc        pkf(C2Vc53)
093900140604     C                   add       vlfvoc        vlf(C2Vc53)
094000140604     c                   end
094100140605     c                   if        C2Vc54 = 0
094200140605     c                   leavesr
094300140605     c                   end
094400140605     c                   if        voc(C2Vc54)<> 0
094500140604     c                   movel     ric(C2Vc54)   $ric
094600140604     c                   exsr      srric
094700140605     c                   add       C2Vi54        imp(C2Vc54)
094800140604     c                   add       1             nsp(C2Vc54)
094900140604     c                   add       tasncl        ncl(C2Vc54)
095000140604     C                   add       Pkfvoc        pkf(C2Vc54)
095100140604     C                   add       vlfvoc        vlf(C2Vc54)
095200140604     c                   end
095300140605     c                   if        C2Vc55 = 0
095400140605     c                   leavesr
095500140605     c                   end
095600140605     c                   if        voc(C2Vc55)<> 0
095700140604     c                   movel     ric(C2Vc55)   $ric
095800140604     c                   exsr      srric
095900140605     c                   add       C2Vi55        imp(C2Vc55)
096000140604     c                   add       1             nsp(C2Vc55)
096100140604     c                   add       tasncl        ncl(C2Vc55)
096200140604     C                   add       Pkfvoc        pkf(C2Vc55)
096300140604     C                   add       vlfvoc        vlf(C2Vc55)
096400140604     c                   end
096500140605     c                   if        C2Vc56 = 0
096600140605     c                   leavesr
096700140605     c                   end
096800140605     c                   if        voc(C2Vc56) <> 0
096900140604     c                   movel     ric(C2Vc56)   $ric
097000140604     c                   exsr      srric
097100140605     c                   add       C2Vi56        imp(C2Vc56)
097200140604     c                   add       1             nsp(C2Vc56)
097300140604     c                   add       tasncl        ncl(C2Vc56)
097400140604     C                   add       Pkfvoc        pkf(C2Vc56)
097500140604     C                   add       vlfvoc        vlf(C2Vc56)
097600140604     c                   end
097700140605     c                   if        C2Vc57 = 0
097800140605     c                   leavesr
097900140605     c                   end
098000140605     c                   if        voc(C2Vc57)<> 0
098100140604     c                   movel     ric(C2Vc57)   $ric
098200140604     c                   exsr      srric
098300140605     c                   add       C2Vi57        imp(C2Vc57)
098400140604     c                   add       1             nsp(C2Vc57)
098500140604     c                   add       tasncl        ncl(C2Vc57)
098600140604     C                   add       Pkfvoc        pkf(C2Vc57)
098700140604     C                   add       vlfvoc        vlf(C2Vc57)
098800140604     c                   end
098900140605     c                   if        C2Vc58 = 0
099000140605     c                   leavesr
099100140605     c                   end
099200140605     c                   if        voc(C2Vc58) <> 0
099300140604     c                   movel     ric(C2Vc58)   $ric
099400140604     c                   exsr      srric
099500140605     c                   add       C2Vi58        imp(C2Vc58)
099600140604     c                   add       1             nsp(C2Vc58)
099700140604     c                   add       tasncl        ncl(C2Vc58)
099800140604     C                   add       Pkfvoc        pkf(C2Vc58)
099900140604     C                   add       vlfvoc        vlf(C2Vc58)
100000140604     c                   end
100100140605     c                   if        C2Vc59 = 0
100200140605     c                   leavesr
100300140605     c                   end
100400140605     c                   if        voc(C2Vc59)<> 0
100500140604     c                   movel     ric(C2Vc59)   $ric
100600140604     c                   exsr      srric
100700140605     c                   add       C2Vi59        imp(C2Vc59)
100800140604     c                   add       1             nsp(C2Vc59)
100900140604     c                   add       tasncl        ncl(C2Vc59)
101000140604     C                   add       Pkfvoc        pkf(C2Vc59)
101100140604     C                   add       vlfvoc        vlf(C2Vc59)
101200140604     c                   end
101300140605     c                   if        C2Vc60 = 0
101400140605     c                   leavesr
101500140605     c                   end
101600140605     c                   if        voc(C2Vc60) <> 0
101700140604     c                   movel     ric(C2Vc60)   $ric
101800140604     c                   exsr      srric
101900140605     c                   add       C2Vi60        imp(C2Vc60)
102000140604     c                   add       1             nsp(C2Vc60)
102100140604     c                   add       tasncl        ncl(C2Vc60)
102200140604     C                   add       Pkfvoc        pkf(C2Vc60)
102300140604     C                   add       vlfvoc        vlf(C2Vc60)
102400140604     c                   end
102500140605     c                   if        C2Vc61 = 0
102600140605     c                   leavesr
102700140605     c                   end
102800140605     c                   if        voc(C2Vc61) <> 0
102900140604     c                   movel     ric(C2Vc61)   $ric
103000140604     c                   exsr      srric
103100140605     c                   add       C2Vi61        imp(C2Vc61)
103200140604     c                   add       1             nsp(C2Vc61)
103300140604     c                   add       tasncl        ncl(C2Vc61)
103400140604     C                   add       Pkfvoc        pkf(C2Vc61)
103500140604     C                   add       vlfvoc        vlf(C2Vc61)
103600140604     c                   end
103700140605     c                   if        C2Vc62 = 0
103800140605     c                   leavesr
103900140605     c                   end
104000140605     c                   if        voc(C2Vc62)<> 0
104100140604     c                   movel     ric(C2Vc62)   $ric
104200140604     c                   exsr      srric
104300140605     c                   add       C2Vi62        imp(C2Vc62)
104400140604     c                   add       1             nsp(C2Vc62)
104500140604     c                   add       tasncl        ncl(C2Vc62)
104600140604     C                   add       Pkfvoc        pkf(C2Vc62)
104700140604     C                   add       vlfvoc        vlf(C2Vc62)
104800140604     c                   end
104900140605     c                   if        C2Vc63 = 0
105000140605     c                   leavesr
105100140605     c                   end
105200140605     c                   if        voc(C2Vc63) <> 0
105300140604     c                   movel     ric(C2Vc63)   $ric
105400140604     c                   exsr      srric
105500140605     c                   add       C2Vi63        imp(C2Vc63)
105600140604     c                   add       1             nsp(C2Vc63)
105700140604     c                   add       tasncl        ncl(C2Vc63)
105800140604     C                   add       Pkfvoc        pkf(C2Vc63)
105900140604     C                   add       vlfvoc        vlf(C2Vc63)
106000140604     c                   end
106100140605     c                   if        C2Vc64 = 0
106200140605     c                   leavesr
106300140605     c                   end
106400140605     c                   if        voc(C2Vc64)<> 0
106500140604     c                   add       C2Vi64        imp(C2Vc64)
106600140604     c                   movel     ric(C2Vc64)   $ric
106700140604     c                   exsr      srric
106800140604     c                   add       1             nsp(C2Vc64)
106900140604     c                   add       tasncl        ncl(C2Vc64)
107000140604     C                   add       Pkfvoc        pkf(C2Vc64)
107100140604     C                   add       vlfvoc        vlf(C2Vc64)
107200140604     c                   end
107300140605     c                   if        C2Vc65 = 0
107400140605     c                   leavesr
107500140605     c                   end
107600140605     c                   if        voc(C2Vc65) <> 0
107700140604     c                   movel     ric(C2Vc65)   $ric
107800140604     c                   exsr      srric
107900140605     c                   add       C2Vi65        imp(C2Vc65)
108000140604     c                   add       1             nsp(C2Vc65)
108100140604     c                   add       tasncl        ncl(C2Vc65)
108200140604     C                   add       Pkfvoc        pkf(C2Vc65)
108300140604     C                   add       vlfvoc        vlf(C2Vc65)
108400140604     c                   end
108500140605     c                   if        C2Vc66 = 0
108600140605     c                   leavesr
108700140605     c                   end
108800140605     c                   if        voc(C2Vc66)<> 0
108900140604     c                   movel     ric(C2Vc66)   $ric
109000140604     c                   exsr      srric
109100140605     c                   add       C2Vi66        imp(C2Vc66)
109200140604     c                   add       1             nsp(C2Vc66)
109300140604     c                   add       tasncl        ncl(C2Vc66)
109400140604     C                   add       Pkfvoc        pkf(C2Vc66)
109500140604     C                   add       vlfvoc        vlf(C2Vc66)
109600140604     c                   end
109700140605     c                   if        C2Vc67 = 0
109800140605     c                   leavesr
109900140605     c                   end
110000140605     c                   if        voc(C2Vc67) <> 0
110100140604     c                   movel     ric(C2Vc67)   $ric
110200140604     c                   exsr      srric
110300140605     c                   add       C2Vi67        imp(C2Vc67)
110400140604     c                   add       1             nsp(C2Vc67)
110500140604     c                   add       tasncl        ncl(C2Vc67)
110600140604     C                   add       Pkfvoc        pkf(C2Vc67)
110700140604     C                   add       vlfvoc        vlf(C2Vc67)
110800140604     c                   end
110900140605     c                   if        C2Vc68 = 0
111000140605     c                   leavesr
111100140605     c                   end
111200140605     c                   if        voc(C2Vc68)<> 0
111300140604     c                   movel     ric(C2Vc68)   $ric
111400140604     c                   exsr      srric
111500140605     c                   add       C2Vi68        imp(C2Vc68)
111600140604     c                   add       1             nsp(C2Vc68)
111700140604     c                   add       tasncl        ncl(C2Vc68)
111800140604     C                   add       Pkfvoc        pkf(C2Vc68)
111900140604     C                   add       vlfvoc        vlf(C2Vc68)
112000140604     c                   end
112100140605     c                   if        C2Vc69 = 0
112200140605     c                   leavesr
112300140605     c                   end
112400140605     c                   if        voc(C2Vc69) <> 0
112500140604     c                   movel     ric(C2Vc69)   $ric
112600140604     c                   exsr      srric
112700140605     c                   add       C2Vi69        imp(C2Vc69)
112800140604     c                   add       1             nsp(C2Vc69)
112900140604     c                   add       tasncl        ncl(C2Vc69)
113000140604     C                   add       Pkfvoc        pkf(C2Vc69)
113100140604     C                   add       vlfvoc        vlf(C2Vc69)
113200140604     c                   end
113300140605     c                   if        C2Vc70 = 0
113400140605     c                   leavesr
113500140605     c                   end
113600140605     c                   if        voc(C2Vc70) <> 0
113700140604     c                   movel     ric(C2Vc70)   $ric
113800140604     c                   exsr      srric
113900140605     c                   add       C2Vi70        imp(C2Vc70)
114000140604     c                   add       1             nsp(C2Vc70)
114100140604     c                   add       tasncl        ncl(C2Vc70)
114200140604     C                   add       Pkfvoc        pkf(C2Vc70)
114300140604     C                   add       vlfvoc        vlf(C2Vc70)
114400140604     c                   end
114500140605     c                   if        C2Vc71 = 0
114600140605     c                   leavesr
114700140605     c                   end
114800140605     c                   if        voc(C2Vc71) <> 0
114900140604     c                   movel     ric(C2Vc71)   $ric
115000140604     c                   exsr      srric
115100140605     c                   add       C2Vi71        imp(C2Vc71)
115200140604     c                   add       1             nsp(C2Vc71)
115300140604     c                   add       tasncl        ncl(C2Vc71)
115400140604     C                   add       Pkfvoc        pkf(C2Vc71)
115500140604     C                   add       vlfvoc        vlf(C2Vc71)
115600140604     c                   end
115700140605     c                   if        C2Vc72 = 0
115800140605     c                   leavesr
115900140605     c                   end
116000140605     c                   if        voc(C2Vc72)<> 0
116100140604     c                   movel     ric(C2Vc72)   $ric
116200140604     c                   exsr      srric
116300140605     c                   add       C2Vi72        imp(C2Vc72)
116400140605     c                   add       1             nsp(C2Vc72)
116500140604     c                   add       tasncl        ncl(C2Vc72)
116600140604     C                   add       Pkfvoc        pkf(C2Vc72)
116700140604     C                   add       vlfvoc        vlf(C2Vc72)
116800140604     c                   end
116900140605     c                   if        C2Vc73 = 0
117000140605     c                   leavesr
117100140605     c                   end
117200140605     c                   if        voc(C2Vc73)<> 0
117300140604     c                   movel     ric(C2Vc73)   $ric
117400140604     c                   exsr      srric
117500140605     c                   add       C2Vi73        imp(C2Vc73)
117600140604     c                   add       1             nsp(C2Vc73)
117700140604     c                   add       tasncl        ncl(C2Vc73)
117800140604     C                   add       Pkfvoc        pkf(C2Vc73)
117900140604     C                   add       vlfvoc        vlf(C2Vc73)
118000140604     c                   end
118100140605     c                   if        C2Vc74 = 0
118200140605     c                   leavesr
118300140605     c                   end
118400140605     c                   if        voc(C2Vc74)<> 0
118500140604     c                   movel     ric(C2Vc74)   $ric
118600140604     c                   exsr      srric
118700140605     c                   add       C2Vi74        imp(C2Vc74)
118800140604     c                   add       1             nsp(C2Vc74)
118900140604     c                   add       tasncl        ncl(C2Vc74)
119000140604     C                   add       Pkfvoc        pkf(C2Vc74)
119100140604     C                   add       vlfvoc        vlf(C2Vc74)
119200140604     c                   end
119300140605     c                   if        C2Vc75 = 0
119400140605     c                   leavesr
119500140605     c                   end
119600140605     c                   if        voc(C2Vc75)<> 0
119700140604     c                   movel     ric(C2Vc75)   $ric
119800140604     c                   exsr      srric
119900140605     c                   add       C2Vi75        imp(C2Vc75)
120000140604     c                   add       1             nsp(C2Vc75)
120100140604     c                   add       tasncl        ncl(C2Vc75)
120200140604     C                   add       Pkfvoc        pkf(C2Vc75)
120300140604     C                   add       vlfvoc        vlf(C2Vc75)
120400140604     c                   end
120500140604     c                   end
120600140604     c                   endsr
120700140606     C***********************************************************************
120800140606     C*   GESLEG - GESTIONE LEGAMI BOLLA
120900140606     C***********************************************************************
121000140606     C     GESLEG        BEGSR
121100140606     C                   MOVEL     titasds       titasdss
121200140606     C                   Z-ADD     tasAAS        LBLAAN
121300140606     C                   Z-ADD     tasLNP        LBLLPN
121400140606     C                   Z-ADD     tasNRS        LBLNRN
121500140606     C                   Z-ADD     tasNSP        LBLNSN
121600140609     C     KEYLBL        setll     FNLBL01L                               99
121700140609     c                   if        not *in99
121800140609     c                   leavesr
121900140609     c                   end
122000140606     C* CICLO PER TROVARE LA BOLLA ORIGINALE (LA PRIMA MAMMA)
122100140606     C                   DO        *hival
122200140609     C     KEYLBL        reade     FNLBL01L
122300140606     c                   if        %eof(fnlbl01l)
122400140606     c                   leave
122500140606     c                   end
122600140606     C                   Z-ADD     LBLAAP        LBLAAN
122700140606     C                   Z-ADD     LBLLPP        LBLLPN
122800140606     C                   Z-ADD     LBLNRP        LBLNRN
122900140606     C                   Z-ADD     LBLNSP        LBLNSN
123000140606     C                   ENDDO
123100140606     C* LEGGO I DATI DELLA MAMMA
123200140609     c                   if        lblnsp = 0
123300140609     c                   leavesr
123400140609     c                   end
123500140606     C                   Z-ADD     LBLAAP        TASAAS
123600140606     C                   Z-ADD     LBLLPP        TASLNP
123700140606     C                   Z-ADD     LBLNRP        TASNRS
123800140606     C                   Z-ADD     LBLNSP        TASNSP
123900140606     C                   MOVEL     *BLANKS       TASTBL
124000140606     C     KEYTAS        SETLL     TITAS30C                           99
124100140606     C  N99              READ      TITAS30C                               99
124200140606     C     *IN99         IFEQ      '0'
124300140606     C     TASAAS        ANDEQ     LBLAAP
124400140606     C     TASLNP        ANDEQ     LBLLPP
124500140606     C     TASNRS        ANDEQ     LBLNRP
124600140606     C     TASNSP        ANDEQ     LBLNSP
124700140606     c                   eval      wlnp = taslnp
124800140606     c                   eval      wtfp = tastfp
124900140606     C                   ENDIF
125000140606     C                   MOVEL     titasdss      titasds
125100140606     C*
125200140606     C                   ENDSR
