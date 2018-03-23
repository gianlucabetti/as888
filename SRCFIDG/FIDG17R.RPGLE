000100950517     H DECEDIT('0,') DATEDIT(*DMY.)
000200070201     H* FIDG17R *----------------------------------------------------*
000300070322     H*         Lancio assegnazione automatica distinte consegna
000400950517     H*--------------------------------------------------------------*
000500070201     FFidg17D   CF   E             WORKSTN
000600070827     FFIprm01L  uF a E           K DISK
000700050519     Fazorg01L  IF   E           K DISK
000800070410     FFIDST09L  IF   E           K DISK
000900070410     FFIDST08L  IF   E           K DISK
001000070201     F                                     RENAME(fidst000:fidst002)
001100080326     FFidsf02L  IF   E           K DISK
001200950517     D*--------------------------------------------------------------*
001300950517     D*  Schiera errori
001400950517     D*--------------------------------------------------------------*
001500160420     D ERR             S             70    DIM(17) CTDATA PERRCD(1)
001600950517     D*--------------------------------------------------------------*
001700950517     D*  DS
001800950517     D*--------------------------------------------------------------*
001900950517     D KPJBA         E DS
002000950517     D***
002100950517     D* PARAMETRI PER CONTROLLO DATA E TRASFORM IN GIORNI - XSRDA8 -
002200950517     D***
002300950517     D WLBDAT          DS                  INZ
002400950517     D  G02DAT                 1      8  0
002500950517     D  G02INV                 9     16  0
002600950517     D  G02ERR                17     17
002700950517     D  G02TGI                18     22  0
002800950517     D***
002900950517     D* PARAMETRI PER TRASFORM GIORNI IN DATA             - XSRDGI -
003000950517     D***
003100950517     D WGIDAT          DS                  INZ
003200950517     D  GIODAT                 1      8  0
003300950517     D  GIOINV                 9     16  0
003400950517     D  GIOTGI                17     21  0
003500950517     D* DS PER FNLV50F - CONTROLLO FILIALE GESTIONE
003600950517     D FNLV50        E DS                  EXTNAME(FNLV50DS)
003700950517     D*
003800070313     D OG146         E DS                  INZ
003900070828     D OG147         E DS                  INZ
004000050519     D og150         E DS
004100160719     D fidg31ds      E DS
004200070201     D Fidg17ds      E DS
004300030710     D ddatiute      e ds
004400160420     D ddstflr       e ds
004500030710     D azuteds       e ds                  extname(AZUTE00F)
004600030710     D tibs34ds      E DS                  inz
004700141029     D fnlry09ds2    E DS                  inz
004800150513     d TRULVPODS     e ds
004900150513     d   sk_PDA               16    765    inz   dim(250)
005000070627     d savndd          s                   like(wndd)
005100070627     d savnda          s                   like(v1cnda)
005200070627     d savtpm          s                   like(v1ctpm)
005300160420     d savtur1         s                   like(v1ctur1)
005400160420     d savtur2         s                   like(v1ctur2)
005500070627     d savddt          s                   like(wddt)
005600070627     d savfil          s                   like(v1cfil)
005700160719     d $FV6            s              6P 0 dim(600)                             F.V.  x FIDG31R
005800950517     C*--------------------------------------------------------------*
005900950517     C*  Ciclo principale
006000950517     C*--------------------------------------------------------------*
006100950517     C* Inizializzazione prima videata
006200950517     C                   EXSR      INZD01
006300950517     C* Loop di gestione videata
006400950517     C     WFINE         DOUEQ     'S'
006500950517     C     WTPVID        CASEQ     '1'           GESD01
006600950517     C                   END
006700950517     C                   END
006800950517     C* Fine Pgm.
006900950517     C                   SETON                                        LR
007000950517     C*--------------------------------------------------------------*
007100950517     C*  Inizializzazione parametri prima videata
007200950517     C*--------------------------------------------------------------*
007300950522     C     INZD01        BEGSR
007400950517     C*
007500950602     C                   SETOFF                                       0302
007600070824     C                   clear                   V1Cfpp
007700070201     C                   MOVEL     'N'           V1CCLEAR
007800070313     C                   MOVEL     'N'           V1CCLRMAN
007900150513     C                   clear                   V1Cp88
008000150513     c* ricerca P.O. abilitati da tabella VPO per step di test
008100150513     c                   clear                   trulvpods
008200150513     c                   eval      ivpoke1 = 'SQA'
008300150513     c                   call      'TRULVPOR'
008400150513     c                   parm                    trulvpods
008500150513     c                   move      '999'         fgsa              3
008600150513     c     fgsa          lookup    sk_pda                                 37
008700150513     c  n37              move      dutpou        fgsa
008800150513     c  n37fgsa          lookup    sk_pda                                 37
008900150513     c                   if        *in37
009000150513     c                   eval      v1cp88 = 'S'
009100150513     c                   else
009200150513     C                   CLEAR                   V1Cp88
009300150513     c                   end
009400070201     c                   clear                   v1cret
009500050523     C                   MOVEL     *blanks       V1Ctpm
009600070313     C                   Z-ADD     WHHDAT        V1CDDC
009700070824     C                   Z-ADD     0             V1CDta
009800070824     C                   Z-ADD     0             V1Cora
009900950517     C                   MOVEL     *BLANKS       V1CNDD
010000070313     C                   Z-ADD     99999         V1CNDA
010100030910     C     dutlpo        IFEQ      '2'
010200030910     C                   MOVEL     dutpou        V1CFIL
010300030910     C                   ELSE
010400030910     C                   MOVEL     SIMFEL        V1CFIL
010500030910     C                   END
010600050520     C* parcel + messaggerie
010700050520     c                   exsr      srtpm
010800950522     C                   MOVE      V1CFIL        WSVFIL            3 0
010900070201     C                   CLEAR                   Fidg17DS
011000160506     C                   MOVEL     'U'           V1Ctur1
011100160506     C                   MOVEL     ' '           V1Ctur2
011200160506     c                   if        *in09
011300160506     C                   MOVEL     '1'           V1Ctur1
011400160506     C                   MOVEL     '2'           V1Ctur2
011500160506     c                   end
011600950517     C*
011700950517     C                   ENDSR
011800050520     C*--------------------------------------------------------------*
011900050520     C*  parcel + messaggerie
012000050520     C*--------------------------------------------------------------*
012100050520     C     srtpm         BEGSR
012200050520     c* leggo organigramma x gestire ind 10 parcel+messaggerie
012300050520     c     v1cfil        chain     azorg01l
012400050520     c                   if        %found(azorg01l)
012500050520     c                   movel     orgdf0        og150
012600070313     C                   MOVEL     ORGDE6        og146
012700070828     C                   MOVEL     ORGDE7        og147
012800160420     c     §OGturno      comp      'S'                                    09
012900160420     c     §OGPKM        comp      'S'                                    10
013000070828     c     §OGdoada      comp      'S'                                    12
013100070828     c     §OGfppda      comp      'S'                                    11
013200070313     c                   end
013300070313     c* se p.o. non partito esco
013400070313     c                   if        §ogdda = ' '
013500070313     c                   seton                                        lr
013600070313     c                   return
013700070313     c                   end
013800050520     C                   ENDSR
013900950517     C*--------------------------------------------------------------*
014000950517     C*  Gestione prima videata
014100950517     C*--------------------------------------------------------------*
014200950517     C     GESD01        BEGSR
014300950517     C*
014400070201     C                   EXFMT     dg17D01
014500950524     C                   SETOFF                                       284244
014600950517     C                   SETOFF                                       404143
014700070919     C                   SETOFF                                       454647
014800160420     C                   SETOFF                                       4849
014900950522     C                   MOVEL     *BLANKS       WNDT
015000950529     C                   Z-ADD     0             WNDD
015100950517     C*  Fine Pgm.
015200950517     C     *INKC         IFEQ      '1'
015300950517     C                   MOVEL     'S'           WFINE
015400950517     C                   END
015500950614     C*
015600950614     C   51              SETOFF                                       51
015700950614     C*
015800950614     C     *IN50         IFEQ      '1'
015900950614     C     WSVFIL        ORNE      V1CFIL
016000950614     C                   CLEAR                   FNLV50
016100950614     C                   MOVEL     KNMUS         D50PRU
016200950614     C                   MOVEL     V1CFIL        D50FGS
016300950614     C                   CALL      'FNLV50R'
016400950614     C                   PARM                    FNLV50
016500950614     C                   SETOFF                                       50
016600950614     C                   MOVE      V1CFIL        WSVFIL            3 0
016700050520     C* parcel + messaggerie
016800050520     c                   exsr      srtpm
016900050520     c                   end
017000950602     C*  Sblocco data
017100950602     C     *INKA         IFEQ      '1'
017200950602     C                   SETON                                        5103
017300070627     c                   setoff                                       29
017400950602     C                   GOTO      FINVD1
017500950602     C                   END
017600950517     C*  Cambio filiale
017700950517     C     *INKS         IFEQ      '1'
017800950602     C                   SETON                                        5002
017900070627     c                   setoff                                       29
018000950517     C                   GOTO      FINVD1
018100950517     C                   END
018200140924?    C* F19- Richiama pgm Disposizioni via WEB
018300140924     C     *inkt         IFEQ      '1'
018400141029     c                   clear                   fnlry09ds2
018500140924     c                   eval      ilry09tch = 'E'
018600141013     c                   exsr      verdispo
018700141029     c                   if        OLRY09ESal = '1'
018800141001     c                   seton                                        97
018900141001     c                   else
019000141001     c                   setoff                                       97
019100141001     c                   endif
019200141001     c                   goto      finvd1
019300140924     c                   endif
019400950517     C*  Controlli
019500950517     C                   EXSR      CTR01
019600950517     C*  Conferma
019700070201     C                   IF        not *in28 and *inkf
019800950517     C                   EXSR      SBMP60
019900070205     c                   if        not *in28
020000070412     C                   MOVEL     'S'           WFINE
020100070205     c                   end
020200070205     C                   END
020300950517     C*
020400950517     C     FINVD1        ENDSR
020500950522     C*--------------------------------------------------------------*
020600950522     C*  Controlli prima videata
020700950522     C*--------------------------------------------------------------*
020800950522     C     CTR01         BEGSR
020900950522     C*
021000950522     C*  Controllo filiale
021100030710     c                   do
021200140926     c                   setoff                                       97
021300140926     c*
021400140926      * verifica presenza disposizioni web
021500141029     c                   clear                   fnlry09ds2
021600140926     c                   eval      ilry09tch = 'C'
021700141013     c                   exsr      verdispo
021800141029     c                   if        OLRY09ESal = '1' and
021900140926     c                             OLRY09ERR  = *blank
022000140926     c                   seton                                        97
022100140926     c                   endif
022200950522     C     D50ERR        IFNE      ' '
022300950522     C                   SETON                                        2840
022400950522     C                   MOVEL     D50MSG        $MSG
022500030710     C                   leave
022600950522     C                   ENDIF
022700160420     C*  turno
022800160420     c                   if        v1ctur1 = 'U' and v1ctur2 <>'U'
022900160420     c                             and v1ctur2 <>' '
023000160420     C                   SETON                                        4928
023100160420     C                   MOVEL     ERR(16)       $MSG
023200160420     C                   leave
023300160420     c                   end
023400160420     c                   if        v1ctur2 = 'U' and v1ctur1 <>'U'
023500160420     c                             and v1ctur1 <>' '
023600160420     C                   SETON                                        4928
023700160420     C                   MOVEL     ERR(16)       $MSG
023800160420     C                   leave
023900160420     c                   end
024000160420     c                   if        v1ctur2 = v1ctur1
024100160420     C                   SETON                                        4928
024200160420     C                   MOVEL     ERR(16)       $MSG
024300160420     C                   leave
024400160420     c                   end
024500160426     c                   if        v1ctpm = 'M'
024600160426     c                   if        v1ctur1 = 'U' or
024700160426     c                             v1ctur2 = 'U'
024800160426     c                   else
024900160422     C                   SETON                                        4928
025000160422     C                   MOVEL     ERR(16)       $MSG
025100160422     C                   leave
025200160422     c                   end
025300160426     c                   end
025400160420     C*  Data distinta
025500950522     C     V1CDDC        IFEQ      0
025600950522     C                   SETON                                        4328
025700950522     C                   MOVEL     ERR(7)        $MSG
025800030710     C                   leave
025900950522     C                   ELSE
026000950522     C                   MOVE      V1CDDC        G02DAT
026100950522     C                   MOVEL     *BLANKS       G02ERR
026200950522     C                   CALL      'XSRDA8'
026300950522     C                   PARM                    WLBDAT
026400950522     C     G02ERR        IFEQ      '1'
026500950522     C                   SETON                                        2843
026600950522     C                   MOVEL     ERR(6)        $MSG
026700030710     C                   leave
026800950522     C                   END
026900950522     C                   END
027000950522     C                   Z-ADD     G02INV        WDDT
027100950522     C                   Z-ADD     G02DAT        V1CDDC
027200070323     c* la data distinta deve essere >= oggi
027300070413     c                   if        wddt < woggi
027400070413     C                   SETON                                        2843
027500070413     C                   MOVEL     ERR(6)        $MSG
027600070413     C                   leave
027700070413     C                   END
027800950522     C*
027900950522     C                   Z-ADD     4             KNPG
028000950522     C                   Z-ADD     V1CFIL        KFGS
028100950522     C                   Z-ADD     WDDT          KDFV
028200070410     C     KDST2         CHAIN     FIDST08L                           3132
028300950522     C     *IN31         IFEQ      '1'
028400950522     C                   SETON                                        4328
028500950522     C                   MOVEL     ERR(5)        $MSG
028600030710     C                   leave
028700950522     C                   END
028800160420     c                   eval      ddstflr = dstflr
028900950522     C*
029000950522     C*  Numero distinta da
029100950522     C     V1CNDD        IFNE      *ZEROS
029200950522     C     V1CNDD        ANDNE     *BLANKS
029300950522     C* Ricerca
029400950522     C     '?'           SCAN      V1CNDD                                 32
029500950522     C     *IN32         IFEQ      '1'
029600160719     C                   Z-ADD     0             fdgNFV
029700160719     C                   MOVEL     V1CFIL        fdgFGS
029800160719     C                   Z-ADD     wddt          fdgGDA
029900160719     C                   Z-ADD     wddt          fdgGAL
030000160719     C                   MOVEL     fidg31ds      KPJBU
030100160719     c                   clear                   $FV6
030200160719      * impostato a tutti 99999
030300160719     c                   do        600           yy                3 0
030400160719     c                   z-add     999999        $FV6(yy)
030500160719     c                   enddo
030600160719     C                   CALL      'FIDG31R'
030700950522     C                   PARM                    KPJBA
030800160719     c                   parm                    $FV6
030900160719     C                   MOVEL     KPJBU         fidg31ds
031000160719     C                   MOVE      fdgNFV        V1CNDD
031100950522     C                   END
031200950522     C*
031300950522     C                   SETOFF                                       31
031400950522     C                   TESTN                   V1CNDD               30
031500950522     C  N30              SETON                                        31
031600950522     C     *IN30         IFEQ      '1'
031700950522     C                   MOVE      V1CNDD        KNFV
031800950522     C                   MOVE      V1CNDD        WNDT
031900070410     C     KDST1         CHAIN     FIDST09L                           3132
032000950522     C                   END
032100950522     C     *IN31         IFEQ      '1'
032200070201     C     dstATB        OREQ      '*'
032300950522     C                   SETON                                        4128
032400950522     C                   MOVEL     ERR(2)        $MSG
032500030710     C                   leave
032600950522     C                   END
032700950522     C     *IN32         IFEQ      '1'
032800950522     C                   SETON                                        4128
032900950522     C                   MOVEL     ERR(8)        $MSG
033000030710     C                   leave
033100950522     C                   END
033200160420     c                   eval      ddstflr = dstflr
033300070201     C     dstFCF        IFEQ      'S'
033400950522     C                   SETON                                        4128
033500950522     C                   MOVEL     ERR(4)        $MSG
033600030710     C                   leave
033700950522     C                   END
033800070627     C     dstfasd       IFeq      'STP'
033900070627     C                   SETON                                        4428
034000070627     C                   MOVEL     ERR(12)       $MSG
034100070627     C                   leave
034200080326     C                   END
034300080326     c* verifico se distinta ha fasi
034400080326     c                   eval      dsftrd = 'STP'
034500080326     c     kdst3         setll     fidsf02l                               42
034600080326     c                   if        not *in42 and (dstpda='E' or dstpda='C')
034700080326     c                   eval      dsftrd = 'PDC'
034800080326     c     kdst3         setll     fidsf02l                               42
034900080326     c                   end
035000080326     c                   if        *in42
035100080326     C                   SETON                                        28
035200080326     C                   MOVEL     ERR(12)       $MSG
035300080326     C                   END
035400080326     c*
035500070201     C     dstDFV        IFNE      WDDT
035600950606     C                   SETON                                        414328
035700950606     C                   MOVEL     ERR(5)        $MSG
035800030710     C                   leave
035900950606     C                   END
036000070824     C     V1CFPp        IFNE      *BLANKS
036100070824     C     V1CFPp        ANDNE     dstFPP
036200070824     C                   SETON                                        414428
036300070824     C                   MOVEL     ERR(9)        $MSG
036400070824     C                   leave
036500070824     C                   END
036600950522     C                   MOVE      V1CNDD        WNDD              7 0
036700950522     C*
036800950522     C     V1CNDA        IFEQ      *ZEROS
036900950522     C                   MOVE      V1CNDD        V1CNDA
037000950522     C                   END
037100950522     C                   END
037200030910     C*  la distinta finale non può essere a 0
037300030910     c     v1cnda        ifeq      0
037400030910     C                   SETON                                        4228
037500030910     C                   MOVEL     ERR(1)        $MSG
037600030910     c                   end
037700950522     C*  Limiti incompatibili
037800950522     C     WNDD          IFGT      V1CNDA
037900950522     C                   SETON                                        414228
038000950522     C                   MOVEL     ERR(3)        $MSG
038100950522     C                   END
038200050520     c* se parcel + messaggerie se scelta una sola distinta controllo che
038300050520     c* il tipo scelto sia congruente
038400050525     c                   if        *in10 and not *in28  and wndd <> 0
038500050523     c                             and v1ctpm <> ' '
038600070201     c                   if        (v1ctpm = 'P' and dsttpm <>' ') or
038700070201     c                             (v1ctpm = 'M' and dsttpm <>'M')
038800050520     C                   SETON                                        414228
038900070201     C                   MOVEL     ERR(10)       $MSG
039000050520     c                   end
039100050523     c                   end
039200160420     c* se turno e se scelta una sola distinta controllo che
039300160420     c* il tipo scelto sia congruente
039400160420     c                   if        *in09 and not *in28  and wndd <> 0
039500160420     c                             and (v1ctur1 = ' ' or v1ctur2 = ' ')
039600160420     c                             and v1ctur1 <> 'U' and v1ctur2 <> 'U'
039700160422     c                             and dsttpm = ' '
039800160420     c                   select
039900160420     c                   when      (v1ctur1 = '1' or
040000160420     c                              v1ctur2 = '1') and
040100160420     c                             §dstturno <> ' '
040200160420     C                   SETON                                        28
040300160420     c                   when      (v1ctur1 = '2' or
040400160420     c                              v1ctur2 = '2') and
040500160420     c                             §dstturno <> '2'
040600160420     C                   SETON                                        28
040700160420     c                   endsl
040800160420     c                   if        *in28
040900160420     c                   seton                                        4142
041000160420     C                   MOVEL     ERR(17)       $MSG
041100160420     C                   leave
041200160420     c                   end
041300160420     c                   end
041400070919     c* se abilitato in organigramma matt/pom. obbligo l'inserimento
041500070919     c* del valore
041600070919     c                   if        §OGfppda = 'S' and  v1cfpp = ' '
041700070919     C                   SETON                                        4728
041800070919     C                   MOVEL     ERR(15)       $MSG
041900070919     C                   leave
042000070919     c                   end
042100070627     c* se richiesta pulizia precedente assegnazione segnalo se una o +
042200070627     c* distinte sono state stampate
042300080326     c                   if        not *in28
042400070627     c                   if        wndd <> savndd or v1cnda <> savnda or
042500160420     c                             v1ctpm <> savtpm or v1cfil <> savfil or
042600160420     c                             v1ctur1<> savtur1 or v1ctur2<> savtur2
042700070627     c                             or wddt <> savddt
042800070627     c                   setoff                                       29
042900070627     c                   eval      savndd = wndd
043000070627     c                   eval      savnda = v1cnda
043100070627     c                   eval      savtpm = v1ctpm
043200070627     c                   eval      savfil = v1cfil
043300070627     c                   eval      savddt = wddt
043400160420     c                   eval      savtur1= v1ctur1
043500160420     c                   eval      savtur2= v1ctur2
043600070627     c                   end
043700080326     c                   if        not *in29
043800070627     C                   Z-ADD     4             KNPG
043900070627     C                   Z-ADD     V1CFIL        KFGS
044000070627     C                   Z-ADD     WDDT          KDFV
044100070627     C     KDST2         setll     FIDST08L
044200070627     c                   do        *hival
044300070627     C     KDST2         reade     FIDST08L
044400070627     c                   if        %eof(fidst08l)
044500070627     c                   leave
044600070627     c                   end
044700160420     c                   eval      ddstflr = dstflr
044800080326     c                   eval      knfv = dstnfv
044900080326     c* verifico se la distinta può essere elaborata
045000080326     c                   eval      dsftrd = 'STP'
045100080326     C     KDST3         chain     FIDSf02l
045200080326     c                   if        not %found(fidsf02l) and
045300080326     c                             (dstpda ='E' or dstpda='C')
045400080326     c                   eval      dsftrd = 'PDC'
045500080326     C     KDST3         chain     FIDSf02l
045600080326     c                   end
045700080326     c                   if        dstfasd = 'STP' or %found(fidsf02l)
045800070627     c                   if        dstnfv >= wndd and dstnfv <= v1cnda
045900160420     c* piking / messaggerie
046000070627     c                   if        (v1ctpm = 'P' and dsttpm = ' ') or
046100070627     c                             (v1ctpm = 'M' and dsttpm ='M') or
046200070627     c                             v1ctpm = ' '
046300070627     C                   SETON                                        2928
046400070627     C                   MOVEL     ERR(13)       $MSG
046500070627     c                   leave
046600070627     c                   end
046700160420     c* turno
046800160420     c                   if        (v1ctur1 = ' ' or v1ctur2 = ' ')
046900160420     c                             and v1ctur1 <> 'U' and v1ctur2 <> 'U'
047000160422     c                             and dsttpm = ' '
047100160420     c                   select
047200160420     c                   when      (v1ctur1 = '1' or
047300160420     c                              v1ctur2 = '1') and
047400160420     c                             §dstturno <> ' '
047500160420     C                   SETON                                        28
047600160420     c                   when      (v1ctur1 = '2' or
047700160420     c                              v1ctur2 = '2') and
047800160420     c                             §dstturno <> '2'
047900160420     C                   SETON                                        28
048000160420     c                   endsl
048100160420     c                   if        *in28
048200160420     c                   seton                                        4142
048300160420     C                   MOVEL     ERR(13)       $MSG
048400160420     c                   leave
048500160420     c                   end
048600160420     c                   end
048700070627     c                   end
048800080326     c                   end
048900030710     C                   ENDdo
049000080326     c                   end
049100070627     c                   end
049200071210     C*  Data limite consegna richiesta PRIMA DEL
049300071210     c                   clear                   wldcr
049400071210     C     V1Cldcr       IFne      0
049500071210     C                   MOVE      V1Cldcr       G02DAT
049600071210     C                   MOVEL     *BLANKS       G02ERR
049700071210     C                   CALL      'XSRDA8'
049800071210     C                   PARM                    WLBDAT
049900071210     C     G02ERR        IFEQ      '1'
050000071210     C                   SETON                                        2848
050100071210     C                   MOVEL     ERR(6)        $MSG
050200071210     C                   leave
050300071210     C                   END
050400071210     C                   Z-ADD     G02INV        Wldcr
050500071210     C                   Z-ADD     G02DAT        V1Cldcr
050600071210     c* la data limite cons. richiesta non può essere <= data distinta
050700071210     c                   if        wldcr <= wddt
050800071210     C                   SETON                                        2848
050900071210     C                   MOVEL     ERR(6)        $MSG
051000071210     C                   leave
051100071210     C                   END
051200071210     C                   END
051300070824     C*  Data abbinamento solo se abilitato p.o.
051400070828     c                   clear                   wdta
051500070824     c                   if        *in12
051600070824     C     V1CDta        IFEQ      0
051700070824     C                   SETON                                        4528
051800070824     C                   MOVEL     ERR(6)        $MSG
051900070824     C                   leave
052000070824     C                   ELSE
052100070824     C                   MOVE      V1CDta        G02DAT
052200070824     C                   MOVEL     *BLANKS       G02ERR
052300070824     C                   CALL      'XSRDA8'
052400070824     C                   PARM                    WLBDAT
052500070824     C     G02ERR        IFEQ      '1'
052600070824     C                   SETON                                        2845
052700070824     C                   MOVEL     ERR(6)        $MSG
052800070824     C                   leave
052900070824     C                   END
053000070824     C                   END
053100070824     C                   Z-ADD     G02INV        WDta
053200070824     C                   Z-ADD     G02DAT        V1CDta
053300071210     c* la data limite abbinamento non può essere > data distinta
053400071210     c                   if        wdta > wddt
053500070824     C                   SETON                                        2845
053600070824     C                   MOVEL     ERR(6)        $MSG
053700070824     C                   leave
053800070824     C                   END
053900070824     c* ora abbinamento diversa da 0
054000070824     c                   if        v1cora = 0 or v1cora > 2400
054100070824     C                   SETON                                        2846
054200070824     C                   MOVEL     ERR(14)       $MSG
054300070824     C                   leave
054400070824     c                   end
054500070824     c                   end
054600140926     C*
054700070627     C                   ENDdo
054800030710     C*
054900030710     C                   ENDSR
055000050426     C*--------------------------------------------------------------*
055100070201     C*  Sottometto preparazione distinta di consegna batch
055200050426     C*--------------------------------------------------------------*
055300050426     C     SBMP60        BEGSR
055400050426     C*
055500070205     c                   clear                   fidg17ds
055600070206     c                   eval      d17cmti = 'S'
055700070206     C                   Z-ADD     V1CFIL        d17FGSi
055800070206     C                   Z-ADD     WDDT          d17Dfvi
055900070206     C                   MOVE      V1CNDD        d17NfDi
056000070206     C                   Z-ADD     V1CNDA        d17NfAi
056100070206     C     d17NfAi       IFEQ      0
056200070206     C                   Z-ADD     9999999       d17NfAi
056300950526     C                   END
056400070206     C                   Z-ADD     v1cret        d17reti
056500070206     C                   eval      d17cleari= v1cclear
056600070313     C                   eval      D17CLRMAN= V1CCLRMAN
056700150430     C                   eval      D17p88i= V1Cp88
056800070828     C                   eval      D17dtai= wdta
056900070828     C                   eval      D17orai= V1Cora
057000071210     C                   eval      D17ldcri= wldcr
057100070824     C                   movel     v1cfpp        d17FPpi
057200070206     C                   MOVEL     v1ctpm        d17tpmi
057300160420     C                   MOVEL     v1ctur1       d17tur1
057400160420     C                   MOVEL     v1ctur2       d17tur2
057500070201     C                   MOVEL(P)  Fidg17DS      KPJBU
057600070206     C                   CALL      'FIDG18C'
057700030910     C                   PARM                    KPJBA
057800070215     C                   eval      Fidg17DS =   KPJBU
057900070206     c*
058000070206     c                   if        d17erro<> ' '
058100070205     c                   seton                                        28
058200070215     C                   MOVEL     ERR(11)       $MSG
058300070827     c                   else
058400070827     c                   eval      prmfgs = v1cfil
058500070827     c                   eval      prmpgm = 'FIDG17R'
058600070827     c     keyprm        chain     fiprm01l
058700070827     c                   eval      prmprf = knmus
058800070828     c                   eval      prmdes = 'Assegnazione AUTOMATICA spedizioni-
058900070828     c                              da cons.'
059000070827     c                   eval      prmpar = kpjbu
059100070829     C                   TIME                    WHHDAT           14 0
059200070829     C                   MOVE      WHHDAT        G02DAT
059300070829     C                   MOVEL     *BLANKS       G02ERR
059400070829     C                   CALL      'XSRDA8'
059500070829     C                   PARM                    WLBDAT
059600070829     C                   MOVE      G02INV        prmdta
059700070829     C                   MOVEl     whhdat        prmora
059800070827     c                   if        not %found(fiprm01l)
059900070827     c                   write     fiprm000
060000070827     c                   else
060100070827     c                   update    fiprm000
060200070827     c                   end
060300070205     c                   end
060400950522     C*
060500950522     C                   ENDSR
060600950517     C*--------------------------------------------------------------*
060700950517     C*  Operazioni iniziali
060800950517     C*--------------------------------------------------------------*
060900950517     C     *INZSR        BEGSR
061000950517     C*
061100950517     C     *ENTRY        PLIST
061200950517     C                   PARM                    KPJBA
061300070313     c* per EDP abilito pulizia spediz. manuali, % rettifica e da distinta
061400070313     c* a distinta
061500070313     c                   if        %subst(knmus: 1: 3) = 'EDP'
061600070313     c                   seton                                        88
061700070313     c                   else
061800070313     c                   setoff                                       88
061900070313     c                   end
062000950517     C*------------------------------------------------------*
062100030710     c     *dtaara       define    §azute        azuteds
062200030710     c     *dtaara       define    §datiute      ddatiute
062300030710     C                   in(E)     *dtaara
062400030710     C                   IF        %Error  or  RSUT = *blanks
062500030710     C                   call      'TIBS34R'
062600030710     C                   parm                    Tibs34Ds
062700030710     C                   in        *dtaara
062800030710     c                   ENDIF
062900950517     C*------------------------------------------------------*
063000950517     C*  Definisco chiavi di accesso
063100070410     C     KDST1         KLIST
063200070410     C                   KFLD                    KFGS
063300950522     C                   KFLD                    KNPG
063400950522     C                   KFLD                    KNFV
063500070410     C     KDST2         KLIST
063600070410     C                   KFLD                    KFGS
063700950517     C                   KFLD                    KNPG
063800950517     C                   KFLD                    KDFV
063900080326     C     Kdst3         KLIST
064000031020     C                   KFLD                    KNPG
064100080326     C                   KFLD                    KNFV
064200031020     C                   KFLD                    KFGS
064300080326     C                   KFLD                    dsftrd
064400070827     C     Keyprm        KLIST
064500070827     C                   KFLD                    prmfgs
064600070827     C                   KFLD                    prmpgm
064700950517     C*------------------------------------------------------*
064800950517     C*  Definisco campi
064900070201     C     *LIKE         DEFINE    dstNPG        KNPG
065000070201     C     *LIKE         DEFINE    dstDFV        KDFV
065100070201     C     *LIKE         DEFINE    dstFGS        KFGS
065200070201     C     *LIKE         DEFINE    dstNFV        KNFV
065300070201     C     *LIKE         DEFINE    dstNFV        WNDT
065400070201     C     *LIKE         DEFINE    dstDFV        WDDT
065500070824     C     *LIKE         DEFINE    dstDFV        WDta
065600071210     C     *LIKE         DEFINE    dstDFV        Wldcr
065700950517     C*------------------------------------------------------*
065800950517     C*  Inizializzo campi
065900160719     C                   CLEAR                   fidg31ds
066000950517     C                   CLEAR                   FNLV50
066100070201     C                   CLEAR                   Fidg17DS
066200160719     C                   MOVEL     4             fdgNPG
066300160719     C                   MOVEL     'FIDG17R'     fdgPGM
066400160719     C                   MOVEL     'S'           fdgRIC
066500160719     C                   MOVEL     '2'           fdgTFV
066600950517     C                   Z-ADD     4             KNPG
066700950522     C                   MOVEL     'N'           WFINE             1
066800950522     C                   MOVEL     '1'           WTPVID            1
066900950517     C*
067000950517     C* DATA DEL GIORNO
067100950522     C                   TIME                    WHHDAT           14 0
067200950522     C                   MOVE      WHHDAT        G02DAT
067300950522     C                   MOVEL     *BLANKS       G02ERR
067400950522     C                   CALL      'XSRDA8'
067500950522     C                   PARM                    WLBDAT
067600950522     C                   MOVE      G02INV        WOGGI             8 0
067700950517     C**
067800950517     C* VEDO SE SONO SIMFEL O REMOTO
067900950517     C**
068000030710     C     dutlpo        IFEQ      '2'
068100030710     C                   MOVEL     dutpou        V1CFIL
068200950517     C                   ELSE
068300030910     c                   seton                                        01
068400950517     C                   MOVEL     SIMFEL        V1CFIL
068500950517     C                   END
068600140926      * verifica presenza disposizioni web
068700141029     c                   clear                   fnlry09ds2
068800140926     c                   eval      ilry09tch = 'C'
068900141013     c                   exsr      verdispo
069000141029     c                   if        OLRY09ESal = '1' and
069100140926     c                             OLRY09ERR  = *blank
069200140926     c                   seton                                        97
069300140926     c                   endif
069400950517     C*
069500950517     C                   ENDSR
069600141013      *___________________________________________________________
069700141013     c     verdispo      begsr
069800141013      *___________________________________________________________
069900141029     c                   eval      ilry09tla = 'L'
070000141029     c                   eval      ilry09fgs = v1cfil
070100141029     c                   eval      ilry09tdis= 'E'
070200141029     c                   movel     fnlry09ds2    kpjbu
070300141013     c                   call      'FNLRY09R'
070400141013     c                   parm                    kpjba
070500141029     c                   movel     kpjbu         fnlry09ds2
070600141013     C                   ENDSR
070700141013      *___________________________________________________________
070800950524**
070900030910Numero distinta obbligatorio                                          01
071000030625Numero distinta inesistente o annullato                               02
071100030625Limiti incompatibilii: numero distinta al minore numero distinta dal  03
071200950522Distinta già chiusa                                                   04
071300950522Non trovate distinte entro i limiti indicati                          05
071400950522Data errata                                                           06
071500950522Data distinta obbligatoria                                            07
071600070201Impossibile gestire distinta                                          08
071700950524Distinta scelta non compatibile con tipo prestazione selezionato      09
071800070205Distinta non congruente con il tipo distinta consegna                 10
071900070216L'elaborazione è terminata con errore ... controllare la stampa       11
072000080326La distinta è in una fase per la quale non è + possibile l'elaboraz.  12
072100080326Ci sono distinte in una fase per la quale non è + possibile l'elabor. 13
072200070824Ora errata                                                            14
072300070919Obbligatorio scegliere il tipo uscita                                 15
072400160420I valori immessi per il Turno con sono corretti                       16
072500160420Distinta non congruente con il turno                                  17
