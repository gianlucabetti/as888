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
004100050519     D DSLR01        E DS
004200070201     D Fidg17ds      E DS
004300030710     D ddatiute      e ds
004301160420     D ddstflr       e ds
004400030710     D azuteds       e ds                  extname(AZUTE00F)
004500030710     D tibs34ds      E DS                  inz
004600141029     D fnlry09ds2    E DS                  inz
004601150513     d TRULVPODS     e ds
004602150513     d   sk_PDA               16    765    inz   dim(250)
004700070627     d savndd          s                   like(wndd)
004800070627     d savnda          s                   like(v1cnda)
004900070627     d savtpm          s                   like(v1ctpm)
004901160420     d savtur1         s                   like(v1ctur1)
004902160420     d savtur2         s                   like(v1ctur2)
005000070627     d savddt          s                   like(wddt)
005100070627     d savfil          s                   like(v1cfil)
005200950517     C*--------------------------------------------------------------*
005300950517     C*  Ciclo principale
005400950517     C*--------------------------------------------------------------*
005500950517     C* Inizializzazione prima videata
005600950517     C                   EXSR      INZD01
005700950517     C* Loop di gestione videata
005800950517     C     WFINE         DOUEQ     'S'
005900950517     C     WTPVID        CASEQ     '1'           GESD01
006000950517     C                   END
006100950517     C                   END
006200950517     C* Fine Pgm.
006300950517     C                   SETON                                        LR
006400950517     C*--------------------------------------------------------------*
006500950517     C*  Inizializzazione parametri prima videata
006600950517     C*--------------------------------------------------------------*
006700950522     C     INZD01        BEGSR
006800950517     C*
006900950602     C                   SETOFF                                       0302
007000070824     C                   clear                   V1Cfpp
007100070201     C                   MOVEL     'N'           V1CCLEAR
007200070313     C                   MOVEL     'N'           V1CCLRMAN
007202150513     C                   clear                   V1Cp88
007203150513     c* ricerca P.O. abilitati da tabella VPO per step di test
007204150513     c                   clear                   trulvpods
007205150513     c                   eval      ivpoke1 = 'SQA'
007206150513     c                   call      'TRULVPOR'
007207150513     c                   parm                    trulvpods
007208150513     c                   move      '999'         fgsa              3
007209150513     c     fgsa          lookup    sk_pda                                 37
007210150513     c  n37              move      dutpou        fgsa
007211150513     c  n37fgsa          lookup    sk_pda                                 37
007212150513     c                   if        *in37
007213150513     c                   eval      v1cp88 = 'S'
007214150513     c                   else
007215150513     C                   CLEAR                   V1Cp88
007216150513     c                   end
007300070201     c                   clear                   v1cret
007400050523     C                   MOVEL     *blanks       V1Ctpm
007500070313     C                   Z-ADD     WHHDAT        V1CDDC
007600070824     C                   Z-ADD     0             V1CDta
007700070824     C                   Z-ADD     0             V1Cora
007800950517     C                   MOVEL     *BLANKS       V1CNDD
007900070313     C                   Z-ADD     99999         V1CNDA
008000030910     C     dutlpo        IFEQ      '2'
008100030910     C                   MOVEL     dutpou        V1CFIL
008200030910     C                   ELSE
008300030910     C                   MOVEL     SIMFEL        V1CFIL
008400030910     C                   END
008500050520     C* parcel + messaggerie
008600050520     c                   exsr      srtpm
008700950522     C                   MOVE      V1CFIL        WSVFIL            3 0
008800070201     C                   CLEAR                   Fidg17DS
008801160506     C                   MOVEL     'U'           V1Ctur1
008802160506     C                   MOVEL     ' '           V1Ctur2
008803160506     c                   if        *in09
008804160506     C                   MOVEL     '1'           V1Ctur1
008805160506     C                   MOVEL     '2'           V1Ctur2
008806160506     c                   end
008900950517     C*
009000950517     C                   ENDSR
009100050520     C*--------------------------------------------------------------*
009200050520     C*  parcel + messaggerie
009300050520     C*--------------------------------------------------------------*
009400050520     C     srtpm         BEGSR
009500050520     c* leggo organigramma x gestire ind 10 parcel+messaggerie
009600050520     c     v1cfil        chain     azorg01l
009700050520     c                   if        %found(azorg01l)
009800050520     c                   movel     orgdf0        og150
009900070313     C                   MOVEL     ORGDE6        og146
010000070828     C                   MOVEL     ORGDE7        og147
010100160420     c     §OGturno      comp      'S'                                    09
010101160420     c     §OGPKM        comp      'S'                                    10
010200070828     c     §OGdoada      comp      'S'                                    12
010300070828     c     §OGfppda      comp      'S'                                    11
010400070313     c                   end
010500070313     c* se p.o. non partito esco
010600070313     c                   if        §ogdda = ' '
010700070313     c                   seton                                        lr
010800070313     c                   return
010900070313     c                   end
011000050520     C                   ENDSR
011100950517     C*--------------------------------------------------------------*
011200950517     C*  Gestione prima videata
011300950517     C*--------------------------------------------------------------*
011400950517     C     GESD01        BEGSR
011500950517     C*
011600070201     C                   EXFMT     dg17D01
011700950524     C                   SETOFF                                       284244
011800950517     C                   SETOFF                                       404143
011900070919     C                   SETOFF                                       454647
012000160420     C                   SETOFF                                       4849
012100950522     C                   MOVEL     *BLANKS       WNDT
012200950529     C                   Z-ADD     0             WNDD
012300950517     C*  Fine Pgm.
012400950517     C     *INKC         IFEQ      '1'
012500950517     C                   MOVEL     'S'           WFINE
012600950517     C                   END
012700950614     C*
012800950614     C   51              SETOFF                                       51
012900950614     C*
013000950614     C     *IN50         IFEQ      '1'
013100950614     C     WSVFIL        ORNE      V1CFIL
013200950614     C                   CLEAR                   FNLV50
013300950614     C                   MOVEL     KNMUS         D50PRU
013400950614     C                   MOVEL     V1CFIL        D50FGS
013500950614     C                   CALL      'FNLV50R'
013600950614     C                   PARM                    FNLV50
013700950614     C                   SETOFF                                       50
013800950614     C                   MOVE      V1CFIL        WSVFIL            3 0
013900050520     C* parcel + messaggerie
014000050520     c                   exsr      srtpm
014100050520     c                   end
014200950602     C*  Sblocco data
014300950602     C     *INKA         IFEQ      '1'
014400950602     C                   SETON                                        5103
014500070627     c                   setoff                                       29
014600950602     C                   GOTO      FINVD1
014700950602     C                   END
014800950517     C*  Cambio filiale
014900950517     C     *INKS         IFEQ      '1'
015000950602     C                   SETON                                        5002
015100070627     c                   setoff                                       29
015200950517     C                   GOTO      FINVD1
015300950517     C                   END
015400140924?    C* F19- Richiama pgm Disposizioni via WEB
015500140924     C     *inkt         IFEQ      '1'
015600141029     c                   clear                   fnlry09ds2
015700140924     c                   eval      ilry09tch = 'E'
015800141013     c                   exsr      verdispo
015900141029     c                   if        OLRY09ESal = '1'
016000141001     c                   seton                                        97
016100141001     c                   else
016200141001     c                   setoff                                       97
016300141001     c                   endif
016400141001     c                   goto      finvd1
016500140924     c                   endif
016600950517     C*  Controlli
016700950517     C                   EXSR      CTR01
016800950517     C*  Conferma
016900070201     C                   IF        not *in28 and *inkf
017000950517     C                   EXSR      SBMP60
017100070205     c                   if        not *in28
017200070412     C                   MOVEL     'S'           WFINE
017300070205     c                   end
017400070205     C                   END
017500950517     C*
017600950517     C     FINVD1        ENDSR
017700950522     C*--------------------------------------------------------------*
017800950522     C*  Controlli prima videata
017900950522     C*--------------------------------------------------------------*
018000950522     C     CTR01         BEGSR
018100950522     C*
018200950522     C*  Controllo filiale
018300030710     c                   do
018400140926     c                   setoff                                       97
018500140926     c*
018600140926      * verifica presenza disposizioni web
018700141029     c                   clear                   fnlry09ds2
018800140926     c                   eval      ilry09tch = 'C'
018900141013     c                   exsr      verdispo
019000141029     c                   if        OLRY09ESal = '1' and
019100140926     c                             OLRY09ERR  = *blank
019200140926     c                   seton                                        97
019300140926     c                   endif
019400950522     C     D50ERR        IFNE      ' '
019500950522     C                   SETON                                        2840
019600950522     C                   MOVEL     D50MSG        $MSG
019700030710     C                   leave
019800950522     C                   ENDIF
019900160420     C*  turno
019901160420     c                   if        v1ctur1 = 'U' and v1ctur2 <>'U'
019902160420     c                             and v1ctur2 <>' '
019903160420     C                   SETON                                        4928
019904160420     C                   MOVEL     ERR(16)       $MSG
019905160420     C                   leave
019906160420     c                   end
019907160420     c                   if        v1ctur2 = 'U' and v1ctur1 <>'U'
019908160420     c                             and v1ctur1 <>' '
019909160420     C                   SETON                                        4928
019910160420     C                   MOVEL     ERR(16)       $MSG
019911160420     C                   leave
019912160420     c                   end
019913160420     c                   if        v1ctur2 = v1ctur1
019915160420     C                   SETON                                        4928
019916160420     C                   MOVEL     ERR(16)       $MSG
019917160420     C                   leave
019918160420     c                   end
019919160426     c                   if        v1ctpm = 'M'
019920160426     c                   if        v1ctur1 = 'U' or
019921160426     c                             v1ctur2 = 'U'
019922160426     c                   else
019923160422     C                   SETON                                        4928
019924160422     C                   MOVEL     ERR(16)       $MSG
019925160422     C                   leave
019926160422     c                   end
019927160426     c                   end
019928160420     C*  Data distinta
020000950522     C     V1CDDC        IFEQ      0
020100950522     C                   SETON                                        4328
020200950522     C                   MOVEL     ERR(7)        $MSG
020300030710     C                   leave
020400950522     C                   ELSE
020500950522     C                   MOVE      V1CDDC        G02DAT
020600950522     C                   MOVEL     *BLANKS       G02ERR
020700950522     C                   CALL      'XSRDA8'
020800950522     C                   PARM                    WLBDAT
020900950522     C     G02ERR        IFEQ      '1'
021000950522     C                   SETON                                        2843
021100950522     C                   MOVEL     ERR(6)        $MSG
021200030710     C                   leave
021300950522     C                   END
021400950522     C                   END
021500950522     C                   Z-ADD     G02INV        WDDT
021600950522     C                   Z-ADD     G02DAT        V1CDDC
021700070323     c* la data distinta deve essere >= oggi
021800070413     c                   if        wddt < woggi
021900070413     C                   SETON                                        2843
022000070413     C                   MOVEL     ERR(6)        $MSG
022100070413     C                   leave
022200070413     C                   END
022300950522     C*
022400950522     C                   Z-ADD     4             KNPG
022500950522     C                   Z-ADD     V1CFIL        KFGS
022600950522     C                   Z-ADD     WDDT          KDFV
022700070410     C     KDST2         CHAIN     FIDST08L                           3132
022800950522     C     *IN31         IFEQ      '1'
022900950522     C                   SETON                                        4328
023000950522     C                   MOVEL     ERR(5)        $MSG
023100030710     C                   leave
023200950522     C                   END
023201160420     c                   eval      ddstflr = dstflr
023300950522     C*
023400950522     C*  Numero distinta da
023500950522     C     V1CNDD        IFNE      *ZEROS
023600950522     C     V1CNDD        ANDNE     *BLANKS
023700950522     C* Ricerca
023800950522     C     '?'           SCAN      V1CNDD                                 32
023900950522     C     *IN32         IFEQ      '1'
024000950522     C                   Z-ADD     0             DLRNFV
024100950522     C                   MOVEL     V1CFIL        DLRFGS
024200950522     C                   Z-ADD     V1CDDC        DLRGDA
024300950522     C                   Z-ADD     V1CDDC        DLRGAL
024400950522     C                   MOVEL     DSLR01        KPJBU
024500070313     C                   CALL      'FNLR02R'
024600950522     C                   PARM                    KPJBA
024700950522     C                   MOVEL     KPJBU         DSLR01
024800070202     C                   MOVE      DLRNFV        V1CNDD
024900950522     C                   END
025000950522     C*
025100950522     C                   SETOFF                                       31
025200950522     C                   TESTN                   V1CNDD               30
025300950522     C  N30              SETON                                        31
025400950522     C     *IN30         IFEQ      '1'
025500950522     C                   MOVE      V1CNDD        KNFV
025600950522     C                   MOVE      V1CNDD        WNDT
025700070410     C     KDST1         CHAIN     FIDST09L                           3132
025800950522     C                   END
025900950522     C     *IN31         IFEQ      '1'
026000070201     C     dstATB        OREQ      '*'
026100950522     C                   SETON                                        4128
026200950522     C                   MOVEL     ERR(2)        $MSG
026300030710     C                   leave
026400950522     C                   END
026500950522     C     *IN32         IFEQ      '1'
026600950522     C                   SETON                                        4128
026700950522     C                   MOVEL     ERR(8)        $MSG
026800030710     C                   leave
026900950522     C                   END
026901160420     c                   eval      ddstflr = dstflr
027000070201     C     dstFCF        IFEQ      'S'
027100950522     C                   SETON                                        4128
027200950522     C                   MOVEL     ERR(4)        $MSG
027300030710     C                   leave
027400950522     C                   END
027500070627     C     dstfasd       IFeq      'STP'
027600070627     C                   SETON                                        4428
027700070627     C                   MOVEL     ERR(12)       $MSG
027800070627     C                   leave
027900080326     C                   END
028000080326     c* verifico se distinta ha fasi
028100080326     c                   eval      dsftrd = 'STP'
028200080326     c     kdst3         setll     fidsf02l                               42
028300080326     c                   if        not *in42 and (dstpda='E' or dstpda='C')
028400080326     c                   eval      dsftrd = 'PDC'
028500080326     c     kdst3         setll     fidsf02l                               42
028600080326     c                   end
028700080326     c                   if        *in42
028800080326     C                   SETON                                        28
028900080326     C                   MOVEL     ERR(12)       $MSG
029000080326     C                   END
029100080326     c*
029200070201     C     dstDFV        IFNE      WDDT
029300950606     C                   SETON                                        414328
029400950606     C                   MOVEL     ERR(5)        $MSG
029500030710     C                   leave
029600950606     C                   END
029700070824     C     V1CFPp        IFNE      *BLANKS
029800070824     C     V1CFPp        ANDNE     dstFPP
029900070824     C                   SETON                                        414428
030000070824     C                   MOVEL     ERR(9)        $MSG
030100070824     C                   leave
030200070824     C                   END
030300950522     C                   MOVE      V1CNDD        WNDD              7 0
030400950522     C*
030500950522     C     V1CNDA        IFEQ      *ZEROS
030600950522     C                   MOVE      V1CNDD        V1CNDA
030700950522     C                   END
030800950522     C                   END
030900030910     C*  la distinta finale non può essere a 0
031000030910     c     v1cnda        ifeq      0
031100030910     C                   SETON                                        4228
031200030910     C                   MOVEL     ERR(1)        $MSG
031300030910     c                   end
031400950522     C*  Limiti incompatibili
031500950522     C     WNDD          IFGT      V1CNDA
031600950522     C                   SETON                                        414228
031700950522     C                   MOVEL     ERR(3)        $MSG
031800950522     C                   END
031900050520     c* se parcel + messaggerie se scelta una sola distinta controllo che
032000050520     c* il tipo scelto sia congruente
032100050525     c                   if        *in10 and not *in28  and wndd <> 0
032200050523     c                             and v1ctpm <> ' '
032300070201     c                   if        (v1ctpm = 'P' and dsttpm <>' ') or
032400070201     c                             (v1ctpm = 'M' and dsttpm <>'M')
032500050520     C                   SETON                                        414228
032600070201     C                   MOVEL     ERR(10)       $MSG
032700050520     c                   end
032800050523     c                   end
032801160420     c* se turno e se scelta una sola distinta controllo che
032802160420     c* il tipo scelto sia congruente
032803160420     c                   if        *in09 and not *in28  and wndd <> 0
032804160420     c                             and (v1ctur1 = ' ' or v1ctur2 = ' ')
032805160420     c                             and v1ctur1 <> 'U' and v1ctur2 <> 'U'
032806160422     c                             and dsttpm = ' '
032807160420     c                   select
032808160420     c                   when      (v1ctur1 = '1' or
032809160420     c                              v1ctur2 = '1') and
032810160420     c                             §dstturno <> ' '
032811160420     C                   SETON                                        28
032812160420     c                   when      (v1ctur1 = '2' or
032813160420     c                              v1ctur2 = '2') and
032814160420     c                             §dstturno <> '2'
032815160420     C                   SETON                                        28
032816160420     c                   endsl
032817160420     c                   if        *in28
032818160420     c                   seton                                        4142
032819160420     C                   MOVEL     ERR(17)       $MSG
032820160420     C                   leave
032821160420     c                   end
032822160420     c                   end
032900070919     c* se abilitato in organigramma matt/pom. obbligo l'inserimento
033000070919     c* del valore
033100070919     c                   if        §OGfppda = 'S' and  v1cfpp = ' '
033200070919     C                   SETON                                        4728
033300070919     C                   MOVEL     ERR(15)       $MSG
033400070919     C                   leave
033500070919     c                   end
033600070627     c* se richiesta pulizia precedente assegnazione segnalo se una o +
033700070627     c* distinte sono state stampate
033800080326     c                   if        not *in28
033900070627     c                   if        wndd <> savndd or v1cnda <> savnda or
034000160420     c                             v1ctpm <> savtpm or v1cfil <> savfil or
034001160420     c                             v1ctur1<> savtur1 or v1ctur2<> savtur2
034100070627     c                             or wddt <> savddt
034200070627     c                   setoff                                       29
034300070627     c                   eval      savndd = wndd
034400070627     c                   eval      savnda = v1cnda
034500070627     c                   eval      savtpm = v1ctpm
034600070627     c                   eval      savfil = v1cfil
034700070627     c                   eval      savddt = wddt
034701160420     c                   eval      savtur1= v1ctur1
034702160420     c                   eval      savtur2= v1ctur2
034800070627     c                   end
034900080326     c                   if        not *in29
035000070627     C                   Z-ADD     4             KNPG
035100070627     C                   Z-ADD     V1CFIL        KFGS
035200070627     C                   Z-ADD     WDDT          KDFV
035300070627     C     KDST2         setll     FIDST08L
035400070627     c                   do        *hival
035500070627     C     KDST2         reade     FIDST08L
035600070627     c                   if        %eof(fidst08l)
035700070627     c                   leave
035800070627     c                   end
035801160420     c                   eval      ddstflr = dstflr
035900080326     c                   eval      knfv = dstnfv
036000080326     c* verifico se la distinta può essere elaborata
036100080326     c                   eval      dsftrd = 'STP'
036200080326     C     KDST3         chain     FIDSf02l
036300080326     c                   if        not %found(fidsf02l) and
036400080326     c                             (dstpda ='E' or dstpda='C')
036500080326     c                   eval      dsftrd = 'PDC'
036600080326     C     KDST3         chain     FIDSf02l
036700080326     c                   end
036800080326     c                   if        dstfasd = 'STP' or %found(fidsf02l)
036900070627     c                   if        dstnfv >= wndd and dstnfv <= v1cnda
036901160420     c* piking / messaggerie
037000070627     c                   if        (v1ctpm = 'P' and dsttpm = ' ') or
037100070627     c                             (v1ctpm = 'M' and dsttpm ='M') or
037200070627     c                             v1ctpm = ' '
037300070627     C                   SETON                                        2928
037400070627     C                   MOVEL     ERR(13)       $MSG
037500070627     c                   leave
037600070627     c                   end
037601160420     c* turno
037604160420     c                   if        (v1ctur1 = ' ' or v1ctur2 = ' ')
037605160420     c                             and v1ctur1 <> 'U' and v1ctur2 <> 'U'
037606160422     c                             and dsttpm = ' '
037607160420     c                   select
037608160420     c                   when      (v1ctur1 = '1' or
037609160420     c                              v1ctur2 = '1') and
037610160420     c                             §dstturno <> ' '
037611160420     C                   SETON                                        28
037612160420     c                   when      (v1ctur1 = '2' or
037613160420     c                              v1ctur2 = '2') and
037614160420     c                             §dstturno <> '2'
037615160420     C                   SETON                                        28
037616160420     c                   endsl
037617160420     c                   if        *in28
037618160420     c                   seton                                        4142
037619160420     C                   MOVEL     ERR(13)       $MSG
037620160420     c                   leave
037621160420     c                   end
037622160420     c                   end
037700070627     c                   end
037800080326     c                   end
037900030710     C                   ENDdo
038000080326     c                   end
038100070627     c                   end
038200071210     C*  Data limite consegna richiesta PRIMA DEL
038300071210     c                   clear                   wldcr
038400071210     C     V1Cldcr       IFne      0
038500071210     C                   MOVE      V1Cldcr       G02DAT
038600071210     C                   MOVEL     *BLANKS       G02ERR
038700071210     C                   CALL      'XSRDA8'
038800071210     C                   PARM                    WLBDAT
038900071210     C     G02ERR        IFEQ      '1'
039000071210     C                   SETON                                        2848
039100071210     C                   MOVEL     ERR(6)        $MSG
039200071210     C                   leave
039300071210     C                   END
039400071210     C                   Z-ADD     G02INV        Wldcr
039500071210     C                   Z-ADD     G02DAT        V1Cldcr
039600071210     c* la data limite cons. richiesta non può essere <= data distinta
039700071210     c                   if        wldcr <= wddt
039800071210     C                   SETON                                        2848
039900071210     C                   MOVEL     ERR(6)        $MSG
040000071210     C                   leave
040100071210     C                   END
040200071210     C                   END
040300070824     C*  Data abbinamento solo se abilitato p.o.
040400070828     c                   clear                   wdta
040500070824     c                   if        *in12
040600070824     C     V1CDta        IFEQ      0
040700070824     C                   SETON                                        4528
040800070824     C                   MOVEL     ERR(6)        $MSG
040900070824     C                   leave
041000070824     C                   ELSE
041100070824     C                   MOVE      V1CDta        G02DAT
041200070824     C                   MOVEL     *BLANKS       G02ERR
041300070824     C                   CALL      'XSRDA8'
041400070824     C                   PARM                    WLBDAT
041500070824     C     G02ERR        IFEQ      '1'
041600070824     C                   SETON                                        2845
041700070824     C                   MOVEL     ERR(6)        $MSG
041800070824     C                   leave
041900070824     C                   END
042000070824     C                   END
042100070824     C                   Z-ADD     G02INV        WDta
042200070824     C                   Z-ADD     G02DAT        V1CDta
042300071210     c* la data limite abbinamento non può essere > data distinta
042400071210     c                   if        wdta > wddt
042500070824     C                   SETON                                        2845
042600070824     C                   MOVEL     ERR(6)        $MSG
042700070824     C                   leave
042800070824     C                   END
042900070824     c* ora abbinamento diversa da 0
043000070824     c                   if        v1cora = 0 or v1cora > 2400
043100070824     C                   SETON                                        2846
043200070824     C                   MOVEL     ERR(14)       $MSG
043300070824     C                   leave
043400070824     c                   end
043500070824     c                   end
043600140926     C*
043700070627     C                   ENDdo
043800030710     C*
043900030710     C                   ENDSR
044000050426     C*--------------------------------------------------------------*
044100070201     C*  Sottometto preparazione distinta di consegna batch
044200050426     C*--------------------------------------------------------------*
044300050426     C     SBMP60        BEGSR
044400050426     C*
044500070205     c                   clear                   fidg17ds
044600070206     c                   eval      d17cmti = 'S'
044700070206     C                   Z-ADD     V1CFIL        d17FGSi
044800070206     C                   Z-ADD     WDDT          d17Dfvi
044900070206     C                   MOVE      V1CNDD        d17NfDi
045000070206     C                   Z-ADD     V1CNDA        d17NfAi
045100070206     C     d17NfAi       IFEQ      0
045200070206     C                   Z-ADD     9999999       d17NfAi
045300950526     C                   END
045400070206     C                   Z-ADD     v1cret        d17reti
045500070206     C                   eval      d17cleari= v1cclear
045600070313     C                   eval      D17CLRMAN= V1CCLRMAN
045601150430     C                   eval      D17p88i= V1Cp88
045700070828     C                   eval      D17dtai= wdta
045800070828     C                   eval      D17orai= V1Cora
045900071210     C                   eval      D17ldcri= wldcr
046000070824     C                   movel     v1cfpp        d17FPpi
046100070206     C                   MOVEL     v1ctpm        d17tpmi
046101160420     C                   MOVEL     v1ctur1       d17tur1
046102160420     C                   MOVEL     v1ctur2       d17tur2
046200070201     C                   MOVEL(P)  Fidg17DS      KPJBU
046300070206     C                   CALL      'FIDG18C'
046400030910     C                   PARM                    KPJBA
046500070215     C                   eval      Fidg17DS =   KPJBU
046600070206     c*
046700070206     c                   if        d17erro<> ' '
046800070205     c                   seton                                        28
046900070215     C                   MOVEL     ERR(11)       $MSG
047000070827     c                   else
047100070827     c                   eval      prmfgs = v1cfil
047200070827     c                   eval      prmpgm = 'FIDG17R'
047300070827     c     keyprm        chain     fiprm01l
047400070827     c                   eval      prmprf = knmus
047500070828     c                   eval      prmdes = 'Assegnazione AUTOMATICA spedizioni-
047600070828     c                              da cons.'
047700070827     c                   eval      prmpar = kpjbu
047800070829     C                   TIME                    WHHDAT           14 0
047900070829     C                   MOVE      WHHDAT        G02DAT
048000070829     C                   MOVEL     *BLANKS       G02ERR
048100070829     C                   CALL      'XSRDA8'
048200070829     C                   PARM                    WLBDAT
048300070829     C                   MOVE      G02INV        prmdta
048400070829     C                   MOVEl     whhdat        prmora
048500070827     c                   if        not %found(fiprm01l)
048600070827     c                   write     fiprm000
048700070827     c                   else
048800070827     c                   update    fiprm000
048900070827     c                   end
049000070205     c                   end
049100950522     C*
049200950522     C                   ENDSR
049300950517     C*--------------------------------------------------------------*
049400950517     C*  Operazioni iniziali
049500950517     C*--------------------------------------------------------------*
049600950517     C     *INZSR        BEGSR
049700950517     C*
049800950517     C     *ENTRY        PLIST
049900950517     C                   PARM                    KPJBA
050000070313     c* per EDP abilito pulizia spediz. manuali, % rettifica e da distinta
050100070313     c* a distinta
050200070313     c                   if        %subst(knmus: 1: 3) = 'EDP'
050300070313     c                   seton                                        88
050400070313     c                   else
050500070313     c                   setoff                                       88
050600070313     c                   end
050700950517     C*------------------------------------------------------*
050800030710     c     *dtaara       define    §azute        azuteds
050900030710     c     *dtaara       define    §datiute      ddatiute
051000030710     C                   in(E)     *dtaara
051100030710     C                   IF        %Error  or  RSUT = *blanks
051200030710     C                   call      'TIBS34R'
051300030710     C                   parm                    Tibs34Ds
051400030710     C                   in        *dtaara
051500030710     c                   ENDIF
051600950517     C*------------------------------------------------------*
051700950517     C*  Definisco chiavi di accesso
051800070410     C     KDST1         KLIST
051900070410     C                   KFLD                    KFGS
052000950522     C                   KFLD                    KNPG
052100950522     C                   KFLD                    KNFV
052200070410     C     KDST2         KLIST
052300070410     C                   KFLD                    KFGS
052400950517     C                   KFLD                    KNPG
052500950517     C                   KFLD                    KDFV
052600080326     C     Kdst3         KLIST
052700031020     C                   KFLD                    KNPG
052800080326     C                   KFLD                    KNFV
052900031020     C                   KFLD                    KFGS
053000080326     C                   KFLD                    dsftrd
053100070827     C     Keyprm        KLIST
053200070827     C                   KFLD                    prmfgs
053300070827     C                   KFLD                    prmpgm
053400950517     C*------------------------------------------------------*
053500950517     C*  Definisco campi
053600070201     C     *LIKE         DEFINE    dstNPG        KNPG
053700070201     C     *LIKE         DEFINE    dstDFV        KDFV
053800070201     C     *LIKE         DEFINE    dstFGS        KFGS
053900070201     C     *LIKE         DEFINE    dstNFV        KNFV
054000070201     C     *LIKE         DEFINE    dstNFV        WNDT
054100070201     C     *LIKE         DEFINE    dstDFV        WDDT
054200070824     C     *LIKE         DEFINE    dstDFV        WDta
054300071210     C     *LIKE         DEFINE    dstDFV        Wldcr
054400950517     C*------------------------------------------------------*
054500950517     C*  Inizializzo campi
054600950517     C                   CLEAR                   DSLR01
054700950517     C                   CLEAR                   FNLV50
054800070201     C                   CLEAR                   Fidg17DS
054900950517     C                   MOVEL     4             DLRNPG
055000070201     C                   MOVEL     'FIDG17R'     DLRPGM
055100950517     C                   MOVEL     'S'           DLRRIC
055200950517     C                   MOVEL     '2'           DLRTFV
055300950517     C                   Z-ADD     4             KNPG
055400950522     C                   MOVEL     'N'           WFINE             1
055500950522     C                   MOVEL     '1'           WTPVID            1
055600950517     C*
055700950517     C* DATA DEL GIORNO
055800950522     C                   TIME                    WHHDAT           14 0
055900950522     C                   MOVE      WHHDAT        G02DAT
056000950522     C                   MOVEL     *BLANKS       G02ERR
056100950522     C                   CALL      'XSRDA8'
056200950522     C                   PARM                    WLBDAT
056300950522     C                   MOVE      G02INV        WOGGI             8 0
056400950517     C**
056500950517     C* VEDO SE SONO SIMFEL O REMOTO
056600950517     C**
056700030710     C     dutlpo        IFEQ      '2'
056800030710     C                   MOVEL     dutpou        V1CFIL
056900950517     C                   ELSE
057000030910     c                   seton                                        01
057100950517     C                   MOVEL     SIMFEL        V1CFIL
057200950517     C                   END
057300140926      * verifica presenza disposizioni web
057400141029     c                   clear                   fnlry09ds2
057500140926     c                   eval      ilry09tch = 'C'
057600141013     c                   exsr      verdispo
057700141029     c                   if        OLRY09ESal = '1' and
057800140926     c                             OLRY09ERR  = *blank
057900140926     c                   seton                                        97
058000140926     c                   endif
058100950517     C*
058200950517     C                   ENDSR
058300141013      *___________________________________________________________
058400141013     c     verdispo      begsr
058500141013      *___________________________________________________________
058600141029     c                   eval      ilry09tla = 'L'
058700141029     c                   eval      ilry09fgs = v1cfil
058800141029     c                   eval      ilry09tdis= 'E'
058900141029     c                   movel     fnlry09ds2    kpjbu
059000141013     c                   call      'FNLRY09R'
059100141013     c                   parm                    kpjba
059200141029     c                   movel     kpjbu         fnlry09ds2
059300141013     C                   ENDSR
059400141013      *___________________________________________________________
059500950524**
059600030910Numero distinta obbligatorio                                          01
059700030625Numero distinta inesistente o annullato                               02
059800030625Limiti incompatibilii: numero distinta al minore numero distinta dal  03
059900950522Distinta già chiusa                                                   04
060000950522Non trovate distinte entro i limiti indicati                          05
060100950522Data errata                                                           06
060200950522Data distinta obbligatoria                                            07
060300070201Impossibile gestire distinta                                          08
060400950524Distinta scelta non compatibile con tipo prestazione selezionato      09
060500070205Distinta non congruente con il tipo distinta consegna                 10
060600070216L'elaborazione è terminata con errore ... controllare la stampa       11
060700080326La distinta è in una fase per la quale non è + possibile l'elaboraz.  12
060800080326Ci sono distinte in una fase per la quale non è + possibile l'elabor. 13
060900070824Ora errata                                                            14
061000070919Obbligatorio scegliere il tipo uscita                                 15
061100160420I valori immessi per il Turno con sono corretti                       16
061200160420Distinta non congruente con il turno                                  17
