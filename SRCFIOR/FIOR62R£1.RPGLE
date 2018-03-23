000100950517     H DECEDIT('0,') DATEDIT(*DMY.)
000200070201     H* FIDG17R *----------------------------------------------------*
000300070322     H*         Lancio assegnazione automatica distinte consegna
000400950517     H*--------------------------------------------------------------*
000500070201     FFidg17D   CF   E             WORKSTN
000600050519     Fazorg01L  IF   E           K DISK
000700070410     FFIDST09L  IF   E           K DISK
000800070410     FFIDST08L  IF   E           K DISK
000900070201     F                                     RENAME(fidst000:fidst002)
001000070123     FFNBRV02L  IF   E           K DISK
001100950517     D*--------------------------------------------------------------*
001200950517     D*  Schiera errori
001300950517     D*--------------------------------------------------------------*
001400070627     D ERR             S             70    DIM(13) CTDATA PERRCD(1)
001500950517     D*--------------------------------------------------------------*
001600950517     D*  DS
001700950517     D*--------------------------------------------------------------*
001800950517     D KPJBA         E DS
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
003701070313     D OG146         E DS                  INZ
003800050519     D og150         E DS
003900050519     D DSLR01        E DS
004000070201     D Fidg17ds      E DS
004100030710     D ddatiute      e ds
004200030710     D azuteds       e ds                  extname(AZUTE00F)
004300030710     D tibs34ds      E DS                  inz
004301070627     d savndd          s                   like(wndd)
004302070627     d savnda          s                   like(v1cnda)
004303070627     d savtpm          s                   like(v1ctpm)
004304070627     d savddt          s                   like(wddt)
004305070627     d savfil          s                   like(v1cfil)
004400950517     C*--------------------------------------------------------------*
004500950517     C*  Ciclo principale
004600950517     C*--------------------------------------------------------------*
004700950517     C* Inizializzazione prima videata
004800950517     C                   EXSR      INZD01
004900950517     C* Loop di gestione videata
005000950517     C     WFINE         DOUEQ     'S'
005100950517     C     WTPVID        CASEQ     '1'           GESD01
005200950517     C                   END
005300950517     C                   END
005400950517     C* Fine Pgm.
005500950517     C                   SETON                                        LR
005600950517     C*--------------------------------------------------------------*
005700950517     C*  Inizializzazione parametri prima videata
005800950517     C*--------------------------------------------------------------*
005900950522     C     INZD01        BEGSR
006000950517     C*
006100950602     C                   SETOFF                                       0302
006101070313     C*                  clear                   V1Cfpp
006400070201     C                   MOVEL     'N'           V1CCLEAR
006401070313     C                   MOVEL     'N'           V1CCLRMAN
006402070201     c                   clear                   v1cret
006700050523     C                   MOVEL     *blanks       V1Ctpm
006701070313     C                   Z-ADD     WHHDAT        V1CDDC
006800950517     C                   MOVEL     *BLANKS       V1CNDD
007100070313     C                   Z-ADD     99999         V1CNDA
007200030910     C     dutlpo        IFEQ      '2'
007300030910     C                   MOVEL     dutpou        V1CFIL
007400030910     C                   ELSE
007500030910     C                   MOVEL     SIMFEL        V1CFIL
007600030910     C                   END
007700050520     C* parcel + messaggerie
007800050520     c                   exsr      srtpm
007900950522     C                   MOVE      V1CFIL        WSVFIL            3 0
008000070201     C                   CLEAR                   Fidg17DS
008100950517     C*
008200950517     C                   ENDSR
008300050520     C*--------------------------------------------------------------*
008400050520     C*  parcel + messaggerie
008500050520     C*--------------------------------------------------------------*
008600050520     C     srtpm         BEGSR
008700050520     c* leggo organigramma x gestire ind 10 parcel+messaggerie
008800050520     c     v1cfil        chain     azorg01l
008900050520     c                   if        %found(azorg01l)
009000050520     c                   movel     orgdf0        og150
009001070313     C                   MOVEL     ORGDE6        og146
009100050520     c     §OGPKM        comp      'S'                                    10
009200070313     c                   end
009201070313     c* se p.o. non partito esco
009202070313     c                   if        §ogdda = ' '
009203070313     c                   seton                                        lr
009204070313     c                   return
009205070313     c                   end
009300050520     C                   ENDSR
009400950517     C*--------------------------------------------------------------*
009500950517     C*  Gestione prima videata
009600950517     C*--------------------------------------------------------------*
009700950517     C     GESD01        BEGSR
009800950517     C*
009900070201     C                   EXFMT     dg17D01
010000950524     C                   SETOFF                                       284244
010100950517     C                   SETOFF                                       404143
010200950522     C                   MOVEL     *BLANKS       WNDT
010300950529     C                   Z-ADD     0             WNDD
010500950517     C*  Fine Pgm.
010600950517     C     *INKC         IFEQ      '1'
010700950517     C                   MOVEL     'S'           WFINE
010800950517     C                   END
010900950614     C*
011000950614     C   51              SETOFF                                       51
011100950614     C*
011200950614     C     *IN50         IFEQ      '1'
011300950614     C     WSVFIL        ORNE      V1CFIL
011400950614     C                   CLEAR                   FNLV50
011500950614     C                   MOVEL     KNMUS         D50PRU
011600950614     C                   MOVEL     V1CFIL        D50FGS
011700950614     C                   CALL      'FNLV50R'
011800950614     C                   PARM                    FNLV50
011900950614     C                   SETOFF                                       50
012000950614     C                   MOVE      V1CFIL        WSVFIL            3 0
012100050520     C* parcel + messaggerie
012200050520     c                   exsr      srtpm
012300050520     c                   end
012400950602     C*  Sblocco data
012500950602     C     *INKA         IFEQ      '1'
012600950602     C                   SETON                                        5103
012601070627     c                   setoff                                       29
012700950602     C                   GOTO      FINVD1
012800950602     C                   END
012900950517     C*  Cambio filiale
013000950517     C     *INKS         IFEQ      '1'
013100950602     C                   SETON                                        5002
013101070627     c                   setoff                                       29
013200950517     C                   GOTO      FINVD1
013300950517     C                   END
013400950517     C*  Controlli
013500950517     C                   EXSR      CTR01
013600950517     C*  Conferma
013700070201     C                   IF        not *in28 and *inkf
014900950517     C                   EXSR      SBMP60
014901070205     c                   if        not *in28
014902070412     C                   MOVEL     'S'           WFINE
014904070205     c                   end
014905070205     C                   END
015200950517     C*
015300950517     C     FINVD1        ENDSR
015400950522     C*--------------------------------------------------------------*
015500950522     C*  Controlli prima videata
015600950522     C*--------------------------------------------------------------*
015700950522     C     CTR01         BEGSR
015800950522     C*
015900950522     C*  Controllo filiale
016000030710     c                   do
016100950522     C     D50ERR        IFNE      ' '
016200950522     C                   SETON                                        2840
016300950522     C                   MOVEL     D50MSG        $MSG
016400030710     C                   leave
016500950522     C                   ENDIF
016600950522     C*  Data distinta
016700950522     C     V1CDDC        IFEQ      0
016800950522     C                   SETON                                        4328
016900950522     C                   MOVEL     ERR(7)        $MSG
017000030710     C                   leave
017100950522     C                   ELSE
017200950522     C                   MOVE      V1CDDC        G02DAT
017300950522     C                   MOVEL     *BLANKS       G02ERR
017400950522     C                   CALL      'XSRDA8'
017500950522     C                   PARM                    WLBDAT
017600950522     C     G02ERR        IFEQ      '1'
017700950522     C                   SETON                                        2843
017800950522     C                   MOVEL     ERR(6)        $MSG
017900030710     C                   leave
018000950522     C                   END
018100950522     C                   END
018200950522     C                   Z-ADD     G02INV        WDDT
018300950522     C                   Z-ADD     G02DAT        V1CDDC
018301070323     c* la data distinta deve essere >= oggi
018302070413     c                   if        wddt < woggi
018303070413     C                   SETON                                        2843
018304070413     C                   MOVEL     ERR(6)        $MSG
018305070413     C                   leave
018306070413     C                   END
018400950522     C*
018500950522     C                   Z-ADD     4             KNPG
018600950522     C                   Z-ADD     V1CFIL        KFGS
018700950522     C                   Z-ADD     WDDT          KDFV
018800070410     C     KDST2         CHAIN     FIDST08L                           3132
018900950522     C     *IN31         IFEQ      '1'
019000950522     C                   SETON                                        4328
019100950522     C                   MOVEL     ERR(5)        $MSG
019200030710     C                   leave
019300950522     C                   END
019400950522     C*
019500950522     C*  Numero distinta da
019600950522     C     V1CNDD        IFNE      *ZEROS
019700950522     C     V1CNDD        ANDNE     *BLANKS
019800950522     C* Ricerca
019900950522     C     '?'           SCAN      V1CNDD                                 32
020000950522     C     *IN32         IFEQ      '1'
020100950522     C                   Z-ADD     0             DLRNFV
020200950522     C                   MOVEL     V1CFIL        DLRFGS
020300950522     C                   Z-ADD     V1CDDC        DLRGDA
020400950522     C                   Z-ADD     V1CDDC        DLRGAL
020500950522     C                   MOVEL     DSLR01        KPJBU
020600070313     C                   CALL      'FNLR02R'
020700950522     C                   PARM                    KPJBA
020800950522     C                   MOVEL     KPJBU         DSLR01
020901070202     C                   MOVE      DLRNFV        V1CNDD
021000950522     C                   END
021100950522     C*
021200950522     C                   SETOFF                                       31
021300950522     C                   TESTN                   V1CNDD               30
021400950522     C  N30              SETON                                        31
021500950522     C     *IN30         IFEQ      '1'
021600950522     C                   MOVE      V1CNDD        KNFV
021700031020     C                   MOVE      V1CNDD        KNFV1
021800950522     C                   MOVE      V1CNDD        WNDT
021900070410     C     KDST1         CHAIN     FIDST09L                           3132
022000950522     C                   END
022100950522     C     *IN31         IFEQ      '1'
022200070201     C     dstATB        OREQ      '*'
022300950522     C                   SETON                                        4128
022400950522     C                   MOVEL     ERR(2)        $MSG
022500030710     C                   leave
022600950522     C                   END
022700950522     C     *IN32         IFEQ      '1'
023000950522     C                   SETON                                        4128
023100950522     C                   MOVEL     ERR(8)        $MSG
023200030710     C                   leave
023300950522     C                   END
023400070201     C     dstFCF        IFEQ      'S'
023500950522     C                   SETON                                        4128
023600950522     C                   MOVEL     ERR(4)        $MSG
023700030710     C                   leave
023800950522     C                   END
023801070627     C     dstfasd       IFeq      'STP'
023802070627     C                   SETON                                        4428
023803070627     C                   MOVEL     ERR(12)       $MSG
023804070627     C                   leave
023805070627     C                   END
023900070201     C     dstDFV        IFNE      WDDT
024000950606     C                   SETON                                        414328
024100950606     C                   MOVEL     ERR(5)        $MSG
024200030710     C                   leave
024300950606     C                   END
024400070313     C*    V1CFPp        IFNE      *BLANKS
024500070313     C*    V1CFPp        ANDNE     dstFPP
024600070313     C*                  SETON                                        414428
024700070313     C*                  MOVEL     ERR(9)        $MSG
024800070313     C*                  leave
024900070313     C*                  END
026100950522     C                   MOVE      V1CNDD        WNDD              7 0
026200950522     C*
026300950522     C     V1CNDA        IFEQ      *ZEROS
026400950522     C                   MOVE      V1CNDD        V1CNDA
026500950522     C                   END
026600950522     C                   END
026700030910     C*  la distinta finale non può essere a 0
026800030910     c     v1cnda        ifeq      0
026900030910     C                   SETON                                        4228
027000030910     C                   MOVEL     ERR(1)        $MSG
027100030910     c                   end
027200950522     C*  Limiti incompatibili
027300950522     C     WNDD          IFGT      V1CNDA
027400950522     C                   SETON                                        414228
027500950522     C                   MOVEL     ERR(3)        $MSG
027600950522     C                   END
027700050520     c* se parcel + messaggerie se scelta una sola distinta controllo che
027800050520     c* il tipo scelto sia congruente
027900050525     c                   if        *in10 and not *in28  and wndd <> 0
028000050523     c                             and v1ctpm <> ' '
028100070201     c                   if        (v1ctpm = 'P' and dsttpm <>' ') or
028200070201     c                             (v1ctpm = 'M' and dsttpm <>'M')
028300050520     C                   SETON                                        414228
028400070201     C                   MOVEL     ERR(10)       $MSG
028500050520     c                   end
028600050523     c                   end
028601070627     c* se richiesta pulizia precedente assegnazione segnalo se una o +
028602070627     c* distinte sono state stampate
028604070627     c                   if        V1CCLEAR = 'S' and not *in28
028605070627     c                   if        wndd <> savndd or v1cnda <> savnda or
028606070627     c                             v1ctpm <> savtpm or v1cfil <> savfil
028607070627     c                             or wddt <> savddt
028608070627     c                   setoff                                       29
028609070627     c                   eval      savndd = wndd
028610070627     c                   eval      savnda = v1cnda
028611070627     c                   eval      savtpm = v1ctpm
028612070627     c                   eval      savfil = v1cfil
028613070627     c                   eval      savddt = wddt
028614070627     c                   end
028615070627     c                   if        not *in29
028616070627     C                   Z-ADD     4             KNPG
028617070627     C                   Z-ADD     V1CFIL        KFGS
028618070627     C                   Z-ADD     WDDT          KDFV
028619070627     C     KDST2         setll     FIDST08L
028620070627     c                   do        *hival
028621070627     C     KDST2         reade     FIDST08L
028622070627     c                   if        %eof(fidst08l)
028623070627     c                   leave
028624070627     c                   end
028625070627     c                   if        dstfasd = 'STP'
028626070627     c                   if        dstnfv >= wndd and dstnfv <= v1cnda
028627070627     c                   if        (v1ctpm = 'P' and dsttpm = ' ') or
028628070627     c                             (v1ctpm = 'M' and dsttpm ='M') or
028629070627     c                             v1ctpm = ' '
028630070627     C                   SETON                                        2928
028631070627     C                   MOVEL     ERR(13)       $MSG
028632070627     c                   leave
028633070627     c                   end
028634070627     c                   end
028635070627     c                   end
028700030710     C                   ENDdo
028701070627     c                   end
028702070627     c                   end
028703070627     c*
028704070627     C                   ENDdo
028800030710     C*
028900030710     C                   ENDSR
030000050426     C*--------------------------------------------------------------*
030100070201     C*  Sottometto preparazione distinta di consegna batch
030200050426     C*--------------------------------------------------------------*
030300050426     C     SBMP60        BEGSR
030400050426     C*
030401070205     c                   clear                   fidg17ds
030402070206     c                   eval      d17cmti = 'S'
030600070206     C                   Z-ADD     V1CFIL        d17FGSi
030700070206     C                   Z-ADD     WDDT          d17Dfvi
030800070206     C                   MOVE      V1CNDD        d17NfDi
030900070206     C                   Z-ADD     V1CNDA        d17NfAi
031000070206     C     d17NfAi       IFEQ      0
031100070206     C                   Z-ADD     9999999       d17NfAi
031200950526     C                   END
031201070206     C                   Z-ADD     v1cret        d17reti
031202070206     C                   eval      d17cleari= v1cclear
031203070313     C                   eval      D17CLRMAN= V1CCLRMAN
031300070327     C                   clear                   d17FPpi
031800070206     C                   MOVEL     v1ctpm        d17tpmi
031900070201     C                   MOVEL(P)  Fidg17DS      KPJBU
033500070206     C                   CALL      'FIDG18C'
033600030910     C                   PARM                    KPJBA
033601070215     C                   eval      Fidg17DS =   KPJBU
033602070206     c*
033603070206     c                   if        d17erro<> ' '
033604070205     c                   seton                                        28
033605070215     C                   MOVEL     ERR(11)       $MSG
033611070205     c                   end
034300950522     C*
034400950522     C                   ENDSR
034500950517     C*--------------------------------------------------------------*
034600950517     C*  Operazioni iniziali
034700950517     C*--------------------------------------------------------------*
034800950517     C     *INZSR        BEGSR
034900950517     C*
035000950517     C     *ENTRY        PLIST
035100950517     C                   PARM                    KPJBA
035101070313     c* per EDP abilito pulizia spediz. manuali, % rettifica e da distinta
035102070313     c* a distinta
035103070313     c                   if        %subst(knmus: 1: 3) = 'EDP'
035104070313     c                   seton                                        88
035105070313     c                   else
035106070313     c                   setoff                                       88
035107070313     c                   end
035200950517     C*------------------------------------------------------*
035300030710     c     *dtaara       define    §azute        azuteds
035400030710     c     *dtaara       define    §datiute      ddatiute
035500030710     C                   in(E)     *dtaara
035600030710     C                   IF        %Error  or  RSUT = *blanks
035700030710     C                   call      'TIBS34R'
035800030710     C                   parm                    Tibs34Ds
035900030710     C                   in        *dtaara
036000030710     c                   ENDIF
036100950517     C*------------------------------------------------------*
036200950517     C*  Definisco chiavi di accesso
036300070410     C     KDST1         KLIST
036301070410     C                   KFLD                    KFGS
036400950522     C                   KFLD                    KNPG
036500950522     C                   KFLD                    KNFV
036700070410     C     KDST2         KLIST
036701070410     C                   KFLD                    KFGS
036800950517     C                   KFLD                    KNPG
036900950517     C                   KFLD                    KDFV
037100031020     C     KbrV          KLIST
037200031020     C                   KFLD                    KNPG
037300031020     C                   KFLD                    KNFV1
037400031020     C                   KFLD                    KFGS
037500950517     C*------------------------------------------------------*
037600950517     C*  Definisco campi
037700070201     C     *LIKE         DEFINE    dstNPG        KNPG
037800070201     C     *LIKE         DEFINE    dstDFV        KDFV
037900070201     C     *LIKE         DEFINE    dstFGS        KFGS
038000070201     C     *LIKE         DEFINE    dstNFV        KNFV
038100070201     C     *LIKE         DEFINE    dstNFV        WNDT
038200070201     C     *LIKE         DEFINE    dstDFV        WDDT
038300031020     C     *LIKE         DEFiNe    BRVNFV        KNFV1
038400950517     C*------------------------------------------------------*
038500950517     C*  Inizializzo campi
038600950517     C                   CLEAR                   DSLR01
038700950517     C                   CLEAR                   FNLV50
038800070201     C                   CLEAR                   Fidg17DS
038900950517     C                   MOVEL     4             DLRNPG
039000070201     C                   MOVEL     'FIDG17R'     DLRPGM
039100950517     C                   MOVEL     'S'           DLRRIC
039200950517     C                   MOVEL     '2'           DLRTFV
039300950517     C                   Z-ADD     4             KNPG
039400950522     C                   MOVEL     'N'           WFINE             1
039500950522     C                   MOVEL     '1'           WTPVID            1
039600950517     C*
039700950517     C* DATA DEL GIORNO
039800950522     C                   TIME                    WHHDAT           14 0
039900950522     C                   MOVE      WHHDAT        G02DAT
040000950522     C                   MOVEL     *BLANKS       G02ERR
040100950522     C                   CALL      'XSRDA8'
040200950522     C                   PARM                    WLBDAT
040300950522     C                   MOVE      G02INV        WOGGI             8 0
040400950517     C**
040500950517     C* VEDO SE SONO SIMFEL O REMOTO
040600950517     C**
040700030710     C     dutlpo        IFEQ      '2'
040800030710     C                   MOVEL     dutpou        V1CFIL
040900950517     C                   ELSE
041000030910     c                   seton                                        01
041100950517     C                   MOVEL     SIMFEL        V1CFIL
041200950517     C                   END
041300950517     C*
041400950517     C                   ENDSR
041500950524**
041600030910Numero distinta obbligatorio                                          01
041700030625Numero distinta inesistente o annullato                               02
041800030625Limiti incompatibilii: numero distinta al minore numero distinta dal  03
041900950522Distinta già chiusa                                                   04
042000950522Non trovate distinte entro i limiti indicati                          05
042100950522Data errata                                                           06
042200950522Data distinta obbligatoria                                            07
042300070201Impossibile gestire distinta                                          08
042400950524Distinta scelta non compatibile con tipo prestazione selezionato      09
042600070205Distinta non congruente con il tipo distinta consegna                 10
042700070216L'elaborazione è terminata con errore ... controllare la stampa       11
042800070627LDV già stampate non è possibile la pulizia                           12
042900070627Ci sono LDV già stampate. Le relative distinte non verranno pulite    13
