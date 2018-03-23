000100950517     H DECEDIT('0,') DATEDIT(*DMY.)
000200030625     H* FNLRF6R *----------------------------------------------------*
000300030625     H*         Generazione e quadratura distinte AUT
000400950517     H*--------------------------------------------------------------*
000500030625     FFNLRF6D   CF   E             WORKSTN
000600050519     Fazorg01L  IF   E           K DISK
000700050519     FFNFVV01L  IF   E           K DISK
000800950522     FFNFVV02L  IF   E           K DISK
000900950522     F                                     RENAME(FNFVV000:FNFVV002)
001000070123     FFNBRV02L  IF   E           K DISK
001100950517     D*--------------------------------------------------------------*
001200950517     D*  Schiera errori
001300950517     D*--------------------------------------------------------------*
001400160525     D ERR             S             70    DIM(14) CTDATA PERRCD(1)
001500950517     D*--------------------------------------------------------------*
001600950517     D*  DS
001700950517     D*--------------------------------------------------------------*
001800160525     d TRULVPODS     e ds
001900160525     d   sk_PDA               16    765    inz   dim(250)
002000950517     D KPJBA         E DS
002100151007     d kpjbus          S                   like(KPJBU)
002200050520     D dfvvspn       E DS
002300950517     D***
002400950517     D* PARAMETRI PER CONTROLLO DATA E TRASFORM IN GIORNI - XSRDA8 -
002500950517     D***
002600950517     D WLBDAT          DS                  INZ
002700950517     D  G02DAT                 1      8  0
002800950517     D  G02INV                 9     16  0
002900950517     D  G02ERR                17     17
003000950517     D  G02TGI                18     22  0
003100950517     D***
003200950517     D* PARAMETRI PER TRASFORM GIORNI IN DATA             - XSRDGI -
003300950517     D***
003400950517     D WGIDAT          DS                  INZ
003500950517     D  GIODAT                 1      8  0
003600950517     D  GIOINV                 9     16  0
003700950517     D  GIOTGI                17     21  0
003800950517     D* DS PER FNLV50F - CONTROLLO FILIALE GESTIONE
003900950517     D FNLV50        E DS                  EXTNAME(FNLV50DS)
004000950517     D*
004100050519     D og150         E DS
004200160719     D fidg31ds      E DS
004300030703     D FNLR59ds      E DS
004400030710     D ddatiute      e ds
004500030710     D azuteds       e ds                  extname(AZUTE00F)
004600030710     D tibs34ds      E DS                  inz
004700151007     D FNLRC6ds      E DS
004800160719     d $FV6            s              6P 0 dim(600)                             F.V.  x FIDG31R
004900950517     C*--------------------------------------------------------------*
005000950517     C*  Ciclo principale
005100950517     C*--------------------------------------------------------------*
005200950517     C* Inizializzazione prima videata
005300950517     C                   EXSR      INZD01
005400950517     C* Loop di gestione videata
005500950517     C     WFINE         DOUEQ     'S'
005600950517     C     WTPVID        CASEQ     '1'           GESD01
005700950517     C                   END
005800950517     C                   END
005900950517     C* Fine Pgm.
006000950517     C                   SETON                                        LR
006100950517     C*--------------------------------------------------------------*
006200950517     C*  Inizializzazione parametri prima videata
006300950517     C*--------------------------------------------------------------*
006400950522     C     INZD01        BEGSR
006500950517     C*
006600950602     C                   SETOFF                                       0302
006700950522     C                   MOVEL     *BLANKS       V1CFPD
006800061003     C                   MOVEL     'S'           V1Cbai
006900061003     C                   MOVEL     'A'           V1CSTP
007000030822     C                   MOVEL     'N'           V1CSOR
007100160428     C                   MOVEL     '1'           V1Ctur1
007200160428     C                   MOVEL     '2'           V1Ctur2
007300950517     C                   MOVEL     *BLANKS       V1CNDD
007400950606     C                   MOVEL     *BLANKS       V1DNDD
007500950522     C                   Z-ADD     WHHDAT        V1CDDC
007600950517     C                   Z-ADD     0             V1CNDA
007700030910     C     dutlpo        IFEQ      '2'
007800030910     C                   MOVEL     dutpou        V1CFIL
007900030910     C                   ELSE
008000030910     C                   MOVEL     SIMFEL        V1CFIL
008100030910     C                   END
008200050520     C* parcel + messaggerie
008300050520     c                   exsr      srtpm
008400950522     C                   MOVE      V1CFIL        WSVFIL            3 0
008500031020     C                   CLEAR                   FNLR59DS
008600950517     C*
008700950517     C                   ENDSR
008800050520     C*--------------------------------------------------------------*
008900050520     C*  parcel + messaggerie
009000050520     C*--------------------------------------------------------------*
009100050520     C     srtpm         BEGSR
009200050520     c* leggo organigramma x gestire ind 10 parcel+messaggerie
009300050520     c     v1cfil        chain     azorg01l
009400050520     c                   if        %found(azorg01l)
009500050520     c                   movel     orgdf0        og150
009600050520     c     §OGPKM        comp      'S'                                    10
009700160428     c     §OGturno      comp      'S'                                    11
009800050520     c                   end
009900160909     c* abilito l'eliminazione delle spedizioni dalla distinta pt88
010000160525     c                   move      '999'         fgsa              3
010100160525     c     fgsa          lookup    sk_pda                                 37
010200160525     c  n37              move      v1cfil        fgsa
010300160525     c  n37fgsa          lookup    sk_pda                                 37
010301160909     c* se doppio turno abilito comunque
010302160909     c                   if        *in11
010303160909     c                   seton                                        37
010304160909     c                   end
010400160609     c                   eval      v1cp88 = 'N'
010500050520     C                   ENDSR
010600950517     C*--------------------------------------------------------------*
010700950517     C*  Gestione prima videata
010800950517     C*--------------------------------------------------------------*
010900950517     C     GESD01        BEGSR
011000950517     C*
011100030703     C                   EXFMT     LRf6D01
011200950524     C                   SETOFF                                       284244
011300950517     C                   SETOFF                                       404143
011400160504     C                   SETOFF                                       45
011500950522     C                   MOVEL     *BLANKS       WNDT
011600950529     C                   Z-ADD     0             WNDD
011700950606     C                   MOVEL     *BLANKS       V1DNDD
011800950517     C*  Fine Pgm.
011900950517     C     *INKC         IFEQ      '1'
012000950517     C                   MOVEL     'S'           WFINE
012100950517     C                   END
012200950614     C*
012300950614     C   51              SETOFF                                       51
012400950614     C*
012500950614     C     *IN50         IFEQ      '1'
012600950614     C     WSVFIL        ORNE      V1CFIL
012700950614     C                   CLEAR                   FNLV50
012800950614     C                   MOVEL     KNMUS         D50PRU
012900950614     C                   MOVEL     V1CFIL        D50FGS
013000950614     C                   CALL      'FNLV50R'
013100950614     C                   PARM                    FNLV50
013200950614     C                   SETOFF                                       50
013300950614     C                   MOVE      V1CFIL        WSVFIL            3 0
013400050520     C* parcel + messaggerie
013500050520     c                   exsr      srtpm
013600050520     c                   end
013700950602     C*  Sblocco data
013800950602     C     *INKA         IFEQ      '1'
013900950602     C                   SETON                                        5103
014000950602     C                   GOTO      FINVD1
014100950602     C                   END
014200950517     C*  Cambio filiale
014300950517     C     *INKS         IFEQ      '1'
014400950602     C                   SETON                                        5002
014500950517     C                   GOTO      FINVD1
014600950517     C                   END
014700950517     C*  Controlli
014800950517     C                   EXSR      CTR01
014900950517     C*  Conferma
015000030910     C                   IF        not *in28 and (*inkf or *inkv)
015100050426     c* emetto window per chiedere la stampante  laser o ad aghi
015200050426     c                   if        v1cstp = 'A'
015300050426     C                   EXSR      wind60
015400050426     c                   if        laser = ' '
015500050426     c                   seton                                        28
015600050426     c                   clear                   $msg
015700050426     c                   goto      finvd1
015800050426     c                   end
015900050427     c                   else
016000050427     c                   eval      laser='N'
016100050426     C                   END
016200950517     C                   EXSR      SBMP60
016300950517     C                   EXSR      INZD01
016400050426     C                   END
016500950517     C*
016600950517     C     FINVD1        ENDSR
016700950522     C*--------------------------------------------------------------*
016800950522     C*  Controlli prima videata
016900950522     C*--------------------------------------------------------------*
017000950522     C     CTR01         BEGSR
017100950522     C*
017200950522     C*  Controllo filiale
017300030710     c                   do
017400950522     C     D50ERR        IFNE      ' '
017500950522     C                   SETON                                        2840
017600950522     C                   MOVEL     D50MSG        $MSG
017700030710     C                   leave
017800950522     C                   ENDIF
017900950522     C*  Data distinta
018000950522     C     V1CDDC        IFEQ      0
018100950522     C                   SETON                                        4328
018200950522     C                   MOVEL     ERR(7)        $MSG
018300030710     C                   leave
018400950522     C                   ELSE
018500950522     C                   MOVE      V1CDDC        G02DAT
018600950522     C                   MOVEL     *BLANKS       G02ERR
018700950522     C                   CALL      'XSRDA8'
018800950522     C                   PARM                    WLBDAT
018900950522     C     G02ERR        IFEQ      '1'
019000950522     C                   SETON                                        2843
019100950522     C                   MOVEL     ERR(6)        $MSG
019200030710     C                   leave
019300950522     C                   END
019400950522     C                   END
019500950522     C                   Z-ADD     G02INV        WDDT
019600950522     C                   Z-ADD     G02DAT        V1CDDC
019700950522     C*
019800950522     C                   Z-ADD     4             KNPG
019900950522     C                   Z-ADD     V1CFIL        KFGS
020000950522     C                   Z-ADD     WDDT          KDFV
020100950522     C     KFVV2         CHAIN     FNFVV02L                           3132
020200950522     C     *IN31         IFEQ      '1'
020300950522     C                   SETON                                        4328
020400950522     C                   MOVEL     ERR(5)        $MSG
020500030710     C                   leave
020600950522     C                   END
020700950522     C*
020800950522     C*  Numero distinta da
020900950522     C     V1CNDD        IFNE      *ZEROS
021000950522     C     V1CNDD        ANDNE     *BLANKS
021100950522     C* Ricerca
021200950522     C     '?'           SCAN      V1CNDD                                 32
021300950522     C     *IN32         IFEQ      '1'
021400160719     C                   Z-ADD     0             fdgNFV
021500160719     C                   MOVEL     V1CFIL        fdgFGS
021600160720     C                   Z-ADD     wddt          fdgGDA
021700160719     C                   Z-ADD     wddt          fdgGAL
021800160719     C                   MOVEL     v1cstp        fdgstp
021900160719     C                   MOVEL     'N'           V1CSOR
022000160719     C                   MOVEL     v1ctpm        fdgtdis
022100160719     C                   MOVEL     v1cfpd        fdgtusc
022200160719     C                   MOVEL     V1Ctur1       FDGTUR1
022300160719     C                   MOVEL     V1Ctur2       FDGTUR2
022400160719     C                   MOVEL     fidg31ds      KPJBU
022500160719     c                   clear                   $FV6
022600160719      * impostato a tutti 99999
022700160719     c                   do        600           yy                3 0
022800160719     c                   z-add     999999        $FV6(yy)
022900160719     c                   enddo
023000160719     C                   CALL      'FIDG31R'
023100950522     C                   PARM                    KPJBA
023200160719     C                   PARM                    $fv6
023300160719     C                   MOVEL     KPJBU         fidg31ds
023400160719     C                   MOVE      fdgNFV        V1CNDD
023500950522     C                   END
023600950522     C*
023700950522     C                   SETOFF                                       31
023800950522     C                   TESTN                   V1CNDD               30
023900950522     C  N30              SETON                                        31
024000950522     C     *IN30         IFEQ      '1'
024100950522     C                   MOVE      V1CNDD        KNFV
024200031020     C                   MOVE      V1CNDD        KNFV1
024300950522     C                   MOVE      V1CNDD        WNDT
024400950522     C     KFVV1         CHAIN     FNFVV01L                           3132
024500950522     C                   END
024600950522     C     *IN31         IFEQ      '1'
024700950602     C     FVVATB        OREQ      '*'
024800950522     C                   SETON                                        4128
024900950522     C                   MOVEL     ERR(2)        $MSG
025000030710     C                   leave
025100160719     C                   END
025200950522     C     *IN32         IFEQ      '1'
025300950522     C     FVVFCF        OREQ      'A'
025400950522     C     FVVFCF        OREQ      'F'
025500950522     C                   SETON                                        4128
025600950522     C                   MOVEL     ERR(8)        $MSG
025700030710     C                   leave
025800950522     C                   END
025900950522     C     FVVFCF        IFEQ      'S'
026000950522     C                   SETON                                        4128
026100950522     C                   MOVEL     ERR(4)        $MSG
026200030710     C                   leave
026300950522     C                   END
026400950606     C     FVVDFV        IFNE      WDDT
026500950606     C                   SETON                                        414328
026600950606     C                   MOVEL     ERR(5)        $MSG
026700030710     C                   leave
026800950606     C                   END
026900950524     C     V1CFPD        IFNE      *BLANKS
027000950524     C     V1CFPD        ANDNE     FVVFPP
027100950524     C                   SETON                                        414428
027200950524     C                   MOVEL     ERR(9)        $MSG
027300030710     C                   leave
027400950524     C                   END
027500950522     C                   MOVEL     FVVDSF        V1DNDD
027600050520     C                   MOVEL     FVVspn        dfvvspn
027700031020     C* se sto lanciando il pgm "da spunte" controllo che ne esista almeno una
027800031020     c                   if        kcdaz = 'LR59'
027900070123     C     kbrv          CHAIN     FNBRV02L
028000070123     C                   if        not %found(fnbrv02l)
028100031020     C                   SETON                                        4128
028200031020     C                   MOVEL     err(10)       $MSG
028300031020     C                   leave
028400031020     C                   end
028500031020     C                   end
028600950522     C                   MOVE      V1CNDD        WNDD              7 0
028700950522     C*
028800950522     C     V1CNDA        IFEQ      *ZEROS
028900950522     C                   MOVE      V1CNDD        V1CNDA
029000950522     C                   END
029100950522     C                   END
029200030910     C*  la distinta finale non può essere a 0
029300030910     c     v1cnda        ifeq      0
029400030910     C                   SETON                                        4228
029500030910     C                   MOVEL     ERR(1)        $MSG
029600030910     c                   end
029700950522     C*  Limiti incompatibili
029800950522     C     WNDD          IFGT      V1CNDA
029900950522     C                   SETON                                        414228
030000950522     C                   MOVEL     ERR(3)        $MSG
030100950522     C                   END
030200160525     c* se scelto di eliminare le spedizioni pt 88 da distinta occorre
030300160525     c* aver sacelto un range di sped.
030400160525     c                   if        v1cp88 = 'S' and wndd = v1cnda
030500160525     C                   SETON                                        414228
030600160525     C                   MOVEL     ERR(14)       $MSG
030700160525     C                   END
030800050520     c* se parcel + messaggerie se scelta una sola distinta controllo che
030900050520     c* il tipo scelto sia congruente
031000050525     c                   if        *in10 and not *in28  and wndd <> 0
031100050523     c                             and v1ctpm <> ' '
031200050523     c                   if        (v1ctpm = 'P' and §fvvtpm <>' ') or
031300050523     c                             (v1ctpm = 'M' and §fvvtpm <>'M')
031400050520     C                   SETON                                        414228
031500050520     C                   MOVEL     ERR(11)       $MSG
031600050520     c                   end
031700050523     c                   end
031800160428     c* se turno e se scelta una sola distinta controllo che
031900160428     c* il tipo scelto sia congruente
032000160428     c                   if        *in11 and not *in28  and wndd <> 0
032100160428     c                             and (v1ctur1 = ' ' or v1ctur2 = ' ')
032200160428     c                             and §fvvtpm= ' '
032300160428     c                   select
032400160428     c                   when      (v1ctur1 = '1' or
032500160428     c                              v1ctur2 = '1') and
032600160505     c                             §fvvturno<> ' '
032700160428     C                   SETON                                        28
032800160428     c                   when      (v1ctur1 = '2' or
032900160428     c                              v1ctur2 = '2') and
033000160505     c                             §fvvturno <> '2'
033100160428     C                   SETON                                        28
033200160428     c                   endsl
033300160428     c                   if        *in28
033400160504     c                   seton                                        45
033500160428     C                   MOVEL     ERR(12)       $MSG
033600160428     C                   leave
033700160428     c                   end
033800160428     c                   end
033900160504     c* se parcel con + distinte
034000160505     c                   if        *in11 and not *in28
034100160504     c                             and v1ctur1 = ' ' and v1ctur2 = ' '
034200160505     c                             and (v1ctpm= 'P' or v1ctpm = ' ')
034300160504     c                   seton                                        4528
034400160504     C                   MOVEL     ERR(13)       $MSG
034500160504     C                   leave
034600160504     c                   end
034700030710     C                   ENDdo
034800030710     C*
034900030710     C                   ENDSR
035000950522     C*--------------------------------------------------------------*
035100950522     C*  Sottometto preparazione distinta da spunte
035200950522     C*--------------------------------------------------------------*
035300050426     C     wind60        BEGSR
035400950522     C*
035500050426     c                   clear                   laser             1
035600050426     c                   call      'FNLRF6R1'
035700050426     c                   parm                    laser
035800050426     C*
035900050426     C                   ENDSR
036000050426     C*--------------------------------------------------------------*
036100050426     C*  Sottometto preparazione distinta da spunte
036200050426     C*--------------------------------------------------------------*
036300050426     C     SBMP60        BEGSR
036400151009     C*
036500151009     c* controlla
036600151009     c                   if        V1CNDD <> %editc(V1CNDA:'X')
036700151009     c                   eval      kpjbus = kpjbu
036800151009     c                   clear                   kpjbu
036900151009     c                   eval      ILRC6DAT = Wddt
037000151009     c                   eval      ILRC6LNA = v1cFIL
037100151009     c                   eval      kpjbu  = fnlrc6ds
037200151009     c                   call      'FNLRC6R'                            99
037300151009     c                   parm                    kpjba
037400151009     c                   eval      kpjbu  = kpjbus
037500151009     c                   end
037600050426     C*
037700050426     C                   eval      d59lsr = laser
037800050426     C                   Z-ADD     V1CFIL        d59FGS
037900030703     C                   Z-ADD     WDDT          d59DDT
038000030703     C                   MOVE      V1CNDD        d59NDD
038100030703     C                   Z-ADD     V1CNDA        d59NDA
038200030703     C     d59NDA        IFEQ      0
038300030703     C                   Z-ADD     9999999       d59NDA
038400950526     C                   END
038500030703     C                   MOVEL     V1CFPD        d59FPD
038600070601     C                   clear                   d59TPD
038700030822     C                   MOVEL     v1cstp        d59STP
038800061003     C                   MOVEL     v1cbai        d59bai
038900030822     C                   MOVEL     v1csor        d59SOR
039000050520     C                   MOVEL     v1ctpm        d59tpm
039100160428     C                   MOVEL     v1ctur1       d59tur1
039200160428     C                   MOVEL     v1ctur2       d59tur2
039300160525     C                   MOVEL     v1cp88        d59pt88
039400030703     C                   MOVEL(P)  FNLR59DS      KPJBU
039500030910     C*  per f6 interattivo per f21 batch
039600040407     c* per + distinte forzo la coda KEDPCED
039700040525     C     *inkv         IFEQ      '1'
039800040407     c                   if        d59nda <> d59ndd
039900040407     c                   eval      kqieb = 'KEDPCED'
040000040407     c                   else
040100040407     c                   clear                   kqieb
040200040407     c                   end
040300031017     c* pgm "da spunte"
040400031017     c                   if        kcdaz = 'LR59'
040500031021     c                   movel     'LR60'        kcoaz
040600031017     c                   else
040700031017     c* pgm "quadratura"
040800030910     C                   MOVEL     'LRF7'        KCOAZ
040900041021     c                   end
041000030910     C                   CALL      'BCH10'
041100030910     C                   PARM                    KPJBA
041200040407     C                   END
041300040525     c*
041400040525     C     *inkf         IFEQ      '1'
041500040407     C                   CALL      'FNLRF7R5'
041600040407     C                   PARM                    KPJBA
041700040407     C                   END
041800950522     C*
041900950522     C                   ENDSR
042000950517     C*--------------------------------------------------------------*
042100950517     C*  Operazioni iniziali
042200950517     C*--------------------------------------------------------------*
042300950517     C     *INZSR        BEGSR
042400950517     C*
042500950517     C     *ENTRY        PLIST
042600950517     C                   PARM                    KPJBA
042700950517     C*------------------------------------------------------*
042800030710     c     *dtaara       define    §azute        azuteds
042900030710     c     *dtaara       define    §datiute      ddatiute
043000030710     C                   in(E)     *dtaara
043100030710     C                   IF        %Error  or  RSUT = *blanks
043200030710     C                   call      'TIBS34R'
043300030710     C                   parm                    Tibs34Ds
043400030710     C                   in        *dtaara
043500030710     c                   ENDIF
043600160525     c* ricerca P.O. abilitati da tabella VPO per step di test
043700160525     c                   clear                   trulvpods
043800160526     c                   eval      ivpoke1 = 'FNLRF6R'
043900160525     c                   call      'TRULVPOR'
044000160525     c                   parm                    trulvpods
044100950517     C*------------------------------------------------------*
044200950517     C*  Definisco chiavi di accesso
044300950522     C     KFVV1         KLIST
044400950522     C                   KFLD                    KNPG
044500950522     C                   KFLD                    KNFV
044600950522     C                   KFLD                    KFGS
044700950522     C     KFVV2         KLIST
044800950517     C                   KFLD                    KNPG
044900950517     C                   KFLD                    KDFV
045000950522     C                   KFLD                    KFGS
045100031020     C     KbrV          KLIST
045200031020     C                   KFLD                    KNPG
045300031020     C                   KFLD                    KNFV1
045400031020     C                   KFLD                    KFGS
045500950517     C*------------------------------------------------------*
045600950517     C*  Definisco campi
045700950517     C     *LIKE         DEFINE    FVVNPG        KNPG
045800950517     C     *LIKE         DEFINE    FVVDFV        KDFV
045900950517     C     *LIKE         DEFINE    FVVFGS        KFGS
046000950522     C     *LIKE         DEFINE    FVVNFV        KNFV
046100950522     C     *LIKE         DEFINE    FVVNFV        WNDT
046200950522     C     *LIKE         DEFINE    FVVDFV        WDDT
046300031020     C     *LIKE         DEFiNe    BRVNFV        KNFV1
046400950517     C*------------------------------------------------------*
046500950517     C*  Inizializzo campi
046600160719     C                   CLEAR                   fidg31ds
046700950517     C                   CLEAR                   FNLV50
046800030703     C                   CLEAR                   FNLR59DS
046900160719     C                   MOVEL     4             fdgNPG
047000160719     C                   MOVEL     'FNLRF6R'     fdgPGM
047100160719     C                   MOVEL     'S'           fdgRIC
047200160719     C                   MOVEL     '2'           fdgTFV
047300950517     C                   Z-ADD     4             KNPG
047400950522     C                   MOVEL     'N'           WFINE             1
047500950522     C                   MOVEL     '1'           WTPVID            1
047600950517     C*
047700950517     C* DATA DEL GIORNO
047800950522     C                   TIME                    WHHDAT           14 0
047900950522     C                   MOVE      WHHDAT        G02DAT
048000950522     C                   MOVEL     *BLANKS       G02ERR
048100950522     C                   CALL      'XSRDA8'
048200950522     C                   PARM                    WLBDAT
048300950522     C                   MOVE      G02INV        WOGGI             8 0
048400950517     C**
048500950517     C* VEDO SE SONO SIMFEL O REMOTO
048600950517     C**
048700030710     C     dutlpo        IFEQ      '2'
048800030710     C                   MOVEL     dutpou        V1CFIL
048900950517     C                   ELSE
049000030910     c                   seton                                        01
049100950517     C                   MOVEL     SIMFEL        V1CFIL
049200950517     C                   END
049300090212     c*
049400090212     c     kcdaz         comp      'LR59'                                 88
049500950517     C*
049600950517     C                   ENDSR
049700950524**
049800030910Numero distinta obbligatorio                                          01
049900030625Numero distinta inesistente o annullato                               02
050000030625Limiti incompatibilii: numero distinta al minore numero distinta dal  03
050100950522Distinta già chiusa                                                   04
050200950522Non trovate distinte entro i limiti indicati                          05
050300950522Data errata                                                           06
050400950522Data distinta obbligatoria                                            07
050500950522Impossibile gestire distinta/e attendi 5'' e ritenta                  08
050600950524Distinta scelta non compatibile con tipo prestazione selezionato      09
050700031020Non esistono spunte per il numero distinta immesso                    10
050800050523Distinta non congruente con il tipo distinta consegna                 11
050900160428Distinta non congruente con il turno                                  12
051000160504Per distinte Parcel è obbligatoria la scelta del turno                13
051100160525Non è possibile eliminare le spedizioni con pt88 per una sola distinta14
