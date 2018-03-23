000100020308      /TITLE Rettifiche conteggi
000200011221
000300930111     H DECEDIT('0,') DATEDIT(*DMY.)
000400011221
000500020320     FFNWLRC0F  UF A E           K DISK
000600020320     FFNWLRC1L  UF   E           K DISK
000700020320     F                                     RENAME(FNWLRC00:FNWC01)
000800021203     Ffiapd01L  IF   E           K DISK
000900030214     FFiCtt03L  uF a E           K DISK    usropn
001000020320     FFiftt02L  IF   E           K DISK
001100030128     Ffifre02L  uF a E           K DISK    usropn commit
001200020322     Ftabel00f  IF   E           K DISK
001300020320     Ftntbe01L  IF   E           K DISK
001400020320     Fficn40d   CF   E             WORKSTN
001500020308     F                                     SFILE(CA81SF1:SF1NRR)
001600020308     F                                     SFILE(CA81SF2:SF2NRR)
001700030428     F*---------------------------------------------------------------------------------
001800030428      * Per Controllare che non ci siano le trasmissioni in sede in corso
001900030428      *  durante l'apertura di questo programma si scrive un record su questo file.
002000030430     Ffiwalc1l  UF A E           K DISK    usropn commit
002100030428     F*---------------------------------------------------------------------------------
002200011221     D                SDS
002300011221     D PgmNam            *PROC
002400020308     D ficn40ds      E DS
002500030225     D trul52ds      E DS
002600891030     D KPJBA         E DS
002700020701     D* REM                    1      3
002800020701     D* REMFIL                 4      6
002900020318     D DSUL06        E DS                  EXTNAME(TRUL06DS)
003000020318     D  LIN                    1     90  0
003100020318     D                                     DIM(30)                              FIL. COMODO
003200020516     D dcre          E DS
003300030129     D dccg          E DS
003400030129     D ds8D          E DS
003500030522     d fgtds         e ds                  extname(fifgt00f)
003600021203     D fnlv24ds      E DS
003700020424     D ut§dse        E DS                  extname(ut§dse0f)
003800020318     D CNCR80        E DS
003900131007     D tibs42ds      E DS
004000131007     D uteautds      E DS
004100030129     D Tibs36ds      E DS
004200030128     D tntb40ds      E DS
004300020318     D WLBDAT          DS
004400020318     D  G02DAT                 1      8  0
004500020318     D  G02INV                 9     16  0
004600020318     D  G02ERR                17     17
004700020318     D  G02TGI                18     22  0
004800030128      *
004900030128     D WGIDAT          DS
005000030128     D  GIODAT                 1      8  0
005100030128     D  GIOINV                 9     16  0
005200030128     D  GIOTGI                17     21  0
005300030522     c* funzione per controllo tariffa
005400030522     D param           DS
005500030522     D  datfgt                        8s 0
005600030522     D  codfgt                        7s 0
005700030522     D  tipopre                       1
005800030522     d  dsfgtout                           like(fgtds)
005900030522     d  errfgt                        1
006000030128      *
006100020320     Dcomando          s             80    inz
006200020320     Dcmd              s             37    inz('STRCMTCTL LCKLVL(*CHG) CMTSCOPE-
006300020320     d                                     (*JOB)')
006400020320     Dcmd1             s              9    inz('ENDCMTCTL')
006500020320     Dlun              s             15  5 inz(80,00000)
006600020308     D datiso          S               d
006700020320     D dateur          S               d   datfmt(*eur)
006800020308     D L6              S              3  0 DIM(30)
006900020318     D cre             S              3    DIM(50)
007000160303     D msg             S             50    ctdata DIM(11)
007100020320     D dateu8          S                   like(*date)
007200030522     D data            S                   like(freddc)
007300030129      *
007400030128     D Autotr          C                   CONST('Autotrasportatore')
007500030128     D Cooper          C                   CONST('Cooperativa')
007600030514     D costif          C                   CONST('Punto operativo......')
007700030128     D Aut             C                   CONST('Autotrasp.')
007800030128     D Coop            C                   CONST('Coop.')
007900030529     D titvar          C                   CONST(' Variazione ai corrispettivi')
008000030529     D titsti          C                   CONST('Stime Costi/affitti figurativ-
008100030529     D                                     i')
008200011219
008300921222     C                   EXSR      DEFCAM
008400921222     C     *INKC         DOWEQ     '0'
008500020318     c* primo video
008600020319     C                   IF        §40flg = ' '
008700011221     C                   IF        *IN99
008800011221     C                   EVAL      *IN99 = *OFF
008900011221     C                   WRITE     CA81D1
009000011221     C                   EVAL      *IN99 = *ON
009100011221     C                   ENDIF
009200020318     c* se chiamato da menù emetto la prima videata di richiesta parametri
009300030514     c                   select
009400030514     c                   when      kfrtip = 'A'
009500030128     c                   movel     Autotr        PTIP
009600030514     c                   when      kfrtip = 'C'
009700030128     c                   movel     Cooper        PTIP
009800030514     c                   when      kfrtip = 'G'
009900030514     c                   movel     Costif        PTIP
010000030514     c                   endsl
010100030128     c                   move      ind55         *in55
010200030514     c                   move      ind56         *in56
010300921222     C                   EXFMT     CA81D1
010400020308     C                   ENDIF
010500020318     c* fine
010600020318     C                   IF        *INKC
010700020318     c                   leave
010800020318     C                   ENDIF
010900020318     c* controlli
011000921222     C                   EXSR      CTRD1
011100020321     c                   if        *in99
011200020321     c                   if        §40flg <> ' '
011300020321     c                   leave
011400020321     c                   else
011500020321     c                   iter
011600020321     c                   end
011700020321     c                   end
011800020319     c* riemetto il formato per toglire gli errori
011900020319     C                   IF        §40flg = ' '
012000020318     C                   WRITE     CA81D1
012100020319     C                   ENDIF
012200020319     C* CARICO SUBFILE
012300020308     C                   EXSR      CARSF2
012400030128     C* testatina video
012500030128     c                   if        kfrtip = 'A'
012600030128     c                   movel     Aut           PTP10
012700030514     c                   end
012800030514     c                   if        kfrtip = 'C'
012900030128     c                   movel     Coop          PTP10
013000030128     c                   end
013100020318     c*
013200020318     C                   WRITE     TOP
013300921222     C     *INKF         DOWEQ     '0'
013400921222     C     *INKL         ANDEQ     '0'
013500030128     c                   move      ind55         *in55
013600030514     c                   move      ind56         *in56
013700930108     C                   WRITE     CA81D3
013800921222     C                   EXFMT     CA81CT2
013900020319     c*
014000020318     C                   IF        *INKL
014100020320     c                   rolbk(e)
014200020318     C                   IF        §40flg <> ' '
014300011221     C                   EXSR      Uscita
014400020318     c                   else
014500020318     c                   leave
014600020318     c                   end
014700011221     C                   ENDIF
014800020308     c* controlli
014900020308     C                   EXSR      CTRSF2
015000020424     c                   if        *in99
015100020424     c                   move      *off          *inkf
015200020424     c                   iter
015300020424     c                   end
015400020308     c* aggiorno
015500020308     C   kf              EXSR      AGGIOR
015600020320     c                   if        *inkf
015700020320     c                   if        §40flg <> ' '
015800020320     C                   move      '1'           *inkc
015900020320     c                   else
016000020320     C                   move      '1'           *inkl
016100020320     c                   end
016200020320     c                   end
016300020318     C                   ENDDO
016400020320     c* rilascio il padroncino
016500020320     c                   exsr      srwc01
016600921222     C*
016700020318     C                   ENDDO
016800020320     c* chiudo in base al tipo elaborazione
016900020320     C                   EXSR      uscita
017000921222     C**************************************************************************
017100921222     C* CONTROLLI VIDEO1
017200020308     C**************************************************************************
017300921223     C     CTRD1         BEGSR                                                  *
017400020321     C                   SETOFF                                       9998
017500020308     C* Filiale Padroncino
017600030514    1C     pov           IFEQ      0
017700020308     C                   SETON                                        0199
017800020308     C                   ELSE
017900030514     C* x Cooperativa e x inserimento costi figurativi                 O.
018000030514     C* si possono modificare i dati del proprio P.O.                  O.
018100030514    2c                   if        kfrtip = 'C' or
018200030514     c                             kfrtip = 'G'
018300030514   X2c                   else
018400030514     C* x autotrasp. se REM su £6, se SIMFEL su £1
018500030514    3C                   IF        SimTpP = '2' OR SimTpP = *BLANK
018600020308     C     pov           LOOKUP    L6                                     20
018700020308     C  n20              SETON                                        0199
018800030514    3C                   END
018900030514    2C                   END
019000030129      *
019100030514    1c                   EndIf
019200030514     C* PADRONCINO obbligatorio solo se no costi figurativi
019300030514     c                   clear                   frepdr
019400030516    1c                   if        kfrtip = 'G'
019500030516     C                   MOVEL     POV           FREPDR
019600030516     c                   else
019700030514    2c                   if        not *in01
019800020308     C     '?'           SCAN      PDRV                                   90
019900030514    3C     *IN90         IFEQ      *ON
020000021203     C                   Z-ADD     pov           d24FIL
020100021203     C                   Z-ADD     0             d24pdr
020200021203     C                   MOVEL     'R'           d24FLG
020300030128     C                   MOVEL     Kfrtip        d24tip
020400021203     C                   MOVEL     *BLANKS       d24rsc
020500021203     C                   MOVEL(p)  fnlv24ds      KPJBU
020600020308     C                   CALL      'FNLV24R'
020700020308     C                   PARM                    KPJBA
020800021203     C                   MOVEL     KPJBU         fnlv24ds
020900030214     c                   move      d24pdr        frepdr
021000030214     c                   move      d24pdr        pdrv
021100021203     c                   movel     d24rsc        pdrdv
021200050308   X3c                   end
021300020308     c                   clear                   pdrdv
021400030514    4C     pdrv          IFne      *blanks
021500020318     C                   MOVEL     POV           FREPDR
021600020318     C                   MOVE      PDRV          FREPDR
021700021203     C     kapdf         CHAIN     fiapd01L                           02
021800030514    5C     *IN02         IFEQ      *ON
021900020308     C     *IN02         OREQ      *OFF
022000030514     C     APDATB        ANDNE     ' '
022100020308     C                   SETON                                        0299
022200030514   X5c                   else
022300100223     c                   movel(p)  apdrsf        pdrdv
022400030514   E5C                   ENDIF
022500030514   X4c                   else
022600020308     C                   SETON                                        0299
022700030514   E4c                   end
022800030514    2C                   ENDIF
022900030514    1C                   ENDIF
023000020308     C* Data distinta
023100020308    1C     ddcv          IFGT      0
023200020318     C                   Z-ADD     ddcv          G02DAT
023300020318     C                   MOVEL     *BLANK        G02ERR
023400020318     C                   CALL      'XSRDA8'
023500020318     C                   PARM                    WLBDAT
023600020318    2C     G02ERR        IFEQ      '1'
023700020318     C                   SETON                                        0399
023800030514   X2C                   Else
023900020318     C                   move      G02dat        ddcv
024000020318     C                   move      G02INV        datiso
024100020319     c                   move      datiso        freddc
024200030514    2C                   ENDIF
024300030514   X1c                   else
024400020318     C                   SETON                                        0399
024500030514   E1C                   ENDIF
024600030225     c* reperisce data limite inserimento rettifiche
024700030515     c                   if        kfrtip = 'C' or kfrtip = 'G'
024800030515     c                   clear                   trul52ds
024900030515     C                   MOVE      *date         datiso
025000030515     C                   MOVE      datiso        d52din
025100030515     c                   eval      d52sgn = '-'
025200030515     c                   select
025300030515     c                   when      kfrtip = 'C'
025400030225     c                   eval      d52gio = §cgggm
025500030515     c                   when      kfrtip = 'G'
025600030515     c                   eval      d52gio = §cgadr
025700030515     c                   endsl
025800030225     c                   call      'TRUL52R'
025900030225     c                   parm                    trul52ds
026000030225     c                   if        d52err <> ' '
026100030225     c                   seton                                        99
026200030225     c                   end
026300030515     c                   end
026400030214      *
026500030514     C* Tipo servizio
026600030522     c* se coop
026700030522     c                   if        kfrtip = 'C'
026800030522     c* i tipi servizi validi sono X P T
026900030522     c                   if        (ptsr<>'X' and ptsr<>'P' and ptsr<>'T')
027000030514     C                   SETON                                        0599
027100030522     c                   else
027200030522     C* controllo se esiste la tariffa x evitare che mi inseriscano
027300030522     c* delle rettifiche su codici che non sono abilitati al tipo servizio
027400030522     c                   clear                   param
027500030522     c                   move      freddc        datfgt
027600030522     c                   movel     pov           codfgt
027700030522     c                   move      pdrv          codfgt
027800030522     c                   eval      tipopre = ptsr
027900030526     c                   call      'FICN36R5'
028000030522     c                   parm                    param
028100030522     c                   if        errfgt <> *blanks
028200030522     c                   seton                                        0699
028300030522     c                   end
028400030514     C                   End
028500030522     C                   End
028600030514     c* se costi fig. i tipi servizi validi sono A B D
028700030514     c                   if        kfrtip = 'G' and
028800030514     c                             (ptsr<>'A' and ptsr<>'B' and ptsr<>'D')
028900030514     C                   SETON                                        0599
029000030514     C                   End
029100030128      * se Autotrasportatori
029200030514    1c                   if        not *in99
029300030514    2c                   if        kfrTIP = 'A'
029400020320     c* controllo che esista il conteggio
029500020320     c                   movel     ' '           fttfvl
029600020320     c     kftt          chain     fiftt02l
029700030514    3c                   if        not %found(fiftt02l)
029800020320     c                   movel     'V'           fttfvl
029900020320     c     kftt          chain     fiftt02l
030000030514    4c                   if        not %found(fiftt02l)
030100020320     c* controllo se già confermato proteggo
030200020320     c                   movel     'C'           fttfvl
030300020320     c     kftt          chain     fiftt02l
030400030514    5c                   if        %found(fiftt02l)
030500020320     c                   seton                                        98
030600030514   X5c                   else
030700020320     c                   seton                                        0499
030800030514    5c                   end
030900030514    4c                   end
031000030514    3c                   end
031100030128      *
031200130125   X2c                   elseIF      kfrTIP = 'C'
031300030522     c* calcolo il fine mese
031400030522     c                   move      freddc        data
031500030522     c                   move      01            data
031600030522     c                   move      data          datiso
031700030522     c                   adddur    1:*m          datiso
031800030522     c                   subdur    1:*d          datiso
031900030522     c                   move      datiso        data
032000030522      * se cooperative o costi figurativi ( per i costi fig. non ci
032100030522     c* sarà mai il fictt)
032200030522     c                   if        ptsr = 'X'
032300030217     c     kctt          chain(n)  fictt03l
032400030522     c                   else
032500030522     c     kctt1         chain(n)  fictt03l
032600030522     c                   end
032700030514    3c                   if        %found(fictt03l)
032800030217      *  Se confermate proteggo
032900030514    4c                   if        cttfvl = 'C'
033000030217     c                   seton                                        98
033100030514   X4c                   else
033200030219      *  Se valorizzate e inferiori di enne gg. non è possibile
033300030219      *  inserire rettifiche
033400030514    5c                   if        cttddc < d52dfi
033500030219     c                   seton                                        98
033600030514    5c                   end
033700030514    4c                   end
033800030514   X3c                   else
033900030214     c* errore di non trovata valorizzazione solo se il tipo servizio = X.
034000030514     c* per P e T e x costi figurativi non importa che ci sia il record
034100030514     c* di testata
034200030514    4c                   if        ptsr = 'X'
034300030128     c                   seton                                        0499
034400030514   X4c                   else
034500030219      *  Se valorizzate e inferiori di enne gg. non è possibile
034600030219      *  inserire rettifiche
034700030514    5c                   if        freddc < d52dfi
034800030219     c                   seton                                        98
034900030514    5c                   end
035000030514    4c                   end
035100030514    3c                   end
035200030514    2c                   end
035300030514    1c                   End
035400030128      *
035500020319     c* Controllo se abilitato alla manutenzione delle rettifiche
035600131007     C                   clear                   tibs42ds
035700131007     c                   movel     pov           i42pge
035800131007     c                   call      'TIBS42R'
035900131007     c                   parm                    tibs42ds
036000131007     c                   movel     o42uni        uteautds
036100030514     c                   if        kfrtip = 'A'
036200020319     c* 98 indicatore che protegge tutto
036300161104     c     §autret       comp      'S'                                9696
036400161104     c   96              seton                                        1299
036500030515     c* controllo se già stampato per autotrasp.
036600030515     c     kfre          chain     fifre02l
036700030515     c                   if        %found(fifre02l) and fredds <> 0
036800030515     c                   seton                                        98
036900030515     c                   end
037000030514     c                   else
037100161104     c     §autretc      comp      'S'                                9696
037200161104     c   96              seton                                        1299
037300030204     c                   end
037400030204      *
037500020320     c* se sono abilitato alla modifica controllo che non ci sia già
037600020320     c* qualcuno che sta confermando i conteggi/rettifiche da me scelte
037700020321     c                   if        not *in98 and not *in99
037800020320     C                   MOVE      *blanks       WLRSML
037900020320     C                   MOVE      frePDR        WLRPDR
038000020320     C                   MOVE      freDDC        WLRDDC
038100020320     C                   MOVE      *date         datiso
038200020320     C                   MOVE      datiso        WLRIMM
038300020320     C                   MOVE      datiso        dateu8
038400020320     C                   MOVEL     KNMEB         WLRNMB
038500020320     C                   MOVEL     KNMUS         WLRNUS
038600020320     C                   Z-ADD     KNRJO         WLRNJO
038700020320     C                   WRITE     FNWLRC00                             30
038800020320     C  N30              MOVE      'N'           WEXIST            1
038900020320     C     *IN30         IFEQ      *ON
039000020320     C     KWC0          CHAIN     FNWLRC0F                           30
039100020320     C* SE STESSO UTENTE/TERMINALE DO X SCONTATO CHE SI TRATTI DI UN
039200020320     C* LAVORO TERMINATO IN MODO ANOMALO
039300020320     C     *IN30         IFEQ      *OFF
039400020320     C     WLRIMM        ANDLT     DATEU8
039500020320     C                   Z-ADD     DATEU8        WLRIMM
039600020320     C                   MOVEL     KNMEB         WLRNMB
039700020320     C                   MOVEL     KNMUS         WLRNUS
039800020320     C                   Z-ADD     KNRJO         WLRNJO
039900020320     C                   UPDATE    FNWLRC00
040000020320     C                   MOVE      'N'           WEXIST
040100020320     C                   ELSE
040200020320     c* errore
040300020701     C                   IF        SimUte <> WLRNUS
040400020701     C                             OR
040500020701     C                             KNmTD <> WLRNMB
040600020701     C                             OR
040700020701     C                             %SUBST(SimUte:1:3) = 'EDP'
040800020320     C                   MOVE      'E'           WEXIST
040900020320     C                   UNLOCK    FNWLRC0F
041000020320     C                   ELSE
041100020320     C                   Z-ADD     DATEU8        WLRIMM
041200020320     C                   MOVEL     KNMEB         WLRNMB
041300020320     C                   MOVEL     KNMUS         WLRNUS
041400020320     C                   Z-ADD     KNRJO         WLRNJO
041500020320     C                   UPDATE    FNWLRC00
041600020320     C                   MOVE      'N'           WEXIST
041700020320     C                   END
041800020320     C                   END
041900020320     C                   END
042000020320     C     WEXIST        IFEQ      'E'
042100020320     C                   seton                                        98
042200020320     C                   MOVEL     msg(4)        msgv
042300020320     C                   END
042400020321     C                   END
042500011221     C*
042600921223     C                   ENDSR
042700020308     C**************************************************************************
042800020308     C* CARICAMENTO SUBFILE rettifiche
042900020308     C**************************************************************************
043000921223     C     CARSF2        BEGSR
043100020104     C*
043200930108     C                   SETON                                        70
043300930108     C                   Z-ADD     0             SF2NRR
043400930108     C                   WRITE     CA81CT2
043500020321     C                   SETOFF                                       70klkf
043600030506      * se nella routine precedente c'era un problema di allocazione
043700030515      * aveva già impostato il msg e l'indicatore.
043800030506     c  n98              movel     *blanks       msgv
043900030515     C* CONTROLLO SE SONO GIA' STATE MEMORIZZATE rettifiche
044000030128     C     Kfre          SETLL     fifre02L                               86
044100020107     C                   DO        *hival
044200030128     C     kfre          READE     fifre02L                               86
044300020107     C   86              leave
044400020319     c* 97 proteggo il campo della causale
044500020319     c                   seton                                        97
044600020424     c                   setoff                                       10
044700030515     c* controllo il tipo servizio (x autotras. ptsr è = ' ')
044800030515     c                   if        ptsr <> ' '
044900030515     c                   if        fretsr <> ptsr
045000020318     c                   iter
045100020318     c                   end
045200030515     c                   else
045300030515     c                   if        fretsr <> 'C' and fretsr <> 'R'
045400030515     c                   iter
045500030515     c                   end
045600030515     c                   end
045700030515     c* controllo il livello di riservatezza
045800030515     c                   if        frepno > 1
045900030515     c                   iter
046000030515     c                   end
046100020319     c*
046200020319     c                   move      frepno        sf2pno
046300020318     c                   if        frenot <> *blanks
046400020308     c                   movel     'N'           sf2not
046500020318     c                   else
046600020318     c                   movel     ' '           sf2not
046700020107     c                   end
046800020308     c                   movel     ' '           sf2sce
046900020308     c                   movel     frecre        sf2cre
047000020308     c                   exsr      srcre
047100061011      * se causale pj la salta
047200061011     c                   if        §crtsr = 'F'
047300061011     c                   iter
047400061011     c                   endif
047500061011      *
047600160406     c                   if        §crfise = 'S' and o36pou <> 046
047700160406     c                   iter
047800160406     c                   endif
047900160406      *
048000020516     c                   if        dcre  <> *blanks
048100020424     C                   movel(p)  §crdes        sf2des
048200030515     c                   movel     §crtsr        sf2tsr
048300020318     c                   else
048400020424     c                   seton                                        1099
048500020318     C                   MOVEL     MSG(01)       msgv
048600020318     C                   END
048700020308     c                   z-add     fretim        sf2tim
048800030128     c                   z-add     fretim        sf2Sim
048900930108     C                   ADD       1             SF2NRR
049000930108     C                   WRITE     CA81SF2
049100020107     C                   enddo
049200030129      *
049300020319     c* riempo il subfile con 100 righe vuote solo se ammesso
049400020319     c                   if        not *in98
049500020319     c* 97 sproteggo il campo della causale
049600020424     c                   setoff                                       971099
049700020308     c                   do        100
049800020308     c                   clear                   sf2sce
049900020318     c                   clear                   sf2des
050000030515     c                   clear                   sf2tsr
050100020308     c                   clear                   sf2not
050200020308     c                   clear                   sf2cre
050300020308     c                   clear                   descau
050400020308     c                   clear                   sf2tim
050500030128     c                   clear                   sf2Sim
050600020319     c                   clear                   sf2pno
050700020308     C                   ADD       1             SF2NRR
050800020308     C                   WRITE     CA81SF2
050900020318     c                   enddo
051000020320     c                   end
051100020319     C* abilito sfldsp
051200020319     c     sf2nrr        comp      0                                  7171
051300930112     C                   Z-ADD     SF2NRR        TOTSF2            4 0
051400020308     C                   Z-ADD     1             HKEY2
051500020319     C                   ENDSR
051600020308     C**************************************************************************
051700020308     C* CONTROLLI SUBFILE causali rettifiche
051800020308     C**************************************************************************
051900921223     C     CTRSF2        BEGSR
052000020319     c                   setoff                                       99
052100020308     c                   clear                   cre
052200020319     c                   clear                   hkey2
052300020318     c                   movel     *blanks       msgv
052400020319     c*
052500930112     C                   DO        TOTSF2        X                 4 0
052600930114     C     X             CHAIN     CA81SF2                            86
052700030310     c                   setoff                                       101112
052800020424     c* se si vuole cancellare non controllo
052900020424     c                   if        sf2sce = '4'
053000020424     c                   iter
053100020424     c                   end
053200020308     C* CONTROLLO Causale
053300020318     c                   clear                   sf2des
053400030515     c                   clear                   sf2tsr
053500020319     C     '?'           SCAN      sf2cre                                 90
053600020308     c* ricerca causale
053700020319     C     *IN90         IFEQ      *ON
053800030515     C                   clear                   Tntb40DS
053900020308     C                   eval      t02tla = 'L'
054000030514     C                   movel     ptsr          t02TSR
054100020308     C                   movel     'R'           t02mod
054200020308     C                   movel     'CRE'         t02cod
054300020308     C                   movel     knsif         t02sif
054400030128     C                   CALL      'TNTB40R'
054500020308     C                   parm                    KPJBA
054600030128     C                   parm                    TNTB40DS
054700020308    2C                   IF        T02err = *blanks
054800020318     C                   movel     T02ke1        sf2cre
054900020308     C                   else
055000020318     C                   seton                                        10
055100020308     C                   MOVEL     MSG(01)       msgv
055200020308     C                   ENDIF
055300030515    1c                   end
055400020308     c* controllo causale
0555000305152    c                   if        sf2cre <> *blanks
055600020308     c                   exsr      srcre
0557000302143    c                   if        dcre  <> *blanks
055800020424     C                   movel(p)  §crdes        sf2des
055900030515     C                   movel(p)  §crtsr        sf2tsr
056000160303     c* controllo causale
0561001603035    C                   if        §crfise = 'S' and o36pou  <> 046
056200160303     c                   seton                                        10
056300160303     C                   MOVEL     MSG(10)       msgv
056400160303x5   c                   endif
0565001604065    C*m                 if        §crfise = ' ' and o36pou  = 046
056600160406     c*m                 seton                                        10
056700160406     C*m                 MOVEL     MSG(11)       msgv
056800160406x5   c*m                 endif
056900030514      * x autotrasportatore
0570000305144    c                   if        kfrtip = 'A'
0571000305145    C                   if        §crTSR <> 'C' and §crTSR <> 'R'
057200030128     c                   seton                                        10
057300030128     C                   MOVEL     MSG(07)       msgv
057400030514x5   c                   else
057500020424     c*controllo il tipo di causale se di ritiro/consegna deve esistere
057600020424     c* il record su fiftt
057700020424     c     kftt1         chain     fiftt02l
0578000302146    c                   if        not %found(fiftt02l)
057900020424     c                   seton                                        10
058000020424     C                   MOVEL     MSG(05)       msgv
058100030326e6   c                   end
058200030214e5   c                   endIF
058300030514      * x Cooperative e costi figurativi
058400030515x4   c                   else
0585000305145    C                   if        §crTSR <> ptsr
058600030514     c                   seton                                        10
058700030514     C                   MOVEL     MSG(07)       msgv
058800030514e5   c                   end
058900030514e4   c                   End
059000030214     c* non trovata causale
059100030214x3   c                   else
059200020318     c                   seton                                        10
059300020318     C                   MOVEL     MSG(01)       msgv
059400030214e3   C                   END
059500030214e2   C                   END
059600020308     c* controllo che non ci siamo causali ridondanti
059700020319     c                   if        not *in10  and sf2cre <> *blanks
059800020318     c                   z-add     0             conta
059900020318     c                   do        50            yy                3 0
060000020318     c     sf2cre        ifeq      cre(yy)
060100020318     c                   add       1             conta             3 0
060200020318     c                   end
060300020318     c                   enddo
060400020318     c                   if        conta > 0
060500020318     c                   seton                                        10
060600020318     C                   MOVEL     MSG(02)       msgv
060700020308     c                   else
060800020318     c                   z-add     1             yy
060900020318     c     '   '         lookup    cre(yy)                                25
061000020318     c   25              movel     sf2cre        cre(yy)
061100020308     c                   end
061200020318     c                   end
061300030515     C* controllo importi
061400020318     C     SF2tim        IFne      0
061500020318     C     SF2cre        andeq     *blanks
061600020318     C                   SETON                                        10
061700020318     C                   MOVEL     MSG(01)       msgv
061800020318     c                   end
061900020318     C     SF2tim        IFeq      0
062000020318     C     SF2cre        andne     *blanks
062100020318     C     *in10         andeq     '0'
062200020318     C                   SETON                                        11
062300020318     C                   MOVEL     MSG(03)       msgv
062400020318     c                   else
062500020318     c                   setoff                                       11
062600020318     c                   end
062700020614     c*
062800030609     C     SF2tim        IFgt      9999,99
062900020614     C     *in11         andeq     '0'
063000030701     C     SF2tim        orlt      -9999,99
063100030701     C     *in11         andeq     '0'
063200020614     C                   SETON                                        11
063300020614     C                   MOVEL     MSG(03)       msgv
063400020614     c                   end
063500020308     c* sfl note
063600020319     c                   if        *in10 or *in11
063700020319     c                   seton                                        99
063800020319     c                   else
063900030310     c                   if        sf2sce = 'N' and sf2cre <> *blanks or
064000030514     c                             kfrtip = 'C' and sf2cre <> *blanks and
064100030310     c                             sf2not = *blank
064200030129     c                   exsr      SRNote
064300030310      * se esce senza aver immesso delle Note segnala come errore
064400030514     C                   if        sf2NOT = *Blank and kfrtip = 'C'
064500030310     C                   SETON                                        12  99
064600030310     C                   MOVEL     MSG(08)       msgv
064700030310     c                   end
064800030310      *
064900020318     c                   movel     *blanks       sf2sce
065000020318     c                   end
065100020319     c                   end
065200020319     c* 97 proteggo il campo della causale
065300020319     c     sf2pno        comp      1                                      97
065400020424     c* memorizzo record per posizionamento errore
065500020319     c                   if        hkey2 = 0 and (*in10 or *in11)
065600020319     C                   Z-ADD     X             HKEY2
065700020424     c                   end
065800020424     c*
065900020308     C                   UPDATE    CA81SF2
066000020308     c                   enddo
066100020319     c* rimposto gli indicatori
066200020319     c                   if        hkey2 = 0
066300020319     c                   z-add     1             hkey2
066400020319     c                   else
066500020319     c                   seton                                        99
066600020319     c                   end
066700020321     c                   setoff                                       kl
066800930111     C*
066900921223     C                   ENDSR
067000020308     C**************************************************************************
067100020308     C* CARICAMENTO SUBFILE NOTE
067200020308     C**************************************************************************
067300020308     C     srnote        BEGSR
067400020308     C* CARICO SUBFILE
067500020308     C                   EXSR      CARSF1
067600020318     C                   DO        *hival
067700030128     C* testatina video
067800030128     c                   if        kfrtip = 'A'
067900030128     c                   movel     Aut           PTP10
068000030514     c                   end
068100030514     c                   if        kfrtip = 'C'
068200030128     c                   movel     Coop          PTP10
068300030128     c                   end
068400030128     c                   move      ind55         *in55
068500030514     c                   move      ind56         *in56
068600020308     C                   WRITE     CA81D4
068700020308     C                   EXFMT     CA81CT1
068800020318     c* ritorno
068900020318     c   kl              leave
069000020308     c* aggiorna
069100020320     c                   exsr      aggio1
069200020308     c                   enddo
069300020321     c                   setoff                                       kl
069400020308     C                   ENDSR
069500020308     C**************************************************************************
069600020308     C* CARICAMENTO SUBFILE NOTE
069700020308     C**************************************************************************
069800020308     C     carsf1        BEGSR
069900020308     C*
070000020318     C                   SETON                                        73
070100020318     C                   Z-ADD     0             SF1NRR
070200020308     C                   WRITE     CA81CT1
070300020321     C                   SETOFF                                       73
070400020417     c* decodifico causale
070500020308     c                   exsr      srcre
070600020308     c                   movel     tbeuni        descau
070700020308     C* CONTROLLO SE SONO GIA' STATe MEMORIZZATe rettifiche
070800030128     C     Kfre2         SETLL     fifre02L                               86
070900020308     C                   DO        *hival
071000030128     C     kfre2         READE     fifre02L                               86
071100020308     C   86              leave
071200020308     c* controllo il livello di riservatezza
071300020308     c                   movel     frenot        sf1not
071400020319     c                   move      frepno        sf1pno
071500020308     C                   ADD       1             SF1NRR
071600020308     C                   WRITE     CA81SF1
071700020308     C                   enddo
071800020319     c* riempo il subfile con 100 righe vuote solo se ammesso
071900020319     c                   if        not *in98
072000020308     c                   do        100
072100020308     c                   clear                   sf1not
072200020319     c                   clear                   sf1pno
072300020308     C                   ADD       1             SF1NRR
072400020308     C                   WRITE     CA81SF1
072500020319     c                   enddo
072600020319     c                   end
072700020308     C*
072800020319     c     sf1nrr        comp      0                                  7474
072900020308     C                   Z-ADD     SF1NRR        TOTSF1            4 0
073000020308     C                   Z-ADD     1             HKEY1
073100020308     C                   ENDSR
073200020308     C**************************************************************************
073300020308     C* tabella CRE
073400020308     C**************************************************************************
073500020308     C     srcre         BEGSR
073600020318     C                   movel(P)  sf2cre        tbeke1
073700030128     c                   movel     'CRE'         tbecod
073800020308     C     Ktbe          chain     tntbe01l                           10
073900020308     C     *in10         ifeq      *on
074000020318     C     tbeatb        orne      *blanks
074100020516     c                   clear                   dcre
074200020424     c                   else
074300020516     c                   movel     tbeuni        dcre
074400121001      *** se NON utilizzabile ....
074500121001      ***   è come se fosse stata ANNULLATA -> sblenka la DS
074600121001     c                   if        §CRuti = 'N'
074700121001     c                   clear                   dcre
074800121001     c                   end
074900020308     c                   end
075000020308     C                   ENDSR
075100020308     C**************************************************************************
075200020308     C* AGGIORNAMENTO rettifiche
075300020308     C**************************************************************************
075400020308     C     AGGIOR        BEGSR
075500020308     C* scrivo record
075600020308     C                   DO        TOTSF2        X
075700020308     C     X             CHAIN     CA81SF2                            86
075800020319     C                   if        *in86
075900020319     c                   leave
076000020319     c                   end
076100020319     c*
076200030128     c     kfre2         chain     fifre02l
076300020318     C                   MOVEL     sf2cre        frecre
076400020308     C                   Z-ADD     sf2tim        fretim
076500030515     C                   movel     sf2tsr        fretsr
076600020319     c*
076700020319     c                   select
076800020319     c                   when      sf2sce = ' ' and sf2cre <> *blanks
076900030128     c                   if        not %found(fifre02l)
077000020318     C                   movel     *blanks       frenot
077100030128     C                   movel     Kfrtip        fretip
077200020319     c                   z-add     1             frepno
077300020319     c                   z-add     0             fredds
077400030129     c     sf2Tim        sub       sf2Sim        TotDiffe         10 3
077500020318     C                   WRITE     fifre000
077600020318     c                   else
077700030129     c     sf2Tim        sub       sf2Sim        TotDiffe
077800020318     C                   update    fifre000
077900020318     C                   END
078000020319     c* cancella
078100020319     c                   when      sf2sce = '4'
078200030128     c     kfre2         setll     fifre02l
078300020319     c                   do        *hival
078400030128     c     kfre2         reade     fifre02l
078500030128     c                   if        %eof(fifre02l)
078600020319     c                   leave
078700020319     c                   end
078800030515     c                   if        fretsr = sf2tsr
078900030707     c                   Z-SUB     sf2Sim        TotDiffe         10 3
079000020319     C                   delete    fifre000
079100030128     c                   end
079200030214     C                   ENDdo
079300020319     C                   ENDsl
079400030214      * Solo per Cooperative deve scrivere/aggiornare la testata
079500030129      *   relativa alla rettifica modificando il valore rettifica
079600030129      *     come tot.Partenze/Arrivi in base al cod."CRE" ed alla "8D".
079700030129     c                   if        kfrtip = 'C' and sf2CRE<>*blank
079800030129     C                   Exsr      PAR_ARR_CRE
079900030129     C                   Exsr      AGG_CTT_Coop
080000030129     c                   end
080100020308     C                   ENDdo
080200030129      *
080300020320     c                   commit
080400020308     C*
080500020308     C                   ENDSR
080600020308     C**************************************************************************
080700020308     C* AGGIORNAMENTO note
080800020308     C**************************************************************************
080900020308     C     AGGIO1        BEGSR
081000020319     c                   clear                   sf2not
081100020319     C                   MOVEL     sf2cre        frecre
081200030515     C                   MOVEL     sf2tsr        fretsr
081300020308     C* scrivo record
081400020318     c                   z-add     0             frepno
081500020318     C                   Z-ADD     sf2tim        fretim
081600020308     C                   DO        TOTSF1        X
081700020308     C     X             CHAIN     CA81SF1                            86
081800020319     C                   if        *in86
081900020319     c                   leave
082000020319     c                   end
082100020319     C                   if        sf1not = *blanks and sf1pno = 0
082200020319     c                   iter
082300020319     c                   end
082400020319     c*
082500030128     c     kfre3         chain     fifre02l
082600020319     C                   if        SF1not <> *blanks
082700020319     c                   movel     'N'           sf2not
082800020319     c                   move      1             sf2pno
082900020308     C                   movel     SF1not        frenot
083000030515     c* aggiorno
083100030128     c                   if        %found(fifre02l)
083200020319     C                   update    fifre000
083300020319     c                   else
083400020319     c* scrivo
083500020319     c                   add       1             frepno
083600020408     c                   if        frepno > 1
083700020408     C                   Z-ADD     0             fretim
083800020408     c                   end
083900020319     c                   z-add     0             fredds
084000030128     c                   movel     kfrtip        fretip
084100020308     C                   WRITE     fifre000
084200020408     C                   Z-ADD     frepno        sf1pno
084300020308     C                   END
084400020319     c                   else
084500030128     c                   if        %found(fifre02l)
084600020319     c* non cancello il primo record
084700020319     c                   if        sf1pno = 1
084800020319     C                   movel     *blanks       frenot
084900020319     C                   update    fifre000
085000020319     c                   else
085100020319     c* cancella
085200020319     c                   delete    fifre000
085300020319     c                   end
085400020408     c                   end
085500020308     C                   END
085600020408     c                   update    ca81sf1
085700020308     C                   ENDdo
085800020308     C*
085900020308     C                   ENDSR
086000020320      **************************************************************************
086100030129     C* Partenze/Arrivi in base alla Competenza (8D) del C.REttifica
086200020320      **************************************************************************
086300030129     C     PAR_ARR_Cre   BEGSR
086400030129     C*
086500030129     C*  Reperisce la Competenza su tabella "CRE"
086600030129     c                   exsr      srCRE
086700030129      *
086800030129     C*  Quindi su Tabel:"8D" decodifica se è una Partenza o un Arrivo
086900030129     C                   MOVEL     '8D'          COD_Tab
087000030129     C                   MOVEL     *BLANKS       KEY_Tab
087100030129     C                   MOVEL     §crVOC        KEY_Tab
087200030707     c                   clear                   par_arr
087300030129     C     KTAB          CHAIN     Tabel00F
087400030129     c                   if        %Found(Tabel00f)
087500030129     C                   MOVEL     tblUNI        Ds8D
087600030129     c                   move      §8dPA         Par_Arr           1
087700030129     c                   EndIf
087800030129     C*
087900030129     C                   ENDSR
088000030129      **************************************************************************
088100030129     C* Aggiorna Testata Conteggi Cooperative sui campi TOT.Rett.Arrivi/Partenze
088200030129      **************************************************************************
088300030129     C     AGG_CTT_Coop  BEGSR
088400030129     C*
088500030217     c     kctt          chain     fictt03l
088600030217     c                   if        %found(fictt03l)
088700030707     C*    aggiorna rettif.arrivo
088800030707     c                   if        Par_Arr = 'A'
088900030707     c                   add       TotDiffe      cttITMP
089000030707     c                   else
089100030707     C*    aggiorna rettif.partenza/prestazioni P o T
089200030129     c                   add       TotDiffe      cttITMA
089300030704     c                   End
089400030129     c                   update    FICTT000
089500030214     C* scrive il record su fictt00f come valorizzato solo per servizi T o P
089600030214     c                   else
089700030214     c                   clear                   fictt000
089800030214     C                   eval      cttfgs = pov
089900030214     C                   eval      cttpdr = frepdr
090000030214     C                   eval      cttddc = freddc
090100030214     C                   eval      ctttsr = ptsr
090200030214     C                   eval      cttitma = totdiffe
090300030214     c                   eval      cttdiv = 'EUR'
090400120827     c*                  eval      cttsoc = apdcsf
090500120827     c*                  movel     *all'0'       cttcdf
090600120827     c*                  move      apdksc        cttcdf
090700030214     c                   write     fictt000
090800030129     c                   EndIF
090900030129     C*
091000030129     C                   ENDSR
091100030129      **************************************************************************
091200030129     C* DELET DA FILE DI LAVORO I RECORDS del padroncino e data
091300030129      **************************************************************************
091400030129     C     SRWC01        BEGSR
091500020320     C     KWC1          SETLL     FNWLRC1L
091600020320     C     KWC1          READE     FNWLRC1L                               30
091700020320     C     *IN30         DOWEQ     *OFF
091800020320     c                   if        wlrpdr = frepdr and wlrddc = freddc
091900020320     C                   DELETE    FNWC01
092000020320     c                   end
092100020320     C     KWC1          READE     FNWLRC1L                               30
092200020320     C                   ENDDO
092300020320     C                   ENDSR
092400020308     C**************************************************************************
092500921222     C* INIZIALIZZAZIONE
092600020308     C**************************************************************************
092700921223     C     DEFCAM        BEGSR                                                  *
092800030128      *
092900030128     C                   SETOFF                                       98kc55
093000030514     C                   SETOFF                                       56
093100030128      *
093200921223     C     *ENTRY        PLIST                                                  *
093300921223     C                   PARM                    KPJBA                          *
093400020308     c                   movel     kpjbu         ficn40ds
093500030428      *
093600030529     c                   movel     §40tip        kfrtip
093700030514      * se richiamato da menù per i padroncini
093800030514      * riceve un blank altrimenti viene impostata la "C" per le cooperative
093900030514     c                   if        kfrtip = *blank
094000030514     c                   movel     'A'           kfrtip
094100030514     c                   end
094200030128      * se cooperative 55 = *on e se lo memorizza in un flag (IND55)
094300030514     c                   if        kfrtip <>'A'
094400030128     c                   seton                                        55
094500130125      * solo x le coop
094600130125     c                   if        kfrtip = 'C'
094700030210     c                   open      fictt03l
094800130125     c                   end
094900030515      *  Reperisce il nr. dei giorni oltre i quali non è possibile
095000030515      *  inserire rettifiche su conteggi Cooperative confermati.
095100030515     c
095200030515     C                   movel(P)  '1'           tbeke1
095300030515     c                   movel     'CCG'         tbecod
095400030515     C     Ktbe          chain     tntbe01l                           10
095500030515     C     *in10         ifeq      *on
095600030515     C     tbeatb        orne      *blanks
095700030515     c                   clear                   dccg
095800030515     c                   else
095900030515     c                   movel     tbeuni        dccg
096000030515     c                   end
096100030128     c                   endIF
096200030515     c* 55 e 56 ind. di comodo per decodifiche a video
096300030529     c                   movel(p)  titvar        titolov
096400030514     c                   if        kfrtip ='G'
096500030529     c                   movel(p)  titsti        titolov
096600030514     c                   seton                                        56
096700030514     c                   endIF
096800030514     c                   move      *in55         ind55             1
096900030514     c                   move      *in56         ind56             1
097000030429      *----------*
097100030429      * Solo per gli Autotrasportatori:
097200030429      *  Alloca scrivendo un record nel Fiwalc1L per impedire che ci siano dei
097300030429      *   conflitti con le trasmissioni.
097400030514     c                   if        §40flg = *blank and kfrtip = 'A'
097500030430     c                   open      fiwalc1l
097600030429     C* GIRO DATA ODIERNA
097700030429     C                   TIME                    W0140            14 0
097800030429     C                   MOVE      W0140         UDATE8            8 0
097900030429     C                   Z-ADD     UDATE8        G02DAT
098000030429     C                   MOVE      *ZEROS        G02INV
098100030429     C                   MOVE      *BLANKS       G02ERR
098200030429     C                   CALL      'XSRDA8'
098300030429     C                   PARM                    WLBDAT
098400030429     C                   clear                   Fiwalc00
098500030429     C                   movel     w0140         walOrc
098600030429     C                   movel     g02inv        waldac
098700030429     C                   eval      WalSIF = KNSIF
098800030429     C                   eval      WalPGM = 'FICN40R   '
098900030429     C                   eval      WalNOJ = KNMEB
099000030429     C                   eval      WalNUS = KNMUS
099100030429     C                   eval      WalNRJ = KNRJO
099200030429     C                   write     Fiwalc00
099300030429     c                   feod      Fiwalc1l
099400030429     c                   end
099500030129      *  In base all'utente prende il P.O.
099600030129     C                   CLEAR                   Tibs36Ds
099700030129     c                   EVAL      I36ute = Knmus
099800030129     c                   EVAL      I36Tla = 'L'
099900030129     C                   CALL      'TIBS36R'
100000030129     C                   PARM                    Tibs36Ds
100100030128      *
100200020308     c                   clear                   §40tim
100300020320     c* chiamo per chiudere i file
100400020320     c                   select
100500020320     c                   when      §40tla = 'C'
100600020320     c                   exsr      uscita
100700020320     c* se tipo elaborazione LR oppure se tipo elaborazione RT ed è la
100800030128     c* 1° volta faccio STRCMTCTL e apro il file fifre02l
100900030128     c                   WHEN      not %open(fifre02l)
101000020320     c                   movel(p)  cmd           comando
101100020320     c                   exsr      srcmd
101200030128     c                   open      fifre02l
101300020321     c                   endsl
101400030128      *
101500020308     C* DEFINIZIONE CHIAVI DI ACCESSO
101600030428     C     Kwalc         KLIST
101700030428     C                   KFLD                    WalPGM
101800030428     C                   KFLD                    WalNOJ
101900030428     C                   KFLD                    WalNUS
102000030428     C                   KFLD                    WalNRJ
102100030428     C*
102200021203     C     Kapdf         KLIST
102300021203     C                   KFLD                    apdtip
102400021203     C                   KFLD                    frepdr
102500030128     c                   movel     kfrtip        apdtip
102600030129     C*
102700030129     C     KTAB          KLIST
102800030129     C                   KFLD                    CODUT             1 0
102900030129     C                   KFLD                    COD_tab           2
103000030129     C                   KFLD                    KEY_tab           8
103100021203     c*
103200021203     C     KTbe          KLIST
103300021203     C                   KFLD                    tbecod
103400021203     C                   KFLD                    tbeke1
103500020308     c*
103600020308     C     Kfre          KLIST
103700030128     c                   KFLD                    kfrtip            1
103800020318     C                   KFLD                    frepdr
103900020308     C                   KFLD                    freddc
104000020308     c*
104100020308     C     Kfre2         KLIST
104200030128     c                   KFLD                    kfrtip
104300020318     C                   KFLD                    frepdr
104400020308     C                   KFLD                    freddc
104500030515     C                   KFLD                    sf2tsr
104600020308     C                   KFLD                    sf2cre
104700020319     c*
104800020319     C     Kfre3         KLIST
104900030128     c                   KFLD                    kfrtip
105000020319     C                   KFLD                    frepdr
105100020319     C                   KFLD                    freddc
105200030515     C                   KFLD                    sf2tsr
105300020319     C                   KFLD                    sf2cre
105400020319     C                   KFLD                    sf1pno
105500020319     c*
105600020319     C     Kftt          KLIST
105700020319     C                   KFLD                    frepdr
105800020319     C                   KFLD                    fttfvl
105900020319     C                   KFLD                    freddc
106000020424     C     Kftt1         KLIST
106100020424     C                   KFLD                    frepdr
106200020424     C                   KFLD                    fttfvl
106300020424     C                   KFLD                    freddc
106400020424     C                   KFLD                    §crtsr
106500020320     C     KWC0          KLIST
106600020320     C                   KFLD                    wlrSML
106700020320     C                   KFLD                    frePDR
106800020320     C                   KFLD                    freDDC
106900020320     C     KWC1          KLIST
107000020320     C                   KFLD                    KNMEB                          NOME JOB
107100020320     C                   KFLD                    KNMUS                          NOME USER
107200020320     C                   KFLD                    KNRJO                          NR.JOB
107300030128     c*
107400030128     C     Kctt          KLIST
107500030214     C                   KFLD                    pov
107600030214     C                   KFLD                    frepdr
107700030128     C                   KFLD                    ptsr
107800030217     C                   KFLD                    freddc
107900030522     C     Kctt1         KLIST
108000030522     C                   KFLD                    pov
108100030522     C                   KFLD                    frepdr
108200030522     C                   KFLD                    ptsr
108300030522     C                   KFLD                    data
108400020320     c*
108500020320     C                   Z-ADD     1             CODUT
108600020320     C                   CALL      'X§PARUT'
108700020320     C                   PARM                    UT§DSE
108800020320     C                   MOVEL     RAGUT         RSUT
108900020320     C                   MOVEL     REC80         CNCR80
109000020318     C                   EXSR      PUL01
109100020308     C***
109200020308     C* CARICO TABELLA FILIALI GEST.ARRIVI £6
109300020308     C***
109400020308     C                   CLEAR                   DSUL06
109500020308     C                   MOVE      '£6'          D06COD
109600020701     C                   MOVEL     SimPOU        D06KEY
109700020308     C                   MOVEL     'L'           D06TLA
109800020308     C                   MOVEL     DSUL06        KPJBU
109900020308     C                   CALL      'TRUL06R'
110000020308     C                   PARM                    KPJBA
110100020308     C                   MOVEL     KPJBU         DSUL06
110200020308     C                   MOVEA     LIN           L6
110300020308     c* pgm richiamato
110400020422     C                   if        §40flg <> ' '
110500020422     C                   MOVEL     §40PDR        pov
110600020308     C                   MOVE      §40pdr        pdrv
110700020308     C                   move      §40ddc        datiso
110800020308     C     *dmy          move      datiso        ddcv
110900020319     C                   move      §40ddc        freddc
111000020319     C                   END
111100020308     C                   movel     pov           frepdr
111200020308     C                   move      pdrv          frepdr
111300020319     C                   if        §40flg = 'I'
111400020319     c                   move      '1'           *inkc
111500020319     c                   end
111600030128     C*
111700921223     C                   ENDSR
111800020320     C*---------------------------------------------------------------*
111900020320     C* PULISCO 1 FORMATO --------------------------------------------*
112000020320     C*---------------------------------------------------------------*
112100020320     C     PUL01         BEGSR
112200020308     C                   CLEAR                   pov
112300020308     C                   CLEAR                   pdrv
112400020308     C                   CLEAR                   DDCv
112500020308     C                   CLEAR                   pdrdv
112600020701     C                   Z-ADD     SimPOU        pov
112700030128     C                   CLEAR                   ptsr
112800030514     C* se cooperative
112900120920     c                   if        §40TSR <> *blank
113000120920     c                   eval      ptsr = §40TSR
113100120920     c                   else
113200030514     c                   if        kfrtip = 'C'
113300030129     C                   eval        ptsr = 'X'
113400030129     c                   end
113500030514     C* se costi figurativi
113600030514     c                   if        kfrtip = 'G'
113700030514     C                   eval        ptsr = 'A'
113800030514     c                   end
113900120920     c                   endif
114000020308     C                   ENDSR
114100020308     ***********************************************************************
114200011221     ** Uscita dal programma.
114300011221     ***********************************************************************
114400011221     C     Uscita        BEGSR
114500030428      *
114600020321     c* rilascio il padroncino
114700020321     c                   if        §40tla <> 'C'
114800020321     c                   exsr      srwc01
114900020321     c                   end
115000020322     C* Calcolo il totale delle rettifiche
115100020322     c                   move      frepdr        §40pdr
115200020322     c                   move      freddc        §40ddc
115300030128     C     Kfre          SETLL     fifre02L                               86
115400020322     C                   DO        *hival
115500030128     C     kfre          READE     fifre02L                               86
115600020322     C   86              leave
115700030515     c* controllo il tipo servizio (x autotras. ptsr è = ' ')
115800030515     c                   if        ptsr <> ' '
115900030515     c                   if        fretsr <> ptsr
116000030515     c                   iter
116100030515     c                   end
116200030515     c                   else
116300030515     c                   if        fretsr <> 'C' and fretsr <> 'R'
116400030515     c                   iter
116500030515     c                   end
116600030515     c                   end
116700020322     c                   add       fretim        §40tim
116800020322     c                   end
116900030428      *
117000030428      *------------------*
117100030428      * Solo se ricihamato da menù e per Autotrasportatori
117200030514     c                   if        §40flg = *blank and kfrtip = 'A'
117300030428      *
117400030428     C*  deve cancellare il record di allocazione x impedire le trasmissioni
117500030428     C*   in sede durante l'utilizzo di questo programma.
117600030429     C     Kwalc         setll     fiwalc1l
117700030429     C     Kwalc         reade     fiwalc1l
117800030429     c                   dow       not %eof(fiwalc1l)
117900030428     c                   delete    fiwalc00
118000030429     C     Kwalc         reade     fiwalc1l
118100030428     c                   end
118200030430      *
118300030430     c                   commit
118400030430     c                   close     fiwalc1l
118500030428     c                   end
118600030428      *------------------*
118700020321     C*
118800020320     c                   movel(p)  ficn40ds      kpjbu
118900020321     c                   if        §40tla = ' '
119000020321     C                   SETON                                        RT
119100020321     c                   else
119200030128     c                   close     fifre02l
119300020320     c                   movel(p)  cmd1          comando
119400020320     c                   exsr      srcmd
119500020320     C                   SETON                                        LR
119600020320     c                   end
119700011221     C                   RETURN
119800011221
119900011221     C                   ENDSR
120000020320     ***********************************************************************
120100020320     ** QCMDEXC
120200020320     ***********************************************************************
120300020320     C     srcmd         BEGSR
120400020320
120500020321     C                   call      'QCMDEXC'                            30
120600020320     C                   parm                    comando
120700020320     C                   parm                    lun
120800020320     C                   ENDSR
120900020318**
121000020318Causale errata
121100020318Causali duplicate
121200020318Importo errato
121300020320ATTENZIONE!! Lavoro in conferma da altri utenti
121400020424NON inserire rettifiche senza relativa prestazione
121500020719NON inserire rett. su una prestazione senza valore
121600030128Causale errata o non idonea per il Tipo Servizio
121700030310Per Cooperative -> Le Note sono obbligatorie !!
121800030326NON inserire rett.su prestazione senza peso/valore
121900160303Causale non utilizzabile riservata Sede
122000160303Causale non utilizzabile da profili di Sede
