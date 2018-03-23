000100940211     H DECEDIT('0,') DATEDIT(*DMY.)
000200070316     h dftactgrp(*no) actgrp(*caller)
000300070316
000400070418      * Fidg96R *----------------------------------------------------*
000500070403      *     ?   - ASSEGNAZIONE MANUALE spedizioni da consegnare     ?
000600070403      *--------------------------------------------------------------*
000700070403      * Se il p.o. è in parallelo testo la distinta su ARG altrimenti
000800070403      * Se il p.o. è partito testo la distinta su ARB
000900070227      *--------------------------------------------------------------*
001000070404      *  Output
001100070418     Ffidst01l  If   e           k disk    infds(dstds)
001200070404      *
001300070404      *  Input
001400070404     Ffnarb70l  iF   E           K DISK    rename(fnarb000:fnarb70)
001500070307     Ftntbe01L  IF   E           K DISK
001600070228     Fazorg01l  IF   E           K DISK
001700070227      *
001800070418     Ffidg96d   CF   E             WORKSTN
001900940201     F                                     INFDS(DSFMT)
002000070305     D*----------------------------------------------------*
002100070326     D Errmsg          S             78    DIM(30) CTDATA PERRCD(1)             MSG DI ERRORE
002200070326     D*----------------------------------------------------*
002300940211     D* Passaggio Parametri
002400940211     D KPJBA         E DS
002500070227     D KPJBUs          s                   like(KPJBU)
002600940211     D*-------------
002700940211     D DSFMT           DS           512
002800940506     D  $TASTO               369    369
002900940211     D  NRG                  370    370
003000940211     D  NCL                  371    371
003100940211     D  SFLNRR               378    379B 0
003200940127     D*-------------
003300940127     D* Reperimento nome PGM
003400940127     D STATUS         SDS           333
003500940127     D  DSPGM            *PROC
003600030113     D*-------------
003700070320     d dstds           ds
003800070320     d  dst_nrr              397    400b 0
003900070227      *--------------------------
004000070319     D Found_Dist      S              1    INZ('N')
004100070319     D Error_Dist      S              1    INZ('N')
004200070319     D Rec_allocato    S              1    INZ('N')
004300070320      *--------------------------
004400070301     d wNFV            s                   like(dstNFV)
004500070301     d wNPG            s                   like(dstNPG)
004600070316     d wFGS            s                   like(dstFGS)
004700070321     d wCGI            s                   like(argCGI)
004800070321     d wDDC            s                   like(h1cddc)
004900070307     d kCOD1           s                   like(TBEcod)
005000070307     d kKEY1           s                   like(TBEke1)
005100070404     d wfgs_ifp        s                   like(ARBifp)
005200070228      *--------------------------
005300070305      * PARAMETRI PER CONTROLLO DATA E TRASFORM IN GIORNI - XSRDA8 -
005400070305     D WLBDAT          DS                  INZ
005500070305     D  G02DAT                 1      8  0
005600070227     D  G02INV                 9     16  0
005700070227     D  G02ERR                17     17
005800070227     D  G02TGI                18     22  0
005900070307      *
006000070307     D DATPAR          DS
006100070307     D  GIODAT                 1      8  0
006200070307     D  GIOINV                 9     16  0
006300070307     D  GIOTGI                17     21  0
006400070307      *
006500070307     d Wdata8          DS
006600070307     d  dadata                 1      8  0
006700070307     d  adata                  9     16  0
006800070307     d  GioLav                17     21  0
006900070320      *--------------------------
007000070227     d AZUTEds       e ds                  extname(AZUTE00F)
007100070227     d DDatiUte      e ds
007200070320     d* Ds per dati organigramma
007300070228     D OG150         E DS                  INZ
007400070228     D OG146         E DS                  INZ
007500070320     D DgrgFLO       E DS                  INZ
007600070320      *------
007700070404     d $FV6            s              6P 0 dim(600)                             F.V.  x FIDG31R
007800070404      *------
007900070307     D DDGD          E DS
008000070320      *--------------------------
008100070320     d* Parametri x Controllo profilo utenti
008200070320     d TIBS34ds      e ds
008300070227      *--------------------------
008400070227     D* DS PER FNLV50R - CONTROLLO FILIALE GESTIONE
008500070227     D DSLV50        E DS                  EXTNAME(FNLV50DS)
008600070417      *--------------------------
008700070417     D* DS PER - per pulire le distinte
008800070417     D fidg20ds      E DS
008900070320      *--------------------------
009000070320     D* DS PER FIDG04R - ricerca numero distinta
009100070320     D fidg31ds      E DS
009200070320      *--------------------------
009300070228     D fnarbds       e ds                  extname(fnarb00f)
009400070228     D fiargds       e ds                  extname(fiarg00f)
009500070227      *--------------------------
009600070320      *?  COSTANTI                                      ?
009700070320      *--------------------------
009800940506     D* Tasti di funzione
009900940506     D F01             C                   CONST(X'31')
010000940506     D F02             C                   CONST(X'32')
010100940506     D F03             C                   CONST(X'33')
010200940506     D F04             C                   CONST(X'34')
010300940506     D F05             C                   CONST(X'35')
010400940506     D F06             C                   CONST(X'36')
010500940506     D F07             C                   CONST(X'37')
010600940506     D F08             C                   CONST(X'38')
010700940506     D F09             C                   CONST(X'39')
010800940506     D F10             C                   CONST(X'3A')
010900940506     D F11             C                   CONST(X'3B')
011000070315     D F12             C                   CONST(X'3C')
011100940506     D F13             C                   CONST(X'B1')
011200940506     D F14             C                   CONST(X'B2')
011300940506     D F15             C                   CONST(X'B3')
011400940506     D F16             C                   CONST(X'B4')
011500940506     D F17             C                   CONST(X'B5')
011600940506     D F18             C                   CONST(X'B6')
011700940506     D F19             C                   CONST(X'B7')
011800940506     D F20             C                   CONST(X'B8')
011900940506     D F21             C                   CONST(X'B9')
012000940506     D F22             C                   CONST(X'BA')
012100940506     D F23             C                   CONST(X'BB')
012200940506     D F24             C                   CONST(X'BC')
012300940506     D ENTER           C                   CONST(X'F1')
012400940506     D ROLDWN          C                   CONST(X'F4')
012500940506     D ROLLUP          C                   CONST(X'F5')
012600070301     d digits          c                   '0123456789'
012700070302     d Alla_Dist       c                   'alla dist.:'
012800070326     d OPZ_2           c                   'TOGLIE     '
012900070306     d Alla            c                   'alla '
013000070306     d Dalla           c                   'dalla'
013100070307     d FUN_1           c                   'F1=Selez.tutto'
013200070305     d FUN_3           c                   'F3=Fine'
013300070305     d FUN_12          c                   'F12=Ritorno'
013400070309     d FUN_9_Drop      c                   'F9=Altri Dati'
013500940127     C*----------------------------------------------------*
013600070320      *?  MAIN PROGRAM                                     ?
013700940127     C*----------------------------------------------------*
013800940223     C* inizializzazione variabili
013900940223     C                   EXSR      INZVAR
014000940223     C*
014100940223     C     $FINE         DOWEQ     *OFF
014200070413     C*
014300070413     C* 1° Video
014400070413     C     $GEST         CASEQ     'D1'          GESD1                          Video Parametri
014500070416     C* giro Batch:
014600070417      * ? Questa è la Routine che esegue la pulizia chiamando il FIDG20R
014700070417     C     $GEST         CASEQ     'PU'          GESPUL                         Esegue Pulizia
014800070417     C*
014900940117     C                   END
015000070417     C*
015100070417     C                   ENDdo
015200940325     C* fine programma
015300940325     C                   SETON                                            LR
015400070417      *---------------------------------------------------------------*
015500070417      *  ?  OPERAZIONI INIZIALI      ?
015600070417      *---------------------------------------------------------------*
015700070417     C     *INZSR        BEGSR
015800070417     C*
015900070417     C* Reperimento parametri
016000070417     C     *ENTRY        PLIST
016100070417     C                   PARM                    KPJBA
016200070417      *
016300070417      *  Se richiamata da Azione/Menù
016400070417     c                   CLEAR                   se_EDP            1
016500070417     c                   if        %subst(Knmus:1:3) = 'EDP'
016600070417     c                   MOVE      'S'           se_EDP
016700070417     c                   end
016800070417      *
016900070417     c     karb70        klist
017000070417     c                   kfld                    wfgs_ifp
017100070417     c                   kfld                    h1ndc_meno
017200070417      *
017300070417     c     karb70gt      klist
017400070417     c                   kfld                    wfgs_ifp
017500070417     c                   kfld                    arbndc
017600070417     C*
017700070417     C     Kdst          KLIST
017800070417     C                   KFLD                    wnpg
017900070417     C                   KFLD                    Wnfv
018000070417     C                   KFLD                    wfgs
018100070417     C*
018200070417     C     Kdst_01       KLIST
018300070417     C                   KFLD                    wnpg
018400070417     C                   KFLD                    Numero6           6 0
018500070417     C                   KFLD                    wfgs
018600070417     C*
018700070417     C     Kdst8         KLIST
018800070417     C                   KFLD                    wfgs
018900070417     C                   KFLD                    wnpg
019000070417     C                   KFLD                    h1cDDC
019100070417      *
019200070417     C     KTBE          KLIST
019300070417     C                   KFLD                    KCOD1
019400070417     C                   KFLD                    KKEY1
019500070417      *
019600070417      * Reperisco dati job
019700070417     c                   exsr      DatiJob
019800070417
019900070417     C* Variabili per gestione videate
020000070417     C                   MOVE      *BLANK        test_Parallelo    1
020100070417     C                   MOVE      *BLANK        $GEST             2
020200070417     C                   MOVE      *BLANK        $FINE             1
020300070417     C                   MOVE      *BLANK        $INZD1            1
020400070417     C                   MOVE      *BLANK        $EFILE            1
020500070417     C                   MOVE      *BLANK        $ESCI             1
020600070417     C                   MOVE      *BLANK        $RCDOK            1
020700070417     C* Indici
020800070417     C                   Z-ADD     0             X                 3 0
020900070417     C                   Z-ADD     0             Y                 3 0
021000070417     C***
021100070417     C* GIRO DATA DEL GIORNO: LA PRENDO DA TIME
021200070417     C***
021300070417     C                   TIME                    W0140            14 0
021400070417     C                   MOVE      W0140         UDATE8            8 0
021500070417     C                   MOVEL     W0140         HHMM              4 0          *ORA/MINUTI
021600070417     C                   MOVEL     W0140         HHMMss            6 0          *ORA/MINUTI/sec
021700070417     C*
021800070417     C                   Z-ADD     UDATE8        G02DAT                         *GG/MM/AAAA
021900070417     C                   MOVEL     *BLANK        G02ERR
022000070417     C                   CALL      'XSRDA8'
022100070417     C                   PARM                    WLBDAT
022200070417     c                   Z-ADD     G02INV        DATEU8            8 0          *AAAA/MM/GG
022300070417     C*
022400070417     C*  Aggancio tabella 'DGD' x reperire LIMITI CONTROLLO DISTINTA
022500070417     C                   MOVEL     'DGD'         KCOD1
022600070417     C                   MOVEL     *BLANKS       KKEY1
022700070417     C                   MOVEL     '1'           KKEY1
022800070417     C     KTBE          CHAIN     TNTBE01L
022900070417     C                   If        %Found(TNTBE01L)
023000070417     C                   MOVEL     TBEUNI        DDGD
023100070417     C                   END
023200070417     C*
023300070417     C                   ENDSR
023400070417      *---------------------------------------------------------------*
023500070417      *?  Reperimento Dati del job (Utente/Operativi)                ?
023600070417      *---------------------------------------------------------------*
023700070417     c     DatiJOB       BEGSR
023800070417      *
023900070417     c     *dtaara       define    §azute        azuteds
024000070417     c     *dtaara       define    §datiute      ddatiute
024100070417      *
024200070417     c                   in(E)     *dtaara
024300070417     c                   IF        %ERROR or RSUT = *blanks
024400070417     c                   clear                   Tibs34Ds
024500070417     c                   call      'TIBS34R'
024600070417     c                   parm                    Tibs34Ds
024700070417     c                   in        *dtaara
024800070417     c                   ENDIF
024900070417      *
025000070417     c                   ENDSR
025100030113     C************************************************************
025200070320      *?  INIZIALIZZAZIONE VARIABILI                       ?
025300030113     C************************************************************
025400030113     C     INZVAR        BEGSR
025500030113     C*
025600030113     C* Variabili per gestione videate
025700030113     C                   MOVE      *OFF          $FINE
025800030113     C                   MOVE      *OFF          $EFILE
025900030113     C                   MOVE      *OFF          $ESCI
026000030113     C                   MOVE      *OFF          $RCDOK
026100070227     C* Emissione primo video
026200070227     C                   MOVE      'D1'          $GEST
026300070227     C                   MOVE      *ON           $INZD1
026400030113     c*
026500030113     C                   ENDSR
026600070227     C************************************************************
026700070320      *?  Emissione primo video di SCELTA                        ?
026800070227     C************************************************************
026900070227     C     GESD1         BEGSR
027000070227     C*
027100070227     C* inizializzazione videata
027200070227     C     $INZD1        IFEQ      *ON
027300070227     C                   EXSR      INZD1
027400070227     C                   MOVE      *OFF          $INZD1
027500070227     C                   ENDIF
027600070227     C*
027700070228     c     Video_1       tag
027800070302     C*  Emissione Video
027900070228      * ?            *-----------------------*
028000070227     C                   EXFMT     FDG0D01
028100070228      * ?            *-----------------------*
028200070326     c                   setoff                                       99
028300070417      * Selezioni
0284000702271    C                   SELECT
028500070227     C* F3=Fine
028600070227     C                   WHEN      $TASTO   =   F03
028700070418     C                   MOVE      *ON           $FINE
028800070417     c                   goto      end_gesd1
028900070416      *
0290000704161-   C                   ENDSL
029100070227     C* CONTROLLO DATI
029200070227     C                   EXSR      CTRD1
029300070227     C*
029400070228     C* Per errori o decodifiche
029500070228     c   99              goto      Video_1
029600070417      *
029700070417     C* F6=Esegue Batch
029800070417      * ? Questa è la Routine che esegue la pulizia chiamando il FIDG20R
029900070417     C                   if        $TASTO   =   F06
030000070417     c                   eval      $GEST ='PU'
030100070417     c                   end
030200070228      *
030300070417     C     end_gesd1     ENDSR
030400070306     C/EJECT
030500070227     C************************************************************
030600070416      *?  Esegue in Batch  un ciclo sulle distinte               ?
030700070227     C************************************************************
030800070417     C     GESPUL        BEGSR
030900070227     C*
031000070416      * ? ----------------------------
031100070416     c* se il p.o. è in parallelo testo la distinta su ARG altrimenti
031200070416     c* se il p.o. è partito testo la distinta su ARB e ARG
031300070416     c                   if        test_Parallelo = 'S'
031400070416     C/EXEC SQL
031500070417     C+ DECLARE A1 CURSOR FOR SELECT argndc, argddc FROM fiarg02l JOIN
031600070417     C+ fnarb01l ON arbaas= argaas and arblnp=arglnp and arbnrs= argnrs
031700070417     C+ and arbnsp= argnsp WHERE argpoc = :d1cFGS and argNDC between
031800070417     C+ :h1ndc_meno and :h1ndc_men2 GROUP BY argndc, argddc ORDER BY
031900070417     C+ argndc, argddc
032000070416     C/END-EXEC
032100070416
032200070416     C/EXEC SQL
032300070416     C+ OPEN a1
032400070416     C/END-EXEC
032500070416     c                   else
032600070416     c                   z-add     wfgs          wfgs_ifp
032700070416      * ? ----------------------------
032800070416     c     karb70        setll     fnarb70l
032900070416      * ? ----------------------------
033000070416     c                   end
033100070416      * ? ----------------------------
033200070416     C*
033300070416     C*  Per EoF deve uscire
033400070416     C                   MOVEL     *OFF          $EFILE
033500070416     C*  Per Record Ok deve uscire riportando i dati
033600070416     C                   MOVEL     *OFF          $RCDOK
033700070416     C*
0338000704161    C     $EFILE        DOUEQ     *ON
033900070416     C     $RCDOK        OREQ      *ON
034000070416     C*
034100070416      *  x Togliere
034200070416     c                   if        test_Parallelo = 'S'
034300070416      * ?LETTURE  FILE
034400070416     C/EXEC SQL
034500070417     C+ FETCH NEXT FROM a1 INTO  :argndc, :argddc
034600070416     C/END-EXEC
034700070417     c                   z-add     argndc        Numero            7 0
034800070417     c                   z-add     argddc        Data              8 0
034900070416     c                   else
035000070416      **
035100070416     c     wfgs_ifp      reade     fnarb70l
035200070416     C                   eval      sqlcod = 0
035300070416      *
035400070416      * Fine File
035500070416     c                   if        %Eof(fnarb70l)
035600070416     C                   eval      sqlcod = 100
035700070416     c                   else
035800070416      **
035900070416      * NO Fine File
036000070416     c                   if        arbndc > h1ndc_men2
036100070416     C                   eval      sqlcod = 100
036200070416     c                   else
036300070417      **
036400070417     c                   z-add     arbndc        Numero            7 0
036500070417     c                   z-add     arbddc        Data              8 0
036600070416     c     karb70gt      setgt     fnarb70l
036700070416     c                   end
036800070416      **
036900070416     c                   end
037000070416      **
037100070416     c                   end
037200070416      *-------------
037300070416     C                   select
037400070416      * Fine File
037500070416     C                   when      sqlcod = 100
037600070416     C                   MOVEL     *ON           $EFILE
037700070416     C                   MOVE      $EFILE        *IN33
037800070416      * errori
037900070416     C                   WHEN      SqlCod < 0
038000070416     c                   if        sqlcod <> -913
038100070416     C                   seton                                        H1
038200070416     c                   end
038300070416      *
038400070416     C                   other
038500070417      *
038600070417      * Aggancia la distinta
038700070417     c                   z-add     numero        numero6
038800070418     C     Kdst_01       chain     fiDST01l
038900070418      * Esegue la pulizia non delle manuali
039000070417     c* se devo pulire i riferimenti della distinta chiamo il pgm FIDG20R
039100070417     c                   if        %Found(fiDST01l)
039200070417     c                   clear                   fidg20ds
039300070417     c                   eval      d20cmti  = 'S'
039400070417     c                   eval      d20fgsi  = Wfgs
039500070417     c                   eval      d20nfvi  = dstnfv
039600070417     c                   eval      d20dfvi  = dstdfv
039700070417     c                   eval      d20fppi  = dstfpp
039800070417     c                   eval      d20tpmi  = dsttpm
039900070417     c                   eval      kpjbus   = kpjbu
040000070417     c                   eval      kpjbu    = fidg20ds
040100070417     c                   call      'FIDG20R'
040200070417     c                   parm                    kpjba
040300070417     c                   eval      fidg20ds = kpjbu
040400070417     c                   eval      kpjbu    = kpjbus
040500070417     c                   if        d20erro<> ' '
040600070417     c                   end
040700070417     c                   end
040800070416      *
040900070416     C                   endsl
041000070416     C*
0411000704161-   C                   ENDDO
041200070417     C*
041300070417     c                   if        test_Parallelo = 'S'
041400070416     C/EXEC SQL
041500070416     C+ CLOSE a1
041600070416     C/END-EXEC
041700070417     c                   end
041800070417     C*
041900070417     C* Emissione primo video
042000070417     C                   MOVE      'D1'          $GEST
042100070417     C                   MOVE      *ON           $INZD1
042200070417     C*
042300070416     C                   ENDSR
042400070416     C/EJECT
042500070416     C************************************************************
042600070416      *?  INIZIALIZZAZIONE Primo video                           ?
042700070416     C************************************************************
042800070416     C     INZD1         BEGSR
042900070416     C*
043000070227     C* Pulizia campi e indicatori
043100070308     c                   exsr      INZ_indERR
043200070227     C* Pulizia formato record
043300070227     c                   clear                   FDG0D01
043400070227     C* campi titolo
043500070302     C                   MOVEL     dsPGM         D1CPGM
043600070227     C                   MOVEL     knsif         D1nsif
043700070227     C                   MOVEL     knmus         D1nmus
043800070227     C                   MOVEL     RSUT          D1crsu
043900070227     C* campi video
044000070227     C* Il p.o.deve essere impostato in base al profilo
044100070413     C     dutlpo        IFEQ      '2'
044200070227     C                   MOVEL     dutpou        D1CFGS                         *FIL.GESTIONE
044300070413     C                   ELSE
044400070413     C                   MOVEL     SIMFEL        D1CFGS
044500070413     C                   END
044600070227     C                   z-add     UDATE8        D1cDDC
044700070227     C*
044800070227     C                   ENDSR
044900940127     C************************************************************
045000070320      *? Controlli primo video di SCELTA Operazione da eseguire  ?
045100940127     C************************************************************
045200070227     C     CTRD1         BEGSR
045300030113     C*
045400070227     C*  Spegne indicatori di errore
045500070308     c                   exsr      INZ_indERR
045600070227     c                   setoff                                       99
045700070228     C*  campi di Work
045800070308     c                   clear                   h1cddc
045900070319     c                   clear                   h1ndc_meno
046000070319     c                   clear                   h1fpp_meno
046100070416     c                   clear                   h1ndc_men2
046200070416     c                   clear                   h1fpp_men2
046300070321     c                   clear                   wfgs
046400070321     c                   clear                   wddc
046500070321     c                   clear                   wcgi
046600070301      * Categoria Foglio x controllo nr.distinta
046700070301     C                   z-add     4             wNPG
046800070227     C* --------------
046900070227      * ?- Controlla la filiale in gestione
047000070227     C                   CLEAR                   DSLV50
047100070227     C                   MOVEL     KNMUS         D50PRU
047200070227     C                   z-add     d1cFGS        D50FGS
047300070227     C                   CALL      'FNLV50R'
047400070227     C                   PARM                    DSLV50
047500070227    1C     D50ERR        IFNE      ' '
047600070326     c                   movel     ErrMsg(1)     d1msg
047700070326     C                   SETON                                        51  99
047800070227     C                   goto      ENDCTR1
047900070227    1C                   ENDIF
048000070227      *
048100070227     C*  Controlla Con il P.O. la data abilitazione
048200070227     C     d1cFGS        CHAIN     AZORG01l
048300070227    1C                   IF        %found(azorg01l)
048400070227     C                   MOVEL     ORGdf0        og150
048500070227     C                   MOVEL     ORGde6        og146
048600070227     c                   if             §ogdda = *blank or §ogdda = *all'0'
048700070227     C                   MOVE      *ON           $FINE
048800070227     C                   goto      ENDCTR1
048900070227     c                   else
049000070227      * se c'è una data in organigramma
049100070227     c                   move      §ogdda        data_org          8 0
049200070227      * se siamo in fase di Parallelo prima di essere partiti
049300070426     c                   if        data_org = 20391231 or dateu8 < data_org
049400070227     C                   eval      test_Parallelo = 'S'
049500070227     c                   end
049600070227      * se data distinte automatiche NON è
049700070227      * né = 31/12/2039 che significa p.o. in fase di test o parallelo
049800070227      * né < oggi che significa p.o. partito
049900070227      * allora fuori
050000070426     c******             if        data_org <> 20391231 and dateu8 < data_org
050100070426     C******             MOVE      *ON           $FINE
050200070426     C******             goto      ENDCTR1
050300070426     c******             end
050400070227     c                   end
050500070227      *-
050600070227   X1C                   ELSE
050700070227      * Non presente in organigramma
050800070326     C                   SETON                                        51  99
050900070326     c                   movel     ErrMsg(1)     d1msg
051000070227     C                   goto      ENDCTR1
051100070227    1C                   ENDIF
051200070227     C*
051300070316      * Salva la filiale su campo di work x tutto il pgm
051400070316     c                   z-add     d1cfgs        wfgs
051500070227     C* --------------
051600070227      * ?- Data Distinte del:
051700070227     C                   Z-ADD     d1cDDC        G02DAT
051800070227     C                   MOVEL     *BLANK        G02ERR
051900070227     C                   CALL      'XSRDA8'
052000070227     C                   PARM                    WLBDAT
052100070227    1C     G02ERR        IFEQ      '1'
052200070326     c                   movel     ErrMsg(2)     d1msg
052300070326     C                   SETON                                        52  99
052400070227     C                   goto      ENDCTR1
052500070227    1C                   ENDIF
052600070307      *
052700070307      *  Per testare ??????????????????????????????????
052800070307     c                   goto      salta_ctrl
052900070307      *
053000070307      * Controlli identici al FNLR50R sulla data distinta
053100070307      * prima della data del giorno
053200070307     c                   if        g02inv < dateu8
053300070326     c                   movel     ErrMsg(3)     d1msg
053400070326     c                   SETON                                        52  99
053500070307     C                   goto      ENDCTR1
053600070307     c                   endif
053700070307      *controllo range fra le date di oggi e quella immessa
053800070413      *  non può essere superiore di 3 giorni lavorativi
053900070307     c                   eval      dadata = dateu8
054000070307     c                   eval      adata  = g02inv
054100070307     c                   CALL      'XSRLAV8'
054200070307     c                   PARM                    Wdata8
054300070413     c                   if        giolav > §dgddc or
054400070413     c                             giolav > 3
054500070326     c                   movel     ErrMsg(4)     d1msg
054600070326     c                   SETON                                        52  99
054700070307     C                   goto      ENDCTR1
054800070307     c                   end
054900070307      *
055000070307     C* VERIFICO CHE LA DATA DELLA DISTINTA NON SIA ANTECEDENTE O
055100070307     C* SEGUENTE ALLA UDATE DI OLTRE 15 GIORNI
055200070307     C* PER TUTTO IL MESE DI AGOSTO NON CONTROLLO 1 15 GG
055300070307     C                   MOVE      dateu8        WMGUDT            4 0
055400070307     C                   MOVEL     WMGUDT        WMUDT             2 0
055500070307     C                   Z-ADD     §DGDGG        NRGIO             3 0
055600070307      *
055700070307     C     WMGUDT        IFGE      §DGDEI
055800070307     C     WMGUDT        ANDLE     §DGDEF
055900070307     C                   Z-ADD     §DGDGE        NRGIO
056000070307     C                   END
056100070307      *
056200070307     C     dateu8        IFGE      §DGDII
056300070307     C     dateu8        ANDLE     §DGDIF
056400070307     C                   Z-ADD     §DGDGI        NRGIO
056500070307     C                   END
056600070307      *
056700070307     C     G02TGI        SUB       NRGIO         GIOTGI
056800070307     C                   CALL      'XSRGI8'
056900070307     C                   PARM                    DATPAR
057000070307     C                   Z-ADD     GIOINV        DATM15            8 0
057100070307      *
057200070307     C     DATM15        IFGT      dateu8
057300070326     c                   movel     ErrMsg(5)     d1msg
057400070326     C                   SETON                                        52  99
057500070307     C                   goto      ENDCTR1
057600070307     C                   END
057700070307      *
057800070307     C     G02TGI        ADD       NRGIO         GIOTGI
057900070307     C                   CALL      'XSRGI8'
058000070307     C                   PARM                    DATPAR
058100070307     C                   Z-ADD     GIOINV        DATP15            8 0
058200070307      *
058300070307     C     DATP15        IFLT      dateu8
058400070326     c                   movel     ErrMsg(5)     d1msg
058500070326     C                   SETON                                        52  99
058600070307     C                   goto      ENDCTR1
058700070307     C                   END
058800070307      *
058900070307     c     salta_ctrl    tag
059000070307      *
059100070227     C* IMPOSTO A VIDEO LA DATA A 8 SE IMMESSA A 6
059200070301     C                   Z-ADD     G02INV        h1cDDC
059300070321     c                   z-add     G02INV        wddc
059400070301     C                   Z-ADD     G02DAT        D1cDDC
059500070301     C*------------------------
059600070416      ** Se richiesto tutto
059700070416     c                   if        (d1ndc2 = *zeros or d1ndc2 = *blanks) and
059800070416     c                                d1ndc3 = *all'9'
059900070416      **
060000070416     c                   if        $TASTO   =   F09
060100070416     c                   movel     ErrMsg(7)     d1msg
060200070416     C                   SETON                                        565899
060300070416     C                   goto      ENDCTR1
060400070416     c                   end
060500070416      **
060600070416     c                   move      d1ndc2        h1ndc_meno
060700070416     c                   move      d1ndc3        h1ndc_men2
060800070416      **
060900070416     c                   else
061000070416      **
061100070416      ** Se non richiesto tutto
061200070416     c                   if        d1ndc2 = *zeros or d1ndc2 = *blanks
061300070313     c                   clear                   d1ndc2
061400070313     c                   end
061500070316     c     '?'           scan      d1ndc2
061600070228     C*   ricerca distinta appartenente al giro oppure tutte
061700070316     c                   if        %Found   or
061800070316     c                             d1ndc2 = *zeros or d1ndc2 = *blanks
061900070326     C******             SETON                                            99
062000070321     c                   exsr      Ricerca_Dist
062100070228     C                   MOVE      *zeros        d1ndc2
062200070313     C                   MOVE      FDGNFV        d1ndc2
062300070326     C******             goto      ENDCTR1
062400070228     c                   end
062500070228      *  controllo validità distinta
062600070301     c     digits        check     d1ndc2
062700070301     c                   if        %Found
062800070326     c                   movel     ErrMsg(9)     d1msg
062900070301     C                   SETON                                        56  99
063000070301     C                   goto      ENDCTR1
063100070301     c                   end
063200070301      *
063300070301     c                   move      d1ndc2        wNFV
063400070319      *                                     ?  =======  ?
063500070316     c                   exsr      CHAIN_Dist
063600070319     c                   if        Error_Dist = 'S'
063700070319     c                   if        Rec_Allocato = 'S'
063800070326     c                   movel     ErrMsg(11)    d1msg
063900070319     c                   end
064000070326     C                   SETON                                            56  99
064100070319     C                   goto      ENDCTR1
064200070319     c                   end
064300070319      *
064400070316     c                   if        Found_Dist = 'N'
064500070326     c                   movel     ErrMsg(9)     d1msg
064600070301     C                   SETON                                        56  99
064700070301     C                   goto      ENDCTR1
064800070301     c                   else
064900070417      * Distinta non nella data
065000070417     c                   if        dstDFV <> h1cDDC
065100070417     c                   movel     ErrMsg(8)     d1msg
065200070417     C                   SETON                                        56  99
065300070417     C                   goto      ENDCTR1
065400070417     c                   end
065500070301      * Foglio chiuso o Annullato
065600070301    3C     dstFCF        ifEQ      'S'
065700070301    4C     dstATB        orNE      ' '
065800070326     c                   movel     ErrMsg(10)    d1msg
065900070326     C                   SETON                                        56  99
066000070301     C                   goto      ENDCTR1
066100070301     c                   endIF
066200070301     c                   end
066300070301      * se tutto OK
066400070319     c                   z-add     dstPDR        h1pdr_meno
066500070319     c                   move      dstFPP        h1fpp_meno
066600070319     c                   move      d1ndc2        h1ndc_meno
066700070416      *
066800070301     C*---------------
066900070416     c                   if        d1ndc3 = *zeros or d1ndc3 = *blanks
067000070416     c                   clear                   d1ndc3
067100070416     c                   end
067200070416     c     '?'           scan      d1ndc3
067300070228     C*   ricerca distinta appartenente al giro oppure tutte
067400070326     c                   if        %Found
067500070416     c                             or d1ndc3 = *zeros or d1ndc3 = *blanks
067600070326     C**********         SETON                                            99
067700070321     c                   exsr      Ricerca_Dist
067800070416     C                   MOVE      *zeros        d1ndc3
067900070416     C                   MOVE      FDGNFV        d1ndc3
068000070326     C********           goto      ENDCTR1
068100070228     c                   end
068200070301      *  controllo validità distinta
068300070416     c     digits        check     d1ndc3
068400070301     c                   if        %Found
068500070326     c                   movel     ErrMsg(9)     d1msg
068600070301     C                   SETON                                        58  99
068700070301     C                   goto      ENDCTR1
068800070301     c                   end
068900070227     C*
069000070416     c                   move      d1ndc3        wNFV
069100070319      *                                     ?  =======  ?
069200070316     c                   exsr      CHAIN_Dist
069300070319     c                   if        Error_Dist = 'S'
069400070319     c                   if        Rec_Allocato = 'S'
069500070326     c                   movel     ErrMsg(11)    d1msg
069600070319     c                   end
069700070326     C                   SETON                                            58  99
069800070319     C                   goto      ENDCTR1
069900070319     c                   end
070000070319      *
070100070316     c                   if        Found_Dist = 'N'
070200070326     c                   movel     ErrMsg(9)     d1msg
070300070301     C                   SETON                                        58  99
070400070301     C                   goto      ENDCTR1
070500070301     c                   else
070600070417      * Distinta non nella data
070700070417     c                   if        dstDFV <> h1cDDC
070800070417     c                   movel     ErrMsg(8)     d1msg
070900070417     C                   SETON                                        58  99
071000070417     C                   goto      ENDCTR1
071100070417     c                   end
071200070301      * Foglio chiuso o Annullato
071300070301    3C     dstFCF        ifEQ      'S'
071400070301    4C     dstATB        orNE      ' '
071500070326     c                   movel     ErrMsg(10)    d1msg
071600070326     C                   SETON                                        58  99
071700070301     C                   goto      ENDCTR1
071800070301     c                   endIF
071900070301     c                   end
072000070314      *---------------
072100070301      * se tutto OK
072200070416     c                   z-add     dstPDR        h1pdr_men2
072300070416     c                   move      dstFPP        h1fpp_men2
072400070416     c                   move      d1ndc3        h1ndc_men2
072500070301     C*
072600070416     c                   if            d1ndc3 < D1NDC2
072700070416     c                   movel     ErrMsg(7)     d1msg
072800070416     C                   SETON                                        565899
072900070416     C                   goto      ENDCTR1
073000070416     c                   end
073100070416      *
073200070416     c                   end
073300070416      *
073400070227     C     ENDCTR1       ENDSR
073500070301      *---------------------------------------------------------------*
073600070319      *?  Aggancia la Distinta x allocarla ed in seguito x aggiornarla ?
073700070301      *---------------------------------------------------------------*
073800070321     C     Ricerca_Dist  BEGSR
073900070228     C*
074000070321     C                   clear                   FIDG31DS
074100070321     C                   Z-ADD     0             FDGNFV
074200070321     C                   MOVEL     wFGS          FDGFGS
074300070326      *
074400070326     C                   if        $TASTO   <>  F05
074500070321     C                   MOVEL     wCGI          FDGCGI
074600070326     c                   end
074700070326      *
074800070321     C                   Z-ADD     wDDC          FDGGDA
074900070321     C                   Z-ADD     wDDC          FDGGAL
075000070321     C                   move      wNPG          FDGNPG
075100070321     C                   move      'S'           FDGRIC
075200070321     C                   move      '2'           FDGTFV
075300070321     C                   move      'P'           FDGTPV
075400070321     c                   eval      kpjbus = kpjbu
075500070404     c                   clear                   $FV6
075600070404      * impostato a tutti 99999
075700070404     c                   do        600           yy                3 0
075800070404     c                   z-add     999999        $FV6(yy)
075900070404     c                   enddo
076000070321     C                   MOVEL(p)  FIDG31DS      KPJBU
076100070321     C                   CALL      'FIDG31R'
076200070321     C                   PARM                    KPJBA
076300070404     C                   PARM                    $FV6
076400070321     C                   MOVEL     KPJBU         FIDG31DS
076500070321     c                   eval      kpjbu = kpjbus
076600070321     C*
076700070321     C                   ENDSR
076800070321      *---------------------------------------------------------------*
076900070321      *?  Aggancia la Distinta x allocarla ed in seguito x aggiornarla ?
077000070321      *---------------------------------------------------------------*
077100070321     C     Chain_Dist    BEGSR
077200070321     C*
077300070316      * inizializza il trovato/non trovato
077400070316     c                   eval      Found_Dist = 'N'
077500070319     c                   eval      Error_Dist = 'N'
077600070319     c                   eval      rec_allocato = 'N'
077700070316     C*
077800070418     C     Kdst          CHAIN     fidst01l
077900070316      * Trovato record
078000070316     c                   if        %Found(fidst01l)
078100070316     c                   eval      Found_Dist = 'S'
078200070316     c                   end
078300070316     C*
078400070316     C     ENDChain      ENDSR
078500070302     C/EJECT
078600070302     C************************************************************
078700070320      *?  INIZIALIZZA indicatori di errore                           ?
078800070302     C************************************************************
078900070308     C     INZ_indERR    BEGSR
079000070308      *
079100070308     C                   move      *ALL'0'       IN5190           40
079200070308     C                   moveA     IN5190        *IN(51)
079300070308      *
079400070308     C                   ENDSR
079500070308     C/EJECT
079600070301      *---------------------------------------------------------------*
079700070326** ErrMsg
079800070326La Filiale non è in gestione                                                   01
079900070326Data errata                                                                    02
080000070326Data antecedente alla data odierna                                             03
080100070326Data distinta superiore alla data odierna dei gg.ammessi                       04
080200070326Data distinta anteriore o superiore alla data odierna dei gg.ammessi           05
080300070326Non esistono distinte aperte nella data scelta                                 06
080400070416Range Distinte non corretto o non ammesso                                      07
080500070417Distinta non appartenente alla data selezionata                                08
080600070326Numero Distinta non valido o inesistente                                       09
080700070326Distinta chiusa o annullata                                                    10
080800070413Distinta ancora in uso da un altro lavoro.                                     11
080900070416                                                                               12
081000070326La Distinta è inesistente o non è stata selezionata                            13
081100070326La Distinta è chiusa o annullata                                               14
081200070326La Distinta è ancora allocata. Non è quindi possibile selezionarla.            15
081300070416                                                                               16
081400070418                                                                               17
081500070418                                                                               18
081600070418                                                                               19
081700070418                                                                               20
081800070418                                                                               21
081900070418                                                                               22
082000070418                                                                               23
082100070418                                                                               24
082200070418                                                                               25
082300070418                                                                               26
082400070418SI PREGA DI USCIRE dal lavoro!                                                 27
082500070418                                                                               28
082600070418                                                                               29
082700070326                                                                               30
