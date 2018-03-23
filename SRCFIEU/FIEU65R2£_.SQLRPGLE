000100150429     hDECEDIT('0,') DATEDIT(*YMD.)
000200000000      *****************************************************************
000300160930      *    crea recupero x DIROTTAMENTO fatto da CLEARING su solo ESTERO
000400000000      *****************************************************************
000500160930     FFNARB01L  IF   E           K DISK
000600161003     Ffibsp01l  iF   E           K DISK
000700161003     Ftabel00f  iF   E           K DISK
000800161003     FFiapd01L  IF   E           K DISK
000900161014     FTita430C  uF   E           k DISK
001000161005     FTitaA30C  iF   E           k DISK
001100161005     FTitaS30C  iF   E           k DISK
001200161003      **?------------------------------------------------------------------ */
001300161003     D TISI95DS      E DS
001400161005     d TIBS69DS      E DS                  INZ
001500161005     d DS_cnaco      E DS                  extname(CNACO00F)
001600161005     d DS_cnind      E DS                  extname(CNIND00F)
001700161005     d DS_cnclp      E DS                  extname(CNCLP00F)
001800161005     d DS_fncls      E DS                  extname(FNCLS00F)
001900161005     D DTA4A         E DS
002000161003     D DTASV         E DS
002100161003     D Dblp          E DS
002200161003     D Dblp01        E DS
002300170324     d ParLnpB         s              3  0
002400161003     D trul90ds      E DS
002500161005     D DS3A          E DS
002600161014     D DSbl4I        E DS
002700161003      **?------------------------------------------------------------------ */
002800161003      *?  Parametri x Controllo profilo utenti  ?
002900161003      **?------------------------------------------------------------------ */
003000161003     d TIBS34ds      e ds
003100161003     d TIBS36ds      e ds
003200161003     d AZUTEds       e ds                  extname(AZUTE00F)
003300161003     d DDatiUte      e ds
003400161003     D                 DS
003500161003     D  TESTO3                 1      8
003600161003     D  RAAS                   9     10  0
003700161003     D  RLNP                  12     14  0
003800161003     D  RNRS                  16     17  0
003900161003     D  RNSP                  19     25  0
004000161003     D  RIFSPE                 1     25
004100161003     D*------------------
004200161003     D* DS REPEIMENTO TERMINALs
004300161003     D*------------------
004400161003     D FNLV55DS      E DS
004500161003      *---------------------
004600161003     D Wdataeur        S               D   DATFMT(*eur)
004700161003     D Wdataiso        S               D   DATFMT(*iso)
004800161003      *---------------------
004900160930      *
005000160930      *----------------------------------------------------*
005100030116     d KPJBA         E DS
005200160930     D savKPJBU        S                   like(KPJBU) inz
005300160927     D*
005400160926     D tnecf00f      e DS
005500160926     D tnefr00f      e DS
005600161130     D FIEU18        E DS                  EXTNAME(FIEU18DS)
005700160930     D FNARB00       E DS                  EXTNAME(FNARB00F)
005800160926     D*
005900151210     d Wdata8          DS
006000151210     d  dagiolav               1      8  0
006100151210     d  agiolav                9     16  0
006200151210     d  GioLav                17     21  0
006300160926      *
006400160926     D WrkSqlCmd       S           1024
006500151106      *****************************************************************
006600000000     D WLBDAT          DS
006700940926     D  G02DAT                 1      8  0
006800940926     D  G02INV                 9     16  0
006900940926     D  G02ERR                17     17
007000940926     D  G02TGI                18     22  0
007100160308      *
007200151106     D parDate         DS
007300151106     D   DaData                1      8  0
007400161003     D    aData                9     16  0
007500160923     D     Anno                9     12  0
007600160923     D     Mese               13     16  0
007700151106      *
007800151106     D DATA_eur        S               D   DATFMT(*eur)
007900151106     D DATA_iso        S               D   DATFMT(*iso)
008000161003      *---------------------------------------------------------------------*
008100161003     d Digits          c                   Const('0123456789')
008200161014      *---------------------------------------------------------------------*
008300161014     iTITA4000      01
008400161014     iTITA4P00      02
008500161014      *---------------------------------------------------------------------*
008600000000     C     *ENTRY        PLIST
008700000000     C                   PARM                    KPJBA
008800160923     C                   eval      parDate = KPJBU
008900160930     C                   eval      savKPJBU= KPJBU
009000030120      *
009100160930     C     kbolla        kLIST
009200160930     C                   kfld                    ecfAAS
009300160930     C                   kfld                    ecfLNP
009400160930     C                   kfld                    ecfNRS
009500160930     C                   kfld                    ecfNSP
009600161003     C*
009700161003     C     Kapd          KLIST
009800161003     C                   KFLD                    tipoaut
009900161003     C                   KFLD                    Autista
010000161003     C                   move      'A'           tipoaut           1
010100161003      *
010200161003     C     KtaB          KLIST
010300161003     C                   KFLD                    tblKUT
010400161003     C                   KFLD                    tblCOD
010500161003     C                   KFLD                    tblKEY
010600161003     C                   Z-ADD     1             tblKUT
010700161003     C*
010800161014     C     Kta4          KLIST
010900161003     C                   KFLD                    tasAAS
011000161003     C                   KFLD                    tasLNP
011100161003     C                   KFLD                    tasNRS
011200161003     C                   KFLD                    tasNSP
011300161003     C                   KFLD                    §TRC              1
011400161003      *
011500170406     c                   in(E)     *dtaara
011600161003     c                   IF        %ERROR or RSUT = *blanks
011700161003     c                   clear                   Tibs34Ds
011800161003     c                   call      'TIBS34R'
011900161003     c                   parm                    Tibs34Ds
012000161003     c                   in        *dtaara
012100161003     c                   ENDIF
012200161003      *
012300161003     c     *dtaara       define    §azute        azuteds
012400161003     c     *dtaara       define    §datiute      ddatiute
012500161003      *
012600161003     C                   CLEAR                   Tibs36Ds
012700161003     c                   EVAL      I36ute = 'EDPCED'
012800161003     c                   EVAL      I36Tla = 'L'
012900161003     C                   CALL      'TIBS36R'
013000161003     C                   PARM                    Tibs36Ds
013100161003      *
013200161003      * Recupero data e ora
013300161003     C                   TIME                    WORA              6 0
013400161003     C                   TIME                    W0140            14 0
013500161003      * UDATE IN GGMMAAAA
013600161003     C                   MOVE      W0140         oggi              8 0
013700161003      * UDATE IN AAAAMMGG
013800161003     C                   MOVEL     oggi          Wdataeur
013900161003     C                   MOVEL     Wdataeur      Wdataiso
014000161003     C                   MOVEL     Wdataiso      datacor           8 0
014100161003      *
014200030827     c                   movel     KNMUS         ALFA3             3
014300160930      *
014400160930      *--------------------------------------------------------------*
014500160926      *
014600160930      *   IMPOSTA SQL x lettura
014700160926     c                   exsr      istruz
014800160926      *
014900000000      *--------------------------------------------------------------*
015000160926      *
015100160926     C/EXEC SQL
015200160926     C+ PREPARE S1 FROM :WrkSqlCmd
015300160926     C/END-EXEC
015400160926      *
015500160926     C/EXEC SQL
015600160926     C+ DECLARE A1 CURSOR FOR S1
015700160926     C/END-EXEC
015800160926      *
015900160926     C/EXEC SQL
016000160926     C+ OPEN A1
016100160926     C/END-EXEC
016200160926      *
016300160926     C                   DOU       SqlCod <> 0
016400160926     C/EXEC SQL
016500160927     C+ FETCH NEXT FROM A1 INTO :efrRIF, :efrCAU, :efrDFT,
016600161003     C+       :ecfAAS, :ecfLNP, :ecfNRS, :ecfNSP, :ecfRIF, :ecfLNA
016700160926     C/END-EXEC
016800160926     C                   SELECT
016900160926     c*
017000160926     c* a fine file Totali x rotture
017100160926     C                   WHEN      SqlCod = 100
017200160926     c                   leave
017300160926     **
017400160926     C                   WHEN      SqlCod < 0
017500160926     C                   seton                                        H1
017600160926     c                   goto      fine
017700160926      *
017800160926     C                   OTHER
017900160926     c* DETTAGLIO
018000161014     c                   exsr      Elab_Bolla
018100160926     C                   ENDSL
018200160926      *
018300160926     C                   ENDDO
018400160926     C/EXEC SQL
018500160926     C+ CLOSE A1
018600160926     C/END-EXEC
018700160926     c     fine          tag
018800160930     C                   eval       KPJBU = savKPJBU
018900160926     C                   SETON                                        LR
019000160926     C**************************************************************************
019100160926     C* mi compongo l'istruzione sql
019200160926     C**************************************************************************
019300160926     C     istruz        BEGSR                                                  *
019400160926      **
019500160926     c* MI COMPONGO L'ISTRUZIONE SQL che legge il file TNECF
019600160926     C                   EVAL      WrkSqlCmd  =
019700160926     C                             'with ORIGINE as ( '                        +
019800160926     c                             ' SELECT efrRIF, efrCAU, efrDFT '           +
019900160926     c                             ' FROM tnefr00f WHERE efrntw=''DPD'' and '  +
020000160926     c                             ' efrrif like''084%'' and '                 +
020100160929     c                             ' substr(efrcau, 1, 1)=''D'' and '          +
020200160926     c                             ' efrdft between ' + %editc(daDATA:'X')     +
020300160926     c                             ' and ' + %editc(aDATA:'X')                 +
020400160926     c                             ' GROUP BY efrrif, EFRCAU, efrdft ),'       +
020500160926     C                             '     LEGAME  as ( '                        +
020600160926     c                             ' SELECT ecfAAS, ecfLNP, ecfNRS, ecfNSP, '  +
020700161003     c                             '  ecfRIF, ecfLNA FROM tnecf00f WHERE '     +
020800160926     c                             '  ecfgrp =''001'' and ecftbl=''F1'' and '  +
020900160926     c                             '  ecfntw=''DPD'' and ecfrif like ''084%'' '+
021000160926     c                             '  and ecfdfp between ' + %editc(daDATA:'X')+
021100160926     c                             ' and ' + %editc(aDATA:'X') + ')'           +
021200160926     c                             '  SELECT Origine.*, legame.*            '  +
021300160926     c                             '    FROM legame join origine            '  +
021400160926     c                             '      on ecfrif = efrrif                '  +
021500160926     c                             '      order by efrcau, ecfaas, ecflnp   '
021600160926     c                   ENDSR
021700160926      * ?------------------------------------------------------------------ */
021800160926      **
021900151113      * ?------------------------------------------------------------------ */
022000161014     c     Elab_Bolla    BEGSR
022100161003      *
022200161130     c                   clear                   FIEU18
022300160930     c                   clear                   fnarb00
022400161003      *
022500161003     c                   eval      i5PARCEL = ecfRIF
022600161003      *
022700160930     c     Kbolla        chain     FNARB01l
022800161003      *  deve allora cercare in SEDE
022900161003     c                   if           %Found(FNARB01L)
023000161003      **
023100161003      * Scrive una bolla di recupero su DIROTTAMENTI segnalati da DPD
023200161003      * che verranno poi addebitati sul clearing
023300161003     c                   call      'FIEU18R2'
023400161130     c                   parm                    FIEU18
023500161003     c                   parm                    fnarb00
023600161003     c                   parm                    KPJBA
023700161003      **
023800161003     c                   else
023900161003      **
024000161003      **  SE GIA' ESEGUITO IL TRASFERIMENTO va sulle BOLLE di SEDE
024100161003     c     Kbolla        chain     TITAS30C
024200161003     c                   if           %Found(TITAS30C)
024300161014     C*
024400161014     C                   CLEAR                   TA4NOT
024500161014     C                   MOVE      'I'           §TRC
024600161017     C     Kta4          CHAIN(N)  tita430c
024700161014     C                   IF        %FOUND(TITA430C)
024800161014     C                   MOVEL     ta4NOT        DsBL4I
024900161014     c                   if        §B4RECDIR = *blank
025000161014      **
025100161003      * sostituisce i campi di ARB
025200161003     c                   eXSR      BOLLA_SEDE
025300161014      **
025400161014     c                   if          ha_scritto = 'S'
025500161017     C                   CLEAR                   TA4NOT
025600161017     C                   MOVE      'I'           §TRC
025700161017     C     Kta4          CHAIN     tita430c
025800161017     C                   IF        %FOUND(TITA430C)
025900161017     C                   MOVEL     ta4NOT        DsBL4I
026000161014     c                   eval      §B4RECDIR = 'D'
026100161017     C                   eval      ta4NOT    =   DsBL4I
026200161014     c   01              update    TITA4000
026300161014     c   02              update    TITA4P00
026400161017     c                   end
026500161017     c                   endif
026600161014      **
026700161014     c                   end
026800161014     c                   end
026900161003     c                   end
027000161003      *
027100161003     c                   end
027200161003      *
027300160926     c                   ENDSR
027400161003      * ?****************************************************************** */
027500161003      *  GENERA BOLLA DI RECUPERO SPESE DI DIROTTAMENTO al cliente
027600161003      * ?****************************************************************** */
027700161003     C     BOLLA_SEDE    BEGSR
027800161005      *
027900161014     c                   move      'N'           ha_scritto        1
028000161003      *
028100161025     c***   per non codificato esce dalla routine senza fare alcuna bolla
028200161025     c                   move      tasksc        fld8888           4
028300161025     c                   if        fld8888 ='8888'
028400161025     c                   leavesr
028500161025     c                   end
028600161025      *
028700161003     C* REPERISCE IL TERMINAL PARTENZA x l'area dati 34DS
028800161003     C                   CLEAR                   FNLV55DS
028900161003     C                   MOVEL     'P'           D55TPT
029000161003     C                   MOVEL     ecfLNA        D55LIN
029100161003     C                   Z-ADD     DATaCOR       D55DRF
029200161003     C                   CALL      'FNLV55R'
029300161003     C                   PARM                    FNLV55DS
029400161003IF  1C     D55ERR        IFNE      *BLANKS
029500161003     C                   MOVEL     ecfLNA        D55TFP
029600161003E   1C                   ENDIF
029700161003     C*
029800161003      ** imposta il SIMFEL come terminal di partenza
029900161003      *   Aggiorno area dati Dati Utente
030000161003     C     *lock         in        DDatiUte
030100161003     c                   movel     d55tfp        simfel
030200161003     C                   out       DDatiUte
030300161003     C                   UnLock(e) DDatiUte
030400161005      * ----------
030500161003     C                   clear                   DTASV
030600161003     C                   clear                   trul90ds
030700161003     c                   clear                   dblp
030800161003      *
030900161003     c                   eval      §lpnpr = ECFRIF
031000161003     c                   eval      §lpfpr='D'
031100161003     c                   move      'F*'          §lpcbo
031200161003     C                   MOVEL     '*'           §LPSOP
031300161003     C                   MOVEL     'G'           §LPRIC
031400161003      *  Provenienza
031500161003     c                   move      *all'9'       §lppoe
031600161003     c                   move      *all'9'       §lpnsr
031700161003     c                   move      *all'9'       §lpnor
031800161003     c                   move      *all'9'       §lpnrv
031900161003      *
032000161005     c                   clear                   dblp01
032100161003     c                   movel     *blank        §lptrd
032200161003     c                   movel     dblp01        §lpflo
032300161003      *
032400161003      *  Inverto mittente e destinatario
032500161003     C                   MOVEL     tasRSD        §LPRSM
032600161003     C                   MOVEL     tasIND        §LPINM
032700161003     C                   MOVEL     tasCAD        §LPCAM
032800161003     C                   MOVEL     tasLOD        §LPLOM
032900161003     C                   MOVEL     tasPRD        §LPPRM
033000161003     C                   MOVEL     tasNZD        §LPNZM
033100161003     C                   MOVEL     tasLNP        §LPLNA
033200161003     C                   MOVEL     tasLNA        §LPLNP
033300161003     C                   MOVE      tasCCM        §LPCCM
033400161003      **
033500161003     C                   MOVEL     §LPCCM        §LIN              3 0
033600161003     C                   MOVE      §LPCCM        Campo4            4 0
033700161003     C     §LIN          IFNE      §LPLNP
033800161003     C     Campo4        ANDEQ     8888
033900161003     C                   MOVEL     §LPLNP        §LPCCM
034000161003     C                   END
034100161003     C*
034200161003     c                   movel     taslna        §lpccn
034300161003     c                   move      9999          §lpccn
034400161003     C*
034500161003     C                   MOVEL     DATACOR       §LPAAS
034600161003     C                   MOVE      DATACOR       §LPMGS
034700161003     C                   MOVE      datacor       §LPDRT
034800161003     C*
034900161003     C                   MOVE      tasRMN        §LPRMN
035000161003     C                   MOVEL     'P'           §LPFPP
035100161003     c                   movel     tasLNA        autista           7 0
035200161003     c                   move      '0998'        autista
035300161003     c                   z-add     autista       §lppdr
035400161003     c     kapd          chain     fiapd01l
035500161003     c                   IF        not %Found(Fiapd01l)
035600161003      *   L'Autista
035700161003     c* il padroncino è da prendere da tb bsp: altrimenti imposto fisso 999
035800161003     c     tasLNA        chain     fibsp01l
035900161003     c                   if        %found(fibsp01l)
036000161003     c                   z-add     bspPDR        §lppdr
036100161003     c                   else
036200161003     c                   z-add     999           §lppdr
036300161003     c                   end
036400161006     c                   movel     §lplnp        §lppdr
036500161006     C*
036600161003     c                   endIF
036700161003     C*
036800161003     C                   MOVE      tasNCL        §LPNCL
036900161003     C                   MOVE      tasPKF        §LPPKB
037000161003     C                   MOVE      tasPKC        §LPPKC
037100161003     C                   MOVE      tasNCP        §LPNCP
037200161003     C                   MOVE      tasVLF        §LPVLB
037300161003     C                   MOVE      tasVLC        §LPVLC
037400161003     C                   MOVE      tasNCR        §LPNCR
037500161003      ***
037600161003     C                   MOVE      tasTSP        §LPTSP
037700161003      * forzatura h 10,30 x dirottamenti resi viene trasformato in 'E'
037800161003     c                   if        §lptsp = 'H'
037900161003     c                   move      'E'           §lptsp
038000161003     c                   endif
038100161003      *
038200161003     C* reperisco variabili tipo bolla
038300161003     C                   CLEAR                   DS3A
038400161003     C                   MOVEL     '3A'          TBLCOD
038500161003     C                   MOVEL     *BLANK        TBLKEY
038600161003     C                   MOVEL     §lpCBO        TBLKEY
038700161003     C     KTAB          CHAIN     TABEL00F                           21
038800161003     C  N21              MOVEL     TBLUNI        DS3A
038900161003      *
039000161003     C     tasFBR        IFNE      *BLANK
039100161003     C     §3asva        andeq     *blank
039200161003     C                   MOVE      tasFBR        §LPFBR
039300161003     C                   END
039400161003     C*  Se bolla originale in franco frontiera olla figlia no
039500161003     C     tasFBR        IFEQ      'F'
039600161003     C                   MOVEL     *BLANKS       §LPFBR
039700161003     C                   END
039800161003      **
039900161003     C                   MOVEL     §3ATB1        WNEWPO            1
040000161003     C     WNEWPO        IFEQ      'A'
040100161003     C     §3ATB1        ANDNE     'AS'
040200161003     C     Campo4        ANDEQ     8888
040300161003     C                   MOVE      '9999'        §LPCCM
040400161003     C                   MOVE      '9999'        §LPCCN
040500161003     C                   END
040600161003     C*
040700161003     C* SE CODICE MITTENTE = 8888/9999        PASSO 0 NEL CODICE
040800161003     C* TARIFFA ALTRIMENTI GLI PASSO IL CODICE DELLA BOLLA ORIG.
040900161003     C                   MOVE      §LPCCM        W8899             4 0
041000161003     C     W8899         IFne      8888
041100161003     C     W8899         ANDne     9999
041200161003     C                   MOVE      tasCTR        §LPCTR
041300161003     C                   END
041400161003      *
041500161003      *  Calcolo zona consegna
041600161003     C                   CLEAR                   TISI95ds
041700161003     C                   MOVEL     '3'           I95TCN
041800161003     C                   MOVEL     *BLANK        I95TLA
041900161003     C                   MOVEL     §LPNZD        I95NAR
042000161003     C                   MOVEL     §LPIND        I95IND
042100161003     C                   MOVEL     §LPCAD        I95CAP
042200161003     C                   MOVEL     §LPLOD        I95LOC
042300161003     C                   MOVEL     §LPPRD        I95PRV
042400161003     C                   MOVEL     tasPKF        I95LKG
042500161003     C                   MOVEL     tasVLF        I95LMC
042600161003     C                   MOVEL     tasFFD        I95FFD
042700161003     C                   MOVEL     tasTSP        I95TSP
042800161003     C                   MOVEL     'D'           I95fi1
042900161003     C                   MOVEL     tasFTC        I95fi2
043000161003     C                   MOVE      tasTC2        I95fi2
043100161003     C                   CALL      'TISI95R'
043200161003     C                   PARM                    TISI95ds
043300161003     C                   MOVEL     O95ZNC        §LPZNC
043400161003     C*
043500161003     C* FORZO CTM x eseguire un RECUPERO e deve essere forzatamente un "4F"
043600161003     C*  altrimenti la LNA non viene presa in considerazione dalla bollettazione
043700161003     C                   MOVEL     '4F'          §LPCTM
043800161003     C*
043900161005     C                   CLEAR                   TA4NOT
044000161003     C                   MOVE      'A'           §TRC              1
044100161014     C     Kta4          CHAIN(N)  tita430c                           43
044200161005     C                   IF        %FOUND(TITA430C)
044300161005     C                   MOVEL     ta4NOT        DTA4A
044400161005     C                   MOVEL     §TA4ARMA      §LPRMA
044500161005     C                   MOVEL     §TA4ANAS      §LPNAS
044600161005     c                   end
044700161005      *
044800161003     C                   MOVE      tasFVF        §LPFVB
044900161003     C*  Se il flag del volume bollettato è  'C' o 'K' oppure
045000161003     C*  è  'P' ed il cliente è codificato imposto 'R' in modo
045100161003     C*  che non venga più ricalcolato
045200161003     C     tasFVF        IFEQ      'C'
045300161003     C     tasFVF        OREQ      'K'
045400161003     C     tasFVF        OREQ      'P'
045500161003     C     Campo4        ANDEQ     8888
045600161003     C     tasFVF        OREQ      'P'
045700161003     C     Campo4        ANDEQ     9999
045800161003     C                   MOVEL     'R'           §LPFVB
045900161003     C                   END
046000161003     c*
046100161003     c                   move      tasksc        §lpksca
046200161003     c                   move      tasctr        §lpctra
046300161003     c                   z-add     tasaas        §LPaasM
046400161003     c                   z-add     taslnp        §LPlnpM
046500161003     c                   z-add     tasnrs        §LPnrsM
046600161003     c                   z-add     tasnsp        §LPnspM
046700161003     c*   NOTE
046800161003     C                   clear                   §LPNot
046900161003     C                   clear                   §LPNT2
047000161003     c                   exsr      oldnot
047100161003     C*
047200161003      *imposto i riferimenti EX-SPED solo se non è dirottamento
047300161003     C                   MOVEL     'SPED.:'      TESTO3
047400161003     C                   MOVE      tasAAS        RAAS
047500161003     C                   MOVE      tasLNP        RLNP
047600161003     C                   MOVE      tasNRS        RNRS
047700161003     C                   MOVE      tasNSP        RNSP
047800161003     C                   MOVEL     RIFSPE        §LPNT2
047900161005     C*
048000161005     C                   MOVE      'M'           §TRC              1
048100161014     c     kta4          chain     titaa30c
048200161005     c                   if        %Found(titaa30c)
048300161005     C                   MOVEL     TAARSC        §LPRSD
048400161005     C                   MOVEL     TAAIND        §LPIND
048500161005     C                   MOVEL     TAACAP        §LPCAD
048600161005     C                   MOVEL     TAALOC        §LPLOD
048700161005     C                   MOVEL     TAAPRV        §LPPRD
048800161005     C                   MOVEL     TAANAZ        §LPNZD
048900161005     c                   else
049000161005     c                   move      tasccm        i69kac
049100161005     c                   move      tasccm        i69kin
049200161005     C                   CALL      'TIBS69R'
049300161005     C                   PARM                    tibs69DS
049400161005     C                   PARM                    DS_cnaco
049500161005     C                   PARM                    DS_cnind
049600161005     C                   PARM                    DS_cnclp
049700161005     C                   PARM                    DS_fncls
049800161005      *
049900161005     C                   if        O69ERR <> '1'
050000161005     C                   MOVEL     acoRAG        §LPRSD
050100161005     C                   MOVEL     indVIA        §LPIND
050200161005     C                   MOVEL     indCAP        §LPCAD
050300161005     C                   MOVEL     indCIT        §LPLOD
050400161005     C                   MOVEL     indPRV        §LPPRD
050500161005     C                   MOVEL     indSTA        §LPNZD
050600161005     c                   end
050700161005     C*
050800161005     c                   end
050900161005     C*
051000161005     C                   MOVE      'O'           §TRC              1
051100161014     c     kta4          chain     titaa30c
051200161005     c                   if        %Found(titaa30c)
051300161005     C                   MOVEL     TAARSC        §LPRMO
051400161005     c                   end
051500161003      ******
051600170324     c                   eval      knmus = 'EDPCEDXXX'
051700170406     c                   eval      knsif = 'FILTRA201'
051800170324     C                   z-add     §LPLNP        ParLnpB
051900161003     C                   CALL      'FNLS01R'
052000161003     C                   PARM                    KPJBA
052100161003     C                   PARM                    DBLP
052200161003     C                   PARM                    DTASV
052300161003     C                   PARM                    trul90ds
052400170324     C                   PARM                    ParLnpB
052500161003      *
052600161003      *  Se non c'è riuscito a scrivere la bolla
052700161014     c                   if        §LPFPR <> '5'
052800161014     c                   move      'S'           ha_scritto
052900161003     c                   end
053000161003      *
053100161003     C                   ENDSR
053200161003      **?------------------------------------------------------------------ */
053300161003     C* O l d N o t - reperimento note da vecchia bolla
053400161003      **?------------------------------------------------------------------ */
053500161003     C     oldnot        BEGSR
053600161003      *
053700161003     c                   clear                   campo_70         70
053800161003     C                   MOVE      '8'           §TRC
053900161014     C     Kta4          CHAIN(N)  tita430c
054000161003     c                   if        %found(tita430c)
054100161003     c                   if        campo_70 <> *blank
054200161003     C                   eval      campo_70 = %trim(campo_70) + ' * ' +
054300161005     c                             %trim(TA4not)
054400161003     c                   else
054500161005     C                   eval      campo_70 = %trim(TA4not)
054600161003     c                   endif
054700161003     c                   endif
054800161003     C                   MOVE      '9'           §TRC
054900161014     C     Kta4          CHAIN(N)  tita430c
055000161003     c                   if        %found(tita430c)
055100161003     c                   if        campo_70 <> *blank
055200161003     C                   eval      campo_70 = %trim(campo_70) + ' * ' +
055300161005     c                             %trim(TA4not)
055400161003     c                   else
055500161005     C                   eval      campo_70 = %trim(TA4not)
055600161003     c                   endif
055700161003     c                   end
055800161003     C                   movel     campo_70      §lpNot
055900161003     C                   move      campo_70      §lpNt2
056000161003
056100161003     C                   ENDSR
056200161003     C*-----------------------------------------------------
