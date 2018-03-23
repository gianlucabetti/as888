000100150429     hDECEDIT('0,') DATEDIT(*YMD.)
000200000000      *****************************************************************
000300160930      *    crea recupero x DIROTTAMENTO fatto da CLEARING su solo ESTERO
000400000000      *****************************************************************
000500160930     FFNARB01L  IF   E           K DISK
000600161003     Ffibsp01l  iF   E           K DISK
000700161003     Ftabel00f  iF   E           K DISK
000800161003     FFiapd01L  IF   E           K DISK
000900170925     FfiPND01L  uF   E           K DISK
001000161014     FTita430C  uF   E           k DISK
001100161005     FTitaA30C  iF   E           k DISK
001200161005     FTitaS30C  iF   E           k DISK
001300161003      **?------------------------------------------------------------------ */
001400161003     D TISI95DS      E DS
001500161005     d TIBS69DS      E DS                  INZ
001600161005     d DS_cnaco      E DS                  extname(CNACO00F)
001700161005     d DS_cnind      E DS                  extname(CNIND00F)
001800161005     d DS_cnclp      E DS                  extname(CNCLP00F)
001900161005     d DS_fncls      E DS                  extname(FNCLS00F)
002000161005     D DTA4A         E DS
002100161003     D DTASV         E DS
002200161003     D Dblp          E DS
002300161003     D Dblp01        E DS
002400170324     d ParLnpB         s              3  0
002500161003     D trul90ds      E DS
002600161005     D DS3A          E DS
002700170925     D DpndFLO       E DS
002800161003      **?------------------------------------------------------------------ */
002900161003      *?  Parametri x Controllo profilo utenti  ?
003000161003      **?------------------------------------------------------------------ */
003100161003     d TIBS34ds      e ds
003200161003     d TIBS36ds      e ds
003300161003     d AZUTEds       e ds                  extname(AZUTE00F)
003400161003     d DDatiUte      e ds
003500161003     D                 DS
003600161003     D  TESTO3                 1      8
003700161003     D  RAAS                   9     10  0
003800161003     D  RLNP                  12     14  0
003900161003     D  RNRS                  16     17  0
004000161003     D  RNSP                  19     25  0
004100161003     D  RIFSPE                 1     25
004200161003     D*------------------
004300161003     D* DS REPEIMENTO TERMINALs
004400161003     D*------------------
004500161003     D FNLV55DS      E DS
004600161003      *---------------------
004700161003     D Wdataeur        S               D   DATFMT(*eur)
004800161003     D Wdataiso        S               D   DATFMT(*iso)
004900161003      *---------------------
005000160930      *
005100160930      *----------------------------------------------------*
005200030116     d KPJBA         E DS
005300160930     D savKPJBU        S                   like(KPJBU) inz
005400160927     D*
005500160926     D tnecf00f      e DS
005600160926     D tnefr00f      e DS
005700161130     D FIEU18        E DS                  EXTNAME(FIEU18DS)
005800160930     D FNARB00       E DS                  EXTNAME(FNARB00F)
005900160926     D*
006000151210     d Wdata8          DS
006100151210     d  dagiolav               1      8  0
006200151210     d  agiolav                9     16  0
006300151210     d  GioLav                17     21  0
006400160926      *
006500160926     D WrkSqlCmd       S           1024
006600151106      *****************************************************************
006700000000     D WLBDAT          DS
006800940926     D  G02DAT                 1      8  0
006900940926     D  G02INV                 9     16  0
007000940926     D  G02ERR                17     17
007100940926     D  G02TGI                18     22  0
007200160308      *
007300151106     D parDate         DS
007400151106     D   DaData                1      8  0
007500161003     D    aData                9     16  0
007600160923     D     Anno                9     12  0
007700160923     D     Mese               13     16  0
007800151106      *
007900151106     D DATA_eur        S               D   DATFMT(*eur)
008000151106     D DATA_iso        S               D   DATFMT(*iso)
008100161003      *---------------------------------------------------------------------*
008200161003     d Digits          c                   Const('0123456789')
008300161014      *---------------------------------------------------------------------*
008400000000     C     *ENTRY        PLIST
008500000000     C                   PARM                    KPJBA
008600160923     C                   eval      parDate = KPJBU
008700160930     C                   eval      savKPJBU= KPJBU
008800030120      *
008900160930     C     kbolla        kLIST
009000160930     C                   kfld                    ecfAAS
009100160930     C                   kfld                    ecfLNP
009200160930     C                   kfld                    ecfNRS
009300160930     C                   kfld                    ecfNSP
009400170925      *
009500170925     C     kPND          kLIST
009600170925     C                   kfld                    tasAAS
009700170925     C                   kfld                    tasLNP
009800170925     C                   kfld                    tasNRS
009900170925     C                   kfld                    tasNSP
010000161003     C*
010100161003     C     Kapd          KLIST
010200161003     C                   KFLD                    tipoaut
010300161003     C                   KFLD                    Autista
010400161003     C                   move      'A'           tipoaut           1
010500161003      *
010600161003     C     KtaB          KLIST
010700161003     C                   KFLD                    tblKUT
010800161003     C                   KFLD                    tblCOD
010900161003     C                   KFLD                    tblKEY
011000161003     C                   Z-ADD     1             tblKUT
011100161003     C*
011200161014     C     Kta4          KLIST
011300161003     C                   KFLD                    tasAAS
011400161003     C                   KFLD                    tasLNP
011500161003     C                   KFLD                    tasNRS
011600161003     C                   KFLD                    tasNSP
011700161003     C                   KFLD                    §TRC              1
011800161003      *
011900170406     c                   in(E)     *dtaara
012000161003     c                   IF        %ERROR or RSUT = *blanks
012100161003     c                   clear                   Tibs34Ds
012200161003     c                   call      'TIBS34R'
012300161003     c                   parm                    Tibs34Ds
012400161003     c                   in        *dtaara
012500161003     c                   ENDIF
012600161003      *
012700161003     c     *dtaara       define    §azute        azuteds
012800161003     c     *dtaara       define    §datiute      ddatiute
012900161003      *
013000161003     C                   CLEAR                   Tibs36Ds
013100161003     c                   EVAL      I36ute = 'EDPCED'
013200161003     c                   EVAL      I36Tla = 'L'
013300161003     C                   CALL      'TIBS36R'
013400161003     C                   PARM                    Tibs36Ds
013500161003      *
013600161003      * Recupero data e ora
013700161003     C                   TIME                    WORA              6 0
013800161003     C                   TIME                    W0140            14 0
013900161003      * UDATE IN GGMMAAAA
014000161003     C                   MOVE      W0140         oggi              8 0
014100161003      * UDATE IN AAAAMMGG
014200161003     C                   MOVEL     oggi          Wdataeur
014300161003     C                   MOVEL     Wdataeur      Wdataiso
014400161003     C                   MOVEL     Wdataiso      datacor           8 0
014500161003      *
014600030827     c                   movel     KNMUS         ALFA3             3
014700160930      *
014800160930      *--------------------------------------------------------------*
014900160926      *
015000160930      *   IMPOSTA SQL x lettura
015100160926     c                   exsr      istruz
015200160926      *
015300000000      *--------------------------------------------------------------*
015400160926      *
015500160926     C/EXEC SQL
015600160926     C+ PREPARE S1 FROM :WrkSqlCmd
015700160926     C/END-EXEC
015800160926      *
015900160926     C/EXEC SQL
016000160926     C+ DECLARE A1 CURSOR FOR S1
016100160926     C/END-EXEC
016200160926      *
016300160926     C/EXEC SQL
016400160926     C+ OPEN A1
016500160926     C/END-EXEC
016600160926      *
016700160926     C                   DOU       SqlCod <> 0
016800160926     C/EXEC SQL
016900160927     C+ FETCH NEXT FROM A1 INTO :efrRIF, :efrCAU, :efrDFT,
017000161003     C+       :ecfAAS, :ecfLNP, :ecfNRS, :ecfNSP, :ecfRIF, :ecfLNA
017100160926     C/END-EXEC
017200160926     C                   SELECT
017300160926     c*
017400160926     c* a fine file Totali x rotture
017500160926     C                   WHEN      SqlCod = 100
017600160926     c                   leave
017700160926     **
017800160926     C                   WHEN      SqlCod < 0
017900160926     C                   seton                                        H1
018000160926     c                   goto      fine
018100160926      *
018200160926     C                   OTHER
018300160926     c* DETTAGLIO
018400161014     c                   exsr      Elab_Bolla
018500160926     C                   ENDSL
018600160926      *
018700160926     C                   ENDDO
018800160926     C/EXEC SQL
018900160926     C+ CLOSE A1
019000160926     C/END-EXEC
019100160926     c     fine          tag
019200160930     C                   eval       KPJBU = savKPJBU
019300160926     C                   SETON                                        LR
019400160926     C**************************************************************************
019500160926     C* mi compongo l'istruzione sql
019600160926     C**************************************************************************
019700160926     C     istruz        BEGSR                                                  *
019800160926      **
019900160926     c* MI COMPONGO L'ISTRUZIONE SQL che legge il file TNECF
020000160926     C                   EVAL      WrkSqlCmd  =
020100160926     C                             'with ORIGINE as ( '                        +
020200160926     c                             ' SELECT efrRIF, efrCAU, efrDFT '           +
020300160926     c                             ' FROM tnefr00f WHERE efrntw=''DPD'' and '  +
020400160926     c                             ' efrrif like''084%'' and '                 +
020500160929     c                             ' substr(efrcau, 1, 1)=''D'' and '          +
020600160926     c                             ' efrdft between ' + %editc(daDATA:'X')     +
020700160926     c                             ' and ' + %editc(aDATA:'X')                 +
020800160926     c                             ' GROUP BY efrrif, EFRCAU, efrdft ),'       +
020900160926     C                             '     LEGAME  as ( '                        +
021000160926     c                             ' SELECT ecfAAS, ecfLNP, ecfNRS, ecfNSP, '  +
021100161003     c                             '  ecfRIF, ecfLNA FROM tnecf00f WHERE '     +
021200160926     c                             '  ecfgrp =''001'' and ecftbl=''F1'' and '  +
021300160926     c                             '  ecfntw=''DPD'' and ecfrif like ''084%'' '+
021400160926     c                             '  and ecfdfp between ' + %editc(daDATA:'X')+
021500160926     c                             ' and ' + %editc(aDATA:'X') + ')'           +
021600160926     c                             '  SELECT Origine.*, legame.*            '  +
021700160926     c                             '    FROM legame join origine            '  +
021800160926     c                             '      on ecfrif = efrrif                '  +
021900160926     c                             '      order by efrcau, ecfaas, ecflnp   '
022000160926     c                   ENDSR
022100160926      * ?------------------------------------------------------------------ */
022200160926      **
022300151113      * ?------------------------------------------------------------------ */
022400161014     c     Elab_Bolla    BEGSR
022500161003      *
022600161130     c                   clear                   FIEU18
022700160930     c                   clear                   fnarb00
022800161003      *
022900161003     c                   eval      i5PARCEL = ecfRIF
023000161003      *
023100160930     c     Kbolla        chain     FNARB01l
023200161003      *  deve allora cercare in SEDE
023300161003     c                   if           %Found(FNARB01L)
023400161003      **
023500161003      * Scrive una bolla di recupero su DIROTTAMENTI segnalati da DPD
023600161003      * che verranno poi addebitati sul clearing
023700161003     c                   call      'FIEU18R2'
023800161130     c                   parm                    FIEU18
023900161003     c                   parm                    fnarb00
024000161003     c                   parm                    KPJBA
024100161003      **
024200161003     c                   else
024300161003      **
024400161003      **  SE GIA' ESEGUITO IL TRASFERIMENTO va sulle BOLLE di SEDE
024500161003     c     Kbolla        chain     TITAS30C
024600161003     c                   if           %Found(TITAS30C)
024700170926      **
024800170925     C     KPND          CHAIN(N)  fiPND01l
024900170925     C                   IF        %FOUND(fiPND01l)
025000170925     c                   eval      dPNDFLO = pndFLO
025100170925      ** se NON ancora eseguito
025200170925     c                   if        §PNDRECDIR = *blank
025300170925      **
025400161003      * sostituisce i campi di ARB
025500161003     c                   eXSR      BOLLA_SEDE
025600161014      **
025700161014     c                   if          ha_scritto = 'S'
025800170925      **
025900170925     C     KPND          CHAIN     fiPND01l
026000170925     C                   IF        %FOUND(fiPND01l)
026100170925     c                   eval      dPNDFLO = pndFLO
026200170925     c                   eval      §PNDRECDIR = 'D'
026300170925     c                   eval      PNDFLO = DpndFLO
026400170925      ** aggiorna come eseguito  (per non riaddebitare in seguito)
026500170925     c                   update    fiPND000
026600170925     c                   end
026700170926      **
026800170925     c                   endif
026900170926      **
027000161014     c                   end
027100161014     c                   end
027200161003     c                   end
027300161003      *
027400161003     c                   end
027500161003      *
027600160926     c                   ENDSR
027700161003      * ?****************************************************************** */
027800161003      *  GENERA BOLLA DI RECUPERO SPESE DI DIROTTAMENTO al cliente
027900161003      * ?****************************************************************** */
028000161003     C     BOLLA_SEDE    BEGSR
028100161005      *
028200161014     c                   move      'N'           ha_scritto        1
028300161003      *
028400161025     c***   per non codificato esce dalla routine senza fare alcuna bolla
028500161025     c                   move      tasksc        fld8888           4
028600161025     c                   if        fld8888 ='8888'
028700161025     c                   leavesr
028800161025     c                   end
028900161025      *
029000161003     C* REPERISCE IL TERMINAL PARTENZA x l'area dati 34DS
029100161003     C                   CLEAR                   FNLV55DS
029200161003     C                   MOVEL     'P'           D55TPT
029300161003     C                   MOVEL     ecfLNA        D55LIN
029400161003     C                   Z-ADD     DATaCOR       D55DRF
029500161003     C                   CALL      'FNLV55R'
029600161003     C                   PARM                    FNLV55DS
029700161003IF  1C     D55ERR        IFNE      *BLANKS
029800161003     C                   MOVEL     ecfLNA        D55TFP
029900161003E   1C                   ENDIF
030000161003     C*
030100161003      ** imposta il SIMFEL come terminal di partenza
030200161003      *   Aggiorno area dati Dati Utente
030300161003     C     *lock         in        DDatiUte
030400161003     c                   movel     d55tfp        simfel
030500161003     C                   out       DDatiUte
030600161003     C                   UnLock(e) DDatiUte
030700161005      * ----------
030800161003     C                   clear                   DTASV
030900161003     C                   clear                   trul90ds
031000161003     c                   clear                   dblp
031100161003      *
031200161003     c                   eval      §lpnpr = ECFRIF
031300161003     c                   eval      §lpfpr='D'
031400161003     c                   move      'F*'          §lpcbo
031500161003     C                   MOVEL     '*'           §LPSOP
031600161003     C                   MOVEL     'G'           §LPRIC
031700161003      *  Provenienza
031800161003     c                   move      *all'9'       §lppoe
031900161003     c                   move      *all'9'       §lpnsr
032000161003     c                   move      *all'9'       §lpnor
032100161003     c                   move      *all'9'       §lpnrv
032200161003      *
032300161005     c                   clear                   dblp01
032400161003     c                   movel     *blank        §lptrd
032500161003     c                   movel     dblp01        §lpflo
032600161003      *
032700161003      *  Inverto mittente e destinatario
032800161003     C                   MOVEL     tasRSD        §LPRSM
032900161003     C                   MOVEL     tasIND        §LPINM
033000161003     C                   MOVEL     tasCAD        §LPCAM
033100161003     C                   MOVEL     tasLOD        §LPLOM
033200161003     C                   MOVEL     tasPRD        §LPPRM
033300161003     C                   MOVEL     tasNZD        §LPNZM
033400161003     C                   MOVEL     tasLNP        §LPLNA
033500161003     C                   MOVEL     tasLNA        §LPLNP
033600161003     C                   MOVE      tasCCM        §LPCCM
033700161003      **
033800161003     C                   MOVEL     §LPCCM        §LIN              3 0
033900161003     C                   MOVE      §LPCCM        Campo4            4 0
034000161003     C     §LIN          IFNE      §LPLNP
034100161003     C     Campo4        ANDEQ     8888
034200161003     C                   MOVEL     §LPLNP        §LPCCM
034300161003     C                   END
034400161003     C*
034500161003     c                   movel     taslna        §lpccn
034600161003     c                   move      9999          §lpccn
034700161003     C*
034800161003     C                   MOVEL     DATACOR       §LPAAS
034900161003     C                   MOVE      DATACOR       §LPMGS
035000161003     C                   MOVE      datacor       §LPDRT
035100161003     C*
035200161003     C                   MOVE      tasRMN        §LPRMN
035300161003     C                   MOVEL     'P'           §LPFPP
035400161003     c                   movel     tasLNA        autista           7 0
035500161003     c                   move      '0998'        autista
035600161003     c                   z-add     autista       §lppdr
035700161003     c     kapd          chain     fiapd01l
035800161003     c                   IF        not %Found(Fiapd01l)
035900161003      *   L'Autista
036000161003     c* il padroncino è da prendere da tb bsp: altrimenti imposto fisso 999
036100161003     c     tasLNA        chain     fibsp01l
036200161003     c                   if        %found(fibsp01l)
036300161003     c                   z-add     bspPDR        §lppdr
036400161003     c                   else
036500161003     c                   z-add     999           §lppdr
036600161003     c                   end
036700161006     c                   movel     §lplnp        §lppdr
036800161006     C*
036900161003     c                   endIF
037000161003     C*
037100161003     C                   MOVE      tasNCL        §LPNCL
037200161003     C                   MOVE      tasPKF        §LPPKB
037300161003     C                   MOVE      tasPKC        §LPPKC
037400161003     C                   MOVE      tasNCP        §LPNCP
037500161003     C                   MOVE      tasVLF        §LPVLB
037600161003     C                   MOVE      tasVLC        §LPVLC
037700161003     C                   MOVE      tasNCR        §LPNCR
037800161003      ***
037900161003     C                   MOVE      tasTSP        §LPTSP
038000161003      * forzatura h 10,30 x dirottamenti resi viene trasformato in 'E'
038100161003     c                   if        §lptsp = 'H'
038200161003     c                   move      'E'           §lptsp
038300161003     c                   endif
038400161003      *
038500161003     C* reperisco variabili tipo bolla
038600161003     C                   CLEAR                   DS3A
038700161003     C                   MOVEL     '3A'          TBLCOD
038800161003     C                   MOVEL     *BLANK        TBLKEY
038900161003     C                   MOVEL     §lpCBO        TBLKEY
039000161003     C     KTAB          CHAIN     TABEL00F                           21
039100161003     C  N21              MOVEL     TBLUNI        DS3A
039200161003      *
039300161003     C     tasFBR        IFNE      *BLANK
039400161003     C     §3asva        andeq     *blank
039500161003     C                   MOVE      tasFBR        §LPFBR
039600161003     C                   END
039700161003     C*  Se bolla originale in franco frontiera olla figlia no
039800161003     C     tasFBR        IFEQ      'F'
039900161003     C                   MOVEL     *BLANKS       §LPFBR
040000161003     C                   END
040100161003      **
040200161003     C                   MOVEL     §3ATB1        WNEWPO            1
040300161003     C     WNEWPO        IFEQ      'A'
040400161003     C     §3ATB1        ANDNE     'AS'
040500161003     C     Campo4        ANDEQ     8888
040600161003     C                   MOVE      '9999'        §LPCCM
040700161003     C                   MOVE      '9999'        §LPCCN
040800161003     C                   END
040900161003     C*
041000161003     C* SE CODICE MITTENTE = 8888/9999        PASSO 0 NEL CODICE
041100161003     C* TARIFFA ALTRIMENTI GLI PASSO IL CODICE DELLA BOLLA ORIG.
041200161003     C                   MOVE      §LPCCM        W8899             4 0
041300161003     C     W8899         IFne      8888
041400161003     C     W8899         ANDne     9999
041500161003     C                   MOVE      tasCTR        §LPCTR
041600161003     C                   END
041700161003      *
041800161003      *  Calcolo zona consegna
041900161003     C                   CLEAR                   TISI95ds
042000161003     C                   MOVEL     '3'           I95TCN
042100161003     C                   MOVEL     *BLANK        I95TLA
042200161003     C                   MOVEL     §LPNZD        I95NAR
042300161003     C                   MOVEL     §LPIND        I95IND
042400161003     C                   MOVEL     §LPCAD        I95CAP
042500161003     C                   MOVEL     §LPLOD        I95LOC
042600161003     C                   MOVEL     §LPPRD        I95PRV
042700161003     C                   MOVEL     tasPKF        I95LKG
042800161003     C                   MOVEL     tasVLF        I95LMC
042900161003     C                   MOVEL     tasFFD        I95FFD
043000161003     C                   MOVEL     tasTSP        I95TSP
043100161003     C                   MOVEL     'D'           I95fi1
043200161003     C                   MOVEL     tasFTC        I95fi2
043300161003     C                   MOVE      tasTC2        I95fi2
043400161003     C                   CALL      'TISI95R'
043500161003     C                   PARM                    TISI95ds
043600161003     C                   MOVEL     O95ZNC        §LPZNC
043700161003     C*
043800161003     C* FORZO CTM x eseguire un RECUPERO e deve essere forzatamente un "4F"
043900161003     C*  altrimenti la LNA non viene presa in considerazione dalla bollettazione
044000161003     C                   MOVEL     '4F'          §LPCTM
044100161003     C*
044200161005     C                   CLEAR                   TA4NOT
044300161003     C                   MOVE      'A'           §TRC              1
044400161014     C     Kta4          CHAIN(N)  tita430c                           43
044500161005     C                   IF        %FOUND(TITA430C)
044600161005     C                   MOVEL     ta4NOT        DTA4A
044700161005     C                   MOVEL     §TA4ARMA      §LPRMA
044800161005     C                   MOVEL     §TA4ANAS      §LPNAS
044900161005     c                   end
045000161005      *
045100161003     C                   MOVE      tasFVF        §LPFVB
045200161003     C*  Se il flag del volume bollettato è  'C' o 'K' oppure
045300161003     C*  è  'P' ed il cliente è codificato imposto 'R' in modo
045400161003     C*  che non venga più ricalcolato
045500161003     C     tasFVF        IFEQ      'C'
045600161003     C     tasFVF        OREQ      'K'
045700161003     C     tasFVF        OREQ      'P'
045800161003     C     Campo4        ANDEQ     8888
045900161003     C     tasFVF        OREQ      'P'
046000161003     C     Campo4        ANDEQ     9999
046100161003     C                   MOVEL     'R'           §LPFVB
046200161003     C                   END
046300161003     c*
046400161003     c                   move      tasksc        §lpksca
046500161003     c                   move      tasctr        §lpctra
046600161003     c                   z-add     tasaas        §LPaasM
046700161003     c                   z-add     taslnp        §LPlnpM
046800161003     c                   z-add     tasnrs        §LPnrsM
046900161003     c                   z-add     tasnsp        §LPnspM
047000161003     c*   NOTE
047100161003     C                   clear                   §LPNot
047200161003     C                   clear                   §LPNT2
047300161003     c                   exsr      oldnot
047400161003     C*
047500161003      *imposto i riferimenti EX-SPED solo se non è dirottamento
047600161003     C                   MOVEL     'SPED.:'      TESTO3
047700161003     C                   MOVE      tasAAS        RAAS
047800161003     C                   MOVE      tasLNP        RLNP
047900161003     C                   MOVE      tasNRS        RNRS
048000161003     C                   MOVE      tasNSP        RNSP
048100161003     C                   MOVEL     RIFSPE        §LPNT2
048200161005     C*
048300161005     C                   MOVE      'M'           §TRC              1
048400161014     c     kta4          chain     titaa30c
048500161005     c                   if        %Found(titaa30c)
048600161005     C                   MOVEL     TAARSC        §LPRSD
048700161005     C                   MOVEL     TAAIND        §LPIND
048800161005     C                   MOVEL     TAACAP        §LPCAD
048900161005     C                   MOVEL     TAALOC        §LPLOD
049000161005     C                   MOVEL     TAAPRV        §LPPRD
049100161005     C                   MOVEL     TAANAZ        §LPNZD
049200161005     c                   else
049300161005     c                   move      tasccm        i69kac
049400161005     c                   move      tasccm        i69kin
049500161005     C                   CALL      'TIBS69R'
049600161005     C                   PARM                    tibs69DS
049700161005     C                   PARM                    DS_cnaco
049800161005     C                   PARM                    DS_cnind
049900161005     C                   PARM                    DS_cnclp
050000161005     C                   PARM                    DS_fncls
050100161005      *
050200161005     C                   if        O69ERR <> '1'
050300161005     C                   MOVEL     acoRAG        §LPRSD
050400161005     C                   MOVEL     indVIA        §LPIND
050500161005     C                   MOVEL     indCAP        §LPCAD
050600161005     C                   MOVEL     indCIT        §LPLOD
050700161005     C                   MOVEL     indPRV        §LPPRD
050800161005     C                   MOVEL     indSTA        §LPNZD
050900161005     c                   end
051000161005     C*
051100161005     c                   end
051200161005     C*
051300161005     C                   MOVE      'O'           §TRC              1
051400161014     c     kta4          chain     titaa30c
051500161005     c                   if        %Found(titaa30c)
051600161005     C                   MOVEL     TAARSC        §LPRMO
051700161005     c                   end
051800161003      ******
051900170324     c                   eval      knmus = 'EDPCEDXXX'
052000170406     c                   eval      knsif = 'FILTRA201'
052100170324     C                   z-add     §LPLNP        ParLnpB
052200161003     C                   CALL      'FNLS01R'
052300161003     C                   PARM                    KPJBA
052400161003     C                   PARM                    DBLP
052500161003     C                   PARM                    DTASV
052600161003     C                   PARM                    trul90ds
052700170324     C                   PARM                    ParLnpB
052800161003      *
052900161003      *  Se non c'è riuscito a scrivere la bolla
053000161014     c                   if        §LPFPR <> '5'
053100161014     c                   move      'S'           ha_scritto
053200161003     c                   end
053300161003      *
053400161003     C                   ENDSR
053500161003      **?------------------------------------------------------------------ */
053600161003     C* O l d N o t - reperimento note da vecchia bolla
053700161003      **?------------------------------------------------------------------ */
053800161003     C     oldnot        BEGSR
053900161003      *
054000161003     c                   clear                   campo_70         70
054100161003     C                   MOVE      '8'           §TRC
054200161014     C     Kta4          CHAIN(N)  tita430c
054300161003     c                   if        %found(tita430c)
054400161003     c                   if        campo_70 <> *blank
054500161003     C                   eval      campo_70 = %trim(campo_70) + ' * ' +
054600161005     c                             %trim(TA4not)
054700161003     c                   else
054800161005     C                   eval      campo_70 = %trim(TA4not)
054900161003     c                   endif
055000161003     c                   endif
055100161003     C                   MOVE      '9'           §TRC
055200161014     C     Kta4          CHAIN(N)  tita430c
055300161003     c                   if        %found(tita430c)
055400161003     c                   if        campo_70 <> *blank
055500161003     C                   eval      campo_70 = %trim(campo_70) + ' * ' +
055600161005     c                             %trim(TA4not)
055700161003     c                   else
055800161005     C                   eval      campo_70 = %trim(TA4not)
055900161003     c                   endif
056000161003     c                   end
056100161003     C                   movel     campo_70      §lpNot
056200161003     C                   move      campo_70      §lpNt2
056300161003
056400161003     C                   ENDSR
056500161003     C*-----------------------------------------------------
