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
003300170927     d tibs55ds      E DS
003400161003     d AZUTEds       e ds                  extname(AZUTE00F)
003500161003     d DDatiUte      e ds
003600161003     D                 DS
003700161003     D  TESTO3                 1      8
003800161003     D  RAAS                   9     10  0
003900161003     D  RLNP                  12     14  0
004000161003     D  RNRS                  16     17  0
004100161003     D  RNSP                  19     25  0
004200161003     D  RIFSPE                 1     25
004300161003     D*------------------
004400161003     D* DS REPEIMENTO TERMINALs
004500161003     D*------------------
004600161003     D FNLV55DS      E DS
004700161003      *---------------------
004800161003     D Wdataeur        S               D   DATFMT(*eur)
004900161003     D Wdataiso        S               D   DATFMT(*iso)
005000161003      *---------------------
005100160930      *
005200160930      *----------------------------------------------------*
005300030116     d KPJBA         E DS
005400160930     D savKPJBU        S                   like(KPJBU) inz
005500160927     D*
005600160926     D tnecf00f      e DS
005700160926     D tnefr00f      e DS
005800161130     D FIEU18        E DS                  EXTNAME(FIEU18DS)
005900160930     D FNARB00       E DS                  EXTNAME(FNARB00F)
006000160926     D*
006100151210     d Wdata8          DS
006200151210     d  dagiolav               1      8  0
006300151210     d  agiolav                9     16  0
006400151210     d  GioLav                17     21  0
006500160926      *
006600160926     D WrkSqlCmd       S           1024
006700151106      *****************************************************************
006800000000     D WLBDAT          DS
006900940926     D  G02DAT                 1      8  0
007000940926     D  G02INV                 9     16  0
007100940926     D  G02ERR                17     17
007200940926     D  G02TGI                18     22  0
007300160308      *
007400151106     D parDate         DS
007500151106     D   DaData                1      8  0
007600161003     D    aData                9     16  0
007700160923     D     Anno                9     12  0
007800160923     D     Mese               13     16  0
007900151106      *
008000151106     D DATA_eur        S               D   DATFMT(*eur)
008100151106     D DATA_iso        S               D   DATFMT(*iso)
008200161003      *---------------------------------------------------------------------*
008300161003     d Digits          c                   Const('0123456789')
008400161014      *---------------------------------------------------------------------*
008500000000     C     *ENTRY        PLIST
008600000000     C                   PARM                    KPJBA
008700160923     C                   eval      parDate = KPJBU
008800160930     C                   eval      savKPJBU= KPJBU
008900030120      *
009000160930     C     kbolla        kLIST
009100160930     C                   kfld                    ecfAAS
009200160930     C                   kfld                    ecfLNP
009300160930     C                   kfld                    ecfNRS
009400160930     C                   kfld                    ecfNSP
009500170925      *
009600170925     C     kPND          kLIST
009700170925     C                   kfld                    tasAAS
009800170925     C                   kfld                    tasLNP
009900170925     C                   kfld                    tasNRS
010000170925     C                   kfld                    tasNSP
010100161003     C*
010200161003     C     Kapd          KLIST
010300161003     C                   KFLD                    tipoaut
010400161003     C                   KFLD                    Autista
010500161003     C                   move      'A'           tipoaut           1
010600161003      *
010700161003     C     KtaB          KLIST
010800161003     C                   KFLD                    tblKUT
010900161003     C                   KFLD                    tblCOD
011000161003     C                   KFLD                    tblKEY
011100161003     C                   Z-ADD     1             tblKUT
011200161003     C*
011300161014     C     Kta4          KLIST
011400161003     C                   KFLD                    tasAAS
011500161003     C                   KFLD                    tasLNP
011600161003     C                   KFLD                    tasNRS
011700161003     C                   KFLD                    tasNSP
011800161003     C                   KFLD                    §TRC              1
011900161003      *
012000170406     c                   in(E)     *dtaara
012100161003     c                   IF        %ERROR or RSUT = *blanks
012200161003     c                   clear                   Tibs34Ds
012300161003     c                   call      'TIBS34R'
012400161003     c                   parm                    Tibs34Ds
012500161003     c                   in        *dtaara
012600161003     c                   ENDIF
012700161003      *
012800161003     c     *dtaara       define    §azute        azuteds
012900161003     c     *dtaara       define    §datiute      ddatiute
013000161003      *
013100161003     C                   CLEAR                   Tibs36Ds
013200161003     c                   EVAL      I36ute = 'EDPCED'
013300161003     c                   EVAL      I36Tla = 'L'
013400161003     C                   CALL      'TIBS36R'
013500161003     C                   PARM                    Tibs36Ds
013600161003      *
013700161003      * Recupero data e ora
013800161003     C                   TIME                    WORA              6 0
013900161003     C                   TIME                    W0140            14 0
014000161003      * UDATE IN GGMMAAAA
014100161003     C                   MOVE      W0140         oggi              8 0
014200161003      * UDATE IN AAAAMMGG
014300161003     C                   MOVEL     oggi          Wdataeur
014400161003     C                   MOVEL     Wdataeur      Wdataiso
014500161003     C                   MOVEL     Wdataiso      datacor           8 0
014600161003      *
014700030827     c                   movel     KNMUS         ALFA3             3
014800160930      *
014900160930      *--------------------------------------------------------------*
015000160926      *
015100160930      *   IMPOSTA SQL x lettura
015200160926     c                   exsr      istruz
015300160926      *
015400000000      *--------------------------------------------------------------*
015500160926      *
015600160926     C/EXEC SQL
015700160926     C+ PREPARE S1 FROM :WrkSqlCmd
015800160926     C/END-EXEC
015900160926      *
016000160926     C/EXEC SQL
016100160926     C+ DECLARE A1 CURSOR FOR S1
016200160926     C/END-EXEC
016300160926      *
016400160926     C/EXEC SQL
016500160926     C+ OPEN A1
016600160926     C/END-EXEC
016700160926      *
016800160926     C                   DOU       SqlCod <> 0
016900160926     C/EXEC SQL
017000160927     C+ FETCH NEXT FROM A1 INTO :efrRIF, :efrCAU, :efrDFT,
017100161003     C+       :ecfAAS, :ecfLNP, :ecfNRS, :ecfNSP, :ecfRIF, :ecfLNA
017200160926     C/END-EXEC
017300160926     C                   SELECT
017400160926     c*
017500160926     c* a fine file Totali x rotture
017600160926     C                   WHEN      SqlCod = 100
017700160926     c                   leave
017800160926     **
017900160926     C                   WHEN      SqlCod < 0
018000160926     C                   seton                                        H1
018100160926     c                   goto      fine
018200160926      *
018300160926     C                   OTHER
018400160926     c* DETTAGLIO
018500161014     c                   exsr      Elab_Bolla
018600160926     C                   ENDSL
018700160926      *
018800160926     C                   ENDDO
018900160926     C/EXEC SQL
019000160926     C+ CLOSE A1
019100160926     C/END-EXEC
019200160926     c     fine          tag
019300160930     C                   eval       KPJBU = savKPJBU
019400160926     C                   SETON                                        LR
019500160926     C**************************************************************************
019600160926     C* mi compongo l'istruzione sql
019700160926     C**************************************************************************
019800160926     C     istruz        BEGSR                                                  *
019900160926      **
020000160926     c* MI COMPONGO L'ISTRUZIONE SQL che legge il file TNECF
020100160926     C                   EVAL      WrkSqlCmd  =
020200160926     C                             'with ORIGINE as ( '                        +
020300160926     c                             ' SELECT efrRIF, efrCAU, efrDFT '           +
020400160926     c                             ' FROM tnefr00f WHERE efrntw=''DPD'' and '  +
020500160926     c                             ' efrrif like''084%'' and '                 +
020600160929     c                             ' substr(efrcau, 1, 1)=''D'' and '          +
020700160926     c                             ' efrdft between ' + %editc(daDATA:'X')     +
020800160926     c                             ' and ' + %editc(aDATA:'X')                 +
020900160926     c                             ' GROUP BY efrrif, EFRCAU, efrdft ),'       +
021000160926     C                             '     LEGAME  as ( '                        +
021100160926     c                             ' SELECT ecfAAS, ecfLNP, ecfNRS, ecfNSP, '  +
021200161003     c                             '  ecfRIF, ecfLNA FROM tnecf00f WHERE '     +
021300160926     c                             '  ecfgrp =''001'' and ecftbl=''F1'' and '  +
021400160926     c                             '  ecfntw=''DPD'' and ecfrif like ''084%'' '+
021500160926     c                             '  and ecfdfp between ' + %editc(daDATA:'X')+
021600160926     c                             ' and ' + %editc(aDATA:'X') + ')'           +
021700160926     c                             '  SELECT Origine.*, legame.*            '  +
021800160926     c                             '    FROM legame join origine            '  +
021900160926     c                             '      on ecfrif = efrrif                '  +
022000160926     c                             '      order by efrcau, ecfaas, ecflnp   '
022100160926     c                   ENDSR
022200160926      * ?------------------------------------------------------------------ */
022300160926      **
022400151113      * ?------------------------------------------------------------------ */
022500161014     c     Elab_Bolla    BEGSR
022600161003      *
022700161130     c                   clear                   FIEU18
022800160930     c                   clear                   fnarb00
022900161003      *
023000161003     c                   eval      i5PARCEL = ecfRIF
023100161003      *
023200160930     c     Kbolla        chain     FNARB01l
023300161003      *  deve allora cercare in SEDE
023400161003     c                   if           %Found(FNARB01L)
023500161003      **
023600161003      * Scrive una bolla di recupero su DIROTTAMENTI segnalati da DPD
023700161003      * che verranno poi addebitati sul clearing
023800161003     c                   call      'FIEU18R2'
023900161130     c                   parm                    FIEU18
024000161003     c                   parm                    fnarb00
024100161003     c                   parm                    KPJBA
024200161003      **
024300161003     c                   else
024400161003      **
024500161003      **  SE GIA' ESEGUITO IL TRASFERIMENTO va sulle BOLLE di SEDE
024600161003     c     Kbolla        chain     TITAS30C
024700161003     c                   if           %Found(TITAS30C)
024800170926      **
024900170925     C     KPND          CHAIN(N)  fiPND01l
025000170925     C                   IF        %FOUND(fiPND01l)
025100170925     c                   eval      dPNDFLO = pndFLO
025200170925      ** se NON ancora eseguito
025300170925     c                   if        §PNDRECDIR = *blank
025400170925      **
025500161003      * sostituisce i campi di ARB
025600161003     c                   eXSR      BOLLA_SEDE
025700161014      **
025800161014     c                   if          ha_scritto = 'S'
025900170925      **
026000170925     C     KPND          CHAIN     fiPND01l
026100170925     C                   IF        %FOUND(fiPND01l)
026200170925     c                   eval      dPNDFLO = pndFLO
026300170925     c                   eval      §PNDRECDIR = 'D'
026400170925     c                   eval      PNDFLO = DpndFLO
026500170925      ** aggiorna come eseguito  (per non riaddebitare in seguito)
026600170925     c                   update    fiPND000
026700170925     c                   end
026800170926      **
026900170925     c                   endif
027000170926      **
027100161014     c                   end
027200161014     c                   end
027300161003     c                   end
027400161003      *
027500161003     c                   end
027600161003      *
027700160926     c                   ENDSR
027800161003      * ?****************************************************************** */
027900161003      *  GENERA BOLLA DI RECUPERO SPESE DI DIROTTAMENTO al cliente
028000161003      * ?****************************************************************** */
028100161003     C     BOLLA_SEDE    BEGSR
028200161005      *
028300161014     c                   move      'N'           ha_scritto        1
028400161003      *
028500161025     c***   per non codificato esce dalla routine senza fare alcuna bolla
028600161025     c                   move      tasksc        fld8888           4
028700161025     c                   if        fld8888 ='8888'
028800161025     c                   leavesr
028900161025     c                   end
029000161025      *
029100161003     C* REPERISCE IL TERMINAL PARTENZA x l'area dati 34DS
029200161003     C                   CLEAR                   FNLV55DS
029300161003     C                   MOVEL     'P'           D55TPT
029400161003     C                   MOVEL     ecfLNA        D55LIN
029500161003     C                   Z-ADD     DATaCOR       D55DRF
029600161003     C                   CALL      'FNLV55R'
029700161003     C                   PARM                    FNLV55DS
029800161003IF  1C     D55ERR        IFNE      *BLANKS
029900161003     C                   MOVEL     ecfLNA        D55TFP
030000161003E   1C                   ENDIF
030100161003     C*
030200161003      ** imposta il SIMFEL come terminal di partenza
030300161003      *   Aggiorno area dati Dati Utente
030400161003     C     *lock         in        DDatiUte
030500161003     c                   movel     d55tfp        simfel
030600161003     C                   out       DDatiUte
030700161003     C                   UnLock(e) DDatiUte
030800161005      * ----------
030900161003     C                   clear                   DTASV
031000161003     C                   clear                   trul90ds
031100161003     c                   clear                   dblp
031200161003      *
031300161003     c                   eval      §lpnpr = ECFRIF
031400161003     c                   eval      §lpfpr='D'
031500161003     c                   move      'F*'          §lpcbo
031600161003     C                   MOVEL     '*'           §LPSOP
031700161003     C                   MOVEL     'G'           §LPRIC
031800161003      *  Provenienza
031900161003     c                   move      *all'9'       §lppoe
032000161003     c                   move      *all'9'       §lpnsr
032100161003     c                   move      *all'9'       §lpnor
032200161003     c                   move      *all'9'       §lpnrv
032300161003      *
032400161005     c                   clear                   dblp01
032500161003     c                   movel     *blank        §lptrd
032600161003     c                   movel     dblp01        §lpflo
032700161003      *
032800161003      *  Inverto mittente e destinatario
032900161003     C                   MOVEL     tasRSD        §LPRSM
033000161003     C                   MOVEL     tasIND        §LPINM
033100161003     C                   MOVEL     tasCAD        §LPCAM
033200161003     C                   MOVEL     tasLOD        §LPLOM
033300161003     C                   MOVEL     tasPRD        §LPPRM
033400161003     C                   MOVEL     tasNZD        §LPNZM
033500161003     C                   MOVEL     tasLNP        §LPLNA
033600161003     C                   MOVEL     tasLNA        §LPLNP
033700161003     C                   MOVE      tasCCM        §LPCCM
033800161003      **
033900161003     C                   MOVEL     §LPCCM        §LIN              3 0
034000161003     C                   MOVE      §LPCCM        Campo4            4 0
034100161003     C     §LIN          IFNE      §LPLNP
034200161003     C     Campo4        ANDEQ     8888
034300161003     C                   MOVEL     §LPLNP        §LPCCM
034400161003     C                   END
034500161003     C*
034600161003     c                   movel     taslna        §lpccn
034700161003     c                   move      9999          §lpccn
034800161003     C*
034900161003     C                   MOVEL     DATACOR       §LPAAS
035000161003     C                   MOVE      DATACOR       §LPMGS
035100161003     C                   MOVE      datacor       §LPDRT
035200161003     C*
035300161003     C                   MOVE      tasRMN        §LPRMN
035400161003     C                   MOVEL     'P'           §LPFPP
035500161003     c                   movel     tasLNA        autista           7 0
035600161003     c                   move      '0998'        autista
035700161003     c                   z-add     autista       §lppdr
035800161003     c     kapd          chain     fiapd01l
035900161003     c                   IF        not %Found(Fiapd01l)
036000161003      *   L'Autista
036100161003     c* il padroncino è da prendere da tb bsp: altrimenti imposto fisso 999
036200161003     c     tasLNA        chain     fibsp01l
036300161003     c                   if        %found(fibsp01l)
036400161003     c                   z-add     bspPDR        §lppdr
036500161003     c                   else
036600161003     c                   z-add     999           §lppdr
036700161003     c                   end
036800161006     c                   movel     §lplnp        §lppdr
036900161006     C*
037000161003     c                   endIF
037100161003     C*
037200161003     C                   MOVE      tasNCL        §LPNCL
037300161003     C                   MOVE      tasPKF        §LPPKB
037400161003     C                   MOVE      tasPKC        §LPPKC
037500161003     C                   MOVE      tasNCP        §LPNCP
037600161003     C                   MOVE      tasVLF        §LPVLB
037700161003     C                   MOVE      tasVLC        §LPVLC
037800161003     C                   MOVE      tasNCR        §LPNCR
037900161003      ***
038000161003     C                   MOVE      tasTSP        §LPTSP
038100161003      * forzatura h 10,30 x dirottamenti resi viene trasformato in 'E'
038200161003     c                   if        §lptsp = 'H'
038300161003     c                   move      'E'           §lptsp
038400161003     c                   endif
038500161003      *
038600161003     C* reperisco variabili tipo bolla
038700161003     C                   CLEAR                   DS3A
038800161003     C                   MOVEL     '3A'          TBLCOD
038900161003     C                   MOVEL     *BLANK        TBLKEY
039000161003     C                   MOVEL     §lpCBO        TBLKEY
039100161003     C     KTAB          CHAIN     TABEL00F                           21
039200161003     C  N21              MOVEL     TBLUNI        DS3A
039300161003      *
039400161003     C     tasFBR        IFNE      *BLANK
039500161003     C     §3asva        andeq     *blank
039600161003     C                   MOVE      tasFBR        §LPFBR
039700161003     C                   END
039800161003     C*  Se bolla originale in franco frontiera olla figlia no
039900161003     C     tasFBR        IFEQ      'F'
040000161003     C                   MOVEL     *BLANKS       §LPFBR
040100161003     C                   END
040200161003      **
040300161003     C                   MOVEL     §3ATB1        WNEWPO            1
040400161003     C     WNEWPO        IFEQ      'A'
040500161003     C     §3ATB1        ANDNE     'AS'
040600161003     C     Campo4        ANDEQ     8888
040700161003     C                   MOVE      '9999'        §LPCCM
040800161003     C                   MOVE      '9999'        §LPCCN
040900161003     C                   END
041000161003     C*
041100161003     C* SE CODICE MITTENTE = 8888/9999        PASSO 0 NEL CODICE
041200161003     C* TARIFFA ALTRIMENTI GLI PASSO IL CODICE DELLA BOLLA ORIG.
041300161003     C                   MOVE      §LPCCM        W8899             4 0
041400161003     C     W8899         IFne      8888
041500161003     C     W8899         ANDne     9999
041600161003     C                   MOVE      tasCTR        §LPCTR
041700161003     C                   END
041800161003      *
041900161003      *  Calcolo zona consegna
042000161003     C                   CLEAR                   TISI95ds
042100161003     C                   MOVEL     '3'           I95TCN
042200161003     C                   MOVEL     *BLANK        I95TLA
042300161003     C                   MOVEL     §LPNZD        I95NAR
042400161003     C                   MOVEL     §LPIND        I95IND
042500161003     C                   MOVEL     §LPCAD        I95CAP
042600161003     C                   MOVEL     §LPLOD        I95LOC
042700161003     C                   MOVEL     §LPPRD        I95PRV
042800161003     C                   MOVEL     tasPKF        I95LKG
042900161003     C                   MOVEL     tasVLF        I95LMC
043000161003     C                   MOVEL     tasFFD        I95FFD
043100161003     C                   MOVEL     tasTSP        I95TSP
043200161003     C                   MOVEL     'D'           I95fi1
043300161003     C                   MOVEL     tasFTC        I95fi2
043400161003     C                   MOVE      tasTC2        I95fi2
043500161003     C                   CALL      'TISI95R'
043600161003     C                   PARM                    TISI95ds
043700161003     C                   MOVEL     O95ZNC        §LPZNC
043800161003     C*
043900161003     C* FORZO CTM x eseguire un RECUPERO e deve essere forzatamente un "4F"
044000161003     C*  altrimenti la LNA non viene presa in considerazione dalla bollettazione
044100161003     C                   MOVEL     '4F'          §LPCTM
044200161003     C*
044300161005     C                   CLEAR                   TA4NOT
044400161003     C                   MOVE      'A'           §TRC              1
044500161014     C     Kta4          CHAIN(N)  tita430c                           43
044600161005     C                   IF        %FOUND(TITA430C)
044700161005     C                   MOVEL     ta4NOT        DTA4A
044800161005     C                   MOVEL     §TA4ARMA      §LPRMA
044900161005     C                   MOVEL     §TA4ANAS      §LPNAS
045000161005     c                   end
045100161005      *
045200161003     C                   MOVE      tasFVF        §LPFVB
045300161003     C*  Se il flag del volume bollettato è  'C' o 'K' oppure
045400161003     C*  è  'P' ed il cliente è codificato imposto 'R' in modo
045500161003     C*  che non venga più ricalcolato
045600161003     C     tasFVF        IFEQ      'C'
045700161003     C     tasFVF        OREQ      'K'
045800161003     C     tasFVF        OREQ      'P'
045900161003     C     Campo4        ANDEQ     8888
046000161003     C     tasFVF        OREQ      'P'
046100161003     C     Campo4        ANDEQ     9999
046200161003     C                   MOVEL     'R'           §LPFVB
046300161003     C                   END
046400161003     c*
046500161003     c                   move      tasksc        §lpksca
046600161003     c                   move      tasctr        §lpctra
046700161003     c                   z-add     tasaas        §LPaasM
046800161003     c                   z-add     taslnp        §LPlnpM
046900161003     c                   z-add     tasnrs        §LPnrsM
047000161003     c                   z-add     tasnsp        §LPnspM
047100161003     c*   NOTE
047200161003     C                   clear                   §LPNot
047300161003     C                   clear                   §LPNT2
047400161003     c                   exsr      oldnot
047500161003     C*
047600161003      *imposto i riferimenti EX-SPED solo se non è dirottamento
047700161003     C                   MOVEL     'SPED.:'      TESTO3
047800161003     C                   MOVE      tasAAS        RAAS
047900161003     C                   MOVE      tasLNP        RLNP
048000161003     C                   MOVE      tasNRS        RNRS
048100161003     C                   MOVE      tasNSP        RNSP
048200161003     C                   MOVEL     RIFSPE        §LPNT2
048300161005     C*
048400161005     C                   MOVE      'M'           §TRC              1
048500161014     c     kta4          chain     titaa30c
048600161005     c                   if        %Found(titaa30c)
048700161005     C                   MOVEL     TAARSC        §LPRSD
048800161005     C                   MOVEL     TAAIND        §LPIND
048900161005     C                   MOVEL     TAACAP        §LPCAD
049000161005     C                   MOVEL     TAALOC        §LPLOD
049100161005     C                   MOVEL     TAAPRV        §LPPRD
049200161005     C                   MOVEL     TAANAZ        §LPNZD
049300161005     c                   else
049400161005     c                   move      tasccm        i69kac
049500161005     c                   move      tasccm        i69kin
049600161005     C                   CALL      'TIBS69R'
049700161005     C                   PARM                    tibs69DS
049800161005     C                   PARM                    DS_cnaco
049900161005     C                   PARM                    DS_cnind
050000161005     C                   PARM                    DS_cnclp
050100161005     C                   PARM                    DS_fncls
050200161005      *
050300161005     C                   if        O69ERR <> '1'
050400161005     C                   MOVEL     acoRAG        §LPRSD
050500161005     C                   MOVEL     indVIA        §LPIND
050600161005     C                   MOVEL     indCAP        §LPCAD
050700161005     C                   MOVEL     indCIT        §LPLOD
050800161005     C                   MOVEL     indPRV        §LPPRD
050900161005     C                   MOVEL     indSTA        §LPNZD
051000161005     c                   end
051100161005     C*
051200161005     c                   end
051300161005     C*
051400161005     C                   MOVE      'O'           §TRC              1
051500161014     c     kta4          chain     titaa30c
051600161005     c                   if        %Found(titaa30c)
051700161005     C                   MOVEL     TAARSC        §LPRMO
051800161005     c                   end
051900161003      ******
052000170324     c                   eval      knmus = 'EDPCEDXXX'
052100170927     c**********         eval      knsif = 'FILTRA201'
052200170927      *--
052300170927     C* Richiamo TIBS55R per conoscere S.Informativi di FILIALE
052400170927     C* in cui utilizzare il lavoro
052500170927     C                   CLEAR                   TIBS55ds
052600170927     C                   MOVEL     'L'           I50TLA
052700170927     C                   MOVEL     '046'         I50PPO
052800170927     C                   MOVEL     '001'         I50APO
052900170927     C                   CALL      'TIBS55R'
053000170927     C                   PARM                    TIBS55ds
053100170927     c                   eval      knsif = O50ASI
053200170927      *--
053300170324     C                   z-add     §LPLNP        ParLnpB
053400161003     C                   CALL      'FNLS01R'
053500161003     C                   PARM                    KPJBA
053600161003     C                   PARM                    DBLP
053700161003     C                   PARM                    DTASV
053800161003     C                   PARM                    trul90ds
053900170324     C                   PARM                    ParLnpB
054000161003      *
054100161003      *  Se non c'è riuscito a scrivere la bolla
054200161014     c                   if        §LPFPR <> '5'
054300161014     c                   move      'S'           ha_scritto
054400161003     c                   end
054500161003      *
054600161003     C                   ENDSR
054700161003      **?------------------------------------------------------------------ */
054800161003     C* O l d N o t - reperimento note da vecchia bolla
054900161003      **?------------------------------------------------------------------ */
055000161003     C     oldnot        BEGSR
055100161003      *
055200161003     c                   clear                   campo_70         70
055300161003     C                   MOVE      '8'           §TRC
055400161014     C     Kta4          CHAIN(N)  tita430c
055500161003     c                   if        %found(tita430c)
055600161003     c                   if        campo_70 <> *blank
055700161003     C                   eval      campo_70 = %trim(campo_70) + ' * ' +
055800161005     c                             %trim(TA4not)
055900161003     c                   else
056000161005     C                   eval      campo_70 = %trim(TA4not)
056100161003     c                   endif
056200161003     c                   endif
056300161003     C                   MOVE      '9'           §TRC
056400161014     C     Kta4          CHAIN(N)  tita430c
056500161003     c                   if        %found(tita430c)
056600161003     c                   if        campo_70 <> *blank
056700161003     C                   eval      campo_70 = %trim(campo_70) + ' * ' +
056800161005     c                             %trim(TA4not)
056900161003     c                   else
057000161005     C                   eval      campo_70 = %trim(TA4not)
057100161003     c                   endif
057200161003     c                   end
057300161003     C                   movel     campo_70      §lpNot
057400161003     C                   move      campo_70      §lpNt2
057500161003
057600161003     C                   ENDSR
057700161003     C*-----------------------------------------------------
