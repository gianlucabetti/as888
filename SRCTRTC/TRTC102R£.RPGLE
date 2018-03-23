000100041117     H DECEDIT('0,') DATEDIT(*YMD.)
000200000929      ***********************************************************************
000300041221      *   Il pgm serve per completare in Filiale  - IFTSTA - IFCSUM -       *
000400041221      *   da mandare e tradurre poi in SEDE       (GIRA IN FILIALE)         *
000500041117      ***********************************************************************
000600041117      *  ATTENZIONE :
000700041117      *    ogni messaggio tradotto ed elaborato ha 4 stati di avanzamenti
000800041117      *         blk = ricevuto da EDI e da elaborare da questo pgm
000900041117      *        'OK' = una volta elaborato il Partner è noto e tutto è OK
001000041223      *        'EF' = msg con Partner non riconosciuto su tab. PT in FILIALE
001100041117      *        'RX' = msg ricevuto ed elaborato in filiale con i pgm specifici
001200041223      *        'ER' = msg con Partner non riconosciuto su tab. PT
001300000929      ***********************************************************************
001400041220     FSNDIFTSTA1UF   E           k DISK    usropn rename(snddati:sndsta)        in lettura
001500041207     f                                     commit
001600041220     FSNDIFCSUM1UF   E           k DISK    usropn rename(snddati:sndsum)        in lettura
001700041207     f                                     commit
001800010115      *---------
001900050321     FEDTAB01L  iF   E           K DISK    usropn
002000050321     f******                               commit
002100010115      *---------
002200010123     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
002300041117      *---------------------------------------------------------------------*
002400041117      * Schiere
002500041117      *---------------------------------------------------------------------*
002600001002      * Tabella PT --> partner
002700001002     D CPT             S             35    DIM(100)
002800000125     D CPTparz         S             35    DIM(100)
002900960528     D DPT             S             90    DIM(100)
003000010123     D DPU             S             90    DIM(100)
003100010123      *---------------------------------------------------------------------*
003200010123      * Comandi
003300010123      *---------------------------------------------------------------------*
003400010123     D ERR             S             78    DIM(5) CTDATA PERRCD(1)
003500041125     D CMD             S             78    DIM(6) CTDATA PERRCD(1)
003600041203      *---------------------------------------------------------------------*
003700041203      * Schiere
003800041203      *---------------------------------------------------------------------*
003900041203      * x lancio azione di traduzione in filiale
004000041203     D LnpX            S              3  0 DIM(999) inz
004100041203     D HubX            S              3  0 DIM(999) inz
004200041203     D NumX            S              7  0 DIM(999) inz
004300000929      *---------------------------------------------------------------------*
004400001005      * Flag oprativi di work
004500001005      *---------------------------------------------------------------------*
004600001003     D WCOD            S             35
004700020904     D W035A           S             35
004800010123     D DesErr          S             78
004900010123     D WValor          S             35
005000010413     D WValo1          S             35
005100041117      *---------------------------------------------------------------------*
005200041117     D Sav_nome        S             35
005300041117     D sav_date        S              8  0 INZ(0)
005400041117     D sav_time        S              4  0 INZ(0)
005500041117     D Sav_ksc         s              7  0 INZ(0)
005600041117     D Sav_Lnp         s              3  0 INZ(0)
005700041125     D Sav_Nr785       s              7  0 INZ(0)
005800041117     D Sav_Nr784       s              7  0 INZ(0)
005900041117     D Sav_Nr781       s              7  0 INZ(0)
006000041125     D Prima_TST785    s              1  0 INZ(0)
006100041125     D Prima_TST784    s              1  0 INZ(0)
006200041117     D Prima_TST781    s              1  0 INZ(0)
006300071108     D NumDopo         s              3  0 INZ(0)
006400041117     D Num_Progr       s              5  0 INZ(0)
006500041125     D Sav_Docum       s              3    INZ('  ')
006600041117     D Partner_OK      S              2    INZ('  ')
006700041117     D Err_su_msg      S              2    INZ('  ')
006800041118     D CodMsgErr       S              2    INZ('  ')
006900041118     D Anno_04         s              4  0 INZ(0)
007000041222     D Num_Salta       s                   like(SUMNUM)
007100050221     D MSGd01          S              1    INZ(' ')
007200041129      *
007300041129     D data_iso        s               d   datfmt(*iso)
007400041129     D data_old        s               d   datfmt(*iso)
007500010115      *---------------------------------------------------------------------*
007600010319     D  MSGDS          S            100
007700010124     D  LENGH          S             15  5
007800001003     D X               s              3  0 INZ(0)
007900041203     D nrX             s              3  0 INZ(0)
008000041203     D svX             s              3  0 INZ(0)
008100000120      *---------------------------------------------------------------------*
008200000120      * DS
008300000120      *---------------------------------------------------------------------*
008400041125     D EDST00DS      E DS                  prefix(ST_)                          Sta
008500041125     D EDTT00DS      E DS                  prefix(SM_)                          Sum
008600041125      *
008700010123     D EDIDSPT       E DS
008800010123     D EDIDSPU       E DS
008900041221      *
009000050825     D EDIDSer       E DS
009100050825      *
009200041203      * DS per FNLV55R - Reperimento Terminal di Partenza e di Arrivo
009300041203     D FNLV55DS      E DS
009400041202      *
009500041202     D kpjba         E DS                  inz
009600041202      *
009700041202     D                SDS
009800041202     D  §PGMname               1     10
009900041202     D  §USRname             254    263
010000000120      *---------------------------------------------------------------------*
010100050824     D*PuliSND         DS            30
010200050824     d**DataPulizia                   8  0
010300050824     d**GG_Piu_old                    3  0
010400050824     d**Free_Field                   19
010500060116      * ?------------------------------------------------------------------ */
010600060116      * ?-  Parametri x controllo set di Caratteri con XCHKCHAR x CED INTESA
010700060116      * ?------------------------------------------------------------------ */
010800060116     D TxtInOut        S           2048
010900060116     D ElencoChar      S            256
011000060116     D TipoElenco      S              1
011100060116     D CharSost        S              1
011200060116     D UpperCase       S              1
011300060116     D ChkNull         S              1
011400060116     D CharNull        S              1
011500060116     D Esito           S              1
011600041129      *---------------------------------------------------------------------*
011700000120     D UTEDSE0F      E DS
011800000120     D  TCU                  398    697    DIM(50)                              Flg 8 tp.conto
011900000120     D  KCU                  698    847P 0 DIM(50)  PACKEVEN                    Capiconto
012000000929      *---------------------------------------------------------------------*
012100041221      *                    .. 1   4  : Numero da tab.MS di filiale
012200041221      *                    .. 5   8  : tipo record letto
012300041221     D DSdati          DS          1950
012400041221     D  SUMNUM                 1      4  0
012500041221     D  SUMTPR                 5      8
012600000929     D  SUMINR                 5      6
012700041117      *---------------------------------------------------------------------*
012800041117     D Digits          C                   CONST('0123456789')
012900060116      * ?------------------------------------------------------------------ */
013000060116      * ?controllo x CEDINTESA
013100060116     D SETA            C                   CONST('ABCDEFGHIJKLMNOPQRSTUVWXYZ012-
013200060116     D                                     3456789 .,-()/=''+:?!"%&*;<>')
013300060116      * ?------------------------------------------------------------------ */
013400060116      * ?set caratteri Maiuscoli/Minuscoli
013500060116     D Up              C                   CONST('ABCDEFGHIJKLMNOPQRSTUVWXYZ')
013600060116     D Lo              C                   CONST('abcdefghijklmnopqrstuvwxyz')
013700000120      *---------------------------------------------------------------------*
013800000120      * Ciclo principale
013900000120      *---------------------------------------------------------------------*
014000041125     c*  esegue un ciclo oppure l'altro a seconda del tipo di msg da trattare
014100041117     c                   clear                   Err_su_msg
014200041125     C*
014300041125     c                   select
014400041125     C*
014500041221     c                   when      tipfile= 'IFCSUM'
014600041125     c                   exsr      Ciclo_SUM
014700041125     C*
014800041221     c                   when      tipfile= 'IFTSTA'
014900041125     c                   exsr      Ciclo_STA
015000041125     C*
015100041125     c                   endsl
015200050321      *
015300050321      * ?------------------------------------------------------------------ */
015400050321      * ?Esegue il Commit prima di sottomettere le azioni
015500050321      * ? altrimenti potrebbero partire i lavori sottomessi prima
015600050321      * ?   di chiudere gli aggiornamenti su questo pgm
015700050321      * ?------------------------------------------------------------------ */
015800050321     C                   Commit
015900041221      *
016000041221      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
016100041221      *  Quindi lancia l'azione in ambiente di sede per ricevere i files
016200041221      *  da trasmettere dopo averli tradotti in EDI
016300041221     c                   do        99            svX
016400041221     c                   if        lnpX(svX) = 0
016500041221     c                   leave
016600051110     c                   end
016700041221     c                   move      LnpX(svX)     Lnp_alf           3
016800041221     c                   move      HubX(svX)     Hub_alf           3
016900041221     c                   move      NumX(svX)     Num_alf           7
017000041221     c                   clear                   kpjbu
017100060117     c                   movel     'TCC8'        kcoaz
017200051117      *******
017300041221     c                   if        flgtst = ' '
017400041221     c                   movel     'GAITRA201 '  knsif
017500041221     c                   eval      kqebi = 'KEDI'
017600041221     c                   eval      knmus = 'EDPCED'
017700041221     c                   eval      knmtd = 'EDPCED'
017800041221     c                   else
017900041221     c                   movel     'GAITRAPSP '  knsif
018000041221     c                   eval      kqebi = 'KEDI'
018100041221     c                   eval      knmus = 'EDPCED'
018200041221     c                   eval      knmtd = 'EDPCED'
018300041221     c                   end
018400041221     c                   movel     'QPRINTS   '  kqopr
018500050126     c                   eval      %subst(kpjbu:227:10) = tipfile
018600050126     c                   eval      %subst(kpjbu:237:3) = codfile
018700041222     c                   eval      %subst(kpjbu:240:7) = Num_alf
018800041221     c                   eval      %subst(kpjbu:247:1) = 'M'
018900041221     c                   eval      %subst(kpjbu:248:3) = Lnp_alf
019000041221     c                   eval      %subst(kpjbu:251:3) = '046'
019100041221     c                   eval      %subst(kpjbu:254:3) = Hub_alf
019200041221      *
019300041221      *  x provare la procedura mettevo in Hold il lavoro
019400041221     c*****              eval      KSTEB = 'H'
019500041221     c*****              eval      KSTJO = 'H'
019600041221     c                   call      'BCH10'
019700041221     c                   parm                    kpjba
019800041221     c                   endDO
019900041221      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
020000041125      *
020100041129      * Prima di uscire esegue una pulizia dei vecchi records
020200041129      *     + vecchi di xx giorni
020300041129      **              *--------------------------*
020400050824     c********           exsr      Pulizia_old
020500041129      **              *--------------------------*
020600050321      *
020700050321      * ?------------------------------------------------------------------ */
020800050321      * ?Dopo la pulizia riesegue il COMMIT
020900050321      * ?------------------------------------------------------------------ */
021000050321     C                   Commit
021100041129      *
021200041203     C                   CLEAR                   FNLV55DS
021300041203     C                   MOVEL     'C'           D55TLA
021400041203     C                   CALL      'FNLV55R'
021500041203     C                   PARM                    FNLV55DS
021600041207      *
021700041125     C                   SETON                                        LR
021800050321 ?    ***********************************************************************/
021900041125      *  Ciclo per la codifica degli Stati
022000041125      *---------------------------------------------------------------------*
022100041125     C     Ciclo_STA     Begsr
022200041125      *
022300041117      * Lettura iniziale sul file di tutti i records allo stato "BLK"
022400041125      *
022500041220     C     Ksnd          setll     sndiftsta1
022600041220     C     Ksnd          reade     sndiftsta1
022700000929      *
022800010123      * Ciclo di Lettura fino a EOF
022900041220     C                   DOW       not %EOF(sndiftsta1)
023000001002      *
023100041117      *  muove i dati flat su una DS
023200001002      *                          ++++++++++++++++++++++++
023300041221     C                   MOVEL     ssIFTSTA      DSdati
023400041117      * -----------              ++++++++++++++++++++++++
023500041117      * Testata MSG
023600041117      * -----------
023700041117     C                   If        SUMTPR = 'ST00'                              TESTATA
023800041117      **              *--------------------------*
023900041117     C                   EXSR      DATI_Testata
024000041117      **              *--------------------------*
024100000929     C                   EndIF
024200041117      **
024300041117      **   deve aggiornare i records solo dopo aver esaminato la testata
024400041222     c                   if        sumnum <> num_salta
024500041117      **              *--------------------------*
024600041117     c                   exsr      AGG_record
024700041117      **              *--------------------------*
024800041222     c                   end
024900010123      *      -----------
025000960329      * Lettura successiva
025100041220     C     Ksnd          reade     sndiftsta1
025200010123     C                   ENDdo
025300041117     C*------------------
025400041117      *  se ci sono stati dei problemi con dei messaggi
025500041117     c                   if        Err_su_msg = 'SI'
025600041117      **              *--------------------------*
025700041125     c                   exsr      snd_msg_sta
025800041117      **              *--------------------------*
025900041117     c                   end
026000041125      *
026100041125     C                   Endsr
026200041125      *---------------------------------------------------------------------*
026300041125      *  Ciclo per la codifica delle Bolle
026400041125      *---------------------------------------------------------------------*
026500041125     C     Ciclo_SUM     Begsr
026600041125      *
026700041125      * Lettura iniziale sul file di tutti i records allo stato "BLK"
026800041125      *
026900041220     C     Ksnd          setll     sndifcsum1
027000041220     C     Ksnd          reade     sndifcsum1
027100041125      *
027200041125      * Ciclo di Lettura fino a EOF
027300041220     C                   DOW       not %EOF(sndifcsum1)
027400041125      *  muove i dati flat su una DS
027500041125      *                          ++++++++++++++++++++++++
027600041221     C                   MOVEL     ssIFCSUM      DSdati
027700041125      * -----------              ++++++++++++++++++++++++
027800041125      * Testata MSG
027900041125      * -----------
028000041125     C                   If        SUMTPR = 'TT00'                              TESTATA
028100041125      **              *--------------------------*
028200041125     C                   EXSR      DATI_Testata
028300041203      **              *--------------------------*
028400041125     C                   EndIF
028500041125      **
028600041125      **   deve aggiornare i records solo dopo aver esaminato la testata
028700041222     c                   if        sumnum <> num_salta
028800041125      **              *--------------------------*
028900041125     c                   exsr      AGG_record
029000041125      **              *--------------------------*
029100041222     c                   end
029200041125      *      -----------
029300041125      * Lettura successiva
029400041220     C     Ksnd          reade     sndifcsum1
029500041125     C                   ENDdo
029600041125     C*------------------
029700041202      *
029800041125      *  se ci sono stati dei problemi con dei messaggi
029900041125     c                   if        Err_su_msg = 'SI'
030000041125      **              *--------------------------*
030100041125     c                   exsr      snd_msg_sum
030200041125      **              *--------------------------*
030300041125     c                   end
030400041125      *
030500041125     C                   Endsr
030600010115     C*-------------------------------------------------------------------------------------
030700010123     C*?    Controlla la testata a quale Filiale appartiene il Partner
030800010115     C*-------------------------------------------------------------------------------------
030900041117     C     DATI_Testata  BEGSR
031000041117      *
031100010115     C* inizializzo i campi di salvataggio codici
031200041117     c                   clear                   num_progr
031300050207     c                   if        Inz_Progr <> *blank
031400050207     c                   add       Da_Progr      num_progr
031500060116      **
031600060116      **  Solo x IFCSUM di Pracht
031700060116      * x Pracht deve impostare 4 msg
031800060116     c                   if        tipfile= 'IFCSUM'
031900060116      * secondo msg
032000060116     c                   if        Da_Progr  = 5000
032100060116     c                   add       1             sumnum
032200071108     c                   z-add     1             numdopo
032300060116     c                   end
032400060116      * terzo   msg
032500060116     c                   if        Da_Progr  = 10000
032600060116     c                   add       2             sumnum
032700071108     c                   z-add     2             numdopo
032800060116     c                   end
032900060116      * quarto  msg
033000060116     c                   if        Da_Progr  = 15000
033100060116     c                   add       3             sumnum
033200071108     c                   z-add     3             numdopo
033300060116     c                   end
033400060116     c                   end
033500060116      **
033600050207     c                   end
033700060116      **
033800041117     c                   clear                   Sav_Docum
033900041117     C                   clear                   sav_Ksc
034000041117     C                   clear                   sav_Lnp
034100041117     C                   clear                   sav_nome
034200041117     C                   clear                   sav_date
034300041117     C                   clear                   sav_time
034400041117      *
034500041117     C                   clear                   Partner_OK
034600041117     C                   clear                   WCod
034700010115     C                   clear                   EDIDSPT
034800010123     C                   clear                   EDIDSPU
034900010115     C                   clear                   EDST00DS
035000041125     C                   clear                   EDTT00DS
035100010115     C*
035200010123      *  Imposta la DS di controllo del record TESTATA << ST00 >>
035300041203      *    Status
035400041221     c                   if        tipfile= 'IFTSTA'
035500041221     C                   MOVEL     ssIFTSTA      EDST00DS
035600041117      * dati da riportare sul file x identificare il messaggio
035700041125     c                   move      St_TA1001     Sav_Docum
035800041125     C                   MOVE      St_TA1004     sav_nome
035900041125     C                   MOVE      St_TA0017     sav_date
036000041125     C                   MOVE      St_TA0019     sav_time
036100010115      * Primo tentativo rep. da codice + qualificatore
036200041221     C                   MOVEL     St_TA0010     WCOD
036300041221     C                   MOVE      St_TAA007     WCOD
036400041203     c                   end
036500041203      *    Bolle
036600041221     c                   if        tipfile= 'IFCSUM'
036700041221     C                   MOVEL     ssIFCSUM      EDTT00DS
036800041125      * dati da riportare sul file x identificare il messaggio
036900041125     c                   move      Sm_TA1001     Sav_Docum
037000041125     C                   MOVE      Sm_TA1004     sav_nome
037100041125     C                   MOVE      Sm_TA0017     sav_date
037200041125     C                   MOVE      Sm_TA0019     sav_time
037300041125      * Primo tentativo rep. da codice + qualificatore
037400041221     C                   MOVEL     Sm_TA0010     WCOD
037500041221     C                   MOVE      Sm_TAA007     WCOD
037600041125     c                   end
037700041203      *
037800041222      * _______________________________________________________________________
037900041222      * attenzione se non è il tipo di documento da tradurre deve saltare tutte
038000041222      *  le righe del documento NUMERATO.
038100041222     c                   z-add     0             num_salta
038200041222     c                   if        sav_Docum <> codfile
038300041222     c                   z-add     sumNUM        num_salta
038400041222     c                   goto      end_routine
038500041222     c                   end
038600041222      * _______________________________________________________________________
038700041222      *
038800041125     C                   MOVEL     '20'          sav_date
038900041125      *
039000010115     C                   Z-ADD     1             X
039100010115     C     WCOD          LOOKUP    CPT(X)                                 33
039200010123      *  Prova a cercarlo con chiave parziale
039300010115     C                   If        *IN33 = *off
039400010123     C                   eval      X = 1
039500041221     c                   if        tipfile= 'IFTSTA'
039600041221     C                   eval      W035A = %subst(St_TA0010:1:31)
039700041203     c                   end
039800041221     c                   if        tipfile= 'IFCSUM'
039900041221     C                   eval      W035A = %subst(Sm_TA0010:1:31)
040000041125     c                   end
040100010115     C     W035A         lookup    CPTparz(X)                             33
040200010115     C                   Endif
040300041115      *
040400010115      *  Salva dalla tabella "PT" il cliente solo se è stata presa direttamente
040500010115      *   o in modo parziale.
040600010115     C                   If        *IN33 = *on
040700041117     C                   move      'OK'          Partner_OK                     tutto OK
040800010115     C                   MOVEL     DPT(X)        EDIDSPT
040900010123     C                   MOVEL     DPU(X)        EDIDSPU
041000041117     C                   MOVEL     §PTLNP        sav_Lnp
041100041117     C                   z-add     §PTKSC        sav_ksc
041200010123      *
041300010123     C                   Else
041400041117      *
041500041115      * se lo trova deve segnalare l'errore x Problemi di reperimento del Partner
041600041223     C                   move      'EF'          Partner_OK                     errore
041700041117     c                   move      'SI'          Err_su_msg
041800041117      *
041900041117      *  Problemi di reperimento del Partner
042000010123     C                   movel     ERR(1)        DesERR
042100010123     C                   movel     Wcod          WValor
042200010413     C                   clear                   WValo1
042300041221     C                   move      '02'          codmsgerr
042400041117      **              *--------------------------*
042500010123     C                   Exsr      STPERR
042600041117      **              *--------------------------*
042700010115     C                   Endif
042800041203      **           - - - - - - - - - - - - - - - - - - - - - - - -
042900041221      **   Comunque imposta il numeratore di filiale del messaggio
043000041221      **    con cui numererà tutti i records.
043100041221     c                   IF        Sav_Docum = '781'
043200041221     c                   z-add     sumNUM        Sav_nr781
043300041221     C                   Endif
043400041221     c                   IF        Sav_Docum = '784'
043500041221     c                   z-add     sumNUM        Sav_nr784
043600041221     C                   Endif
043700041221     c                   IF        Sav_Docum = '785'
043800041221     c                   z-add     sumNUM        Sav_nr785
043900041221     C                   Endif
044000041221      **           - - - - - - - - - - - - - - - - - - - - - - - -
044100041203      * imposta la LNP e l'HUB di partenza su schiera
044200041221      * per gestire in seguito il lancio delle azioni di ricezione in sede.
044300041203     C                   clear                   FNLV55DS
044400041203     C                   movel     'P'           D55tpt
044500041203     C                   move      sav_lnp       D55lin
044600041203     C                   move      *date         d55drf
044700041203     C                   CALL      'FNLV55R'
044800041203     C                   parm                    FNLV55DS
044900041203     c                   clear                   HubP              3 0
045000041203     c                   if        d55err = *blanks
045100041203     c                   movel     d55tfp        HubP
045200041203     c                   end
045300041203     c                   z-add     1             nrx
045400041203     c     sav_lnp       lookup    lnpX(nrX)                              34
045500041203     c                   if        *in34 = *Off
045600041203     c     *zeros        lookup    lnpX(nrX)                              34
045700041203     c                   end
045800041203     c                   z-add     sav_lnp       lnpX(nrX)
045900041203     c                   z-add     HubP          HubX(nrX)
046000041222     c                   if        SUMNUM > NumX(nrX)
046100041222     c                   z-add     SUMNUM        NumX(nrX)
046200041222     c                   end
046300041203      **           - - - - - - - - - - - - - - - - - - - - - - - -
046400041125      *
046500041118      *  per differenziare i messaggi temporalmente
046600041118     C                   TIME                    ST00_ora          6 0
046700041118      *
046800041222     C     End_Routine   Endsr
046900010123     C*----------------------------------------------------------------
047000041117     C*?  Deve Aggiornare il messaggio appena letto se OK oppure ER
047100010123     C*----------------------------------------------------------------
047200041117     C     AGG_record    BEGSR
047300041117      *              *-------------------------------*
047400041117      *  Aggiorna i campi gestione Bartolini
047500041117      *
047600041221     C                   eval      ssELABORA = Partner_OK
047700041221     C                   eval      ssNOMEDOC = Sav_Nome
047800041221     C                   eval      ssDATA    = Sav_Date
047900041221     C     100           mult      Sav_Time      ssORA
048000071108     C                   add       numDopo       ssORA
048100041221     C                   eval      ssLINEA   = Sav_Lnp
048200041221     C                   eval      ssDATELA  = *date
048300041221     C                   eval      ssORAELA  = ST00_ora
048400041125      *
048500041221     c                   if        tipfile= 'IFTSTA'
048600041125     c                   if        Sav_Docum = '784'
048700041221     C                   eval      ssNUMERO  = Sav_nr784
048800041221     C                   eval      ssTIPO    = '784'
048900041125     c                   else
049000041221     C                   eval      ssNUMERO  = Sav_nr781
049100041221     C                   eval      ssTIPO    = '781'
049200041125     c                   end
049300041125     c                   end
049400041117      *
049500041221     c                   if        tipfile= 'IFCSUM'
049600041125     c                   if        Sav_Docum = '785'
049700041221     C                   eval      ssNUMERO  = Sav_nr785
049800060116      **
049900060116      *  reimposta il numero nelle prime 4 posizioni --> vedi Pracht
050000060116     c                   if        da_progr > 0
050100060116     c                   move      ssNUMERO      numero_04         4
050200060116     c                   movel     numero_04     ssIFCSUM
050300060116     c                   end
050400060116      **
050500041221     C                   eval      ssTIPO    = '785'
050600041117     c                   end
050700041125     c                   end
050800041117      *
050900041117     C                   eval      num_progr = num_progr + 1
051000041221     C                   eval      ssPROGR   = num_progr
051100041125      *
051200060116      * ?--------------------------------------------------------------------
051300060116      *   Esegue controllo formale sui caratteri del Flat File da inviare
051400060116      *     il Set di Caratteri ammessi è: (SETA) solo Maiuscoli
051500060116      * ?  ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789 .,-()/='+:?!"%&*;<>
051600060116      * ?--------------------------------------------------------------------
051700060116      * ?  prima converte tutto in Caratteri Maiuscoli
051800060116     c                   if        tipfile= 'IFTSTA'
051900060116     c                   movel     SSiftsta      sta1950        1950
052000060116     C     Lo:Up         XLATE     sta1950       STA1950Up      1950
052100060116     c                   movel     STA1950Up     SSiftsta
052200060116     c                   end
052300060116     c                   if        tipfile= 'IFCSUM'
052400060116     c                   movel     SSifcsum      sum1950        1950
052500060116     C     Lo:Up         XLATE     sum1950       SUM1950Up      1950
052600060116     c                   movel     SUM1950Up     SSifcsum
052700060116     c                   end
052800060116      *
052900060116      * ?  poi esegue il controllo su caratteri non ammessi
053000060116     c                   if        tipfile= 'IFTSTA'
053100060116     c                   movel     SSiftSTA      TxtInOut
053200060116     c                   end
053300060116     c                   if        tipfile= 'IFCSUM'
053400060116     c                   movel     SSifcSUM      TxtInOut
053500060116     c                   end
053600060116     c                   eval      ElencoChar = SETA
053700060116     c                   eval      TipoElenco = *blank
053800060116     c                   eval      CharSost   = *blank
053900060116     c                   eval      UpperCase  = '1'
054000060116     c                   eval      ChkNull    = *blank
054100060116     c                   eval      CharNull   = *blank
054200060116     c                   eval      Esito      = *blank
054300060116      *
054400060116     c                   call      'XCHKCHAR'
054500060116     c                   parm                    TxtInOut
054600060116     c                   parm                    ElencoChar
054700060116     c                   parm                    TipoElenco
054800060116     c                   parm                    CharSost
054900060116     c                   parm                    UpperCase
055000060116     c                   parm                    ChkNull
055100060116     c                   parm                    CharNull
055200060116     c                   parm                    Esito
055300060116      *
055400060116      *   se è stato modificato qualcosa nella riga di FLAT
055500060116     c                   if        Esito = '1'
055600060116     c                   if        tipfile= 'IFTSTA'
055700060116     c                   movel     TxtInOut      SSiftSTA
055800060116     c                   end
055900060116     c                   if        tipfile= 'IFCSUM'
056000060116     c                   movel     TxtInOut      SSifcSUM
056100060116     c                   end
056200060116     c                   end
056300060116      *
056400041125      *              *-------------------------------*
056500041221     c                   if        tipfile= 'IFTSTA'
056600041220     C                   UPDATE    sndsta
056700041125     c                   end
056800041221     c                   if        tipfile= 'IFCSUM'
056900041220     C                   UPDATE    sndsum
057000041125     c                   end
057100041117      *              *-------------------------------*
057200041125      *
057300041117     C                   Endsr
057400041117     C*----------------------------------------------------------------
057500041117     C*? STPERR: STAMPA ERRORE
057600041117     C*----------------------------------------------------------------
057700041117     C     STPERR        BEGSR
057800041117     C*
057900010123     C     *INOF         IFEQ      '1'
058000041125     C*
058100041221     c                   if        tipfile='IFTSTA'
058200041125     C                   EXCEPT    TESSTA
058300041125     c                   else
058400041125     C                   EXCEPT    TESSUM
058500041125     c                   end
058600041125     C*
058700010123     C                   EXCEPT    ERRCLI
058800010123     C                   END
058900041125     C*
059000010123     C                   EXCEPT    DETER
059100010413     C*
059200010413     C     Wvalo1        IFne      *blank
059300010413     C                   EXCEPT    MAXDET
059400010413     C                   End
059500010123     C*
059600010321     C*  invia messaggio di errore alle code operatore e di programma
059700040219      * una sola volta x msg
059800041221     c                   if        primo_sum = '0' and tipfile= 'IFCSUM' or
059900041221     c                             primo_sta = '0' and tipfile= 'IFTSTA'
060000041118     C                   call      'TRTC100MSG'
060100010321     C                   PARM                    WCOD
060200010906     C                   PARM                    USRMSG
060300041118     C                   PARM                    codmsgerr
060400041221     C                   PARM                    tipfile
060500041221     c                   if        tipfile= 'IFCSUM'
060600041125     c                   move      '1'           primo_sum
060700041125     c                   else
060800041125     c                   move      '1'           primo_sta
060900041125     c                   end
061000040219     c                   end
061100010321     C*
061200010123     C                   ENDSR
061300010123     C*----------------------------------------------------------------
061400041117     C*  INVIO Messaggio
061500041117     C*----------------------------------------------------------------
061600041125     C     SND_Msg_sta   BEGSR
061700010413      *
061800010703      * SNDPGMMSG di avviso errore TOUSR(EDPAB)
061900010703     C                   clear                   MSGDS
062000041117     C                   MOVEL     CMD(3)        MSGDS
062100010703     C                   eval      MSGDS = %trim(MSGDS) +
062200010905     C                             ' TOUSR('+
062300010905     C                             %trim(USRMSG) +')'
062400010703     C                   Z-ADD     100           LENGH
062500010703     C                   CALL      'QCMDEXC'                            99
062600010703     C                   PARM                    MSGDS
062700010703     C                   PARM                    LENGH
062800010703      *
062900010703      * SNDBRKMSG di avviso errore TOUSR(DSP01)
063000050221     c                   if        msgd01 = 'S'
063100010703     C                   clear                   MSGDS
063200041117     C                   MOVEL     CMD(4)        MSGDS
063300010703     C                   eval      MSGDS = %trim(MSGDS) +
063400010703     C                             ' TOMSGQ(DSP01)'
063500010703     C                   Z-ADD     100           LENGH
063600010703     C                   CALL      'QCMDEXC'                            99
063700010703     C                   PARM                    MSGDS
063800010703     C                   PARM                    LENGH
063900050221     c                   end
064000010123      *
064100010123     C                   Endsr
064200041125     C*----------------------------------------------------------------
064300041125     C*  INVIO Messaggio
064400041125     C*----------------------------------------------------------------
064500041125     C     SND_Msg_sum   BEGSR
064600041125      *
064700041125      * SNDPGMMSG di avviso errore TOUSR(EDPAB)
064800041125     C                   clear                   MSGDS
064900041125     C                   MOVEL     CMD(5)        MSGDS
065000041125     C                   eval      MSGDS = %trim(MSGDS) +
065100041125     C                             ' TOUSR('+
065200041125     C                             %trim(USRMSG) +')'
065300041125     C                   Z-ADD     100           LENGH
065400041125     C                   CALL      'QCMDEXC'                            99
065500041125     C                   PARM                    MSGDS
065600041125     C                   PARM                    LENGH
065700041125      *
065800041125      * SNDBRKMSG di avviso errore TOUSR(DSP01)
065900050221     c                   if        msgd01 = 'S'
066000041125     C                   clear                   MSGDS
066100041125     C                   MOVEL     CMD(6)        MSGDS
066200041125     C                   eval      MSGDS = %trim(MSGDS) +
066300041125     C                             ' TOMSGQ(DSP01)'
066400041125     C                   Z-ADD     100           LENGH
066500041125     C                   CALL      'QCMDEXC'                            99
066600041125     C                   PARM                    MSGDS
066700041125     C                   PARM                    LENGH
066800050221     c                   end
066900041125      *
067000041125     C                   Endsr
067100001003      *----------------------------------------------------------------
067200041129     C*? Pulisce i vecchi records.
067300001003     C*----------------------------------------------------------------
067400050824     C**** Pulizia_old   BEGSR
067500050824      ****
067600050824     c****               move      *date         dataOGGI          8 0
067700050824      ****
067800050824      * ?------------------------------------------------------------------ */
067900050824      * ?Tolta la Pulizia qui poichè eseguita nel TRTC103R
068000050824      * ?------------------------------------------------------------------ */
068100041129      *   Vengono puliti i records + vecchi di 15 giorni
068200050824     c***  *dtaara       define                  puliSND
068300050824     c***  *lock         in(E)     puliSND
068400050824     c***                IF        %Error
068500050824     c***                z-add     15            GG_Piu_old
068600050824     c***                z-add     dataOGGI      dataPulizia
068700050824     c***                end
068800050824      ***
068900050824      ** solo se la data presente sull'area Dati
069000050824     C***                IF        dataOGGI <> DataPulizia
069100050824      ***
069200050824     c***                move      *date         data_iso
069300050824     c***  data_iso      subdur    GG_Piu_old:*d data_old
069400050824     c***                move      data_old      data08            8 0
069500050824      ***
069600050824     C***/EXEC SQL
069700050824     C***+ DELETE   FROM SNDIFTSTA  where
069800050824     C***+ SSDATELA <  :DATA08      and
069900050824     C***+ SSELABORA IN ('RX')
070000050824     C***/END-EXEC
070100050824      ***
070200050824     C***/EXEC SQL
070300050824     C***+ DELETE   FROM SNDIFCSUM  where
070400050824     C***+ SSDATELA <  :DATA08      and
070500050824     C***+ SSELABORA IN ('RX')
070600050824     C***/END-EXEC
070700041129      *  quindi aggiorna con la data di oggi l'area dati
070800041129      *   x non rieseguire in seguito ancora la pulizia che deve essere giornaliera
070900050824     c****               z-add     dataOGGI      dataPulizia
071000050824     c****               out       puliSND
071100050824      ****
071200050824     C****               EndIF
071300050824      ****
071400050824     C****               Endsr
071500041129      *----------------------------------------------------------------
071600041129     C*? *INZSR - OPERAZIONI INIZIALI
071700041129     C*----------------------------------------------------------------
071800041129     C     *INZSR        BEGSR
071900041129     C*
072000010320     C* Riceve un parametro per capire se si trova in ambiente di produzione
072100010320     C*  o in ambiente di test
072200010320     C     *entry        plist
072300010320     C                   parm                    flgtst            1
072400010906     C                   parm                    UsrMsg           10
072500041221     C                   parm                    TipFile          10
072600041221     C                   parm                    CodFile           3
072700050207     C                   parm                    Inz_Progr         5
072800041125     C*
072900050207     C*  Inizializza Progressivo da cui partire
073000050207     C*    (Vedi caso Pracht x spezzare in 2 un msg e differenziare anche tramite
073100050207     C*     progressivo i dettagli dei 2 msg Pracht)
073200050207     c                   if        Inz_Progr <> *blank
073300050207     c                   move      Inz_Progr     Da_Progr          5 0
073400050207     c                   end
073500050207     C*
073600041125     C*  Identifica che messaggio deve trattare
073700041125     c                   select
073800041221     c                   when      tipfile= 'IFTSTA'
073900041220     c                   open      SNDiftsta1
074000041221     c                   when      tipfile= 'IFCSUM'
074100041220     c                   open      SNDifcsum1
074200041125     c                   endsl
074300010320     C*
074400001003     C* Definisco chiavi di accesso
074500041220     C     KSND          KLIST
074600041220     C                   KFLD                    Blk_SND
074700041220     C                   move      *blank        Blk_SND           2
074800001003      *
074900041117     C     Ktab          KLIST
075000041117     C                   KFLD                    tabcod
075100041117     C                   KFLD                    tabkey
075200041117      *
075300001003      * RECUPERO DATI DELL'UTENTE
075400001003     C                   Z-ADD     1             CODUT
075500001003     C                   CALL      'XPARUT'
075600001003     C                   PARM                    UTEDSE0F
075700010123     C                   MOVEL     RAGUT         RSUT             20
075800010123     C                   Seton                                        OF
075900041125     c                   move      '0'           primo_sum         1
076000041125     c                   move      '0'           primo_sta         1
076100001003      *
076200001003      * Caricamento Tabella Partner esteri
076300001003     C                   Z-ADD     0             X
076400010307      *
076500020904      * Punta alle Tabelle in Lista di Librerie
076600010319     C                   clear                   MSGDS
076700041117     C                   MOVEL     CMD(2)        MSGDS
076800010319     C                   Z-ADD     100           LENGH
076900010307     C                   CALL      'QCMDEXC'
077000010307     C                   PARM                    MSGDS
077100010307     C                   PARM                    LENGH
077200010307      *
077300010307      * Carica la parte BARTOLINI
077400010307     C                   OPEN      EDTAB01L
077500001003     C                   MOVEL     'PT'          TABCOD
077600001006     C     TABCOD        SETLL     EDTAB01L
077700050321     C     TABCOD        READE     EDTAB01L
077800001003     C                   DOW       not %EOF(EDTAB01L)
077900001003     C     TABFLG        IFEQ      *BLANKS
078000001003     C                   ADD       1             X
078100001003     C                   MOVEL     TABKEY        CPT(X)
078200001003     C                   eval      CPTparz(X) = %subst(TABKEY:1:30)
078300001003     C                   MOVEL     TABUNI        DPT(X)
078400001003     C                   End
078500050321     C     TABCOD        READE     EDTAB01L
078600001003     C                   ENDdo
078700010123     C*
078800010123     C                   MOVEL     'PU'          TABCOD
078900010123     C     TABCOD        SETLL     EDTAB01L
079000050321     C     TABCOD        READE     EDTAB01L
079100010123     C                   DOW       not %EOF(EDTAB01L)
079200010123     C     TABFLG        IFEQ      *BLANKS
079300010123     C                   Z-ADD     1             X
079400010123     C     TABKEY        LOOKUP    CPT(X)                                 32
079500010123     C   32              MOVEL     TABUNI        DPU(X)
079600010123     C                   END
079700050321     C     TABCOD        READE     EDTAB01L                               30
079800010123     C                   END
079900050321     C*
080000050321     C                   close     EDTAB01L
080100050321      * Toglie l'OVRRIDE
080200050321     C                   clear                   MSGDS
080300050321     C                   MOVEL     CMD(1)        MSGDS
080400050321     C                   Z-ADD     100           LENGH
080500050321     C                   CALL      'QCMDEXC'
080600050321     C                   PARM                    MSGDS
080700050321     C                   PARM                    LENGH
080800041117      *
080900041117      * Prende i numeratori dalla tabella x num. di sede
081000041117     c                   clear                   sav_nr784
081100041117     c                   clear                   sav_nr781
081200041125     c                   clear                   sav_nr785
081300050221     C*
081400050221     C*  Recupara se deve mandare Msg a DSP01 dalla Area dati ACTERRMSG
081500050825      * ?------------------------------------------------------------------ */
081600050825      * ?Call x reperire se deve mandare Msg a DSP01
081700050825      * ? (x non aggiornare tabella KPJBU deve essere pulita)
081800050825      * ?------------------------------------------------------------------ */
081900050825     c                   clear                   kpjbu
082000050825     c                   call      'TRTC95BR'
082100050825     c                   parm                    kpjba
082200050825     c                   movel     kpjbu         edidsER
082300050825     c                   eval      MSGd01 = §ERD01
082400050825      **
082500041125     C*
082600010123     C* RECUPERO DATA E ORA
082700010123     C                   TIME                    WHHD12           12 0
082800010123     C                   MOVE      WHHD12        DATA6             6 0
082900010123     C                   MOVEL     DATA6         DATA              6 0
083000010123     C                   TIME                    WORA              6 0
083100010123     C*
083200001003     C                   ENDSR
083300001002      *****************************************************************
083400041125     OQSYSPRT   E            TESSTA           01
083500010123     O                       RSUT                20
083600041221     O                                           46 '** ERRORI TRASM. Status da'
083700041221     O                                              ' Filiale su  ->SNDIFTSTA *'
083800041118     O                                              '*'
083900041118     O                       DATA                89 '  /  /  '
084000041118     O                                           95 'Pag.:'
084100041118     O                       PAGE          Z    100
084200041125     O          E            TESSTA         1 02
084300041118     O                       WORA                89 '  :  :  '
084400050221     O                                          100 'TRTC102R'
084500010123     O*------------------------------------
084600041125     O          E            TESSUM           01
084700041125     O                       RSUT                20
084800041221     O                                           46 '** ERRORI TRASM. Bolle  da'
084900041221     O                                              ' Filiale su  ->SNDIFCSUM *'
085000041125     O                                              '*'
085100041125     O                       DATA                89 '  /  /  '
085200041125     O                                           95 'Pag.:'
085300041125     O                       PAGE          Z    100
085400041125     O          E            TESSUM         1 02
085500041125     O                       WORA                89 '  :  :  '
085600050221     O                                          100 'TRTC102R'
085700041125     O*------------------------------------
085800010123     O          E            ERRCLI         1
085900010123     O                                           15 'CODICE CLIENTE:'
086000010123     O                       §PTKSC        Z     23
086100010123     O                       WCOD                60
086200010123     O*------------------------------------
086300010123     O          E            DETER          1
086400010123     O                       DESERR              78
086500010123     O                       WVALOR             113
086600010413     O*------------------------------------
086700010413     O          E            MAXDET         1
086800010413     O                       WVALO1             113
086900010123     O*------------------------------------
087000010123** (ERR)
087100010123Il  Partner  NON è stato trovato nella Tabella "PT" con questo codice:
087200010124** (CMD)
087300010402DLTOVR FILE(EDTAB01L)
087400020904OVRDBF FILE(EDTAB01L) TOFILE(*LIBL/EDTAB01L)
087500041221SNDMSG MSG('ATTENZIONE Trasm. EDI - ERRORI in   SNDIFTSTA in GRU')
087600041221SNDBRKMSG MSG('ATTENZIONE Trasm. EDI - ERRORI in   SNDIFTSTA in GRU')
087700041221SNDMSG MSG('ATTENZIONE Trasm. EDI - ERRORI in   SNDIFCSUM in GRU')
087800041221SNDBRKMSG MSG('ATTENZIONE Trasm. EDI - ERRORI in   SNDIFCSUM in GRU')
