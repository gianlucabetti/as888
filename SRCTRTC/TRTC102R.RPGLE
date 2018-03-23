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
002700121105     D CPT             S             35    DIM(200)
002800121105     D CPTparz         S             35    DIM(%elem(CPT))
002900121105     D DPT             S             90    DIM(%elem(CPT))
003000121105     D TRUL0SDS      E DS
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
008800041221      *
008900050825     D EDIDSer       E DS
009000050825      *
009100041203      * DS per FNLV55R - Reperimento Terminal di Partenza e di Arrivo
009200041203     D FNLV55DS      E DS
009300041202      *
009400041202     D kpjba         E DS                  inz
009500121112     d kpjbusav        S                   like(Kpjbu)
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
034800010115     C                   clear                   EDST00DS
034900041125     C                   clear                   EDTT00DS
035000010115     C*
035100010123      *  Imposta la DS di controllo del record TESTATA << ST00 >>
035200041203      *    Status
035300041221     c                   if        tipfile= 'IFTSTA'
035400041221     C                   MOVEL     ssIFTSTA      EDST00DS
035500041117      * dati da riportare sul file x identificare il messaggio
035600041125     c                   move      St_TA1001     Sav_Docum
035700041125     C                   MOVE      St_TA1004     sav_nome
035800041125     C                   MOVE      St_TA0017     sav_date
035900041125     C                   MOVE      St_TA0019     sav_time
036000010115      * Primo tentativo rep. da codice + qualificatore
036100041221     C                   MOVEL     St_TA0010     WCOD
036200041221     C                   MOVE      St_TAA007     WCOD
036300041203     c                   end
036400041203      *    Bolle
036500041221     c                   if        tipfile= 'IFCSUM'
036600041221     C                   MOVEL     ssIFCSUM      EDTT00DS
036700041125      * dati da riportare sul file x identificare il messaggio
036800041125     c                   move      Sm_TA1001     Sav_Docum
036900041125     C                   MOVE      Sm_TA1004     sav_nome
037000041125     C                   MOVE      Sm_TA0017     sav_date
037100041125     C                   MOVE      Sm_TA0019     sav_time
037200041125      * Primo tentativo rep. da codice + qualificatore
037300041221     C                   MOVEL     Sm_TA0010     WCOD
037400041221     C                   MOVE      Sm_TAA007     WCOD
037500041125     c                   end
037600041203      *
037700041222      * _______________________________________________________________________
037800041222      * attenzione se non è il tipo di documento da tradurre deve saltare tutte
037900041222      *  le righe del documento NUMERATO.
038000041222     c                   z-add     0             num_salta
038100041222     c                   if        sav_Docum <> codfile
038200041222     c                   z-add     sumNUM        num_salta
038300041222     c                   goto      end_routine
038400041222     c                   end
038500041222      * _______________________________________________________________________
038600041222      *
038700041125     C                   MOVEL     '20'          sav_date
038800041125      *
038900010115     C                   Z-ADD     1             X
039000010115     C     WCOD          LOOKUP    CPT(X)                                 33
039100010123      *  Prova a cercarlo con chiave parziale
039200010115     C                   If        *IN33 = *off
039300010123     C                   eval      X = 1
039400041221     c                   if        tipfile= 'IFTSTA'
039500041221     C                   eval      W035A = %subst(St_TA0010:1:31)
039600041203     c                   end
039700041221     c                   if        tipfile= 'IFCSUM'
039800041221     C                   eval      W035A = %subst(Sm_TA0010:1:31)
039900041125     c                   end
040000010115     C     W035A         lookup    CPTparz(X)                             33
040100010115     C                   Endif
040200041115      *
040300010115      *  Salva dalla tabella "PT" il cliente solo se è stata presa direttamente
040400010115      *   o in modo parziale.
040500010115     C                   If        *IN33 = *on
040600041117     C                   move      'OK'          Partner_OK                     tutto OK
040700010115     C                   MOVEL     DPT(X)        EDIDSPT
040800041117     C                   MOVEL     §PTLNP        sav_Lnp
040900041117     C                   z-add     §PTKSC        sav_ksc
041000010123      *
041100010123     C                   Else
041200041117      *
041300041115      * se lo trova deve segnalare l'errore x Problemi di reperimento del Partner
041400041223     C                   move      'EF'          Partner_OK                     errore
041500041117     c                   move      'SI'          Err_su_msg
041600041117      *
041700041117      *  Problemi di reperimento del Partner
041800010123     C                   movel     ERR(1)        DesERR
041900010123     C                   movel     Wcod          WValor
042000010413     C                   clear                   WValo1
042100041221     C                   move      '02'          codmsgerr
042200041117      **              *--------------------------*
042300010123     C                   Exsr      STPERR
042400041117      **              *--------------------------*
042500010115     C                   Endif
042600041203      **           - - - - - - - - - - - - - - - - - - - - - - - -
042700041221      **   Comunque imposta il numeratore di filiale del messaggio
042800041221      **    con cui numererà tutti i records.
042900041221     c                   IF        Sav_Docum = '781'
043000041221     c                   z-add     sumNUM        Sav_nr781
043100041221     C                   Endif
043200041221     c                   IF        Sav_Docum = '784'
043300041221     c                   z-add     sumNUM        Sav_nr784
043400041221     C                   Endif
043500041221     c                   IF        Sav_Docum = '785'
043600041221     c                   z-add     sumNUM        Sav_nr785
043700041221     C                   Endif
043800041221      **           - - - - - - - - - - - - - - - - - - - - - - - -
043900041203      * imposta la LNP e l'HUB di partenza su schiera
044000041221      * per gestire in seguito il lancio delle azioni di ricezione in sede.
044100041203     C                   clear                   FNLV55DS
044200041203     C                   movel     'P'           D55tpt
044300041203     C                   move      sav_lnp       D55lin
044400041203     C                   move      *date         d55drf
044500041203     C                   CALL      'FNLV55R'
044600041203     C                   parm                    FNLV55DS
044700041203     c                   clear                   HubP              3 0
044800041203     c                   if        d55err = *blanks
044900041203     c                   movel     d55tfp        HubP
045000041203     c                   end
045100041203     c                   z-add     1             nrx
045200041203     c     sav_lnp       lookup    lnpX(nrX)                              34
045300041203     c                   if        *in34 = *Off
045400041203     c     *zeros        lookup    lnpX(nrX)                              34
045500041203     c                   end
045600041203     c                   z-add     sav_lnp       lnpX(nrX)
045700041203     c                   z-add     HubP          HubX(nrX)
045800041222     c                   if        SUMNUM > NumX(nrX)
045900041222     c                   z-add     SUMNUM        NumX(nrX)
046000041222     c                   end
046100041203      **           - - - - - - - - - - - - - - - - - - - - - - - -
046200041125      *
046300041118      *  per differenziare i messaggi temporalmente
046400041118     C                   TIME                    ST00_ora          6 0
046500041118      *
046600041222     C     End_Routine   Endsr
046700010123     C*----------------------------------------------------------------
046800041117     C*?  Deve Aggiornare il messaggio appena letto se OK oppure ER
046900010123     C*----------------------------------------------------------------
047000041117     C     AGG_record    BEGSR
047100041117      *              *-------------------------------*
047200041117      *  Aggiorna i campi gestione Bartolini
047300041117      *
047400041221     C                   eval      ssELABORA = Partner_OK
047500041221     C                   eval      ssNOMEDOC = Sav_Nome
047600041221     C                   eval      ssDATA    = Sav_Date
047700041221     C     100           mult      Sav_Time      ssORA
047800071108     C                   add       numDopo       ssORA
047900041221     C                   eval      ssLINEA   = Sav_Lnp
048000041221     C                   eval      ssDATELA  = *date
048100041221     C                   eval      ssORAELA  = ST00_ora
048200041125      *
048300041221     c                   if        tipfile= 'IFTSTA'
048400041125     c                   if        Sav_Docum = '784'
048500041221     C                   eval      ssNUMERO  = Sav_nr784
048600041221     C                   eval      ssTIPO    = '784'
048700041125     c                   else
048800041221     C                   eval      ssNUMERO  = Sav_nr781
048900041221     C                   eval      ssTIPO    = '781'
049000041125     c                   end
049100041125     c                   end
049200041117      *
049300041221     c                   if        tipfile= 'IFCSUM'
049400041125     c                   if        Sav_Docum = '785'
049500041221     C                   eval      ssNUMERO  = Sav_nr785
049600060116      **
049700060116      *  reimposta il numero nelle prime 4 posizioni --> vedi Pracht
049800060116     c                   if        da_progr > 0
049900060116     c                   move      ssNUMERO      numero_04         4
050000060116     c                   movel     numero_04     ssIFCSUM
050100060116     c                   end
050200060116      **
050300041221     C                   eval      ssTIPO    = '785'
050400041117     c                   end
050500041125     c                   end
050600041117      *
050700041117     C                   eval      num_progr = num_progr + 1
050800041221     C                   eval      ssPROGR   = num_progr
050900041125      *
051000060116      * ?--------------------------------------------------------------------
051100060116      *   Esegue controllo formale sui caratteri del Flat File da inviare
051200060116      *     il Set di Caratteri ammessi è: (SETA) solo Maiuscoli
051300060116      * ?  ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789 .,-()/='+:?!"%&*;<>
051400060116      * ?--------------------------------------------------------------------
051500060116      * ?  prima converte tutto in Caratteri Maiuscoli
051600060116     c                   if        tipfile= 'IFTSTA'
051700060116     c                   movel     SSiftsta      sta1950        1950
051800060116     C     Lo:Up         XLATE     sta1950       STA1950Up      1950
051900060116     c                   movel     STA1950Up     SSiftsta
052000060116     c                   end
052100060116     c                   if        tipfile= 'IFCSUM'
052200060116     c                   movel     SSifcsum      sum1950        1950
052300060116     C     Lo:Up         XLATE     sum1950       SUM1950Up      1950
052400060116     c                   movel     SUM1950Up     SSifcsum
052500060116     c                   end
052600060116      *
052700060116      * ?  poi esegue il controllo su caratteri non ammessi
052800060116     c                   if        tipfile= 'IFTSTA'
052900060116     c                   movel     SSiftSTA      TxtInOut
053000060116     c                   end
053100060116     c                   if        tipfile= 'IFCSUM'
053200060116     c                   movel     SSifcSUM      TxtInOut
053300060116     c                   end
053400060116     c                   eval      ElencoChar = SETA
053500060116     c                   eval      TipoElenco = *blank
053600060116     c                   eval      CharSost   = *blank
053700060116     c                   eval      UpperCase  = '1'
053800060116     c                   eval      ChkNull    = *blank
053900060116     c                   eval      CharNull   = *blank
054000060116     c                   eval      Esito      = *blank
054100060116      *
054200060116     c                   call      'XCHKCHAR'
054300060116     c                   parm                    TxtInOut
054400060116     c                   parm                    ElencoChar
054500060116     c                   parm                    TipoElenco
054600060116     c                   parm                    CharSost
054700060116     c                   parm                    UpperCase
054800060116     c                   parm                    ChkNull
054900060116     c                   parm                    CharNull
055000060116     c                   parm                    Esito
055100060116      *
055200060116      *   se è stato modificato qualcosa nella riga di FLAT
055300060116     c                   if        Esito = '1'
055400060116     c                   if        tipfile= 'IFTSTA'
055500060116     c                   movel     TxtInOut      SSiftSTA
055600060116     c                   end
055700060116     c                   if        tipfile= 'IFCSUM'
055800060116     c                   movel     TxtInOut      SSifcSUM
055900060116     c                   end
056000060116     c                   end
056100060116      *
056200041125      *              *-------------------------------*
056300041221     c                   if        tipfile= 'IFTSTA'
056400041220     C                   UPDATE    sndsta
056500041125     c                   end
056600041221     c                   if        tipfile= 'IFCSUM'
056700041220     C                   UPDATE    sndsum
056800041125     c                   end
056900041117      *              *-------------------------------*
057000041125      *
057100041117     C                   Endsr
057200041117     C*----------------------------------------------------------------
057300041117     C*? STPERR: STAMPA ERRORE
057400041117     C*----------------------------------------------------------------
057500041117     C     STPERR        BEGSR
057600041117     C*
057700010123     C     *INOF         IFEQ      '1'
057800041125     C*
057900041221     c                   if        tipfile='IFTSTA'
058000041125     C                   EXCEPT    TESSTA
058100041125     c                   else
058200041125     C                   EXCEPT    TESSUM
058300041125     c                   end
058400041125     C*
058500010123     C                   EXCEPT    ERRCLI
058600010123     C                   END
058700041125     C*
058800010123     C                   EXCEPT    DETER
058900010413     C*
059000010413     C     Wvalo1        IFne      *blank
059100010413     C                   EXCEPT    MAXDET
059200010413     C                   End
059300010123     C*
059400010321     C*  invia messaggio di errore alle code operatore e di programma
059500040219      * una sola volta x msg
059600041221     c                   if        primo_sum = '0' and tipfile= 'IFCSUM' or
059700041221     c                             primo_sta = '0' and tipfile= 'IFTSTA'
059800041118     C                   call      'TRTC100MSG'
059900010321     C                   PARM                    WCOD
060000010906     C                   PARM                    USRMSG
060100041118     C                   PARM                    codmsgerr
060200041221     C                   PARM                    tipfile
060300041221     c                   if        tipfile= 'IFCSUM'
060400041125     c                   move      '1'           primo_sum
060500041125     c                   else
060600041125     c                   move      '1'           primo_sta
060700041125     c                   end
060800040219     c                   end
060900010321     C*
061000010123     C                   ENDSR
061100010123     C*----------------------------------------------------------------
061200041117     C*  INVIO Messaggio
061300041117     C*----------------------------------------------------------------
061400041125     C     SND_Msg_sta   BEGSR
061500010413      *
061600010703      * SNDPGMMSG di avviso errore TOUSR(EDPAB)
061700010703     C                   clear                   MSGDS
061800041117     C                   MOVEL     CMD(3)        MSGDS
061900010703     C                   eval      MSGDS = %trim(MSGDS) +
062000010905     C                             ' TOUSR('+
062100010905     C                             %trim(USRMSG) +')'
062200010703     C                   Z-ADD     100           LENGH
062300010703     C                   CALL      'QCMDEXC'                            99
062400010703     C                   PARM                    MSGDS
062500010703     C                   PARM                    LENGH
062600010703      *
062700010703      * SNDBRKMSG di avviso errore TOUSR(DSP01)
062800050221     c                   if        msgd01 = 'S'
062900010703     C                   clear                   MSGDS
063000041117     C                   MOVEL     CMD(4)        MSGDS
063100010703     C                   eval      MSGDS = %trim(MSGDS) +
063200010703     C                             ' TOMSGQ(DSP01)'
063300010703     C                   Z-ADD     100           LENGH
063400010703     C                   CALL      'QCMDEXC'                            99
063500010703     C                   PARM                    MSGDS
063600010703     C                   PARM                    LENGH
063700050221     c                   end
063800010123      *
063900010123     C                   Endsr
064000041125     C*----------------------------------------------------------------
064100041125     C*  INVIO Messaggio
064200041125     C*----------------------------------------------------------------
064300041125     C     SND_Msg_sum   BEGSR
064400041125      *
064500041125      * SNDPGMMSG di avviso errore TOUSR(EDPAB)
064600041125     C                   clear                   MSGDS
064700041125     C                   MOVEL     CMD(5)        MSGDS
064800041125     C                   eval      MSGDS = %trim(MSGDS) +
064900041125     C                             ' TOUSR('+
065000041125     C                             %trim(USRMSG) +')'
065100041125     C                   Z-ADD     100           LENGH
065200041125     C                   CALL      'QCMDEXC'                            99
065300041125     C                   PARM                    MSGDS
065400041125     C                   PARM                    LENGH
065500041125      *
065600041125      * SNDBRKMSG di avviso errore TOUSR(DSP01)
065700050221     c                   if        msgd01 = 'S'
065800041125     C                   clear                   MSGDS
065900041125     C                   MOVEL     CMD(6)        MSGDS
066000041125     C                   eval      MSGDS = %trim(MSGDS) +
066100041125     C                             ' TOMSGQ(DSP01)'
066200041125     C                   Z-ADD     100           LENGH
066300041125     C                   CALL      'QCMDEXC'                            99
066400041125     C                   PARM                    MSGDS
066500041125     C                   PARM                    LENGH
066600050221     c                   end
066700041125      *
066800041125     C                   Endsr
066900001003      *----------------------------------------------------------------
067000041129     C*? Pulisce i vecchi records.
067100001003     C*----------------------------------------------------------------
067200050824     C**** Pulizia_old   BEGSR
067300050824      ****
067400050824     c****               move      *date         dataOGGI          8 0
067500050824      ****
067600050824      * ?------------------------------------------------------------------ */
067700050824      * ?Tolta la Pulizia qui poichè eseguita nel TRTC103R
067800050824      * ?------------------------------------------------------------------ */
067900041129      *   Vengono puliti i records + vecchi di 15 giorni
068000050824     c***  *dtaara       define                  puliSND
068100050824     c***  *lock         in(E)     puliSND
068200050824     c***                IF        %Error
068300050824     c***                z-add     15            GG_Piu_old
068400050824     c***                z-add     dataOGGI      dataPulizia
068500050824     c***                end
068600050824      ***
068700050824      ** solo se la data presente sull'area Dati
068800050824     C***                IF        dataOGGI <> DataPulizia
068900050824      ***
069000050824     c***                move      *date         data_iso
069100050824     c***  data_iso      subdur    GG_Piu_old:*d data_old
069200050824     c***                move      data_old      data08            8 0
069300050824      ***
069400050824     C***/EXEC SQL
069500050824     C***+ DELETE   FROM SNDIFTSTA  where
069600050824     C***+ SSDATELA <  :DATA08      and
069700050824     C***+ SSELABORA IN ('RX')
069800050824     C***/END-EXEC
069900050824      ***
070000050824     C***/EXEC SQL
070100050824     C***+ DELETE   FROM SNDIFCSUM  where
070200050824     C***+ SSDATELA <  :DATA08      and
070300050824     C***+ SSELABORA IN ('RX')
070400050824     C***/END-EXEC
070500041129      *  quindi aggiorna con la data di oggi l'area dati
070600041129      *   x non rieseguire in seguito ancora la pulizia che deve essere giornaliera
070700050824     c****               z-add     dataOGGI      dataPulizia
070800050824     c****               out       puliSND
070900050824      ****
071000050824     C****               EndIF
071100050824      ****
071200050824     C****               Endsr
071300041129      *----------------------------------------------------------------
071400041129     C*? *INZSR - OPERAZIONI INIZIALI
071500041129     C*----------------------------------------------------------------
071600041129     C     *INZSR        BEGSR
071700041129     C*
071800010320     C* Riceve un parametro per capire se si trova in ambiente di produzione
071900010320     C*  o in ambiente di test
072000010320     C     *entry        plist
072100010320     C                   parm                    flgtst            1
072200010906     C                   parm                    UsrMsg           10
072300041221     C                   parm                    TipFile          10
072400041221     C                   parm                    CodFile           3
072500050207     C                   parm                    Inz_Progr         5
072600041125     C*
072700050207     C*  Inizializza Progressivo da cui partire
072800050207     C*    (Vedi caso Pracht x spezzare in 2 un msg e differenziare anche tramite
072900050207     C*     progressivo i dettagli dei 2 msg Pracht)
073000050207     c                   if        Inz_Progr <> *blank
073100050207     c                   move      Inz_Progr     Da_Progr          5 0
073200050207     c                   end
073300050207     C*
073400041125     C*  Identifica che messaggio deve trattare
073500041125     c                   select
073600041221     c                   when      tipfile= 'IFTSTA'
073700041220     c                   open      SNDiftsta1
073800041221     c                   when      tipfile= 'IFCSUM'
073900041220     c                   open      SNDifcsum1
074000041125     c                   endsl
074100010320     C*
074200001003     C* Definisco chiavi di accesso
074300041220     C     KSND          KLIST
074400041220     C                   KFLD                    Blk_SND
074500041220     C                   move      *blank        Blk_SND           2
074600001003      *
074700041117     C     Ktab          KLIST
074800041117     C                   KFLD                    tabcod
074900041117     C                   KFLD                    tabkey
075000041117      *
075100001003      * RECUPERO DATI DELL'UTENTE
075200001003     C                   Z-ADD     1             CODUT
075300001003     C                   CALL      'XPARUT'
075400001003     C                   PARM                    UTEDSE0F
075500010123     C                   MOVEL     RAGUT         RSUT             20
075600010123     C                   Seton                                        OF
075700041125     c                   move      '0'           primo_sum         1
075800041125     c                   move      '0'           primo_sta         1
075900001003      *
076000001003      * Caricamento Tabella Partner esteri
076100001003     C                   Z-ADD     0             X
076200010307      *
076300020904      * Punta alle Tabelle in Lista di Librerie
076400010319     C                   clear                   MSGDS
076500041117     C                   MOVEL     CMD(2)        MSGDS
076600010319     C                   Z-ADD     100           LENGH
076700010307     C                   CALL      'QCMDEXC'
076800010307     C                   PARM                    MSGDS
076900010307     C                   PARM                    LENGH
077000010307      *
077100010307      * Carica la parte BARTOLINI
077200010307     C                   OPEN      EDTAB01L
077300001003     C                   MOVEL     'PT'          TABCOD
077400001006     C     TABCOD        SETLL     EDTAB01L
077500050321     C     TABCOD        READE     EDTAB01L
077600001003     C                   DOW       not %EOF(EDTAB01L)
077700001003     C     TABFLG        IFEQ      *BLANKS
077800001003     C                   ADD       1             X
077900001003     C                   MOVEL     TABKEY        CPT(X)
078000001003     C                   eval      CPTparz(X) = %subst(TABKEY:1:30)
078100001003     C                   MOVEL     TABUNI        DPT(X)
078200001003     C                   End
078300050321     C     TABCOD        READE     EDTAB01L
078400001003     C                   ENDdo
078500121105      *
078600121105      * Controlla riempimento della PT x limiti di schiera
078700121105      *  se deve inviare la mail di alert x limite quasi raggiunto.
078800121105     c                   exsr      ChecK_PT
078900050321     C*
079000050321     C                   close     EDTAB01L
079100050321      * Toglie l'OVRRIDE
079200050321     C                   clear                   MSGDS
079300050321     C                   MOVEL     CMD(1)        MSGDS
079400050321     C                   Z-ADD     100           LENGH
079500050321     C                   CALL      'QCMDEXC'
079600050321     C                   PARM                    MSGDS
079700050321     C                   PARM                    LENGH
079800041117      *
079900041117      * Prende i numeratori dalla tabella x num. di sede
080000041117     c                   clear                   sav_nr784
080100041117     c                   clear                   sav_nr781
080200041125     c                   clear                   sav_nr785
080300050221     C*
080400050221     C*  Recupara se deve mandare Msg a DSP01 dalla Area dati ACTERRMSG
080500050825      * ?------------------------------------------------------------------ */
080600050825      * ?Call x reperire se deve mandare Msg a DSP01
080700050825      * ? (x non aggiornare tabella KPJBU deve essere pulita)
080800050825      * ?------------------------------------------------------------------ */
080900050825     c                   clear                   kpjbu
081000050825     c                   call      'TRTC95BR'
081100050825     c                   parm                    kpjba
081200050825     c                   movel     kpjbu         edidsER
081300050825     c                   eval      MSGd01 = §ERD01
081400050825      **
081500041125     C*
081600010123     C* RECUPERO DATA E ORA
081700010123     C                   TIME                    WHHD12           12 0
081800010123     C                   MOVE      WHHD12        DATA6             6 0
081900010123     C                   MOVEL     DATA6         DATA              6 0
082000010123     C                   TIME                    WORA              6 0
082100010123     C*
082200001003     C                   ENDSR
082300121105      *---------------------------------------------------------------*
082400121105      *                                                              -*
082500121105      *---------------------------------------------------------------*
082600121105     C     Check_PT      begsr
082700121105     C*
082800121105     c* Controllo riempimento schiera
082900121105     c                   clear                   trul0sds
083000121105     c                   eval      i0sski='CPT'
083100121105     c                   eval      i0sele=%elem(CPT)
083200121105     c                   eval      i0spie=x
083300121105     c                   eval      i0sfile='EDTAB00F'
083400121105     c                   eval      i0ssif=knsif
083500121105     c                   eval      i0spgm='TRTC102R'
083600121112     c                   movel     kpjbu         kpjbusav
083700121112     c                   clear                   kpjbu
083800121105     c                   movel     trul0sds      kpjbu
083900121105     c                   call      'TRUL0SR'
084000121105     c                   parm                    kpjba
084100121112     c                   movel     kpjbusav      kpjbu
084200121105     C*
084300121105     C                   ENDSR
084400001002      *****************************************************************
084500041125     OQSYSPRT   E            TESSTA           01
084600010123     O                       RSUT                20
084700041221     O                                           46 '** ERRORI TRASM. Status da'
084800041221     O                                              ' Filiale su  ->SNDIFTSTA *'
084900041118     O                                              '*'
085000041118     O                       DATA                89 '  /  /  '
085100041118     O                                           95 'Pag.:'
085200041118     O                       PAGE          Z    100
085300041125     O          E            TESSTA         1 02
085400041118     O                       WORA                89 '  :  :  '
085500050221     O                                          100 'TRTC102R'
085600010123     O*------------------------------------
085700041125     O          E            TESSUM           01
085800041125     O                       RSUT                20
085900041221     O                                           46 '** ERRORI TRASM. Bolle  da'
086000041221     O                                              ' Filiale su  ->SNDIFCSUM *'
086100041125     O                                              '*'
086200041125     O                       DATA                89 '  /  /  '
086300041125     O                                           95 'Pag.:'
086400041125     O                       PAGE          Z    100
086500041125     O          E            TESSUM         1 02
086600041125     O                       WORA                89 '  :  :  '
086700050221     O                                          100 'TRTC102R'
086800041125     O*------------------------------------
086900010123     O          E            ERRCLI         1
087000010123     O                                           15 'CODICE CLIENTE:'
087100010123     O                       §PTKSC        Z     23
087200010123     O                       WCOD                60
087300010123     O*------------------------------------
087400010123     O          E            DETER          1
087500010123     O                       DESERR              78
087600010123     O                       WVALOR             113
087700010413     O*------------------------------------
087800010413     O          E            MAXDET         1
087900010413     O                       WVALO1             113
088000010123     O*------------------------------------
088100010123** (ERR)
088200010123Il  Partner  NON è stato trovato nella Tabella "PT" con questo codice:
088300010124** (CMD)
088400010402DLTOVR FILE(EDTAB01L)
088500020904OVRDBF FILE(EDTAB01L) TOFILE(*LIBL/EDTAB01L)
088600041221SNDMSG MSG('ATTENZIONE Trasm. EDI - ERRORI in   SNDIFTSTA in GRU')
088700041221SNDBRKMSG MSG('ATTENZIONE Trasm. EDI - ERRORI in   SNDIFTSTA in GRU')
088800041221SNDMSG MSG('ATTENZIONE Trasm. EDI - ERRORI in   SNDIFCSUM in GRU')
088900041221SNDBRKMSG MSG('ATTENZIONE Trasm. EDI - ERRORI in   SNDIFCSUM in GRU')
