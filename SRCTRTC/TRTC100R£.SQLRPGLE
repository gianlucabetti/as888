000100041117     H DECEDIT('0,') DATEDIT(*YMD.)
000200000929      ***********************************************************************
000300010115      *   Il pgm serve per smistare alle filiali gli - IFTSTA -             *
000400041221      *   ricevuti e già tradotti  in SEDE       (GIRA IN SEDE)             *
000500041117      ***********************************************************************
000600041117      *  ATTENZIONE :
000700041117      *    ogni messaggio tradotto ed elaborato ha 4 stati di avanzamenti
000800041117      *         blk = ricevuto da EDI e da elaborare da questo pgm
000900041117      *        'OK' = una volta elaborato il Partner è noto e tutto è OK
001000041117      *        'ER' = msg con Partner non riconosciuto su tab. PT
001100041117      *        'RX' = msg ricevuto ed elaborato in filiale con i pgm specifici
001200041220      *        'EF' = msg con Partner non riconosciuto su tab. PT in FILIALE
001300000929      ***********************************************************************
001400041125     FRCVIFTSTA1UF   E           k DISK    usropn rename(rcvdati:rcvsta)        in lettura
001500041207     f                                     commit
001600041125     FRCVIFCSUM1UF   E           k DISK    usropn rename(rcvdati:rcvsum)        in lettura
001700050914     F                                     INFDS(SRECDS)
001800041207     f                                     commit
001900010115      *---------
002000041117     FEDTAB01L  UF   E           K DISK    usropn
002100041207     f                                     commit
002200010115      *---------
002300010123     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
002400041117      *---------------------------------------------------------------------*
002500041117      * Schiere
002600041117      *---------------------------------------------------------------------*
002700001002      * Tabella PT --> partner
002800001002     D CPT             S             35    DIM(100)
002900000125     D CPTparz         S             35    DIM(100)
003000960528     D DPT             S             90    DIM(100)
003100010123     D DPU             S             90    DIM(100)
003101111017     D DPZ             S             90    DIM(100)
003200050914      *
003300050914      * numero relativo di record
003400050914     D SRECDS          DS
003500050914     D  NRREC                397    400B 0
003600010123      *---------------------------------------------------------------------*
003700010123      * Comandi
003800010123      *---------------------------------------------------------------------*
003900010123     D ERR             S             78    DIM(5) CTDATA PERRCD(1)
004000041125     D CMD             S             78    DIM(6) CTDATA PERRCD(1)
004100041203      *---------------------------------------------------------------------*
004200041203      * Schiere
004300041203      *---------------------------------------------------------------------*
004400041203      * x lancio azione di traduzione in filiale
004500041203     D LnpX            S              3  0 DIM(999) inz
004600041203     D HubX            S              3  0 DIM(999) inz
004700041203     D NumX            S              7  0 DIM(999) inz
004800111017     D SumX            S              6    DIM(999) inz
004900000929      *---------------------------------------------------------------------*
005000001005      * Flag oprativi di work
005100001005      *---------------------------------------------------------------------*
005200001003     D WCOD            S             35
005300020904     D W035A           S             35
005400010123     D DesErr          S             78
005500010123     D WValor          S             35
005600010413     D WValo1          S             35
005700041117      *---------------------------------------------------------------------*
005800041117     D Sav_nome        S             35
005900041117     D sav_date        S              8  0 INZ(0)
006000041117     D sav_time        S              4  0 INZ(0)
006100041117     D Sav_ksc         s              7  0 INZ(0)
006200041117     D Sav_Lnp         s              3  0 INZ(0)
006300041125     D Sav_Nr785       s              7  0 INZ(0)
006400070504     D Sav_Nr350       s              7  0 INZ(0)
006500061214     D Sav_Nr730       s              7  0 INZ(0)
006600041117     D Sav_Nr784       s              7  0 INZ(0)
006700041117     D Sav_Nr781       s              7  0 INZ(0)
006800041125     D Prima_TST785    s              1  0 INZ(0)
006900070504     D Prima_TST350    s              1  0 INZ(0)
007000061214     D Prima_TST730    s              1  0 INZ(0)
007100041125     D Prima_TST784    s              1  0 INZ(0)
007200041117     D Prima_TST781    s              1  0 INZ(0)
007300041117     D Num_Progr       s              5  0 INZ(0)
007400041125     D Sav_Docum       s              3    INZ('  ')
007500041117     D Partner_OK      S              2    INZ('  ')
007600041117     D Err_su_msg      S              2    INZ('  ')
007700041118     D CodMsgErr       S              2    INZ('  ')
007800041118     D Anno_04         s              4  0 INZ(0)
007900050221     D MSGd01          S              1    INZ(' ')
008000111017     D Sav_Map_SUM     s              6    INZ(' ')
008100111017     D Sav_Map_STA     s              6    INZ(' ')
008200041129      *
008300041129     D data_iso        s               d   datfmt(*iso)
008400041129     D data_old        s               d   datfmt(*iso)
008500010115      *---------------------------------------------------------------------*
008600010319     D  MSGDS          S            100
008700010124     D  LENGH          S             15  5
008800001003     D X               s              3  0 INZ(0)
008900041203     D nrX             s              3  0 INZ(0)
009000041203     D svX             s              3  0 INZ(0)
009100000120      *---------------------------------------------------------------------*
009200000120      * DS
009300000120      *---------------------------------------------------------------------*
009400041125     D EDST00DS      E DS                  prefix(ST_)                          Sta
009500041125     D EDTT00DS      E DS                  prefix(SM_)                          Sum
009600041125      *
009700010123     D EDIDSPT       E DS
009800010123     D EDIDSPU       E DS
009801111017     D EDIDSPZ       E DS
009900041117     D EDIDSMS       E DS
010000041203      * DS per FNLV55R - Reperimento Terminal di Partenza e di Arrivo
010100041203     D FNLV55DS      E DS
010200041202      *
010300041202     D kpjba         E DS                  inz
010400041202      *
010500041202     D                SDS
010600041202     D  §PGMname               1     10
010700041202     D  §USRname             254    263
010800000120      *---------------------------------------------------------------------*
010900050825     D EDIDSER       e ds
011000050823     D EDIDSGG       e ds
011100050823      **
011200041129     D PuliRCV         DS            30
011300041129     d  DataPulizia                   8  0
011400041129     d  GG_Piu_old                    3  0
011500050823     d**Free_Field                   19
011600041129      *---------------------------------------------------------------------*
011700000120     D UTEDSE0F      E DS
011800000120     D  TCU                  398    697    DIM(50)                              Flg 8 tp.conto
011900000120     D  KCU                  698    847P 0 DIM(50)  PACKEVEN                    Capiconto
012000000929      *---------------------------------------------------------------------*
012100010123      *                    .. 5   8  : tipo record letto
012200041221     D DSdati          DS          1950
012300041221     D  SUMNUM                 1      4
012400041221     D  SUMTPR                 5      8
012500000929     D  SUMINR                 5      6
012600041117      *---------------------------------------------------------------------*
012700041117     D Digits          C                   CONST('0123456789')
012800000120      *---------------------------------------------------------------------*
012900000120      * Ciclo principale
013000000120      *---------------------------------------------------------------------*
013100041125     c*  esegue un ciclo oppure l'altro a seconda del tipo di msg da trattare
013200041117     c                   clear                   Err_su_msg
013300041125     C*
013400041125     c                   select
013500041125     C*
013600041125     c                   when      tipfil = 'IFCSUM'
013700041125     c                   exsr      Ciclo_SUM
013800041125     C*
013900041125     c                   when      tipfil = 'IFTSTA'
014000041125     c                   exsr      Ciclo_STA
014100041125     C*
014200041125     c                   endsl
014300041125     C*
014400041125     C                   close     EDTAB01L
014500041125      * Toglie l'OVRRIDE
014600041125     C                   clear                   MSGDS
014700041125     C                   MOVEL     CMD(1)        MSGDS
014800041125     C                   Z-ADD     100           LENGH
014900041125     C                   CALL      'QCMDEXC'
015000041125     C                   PARM                    MSGDS
015100041125     C                   PARM                    LENGH
015200041125      *
015300041129      * Prima di uscire esegue una pulizia dei vecchi records
015400041129      *     + vecchi di xx giorni
015500041129      **              *--------------------------*
015600041129     c                   exsr      Pulizia_old
015700041129      **              *--------------------------*
015800041129      *
015900041203     C                   CLEAR                   FNLV55DS
016000041203     C                   MOVEL     'C'           D55TLA
016100041203     C                   CALL      'FNLV55R'
016200041203     C                   PARM                    FNLV55DS
016300041203      *
016400041207     C                   Commit
016500041207      *
016600041125     C                   SETON                                        LR
016700041125      *---------------------------------------------------------------------*
016800041125      *  Ciclo per la codifica degli Stati
016900041125      *---------------------------------------------------------------------*
017000041125     C     Ciclo_STA     Begsr
017100041125      *
017200041117      * Lettura iniziale sul file di tutti i records allo stato "BLK"
017300041125      *
017400041125     C     Krcv          setll     RCViftsta1
017500041125     C     Krcv          reade     RCViftsta1
017600000929      *
017700010123      * Ciclo di Lettura fino a EOF
017800041115     C                   DOW       not %EOF(RCViftsta1)
017900001002      *
018000041117      *  muove i dati flat su una DS
018100001002      *                          ++++++++++++++++++++++++
018200041117     C                   MOVEL     rrIFTSTA      DSdati
018300041117      * -----------              ++++++++++++++++++++++++
018400041117      * Testata MSG
018500041117      * -----------
018600041117     C                   If        SUMTPR = 'ST00'                              TESTATA
018700041117      **              *--------------------------*
018800041117     C                   EXSR      DATI_Testata
018900041117      **              *--------------------------*
019000000929     C                   EndIF
019100041117      **
019200041117      **   deve aggiornare i records solo dopo aver esaminato la testata
019300041117      **              *--------------------------*
019400041117     c                   exsr      AGG_record
019500041117      **              *--------------------------*
019600010123      *      -----------
019700960329      * Lettura successiva
019800041115     C     Krcv          reade     RCViftsta1
019900010123     C                   ENDdo
020000041117     C*------------------
020100041117     C*  Prima di uscire deve aggiornare i numeratori 784SEDE e 781SEDE
020200041117      *  aggiorna i numeratori dalla tabella x num. di sede
020300041117     C                   MOVEL(p)  'MS'          TABCOD
020400041117     c                   MOVEL(p)  '784SEDE'     TABKEY
020500041117     C     Ktab          chain     EDTAB01L
020600041117     C                   if        %found(EDTAB01L)
020700041117     C                   MOVEL     TABUNI        edidsms
020800041117     c                   if        §msnum <> sav_nr784 or
020900041117     c                             §msnum =  sav_nr784 and Prima_TST784 = 1
021000041117     c                   eval      §MSNUM = sav_nr784 + 1
021100041117     c                   eval      §MSDAT = *date
021200041117     C                   MOVEL     edidsms       TABUNI
021300041117     c                   end
021400041117     C                   update    EDTAB000
021500041117     c                   end
021600041117     C*
021700041117     C                   MOVEL(p)  'MS'          TABCOD
021800041117     c                   MOVEL(p)  '781SEDE'     TABKEY
021900041117     C     Ktab          chain     EDTAB01L
022000041117     C                   if        %found(EDTAB01L)
022100041117     C                   MOVEL     TABUNI        edidsms
022200041117     c                   if        §msnum <> sav_nr781 or
022300041117     c                             §msnum =  sav_nr781 and Prima_TST781 = 1
022400041117     c                   eval      §MSNUM = sav_nr781 + 1
022500041117     c                   eval      §MSDAT = *date
022600041117     C                   MOVEL     edidsms       TABUNI
022700041117     c                   end
022800041117     C                   update    EDTAB000
022900041117     c                   end
023000041117      *
023100041117      *  se ci sono stati dei problemi con dei messaggi
023200041117     c                   if        Err_su_msg = 'SI'
023300041117      **              *--------------------------*
023400041125     c                   exsr      snd_msg_sta
023500041117      **              *--------------------------*
023600041117     c                   end
023700041125      *
023800041125     C                   Endsr
023900041125      *---------------------------------------------------------------------*
024000041125      *  Ciclo per la codifica delle Bolle
024100041125      *---------------------------------------------------------------------*
024200041125     C     Ciclo_SUM     Begsr
024300041125      *
024400041125      * Lettura iniziale sul file di tutti i records allo stato "BLK"
024500041125      *
024600041125     C     Krcv          setll     RCVifcsum1
024700041125     C     Krcv          reade     RCVifcsum1
024800041125      *
024900041125      * Ciclo di Lettura fino a EOF
025000041125     C                   DOW       not %EOF(RCVifcsum1)
025100041125      *  muove i dati flat su una DS
025200041125      *                          ++++++++++++++++++++++++
025300041125     C                   MOVEL     rrIFCSUM      DSdati
025400041125      * -----------              ++++++++++++++++++++++++
025500041125      * Testata MSG
025600041125      * -----------
025700041125     C                   If        SUMTPR = 'TT00'                              TESTATA
025800041125      **              *--------------------------*
025900041125     C                   EXSR      DATI_Testata
026000041203      **              *--------------------------*
026100041125     C                   EndIF
026200041125      **
026300041125      **   deve aggiornare i records solo dopo aver esaminato la testata
026400041125      **              *--------------------------*
026500041125     c                   exsr      AGG_record
026600041125      **              *--------------------------*
026700041125      *      -----------
026800041125      * Lettura successiva
026900041125     C     Krcv          reade     RCVifcsum1
027000041125     C                   ENDdo
027100041125     C*------------------
027200061214     C*  Prima di uscire deve aggiornare i numeratori 785SEDE e 730SEDE
027300070504     C*  e 350SEDE
027400041125      *  aggiorna i numeratori dalla tabella x num. di sede
027500041125     C                   MOVEL(p)  'MS'          TABCOD
027600041125     c                   MOVEL(p)  '785SEDE'     TABKEY
027700041125     C     Ktab          chain     EDTAB01L
027800041125     C                   if        %found(EDTAB01L)
027900041125     C                   MOVEL     TABUNI        edidsms
028000041125     c                   if        §msnum <> sav_nr785 or
028100041125     c                             §msnum =  sav_nr785 and Prima_TST785 = 1
028200041125     c                   eval      §MSNUM = sav_nr785 + 1
028300041125     c                   eval      §MSDAT = *date
028400041125     C                   MOVEL     edidsms       TABUNI
028500041125     c                   end
028600041125     C                   update    EDTAB000
028700041125     c                   end
028800061214     C*
028900061214     C                   MOVEL(p)  'MS'          TABCOD
029000061214     c                   MOVEL(p)  '730SEDE'     TABKEY
029100061214     C     Ktab          chain     EDTAB01L
029200061214     C                   if        %found(EDTAB01L)
029300061214     C                   MOVEL     TABUNI        edidsms
029400061214     c                   if        §msnum <> sav_nr730 or
029500061214     c                             §msnum =  sav_nr730 and Prima_TST730 = 1
029600061214     c                   eval      §MSNUM = sav_nr730 + 1
029700061214     c                   eval      §MSDAT = *date
029800061214     C                   MOVEL     edidsms       TABUNI
029900061214     c                   end
030000061214     C                   update    EDTAB000
030100061214     c                   end
030200041125      *
030300070504     C                   MOVEL(p)  'MS'          TABCOD
030400070504     c                   MOVEL(p)  '350SEDE'     TABKEY
030500070504     C     Ktab          chain     EDTAB01L
030600070504     C                   if        %found(EDTAB01L)
030700070504     C                   MOVEL     TABUNI        edidsms
030800070504     c                   if        §msnum <> sav_nr350 or
030900070504     c                             §msnum =  sav_nr350 and Prima_TST350 = 1
031000070504     c                   eval      §MSNUM = sav_nr350 + 1
031100070504     c                   eval      §MSDAT = *date
031200070504     C                   MOVEL     edidsms       TABUNI
031300070504     c                   end
031400070504     C                   update    EDTAB000
031500070504     c                   end
031600070504      *
031700060314      * ?Esegue il Commit prima di lanciare le sottomissioni
031800060314      * ?              *--------*
031900060314     c                   commit
032000060314      * ?              *--------*
032100060314      *
032200041202      *  Quindi lancia l'azione in ambiente di filiale per ricevere
032300041202      *  le Bolle sui relativi EDVAB o FIVAB
032400041203     c                   do        99            svX
032500041203     c                   if        lnpX(svX) = 0
032600041203     c                   leave
032700041203     c                   end
032800041203     c                   move      LnpX(svX)     Lnp_alf           3
032900041203     c                   move      HubX(svX)     Hub_alf           3
033000041203     c                   move      NumX(svX)     Num_alf           7
033100041202     c                   clear                   kpjbu
033200070104      *
033300070104      * Versione SUM --> S93A
033400111017     c                   if        SumX(svX) = 'SUM93A'
033500041202     c                   movel     'TC80'        kcoaz
033600070104     c                   end
033700070104      *
033800070104      * Versione SUM --> D96A
033900070104      * ?da modificare quando pronto il nuovo traduttore versione 96A
034000111017     c                   if        SumX(svX) = 'SUM96A'
034100070226     c                   movel     'TC79'        kcoaz
034200070104     c                   end
034300070104      *
034400041202     c                   if        flgtst = ' '
034500041202     c                   movel     'FILTRA201 '  knsif
034600041203     c                   eval      kqebi = 'K'+ Hub_alf + 'EDI'
034700041203     c                   eval      knmus = 'EDPCED' + Hub_alf
034800041203     c                   eval      knmtd = knmus
034900041203     c                   else
035000041203     c                   movel     'FILTRAPFP '  knsif
035100041203     c                   eval      kqebi = 'K043EDI'
035200041203     c                   eval      knmus = 'EDPCED043'
035300041203     c                   eval      knmtd = knmus
035400041203     c                   end
035500041202     c                   movel     'QPRINTS   '  kqopr
035600041203     c                   eval      %subst(kpjbu:240:7) = Num_alf
035700041203     c                   eval      %subst(kpjbu:247:4) = 'M046'
035800041203     c                   eval      %subst(kpjbu:251:3) = Lnp_alf
035900041203     c                   eval      %subst(kpjbu:254:3) = Hub_alf
036000041203      *
036100041206      *  x provare la procedura mettevo in Hold il lavoro
036200041206     c*****              eval      KSTEB = 'H'
036300041206     c*****              eval      KSTJO = 'H'
036400041202     c                   call      'BCH10'
036500041202     c                   parm                    kpjba
036600041203     c                   endDO
036700041202      *
036800041125      *  se ci sono stati dei problemi con dei messaggi
036900041125     c                   if        Err_su_msg = 'SI'
037000041125      **              *--------------------------*
037100041125     c                   exsr      snd_msg_sum
037200041125      **              *--------------------------*
037300041125     c                   end
037400041125      *
037500041125     C                   Endsr
037600010115     C*-------------------------------------------------------------------------------------
037700010123     C*?    Controlla la testata a quale Filiale appartiene il Partner
037800010115     C*-------------------------------------------------------------------------------------
037900041117     C     DATI_Testata  BEGSR
038000041117      *
038100010115     C* inizializzo i campi di salvataggio codici
038200041117     c                   clear                   num_progr
038300041117     c                   clear                   Sav_Docum
038400041117     C                   clear                   sav_Ksc
038500041117     C                   clear                   sav_Lnp
038600041117     C                   clear                   sav_nome
038700041117     C                   clear                   sav_date
038800041117     C                   clear                   sav_time
038900070104     C                   clear                   sav_Map_SUM
039000070104     C                   clear                   sav_Map_STA
039100041117      *
039200041117     C                   clear                   Partner_OK
039300041117     C                   clear                   WCod
039400010115     C                   clear                   EDIDSPT
039500010123     C                   clear                   EDIDSPU
039600010115     C                   clear                   EDST00DS
039700041125     C                   clear                   EDTT00DS
039800010115     C*
039900010123      *  Imposta la DS di controllo del record TESTATA << ST00 >>
040000041203      *    Status
040100041125     c                   if        tipfil = 'IFTSTA'
040200041117     C                   MOVEL     rrIFTSTA      EDST00DS
040300041117      * dati da riportare sul file x identificare il messaggio
040400041125     c                   move      St_TA1001     Sav_Docum
040500041125     C                   MOVE      St_TA1004     sav_nome
040600041125     C                   MOVE      St_TA0017     sav_date
040700041125     C                   MOVE      St_TA0019     sav_time
040800010115      * Primo tentativo rep. da codice + qualificatore
040900041125     C                   MOVEL     St_TA0004     WCOD
041000041125     C                   MOVE      St_TA0007     WCOD
041100041203     c                   end
041200041203      *    Bolle
041300041203     c                   if        tipfil = 'IFCSUM'
041400041125     C                   MOVEL     rrIFCSUM      EDTT00DS
041500041125      * dati da riportare sul file x identificare il messaggio
041600041125     c                   move      Sm_TA1001     Sav_Docum
041700041125     C                   MOVE      Sm_TA1004     sav_nome
041800041125     C                   MOVE      Sm_TA0017     sav_date
041900041125     C                   MOVE      Sm_TA0019     sav_time
042000041125      * Primo tentativo rep. da codice + qualificatore
042100041125     C                   MOVEL     Sm_TA0004     WCOD
042200050914      **
042300050914      **  se qualificatore senza niente --> Blank
042400050914     c                   if        Sm_TA0007 = *blank
042500050914      **   imposta la nazione
042600050914     c                   z-add     nrrec         rec_pos           9 0
042700050914     c                   move      *blank        Naz_mitt          3
042800050914      *  Restituisce la Nazione con cui agganciare l'UNB su tab.:PT
042900050914     c                   call      'TRTC100R1'
043000050914     c                   parm                    rec_pos
043100050914     c                   parm                    Naz_mitt
043200050914     C                   MOVEl     Naz_mitt      Sm_TA0007
043300050914     c                   end
043400050914      *
043500041125     c                   end
043600050914      *
043700050914     C                   MOVE      Sm_TA0007     WCOD
043800041203      *
043900041125     C                   MOVEL     '20'          sav_date
044000050914      *
044100010115     C                   Z-ADD     1             X
044200010115     C     WCOD          LOOKUP    CPT(X)                                 33
044300010123      *  Prova a cercarlo con chiave parziale
044400010115     C                   If        *IN33 = *off
044500010123     C                   eval      X = 1
044600041125     c                   if        tipfil = 'IFTSTA'
044700041125     C                   eval      W035A = %subst(St_TA0004:1:31)
044800041203     c                   end
044900041203     c                   if        tipfil = 'IFCSUM'
045000041125     C                   eval      W035A = %subst(Sm_TA0004:1:31)
045100041125     c                   end
045200010115     C     W035A         lookup    CPTparz(X)                             33
045300010115     C                   Endif
045400041115      *
045500010115      *  Salva dalla tabella "PT" il cliente solo se è stata presa direttamente
045600010115      *   o in modo parziale.
045700010115     C                   If        *IN33 = *on
045800041117     C                   move      'OK'          Partner_OK                     tutto OK
045900010115     C                   MOVEL     DPT(X)        EDIDSPT
046000010123     C                   MOVEL     DPU(X)        EDIDSPU
046001111017     C                   MOVEL     DPZ(X)        EDIDSPZ
046100041117     C                   MOVEL     §PTLNP        sav_Lnp
046200041117     C                   z-add     §PTKSC        sav_ksc
046300111017     C*********          move      §PUsum        sav_Map_SUM
046400111017     C*********          move      §PUsta        sav_Map_STA
046500010123      *
046501111017     C                   move      §PZPGMRCZB    sav_Map_SUM
046502111017     C                   move      §PZPGMRCZS    sav_Map_STA
046503111017      *
046600010123     C                   Else
046700041117      *
046800041115      * se lo trova deve segnalare l'errore x Problemi di reperimento del Partner
046900041117     C                   move      'ER'          Partner_OK                     errore
047000041117     c                   move      'SI'          Err_su_msg
047100041117      *
047200041117      *  Problemi di reperimento del Partner
047300010123     C                   movel     ERR(1)        DesERR
047400010123     C                   movel     Wcod          WValor
047500010413     C                   clear                   WValo1
047600041118     C                   move      '01'          codmsgerr
047700041117      **              *--------------------------*
047800010123     C                   Exsr      STPERR
047900041117      **              *--------------------------*
048000010115     C                   Endif
048100041117      *
048200041117      *  Poichè in tabella "MS" il numeratore funziona come un semaforo in modo che
048300041117      *  i programmi di filiale possano elaborare solo i messaggi precedenti al nr.di tabella.
048400041117      * Alla prima testata il numero è quello preso direttamente in tabella poi verrà
048500041117      * incrementato.
048600041117      * A chiusura programma si dovrà riportare i numeratori sulla tabella incrementati di 1
048700041117      * x mantenere la logica sopra dichiarata.
048800041125      *  Se Status
048900041125     c                   if        tipfil = 'IFTSTA'
049000041125      *
049100041117     c                   IF        Sav_Docum = '784'
049200041117     c                   if        Prima_TST784  = 0
049300041117     c                   eval      Prima_TST784  = 1
049400041117     c                   else
049500041117     C                   eval      Sav_nr784 = Sav_nr784 + 1
049600041117     c                   end
049700041117     c                   END
049800041117      *
049900041117     c                   IF        Sav_Docum = '781'
050000041117     c                   if        Prima_TST781  = 0
050100041117     c                   eval      Prima_TST781  = 1
050200041117     c                   else
050300041117     C                   eval      Sav_nr781 = Sav_nr781 + 1
050400041117     c                   end
050500041117     c                   END
050600041118      *
050700041125     c                   END
050800041125      *
050900041125      *  Se Bolle
051000041125     c                   if        tipfil = 'IFCSUM'
051100041125      *
051200041125     c                   IF        Sav_Docum = '785'
051300041206      **
051400041125     c                   if        Prima_TST785  = 0
051500041125     c                   eval      Prima_TST785  = 1
051600041125     c                   else
051700041125     C                   eval      Sav_nr785 = Sav_nr785 + 1
051800041206     c                   end
051900041203      **
052000041203      **           - - - - - - - - - - - - - - - - - - - - - - - -
052100041203      * imposta la LNP e l'HUB di partenza su schiera
052200041203      * per gestire in seguito il lancio delle azioni di ricezione in filiale
052300041203      * dei messaggi. (Occorre fare questo poichè ciascun User può gestire delle linee
052400041203      * di partenza e non tutte quidni non sarebbe facile decodificare correttamente i
052500041203      * messaggi).
052600041203     C                   clear                   FNLV55DS
052700041203     C                   movel     'P'           D55tpt
052800041203     C                   move      sav_lnp       D55lin
052900041203     C                   move      *date         d55drf
053000041203     C                   CALL      'FNLV55R'
053100041203     C                   parm                    FNLV55DS
053200041203     c                   clear                   HubP              3 0
053300041203     c                   if        d55err = *blanks
053400041203     c                   movel     d55tfp        HubP
053500041203     c                   end
053600041203     c                   z-add     1             nrx
053700041203     c     sav_lnp       lookup    lnpX(nrX)                              34
053800041203     c                   if        *in34 = *Off
053900041203     c     *zeros        lookup    lnpX(nrX)                              34
054000041203     c                   end
054100041203     c                   z-add     sav_lnp       lnpX(nrX)
054200070104     c                   move      sav_Map_SUM   SumX(nrX)
054300041203     c                   z-add     HubP          HubX(nrX)
054400041222     c                   if        Sav_nr785 > NumX(nrX)
054500041203     c                   z-add     Sav_nr785     NumX(nrX)
054600041222     c                   end
054700041203      **           - - - - - - - - - - - - - - - - - - - - - - - -
054800041125     c                   END
054900041125      *
055000070504      *
055100070504     c                   IF        Sav_Docum = '350'
055200070504      **
055300070504     c                   if        Prima_TST350  = 0
055400070504     c                   eval      Prima_TST350  = 1
055500070504     c                   else
055600070504     C                   eval      Sav_nr350 = Sav_nr350 + 1
055700070504     c                   end
055800070504      **
055900070504      **           - - - - - - - - - - - - - - - - - - - - - - - -
056000070504      * imposta la LNP e l'HUB di partenza su schiera
056100070504      * per gestire in seguito il lancio delle azioni di ricezione in filiale
056200070504      * dei messaggi. (Occorre fare questo poichè ciascun User può gestire delle linee
056300070504      * di partenza e non tutte quidni non sarebbe facile decodificare correttamente i
056400070504      * messaggi).
056500070504     C                   clear                   FNLV55DS
056600070504     C                   movel     'P'           D55tpt
056700070504     C                   move      sav_lnp       D55lin
056800070504     C                   move      *date         d55drf
056900070504     C                   CALL      'FNLV55R'
057000070504     C                   parm                    FNLV55DS
057100070504     c                   clear                   HubP              3 0
057200070504     c                   if        d55err = *blanks
057300070504     c                   movel     d55tfp        HubP
057400070504     c                   end
057500070504     c                   z-add     1             nrx
057600070504     c     sav_lnp       lookup    lnpX(nrX)                              34
057700070504     c                   if        *in34 = *Off
057800070504     c     *zeros        lookup    lnpX(nrX)                              34
057900070504     c                   end
058000070504     c                   z-add     sav_lnp       lnpX(nrX)
058100070504     c                   move      sav_Map_SUM   SumX(nrX)
058200070504     c                   z-add     HubP          HubX(nrX)
058300070504     c                   if        Sav_nr350 > NumX(nrX)
058400070504     c                   z-add     Sav_nr350     NumX(nrX)
058500070504     c                   end
058600070504      **           - - - - - - - - - - - - - - - - - - - - - - - -
058700070504     c                   END
058800061214      *
058900070504      *
059000061214     c                   IF        Sav_Docum = '730'
059100061214      **
059200061214     c                   if        Prima_TST730  = 0
059300061214     c                   eval      Prima_TST730  = 1
059400061214     c                   else
059500061214     C                   eval      Sav_nr730 = Sav_nr730 + 1
059600061214     c                   end
059700061214      **
059800061214      **           - - - - - - - - - - - - - - - - - - - - - - - -
059900061214      * imposta la LNP e l'HUB di partenza su schiera
060000061214      * per gestire in seguito il lancio delle azioni di ricezione in filiale
060100061214      * dei messaggi. (Occorre fare questo poichè ciascun User può gestire delle linee
060200061214      * di partenza e non tutte quidni non sarebbe facile decodificare correttamente i
060300061214      * messaggi).
060400061214     C                   clear                   FNLV55DS
060500061214     C                   movel     'P'           D55tpt
060600061214     C                   move      sav_lnp       D55lin
060700061214     C                   move      *date         d55drf
060800061214     C                   CALL      'FNLV55R'
060900061214     C                   parm                    FNLV55DS
061000061214     c                   clear                   HubP              3 0
061100061214     c                   if        d55err = *blanks
061200061214     c                   movel     d55tfp        HubP
061300061214     c                   end
061400061214     c                   z-add     1             nrx
061500061214     c     sav_lnp       lookup    lnpX(nrX)                              34
061600061214     c                   if        *in34 = *Off
061700061214     c     *zeros        lookup    lnpX(nrX)                              34
061800061214     c                   end
061900061214     c                   z-add     sav_lnp       lnpX(nrX)
062000070104     c                   move      sav_Map_SUM   SumX(nrX)
062100061214     c                   z-add     HubP          HubX(nrX)
062200061214     c                   if        Sav_nr730 > NumX(nrX)
062300061214     c                   z-add     Sav_nr730     NumX(nrX)
062400061214     c                   end
062500061214      **           - - - - - - - - - - - - - - - - - - - - - - - -
062600061214     c                   END
062700061214      *
062800041125     c                   END
062900041125      *
063000041118      *  per differenziare i messaggi temporalmente
063100041118     C                   TIME                    ST00_ora          6 0
063200041118      *
063300010115     C                   Endsr
063400010123     C*----------------------------------------------------------------
063500041117     C*?  Deve Aggiornare il messaggio appena letto se OK oppure ER
063600010123     C*----------------------------------------------------------------
063700041117     C     AGG_record    BEGSR
063800041117      *              *-------------------------------*
063900041117      *  Aggiorna i campi gestione Bartolini
064000041117      *
064100041117     C                   eval      RRELABORA = Partner_OK
064200041117     C                   eval      RRNOMEDOC = Sav_Nome
064300041117     C                   eval      RRDATA    = Sav_Date
064400041206     C     100           mult      Sav_Time      RRORA
064500041117     C                   eval      RRLINEA   = Sav_Lnp
064600041118     C                   eval      RRDATELA  = *date
064700041118     C                   eval      RRORAELA  = ST00_ora
064800041125      *
064900041125     c                   if        tipfil = 'IFTSTA'
065000041125     c                   if        Sav_Docum = '784'
065100041125     C                   eval      RRNUMERO  = Sav_nr784
065200041201     C                   eval      RRTIPO    = '784'
065300061214     c                   end
065400061214     c                   if        Sav_Docum = '781'
065500041125     C                   eval      RRNUMERO  = Sav_nr781
065600041201     C                   eval      RRTIPO    = '781'
065700041125     c                   end
065800041125     c                   end
065900041117      *
066000041125     c                   if        tipfil = 'IFCSUM'
066100041125     c                   if        Sav_Docum = '785'
066200041125     C                   eval      RRNUMERO  = Sav_nr785
066300041201     C                   eval      RRTIPO    = '785'
066400041117     c                   end
066500070504     c                   if        Sav_Docum = '350'
066600070504     C                   eval      RRNUMERO  = Sav_nr350
066700070504     C                   eval      RRTIPO    = '350'
066800070504     c                   end
066900061214     c                   if        Sav_Docum = '730'
067000061214     C                   eval      RRNUMERO  = Sav_nr730
067100061214     C                   eval      RRTIPO    = '730'
067200061214     c                   end
067300041125     c                   end
067400041117      *
067500041117     C                   eval      num_progr = num_progr + 1
067600041117     C                   eval      RRPROGR   = num_progr
067700041221     c                   move      rrNUMERO      four_digit        4
067800041125      *
067900041125      *              *-------------------------------*
068000041125     c                   if        tipfil = 'IFTSTA'
068100041221     c                   movel     four_digit    rrIFTSTA
068200041125     C                   UPDATE    rcvsta
068300041125     c                   end
068400041125     c                   if        tipfil = 'IFCSUM'
068500041221     c                   movel     four_digit    rrIFCSUM
068600041125     C                   UPDATE    rcvsum
068700041125     c                   end
068800041117      *              *-------------------------------*
068900041125      *
069000041117     C                   Endsr
069100041117     C*----------------------------------------------------------------
069200041117     C*? STPERR: STAMPA ERRORE
069300041117     C*----------------------------------------------------------------
069400041117     C     STPERR        BEGSR
069500041117     C*
069600010123     C     *INOF         IFEQ      '1'
069700041125     C*
069800041125     c                   if        tipfil ='IFTSTA'
069900041125     C                   EXCEPT    TESSTA
070000041125     c                   else
070100041125     C                   EXCEPT    TESSUM
070200041125     c                   end
070300041125     C*
070400010123     C                   EXCEPT    ERRCLI
070500010123     C                   END
070600041125     C*
070700010123     C                   EXCEPT    DETER
070800010413     C*
070900010413     C     Wvalo1        IFne      *blank
071000010413     C                   EXCEPT    MAXDET
071100010413     C                   End
071200010123     C*
071300010321     C*  invia messaggio di errore alle code operatore e di programma
071400040219      * una sola volta x msg
071500041125     c                   if        primo_sum = '0' and tipfil = 'IFCSUM' or
071600041125     c                             primo_sta = '0' and tipfil = 'IFTSTA'
071700041118     C                   call      'TRTC100MSG'
071800010321     C                   PARM                    WCOD
071900010906     C                   PARM                    USRMSG
072000041118     C                   PARM                    codmsgerr
072100041125     C                   PARM                    tipfil
072200041125     c                   if        tipfil = 'IFCSUM'
072300041125     c                   move      '1'           primo_sum
072400041125     c                   else
072500041125     c                   move      '1'           primo_sta
072600041125     c                   end
072700040219     c                   end
072800010321     C*
072900010123     C                   ENDSR
073000010123     C*----------------------------------------------------------------
073100041117     C*  INVIO Messaggio
073200041117     C*----------------------------------------------------------------
073300041125     C     SND_Msg_sta   BEGSR
073400010413      *
073500010703      * SNDPGMMSG di avviso errore TOUSR(EDPAB)
073600010703     C                   clear                   MSGDS
073700041117     C                   MOVEL     CMD(3)        MSGDS
073800010703     C                   eval      MSGDS = %trim(MSGDS) +
073900010905     C                             ' TOUSR('+
074000010905     C                             %trim(USRMSG) +')'
074100010703     C                   Z-ADD     100           LENGH
074200010703     C                   CALL      'QCMDEXC'                            99
074300010703     C                   PARM                    MSGDS
074400010703     C                   PARM                    LENGH
074500010703      *
074600010703      * SNDBRKMSG di avviso errore TOUSR(DSP01)
074700050221     c                   if        MSGd01 = 'S'
074800010703     C                   clear                   MSGDS
074900041117     C                   MOVEL     CMD(4)        MSGDS
075000010703     C                   eval      MSGDS = %trim(MSGDS) +
075100010703     C                             ' TOMSGQ(DSP01)'
075200010703     C                   Z-ADD     100           LENGH
075300010703     C                   CALL      'QCMDEXC'                            99
075400010703     C                   PARM                    MSGDS
075500010703     C                   PARM                    LENGH
075600050221     c                   end
075700010123      *
075800010123     C                   Endsr
075900041125     C*----------------------------------------------------------------
076000041125     C*  INVIO Messaggio
076100041125     C*----------------------------------------------------------------
076200041125     C     SND_Msg_sum   BEGSR
076300041125      *
076400041125      * SNDPGMMSG di avviso errore TOUSR(EDPAB)
076500041125     C                   clear                   MSGDS
076600041125     C                   MOVEL     CMD(5)        MSGDS
076700041125     C                   eval      MSGDS = %trim(MSGDS) +
076800041125     C                             ' TOUSR('+
076900041125     C                             %trim(USRMSG) +')'
077000041125     C                   Z-ADD     100           LENGH
077100041125     C                   CALL      'QCMDEXC'                            99
077200041125     C                   PARM                    MSGDS
077300041125     C                   PARM                    LENGH
077400041125      *
077500041125      * SNDBRKMSG di avviso errore TOUSR(DSP01)
077600050221     c                   if        MSGd01 = 'S'
077700041125     C                   clear                   MSGDS
077800041125     C                   MOVEL     CMD(6)        MSGDS
077900041125     C                   eval      MSGDS = %trim(MSGDS) +
078000041125     C                             ' TOMSGQ(DSP01)'
078100041125     C                   Z-ADD     100           LENGH
078200041125     C                   CALL      'QCMDEXC'                            99
078300041125     C                   PARM                    MSGDS
078400041125     C                   PARM                    LENGH
078500050221     c                   end
078600041125      *
078700041125     C                   Endsr
078800001003      *----------------------------------------------------------------
078900041129     C*? Pulisce i vecchi records.
079000001003     C*----------------------------------------------------------------
079100041129     C     Pulizia_old   BEGSR
079200041129      *
079300041129     c                   move      *date         dataOGGI          8 0
079400041129      *
079500050823      * ?------------------------------------------------------------------ */
079600050823      * ?Prima venivano presi dall'area dati PULIRCV adesso è attiva la
079700050823      * ?tabella EDTAB --> GG con i giorni e l'ultima data pulizia.
079800050823      * ?------------------------------------------------------------------ */
079900041129      *   Vengono puliti i records + vecchi di 15 giorni
080000050823     c***  *dtaara       define                  puliRCV
080100050823     c***  *lock         in(E)     puliRCV
080200050823     c***                IF        %Error
080300050823     c***                z-add     15            GG_Piu_old
080400050823     c***                z-add     dataOGGI      dataPulizia
080500050823     c***                end
080600050823      * ?------------------------------------------------------------------ */
080700050823      * ?Call x reperire l'ultima data pulizia ed i giorni oltre i quali
080800050823      * ?pulire i records. (x non aggiornare tabella KPJBU deve essere pulita)
080900050823      * ?------------------------------------------------------------------ */
081000050823     c                   clear                   kpjbu
081100050823     c                   call      'TRTC96BR'
081200050823     c                   parm                    kpjba
081300050823     c                   movel     kpjbu         edidsGG
081400050823      **
081500050823      * ?se x assurdo la tabella fosse vuota  imposta fisso a 15 gg.
081600050824     c                   IF        §ggGGR = '000' or §ggGGR = *blank
081700060215     c                   z-add     30            GG_Piu_old
081800050823     c                   z-add     dataOGGI      dataPulizia
081900050823     c                   else
082000050823      * ?i campi su DS sono Alfa
082100050823     c                   move      §ggDTR        dataPulizia
082200050823     c                   move      §ggGGR        GG_Piu_old
082300050823     c                   end
082400050823      * ?     /* ---------------------------------------------- */
082500041129      *
082600041129      *  solo se la data presente sull'area Dati
082700041129     C                   IF        dataOGGI <> DataPulizia
082800041129      *
082900041129     c                   move      *date         data_iso
083000041129     c     data_iso      subdur    GG_Piu_old:*d data_old
083100041129     c                   move      data_old      data08            8 0
083200041129      *
083300041129     C/EXEC SQL
083400041129     C+ DELETE   FROM RCVIFTSTA  where
083500041129     C+ RRDATELA <  :DATA08      and
083600080327     C+ RRELABORA IN ('RX', 'ER')
083700041129     C/END-EXEC
083800041129      *
083900041129     C/EXEC SQL
084000041129     C+ DELETE   FROM RCVIFCSUM  where
084100041129     C+ RRDATELA <  :DATA08      and
084200080327     C+ RRELABORA IN ('RX', 'ER')
084300041129     C/END-EXEC
084400041129      *  quindi aggiorna con la data di oggi l'area dati
084500041129      *   x non rieseguire in seguito ancora la pulizia che deve essere giornaliera
084600050823     c****               z-add     dataOGGI      dataPulizia
084700050823     c****               out       puliRCV
084800050823      * ?------------------------------------------------------------------ */
084900050823      * ?Call x aggiornare l'ultima data pulizia
085000050823      * ? (x aggiornare tabella passa in KPJBU la data RCV)
085100050823      * ?------------------------------------------------------------------ */
085200050823     c                   clear                   edidsGG
085300050823     c                   move      dataOGGI      §ggDTR
085400050823     c                   clear                   kpjbu
085500050823     c                   movel     edidsGG       kpjbu
085600050823     c                   call      'TRTC96BR'
085700050823     c                   parm                    kpjba
085800050823      **
085900041129     C                   EndIF
086000041129      *
086100041129     C                   Endsr
086200041129      *----------------------------------------------------------------
086300041129     C*? *INZSR - OPERAZIONI INIZIALI
086400041129     C*----------------------------------------------------------------
086500041129     C     *INZSR        BEGSR
086600041129     C*
086700010320     C* Riceve un parametro per capire se si trova in ambiente di produzione
086800010320     C*  o in ambiente di test
086900010320     C     *entry        plist
087000010320     C                   parm                    flgtst            1
087100010906     C                   parm                    UsrMsg           10
087200041125     C                   parm                    TipFil           10
087300041125     C*
087400041125     C*  Identifica che messaggio deve trattare
087500041125     c                   select
087600041125     c                   when      tipfil = 'IFTSTA'
087700041125     c                   open      rcviftsta1
087800041125     c                   when      tipfil = 'IFCSUM'
087900041125     c                   open      rcvifcsum1
088000041125     c                   endsl
088100010320     C*
088200001003     C* Definisco chiavi di accesso
088300041115     C     Krcv          KLIST
088400041115     C                   KFLD                    Blk_RCV
088500041115     C                   move      *blank        Blk_RCV           2
088600001003      *
088700041117     C     Ktab          KLIST
088800041117     C                   KFLD                    tabcod
088900041117     C                   KFLD                    tabkey
089000041117      *
089100001003      * RECUPERO DATI DELL'UTENTE
089200001003     C                   Z-ADD     1             CODUT
089300001003     C                   CALL      'XPARUT'
089400001003     C                   PARM                    UTEDSE0F
089500010123     C                   MOVEL     RAGUT         RSUT             20
089600010123     C                   Seton                                        OF
089700041125     c                   move      '0'           primo_sum         1
089800041125     c                   move      '0'           primo_sta         1
089900001003      *
090000001003      * Caricamento Tabella Partner esteri
090100001003     C                   Z-ADD     0             X
090200010307      *
090300020904      * Punta alle Tabelle in Lista di Librerie
090400010319     C                   clear                   MSGDS
090500041117     C                   MOVEL     CMD(2)        MSGDS
090600010319     C                   Z-ADD     100           LENGH
090700010307     C                   CALL      'QCMDEXC'
090800010307     C                   PARM                    MSGDS
090900010307     C                   PARM                    LENGH
091000010307      *
091100010307      * Carica la parte BARTOLINI
091200010307     C                   OPEN      EDTAB01L
091300001003     C                   MOVEL     'PT'          TABCOD
091400001006     C     TABCOD        SETLL     EDTAB01L
091500041117     C     TABCOD        READE(N)  EDTAB01L
091600001003     C                   DOW       not %EOF(EDTAB01L)
091700001003     C     TABFLG        IFEQ      *BLANKS
091800001003     C                   ADD       1             X
091900001003     C                   MOVEL     TABKEY        CPT(X)
092000001003     C                   eval      CPTparz(X) = %subst(TABKEY:1:30)
092100001003     C                   MOVEL     TABUNI        DPT(X)
092200001003     C                   End
092300041117     C     TABCOD        READE(N)  EDTAB01L
092400001003     C                   ENDdo
092500010123     C*
092600010123     C                   MOVEL     'PU'          TABCOD
092700010123     C     TABCOD        SETLL     EDTAB01L
092800041117     C     TABCOD        READE(N)  EDTAB01L
092900010123     C                   DOW       not %EOF(EDTAB01L)
093000010123     C     TABFLG        IFEQ      *BLANKS
093100010123     C                   Z-ADD     1             X
093200010123     C     TABKEY        LOOKUP    CPT(X)                                 32
093300010123     C   32              MOVEL     TABUNI        DPU(X)
093400010123     C                   END
093500041117     C     TABCOD        READE(N)  EDTAB01L                               30
093600010123     C                   END
093700041117      *
093701111017     C*  Modificato x sicurezza anche se non servirà +.
093702111017     C                   MOVEL     'PZ'          TABCOD
093703111017     C     TABCOD        SETLL     EDTAB01L
093704111017     C     TABCOD        READE(N)  EDTAB01L
093705111017     C                   DOW       not %EOF(EDTAB01L)
093706111017     C     TABFLG        IFEQ      *BLANKS
093707111017     C                   Z-ADD     1             X
093708111017     C     TABKEY        LOOKUP    CPT(X)                                 32
093709111017     C   32              MOVEL     TABUNI        DPZ(X)
093710111017     C                   END
093711111017     C     TABCOD        READE(N)  EDTAB01L                               30
093712111017     C                   END
093713111017      *
093800041117      * Prende i numeratori dalla tabella x num. di sede
093900041117     c                   clear                   sav_nr784
094000041117     c                   clear                   sav_nr781
094100041125     c                   clear                   sav_nr785
094200061214     c                   clear                   sav_nr730
094300070504     c                   clear                   sav_nr350
094400041117      *
094500041125      *  Stati di consegna - IFTSTA
094600041125     c                   if        tipfil = 'IFTSTA'
094700041125      *
094800041117     C                   MOVEL(p)  'MS'          TABCOD
094900041117     c                   MOVEL(p)  '784SEDE'     TABKEY
095000041117     C     Ktab          chain(n)  EDTAB01L
095100041117     C                   if        %found(EDTAB01L)
095200041117     C                   MOVEL     TABUNI        edidsms
095300041118     c                   z-add     §MSNUM        sav_nr784
095400041222     c                   if        sav_nr784 = 0
095500041118     c                   z-add     1             sav_nr784
095600041118     c                   end
095700041118     c                   end
095800010307     C*
095900041117     C                   MOVEL(p)  'MS'          TABCOD
096000041117     c                   MOVEL(p)  '781SEDE'     TABKEY
096100041117     C     Ktab          chain(n)  EDTAB01L
096200041117     C                   if        %found(EDTAB01L)
096300041117     C                   MOVEL     TABUNI        edidsms
096400041118     c                   z-add     §MSNUM        sav_nr781
096500041222     c                   if        sav_nr781 = 0
096600041118     c                   z-add     1             sav_nr781
096700041118     c                   end
096800041117     c                   end
096900041117     C*
097000041125     c                   endIF
097100041125      *
097200041125      *  Bolle - IFCSUM
097300041125     c                   if        tipfil = 'IFCSUM'
097400041125      *
097500041125     C                   MOVEL(p)  'MS'          TABCOD
097600041125     c                   MOVEL(p)  '785SEDE'     TABKEY
097700041125     C     Ktab          chain(n)  EDTAB01L
097800041125     C                   if        %found(EDTAB01L)
097900041125     C                   MOVEL     TABUNI        edidsms
098000041125     c                   z-add     §MSNUM        sav_nr785
098100041222     c                   if        sav_nr785 = 0
098200041125     c                   z-add     1             sav_nr785
098300041125     c                   end
098400041125     c                   end
098500070504      *
098600070504     C                   MOVEL(p)  'MS'          TABCOD
098700070504     c                   MOVEL(p)  '350SEDE'     TABKEY
098800070504     C     Ktab          chain(n)  EDTAB01L
098900070504     C                   if        %found(EDTAB01L)
099000070504     C                   MOVEL     TABUNI        edidsms
099100070504     c                   z-add     §MSNUM        sav_nr350
099200070504     c                   if        sav_nr350 = 0
099300070504     c                   z-add     1             sav_nr350
099400070504     c                   end
099500070504     c                   end
099600061214     C*
099700061214     C                   MOVEL(p)  'MS'          TABCOD
099800061214     c                   MOVEL(p)  '730SEDE'     TABKEY
099900061214     C     Ktab          chain(n)  EDTAB01L
100000061214     C                   if        %found(EDTAB01L)
100100061214     C                   MOVEL     TABUNI        edidsms
100200061214     c                   z-add     §MSNUM        sav_nr730
100300061214     c                   if        sav_nr730 = 0
100400061214     c                   z-add     1             sav_nr730
100500061214     c                   end
100600061214     c                   end
100700041125     C*
100800041125     c                   endIF
100900050221     C*
101000050825      * ?------------------------------------------------------------------ */
101100050825      * ?Call x reperire se deve mandare Msg a DSP01
101200050825      * ? (x non aggiornare tabella KPJBU deve essere pulita)
101300050825      * ?------------------------------------------------------------------ */
101400050825     c                   clear                   kpjbu
101500050825     c                   call      'TRTC95BR'
101600050825     c                   parm                    kpjba
101700050825     c                   movel     kpjbu         edidsER
101800050825     c                   eval      MSGd01 = §ERD01
101900050825      **
102000010123     C* RECUPERO DATA E ORA
102100010123     C                   TIME                    WHHD12           12 0
102200010123     C                   MOVE      WHHD12        DATA6             6 0
102300010123     C                   MOVEL     DATA6         DATA              6 0
102400010123     C                   TIME                    WORA              6 0
102500010123     C*
102600001003     C                   ENDSR
102700001002      *****************************************************************
102800041125     OQSYSPRT   E            TESSTA           01
102900010123     O                       RSUT                20
103000041118     O                                           46 '** ERRORI RICEZ. Status da'
103100041118     O                                              ' EDI in Sede ->RCVIFTSTA *'
103200041118     O                                              '*'
103300041118     O                       DATA                89 '  /  /  '
103400041118     O                                           95 'Pag.:'
103500041118     O                       PAGE          Z    100
103600041125     O          E            TESSTA         1 02
103700041118     O                       WORA                89 '  :  :  '
103800041118     O                                          100 'TRTC100R'
103900010123     O*------------------------------------
104000041125     O          E            TESSUM           01
104100041125     O                       RSUT                20
104200041125     O                                           46 '** ERRORI RICEZ. Bolle  da'
104300041125     O                                              ' EDI in Sede ->RCVIFCSUM *'
104400041125     O                                              '*'
104500041125     O                       DATA                89 '  /  /  '
104600041125     O                                           95 'Pag.:'
104700041125     O                       PAGE          Z    100
104800041125     O          E            TESSUM         1 02
104900041125     O                       WORA                89 '  :  :  '
105000041125     O                                          100 'TRTC100R'
105100041125     O*------------------------------------
105200010123     O          E            ERRCLI         1
105300010123     O                                           15 'CODICE CLIENTE:'
105400010123     O                       §PTKSC        Z     23
105500010123     O                       WCOD                60
105600010123     O*------------------------------------
105700010123     O          E            DETER          1
105800010123     O                       DESERR              78
105900010123     O                       WVALOR             113
106000010413     O*------------------------------------
106100010413     O          E            MAXDET         1
106200010413     O                       WVALO1             113
106300010123     O*------------------------------------
106400010123** (ERR)
106500010123Il  Partner  NON è stato trovato nella Tabella "PT" con questo codice:
106600010124** (CMD)
106700010402DLTOVR FILE(EDTAB01L)
106800020904OVRDBF FILE(EDTAB01L) TOFILE(*LIBL/EDTAB01L)
106900041118SNDMSG MSG('ATTENZIONE Ricez. EDI - ERRORI in   RCVIFTSTA in GRU')
107000041117SNDBRKMSG MSG('ATTENZIONE Ricez. EDI - ERRORI in   RCVIFTSTA in GRU')
107100041125SNDMSG MSG('ATTENZIONE Ricez. EDI - ERRORI in   RCVIFCSUM in GRU')
107200041125SNDBRKMSG MSG('ATTENZIONE Ricez. EDI - ERRORI in   RCVIFCSUM in GRU')
