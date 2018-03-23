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
002800121105     D CPT             S             35    DIM(200)
002900121105     D CPTparz         S             35    DIM(%elem(CPT))
003000121105     D DPT             S             90    DIM(%elem(CPT))
003100121105     D DPU             S             90    DIM(%elem(CPT))
003200121105     D DPZ             S             90    DIM(%elem(CPT))
003300121105     D TRUL0SDS      E DS
003400121112     d kpjbusav        S                   like(Kpjbu)
003500050914      *
003600050914      * numero relativo di record
003700050914     D SRECDS          DS
003800050914     D  NRREC                397    400B 0
003900010123      *---------------------------------------------------------------------*
004000010123      * Comandi
004100010123      *---------------------------------------------------------------------*
004200010123     D ERR             S             78    DIM(5) CTDATA PERRCD(1)
004300041125     D CMD             S             78    DIM(6) CTDATA PERRCD(1)
004400041203      *---------------------------------------------------------------------*
004500041203      * Schiere
004600041203      *---------------------------------------------------------------------*
004700041203      * x lancio azione di traduzione in filiale
004800041203     D LnpX            S              3  0 DIM(999) inz
004900041203     D HubX            S              3  0 DIM(999) inz
005000041203     D NumX            S              7  0 DIM(999) inz
005100111017     D SumX            S              6    DIM(999) inz
005200000929      *---------------------------------------------------------------------*
005300001005      * Flag oprativi di work
005400001005      *---------------------------------------------------------------------*
005500001003     D WCOD            S             35
005600020904     D W035A           S             35
005700010123     D DesErr          S             78
005800010123     D WValor          S             35
005900010413     D WValo1          S             35
006000041117      *---------------------------------------------------------------------*
006100041117     D Sav_nome        S             35
006200041117     D sav_date        S              8  0 INZ(0)
006300041117     D sav_time        S              4  0 INZ(0)
006400041117     D Sav_ksc         s              7  0 INZ(0)
006500041117     D Sav_Lnp         s              3  0 INZ(0)
006600041125     D Sav_Nr785       s              7  0 INZ(0)
006700070504     D Sav_Nr350       s              7  0 INZ(0)
006800061214     D Sav_Nr730       s              7  0 INZ(0)
006900041117     D Sav_Nr784       s              7  0 INZ(0)
007000041117     D Sav_Nr781       s              7  0 INZ(0)
007100041125     D Prima_TST785    s              1  0 INZ(0)
007200070504     D Prima_TST350    s              1  0 INZ(0)
007300061214     D Prima_TST730    s              1  0 INZ(0)
007400041125     D Prima_TST784    s              1  0 INZ(0)
007500041117     D Prima_TST781    s              1  0 INZ(0)
007600041117     D Num_Progr       s              5  0 INZ(0)
007700041125     D Sav_Docum       s              3    INZ('  ')
007800041117     D Partner_OK      S              2    INZ('  ')
007900041117     D Err_su_msg      S              2    INZ('  ')
008000041118     D CodMsgErr       S              2    INZ('  ')
008100041118     D Anno_04         s              4  0 INZ(0)
008200050221     D MSGd01          S              1    INZ(' ')
008300111017     D Sav_Map_SUM     s              6    INZ(' ')
008400111017     D Sav_Map_STA     s              6    INZ(' ')
008500041129      *
008600041129     D data_iso        s               d   datfmt(*iso)
008700041129     D data_old        s               d   datfmt(*iso)
008800010115      *---------------------------------------------------------------------*
008900010319     D  MSGDS          S            100
009000010124     D  LENGH          S             15  5
009100001003     D X               s              3  0 INZ(0)
009200041203     D nrX             s              3  0 INZ(0)
009300041203     D svX             s              3  0 INZ(0)
009400000120      *---------------------------------------------------------------------*
009500000120      * DS
009600000120      *---------------------------------------------------------------------*
009700041125     D EDST00DS      E DS                  prefix(ST_)                          Sta
009800041125     D EDTT00DS      E DS                  prefix(SM_)                          Sum
009900041125      *
010000010123     D EDIDSPT       E DS
010100010123     D EDIDSPU       E DS
010200111017     D EDIDSPZ       E DS
010300041117     D EDIDSMS       E DS
010400041203      * DS per FNLV55R - Reperimento Terminal di Partenza e di Arrivo
010500041203     D FNLV55DS      E DS
010600041202      *
010700041202     D kpjba         E DS                  inz
010800041202      *
010900041202     D                SDS
011000041202     D  §PGMname               1     10
011100041202     D  §USRname             254    263
011200000120      *---------------------------------------------------------------------*
011300050825     D EDIDSER       e ds
011400050823     D EDIDSGG       e ds
011500050823      **
011600041129     D PuliRCV         DS            30
011700041129     d  DataPulizia                   8  0
011800041129     d  GG_Piu_old                    3  0
011900050823     d**Free_Field                   19
012000041129      *---------------------------------------------------------------------*
012100000120     D UTEDSE0F      E DS
012200000120     D  TCU                  398    697    DIM(50)                              Flg 8 tp.conto
012300000120     D  KCU                  698    847P 0 DIM(50)  PACKEVEN                    Capiconto
012400000929      *---------------------------------------------------------------------*
012500010123      *                    .. 5   8  : tipo record letto
012600041221     D DSdati          DS          1950
012700041221     D  SUMNUM                 1      4
012800041221     D  SUMTPR                 5      8
012900000929     D  SUMINR                 5      6
013000041117      *---------------------------------------------------------------------*
013100041117     D Digits          C                   CONST('0123456789')
013200000120      *---------------------------------------------------------------------*
013300000120      * Ciclo principale
013400000120      *---------------------------------------------------------------------*
013500041125     c*  esegue un ciclo oppure l'altro a seconda del tipo di msg da trattare
013600041117     c                   clear                   Err_su_msg
013700041125     C*
013800041125     c                   select
013900041125     C*
014000041125     c                   when      tipfil = 'IFCSUM'
014100041125     c                   exsr      Ciclo_SUM
014200041125     C*
014300041125     c                   when      tipfil = 'IFTSTA'
014400041125     c                   exsr      Ciclo_STA
014500041125     C*
014600041125     c                   endsl
014700041125     C*
014800041125     C                   close     EDTAB01L
014900041125      * Toglie l'OVRRIDE
015000041125     C                   clear                   MSGDS
015100041125     C                   MOVEL     CMD(1)        MSGDS
015200041125     C                   Z-ADD     100           LENGH
015300041125     C                   CALL      'QCMDEXC'
015400041125     C                   PARM                    MSGDS
015500041125     C                   PARM                    LENGH
015600041125      *
015700041129      * Prima di uscire esegue una pulizia dei vecchi records
015800041129      *     + vecchi di xx giorni
015900041129      **              *--------------------------*
016000041129     c                   exsr      Pulizia_old
016100041129      **              *--------------------------*
016200041129      *
016300041203     C                   CLEAR                   FNLV55DS
016400041203     C                   MOVEL     'C'           D55TLA
016500041203     C                   CALL      'FNLV55R'
016600041203     C                   PARM                    FNLV55DS
016700041203      *
016800041207     C                   Commit
016900041207      *
017000041125     C                   SETON                                        LR
017100041125      *---------------------------------------------------------------------*
017200041125      *  Ciclo per la codifica degli Stati
017300041125      *---------------------------------------------------------------------*
017400041125     C     Ciclo_STA     Begsr
017500041125      *
017600041117      * Lettura iniziale sul file di tutti i records allo stato "BLK"
017700041125      *
017800041125     C     Krcv          setll     RCViftsta1
017900041125     C     Krcv          reade     RCViftsta1
018000000929      *
018100010123      * Ciclo di Lettura fino a EOF
018200041115     C                   DOW       not %EOF(RCViftsta1)
018300001002      *
018400041117      *  muove i dati flat su una DS
018500001002      *                          ++++++++++++++++++++++++
018600041117     C                   MOVEL     rrIFTSTA      DSdati
018700041117      * -----------              ++++++++++++++++++++++++
018800041117      * Testata MSG
018900041117      * -----------
019000041117     C                   If        SUMTPR = 'ST00'                              TESTATA
019100041117      **              *--------------------------*
019200041117     C                   EXSR      DATI_Testata
019300041117      **              *--------------------------*
019400000929     C                   EndIF
019500041117      **
019600041117      **   deve aggiornare i records solo dopo aver esaminato la testata
019700041117      **              *--------------------------*
019800041117     c                   exsr      AGG_record
019900041117      **              *--------------------------*
020000010123      *      -----------
020100960329      * Lettura successiva
020200041115     C     Krcv          reade     RCViftsta1
020300010123     C                   ENDdo
020400041117     C*------------------
020500041117     C*  Prima di uscire deve aggiornare i numeratori 784SEDE e 781SEDE
020600041117      *  aggiorna i numeratori dalla tabella x num. di sede
020700041117     C                   MOVEL(p)  'MS'          TABCOD
020800041117     c                   MOVEL(p)  '784SEDE'     TABKEY
020900041117     C     Ktab          chain     EDTAB01L
021000041117     C                   if        %found(EDTAB01L)
021100041117     C                   MOVEL     TABUNI        edidsms
021200041117     c                   if        §msnum <> sav_nr784 or
021300041117     c                             §msnum =  sav_nr784 and Prima_TST784 = 1
021400041117     c                   eval      §MSNUM = sav_nr784 + 1
021500041117     c                   eval      §MSDAT = *date
021600041117     C                   MOVEL     edidsms       TABUNI
021700041117     c                   end
021800041117     C                   update    EDTAB000
021900041117     c                   end
022000041117     C*
022100041117     C                   MOVEL(p)  'MS'          TABCOD
022200041117     c                   MOVEL(p)  '781SEDE'     TABKEY
022300041117     C     Ktab          chain     EDTAB01L
022400041117     C                   if        %found(EDTAB01L)
022500041117     C                   MOVEL     TABUNI        edidsms
022600041117     c                   if        §msnum <> sav_nr781 or
022700041117     c                             §msnum =  sav_nr781 and Prima_TST781 = 1
022800041117     c                   eval      §MSNUM = sav_nr781 + 1
022900041117     c                   eval      §MSDAT = *date
023000041117     C                   MOVEL     edidsms       TABUNI
023100041117     c                   end
023200041117     C                   update    EDTAB000
023300041117     c                   end
023400041117      *
023500041117      *  se ci sono stati dei problemi con dei messaggi
023600041117     c                   if        Err_su_msg = 'SI'
023700041117      **              *--------------------------*
023800041125     c                   exsr      snd_msg_sta
023900041117      **              *--------------------------*
024000041117     c                   end
024100041125      *
024200041125     C                   Endsr
024300041125      *---------------------------------------------------------------------*
024400041125      *  Ciclo per la codifica delle Bolle
024500041125      *---------------------------------------------------------------------*
024600041125     C     Ciclo_SUM     Begsr
024700041125      *
024800041125      * Lettura iniziale sul file di tutti i records allo stato "BLK"
024900041125      *
025000041125     C     Krcv          setll     RCVifcsum1
025100041125     C     Krcv          reade     RCVifcsum1
025200041125      *
025300041125      * Ciclo di Lettura fino a EOF
025400041125     C                   DOW       not %EOF(RCVifcsum1)
025500041125      *  muove i dati flat su una DS
025600041125      *                          ++++++++++++++++++++++++
025700041125     C                   MOVEL     rrIFCSUM      DSdati
025800041125      * -----------              ++++++++++++++++++++++++
025900041125      * Testata MSG
026000041125      * -----------
026100041125     C                   If        SUMTPR = 'TT00'                              TESTATA
026200041125      **              *--------------------------*
026300041125     C                   EXSR      DATI_Testata
026400041203      **              *--------------------------*
026500041125     C                   EndIF
026600041125      **
026700041125      **   deve aggiornare i records solo dopo aver esaminato la testata
026800041125      **              *--------------------------*
026900041125     c                   exsr      AGG_record
027000041125      **              *--------------------------*
027100041125      *      -----------
027200041125      * Lettura successiva
027300041125     C     Krcv          reade     RCVifcsum1
027400041125     C                   ENDdo
027500041125     C*------------------
027600061214     C*  Prima di uscire deve aggiornare i numeratori 785SEDE e 730SEDE
027700070504     C*  e 350SEDE
027800041125      *  aggiorna i numeratori dalla tabella x num. di sede
027900041125     C                   MOVEL(p)  'MS'          TABCOD
028000041125     c                   MOVEL(p)  '785SEDE'     TABKEY
028100041125     C     Ktab          chain     EDTAB01L
028200041125     C                   if        %found(EDTAB01L)
028300041125     C                   MOVEL     TABUNI        edidsms
028400041125     c                   if        §msnum <> sav_nr785 or
028500041125     c                             §msnum =  sav_nr785 and Prima_TST785 = 1
028600041125     c                   eval      §MSNUM = sav_nr785 + 1
028700041125     c                   eval      §MSDAT = *date
028800041125     C                   MOVEL     edidsms       TABUNI
028900041125     c                   end
029000041125     C                   update    EDTAB000
029100041125     c                   end
029200061214     C*
029300061214     C                   MOVEL(p)  'MS'          TABCOD
029400061214     c                   MOVEL(p)  '730SEDE'     TABKEY
029500061214     C     Ktab          chain     EDTAB01L
029600061214     C                   if        %found(EDTAB01L)
029700061214     C                   MOVEL     TABUNI        edidsms
029800061214     c                   if        §msnum <> sav_nr730 or
029900061214     c                             §msnum =  sav_nr730 and Prima_TST730 = 1
030000061214     c                   eval      §MSNUM = sav_nr730 + 1
030100061214     c                   eval      §MSDAT = *date
030200061214     C                   MOVEL     edidsms       TABUNI
030300061214     c                   end
030400061214     C                   update    EDTAB000
030500061214     c                   end
030600041125      *
030700070504     C                   MOVEL(p)  'MS'          TABCOD
030800070504     c                   MOVEL(p)  '350SEDE'     TABKEY
030900070504     C     Ktab          chain     EDTAB01L
031000070504     C                   if        %found(EDTAB01L)
031100070504     C                   MOVEL     TABUNI        edidsms
031200070504     c                   if        §msnum <> sav_nr350 or
031300070504     c                             §msnum =  sav_nr350 and Prima_TST350 = 1
031400070504     c                   eval      §MSNUM = sav_nr350 + 1
031500070504     c                   eval      §MSDAT = *date
031600070504     C                   MOVEL     edidsms       TABUNI
031700070504     c                   end
031800070504     C                   update    EDTAB000
031900070504     c                   end
032000070504      *
032100060314      * ?Esegue il Commit prima di lanciare le sottomissioni
032200060314      * ?              *--------*
032300060314     c                   commit
032400060314      * ?              *--------*
032500060314      *
032600041202      *  Quindi lancia l'azione in ambiente di filiale per ricevere
032700041202      *  le Bolle sui relativi EDVAB o FIVAB
032800041203     c                   do        99            svX
032900041203     c                   if        lnpX(svX) = 0
033000041203     c                   leave
033100041203     c                   end
033200041203     c                   move      LnpX(svX)     Lnp_alf           3
033300041203     c                   move      HubX(svX)     Hub_alf           3
033400041203     c                   move      NumX(svX)     Num_alf           7
033500041202     c                   clear                   kpjbu
033600070104      *
033700070104      * Versione SUM --> S93A
033800111017     c                   if        SumX(svX) = 'SUM93A'
033900041202     c                   movel     'TC80'        kcoaz
034000070104     c                   end
034100070104      *
034200070104      * Versione SUM --> D96A
034300070104      * ?da modificare quando pronto il nuovo traduttore versione 96A
034400111017     c                   if        SumX(svX) = 'SUM96A'
034500070226     c                   movel     'TC79'        kcoaz
034600070104     c                   end
034700070104      *
034800041202     c                   if        flgtst = ' '
034900041202     c                   movel     'FILTRA201 '  knsif
035000041203     c                   eval      kqebi = 'K'+ Hub_alf + 'EDI'
035100041203     c                   eval      knmus = 'EDPCED' + Hub_alf
035200041203     c                   eval      knmtd = knmus
035300041203     c                   else
035400041203     c                   movel     'FILTRAPFP '  knsif
035500041203     c                   eval      kqebi = 'K043EDI'
035600041203     c                   eval      knmus = 'EDPCED043'
035700041203     c                   eval      knmtd = knmus
035800041203     c                   end
035900041202     c                   movel     'QPRINTS   '  kqopr
036000041203     c                   eval      %subst(kpjbu:240:7) = Num_alf
036100041203     c                   eval      %subst(kpjbu:247:4) = 'M046'
036200041203     c                   eval      %subst(kpjbu:251:3) = Lnp_alf
036300041203     c                   eval      %subst(kpjbu:254:3) = Hub_alf
036400041203      *
036500041206      *  x provare la procedura mettevo in Hold il lavoro
036600041206     c*****              eval      KSTEB = 'H'
036700041206     c*****              eval      KSTJO = 'H'
036800041202     c                   call      'BCH10'
036900041202     c                   parm                    kpjba
037000041203     c                   endDO
037100041202      *
037200041125      *  se ci sono stati dei problemi con dei messaggi
037300041125     c                   if        Err_su_msg = 'SI'
037400041125      **              *--------------------------*
037500041125     c                   exsr      snd_msg_sum
037600041125      **              *--------------------------*
037700041125     c                   end
037800041125      *
037900041125     C                   Endsr
038000010115     C*-------------------------------------------------------------------------------------
038100010123     C*?    Controlla la testata a quale Filiale appartiene il Partner
038200010115     C*-------------------------------------------------------------------------------------
038300041117     C     DATI_Testata  BEGSR
038400041117      *
038500010115     C* inizializzo i campi di salvataggio codici
038600041117     c                   clear                   num_progr
038700041117     c                   clear                   Sav_Docum
038800041117     C                   clear                   sav_Ksc
038900041117     C                   clear                   sav_Lnp
039000041117     C                   clear                   sav_nome
039100041117     C                   clear                   sav_date
039200041117     C                   clear                   sav_time
039300070104     C                   clear                   sav_Map_SUM
039400070104     C                   clear                   sav_Map_STA
039500041117      *
039600041117     C                   clear                   Partner_OK
039700041117     C                   clear                   WCod
039800010115     C                   clear                   EDIDSPT
039900010123     C                   clear                   EDIDSPU
040000010115     C                   clear                   EDST00DS
040100041125     C                   clear                   EDTT00DS
040200010115     C*
040300010123      *  Imposta la DS di controllo del record TESTATA << ST00 >>
040400041203      *    Status
040500041125     c                   if        tipfil = 'IFTSTA'
040600041117     C                   MOVEL     rrIFTSTA      EDST00DS
040700041117      * dati da riportare sul file x identificare il messaggio
040800041125     c                   move      St_TA1001     Sav_Docum
040900041125     C                   MOVE      St_TA1004     sav_nome
041000041125     C                   MOVE      St_TA0017     sav_date
041100041125     C                   MOVE      St_TA0019     sav_time
041200010115      * Primo tentativo rep. da codice + qualificatore
041300041125     C                   MOVEL     St_TA0004     WCOD
041400041125     C                   MOVE      St_TA0007     WCOD
041500041203     c                   end
041600041203      *    Bolle
041700041203     c                   if        tipfil = 'IFCSUM'
041800041125     C                   MOVEL     rrIFCSUM      EDTT00DS
041900041125      * dati da riportare sul file x identificare il messaggio
042000041125     c                   move      Sm_TA1001     Sav_Docum
042100041125     C                   MOVE      Sm_TA1004     sav_nome
042200041125     C                   MOVE      Sm_TA0017     sav_date
042300041125     C                   MOVE      Sm_TA0019     sav_time
042400041125      * Primo tentativo rep. da codice + qualificatore
042500041125     C                   MOVEL     Sm_TA0004     WCOD
042600050914      **
042700050914      **  se qualificatore senza niente --> Blank
042800050914     c                   if        Sm_TA0007 = *blank
042900050914      **   imposta la nazione
043000050914     c                   z-add     nrrec         rec_pos           9 0
043100050914     c                   move      *blank        Naz_mitt          3
043200050914      *  Restituisce la Nazione con cui agganciare l'UNB su tab.:PT
043300050914     c                   call      'TRTC100R1'
043400050914     c                   parm                    rec_pos
043500050914     c                   parm                    Naz_mitt
043600050914     C                   MOVEl     Naz_mitt      Sm_TA0007
043700050914     c                   end
043800050914      *
043900041125     c                   end
044000050914      *
044100050914     C                   MOVE      Sm_TA0007     WCOD
044200041203      *
044300041125     C                   MOVEL     '20'          sav_date
044400050914      *
044500010115     C                   Z-ADD     1             X
044600010115     C     WCOD          LOOKUP    CPT(X)                                 33
044700010123      *  Prova a cercarlo con chiave parziale
044800010115     C                   If        *IN33 = *off
044900010123     C                   eval      X = 1
045000041125     c                   if        tipfil = 'IFTSTA'
045100041125     C                   eval      W035A = %subst(St_TA0004:1:31)
045200041203     c                   end
045300041203     c                   if        tipfil = 'IFCSUM'
045400041125     C                   eval      W035A = %subst(Sm_TA0004:1:31)
045500041125     c                   end
045600010115     C     W035A         lookup    CPTparz(X)                             33
045700010115     C                   Endif
045800041115      *
045900010115      *  Salva dalla tabella "PT" il cliente solo se è stata presa direttamente
046000010115      *   o in modo parziale.
046100010115     C                   If        *IN33 = *on
046200041117     C                   move      'OK'          Partner_OK                     tutto OK
046300010115     C                   MOVEL     DPT(X)        EDIDSPT
046400010123     C                   MOVEL     DPU(X)        EDIDSPU
046500111017     C                   MOVEL     DPZ(X)        EDIDSPZ
046600041117     C                   MOVEL     §PTLNP        sav_Lnp
046700041117     C                   z-add     §PTKSC        sav_ksc
046800111017     C*********          move      §PUsum        sav_Map_SUM
046900111017     C*********          move      §PUsta        sav_Map_STA
047000010123      *
047100111017     C                   move      §PZPGMRCZB    sav_Map_SUM
047200111017     C                   move      §PZPGMRCZS    sav_Map_STA
047300111017      *
047400010123     C                   Else
047500041117      *
047600041115      * se lo trova deve segnalare l'errore x Problemi di reperimento del Partner
047700041117     C                   move      'ER'          Partner_OK                     errore
047800041117     c                   move      'SI'          Err_su_msg
047900041117      *
048000041117      *  Problemi di reperimento del Partner
048100010123     C                   movel     ERR(1)        DesERR
048200010123     C                   movel     Wcod          WValor
048300010413     C                   clear                   WValo1
048400041118     C                   move      '01'          codmsgerr
048500041117      **              *--------------------------*
048600010123     C                   Exsr      STPERR
048700041117      **              *--------------------------*
048800010115     C                   Endif
048900041117      *
049000041117      *  Poichè in tabella "MS" il numeratore funziona come un semaforo in modo che
049100041117      *  i programmi di filiale possano elaborare solo i messaggi precedenti al nr.di tabella.
049200041117      * Alla prima testata il numero è quello preso direttamente in tabella poi verrà
049300041117      * incrementato.
049400041117      * A chiusura programma si dovrà riportare i numeratori sulla tabella incrementati di 1
049500041117      * x mantenere la logica sopra dichiarata.
049600041125      *  Se Status
049700041125     c                   if        tipfil = 'IFTSTA'
049800041125      *
049900041117     c                   IF        Sav_Docum = '784'
050000041117     c                   if        Prima_TST784  = 0
050100041117     c                   eval      Prima_TST784  = 1
050200041117     c                   else
050300041117     C                   eval      Sav_nr784 = Sav_nr784 + 1
050400041117     c                   end
050500041117     c                   END
050600041117      *
050700041117     c                   IF        Sav_Docum = '781'
050800041117     c                   if        Prima_TST781  = 0
050900041117     c                   eval      Prima_TST781  = 1
051000041117     c                   else
051100041117     C                   eval      Sav_nr781 = Sav_nr781 + 1
051200041117     c                   end
051300041117     c                   END
051400041118      *
051500041125     c                   END
051600041125      *
051700041125      *  Se Bolle
051800041125     c                   if        tipfil = 'IFCSUM'
051900041125      *
052000041125     c                   IF        Sav_Docum = '785'
052100041206      **
052200041125     c                   if        Prima_TST785  = 0
052300041125     c                   eval      Prima_TST785  = 1
052400041125     c                   else
052500041125     C                   eval      Sav_nr785 = Sav_nr785 + 1
052600041206     c                   end
052700041203      **
052800041203      **           - - - - - - - - - - - - - - - - - - - - - - - -
052900041203      * imposta la LNP e l'HUB di partenza su schiera
053000041203      * per gestire in seguito il lancio delle azioni di ricezione in filiale
053100041203      * dei messaggi. (Occorre fare questo poichè ciascun User può gestire delle linee
053200041203      * di partenza e non tutte quidni non sarebbe facile decodificare correttamente i
053300041203      * messaggi).
053400041203     C                   clear                   FNLV55DS
053500041203     C                   movel     'P'           D55tpt
053600041203     C                   move      sav_lnp       D55lin
053700041203     C                   move      *date         d55drf
053800041203     C                   CALL      'FNLV55R'
053900041203     C                   parm                    FNLV55DS
054000041203     c                   clear                   HubP              3 0
054100041203     c                   if        d55err = *blanks
054200041203     c                   movel     d55tfp        HubP
054300041203     c                   end
054400041203     c                   z-add     1             nrx
054500041203     c     sav_lnp       lookup    lnpX(nrX)                              34
054600041203     c                   if        *in34 = *Off
054700041203     c     *zeros        lookup    lnpX(nrX)                              34
054800041203     c                   end
054900041203     c                   z-add     sav_lnp       lnpX(nrX)
055000070104     c                   move      sav_Map_SUM   SumX(nrX)
055100041203     c                   z-add     HubP          HubX(nrX)
055200041222     c                   if        Sav_nr785 > NumX(nrX)
055300041203     c                   z-add     Sav_nr785     NumX(nrX)
055400041222     c                   end
055500041203      **           - - - - - - - - - - - - - - - - - - - - - - - -
055600041125     c                   END
055700041125      *
055800070504      *
055900070504     c                   IF        Sav_Docum = '350'
056000070504      **
056100070504     c                   if        Prima_TST350  = 0
056200070504     c                   eval      Prima_TST350  = 1
056300070504     c                   else
056400070504     C                   eval      Sav_nr350 = Sav_nr350 + 1
056500070504     c                   end
056600070504      **
056700070504      **           - - - - - - - - - - - - - - - - - - - - - - - -
056800070504      * imposta la LNP e l'HUB di partenza su schiera
056900070504      * per gestire in seguito il lancio delle azioni di ricezione in filiale
057000070504      * dei messaggi. (Occorre fare questo poichè ciascun User può gestire delle linee
057100070504      * di partenza e non tutte quidni non sarebbe facile decodificare correttamente i
057200070504      * messaggi).
057300070504     C                   clear                   FNLV55DS
057400070504     C                   movel     'P'           D55tpt
057500070504     C                   move      sav_lnp       D55lin
057600070504     C                   move      *date         d55drf
057700070504     C                   CALL      'FNLV55R'
057800070504     C                   parm                    FNLV55DS
057900070504     c                   clear                   HubP              3 0
058000070504     c                   if        d55err = *blanks
058100070504     c                   movel     d55tfp        HubP
058200070504     c                   end
058300070504     c                   z-add     1             nrx
058400070504     c     sav_lnp       lookup    lnpX(nrX)                              34
058500070504     c                   if        *in34 = *Off
058600070504     c     *zeros        lookup    lnpX(nrX)                              34
058700070504     c                   end
058800070504     c                   z-add     sav_lnp       lnpX(nrX)
058900070504     c                   move      sav_Map_SUM   SumX(nrX)
059000070504     c                   z-add     HubP          HubX(nrX)
059100070504     c                   if        Sav_nr350 > NumX(nrX)
059200070504     c                   z-add     Sav_nr350     NumX(nrX)
059300070504     c                   end
059400070504      **           - - - - - - - - - - - - - - - - - - - - - - - -
059500070504     c                   END
059600061214      *
059700070504      *
059800061214     c                   IF        Sav_Docum = '730'
059900061214      **
060000061214     c                   if        Prima_TST730  = 0
060100061214     c                   eval      Prima_TST730  = 1
060200061214     c                   else
060300061214     C                   eval      Sav_nr730 = Sav_nr730 + 1
060400061214     c                   end
060500061214      **
060600061214      **           - - - - - - - - - - - - - - - - - - - - - - - -
060700061214      * imposta la LNP e l'HUB di partenza su schiera
060800061214      * per gestire in seguito il lancio delle azioni di ricezione in filiale
060900061214      * dei messaggi. (Occorre fare questo poichè ciascun User può gestire delle linee
061000061214      * di partenza e non tutte quidni non sarebbe facile decodificare correttamente i
061100061214      * messaggi).
061200061214     C                   clear                   FNLV55DS
061300061214     C                   movel     'P'           D55tpt
061400061214     C                   move      sav_lnp       D55lin
061500061214     C                   move      *date         d55drf
061600061214     C                   CALL      'FNLV55R'
061700061214     C                   parm                    FNLV55DS
061800061214     c                   clear                   HubP              3 0
061900061214     c                   if        d55err = *blanks
062000061214     c                   movel     d55tfp        HubP
062100061214     c                   end
062200061214     c                   z-add     1             nrx
062300061214     c     sav_lnp       lookup    lnpX(nrX)                              34
062400061214     c                   if        *in34 = *Off
062500061214     c     *zeros        lookup    lnpX(nrX)                              34
062600061214     c                   end
062700061214     c                   z-add     sav_lnp       lnpX(nrX)
062800070104     c                   move      sav_Map_SUM   SumX(nrX)
062900061214     c                   z-add     HubP          HubX(nrX)
063000061214     c                   if        Sav_nr730 > NumX(nrX)
063100061214     c                   z-add     Sav_nr730     NumX(nrX)
063200061214     c                   end
063300061214      **           - - - - - - - - - - - - - - - - - - - - - - - -
063400061214     c                   END
063500061214      *
063600041125     c                   END
063700041125      *
063800041118      *  per differenziare i messaggi temporalmente
063900041118     C                   TIME                    ST00_ora          6 0
064000041118      *
064100010115     C                   Endsr
064200010123     C*----------------------------------------------------------------
064300041117     C*?  Deve Aggiornare il messaggio appena letto se OK oppure ER
064400010123     C*----------------------------------------------------------------
064500041117     C     AGG_record    BEGSR
064600041117      *              *-------------------------------*
064700041117      *  Aggiorna i campi gestione Bartolini
064800041117      *
064900041117     C                   eval      RRELABORA = Partner_OK
065000041117     C                   eval      RRNOMEDOC = Sav_Nome
065100041117     C                   eval      RRDATA    = Sav_Date
065200041206     C     100           mult      Sav_Time      RRORA
065300041117     C                   eval      RRLINEA   = Sav_Lnp
065400041118     C                   eval      RRDATELA  = *date
065500041118     C                   eval      RRORAELA  = ST00_ora
065600041125      *
065700041125     c                   if        tipfil = 'IFTSTA'
065800041125     c                   if        Sav_Docum = '784'
065900041125     C                   eval      RRNUMERO  = Sav_nr784
066000041201     C                   eval      RRTIPO    = '784'
066100061214     c                   end
066200061214     c                   if        Sav_Docum = '781'
066300041125     C                   eval      RRNUMERO  = Sav_nr781
066400041201     C                   eval      RRTIPO    = '781'
066500041125     c                   end
066600041125     c                   end
066700041117      *
066800041125     c                   if        tipfil = 'IFCSUM'
066900041125     c                   if        Sav_Docum = '785'
067000041125     C                   eval      RRNUMERO  = Sav_nr785
067100041201     C                   eval      RRTIPO    = '785'
067200041117     c                   end
067300070504     c                   if        Sav_Docum = '350'
067400070504     C                   eval      RRNUMERO  = Sav_nr350
067500070504     C                   eval      RRTIPO    = '350'
067600070504     c                   end
067700061214     c                   if        Sav_Docum = '730'
067800061214     C                   eval      RRNUMERO  = Sav_nr730
067900061214     C                   eval      RRTIPO    = '730'
068000061214     c                   end
068100041125     c                   end
068200041117      *
068300041117     C                   eval      num_progr = num_progr + 1
068400041117     C                   eval      RRPROGR   = num_progr
068500041221     c                   move      rrNUMERO      four_digit        4
068600041125      *
068700041125      *              *-------------------------------*
068800041125     c                   if        tipfil = 'IFTSTA'
068900041221     c                   movel     four_digit    rrIFTSTA
069000041125     C                   UPDATE    rcvsta
069100041125     c                   end
069200041125     c                   if        tipfil = 'IFCSUM'
069300041221     c                   movel     four_digit    rrIFCSUM
069400041125     C                   UPDATE    rcvsum
069500041125     c                   end
069600041117      *              *-------------------------------*
069700041125      *
069800041117     C                   Endsr
069900041117     C*----------------------------------------------------------------
070000041117     C*? STPERR: STAMPA ERRORE
070100041117     C*----------------------------------------------------------------
070200041117     C     STPERR        BEGSR
070300041117     C*
070400010123     C     *INOF         IFEQ      '1'
070500041125     C*
070600041125     c                   if        tipfil ='IFTSTA'
070700041125     C                   EXCEPT    TESSTA
070800041125     c                   else
070900041125     C                   EXCEPT    TESSUM
071000041125     c                   end
071100041125     C*
071200010123     C                   EXCEPT    ERRCLI
071300010123     C                   END
071400041125     C*
071500010123     C                   EXCEPT    DETER
071600010413     C*
071700010413     C     Wvalo1        IFne      *blank
071800010413     C                   EXCEPT    MAXDET
071900010413     C                   End
072000010123     C*
072100010321     C*  invia messaggio di errore alle code operatore e di programma
072200040219      * una sola volta x msg
072300041125     c                   if        primo_sum = '0' and tipfil = 'IFCSUM' or
072400041125     c                             primo_sta = '0' and tipfil = 'IFTSTA'
072500041118     C                   call      'TRTC100MSG'
072600010321     C                   PARM                    WCOD
072700010906     C                   PARM                    USRMSG
072800041118     C                   PARM                    codmsgerr
072900041125     C                   PARM                    tipfil
073000041125     c                   if        tipfil = 'IFCSUM'
073100041125     c                   move      '1'           primo_sum
073200041125     c                   else
073300041125     c                   move      '1'           primo_sta
073400041125     c                   end
073500040219     c                   end
073600010321     C*
073700010123     C                   ENDSR
073800010123     C*----------------------------------------------------------------
073900041117     C*  INVIO Messaggio
074000041117     C*----------------------------------------------------------------
074100041125     C     SND_Msg_sta   BEGSR
074200010413      *
074300010703      * SNDPGMMSG di avviso errore TOUSR(EDPAB)
074400010703     C                   clear                   MSGDS
074500041117     C                   MOVEL     CMD(3)        MSGDS
074600010703     C                   eval      MSGDS = %trim(MSGDS) +
074700010905     C                             ' TOUSR('+
074800010905     C                             %trim(USRMSG) +')'
074900010703     C                   Z-ADD     100           LENGH
075000010703     C                   CALL      'QCMDEXC'                            99
075100010703     C                   PARM                    MSGDS
075200010703     C                   PARM                    LENGH
075300010703      *
075400010703      * SNDBRKMSG di avviso errore TOUSR(DSP01)
075500050221     c                   if        MSGd01 = 'S'
075600010703     C                   clear                   MSGDS
075700041117     C                   MOVEL     CMD(4)        MSGDS
075800010703     C                   eval      MSGDS = %trim(MSGDS) +
075900010703     C                             ' TOMSGQ(DSP01)'
076000010703     C                   Z-ADD     100           LENGH
076100010703     C                   CALL      'QCMDEXC'                            99
076200010703     C                   PARM                    MSGDS
076300010703     C                   PARM                    LENGH
076400050221     c                   end
076500010123      *
076600010123     C                   Endsr
076700041125     C*----------------------------------------------------------------
076800041125     C*  INVIO Messaggio
076900041125     C*----------------------------------------------------------------
077000041125     C     SND_Msg_sum   BEGSR
077100041125      *
077200041125      * SNDPGMMSG di avviso errore TOUSR(EDPAB)
077300041125     C                   clear                   MSGDS
077400041125     C                   MOVEL     CMD(5)        MSGDS
077500041125     C                   eval      MSGDS = %trim(MSGDS) +
077600041125     C                             ' TOUSR('+
077700041125     C                             %trim(USRMSG) +')'
077800041125     C                   Z-ADD     100           LENGH
077900041125     C                   CALL      'QCMDEXC'                            99
078000041125     C                   PARM                    MSGDS
078100041125     C                   PARM                    LENGH
078200041125      *
078300041125      * SNDBRKMSG di avviso errore TOUSR(DSP01)
078400050221     c                   if        MSGd01 = 'S'
078500041125     C                   clear                   MSGDS
078600041125     C                   MOVEL     CMD(6)        MSGDS
078700041125     C                   eval      MSGDS = %trim(MSGDS) +
078800041125     C                             ' TOMSGQ(DSP01)'
078900041125     C                   Z-ADD     100           LENGH
079000041125     C                   CALL      'QCMDEXC'                            99
079100041125     C                   PARM                    MSGDS
079200041125     C                   PARM                    LENGH
079300050221     c                   end
079400041125      *
079500041125     C                   Endsr
079600001003      *----------------------------------------------------------------
079700041129     C*? Pulisce i vecchi records.
079800001003     C*----------------------------------------------------------------
079900041129     C     Pulizia_old   BEGSR
080000041129      *
080100041129     c                   move      *date         dataOGGI          8 0
080200041129      *
080300050823      * ?------------------------------------------------------------------ */
080400050823      * ?Prima venivano presi dall'area dati PULIRCV adesso è attiva la
080500050823      * ?tabella EDTAB --> GG con i giorni e l'ultima data pulizia.
080600050823      * ?------------------------------------------------------------------ */
080700041129      *   Vengono puliti i records + vecchi di 15 giorni
080800050823     c***  *dtaara       define                  puliRCV
080900050823     c***  *lock         in(E)     puliRCV
081000050823     c***                IF        %Error
081100050823     c***                z-add     15            GG_Piu_old
081200050823     c***                z-add     dataOGGI      dataPulizia
081300050823     c***                end
081400050823      * ?------------------------------------------------------------------ */
081500050823      * ?Call x reperire l'ultima data pulizia ed i giorni oltre i quali
081600050823      * ?pulire i records. (x non aggiornare tabella KPJBU deve essere pulita)
081700050823      * ?------------------------------------------------------------------ */
081800050823     c                   clear                   kpjbu
081900050823     c                   call      'TRTC96BR'
082000050823     c                   parm                    kpjba
082100050823     c                   movel     kpjbu         edidsGG
082200050823      **
082300050823      * ?se x assurdo la tabella fosse vuota  imposta fisso a 15 gg.
082400050824     c                   IF        §ggGGR = '000' or §ggGGR = *blank
082500060215     c                   z-add     30            GG_Piu_old
082600050823     c                   z-add     dataOGGI      dataPulizia
082700050823     c                   else
082800050823      * ?i campi su DS sono Alfa
082900050823     c                   move      §ggDTR        dataPulizia
083000050823     c                   move      §ggGGR        GG_Piu_old
083100050823     c                   end
083200050823      * ?     /* ---------------------------------------------- */
083300041129      *
083400041129      *  solo se la data presente sull'area Dati
083500041129     C                   IF        dataOGGI <> DataPulizia
083600041129      *
083700041129     c                   move      *date         data_iso
083800041129     c     data_iso      subdur    GG_Piu_old:*d data_old
083900041129     c                   move      data_old      data08            8 0
084000041129      *
084100041129     C/EXEC SQL
084200041129     C+ DELETE   FROM RCVIFTSTA  where
084300041129     C+ RRDATELA <  :DATA08      and
084400080327     C+ RRELABORA IN ('RX', 'ER')
084500041129     C/END-EXEC
084600041129      *
084700041129     C/EXEC SQL
084800041129     C+ DELETE   FROM RCVIFCSUM  where
084900041129     C+ RRDATELA <  :DATA08      and
085000080327     C+ RRELABORA IN ('RX', 'ER')
085100041129     C/END-EXEC
085200041129      *  quindi aggiorna con la data di oggi l'area dati
085300041129      *   x non rieseguire in seguito ancora la pulizia che deve essere giornaliera
085400050823     c****               z-add     dataOGGI      dataPulizia
085500050823     c****               out       puliRCV
085600050823      * ?------------------------------------------------------------------ */
085700050823      * ?Call x aggiornare l'ultima data pulizia
085800050823      * ? (x aggiornare tabella passa in KPJBU la data RCV)
085900050823      * ?------------------------------------------------------------------ */
086000050823     c                   clear                   edidsGG
086100050823     c                   move      dataOGGI      §ggDTR
086200050823     c                   clear                   kpjbu
086300050823     c                   movel     edidsGG       kpjbu
086400050823     c                   call      'TRTC96BR'
086500050823     c                   parm                    kpjba
086600050823      **
086700041129     C                   EndIF
086800041129      *
086900041129     C                   Endsr
087000041129      *----------------------------------------------------------------
087100041129     C*? *INZSR - OPERAZIONI INIZIALI
087200041129     C*----------------------------------------------------------------
087300041129     C     *INZSR        BEGSR
087400041129     C*
087500010320     C* Riceve un parametro per capire se si trova in ambiente di produzione
087600010320     C*  o in ambiente di test
087700010320     C     *entry        plist
087800010320     C                   parm                    flgtst            1
087900010906     C                   parm                    UsrMsg           10
088000041125     C                   parm                    TipFil           10
088100041125     C*
088200041125     C*  Identifica che messaggio deve trattare
088300041125     c                   select
088400041125     c                   when      tipfil = 'IFTSTA'
088500041125     c                   open      rcviftsta1
088600041125     c                   when      tipfil = 'IFCSUM'
088700041125     c                   open      rcvifcsum1
088800041125     c                   endsl
088900010320     C*
089000001003     C* Definisco chiavi di accesso
089100041115     C     Krcv          KLIST
089200041115     C                   KFLD                    Blk_RCV
089300041115     C                   move      *blank        Blk_RCV           2
089400001003      *
089500041117     C     Ktab          KLIST
089600041117     C                   KFLD                    tabcod
089700041117     C                   KFLD                    tabkey
089800041117      *
089900001003      * RECUPERO DATI DELL'UTENTE
090000001003     C                   Z-ADD     1             CODUT
090100001003     C                   CALL      'XPARUT'
090200001003     C                   PARM                    UTEDSE0F
090300010123     C                   MOVEL     RAGUT         RSUT             20
090400010123     C                   Seton                                        OF
090500041125     c                   move      '0'           primo_sum         1
090600041125     c                   move      '0'           primo_sta         1
090700001003      *
090800001003      * Caricamento Tabella Partner esteri
090900001003     C                   Z-ADD     0             X
091000010307      *
091100020904      * Punta alle Tabelle in Lista di Librerie
091200010319     C                   clear                   MSGDS
091300041117     C                   MOVEL     CMD(2)        MSGDS
091400010319     C                   Z-ADD     100           LENGH
091500010307     C                   CALL      'QCMDEXC'
091600010307     C                   PARM                    MSGDS
091700010307     C                   PARM                    LENGH
091800010307      *
091900010307      * Carica la parte BARTOLINI
092000010307     C                   OPEN      EDTAB01L
092100001003     C                   MOVEL     'PT'          TABCOD
092200001006     C     TABCOD        SETLL     EDTAB01L
092300041117     C     TABCOD        READE(N)  EDTAB01L
092400001003     C                   DOW       not %EOF(EDTAB01L)
092500001003     C     TABFLG        IFEQ      *BLANKS
092600001003     C                   ADD       1             X
092700001003     C                   MOVEL     TABKEY        CPT(X)
092800001003     C                   eval      CPTparz(X) = %subst(TABKEY:1:30)
092900001003     C                   MOVEL     TABUNI        DPT(X)
093000001003     C                   End
093100041117     C     TABCOD        READE(N)  EDTAB01L
093200001003     C                   ENDdo
093300121105      *
093400121105      *  se deve inviare la mail di alert x limite quasi raggiunto.
093500121105     c                   exsr      ChecK_PT
093600121105      *
093700010123     C*
093800010123     C                   MOVEL     'PU'          TABCOD
093900010123     C     TABCOD        SETLL     EDTAB01L
094000041117     C     TABCOD        READE(N)  EDTAB01L
094100010123     C                   DOW       not %EOF(EDTAB01L)
094200010123     C     TABFLG        IFEQ      *BLANKS
094300010123     C                   Z-ADD     1             X
094400010123     C     TABKEY        LOOKUP    CPT(X)                                 32
094500010123     C   32              MOVEL     TABUNI        DPU(X)
094600010123     C                   END
094700041117     C     TABCOD        READE(N)  EDTAB01L                               30
094800010123     C                   END
094900041117      *
095000111017     C*  Modificato x sicurezza anche se non servirà +.
095100111017     C                   MOVEL     'PZ'          TABCOD
095200111017     C     TABCOD        SETLL     EDTAB01L
095300111017     C     TABCOD        READE(N)  EDTAB01L
095400111017     C                   DOW       not %EOF(EDTAB01L)
095500111017     C     TABFLG        IFEQ      *BLANKS
095600111017     C                   Z-ADD     1             X
095700111017     C     TABKEY        LOOKUP    CPT(X)                                 32
095800111017     C   32              MOVEL     TABUNI        DPZ(X)
095900111017     C                   END
096000111017     C     TABCOD        READE(N)  EDTAB01L                               30
096100111017     C                   END
096200111017      *
096300041117      * Prende i numeratori dalla tabella x num. di sede
096400041117     c                   clear                   sav_nr784
096500041117     c                   clear                   sav_nr781
096600041125     c                   clear                   sav_nr785
096700061214     c                   clear                   sav_nr730
096800070504     c                   clear                   sav_nr350
096900041117      *
097000041125      *  Stati di consegna - IFTSTA
097100041125     c                   if        tipfil = 'IFTSTA'
097200041125      *
097300041117     C                   MOVEL(p)  'MS'          TABCOD
097400041117     c                   MOVEL(p)  '784SEDE'     TABKEY
097500041117     C     Ktab          chain(n)  EDTAB01L
097600041117     C                   if        %found(EDTAB01L)
097700041117     C                   MOVEL     TABUNI        edidsms
097800041118     c                   z-add     §MSNUM        sav_nr784
097900041222     c                   if        sav_nr784 = 0
098000041118     c                   z-add     1             sav_nr784
098100041118     c                   end
098200041118     c                   end
098300010307     C*
098400041117     C                   MOVEL(p)  'MS'          TABCOD
098500041117     c                   MOVEL(p)  '781SEDE'     TABKEY
098600041117     C     Ktab          chain(n)  EDTAB01L
098700041117     C                   if        %found(EDTAB01L)
098800041117     C                   MOVEL     TABUNI        edidsms
098900041118     c                   z-add     §MSNUM        sav_nr781
099000041222     c                   if        sav_nr781 = 0
099100041118     c                   z-add     1             sav_nr781
099200041118     c                   end
099300041117     c                   end
099400041117     C*
099500041125     c                   endIF
099600041125      *
099700041125      *  Bolle - IFCSUM
099800041125     c                   if        tipfil = 'IFCSUM'
099900041125      *
100000041125     C                   MOVEL(p)  'MS'          TABCOD
100100041125     c                   MOVEL(p)  '785SEDE'     TABKEY
100200041125     C     Ktab          chain(n)  EDTAB01L
100300041125     C                   if        %found(EDTAB01L)
100400041125     C                   MOVEL     TABUNI        edidsms
100500041125     c                   z-add     §MSNUM        sav_nr785
100600041222     c                   if        sav_nr785 = 0
100700041125     c                   z-add     1             sav_nr785
100800041125     c                   end
100900041125     c                   end
101000070504      *
101100070504     C                   MOVEL(p)  'MS'          TABCOD
101200070504     c                   MOVEL(p)  '350SEDE'     TABKEY
101300070504     C     Ktab          chain(n)  EDTAB01L
101400070504     C                   if        %found(EDTAB01L)
101500070504     C                   MOVEL     TABUNI        edidsms
101600070504     c                   z-add     §MSNUM        sav_nr350
101700070504     c                   if        sav_nr350 = 0
101800070504     c                   z-add     1             sav_nr350
101900070504     c                   end
102000070504     c                   end
102100061214     C*
102200061214     C                   MOVEL(p)  'MS'          TABCOD
102300061214     c                   MOVEL(p)  '730SEDE'     TABKEY
102400061214     C     Ktab          chain(n)  EDTAB01L
102500061214     C                   if        %found(EDTAB01L)
102600061214     C                   MOVEL     TABUNI        edidsms
102700061214     c                   z-add     §MSNUM        sav_nr730
102800061214     c                   if        sav_nr730 = 0
102900061214     c                   z-add     1             sav_nr730
103000061214     c                   end
103100061214     c                   end
103200041125     C*
103300041125     c                   endIF
103400050221     C*
103500050825      * ?------------------------------------------------------------------ */
103600050825      * ?Call x reperire se deve mandare Msg a DSP01
103700050825      * ? (x non aggiornare tabella KPJBU deve essere pulita)
103800050825      * ?------------------------------------------------------------------ */
103900050825     c                   clear                   kpjbu
104000050825     c                   call      'TRTC95BR'
104100050825     c                   parm                    kpjba
104200050825     c                   movel     kpjbu         edidsER
104300050825     c                   eval      MSGd01 = §ERD01
104400050825      **
104500010123     C* RECUPERO DATA E ORA
104600010123     C                   TIME                    WHHD12           12 0
104700010123     C                   MOVE      WHHD12        DATA6             6 0
104800010123     C                   MOVEL     DATA6         DATA              6 0
104900010123     C                   TIME                    WORA              6 0
105000010123     C*
105100001003     C                   ENDSR
105200121105      *---------------------------------------------------------------*
105300121105      * CTRL caricamento schiere    PT                               -*
105400121105      *---------------------------------------------------------------*
105500121105     C     Check_PT      begsr
105600121105     C*
105700121105     c* Controllo riempimento schiera
105800121105     c                   clear                   trul0sds
105900121105     c                   eval      i0sski='CPT'
106000121105     c                   eval      i0sele=%elem(CPT)
106100121105     c                   eval      i0spie=x
106200121105     c                   eval      i0sfile='EDTAB00F'
106300121105     c                   eval      i0ssif=knsif
106400121105     c                   eval      i0spgm='TRTC100R'
106500121112     c                   movel     kpjbu         kpjbusav
106600121112     c                   clear                   kpjbu
106700121105     c                   movel     trul0sds      kpjbu
106800121105     c                   call      'TRUL0SR'
106900121105     c                   parm                    kpjba
107000121112     c                   movel     kpjbusav      kpjbu
107100121105     C*
107200121105     C                   ENDSR
107300001002      *****************************************************************
107400041125     OQSYSPRT   E            TESSTA           01
107500010123     O                       RSUT                20
107600041118     O                                           46 '** ERRORI RICEZ. Status da'
107700041118     O                                              ' EDI in Sede ->RCVIFTSTA *'
107800041118     O                                              '*'
107900041118     O                       DATA                89 '  /  /  '
108000041118     O                                           95 'Pag.:'
108100041118     O                       PAGE          Z    100
108200041125     O          E            TESSTA         1 02
108300041118     O                       WORA                89 '  :  :  '
108400041118     O                                          100 'TRTC100R'
108500010123     O*------------------------------------
108600041125     O          E            TESSUM           01
108700041125     O                       RSUT                20
108800041125     O                                           46 '** ERRORI RICEZ. Bolle  da'
108900041125     O                                              ' EDI in Sede ->RCVIFCSUM *'
109000041125     O                                              '*'
109100041125     O                       DATA                89 '  /  /  '
109200041125     O                                           95 'Pag.:'
109300041125     O                       PAGE          Z    100
109400041125     O          E            TESSUM         1 02
109500041125     O                       WORA                89 '  :  :  '
109600041125     O                                          100 'TRTC100R'
109700041125     O*------------------------------------
109800010123     O          E            ERRCLI         1
109900010123     O                                           15 'CODICE CLIENTE:'
110000010123     O                       §PTKSC        Z     23
110100010123     O                       WCOD                60
110200010123     O*------------------------------------
110300010123     O          E            DETER          1
110400010123     O                       DESERR              78
110500010123     O                       WVALOR             113
110600010413     O*------------------------------------
110700010413     O          E            MAXDET         1
110800010413     O                       WVALO1             113
110900010123     O*------------------------------------
111000010123** (ERR)
111100010123Il  Partner  NON è stato trovato nella Tabella "PT" con questo codice:
111200010124** (CMD)
111300010402DLTOVR FILE(EDTAB01L)
111400020904OVRDBF FILE(EDTAB01L) TOFILE(*LIBL/EDTAB01L)
111500041118SNDMSG MSG('ATTENZIONE Ricez. EDI - ERRORI in   RCVIFTSTA in GRU')
111600041117SNDBRKMSG MSG('ATTENZIONE Ricez. EDI - ERRORI in   RCVIFTSTA in GRU')
111700041125SNDMSG MSG('ATTENZIONE Ricez. EDI - ERRORI in   RCVIFCSUM in GRU')
111800041125SNDBRKMSG MSG('ATTENZIONE Ricez. EDI - ERRORI in   RCVIFCSUM in GRU')
