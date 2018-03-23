000100001005     H*--------------------------------------------------------------------------------------------*
000200001005     H* Crea la statistica "RIEPILOGO ESITI CONSEGNE" per WURTH
000300011029     h*
000400011029     h* NOTE: Il pgm ha bisogno che il file printer TIS7W4P si chiami WURTH, ma il comando PC
000500011029     h* andando in batch non prenderebbe la sostituzione, QUINDI occorre copiare in QGPL il printer
000600011029     h* file TIS7W4P chiamandolo WURTH e poi compilare con PC (dopo ricordarsi di cancellarlo).
000700001005     H*--------------------------------------------------------------------------------------------*
000800001005     H/TITLE Carica righe spedizione trovate
000900990907     Hdatedit(*DMY)
001000001005     F*--------------------------------------------------------------------------------------------*
001100001005     F* Data base
001200001005     F*--------------------------------------------------------------------------------------------*
001300001005     Fazorg01l  if   e           k disk    usropn
001400001005     Ftabel00f  if   e           k disk    usropn
001500070222     Ftitas30c  if   e           k disk    usropn
001600011005     ftita430c  IF   E           K DISK    USROPN
001700050321     ftigcp51l  IF   E           K DISK    USROPN
001800050210     ffnevb04l  IF   E           K DISK    USROPN
001900050526     ffnlbl01l  IF   E           K DISK
002000050526     fWFS7W50F  UF A E           K DISK    USROPN
002100050614     fWFS7W51F  UF A E           K DISK
002200071023     fWFS7W52F  UF A E           K DISK
002300080214     FTIVGD00F  UF A E             DISK    commit
002400050524     FTIS7W4P   O    e             PRINTER usropn
002500001005     F                                     oflind(*in05)
002600050215     Fprtf198   O    f  132        printer oflind(*inof) usropn
002700001005     D*--------------------------------------------------------------------------------------------*
002800001005     D* Data structure
002900001005     D*--------------------------------------------------------------------------------------------*
003000001005     D*---
003100001005     D* Costanti
003200001005     D*---
003300001005     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
003400001005     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
003500001005     D*---
003600001005     D* Variabili riferite al data base
003700001005     D*---
003800001005     D ktaaas          S                   LIKE(tasaas)
003900001005     D ktalnp          S                   LIKE(taslnp)
004000001005     D ktanrs          S                   LIKE(tasnrs)
004100001005     D ktansp          S                   LIKE(tasnsp)
004200001005     D ktatbl          S                   LIKE(tastbl)
004300011005     d ka4aas          S                   LIKE(ta4aas)                         *lettura tita430c
004400011005     d ka4lnp          S                   LIKE(ta4lnp)
004500011005     d ka4nrs          S                   LIKE(ta4nrs)
004600011005     d ka4nsp          S                   LIKE(ta4nsp)
004700011005     d ka4trc          S                   LIKE(ta4trc)
004800050321     d kcpaas          S                   LIKE(gcpaas)                         *lettura tigcp00f
004900021108     d kcplnp          S                   LIKE(gcplnp)
005000021108     d kcpnrs          S                   LIKE(gcpnrs)
005100021108     d kcpnsp          S                   LIKE(gcpnsp)
005200021108     d kcpfrg          S                   LIKE(gcpfrg)
005300040427     d kevaas          S                   LIKE(evbaas)                         *lettura fnevb00f
005400040427     d kevlnp          S                   LIKE(evblnp)
005500040427     d kevnrs          S                   LIKE(evbnrs)
005600040427     d kevnsp          S                   LIKE(evbnsp)
005700050210     d kevcev          S                   LIKE(evbcev)
005800041122     D klbaas          S                   LIKE(lblaan)
005900041122     D klblnp          S                   LIKE(lbllpn)
006000041122     D klbnrs          S                   LIKE(lblnrn)
006100041122     D klbnsp          S                   LIKE(lblnsn)
006200001005     D ndst            S                   LIKE(Dst)                            *campi"normalizzati"
006300001005     D nmit            S                   LIKE(Mit)                            *campi"normalizzati"
006400001005     D nprd            S                   LIKE(Prd)                            *campi"normalizzati"
006500001005     D nlod            S                   LIKE(Lod)                            *campi"normalizzati"
006600001005     D tute            S                   LIKE(tblkut) inz(1)
006700001005     D tcod            S                   LIKE(tblcod)
006800001005     D tkey            S                   LIKE(tblkey)
006900040227     D deppoa          S                   LIKE(taslna)                         *depositi
007000060217     D depare          S                   LIKE(orgcar)                         *depositi
007100001005     D*---
007200001005     D* Variabili
007300001005     D*---
007400001006     D divis           s             10  3 inz(*zeros)
007500001005     D pardai          s              8  0 inz(*zeros)
007600001005     D pardaf          s              8  0 inz(*zeros)
007700011213     D a35             s             35    inz(*blanks)
007800011128     D a1              s              1    inz(*blanks)
007900001005     D pos             s              2  0 inz(*zeros)
008000001005     D wrkdata         s               d   DATFMT(*ISO) INZ(D'1999-01-01')
008100001005     D wpgm            s             20                                         *programma
008200001009     D dessta          c                   const('Esiti consegne:')
008300001006     D desdai          s              8    inz(*blanks)
008400001006     D desdaf          s              8    inz(*blanks)
008500001006     D despoa          s              3    inz(*blanks)
008600001006     D desprd          s              2    inz(*blanks)
008700001006     D deslod          s              2    inz(*blanks)
008800001006     D desfl1          s              5    inz(*blanks)
008900001006     D desfl2          s              5    inz(*blanks)
009000001006     D desfl3          s              5    inz(*blanks)
009100050210     D desfl4          s              5    inz(*blanks)
009200050210     D desfl5          s              5    inz(*blanks)
009300050210     D desfl6          s              5    inz(*blanks)
009400001006     D desmit          s             20    inz(*blanks)
009500001006     D desdst          s             20    inz(*blanks)
009600001006     D var100          s            100    inz(*blanks)
009700001006     D varcmd          s            400    inz(*blanks)
009800001006     D varsrv          s            155    inz(*blanks)
009900011029     d i               S              5  0 inz(*zeros)
010000011029     D*---
010100011029     D* Variabili di memorizzazione
010200011029     D*---
010300060217     D* GENERALI
010400060217     D*---
010500011029     d GTOTCON         S              7  0 inz(*zeros)
010600011029     d GCONANT         S              7  0 inz(*zeros)
010700011029     d GCON00          S              7  0 inz(*zeros)
010800011029     d GCON12          S              7  0 inz(*zeros)
010900011029     d GCON24          S              7  0 inz(*zeros)
011000011029     d GCON48          S              7  0 inz(*zeros)
011100011029     d GCON72          S              7  0 inz(*zeros)
011200050610     d GCON96          S              7  0 inz(*zeros)
011300011029     d GCON120         S              7  0 inz(*zeros)
011400011029     d GTMPMED         S              7  0 inz(*zeros)
011500050214     d GTOTGICH        S              7  0 inz(*zeros)
011600050214     d GTOTRESI        S              7  0 inz(*zeros)
011700011029     d GTOTANO         S              7  0 inz(*zeros)
011800011029     d GTOTGIA         S              7  0 inz(*zeros)
011900050210     d GTOTPAR         S              7  0 inz(*zeros)
012000040427     d GTOTAPP         S              7  0 inz(*zeros)
012100040427     d GTOTLAV         S              7  0 inz(*zeros)
012200011029     d GTOTALT         S              7  0 inz(*zeros)
012300050610     d GORETOT         S             20  0 inz(*zeros)
012400011029     d GPERANT         S              5  2 inz(*zeros)
012500011029     d GPER00          S              5  2 inz(*zeros)
012600011029     d GPER12          S              5  2 inz(*zeros)
012700011029     d GPER24          S              5  2 inz(*zeros)
012800011029     d GPER48          S              5  2 inz(*zeros)
012900011029     d GPER72          S              5  2 inz(*zeros)
013000011029     d GPER96          S              5  2 inz(*zeros)
013100011029     d GPER120         S              5  2 inz(*zeros)
013200010926     D*---
013300010926     D* Variabili di lavoro
013400010926     D*---
013500040427     D $statoOK        S              1                                         *stato bolla OK
013600050214     d $stato          S              5                                         *stati bolla
013700040427     D $notgia         S              1    INZ('N')                             *note "giacenza"
013800050321     D $gcp            S              1    INZ('N')                             *giacenza in TIGCP
013900050210     d $parz           S              1    INZ('N')                             *cons.parziale
014000040427     D $lav            S              1    INZ('N')                             *l.avviso in FNEVB
014100040427     D $MPoa           S              1    INZ('N')                             *d.memorizzati POA
014200040427     D $MGen           S              1    INZ('N')                             *d.memorizzati GEN
014300011029     D*---
014400011029     D* Schiere
014500011029     D*---
014600040427     D sorg            S              3  0 DIM(300)                             *organigramma
014700040427     D sdorg           S             20    DIM(300)
014800060217     D sare            S              3  0 DIM(300)                             *aree
014900060217     D sdare           S             25    DIM(300)
015000040427     D sprv            S              2    DIM(200)                             *provincie
015100040427     D sdprv           S             20    DIM(200)
015200080903     D sk2Alav         S              3    DIM(200)                             *eventi L/A
015300001006     D*---
015400001006     D* DS dati di servizio per gestione remota della stampa
015500001006     D*---
015600001207     D                 ds                  inz
015700001006     D sgiusrnbr               1      9
015800001006     D tipstp                 10     12
015900001006     D tipsnd                 13     14
016000001006     D email                  15     64
016100050210     D param                   1     64
016200001005     D*---
016300001005     D* DS controlla data
016400001005     D*---
016500001207     D wlbda8          ds                  inz                                  *controlla data
016600001005     D  g08dat                        8  0                                       -data dritta
016700001005     D  g08inv                        8  0                                       -data invertita
016800001005     D  g08err                        1                                          -errore
016900001005     D  g08tgi                        5  0                                       -giorni fra date
017000011128      * scomposizione data generica
017100001207     D                 ds                  inz
017200001005     D  dsann                  1      4  0                                       -anno
017300001005     D  dsmes                  5      6  0                                       -mese
017400001005     D  dsgio                  7      8  0                                       -giorno
017500001005     D  dsmgs                  5      8  0                                       -giorno
017600001005     D dsdat                   1      8  0                                      *data
017700011128      * Scomposizione data editata
017800040427     D                 DS                  INZ
017900040427     D  dsgioe                 1      2                                          -giorno
018000040427     D  dsvd1                  3      3                                          -'/'
018100040427     D  dsmese                 4      5                                          -mese
018200040427     D  dsvd2                  6      6                                          -'/'
018300040427     D  dsanne                 7     10                                          -anno
018400040427     D dsdate                  1     10                                         *data editata
018500011128      * Composizione data NON editata -alfabetica-
018600040427     D                 DS                  INZ
018700040427     D  dsgioa                 1      2                                          -giorno
018800040427     D  dsmesa                 3      4                                          -mese
018900040427     D  dsanna                 5      8                                          -anno
019000040427     D dsdata                  1      8                                         *data NON editata
019100050214      * Composizione chiave bolla
019200050214     d dskeybol        DS                  INZ
019300050214     d  tasaas                        4  0                                       -annp
019400050214     d  taslnp                        3  0                                       -linea partenza
019500050214     d  tasnrs                        2  0                                       -serie
019600050214     d  tasnsp                        7  0                                       -numero spedizione
019700001005     D*---
019800001005     D* DS contenente i dati dI INPUT
019900001005     D*---
020000001207     D TIS1W2IDS     e ds
020100001005     D*---
020200050518     d Tibs10ds      e ds
020300050518     d  sksc11                21   5520  0 dim(500)
020400050518     d*
020500050518     d  sksc7          s              7  0 dim(500)
020600060217     D*---
020700001005     D* DS per lettura recordset da selezione SQL
020800001005     D*---
020900040227     D DSTAS         e ds                  inz extname(TITAS00F) PREFIX(S_)
021000060217     D S_ORGCAR        s                   like(ORGCAR)
021100001005     D*---
021200001005     D* DS per scomposizione tabella provincie
021300001005     D*---
021400001207     D DSPR          e ds                  inz
021500060217     D*---
021600060217     D* DS per scomposizione tabella aree
021700060217     D*---
021800060217     D DS05          e ds                  inz
021900040427     D*---
022000040427     D* DS per scomposizione tabella codici eventi (2A)
022100040427     D*---
022200040427     D DS2A          e ds                  inz
022300071023     D*---
022400071023     D* DS per ridefinizione estensione bolla TA4-TRC"A"
022500071023     D*---
022600071023     D DTA4A         e ds                  inz
022700080213     D*---
022800080213     D* DS per ridefinizione file dettagli WFS7W52F
022900080213     D*---
023000080213     D WFS7W52DS     e ds                  inz extname(WFS7W52F)
023100080213     D*---
023200080213     D* DS per semaforizzazione scrittura file generico download TIVGD
023300080213     D*---
023400080213     D trul47ds      E DS
023500040628     D*---
023600040628     D* DS architettura
023700040628     D*---
023800040628     D KPJBA         e ds
023900090609
024000090609     d ds5e          e ds
024100040628     D*----
024200040628     D* DS x passaggio parametri
024300040628     D*----
024400050210     D PARAMETRI       DS                  INZ
024500040628     D  PRMTIS1W2IDS                       LIKE(TIS1W2IDS)
024600040628     D  PRMDAT                       10
024700040628     D  PRMORA                        8
024800050518     D  PRMSUF                        1
024900050518     D  PRMTLG                        2
025000050614     D  PRMFILE                       1
025100080213     D  PRMVGD                        1
025200080213     D  PRMCLIVAS                     8
025300001003     D*------------
025400001003     D* COMANDI
025500001003     D*------------
025600001005     D CMD             S             80    DIM(4) CTDATA PERRCD(1)
025700001003     C*--------------------------------------------------------------------------------------------*
025800001003     C* Main lines
025900001003     C*--------------------------------------------------------------------------------------------*
026000071023     C*
026100071023     C* Definisco le opzioni con cui verranno d seguito utilizzate le istruzioni SQL
026200071023     C
026300071023     C/EXEC SQL
026400080214     C+ SET OPTION DYNUSRPRF = *OWNER, CLOSQLCSR = *ENDMOD
026500071023     C/END-EXEC
026600050614     C*
026700050614     C* eseguo operazioni iniziali
026800050614     C                   exsr      opeini
026900011029     C*
027000011029     C* apre i file
027100011029     C                   open      tabel00f
027200011029     C                   open      azorg01l
027300011029     C*
027400011029     C* Esegue prime considerazioni sui parametri ricevuti
027500011029     C                   exsr      norminput
027600011029     C*
027700011029     C* carica le tabelle occorrenti
027800011029     C                   exsr      cartab
027900011029     C*
028000011029     C* imposta i parametri di lancio (servono per l'ORVPRTF)
028100011029     C                   exsr      imppar                                       *impsta parametri
028200011029     C* procede
028300001003     C                   exsr      procedi
028400011029     C*
028500011029     C* chide i file
028600011029     C                   close     tabel00f
028700011029     C                   close     azorg01l
028800001003     C*
028900001003     C                   seton                                        LR
029000011026     C*--------------------------------------------------------------------------------------------*
029100001003     C* PROCEDI - Routine di elaborazione
029200011026     C*--------------------------------------------------------------------------------------------*
029300001003     C     PROCEDI       BEGSR
029400001003     C*
029500040628     C* Solo se SGIUSER <> 0 => Eseguo le operazioni di sovrapposizione del printer-file
029600040628     C                   IF        RQSCID <> *zeros
029700001006     C                   Z-ADD     400           LENGH            15 5
029800001006     C                   CALL(e)   'QCMDEXC'                                    *LANCIA CMD
029900001006     C                   PARM                    varcmd
030000001003     C                   PARM                    LENGH
030100040628     C                   ENDIF
030200001003     C*
030300011029     C* Se riuscito comando, procede
030400011029if  1C                   if        not %error
030500080213     C*
030600080213     C* Inizializzo variabili d wrk
030700080213     C                   movel     'N'           wProcedi          1
030800080213     C*
030900080213     C* Avvio il blocco elaborazione TIVGD x tipo file: 'W5'
031000080214     C                   if        prmVGD = 'S'
031100080213     C                   clear                   trul47ds
031200080213     C                   eval      d47opz  = 'I'
031300080213     C                   eval      d47tip  = 'W5'
031400080213     C                   eval      d47lck  = 'N'
031500080213     C                   eval      d47chkj = 'N'
031600080213     C                   eval      d47pgm  = 'TIS7W5R'
031700080213     C                   call      'TRUL47R'
031800080213     C                   parm                    trul47ds
031900080213     C*
032000080213     C* Se elaborazione consentita => proseguo
032100080213     C                   if        d47sts <> 'A'
032200080213     C                   movel     'S'           wProcedi
032300080213     C                   endif
032400080214     C                   endif
032500080213     C*
032600080213     C* Se ok a procedere => elaboro
032700080214     C                   if        prmVGD   <> 'S' OR
032800080214     C                             wProcedi  = 'S'
032900011029     C*
033000011029     C* apre i file
033100050524     C                   open      TIS7W4P
033200050215     C                   open      prtf198
033300001005     C                   open      titas30c
033400011005     C                   open      tita430c
033500050321     C                   open      tigcp51l
033600050210     C                   open      fnevb04l
033700011029     C*
033800011029     C* stampa la testata
033900040427     C                   exsr      states
034000040427     C*
034100040227     C                   move      KSC           NumKSC            7 0
034200001003     C*
034300001003     C/EXEC SQL
034400060217     C+ declare C1 cursor for
034500060217     C+ select TAS.*, orgcar  from (
034600040427     C+ select * from titas00f where
034700040227     C+ tasaas*10000+tasmgs >= :PARDAI and
034800050518     C+ tasaas*10000+tasmgs <= :PARDAF and
034900050526     C+ (tasksc in (select s7w5ksc from wfs7w50f) or
035000050526     C+  tasccm in (select s7w5ksc from wfs7w50f))
035100071024     C+ union
035200071024     C+ select * from titas10f where
035300071024     C+ tasaas*10000+tasmgs >= :PARDAI and
035400071024     C+ tasaas*10000+tasmgs <= :PARDAF and
035500071024     C+ (tasksc in (select s7w5ksc from wfs7w50f) or
035600071024     C+  tasccm in (select s7w5ksc from wfs7w50f))
035700071024     C+ union
035800071024     C+ select * from titasP0f where
035900071024     C+ tasaas*10000+tasmgs >= :PARDAI and
036000071024     C+ tasaas*10000+tasmgs <= :PARDAF and
036100071024     C+ (tasksc in (select s7w5ksc from wfs7w50f) or
036200071024     C+  tasccm in (select s7w5ksc from wfs7w50f))
036300060217     C+ ) as TAS join azorg00f
036400060217     C+ on taslna=orgfil
036500060217     C+ order by orgcar, taslna
036600001003     C+ for fetch only
036700001003     C/END-EXEC
036800001003     C
036900001003     C/EXEC SQL
037000001003     C+ open C1
037100001003     C/END-EXEC
037200001003     C
037300001003     C/EXEC SQL
037400060217     C+ Fetch C1 into :dstas, :s_orgcar
037500001003     C/END-EXEC
037600011029     c*
037700011029do  2C                   dow       sqlcod = *zeros
037800060217     C*
037900060217     C* operazioni per nuova area
038000060217     c                   exsr      neware
038100060217     C*
038200060217     C* ciclo fino a rottura area
038300060217do  3C                   dow       sqlcod = *zeros   AND
038400060217     c                             depare = S_orgcar
038500011026     C*
038600011026     C* operazioni per nuovo punto operativo di arrivo
038700011026     c                   exsr      newpoa
038800011026     C*
038900011026     C* ciclo fino a rottura punto operativo di arrivo
039000060217do  4C                   dow       sqlcod = *zeros   AND
039100040227     c                             deppoa = S_taslna
039200011026     C*
039300011026     C* controlla validità record
039400011026     C                   exsr      chkvta
039500011026     C*
039600011029     C* memorizza i dati
039700060217     C                   if        errl00 = '0'
039800011026     C                   exsr      membol
039900060217     C                   endif
040000001003     C*
040100001003     C/EXEC SQL
040200060217     C+ Fetch C1 into :dstas, :s_orgcar
040300001003     C/END-EXEC
040400060217e   4C                   enddo                                                  *rottura linea
040500011029     C*
040600011029     C* stampa il totale punto operativo di arrivo
040700060217     c                   if        $mpoa = 'S'                                  *memorizzato qc
040800011029     C                   exsr      perpoa
040900011029     C                   exsr      stapoa
041000060217     c                   endif
041100060217     C*
041200060221e   3C                   enddo                                                  *rottura area
041300060217     C*
041400060217     C* stampa il totale punto operativo di arrivo
041500060217if  3c                   if        $mpoa = 'S'                                  *memorizzato qc
041600060217     C                   exsr      perare
041700060217     C                   exsr      staare
041800060217e   3c                   endif
041900011029     c*
042000011029e   2C                   enddo                                                  *fine file
042100001003     C*
042200001003     C/EXEC SQL
042300001003     C+ close C1
042400001003     C/END-EXEC
042500011029     C*
042600011029     C* stampa il totale generale
042700011029if  2c                   if        $mgen = 'S'                                  *memorizzato qc
042800011029     C                   exsr      pergen
042900011029     C                   exsr      stagen
043000011029e   2c                   endif
043100011029     C*
043200011029     C* stampa la riga finale
043300011029     c                   exsr      stafin
043400001003     C*
043500011029     C* chiude i file
043600050524     C                   close     TIS7W4P
043700050215     C                   close     prtf198
043800001005     C                   close     titas30c
043900011005     C                   close     tita430c
044000050321     C                   close     tigcp51l
044100050210     C                   close     fnevb04l
044200080213     C*
044300080213     C                   endif
044400080213     C*
044500080213     C* Infine eliminazione "blocco TRUL47"
044600080213     C                   EXSR      endTRUL47
044700001005     C*
044800040628     C* Solo se effettuate in precedenza cancello le operazioni di sovrapposizione del printer-file
044900040628     C                   IF        RQSCID <> *zeros
045000001005     C                   Z-ADD     80            LENGH            15 5
045100001005     C                   CALL(e)   'QCMDEXC'                                    *LANCIA CMD
045200001006     C                   PARM                    CMD(3)
045300001005     C                   PARM                    LENGH
045400040628     C                   ENDIF
045500011029e   1C                   endif
045600001003     C*
045700001003     C                   ENDSR
045800080213     C*--------------------------------------------------------------------------------------------*
045900080213     C* RILASCIA LA SEMAFORIZZAZIONE X IL TIVGD
046000080213     C*--------------------------------------------------------------------------------------------*
046100080213     C     endTRUL47     BEGSR
046200080213     C*
046300080213     C                   if        wProcedi = 'S'
046400080214     C*
046500080214     C* Sancisco il commit
046600080214     C                   commit
046700080213     C*
046800080213     C* Elimino il blocco elaborazione TIVGD x tipo file: 'W5'
046900080213     C                   clear                   trul47ds
047000080213     C                   eval      d47opz  = 'F'
047100080213     C                   eval      d47tip  = 'W5'
047200080213     C                   call      'TRUL47R'
047300080213     C                   parm                    trul47ds
047400080213     C*
047500080213     C                   endif
047600080213     C*
047700080213     C                   ENDSR
047800001003     C*--------------------------------------------------------------------------------------------*
047900001003     C* NORMALIZZA I CAMPI RICEVUTI IN INPUT
048000001003     C*--------------------------------------------------------------------------------------------*
048100001003     C     norminput     BEGSR
048200011128     c*
048300011128     C* data fine, se non inserita = data corrente
048400011128     C                   Z-ADD     datcor        pardaf
048500011128     c*
048600011128     c* controlla se la data fine è numerica
048700011128     c                   MOVEL     Dfi           dsdate
048800011128     c                   MOVEL     dsanne        dsanna
048900011128     c                   MOVEL     dsmese        dsmesa
049000011128     c                   MOVEL     dsgioe        dsgioa
049100011128     c                   MOVE      dsdata        a1                             *ultimo carattere
049200011128     c                   SETOFF                                       98
049300011128     c                   TESTN                   dsdata               98
049400011128if  1c                   IF        *in98  AND                                   *da TESTN è numero
049500011128     c                             a1 >= '0'                                    *NON trovata lettera
049600011128     c                   MOVEL     dsanna        dsann
049700011128     c                   MOVEL     dsmesa        dsmes
049800011128     c                   MOVEL     dsgioa        dsgio
049900011128     c                   Z-ADD     dsdat         g08inv
050000011128     c                   Z-ADD     *zeros        g08dat
050100011128     C                   MOVEL     '3'           g08err
050200011128     c                   CALL      'XSRDA8'
050300011128     c                   PARM                    wlbda8
050400011128if  2c                   IF        g08err <> '1'                                *no errore
050500011128     c                   Z-ADD     g08inv        pardaf
050600011128e   2c                   ENDIF
050700011128e   1c                   ENDIF
050800011128     c*
050900011128     c* data inizio, se nessuna richiesta = data corrente - 1 anno
051000011128     c                   Z-ADD     datcor        dsdat
051100011128     c                   EVAL      dsann = dsann - 1
051200011128     c                   Z-ADD     dsdat         pardai                         *nessuna richiesta
051300011128     c*
051400011128     c* controlla se la data inizio è numerica
051500011128     c                   MOVEL     Din           dsdate
051600011128     c                   MOVEL     dsanne        dsanna
051700011128     c                   MOVEL     dsmese        dsmesa
051800011128     c                   MOVEL     dsgioe        dsgioa
051900011128     c                   MOVE      dsdata        a1                             *ultimo carattere
052000011128     c                   SETOFF                                       98
052100011128     c                   TESTN                   dsdata               98
052200011128if  1c                   IF        *in98  AND                                   *da TESTN è numero
052300011128     c                             a1 >= '0'                                    *NON trovata lettera
052400011128     c                   MOVEL     dsanna        dsann
052500011128     c                   MOVEL     dsmesa        dsmes
052600011128     c                   MOVEL     dsgioa        dsgio
052700011128     c                   Z-ADD     dsdat         g08inv
052800011128     c                   Z-ADD     *zeros        g08dat
052900011128     C                   MOVEL     '3'           g08err
053000011128     c                   CALL      'XSRDA8'
053100011128     c                   PARM                    wlbda8
053200011128if  2c                   IF        g08err <> '1'                                *no errore
053300011128     c                   Z-ADD     g08inv        pardai
053400011128e   2c                   ENDIF
053500011128e   1c                   ENDIF
053600001003     C*
053700001003     C* pulisce le variabili di lavoro
053800001003     C                   CLEAR                   ndst
053900001003     C                   CLEAR                   nmit
054000001003     C                   CLEAR                   nprd
054100001003     C                   CLEAR                   nlod
054200001003     C*
054300001003     C* destinatario
054400001003     C     minu:maiu     XLATE     Dst           ndst                           *Minus -> Maiuscolo
054500001003     C*
054600001003     C* mittente
054700001003     C     minu:maiu     XLATE     Mit           nmit                           *Minus -> Maiuscolo
054800001003     C*
054900001003     C* provincia di arrivo
055000001003     C     minu:maiu     XLATE     Prd           nprd                           *Minus -> Maiuscolo
055100001003     C*
055200001003     C* località di arrivo
055300001003     C     minu:maiu     XLATE     Lod           nlod                           *Minus -> Maiuscolo
055400001003     C*
055500001003     C* data fine + data inizio, deve esserci congruenza
055600001003if  2C                   if        pardai > pardaf                              *incongruenza
055700001003     C                   z-add     pardaf        pardai                         *data fine = inizio
055800001003e   1C                   endif
055900001003     C*
056000001003     C                   ENDSR
056100001003     C*--------------------------------------------------------------------------------------------*
056200001005     C* ESEGUO CHECK X VERIFICARE SE CONSIDERARE O MENO IL RECORD CORRENTE  SU TIVTA
056300001003     C*--------------------------------------------------------------------------------------------*
056400001003     C     chkvta        BEGSR
056500010926     C*
056600001003     C                   movel     '0'           errl00            1
056700001003     C*
056800011026     C* Controllo se richiesto un particolare P.O. di arrivo (se 999 = Tutti i PO di arrivo)
056900001003     C                   if        errl00 = '0'
057000011026     C                   if        lna > *zeros AND
057100011026     c                             lna <> 999
057200040227     C                   if        S_taslna <> lna
057300001003     C                   movel     '1'           errl00
057400001003     C                   endif
057500001003     C                   endif
057600001003     C                   endif
057700001003     C*
057800001003     C* Controllo se richiesta una particolare provincia di arrivo
057900001003     C                   if        errl00 = '0'
058000001003     C                   if        %trim(nprd) > *blanks
058100040227     C                   if        %trim(S_tasprd) <> %trim(nprd)
058200001003     C                   movel     '1'           errl00
058300001003     C                   endif
058400001003     C                   endif
058500001003     C                   endif
058600001003     C*
058700001003     C* Controllo se richiesta un particolare destinatario
058800001003     C                   if        errl00 = '0'
058900001003     C                   if        %trim(ndst) > *blanks
059000040227     C                   eval      a35 = %trim(S_tasrsd)
059100011213     C                   eval      pos = %scan(%trim(ndst):a35)
059200001005     C                   if        pos <= *zeros
059300001003     C                   movel     '1'           errl00
059400001003     C                   endif
059500001003     C                   endif
059600001003     C                   endif
059700001005     C*
059800001005     C* Controllo se richiesta una particolare località di arrivo
059900001005     C                   if        errl00 = '0'
060000001005     C                   if        %trim(nlod) > *blanks
060100040227     C                   eval      a35 = %trim(S_taslod)
060200011213     C                   eval      pos = %scan(%trim(nlod):a35)
060300001005     C                   if        pos <= *zeros
060400001005     C                   movel     '1'           errl00
060500001005     C                   endif
060600001005     C                   endif
060700001005     C                   endif
060800050526     C*
060900050526     C* Controllo che il tipo bolla sia tra quelli consentiti
061000050526     C                   if        S_tastbl <> 'AR' and
061100050526     C                             S_tastbl <> 'A3' and
061200050526     C                             S_tastbl <> 'F3' and
061300050526     C                             S_tastbl <> 'AP' and
061400050526     C                             S_tastbl <> 'FC'
061500050526     C                   else
061600050526     C                   movel     '1'           errl00
061700050526     C                   endif
061800050526     C*
061900050526     C* Non considera se bolla non è del cliente richiesto
062000050526     C*                  if        errl00 = '0'
062100050526     C*    S_tasksc      lookup    sksc7                                  56
062200050526     C*    S_tasccm      lookup    sksc7                                  57
062300050526     C*                  if        *in56 = *on OR *in57 = *on
062400050526     C*                  else
062500050526     C*                  movel     '1'           errl00
062600050526     C*                  endif
062700050526     C*                  endif
062800041122     C*
062900041122     C* NN considera le bolle legate
063000041122     C                   if        errl00 = '0'
063100041122     C                   eval      klbaas = S_tasAAS
063200041122     C                   eval      klblnp = S_tasLNP
063300041122     C                   eval      klbnrs = S_tasNRS
063400041122     C                   eval      klbnsp = S_tasNSP
063500041122     C     KEYlbl1C      chain     fnlbl01l
063600041122     C                   if        %found(fnlbl01l)
063700041122     C                   movel     '1'           errl00
063800041122     C                   endif
063900041122     C                   endif
064000011026     C*
064100050526     C* Legge e controlla la bolla
064200040227     C                   if        errl00 = '0'
064300040227     C                   eval      dsdat = S_tasaas*10000+S_tasmgs
064400011026     C                   z-add     dsann         ktaaas
064500040227     C                   z-add     S_taslnp      ktalnp
064600040227     C                   z-add     S_tasnrs      ktanrs
064700040227     C                   z-add     S_tasnsp      ktansp
064800040227     C                   movel     S_tastbl      ktatbl
064900011026     C     keytas        chain     titas30c
065000011026     C                   if        %found
065100011026     C                   exsr      chktas
065200011026     C                   endif
065300011026     C                   endif
065400001003     C*
065500001003     C                   ENDSR
065600001005     C*--------------------------------------------------------------------------------------------*
065700001005     C* ESEGUO CHECK X VERIFICARE SE CONSIDERARE O MENO IL RECORD CORRENTE SU TITAS
065800001005     C*--------------------------------------------------------------------------------------------*
065900001005     C     chktas        BEGSR
066000010926     c*
066100010926     c* REimposta le variabili di lavoro
066200011026     C                   movel     '0'           errl00
066300010926     c                   EVAL      $statoOK = 'N'                               *stato NON ok
066400011005     c                   EVAL      $notgia  = 'N'                               *note "giacenza"
066500001005     C*
066600011128     C* Consegnate - NORMALI
066700001005     C                   if        errl00 = '0'
066800011128     C                   if        tasdcm > *zeros AND
066900011128     c                             tascca  = *blanks                            *SENZA anomalie
067000010926     C                   if        ckc <> *blanks
067100010926     c                   EVAL      $statoOK = 'S'                               *stato OK
067200001005     C                   endif
067300010926     C                   endif
067400001005     C                   endif
067500011128     c*
067600011128     C* consegnate - ANOMALE
067700011128     C                   if        errl00 = '0'
067800011128     C                   if        tasdcm > *zeros AND
067900011128     c                             tascca  <> *blanks                           *CON   anomalie
068000011128     C                   if        ckm <> *blanks
068100011128     c                   EVAL      $statoOK = 'S'                               *stato OK
068200011128     C                   endif
068300011128     C                   endif
068400011128     C                   endif
068500021108     c*
068600021108     c* reperisce evenutale GIACENZA
068700021108     c                   EXSR      repgcp
068800011026     c*
068900011005     c* reperisce motivi della NON consegna per GIACENZA
069000011005     c                   Z-ADD     tasaas        ka4aas
069100011005     c                   Z-ADD     taslnp        ka4lnp
069200011005     c                   Z-ADD     tasnrs        ka4nrs
069300011005     c                   Z-ADD     tasnsp        ka4nsp
069400011005     c     ke2ta4        SETLL     tita430c
069500011005     c     ke2ta4        READE     tita430c                               98
069600011005do  1c                   DOW       NOT *in98
069700011005if  2c                   IF        ta4trc = 'G' OR                              *Giacenza
069800011005     c                             ta4trc = 'H'
069900011005     c                   EVAL      $notgia = 'S'                                *stato GIACIENTE
070000011005e   2c                   ENDIF
070100011005     c     ke2ta4        READE     tita430c                               98
070200011005e   1c                   ENDDO
070300001005     C*
070400001005     C* Non consenate - Giacenti
070500001005     C                   if        errl00 = '0'
070600011005if  1C                   IF        tasdcm = *zeros   AND
070700021108     C                             (tasfgc<>*blanks OR $notgia='S' OR $gcp='S')
070800010926     C                   if        ckg <> *blanks
070900010926     c                   EVAL      $statoOK = 'S'                               *stato OK
071000001005     C                   endif
071100010926     C                   endif
071200001005     C                   endif
071300040427     C*
071400040427     C* Non consenate
071500040427     C* - Altre
071600040427     C                   if        errl00 = '0'
071700040427if  1C                   IF        tasdcm = *zeros AND
071800040427     C                             (tasfgc=*blank AND $notgia='N' AND $gcp='N')
071900050210     C*
072000050210     C* reperisce evenutale CONSEGNA PARZIALE
072100050210     C                   EXSR      repparz
072200050210     C                   if        ckp <> *blanks
072300050210     C                   EVAL      $statoOK = 'S'                               *stato OK
072400050210     C                   endif
072500050210     C*
072600050210     C* verifica evenutale APPUNTAMENTO
072700050310     C                   if        tasftc = 'A' OR tastc2 = 'A'
072800050310     C                   if        ckt <> *blanks
072900050210     C                   EVAL      $statoOK = 'S'                               *stato OK
073000050310     C                   endif
073100050210     C                   endif
073200040427     C*
073300040427     C* reperisce evenutale LASCIATO AVVISO
073400040427     C                   EXSR      replav
073500050310     C                   if        $lav='S'
073600050310     C                   if        ckl <> *blanks
073700040427     C                   EVAL      $statoOK = 'S'                               *stato OK
073800040427     C                   endif
073900050310     C                   endif
074000050310     C*
074100050310     C* se scelte ALTRE record cmq sempre da considerare
074200050310     C                   if        cka <> *blanks
074300050310     C                   EVAL      $statoOK = 'S'                               *stato OK
074400050310     C                   endif
074500050210     C*
074600040427     C                   endif
074700040427     C                   endif
074800080606     C*
074900080606     C* Tipo servizio
075000080606     C                   if        errl00 = '0'
075100080606     C                   if        (SERVIZIO_C <> *blanks     AND
075200080606     C                              tasTSP      = SERVIZIO_C)  OR
075300080606     C                             (SERVIZIO_D <> *blanks     AND
075400080606     C                              tasTSP      = SERVIZIO_D)  OR
075500080606     C                             (SERVIZIO_E <> *blanks     AND
075600091111     C                              tasTSP      = SERVIZIO_E)  OR
075700091111     C                             (SERVIZIO_H <> *blanks     AND
075800091111     C                              tasTSP      = SERVIZIO_H)
075900080606     C                   else
076000080606     C                   movel     '1'           errl00
076100080606     C                   endif
076200080606     C                   endif
076300010926     c*
076400010926     C                   if        errl00 = '0'
076500010926     c                   if        $statoOK <> 'S'
076600010926     C                   movel     '1'           errl00
076700010926     C                   endif
076800010926     C                   endif
076900001005     C*
077000001005     C                   ENDSR
077100021108     c*--------------------------------------------------------------------------------------------*
077200021108     c* reperisce la giacenza
077300021108     c*--------------------------------------------------------------------------------------------*
077400021108     c     repgcp        BEGSR
077500021108     c*
077600021108     c* imposta le variabili di lavoro
077700050321     c                   EVAL      $gcp = 'N'                                   NO giacenza in TIGCP
077800021108     c*
077900021108     c* legge la giacenza
078000021108     c                   Z-ADD     tasaas        kcpaas
078100021108     c                   Z-ADD     taslnp        kcplnp
078200021108     c                   Z-ADD     tasnrs        kcpnrs
078300021108     c                   Z-ADD     tasnsp        kcpnsp
078400021108     c                   Z-ADD     *zeros        kcpfrg
078500050321     c     keygcp        CHAIN     tigcp51l                           99
078600021108if  1c                   IF        NOT *in99        AND                         *esistente
078700021108     c                             gcpatb = *blanks AND                         *no annullata
078800021108     c                             gcptcm <> 'Z'
078900021108     c                   MOVEL     'S'           $gcp
079000021108e   1c                   ENDIF
079100021108     c*
079200021108     c                   ENDSR
079300040427     c*--------------------------------------------------------------------------------------------*
079400040427     c* reperisce se ultimo evento è di tipo lasciato avviso
079500040427     c*--------------------------------------------------------------------------------------------*
079600040427     c     replav        BEGSR
079700040427     c*
079800040427     c* imposta le variabili di lavoro
079900040427     c                   EVAL      $lav = 'N'                                   NO evento d tipo L/A
080000040427     c*
080100050210     c* legge gli eventi
080200040427     c                   Z-ADD     tasaas        kevaas
080300040427     c                   Z-ADD     taslnp        kevlnp
080400040427     c                   Z-ADD     tasnrs        kevnrs
080500040427     c                   Z-ADD     tasnsp        kevnsp
080600050210     c     keyevb        SETLL     fnevb04l                               99
080700050210     c                   IF        %equal(fnevb04l)
080800050210     c     keyevb        READE     fnevb04l
080900050210     c                   DOW       not %eof(fnevb04l)
081000040427     c                   IF        evbatb = *blanks                             *no annullata
081100040427     c     evbCEV        LOOKUP    sk2Alav                                20
081200040427     c                   IF        *IN20 = *ON
081300040427     c                   EVAL      $lav = 'S'                                   evento d tipo L/A
081400040427     c                   ELSE
081500040427     c                   EVAL      $lav = 'N'                                   NO evento d tipo L/A
081600040427     c                   ENDIF
081700040427     c                   ENDIF
081800050210     c     keyevb        READE     fnevb04l
081900040427     c                   ENDDO
082000040427     c                   ENDIF
082100040427     c*
082200040427     c                   ENDSR
082300050210     c*--------------------------------------------------------------------------------------------*
082400050210     c* reperisce se la spedizone ha/avuto un evento d consegna parziale
082500050210     c*--------------------------------------------------------------------------------------------*
082600050210     c     repparz       BEGSR
082700050210     c*
082800050210     c* imposta le variabili di lavoro
082900050210     c                   EVAL      $parz = 'N'                                  NO evento Cons.Parz.
083000050210     c*
083100050210     c* legge la giacenza
083200050210     c                   Z-ADD     tasaas        kevaas
083300050210     c                   Z-ADD     taslnp        kevlnp
083400050210     c                   Z-ADD     tasnrs        kevnrs
083500050210     c                   Z-ADD     tasnsp        kevnsp
083600050210     c                   MOVEL(P)  'P'           kevcev
083700050210     c     keyevb2       SETLL     fnevb04l                               99
083800050210     c                   IF        %equal(fnevb04l)
083900050210     c     keyevb2       READE     fnevb04l
084000050210     c                   DOW       not %eof(fnevb04l)
084100050210     c                   IF        evbatb = *blanks                             *no annullata
084200050210     c                   EVAL      $parz= 'S'                                   evento d cons.parz.
084300050210     c                   LEAVE
084400050210     c                   ENDIF
084500050210     c     keyevb2       READE     fnevb04l
084600050210     c                   ENDDO
084700050210     c                   ENDIF
084800050210     c*
084900050210     c                   ENDSR
085000060217     C*--------------------------------------------------------------------------------------------*
085100060217     C* Operazioni per nuova area di arrivo
085200060217     C*--------------------------------------------------------------------------------------------*
085300060217     C     neware        BEGSR
085400060217     C*
085500060217     C* memorizza il nuovo punto operativo di arrivo in un deposito
085600060217     c                   z-add     S_orgcar      depare
085700060217     C*
085800060217     C* reimposta le variabili di lavoro
085900060217     C                   eval      $mpoa = 'N'                                  *NO dati memorizzati
086000060217     c                   clear                   ATOTCON           7 0          *dati memorizzazione
086100060217     c                   clear                   ACONANT           7 0
086200060217     c                   clear                   ACON00            7 0
086300060217     c                   clear                   ACON12            7 0
086400060217     c                   clear                   ACON24            7 0
086500060217     c                   clear                   ACON48            7 0
086600060217     c                   clear                   ACON72            7 0
086700060217     c                   clear                   ACON96            7 0
086800060217     c                   clear                   ACON120           7 0
086900060217     c                   clear                   ATMPMED           5 2
087000060217     c                   clear                   ATOTGICH          7 0
087100060217     c                   clear                   ATOTRESI          7 0
087200060217     c                   clear                   ATOTANO           7 0
087300060217     c                   clear                   ATOTGIA           7 0
087400060217     c                   clear                   ATOTPAR           7 0
087500060217     c                   clear                   ATOTAPP           7 0
087600060217     c                   clear                   ATOTLAV           7 0
087700060217     c                   clear                   ATOTALT           7 0
087800060217     c                   clear                   AORETOT          20 0
087900060217     c*
088000060217     c                   clear                   APERANT           5 2
088100060217     c                   clear                   APER00            5 2
088200060217     c                   clear                   APER12            5 2
088300060217     c                   clear                   APER24            5 2
088400060217     c                   clear                   APER48            5 2
088500060217     c                   clear                   APER72            5 2
088600060217     c                   clear                   APER96            5 2
088700060217     c                   clear                   APER120           5 2
088800060217     C*
088900060217     C                   ENDSR
089000011026     C*--------------------------------------------------------------------------------------------*
089100011026     C* Operazioni per nuovo punto operativo di arrivo
089200011026     C*--------------------------------------------------------------------------------------------*
089300011026     C     newpoa        BEGSR
089400011026     C*
089500011026     C* memorizza il nuovo punto operativo di arrivo in un deposito
089600040227     c                   z-add     S_taslna      deppoa
089700011026     C*
089800011026     C* reimposta le variabili di lavoro
089900011026     C                   eval      $mpoa = 'N'                                  *NO dati memorizzati
090000011029     c                   clear                   TOTCON            7 0          *dati memorizzazione
090100011026     c                   clear                   CONANT            7 0
090200011026     c                   clear                   CON00             7 0
090300011026     c                   clear                   CON12             7 0
090400011026     c                   clear                   CON24             7 0
090500011026     c                   clear                   CON48             7 0
090600011026     c                   clear                   CON72             7 0
090700011026     c                   clear                   CON96             7 0
090800011026     c                   clear                   CON120            7 0
090900011026     c                   clear                   TMPMED            5 2
091000050214     c                   clear                   TOTGICH           7 0
091100050214     c                   clear                   TOTRESI           7 0
091200011026     c                   clear                   TOTANO            7 0
091300011026     c                   clear                   TOTGIA            7 0
091400050210     c                   clear                   TOTPAR            7 0
091500040427     c                   clear                   TOTAPP            7 0
091600040427     c                   clear                   TOTLAV            7 0
091700011026     c                   clear                   TOTALT            7 0
091800011029     c                   clear                   ORETOT           20 0
091900011026     c*
092000011029     c                   clear                   PERANT            5 2
092100011026     c                   clear                   PER00             5 2
092200011026     c                   clear                   PER12             5 2
092300011026     c                   clear                   PER24             5 2
092400011026     c                   clear                   PER48             5 2
092500011026     c                   clear                   PER72             5 2
092600011026     c                   clear                   PER96             5 2
092700011026     c                   clear                   PER120            5 2
092800011026     C*
092900011026     C                   ENDSR
093000001003     C*--------------------------------------------------------------------------------------------*
093100011026     C* Memorizza i dati della bolla
093200001003     C*--------------------------------------------------------------------------------------------*
093300011026     C     membol        BEGSR
093400011029     c*
093500011029     c* imposta il passaggio di memorizzazione dati
093600011029     C                   eval      $mpoa = 'S'                                  *SI per PO arrivo
093700011029     C                   eval      $mgen = 'S'                                  *SI per Generale
093800050214     C                   CLEAR                   $stato
093900071023     C                   MOVEL     *blanks       $AssegnaTip       8
094000071023     C                   MOVEL     *blanks       $AssegnaVal       8
094100071023     C                   MOVEL     *blanks       $TempoBolla       4
094200071023     C                   EVAL      $TempoBolla = %editc(tasnrc:'N')
094300011029     C*---
094400011029     C* Consegnate
094500011029     C*---
094600001027     C                   if        tasdcm > *zeros
094700050214     C* Consegnate ma che hanno avuto giacenza
094800050214     C                   if        (tasfgc<>*blank OR $notgia='S' OR $gcp='S')
094900050214     C                   add       1             TOTGICH                        *PO di arrivo
095000060217     C                   add       1             ATOTGICH                       *AREA di arrivo
095100050214     C                   add       1             GTOTGICH                       *Generale
095200071023     C                   eval      $AssegnaTip = 'CONS.'
095300071023     C                   eval      $AssegnaVal = 'GIAC.CH.'
095400050214     C                   EVAL      $stato = %trim($stato) + 'CG'                *stato CONS.+GIAC.
095500050214     C                   endif
095600011128     c* SENZA anomalia
095700011128     C                   if        tascca = *blanks
095800011029     C                   add       1             TOTCON                         *PO di arrivo
095900060217     C                   add       1             ATOTCON                        *AREA di arrivo
096000011029     C                   add       1             GTOTCON                        *Generale
096100050214     C                   EVAL      $stato = %trim($stato) + 'C'                 *stato CONSEGNATA
096200001027     C                   if        tasnrc < *zeros
096300011029     C                   add       1             CONANT                         *PO di arrivo
096400060217     C                   add       1             ACONANT                        *AREA di arrivo
096500011029     C                   add       1             GCONANT                        *Generale
096600071023     C                   eval      $AssegnaTip = 'CONS.'
096700071023     C                   eval      $AssegnaVal = $TempoBolla
096800011029     C                   eval      ORETOT = oretot + tasnrc
096900060220     C                   eval      AORETOT = aoretot + tasnrc
097000011029     C                   eval      GORETOT = goretot + tasnrc
097100001005     C                   endif
097200001027     C                   if        tasnrc = *zeros
097300011029     C                   add       1             CON00                          *PO di arrivo
097400060217     C                   add       1             ACON00                         *AREA di arrivo
097500011029     C                   add       1             GCON00                         *Generale
097600071023     C                   eval      $AssegnaTip = 'CONS.'
097700071023     C                   eval      $AssegnaVal = $TempoBolla
097800001005     C                   endif
097900001027     C                   if        tasnrc > 0 and tasnrc < 24
098000011029     C                   add       1             CON12                          *PO di arrivo
098100060217     C                   add       1             ACON12                         *AREA di arrivo
098200011029     C                   add       1             GCON12                         *Generale
098300071023     C                   eval      $AssegnaTip = 'CONS.'
098400071023     C                   eval      $AssegnaVal = $TempoBolla
098500011029     C                   eval      ORETOT = oretot + tasnrc
098600060220     C                   eval      AORETOT = aoretot + tasnrc
098700011029     C                   eval      GORETOT = goretot + tasnrc
098800001005     C                   endif
098900001027     C                   if        tasnrc >= 24 and tasnrc < 48
099000011029     C                   add       1             CON24                          *PO di arrivo
099100060217     C                   add       1             ACON24                         *AREA di arrivo
099200011029     C                   add       1             GCON24                         *Generale
099300071023     C                   eval      $AssegnaTip = 'CONS.'
099400071023     C                   eval      $AssegnaVal = $TempoBolla
099500011029     C                   eval      ORETOT = oretot + tasnrc
099600060220     C                   eval      AORETOT = aoretot + tasnrc
099700071023     C                   eval      GORETOT = goretot + tasnrc
099800001005     C                   endif
099900001027     C                   if        tasnrc >= 48 and tasnrc < 72
100000011029     C                   add       1             CON48                          *PO di arrivo
100100060217     C                   add       1             ACON48                         *AREA di arrivo
100200011029     C                   add       1             GCON48                         *Generale
100300071023     C                   eval      $AssegnaTip = 'CONS.'
100400071023     C                   eval      $AssegnaVal = $TempoBolla
100500011029     C                   eval      ORETOT = oretot + tasnrc
100600060220     C                   eval      AORETOT = aoretot + tasnrc
100700011029     C                   eval      GORETOT = goretot + tasnrc
100800001005     C                   endif
100900001027     C                   if        tasnrc >= 72 and tasnrc < 96
101000011029     C                   add       1             CON72                          *PO di arrivo
101100060217     C                   add       1             ACON72                         *AREA di arrivo
101200011029     C                   add       1             GCON72                         *Generale
101300071023     C                   eval      $AssegnaTip = 'CONS.'
101400071023     C                   eval      $AssegnaVal = $TempoBolla
101500011029     C                   eval      ORETOT = oretot + tasnrc
101600060220     C                   eval      AORETOT = aoretot + tasnrc
101700011029     C                   eval      GORETOT = goretot + tasnrc
101800001005     C                   endif
101900001027     C                   if        tasnrc >= 96 and tasnrc < 120
102000011029     C                   add       1             CON96                          *PO di arrivo
102100060217     C                   add       1             ACON96                         *AREA di arrivo
102200011029     C                   add       1             GCON96                         *Generale
102300071023     C                   eval      $AssegnaTip = 'CONS.'
102400071023     C                   eval      $AssegnaVal = $TempoBolla
102500011029     C                   eval      ORETOT = oretot + tasnrc
102600060220     C                   eval      AORETOT = aoretot + tasnrc
102700011029     C                   eval      GORETOT = goretot + tasnrc
102800001005     C                   endif
102900001027     C                   if        tasnrc >= 120
103000011029     C                   add       1             CON120                         *PO di arrivo
103100060217     C                   add       1             ACON120                        *AREA di arrivo
103200011029     C                   add       1             GCON120                        *Generale
103300071023     C                   eval      $AssegnaTip = 'CONS.'
103400071023     C                   eval      $AssegnaVal = $TempoBolla
103500011029     C                   eval      ORETOT = oretot + tasnrc
103600060220     C                   eval      AORETOT = aoretot + tasnrc
103700011029     C                   eval      GORETOT = goretot + tasnrc
103800001005     C                   endif
103900050214     C* CON anomalia
104000001027     C                   else
104100050214     C* CON anomalia -> RESI
104200050214     C                   if        tascca = '2'
104300050214     C                   add       1             TOTRESI                        *PO di arrivo
104400060217     C                   add       1             ATOTRESI                       *AREA di arrivo
104500050214     C                   add       1             GTOTRESI                       *Generale
104600071023     C                   eval      $AssegnaTip = 'CONS.'
104700071023     C                   eval      $AssegnaVal = 'RESO'
104800050214     C                   EVAL      $stato = %trim($stato) + 'R'                 *stato RESO
104900050214     C* CON anomalia -> altre anomalie
105000050214     C                   else
105100011029     C                   add       1             TOTANO                         *PO di arrivo
105200060217     C                   add       1             ATOTANO                        *AREA di arrivo
105300011029     C                   add       1             GTOTANO                        *Generale
105400071023     C                   eval      $AssegnaTip = 'CONS.'
105500071023     C                   eval      $AssegnaVal = 'ANOMALA'
105600050214     C                   EVAL      $stato = %trim($stato) + 'M'                 *stato CONS ANOMALA
105700001027     C                   endif
105800050214     C                   endif
105900040427     C*---
106000040427     C* Non consegnate
106100040427     C*---
106200040427     C                   else
106300040427     C                   if        (tasfgc=*blank AND $notgia='N' AND $gcp='N')
106400050210     C* - Consegna parziale
106500050310     C                   if        $parz = 'S'
106600050310     C                   if        ckp <> *blanks
106700050210     C                   add       1             TOTPAR                         *PO di arrivo
106800060217     C                   add       1             ATOTPAR                        *AREA di arrivo
106900050210     C                   add       1             GTOTPAR                        *Generale
107000071023     C                   eval      $AssegnaTip = 'NON CONS.'
107100071023     C                   eval      $AssegnaVal = 'PARZIALE'
107200050214     C                   EVAL      $stato = %trim($stato) + 'P'                 *stato CONS.PARZ.
107300050310     C                   endif
107400050210     C                   else
107500040427     C* - Appuntamento
107600050310     C                   if        tasftc = 'A' OR tastc2 = 'A'
107700050310     C                   if        ckt <> *blanks
107800040427     C                   add       1             TOTAPP                         *PO di arrivo
107900060217     C                   add       1             ATOTAPP                        *AREA di arrivo
108000040427     C                   add       1             GTOTAPP                        *Generale
108100071023     C                   eval      $AssegnaTip = 'NON CONS.'
108200071023     C                   eval      $AssegnaVal = 'APPUNT.'
108300050214     C                   EVAL      $stato = %trim($stato) + 'T'                 *stato APPUNTAMENTO
108400050310     C                   endif
108500040427     C                   else
108600040427     C* - Lasciato avviso
108700050310     C                   if        $lav = 'S'
108800050310     C                   if        ckl <> *blanks
108900040427     C                   add       1             TOTLAV                         *PO di arrivo
109000060217     C                   add       1             ATOTLAV                        *AREA di arrivo
109100040427     C                   add       1             GTOTLAV                        *Generale
109200071023     C                   eval      $AssegnaTip = 'NON CONS.'
109300071023     C                   eval      $AssegnaVal = 'L.AVVISO'
109400050214     C                   EVAL      $stato = %trim($stato) + 'V'                 *stato LASC./AVV.
109500050310     C                   endif
109600040427     C                   else
109700040427     C* - Altre
109800050211     C                   if        cka <> *blanks
109900040427     C                   add       1             TOTALT                         *PO di arrivo
110000060217     C                   add       1             ATOTALT                        *AREA di arrivo
110100040427     C                   add       1             GTOTALT                        *Generale
110200071023     C                   eval      $AssegnaTip = 'NON CONS.'
110300071023     C                   eval      $AssegnaVal = 'ALTRE'
110400050214     C                   EVAL      $stato = %trim($stato) + 'A'                 *stato ALTRE
110500040427     C                   endif
110600050211     C                   endif
110700040427     C                   endif
110800050210     C                   endif
110900040427     C* Giacenti
111000040427     C                   else
111100050211     C                   if        ckg <> *blanks
111200040427     C                   add       1             TOTGIA                         *PO di arrivo
111300060217     C                   add       1             ATOTGIA                        *AREA di arrivo
111400040427     C                   add       1             GTOTGIA                        *Generale
111500071023     C                   eval      $AssegnaTip = 'NON CONS.'
111600071023     C                   eval      $AssegnaVal = 'GIAC.AP.'
111700050214     C                   EVAL      $stato = %trim($stato) + 'G'                 *stato GIACENTI
111800040427     C                   endif
111900050211     C                   endif
112000040427     C                   endif
112100011029     C*---
112200001005     C* Calcolo il tempo medio di ritardo
112300011029     C*---
112400011029     c* PO di arrivo
112500050310     C                   if        TOTCON = *zeros
112600001116     C                   eval      TMPMED = *zeros
112700001116     C                   else
112800050310     C                   eval      TMPMED = ORETOT / TOTCON
112900001116     C                   endif
113000060217     c* AREA di arrivo
113100060217     C                   if        ATOTCON = *zeros
113200060217     C                   eval      ATMPMED = *zeros
113300060217     C                   else
113400060217     C                   eval      ATMPMED = AORETOT / ATOTCON
113500060217     C                   endif
113600011029     c* Generale
113700050310     C                   if        GTOTCON = *zeros
113800011029     C                   eval      GTMPMED = *zeros
113900011029     C                   else
114000050310     C                   eval      GTMPMED = GORETOT / GTOTCON
114100011029     C                   endif
114200050214     C*
114300050214     C* Stampo il dettaglio bolle considerate con il relativo stato attribuito
114400071023     C***                except    DettBolla
114500071023     C*
114600071023     C* Se richiesta generazione file eseguo pulizia preventiva mirata al cliente
114700071023     C                   if        prmfile = 'S'
114800071023     C                   exsr      WriDett
114900071023     C                   endif
115000050214     C*
115100001003     C                   ENDSR
115200071023     C*--------------------------------------------------------------------------------------------*
115300071023     C* SCRIVE IL FILE DETTAGLIO BOLLE
115400071023     C*--------------------------------------------------------------------------------------------*
115500071023     C     WriDett       BEGSR
115600071023     C*
115700071023     C                   clear                   WFS7W520
115800071023     C*
115900071023     C                   eval      W52KSC     = KSC
116000071023     C                   eval      W52LNA     = tasLNA
116100071023     C*
116200071023     C                   setoff                                           99
116300071023     C                   z-add     1             i
116400071023     C     tasLNA        lookup    sorg(i)                                99
116500071023     C                   if        *in99                                        *trovato
116600071023     C                   movel     sdorg(i)      W52LNADES
116700071023     C                   endif
116800071023     C*
116900091111     C                   eval      W52DSP     = tasAAS*10000+tasMGS
117000071023     C                   eval      W52LNP     = tasLNP
117100071023     C                   eval      W52NRS     = tasNRS
117200071023     C                   eval      W52NSP     = tasNSP
117300071023     C                   eval      W52RMN     = tasRMN
117400071023     C                   eval      W52RSD     = tasRSD
117500071023     C                   eval      W52IND     = tasIND
117600071023     C                   eval      W52CAD     = tasCAD
117700071023     C                   eval      W52LOD     = tasLOD
117800071023     C                   eval      W52PRD     = tasPRD
117900071023     C                   eval      W52DCM     = tasDCM
118000071023     C                   eval      W52TIPO    = $AssegnaTip
118100071023     C                   eval      W52VALORE  = $AssegnaVal
118200071023     C*
118300071023     C                   if        $AssegnaTip = 'CONS.'
118400071023     C                   else
118500071023     C                   z-add     *zeros        W52DCM
118600071023     C                   endif
118700071023     C*
118800071023     C* Reperisco il riferimenti mittente alfanumerico dal relativo file estensioni bolla
118900071023     C                   z-add     tasaas        ka4aas
119000071023     C                   z-add     taslnp        ka4lnp
119100071023     C                   z-add     tasnrs        ka4nrs
119200071023     C                   z-add     tasnsp        ka4nsp
119300071023     C                   movel     'A'           ka4trc
119400071023     C     keyta4        chain     tita430c
119500071023     C                   if        %found(tita430c)
119600071023     C                   eval      DTA4A  = ta4NOT
119700071023     C                   eval      W52RMA = §TA4ARMA
119800071023     C                   endif
119900071023     C*
120000080213     C                   if        prmVGD = 'S'
120100080213     C                   exsr      WRIVGD
120200080213     C                   else
120300071023     C                   write     WFS7W520
120400080213     C                   endif
120500071023     C*
120600071023     C                   ENDSR
120700080213     C*--------------------------------------------------------------------------------------------*
120800080213     C* SCRITTURA FILE DETTAGLI LIVELLI DI SERVIZIO SU TIVGD
120900080213     C*--------------------------------------------------------------------------------------------*
121000080213     C     wriVGD        BEGSR
121100080213     C*
121200080213     C                   clear                   tivgd000
121300080407     C                   eval      vgdDTA = %subst(WFS7W52DS:1:%size(WFS7W52DS))
121400080213     C                   eval      vgdTIP = 'W5'
121500080213     C                   eval      vgdKSU = prmCLIVAS
121600080213     C                   eval      vgdTSC = 'WW'
121700080213     C                   eval      vgdDAT = datcor
121800080213     C                   eval      vgdPGM = 'TIS7W5R'
121900080213     C                   write     tivgd000
122000080213     C*
122100080213     C                   ENDSR
122200001005     C*--------------------------------------------------------------------------------------------*
122300011029     C* EFFETTUA IL CALCOLO DELLE PERCENTUALI per PO di arrivo
122400001005     C*--------------------------------------------------------------------------------------------*
122500011029     C     perpoa        BEGSR
122600001005     C*
122700001005     C* Non "anomale"
122800001116     C                   eval      DIVIS = TOTCON/100
122900001116     C                   if        DIVIS <> *zeros
123000001006     C                   eval      PERANT = CONANT/DIVIS
123100001006     C                   eval      PER00  = CON00 /DIVIS
123200001006     C                   eval      PER12  = CON12 /DIVIS
123300001006     C                   eval      PER24  = CON24 /DIVIS
123400001006     C                   eval      PER48  = CON48 /DIVIS
123500001006     C                   eval      PER72  = CON72 /DIVIS
123600001006     C                   eval      PER96  = CON96 /DIVIS
123700001006     C                   eval      PER120 = CON120/DIVIS
123800001116     C                   endif
123900011026     C*
124000001005     C                   ENDSR
124100060217     C*--------------------------------------------------------------------------------------------*
124200060217     C* EFFETTUA IL CALCOLO DELLE PERCENTUALI per AREA di arrivo
124300060217     C*--------------------------------------------------------------------------------------------*
124400060217     C     perare        BEGSR
124500060217     C*
124600060217     C* Non "anomale"
124700060217     C                   eval      DIVIS = ATOTCON/100
124800060217     C                   if        DIVIS <> *zeros
124900060217     C                   eval      APERANT = ACONANT/DIVIS
125000060217     C                   eval      APER00  = ACON00 /DIVIS
125100060217     C                   eval      APER12  = ACON12 /DIVIS
125200060217     C                   eval      APER24  = ACON24 /DIVIS
125300060217     C                   eval      APER48  = ACON48 /DIVIS
125400060217     C                   eval      APER72  = ACON72 /DIVIS
125500060217     C                   eval      APER96  = ACON96 /DIVIS
125600060217     C                   eval      APER120 = ACON120/DIVIS
125700060217     C                   endif
125800060217     C*
125900060217     C                   ENDSR
126000011029     C*--------------------------------------------------------------------------------------------*
126100011029     C* EFFETTUA IL CALCOLO DELLE PERCENTUALI per Totale generale
126200011029     C*--------------------------------------------------------------------------------------------*
126300011029     C     pergen        BEGSR
126400011029     C*
126500011029     C* Non "anomale"
126600011029     C                   eval      DIVIS = GTOTCON/100
126700011029     C                   if        DIVIS <> *zeros
126800011029     C                   eval      GPERANT = GCONANT/DIVIS
126900011029     C                   eval      GPER00  = GCON00 /DIVIS
127000011029     C                   eval      GPER12  = GCON12 /DIVIS
127100011029     C                   eval      GPER24  = GCON24 /DIVIS
127200011029     C                   eval      GPER48  = GCON48 /DIVIS
127300011029     C                   eval      GPER72  = GCON72 /DIVIS
127400011029     C                   eval      GPER96  = GCON96 /DIVIS
127500011029     C                   eval      GPER120 = GCON120/DIVIS
127600011029     C                   endif
127700011029     C*
127800011029     C                   ENDSR
127900011029     C*--------------------------------------------------------------------------------------------*
128000011029     C* ESEGUE LE OPERAZIONI DI STAMPA della testata
128100011029     C*--------------------------------------------------------------------------------------------*
128200011029     C     states        BEGSR
128300011029     C*
128400011029     C                   write     s7w4te
128500011029     C*
128600011029     C                   ENDSR
128700011029     C*--------------------------------------------------------------------------------------------*
128800011029     C* ESEGUE LE OPERAZIONI DI STAMPA della riga finale
128900011029     C*--------------------------------------------------------------------------------------------*
129000011029     C     stafin        BEGSR
129100011029     C*
129200011029     C                   write     s7w4fi
129300011029     C*
129400011029     C                   ENDSR
129500001005     C*--------------------------------------------------------------------------------------------*
129600011029     C* ESEGUE LE OPERAZIONI DI STAMPA per PO di arrivo
129700001005     C*--------------------------------------------------------------------------------------------*
129800011029     C     stapoa        BEGSR
129900001005     C*
130000011029     c* decodifica il PO di arrivo
130100011029     c                   Z-ADD     deppoa        ppoa                           *codice
130200011029     C                   movel     *blanks       ppoad                          *descrizione
130300011029     c                   SETOFF                                           99
130400011029     c                   Z-ADD     1             i
130500011029     c     deppoa        LOOKUP    sorg(i)                                99
130600011029if  1c                   IF        *in99                                        *trovato
130700011029     C                   MOVEL     sdorg(i)      ppoad
130800011029e   1c                   ENDIF
130900011029     C*
131000011029     C* porta i campi memorizzati in stampa
131100011029     c                   Z-ADD     TOTCON        PTOTCON                        *valori numerici
131200011029     c                   Z-ADD     CONANT        PCONANT
131300011029     c                   Z-ADD     CON00         PCON00
131400011029     c                   Z-ADD     CON12         PCON12
131500011029     c                   Z-ADD     CON24         PCON24
131600011029     c                   Z-ADD     CON48         PCON48
131700011029     c                   Z-ADD     CON72         PCON72
131800011029     c                   Z-ADD     CON96         PCON96
131900011029     c                   Z-ADD     CON120        PCON120
132000011029     c                   Z-ADD     TMPMED        PTMPMED
132100050214     c                   Z-ADD     TOTGICH       PTOTGICH
132200050214     c                   Z-ADD     TOTRESI       PTOTRESI
132300011029     c                   Z-ADD     TOTANO        PTOTANO
132400011029     c                   Z-ADD     TOTGIA        PTOTGIA
132500050210     c                   Z-ADD     TOTPAR        PTOTPAR
132600040427     c                   Z-ADD     TOTAPP        PTOTAPP
132700040427     c                   Z-ADD     TOTLAV        PTOTLAV
132800011029     c                   Z-ADD     TOTALT        PTOTALT
132900011029     c                   Z-ADD     PERANT        PPERANT                        *valori percentuali
133000011029     c                   Z-ADD     PER00         PPER00
133100011029     c                   Z-ADD     PER12         PPER12
133200011029     c                   Z-ADD     PER24         PPER24
133300011029     c                   Z-ADD     PER48         PPER48
133400011029     c                   Z-ADD     PER72         PPER72
133500011029     c                   Z-ADD     PER96         PPER96
133600011029     c                   Z-ADD     PER120        PPER120
133700050610     C*
133800050610     C* ottengo i valori calcolati
133900060217     C                   eval      W5TOTCONTT = TOTCON+TOTGICH+TOTRESI+
134000060217     C                                          TOTANO+TOTGIA+TOTPAR+TOTAPP+
134100050616     C                                          TOTLAV+TOTALT
134200050616     C                   eval      W5PERCONSL = TOTCON * 100 / W5TOTCONTT
134300050616     C                   eval      W5TOTDADEF = W5TOTCONTT - TOTCON
134400050616     C                   eval      W5TOTCONL  = TOTCON+TOTGICH+TOTRESI+TOTANO
134500050616     C                   eval      W5PERCONL  = W5TOTCONL * 100 / W5TOTCONTT
134600050616     C                   eval      W5TOTNOCON = TOTGIA+TOTPAR+TOTAPP+
134700050616     C                                          TOTLAV+TOTALT
134800050616     C                   eval      W5PERNOCON = W5TOTNOCON * 100 / W5TOTCONTT
134900050610     C*
135000050616     C* scrittura file
135100050616     C                   exsr      wriwfxls
135200050610     C*
135300011029     C* stampa
135400011029     C                   write     s7w4de
135500001005     C*
135600001005     C                   ENDSR
135700060217     C*--------------------------------------------------------------------------------------------*
135800060217     C* ESEGUE LE OPERAZIONI DI STAMPA per AREA di arrivo
135900060217     C*--------------------------------------------------------------------------------------------*
136000060217     C     staare        BEGSR
136100060217     C*
136200060217     c* decodifica l'AREA di arrivo
136300060217     c                   Z-ADD     depare        ppoa                           *codice
136400060217     C                   movel     *blanks       ppoad                          *descrizione
136500060217     c                   SETOFF                                           99
136600060217     c                   Z-ADD     1             i
136700060217     c     depare        LOOKUP    sare(i)                                99
136800060217if  1c                   IF        *in99                                        *trovato
136900060217     C                   MOVEL     sdare(i)      ppoad
137000060217e   1c                   ENDIF
137100060217     C                   EVAL      ppoad = 'TOT ' + %trim(ppoad)
137200060217     C*
137300060217     C* porta i campi memorizzati in stampa
137400060217     c                   Z-ADD     ATOTCON       PTOTCON                        *valori numerici
137500060217     c                   Z-ADD     ACONANT       PCONANT
137600060217     c                   Z-ADD     ACON00        PCON00
137700060217     c                   Z-ADD     ACON12        PCON12
137800060217     c                   Z-ADD     ACON24        PCON24
137900060217     c                   Z-ADD     ACON48        PCON48
138000060217     c                   Z-ADD     ACON72        PCON72
138100060217     c                   Z-ADD     ACON96        PCON96
138200060217     c                   Z-ADD     ACON120       PCON120
138300060217     c                   Z-ADD     ATMPMED       PTMPMED
138400060217     c                   Z-ADD     ATOTGICH      PTOTGICH
138500060217     c                   Z-ADD     ATOTRESI      PTOTRESI
138600060217     c                   Z-ADD     ATOTANO       PTOTANO
138700060217     c                   Z-ADD     ATOTGIA       PTOTGIA
138800060217     c                   Z-ADD     ATOTPAR       PTOTPAR
138900060217     c                   Z-ADD     ATOTAPP       PTOTAPP
139000060217     c                   Z-ADD     ATOTLAV       PTOTLAV
139100060217     c                   Z-ADD     ATOTALT       PTOTALT
139200060217     c                   Z-ADD     APERANT       PPERANT                        *valori percentuali
139300060217     c                   Z-ADD     APER00        PPER00
139400060217     c                   Z-ADD     APER12        PPER12
139500060217     c                   Z-ADD     APER24        PPER24
139600060217     c                   Z-ADD     APER48        PPER48
139700060217     c                   Z-ADD     APER72        PPER72
139800060217     c                   Z-ADD     APER96        PPER96
139900060217     c                   Z-ADD     APER120       PPER120
140000060217     C*
140100060217     C* ottengo i valori calcolati
140200060217     C                   eval      W5TOTCONTT = ATOTCON+ATOTGICH+ATOTRESI+
140300060217     C                                          ATOTANO+ATOTGIA+ATOTPAR+ATOTAPP+
140400060217     C                                          ATOTLAV+ATOTALT
140500060217     C                   eval      W5PERCONSL = ATOTCON * 100 / W5TOTCONTT
140600060217     C                   eval      W5TOTDADEF = W5TOTCONTT - ATOTCON
140700060217     C                   eval      W5TOTCONL  = ATOTCON+ATOTGICH+ATOTRESI+
140800060217     C                                          ATOTANO
140900060217     C                   eval      W5PERCONL  = W5TOTCONL * 100 / W5TOTCONTT
141000060217     C                   eval      W5TOTNOCON = ATOTGIA+ATOTPAR+ATOTAPP+
141100060217     C                                          ATOTLAV+ATOTALT
141200060217     C                   eval      W5PERNOCON = W5TOTNOCON * 100 / W5TOTCONTT
141300060217     C*
141400060217     C* scrittura file
141500060217     C                   exsr      wriwfxls
141600060217     C* stampa
141700060217     C                   write     s7w4de
141800060217     C*
141900060217     C* stampa riga separazine
142000060217     C                   write     s7w4ri
142100060217     C*
142200060217     C                   ENDSR
142300011029     C*--------------------------------------------------------------------------------------------*
142400011029     C* ESEGUE LE OPERAZIONI DI STAMPA per Totale generale
142500011029     C*--------------------------------------------------------------------------------------------*
142600011029     C     stagen        BEGSR
142700011029     C*
142800011029     c* decodifica il Totale generale
142900011029     c                   Z-ADD     *zeros        ppoa                           *codice
143000011029     C                   EVAL      ppoad = 'TOTALE GENERALE'                    *descrizione
143100011029     C*
143200011029     C* porta i campi memorizzati in stampa
143300011029     c                   Z-ADD     GTOTCON       PTOTCON                        *valori numerici
143400011029     c                   Z-ADD     GCONANT       PCONANT
143500011029     c                   Z-ADD     GCON00        PCON00
143600011029     c                   Z-ADD     GCON12        PCON12
143700011029     c                   Z-ADD     GCON24        PCON24
143800011029     c                   Z-ADD     GCON48        PCON48
143900011029     c                   Z-ADD     GCON72        PCON72
144000011029     c                   Z-ADD     GCON96        PCON96
144100011029     c                   Z-ADD     GCON120       PCON120
144200011029     c                   Z-ADD     GTMPMED       PTMPMED
144300050214     c                   Z-ADD     GTOTGICH      PTOTGICH
144400050214     c                   Z-ADD     GTOTRESI      PTOTRESI
144500011029     c                   Z-ADD     GTOTANO       PTOTANO
144600011029     c                   Z-ADD     GTOTGIA       PTOTGIA
144700050210     c                   Z-ADD     GTOTPAR       PTOTPAR
144800040427     c                   Z-ADD     GTOTAPP       PTOTAPP
144900040427     c                   Z-ADD     GTOTLAV       PTOTLAV
145000011029     c                   Z-ADD     GTOTALT       PTOTALT
145100011029     c                   Z-ADD     GPERANT       PPERANT                        *valori percentuali
145200011029     c                   Z-ADD     GPER00        PPER00
145300011029     c                   Z-ADD     GPER12        PPER12
145400011029     c                   Z-ADD     GPER24        PPER24
145500011029     c                   Z-ADD     GPER48        PPER48
145600011029     c                   Z-ADD     GPER72        PPER72
145700011029     c                   Z-ADD     GPER96        PPER96
145800011029     c                   Z-ADD     GPER120       PPER120
145900050610     C*
146000050610     C* ottengo i valori calcolati
146100050616     C                   eval      W5TOTCONTT = GTOTCON+GTOTGICH+GTOTRESI+
146200050610     C                                          GTOTANO+GTOTGIA+GTOTPAR+
146300050610     C                                          GTOTAPP+GTOTLAV+GTOTALT
146400050616     C                   eval      W5PERCONSL = GTOTCON * 100 / W5TOTCONTT
146500050616     C                   eval      W5TOTDADEF = W5TOTCONTT - GTOTCON
146600050616     C                   eval      W5TOTCONL  = GTOTCON+GTOTGICH+GTOTRESI+
146700050610     C                                          GTOTANO
146800050616     C                   eval      W5PERCONL  = W5TOTCONL * 100 / W5TOTCONTT
146900050616     C                   eval      W5TOTNOCON = GTOTGIA+GTOTPAR+GTOTAPP+
147000050610     C                                          GTOTLAV+GTOTALT
147100050616     C                   eval      W5PERNOCON = W5TOTNOCON * 100 / W5TOTCONTT
147200050610     C*
147300050616     C* scrittura file
147400050616     C                   exsr      wriwfxls
147500060217     C*
147600060217     C* stampa riga separazine
147700060217     C                   write     s7w4ri
147800050610     C*
147900011029     C* stampa
148000011029     C                   write     s7w4de
148100060217     C*
148200060217     C* stampa riga separazine
148300060217     C                   write     s7w4ri
148400011029     C*
148500011029     C                   ENDSR
148600050616     C*--------------------------------------------------------------------------------------------*
148700050616     C* SCRIVE IL WRK-FILE X SCARICO DATI SU FILE XLS
148800050616     C*--------------------------------------------------------------------------------------------*
148900050616     C     wriwfxls      BEGSR
149000050616     C*
149100060217     C* Scrivo solo se richiesto nel lancio
149200060217     C                   if        prmfile = 'S'
149300050616     C                   eval      W5KSC      = KSC
149400050616     C                   eval      W5LNA      = PPOA
149500050616     C                   eval      W5LNADES   = PPOAD
149600050616     C                   eval      W5TOTCON   = PTOTCON
149700050616     C                   eval      W5CONANT   = PCONANT
149800050616     C                   eval      W5CON00    = PCON00
149900050616     C                   eval      W5CON12    = PCON12
150000050616     C                   eval      W5CON24    = PCON24
150100050616     C                   eval      W5CON48    = PCON48
150200050616     C                   eval      W5CON72    = PCON72
150300050616     C                   eval      W5CON96    = PCON96
150400050616     C                   eval      W5CON120   = PCON120
150500050616     C                   eval      W5TMPMED   = PTMPMED
150600050616     C                   eval      W5TOTGICH  = PTOTGICH
150700050616     C                   eval      W5TOTRESI  = PTOTRESI
150800050616     C                   eval      W5TOTANO   = PTOTANO
150900050616     C                   eval      W5TOTGIA   = PTOTGIA
151000050616     C                   eval      W5TOTPAR   = PTOTPAR
151100050616     C                   eval      W5TOTAPP   = PTOTAPP
151200050616     C                   eval      W5TOTLAV   = PTOTLAV
151300050616     C                   eval      W5TOTALT   = PTOTALT
151400050616     C                   eval      W5PERANT   = PPERANT
151500050616     C                   eval      W5PER00    = PPER00
151600050616     C                   eval      W5PER12    = PPER12
151700050616     C                   eval      W5PER24    = PPER24
151800050616     C                   eval      W5PER48    = PPER48
151900050616     C                   eval      W5PER72    = PPER72
152000050616     C                   eval      W5PER96    = PPER96
152100050616     C                   eval      W5PER120   = PPER120
152200050617     C*
152300050617     C* Scarico il buffer del file
152400050617     C                   write     WFS7W510
152500060217     C                   endif
152600050616     C*
152700050616     C                   ENDSR
152800011029     C*--------------------------------------------------------------------------------------------*
152900011029     C* CARICA LE TABELLE OCCORRENTI
153000011029     C*--------------------------------------------------------------------------------------------*
153100011029     C     cartab        BEGSR
153200011029     c*---
153300011029     c* Organigramma
153400011029     c*---
153500011029     c                   CLEAR                   i                              *indice
153600011029     C                   CLEAR                   sorg
153700011029     C                   CLEAR                   sdorg
153800011029     c     *loval        SETLL     azorg01l
153900011029     c                   READ      azorg01l                               99
154000011029do  1c                   DOW       NOT *in99
154100011029if  2c                   IF        orgfva = *blanks AND
154200011029     c                             orgfag <> 'V'
154300011029     c                   ADD       1             i
154400011029     c                   Z-ADD     orgfil        sorg(i)
154500011029     c                   MOVEL     orgdes        sdorg(i)
154600011029e   2c                   ENDIF
154700011029     c                   READ      azorg01l                               99
154800011029e   1c                   ENDDO
154900060217     c*---
155000060217     c* Aree
155100060217     c*---
155200060217     c                   CLEAR                   i                              *indice
155300060217     C                   CLEAR                   sare
155400060217     C                   CLEAR                   sdare
155500060217     C                   Z-ADD     1             tute
155600060217     C                   MOVEL     '05'          tcod
155700060217     c     ke2tab        SETLL     tabel00f
155800060217     c     ke2tab        READE     tabel00f                               99
155900060217do  1c                   DOW       NOT *in99
156000060217if  2c                   IF        tblflg = *blanks
156100060217     c                   ADD       1             i
156200060217     c                   MOVEL     tblkey        sare(i)
156300060217     C                   MOVEL     tbluni        DS05
156400060217     c                   MOVEL     §05des        sdare(i)
156500060217e   2c                   ENDIF
156600060217     c     ke2tab        READE     tabel00f                               99
156700060217e   1c                   ENDDO
156800011029     c*---
156900011029     c* Provincie
157000011029     c*---
157100011029     c                   CLEAR                   i                              *indice
157200011029     C                   CLEAR                   sprv
157300011029     C                   CLEAR                   sdprv
157400011029     C                   Z-ADD     1             tute
157500011029     C                   MOVEL     'PR'          tcod
157600011029     c     ke2tab        SETLL     tabel00f
157700011029     c     ke2tab        READE     tabel00f                               99
157800011029do  1c                   DOW       NOT *in99
157900011029if  2c                   IF        tblflg = *blanks
158000011029     c                   ADD       1             i
158100011029     c                   MOVE      tblkey        sprv(i)
158200011029     C                   MOVEL     tbluni        DSPR
158300011029     c                   MOVEL     §prdes        sdprv(i)
158400011029e   2c                   ENDIF
158500011029     c     ke2tab        READE     tabel00f                               99
158600011029e   1c                   ENDDO
158700040427     c*---
158800040427     c* Eventi d tipo "lasciato avviso"
158900040427     c*---
159000040427     C                   clear                   sk2Alav
159100040427     C                   z-add     *zeros        j                 3 0
159200040427     C                   eval      tute = 1
159300040427     C                   eval      tcod = '2A'
159400040427     C     ke2tab        setll     tabel00f
159500040427     C     ke2tab        reade     tabel00f
159600040427     C                   dow       not %eof(tabel00f)
159700040427     C                   movel(p)  tblUNI        ds2a
159800040427     C                   if        §2aFTC = 'A'
159900040427     C                   add       1             j
160000040427     C                   eval      sk2Alav(j) = tblKEY
160100040427     C                   endif
160200040427     C     ke2tab        reade     tabel00f
160300040427     C                   enddo
160400011029     C*
160500011029     c                   ENDSR
160600001005     C*--------------------------------------------------------------------------------------------*
160700001006     C* IMPOSTA I CAMPI DI TESTATA IN STAMPA E VALORIZZO CAMPONE "USRDFNDTA" (per 100 byte)
160800001005     C*--------------------------------------------------------------------------------------------*
160900001207     C     imppar        BEGSR
161000001207     C*
161100001207     C* Pulisce il formato record di testata del PRTF
161200011026     C                   clear                   s7w4te
161300001005     C*
161400001005     C* Inverto le date in input
161500001005     C                   z-add     *zeros        G08DAT
161600001005     C                   z-add     PARDAI        G08INV
161700001005     C                   movel     '3'           G08ERR
161800001005     C                   call      'XSRDA8'
161900001005     C                   parm                    WLBDA8
162000001005     C                   z-add     G08DAT        STPDAI
162100001006     C                   eval      desdai = %trim(%editc(PARDAI:'Z'))
162200001005     C*
162300001005     C                   z-add     *zeros        G08DAT
162400001005     C                   z-add     PARDAF        G08INV
162500001005     C                   movel     '3'           G08ERR
162600001005     C                   call      'XSRDA8'
162700001005     C                   parm                    WLBDA8
162800001005     C                   z-add     G08DAT        STPDAF
162900001006     C                   eval      desdaf = %trim(%editc(PARDAF:'Z'))
163000001005     C*
163100001005     C* Reperisco la descrizione del Punto Operativo e valorizzo i due parametri ad esso riferiti
163200011026     c                   if        lna = 999
163300011026     c                   eval      STPLNAD = 'Tutti'
163400011026     c                   else
163500001030     C     lna           chain     azorg01l
163600001005     C                   if        %found(azorg01l)
163700001005     C                   movel     orgdes        STPLNAD
163800001006     C                   eval      despoa = %trim(%editc(lna:'Z'))
163900001005     C                   endif
164000011026     c                   endif
164100001005     C                   z-add     lna           STPLNA
164200011029     C*
164300011029     C* Reperisco la descrizione della provincia e valorizzo entrambi i parametri ad essa riferiti
164400011029     C                   movel     prd           STPPRD
164500011029     C                   movel     'PR'          tcod
164600011029     C                   movel(p)  prd           tkey
164700011029     C     keytab        chain     tabel00f
164800011029     C                   if        %found(tabel00f)
164900011029     C                   movel     tbluni        DSPR
165000011029     C                   movel     §PRDES        STPPRDD
165100011029     C                   eval      desprd = %trim(nprd)
165200011029     C                   endif
165300080606     C*
165400080606     C* Descrizione filtro x tipo servizio
165500080606     c                   if        SERVIZIO_C <> *blanks AND
165600080606     c                             SERVIZIO_D <> *blanks AND
165700091111     c                             SERVIZIO_E <> *blanks AND
165800091111     c                             SERVIZIO_H <> *blanks
165900080606     c                   eval      STPTSP = 'Tutti'
166000080606     c                   else
166100090609      * tipo servizio 'C'
166200080606     c                   if        SERVIZIO_C <> *blanks
166300090609     c                   clear                   ds5e
166400090609     c                   eval      tcod = '5E'
166500090609     c                   eval      tkey = 'C'
166600090609     c     keytab        chain     tabel00f
166700090609     c                   if        %found(tabel00f)
166800090609     c                   eval      ds5e = tbluni
166900090609     c                   endif
167000090609     c                   eval      STPTSP = %trim(STPTSP)+' '+ §5edes
167100080606     c                   endif
167200090609      * tipo servizio 'D'
167300080606     c                   if        SERVIZIO_D <> *blanks
167400090609     c                   clear                   ds5e
167500090609     c                   eval      tcod = '5E'
167600090609     c                   eval      tkey = 'D'
167700090609     c     keytab        chain     tabel00f
167800090609     c                   if        %found(tabel00f)
167900090609     c                   eval      ds5e = tbluni
168000090609     c                   endif
168100090609     c                   eval      STPTSP = %trim(STPTSP)+' '+ §5edes
168200080606     c                   endif
168300090609      * tipo servizio 'E'
168400080606     c                   if        SERVIZIO_E <> *blanks
168500090609     c                   clear                   ds5e
168600090609     c                   eval      tcod = '5E'
168700090609     c                   eval      tkey = 'E'
168800090609     c     keytab        chain     tabel00f
168900090609     c                   if        %found(tabel00f)
169000090609     c                   eval      ds5e = tbluni
169100090609     c                   endif
169200090609     c                   eval      STPTSP = %trim(STPTSP)+' '+ §5edes
169300080606     c                   endif
169400091111      * tipo servizio 'H'
169500091111     c                   if        SERVIZIO_H <> *blanks
169600091111     c                   clear                   ds5e
169700091111     c                   eval      tcod = '5E'
169800091111     c                   eval      tkey = 'H'
169900091111     c     keytab        chain     tabel00f
170000091111     c                   if        %found(tabel00f)
170100091111     c                   eval      ds5e = tbluni
170200091111     c                   endif
170300091111     c                   eval      STPTSP = %trim(STPTSP)+' '+ §5edes
170400091111     c                   endif
170500091111     c                   endif
170600001005     C*
170700001005     C* Quindi tutti i rimanenti parametri
170800001005     C                   movel     ksc           STPKSU
170900001005     C                   movel     ndst          STPDST
171000001006     C                   eval      desdst = %trim(ndst)
171100001005     C                   movel     nmit          STPMIT
171200001006     C                   eval      desmit = %trim(nmit)
171300001005     C                   movel     lod           STPLOD
171400001006     C                   eval      deslod = %trim(nlod)
171500001005     C                   if        ckc <> *blanks
171600001207     C                   movel     'S'           STPFCO
171700001006     C                   eval      desfl1 = 'CONS.'
171800001005     C                   endif
171900011128     C                   if        ckm <> *blanks
172000011128     C                   movel     'S'           STPFCM
172100011128     C                   eval      desfl1 = 'ANOM.'
172200011128     C                   endif
172300050210     C                   if        ckg <> *blanks
172400050210     C                   eval      STPFLG = %trim(STPFLG) + ' GI'
172500050210     C                   eval      desfl2 = 'GIAC.'
172600050210     C                   endif
172700050210     C                   if        ckp <> *blanks
172800050210     C                   eval      STPFLG = %trim(STPFLG) + ' CP'
172900050210     C                   eval      desfl3 = 'PARZ.'
173000050210     C                   endif
173100050210     C                   if        ckt <> *blanks
173200050210     C                   eval      STPFLG = %trim(STPFLG) + ' AP'
173300050210     C                   eval      desfl4 = 'APP.'
173400050210     C                   endif
173500050210     C                   if        ckl <> *blanks
173600050210     C                   eval      STPFLG = %trim(STPFLG) + ' LA'
173700050210     C                   eval      desfl5 = 'L.AVV'
173800050210     C                   endif
173900050210     C                   if        cka <> *blanks
174000050210     C                   eval      STPFLG = %trim(STPFLG) + ' AL'
174100050210     C                   eval      desfl6 = 'ALTRE'
174200050210     C                   endif
174300001006     C*
174400001006     C* A questo punto compongo la stringa da passare nell'USRDFNDTA e i dati di servizio
174500001006     C                   move      rqscid        sgiusrnbr
174600001006     C                   eval      tipstp = 'REC'
174700001006     C                   eval      tipsnd = 'SGI'
174800001006     C                   eval      email = *blanks
174900001006     C*
175000001009     C                   eval      varcmd=''''+dessta+' '+%trim(prmdat)+' '+
175100001009     C                                        %trim(prmora)
175200001006     C                   if        desdai>*blanks
175300001006     C                   eval      varcmd = %trim(varcmd)+%trim(desdai)+'+'
175400001006     C                   endif
175500001006     C                   if        desdaf>*blanks
175600001006     C                   eval      varcmd = %trim(varcmd)+%trim(desdaf)+'+'
175700001006     C                   endif
175800001006     C                   if        despoa>*blanks
175900001006     C                   eval      varcmd = %trim(varcmd)+%trim(despoa)+'+'
176000001006     C                   endif
176100001006     C                   if        desprd>*blanks
176200001006     C                   eval      varcmd = %trim(varcmd)+%trim(desprd)+'+'
176300001006     C                   endif
176400001006     C                   if        deslod>*blanks
176500001006     C                   eval      varcmd = %trim(varcmd)+%trim(deslod)+'+'
176600001006     C                   endif
176700001006     C                   if        desfl1>*blanks
176800001006     C                   eval      varcmd = %trim(varcmd)+%trim(desfl1)+'+'
176900001006     C                   endif
177000001006     C                   if        desfl2>*blanks
177100001006     C                   eval      varcmd = %trim(varcmd)+%trim(desfl2)+'+'
177200001006     C                   endif
177300001006     C                   if        desfl3>*blanks
177400001006     C                   eval      varcmd = %trim(varcmd)+%trim(desfl3)+'+'
177500001006     C                   endif
177600050210     C                   if        desfl4>*blanks
177700050210     C                   eval      varcmd = %trim(varcmd)+%trim(desfl4)+'+'
177800050210     C                   endif
177900050210     C                   if        desfl5>*blanks
178000050210     C                   eval      varcmd = %trim(varcmd)+%trim(desfl5)+'+'
178100050210     C                   endif
178200050210     C                   if        desfl6>*blanks
178300050210     C                   eval      varcmd = %trim(varcmd)+%trim(desfl6)+'+'
178400050210     C                   endif
178500001006     C                   if        desmit>*blanks
178600001006     C                   eval      varcmd = %trim(varcmd)+%trim(desmit)+'+'
178700001006     C                   endif
178800001006     C                   if        desdst>*blanks
178900001006     C                   eval      varcmd = %trim(varcmd)+%trim(desdst)
179000001006     C                   endif
179100001009     C                   if        %len(%trim(varcmd)) >=97
179200001009     C                   eval      %subst(var100:97:4) = '...'
179300001009     C                   else
179400001009     C                   eval      varcmd = %trim(varcmd)
179500001009     C                   endif
179600001009     C                   movel     varcmd        var100
179700050210     C                   move(p)   param         varsrv
179800001006     C                   eval      varcmd = %trim(CMD(1))+' '+%trim(CMD(2))+
179900001006     C                                      var100 + varsrv+''''+')'
180000001005     C*
180100001005     C                   ENDSR
180200050614     C*--------------------------------------------------------------------------------------------*
180300050614     C* operazioni iniziali
180400050614     C*--------------------------------------------------------------------------------------------*
180500050614     C     opeini        BEGSR
180600050614     C*
180700050614     C* Aggiunge le librerie occorrenti in lista
180800050614     C                   EVAL      wpgm = 'GAITRAOBJ/TIS777C'
180900050614     C                   CALL      wpgm
181000050614     C*--------------------
181100050614     C* CALCOLA LA DATA CORRENTE
181200050614     C*--------------------
181300050614     C                   time                    WN14             14 0          *ORA (6)+ DATA (8)
181400050614     C                   move      WN14          WN8               8 0          *DATA (8) IN G/M/AA
181500050614     C                   z-add     WN8           G08DAT
181600050614     C                   z-add     *zeros        G08INV
181700050614     C                   movel     '0'           G08ERR
181800050614     C                   call      'XSRDA8'
181900050614     C                   parm                    WLBDA8
182000050614     C                   z-add     G08INV        DATCOR            8 0          *DATA CORRENTE AA/M/
182100050614     c*
182200050614     c* IMPOSTAZIONE SCHIERA DEI CLIENTI DA ELABORARE
182300050614     c* Se richiesta elaborazione per cliente unificante richiamo tibs10r
182400050614     c                   clear                   sksc7
182500050614     c                   if        prmsuf = 'U'
182600050614     c                   clear                   tibs10ds
182700050614     c                   eval      d10tle = prmtlg
182800050614     c                   eval      d10paf = 'F'
182900050614     c                   move      ksc           d10cod
183000050614     c                   z-add     datcor        d10drf
183100050614     c                   call      'TIBS10R'
183200050614     c                   Parm                    Tibs10ds
183300050614     c                   if        d10err = *blanks
183400050614     c                   z-add     sksc11        sksc7
183500050614     c                   else
183600050614     c                   move      ksc           sksc7(1)
183700050614     c                   endif
183800050614     c                   else
183900050614     c* altrimenti imposto primo elemnto schiera = codice cliente ricevuto
184000050614     c                   move      ksc           sksc7(1)
184100050614     c                   endif
184200050614     C*
184300050614     C* Scrivo i clienti così estratti nel wrk-file d procedura
184400050614     C                   if        not %open(wfs7w50f)
184500050614     C                   open      wfs7w50f
184600050614     C                   endif
184700050614     C                   z-add     1             wIndice           3 0
184800050614     C                   dow       wIndice <= %elem(sksc7)
184900050614     C                   if        sksc7(wIndice) <> *zeros
185000050614     C                   eval      s7w5ksc = sksc7(wIndice)
185100050614     C                   write     wfs7w500
185200050614     C                   else
185300050614     C                   leave
185400050614     C                   endif
185500050614     C                   eval      wIndice = wIndice + 1
185600050614     C                   enddo
185700050614     C                   if        %open(wfs7w50f)
185800050614     C                   close     wfs7w50f
185900050614     C                   endif
186000050614     C*
186100050614     C* Se richiesta generazione file eseguo pulizia preventiva mirata al cliente
186200050614     C                   if        prmfile = 'S'
186300071023     C* File WFS7W51F
186400050614     C     KSC           setll     wfs7w51f
186500050614     C                   if        %found(wfs7w51f)
186600050614     C     KSC           reade     wfs7w51f
186700050614     C                   dow       not %eof(wfs7w51f)
186800050614     C                   delete    wfs7w51f
186900050614     C     KSC           reade     wfs7w51f
187000050614     C                   enddo
187100050614     C                   endif
187200071023     C* File WFS7W52F
187300071023     C     KSC           setll     wfs7w52f
187400071023     C                   if        %found(wfs7w52f)
187500071023     C     KSC           reade     wfs7w52f
187600071023     C                   dow       not %eof(wfs7w52f)
187700071023     C                   delete    wfs7w52f
187800071023     C     KSC           reade     wfs7w52f
187900071023     C                   enddo
188000071023     C                   endif
188100050614     C                   endif
188200050614     C*
188300050614     C                   ENDSR
188400080214     C*--------------------------------------------------------------------------------------------*
188500080214     C* Gestione errori imprevisti
188600080214     C*--------------------------------------------------------------------------------------------*
188700080214     C     *pssr         BEGSR
188800080214     C*
188900080214     C                   rolbk
189000080214     C*
189100080214     C                   seton                                        LR
189200080214     C*
189300080214     C                   ENDSR     '*CANCL'
189400001003     C*--------------------------------------------------------------------------------------------*
189500001003     C* impostazioni iniziali
189600011029     C*--------------------------------------------------------------------------------------------*
189700001003     C     *inzsr        BEGSR
189800001003     C*
189900001003     C* Ricevimento parametri
190000001003     C     *ENTRY        PLIST
190100040628     C                   PARM                    KPJBA
190200050210     C                   MOVEL     KPJBU         PARAMETRI
190300040628     C*
190400040628     C                   EVAL      TIS1W2IDS = PRMTIS1W2IDS
190500001003     C*
190600001003     C* Chiavi di lettura
190700001003     C     keytas        KLIST                                                  *TITAS30C
190800001003     C                   KFLD                    ktaaas
190900001003     C                   KFLD                    ktalnp
191000001003     C                   KFLD                    ktanrs
191100001003     C                   KFLD                    ktansp
191200001003     C                   KFLD                    ktatbl
191300011005     c     ke2ta4        KLIST                                                  tita430c
191400011005     c                   KFLD                    ka4aas                         -anno spedizione
191500011005     c                   KFLD                    ka4lnp                         -linea partenza
191600011005     c                   KFLD                    ka4nrs                         -serie
191700011005     c                   KFLD                    ka4nsp                         -spedizione
191800071023     c     keyta4        KLIST                                                  tita430c
191900071023     c                   KFLD                    ka4aas                         -anno spedizione
192000071023     c                   KFLD                    ka4lnp                         -linea partenza
192100071023     c                   KFLD                    ka4nrs                         -serie
192200071023     c                   KFLD                    ka4nsp                         -spedizione
192300071023     c                   KFLD                    ka4trc                         -tipo record
192400050321     c     keygcp        KLIST                                                  *tigcp51l
192500021108     c                   KFLD                    kcpaas                         -anno spedizione
192600021108     c                   KFLD                    kcplnp                         -linea partenza
192700021108     c                   KFLD                    kcpnrs                         -serie
192800021108     c                   KFLD                    kcpnsp                         -spedizione
192900021108     c                   KFLD                    kcpfrg                         -prg aperture
193000050210     c     keyevb        KLIST                                                  *fnevb04l
193100040427     c                   KFLD                    kevaas                         -anno spedizione
193200040427     c                   KFLD                    kevlnp                         -linea partenza
193300040427     c                   KFLD                    kevnrs                         -serie
193400040427     c                   KFLD                    kevnsp                         -spedizione
193500050210     c     keyevb2       KLIST                                                  *fnevb04l
193600050210     c                   KFLD                    kevaas                         -anno spedizione
193700050210     c                   KFLD                    kevlnp                         -linea partenza
193800050210     c                   KFLD                    kevnrs                         -serie
193900050210     c                   KFLD                    kevnsp                         -spedizione
194000050210     c                   KFLD                    kevcev                         -codice evento
194100041122     C     keylbl1c      KLIST                                                  *fnlbl01l
194200041122     C                   KFLD                    klbaas
194300041122     C                   KFLD                    klblnp
194400041122     C                   KFLD                    klbnrs
194500041122     C                   KFLD                    klbnsp
194600001005     C*
194700001005     C* Definizione chiavi - TABEL00F
194800001005     C     keytab        KLIST
194900001005     C                   KFLD                    tute
195000001005     C                   KFLD                    tcod
195100001005     C                   KFLD                    tkey
195200011029     C     ke2tab        KLIST
195300011029     C                   KFLD                    tute
195400011029     C                   KFLD                    tcod
195500001003     C*
195600001003     C                   ENDSR
195700011029     O*--------------------------------------------------------------------------------------------*
195800050214     OPRTF198   E            DettBolla        01
195900050214     O                                        +   1 'Chiave bolla:'
196000050214     O                       dskeybol         +   1
196100050214     O                                        +  10 'Stato bolla:'
196200050214     O                       $stato           +   1
196300001005** CMD - COMANDI CL
196400130429OVRPRTF OVRSCOPE(*CALLLVL) FILE(TIS7W4P) TOFILE(TIS7W4P)
196500001006OUTQ(STRATEOBJ/SGIIN) USRDFNDTA(
196600130429DLTOVR FILE(TIS7W4P)  LVL(*)
196700001006
