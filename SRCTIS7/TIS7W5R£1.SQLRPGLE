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
023900040628     D*----
024000040628     D* DS x passaggio parametri
024100040628     D*----
024200050210     D PARAMETRI       DS                  INZ
024300040628     D  PRMTIS1W2IDS                       LIKE(TIS1W2IDS)
024400040628     D  PRMDAT                       10
024500040628     D  PRMORA                        8
024600050518     D  PRMSUF                        1
024700050518     D  PRMTLG                        2
024800050614     D  PRMFILE                       1
024900080213     D  PRMVGD                        1
025000080213     D  PRMCLIVAS                     8
025100001003     D*------------
025200001003     D* COMANDI
025300001003     D*------------
025400001005     D CMD             S             80    DIM(4) CTDATA PERRCD(1)
025500001003     C*--------------------------------------------------------------------------------------------*
025600001003     C* Main lines
025700001003     C*--------------------------------------------------------------------------------------------*
025800071023     C*
025900071023     C* Definisco le opzioni con cui verranno d seguito utilizzate le istruzioni SQL
026000071023     C
026100071023     C/EXEC SQL
026200080214     C+ SET OPTION DYNUSRPRF = *OWNER, CLOSQLCSR = *ENDMOD
026300071023     C/END-EXEC
026400050614     C*
026500050614     C* eseguo operazioni iniziali
026600050614     C                   exsr      opeini
026700011029     C*
026800011029     C* apre i file
026900011029     C                   open      tabel00f
027000011029     C                   open      azorg01l
027100011029     C*
027200011029     C* Esegue prime considerazioni sui parametri ricevuti
027300011029     C                   exsr      norminput
027400011029     C*
027500011029     C* carica le tabelle occorrenti
027600011029     C                   exsr      cartab
027700011029     C*
027800011029     C* imposta i parametri di lancio (servono per l'ORVPRTF)
027900011029     C                   exsr      imppar                                       *impsta parametri
028000011029     C* procede
028100001003     C                   exsr      procedi
028200011029     C*
028300011029     C* chide i file
028400011029     C                   close     tabel00f
028500011029     C                   close     azorg01l
028600001003     C*
028700001003     C                   seton                                        LR
028800011026     C*--------------------------------------------------------------------------------------------*
028900001003     C* PROCEDI - Routine di elaborazione
029000011026     C*--------------------------------------------------------------------------------------------*
029100001003     C     PROCEDI       BEGSR
029200001003     C*
029300040628     C* Solo se SGIUSER <> 0 => Eseguo le operazioni di sovrapposizione del printer-file
029400040628     C                   IF        RQSCID <> *zeros
029500001006     C                   Z-ADD     400           LENGH            15 5
029600001006     C                   CALL(e)   'QCMDEXC'                                    *LANCIA CMD
029700001006     C                   PARM                    varcmd
029800001003     C                   PARM                    LENGH
029900040628     C                   ENDIF
030000001003     C*
030100011029     C* Se riuscito comando, procede
030200011029if  1C                   if        not %error
030300080213     C*
030400080213     C* Inizializzo variabili d wrk
030500080213     C                   movel     'N'           wProcedi          1
030600080213     C*
030700080213     C* Avvio il blocco elaborazione TIVGD x tipo file: 'W5'
030800080214     C                   if        prmVGD = 'S'
030900080213     C                   clear                   trul47ds
031000080213     C                   eval      d47opz  = 'I'
031100080213     C                   eval      d47tip  = 'W5'
031200080213     C                   eval      d47lck  = 'N'
031300080213     C                   eval      d47chkj = 'N'
031400080213     C                   eval      d47pgm  = 'TIS7W5R'
031500080213     C                   call      'TRUL47R'
031600080213     C                   parm                    trul47ds
031700080213     C*
031800080213     C* Se elaborazione consentita => proseguo
031900080213     C                   if        d47sts <> 'A'
032000080213     C                   movel     'S'           wProcedi
032100080213     C                   endif
032200080214     C                   endif
032300080213     C*
032400080213     C* Se ok a procedere => elaboro
032500080214     C                   if        prmVGD   <> 'S' OR
032600080214     C                             wProcedi  = 'S'
032700011029     C*
032800011029     C* apre i file
032900050524     C                   open      TIS7W4P
033000050215     C                   open      prtf198
033100001005     C                   open      titas30c
033200011005     C                   open      tita430c
033300050321     C                   open      tigcp51l
033400050210     C                   open      fnevb04l
033500011029     C*
033600011029     C* stampa la testata
033700040427     C                   exsr      states
033800040427     C*
033900040227     C                   move      KSC           NumKSC            7 0
034000001003     C*
034100001003     C/EXEC SQL
034200060217     C+ declare C1 cursor for
034300060217     C+ select TAS.*, orgcar  from (
034400040427     C+ select * from titas00f where
034500040227     C+ tasaas*10000+tasmgs >= :PARDAI and
034600050518     C+ tasaas*10000+tasmgs <= :PARDAF and
034700050526     C+ (tasksc in (select s7w5ksc from wfs7w50f) or
034800050526     C+  tasccm in (select s7w5ksc from wfs7w50f))
034900071024     C+ union
035000071024     C+ select * from titas10f where
035100071024     C+ tasaas*10000+tasmgs >= :PARDAI and
035200071024     C+ tasaas*10000+tasmgs <= :PARDAF and
035300071024     C+ (tasksc in (select s7w5ksc from wfs7w50f) or
035400071024     C+  tasccm in (select s7w5ksc from wfs7w50f))
035500071024     C+ union
035600071024     C+ select * from titasP0f where
035700071024     C+ tasaas*10000+tasmgs >= :PARDAI and
035800071024     C+ tasaas*10000+tasmgs <= :PARDAF and
035900071024     C+ (tasksc in (select s7w5ksc from wfs7w50f) or
036000071024     C+  tasccm in (select s7w5ksc from wfs7w50f))
036100060217     C+ ) as TAS join azorg00f
036200060217     C+ on taslna=orgfil
036300060217     C+ order by orgcar, taslna
036400001003     C+ for fetch only
036500001003     C/END-EXEC
036600001003     C
036700001003     C/EXEC SQL
036800001003     C+ open C1
036900001003     C/END-EXEC
037000001003     C
037100001003     C/EXEC SQL
037200060217     C+ Fetch C1 into :dstas, :s_orgcar
037300001003     C/END-EXEC
037400011029     c*
037500011029do  2C                   dow       sqlcod = *zeros
037600060217     C*
037700060217     C* operazioni per nuova area
037800060217     c                   exsr      neware
037900060217     C*
038000060217     C* ciclo fino a rottura area
038100060217do  3C                   dow       sqlcod = *zeros   AND
038200060217     c                             depare = S_orgcar
038300011026     C*
038400011026     C* operazioni per nuovo punto operativo di arrivo
038500011026     c                   exsr      newpoa
038600011026     C*
038700011026     C* ciclo fino a rottura punto operativo di arrivo
038800060217do  4C                   dow       sqlcod = *zeros   AND
038900040227     c                             deppoa = S_taslna
039000011026     C*
039100011026     C* controlla validità record
039200011026     C                   exsr      chkvta
039300011026     C*
039400011029     C* memorizza i dati
039500060217     C                   if        errl00 = '0'
039600011026     C                   exsr      membol
039700060217     C                   endif
039800001003     C*
039900001003     C/EXEC SQL
040000060217     C+ Fetch C1 into :dstas, :s_orgcar
040100001003     C/END-EXEC
040200060217e   4C                   enddo                                                  *rottura linea
040300011029     C*
040400011029     C* stampa il totale punto operativo di arrivo
040500060217     c                   if        $mpoa = 'S'                                  *memorizzato qc
040600011029     C                   exsr      perpoa
040700011029     C                   exsr      stapoa
040800060217     c                   endif
040900060217     C*
041000060221e   3C                   enddo                                                  *rottura area
041100060217     C*
041200060217     C* stampa il totale punto operativo di arrivo
041300060217if  3c                   if        $mpoa = 'S'                                  *memorizzato qc
041400060217     C                   exsr      perare
041500060217     C                   exsr      staare
041600060217e   3c                   endif
041700011029     c*
041800011029e   2C                   enddo                                                  *fine file
041900001003     C*
042000001003     C/EXEC SQL
042100001003     C+ close C1
042200001003     C/END-EXEC
042300011029     C*
042400011029     C* stampa il totale generale
042500011029if  2c                   if        $mgen = 'S'                                  *memorizzato qc
042600011029     C                   exsr      pergen
042700011029     C                   exsr      stagen
042800011029e   2c                   endif
042900011029     C*
043000011029     C* stampa la riga finale
043100011029     c                   exsr      stafin
043200001003     C*
043300011029     C* chiude i file
043400050524     C                   close     TIS7W4P
043500050215     C                   close     prtf198
043600001005     C                   close     titas30c
043700011005     C                   close     tita430c
043800050321     C                   close     tigcp51l
043900050210     C                   close     fnevb04l
044000080213     C*
044100080213     C                   endif
044200080213     C*
044300080213     C* Infine eliminazione "blocco TRUL47"
044400080213     C                   EXSR      endTRUL47
044500001005     C*
044600040628     C* Solo se effettuate in precedenza cancello le operazioni di sovrapposizione del printer-file
044700040628     C                   IF        RQSCID <> *zeros
044800001005     C                   Z-ADD     80            LENGH            15 5
044900001005     C                   CALL(e)   'QCMDEXC'                                    *LANCIA CMD
045000001006     C                   PARM                    CMD(3)
045100001005     C                   PARM                    LENGH
045200040628     C                   ENDIF
045300011029e   1C                   endif
045400001003     C*
045500001003     C                   ENDSR
045600080213     C*--------------------------------------------------------------------------------------------*
045700080213     C* RILASCIA LA SEMAFORIZZAZIONE X IL TIVGD
045800080213     C*--------------------------------------------------------------------------------------------*
045900080213     C     endTRUL47     BEGSR
046000080213     C*
046100080213     C                   if        wProcedi = 'S'
046200080214     C*
046300080214     C* Sancisco il commit
046400080214     C                   commit
046500080213     C*
046600080213     C* Elimino il blocco elaborazione TIVGD x tipo file: 'W5'
046700080213     C                   clear                   trul47ds
046800080213     C                   eval      d47opz  = 'F'
046900080213     C                   eval      d47tip  = 'W5'
047000080213     C                   call      'TRUL47R'
047100080213     C                   parm                    trul47ds
047200080213     C*
047300080213     C                   endif
047400080213     C*
047500080213     C                   ENDSR
047600001003     C*--------------------------------------------------------------------------------------------*
047700001003     C* NORMALIZZA I CAMPI RICEVUTI IN INPUT
047800001003     C*--------------------------------------------------------------------------------------------*
047900001003     C     norminput     BEGSR
048000011128     c*
048100011128     C* data fine, se non inserita = data corrente
048200011128     C                   Z-ADD     datcor        pardaf
048300011128     c*
048400011128     c* controlla se la data fine è numerica
048500011128     c                   MOVEL     Dfi           dsdate
048600011128     c                   MOVEL     dsanne        dsanna
048700011128     c                   MOVEL     dsmese        dsmesa
048800011128     c                   MOVEL     dsgioe        dsgioa
048900011128     c                   MOVE      dsdata        a1                             *ultimo carattere
049000011128     c                   SETOFF                                       98
049100011128     c                   TESTN                   dsdata               98
049200011128if  1c                   IF        *in98  AND                                   *da TESTN è numero
049300011128     c                             a1 >= '0'                                    *NON trovata lettera
049400011128     c                   MOVEL     dsanna        dsann
049500011128     c                   MOVEL     dsmesa        dsmes
049600011128     c                   MOVEL     dsgioa        dsgio
049700011128     c                   Z-ADD     dsdat         g08inv
049800011128     c                   Z-ADD     *zeros        g08dat
049900011128     C                   MOVEL     '3'           g08err
050000011128     c                   CALL      'XSRDA8'
050100011128     c                   PARM                    wlbda8
050200011128if  2c                   IF        g08err <> '1'                                *no errore
050300011128     c                   Z-ADD     g08inv        pardaf
050400011128e   2c                   ENDIF
050500011128e   1c                   ENDIF
050600011128     c*
050700011128     c* data inizio, se nessuna richiesta = data corrente - 1 anno
050800011128     c                   Z-ADD     datcor        dsdat
050900011128     c                   EVAL      dsann = dsann - 1
051000011128     c                   Z-ADD     dsdat         pardai                         *nessuna richiesta
051100011128     c*
051200011128     c* controlla se la data inizio è numerica
051300011128     c                   MOVEL     Din           dsdate
051400011128     c                   MOVEL     dsanne        dsanna
051500011128     c                   MOVEL     dsmese        dsmesa
051600011128     c                   MOVEL     dsgioe        dsgioa
051700011128     c                   MOVE      dsdata        a1                             *ultimo carattere
051800011128     c                   SETOFF                                       98
051900011128     c                   TESTN                   dsdata               98
052000011128if  1c                   IF        *in98  AND                                   *da TESTN è numero
052100011128     c                             a1 >= '0'                                    *NON trovata lettera
052200011128     c                   MOVEL     dsanna        dsann
052300011128     c                   MOVEL     dsmesa        dsmes
052400011128     c                   MOVEL     dsgioa        dsgio
052500011128     c                   Z-ADD     dsdat         g08inv
052600011128     c                   Z-ADD     *zeros        g08dat
052700011128     C                   MOVEL     '3'           g08err
052800011128     c                   CALL      'XSRDA8'
052900011128     c                   PARM                    wlbda8
053000011128if  2c                   IF        g08err <> '1'                                *no errore
053100011128     c                   Z-ADD     g08inv        pardai
053200011128e   2c                   ENDIF
053300011128e   1c                   ENDIF
053400001003     C*
053500001003     C* pulisce le variabili di lavoro
053600001003     C                   CLEAR                   ndst
053700001003     C                   CLEAR                   nmit
053800001003     C                   CLEAR                   nprd
053900001003     C                   CLEAR                   nlod
054000001003     C*
054100001003     C* destinatario
054200001003     C     minu:maiu     XLATE     Dst           ndst                           *Minus -> Maiuscolo
054300001003     C*
054400001003     C* mittente
054500001003     C     minu:maiu     XLATE     Mit           nmit                           *Minus -> Maiuscolo
054600001003     C*
054700001003     C* provincia di arrivo
054800001003     C     minu:maiu     XLATE     Prd           nprd                           *Minus -> Maiuscolo
054900001003     C*
055000001003     C* località di arrivo
055100001003     C     minu:maiu     XLATE     Lod           nlod                           *Minus -> Maiuscolo
055200001003     C*
055300001003     C* data fine + data inizio, deve esserci congruenza
055400001003if  2C                   if        pardai > pardaf                              *incongruenza
055500001003     C                   z-add     pardaf        pardai                         *data fine = inizio
055600001003e   1C                   endif
055700001003     C*
055800001003     C                   ENDSR
055900001003     C*--------------------------------------------------------------------------------------------*
056000001005     C* ESEGUO CHECK X VERIFICARE SE CONSIDERARE O MENO IL RECORD CORRENTE  SU TIVTA
056100001003     C*--------------------------------------------------------------------------------------------*
056200001003     C     chkvta        BEGSR
056300010926     C*
056400001003     C                   movel     '0'           errl00            1
056500001003     C*
056600011026     C* Controllo se richiesto un particolare P.O. di arrivo (se 999 = Tutti i PO di arrivo)
056700001003     C                   if        errl00 = '0'
056800011026     C                   if        lna > *zeros AND
056900011026     c                             lna <> 999
057000040227     C                   if        S_taslna <> lna
057100001003     C                   movel     '1'           errl00
057200001003     C                   endif
057300001003     C                   endif
057400001003     C                   endif
057500001003     C*
057600001003     C* Controllo se richiesta una particolare provincia di arrivo
057700001003     C                   if        errl00 = '0'
057800001003     C                   if        %trim(nprd) > *blanks
057900040227     C                   if        %trim(S_tasprd) <> %trim(nprd)
058000001003     C                   movel     '1'           errl00
058100001003     C                   endif
058200001003     C                   endif
058300001003     C                   endif
058400001003     C*
058500001003     C* Controllo se richiesta un particolare destinatario
058600001003     C                   if        errl00 = '0'
058700001003     C                   if        %trim(ndst) > *blanks
058800040227     C                   eval      a35 = %trim(S_tasrsd)
058900011213     C                   eval      pos = %scan(%trim(ndst):a35)
059000001005     C                   if        pos <= *zeros
059100001003     C                   movel     '1'           errl00
059200001003     C                   endif
059300001003     C                   endif
059400001003     C                   endif
059500001005     C*
059600001005     C* Controllo se richiesta una particolare località di arrivo
059700001005     C                   if        errl00 = '0'
059800001005     C                   if        %trim(nlod) > *blanks
059900040227     C                   eval      a35 = %trim(S_taslod)
060000011213     C                   eval      pos = %scan(%trim(nlod):a35)
060100001005     C                   if        pos <= *zeros
060200001005     C                   movel     '1'           errl00
060300001005     C                   endif
060400001005     C                   endif
060500001005     C                   endif
060600050526     C*
060700050526     C* Controllo che il tipo bolla sia tra quelli consentiti
060800050526     C                   if        S_tastbl <> 'AR' and
060900050526     C                             S_tastbl <> 'A3' and
061000050526     C                             S_tastbl <> 'F3' and
061100050526     C                             S_tastbl <> 'AP' and
061200050526     C                             S_tastbl <> 'FC'
061300050526     C                   else
061400050526     C                   movel     '1'           errl00
061500050526     C                   endif
061600050526     C*
061700050526     C* Non considera se bolla non è del cliente richiesto
061800050526     C*                  if        errl00 = '0'
061900050526     C*    S_tasksc      lookup    sksc7                                  56
062000050526     C*    S_tasccm      lookup    sksc7                                  57
062100050526     C*                  if        *in56 = *on OR *in57 = *on
062200050526     C*                  else
062300050526     C*                  movel     '1'           errl00
062400050526     C*                  endif
062500050526     C*                  endif
062600041122     C*
062700041122     C* NN considera le bolle legate
062800041122     C                   if        errl00 = '0'
062900041122     C                   eval      klbaas = S_tasAAS
063000041122     C                   eval      klblnp = S_tasLNP
063100041122     C                   eval      klbnrs = S_tasNRS
063200041122     C                   eval      klbnsp = S_tasNSP
063300041122     C     KEYlbl1C      chain     fnlbl01l
063400041122     C                   if        %found(fnlbl01l)
063500041122     C                   movel     '1'           errl00
063600041122     C                   endif
063700041122     C                   endif
063800011026     C*
063900050526     C* Legge e controlla la bolla
064000040227     C                   if        errl00 = '0'
064100040227     C                   eval      dsdat = S_tasaas*10000+S_tasmgs
064200011026     C                   z-add     dsann         ktaaas
064300040227     C                   z-add     S_taslnp      ktalnp
064400040227     C                   z-add     S_tasnrs      ktanrs
064500040227     C                   z-add     S_tasnsp      ktansp
064600040227     C                   movel     S_tastbl      ktatbl
064700011026     C     keytas        chain     titas30c
064800011026     C                   if        %found
064900011026     C                   exsr      chktas
065000011026     C                   endif
065100011026     C                   endif
065200001003     C*
065300001003     C                   ENDSR
065400001005     C*--------------------------------------------------------------------------------------------*
065500001005     C* ESEGUO CHECK X VERIFICARE SE CONSIDERARE O MENO IL RECORD CORRENTE SU TITAS
065600001005     C*--------------------------------------------------------------------------------------------*
065700001005     C     chktas        BEGSR
065800010926     c*
065900010926     c* REimposta le variabili di lavoro
066000011026     C                   movel     '0'           errl00
066100010926     c                   EVAL      $statoOK = 'N'                               *stato NON ok
066200011005     c                   EVAL      $notgia  = 'N'                               *note "giacenza"
066300001005     C*
066400011128     C* Consegnate - NORMALI
066500001005     C                   if        errl00 = '0'
066600011128     C                   if        tasdcm > *zeros AND
066700011128     c                             tascca  = *blanks                            *SENZA anomalie
066800010926     C                   if        ckc <> *blanks
066900010926     c                   EVAL      $statoOK = 'S'                               *stato OK
067000001005     C                   endif
067100010926     C                   endif
067200001005     C                   endif
067300011128     c*
067400011128     C* consegnate - ANOMALE
067500011128     C                   if        errl00 = '0'
067600011128     C                   if        tasdcm > *zeros AND
067700011128     c                             tascca  <> *blanks                           *CON   anomalie
067800011128     C                   if        ckm <> *blanks
067900011128     c                   EVAL      $statoOK = 'S'                               *stato OK
068000011128     C                   endif
068100011128     C                   endif
068200011128     C                   endif
068300021108     c*
068400021108     c* reperisce evenutale GIACENZA
068500021108     c                   EXSR      repgcp
068600011026     c*
068700011005     c* reperisce motivi della NON consegna per GIACENZA
068800011005     c                   Z-ADD     tasaas        ka4aas
068900011005     c                   Z-ADD     taslnp        ka4lnp
069000011005     c                   Z-ADD     tasnrs        ka4nrs
069100011005     c                   Z-ADD     tasnsp        ka4nsp
069200011005     c     ke2ta4        SETLL     tita430c
069300011005     c     ke2ta4        READE     tita430c                               98
069400011005do  1c                   DOW       NOT *in98
069500011005if  2c                   IF        ta4trc = 'G' OR                              *Giacenza
069600011005     c                             ta4trc = 'H'
069700011005     c                   EVAL      $notgia = 'S'                                *stato GIACIENTE
069800011005e   2c                   ENDIF
069900011005     c     ke2ta4        READE     tita430c                               98
070000011005e   1c                   ENDDO
070100001005     C*
070200001005     C* Non consenate - Giacenti
070300001005     C                   if        errl00 = '0'
070400011005if  1C                   IF        tasdcm = *zeros   AND
070500021108     C                             (tasfgc<>*blanks OR $notgia='S' OR $gcp='S')
070600010926     C                   if        ckg <> *blanks
070700010926     c                   EVAL      $statoOK = 'S'                               *stato OK
070800001005     C                   endif
070900010926     C                   endif
071000001005     C                   endif
071100040427     C*
071200040427     C* Non consenate
071300040427     C* - Altre
071400040427     C                   if        errl00 = '0'
071500040427if  1C                   IF        tasdcm = *zeros AND
071600040427     C                             (tasfgc=*blank AND $notgia='N' AND $gcp='N')
071700050210     C*
071800050210     C* reperisce evenutale CONSEGNA PARZIALE
071900050210     C                   EXSR      repparz
072000050210     C                   if        ckp <> *blanks
072100050210     C                   EVAL      $statoOK = 'S'                               *stato OK
072200050210     C                   endif
072300050210     C*
072400050210     C* verifica evenutale APPUNTAMENTO
072500050310     C                   if        tasftc = 'A' OR tastc2 = 'A'
072600050310     C                   if        ckt <> *blanks
072700050210     C                   EVAL      $statoOK = 'S'                               *stato OK
072800050310     C                   endif
072900050210     C                   endif
073000040427     C*
073100040427     C* reperisce evenutale LASCIATO AVVISO
073200040427     C                   EXSR      replav
073300050310     C                   if        $lav='S'
073400050310     C                   if        ckl <> *blanks
073500040427     C                   EVAL      $statoOK = 'S'                               *stato OK
073600040427     C                   endif
073700050310     C                   endif
073800050310     C*
073900050310     C* se scelte ALTRE record cmq sempre da considerare
074000050310     C                   if        cka <> *blanks
074100050310     C                   EVAL      $statoOK = 'S'                               *stato OK
074200050310     C                   endif
074300050210     C*
074400040427     C                   endif
074500040427     C                   endif
074600080606     C*
074700080606     C* Tipo servizio
074800080606     C                   if        errl00 = '0'
074900080606     C                   if        (SERVIZIO_C <> *blanks     AND
075000080606     C                              tasTSP      = SERVIZIO_C)  OR
075100080606     C                             (SERVIZIO_D <> *blanks     AND
075200080606     C                              tasTSP      = SERVIZIO_D)  OR
075300080606     C                             (SERVIZIO_E <> *blanks     AND
075400080606     C                              tasTSP      = SERVIZIO_E)
075500080606     C                   else
075600080606     C                   movel     '1'           errl00
075700080606     C                   endif
075800080606     C                   endif
075900010926     c*
076000010926     C                   if        errl00 = '0'
076100010926     c                   if        $statoOK <> 'S'
076200010926     C                   movel     '1'           errl00
076300010926     C                   endif
076400010926     C                   endif
076500001005     C*
076600001005     C                   ENDSR
076700021108     c*--------------------------------------------------------------------------------------------*
076800021108     c* reperisce la giacenza
076900021108     c*--------------------------------------------------------------------------------------------*
077000021108     c     repgcp        BEGSR
077100021108     c*
077200021108     c* imposta le variabili di lavoro
077300050321     c                   EVAL      $gcp = 'N'                                   NO giacenza in TIGCP
077400021108     c*
077500021108     c* legge la giacenza
077600021108     c                   Z-ADD     tasaas        kcpaas
077700021108     c                   Z-ADD     taslnp        kcplnp
077800021108     c                   Z-ADD     tasnrs        kcpnrs
077900021108     c                   Z-ADD     tasnsp        kcpnsp
078000021108     c                   Z-ADD     *zeros        kcpfrg
078100050321     c     keygcp        CHAIN     tigcp51l                           99
078200021108if  1c                   IF        NOT *in99        AND                         *esistente
078300021108     c                             gcpatb = *blanks AND                         *no annullata
078400021108     c                             gcptcm <> 'Z'
078500021108     c                   MOVEL     'S'           $gcp
078600021108e   1c                   ENDIF
078700021108     c*
078800021108     c                   ENDSR
078900040427     c*--------------------------------------------------------------------------------------------*
079000040427     c* reperisce se ultimo evento è di tipo lasciato avviso
079100040427     c*--------------------------------------------------------------------------------------------*
079200040427     c     replav        BEGSR
079300040427     c*
079400040427     c* imposta le variabili di lavoro
079500040427     c                   EVAL      $lav = 'N'                                   NO evento d tipo L/A
079600040427     c*
079700050210     c* legge gli eventi
079800040427     c                   Z-ADD     tasaas        kevaas
079900040427     c                   Z-ADD     taslnp        kevlnp
080000040427     c                   Z-ADD     tasnrs        kevnrs
080100040427     c                   Z-ADD     tasnsp        kevnsp
080200050210     c     keyevb        SETLL     fnevb04l                               99
080300050210     c                   IF        %equal(fnevb04l)
080400050210     c     keyevb        READE     fnevb04l
080500050210     c                   DOW       not %eof(fnevb04l)
080600040427     c                   IF        evbatb = *blanks                             *no annullata
080700040427     c     evbCEV        LOOKUP    sk2Alav                                20
080800040427     c                   IF        *IN20 = *ON
080900040427     c                   EVAL      $lav = 'S'                                   evento d tipo L/A
081000040427     c                   ELSE
081100040427     c                   EVAL      $lav = 'N'                                   NO evento d tipo L/A
081200040427     c                   ENDIF
081300040427     c                   ENDIF
081400050210     c     keyevb        READE     fnevb04l
081500040427     c                   ENDDO
081600040427     c                   ENDIF
081700040427     c*
081800040427     c                   ENDSR
081900050210     c*--------------------------------------------------------------------------------------------*
082000050210     c* reperisce se la spedizone ha/avuto un evento d consegna parziale
082100050210     c*--------------------------------------------------------------------------------------------*
082200050210     c     repparz       BEGSR
082300050210     c*
082400050210     c* imposta le variabili di lavoro
082500050210     c                   EVAL      $parz = 'N'                                  NO evento Cons.Parz.
082600050210     c*
082700050210     c* legge la giacenza
082800050210     c                   Z-ADD     tasaas        kevaas
082900050210     c                   Z-ADD     taslnp        kevlnp
083000050210     c                   Z-ADD     tasnrs        kevnrs
083100050210     c                   Z-ADD     tasnsp        kevnsp
083200050210     c                   MOVEL(P)  'P'           kevcev
083300050210     c     keyevb2       SETLL     fnevb04l                               99
083400050210     c                   IF        %equal(fnevb04l)
083500050210     c     keyevb2       READE     fnevb04l
083600050210     c                   DOW       not %eof(fnevb04l)
083700050210     c                   IF        evbatb = *blanks                             *no annullata
083800050210     c                   EVAL      $parz= 'S'                                   evento d cons.parz.
083900050210     c                   LEAVE
084000050210     c                   ENDIF
084100050210     c     keyevb2       READE     fnevb04l
084200050210     c                   ENDDO
084300050210     c                   ENDIF
084400050210     c*
084500050210     c                   ENDSR
084600060217     C*--------------------------------------------------------------------------------------------*
084700060217     C* Operazioni per nuova area di arrivo
084800060217     C*--------------------------------------------------------------------------------------------*
084900060217     C     neware        BEGSR
085000060217     C*
085100060217     C* memorizza il nuovo punto operativo di arrivo in un deposito
085200060217     c                   z-add     S_orgcar      depare
085300060217     C*
085400060217     C* reimposta le variabili di lavoro
085500060217     C                   eval      $mpoa = 'N'                                  *NO dati memorizzati
085600060217     c                   clear                   ATOTCON           7 0          *dati memorizzazione
085700060217     c                   clear                   ACONANT           7 0
085800060217     c                   clear                   ACON00            7 0
085900060217     c                   clear                   ACON12            7 0
086000060217     c                   clear                   ACON24            7 0
086100060217     c                   clear                   ACON48            7 0
086200060217     c                   clear                   ACON72            7 0
086300060217     c                   clear                   ACON96            7 0
086400060217     c                   clear                   ACON120           7 0
086500060217     c                   clear                   ATMPMED           5 2
086600060217     c                   clear                   ATOTGICH          7 0
086700060217     c                   clear                   ATOTRESI          7 0
086800060217     c                   clear                   ATOTANO           7 0
086900060217     c                   clear                   ATOTGIA           7 0
087000060217     c                   clear                   ATOTPAR           7 0
087100060217     c                   clear                   ATOTAPP           7 0
087200060217     c                   clear                   ATOTLAV           7 0
087300060217     c                   clear                   ATOTALT           7 0
087400060217     c                   clear                   AORETOT          20 0
087500060217     c*
087600060217     c                   clear                   APERANT           5 2
087700060217     c                   clear                   APER00            5 2
087800060217     c                   clear                   APER12            5 2
087900060217     c                   clear                   APER24            5 2
088000060217     c                   clear                   APER48            5 2
088100060217     c                   clear                   APER72            5 2
088200060217     c                   clear                   APER96            5 2
088300060217     c                   clear                   APER120           5 2
088400060217     C*
088500060217     C                   ENDSR
088600011026     C*--------------------------------------------------------------------------------------------*
088700011026     C* Operazioni per nuovo punto operativo di arrivo
088800011026     C*--------------------------------------------------------------------------------------------*
088900011026     C     newpoa        BEGSR
089000011026     C*
089100011026     C* memorizza il nuovo punto operativo di arrivo in un deposito
089200040227     c                   z-add     S_taslna      deppoa
089300011026     C*
089400011026     C* reimposta le variabili di lavoro
089500011026     C                   eval      $mpoa = 'N'                                  *NO dati memorizzati
089600011029     c                   clear                   TOTCON            7 0          *dati memorizzazione
089700011026     c                   clear                   CONANT            7 0
089800011026     c                   clear                   CON00             7 0
089900011026     c                   clear                   CON12             7 0
090000011026     c                   clear                   CON24             7 0
090100011026     c                   clear                   CON48             7 0
090200011026     c                   clear                   CON72             7 0
090300011026     c                   clear                   CON96             7 0
090400011026     c                   clear                   CON120            7 0
090500011026     c                   clear                   TMPMED            5 2
090600050214     c                   clear                   TOTGICH           7 0
090700050214     c                   clear                   TOTRESI           7 0
090800011026     c                   clear                   TOTANO            7 0
090900011026     c                   clear                   TOTGIA            7 0
091000050210     c                   clear                   TOTPAR            7 0
091100040427     c                   clear                   TOTAPP            7 0
091200040427     c                   clear                   TOTLAV            7 0
091300011026     c                   clear                   TOTALT            7 0
091400011029     c                   clear                   ORETOT           20 0
091500011026     c*
091600011029     c                   clear                   PERANT            5 2
091700011026     c                   clear                   PER00             5 2
091800011026     c                   clear                   PER12             5 2
091900011026     c                   clear                   PER24             5 2
092000011026     c                   clear                   PER48             5 2
092100011026     c                   clear                   PER72             5 2
092200011026     c                   clear                   PER96             5 2
092300011026     c                   clear                   PER120            5 2
092400011026     C*
092500011026     C                   ENDSR
092600001003     C*--------------------------------------------------------------------------------------------*
092700011026     C* Memorizza i dati della bolla
092800001003     C*--------------------------------------------------------------------------------------------*
092900011026     C     membol        BEGSR
093000011029     c*
093100011029     c* imposta il passaggio di memorizzazione dati
093200011029     C                   eval      $mpoa = 'S'                                  *SI per PO arrivo
093300011029     C                   eval      $mgen = 'S'                                  *SI per Generale
093400050214     C                   CLEAR                   $stato
093500071023     C                   MOVEL     *blanks       $AssegnaTip       8
093600071023     C                   MOVEL     *blanks       $AssegnaVal       8
093700071023     C                   MOVEL     *blanks       $TempoBolla       4
093800071023     C                   EVAL      $TempoBolla = %editc(tasnrc:'N')
093900011029     C*---
094000011029     C* Consegnate
094100011029     C*---
094200001027     C                   if        tasdcm > *zeros
094300050214     C* Consegnate ma che hanno avuto giacenza
094400050214     C                   if        (tasfgc<>*blank OR $notgia='S' OR $gcp='S')
094500050214     C                   add       1             TOTGICH                        *PO di arrivo
094600060217     C                   add       1             ATOTGICH                       *AREA di arrivo
094700050214     C                   add       1             GTOTGICH                       *Generale
094800071023     C                   eval      $AssegnaTip = 'CONS.'
094900071023     C                   eval      $AssegnaVal = 'GIAC.CH.'
095000050214     C                   EVAL      $stato = %trim($stato) + 'CG'                *stato CONS.+GIAC.
095100050214     C                   endif
095200011128     c* SENZA anomalia
095300011128     C                   if        tascca = *blanks
095400011029     C                   add       1             TOTCON                         *PO di arrivo
095500060217     C                   add       1             ATOTCON                        *AREA di arrivo
095600011029     C                   add       1             GTOTCON                        *Generale
095700050214     C                   EVAL      $stato = %trim($stato) + 'C'                 *stato CONSEGNATA
095800001027     C                   if        tasnrc < *zeros
095900011029     C                   add       1             CONANT                         *PO di arrivo
096000060217     C                   add       1             ACONANT                        *AREA di arrivo
096100011029     C                   add       1             GCONANT                        *Generale
096200071023     C                   eval      $AssegnaTip = 'CONS.'
096300071023     C                   eval      $AssegnaVal = $TempoBolla
096400011029     C                   eval      ORETOT = oretot + tasnrc
096500060220     C                   eval      AORETOT = aoretot + tasnrc
096600011029     C                   eval      GORETOT = goretot + tasnrc
096700001005     C                   endif
096800001027     C                   if        tasnrc = *zeros
096900011029     C                   add       1             CON00                          *PO di arrivo
097000060217     C                   add       1             ACON00                         *AREA di arrivo
097100011029     C                   add       1             GCON00                         *Generale
097200071023     C                   eval      $AssegnaTip = 'CONS.'
097300071023     C                   eval      $AssegnaVal = $TempoBolla
097400001005     C                   endif
097500001027     C                   if        tasnrc > 0 and tasnrc < 24
097600011029     C                   add       1             CON12                          *PO di arrivo
097700060217     C                   add       1             ACON12                         *AREA di arrivo
097800011029     C                   add       1             GCON12                         *Generale
097900071023     C                   eval      $AssegnaTip = 'CONS.'
098000071023     C                   eval      $AssegnaVal = $TempoBolla
098100011029     C                   eval      ORETOT = oretot + tasnrc
098200060220     C                   eval      AORETOT = aoretot + tasnrc
098300011029     C                   eval      GORETOT = goretot + tasnrc
098400001005     C                   endif
098500001027     C                   if        tasnrc >= 24 and tasnrc < 48
098600011029     C                   add       1             CON24                          *PO di arrivo
098700060217     C                   add       1             ACON24                         *AREA di arrivo
098800011029     C                   add       1             GCON24                         *Generale
098900071023     C                   eval      $AssegnaTip = 'CONS.'
099000071023     C                   eval      $AssegnaVal = $TempoBolla
099100011029     C                   eval      ORETOT = oretot + tasnrc
099200060220     C                   eval      AORETOT = aoretot + tasnrc
099300071023     C                   eval      GORETOT = goretot + tasnrc
099400001005     C                   endif
099500001027     C                   if        tasnrc >= 48 and tasnrc < 72
099600011029     C                   add       1             CON48                          *PO di arrivo
099700060217     C                   add       1             ACON48                         *AREA di arrivo
099800011029     C                   add       1             GCON48                         *Generale
099900071023     C                   eval      $AssegnaTip = 'CONS.'
100000071023     C                   eval      $AssegnaVal = $TempoBolla
100100011029     C                   eval      ORETOT = oretot + tasnrc
100200060220     C                   eval      AORETOT = aoretot + tasnrc
100300011029     C                   eval      GORETOT = goretot + tasnrc
100400001005     C                   endif
100500001027     C                   if        tasnrc >= 72 and tasnrc < 96
100600011029     C                   add       1             CON72                          *PO di arrivo
100700060217     C                   add       1             ACON72                         *AREA di arrivo
100800011029     C                   add       1             GCON72                         *Generale
100900071023     C                   eval      $AssegnaTip = 'CONS.'
101000071023     C                   eval      $AssegnaVal = $TempoBolla
101100011029     C                   eval      ORETOT = oretot + tasnrc
101200060220     C                   eval      AORETOT = aoretot + tasnrc
101300011029     C                   eval      GORETOT = goretot + tasnrc
101400001005     C                   endif
101500001027     C                   if        tasnrc >= 96 and tasnrc < 120
101600011029     C                   add       1             CON96                          *PO di arrivo
101700060217     C                   add       1             ACON96                         *AREA di arrivo
101800011029     C                   add       1             GCON96                         *Generale
101900071023     C                   eval      $AssegnaTip = 'CONS.'
102000071023     C                   eval      $AssegnaVal = $TempoBolla
102100011029     C                   eval      ORETOT = oretot + tasnrc
102200060220     C                   eval      AORETOT = aoretot + tasnrc
102300011029     C                   eval      GORETOT = goretot + tasnrc
102400001005     C                   endif
102500001027     C                   if        tasnrc >= 120
102600011029     C                   add       1             CON120                         *PO di arrivo
102700060217     C                   add       1             ACON120                        *AREA di arrivo
102800011029     C                   add       1             GCON120                        *Generale
102900071023     C                   eval      $AssegnaTip = 'CONS.'
103000071023     C                   eval      $AssegnaVal = $TempoBolla
103100011029     C                   eval      ORETOT = oretot + tasnrc
103200060220     C                   eval      AORETOT = aoretot + tasnrc
103300011029     C                   eval      GORETOT = goretot + tasnrc
103400001005     C                   endif
103500050214     C* CON anomalia
103600001027     C                   else
103700050214     C* CON anomalia -> RESI
103800050214     C                   if        tascca = '2'
103900050214     C                   add       1             TOTRESI                        *PO di arrivo
104000060217     C                   add       1             ATOTRESI                       *AREA di arrivo
104100050214     C                   add       1             GTOTRESI                       *Generale
104200071023     C                   eval      $AssegnaTip = 'CONS.'
104300071023     C                   eval      $AssegnaVal = 'RESO'
104400050214     C                   EVAL      $stato = %trim($stato) + 'R'                 *stato RESO
104500050214     C* CON anomalia -> altre anomalie
104600050214     C                   else
104700011029     C                   add       1             TOTANO                         *PO di arrivo
104800060217     C                   add       1             ATOTANO                        *AREA di arrivo
104900011029     C                   add       1             GTOTANO                        *Generale
105000071023     C                   eval      $AssegnaTip = 'CONS.'
105100071023     C                   eval      $AssegnaVal = 'ANOMALA'
105200050214     C                   EVAL      $stato = %trim($stato) + 'M'                 *stato CONS ANOMALA
105300001027     C                   endif
105400050214     C                   endif
105500040427     C*---
105600040427     C* Non consegnate
105700040427     C*---
105800040427     C                   else
105900040427     C                   if        (tasfgc=*blank AND $notgia='N' AND $gcp='N')
106000050210     C* - Consegna parziale
106100050310     C                   if        $parz = 'S'
106200050310     C                   if        ckp <> *blanks
106300050210     C                   add       1             TOTPAR                         *PO di arrivo
106400060217     C                   add       1             ATOTPAR                        *AREA di arrivo
106500050210     C                   add       1             GTOTPAR                        *Generale
106600071023     C                   eval      $AssegnaTip = 'NON CONS.'
106700071023     C                   eval      $AssegnaVal = 'PARZIALE'
106800050214     C                   EVAL      $stato = %trim($stato) + 'P'                 *stato CONS.PARZ.
106900050310     C                   endif
107000050210     C                   else
107100040427     C* - Appuntamento
107200050310     C                   if        tasftc = 'A' OR tastc2 = 'A'
107300050310     C                   if        ckt <> *blanks
107400040427     C                   add       1             TOTAPP                         *PO di arrivo
107500060217     C                   add       1             ATOTAPP                        *AREA di arrivo
107600040427     C                   add       1             GTOTAPP                        *Generale
107700071023     C                   eval      $AssegnaTip = 'NON CONS.'
107800071023     C                   eval      $AssegnaVal = 'APPUNT.'
107900050214     C                   EVAL      $stato = %trim($stato) + 'T'                 *stato APPUNTAMENTO
108000050310     C                   endif
108100040427     C                   else
108200040427     C* - Lasciato avviso
108300050310     C                   if        $lav = 'S'
108400050310     C                   if        ckl <> *blanks
108500040427     C                   add       1             TOTLAV                         *PO di arrivo
108600060217     C                   add       1             ATOTLAV                        *AREA di arrivo
108700040427     C                   add       1             GTOTLAV                        *Generale
108800071023     C                   eval      $AssegnaTip = 'NON CONS.'
108900071023     C                   eval      $AssegnaVal = 'L.AVVISO'
109000050214     C                   EVAL      $stato = %trim($stato) + 'V'                 *stato LASC./AVV.
109100050310     C                   endif
109200040427     C                   else
109300040427     C* - Altre
109400050211     C                   if        cka <> *blanks
109500040427     C                   add       1             TOTALT                         *PO di arrivo
109600060217     C                   add       1             ATOTALT                        *AREA di arrivo
109700040427     C                   add       1             GTOTALT                        *Generale
109800071023     C                   eval      $AssegnaTip = 'NON CONS.'
109900071023     C                   eval      $AssegnaVal = 'ALTRE'
110000050214     C                   EVAL      $stato = %trim($stato) + 'A'                 *stato ALTRE
110100040427     C                   endif
110200050211     C                   endif
110300040427     C                   endif
110400050210     C                   endif
110500040427     C* Giacenti
110600040427     C                   else
110700050211     C                   if        ckg <> *blanks
110800040427     C                   add       1             TOTGIA                         *PO di arrivo
110900060217     C                   add       1             ATOTGIA                        *AREA di arrivo
111000040427     C                   add       1             GTOTGIA                        *Generale
111100071023     C                   eval      $AssegnaTip = 'NON CONS.'
111200071023     C                   eval      $AssegnaVal = 'GIAC.AP.'
111300050214     C                   EVAL      $stato = %trim($stato) + 'G'                 *stato GIACENTI
111400040427     C                   endif
111500050211     C                   endif
111600040427     C                   endif
111700011029     C*---
111800001005     C* Calcolo il tempo medio di ritardo
111900011029     C*---
112000011029     c* PO di arrivo
112100050310     C                   if        TOTCON = *zeros
112200001116     C                   eval      TMPMED = *zeros
112300001116     C                   else
112400050310     C                   eval      TMPMED = ORETOT / TOTCON
112500001116     C                   endif
112600060217     c* AREA di arrivo
112700060217     C                   if        ATOTCON = *zeros
112800060217     C                   eval      ATMPMED = *zeros
112900060217     C                   else
113000060217     C                   eval      ATMPMED = AORETOT / ATOTCON
113100060217     C                   endif
113200011029     c* Generale
113300050310     C                   if        GTOTCON = *zeros
113400011029     C                   eval      GTMPMED = *zeros
113500011029     C                   else
113600050310     C                   eval      GTMPMED = GORETOT / GTOTCON
113700011029     C                   endif
113800050214     C*
113900050214     C* Stampo il dettaglio bolle considerate con il relativo stato attribuito
114000071023     C***                except    DettBolla
114100071023     C*
114200071023     C* Se richiesta generazione file eseguo pulizia preventiva mirata al cliente
114300071023     C                   if        prmfile = 'S'
114400071023     C                   exsr      WriDett
114500071023     C                   endif
114600050214     C*
114700001003     C                   ENDSR
114800071023     C*--------------------------------------------------------------------------------------------*
114900071023     C* SCRIVE IL FILE DETTAGLIO BOLLE
115000071023     C*--------------------------------------------------------------------------------------------*
115100071023     C     WriDett       BEGSR
115200071023     C*
115300071023     C                   clear                   WFS7W520
115400071023     C*
115500071023     C                   eval      W52KSC     = KSC
115600071023     C                   eval      W52LNA     = tasLNA
115700071023     C*
115800071023     C                   setoff                                           99
115900071023     C                   z-add     1             i
116000071023     C     tasLNA        lookup    sorg(i)                                99
116100071023     C                   if        *in99                                        *trovato
116200071023     C                   movel     sdorg(i)      W52LNADES
116300071023     C                   endif
116400071023     C*
116500071023     C                   eval      W52DSP     = tasAAS*1000+tasMGS
116600071023     C                   eval      W52LNP     = tasLNP
116700071023     C                   eval      W52NRS     = tasNRS
116800071023     C                   eval      W52NSP     = tasNSP
116900071023     C                   eval      W52RMN     = tasRMN
117000071023     C                   eval      W52RSD     = tasRSD
117100071023     C                   eval      W52IND     = tasIND
117200071023     C                   eval      W52CAD     = tasCAD
117300071023     C                   eval      W52LOD     = tasLOD
117400071023     C                   eval      W52PRD     = tasPRD
117500071023     C                   eval      W52DCM     = tasDCM
117600071023     C                   eval      W52TIPO    = $AssegnaTip
117700071023     C                   eval      W52VALORE  = $AssegnaVal
117800071023     C*
117900071023     C                   if        $AssegnaTip = 'CONS.'
118000071023     C                   else
118100071023     C                   z-add     *zeros        W52DCM
118200071023     C                   endif
118300071023     C*
118400071023     C* Reperisco il riferimenti mittente alfanumerico dal relativo file estensioni bolla
118500071023     C                   z-add     tasaas        ka4aas
118600071023     C                   z-add     taslnp        ka4lnp
118700071023     C                   z-add     tasnrs        ka4nrs
118800071023     C                   z-add     tasnsp        ka4nsp
118900071023     C                   movel     'A'           ka4trc
119000071023     C     keyta4        chain     tita430c
119100071023     C                   if        %found(tita430c)
119200071023     C                   eval      DTA4A  = ta4NOT
119300071023     C                   eval      W52RMA = §TA4ARMA
119400071023     C                   endif
119500071023     C*
119600080213     C                   if        prmVGD = 'S'
119700080213     C                   exsr      WRIVGD
119800080213     C                   else
119900071023     C                   write     WFS7W520
120000080213     C                   endif
120100071023     C*
120200071023     C                   ENDSR
120300080213     C*--------------------------------------------------------------------------------------------*
120400080213     C* SCRITTURA FILE DETTAGLI LIVELLI DI SERVIZIO SU TIVGD
120500080213     C*--------------------------------------------------------------------------------------------*
120600080213     C     wriVGD        BEGSR
120700080213     C*
120800080213     C                   clear                   tivgd000
120900080407     C                   eval      vgdDTA = %subst(WFS7W52DS:1:%size(WFS7W52DS))
121000080213     C                   eval      vgdTIP = 'W5'
121100080213     C                   eval      vgdKSU = prmCLIVAS
121200080213     C                   eval      vgdTSC = 'WW'
121300080213     C                   eval      vgdDAT = datcor
121400080213     C                   eval      vgdPGM = 'TIS7W5R'
121500080213     C                   write     tivgd000
121600080213     C*
121700080213     C                   ENDSR
121800001005     C*--------------------------------------------------------------------------------------------*
121900011029     C* EFFETTUA IL CALCOLO DELLE PERCENTUALI per PO di arrivo
122000001005     C*--------------------------------------------------------------------------------------------*
122100011029     C     perpoa        BEGSR
122200001005     C*
122300001005     C* Non "anomale"
122400001116     C                   eval      DIVIS = TOTCON/100
122500001116     C                   if        DIVIS <> *zeros
122600001006     C                   eval      PERANT = CONANT/DIVIS
122700001006     C                   eval      PER00  = CON00 /DIVIS
122800001006     C                   eval      PER12  = CON12 /DIVIS
122900001006     C                   eval      PER24  = CON24 /DIVIS
123000001006     C                   eval      PER48  = CON48 /DIVIS
123100001006     C                   eval      PER72  = CON72 /DIVIS
123200001006     C                   eval      PER96  = CON96 /DIVIS
123300001006     C                   eval      PER120 = CON120/DIVIS
123400001116     C                   endif
123500011026     C*
123600001005     C                   ENDSR
123700060217     C*--------------------------------------------------------------------------------------------*
123800060217     C* EFFETTUA IL CALCOLO DELLE PERCENTUALI per AREA di arrivo
123900060217     C*--------------------------------------------------------------------------------------------*
124000060217     C     perare        BEGSR
124100060217     C*
124200060217     C* Non "anomale"
124300060217     C                   eval      DIVIS = ATOTCON/100
124400060217     C                   if        DIVIS <> *zeros
124500060217     C                   eval      APERANT = ACONANT/DIVIS
124600060217     C                   eval      APER00  = ACON00 /DIVIS
124700060217     C                   eval      APER12  = ACON12 /DIVIS
124800060217     C                   eval      APER24  = ACON24 /DIVIS
124900060217     C                   eval      APER48  = ACON48 /DIVIS
125000060217     C                   eval      APER72  = ACON72 /DIVIS
125100060217     C                   eval      APER96  = ACON96 /DIVIS
125200060217     C                   eval      APER120 = ACON120/DIVIS
125300060217     C                   endif
125400060217     C*
125500060217     C                   ENDSR
125600011029     C*--------------------------------------------------------------------------------------------*
125700011029     C* EFFETTUA IL CALCOLO DELLE PERCENTUALI per Totale generale
125800011029     C*--------------------------------------------------------------------------------------------*
125900011029     C     pergen        BEGSR
126000011029     C*
126100011029     C* Non "anomale"
126200011029     C                   eval      DIVIS = GTOTCON/100
126300011029     C                   if        DIVIS <> *zeros
126400011029     C                   eval      GPERANT = GCONANT/DIVIS
126500011029     C                   eval      GPER00  = GCON00 /DIVIS
126600011029     C                   eval      GPER12  = GCON12 /DIVIS
126700011029     C                   eval      GPER24  = GCON24 /DIVIS
126800011029     C                   eval      GPER48  = GCON48 /DIVIS
126900011029     C                   eval      GPER72  = GCON72 /DIVIS
127000011029     C                   eval      GPER96  = GCON96 /DIVIS
127100011029     C                   eval      GPER120 = GCON120/DIVIS
127200011029     C                   endif
127300011029     C*
127400011029     C                   ENDSR
127500011029     C*--------------------------------------------------------------------------------------------*
127600011029     C* ESEGUE LE OPERAZIONI DI STAMPA della testata
127700011029     C*--------------------------------------------------------------------------------------------*
127800011029     C     states        BEGSR
127900011029     C*
128000011029     C                   write     s7w4te
128100011029     C*
128200011029     C                   ENDSR
128300011029     C*--------------------------------------------------------------------------------------------*
128400011029     C* ESEGUE LE OPERAZIONI DI STAMPA della riga finale
128500011029     C*--------------------------------------------------------------------------------------------*
128600011029     C     stafin        BEGSR
128700011029     C*
128800011029     C                   write     s7w4fi
128900011029     C*
129000011029     C                   ENDSR
129100001005     C*--------------------------------------------------------------------------------------------*
129200011029     C* ESEGUE LE OPERAZIONI DI STAMPA per PO di arrivo
129300001005     C*--------------------------------------------------------------------------------------------*
129400011029     C     stapoa        BEGSR
129500001005     C*
129600011029     c* decodifica il PO di arrivo
129700011029     c                   Z-ADD     deppoa        ppoa                           *codice
129800011029     C                   movel     *blanks       ppoad                          *descrizione
129900011029     c                   SETOFF                                           99
130000011029     c                   Z-ADD     1             i
130100011029     c     deppoa        LOOKUP    sorg(i)                                99
130200011029if  1c                   IF        *in99                                        *trovato
130300011029     C                   MOVEL     sdorg(i)      ppoad
130400011029e   1c                   ENDIF
130500011029     C*
130600011029     C* porta i campi memorizzati in stampa
130700011029     c                   Z-ADD     TOTCON        PTOTCON                        *valori numerici
130800011029     c                   Z-ADD     CONANT        PCONANT
130900011029     c                   Z-ADD     CON00         PCON00
131000011029     c                   Z-ADD     CON12         PCON12
131100011029     c                   Z-ADD     CON24         PCON24
131200011029     c                   Z-ADD     CON48         PCON48
131300011029     c                   Z-ADD     CON72         PCON72
131400011029     c                   Z-ADD     CON96         PCON96
131500011029     c                   Z-ADD     CON120        PCON120
131600011029     c                   Z-ADD     TMPMED        PTMPMED
131700050214     c                   Z-ADD     TOTGICH       PTOTGICH
131800050214     c                   Z-ADD     TOTRESI       PTOTRESI
131900011029     c                   Z-ADD     TOTANO        PTOTANO
132000011029     c                   Z-ADD     TOTGIA        PTOTGIA
132100050210     c                   Z-ADD     TOTPAR        PTOTPAR
132200040427     c                   Z-ADD     TOTAPP        PTOTAPP
132300040427     c                   Z-ADD     TOTLAV        PTOTLAV
132400011029     c                   Z-ADD     TOTALT        PTOTALT
132500011029     c                   Z-ADD     PERANT        PPERANT                        *valori percentuali
132600011029     c                   Z-ADD     PER00         PPER00
132700011029     c                   Z-ADD     PER12         PPER12
132800011029     c                   Z-ADD     PER24         PPER24
132900011029     c                   Z-ADD     PER48         PPER48
133000011029     c                   Z-ADD     PER72         PPER72
133100011029     c                   Z-ADD     PER96         PPER96
133200011029     c                   Z-ADD     PER120        PPER120
133300050610     C*
133400050610     C* ottengo i valori calcolati
133500060217     C                   eval      W5TOTCONTT = TOTCON+TOTGICH+TOTRESI+
133600060217     C                                          TOTANO+TOTGIA+TOTPAR+TOTAPP+
133700050616     C                                          TOTLAV+TOTALT
133800050616     C                   eval      W5PERCONSL = TOTCON * 100 / W5TOTCONTT
133900050616     C                   eval      W5TOTDADEF = W5TOTCONTT - TOTCON
134000050616     C                   eval      W5TOTCONL  = TOTCON+TOTGICH+TOTRESI+TOTANO
134100050616     C                   eval      W5PERCONL  = W5TOTCONL * 100 / W5TOTCONTT
134200050616     C                   eval      W5TOTNOCON = TOTGIA+TOTPAR+TOTAPP+
134300050616     C                                          TOTLAV+TOTALT
134400050616     C                   eval      W5PERNOCON = W5TOTNOCON * 100 / W5TOTCONTT
134500050610     C*
134600050616     C* scrittura file
134700050616     C                   exsr      wriwfxls
134800050610     C*
134900011029     C* stampa
135000011029     C                   write     s7w4de
135100001005     C*
135200001005     C                   ENDSR
135300060217     C*--------------------------------------------------------------------------------------------*
135400060217     C* ESEGUE LE OPERAZIONI DI STAMPA per AREA di arrivo
135500060217     C*--------------------------------------------------------------------------------------------*
135600060217     C     staare        BEGSR
135700060217     C*
135800060217     c* decodifica l'AREA di arrivo
135900060217     c                   Z-ADD     depare        ppoa                           *codice
136000060217     C                   movel     *blanks       ppoad                          *descrizione
136100060217     c                   SETOFF                                           99
136200060217     c                   Z-ADD     1             i
136300060217     c     depare        LOOKUP    sare(i)                                99
136400060217if  1c                   IF        *in99                                        *trovato
136500060217     C                   MOVEL     sdare(i)      ppoad
136600060217e   1c                   ENDIF
136700060217     C                   EVAL      ppoad = 'TOT ' + %trim(ppoad)
136800060217     C*
136900060217     C* porta i campi memorizzati in stampa
137000060217     c                   Z-ADD     ATOTCON       PTOTCON                        *valori numerici
137100060217     c                   Z-ADD     ACONANT       PCONANT
137200060217     c                   Z-ADD     ACON00        PCON00
137300060217     c                   Z-ADD     ACON12        PCON12
137400060217     c                   Z-ADD     ACON24        PCON24
137500060217     c                   Z-ADD     ACON48        PCON48
137600060217     c                   Z-ADD     ACON72        PCON72
137700060217     c                   Z-ADD     ACON96        PCON96
137800060217     c                   Z-ADD     ACON120       PCON120
137900060217     c                   Z-ADD     ATMPMED       PTMPMED
138000060217     c                   Z-ADD     ATOTGICH      PTOTGICH
138100060217     c                   Z-ADD     ATOTRESI      PTOTRESI
138200060217     c                   Z-ADD     ATOTANO       PTOTANO
138300060217     c                   Z-ADD     ATOTGIA       PTOTGIA
138400060217     c                   Z-ADD     ATOTPAR       PTOTPAR
138500060217     c                   Z-ADD     ATOTAPP       PTOTAPP
138600060217     c                   Z-ADD     ATOTLAV       PTOTLAV
138700060217     c                   Z-ADD     ATOTALT       PTOTALT
138800060217     c                   Z-ADD     APERANT       PPERANT                        *valori percentuali
138900060217     c                   Z-ADD     APER00        PPER00
139000060217     c                   Z-ADD     APER12        PPER12
139100060217     c                   Z-ADD     APER24        PPER24
139200060217     c                   Z-ADD     APER48        PPER48
139300060217     c                   Z-ADD     APER72        PPER72
139400060217     c                   Z-ADD     APER96        PPER96
139500060217     c                   Z-ADD     APER120       PPER120
139600060217     C*
139700060217     C* ottengo i valori calcolati
139800060217     C                   eval      W5TOTCONTT = ATOTCON+ATOTGICH+ATOTRESI+
139900060217     C                                          ATOTANO+ATOTGIA+ATOTPAR+ATOTAPP+
140000060217     C                                          ATOTLAV+ATOTALT
140100060217     C                   eval      W5PERCONSL = ATOTCON * 100 / W5TOTCONTT
140200060217     C                   eval      W5TOTDADEF = W5TOTCONTT - ATOTCON
140300060217     C                   eval      W5TOTCONL  = ATOTCON+ATOTGICH+ATOTRESI+
140400060217     C                                          ATOTANO
140500060217     C                   eval      W5PERCONL  = W5TOTCONL * 100 / W5TOTCONTT
140600060217     C                   eval      W5TOTNOCON = ATOTGIA+ATOTPAR+ATOTAPP+
140700060217     C                                          ATOTLAV+ATOTALT
140800060217     C                   eval      W5PERNOCON = W5TOTNOCON * 100 / W5TOTCONTT
140900060217     C*
141000060217     C* scrittura file
141100060217     C                   exsr      wriwfxls
141200060217     C* stampa
141300060217     C                   write     s7w4de
141400060217     C*
141500060217     C* stampa riga separazine
141600060217     C                   write     s7w4ri
141700060217     C*
141800060217     C                   ENDSR
141900011029     C*--------------------------------------------------------------------------------------------*
142000011029     C* ESEGUE LE OPERAZIONI DI STAMPA per Totale generale
142100011029     C*--------------------------------------------------------------------------------------------*
142200011029     C     stagen        BEGSR
142300011029     C*
142400011029     c* decodifica il Totale generale
142500011029     c                   Z-ADD     *zeros        ppoa                           *codice
142600011029     C                   EVAL      ppoad = 'TOTALE GENERALE'                    *descrizione
142700011029     C*
142800011029     C* porta i campi memorizzati in stampa
142900011029     c                   Z-ADD     GTOTCON       PTOTCON                        *valori numerici
143000011029     c                   Z-ADD     GCONANT       PCONANT
143100011029     c                   Z-ADD     GCON00        PCON00
143200011029     c                   Z-ADD     GCON12        PCON12
143300011029     c                   Z-ADD     GCON24        PCON24
143400011029     c                   Z-ADD     GCON48        PCON48
143500011029     c                   Z-ADD     GCON72        PCON72
143600011029     c                   Z-ADD     GCON96        PCON96
143700011029     c                   Z-ADD     GCON120       PCON120
143800011029     c                   Z-ADD     GTMPMED       PTMPMED
143900050214     c                   Z-ADD     GTOTGICH      PTOTGICH
144000050214     c                   Z-ADD     GTOTRESI      PTOTRESI
144100011029     c                   Z-ADD     GTOTANO       PTOTANO
144200011029     c                   Z-ADD     GTOTGIA       PTOTGIA
144300050210     c                   Z-ADD     GTOTPAR       PTOTPAR
144400040427     c                   Z-ADD     GTOTAPP       PTOTAPP
144500040427     c                   Z-ADD     GTOTLAV       PTOTLAV
144600011029     c                   Z-ADD     GTOTALT       PTOTALT
144700011029     c                   Z-ADD     GPERANT       PPERANT                        *valori percentuali
144800011029     c                   Z-ADD     GPER00        PPER00
144900011029     c                   Z-ADD     GPER12        PPER12
145000011029     c                   Z-ADD     GPER24        PPER24
145100011029     c                   Z-ADD     GPER48        PPER48
145200011029     c                   Z-ADD     GPER72        PPER72
145300011029     c                   Z-ADD     GPER96        PPER96
145400011029     c                   Z-ADD     GPER120       PPER120
145500050610     C*
145600050610     C* ottengo i valori calcolati
145700050616     C                   eval      W5TOTCONTT = GTOTCON+GTOTGICH+GTOTRESI+
145800050610     C                                          GTOTANO+GTOTGIA+GTOTPAR+
145900050610     C                                          GTOTAPP+GTOTLAV+GTOTALT
146000050616     C                   eval      W5PERCONSL = GTOTCON * 100 / W5TOTCONTT
146100050616     C                   eval      W5TOTDADEF = W5TOTCONTT - GTOTCON
146200050616     C                   eval      W5TOTCONL  = GTOTCON+GTOTGICH+GTOTRESI+
146300050610     C                                          GTOTANO
146400050616     C                   eval      W5PERCONL  = W5TOTCONL * 100 / W5TOTCONTT
146500050616     C                   eval      W5TOTNOCON = GTOTGIA+GTOTPAR+GTOTAPP+
146600050610     C                                          GTOTLAV+GTOTALT
146700050616     C                   eval      W5PERNOCON = W5TOTNOCON * 100 / W5TOTCONTT
146800050610     C*
146900050616     C* scrittura file
147000050616     C                   exsr      wriwfxls
147100060217     C*
147200060217     C* stampa riga separazine
147300060217     C                   write     s7w4ri
147400050610     C*
147500011029     C* stampa
147600011029     C                   write     s7w4de
147700060217     C*
147800060217     C* stampa riga separazine
147900060217     C                   write     s7w4ri
148000011029     C*
148100011029     C                   ENDSR
148200050616     C*--------------------------------------------------------------------------------------------*
148300050616     C* SCRIVE IL WRK-FILE X SCARICO DATI SU FILE XLS
148400050616     C*--------------------------------------------------------------------------------------------*
148500050616     C     wriwfxls      BEGSR
148600050616     C*
148700060217     C* Scrivo solo se richiesto nel lancio
148800060217     C                   if        prmfile = 'S'
148900050616     C                   eval      W5KSC      = KSC
149000050616     C                   eval      W5LNA      = PPOA
149100050616     C                   eval      W5LNADES   = PPOAD
149200050616     C                   eval      W5TOTCON   = PTOTCON
149300050616     C                   eval      W5CONANT   = PCONANT
149400050616     C                   eval      W5CON00    = PCON00
149500050616     C                   eval      W5CON12    = PCON12
149600050616     C                   eval      W5CON24    = PCON24
149700050616     C                   eval      W5CON48    = PCON48
149800050616     C                   eval      W5CON72    = PCON72
149900050616     C                   eval      W5CON96    = PCON96
150000050616     C                   eval      W5CON120   = PCON120
150100050616     C                   eval      W5TMPMED   = PTMPMED
150200050616     C                   eval      W5TOTGICH  = PTOTGICH
150300050616     C                   eval      W5TOTRESI  = PTOTRESI
150400050616     C                   eval      W5TOTANO   = PTOTANO
150500050616     C                   eval      W5TOTGIA   = PTOTGIA
150600050616     C                   eval      W5TOTPAR   = PTOTPAR
150700050616     C                   eval      W5TOTAPP   = PTOTAPP
150800050616     C                   eval      W5TOTLAV   = PTOTLAV
150900050616     C                   eval      W5TOTALT   = PTOTALT
151000050616     C                   eval      W5PERANT   = PPERANT
151100050616     C                   eval      W5PER00    = PPER00
151200050616     C                   eval      W5PER12    = PPER12
151300050616     C                   eval      W5PER24    = PPER24
151400050616     C                   eval      W5PER48    = PPER48
151500050616     C                   eval      W5PER72    = PPER72
151600050616     C                   eval      W5PER96    = PPER96
151700050616     C                   eval      W5PER120   = PPER120
151800050617     C*
151900050617     C* Scarico il buffer del file
152000050617     C                   write     WFS7W510
152100060217     C                   endif
152200050616     C*
152300050616     C                   ENDSR
152400011029     C*--------------------------------------------------------------------------------------------*
152500011029     C* CARICA LE TABELLE OCCORRENTI
152600011029     C*--------------------------------------------------------------------------------------------*
152700011029     C     cartab        BEGSR
152800011029     c*---
152900011029     c* Organigramma
153000011029     c*---
153100011029     c                   CLEAR                   i                              *indice
153200011029     C                   CLEAR                   sorg
153300011029     C                   CLEAR                   sdorg
153400011029     c     *loval        SETLL     azorg01l
153500011029     c                   READ      azorg01l                               99
153600011029do  1c                   DOW       NOT *in99
153700011029if  2c                   IF        orgfva = *blanks AND
153800011029     c                             orgfag <> 'V'
153900011029     c                   ADD       1             i
154000011029     c                   Z-ADD     orgfil        sorg(i)
154100011029     c                   MOVEL     orgdes        sdorg(i)
154200011029e   2c                   ENDIF
154300011029     c                   READ      azorg01l                               99
154400011029e   1c                   ENDDO
154500060217     c*---
154600060217     c* Aree
154700060217     c*---
154800060217     c                   CLEAR                   i                              *indice
154900060217     C                   CLEAR                   sare
155000060217     C                   CLEAR                   sdare
155100060217     C                   Z-ADD     1             tute
155200060217     C                   MOVEL     '05'          tcod
155300060217     c     ke2tab        SETLL     tabel00f
155400060217     c     ke2tab        READE     tabel00f                               99
155500060217do  1c                   DOW       NOT *in99
155600060217if  2c                   IF        tblflg = *blanks
155700060217     c                   ADD       1             i
155800060217     c                   MOVEL     tblkey        sare(i)
155900060217     C                   MOVEL     tbluni        DS05
156000060217     c                   MOVEL     §05des        sdare(i)
156100060217e   2c                   ENDIF
156200060217     c     ke2tab        READE     tabel00f                               99
156300060217e   1c                   ENDDO
156400011029     c*---
156500011029     c* Provincie
156600011029     c*---
156700011029     c                   CLEAR                   i                              *indice
156800011029     C                   CLEAR                   sprv
156900011029     C                   CLEAR                   sdprv
157000011029     C                   Z-ADD     1             tute
157100011029     C                   MOVEL     'PR'          tcod
157200011029     c     ke2tab        SETLL     tabel00f
157300011029     c     ke2tab        READE     tabel00f                               99
157400011029do  1c                   DOW       NOT *in99
157500011029if  2c                   IF        tblflg = *blanks
157600011029     c                   ADD       1             i
157700011029     c                   MOVE      tblkey        sprv(i)
157800011029     C                   MOVEL     tbluni        DSPR
157900011029     c                   MOVEL     §prdes        sdprv(i)
158000011029e   2c                   ENDIF
158100011029     c     ke2tab        READE     tabel00f                               99
158200011029e   1c                   ENDDO
158300040427     c*---
158400040427     c* Eventi d tipo "lasciato avviso"
158500040427     c*---
158600040427     C                   clear                   sk2Alav
158700040427     C                   z-add     *zeros        j                 3 0
158800040427     C                   eval      tute = 1
158900040427     C                   eval      tcod = '2A'
159000040427     C     ke2tab        setll     tabel00f
159100040427     C     ke2tab        reade     tabel00f
159200040427     C                   dow       not %eof(tabel00f)
159300040427     C                   movel(p)  tblUNI        ds2a
159400040427     C                   if        §2aFTC = 'A'
159500040427     C                   add       1             j
159600040427     C                   eval      sk2Alav(j) = tblKEY
159700040427     C                   endif
159800040427     C     ke2tab        reade     tabel00f
159900040427     C                   enddo
160000011029     C*
160100011029     c                   ENDSR
160200001005     C*--------------------------------------------------------------------------------------------*
160300001006     C* IMPOSTA I CAMPI DI TESTATA IN STAMPA E VALORIZZO CAMPONE "USRDFNDTA" (per 100 byte)
160400001005     C*--------------------------------------------------------------------------------------------*
160500001207     C     imppar        BEGSR
160600001207     C*
160700001207     C* Pulisce il formato record di testata del PRTF
160800011026     C                   clear                   s7w4te
160900001005     C*
161000001005     C* Inverto le date in input
161100001005     C                   z-add     *zeros        G08DAT
161200001005     C                   z-add     PARDAI        G08INV
161300001005     C                   movel     '3'           G08ERR
161400001005     C                   call      'XSRDA8'
161500001005     C                   parm                    WLBDA8
161600001005     C                   z-add     G08DAT        STPDAI
161700001006     C                   eval      desdai = %trim(%editc(PARDAI:'Z'))
161800001005     C*
161900001005     C                   z-add     *zeros        G08DAT
162000001005     C                   z-add     PARDAF        G08INV
162100001005     C                   movel     '3'           G08ERR
162200001005     C                   call      'XSRDA8'
162300001005     C                   parm                    WLBDA8
162400001005     C                   z-add     G08DAT        STPDAF
162500001006     C                   eval      desdaf = %trim(%editc(PARDAF:'Z'))
162600001005     C*
162700001005     C* Reperisco la descrizione del Punto Operativo e valorizzo i due parametri ad esso riferiti
162800011026     c                   if        lna = 999
162900011026     c                   eval      STPLNAD = 'Tutti'
163000011026     c                   else
163100001030     C     lna           chain     azorg01l
163200001005     C                   if        %found(azorg01l)
163300001005     C                   movel     orgdes        STPLNAD
163400001006     C                   eval      despoa = %trim(%editc(lna:'Z'))
163500001005     C                   endif
163600011026     c                   endif
163700001005     C                   z-add     lna           STPLNA
163800011029     C*
163900011029     C* Reperisco la descrizione della provincia e valorizzo entrambi i parametri ad essa riferiti
164000011029     C                   movel     prd           STPPRD
164100011029     C                   movel     'PR'          tcod
164200011029     C                   movel(p)  prd           tkey
164300011029     C     keytab        chain     tabel00f
164400011029     C                   if        %found(tabel00f)
164500011029     C                   movel     tbluni        DSPR
164600011029     C                   movel     §PRDES        STPPRDD
164700011029     C                   eval      desprd = %trim(nprd)
164800011029     C                   endif
164900080606     C*
165000080606     C* Descrizione filtro x tipo servizio
165100080606     c                   if        SERVIZIO_C <> *blanks AND
165200080606     c                             SERVIZIO_D <> *blanks AND
165300080606     c                             SERVIZIO_E <> *blanks
165400080606     c                   eval      STPTSP = 'Tutti'
165500080606     c                   else
165600080606     c                   if        SERVIZIO_C <> *blanks
165700080606     c                   eval      STPTSP = %trim(STPTSP)+' '+'Corriere'
165800080606     c                   endif
165900080606     c                   if        SERVIZIO_D <> *blanks
166000080606     c                   eval      STPTSP = %trim(STPTSP)+' '+'Distribuzione'
166100080606     c                   endif
166200080606     c                   if        SERVIZIO_E <> *blanks
166300080606     c                   eval      STPTSP = %trim(STPTSP)+' '+'Espresso'
166400080606     c                   endif
166500080606     c                   endif
166600001005     C*
166700001005     C* Quindi tutti i rimanenti parametri
166800001005     C                   movel     ksc           STPKSU
166900001005     C                   movel     ndst          STPDST
167000001006     C                   eval      desdst = %trim(ndst)
167100001005     C                   movel     nmit          STPMIT
167200001006     C                   eval      desmit = %trim(nmit)
167300001005     C                   movel     lod           STPLOD
167400001006     C                   eval      deslod = %trim(nlod)
167500001005     C                   if        ckc <> *blanks
167600001207     C                   movel     'S'           STPFCO
167700001006     C                   eval      desfl1 = 'CONS.'
167800001005     C                   endif
167900011128     C                   if        ckm <> *blanks
168000011128     C                   movel     'S'           STPFCM
168100011128     C                   eval      desfl1 = 'ANOM.'
168200011128     C                   endif
168300050210     C                   if        ckg <> *blanks
168400050210     C                   eval      STPFLG = %trim(STPFLG) + ' GI'
168500050210     C                   eval      desfl2 = 'GIAC.'
168600050210     C                   endif
168700050210     C                   if        ckp <> *blanks
168800050210     C                   eval      STPFLG = %trim(STPFLG) + ' CP'
168900050210     C                   eval      desfl3 = 'PARZ.'
169000050210     C                   endif
169100050210     C                   if        ckt <> *blanks
169200050210     C                   eval      STPFLG = %trim(STPFLG) + ' AP'
169300050210     C                   eval      desfl4 = 'APP.'
169400050210     C                   endif
169500050210     C                   if        ckl <> *blanks
169600050210     C                   eval      STPFLG = %trim(STPFLG) + ' LA'
169700050210     C                   eval      desfl5 = 'L.AVV'
169800050210     C                   endif
169900050210     C                   if        cka <> *blanks
170000050210     C                   eval      STPFLG = %trim(STPFLG) + ' AL'
170100050210     C                   eval      desfl6 = 'ALTRE'
170200050210     C                   endif
170300001006     C*
170400001006     C* A questo punto compongo la stringa da passare nell'USRDFNDTA e i dati di servizio
170500001006     C                   move      rqscid        sgiusrnbr
170600001006     C                   eval      tipstp = 'REC'
170700001006     C                   eval      tipsnd = 'SGI'
170800001006     C                   eval      email = *blanks
170900001006     C*
171000001009     C                   eval      varcmd=''''+dessta+' '+%trim(prmdat)+' '+
171100001009     C                                        %trim(prmora)
171200001006     C                   if        desdai>*blanks
171300001006     C                   eval      varcmd = %trim(varcmd)+%trim(desdai)+'+'
171400001006     C                   endif
171500001006     C                   if        desdaf>*blanks
171600001006     C                   eval      varcmd = %trim(varcmd)+%trim(desdaf)+'+'
171700001006     C                   endif
171800001006     C                   if        despoa>*blanks
171900001006     C                   eval      varcmd = %trim(varcmd)+%trim(despoa)+'+'
172000001006     C                   endif
172100001006     C                   if        desprd>*blanks
172200001006     C                   eval      varcmd = %trim(varcmd)+%trim(desprd)+'+'
172300001006     C                   endif
172400001006     C                   if        deslod>*blanks
172500001006     C                   eval      varcmd = %trim(varcmd)+%trim(deslod)+'+'
172600001006     C                   endif
172700001006     C                   if        desfl1>*blanks
172800001006     C                   eval      varcmd = %trim(varcmd)+%trim(desfl1)+'+'
172900001006     C                   endif
173000001006     C                   if        desfl2>*blanks
173100001006     C                   eval      varcmd = %trim(varcmd)+%trim(desfl2)+'+'
173200001006     C                   endif
173300001006     C                   if        desfl3>*blanks
173400001006     C                   eval      varcmd = %trim(varcmd)+%trim(desfl3)+'+'
173500001006     C                   endif
173600050210     C                   if        desfl4>*blanks
173700050210     C                   eval      varcmd = %trim(varcmd)+%trim(desfl4)+'+'
173800050210     C                   endif
173900050210     C                   if        desfl5>*blanks
174000050210     C                   eval      varcmd = %trim(varcmd)+%trim(desfl5)+'+'
174100050210     C                   endif
174200050210     C                   if        desfl6>*blanks
174300050210     C                   eval      varcmd = %trim(varcmd)+%trim(desfl6)+'+'
174400050210     C                   endif
174500001006     C                   if        desmit>*blanks
174600001006     C                   eval      varcmd = %trim(varcmd)+%trim(desmit)+'+'
174700001006     C                   endif
174800001006     C                   if        desdst>*blanks
174900001006     C                   eval      varcmd = %trim(varcmd)+%trim(desdst)
175000001006     C                   endif
175100001009     C                   if        %len(%trim(varcmd)) >=97
175200001009     C                   eval      %subst(var100:97:4) = '...'
175300001009     C                   else
175400001009     C                   eval      varcmd = %trim(varcmd)
175500001009     C                   endif
175600001009     C                   movel     varcmd        var100
175700050210     C                   move(p)   param         varsrv
175800001006     C                   eval      varcmd = %trim(CMD(1))+' '+%trim(CMD(2))+
175900001006     C                                      var100 + varsrv+''''+')'
176000001005     C*
176100001005     C                   ENDSR
176200050614     C*--------------------------------------------------------------------------------------------*
176300050614     C* operazioni iniziali
176400050614     C*--------------------------------------------------------------------------------------------*
176500050614     C     opeini        BEGSR
176600050614     C*
176700050614     C* Aggiunge le librerie occorrenti in lista
176800050614     C                   EVAL      wpgm = 'GAITRAOBJ/TIS777C'
176900050614     C                   CALL      wpgm
177000050614     C*--------------------
177100050614     C* CALCOLA LA DATA CORRENTE
177200050614     C*--------------------
177300050614     C                   time                    WN14             14 0          *ORA (6)+ DATA (8)
177400050614     C                   move      WN14          WN8               8 0          *DATA (8) IN G/M/AA
177500050614     C                   z-add     WN8           G08DAT
177600050614     C                   z-add     *zeros        G08INV
177700050614     C                   movel     '0'           G08ERR
177800050614     C                   call      'XSRDA8'
177900050614     C                   parm                    WLBDA8
178000050614     C                   z-add     G08INV        DATCOR            8 0          *DATA CORRENTE AA/M/
178100050614     c*
178200050614     c* IMPOSTAZIONE SCHIERA DEI CLIENTI DA ELABORARE
178300050614     c* Se richiesta elaborazione per cliente unificante richiamo tibs10r
178400050614     c                   clear                   sksc7
178500050614     c                   if        prmsuf = 'U'
178600050614     c                   clear                   tibs10ds
178700050614     c                   eval      d10tle = prmtlg
178800050614     c                   eval      d10paf = 'F'
178900050614     c                   move      ksc           d10cod
179000050614     c                   z-add     datcor        d10drf
179100050614     c                   call      'TIBS10R'
179200050614     c                   Parm                    Tibs10ds
179300050614     c                   if        d10err = *blanks
179400050614     c                   z-add     sksc11        sksc7
179500050614     c                   else
179600050614     c                   move      ksc           sksc7(1)
179700050614     c                   endif
179800050614     c                   else
179900050614     c* altrimenti imposto primo elemnto schiera = codice cliente ricevuto
180000050614     c                   move      ksc           sksc7(1)
180100050614     c                   endif
180200050614     C*
180300050614     C* Scrivo i clienti così estratti nel wrk-file d procedura
180400050614     C                   if        not %open(wfs7w50f)
180500050614     C                   open      wfs7w50f
180600050614     C                   endif
180700050614     C                   z-add     1             wIndice           3 0
180800050614     C                   dow       wIndice <= %elem(sksc7)
180900050614     C                   if        sksc7(wIndice) <> *zeros
181000050614     C                   eval      s7w5ksc = sksc7(wIndice)
181100050614     C                   write     wfs7w500
181200050614     C                   else
181300050614     C                   leave
181400050614     C                   endif
181500050614     C                   eval      wIndice = wIndice + 1
181600050614     C                   enddo
181700050614     C                   if        %open(wfs7w50f)
181800050614     C                   close     wfs7w50f
181900050614     C                   endif
182000050614     C*
182100050614     C* Se richiesta generazione file eseguo pulizia preventiva mirata al cliente
182200050614     C                   if        prmfile = 'S'
182300071023     C* File WFS7W51F
182400050614     C     KSC           setll     wfs7w51f
182500050614     C                   if        %found(wfs7w51f)
182600050614     C     KSC           reade     wfs7w51f
182700050614     C                   dow       not %eof(wfs7w51f)
182800050614     C                   delete    wfs7w51f
182900050614     C     KSC           reade     wfs7w51f
183000050614     C                   enddo
183100050614     C                   endif
183200071023     C* File WFS7W52F
183300071023     C     KSC           setll     wfs7w52f
183400071023     C                   if        %found(wfs7w52f)
183500071023     C     KSC           reade     wfs7w52f
183600071023     C                   dow       not %eof(wfs7w52f)
183700071023     C                   delete    wfs7w52f
183800071023     C     KSC           reade     wfs7w52f
183900071023     C                   enddo
184000071023     C                   endif
184100050614     C                   endif
184200050614     C*
184300050614     C                   ENDSR
184400080214     C*--------------------------------------------------------------------------------------------*
184500080214     C* Gestione errori imprevisti
184600080214     C*--------------------------------------------------------------------------------------------*
184700080214     C     *pssr         BEGSR
184800080214     C*
184900080214     C                   rolbk
185000080214     C*
185100080214     C                   seton                                        LR
185200080214     C*
185300080214     C                   ENDSR     '*CANCL'
185400001003     C*--------------------------------------------------------------------------------------------*
185500001003     C* impostazioni iniziali
185600011029     C*--------------------------------------------------------------------------------------------*
185700001003     C     *inzsr        BEGSR
185800001003     C*
185900001003     C* Ricevimento parametri
186000001003     C     *ENTRY        PLIST
186100040628     C                   PARM                    KPJBA
186200050210     C                   MOVEL     KPJBU         PARAMETRI
186300040628     C*
186400040628     C                   EVAL      TIS1W2IDS = PRMTIS1W2IDS
186500001003     C*
186600001003     C* Chiavi di lettura
186700001003     C     keytas        KLIST                                                  *TITAS30C
186800001003     C                   KFLD                    ktaaas
186900001003     C                   KFLD                    ktalnp
187000001003     C                   KFLD                    ktanrs
187100001003     C                   KFLD                    ktansp
187200001003     C                   KFLD                    ktatbl
187300011005     c     ke2ta4        KLIST                                                  tita430c
187400011005     c                   KFLD                    ka4aas                         -anno spedizione
187500011005     c                   KFLD                    ka4lnp                         -linea partenza
187600011005     c                   KFLD                    ka4nrs                         -serie
187700011005     c                   KFLD                    ka4nsp                         -spedizione
187800071023     c     keyta4        KLIST                                                  tita430c
187900071023     c                   KFLD                    ka4aas                         -anno spedizione
188000071023     c                   KFLD                    ka4lnp                         -linea partenza
188100071023     c                   KFLD                    ka4nrs                         -serie
188200071023     c                   KFLD                    ka4nsp                         -spedizione
188300071023     c                   KFLD                    ka4trc                         -tipo record
188400050321     c     keygcp        KLIST                                                  *tigcp51l
188500021108     c                   KFLD                    kcpaas                         -anno spedizione
188600021108     c                   KFLD                    kcplnp                         -linea partenza
188700021108     c                   KFLD                    kcpnrs                         -serie
188800021108     c                   KFLD                    kcpnsp                         -spedizione
188900021108     c                   KFLD                    kcpfrg                         -prg aperture
189000050210     c     keyevb        KLIST                                                  *fnevb04l
189100040427     c                   KFLD                    kevaas                         -anno spedizione
189200040427     c                   KFLD                    kevlnp                         -linea partenza
189300040427     c                   KFLD                    kevnrs                         -serie
189400040427     c                   KFLD                    kevnsp                         -spedizione
189500050210     c     keyevb2       KLIST                                                  *fnevb04l
189600050210     c                   KFLD                    kevaas                         -anno spedizione
189700050210     c                   KFLD                    kevlnp                         -linea partenza
189800050210     c                   KFLD                    kevnrs                         -serie
189900050210     c                   KFLD                    kevnsp                         -spedizione
190000050210     c                   KFLD                    kevcev                         -codice evento
190100041122     C     keylbl1c      KLIST                                                  *fnlbl01l
190200041122     C                   KFLD                    klbaas
190300041122     C                   KFLD                    klblnp
190400041122     C                   KFLD                    klbnrs
190500041122     C                   KFLD                    klbnsp
190600001005     C*
190700001005     C* Definizione chiavi - TABEL00F
190800001005     C     keytab        KLIST
190900001005     C                   KFLD                    tute
191000001005     C                   KFLD                    tcod
191100001005     C                   KFLD                    tkey
191200011029     C     ke2tab        KLIST
191300011029     C                   KFLD                    tute
191400011029     C                   KFLD                    tcod
191500001003     C*
191600001003     C                   ENDSR
191700011029     O*--------------------------------------------------------------------------------------------*
191800050214     OPRTF198   E            DettBolla        01
191900050214     O                                        +   1 'Chiave bolla:'
192000050214     O                       dskeybol         +   1
192100050214     O                                        +  10 'Stato bolla:'
192200050214     O                       $stato           +   1
192300001005** CMD - COMANDI CL
192400050524OVRPRTF OVRSCOPE(*CALLLVL) TOFILE(TIS7W4P)
192500001006OUTQ(STRATEOBJ/SGIIN) USRDFNDTA(
192600040427DLTOVR FILE(WURTH) LVL(*)
192700001006
