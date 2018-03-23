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
015200040427     D sk2Alav         S              3    DIM(100)                             *eventi L/A
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
074600010926     c*
074700010926     C                   if        errl00 = '0'
074800010926     c                   if        $statoOK <> 'S'
074900010926     C                   movel     '1'           errl00
075000010926     C                   endif
075100010926     C                   endif
075200001005     C*
075300001005     C                   ENDSR
075400021108     c*--------------------------------------------------------------------------------------------*
075500021108     c* reperisce la giacenza
075600021108     c*--------------------------------------------------------------------------------------------*
075700021108     c     repgcp        BEGSR
075800021108     c*
075900021108     c* imposta le variabili di lavoro
076000050321     c                   EVAL      $gcp = 'N'                                   NO giacenza in TIGCP
076100021108     c*
076200021108     c* legge la giacenza
076300021108     c                   Z-ADD     tasaas        kcpaas
076400021108     c                   Z-ADD     taslnp        kcplnp
076500021108     c                   Z-ADD     tasnrs        kcpnrs
076600021108     c                   Z-ADD     tasnsp        kcpnsp
076700021108     c                   Z-ADD     *zeros        kcpfrg
076800050321     c     keygcp        CHAIN     tigcp51l                           99
076900021108if  1c                   IF        NOT *in99        AND                         *esistente
077000021108     c                             gcpatb = *blanks AND                         *no annullata
077100021108     c                             gcptcm <> 'Z'
077200021108     c                   MOVEL     'S'           $gcp
077300021108e   1c                   ENDIF
077400021108     c*
077500021108     c                   ENDSR
077600040427     c*--------------------------------------------------------------------------------------------*
077700040427     c* reperisce se ultimo evento è di tipo lasciato avviso
077800040427     c*--------------------------------------------------------------------------------------------*
077900040427     c     replav        BEGSR
078000040427     c*
078100040427     c* imposta le variabili di lavoro
078200040427     c                   EVAL      $lav = 'N'                                   NO evento d tipo L/A
078300040427     c*
078400050210     c* legge gli eventi
078500040427     c                   Z-ADD     tasaas        kevaas
078600040427     c                   Z-ADD     taslnp        kevlnp
078700040427     c                   Z-ADD     tasnrs        kevnrs
078800040427     c                   Z-ADD     tasnsp        kevnsp
078900050210     c     keyevb        SETLL     fnevb04l                               99
079000050210     c                   IF        %equal(fnevb04l)
079100050210     c     keyevb        READE     fnevb04l
079200050210     c                   DOW       not %eof(fnevb04l)
079300040427     c                   IF        evbatb = *blanks                             *no annullata
079400040427     c     evbCEV        LOOKUP    sk2Alav                                20
079500040427     c                   IF        *IN20 = *ON
079600040427     c                   EVAL      $lav = 'S'                                   evento d tipo L/A
079700040427     c                   ELSE
079800040427     c                   EVAL      $lav = 'N'                                   NO evento d tipo L/A
079900040427     c                   ENDIF
080000040427     c                   ENDIF
080100050210     c     keyevb        READE     fnevb04l
080200040427     c                   ENDDO
080300040427     c                   ENDIF
080400040427     c*
080500040427     c                   ENDSR
080600050210     c*--------------------------------------------------------------------------------------------*
080700050210     c* reperisce se la spedizone ha/avuto un evento d consegna parziale
080800050210     c*--------------------------------------------------------------------------------------------*
080900050210     c     repparz       BEGSR
081000050210     c*
081100050210     c* imposta le variabili di lavoro
081200050210     c                   EVAL      $parz = 'N'                                  NO evento Cons.Parz.
081300050210     c*
081400050210     c* legge la giacenza
081500050210     c                   Z-ADD     tasaas        kevaas
081600050210     c                   Z-ADD     taslnp        kevlnp
081700050210     c                   Z-ADD     tasnrs        kevnrs
081800050210     c                   Z-ADD     tasnsp        kevnsp
081900050210     c                   MOVEL(P)  'P'           kevcev
082000050210     c     keyevb2       SETLL     fnevb04l                               99
082100050210     c                   IF        %equal(fnevb04l)
082200050210     c     keyevb2       READE     fnevb04l
082300050210     c                   DOW       not %eof(fnevb04l)
082400050210     c                   IF        evbatb = *blanks                             *no annullata
082500050210     c                   EVAL      $parz= 'S'                                   evento d cons.parz.
082600050210     c                   LEAVE
082700050210     c                   ENDIF
082800050210     c     keyevb2       READE     fnevb04l
082900050210     c                   ENDDO
083000050210     c                   ENDIF
083100050210     c*
083200050210     c                   ENDSR
083300060217     C*--------------------------------------------------------------------------------------------*
083400060217     C* Operazioni per nuova area di arrivo
083500060217     C*--------------------------------------------------------------------------------------------*
083600060217     C     neware        BEGSR
083700060217     C*
083800060217     C* memorizza il nuovo punto operativo di arrivo in un deposito
083900060217     c                   z-add     S_orgcar      depare
084000060217     C*
084100060217     C* reimposta le variabili di lavoro
084200060217     C                   eval      $mpoa = 'N'                                  *NO dati memorizzati
084300060217     c                   clear                   ATOTCON           7 0          *dati memorizzazione
084400060217     c                   clear                   ACONANT           7 0
084500060217     c                   clear                   ACON00            7 0
084600060217     c                   clear                   ACON12            7 0
084700060217     c                   clear                   ACON24            7 0
084800060217     c                   clear                   ACON48            7 0
084900060217     c                   clear                   ACON72            7 0
085000060217     c                   clear                   ACON96            7 0
085100060217     c                   clear                   ACON120           7 0
085200060217     c                   clear                   ATMPMED           5 2
085300060217     c                   clear                   ATOTGICH          7 0
085400060217     c                   clear                   ATOTRESI          7 0
085500060217     c                   clear                   ATOTANO           7 0
085600060217     c                   clear                   ATOTGIA           7 0
085700060217     c                   clear                   ATOTPAR           7 0
085800060217     c                   clear                   ATOTAPP           7 0
085900060217     c                   clear                   ATOTLAV           7 0
086000060217     c                   clear                   ATOTALT           7 0
086100060217     c                   clear                   AORETOT          20 0
086200060217     c*
086300060217     c                   clear                   APERANT           5 2
086400060217     c                   clear                   APER00            5 2
086500060217     c                   clear                   APER12            5 2
086600060217     c                   clear                   APER24            5 2
086700060217     c                   clear                   APER48            5 2
086800060217     c                   clear                   APER72            5 2
086900060217     c                   clear                   APER96            5 2
087000060217     c                   clear                   APER120           5 2
087100060217     C*
087200060217     C                   ENDSR
087300011026     C*--------------------------------------------------------------------------------------------*
087400011026     C* Operazioni per nuovo punto operativo di arrivo
087500011026     C*--------------------------------------------------------------------------------------------*
087600011026     C     newpoa        BEGSR
087700011026     C*
087800011026     C* memorizza il nuovo punto operativo di arrivo in un deposito
087900040227     c                   z-add     S_taslna      deppoa
088000011026     C*
088100011026     C* reimposta le variabili di lavoro
088200011026     C                   eval      $mpoa = 'N'                                  *NO dati memorizzati
088300011029     c                   clear                   TOTCON            7 0          *dati memorizzazione
088400011026     c                   clear                   CONANT            7 0
088500011026     c                   clear                   CON00             7 0
088600011026     c                   clear                   CON12             7 0
088700011026     c                   clear                   CON24             7 0
088800011026     c                   clear                   CON48             7 0
088900011026     c                   clear                   CON72             7 0
089000011026     c                   clear                   CON96             7 0
089100011026     c                   clear                   CON120            7 0
089200011026     c                   clear                   TMPMED            5 2
089300050214     c                   clear                   TOTGICH           7 0
089400050214     c                   clear                   TOTRESI           7 0
089500011026     c                   clear                   TOTANO            7 0
089600011026     c                   clear                   TOTGIA            7 0
089700050210     c                   clear                   TOTPAR            7 0
089800040427     c                   clear                   TOTAPP            7 0
089900040427     c                   clear                   TOTLAV            7 0
090000011026     c                   clear                   TOTALT            7 0
090100011029     c                   clear                   ORETOT           20 0
090200011026     c*
090300011029     c                   clear                   PERANT            5 2
090400011026     c                   clear                   PER00             5 2
090500011026     c                   clear                   PER12             5 2
090600011026     c                   clear                   PER24             5 2
090700011026     c                   clear                   PER48             5 2
090800011026     c                   clear                   PER72             5 2
090900011026     c                   clear                   PER96             5 2
091000011026     c                   clear                   PER120            5 2
091100011026     C*
091200011026     C                   ENDSR
091300001003     C*--------------------------------------------------------------------------------------------*
091400011026     C* Memorizza i dati della bolla
091500001003     C*--------------------------------------------------------------------------------------------*
091600011026     C     membol        BEGSR
091700011029     c*
091800011029     c* imposta il passaggio di memorizzazione dati
091900011029     C                   eval      $mpoa = 'S'                                  *SI per PO arrivo
092000011029     C                   eval      $mgen = 'S'                                  *SI per Generale
092100050214     C                   CLEAR                   $stato
092200071023     C                   MOVEL     *blanks       $AssegnaTip       8
092300071023     C                   MOVEL     *blanks       $AssegnaVal       8
092400071023     C                   MOVEL     *blanks       $TempoBolla       4
092500071023     C                   EVAL      $TempoBolla = %editc(tasnrc:'N')
092600011029     C*---
092700011029     C* Consegnate
092800011029     C*---
092900001027     C                   if        tasdcm > *zeros
093000050214     C* Consegnate ma che hanno avuto giacenza
093100050214     C                   if        (tasfgc<>*blank OR $notgia='S' OR $gcp='S')
093200050214     C                   add       1             TOTGICH                        *PO di arrivo
093300060217     C                   add       1             ATOTGICH                       *AREA di arrivo
093400050214     C                   add       1             GTOTGICH                       *Generale
093500071023     C                   eval      $AssegnaTip = 'CONS.'
093600071023     C                   eval      $AssegnaVal = 'GIAC.CH.'
093700050214     C                   EVAL      $stato = %trim($stato) + 'CG'                *stato CONS.+GIAC.
093800050214     C                   endif
093900011128     c* SENZA anomalia
094000011128     C                   if        tascca = *blanks
094100011029     C                   add       1             TOTCON                         *PO di arrivo
094200060217     C                   add       1             ATOTCON                        *AREA di arrivo
094300011029     C                   add       1             GTOTCON                        *Generale
094400050214     C                   EVAL      $stato = %trim($stato) + 'C'                 *stato CONSEGNATA
094500001027     C                   if        tasnrc < *zeros
094600011029     C                   add       1             CONANT                         *PO di arrivo
094700060217     C                   add       1             ACONANT                        *AREA di arrivo
094800011029     C                   add       1             GCONANT                        *Generale
094900071023     C                   eval      $AssegnaTip = 'CONS.'
095000071023     C                   eval      $AssegnaVal = $TempoBolla
095100011029     C                   eval      ORETOT = oretot + tasnrc
095200060220     C                   eval      AORETOT = aoretot + tasnrc
095300011029     C                   eval      GORETOT = goretot + tasnrc
095400001005     C                   endif
095500001027     C                   if        tasnrc = *zeros
095600011029     C                   add       1             CON00                          *PO di arrivo
095700060217     C                   add       1             ACON00                         *AREA di arrivo
095800011029     C                   add       1             GCON00                         *Generale
095900071023     C                   eval      $AssegnaTip = 'CONS.'
096000071023     C                   eval      $AssegnaVal = $TempoBolla
096100001005     C                   endif
096200001027     C                   if        tasnrc > 0 and tasnrc < 24
096300011029     C                   add       1             CON12                          *PO di arrivo
096400060217     C                   add       1             ACON12                         *AREA di arrivo
096500011029     C                   add       1             GCON12                         *Generale
096600071023     C                   eval      $AssegnaTip = 'CONS.'
096700071023     C                   eval      $AssegnaVal = $TempoBolla
096800011029     C                   eval      ORETOT = oretot + tasnrc
096900060220     C                   eval      AORETOT = aoretot + tasnrc
097000011029     C                   eval      GORETOT = goretot + tasnrc
097100001005     C                   endif
097200001027     C                   if        tasnrc >= 24 and tasnrc < 48
097300011029     C                   add       1             CON24                          *PO di arrivo
097400060217     C                   add       1             ACON24                         *AREA di arrivo
097500011029     C                   add       1             GCON24                         *Generale
097600071023     C                   eval      $AssegnaTip = 'CONS.'
097700071023     C                   eval      $AssegnaVal = $TempoBolla
097800011029     C                   eval      ORETOT = oretot + tasnrc
097900060220     C                   eval      AORETOT = aoretot + tasnrc
098000071023     C                   eval      GORETOT = goretot + tasnrc
098100001005     C                   endif
098200001027     C                   if        tasnrc >= 48 and tasnrc < 72
098300011029     C                   add       1             CON48                          *PO di arrivo
098400060217     C                   add       1             ACON48                         *AREA di arrivo
098500011029     C                   add       1             GCON48                         *Generale
098600071023     C                   eval      $AssegnaTip = 'CONS.'
098700071023     C                   eval      $AssegnaVal = $TempoBolla
098800011029     C                   eval      ORETOT = oretot + tasnrc
098900060220     C                   eval      AORETOT = aoretot + tasnrc
099000011029     C                   eval      GORETOT = goretot + tasnrc
099100001005     C                   endif
099200001027     C                   if        tasnrc >= 72 and tasnrc < 96
099300011029     C                   add       1             CON72                          *PO di arrivo
099400060217     C                   add       1             ACON72                         *AREA di arrivo
099500011029     C                   add       1             GCON72                         *Generale
099600071023     C                   eval      $AssegnaTip = 'CONS.'
099700071023     C                   eval      $AssegnaVal = $TempoBolla
099800011029     C                   eval      ORETOT = oretot + tasnrc
099900060220     C                   eval      AORETOT = aoretot + tasnrc
100000011029     C                   eval      GORETOT = goretot + tasnrc
100100001005     C                   endif
100200001027     C                   if        tasnrc >= 96 and tasnrc < 120
100300011029     C                   add       1             CON96                          *PO di arrivo
100400060217     C                   add       1             ACON96                         *AREA di arrivo
100500011029     C                   add       1             GCON96                         *Generale
100600071023     C                   eval      $AssegnaTip = 'CONS.'
100700071023     C                   eval      $AssegnaVal = $TempoBolla
100800011029     C                   eval      ORETOT = oretot + tasnrc
100900060220     C                   eval      AORETOT = aoretot + tasnrc
101000011029     C                   eval      GORETOT = goretot + tasnrc
101100001005     C                   endif
101200001027     C                   if        tasnrc >= 120
101300011029     C                   add       1             CON120                         *PO di arrivo
101400060217     C                   add       1             ACON120                        *AREA di arrivo
101500011029     C                   add       1             GCON120                        *Generale
101600071023     C                   eval      $AssegnaTip = 'CONS.'
101700071023     C                   eval      $AssegnaVal = $TempoBolla
101800011029     C                   eval      ORETOT = oretot + tasnrc
101900060220     C                   eval      AORETOT = aoretot + tasnrc
102000011029     C                   eval      GORETOT = goretot + tasnrc
102100001005     C                   endif
102200050214     C* CON anomalia
102300001027     C                   else
102400050214     C* CON anomalia -> RESI
102500050214     C                   if        tascca = '2'
102600050214     C                   add       1             TOTRESI                        *PO di arrivo
102700060217     C                   add       1             ATOTRESI                       *AREA di arrivo
102800050214     C                   add       1             GTOTRESI                       *Generale
102900071023     C                   eval      $AssegnaTip = 'CONS.'
103000071023     C                   eval      $AssegnaVal = 'RESO'
103100050214     C                   EVAL      $stato = %trim($stato) + 'R'                 *stato RESO
103200050214     C* CON anomalia -> altre anomalie
103300050214     C                   else
103400011029     C                   add       1             TOTANO                         *PO di arrivo
103500060217     C                   add       1             ATOTANO                        *AREA di arrivo
103600011029     C                   add       1             GTOTANO                        *Generale
103700071023     C                   eval      $AssegnaTip = 'CONS.'
103800071023     C                   eval      $AssegnaVal = 'ANOMALA'
103900050214     C                   EVAL      $stato = %trim($stato) + 'M'                 *stato CONS ANOMALA
104000001027     C                   endif
104100050214     C                   endif
104200040427     C*---
104300040427     C* Non consegnate
104400040427     C*---
104500040427     C                   else
104600040427     C                   if        (tasfgc=*blank AND $notgia='N' AND $gcp='N')
104700050210     C* - Consegna parziale
104800050310     C                   if        $parz = 'S'
104900050310     C                   if        ckp <> *blanks
105000050210     C                   add       1             TOTPAR                         *PO di arrivo
105100060217     C                   add       1             ATOTPAR                        *AREA di arrivo
105200050210     C                   add       1             GTOTPAR                        *Generale
105300071023     C                   eval      $AssegnaTip = 'NON CONS.'
105400071023     C                   eval      $AssegnaVal = 'PARZIALE'
105500050214     C                   EVAL      $stato = %trim($stato) + 'P'                 *stato CONS.PARZ.
105600050310     C                   endif
105700050210     C                   else
105800040427     C* - Appuntamento
105900050310     C                   if        tasftc = 'A' OR tastc2 = 'A'
106000050310     C                   if        ckt <> *blanks
106100040427     C                   add       1             TOTAPP                         *PO di arrivo
106200060217     C                   add       1             ATOTAPP                        *AREA di arrivo
106300040427     C                   add       1             GTOTAPP                        *Generale
106400071023     C                   eval      $AssegnaTip = 'NON CONS.'
106500071023     C                   eval      $AssegnaVal = 'APPUNT.'
106600050214     C                   EVAL      $stato = %trim($stato) + 'T'                 *stato APPUNTAMENTO
106700050310     C                   endif
106800040427     C                   else
106900040427     C* - Lasciato avviso
107000050310     C                   if        $lav = 'S'
107100050310     C                   if        ckl <> *blanks
107200040427     C                   add       1             TOTLAV                         *PO di arrivo
107300060217     C                   add       1             ATOTLAV                        *AREA di arrivo
107400040427     C                   add       1             GTOTLAV                        *Generale
107500071023     C                   eval      $AssegnaTip = 'NON CONS.'
107600071023     C                   eval      $AssegnaVal = 'L.AVVISO'
107700050214     C                   EVAL      $stato = %trim($stato) + 'V'                 *stato LASC./AVV.
107800050310     C                   endif
107900040427     C                   else
108000040427     C* - Altre
108100050211     C                   if        cka <> *blanks
108200040427     C                   add       1             TOTALT                         *PO di arrivo
108300060217     C                   add       1             ATOTALT                        *AREA di arrivo
108400040427     C                   add       1             GTOTALT                        *Generale
108500071023     C                   eval      $AssegnaTip = 'NON CONS.'
108600071023     C                   eval      $AssegnaVal = 'ALTRE'
108700050214     C                   EVAL      $stato = %trim($stato) + 'A'                 *stato ALTRE
108800040427     C                   endif
108900050211     C                   endif
109000040427     C                   endif
109100050210     C                   endif
109200040427     C* Giacenti
109300040427     C                   else
109400050211     C                   if        ckg <> *blanks
109500040427     C                   add       1             TOTGIA                         *PO di arrivo
109600060217     C                   add       1             ATOTGIA                        *AREA di arrivo
109700040427     C                   add       1             GTOTGIA                        *Generale
109800071023     C                   eval      $AssegnaTip = 'NON CONS.'
109900071023     C                   eval      $AssegnaVal = 'GIAC.AP.'
110000050214     C                   EVAL      $stato = %trim($stato) + 'G'                 *stato GIACENTI
110100040427     C                   endif
110200050211     C                   endif
110300040427     C                   endif
110400011029     C*---
110500001005     C* Calcolo il tempo medio di ritardo
110600011029     C*---
110700011029     c* PO di arrivo
110800050310     C                   if        TOTCON = *zeros
110900001116     C                   eval      TMPMED = *zeros
111000001116     C                   else
111100050310     C                   eval      TMPMED = ORETOT / TOTCON
111200001116     C                   endif
111300060217     c* AREA di arrivo
111400060217     C                   if        ATOTCON = *zeros
111500060217     C                   eval      ATMPMED = *zeros
111600060217     C                   else
111700060217     C                   eval      ATMPMED = AORETOT / ATOTCON
111800060217     C                   endif
111900011029     c* Generale
112000050310     C                   if        GTOTCON = *zeros
112100011029     C                   eval      GTMPMED = *zeros
112200011029     C                   else
112300050310     C                   eval      GTMPMED = GORETOT / GTOTCON
112400011029     C                   endif
112500050214     C*
112600050214     C* Stampo il dettaglio bolle considerate con il relativo stato attribuito
112700071023     C***                except    DettBolla
112800071023     C*
112900071023     C* Se richiesta generazione file eseguo pulizia preventiva mirata al cliente
113000071023     C                   if        prmfile = 'S'
113100071023     C                   exsr      WriDett
113200071023     C                   endif
113300050214     C*
113400001003     C                   ENDSR
113500071023     C*--------------------------------------------------------------------------------------------*
113600071023     C* SCRIVE IL FILE DETTAGLIO BOLLE
113700071023     C*--------------------------------------------------------------------------------------------*
113800071023     C     WriDett       BEGSR
113900071023     C*
114000071023     C                   clear                   WFS7W520
114100071023     C*
114200071023     C                   eval      W52KSC     = KSC
114300071023     C                   eval      W52LNA     = tasLNA
114400071023     C*
114500071023     C                   setoff                                           99
114600071023     C                   z-add     1             i
114700071023     C     tasLNA        lookup    sorg(i)                                99
114800071023     C                   if        *in99                                        *trovato
114900071023     C                   movel     sdorg(i)      W52LNADES
115000071023     C                   endif
115100071023     C*
115200071023     C                   eval      W52DSP     = tasAAS*1000+tasMGS
115300071023     C                   eval      W52LNP     = tasLNP
115400071023     C                   eval      W52NRS     = tasNRS
115500071023     C                   eval      W52NSP     = tasNSP
115600071023     C                   eval      W52RMN     = tasRMN
115700071023     C                   eval      W52RSD     = tasRSD
115800071023     C                   eval      W52IND     = tasIND
115900071023     C                   eval      W52CAD     = tasCAD
116000071023     C                   eval      W52LOD     = tasLOD
116100071023     C                   eval      W52PRD     = tasPRD
116200071023     C                   eval      W52DCM     = tasDCM
116300071023     C                   eval      W52TIPO    = $AssegnaTip
116400071023     C                   eval      W52VALORE  = $AssegnaVal
116500071023     C*
116600071023     C                   if        $AssegnaTip = 'CONS.'
116700071023     C                   else
116800071023     C                   z-add     *zeros        W52DCM
116900071023     C                   endif
117000071023     C*
117100071023     C* Reperisco il riferimenti mittente alfanumerico dal relativo file estensioni bolla
117200071023     C                   z-add     tasaas        ka4aas
117300071023     C                   z-add     taslnp        ka4lnp
117400071023     C                   z-add     tasnrs        ka4nrs
117500071023     C                   z-add     tasnsp        ka4nsp
117600071023     C                   movel     'A'           ka4trc
117700071023     C     keyta4        chain     tita430c
117800071023     C                   if        %found(tita430c)
117900071023     C                   eval      DTA4A  = ta4NOT
118000071023     C                   eval      W52RMA = §TA4ARMA
118100071023     C                   endif
118200071023     C*
118300080213     C                   if        prmVGD = 'S'
118400080213     C                   exsr      WRIVGD
118500080213     C                   else
118600071023     C                   write     WFS7W520
118700080213     C                   endif
118800071023     C*
118900071023     C                   ENDSR
119000080213     C*--------------------------------------------------------------------------------------------*
119100080213     C* SCRITTURA FILE DETTAGLI LIVELLI DI SERVIZIO SU TIVGD
119200080213     C*--------------------------------------------------------------------------------------------*
119300080213     C     wriVGD        BEGSR
119400080213     C*
119500080213     C                   clear                   tivgd000
119600080407     C                   eval      vgdDTA = %subst(WFS7W52DS:1:%size(WFS7W52DS))
119700080213     C                   eval      vgdTIP = 'W5'
119800080213     C                   eval      vgdKSU = prmCLIVAS
119900080213     C                   eval      vgdTSC = 'WW'
120000080213     C                   eval      vgdDAT = datcor
120100080213     C                   eval      vgdPGM = 'TIS7W5R'
120200080213     C                   write     tivgd000
120300080213     C*
120400080213     C                   ENDSR
120500001005     C*--------------------------------------------------------------------------------------------*
120600011029     C* EFFETTUA IL CALCOLO DELLE PERCENTUALI per PO di arrivo
120700001005     C*--------------------------------------------------------------------------------------------*
120800011029     C     perpoa        BEGSR
120900001005     C*
121000001005     C* Non "anomale"
121100001116     C                   eval      DIVIS = TOTCON/100
121200001116     C                   if        DIVIS <> *zeros
121300001006     C                   eval      PERANT = CONANT/DIVIS
121400001006     C                   eval      PER00  = CON00 /DIVIS
121500001006     C                   eval      PER12  = CON12 /DIVIS
121600001006     C                   eval      PER24  = CON24 /DIVIS
121700001006     C                   eval      PER48  = CON48 /DIVIS
121800001006     C                   eval      PER72  = CON72 /DIVIS
121900001006     C                   eval      PER96  = CON96 /DIVIS
122000001006     C                   eval      PER120 = CON120/DIVIS
122100001116     C                   endif
122200011026     C*
122300001005     C                   ENDSR
122400060217     C*--------------------------------------------------------------------------------------------*
122500060217     C* EFFETTUA IL CALCOLO DELLE PERCENTUALI per AREA di arrivo
122600060217     C*--------------------------------------------------------------------------------------------*
122700060217     C     perare        BEGSR
122800060217     C*
122900060217     C* Non "anomale"
123000060217     C                   eval      DIVIS = ATOTCON/100
123100060217     C                   if        DIVIS <> *zeros
123200060217     C                   eval      APERANT = ACONANT/DIVIS
123300060217     C                   eval      APER00  = ACON00 /DIVIS
123400060217     C                   eval      APER12  = ACON12 /DIVIS
123500060217     C                   eval      APER24  = ACON24 /DIVIS
123600060217     C                   eval      APER48  = ACON48 /DIVIS
123700060217     C                   eval      APER72  = ACON72 /DIVIS
123800060217     C                   eval      APER96  = ACON96 /DIVIS
123900060217     C                   eval      APER120 = ACON120/DIVIS
124000060217     C                   endif
124100060217     C*
124200060217     C                   ENDSR
124300011029     C*--------------------------------------------------------------------------------------------*
124400011029     C* EFFETTUA IL CALCOLO DELLE PERCENTUALI per Totale generale
124500011029     C*--------------------------------------------------------------------------------------------*
124600011029     C     pergen        BEGSR
124700011029     C*
124800011029     C* Non "anomale"
124900011029     C                   eval      DIVIS = GTOTCON/100
125000011029     C                   if        DIVIS <> *zeros
125100011029     C                   eval      GPERANT = GCONANT/DIVIS
125200011029     C                   eval      GPER00  = GCON00 /DIVIS
125300011029     C                   eval      GPER12  = GCON12 /DIVIS
125400011029     C                   eval      GPER24  = GCON24 /DIVIS
125500011029     C                   eval      GPER48  = GCON48 /DIVIS
125600011029     C                   eval      GPER72  = GCON72 /DIVIS
125700011029     C                   eval      GPER96  = GCON96 /DIVIS
125800011029     C                   eval      GPER120 = GCON120/DIVIS
125900011029     C                   endif
126000011029     C*
126100011029     C                   ENDSR
126200011029     C*--------------------------------------------------------------------------------------------*
126300011029     C* ESEGUE LE OPERAZIONI DI STAMPA della testata
126400011029     C*--------------------------------------------------------------------------------------------*
126500011029     C     states        BEGSR
126600011029     C*
126700011029     C                   write     s7w4te
126800011029     C*
126900011029     C                   ENDSR
127000011029     C*--------------------------------------------------------------------------------------------*
127100011029     C* ESEGUE LE OPERAZIONI DI STAMPA della riga finale
127200011029     C*--------------------------------------------------------------------------------------------*
127300011029     C     stafin        BEGSR
127400011029     C*
127500011029     C                   write     s7w4fi
127600011029     C*
127700011029     C                   ENDSR
127800001005     C*--------------------------------------------------------------------------------------------*
127900011029     C* ESEGUE LE OPERAZIONI DI STAMPA per PO di arrivo
128000001005     C*--------------------------------------------------------------------------------------------*
128100011029     C     stapoa        BEGSR
128200001005     C*
128300011029     c* decodifica il PO di arrivo
128400011029     c                   Z-ADD     deppoa        ppoa                           *codice
128500011029     C                   movel     *blanks       ppoad                          *descrizione
128600011029     c                   SETOFF                                           99
128700011029     c                   Z-ADD     1             i
128800011029     c     deppoa        LOOKUP    sorg(i)                                99
128900011029if  1c                   IF        *in99                                        *trovato
129000011029     C                   MOVEL     sdorg(i)      ppoad
129100011029e   1c                   ENDIF
129200011029     C*
129300011029     C* porta i campi memorizzati in stampa
129400011029     c                   Z-ADD     TOTCON        PTOTCON                        *valori numerici
129500011029     c                   Z-ADD     CONANT        PCONANT
129600011029     c                   Z-ADD     CON00         PCON00
129700011029     c                   Z-ADD     CON12         PCON12
129800011029     c                   Z-ADD     CON24         PCON24
129900011029     c                   Z-ADD     CON48         PCON48
130000011029     c                   Z-ADD     CON72         PCON72
130100011029     c                   Z-ADD     CON96         PCON96
130200011029     c                   Z-ADD     CON120        PCON120
130300011029     c                   Z-ADD     TMPMED        PTMPMED
130400050214     c                   Z-ADD     TOTGICH       PTOTGICH
130500050214     c                   Z-ADD     TOTRESI       PTOTRESI
130600011029     c                   Z-ADD     TOTANO        PTOTANO
130700011029     c                   Z-ADD     TOTGIA        PTOTGIA
130800050210     c                   Z-ADD     TOTPAR        PTOTPAR
130900040427     c                   Z-ADD     TOTAPP        PTOTAPP
131000040427     c                   Z-ADD     TOTLAV        PTOTLAV
131100011029     c                   Z-ADD     TOTALT        PTOTALT
131200011029     c                   Z-ADD     PERANT        PPERANT                        *valori percentuali
131300011029     c                   Z-ADD     PER00         PPER00
131400011029     c                   Z-ADD     PER12         PPER12
131500011029     c                   Z-ADD     PER24         PPER24
131600011029     c                   Z-ADD     PER48         PPER48
131700011029     c                   Z-ADD     PER72         PPER72
131800011029     c                   Z-ADD     PER96         PPER96
131900011029     c                   Z-ADD     PER120        PPER120
132000050610     C*
132100050610     C* ottengo i valori calcolati
132200060217     C                   eval      W5TOTCONTT = TOTCON+TOTGICH+TOTRESI+
132300060217     C                                          TOTANO+TOTGIA+TOTPAR+TOTAPP+
132400050616     C                                          TOTLAV+TOTALT
132500050616     C                   eval      W5PERCONSL = TOTCON * 100 / W5TOTCONTT
132600050616     C                   eval      W5TOTDADEF = W5TOTCONTT - TOTCON
132700050616     C                   eval      W5TOTCONL  = TOTCON+TOTGICH+TOTRESI+TOTANO
132800050616     C                   eval      W5PERCONL  = W5TOTCONL * 100 / W5TOTCONTT
132900050616     C                   eval      W5TOTNOCON = TOTGIA+TOTPAR+TOTAPP+
133000050616     C                                          TOTLAV+TOTALT
133100050616     C                   eval      W5PERNOCON = W5TOTNOCON * 100 / W5TOTCONTT
133200050610     C*
133300050616     C* scrittura file
133400050616     C                   exsr      wriwfxls
133500050610     C*
133600011029     C* stampa
133700011029     C                   write     s7w4de
133800001005     C*
133900001005     C                   ENDSR
134000060217     C*--------------------------------------------------------------------------------------------*
134100060217     C* ESEGUE LE OPERAZIONI DI STAMPA per AREA di arrivo
134200060217     C*--------------------------------------------------------------------------------------------*
134300060217     C     staare        BEGSR
134400060217     C*
134500060217     c* decodifica l'AREA di arrivo
134600060217     c                   Z-ADD     depare        ppoa                           *codice
134700060217     C                   movel     *blanks       ppoad                          *descrizione
134800060217     c                   SETOFF                                           99
134900060217     c                   Z-ADD     1             i
135000060217     c     depare        LOOKUP    sare(i)                                99
135100060217if  1c                   IF        *in99                                        *trovato
135200060217     C                   MOVEL     sdare(i)      ppoad
135300060217e   1c                   ENDIF
135400060217     C                   EVAL      ppoad = 'TOT ' + %trim(ppoad)
135500060217     C*
135600060217     C* porta i campi memorizzati in stampa
135700060217     c                   Z-ADD     ATOTCON       PTOTCON                        *valori numerici
135800060217     c                   Z-ADD     ACONANT       PCONANT
135900060217     c                   Z-ADD     ACON00        PCON00
136000060217     c                   Z-ADD     ACON12        PCON12
136100060217     c                   Z-ADD     ACON24        PCON24
136200060217     c                   Z-ADD     ACON48        PCON48
136300060217     c                   Z-ADD     ACON72        PCON72
136400060217     c                   Z-ADD     ACON96        PCON96
136500060217     c                   Z-ADD     ACON120       PCON120
136600060217     c                   Z-ADD     ATMPMED       PTMPMED
136700060217     c                   Z-ADD     ATOTGICH      PTOTGICH
136800060217     c                   Z-ADD     ATOTRESI      PTOTRESI
136900060217     c                   Z-ADD     ATOTANO       PTOTANO
137000060217     c                   Z-ADD     ATOTGIA       PTOTGIA
137100060217     c                   Z-ADD     ATOTPAR       PTOTPAR
137200060217     c                   Z-ADD     ATOTAPP       PTOTAPP
137300060217     c                   Z-ADD     ATOTLAV       PTOTLAV
137400060217     c                   Z-ADD     ATOTALT       PTOTALT
137500060217     c                   Z-ADD     APERANT       PPERANT                        *valori percentuali
137600060217     c                   Z-ADD     APER00        PPER00
137700060217     c                   Z-ADD     APER12        PPER12
137800060217     c                   Z-ADD     APER24        PPER24
137900060217     c                   Z-ADD     APER48        PPER48
138000060217     c                   Z-ADD     APER72        PPER72
138100060217     c                   Z-ADD     APER96        PPER96
138200060217     c                   Z-ADD     APER120       PPER120
138300060217     C*
138400060217     C* ottengo i valori calcolati
138500060217     C                   eval      W5TOTCONTT = ATOTCON+ATOTGICH+ATOTRESI+
138600060217     C                                          ATOTANO+ATOTGIA+ATOTPAR+ATOTAPP+
138700060217     C                                          ATOTLAV+ATOTALT
138800060217     C                   eval      W5PERCONSL = ATOTCON * 100 / W5TOTCONTT
138900060217     C                   eval      W5TOTDADEF = W5TOTCONTT - ATOTCON
139000060217     C                   eval      W5TOTCONL  = ATOTCON+ATOTGICH+ATOTRESI+
139100060217     C                                          ATOTANO
139200060217     C                   eval      W5PERCONL  = W5TOTCONL * 100 / W5TOTCONTT
139300060217     C                   eval      W5TOTNOCON = ATOTGIA+ATOTPAR+ATOTAPP+
139400060217     C                                          ATOTLAV+ATOTALT
139500060217     C                   eval      W5PERNOCON = W5TOTNOCON * 100 / W5TOTCONTT
139600060217     C*
139700060217     C* scrittura file
139800060217     C                   exsr      wriwfxls
139900060217     C* stampa
140000060217     C                   write     s7w4de
140100060217     C*
140200060217     C* stampa riga separazine
140300060217     C                   write     s7w4ri
140400060217     C*
140500060217     C                   ENDSR
140600011029     C*--------------------------------------------------------------------------------------------*
140700011029     C* ESEGUE LE OPERAZIONI DI STAMPA per Totale generale
140800011029     C*--------------------------------------------------------------------------------------------*
140900011029     C     stagen        BEGSR
141000011029     C*
141100011029     c* decodifica il Totale generale
141200011029     c                   Z-ADD     *zeros        ppoa                           *codice
141300011029     C                   EVAL      ppoad = 'TOTALE GENERALE'                    *descrizione
141400011029     C*
141500011029     C* porta i campi memorizzati in stampa
141600011029     c                   Z-ADD     GTOTCON       PTOTCON                        *valori numerici
141700011029     c                   Z-ADD     GCONANT       PCONANT
141800011029     c                   Z-ADD     GCON00        PCON00
141900011029     c                   Z-ADD     GCON12        PCON12
142000011029     c                   Z-ADD     GCON24        PCON24
142100011029     c                   Z-ADD     GCON48        PCON48
142200011029     c                   Z-ADD     GCON72        PCON72
142300011029     c                   Z-ADD     GCON96        PCON96
142400011029     c                   Z-ADD     GCON120       PCON120
142500011029     c                   Z-ADD     GTMPMED       PTMPMED
142600050214     c                   Z-ADD     GTOTGICH      PTOTGICH
142700050214     c                   Z-ADD     GTOTRESI      PTOTRESI
142800011029     c                   Z-ADD     GTOTANO       PTOTANO
142900011029     c                   Z-ADD     GTOTGIA       PTOTGIA
143000050210     c                   Z-ADD     GTOTPAR       PTOTPAR
143100040427     c                   Z-ADD     GTOTAPP       PTOTAPP
143200040427     c                   Z-ADD     GTOTLAV       PTOTLAV
143300011029     c                   Z-ADD     GTOTALT       PTOTALT
143400011029     c                   Z-ADD     GPERANT       PPERANT                        *valori percentuali
143500011029     c                   Z-ADD     GPER00        PPER00
143600011029     c                   Z-ADD     GPER12        PPER12
143700011029     c                   Z-ADD     GPER24        PPER24
143800011029     c                   Z-ADD     GPER48        PPER48
143900011029     c                   Z-ADD     GPER72        PPER72
144000011029     c                   Z-ADD     GPER96        PPER96
144100011029     c                   Z-ADD     GPER120       PPER120
144200050610     C*
144300050610     C* ottengo i valori calcolati
144400050616     C                   eval      W5TOTCONTT = GTOTCON+GTOTGICH+GTOTRESI+
144500050610     C                                          GTOTANO+GTOTGIA+GTOTPAR+
144600050610     C                                          GTOTAPP+GTOTLAV+GTOTALT
144700050616     C                   eval      W5PERCONSL = GTOTCON * 100 / W5TOTCONTT
144800050616     C                   eval      W5TOTDADEF = W5TOTCONTT - GTOTCON
144900050616     C                   eval      W5TOTCONL  = GTOTCON+GTOTGICH+GTOTRESI+
145000050610     C                                          GTOTANO
145100050616     C                   eval      W5PERCONL  = W5TOTCONL * 100 / W5TOTCONTT
145200050616     C                   eval      W5TOTNOCON = GTOTGIA+GTOTPAR+GTOTAPP+
145300050610     C                                          GTOTLAV+GTOTALT
145400050616     C                   eval      W5PERNOCON = W5TOTNOCON * 100 / W5TOTCONTT
145500050610     C*
145600050616     C* scrittura file
145700050616     C                   exsr      wriwfxls
145800060217     C*
145900060217     C* stampa riga separazine
146000060217     C                   write     s7w4ri
146100050610     C*
146200011029     C* stampa
146300011029     C                   write     s7w4de
146400060217     C*
146500060217     C* stampa riga separazine
146600060217     C                   write     s7w4ri
146700011029     C*
146800011029     C                   ENDSR
146900050616     C*--------------------------------------------------------------------------------------------*
147000050616     C* SCRIVE IL WRK-FILE X SCARICO DATI SU FILE XLS
147100050616     C*--------------------------------------------------------------------------------------------*
147200050616     C     wriwfxls      BEGSR
147300050616     C*
147400060217     C* Scrivo solo se richiesto nel lancio
147500060217     C                   if        prmfile = 'S'
147600050616     C                   eval      W5KSC      = KSC
147700050616     C                   eval      W5LNA      = PPOA
147800050616     C                   eval      W5LNADES   = PPOAD
147900050616     C                   eval      W5TOTCON   = PTOTCON
148000050616     C                   eval      W5CONANT   = PCONANT
148100050616     C                   eval      W5CON00    = PCON00
148200050616     C                   eval      W5CON12    = PCON12
148300050616     C                   eval      W5CON24    = PCON24
148400050616     C                   eval      W5CON48    = PCON48
148500050616     C                   eval      W5CON72    = PCON72
148600050616     C                   eval      W5CON96    = PCON96
148700050616     C                   eval      W5CON120   = PCON120
148800050616     C                   eval      W5TMPMED   = PTMPMED
148900050616     C                   eval      W5TOTGICH  = PTOTGICH
149000050616     C                   eval      W5TOTRESI  = PTOTRESI
149100050616     C                   eval      W5TOTANO   = PTOTANO
149200050616     C                   eval      W5TOTGIA   = PTOTGIA
149300050616     C                   eval      W5TOTPAR   = PTOTPAR
149400050616     C                   eval      W5TOTAPP   = PTOTAPP
149500050616     C                   eval      W5TOTLAV   = PTOTLAV
149600050616     C                   eval      W5TOTALT   = PTOTALT
149700050616     C                   eval      W5PERANT   = PPERANT
149800050616     C                   eval      W5PER00    = PPER00
149900050616     C                   eval      W5PER12    = PPER12
150000050616     C                   eval      W5PER24    = PPER24
150100050616     C                   eval      W5PER48    = PPER48
150200050616     C                   eval      W5PER72    = PPER72
150300050616     C                   eval      W5PER96    = PPER96
150400050616     C                   eval      W5PER120   = PPER120
150500050617     C*
150600050617     C* Scarico il buffer del file
150700050617     C                   write     WFS7W510
150800060217     C                   endif
150900050616     C*
151000050616     C                   ENDSR
151100011029     C*--------------------------------------------------------------------------------------------*
151200011029     C* CARICA LE TABELLE OCCORRENTI
151300011029     C*--------------------------------------------------------------------------------------------*
151400011029     C     cartab        BEGSR
151500011029     c*---
151600011029     c* Organigramma
151700011029     c*---
151800011029     c                   CLEAR                   i                              *indice
151900011029     C                   CLEAR                   sorg
152000011029     C                   CLEAR                   sdorg
152100011029     c     *loval        SETLL     azorg01l
152200011029     c                   READ      azorg01l                               99
152300011029do  1c                   DOW       NOT *in99
152400011029if  2c                   IF        orgfva = *blanks AND
152500011029     c                             orgfag <> 'V'
152600011029     c                   ADD       1             i
152700011029     c                   Z-ADD     orgfil        sorg(i)
152800011029     c                   MOVEL     orgdes        sdorg(i)
152900011029e   2c                   ENDIF
153000011029     c                   READ      azorg01l                               99
153100011029e   1c                   ENDDO
153200060217     c*---
153300060217     c* Aree
153400060217     c*---
153500060217     c                   CLEAR                   i                              *indice
153600060217     C                   CLEAR                   sare
153700060217     C                   CLEAR                   sdare
153800060217     C                   Z-ADD     1             tute
153900060217     C                   MOVEL     '05'          tcod
154000060217     c     ke2tab        SETLL     tabel00f
154100060217     c     ke2tab        READE     tabel00f                               99
154200060217do  1c                   DOW       NOT *in99
154300060217if  2c                   IF        tblflg = *blanks
154400060217     c                   ADD       1             i
154500060217     c                   MOVEL     tblkey        sare(i)
154600060217     C                   MOVEL     tbluni        DS05
154700060217     c                   MOVEL     §05des        sdare(i)
154800060217e   2c                   ENDIF
154900060217     c     ke2tab        READE     tabel00f                               99
155000060217e   1c                   ENDDO
155100011029     c*---
155200011029     c* Provincie
155300011029     c*---
155400011029     c                   CLEAR                   i                              *indice
155500011029     C                   CLEAR                   sprv
155600011029     C                   CLEAR                   sdprv
155700011029     C                   Z-ADD     1             tute
155800011029     C                   MOVEL     'PR'          tcod
155900011029     c     ke2tab        SETLL     tabel00f
156000011029     c     ke2tab        READE     tabel00f                               99
156100011029do  1c                   DOW       NOT *in99
156200011029if  2c                   IF        tblflg = *blanks
156300011029     c                   ADD       1             i
156400011029     c                   MOVE      tblkey        sprv(i)
156500011029     C                   MOVEL     tbluni        DSPR
156600011029     c                   MOVEL     §prdes        sdprv(i)
156700011029e   2c                   ENDIF
156800011029     c     ke2tab        READE     tabel00f                               99
156900011029e   1c                   ENDDO
157000040427     c*---
157100040427     c* Eventi d tipo "lasciato avviso"
157200040427     c*---
157300040427     C                   clear                   sk2Alav
157400040427     C                   z-add     *zeros        j                 3 0
157500040427     C                   eval      tute = 1
157600040427     C                   eval      tcod = '2A'
157700040427     C     ke2tab        setll     tabel00f
157800040427     C     ke2tab        reade     tabel00f
157900040427     C                   dow       not %eof(tabel00f)
158000040427     C                   movel(p)  tblUNI        ds2a
158100040427     C                   if        §2aFTC = 'A'
158200040427     C                   add       1             j
158300040427     C                   eval      sk2Alav(j) = tblKEY
158400040427     C                   endif
158500040427     C     ke2tab        reade     tabel00f
158600040427     C                   enddo
158700011029     C*
158800011029     c                   ENDSR
158900001005     C*--------------------------------------------------------------------------------------------*
159000001006     C* IMPOSTA I CAMPI DI TESTATA IN STAMPA E VALORIZZO CAMPONE "USRDFNDTA" (per 100 byte)
159100001005     C*--------------------------------------------------------------------------------------------*
159200001207     C     imppar        BEGSR
159300001207     C*
159400001207     C* Pulisce il formato record di testata del PRTF
159500011026     C                   clear                   s7w4te
159600001005     C*
159700001005     C* Inverto le date in input
159800001005     C                   z-add     *zeros        G08DAT
159900001005     C                   z-add     PARDAI        G08INV
160000001005     C                   movel     '3'           G08ERR
160100001005     C                   call      'XSRDA8'
160200001005     C                   parm                    WLBDA8
160300001005     C                   z-add     G08DAT        STPDAI
160400001006     C                   eval      desdai = %trim(%editc(PARDAI:'Z'))
160500001005     C*
160600001005     C                   z-add     *zeros        G08DAT
160700001005     C                   z-add     PARDAF        G08INV
160800001005     C                   movel     '3'           G08ERR
160900001005     C                   call      'XSRDA8'
161000001005     C                   parm                    WLBDA8
161100001005     C                   z-add     G08DAT        STPDAF
161200001006     C                   eval      desdaf = %trim(%editc(PARDAF:'Z'))
161300001005     C*
161400001005     C* Reperisco la descrizione del Punto Operativo e valorizzo i due parametri ad esso riferiti
161500011026     c                   if        lna = 999
161600011026     c                   eval      STPLNAD = 'Tutti'
161700011026     c                   else
161800001030     C     lna           chain     azorg01l
161900001005     C                   if        %found(azorg01l)
162000001005     C                   movel     orgdes        STPLNAD
162100001006     C                   eval      despoa = %trim(%editc(lna:'Z'))
162200001005     C                   endif
162300011026     c                   endif
162400001005     C                   z-add     lna           STPLNA
162500011029     C*
162600011029     C* Reperisco la descrizione della provincia e valorizzo entrambi i parametri ad essa riferiti
162700011029     C                   movel     prd           STPPRD
162800011029     C                   movel     'PR'          tcod
162900011029     C                   movel(p)  prd           tkey
163000011029     C     keytab        chain     tabel00f
163100011029     C                   if        %found(tabel00f)
163200011029     C                   movel     tbluni        DSPR
163300011029     C                   movel     §PRDES        STPPRDD
163400011029     C                   eval      desprd = %trim(nprd)
163500011029     C                   endif
163600001005     C*
163700001005     C* Quindi tutti i rimanenti parametri
163800001005     C                   movel     ksc           STPKSU
163900001005     C                   movel     ndst          STPDST
164000001006     C                   eval      desdst = %trim(ndst)
164100001005     C                   movel     nmit          STPMIT
164200001006     C                   eval      desmit = %trim(nmit)
164300001005     C                   movel     lod           STPLOD
164400001006     C                   eval      deslod = %trim(nlod)
164500001005     C                   if        ckc <> *blanks
164600001207     C                   movel     'S'           STPFCO
164700001006     C                   eval      desfl1 = 'CONS.'
164800001005     C                   endif
164900011128     C                   if        ckm <> *blanks
165000011128     C                   movel     'S'           STPFCM
165100011128     C                   eval      desfl1 = 'ANOM.'
165200011128     C                   endif
165300050210     C                   if        ckg <> *blanks
165400050210     C                   eval      STPFLG = %trim(STPFLG) + ' GI'
165500050210     C                   eval      desfl2 = 'GIAC.'
165600050210     C                   endif
165700050210     C                   if        ckp <> *blanks
165800050210     C                   eval      STPFLG = %trim(STPFLG) + ' CP'
165900050210     C                   eval      desfl3 = 'PARZ.'
166000050210     C                   endif
166100050210     C                   if        ckt <> *blanks
166200050210     C                   eval      STPFLG = %trim(STPFLG) + ' AP'
166300050210     C                   eval      desfl4 = 'APP.'
166400050210     C                   endif
166500050210     C                   if        ckl <> *blanks
166600050210     C                   eval      STPFLG = %trim(STPFLG) + ' LA'
166700050210     C                   eval      desfl5 = 'L.AVV'
166800050210     C                   endif
166900050210     C                   if        cka <> *blanks
167000050210     C                   eval      STPFLG = %trim(STPFLG) + ' AL'
167100050210     C                   eval      desfl6 = 'ALTRE'
167200050210     C                   endif
167300001006     C*
167400001006     C* A questo punto compongo la stringa da passare nell'USRDFNDTA e i dati di servizio
167500001006     C                   move      rqscid        sgiusrnbr
167600001006     C                   eval      tipstp = 'REC'
167700001006     C                   eval      tipsnd = 'SGI'
167800001006     C                   eval      email = *blanks
167900001006     C*
168000001009     C                   eval      varcmd=''''+dessta+' '+%trim(prmdat)+' '+
168100001009     C                                        %trim(prmora)
168200001006     C                   if        desdai>*blanks
168300001006     C                   eval      varcmd = %trim(varcmd)+%trim(desdai)+'+'
168400001006     C                   endif
168500001006     C                   if        desdaf>*blanks
168600001006     C                   eval      varcmd = %trim(varcmd)+%trim(desdaf)+'+'
168700001006     C                   endif
168800001006     C                   if        despoa>*blanks
168900001006     C                   eval      varcmd = %trim(varcmd)+%trim(despoa)+'+'
169000001006     C                   endif
169100001006     C                   if        desprd>*blanks
169200001006     C                   eval      varcmd = %trim(varcmd)+%trim(desprd)+'+'
169300001006     C                   endif
169400001006     C                   if        deslod>*blanks
169500001006     C                   eval      varcmd = %trim(varcmd)+%trim(deslod)+'+'
169600001006     C                   endif
169700001006     C                   if        desfl1>*blanks
169800001006     C                   eval      varcmd = %trim(varcmd)+%trim(desfl1)+'+'
169900001006     C                   endif
170000001006     C                   if        desfl2>*blanks
170100001006     C                   eval      varcmd = %trim(varcmd)+%trim(desfl2)+'+'
170200001006     C                   endif
170300001006     C                   if        desfl3>*blanks
170400001006     C                   eval      varcmd = %trim(varcmd)+%trim(desfl3)+'+'
170500001006     C                   endif
170600050210     C                   if        desfl4>*blanks
170700050210     C                   eval      varcmd = %trim(varcmd)+%trim(desfl4)+'+'
170800050210     C                   endif
170900050210     C                   if        desfl5>*blanks
171000050210     C                   eval      varcmd = %trim(varcmd)+%trim(desfl5)+'+'
171100050210     C                   endif
171200050210     C                   if        desfl6>*blanks
171300050210     C                   eval      varcmd = %trim(varcmd)+%trim(desfl6)+'+'
171400050210     C                   endif
171500001006     C                   if        desmit>*blanks
171600001006     C                   eval      varcmd = %trim(varcmd)+%trim(desmit)+'+'
171700001006     C                   endif
171800001006     C                   if        desdst>*blanks
171900001006     C                   eval      varcmd = %trim(varcmd)+%trim(desdst)
172000001006     C                   endif
172100001009     C                   if        %len(%trim(varcmd)) >=97
172200001009     C                   eval      %subst(var100:97:4) = '...'
172300001009     C                   else
172400001009     C                   eval      varcmd = %trim(varcmd)
172500001009     C                   endif
172600001009     C                   movel     varcmd        var100
172700050210     C                   move(p)   param         varsrv
172800001006     C                   eval      varcmd = %trim(CMD(1))+' '+%trim(CMD(2))+
172900001006     C                                      var100 + varsrv+''''+')'
173000001005     C*
173100001005     C                   ENDSR
173200050614     C*--------------------------------------------------------------------------------------------*
173300050614     C* operazioni iniziali
173400050614     C*--------------------------------------------------------------------------------------------*
173500050614     C     opeini        BEGSR
173600050614     C*
173700050614     C* Aggiunge le librerie occorrenti in lista
173800050614     C                   EVAL      wpgm = 'GAITRAOBJ/TIS777C'
173900050614     C                   CALL      wpgm
174000050614     C*--------------------
174100050614     C* CALCOLA LA DATA CORRENTE
174200050614     C*--------------------
174300050614     C                   time                    WN14             14 0          *ORA (6)+ DATA (8)
174400050614     C                   move      WN14          WN8               8 0          *DATA (8) IN G/M/AA
174500050614     C                   z-add     WN8           G08DAT
174600050614     C                   z-add     *zeros        G08INV
174700050614     C                   movel     '0'           G08ERR
174800050614     C                   call      'XSRDA8'
174900050614     C                   parm                    WLBDA8
175000050614     C                   z-add     G08INV        DATCOR            8 0          *DATA CORRENTE AA/M/
175100050614     c*
175200050614     c* IMPOSTAZIONE SCHIERA DEI CLIENTI DA ELABORARE
175300050614     c* Se richiesta elaborazione per cliente unificante richiamo tibs10r
175400050614     c                   clear                   sksc7
175500050614     c                   if        prmsuf = 'U'
175600050614     c                   clear                   tibs10ds
175700050614     c                   eval      d10tle = prmtlg
175800050614     c                   eval      d10paf = 'F'
175900050614     c                   move      ksc           d10cod
176000050614     c                   z-add     datcor        d10drf
176100050614     c                   call      'TIBS10R'
176200050614     c                   Parm                    Tibs10ds
176300050614     c                   if        d10err = *blanks
176400050614     c                   z-add     sksc11        sksc7
176500050614     c                   else
176600050614     c                   move      ksc           sksc7(1)
176700050614     c                   endif
176800050614     c                   else
176900050614     c* altrimenti imposto primo elemnto schiera = codice cliente ricevuto
177000050614     c                   move      ksc           sksc7(1)
177100050614     c                   endif
177200050614     C*
177300050614     C* Scrivo i clienti così estratti nel wrk-file d procedura
177400050614     C                   if        not %open(wfs7w50f)
177500050614     C                   open      wfs7w50f
177600050614     C                   endif
177700050614     C                   z-add     1             wIndice           3 0
177800050614     C                   dow       wIndice <= %elem(sksc7)
177900050614     C                   if        sksc7(wIndice) <> *zeros
178000050614     C                   eval      s7w5ksc = sksc7(wIndice)
178100050614     C                   write     wfs7w500
178200050614     C                   else
178300050614     C                   leave
178400050614     C                   endif
178500050614     C                   eval      wIndice = wIndice + 1
178600050614     C                   enddo
178700050614     C                   if        %open(wfs7w50f)
178800050614     C                   close     wfs7w50f
178900050614     C                   endif
179000050614     C*
179100050614     C* Se richiesta generazione file eseguo pulizia preventiva mirata al cliente
179200050614     C                   if        prmfile = 'S'
179300071023     C* File WFS7W51F
179400050614     C     KSC           setll     wfs7w51f
179500050614     C                   if        %found(wfs7w51f)
179600050614     C     KSC           reade     wfs7w51f
179700050614     C                   dow       not %eof(wfs7w51f)
179800050614     C                   delete    wfs7w51f
179900050614     C     KSC           reade     wfs7w51f
180000050614     C                   enddo
180100050614     C                   endif
180200071023     C* File WFS7W52F
180300071023     C     KSC           setll     wfs7w52f
180400071023     C                   if        %found(wfs7w52f)
180500071023     C     KSC           reade     wfs7w52f
180600071023     C                   dow       not %eof(wfs7w52f)
180700071023     C                   delete    wfs7w52f
180800071023     C     KSC           reade     wfs7w52f
180900071023     C                   enddo
181000071023     C                   endif
181100050614     C                   endif
181200050614     C*
181300050614     C                   ENDSR
181400080214     C*--------------------------------------------------------------------------------------------*
181500080214     C* Gestione errori imprevisti
181600080214     C*--------------------------------------------------------------------------------------------*
181700080214     C     *pssr         BEGSR
181800080214     C*
181900080214     C                   rolbk
182000080214     C*
182100080214     C                   seton                                        LR
182200080214     C*
182300080214     C                   ENDSR     '*CANCL'
182400001003     C*--------------------------------------------------------------------------------------------*
182500001003     C* impostazioni iniziali
182600011029     C*--------------------------------------------------------------------------------------------*
182700001003     C     *inzsr        BEGSR
182800001003     C*
182900001003     C* Ricevimento parametri
183000001003     C     *ENTRY        PLIST
183100040628     C                   PARM                    KPJBA
183200050210     C                   MOVEL     KPJBU         PARAMETRI
183300040628     C*
183400040628     C                   EVAL      TIS1W2IDS = PRMTIS1W2IDS
183500001003     C*
183600001003     C* Chiavi di lettura
183700001003     C     keytas        KLIST                                                  *TITAS30C
183800001003     C                   KFLD                    ktaaas
183900001003     C                   KFLD                    ktalnp
184000001003     C                   KFLD                    ktanrs
184100001003     C                   KFLD                    ktansp
184200001003     C                   KFLD                    ktatbl
184300011005     c     ke2ta4        KLIST                                                  tita430c
184400011005     c                   KFLD                    ka4aas                         -anno spedizione
184500011005     c                   KFLD                    ka4lnp                         -linea partenza
184600011005     c                   KFLD                    ka4nrs                         -serie
184700011005     c                   KFLD                    ka4nsp                         -spedizione
184800071023     c     keyta4        KLIST                                                  tita430c
184900071023     c                   KFLD                    ka4aas                         -anno spedizione
185000071023     c                   KFLD                    ka4lnp                         -linea partenza
185100071023     c                   KFLD                    ka4nrs                         -serie
185200071023     c                   KFLD                    ka4nsp                         -spedizione
185300071023     c                   KFLD                    ka4trc                         -tipo record
185400050321     c     keygcp        KLIST                                                  *tigcp51l
185500021108     c                   KFLD                    kcpaas                         -anno spedizione
185600021108     c                   KFLD                    kcplnp                         -linea partenza
185700021108     c                   KFLD                    kcpnrs                         -serie
185800021108     c                   KFLD                    kcpnsp                         -spedizione
185900021108     c                   KFLD                    kcpfrg                         -prg aperture
186000050210     c     keyevb        KLIST                                                  *fnevb04l
186100040427     c                   KFLD                    kevaas                         -anno spedizione
186200040427     c                   KFLD                    kevlnp                         -linea partenza
186300040427     c                   KFLD                    kevnrs                         -serie
186400040427     c                   KFLD                    kevnsp                         -spedizione
186500050210     c     keyevb2       KLIST                                                  *fnevb04l
186600050210     c                   KFLD                    kevaas                         -anno spedizione
186700050210     c                   KFLD                    kevlnp                         -linea partenza
186800050210     c                   KFLD                    kevnrs                         -serie
186900050210     c                   KFLD                    kevnsp                         -spedizione
187000050210     c                   KFLD                    kevcev                         -codice evento
187100041122     C     keylbl1c      KLIST                                                  *fnlbl01l
187200041122     C                   KFLD                    klbaas
187300041122     C                   KFLD                    klblnp
187400041122     C                   KFLD                    klbnrs
187500041122     C                   KFLD                    klbnsp
187600001005     C*
187700001005     C* Definizione chiavi - TABEL00F
187800001005     C     keytab        KLIST
187900001005     C                   KFLD                    tute
188000001005     C                   KFLD                    tcod
188100001005     C                   KFLD                    tkey
188200011029     C     ke2tab        KLIST
188300011029     C                   KFLD                    tute
188400011029     C                   KFLD                    tcod
188500001003     C*
188600001003     C                   ENDSR
188700011029     O*--------------------------------------------------------------------------------------------*
188800050214     OPRTF198   E            DettBolla        01
188900050214     O                                        +   1 'Chiave bolla:'
189000050214     O                       dskeybol         +   1
189100050214     O                                        +  10 'Stato bolla:'
189200050214     O                       $stato           +   1
189300001005** CMD - COMANDI CL
189400050524OVRPRTF OVRSCOPE(*CALLLVL) TOFILE(TIS7W4P)
189500001006OUTQ(STRATEOBJ/SGIIN) USRDFNDTA(
189600040427DLTOVR FILE(WURTH) LVL(*)
189700001006
