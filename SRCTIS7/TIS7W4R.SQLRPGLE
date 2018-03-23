000100001005     H*--------------------------------------------------------------------------------------------*
000200001005     H* Crea la statistica "RIEPILOGO ESITI CONSEGNE" per WURTH
000300011029     h*
000400011029     h* NOTE: Il pgm ha bisogno che il file printer TIS7W4P si chiami WURTH, ma il comando PC
000500011029     h* andando in batch non prenderebbe la sostituzione, QUINDI occorre copiare in QGPL il printer
000600011029     h* file TIS7W4P chiamandolo WURTH e poi compilare con PC (dopo ricordarsi di cancellarlo).
000700001005     H*--------------------------------------------------------------------------------------------*
000800001005     H/TITLE Carica righe spedizione trovate
000900060613     Hdatedit(*DMY)
001000001005     F*--------------------------------------------------------------------------------------------*
001100001005     F* Data base
001200001005     F*--------------------------------------------------------------------------------------------*
001300001005     Fazorg01l  if   e           k disk    usropn
001400001005     Ftabel00f  if   e           k disk    usropn
001500001005     Ftitas30c  if   e           k disk    usropn ignore(titasp00)
001600011005     ftita430c  IF   E           K DISK    USROPN
001700050321     ftigcp51l  IF   E           K DISK    USROPN
001800050210     ffnevb04l  IF   E           K DISK    USROPN
001900001006     Fwurth     O    e             PRINTER usropn
002000001005     F                                     oflind(*in05)
002100050215     Fprtf198   O    f  132        printer oflind(*inof) usropn
002200001005     D*--------------------------------------------------------------------------------------------*
002300001005     D* Data structure
002400001005     D*--------------------------------------------------------------------------------------------*
002500001005     D*---
002600001005     D* Costanti
002700001005     D*---
002800001005     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
002900001005     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
003000001005     D*---
003100001005     D* Variabili riferite al data base
003200001005     D*---
003300001005     D ktaaas          S                   LIKE(tasaas)
003400001005     D ktalnp          S                   LIKE(taslnp)
003500001005     D ktanrs          S                   LIKE(tasnrs)
003600001005     D ktansp          S                   LIKE(tasnsp)
003700001005     D ktatbl          S                   LIKE(tastbl)
003800011005     d ka4aas          S                   LIKE(ta4aas)                         *lettura tita430c
003900011005     d ka4lnp          S                   LIKE(ta4lnp)
004000011005     d ka4nrs          S                   LIKE(ta4nrs)
004100011005     d ka4nsp          S                   LIKE(ta4nsp)
004200011005     d ka4trc          S                   LIKE(ta4trc)
004300050321     d kcpaas          S                   LIKE(gcpaas)                         *lettura tigcp00f
004400021108     d kcplnp          S                   LIKE(gcplnp)
004500021108     d kcpnrs          S                   LIKE(gcpnrs)
004600021108     d kcpnsp          S                   LIKE(gcpnsp)
004700021108     d kcpfrg          S                   LIKE(gcpfrg)
004800040427     d kevaas          S                   LIKE(evbaas)                         *lettura fnevb00f
004900040427     d kevlnp          S                   LIKE(evblnp)
005000040427     d kevnrs          S                   LIKE(evbnrs)
005100040427     d kevnsp          S                   LIKE(evbnsp)
005200050210     d kevcev          S                   LIKE(evbcev)
005300001005     D ndst            S                   LIKE(Dst)                            *campi"normalizzati"
005400001005     D nmit            S                   LIKE(Mit)                            *campi"normalizzati"
005500001005     D nprd            S                   LIKE(Prd)                            *campi"normalizzati"
005600001005     D nlod            S                   LIKE(Lod)                            *campi"normalizzati"
005700001005     D tute            S                   LIKE(tblkut) inz(1)
005800001005     D tcod            S                   LIKE(tblcod)
005900001005     D tkey            S                   LIKE(tblkey)
006000011029     D deppoa          S                   LIKE(vtapoa)                         *depositi
006100060221     D depare          S                   LIKE(orgcar)                         *depositi
006200001005     D*---
006300001005     D* Variabili
006400001005     D*---
006500001006     D divis           s             10  3 inz(*zeros)
006600001005     D pardai          s              8  0 inz(*zeros)
006700001005     D pardaf          s              8  0 inz(*zeros)
006800011213     D a35             s             35    inz(*blanks)
006900011128     D a1              s              1    inz(*blanks)
007000001005     D pos             s              2  0 inz(*zeros)
007100001005     D wrkdata         s               d   DATFMT(*ISO) INZ(D'1999-01-01')
007200001005     D wpgm            s             20                                         *programma
007300001009     D dessta          c                   const('Esiti consegne:')
007400001006     D prmdat          s             10
007500001005     D prmora          s              8
007600001006     D desdai          s              8    inz(*blanks)
007700001006     D desdaf          s              8    inz(*blanks)
007800001006     D despoa          s              3    inz(*blanks)
007900001006     D desprd          s              2    inz(*blanks)
008000001006     D deslod          s              2    inz(*blanks)
008100001006     D desfl1          s              5    inz(*blanks)
008200001006     D desfl2          s              5    inz(*blanks)
008300001006     D desfl3          s              5    inz(*blanks)
008400050210     D desfl4          s              5    inz(*blanks)
008500050210     D desfl5          s              5    inz(*blanks)
008600050210     D desfl6          s              5    inz(*blanks)
008700001006     D desmit          s             20    inz(*blanks)
008800001006     D desdst          s             20    inz(*blanks)
008900001006     D var100          s            100    inz(*blanks)
009000001006     D varcmd          s            400    inz(*blanks)
009100001006     D varsrv          s            155    inz(*blanks)
009200011029     d i               S              5  0 inz(*zeros)
009300011029     D*---
009400011029     D* Variabili di memorizzazione
009500060221     D*---
009600060221     D* GENERALI
009700060221     D*---
009800011029     d GTOTCON         S              7  0 inz(*zeros)
009900011029     d GCONANT         S              7  0 inz(*zeros)
010000011029     d GCON00          S              7  0 inz(*zeros)
010100011029     d GCON12          S              7  0 inz(*zeros)
010200011029     d GCON24          S              7  0 inz(*zeros)
010300011029     d GCON48          S              7  0 inz(*zeros)
010400011029     d GCON72          S              7  0 inz(*zeros)
010500011029     d GCON96          S              5  2 inz(*zeros)
010600011029     d GCON120         S              7  0 inz(*zeros)
010700011029     d GTMPMED         S              7  0 inz(*zeros)
010800050214     d GTOTGICH        S              7  0 inz(*zeros)
010900050214     d GTOTRESI        S              7  0 inz(*zeros)
011000011029     d GTOTANO         S              7  0 inz(*zeros)
011100011029     d GTOTGIA         S              7  0 inz(*zeros)
011200050210     d GTOTPAR         S              7  0 inz(*zeros)
011300040427     d GTOTAPP         S              7  0 inz(*zeros)
011400040427     d GTOTLAV         S              7  0 inz(*zeros)
011500011029     d GTOTALT         S              7  0 inz(*zeros)
011600011029     d GORETOT         S             20  0 inz(*zeros)
011700011029     d GPERANT         S              5  2 inz(*zeros)
011800011029     d GPER00          S              5  2 inz(*zeros)
011900011029     d GPER12          S              5  2 inz(*zeros)
012000011029     d GPER24          S              5  2 inz(*zeros)
012100011029     d GPER48          S              5  2 inz(*zeros)
012200011029     d GPER72          S              5  2 inz(*zeros)
012300011029     d GPER96          S              5  2 inz(*zeros)
012400011029     d GPER120         S              5  2 inz(*zeros)
012500010926     D*---
012600010926     D* Variabili di lavoro
012700010926     D*---
012800010926     d $statoOK        S              1                                         *stato bolla OK
012900050214     d $stato          S              5                                         *stati bolla
013000011005     d $notgia         S              1    INZ('N')                             *note "giacenza"
013100050321     d $gcp            S              1    INZ('N')                             *giacenza in TIGCP
013200050210     d $parz           S              1    INZ('N')                             *cons.parziale
013300040427     d $lav            S              1    INZ('N')                             *l.avviso in FNEVB
013400011029     d $MPoa           S              1    INZ('N')                             *d.memorizzati POA
013500011029     d $MGen           S              1    INZ('N')                             *d.memorizzati GEN
013600011029     D*---
013700011029     D* Schiere
013800011029     D*---
013900011029     d sorg            S              3  0 DIM(300)                             *organigramma
014000011029     d sdorg           S             20    DIM(300)
014100060221     D sare            S              3  0 DIM(300)                             *aree
014200060221     D sdare           S             25    DIM(300)
014300011029     d sprv            S              2    DIM(200)                             *provincie
014400011029     d sdprv           S             20    DIM(200)
014500080903     D sk2Alav         S              3    DIM(200)                             *eventi L/A
014600001006     D*---
014700001006     D* DS dati di servizio per gestione remota della stampa
014800001006     D*---
014900001207     D                 ds                  inz
015000001006     D sgiusrnbr               1      9
015100001006     D tipstp                 10     12
015200001006     D tipsnd                 13     14
015300001006     D email                  15     64
015400050210     D param                   1     64
015500001005     D*---
015600001005     D* DS controlla data
015700001005     D*---
015800001207     D wlbda8          ds                  inz                                  *controlla data
015900001005     D  g08dat                        8  0                                       -data dritta
016000001005     D  g08inv                        8  0                                       -data invertita
016100001005     D  g08err                        1                                          -errore
016200001005     D  g08tgi                        5  0                                       -giorni fra date
016300011128      * scomposizione data generica
016400001207     D                 ds                  inz
016500001005     D  dsann                  1      4  0                                       -anno
016600001005     D  dsmes                  5      6  0                                       -mese
016700001005     D  dsgio                  7      8  0                                       -giorno
016800001005     D  dsmgs                  5      8  0                                       -giorno
016900001005     D dsdat                   1      8  0                                      *data
017000011128      * Scomposizione data editata
017100011128     d                 DS                  INZ
017200011128     d  dsgioe                 1      2                                          -giorno
017300011128     d  dsvd1                  3      3                                          -'/'
017400011128     d  dsmese                 4      5                                          -mese
017500011128     d  dsvd2                  6      6                                          -'/'
017600011128     d  dsanne                 7     10                                          -anno
017700011128     d dsdate                  1     10                                         *data editata
017800011128      * Composizione data NON editata -alfabetica-
017900011128     d                 DS                  INZ
018000011128     d  dsgioa                 1      2                                          -giorno
018100011128     d  dsmesa                 3      4                                          -mese
018200011128     d  dsanna                 5      8                                          -anno
018300011128     d dsdata                  1      8                                         *data NON editata
018400050214      * Composizione chiave bolla
018500050214     d dskeybol        DS                  INZ
018600050214     d  tasaas                        4  0                                       -annp
018700050214     d  taslnp                        3  0                                       -linea partenza
018800050214     d  tasnrs                        2  0                                       -serie
018900050214     d  tasnsp                        7  0                                       -numero spedizione
019000001005     D*---
019100001005     D* DS contenente i dati dI INPUT
019200001005     D*---
019300001207     D TIS1W2IDS     e ds
019400001005     D*---
019500001005     D* DS per lettura recordset da selezione SQL
019600001005     D*---
019700090918     D DSVTA         e ds                  inz extname(TIVTA10F)
019800060221 xxx D S_ORGCAR        s                   like(ORGCAR)
019900001005     D*---
020000001005     D* DS per scomposizione tabella provincie
020100001005     D*---
020200001207     D DSPR          e ds                  inz
020300060221     D*---
020400060221     D* DS per scomposizione tabella aree
020500060221     D*---
020600060221     D DS05          e ds                  inz
020700040427     D*---
020800040427     D* DS per scomposizione tabella codici eventi (2A)
020900040427     D*---
021000040427     D DS2A          e ds                  inz
021100090609
021200090609     d ds5e          e ds
021300001003     D*------------
021400001003     D* COMANDI
021500001003     D*------------
021600001005     D CMD             S             80    DIM(4) CTDATA PERRCD(1)
021700001003     C*--------------------------------------------------------------------------------------------*
021800001003     C* Main lines
021900001003     C*--------------------------------------------------------------------------------------------*
022000080606     C*
022100080606     C/EXEC SQL
022200080606     C+ SET OPTION CLOSQLCSR = *ENDMOD, DYNUSRPRF = *OWNER
022300080606     C/END-EXEC
022400011029     C*
022500011029     C* apre i file
022600011029     C                   open      tabel00f
022700011029     C                   open      azorg01l
022800011029     C*
022900011029     C* Esegue prime considerazioni sui parametri ricevuti
023000011029     C                   exsr      norminput
023100011029     C*
023200011029     C* carica le tabelle occorrenti
023300011029     C                   exsr      cartab
023400011029     C*
023500011029     C* imposta i parametri di lancio (servono per l'ORVPRTF)
023600011029     C                   exsr      imppar                                       *impsta parametri
023700011029     C* procede
023800001003     C                   exsr      procedi
023900011029     C*
024000011029     C* chide i file
024100011029     C                   close     tabel00f
024200011029     C                   close     azorg01l
024300001003     C*
024400001003     C                   seton                                        LR
024500011026     C*--------------------------------------------------------------------------------------------*
024600001003     C* PROCEDI - Routine di elaborazione
024700011026     C*--------------------------------------------------------------------------------------------*
024800001003     C     PROCEDI       BEGSR
024900001003     C*
025000001003     C* Eseguo le operazioni di sovrapposizione del printer-file
025100001006     C                   Z-ADD     400           LENGH            15 5
025200001006     C                   CALL(e)   'QCMDEXC'                                    *LANCIA CMD
025300001006     C                   PARM                    varcmd
025400001003     C                   PARM                    LENGH
025500001003     C*
025600011029     C* Se riuscito comando, procede
025700011029if  1C                   if        not %error
025800011029     C*
025900011029     C* apre i file
026000001006     C                   open      wurth
026100050215     C                   open      prtf198
026200001005     C                   open      titas30c
026300011005     C                   open      tita430c
026400050321     c                   open      tigcp51l
026500050210     C                   open      fnevb04l
026600011029     C*
026700011029     C* stampa la testata
026800011029     c                   exsr      states
026900011029     c*
027000001003     C*
027100001003     C/EXEC SQL
027200001003     C+ declare C1 cursor for
027300060221     C+ select VTA.*, orgcar from (
027400090918     C+ select * from tivta10f where
027500001003     C+ vtaksu = :KSC and
027600060221     C+ vtadsp >= :PARDAI and vtadsp <= :PARDAF
027700060221     C+ ) as VTA join azorg00f
027800060221     C+ on vtapoa=orgfil
027900060221     C+ order by orgcar, vtapoa
028000001003     C+ for fetch only
028100001003     C/END-EXEC
028200001003     C
028300001003     C/EXEC SQL
028400001003     C+ open C1
028500001003     C/END-EXEC
028600001003     C
028700001003     C/EXEC SQL
028800060221     C+ Fetch C1 into :dsvta, :s_orgcar
028900001003     C/END-EXEC
029000011029     c*
029100011029do  2C                   dow       sqlcod = *zeros
029200060221     C*
029300060221     C* operazioni per nuova area
029400060221     C                   exsr      neware
029500060221     C*
029600060221     C* ciclo fino a rottura area
029700060221do  3C                   dow       sqlcod = *zeros   AND
029800060221     c                             depare = S_orgcar
029900011026     C*
030000011026     C* operazioni per nuovo punto operativo di arrivo
030100060221     C                   exsr      newpoa
030200011026     C*
030300011026     C* ciclo fino a rottura punto operativo di arrivo
030400060221do  4C                   dow       sqlcod = *zeros AND
030500011026     c                             deppoa = vtapoa
030600011026     C*
030700011026     C* controlla validità record
030800011026     C                   exsr      chkvta
030900011026     C*
031000011029     C* memorizza i dati
031100011029if  4C                   if        errl00 = '0'
031200011026     C                   exsr      membol
031300011029e   4C                   endif
031400001003     C*
031500001003     C/EXEC SQL
031600060221     C+ Fetch C1 into :dsvta, :s_orgcar
031700001003     C/END-EXEC
031800060221e   4C                   enddo                                                  *rottura linea
031900011029     C*
032000011029     C* stampa il totale punto operativo di arrivo
032100011029if  3c                   if        $mpoa = 'S'                                  *memorizzato qc
032200011029     C                   exsr      perpoa
032300011029     C                   exsr      stapoa
032400011029e   3c                   endif
032500060221     C*
032600060221e   3C                   enddo                                                  *rottura area
032700060221     C*
032800060221     C* stampa il totale punto operativo di arrivo
032900060221if  3c                   if        $mpoa = 'S'                                  *memorizzato qc
033000060221     C                   exsr      perare
033100060221     C                   exsr      staare
033200060221e   3c                   endif
033300011029     c*
033400011029e   2C                   enddo                                                  *fine file
033500001003     C*
033600001003     C/EXEC SQL
033700001003     C+ close C1
033800001003     C/END-EXEC
033900011029     C*
034000011029     C* stampa il totale generale
034100011029if  2c                   if        $mgen = 'S'                                  *memorizzato qc
034200011029     C                   exsr      pergen
034300011029     C                   exsr      stagen
034400011029e   2c                   endif
034500011029     C*
034600011029     C* stampa la riga finale
034700011029     c                   exsr      stafin
034800001003     C*
034900011029     C* chiude i file
035000011029     C                   close     wurth
035100050215     C                   close     prtf198
035200001005     C                   close     titas30c
035300011005     C                   close     tita430c
035400050321     c                   close     tigcp51l
035500050210     C                   close     fnevb04l
035600001005     C*
035700001005     C* Cancello le operazioni di sovrapposizione del printer-file
035800001005     C                   Z-ADD     80            LENGH            15 5
035900001005     C                   CALL(e)   'QCMDEXC'                                    *LANCIA CMD
036000001006     C                   PARM                    CMD(3)
036100001005     C                   PARM                    LENGH
036200011029e   1C                   endif
036300001003     C*
036400001003     C                   ENDSR
036500001003     C*--------------------------------------------------------------------------------------------*
036600001003     C* NORMALIZZA I CAMPI RICEVUTI IN INPUT
036700001003     C*--------------------------------------------------------------------------------------------*
036800001003     C     norminput     BEGSR
036900011128     c*
037000011128     C* data fine, se non inserita = data corrente
037100011128     C                   Z-ADD     datcor        pardaf
037200011128     c*
037300011128     c* controlla se la data fine è numerica
037400011128     c                   MOVEL     Dfi           dsdate
037500011128     c                   MOVEL     dsanne        dsanna
037600011128     c                   MOVEL     dsmese        dsmesa
037700011128     c                   MOVEL     dsgioe        dsgioa
037800011128     c                   MOVE      dsdata        a1                             *ultimo carattere
037900011128     c                   SETOFF                                       98
038000011128     c                   TESTN                   dsdata               98
038100011128if  1c                   IF        *in98  AND                                   *da TESTN è numero
038200011128     c                             a1 >= '0'                                    *NON trovata lettera
038300011128     c                   MOVEL     dsanna        dsann
038400011128     c                   MOVEL     dsmesa        dsmes
038500011128     c                   MOVEL     dsgioa        dsgio
038600011128     c                   Z-ADD     dsdat         g08inv
038700011128     c                   Z-ADD     *zeros        g08dat
038800011128     C                   MOVEL     '3'           g08err
038900011128     c                   CALL      'XSRDA8'
039000011128     c                   PARM                    wlbda8
039100011128if  2c                   IF        g08err <> '1'                                *no errore
039200011128     c                   Z-ADD     g08inv        pardaf
039300011128e   2c                   ENDIF
039400011128e   1c                   ENDIF
039500011128     c*
039600011128     c* data inizio, se nessuna richiesta = data corrente - 1 anno
039700011128     c                   Z-ADD     datcor        dsdat
039800011128     c                   EVAL      dsann = dsann - 1
039900011128     c                   Z-ADD     dsdat         pardai                         *nessuna richiesta
040000011128     c*
040100011128     c* controlla se la data inizio è numerica
040200011128     c                   MOVEL     Din           dsdate
040300011128     c                   MOVEL     dsanne        dsanna
040400011128     c                   MOVEL     dsmese        dsmesa
040500011128     c                   MOVEL     dsgioe        dsgioa
040600011128     c                   MOVE      dsdata        a1                             *ultimo carattere
040700011128     c                   SETOFF                                       98
040800011128     c                   TESTN                   dsdata               98
040900011128if  1c                   IF        *in98  AND                                   *da TESTN è numero
041000011128     c                             a1 >= '0'                                    *NON trovata lettera
041100011128     c                   MOVEL     dsanna        dsann
041200011128     c                   MOVEL     dsmesa        dsmes
041300011128     c                   MOVEL     dsgioa        dsgio
041400011128     c                   Z-ADD     dsdat         g08inv
041500011128     c                   Z-ADD     *zeros        g08dat
041600011128     C                   MOVEL     '3'           g08err
041700011128     c                   CALL      'XSRDA8'
041800011128     c                   PARM                    wlbda8
041900011128if  2c                   IF        g08err <> '1'                                *no errore
042000011128     c                   Z-ADD     g08inv        pardai
042100011128e   2c                   ENDIF
042200011128e   1c                   ENDIF
042300001003     C*
042400001003     C* pulisce le variabili di lavoro
042500001003     C                   CLEAR                   ndst
042600001003     C                   CLEAR                   nmit
042700001003     C                   CLEAR                   nprd
042800001003     C                   CLEAR                   nlod
042900001003     C*
043000001003     C* destinatario
043100001003     C     minu:maiu     XLATE     Dst           ndst                           *Minus -> Maiuscolo
043200001003     C*
043300001003     C* mittente
043400001003     C     minu:maiu     XLATE     Mit           nmit                           *Minus -> Maiuscolo
043500001003     C*
043600001003     C* provincia di arrivo
043700001003     C     minu:maiu     XLATE     Prd           nprd                           *Minus -> Maiuscolo
043800001003     C*
043900001003     C* località di arrivo
044000001003     C     minu:maiu     XLATE     Lod           nlod                           *Minus -> Maiuscolo
044100001003     C*
044200001003     C* data fine + data inizio, deve esserci congruenza
044300001003if  2C                   if        pardai > pardaf                              *incongruenza
044400001003     C                   z-add     pardaf        pardai                         *data fine = inizio
044500001003e   1C                   endif
044600001003     C*
044700001003     C                   ENDSR
044800001003     C*--------------------------------------------------------------------------------------------*
044900001005     C* ESEGUO CHECK X VERIFICARE SE CONSIDERARE O MENO IL RECORD CORRENTE  SU TIVTA
045000001003     C*--------------------------------------------------------------------------------------------*
045100001003     C     chkvta        BEGSR
045200010926     C*
045300001003     C                   movel     '0'           errl00            1
045400001003     C*
045500011026     C* Controllo se richiesto un particolare P.O. di arrivo (se 999 = Tutti i PO di arrivo)
045600001003     C                   if        errl00 = '0'
045700011026     C                   if        lna > *zeros AND
045800011026     c                             lna <> 999
045900001003     C                   if        vtapoa <> lna
046000001003     C                   movel     '1'           errl00
046100001003     C                   endif
046200001003     C                   endif
046300001003     C                   endif
046400001003     C*
046500001003     C* Controllo se richiesta una particolare provincia di arrivo
046600001003     C                   if        errl00 = '0'
046700001003     C                   if        %trim(nprd) > *blanks
046800001003     C                   if        %trim(vtaprd) <> %trim(nprd)
046900001003     C                   movel     '1'           errl00
047000001003     C                   endif
047100001003     C                   endif
047200001003     C                   endif
047300001003     C*
047400001003     C* Controllo se richiesta un particolare destinatario
047500001003     C                   if        errl00 = '0'
047600001003     C                   if        %trim(ndst) > *blanks
047700011213     C                   eval      a35 = %trim(vtarsd)
047800011213     C                   eval      pos = %scan(%trim(ndst):a35)
047900001005     C                   if        pos <= *zeros
048000001003     C                   movel     '1'           errl00
048100001003     C                   endif
048200001003     C                   endif
048300001003     C                   endif
048400001005     C*
048500001005     C* Controllo se richiesta una particolare località di arrivo
048600001005     C                   if        errl00 = '0'
048700001005     C                   if        %trim(nlod) > *blanks
048800011213     C                   eval      a35 = %trim(vtalna)
048900011213     C                   eval      pos = %scan(%trim(nlod):a35)
049000001005     C                   if        pos <= *zeros
049100001005     C                   movel     '1'           errl00
049200001005     C                   endif
049300001005     C                   endif
049400001005     C                   endif
049500001003     C*
049600001003     C* Controllo se richiesta un particolare mittente
049700001003     C                   if        errl00 = '0'
049800001003     C                   if        %trim(nmit) > *blanks
049900011213     C                   eval      a35 = %trim(vtarsm)
050000011213     C                   eval      pos = %scan(%trim(nmit):a35)
050100001005     C                   if        pos <= *zeros
050200001003     C                   movel     '1'           errl00
050300001003     C                   endif
050400001003     C                   endif
050500001003     C                   endif
050600011026     C*
050700011029     C* legge e controlla la bolla
050800011026     C                   if        errl00 = '0'
050900011026     C                   z-add     vtadsp        dsdat
051000011026     C                   z-add     dsann         ktaaas
051100011026     C                   z-add     vtalnp        ktalnp
051200011026     C                   z-add     vtanrs        ktanrs
051300011026     C                   z-add     vtansp        ktansp
051400011026     C                   movel     vtatbl        ktatbl
051500011026     C     keytas        chain     titas30c
051600011026     C                   if        %found
051700011026     C                   exsr      chktas
051800011026     C                   endif
051900011026     C                   endif
052000001003     C*
052100001003     C                   ENDSR
052200001005     C*--------------------------------------------------------------------------------------------*
052300001005     C* ESEGUO CHECK X VERIFICARE SE CONSIDERARE O MENO IL RECORD CORRENTE SU TITAS
052400001005     C*--------------------------------------------------------------------------------------------*
052500001005     C     chktas        BEGSR
052600010926     c*
052700050210     c* Reimposta le variabili di lavoro
052800011026     C                   movel     '0'           errl00
052900010926     c                   EVAL      $statoOK = 'N'                               *stato NON ok
053000011005     c                   EVAL      $notgia  = 'N'                               *note "giacenza"
053100001005     C*
053200011128     C* Consegnate - NORMALI
053300001005     C                   if        errl00 = '0'
053400011128     C                   if        tasdcm > *zeros AND
053500080606     C                             tascca  = *blanks                            *SENZA anomalie
053600010926     C                   if        ckc <> *blanks
053700080606     C                   EVAL      $statoOK = 'S'                               *stato OK
053800001005     C                   endif
053900010926     C                   endif
054000001005     C                   endif
054100080606     C*
054200080606     C* Consegnate - ANOMALE
054300011128     C                   if        errl00 = '0'
054400011128     C                   if        tasdcm > *zeros AND
054500011128     c                             tascca  <> *blanks                           *CON   anomalie
054600011128     C                   if        ckm <> *blanks
054700080606     C                   EVAL      $statoOK = 'S'                               *stato OK
054800011128     C                   endif
054900011128     C                   endif
055000011128     C                   endif
055100080606     C*
055200021108     c* reperisce evenutale GIACENZA
055300021108     c                   EXSR      repgcp
055400011026     c*
055500011005     c* reperisce motivi della NON consegna per GIACENZA
055600011005     c                   Z-ADD     tasaas        ka4aas
055700011005     c                   Z-ADD     taslnp        ka4lnp
055800011005     c                   Z-ADD     tasnrs        ka4nrs
055900011005     c                   Z-ADD     tasnsp        ka4nsp
056000011005     c     ke2ta4        SETLL     tita430c
056100050210     c     ke2ta4        READE     tita430c                               98
056200050210     c                   DOW       NOT *in98
056300050210     c                   IF        ta4trc = 'G' OR                              *Giacenza
056400050210     c                             ta4trc = 'H'
056500050210     c                   EVAL      $notgia = 'S'                                *stato GIACIENTE
056600050210     c                   ENDIF
056700050210     c     ke2ta4        READE     tita430c                               98
056800050210     c                   ENDDO
056900050210     C*
057000050210     C* Non consenate - Giacenti
057100050210     C                   if        errl00 = '0'
057200050210     C                   IF        tasdcm = *zeros   AND
057300050210     C                             (tasfgc<>*blanks OR $notgia='S' OR $gcp='S')
057400050210     C                   if        ckg <> *blanks
057500010926     c                   EVAL      $statoOK = 'S'                               *stato OK
057600001005     C                   endif
057700010926     C                   endif
057800001005     C                   endif
057900040427     C*
058000040427     C* Non consenate  - "altre"
058100040427     C* - Altre
058200040427     C                   if        errl00 = '0'
058300050210     C                   IF        tasdcm = *zeros AND
058400040427     C                             (tasfgc=*blank AND $notgia='N' AND $gcp='N')
058500050210     C*
058600050210     C* reperisce evenutale CONSEGNA PARZIALE
058700050210     C                   EXSR      repparz
058800050210     C                   if        ckp <> *blanks
058900050210     C                   EVAL      $statoOK = 'S'                               *stato OK
059000050210     C                   endif
059100040427     C*
059200050210     C* verifica evenutale APPUNTAMENTO
059300050310     C                   if        tasftc = 'A' OR tastc2 = 'A'
059400050310     C                   if        ckt <> *blanks
059500040427     C                   EVAL      $statoOK = 'S'                               *stato OK
059600050310     C                   endif
059700040427     C                   endif
059800050210     C*
059900050210     C* reperisce evenutale LASCIATO AVVISO
060000050210     C                   EXSR      replav
060100050310     C                   if        $lav='S'
060200050310     C                   if        ckl <> *blanks
060300050210     C                   EVAL      $statoOK = 'S'                               *stato OK
060400050310     C                   endif
060500050210     C                   endif
060600050309     C*
060700050309     C* se scelte ALTRE record cmq sempre da considerare
060800050309     C                   if        cka <> *blanks
060900050309     C                   EVAL      $statoOK = 'S'                               *stato OK
061000050309     C                   endif
061100050210     C*
061200050210     C                   endif
061300050210     C                   endif
061400080606     C*
061500080606     C* Tipo servizio
061600080606     C                   if        errl00 = '0'
061700080606     C                   if        (SERVIZIO_C <> *blanks     AND
061800080606     C                              tasTSP      = SERVIZIO_C)  OR
061900080606     C                             (SERVIZIO_D <> *blanks     AND
062000080606     C                              tasTSP      = SERVIZIO_D)  OR
062100080606     C                             (SERVIZIO_E <> *blanks     AND
062200091113     C                              tasTSP      = SERVIZIO_E)  OR
062300091113     C                             (SERVIZIO_H <> *blanks     AND
062400091113     C                              tasTSP      = SERVIZIO_H)
062500080606     C                   else
062600080606     C                   movel     '1'           errl00
062700080606     C                   endif
062800080606     C                   endif
062900010926     c*
063000010926     C                   if        errl00 = '0'
063100010926     c                   if        $statoOK <> 'S'
063200010926     C                   movel     '1'           errl00
063300010926     C                   endif
063400010926     C                   endif
063500001005     C*
063600001005     C                   ENDSR
063700021108     c*--------------------------------------------------------------------------------------------*
063800021108     c* reperisce la giacenza
063900021108     c*--------------------------------------------------------------------------------------------*
064000021108     c     repgcp        BEGSR
064100021108     c*
064200021108     c* imposta le variabili di lavoro
064300050321     c                   EVAL      $gcp = 'N'                                   NO giacenza in TIGCP
064400021108     c*
064500050210     c* legge la giacenza
064600021108     c                   Z-ADD     tasaas        kcpaas
064700021108     c                   Z-ADD     taslnp        kcplnp
064800021108     c                   Z-ADD     tasnrs        kcpnrs
064900021108     c                   Z-ADD     tasnsp        kcpnsp
065000021108     c                   Z-ADD     *zeros        kcpfrg
065100050321     c     keygcp        CHAIN     tigcp51l                           99
065200050210     c                   IF        NOT *in99        AND                         *esistente
065300021108     c                             gcpatb = *blanks AND                         *no annullata
065400021108     c                             gcptcm <> 'Z'
065500021108     c                   MOVEL     'S'           $gcp
065600050210     c                   ENDIF
065700021108     c*
065800021108     c                   ENDSR
065900040427     c*--------------------------------------------------------------------------------------------*
066000040427     c* reperisce se ultimo evento è di tipo lasciato avviso
066100040427     c*--------------------------------------------------------------------------------------------*
066200050210     c     replav        BEGSR
066300040427     c*
066400040427     c* imposta le variabili di lavoro
066500040427     c                   EVAL      $lav = 'N'                                   NO evento d tipo L/A
066600040427     c*
066700050210     c* legge gli eventi
066800040427     c                   Z-ADD     tasaas        kevaas
066900040427     c                   Z-ADD     taslnp        kevlnp
067000040427     c                   Z-ADD     tasnrs        kevnrs
067100040427     c                   Z-ADD     tasnsp        kevnsp
067200050210     c     keyevb        SETLL     fnevb04l                               99
067300050210     c                   IF        %equal(fnevb04l)
067400050210     c     keyevb        READE     fnevb04l
067500050210     c                   DOW       not %eof(fnevb04l)
067600040427     c                   IF        evbatb = *blanks                             *no annullata
067700040427     c     evbCEV        LOOKUP    sk2Alav                                20
067800040427     c                   IF        *IN20 = *ON
067900040427     c                   EVAL      $lav = 'S'                                   evento d tipo L/A
068000040427     c                   ELSE
068100040427     c                   EVAL      $lav = 'N'                                   NO evento d tipo L/A
068200040427     c                   ENDIF
068300040427     c                   ENDIF
068400050210     c     keyevb        READE     fnevb04l
068500040427     c                   ENDDO
068600040427     c                   ENDIF
068700040427     c*
068800040427     c                   ENDSR
068900050210     c*--------------------------------------------------------------------------------------------*
069000050210     c* reperisce se la spedizone ha/avuto un evento d consegna parziale
069100050210     c*--------------------------------------------------------------------------------------------*
069200050210     c     repparz       BEGSR
069300050210     c*
069400050210     c* imposta le variabili di lavoro
069500050210     c                   EVAL      $parz = 'N'                                  NO evento Cons.Parz.
069600050210     c*
069700050210     c* legge la giacenza
069800050210     c                   Z-ADD     tasaas        kevaas
069900050210     c                   Z-ADD     taslnp        kevlnp
070000050210     c                   Z-ADD     tasnrs        kevnrs
070100050210     c                   Z-ADD     tasnsp        kevnsp
070200050210     c                   MOVEL(P)  'P'           kevcev
070300050210     c     keyevb2       SETLL     fnevb04l                               99
070400050210     c                   IF        %equal(fnevb04l)
070500050210     c     keyevb2       READE     fnevb04l
070600050210     c                   DOW       not %eof(fnevb04l)
070700050210     c                   IF        evbatb = *blanks                             *no annullata
070800050210     c                   EVAL      $parz= 'S'                                   evento d cons.parz.
070900050210     c                   LEAVE
071000050210     c                   ENDIF
071100050210     c     keyevb2       READE     fnevb04l
071200050210     c                   ENDDO
071300050210     c                   ENDIF
071400050210     c*
071500050210     c                   ENDSR
071600060221     C*--------------------------------------------------------------------------------------------*
071700060221     C* Operazioni per nuova area di arrivo
071800060221     C*--------------------------------------------------------------------------------------------*
071900060221     C     neware        BEGSR
072000060221     C*
072100060221     C* memorizza il nuovo punto operativo di arrivo in un deposito
072200060221     c                   z-add     S_orgcar      depare
072300060221     C*
072400060221     C* reimposta le variabili di lavoro
072500060221     C                   eval      $mpoa = 'N'                                  *NO dati memorizzati
072600060221     c                   clear                   ATOTCON           7 0          *dati memorizzazione
072700060221     c                   clear                   ACONANT           7 0
072800060221     c                   clear                   ACON00            7 0
072900060221     c                   clear                   ACON12            7 0
073000060221     c                   clear                   ACON24            7 0
073100060221     c                   clear                   ACON48            7 0
073200060221     c                   clear                   ACON72            7 0
073300060221     c                   clear                   ACON96            7 0
073400060221     c                   clear                   ACON120           7 0
073500060221     c                   clear                   ATMPMED           5 2
073600060221     c                   clear                   ATOTGICH          7 0
073700060221     c                   clear                   ATOTRESI          7 0
073800060221     c                   clear                   ATOTANO           7 0
073900060221     c                   clear                   ATOTGIA           7 0
074000060221     c                   clear                   ATOTPAR           7 0
074100060221     c                   clear                   ATOTAPP           7 0
074200060221     c                   clear                   ATOTLAV           7 0
074300060221     c                   clear                   ATOTALT           7 0
074400060221     c                   clear                   AORETOT          20 0
074500060221     c*
074600060221     c                   clear                   APERANT           5 2
074700060221     c                   clear                   APER00            5 2
074800060221     c                   clear                   APER12            5 2
074900060221     c                   clear                   APER24            5 2
075000060221     c                   clear                   APER48            5 2
075100060221     c                   clear                   APER72            5 2
075200060221     c                   clear                   APER96            5 2
075300060221     c                   clear                   APER120           5 2
075400060221     C*
075500060221     C                   ENDSR
075600011026     C*--------------------------------------------------------------------------------------------*
075700011026     C* Operazioni per nuovo punto operativo di arrivo
075800011026     C*--------------------------------------------------------------------------------------------*
075900011026     C     newpoa        BEGSR
076000011026     C*
076100011026     C* memorizza il nuovo punto operativo di arrivo in un deposito
076200011026     c                   z-add     vtapoa        deppoa
076300011026     C*
076400011026     C* reimposta le variabili di lavoro
076500011026     C                   eval      $mpoa = 'N'                                  *NO dati memorizzati
076600011029     c                   clear                   TOTCON            7 0          *dati memorizzazione
076700011026     c                   clear                   CONANT            7 0
076800011026     c                   clear                   CON00             7 0
076900011026     c                   clear                   CON12             7 0
077000011026     c                   clear                   CON24             7 0
077100011026     c                   clear                   CON48             7 0
077200011026     c                   clear                   CON72             7 0
077300011026     c                   clear                   CON96             7 0
077400011026     c                   clear                   CON120            7 0
077500011026     c                   clear                   TMPMED            5 2
077600050214     c                   clear                   TOTGICH           7 0
077700050214     c                   clear                   TOTRESI           7 0
077800011026     c                   clear                   TOTANO            7 0
077900011026     c                   clear                   TOTGIA            7 0
078000050210     c                   clear                   TOTPAR            7 0
078100040427     c                   clear                   TOTAPP            7 0
078200040427     c                   clear                   TOTLAV            7 0
078300011026     c                   clear                   TOTALT            7 0
078400011029     c                   clear                   ORETOT           20 0
078500011026     c*
078600011029     c                   clear                   PERANT            5 2
078700011026     c                   clear                   PER00             5 2
078800011026     c                   clear                   PER12             5 2
078900011026     c                   clear                   PER24             5 2
079000011026     c                   clear                   PER48             5 2
079100011026     c                   clear                   PER72             5 2
079200011026     c                   clear                   PER96             5 2
079300011026     c                   clear                   PER120            5 2
079400011026     C*
079500011026     C                   ENDSR
079600001003     C*--------------------------------------------------------------------------------------------*
079700011026     C* Memorizza i dati della bolla
079800001003     C*--------------------------------------------------------------------------------------------*
079900011026     C     membol        BEGSR
080000050214     C*
080100050214     C* imposta il passaggio di memorizzazione dati
080200011029     C                   eval      $mpoa = 'S'                                  *SI per PO arrivo
080300011029     C                   eval      $mgen = 'S'                                  *SI per Generale
080400050214     C                   CLEAR                   $stato
080500011029     C*---
080600011029     C* Consegnate
080700011029     C*---
080800001027     C                   if        tasdcm > *zeros
080900050214     C* Consegnate ma che hanno avuto giacenza
081000050214     C                   if        (tasfgc<>*blank OR $notgia='S' OR $gcp='S')
081100050214     C                   add       1             TOTGICH                        *PO di arrivo
081200060221     C                   add       1             ATOTGICH                       *AREA di arrivo
081300050214     C                   add       1             GTOTGICH                       *Generale
081400050214     C                   EVAL      $stato = %trim($stato) + 'GC'                  *stato CONS.+GIAC.
081500050214     C                   endif
081600050214     C* SENZA anomalia
081700011128     C                   if        tascca = *blanks
081800011029     C                   add       1             TOTCON                         *PO di arrivo
081900060221     C                   add       1             ATOTCON                        *AREA di arrivo
082000011029     C                   add       1             GTOTCON                        *Generale
082100050214     C                   EVAL      $stato = %trim($stato) + 'C'                 *stato CONSEGNATA
082200001027     C                   if        tasnrc < *zeros
082300011029     C                   add       1             CONANT                         *PO di arrivo
082400060221     C                   add       1             ACONANT                        *AREA di arrivo
082500011029     C                   add       1             GCONANT                        *Generale
082600011029     C                   eval      ORETOT = oretot + tasnrc
082700060221     C                   eval      AORETOT = aoretot + tasnrc
082800011029     C                   eval      GORETOT = goretot + tasnrc
082900001005     C                   endif
083000001027     C                   if        tasnrc = *zeros
083100011029     C                   add       1             CON00                          *PO di arrivo
083200060221     C                   add       1             ACON00                         *AREA di arrivo
083300011029     C                   add       1             GCON00                         *Generale
083400001005     C                   endif
083500001027     C                   if        tasnrc > 0 and tasnrc < 24
083600011029     C                   add       1             CON12                          *PO di arrivo
083700060221     C                   add       1             ACON12                         *AREA di arrivo
083800011029     C                   add       1             GCON12                         *Generale
083900011029     C                   eval      ORETOT = oretot + tasnrc
084000060221     C                   eval      AORETOT = aoretot + tasnrc
084100011029     C                   eval      GORETOT = goretot + tasnrc
084200001005     C                   endif
084300001027     C                   if        tasnrc >= 24 and tasnrc < 48
084400011029     C                   add       1             CON24                          *PO di arrivo
084500060221     C                   add       1             ACON24                         *AREA di arrivo
084600011029     C                   add       1             GCON24                         *Generale
084700011029     C                   eval      ORETOT = oretot + tasnrc
084800060221     C                   eval      AORETOT = aoretot + tasnrc
084900011029     C                   eval      GORETOT = goretot + tasnrc
085000001005     C                   endif
085100001027     C                   if        tasnrc >= 48 and tasnrc < 72
085200011029     C                   add       1             CON48                          *PO di arrivo
085300060221     C                   add       1             ACON48                         *AREA di arrivo
085400011029     C                   add       1             GCON48                         *Generale
085500011029     C                   eval      ORETOT = oretot + tasnrc
085600060221     C                   eval      AORETOT = aoretot + tasnrc
085700011029     C                   eval      GORETOT = goretot + tasnrc
085800001005     C                   endif
085900001027     C                   if        tasnrc >= 72 and tasnrc < 96
086000011029     C                   add       1             CON72                          *PO di arrivo
086100060221     C                   add       1             ACON72                         *AREA di arrivo
086200011029     C                   add       1             GCON72                         *Generale
086300011029     C                   eval      ORETOT = oretot + tasnrc
086400060221     C                   eval      AORETOT = aoretot + tasnrc
086500011029     C                   eval      GORETOT = goretot + tasnrc
086600001005     C                   endif
086700001027     C                   if        tasnrc >= 96 and tasnrc < 120
086800011029     C                   add       1             CON96                          *PO di arrivo
086900060221     C                   add       1             ACON96                         *AREA di arrivo
087000011029     C                   add       1             GCON96                         *Generale
087100011029     C                   eval      ORETOT = oretot + tasnrc
087200060221     C                   eval      AORETOT = aoretot + tasnrc
087300011029     C                   eval      GORETOT = goretot + tasnrc
087400001005     C                   endif
087500001027     C                   if        tasnrc >= 120
087600011029     C                   add       1             CON120                         *PO di arrivo
087700060221     C                   add       1             ACON120                        *AREA di arrivo
087800011029     C                   add       1             GCON120                        *Generale
087900011029     C                   eval      ORETOT = oretot + tasnrc
088000060221     C                   eval      AORETOT = aoretot + tasnrc
088100011029     C                   eval      GORETOT = goretot + tasnrc
088200001005     C                   endif
088300050214     C* CON anomalia
088400001027     C                   else
088500050214     C* CON anomalia -> RESI
088600050214     C                   if        tascca = '2'
088700050214     C                   add       1             TOTRESI                        *PO di arrivo
088800060221     C                   add       1             ATOTRESI                       *AREA di arrivo
088900050214     C                   add       1             GTOTRESI                       *Generale
089000050214     C                   EVAL      $stato = %trim($stato) + 'R'                 *stato RESO
089100050214     C* CON anomalia -> altre anomalie
089200050214     C                   else
089300011029     C                   add       1             TOTANO                         *PO di arrivo
089400060221     C                   add       1             ATOTANO                        *AREA di arrivo
089500011029     C                   add       1             GTOTANO                        *Generale
089600050214     C                   EVAL      $stato = %trim($stato) + 'M'                 *stato CONS ANOMALA
089700001027     C                   endif
089800050214     C                   endif
089900011029     C*---
090000001005     C* Non consegnate
090100011029     C*---
090200001005     C                   else
090300021108     C                   if        (tasfgc=*blank AND $notgia='N' AND $gcp='N')
090400050210     C* - Consegna parziale
090500050310     C                   if        $parz = 'S'
090600050310     C                   if        ckp <> *blanks
090700050210     C                   add       1             TOTPAR                         *PO di arrivo
090800060221     C                   add       1             ATOTPAR                        *AREA di arrivo
090900050210     C                   add       1             GTOTPAR                        *Generale
091000050214     C                   EVAL      $stato = %trim($stato) + 'P'                 *stato CONS.PARZ.
091100050310     C                   endif
091200050210     C                   else
091300040427     C* - Appuntamento
091400050310     C                   if        tasftc = 'A' OR tastc2 = 'A'
091500050310     C                   if        ckt <> *blanks
091600040427     C                   add       1             TOTAPP                         *PO di arrivo
091700060221     C                   add       1             ATOTAPP                        *AREA di arrivo
091800040427     C                   add       1             GTOTAPP                        *Generale
091900050214     C                   EVAL      $stato = %trim($stato) + 'T'                 *stato APPUNTAMENTO
092000050310     C                   endif
092100040427     C                   else
092200040427     C* - Lasciato avviso
092300050310     C                   if        $lav = 'S'
092400050310     C                   if        ckl <> *blanks
092500040427     C                   add       1             TOTLAV                         *PO di arrivo
092600060221     C                   add       1             ATOTLAV                        *AREA di arrivo
092700040427     C                   add       1             GTOTLAV                        *Generale
092800050214     C                   EVAL      $stato = %trim($stato) + 'V'                 *stato LASC./AVV.
092900050310     C                   endif
093000040427     C                   else
093100040427     C* - Altre
093200050211     C                   if        cka <> *blanks
093300011029     C                   add       1             TOTALT                         *PO di arrivo
093400060221     C                   add       1             ATOTALT                        *AREA di arrivo
093500011029     C                   add       1             GTOTALT                        *Generale
093600050214     C                   EVAL      $stato = %trim($stato) + 'A'                 *stato ALTRE
093700040427     C                   endif
093800050211     C                   endif
093900040427     C                   endif
094000050210     C                   endif
094100001005     C* Giacenti
094200001005     C                   else
094300050211     C                   if        ckg <> *blanks
094400011029     C                   add       1             TOTGIA                         *PO di arrivo
094500060221     C                   add       1             ATOTGIA                        *AREA di arrivo
094600011029     C                   add       1             GTOTGIA                        *Generale
094700050214     C                   EVAL      $stato = %trim($stato) + 'G'                 *stato GIACENTI
094800050211     C                   endif
094900001005     C                   endif
095000001005     C                   endif
095100011029     C*---
095200001005     C* Calcolo il tempo medio di ritardo
095300011029     C*---
095400011029     c* PO di arrivo
095500050310     C                   if        TOTCON = *zeros
095600001116     C                   eval      TMPMED = *zeros
095700001116     C                   else
095800050310     C                   eval      TMPMED = ORETOT / TOTCON
095900001116     C                   endif
096000060221     c* AREA di arrivo
096100060221     C                   if        ATOTCON = *zeros
096200060221     C                   eval      ATMPMED = *zeros
096300060221     C                   else
096400060221     C                   eval      ATMPMED = AORETOT / ATOTCON
096500060221     C                   endif
096600011029     c* Generale
096700050310     C                   if        GTOTCON = *zeros
096800011029     C                   eval      GTMPMED = *zeros
096900011029     C                   else
097000050310     C                   eval      GTMPMED = GORETOT / GTOTCON
097100011029     C                   endif
097200001003     C*
097300050214     C* Stampo il dettaglio bolle considerate con il relativo stato attribuito
097400050223     C***                except    DettBolla
097500050214     C*
097600001003     C                   ENDSR
097700001005     C*--------------------------------------------------------------------------------------------*
097800011029     C* EFFETTUA IL CALCOLO DELLE PERCENTUALI per PO di arrivo
097900001005     C*--------------------------------------------------------------------------------------------*
098000011029     C     perpoa        BEGSR
098100001005     C*
098200001005     C* Non "anomale"
098300001116     C                   eval      DIVIS = TOTCON/100
098400001116     C                   if        DIVIS <> *zeros
098500001006     C                   eval      PERANT = CONANT/DIVIS
098600001006     C                   eval      PER00  = CON00 /DIVIS
098700001006     C                   eval      PER12  = CON12 /DIVIS
098800001006     C                   eval      PER24  = CON24 /DIVIS
098900001006     C                   eval      PER48  = CON48 /DIVIS
099000001006     C                   eval      PER72  = CON72 /DIVIS
099100001006     C                   eval      PER96  = CON96 /DIVIS
099200001006     C                   eval      PER120 = CON120/DIVIS
099300001116     C                   endif
099400011026     C*
099500001005     C                   ENDSR
099600060221     C*--------------------------------------------------------------------------------------------*
099700060221     C* EFFETTUA IL CALCOLO DELLE PERCENTUALI per AREA di arrivo
099800060221     C*--------------------------------------------------------------------------------------------*
099900060221     C     perare        BEGSR
100000060221     C*
100100060221     C* Non "anomale"
100200060221     C                   eval      DIVIS = ATOTCON/100
100300060221     C                   if        DIVIS <> *zeros
100400060221     C                   eval      APERANT = ACONANT/DIVIS
100500060221     C                   eval      APER00  = ACON00 /DIVIS
100600060221     C                   eval      APER12  = ACON12 /DIVIS
100700060221     C                   eval      APER24  = ACON24 /DIVIS
100800060221     C                   eval      APER48  = ACON48 /DIVIS
100900060221     C                   eval      APER72  = ACON72 /DIVIS
101000060221     C                   eval      APER96  = ACON96 /DIVIS
101100060221     C                   eval      APER120 = ACON120/DIVIS
101200060221     C                   endif
101300060221     C*
101400060221     C                   ENDSR
101500011029     C*--------------------------------------------------------------------------------------------*
101600011029     C* EFFETTUA IL CALCOLO DELLE PERCENTUALI per Totale generale
101700011029     C*--------------------------------------------------------------------------------------------*
101800011029     C     pergen        BEGSR
101900011029     C*
102000011029     C* Non "anomale"
102100011029     C                   eval      DIVIS = GTOTCON/100
102200011029     C                   if        DIVIS <> *zeros
102300011029     C                   eval      GPERANT = GCONANT/DIVIS
102400011029     C                   eval      GPER00  = GCON00 /DIVIS
102500011029     C                   eval      GPER12  = GCON12 /DIVIS
102600011029     C                   eval      GPER24  = GCON24 /DIVIS
102700011029     C                   eval      GPER48  = GCON48 /DIVIS
102800011029     C                   eval      GPER72  = GCON72 /DIVIS
102900011029     C                   eval      GPER96  = GCON96 /DIVIS
103000011029     C                   eval      GPER120 = GCON120/DIVIS
103100011029     C                   endif
103200011029     C*
103300011029     C                   ENDSR
103400011029     C*--------------------------------------------------------------------------------------------*
103500011029     C* ESEGUE LE OPERAZIONI DI STAMPA della testata
103600011029     C*--------------------------------------------------------------------------------------------*
103700011029     C     states        BEGSR
103800011029     C*
103900011029     C                   write     s7w4te
104000011029     C*
104100011029     C                   ENDSR
104200011029     C*--------------------------------------------------------------------------------------------*
104300011029     C* ESEGUE LE OPERAZIONI DI STAMPA della riga finale
104400011029     C*--------------------------------------------------------------------------------------------*
104500011029     C     stafin        BEGSR
104600011029     C*
104700011029     C                   write     s7w4fi
104800011029     C*
104900011029     C                   ENDSR
105000001005     C*--------------------------------------------------------------------------------------------*
105100011029     C* ESEGUE LE OPERAZIONI DI STAMPA per PO di arrivo
105200001005     C*--------------------------------------------------------------------------------------------*
105300011029     C     stapoa        BEGSR
105400001005     C*
105500011029     c* decodifica il PO di arrivo
105600011029     c                   Z-ADD     deppoa        ppoa                           *codice
105700011029     C                   movel     *blanks       ppoad                          *descrizione
105800011029     c                   SETOFF                                           99
105900011029     c                   Z-ADD     1             i
106000011029     c     deppoa        LOOKUP    sorg(i)                                99
106100011029if  1c                   IF        *in99                                        *trovato
106200011029     C                   MOVEL     sdorg(i)      ppoad
106300011029e   1c                   ENDIF
106400011029     C*
106500011029     C* porta i campi memorizzati in stampa
106600011029     c                   Z-ADD     TOTCON        PTOTCON                        *valori numerici
106700011029     c                   Z-ADD     CONANT        PCONANT
106800011029     c                   Z-ADD     CON00         PCON00
106900011029     c                   Z-ADD     CON12         PCON12
107000011029     c                   Z-ADD     CON24         PCON24
107100011029     c                   Z-ADD     CON48         PCON48
107200011029     c                   Z-ADD     CON72         PCON72
107300011029     c                   Z-ADD     CON96         PCON96
107400011029     c                   Z-ADD     CON120        PCON120
107500011029     c                   Z-ADD     TMPMED        PTMPMED
107600050214     c                   Z-ADD     TOTGICH       PTOTGICH
107700050214     c                   Z-ADD     TOTRESI       PTOTRESI
107800011029     c                   Z-ADD     TOTANO        PTOTANO
107900011029     c                   Z-ADD     TOTGIA        PTOTGIA
108000050210     c                   Z-ADD     TOTPAR        PTOTPAR
108100040427     c                   Z-ADD     TOTAPP        PTOTAPP
108200040427     c                   Z-ADD     TOTLAV        PTOTLAV
108300011029     c                   Z-ADD     TOTALT        PTOTALT
108400011029     c                   Z-ADD     PERANT        PPERANT                        *valori percentuali
108500011029     c                   Z-ADD     PER00         PPER00
108600011029     c                   Z-ADD     PER12         PPER12
108700011029     c                   Z-ADD     PER24         PPER24
108800011029     c                   Z-ADD     PER48         PPER48
108900011029     c                   Z-ADD     PER72         PPER72
109000011029     c                   Z-ADD     PER96         PPER96
109100011029     c                   Z-ADD     PER120        PPER120
109200011029     C* stampa
109300011029     C                   write     s7w4de
109400001005     C*
109500001005     C                   ENDSR
109600060221     C*--------------------------------------------------------------------------------------------*
109700060221     C* ESEGUE LE OPERAZIONI DI STAMPA per AREA di arrivo
109800060221     C*--------------------------------------------------------------------------------------------*
109900060221     C     staare        BEGSR
110000060221     C*
110100060221     c* decodifica l'AREA di arrivo
110200060221     c                   Z-ADD     depare        ppoa                           *codice
110300060221     C                   movel     *blanks       ppoad                          *descrizione
110400060221     c                   SETOFF                                           99
110500060221     c                   Z-ADD     1             i
110600060221     c     depare        LOOKUP    sare(i)                                99
110700060221if  1c                   IF        *in99                                        *trovato
110800060221     C                   MOVEL     sdare(i)      ppoad
110900060221e   1c                   ENDIF
111000060221     C                   EVAL      ppoad = 'TOT ' + %trim(ppoad)
111100060221     C*
111200060221     C* porta i campi memorizzati in stampa
111300060221     c                   Z-ADD     ATOTCON       PTOTCON                        *valori numerici
111400060221     c                   Z-ADD     ACONANT       PCONANT
111500060221     c                   Z-ADD     ACON00        PCON00
111600060221     c                   Z-ADD     ACON12        PCON12
111700060221     c                   Z-ADD     ACON24        PCON24
111800060221     c                   Z-ADD     ACON48        PCON48
111900060221     c                   Z-ADD     ACON72        PCON72
112000060221     c                   Z-ADD     ACON96        PCON96
112100060221     c                   Z-ADD     ACON120       PCON120
112200060221     c                   Z-ADD     ATMPMED       PTMPMED
112300060221     c                   Z-ADD     ATOTGICH      PTOTGICH
112400060221     c                   Z-ADD     ATOTRESI      PTOTRESI
112500060221     c                   Z-ADD     ATOTANO       PTOTANO
112600060221     c                   Z-ADD     ATOTGIA       PTOTGIA
112700060221     c                   Z-ADD     ATOTPAR       PTOTPAR
112800060221     c                   Z-ADD     ATOTAPP       PTOTAPP
112900060221     c                   Z-ADD     ATOTLAV       PTOTLAV
113000060221     c                   Z-ADD     ATOTALT       PTOTALT
113100060221     c                   Z-ADD     APERANT       PPERANT                        *valori percentuali
113200060221     c                   Z-ADD     APER00        PPER00
113300060221     c                   Z-ADD     APER12        PPER12
113400060221     c                   Z-ADD     APER24        PPER24
113500060221     c                   Z-ADD     APER48        PPER48
113600060221     c                   Z-ADD     APER72        PPER72
113700060221     c                   Z-ADD     APER96        PPER96
113800060221     c                   Z-ADD     APER120       PPER120
113900060221     C* stampa
114000060221     C                   write     s7w4de
114100060221     C*
114200060221     C* stampa riga separazine
114300060221     C                   write     s7w4ri
114400060221     C*
114500060221     C                   ENDSR
114600011029     C*--------------------------------------------------------------------------------------------*
114700011029     C* ESEGUE LE OPERAZIONI DI STAMPA per Totale generale
114800011029     C*--------------------------------------------------------------------------------------------*
114900011029     C     stagen        BEGSR
115000011029     C*
115100011029     c* decodifica il Totale generale
115200011029     c                   Z-ADD     *zeros        ppoa                           *codice
115300011029     C                   EVAL      ppoad = 'TOTALE GENERALE'                    *descrizione
115400011029     C*
115500011029     C* porta i campi memorizzati in stampa
115600011029     c                   Z-ADD     GTOTCON       PTOTCON                        *valori numerici
115700011029     c                   Z-ADD     GCONANT       PCONANT
115800011029     c                   Z-ADD     GCON00        PCON00
115900011029     c                   Z-ADD     GCON12        PCON12
116000011029     c                   Z-ADD     GCON24        PCON24
116100011029     c                   Z-ADD     GCON48        PCON48
116200011029     c                   Z-ADD     GCON72        PCON72
116300011029     c                   Z-ADD     GCON96        PCON96
116400011029     c                   Z-ADD     GCON120       PCON120
116500011029     c                   Z-ADD     GTMPMED       PTMPMED
116600050214     c                   Z-ADD     GTOTGICH      PTOTGICH
116700050214     c                   Z-ADD     GTOTRESI      PTOTRESI
116800011029     c                   Z-ADD     GTOTANO       PTOTANO
116900011029     c                   Z-ADD     GTOTGIA       PTOTGIA
117000050210     c                   Z-ADD     GTOTPAR       PTOTPAR
117100040427     c                   Z-ADD     GTOTAPP       PTOTAPP
117200040427     c                   Z-ADD     GTOTLAV       PTOTLAV
117300011029     c                   Z-ADD     GTOTALT       PTOTALT
117400011029     c                   Z-ADD     GPERANT       PPERANT                        *valori percentuali
117500011029     c                   Z-ADD     GPER00        PPER00
117600011029     c                   Z-ADD     GPER12        PPER12
117700011029     c                   Z-ADD     GPER24        PPER24
117800011029     c                   Z-ADD     GPER48        PPER48
117900011029     c                   Z-ADD     GPER72        PPER72
118000011029     c                   Z-ADD     GPER96        PPER96
118100011029     c                   Z-ADD     GPER120       PPER120
118200060221     C*
118300060221     C* stampa riga separazine
118400060221     C                   write     s7w4ri
118500011029     C* stampa
118600011029     C                   write     s7w4de
118700060221     C*
118800060221     C* stampa riga separazine
118900060221     C                   write     s7w4ri
119000011029     C*
119100011029     C                   ENDSR
119200011029     C*--------------------------------------------------------------------------------------------*
119300011029     C* CARICA LE TABELLE OCCORRENTI
119400011029     C*--------------------------------------------------------------------------------------------*
119500011029     C     cartab        BEGSR
119600011029     c*---
119700011029     c* Organigramma
119800011029     c*---
119900011029     c                   CLEAR                   i                              *indice
120000011029     C                   CLEAR                   sorg
120100011029     C                   CLEAR                   sdorg
120200011029     c     *loval        SETLL     azorg01l
120300011029     c                   READ      azorg01l                               99
120400011029do  1c                   DOW       NOT *in99
120500011029if  2c                   IF        orgfva = *blanks AND
120600011029     c                             orgfag <> 'V'
120700011029     c                   ADD       1             i
120800011029     c                   Z-ADD     orgfil        sorg(i)
120900011029     c                   MOVEL     orgdes        sdorg(i)
121000011029e   2c                   ENDIF
121100011029     c                   READ      azorg01l                               99
121200011029e   1c                   ENDDO
121300060221     c*---
121400060221     c* Aree
121500060221     c*---
121600060221     c                   CLEAR                   i                              *indice
121700060221     C                   CLEAR                   sare
121800060221     C                   CLEAR                   sdare
121900060221     C                   Z-ADD     1             tute
122000060221     C                   MOVEL     '05'          tcod
122100060221     c     ke2tab        SETLL     tabel00f
122200060221     c     ke2tab        READE     tabel00f                               99
122300060221do  1c                   DOW       NOT *in99
122400060221if  2c                   IF        tblflg = *blanks
122500060221     c                   ADD       1             i
122600060221     c                   MOVEL     tblkey        sare(i)
122700060221     C                   MOVEL     tbluni        DS05
122800060221     c                   MOVEL     §05des        sdare(i)
122900060221e   2c                   ENDIF
123000060221     c     ke2tab        READE     tabel00f                               99
123100060221e   1c                   ENDDO
123200011029     c*---
123300011029     c* Provincie
123400011029     c*---
123500011029     c                   CLEAR                   i                              *indice
123600011029     C                   CLEAR                   sprv
123700011029     C                   CLEAR                   sdprv
123800011029     C                   Z-ADD     1             tute
123900011029     C                   MOVEL     'PR'          tcod
124000011029     c     ke2tab        SETLL     tabel00f
124100011029     c     ke2tab        READE     tabel00f                               99
124200011029do  1c                   DOW       NOT *in99
124300011029if  2c                   IF        tblflg = *blanks
124400011029     c                   ADD       1             i
124500011029     c                   MOVE      tblkey        sprv(i)
124600011029     C                   MOVEL     tbluni        DSPR
124700011029     c                   MOVEL     §prdes        sdprv(i)
124800011029e   2c                   ENDIF
124900011029     c     ke2tab        READE     tabel00f                               99
125000011029e   1c                   ENDDO
125100040427     c*---
125200040427     c* Eventi d tipo "lasciato avviso"
125300040427     c*---
125400040427     C                   clear                   sk2Alav
125500040427     C                   z-add     *zeros        j                 3 0
125600040427     C                   eval      tute = 1
125700040427     C                   eval      tcod = '2A'
125800040427     C     ke2tab        setll     tabel00f
125900040427     C     ke2tab        reade     tabel00f
126000040427     C                   dow       not %eof(tabel00f)
126100040427     C                   movel(p)  tblUNI        ds2a
126200040427     C                   if        §2aFTC = 'A'
126300040427     C                   add       1             j
126400040427     C                   eval      sk2Alav(j) = tblKEY
126500040427     C                   endif
126600040427     C     ke2tab        reade     tabel00f
126700040427     C                   enddo
126800011029     C*
126900011029     c                   ENDSR
127000001005     C*--------------------------------------------------------------------------------------------*
127100001006     C* IMPOSTA I CAMPI DI TESTATA IN STAMPA E VALORIZZO CAMPONE "USRDFNDTA" (per 100 byte)
127200001005     C*--------------------------------------------------------------------------------------------*
127300001207     C     imppar        BEGSR
127400001207     C*
127500001207     C* Pulisce il formato record di testata del PRTF
127600011026     C                   clear                   s7w4te
127700001005     C*
127800001005     C* Inverto le date in input
127900001005     C                   z-add     *zeros        G08DAT
128000001005     C                   z-add     PARDAI        G08INV
128100001005     C                   movel     '3'           G08ERR
128200001005     C                   call      'XSRDA8'
128300001005     C                   parm                    WLBDA8
128400001005     C                   z-add     G08DAT        STPDAI
128500001006     C                   eval      desdai = %trim(%editc(PARDAI:'Z'))
128600001005     C*
128700001005     C                   z-add     *zeros        G08DAT
128800001005     C                   z-add     PARDAF        G08INV
128900001005     C                   movel     '3'           G08ERR
129000001005     C                   call      'XSRDA8'
129100001005     C                   parm                    WLBDA8
129200001005     C                   z-add     G08DAT        STPDAF
129300001006     C                   eval      desdaf = %trim(%editc(PARDAF:'Z'))
129400001005     C*
129500001005     C* Reperisco la descrizione del Punto Operativo e valorizzo i due parametri ad esso riferiti
129600011026     c                   if        lna = 999
129700011026     c                   eval      STPLNAD = 'Tutti'
129800011026     c                   else
129900001030     C     lna           chain     azorg01l
130000001005     C                   if        %found(azorg01l)
130100001005     C                   movel     orgdes        STPLNAD
130200001006     C                   eval      despoa = %trim(%editc(lna:'Z'))
130300001005     C                   endif
130400011026     c                   endif
130500001005     C                   z-add     lna           STPLNA
130600011029     C*
130700011029     C* Reperisco la descrizione della provincia e valorizzo entrambi i parametri ad essa riferiti
130800011029     C                   movel     prd           STPPRD
130900011029     C                   movel     'PR'          tcod
131000011029     C                   movel(p)  prd           tkey
131100011029     C     keytab        chain     tabel00f
131200011029     C                   if        %found(tabel00f)
131300011029     C                   movel     tbluni        DSPR
131400011029     C                   movel     §PRDES        STPPRDD
131500011029     C                   eval      desprd = %trim(nprd)
131600011029     C                   endif
131700080606     C*
131800080606     C* Descrizione filtro x tipo servizio
131900080606     c                   if        SERVIZIO_C <> *blanks AND
132000080606     c                             SERVIZIO_D <> *blanks AND
132100091113     c                             SERVIZIO_E <> *blanks AND
132200091113     c                             SERVIZIO_H <> *blanks
132300080606     c                   eval      STPTSP = 'Tutti'
132400080606     c                   else
132500090609      * tipo servizio 'C'
132600080606     c                   if        SERVIZIO_C <> *blanks
132700090609     c                   clear                   ds5e
132800090609     c                   eval      tcod = '5E'
132900090609     c                   eval      tkey = 'C'
133000090609     c     keytab        chain     tabel00f
133100090609     c                   if        %found(tabel00f)
133200090609     c                   eval      ds5e = tbluni
133300090609     c                   endif
133400090609     c                   eval      STPTSP = %trim(STPTSP)+' '+ §5edes
133500080606     c                   endif
133600090609      * tipo servizio 'D'
133700080606     c                   if        SERVIZIO_D <> *blanks
133800090609     c                   clear                   ds5e
133900090609     c                   eval      tcod = '5E'
134000090609     c                   eval      tkey = 'D'
134100090609     c     keytab        chain     tabel00f
134200090609     c                   if        %found(tabel00f)
134300090609     c                   eval      ds5e = tbluni
134400090609     c                   endif
134500090609     c                   eval      STPTSP = %trim(STPTSP)+' '+ §5edes
134600080606     c                   endif
134700090609      * tipo servizio 'E'
134800080606     c                   if        SERVIZIO_E <> *blanks
134900090609     c                   clear                   ds5e
135000090609     c                   eval      tcod = '5E'
135100090609     c                   eval      tkey = 'E'
135200090609     c     keytab        chain     tabel00f
135300090609     c                   if        %found(tabel00f)
135400090609     c                   eval      ds5e = tbluni
135500090609     c                   endif
135600090609     c                   eval      STPTSP = %trim(STPTSP)+' '+ §5edes
135700080606     c                   endif
135800091113      * tipo servizio 'H'
135900091113     c                   if        SERVIZIO_H <> *blanks
136000091113     c                   clear                   ds5e
136100091113     c                   eval      tcod = '5E'
136200091113     c                   eval      tkey = 'H'
136300091113     c     keytab        chain     tabel00f
136400091113     c                   if        %found(tabel00f)
136500091113     c                   eval      ds5e = tbluni
136600091113     c                   endif
136700091113     c                   eval      STPTSP = %trim(STPTSP)+' '+ §5edes
136800091113     c                   endif
136900080606     c                   endif
137000001005     C*
137100001005     C* Quindi tutti i rimanenti parametri
137200001005     C                   movel     ksc           STPKSU
137300001005     C                   movel     ndst          STPDST
137400001006     C                   eval      desdst = %trim(ndst)
137500001005     C                   movel     nmit          STPMIT
137600001006     C                   eval      desmit = %trim(nmit)
137700001005     C                   movel     lod           STPLOD
137800001006     C                   eval      deslod = %trim(nlod)
137900001005     C                   if        ckc <> *blanks
138000001207     C                   movel     'S'           STPFCO
138100001006     C                   eval      desfl1 = 'CONS.'
138200001005     C                   endif
138300011128     C                   if        ckm <> *blanks
138400011128     C                   movel     'S'           STPFCM
138500011128     C                   eval      desfl1 = 'ANOM.'
138600011128     C                   endif
138700001005     C                   if        ckg <> *blanks
138800050210     C                   eval      STPFLG = %trim(STPFLG) + ' GI'
138900001006     C                   eval      desfl2 = 'GIAC.'
139000001005     C                   endif
139100050210     C                   if        ckp <> *blanks
139200050210     C                   eval      STPFLG = %trim(STPFLG) + ' CP'
139300050210     C                   eval      desfl3 = 'PARZ.'
139400050210     C                   endif
139500050210     C                   if        ckt <> *blanks
139600050210     C                   eval      STPFLG = %trim(STPFLG) + ' AP'
139700050210     C                   eval      desfl4 = 'APP.'
139800050210     C                   endif
139900050210     C                   if        ckl <> *blanks
140000050210     C                   eval      STPFLG = %trim(STPFLG) + ' LA'
140100050210     C                   eval      desfl5 = 'L.AVV'
140200050210     C                   endif
140300001005     C                   if        cka <> *blanks
140400050210     C                   eval      STPFLG = %trim(STPFLG) + ' AL'
140500050210     C                   eval      desfl6 = 'ALTRE'
140600001005     C                   endif
140700001006     C*
140800001006     C* A questo punto compongo la stringa da passare nell'USRDFNDTA e i dati di servizio
140900001006     C                   move      rqscid        sgiusrnbr
141000001006     C                   eval      tipstp = 'REC'
141100001006     C                   eval      tipsnd = 'SGI'
141200001006     C                   eval      email = *blanks
141300001006     C*
141400001009     C                   eval      varcmd=''''+dessta+' '+%trim(prmdat)+' '+
141500001009     C                                        %trim(prmora)
141600001006     C                   if        desdai>*blanks
141700001006     C                   eval      varcmd = %trim(varcmd)+%trim(desdai)+'+'
141800001006     C                   endif
141900001006     C                   if        desdaf>*blanks
142000001006     C                   eval      varcmd = %trim(varcmd)+%trim(desdaf)+'+'
142100001006     C                   endif
142200001006     C                   if        despoa>*blanks
142300001006     C                   eval      varcmd = %trim(varcmd)+%trim(despoa)+'+'
142400001006     C                   endif
142500001006     C                   if        desprd>*blanks
142600001006     C                   eval      varcmd = %trim(varcmd)+%trim(desprd)+'+'
142700001006     C                   endif
142800001006     C                   if        deslod>*blanks
142900001006     C                   eval      varcmd = %trim(varcmd)+%trim(deslod)+'+'
143000001006     C                   endif
143100001006     C                   if        desfl1>*blanks
143200001006     C                   eval      varcmd = %trim(varcmd)+%trim(desfl1)+'+'
143300001006     C                   endif
143400001006     C                   if        desfl2>*blanks
143500001006     C                   eval      varcmd = %trim(varcmd)+%trim(desfl2)+'+'
143600001006     C                   endif
143700001006     C                   if        desfl3>*blanks
143800001006     C                   eval      varcmd = %trim(varcmd)+%trim(desfl3)+'+'
143900001006     C                   endif
144000050210     C                   if        desfl4>*blanks
144100050210     C                   eval      varcmd = %trim(varcmd)+%trim(desfl4)+'+'
144200050210     C                   endif
144300050210     C                   if        desfl5>*blanks
144400050210     C                   eval      varcmd = %trim(varcmd)+%trim(desfl5)+'+'
144500050210     C                   endif
144600050210     C                   if        desfl6>*blanks
144700050210     C                   eval      varcmd = %trim(varcmd)+%trim(desfl6)+'+'
144800050210     C                   endif
144900001006     C                   if        desmit>*blanks
145000001006     C                   eval      varcmd = %trim(varcmd)+%trim(desmit)+'+'
145100001006     C                   endif
145200001006     C                   if        desdst>*blanks
145300001006     C                   eval      varcmd = %trim(varcmd)+%trim(desdst)
145400001006     C                   endif
145500001009     C                   if        %len(%trim(varcmd)) >=97
145600001009     C                   eval      %subst(var100:97:4) = '...'
145700001009     C                   else
145800001009     C                   eval      varcmd = %trim(varcmd)
145900001009     C                   endif
146000001009     C                   movel     varcmd        var100
146100050210     C                   move(p)   param         varsrv
146200001006     C                   eval      varcmd = %trim(CMD(1))+' '+%trim(CMD(2))+
146300001006     C                                      var100 + varsrv+''''+')'
146400001005     C*
146500001005     C                   ENDSR
146600001003     C*--------------------------------------------------------------------------------------------*
146700001003     C* impostazioni iniziali
146800011029     C*--------------------------------------------------------------------------------------------*
146900001003     C     *inzsr        BEGSR
147000001003     C*
147100001003     C* Ricevimento parametri
147200001003     C     *ENTRY        PLIST
147300060613     C                   PARM                    TIS1W2IDS
147400001005     C                   PARM                    prmdat
147500001005     C                   PARM                    prmora
147600001006     C*
147700001006     C* Aggiunge le librerie occorrenti in lista
147800060613     C*                  EVAL      wpgm = 'GAITRAOBJ/TIS777C'
147900060613     C*                  CALL      wpgm
148000001003     C*--------------------
148100001003     C* CALCOLA LA DATA CORRENTE
148200001003     C*--------------------
148300001003     C                   time                    WN14             14 0          *ORA (6)+ DATA (8)
148400001003     C                   move      WN14          WN8               8 0          *DATA (8) IN G/M/AA
148500001003     C                   z-add     WN8           G08DAT
148600001003     C                   z-add     *zeros        G08INV
148700001003     C                   movel     '0'           G08ERR
148800001003     C                   call      'XSRDA8'
148900001003     C                   parm                    WLBDA8
149000001003     C                   z-add     G08INV        DATCOR            8 0          *DATA CORRENTE AA/M/
149100001003     C*
149200001003     C* Chiavi di lettura
149300001003     C     keytas        KLIST                                                  *TITAS30C
149400001003     C                   KFLD                    ktaaas
149500001003     C                   KFLD                    ktalnp
149600001003     C                   KFLD                    ktanrs
149700001003     C                   KFLD                    ktansp
149800001003     C                   KFLD                    ktatbl
149900011005     c     ke2ta4        KLIST                                                  tita430c
150000011005     c                   KFLD                    ka4aas                         -anno spedizione
150100011005     c                   KFLD                    ka4lnp                         -linea partenza
150200011005     c                   KFLD                    ka4nrs                         -serie
150300011005     c                   KFLD                    ka4nsp                         -spedizione
150400050321     c     keygcp        KLIST                                                  *tigcp51l
150500021108     c                   KFLD                    kcpaas                         -anno spedizione
150600021108     c                   KFLD                    kcplnp                         -linea partenza
150700021108     c                   KFLD                    kcpnrs                         -serie
150800021108     c                   KFLD                    kcpnsp                         -spedizione
150900021108     c                   KFLD                    kcpfrg                         -prg aperture
151000050210     c     keyevb        KLIST                                                  *fnevb04l
151100040427     c                   KFLD                    kevaas                         -anno spedizione
151200040427     c                   KFLD                    kevlnp                         -linea partenza
151300040427     c                   KFLD                    kevnrs                         -serie
151400040427     c                   KFLD                    kevnsp                         -spedizione
151500050210     c     keyevb2       KLIST                                                  *fnevb04l
151600050210     c                   KFLD                    kevaas                         -anno spedizione
151700050210     c                   KFLD                    kevlnp                         -linea partenza
151800050210     c                   KFLD                    kevnrs                         -serie
151900050210     c                   KFLD                    kevnsp                         -spedizione
152000050210     c                   KFLD                    kevcev                         -codice evento
152100001005     C*
152200001005     C* Definizione chiavi - TABEL00F
152300001005     C     keytab        KLIST
152400001005     C                   KFLD                    tute
152500001005     C                   KFLD                    tcod
152600001005     C                   KFLD                    tkey
152700011029     C     ke2tab        KLIST
152800011029     C                   KFLD                    tute
152900011029     C                   KFLD                    tcod
153000060613     C*
153100001003     C                   ENDSR
153200011029     O*--------------------------------------------------------------------------------------------*
153300050214     OPRTF198   E            DettBolla   2
153400050214     O                                        +   1 'Chiave bolla:'
153500050214     O                       dskeybol         +   1
153600050214     O                                        +  10 'Stato bolla:'
153700050214     O                       $stato           +   1
153800001005** CMD - COMANDI CL
153900001006OVRPRTF OVRSCOPE(*CALLLVL) FILE(WURTH) TOFILE(TIS7W4P)
154000001006OUTQ(STRATEOBJ/SGIIN) USRDFNDTA(
154100001005DLTOVR FILE(WURTH) LVL(*)
154200001006
