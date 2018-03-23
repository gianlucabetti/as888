000100970826     h*--------------------------------------------------------------------------------------------*
000200970826     h* Aggiornamento archivio località da archivio località poste - Esegue
000300970826     h*--------------------------------------------------------------------------------------------*
000400970826     h DECEDIT('0,') DATEDIT(*DMY.)
000500970826     f*--------------------------------------------------------------------------------------------*
000600970826     f* Data base
000700970826     f*--------------------------------------------------------------------------------------------*
000800970826     fazloc00f  IF   E             DISK                                         *Località poste
000900120307     fazcpl01l  UF A E           K DISK                                         *Loc.BRT x cap
001000120307     fazcpl02l  IF   E           K DISK                                         *Loc.BRT x loc
001100970923     f                                     rename(azcpl000:azcpl002)
001200970828     fazcps01l  UF A E           K DISK                                         *Sinonimi localitài
001300970922     fazcpc01l  UF A E           K DISK                                         *Cappario
001400970923     ftabel00f  IF   E           K DISK                                         *Tabelle
001500970923     fprtf198   O    F  198        PRINTER OFLIND(*inof)                        *Printer file
001600970826     d*--------------------------------------------------------------------------------------------*
001700970826     d* Definitions
001800970826     d*--------------------------------------------------------------------------------------------*
001900970826     d*---
002000970826     d* Prefisso variabili --> § = passaggio routine esterne
002100970826     d*                    --> w = passaggio routine interne
002200970826     d*                    --> $ = di controllo
002300970826     d*---
002400970826     d*----------------
002500970826     d* schiere
002600970826     d*----------------
002700970826     d slin            S             66    DIM(10) CTDATA PERRCD(1)             *righe di stampa
002800970828     D sin             S             35    DIM(60)                              *sinonimi località
002900970922     D cap             S              1    DIM(9)                               *caratteri cap
003000970922     D caf             S              1    DIM(9)                               *caratteri cap fitt
003100970826     d*----------------
003200970826     d* Variabili riferite al data base
003300970826     d*----------------
003400970922     d kccver          S                   LIKE(cpcver)                         *chiave azcpc00f
003500970922     d kccnar          S                   LIKE(cpcnar)
003600970922     d kcccap          S                   LIKE(cpccap)
003700970826     d kclver          S                   LIKE(cplver)                         *chiave azcpl00f
003800970826     d kclnar          S                   LIKE(cplnar)
003900970826     d kclcap          S                   LIKE(cplcap)
004000970826     d kclloc          S                   LIKE(cplloc)
004100970828     d kcsver          S                   LIKE(cpsver)                         *chiave azcps00f
004200970828     d kcsnar          S                   LIKE(cpsnar)
004300970828     d kcscap          S                   LIKE(cpscap)
004400970828     d kcsloc          S                   LIKE(cpsloc)
004500970828     d kcssin          S                   LIKE(cpssin)
004600970922     d ktbkut          S                   LIKE(tblkut)                         *chiave tabel00f
004700970922     d ktbcod          S                   LIKE(tblcod)
004800970922     d ktbkey          S                   LIKE(tblkey)
004900970922     d staprv          S                   LIKE(locprv)
005000970922     d stacap          S                   LIKE(loccap)
005100970922     d staloc          S                   LIKE(locloc)
005200970922     d wcap            S                   LIKE(cpccap)
005300970924     d dlna            S                   LIKE(cpclna)
005400970924     d dznc            S                   LIKE(cpcznc)
005500970924     d diso            S                   LIKE(cpciso)
005600970924     d dzfd            S                   LIKE(cpczfd)
005700970924     d dlkg            S                   LIKE(cpclkg)
005800970924     d dlmc            S                   LIKE(cpclmc)
005900970924     d dlol            S                   LIKE(cpclol)
006000970924     d dzol            S                   LIKE(cpczol)
006100970924     d dlks            S                   LIKE(cpclks)
006200970924     d dlms            S                   LIKE(cpclms)
006300970924     d dlos            S                   LIKE(cpclos)
006400970924     d dzos            S                   LIKE(cpczos)
006500970924     d dlk2            S                   LIKE(cpclk2)
006600970924     d dlm2            S                   LIKE(cpclm2)
006700970924     d dlo2            S                   LIKE(cpclo2)
006800970924     d dzo2            S                   LIKE(cpczo2)
006900970924     d dcts            S                   LIKE(cpccts)
007000970924     d dcte            S                   LIKE(cpccte)
007100970924     d dttc            S                   LIKE(cpcttc)
007200970924     d dpii            S                   LIKE(cpcpii)
007300970924     d dpid            S                   LIKE(cpcpid)
007400970924     d dpis            S                   LIKE(cpcpis)
007500970924     d dgdd            S                   LIKE(cpcgdd)
007600970925     d plna            S                   LIKE(cpclna)
007700970925     d pznc            S                   LIKE(cpcznc)
007800970925     d piso            S                   LIKE(cpciso)
007900970925     d pzfd            S                   LIKE(cpczfd)
008000970925     d plkg            S                   LIKE(cpclkg)
008100970925     d plmc            S                   LIKE(cpclmc)
008200970925     d plol            S                   LIKE(cpclol)
008300970925     d pzol            S                   LIKE(cpczol)
008400970925     d plks            S                   LIKE(cpclks)
008500970925     d plms            S                   LIKE(cpclms)
008600970925     d plos            S                   LIKE(cpclos)
008700970925     d pzos            S                   LIKE(cpczos)
008800970925     d plk2            S                   LIKE(cpclk2)
008900970925     d plm2            S                   LIKE(cpclm2)
009000970925     d plo2            S                   LIKE(cpclo2)
009100970925     d pzo2            S                   LIKE(cpczo2)
009200970925     d pcts            S                   LIKE(cpccts)
009300970925     d pcte            S                   LIKE(cpccte)
009400970925     d pttc            S                   LIKE(cpcttc)
009500970925     d ppii            S                   LIKE(cpcpii)
009600970925     d ppid            S                   LIKE(cpcpid)
009700970925     d ppis            S                   LIKE(cpcpis)
009800970925     d pgdd            S                   LIKE(cpcgdd)
009900970826     d*----------------
010000970826     d* Variabili di controllo
010100970826     d*----------------
010200970826     d $fine           S              1    INZ('N')                             *fine programma
010300970826     d $err            S              1    INZ('N')                             *errore generico
010400970826     d*----------------
010500970826     d* Variabili a programma
010600970826     d*----------------
010700970826     d i               S              5  0                                      *indice
010800970826     d a5              S              5                                         *alfanumerico
010900970828     d locm35          S              1    INZ('N')                             *località > 35 char
011000970829     d esisin          S              1    INZ('N')                             *esistenza sinonimi
011100970923     d nlolet          S              9  0                                      *contatore loc lette
011200970923     d nloagg          S              9  0                                      *contatore loc agg.
011300970924     d ncaagg          S              9  0                                      *contatore cap agg.
011400970923     d staage          S             33                                         *agenzia in stampa
011500970923     d stanot          S             32                                         *note in stampa
011600970826     d*----------------
011700970826     d* Ds
011800970826     d*----------------
011900020919     D TIBS34DS      E DS                                                       *Profili utente
012000020919     D DDATIUTE      E DS                                                       *Dati utente
012100020919     D AZUTEDS       E DS                  extname(AZUTE00F)                    *Utenti
012200970918     d*p96dri          S              8  0                                      *parametri tisi96r
012300970918     d*p96ver          S              5  0
012400970828     D dssi88        E DS                  EXTNAME(tisi88ds)
012500970923     D dssi81        E DS                  EXTNAME(tisi81ds)
012600970923     D ds15          E DS                                                       *tab.nazioni
012700970918     d param           DS                                                       *parametri in input
012800970918     d  pardri                        8  0                                      *data riferim.
012900970918     d  paragg                        1                                         *agg. file s/n
013000970918     d  parsta                        1                                         *stampa s/n
013100970918     d  parnar                        3                                         *nazione x ins.
013200970918     d  parver                        3  0                                      *versione
013300970826     d kpjba         E DS                                                       *architettura
013400970826     c*--------------------------------------------------------------------------------------------*
013500970826     c* Main lines
013600970826     c*--------------------------------------------------------------------------------------------*
013700970826     C*
013800970826     C* stampa testata
013900970826if  1c                   IF        $err = 'N'                                   *no errori entrata
014000970826     C                   EXSR      states
014100970826     C*
014200970826     C* prima lettura -località poste-
014300970826     C                   EXSR      letfil
014400970826     C*
014500970826     C* ciclo fino a fine file
014600970826do  2c                   DOW       $fine='N'
014700970826     C*
014800120307     C* aggiorna le località BRT
014900970826     C                   EXSR      aggcpl
015000970826     C*
015100970826     C* lettura successiva -località poste-
015200970826     C                   EXSR      letfil
015300970826e   2c                   ENDDO                                                  *fine file
015400970826     C*
015500970826     C* stampa sintesi elaborazione
015600970826     C                   EXSR      staela
015700970826e   1c                   ENDIF
015800970826     C*
015900970826     C                   SETON                                        LR
016000970826     c*--------------------------------------------------------------------------------------------*
016100970826     c* letfil - lettura record
016200970826     c*--------------------------------------------------------------------------------------------*
016300970826     C     letfil        BEGSR
016400970826     c*
016500970826     C                   READ      azloc00f                               99
016600970826if  1c                   IF        *in99                                        *fine file
016700970826     c                   MOVEL     'S'           $fine                          *fine programma
016800970826e   1c                   ENDIF
016900970826     C*
017000970826     C                   ENDSR
017100970826     C*--------------------------------------------------------------------------------------------*
017200120307     C* aggcpl - aggiorna località BRT
017300970826     C*--------------------------------------------------------------------------------------------*
017400970826     C     aggcpl        BEGSR
017500970828     C*
017600970828     C* reimposta le variabili di lavoro
017700970923     c                   SETOFF                                       505152
017800970923     c                   SETOFF                                       535455
017900970923     c                   SETOFF                                       565759
018000970828     c                   MOVEL     'N'           locm35
018100970829     c                   MOVEL     'N'           esisin
018200970828     c                   CLEAR                   sin
018300970922     c                   CLEAR                   wcap
018400970826     C*
018500970826     C* conta il numero di località-poste lette
018600970923     c                   ADD       1             nlolet
018700970922     C*
018800970922     C* salva campi per stampa e lavoro
018900970922     c                   MOVEL     loccap        stacap
019000970922     c                   MOVEL     locloc        staloc
019100970922     c                   MOVEL     locprv        staprv
019200970923     c                   MOVEL     locage        staage
019300970923     c                   MOVEL     locnot        stanot
019400970826     C*
019500120307     C* controlla se la località poste è già nelle località BRT, se si esce
019600970918     c                   Z-ADD     parver        kclver
019700970918     c                   MOVEL     parnar        kclnar
019800970826     c                   MOVEL     loccap        kclcap
019900970826     c                   MOVEL     locloc        kclloc
020000970922     c     kcpl1         CHAIN     azcpl01l                           98
020100970826     C*
020200970922     C* località inesistente
020300970826if  1c                   IF        *in98
020400970923     C*
020500970923     C* tratta solo cap diversi da blank
020600970923if 1ac                   IF        loccap <> *blanks
020700970922     C*
020800970922     C* controlla se località poste maggiore di 35 char
020900970826     c     5             SUBST     locloc:36     a5
021000970922if  2c                   IF         a5 <> *blanks
021100970922     c                   MOVEL     'S'           locm35
021200970923     c                   SETON                                            50    *stampa magg.35 car.
021300970922e   2c                   ENDIF
021400970923     C*
021500970923     C* controllo se loc. poste esiste già in altro cap ed è univoca
021600970923     c                   EXSR      locuni
021700970922     C*
021800970922     C* controlla se esiste il cap delle poste
021900970922     c                   MOVEL     'N'           wcappt            1
022000970924     c                   MOVEL     'N'           wcapcr            1
022100970922     c                   Z-ADD     parver        kccver
022200970922     c                   MOVEL     parnar        kccnar
022300970922     c                   MOVEL     loccap        kcccap
022400970923     c     keycpc        CHAIN(N)  azcpc01l                           97
022500970922     C*
022600970924if  2c                   IF        *in97                                        *cap inesistente
022700970924     c                   SETON                                            52    *stampa nuovo cap
022800970924x   2c                   ELSE                                                   *cap esistente
022900970924     c                   MOVEL     'S'           wcappt
023000970924e   2c                   ENDIF
023100970923     C*
023200970923     C* ----- AGGIORNAMENTI -----
023300970923     C* se richiesto aggiornamento file cap, località e sinonimi e loc. inf. a 35 caratt.
023400970923if  2c                   IF         (paragg = 'S' or paragg = 'G')
023500970923     c                              and locm35 = 'N'
023600970923     C*
023700970923     C* ----- CAP -----
023800970923     C* cap poste inesistente
023900970924if  3c                   IF        wcappt = 'N'
024000970923     C* se loc. poste esiste già in altro cap ed è univoca
024100970923     C* creo cap delle poste dal cap di questa loc
024200970923if  4c                   IF        loctro = 1 and (paragg = 'G'
024300970923     c                             or paragg = 'S')
024400970923     c                   EXSR      crecap
024500970924     c                   MOVEL     'S'           wcapcr                         *cap creato
024600970923e   4c                   ENDIF
024700970923     C* se cap poste ancora non trovato, creo nuovo cap da cap generico
024800970924if  4c                   IF        wcapcr = 'N' and paragg = 'G'
024900970923     c                   EXSR      capgen
025000970923     c                   EXSR      crecap
025100970924     c                   MOVEL     'S'           wcapcr                         *cap creato
025200970922e   4c                   ENDIF
025300970922e   3c                   ENDIF
025400970922     C*
025500970923     C* ----- SINONIMI/LOCALITA ------
025600970922     C* se sono riuscito a creare il record del cap poste oppure esisteva già,
025700970922     C* creo anche sinonimi e località
025800970924if  3c                   IF        wcapcr = 'S' or wcappt = 'S'
025900970923     C* crea la località
026000970923     C* se una località già esistente cambia di cap rendo obsoleto il record con vecchio cap
026100970924     C* e scrivo nuova località con le stesse caratteristiche
026200970924if  4c                   IF        loctro = 1
026300970925     c                   EXSR      copsin                                       *copia sinonimi
026400970925     c                   EXSR      coploc
026500970924x   4c                   ELSE
026600970925     C* non trovata località univoca scrivo record sinonimi/località con dati standard
026700970925     c                   EXSR      cresin                                       *crea sinonimi
026800970925     c                   EXSR      creloc                                       *crea località
026900970924e   4c                   ENDIF
027000970923     C* aggiorna i flags del cap dai flags delle localita
027100970923     c                   EXSR      flacap
027200970922e   3c                   ENDIF
027300970923     C*
027400970923e   2c                   ENDIF
027500970923e  1ac                   ENDIF
027600970922     C*
027700970923     C* ----- STAMPA ------
027800970922     C* stampa riga dettaglio
027900970922     c                   EXSR      stadet
028000970922     C*
028100970918e   1c                   ENDIF
028200970826     C*
028300970826     C                   ENDSR
028400970922     c*--------------------------------------------------------------------------------------------*
028500120307     C* locuni - quando il cap delle poste non esiste cerca il cap BRT da cui copiare i dati.
028600120307     C*          controlla se la località delle poste è già nel localario BRT con
028700970922     C*          altro cap (e univoca in prov.) ed utilizza il cap della località diventata obsoleta
028800970922     c*--------------------------------------------------------------------------------------------*
028900970922     C     locuni        BEGSR
029000970922     C*
029100970922     c* controlla se la localita' delle poste esiste ed  è univoca
029200970922     c                   Z-ADD     *zeros        loctro            3 0          *loc.trovate
029300970922     c                   Z-ADD     parver        kclver
029400970922     c                   MOVEL     parnar        kclnar
029500970922     c                   MOVEL     locloc        kclloc
029600970922     C     kcpl2         SETLL     azcpl02l                           98
029700970922     C     kcpl2         READE     azcpl02l                               98
029800970922do  1c                   DOW       NOT *in98
029900970922if  2c                   IF        cplatb=*blanks                               *valida
030000970922     c                   Z-ADD     parver        kccver
030100970922     c                   MOVEL     parnar        kccnar
030200970922     c                   MOVEL     cplcap        kcccap
030300970925     c     keycpc        CHAIN(N)  azcpc01l                           99
030400970923if  3c                   IF        cpcprv=locprv or cpcpra=locprv               *stessa provincia
030500970922     c                   ADD       1             loctro                         *loc.trovate
030600970922     c                   MOVEL     cpccap        wcap                           *mem. cap
030700970922e   3C                   ENDIF
030800970922e   2C                   ENDIF
030900970922     C     kcpl2         READE     azcpl02l                               98
031000970922e   1C                   ENDDO                                                  *fine eccezioni
031100970923     c*
031200970923     C* se ho trovato piu' località poste, non è univoca
031300970923if  3c                   IF        loctro <> 1
031400970923     c                   CLEAR                   wcap
031500970923e   3C                   ENDIF
031600970923     C* se località poste inesistente, stampo 'nuova loc'
031700970923if  2c                   IF        loctro = 0
031800970923     c                   SETON                                            51
031900970923e   2c                   ENDIF
032000970923     C* se località poste trovata ed univoca, stampo 'veccio cap:'
032100970923if  2c                   IF        loctro = 1
032200970923     c                   SETON                                            53
032300970923e   2c                   ENDIF
032400970922     C*
032500970922     c                   ENDSR
032600970922     c*--------------------------------------------------------------------------------------------*
032700970922     C* capgen - crea cap delle poste copiando dati dal cap generico
032800970922     c*--------------------------------------------------------------------------------------------*
032900970922     C     capgen        BEGSR
033000970922     C*
033100970922     C* cerca cap generico
033200970923     C                   MOVEA     loccap        CAP
033300970922     C                   Z-ADD     §15PCF        P                 2 0
033400970922     C                   CLEAR                   CAF
033500970922     C                   MOVEA     CAP(P)        CAF(P)
033600970922     C                   ADD       §15ECF        P                              *CARATT.DA TENERE
033700970922     C                   IF        p < 10
033800970922     C                   MOVEA     *BLANKS       CAF(P)
033900970922     C                   ENDIF
034000970922     C                   MOVEA     CAF           WCAP                           *mem. cap fittizio
034100970922     c*
034200970922     c                   ENDSR
034300970922     c*--------------------------------------------------------------------------------------------*
034400970922     C* crecap - crea cap
034500970922     c*--------------------------------------------------------------------------------------------*
034600970922     C     crecap        BEGSR
034700970922     C*
034800970922     c                   Z-ADD     parver        kccver
034900970922     c                   MOVEL     parnar        kccnar
035000970922     c                   MOVEL     wcap          kcccap
035100970925     c     keycpc        CHAIN(N)  azcpc01l                           99
035200970922     c                   MOVEL     loccap        cpccap
035300970922     c                   MOVEL     'N'           cpcese
035400970922     c                   MOVEL     'N'           cpcesl
035500970922     c                   MOVEL     *blanks       cpcftr
035600970924     c                   Z-ADD     *zeros        cpcdtr
035700970923     c                   WRITE     azcpc000
035800970924     c                   ADD       1             ncaagg
035900970923     c                   SETON                                            55
036000970922     c*
036100970922     c                   ENDSR
036200970923     c*--------------------------------------------------------------------------------------------*
036300970923     C* creloc - crea localita
036400970923     c*--------------------------------------------------------------------------------------------*
036500970923     C     creloc        BEGSR
036600970923     C*
036700970923     c                   CLEAR                   azcpl000
036800970923     c                   Z-ADD     parver        cplver
036900970923     c                   MOVEL     parnar        cplnar
037000970923     c                   MOVEL     loccap        cplcap
037100970923     c                   MOVEL     locloc        cplloc
037200970923     c                   MOVEL     'C'           cplliv
037300970923     c                   MOVEL     'N'           cpldlv
037400980612     c                   MOVEL     *blanks       cplesv
037500970923     c                   MOVEL     'N'           cpless
037600970923if  2c                   IF        esisin = 'S'
037700970923     c                   MOVEL     'S'           cpless
037800970923e   2c                   ENDIF
037900970923     c                   WRITE     azcpl000
038000120307     C* conta il numero di località-BRT aggiunte
038100970923     c                   ADD       1             nloagg
038200970923     c                   SETON                                            56
038300970923     c*
038400970923     c                   ENDSR
038500970923     c*--------------------------------------------------------------------------------------------*
038600970925     C* coploc - rende obsoleta la località sul vecchio cap e scrive nuova loc.
038700970923     c*--------------------------------------------------------------------------------------------*
038800970925     C     coploc        BEGSR
038900970923     C*
039000970924     C* obsoletizza vecchia località
039100970923     c                   Z-ADD     parver        kclver
039200970923     c                   MOVEL     parnar        kclnar
039300970923     c                   MOVEL     wcap          kclcap
039400970923     c                   MOVEL     locloc        kclloc
039500970923     c     kcpl1         CHAIN     azcpl01l                           99
039600970923     c                   MOVEL     'O'           cplfi1
039700970923     c                   MOVEL     *blanks       cplftr
039800970923     c                   UPDATE    azcpl000
039900970924     C* scrive la nuova
040000970924     c                   MOVEL     loccap        cplcap
040100970924     c                   MOVEL     *blanks       cplfi1
040200970924     c                   MOVEL     *blanks       cplftr
040300970924     c                   Z-ADD     *zeros        cpldtr
040400970924     c                   MOVEL     'N'           cpless
040500970924if  2c                   IF        esisin = 'S'
040600970924     c                   MOVEL     'S'           cpless
040700970924e   2c                   ENDIF
040800970924     c* se il cap delle poste esisteva già prendo i dati della loc. da quel cap + livello L (loc.)
040900970924if  2c                   IF        wcapcr = 'N' and wcappt = 'S'
041000970924     c                   EXSR      caploc
041100970924e   2c                   ENDIF
041200970924     c                   WRITE     azcpl000
041300970924     c                   ADD       1             nloagg
041400970924     c                   SETON                                            56
041500970923     c*
041600970923     c                   ENDSR
041700970924     c*--------------------------------------------------------------------------------------------*
041800970924     C* caploc - imposta i dati della località con i dati del cap che aveva precedentemente+liv. 'L'
041900970924     c*--------------------------------------------------------------------------------------------*
042000970924     C     caploc        BEGSR
042100970924     C*
042200970925     C* leggo cap attuale della località e memorizzo dati (dopo)
042300970924     c                   Z-ADD     parver        kccver
042400970924     c                   MOVEL     parnar        kccnar
042500970924     c                   MOVEL     loccap        kcccap
042600970925     c     keycpc        CHAIN(N)  azcpc01l                           99
042700970924     c                   Z-ADD     cpclna        dlna
042800970924     c                   Z-ADD     cpcznc        dznc
042900970924     c                   MOVEL     cpciso        diso
043000970924     c                   Z-ADD     cpczfd        dzfd
043100970924     c                   Z-ADD     cpclkg        dlkg
043200970924     c                   Z-ADD     cpclmc        dlmc
043300970924     c                   Z-ADD     cpclol        dlol
043400970924     c                   Z-ADD     cpczol        dzol
043500970924     c                   Z-ADD     cpclks        dlks
043600970924     c                   Z-ADD     cpclms        dlms
043700970924     c                   Z-ADD     cpclos        dlos
043800970924     c                   Z-ADD     cpczos        dzos
043900970924     c                   Z-ADD     cpclk2        dlk2
044000970924     c                   Z-ADD     cpclm2        dlm2
044100970924     c                   Z-ADD     cpclo2        dlo2
044200970924     c                   Z-ADD     cpczo2        dzo2
044300970924     c                   MOVEL     cpccts        dcts
044400970924     c                   MOVEL     cpccte        dcte
044500970924     c                   Z-ADD     cpcttc        dttc
044600970924     c                   MOVEL     cpcpii        dpii
044700970924     c                   MOVEL     cpcpid        dpid
044800970924     c                   MOVEL     cpcpis        dpis
044900970924     c                   MOVEL     cpcgdd        dgdd
045000970924     C*
045100970925     C* leggo cap/loc  precedente della località (prima)
045200970925     C* se loc. aveva liv. 'L' confronto con i dati della loc
045300970925     c                   IF        cplliv = 'L'
045400970925     c                   Z-ADD     cpllna        plna
045500970925     c                   Z-ADD     cplznc        pznc
045600970925     c                   MOVEL     cpliso        piso
045700970925     c                   Z-ADD     cplzfd        pzfd
045800970925     c                   Z-ADD     cpllkg        plkg
045900970925     c                   Z-ADD     cpllmc        plmc
046000970925     c                   Z-ADD     cpllol        plol
046100970925     c                   Z-ADD     cplzol        pzol
046200970925     c                   Z-ADD     cpllks        plks
046300970925     c                   Z-ADD     cpllms        plms
046400970925     c                   Z-ADD     cpllos        plos
046500970925     c                   Z-ADD     cplzos        pzos
046600970925     c                   Z-ADD     cpllk2        plk2
046700970925     c                   Z-ADD     cpllm2        plm2
046800970925     c                   Z-ADD     cpllo2        plo2
046900970925     c                   Z-ADD     cplzo2        pzo2
047000970925     c                   MOVEL     cplcts        pcts
047100970925     c                   MOVEL     cplcte        pcte
047200970925     c                   Z-ADD     cplttc        pttc
047300970925     c                   MOVEL     cplpii        ppii
047400970925     c                   MOVEL     cplpid        ppid
047500970925     c                   MOVEL     cplpis        ppis
047600970925     c                   MOVEL     cplgdd        pgdd
047700970925     c                   ELSE
047800970925     C* se loc. aveva liv. 'C' confronto con i dati del cap
047900970924     c                   Z-ADD     parver        kccver
048000970924     c                   MOVEL     parnar        kccnar
048100970924     c                   MOVEL     wcap          kcccap
048200970925     c     keycpc        CHAIN(N)  azcpc01l                           99
048300970925     c                   Z-ADD     cpclna        plna
048400970925     c                   Z-ADD     cpcznc        pznc
048500970925     c                   MOVEL     cpciso        piso
048600970925     c                   Z-ADD     cpczfd        pzfd
048700970925     c                   Z-ADD     cpclkg        plkg
048800970925     c                   Z-ADD     cpclmc        plmc
048900970925     c                   Z-ADD     cpclol        plol
049000970925     c                   Z-ADD     cpczol        pzol
049100970925     c                   Z-ADD     cpclks        plks
049200970925     c                   Z-ADD     cpclms        plms
049300970925     c                   Z-ADD     cpclos        plos
049400970925     c                   Z-ADD     cpczos        pzos
049500970925     c                   Z-ADD     cpclk2        plk2
049600970925     c                   Z-ADD     cpclm2        plm2
049700970925     c                   Z-ADD     cpclo2        plo2
049800970925     c                   Z-ADD     cpczo2        pzo2
049900970925     c                   MOVEL     cpccts        pcts
050000970925     c                   MOVEL     cpccte        pcte
050100970925     c                   Z-ADD     cpcttc        pttc
050200970925     c                   MOVEL     cpcpii        ppii
050300970925     c                   MOVEL     cpcpid        ppid
050400970925     c                   MOVEL     cpcpis        ppis
050500970925     c                   MOVEL     cpcgdd        pgdd
050600970925     c                   ENDIF
050700970924     C*
050800970925     C* se tutti i valori sono uguali tra prima e dopo, lascio
050900970925     c                   IF        plna = dlna and
051000970925     c                             pznc = dznc and
051100970925     c                             piso = diso and
051200970925     c                             pzfd = dzfd and
051300970925     c                             plkg = dlkg and
051400970925     c                             plmc = dlmc and
051500970925     c                             plol = dlol and
051600970925     c                             pzol = dzol and
051700970925     c                             plks = dlks and
051800970925     c                             plms = dlms and
051900970925     c                             plos = dlos and
052000970925     c                             pzos = dzos and
052100970925     c                             plk2 = dlk2 and
052200970925     c                             plm2 = dlm2 and
052300970925     c                             plo2 = dlo2 and
052400970925     c                             pzo2 = dzo2 and
052500970925     c                             pcts = dcts and
052600970925     c                             pcte = dcte and
052700970925     c                             pttc = dttc and
052800970925     c                             ppii = dpii and
052900970925     c                             ppid = dpid and
053000970925     c                             ppis = dpis and
053100970925     c                             pgdd = dgdd
053200970925     C* se tutti i valori sono uguali imposto liv. 'C' e azzero tutti i dati
053300970925     c                   MOVEL     'C'           cplliv
053400970925     c                   Z-ADD     *zeros        cpllna
053500970925     c                   Z-ADD     *zeros        cplznc
053600970925     c                   MOVEL     *blanks       cpliso
053700970925     c                   Z-ADD     *zeros        cplzfd
053800970925     c                   Z-ADD     *zeros        cpllkg
053900970925     c                   Z-ADD     *zeros        cpllmc
054000970925     c                   Z-ADD     *zeros        cpllol
054100970925     c                   Z-ADD     *zeros        cplzol
054200970925     c                   Z-ADD     *zeros        cpllks
054300970925     c                   Z-ADD     *zeros        cpllms
054400970925     c                   Z-ADD     *zeros        cpllos
054500970925     c                   Z-ADD     *zeros        cplzos
054600970925     c                   Z-ADD     *zeros        cpllk2
054700970925     c                   Z-ADD     *zeros        cpllm2
054800970925     c                   Z-ADD     *zeros        cpllo2
054900970925     c                   Z-ADD     *zeros        cplzo2
055000970925     c                   MOVEL     *blanks       cplcts
055100970925     c                   MOVEL     *blanks       cplcte
055200970925     c                   Z-ADD     *zeros        cplttc
055300970925     c                   MOVEL     *blanks       cplpii
055400970925     c                   MOVEL     *blanks       cplpid
055500970925     c                   MOVEL     *blanks       cplpis
055600970925     c                   MOVEL     *blanks       cplgdd
055700970925     C* se c'e' almeno un valore diverso prendo dati 'prima' + liv. 'L'
055800970924     c                   ELSE
055900970924     c                   MOVEL     'L'           cplliv
056000970925     c                   Z-ADD     plna          cpllna
056100970925     c                   Z-ADD     pznc          cplznc
056200970925     c                   MOVEL     piso          cpliso
056300970925     c                   Z-ADD     pzfd          cplzfd
056400970925     c                   Z-ADD     plkg          cpllkg
056500970925     c                   Z-ADD     plmc          cpllmc
056600970925     c                   Z-ADD     plol          cpllol
056700970925     c                   Z-ADD     pzol          cplzol
056800970925     c                   Z-ADD     plks          cpllks
056900970925     c                   Z-ADD     plms          cpllms
057000970925     c                   Z-ADD     plos          cpllos
057100970925     c                   Z-ADD     pzos          cplzos
057200970925     c                   Z-ADD     plk2          cpllk2
057300970925     c                   Z-ADD     plm2          cpllm2
057400970925     c                   Z-ADD     plo2          cpllo2
057500970925     c                   Z-ADD     pzo2          cplzo2
057600970925     c                   MOVEL     pcts          cplcts
057700970925     c                   MOVEL     pcte          cplcte
057800970925     c                   Z-ADD     pttc          cplttc
057900970925     c                   MOVEL     ppii          cplpii
058000970925     c                   MOVEL     ppid          cplpid
058100970925     c                   MOVEL     ppis          cplpis
058200970925     c                   MOVEL     pgdd          cplgdd
058300970924     c                   ENDIF
058400970924     c*
058500970924     c                   ENDSR
058600970925     c*--------------------------------------------------------------------------------------------*
058700970925     C* copsin - copia i sinonimi della località già esistente
058800970925     c*--------------------------------------------------------------------------------------------*
058900970925     C     copsin        BEGSR
059000970925     C*
059100970925     c                   Z-ADD     parver        kcsver
059200970925     c                   MOVEL     parnar        kcsnar
059300970925     c                   MOVEL     wcap          kcscap
059400970925     c                   MOVEL     locloc        kcsloc
059500970925     C     kcpsr         SETLL     azcps01l                           98
059600970925     C     kcpsr         READE(N)  azcps01l                               98
059700970925do  1c                   DOW       NOT *in98
059800970925if  2c                   IF        cpsatb=*blanks                               *valida
059900970925     c                   MOVEL     loccap        cpscap
060000970925     c                   MOVEL     *blanks       cpsftr
060100970925     c                   Z-ADD     *zeros        cpsdtr
060200970925     c                   WRITE     azcps000
060300970925     c                   MOVEL     'S'           esisin                         *esistenza sinonimi
060400970925     c                   SETON                                            57
060500970925e   2C                   ENDIF
060600970925     C     kcpsr         READE(N)  azcps01l                               98
060700970925e   1C                   ENDDO                                                  *fine eccezioni
060800970925     c*
060900970925     c                   ENDSR
061000970922     c*--------------------------------------------------------------------------------------------*
061100970922     C* cresin - crea sinonimi
061200970922     c*--------------------------------------------------------------------------------------------*
061300970922     C     cresin        BEGSR
061400970922     C*
061500970922     c                   CLEAR                   dssi88
061600970922     c                   MOVEL     locloc        i88loc
061700970922     C                   CALL      'TISI88R'
061800970922     C                   PARM                    dssi88
061900970922     c                   MOVEA     o88sin        sin
062000970922do  1c     1             DO        60            i
062100970922if  2c                   IF        sin(i) <> *blanks
062200970922     c                   Z-ADD     parver        kcsver
062300970922     c                   MOVEL     parnar        kcsnar
062400970922     c                   MOVEL     loccap        kcscap
062500970922     c                   MOVEL     locloc        kcsloc
062600970922     c                   MOVEL     sin(i)        kcssin
062700970922     c     keycps        CHAIN     azcps01l                           98
062800970922if  3c                   IF        *in98                                        *inesistente
062900970922     c                   CLEAR                   azcps000
063000970922     c                   Z-ADD     kcsver        cpsver
063100970922     c                   MOVEL     kcsnar        cpsnar
063200970922     c                   MOVEL     kcscap        cpscap
063300970922     c                   MOVEL     kcsloc        cpsloc
063400970922     c                   MOVEL     kcssin        cpssin
063500970922     c                   WRITE     azcps000
063600970922     c                   MOVEL     'S'           esisin                         *esistenza sinonimi
063700970923     c                   SETON                                            57
063800970922e   3c                   ENDIF
063900970922e   2c                   ENDIF
064000970922e   1c                   ENDDO
064100970922     C*
064200970922     c                   ENDSR
064300970923     c*--------------------------------------------------------------------------------------------*
064400970923     C* flacap - aggiorna flag cap da flag localita
064500970923     c*--------------------------------------------------------------------------------------------*
064600970923     C     flacap        BEGSR
064700970923     C*
064800970923     c                   CLEAR                   dssi81
064900970923     c                   Z-ADD     parver        d81ver
065000970923     c                   MOVEL     parnar        d81nar
065100970923     c                   MOVEL     loccap        d81cap
065200970923     c                   MOVEL     'L'           d81chk
065300970923     C                   CALL      'TISI81R'
065400970923     C                   PARM                    dssi81
065500970923     C*
065600970923     c                   ENDSR
065700970826     c*--------------------------------------------------------------------------------------------*
065800970826     C* stadet - stampa riga di dettaglio
065900970826     c*--------------------------------------------------------------------------------------------*
066000970826     C     stadet        BEGSR
066100970826     C*
066200970826     C* stampa la testata
066300970826     c                   EXSR      states
066400970923     c                   IF        loclo1 <> *blanks
066500970923     c                   SETON                                        59
066600970923     c                   ENDIF
066700970826     c                   EXCEPT    dettaglio
066800970826     C*
066900970826     c                   ENDSR
067000970826     C*--------------------------------------------------------------------------------------------*
067100970826     C* staela - stampa sintesi elaborazione
067200970826     C*--------------------------------------------------------------------------------------------*
067300970826     C     staela        BEGSR
067400970826     C*
067500970826     c                   EXSR      states
067600970826     c                   EXCEPT    riga
067700970826     c                   EXCEPT    elabora
067800970826     C*
067900970826     c                   ENDSR
068000970826     C*--------------------------------------------------------------------------------------------*
068100970826     C* states - stampa testata
068200970826     C*--------------------------------------------------------------------------------------------*
068300970826     C     states        BEGSR
068400970826     C*
068500970826if  1c                   IF        *inof                                        *se fine pagina
068600970826     c                   EXCEPT    testata
068700970826     c                   SETOFF                                       of
068800970826e   1c                   ENDIF
068900970826     C*
069000970826     c                   ENDSR
069100020919     C*--------------------------------------------------------------------------------------------*
069200020919     C* REPERISCE I DATI UTENTE
069300020919     C*--------------------------------------------------------------------------------------------*
069400020919     C     REPDATIUTE    BEGSR
069500020919     C*
069600020919     C* INIZIALIZZA VARIABILI DI WRK
069700020919     C                   CLEAR                   TIBS34DS
069800020919     C                   CLEAR                   AZUTEDS
069900020919     C                   CLEAR                   DDATIUTE
070000020919     C*
070100020919     C     *DTAARA       DEFINE    §azute        azuteds
070200020919     C     *DTAARA       DEFINE    §datiute      ddatiute
070300020919     C                   IN(E)     *DTAARA
070400020919if  1C                   IF        %Error
070500020919     c                   EVAL      I34Tla = 'L'
070600020919     C                   CALL      'TIBS34R'
070700020919     C                   PARM                    Tibs34Ds
070800020919     C                   IN        *DTAARA
070900020919e   1C                   ENDIF
071000020919     C*
071100020919     C* ASSEGNO LA DESCRIZIONE DEL S.I./UTENTE AL CAMPO DEL VIDEO
071200020919     C                   MOVEL(P)  RSUT          DSFIRS           20
071300020919     C*
071400020919     C                   ENDSR
071500970826     c*--------------------------------------------------------------------------------------------*
071600970826     C* *inzsr - operazioni iniziali
071700970826     c*--------------------------------------------------------------------------------------------*
071800970826     C     *inzsr        BEGSR
071900970826     c*
072000970826     c* ricevimento parametri
072100970826     c     *ENTRY        PLIST
072200970826     c                   PARM                    kpjba
072300970918     c                   MOVEL     kpjbu         param
072400970826     C*
072500970826     C* chiavi di lettura
072600970922     C     keycpc        KLIST                                                  *azcpc01l
072700970922     C                   KFLD                    kccver                         -versione
072800970922     C                   KFLD                    kccnar                         -nazione
072900970922     C                   KFLD                    kcccap                         -cap
073000970922     C     kcpl1         KLIST                                                  *azcpl01l
073100970826     C                   KFLD                    kclver                         -versione
073200970826     C                   KFLD                    kclnar                         -nazione
073300970826     C                   KFLD                    kclcap                         -cap
073400970826     C                   KFLD                    kclloc                         -località
073500970922     C     kcpl2         KLIST                                                  *azcpl02l
073600970922     C                   KFLD                    kclver                         -versione
073700970922     C                   KFLD                    kclnar                         -nazione
073800970922     C                   KFLD                    kclloc                         -località
073900970828     C     keycps        KLIST                                                  *azcps01l
074000970828     C                   KFLD                    kcsver                         -versione
074100970828     C                   KFLD                    kcsnar                         -nazione
074200970828     C                   KFLD                    kcscap                         -cap
074300970828     C                   KFLD                    kcsloc                         -località
074400970828     C                   KFLD                    kcssin                         -sinonimo
074500970925     C     kcpsr         KLIST                                                  *azcps01l ridotta
074600970925     C                   KFLD                    kcsver                         -versione
074700970925     C                   KFLD                    kcsnar                         -nazione
074800970925     C                   KFLD                    kcscap                         -cap
074900970925     C                   KFLD                    kcsloc                         -località
075000970923     C     ktbl          KLIST                                                  *tabel00f
075100970922     C                   KFLD                    ktbkut                         -cod.utente
075200970922     C                   KFLD                    ktbcod                         -tipo tabella
075300970922     C                   KFLD                    ktbkey                         -chiave tab.
075400970826     C*
075500970826     C* reperimento dati utente
075600020919     C                   EXSR      REPDATIUTE
075700970826     C*
075800970922     C* recupera i dati della nazione
075900970922     c                   Z-ADD     1             ktbkut
076000970922     c                   MOVEL     '15'          ktbcod
076100970922     c                   MOVEL     *blanks       ktbkey
076200970922     c                   MOVEL     parnar        ktbkey
076300970923     c     ktbl          CHAIN     tabel00f                           99
076400970922     c  N99              MOVEL     tbluni        ds15
076500970826     C*
076600970826     C* impostazione iniziale variabili
076700970826     c                   SETON                                        of        *overflow
076800970826     C*
076900970826     C                   ENDSR
077000970826     o*--------------------------------------------------------------------------------------------*
077100970826     o* Stampa
077200970826     o*--------------------------------------------------------------------------------------------*
077300970923     Oprtf198   E            testata           2
077400970826     O                       slin(1)             66
077500970826     O                       slin(2)            132
077600970826     O                       dsfirs              20
077700970826     O                       udate              114 '  /  /  '
077800970924     O                       page          z    132
077900970826     O          E            testata           3
078000970826     O                       slin(3)             66
078100970826     O                       slin(3)            132
078200970923     O                       slin(3)            198
078300970826     O          E            testata           4
078400970826     O                       slin(4)             66
078500970924     O                       slin(6)            188
078600970826     O          E            testata           5
078700970826     O                       slin(3)             66
078800970826     O                       slin(3)            132
078900970923     O                       slin(3)            198
079000970826     O*------------------------
079100970826     O          E            dettaglio   1
079200970922     o                       staprv               2
079300970923     o                       stacap               8
079400970922     o                       staloc              50
079500970923     o               50                          +1 'Magg.35 carat.'
079600970923     o               51                          +1 'Nuova loc.'
079700970923     o               52                          +1 'Nuovo CAP '
079800970923     o               53                          +1 'Vecchio CAP:'
079900970923     o                       wcap                +1
080000970923     o               55                          +1 'Agg.CAP'
080100970923     o               56                          +1 'Agg.Loc.'
080200970923     o               57                          +1 'Agg.Sin.'
080300970923     o                       staage             171
080400970923     o                       stanot             198
080500970923     O          E    59      dettaglio   1
080600970924     o                       loclo1             176
080700970826     O*------------------------
080800970826     O          E            riga        1
080900970826     O                       slin(3)             66
081000970826     O                       slin(3)            132
081100970923     O                       slin(3)            198
081200970826     O*------------------------
081300970826     O          E            elabora     3
081400970826     O                       slin(7)             66
081500970826     O                       slin(8)            132
081600970924     O                       slin(9)            198
081700970923     o                       nlolet        2     34
081800110511     o                       nloagg        2    101
081900970924     o                       ncaagg        2    167
082000970826     o*--------------------------------------------------------------------------------------------*
082100970826     o* righe di stampa
082200970826     o*--------------------------------------------------------------------------------------------*
082300970826** slin - righe di stampa
082400970826xxxxxxxxxxxxxxxxxxxx        TISI84R     * AGGIORNAMENTO LOCALITA' 1
082500110511BRT DA LOCALITA' POSTE *                99/99/99          PAG.99992
082600970826------------------------------------------------------------------3
082700970924PR  CAP   NUOVA LOCALITA'                                         4
082800970826xxx xxxxx xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx               5
082900970924                AGENZIA POSTE/BILINGUE      NOTE                  6
083000970826LOCALITA' POSTE LETTE: 999.999.999                                7
083100110511LOCALITA' BRT AGGIUNTE: 999.999.999                               8
083200120307CAP BRT       AGGIUNTI: 999.999.999                               9
08330097082610
