000100970912     h*--------------------------------------------------------------------------------------------*
000200970912     h* Allineamento cappario con localià per clienti - Esegue
000300970912     h*--------------------------------------------------------------------------------------------*
000400970912     h DECEDIT('0,') DATEDIT(*DMY.)
000500970912     f*--------------------------------------------------------------------------------------------*
000600970912     f* Data base
000700970912     f*--------------------------------------------------------------------------------------------*
000800970912     f*---
000900970912     f* Archivi da copiare
001000970912     f*---
001100970912     fazcpr01l  IF   E           K DISK                                         *Versioni
001200970912     f                                     PREFIX(f_)
001300970912     fazcpc01l  IF   E           K DISK                                         *Cap
001400970912     f                                     PREFIX(f_)
001500970912     fazcpl01l  IF   E           K DISK                                         *Località
001600970912     f                                     PREFIX(f_)
001700970912     fazcps01l  IF   E           K DISK                                         *Sinonimi
001800970912     f                                     PREFIX(f_)
001900970912     fazcpe01l  IF   E           K DISK                                         *Eccezioni linee
002000970912     f                                     PREFIX(f_)
002100971003     fazcae01l  IF   E           K DISK                                         *Eccezioni terminal
002200971003     f                                     PREFIX(f_)
002300970912     f*---
002400970912     f* Archivi su cui copiare
002500970912     f*---
002600970912     fazcpr00c  O    E           K DISK                                         *Versioni
002700970912     f                                     RENAME(azcpr000:azcprc)
002800970912     f                                     PREFIX(c_)
002900970912     fazcpc00c  O    E           K DISK                                         *Cap
003000970912     f                                     RENAME(azcpc000:azcpcc)
003100970912     f                                     PREFIX(c_)
003200970912     fazcpl00c  O    E           K DISK                                         *Località
003300970912     f                                     RENAME(azcpl000:azcplc)
003400970912     f                                     PREFIX(c_)
003500970912     fazcps00c  O    E           K DISK                                         *Sinonimi
003600970912     f                                     RENAME(azcps000:azcpsc)
003700970912     f                                     PREFIX(c_)
003800970912     fazcpe00c  O    E           K DISK                                         *Eccezioni linee
003900970912     f                                     RENAME(azcpe000:azcpec)
004000970912     f                                     PREFIX(c_)
004100971003     fazcae00c  O    E           K DISK                                         *Eccezioni terminal
004200971003     f                                     RENAME(azcae000:azcaec)
004300971003     f                                     PREFIX(c_)
004400970912     d*--------------------------------------------------------------------------------------------*
004500970912     d* Data structure
004600970912     d*--------------------------------------------------------------------------------------------*
004700970912     d*----------------
004800970912     d* Variabili riferite al data base
004900970912     d*----------------
005000970912     d kccver          S                   LIKE(f_cpcver)                       *per azcpc00f
005100970912     d kclver          S                   LIKE(f_cplver)                       *per azcpl00f
005200970912     d kcsver          S                   LIKE(f_cpsver)                       *per azcps00f
005300970912     d kcsnar          S                   LIKE(f_cpsnar)
005400970912     d kcscap          S                   LIKE(f_cpscap)
005500970912     d kcsloc          S                   LIKE(f_cpsloc)
005600970912     d kcever          S                   LIKE(f_cpever)                       *per azcpe00f
005700970912     d*----------------
005800970912     d* Variabili di controllo
005900970912     d*----------------
006000970912     d $fine           S              1    INZ('N')                             *fine programma
006100970912     d $finc           S              1    INZ('N')                             *fine ciclo
006200970912     d $recok          S              1    INZ('N')                             *validità record
006300970912     d*----------------
006400970912     d* Variabili di programma
006500970912     d*----------------
006600970912     d n14             S             14  0                                      *Numerico 14
006700970912     d n8              S              8  0                                      *Numerico 8
006800970912     d datcor          S              8  0                                      *data corrente a/m/g
006900970912     d*----------------
007000970912     d* Ds
007100970912     d*----------------
007200970912     d dsazcprf      E DS                  EXTNAME(azcpr00f)                    *ds record fisico
007300970912     d                                     INZ
007400970912     d                                     PREFIX(f_)
007500970912     d dsazcprc      E DS                  EXTNAME(azcpr00c)                    *ds record cliente
007600970912     d                                     PREFIX(c_)
007700970912     d                                     INZ
007800970912     d dsazcpcf      E DS                  EXTNAME(azcpc00f)                    *ds record fisico
007900970912     d                                     INZ
008000970912     d                                     PREFIX(f_)
008100970912     d dsazcpcc      E DS                  EXTNAME(azcpc00c)                    *ds record cliente
008200970912     d                                     PREFIX(c_)
008300970912     d                                     INZ
008400970912     d dsazcplf      E DS                  EXTNAME(azcpl00f)                    *ds record fisico
008500970912     d                                     INZ
008600970912     d                                     PREFIX(f_)
008700970912     d dsazcplc      E DS                  EXTNAME(azcpl00c)                    *ds record cliente
008800970912     d                                     PREFIX(c_)
008900970912     d                                     INZ
009000970912     d dsazcpsf      E DS                  EXTNAME(azcps00f)                    *ds record fisico
009100970912     d                                     INZ
009200970912     d                                     PREFIX(f_)
009300970912     d dsazcpsc      E DS                  EXTNAME(azcps00c)                    *ds record cliente
009400970912     d                                     PREFIX(c_)
009500970912     d                                     INZ
009600970912     d dsazcpef      E DS                  EXTNAME(azcpe00f)                    *ds record fisico
009700970912     d                                     INZ
009800970912     d                                     PREFIX(f_)
009900970912     d dsazcpec      E DS                  EXTNAME(azcpe00c)                    *ds record cliente
010000970912     d                                     PREFIX(c_)
010100970912     d                                     INZ
010200971003     d dsazcaef      E DS                  EXTNAME(azcae00f)                    *ds record fisico
010300971003     d                                     INZ
010400971003     d                                     PREFIX(f_)
010500971003     d dsazcaec      E DS                  EXTNAME(azcae00c)                    *ds record cliente
010600971003     d                                     PREFIX(c_)
010700971003     d                                     INZ
010800970912     d wlbda8          DS                                                       *controlla data (8)
010900970912     d  g08dat                        8  0
011000970912     d  g08inv                        8  0
011100970912     d  g08err                        1
011200970912     d  g08tgi                        5  0
011300970912     d dspar54         DS                                                       *ds parametri lancio
011400970912     d  p54dri                        8                                          -data riferimento
011500970912     d  p54src                        1                                          -scarica sorgenti
011600010918     d  p54azz                        1                                          -azzeramento linee
011700970912     d ut§dse        E DS                  EXTNAME(ut§dse0f)                    *reper.dati utente
011800970912     d cncr80        E DS
011900970912     d kpjba         E DS                                                       *architettura
012000970912     c*--------------------------------------------------------------------------------------------*
012100970912     c* Main lines
012200970912     c*--------------------------------------------------------------------------------------------*
012300970912     C*
012400970912     C* controlla e imposta i parametri di entrata
012500970912     C                   EXSR      imppar
012600970912     C*
012700970912     C* legge le versioni da copiare
012800970912     C                   EXSR      setfil
012900970912     C*
013000970912     C* ciclo fino a fine file
013100971003do  1C                   DOW       $fine='N'
013200970912     C*
013300970912     C* copia le versioni
013400970912     C                   EXSR      cpycpr
013500970912     C*
013600970912     C* copia i cap
013700970912     C                   EXSR      cpycpc
013800970912     C*
013900970912     C* copia le località e i sinonimi
014000970912     C                   EXSR      cpycpls
014100970912     C*
014200970912     C* copia le eccezioni linee/zone
014300970912     C                   EXSR      cpycpe
014400970912     C*
014500970912     C* lettura sucessiva record
014600970912     C                   EXSR      letfil
014700970912     C*
014800971003e   1C                   ENDDO                                                  *fine versioni
014900971003     C*
015000971003     C* copia le eccezioni terminal
015100971003     C                   EXSR      cpycae
015200970912     C*
015300970912     C                   SETON                                            lr
015400970912     c*--------------------------------------------------------------------------------------------*
015500970912     c* setfil - posizionamento sul file e prima lettura
015600970912     c*--------------------------------------------------------------------------------------------*
015700970912     C     setfil        BEGSR
015800970912     C*
015900970912     C* posizionamento sul file
016000970912     c     *loval        SETLL     azcpr01l                           99
016100970912if  1c                   IF        NOT *in99
016200970912     c                   EXSR      letfil                                       *lettura record
016300970912x   1c                   ELSE
016400970912     C                   MOVEL     'S'           $fine                          *fine programma
016500970912e   1C                   ENDiF
016600970912     C*
016700970912     C                   ENDSR
016800970912     c*--------------------------------------------------------------------------------------------*
016900970912     c* letfil - lettura record
017000970912     c*--------------------------------------------------------------------------------------------*
017100970912     C     letfil        BEGSR
017200970912     C*
017300970912     C* Legge fino a:
017400970912     C*       -fine file
017500970912     C*       -trovato record valido
017600970912     c                   MOVEL     'N'           $recok                         *record non valido
017700970912do  1c                   DOW       NOT *in99 AND
017800970912     c                             $recok='N'
017900970912     C                   READ      azcpr01l                               99
018000970912if  2c                   IF        *in99                                        *fine file
018100970912     c                   MOVEL     'S'           $fine                          *fine programma
018200970912x   2c                   ELSE                                                   *record trovato
018300970912     c                   EXSR      chkcpr                                       *controlla record
018400970912e   2c                   ENDIF
018500970912e   1c                   ENDDO
018600970912     C*
018700970912     C                   ENDSR
018800970912     c*--------------------------------------------------------------------------------------------*
018900970912     C* chkcpr - controlla validità record -azcpr-
019000970912     c*--------------------------------------------------------------------------------------------*
019100970912     C     chkcpr        BEGSR
019200970912     C*
019300970912     c                   MOVEL     'S'           $recok                         *record valido
019400970912     C*
019500970912     C* record annullato, esclude
019600970912if  1c                   IF        f_cpratb <> *blanks
019700970912     c                   MOVEL     'N'           $recok                         *record non valido
019800970912e   1c                   ENDIF
019900970912     C*
020000970912     C* record con decorrenza minore della data richiesta, esclude
020100970912     c                   MOVEL     p54dri        n8
020200970912if  1c                   IF        f_cprdsc < n8
020300970912     c                   MOVEL     'N'           $recok                         *record non valido
020400970912e   1c                   ENDIF
020500970912     C*
020600970912     c                   ENDSR
020700970912     c*--------------------------------------------------------------------------------------------*
020800970912     C* cpycpr - copia le versioni
020900970912     c*--------------------------------------------------------------------------------------------*
021000970912     C     cpycpr        BEGSR
021100970912     C*
021200970912     c                   MOVEL     dsazcprf      dsazcprc                       *muove formato rec
021300011022     c                   IF        p54azz = 'S'
021400011022     c                   Z-ADD     20391231      c_cprdsc
021500011022     c                   MOVEL(P)  'DISK C'      c_cprdes
021600011022     c                   ENDIF
021700971113     c                   Z-ADD     99999999      c_cprdtr                       *deve essere pieno
021800010918     c                   WRITE     azcprc                                       *scrive
021900970912     c*
022000970912     c                   ENDSR
022100970912     c*--------------------------------------------------------------------------------------------*
022200970912     C* cpycpc - copia i cap
022300970912     c*--------------------------------------------------------------------------------------------*
022400970912     C     cpycpc        BEGSR
022500970912     c*
022600970912     c                   Z-ADD     f_cprver      kccver                         *versione
022700970912     c     keycpc        SETLL     azcpc01l
022800970912     C     keycpc        READE     azcpc01l                               98
022900970912do  1c                   DOW       NOT *in98
023000970912     c                   EXSR      chkcpc                                       *controlla record
023100970912if  2c                   IF        $recok = 'S'                                 *record valido
023200970912     c                   MOVEL     dsazcpcf      dsazcpcc                       *muove formato rec
023300970912     c* limite oltre
023400970912if  3c                   IF        f_cpclkg = *zeros
023500970912     c                   Z-ADD     999999,9      c_cpclkg
023600970912e   3c                   ENDIF
023700970912if  3c                   IF        f_cpclmc = *zeros
023800970912     c                   Z-ADD     99,999        c_cpclmc
023900970912e   3c                   ENDIF
024000970912if  3c                   IF        f_cpclol = *zeros
024100970912     c                   Z-ADD     f_cpclna      c_cpclol
024200970912     c                   Z-ADD     f_cpcznc      c_cpczol
024300970912e   3c                   ENDIF
024400970912     C*
024401030129if  3c                   IF        f_cpcfi2 = *blanks
024402030129     c                   MOVEL     '99'          c_cpcfi2                       *deve essere pieno
024403030129     c                   ELSE
024500030129     c                   MOVEL     f_cpcfi2      c_cpcfi2                       *deve essere pieno
024501030129e   3c                   ENDIF
024600010918     c*
024700010918     c* Se richiesto azzeramento linee testo qui e procedo
024800010918     c                   IF        p54azz = 'S'
024900010921     c                   IF        c_cpclna <> *zeros
025000010921     c                   EVAL      c_cpclna = 789
025100010921     c                   ENDIF
025200010921     c                   EVAL      c_cpcznc = *zeros
025300010921     c                   IF        c_cpclol <> *zeros
025400010921     c                   EVAL      c_cpclol = 789
025500010921     c                   ENDIF
025600010918     c                   EVAL      c_cpczol = *zeros
025700010921     c                   IF        c_cpclos <> *zeros
025800010921     c                   EVAL      c_cpclos = 789
025900010921     c                   ENDIF
026000010918     c                   EVAL      c_cpczos = *zeros
026100010921     c                   IF        c_cpclo2 <> *zeros
026200010921     c                   EVAL      c_cpclo2 = 789
026300010921     c                   ENDIF
026400010918     c                   EVAL      c_cpczo2 = *zeros
026500010918     c                   EVAL      c_cpczfd = *zeros
026600010918     c                   ENDIF
026700010918     c*
026800010918     c                   WRITE     azcpcc                                       *scrive
026900970912     C*
027000970912e   2c                   ENDIF
027100970912     C     keycpc        READE     azcpc01l                               98
027200970912e   1c                   ENDDO
027300970912     c*
027400970912     c                   ENDSR
027500970912     c*--------------------------------------------------------------------------------------------*
027600970912     C* chkcpc - controlla validità record -azcpc-
027700970912     c*--------------------------------------------------------------------------------------------*
027800970912     C     chkcpc        BEGSR
027900970912     C*
028000970912     c                   MOVEL     'S'           $recok                         *record valido
028100970912     C*
028200970912     C* record annullato, esclude
028300000622if  1c                   IF        f_cpcatb <> *blanks OR
028400000622     c                             f_cpcfi1 <> *blanks
028500970912     c                   MOVEL     'N'           $recok                         *record non valido
028600970912e   1c                   ENDIF
028700970912     C*
028800970912     c                   ENDSR
028900970912     c*--------------------------------------------------------------------------------------------*
029000970912     C* cpycpls - copia le località e i sinonimi
029100970912     c*--------------------------------------------------------------------------------------------*
029200970912     C     cpycpls       BEGSR
029300970912     c*
029400970912     c                   Z-ADD     f_cprver      kclver                         *versione
029500970912     c     keycpl        SETLL     azcpl01l
029600970912     C     keycpl        READE     azcpl01l                               98
029700970912do  1c                   DOW       NOT *in98
029800970912     c                   EXSR      chkcpl                                       *controlla record
029900970912if  2c                   IF        $recok = 'S'                                 *record valido
030000970912     c                   MOVEL     dsazcplf      dsazcplc                       *muove formato rec
030100970912     c* limite oltre
030200970912if  3c                   IF        f_cpllkg = *zeros
030300970912     c                   Z-ADD     999999,9      c_cpllkg
030400970912e   3c                   ENDIF
030500970912if  3c                   IF        f_cpllmc = *zeros
030600970912     c                   Z-ADD     99,999        c_cpllmc
030700970912e   3c                   ENDIF
030800970912if  3c                   IF        f_cpllol = *zeros
030900970912     c                   Z-ADD     f_cpllna      c_cpllol
031000970912     c                   Z-ADD     f_cplznc      c_cplzol
031100970912e   3c                   ENDIF
031200970912     C*
031201030129if  3c                   IF        f_cplfi2 = *blanks
031202030129     c                   MOVEL     '99'          c_cplfi2                       *deve essere pieno
031203030129     c                   ELSE
031204030129     c                   MOVEL     f_cplfi2      c_cplfi2                       *deve essere pieno
031205030129e   3c                   ENDIF
031400010918     c*
031500010921     c* Se richiesto azzeramento linee testo qui e procedo
031600010918     c                   IF        p54azz = 'S'
031700010921     c                   IF        c_cpllna <> *zeros
031800010921     c                   EVAL      c_cpllna = 789
031900010921     c                   ENDIF
032000010921     c                   EVAL      c_cplznc = *zeros
032100010921     c                   IF        c_cpllol <> *zeros
032200010921     c                   EVAL      c_cpllol = 789
032300010921     c                   ENDIF
032400010921     c                   EVAL      c_cplzol = *zeros
032500010921     c                   IF        c_cpllos <> *zeros
032600010921     c                   EVAL      c_cpllos = 789
032700010921     c                   ENDIF
032800010921     c                   EVAL      c_cplzos = *zeros
032900010921     c                   IF        c_cpllo2 <> *zeros
033000010921     c                   EVAL      c_cpllo2 = 789
033100010921     c                   ENDIF
033200010921     c                   EVAL      c_cplzo2 = *zeros
033300010921     c                   EVAL      c_cplzfd = *zeros
033400010918     c                   ENDIF
033500010918     c*
033600010918     c                   WRITE     azcplc                                       *scrive
033700970912     C*
033800970912     C* copia i sinonimi della località
033900970912     c                   EXSR      cpycps
034000970912e   2c                   ENDIF
034100970912     C     keycpl        READE     azcpl01l                               98
034200970912e   1c                   ENDDO
034300970912     c*
034400970912     c                   ENDSR
034500970912     c*--------------------------------------------------------------------------------------------*
034600970912     C* chkcpl - controlla validità record -azcpl-
034700970912     c*--------------------------------------------------------------------------------------------*
034800970912     C     chkcpl        BEGSR
034900970912     C*
035000970912     c                   MOVEL     'S'           $recok                         *record valido
035100970912     C*
035200980520     C* record annullato o obsoleto, esclude
035300980520if  1c                   IF        f_cplatb <> *blanks OR
035400980520     c                             f_cplfi1 <> *blanks
035500970912     c                   MOVEL     'N'           $recok                         *record non valido
035600970912e   1c                   ENDIF
035700970912     C*
035800970912     c                   ENDSR
035900970912     c*--------------------------------------------------------------------------------------------*
036000970912     C* cpycps - copia i sinonimi della località
036100970912     c*--------------------------------------------------------------------------------------------*
036200970912     C     cpycps        BEGSR
036300970912     c*
036400970912     c                   Z-ADD     f_cplver      kcsver                         *versione
036500970912     c                   MOVEL     f_cplnar      kcsnar                         *nazione
036600970912     c                   MOVEL     f_cplcap      kcscap                         *cap
036700970912     c                   MOVEL     f_cplloc      kcsloc                         *località
036800970912     c     keycps        SETLL     azcps01l
036900970912     C     keycps        READE     azcps01l                               98
037000970912do  1c                   DOW       NOT *in98
037100970912     c                   EXSR      chkcps                                       *controlla record
037200970912if  2c                   IF        $recok = 'S'                                 *record valido
037300970912     c                   MOVEL     dsazcpsf      dsazcpsc                       *muove formato rec
037400970912     C*
037500971113     c                   Z-ADD     99999999      c_cpsdtr                       *deve essere pieno
037600970912     c                   WRITE     azcpsc                                       *scrive
037700970912     C*
037800970912e   2c                   ENDIF
037900970912     C     keycps        READE     azcps01l                               98
038000970912e   1c                   ENDDO
038100970912     c*
038200970912     c                   ENDSR
038300970912     c*--------------------------------------------------------------------------------------------*
038400970912     C* chkcps - controlla validità record -azcps-
038500970912     c*--------------------------------------------------------------------------------------------*
038600970912     C     chkcps        BEGSR
038700970912     C*
038800970912     c                   MOVEL     'S'           $recok                         *record valido
038900970912     C*
039000970912     C* record annullato, esclude
039100980520if  1c                   IF        f_cpsatb <> *blanks
039200970912     c                   MOVEL     'N'           $recok                         *record non valido
039300970912e   1c                   ENDIF
039400970912     C*
039500970912     c                   ENDSR
039600970912     c*--------------------------------------------------------------------------------------------*
039700970912     C* cpycpe - copia le eccezioni linee/zone
039800970912     c*--------------------------------------------------------------------------------------------*
039900970912     C     cpycpe        BEGSR
040000970912     c*
040100970912     c                   Z-ADD     f_cprver      kcever                         *versione
040200970912     c     keycpe        SETLL     azcpe01l
040300970912     C     keycpe        READE     azcpe01l                               98
040400970912do  1c                   DOW       NOT *in98
040500970912     c                   EXSR      chkcpe                                       *controlla record
040600970912if  2c                   IF        $recok = 'S'                                 *record valido
040700970912     c                   MOVEL     dsazcpef      dsazcpec                       *muove formato rec
040800970912     C*
040900971113     c                   Z-ADD     99999999      c_cpedtr                       *deve essere pieno
041000010918     c*
041100010921     c* Se richiesto azzeramento linee testo qui e procedo
041200010918     c                   IF        p54azz = 'S'
041300010921     c                   IF        c_cpelna <> *zeros
041400010921     c                   EVAL      c_cpelna = 789
041500010921     c                   ENDIF
041600010921     c                   EVAL      c_cpeznc = *zeros
041700010921     c                   IF        c_cpelol <> *zeros
041800010921     c                   EVAL      c_cpelol = 789
041900010921     c                   ENDIF
042000010921     c                   EVAL      c_cpezol = *zeros
042100010921     c                   IF        c_cpelos <> *zeros
042200010921     c                   EVAL      c_cpelos = 789
042300010921     c                   ENDIF
042400010921     c                   EVAL      c_cpezos = *zeros
042500010921     c                   IF        c_cpelo2 <> *zeros
042600010921     c                   EVAL      c_cpelo2 = 789
042700010921     c                   ENDIF
042800010921     c                   EVAL      c_cpezo2 = *zeros
042900010918     c                   ENDIF
043000010918     C*
043100010918     c                   WRITE     azcpec                                       *scrive
043200970912     C*
043300970912e   2c                   ENDIF
043400970912     C     keycpe        READE     azcpe01l                               98
043500970912e   1c                   ENDDO
043600970912     c*
043700970912     c                   ENDSR
043800970912     c*--------------------------------------------------------------------------------------------*
043900970912     C* chkcpe - controlla validità record -azcpe-
044000970912     c*--------------------------------------------------------------------------------------------*
044100970912     C     chkcpe        BEGSR
044200970912     C*
044300970912     c                   MOVEL     'S'           $recok                         *record valido
044400970912     C*
044500970912     C* record annullato, esclude
044600970912if  1c                   IF        f_cpeatb <> *blanks
044700970912     c                   MOVEL     'N'           $recok                         *record non valido
044800970912e   1c                   ENDIF
044900970912     C*
045000970912     c                   ENDSR
045100971003     c*--------------------------------------------------------------------------------------------*
045200971003     C* cpycae - copia le eccezioni terminal
045300971003     c*--------------------------------------------------------------------------------------------*
045400971003     C     cpycae        BEGSR
045500971003     c*
045600971003     c     *loval        SETLL     azcae01l
045700971003     C                   READ      azcae01l                               98
045800971003do  1c                   DOW       NOT *in98
045900971003     c                   EXSR      chkcae                                       *controlla record
046000971003if  2c                   IF        $recok = 'S'                                 *record valido
046100971003     c                   MOVEL     dsazcaef      dsazcaec                       *muove formato rec
046200971003     C*
046300971113     c                   Z-ADD     99999999      c_caedtr                       *deve essere pieno
046400971003     c                   WRITE     azcaec                                       *scrive
046500971003     C*
046600971003e   2c                   ENDIF
046700971003     C                   READ      azcae01l                               98
046800971003e   1c                   ENDDO
046900010921     c* dopo aver inserito i record partenza (P) inserisco un unico record arrivo (A) "truccato"
047000011211     c* solo per cappario con 789                                           rrivo (A) "truccato"
047100011211     c     p54azz        ifeq      'S'
047200010921     c                   MOVEL     'A'           c_caeepa
047300010921     c                   EVAL      c_caetfp = *zeros
047400010921     c                   EVAL      c_caetfa = 789
047500010921     c                   EVAL      c_caetfe = 789
047600010921     c                   Z-ADD     19900101      c_caedde
047700010921     c                   Z-ADD     20391231      c_caedsc
047800010921     c                   Z-ADD     99999999      c_caedtr                       *deve essere pieno
047900010921     c                   WRITE     azcaec                                       *scrive
048000011211     c                   endif
048100971003     c*
048200971003     c                   ENDSR
048300971003     c*--------------------------------------------------------------------------------------------*
048400971003     C* chkcae - controlla validità record -azcae-
048500971003     c*--------------------------------------------------------------------------------------------*
048600971003     C     chkcae        BEGSR
048700971003     C*
048800971003     c                   MOVEL     'S'           $recok                         *record valido
048900971003     C*
049000971003     C* record annullato, esclude
049100971003if  1c                   IF        f_caeatb <> *blanks
049200971003     c                   MOVEL     'N'           $recok                         *record non valido
049300971003e   1c                   ENDIF
049400971003     C*
049500971003     C* record non in decorrenza, esclude
049600971003     c                   MOVEL     p54dri        n8
049700971008if  1c                   IF        f_caedsc < n8
049800971003     c                   MOVEL     'N'           $recok                         *record non valido
049900971003e   1c                   ENDIF
050000010921     C*
050100011211     C* record non di tipo partenza (P) esclude   se con azzeramento
050200011211if  1c                   IF        f_caeepa <> 'P' and
050300011211if  1c                             p54azz = 'S'
050400010921     c                   MOVEL     'N'           $recok                         *record non valido
050500010921e   1c                   ENDIF
050600971003     C*
050700971003     c                   ENDSR
050800970912     c*--------------------------------------------------------------------------------------------*
050900970912     C* imppar - controlla e imposta i parametri di entrata
051000970912     c*--------------------------------------------------------------------------------------------*
051100970912     C     imppar        BEGSR
051200970912     C*
051300970912if  1c                   IF        p54dri=*blanks                               parametri no passati
051400970912     c                   MOVEL     datcor        p54dri                         *data riferimento
051500970912e   1c                   ENDIF
051600970912     C*
051700970912     C                   ENDSR
051800970912     c*--------------------------------------------------------------------------------------------*
051900970912     C* *inzsr - operazioni iniziali
052000970912     c*--------------------------------------------------------------------------------------------*
052100970912     C     *inzsr        BEGSR
052200970912     c*
052300970912     c* ricevimento parametri
052400970912     c     *ENTRY        PLIST
052500970912     c                   PARM                    kpjba
052600970912     c                   MOVEL     kpjbu         dspar54
052700970912     C*
052800970912     C* chiavi di lettura
052900970912     C     keycpc        KLIST                                                  di azcpc01l
053000970912     C                   KFLD                    kccver                         -versione
053100970912     C     keycpl        KLIST                                                  di azcpl01l
053200970912     C                   KFLD                    kclver                         -versione
053300970912     C     keycps        KLIST                                                  di azcps01l
053400970912     C                   KFLD                    kcsver                         -versione
053500970912     C                   KFLD                    kcsnar                         -nazione
053600970912     C                   KFLD                    kcscap                         -cap
053700970912     C                   KFLD                    kcsloc                         -località
053800970912     C     keycpe        KLIST                                                  di azcpe01l
053900970912     C                   KFLD                    kcever                         -versione
054000970912     C*
054100970912     C* reperimento dati utente
054200020906     c                   Z-ADD     1             codut
054300970912     C                   CALL      'X§PARUT'
054400970912     C                   PARM                    ut§dse
054500970912     C                   MOVEL     rec80         cncr80
054600970912     C*
054700970912     C* reperimento data corrente
054800970912     C                   TIME                    n14                            *ora (6)+ data (8)
054900970912     C                   MOVE      n14           n8                             *data (8) in g/m/a
055000970912     C                   Z-ADD     n8            g08dat
055100970912     C                   Z-ADD     *zeros        g08inv
055200970912     C                   MOVEL     '0'           g08err
055300970912     C                   CALL      'XSRDA8'
055400970912     C                   PARM                    wlbda8
055500970912     C                   Z-ADD     g08inv        datcor                         *data corrente a/m/g
055600970912     C*
055700970912     C                   ENDSR
