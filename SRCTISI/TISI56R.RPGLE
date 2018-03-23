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
011700061020     d  p54obs                        1                                          -esclusione obsolet
011800970912     d ut§dse        E DS                  EXTNAME(ut§dse0f)                    *reper.dati utente
011900970912     d cncr80        E DS
012000970912     d kpjba         E DS                                                       *architettura
012100970912     c*--------------------------------------------------------------------------------------------*
012200970912     c* Main lines
012300970912     c*--------------------------------------------------------------------------------------------*
012400970912     C*
012500970912     C* controlla e imposta i parametri di entrata
012600970912     C                   EXSR      imppar
012700970912     C*
012800970912     C* legge le versioni da copiare
012900970912     C                   EXSR      setfil
013000970912     C*
013100970912     C* ciclo fino a fine file
013200971003do  1C                   DOW       $fine='N'
013300970912     C*
013400970912     C* copia le versioni
013500970912     C                   EXSR      cpycpr
013600970912     C*
013700970912     C* copia i cap
013800970912     C                   EXSR      cpycpc
013900970912     C*
014000970912     C* copia le località e i sinonimi
014100970912     C                   EXSR      cpycpls
014200970912     C*
014300970912     C* copia le eccezioni linee/zone
014400970912     C                   EXSR      cpycpe
014500970912     C*
014600970912     C* lettura sucessiva record
014700970912     C                   EXSR      letfil
014800970912     C*
014900971003e   1C                   ENDDO                                                  *fine versioni
015000971003     C*
015100971003     C* copia le eccezioni terminal
015200971003     C                   EXSR      cpycae
015300970912     C*
015400970912     C                   SETON                                            lr
015500970912     c*--------------------------------------------------------------------------------------------*
015600970912     c* setfil - posizionamento sul file e prima lettura
015700970912     c*--------------------------------------------------------------------------------------------*
015800970912     C     setfil        BEGSR
015900970912     C*
016000970912     C* posizionamento sul file
016100970912     c     *loval        SETLL     azcpr01l                           99
016200970912if  1c                   IF        NOT *in99
016300970912     c                   EXSR      letfil                                       *lettura record
016400970912x   1c                   ELSE
016500970912     C                   MOVEL     'S'           $fine                          *fine programma
016600970912e   1C                   ENDiF
016700970912     C*
016800970912     C                   ENDSR
016900970912     c*--------------------------------------------------------------------------------------------*
017000970912     c* letfil - lettura record
017100970912     c*--------------------------------------------------------------------------------------------*
017200970912     C     letfil        BEGSR
017300970912     C*
017400970912     C* Legge fino a:
017500970912     C*       -fine file
017600970912     C*       -trovato record valido
017700970912     c                   MOVEL     'N'           $recok                         *record non valido
017800970912do  1c                   DOW       NOT *in99 AND
017900970912     c                             $recok='N'
018000970912     C                   READ      azcpr01l                               99
018100970912if  2c                   IF        *in99                                        *fine file
018200970912     c                   MOVEL     'S'           $fine                          *fine programma
018300970912x   2c                   ELSE                                                   *record trovato
018400970912     c                   EXSR      chkcpr                                       *controlla record
018500970912e   2c                   ENDIF
018600970912e   1c                   ENDDO
018700970912     C*
018800970912     C                   ENDSR
018900970912     c*--------------------------------------------------------------------------------------------*
019000970912     C* chkcpr - controlla validità record -azcpr-
019100970912     c*--------------------------------------------------------------------------------------------*
019200970912     C     chkcpr        BEGSR
019300970912     C*
019400970912     c                   MOVEL     'S'           $recok                         *record valido
019500970912     C*
019600970912     C* record annullato, esclude
019700970912if  1c                   IF        f_cpratb <> *blanks
019800970912     c                   MOVEL     'N'           $recok                         *record non valido
019900970912e   1c                   ENDIF
020000970912     C*
020100970912     C* record con decorrenza minore della data richiesta, esclude
020200970912     c                   MOVEL     p54dri        n8
020300970912if  1c                   IF        f_cprdsc < n8
020400970912     c                   MOVEL     'N'           $recok                         *record non valido
020500970912e   1c                   ENDIF
020600970912     C*
020700970912     c                   ENDSR
020800970912     c*--------------------------------------------------------------------------------------------*
020900970912     C* cpycpr - copia le versioni
021000970912     c*--------------------------------------------------------------------------------------------*
021100970912     C     cpycpr        BEGSR
021200970912     C*
021300970912     c                   MOVEL     dsazcprf      dsazcprc                       *muove formato rec
021400011022     c                   IF        p54azz = 'S'
021500011022     c                   Z-ADD     20391231      c_cprdsc
021600011022     c                   MOVEL(P)  'DISK C'      c_cprdes
021700011022     c                   ENDIF
021800971113     c                   Z-ADD     99999999      c_cprdtr                       *deve essere pieno
021900010918     c                   WRITE     azcprc                                       *scrive
022000970912     c*
022100970912     c                   ENDSR
022200970912     c*--------------------------------------------------------------------------------------------*
022300970912     C* cpycpc - copia i cap
022400970912     c*--------------------------------------------------------------------------------------------*
022500970912     C     cpycpc        BEGSR
022600970912     c*
022700970912     c                   Z-ADD     f_cprver      kccver                         *versione
022800970912     c     keycpc        SETLL     azcpc01l
022900970912     C     keycpc        READE     azcpc01l                               98
023000970912do  1c                   DOW       NOT *in98
023100970912     c                   EXSR      chkcpc                                       *controlla record
023200970912if  2c                   IF        $recok = 'S'                                 *record valido
023300970912     c                   MOVEL     dsazcpcf      dsazcpcc                       *muove formato rec
023400970912     c* limite oltre
023500970912if  3c                   IF        f_cpclkg = *zeros
023600970912     c                   Z-ADD     999999,9      c_cpclkg
023700970912e   3c                   ENDIF
023800970912if  3c                   IF        f_cpclmc = *zeros
023900970912     c                   Z-ADD     99,999        c_cpclmc
024000970912e   3c                   ENDIF
024100970912if  3c                   IF        f_cpclol = *zeros
024200970912     c                   Z-ADD     f_cpclna      c_cpclol
024300970912     c                   Z-ADD     f_cpcznc      c_cpczol
024400970912e   3c                   ENDIF
024500970912     C*
024600030129if  3c                   IF        f_cpcfi2 = *blanks
024700030129     c                   MOVEL     '99'          c_cpcfi2                       *deve essere pieno
024800030129     c                   ELSE
024900030129     c                   MOVEL     f_cpcfi2      c_cpcfi2                       *deve essere pieno
025000030129e   3c                   ENDIF
025100010918     c*
025200010918     c* Se richiesto azzeramento linee testo qui e procedo
025300010918     c                   IF        p54azz = 'S'
025400010921     c                   IF        c_cpclna <> *zeros
025500010921     c                   EVAL      c_cpclna = 789
025600010921     c                   ENDIF
025700010921     c                   EVAL      c_cpcznc = *zeros
025800010921     c                   IF        c_cpclol <> *zeros
025900010921     c                   EVAL      c_cpclol = 789
026000010921     c                   ENDIF
026100010918     c                   EVAL      c_cpczol = *zeros
026200010921     c                   IF        c_cpclos <> *zeros
026300010921     c                   EVAL      c_cpclos = 789
026400010921     c                   ENDIF
026500010918     c                   EVAL      c_cpczos = *zeros
026600010921     c                   IF        c_cpclo2 <> *zeros
026700010921     c                   EVAL      c_cpclo2 = 789
026800010921     c                   ENDIF
026900010918     c                   EVAL      c_cpczo2 = *zeros
027000010918     c                   EVAL      c_cpczfd = *zeros
027100010918     c                   ENDIF
027200010918     c*
027300010918     c                   WRITE     azcpcc                                       *scrive
027400970912     C*
027500970912e   2c                   ENDIF
027600970912     C     keycpc        READE     azcpc01l                               98
027700970912e   1c                   ENDDO
027800970912     c*
027900970912     c                   ENDSR
028000970912     c*--------------------------------------------------------------------------------------------*
028100970912     C* chkcpc - controlla validità record -azcpc-
028200970912     c*--------------------------------------------------------------------------------------------*
028300970912     C     chkcpc        BEGSR
028400970912     C*
028500970912     c                   MOVEL     'S'           $recok                         *record valido
028600970912     C*
028700970912     C* record annullato, esclude
028800061019if  1c                   IF        f_cpcatb <> *blanks
028900970912     c                   MOVEL     'N'           $recok                         *record non valido
029000970912e   1c                   ENDIF
029100061020     c*
029200061020     c                   IF        p54obs = 'S' AND
029300061020     c                             f_cpcfi1 <> *blanks
029400061020     c                   MOVEL     'N'           $recok                         *record non valido
029500061020e   1c                   ENDIF
029600970912     C*
029700970912     c                   ENDSR
029800970912     c*--------------------------------------------------------------------------------------------*
029900970912     C* cpycpls - copia le località e i sinonimi
030000970912     c*--------------------------------------------------------------------------------------------*
030100970912     C     cpycpls       BEGSR
030200970912     c*
030300970912     c                   Z-ADD     f_cprver      kclver                         *versione
030400970912     c     keycpl        SETLL     azcpl01l
030500970912     C     keycpl        READE     azcpl01l                               98
030600970912do  1c                   DOW       NOT *in98
030700970912     c                   EXSR      chkcpl                                       *controlla record
030800970912if  2c                   IF        $recok = 'S'                                 *record valido
030900970912     c                   MOVEL     dsazcplf      dsazcplc                       *muove formato rec
031000970912     c* limite oltre
031100970912if  3c                   IF        f_cpllkg = *zeros
031200970912     c                   Z-ADD     999999,9      c_cpllkg
031300970912e   3c                   ENDIF
031400970912if  3c                   IF        f_cpllmc = *zeros
031500970912     c                   Z-ADD     99,999        c_cpllmc
031600970912e   3c                   ENDIF
031700970912if  3c                   IF        f_cpllol = *zeros
031800970912     c                   Z-ADD     f_cpllna      c_cpllol
031900970912     c                   Z-ADD     f_cplznc      c_cplzol
032000970912e   3c                   ENDIF
032100970912     C*
032200030129if  3c                   IF        f_cplfi2 = *blanks
032300030129     c                   MOVEL     '99'          c_cplfi2                       *deve essere pieno
032400030129     c                   ELSE
032500030129     c                   MOVEL     f_cplfi2      c_cplfi2                       *deve essere pieno
032600030129e   3c                   ENDIF
032700010918     c*
032800010921     c* Se richiesto azzeramento linee testo qui e procedo
032900010918     c                   IF        p54azz = 'S'
033000010921     c                   IF        c_cpllna <> *zeros
033100010921     c                   EVAL      c_cpllna = 789
033200010921     c                   ENDIF
033300010921     c                   EVAL      c_cplznc = *zeros
033400010921     c                   IF        c_cpllol <> *zeros
033500010921     c                   EVAL      c_cpllol = 789
033600010921     c                   ENDIF
033700010921     c                   EVAL      c_cplzol = *zeros
033800010921     c                   IF        c_cpllos <> *zeros
033900010921     c                   EVAL      c_cpllos = 789
034000010921     c                   ENDIF
034100010921     c                   EVAL      c_cplzos = *zeros
034200010921     c                   IF        c_cpllo2 <> *zeros
034300010921     c                   EVAL      c_cpllo2 = 789
034400010921     c                   ENDIF
034500010921     c                   EVAL      c_cplzo2 = *zeros
034600010921     c                   EVAL      c_cplzfd = *zeros
034700010918     c                   ENDIF
034800010918     c*
034900010918     c                   WRITE     azcplc                                       *scrive
035000970912     C*
035100970912     C* copia i sinonimi della località
035200970912     c                   EXSR      cpycps
035300970912e   2c                   ENDIF
035400970912     C     keycpl        READE     azcpl01l                               98
035500970912e   1c                   ENDDO
035600970912     c*
035700970912     c                   ENDSR
035800970912     c*--------------------------------------------------------------------------------------------*
035900970912     C* chkcpl - controlla validità record -azcpl-
036000970912     c*--------------------------------------------------------------------------------------------*
036100970912     C     chkcpl        BEGSR
036200970912     C*
036300970912     c                   MOVEL     'S'           $recok                         *record valido
036400970912     C*
036500980520     C* record annullato o obsoleto, esclude
036600061019if  1c                   IF        f_cplatb <> *blanks
036700970912     c                   MOVEL     'N'           $recok                         *record non valido
036800970912e   1c                   ENDIF
036900061020     c*
037000061020     c                   IF        p54obs = 'S' AND
037100061020     c                             f_cplfi1 <> *blanks
037200061020     c                   MOVEL     'N'           $recok                         *record non valido
037300061020e   1c                   ENDIF
037400970912     C*
037500970912     c                   ENDSR
037600970912     c*--------------------------------------------------------------------------------------------*
037700970912     C* cpycps - copia i sinonimi della località
037800970912     c*--------------------------------------------------------------------------------------------*
037900970912     C     cpycps        BEGSR
038000970912     c*
038100970912     c                   Z-ADD     f_cplver      kcsver                         *versione
038200970912     c                   MOVEL     f_cplnar      kcsnar                         *nazione
038300970912     c                   MOVEL     f_cplcap      kcscap                         *cap
038400970912     c                   MOVEL     f_cplloc      kcsloc                         *località
038500970912     c     keycps        SETLL     azcps01l
038600970912     C     keycps        READE     azcps01l                               98
038700970912do  1c                   DOW       NOT *in98
038800970912     c                   EXSR      chkcps                                       *controlla record
038900970912if  2c                   IF        $recok = 'S'                                 *record valido
039000970912     c                   MOVEL     dsazcpsf      dsazcpsc                       *muove formato rec
039100970912     C*
039200971113     c                   Z-ADD     99999999      c_cpsdtr                       *deve essere pieno
039300970912     c                   WRITE     azcpsc                                       *scrive
039400970912     C*
039500970912e   2c                   ENDIF
039600970912     C     keycps        READE     azcps01l                               98
039700970912e   1c                   ENDDO
039800970912     c*
039900970912     c                   ENDSR
040000970912     c*--------------------------------------------------------------------------------------------*
040100970912     C* chkcps - controlla validità record -azcps-
040200970912     c*--------------------------------------------------------------------------------------------*
040300970912     C     chkcps        BEGSR
040400970912     C*
040500970912     c                   MOVEL     'S'           $recok                         *record valido
040600970912     C*
040700970912     C* record annullato, esclude
040800980520if  1c                   IF        f_cpsatb <> *blanks
040900970912     c                   MOVEL     'N'           $recok                         *record non valido
041000970912e   1c                   ENDIF
041100970912     C*
041200970912     c                   ENDSR
041300970912     c*--------------------------------------------------------------------------------------------*
041400970912     C* cpycpe - copia le eccezioni linee/zone
041500970912     c*--------------------------------------------------------------------------------------------*
041600970912     C     cpycpe        BEGSR
041700970912     c*
041800970912     c                   Z-ADD     f_cprver      kcever                         *versione
041900970912     c     keycpe        SETLL     azcpe01l
042000970912     C     keycpe        READE     azcpe01l                               98
042100970912do  1c                   DOW       NOT *in98
042200970912     c                   EXSR      chkcpe                                       *controlla record
042300970912if  2c                   IF        $recok = 'S'                                 *record valido
042400970912     c                   MOVEL     dsazcpef      dsazcpec                       *muove formato rec
042500970912     C*
042600971113     c                   Z-ADD     99999999      c_cpedtr                       *deve essere pieno
042700010918     c*
042800010921     c* Se richiesto azzeramento linee testo qui e procedo
042900010918     c                   IF        p54azz = 'S'
043000010921     c                   IF        c_cpelna <> *zeros
043100010921     c                   EVAL      c_cpelna = 789
043200010921     c                   ENDIF
043300010921     c                   EVAL      c_cpeznc = *zeros
043400010921     c                   IF        c_cpelol <> *zeros
043500010921     c                   EVAL      c_cpelol = 789
043600010921     c                   ENDIF
043700010921     c                   EVAL      c_cpezol = *zeros
043800010921     c                   IF        c_cpelos <> *zeros
043900010921     c                   EVAL      c_cpelos = 789
044000010921     c                   ENDIF
044100010921     c                   EVAL      c_cpezos = *zeros
044200010921     c                   IF        c_cpelo2 <> *zeros
044300010921     c                   EVAL      c_cpelo2 = 789
044400010921     c                   ENDIF
044500010921     c                   EVAL      c_cpezo2 = *zeros
044600010918     c                   ENDIF
044700010918     C*
044800010918     c                   WRITE     azcpec                                       *scrive
044900970912     C*
045000970912e   2c                   ENDIF
045100970912     C     keycpe        READE     azcpe01l                               98
045200970912e   1c                   ENDDO
045300970912     c*
045400970912     c                   ENDSR
045500970912     c*--------------------------------------------------------------------------------------------*
045600970912     C* chkcpe - controlla validità record -azcpe-
045700970912     c*--------------------------------------------------------------------------------------------*
045800970912     C     chkcpe        BEGSR
045900970912     C*
046000970912     c                   MOVEL     'S'           $recok                         *record valido
046100970912     C*
046200970912     C* record annullato, esclude
046300970912if  1c                   IF        f_cpeatb <> *blanks
046400970912     c                   MOVEL     'N'           $recok                         *record non valido
046500970912e   1c                   ENDIF
046600970912     C*
046700970912     c                   ENDSR
046800971003     c*--------------------------------------------------------------------------------------------*
046900971003     C* cpycae - copia le eccezioni terminal
047000971003     c*--------------------------------------------------------------------------------------------*
047100971003     C     cpycae        BEGSR
047200971003     c*
047300971003     c     *loval        SETLL     azcae01l
047400971003     C                   READ      azcae01l                               98
047500971003do  1c                   DOW       NOT *in98
047600971003     c                   EXSR      chkcae                                       *controlla record
047700971003if  2c                   IF        $recok = 'S'                                 *record valido
047800971003     c                   MOVEL     dsazcaef      dsazcaec                       *muove formato rec
047900971003     C*
048000971113     c                   Z-ADD     99999999      c_caedtr                       *deve essere pieno
048100971003     c                   WRITE     azcaec                                       *scrive
048200971003     C*
048300971003e   2c                   ENDIF
048400971003     C                   READ      azcae01l                               98
048500971003e   1c                   ENDDO
048600010921     c* dopo aver inserito i record partenza (P) inserisco un unico record arrivo (A) "truccato"
048700011211     c* solo per cappario con 789                                           rrivo (A) "truccato"
048800011211     c     p54azz        ifeq      'S'
048900010921     c                   MOVEL     'A'           c_caeepa
049000010921     c                   EVAL      c_caetfp = *zeros
049100010921     c                   EVAL      c_caetfa = 789
049200010921     c                   EVAL      c_caetfe = 789
049300010921     c                   Z-ADD     19900101      c_caedde
049400010921     c                   Z-ADD     20391231      c_caedsc
049500010921     c                   Z-ADD     99999999      c_caedtr                       *deve essere pieno
049600010921     c                   WRITE     azcaec                                       *scrive
049700011211     c                   endif
049800971003     c*
049900971003     c                   ENDSR
050000971003     c*--------------------------------------------------------------------------------------------*
050100971003     C* chkcae - controlla validità record -azcae-
050200971003     c*--------------------------------------------------------------------------------------------*
050300971003     C     chkcae        BEGSR
050400971003     C*
050500971003     c                   MOVEL     'S'           $recok                         *record valido
050600971003     C*
050700971003     C* record annullato, esclude
050800971003if  1c                   IF        f_caeatb <> *blanks
050900971003     c                   MOVEL     'N'           $recok                         *record non valido
051000971003e   1c                   ENDIF
051100971003     C*
051200971003     C* record non in decorrenza, esclude
051300971003     c                   MOVEL     p54dri        n8
051400971008if  1c                   IF        f_caedsc < n8
051500971003     c                   MOVEL     'N'           $recok                         *record non valido
051600971003e   1c                   ENDIF
051700010921     C*
051800011211     C* record non di tipo partenza (P) esclude   se con azzeramento
051900011211if  1c                   IF        f_caeepa <> 'P' and
052000011211if  1c                             p54azz = 'S'
052100010921     c                   MOVEL     'N'           $recok                         *record non valido
052200010921e   1c                   ENDIF
052300971003     C*
052400971003     c                   ENDSR
052500970912     c*--------------------------------------------------------------------------------------------*
052600970912     C* imppar - controlla e imposta i parametri di entrata
052700970912     c*--------------------------------------------------------------------------------------------*
052800970912     C     imppar        BEGSR
052900970912     C*
053000970912if  1c                   IF        p54dri=*blanks                               parametri no passati
053100970912     c                   MOVEL     datcor        p54dri                         *data riferimento
053200970912e   1c                   ENDIF
053300970912     C*
053400970912     C                   ENDSR
053500970912     c*--------------------------------------------------------------------------------------------*
053600970912     C* *inzsr - operazioni iniziali
053700970912     c*--------------------------------------------------------------------------------------------*
053800970912     C     *inzsr        BEGSR
053900970912     c*
054000970912     c* ricevimento parametri
054100970912     c     *ENTRY        PLIST
054200970912     c                   PARM                    kpjba
054300970912     c                   MOVEL     kpjbu         dspar54
054400970912     C*
054500970912     C* chiavi di lettura
054600970912     C     keycpc        KLIST                                                  di azcpc01l
054700970912     C                   KFLD                    kccver                         -versione
054800970912     C     keycpl        KLIST                                                  di azcpl01l
054900970912     C                   KFLD                    kclver                         -versione
055000970912     C     keycps        KLIST                                                  di azcps01l
055100970912     C                   KFLD                    kcsver                         -versione
055200970912     C                   KFLD                    kcsnar                         -nazione
055300970912     C                   KFLD                    kcscap                         -cap
055400970912     C                   KFLD                    kcsloc                         -località
055500970912     C     keycpe        KLIST                                                  di azcpe01l
055600970912     C                   KFLD                    kcever                         -versione
055700970912     C*
055800970912     C* reperimento dati utente
055900020906     c                   Z-ADD     1             codut
056000970912     C                   CALL      'X§PARUT'
056100970912     C                   PARM                    ut§dse
056200970912     C                   MOVEL     rec80         cncr80
056300970912     C*
056400970912     C* reperimento data corrente
056500970912     C                   TIME                    n14                            *ora (6)+ data (8)
056600970912     C                   MOVE      n14           n8                             *data (8) in g/m/a
056700970912     C                   Z-ADD     n8            g08dat
056800970912     C                   Z-ADD     *zeros        g08inv
056900970912     C                   MOVEL     '0'           g08err
057000970912     C                   CALL      'XSRDA8'
057100970912     C                   PARM                    wlbda8
057200970912     C                   Z-ADD     g08inv        datcor                         *data corrente a/m/g
057300970912     C*
057400970912     C                   ENDSR
