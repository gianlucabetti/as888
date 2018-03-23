000100000000     H*------------------------------------------------------------------------*
000200060901     H* CONTROLLO ASSORBIMENTO X LNA
000300000000     H*------------------------------------------------------------------------*
000400000000     H DECEDIT('0,') DATEDIT(*DMY.)
000500000914     H*------------------------------------------------------------------------*
000600000000     FAZORG01L  IF   E           K DISK
000700001016     FTABEL00F  IF   E           K DISK
000800061030     FAZCPL01L  IF   E           K DISK
000900060901     FTIALA01L  IF   E           K DISK
001000061027     FTIALA02L  IF   E           K DISK    rename(tiala000:tiala002)
001100060901     FTIST48P   O    E             PRINTER OFLIND(*IN01)
001200000000     D*------------------------------------------------------------------------*
001300000000     D* SCHIERE
001400000000     D*------------------------------------------------------------------------*
001500000000     D*-------------------
001600060904     D* FILIALI E RELATIVE DESCRIZIONI
001700000000     D*-------------------
001800000915     D FIL             S              3  0 DIM(500)                             *FILIALE
001900001106     D DFIL            S             20    DIM(500)                             *DESCRIZIONE FILIALE
002000000000     D*------------------------------------------------------------------------*
002100000000     D* INPUT
002200000000     D*------------------------------------------------------------------------*
002300000000     D*-------------------
002400000000     D* ARCHITETTURA
002500000000     D*-------------------
002600000000     D KPJBA         E DS
002700060901     D*-------------------
002800060901     D* DS PARAMETRI DI LANCIO PGM
002900060901     D*-------------------
003000060901     D PARAM           DS                  INZ
003100060901     D  PAROPE                        1
003200060901     D  PARDAI                        8  0
003300060901     D  PARDAF                        8  0
003400060901     D  PARIAD                        8  0
003500060901     D  PARFAD                        8  0
003600060901     D  PARGIO                        3  0
003700060901     D  PARDET                        1
003800061027     D  PARORD                        1
003900060901     D  PARKG1DA                      7  1
004000060901     D  PARKG1A                       7  1
004100060901     D  PARKG2DA                      7  1
004200060901     D  PARKG2A                       7  1
004300060901     D  PARKG3DA                      7  1
004400060901     D  PARKG3A                       7  1
004500060901     D  PARKG4DA                      7  1
004600060901     D  PARKG4A                       7  1
004700060901     D  PARDIS                        1
004800060901     D  PARARE                        3  0
004900060901     D  PARFIL                        3  0
005000060901     D  PARCAD                        2
005100060901     D  PARCAA                        2
005200061030     D*------------------
005300061030     D* DS "XSRDA8" - CONTROLLA DATA (8)
005400061030     D*------------------
005500061030     D WLBDA8          DS                  INZ
005600061030     D  G08DAT                 1      8  0
005700061030     D  G08INV                 9     16  0
005800061030     D  G08ERR                17     17
005900061030     D  G08TGI                18     22  0
006000000914     D*------------------------------------------------------------------------*
006100060904     D* VARIABILI DI WRK
006200000914     D*------------------------------------------------------------------------*
006300060904     D tialads_key   e ds                  extname(tiala00f) prefix(k_) inz
006400060904     D depspv          s              7S 1 inz
006500060904     D*-------------------
006600060904     D* Variabili riferite al data base
006700060904     D*-------------------
006800060901     D depdiv          S                   LIKE(aladiv)
006900060901     D depare          S                   LIKE(alaare)
007000060904     D depfil          S                   LIKE(alalna)
007100060904     D depznc          S                   LIKE(alaznc)
007200060904     D depnaz          S                   LIKE(alanaz)
007300060901     D depprv          S                   LIKE(alaprv)
007400060901     D depcap          S                   LIKE(alacap)
007500060901     D deploc          S                   LIKE(alaloc)
007600061030     D depfi1          S                   LIKE(cplfi1)
007700000000     C*------------------------------------------------------------------------*
007800000000     C* MAIN LINE
007900000000     C*------------------------------------------------------------------------*
008000060904     C*
008100060904     C* Caricamento organigramma
008200060904     C                   exsr      carorg
008300000000     C*
008400060904     C* Eseguo stampa
008500001106     C                   exsr      stampa
008600000914     C*
008700000914     C                   SETON                                        LR
008800001106     C*------------------------------------------------------------------------*
008900001106     C* STAMPA - Routine di stampa
009000001106     C*------------------------------------------------------------------------*
009100001106     C     STAMPA        BEGSR
009200001207     C*
009300001207     C* Inizializzo la testata
009400060901     C                   clear                   ST48TE
009500001109     C*
009600001109     C* Imposto i parametri in testata
009700060901     C                   z-add     PARKG1A       STPPP1A
009800060901     C                   z-add     PARKG1DA      STPPP1D
009900060901     C                   z-add     PARKG2A       STPPP2A
010000060901     C                   z-add     PARKG2DA      STPPP2D
010100060901     C                   z-add     PARKG3A       STPPP3A
010200060901     C                   z-add     PARKG3DA      STPPP3D
010300060901     C                   z-add     PARKG4A       STPPP4A
010400060901     C                   z-add     PARKG4DA      STPPP4D
010500060904     C                   z-add     PARIAD        STPPDAI
010600060904     C                   z-add     PARFAD        STPPDAF
010700060904     C                   z-add     PARGIO        STPPGIO
010800020312     C*
010900001109     C                   if        PARCAD = *blanks
011000060904     C                   movel     *zeros        STPPCAD
011100001109     C                   else
011200060904     C                   movel     PARCAD        STPPCAD
011300001109     C                   endif
011400060904     C*
011500001109     C                   if        PARCAA = *blanks
011600060904     C                   movel     *all'9'       STPPCAA
011700001109     C                   else
011800060904     C                   movel     PARCAA        STPPCAA
011900001109     C                   endif
012000061030     C
012100061030     C* Indicazione tipo ordinamento stampa
012200061030     C                   if        PARORD = 'C'
012300061030     C                   eval      STPTIT = '          (Ordinamento per CAP)'
012400061030     C                   else
012500061030     C                   eval      STPTIT = '         (Ordinamento per ZONA)'
012600061030     C                   endif
012700060904     C
012800070410     C* Gestire valori e decodifiche DIVISIONE / AREA / FIL.
012900060904     C                   movel     PARDIS        STPPDIS
013000060904     C                   if        STPPDIS = *blanks
013100060904     C                   eval      STPPDISD = 'Tutti'
013200060904     C                   else
013300060904     C                   movel     STPPDIS       depdiv
013400060904     C                   exsr      DECDIV
013500060904     C                   eval      STPPDISD = desdiv
013600060904     C                   endif
013700060904     C*
013800060904     C                   z-add     PARARE        STPPARE
013900060904     C                   if        STPPARE = *zeros
014000060904     C                   eval      STPPARED = 'Tutte'
014100060904     C                   else
014200060904     C                   movel     STPPARE       depare
014300060904     C                   exsr      DECARE
014400060904     C                   eval      STPPARED = desare
014500060904     C                   endif
014600060904     C*
014700060904     C                   z-add     PARFIL        STPPFIL
014800060904     C                   if        STPPFIL = *zeros
014900060904     C                   eval      STPPFILD = 'Tutti'
015000060904     C                   else
015100060904     C                   movel     STPPFIL       depfil
015200060904     C                   exsr      DECFIL
015300060904     C                   eval      STPPFILD = desfil
015400060904     C                   endif
015500060904     C*
015600060904     C* Inizializzo le variabili d deposito x le rotture d codice in stampa
015700060904     C                   clear                   depdiv
015800060904     C                   clear                   depare
015900060904     C                   clear                   depfil
016000060904     C                   clear                   depznc
016100060904     C                   clear                   depnaz
016200060904     C                   clear                   depprv
016300060904     C                   clear                   depcap
016400060904     C                   clear                   deploc
016500061030     C                   clear                   depfi1
016600061027     C*
016700061027     C* Verifico il tipo d ordinamento richiesto
016800061027     C                   IF        PARORD = 'C'
016900061027     C                   SETON                                        10
017000061027     C                   ELSE
017100061027     C                   SETOFF                                       10
017200061027     C                   ENDIF
017300070117     C*
017400070117     C* Posizionamento sul file di wrk e prima lettura
017500070117     C                   exsr      setfil
017600061027     C*
017700061027     C* Elaborazione x ordionamento richiesto
017800061027     C   10              exsr      elaOrd_CL
017900061027     C  N10              exsr      elaOrd_ZCL
018000001106     C*
018100001106     C* Stampa il totale GENERALE
018200001109     C                   exsr      stagen
018300001106     C*
018400001106     C                   ENDSR
018500061027     C*------------------------------------------------------------------------*
018600061027     C* ELAORD_CL - ELABORAZIONE ORDINATA X CAP/LOC
018700061027     C*------------------------------------------------------------------------*
018800061027     C     ELAORD_CL     BEGSR
018900061027     C*
019000061027     C* Ciclo fino a fine file di wrk
019100061027     C     finl00        doweq     '0'
019200061027     C*
019300061027     C* Operazioni per nuova DIVISIONE
019400061027     C                   exsr      newdiv
019500061027     C*
019600061027     C* Ciclo fino a rottura divisione
019700061027     C     finl00        doweq     '0'
019800061027     C     aladiv        andeq     depdiv
019900061027     C*
020000061027     C* Operazioni per nuova AREA
020100061027     C                   exsr      neware
020200061027     C*
020300061027     C* Ciclo fino a rottura di AREA
020400061027     C     finl00        doweq     '0'
020500061027     C     aladiv        andeq     depdiv
020600061027     C     alaare        andeq     depare
020700061027     C*
020800061027     C* Operazioni per nuova FILIALE
020900061027     C                   exsr      newfil
021000061027     C*
021100061027     C* Ciclo fino a rottura di FILIALE
021200061027     C     finl00        doweq     '0'
021300061027     C     aladiv        andeq     depdiv
021400061027     C     alaare        andeq     depare
021500061027     C     alalna        andeq     depfil
021600061027     C*
021700061027     C* Operazioni per nuovo PRV/CAP
021800061027     C                   exsr      newcap
021900061027     C*
022000061027     C* Ciclo fino a rottura di PRV/CAP
022100061027     C     finl00        doweq     '0'
022200061027     C     aladiv        andeq     depdiv
022300061027     C     alaare        andeq     depare
022400061027     C     alalna        andeq     depfil
022500061027     C     alanaz        andeq     depnaz
022600061027     C     alaprv        andeq     depprv
022700061027     C     alacap        andeq     depcap
022800061027     C*
022900061027     C* Operazioni per nuovo PRV/CAP/LOC
023000061027     C                   exsr      newdet_c
023100061027     C*
023200061027     C* Ciclo fino a rottura di PRV/CAP/LOC
023300061027     C     finl00        doweq     '0'
023400061027     C     aladiv        andeq     depdiv
023500061027     C     alaare        andeq     depare
023600061027     C     alalna        andeq     depfil
023700061027     C     alanaz        andeq     depnaz
023800061027     C     alaprv        andeq     depprv
023900061027     C     alacap        andeq     depcap
024000061027     C     alaloc        andeq     deploc
024100061027     C*
024200061027     C* Memorizza i dati di DETTAGLIO
024300061027     C     WRECOK        ifeq      'S'
024400061027     C                   exsr      memspe
024500061027     C                   endif
024600061027     C*
024700061027     C* Legge il record successivo
024800061027     C                   exsr      letfil
024900061027     C*
025000061027     C                   enddo
025100061027     C*
025200061027     C* Stampa i dati di DETTAGLIO
025300061027     C     PARDET        ifeq      'S'
025400061027     C                   exsr      stadet
025500061027     C                   endif
025600061027     C*
025700061027     C                   enddo
025800061027     C*
025900061027     C* Stampa il totale CAP
026000061027     C                   exsr      stacap
026100061027     C*
026200061027     C                   enddo
026300061027     C*
026400061027     C* Stampa il totale FILIALE
026500061027     C                   exsr      stafil
026600061027     C*
026700061027     C                   enddo
026800061027     C*
026900061027     C* Stampa il totale AREA
027000061027     C                   exsr      staare
027100061027     C*
027200061027     C                   enddo
027300061027     C*
027400061027     C* Stampa il totale DIVISIONE
027500061027     C                   exsr      stadiv
027600061027     C*
027700061027     C                   enddo
027800061027     C*
027900061027     C                   ENDSR
028000061027     C*------------------------------------------------------------------------*
028100061027     C* ELAORD_ZCL - ELABORAZIONE ORDINATA X ZNC/CAP/LOC
028200061027     C*------------------------------------------------------------------------*
028300061027     C     ELAORD_ZCL    BEGSR
028400061027     C*
028500061027     C* Ciclo fino a fine file di wrk
028600061027     C     finl00        doweq     '0'
028700061027     C*
028800061027     C* Operazioni per nuova DIVISIONE
028900061027     C                   exsr      newdiv
029000061027     C*
029100061027     C* Ciclo fino a rottura divisione
029200061027     C     finl00        doweq     '0'
029300061027     C     aladiv        andeq     depdiv
029400061027     C*
029500061027     C* Operazioni per nuova AREA
029600061027     C                   exsr      neware
029700061027     C*
029800061027     C* Ciclo fino a rottura di AREA
029900061027     C     finl00        doweq     '0'
030000061027     C     aladiv        andeq     depdiv
030100061027     C     alaare        andeq     depare
030200061027     C*
030300061027     C* Operazioni per nuova FILIALE
030400061027     C                   exsr      newfil
030500061027     C*
030600061027     C* Ciclo fino a rottura di FILIALE
030700061027     C     finl00        doweq     '0'
030800061027     C     aladiv        andeq     depdiv
030900061027     C     alaare        andeq     depare
031000061027     C     alalna        andeq     depfil
031100061027     C*
031200061027     C* Operazioni per nuova ZNC
031300061027     C                   exsr      newznc
031400061027     C*
031500061027     C* Ciclo fino a rottura di ZONA CONSEGNA
031600061027     C     finl00        doweq     '0'
031700061027     C     aladiv        andeq     depdiv
031800061027     C     alaare        andeq     depare
031900061027     C     alalna        andeq     depfil
032000061027     C     alaznc        andeq     depznc
032100061027     C*
032200061027     C* Operazioni per buovo PRV/CAP/LOC
032300061027     C                   exsr      newdet_z
032400061027     C*
032500061027     C* Ciclo fino a rottura di PRV/CAP/LOC
032600061027     C     finl00        doweq     '0'
032700061027     C     aladiv        andeq     depdiv
032800061027     C     alaare        andeq     depare
032900061027     C     alalna        andeq     depfil
033000061027     C     alaznc        andeq     depznc
033100061027     C     alanaz        andeq     depnaz
033200061027     C     alaprv        andeq     depprv
033300061027     C     alacap        andeq     depcap
033400061027     C     alaloc        andeq     deploc
033500061027     C*
033600061027     C* Memorizza i dati di DETTAGLIO
033700061027     C     WRECOK        ifeq      'S'
033800061027     C                   exsr      memspe
033900061027     C                   endif
034000061027     C*
034100061027     C* Legge il record successivo
034200061027     C                   exsr      letfil
034300061027     C*
034400061027     C                   enddo
034500061027     C*
034600061027     C* Stampa i dati di DETTAGLIO
034700061027     C     PARDET        ifeq      'S'
034800061027     C                   exsr      stadet
034900061027     C                   endif
035000061027     C*
035100061027     C                   enddo
035200061027     C*
035300061027     C* Stampa il totale ZONA CONSEGNA
035400061027     C                   exsr      staznc
035500061027     C*
035600061027     C                   enddo
035700061027     C*
035800061027     C* Stampa il totale FILIALE
035900061027     C                   exsr      stafil
036000061027     C*
036100061027     C                   enddo
036200061027     C*
036300061027     C* Stampa il totale AREA
036400061027     C                   exsr      staare
036500061027     C*
036600061027     C                   enddo
036700061027     C*
036800061027     C* Stampa il totale DIVISIONE
036900061027     C                   exsr      stadiv
037000061027     C*
037100061027     C                   enddo
037200061027     C*
037300061027     C                   ENDSR
037400001106     C*------------------------------------------------------------------------*
037500001106     C* SETFIL - POSIZIONAMENTO E PRIMA LETTURA DEL FILE
037600001106     C*------------------------------------------------------------------------*
037700001106     C     SETFIL        BEGSR
037800001106     C*
037900001106     C* Posizionamento
038000070118     C                   movel     PARDIS        k_aladiv
038100070118     C                   z-add     PARARE        k_alaare
038200070118     C                   z-add     PARFIL        k_alalna
038300060904     C                   movel     *blanks       k_alanaz
038400060904     C                   movel     *blanks       k_alaprv
038500060904     C                   movel     *blanks       k_alacap
038600060904     C                   movel     *blanks       k_alaloc
038700060904     C                   z-add     *zeros        k_alaznc
038800060904     C                   movel     *blanks       k_alaspv
038900001113     C*
039000061027     C   10KEYala01_c    setll     tiala01l                           99
039100061027     C  N10KEYala02_c    setll     tiala02l                           99
039200001106     C*
039300001106     C     *in99         ifeq      '1'
039400001106     C                   movel     '1'           finl00
039500001106     C                   else
039600001106     C                   exsr      letfil
039700001106     C                   endif
039800001106     C*
039900001106     C                   ENDSR
040000001106     C*------------------------------------------------------------------------*
040100001106     C* LETFIL - LETTURA PROSSIMO RECORD
040200001106     C*------------------------------------------------------------------------*
040300001106     C     LETFIL        BEGSR
040400001106     C*
040500001106     C* LEGGO FINO A:
040600001106     C*     - FINE FILE O SUPERATO LIMITE FINALE
040700001106     C*     - TROVATO RECORD VALIDO
040800001109     C                   movel     'N'           WRECOK
040900001106     C*
041000001106     C* Letture successive alla prima
041100061027     C   10              read      tiala01l                               99
041200061027     C  N10              read      tiala02l                               99
041300001106     C*
041400001106     C     *in99         ifeq      '1'                                          *FINE FILE
041500001106     C                   movel     '1'           finl00
041600001106     C                   else
041700001106     C                   exsr      chkrec                                       *CONTR.VALIDITA REC.
041800001106     C                   endif
041900001106     C*
042000001106     C                   ENDSR
042100001106     C*------------------------------------------------------------------------*
042200001114     C* CHKREC - Routine di controllo validità record in stampa
042300001106     C*------------------------------------------------------------------------*
042400060904     C     CHKREC        BEGSR
042500001106     C*
042600060904     C                   movel     'S'           WRECOK            1
042700060904     C*--------------------
042800060904     C* CONTROLLO DISTRETTO
042900060904     C*--------------------
043000060904     C                   if        WRECOK = 'S'
043100060904     C                   if        PARDIS <> *blanks
043200060904     C                   if        aladiv <> PARDIS
043300060904     C                   movel     'N'           WRECOK
043400060904     C                   endif
043500060904     C                   if        aladiv  > PARDIS
043600060904     C                   movel     '1'           FINL00
043700060904     C                   endif
043800060904     C                   endif
043900060904     C                   endif
044000060904     C*--------------------
044100060904     C* CONTROLLO AREA
044200060904     C*--------------------
044300060904     C                   if        WRECOK = 'S'
044400060904     C                   if        PARARE <> *zeros
044500060904     C                   if        alaare <> PARARE
044600060904     C                   movel     'N'           WRECOK
044700060904     C                   endif
044800060904     C                   if        alaare  > PARARE
044900060904     C                   movel     '1'           FINL00
045000060904     C                   endif
045100060904     C                   endif
045200060904     C                   endif
045300060904     C*--------------------
045400070410     C* CONTROLLO FIL.
045500060904     C*--------------------
045600060904     C                   if        WRECOK = 'S'
045700060904     C                   if        PARFIL <> *zeros
045800060904     C                   if        alalna <> PARFIL
045900060904     C                   movel     'N'           WRECOK
046000060904     C                   endif
046100060904     C                   if        alalna  > PARFIL
046200060904     C                   movel     '1'           FINL00
046300060904     C                   endif
046400060904     C                   endif
046500060904     C                   endif
046600001106     C*--------------------
046700060904     C* CONTROLLO CAP "DA"/"A"
046800001106     C*--------------------
046900001106     C                   if        WRECOK = 'S'
047000001109     C                   if        PARCAD > *blanks
047100060901     C                   if        %subst(alacap:1:2) >= PARCAD
047200001106     C                   else
047300001106     C                   movel     'N'           WRECOK
047400001106     C                   endif
047500001106     C                   endif
047600001109     C                   if        PARCAA > *blanks
047700060901     C                   if        %subst(alacap:1:2) <= PARCAA
047800001109     C                   else
047900001109     C                   movel     'N'           WRECOK
048000001109     C                   endif
048100001109     C                   endif
048200001106     C                   endif
048300001106     C*
048400001106     C                   ENDSR
048500001106     C*------------------------------------------------------------------------*
048600001106     C* NEWDIV - OPERAZIONI PER NUOVA DIVISIONE
048700001106     C*------------------------------------------------------------------------*
048800001106     C     NEWDIV        BEGSR
048900061027     C*
049000061027     C                   SETOFF                                       20
049100001106     C*
049200001106     C* MEMORIZZA LA NUOVA DIVISIONE IN UN DEPOSITO
049300060901     C                   MOVEL     aladiv        depdiv
049400001106     C*
049500001106     C* DECODIFICA LA DIVISIONE
049600001106     C                   EXSR      DECDIV
049700001110     C*
049800001110     C* DESCRIZIONE IN TESTATA
049900001110     C                   EVAL      STPDIV='DIV  '+depdiv+' '+desdiv
050000001106     C*
050100001106     C* AZZERA I TOTALI DELLA DIVISIONE
050200001106     C                   Z-ADD     *ZEROS        TDFP1            11 1
050300001106     C                   Z-ADD     *ZEROS        TDFP2            11 1
050400001106     C                   Z-ADD     *ZEROS        TDFP3            11 1
050500001106     C                   Z-ADD     *ZEROS        TDFP4            11 1
050600060904     C                   Z-ADD     *ZEROS        TDFV1            13 3
050700060904     C                   Z-ADD     *ZEROS        TDFV2            13 3
050800060904     C                   Z-ADD     *ZEROS        TDFV3            13 3
050900060904     C                   Z-ADD     *ZEROS        TDFV4            13 3
051000001106     C                   Z-ADD     *ZEROS        TDFC1            11 0
051100001106     C                   Z-ADD     *ZEROS        TDFC2            11 0
051200001106     C                   Z-ADD     *ZEROS        TDFC3            11 0
051300001106     C                   Z-ADD     *ZEROS        TDFC4            11 0
051400001106     C                   Z-ADD     *ZEROS        TDFS1            11 0
051500001106     C                   Z-ADD     *ZEROS        TDFS2            11 0
051600001106     C                   Z-ADD     *ZEROS        TDFS3            11 0
051700001106     C                   Z-ADD     *ZEROS        TDFS4            11 0
051800001109     C                   Z-ADD     *ZEROS        TDTOS            11 0
051900001109     C                   Z-ADD     *ZEROS        TDTOC            11 0
052000001109     C                   Z-ADD     *ZEROS        TDTOP            11 1
052100001109     C                   Z-ADD     *ZEROS        TDTOV            13 3
052200001106     C*
052300001106     C                   ENDSR
052400001106     C*------------------------------------------------------------------------*
052500001106     C* NEWARE - OPERAZIONI PER NUOVA AREA
052600001106     C*------------------------------------------------------------------------*
052700001106     C     NEWARE        BEGSR
052800061027     C*
052900061027     C                   SETOFF                                       20
053000001106     C*
053100001106     C* MEMORIZZA LA NUOVA AREA IN UN DEPOSITO
053200060901     C                   Z-ADD     alaare        depare
053300001106     C*
053400001106     C* DECODIFICA L'AREA
053500001106     C                   EXSR      DECARE
053600001110     C*
053700001110     C* DESCRIZIONE IN TESTATA
053800001110     C                   EVAL      STPARE='AREA '+%editc(depare:'Z')+
053900001110     C                                    '  '+desare
054000001106     C*
054100001106     C* AZZERA I TOTALI DELL'AREA
054200001106     C                   Z-ADD     *ZEROS        TAFP1            11 1
054300001106     C                   Z-ADD     *ZEROS        TAFP2            11 1
054400001106     C                   Z-ADD     *ZEROS        TAFP3            11 1
054500001106     C                   Z-ADD     *ZEROS        TAFP4            11 1
054600001106     C                   Z-ADD     *ZEROS        TAFV1            13 3
054700001106     C                   Z-ADD     *ZEROS        TAFV2            13 3
054800001106     C                   Z-ADD     *ZEROS        TAFV3            13 3
054900001106     C                   Z-ADD     *ZEROS        TAFV4            13 3
055000001106     C                   Z-ADD     *ZEROS        TAFC1            11 0
055100001106     C                   Z-ADD     *ZEROS        TAFC2            11 0
055200001106     C                   Z-ADD     *ZEROS        TAFC3            11 0
055300001106     C                   Z-ADD     *ZEROS        TAFC4            11 0
055400001106     C                   Z-ADD     *ZEROS        TAFS1            11 0
055500001106     C                   Z-ADD     *ZEROS        TAFS2            11 0
055600001106     C                   Z-ADD     *ZEROS        TAFS3            11 0
055700001106     C                   Z-ADD     *ZEROS        TAFS4            11 0
055800001109     C                   Z-ADD     *ZEROS        TATOS            11 0
055900001109     C                   Z-ADD     *ZEROS        TATOC            11 0
056000001109     C                   Z-ADD     *ZEROS        TATOP            11 1
056100001109     C                   Z-ADD     *ZEROS        TATOV            13 3
056200001106     C*
056300001106     C                   ENDSR
056400001106     C*------------------------------------------------------------------------*
056500001106     C* NEWFIL - OPERAZIONI PER NUOVA FILIALE
056600001106     C*------------------------------------------------------------------------*
056700001106     C     NEWFIL        BEGSR
056800061027     C*
056900061027     C                   SETOFF                                       20
057000001106     C*
057100001106     C* MEMORIZZA LA NUOVA FILIALE IN UN DEPOSITO
057200060901     C                   Z-ADD     alalna        depfil
057300001106     C*
057400001106     C* DECODIFICA LA FILIALE
057500001106     C                   EXSR      DECFIL
057501070410     C*
057502070410     C* DESCRIZIONE IN TESTATA
057504070410     C                   EVAL      STPTES='FIL. '+%editc(depfil:'Z')+
057505070410     C                                    '  '+desfil
057600001106     C*
057700001106     C* AZZERA I TOTALI DELLA FILIALE
057800001106     C                   Z-ADD     *ZEROS        TFFP1            11 1
057900001106     C                   Z-ADD     *ZEROS        TFFP2            11 1
058000001106     C                   Z-ADD     *ZEROS        TFFP3            11 1
058100001106     C                   Z-ADD     *ZEROS        TFFP4            11 1
058200001106     C                   Z-ADD     *ZEROS        TFFV1            13 3
058300001106     C                   Z-ADD     *ZEROS        TFFV2            13 3
058400001106     C                   Z-ADD     *ZEROS        TFFV3            13 3
058500001106     C                   Z-ADD     *ZEROS        TFFV4            13 3
058600001106     C                   Z-ADD     *ZEROS        TFFC1            11 0
058700001106     C                   Z-ADD     *ZEROS        TFFC2            11 0
058800001106     C                   Z-ADD     *ZEROS        TFFC3            11 0
058900001106     C                   Z-ADD     *ZEROS        TFFC4            11 0
059000001106     C                   Z-ADD     *ZEROS        TFFS1            11 0
059100001106     C                   Z-ADD     *ZEROS        TFFS2            11 0
059200001106     C                   Z-ADD     *ZEROS        TFFS3            11 0
059300001106     C                   Z-ADD     *ZEROS        TFFS4            11 0
059400001109     C                   Z-ADD     *ZEROS        TFTOS            11 0
059500001109     C                   Z-ADD     *ZEROS        TFTOC            11 0
059600001109     C                   Z-ADD     *ZEROS        TFTOP            11 1
059700001109     C                   Z-ADD     *ZEROS        TFTOV            13 3
059800001106     C*
059900001106     C                   ENDSR
060000001113     C*------------------------------------------------------------------------*
060100001113     C* NEWCAP - OPERAZIONI PER NUOVO PRV/CAP
060200001113     C*------------------------------------------------------------------------*
060300001113     C     NEWCAP        BEGSR
060400061027     C*
060500061027     C                   SETOFF                                       20
060600001113     C*
060700001113     C* MEMORIZZA NUOVO DETTAGLIO
060800060901     C                   movel     alanaz        depnaz
060900060901     C                   movel     alaprv        depprv
061000060901     C                   movel     alacap        depcap
061100001113     C*
061200001113     C* AZZERA I TOTALI DEL CAP
061300001113     C                   Z-ADD     *ZEROS        TCFP1            11 1
061400001113     C                   Z-ADD     *ZEROS        TCFP2            11 1
061500001113     C                   Z-ADD     *ZEROS        TCFP3            11 1
061600001113     C                   Z-ADD     *ZEROS        TCFP4            11 1
061700001113     C                   Z-ADD     *ZEROS        TCFV1            13 3
061800001113     C                   Z-ADD     *ZEROS        TCFV2            13 3
061900001113     C                   Z-ADD     *ZEROS        TCFV3            13 3
062000001113     C                   Z-ADD     *ZEROS        TCFV4            13 3
062100001113     C                   Z-ADD     *ZEROS        TCFC1            11 0
062200001113     C                   Z-ADD     *ZEROS        TCFC2            11 0
062300001113     C                   Z-ADD     *ZEROS        TCFC3            11 0
062400001113     C                   Z-ADD     *ZEROS        TCFC4            11 0
062500001113     C                   Z-ADD     *ZEROS        TCFS1            11 0
062600001113     C                   Z-ADD     *ZEROS        TCFS2            11 0
062700001113     C                   Z-ADD     *ZEROS        TCFS3            11 0
062800001113     C                   Z-ADD     *ZEROS        TCFS4            11 0
062900001113     C                   Z-ADD     *ZEROS        TCTOS            11 0
063000001113     C                   Z-ADD     *ZEROS        TCTOC            11 0
063100001113     C                   Z-ADD     *ZEROS        TCTOP            11 1
063200001113     C                   Z-ADD     *ZEROS        TCTOV            13 3
063300001113     C*
063400001113     C                   ENDSR
063500061027     C*------------------------------------------------------------------------*
063600061027     C* NEWZNC - OPERAZIONI PER NUOVA ZONA CONSEGNA
063700061027     C*------------------------------------------------------------------------*
063800061027     C     NEWZNC        BEGSR
063900061027     C*
064000061027     C                   SETOFF                                       20
064100061027     C*
064200061027     C* MEMORIZZA NUOVO DETTAGLIO
064300061027     C                   movel     alaznc        depznc
064400061027     C*
064500061027     C* AZZERA I TOTALI DELLA ZONA CONSEGNA
064600061027     C                   Z-ADD     *ZEROS        TZFP1            11 1
064700061027     C                   Z-ADD     *ZEROS        TZFP2            11 1
064800061027     C                   Z-ADD     *ZEROS        TZFP3            11 1
064900061027     C                   Z-ADD     *ZEROS        TZFP4            11 1
065000061027     C                   Z-ADD     *ZEROS        TZFV1            13 3
065100061027     C                   Z-ADD     *ZEROS        TZFV2            13 3
065200061027     C                   Z-ADD     *ZEROS        TZFV3            13 3
065300061027     C                   Z-ADD     *ZEROS        TZFV4            13 3
065400061027     C                   Z-ADD     *ZEROS        TZFC1            11 0
065500061027     C                   Z-ADD     *ZEROS        TZFC2            11 0
065600061027     C                   Z-ADD     *ZEROS        TZFC3            11 0
065700061027     C                   Z-ADD     *ZEROS        TZFC4            11 0
065800061027     C                   Z-ADD     *ZEROS        TZFS1            11 0
065900061027     C                   Z-ADD     *ZEROS        TZFS2            11 0
066000061027     C                   Z-ADD     *ZEROS        TZFS3            11 0
066100061027     C                   Z-ADD     *ZEROS        TZFS4            11 0
066200061027     C                   Z-ADD     *ZEROS        TZTOS            11 0
066300061027     C                   Z-ADD     *ZEROS        TZTOC            11 0
066400061027     C                   Z-ADD     *ZEROS        TZTOP            11 1
066500061027     C                   Z-ADD     *ZEROS        TZTOV            13 3
066600061027     C*
066700061027     C                   ENDSR
066800001106     C*------------------------------------------------------------------------*
066900061027     C* NEWDET_C - OPERAZIONI PER NUOVO PRV/CAP/LOC - ORD X CAP
067000001106     C*------------------------------------------------------------------------*
067100061027     C     NEWDET_C      BEGSR
067200001106     C*
067300001106     C* MEMORIZZA NUOVO DETTAGLIO
067400060901     C                   movel     alaloc        deploc
067500061030     C*
067600061030     C* REPERISCO SE LOCALITA' CORRENTE OBSOLETA
067700061030     C                   eval      cplVER = o96ver
067800061030     C                   eval      cplNAR = *blanks
067900061030     C                   eval      cplCAP = alacap
068000061030     C                   eval      cplLOC = alaloc
068100061030     C     KEYcpl01_c    chain     azcpl01l
068200061030     C                   if        %found(azcpl01l)
068300061030     C                   eval      depfi1 = cplFI1
068400061030     C                   else
068500061030     C                   eval      depfi1 = *blanks
068600061030     C                   endif
068700001106     C*
068800001106     C* AZZERA I TOTALI DEL DETTAGLIO
068900001106     C                   Z-ADD     *ZEROS        TLFP1            11 1
069000001106     C                   Z-ADD     *ZEROS        TLFP2            11 1
069100001106     C                   Z-ADD     *ZEROS        TLFP3            11 1
069200001106     C                   Z-ADD     *ZEROS        TLFP4            11 1
069300001106     C                   Z-ADD     *ZEROS        TLFV1            13 3
069400001106     C                   Z-ADD     *ZEROS        TLFV2            13 3
069500001106     C                   Z-ADD     *ZEROS        TLFV3            13 3
069600001106     C                   Z-ADD     *ZEROS        TLFV4            13 3
069700001106     C                   Z-ADD     *ZEROS        TLFC1            11 0
069800001106     C                   Z-ADD     *ZEROS        TLFC2            11 0
069900001106     C                   Z-ADD     *ZEROS        TLFC3            11 0
070000001106     C                   Z-ADD     *ZEROS        TLFC4            11 0
070100001106     C                   Z-ADD     *ZEROS        TLFS1            11 0
070200001106     C                   Z-ADD     *ZEROS        TLFS2            11 0
070300001106     C                   Z-ADD     *ZEROS        TLFS3            11 0
070400001106     C                   Z-ADD     *ZEROS        TLFS4            11 0
070500001109     C                   Z-ADD     *ZEROS        TLTOS            11 0
070600001109     C                   Z-ADD     *ZEROS        TLTOC            11 0
070700001109     C                   Z-ADD     *ZEROS        TLTOP            11 1
070800001109     C                   Z-ADD     *ZEROS        TLTOV            13 3
070900001106     C*
071000001106     C                   ENDSR
071100061027     C*------------------------------------------------------------------------*
071200061027     C* NEWDET_Z - OPERAZIONI PER NUOVO PRV/CAP/LOC - ORD X ZNC
071300061027     C*------------------------------------------------------------------------*
071400061027     C     NEWDET_Z      BEGSR
071500061027     C*
071600061027     C* MEMORIZZA NUOVO DETTAGLIO
071700061027     C                   movel     alaprv        depprv
071800061027     C                   movel     alacap        depcap
071900061027     C                   movel     alaloc        deploc
072000061030     C*
072100061030     C* REPERISCO SE LOCALITA' CORRENTE OBSOLETA
072200061030     C                   eval      cplVER = o96ver
072300061030     C                   eval      cplNAR = *blanks
072400061030     C                   eval      cplCAP = alacap
072500061030     C                   eval      cplLOC = alaloc
072600061030     C     KEYcpl01_c    chain     azcpl01l
072700061030     C                   if        %found(azcpl01l)
072800061030     C                   eval      depfi1 = cplFI1
072900061030     C                   else
073000061030     C                   eval      depfi1 = *blanks
073100061030     C                   endif
073200061027     C*
073300061027     C* AZZERA I TOTALI DEL DETTAGLIO
073400061027     C                   Z-ADD     *ZEROS        TLFP1            11 1
073500061027     C                   Z-ADD     *ZEROS        TLFP2            11 1
073600061027     C                   Z-ADD     *ZEROS        TLFP3            11 1
073700061027     C                   Z-ADD     *ZEROS        TLFP4            11 1
073800061027     C                   Z-ADD     *ZEROS        TLFV1            13 3
073900061027     C                   Z-ADD     *ZEROS        TLFV2            13 3
074000061027     C                   Z-ADD     *ZEROS        TLFV3            13 3
074100061027     C                   Z-ADD     *ZEROS        TLFV4            13 3
074200061027     C                   Z-ADD     *ZEROS        TLFC1            11 0
074300061027     C                   Z-ADD     *ZEROS        TLFC2            11 0
074400061027     C                   Z-ADD     *ZEROS        TLFC3            11 0
074500061027     C                   Z-ADD     *ZEROS        TLFC4            11 0
074600061027     C                   Z-ADD     *ZEROS        TLFS1            11 0
074700061027     C                   Z-ADD     *ZEROS        TLFS2            11 0
074800061027     C                   Z-ADD     *ZEROS        TLFS3            11 0
074900061027     C                   Z-ADD     *ZEROS        TLFS4            11 0
075000061027     C                   Z-ADD     *ZEROS        TLTOS            11 0
075100061027     C                   Z-ADD     *ZEROS        TLTOC            11 0
075200061027     C                   Z-ADD     *ZEROS        TLTOP            11 1
075300061027     C                   Z-ADD     *ZEROS        TLTOV            13 3
075400061027     C*
075500061027     C                   ENDSR
075600001106     C*------------------------------------------------------------------------*
075700001106     C* MEMSPE - MEMORIZZA I DATI DI DETTAGLIO
075800001106     C*------------------------------------------------------------------------*
075900060904     C     MEMSPE        BEGSR
076000060901     C*
076100060904     C* TESTO CORRISPONDENZA FASCE DI PESO / VOLUME RISPETTO AI PARAMETRI RICHIESTI
076200060904     C                   SETOFF                                       303132
076300060904     C                   SETOFF                                       33
076400060904     C*
076500060904     C                   MOVE(P)   alaSPV        depspv
076600060904     C*
076700060904     C* => 1° FASCIA DI PESO IN STAMPA
076800060904     C                   IF        PARKG1DA > *zeros OR PARKG1A > *zeros
076900060904     C                   IF        depSPV  > PARKG1DA AND
077000060904     C                             depSPV <= PARKG1A
077100060904     C                   SETON                                        30
077200060904     C                   ENDIF
077300060904     C                   ENDIF
077400060904     C*
077500060904     C* => 2° FASCIA DI PESO IN STAMPA
077600060904     C                   IF        PARKG2DA > *zeros OR PARKG2A > *zeros
077700060904     C                   IF        depSPV  > PARKG2DA AND
077800060904     C                             depSPV <= PARKG2A
077900060904     C                   SETON                                        31
078000060904     C                   ENDIF
078100060904     C                   ENDIF
078200060904     C*
078300060904     C* => 3° FASCIA DI PESO IN STAMPA
078400060904     C                   IF        PARKG3DA > *zeros OR PARKG3A > *zeros
078500060904     C                   IF        depSPV  > PARKG3DA AND
078600060904     C                             depSPV <= PARKG3A
078700060904     C                   SETON                                        32
078800060904     C                   ENDIF
078900060904     C                   ENDIF
079000060904     C*
079100060904     C* => 4° FASCIA DI PESO IN STAMPA
079200060904     C                   IF        PARKG4DA > *zeros OR PARKG4A > *zeros
079300060904     C                   IF        depSPV  > PARKG4DA AND
079400060904     C                             depSPV <= PARKG4A
079500060904     C                   SETON                                        33
079600060904     C                   ENDIF
079700060904     C                   ENDIF
079800001106     C*
079900001109     C* TOTALE PRV/CAP/LOC (DETTAGLIO)
080000060904     C   30              ADD       alatop        TLFP1
080100060904     C   31              ADD       alatop        TLFP2
080200060904     C   32              ADD       alatop        TLFP3
080300060904     C   33              ADD       alatop        TLFP4
080400060904     C   30              ADD       alatov        TLFV1
080500060904     C   31              ADD       alatov        TLFV2
080600060904     C   32              ADD       alatov        TLFV3
080700060904     C   33              ADD       alatov        TLFV4
080800060904     C   30              ADD       alatoc        TLFC1
080900060904     C   31              ADD       alatoc        TLFC2
081000060904     C   32              ADD       alatoc        TLFC3
081100060904     C   33              ADD       alatoc        TLFC4
081200060904     C   30              ADD       alatos        TLFS1
081300060904     C   31              ADD       alatos        TLFS2
081400060904     C   32              ADD       alatos        TLFS3
081500060904     C   33              ADD       alatos        TLFS4
081600060904     C                   IF        *in30=*on OR *in31=*on OR
081700060904     C                             *in32=*on OR *in33=*on
081800060901     C                   ADD       alatos        TLTOS
081900060901     C                   ADD       alatoc        TLTOC
082000060901     C                   ADD       alatop        TLTOP
082100060901     C                   ADD       alatov        TLTOV
082200060904     C                   ENDIF
082300061027     C* TOTALE ZNC
082400061027     C   30              ADD       alatop        TZFP1
082500061027     C   31              ADD       alatop        TZFP2
082600061027     C   32              ADD       alatop        TZFP3
082700061027     C   33              ADD       alatop        TZFP4
082800061027     C   30              ADD       alatov        TZFV1
082900061027     C   31              ADD       alatov        TZFV2
083000061027     C   32              ADD       alatov        TZFV3
083100061027     C   33              ADD       alatov        TZFV4
083200061027     C   30              ADD       alatoc        TZFC1
083300061027     C   31              ADD       alatoc        TZFC2
083400061027     C   32              ADD       alatoc        TZFC3
083500061027     C   33              ADD       alatoc        TZFC4
083600061027     C   30              ADD       alatos        TZFS1
083700061027     C   31              ADD       alatos        TZFS2
083800061027     C   32              ADD       alatos        TZFS3
083900061027     C   33              ADD       alatos        TZFS4
084000061027     C                   IF        *in30=*on OR *in31=*on OR
084100061027     C                             *in32=*on OR *in33=*on
084200061027     C                   ADD       alatos        TZTOS
084300061027     C                   ADD       alatoc        TZTOC
084400061027     C                   ADD       alatop        TZTOP
084500061027     C                   ADD       alatov        TZTOV
084600061027     C                   ENDIF
084700001113     C* TOTALE PRV/CAP
084800060904     C   30              ADD       alatop        TCFP1
084900060904     C   31              ADD       alatop        TCFP2
085000060904     C   32              ADD       alatop        TCFP3
085100060904     C   33              ADD       alatop        TCFP4
085200060904     C   30              ADD       alatov        TCFV1
085300060904     C   31              ADD       alatov        TCFV2
085400060904     C   32              ADD       alatov        TCFV3
085500060904     C   33              ADD       alatov        TCFV4
085600060904     C   30              ADD       alatoc        TCFC1
085700060904     C   31              ADD       alatoc        TCFC2
085800060904     C   32              ADD       alatoc        TCFC3
085900060904     C   33              ADD       alatoc        TCFC4
086000060904     C   30              ADD       alatos        TCFS1
086100060904     C   31              ADD       alatos        TCFS2
086200060904     C   32              ADD       alatos        TCFS3
086300060904     C   33              ADD       alatos        TCFS4
086400060904     C                   IF        *in30=*on OR *in31=*on OR
086500060904     C                             *in32=*on OR *in33=*on
086600060904     C                   ADD       alatos        TCTOS
086700060904     C                   ADD       alatoc        TCTOC
086800060904     C                   ADD       alatop        TCTOP
086900060904     C                   ADD       alatov        TCTOV
087000060904     C                   ENDIF
087100001106     C* TOTALE FILIALE
087200060904     C   30              ADD       alatop        TFFP1
087300060904     C   31              ADD       alatop        TFFP2
087400060904     C   32              ADD       alatop        TFFP3
087500060904     C   33              ADD       alatop        TFFP4
087600060904     C   30              ADD       alatov        TFFV1
087700060904     C   31              ADD       alatov        TFFV2
087800060904     C   32              ADD       alatov        TFFV3
087900060904     C   33              ADD       alatov        TFFV4
088000060904     C   30              ADD       alatoc        TFFC1
088100060904     C   31              ADD       alatoc        TFFC2
088200060904     C   32              ADD       alatoc        TFFC3
088300060904     C   33              ADD       alatoc        TFFC4
088400060904     C   30              ADD       alatos        TFFS1
088500060904     C   31              ADD       alatos        TFFS2
088600060904     C   32              ADD       alatos        TFFS3
088700060904     C   33              ADD       alatos        TFFS4
088800060904     C                   IF        *in30=*on OR *in31=*on OR
088900060904     C                             *in32=*on OR *in33=*on
089000060904     C                   ADD       alatos        TFTOS
089100060904     C                   ADD       alatoc        TFTOC
089200060904     C                   ADD       alatop        TFTOP
089300060904     C                   ADD       alatov        TFTOV
089400060904     C                   ENDIF
089500001106     C* TOTALE AREA
089600060904     C   30              ADD       alatop        TAFP1
089700060904     C   31              ADD       alatop        TAFP2
089800060904     C   32              ADD       alatop        TAFP3
089900060904     C   33              ADD       alatop        TAFP4
090000060904     C   30              ADD       alatov        TAFV1
090100060904     C   31              ADD       alatov        TAFV2
090200060904     C   32              ADD       alatov        TAFV3
090300060904     C   33              ADD       alatov        TAFV4
090400060904     C   30              ADD       alatoc        TAFC1
090500060904     C   31              ADD       alatoc        TAFC2
090600060904     C   32              ADD       alatoc        TAFC3
090700060904     C   33              ADD       alatoc        TAFC4
090800060904     C   30              ADD       alatos        TAFS1
090900060904     C   31              ADD       alatos        TAFS2
091000060904     C   32              ADD       alatos        TAFS3
091100060904     C   33              ADD       alatos        TAFS4
091200060904     C                   IF        *in30=*on OR *in31=*on OR
091300060904     C                             *in32=*on OR *in33=*on
091400060904     C                   ADD       alatos        TATOS
091500060904     C                   ADD       alatoc        TATOC
091600060904     C                   ADD       alatop        TATOP
091700060904     C                   ADD       alatov        TATOV
091800060904     C                   ENDIF
091900001106     C* TOTALE DIVISIONE
092000060904     C   30              ADD       alatop        TDFP1
092100060904     C   31              ADD       alatop        TDFP2
092200060904     C   32              ADD       alatop        TDFP3
092300060904     C   33              ADD       alatop        TDFP4
092400060904     C   30              ADD       alatov        TDFV1
092500060904     C   31              ADD       alatov        TDFV2
092600060904     C   32              ADD       alatov        TDFV3
092700060904     C   33              ADD       alatov        TDFV4
092800060904     C   30              ADD       alatoc        TDFC1
092900060904     C   31              ADD       alatoc        TDFC2
093000060904     C   32              ADD       alatoc        TDFC3
093100060904     C   33              ADD       alatoc        TDFC4
093200060904     C   30              ADD       alatos        TDFS1
093300060904     C   31              ADD       alatos        TDFS2
093400060904     C   32              ADD       alatos        TDFS3
093500060904     C   33              ADD       alatos        TDFS4
093600060904     C                   IF        *in30=*on OR *in31=*on OR
093700060904     C                             *in32=*on OR *in33=*on
093800060904     C                   ADD       alatos        TDTOS
093900060904     C                   ADD       alatoc        TDTOC
094000060904     C                   ADD       alatop        TDTOP
094100060904     C                   ADD       alatov        TDTOV
094200060904     C                   ENDIF
094300001114     C* TOTALE GENERALE
094400060904     C   30              ADD       alatop        TGFP1            11 1
094500060904     C   31              ADD       alatop        TGFP2            11 1
094600060904     C   32              ADD       alatop        TGFP3            11 1
094700060904     C   33              ADD       alatop        TGFP4            11 1
094800060904     C   30              ADD       alatov        TGFV1            13 3
094900060904     C   31              ADD       alatov        TGFV2            13 3
095000060904     C   32              ADD       alatov        TGFV3            13 3
095100060904     C   33              ADD       alatov        TGFV4            13 3
095200060904     C   30              ADD       alatoc        TGFC1            11 0
095300060904     C   31              ADD       alatoc        TGFC2            11 0
095400060904     C   32              ADD       alatoc        TGFC3            11 0
095500060904     C   33              ADD       alatoc        TGFC4            11 0
095600060904     C   30              ADD       alatos        TGFS1            11 0
095700060904     C   31              ADD       alatos        TGFS2            11 0
095800060904     C   32              ADD       alatos        TGFS3            11 0
095900060904     C   33              ADD       alatos        TGFS4            11 0
096000060904     C                   IF        *in30=*on OR *in31=*on OR
096100060904     C                             *in32=*on OR *in33=*on
096200060901     C                   ADD       alatos        TGTOS            11 0
096300060901     C                   ADD       alatoc        TGTOC            11 0
096400060901     C                   ADD       alatop        TGTOP            11 1
096500060901     C                   ADD       alatov        TGTOV            13 3
096600060904     C                   ENDIF
096700001106     C*
096800001106     C                   ENDSR
096900001106     C*------------------------------------------------------------------------*
097000001106     C* DECDIV - DECODIFICA DIVISIONE
097100001106     C*------------------------------------------------------------------------*
097200001106     C     DECDIV        BEGSR
097300001106     C*
097400060904     C                   MOVEL     1             tblKUT
097500060904     C                   MOVE      '17'          tblCOD
097600060904     C                   MOVEL(P)  depdiv        tblKEY
097700060904     C     KEYtab_c      CHAIN     TABEL                              97
097800001106     C     *IN97         IFEQ      '1'
097900001106     C     TBLFLG        OREQ      '*'
098000001106     C                   MOVEL     *ALL'*'       DESDIV           20
098100001106     C                   ELSE
098200001106     C                   MOVEL     TBLUNI        DESDIV
098300001106     C                   ENDIF
098400001106     C*
098500060904     C                   ENDSR
098600001106     C*------------------------------------------------------------------------*
098700001106     C* DECARE - DECODIFICA AREA
098800001106     C*------------------------------------------------------------------------*
098900001106     C     DECARE        BEGSR
099000001106     C*
099100060904     C                   MOVEL     1             tblKUT
099200060904     C                   MOVE      '05'          tblCOD
099300060904     C                   MOVEL(P)  depare        tblKEY
099400060904     C     KEYtab_c      CHAIN     TABEL                              97
099500001106     C     *IN97         IFEQ      '1'
099600001106     C     TBLFLG        OREQ      '*'
099700001106     C                   MOVEL     *ALL'*'       DESARE           20
099800001106     C                   ELSE
099900001106     C                   MOVEL     TBLUNI        DESARE
100000001106     C                   ENDIF
100100001106     C*
100200060904     C                   ENDSR
100300001106     C*------------------------------------------------------------------------*
100400001106     C* DECFIL - DECODIFICA FILIALE/TERMINAL
100500001106     C*------------------------------------------------------------------------*
100600001106     C     DECFIL        BEGSR
100700001106     C*
100800060904     C                   Z-ADD     1             I                 3 0
100900001106     C     DEPFIL        LOOKUP    FIL(I)                                 95
101000001106     C     *IN95         IFEQ      '0'
101100001106     C                   MOVEL     *ALL'*'       DESFIL           20
101200001106     C                   ELSE
101300001106     C                   MOVEL     DFIL(I)       DESFIL
101400001106     C                   ENDIF
101500001106     C*
101600060904     C                   ENDSR
101700001106     C*------------------------------------------------------------------------*
101800001109     C* STADET - STAMPA RIGA DI DETTAGLIO
101900001106     C*------------------------------------------------------------------------*
102000001106     C     STADET        BEGSR
102100001109     C*
102200070410     C                   EVAL      STPTES='FIL. '+%editc(depfil:'Z')+
102300001113     C                                    '  '+desfil
102400001113     C                   EVAL      STPDES = depprv+' '+%subst(depcap:1:5)+
102500061030     C                                      ' '+depfi1+deploc
102600061027     C  N10              MOVE      depznc        STPDES
102700001113     C* VALORIZZO ALTRI DATI IN STAMPA
102800001120     C                   EVAL(H)   STPFP1 = TLFP1 / PARGIO / 100
102900001120     C                   EVAL(H)   STPFP2 = TLFP2 / PARGIO / 100
103000001120     C                   EVAL(H)   STPFP3 = TLFP3 / PARGIO / 100
103100001120     C                   EVAL(H)   STPFP4 = TLFP4 / PARGIO / 100
103200001120     C                   EVAL(H)   STPFV1 = TLFV1 / PARGIO
103300001120     C                   EVAL(H)   STPFV2 = TLFV2 / PARGIO
103400001120     C                   EVAL(H)   STPFV3 = TLFV3 / PARGIO
103500001120     C                   EVAL(H)   STPFV4 = TLFV4 / PARGIO
103600001120     C                   EVAL(H)   STPFC1 = TLFC1 / PARGIO
103700001120     C                   EVAL(H)   STPFC2 = TLFC2 / PARGIO
103800001120     C                   EVAL(H)   STPFC3 = TLFC3 / PARGIO
103900001120     C                   EVAL(H)   STPFC4 = TLFC4 / PARGIO
104000001120     C                   EVAL(H)   STPFS1 = TLFS1 / PARGIO
104100001120     C                   EVAL(H)   STPFS2 = TLFS2 / PARGIO
104200001120     C                   EVAL(H)   STPFS3 = TLFS3 / PARGIO
104300001120     C                   EVAL(H)   STPFS4 = TLFS4 / PARGIO
104400001120     C                   EVAL(H)   STPTOS = TLTOS / PARGIO
104500001120     C                   EVAL(H)   STPTOC = TLTOC / PARGIO
104600001120     C                   EVAL(H)   STPTOP = TLTOP / PARGIO / 100
104700001120     C                   EVAL(H)   STPTOV = TLTOV / PARGIO
104800001113     C*
104900001113     C* STAMPO SOLO SE ESISTONO SPEDIZIONI
105000001113     C                   IF        STPTOS > 0
105100001109     C*
105200001109     C* SE FINE PAGINA STAMPA TESTATA SU FOGLIO SUCCESSIVO
105300001109     C     *IN01         IFEQ      '1'
105400060901     C                   WRITE     ST48TE                                       *TESTATA
105500001110     C                   SETOFF                                       01OF
105600001109     C                   ENDIF
105700060901     C                   WRITE     ST48DE                                       *DETTAGLIO
105800061027     C                   SETON                                        20
105900001110     C                   ENDIF
106000001106     C*
106100001106     C                   ENDSR
106200061027     C*------------------------------------------------------------------------*
106300061027     C* STAZNC - STAMPA TOTALI DELLA ZONA CONSEGNA
106400061027     C*------------------------------------------------------------------------*
106500061027     C     STAZNC        BEGSR
106600061027     C*
106700061027     C                   EVAL      STPDES='       ZNC  '+%editc(depznc:'Z')
106800061027     C* VALORIZZO ALTRI DATI IN STAMPA
106900061027     C                   EVAL(H)   STPFP1 = TZFP1 / PARGIO / 100
107000061027     C                   EVAL(H)   STPFP2 = TZFP2 / PARGIO / 100
107100061027     C                   EVAL(H)   STPFP3 = TZFP3 / PARGIO / 100
107200061027     C                   EVAL(H)   STPFP4 = TZFP4 / PARGIO / 100
107300061027     C                   EVAL(H)   STPFV1 = TZFV1 / PARGIO
107400061027     C                   EVAL(H)   STPFV2 = TZFV2 / PARGIO
107500061027     C                   EVAL(H)   STPFV3 = TZFV3 / PARGIO
107600061027     C                   EVAL(H)   STPFV4 = TZFV4 / PARGIO
107700061027     C                   EVAL(H)   STPFC1 = TZFC1 / PARGIO
107800061027     C                   EVAL(H)   STPFC2 = TZFC2 / PARGIO
107900061027     C                   EVAL(H)   STPFC3 = TZFC3 / PARGIO
108000061027     C                   EVAL(H)   STPFC4 = TZFC4 / PARGIO
108100061027     C                   EVAL(H)   STPFS1 = TZFS1 / PARGIO
108200061027     C                   EVAL(H)   STPFS2 = TZFS2 / PARGIO
108300061027     C                   EVAL(H)   STPFS3 = TZFS3 / PARGIO
108400061027     C                   EVAL(H)   STPFS4 = TZFS4 / PARGIO
108500061027     C                   EVAL(H)   STPTOS = TZTOS / PARGIO
108600061027     C                   EVAL(H)   STPTOC = TZTOC / PARGIO
108700061027     C                   EVAL(H)   STPTOP = TZTOP / PARGIO / 100
108800061027     C                   EVAL(H)   STPTOV = TZTOV / PARGIO
108900061027     C*
109000061027     C* STAMPO SOLO SE ESISTONO SPEDIZIONI
109100061027     C                   IF        STPTOS > 0
109200061027     C*
109300061027     C* SE FINE PAGINA STAMPA TESTATA SU FOGLIO SUCCESSIVO
109400061027     C     *IN01         IFEQ      '1'
109500061027     C                   WRITE     ST48TE                                       *TESTATA
109600061027     C                   SETOFF                                       01OF
109700061027     C                   ENDIF
109800061027     C                   WRITE     ST48DE                                       *DETTAGLIO
109900061027     C                   ENDIF
110000061027     C*
110100061027     C                   SETOFF                                       20
110200061027     C*
110300061027     C                   ENDSR
110400001113     C*------------------------------------------------------------------------*
110500001113     C* STACAP - STAMPA TOTALI DEL CAP
110600001113     C*------------------------------------------------------------------------*
110700001113     C     STACAP        BEGSR
110800001113     C*
110900001113     C                   EVAL      STPDES='       CAP  '+%subst(depcap:1:5)
111000001113     C* VALORIZZO ALTRI DATI IN STAMPA
111100001120     C                   EVAL(H)   STPFP1 = TCFP1 / PARGIO / 100
111200001120     C                   EVAL(H)   STPFP2 = TCFP2 / PARGIO / 100
111300001120     C                   EVAL(H)   STPFP3 = TCFP3 / PARGIO / 100
111400001120     C                   EVAL(H)   STPFP4 = TCFP4 / PARGIO / 100
111500001120     C                   EVAL(H)   STPFV1 = TCFV1 / PARGIO
111600001120     C                   EVAL(H)   STPFV2 = TCFV2 / PARGIO
111700001120     C                   EVAL(H)   STPFV3 = TCFV3 / PARGIO
111800001120     C                   EVAL(H)   STPFV4 = TCFV4 / PARGIO
111900001120     C                   EVAL(H)   STPFC1 = TCFC1 / PARGIO
112000001120     C                   EVAL(H)   STPFC2 = TCFC2 / PARGIO
112100001120     C                   EVAL(H)   STPFC3 = TCFC3 / PARGIO
112200001120     C                   EVAL(H)   STPFC4 = TCFC4 / PARGIO
112300001120     C                   EVAL(H)   STPFS1 = TCFS1 / PARGIO
112400001120     C                   EVAL(H)   STPFS2 = TCFS2 / PARGIO
112500001120     C                   EVAL(H)   STPFS3 = TCFS3 / PARGIO
112600001120     C                   EVAL(H)   STPFS4 = TCFS4 / PARGIO
112700001120     C                   EVAL(H)   STPTOS = TCTOS / PARGIO
112800001120     C                   EVAL(H)   STPTOC = TCTOC / PARGIO
112900001120     C                   EVAL(H)   STPTOP = TCTOP / PARGIO / 100
113000001120     C                   EVAL(H)   STPTOV = TCTOV / PARGIO
113100001113     C*
113200001113     C* STAMPO SOLO SE ESISTONO SPEDIZIONI
113300001113     C                   IF        STPTOS > 0
113400001113     C*
113500001113     C* SE FINE PAGINA STAMPA TESTATA SU FOGLIO SUCCESSIVO
113600001113     C     *IN01         IFEQ      '1'
113700060901     C                   WRITE     ST48TE                                       *TESTATA
113800001113     C                   SETOFF                                       01OF
113900001113     C                   ENDIF
114000060901     C                   WRITE     ST48DE                                       *DETTAGLIO
114100001113     C                   ENDIF
114200001113     C*
114300061027     C                   SETOFF                                       20
114400061027     C*
114500001113     C                   ENDSR
114600001109     C*------------------------------------------------------------------------*
114700001109     C* STAFIL - STAMPA TOTALI DI FILIALE
114800001109     C*------------------------------------------------------------------------*
114900001109     C     STAFIL        BEGSR
115000001109     C*
115100070410     C                   EVAL      STPTES='FIL.        '+%editc(depfil:'Z')+
115200001109     C                                    '  '+desfil
115300070410     C                   EVAL      STPDES='TOTALE FIL. '+%editc(depfil:'Z')+
115400001109     C                                    '  '+desfil
115500001113     C* VALORIZZO ALTRI DATI IN STAMPA
115600001120     C                   EVAL(H)   STPFP1 = TFFP1 / PARGIO / 100
115700001120     C                   EVAL(H)   STPFP2 = TFFP2 / PARGIO / 100
115800001120     C                   EVAL(H)   STPFP3 = TFFP3 / PARGIO / 100
115900001120     C                   EVAL(H)   STPFP4 = TFFP4 / PARGIO / 100
116000001120     C                   EVAL(H)   STPFV1 = TFFV1 / PARGIO
116100001120     C                   EVAL(H)   STPFV2 = TFFV2 / PARGIO
116200001120     C                   EVAL(H)   STPFV3 = TFFV3 / PARGIO
116300001120     C                   EVAL(H)   STPFV4 = TFFV4 / PARGIO
116400001120     C                   EVAL(H)   STPFC1 = TFFC1 / PARGIO
116500001120     C                   EVAL(H)   STPFC2 = TFFC2 / PARGIO
116600001120     C                   EVAL(H)   STPFC3 = TFFC3 / PARGIO
116700001120     C                   EVAL(H)   STPFC4 = TFFC4 / PARGIO
116800001120     C                   EVAL(H)   STPFS1 = TFFS1 / PARGIO
116900001120     C                   EVAL(H)   STPFS2 = TFFS2 / PARGIO
117000001120     C                   EVAL(H)   STPFS3 = TFFS3 / PARGIO
117100001120     C                   EVAL(H)   STPFS4 = TFFS4 / PARGIO
117200001120     C                   EVAL(H)   STPTOS = TFTOS / PARGIO
117300001120     C                   EVAL(H)   STPTOC = TFTOC / PARGIO
117400001120     C                   EVAL(H)   STPTOP = TFTOP / PARGIO / 100
117500001120     C                   EVAL(H)   STPTOV = TFTOV / PARGIO
117600001113     C*
117700001113     C* STAMPO SOLO SE ESISTONO SPEDIZIONI
117800001113     C                   IF        STPTOS > 0
117900001109     C*
118000001109     C* SE FINE PAGINA STAMPA TESTATA SU FOGLIO SUCCESSIVO
118100001110     C     *IN01         IFEQ      '1'
118200060901     C                   WRITE     ST48TE                                       *TESTATA
118300001110     C                   SETOFF                                       OF
118400001110     C                   ENDIF
118500001110     C                   SETON                                        01
118600060901     C                   WRITE     ST48RI                                       *SEPARAZIONE
118700060901     C                   WRITE     ST48DE                                       *DETTAGLIO
118800060901     C                   WRITE     ST48RI                                       *SEPARAZIONE
118900001110     C                   ENDIF
119000001109     C*
119100001109     C                   ENDSR
119200001109     C*------------------------------------------------------------------------*
119300001109     C* STAARE - STAMPA TOTALI DI AREA
119400001109     C*------------------------------------------------------------------------*
119500001109     C     STAARE        BEGSR
119600001109     C*
119700001109     C                   EVAL      STPTES='AREA        '+%editc(depare:'Z')+' '+
119800001109     C                                    desare
119900001109     C                   EVAL      STPDES='TOTALE AREA '+%editc(depare:'Z')+' '+
120000001109     C                                    desare
120100001113     C* VALORIZZO ALTRI DATI IN STAMPA
120200001120     C                   EVAL(H)   STPFP1 = TAFP1 / PARGIO / 100
120300001120     C                   EVAL(H)   STPFP2 = TAFP2 / PARGIO / 100
120400001120     C                   EVAL(H)   STPFP3 = TAFP3 / PARGIO / 100
120500001120     C                   EVAL(H)   STPFP4 = TAFP4 / PARGIO / 100
120600001120     C                   EVAL(H)   STPFV1 = TAFV1 / PARGIO
120700001120     C                   EVAL(H)   STPFV2 = TAFV2 / PARGIO
120800001120     C                   EVAL(H)   STPFV3 = TAFV3 / PARGIO
120900001120     C                   EVAL(H)   STPFV4 = TAFV4 / PARGIO
121000001120     C                   EVAL(H)   STPFC1 = TAFC1 / PARGIO
121100001120     C                   EVAL(H)   STPFC2 = TAFC2 / PARGIO
121200001120     C                   EVAL(H)   STPFC3 = TAFC3 / PARGIO
121300001120     C                   EVAL(H)   STPFC4 = TAFC4 / PARGIO
121400001120     C                   EVAL(H)   STPFS1 = TAFS1 / PARGIO
121500001120     C                   EVAL(H)   STPFS2 = TAFS2 / PARGIO
121600001120     C                   EVAL(H)   STPFS3 = TAFS3 / PARGIO
121700001120     C                   EVAL(H)   STPFS4 = TAFS4 / PARGIO
121800001120     C                   EVAL(H)   STPTOS = TATOS / PARGIO
121900001120     C                   EVAL(H)   STPTOC = TATOC / PARGIO
122000001120     C                   EVAL(H)   STPTOP = TATOP / PARGIO / 100
122100001120     C                   EVAL(H)   STPTOV = TATOV / PARGIO
122200001113     C*
122300001113     C* PROSEGUO E STAMPO SOLO SE ESISTONO SPEDIZIONI
122400001113     C                   IF        STPTOS > 0
122500001109     C*
122600001109     C* SE FINE PAGINA STAMPA TESTATA SU FOGLIO SUCCESSIVO
122700001110     C     *IN01         IFEQ      '1'
122800060901     C                   WRITE     ST48TE                                       *TESTATA
122900001110     C                   SETOFF                                       OF
123000001110     C                   ENDIF
123100001110     C                   SETON                                        01
123200060901     C                   WRITE     ST48RI                                       *SEPARAZIONE
123300060901     C                   WRITE     ST48DE                                       *DETTAGLIO
123400060901     C                   WRITE     ST48RI                                       *SEPARAZIONE
123500001110     C                   ENDIF
123600001109     C*
123700001109     C                   ENDSR
123800001109     C*------------------------------------------------------------------------*
123900001109     C* STADIV - STAMPA TOTALI DI DIVISIONE
124000001109     C*------------------------------------------------------------------------*
124100001109     C     STADIV        BEGSR
124200001109     C*
124300001109     C                   EVAL      STPTES='DIVISIONE        '+depdiv+' '+desdiv
124400001109     C                   EVAL      STPDES='TOTALE DIVIONE   '+depdiv+' '+desdiv
124402070410     C* ELIMINO DESCRIZIONE AREA
124403070410     C                   EVAL      STPARE=*blanks
124500001113     C* VALORIZZO ALTRI DATI IN STAMPA
124600001120     C                   EVAL(H)   STPFP1 = TDFP1 / PARGIO / 100
124700001120     C                   EVAL(H)   STPFP2 = TDFP2 / PARGIO / 100
124800001120     C                   EVAL(H)   STPFP3 = TDFP3 / PARGIO / 100
124900001120     C                   EVAL(H)   STPFP4 = TDFP4 / PARGIO / 100
125000001120     C                   EVAL(H)   STPFV1 = TDFV1 / PARGIO
125100001120     C                   EVAL(H)   STPFV2 = TDFV2 / PARGIO
125200001120     C                   EVAL(H)   STPFV3 = TDFV3 / PARGIO
125300001120     C                   EVAL(H)   STPFV4 = TDFV4 / PARGIO
125400001120     C                   EVAL(H)   STPFC1 = TDFC1 / PARGIO
125500001120     C                   EVAL(H)   STPFC2 = TDFC2 / PARGIO
125600001120     C                   EVAL(H)   STPFC3 = TDFC3 / PARGIO
125700001120     C                   EVAL(H)   STPFC4 = TDFC4 / PARGIO
125800001120     C                   EVAL(H)   STPFS1 = TDFS1 / PARGIO
125900001120     C                   EVAL(H)   STPFS2 = TDFS2 / PARGIO
126000001120     C                   EVAL(H)   STPFS3 = TDFS3 / PARGIO
126100001120     C                   EVAL(H)   STPFS4 = TDFS4 / PARGIO
126200001120     C                   EVAL(H)   STPTOS = TDTOS / PARGIO
126300001120     C                   EVAL(H)   STPTOC = TDTOC / PARGIO
126400001120     C                   EVAL(H)   STPTOP = TDTOP / PARGIO / 100
126500001120     C                   EVAL(H)   STPTOV = TDTOV / PARGIO
126600001113     C*
126700001113     C* PROSEGUO E STAMPO SOLO SE ESISTONO SPEDIZIONI
126800001113     C                   IF        STPTOS > 0
126900001109     C*
127000001109     C* SE FINE PAGINA STAMPA TESTATA SU FOGLIO SUCCESSIVO
127100001110     C     *IN01         IFEQ      '1'
127200060901     C                   WRITE     ST48TE                                       *TESTATA
127300001110     C                   SETOFF                                       OF
127400001110     C                   ENDIF
127500001110     C                   SETON                                        01
127600060901     C                   WRITE     ST48RI                                       *SEPARAZIONE
127700060901     C                   WRITE     ST48DE                                       *DETTAGLIO
127800060901     C                   WRITE     ST48RI                                       *SEPARAZIONE
127900001110     C                   ENDIF
128000001109     C*
128100001109     C                   ENDSR
128200001109     C*------------------------------------------------------------------------*
128300001109     C* STAGEN - STAMPA TOTALI GENERALI
128400001109     C*------------------------------------------------------------------------*
128500001109     C     STAGEN        BEGSR
128600001109     C*
128700001110     C                   EVAL      STPDIV = *BLANKS
128800001110     C                   EVAL      STPARE = *BLANKS
128900001110     C                   EVAL      STPTES = *BLANKS
129000001109     C                   EVAL      STPDES = 'TOTALE GENERALE'
129100001109     C*
129200001120     C                   EVAL(H)   STPFP1 = TGFP1 / PARGIO / 100
129300001120     C                   EVAL(H)   STPFP2 = TGFP2 / PARGIO / 100
129400001120     C                   EVAL(H)   STPFP3 = TGFP3 / PARGIO / 100
129500001120     C                   EVAL(H)   STPFP4 = TGFP4 / PARGIO / 100
129600001120     C                   EVAL(H)   STPFV1 = TGFV1 / PARGIO
129700001120     C                   EVAL(H)   STPFV2 = TGFV2 / PARGIO
129800001120     C                   EVAL(H)   STPFV3 = TGFV3 / PARGIO
129900001120     C                   EVAL(H)   STPFV4 = TGFV4 / PARGIO
130000001120     C                   EVAL(H)   STPFC1 = TGFC1 / PARGIO
130100001120     C                   EVAL(H)   STPFC2 = TGFC2 / PARGIO
130200001120     C                   EVAL(H)   STPFC3 = TGFC3 / PARGIO
130300001120     C                   EVAL(H)   STPFC4 = TGFC4 / PARGIO
130400001120     C                   EVAL(H)   STPFS1 = TGFS1 / PARGIO
130500001120     C                   EVAL(H)   STPFS2 = TGFS2 / PARGIO
130600001120     C                   EVAL(H)   STPFS3 = TGFS3 / PARGIO
130700001120     C                   EVAL(H)   STPFS4 = TGFS4 / PARGIO
130800001120     C                   EVAL(H)   STPTOS = TGTOS / PARGIO
130900001120     C                   EVAL(H)   STPTOC = TGTOC / PARGIO
131000001120     C                   EVAL(H)   STPTOP = TGTOP / PARGIO / 100
131100001120     C                   EVAL(H)   STPTOV = TGTOV / PARGIO
131200001109     C*
131300001109     C* SE FINE PAGINA STAMPA TESTATA SU FOGLIO SUCCESSIVO
131400001110     C     *IN01         IFEQ      '1'
131500060901     C                   WRITE     ST48TE                                       *TESTATA
131600001110     C                   SETOFF                                       OF
131700001110     C                   ENDIF
131800060901     C                   WRITE     ST48RI                                       *SEPARAZIONE
131900060901     C                   WRITE     ST48DE                                       *DETTAGLIO
132000060901     C                   WRITE     ST48RI                                       *SEPARAZIONE
132100060901     C                   WRITE     ST48FI                                       *FINE
132200001109     C*
132300060904     C                   ENDSR
132400000914     C*------------------------------------------------------------------------*
132500060904     C* CARORG - CARICA TUTTE LE FILIALI DALL'ORGANIGRAMMA
132600000914     C*------------------------------------------------------------------------*
132700000914     C     CARORG        BEGSR
132800020226     C*
132900060904     C                   Z-ADD     *ZEROS        y                 3 0
133000000914     C*
133100000914     C* POSIZIONAMENTO SUL FILE E PRIMA LETTURA
133200060904     C     *LOVAL        SETLL     AZORG01L                           99        *FINE-ARCHIV
133300000914     C  N99              READ      AZORG01L                               99
133400000915     C                   Z-ADD     0             y                              *INDICE SCHIERA
133500000914     C     *IN99         DOWEQ     '0'                                          --- 1 -->
133600000914     C     ORGFVA        IFEQ      *BLANKS                                      --- 2 -->
133700000914     C     ORGFAG        ANDNE     'V'
133800000915     C                   ADD       1             y
133900000915     C                   Z-ADD     ORGFIL        FIL(y)
134000001106     C                   MOVEL     ORGDES        DFIL(y)
134100001016     C                   ENDIF
134200000914     C                   READ      AZORG01L                               99
134300001016     C                   ENDDO
134400000914     C*
134500000914     C                   ENDSR
134600000000     C*------------------------------------------------------------------------*
134700000000     C* *INZSR - ROUTINE INIZIALE
134800000000     C*------------------------------------------------------------------------*
134900000000     C     *INZSR        BEGSR
135000000000     C*--------------------
135100000000     C* RICEVIMENTO PARAMETRI
135200000000     C*--------------------
135300000000     C     *ENTRY        PLIST
135400000000     C                   PARM                    KPJBA
135500010417     C                   MOVEL(p)  KPJBU         PARAM
135600061027     C
135700000000     C*--------------------
135800000000     C* CHIAVI DI LETTURA
135900000000     C*--------------------
136000000000     C*
136100060904     C* CHIAVE LETTURA TABEL00F - COMPLETA
136200060904     C     KEYtab_c      KLIST
136300060904     C                   KFLD                    tblKUT
136400060904     C                   KFLD                    tblCOD
136500060904     C                   KFLD                    tblKEY
136600061030     C*
136700061030     C* CHIAVE LETTURA AZCPL01L - COMPLETA
136800061030     C     KEYcpl01_c    KLIST
136900061030     C                   KFLD                    cplVER
137000061030     C                   KFLD                    cplNAR
137100061030     C                   KFLD                    cplCAP
137200061030     C                   KFLD                    cplLOC
137300000914     C*
137400060904     C* CHIAVE LETTURA TIALA01L - COMPLETA
137500060904     C     KEYala01_c    KLIST
137600060904     C                   KFLD                    k_alaDIV
137700060904     C                   KFLD                    k_alaARE
137800060904     C                   KFLD                    k_alaLNA
137900060904     C                   KFLD                    k_alaNAZ
138000060904     C                   KFLD                    k_alaPRV
138100060904     C                   KFLD                    k_alaCAP
138200060904     C                   KFLD                    k_alaLOC
138300060904     C                   KFLD                    k_alaZNC
138400060904     C                   KFLD                    k_alaSPV
138500061027     C*
138600061027     C* CHIAVE LETTURA TIALA02L - COMPLETA
138700061027     C     KEYala02_c    KLIST
138800061027     C                   KFLD                    k_alaDIV
138900061027     C                   KFLD                    k_alaARE
139000061027     C                   KFLD                    k_alaLNA
139100061027     C                   KFLD                    k_alaZNC
139200061027     C                   KFLD                    k_alaNAZ
139300061027     C                   KFLD                    k_alaPRV
139400061027     C                   KFLD                    k_alaCAP
139500061027     C                   KFLD                    k_alaLOC
139600061027     C                   KFLD                    k_alaSPV
139700061030     C*
139800061030     C* CALCOLA LA DATA CORRENTE
139900061030     C                   time                    wn14             14 0
140000061030     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
140100061030     C                   z-add     wn8           g08dat
140200061030     C                   z-add     *zeros        g08inv
140300061030     C                   movel     '0'           g08err
140400061030     C                   call      'XSRDA8'
140500061030     C                   parm                    wlbda8
140600061030     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
140700061030     C*
140800061030     C* Reperisco la versione cappario alla data corrente
140900061030     C                   z-add     *zeros        o96ver            5 0
141000061030     C                   z-add     datcor        i96dri            8 0
141100061030     C                   call      'TISI96R'
141200061030     C                   parm                    i96dri
141300061030     C                   parm                    o96ver
141400000000     C*--------------------
141500060904     C* CAMPI DI CONTROLLO
141600000000     C*--------------------
141700000000     C                   MOVEL     '0'           FINL00            1
141800060904     C                   SETON                                        01
141900000000     C*
142000000000     C                   ENDSR
