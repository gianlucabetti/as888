000100130314     /*PRM dbgview(*source)
000200130313     /*END
000300000000     H DECEDIT('0,') DATEDIT(*DMY.)
000400941109     H* FNLR39R *----------------------------------------------------*
000500911025     H*         - BLOCCO SBLOCCO SPEDIZIONI
000600000000     H*--------------------------------------------------------------*
000700031103     FFNLR39D   CF   E             WORKSTN usropn
000800900517     F                                     INFDS(CA02DS)
000900941109     F                                     SFILE(LR39DSF:NRR)
001000070322     F                                     SFILE(LR39DSF2:NRR)
001100911030     FAZORG01L  IF   E           K DISK
001200070321     Ffiar401l  uF   E           K DISK
001300070322     Ffiarn12l  iF a E           K DISK
001400950918     FFNART01L  IF   E           K DISK
001500990513     FFNANM01L  IF   E           K DISK
001600990513     F                                     INFDS(FNANM1)
001700050331     FTigcp21L  IF   E           K DISK
001800941109     FFNLBL02L  IF   E           K DISK
001900941109     FFNLBL01L  IF   E           K DISK
002000941109     F                                     RENAME(FNLBL000:FNLBL001)
002100031020     FFNARB01L  UF   E           K DISK    COMMIT(DR2COMIT) USROPN
002200070321     FFNARB05L  IF   E           K DISK
002300070321     F                                     RENAME(FNARB000:FNARB005)
002400030613
002500070329     dDECBLO           s             20    dim(5) ctdata perrcd(1)
002600070329     dTES              s             34    dim(2) ctdata perrcd(1)
002700920813     D L6              S              3  0 DIM(30)                              FIL GEST ARRIV
002800030613
002900030613     D CA02DS          DS
003000030613     D  PRIMA                378    379B 0
003100030613     D FNANM1          DS
003200030613     D  ANMNR1               397    400B 0
003300920706     D* PER INTERROGAZIONE BOLLE ARRIVI
003400911023     D PARAM           DS
003500911023     D  GIA                    1     12  0
003600911023     D  RICH                  13     13
003700941109     D  VI5AAS                14     17  0
003800941109     D  VI5LNP                18     20  0
003900941109     D  VI5NRS                21     22  0
004000941109     D  VI5NSP                23     29  0
004100941109     D  VI5KEY                14     29
004200941109     D  VIDFLG                31     31
004300900516     D MMGG            DS
004400900516     D  MM                     1      2  0
004500900516     D  GG                     3      4  0
004600901116     D                 DS
004700911025     D  VIDLNP                 1      3  0
004800911025     D  VIDNRS                 4      5  0
004900911025     D  VIDNSP                 6     12  0
005000911025     D  SEL1                   1     12  0
005100030613      *
005200911024     D KPJBA         E DS
005300941109     D* DS PER FNLV50F - CONTROLLO FILIALE GESTIONE
005400941109     D DSLV50        E DS                  EXTNAME(FNLV50DS)
005500941109     D* DS PER TRUL06R - CARICAMENTO £X
005600941109     D DSUL06        E DS                  EXTNAME(TRUL06DS)
005700941109     D  LIN                    1     90  0
005800941109     D                                     DIM(30)                              FIL. COMODO
005900130313      *
006000070322     d fnlv19ds      e ds                  inz
006100140131     d fnlrG7ds      e ds                  inz
006200140131     d FNLRN6DS      e ds                  inz
006300030613      *
006400941109     D* PASSAGGIO PARAMETRI
006500941109     D DSLR02        E DS
006600941109     D*
006700941109     D CNCR80        E DS
006800911024     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
006900960415     D DSLR33        E DS                  EXTNAME(FNLR33DS)
007000001122     D OG143         E DS
007100070321     D OG148         E DS
007200070321     D DSBL4L        E DS
007300070323      * ds per il controllo della presenza di CA per la spedizione richiesta
007400070323     d FIDN12DS      e ds
007500070323     d  skKey                 26    305    dim(20)
007600070323     d  skAnn                306    325    dim(20)
007700070323     d  skDca                326    485  0 dim(20)
007800070323     d  skFca                486    545  0 dim(20)
007900070323     d  skDch                546    705  0 dim(20)
008000070323     d  skCch                706    745    dim(20)
008100070323      *
008200031117
008300031117     d Tibs02ds      e ds
008400020919
008500070321     d Klna            s                   like(arblna) inz
008600070322     d KCDN            s                   like(arncdn) inz('BGE')
008700030613      *
008800030613     d Wfile           s             10    inz
008900030613     d Woutq           s             10    inz
009000030613     d Wform           s             10    inz
009100030613     d Wusrd           s             10    inz('Bolle.Sbl.')
009200031117
009300031117     d mbbpsw          s             10
009400031117     d wokpsw          s              1
009500070321     d LL              s              3  0
009600070323     d DSRECDCT      E DS                  extname(fndct00f)
009700070326     d DSRECARB      E DS                  extname(fnARB00f)
009800070326     d DSRECAR4      E DS                  extname(fiAR400f)
009900070323     d FGSGEOT         s                   like(§oggeot)
010000070321     d ktrc            s                   like(ar4trc) inz('L')
010100070321     d Dataeur         s               d   datfmt(*eur)
010200030613
010300900518     C****************************************************************
010400900518     C*  RIEPILOGO INDICATORI
010500900518     C***************************************************************
010600911025     C* 01    - SELEZIONE PER SPEDIZIONE
010700920706     C* 02    - PASSATA KEY
010800070321     C* 03    - fil gestione ha geo attiva
010900070321     C* 04    - MODIFICA FILIALE IN GESTIONE (DA F18)
011000020424     C* 05    - SONO UN SECONDO LIVELLO
011100070322     C* 10    - ON  -  visualizzo blocco consegna
011200070322     C* 11    - On  -  visualizzo sospensione consegna
011300070329     C* 12    - SFL DROP attivo
011400070329     c* 13    - non visulizzo mai il blocco "A-lasciato avviso" se geo attiv
011500941109     C* 28    - EMETTO MESSAGGIO DI ERRORE
011600911023     C* 30/31 - LETTURA COMODO
011700911023     C* 34    - LETTURA SUBFILE
011800911023     C* 35    - PULIZIA SUBFILE
011900990513     C* 38    - LETTURA FNANM/FNART X CHIUSURA ANOMALIA 140
012000911025     C* 40-45 - ERRORI
012100001122     C* 52    - ERRORI
012200911025     C* 58-59 - ERRORI
012300900518     C* 90    - INDICA LA PRESENZA DI UN ERRORE
012400900518     C*****************************************************************
012500941220     C*
012600030613     C* DR2RIC = 1 --> PGM RICHIAMATO               02 ON
012700941109     C     DR2RIC        IFEQ      '1'
012800941109     C                   Z-ADD     DR2AAS        VIDAAS
012900941109     C                   Z-ADD     DR2LNP        VIDLNP
013000941109     C                   Z-ADD     DR2NRS        VIDNRS
013100941109     C                   Z-ADD     DR2NSP        VIDNSP
013200941109     C                   Z-ADD     DR2FGS        VIDFIL
013300941109     C                   SETON                                        0204
013400941109     C                   GOTO      CONTRO
013500941109     C                   END
013600911030     C*
013700911025     C     INIZIO        TAG
013800920813     C                   SETOFF                                       04
013900911025     C* PULIZIA CAMPI
014000911025     C                   MOVEL     ' '           VIDFBC
014100070323     C                   MOVEL     'N'           VIDca
014200941109     C                   TIME                    W0140            14 0
014300941109     C                   MOVE      W0140         VIDAAS
014400911025     C                   MOVEL     *ZEROS        VIDLNP
014500911025     C                   MOVEL     *ZEROS        VIDNRS
014600911025     C                   MOVEL     *ZEROS        VIDNSP
014700941109     C                   SETON                                        35
014800941109     C                   WRITE     LR39DCT
014900070322     C                   WRITE     LR39DCT2
015000941109     C                   SETOFF                                       35
015100911025     C*
015200900509     C     FOR01         TAG
015300941109     C                   EXFMT     LR39D01
015400941109     C*
015500941109     C* SPENGO GLI INDICATORI DI POSIZIONAMENTO CURSORE
015600941109     C                   SETOFF                                       4928
015700941109     C*
015800070321     C** F3  - FINE LAVORO
015900900509     C   KC              GOTO      FINE
016000920813     C*
016100070321     C** F7  - INTERROGAZIONE BOLLE ARRIVI
016200911025     C   KG              DO
016300941109     C                   MOVEL     *ZEROS        VI5KEY
016400911213     C                   MOVEL     '2'           VIDFLG
016500911025     C                   MOVEL     PARAM         KPJBU
016600941109     C                   CALL      'FNLR36R'
016700911025     C                   PARM                    KPJBA
016800911025     C                   MOVEL     KPJBU         PARAM
016900941109     C*
017000911025     C* SE PASSATA KEY LA IMPOSTO
017100941109     C     VI5KEY        IFGT      *ZEROS
017200911025     C                   MOVEL     VI5AAS        VIDAAS
017300911025     C                   MOVEL     VI5LNP        VIDLNP
017400911025     C                   MOVEL     VI5NRS        VIDNRS
017500911025     C                   MOVEL     VI5NSP        VIDNSP
017600911025     C                   END
017700911025     C*
017800911025     C                   GOTO      FOR01
017900911025     C                   END
018000920813     C*
018100070321     C** F18- CAMBIO FILIALE
018200920813     C   KS              SETON                                        04
018300920813     C   KS              GOTO      FOR01
018400920813     C*
018500920813     C     CONTRO        TAG
018600941109     C*
018700941109     C* 04 ON - CONTROLLO FILIALE IN GESTIONE
018800070328    1c                   if        *in04
018900070322     C                   EXSR      CTRFGS
019000070328     c
019100070328    2c                   if        *in90
019200070328    3c                   if        dr2vid = ' '
019300070328     C                   GOTO      FOR01
019400070328   x3c                   else
019500070328     c* non gestisce video
019600070328     c                   movel     '1'           dr2err
019700070328     c                   goto      fine
019800070328    3c                   end
019900070328    2c                   endif
020000070328     c
020100070322     c* La prima volta che ho cambiato fil gestione emetto lo stesso
020200070322     c*  il video  anche senza errori
020300070322     c                   setoff                                       04
020400070328    2c                   if        dr2ric=' '
020500070322     c                   goto      for01
020600070328    2c                   endif
020700070328    1c                   endif
020800941109     C*
020900900509     C* CONTROLLI FORMATO1
021000900509     C                   EXSR      CONTR
021100031017     c* errore primo video
021200031017     c                   if        *in90
021300031017     c* gestisce video
021400031017     c                   if        dr2vid = ' '
021500070328     C                   GOTO      FOR01
021600031017     c                   else
021700031017     c* non gestisce video
021800031017     c                   movel     '1'           dr2err
021900031017     c                   goto      fine
022000031017     c                   end
022100031017     c                   end
022200901115     C*
022300911025     C* CARICO SOLO LA SPEDIZIONE SCELTA
022400911025     C                   Z-ADD     0             NRR               4 0
022500911025     C                   Z-ADD     0             COMOD
022600911025     C*
022700911025     C     SEL1          IFNE      0
022800070321     c* Se geo attiva, chain record fiar4
022900070321     c                   EXSR      CHAINar4
023000070322     c* se geo attiva la spedizione la carico 2 volte per entrambi i blocchi
023100070322     c                   if        FGSGEOT='S'
023200070322     c                   seton                                        1110
023300070322     c                   else
023400070416     c                   setoff                                       11
023500070416     c                   seton                                        10
023600070322     c                   endif
023700070322     c
023800911025     C                   EXSR      RIEMPI
023900070322     c
024000911025     C                   ELSE
024100911025     C* CARICO SUBFILE
024200070321     C                   SETOFF                                       3033
024300070321     C                   Z-ADD     0             LL
024400070321     c                   exsr      LegL6
024500911025     C                   EXSR      CARSF
024600911025     C                   END
024700911023     C*
024800901115     C* SE NON CI SONO BOLLE SEGNALO ERRORE
024900900516     C     NRR           IFEQ      0
025000900516     C                   SETON                                        58
025100900516     C                   GOTO      FOR01
025200900516     C                   END
025300031017     c* se pgm non gestisce il video faccio i controlli e se tutto ok aggior
025400031017     c                   if        dr2vid = 'N'
025500070327     c                   exsr      SRCTR
025600031017     c                   if        *in90
025700031017     c                   movel     '1'           dr2err
025800031017     c                   else
025900031017     c                   clear                   dr2err
026000031103     c                   exsr      aggior1
026100031017     c                   end
026200031017     c                   else
026300911025     C**
026400911025     C** SUBFILE
026500911025     C**
026600911025     C                   Z-ADD     1             NRR
026700911025     C                   Z-ADD     NRR           REC
026800070412     c
026900070412     C     FORCT2        TAG
027000941109     C                   WRITE     LR39D02
027100900515     C** COMANDI SUBFILE
027200900515     C     FORCT         TAG
027300070322     c                   if        fgsgeot=' '
027400941109     C                   EXFMT     LR39DCT
027500070322     c                   else
027600070322     C                   EXFMT     LR39DCT2
027700070322     c                   endif
027800941109     C**
027900070321     C* F3 - FINE
028000900515     C   KC              GOTO      FINE
028100941109     C**
028200070321     C* F12 - RITORNO
028300920706     C   KL
028400920706     CANN02              GOTO      INIZIO
028500920706     C   KL
028600920706     CAN 02              GOTO      FINE
028700911025     C**
028800070330     C                   SETOFF                                       90
028900070322     c                   if        fgsgeot=' '
029000941109     C                   READC     LR39DSF                                34
029100070322     c                   else
029200070322     C                   READC     LR39DSF2                               34
029300070322     c                   endif
029400911025     C*
029500941109    1C     *IN34         DOWEQ     '0'
029600911025     C*
029700070330    2C     VI5SCE        IFne      ' '
029800070330     C* INTERROGAZIONE BOLLA
029900070330     c                   if        vi5sce='5'
030000911025     C                   MOVEL     '1'           VIDFLG
030100911025     C                   MOVEL     PARAM         KPJBU
030200941109     C                   CALL      'FNLR36R'
030300911025     C                   PARM                    KPJBA
030400070330     c                   endif
030500070330     c* Note LDV
030600070330     c                   if        vi5sce='N'
030700140131     c                   clear                   fnlrn6ds
030800140131     c                   eval      in6mod='V'
030900140131     c                   eval      in6aas=vi5aas
031000140131     c                   eval      in6lnp=vi5lnp
031100140131     c                   eval      in6nrs=vi5nrs
031200140131     c                   eval      in6nsp=vi5nsp
031300140131     c                   eval      in6pgm='FNLR39R'
031400140131     c                   call      'FNLRN6R'
031500070330     c                   parm                    kpjba
031600140131     c                   parm                    fnlrn6ds
031700070330     c                   endif
031800070330     c
031900070330     C                   SETON                                        34
032000070330     C                   MOVEL     *BLANKS       VI5SCE
032100070330     c
032200911025     C*
032300941109   X2C                   ELSE
032400031017     C* CONTROLLO FLAG BLOCCO MODIFICATO
032500031017     c                   exsr      srctr
032600941109    2C                   END
032700070412     C*
032800911025     C* AGGIORNO
032900911025     C                   Z-ADD     NRR           REC
033000911025     C                   SETON                                        36
033100070322     c
033200070322     c                   if        fgsgeot=' '
033300941109     C                   UPDATE    LR39DSF
033400070322     c                   else
033500070329     c* non visulizzo mai blocco "A"
033600070329     c                   if        oldfbc='A'
033700070329     c                   seton                                        13
033800070329     c                   else
033900070329     c                   setoff                                       13
034000070329     c                   endif
034100070329     c
034200070322     C                   UPDATE    LR39DSF2
034300070322     c                   endif
034400070322     c
034500911025     C                   SETOFF                                       36
034600911022     C*
034700070322     c  n34              if        fgsgeot=' '
034800070322     C                   READC     LR39DSF                                34
034900070322     c                   else
035000070322     C                   READC     LR39DSF2                               34
035100070322     c                   endif
035200070322     c
035300941109    1C                   ENDDO
035400911025     C*
035500920312     C* ERRORE
035600941109     C   90              GOTO      FORCT
035700920312     C*
035800070321     C* F6 - AGGIORNAMENTO BOLLE
035900941109     C   KF              EXSR      AGGIOR
036000941109     C   KF
036100941109     CANN02              GOTO      INIZIO
036200941109     C   KF
036300941109     CAN 02              GOTO      FINE
036400911025     C*
036500070412     C                   GOTO      FORCT2
036600031017     c                   end
036700911023     C*
036800000000     C     FINE          TAG
036900070322     c
037000960415     C* RICHIAMO PGM PER CHIUSURA FILES
037100960415     C     W33R          IFEQ      '1'
037200960415     C                   MOVEL     'C'           D33TLA
037300960415     C                   CALL      'FNLR33R'
037400960415     C                   PARM                    DSLR33
037500960415     C                   END
037600031017     c                   movel(p)  dslr02        kpjbu
037700000000     C                   SETON                                        LR
037800031017     C* --------------------------------------------------------------*
037900031017     C* controlli ----------------------------------------------------*
038000031017     C* --------------------------------------------------------------*
038100070321     C     SRCTR         BEGSR
038200070321     c
038300070330     c* Visualizzazione dettagliata altre abiitazioni
038400070329    1c                   if        vi5gesd='?'
038500070327     c*
038600140131     c                   clear                   fnlrg7ds
038700140131     c                   eval      ilrg7aas=vi5aas
038800140131     c                   eval      ilrg7lnp=vi5lnp
038900140131     c                   eval      ilrg7nrs=vi5nrs
039000140131     c                   eval      ilrg7nsp=vi5nsp
039100140131     c                   eval      ilrg7vsdet='?'
039200070327     c* Escludo dalla visualizzazione il blocco consegna e blocco generico
039300140131     c                   movel     ' '           ilrg7nb1
039400140131     c                   movel     ' '           ilrg7nb2
039500140131     c                   call      'FNLRG7R'
039600140131     c                   parm                    fnlrg7ds
039700070327     c
039800070327     c                   clear                   vi5gesd
039900070327     C                   Z-ADD     PRIMA         REC
040000070417     c                   goto      endcts
040100070329    1c                   endif
040200070330     c* Note LDV
040300070327     c
040400070322     c* Blocco consegna ARBFBC
040500070329    1c                   if        vh5in10='1'
040600070322     c
040700031017     C* CONTROLLO FLAG BLOCCO MODIFICATO
040800070329    2C     OLDFBC        IFNE      NEWFBC
040900031017     C*
041000070322     C* POSSO SOLO BLOCCARLA CON "B"
041100070329    3C     NEWFBC        IFNE      ' '
041200070322     C     NEWFBC        ANDNE     'B'
041300070322     C                   SETON                                        344590
041400070322     C                   Z-ADD     PRIMA         REC
041500070329    3C                   END
041600070507     c* Non posso bloccare con B se in distinta
041700070507     c                   if        newfbc='B' and newfbc<>oldfbc and vh5ndc>0
041800070507     C                   SETON                                        345690
041900070507     C                   Z-ADD     PRIMA         REC
042000070507    3C                   END
042100031017     C*
042200031017     C* SE ERA 'G' NON POSSO VARIARE SE C'E' RECORD DI GIACENZA
042300070329    3C  N90OLDFBC        IFEQ      'G'
042400031017     C*
042500031017     C* VERIFICO SE LA BOLLA IN QUESTIONE E' UNA FIGLIA; SE E' UNA FIGLIA
042600031017     C* E LA MAMMA HA UNA GIACENZA APERTA NON SI PUO' SBLOCCARE
042700031017     C     KARB2         CHAIN     FNLBL01L                           55
042800070329    4C     *IN55         IFEQ      '0'
042900031017     C     LBLLAP        ANDNE     LBLLAN
043000031017     C                   SETON                                        55
043100070329    4C                   END
043200031017     C*
043300050331     C   55KGCA          CHAIN     Tigcp21l                           40
043400050331     C  N55KLEG          CHAIN     Tigcp21l                           40
043500031017     C*
043600031017     C* SE TROVO GIACENZA MA E' ANNULLATA POSSO SBLOCCARE LA SPEDIZIONE
043700031017     C* O CON FASE >= 35
043800070329RE  4C  N40GCPFAS        IFGE      35
043900070329RE   C     GCPFAS        andne     36
044000031017     C     GCPATB        ORNE      ' '
044100031017     C                   SETON                                        40
044200070329    4C                   END
044300031017     C*
044400031017     C* 40 OFF - GIACENZA TROVATA
044500070329    4C     *IN40         IFEQ      *OFF
044600031017     C                   SETON                                        344690
044700031017     C                   Z-ADD     PRIMA         REC
044800070329    4C                   END
044900070329    3C                   END
045000031017     C*
045100031017     C* SE LA BOLLA E' UNA MAMMA NON LA SI PUO' SBLOCCARE
045200070322     C     KARB2         CHAIN     FNLBL02L                           32
045300070329    3C     *IN32         IFEQ      *OFF
045400031017     C                   SETON                                        344790
045500031017     C                   Z-ADD     PRIMA         REC
045600070329    3C                   END
045700070322     c
045800031117      * Se non è richiamato e posso gestire il video
045900070329    3c                   If        dr2vid = ' '  and
046000031117      * se devo bloccare la spedizione con blocco 'B'
046100031117     c                             newfbc = 'B'  and
046200031117      * se non è già stata inserita e se non ci sono errori
046300031117     c                             wokpsw = *Blanks and Not *In90
046400031117     c                   Exsr      Sr_Gespsw
046500070329    3c                   EndIf
046600070329    2C                   END
046700070329    1c                   EndIf
046800070322     c
046900070322     c* Per 11 on sospensione consegna
047000070322     c                   if        vh5in11='1'
047100070322     c
047200070322    1C     OLDBGE        IFNE      NEWBGE
047300070322     C*
047400070322     C* POSSO SOLO BLOCCARLA CON "S"
047500070322    4C     NEWBGE        IFNE      ' '
047600070322     C     NEWBGE        ANDNE     'S'
047700070322     C                   SETON                                        345190
047800070322     C                   Z-ADD     PRIMA         REC
047900070322    4C                   END
048000070322     c* Motivo obbligatortio se modificato il blocco
048100070322     c                   if        vi5nobnew=*blanks
048200070322     C                   SETON                                        345090
048300070322     C                   Z-ADD     PRIMA         REC
048400070322    4C                   END
048500070322    4C                   END
048600070322    4C                   END
048700070322     C*
048800031117
048900070417     c     ENDCTS        endsr
049000031117      *---------------------------------------------------------------*
049100031117      * Gestione videata password x blocco/sblocco                    *
049200031117      *---------------------------------------------------------------*
049300031117     c     Sr_Gespsw     BegSr
049400031117
049500031117     c                   Clear                   w01psw
049600031117     c                   Do        *Hival
049700031117     c                   Exfmt     lr39w01
049800031117     c                   Eval      *In28 = *Off
049900031117     c   kl              Leave
050000031117     c                   If        w01psw = *Blanks
050100031117     c                   Eval      w01msg = 'Immettere la password'
050200031117     c                   Eval      *In28 = *On
050300031117     c                   Iter
050400031117     c                   EndIf
050500031117     c                   If        w01psw <> mbbpsw
050600031117     c                   Eval      w01msg = 'Password errata'
050700031117     c                   Eval      *In28 = *On
050800031117     c                   Iter
050900031117     c                   EndIf
051000031117     c                   Eval      wokpsw = '1'
051100031117     c                   Leave
051200031117     c                   EndDo
051300031117
051400031117     c                   Endsr
051500031117
051600031017     C* OPERAZIONI INIZIALI ------------------------------------------*
051700941109     C     *INZSR        BEGSR
051800941110     C*
051900941110     C     *ENTRY        PLIST
052000941110     C                   PARM                    KPJBA
052100941110     C                   MOVEL     KPJBU         DSLR02
052200031103     c                   if        dr2vid = ' '
052300031103     C                   OPEN      FNlr39d
052400031103     c                   end
052500031017     c* disabilito il comit su FNARB a meno che non sia espressamente
052600031017     c* richiesto ('1')
052700031017     c                   if        dr2comit <> '1'
052800031017     c                   movel     '0'           dr2comit
052900031017     c                   end
053000031020     C                   OPEN      FNARB01L
053100070321     c
053200070321     c                   time                    w0140            14 0
053300070321     c                   move      w0140         w0080             8 0
053400070321     c     *eur          movel     w0080         dataeur
053500070321     c     *iso          movel     dataeur       dateu             8 0
053600941109     C**
053700950918     C* KLIST
053800941109     C**
053900941109     C     KARB          KLIST
054000941109     C                   KFLD                    VIDAAS
054100941109     C                   KFLD                    VIDLNP
054200941109     C                   KFLD                    VIDNRS
054300941109     C                   KFLD                    VIDNSP
054400070321     C     KAR4          KLIST
054500070321     C                   KFLD                    arbAAS
054600070321     C                   KFLD                    arbLNP
054700070321     C                   KFLD                    arbNRS
054800070321     C                   KFLD                    arbNSP
054900070321     C                   KFLD                    ktrc
055000070322     C     KARN          KLIST
055100070322     C                   KFLD                    arbAAS
055200070322     C                   KFLD                    arbLNP
055300070322     C                   KFLD                    arbNRS
055400070322     C                   KFLD                    arbNSP
055500070322     C                   KFLD                    kcdn
055600941109     C*
055700941109     C     KLEG          KLIST
055800941109     C                   KFLD                    LBLAAP
055900941109     C                   KFLD                    LBLLPP
056000941109     C                   KFLD                    LBLNRP
056100941109     C                   KFLD                    LBLNSP
056200950307     C                   KFLD                    KFRG
056300950307     C*
056400950307     C     KGCA          KLIST
056500950307     C                   KFLD                    VI5AAS
056600950307     C                   KFLD                    VI5LNP
056700950307     C                   KFLD                    VI5NRS
056800950307     C                   KFLD                    VI5NSP
056900990713     C                   KFLD                    KFRG              2 0
057000950307     C                   Z-ADD     0             KFRG
057100941109     C*
057200941109     C     KARB2         KLIST
057300941109     C                   KFLD                    VI5AAS
057400941109     C                   KFLD                    VI5LNP
057500941109     C                   KFLD                    VI5NRS
057600941109     C                   KFLD                    VI5NSP
057700070321     C     KAR42         KLIST
057800070321     C                   KFLD                    VI5AAS
057900070321     C                   KFLD                    VI5LNP
058000070321     C                   KFLD                    VI5NRS
058100070321     C                   KFLD                    VI5NSP
058200070321     C                   KFLD                    ktrc
058300950918     C*
058400950918     C     KART          KLIST
058500950918     C                   KFLD                    KAAS
058600950918     C                   KFLD                    KLNP
058700950918     C                   KFLD                    KNRS
058800950918     C                   KFLD                    KNSP
058900950918     C     KANM          KLIST
059000990513     C                   KFLD                    ARTAAS
059100950918     C                   KFLD                    KLNP
059200950918     C                   KFLD                    KNRS
059300950918     C                   KFLD                    KNSP
059400950918     C                   KFLD                    KCAA
059500950918     C*
059600950918     C     KLBL          KLIST
059700950918     C                   KFLD                    KAAS
059800950918     C                   KFLD                    KLNP
059900950918     C                   KFLD                    KNRS
060000950918     C                   KFLD                    KNSP
060100941109     C**
060200941109     C                   Z-ADD     0             NDC               7 0
060300941109     C**
060400941109     C                   Z-ADD     1             CODUT
060500941109     C                   CALL      'X§PARUT'
060600941109     C                   PARM                    UT§DSE
060700941109     C                   MOVEL     RAGUT         VIDDSA
060800941109     C                   MOVEL     REC80         CNCR80
060900941109     C**
061000020424     C* VEDO SE SONO PRIMO O SECONDO LIVELLO
061100941109     C**
061200020424     C                   Z-ADD     SIMPOU        VIDFIL
061300020424     C     SIMTPP        IFEQ      '2'
061400020424     C     SIMTPP        OREQ      *BLANKS
061500020424     C                   SETON                                        05
061600020424     C                   ENDIF
061700941109     C*
061800941109     C                   EXSR      CARL6
061900941109     C*
062000950918     C*
062100950918     C*  DEFINIZIONE VARIABILI
062200950918     C     *LIKE         DEFINE    ANMCAA        KCAA
062300950918     C     *LIKE         DEFINE    ARBAAS        KAAS
062400950918     C     *LIKE         DEFINE    ARBLNP        KLNP
062500950918     C     *LIKE         DEFINE    ARBNRS        KNRS
062600950918     C     *LIKE         DEFINE    ARBNSP        KNSP
062700031117
062800031117      * Aggancio tabella MBB
062900031117     c                   Clear                   mbbpsw
063000031117     c                   Clear                   Tibs02ds
063100031117     c                   Eval      T02Mod = 'C'
063200031117     c                   Eval      T02Sif = knsif
063300031117     c                   Eval      T02Cod = 'MBB'
063400031117     c                   Movel(p)  'TUTTI'       T02Ke1
063500031117     c                   Call      'TIBS02R'
063600031117     c                   Parm                    Kpjba
063700031117     c                   Parm                    Tibs02ds
063800031117     c                   If        T02Err = *Blanks
063900031117     c                   Movel     T02Uni        mbbpsw
064000031117     c                   EndIf
064100031117
064200941109     C                   ENDSR
064300920813     C*
064400920813     C* CARICO L6 ------------------------------------------***********
064500920813     C     CARL6         BEGSR
064600941109     C*
064700941109     C                   CLEAR                   DSUL06
064800941109     C                   MOVE      '£6'          D06COD
064900941109     C                   MOVEL     VIDFIL        D06KEY
065000941109     C                   MOVEL     DSUL06        KPJBU
065100941109     C                   CALL      'TRUL06R'
065200941109     C                   PARM                    KPJBA
065300941109     C                   MOVEL     KPJBU         DSUL06
065400941109     C                   MOVEA     LIN           L6
065500070321     c
065600070321     c* Verifico se ha geo attiva
065700070321     c     vidfil        chain     azorg01l
065800070321     c                   if        %found(azorg01l)
065900070321     C                   MOVEL     ORGDES        DESFIL
066000070321     c                   movel     orgde8        og148
066100070321     c                   eval      FGSGEOT=§oggeot
066200070321     c                   else
066300070321     c                   clear                   FGSGEOT
066400070321     c                   eval      desfil=*all'?'
066500070321     c                   endif
066600070321     c
066700070321     c                   if        fgsgeot='S'
066800070321     c                   seton                                        03
066900070329     C                   MOVE      TES(2)        VIDTES
067000070321     c                   else
067100070321     c                   setoff                                       03
067200070329     C                   MOVE      TES(1)        VIDTES
067300070321     c                   endif
067400070321     c
067500920813     C                   ENDSR
067600900509     C*
067700900509     C*--- CONTROLLI FORMATO1 ----------------------------------------*
067800900509     C     CONTR         BEGSR
067900911025     C                   SETOFF                                       9001
068000070322     c                   clear                   decfbc
068100070321     C* SELEZIONO PER DATA SPEDIZIONE O PER blocco SPED
068200901115     C     SEL1          IFEQ      0
068300941109     C     VIDFBC        ANDEQ     ' '
068400911025     C                   SETON                                        4090
068500901115     C                   GOTO      ENDCTR
068600901115     C                   END
068700901115     C     SEL1          IFNE      0
068800941109     C     VIDFBC        ANDNE     ' '
068900911025     C                   SETON                                        4090
069000901115     C                   GOTO      ENDCTR
069100901115     C                   END
069200901115     C* DATA SPEDIZIONE
069300901115     C     SEL1          IFNE      0
069400031017     C* SPEDIZIONE POSTE: non controllo se pgm richiamato e il parametro
069500031017     C* dr2pos = 'N'
069600070321     c**                 if        dr2pos = ' '
069700070321     C**                 CLEAR                   §OGPT
069800070321     C**   VIDLNP        CHAIN     AZORG01L                           30
069900070321     C**N30              MOVEL     ORGDE3        OG143
070000070321     C**   §OgNTW        IFEQ      'PPT'
070100070321     C**                 SETON                                        52  90
070200070321     C**                 GOTO      ENDCTR
070300070321     C**                 ENDIF
070400070321     c**                 end
070500070321     c
070600911025     C* SPEDIZIONE INESISTENTE O GIA' CONSEGNATA
070700070329     C     KARB          CHAIN     FNARB01L                           3031
070800911025     C   30              SETON                                        4190
070900911025     C   30              GOTO      ENDCTR
071000070329     c
071100070329     c                   if        *in31
071200070329     C                   SETON                                        5390
071300070329     C                   GOTO      ENDCTR
071400070329     c                   endif
071500911030     C*
071600911030     C* SPEDIZIONE NON IN GESTIONE
071700911030     C     ARBLNA        LOOKUP    L6                                     30
071800911030     C  N30              SETON                                        4390
071900911030     C  N30              GOTO      ENDCTR
072000911025     C*
072100941109     C* SPEDIZIONE IN CONSEGNA O CONSEGNATA
072200070507     C  n03ARBNDC        IFNE      0
072300941109     C     ARBDCM        ORNE      0
072400911025     C                   SETON                                        4290
072500911025     C                   GOTO      ENDCTR
072600911025     C                   END
072700070507     c
072800070507     c* Per geo attiva si può mettere in sospensione se in distinta
072900070507     C   03ARBDCM        ifne      0
073000070507     C                   SETON                                        4190
073100070507     C                   GOTO      ENDCTR
073200070507     C                   END
073300911025     C*
073400911025     C                   SETON                                        01
073500901115     C                   END
073600070321     c
073700070321     c* Se GEOT non attiva non posso selezionare blocco "S"
073800070321     c                   if        not *in03 and vidfbc='S'
073900070321     C                   SETON                                        4490
074000070321     C                   GOTO      ENDCTR
074100070321     c                   endif
074200070328     c* Se GEOT     attiva non posso selezionare blocco "A"
074300070328     c                   if            *in03 and vidfbc='A'
074400070328     C                   SETON                                        4690
074500070328     C                   GOTO      ENDCTR
074600070328     c                   endif
074700070322     c* Imposto indicatore di visualizzazione tipo blocco
074800070322     c                   select
074900070322     c                   when      vidfbc=' '
075000070322     c                   setoff                                       101112
075100070322     c
075200070322     c                   when      vidfbc='S'
075300070322     c                   setoff                                       1012
075400070322     c                   seton                                        11
075500070322     c                   eval      decfbc=decblo(3)
075600070322
075700070322     c                   other
075800070322     c*SFLDROP attivo
075900070322     c                   setoff                                       11
076000070322     c                   seton                                        1012
076100070322     c                   if        vidfbc='B'
076200070329     C* SE GEO ATTIVA SI CHIAMANO RIMPALLI
076300070329     C                   if        fgsgeot='S'
076400070329     c                   eval      decfbc=decblo(5)
076500070329     c                   else
076600070322     c                   eval      decfbc=decblo(4)
076700070322     c                   endif
076800070329     c                   endif
076900070322     c                   if        vidfbc='A'
077000070322     c                   eval      decfbc=decblo(1)
077100070322     c                   endif
077200070322     c                   if        vidfbc='G'
077300070322     c                   eval      decfbc=decblo(2)
077400070322     c                   endif
077500070322     c
077600070322     c                   endsl
077700911023     C*
077800900514     C     ENDCTR        ENDSR
077900900515     C*
078000911025     C*--- CARICO SUBFILE --------------------------------------------*
078100911025     C     CARSF         BEGSR
078200900517     C*
078300900518     C                   Z-ADD     COMOD         NRR
078400941109     C*
078500070322    1c                   dow       not *in33
078600070321     c
078700070321     c* se 30 on vado avanti a leggere la L6
078800070321    2c                   if        *in30
078900070321     c                   exsr      LEGL6
079000070321    2c                   endif
079100911030     C*
079200070321    2C     *IN30         IFEQ      '0'
079300070321     C     klna          READE     FNARB05L                               30
079400070321     C
079500070321     c* Leggo le non consegnate e non in distinta
079600070507    3C                   if         not *in30 and arbdcm=0
079700070507     c
079800070507     c* se ho chiesto la sospensione consegna, non eescludo quelle in di
079900070507     c*  distinta
080000070508   3aC                   if        vidfbc='S' or (vidfbc<>'S' and arbndc=0)
080100070323     c                   clear                   wesca             1
080200070323     c
080300070323     c* Vedo se pazializzazione per c.a. solo se GEO attiva
080400070323    4c                   if        fgsgeot='S'
080500070323     c* richiamo fidn12r per vedere se c'e' c.a.
080600070323     c                   clear                   fidn12ds
080700070323     c                   eval      i12aas = arbaas
080800070323     c                   eval      i12lnp = arblnp
080900070323     c                   eval      i12nrs = arbnrs
081000070323     c                   eval      i12nsp = arbnsp
081100070323     c                   eval      i12fel = 'E'
081200070323     c                   eval      i12fan = 'E'
081300070323      *
081400070323     c                   call      'FIDN12R'
081500070323     c                   parm                    fidn12ds
081600070323      *
081700070323      * se non ci sono errori
081800070323    5c                   if        o12err = ' '
081900070323      * se numero di CA trovate maggiore di zero
082000070323    6c                   if        o12nca > 0
082100070323     c
082200070323     c                   z-add     *zeros        ii                2 0
082300070323    7c     1             do        o12nca        ii
082400070323     C                   Z-ADD     skdch(ii)     dctdch
082500070323      * se CA aperta imposto il flag e vado a fine
082600070323    8c                   If        dctdch = 0
082700070323     c                   eval      wesca = 'S'
082800070323     c                   leave
082900070323    8c                   endif
083000070323    7C                   ENDDO
083100070323      *
083200070323    6c                   endif
083300070323    5c                   endif
083400070323    4c                   endif
083500070323     c*
083600070323    4c                   if        fgsgeot=' ' or vidca=' ' or
083700070323     c                             (vidca='N' and wesca=' ') or
083800070323     c                             (vidca='S' and wesca='S')
083900070321     c
084000070321     c                   EXSR      CHAINAR4
084100070321     c
084200941109     C* SE CORRISPONDE AL BLOCCO  DIGITATO CARICO
084300070323    5c                   if        (vidfbc<>'S' and arbfbc=vidfbc)  or
084400070321     c                             (vidfbc='S' and §b4lbge='S')
084500070321     c
084600911025     C                   EXSR      RIEMPI
084700070323    5C                   ENDif
084800070323    4C                   ENDif
084900070508   3aC                   ENDif
085000070508    3C                   ENDif
085100911025     C*
085200070321    2C                   END
085300070321    1C                   END
085400900516     C*
085500911219     C                   Z-ADD     NRR           COMOD             4 0
085600900515     C                   ENDSR
085700070321     C*
085800070321     C*--- Leggo filiali della L6 ------------------------------------*
085900070321     C     LegL6         BEGSR
086000070321     c                   add       1             LL                3 0
086100070321     c                   if        l6(LL)>0
086200070321     c                   setoff                                       30
086300070321     c                   z-add     l6(LL)        klna
086400070321     c     klna          setll     fnarb05l
086500070321     c                   else
086600070321     c                   seton                                        3033
086700070321     c                   endif
086800070321     c
086900070321     C                   ENDSR
087000900515     C*
087100900515     C* EMISSIONE SUBFILE -------------------------------------------
087200911025     C     RIEMPI        BEGSR
087300070329     c                   setoff                                       13
087400070323     C                   MOVEL     *BLANKS       VI5SCE
087500070323     C                   MOVEL     *BLANKS       VI5ca
087600070323     c
087700911025     C                   MOVEL     ARBLNP        VI5LNP
087800911025     C                   MOVEL     ARBLNA        VI5LNA
087900911023     C                   MOVEL     ARBMGS        MMGG
088000900516     C                   MOVEL     GG            VI5DSP
088100900516     C                   MOVE      MM            VI5DSP
088200911023     C                   MOVE      ARBAAS        VI5AAS
088300900516     C*
088400911023     C                   MOVEL     ARBNSP        VI5NSP
088500911023     C                   MOVEL     ARBRSD        VI5RSD
088600911025     C                   MOVEL     ARBRSM        VI5RSM
088700911023     C                   MOVEL     ARBNRS        VI5NRS
088800911023     C                   MOVEL     ARBNCD        VI5SCD
088900911023     C                   MOVEL     ARBNCA        VI5SCA
089000911025     C                   MOVEL     ARBTSP        VI5TSP
089100070507     c                   z-add     arbndc        vh5ndc
089200070322     c
089300070322     c* Per la videata nuova, imposto i segnacolli il un unico campo
089400070322     c                   if        fgsgeot='S'
089500070329     c*
089600070329     c* Se nel blocco consegna c'e' "A" non lo visulizzo
089700070329     c                   if        ARBFBC='A'
089800070329     c                   seton                                        13
089900070329     c                   endif
090000070323     c
090100070323     c* Presente C.A.
090200070323     c                   if        wesca='S'
090300070323     c                   eval      vi5ca='C.A'
090400070323     c                   endif
090500070323     c
090600070322     c* se non sequenziali lo scrivo
090700070322     c                   if        arbfns='S'
090800070322     c                   eval      vi5scn='NON SEQUENZIALI'
090900070322     C                   ELSE
091000070322     c                   eval      vi5scn=%editc(arbncd: 'Z')+'/' +
091100070322     c                             %editc(arbnca: 'Z')
091200070322     C                   ENDIF
091300070322     C
091400140131     C*Altre Gestioni: richiamo pgm FNLRG7R
091500070322     c                   clear                   vi5ges
091600070323     C                   MOVEL     *BLANKS       VI5gesd
091700070326     c*
091800140131     c                   clear                   fnlrg7ds
091900140131     c                   eval      ilrg7aas=arbaas
092000140131     c                   eval      ilrg7lnp=arblnp
092100140131     c                   eval      ilrg7nrs=arbnrs
092200140131     c                   eval      ilrg7nsp=arbnsp
092300070329     c* Escludo dalla visualizzazione il blocco generico
092400140131     c                   movel     'O'           ilrg7nb2
092500140131     c* Escludo dalla visualizzazione la merce
092600140131     c                   movel     'M'           ilrg7nb3
092700140131     c                   call      'FNLRG7R'
092800140131     c                   parm                    fnlrg7ds
092900070326     c                   parm                    dsrecarb
093000070326     c                   parm                    DSrecar4
093100140131     c                   movel     olrg7vssin    vi5ges
093200070323     c
093300070322     c* chain fiarn ultimo record motivo sospensione generica
093400070322     c     karn          chain     fiarn12l
093500070322     c                   if        %found(fiarn12l)
093600070322     c                   movel     arnnob        vi5nobold
093700070322     c                   else
093800070322     c                   clear                   vi5nobold
093900070322     c                   endif
094000070322     c                   clear                   vi5nobnew
094100070322     c                   endif
094200070322     c* 11 on - blocco sospensine consegna
094300070322     c
094400070322     c                   movel     §b4lbge       oldBGE
094500070322     c                   movel     §b4lbge       NEWBGE
094600070322     c
094700070321     C                   MOVEL     ARBFBC        OLDFBC
094800070321     C                   MOVEL     ARBFBC        NEWFBC
094900070322     c                   movel     *in11         vh5in11
095000070322     c                   movel     *in10         vh5in10
095100070321     c
095200070321     c* Se richiamato passo il richiamo
095300031017     c                   if        dr2vid = 'N'
095400031017     c                   movel     dr2bs         newfbc
095500031017     c                   end
095600900522     C*
095700911025     C                   ADD       1             NRR
095800031103     c                   if        dr2vid = ' '
095900070322     c                   if        fgsgeot=' '
096000941109     C                   WRITE     LR39DSF
096100070322     c                   else
096200070322     C                   WRITE     LR39DSF2
096300070322     c                   endif
096400070322     c                   end
096500900515     C                   ENDSR
096600911025     C*
096700911025     C*--- AGGIORNO BOLLE --------------------------------------------*
096800911025     C     AGGIOR        BEGSR
096900030613      *
097000911025     C                   Z-ADD     1             NRR
097100070322     c                   if        fgsgeot=' '
097200941109     C                   READC     LR39DSF                                34
097300070322     c                   else
097400070322     C                   READC     LR39DSF2                               34
097500070322     c                   endif
097600911025     C*
097700070322    1C     *IN34         DOWEQ     '0'
097800070321     c* Gestisco blocco "sospensione consegna" a parte
097900070322    2c                   if        vh5in11='1'
098000070321     c                   exsr      AggioAR4
098100070322    2c                   endif
098200130313     c*
098300070322    2c                   if        vh5in10='1'
098400070321     c                   EXSR      Aggior1
098500070322    1c                   endif
098600911025     C*
098700070322     c                   if        fgsgeot=' '
098800941109     C                   READC     LR39DSF                                34
098900070322     c                   else
099000070322     C                   READC     LR39DSF2                               34
099100070322     c                   endif
099200070322     c
099300070321     C                   ENDdo
099400911025     C                   ENDSR
099500070321     C*
099600070321     C*--- AGGIORNO BOLLE record FIAR4 -------------------------------*
099700070321     C     AGGIOar4      BEGSR
099800070321     c
099900070322     C* SCRIVO MOTIVO BLOCCO/SBLOCCO SOSPENSIONE CONSEGNA
100000070323     c* se se pieno
100100070323     c                   if        vi5nobnew<>*blanks
100200070322     C                   CLEAR                   FIARN000
100300070322     C                   EVAL      ARNAAS=vi5AAS
100400070322     C                   EVAL      ARNLNP=vi5LNP
100500070322     C                   EVAL      ARNNRS=vi5NRS
100600070322     C                   EVAL      ARNNSP=vi5NSP
100700070322     C                   EVAL      ARNCDN=kcdn
100800070322     C                   EVAL      ARNpno=001
100900070322     C                   EVAL      ARNnob=vi5nobnew
101000070402     C                   EVAL      ARNfl1=newbge
101100070322     c                   eval      arndim=dateu
101200070322     c                   time                    arnoim
101300070322     c                   eval      arnpru=knmus
101400070322     c                   eval      arnpgm='FNLR39R'
101500070322     c                   write     fiarn000
101600070323     c                   endif
101700070323     c
101800070322     c
101900070321     C     KAR42         CHAIN     Fiar401L                           31
102000070321     c                   if        %found(fiar401l)
102100070321     c                   movel     ar4not        dsbl4l
102200070322     c                   movel     newBGE        §b4lBGE
102300070321     c                   movel     dsbl4l        ar4not
102400070321     c                   movel     dateu         ar4duv
102500070321     c                   update    fiar4000
102600070322     c                   else
102700070322     c                   clear                   fnlv19ds
102800070322     c                   clear                   dsbl4l
102900070322     c                   movel     newBGE        §b4lBGE
103000070322     c                   movel     dsbl4l        d19nt1
103100070322     C                   MOVEL     vi5AAS        D19AAS
103200070322     C                   MOVEL     vi5lNP        D19LNP
103300070322     C                   MOVEL     vi5NRS        D19NRS
103400070322     C                   MOVEL     vi5NSP        D19NSP
103500070322     C                   MOVEL     'L'           D19TRC
103600070322     C                   MOVEL     'T'           D19FTR
103700070322     C                   CLEAR                   D19NT2
103800070322     C                   CALL      'FNLV19R'
103900070322     C                   PARM                    fnlv19ds
104000070321     c                   endif
104100070321     c
104200070321     C                   ENDSR
104300070321     c
104400070321     C*--- AGGIORNO BOLLE record FNARB -------------------------------*
104500031103     C     AGGIOR1       BEGSR
104600031103     C*
104700031103     C     KARB2         CHAIN     FNARB01L                           31
104800070329     c
104900031103     C* SOLO SE MODIFICO  IL BLOCCO CONSEGNA
105000031103     C     NEWFBC        IFNE      ARBFBC
105100031103     C                   MOVEL     *BLANKS       ARBCMC
105200031103     C  N31              MOVEL     NEWFBC        ARBFBC
105300031103     C  N31              UPDATE    FNARB000
105400031103     C* SE RIMETTO IN CONSEGNA LA BOLLA CHIUDO EVENTUALI ANOMALIE
105500031103     C* DI GIACENZA
105600031103     C     NEWFBC        IFEQ      *BLANKS
105700031103     C                   EXSR      CLOANM
105800031103     C                   END
105900070329     c
106000070329     c                   else
106100070329     c                   unlock    fnarb01l
106200070329     C                   ENDIF
106300031103     C                   ENDSR
106400950918     C*-----------------------------------------------------*
106500950918     C*  Chiudo anomalie 140 su dettaglio segnacolli
106600950918     C*-----------------------------------------------------*
106700950918     C     CLOANM        BEGSR
106800950918     C*
106900950918     C*  Controllo se bolla con segnacolli
107000950918     C                   Z-ADD     ARBAAS        KAAS
107100950918     C                   Z-ADD     ARBLNP        KLNP
107200950918     C                   Z-ADD     ARBNRS        KNRS
107300950918     C                   Z-ADD     ARBNSP        KNSP
107400950918     C     KART          CHAIN     FNART01L                           38
107500950918     C*  Se non esiste e spedizione figlia mi posiziono su mamma
107600950918     C     *IN38         IFEQ      '1'
107700950918     C     KLBL          CHAIN     FNLBL01L                           38
107800950918     C     *IN38         IFEQ      '0'
107900950918     C     LBLLAP        ANDEQ     LBLLAN
108000950918     C                   Z-ADD     LBLAAP        KAAS
108100950918     C                   Z-ADD     LBLLPP        KLNP
108200950918     C                   Z-ADD     LBLNRP        KNRS
108300950918     C                   Z-ADD     LBLNSP        KNSP
108400950918     C     KART          CHAIN     FNART01L                           38
108500950918     C                   END
108600950918     C                   END
108700950918     C*  Se ho trovato i segnacolli vado a chiudere le anomalie
108800950918     C     *IN38         IFEQ      '0'
108900950918     C                   Z-ADD     140           KCAA
109000960415     C                   CLEAR                   DSLR33
109100960415     C                   MOVE      '4'           D33FSC
109200070321     C                   MOVE      dateu         D33DCH
109300960415     C                   MOVEL     'DI'          D33CCH
109400990513     C     KANM          CHAIN     FNANM01L                           38
109500960415     C  N38              MOVE      '1'           W33R              1
109600950918     C     *IN38         DOWEQ     '0'
109700960415     C                   Z-ADD     ANMNR1        D33NRR
109800960415     C                   CALL      'FNLR33R'
109900960415     C                   PARM                    DSLR33
110000990513     C     KANM          READE     FNANM01L                               38
110100960415     C                   ENDDO
110200950918     C*
110300950918     C                   END
110400950918     C*
110500950918     C                   ENDSR
110600070321     C*
110700070321     C* CHain record fiar4 record L ----------------------------------*
110800070321     C     CHAINAR4      BEGSR
110900070321     c                   clear                   dsbl4L
111000070321    1c                   if        fgsgeot='S'
111100070321     c     kar4          chain(N)  fiar401l
111200070321    2c                   if        %found(fiar401l)
111300070321     c                   movel     ar4not        dsbl4L
111400070328     c                   else
111500070328     c                   clear                   fiar4000
111600070321    2c                   endif
111700070321    1c                   endif
111800070321     C                   ENDSR
111900941109     C*
112000941109     C* CONTROLLO LA FILIALE IN GESTIONE -----------------------------*
112100941109     C     CTRFGS        BEGSR
112200941109     C                   SETOFF                                       90
112300941109     C                   CLEAR                   DSLV50
112400941109     C                   MOVEL     KNMUS         D50PRU
112500941109     C                   MOVEL     VIDFIL        D50FGS
112600941109     C                   CALL      'FNLV50R'
112700941109     C                   PARM                    DSLV50
112800941109     C*
112900941109     C     D50ERR        IFNE      ' '
113000941109     C                   MOVEL     D50MSG        VIDMSG
113100941109     C                   SETON                                        499028
113200941109     C                   GOTO      ENDCTF
113300941109     C                   ENDIF
113400070321     c
113500070321     C* CARICO LA L6   e decodifica
113600941109     C                   EXSR      CARL6
113700941109     C*
113800941109     C     ENDCTF        ENDSR
113900030613
114000030613
114100070322**  decodifica blocchi
114200070322  LASCIATI  AVVISO
114300070322      GIACENZE
114400070322SOSPENSIONE CONSEGNA
114500070322      BLOCCATE
114600070329      RIMPALLI
114700070329**  Testata
114800070329 ** BLOCCO / SBLOCCO SPEDIZIONI **
114900070329 * GESTIONE SOSPENSIONE CONSEGNA *
