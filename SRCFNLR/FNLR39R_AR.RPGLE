000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200941109     H* FNLR39R *----------------------------------------------------*
000300911025     H*         - BLOCCO SBLOCCO SPEDIZIONI
000400000000     H*--------------------------------------------------------------*
000500031103     FFNLR39D   CF   E             WORKSTN usropn
000600900517     F                                     INFDS(CA02DS)
000700941109     F                                     SFILE(LR39DSF:NRR)
000800070322     F                                     SFILE(LR39DSF2:NRR)
000900911030     FAZORG01L  IF   E           K DISK
001000070321     Ffiar401l  uF   E           K DISK
001100070322     Ffiarn12l  iF a E           K DISK
001200090309     FFiART01L  IF   E           K DISK
001300990513     FFNANM01L  IF   E           K DISK
001400990513     F                                     INFDS(FNANM1)
001500050331     FTigcp21L  IF   E           K DISK
001600941109     FFNLBL02L  IF   E           K DISK
001700941109     FFNLBL01L  IF   E           K DISK
001800941109     F                                     RENAME(FNLBL000:FNLBL001)
001900031020     FFNARB01L  UF   E           K DISK    COMMIT(DR2COMIT) USROPN
002000070321     FFNARB05L  IF   E           K DISK
002100070321     F                                     RENAME(FNARB000:FNARB005)
002200030613
002300030613     d $Cmd            s             80    dim(3) ctdata perrcd(1)
002400070329     dDECBLO           s             20    dim(5) ctdata perrcd(1)
002500070329     dTES              s             34    dim(2) ctdata perrcd(1)
002600920813     D L6              S              3  0 DIM(30)                              FIL GEST ARRIV
002700030613
002800030613     D CA02DS          DS
002900030613     D  PRIMA                378    379B 0
003000030613     D FNANM1          DS
003100030613     D  ANMNR1               397    400B 0
003200920706     D* PER INTERROGAZIONE BOLLE ARRIVI
003300911023     D PARAM           DS
003400911023     D  GIA                    1     12  0
003500911023     D  RICH                  13     13
003600941109     D  VI5AAS                14     17  0
003700941109     D  VI5LNP                18     20  0
003800941109     D  VI5NRS                21     22  0
003900941109     D  VI5NSP                23     29  0
004000941109     D  VI5KEY                14     29
004100941109     D  VIDFLG                31     31
004200900516     D MMGG            DS
004300900516     D  MM                     1      2  0
004400900516     D  GG                     3      4  0
004500901116     D                 DS
004600911025     D  VIDLNP                 1      3  0
004700911025     D  VIDNRS                 4      5  0
004800911025     D  VIDNSP                 6     12  0
004900911025     D  SEL1                   1     12  0
005000030613      *
005100911024     D KPJBA         E DS
005200941109     D* DS PER FNLV50F - CONTROLLO FILIALE GESTIONE
005300941109     D DSLV50        E DS                  EXTNAME(FNLV50DS)
005400941109     D* DS PER TRUL06R - CARICAMENTO £X
005500941109     D DSUL06        E DS                  EXTNAME(TRUL06DS)
005600941109     D  LIN                    1     90  0
005700941109     D                                     DIM(30)                              FIL. COMODO
005800030613      * Parametri per richiesta stampanti
005900030613     d TRUL90DS      e ds                  inz
006000030613     d   D90rsb      e                     inz('S')
006100030613      *
006200030613      * DS per pgm di STAMPA BOLLE
006300030613     d FNLSB5DS      e ds                  inz
006400070322     d
006500070322     d fnlv19ds      e ds                  inz
006600070326     d fnlrG6ds      e ds                  inz
006700070330     d FNLRN5DS      e ds                  inz
006800030613      *
006900941109     D* PASSAGGIO PARAMETRI
007000941109     D DSLR02        E DS
007100941109     D*
007200941109     D CNCR80        E DS
007300911024     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
007400960415     D DSLR33        E DS                  EXTNAME(FNLR33DS)
007500001122     D OG143         E DS
007600070321     D OG148         E DS
007700070321     D DSBL4L        E DS
007800070323      * ds per il controllo della presenza di CA per la spedizione richiesta
007900070323     d FIDN12DS      e ds
008000070323     d  skKey                 26    305    dim(20)
008100070323     d  skAnn                306    325    dim(20)
008200070323     d  skDca                326    485  0 dim(20)
008300070323     d  skFca                486    545  0 dim(20)
008400070323     d  skDch                546    705  0 dim(20)
008500070323     d  skCch                706    745    dim(20)
008600070323      *
008700031117
008800031117     d Tibs02ds      e ds
008900020919
009000070321     d Klna            s                   like(arblna) inz
009100070322     d KCDN            s                   like(arncdn) inz('BGE')
009200030613      *
009300030613     d $trul90         s              1    inz(*off)
009400030613      *
009500030613     d Qcmd            s            150    inz
009600030613     d Qlen            s             15  5 inz(150)
009700030613      *
009800030613     d Wfile           s             10    inz
009900030613     d Woutq           s             10    inz
010000030613     d Wform           s             10    inz
010100030613     d Wusrd           s             10    inz('Bolle.Sbl.')
010200031117
010300031117     d mbbpsw          s             10
010400031117     d wokpsw          s              1
010500070321     d LL              s              3  0
010600070323     d DSRECDCT      E DS                  extname(fndct00f)
010700070326     d DSRECARB      E DS                  extname(fnARB00f)
010800070326     d DSRECAR4      E DS                  extname(fiAR400f)
010900070323     d FGSGEOT         s                   like(§oggeot)
011000070321     d ktrc            s                   like(ar4trc) inz('L')
011100070321     d Dataeur         s               d   datfmt(*eur)
011200030613
011300900518     C****************************************************************
011400900518     C*  RIEPILOGO INDICATORI
011500900518     C***************************************************************
011600911025     C* 01    - SELEZIONE PER SPEDIZIONE
011700920706     C* 02    - PASSATA KEY
011800070321     C* 03    - fil gestione ha geo attiva
011900070321     C* 04    - MODIFICA FILIALE IN GESTIONE (DA F18)
012000020424     C* 05    - SONO UN SECONDO LIVELLO
012100070322     C* 10    - ON  -  visualizzo blocco consegna
012200070322     C* 11    - On  -  visualizzo sospensione consegna
012300070329     C* 12    - SFL DROP attivo
012400070329     c* 13    - non visulizzo mai il blocco "A-lasciato avviso" se geo attiv
012500941109     C* 28    - EMETTO MESSAGGIO DI ERRORE
012600911023     C* 30/31 - LETTURA COMODO
012700911023     C* 34    - LETTURA SUBFILE
012800911023     C* 35    - PULIZIA SUBFILE
012900090309     C* 38    - LETTURA FNANM/FiART X CHIUSURA ANOMALIA 140
013000911025     C* 40-45 - ERRORI
013100001122     C* 52    - ERRORI
013200911025     C* 58-59 - ERRORI
013300900518     C* 90    - INDICA LA PRESENZA DI UN ERRORE
013400900518     C*****************************************************************
013500941109     C*
013600950121     C                   MOVEL     'N'           VIDSTA
013700941220     C*
013800030613     C* DR2RIC = 1 --> PGM RICHIAMATO               02 ON
013900941109     C     DR2RIC        IFEQ      '1'
014000941109     C                   Z-ADD     DR2AAS        VIDAAS
014100941109     C                   Z-ADD     DR2LNP        VIDLNP
014200941109     C                   Z-ADD     DR2NRS        VIDNRS
014300941109     C                   Z-ADD     DR2NSP        VIDNSP
014400941109     C                   Z-ADD     DR2FGS        VIDFIL
014500941109     C                   SETON                                        0204
014600941109     C                   GOTO      CONTRO
014700941109     C                   END
014800911030     C*
014900911025     C     INIZIO        TAG
015000920813     C                   SETOFF                                       04
015100911025     C* PULIZIA CAMPI
015200911025     C                   MOVEL     ' '           VIDFBC
015300070323     C                   MOVEL     'N'           VIDca
015400941109     C                   TIME                    W0140            14 0
015500941109     C                   MOVE      W0140         VIDAAS
015600911025     C                   MOVEL     *ZEROS        VIDLNP
015700911025     C                   MOVEL     *ZEROS        VIDNRS
015800911025     C                   MOVEL     *ZEROS        VIDNSP
015900941109     C                   SETON                                        35
016000941109     C                   WRITE     LR39DCT
016100070322     C                   WRITE     LR39DCT2
016200941109     C                   SETOFF                                       35
016300911025     C*
016400900509     C     FOR01         TAG
016500941109     C                   EXFMT     LR39D01
016600941109     C*
016700941109     C* SPENGO GLI INDICATORI DI POSIZIONAMENTO CURSORE
016800941109     C                   SETOFF                                       4928
016900941109     C*
017000070321     C** F3  - FINE LAVORO
017100900509     C   KC              GOTO      FINE
017200920813     C*
017300070321     C** F7  - INTERROGAZIONE BOLLE ARRIVI
017400911025     C   KG              DO
017500941109     C                   MOVEL     *ZEROS        VI5KEY
017600911213     C                   MOVEL     '2'           VIDFLG
017700911025     C                   MOVEL     PARAM         KPJBU
017800941109     C                   CALL      'FNLR36R'
017900911025     C                   PARM                    KPJBA
018000911025     C                   MOVEL     KPJBU         PARAM
018100941109     C*
018200911025     C* SE PASSATA KEY LA IMPOSTO
018300941109     C     VI5KEY        IFGT      *ZEROS
018400911025     C                   MOVEL     VI5AAS        VIDAAS
018500911025     C                   MOVEL     VI5LNP        VIDLNP
018600911025     C                   MOVEL     VI5NRS        VIDNRS
018700911025     C                   MOVEL     VI5NSP        VIDNSP
018800911025     C                   END
018900911025     C*
019000911025     C                   GOTO      FOR01
019100911025     C                   END
019200920813     C*
019300070321     C** F18- CAMBIO FILIALE
019400920813     C   KS              SETON                                        04
019500920813     C   KS              GOTO      FOR01
019600920813     C*
019700920813     C     CONTRO        TAG
019800941109     C*
019900941109     C* 04 ON - CONTROLLO FILIALE IN GESTIONE
020000070328    1c                   if        *in04
020100070322     C                   EXSR      CTRFGS
020200070328     c
020300070328    2c                   if        *in90
020400070328    3c                   if        dr2vid = ' '
020500070328     C                   GOTO      FOR01
020600070328   x3c                   else
020700070328     c* non gestisce video
020800070328     c                   movel     '1'           dr2err
020900070328     c                   goto      fine
021000070328    3c                   end
021100070328    2c                   endif
021200070328     c
021300070322     c* La prima volta che ho cambiato fil gestione emetto lo stesso
021400070322     c*  il video  anche senza errori
021500070322     c                   setoff                                       04
021600070328    2c                   if        dr2ric=' '
021700070322     c                   goto      for01
021800070328    2c                   endif
021900070328    1c                   endif
022000941109     C*
022100900509     C* CONTROLLI FORMATO1
022200900509     C                   EXSR      CONTR
022300031017     c* errore primo video
022400031017     c                   if        *in90
022500031017     c* gestisce video
022600031017     c                   if        dr2vid = ' '
022700070328     C                   GOTO      FOR01
022800031017     c                   else
022900031017     c* non gestisce video
023000031017     c                   movel     '1'           dr2err
023100031017     c                   goto      fine
023200031017     c                   end
023300031017     c                   end
023400030613      *
023500030613      * Se richiesta la stampa delle bolle sbloccate
023600030613      *   richiedo i dati per la stampa ed eseguo le override
023700030613if  1c                   if            VIDsta  = 'S'
023800030613     c                             and $trul90 = *off
023900030613     c                   eval      $trul90 = *on
024000030613     c                   exsr      SR_OvrPrtF
024100030613     c     D90F3         cabeq     *on           FINE
024200030613     c     *in90         cabeq     *on           FOR01
024300030613e   1c                   endif
024400901115     C*
024500911025     C* CARICO SOLO LA SPEDIZIONE SCELTA
024600911025     C                   Z-ADD     0             NRR               4 0
024700911025     C                   Z-ADD     0             COMOD
024800911025     C*
024900911025     C     SEL1          IFNE      0
025000070321     c* Se geo attiva, chain record fiar4
025100070321     c                   EXSR      CHAINar4
025200070322     c* se geo attiva la spedizione la carico 2 volte per entrambi i blocchi
025300070322     c                   if        FGSGEOT='S'
025400070322     c                   seton                                        1110
025500070322     c                   else
025600070416     c                   setoff                                       11
025700070416     c                   seton                                        10
025800070322     c                   endif
025900070322     c
026000911025     C                   EXSR      RIEMPI
026100070322     c
026200911025     C                   ELSE
026300911025     C* CARICO SUBFILE
026400070321     C                   SETOFF                                       3033
026500070321     C                   Z-ADD     0             LL
026600070321     c                   exsr      LegL6
026700911025     C                   EXSR      CARSF
026800911025     C                   END
026900911023     C*
027000901115     C* SE NON CI SONO BOLLE SEGNALO ERRORE
027100900516     C     NRR           IFEQ      0
027200900516     C                   SETON                                        58
027300900516     C                   GOTO      FOR01
027400900516     C                   END
027500031017     c* se pgm non gestisce il video faccio i controlli e se tutto ok aggior
027600031017     c                   if        dr2vid = 'N'
027700070327     c                   exsr      SRCTR
027800031017     c                   if        *in90
027900031017     c                   movel     '1'           dr2err
028000031017     c                   else
028100031017     c                   clear                   dr2err
028200031103     c                   exsr      aggior1
028300031017     c                   end
028400031017     c                   else
028500911025     C**
028600911025     C** SUBFILE
028700911025     C**
028800911025     C                   Z-ADD     1             NRR
028900911025     C                   Z-ADD     NRR           REC
029000070412     c
029100070412     C     FORCT2        TAG
029200941109     C                   WRITE     LR39D02
029300900515     C** COMANDI SUBFILE
029400900515     C     FORCT         TAG
029500070322     c                   if        fgsgeot=' '
029600941109     C                   EXFMT     LR39DCT
029700070322     c                   else
029800070322     C                   EXFMT     LR39DCT2
029900070322     c                   endif
030000941109     C**
030100070321     C* F3 - FINE
030200900515     C   KC              GOTO      FINE
030300941109     C**
030400070321     C* F12 - RITORNO
030500920706     C   KL
030600920706     CANN02              GOTO      INIZIO
030700920706     C   KL
030800920706     CAN 02              GOTO      FINE
030900911025     C**
031000070330     C                   SETOFF                                       90
031100070322     c                   if        fgsgeot=' '
031200941109     C                   READC     LR39DSF                                34
031300070322     c                   else
031400070322     C                   READC     LR39DSF2                               34
031500070322     c                   endif
031600911025     C*
031700941109    1C     *IN34         DOWEQ     '0'
031800911025     C*
031900070330    2C     VI5SCE        IFne      ' '
032000070330     C* INTERROGAZIONE BOLLA
032100070330     c                   if        vi5sce='5'
032200911025     C                   MOVEL     '1'           VIDFLG
032300911025     C                   MOVEL     PARAM         KPJBU
032400941109     C                   CALL      'FNLR36R'
032500911025     C                   PARM                    KPJBA
032600070330     c                   endif
032700070330     c* Note LDV
032800070330     c                   if        vi5sce='N'
032900070330     c                   clear                   fnlrn5ds
033000070330     c                   eval      in5mod='V'
033100070330     c                   eval      in5aas=vi5aas
033200070330     c                   eval      in5lnp=vi5lnp
033300070330     c                   eval      in5nrs=vi5nrs
033400070330     c                   eval      in5nsp=vi5nsp
033500070330     c                   call      'FNLRN5R'
033600070330     c                   parm                    kpjba
033700070330     c                   parm                    fnlrn5ds
033800070330     c                   endif
033900070330     c
034000070330     C                   SETON                                        34
034100070330     C                   MOVEL     *BLANKS       VI5SCE
034200070330     c
034300911025     C*
034400941109   X2C                   ELSE
034500031017     C* CONTROLLO FLAG BLOCCO MODIFICATO
034600031017     c                   exsr      srctr
034700941109    2C                   END
034800070412     C*
034900911025     C* AGGIORNO
035000911025     C                   Z-ADD     NRR           REC
035100911025     C                   SETON                                        36
035200070322     c
035300070322     c                   if        fgsgeot=' '
035400941109     C                   UPDATE    LR39DSF
035500070322     c                   else
035600070329     c* non visulizzo mai blocco "A"
035700070329     c                   if        oldfbc='A'
035800070329     c                   seton                                        13
035900070329     c                   else
036000070329     c                   setoff                                       13
036100070329     c                   endif
036200070329     c
036300070322     C                   UPDATE    LR39DSF2
036400070322     c                   endif
036500070322     c
036600911025     C                   SETOFF                                       36
036700911022     C*
036800070322     c  n34              if        fgsgeot=' '
036900070322     C                   READC     LR39DSF                                34
037000070322     c                   else
037100070322     C                   READC     LR39DSF2                               34
037200070322     c                   endif
037300070322     c
037400941109    1C                   ENDDO
037500911025     C*
037600920312     C* ERRORE
037700941109     C   90              GOTO      FORCT
037800920312     C*
037900070321     C* F6 - AGGIORNAMENTO BOLLE
038000941109     C   KF              EXSR      AGGIOR
038100941109     C   KF
038200941109     CANN02              GOTO      INIZIO
038300941109     C   KF
038400941109     CAN 02              GOTO      FINE
038500911025     C*
038600070412     C                   GOTO      FORCT2
038700031017     c                   end
038800911023     C*
038900000000     C     FINE          TAG
039000070322     c
039100960415     C* RICHIAMO PGM PER CHIUSURA FILES
039200960415     C     W33R          IFEQ      '1'
039300960415     C                   MOVEL     'C'           D33TLA
039400960415     C                   CALL      'FNLR33R'
039500960415     C                   PARM                    DSLR33
039600960415     C                   END
039700031017     c                   movel(p)  dslr02        kpjbu
039800000000     C                   SETON                                        LR
039900031017     C* --------------------------------------------------------------*
040000031017     C* controlli ----------------------------------------------------*
040100031017     C* --------------------------------------------------------------*
040200070321     C     SRCTR         BEGSR
040300070321     c
040400070330     c* Visualizzazione dettagliata altre abiitazioni
040500070329    1c                   if        vi5gesd='?'
040600070327     c*
040700070327     c                   clear                   fnlrg6ds
040800070327     c                   eval      ilrg6aas=vi5aas
040900070327     c                   eval      ilrg6lnp=vi5lnp
041000070327     c                   eval      ilrg6nrs=vi5nrs
041100070327     c                   eval      ilrg6nsp=vi5nsp
041200070327     c                   eval      ilrg6vsdet='?'
041300070327     c* Escludo dalla visualizzazione il blocco consegna e blocco generico
041400070327     c                   movel     ' '           ilrg6nb1
041500070327     c                   movel     ' '           ilrg6nb2
041600070327     c                   call      'FNLRG6R'
041700070327     c                   parm                    fnlrg6ds
041800070327     c
041900070327     c                   clear                   vi5gesd
042000070327     C                   Z-ADD     PRIMA         REC
042100070417     c                   goto      endcts
042200070329    1c                   endif
042300070330     c* Note LDV
042400070327     c
042500070322     c* Blocco consegna ARBFBC
042600070329    1c                   if        vh5in10='1'
042700070322     c
042800031017     C* CONTROLLO FLAG BLOCCO MODIFICATO
042900070329    2C     OLDFBC        IFNE      NEWFBC
043000031017     C*
043100070322     C* POSSO SOLO BLOCCARLA CON "B"
043200070329    3C     NEWFBC        IFNE      ' '
043300070322     C     NEWFBC        ANDNE     'B'
043400070322     C                   SETON                                        344590
043500070322     C                   Z-ADD     PRIMA         REC
043600070329    3C                   END
043700070507     c* Non posso bloccare con B se in distinta
043800070507     c                   if        newfbc='B' and newfbc<>oldfbc and vh5ndc>0
043900070507     C                   SETON                                        345690
044000070507     C                   Z-ADD     PRIMA         REC
044100070507    3C                   END
044200031017     C*
044300031017     C* SE ERA 'G' NON POSSO VARIARE SE C'E' RECORD DI GIACENZA
044400070329    3C  N90OLDFBC        IFEQ      'G'
044500031017     C*
044600031017     C* VERIFICO SE LA BOLLA IN QUESTIONE E' UNA FIGLIA; SE E' UNA FIGLIA
044700031017     C* E LA MAMMA HA UNA GIACENZA APERTA NON SI PUO' SBLOCCARE
044800031017     C     KARB2         CHAIN     FNLBL01L                           55
044900070329    4C     *IN55         IFEQ      '0'
045000031017     C     LBLLAP        ANDNE     LBLLAN
045100031017     C                   SETON                                        55
045200070329    4C                   END
045300031017     C*
045400050331     C   55KGCA          CHAIN     Tigcp21l                           40
045500050331     C  N55KLEG          CHAIN     Tigcp21l                           40
045600031017     C*
045700031017     C* SE TROVO GIACENZA MA E' ANNULLATA POSSO SBLOCCARE LA SPEDIZIONE
045800031017     C* O CON FASE >= 35
045900070329RE  4C  N40GCPFAS        IFGE      35
046000070329RE   C     GCPFAS        andne     36
046100031017     C     GCPATB        ORNE      ' '
046200031017     C                   SETON                                        40
046300070329    4C                   END
046400031017     C*
046500031017     C* 40 OFF - GIACENZA TROVATA
046600070329    4C     *IN40         IFEQ      *OFF
046700031017     C                   SETON                                        344690
046800031017     C                   Z-ADD     PRIMA         REC
046900070329    4C                   END
047000070329    3C                   END
047100031017     C*
047200031017     C* SE LA BOLLA E' UNA MAMMA NON LA SI PUO' SBLOCCARE
047300070322     C     KARB2         CHAIN     FNLBL02L                           32
047400070329    3C     *IN32         IFEQ      *OFF
047500031017     C                   SETON                                        344790
047600031017     C                   Z-ADD     PRIMA         REC
047700070329    3C                   END
047800070322     c
047900031117      * Se non è richiamato e posso gestire il video
048000070329    3c                   If        dr2vid = ' '  and
048100031117      * se devo bloccare la spedizione con blocco 'B'
048200031117     c                             newfbc = 'B'  and
048300031117      * se non è già stata inserita e se non ci sono errori
048400031117     c                             wokpsw = *Blanks and Not *In90
048500031117     c                   Exsr      Sr_Gespsw
048600070329    3c                   EndIf
048700070329    2C                   END
048800070329    1c                   EndIf
048900070322     c
049000070322     c* Per 11 on sospensione consegna
049100070322     c                   if        vh5in11='1'
049200070322     c
049300070322    1C     OLDBGE        IFNE      NEWBGE
049400070322     C*
049500070322     C* POSSO SOLO BLOCCARLA CON "S"
049600070322    4C     NEWBGE        IFNE      ' '
049700070322     C     NEWBGE        ANDNE     'S'
049800070322     C                   SETON                                        345190
049900070322     C                   Z-ADD     PRIMA         REC
050000070322    4C                   END
050100070322     c* Motivo obbligatortio se modificato il blocco
050200070322     c                   if        vi5nobnew=*blanks
050300070322     C                   SETON                                        345090
050400070322     C                   Z-ADD     PRIMA         REC
050500070322    4C                   END
050600070322    4C                   END
050700070322    4C                   END
050800070322     C*
050900031117
051000070417     c     ENDCTS        endsr
051100031117      *---------------------------------------------------------------*
051200031117      * Gestione videata password x blocco/sblocco                    *
051300031117      *---------------------------------------------------------------*
051400031117     c     Sr_Gespsw     BegSr
051500031117
051600031117     c                   Clear                   w01psw
051700031117     c                   Do        *Hival
051800031117     c                   Exfmt     lr39w01
051900031117     c                   Eval      *In28 = *Off
052000031117     c   kl              Leave
052100031117     c                   If        w01psw = *Blanks
052200031117     c                   Eval      w01msg = 'Immettere la password'
052300031117     c                   Eval      *In28 = *On
052400031117     c                   Iter
052500031117     c                   EndIf
052600031117     c                   If        w01psw <> mbbpsw
052700031117     c                   Eval      w01msg = 'Password errata'
052800031117     c                   Eval      *In28 = *On
052900031117     c                   Iter
053000031117     c                   EndIf
053100031117     c                   Eval      wokpsw = '1'
053200031117     c                   Leave
053300031117     c                   EndDo
053400031117
053500031117     c                   Endsr
053600031117
053700031017     C* OPERAZIONI INIZIALI ------------------------------------------*
053800941109     C     *INZSR        BEGSR
053900941110     C*
054000941110     C     *ENTRY        PLIST
054100941110     C                   PARM                    KPJBA
054200941110     C                   MOVEL     KPJBU         DSLR02
054300031103     c                   if        dr2vid = ' '
054400031103     C                   OPEN      FNlr39d
054500031103     c                   end
054600031017     c* disabilito il comit su FNARB a meno che non sia espressamente
054700031017     c* richiesto ('1')
054800031017     c                   if        dr2comit <> '1'
054900031017     c                   movel     '0'           dr2comit
055000031017     c                   end
055100031020     C                   OPEN      FNARB01L
055200070321     c
055300070321     c                   time                    w0140            14 0
055400070321     c                   move      w0140         w0080             8 0
055500070321     c     *eur          movel     w0080         dataeur
055600070321     c     *iso          movel     dataeur       dateu             8 0
055700941109     C**
055800950918     C* KLIST
055900941109     C**
056000941109     C     KARB          KLIST
056100941109     C                   KFLD                    VIDAAS
056200941109     C                   KFLD                    VIDLNP
056300941109     C                   KFLD                    VIDNRS
056400941109     C                   KFLD                    VIDNSP
056500070321     C     KAR4          KLIST
056600070321     C                   KFLD                    arbAAS
056700070321     C                   KFLD                    arbLNP
056800070321     C                   KFLD                    arbNRS
056900070321     C                   KFLD                    arbNSP
057000070321     C                   KFLD                    ktrc
057100070322     C     KARN          KLIST
057200070322     C                   KFLD                    arbAAS
057300070322     C                   KFLD                    arbLNP
057400070322     C                   KFLD                    arbNRS
057500070322     C                   KFLD                    arbNSP
057600070322     C                   KFLD                    kcdn
057700941109     C*
057800941109     C     KLEG          KLIST
057900941109     C                   KFLD                    LBLAAP
058000941109     C                   KFLD                    LBLLPP
058100941109     C                   KFLD                    LBLNRP
058200941109     C                   KFLD                    LBLNSP
058300950307     C                   KFLD                    KFRG
058400950307     C*
058500950307     C     KGCA          KLIST
058600950307     C                   KFLD                    VI5AAS
058700950307     C                   KFLD                    VI5LNP
058800950307     C                   KFLD                    VI5NRS
058900950307     C                   KFLD                    VI5NSP
059000990713     C                   KFLD                    KFRG              2 0
059100950307     C                   Z-ADD     0             KFRG
059200941109     C*
059300941109     C     KARB2         KLIST
059400941109     C                   KFLD                    VI5AAS
059500941109     C                   KFLD                    VI5LNP
059600941109     C                   KFLD                    VI5NRS
059700941109     C                   KFLD                    VI5NSP
059800070321     C     KAR42         KLIST
059900070321     C                   KFLD                    VI5AAS
060000070321     C                   KFLD                    VI5LNP
060100070321     C                   KFLD                    VI5NRS
060200070321     C                   KFLD                    VI5NSP
060300070321     C                   KFLD                    ktrc
060400950918     C*
060500950918     C     KART          KLIST
060600950918     C                   KFLD                    KAAS
060700950918     C                   KFLD                    KLNP
060800950918     C                   KFLD                    KNRS
060900950918     C                   KFLD                    KNSP
061000950918     C     KANM          KLIST
061100990513     C                   KFLD                    ARTAAS
061200950918     C                   KFLD                    KLNP
061300950918     C                   KFLD                    KNRS
061400950918     C                   KFLD                    KNSP
061500950918     C                   KFLD                    KCAA
061600950918     C*
061700950918     C     KLBL          KLIST
061800950918     C                   KFLD                    KAAS
061900950918     C                   KFLD                    KLNP
062000950918     C                   KFLD                    KNRS
062100950918     C                   KFLD                    KNSP
062200941109     C**
062300941109     C                   Z-ADD     0             NDC               7 0
062400941109     C**
062500941109     C                   Z-ADD     1             CODUT
062600941109     C                   CALL      'X§PARUT'
062700941109     C                   PARM                    UT§DSE
062800941109     C                   MOVEL     RAGUT         VIDDSA
062900941109     C                   MOVEL     REC80         CNCR80
063000941109     C**
063100020424     C* VEDO SE SONO PRIMO O SECONDO LIVELLO
063200941109     C**
063300020424     C                   Z-ADD     SIMPOU        VIDFIL
063400020424     C     SIMTPP        IFEQ      '2'
063500020424     C     SIMTPP        OREQ      *BLANKS
063600020424     C                   SETON                                        05
063700020424     C                   ENDIF
063800941109     C*
063900941109     C                   EXSR      CARL6
064000941109     C*
064100950918     C*
064200950918     C*  DEFINIZIONE VARIABILI
064300950918     C     *LIKE         DEFINE    ANMCAA        KCAA
064400950918     C     *LIKE         DEFINE    ARBAAS        KAAS
064500950918     C     *LIKE         DEFINE    ARBLNP        KLNP
064600950918     C     *LIKE         DEFINE    ARBNRS        KNRS
064700950918     C     *LIKE         DEFINE    ARBNSP        KNSP
064800031117
064900031117      * Aggancio tabella MBB
065000031117     c                   Clear                   mbbpsw
065100031117     c                   Clear                   Tibs02ds
065200031117     c                   Eval      T02Mod = 'C'
065300031117     c                   Eval      T02Sif = knsif
065400031117     c                   Eval      T02Cod = 'MBB'
065500031117     c                   Movel(p)  'TUTTI'       T02Ke1
065600031117     c                   Call      'TIBS02R'
065700031117     c                   Parm                    Kpjba
065800031117     c                   Parm                    Tibs02ds
065900031117     c                   If        T02Err = *Blanks
066000031117     c                   Movel     T02Uni        mbbpsw
066100031117     c                   EndIf
066200031117
066300941109     C                   ENDSR
066400920813     C*
066500920813     C* CARICO L6 ------------------------------------------***********
066600920813     C     CARL6         BEGSR
066700941109     C*
066800941109     C                   CLEAR                   DSUL06
066900941109     C                   MOVE      '£6'          D06COD
067000941109     C                   MOVEL     VIDFIL        D06KEY
067100941109     C                   MOVEL     DSUL06        KPJBU
067200941109     C                   CALL      'TRUL06R'
067300941109     C                   PARM                    KPJBA
067400941109     C                   MOVEL     KPJBU         DSUL06
067500941109     C                   MOVEA     LIN           L6
067600070321     c
067700070321     c* Verifico se ha geo attiva
067800070321     c     vidfil        chain     azorg01l
067900070321     c                   if        %found(azorg01l)
068000070321     C                   MOVEL     ORGDES        DESFIL
068100070321     c                   movel     orgde8        og148
068200070321     c                   eval      FGSGEOT=§oggeot
068300070321     c                   else
068400070321     c                   clear                   FGSGEOT
068500070321     c                   eval      desfil=*all'?'
068600070321     c                   endif
068700070321     c
068800070321     c                   if        fgsgeot='S'
068900070321     c                   seton                                        03
069000070329     C                   MOVE      TES(2)        VIDTES
069100070321     c                   else
069200070321     c                   setoff                                       03
069300070329     C                   MOVE      TES(1)        VIDTES
069400070321     c                   endif
069500070321     c
069600920813     C                   ENDSR
069700900509     C*
069800900509     C*--- CONTROLLI FORMATO1 ----------------------------------------*
069900900509     C     CONTR         BEGSR
070000911025     C                   SETOFF                                       9001
070100070322     c                   clear                   decfbc
070200070321     C* SELEZIONO PER DATA SPEDIZIONE O PER blocco SPED
070300901115     C     SEL1          IFEQ      0
070400941109     C     VIDFBC        ANDEQ     ' '
070500911025     C                   SETON                                        4090
070600901115     C                   GOTO      ENDCTR
070700901115     C                   END
070800901115     C     SEL1          IFNE      0
070900941109     C     VIDFBC        ANDNE     ' '
071000911025     C                   SETON                                        4090
071100901115     C                   GOTO      ENDCTR
071200901115     C                   END
071300901115     C* DATA SPEDIZIONE
071400901115     C     SEL1          IFNE      0
071500031017     C* SPEDIZIONE POSTE: non controllo se pgm richiamato e il parametro
071600031017     C* dr2pos = 'N'
071700070321     c**                 if        dr2pos = ' '
071800070321     C**                 CLEAR                   §OGPT
071900070321     C**   VIDLNP        CHAIN     AZORG01L                           30
072000070321     C**N30              MOVEL     ORGDE3        OG143
072100070321     C**   §OgNTW        IFEQ      'PPT'
072200070321     C**                 SETON                                        52  90
072300070321     C**                 GOTO      ENDCTR
072400070321     C**                 ENDIF
072500070321     c**                 end
072600070321     c
072700911025     C* SPEDIZIONE INESISTENTE O GIA' CONSEGNATA
072800070329     C     KARB          CHAIN     FNARB01L                           3031
072900911025     C   30              SETON                                        4190
073000911025     C   30              GOTO      ENDCTR
073100070329     c
073200070329     c                   if        *in31
073300070329     C                   SETON                                        5390
073400070329     C                   GOTO      ENDCTR
073500070329     c                   endif
073600911030     C*
073700911030     C* SPEDIZIONE NON IN GESTIONE
073800911030     C     ARBLNA        LOOKUP    L6                                     30
073900911030     C  N30              SETON                                        4390
074000911030     C  N30              GOTO      ENDCTR
074100911025     C*
074200941109     C* SPEDIZIONE IN CONSEGNA O CONSEGNATA
074300070507     C  n03ARBNDC        IFNE      0
074400941109     C     ARBDCM        ORNE      0
074500911025     C                   SETON                                        4290
074600911025     C                   GOTO      ENDCTR
074700911025     C                   END
074800070507     c
074900070507     c* Per geo attiva si può mettere in sospensione se in distinta
075000070507     C   03ARBDCM        ifne      0
075100070507     C                   SETON                                        4190
075200070507     C                   GOTO      ENDCTR
075300070507     C                   END
075400911025     C*
075500911025     C                   SETON                                        01
075600901115     C                   END
075700070321     c
075800070321     c* Se GEOT non attiva non posso selezionare blocco "S"
075900070321     c                   if        not *in03 and vidfbc='S'
076000070321     C                   SETON                                        4490
076100070321     C                   GOTO      ENDCTR
076200070321     c                   endif
076300070328     c* Se GEOT     attiva non posso selezionare blocco "A"
076400070328     c                   if            *in03 and vidfbc='A'
076500070328     C                   SETON                                        4690
076600070328     C                   GOTO      ENDCTR
076700070328     c                   endif
076800070322     c* Imposto indicatore di visualizzazione tipo blocco
076900070322     c                   select
077000070322     c                   when      vidfbc=' '
077100070322     c                   setoff                                       101112
077200070322     c
077300070322     c                   when      vidfbc='S'
077400070322     c                   setoff                                       1012
077500070322     c                   seton                                        11
077600070322     c                   eval      decfbc=decblo(3)
077700070322
077800070322     c                   other
077900070322     c*SFLDROP attivo
078000070322     c                   setoff                                       11
078100070322     c                   seton                                        1012
078200070322     c                   if        vidfbc='B'
078300070329     C* SE GEO ATTIVA SI CHIAMANO RIMPALLI
078400070329     C                   if        fgsgeot='S'
078500070329     c                   eval      decfbc=decblo(5)
078600070329     c                   else
078700070322     c                   eval      decfbc=decblo(4)
078800070322     c                   endif
078900070329     c                   endif
079000070322     c                   if        vidfbc='A'
079100070322     c                   eval      decfbc=decblo(1)
079200070322     c                   endif
079300070322     c                   if        vidfbc='G'
079400070322     c                   eval      decfbc=decblo(2)
079500070322     c                   endif
079600070322     c
079700070322     c                   endsl
079800911023     C*
079900900514     C     ENDCTR        ENDSR
080000900515     C*
080100911025     C*--- CARICO SUBFILE --------------------------------------------*
080200911025     C     CARSF         BEGSR
080300900517     C*
080400900518     C                   Z-ADD     COMOD         NRR
080500941109     C*
080600070322    1c                   dow       not *in33
080700070321     c
080800070321     c* se 30 on vado avanti a leggere la L6
080900070321    2c                   if        *in30
081000070321     c                   exsr      LEGL6
081100070321    2c                   endif
081200911030     C*
081300070321    2C     *IN30         IFEQ      '0'
081400070321     C     klna          READE     FNARB05L                               30
081500070321     C
081600070321     c* Leggo le non consegnate e non in distinta
081700070507    3C                   if         not *in30 and arbdcm=0
081800070507     c
081900070507     c* se ho chiesto la sospensione consegna, non eescludo quelle in di
082000070507     c*  distinta
082100070508   3aC                   if        vidfbc='S' or (vidfbc<>'S' and arbndc=0)
082200070323     c                   clear                   wesca             1
082300070323     c
082400070323     c* Vedo se pazializzazione per c.a. solo se GEO attiva
082500070323    4c                   if        fgsgeot='S'
082600070323     c* richiamo fidn12r per vedere se c'e' c.a.
082700070323     c                   clear                   fidn12ds
082800070323     c                   eval      i12aas = arbaas
082900070323     c                   eval      i12lnp = arblnp
083000070323     c                   eval      i12nrs = arbnrs
083100070323     c                   eval      i12nsp = arbnsp
083200070323     c                   eval      i12fel = 'E'
083300070323     c                   eval      i12fan = 'E'
083400070323      *
083500070323     c                   call      'FIDN12R'
083600070323     c                   parm                    fidn12ds
083700070323      *
083800070323      * se non ci sono errori
083900070323    5c                   if        o12err = ' '
084000070323      * se numero di CA trovate maggiore di zero
084100070323    6c                   if        o12nca > 0
084200070323     c
084300070323     c                   z-add     *zeros        ii                2 0
084400070323    7c     1             do        o12nca        ii
084500070323     C                   Z-ADD     skdch(ii)     dctdch
084600070323      * se CA aperta imposto il flag e vado a fine
084700070323    8c                   If        dctdch = 0
084800070323     c                   eval      wesca = 'S'
084900070323     c                   leave
085000070323    8c                   endif
085100070323    7C                   ENDDO
085200070323      *
085300070323    6c                   endif
085400070323    5c                   endif
085500070323    4c                   endif
085600070323     c*
085700070323    4c                   if        fgsgeot=' ' or vidca=' ' or
085800070323     c                             (vidca='N' and wesca=' ') or
085900070323     c                             (vidca='S' and wesca='S')
086000070321     c
086100070321     c                   EXSR      CHAINAR4
086200070321     c
086300941109     C* SE CORRISPONDE AL BLOCCO  DIGITATO CARICO
086400070323    5c                   if        (vidfbc<>'S' and arbfbc=vidfbc)  or
086500070321     c                             (vidfbc='S' and §b4lbge='S')
086600070321     c
086700911025     C                   EXSR      RIEMPI
086800070323    5C                   ENDif
086900070323    4C                   ENDif
087000070508   3aC                   ENDif
087100070508    3C                   ENDif
087200911025     C*
087300070321    2C                   END
087400070321    1C                   END
087500900516     C*
087600911219     C                   Z-ADD     NRR           COMOD             4 0
087700900515     C                   ENDSR
087800070321     C*
087900070321     C*--- Leggo filiali della L6 ------------------------------------*
088000070321     C     LegL6         BEGSR
088100070321     c                   add       1             LL                3 0
088200070321     c                   if        l6(LL)>0
088300070321     c                   setoff                                       30
088400070321     c                   z-add     l6(LL)        klna
088500070321     c     klna          setll     fnarb05l
088600070321     c                   else
088700070321     c                   seton                                        3033
088800070321     c                   endif
088900070321     c
089000070321     C                   ENDSR
089100900515     C*
089200900515     C* EMISSIONE SUBFILE -------------------------------------------
089300911025     C     RIEMPI        BEGSR
089400070329     c                   setoff                                       13
089500070323     C                   MOVEL     *BLANKS       VI5SCE
089600070323     C                   MOVEL     *BLANKS       VI5ca
089700070323     c
089800911025     C                   MOVEL     ARBLNP        VI5LNP
089900911025     C                   MOVEL     ARBLNA        VI5LNA
090000911023     C                   MOVEL     ARBMGS        MMGG
090100900516     C                   MOVEL     GG            VI5DSP
090200900516     C                   MOVE      MM            VI5DSP
090300911023     C                   MOVE      ARBAAS        VI5AAS
090400900516     C*
090500911023     C                   MOVEL     ARBNSP        VI5NSP
090600911023     C                   MOVEL     ARBRSD        VI5RSD
090700911025     C                   MOVEL     ARBRSM        VI5RSM
090800911023     C                   MOVEL     ARBNRS        VI5NRS
090900911023     C                   MOVEL     ARBNCD        VI5SCD
091000911023     C                   MOVEL     ARBNCA        VI5SCA
091100911025     C                   MOVEL     ARBTSP        VI5TSP
091200070507     c                   z-add     arbndc        vh5ndc
091300070322     c
091400070322     c* Per la videata nuova, imposto i segnacolli il un unico campo
091500070322     c                   if        fgsgeot='S'
091600070329     c*
091700070329     c* Se nel blocco consegna c'e' "A" non lo visulizzo
091800070329     c                   if        ARBFBC='A'
091900070329     c                   seton                                        13
092000070329     c                   endif
092100070323     c
092200070323     c* Presente C.A.
092300070323     c                   if        wesca='S'
092400070323     c                   eval      vi5ca='C.A'
092500070323     c                   endif
092600070323     c
092700070322     c* se non sequenziali lo scrivo
092800070322     c                   if        arbfns='S'
092900070322     c                   eval      vi5scn='NON SEQUENZIALI'
093000070322     C                   ELSE
093100070322     c                   eval      vi5scn=%editc(arbncd: 'Z')+'/' +
093200070322     c                             %editc(arbnca: 'Z')
093300070322     C                   ENDIF
093400070322     C
093500070326     C*Altre Gestioni: richiamo pgm FNLRG6R
093600070322     c                   clear                   vi5ges
093700070323     C                   MOVEL     *BLANKS       VI5gesd
093800070326     c*
093900070326     c                   clear                   fnlrg6ds
094000070326     c                   eval      ilrg6aas=arbaas
094100070326     c                   eval      ilrg6lnp=arblnp
094200070326     c                   eval      ilrg6nrs=arbnrs
094300070326     c                   eval      ilrg6nsp=arbnsp
094400070329     c* Escludo dalla visualizzazione il blocco generico
094500070327     c                   movel     'O'           ilrg6nb2
094600070326     c                   call      'FNLRG6R'
094700070326     c                   parm                    fnlrg6ds
094800070326     c                   parm                    dsrecarb
094900070326     c                   parm                    DSrecar4
095000070326     c                   movel     olrg6vssin    vi5ges
095100070323     c
095200070322     c* chain fiarn ultimo record motivo sospensione generica
095300070322     c     karn          chain     fiarn12l
095400070322     c                   if        %found(fiarn12l)
095500070322     c                   movel     arnnob        vi5nobold
095600070322     c                   else
095700070322     c                   clear                   vi5nobold
095800070322     c                   endif
095900070322     c                   clear                   vi5nobnew
096000070322     c                   endif
096100070322     c* 11 on - blocco sospensine consegna
096200070322     c
096300070322     c                   movel     §b4lbge       oldBGE
096400070322     c                   movel     §b4lbge       NEWBGE
096500070322     c
096600070321     C                   MOVEL     ARBFBC        OLDFBC
096700070321     C                   MOVEL     ARBFBC        NEWFBC
096800070322     c                   movel     *in11         vh5in11
096900070322     c                   movel     *in10         vh5in10
097000070321     c
097100070321     c* Se richiamato passo il richiamo
097200031017     c                   if        dr2vid = 'N'
097300031017     c                   movel     dr2bs         newfbc
097400031017     c                   end
097500900522     C*
097600911025     C                   ADD       1             NRR
097700031103     c                   if        dr2vid = ' '
097800070322     c                   if        fgsgeot=' '
097900941109     C                   WRITE     LR39DSF
098000070322     c                   else
098100070322     C                   WRITE     LR39DSF2
098200070322     c                   endif
098300070322     c                   end
098400900515     C                   ENDSR
098500911025     C*
098600911025     C*--- AGGIORNO BOLLE --------------------------------------------*
098700911025     C     AGGIOR        BEGSR
098800030613      *
098900030613      * Se richiesta la stampa delle bolle sbloccate
099000030613      *   richiedo i dati per la stampa ed eseguo le override
099100030613if  1c                   if            VIDsta  = 'S'
099200030613     c                             and $trul90 = *off
099300030613     c                   eval      $trul90 = *on
099400030613     c                   exsr      SR_OvrPrtF
099500030613e   1c                   endif
099600030613      *
099700911025     C                   Z-ADD     1             NRR
099800070322     c                   if        fgsgeot=' '
099900941109     C                   READC     LR39DSF                                34
100000070322     c                   else
100100070322     C                   READC     LR39DSF2                               34
100200070322     c                   endif
100300911025     C*
100400070322    1C     *IN34         DOWEQ     '0'
100500070321     c* Gestisco blocco "sospensione consegna" a parte
100600070322    2c                   if        vh5in11='1'
100700070321     c                   exsr      AggioAR4
100800070322    2c                   endif
100900070322     c
101000070322    2c                   if        vh5in10='1'
101100070321     c                   EXSR      Aggior1
101200911025     C*
101300070321     C* SE ERA UN LASCIATO AVVISO, SBLOCCATO --> RISTAMPO LA BOLLA
101400070322    3C     OLDFBC        IFEQ      'A'
101500941220     C     NEWFBC        ANDEQ     *BLANKS
101600941220     C     VIDSTA        ANDEQ     'S'
101700030613     c                   clear                   FNLSB5DS
101800941220     C                   MOVEL     'A'           DB0TBO
101900941220     C                   MOVEL     'R'           DB0RIS
102000941220     C                   MOVEL     ARBAAS        DB0AAS
102100941220     C                   MOVEL     ARBLNP        DB0LNP
102200941220     C                   MOVEL     ARBNRS        DB0NRS
102300941220     C                   MOVEL     ARBNSP        DB0NSP
102400030613     c                   call      D90psl
102500030613     c                   parm                    FNLSB5DS
102600070322    3C                   ENDIF
102700070322    1c                   endif
102800911025     C*
102900070322     c                   if        fgsgeot=' '
103000941109     C                   READC     LR39DSF                                34
103100070322     c                   else
103200070322     C                   READC     LR39DSF2                               34
103300070322     c                   endif
103400070322     c
103500070321     C                   ENDdo
103600911025     C                   ENDSR
103700070321     C*
103800070321     C*--- AGGIORNO BOLLE record FIAR4 -------------------------------*
103900070321     C     AGGIOar4      BEGSR
104000070321     c
104100070322     C* SCRIVO MOTIVO BLOCCO/SBLOCCO SOSPENSIONE CONSEGNA
104200070323     c* se se pieno
104300070323     c                   if        vi5nobnew<>*blanks
104400070322     C                   CLEAR                   FIARN000
104500070322     C                   EVAL      ARNAAS=vi5AAS
104600070322     C                   EVAL      ARNLNP=vi5LNP
104700070322     C                   EVAL      ARNNRS=vi5NRS
104800070322     C                   EVAL      ARNNSP=vi5NSP
104900070322     C                   EVAL      ARNCDN=kcdn
105000070322     C                   EVAL      ARNpno=001
105100070322     C                   EVAL      ARNnob=vi5nobnew
105200070402     C                   EVAL      ARNfl1=newbge
105300070322     c                   eval      arndim=dateu
105400070322     c                   time                    arnoim
105500070322     c                   eval      arnpru=knmus
105600070322     c                   eval      arnpgm='FNLR39R'
105700070322     c                   write     fiarn000
105800070323     c                   endif
105900070323     c
106000070322     c
106100070321     C     KAR42         CHAIN     Fiar401L                           31
106200070321     c                   if        %found(fiar401l)
106300070321     c                   movel     ar4not        dsbl4l
106400070322     c                   movel     newBGE        §b4lBGE
106500070321     c                   movel     dsbl4l        ar4not
106600070321     c                   movel     dateu         ar4duv
106700070321     c                   update    fiar4000
106800070322     c                   else
106900070322     c                   clear                   fnlv19ds
107000070322     c                   clear                   dsbl4l
107100070322     c                   movel     newBGE        §b4lBGE
107200070322     c                   movel     dsbl4l        d19nt1
107300070322     C                   MOVEL     vi5AAS        D19AAS
107400070322     C                   MOVEL     vi5lNP        D19LNP
107500070322     C                   MOVEL     vi5NRS        D19NRS
107600070322     C                   MOVEL     vi5NSP        D19NSP
107700070322     C                   MOVEL     'L'           D19TRC
107800070322     C                   MOVEL     'T'           D19FTR
107900070322     C                   CLEAR                   D19NT2
108000070322     C                   CALL      'FNLV19R'
108100070322     C                   PARM                    fnlv19ds
108200070321     c                   endif
108300070321     c
108400070321     C                   ENDSR
108500070321     c
108600070321     C*--- AGGIORNO BOLLE record FNARB -------------------------------*
108700031103     C     AGGIOR1       BEGSR
108800031103     C*
108900031103     C     KARB2         CHAIN     FNARB01L                           31
109000070329     c
109100031103     C* SOLO SE MODIFICO  IL BLOCCO CONSEGNA
109200031103     C     NEWFBC        IFNE      ARBFBC
109300031103     C                   MOVEL     *BLANKS       ARBCMC
109400031103     C  N31              MOVEL     NEWFBC        ARBFBC
109500031103     C  N31              UPDATE    FNARB000
109600031103     C* SE RIMETTO IN CONSEGNA LA BOLLA CHIUDO EVENTUALI ANOMALIE
109700031103     C* DI GIACENZA
109800031103     C     NEWFBC        IFEQ      *BLANKS
109900031103     C                   EXSR      CLOANM
110000031103     C                   END
110100070329     c
110200070329     c                   else
110300070329     c                   unlock    fnarb01l
110400070329     C                   ENDIF
110500031103     C                   ENDSR
110600950918     C*-----------------------------------------------------*
110700950918     C*  Chiudo anomalie 140 su dettaglio segnacolli
110800950918     C*-----------------------------------------------------*
110900950918     C     CLOANM        BEGSR
111000950918     C*
111100950918     C*  Controllo se bolla con segnacolli
111200950918     C                   Z-ADD     ARBAAS        KAAS
111300950918     C                   Z-ADD     ARBLNP        KLNP
111400950918     C                   Z-ADD     ARBNRS        KNRS
111500950918     C                   Z-ADD     ARBNSP        KNSP
111600090309     C     KART          CHAIN     FiART01L                           38
111700950918     C*  Se non esiste e spedizione figlia mi posiziono su mamma
111800950918     C     *IN38         IFEQ      '1'
111900950918     C     KLBL          CHAIN     FNLBL01L                           38
112000950918     C     *IN38         IFEQ      '0'
112100950918     C     LBLLAP        ANDEQ     LBLLAN
112200950918     C                   Z-ADD     LBLAAP        KAAS
112300950918     C                   Z-ADD     LBLLPP        KLNP
112400950918     C                   Z-ADD     LBLNRP        KNRS
112500950918     C                   Z-ADD     LBLNSP        KNSP
112600090309     C     KART          CHAIN     FiART01L                           38
112700950918     C                   END
112800950918     C                   END
112900950918     C*  Se ho trovato i segnacolli vado a chiudere le anomalie
113000950918     C     *IN38         IFEQ      '0'
113100950918     C                   Z-ADD     140           KCAA
113200960415     C                   CLEAR                   DSLR33
113300960415     C                   MOVE      '4'           D33FSC
113400070321     C                   MOVE      dateu         D33DCH
113500960415     C                   MOVEL     'DI'          D33CCH
113600990513     C     KANM          CHAIN     FNANM01L                           38
113700960415     C  N38              MOVE      '1'           W33R              1
113800950918     C     *IN38         DOWEQ     '0'
113900960415     C                   Z-ADD     ANMNR1        D33NRR
114000960415     C                   CALL      'FNLR33R'
114100960415     C                   PARM                    DSLR33
114200990513     C     KANM          READE     FNANM01L                               38
114300960415     C                   ENDDO
114400950918     C*
114500950918     C                   END
114600950918     C*
114700950918     C                   ENDSR
114800070321     C*
114900070321     C* CHain record fiar4 record L ----------------------------------*
115000070321     C     CHAINAR4      BEGSR
115100070321     c                   clear                   dsbl4L
115200070321    1c                   if        fgsgeot='S'
115300070321     c     kar4          chain(N)  fiar401l
115400070321    2c                   if        %found(fiar401l)
115500070321     c                   movel     ar4not        dsbl4L
115600070328     c                   else
115700070328     c                   clear                   fiar4000
115800070321    2c                   endif
115900070321    1c                   endif
116000070321     C                   ENDSR
116100941109     C*
116200941109     C* CONTROLLO LA FILIALE IN GESTIONE -----------------------------*
116300941109     C     CTRFGS        BEGSR
116400941109     C                   SETOFF                                       90
116500941109     C                   CLEAR                   DSLV50
116600941109     C                   MOVEL     KNMUS         D50PRU
116700941109     C                   MOVEL     VIDFIL        D50FGS
116800941109     C                   CALL      'FNLV50R'
116900941109     C                   PARM                    DSLV50
117000941109     C*
117100941109     C     D50ERR        IFNE      ' '
117200941109     C                   MOVEL     D50MSG        VIDMSG
117300941109     C                   SETON                                        499028
117400941109     C                   GOTO      ENDCTF
117500941109     C                   ENDIF
117600070321     c
117700070321     C* CARICO LA L6   e decodifica
117800941109     C                   EXSR      CARL6
117900941109     C*
118000941109     C     ENDCTF        ENDSR
118100030613
118200030613      *---------------------------------------------------------------*
118300030613      * Richiedo i dati per la stampa   ed   Eseguo le override       *
118400030613      *---------------------------------------------------------------*
118500030613     c     SR_OvrPrtF    BEGSR
118600030613      *
118700030613     c                   call      'TRUL90R'
118800030613     c                   parm                    KPJBA
118900030613     c                   parm                    TRUL90DS
119000030613      * - F3 > Fine
119100030613     c     D90F3         cabeq     *on           End_Ovr
119200030613      *
119300030613      * - FNLSB5PA4 > bolle laser in formato A4
119400030613     c                   eval      wFILE = 'FNLSB5PA4 '
119500030613     c                   eval      wOUTQ = D90prb4
119600030613     c                   eval      wFORM = D90mdb4
119700030613     c***                eval      wUSRD = 'Bolle.Sbl.'
119800030613     c                   exsr      SR_x_Cmd
119900030613     c     *in90         cabeq     *on           End_Ovr
120000030613      *
120100030613      * - FNLSB5PA5 > bolle laser in formato A5
120200030613     c                   eval      wFILE = 'FNLSB5PA5 '
120300030613     c                   eval      wOUTQ = D90prb5
120400030613     c                   eval      wFORM = D90mdb5
120500030613     c***                eval      wUSRD = 'Bolle.Sbl.'
120600030613     c                   exsr      SR_x_Cmd
120700030613     c     *in90         cabeq     *on           End_Ovr
120800030613      *
120900030613     c     End_Ovr       ENDSR
121000030613
121100030613      *---------------------------------------------------------------*
121200030613
121300030613     c     SR_x_Cmd      BEGSR
121400030613      *
121500030613      * impostazione comando da eseguire
121600030613     c                   eval      Qcmd = %trim($Cmd(1)) + ' '
121700030613     c                                  + %trim($Cmd(2))
121800030613      * - nome del file
121900030613     c                   eval      Qcmd = %replace(wFILE:Qcmd:
122000030613     c                                    %scan('&FILE':Qcmd))
122100030613      * - nome della coda di stampa
122200030613     c                   eval      Qcmd = %replace(wOUTQ:Qcmd:
122300030613     c                                    %scan('&OUTQ':Qcmd))
122400030613      * - tipo di modulo
122500030613     c                   eval      Qcmd = %replace(wFORM:Qcmd:
122600030613     c                                    %scan('&FORMTYPE':Qcmd))
122700030613      * - dati utente
122800030613     c                   eval      Qcmd = %replace(wUSRD:Qcmd:
122900030613     c                                    %scan('&USRDTA':Qcmd))
123000030613      *
123100030613     c                   call      'QCMDEXC'                            90
123200030613     c                   parm                    Qcmd
123300030613     c                   parm                    Qlen
123400030613      *
123500030613     c                   ENDSR
123600030613
123700030613**  $Cmd
123800030613OVRPRTF    FILE(&FILE     ) OUTQ(&OUTQ     ) FORMTYPE('&FORMTYPE ')
123900030613           USRDTA('&USRDTA   ') SHARE(*YES)
124000030613           COPIES(2)
124100070322**  decodifica blocchi
124200070322  LASCIATI  AVVISO
124300070322      GIACENZE
124400070322SOSPENSIONE CONSEGNA
124500070322      BLOCCATE
124600070329      RIMPALLI
124700070329**  Testata
124800070329 ** BLOCCO / SBLOCCO SPEDIZIONI **
124900070329 * GESTIONE SOSPENSIONE CONSEGNA *
