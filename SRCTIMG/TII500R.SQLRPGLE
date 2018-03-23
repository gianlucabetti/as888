000100940211     H DECEDIT('0,') DATEDIT(*DMY.)
000200070316     h dftactgrp(*no) actgrp(*caller)
000300110826      *
000400110725      * TII500R *----------------------------------------------------*
000500110826      *     ?   - GESTIONE ORDINI MAGAZZINO -    ?
000600070403      *--------------------------------------------------------------*
000700110725      * Occorre verificare che l'utente sia abilitato ad utilizzare il pgm.
000800110725      * Il programma può caricare ordini già preconfezionati archiviati
000900110725      * su un file appositamente preparato oppure può inserire righe d'ordine
001000110725      * per soddisfare altre esigenze delle filiali nel riapprovigionamento
001100110725      * dei materiali dalla Logistica.
001200070227      *--------------------------------------------------------------*
001300110725      * Il n°d'ordine è univoco.
001400110725      *--------------------------------------------------------------*
001500940307      *  21           GENERICO OPERAZIONI I/O
001600940224      *  22           GENERICO ERRORE OPERAZIONI I/O
001700070301      *  23           GENERICO Operazioni varie
001800940224      *  30           SFLDSP
001900940224      * N31           SFLCLR
002000940224      *  31           SFLDSPCTL
002100940224      *  32           SFLNXTCHG
002200940224      *  33           SFLEND
002300940224      *  39           OF PRTF
002400070302      *  40 <---> 49  DSPATR SU video
002500940608      *  Specificare l'uso dei singoli indicatori
002600070227      *  50           Emissione della window totale/parziale
002700070227      *  51 <---> 90  ERRORI SU VIDEO
002800940506      *  97           ERRORE SPECIALE : TASTO   NON ABIL.
002900070829      *  98           ERRORE SPECIALE :
003000940224      *  99           INDIC. GENERALE DI ERRORE
003100940128     F*----------------------------------------------------*
003200070404      *  Output
003300110906     FTMorp01L  iF   E           K DISK    rename(tmORP000:tmorp01)
003400111012     FTMorp02L  uF   E           K DISK    infds(orpds) usropn
003500110906     FTMorp00f  uF   E             DISK    rename(tmORP000:tmorpfis) usropn
003600110906     FTmOPP01L  iF   E           K DISK    infds(oppds)
003700080609      *
003800070404      *  Input
003900110830     FazORG01l  if   e           k disk
004000110830     FtnTBE01l  if   e           k disk
004100110826     FanTAB01l  if   e           k disk
004200110921     FtMAMP01l  if   e           k disk    rename(tmAMP000:tmAMP01)
004300110921     FtMAMP10l  if   e           k disk
004400110725     Ftii500d   CF   E             WORKSTN
004500110826     F                                     SFILE(TII5S00:S0NRR)
004600110826     F                                     SFILE(TII5S01:S1NRR)
004700940201     F                                     INFDS(DSFMT)
004800070305     D*----------------------------------------------------*
004900070829     D Errmsg          S             78    DIM(40) CTDATA PERRCD(1)             MSG DI ERRORE
005000070326     D*----------------------------------------------------*
005100940211     D* Passaggio Parametri
005200940211     D KPJBA         E DS
005300070227     D KPJBUs          s                   like(KPJBU)
005400110726     D*-------------
005500110726     d ficn70ds      e ds
005600110826     d antab155      e ds
005700110826     d antab156      e ds
005800110901     d  og143        e ds
005900110830     d dAUT          e ds
006000110830     d Tii501DS      e ds
006100110909     d Tii502DS      e ds
006200940211     D*-------------
006300110824     d dsWEK         e ds
006400110905     D  weekWEK                       2    DIM(54)
006500110824     D*-------------
006600110830     D  SubClassi                     5    DIM(500)
006700111011     D  TIPOSCLAS                     1    DIM(500)
006800110830     D*-------------
006900940211     D DSFMT           DS           512
007000940506     D  $TASTO               369    369
007100940211     D  NRG                  370    370
007200940211     D  NCL                  371    371
007300940211     D  SFLNRR               378    379B 0
007400940127     D*-------------
007500940127     D* Reperimento nome PGM
007600940127     D STATUS         SDS           333
007700940127     D  DSPGM            *PROC
007800030113     D*-------------
007900110906     d oppds           ds
008000110906     d  opp_nrr              397    400b 0
008100110830     D*-------------
008200110830     d orpds           ds
008300110830     d  orp_nrr              397    400b 0
008400110725     D*-------------
008500110725     D*--- DATA AREA NUMERAZIONE ORDINI :
008600110725     D DTAORC          DS
008700110725     D  ORDNUO                 1      5  0
008800070320     D*-------------
008900070316      * Variabili appoggio sempre presenti in tutte le anagrafiche
009000110826$003 D S0NRR           S                   Like(C0rcd)
009100110826$003 D S0pag           S                   Like(C0rcd)
009200070227$003 D S1NRR           S                   Like(C1rcd)
009300070228$003 D S1pag           S                   Like(C1rcd)
009400070302$003 D WSfl            S                   Like(C1nrr)
009500030113$003 D Wmax            S                   Like(C1rcd)
009600030113$003 D Wpag            S                   Like(C1rcd)
009700070330$003 D sav_WSfl        S                   Like(WSfl)
009800070330$003 D sav_Wmax        S                   Like(Wmax)
009900070330$003 D sav_Wpag        S                   Like(Wpag)
010000070301$003 D Wpagine         S                   Like(C1rcd)
010100110824      *--------------------------
010200110826     D RigPag_SFl0     S              3  0 INZ(18)
010300110826     D RigPag_SFl1     S              3  0 INZ(18)
010400070227      *--------------------------
010500070418     D sav_d1cfgs      S                   Like(d1cfgs) inz(0)
010600110826$003 D sav_S0NRR       S                   Like(s0nrr)
010700110826$003 D sav_S1NRR       S                   Like(s1nrr)
010800101129      *--------------------------
010900110726     d dataiso         s               d   datfmt(*iso)
011000110825     d oggiiso         s               d   datfmt(*iso)
011100101129     d dataeur         s               d   datfmt(*eur)
011200070320      *--------------------------
011300110824     d wFGS            s                   like(d1cFGS)
011400110830     d kCOD1           s                   like(TBEcod)
011500070307     d kKEY1           s                   like(TBEke1)
011600070305      *--------------------------
011700110921     d Sotto_Classe    s                   like(ampSCM)
011800110921      *--------------------------
011900110906     D scritto_almeno_una_riga_ordine...
012000110906     D                 S              1A
012100110906     D premuto_F10...
012200110830     D                 S              1A
012300110909     D Cambia_Video...
012400110909     D                 S              1A
012500110906     D dopo_alert_window...
012600110906     D                 S              1A
012700110901     D Tipo_Filiale    S              1A
012800110905     D rev_Ordine      S                   like(*in12)
012900110830      *--------------------------
013000070305      * PARAMETRI PER CONTROLLO DATA E TRASFORM IN GIORNI - XSRDA8 -
013100070305     D WLBDAT          DS                  INZ
013200070305     D  G02DAT                 1      8  0
013300070227     D  G02INV                 9     16  0
013400070227     D  G02ERR                17     17
013500070227     D  G02TGI                18     22  0
013600070307      *
013700070307     D DATPAR          DS
013800070307     D  GIODAT                 1      8  0
013900070307     D  GIOINV                 9     16  0
014000070307     D  GIOTGI                17     21  0
014100070307      *
014200070307     d Wdata8          DS
014300070307     d  dadata                 1      8  0
014400070307     d  adata                  9     16  0
014500070307      *--------------------------------
014600070227     d AZUTEds       e ds                  extname(AZUTE00F)
014700070227     d DDatiUte      e ds
014800110725      *--------------------------
014900070320     d* Ds per dati organigramma
015000070404      *------
015100100505     d DTAISO          s               D
015200070320      *--------------------------
015300070320     d* Parametri x Controllo profilo utenti
015400070320     d TIBS34ds      e ds
015500070227      *--------------------------
015600070227     D* DS PER FNLV50R - CONTROLLO FILIALE GESTIONE
015700070227     D DSLV50        E DS                  EXTNAME(FNLV50DS)
015800070227      *--------------------------
015900070320      *?  COSTANTI                                      ?
016000070320      *--------------------------
016100940506     D* Tasti di funzione
016200940506     D F01             C                   CONST(X'31')
016300940506     D F02             C                   CONST(X'32')
016400940506     D F03             C                   CONST(X'33')
016500940506     D F04             C                   CONST(X'34')
016600940506     D F05             C                   CONST(X'35')
016700940506     D F06             C                   CONST(X'36')
016800940506     D F07             C                   CONST(X'37')
016900940506     D F08             C                   CONST(X'38')
017000940506     D F09             C                   CONST(X'39')
017100940506     D F10             C                   CONST(X'3A')
017200940506     D F11             C                   CONST(X'3B')
017300070315     D F12             C                   CONST(X'3C')
017400940506     D F13             C                   CONST(X'B1')
017500940506     D F14             C                   CONST(X'B2')
017600940506     D F15             C                   CONST(X'B3')
017700940506     D F16             C                   CONST(X'B4')
017800940506     D F17             C                   CONST(X'B5')
017900940506     D F18             C                   CONST(X'B6')
018000940506     D F19             C                   CONST(X'B7')
018100940506     D F20             C                   CONST(X'B8')
018200940506     D F21             C                   CONST(X'B9')
018300940506     D F22             C                   CONST(X'BA')
018400940506     D F23             C                   CONST(X'BB')
018500940506     D F24             C                   CONST(X'BC')
018600940506     D ENTER           C                   CONST(X'F1')
018700940506     D ROLDWN          C                   CONST(X'F4')
018800940506     D ROLLUP          C                   CONST(X'F5')
018900070301     d digits          c                   '0123456789'
019000110826     d Pianificato     c                   'P'
019100110831     d aConsumo        c                   'C'
019200110829     d Classe_155      c                   '155'
019300110829     d SottoClas_156   c                   '156'
019400110831     d Acquisti        c                   'A'
019500110831     d Marketing       c                   'M'
019600110921     d SI              c                   'S'
019700110921     d NO              c                   'N'
019800111013      **********************************************************************
019900111013      * Constants
020000111013      *     MaxKey - è il max. num. di campi chiave permesso in questo pgm
020100111013      **********************************************************************
020200111013     D MaxKey          C                    2
020300111013$xxx D Obbligatori     C                   '1'
020400111013     D Ascendente      C                   1
020500111013     D Discendente     C                   2
020600111013     D Carattere       C                   6
020700111013     D Put             C                   1
020800111013     D EndPut          C                   2
020900111013     D Get             C                   3
021000111013     D Numerico        C                   8
021100111013      **********************************************************************
021200111013      * Campi utili:
021300111013      *     RRN1       - Numero relativo di record del Subfile
021400111013      *     SizeList   - Dimensione della lista
021500111013      *     ReturnSize - Dimensione della lista restituita dall'API di ordinamento
021600111013      **********************************************************************
021700111013     D Rrn1            S              5I 0
021800111013     D NotUsed         S             16A
021900111013     D ReturnSize      S              9B 0
022000111013     D SizeList        S              9B 0
022100111013     D RrnLast         S              5I 0
022200111013     D WrkSort         S              1
022300111013      **********************************************************************
022400111013      * Data Structures
022500111013      *     SflRcd0    - L'intero record del SFL da passare x l'ordinamento
022600111013      *     QLGSCB     - The sort request block for the QLGSORT API
022700111013      *     QLGSCB00   - The sort request block for the QLGSRTIO API
022800111013      *     QLGSKL     - Used to build the key entry (QLGKL ) in QLGSCB
022900111013      *     QUSEC      - Error structure for the QLGSORT API
023000111013      **********************************************************************
023100111013     D SflRcd0         DS
023200111013     D  S0SCE
023300111013     D  S0COD
023400111013     D  S0DES
023500111013     D  h0INDIS
023600111013     D  Selected1                     1A
023700111013      * ?------------------------------------------------------------------ */
023800111013      * ?set caratteri Maiuscoli/Minuscoli
023900111013     D Up              C                   CONST('ABCDEFGHIJKLMNOPQRSTUVWXYZ')
024000111013     D Lo              C                   CONST('abcdefghijklmnopqrstuvwxyz')
024100111013      * ?------------------------------------------------------------------ */
024200111013      /COPY QSYSINC/QRPGLESRC,QLGSORT
024300111013     D QLGKL                         16    DIM(MaxKey)
024400111013     D  QLGSP00                       9B 0 OVERLAY(QLGKL:00001)
024500111013     D  QLGSS00                       9B 0 OVERLAY(QLGKL:00005)
024600111013     D  QLGDT00                       9B 0 OVERLAY(QLGKL:00009)
024700111013     D  QLGSO00                       9B 0 OVERLAY(QLGKL:00013)
024800111013
024900111013      /COPY QSYSINC/QRPGLESRC,QLGSRTIO
025000111013      /COPY QSYSINC/QRPGLESRC,QUSEC
025100111013     C*----------------------------------------------------*
025200070419      *?     MAIN LINE PROGRAM                             ?
025300940127     C*----------------------------------------------------*
025400940223     C* inizializzazione variabili
025500940223     C                   EXSR      INZVAR
025600940223     C*
025700940223     C     $FINE         DOWEQ     *OFF
025800110909     C*
025900110909     C*  re-imposta il passaggio fra video e video
026000110909     c                   eval      cambia_video = *blank
026100110909     c                   if        $GEST <> sav$GEST and $GEST <> 'W1'
026200110909     c                   eval      cambia_video = 'S'
026300110909     C                   end
026400110823     C*
026500110906     C* Controllo Autorizzazione PROFILO
026600110906     C*  altrimenti non Entra a programma
026700110823     C     $GEST         CASEQ     'AU'          GESAU                          Controllo DAUT
026800110826     C*
026900110826     C* Finestra x ALLARME messaggi importanti
027000110826     C     $GEST         CASEQ     'W1'          GESW1                          finestra errori
027100110906     C*
027200110826     C* 1° Video
027300070413     C     $GEST         CASEQ     'D1'          GESD1                          Video Parametri
027400070413     C*
027500110906     C* 2° Video SFL Sottoclassi da scegliere
027600110906     C     $GEST         CASEQ     'S0'          GESS0                          Lista SottoClassi
027700110826     C*
027800110906     C* 3° Video SFL riepilogativo dell'ordine
027900110906     C     $GEST         CASEQ     'S1'          GESS1                          Lista Articoli Immes
028000110906     C*
028100110906     C* Uscita forzata - Cancella Ordine immesso
028200110906     C     $GEST         CASEQ     'CO'          GESCO                          Cancella Ordine
028300110906      *
028400940117     C                   END
028500110909      *
028600110909     C                   ENDdo
028700940325     C* fine programma
028800110901     C                   clear                   Tii501ds
028900110901     C                   move      Tipo_Filiale  i501livLPO
029000110901     C                   move      dutCOU        i501codute
029100110901     C                   move      knmus         i501knmus
029200110901     C                   move      anno          i501weekAA
029300110901     C                   movel     settimana     i501weekAA
029400110901     C                   move      dutpou        i501filial
029500110901     C                   movel(p)  Tii501ds      KPJBU
029600110901      **
029700940325     C                   SETON                                            LR
029800030113     C************************************************************
029900070320      *?  INIZIALIZZAZIONE VARIABILI                       ?
030000030113     C************************************************************
030100030113     C     INZVAR        BEGSR
030200030113     C*
030300030113     C* Variabili per gestione videate
030400030113     C                   MOVE      *OFF          $FINE
030500110824     C                   MOVE      *OFF          dopo_in_$FINE
030600110824     C                   MOVE      *ON           $INZD1
030700110826     C                   MOVE      *OFF          $INZS0
030800110826     C                   MOVE      *OFF          $INZS1
030900030113     C                   MOVE      *OFF          $EFILE
031000030113     C                   MOVE      *OFF          $ESCI
031100030113     C                   MOVE      *OFF          $RCDOK
031200110823     C* Prima di tutto Controlla Autorizzazioni ad Accedere al PGM
031300110823     C                   MOVE      'AU'          $GEST
031400030113     C* Variabili appoggio
031500030114     C                   Z-ADD     1             WPAG
031600030113     c*
031700111012     c                   open      tmorp02L
031800111012     c*
031900030113     C                   ENDSR
032000110823     C************************************************************
032100110823      *?  Emissione primo video di SCELTA                        ?
032200110823     C************************************************************
032300110823     C     GESD1         BEGSR
032400110823     C*
032500070227     C* inizializzazione videata
032600070227     C     $INZD1        IFEQ      *ON
032700070227     C                   EXSR      INZD1
032800070227     C                   MOVE      *OFF          $INZD1
032900070227     C                   ENDIF
033000070227     C*
033100070228     c     Video_1       tag
033200110921     c                   if        dutlpo ='S'
033300110921      * sblocca il campo
033400110921     C                   SETON                                        07
033500110921     c                   end
033600070302     C*  Emissione Video
033700070228      * ?            *-----------------------*
033800110725     C                   EXFMT     TII5D01
033900070228      * ?            *-----------------------*
034000070326     c                   setoff                                       99
034100070305     C                   movel     'D1'          sav$GEST
034200070227     C* Selezioni
0343000702271    C                   SELECT
034400070413     C*  Cambio filiale
034500070413     C                   WHEN      $TASTO   =   F18
034600070418     C                   SETON                                        07
034700070227     C* F3=Fine
034800070227     C                   WHEN      $TASTO   =   F03
034900070227     C                   EXSR      F03_exit
035000070227      *
0351000702271O   C                   OTHER
035200070227     C* CONTROLLO DATI
035300070227     C                   EXSR      CTRD1
035400080626      *
0355000702271-   C                   ENDSL
035600070227     C*
035700070418     C* Per errori o decodifiche o tasto di cambio P.O.
035800110921     C                   if         *in99
035900070418     c                   goto      Video_1
036000070418     c                   end
036100110902     C*
036200110725     C* Se non ci sono errori passa alle operazioni sul SFL
036300110725     C*   Reperisce il nuovo numero Ordine
036400110725     c                   clear                   ordine_OK         1
036500110906     c                   eval      scritto_almeno_una_riga_ordine = ' '
036600110906     c                   eval      premuto_F10 = ' '
036700110906     C*
036800110906     C*  preleva un nuovo numero ordine
036900111013     c                   if          num_Ordine = 0 and
037000111013     c                                ord_trovato   <> 'S'
037100110725     c                   exsr      new_ordine
037200111013     c                   end
037300110725     C*
037400120313     c******             if        ordine_OK ='S'
037500110824     C*  Emissione SFL per ordini
037600110826     c                   eval      $GEST ='S0'
037700110826     C                   MOVE      *ON           $INZS0
037800120313     c******             end
037900110902     C*
038000070228      *
038100080606     C     Fine_GESD1    ENDSR
038200070306     C/EJECT
038300110726      *---------------------------------------------------------------*
038400070320      *?  INIZIALIZZAZIONE Primo video                           ?
038500110726      *---------------------------------------------------------------*
038600070227     C     INZD1         BEGSR
038700070227     C*
038800070227     C* Pulizia campi e indicatori
038900070308     c                   exsr      INZ_indERR
039000110726     C*
039100070227     C* Pulizia formato record
039200110725     c                   clear                   TII5D01
039300070227     C* campi titolo
039400070302     C                   MOVEL     dsPGM         D1CPGM
039500070227     C                   MOVEL     knsif         D1nsif
039600070227     C                   MOVEL     knmus         D1nmus
039700070227     C                   MOVEL     RSUT          D1crsu
039800110831     C                   MOVEL     aConsumo      D1Torp
039900110831     C                   MOVEL     Acquisti      D1UFFc
040000110824     C                   z-add     settimana     D1week
040100070227     C* campi video
040200070227     C* Il p.o.deve essere impostato in base al profilo
040300070413     C     dutlpo        IFEQ      '2'
040400070227     C                   MOVEL     dutpou        D1CFGS                         *FIL.GESTIONE
040500070413     C                   ELSE
040600070413     c                   seton                                        08
040700070413     C                   MOVEL     SIMFEL        D1CFGS
040800070413     C                   END
040900080606      *
041000110830     C                   z-add     woggi         D1cDDC
041100080606      *
041200110824      *  reimposta la filiale
041300070418     c                   if        sav_d1cfgs > 0
041400070418     C                   z-add     sav_d1cfgs    D1cfgs
041500070418     c                   end
041600070418     C*
041700080606     C     END_INZ1      ENDSR
041800940127     C************************************************************
041900070320      *? Controlli primo video di SCELTA Operazione da eseguire  ?
042000940127     C************************************************************
042100070227     C     CTRD1         BEGSR
042200070606     C*
042300070227     C*  Spegne indicatori di errore
042400070308     c                   exsr      INZ_indERR
042500110905     c                   setoff                                       99  12
042600111013     c                   clear                   ord_trovato       1
042700110726     C* --------------
042800070227      * ?- Controlla la filiale in gestione
042900110725     c                   clear                   wfgs
043000110725     C                   clear                   sav_d1cFGS
043100070227     C                   CLEAR                   DSLV50
043200070227     C                   MOVEL     KNMUS         D50PRU
043300070227     C                   z-add     d1cFGS        D50FGS
043400070227     C                   CALL      'FNLV50R'
043500070227     C                   PARM                    DSLV50
043600070227    1C     D50ERR        IFNE      ' '
043700110921    1C     dutlpo        andne     'S'
043800070326     c                   movel     ErrMsg(1)     d1msg
043900110921     c                   if        dutlpo ='S'
044000110921      * sblocca il campo
044100110921     C                   SETON                                        07
044200110921     c                   end
044300070326     C                   SETON                                        51  99
044400070227     C                   goto      ENDCTR1
044500070227    1C                   ENDIF
044600070227      *
044700110726     C*  Controlla il P.O.
044800110901     c                   movel     dutLPO        Tipo_Filiale
044900070227     C     d1cFGS        CHAIN     AZORG01l
045000110725      * Non presente in organigramma
045100110725    1C                   IF        not %found(azorg01l)
045200070326     C                   SETON                                        51  99
045300110921     c                   if        dutlpo ='S' and d1cfgs = 0
045400110921      * sblocca il campo
045500110921     C                   SETON                                        07
045600110921     c                   end
045700070326     c                   movel     ErrMsg(1)     d1msg
045800070227     C                   goto      ENDCTR1
045900110901    1C                   else
046000110901     c                   eval      og143 = orgDE3
046100110901     c                   if        §ogNTW = 'LOG'
046200110901     c                   movel     'L'           Tipo_Filiale
046300110901     c                   end
046400110901    1C                   ENDIF
046500070227     C*
046600070316      * Salva la filiale su campo di work x tutto il pgm
046700070316     c                   z-add     d1cfgs        wfgs
046800070418     C                   Z-ADD     D1cFGS        sav_d1cFGS
046900070307      *
047000110824      * > TABELLA WEK x le settimane degli ordini mensili
047100110826     c                   if        D1TORP = Pianificato
047200110824     C                   eval        tbeCOD = 'WEK'
047300110825     C                   movel(p)  wfgs          tbeKE1
047400110824     c     kTnTBE        chain     tntbe01l
047500110921      * >>>>>>>>>>>>>
047600110825    1c                   if        not %Found(tntbe01l)
047700110725     C                   SETON                                        52  99
047800110825     c                   movel     ErrMsg(2)     d1msg
047900110725     C                   goto      ENDCTR1
048000110825      * >>>>>>>>>>>>>
048100110725    1C                   Else
048200110726      * >>>>>>>>>>>>>
048300110826      *  Controlla se è la settimana corretta per l'inserimento del Pianificato
048400110921     c                   movea     tbeUNI        weekWEK
048500110905     c                   move      settimana     week2A            2
048600110905     c     week2a        lookup    weekWEK                                21
048700110921      ***
048800110824     c                   if        not *in21
048900110824     C                   SETON                                        52  99
049000110825     c                   movel     ErrMsg(5)     d1msg
049100110824     C                   goto      ENDCTR1
049200110824     c                   end
049300110824      ***
049400110824     C                   eval       orpFIL  = d1cfgs
049500110905     C                   eval       orpUFF  = d1uffC
049600110824     C                   eval       orpANNO = Anno
049700110824     C                   eval       orpWEEK = settimana
049800110905     C                   eval       orpPIAN = d1torp
049900110908     c     kTmORP        chain     tmORP01L
050000110908     c                   if        %Found(tmORP01L)
050100111013     c                   eval       *in12 = *on
050200110912      **
050300110912      ** se già esiste la ggancia da confermare
050400110912     c                   z-add     orpNOR        num_Ordine
050500111013     c                   move      'S'           ord_trovato
050600111013     c                   move      'S'           ordine_OK
050700110912     c                   z-add     orpDOR        h0DOR
050800110912     C*
050900110912     C                   Z-ADD     h0DOR         G02INV                         *AAAA/MM/GG
051000110912     C                   MOVEL     '3'           G02ERR
051100110912     C                   CALL      'XSRDA8'
051200110912     C                   PARM                    WLBDAT
051300110912     c                   Z-ADD     G02DAT        Data_Ordine
051400110921     C*
051500110912     c                   move      'S1'          $GEST
051600110912     c                   move      *ON           $INZS1
051700110912      **
051800110921      ** se già esiste confermato - emette l'errore.
051900110908     c                   if          orpconf = '1'
052000110824     C                   SETON                                        52  99
052100110825     c                   movel     ErrMsg(6)     d1msg
052200110824     C                   goto      ENDCTR1
052300110824     c                   endif
052400110908     c                   end
052500110902      ***
052600110825    1c                   endif
052700110921      * >>>>>>>>>>>>>
052800110825     C                   ENDIF
052900111013     C*---------------
053000111013     c                   eval      rev_ordine = *in12
053100070307     C*---------------
053200070227     C     ENDCTR1       ENDSR
053300070319      *---------------------------------------------------------------*
053400110726      *  ?  GESTIONE SFL di immissione righe di ordine               ?
053500070319      *---------------------------------------------------------------*
053600110826     C     GESS0         BEGSR
053700070319     C*
053800940223     C* inizializzazione videata
053900110826     C     $INZS0        IFEQ      *ON
054000110826     C                   EXSR      INZS0
054100110826     C                   MOVE      *OFF          $INZS0
054200940127     C                   ENDIF
054300070327     C*
054400070306     C* Inizializza e imposta piede videata
054500110826     C                   EXSR      INZZ0
054600070306     C*
054700030113     C* emissione piede videata
054800070302      * ?            *-----------------------*
054900110826     C                   WRITE     TII5Z00
055000070302      * ?            *-----------------------*
055100070228     C*
055200940223     C     WMAX          IFEQ      0
055300070228     C* Non ci sono records
055400070302      * ?            *-----------------------*
055500110826     C                   WRITE     TII5V00
055600070302      * ?            *-----------------------*
055700070301     c                   setoff                                       30
055800030114     C                   Else
055900070301     c                   seton                                        30
056000070330     c                   eval       sav_WSfl =  WSfl
056100070330     c                   eval       sav_Wpag =  Wpag
056200070301      *
056300030114     C     Wsfl          IFgt      0
056400110826     C                   Z-ADD     wsfl          C0RCD
056500030114     C                   Else
056600030114     C     Wpag          IFgt      0
056700110826     C                   Z-ADD     wpag          C0RCD
056800030114     C                   EndIF
056900030114     C                   EndIF
057000030114     C                   ENDIF
057100070228      *
057200070228      *  Emissione SFL dei GIRI
057300070228      * ?            *-----------------------*
057400110826     C                   EXFMT     TII5C00
057500070228      * ?            *-----------------------*
057600070326     c                   setoff                                       99
057700110908     c                   clear                   AlmenoUnaSCl      1
057800110826     C                   movel     'S0'          sav$GEST
057900070228      *
058000110826     C     C0NRR         IFNE      0
058100110826     C                   Z-ADD     C0NRR         WSFL
058200940204     C                   ENDIF
058300110826     C                   Z-ADD     SFLNRR        C0RCD
058400030113     C* Selezioni
0585009401271    C                   SELECT
058600070328     C* F3=Fine
058700070328     C     $TASTO        WHENEQ    F03
058800070328     C                   EXSR      F03_exit
058900110726      *
059000110906     C* F10=Avanza
059100110906     C     $TASTO        WHENEQ    F10
059200110906     c                   move      'S1'          $GEST
059300110906     C                   MOVE      *ON           $INZS1
059400110906      *
059500070328     C* F12=Ritorno
059600070328     C     $TASTO        WHENEQ    F12
059700070328     c                   move      'D1'          $GEST
059800070328     C                   MOVE      *ON           $INZD1
059900070328      *
0600009401271O   C                   OTHER
060100070228     C* CONTROLLO testata
060200110826     C                   EXSR      CTRC0
060300070228     C* CONTROLLO subfile
060400940201     C     *IN99         IFEQ      *OFF
060500110826     C                   EXSR      CTRS0
060600940131     C                   END
060700070327      *
060800110830      * ? Visualizza l'ordine se scritto almeno un record di ordine
060900110830      *    dal programma chiamato.
061000110830     C     *IN99         IFEQ      *OFF
061100110902     C**** $TASTO        andeq     F06
061200110908     c     AlmenoUnaSCl  andeq     'S'
061300110830     c     ERRmsg80      andEQ     *blank
061400110830     C     WMAX          andGT     0
061500110909     c     i501keyRET    andNE     F12
061600110826     C                   MOVE      *ON           $INZS1
061700110826     C                   movel     'S1'          $GEST
061800070314     c                   end
061900070228      *
0620009401271-   C                   ENDSL
062100940127     C*
062200940127     C                   ENDSR
062300070328     C/EJECT
062400070328      *---------------------------------------------------------------*
062500070328      *? INIZIALIZZAZIONE e IMPOSTA il PIEDE del SFL                 ?
062600070328      *---------------------------------------------------------------*
062700110826     C     INZZ0         BEGSR
062800070328     C*
062900070306     C* Messaggio o tasti funzionali
063000110826     c                   clear                   z0dmsg
063100110826     c                   clear                   ERRmsg80         80
063200070307     C*
063300070328      * imposta eventuali errori
063400110826     c                   eval      z0dmsg = ERRmsg80
063500070306     C*
063600110906     C*   Attenzione se ho scritto delle righe di Ordine
063700110906     C*     NON deve permettere di tornare alla videata iniziale
063800110906     c                   setOFF                                       12
063900110906     c                   if        scritto_almeno_una_riga_ordine ='S'
064000110906     c                               and
064100110906     c                             premuto_F10 = *blank
064200110906     c                   setON                                        12
064300110906     c                   end
064400110906     C*
064500070306     C                   ENDSR
064600070306     C/EJECT
064700070306      *---------------------------------------------------------------*
064800070320      *? INIZIALIZZAZIONE LISTA dei GIRI da selezionare              ?
064900070306      *---------------------------------------------------------------*
065000110826     C     INZS0         BEGSR
065100070306     C*
065200070228     C* reset indicatori DSPATR
065300070308     c                   exsr      INZ_indERR
065400940302     C* pulizia SFL
065500940128     C                   SETOFF                                         3031
065600110826     C                   WRITE     TII5C00
065700940128     C                   SETON                                          31
065800940128     C*
065900110826     C* Righe x Pagina del 0° SFL
066000110826     c                   z-add     RigPag_SFl0   s0pag
066100070228      *
066200030113     C* CARICAMENTO SFL totale
066300110826     C                   Z-ADD     0             S0NRR
066400110826     C                   Z-ADD     1             C0RCD
066500940128     C                   Z-ADD     0             WMAX
066600070302     C                   Z-ADD     0             Wsfl
066700070302     C* campi titolo
066800110826     C                   MOVEL     dsPGM         c0CPGM
066900110826     C                   MOVEL     knsif         c0nsif
067000110826     C                   MOVEL     knmus         c0nmus
067100110826     C                   MOVEL     RSUT          c0crsu
067200110826     C                   z-add     settimana     c0week
067300110826     C                   z-add     num_ordine    c0NOR
067400110826     C                   z-add     data_ordine   c0DOR
067500111012     C                   z-add     d1cfgs        c0FIL
067600110830     C*
067700110830     C*  Se richiesto un ordine pianificato x Filiale
067800110830     C*    occorre precaricarsi le sottoclassi presenti sugli articoli
067900110830     C*  dell'ordine pianificato.
068000110830     C                   exsr      carica_subclas
068100110830     C*
068200110725      **----------------
068300110725     C* Posizionamento su file pilota
068400110825     C                   eval        $EFILE    = *OFF
068500110829     C     SottoClas_156 setll     Antab01L
068600110725      **----------------
068700030113     C* Carico il SFL
068800110826     C                   EXSR      ROLS0
068900030113     C*
069000111013     c                   if        D1TORP = Pianificato
069100111013     c                   eval       RrnLast  =  Wmax
069200111013     C                   EXSR      ORDINA
069300111013     c                   end
069400111013     C*
069500030114     C                   Z-ADD     1             WPAG
069600110726     c                   eval       sav_Wmax =  Wmax
069700110725     C*
069800110725     C                   ENDSR
069900110725     C/EJECT
070000110725      *---------------------------------------------------------------*
070100070320      *? CARICAMENTO PAGINA LISTA                                    ?
070200070305      *---------------------------------------------------------------*
070300110826     C     ROLS0         BEGSR
070400070305     C*
070500070305     C                   SETOFF                                       32
070600110831     C                   Z-ADD     1             Y
070700110826     C                   Z-ADD     WMAX          S0NRR
070800070305     C*
070900070305     C* Caricamento del Sfl x Giro
071000110826     c                   Exsr      Write_S0
071100070305     C*
071200110826     C                   Z-ADD     S0NRR         WMAX                 30
071300070305     C*
071400110826     C* POSIZIONAMENTO AL 0° RCD DELLA PAGINA
071500110826     C     S0NRR         DIV       S0pag         PAGINE            4 0
071600070305     C                   MVR                     RESTO             3 0
071700110826     C     PAGINE        MULT      S0pag         C0RCD
0718000703051    C     RESTO         IFGT      0
071900110826     C                   ADD       1             C0RCD
0720000703051E   C                   ELSE
072100110826     C                   SUB       S0pag         C0RCD
072200110826     C                   ADD       1             C0RCD
0723000703051-   C                   ENDIF
072400070305     C*
072500070305     C                   ENDSR
072600070301      *---------------------------------------------------------------*
072700070320      *?  Scrive x Ogni rottura di GIRO                              ?
072800070301      *---------------------------------------------------------------*
072900110826     C     Write_S0      BEGSR
073000070305      *
073100110826      *  Precarica da Ordine Pianificato
073200110826     c                   exsr      Legge_rec0
073300070228      *
073400070228      * Finchè il giro è lo stesso
0735001108311    C     $EFILE        DowEQ     *OFF
073600110826     C                   clear                   TII5S00
073700070228      *
073800110830      * Solo x ordini mensili deve impostare le sottoclassi
073900110830      *  degli articoli pianificati preselezionate con "1" e proteggere
074000110830      *   altre scelte.
074100110826     c                   exsr      carica_campi0
074200070305      *---------
074300110826     C                   ADD       1             S0NRR
074400070305     C                   ADD       1             Y
074500110826     C                   WRITE     TII5S00
074600070305      *---------
074700110826     c                   exsr      Legge_rec0
074800110825     C*
074900070228     c                   EndDO
075000070228     C*
075100070228     C                   ENDSR
075200110825      *---------------------------------------------------------------*
075300110825      *?  LETTURA RCD ARCHIVIO PILOTA                                ?
075400110825      *---------------------------------------------------------------*
075500110826     C     LEGGE_REC0    BEGSR
075600110825     C*
075700110825     C*  Per EoF deve uscire
075800110825     C*  Per Record Ok deve uscire riportando i dati
075900110825     C                   MOVEL     *OFF          $EFILE
076000110825     C                   MOVEL     *OFF          $RCDOK
076100110825     C*
0762001108251    C     $EFILE        DOUEQ     *ON
076300110825     C     $RCDOK        OREQ      *ON
076400110825     C*
076500110826      * ? Se Ordine Pianificato deve caricare da file precaricato
076600110825     c****   LETTURA RECORD x Filiale
076700110829     c     SottoClas_156 reade     antab01l
076800110825      * Fine File
076900110826     C                   IF        %Eof(antab01l)
077000110825     C                   MOVEL     *ON           $EFILE
077100110825     C                   MOVE      $EFILE        *IN33
077200110825     C                   Else
077300110825      * ?Controlla se la Spedizione è da considerare
077400110825     C                   MOVEL     *ON           $RCDOK
077500110826      *
077600110826     C                   exsr      Check_RECord0
077700110826      *
077800110825     C                   END
077900110825     C*
0780001108251-   C                   ENDDO
078100110825     C*
078200110825     C                   ENDSR
078300110825      *---------------------------------------------------------------*
078400110825      *?   CONTROLLO il record letto se va bene                      ?
078500110825      *---------------------------------------------------------------*
078600110826     C     Check_Record0 BEGSR
078700110825     C*
078800110921     C*  Controlla se la sottoclasse è presente
078900110830     C                   MOVE      TABCOD        SCLASSE           5
079000111013     c                   z-add     1             scl
079100111013     C     SCLASSE       lookup    SubClassi(SCL)                         22
079200110830     c                   if        not *in22
079300110830     C                   MOVEL     *OFF          $RCDOK
079400111013     C                   Else
079500111013     c                   eval      Tipo_SCl = TipoSCLAS(SCL)
079600111013     C                   End
079700110825     C*
079800110901     C*  Sottoclasse autorizzata alla Filiale 1°/2°Livello o x Logistica
079900110901     C                   MOVEl     TABdes        ANTAB156
080000111011     C*
080100111011     C* Controlla che la Sottoclasse appartenga alla tipologia richiesta
080200111011     c                   if        gru156V <> d1uffC
080300111011     C                   MOVEL     *OFF          $RCDOK
080400111011     C                   End
080500110901     C*
080600110826     C     end_Check0    ENDSR
080700070301      *---------------------------------------------------------------*
080800110825      *?  Carica gli articoli pianificati per il periodo x  Filiale  ?
080900070301      *---------------------------------------------------------------*
081000110826     C     Carica_campi0 BEGSR
081100110726     C*
081200110830     c                   eval      s0DES =  tabDES
081300110830     c                   move      tabCOD        s0COD
081400110830     C*
081500110907     C                   SETOFF                                       34  32
081600111013     C*
081700111013      * solo se obbligatoria  per il pianificato la seleziona protetta
081800110830     c                   if        D1TORP = Pianificato
081900111013     c                             and Tipo_SCl = 'O'
082000110830     c                   move      '1'           s0sce
082100110907     C                   SETON                                        34  32
082200110830     C                   END
082300110825     C*
082400110726     C                   ENDSR
082500110726      *---------------------------------------------------------------*
082600110726      *?   CONTROLLO TESTATA LISTA                                   ?
082700110726      *---------------------------------------------------------------*
082800110826     C     CTRC0         BEGSR
082900110726     C*
083000070301     C                   MOVE      *OFF          *IN99
083100110726     C*
083200070301     C                   ENDSR
083300070301      *---------------------------------------------------------------*
083400070320      *?   CONTROLLO OPZIONI LISTA                                   ?
083500070301      *---------------------------------------------------------------*
083600110826     C     CTRS0         BEGSR
083700070301     C*
083800070301     C                   MOVEL     *OFF          $ESCI
083900110830     c                   clear                   ERRmsg80
084000070301     C                   SETOFF                                       99
084100070301     C*
084200070301     C* Leggo il sfl solo se ci sono rcd
0843000703011    C     WMAX          IFGT      0
084400110826     C                   READC     TII5S00                                21
084500070301     C*
084600070301     C* esce se fine sfl o errore che richiede l'uscita
0847000703012    C     *IN21         DOWEQ     *OFF
084800070301     C     $ESCI         ANDEQ     *OFF
084900110826     C                   Z-ADD     S0NRR         C0RCD
085000070301     C* ctrl su riga
085100110826     C                   EXSR      RECS0
085200070301      *
085300070301     C* se rilevato errore o richiesta uscita, attivo sflnxtchg
0854000703013    C     *IN99         IFEQ      *ON
085500070301     C     $ESCI         OREQ      *ON
085600070301     C* disabilito l'eventuale richiesta di reinizializzazione sfl;
085700070301     C* la ripristinerò a conclusione del ciclo di READC
085800110826     C                   MOVE      *OFF          $INZS0
0859000703013-   C                   ENDIF
086000070301     C*
086100070301      * ? Attivo sempre il SFLNXTCHG e aggiorno SFL
086200070301     C                   MOVE      *ON           *IN32
086300110826     C                   UPDATE    TII5S00
086400070301     C*
086500070301     C* leggo prossimo rcd dal sfl se no richiesta uscita ciclo
0866000703013    C     $ESCI         IFEQ      *OFF
086700110826     C                   READC     TII5S00                                21
086800070301      *
086900070301     C* a fine ciclo ripristino il flag richiesta iniz.sfl
0870000703014    C     *IN21         IFEQ      *ON
087100070301     C* calcolo pagina a cui deve posizionarsi
087200110826     c                   z-add     s0pag         Wpagine
087300070301     C                   EXSR      CLCPAG
0874000703014-   C                   ENDIF
0875000703013-   C                   ENDIF
087600070301     C*
0877000703012-   C                   ENDDO
087800070301     C*
0879000703011-   C                   ENDIF
088000110726     C* __________________________________________________
088100110826     C*  errori da evidenziare tramite finestra di ALLERTA
088200110826     c                   exsr      Errori
088300110726     C* __________________________________________________
088400070328     C*
088500070301     C                   ENDSR
088600070301     C/EJECT
088700070301      *---------------------------------------------------------------*
088800070320      *?   CONTROLLO CAMPI I/O RIGA LISTA                            ?
088900070301      *---------------------------------------------------------------*
089000110826     C     RECS0         BEGSR
089100070301     C*
089200070301     C* reset indicatori DSPATR
089300070308     c                   exsr      INZ_indERR
089400070326     C*
089500110830     c                   setON                                        32
089600110826     C* gestione opzioni
0897001108263    C     S0sce         IFNE      *blank
089800110826     C     *IN99         ANDEQ     *OFF
089900110902     C**** $TASTO        ANDEQ     F06
090000110826      *
090100110908     c                   eval      AlmenoUnaSCl ='S'
090200110826     C                   EXSR      SCE_S0
090300110830     c                   setOFF                                       32
090400110830     c                   clear                   s0sce
090500110826      *
0906001108263-   C                   ENDIF
090700110826     C*
090800070327     C*
090900110826     C     endSF0        ENDSR
091000110826      *---------------------------------------------------------------*
091100110826      *?   GESTIONE OPZIONI LISTA                                    ?
091200110826      *---------------------------------------------------------------*
091300110826     C     SCE_S0        BEGSR
091400110826     C*
091500110826     C*  Selezionata la riga
091600110826     c                   If        s0SCE = '1'
091700110826     c                   eval      kpjbus = kpjbu
091800110826     C                   clear                   Tii501ds
091900110901     C                   move      Tipo_Filiale  i501livLPO
092000110826     C                   move      dutCOU        i501codute
092100110826     C                   move      knmus         i501knmus
092200110830     C                   move      c0DOR         i501weekAA
092300110826     C                   movel     d1week        i501weekAA
092400110826     C                   move      d1cfgs        i501filial
092500110826     C                   move      d1tORP        i501tipord
092600110826     C                   move      d1uffC        i501uffcom
092700110831     C                   move      *all'0'       i501sclass
092800110831     C                   move      s0cod         i501sclass
092900110826     C                   move      num_ordine    i501numord
093000110826     C                   move      data_ordine   i501dataor
093100110906     C                   move      *blank        i501cpyOPP
093200110830     C                   move      *blank        i501keyRET
093300110830     C                   move      *blank        i501wriUNO
093400110830      * se ordine PIanificato
093500110830     c                   if        D1TORP = Pianificato
093600110906     C                   move      'S'           i501cpyOPP
093700110830     c                   end
093800110826     C                   movel(p)  Tii501ds      KPJBU
093900111012     c                   close     tmorp02L
094000111012      *--
094100110831     C                   call      'TII501R'
094200110826     C                   parm                    KPJBA
094300111012      *--
094400111012     c                   open      tmorp02L
094500110830     C                   movel(p)  KPJBU         Tii501ds
094600110826     c                   movel     kpjbus        kpjbu
094700110830      * se è stata immessa almeno una riga di ordine
094800110830      ** per visualizzare in riepilogo.
094900110906     c                   If        i501wriUNO = 'S'
095000110906     c                   eval      scritto_almeno_una_riga_ordine = 'S'
095100110830     c                   end
095200110908      * deve uscire
095300110908     c                   If        i501keyRET = F03
095400110901     C                   EXSR      F03_exit
095500110901     c                   end
095600110908      * deve tornare alla scelta sottoclassi
095700110908     c                   If        i501keyRET = F12
095800110908     C                   eval      $ESCI = *on
095900110909     c                   eval      $GEST = 'S0'
096000110909     c******             seton                                        99
096100110908     c                   end
096200110826     c                   endIf
096300110826     C*
096400110826     C                   ENDSR
096500110826     C/EJECT
096600110826      *---------------------------------------------------------------*
096700110830      *  ?  Carica le Sottoclassi presenti su articoli PIANIFICATI   ?
096800110826      *---------------------------------------------------------------*
096900110830     C     carica_subclasBEGSR
097000110830     C*
097100110921     C*  Leggendo gli articoli verifica a quale classe appartengono
097200110921     C*    quindi prende in considerazione la sottoclasse
097300110830     c                   clear                   SubClassi
097400110921     C*
097500110921     c     *loval        setll     tMamp10l
097600110921     c                   read      tMamp10l
097700110921     C*
097800110921     c                   dow       not %EoF(tMamp10l)
097900110921     c                   clear                   da_Prendere       1
098000110921     C*
098100110921     C* decodifica la sottoclasse se da prendere in considerazione
098200110921     c                   exsr      Decod_SubCLASS
098300110921     C*
098400110830     c* cerca decodifiche su file articoli
098500110921     c                   if         da_Prendere = SI
098600110921     c                   eval      sotto_classe = ampSCM
098700110921     c                   exsr      ADD_in_Schiera
098800111013     C*
098900110921     c     sotto_classe  setgt     tMamp10l
099000110921     c                   end
099100110921     C*
099200110921     c                   read      tMamp10l
099300110830     c                   enddo
099400110830     C*
099500110830     C                   ENDSR
099600110830     C/EJECT
099700110830      *---------------------------------------------------------------*
099800110921      *  ?  Decodifica la SottoCLASSE se da prendere oppure NO       ?
099900110830      *---------------------------------------------------------------*
100000110921     C     Decod_SUBCLASSBEGSR
100100110921     C*
100200110921     C*      Classe Merceologica
100300110921     c                   clear                   obbligatorio      1
100400110921     c                   clear                   facoltativo       1
100500110921     c                   clear                   a_consumo         1
100600110921     C*
100700110921     c                   move      Classe_155    tabGRU
100800110921     c                   move      *all'0'       tabCOD
100900110921     c                   move      ampCLM        tabCOD
101000110921     c     kAnTAB        chain     antab01l
101100111013     C*
101200110921     c                   if          %Found(AnTAB01l)
101300110921     c                   eval      antab155 = tabDES
101400110921     c                   eval         obbligatorio = PE1155v
101500110921     c                   eval         facoltativo  = PE2155v
101600110921     c                   eval         a_consumo    = PE3155v
101700110921     c                   end
101800110921     C*
101900111013     c                   if        obbligatorio <> *blank and D1Torp=Pianificato
102000111013     c                               or
102100111013     c                             facoltativo  <> *blank and D1Torp=Pianificato
102200111013     c                               or
102300111013     c                             a_consumo    <> *blank and D1Torp=aConsumo
102400111013     c                   eval       da_Prendere = SI
102500110921     c                   end
102600110921     C*
102700110921     C*
102800110921     C                   ENDSR
102900110921     C/EJECT
103000110921      *---------------------------------------------------------------*
103100110921      *  ?  aggiunge nella schiera delle sottoclassi                 ?
103200110921      *---------------------------------------------------------------*
103300110921     C     ADD_in_schieraBEGSR
103400110830     C*
103500110921     C                   Z-ADD     1             SCL               3 0
103600110921     C     sotto_classe  LOOKUP    SubClassi(SCL)                         22
103700110921     c*
103800110921      * Solo se non c'è la imposta in schiera.
103900110921     C                   If        *IN22 = *oFF
104000110921     C                   Z-ADD     1             SCL
104100110921     C     *blank        LOOKUP    SubClassi(SCL)                         22
104200110921     c* lo aggiunge
104300110921     C                   If        *IN22 = *oN
104400110921     C                   eval      SubClassi(SCL) = sotto_classe
104500111013     c                   clear                   Tipo_Scl          1
104600111013     c                   if           obbligatorio <> *blank
104700111013     c                   eval           Tipo_Scl = 'O'
104800111013     c                   elseIf       facoltativo  <> *blank
104900111013     c                   eval           Tipo_Scl = 'F'
105000111013     c                   elseIf       a_consumo    <> *blank
105100111013     c                   eval           Tipo_Scl = 'C'
105200111013     c                   end
105300111013     C                   eval      TipoSClas(SCL) = Tipo_Scl
105400110921     c                   end
105500110921     c                   end
105600110921     C*
105700110921     C                   ENDSR
105800110921     C/EJECT
105900110921      *---------------------------------------------------------------*
106000110921      *  ?  GESTIONE SFL di immissione righe di ordine               ?
106100110921      *---------------------------------------------------------------*
106200110921     C     GESS1         BEGSR
106300110921     C*
106400110826     C* inizializzazione videata
106500110826     C     $INZS1        IFEQ      *ON
106600110826     C                   EXSR      INZS1
106700110826     C                   MOVE      *OFF          $INZS1
106800110826     C                   ENDIF
106900110826     C*
107000110826     C* Inizializza e imposta piede videata
107100110826     C                   EXSR      INZZ1
107200110826     C*
107300110826     C* emissione piede videata
107400110826      * ?            *-----------------------*
107500110826     C                   WRITE     TII5Z01
107600110826      * ?            *-----------------------*
107700110826     C*
107800110826     C     WMAX          IFEQ      0
107900110826     C* Non ci sono records
108000110826      * ?            *-----------------------*
108100110826     C                   WRITE     TII5V01
108200110826      * ?            *-----------------------*
108300110826     c                   setoff                                       30
108400110826     C                   Else
108500110826     c                   seton                                        30
108600110826     c                   eval       sav_WSfl =  WSfl
108700110826     c                   eval       sav_Wpag =  Wpag
108800110826      *
108900110826     C     Wsfl          IFgt      0
109000110826     C                   Z-ADD     wsfl          C1RCD
109100110826     C                   Else
109200110826     C     Wpag          IFgt      0
109300110826     C                   Z-ADD     wpag          C1RCD
109400110826     C                   EndIF
109500110826     C                   EndIF
109600110826     C                   ENDIF
109700110826      *
109800110908      *    se pianificato 35-ON
109900110908     c                   setoFF                                       35
110000110908     c                   if        D1TORP = Pianificato
110100110908     c                   setoN                                        35
110200110908     C                   EndIF
110300110908      *
110400110905      * se deve rivedere un ordine già immesso
110500110905     c                   move      rev_ordine    *in12
110600110908      *
110700110826      *  Emissione SFL dei GIRI
110800110826      * ?            *-----------------------*
110900110826     C                   EXFMT     TII5C01
111000110826      * ?            *-----------------------*
111100110826     c                   setoff                                       99
111200110826     C                   movel     'S1'          sav$GEST
111300110826      *
111400110826     C     C1NRR         IFNE      0
111500110826     C                   Z-ADD     C1NRR         WSFL
111600110826     C                   ENDIF
111700110826     C                   Z-ADD     SFLNRR        C1RCD
111800110826     C* Selezioni
1119001108261    C                   SELECT
112000110826     C* F3=Fine
112100110826     C     $TASTO        WHENEQ    F03
112200110826     C                   EXSR      F03_exit
112300110826      *
112400110826     C* F12=Ritorno
112500110826     C     $TASTO        WHENEQ    F12
112600110826     c                   move      'S0'          $GEST
112700110826     C                   MOVE      *ON           $INZS0
112800110826      *
1129001108261O   C                   OTHER
113000110826     C* CONTROLLO testata
113100110826     C                   EXSR      CTRC1
113200110826     C* CONTROLLO subfile
113300110826     C     *IN99         IFEQ      *OFF
113400110826     C                   EXSR      CTRS1
113500110826     C                   END
113600110826      *
113700110826      * ? Scrive   l'ordine
113800110826     C     *IN99         IFEQ      *OFF
113900110826     c     ERRmsg80      andEQ     *blank
114000110826     C     WMAX          andGT     0
114100110905     C     $TASTO        ifeq      F10
114200110905     C     $TASTO        oreq      F08
114300110905      **
114400110902     C* F10=Valida l'Ordine come NON PIU' manutenibile
114500110905     C     $TASTO        ifeq      F10
114600110906     C                   EXSR      F10_Chiude_ORD
114700120313     C                   z-add     0             Num_Ordine        5 0
114800110905     c                   end
114900110905      **
115000110826     C                   MOVE      *ON           $INZD1
115100110826     C                   movel     'D1'          $GEST
115200110826     c                   end
115300110905     c                   end
115400110826      *
1155001108261-   C                   ENDSL
115600110826     C*
115700110826     C                   ENDSR
115800110826     C/EJECT
115900110826      *---------------------------------------------------------------*
116000110826      *? INIZIALIZZAZIONE e IMPOSTA il PIEDE del SFL                 ?
116100110826      *---------------------------------------------------------------*
116200110826     C     INZZ1         BEGSR
116300110826     C*
116400110826     C* Messaggio o tasti funzionali
116500110826     c                   clear                   z1dmsg
116600110826     c                   clear                   ERRmsg80         80
116700110826     C*
116800110826      * imposta eventuali errori
116900110826     c                   eval      z1dmsg = ERRmsg80
117000110826     C*
117100110826     C                   ENDSR
117200110826     C/EJECT
117300110826      *---------------------------------------------------------------*
117400110826      *? INIZIALIZZAZIONE LISTA dei GIRI da selezionare              ?
117500110826      *---------------------------------------------------------------*
117600110826     C     INZS1         BEGSR
117700110826     C*
117800110826     C* reset indicatori DSPATR
117900110826     c                   exsr      INZ_indERR
118000110826     C* pulizia SFL
118100110908     C                   SETOFF                                       3031
118200110826     C                   WRITE     TII5C01
118300110826     C                   SETON                                          31
118400110826     C*
118500110826     C* Righe x Pagina del 1° SFL
118600110826     c                   z-add     RigPag_SFl1   s1pag
118700110826      *
118800110826     C* CARICAMENTO SFL totale
118900110826     C                   Z-ADD     0             S1NRR
119000110826     C                   Z-ADD     1             C1RCD
119100110826     C                   Z-ADD     0             WMAX
119200110826     C                   Z-ADD     0             Wsfl
119300110826     C* campi titolo
119400110826     C                   MOVEL     dsPGM         c1CPGM
119500110826     C                   MOVEL     knsif         c1nsif
119600110826     C                   MOVEL     knmus         c1nmus
119700110826     C                   MOVEL     RSUT          c1crsu
119800110826     C                   z-add     settimana     c1week
119900110830     C                   z-add     num_ordine    c1NOR
120000110830     C                   z-add     data_ordine   c1DOR
120100111012     C                   z-add     d1cfgs        c1FIL
120200110826      **----------------
120300110826     C* Posizionamento su file pilota
120400110830     C*   Riepilogo per numero Ordine Tutti gli articoli selezionati
120500110826     C                   eval        $EFILE    = *OFF
120600110906     C     c1NOR         setll     tmORP02L
120700110826      **----------------
120800110826     C* Carico il SFL
120900110826     C                   EXSR      ROLS1
121000110826     C*
121100110826     C                   Z-ADD     1             WPAG
121200110826     c                   eval       sav_Wmax =  Wmax
121300110826     C*
121400110826     C                   ENDSR
121500110826     C/EJECT
121600110826      *---------------------------------------------------------------*
121700110826      *? CARICAMENTO PAGINA LISTA                                    ?
121800110826      *---------------------------------------------------------------*
121900110826     C     ROLS1         BEGSR
122000110826     C*
122100110826     C                   SETOFF                                       32
122200110831     C                   Z-ADD     1             Y
122300110826     C                   Z-ADD     WMAX          S1NRR
122400110826     C*
122500110826     C* Caricamento del Sfl x Giro
122600110826     c                   Exsr      Write_S1
122700110826     C*
122800110826     C                   Z-ADD     S1NRR         WMAX                 30
122900110826     C*
123000110826     C* POSIZIONAMENTO AL 1° RCD DELLA PAGINA
123100110826     C     S1NRR         DIV       S1pag         PAGINE            4 0
123200110826     C                   MVR                     RESTO             3 0
123300110826     C     PAGINE        MULT      S1pag         C1RCD
1234001108261    C     RESTO         IFGT      0
123500110826     C                   ADD       1             C1RCD
1236001108261E   C                   ELSE
123700110826     C                   SUB       S1pag         C1RCD
123800110826     C                   ADD       1             C1RCD
1239001108261-   C                   ENDIF
124000110826     C*
124100110826     C                   ENDSR
124200110826      *---------------------------------------------------------------*
124300111014      *?  Scrive x Ogni                                              ?
124400110826      *---------------------------------------------------------------*
124500110826     C     Write_S1      BEGSR
124600110826      *
124700110826      *  Precarica da Ordine Pianificato
124800110826     c                   exsr      Legge_rec1
124900110826      *
125000110826      * Finchè il giro è lo stesso
1251001108311    C     $EFILE        DowEQ     *OFF
125200110826     C                   clear                   TII5S01
125300110826      *
125400110826      * Solo x ordini mensili
125500110826     c                   exsr      carica_campi1
125600110826      *---------
125700110826     C                   ADD       1             S1NRR
125800110826     C                   ADD       1             Y
125900110826     C                   WRITE     TII5S01
126000110826      *---------
126100110826     c                   exsr      Legge_rec1
126200110826     c                   EndDO
126300110826     C*
126400110826     C                   ENDSR
126500110826      *---------------------------------------------------------------*
126600110826      *?  LETTURA RCD ARCHIVIO PILOTA                                ?
126700110826      *---------------------------------------------------------------*
126800110826     C     LEGGE_REC1    BEGSR
126900110826     C*
127000110826     C*  Per EoF deve uscire
127100110826     C*  Per Record Ok deve uscire riportando i dati
127200110826     C                   MOVEL     *OFF          $EFILE
127300110826     C                   MOVEL     *OFF          $RCDOK
127400110826     C*
1275001108261    C     $EFILE        DOUEQ     *ON
127600110826     C     $RCDOK        OREQ      *ON
127700110826     C*
127800110830      * ? Riepilogo x Numero d'Ordine di tutti gli articoli memorizzati
127900110830     c****   LETTURA RECORD x N°ORDINE
128000110906     C     c1NOR         reade     tmORP02L
128100110826      * Fine File
128200110906     C                   IF        %Eof(tmORP02l)
128300110826     C                   MOVEL     *ON           $EFILE
128400110826     C                   MOVE      $EFILE        *IN33
128500110826     C                   Else
128600110826      * ?Controlla se la Spedizione è da considerare
128700110826     C                   MOVEL     *ON           $RCDOK
128800110826     C                   exsr      Check_RECord1
128900110826     C                   END
129000110826     C*
1291001108261-   C                   ENDDO
129200110826     C*
129300110826     C                   ENDSR
129400110826      *---------------------------------------------------------------*
129500110826      *?   CONTROLLO il record letto se va bene                      ?
129600110826      *---------------------------------------------------------------*
129700110826     C     Check_Record1 BEGSR
129800110826     C*
129900110826     C*
130000110826     C     end_check1    ENDSR
130100110826      *---------------------------------------------------------------*
130200110826      *?  Carica gli articoli pianificati per il periodo x  Filiale  ?
130300110826      *---------------------------------------------------------------*
130400110826     C     Carica_campi1 BEGSR
130500110826     C*
130600110908     c                   setoff                                       34
130700111013     C*
130800110908     c                   eval      scritto_almeno_una_riga_ordine ='S'
130900110908     C*
131000110826     C*  carica i campi e li protegge
131100110902     c                   z-add     orp_nrr       h1nrrorp
131200110830     c                   eval      s1MAT =  orpMAT
131300110908     C*
131400110908     C* i Sospesi possono essere solamente per Ordini pianificati e di
131500110908     C*   articoli Obbligatori.
131600110906     c                   if        orpATT = 'S'
131700110906     c                   eval      s1selSOS = orpATT
131800110908     c                   eval      h1selSOS = orpATT
131900111013     c                   move(p)   'SOSP'        s1sospeso
132000110906     c                   end
132100110908      **
132200110906     c                   if        orpATT = 'A'
132300110908     c                   eval      h1sosp = 'A'
132400110908     c                   eval      h1selSOS = orpATT
132500110906     c                   eval      s1selSOS = *blank
132600111013     c                   move(p)   '*SOSP'       s1sospeso
132700110906     c                   end
132800110908      **
132900110830     c                   eval      s1QTA =  orpQTA
133000110830     c                   if        orpNOTE <> *blank
133100110830     c                   move      '*'           s1not
133200110908     c                   move      '*'           h1not
133300110830     c                   eval      h1NOTA1 = %Subst(orpNOTE:01:60)
133400110830     c                   eval      h1NOTA2 = %Subst(orpNOTE:61:60)
133500110830     c                   eval      h1NOTA3 = %Subst(orpNOTE:121:60)
133600110830     c                   eval      h1NOTA4 = %Subst(orpNOTE:181:60)
133700110830     c                   end
133800110826     C*
133900110826     C* decodifiche Anagrafica articoli
134000110826     c     s1mat         chain     tmAMP01l
134100110826     c                   if        %Found(tmAMP01L)
134200110826     c                   eval      s1DES =  AMPDMT
134300110826     c                   eval      s1UMA =  AMPUMA
134400110826     c                   else
134500110826     c                   eval      s1DES =  *all'*'
134600110826     c                   eval      s1UMA =  *all'*'
134700110826     c                   end
134800110908     C*
134900110908     C* SOLO PER ORDINI PIANIFICATI su articoli obbligatori
135000110908     C*   SPROTEGGO il campo sospensione
135100110908     c                   if           d1TORP = Pianificato
135200110908     C*      Classe Merceologica
135300110908     c                   clear                   obbligatorio      1
135400110908     c                   clear                   facoltativo       1
135500110908     c                   clear                   a_consumo         1
135600110908     C*
135700110908     c                   move      Classe_155    tabGRU
135800110908     c                   move      *all'0'       tabCOD
135900110908     c                   move      ampCLM        tabCOD
136000110908     c     kAnTAB        chain     antab01l
136100110908     c                   if          %Found(AnTAB01l)
136200110908     c                   eval      antab155 = tabDES
136300110908     c                   eval         obbligatorio = PE1155v
136400110908     c                   eval         facoltativo  = PE2155v
136500110908     c                   eval         a_consumo    = PE3155v
136600110908      ****
136700110908     c                   if          obbligatorio <> *blank and h1sosp <>'A'
136800111014     c                               and s1selsos <> 'S'
136900110908     C                   setON                                        34
137000110908     c                   end
137100110908     c                   end
137200110908     c                   end
137300110908     C*
137400111014     C                   eval      h1in34 = *in34
137500111014     C*
137600110826     C                   ENDSR
137700110826      *---------------------------------------------------------------*
137800110826      *?   CONTROLLO TESTATA LISTA                                   ?
137900110826      *---------------------------------------------------------------*
138000110826     C     CTRC1         BEGSR
138100110826     C*
138200110826     C                   MOVE      *OFF          *IN99
138300110826     C*
138400110826     C                   ENDSR
138500110826      *---------------------------------------------------------------*
138600110826      *?   CONTROLLO OPZIONI LISTA                                   ?
138700110826      *---------------------------------------------------------------*
138800110826     C     CTRS1         BEGSR
138900110826     C*
139000110826     C                   MOVEL     *OFF          $ESCI
139100110826     c                   clear                   ERRmsg80
139200110826     C                   SETOFF                                       99
139300110826     C*
139400110826     C* Leggo il sfl solo se ci sono rcd
1395001108261    C     WMAX          IFGT      0
139600110826     C                   READC     TII5S01                                21
139700110826     C*
139800110826     C* esce se fine sfl o errore che richiede l'uscita
1399001108262    C     *IN21         DOWEQ     *OFF
140000110826     C     $ESCI         ANDEQ     *OFF
140100110826     C                   Z-ADD     S1NRR         C1RCD
140200110826     C* ctrl su riga
140300110826     C                   EXSR      RECS1
140400110826      *
140500110826     C* se rilevato errore o richiesta uscita, attivo sflnxtchg
1406001108263    C     *IN99         IFEQ      *ON
140700110826     C     $ESCI         OREQ      *ON
140800110826     C* disabilito l'eventuale richiesta di reinizializzazione sfl;
140900110826     C* la ripristinerò a conclusione del ciclo di READC
141000110826     C                   MOVE      *OFF          $INZS1
1411001108263-   C                   ENDIF
141200110826     C*
141300110826      * ? Attivo sempre il SFLNXTCHG e aggiorno SFL
141400110826     C                   MOVE      *ON           *IN32
141500111014     C                   eval       *in34 = h1in34
141600110826     C                   UPDATE    TII5S01
141700110826     C*
141800110826     C* leggo prossimo rcd dal sfl se no richiesta uscita ciclo
1419001108263    C     $ESCI         IFEQ      *OFF
142000110826     C                   READC     TII5S01                                21
142100110826      *
142200110826     C* a fine ciclo ripristino il flag richiesta iniz.sfl
1423001108264    C     *IN21         IFEQ      *ON
142400110826     C* calcolo pagina a cui deve posizionarsi
142500110826     c                   z-add     s1pag         Wpagine
142600110826     C                   EXSR      CLCPAG
1427001108264-   C                   ENDIF
1428001108263-   C                   ENDIF
142900110826     C*
1430001108262-   C                   ENDDO
143100110826     C*
1432001108261-   C                   ENDIF
143300110826     C* __________________________________________________
143400110826     C*  errori da evidenziare tramite finestra di ALLERTA
143500110826     c                   exsr      errori
143600110826     C* __________________________________________________
143700110826     C*
143800110826     C                   ENDSR
143900110826     C/EJECT
144000110826      *---------------------------------------------------------------*
144100110826      *?   CONTROLLO CAMPI I/O RIGA LISTA                            ?
144200110826      *---------------------------------------------------------------*
144300110826     C     RECS1         BEGSR
144400110826     C*
144500110826     C* reset indicatori DSPATR
144600110826     c                   exsr      INZ_indERR
144700110826     C*
144800110830     c* se è richiesta manutenzione NOTE
144900110830     C* gestione opzioni
1450001109063    C     s1selNOT      IFNE      *blank
145100110830     C     *IN99         ANDEQ     *OFF
145200110830     C                   EXSR      SCE_S1
145300110906     c                   clear                   s1selNOT
1454001108303-   C                   ENDIF
145500110826     C*
145600110906     c                   clear                   s1sospeso
1457001109083    C     s1selSOS      IFNE      h1selsos
145800110906     C     *IN99         ANDEQ     *OFF
145900110906     C                   EXSR      SCE_S1
1460001109063-   C                   ENDIF
146100110906     C*
146200110826     C     endSF1        ENDSR
146300110830      *---------------------------------------------------------------*
146400110830      *?   GESTIONE OPZIONE NOTE                                     ?
146500110830      *---------------------------------------------------------------*
146600110830     C     SCE_S1        BEGSR
146700110830     C*
146800110908     c                   clear                   cambia_ORP        1
146900110909     C*
147000110909     C*  INSERIMENTO NOTE:
147100110909     C*  ----------------
147200110908     C*
1473001109063    C     s1selNOT      IFNE      *blank
147400110909     C*
147500110908     c                   clear                   s1selNOT
147600110909     c                   clear                   NOTfree
147700110909     C*
147800110830     c                   eval      notMAT = s1MAT
147900110830     c                   eval      notDES = s1DES
148000110830     c                   eval      notQTA = s1QTA
148100110830     c                   eval      not001 = h1nota1
148200110830     c                   eval      not002 = h1nota2
148300110830     c                   eval      not003 = h1nota3
148400110830     c                   eval      not004 = h1nota4
148500110830     C*
148600110830     C*  Selezionata la riga per visualizzare le note
148700110830     C                   exfmt     TII5NOT
148800110908     C*
148900110908     c                   clear                   s1not
149000110908     c                   if             not001 <> *blank or
149100110908     c                                  not002 <> *blank or
149200110908     c                                  not003 <> *blank or
149300110908     c                                  not004 <> *blank
149400110908     c                   movel     '*'           s1not
149500111014     c                   end
149600110908     C*
149700110902     c                   eval       h1nota1 = not001
149800110902     c                   eval       h1nota2 = not002
149900110902     c                   eval       h1nota3 = not003
150000110902     c                   eval       h1nota4 = not004
150100110906     C*
150200110907     c                   end
150300110908     C*
150400110909     C*  SOSPENSIONE ARTICOLO:
150500110909     C*  --------------------
150600111014      **
150700111014     C*  se rilevata variazione sulla sospensione di qualche articolo
150800111014     c                   if          s1SELsos <> h1SELsos
150900111014     c                   eval         Err001 = errmsg(13)
151000111014     c                   eval         Err002 = errmsg(14)
151100111014     c                   eval         Err003 = *blank
151200111014     c                   end
151300110909     C*
151400110907     C* Deve comunque aggiornare x NOTE oppure se sospeso
151500110908     c                   if          s1not <> h1not
151600110907     c                                  or
151700110908     c                               s1SELsos <> h1SELsos
151800110909     C*
151900110908     c                   eval      h1not = s1not
152000110908     C*
152100110906     c                   open      tmorp00f
152200110906     c     h1nrrorp      chain     tmorp00f
152300110906     c                   if        %Found(tmorp00f)
152400110906      *
152500110902     c                   eval      orpNOTE = h1nota1 + h1nota2 + h1nota3 +
152600110902     c                                       h1nota4
152700110906      *
152800110908     c                   if        s1SELsos <> h1SELsos  and
152900110906     c                               h1sosp = *blank
153000110906      * sospeso dalla Filiale --> "S"
153100111014     c                   setoff                                       34
153200110908     c                   eval      orpATT = s1SELsos
153300110908     c                   clear                   s1sospeso
153400110908     c                   if        orpatt <> *blank
153500111013     c                   move      'SOSP'        s1sospeso
153600111014     c                   seton                                        34
153700110906     c                   end
153800110908     c                   end
153900110906      *  invece
154000110906      * sospeso da Sede --> "A"
154100110906     c                   if          h1sosp = 'A'
154200111013     c                   move      '*SOSP'       s1sospeso
154300111014     c                   seton                                        34
154400110906     c                   end
154500110906      *
154600110906     c                   update    tmorpfis
154700110906     c                   close     tmorp00f
154800111014     C*
154900111014     c                   eval        h1SELsos =  s1SELsos
155000110902     c                   end
155100110907     C*
155200110907     c                   end
155300110830     C*
155400110830     C                   ENDSR
155500110830     C/EJECT
155600070302      *---------------------------------------------------------------*
155700070320      *?  GESTIONE F06 x aggiornare il Data Base                     ?
155800070302      *---------------------------------------------------------------*
155900070302     C     F06_Aggiorna  BEGSR
156000070306      **
156100070305     C*
156200070302     C                   ENDSR
156300110826     C/EJECT
156400110826      *---------------------------------------------------------------*
156500110826      *?  GESTIONE F10 x imissione Multipla                          ?
156600110826      *---------------------------------------------------------------*
156700110906     C     F10_chiude_ORDBEGSR
156800110826      *
156900110906     c                   eval      premuto_F10 = 'S'
157000110906      *
157100110830      *    imposta il flag di fine manutenzioni Ordine per essere poi confermato
157200110902      **
157300110902      **----------------
157400110902     C* Posizionamento su file pilota
157500110902     C*   Riepilogo per numero Ordine Tutti gli articoli selezionati
157600110902     C                   eval        $EFILE    = *OFF
157700110906     C     c1NOR         setll     tmORP02L
157800110902     C                   MOVEL     *OFF          $EFILE
157900110902     C                   MOVEL     *OFF          $RCDOK
158000110902     C*
1581001109021    C     $EFILE        DOUEQ     *ON
158200110902     C*
158300110902      * ? Riepilogo x Numero d'Ordine di tutti gli articoli memorizzati
158400110906     C     c1NOR         reade     tmORP02L
158500110902      * Fine File
158600110906     C                   IF        %Eof(tmORP02l)
158700110902     C                   MOVEL     *ON           $EFILE
158800110902     C                   Else
158900110902      * ?aggiorna
159000110902     C                   MOVEL     *ON           orpCONF
159100110906     C                   update    tmORP000
159200110902     C                   END
159300110902     C*
1594001109021-   C                   ENDDO
159500110902     C*
159600110921     c                   eval      d1nor = c1nor
159700110921     c                   exfmt     tii5imm
159800110902      **----------------
159900110902      **
160000110826     C                   ENDSR
160100110826     C/EJECT
160200110826      *---------------------------------------------------------------*
160300110826      *?  GESTIONE F03 x uscire LR                                   ?
160400110826      *---------------------------------------------------------------*
160500110826     C     F03_exit      BEGSR
160600110909      **
160700110909     C* se si è su un altro video rispetto a prima occorre resettare
160800110909      **  la finestra con il messaggio di avviso.
160900110909     c                   if        cambia_video = 'S'
161000110909     c                   eval      dopo_alert_window    = *blank
161100110909     c                   end
161200110906     C* fine programma
161300110906     c****
161400110906      *  se ha emesso la finestra di allerta per un ordine non CONFERMATO
161500110906      *   in DEFINITIVA deve eseguire la cancellazione dell'ordine per poi
161600110906      *    uscire forzatamente.
161700110906     c                   if           dopo_alert_window ='S' and
161800110906     c                             scritto_almeno_una_riga_ordine = 'S'
161900110906     c                                and premuto_F10 = *blank
162000110906     c                                and   D1TORP = aConsumo
162100110906     C                   MOVE      'CO'          $GEST
162200110906     c                   end
162300110906     c****
162400110906      *   deve emettere la prima volta la Window di avviso per
162500110906      *     Ordine NON CONFERMATO DEFINITIVAMENTE
162600110906     c                   if        scritto_almeno_una_riga_ordine = 'S'
162700110906     c                                and premuto_F10 = *blank  and
162800110906     c                                dopo_alert_window = *blank
162900110906     c                                and   D1TORP = aConsumo
163000110906      *
163100110906     c                   eval      Err001 = ErrMsg(11)
163200110909     c                   eval      Err002 = ErrMsg(15)
163300110906     c                   eval      Err003 = ErrMsg(12)
163400110906      *   Messaggio errore a Finestra
163500110906     C                   MOVE      *off          dopo_in_$FINE
163600110906     C                   MOVE      'W1'          $GEST
163700110906     c                   end
163800110906      *
163900111014     c****
164000111014      *   deve emettere la Window di avviso per
164100111014      *     Ordine NON CONFERMATO DEFINITIVAMENTE su Pianificato
164200111014     c                   if        scritto_almeno_una_riga_ordine = 'S'
164300111014     c                                and   D1TORP = Pianificato
164400111014      *
164500111014     c                   eval      Err001 = ErrMsg(20)
164600111014     c                   eval      Err002 = ErrMsg(21)
164700111014     c                   eval      Err003 = ErrMsg(22)
164800111014      *   Messaggio errore a Finestra
164900111014     C                   MOVE      *oN           dopo_in_$FINE
165000111014     C                   MOVE      'W1'          $GEST
165100111014     c                   end
165200111014      *
165300110906      * Se ha scritto DEFINITIVAMENTE L'Ordine può uscire dal programma
165400110906      **  oppure
165500110906      * Se non ha fatto nulla può uscire dal programma
165600110906     c                   if        scritto_almeno_una_riga_ordine = 'S'
165700110906     c                                and premuto_F10 = 'S'
165800110906     c                                       or
165900110906     c                             scritto_almeno_una_riga_ordine = *blank
166000110906     C                   MOVE      *ON           $FINE
166100110906     c                   end
166200110826      *
166300110826     C                   ENDSR
166400110826     C/EJECT
166500110825      *---------------------------------------------------------------*
166600110825      * ?  CALCOLO PAGINA FINO A CUI DEVE ESSERE RICARICATO IL SFL   ?
166700110825      *---------------------------------------------------------------*
166800110825     C     CLCPAG        BEGSR
166900110825     C* Input :
167000110825     C* - WSFL = numero dell'ultimo rcd su cui era POSIZIONATO il
167100110825     C*          cursore
167200110825     C* - Wpagine= numero rcd per pagina sfl
167300110825     C* Output :
167400110825     C* - WPAG = pagina fino a cui deve essere ricaricato il sfl
167500110825     C*
167600110825     C     WSFL          DIV       Wpagine       PAGINE            4 0
167700110825     C                   MVR                     RESTO             3 0
167800110825     C     RESTO         IFGT      0
167900110825     C                   ADD       1             PAGINE
168000110825     C                   ENDIF
168100110825     C     PAGINE        MULT      Wpagine       WPAG
168200110825     C*
168300110825     C                   ENDSR
168400110825     C************************************************************
168500110825      *?  INIZIALIZZA indicatori di errore                           ?
168600110825     C************************************************************
168700110825     C     INZ_indERR    BEGSR
168800110825      *
168900110825     C                   move      *ALL'0'       IN5190           40
169000110825     C                   moveA     IN5190        *IN(51)
169100110825      *
169200110825     C                   ENDSR
169300070301     C/EJECT
169400070301      *---------------------------------------------------------------*
169500070316      *  ?  OPERAZIONI INIZIALI      ?
169600070301      *---------------------------------------------------------------*
169700940131     C     *INZSR        BEGSR
169800110726     C*
169900110908     C*  chiavi x files.
170000110824     c     kTnTBE        klist
170100110824     C                   KFLD                    tbeCOD
170200110824     C                   KFLD                    tbeKE1
170300110824     C*
170400110908     c     kAnTAB        klist
170500110908     C                   KFLD                    tabGRU
170600110908     C                   KFLD                    tabCOD
170700110908     C*
170800110906     c     kTmORP        klist
170900110824     C                   KFLD                    orpFIL
171000110905     C                   KFLD                    orpUFF
171100110824     C                   KFLD                    orpANNO
171200110824     C                   KFLD                    orpWEEK
171300110905     C                   KFLD                    orpPIAN
171400110824     C*
171500110906     c     kTMopp        klist
171600110906     C                   KFLD                    oppFIL
171700110906     C                   KFLD                    oppUFF
171800110830     C*
171900030113     C* Reperimento parametri
172000030113     C     *ENTRY        PLIST
172100030113     C                   PARM                    KPJBA
172200070228      *
172300110726      *  Se è EDP
172400070308     c                   CLEAR                   se_EDP            1
172500070308     c                   if        %subst(Knmus:1:3) = 'EDP'
172600070308     c                   MOVE      'S'           se_EDP
172700070307     c                   end
172800070301      *
172900110725     C     *DTAARA       DEFINE                  DTAORC                         *
173000110725     C*
173100070305     C*
173200070227      * Reperisco dati job
173300070227     c                   exsr      DatiJob
173400110901     C*  Controlla il P.O.
173500110901     c                   movel     dutLPO        Tipo_Filiale
173600110901     C     d1cFGS        CHAIN     AZORG01l
173700110901      * Non presente in organigramma
173800110901    1C                   IF        %found(azorg01l)
173900110901     c                   eval      og143 = orgDE3
174000110901     c                   if        §ogNTW = 'LOG'
174100110901     c                   movel     'L'           Tipo_Filiale
174200110901     c                   end
174300110901    1C                   ENDIF
174400110901     C*
174500070227
174600030113     C* Variabili per gestione videate
174700030113     C                   MOVE      *BLANK        $GEST             2
174800070305     C                   MOVE      *BLANK        sav$GEST          2
174900110906     C                   MOVE      *off          $FINE             1
175000110906     C                   MOVE      *off          dopo_in_$FINE     1
175100110906     C                   MOVE      *off          $INZD1            1
175200110906     C                   MOVE      *off          $INZS1            1
175300110906     C                   MOVE      *off          $INZS0            1
175400110906     C                   MOVE      *off          $EFILE            1
175500110906     C                   MOVE      *off          $ESCI             1
175600110906     C                   MOVE      *off          $RCDOK            1
175700030113     C* Indici
175800030113     C                   Z-ADD     0             X                 3 0
175900030113     C                   Z-ADD     0             Y                 3 0
176000070227     C***
176100070227     C* GIRO DATA DEL GIORNO: LA PRENDO DA TIME
176200070227     C                   TIME                    W0140            14 0
176300110824     C                   MOVE      W0140         woggi             8 0          *GG/MM/AAAA
176400110824     C                   MOVE      W0140         Anno              4 0          *GG/MM/AAAA
176500070227     C                   MOVEL     W0140         HHMM              4 0          *ORA/MINUTI
176600070319     C                   MOVEL     W0140         HHMMss            6 0          *ORA/MINUTI/sec
176700070227     C*
176800110824     C                   Z-ADD     woggi         G02DAT                         *GG/MM/AAAA
176900070227     C                   MOVEL     *BLANK        G02ERR
177000070227     C                   CALL      'XSRDA8'
177100070227     C                   PARM                    WLBDAT
177200110825     c                   Z-ADD     G02INV        WoggiAMG          8 0          *AAAA/MM/GG
177300110825     c                   eval      dataiso = %date(WoggiAMG:*iso)
177400110824     c                   eval      dataeur = dataiso
177500110825     c                   eval      oggiiso = dataiso
177600110824     c                   clear                   settimana         2 0
177700110824     C*
177800110824     C* NR.Settimana in corso
177900110824     C/EXEC SQL
178000110831     C+ SELECT week_iso(:OGGIISO) INTO :settimana FROM tmAMP00F
178100110824     C/END-EXEC
178200100505     C*
178300940117     C                   ENDSR
178400070227      *---------------------------------------------------------------*
178500070320      *?  Reperimento Dati del job (Utente/Operativi)                ?
178600070227      *---------------------------------------------------------------*
178700070228     c     DatiJOB       BEGSR
178800070227      *
178900070227     c     *dtaara       define    §azute        azuteds
179000070227     c     *dtaara       define    §datiute      ddatiute
179100070227      *
179200070227     c                   in(E)     *dtaara
179300070227     c                   IF        %ERROR or RSUT = *blanks
179400070227     c                   clear                   Tibs34Ds
179500070227     c                   call      'TIBS34R'
179600070227     c                   parm                    Tibs34Ds
179700070227     c                   in        *dtaara
179800070227     c                   ENDIF
179900080606      *
180000070227     c                   ENDSR
180100110825      *---------------------------------------------------------------*
180200110825      *? CARICAMENTO PAGINA LISTA                                    ?
180300110825      *---------------------------------------------------------------*
180400110825     c     New_Ordine    begSR
180500110825     C*
180600110825     C                   z-add     0             Conta_Tenta       3 0
180700110826     C                   z-add     0             Num_Ordine        5 0
180800110826     C                   z-add     0             Data_Ordine       8 0
180900110825     C*
181000110825     C*- LETTURA NUMERO ORDINE NUOVO DA AREA DATI
181100110825     C     SULOCK        TAG                                                    *
181200110825     C*          -------------                             *
181300110825     C     *LOCK         IN        DTAORC                               46      *
181400110825     C   46              eval      Conta_Tenta = Conta_Tenta + 1
181500110825     C   46              if        Conta_Tenta = 999
181600110825     C                   GOTO      No_numero                                    *
181700110825     c                   end
181800110825     C   46              GOTO      SULOCK                                       *
181900110826     C                   Z-ADD     ORDNUO        Num_Ordine                     *
182000110825     C                   ADD       1             ORDNUO                         *
182100110825     C                   OUT       DTAORC                                       *
182200110825     c                   move      'S'           ordine_OK
182300110826     C                   MOVE      woggi         Data_Ordine
182400110825     C*
182500110825     C     No_numero     ENDSR
182600110825     C/EJECT
182700110826      * ?-------------------------------------------------------------*?
182800110826      *?   SUBROUTINE Autorizzazione                                 ?
182900110826      * ?-------------------------------------------------------------*?
183000110826     C     GESAU         BEGSR
183100110826      *
183200110826     C* Se sarà un profilo Abilitato, imposta l'Entrata dal Primo Video
183300110826     C                   MOVE      'D1'          $GEST
183400110826     C*
183500110826      * ?- Controlla autorizzazione sull'utente
183600110826     c                   movel     dutpou        i70pog
183700110826     C* Il p.o.deve essere impostato in base al profilo
183800110826     C     dutlpo        IFEQ      '2'
183900110826     C                   MOVEL     dutpou        i70pge                         *FIL.GESTIONE
184000110826     C                   ELSE
184100110826     C                   MOVEL     SIMFEL        i70pge
184200110826     C                   END
184300110826     c                   movel     knmus         i70prf
184400110826     c                   call      'FICN70R'
184500110826     c                   parm                    ficn70ds
184600110826      *
184700110826     c                   movel     o70uni        daut
184800110826      *
184900110901     c* utente NON abilitato alla gestione magazzino OPPURE IN SEDE
185000110921     c                   if        §autmag <>'S'
185100110826      *
185200110906     c                   eval      Err001 = ' '
185300110906     c                   eval      Err002 = ErrMsg(3)
185400110826     c                   eval      Err003 = ' '
185500110826      *   Messaggio errore a Finestra
185600110906     C                   MOVE      *on           dopo_in_$FINE
185700110826     C                   MOVE      'W1'          $GEST
185800110826      *
185900110826     c                   end
186000110826     c*
186100110826     C     endAU         ENDSR
186200110826     C************************************************************
186300110906      *?  Gestione Fine programma                                ?
186400110826     C************************************************************
186500110906     C     GESCO         BEGSR
186600110826     C*
186700110906     C*  Cancella l'ordine immesso ed esce
186800110906      **----------------
186900110906     C* Posizionamento su file pilota
187000110906     C*   Riepilogo per numero Ordine Tutti gli articoli selezionati
187100110906     C                   eval        $EFILE    = *OFF
187200110906     C     c1NOR         setll     tmORP02L
187300110906     C                   MOVEL     *OFF          $EFILE
187400110906     C                   MOVEL     *OFF          $RCDOK
187500110906     C*
1876001109061    C     $EFILE        DOUEQ     *ON
187700110906     C*
187800110906      * ? Riepilogo x Numero d'Ordine di tutti gli articoli memorizzati
187900110906     C     c1NOR         reade     tmORP02L
188000110906      * Fine File
188100110906     C                   IF        %Eof(tmORP02l)
188200110906     C                   MOVEL     *ON           $EFILE
188300110906     C                   Else
188400110906      * ?cancella x uscita forzata da pgm senza convalidare
188500110906     C                   delete    tmORP000
188600110906     C                   END
188700110906     C*
1888001109061-   C                   ENDDO
188900110906     C*
189000110906      **----------------
189100110906     C*
189200110906     C*   quindi esce dal programma
189300110906     c                   eval      $FINE = *ON
189400110906     c*
189500110906     C     endEP         ENDSR
189600110906     C************************************************************
189700110906      *?  Emissione Finestra Errori                              ?
189800110906     C************************************************************
189900110906     C     GESW1         BEGSR
190000110906     C*
190100110826     C                   eval      $FINE = dopo_in_$FINE
190200110826     C                   eval      $GEST = sav$GEST
190300110906     c                   eval      dopo_alert_window    = *blank
190400110826     c*
190500110826     c                   exfmt     TII5WER
190600110906     c*
190700110906     c                   eval         Err001 = *blank
190800110906     c                   eval         Err002 = *blank
190900110906     c                   eval         Err003 = *blank
191000110906     c                   eval       dopo_alert_window    ='S'
191100110906     c*
191200110906     C* F3=Fine - Uscita Forzata
191300110906     C                   if        $TASTO = F03 and $FINE = *off
191400110906     C                   exsr      F03_exit
191500110906     c                   endIF
191600110826     c*
191700110826     c* emesso l'errore torna al video che lo aveva generato
191800110826     c*  oppure va fuori dal programma  in base a $FINE e $GEST
191900110826     c*
192000110826     C                   ENDSR
192100110726      * ?-------------------------------------------------------------*?
192200110826     C*   Routine degli errori da visualizzare sui SFL
192300110826      * ?-------------------------------------------------------------*?
192400110826     C     Errori        BEGSR
192500110906     C*
192600110906     C*  Messaggio di avviso    SE deve emettere un messaggio
192700110906     c                   if        err001 <> *blank or
192800110906     c                             err002 <> *blank or
192900110906     c                             err003 <> *blank
193000110826     C*
193100110826     c                   seton                                        99
193200110826      * ?            *-----------------------*
193300110826     C                   exfmt     TII5WER
193400110826      * ?            *-----------------------*
193500110826     C*
193600110909     c                   clear                   err001
193700110909     c                   clear                   err002
193800110909     c                   clear                   err003
193900110909     C*
194000110826     C*  Imposta per il dopo emissione errore
194100110826     C                   eval      $FINE = dopo_in_$FINE
194200110826     C                   eval      $GEST = sav$GEST
194300110906     C*
194400110906     c                   end
194500110826     C*
194600110826     c                   ENDSR
194700111013      *---------------------------------------------------------------*
194800111013      *? GESTIONE ordinamento                                        ?
194900111013      *---------------------------------------------------------------*
195000111013     C     Ordina        BEGSR
195100111013     C*
195200111013      * Per Pianificato prima gli obbligatori poi i facoltativi
195300111013     C                   CLEAR                   QLGSCB
195400111013     C                   CLEAR                   QLGSCB00
195500111013     c                   setoff                                       34
195600111013
195700111013     C                   eval      WrkSort = Obbligatori
195800111013     c                   eval      wsfl = 1
195900111013
196000111013     ** Gestione della scelta ordinamento.
196100111013     C                   SELECT
196200111013      *
196300111013      *?  Ordinamento x Obbligatori  ?
196400111013     C                   WHEN      WrkSort = Obbligatori
196500111013     ** 1 campo chiave.
196600111013     C                   EVAL      QLGNBRK    = 1
196700111013     C                   EVAL      QLGSP      = 1
196800111013     C                   EVAL      QLGSS      = %SIZE(S0SCE)
196900111013     C                   EVAL      QLGDT      = Carattere
197000111013     C                   EVAL      QLGSO      = Discendente
197100111013     C                   EVAL      QLGKL(1)   = QLGSKL
197200111013
197300111013     C                   ENDSL
197400111013
197500111013      * Load other sort parameters.
197600111013     C                   EVAL      QLGLB     = 80 + 16 * MaxKey
197700111013     C                   EVAL      QLGRL     = %SIZE(SflRcd0) - 1
197800111013     C                   EVAL      QLGRT     = 8
197900111013     C                   EVAL      QLGOKL    = 80
198000111013     C                   EVAL      QLGLKE    = 16
198100111013     C                   EVAL      QLGLSS    = 290
198200111013
198300111013      * Initialize Sort I/O API fields.
198400111013     C                   EVAL      QLGRL00  = QLGRL
198500111013     C                   EVAL      QLGRC00  = 1
198600111013     C                   CLEAR                   QUSEI
198700111013     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
198800111013
198900111013      * First step - Initialize the sort routine.
199000111013     C                   CALL      'QLGSORT'
199100111013     C                   PARM                    QLGSCB
199200111013     C                   PARM                    NotUsed
199300111013     C                   PARM                    NotUsed
199400111013     C                   PARM                    SizeList
199500111013     C                   PARM                    ReturnSize
199600111013     C                   PARM                    QUSEC
199700111013
199800111013      * Next step - Write records to I/O routine.
199900111013     C                   EVAL      QLGRT00 = Put
200000111013
200100111013     C                   DO        RrnLast       S0nrr
200200111013
200300111013     C     S0nrr         CHAIN     TII5S00
200400111013
200500111013     ** Solo le righe con Selected = 'Y' sono riordinate,
200600111013     ** quindi per fare un ordinamento di tutte le righe
200700111013     ** metto 'Y' sempre.
200800111013     C                   EVAL      Selected1 = 'Y'
200900111013
201000111013     C                   CLEAR                   QUSEI
201100111013     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
201200111013      *
201300111013     C                   CALL      'QLGSRTIO'
201400111013     C                   PARM                    QLGSCB00
201500111013     C                   PARM                    SflRcd0
201600111013     C                   PARM                    NotUsed
201700111013     C                   PARM                    SizeList
201800111013     C                   PARM                    NotUsed
201900111013     C                   PARM                    QUSEC
202000111013
202100111013     C                   ENDDO
202200111013
202300111013      * Next step - Signal end of input, clear subfile for reload.
202400111013     C                   EVAL      QLGRT00 = EndPut
202500111013     C                   CLEAR                   QUSEI
202600111013     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
202700111013
202800111013     C                   CALL      'QLGSRTIO'
202900111013     C                   PARM                    QLGSCB00
203000111013     C                   PARM                    SflRcd0
203100111013     C                   PARM                    NotUsed
203200111013     C                   PARM                    SizeList
203300111013     C                   PARM                    NotUsed
203400111013     C                   PARM                    QUSEC
203500111013
203600111013     C* pulizia SFL
203700111013     C                   SETOFF                                         3031
203800111013     C                   WRITE     TII5C00
203900111013     C                   SETON                                          31
204000111013
204100111013      * Final step - Write the records back to the subfile.
204200111013     C                   EVAL      QLGRT00 = Get
204300111013
204400111013     C                   DO        RrnLast       s0nrr
204500111013     C                   CLEAR                   QUSEI
204600111013     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
204700111013     C                   CALL      'QLGSRTIO'
204800111013     C                   PARM                    QLGSCB00
204900111013     C                   PARM                    NotUsed
205000111013     C                   PARM                    SflRcd0
205100111013     C                   PARM                    QLGRL00
205200111013     C                   PARM                    NotUsed
205300111013     C                   PARM                    QUSEC
205400111013      *
205500111013     c                   setoff                                       34
205600111013      *
205700111013     c                   if        d1Torp = Pianificato
205800111013     c                              and s0sce = '1'
205900111013     c                   seton                                        34
206000111013     c                   end
206100111013      *
206200111013     C                   WRITE     TII5S00
206300111013     C                   ENDDO
206400111013      *
206500111013     C     end_ordina    ENDSR
206600111013     C/EJECT
206700111013      *---------------------------------------------------------------------------------
206800080609      *
206900070326** ErrMsg
207000110825La Filiale non è in gestione                                                   01
207100110921Non è previsto inserire l'ordine Periodico in questa settimana.                02
207200110921                 L'Utente  non  è  autorizzato                                 03
207300110726Scelta obbligatoria                                                            04
207400110921Non è previsto inserire l'ordine Periodico in questa settimana.                05
207500110921E' già stato fatto un ordine Periodico nei giorni precedenti.                  06
207600110902L' Articolo non è presente in anagrafica                                       07
207700110902L' Articolo non appartiene all'ufficio di competenza                           08
207800110902Numero Ordine inesistente                                                      09
207900110902L'Ordine non è più disponibile                                                 10
208000110913ATTENZIONE : premendo F3 i dati inseriti andranno Perduti !!!                  11
208100110913    F3=Fine                           Enter x Continuare                       12
208200111014Se si SOSPENDE un ARTICOLO occorre inserire le NOTE per                        13
208300111014   spiegare il motivo della sospensione nell'ordine.                           14
208400110913      Per confermare i dati inseriti premere F10                               15
208500110725                                                                               16
208600110725                                                                               17
208700110725                                                                               18
208800110726                                                                               19
208900111014       L'Ordine non è stato Confermato Definitivamente.                        20
209000111014ATTENZIONE:  Solo Confermandolo Definitivamente                                21
209100111014           sarà preso in considerazione dalla SEDE.                            22
209200110726                                                                               23
209300110726                                                                               24
209400110726                                                                               25
209500110726                                                                               26
209600110726                                                                               27
209700110726                                                                               28
209800110726                                                                               29
209900110726                                                                               30
210000110726                                                                               31
210100110725                                                                               32
210200110726                                                                               33
210300110725                                                                               34
210400070829                                                                               35
210500070829                                                                               36
210600070829                                                                               37
210700070829                                                                               38
210800070829                                                                               39
210900110726Il valore immesso per il campo non è valido.                                   40
