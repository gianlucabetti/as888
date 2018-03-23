000100121115     H DECEDIT('0,') DATEDIT(*DMY.) DFTACTGRP(*NO) ACTGRP('FICNG5R')
000200121011     H*--------------------------------------------------------------*
000300121011      * utility per verificare se a seconda del tipo servizio passato
000400121011      * e filiale esistono conteggi o rettifiche non ancora firmate
000500121011      * in tal caso non si può procedere con la autofatturazione
000600121011      * I limiti, in termini di giorni trascorsi oltre i quali dare
000700121011      * l'ALERT, sono definiti nella tabella - DS5AAUT -
000800121011      * Il primo serve per confrontare quanti giorni sono trascorsi
000900121011      * dalla valorizzazione più vecchia NON firmata e quindi avvisare
001000121011      * emettendo una finestra con la situazione della Filiale.
001100121011      * Il secondo serve per confrontare se i giorni trascorsi hanno
001200121011      * superato tale limite x bloccare ogni tipo di Valorizzazione x
001300121011      * quella filiale fintanto che non si è sistemato tutto il pregresso.
001400121011      *--------------------------------------------------------------*
001500121108      * Aggiunto un ulteriore tipo di controllo solo durante la Contabilizzazione.
001600121108      * Il programma deve essere chiamato con la "C" (dalla Contabilizzazione)
001700121108      * e con il Tipo di Valorizzazione C/A/D quindi NON BLANK, si deve eseguire
001800121108      * un altro tipo di controllo basato solo sull'esistenza di una data estratta
001900121108      * dal singolo SQL. In presenza di tale data occorre permettere di Forzare F8
002000121108      * e proseguire la Contabilizzazione oppure bloccare e non proseguire sul
002100121108      * FICN50R.
002200121108      *--------------------------------------------------------------*
002300121011      *  12           NON permette di proseguire con le valorizzazioni
002400121011      *               non facendo vedere F12
002500040930      *  11           x selezione di un codice da ripassare al pgm chiamante
002600940307      *  21           GENERICO OPERAZIONI I/O
002700940224      *  22           GENERICO ERRORE OPERAZIONI I/O
002800940224      *  30           SFLDSP
002900940224      * N31           SFLCLR
003000940224      *  31           SFLDSPCTL
003100940224      *  32           SFLNXTCHG
003200940224      *  33           SFLEND
003300940224      *  39           OF PRTF
003400940224      *  40 <---> 49  DSPATR ERRORI SU SFL
003500940608      *  Specificare l'uso dei singoli indicatori
003600940224      *  50 <---> 98  ERRORI SU VIDEO
003700940608      *  Specificare l'uso dei singoli indicatori
003800940506      *  97           ERRORE SPECIALE : TASTO   NON ABIL.
003900940223      *  98           ERRORE SPECIALE : RICERCA NON ABIL. NELLA POSIZ.
004000940224      *  99           INDIC. GENERALE DI ERRORE
004100940128     F*----------------------------------------------------*
004200121011     FTABEL00F  IF   E           K DISK
004300121015     FTntbe01L  IF   E           K DISK
004400121011     FFICNG5D   CF   E             WORKSTN
004500940607     F                                     SFILE(S1:S1NRR)
004600940201     F                                     INFDS(DSFMT)
004700940128     D*----------------------------------------------------*
004800940211     D* Passaggio Parametri
004900940211     D KPJBA         E DS
005000121011     D ficnG5ds      E DS
005100121011     D ds5aAUT       E DS
005200121011     D CNCR80        E DS
005300121011     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
005400121015     D dvpocont      E DS
005500121206     D dblc          E DS
005600121206     D dblo          E DS
005700121206     D dbld          E DS
005800940211     D*-------------
005900940211     D DSFMT           DS           512
006000940506     D  $TASTO               369    369
006100940211     D  NRG                  370    370
006200940211     D  NCL                  371    371
006300940211     D  SFLNRR               378    379B 0
006400940207     D*-------------
006500940207     D* Nome PGM a video
006600940207     D                 DS
006700940207     D  PROGR                  1     10
006800940207     D  ASTER1                 1      1    INZ('*')
006900940207     D  SIGLA                  2      9
007000940207     D  ASTER2                10     10    INZ('*')
007100940127     D*-------------
007200940127     D* Reperimento nome PGM
007300940127     D STATUS         SDS           333
007400940127     D  DSPGM            *PROC
007500940607     D* descrizione tabella
007600030113     D*-------------
007700121011     D WLBDAT          DS
007800121011     D  G02DAT                 1      8  0
007900121011     D  G02INV                 9     16  0
008000121011     D  G02ERR                17     17
008100121011     D  G02TGI                18     22  0
008200121011     d dataiso         s               d   datfmt(*iso)
008300121011     d dataeur         s               d   datfmt(*eur)
008400121011     d udatiso         s               d   Datfmt(*iso)
008500121011     d dt_ISO_limite   s               d   Datfmt(*iso)
008600121011     d dt_ISO_blocco   s               d   Datfmt(*iso)
008700121024     d VALUE_IND       s              5i 0
008800121015     d DATA_VPO        s              8s 0
008900121011     d DATA_limite     s              8s 0
009000121011     d DATA_blocco     s              8s 0
009100121011     d gg_limite       s              3s 0 INZ(40)
009200121011     d gg_blocco       s              3s 0 INZ(55)
009300121011     d MIN_data_SQL    s              8s 0
009400121206     d Limite_Estraz...
009500121206     d                 s              8s 0
009600121011     D*-------------
009700030113     C* Variabili appoggio sempre presenti in tutte le anagrafiche
009800030113$003 D S1NRR           S                   Like(C1rcd)
009900030113$003 D WSfl            S                   Like(C1nrr)
010000030113$003 D Wmax            S                   Like(C1rcd)
010100030113$003 D Wpag            S                   Like(C1rcd)
010200030113$003 D Winzs1          S                   Like($inzs1)
010300121108$003 D Controllo_Contabile_con_Forzatura...
010400121108     D                 S              1
010500121108$003 D Deve_Bloccare...
010600121108     D                 S              1
010700940207     D*-------------
010800940211     D* COSTANTI
010900940211     D*-------------
011000121011     D SFLPAG          C                   CONST(3)
011100940314     D* dimensione della schiera $MS1
011200940506     D*
011300940506     D* Tasti di funzione
011400940506     D F01             C                   CONST(X'31')
011500940506     D F02             C                   CONST(X'32')
011600940506     D F03             C                   CONST(X'33')
011700940506     D F04             C                   CONST(X'34')
011800940506     D F05             C                   CONST(X'35')
011900940506     D F06             C                   CONST(X'36')
012000940506     D F07             C                   CONST(X'37')
012100940506     D F08             C                   CONST(X'38')
012200940506     D F09             C                   CONST(X'39')
012300940506     D F10             C                   CONST(X'3A')
012400940506     D F11             C                   CONST(X'3B')
012500940506     D F12             C                   CONST(X'3C')
012600940506     D F13             C                   CONST(X'B1')
012700940506     D F14             C                   CONST(X'B2')
012800940506     D F15             C                   CONST(X'B3')
012900940506     D F16             C                   CONST(X'B4')
013000940506     D F17             C                   CONST(X'B5')
013100940506     D F18             C                   CONST(X'B6')
013200940506     D F19             C                   CONST(X'B7')
013300940506     D F20             C                   CONST(X'B8')
013400940506     D F21             C                   CONST(X'B9')
013500940506     D F22             C                   CONST(X'BA')
013600940506     D F23             C                   CONST(X'BB')
013700940506     D F24             C                   CONST(X'BC')
013800940506     D ENTER           C                   CONST(X'F1')
013900940506     D ROLDWN          C                   CONST(X'F4')
014000940506     D ROLLUP          C                   CONST(X'F5')
014100121109     D RITORNO         C                   CONST('F12=Ritorno')
014200121109     D FORZATURA       C                   CONST('F8=Forzare Avanzamento')
014300121206     D CONTABILIZZAZIONE...
014400121206     D                 C                   CONST('C')
014500121011     C*-------------------------------------------------------------------------*
014600940127     C*                MAIN LINE PROGRAM
014700121011     C*-------------------------------------------------------------------------*
014800121115     C* Reperimento parametri
014900121115     C     *ENTRY        PLIST
015000121115     C                   PARM                    KPJBA
015100121115     C                   movel     kpjbu         ficnG5Ds
015200121115     C*
015300940223     C* inizializzazione variabili
015400940223     C                   EXSR      INZVAR
015500940223     C*
015600940223     C     $FINE         DOWEQ     *OFF
015700940131     C     $GEST         CASEQ     'S1'          GESS1
015800940117     C                   END
015900940117     C                   END
016000940325     C* fine programma
016100121011     c                   eval      kpjbu = ficnG5ds
016200940325     C                   SETON                                            LR
016300121011      ************************************************************
016400121011      * OPERAZIONI INIZIALI
016500121011      ************************************************************
016600121011     C     INZVAR        BEGSR
016700121011     C*
016800121108     C* Se in contabilizzazione e per Tipologia
016900121108     C                   EVAL      Controllo_Contabile_con_Forzatura = *BLANK
017000121108     C                   EVAL       Deve_Bloccare = *BLANK
017100121206     c                   eval      limite_estraz = 20391231
017200121108      *
017300121206     c                   if        §G5CNVL = Contabilizzazione
017400121206     c                                and §G5TIPO <> *Blank
017500121108     C                   EVAL      Controllo_Contabile_con_Forzatura = 'S'
017600121206     c                   exsr      Limite__Estraz
017700121206     c                   end
017800121108     C*
017900121017     c                   clear                   §G5FUN
018000121017     c                   clear                   §G5ESITO
018100121017     C*
018200121011     C                   Z-ADD     1             CODUT
018300121011     C                   CALL      'X§PARUT'
018400121011     C                   PARM                    UT§DSE
018500121011     C                   MOVEL     REC80         CNCR80
018600121015     C                   MOVEL     RAGUT         RSUT
018700121015     C*
018800121011     C*   imposta il range da utilizzare in seguito negli SQL
018900121011     c     §G5FIL        mult      10000         DApdr             7 0
019000121011     c     DApdr         add       9999          Apdr              7 0
019100121011     C*
019200121015     c* tabbella VPO record CONT
019300121015     C                   movel     'VPO'         tbecod
019400121015     C                   MOVEL(P)  'CONT'        tbeke1
019500121015     C                   clear                   tbeke2
019600121015     C                   clear                   dvpocont
019700121015     C     KTbe          CHAIN     Tntbe01l
019800121015     c                   if        %found(tntbe01l)
019900121015     C                   movel     tbeuni        dvpocont
020000121015     C                   movel     §VPODFIR      data_VPO
020100121015     c                   else
020200121015     c                   clear                   dvpocont
020300121015     c                   end
020400121011     C***  aggancia la tabella per reperire i limiti dei giorni
020500121011     C                   movel     '5A'          cod
020600121011     C                   MOVEL(P)  'AUT'         KEY
020700121011     C                   clear                   ds5Aaut
020800121011     C     KTAB          CHAIN     TABEL00F
020900121011     c                   if            %Found(tabel00f)
021000121011     C                   movel     tbluni        ds5Aaut
021100121011     C                   move      §5AAVVFIR     gg_limite
021200121011     C                   move      §5ABLCFIR     gg_blocco
021300121011     c                   end
021400121011     C* GIRO DATA ODIERNA
021500121011     C                   TIME                    W0140            14 0
021600121011     C                   MOVE      W0140         UDATE8            8 0
021700121011     C                   Z-ADD     UDATE8        G02DAT
021800121011     C                   MOVE      *ZEROS        G02INV
021900121011     C                   MOVE      *BLANKS       G02ERR
022000121011     C                   CALL      'XSRDA8'
022100121011     C                   PARM                    WLBDAT
022200121011     C                   Z-ADD     G02INV        DATEU8            8 0
022300121011     C                   MOVE      g02inv        UDATiso
022400121011     C*
022500121011     C*   imposta la data da utilizzare in seguito con gli SQL
022600121011     C*
022700121011     c     udatiso       subdur    GG_limite:*D  dt_ISO_limite
022800121011     c     udatiso       subdur    GG_blocco:*D  dt_ISO_blocco
022900121011      *
023000121011     C                   MOVE      dt_ISO_blocco DATA_blocco
023100121011     C                   MOVE      dt_ISO_limite DATA_limite
023200121011      ***
023300121011      * Variabili per gestione videate
023400121011     C                   MOVE      *ON           $INZS1            1
023500121011     C                   MOVE      'S1'          $GEST             2
023600121011     C                   MOVE      *OFF          $FINE             1
023700121011     C                   MOVE      *OFF          $EFILE            1
023800121011     C                   MOVE      *OFF          $ESCI             1
023900121011     C                   MOVE      *OFF          $RCDOK            1
024000121015     c                   move      'NO'          emette_video      2
024100121011      * Klist
024200121011     C     KTAB          KLIST
024300121011     C                   KFLD                    CODUT
024400121011     C                   KFLD                    COD               2
024500121011     C                   KFLD                    KEY               8
024600121015      *
024700121015     C     Ktbe          KLIST
024800121015     C                   KFLD                    tbecod
024900121015     C                   KFLD                    tbeke1
025000121015     C                   KFLD                    tbeke2
025100121206     C     Ktbe1         KLIST
025200121206     C                   KFLD                    tbecod
025300121206     C                   KFLD                    tbeke1
025400121015      *
025500121011     C* Indici
025600121011     C                   Z-ADD     0             X                 3 0
025700121011     C                   Z-ADD     0             Y                 3 0
025800030113     C*
025900030113     C* Pulizia campi e indicatori
026000030113     C                   MOVE      *ALL'0'       IN4049           10
026100030113     C                   MOVEA     IN4049        *IN(40)
026200121011     C*
026300030113     C* Variabili appoggio
026400030114     C                   Z-ADD     1             WPAG
026500030113     c*
026600030113     C                   ENDSR
026700940127     C************************************************************
026800121206     C* Solo se chiamato da Contabilizzazione trova il limite Estrazione
026900940127     C************************************************************
027000121206     C     Limite__EstrazBEGSR
027100121206      *
027200121206      *  lettura tabella BLC x autotrasportatori
027300121206     c                   select
027400121206     c                   when      §G5TIPO ='A'
027500121206     c                   clear                   dblc
027600121206     c                   movel(p)  '1'           tbeke1
027700121206     c                   movel     'BLC'         tbecod
027800121206     c     ktbe1         chain     tntbe01l
027900121206     c* data protocollo
028000121206     c                   if        %found(tntbe01l)
028100121206     c                   movel     tbeuni        dblc
028200121206     c                   move      §blcdtp       limite_estraz
028300121206     c                   end
028400121206      *
028500121206      *  lettura tabella BLO x cooperative
028600121206     c                   when      §G5TIPO ='C'
028700121206     c                   clear                   dblo
028800121206     c                   movel(p)  '1'           tbeke1
028900121206     c                   movel     'BLO'         tbecod
029000121206     c     ktbe1         chain     tntbe01l
029100121206     c* data protocollo
029200121206     c                   if        %found(tntbe01l)
029300121206     c                   movel     tbeuni        dblo
029400121206     c                   move      §blodtp       limite_estraz
029500121206     c                   end
029600121206      *
029700121206      *  lettura tabella BLD x aff/defl.
029800121206     c                   when      §G5TIPO ='D'
029900121206     c                   clear                   dbld
030000121206     c                   movel(p)  '1'           tbeke1
030100121206     c                   movel     'BLD'         tbecod
030200121206     c     ktbe1         chain     tntbe01l
030300121206     c* data protocollo
030400121206     c                   if        %found(tntbe01l)
030500121206     c                   movel     tbeuni        dbld
030600121206     c                   move      §blddtp       limite_estraz
030700121206     c                   end
030800121206     c                   endsl
030900121206      *
031000121206     C                   ENDSR
031100121206     C************************************************************
031200121206     C* GESTIONE LISTA
031300121206     C************************************************************
031400121206     C     GESS1         BEGSR
031500121206     C*
031600940223     C* inizializzazione videata
031700940223     C     $INZS1        IFEQ      *ON
031800940127     C                   EXSR      INZS1
031900940223     C                   MOVE      *OFF          $INZS1
032000940127     C                   ENDIF
032100121015     c***
032200121015     c***  solo se ha delle righe da mostrare
032300121015     c***
032400121016     c                   IF          emette_video = 'NO'
032500121016     C* deve uscire senza fare nulla
032600121016     C                   MOVE      *ON           $FINE
032700121016     C*
032800121016     c                   else
032900030113     C*
033000030113     C* emissione piede videata
033100030113     C                   WRITE     Z1
033200121011     C*
033300030113     C* Non ci sono records
033400940223     C     WMAX          IFEQ      0
033500940607     C                   WRITE     D1
033600030114     C                   Else
033700120412     C     Wsfl          IFgt      Wmax
033800120412     c                   z-add     wmax          Wsfl
033900120412     c                   end
034000030114     C     Wsfl          IFgt      0
034100030114     C                   Z-ADD     wsfl          C1RCD
034200030114     C                   Else
034300030114     C     Wpag          IFgt      0
034400030114     C                   Z-ADD     wpag          C1RCD
034500030114     C                   EndIF
034600030114     C                   EndIF
034700030114     C                   ENDIF
034800940127     C*
034900030113     C*              *------------------*
035000940607     C                   EXFMT     C1
035100030113     C*              *------------------*
035200030113     C*
035300940204     C     C1NRR         IFNE      0
035400940204     C                   Z-ADD     C1NRR         WSFL
035500940204     C                   ENDIF
035600940127     C                   Z-ADD     SFLNRR        C1RCD
035700030113     C* Selezioni
0358009401271    C                   SELECT
035900121011     C* F12=Immissione
036000121011     C     $TASTO        WHENEQ    F12
036100121011     C                   EXSR      F12S1
036200121011     C*
036300121108     C* F08=Forzare e proseguire
036400121108     C     $TASTO        WHENEQ    F08
036500121108     C                   EXSR      F08S1
036600121108     C*
0367009401271O   C                   OTHER
036800940127     C* CONTROLLO DATI
036900940131     C                   EXSR      CTRC1
037000940201     C     *IN99         IFEQ      *OFF
037100940131     C                   EXSR      CTRS1
037200940131     C                   END
0373009401271-   C                   ENDSL
037400940127     C*
037500121015     C                   ENDif
037600121015     C*
037700940127     C                   ENDSR
037800940224     C/EJECT
037900940127     C************************************************************
038000940131     C* INIZIALIZZAZIONE LISTA
038100940127     C************************************************************
038200940127     C     INZS1         BEGSR
038300940302     C* pulizia SFL
038400940128     C                   SETOFF                                         3031
038500940607     C                   WRITE     C1
038600940128     C                   SETON                                          31
038700940128     C*
038800030113     C* CARICAMENTO SFL totale
038900940201     C                   Z-ADD     0             S1NRR
039000030113     C                   Z-ADD     1             C1RCD
039100940128     C                   Z-ADD     0             WMAX
039200120412     C                   Z-ADD     1             WPAG
039300121011     C                   Z-ADD     0             conta3            3 0
039400121016     C                   Z-ADD     0             H1DATAsql         8 0
039500121108     c                   eval       SQLCOD = 0
039600121108     c                   eval       VALUE_IND = 0
039700940224     C*
039800121011     C* Legge 3 SQL di controllo
039900940608    >C                   EXSR      REDANA
040000121011     C*
040100121011     C* Carico il SFL max 3 righe
040200940127     C                   EXSR      ROLS1
040300030113     C*
040400121011     C*   imposta messaggio di errore
040500121011     c                   clear                   DATA_error        8 0
040600121109     c                   clear                   p1ERR0
040700121109     c                   clear                   p1ERR1
040800121011     c                   clear                   p1ERR2
040900121109     C*
041000121109     c                   setoff                                       08
041100121108     c                   if        h1dataSQL > 0 and §G5TIPO = *blank
041200121011     C*
041300121011     C*   avvisa che ci il blocco avverrà il giorno X se non si interviene
041400121016     c     *iso          move      h1dataSQL     dataiso
041500121011     c     dataiso       adddur    GG_blocco:*D  dataiso
041600121011     c                   move      dataiso       dataeur
041700121011     c                   move      dataeur       DATA_error        8 0
041800121011      **
041900121109     C* comunque deve permettere di poter tornare con F12
042000121109     c                   move      'F12=Ritorno' p1ERR0
042100121109      **
042200121109     c                   eval      p1ERR1 = 'ATTENZIONE: dal ' +
042300121011     c                             %editc(DATA_error:'Y') +
042400121109     c                             ' NON sarà più possibile valorizzare.'
042500121109     c                   eval      p1ERR2 = '             Firmare i documenti i-
042600121011     c                             n sospeso.'
042700121109     C*
042800121109     c                   else
042900121109     C*
043000121109     C* comunque deve permettere di poter tornare con F12
043100121109     c                   move      Ritorno       p1ERR1
043200121109     C*
043300121109     C*  Deve attivare il comando di Forzatura F8 = Forzabile
043400121109$003 C                   IF        Controllo_Contabile_con_Forzatura = 'S'
043500121109$003 C                              and Deve_Bloccare = 'S'
043600121109     c                   seton                                        08
043700121109     c                   movel     Forzatura     p1ERR1
043800121109     c                   end
043900121109     C*
044000121109     c                   end
044100121011     C*
044200940127     C                   ENDSR
044300940127     C************************************************************
044400940131     C* CARICAMENTO PAGINA LISTA
044500940127     C************************************************************
044600940127     C     ROLS1         BEGSR
044700940127     C*
044800940128     C                   SETOFF                                       32
044900940223     C                   Z-ADD     0             Y
045000940127     C                   Z-ADD     WMAX          S1NRR
045100940127     C*
045200940127     C* Leggo dal file anagrafico per caricare la lista
0453009401311    C     $EFILE        DOWEQ     *OFF
045400940127     C*
045500121011     c                   exsr      WRITE_RIGASFL
045600940127     C*
045700940127     C                   ADD       1             S1NRR
045800940127     C                   ADD       1             Y
045900940607     C                   WRITE     S1
046000121011     c                   if        s1NRR  = 3
046100121011     C                   leave
046200121011     c                   end
046300940131     C*
046400940316    >C                   EXSR      REDANA
0465009401271-   C                   ENDDO
046600940127     C*
046700940223     C                   Z-ADD     S1NRR         WMAX                 30
046800940127     C*
046900940127     C* POSIZIONAMENTO AL 1° RCD DELLA PAGINA
047000940127     C*
047100030113     C     S1NRR         DIV       SFLPAG        PAGINE            4 0
047200940127     C                   MVR                     RESTO             3 0
047300030114     C     PAGINE        MULT      SFLPAG        C1RCD
0474000301141    C     RESTO         IFGT      0
047500030114     C                   ADD       1             C1RCD
0476000301141E   C                   ELSE
047700030114     C                   SUB       SFLPAG        C1RCD
047800030114     C                   ADD       1             C1RCD
0479000301141-   C                   ENDIF
048000940128     C*
048100940127     C                   ENDSR
048200940128     C************************************************************
048300940131     C* LETTURA RCD ARCHIVIO PILOTA
048400940128     C************************************************************
048500940607     C     REDANA        BEGSR
048600940128     C*
048700121011     C*   deve eseguirla 3 volte sempre per controllare se le
048800121011     C*   valorizzazioni sono senza la firma ad una certa data.
048900940131     C                   MOVEL     *OFF          $RCDOK
049000121011     c                   z-add     0             MIN_data_SQL
049100121011     c                   clear                   Titolo           30
049200121011     c                   clear                   Data_minimo       3 0
049300121011     C*
049400121011     C                   add       1             conta3
049500940131     C*
049600121011     c                   select
049700121011      *-------
049800121011     c                   when      Conta3 = 1
049900121108      *
050000121108     C                   if        Controllo_Contabile_con_Forzatura = *blank
050100121108     c                                or
050200121108     C                             Controllo_Contabile_con_Forzatura = 'S'
050300121108     c                             and  §G5TIPO ='A'
050400121011     C/EXEC SQL
050500121024     C+ SELECT  min(FTTDDC) into :MIN_data_SQL INDICATOR :VALUE_IND
050600121011     C+    FROM fiftt00F WHERE fttpdr between :DApdr and :Apdr
050700121011     C+    and fttfvl = 'C' and substr(fttflr, 1, 1) = ' '
050800130207     C+    and ftttsr = ' ' and
050900130207     C+    FTTITT + FTTITA + FTTTPE + FTTTBN + FTTTIM + FTTMNT > 0,001
051000121206     C+    and fttddc >= :data_VPO  and fttddc <= :limite_estraz
051100121011     C/END-EXEC
051200130207      * deve prendere in considerazione solo quelle valorizzate e non azzerate.
051300130207      * lo zero virtuale è espresso come 0,001 anzichè 0.
051400130207      *
051500121025     c                   if           SQLCOD = 0 and VALUE_IND = 0
051600121011     c                   eval       Titolo = 'Autisti non firmati dal........'
051700121011     C                   MOVE      *ON           $RCDOK
051800121011     c                   end
051900121108     c                   endif
052000121011      *-------
052100121011     c                   when      Conta3 = 2
052200121108      *
052300121108     C                   if        Controllo_Contabile_con_Forzatura = *blank
052400121108     c                                or
052500121108     C                             Controllo_Contabile_con_Forzatura = 'S'
052600121108     c                             and  §G5TIPO ='C'
052700121011     C/EXEC SQL
052800121024     C+ SELECT  min(CTTDDC) into :MIN_data_SQL INDICATOR :VALUE_IND
052900121011     C+    FROM fictt00F WHERE cttpdr between :DApdr and :Apdr
053000121011     C+    and cttfvl = 'C' and cttfsr = ' '
053100121206     C+    and cttddc >= :data_VPO  and cttddc <= :limite_estraz
053200121011     C/END-EXEC
053300121025     c                   if           SQLCOD = 0  and VALUE_IND = 0
053400121011     c                   eval       Titolo = 'Cooperative non firmate dal....'
053500121011     C                   MOVE      *ON           $RCDOK
053600121011     c                   end
053700121108     c                   endif
053800121011      *-------
053900121011     c                   when      Conta3 = 3
054000121011      **
054100121108     C                   if        Controllo_Contabile_con_Forzatura = *blank
054200121108     c                                or
054300121108     C                             Controllo_Contabile_con_Forzatura = 'S'
054400121108     c                             and  §G5TIPO ='D'
054500121011     C/EXEC SQL
054600121024     C+ SELECT  min(ATTDDC) into :MIN_data_SQL INDICATOR :VALUE_IND
054700121011     C+    FROM fiatt00F WHERE attpdr between :DApdr and :Apdr
054800130207     C+    and attflg = 'C' and substr(attflr, 1, 1) = ' ' and
054900130207     C+        attimpP > 0,001
055000121206     C+    and attddc >= :data_VPO  and attddc <= :limite_estraz
055100121011     C/END-EXEC
055200130207      *  prima il test era con maggiore di (0) invece deve essere considerato
055300130207      * (0) l'importo minimo (0,001)
055400130207      *
055500121025     c                   if           SQLCOD = 0  and VALUE_IND = 0
055600121011     c                   eval       Titolo = 'Aff/Defl non firmate dal.......'
055700121011     C                   MOVE      *ON           $RCDOK
055800121011     c                   end
055900121108     c                   endif
056000121011      **
056100121011     c                   endsl
056200121115      **
056300121011      ** ------------
056400121016      *    imposta la data più bassa quindi alla fine h1DATAsql conterrà
056500121011      *    la data minima fra le date minime, se presenti.
056600121011      ** ------------
056700121016     C                   if          h1DATAsql = 0 and MIN_data_SQL > 0
056800121011     c                                 or
056900121116     C                               MIN_data_SQL < h1DATAsql
057000121116     c                                      and MIN_data_SQL > 0
057100121115     c* se il limite viene superato
057200121115     c*    ossia la data minima letta con SQL è inferiore della DATA limite
057300121115     c* deve iniziare a segnalare con la finestra.
057400121206     c* in contabilizzazione invece lo deve fare sempre
057500121115     C                   if               MIN_data_SQL < DATA_limite
057600121116     c                                      and MIN_data_SQL > 0
057700121206     c                                    or §G5CNVL = Contabilizzazione
057800121206     c                                      and §G5TIPO <> *Blank
057900121011      *  1 = Aut di città
058000121011      *  2 = COOP
058100121011      *  3 = Affl/Defl
058200121011     c                   z-add     conta3        Data_minimo
058300121016     C                   Z-ADD     MIN_data_SQL  h1DATAsql
058400121116     C                   EVAL       Deve_Bloccare = 'S'
058500121115     c                   end
058600121011      *
058700121011     c                   end
058800121011      ** ------------
058900121011      *
059000940131     C                   ENDSR
059100940224     C************************************************************
059200121011     C* scrive la riga del SFL
059300940224     C************************************************************
059400121011     C     WRITE_RIGASFL BEGSR
059500121011      *
059600121015     C                   clear                   S1
059700121116      *
059800121116      **  deve emettere il video solo se ha superato la data limite
059900121206     c* in contabilizzazione invece lo deve fare sempre
060000121116     C                   if               MIN_data_SQL < DATA_limite
060100121116     c                                      and MIN_data_SQL > 0
060200121206     c                                    or §G5CNVL = Contabilizzazione
060300121206     c                                      and §G5TIPO <> *Blank
060400121116      *
060500121011     c                   movel     Titolo        h1des
060600121011     c                   move      MIN_data_SQL  h1DAT
060700121011      *
060800121011      **   solo se la riga è stata trovata
060900121025     c                   if           SQLCOD = 0 and VALUE_IND = 0
061000121011     c                   movel     h1DES         s1des
061100121011      **
061200121011     c                   if        H1dat > 0
061300121015     c                   move      'SI'          emette_video      2
061400121011     c                   move      H1dat         dataiso
061500121011     c                   move      dataiso       dataeur
061600121011     c                   move      dataeur       s1DAT
061700121011     c                   else
061800121011     c                   clear                   s1DAT
061900121011     c                   endif
062000121011      *
062100121011     C                   end
062200121011      *
062300121116     c                   end
062400121116      *
062500121011     C                   ENDSR
062600121011     C************************************************************
062700121011     C* CALCOLO PAGINA FINO A CUI DEVE ESSERE RICARICATO IL SFL
062800121011     C************************************************************
062900121011     C     CLCPAG        BEGSR
063000940224     C* Input :
063100940224     C* - WSFL = numero dell'ultimo rcd su cui era POSIZIONATO il
063200940224     C*          cursore
063300940224     C* - SFLPAG = numero rcd per pagina sfl
063400940224     C* Output :
063500940224     C* - WPAG = pagina fino a cui deve essere ricaricato il sfl
063600940224     C*
063700120412     c                   if        WSFL >0
063800940224     C     WSFL          DIV       SFLPAG        PAGINE            4 0
063900940224     C                   MVR                     RESTO             3 0
064000940224     C     RESTO         IFGT      0
064100940224     C                   ADD       1             PAGINE
064200940224     C                   ENDIF
064300940224     C     PAGINE        MULT      SFLPAG        WPAG
064400120412     c                   else
064500120412     C                   Z-ADD     1             WPAG
064600120412     c                   end
064700940224     C*
064800940224     C                   ENDSR
064900940309     C/EJECT
065000940309     C************************************************************
065100121011     C* GESTIONE F12 ritorno
065200940309     C************************************************************
065300121011     C     F12S1         BEGSR
065400940309     C*
065500121011     C                   MOVE      *ON           $FINE
065600121011     c                   eval      §G5FUN = '12'
065700121011     C*  sempre forzabile
065800121011     c                   eval      §G5ESITO = 'F'
065900121011     C*
066000121011     C*   bloccante se si sono superati i limiti di blocco
066100121016     c                   if        data_BLOCCO > h1dataSQL
066200121108     c                                 or
066300121108$003 C                             Controllo_Contabile_con_Forzatura = 'S'
066400121108$003 C                              and Deve_Bloccare = 'S'
066500121011     c                   eval      §G5ESITO = 'B'
066600121011     c                   end
066700121011     C*
066800940309     C                   ENDSR
066900121108     C/EJECT
067000121108     C************************************************************
067100121108     C* GESTIONE F08 FORZA AVANZAMENTO
067200121108     C************************************************************
067300121108     C     F08S1         BEGSR
067400121108     C*
067500121108     C                   MOVE      *ON           $FINE
067600121108     C*   forzabile
067700121108     c                   eval      §G5FUN = '08'
067800121108     c                   eval      §G5ESITO = 'F'
067900121108     C*
068000121108     C                   ENDSR
068100940128     C************************************************************
068200940131     C* CONTROLLO TESTATA LISTA
068300940128     C************************************************************
068400940131     C     CTRC1         BEGSR
068500940128     C*
068600940201     C                   MOVE      *OFF          *IN99
068700940131     C*
068800940202     C                   ENDSR
068900940131     C************************************************************
069000940131     C* CONTROLLO OPZIONI LISTA
069100940131     C************************************************************
069200940131     C     CTRS1         BEGSR
069300940131     C*
069400940201     C                   SETOFF                                       99
069500940131     C*
069600940127     C                   ENDSR
069700940127     C/EJECT
069800121011     C************************************************************
