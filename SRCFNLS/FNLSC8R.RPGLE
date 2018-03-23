000100000810     H DECEDIT('0,') DATEDIT(*YMD.)
000200000000      **************************************************************************
000300970130      *Nome programma:  FNLSC8R
000400970207      *Descrizione   :  Sottomissione statistiche
000500970130      *                 Se il periodo dal-al immesso dall'utente inizia
000600970130      *                 prima(o va oltre) della data prima elaborazione
000700970130      *                 statistica(o dell'ultima data) vengono eseguite
000800970130      *                 anche le elaborazioni mancanti
000900970130      *Data creazione:  24 FEB 1994
001000000000      **************************************************************************
001100950210      *  ??PASSAGGIO PARAMETRI:?
001200950216      *  DS1TBO   'A'=ARRIVI, 'P'=PARTENZE, 'T'=TRANSITI
001300950210      *  DS1FGS   FILIALE RICHIEDENTE  ( 0=ELABORAZ.GIORNALIERE )
001400950210      *  DS1FEL   FILIALE ELABORATORE (AREA CHE SI VUOLE STAMPARE)
001500950210      *  DS1LNP   FILIALE DI CUI SI VUOLE LA STAMPA
001600950210      *  DS1GDA   DATA INIZIALE (GG/MM/AAAA)
001700950210      *  DS1GAL   DATA FINALE (GG/MM/AAAA)
001800950210      *  DS1ADA   DATA INIZIALE (AAAA/MM/GG)
001900950210      *  DS1AAL   DATA FINALE (AAAA/MM/GG)
002000950404      *  DS1RIS   'R'=RISTAMPA, 'S'=ELABOR.+STAMPA, ' '=SOLO ELABOR.
002100970206      *  DS1FPG   STATISTICA SELEZIONATA  ( ' '=TUTTE )
002200970207      *  DS1PGM   NOME PROGRAMMA DA ELABORARE E CHE SERVE PER LA CHAIN
002300970207      *           NELLA DS3L
002400950210      **************************************************************************
002500000000      *  INDICATORI UTILIZZATI:
002600000000      *  30         Di comodo, utilizzato in un ciclo
002700000000      *  31   ON    Sto eseguendo le stampe giornaliere
002800000000      *  31   OFF   Sono lanciato da filtro utente
002900000000      *  99         Generico, utilizzabile a piacere
003000000000      **************************************************************************
003100970130     FTABEL00F  IF A E           K DISK
003200020624     FFNBTP11L  IF   E           K DISK
003300000810     FAZORG01L  IF   E           K DISK
003400021017     fFista01L  uf   e           k disk    Usropn
003500050331     fFista02L  uf   e           k disk    Usropn
003600050331     f                                     rename(fista000:fista002)
003700000000      *
003800001027     D MES             S              2  0 DIM(12) CTDATA PERRCD(12)
003900050401     D EST             S              3  0 DIM(30)
004000000000      *
004100000810     D OG143         E DS
004200000810     D DS5A          E DS
004300040924     D DS5AS1        E DS
004400001027     D DS5APT        E DS
004500970130     D DS7AF         E DS
004600970130     D  ALFDPE                68     75
004700970130     D  ALFDUE                76     83
004800000000     D KPJBA         E DS
004900950210     D DSLS01        E DS
005000000000     D                 DS
005100941012     D  $DATA                  1      8  0
005200941012     D  AA                     1      4  0
005300941012     D  MM                     5      6  0
005400941012     D  GG                     7      8  0
005500001027     D* DS PER TRUL06R - CARICAMENTO £X
005600001027     D DSUL06        E DS                  EXTNAME(TRUL06DS)
005700001027     D  LIN                    1     90  0
005800001027     D                                     DIM(30)
005900000000     D WLBDAT          DS
006000941012     D  G02DAT                 1      8  0
006100941012     D  G02INV                 9     16  0
006200941012     D  G02ERR                17     17
006300941012     D  G02TGI                18     22  0
006400000000     D WGIDAT          DS
006500941012     D  GIODAT                 1      8  0
006600941012     D  GIOINV                 9     16  0
006700941012     D  GIOTGI                17     21  0
006800021022
006900021022     d SavSta        e ds                  extname(Fista00f)
007000021022     d                                     Prefix(Sav_)
007100021022     d DsSta         e ds                  extname(Fista00f)
007200021022
007300001027     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
007400000000     D CNCR80        E DS
007500000000     D DS3L          E DS
007600021016
007700021016     d Tibs02Ds      e ds
007800021016      * Ds date statistiche
007900021016     d dSdf          e ds
008000021017
008100021021     d Wfil            s                   Like(StaFil)
008200021016
008300000000      *
008400000000     C     *ENTRY        PLIST
008500000000     C                   PARM                    KPJBA
008600950210     C                   MOVEL     KPJBU         DSLS01
008700000000      *
008800021022      * CONTROLLO SE SONO P.O. POSTE (§Ogntw=PPT)
008900000810     C                   MOVEL     DS1FGS        WPOSTE            3 0
009000021022     C                   CLEAR                   §OGntw
009100000810     C     WPOSTE        CHAIN     AZORG01L                           57
009200000810     C  N57              MOVEL     ORGDE3        OG143
009300021022     c                   If        §ogntw = 'PPT'
009400021022     C                   MOVEL     'S'           FGSPT             1
009500021022     c                   EndIf
009600000912     C*
009700021022     C                   CLEAR                   §OGntw
009800000912     C                   CLEAR                   LNPPT
009900000912     C*
010000000912     C     DS1LNP        IFGT      0
010100000912     C     DS1LNP        CHAIN     AZORG01L                           57
010200000912     C  N57              MOVEL     ORGDE3        OG143
010300021022     c                   If        §ogntw = 'PPT'
010400021022     C                   MOVEL     'S'           LNPPT             1
010500021022     c                   EndIf
010600000912     C                   ENDIF
010700001027     C* CARICO  LA  £1 PER VEDERE SE HO ALMENO UN P.O. POSTE:
010800001027     C*  DEVO PRENDERE I GG DI PULIZIA PIU' BASSI TRAI 2 PER LA RIELAB
010900001027     C                   EXSR      CARTAB
011000000000      **************************************************************************
011100000000      *               M A I N     P R O G R A M
011200000000      **************************************************************************
011300940505      *
011400940505      * Se la data fine periodo e' nulla la imposto uguale a quella iniziale
011500950210     C     DS1AAL        IFEQ      *ZERO
011600950210     C                   Z-ADD     DS1ADA        DS1AAL
011700950210     C                   Z-ADD     DS1GDA        DS1GAL
011800940505     C                   ENDIF
011900000000      *
012000000000      * Memorizzo alcuni campi che mi servono
012100950210     C                   Z-ADD     DS1GDA        $VIDD
012200950210     C                   Z-ADD     DS1GAL        $VIDA
012300950210     C                   Z-ADD     DS1ADA        $COMD
012400950210     C                   Z-ADD     DS1AAL        $COMA
012500950210     C                   MOVE      DS1RIS        $STARI
012600970130     C*
012700970130     C* E' LA LINEA DI PARTENZA PER LE STATISTICHE PARTENZA
012800970130     C* E' LA LINEA DI ARRIVO PER LE STATISTICHE ARRIVO
012900970206     C                   Z-ADD     DS1LNP        $LIN
013000970206     C* MI SALVO IL TIPO STATISTICA PASSATO PERCHE' SE ' '
013100970206     C*  SONO TUTTE
013200970206     C                   MOVEL     DS1FPG        WFPG              1
013300000000      *
013400000000      * Accendo il 31 se sono lanciato dalle stampe giornaliere
013500950210     C     DS1FGS        COMP      *ZERO                                  31
013600000000      *
013700000000      * Se provengo dalle stampe giornaliere DEVO sempre rielaborare i dati
013800950210     C   31              MOVE      'S'           DS1RIS
013900000000     C   31              MOVE      'S'           $STARI
014000970130      *
014100970130     C     DS1TBO        IFEQ      'A'
014200970130     C                   EXSR      PREARR
014300970130     C                   ELSE
014400970130     C                   EXSR      PREPAR
014500970130     C                   ENDIF
014600000000      *
014700000000     C                   SETON                                        LR
014800970130     C**
014900970130      **************************************************************************
015000970130      * IMPOSTO le data aree con la data ultima elaborazione eseguita
015100970130      **************************************************************************
015200970130     C     PREPAR        BEGSR
015300970130     C**
015400970130      *
015500970130      * Problema: se 'R'=Ristampa e la data area e' vuota non stampa nulla.
015600970130      * Imposto, il primo ed ultimo giorno elaborazione, al giorno precedente
015700970130      * il periodo richiesto dall'utente (DS1ADA-DS1AAL)
015800970130     C     DS1RIS        IFEQ      'R'
015900970130     C*
016000970130     C     C1DAPE        IFEQ      *ZERO
016100970130     C     DS1ADA        SUB       1             C1DAPE
016200970130     C                   Z-ADD     C1DAPE        $DATA
016300970130     C                   EXSR      CONDAT
016400970130     C                   Z-ADD     $DATA         C1DAPE
016500970130     C                   Z-ADD     C1DAPE        C1DAUE
016600970130     C                   ENDIF
016700970130      *
016800970130     C     C2DAPE        IFEQ      *ZERO
016900970130     C     DS1ADA        SUB       1             C2DAPE
017000970130     C                   Z-ADD     C2DAPE        $DATA
017100970130     C                   EXSR      CONDAT
017200970130     C                   Z-ADD     $DATA         C2DAPE
017300970130     C                   Z-ADD     C2DAPE        C2DAUE
017400970130     C                   ENDIF
017500970130      *
017600970130     C     C3DAPE        IFEQ      *ZERO
017700970130     C     DS1ADA        SUB       1             C3DAPE
017800970130     C                   Z-ADD     C3DAPE        $DATA
017900970130     C                   EXSR      CONDAT
018000970130     C                   Z-ADD     $DATA         C3DAPE
018100970130     C                   Z-ADD     C3DAPE        C3DAUE
018200970130     C                   ENDIF
018300970130      *
018400970130     C     C5DAPE        IFEQ      *ZERO
018500970130     C     DS1ADA        SUB       1             C5DAPE
018600970130     C                   Z-ADD     C5DAPE        $DATA
018700970130     C                   EXSR      CONDAT
018800970130     C                   Z-ADD     $DATA         C5DAPE
018900970130     C                   Z-ADD     C5DAPE        C5DAUE
019000970130     C                   ENDIF
019100970130      *
019200970130     C                   ENDIF
019300970130      *
019400970130      * ?   S T A T I S T I C A    C O N T R O L L O    E N T R A T E   ?
019500970206     C     WFPG          IFEQ      *BLANKS
019600970206     C     WFPG          OREQ      'E'
019700970206     C                   MOVE      'E'           DS1FPG
019800970130     C                   MOVE      'LSC3'        KCOAZ
019900970207     C                   MOVEL     'FNLSC3R'     DS1PGM
020000970130     C                   Z-ADD     C3DAPE        $DAPE             8 0
020100970130     C                   Z-ADD     C3DAUE        $DAUE             8 0
020200970130     C                   EXSR      BCICLO
020300970130     C                   ENDIF
020400970130      *
020500970130      * ?   S T A T I S T I C A    B O L L E T T A T O   ?
020600970206     C     WFPG          IFEQ      *BLANKS
020700970206     C     WFPG          OREQ      'B'
020800970206     C                   MOVE      'B'           DS1FPG
020900970130     C                   MOVE      'LSC5'        KCOAZ
021000970207     C                   MOVEL     'FNLSC5R'     DS1PGM
021100970130     C                   Z-ADD     C5DAPE        $DAPE
021200970130     C                   Z-ADD     C5DAUE        $DAUE
021300970130     C                   EXSR      BCICLO
021400970130     C                   ENDIF
021500970130      *
021600970130      * ?   S T A T I S T I C A    P A R T E N Z E   ?
021700970206     C     WFPG          IFEQ      *BLANKS
021800970206     C     WFPG          OREQ      'P'
021900970206     C                   MOVE      'P'           DS1FPG
022000970130     C                   MOVE      'LSC1'        KCOAZ
022100970807     C                   MOVEL     'FNLSC1R'     DS1PGM
022200970130     C                   Z-ADD     C1DAPE        $DAPE
022300970130     C                   Z-ADD     C1DAUE        $DAUE
022400050331     C* Se elaborazione giornaliera o 1 lancio di data nuova di statistica
022500050331     C*    verifico se devo rielaborare dei giorni leggendo FISTA00F
022600050331     c                   if        *in31 or $daue<$coma
022700050331     C                   EXSR      FNSTA
022800050331     c                   endif
022900050331     c
023000970130     C                   EXSR      BCICLO
023100970130     C                   ENDIF
023200970130      *
023300970130      * ?   S T A T I S T I C A    A    T E R R A   ?
023400970206     C     WFPG          IFEQ      *BLANKS
023500970206     C     WFPG          OREQ      'T'
023600970206     C                   MOVE      'T'           DS1FPG
023700970130     C                   MOVE      'LSC2'        KCOAZ
023800970807     C                   MOVEL     'FNLSC2C'     DS1PGM
023900970130     C                   Z-ADD     C2DAPE        $DAPE
024000970130     C                   Z-ADD     C2DAUE        $DAUE
024100970130     C*
024200050331     C* Se elaborazione giornaliera o 1 lancio di data nuova di statistica
024300050331     C*    verifico se devo rielaborare dei giorni leggendo FISTA00F
024400050331     c                   if        *in31 or $daue<$coma
024500050331     C                   EXSR      FNSTA
024600050331     c                   endif
024700000810      * SOLO P.O. NO POSTE
024800000912     C     FGSPT         IFNE      'S'
024900000912     C     LNPPT         ANDNE     'S'
025000970130     C                   EXSR      BCICLO
025100000810     C                   ENDIF
025200000810     C                   ENDIF
025300970130     C                   ENDSR
025400970130      **************************************************************************
025500970130      * IMPOSTO le data aree con la data ultima elaborazione eseguita
025600970130      **************************************************************************
025700970130     C     PREARR        BEGSR
025800970130     C*
025900970130      * ?   S T A T I S T I C A    A R R I V I    ?
026000970206     C     WFPG          IFEQ      *BLANKS
026100970206     C     WFPG          OREQ      'A'
026200970604     C*
026300970604    1C     DS1RIS        IFEQ      'R'
026400970604     C     E6DAPE        ANDEQ     *ZERO
026500970604     C     DS1ADA        SUB       1             E6DAPE
026600970604     C                   Z-ADD     E6DAPE        $DATA
026700970604     C                   EXSR      CONDAT
026800970604     C                   Z-ADD     $DATA         E6DAPE
026900970604     C                   Z-ADD     E6DAPE        E6DAUE
027000970604    1C                   ENDIF
027100970604     C*
027200970206     C                   MOVEL     'A'           DS1FPG
027300970130     C                   Z-ADD     E6DAPE        $DAPE
027400970130     C                   Z-ADD     E6DAUE        $DAUE
027500970207     C                   MOVEL     'FNLRE7C'     DS1PGM
027600970130     C                   MOVE      *BLANKS       $PGM1            10
027700970130     C                   MOVEL     'FNLRE6R'     $PGM1
027800970604     C* Se elaborazione giornaliera verifico se devo rielaborare
027900021021     C* dei giorni leggendo FISTA00F
028000970604     C                   MOVEL     '1'           WPGM
028100050331     C* Se elaborazione giornaliera o 1 lancio di data nuova di statistica
028200050331     C*    verifico se devo rielaborare dei giorni leggendo FISTA00F
028300050331     c                   if        *in31 or $daue<$coma
028400050331     C                   EXSR      FNSTA
028500050331     c                   endif
028600970604     C*
028700970130     C                   EXSR      BCICLO
028800970130     C                   ENDIF
028900970130     C*
029000970207      * ?   S T A T I S T I C A    C O N S E G N E    ?
029100970207     C     WFPG          IFEQ      'C'
029200970604     C* PER LA STATISTICA CONSEGNE DEVO CHAINARE LA DS7A
029300970604     C                   MOVEL     '7A'          KCOD
029400970604     C                   MOVEL     $LIN          KKEY
029500970604     C     KTAB1         CHAIN     TABEL                              34
029600970604     C*
029700970604    1C     *IN34         IFEQ      *ON
029800970604     C                   EXSR      CRTTAB
029900970604    1C                   ENDIF
030000970604     C*
030100970604     C                   MOVEL     TBLUNI        DS7AF
030200970604     C     ALFDPE        IFLT      *ZERO
030300970604     C     DS1ADA        SUB       1             E6DAPE
030400970604     C                   Z-ADD     E6DAPE        $DATA
030500970604     C                   EXSR      CONDAT
030600970604     C                   MOVEL     $DATA         ALFDPE
030700970604     C                   MOVEL     $DATA         ALFDUE
030800970604     C                   ENDIF
030900970604     C*
031000970130     C                   Z-ADD     §7ACPE        $DAPE
031100970130     C                   Z-ADD     §7ACUE        $DAUE
031200970207     C                   MOVEL     'FNLR78R'     DS1PGM
031300970303     C                   MOVE      'LR78'        KCOAZ
031400970130     C* NON ESISTONO STAMPE GIORNALIERE PER LA STATISTICA CONSEGNE
031500970130     C                   EXSR      BCICLO
031600970207     C*
031700970207     C                   EXSR      FNSTA
031800970130     C                   ENDIF
031900970130     C                   ENDSR
032000000000      **************************************************************************
032100000000      * (Leggasi bi-ciclo, la routine che pedala...)
032200000000      * Partendo dalla data ultima elaborazione, viene sottomessa la statistica
032300000000      * (azione KCOAZ) tante volte quanti sono i giorni per arrivare alla data
032400950210      * fine periodo (DS1AAL)
032500000000      **************************************************************************
032600000000     C     BCICLO        BEGSR
032700980513     C                   CLEAR                   WELAB
032800041203      *
032900041203      * Richiamo la tabella 3L
033000041203     C                   MOVEL     '3L'          KCOD
033100041203     C                   MOVEL     DS1PGM        KKEY
033200041203     C                   MOVE      DS1FPG        KKEY
033300041203     C     KTAB1         CHAIN     TABEL00F                           99
033400041203     C  N99              MOVEL     TBLUNI        DS3L
033500041203     C   99              CLEAR                   DS3L
033600041203     C
033700970130     c* visto che per la statistica arrivi ho 2 pgm, imposto in un camp
033800970130     c* quale deve essere lanciato:
033900970130     c* - in quello che elabora --> WPGM='1'
034000970130     c* - in quello che stampa  --> WPGM='2'
034100970130     c*
034200970130     c* IN RIELABORAZIONE SALDI DA MENU'--> N31 :
034300970130     c* Pulisco la linea di partenza perche'
034400970130     c*  devo far caricare tutta la L1 anche se a filtro ho chiesto la
034500970130     c*  stampa soltanto di una
034600970130     c* IN RIELABORAZIONE SALDI DA STAMPE GIORNALIERE -> 31 :
034700970207     c* la lnp deve essere piena
034800970130     c*  perche' non e' la  linea che elabora ma la linea con cui
034900970130     c*  controlla quali filiale elaborare dalla tabella 3L
035000970130     c*  (vedi pgm fnlsc4r)
035100000000      *
035200000000     C                   Z-ADD     *ZERO         $ALMEN            1 0
035300950210     C                   Z-ADD     DS1GDA        $VIDSD            8 0
035400950210     C                   Z-ADD     DS1ADA        $CODSD            8 0
035500950210     C                   Z-ADD     DS1GAL        $VIDSA            8 0
035600950210     C                   Z-ADD     DS1AAL        $CODSA            8 0
035700000000      *
035800000000      *?    FASE  1° :    fino alla data iniziale                              ?
035900041203     c                   if        §3lela<>'N'
036000970206    1C     $DAPE         IFNE      *ZERO
036100970206     C     SIMFEL        ANDGT     0
036200970130      * Elaboro se la data DAL e' inferiore alla data prima elaborazion
036300970130      * fintanto che non diventano uguali
036400970130    2C     $DAPE         DOWGT     $COMD
036500000000     C                   SUB       1             $DAPE
036600000000     C                   Z-ADD     $DAPE         $DATA
036700000000     C                   EXSR      CONDAT
036800000000     C                   Z-ADD     $DATA         $DAPE
036900000000      *
037000950210     C                   Z-ADD     $GMA          DS1GDA
037100950210     C                   Z-ADD     $GMA          DS1GAL
037200950210     C                   Z-ADD     $DAPE         DS1ADA
037300950210     C                   Z-ADD     $DAPE         DS1AAL
037400970403     C* SE E' LA DATA RICHIESTA ELABORO ANCHE LA LISTA
037500970403     C*  SE RICHIESTA PER LA STATISTICA CONSEGNE
037600970403     C     WFPG          IFEQ      'C'
037700970403     C     DS1AAL        ANDEQ     $COMA
037800970403     C     DS1FL1        IFEQ      'L'
037900970403     C     DS1FL1        OREQ      'E'
038000970403     C                   MOVEL     'L'           DS1FL3
038100970403     C                   ENDIF
038200970403     C                   ENDIF
038300000000      *
038400950210      * Pongo nel campo STAMPA/RISTAMPA (DS1RIS) il valore *BLANKS che
038500000000      * significa "elaborazione ma senza tabutalo"
038600950210     C                   MOVE      *BLANKS       DS1RIS
038700000000      *
038800000000     C                   EXSR      TESTF
038900970130    3C     $OK           IFEQ      'S'
039000970130     C*
039100950210     C  N31              Z-ADD     *ZERO         DS1LNP
039200970130     C                   MOVEL     '1'           WPGM
039300000000     C                   EXSR      CHIAMA
039400000000     C                   Z-ADD     1             $ALMEN
039500970403     C     WFPG          IFEQ      'C'
039600970403     C                   CLEAR                   DS1FL3
039700970403     C                   ENDIF
039800000000      *
039900000000      * Mi memorizzo la data appena elaborata
040000970130     c*  perche' se i TEST della pulizia non passano, non posso
040100970130     c*  elaborare e quindi non voglio nemmeno stampare tale data
040200970130     c*  in modo che non sembri che ci sia ed invece non c'e' nella
040300970130     c*  statistica richiesta
040400950210     C                   Z-ADD     DS1GDA        $VIDSD
040500950210     C                   Z-ADD     DS1ADA        $CODSD
040600970130    4C     DS1ADA        IFGT      $CODSA
040700950210     C                   Z-ADD     DS1GDA        $VIDSA
040800950210     C                   Z-ADD     DS1ADA        $CODSA
040900970130    4C                   ENDIF
041000970130    3C                   ENDIF
041100000000      *
041200970130    2C                   ENDDO
041300000000      *
041400000000      *?    FASE  2° :    dalla data finale                                    ?
041500970130      * Elaboro se la data AL e' Maggiore della data ultima elaborazion
041600970130      * fintanto che non diventano uguali
041700970130    2C     $DAUE         DOWLT     $COMA
041800000000     C                   ADD       1             $DAUE
041900000000     C                   Z-ADD     $DAUE         $DATA
042000000000     C                   EXSR      CONDAT
042100000000     C                   Z-ADD     $DATA         $DAUE
042200000000      *
042300950210     C                   Z-ADD     $GMA          DS1GDA
042400950210     C                   Z-ADD     $GMA          DS1GAL
042500950210     C                   Z-ADD     $DAUE         DS1ADA
042600950210     C                   Z-ADD     $DAUE         DS1AAL
042700970403     C* SE E' LA DATA RICHIESTA ELABORO ANCHE LA LISTA
042800970403     C*  SE RICHIESTA
042900970403     C     WFPG          IFEQ      'C'
043000970403     C     DS1AAL        ANDEQ     $COMA
043100970403     C     DS1FL1        IFEQ      'L'
043200970403     C     DS1FL1        OREQ      'E'
043300970403     C                   MOVEL     'L'           DS1FL3
043400970403     C                   ENDIF
043500970403     C                   ENDIF
043600000000      *
043700950210      * Pongo nel campo STAMPA/RISTAMPA (DS1RIS) il valore *BLANKS che
043800000000      * significa "elaborazione ma senza tabutalo"
043900950210     C                   MOVE      *BLANKS       DS1RIS
044000980513     C                   MOVEL     'E'           WELAB             1
044100000000      *
044200000000     C                   EXSR      TESTF
044300970130    3C     $OK           IFEQ      'S'
044400950210     C  N31              Z-ADD     *ZERO         DS1LNP
044500970130     C                   MOVEL     '1'           WPGM
044600000000     C                   EXSR      CHIAMA
044700000000     C                   Z-ADD     1             $ALMEN
044800970403     C     WFPG          IFEQ      'C'
044900970403     C                   CLEAR                   DS1FL3
045000970403     C                   ENDIF
045100000000      * Mi memorizzo la data appena elaborata
045200950210     C                   Z-ADD     DS1GAL        $VIDSA
045300950210     C                   Z-ADD     DS1AAL        $CODSA
045400970130    4C     DS1AAL        IFLT      $CODSD
045500950210     C                   Z-ADD     DS1GAL        $VIDSD
045600950210     C                   Z-ADD     DS1AAL        $CODSD
045700970130    4C                   ENDIF
045800970130    3C                   ENDIF
045900000000      *
046000970130    2C                   ENDDO
046100000000      *?                                                                       ?
046200000000      *
046300041203    1C                   ENDIF
046400041203    1C                   ENDIF
046500000000      *
046600970130     C* Se e' stata richiesta l'Elaborazione e non ho ancora elaborato
046700970130     c*  niente (e' stata cioe' richiesta una data compresa nel dal/al
046800970130     C*  della Data Area) --> elaboro
046900000000    1C     $STARI        IFEQ      'S'
047000000000     C     $ALMEN        ANDEQ     *ZERO
047100970130     c*
047200970130     c* se richiesta da menu' elaboro e stampo perche' tanto
047300970130     c* non c'e' un DAL-AL ma si puo' richiedere una data soltanto
047400970130     C  N31              MOVE      $STARI        DS1RIS
047500970130     c* nelle stampe giornaliere stampo tutto dopo
047600970130     C   31              MOVEL     *BLANKS       DS1RIS
047700970130     c*
047800000000     C                   EXSR      TESTF
047900000000      *
048000000000     C     $OK           IFEQ      'S'
048100950210     C                   Z-ADD     $VIDD         DS1GDA
048200950210     C                   Z-ADD     $VIDA         DS1GAL
048300950210     C                   Z-ADD     $COMD         DS1ADA
048400950210     C                   Z-ADD     $COMA         DS1AAL
048500970130     c* azzero la lineA di partenza, perche tanto non puo' essere
048600970130     c*  piena dal momento che non si puo' richiedere una sola linea
048700970130     c*  se e' elaborazione
048800950210     C  N31              Z-ADD     *ZERO         DS1LNP
048900970130     c*
049000970130     C                   MOVEL     '1'           WPGM
049100000000     C                   EXSR      CHIAMA
049200970130     C**
049300970130     C* SOLO PER LA STATISTICA ARRIVI
049400970130     C     *IN31         IFEQ      *OFF
049500970130     C     DS1TBO        ANDEQ     'A'
049600970206     C     DS1FPG        ANDEQ     'A'
049700970130     C                   MOVEL     '2'           WPGM
049800970130     C                   EXSR      CHIAMA
049900000000     C                   ENDIF
050000970130     C                   ENDIF
050100000000      *
050200000000   X1C                   ELSE
050300000000     C* SOLO DA MENU'
050400000000     C     *IN31         IFEQ      *OFF
050500000000      *
050600000000      * Se l'utente voleva 'S' DEVO stampare il periodo DAL-AL che ho
050700000000      * determinato io
050800970303      * Per la Statistica CONSEGNE stampo sempre e solo quello richiest
050900000000     C     $STARI        IFEQ      'S'
051000970303     C     WFPG          ANDNE     'C'
051100950210     C                   Z-ADD     $VIDSD        DS1GDA
051200950210     C                   Z-ADD     $VIDSA        DS1GAL
051300950210     C                   Z-ADD     $CODSD        DS1ADA
051400950210     C                   Z-ADD     $CODSA        DS1AAL
051500950210     C                   Z-ADD     *ZERO         DS1LNP
051600000000     C                   ELSE
051700000000      * Se l'utente voleva 'R' DEVO invece stampare il periodo richiesto
051800950210     C                   Z-ADD     $VIDD         DS1GDA
051900950210     C                   Z-ADD     $VIDA         DS1GAL
052000950210     C                   Z-ADD     $COMD         DS1ADA
052100950210     C                   Z-ADD     $COMA         DS1AAL
052200970206     C                   Z-ADD     $LIN          DS1LNP
052300000000     C                   ENDIF
052400000000     C*
052500970130     C                   MOVEL     '2'           WPGM              1
052600950210     C                   MOVE      'R'           DS1RIS
052700980513     C* QUESTO FLAG MI SERVE IN STATISTICA CONSEGNE PER NON STAMPARE
052800980513     C*  "RISTAMPA" NELLA STATISTICA
052900980513     C     WFPG          IFEQ      'C'
053000980513     C                   MOVEL     WELAB         DS1FL4
053100980513     C                   ENDIF
053200970225     C* SOLO PER LA STATISTICA ARRIVI
053300970225     C     DS1TBO        IFEQ      'A'
053400970225     C     DS1FPG        ANDEQ     'A'
053500970225     C     $ALMEN        ANDEQ     1
053600970130     C     $STARI        ANDEQ     'S'
053700970130     C                   MOVE      'S'           DS1RIS
053800970130     C                   ENDIF
053900970613     C**
054000000000     C                   EXSR      LANCIO
054100970613     C**
054200970613     C* SE RISTAMPA MA IN REALTA' HO ELABORATO DEI DATI SOTTOMETTO LA
054300970613     C*  TRASMISSIONE
054400040924     C***  $STARI        IFEQ      'R'
054500040924     C***  $ALMEN        ANDGT     0
054600040924     C***                MOVEL     'S'           WTRASM            1
054700040924     C***                ENDIF
054800000000      *
054900000000     C                   ENDIF
055000000000     C                   ENDIF
055100000000      *
055200000000      * Solo se provengo da stampe giornaliere significa che sono gia' state
055300000000      * fatte tutte le elaborazioni del caso, manca quindi solo la stampa
055400000000      * del tabulato; imposto il flag come 'R'=Ristampa
055500941012      * Come fa FNLS41R (lancio elabor.giornaliere) chiamo il programma di
055600000000      * stampa due volte: una per la filiale 1° livello e la seconda per tutti
055700000000      * i 2° livelli (sempre se richiesti dalla 3L)
055800000000     C     *IN31         IFEQ      *ON
055900950210     C                   Z-ADD     $VIDSD        DS1GDA
056000950210     C                   Z-ADD     $VIDSA        DS1GAL
056100950210     C                   Z-ADD     $CODSD        DS1ADA
056200950210     C                   Z-ADD     $CODSA        DS1AAL
056300950210     C                   MOVE      'R'           DS1RIS
056400000000      *
056500000000      * Richiamo la tabella 3L
056600041203     C**                 MOVEL     '3L'          KCOD
056700041203     C**                 MOVEL     DS1PGM        KKEY
056800041203     C**                 MOVE      DS1FPG        KKEY
056900041203     C**   KTAB1         CHAIN     TABEL00F                           99
057000041203     C**N99              MOVEL     TBLUNI        DS3L
057100041203     C** 99              CLEAR                   DS3L
057200950630     C*
057300000000      * Lancio per il 1° livello
057400000000     C     §3LS1L        IFEQ      'S'
057500950210     C                   Z-ADD     0             DS1FGS
057600950210     C                   MOVEL     SIMFEL        DS1LNP
057700970130     C                   MOVEL     '2'           WPGM
057800000000     C                   EXSR      LANCIO
057900000000     C                   END
058000000000      *
058100000000      * Lancio per 2° livelli meccanizzati
058200000000     C     §3LS2L        IFEQ      'S'
058300950210     C                   Z-ADD     0             DS1FGS
058400950210     C                   MOVEL     000           DS1LNP
058500970130     C                   MOVEL     '2'           WPGM
058600000000     C                   EXSR      LANCIO
058700000000     C                   ENDIF
058800000000     C                   ENDIF
058900000000      *
059000000000     C                   ENDSR
059100000000      **************************************************************************
059200000000      * Solo per alcune statistiche devo stampare i giorni singolarmente
059300000000      **************************************************************************
059400000000     C     LANCIO        BEGSR
059500000000      *
059600970206     C     DS1TBO        IFEQ      'P'
059700970206     C     DS1FPG        ANDEQ     'E'
059800970206     C     DS1TBO        OREQ      'P'
059900970206     C     DS1FPG        ANDEQ     'T'
060000000000     C                   EXSR      SINGOL
060100000000     C                   ELSE
060200000000     C                   EXSR      CHIAMA
060300000000     C                   ENDIF
060400000000      *
060500000000     C                   ENDSR
060600000000      **************************************************************************
060700000000      * Chiamata o sottomissione di una singola statistica
060800000000      **************************************************************************
060900000000     C     CHIAMA        BEGSR
061000000000      *
061100000000      * Se sono nelle stampe giornaliere eseguo la chiamata direttamente
061200000000      * altrimenti sottometto in batch
061300950210     C                   MOVEL     DSLS01        KPJBU
061400970130     C* SOLO PER LA STATISTICA DELL'ARRIVATO FACCIO LA CALL AI PGM
061500970130    1C     DS1TBO        IFEQ      'A'
061600970206     C     DS1FPG        ANDEQ     'A'
061700970130     C* PGM CHE ELABORA
061800970130    2C     WPGM          IFEQ      '1'
061900970130     C                   CALL      $PGM1                                99
062000970130     C                   PARM                    KPJBA
062100970130   X2C                   ELSE
062200970130     C* PGM CHE STAMPA
062300970207     C                   CALL      DS1PGM                               99
062400970130     C                   PARM                    KPJBA
062500970130    2C                   ENDIF
062600970130      *
062700970130   X1C                   ELSE
062800970130     C*
062900970130     C* NELLE STATISTICHE PARTENZA E' LO STESSO PGM CHE STAMPA ED ELAB
063000970130    2C     *IN31         IFEQ      *OFF
063100000000     C                   CALL      'BCH10'
063200000000     C                   PARM                    KPJBA
063300000000      *
063400970130   X2C                   ELSE
063500970206     C                   CALL      DS1PGM                               99
063600000000     C                   PARM                    KPJBA
063700970130    2C                   ENDIF
063800970130    1C                   ENDIF
063900000000      *
064000000000     C                   ENDSR
064100000000      **************************************************************************
064200950210      * Dalla data DAL (DS1ADA) alla data AL (DS1AAL) viene lanciata una stampa
064300000000      * per ogni giorno singolo del periodo
064400000000      **************************************************************************
064500000000     C     SINGOL        BEGSR
064600000000      *
064700950210     C     DS1ADA        SUB       1             $DAL              8 0
064800950210     C                   Z-ADD     DS1AAL        $AL               8 0
064900000000      *
065000000000      * Ciclo fino a quando le due date coincidono
065100000000     C     $DAL          DOWLT     $AL
065200000000     C                   ADD       1             $DAL
065300000000     C                   Z-ADD     $DAL          $DATA
065400000000     C                   EXSR      CONDAT
065500000000     C                   Z-ADD     $DATA         $DAL
065600000000      *
065700950210     C                   Z-ADD     $GMA          DS1GDA
065800950210     C                   Z-ADD     $GMA          DS1GAL
065900950210     C                   Z-ADD     $DAL          DS1ADA
066000950210     C                   Z-ADD     $DAL          DS1AAL
066100000000      *
066200000000     C                   EXSR      CHIAMA
066300000000     C                   ENDDO
066400000000      *
066500000000     C                   ENDSR
066600000000      **************************************************************************
066700000000      * Controllo che la data abbia un giusto formato
066800000000      **************************************************************************
066900000000     C     CONDAT        BEGSR
067000000000      *
067100000000      * Prima dei controlli verifico se l'anno e' bisestile
067200000000     C                   Z-ADD     28            MES(2)
067300000000     C     AA            DIV       4             $COMOD            4 0
067400000000     C                   MVR                     $COMO1            4 0
067500000000     C     $COMO1        IFEQ      0
067600000000     C                   Z-ADD     29            MES(2)
067700000000     C                   ENDIF
067800000000      *
067900000000     C     GG            IFGT      MES(MM)
068000000000     C                   ADD       1             MM
068100000000     C                   Z-ADD     1             GG
068200950214      *
068300950214     C     MM            IFGT      12
068400950214     C                   ADD       1             AA
068500950214     C                   Z-ADD     1             MM
068600950214     C                   ENDIF
068700000000     C                   ENDIF
068800000000      *
068900000000     C     GG            IFLT      1
069000000000     C                   SUB       1             MM
069100950214      *
069200950214     C     MM            IFLT      1
069300950214     C                   SUB       1             AA
069400950214     C                   Z-ADD     12            MM
069500950214     C                   ENDIF
069600950214      *
069700000000     C                   Z-ADD     MES(MM)       GG
069800000000     C                   ENDIF
069900000000      *
070000941012      * Riempo un campo con la stessa data nel formato GG/MM/AAAA
070100941012     C                   Z-ADD     $DATA         G02INV
070200941012     C                   Z-ADD     *ZERO         G02DAT
070300941012     C                   MOVEL     '3'           G02ERR
070400941012     C                   CALL      'XSRDA8'
070500941012     C                   PARM                    WLBDAT
070600941012     C                   Z-ADD     G02DAT        $GMA              8 0
070700000000      *
070800000000     C                   ENDSR
070900000000      **************************************************************************
071000000000      * Purtroppo, e' necessario testare i giorni di pulizia di ciascun file
071100000000      * prima di eseguire l'elaborazione
071200000000      * Il metodo: viene rilevato l'ultimo giorno di utilizzo del pgm che si
071300000000      * occupa della pulizia archivio (ogni file, o quasi, ha il proprio pgm);
071400970718      * nella tabella 5A  c'e' il nr.giorni di mantenimento dei dati; si
071500000000      * sottrae quest'ultimo dal giorno ultimo utilizzo per sapere se vi sono
071600000000      * ancora disponibili i dati per la statistica.
071700000000      **************************************************************************
071800000000     C     TESTF         BEGSR
071900940517      *
072000940517      * I test sulle pulizie degli archivi devono essere fatte solo se:
072100940517      *   .  richiesta la 'S'=Stampa
072200940517      *   .  oppure data DAL e' precedente alla 1° data della data area
072300940517      * Per ogni statistica verifico la data area
072400940517     C                   MOVE      *BLANKS       $TEST             1
072500940517      *
072600970130    1C     DS1RIS        IFNE      'S'
072700970130     C*
072800970206    2C     DS1TBO        IFEQ      'P'
072900970130     C*
073000970206    3C     DS1FPG        IFEQ      'E'
073100970206     C     DS1ADA        ANDLT     C3DAPE
073200940517     C                   MOVE      'S'           $TEST
073300970206    3C                   ENDIF
073400940517      *
073500970206     C     DS1FPG        IFEQ      'B'
073600970206     C     DS1ADA        ANDLT     C5DAPE
073700940517     C                   MOVE      'S'           $TEST
073800940517     C                   ENDIF
073900940517      *
074000970206     C     DS1FPG        IFEQ      'P'
074100970206     C     DS1ADA        ANDLT     C1DAPE
074200940517     C                   MOVE      'S'           $TEST
074300940517     C                   ENDIF
074400940517      *
074500970206     C     DS1FPG        IFEQ      'T'
074600970206     C     DS1ADA        ANDLT     C2DAPE
074700970130     C                   MOVE      'S'           $TEST
074800970130     C                   ENDIF
074900970130     C*
075000970130   X2C                   ELSE
075100970130     C* STATISTICA ARRIVI
075200970206     C     DS1FPG        IFEQ      'A'
075300970206     C     DS1ADA        ANDLT     E6DAPE
075400970130     C                   MOVE      'S'           $TEST
075500970130     C                   ENDIF
075600970130     C* STATISTICA CONSEGNE
075700970206     C     DS1FPG        IFEQ      'C'
075800970130     C     DS1ADA        ANDLT     §7ACPE
075900970130     C                   MOVE      'S'           $TEST
076000970130     C                   ENDIF
076100970130     C*
076200970130    2C                   ENDIF
076300970130    1C                   ENDIF
076400940517      *
076500970206    1C     DS1RIS        IFNE      'R'
076600940517     C     $TEST         OREQ      'S'
076700000000      *
076800000000      *?   Test pulizia archivio bolle partenze (FLBLP00F, §5ABLT)   ?
076900970206    2C     DS1TBO        IFEQ      'P'
077000000000     C                   MOVE      *BLANKS       $PGM             10
077100970206     C                   MOVEL     'FNLS61R'     $PGM
077200001027     C*
077300001027     C* PRENDO GG PIÙ BASSI TRA PULIZIA BOLLE PARTOLINI E BOLLE PT
077400001027     C     §5ABLP        IFLE      §5APT1
077500970206     C                   Z-ADD     §5ABLP        $DATPU            8 0
077600001027     C                   ELSE
077700001027     C                   Z-ADD     §5APT1        $DATPU            8 0
077800001027     C                   ENDIF
077900001027     C*
078000000000     C                   EXSR      TESTF1
078100970130     C     $OK           CABEQ     'N'           ENDTES
078200970130     C*
078300970206   X2C                   ELSE
078400970130      *?   Test pulizia archivio bolle arrivo (FLARB00F, §5AARB)   ?
078500970130     C                   MOVE      *BLANKS       $PGM
078600970130     C                   MOVEL     'FNLR84R'     $PGM
078700001027     C* PRENDO GG PIÙ BASSI TRA PULIZIA BOLLE PARTOLINI E BOLLE PT
078800001027     C     §5AARB        IFLE      §5APT1
078900970130     C                   Z-ADD     §5AARB        $DATPU
079000001027     C                   ELSE
079100001027     C                   Z-ADD     §5APT5        $DATPU
079200001027     C                   ENDIF
079300001027     C*
079400970130     C                   EXSR      TESTF1
079500970130     C     $OK           CABEQ     'N'           ENDTES
079600970130      *
079700970206      *?   Test pulizia archivio fogli partenze allegati agli arrivi
079800970206    3C     DS1FPG        IFEQ      'A'
079900970130     C                   MOVE      *BLANKS       $PGM
080000970130     C                   MOVEL     'FNLRA7R'     $PGM
080100970130     C                   Z-ADD     §5AFVA        $DATPU
080200970130     C                   EXSR      TESTF1
080300970130     C     $OK           CABEQ     'N'           ENDTES
080400970130      *
080500970130      *?   Test pulizia archivio fogli vari (FNFVV00F, §5AFVV)   ?
080600970130     C                   MOVE      *BLANKS       $PGM
080700970130     C                   MOVEL     'FNLRA7R'     $PGM
080800970130     C                   Z-ADD     §5AFVV        $DATPU
080900970130     C                   EXSR      TESTF1
081000970130     C     $OK           CABEQ     'N'           ENDTES
081100970206    3C                   ENDIF
081200970206    2C                   ENDIF
081300000000      *
081400000000      * Solo per Statistica Partenze e Statistica A Terra
081500970206     C     DS1TBO        IFEQ      'P'
081600970206     C     DS1FPG        ANDEQ     'P'
081700970206     C     DS1TBO        OREQ      'P'
081800970206     C     DS1FPG        ANDEQ     'T'
081900970130     C*
082000970130     C     DS1TBO        OREQ      'A'
082100970206     C     DS1FPG        ANDEQ     'A'
082200950111      *?   Test pulizia archivio fogli viaggio (FNFGV00F, §5AFGV)   ?
082300000000     C                   MOVE      *BLANKS       $PGM
082400970206     C                   MOVEL     'FNLRA7R'     $PGM
082500970206     C                   Z-ADD     §5AFGV        $DATPU
082600000000     C                   EXSR      TESTF1
082700000000     C     $OK           CABEQ     'N'           ENDTES
082800970130     C*
082900941012      *?   Test pulizia archivio bolle transito (FNBTP00F, §5ABTP)   ?
083000020624     C     SIMFEL        SETLL     FNBTP11L
083100020624     C                   READ      FNBTP11L                               30
083200000000     C     *IN30         IFEQ      *OFF
083300000000     C                   MOVE      *BLANKS       $PGM
083400970206     C                   MOVEL     'FNLR85R'     $PGM
083500970206     C                   Z-ADD     §5ABTP        $DATPU
083600000000     C                   EXSR      TESTF1
083700000000     C     $OK           CABEQ     'N'           ENDTES
083800000000     C                   ENDIF
083900970130     C*
084000000000     C                   ENDIF
084100000000      *
084200970206      *?   Test pulizia archivio spunte arrivi (FNBRV00F, §5ABRV)   ?
084300970206      * Solo per Statistica Controllo Entrate E STSTISTICA ARRIVI
084400970206     C     DS1TBO        IFEQ      'A'
084500970206     C     DS1FPG        ANDEQ     'E'
084600970206     C*
084700970130     C     DS1TBO        OREQ      'A'
084800970206     C     DS1FPG        ANDEQ     'A'
084900000000     C                   MOVE      *BLANKS       $PGM
085000970206     C                   MOVEL     'FNLR10R'     $PGM
085100970206     C                   Z-ADD     §5ABRV        $DATPU
085200000000     C                   EXSR      TESTF1
085300000000     C     $OK           CABEQ     'N'           ENDTES
085400000000     C                   ENDIF
085500940517      *
085600940517     C                   ENDIF
085700000000      *
085800970718      *?   Test pulizia archivio statistiche partenze (FLS..00F, §5ASTP)   ?
085900000000      * SOLO SE RICHIESTA LA RISTAMPA (R)
086000950210     C     DS1RIS        IFEQ      'R'
086100970130     C*
086200970206     C     DS1TBO        IFEQ      'P'
086300000000     C                   MOVE      *BLANKS       $PGM
086400970206     C                   MOVEL     'FNLSC7R'     $PGM
086500040924     C****               Z-ADD     §5ASTP        $DATPU
086600040924     C                   Z-ADD     §5ASTS        $DATPU
086700000000     C                   EXSR      TESTF1
086800970206     C                   ELSE
086900970130     C                   MOVE      *BLANKS       $PGM
087000970130     C                   MOVEL     'FNLRE4R'     $PGM
087100040924     C*****              Z-ADD     §5ASTA        $DATPU
087200040924     C                   Z-ADD     §5ASTS        $DATPU
087300970130     C                   EXSR      TESTF1
087400970130     C                   ENDIF
087500970206     C*
087600970130     C     $OK           CABEQ     'N'           ENDTES
087700970130     C                   ENDIF
087800000000      *
087900041210     c     endtes        tag
088000090916
088100090916     c* per EDP in AS888 --> eseguo lo stesso
088200090916     c                   if        %subst(knsif:7:3)<>'201'  and
088300090916     c                             %subst(knmus:1:3)= 'EDP'
088400090916     C                   MOVE      'S'           $OK
088500090916     c                   endif
088600041210     C                   ENDSR
088700000000      **************************************************************************
088800000000      * Viene testata la data di ultimo utilizzo di un file e sottratta del
088900970718      * nr.giorni indicato nella tabella 5A
089000000000      **************************************************************************
089100000000     C     TESTF1        BEGSR
089200000000      *
089300000000     C                   CLEAR                   PRIDAT
089400000000     C                   MOVE      'S'           $OK               1
089500000000      *
089600000000      * REPERISCO DATA ULTIMO UTILIZZO PGM DI PULIZIA STATISTICHE ARRIVI
089700000000     C                   MOVE      *BLANKS       $DAT
089800000000     C                   MOVE      *BLANKS       $ERR
089900950111     C                   CALL      'TRUL49C'                            99
090000000000     C                   PARM                    $PGM
090100000103     C                   PARM                    $DAT              8
090200000000     C                   PARM                    $ERR              1
090300000000      *
090400941012      * PROGRAMMA IN USO DATA ULTIMO ULIZZO = DATA DEL GIORNO
090500970306    1C     $ERR          IFEQ      '1'
090600950111     C     *IN99         OREQ      '1'
090700040924     c     $dat          oreq      '00000000'
090800941012     C                   Z-ADD     WDATEU        G02INV
090900970306   X1C                   ELSE
091000970306    2C     $DAT          IFEQ      *BLANKS
091100970306     C                   CLEAR                   G02INV
091200970306   X2C                   ELSE
091300000000     C                   MOVE      $DAT          G02INV
091400970306    2C                   ENDIF
091500970306    1C                   ENDIF
091600000000      *
091700000000      * SOTTRAGGO ALLA DATA DI ULTIMO UTILIZZO I GIORNI CHE TIENE DALLA
091800000000      * PULIZIA E TROVO LA PRIMA DATA AL DI SOTTO DELLA QUALE NON SI
091900000000      * POSSONO LANCIARE ELABORAZIONI PER MANCANZA DI DATI
092000000000      *
092100000000      * SE LA DATA E' UGUALE A ZERO NON ESEGUO LA SOTTRAZIONE
092200000000     C     G02INV        IFNE      *ZEROS
092300000000     C                   Z-ADD     *ZEROS        G02DAT
092400000000     C                   Z-ADD     *ZEROS        G02TGI
092500000000     C                   MOVEL     '3'           G02ERR
092600941012     C                   CALL      'XSRDA8'
092700000000     C                   PARM                    WLBDAT
092800000000      *
092900000000     C                   SUB       $DATPU        G02TGI
093000000000     C                   Z-ADD     *ZEROS        GIODAT
093100000000     C                   Z-ADD     *ZEROS        GIOINV
093200000000     C                   Z-ADD     G02TGI        GIOTGI
093300941012     C                   CALL      'XSRGI8'
093400000000     C                   PARM                    WGIDAT
093500000000      *
093600941012     C                   Z-ADD     GIOINV        PRIDAT            8 0
093700000000     C                   ENDIF
093800000000      *
093900000000      * DATA DAL MAGGIORE DATA CALCOLATA
094000950210     C     DS1ADA        IFLE      PRIDAT
094100000000     C                   MOVE      'N'           $OK
094200000000     C                   ENDIF
094300090916
094400090916     c* per EDP in AS888 --> eseguo lo stesso
094500090916     c                   if        %subst(knsif:7:3)<>'201'  and
094600090916     c                             %subst(knmus:1:3)= 'EDP'
094700090916     C                   MOVE      'S'           $OK
094800090916     c                   endif
094900000000      *
095000000000     C                   ENDSR
095100950705      **************************************************************************
095200021021      * Lettura file FISTA00F per rielaborare statistiche               del
095300950705      **************************************************************************
095400950705     C     FNSTA         BEGSR
095500950705     C*
095600950712     C* SALVO CAMPI RICEVUTI
095700970303     C                   MOVEL     DSLS01        SAVDS
095800001027     C                   CLEAR                   DSSTA
095900001027     C                   CLEAR                   SAVSTA
096000950706     C*
096100021017     c                   Open      Fista01l
096200050331     c                   Open      Fista02l
096300021022     c                   If        Ds1Fpg = 'C'
096400021022     c                   Movel     $lin          Wfil
096500021022     c                   Else
096600021022     c                   Clear                   Wfil
096700021022     c                   EndIf
096800050331     c
096900050331     c                   exsr      READFNSTA
097000050401     c
097100050401     c* Se statistica consegne e ci sono p.o. esteri nella £1, cerco
097200050401     c*   i relativi record in fista, solo se chi richiede è il terminal PAR
097300050401     c                   if        ds1fpg='C' and est(1)<>0  and $lin=simfel
097400050401     c                   z-add     1             yy
097500050401     c                   dow       est(yy)>0
097600050401     c                   eval      wfil=est(yy)
097700050401     c                   exsr      READFNSTA
097800050401     c
097900050401     c                   add       1             yy
098000050401     c                   enddo
098100050401     c                   endif
098200001027     C*
098300021017     c                   Close     Fista01l
098400050331     c                   Close     Fista02l
098500950706     C*
098600970303     C                   MOVEL     SAVDS         DSLS01
098700950705     C*
098800950705     C                   ENDSR
098900050331     C*---------------------------------------------------------------*
099000050331     C* lettura file FISTA00F
099100050331     C*---------------------------------------------------------------*
099200050331     c     READFNSTA     BEGSR
099300050331     c* Se £1 contiene un p.o. poste, leggo senza il p.o.
099400050401     c* perchè i p.o. poste hanno il campo STAFIL impostato  per il partito
099500050401     c                   if        wconpt=' '   or ds1fpg<>'P'
099600050331     C     Kfista1       SETLL     FiSTA01L
099700050331     C     Kfista1       READE     FiSTA01L                             9899
099800050331     c                   else
099900050331     C     Kfista2       SETLL     FiSTA02L
100000050331     C     Kfista2       READE     FiSTA02L                             9899
100100050331     c                   endif
100200050331     c
100300050331     C                   MOVEL     DSSTA         SAVSTA
100400050331     C                   Z-ADD     0             WCONT             5 0
100500050331     C                   CLEAR                   Sav_StaNcl
100600050331     c                   Clear                   Stappt            1
100700050331     c                   Clear                   Stanoppt          1
100800050331     C*
100900050331    1C     *IN99         DOWEQ     *OFF
101000050331     C     *IN98         ANDEQ     *OFF
101100050331     c
101200050331     C* ELABORO PER ROTTURA DI DATA PERCHE' POTREI AVERE
101300050331     C*  PIU' RECORD DELLA STESSA DATA (IN CASA DI RIELABORAZIONE
101400050331     C*  CONTEMPORANEA A NUOVE SCRITTURE)
101500050331    2C     STADTS        IFNE      SAV_StaDTS
101600050331     C                   EXSR      ELASTA
101700050331     C**
101800050331     C** MI SALVO KEY PER RIPOSIZIONAMENTO
101900050331     C                   MOVEL     STADTS        WDTS
102000050331     C** DELETO I PRECEDENTI
102100050331     c                   ExSR      DELETOSTA
102200050331     C**
102300050331     C** MI RIPOSIZOINO SUL NUOVO LETTO E LO SALVO IN DS
102400050401     c                   if        wconpt=' '  or ds1fpg<>'P'
102500050331     C     Kfista1w      CHAIN     FiSTA000                           99
102600050331     c                   else
102700050331     C     Kfista2w      CHAIN     FiSTA002                           99
102800050331     c                   endif
102900050331     C**
103000050331     C                   Z-ADD     1             WCONT
103100050331     C                   MOVEL     DSSTA         SAVSTA
103200050331     c                   Clear                   Stappt            1
103300050331     c                   Clear                   Stanoppt          1
103400050331   X2C                   ELSE
103500050331     C*
103600050331     C* RECORD CON STESSA DATA LETTO 2 VOLTE
103700050331     C                   ADD       STANCL        SAV_StaNCL
103800050331     C                   ADD       1             WCONT             5 0
103900050331    2C                   ENDIF
104000050331     c
104100050331      * controllo se sto elaborando un p.o. poste
104200050331      * se non è una statistica consegne
104300050331     c                   If        Ds1Fpg <> 'C'
104400050331     c                             and Stafil > 0
104500050331     c                   Clear                   §ogntw
104600050331     c     stafil        Chain     Azorg01l
104700050331     c                   If        %Found(Azorg01l)
104800050331     c                   Movel     Orgde3        og143
104900050331     c                   If        §ogntw = 'PPT'
105000050331     c                   Eval      Stappt = 'S'
105100050331     c                   EndIf
105200050331     c                   EndIf
105300050331     c
105400050331     c* Memorizzo se il p.o. = 0 ( noposte)
105500050331     c                   else
105600050331     c                   Eval      Stanoppt = 'S'
105700050331     c                   EndIf
105800050331     C*
105900050331     C*
106000050401     c  n99              if        wconpt=' '  or ds1fpg<>'P'
106100050331     C     kfista1       READE     FiSTA01L                             9899
106200050331     c                   else
106300050331     C     kfista2       READE     FiSTA02L                             9899
106400050331     c                   endif
106500050331    1C                   ENDDO
106600050331     c
106700050331     C* ULTIMA ELABORAZIONE
106800050331     C     SAV_StaTRC    IFNE      ' '
106900050331     C                   EXSR      ELASTA
107000050331     C** DELETO L'ULTIMO
107100050331     c                   exsr      DELETOSTA
107200050331     C                   ENDIF
107300050331     c                   endsr
107400001027     C*---------------------------------------------------------------*
107500001027     C* ELABORO STATISTICA CONTENUTA IN FNSTA
107600001027     C*---------------------------------------------------------------*
107700001027     C     ELASTA        BEGSR
107800001027     C                   CLEAR                   WSIDEL
107900001027     C**
108000001027     C* DS1RIS = ' ' --> ELABORAZIONE SENZA STAMPA
108100001027     C                   MOVEL     *BLANKS       DS1RIS
108200001027     C***                  Z-ADDSTADTS    DS1ADA
108300021022     C                   Z-ADD     SAV_StaDTS    DS1ADA
108400001027     C* VEDO CHE TIPO BOLLA E'
108500021022    1C     SAV_StaTRC    IFEQ      'C'
108600021022     C     SAV_StaTRC    OREQ      'A'
108700001027     C                   MOVEL     'A'           DS1TBO
108800001027     C                   ELSE
108900001027     C                   MOVEL     'P'           DS1TBO
109000001027    1C                   ENDIF
109100001027     C**
109200001027     C                   EXSR      TESTF
109300001027      *
109400001027    1C     $OK           IFEQ      'S'
109500050331     c* Se ho sia p.o. poste che no poste, controllo i colli
109600050331     c* del p.o. poste altrimenti quello legato
109700050331     c                   clear                   contrcolli        1
109800050331     c                   if        stanoppt='S'
109900050331     c                   eval      contrcolli='N'
110000050331     c                   else
110100050331     c                   eval      contrcolli='S'
110200050331     c                   endif
110300050331     c
110400001120     C* SE SI TRATTA DI RIELABORAZIONE    POSTE, RIELABORO SOLO SE
110500001027     C*  COLLI > 100 PER STATISTICA PARTITO
110600001120     C* SE SI TRATTA DI RIELABORAZIONE NO POSTE, RIELABORO SOLO SE
110700090330     C*  COLLI >  5  PER STATISTICA PARTITO E "A TERRA"
110800021022    2C     SAV_StaTRC    IFNE      'P'
110900021022    2C     SAV_StaTRC    ANDNE     'T'
111000001122     C*
111100050331     C     contrcolli    ORne      'S'
111200090330     C     SAV_StaNCL    ANDGE     05
111300001122     C*
111400050331     C     contrcolli    OREQ      'S'
111500021022     C     SAV_StaNCL    ANDGE     100
111600001027     C                   MOVEL     'S'           WSIDEL            1
111700001027     C* Inverto data
111800021022     C                   Z-ADD     SAV_StaDTS    G02INV
111900001027     C                   Z-ADD     *ZERO         G02DAT
112000001027     C                   MOVEL     '3'           G02ERR
112100001027     C                   CALL      'XSRDA8'
112200001027     C                   PARM                    WLBDAT
112300001027     C                   Z-ADD     G02DAT        DS1GDA
112400001027     C                   Z-ADD     G02DAT        DS1GAL
112500021022     C                   Z-ADD     SAV_StaDTS    DS1ADA
112600021022     C                   Z-ADD     SAV_StaDTS    DS1AAL
112700001027     C                   MOVEL     SIMFEL        DS1LNP
112800001027     C                   MOVEL     SIMFEL        DS1FEL
112900001027     C* STATISTICA CONSEGNE
113000021022    3C     SAV_StaTRC    IFEQ      'C'
113100021022     C                   Z-ADD     SAV_StaFil    DS1LNP
113200021022     C                   Z-ADD     SAV_StaFil    DS1LNA
113300021022     C                   Z-ADD     SAV_StaFil    DS1FGS
113400001027     C* IMPOSTO IL FLAG DI ELABORAZIONE STATISTICA
113500001027     C                   MOVEL     'S'           DS1FL1
113600001027    3C                   ENDIF
113700001027     C**
113800001027     C* LANCIO  statistica per elaborazione dati
113900001027     C                   EXSR      CHIAMA
114000001027    2C                   ENDIF
114100001027   X1C                   ELSE
114200001027     C**
114300001027     C* IMPOSSIBILE RIELABORARE LA DATA --> DELETO
114400001027     C                   MOVEL     'S'           WSIDEL
114500001027    1C                   ENDIF
114600001027     C                   ENDSR
114700050331     C*---------------------------------------------------------------*
114800050331     C* cancello record di fista elaborati
114900050331     C*---------------------------------------------------------------*
115000050331     c     DELETOSTA     begsr
115100050331     C     WSIDEL        IFEQ      'S'
115200050401     c                   if        wconpt=' '  or ds1fpg<>'P'
115300050331     C     Kfista1s      CHAIN     Fista000                           99
115400050331     C     *IN99         DOWEQ     *OFF
115500050331     C                   DELETE    FiSTA000
115600050331     C     Kfista1s      READE     FiSTA000                               99
115700050331     C                   ENDDO
115800050331     c                   else
115900050331     C     Kfista2s      CHAIN     Fista002                           99
116000050331     C     *IN99         DOWEQ     *OFF
116100050331     C                   DELETE    FiSTA002
116200050331     C     Kfista2s      READE     FiSTA002                               99
116300050331     C                   ENDDO
116400050331     C                   ENDIF
116500050331     c
116600050331     C                   ENDIF
116700050331     c                   ENDSR
116800970130     C*---------------------------------------------------------------*
116900970130     C*                    CRTTAB                                     *
117000970130     C* CREO RECORD IN TABELLA DS7A
117100970130     C*---------------------------------------------------------------*
117200970130     C     CRTTAB        BEGSR
117300970130     C                   CLEAR                   DS7AF
117400970130     C                   CLEAR                   TABEL
117500970130     C                   MOVEL     *ZEROS        ALFDUE
117600970130     C                   MOVEL     *ZEROS        ALFDPE
117700970130     C                   MOVEL     DS7AF         TBLUNI
117800970206     C                   MOVEL     $LIN          TBLKEY
117900970130     C                   MOVEL     '7A'          TBLCOD
118000970130     C                   MOVEL     '1'           TBLKUT
118100970130     C                   WRITE     TABEL
118200970130     C                   ENDSR
118300001027     C*---------------------------------------------------------------*
118400001027     C* CARICO TABELLA £1 PER CONTROLLO SE C'E' P.O. POSTE
118500001027     C*---------------------------------------------------------------*
118600001027     C     CARTAB        BEGSR
118700001027      *
118800001027     C     *LIKE         DEFINE    DSLS01        SAVDS
118900001027     C     *LIKE         DEFINE    DS1GDA        $VIDD
119000001027     C     *LIKE         DEFINE    DS1GAL        $VIDA
119100001027     C     *LIKE         DEFINE    DS1ADA        $COMD
119200001027     C     *LIKE         DEFINE    DS1AAL        $COMA
119300001027     C     *LIKE         DEFINE    DS1RIS        $STARI
119400001027     C     *LIKE         DEFINE    DS1LNP        $LIN
119500001027     C     *LIKE         DEFINE    TBLCOD        KCOD
119600001027     C     *LIKE         DEFINE    TBLKEY        KKEY
119700001027     C     *LIKE         DEFINE    DS1ADA        SAVADA
119800001027     C     *LIKE         DEFINE    DS1GAL        SAVGAL
119900001027     C     *LIKE         DEFINE    DS1GDA        SAVGDA
120000001027     C     *LIKE         DEFINE    DS1AAL        SAVAAL
120100001027     C     *LIKE         DEFINE    DS1LNP        SAVLNP
120200001027     C     *LIKE         DEFINE    DS1RIS        SAVRIS
120300001027     C     *LIKE         DEFINE    DS1FGS        SAVFGS
120400001027     C     *LIKE         DEFINE    STADTS        WDTS
120500001027      *
120600001027     C     KTAB1         KLIST
120700001027     C                   KFLD                    CODUT
120800001027     C                   KFLD                    KCOD
120900001027     C                   KFLD                    KKEY
121000021017
121100050331     c     Kfista1s      Klist
121200021022     c                   Kfld                    Sav_StaTrc
121300021017     c                   Kfld                    Simfel
121400021022     c                   Kfld                    Sav_StaFil
121500021022     c                   Kfld                    Sav_StaDts
121600050331     c     Kfista2s      Klist
121700050331     c                   Kfld                    Sav_StaTrc
121800050331     c                   Kfld                    Simfel
121900050331     c                   Kfld                    Sav_StaDts
122000021022
122100021022     c     Kfista1       Klist
122200021022     c                   Kfld                    Ds1Fpg
122300021022     c                   Kfld                    Simfel
122400021022     c                   Kfld                    Wfil
122500050331
122600050331     c     Kfista2       Klist
122700050331     c                   Kfld                    Ds1Fpg
122800050331     c                   Kfld                    Simfel
122900021022
123000050331     c     Kfista1w      Klist
123100021022     c                   Kfld                    Ds1Fpg
123200021022     c                   Kfld                    Simfel
123300021022     c                   Kfld                    Wfil
123400021022     C                   KFLD                    WDTS
123500050331     c     Kfista2w      Klist
123600050331     c                   Kfld                    Ds1Fpg
123700050331     c                   Kfld                    Simfel
123800050331     C                   KFLD                    WDTS
123900001027      *
124000001027      * RILEVO LA DATA DEL GIORNO E LA INVERTO
124100001027     C                   TIME                    WTIME            14 0
124200001027     C                   MOVE      WTIME         WDATE             8 0
124300001027     C                   Z-ADD     WDATE         G02DAT
124400001027     C                   Z-ADD     *ZERO         G02INV
124500001027     C                   MOVEL     *BLANKS       G02ERR
124600001027     C                   CALL      'XSRDA8'
124700001027     C                   PARM                    WLBDAT
124800001027     C                   Z-ADD     G02INV        WDATEU            8 0
124900001027     C**
125000001027     C                   Z-ADD     1             CODUT
125100001027     C                   CALL      'X§PARUT'
125200001027     C                   PARM                    UT§DSE
125300001027     C                   MOVEL     REC80         CNCR80
125400001027      *
125500001027      * Prelevo le due tabelle contenenti i giorni di pulizia degli archivi
125600001027     C                   MOVE      '5A'          KCOD
125700001027     C                   MOVEL     *BLANKS       KKEY
125800001027     C                   MOVEL     '1'           KKEY
125900001027     C     KTAB1         CHAIN     TABEL00F                           99
126000001027     C   99              CLEAR                   DS5A
126100001027     C  N99              MOVEL     TBLUNI        DS5A
126200001027     C                   MOVE      '5A'          KCOD
126300040924     C****               MOVEL(P)  '2'           KKEY
126400040924     C                   MOVEL(P)  'SEDE1'       KKEY
126500001027     C     KTAB1         CHAIN     TABEL00F                           99
126600040924     C** 99              CLEAR                   DS5A2
126700040924     C**N99              MOVEL     TBLUNI        DS5A2
126800040924     C   99              CLEAR                   DS5AS1
126900040924     C  N99              MOVEL     TBLUNI        DS5AS1
127000001027     C***
127100001027     C* CARICO TABELLA PUNTI OPERATIVI GESTITI £1
127200001027     C***
127300001027     C                   CLEAR                   DSUL06
127400001027     C                   MOVE      '£1'          D06COD
127500001027     C                   MOVEL     SIMFEL        D06KEY
127600001027     C                   MOVEL     'L'           D06TLA
127700001027     C                   MOVEL     DSUL06        KPJBU
127800001027     C                   CALL      'TRUL06R'
127900001027     C                   PARM                    KPJBA
128000001027     C                   MOVEL     KPJBU         DSUL06
128100001027     C* CONTROLLO LIN
128200001027     C                   Z-ADD     1             XX                3 0
128300050401     C                   Z-ADD     1             yy                3 0
128400001027     C                   CLEAR                   WCONPT
128500050401     c
128600001027     C     LIN(XX)       DOWGT     0
128700001027     C     LIN(XX)       CHAIN     AZORG01L                           57
128800001027     C  N57              MOVEL     ORGDE3        OG143
128900021022     C   57              CLEAR                   §OGNTW
129000021022     C     §OGntw        IFEQ      'PPT'
129100001027     C                   MOVEL     'S'           WCONPT            1
129200001027     C                   ENDIF
129300050401     c* Carico in un'altra schiera i p.o. esteri della £1
129400050401     c                   if        §ogntw='EEX' or §ogntw='EUP' or §ogntw=
129500050401     c                             'DPD' or §ogntw='FED'
129600050401     c                   eval      est(yy)=lin(xx)
129700050401     C                   ADD       1             yy
129800050401     c                   endif
129900001027     C**
130000001027     C                   ADD       1             XX
130100001027     C                   ENDDO
130200001027     C**
130300001027      * Prelevo lA TABELLA GG PULIZIA POSTE                            hivi
130400001027     C                   MOVE      '5A'          KCOD
130500001027     C                   MOVEL(P)  'PT'          KKEY
130600001027     C     KTAB1         CHAIN     TABEL00F                           99
130700001027     C   99              CLEAR                   DS5APT
130800001027     C  N99              MOVEL     TBLUNI        DS5APT
130900001027     C**
131000001027     C** SE NON HA P.O. POSTE LA L1 IGNORO GG PULIZIA BOLLE PARTENZA
131100001027     C     WCONPT        IFNE      'S'
131200001027     C                   Z-ADD     999           §5APT1
131300001027     C                   ENDIF
131400021016      * Recupero le date delle statistiche
131500021016     c                   Clear                   Tibs02Ds
131600021016     c                   Clear                   dSdf
131700021016     c                   Eval      T02Mod = 'C'
131800021016     c                   Eval      T02Sif = Knsif
131900021016     c                   Eval      T02Cod = 'SDF'
132000021016     c                   Movel(p)  Simfel        T02Ke1
132100021016     c                   Call      'TIBS02R'
132200021016     c                   Parm                    Kpjba
132300021016     c                   Parm                    Tibs02Ds
132400021016     c                   If        T02Err = *Blanks
132500021016     c                   Movel     T02Uni        dSdf
132600021016     c                   EndIf
132700021016
132800021016     c                   Z-Add     §SdfArrP      E6daPe            8 0
132900021016     c                   Z-Add     §SdfArrU      E6daUe            8 0
133000021016     c                   Z-Add     §SdfParP      C1daPe            8 0
133100021016     c                   Z-Add     §SdfParU      C1daUe            8 0
133200021016     c                   Z-Add     §SdfTerP      C2daPe            8 0
133300021016     c                   Z-Add     §SdfTerU      C2daUe            8 0
133400021016     c                   Z-Add     §SdfEntP      C3daPe            8 0
133500021016     c                   Z-Add     §SdfEntU      C3daUe            8 0
133600021016     c                   Z-Add     §SdfBolP      C5daPe            8 0
133700021016     c                   Z-Add     §SdfBolU      C5daUe            8 0
133800021016
133900001027     C                   ENDSR
134000000000      **************************************************************************
134100000000** Numero giorni dei mesi
134200000000312931303130313130313031
