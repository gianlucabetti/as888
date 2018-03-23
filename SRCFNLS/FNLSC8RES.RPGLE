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
003600160729     f                                     rename(fista000:fista002)
003700160729     fFista03L  uf   e           k disk    Usropn
003800160729     f                                     rename(fista000:fista003)
003900000000      *
004000001027     D MES             S              2  0 DIM(12) CTDATA PERRCD(12)
004100050401     D EST             S              3  0 DIM(30)
004200000000      *
004300000810     D OG143         E DS
004400000810     D DS5A          E DS
004500040924     D DS5AS1        E DS
004600970130     D DS7AF         E DS
004700970130     D  ALFDPE                68     75
004800970130     D  ALFDUE                76     83
004900000000     D KPJBA         E DS
005000950210     D DSLS01        E DS
005100000000     D                 DS
005200941012     D  $DATA                  1      8  0
005300941012     D  AA                     1      4  0
005400941012     D  MM                     5      6  0
005500941012     D  GG                     7      8  0
005600001027     D* DS PER TRUL06R - CARICAMENTO £X
005700001027     D DSUL06        E DS                  EXTNAME(TRUL06DS)
005800001027     D  LIN                    1     90  0
005900001027     D                                     DIM(30)
006000000000     D WLBDAT          DS
006100941012     D  G02DAT                 1      8  0
006200941012     D  G02INV                 9     16  0
006300941012     D  G02ERR                17     17
006400941012     D  G02TGI                18     22  0
006500000000     D WGIDAT          DS
006600941012     D  GIODAT                 1      8  0
006700941012     D  GIOINV                 9     16  0
006800941012     D  GIOTGI                17     21  0
006900021022
007000021022     d SavSta        e ds                  extname(Fista00f)
007100021022     d                                     Prefix(Sav_)
007200021022     d DsSta         e ds                  extname(Fista00f)
007300021022
007400001027     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
007500000000     D CNCR80        E DS
007600000000     D DS3L          E DS
007700021016
007800021016     d Tibs02Ds      e ds
007900021016      * Ds date statistiche
008000021016     d dSdf          e ds
008100021017
008200021021     d Wfil            s                   Like(StaFil)
008300021016
008400000000      *
008500000000     C     *ENTRY        PLIST
008600000000     C                   PARM                    KPJBA
008700950210     C                   MOVEL     KPJBU         DSLS01
008800000000      *
008900000912     C*
009000021022     C                   CLEAR                   §OGntw
009100000912     C*
009200001027     C* CARICO  LA  £1 PER VEDERE SE HO ALMENO UN P.O. POSTE:
009300001027     C*  DEVO PRENDERE I GG DI PULIZIA PIU' BASSI TRAI 2 PER LA RIELAB
009400001027     C                   EXSR      CARTAB
009500000000      **************************************************************************
009600000000      *               M A I N     P R O G R A M
009700000000      **************************************************************************
009800940505      *
009900940505      * Se la data fine periodo e' nulla la imposto uguale a quella iniziale
010000950210     C     DS1AAL        IFEQ      *ZERO
010100950210     C                   Z-ADD     DS1ADA        DS1AAL
010200950210     C                   Z-ADD     DS1GDA        DS1GAL
010300940505     C                   ENDIF
010400000000      *
010500000000      * Memorizzo alcuni campi che mi servono
010600950210     C                   Z-ADD     DS1GDA        $VIDD
010700950210     C                   Z-ADD     DS1GAL        $VIDA
010800950210     C                   Z-ADD     DS1ADA        $COMD
010900950210     C                   Z-ADD     DS1AAL        $COMA
011000950210     C                   MOVE      DS1RIS        $STARI
011100970130     C*
011200970130     C* E' LA LINEA DI PARTENZA PER LE STATISTICHE PARTENZA
011300970130     C* E' LA LINEA DI ARRIVO PER LE STATISTICHE ARRIVO
011400970206     C                   Z-ADD     DS1LNP        $LIN
011500970206     C* MI SALVO IL TIPO STATISTICA PASSATO PERCHE' SE ' '
011600970206     C*  SONO TUTTE
011700970206     C                   MOVEL     DS1FPG        WFPG              1
011800000000      *
011900000000      * Accendo il 31 se sono lanciato dalle stampe giornaliere
012000950210     C     DS1FGS        COMP      *ZERO                                  31
012100000000      *
012200000000      * Se provengo dalle stampe giornaliere DEVO sempre rielaborare i dati
012300950210     C   31              MOVE      'S'           DS1RIS
012400000000     C   31              MOVE      'S'           $STARI
012500970130      *
012600970130     C     DS1TBO        IFEQ      'A'
012700970130     C                   EXSR      PREARR
012800970130     C                   ELSE
012900970130     C                   EXSR      PREPAR
013000970130     C                   ENDIF
013100000000      *
013200000000     C                   SETON                                        LR
013300970130     C**
013400970130      **************************************************************************
013500970130      * IMPOSTO le data aree con la data ultima elaborazione eseguita
013600970130      **************************************************************************
013700970130     C     PREPAR        BEGSR
013800970130     C**
013900970130      *
014000970130      * Problema: se 'R'=Ristampa e la data area e' vuota non stampa nulla.
014100970130      * Imposto, il primo ed ultimo giorno elaborazione, al giorno precedente
014200970130      * il periodo richiesto dall'utente (DS1ADA-DS1AAL)
014300970130     C     DS1RIS        IFEQ      'R'
014400970130     C*
014500970130     C     C1DAPE        IFEQ      *ZERO
014600970130     C     DS1ADA        SUB       1             C1DAPE
014700970130     C                   Z-ADD     C1DAPE        $DATA
014800970130     C                   EXSR      CONDAT
014900970130     C                   Z-ADD     $DATA         C1DAPE
015000970130     C                   Z-ADD     C1DAPE        C1DAUE
015100970130     C                   ENDIF
015200970130      *
015300970130     C     C2DAPE        IFEQ      *ZERO
015400970130     C     DS1ADA        SUB       1             C2DAPE
015500970130     C                   Z-ADD     C2DAPE        $DATA
015600970130     C                   EXSR      CONDAT
015700970130     C                   Z-ADD     $DATA         C2DAPE
015800970130     C                   Z-ADD     C2DAPE        C2DAUE
015900970130     C                   ENDIF
016000970130      *
016100970130     C     C3DAPE        IFEQ      *ZERO
016200970130     C     DS1ADA        SUB       1             C3DAPE
016300970130     C                   Z-ADD     C3DAPE        $DATA
016400970130     C                   EXSR      CONDAT
016500970130     C                   Z-ADD     $DATA         C3DAPE
016600970130     C                   Z-ADD     C3DAPE        C3DAUE
016700970130     C                   ENDIF
016800970130      *
016900970130     C     C5DAPE        IFEQ      *ZERO
017000970130     C     DS1ADA        SUB       1             C5DAPE
017100970130     C                   Z-ADD     C5DAPE        $DATA
017200970130     C                   EXSR      CONDAT
017300970130     C                   Z-ADD     $DATA         C5DAPE
017400970130     C                   Z-ADD     C5DAPE        C5DAUE
017500970130     C                   ENDIF
017600970130      *
017700970130     C                   ENDIF
017800970130      *
017900970130      * ?   S T A T I S T I C A    C O N T R O L L O    E N T R A T E   ?
018000970206     C     WFPG          IFEQ      *BLANKS
018100970206     C     WFPG          OREQ      'E'
018200970206     C                   MOVE      'E'           DS1FPG
018300970130     C                   MOVE      'LSC3'        KCOAZ
018400970207     C                   MOVEL     'FNLSC3R'     DS1PGM
018500970130     C                   Z-ADD     C3DAPE        $DAPE             8 0
018600970130     C                   Z-ADD     C3DAUE        $DAUE             8 0
018700970130     C                   EXSR      BCICLO
018800970130     C                   ENDIF
018900970130      *
019000970130      * ?   S T A T I S T I C A    B O L L E T T A T O   ?
019100970206     C     WFPG          IFEQ      *BLANKS
019200970206     C     WFPG          OREQ      'B'
019300970206     C                   MOVE      'B'           DS1FPG
019400970130     C                   MOVE      'LSC5'        KCOAZ
019500970207     C                   MOVEL     'FNLSC5R'     DS1PGM
019600970130     C                   Z-ADD     C5DAPE        $DAPE
019700970130     C                   Z-ADD     C5DAUE        $DAUE
019800970130     C                   EXSR      BCICLO
019900970130     C                   ENDIF
020000970130      *
020100970130      * ?   S T A T I S T I C A    P A R T E N Z E   ?
020200970206     C     WFPG          IFEQ      *BLANKS
020300970206     C     WFPG          OREQ      'P'
020400970206     C                   MOVE      'P'           DS1FPG
020500970130     C                   MOVE      'LSC1'        KCOAZ
020600970807     C                   MOVEL     'FNLSC1R'     DS1PGM
020700970130     C                   Z-ADD     C1DAPE        $DAPE
020800970130     C                   Z-ADD     C1DAUE        $DAUE
020900050331     C* Se elaborazione giornaliera o 1 lancio di data nuova di statistica
021000050331     C*    verifico se devo rielaborare dei giorni leggendo FISTA00F
021100050331     c                   if        *in31 or $daue<$coma
021200050331     C                   EXSR      FNSTA
021300050331     c                   endif
021400050331     c
021500970130     C                   EXSR      BCICLO
021600970130     C                   ENDIF
021700970130      *
021800970130      * ?   S T A T I S T I C A    A    T E R R A   ?
021900970206     C     WFPG          IFEQ      *BLANKS
022000970206     C     WFPG          OREQ      'T'
022100970206     C                   MOVE      'T'           DS1FPG
022200970130     C                   MOVE      'LSC2'        KCOAZ
022300970807     C                   MOVEL     'FNLSC2C'     DS1PGM
022400970130     C                   Z-ADD     C2DAPE        $DAPE
022500970130     C                   Z-ADD     C2DAUE        $DAUE
022600970130     C*
022700050331     C* Se elaborazione giornaliera o 1 lancio di data nuova di statistica
022800050331     C*    verifico se devo rielaborare dei giorni leggendo FISTA00F
022900050331     c                   if        *in31 or $daue<$coma
023000050331     C                   EXSR      FNSTA
023100050331     c                   endif
023200160726
023300970130     C                   EXSR      BCICLO
023400000810     C                   ENDIF
023500970130     C                   ENDSR
023600970130      **************************************************************************
023700970130      * IMPOSTO le data aree con la data ultima elaborazione eseguita
023800970130      **************************************************************************
023900970130     C     PREARR        BEGSR
024000970130     C*
024100970130      * ?   S T A T I S T I C A    A R R I V I    ?
024200970206     C     WFPG          IFEQ      *BLANKS
024300970206     C     WFPG          OREQ      'A'
024400970604     C*
024500970604    1C     DS1RIS        IFEQ      'R'
024600970604     C     E6DAPE        ANDEQ     *ZERO
024700970604     C     DS1ADA        SUB       1             E6DAPE
024800970604     C                   Z-ADD     E6DAPE        $DATA
024900970604     C                   EXSR      CONDAT
025000970604     C                   Z-ADD     $DATA         E6DAPE
025100970604     C                   Z-ADD     E6DAPE        E6DAUE
025200970604    1C                   ENDIF
025300970604     C*
025400970206     C                   MOVEL     'A'           DS1FPG
025500970130     C                   Z-ADD     E6DAPE        $DAPE
025600970130     C                   Z-ADD     E6DAUE        $DAUE
025700970207     C                   MOVEL     'FNLRE7C'     DS1PGM
025800970130     C                   MOVE      *BLANKS       $PGM1            10
025900970130     C                   MOVEL     'FNLRE6R'     $PGM1
026000970604     C* Se elaborazione giornaliera verifico se devo rielaborare
026100021021     C* dei giorni leggendo FISTA00F
026200970604     C                   MOVEL     '1'           WPGM
026300050331     C* Se elaborazione giornaliera o 1 lancio di data nuova di statistica
026400050331     C*    verifico se devo rielaborare dei giorni leggendo FISTA00F
026500050331     c                   if        *in31 or $daue<$coma
026600050331     C                   EXSR      FNSTA
026700050331     c                   endif
026800970604     C*
026900970130     C                   EXSR      BCICLO
027000970130     C                   ENDIF
027100970130     C*
027200970207      * ?   S T A T I S T I C A    C O N S E G N E    ?
027300970207     C     WFPG          IFEQ      'C'
027400970604     C* PER LA STATISTICA CONSEGNE DEVO CHAINARE LA DS7A
027500970604     C                   MOVEL     '7A'          KCOD
027600160726     C                   MOVEL(p)  $LIN          KKEY
027700970604     C     KTAB1         CHAIN     TABEL                              34
027800970604     C*
027900970604    1C     *IN34         IFEQ      *ON
028000970604     C                   EXSR      CRTTAB
028100970604    1C                   ENDIF
028200970604     C*
028300970604     C                   MOVEL     TBLUNI        DS7AF
028400970604     C     ALFDPE        IFLT      *ZERO
028500970604     C     DS1ADA        SUB       1             E6DAPE
028600970604     C                   Z-ADD     E6DAPE        $DATA
028700970604     C                   EXSR      CONDAT
028800970604     C                   MOVEL     $DATA         ALFDPE
028900970604     C                   MOVEL     $DATA         ALFDUE
029000970604     C                   ENDIF
029100970604     C*
029200970130     C                   Z-ADD     §7ACPE        $DAPE
029300970130     C                   Z-ADD     §7ACUE        $DAUE
029400970207     C                   MOVEL     'FNLR78R'     DS1PGM
029500970303     C                   MOVE      'LR78'        KCOAZ
029600970130     C* NON ESISTONO STAMPE GIORNALIERE PER LA STATISTICA CONSEGNE
029700970130     C                   EXSR      BCICLO
029800970207     C*
029900970207     C                   EXSR      FNSTA
030000970130     C                   ENDIF
030100970130     C                   ENDSR
030200000000      **************************************************************************
030300000000      * (Leggasi bi-ciclo, la routine che pedala...)
030400000000      * Partendo dalla data ultima elaborazione, viene sottomessa la statistica
030500000000      * (azione KCOAZ) tante volte quanti sono i giorni per arrivare alla data
030600950210      * fine periodo (DS1AAL)
030700000000      **************************************************************************
030800000000     C     BCICLO        BEGSR
030900980513     C                   CLEAR                   WELAB
031000041203      *
031100041203      * Richiamo la tabella 3L
031200041203     C                   MOVEL     '3L'          KCOD
031300160726     C                   MOVEL(p)  DS1PGM        KKEY
031400041203     C                   MOVE      DS1FPG        KKEY
031500041203     C     KTAB1         CHAIN     TABEL00F                           99
031600041203     C  N99              MOVEL     TBLUNI        DS3L
031700041203     C   99              CLEAR                   DS3L
031800041203     C
031900970130     c* visto che per la statistica arrivi ho 2 pgm, imposto in un camp
032000970130     c* quale deve essere lanciato:
032100970130     c* - in quello che elabora --> WPGM='1'
032200970130     c* - in quello che stampa  --> WPGM='2'
032300970130     c*
032400970130     c* IN RIELABORAZIONE SALDI DA MENU'--> N31 :
032500970130     c* Pulisco la linea di partenza perche'
032600970130     c*  devo far caricare tutta la L1 anche se a filtro ho chiesto la
032700970130     c*  stampa soltanto di una
032800970130     c* IN RIELABORAZIONE SALDI DA STAMPE GIORNALIERE -> 31 :
032900970207     c* la lnp deve essere piena
033000970130     c*  perche' non e' la  linea che elabora ma la linea con cui
033100970130     c*  controlla quali filiale elaborare dalla tabella 3L
033200970130     c*  (vedi pgm fnlsc4r)
033300000000      *
033400000000     C                   Z-ADD     *ZERO         $ALMEN            1 0
033500950210     C                   Z-ADD     DS1GDA        $VIDSD            8 0
033600950210     C                   Z-ADD     DS1ADA        $CODSD            8 0
033700950210     C                   Z-ADD     DS1GAL        $VIDSA            8 0
033800950210     C                   Z-ADD     DS1AAL        $CODSA            8 0
033900000000      *
034000000000      *?    FASE  1° :    fino alla data iniziale                              ?
034100041203     c                   if        §3lela<>'N'
034200970206    1C     $DAPE         IFNE      *ZERO
034300970206     C     SIMFEL        ANDGT     0
034400970130      * Elaboro se la data DAL e' inferiore alla data prima elaborazion
034500970130      * fintanto che non diventano uguali
034600970130    2C     $DAPE         DOWGT     $COMD
034700000000     C                   SUB       1             $DAPE
034800000000     C                   Z-ADD     $DAPE         $DATA
034900000000     C                   EXSR      CONDAT
035000000000     C                   Z-ADD     $DATA         $DAPE
035100000000      *
035200950210     C                   Z-ADD     $GMA          DS1GDA
035300950210     C                   Z-ADD     $GMA          DS1GAL
035400950210     C                   Z-ADD     $DAPE         DS1ADA
035500950210     C                   Z-ADD     $DAPE         DS1AAL
035600970403     C* SE E' LA DATA RICHIESTA ELABORO ANCHE LA LISTA
035700970403     C*  SE RICHIESTA PER LA STATISTICA CONSEGNE
035800970403     C     WFPG          IFEQ      'C'
035900970403     C     DS1AAL        ANDEQ     $COMA
036000970403     C     DS1FL1        IFEQ      'L'
036100970403     C     DS1FL1        OREQ      'E'
036200970403     C                   MOVEL     'L'           DS1FL3
036300970403     C                   ENDIF
036400970403     C                   ENDIF
036500000000      *
036600950210      * Pongo nel campo STAMPA/RISTAMPA (DS1RIS) il valore *BLANKS che
036700000000      * significa "elaborazione ma senza tabutalo"
036800950210     C                   MOVE      *BLANKS       DS1RIS
036900000000      *
037000000000     C                   EXSR      TESTF
037100970130    3C     $OK           IFEQ      'S'
037200970130     C*
037300950210     C  N31              Z-ADD     *ZERO         DS1LNP
037400970130     C                   MOVEL     '1'           WPGM
037500000000     C                   EXSR      CHIAMA
037600000000     C                   Z-ADD     1             $ALMEN
037700970403     C     WFPG          IFEQ      'C'
037800970403     C                   CLEAR                   DS1FL3
037900970403     C                   ENDIF
038000000000      *
038100000000      * Mi memorizzo la data appena elaborata
038200970130     c*  perche' se i TEST della pulizia non passano, non posso
038300970130     c*  elaborare e quindi non voglio nemmeno stampare tale data
038400970130     c*  in modo che non sembri che ci sia ed invece non c'e' nella
038500970130     c*  statistica richiesta
038600950210     C                   Z-ADD     DS1GDA        $VIDSD
038700950210     C                   Z-ADD     DS1ADA        $CODSD
038800970130    4C     DS1ADA        IFGT      $CODSA
038900950210     C                   Z-ADD     DS1GDA        $VIDSA
039000950210     C                   Z-ADD     DS1ADA        $CODSA
039100970130    4C                   ENDIF
039200970130    3C                   ENDIF
039300000000      *
039400970130    2C                   ENDDO
039500000000      *
039600000000      *?    FASE  2° :    dalla data finale                                    ?
039700970130      * Elaboro se la data AL e' Maggiore della data ultima elaborazion
039800970130      * fintanto che non diventano uguali
039900970130    2C     $DAUE         DOWLT     $COMA
040000000000     C                   ADD       1             $DAUE
040100000000     C                   Z-ADD     $DAUE         $DATA
040200000000     C                   EXSR      CONDAT
040300000000     C                   Z-ADD     $DATA         $DAUE
040400000000      *
040500950210     C                   Z-ADD     $GMA          DS1GDA
040600950210     C                   Z-ADD     $GMA          DS1GAL
040700950210     C                   Z-ADD     $DAUE         DS1ADA
040800950210     C                   Z-ADD     $DAUE         DS1AAL
040900970403     C* SE E' LA DATA RICHIESTA ELABORO ANCHE LA LISTA
041000970403     C*  SE RICHIESTA
041100970403     C     WFPG          IFEQ      'C'
041200970403     C     DS1AAL        ANDEQ     $COMA
041300970403     C     DS1FL1        IFEQ      'L'
041400970403     C     DS1FL1        OREQ      'E'
041500970403     C                   MOVEL     'L'           DS1FL3
041600970403     C                   ENDIF
041700970403     C                   ENDIF
041800000000      *
041900950210      * Pongo nel campo STAMPA/RISTAMPA (DS1RIS) il valore *BLANKS che
042000000000      * significa "elaborazione ma senza tabutalo"
042100950210     C                   MOVE      *BLANKS       DS1RIS
042200980513     C                   MOVEL     'E'           WELAB             1
042300000000      *
042400000000     C                   EXSR      TESTF
042500970130    3C     $OK           IFEQ      'S'
042600950210     C  N31              Z-ADD     *ZERO         DS1LNP
042700970130     C                   MOVEL     '1'           WPGM
042800000000     C                   EXSR      CHIAMA
042900000000     C                   Z-ADD     1             $ALMEN
043000970403     C     WFPG          IFEQ      'C'
043100970403     C                   CLEAR                   DS1FL3
043200970403     C                   ENDIF
043300000000      * Mi memorizzo la data appena elaborata
043400950210     C                   Z-ADD     DS1GAL        $VIDSA
043500950210     C                   Z-ADD     DS1AAL        $CODSA
043600970130    4C     DS1AAL        IFLT      $CODSD
043700950210     C                   Z-ADD     DS1GAL        $VIDSD
043800950210     C                   Z-ADD     DS1AAL        $CODSD
043900970130    4C                   ENDIF
044000970130    3C                   ENDIF
044100000000      *
044200970130    2C                   ENDDO
044300000000      *?                                                                       ?
044400000000      *
044500041203    1C                   ENDIF
044600041203    1C                   ENDIF
044700000000      *
044800970130     C* Se e' stata richiesta l'Elaborazione e non ho ancora elaborato
044900970130     c*  niente (e' stata cioe' richiesta una data compresa nel dal/al
045000970130     C*  della Data Area) --> elaboro
045100000000    1C     $STARI        IFEQ      'S'
045200000000     C     $ALMEN        ANDEQ     *ZERO
045300970130     c*
045400970130     c* se richiesta da menu' elaboro e stampo perche' tanto
045500970130     c* non c'e' un DAL-AL ma si puo' richiedere una data soltanto
045600970130     C  N31              MOVE      $STARI        DS1RIS
045700970130     c* nelle stampe giornaliere stampo tutto dopo
045800970130     C   31              MOVEL     *BLANKS       DS1RIS
045900970130     c*
046000000000     C                   EXSR      TESTF
046100000000      *
046200000000     C     $OK           IFEQ      'S'
046300950210     C                   Z-ADD     $VIDD         DS1GDA
046400950210     C                   Z-ADD     $VIDA         DS1GAL
046500950210     C                   Z-ADD     $COMD         DS1ADA
046600950210     C                   Z-ADD     $COMA         DS1AAL
046700970130     c* azzero la lineA di partenza, perche tanto non puo' essere
046800970130     c*  piena dal momento che non si puo' richiedere una sola linea
046900970130     c*  se e' elaborazione
047000950210     C  N31              Z-ADD     *ZERO         DS1LNP
047100970130     c*
047200970130     C                   MOVEL     '1'           WPGM
047300000000     C                   EXSR      CHIAMA
047400970130     C**
047500970130     C* SOLO PER LA STATISTICA ARRIVI
047600970130     C     *IN31         IFEQ      *OFF
047700970130     C     DS1TBO        ANDEQ     'A'
047800970206     C     DS1FPG        ANDEQ     'A'
047900970130     C                   MOVEL     '2'           WPGM
048000970130     C                   EXSR      CHIAMA
048100000000     C                   ENDIF
048200970130     C                   ENDIF
048300000000      *
048400000000   X1C                   ELSE
048500000000     C* SOLO DA MENU'
048600000000     C     *IN31         IFEQ      *OFF
048700000000      *
048800000000      * Se l'utente voleva 'S' DEVO stampare il periodo DAL-AL che ho
048900000000      * determinato io
049000970303      * Per la Statistica CONSEGNE stampo sempre e solo quello richiest
049100000000     C     $STARI        IFEQ      'S'
049200970303     C     WFPG          ANDNE     'C'
049300950210     C                   Z-ADD     $VIDSD        DS1GDA
049400950210     C                   Z-ADD     $VIDSA        DS1GAL
049500950210     C                   Z-ADD     $CODSD        DS1ADA
049600950210     C                   Z-ADD     $CODSA        DS1AAL
049700950210     C                   Z-ADD     *ZERO         DS1LNP
049800000000     C                   ELSE
049900000000      * Se l'utente voleva 'R' DEVO invece stampare il periodo richiesto
050000950210     C                   Z-ADD     $VIDD         DS1GDA
050100950210     C                   Z-ADD     $VIDA         DS1GAL
050200950210     C                   Z-ADD     $COMD         DS1ADA
050300950210     C                   Z-ADD     $COMA         DS1AAL
050400970206     C                   Z-ADD     $LIN          DS1LNP
050500000000     C                   ENDIF
050600000000     C*
050700970130     C                   MOVEL     '2'           WPGM              1
050800950210     C                   MOVE      'R'           DS1RIS
050900980513     C* QUESTO FLAG MI SERVE IN STATISTICA CONSEGNE PER NON STAMPARE
051000980513     C*  "RISTAMPA" NELLA STATISTICA
051100980513     C     WFPG          IFEQ      'C'
051200980513     C                   MOVEL     WELAB         DS1FL4
051300980513     C                   ENDIF
051400970225     C* SOLO PER LA STATISTICA ARRIVI
051500970225     C     DS1TBO        IFEQ      'A'
051600970225     C     DS1FPG        ANDEQ     'A'
051700970225     C     $ALMEN        ANDEQ     1
051800970130     C     $STARI        ANDEQ     'S'
051900970130     C                   MOVE      'S'           DS1RIS
052000970130     C                   ENDIF
052100970613     C**
052200000000     C                   EXSR      LANCIO
052300970613     C**
052400970613     C* SE RISTAMPA MA IN REALTA' HO ELABORATO DEI DATI SOTTOMETTO LA
052500970613     C*  TRASMISSIONE
052600040924     C***  $STARI        IFEQ      'R'
052700040924     C***  $ALMEN        ANDGT     0
052800040924     C***                MOVEL     'S'           WTRASM            1
052900040924     C***                ENDIF
053000000000      *
053100000000     C                   ENDIF
053200000000     C                   ENDIF
053300000000      *
053400000000      * Solo se provengo da stampe giornaliere significa che sono gia' state
053500000000      * fatte tutte le elaborazioni del caso, manca quindi solo la stampa
053600000000      * del tabulato; imposto il flag come 'R'=Ristampa
053700941012      * Come fa FNLS41R (lancio elabor.giornaliere) chiamo il programma di
053800000000      * stampa due volte: una per la filiale 1° livello e la seconda per tutti
053900000000      * i 2° livelli (sempre se richiesti dalla 3L)
054000000000     C     *IN31         IFEQ      *ON
054100950210     C                   Z-ADD     $VIDSD        DS1GDA
054200950210     C                   Z-ADD     $VIDSA        DS1GAL
054300950210     C                   Z-ADD     $CODSD        DS1ADA
054400950210     C                   Z-ADD     $CODSA        DS1AAL
054500950210     C                   MOVE      'R'           DS1RIS
054600000000      *
054700000000      * Richiamo la tabella 3L
054800041203     C**                 MOVEL     '3L'          KCOD
054900041203     C**                 MOVEL     DS1PGM        KKEY
055000041203     C**                 MOVE      DS1FPG        KKEY
055100041203     C**   KTAB1         CHAIN     TABEL00F                           99
055200041203     C**N99              MOVEL     TBLUNI        DS3L
055300041203     C** 99              CLEAR                   DS3L
055400950630     C*
055500000000      * Lancio per il 1° livello
055600000000     C     §3LS1L        IFEQ      'S'
055700950210     C                   Z-ADD     0             DS1FGS
055800950210     C                   MOVEL     SIMFEL        DS1LNP
055900970130     C                   MOVEL     '2'           WPGM
056000000000     C                   EXSR      LANCIO
056100000000     C                   END
056200000000      *
056300000000      * Lancio per 2° livelli meccanizzati
056400000000     C     §3LS2L        IFEQ      'S'
056500950210     C                   Z-ADD     0             DS1FGS
056600950210     C                   MOVEL     000           DS1LNP
056700970130     C                   MOVEL     '2'           WPGM
056800000000     C                   EXSR      LANCIO
056900000000     C                   ENDIF
057000000000     C                   ENDIF
057100000000      *
057200000000     C                   ENDSR
057300000000      **************************************************************************
057400000000      * Solo per alcune statistiche devo stampare i giorni singolarmente
057500000000      **************************************************************************
057600000000     C     LANCIO        BEGSR
057700000000      *
057800970206     C     DS1TBO        IFEQ      'P'
057900970206     C     DS1FPG        ANDEQ     'E'
058000970206     C     DS1TBO        OREQ      'P'
058100970206     C     DS1FPG        ANDEQ     'T'
058200000000     C                   EXSR      SINGOL
058300000000     C                   ELSE
058400000000     C                   EXSR      CHIAMA
058500000000     C                   ENDIF
058600000000      *
058700000000     C                   ENDSR
058800000000      **************************************************************************
058900000000      * Chiamata o sottomissione di una singola statistica
059000000000      **************************************************************************
059100000000     C     CHIAMA        BEGSR
059200000000      *
059300000000      * Se sono nelle stampe giornaliere eseguo la chiamata direttamente
059400000000      * altrimenti sottometto in batch
059500950210     C                   MOVEL     DSLS01        KPJBU
059600970130     C* SOLO PER LA STATISTICA DELL'ARRIVATO FACCIO LA CALL AI PGM
059700970130    1C     DS1TBO        IFEQ      'A'
059800970206     C     DS1FPG        ANDEQ     'A'
059900970130     C* PGM CHE ELABORA
060000970130    2C     WPGM          IFEQ      '1'
060100970130     C                   CALL      $PGM1                                99
060200970130     C                   PARM                    KPJBA
060300970130   X2C                   ELSE
060400970130     C* PGM CHE STAMPA
060500970207     C                   CALL      DS1PGM                               99
060600970130     C                   PARM                    KPJBA
060700970130    2C                   ENDIF
060800970130      *
060900970130   X1C                   ELSE
061000970130     C*
061100970130     C* NELLE STATISTICHE PARTENZA E' LO STESSO PGM CHE STAMPA ED ELAB
061200970130    2C     *IN31         IFEQ      *OFF
061300000000     C                   CALL      'BCH10'
061400000000     C                   PARM                    KPJBA
061500000000      *
061600970130   X2C                   ELSE
061700970206     C                   CALL      DS1PGM                               99
061800000000     C                   PARM                    KPJBA
061900970130    2C                   ENDIF
062000970130    1C                   ENDIF
062100000000      *
062200000000     C                   ENDSR
062300000000      **************************************************************************
062400950210      * Dalla data DAL (DS1ADA) alla data AL (DS1AAL) viene lanciata una stampa
062500000000      * per ogni giorno singolo del periodo
062600000000      **************************************************************************
062700000000     C     SINGOL        BEGSR
062800000000      *
062900950210     C     DS1ADA        SUB       1             $DAL              8 0
063000950210     C                   Z-ADD     DS1AAL        $AL               8 0
063100000000      *
063200000000      * Ciclo fino a quando le due date coincidono
063300000000     C     $DAL          DOWLT     $AL
063400000000     C                   ADD       1             $DAL
063500000000     C                   Z-ADD     $DAL          $DATA
063600000000     C                   EXSR      CONDAT
063700000000     C                   Z-ADD     $DATA         $DAL
063800000000      *
063900950210     C                   Z-ADD     $GMA          DS1GDA
064000950210     C                   Z-ADD     $GMA          DS1GAL
064100950210     C                   Z-ADD     $DAL          DS1ADA
064200950210     C                   Z-ADD     $DAL          DS1AAL
064300000000      *
064400000000     C                   EXSR      CHIAMA
064500000000     C                   ENDDO
064600000000      *
064700000000     C                   ENDSR
064800000000      **************************************************************************
064900000000      * Controllo che la data abbia un giusto formato
065000000000      **************************************************************************
065100000000     C     CONDAT        BEGSR
065200000000      *
065300000000      * Prima dei controlli verifico se l'anno e' bisestile
065400000000     C                   Z-ADD     28            MES(2)
065500000000     C     AA            DIV       4             $COMOD            4 0
065600000000     C                   MVR                     $COMO1            4 0
065700000000     C     $COMO1        IFEQ      0
065800000000     C                   Z-ADD     29            MES(2)
065900000000     C                   ENDIF
066000000000      *
066100000000     C     GG            IFGT      MES(MM)
066200000000     C                   ADD       1             MM
066300000000     C                   Z-ADD     1             GG
066400950214      *
066500950214     C     MM            IFGT      12
066600950214     C                   ADD       1             AA
066700950214     C                   Z-ADD     1             MM
066800950214     C                   ENDIF
066900000000     C                   ENDIF
067000000000      *
067100000000     C     GG            IFLT      1
067200000000     C                   SUB       1             MM
067300950214      *
067400950214     C     MM            IFLT      1
067500950214     C                   SUB       1             AA
067600950214     C                   Z-ADD     12            MM
067700950214     C                   ENDIF
067800950214      *
067900000000     C                   Z-ADD     MES(MM)       GG
068000000000     C                   ENDIF
068100000000      *
068200941012      * Riempo un campo con la stessa data nel formato GG/MM/AAAA
068300941012     C                   Z-ADD     $DATA         G02INV
068400941012     C                   Z-ADD     *ZERO         G02DAT
068500941012     C                   MOVEL     '3'           G02ERR
068600941012     C                   CALL      'XSRDA8'
068700941012     C                   PARM                    WLBDAT
068800941012     C                   Z-ADD     G02DAT        $GMA              8 0
068900000000      *
069000000000     C                   ENDSR
069100000000      **************************************************************************
069200000000      * Purtroppo, e' necessario testare i giorni di pulizia di ciascun file
069300000000      * prima di eseguire l'elaborazione
069400000000      * Il metodo: viene rilevato l'ultimo giorno di utilizzo del pgm che si
069500000000      * occupa della pulizia archivio (ogni file, o quasi, ha il proprio pgm);
069600970718      * nella tabella 5A  c'e' il nr.giorni di mantenimento dei dati; si
069700000000      * sottrae quest'ultimo dal giorno ultimo utilizzo per sapere se vi sono
069800000000      * ancora disponibili i dati per la statistica.
069900000000      **************************************************************************
070000000000     C     TESTF         BEGSR
070100940517      *
070200940517      * I test sulle pulizie degli archivi devono essere fatte solo se:
070300940517      *   .  richiesta la 'S'=Stampa
070400940517      *   .  oppure data DAL e' precedente alla 1° data della data area
070500940517      * Per ogni statistica verifico la data area
070600940517     C                   MOVE      *BLANKS       $TEST             1
070700940517      *
070800970130    1C     DS1RIS        IFNE      'S'
070900970130     C*
071000970206    2C     DS1TBO        IFEQ      'P'
071100970130     C*
071200970206    3C     DS1FPG        IFEQ      'E'
071300970206     C     DS1ADA        ANDLT     C3DAPE
071400940517     C                   MOVE      'S'           $TEST
071500970206    3C                   ENDIF
071600940517      *
071700970206     C     DS1FPG        IFEQ      'B'
071800970206     C     DS1ADA        ANDLT     C5DAPE
071900940517     C                   MOVE      'S'           $TEST
072000940517     C                   ENDIF
072100940517      *
072200970206     C     DS1FPG        IFEQ      'P'
072300970206     C     DS1ADA        ANDLT     C1DAPE
072400940517     C                   MOVE      'S'           $TEST
072500940517     C                   ENDIF
072600940517      *
072700970206     C     DS1FPG        IFEQ      'T'
072800970206     C     DS1ADA        ANDLT     C2DAPE
072900970130     C                   MOVE      'S'           $TEST
073000970130     C                   ENDIF
073100970130     C*
073200970130   X2C                   ELSE
073300970130     C* STATISTICA ARRIVI
073400970206     C     DS1FPG        IFEQ      'A'
073500970206     C     DS1ADA        ANDLT     E6DAPE
073600970130     C                   MOVE      'S'           $TEST
073700970130     C                   ENDIF
073800970130     C* STATISTICA CONSEGNE
073900970206     C     DS1FPG        IFEQ      'C'
074000970130     C     DS1ADA        ANDLT     §7ACPE
074100970130     C                   MOVE      'S'           $TEST
074200970130     C                   ENDIF
074300970130     C*
074400970130    2C                   ENDIF
074500970130    1C                   ENDIF
074600940517      *
074700970206    1C     DS1RIS        IFNE      'R'
074800940517     C     $TEST         OREQ      'S'
074900000000      *
075000000000      *?   Test pulizia archivio bolle partenze (FLBLP00F, §5ABLT)   ?
075100970206    2C     DS1TBO        IFEQ      'P'
075200000000     C                   MOVE      *BLANKS       $PGM             10
075300970206     C                   MOVEL     'FNLS61R'     $PGM
075400001027     C*
075500970206     C                   Z-ADD     §5ABLP        $DATPU            8 0
075600001027     C*
075700000000     C                   EXSR      TESTF1
075800970130     C     $OK           CABEQ     'N'           ENDTES
075900970130     C*
076000970206   X2C                   ELSE
076100970130      *?   Test pulizia archivio bolle arrivo (FLARB00F, §5AARB)   ?
076200970130     C                   MOVE      *BLANKS       $PGM
076300970130     C                   MOVEL     'FNLR84R'     $PGM
076400970130     C                   Z-ADD     §5AARB        $DATPU
076500001027     C*
076600970130     C                   EXSR      TESTF1
076700970130     C     $OK           CABEQ     'N'           ENDTES
076800970130      *
076900970206      *?   Test pulizia archivio fogli partenze allegati agli arrivi
077000970206    3C     DS1FPG        IFEQ      'A'
077100970130     C                   MOVE      *BLANKS       $PGM
077200970130     C                   MOVEL     'FNLRA7R'     $PGM
077300970130     C                   Z-ADD     §5AFVA        $DATPU
077400970130     C                   EXSR      TESTF1
077500970130     C     $OK           CABEQ     'N'           ENDTES
077600970130      *
077700970130      *?   Test pulizia archivio fogli vari (FNFVV00F, §5AFVV)   ?
077800970130     C                   MOVE      *BLANKS       $PGM
077900970130     C                   MOVEL     'FNLRA7R'     $PGM
078000970130     C                   Z-ADD     §5AFVV        $DATPU
078100970130     C                   EXSR      TESTF1
078200970130     C     $OK           CABEQ     'N'           ENDTES
078300970206    3C                   ENDIF
078400970206    2C                   ENDIF
078500000000      *
078600000000      * Solo per Statistica Partenze e Statistica A Terra
078700970206     C     DS1TBO        IFEQ      'P'
078800970206     C     DS1FPG        ANDEQ     'P'
078900970206     C     DS1TBO        OREQ      'P'
079000970206     C     DS1FPG        ANDEQ     'T'
079100970130     C*
079200970130     C     DS1TBO        OREQ      'A'
079300970206     C     DS1FPG        ANDEQ     'A'
079400950111      *?   Test pulizia archivio fogli viaggio (FNFGV00F, §5AFGV)   ?
079500000000     C                   MOVE      *BLANKS       $PGM
079600970206     C                   MOVEL     'FNLRA7R'     $PGM
079700970206     C                   Z-ADD     §5AFGV        $DATPU
079800000000     C                   EXSR      TESTF1
079900000000     C     $OK           CABEQ     'N'           ENDTES
080000970130     C*
080100941012      *?   Test pulizia archivio bolle transito (FNBTP00F, §5ABTP)   ?
080200020624     C     SIMFEL        SETLL     FNBTP11L
080300020624     C                   READ      FNBTP11L                               30
080400000000     C     *IN30         IFEQ      *OFF
080500000000     C                   MOVE      *BLANKS       $PGM
080600970206     C                   MOVEL     'FNLR85R'     $PGM
080700970206     C                   Z-ADD     §5ABTP        $DATPU
080800000000     C                   EXSR      TESTF1
080900000000     C     $OK           CABEQ     'N'           ENDTES
081000000000     C                   ENDIF
081100970130     C*
081200000000     C                   ENDIF
081300000000      *
081400970206      *?   Test pulizia archivio spunte arrivi (FNBRV00F, §5ABRV)   ?
081500970206      * Solo per Statistica Controllo Entrate E STSTISTICA ARRIVI
081600970206     C     DS1TBO        IFEQ      'A'
081700970206     C     DS1FPG        ANDEQ     'E'
081800970206     C*
081900970130     C     DS1TBO        OREQ      'A'
082000970206     C     DS1FPG        ANDEQ     'A'
082100000000     C                   MOVE      *BLANKS       $PGM
082200970206     C                   MOVEL     'FNLR10R'     $PGM
082300970206     C                   Z-ADD     §5ABRV        $DATPU
082400000000     C                   EXSR      TESTF1
082500000000     C     $OK           CABEQ     'N'           ENDTES
082600000000     C                   ENDIF
082700940517      *
082800940517     C                   ENDIF
082900000000      *
083000970718      *?   Test pulizia archivio statistiche partenze (FLS..00F, §5ASTP)   ?
083100000000      * SOLO SE RICHIESTA LA RISTAMPA (R)
083200950210     C     DS1RIS        IFEQ      'R'
083300970130     C*
083400970206     C     DS1TBO        IFEQ      'P'
083500000000     C                   MOVE      *BLANKS       $PGM
083600970206     C                   MOVEL     'FNLSC7R'     $PGM
083700040924     C****               Z-ADD     §5ASTP        $DATPU
083800040924     C                   Z-ADD     §5ASTS        $DATPU
083900000000     C                   EXSR      TESTF1
084000970206     C                   ELSE
084100970130     C                   MOVE      *BLANKS       $PGM
084200970130     C                   MOVEL     'FNLRE4R'     $PGM
084300040924     C*****              Z-ADD     §5ASTA        $DATPU
084400040924     C                   Z-ADD     §5ASTS        $DATPU
084500970130     C                   EXSR      TESTF1
084600970130     C                   ENDIF
084700970206     C*
084800970130     C     $OK           CABEQ     'N'           ENDTES
084900970130     C                   ENDIF
085000000000      *
085100041210     c     endtes        tag
085200090916
085300090916     c* per EDP in AS888 --> eseguo lo stesso
085400090916     c                   if        %subst(knsif:7:3)<>'201'  and
085500090916     c                             %subst(knmus:1:3)= 'EDP'
085600090916     C                   MOVE      'S'           $OK
085700090916     c                   endif
085800041210     C                   ENDSR
085900000000      **************************************************************************
086000000000      * Viene testata la data di ultimo utilizzo di un file e sottratta del
086100970718      * nr.giorni indicato nella tabella 5A
086200000000      **************************************************************************
086300000000     C     TESTF1        BEGSR
086400000000      *
086500000000     C                   CLEAR                   PRIDAT
086600000000     C                   MOVE      'S'           $OK               1
086700000000      *
086800000000      * REPERISCO DATA ULTIMO UTILIZZO PGM DI PULIZIA STATISTICHE ARRIVI
086900000000     C                   MOVE      *BLANKS       $DAT
087000000000     C                   MOVE      *BLANKS       $ERR
087100950111     C                   CALL      'TRUL49C'                            99
087200000000     C                   PARM                    $PGM
087300000103     C                   PARM                    $DAT              8
087400000000     C                   PARM                    $ERR              1
087500000000      *
087600941012      * PROGRAMMA IN USO DATA ULTIMO ULIZZO = DATA DEL GIORNO
087700970306    1C     $ERR          IFEQ      '1'
087800950111     C     *IN99         OREQ      '1'
087900040924     c     $dat          oreq      '00000000'
088000941012     C                   Z-ADD     WDATEU        G02INV
088100970306   X1C                   ELSE
088200970306    2C     $DAT          IFEQ      *BLANKS
088300970306     C                   CLEAR                   G02INV
088400970306   X2C                   ELSE
088500000000     C                   MOVE      $DAT          G02INV
088600970306    2C                   ENDIF
088700970306    1C                   ENDIF
088800000000      *
088900000000      * SOTTRAGGO ALLA DATA DI ULTIMO UTILIZZO I GIORNI CHE TIENE DALLA
089000000000      * PULIZIA E TROVO LA PRIMA DATA AL DI SOTTO DELLA QUALE NON SI
089100000000      * POSSONO LANCIARE ELABORAZIONI PER MANCANZA DI DATI
089200000000      *
089300000000      * SE LA DATA E' UGUALE A ZERO NON ESEGUO LA SOTTRAZIONE
089400000000     C     G02INV        IFNE      *ZEROS
089500000000     C                   Z-ADD     *ZEROS        G02DAT
089600000000     C                   Z-ADD     *ZEROS        G02TGI
089700000000     C                   MOVEL     '3'           G02ERR
089800941012     C                   CALL      'XSRDA8'
089900000000     C                   PARM                    WLBDAT
090000000000      *
090100000000     C                   SUB       $DATPU        G02TGI
090200000000     C                   Z-ADD     *ZEROS        GIODAT
090300000000     C                   Z-ADD     *ZEROS        GIOINV
090400000000     C                   Z-ADD     G02TGI        GIOTGI
090500941012     C                   CALL      'XSRGI8'
090600000000     C                   PARM                    WGIDAT
090700000000      *
090800941012     C                   Z-ADD     GIOINV        PRIDAT            8 0
090900000000     C                   ENDIF
091000000000      *
091100000000      * DATA DAL MAGGIORE DATA CALCOLATA
091200950210     C     DS1ADA        IFLE      PRIDAT
091300000000     C                   MOVE      'N'           $OK
091400000000     C                   ENDIF
091500090916
091600090916     c* per EDP in AS888 --> eseguo lo stesso
091700090916     c                   if        %subst(knsif:7:3)<>'201'  and
091800090916     c                             %subst(knmus:1:3)= 'EDP'
091900090916     C                   MOVE      'S'           $OK
092000090916     c                   endif
092100000000      *
092200000000     C                   ENDSR
092300950705      **************************************************************************
092400021021      * Lettura file FISTA00F per rielaborare statistiche               del
092500950705      **************************************************************************
092600950705     C     FNSTA         BEGSR
092700950705     C*
092800950712     C* SALVO CAMPI RICEVUTI
092900970303     C                   MOVEL     DSLS01        SAVDS
093000001027     C                   CLEAR                   DSSTA
093100001027     C                   CLEAR                   SAVSTA
093200950706     C*
093300021017     c                   Open      Fista01l
093400050331     c                   Open      Fista02l
093500160729     c                   Open      Fista03l
093600021022     c                   If        Ds1Fpg = 'C'
093700021022     c                   Movel     $lin          Wfil
093800021022     c                   Else
093900021022     c                   Clear                   Wfil
094000021022     c                   EndIf
094100050331     c
094200050331     c                   exsr      READFNSTA
094300050401     c
094400050401     c* Se statistica consegne e ci sono p.o. esteri nella £1, cerco
094500050401     c*   i relativi record in fista, solo se chi richiede è il terminal PAR
094600050401     c                   if        ds1fpg='C' and est(1)<>0  and $lin=simfel
094700050401     c                   z-add     1             yy
094800050401     c                   dow       est(yy)>0
094900050401     c                   eval      wfil=est(yy)
095000050401     c                   exsr      READFNSTA
095100050401     c
095200050401     c                   add       1             yy
095300050401     c                   enddo
095400050401     c                   endif
095500001027     C*
095600021017     c                   Close     Fista01l
095700050331     c                   Close     Fista02l
095800160729     c                   Close     Fista03l
095900950706     C*
096000970303     C                   MOVEL     SAVDS         DSLS01
096100950705     C*
096200950705     C                   ENDSR
096300050331     C*---------------------------------------------------------------*
096400050331     C* lettura file FISTA00F
096500050331     C*---------------------------------------------------------------*
096600050331     c     READFNSTA     BEGSR
096700050331     c* Se £1 contiene un p.o. poste, leggo senza il p.o.
096800050401     c* perchè i p.o. poste hanno il campo STAFIL impostato  per il partito
096900160729     c                   select
097000160729     c                   when      ds1fpg= 'A'
097100050331     C     Kfista1       SETLL     FiSTA01L
097200050331     C     Kfista1       READE     FiSTA01L                             9899
097300160729     c                   when      ds1fpg= 'C'
097400160729     C     Kfista3       SETLL     FiSTA03L
097500160729     C     Kfista3       READE     FiSTA03L                             9899
097600160729     c                   other
097700050331     C     Kfista2       SETLL     FiSTA02L
097800050331     C     Kfista2       READE     FiSTA02L                             9899
097900160729     c                   endsl
098000050331     c
098100050331     C                   MOVEL     DSSTA         SAVSTA
098200050331     C                   Z-ADD     0             WCONT             5 0
098300050331     C                   CLEAR                   Sav_StaNcl
098400050331     C*
098500050331    1C     *IN99         DOWEQ     *OFF
098600050331     C     *IN98         ANDEQ     *OFF
098700050331     c
098800050331     C* ELABORO PER ROTTURA DI DATA PERCHE' POTREI AVERE
098900050331     C*  PIU' RECORD DELLA STESSA DATA (IN CASA DI RIELABORAZIONE
099000050331     C*  CONTEMPORANEA A NUOVE SCRITTURE)
099100050331    2C     STADTS        IFNE      SAV_StaDTS
099200050331     C                   EXSR      ELASTA
099300050331     C**
099400050331     C** MI SALVO KEY PER RIPOSIZIONAMENTO
099500050331     C                   MOVEL     STADTS        WDTS
099600050331     C** DELETO I PRECEDENTI
099700050331     c                   ExSR      DELETOSTA
099800050331     C**
099900050331     C** MI RIPOSIZOINO SUL NUOVO LETTO E LO SALVO IN DS
100000160729     c                   select
100100160729     c                   when      ds1fpg= 'A'
100200050331     C     Kfista1w      CHAIN     FiSTA000                           99
100300160729     c                   when      ds1fpg= 'C'
100400160729     C     Kfista3w      CHAIN     FiSTA003                           99
100500160729     c                   other
100600050331     C     Kfista2w      CHAIN     FiSTA002                           99
100700160729     c                   endsl
100800050331     C**
100900050331     C                   Z-ADD     1             WCONT
101000050331     C                   MOVEL     DSSTA         SAVSTA
101100050331   X2C                   ELSE
101200050331     C*
101300050331     C* RECORD CON STESSA DATA LETTO 2 VOLTE
101400050331     C                   ADD       STANCL        SAV_StaNCL
101500050331     C                   ADD       1             WCONT             5 0
101600050331    2C                   ENDIF
101700050331     c
101800050331     C*
101900160729     c  n99              select
102000160729     c                   when      ds1fpg= 'A'
102100050331     C     kfista1       READE     FiSTA01L                             9899
102200160729     c                   when      ds1fpg= 'C'
102300160729     C     kfista3       READE     FiSTA03L                             9899
102400160729     c                   other
102500050331     C     kfista2       READE     FiSTA02L                             9899
102600160729     c                   endsl
102700050331    1C                   ENDDO
102800050331     c
102900050331     C* ULTIMA ELABORAZIONE
103000050331     C     SAV_StaTRC    IFNE      ' '
103100050331     C                   EXSR      ELASTA
103200050331     C** DELETO L'ULTIMO
103300050331     c                   exsr      DELETOSTA
103400050331     C                   ENDIF
103500050331     c                   endsr
103600001027     C*---------------------------------------------------------------*
103700001027     C* ELABORO STATISTICA CONTENUTA IN FNSTA
103800001027     C*---------------------------------------------------------------*
103900001027     C     ELASTA        BEGSR
104000001027     C                   CLEAR                   WSIDEL
104100001027     C**
104200001027     C* DS1RIS = ' ' --> ELABORAZIONE SENZA STAMPA
104300001027     C                   MOVEL     *BLANKS       DS1RIS
104400001027     C***                  Z-ADDSTADTS    DS1ADA
104500021022     C                   Z-ADD     SAV_StaDTS    DS1ADA
104600001027     C* VEDO CHE TIPO BOLLA E'
104700021022    1C     SAV_StaTRC    IFEQ      'C'
104800021022     C     SAV_StaTRC    OREQ      'A'
104900001027     C                   MOVEL     'A'           DS1TBO
105000001027     C                   ELSE
105100001027     C                   MOVEL     'P'           DS1TBO
105200001027    1C                   ENDIF
105300001027     C**
105400001027     C                   EXSR      TESTF
105500001027      *
105600001027    1C     $OK           IFEQ      'S'
105700050331     c
105800021022    2C     SAV_StaTRC    IFNE      'P'
105900021022    2C     SAV_StaTRC    ANDNE     'T'
106000001122     C*
106100160726     C     SAV_StaNCL    orge      05
106200001122     C*
106300001027     C                   MOVEL     'S'           WSIDEL            1
106400001027     C* Inverto data
106500021022     C                   Z-ADD     SAV_StaDTS    G02INV
106600001027     C                   Z-ADD     *ZERO         G02DAT
106700001027     C                   MOVEL     '3'           G02ERR
106800001027     C                   CALL      'XSRDA8'
106900001027     C                   PARM                    WLBDAT
107000001027     C                   Z-ADD     G02DAT        DS1GDA
107100001027     C                   Z-ADD     G02DAT        DS1GAL
107200021022     C                   Z-ADD     SAV_StaDTS    DS1ADA
107300021022     C                   Z-ADD     SAV_StaDTS    DS1AAL
107400001027     C                   MOVEL     SIMFEL        DS1LNP
107500001027     C                   MOVEL     SIMFEL        DS1FEL
107600001027     C* STATISTICA CONSEGNE
107700021022    3C     SAV_StaTRC    IFEQ      'C'
107800021022     C                   Z-ADD     SAV_StaFil    DS1LNP
107900021022     C                   Z-ADD     SAV_StaFil    DS1LNA
108000021022     C                   Z-ADD     SAV_StaFil    DS1FGS
108100001027     C* IMPOSTO IL FLAG DI ELABORAZIONE STATISTICA
108200001027     C                   MOVEL     'S'           DS1FL1
108300001027    3C                   ENDIF
108400001027     C**
108500001027     C* LANCIO  statistica per elaborazione dati
108600001027     C                   EXSR      CHIAMA
108700001027    2C                   ENDIF
108800001027   X1C                   ELSE
108900001027     C**
109000001027     C* IMPOSSIBILE RIELABORARE LA DATA --> DELETO
109100001027     C                   MOVEL     'S'           WSIDEL
109200001027    1C                   ENDIF
109300001027     C                   ENDSR
109400050331     C*---------------------------------------------------------------*
109500050331     C* cancello record di fista elaborati
109600050331     C*---------------------------------------------------------------*
109700050331     c     DELETOSTA     begsr
109800050331     C     WSIDEL        IFEQ      'S'
109900160729     c                   select
110000160729     c                   when      ds1fpg='A'
110100050331     C     Kfista1s      CHAIN     Fista000                           99
110200050331     C     *IN99         DOWEQ     *OFF
110300050331     C                   DELETE    FiSTA000
110400050331     C     Kfista1s      READE     FiSTA000                               99
110500050331     C                   ENDDO
110600160729
110700160729     c                   when      ds1fpg='C'
110800160729     C     Kfista3s      CHAIN     Fista003                           99
110900160729     C     *IN99         DOWEQ     *OFF
111000160729     C                   DELETE    FiSTA003
111100160729     C     Kfista3s      READE     FiSTA003                               99
111200160729     C                   ENDDO
111300160729     c                   other
111400050331     C     Kfista2s      CHAIN     Fista002                           99
111500050331     C     *IN99         DOWEQ     *OFF
111600050331     C                   DELETE    FiSTA002
111700050331     C     Kfista2s      READE     FiSTA002                               99
111800050331     C                   ENDDO
111900160729     C                   ENDsl
112000050331     c
112100050331     C                   ENDIF
112200050331     c                   ENDSR
112300970130     C*---------------------------------------------------------------*
112400970130     C*                    CRTTAB                                     *
112500970130     C* CREO RECORD IN TABELLA DS7A
112600970130     C*---------------------------------------------------------------*
112700970130     C     CRTTAB        BEGSR
112800970130     C                   CLEAR                   DS7AF
112900970130     C                   CLEAR                   TABEL
113000970130     C                   MOVEL     *ZEROS        ALFDUE
113100970130     C                   MOVEL     *ZEROS        ALFDPE
113200970130     C                   MOVEL     DS7AF         TBLUNI
113300970206     C                   MOVEL     $LIN          TBLKEY
113400970130     C                   MOVEL     '7A'          TBLCOD
113500970130     C                   MOVEL     '1'           TBLKUT
113600970130     C                   WRITE     TABEL
113700970130     C                   ENDSR
113800001027     C*---------------------------------------------------------------*
113900001027     C* CARICO TABELLA £1 PER CONTROLLO SE C'E' P.O. POSTE
114000001027     C*---------------------------------------------------------------*
114100001027     C     CARTAB        BEGSR
114200001027      *
114300001027     C     *LIKE         DEFINE    DSLS01        SAVDS
114400001027     C     *LIKE         DEFINE    DS1GDA        $VIDD
114500001027     C     *LIKE         DEFINE    DS1GAL        $VIDA
114600001027     C     *LIKE         DEFINE    DS1ADA        $COMD
114700001027     C     *LIKE         DEFINE    DS1AAL        $COMA
114800001027     C     *LIKE         DEFINE    DS1RIS        $STARI
114900001027     C     *LIKE         DEFINE    DS1LNP        $LIN
115000001027     C     *LIKE         DEFINE    TBLCOD        KCOD
115100001027     C     *LIKE         DEFINE    TBLKEY        KKEY
115200001027     C     *LIKE         DEFINE    DS1ADA        SAVADA
115300001027     C     *LIKE         DEFINE    DS1GAL        SAVGAL
115400001027     C     *LIKE         DEFINE    DS1GDA        SAVGDA
115500001027     C     *LIKE         DEFINE    DS1AAL        SAVAAL
115600001027     C     *LIKE         DEFINE    DS1LNP        SAVLNP
115700001027     C     *LIKE         DEFINE    DS1RIS        SAVRIS
115800001027     C     *LIKE         DEFINE    DS1FGS        SAVFGS
115900001027     C     *LIKE         DEFINE    STADTS        WDTS
116000001027      *
116100001027     C     KTAB1         KLIST
116200001027     C                   KFLD                    CODUT
116300001027     C                   KFLD                    KCOD
116400001027     C                   KFLD                    KKEY
116500021017
116600050331     c     Kfista1s      Klist
116700021022     c                   Kfld                    Sav_StaTrc
116800021017     c                   Kfld                    Simfel
116900021022     c                   Kfld                    Sav_StaFil
117000021022     c                   Kfld                    Sav_StaDts
117100050331     c     Kfista2s      Klist
117200050331     c                   Kfld                    Sav_StaTrc
117300050331     c                   Kfld                    Simfel
117400050331     c                   Kfld                    Sav_StaDts
117500160729     c     Kfista3s      Klist
117600160729     c                   Kfld                    Sav_StaTrc
117700160729     c                   Kfld                    Sav_StaFil
117800160729     c                   Kfld                    Sav_StaDts
117900021022
118000021022     c     Kfista1       Klist
118100021022     c                   Kfld                    Ds1Fpg
118200021022     c                   Kfld                    Simfel
118300021022     c                   Kfld                    Wfil
118400050331
118500050331     c     Kfista2       Klist
118600050331     c                   Kfld                    Ds1Fpg
118700050331     c                   Kfld                    Simfel
118800160729     c     Kfista3       Klist
118900160729     c                   Kfld                    Ds1Fpg
119000160729     c                   Kfld                    Wfil
119100021022
119200050331     c     Kfista1w      Klist
119300021022     c                   Kfld                    Ds1Fpg
119400021022     c                   Kfld                    Simfel
119500021022     c                   Kfld                    Wfil
119600021022     C                   KFLD                    WDTS
119700050331     c     Kfista2w      Klist
119800050331     c                   Kfld                    Ds1Fpg
119900050331     c                   Kfld                    Simfel
120000050331     C                   KFLD                    WDTS
120100160729     c     Kfista3w      Klist
120200160729     c                   Kfld                    Ds1Fpg
120300160729     c                   Kfld                    Wfil
120400160729     C                   KFLD                    WDTS
120500001027      *
120600001027      * RILEVO LA DATA DEL GIORNO E LA INVERTO
120700001027     C                   TIME                    WTIME            14 0
120800001027     C                   MOVE      WTIME         WDATE             8 0
120900001027     C                   Z-ADD     WDATE         G02DAT
121000001027     C                   Z-ADD     *ZERO         G02INV
121100001027     C                   MOVEL     *BLANKS       G02ERR
121200001027     C                   CALL      'XSRDA8'
121300001027     C                   PARM                    WLBDAT
121400001027     C                   Z-ADD     G02INV        WDATEU            8 0
121500001027     C**
121600001027     C                   Z-ADD     1             CODUT
121700001027     C                   CALL      'X§PARUT'
121800001027     C                   PARM                    UT§DSE
121900001027     C                   MOVEL     REC80         CNCR80
122000001027      *
122100001027      * Prelevo le due tabelle contenenti i giorni di pulizia degli archivi
122200001027     C                   MOVE      '5A'          KCOD
122300001027     C                   MOVEL     *BLANKS       KKEY
122400001027     C                   MOVEL     '1'           KKEY
122500001027     C     KTAB1         CHAIN     TABEL00F                           99
122600001027     C   99              CLEAR                   DS5A
122700001027     C  N99              MOVEL     TBLUNI        DS5A
122800001027     C                   MOVE      '5A'          KCOD
122900040924     C****               MOVEL(P)  '2'           KKEY
123000040924     C                   MOVEL(P)  'SEDE1'       KKEY
123100001027     C     KTAB1         CHAIN     TABEL00F                           99
123200040924     C** 99              CLEAR                   DS5A2
123300040924     C**N99              MOVEL     TBLUNI        DS5A2
123400040924     C   99              CLEAR                   DS5AS1
123500040924     C  N99              MOVEL     TBLUNI        DS5AS1
123600001027     C***
123700001027     C* CARICO TABELLA PUNTI OPERATIVI GESTITI £1
123800001027     C***
123900001027     C                   CLEAR                   DSUL06
124000001027     C                   MOVE      '£1'          D06COD
124100001027     C                   MOVEL     SIMFEL        D06KEY
124200001027     C                   MOVEL     'L'           D06TLA
124300001027     C                   MOVEL     DSUL06        KPJBU
124400001027     C                   CALL      'TRUL06R'
124500001027     C                   PARM                    KPJBA
124600001027     C                   MOVEL     KPJBU         DSUL06
124700001027     C* CONTROLLO LIN
124800001027     C                   Z-ADD     1             XX                3 0
124900050401     C                   Z-ADD     1             yy                3 0
125000050401     c
125100001027     C     LIN(XX)       DOWGT     0
125200001027     C     LIN(XX)       CHAIN     AZORG01L                           57
125300001027     C  N57              MOVEL     ORGDE3        OG143
125400021022     C   57              CLEAR                   §OGNTW
125500050401     c* Carico in un'altra schiera i p.o. esteri della £1
125600050401     c                   if        §ogntw='EEX' or §ogntw='EUP' or §ogntw=
125700050401     c                             'DPD' or §ogntw='FED'
125800050401     c                   eval      est(yy)=lin(xx)
125900050401     C                   ADD       1             yy
126000050401     c                   endif
126100001027     C**
126200001027     C                   ADD       1             XX
126300001027     C                   ENDDO
126400021016      * Recupero le date delle statistiche
126500021016     c                   Clear                   Tibs02Ds
126600021016     c                   Clear                   dSdf
126700021016     c                   Eval      T02Mod = 'C'
126800021016     c                   Eval      T02Sif = Knsif
126900021016     c                   Eval      T02Cod = 'SDF'
127000021016     c                   Movel(p)  Simfel        T02Ke1
127100021016     c                   Call      'TIBS02R'
127200021016     c                   Parm                    Kpjba
127300021016     c                   Parm                    Tibs02Ds
127400021016     c                   If        T02Err = *Blanks
127500021016     c                   Movel     T02Uni        dSdf
127600021016     c                   EndIf
127700021016
127800021016     c                   Z-Add     §SdfArrP      E6daPe            8 0
127900021016     c                   Z-Add     §SdfArrU      E6daUe            8 0
128000021016     c                   Z-Add     §SdfParP      C1daPe            8 0
128100021016     c                   Z-Add     §SdfParU      C1daUe            8 0
128200021016     c                   Z-Add     §SdfTerP      C2daPe            8 0
128300021016     c                   Z-Add     §SdfTerU      C2daUe            8 0
128400021016     c                   Z-Add     §SdfEntP      C3daPe            8 0
128500021016     c                   Z-Add     §SdfEntU      C3daUe            8 0
128600021016     c                   Z-Add     §SdfBolP      C5daPe            8 0
128700021016     c                   Z-Add     §SdfBolU      C5daUe            8 0
128800021016
128900001027     C                   ENDSR
129000000000      **************************************************************************
129100000000** Numero giorni dei mesi
129200000000312931303130313130313031
