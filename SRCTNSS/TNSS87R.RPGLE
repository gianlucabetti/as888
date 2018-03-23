000100960927     H*------------------------------------------------------------------------*
000200960927     H* STATISTICA RITARDI TRAINI - CREA WORKFILE
000300960927     H*------------------------------------------------------------------------*
000400960927     H DECEDIT('0,') DATEDIT(*DMY.)
000500960927     F*------------------------------------------------------------------------*
000600960927     F* DATA BASE
000700960927     F*------------------------------------------------------------------------*
000800960927     FAZORG01L  IF   E           K DISK
000900960927     FTABEL00F  IF   E           K DISK
001000960927     F*------------
001100960927     F* GRIGLIA
001200960927     F*------------
001300960927     FTNTST04L  IF   E           K DISK
001400960927     FTNTSR01L  IF   E           K DISK
001500960927     F*------------
001600960927     F* WORK FILE
001700960927     F*------------
001800960927     FWFRIT01L  UF A E           K DISK
001900960927     D*------------------------------------------------------------------------*
002000960927     D* SCHIERE
002100960927     D*------------------------------------------------------------------------*
002200960927     D*------------------
002300960927     D* ORGANIGRAMMA
002400960927     D*------------------
002500971124     D ORG             S              3  0 DIM(500)                             *FILIALE
002600971124     D ARE             S              3  0 DIM(500)                             *AREA
002700971124     D DIV             S              1    DIM(500)                             *DIVISIONE
002800960927     D*------------------
002900960927     D* CAUSALI RITARDO TRAINI
003000960927     D*------------------
003100960927     D CAU             S              1    DIM(300)                             *CODICE
003200960927     D IMP             S              1    DIM(300)                             *IMPUTAZIONE RITARDO
003300960927     D FST             S              1    DIM(300)                             *FLAG STAMPA
003400960927     D*------------------
003500960927     D* CAUSALI RITARDI
003600960927     D*------------------
003700060410     D $TCR            S              1    DIM(100)                             *TIPI CAUSALI RITARD
003800060410     D $IMP            S              4    DIM(100)                             *IMPUTAZIONE RITARDO
003900060410     D*------------------
004000060410     D nfa             S              3  0 DIM(40)                              *TIPI CAUSALI RITARD
004100060410     D nfp             S              3  0 DIM(40)                              *TIPI CAUSALI RITARD
004200960927     D*------------------------------------------------------------------------*
004300960927     D* INPUT
004400960927     D*------------------------------------------------------------------------*
004500960927     D*------------------
004600960927     D* ARCHITETTURA
004700960927     D*------------------
004800960927     D KPJBA         E DS
004900960927     D*------------------
005000960927     D* DS PARAMETRI LANCIO
005100960927     D*------------------
005200960927     D PARAM           DS
005300990608     D  PARDAT                 1      6  0
005400990608     D  PARDA2                 7     12  0
005500990608     D  PARCRE                13     13
005600000706     D  PARPOS                14     14
005700000706     D  PARTAD                15     15
005800060410     D  PARlin                16     17P 0
005900960927     D*------------------
006000960927     D* DS SCOMPOSIZIONE DATA TRAINO
006100960927     D*------------------
006200960927     D                 DS
006300960927     D  TSTANN                 1      4  0
006400960927     D  TSTMES                 5      6  0
006500990608     D  TSTDAT                 1      6  0
006600960927     D  TSTGIO                 7      8  0
006700960927     D  TSTDTN                 1      8  0
006800960927     D*------------------
006900960927     D* DS COMPOSIZIONE DATA WORKFILE
007000960927     D*------------------
007100960927     D                 DS
007200960927     D  WFLANN                 1      4  0
007300960927     D  WFLMES                 5      6  0
007400960927     D  WFLDAT                 1      6  0
007500960927     D*------------------
007600960927     D* DS SCOMPOSIZIONE DATA GENERICA
007700960927     D*------------------
007800960927     D                 DS
007900960927     D  $DASEC                 1      2  0
008000960927     D  $DAANN                 3      4  0
008100960927     D  $DAAES                 1      4  0
008200960927     D  $DAMES                 5      6  0
008300960927     D  $DAGIO                 7      8  0
008400960927     D  $DATA8                 1      8  0
008500960927     D*------------------
008600960927     D* DS CAUSALI RITARDO TRAINI
008700960927     D*------------------
008800960927     D DSTC          E DS
008900060410     d savtfp          s                   like(tsttfp)
009000960927     C*------------------------------------------------------------------------*
009100960927     C* MAIN LINE
009200960927     C*------------------------------------------------------------------------*
009300960927     C*
009400960927     C* POSIZIONAMENTO SUL FILE E PRIMA LETTURA
009500960927     C                   EXSR      SETFIL
009600960927     C*
009700960927     C* CICLO FINO A FINE FILE
009800960927DO  1C     $FINE         DOWEQ     '0'
009900960927     C*
010000960927     C* SCRIVE IL FILE DI LAVORO
010100960927     C                   EXSR      AGGWFL
010200960927     C*
010300960927     C* LEGGE IL RECORD SUCCESSIVO
010400960927     C                   EXSR      LETFIL
010500960927E   1C                   ENDDO
010600960927     C*
010700960927     C                   SETON                                        LR
010800960927     C*------------------------------------------------------------------------*
010900960927     C* SETFIL - POSIZIONAMENTO E PRIMA LETTURA
011000960927     C*------------------------------------------------------------------------*
011100960927     C     SETFIL        BEGSR
011200960927     C*
011300960927     C     *LOVAL        SETLL     TNTST04L                           99        *FINE ARCHIVIO
011400960927IF  1C     *IN99         IFEQ      *ON
011500960927     C                   MOVEL     '1'           $FINE
011600960927X   1C                   ELSE
011700960927     C                   EXSR      LETFIL                                       *LETTURA RECORD
011800960927E   1C                   ENDIF
011900960927     C*
012000960927     C                   ENDSR
012100960927     C*------------------------------------------------------------------------*
012200960927     C* LETFIL - LETTURA PROSSIMO RECORD
012300960927     C*------------------------------------------------------------------------*
012400960927     C     LETFIL        BEGSR
012500960927     C*
012600960927     C* LEGGE FINO A:
012700960927     C*     - FINE FILE
012800960927     C*     - TROVATO RECORD VALIDO
012900960927     C                   MOVEL     'N'           WRECOK
013000960927DO  1C     $FINE         DOWEQ     '0'
013100960927     C     WRECOK        ANDEQ     'N'
013200960927     C*
013300960927     C                   READ      TNTST04L                               99
013400960927     C*
013500960927IF  2C     *IN99         IFEQ      *ON                                          *FINE ARCHIVIO
013600960927     C                   MOVEL     '1'           $FINE
013700960927X   2C                   ELSE
013800960927     C                   EXSR      CHKREC                                       *CONTROLLO RECORD
013900960927E   2C                   ENDIF
014000960927E   1C                   ENDDO
014100960927     C*
014200960927     C                   ENDSR
014300960927     C*------------------------------------------------------------------------*
014400960927     C* CHKREC - CONTROLLA VALIDITA' RECORD
014500960927     C*------------------------------------------------------------------------*
014600960927     C     CHKREC        BEGSR
014700960927     C*
014800960927     C                   MOVEL     'S'           WRECOK                         *RECORD VALIDO
014900960927     C*
015000960927     C* CREA IL WORKFILE DAL PRIMO MESE INTERO DISPONIBILE IN GRIGLIA
015100960927     C* FINO ALLA DATA FINALE RICHIESTA A VIDEO
015200960927IF  1C     TSTDAT        IFGE      WDICRE                                       *> DATI IN GRIGLIA
015300990608     C     TSTDAT        ANDLE     PARDA2                                       *< DATA FINALE RICH.
015400960927X   1C                   ELSE
015500960927     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
015600960927E   1C                   ENDIF
015700960927     C*
015800960927     C* IL PERIODO PRIMA DI GIUGNO 96 E' ERRATO, QUINDI ESCLUDE
015900960927IF  1C     TSTDAT        IFLT      199606
016000960927     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
016100960927E   1C                   ENDIF
016200960927     C*
016300960927     C* TRAINO ANNULLATO, ESCLUDE
016400960927IF  1C     TSTATB        IFNE      *BLANKS
016500960927     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
016600960927E   1C                   ENDIF
016700960927     C*
016800960927     C* TRAINO NON CONFERMATO, ESCLUDE
016900960927IF  1C     TSTCNF        IFEQ      *BLANKS
017000960927     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
017100960927E   1C                   ENDIF
017200960927     C***
017300960927     C* DETERMINA LA DECADE
017400960927     C***
017500960927IF  1C     TSTGIO        IFGE      1
017600960927     C     TSTGIO        ANDLE     10
017700960927     C                   MOVEL     '1'           TSTDEC            1            *PRIMA DECADE
017800960927X   1C                   ELSE
017900960927IF  2C     TSTGIO        IFGE      11
018000960927     C     TSTGIO        ANDLE     20
018100960927     C                   MOVEL     '2'           TSTDEC            1            *SECONDA DECADE
018200960927X   2C                   ELSE
018300960927     C                   MOVEL     '3'           TSTDEC            1            *TERZA DECADE
018400960927E   2C                   ENDIF
018500960927E   1C                   ENDIF
018600960927     C*
018700960927     C                   ENDSR
018800960927     C*------------------------------------------------------------------------*
018900960927     C* AGGWFL - AGGIORNA IL FILE DI LAVORO
019000960927     C*------------------------------------------------------------------------*
019100960927     C     AGGWFL        BEGSR
019200960927     C*
019300960927     C* REPERISCE I RITARDI PARTENZA E ARRIVO DEL TRAINO
019400960927     C                   EXSR      REPRIT
019500060411     c
019600060411     c* se non trovato nemmeno un ritardo per il terminal principale del
019700060411     c*  traino, lo aggiungo per memorizzare le partenze
019800060411     c                   if        tfptestata=' '
019900060411     c                   add       1             c
020000060411     c                   movel     tsttfp        $imp(C)
020100060410     c                   endif
020200960927     C*
020300960927     C* REPERISCE LA DIVISIONE DELLA FILIALE DI PARTENZA
020400960927     C                   SETOFF                                           28
020500060410     C                   Z-ADD     1             c
020600060410     c
020700060410    1c                   dow       $imp(C)<>*blanks
020800060410     c                   movel     $imp(C)       wtfp              3 0
020900060410     c                   mllzo     1             wtfp
021000060410     C                   Z-ADD     1             I
021100060410     C**   TSTTFP        LOOKUP    ORG(I)                                 28
021200060410     C     wtfp          LOOKUP    ORG(I)                                 28
021300060410IF  2C     *IN28         IFEQ      *ON
021400960927     C*
021500960927     C* LEGGE IL WORKFILE PER VERIFICARE L'ESISTENZA DEL RECORD DA SCRIVERE
021600960927     C                   MOVEL     DIV(I)        KWLDIV
021700960927     C                   Z-ADD     ARE(I)        KWLARE
021800060410     C***                Z-ADD     TSTTFP        KWLTFP
021900060410     C                   Z-ADD     wtfp          KWLTFP
022000960927     C                   Z-ADD     TSTANN        KWLANN
022100960927     C                   Z-ADD     TSTMES        KWLMES
022200960927     C                   MOVEL     TSTDEC        KWLDEC
022300960927     C     KEYWFL        CHAIN     WFRIT01L                           98
022400060410IF  3C     *IN98         IFEQ      *ON                                          *IMMISSIONE
022500960927     C                   CLEAR                   WFRIT000                       *CANCELLA RECORD
022600060410E   3C                   ENDIF
022700060410     c
022800960927     C* IMPOSTA I CAMPI
022900060410    3c                   if        wtfp=tsttfp
023000960927     C                   ADD       1             WFLPAR                         *TOTALE PARTENZE
023100060410    3c                   endif
023200060410     c
023300060410     c                   eval      savtfp=wtfp
023400060410     c
023500060410    3c                   dow       wtfp=savtfp
023600060410IF  3C     $TCR(C)       IFNE      *BLANKS
023700060410     c                   move      $imp(c)       wIMP              1
023800960927     C                   ADD       1             WFLRIT                         *TOTALE RITARDI
023900060410IF  4C     wimp          IFEQ      'P'
024000990817     C                   ADD       1             WFLFIL                         *COLPA FIL/ARR RITAR
024100960927E   4C                   ENDIF
024200060410IF  4C     wimp          IFEQ      'A'
024300960927     C                   ADD       1             WFLAUT                         *COLPA AUTISTA
024400960927E   4C                   ENDIF
024500060410IF  4C     wimp          IFEQ      'F'
024600960927     C                   ADD       1             WFLCFM                         *CAUSA FORZA MAGGIOR
024700960927E   4C                   ENDIF
024800060410IF  4C     wimp          IFEQ      'S'
024900960927     C                   ADD       1             WFLSED                         *TRINI SEDE
025000960927E   4C                   ENDIF
025100060410IF  4C     wimp          IFEQ      'T'
025200960927     C                   ADD       1             WFLTRS                         *TRANSITO
025300960927E   4C                   ENDIF
025400060410IF  4C     wimp          IFEQ      'O'
025500990817     C                   ADD       1             WFLFIO                         *COLPA FIL/ARR ORARI
025600990817E   4C                   ENDIF
025700060410E   4C                   ENDIF
025800060410     c
025900060410     c                   add       1             C
026000060410     c                   if        $imp(c)=*blanks
026100060410     c                   clear                   wtfp
026200060410     c                   else
026300060410     c                   movel     $imp(c)       wtfp
026400060410     c                   mllzo     1             wtfp
026500060410     c                   endif
026600060410     c
026700060410    3c                   enddo
026800060410E   3C**                 ENDIF
026900960927     C* IMMISSIONE
027000060410IF  3C     *IN98         IFEQ      *ON
027100960927     C                   MOVEL     KWLDIV        WFLDIV
027200960927     C                   Z-ADD     KWLARE        WFLARE
027300060410     C                   Z-ADD     kwltfp        WFLTFP
027400960927     C                   Z-ADD     TSTANN        WFLANN
027500960927     C                   Z-ADD     TSTMES        WFLMES
027600960927     C                   MOVEL     TSTDEC        WFLDEC
027700960927     C                   WRITE     WFRIT000
027800960927     C* AGGIORNAMENTO
027900060410X   3C                   ELSE
028000960927     C                   UPDATE    WFRIT000
028100060410E   3C                   ENDIF
028200060410     c
028300060410E   2C                   ENDIF
028400060410    1c                   enddo
028500960927     C*
028600960927     C                   ENDSR
028700960927     C*------------------------------------------------------------------------*
028800960927     C* REPRIT - REPERISCE I RITARDI PARTENZA E ARRIVO DEL TRAINO
028900960927     C*------------------------------------------------------------------------*
029000960927     C     REPRIT        BEGSR
029100960927     C*---
029200960927     C* SE PER IL TRAINO ESISTONO + CAUSALI CON = IMPUTAZIONE, NE CONTEGGIA UNA
029300960927     C*---
029400060410     C                   CLEAR                   $TCR                           *TIPI CAUSALI RITARD
029500060410     C                   CLEAR                   $IMP                           *IMPUTAZIONE  RITARD
029600060410     C                   Z-ADD     *ZEROS        C                 4 0
029700960927     C                   Z-ADD     TSTTRN        KSRTRN                         *NUMERO TRAINO
029800960927     C                   Z-ADD     TSTDTN        KSRDTN                         *DATA TRAINO
029900060410     c                   clear                   nfa
030000060410     c                   clear                   nfp
030100060411     c                   clear                   tfptestata        1
030200060411     c                   clear                   imppart           1
030300060410     c                   clear                   imptfp            3 0
030400060410     c                   z-add     0             IP                3 0
030500060410     c                   z-add     0             IA                3 0
030600960927     C     KEYTSR        SETLL     TNTSR01L
030700960927     C     KEYTSR        READE     TNTSR01L                               98
030800960927DO  1C     *IN98         DOWEQ     *OFF
030900060410IF  2C***  TSRFFP        IFEQ      TSTTFP                                       *FILIALE PARTENZA
031000960927     C*  A R R I V O
031100060410     c* Ogni numero fermata lo elaboro solo una volta
031200060410     c*  se in arrivo c'e' una colpa ritardo imputabile alla partenza,
031300060410     c*  verifico se la stessa causale è stata impostata in una fermata di
031400060410     c*  carico, altrimenti la colpa, come ora, viene data alla partenza
031500060410     c*  del traino
031600060410IF  2C     TSRRTA        IFNE      *BLANKS                                      *IN RITARDO ARRIVO
031700060410     c     tsrnfa        lookup    nfa                                    30
031800060410    3c                   if        not *in30
031900060410     c                   add       1             IA
032000060410     c                   eval      nfa(IA)=tsrnfa
032100960927     C*
032200960927     C* REPERISCE L'IMPUTAZIONE RITARDO ARRIVO
032300960927     C                   Z-ADD     1             I
032400960927     C                   SETOFF                                           28
032500960927     C                   MOVEL     *BLANKS       DEPIMP            1
032600960927     C                   MOVEL     *BLANKS       DEPFST            1
032700960927     C     TSRRTA        LOOKUP    CAU(I)                                 28
032800960927IF  4C     *IN28         IFEQ      *ON                                          *TROVATO
032900960927     C                   MOVEL     IMP(I)        DEPIMP                         *DEPOSITO IMPUTAZION
033000960927     C                   MOVEL     FST(I)        DEPFST                         *DEPOSITO FLAG STAMP
033100960927E   4C                   ENDIF
033200960927     C*
033300960927     C* CONTROLLA CHE LA CAUSALE RITARDO SIA DA STAMPARE
033400960927     C* CONTROLLA CHE NON SIA GIA'MEMORIZZATA UNA CAUSALE CON = IMPUTAZIONE
033500960927IF  4C     DEPFST        IFNE      'N'
033600060410     c* Se imputazione alla partenza lo tengo memorizzato a parte
033700060410    5c                   if        depimp='P'
033800060410     c                   eval      imppart=tsrrta
033900060410   x5c                   else
034000960927     C                   SETOFF                                           28
034100060410     c* KEY: tfp+IMP
034200060410     c                   movel     tsttfp        w004a
034300060410     c                   move      depimp        w004a             4
034400060410     C***  DEPIMP        LOOKUP    $IMP                                   28
034500060410     C     w004a         LOOKUP    $IMP                                   28
034600060410IF  6C     *IN28         IFEQ      *OFF                                         *NON TROVATO
034700960927     C                   ADD       1             C
034800960927     C                   MOVEL     TSRRTA        $TCR(C)                        TIPO RITARDO ARRIVO
034900060410     C***                MOVEL     DEPIMP        $IMP(C)                        IMPUTAZIONE RITARDO
035000060410     C                   MOVEL     w004a         $IMP(C)                        IMPUTAZIONE RITARDO
035100060411     c* Memorizzo che  il terminal per cui ho scritto il ritardo è il
035200060411     c*  terminal di partenza principale del traino
035300060411     c                   eval      tfptestata='S'
035400060410E   6C                   ENDIF
035500060410E   5C                   ENDIF
035600960927E   4C                   ENDIF
035700960927     C*  P A R T E N Z A
035800060410X   3C***                ELSE
035900060410    3c                   endif
036000060410    2c                   endif
036100060410     c* Controllo se impostata una causale ritardo partenza
036200960927     C*
036300960927     C* REPERISCE L'IMPUTAZIONE RITARDO PARTENZA
036400060410IF  2C     TSRRTP        IFNE      *BLANKS                                      *IN RITARDO PARTENZA
036500060410     c     tsrnfp        lookup    nfp                                    30
036600060410    3c                   if        not *in30
036700060410     c                   add       1             IP
036800060410     c                   eval      nfp(Ip)=tsrnfp
036900060410     c
037000960927     C                   Z-ADD     1             I
037100960927     C                   SETOFF                                           28
037200960927     C                   MOVEL     *BLANKS       DEPIMP
037300960927     C                   MOVEL     *BLANKS       DEPFST
037400960927     C     TSRRTP        LOOKUP    CAU(I)                                 28
037500060410IF  4C     *IN28         IFEQ      *ON                                          *TROVATO
037600960927     C                   MOVEL     IMP(I)        DEPIMP                         *DEPOSITO IMPUTAZION
037700960927     C                   MOVEL     FST(I)        DEPFST                         *DEPOSITO FLAG STAMP
037800060410E   4C                   ENDIF
037900960927     C*
038000960927     C* CONTROLLA CHE LA CAUSALE RITARDO SIA DA STAMPARE
038100960927     C* CONTROLLA CHE NON SIA GIA'MEMORIZZATA UNA CAUSALE CON = IMPUTAZIONE
038200060410IF  4C     DEPFST        IFNE      'N'
038300060410     c* Se causale ritardo partenza, memorizzo sempre, altrimenti solo se
038400060410     c*  non presente una causale ritardo arrivo
038500960927     C                   SETOFF                                           28
038600060410    5c                   if        depimp='P'  and imptfp=0
038700060410     c                   eval      imptfp=tsrffp
038800060410     c                   eval      imppart=tsrrtp
038900060410    5c                   endif
039000060410     c
039100060410    5c                   if        depimp<>'P' and tsrrta=' '
039200060410     c                   movel     tsrffp        w004a
039300060410     c                   move      depimp        w004a             4
039400060410     C***  DEPIMP        LOOKUP    $IMP                                   28
039500060410     C     w004a         LOOKUP    $IMP                                   28
039600060410IF  6C     *IN28         IFEQ      *OFF                                         *NON TROVATO
039700960927     C                   ADD       1             C
039800960927     C                   MOVEL     TSRRTP        $TCR(C)                        TIPO RITARDO PARTENZ
039900060410     C***                MOVEL     DEPIMP        $IMP(C)                        IMPUTAZIONE RITARDO
040000060410     C                   MOVEL     w004a         $IMP(C)                        IMPUTAZIONE RITARDO
040100060411     c* Memorizzo se   il terminal per cui ho scritto il ritardo è il
040200060411     c*  terminal di partenza principale del traino
040300060411    7c                   if        tsrffp=tsttfp
040400060411     c                   eval      tfptestata='S'
040500060411E   7C                   ENDIF
040600060411     c
040700060411E   6C                   ENDIF
040800060410E   5C                   ENDIF
040900060410E   4C                   ENDIF
041000060410E   3C                   ENDIF
041100060410E   2C                   ENDIF
041200060410E   2C***                ENDIF
041300960927     C*
041400960927     C     KEYTSR        READE     TNTSR01L                               98
041500960927E   1C                   ENDDO                                                  *FINE TRATTE
041600060410     c
041700060410     c* Imputo il ritardo causa partenza
041800060410    1c                   if        imppart<>' '
041900060410    2c                   if        imptfp<>0
042000060410     c                   movel     imptfp        w004a
042100060410     c                   else
042200060410     c                   movel     tsttfp        w004a
042300060410    2c                   endif
042400060410     c
042500060410     c                   move      'P'           w004a             4
042600060410     C***  DEPIMP        LOOKUP    $IMP                                   28
042700060410     C     w004a         LOOKUP    $IMP                                   28
042800060410IF  2C     *IN28         IFEQ      *OFF                                         *NON TROVATO
042900060410     C                   ADD       1             C
043000060410     C                   MOVEL     imppart       $TCR(C)                        TIPO RITARDO PARTENZ
043100060410     C***                MOVEL     DEPIMP        $IMP(C)                        IMPUTAZIONE RITARDO
043200060410     C                   MOVEL     w004a         $IMP(C)                        IMPUTAZIONE RITARDO
043300060411     c* Memorizzo se   il terminal per cui ho scritto il ritardo è il
043400060411     c*  terminal di partenza principale del traino
043500060411    7c                   if        tsrffp=tsttfp
043600060411     c                   eval      tfptestata='S'
043700060411E   7C                   ENDIF
043800060410E   2C                   ENDIF
043900060410E   1C                   ENDIF
044000960927     C*
044100960927     C                   ENDSR
044200960927     C*------------------------------------------------------------------------*
044300960927     C* CANWFL - CANCELLA IL FILE DI LAVORO
044400960927     C*------------------------------------------------------------------------*
044500960927     C     CANWFL        BEGSR
044600960927     C*
044700960927     C* CANCELLA CIO'CHE SI PUO' RICREARE DALLA GRIGLIA FINO ALLA DATA RICHIESTA
044800960927     C                   READ      WFRIT01L                               98
044900960927DO  1C     *IN98         DOWEQ     *OFF
045000960927IF  2C     WFLDAT        IFGE      WDICRE                                       *> DATI IN GRIGLIA
045100990608     C     WFLDAT        ANDLE     PARDA2                                       *< DATA FINALE RICH.
045200960927     C                   DELETE    WFRIT000
045300960927E   2C                   ENDIF
045400960927     C                   READ      WFRIT01L                               98
045500960927E   1C                   ENDDO
045600960927     C*
045700960927     C                   ENDSR
045800960927     C*------------------------------------------------------------------------*
045900960927     C* CARTAB - CARICA LE TABELLE
046000960927     C*------------------------------------------------------------------------*
046100960927     C     CARTAB        BEGSR
046200960927     C*--------------------
046300960927     C* FILIALI
046400960927     C*--------------------
046500960927     C                   Z-ADD     *ZEROS        I                 4 0
046600960927     C                   READ      AZORG01L                               99
046700960927DO  1C     *IN99         DOWEQ     *OFF
046800960927IF  2C     ORGFVA        IFEQ      *BLANKS
046900960927     C     ORGFAG        ANDNE     'V'
047000960927     C                   ADD       1             I
047100960927     C                   Z-ADD     ORGFIL        ORG(I)                         *CODICE
047200960927     C                   MOVEL     ORGCAR        ARE(I)                         *AREA
047300960927     C                   MOVEL     ORGFL3        DIV(I)                         *DIVISIONE
047400960927E   2C                   ENDIF
047500960927     C                   READ      AZORG01L                               99
047600960927E   1C                   ENDDO
047700960927     C*--------------------
047800960927     C* CAUSALI RITARDO TRAINI
047900960927     C*--------------------
048000960927     C                   Z-ADD     *ZEROS        I                 4 0
048100020906     C                   Z-ADD     1             KTBKUT
048200960927     C                   MOVEL     'TC'          KTBCOD
048300960927     C     KE2TAB        SETLL     TABEL00F
048400960927     C     KE2TAB        READE     TABEL00F                               99
048500960927DO  1C     *IN99         DOWEQ     *OFF
048600960927IF  2C     TBLFLG        IFEQ      *BLANKS
048700960927     C                   MOVEL(P)  TBLUNI        DSTC
048800960927     C                   ADD       1             I
048900960927     C                   MOVEL     TBLKEY        CAU(I)                         *CODICE
049000960927     C                   MOVEL     §TCFST        FST(I)                         *FLAG STAMPA
049100960927     C                   MOVEL     §TCIMP        IMP(I)                         *IMPUTAZIONE RITARDO
049200960927E   2C                   ENDIF
049300960927     C     KE2TAB        READE     TABEL00F                               99
049400960927E   1C                   ENDDO
049500960927     C*
049600960927     C                   ENDSR
049700960927     C*------------------------------------------------------------------------*
049800960927     C* *INZSR - OPERAZIONI INIZIALI
049900960927     C*------------------------------------------------------------------------*
050000960927     C     *INZSR        BEGSR
050100960927     C*--------------------
050200960927     C* RICEVIMENTO PARAMETRI
050300960927     C*--------------------
050400960927     C     *ENTRY        PLIST
050500960927     C                   PARM                    KPJBA
050600960927     C                   MOVEL     KPJBU         PARAM
050700960927     C*--------------------
050800960927     C* VARIABILI RIFERITE AL DATA BASE
050900960927     C*--------------------
051000960927     C     *LIKE         DEFINE    TBLKUT        KTBKUT                         *TABEL00F
051100960927     C     *LIKE         DEFINE    TBLCOD        KTBCOD
051200960927     C     *LIKE         DEFINE    TSRTRN        KSRTRN                         *TNTSR00F
051300960927     C     *LIKE         DEFINE    TSRDTN        KSRDTN
051400960927     C     *LIKE         DEFINE    WFLDIV        KWLDIV                         *WFRIT00F
051500960927     C     *LIKE         DEFINE    WFLARE        KWLARE
051600960927     C     *LIKE         DEFINE    WFLTFP        KWLTFP
051700960927     C     *LIKE         DEFINE    WFLANN        KWLANN
051800960927     C     *LIKE         DEFINE    WFLMES        KWLMES
051900960927     C     *LIKE         DEFINE    WFLDEC        KWLDEC
052000960927     C*--------------------
052100960927     C* CHIAVI DI LETTURA
052200960927     C*--------------------
052300960927     C* LETTURA TABEL00F - PARZIALE
052400960927     C     KE2TAB        KLIST
052500960927     C                   KFLD                    KTBKUT                         *CODICE UTENTE
052600960927     C                   KFLD                    KTBCOD                         *CODICE TABELLA
052700960927     C* LETTURA TNTSR01L - PARZIALE
052800960927     C     KEYTSR        KLIST
052900960927     C                   KFLD                    KSRTRN                         *NUMERO TRAINO
053000960927     C                   KFLD                    KSRDTN                         *DATA TRAINO
053100960927     C* LETTURA WFRIT01L - COMPLETA
053200960927     C     KEYWFL        KLIST
053300960927     C                   KFLD                    KWLDIV                         *DIVISIONE
053400960927     C                   KFLD                    KWLARE                         *AREA
053500960927     C                   KFLD                    KWLTFP                         *FILIALE PARTENZA
053600960927     C                   KFLD                    KWLANN                         *ANNO
053700960927     C                   KFLD                    KWLMES                         *MESE
053800960927     C                   KFLD                    KWLDEC                         *DECADE
053900960927     C*--------------------
054000960927     C* VARIABILI DI CONTROLLO
054100960927     C*--------------------
054200960927     C                   MOVEL     '0'           $FINE             1            *FINE PGM
054300960927     C                   MOVEL     'N'           WRECOK            1            *VALIDITA' RECORD
054400960927     C*
054500960927     C* REPERISCE LA DATA DELL'ULTIMO TRAINO CANCELLATO IN GRIGLIA
054600960927     C     *DTAARA       DEFINE                  TNTL58            8 0
054700960927     C                   IN        TNTL58
054800960927     C*
054900960927     C* CALCOLA IL MESE SUCCESSIVO DALLA DATA X CREARE IL WORKFILE (MESE INTERO)
055000960927     C                   Z-ADD     *ZEROS        WDICRE            6 0
055100960927IF  1C     TNTL58        IFGT      *ZEROS
055200960927     C                   MOVEL     TNTL58        $DATA8
055300960927     C     $DAMES        ADD       1             $DAMES
055400960927IF  2C     $DAMES        IFEQ      13
055500960927     C                   Z-ADD     01            $DAMES
055600960927     C     $DAAES        ADD       1             $DAAES
055700960927E   2C                   ENDIF
055800960927     C                   MOVEL     $DATA8        WDICRE                         DATA INIZIO CREAZION
055900960927X   1C                   ELSE
056000960927     C                   MOVEL     '1'           $FINE                          *FINE PGM
056100960927E   1C                   ENDIF
056200960927     C*
056300960927     C* CARICA LE TABELLE
056400960927     C                   EXSR      CARTAB
056500960927     C*
056600960927     C* CANCELLA IL WORKFILE
056700960927     C                   EXSR      CANWFL
056800960927     C*
056900960927     C                   ENDSR
