000100011017      *------------------------------------------------------------------------*
000200011017      *                                                                        *
000300011017      * Converte l'ultimo progressivo tariffa da LIRE ad EURO                  *
000400011017      * escluse le tariffe di cartello e le tariffe estere.                    *
000500011017      *                                                                        *
000600011017      * NOTA BENE :                                                            *
000700011017      * ===========                                                            *
000800011017      * Questo programma deve girare solo in SEDE.                             *
000900011018      * ...E richiede la KPJBA come parametro !                                *
001000011121      *                                                                        *
001100011121      * Nella KPJBU sono richiesti:   01 - 07 = codice cliente iniziale        *
001200011121      *                               08 - 14 = codice cliente finale          *
001300011017      *                                                                        *
001400011017      *------------------------------------------------------------------------*
001500011017
001600011017     H DECEDIT('0,') DATEDIT(*DMY.)
001700011017
001800011017      *------------------------------------------------------------------------*
001900011017      * ARCHIVI
002000011017      *------------------------------------------------------------------------*
002100011017
002200011017     F* - Tariffe (o Offerte) Clienti - Testata
002300011017     FTNTAM04L  IF   E           K DISK
002400011017     F*
002500011017     FTABEL00F  IF   E           K DISK
002600011017     F*
002700011017     F* - Stampa eventuali errori
002800011017     FQSYSPRT   O    F  132        PRINTER oflind(*inOA)
002900011017
003000011017      *------------------------------------------------------------------------*
003100011017      * SCHIERE
003200011017      *------------------------------------------------------------------------*
003300011017
003400011017     D* Messaggi restituiti dal pgm. TNTA30R
003500011017     D ERR             s              3    dim(2) ctdata perrcd(1)
003600011017     D MSG             s             80    dim(2) alt(ERR)
003700011017
003800011017      *------------------------------------------------------------------------*
003900011017      * STRUTTURE DATI
004000011017      *------------------------------------------------------------------------*
004100011017
004200011017     D* Parametri
004300011017     D KPJBA         e ds
004400011121     D saveKPJBA       s                   like(KPJBA)
004500011121     D*
004600011017     D* Flag operativi di TNTAM (campo TAMFLO)
004700011017     D DSTA01        e ds                  inz
004800011121     D*
004900011121     D* Parametri
005000011121     D ParmDS          ds
005100011121     D   pKSCda                1      7  0
005200011121     D   pKSCal                8     14  0
005300011017     D*
005400011017     D* Parametri per pgm. TNTA30R
005500011017     D TNTA30DS        ds                  inz
005600011017     D* Codice cliente vecchio
005700011017     D  oldksc                 1      7  0
005800011017     D* Progressivo tariffa vecchio
005900011017     D  oldprg                 8     10  0
006000011017     D* Codice cliente nuovo
006100011017     D  newksc                11     17  0
006200011017     D* Codice tariffa vecchio
006300011017     D  oldctr                21     23  0
006400011017     D* Codice tariffa nuovo
006500011017     D  newctr                24     26  0
006600011017     D* Codice divisa nuovo
006700011017     D  newdiv                27     29    inz('EUR')
006800011017     D* Data decorrenza tariffa
006900011018     D  newdto                30     37  0 inz(20020101)
007000011017     D* Data scadenza tariffa
007100011018     D  newsto                38     45  0 inz(20020101)
007200011017     D* Progressivo tariffa nuovo
007300011017     D  newprg                46     48  0
007400011018     D* Flag moneta (nuova) con decimali
007500011017     D  decima                49     49    inz('S')
007600011017     D* Arrotondamento per difetto o per eccesso
007700011017     D  arrde                 50     50
007800011017     D* Importo arrotondamento
007900011017     D  imparr                51     58  3
008000011017     D* Codice divisa vecchia
008100011017     D  olddiv                59     61    inz('ITL')
008200011017     D* Flag arrotondamento al decimale S/
008300011017     D  arrdec                62     62
008400011017     D* Numero decimale al quale arrotondare
008500011017     D  arrnrd                63     63  0
008600011017
008700011017      *------------------------------------------------------------------------*
008800011017      * VARIABILI
008900011017      *------------------------------------------------------------------------*
009000011017
009100011018     D* Indice messaggio di errore
009200011018     D X               s              3  0 inz
009300011018     D* Flag stampato almeno un errore
009400011017     D $Almeno1        s              1    inz(*OFF)
009500011018     D* Campi di stampa
009600011017     D wTime           s              6  0 inz
009700011018     D wErrTxt         s             85    inz
009800011018     D* Campi di lavoro
009900011017     D wKSC            s              3  0 inz
010000011018     D wERR            s              3    inz
010100011017
010200011017      *------------------------------------------------------------------------*
010300011017      * MAIN LINE
010400011017      *------------------------------------------------------------------------*
010500011017
010600011121      * Salvataggio dei parametri ricevuti
010700011121     C                   movel     KPJBA         saveKPJBA
010800011121      * Impostazione dei parametri ricevuti
010900011121     C                   movel     KPJBU         ParmDS
011000011121      *
011100011017      * Elaborazione del file TNTAM
011200011017      *
011300011121     C     pKSCda        setll     TNTAM000
011400011017     C                   read      TNTAM000
011500011017      *
011600011017IF  1C                   doW       %eof     =  *off
011700011121     C                             and TAMksc <= pKSCal
011800011017      *
011900011017IF  2C                   if        TAMdst   <= 20011231
012000011017      *
012100011017     C                   movel     TAMksc        wKSC
012200011017IF  3C                   if            wKSC <= 300
012300011017     C*** superfluo:               and wKSC <> 888
012400011017      *
012500011017     C                   movel     TAMflo        DSTA01
012600011017IF  4C                   if        §TAdiv   =  'ITL'
012700011017      *  creo nuovo progressivo
012800011017     C                   exsr      New_TNTAM
012900011017E   4C                   endif
013000011017      *
013100011017E   3C                   endif
013200011017      *
013300011017E   2C                   endif
013400011017      *
013500011017     C     k2tntam       setgt     TNTAM000
013600011017     C                   read      TNTAM000
013700011017      *
013800011017E   1C                   enddo
013900011017      *
014000011017      * The End !
014100011017     C                   exsr      ENDPGM
014200011017
014300011017      *------------------------------------------------------------------------*
014400011017      * Operazioni Iniziali
014500011017      *------------------------------------------------------------------------*
014600011017     c     *INZSR        BEGSR
014700011017      *
014800011017     c     *entry        plist
014900011017     c                   parm                    KPJBA
015000011017     c*
015100011017     c                   eval      *inOA    = *on
015200011017     c                   eval      $Almeno1 = *off
015300011017      *
015400011017     c     k2tntam       klist
015500011017     c                   kfld                    TAMksc
015600011017     c                   kfld                    TAMctr
015700011017      *
015800011017     c                   ENDSR
015900011017
016000011017      *------------------------------------------------------------------------*
016100011017      * Creo un nuovo progressivo con decorrenza 01/01/2002
016200011017      *                             e scadenza   01/01/2002 convertendo in EURO
016300011017      *------------------------------------------------------------------------*
016400011017     c     New_TNTAM     BEGSR
016500011017      *
016600011017     c                   reset                   TNTA30DS
016700011017     c* Codice cliente vecchio
016800011017     c                   eval      oldksc = TAMksc
016900011017     c* Codice tariffa vecchio
017000011017     c                   eval      oldctr = TAMctr
017100011017     c* Progressivo tariffa vecchio
017200011017     c                   eval      oldprg = TAMprg
017300011017     c* Codice cliente nuovo
017400011017     c                   eval      newksc = TAMksc
017500011017     c* Codice tariffa nuovo
017600011017     c                   eval      newctr = TAMctr
017700011017     c* Progressivo tariffa nuovo
017800011017     c                   eval      newprg = TAMprg + 1
017900011017     c*
018000011017     c                   movel(p)  TNTA30DS      KPJBU
018100011017     c                   call      'TNTA30R'
018200011017     c                   parm                    KPJBA
018300011017     c*
018400011018IF  1c                   if        %subst(KPJBU:254:3) <> '   '
018500011018     c                   move      KPJBU         wERR
018600011017     c                   exsr      Stampa_Err
018700011017E   1c                   endif
018800011017      *
018900011017     c                   ENDSR
019000011017
019100011017      *------------------------------------------------------------------------*
019200011017      * Stampa errore rilevato
019300011017      *------------------------------------------------------------------------*
019400011017     c     Stampa_ERR    BEGSR
019500011017      *
019600011017     c                   if        *inOA
019700011017     c                   time                    wTime
019800011017     c                   except    TESerr
019900011017     c                   eval      *inOA    = *off
020000011017     c                   eval      $Almeno1 = *on
020100011017     c                   endif
020200011018      *
020300011018     c                   clear                   wErrTxt
020400011018     c                   eval      X = 1
020500011018     c     wERR          lookup    ERR(X)                                 10
020600011018     c                   if        *in10
020700011018     c                   movel     MSG(X)        wErrTxt
020800011018     c                   else
020900011018     c                   movel     wERR          wErrTxt
021000011018     c                   endif
021100011017      *
021200011018     c                   if        wErrTxt <> *blanks
021300011017     c                   except    RIGerr
021400011017     c                   endif
021500011017      *
021600011017     c                   ENDSR
021700011017
021800011017      *------------------------------------------------------------------------*
021900011017      * Fine Programma
022000011017      *------------------------------------------------------------------------*
022100011017     c     ENDPGM        BEGSR
022200011017      *
022300011017     c                   if        $Almeno1 = *on
022400011017     c                   except    ENDerr
022500011017     c                   endif
022600011121      *
022700011121      * Ripristino dei parametri ricevuti
022800011121     c                   movel     saveKPJBA     KPJBA
022900011017      *
023000011017     c                   eval      *inLR = *on
023100011017      *
023200011017     c                   ENDSR
023300011017
023400011017      *------------------------------------------------------------------------*
023500011017      * STAMPE
023600011017      *------------------------------------------------------------------------*
023700011017
023800011017     OQSYSPRT   E            TESerr           02
023900011017     o                       *DATE         Y     11
024000011017     o                       WTIME         Y    + 2
024100011017     o                                           55 'Elenco ERRORI rilevati in '
024200011017     o                                          + 0 'fase di trascodifica tarif'
024300011017     o                                          + 0 'fe da LIRE ad EURO        '
024400011017     o                                          125 'Pag.'
024500011017     o                       PAGE          Z    + 0
024600011017     O          E            TESerr      1
024700011017     o                                           55 '=========================='
024800011017     o                                          + 0 '=========================='
024900011017     o                                          + 0 '==================        '
025000011017     O          E            TESerr      2
025100011017     o                                           20 '***** key *****'
025200011017     o                                          + 5 'Errore'
025300011017     O          E            TESerr      1  1
025400011017     o                                           20 '---------------'
025500011017     o                                          + 5 '------'
025600011017     O          E            RIGerr      1
025700011017     o                       TAMksc        z    + 5
025800011017     o                       TAMctr        z    + 1
025900011017     o                       TAMprg        z    + 1
026000011018     o                       wErrTxt            + 5
026100011017     O          E            ENDerr      2
026200011017     o                                           30 '***   Fine Lista   ***   '
026300011017** ERR / MSG
026400011017ERR CODICE DIVISA ERRATA
026500011017SCA DATE SCADENZA E/O DECORRENZA DELLA TARIFFA OLTRE LE DATE LIMITE DELLA TAB. "CV"
