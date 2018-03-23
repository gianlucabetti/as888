000100010920      *------------------------------------------------------------------------*
000200010920      *                                                                        *
000300010920      * Impostazione (a "ITL") della divisa nella tariffa (se *blanks)         *
000400010920      *              dei giorni di rientro nelle tariffe giacenze se > del     *
000500010920      *               campo di confronto: §2GGDP in caso di tariffa DPD        *
000600010920      *                                   §2GGRE in caso di tariffa Estera     *
000700010920      * negli archivi:                                                         *
000800010920      *  - TNTAM00F: parte del campo TAMFLO (v. ds DSTA01).                    *
000900010920      *  - TITGC00F: campo TGCGGR (v. limiti indicati in tab. "2G").           *
001000010920      * oppure:                                                                *
001100010920      *  - TNOFM00F: "copia" dell'archivio TNTAM01L.                           *
001200010920      *  - TIOGC00F: "copia" dell'archivio TITGC01L.                           *
001300010920      *                                                                        *
001400010920      * ELABORA ARCHIVI DIVERSI A SECONDA DEL PARAMETRO PASSATO.               *
001500010920      *                 ¯¯¯¯¯¯¯                                                *
001600010920      * Ha solo due archivi in input/output :  TNTAM01L  &  TITGC01L.          *
001700010920      * Occorre un'eventuale OVERRIDE per far puntare questo programma agli    *
001800010920      *   archivi alternativi (che hanno lo stesso tracciato record).          *
001900010920      *                                                                        *
002000010920      * NOTA BENISSIMO :                                                       *
002100010920      * ================                                                       *
002200010920      * 1) Questo programma deve girare in tutti i P.O. (in entrambi i S.I.).  *
002300010920      * 2) Prima di eseguire codesto pgm. occorre verificare la libreria di    *
002400010920      *    provenienza degli archivi che risultano in fase di aggiornamento:   *
002500010920      *    TNTAM01L & TITGC01L oppure FNOFM01L & TIOGC01L.                     *
002600010920      * 3) E' consigliato salvare tali archivi (fisici) in altra libreria o    *
002700010920      *    con altro nome.                                                     *
002800010920      *                                                                        *
002900010920      *------------------------------------------------------------------------*
003000010920
003100010920     H DECEDIT('0,') DATEDIT(*DMY.)
003200010920
003300010920      *------------------------------------------------------------------------*
003400010920      * ARCHIVI
003500010920      *------------------------------------------------------------------------*
003600010920
003700010920     F* - Tariffe (o Offerte) Clienti - Testata
003800010920     FTNTAM01L  UF   E           K DISK
003900010920     F* - Tariffe (o Offerte) Giacenza
004000010926     FTITGC01L  UF   E           K DISK
004100010920     F*
004200010920     FTABEL00F  IF   E           K DISK
004300010920     F*
004400010920     F* - Stampa eventuali errori
004500010920     FQSYSPRT   O    F  132        PRINTER oflind(*inOA)
004600010920
004700010920      *------------------------------------------------------------------------*
004800010920      * SCHIERE
004900010920      *------------------------------------------------------------------------*
005000010920
005100010920     D* Messaggi di errore in stampa
005200010920     D MSG             s             80    dim(2) ctdata perrcd(1)
005300010920
005400010920      *------------------------------------------------------------------------*
005500010920      * STRUTTURE DATI
005600010920      *------------------------------------------------------------------------*
005700010920
005800010920     D* Flag operativi di TNTAM (campo TAMFLO)
005900010920     D DSTA01        e ds                  inz
006000010920     D* FTabella "2G"
006100010920     D DS2G          e ds                  inz
006200010920
006300010920      *------------------------------------------------------------------------*
006400010920      * VARIABILI
006500010920      *------------------------------------------------------------------------*
006600010920
006700010920     D* - Flag richiesto controllo/aggiornamento del file TITGC
006800010920     D $TITGC          s              1    inz(*OFF)
006900010920     D* - Flag elaborazione in errore
007000010920     D $ERR            s              3  0 inz
007100010920     D* - Flag stampato almeno un errore
007200010920     D $Almeno1        s              1    inz(*OFF)
007300010920     D* - Campi di lavoro
007400010920     D w010an          s             10    inz
007500010920     D* - Campi di stampa
007600010920     D wTime           s              6  0 inz
007700010920     D errFile         s             10    inz
007800010920
007900010920      *------------------------------------------------------------------------*
008000010920      * COSTANTI
008100010920      *------------------------------------------------------------------------*
008200010920
008300010920     D* - Divisa/Valuta da impostare
008400010920     D wDIV            c                   const('ITL')
008500010920
008600010920      *------------------------------------------------------------------------*
008700010920      * MAIN LINE
008800010920      *------------------------------------------------------------------------*
008900010920
009000010920      * Elaborazione del file TNTAM
009100010920      *
009200010920     C     *loval        setll     TNTAM000
009300010920     C                   read      TNTAM000
009400010920      *
009500010920     C                   doW       %eof   = *off
009600010920      *
009700010920     C***                if        TAMATB = *blanks
009800010920      *  aggiornamento file TNTAM
009900010920     C                   exsr      Upd_TNTAM
010000010920      *  elaborazione file TITGC
010100010920     C     $TITGC        casEQ     *on           Upd_TITGC
010200010920     C                   endcs
010300010920     C***                endif
010400010920      *
010500010920     C                   read      TNTAM000
010600010920      *
010700010920     C                   enddo
010800010920      *
010900010920      * The End !
011000010920     C                   exsr      ENDPGM
011100010920
011200010920      *------------------------------------------------------------------------*
011300010920      * Operazioni Iniziali
011400010920      *------------------------------------------------------------------------*
011500010920     c     *INZSR        BEGSR
011600010920      *
011700010920     c                   eval      *inOA    = *on
011800010920     c                   eval      $Almeno1 = *off
011900010920      *
012000010920      * aggancio tabella "2G"
012100010920     c                   eval      codut = 1
012200010920     c                   eval      kcod  = '2G'
012300010920     c                   eval      kkey  = '1'
012400010920     c     kTAB          chain     TABEL00F
012500010920     c                   if        %found
012600010920     c                   movel     TBLuni        DS2G
012700010920     c                   else
012800010920     c                   eval      $ERR = 1
012900010920     c                   exsr      Stampa_ERR
013000010920     c                   exsr      ENDPGM
013100010920     c                   endIF
013200011003      *
013300011003     c                   eval      ErrFILE = 'TITGC01L'
013400010920      *
013500010920     c     kTITGC        klist
013600010920     c                   kfld                    TAMksc
013700010920     c                   kfld                    TAMctr
013800010920     c                   kfld                    TAMprg
013900010920      *
014000010920     c     kTAB          klist
014100010920     c                   kfld                    codut             1 0
014200010920     c                   kfld                    kcod              2
014300010920     c                   kfld                    kkey              8
014400010920      *
014500010920     c                   ENDSR
014600010920
014700010920      *------------------------------------------------------------------------*
014800010920      * Impostazione valuta nel campo TAMFLO del file TNTAM (se *blanks)
014900010920      *------------------------------------------------------------------------*
015000010920     c     Upd_TNTAM     BEGSR
015100010920      *
015200010920     c                   movel     TAMflo        DSTA01
015300010920      *
015400010920      * controllo se impostata la divisa (ed eventualmente aggiorno)
015500010920     c                   if        §TAdiv = *blanks
015600010920     c                   eval      §TAdiv = wDIV
015700010920     c                   movel     DSTA01        TAMflo
015800010920     c                   UPDATE    TNTAM000
015900010920     c                   else
016000010920     c                   UNLOCK    TNTAM01L
016100010920     c                   endif
016200010920      *
016300010920      * controllo se richiesto l'aggancio alle tariffe di giacenza
016400010926     c                   if        §TADPD = 'S'  or  TAMfie = 'E'
016500010920     c                   eval      $TITGC =  *on
016600010921     c                   else
016700010921     c                   eval      $TITGC =  *off
016800010920     c                   endif
016900010920      *
017000010920     c                   ENDSR
017100010920
017200010920      *------------------------------------------------------------------------*
017300010920      * Controllo ed Aggiornamento giorni di rientro nella tariffa giacenze
017400010920      *------------------------------------------------------------------------*
017500010920     c     Upd_TITGC     BEGSR
017600010920      *
017700010920      * aggancio record corrispondente del file TITGC (key unique)
017800010920     c     kTITGC        chain     TITGC01L
017900010920      *
018000010920     c                   if        %found
018100010920      *
018200010920     c                   select
018300010920      *
018400010920      * controllo tariffa DPD
018500010920     c                   when      §TADPD = 'S'
018600010920     c                   if        TGCGGR > §2GGDP
018700010920     c                   z-add     §2GGDP        TGCGGR
018800010920     c                   else
018900010920     c                   eval      $TITGC =  *off
019000010920     c                   endIF
019100010920      *
019200010920      * controllo tariffa Estera
019300010920     c                   when      TAMFIE = 'E'
019400010920     c                   if        TGCGGR > §2GGRE
019500010920     c                   z-add     §2GGRE        TGCGGR
019600010920     c                   else
019700010920     c                   eval      $TITGC =  *off
019800010920     c                   endIF
019900010920      *
020000010920     c                   endsl
020100010920      *
020200010920     c                   else
020300010920      *
020400010920      * segnalazione errore in stampa (record non trovato - file TITGC)
020500010920     c                   eval      $ERR = 2
020600010920     c                   exsr      Stampa_ERR
020700010920     c                   eval      $TITGC =  *off
020800010920      *
020900010920     c                   endif
021000010920      *
021100010920     c                   if        $TITGC =  *on
021200010920     c                   UPDATE    TITGC000
021300010920     c                   else
021400010920     c                   UNLOCK    TITGC01L
021500010920     c                   endif
021600010920      *
021700010920     c                   ENDSR
021800010920
021900010920      *------------------------------------------------------------------------*
022000010920      * Stampa errore rilevato in conversione importi da LIRE a EURO
022100010920      *------------------------------------------------------------------------*
022200010920     c     Stampa_ERR    BEGSR
022300010920      *
022400010920     c                   if        *inOA
022500010920     c                   time                    wTime
022600010920     c                   except    TESerr
022700010920     c                   eval      *inOA    = *off
022800010920     c                   eval      $Almeno1 = *on
022900010920     c                   endif
023000010920      *
023100010920     c                   if        $ERR > 0
023200010920     c                   except    RIGerr
023300010920     c                   endif
023400010920      *
023500010920     c                   ENDSR
023600010920
023700010920      *------------------------------------------------------------------------*
023800010920      * Fine Programma
023900010920      *------------------------------------------------------------------------*
024000010920     c     ENDPGM        BEGSR
024100010920      *
024200010920     c                   if        $Almeno1 = *on
024300010920     c                   except    ENDerr
024400010920     c                   endif
024500010920      *
024600010920     c                   eval      *inLR = *on
024700010920      *
024800010920     c                   ENDSR
024900010920
025000010920      *------------------------------------------------------------------------*
025100010920      * STAMPE
025200010920      *------------------------------------------------------------------------*
025300010920
025400010920     OQSYSPRT   E            TESerr           02
025500010920     o                       *DATE         Y     11
025600010920     o                       WTIME         Y    + 2
025700010920     o                                           55 'Elenco ERRORI rilevati in '
025800010920     o                                          + 0 'fase di impostazione codic'
025900010920     o                                          + 0 'e divisa "ITL"            '
026000010920     o                                          125 'Pag.'
026100010920     o                       PAGE          Z    + 0
026200010920     O          E            TESerr      1
026300010920     o                                           55 '=========================='
026400010920     o                                          + 0 '=========================='
026500010920     o                                          + 0 '==============            '
026600010920     O          E            TESerr      2
026700010920     o                                           11 'Archivio  '
026800010921     o                                          + 5 '***** key *****'
026900010920     o                                          + 5 'Errore'
027000010920     O          E            TESerr      1  1
027100010920     o                                           11 '----------'
027200010921     o                                          + 5 '---------------'
027300010920     o                                          + 5 '------'
027400010920     O          E            RIGerr      1
027500010920     o                       ERRfile             11
027600010921     o                       TAMksc        z    + 5
027700010921     o                       TAMctr        z    + 1
027800010921     o                       TAMprg        z    + 1
027900010921     o                       MSG($ERR)          + 5
028000010920     O          E            ENDerr      2
028100010920     o                                           55 '***   Fine Lista   ***    '
028200010920** MSG
028300010920NON TROVATA TABELLA "2G", QUINDI NON FATTO NULLA !!!
028400011003NON TROVATO IL RECORD CORRISPONDENTE A QUELLO DEL FILE TNTAM01L
