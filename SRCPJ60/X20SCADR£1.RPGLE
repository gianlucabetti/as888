000100000000      *PARMS OPTION(*NOXREF) DFTACTGRP(*NO) BNDDIR(PJXBND) CVTOPT(*DATETIME)
000200130618>>>>> *PARMS OPTIMIZE(*FULL)  ACTGRP(*CALLER)
000300000000C1013 *PARMS
000400000000C1013 *PARMS
000500000000C1013 *PARMS
000600000000     H DECEDIT('0,') DATEDIT(*DMY.)
000700000000      ******************************************************
000800000000      * ATTENZIONE: creando il programma con il livello di ottimizzazione
000900000000      * *FULL il debug funziona male. Prima di avviare il debug per questo
001000000000      * programma, ricrearlo con OPTIMIZE(*NONE). Per ulteriori informazioni
001100000000      * leggere l'aiuto del parametro "Livello di ottimizzazione" del
001200000000      * comando CRTBNDRPG.
001300000000      ******************************************************
001400000000      *              C A L C O L O   S C A D E N Z A R I O
001500000000      ******************************************************
001600000000      *
001700000000      *
001800000000      ******************************************************
001900000000      *     SCOPO DEL PROGRAMMA :                          *
002000000000      ******************************************************
002100000000      *                                                    *
002200000000      * A) Il programma riceve tramite la DS X20SCADDS     *
002300000000      *    i seguenti parametri fondamentali, per          *
002400000000      *    gli altri  vedi la documentazione:              *
002500000000      *                                                    *
002600000000      *       - Codice societa             (x20societa)    *
002700000000      *       - Unit”                      (x20unita)      *
002800000000      *       - Capoconto                  (x20kcc)        *
002900000000      *       - Sottoconto                 (x20ksc)        *
003000000000      *       - Data partita               (x20dtpar)      *
003100000000      *       - Numero partita             (x20nrpar)      *
003200000000      *       - Serie  partita             (x20serpar)     *
003300000000      *       - Data di riferimento per calcolo scaduto e  *
003400000000      *         a scadere                  (x20dtsca)      *
003500000000      *       - Data di registrazione limite entro cui     *
003600000000      *         considerare i movimenti D  (x20dtregmd)    *
003700000000      *                                 P  (x20dtregmp)    *
003800000000      *                                 G  (x20dtregmg)    *
003900000000      *       - Movimenti desiderati       (x20def)        *
004000000000      *                                    (x20prov)       *
004100000000      *                                    (x20gest)       *
004200000000      *       - Elaborazione di pura partita (P)           *
004300000000      *         di partita/scadenza          (S) blank     *
004400000000      *         di partita/scadenza senza SCD(E)           *
004500000000      *                                    (x20parsca)     *
004600000000      *       - Modalit” elaborazione                      *
004700000000      *         normale                      (R) blank     *
004800000000      *         per bonifici              SCD(P)           *
004900000000      *                                    (x20elasca)     *
005000000000      *       - Richiesta parametro esteso                 *
005100000000      *         X20SCADXDS                   (S)           *
005200000000      *                                    (x20parscd)     *
005300000000      *       - Flag tipo elaborazione desiderata:         *
005400000000      *         R=scrittura su file residente/T=scrittura  *
005500000000      *         su file temporaneo/' '=restituzione        *
005600000000      *         parametri relativi alla richiesta fatta    *
005700000000      *                                    (x20write)      *
005800000000      *       - Flag di modo chiusura programma            *
005900000000      *         L=LR / R=RT                (x20lrt)        *
006000000000      *                                                    *
006100000000      *       - Flag di inclusione/esclusione causali      *
006200000000      *         tecniche (tipo bonifici)                   *
006300000000      *  *on  o E=escludere le causali tecniche            *
006400000000      *  *off o blank=leggere tutto        (x20cautec)     *
006500000000      *                                                    *
006600000000      *       - x20FEOD se 'N' no FEOD su archivi          *
006700000000      *         in scrittura /SCD/SCP                      *
006800000000      *                                                    *
006900000000      *                                                    *
007000000000      *                                                    *
007100000000      * B) Dopo la lettura dei movimenti contabili         *
007200000000      *    relativi alla richiesta fatta vengono           *
007300000000      *    restituiti tramite  X20SCADDS   i seguenti      *
007400000000      *    parametri:                                      *
007500000000      *                                                    *
007600000000      *       - Totale Dare  m.c.  mov.D    x20tdld        *
007700000000      *                Avere m.c.      P                   *
007800000000      *                      div.      G                   *
007900000000      *                                                    *
008000000000      *       - Importo scaduto m.c.        x20lscadu      *
008100000000      *       - Importo a scadere m.c.      x20lascad      *
008200000000      *       - Importo rischio                            *
008300000000      *       - Importo esposizione                        *
008400000000      *       - Importo portafoglio                        *
008500000000      *                                                    *
008600000000      *       - Flag stato elaborazione    (x20errori)     *
008700000000      *                                                    *
008800000000      ******************************************************
008900000000      ******************************************************
009000000000      *  FUNZIONE DEGLI INDICATORI :                       *
009100000000      ******************************************************
009200000000      *                                                    *
009300000000      * 28=OPERAZIONI SU NDPPA01L/03L                      *
009400000000      * 29=OPERAZIONI SU NDPAS04L                          *
009500000000      * 30=OPERAZIONI SU ANOPE01L                          *
009600000000      * 31=OPERAZIONI SU NDEFA01L                          *
009700000000      *    OPERAZIONI SU NDEFP01L                          *
009800000000      * 32=INDICATORE PER LOOKUP                           *
009900000000      * 33=OPERAZIONI SU NDSCP01L                          *
010000000000      *    OPERAZIONI SU NDSCD01L                          *
010100000000      * 35=OPERAZIONI SU NDMOV01L                          *
010200000000      *                                                    *
010300000000      ******************************************************
010400000000     H/EJECT
010500000000      ******************************************************
010600000000      *  DATA BASE                                         *
010700000000      ******************************************************
010800000000     FNDPPA01L  IF   E           K DISK    USROPN
010900000000     F                                     RENAME(NDPPA000:NDPPA1)
011000000000      * Vista logica completa
011100000000     FNDPPA03L  IF   E           K DISK    USROPN
011200000000      * Vista logica sole partite aperte
011300000000     FNDPAS04L  IF   E           K DISK    USROPN
011400000000     FNDMOV01L  IF   E           K DISK    USROPN
011500000000     FNDEFA01L  IF   E           K DISK    USROPN
011600000000     FNDEFP01L  IF   E           K DISK    USROPN
011700000000     FNDSCP01L  UF A E           K DISK    USROPN
011800000000     FNDSCD01L  UF   E           K DISK    USROPN
011900000000     FNDSCD00F  O  A E             DISK    USROPN
012000000000     F                                     RENAME(NDSCD000:NDSCD2)
012100000000D1955FNDMRA04L  IF   E           K DISK
012200000000     F/EJECT
012300000000     D* Reperimento nome PGM
012400000000     D STATUS         SDS           333
012500000000     D  STSUSR               254    263
012600000000     D  DSPGM            *PROC
012700000000     D  PARMS            *PARMS
012800000000      ******************************************************
012900000000      * DS per scomposizione dati
013000000000      ******************************************************
013100000000      *
013200000000     D SCDFLG          DS
013300000000     D  SCDFLG1                       1A
013400000000     D  SCDFLG2                       1A
013500000000     D  SCDFLG3                       1A
013600000000     D  SCDFLG4                       7A
013700000000     D SCPFLG          DS
013800000000     D  SCPFLG1                       1A
013900000000     D  SCPFLG2                       1A
014000000000     D  SCPFLG3                       1A
014100000000     D  SCPFLG4                       7A
014200000000      ******************************************************
014300000000      *  SCHIERE                                           *
014400000000      ******************************************************
014500000000     D CODRIS          S              4    DIM(40)                              Causali rischio
014600000000     D DATRIS          S             10A   DIM(40)                              Date rischio
014700000000      *
014800000000D2246D DSCMP           S             11    DIM(100) INZ(*HIVAL)                 Scadenze
014900000000D2246D DSCMD           S             11    DIM(100) INZ(*HIVAL)                 Scadenze
015000000000     D DSC             S             10    DIM(100) INZ(*HIVAL)                 Scadenze
015100000000     D IML             S             19S 3 DIM(100)                             Importi in m.c.
015200000000     D IMD             S             19S 3 DIM(100)                             Importi in divisa
015300000000     D CAU             S              4    DIM(100)                             Causali rate
015400000000D2085D MD              S              1    DIM(100)                             Causali rate
015500000000D2085D MP              S              1    DIM(100)                             Causali rate
015600000000     D NAS             S             17    DIM(100)                             Sys + NrAsReg
015700000000     D NRT             S              3S 0 DIM(100)                             Numero rata
015800000000D2085x*SORT            S             72    DIM(100) inz(*hival)
015900000000D2085D SORT            S             74    DIM(100) inz(*hival)
016000000000      *
016100000000D2085X*sortds          DS            72    inz
016200000000D2085D sortds          DS            74    inz
016300000000     D sortDSC                 1     10
016400000000     D sortIML                11     29S 3
016500000000     D sortIMD                30     48S 3
016600000000     D sortCAU                49     52
016700000000     D sortNAS                53     69
016800000000     D sortNRT                70     72S 0
016900000000D2085D sortMD                 73     73
017000000000D2085D sortMP                 74     74
017100000000     D nasds           DS            17    inz
017200000000     D  nasds1                 1      3S 0
017300000000     D  nasds2                 4     12S 0
017400000000     D  nasds3                13     17S 0
017500000000      *
017600000000     D DLP             S             19  3 DIM(100)                             Dare PF in m.c.
017700000000     D DDP             S             19  3 DIM(100)                             Dare PF divisa
017800000000     D DSP             S             10A   DIM(100)                             Dare PF scadenze
017900000000     D DCAP            S              4A   DIM(100)                             Dare PF causali
018000000000     D DRDP            S              1A   DIM(100)                             Dare PF mod. R.D.
018100000000D1557D DMDP            S              1A   DIM(100)                             Dare PF mod. mdpag
018200000000     D ALP             S             19  3 DIM(100)                             Avere PF in m.c.
018300000000     D ADP             S             19  3 DIM(100)                             Avere PF divisa
018400000000     D ASP             S             10A   DIM(100)                             Avere PF scadenze
018500000000     D ACAP            S              4A   DIM(100)                             Avere PF causali
018600000000     D ARDP            S              1A   DIM(100)                             Avere PF mod. R.D.
018700000000D1557D AMDP            S              1A   DIM(100)                             Avere PF mod. mdpag
018800000000     D Anope         E DS                  extname(anope00f) inz
018900000000     D******************************************************
019000000000     D*  Indici schiere                                    *
019100000000     D******************************************************
019200000000    >D A               S              3  0
019300000000    >D I               S              3  0
019400000000    >D T               S              3  0
019500000000    >D TX              S              3  0
019600000000    >D S               S              3  0
019700000000    >D O               S              3  0
019800000000     D******************************************************
019900000000     D*  Campi di lavoro                                   *
020000000000     D******************************************************
020100000000D1955D wcflgrit        S              1    inz(' ')
020200000000D1955D wpflgrit        S              1    inz(' ')
020300000000    >D wrksldt2l       S             19  3
020400000000    >D wrksldt2d       S             19  3
020500000000    >D wrkdtsca        S                   like(pasdtsca)
020600000000C1013D wrkin01         S              1
020700000000C1013D wrkinris        S              1
020800000000C1013D wrkndmov        S              1
020900000000    >D wcautec         S              1    inz('0')
021000000000     DSocieta          S                   like(passocieta)
021100000000     DCausale          S                   like(pascausrig)
021200000000     DData             S             10
021300000000     DRefresh          S              1
021400000000     DEsito            S              1
021500000000     DGesMsg           S              1
021600000000     DOutput           S           4000
021700000000     DLenOut           S              9B 0
021800000000     DStrutturaO       S             10
021900000000     D******************************************************
022000000000     D*  date di lavoro                                    *
022100000000     D******************************************************
022200000000    >Dkeydtpar         S             10A                                        work
022300000000    >DSavDtsca         S             10A                                        work
022400000000    >DWUDATE           S             10A                                        work
022500000000    >DWDtris           S             10A                                        work
022600000000    >DWdumd            S             10A                                        work
022700000000    >DWdump            S             10A                                        work
022800000000    >DWdumg            S             10A                                        work
022900000000    >DWgdata           S             10A                                        work
023000000000    >DWdtscamin        S             10A                                        data scadenza minima
023100000000     DDATMIN           C                   const('0001-01-01')
023200000000     DDATMAX           C                   const('9999-12-31')
023300000000     DDAT1940          C                   const('1940-01-01')
023400000000     D******************************************************
023500000000     Dskalf            S             10A   dim(40)
023600000000     DDATEDA           S             10A   dim(8)
023700000000     DDATEA            S             10A   dim(8)
023800000000     DIMPcf            S             19  3 DIM(8)                               IMPORTI IN LIRE
023900000000     DIMDCF            S             19  3 DIM(8)                               IMPORTI IN LIRE
024000000000     D******************************************************
024100000000     D*  STRUTTURE DATI                                    *
024200000000     D******************************************************
024300000000      * DS parametri ricevuti   da programma chiamante
024400000000      *              restituiti  a programma chiamante
024500000000C1013D X20SCADDS     E DS                  extname(x20scadds)
024600000000     D  X20FLGTPSC                    1    overlay(x20flg:1)                    Scadenz. retroattivo
024700000000     D  X20FLG9                       9    overlay(x20flg:2)                    Scadenz. retroattivo
024800000000      *
024900000000      * DS parametro esteso se richiesto x20parscd='S'
025000000000C1013D X20SCADXDS    E DS                  extname(x20scadxds)
025100000000      * schiere per parametro esteso
025200000000     D DTSCAX                        10A   DIM(50) overlay(x20dtscax)           Scadenze
025300000000     D IMPRMC                        19  3 DIM(50) overlay(x20imprmc)           Importi in m.c.
025400000000     D IMPRD                         19  3 DIM(50) overlay(x20imprd)            Importi in m.c.
025500000000     D IMPOMC                        19  3 DIM(50) overlay(x20impomc)           Importi in divisa
025600000000     D IMPOD                         19  3 DIM(50) overlay(x20impod)            Importi in divisa
025700000000     D CAUS1                          4A   DIM(50) overlay(x20caus1)            Causali rate
025800000000     D CAUS2                          4A   DIM(50) overlay(x20caus2)            Causali rate
025900000000      *
026000000000      * DS parametri per CALL a PGM X19DTRISR
026100000000C1013D X19DTRISDS    E DS                  extname(x19dtrisds)
026200000000      *
026300000000      * DS parametri per CALL a PGM X18DTSCA
026400000000C1013D X18DTSCADS    E DS                  extname(x18dtscads)
026500000000      *
026600000000C1309D ZDateISO        S               D
026700000000      /EJECT
026800000000      *
026900000000      ******************************************************
027000000000      * FLUSSO PRINCIPALE DEL PROGRAMMA
027100000000      ******************************************************
027200000000      *
027300000000      *------------------------
027400000000      * Parametri d'ingresso
027500000000      *------------------------
027600000000      *
027700000000     C     *ENTRY        PLIST
027800000000     C                   PARM                    X20SCADDS
027900000000     C                   PARM                    X20SCADXDS
028000000000
028100000000C1024C**                 clear                   x20errori
028200000000      *
028300000000      * Reperimento contabilit” mancante
028400000000      *
028500000000     C     x20ctb        IFEQ      *blanks
028600000000     C                   MOVE      'CG'          x20ctb
028700000000     C                   END
028800000000      *
028900000000     C     x20flgtpsc    ifeq      *blanks
029000000000     C                   eval      x20flgtpsc    = *off
029100000000     C                   endif
029200000000      *
029300000000     C                   select
029400000000     C     x20cautec     wheneq    *blanks
029500000000     C                   eval      x20cautec = *off
029600000000     C     x20cautec     wheneq    'E'
029700000000     C                   eval      x20cautec = *on
029800000000     C                   endsl
029900000000      *
030000000000      *------------------------
030100000000      * Operazioni preliminari
030200000000      *------------------------
030300000000      * Se siamo in scrittura
030400000000      *
030500000000     C     x20write      IFNE      *BLANK
030600000000      * Se ﬁ stata richiesta un'opzione di scrittura diversa da quella
030700000000      * della precedente chiamata chiude i file per ricreare la ODP
030800000000      * sui file scadenziario della libreria opportuna
030900000000     C     x20write      IFNE      SAVWRT
031000000000     C     W1SCA         IFNE      *BLANK
031100000000     C                   CLOSE     NDSCP01L
031200000000     C                   CLOSE     NDSCD01L
031300000000     C                   CLOSE     NDSCD00F
031400000000     C                   MOVE      *BLANK        W1SCA
031500000000     C                   END
031600000000     C                   MOVE      x20write      SAVWRT
031700000000     C                   END
031800000000      * Apertura files scadenziario
031900000000     C     W1SCA         IFEQ      *BLANK
032000000000     C                   OPEN      NDSCP01L
032100000000     C                   OPEN      NDSCD01L
032200000000     C                   OPEN      NDSCD00F
032300000000     C                   MOVE      'Y'           W1SCA             1
032400000000     C                   END
032500000000      * Nel caso di scrittura su file residente verifica se il conto
032600000000      * richiesto non ﬁ gi” in fase di aggiornamento
032700000000     C     x20KCC        IFNE      *blanks
032800000000     C     x20write      ANDEQ     'R'
032900000000     C                   EXSR      TESTCONTO
033000000000      * Forza elaborazione su tutte le unit”
033100000000     C                   CLEAR                   x20unita
033200000000     C                   END
033300000000      *
033400000000      * Cancella precedente situazione scadenze e partite
033500000000     C     x20KCC        IFNE      *blanks
033600000000     C                   EXSR      DLTSCDSCP
033700000000     C                   END
033800000000      *
033900000000     C                   END
034000000000      *
034100000000      * Fine x20write NE *blanks
034200000000      *
034300000000      *---------------------------------------------------
034400000000      * Reperimento dati mancanti, o forzature dati errati
034500000000      *---------------------------------------------------
034600000000      *
034700000000     C     x20dtregmd    IFEQ      DATMIN
034800000000     C                   MOVE      WUDATE        x20dtregmd
034900000000     C                   END
035000000000      *
035100000000     C     x20prov       IFEQ      '1'
035200000000     C     x20dtregmp    ANDEQ     DATMIN
035300000000     C                   MOVE      x20dtregmd    x20dtregmp
035400000000     C                   END
035500000000      *
035600000000     C     x20gest       IFEQ      '1'
035700000000     C     x20dtregmg    ANDEQ     DATMIN
035800000000     C                   MOVE      x20dtregmd    x20dtregmg
035900000000     C                   END
036000000000      *
036100000000     C     x20dtsca      IFEQ      DATMIN
036200000000     C                   MOVE      x20dtregmd    x20dtsca
036300000000     C                   END
036400000000      *
036500000000      *---------------------------
036600000000      * Calcolo date rischio
036700000000      *---------------------------
036800000000      *
036900000000     C     x20dtsca      IFNE      SavDtsca
037000000000     C                   EXSR      CALRISCHIO
037100000000     C                   MOVE      x20dtsca      SavDtsca
037200000000     C                   END
037300000000      *
037400000000      *---------------------------
037500000000      * Calcolo date griglia
037600000000      *---------------------------
037700000000      *
037800000000     C     x20tabcf      IFNE      *blanks
037900000000     C     x20tabcf      ANDNE     savtabcf
038000000000     C                   EXSR      CALDTSCA
038100000000     C                   MOVE      x20tabcf      savtabcf
038200000000     C                   END
038300000000      *
038400000000      *---------------------------
038500000000      * Elaborazione
038600000000      *---------------------------
038700000000      *
038800000000      * Se ho un capoconto valido
038900000000     C     x20KCC        IFNE      *blanks
039000000000      *
039100000000      *    Azzera dati conto
039200000000     C                   EXSR      AZZCONTO
039300000000      *
039400000000      *    Lettura partite e movimenti
039500000000      *
039600000000     C                   EXSR      READNDPPA
039700000000      *
039800000000      *    Chiusura elaborazione
039900000000      *       Imposta parametri di ritorno
040000000000      *
040100000000     C                   EXSR      PARRITORNO
040200000000      *
040300000000      * Nel caso di scrittura su file residente sblocca record conto
040400000000      * per permettere aggiornamenti successivi
040500000000      *
040600000000     C     x20write      CASEQ     'R'           RILCONTO
040700000000     C                   END
040800000000      *
040900000000     C                   END
041000000000      *
041100000000      * Chiude programma in base al flag di chiusura
041200000000      *
041300000000     C     x20LRT        IFEQ      'L'
041400000000     C                   SETON                                        LR
041500000000     C                   ELSE
041600000000     C     x20write      IFNE      *BLANK
041700000000     C     x20feod       ANDEQ     *BLANK
041800000000     C                   FEOD      NDSCP01L
041900000000     C                   FEOD      NDSCD00F
042000000000     C                   END
042100000000     C                   RETURN
042200000000     C                   END
042300000000      *
042400000000     C/EJECT
042500000000      ******************************************************
042600000000      *  Pulizia campi del conto                           *
042700000000      ******************************************************
042800000000     C     AZZCONTO      BEGSR
042900000000      *
043000000000     C                   CLEAR                   wctdld
043100000000     C                   CLEAR                   wctald
043200000000     C                   CLEAR                   wctddd
043300000000     C                   CLEAR                   wctadd
043400000000     C                   CLEAR                   wctdlp
043500000000     C                   CLEAR                   wctalp
043600000000     C                   CLEAR                   wctddp
043700000000     C                   CLEAR                   wctadp
043800000000     C                   CLEAR                   wctdlg
043900000000     C                   CLEAR                   wctalg
044000000000     C                   CLEAR                   wctddg
044100000000     C                   CLEAR                   wctadg
044200000000      *
044300000000     C                   CLEAR                   wclscaduto
044400000000     C                   CLEAR                   wcdscaduto
044500000000     C                   CLEAR                   wclascader
044600000000     C                   CLEAR                   wcdascader
044700000000     C                   CLEAR                   wcportaf
044800000000     C                   CLEAR                   wcdportaf
044900000000     C                   CLEAR                   wcrisc
045000000000     C                   CLEAR                   wcdrisc
045100000000     C                   CLEAR                   wcscains
045200000000     C                   CLEAR                   wcdscains
045300000000D1955C                   CLEAR                   wcflgrit
045400000000      *
045500000000      * Griglia scadenze (se richiesta)
045600000000      *
045700000000     C     x20tabcf      IFNE      *blanks
045800000000     C                   CLEAR                   IMPcf
045900000000     C                   CLEAR                   imdcf
046000000000     C                   ENDIF
046100000000      *
046200000000     C                   ENDSR
046300000000     C/EJECT
046400000000      ******************************************************
046500000000      *  Pulizia campi di partita                          *
046600000000      ******************************************************
046700000000     C     AZZPARTITA    BEGSR
046800000000      * Azzera campi di accumulo
046900000000     C                   CLEAR                   wptdld
047000000000     C                   CLEAR                   wptald
047100000000     C                   CLEAR                   wptdlp
047200000000     C                   CLEAR                   wptalp
047300000000     C                   CLEAR                   wptdlg
047400000000     C                   CLEAR                   wptalg
047500000000     C                   CLEAR                   wptddd
047600000000     C                   CLEAR                   wptadd
047700000000     C                   CLEAR                   wptddp
047800000000     C                   CLEAR                   wptadp
047900000000     C                   CLEAR                   wptddg
048000000000     C                   CLEAR                   wptadg
048100000000      * Azzera saldo movimenti di tipo 2
048200000000     C                   clear                   wrksldt2l
048300000000     C                   clear                   wrksldt2d
048400000000      * Azzera data registrazione pi® alta reperita
048500000000     C                   MOVE      DATMIN        Wdumd
048600000000     C                   MOVE      DATMIN        Wdump
048700000000     C                   MOVE      DATMIN        Wdumg
048800000000      * Azzera data scadenza minima
048900000000     C                   MOVE      DATMIN        Wdtscamin
049000000000      * Azzera importo movimenti PF DARE E AVERE
049100000000     C                   CLEAR                   wivadare
049200000000     C                   CLEAR                   wivaaver
049300000000      * Azzera importo insoluti
049400000000     C                   CLEAR                   wimpins
049500000000      * Azzera importo movimenti incasso/pagamento di partita
049600000000     C                   CLEAR                   wpaginc
049700000000      * Azzera flag portafoglio
049800000000     C                   CLEAR                   wportafo
049900000000      * Inizializza divisa di partita
050000000000     C                   MOVE      ppadivisa     wdivisa
050100000000      * Azzera campi di accumulo per scaduto e a scadere
050200000000     C                   reset                   DSC
050300000000D2246C                   clear                   DSCMP
050400000000D2246C                   clear                   DSCMD
050500000000     C                   CLEAR                   IML
050600000000     C                   CLEAR                   IMD
050700000000     C                   CLEAR                   CAU
050800000000D2085C                   CLEAR                   MD
050900000000D2085C                   CLEAR                   MP
051000000000     C                   CLEAR                   NAS
051100000000     C                   CLEAR                   DLP
051200000000     C                   CLEAR                   DDP
051300000000     C                   CLEAR                   DSP
051400000000     C                   CLEAR                   DCAP
051500000000     C                   CLEAR                   DRDP
051600000000D1557C                   CLEAR                   DMDP
051700000000     C                   CLEAR                   ALP
051800000000     C                   CLEAR                   ADP
051900000000     C                   CLEAR                   ASP
052000000000     C                   CLEAR                   ACAP
052100000000     C                   CLEAR                   ARDP
052200000000D1557C                   CLEAR                   AMDP
052300000000      * Scaduto/a scadere ecc.
052400000000     C                   CLEAR                   wplscaduto
052500000000     C                   CLEAR                   wpdscaduto
052600000000     C                   CLEAR                   wplascader
052700000000     C                   CLEAR                   wpdascader
052800000000     C                   CLEAR                   wpportaf
052900000000     C                   CLEAR                   wpdportaf
053000000000     C                   CLEAR                   wprisc
053100000000     C                   CLEAR                   wpdrisc
053200000000     C                   CLEAR                   wpscains
053300000000     C                   CLEAR                   wpdscains
053400000000     C                   CLEAR                   wplincascp
053500000000     C                   CLEAR                   wpdincascp
053600000000     C                   CLEAR                   wplincascd
053700000000     C                   CLEAR                   wpdincascd
053800000000      * Memorizzatori indici / swicth
053900000000D2085C                   Z-ADD     0             SAVIDA            3 0          * Assoluto
054000000000     C                   Z-ADD     1             IDA               3 0          * Assoluto
054100000000     C                   Z-ADD     1             IDAR              3 0          * Assoluto
054200000000     C                   Z-ADD     1             IAVE              3 0          * Assoluto
054300000000     C                   clear                   wmaxida           3 0
054400000000      * Gestione parametro esteso
054500000000     C     x20parscd     IFEQ      'S'
054600000000     C                   CLEAR                   INDEX             3 0          * Assoluto
054700000000     C                   CLEAR                   x20scadxds                     * Assoluto
054800000000     C                   ENDIF
054900000000      *
055000000000     C                   CLEAR                   wnrmov
055100000000D2496x* nuovi campi per memorizzare causale generante fattura
055200000000  "  x* ma non vanno puliti!!
055300000000  "  x* ex d2247         CLEAR                   CFACODSav         4            PF avere
055400000000D2496x* ex d2247         CLEAR                   CFDCODSav         4            PF dare
055500000000D2496C                   clear                   ScdCaus1
055600000000D2496C                   clear                   ScdCaus2
055700000000      *
055800000000     C                   ENDSR
055900000000     C/EJECT
056000000000      ******************************************************
056100000000      *  LETTURA GUIDA PARTICOLARITA PARTITE               *
056200000000      ******************************************************
056300000000     C     READNDPPA     BEGSR
056400000000      *
056500000000     C                   MOVE      x20KCC        keykcc
056600000000     C                   MOVE      x20KSC        keyksc
056700000000     C                   MOVE      x20dtpar      ppadtpar
056800000000     C                   Z-ADD     x20nrpar      ppanrpar
056900000000     C                   MOVE      x20serpar     ppaserpar
057000000000      *
057100000000      * L'elaborazione ﬁ per partita
057200000000      *
057300000000     C     x20dtpar      IFNE      DATMIN
057400000000     C     x20nrpar      ANDNE     *zeros
057500000000     C                   CLEAR                   ppasys
057600000000     C                   CLEAR                   ppanrasint
057700000000     C                   CLEAR                   ppadivisa
057800000000     C                   CLEAR                   ppaunita
057900000000      *
058000000000      * Accedo a NDPPA secondo la richiesta del parametro
058100000000      * e non faccio comunque alcuna selezione
058200000000      *
058300000000     C     x20ppa        IFNE      'N'
058400000000     C                   IF        filppa1opn <> *on
058500000000     C                   OPEN      NDPPA01L
058600000000     C                   MOVE      *on           filppa1opn        1
058700000000     C                   ENDIF
058800000000     C     kppapartit    CHAIN     NDPPA01L                           28
058900000000     C                   Z-ADD     ppasys        x20sysori
059000000000     C                   Z-ADD     ppanrasint    x20nrasint
059100000000     C                   END
059200000000     c*
059300000000     C                   EXSR      READNDPAS
059400000000      *
059500000000D1955 * Memorizza flag esistenza ritenuta per partita/conto
059600000000  "   *
059700000000  "  C     X20Snatura    ifeq      'F'
059800000000  "  c     Wnrmov        andgt     0
059900000000  "  C                   eval      mradrp  = ppadtpar
060000000000  "  C                   eval      mranrp  = ppanrpar
060100000000  "  C                   eval      mraserpar = ppaserpar
060200000000  "  C                   EXSR      MemRitenut
060300000000D1955C                   endif
060400000000      *
060500000000      *
060600000000     C                   ELSE
060700000000      *
060800000000      * L'elaborazione ﬁ per conto
060900000000      *
061000000000     C                   IF        x20dtregmd < wudate
061100000000     C                   IF        filppa1opn <> *on
061200000000     C                   OPEN      NDPPA01L
061300000000     C                   MOVE      *on           filppa1opn        1
061400000000     C                   ENDIF
061500000000     C     kppaconto     CHAIN     NDPPA01L                           28
061600000000     C                   ELSE
061700000000     C                   IF        filppa3opn <> *on
061800000000     C                   OPEN      NDPPA03L
061900000000     C                   MOVE      *on           filppa3opn        1
062000000000     C                   ENDIF
062100000000     C     kppaconto     CHAIN     NDPPA03L                           28
062200000000     C                   ENDIF
062300000000      *
062400000000     C     *IN28         DOWEQ     '0'
062500000000      *
062600000000      * leggere solo le partite volute
062700000000      *
062800000000     C                   MOVE      *on           valida            1
062900000000      *
063000000000     C     x20unita      IFNE      *blanks
063100000000     C     x20unita      ANDNE     ppaunita
063200000000     C                   MOVE      *off          valida
063300000000     C                   END
063400000000      *
063500000000     C     x20divr       IFNE      *blanks
063600000000     C     x20divr       ANDNE     ppadivisa
063700000000     C                   MOVE      *off          valida
063800000000     C                   END
063900000000      *
064000000000     C     ppaparchiu    IFEQ      *on
064100000000     C     x20dtregmd    ANDGT     ppadtchiu
064200000000     C                   MOVE      *off          valida
064300000000     C                   END
064400000000     c*
064500000000D1955 * Memorizza flag esistenza ritenuta per partita/conto
064600000000  "   *
064700000000  "  C     X20Snatura    ifeq      'F'
064800000000  "  C     valida        andeq     *on
064900000000  "  C                   eval      mradrp  = ppadtpar
065000000000  "  C                   eval      mranrp  = ppanrpar
065100000000  "  C                   eval      mraserpar = ppaserpar
065200000000  "  C                   EXSR      MemRitenut
065300000000D1955C                   endif
065400000000      *
065500000000      * leggo i movimenti della partita
065600000000      *
065700000000     C     valida        IFEQ      *on
065800000000     C                   EXSR      READNDPAS
065900000000     C                   END
066000000000      *
066100000000     C                   IF        x20dtregmd < wudate
066200000000     C     kppaconto     READE     NDPPA01L                               28
066300000000     C                   ELSE
066400000000     C     kppaconto     READE     NDPPA03L                               28
066500000000     C                   ENDIF
066600000000      *
066700000000     C                   END                                                    *IN28 DOWEQ '0'
066800000000      *
066900000000      *
067000000000     C                   END                                                    x20dtpar
067100000000      *
067200000000      * fine lettura
067300000000      *
067400000000     C                   ENDSR
067500000000     C/EJECT
067600000000     C******************************************************
067700000000     C*  LETTURA MOVIMENTI DI PARTITE                      *
067800000000     C******************************************************
067900000000     C     READNDPAS     BEGSR
068000000000      *
068100000000     C     kpaspartit    CHAIN     NDPAS04L                           29
068200000000      *
068300000000     C                   EXSR      AZZPARTITA
068400000000      *
068500000000     C     *IN29         DOWEQ     '0'
068600000000      *
068700000000      * leggere solo i movimenti voluti
068800000000      *
068900000000     C                   MOVE      *off          valido            1
069000000000      *
069100000000     C                   SELECT
069200000000      *
069300000000     C     pastpregz     WHENEQ    'D'
069400000000     C     x20def        ANDEQ     *on
069500000000     C     pasdtreg      IFLE      x20dtregmd
069600000000     C                   MOVE      *on           valido
069700000000     C                   END
069800000000      *
069900000000     C     pastpregz     WHENEQ    'P'
070000000000     C     x20prov       ANDEQ     *on
070100000000     C     pasdtreg      IFLE      x20dtregmp
070200000000     C                   MOVE      *on           valido
070300000000     C                   END
070400000000      *
070500000000     C     pastpregz     WHENEQ    'G'
070600000000     C     x20gest       ANDEQ     *on
070700000000     C     pasdtreg      IFLE      x20dtregmg
070800000000     C                   MOVE      *on           valido
070900000000     C                   END
071000000000      *
071100000000     C                   ENDSL
071200000000      *
071300000000     C* Se il movimento ﬁ valido
071400000000     C     valido        ifeq      *on
071500000000     C* reperisco e memorizzo l'anagrafico causale.
071600000000     C                   eval      societa = passocieta
071700000000     C                   eval      causale = pascausrig
071800000000     C                   exsr      callmvc003
071900000000     C     esito         ifne      *off
072000000000     C                   endif
072100000000      *
072200000000     C* Se il movimento ﬁ provvisorio o gestionale ...
072300000000     C* ... controllo i flag di esclusione delle causali tecniche.
072400000000     C                   eval      wcautec   = *off
072500000000     C     pastpregz     ifeq      'P'
072600000000     C     pastpregz     oreq      'G'
072700000000     C     opeescls      ifeq      *on
072800000000     C     x20cautec     andeq     *on
072900000000     C                   eval      wcautec   = *on
073000000000     C                   endif
073100000000     C                   endif
073200000000     C*
073300000000     C                   endif
073400000000      *
073500000000      * Memorizza dati partita del movimento valido
073600000000      *
073700000000     C     valido        IFEQ      *on
073800000000     C                   EXSR      MEMPARTITA
073900000000     C                   END
074000000000      *
074100000000      *
074200000000      * Lettura del file
074300000000      *
074400000000     C     kpaspartit    READE     NDPAS04L                               29
074500000000      *
074600000000     C                   END                                                    *IN29 DOWEQ '0'
074700000000      *
074800000000      * Rottura di partita, incremento i totali
074900000000      *
075000000000     C                   EXSR      INCCONTO
075100000000      *
075200000000     C                   ENDSR
075300000000     C/EJECT
075400000000     C******************************************************
075500000000     C*  MEMORIZZA DATI DI PARTITA                         *
075600000000     C******************************************************
075700000000     C     MEMPARTITA    BEGSR
075800000000      *
075900000000      * Aggiusto data scadenza vuota (conti non gestiti a scadenza)
076000000000     C     pasdtsca      ifeq      datmin
076100000000     C                   eval      pasdtsca = pasdtpar
076200000000     C                   endif
076300000000      *
076400000000      * Memorizza data registrazione pi® alta reperita
076500000000     C                   SELECT
076600000000      *
076700000000     C     pastpregz     WHENEQ    'D'
076800000000     C     pasdtreg      ANDGT     wdumd
076900000000     C                   MOVE      pasdtreg      wdumd
077000000000      *
077100000000     C     pastpregz     WHENEQ    'P'
077200000000     C     pasdtreg      ANDGT     wdump
077300000000     C                   MOVE      pasdtreg      wdump
077400000000      *
077500000000     C     pastpregz     WHENEQ    'G'
077600000000     C     pasdtreg      ANDGT     wdumg
077700000000     C                   MOVE      pasdtreg      wdumg
077800000000      *
077900000000     C                   ENDSL
078000000000      * Verifica/memorizza divisa di partita
078100000000     C     wdivisa       IFEQ      *blanks
078200000000     C     pasdivisa     ANDNE     *blanks
078300000000     C                   MOVE      pasdivisa     wdivisa
078400000000     C                   END
078500000000      *
078600000000      * Imposta data scadenza originale:
078700000000      * da questo momento in poi usare SCDDTSCA, che pu? venire manipolata.
078800000000     C                   eval      scddtsca      = pasdtsca
078900130618
079000130618>>>>>** Solo se sto calcolando lo scadenzario per la griglia di Amerighi
079100130618>>>>>** ricalcolo la data scadenza dell'insoluto sommando 15 giorni
079200130618>>>>>** alla data registrazione.
079300130618>>>>>C*    pascausrig    ifeq      'ADIN'
079400130618>>>>>C*    YOmEfaAsc     ANDEQ     *ON
079500130618>>>>>C*                  move      pasdtreg      dtreg10
079600130618>>>>>C*    dtreg10       adddur    15:*days      dtreg10
079700130618>>>>>C*                  move      dtreg10       scddtsca
079800130618>>>>>C*                  endif
079900130618
080000000000      *
080100000000      * Incrementa totali di partita
080200000000      *
080300000000     C                   SELECT
080400000000     C     pasdare       WHENEQ    1
080500000000     C                   EXSR      DARE
080600000000     C     pasavere      WHENEQ    1
080700000000     C                   EXSR      AVERE
080800000000     C                   ENDSL
080900000000      *
081000000000      * Salva data scadenza minima
081100000000     C     wdtscamin     IFEQ      DATMIN
081200000000     C                   MOVE      scddtsca      wdtscamin
081300000000     C                   END
081400000000     C*
081500000000     C                   ADD       1             wnrmov
081600000000      *
081700000000      * Verifica saldo parziale della partita:
081800000000      * se il saldo parziale ﬁ 0 allora azzero tutto
081900000000     C                   if        ( wptdld + wptdlp + wptdlg )
082000000000     C                             =
082100000000     C                             ( wptald + wptalp + wptalg )
082200000000     C                             and
082300000000     C                             ( wptddd + wptddp + wptddg )
082400000000     C                             =
082500000000     C                             ( wptadd + wptadp + wptadg )
082600000000     C                   clear                   wmaxida
082700000000     C                   reset                   DSC
082800000000D2246C                   clear                   DSCMP
082900000000D2246C                   clear                   DSCMD
083000000000     C                   CLEAR                   IML
083100000000     C                   CLEAR                   IMD
083200000000     C                   CLEAR                   CAU
083300000000D2085C                   CLEAR                   MP
083400000000D2085C                   CLEAR                   MP
083500000000     C                   CLEAR                   NAS
083600000000D1955C                   CLEAR                   wpFlgRit
083700000000     C                   endif
083800000000     C*
083900000000     C                   ENDSR
084000000000     C/EJECT
084100000000     C******************************************************
084200000000     C* INCREMENTA TOTALI DARE                             *
084300000000     C******************************************************
084400000000     C     DARE          BEGSR
084500000000      *
084600000000      * Incrementa totali Dare m.c. e divisa
084700000000      *
084800000000     C     wcautec       ifeq      *off
084900000000     C                   SELECT
085000000000     C     pastpregz     WHENEQ    'D'
085100000000     C                   ADD       pasimporto    wptdld
085200000000     C                   ADD       pasimportd    wptddd
085300000000     C     pastpregz     WHENEQ    'P'
085400000000     C                   ADD       pasimporto    wptdlp
085500000000     C                   ADD       pasimportd    wptddp
085600000000     C     pastpregz     WHENEQ    'G'
085700000000     C                   ADD       pasimporto    wptdlg
085800000000     C                   ADD       pasimportd    wptddg
085900000000     C                   ENDSL
086000000000     C                   endif
086100000000      *
086200000000      * Se non voglio i dati di scadenza
086300000000     C     x20parsca     IFNE      'P'
086400000000      *
086500000000      * Controllo causale
086600000000     C     pascausrig    IFNE      CFDCOD                                       PF dare
086700000000     C     pascausrig    ANDNE     CPDCOD                                       Portafoglio D
086800000000     C     pascausrig    ANDNE     CINCOD                                       Insoluto
086900000000     C     pascausrig    ANDNE     CVDCOD                                       Varie       D
087000000000     C     pascausrig    ANDNE     CPRCOD                                       Regolamento a scader
087100000000     C     pascausrig    ANDNE     CPGCOD
087200000000     C     pascausrig    ANDNE     CICCOD
087300000000     C     pascausrig    ANDNE     CDCCOD
087400000000     C                   EXSR      CAROPE
087500000000     C                   END
087600000000      *
087700000000      * Se richiesta elaborazione per partita
087800000000      * e movimento dare di fornitore
087900000000      * forzo *LOVAL nella data scadenza per scalarla dalla rata pi® vecchia
088000000000      *
088100000000C1013C**   x20elasca     IFEQ      'P'
088200000000C1013C**                 move      dat1940       pasdtsca
088300000000C1013C**                 ENDIF
088400000000      *
088500000000      *
088600000000     C                   SELECT
088700000000      *
088800000000      * Causale di tipo PF
088900000000     C     pascausrig    WHENEQ    CFDCOD                                       Causale IVA
089000000000      *
089100000000      * Memorizza dati movimenti con causale PF su schiere DLP/DDP/DSP
089200000000      *
089300000000     C                   SELECT
089400000000     C     DSP(IDAR)     WHENEQ    *blanks
089500000000     C                   MOVE      scddtsca      DSP(IDAR)
089600000000     C     DSP(IDAR)     WHENNE    scddtsca
089700000000     C                   ADD       1             IDAR
089800000000     C     IDAR          IFGT      WMAXEL
089900000000     C                   Z-ADD     WMAXEL        IDAR
090000000000     C                   ENDIF
090100000000     C                   MOVE      scddtsca      DSP(IDAR)
090200000000     C                   ENDSL
090300000000      *
090400000000     C                   ADD       pasimporto    DLP(IDAR)
090500000000     C                   ADD       pasimportd    DDP(IDAR)
090600000000     C                   MOVE      pascausrig    DCAP(IDAR)
090700000000     C     pasmdrd       IFNE      *blank
090800000000     C                   MOVE      pasmdrd       DRDP(IDAR)
090900000000     C                   END
091000000000D1557C                   MOVE      pasmdpag      DMDP(IDAR)
091100000000      *
091200000000      * PF dare
091300000000     C                   ADD       pasimportd    wivadare
091400000000      * Fine elaborazione causale PF
091500000000      *
091600000000      *
091700000000      * Causale di portafoglio dare
091800000000     C     pascausrig    WHENEQ    CPDCOD
091900000000     C                   MOVE      *on           wportafo
092000000000     C                   SELECT
092100000000     C* se il conto ﬁ un cliente
092200000000     C                   WHEN      x20snatura = 'C'
092300000000     C* vado sugli effetti attivi
092400000000     C                   EXSR      CHNEFFA
092500000000     C* se il conto ﬁ un fornitore
092600000000     C                   WHEN      x20snatura = 'F'
092700000000     C* vado sugli effetti passivi
092800000000     C                   EXSR      CHNEFFP
092900000000     C                   ENDSL
093000000000      *
093100000000      * Causale di insoluti
093200000000     C     pascausrig    WHENEQ    CINCOD
093300000000     C                   ADD       pasimporto    wimpins
093400000000      *
093500000000      * Movimento di pagamento
093600000000     C     pascausrig    wheneq    CPGCOD
093700000000     C                   ADD       pasimporto    wpaginc
093800000000      *
093900000000      * Movimento di incasso
094000000000     C     pascausrig    wheneq    CICCOD
094100000000     C                   sub       pasimporto    wpaginc
094200000000      *
094300000000      *
094400000000     C                   OTHER
094500000000      *
094600000000     C                   ENDSL
094700000000      *
094800000000      * Assegni a rischio o regolamenti a scadere
094900000000     C                   EXSR      ALTTITOLI
095000000000      *
095100000000      * Caricamento rata nelle schiere
095200000000      *
095300000000     C                   EXSR      CARRATE
095400000000      *
095500000000     C                   ENDIF
095600000000      *
095700000000     C                   ENDSR
095800000000     C/EJECT
095900000000     C******************************************************
096000000000     C* INCREMENTA TOTALI AVERE                            *
096100000000     C******************************************************
096200000000     C     AVERE         BEGSR
096300000000      *
096400000000      * Incrementa totali Avere m.c. e divisa
096500000000      *
096600000000     C     wcautec       ifeq      *off
096700000000     C                   select
096800000000     C     pastpregz     WHENEQ    'D'
096900000000     C                   ADD       pasimporto    wptald
097000000000     C                   ADD       pasimportd    wptadd
097100000000     C     pastpregz     WHENEQ    'P'
097200000000     C                   ADD       pasimporto    wptalp
097300000000     C                   ADD       pasimportd    wptadp
097400000000     C     pastpregz     WHENEQ    'G'
097500000000     C                   ADD       pasimporto    wptalg
097600000000     C                   ADD       pasimportd    wptadg
097700000000     C                   ENDSL
097800000000     C                   endif
097900000000      *
098000000000      * Se non voglio i dati di scadenza
098100000000     C     x20parsca     IFNE      'P'
098200000000      *
098300000000      * Controllo causale
098400000000     C     pascausrig    IFNE      CFACOD                                       PF avere
098500000000     C     pascausrig    ANDNE     CPACOD                                       Portafoglio A
098600000000     C     pascausrig    ANDNE     CINCOD                                       Insoluto
098700000000     C     pascausrig    ANDNE     CVACOD                                       Varie       A
098800000000     C     pascausrig    ANDNE     CPRCOD                                       Regolamento a scader
098900000000     C     pascausrig    ANDNE     CICCOD
099000000000     C     pascausrig    ANDNE     CPGCOD
099100000000     C     pascausrig    ANDNE     CDCCOD
099200000000     C                   EXSR      CAROPE
099300000000     C                   END
099400000000      *
099500000000      *
099600000000     C                   SELECT
099700000000      *
099800000000      * Causale di tipo PF
099900000000     C     pascausrig    WHENEQ    CFACOD                                       Causale IVA
100000000000      *
100100000000      * Memorizza dati movimenti con causale PF su schiere ALP/ADP/ASP
100200000000      *
100300000000     C                   SELECT
100400000000     C     ASP(IAVE)     WHENEQ    *blanks
100500000000     C                   MOVE      scddtsca      ASP(IAVE)
100600000000     C     ASP(IAVE)     WHENNE    scddtsca
100700000000     C                   ADD       1             IAVE
100800000000     C     IAVE          IFGT      WMAXEL
100900000000     C                   Z-ADD     WMAXEL        IAVE
101000000000     C                   ENDIF
101100000000     C                   MOVE      scddtsca      ASP(IAVE)
101200000000     C                   ENDSL
101300000000      *
101400000000     C                   ADD       pasimporto    ALP(IAVE)
101500000000     C                   ADD       pasimportd    ADP(IAVE)
101600000000     C                   MOVE      pascausrig    ACAP(IAVE)
101700000000     C     pasmdrd       IFNE      *blank
101800000000     C                   MOVE      pasmdrd       ARDP(IAVE)
101900000000     C                   END
102000000000D1557C                   MOVE      pasmdpag      AMDP(IAVE)
102100000000      *
102200000000      * PF avere
102300000000     C                   ADD       pasimportd    wivaaver
102400000000      * Fine elaborazione causale PF
102500000000      *
102600000000      *
102700000000      * Causale di portafoglio avere
102800000000     C     pascausrig    WHENEQ    CPACOD
102900000000     C                   MOVE      *on           wportafo
103000000000     C                   SELECT
103100000000     C* se il conto ﬁ un cliente
103200000000     C                   WHEN      x20snatura = 'C'
103300000000     C* vado sugli effetti attivi
103400000000     C                   EXSR      CHNEFFA
103500000000     C* se il conto ﬁ un fornitore
103600000000     C                   WHEN      x20snatura = 'F'
103700000000     C* vado sugli effetti passivi
103800000000     C                   EXSR      CHNEFFP
103900000000     C                   ENDSL
104000000000      * Se l'effetto ﬁ insoluto...
104100000000      * ...e non corrisponde ad una rata di fattura...
104200000000      * ...forzo una data vecchia in modo che l'effetto annulli la fattura
104300000000      * anzichﬁ l'insoluto
104400000000     C     *in31         ifeq      *off
104500000000     C     efaesito      andeq     '4'
104600000000     C     efadtesito    andle     x20dtregmd
104700000000     C                   z-add     1             b                 3 0
104800000000     C     scddtsca      lookup    dsp(b)                                 32
104900000000     C     *in32         ifeq      *on
105000000000     C     ddp(b)        andne     pasimportd
105100000000     C     *in32         oreq      *off
105200000000     C                   eval      scddtsca      = dat1940
105300000000     C                   endif
105400000000     C                   endif
105500000000      *
105600000000      * Movimento di incasso
105700000000     C     pascausrig    wheneq    CICCOD
105800000000     C                   ADD       pasimporto    wpaginc
105900000000      *
106000000000      * Movimento di pagamento
106100000000     C     pascausrig    wheneq    CPGCOD
106200000000     C                   sub       pasimporto    wpaginc
106300000000      *
106400000000      * Causale di insoluti
106500000000     C     pascausrig    WHENEQ    CINCOD
106600000000     C                   sub       pasimporto    wimpins
106700000000      *
106800000000     C                   OTHER
106900000000      *
107000000000     C                   ENDSL
107100000000      *
107200000000      * Assegni a rischio o regolamenti a scadere
107300000000      *
107400000000     C                   exsr      alttitoli
107500000000      *
107600000000      * Caricamento rata nelle schiere
107700000000      *
107800000000     C                   EXSR      CARRATE
107900000000      *
108000000000     C                   ENDIF
108100000000      *
108200000000     C                   ENDSR
108300000000     C/EJECT
108400000000     C******************************************************
108500000000     C* CARICAMENTO RATE NELLE SCHIERE                     *
108600000000     C******************************************************
108700000000     C     CARRATE       BEGSR
108800000000      *
108900000000     C     wcautec       ifeq      *off
109000000000C1013
109100000000C1013 * Scelgo la data con cui fare lookup
109200000000C1013C                   eval      wrkdtsca = pasdtsca
109300000000C1013 * Se richiesta elaborazione per partita (Es. estrazione bonifici)
109400000000C1013 * e movimento dare di fornitore
109500000000C1013 * forzo 1940 nella data scadenza per scalarla dalla rata pi® vecchia.
109600000000C1013C     x20elasca     ifeq      'P'
109700000000C1013C     pasdare       andeq     1
109800000000C1013C     x20snatura    andeq     'F'
109900000000C1013C                   eval      wrkdtsca = dat1940
110000000000C1013C                   endif
110100000000      *
110200000000      * Accorpo i movimenti con la stessa scadenza PASDTSCA
110300000000      * tranne gli insoluti e i generici
110400000000     C                   eval      ida           = 1
110500000000     C     operate       ifeq      *off
110600000000D2246C*prima provo con scadenza + modalit” o rimessa diretta
110700000000  "  C                   movel     dsc           dscMP
110800000000  "  C                   move      mp            dscMP
110900000000  "  C                   movel     dsc           dscMD
111000000000  "  C                   move      md            dscMD
111100000000  "  C                   movel     wrkdtsca      WrkDtscaM        11
111200000000  "  c                   if        pasmdpag <> *blanks
111300000000  "  C                   move      pasmdpag      wrkdtscaM
111400000000  "  c     wrkdtscaM     lookup    dscMp(ida)                             35
111500000000  "  c                   else
111600000000  "  C                   move      pasmdrd       wrkdtscaM
111700000000  "  c     wrkdtscaM     lookup    dscMd(ida)                             35
111800000000  "  c                   endif
111900000000  "  c* se non trovato, lascio il lookup con solo la scadenza
112000000000D2246c                   if        *in35=*off
112100000000C1013C     wrkdtsca      lookup    dsc(ida)                               35
112200000000D2246C                   endif
112300000000  "  x**    ex-D2085     if        *in35=*on
112400000000  "  x**prima prova ad accorpare con la stessa mod./pag o rim. diretta
112500000000  "  x**    ex-D2085     move      ida           savida
112600000000  "  x**    ex-D2085     eval      ida           = 1
112700000000  "  x**    ex-D2085     if        pasmdpag <> *blanks
112800000000  "  x**   pasmdpag      lookup    mp(ida)                                35
112900000000  "  x**    ex-D2085     endif
113000000000  "  x**    ex-D2085     if        pasmdrd  <> *blanks
113100000000  "  x**   pasmdrd       lookup    md(ida)                                35
113200000000  "  x**    ex-D2085     endif
113300000000  "  x**se per? non trovati, rimetto tutto come prima, quindi
113400000000  "  x**accendo il 35
113500000000  "  x**N35 ex-D2085     eval      ida     =     savida
113600000000  "  x**N35 ex-D2085     seton                                        35
113700000000D2246x*     ex-D2085     endif
113800000000     C                   else
113900000000     C                   setoff                                       35
114000000000     C                   endif
114100000000      * Differenza cambio: se il movimento non ﬁ riferito ad una scadenza
114200000000      * gi” caricata la sommo alla prima rata piena.
114300000000     C     *in35         ifeq      *off
114400000000     C     opetpdocni    andeq     '6'
114500000000     C                   eval      ida = 1
114600000000     C     ida           dowle     wmaxida
114700000000     C     iml(ida)      andeq     0
114800000000     C                   eval      ida = ida + 1
114900000000     C                   enddo
115000000000     C                   seton                                        35
115100000000     C                   endif
115200000000      * Se non ho gi” incontrato la scadenza
115300000000      * carico la scadenza SCDDTSCA.
115400000000     C     *in35         ifeq      *off
115500000000     C     *hival        lookup    dsc(ida)                               35
115600000000     C     *in35         ifeq      *off
115700000000     C                   eval      ida           = wmaxel
115800000000C1024C**                 eval      x20errori = 'B'
115900000000     C                   else
116000000000     C                   eval      wmaxida = ida
116100000000     C                   endif
116200000000     C                   endif
116300000000      *
116400000000     C     dsc(ida)      ifeq      *hival
116500000000     C                   eval      DSC(IDA)      = scddtsca
116600000000     C                   eval      NRT(IDA)      = pasnrrata
116700000000     C                   endif
116800000000      *
116900000000      *
117000000000      *
117100000000     C     CAU(IDA)      IFEQ      *BLANK
117200000000     C                   MOVE      pascausrig    CAU(IDA)
117300000000D2085C                   MOVE      pasmdrd       MD(IDA)
117400000000D2085C                   MOVE      pasmdPag      MP(IDA)
117500000000     C                   END
117600000000      *
117700000000      *
117800000000     C                   SELECT
117900000000      * Dare
118000000000      *
118100000000     C     pasdare       WHENEQ    1
118200000000     C                   ADD       pasimporto    IML(IDA)
118300000000     C                   ADD       pasimportd    IMD(IDA)
118400000000      *
118500000000     C     IML(IDA)      IFGT      0
118600000000     C     pascausrig    andne     CVDCOD                                       * INSOLUTO
118700000000     C     pascausrig    andne     CDCCOD                                       * INSOLUTO
118800000000     C                   MOVE      pascausrig    CAU(IDA)
118900000000     C                   END
119000000000      * Memorizzo causale rata e n. assoluto registrazione
119100000000     C     pascausrig    IFEQ      CINCOD                                       * INSOLUTO
119200000000     C     IML(IDA)      ANDGT     0
119300000000     C                   MOVE      pascausrig    CAU(IDA)
119400000000     C                   eval      nasds1        = passys
119500000000     C                   eval      nasds2        = pasnrasreg
119600000000     C                   eval      nasds3        = pasnrrigam
119700000000     C                   eval      nas(ida)      = nasds
119800000000     C                   END
119900000000      * Avere
120000000000      *
120100000000     C     pasavere      WHENEQ    1
120200000000     C                   SUB       pasimporto    IML(IDA)
120300000000     C                   SUB       pasimportd    IMD(IDA)
120400000000      *
120500000000     C     IML(IDA)      IFLT      0
120600000000     C     pascausrig    andne     CVACOD                                       * INSOLUTO
120700000000     C     pascausrig    andne     CDCCOD                                       * INSOLUTO
120800000000     C                   MOVE      pascausrig    CAU(IDA)
120900000000     C                   END
121000000000     C* Memorizzo causale rata e n. assoluto registrazione
121100000000     C     IML(IDA)      IFLT      0
121200000000     C     pascausrig    IFEQ      CFACOD
121300000000     C     pascausrig    OREQ      CPACOD
121400000000     C     pascausrig    OREQ      CINCOD
121500000000     C                   MOVE      pascausrig    CAU(IDA)
121600000000     C                   eval      nasds1        = passys
121700000000     C                   eval      nasds2        = pasnrasreg
121800000000     C                   eval      nasds3        = pasnrrigam
121900000000     C                   eval      nas(ida)      = nasds
122000000000     C                   END
122100000000     C                   END
122200000000      *
122300000000     C                   ENDSL
122400000000     C*
122500000000     C                   endif
122600000000     C*
122700000000     C                   ENDSR
122800000000     C/EJECT
122900000000     C******************************************************
123000000000     C* Ricerca effetto attivo                             *
123100000000     C******************************************************
123200000000     C     CHNEFFA       BEGSR
123300000000      *
123400000000     C     KEFA          CHAIN     NDEFA01L                           31
123500000000      * Considero l'effetto solo se non ﬁ esitato oppure
123600000000      * se l'esito ﬁ posteriore alla data limite lettura movimenti
123700000000     C     *IN31         IFEQ      '0'
123800000000     C     efaesito      ANDEQ     '0'
123900000000     C     *IN31         OREQ      '0'
124000000000     C     efadtesito    ANDGT     x20dtregmd
124100000000      *
124200000000     C                   CLEAR                   scdcaurisc
124300000000     C                   CLEAR                   scdflg
124400000000      *
124500000000      * Se la data scadenza ﬁ maggiore della data riferimento calcoli
124600000000      * incrementa importo portafoglio
124700000000      *
124800000000     C     efadtsca      IFGT      x20dtsca
124900000000     C* se l'importo ﬁ stato movimentato in Avere (per l'effetto ﬁ un
125000000000     C*  credito), sommo
125100000000     C                   IF        PASAvere = 1
125200000000     C                   ADD       pasimporto    wpportaf
125300000000     C                   ADD       pasimportd    wpdportaf
125400000000     C                   ENDIF
125500000000     C* se l'importo ﬁ stato movimentato in Dare  (per l'effetto ﬁ un
125600000000     C*  debito ), sottraggo
125700000000     C                   IF        PASDare = 1
125800000000     C                   SUB       pasimporto    wpportaf
125900000000     C                   SUB       pasimportd    wpdportaf
126000000000     C                   ENDIF
126100000000     C     x20tabcf      IFNE      *blanks
126200130618>>>>>C     YOmEfaAsc     ANDEQ     *OFF
126300000000     C* se l'importo ﬁ stato movimentato in Avere (per l'effetto ﬁ un
126400000000     C*  credito), sommo
126500000000     C                   IF        PASAvere = 1
126600000000     C                   Z-ADD     pasimporto    wgimporto
126700000000     C                   Z-ADD     pasimportd    wgimportd
126800000000     C                   ENDIF
126900000000     C* se l'importo ﬁ stato movimentato in Dare  (per l'effetto ﬁ un
127000000000     C*  debito ), sottraggo
127100000000     C                   IF        PASDare = 1
127200000000     C                   Z-SUB     pasimporto    wgimporto
127300000000     C                   Z-SUB     pasimportd    wgimportd
127400000000     C                   ENDIF
127500000000     C                   MOVE      efadtsca      wgdata
127600000000     C                   EXSR      griglia
127700000000     C                   ENDIF
127800000000     C                   ENDIF
127900000000      *
128000000000      * Se la data scadenza ﬁ maggiore della data rischio e minore/uguale
128100000000      * alla data riferimento calcoli incrementa importo rischio
128200000000      *      oppure
128300000000      * se l'effetto ﬁ al dopo incasso (e non esitato) lo considero
128400000000      * a rischio se la sua data scadenza originale ﬁ <= data rif. scad.
128500000000      *
128600000000     C                   Z-ADD     1             I                 3 0
128700000000     C     pascausrig    LOOKUP    CODRIS(I)                              32
128800000000     C     *IN32         IFEQ      '1'
128900000000     C     efadtsca      ANDGT     DATRIS(I)
129000000000     C     efadtsca      ANDLE     x20dtsca
129100000000     C*
129200000000     C     efadinc       oreq      *on
129300000000     C     efadtsca      ANDLE     x20dtsca
129400000000     C* se l'importo ﬁ stato movimentato in Avere (per l'effetto ﬁ un
129500000000     C*  credito), sommo
129600000000     C                   IF        PASAvere = 1
129700000000     C                   ADD       pasimporto    wprisc
129800000000     C                   ADD       pasimportd    wpdrisc
129900000000     C                   ENDIF
130000000000     C* se l'importo ﬁ stato movimentato in Dare  (per l'effetto ﬁ un
130100000000     C*  debito ), sottraggo
130200000000     C                   IF        PASDare = 1
130300000000     C                   SUB       pasimporto    wprisc
130400000000     C                   SUB       pasimportd    wpdrisc
130500000000     C                   ENDIF
130600000000     C                   MOVE      pascausrig    scdcaurisc
130700000000      * Flag di rischio
130800000000     C                   MOVE      'S'           scdflg1
130900000000     C                   END
131000000000      *
131100000000      * Scrive record scadenziario
131200000000     C     x20write      IFNE      *blank
131300000000     C     x20parsca     ANDNE     'E'
131400000000     C                   MOVE      x20societa    scdsocieta
131500000000     C                   MOVE      ppaunita      scdunita
131600000000     C                   MOVE      x20ctb        scdctb
131700000000     C                   MOVE      x20KCC        SCDKCC
131800000000     C                   MOVE      x20KSC        SCDKSC
131900000000     C                   MOVE      x20snatura    scdsnatura
132000000000     C                   MOVE      ppadtpar      scddtpar
132100000000     C                   Z-ADD     ppanrpar      scdnrpar
132200000000     C                   MOVE      ppaserpar     scdserpar
132300000000     C                   Z-ADD     ppasys        scdsys
132400000000     C                   Z-ADD     ppanrasint    scdnrasint
132500000000     C                   MOVE      efadtsca      scddtsca
132600000000     C*
132700000000     C* se il segno ﬁ avere, la riga dell'effetto ﬁ completamente attiva
132800000000     C* se l'importo ﬁ stato movimentato in Avere (per l'effetto ﬁ un
132900000000     C*  credito), sommo
133000000000     C                   IF        PASAvere = 1
133100000000     C                   Z-ADD     pasimporto    scdimprmc
133200000000     C                   Z-ADD     pasimporto    scdimpomc
133300000000     C                   Z-ADD     pasimportd    scdimprd
133400000000     C                   Z-ADD     pasimportd    scdimpod
133500000000     C                   ENDIF
133600000000     C* se il segno ﬁ dare, la riga dell'effetto ﬁ una nota accredito
133700000000     C* se l'importo ﬁ stato movimentato in Dare  (per l'effetto ﬁ un
133800000000     C*  debito ), sottraggo
133900000000     C                   IF        PASDare = 1
134000000000     C                   Z-SUB     pasimporto    scdimprmc
134100000000     C                   Z-SUB     pasimporto    scdimpomc
134200000000     C                   Z-SUB     pasimportd    scdimprd
134300000000     C                   Z-SUB     pasimportd    scdimpod
134400000000     C                   ENDIF
134500000000      *
134600000000     C                   CLEAR                   scdmdrd
134700000000     C                   MOVE      efamdpag      scdmdpag
134800000000     C                   CLEAR                   scdtitoli
134900000000     C                   Z-ADD     efasys        scdsyseff
135000000000     C                   Z-ADD     efanrasreg    scdnraseff
135100000000     C                   z-add     pasnrrigam    scdnrrigam
135200000000     C                   Z-ADD     efanreff      scdnreff
135300000000D2247C                   MOVE      CFDCODSav     scdcaus1
135400000000D2247c                   If        scdcaus1 = *blanks
135500000000     c                   MOVE      pascausrig    scdcaus1
135600000000D2247C                   endif
135700000000     C                   MOVE      pascausrig    scdcaus2
135800000000     C                   MOVE      efastatus     scdflg2
135900000000      *
136000000000     C                   exsr      writescd
136100000000      *
136200000000     C                   END
136300000000     C                   END
136400000000      *
136500000000     C                   ENDSR
136600000000     C/EJECT
136700000000     C******************************************************
136800000000     C* Ricerca effetto passivo                            *
136900000000     C******************************************************
137000000000     C     CHNEFFP       BEGSR
137100000000      *
137200000000     C     KEFP          CHAIN     NDEFP01L                           31
137300000000      * Considero l'effetto se non ﬁ annullato, oppure
137400000000      * se l'annullo ﬁ posteriore alla data limite lettura movimenti
137500000000     C     *IN31         IFEQ      '0'
137600000000     C     efpesito      ANDEQ     '0'
137700000000     C     *IN31         OREQ      '0'
137800000000     C     efpdtesito    ANDGT     x20dtregmd
137900000000      *
138000000000     C                   CLEAR                   scdflg
138100000000      * stato = 4 (addebitato), verifico la data del ritiro e se
138200000000      * inferiore salto l'elaborazione
138300000000     C     efpstatus     IFEQ      '4'
138400000000     C     efpdtdist     ANDLE     x20dtregmd
138500000000     C                   GOTO      ENDEFFP
138600000000     C                   END
138700000000      * Se la data scadenza ﬁ maggiore della data riferimento calcoli
138800000000      * incrementa importo portafoglio
138900000000     C     efpdtsca      IFGT      x20dtsca
139000000000     C* se l'importo ﬁ stato movimentato in Avere (per l'effetto ﬁ un
139100000000     C*  credito), sommo
139200000000     C                   IF        PASAvere = 1
139300000000     C                   ADD       pasimporto    wpportaf
139400000000     C                   ADD       pasimportd    wpdportaf
139500000000     C                   ENDIF
139600000000     C* se l'importo ﬁ stato movimentato in Dare  (per l'effetto ﬁ un
139700000000     C*  debito ), sottraggo
139800000000     C                   IF        PASDare = 1
139900000000     C                   SUB       pasimporto    wpportaf
140000000000     C                   SUB       pasimportd    wpdportaf
140100000000     C                   ENDIF
140200000000     C     x20tabcf      IFNE      *blanks
140300000000     C* se l'importo ﬁ stato movimentato in Avere (per l'effetto ﬁ un
140400000000     C*  credito), sommo
140500000000     C                   IF        PASAvere = 1
140600000000     C                   Z-ADD     pasimporto    wgimporto
140700000000     C                   Z-ADD     pasimportd    wgimportd
140800000000     C                   ENDIF
140900000000     C* se l'importo ﬁ stato movimentato in Dare  (per l'effetto ﬁ un
141000000000     C*  debito ), sottraggo
141100000000     C                   IF        PASDare = 1
141200000000     C                   Z-SUB     pasimporto    wgimporto
141300000000     C                   Z-SUB     pasimportd    wgimportd
141400000000     C                   ENDIF
141500000000     C                   MOVE      efpdtsca      wgdata
141600000000     C                   EXSR      griglia
141700000000     C                   ENDIF
141800000000     C                   ELSE
141900000000     C* se l'importo ﬁ stato movimentato in Avere (per l'effetto ﬁ un
142000000000     C*  credito), sommo
142100000000     C                   IF        PASAvere = 1
142200000000     C                   ADD       pasimporto    wplScaduto
142300000000     C                   ADD       pasimportd    wpdScaduto
142400000000     C                   ENDIF
142500000000     C* se l'importo ﬁ stato movimentato in Dare  (per l'effetto ﬁ un
142600000000     C*  debito ), sottraggo
142700000000     C                   IF        PASDare = 1
142800000000     C                   SUB       pasimporto    wplScaduto
142900000000     C                   SUB       pasimportd    wpdScaduto
143000000000     C                   ENDIF
143100000000     C                   ENDIF
143200000000      *
143300000000     C* Scrive record scadenziario
143400000000     C     x20write      IFNE      *BLANK
143500000000     C     x20parsca     ANDNE     'E'
143600000000     C                   MOVE      x20societa    scdsocieta
143700000000     C                   MOVE      ppaunita      scdunita
143800000000     C                   MOVE      x20ctb        scdctb
143900000000     C                   MOVE      x20KCC        SCDKCC
144000000000     C                   MOVE      x20KSC        SCDKSC
144100000000     C                   MOVE      x20snatura    scdsnatura
144200000000     C                   MOVE      ppadtpar      scddtpar
144300000000     C                   Z-ADD     ppanrpar      scdnrpar
144400000000     C                   MOVE      ppaserpar     scdserpar
144500000000     C                   Z-ADD     ppasys        scdsys
144600000000     C                   Z-ADD     ppanrasint    scdnrasint
144700000000     C                   MOVE      efpdtsca      scddtsca
144800000000     C
144900000000     C* se il segno ﬁ avere, la riga dell'effetto ﬁ una nota d'addebito
145000000000     C                   IF        PASAvere = 1
145100000000     C                   Z-ADD     pasimporto    scdimprmc
145200000000     C                   Z-ADD     pasimporto    scdimpomc
145300000000     C                   Z-ADD     pasimportd    scdimprd
145400000000     C                   Z-ADD     pasimportd    scdimpod
145500000000     C                   ENDIF
145600000000     C* se il segno ﬁ dare, la riga dell'effetto ﬁ completamente passiva
145700000000     C                   IF        PASDare = 1
145800000000     C                   Z-SUB     pasimporto    scdimprmc
145900000000     C                   Z-SUB     pasimporto    scdimpomc
146000000000     C                   Z-SUB     pasimportd    scdimprd
146100000000     C                   Z-SUB     pasimportd    scdimpod
146200000000     C                   ENDIF
146300000000      *
146400000000     C                   CLEAR                   scdmdrd
146500000000     C                   MOVE      efpmdpag      scdmdpag
146600000000     C                   CLEAR                   scdtitoli
146700000000     C                   Z-ADD     efpsys        scdsyseff
146800000000     C                   Z-ADD     efpnrasreg    scdnraseff
146900000000     C                   z-add     pasnrrigam    scdnrrigam
147000000000     C                   Z-ADD     efpnreff      scdnreff
147100000000D2247C                   MOVE      CFACODSav     scdcaus1
147200000000D2247c                   If        scdcaus1 = *blanks
147300000000     c                   MOVE      pascausrig    scdcaus1
147400000000D2247C                   endif
147500000000     C                   MOVE      pascausrig    scdcaus2
147600000000     C                   CLEAR                   scdcaurisc
147700000000     C                   MOVE      efpstatus     scdflg2
147800000000      *
147900000000     C                   exsr      writescd
148000000000     C*
148100000000     C                   END
148200000000     C                   END
148300000000     C*
148400000000     C     ENDEFFP       ENDSR
148500000000     C******************************************************
148600000000     C* GESTIONE ALTRI TITOLI                              *
148700000000     C******************************************************
148800000000     C     ALTTITOLI     BEGSR
148900000000      *
149000000000     C                   clear                   scdtitoli
149100000000     C                   clear                   scdflg
149200000000     C                   clear                   scdcaurisc
149300000000     C                   setoff                                       32
149400000000     C                   eval      wrkndmov      = *off
149500000000     C                   eval      wrkinris      = *off
149600000000      * Se ﬁ un incasso, un pagamento o un regolamento a scadere...
149700000000b1   C     pascausrig    ifeq      ciccod
149800000000     C     pascausrig    oreq      cpgcod
149900000000     C     pascausrig    oreq      cprcod
150000000000      * ...uso la data documento come data scadenza.
150100000000     C                   EXSR      GETNDMOV
150200000000b2   C                   IF        movdtdoc <> DATMIN
150300000000     C                   MOVE      movdtdoc      scddtsca
150400000000e2   C                   ENDIF
150500000000      *
150600000000      * Se non ﬁ un regolamento a scadere
150700000000      * verifico se devo controllare il rischio.
150800000000     C                   Z-ADD     1             I
150900000000     C     pascausrig    LOOKUP    CODRIS(I)                              32
151000000000     C                   eval      wrkinris      = *in32
151100000000      *
151200000000      *
151300000000     C                   eval      wrkin01       = *off
151400000000      * Regolamento a scadere...
151500000000b2   C     scddtsca      ifgt      x20dtsca
151600000000      *
151700000000b3   C     pascausrig    IFEQ      CPRCOD
151800000000     C                   eval      wrkin01       = *on
151900000000     C* ...definitivo
152000000000b4   C     pastpregz     ifeq      'D'
152100000000     C                   eval      scdtitoli     = '2'
152200000000     C*
152300000000b5   C     wcautec       ifeq      *off
152400000000     C*
152500000000b6   C     pasdare       ifeq      0
152600000000     C                   add       pasimporto    wplincascd
152700000000     C                   add       pasimportd    wpdincascd
152800000000b7   C     x20tabcf      IFNE      *blanks
152900000000     C                   Z-ADD     pasimporto    wgimporto
153000000000     C                   Z-ADD     pasimportd    wgimportd
153100000000     C                   MOVE      scddtsca      wgdata
153200000000     C                   EXSR      griglia
153300000000e7   C                   ENDIF
153400000000     C                   else
153500000000     C                   sub       pasimporto    wplincascd
153600000000     C                   sub       pasimportd    wpdincascd
153700000000b7   C     x20tabcf      IFNE      *blanks
153800000000     C                   Z-sub     pasimporto    wgimporto
153900000000     C                   Z-sub     pasimportd    wgimportd
154000000000     C                   MOVE      scddtsca      wgdata
154100000000     C                   EXSR      griglia
154200000000e7   C                   ENDIF
154300000000e6   C                   endif
154400000000     C*
154500000000e5   C                   endif
154600000000     C*
154700000000     C                   else
154800000000     C* ...provvisorio
154900000000     C                   eval      scdtitoli     = '1'
155000000000     C*
155100000000b5   C     wcautec       ifeq      *off
155200000000     C*
155300000000b6   C     pasdare       ifeq      0
155400000000     C                   add       pasimporto    wplincascp
155500000000     C                   add       pasimportd    wpdincascp
155600000000b7   C     x20tabcf      IFNE      *blanks
155700000000     C                   Z-add     pasimporto    wgimporto
155800000000     C                   Z-add     pasimportd    wgimportd
155900000000     C                   MOVE      scddtsca      wgdata
156000000000     C                   EXSR      griglia
156100000000e7   C                   ENDIF
156200000000     C                   else
156300000000     C                   sub       pasimporto    wplincascp
156400000000     C                   sub       pasimportd    wpdincascp
156500000000b7   C     x20tabcf      IFNE      *blanks
156600000000     C                   Z-sub     pasimporto    wgimporto
156700000000     C                   Z-sub     pasimportd    wgimportd
156800000000     C                   MOVE      scddtsca      wgdata
156900000000     C                   EXSR      griglia
157000000000e7   C                   ENDIF
157100000000e6   C                   endif
157200000000     C*
157300000000e5   C                   endif
157400000000     C*
157500000000e4   C                   endif
157600000000     C*
157700000000e3   C                   END
157800000000      *
157900000000     C                   else
158000000000      * Rischio
158100000000b3   C     wrkinris      IFEQ      *on
158200000000     C     scddtsca      andgt     DATRIS(I)
158300000000     C     wcautec       andeq     *off
158400000000     C                   ADD       pasimporto    wprisc
158500000000     C                   ADD       pasimportd    wpdrisc
158600000000     C                   MOVE      pascausrig    scdcaurisc
158700000000     C                   CLEAR                   scdflg
158800000000     C                   MOVE      'S'           scdflg1
158900000000     C                   eval      wrkin01       = *on
159000000000b4   C     pascausrig    ifeq      cprcod
159100000000b5   C     pastpregz     ifeq      'D'
159200000000     C                   eval      scdtitoli = '2'
159300000000     C                   else
159400000000     C                   eval      scdtitoli = '1'
159500000000e4   C                   endif
159600000000e5   C                   endif
159700000000e3   C                   END
159800000000      *
159900000000e2   C                   endif
160000000000      *
160100000000b2   C     x20write      IFNE      *BLANK
160200000000     C     x20parsca     ANDNE     'E'
160300000000     C     wrkin01       andeq     *on
160400000000D1977C     wcautec       andeq     *off
160500000000      *
160600000000     C                   MOVE      x20societa    scdsocieta
160700000000     C                   MOVE      ppaunita      scdunita
160800000000     C                   MOVE      x20ctb        scdctb
160900000000     C                   MOVE      x20kcc        scdkcc
161000000000     C                   MOVE      x20ksc        scdksc
161100000000     C                   MOVE      x20snatura    scdsnatura
161200000000     C                   MOVE      pasdtpar      scddtpar
161300000000     C                   Z-ADD     pasnrpar      scdnrpar
161400000000     C                   MOVE      passerpar     scdserpar
161500000000     C                   Z-ADD     ppasys        scdsys
161600000000     C                   Z-ADD     ppanrasint    scdnrasint
161700000000      *
161800000000b3   C     pasdare       IFEQ      0
161900000000     C                   Z-ADD     pasimporto    scdimprmc
162000000000     C                   Z-ADD     pasimportd    scdimprd
162100000000     C                   Z-ADD     pasimporto    scdimpomc
162200000000     C                   Z-ADD     pasimportd    scdimpod
162300000000     C                   ELSE
162400000000     C* importo al negativo
162500000000     C                   Z-SUB     pasimporto    scdimprmc
162600000000     C                   Z-SUB     pasimportd    scdimprd
162700000000     C                   Z-SUB     pasimporto    scdimpomc
162800000000     C                   Z-SUB     pasimportd    scdimpod
162900000000e3   C                   ENDIF
163000000000      *
163100000000     C                   CLEAR                   scdmdrd
163200000000     C                   CLEAR                   scdmdpag
163300000000      *
163400000000     C                   z-add     passys        scdsyseff
163500000000     C                   z-add     pasnrasreg    scdnraseff
163600000000     C                   z-add     pasnrrigam    scdnrrigam
163700000000      *
163800000000     C                   EXSR      GETNDMOV
163900000000      *
164000000000     C                   z-add     movnrdoc      scdnreff
164100000000     C                   MOVE      pascausrig    scdcaus1
164200000000     C                   MOVE      pascausrig    scdcaus2
164300000000      *
164400000000     C                   exsr      writescd
164500000000      *
164600000000e2   C                   endif
164700000000      *
164800000000      *
164900000000e1   C                   endif
165000000000      *
165100000000     C                   ENDSR
165200000000      *
165300000000     C******************************************************
165400000000     C* REPERIMENTO NDMOV01L                               *
165500000000     C******************************************************
165600000000      *
165700000000     C     GETNDMOV      BEGSR
165800000000      *
165900000000     C                   if        wrkndmov = *off
166000000000     C     kmov          CHAIN     NDMOV01L                           34
166100000000     C                   eval      wrkndmov = *on
166200000000     C                   EndIf
166300000000      *
166400000000     C                   ENDSR
166500000000     C/EJECT
166600000000     C******************************************************
166700000000     C* CALCOLO SCADENZE APERTE                            *
166800000000     C******************************************************
166900000000     C     CALSCADEN     BEGSR
167000000000      *
167100000000      * Verifica saldi della partita:
167200000000      * se la partita ha saldo 0 in divisa e in m.c. ...
167300000000     C                   if        ( wptdld + wptdlp + wptdlg )
167400000000     C                             =
167500000000     C                             ( wptald + wptalp + wptalg )
167600000000     C                             and
167700000000     C                             ( wptddd + wptddp + wptddg )
167800000000     C                             =
167900000000     C                             ( wptadd + wptadp + wptadg )
168000000000      *
168100000000      * ... non calcolo le rate. Pulisco schiera.
168200000000      *
168300000000     C                   reset                   DSC
168400000000D2246C                   clear                   DSCMP
168500000000D2246C                   clear                   DSCMD
168600000000      *
168700000000      *
168800000000     C                   ELSE
168900000000      *
169000000000     C                   exsr      sortadsc
169100000000      *
169200000000      *
169300000000     C     1             DO        wmaxida       I
169400000000     C     IML(I)        IFEQ      0
169500000000     C     IMD(I)        ANDEQ     0
169600000000     C                   reset                   DSC(I)
169700000000     C                   reset                   NRT(I)
169800000000     C                   END
169900000000     C                   END
170000000000      *
170100000000      * Sottraggo residuo dalla rata pi® vecchia
170200000000     C     1             DO        wmaxida       A
170300000000     C     IML(A)        IFLT      0
170400000000      *
170500000000     C                   Z-ADD     1             I
170600000000     C     I             DOWLE     wmaxida
170700000000     C     IML(A)        ANDLT     0
170800000000     C     IML(I)        IFGT      0
170900000000     C                   ADD       IML(A)        IML(I)
171000000000     C                   ADD       IMD(A)        IMD(I)
171100000000     C     IML(I)        IFLT      0
171200000000     C                   Z-ADD     IML(I)        IML(A)
171300000000     C                   Z-ADD     IMD(I)        IMD(A)
171400000000     C                   CLEAR                   IML(I)
171500000000     C                   CLEAR                   IMD(I)
171600000000     C                   ELSE
171700000000     C                   CLEAR                   IML(A)
171800000000     C                   CLEAR                   IMD(A)
171900000000     C                   END
172000000000     C                   END                                                    * IML,I IFGT 0
172100000000     C                   ADD       1             I
172200000000     C                   END                                                    * I DOWLE IDA
172300000000      *
172400000000     C                   END                                                    * IML,A IFLT
172500000000     C                   END                                                    * 1 DO IDA
172600000000      *
172700000000     C                   END
172800000000      *
172900000000     C                   ENDSR
173000000000     C/EJECT
173100000000     C******************************************************
173200000000     C*  INCREMENTA TOTALI CONTO                           *
173300000000     C******************************************************
173400000000     C     INCCONTO      BEGSR
173500000000      *
173600000000      * Solo se ho letto almeno un movimento
173700000000      *
173800000000     C     wnrmov        IFGT      *zeros
173900000000      *
174000000000D1955C                   If        wpflgrit <> *blanks
174100000000  "  c                   eval      wcflgrit = wpflgrit
174200000000D1955c                   endif
174300000000      *
174400000000     C
174500000000      * Imposta scaduto e a scadere di partita
174600000000     C                   ADD       wpportaf      wplascader
174700000000     C                   ADD       wpdportaf     wpdascader
174800000000     C                   ADD       wplincascp    wplascader
174900000000     C                   ADD       wpdincascp    wpdascader
175000000000     C                   ADD       wplincascd    wplascader
175100000000     C                   ADD       wpdincascd    wpdascader
175200000000      *
175300000000      * Se non voglio i dati di scadenza
175400000000     C     x20parsca     IFNE      'P'
175500000000      *
175600000000     C                   EXSR      CALSCADEN
175700000000      *         in lire e divisa
175800000000     C                   Z-ADD     1             I
175900000000     C     I             DOWLE     wmaxida
176000000000     C                   CLEAR                   scdflg
176100000000     C                   CLEAR                   scdsyseff
176200000000     C                   CLEAR                   scdnraseff
176300000000     C                   CLEAR                   scdnrrigam
176400000000     C     IML(I)        IFNE      0
176500000000     C     DSC(I)        ANDNE     *hival
176600000000     C     DSC(I)        IFGT      x20dtsca
176700000000     C                   ADD       IML(I)        wplascader
176800000000     C                   ADD       IMD(I)        wpdascader                     divisa
176900000000     C                   ELSE
177000000000     C                   ADD       IML(I)        wplscaduto
177100000000     C                   ADD       IMD(I)        wpdscaduto                     divisa
177200000000      * Insoluti (solo per clienti)
177300000000     C     x20snatura    IFEQ      'C'
177400000000     C     IML(I)        ANDNE     *zeros
177500000000     C                   eval      societa = x20societa
177600000000     C                   eval      causale = cau(i)
177700000000     C                   exsr      callmvc003
177800000000     C     opeportaf     ifeq      '3'
177900000000     C     opestp        oreq      '3'
178000000000     C                   MOVE      'S'           scdflg3
178100000000     C                   ADD       IML(I)        wpscains
178200000000     C                   ADD       IMD(I)        wpdscains
178300000000     C     nas(i)        ifne      *blanks
178400000000     C                   eval      nasds         = nas(i)
178500000000     C                   eval      scdsyseff     = nasds1
178600000000     C                   eval      scdnraseff    = nasds2
178700000000     C                   eval      scdnrrigam    = nasds3
178800000000     C                   endif
178900000000     C                   END
179000000000     C                   END
179100000000      *
179200000000     C                   END
179300000000      *
179400000000     C     x20tabcf      IFNE      *blanks
179500000000     C                   Z-ADD     IML(I)        wgimporto
179600000000     C                   Z-ADD     IMD(I)        wgimportd
179700000000     C                   MOVE      DSC(I)        wgdata
179800000000     C                   EXSR      griglia
179900000000     C                   ENDIF
180000000000      *
180100000000      * Scrive record scadenziario
180200000000      *      o imposta parametro esteso
180300000000      *
180400000000     C     x20write      IFNE      *BLANK
180500000000     C     x20parscd     OREQ      'S'
180600000000     C                   MOVE      x20societa    scdsocieta
180700000000     C                   MOVE      ppaunita      scdunita
180800000000     C                   MOVE      x20ctb        scdctb
180900000000     C                   MOVE      x20KCC        SCDKCC
181000000000     C                   MOVE      x20KSC        SCDKSC
181100000000     C                   MOVE      x20snatura    scdsnatura
181200000000     C                   MOVE      ppadtpar      scddtpar
181300000000     C                   Z-ADD     ppanrpar      scdnrpar
181400000000     C                   MOVE      ppaserpar     scdserpar
181500000000     C                   Z-ADD     ppasys        scdsys
181600000000     C                   Z-ADD     ppanrasint    scdnrasint
181700000000     C                   MOVE      DSC(I)        scddtsca
181800000000     C                   Z-ADD     IML(I)        scdimprmc
181900000000     C                   Z-ADD     IMD(I)        scdimprd
182000000000     C                   Z-ADD     NRT(I)        scdnrrata
182100000000      *
182200000000     C                   CLEAR                   scdimpomc
182300000000     C                   CLEAR                   scdimpod
182400000000     C                   CLEAR                   scdmdrd
182500000000D1861C                   CLEAR                   scdmdpag
182600000000     C                   Z-ADD     1             A
182700000000      *
182800000000     C                   SELECT
182900000000      *
183000000000      * Cliente (vedo prima rate DARE)
183100000000      *
183200000000     C     x20snatura    WHENEQ    'C'
183300000000     C     DSC(I)        LOOKUP    DSP(A)                                 32
183400000000     C     *IN32         IFEQ      '1'
183500000000     C                   Z-ADD     DLP(A)        scdimpomc
183600000000     C                   Z-ADD     DDP(A)        scdimpod
183700000000     C                   MOVE      DCAP(A)       scdcaus1
183800000000D2085x***                MOVE      DRDP(A)       scdmdrd
183900000000  "  c* se vi sono due rate con rim. diretta diversa e stessa scadenza
184000000000  "  c* nella schiera DRDP c'ﬁ solo l'ultima memorizzata perchﬁ tali
184100000000  "  c* schiere sono raggruppate per data scadenza
184200000000  "  C                   MOVE      MD(I)         scdmdrd
184300000000D2085C                   MOVE      MP(I)         scdmdpag
184400000000D2085x** ex d1557        clear                   scdmdpag
184500000000  "  x**    "            IF        scdmdrd = *blanks
184600000000  "  x**    "            MOVE      DMDP(A)       scdmdpag
184700000000D2085x** ex d1557        endif
184800000000     c*
184900000000     C                   ELSE
185000000000D2085c* RATA AVERE
185100000000     C                   Z-ADD     1             A
185200000000     C     DSC(I)        LOOKUP    ASP(A)                                 32
185300000000     C     *IN32         IFEQ      '1'
185400000000     C                   Z-sub     ALP(A)        scdimpomc
185500000000     C                   Z-sub     ADP(A)        scdimpod
185600000000     C                   MOVE      ACAP(A)       scdcaus1
185700000000D2085C* lascio modalit” originaria fattura se presente
185800000000D2085C                   if        scdmdrd = *blanks
185900000000     C                   MOVE      ARDP(A)       scdmdrd
186000000000D2085C                   endif
186100000000D1557C                   clear                   scdmdpag
186200000000D2085x**                 IF        scdmdrd = *blanks
186300000000  "  c                   IF        scdmdrd=*blanks and scdmdpag=*blanks
186400000000D2085C                   MOVE      AMDP(A)       scdmdpag
186500000000D1557C                   endif
186600000000     C                   END
186700000000     C                   END
186800000000      *
186900000000      * Fornitore (vedo prima rate AVERE)
187000000000      *
187100000000     C     x20snatura    WHENEQ    'F'
187200000000     C     DSC(I)        LOOKUP    ASP(A)                                 32
187300000000     C     *IN32         IFEQ      '1'
187400000000     C                   Z-sub     ALP(A)        scdimpomc
187500000000     C                   Z-sub     ADP(A)        scdimpod
187600000000     C                   MOVE      ACAP(A)       scdcaus1
187700000000D2085x***                MOVE      ARDP(A)       scdmdrd
187800000000  "  c* se vi sono due rate con rim. diretta diversa e stessa scadenza
187900000000  "  c* nella schiera DRDP c'ﬁ solo l'ultima memorizzata perchﬁ tali
188000000000  "  c* schiere sono raggruppate per data scadenza
188100000000  "  C                   MOVE      MD(I)         scdmdrd
188200000000D2085C                   MOVE      MP(I)         scdmdpag
188300000000D2085X* ex d1557         clear                   scdmdpag
188400000000  "  X*     "            IF        scdmdrd = *blanks
188500000000  "  X*     "            MOVE      AMDP(A)       scdmdpag
188600000000D2085X* ex d1557         endif
188700000000     c*
188800000000     C                   ELSE
188900000000D2085c* RATA DARE
189000000000     C                   Z-ADD     1             A
189100000000     C     DSC(I)        LOOKUP    DSP(A)                                 32
189200000000     C     *IN32         IFEQ      '1'
189300000000     C                   Z-ADD     DLP(A)        scdimpomc
189400000000     C                   Z-ADD     DDP(A)        scdimpod
189500000000     C                   MOVE      DCAP(A)       scdcaus1
189600000000D2085C* lascio modalit” originaria fattura se presente
189700000000D2085C                   if        scdmdrd = *blanks
189800000000     c                   MOVE      DRDP(A)       scdmdrd
189900000000D2085C                   endif
190000000000D1557C                   clear                   scdmdpag
190100000000D2085X* ex d1557         IF        scdmdrd = *blanks
190200000000  "  C                   IF        scdmdrd=*blanks and scdmdpag=*blanks
190300000000D2085C                   MOVE      DMDP(A)       scdmdpag
190400000000D1157C                   endif
190500000000     C                   END
190600000000     C                   END
190700000000      *
190800000000     C                   ENDSL
190900000000      *
191000000000     C     scdcaus1      IFEQ      *blanks
191100000000     C                   MOVE      CAU(I)        scdcaus1
191200000000     C                   END
191300000000     C                   MOVE      CAU(I)        scdcaus2
191400000000      *
191500000000     C     x20write      IFNE      *BLANK
191600000000     C     x20parsca     ANDNE     'E'
191700000000D1557x*pulisco modalit” di pagamento per insoluti
191800000000D1557c                   if        ScdFLG3  = 'S'
191900000000     c                   clear                   scdmdpag
192000000000D1557c                   endif
192100000000     C                   CLEAR                   scdtitoli
192200000000     C                   CLEAR                   scdnreff
192300000000     C                   CLEAR                   scdcaurisc
192400000000     C                   exsr      writescd
192500000000     C                   ENDIF
192600000000      *
192700000000     C     x20parscd     IFEQ      'S'
192800000000     C                   EXSR      PARESTESO
192900000000     C                   ENDIF
193000000000      *
193100000000     C                   END
193200000000     C                   END
193300000000     C                   ADD       1             I
193400000000      *
193500000000     C                   END
193600000000      *
193700000000     C                   ENDIF                                                  x20parsca
193800000000      *
193900000000      * Nel caso sia stato richiesto di scrivere su disco crea/aggiorna
194000000000      * file NDSCP01L
194100000000      *
194200000000     C     x20write      IFNE      *blanks
194300000000     C                   EXSR      SCRIVISCP
194400000000     C                   END
194500000000      *
194600000000      * Incrementa totali conto
194700000000     C                   ADD       wptdld        wctdld
194800000000     C                   ADD       wptald        wctald
194900000000     C                   ADD       wptddd        wctddd
195000000000     C                   ADD       wptadd        wctadd
195100000000     C                   ADD       wptdlp        wctdlp
195200000000     C                   ADD       wptalp        wctalp
195300000000     C                   ADD       wptddp        wctddp
195400000000     C                   ADD       wptadp        wctadp
195500000000     C                   ADD       wptdlg        wctdlg
195600000000     C                   ADD       wptalg        wctalg
195700000000     C                   ADD       wptddg        wctddg
195800000000     C                   ADD       wptadg        wctadg
195900000000     C                   ADD       wplscaduto    wclscaduto
196000000000     C                   ADD       wpdscaduto    wcdscaduto
196100000000     C                   ADD       wplascader    wclascader
196200000000     C                   ADD       wpdascader    wcdascader
196300000000     C                   ADD       wpportaf      wcportaf
196400000000     C                   ADD       wpdportaf     wcdportaf
196500000000     C                   ADD       wprisc        wcrisc
196600000000     C                   ADD       wpdrisc       wcdrisc
196700000000     C                   ADD       wpscains      wcscains
196800000000     C                   ADD       wpdscains     wcdscains
196900000000      *
197000000000     C                   ENDIF
197100000000      *
197200000000     C                   ENDSR
197300000000     C/EJECT
197400000000     C******************************************************
197500000000     C*  GESTIONE PARAMETRO ESTESO                         *
197600000000     C******************************************************
197700000000     C     PARESTESO     BEGSR
197800000000      *
197900000000     C                   ADD       1             INDEX
198000000000C1024C     index         ifgt      50
198100000000C1024C                   eval      index = 50
198200000000C1024C**                 eval      x20errori = 'C'
198300000000C1024C                   else
198400000000C1024C                   MOVE      scddtsca      DTSCAX(INDEX)
198500000000C1024C                   endif
198600000000C1024C                   add       scdimprmc     IMPRMC(INDEX)
198700000000C1024C                   add       scdimprd      IMPRD(INDEX)
198800000000C1024C                   add       scdimpomc     IMPOMC(INDEX)
198900000000C1024C                   add       scdimpod      IMPOD(INDEX)
199000000000     C                   MOVE      scdcaus1      CAUS1(INDEX)
199100000000     C                   MOVE      scdcaus2      CAUS2(INDEX)
199200000000      *
199300000000     C                   ENDSR
199400000000      *
199500000000     C******************************************************
199600000000     C*  SCRIVE RECORD NDSCP01L RELATIVO ALLA PARTITA      *
199700000000     C******************************************************
199800000000     C     SCRIVISCP     BEGSR
199900000000      *
200000000000     C                   MOVE      x20societa    scpsocieta
200100000000     C                   MOVE      ppaunita      scpunita
200200000000     C                   MOVE      x20ctb        scpctb
200300000000     C                   MOVE      x20KCC        SCPKCC
200400000000     C                   MOVE      x20KSC        SCPKSC
200500000000     C                   MOVE      x20snatura    scpsnatura
200600000000     C                   MOVE      'P'           scpdett
200700000000     C                   MOVE      ppadtpar      scpdtpar
200800000000     C                   Z-ADD     ppanrpar      scpnrpar
200900000000     C                   MOVE      ppaserpar     scpserpar
201000000000      *
201100000000     C                   Z-ADD     ppasys        scpsys
201200000000     C                   Z-ADD     ppanrasint    scpnrasint
201300000000     C                   Z-ADD     wptdld        scptdld
201400000000     C                   Z-ADD     wptald        scptald
201500000000     C                   Z-ADD     wptddd        scptddd
201600000000     C                   Z-ADD     wptadd        scptadd
201700000000     C                   Z-ADD     wptdlp        scptdlp
201800000000     C                   Z-ADD     wptalp        scptalp
201900000000     C                   Z-ADD     wptddp        scptddp
202000000000     C                   Z-ADD     wptadp        scptadp
202100000000     C                   Z-ADD     wptdlg        scptdlg
202200000000     C                   Z-ADD     wptalg        scptalg
202300000000     C                   Z-ADD     wptddg        scptddg
202400000000     C                   Z-ADD     wptadg        scptadg
202500000000     C                   MOVE      wdivisa       scpdivisa
202600000000     C                   MOVE      wdumd         scpdumd
202700000000     C                   MOVE      wdump         scpdump
202800000000     C                   MOVE      wdumg         scpdumg
202900000000     C                   Z-ADD     wivadare      scpivadare
203000000000     C                   Z-ADD     wivaaver      scpivaaver
203100000000     C                   Z-ADD     wimpins       scpimpins
203200000000     C                   Z-ADD     wpaginc       scppaginc
203300000000     C                   MOVE      wportafo      scpportaf
203400000000     C                   Z-ADD     wplscaduto    scplscadu
203500000000     C                   Z-ADD     wplascader    scplascad
203600000000     C                   Z-ADD     wpdscaduto    scpdscadu
203700000000     C                   Z-ADD     wpdascader    scpdascad
203800000000     C                   MOVE      wudate        scpdata
203900000000     C                   TIME                    scptime
204000000000     C                   MOVE      x20utente     scputente
204100000000     C                   CLEAR                   scpalloc
204200000000     C                   CLEAR                   scpflg
204300000000      *
204400000000      * Flag di rischio
204500000000     C     wprisc        IFNE      0
204600000000     C                   MOVE      'S'           scpflg1
204700000000     C                   END
204800000000      * Flag di insoluti in corso
204900000000     C     wpscains      IFNE      0
205000000000     C                   MOVE      'S'           scpflg3
205100000000     C                   END
205200000000      *
205300000000     C                   exsr      writescp
205400000000      *
205500000000     C                   ENDSR
205600000000     C/EJECT
205700000000     C******************************************************
205800000000     C*  IMPOSTA DATI DA RITORNARE AL CHIAMANTE            *
205900000000     C******************************************************
206000000000     C     PARRITORNO    BEGSR
206100000000      *
206200000000      * Imposta totali Dare e Avere in m.c.
206300000000     C                   Z-ADD     wctdld        x20tdld
206400000000     C                   Z-ADD     wctald        x20tald
206500000000     C                   Z-ADD     wctdlp        x20tdlp
206600000000     C                   Z-ADD     wctalp        x20talp
206700000000     C                   Z-ADD     wctdlg        x20tdlg
206800000000     C                   Z-ADD     wctalg        x20talg
206900000000      * Imposta le date dei movimenti limite
207000000000     C                   MOVE      wdumd         x20dumd
207100000000     C                   MOVE      wdump         x20dump
207200000000     C                   MOVE      wdumg         x20dumg
207300000000      * Scaduto / a scadere
207400000000     C                   Z-ADD     wclscaduto    x20lscadu
207500000000     C                   Z-ADD     wclascader    x20lascad
207600000000      * IVA dare/avere, insoluti, incassi/pagamenti
207700000000     C                   Z-ADD     wivadare      x20ivadare
207800000000     C                   Z-ADD     wivaaver      x20ivaaver
207900000000     C                   Z-ADD     wimpins       x20impins
208000000000     C                   Z-ADD     wpaginc       x20paginc
208100000000      * Rischio/esposizione/portafoglio
208200000000     C                   Z-ADD     wcrisc        x20risc
208300000000     C     x20lscadu     ADD       x20lascad     x20espos
208400000000     C                   Z-ADD     wcportaf      x20portaf
208500000000     C                   Z-ADD     wcscains      x20scains
208600000000      * Griglia scadenze se richiesto
208700000000     C     x20tabcf      IFNE      *blanks
208800000000     C                   Z-ADD     IMPcf(1)      x20prd01
208900000000     C                   Z-ADD     IMPcf(2)      x20prd02
209000000000     C                   Z-ADD     IMPcf(3)      x20prd03
209100000000     C                   Z-ADD     IMPcf(4)      x20prd04
209200000000     C                   Z-ADD     IMPcf(5)      x20prd05
209300000000     C                   Z-ADD     IMPcf(6)      x20prd06
209400000000     C                   Z-ADD     IMPcf(7)      x20prd07
209500000000     C                   Z-ADD     IMPcf(8)      x20prd08
209600000000     C     x20nrpar      ifne      0
209700000000     C                   Z-ADD     imdcf(1)      x20prd01D
209800000000     C                   Z-ADD     imdcf(2)      x20prd02D
209900000000     C                   Z-ADD     imdcf(3)      x20prd03D
210000000000     C                   Z-ADD     imdcf(4)      x20prd04D
210100000000     C                   Z-ADD     imdcf(5)      x20prd05D
210200000000     C                   Z-ADD     imdcf(6)      x20prd06D
210300000000     C                   Z-ADD     imdcf(7)      x20prd07D
210400000000     C                   Z-ADD     imdcf(8)      x20prd08D
210500000000     C                   endif
210600000000     C                   ENDIF
210700000000      *
210800000000     C                   CLEAR                   x20flg9
210900000000D1955c                   eval      x20fl1=wcflgrit
211000000000      *
211100000000      * Imposta dati di sola partita o di tutte partite di una divisa
211200000000     C     x20dtpar      IFNE      DATMIN
211300000000     C     x20nrpar      ANDNE     *zeros
211400000000     C     x20divr       ORNE      *blanks                                      Divisa specifica
211500000000      * Divisa
211600000000     C                   MOVE      wdivisa       x20divisa
211700000000      * Totali Dare e Avere in divisa
211800000000     C                   Z-ADD     wctddd        x20tddd
211900000000     C                   Z-ADD     wctadd        x20tadd
212000000000     C                   Z-ADD     wctddp        x20tddp
212100000000     C                   Z-ADD     wctadp        x20tadp
212200000000     C                   Z-ADD     wctddg        x20tddg
212300000000     C                   Z-ADD     wctadg        x20tadg
212400000000      * Scaduto divisa e a scadere divisa
212500000000     C                   Z-ADD     wcdscaduto    x20dscadu
212600000000     C                   Z-ADD     wcdascader    x20dascad
212700000000     C                   Z-ADD     wcdportaf     x20dportaf
212800000000     C                   Z-ADD     wcdrisc       x20drisc
212900000000     C                   Z-ADD     wcdscains     x20dscains
213000000000      *
213100000000     C                   Z-ADD     wnrmov        x20nrmov
213200000000     C                   END
213300000000      *
213400000000      * Gestione parametro esteso
213500000000     C     x20parscd     IFEQ      'S'
213600000000     C                   Z-ADD     INDEX         X20INDEX
213700000000     C                   ENDIF
213800000000      *
213900000000      *
214000000000      * Flag stato elaborazione
214100000000C1024C                   MOVE      *BLANK        x20errori
214200000000      *
214300000000     C                   ENDSR
214400000000     C/EJECT
214500000000     C******************************************************
214600000000     C* CARICAMENTO SCHIERE CAUSALI                        *
214700000000     C******************************************************
214800000000     C     CAROPE        BEGSR
214900000000     C*****************************
215000000000     C* Causali dare              *
215100000000     C*****************************
215200000000     C     pasdare       IFEQ      1
215300000000     C*
215400000000     C                   select
215500000000     C* Fattura
215600000000     C     opetpc        wheneq    '1'
215700000000     C                   MOVE      pascausrig    CFDCOD
215800000000D2247C                   MOVE      pascausrig    CFDCODSav
215900000000     C* Portafoglio dare
216000000000D0852C* - creazione portafoglio
216100000000     C     opeportaf     wheneq    '1'
216200000000D0852C* - effetti passivi ritirati
216300000000  "  C     opestp        oreq      '2'
216400000000D0852C     opeportaf     andeq     '5'
216500000000     C                   MOVE      pascausrig    CPDCOD
216600000000     C* Pagamento a scadere
216700000000     C     opecalscad    wheneq    '2'
216800000000     C                   MOVE      pascausrig    CPRCOD
216900000000     C* Insoluto
217000000000     C     opeportaf     wheneq    '3'
217100000000     C     opestp        oreq      '3'
217200000000     C                   MOVE      pascausrig    CINCOD
217300000000     C* Pagamento o giroconto
217400000000     C     opetpdocni    wheneq    '0'
217500000000     C     opetpdocni    oreq      '4'
217600000000     C                   MOVE      pascausrig    CPGCOD
217700000000     C* Differenza cambio
217800000000     C     opetpdocni    wheneq    '6'
217900000000     C                   MOVE      pascausrig    CDCCOD
218000000000     C* Incassi (compensazioni di note credito)
218100000000     C     opetpdocni    wheneq    '1'
218200000000     C                   MOVE      pascausrig    CICCOD
218300000000     C* Varie
218400000000     C                   other
218500000000     C                   MOVE      pascausrig    CVDCOD
218600000000     C*
218700000000     C                   endsl
218800000000     C*****************************
218900000000     C* Causali avere             *
219000000000     C*****************************
219100000000     C                   ELSE
219200000000     C*
219300000000     C                   select
219400000000     C* Fattura
219500000000     C     opetpc        wheneq    '1'
219600000000     C                   MOVE      pascausrig    CFACOD
219700000000D2247C                   MOVE      pascausrig    CFACODSav
219800000000     C* Portafoglio avere
219900000000D0852C* - creazione portafoglio
220000000000     C     opeportaf     wheneq    '1'
220100000000D0852C* - effetti passivi ritirati
220200000000  "  C     opestp        oreq      '2'
220300000000D0852C     opeportaf     andeq     '5'
220400000000     C                   MOVE      pascausrig    CPACOD
220500000000     C* Incassi a scadere
220600000000     C     opecalscad    wheneq    '2'
220700000000     C                   MOVE      pascausrig    CPRCOD
220800000000     C* Incasso o giroconto
220900000000     C     opetpdocni    wheneq    '1'
221000000000     C     opetpdocni    oreq      '4'
221100000000     C                   MOVE      pascausrig    CICCOD
221200000000     C* Differenza cambio
221300000000     C     opetpdocni    wheneq    '6'
221400000000     C                   MOVE      pascausrig    CDCCOD
221500000000     C* Pagamento (compensazioni di note credito)
221600000000     C     opetpdocni    wheneq    '0'
221700000000     C                   MOVE      pascausrig    CPGCOD
221800000000     C* Insoluto
221900000000     C* (Di un titolo che aveva chiuso anche una nota credito)
222000000000     C     opeportaf     wheneq    '3'
222100000000     C     opestp        oreq      '3'
222200000000     C                   MOVE      pascausrig    CINCOD
222300000000      * Varie
222400000000     C                   other
222500000000     C                   MOVE      pascausrig    CVACOD
222600000000      *
222700000000     C                   endsl
222800000000      *
222900000000     C                   END
223000000000      *
223100000000     C                   ENDSR
223200000000     C/EJECT
223300000000     C******************************************************
223400000000     C*  SBLOCCA RECORD CONTO   PER PERMETTERE ALTRI       *
223500000000     C*  AGGIORNAMENTI                                     *
223600000000     C******************************************************
223700000000     C     RILCONTO      BEGSR
223800000000     C*
223900000000     C                   MOVE      x20KCC        keykcc
224000000000     C                   MOVE      x20KSC        keyksc
224100000000     C                   MOVE      'S'           SCPDEW
224200000000     C                   MOVE      DATMIN        keydtpar
224300000000     C                   CLEAR                   keynrpar
224400000000     C                   CLEAR                   keyserpar
224500000000     C     KSCPCP        CHAIN     NDSCP01L                           33
224600000000     C     *IN33         IFEQ      '0'
224700000000     C                   MOVE      wudate        scpdata
224800000000     C                   TIME                    scptime
224900000000     C                   MOVE      x20utente     scputente
225000000000     C                   CLEAR                   scpalloc
225100000000     C                   EXCEPT    FINSCP
225200000000     C                   END
225300000000     C*
225400000000     C                   ENDSR
225500000000     C/EJECT
225600000000      ******************************************************
225700000000      *  Cancella precedente situazione scadenze e         *
225800000000      *  partite del conto richiesto                       *
225900000000      ******************************************************
226000000000     C     DLTSCDSCP     BEGSR
226100000000      *
226200000000     C                   MOVE      x20KCC        keykcc
226300000000     C                   MOVE      x20KSC        keyksc
226400000000     C                   MOVE      'P'           SCPDEW
226500000000     C                   MOVE      x20dtpar      keydtpar
226600000000     C                   Z-ADD     x20nrpar      keynrpar
226700000000     C                   MOVE      x20serpar     keyserpar
226800000000      *
226900000000     C                   SELECT
227000000000      *
227100000000     C     x20dtpar      WHENEQ    DATMIN
227200000000     C     x20nrpar      ANDEQ     *zeros
227300000000      *
227400000000     C     *IN33         DOUEQ     '1'
227500000000     C     KSCDC         DELETE    NDSCD01L                           33
227600000000     C                   END
227700000000      *
227800000000     C     *IN33         DOUEQ     '1'
227900000000     C     KSCPC         DELETE    NDSCP01L                           33
228000000000     C                   END
228100000000      *
228200000000     C                   OTHER
228300000000      *
228400000000     C     *IN33         DOUEQ     '1'
228500000000     C     KSCDCP        DELETE    NDSCD01L                           33
228600000000     C                   END
228700000000      *
228800000000     C     *IN33         DOUEQ     '1'
228900000000     C     KSCPCP        DELETE    NDSCP01L                           33
229000000000     C                   END
229100000000      *
229200000000     C                   ENDSL
229300000000      *
229400000000     C                   ENDSR
229500000000     C/EJECT
229600000000     C******************************************************
229700000000     C*  VERIFICA SE SI POSSONO AGGIORNARE I DATI RELATIVI *
229800000000     C*  AL CONTO   RICHIESTO                              *
229900000000     C******************************************************
230000000000     C     TESTCONTO     BEGSR
230100000000      *
230200000000     C                   MOVE      x20KCC        keykcc
230300000000     C                   MOVE      x20KSC        keyksc
230400000000     C                   MOVE      'S'           SCPDEW
230500000000     C                   MOVE      DATMIN        keydtpar
230600000000     C                   CLEAR                   keynrpar
230700000000     C                   CLEAR                   keyserpar
230800000000     C     KSCPCP        CHAIN     NDSCP01L                           33
230900000000      * Crea record NDSCP01L se inesistente
231000000000     C     *IN33         IFEQ      '1'
231100000000     C                   CLEAR                   NDSCP000
231200000000     C                   MOVE      x20societa    scpsocieta
231300000000     C                   MOVE      x20ctb        scpctb
231400000000     C                   MOVE      x20kcc        scpkcc
231500000000     C                   MOVE      x20ksc        scpksc
231600000000     C                   MOVE      x20snatura    scpsnatura
231700000000     C                   MOVE      DATMIN        scpdtpar
231800000000     C                   MOVE      DATMIN        scpdumd
231900000000     C                   MOVE      DATMIN        scpdump
232000000000     C                   MOVE      DATMIN        scpdumg
232100000000     C                   MOVE      wudate        scpdata
232200000000     C                   MOVE      'S'           scpdett
232300000000     C                   MOVE      x20utente     scputente
232400000000     C                   MOVE      'A'           scpalloc
232500000000      *
232600000000     C                   exsr      writescp
232700000000      *
232800000000     C     *IN33         IFEQ      '1'
232900000000     C                   MOVE      'A'           x20errori
233000000000     C                   RETURN
233100000000     C                   END
233200000000     C                   ELSE
233300000000      * Se il record ﬁ gi” in fase di aggiornamento chiude programma
233400000000      * solo se la data ﬁ la stessa
233500000000     C     scpalloc      IFEQ      'A'
233600000000     C     scpdata       ANDEQ     wudate
233700000000     C                   MOVE      'A'           x20errori
233800000000     C                   unlock    ndscp01l
233900000000     C                   RETURN
234000000000      * Altrimenti lo blocca per impedire altri tentativi di aggiorn.
234100000000     C                   ELSE
234200000000     C                   MOVE      'A'           scpalloc
234300000000     C                   EXCEPT    AGGSCP
234400000000     C                   END
234500000000     C                   END
234600000000      *
234700000000     C                   ENDSR
234800000000     C/EJECT
234900000000     C******************************************************
235000000000     C*  RICHIAMA PGM X19DTRISR: date rischio              *
235100000000     C******************************************************
235200000000     C     CALRISCHIO    BEGSR
235300000000      *
235400000000     C     x20parsca     IFNE      'P'
235500000000      * Richiama X19DTRISR per calcolo date rischio
235600000000     C                   MOVE      x20societa    socx19
235700000000     C                   MOVE      x20dtsca      dtrx19
235800000000     C                   CALL      'X19DTRISR'
235900000000     C                   PARM                    X19DTRISDS
236000000000     C                   MOVEA     caux19        CODRIS
236100000000     C                   MOVEA     darx19        skalf
236200000000     C                   MOVE      skalf         DATRIS
236300000000     C                   ELSE
236400000000     C                   MOVE      x20dtsca      SavDtsca
236500000000     C                   END
236600000000      *
236700000000     C                   ENDSR
236800000000     C/EJECT
236900000000     C******************************************************
237000000000     C*  RICHIAMA PGM X18DTSCA:  date per griglia          *
237100000000     C******************************************************
237200000000     C     CALDTSCA      BEGSR
237300000000      *
237400000000     C                   MOVE      x20dtsca      x18datrif
237500000000     C                   MOVE      x20tabcf      x18codtab
237600000000     C                   CALL      'X18DTSCA'
237700000000     C                   PARM                    X18DTSCADS
237800000000     C     x18errore     IFEQ      *off
237900000000     C                   MOVE      x18datd1      DATEDA(1)
238000000000     C                   MOVE      x18datd2      DATEDA(2)
238100000000     C                   MOVE      x18datd3      DATEDA(3)
238200000000     C                   MOVE      x18datd4      DATEDA(4)
238300000000     C                   MOVE      x18datd5      DATEDA(5)
238400000000     C                   MOVE      x18datd6      DATEDA(6)
238500000000     C                   MOVE      x18datd7      DATEDA(7)
238600000000     C                   MOVE      x18datd8      DATEDA(8)
238700000000     C                   MOVE      x18data1      DATEA(1)
238800000000     C                   MOVE      x18data2      DATEA(2)
238900000000     C                   MOVE      x18data3      DATEA(3)
239000000000     C                   MOVE      x18data4      DATEA(4)
239100000000     C                   MOVE      x18data5      DATEA(5)
239200000000     C                   MOVE      x18data6      DATEA(6)
239300000000     C                   MOVE      x18data7      DATEA(7)
239400000000     C                   MOVE      x18data8      DATEA(8)
239500000000     C                   ELSE
239600000000     C                   MOVE      DATMIN        DATEDA
239700000000     C                   MOVE      DATMIN        DATEA
239800000000     C                   ENDIF
239900000000      *
240000000000     C                   ENDSR
240100000000     C******************************************************
240200000000     C*  GESTIONE GRIGLIA DELLE SCADENZE                   *
240300000000     C******************************************************
240400000000     C     GRIGLIA       BEGSR
240500000000      *
240600000000     C     1             DO        8             ICX
240700000000     C     wgdata        IFGE      DATEDA(ICX)
240800000000     C     wgdata        ANDLE     DATEA(ICX)
240900000000     C                   ADD       wgimporto     IMPcf(ICX)
241000000000     C                   ADD       wgimportd     imdcf(ICX)
241100000000D2211C                   Leave
241200000000     C                   END
241300000000     C                   END
241400000000      *
241500000000     C                   ENDSR
241600000000     C/EJECT
241700000000      ******************************************************
241800000000      *  Sort delle schiere per scdenza                    *
241900000000      ******************************************************
242000000000     C     sortadsc      begsr
242100000000      *
242200000000     C                   reset                   sort
242300000000      *
242400000000     C                   do        wmaxida       s
242500000000     C                   eval      sortdsc       = dsc(s)
242600000000     C                   eval      sortiml       = iml(s)
242700000000     C                   eval      sortimd       = imd(s)
242800000000     C                   eval      sortcau       = cau(s)
242900000000     C                   eval      sortnas       = nas(s)
243000000000     C                   eval      sortnrt       = nrt(s)
243100000000D2085C                   eval      sortmp        = mp(s)
243200000000D2085C                   eval      sortmd        = md(s)
243300000000     C                   eval      sort(s)       = sortds
243400000000     C                   enddo
243500000000      *
243600000000     C                   sorta     sort
243700000000      *
243800000000     C                   do        wmaxida       s
243900000000     C                   eval      sortds        = sort(s)
244000000000     C                   eval      dsc(s)        = sortdsc
244100000000     C                   eval      iml(s)        = sortiml
244200000000     C                   eval      imd(s)        = sortimd
244300000000     C                   eval      cau(s)        = sortcau
244400000000     C                   eval      nas(s)        = sortnas
244500000000     C                   eval      nrt(s)        = sortnrt
244600000000D2085C                   eval      mp(s)         = sortmp
244700000000D2085C                   eval      md(s)         = sortmd
244800000000     C                   enddo
244900000000      *
245000000000     C                   endsr
245100000000      /EJECT
245200000000     C******************************************************
245300000000     C*  SCRIVE RECORD NDSCD01L                            *
245400000000     C******************************************************
245500000000     C     WRITESCD      BEGSR
245600000000     C
245700000000      * Flag di scadenzario retroattivo
245800000000     C                   eval      scdtpscad     = x20flgtpsc
245900000000      *
246000000000     C     scdimprmc     ifne      0
246100000000     C     scdimprd      orne      0
246200000000     C                   WRITE     NDSCD2
246300000000     C                   endif
246400000000      *
246500000000     C                   ENDSR
246600000000      /EJECT
246700000000     C******************************************************
246800000000     C*  SCRIVE RECORD NDSCP000                            *
246900000000     C******************************************************
247000000000     C     WRITESCP      BEGSR
247100000000     C
247200000000      * Flag di scadenzario retroattivo
247300000000     C                   eval      scptpscad     = x20flgtpsc
247400000000      *
247500000000     C                   WRITE     NDSCP000                             33
247600000000      *
247700000000     C                   ENDSR
247800000000      /EJECT
247900000000D1955C******************************************************
248000000000     C* Vedo se esistono ritenute per la partita           *
248100000000D1955C******************************************************
248200000000     C     MemRitenut    BEGSR
248300000000     c*
248400000000     C                   CLEAR                   wpFlgRit
248500000000     c*
248600000000     c* fornitore percipiente
248700000000     c                   if        x20snatura = 'F'
248800000000     C                   eval      mrasocieta  = x20societa
248900000000     C                   eval      mrakcc  = x20kcc
249000000000     C                   eval      mraksc  = x20ksc
249100000000     C     K06mra04      setll     NDmra04L                               21
249200000000     C                   if        *in21 = *on
249300000000     C                   eval      wpflgrit= *on
249400000000     C                   endif
249500000000     C                   endif
249600000000     C*
249700000000D1955C                   ENDSR
249800000000     C******************************************************
249900000000     C* Reperimento anagrafica causale contabile           *
250000000000     C******************************************************
250100000000     C     CALLMVC003    BEGSR
250200000000     c*
250300000000     c* Reperisco dati della causale
250400000000     c*
250500000000     C                   CLEAR                   ANOPE
250600000000     C                   EVAL      LenOut = %Size(ANOPE)
250700000000     C                   eval      data = datmin
250800000000     C                   CallB     'NDMVC003'
250900000000     C                   PARM                    Societa
251000000000     C                   PARM                    causale
251100000000     C                   PARM                    data
251200000000     C                   PARM                    *omit
251300000000     C                   PARM      '0'           GesMsg
251400000000     C                   PARM      '0'           Esito
251500000000     C                   PARM      'ANOPE'       StrutturaO
251600000000     C                   PARM                    ANOPE
251700000000     C                   PARM                    LenOut
251800000000     C                   PARM      '0'           Refresh
251900000000     c*
252000000000     C                   ENDSR
252100000000     C/EJECT
252200000000      ******************************************************
252300000000      *  ROUTINE DI APERTURA PROGRAMMA                     *
252400000000      ******************************************************
252500000000     C     *INZSR        BEGSR
252600000000      *
252700000000C1309C     *DMY          Move      Udate         ZDateISO
252800000000C1309C     *JobRun       Move      ZDateISO      ZDate             6 0
252900000000      *****************************
253000000000      *   elenco KLIST usate      *
253100000000      *****************************
253200000000      * KLIST per file ANOPE01L (Causali contabili)
253300000000     C     k02ope01      KLIST
253400000000     C                   KFLD                    x20societa
253500000000     C                   KFLD                    opecausale
253600000000      * KLIST per file NDPPA0xL (Conto)
253700000000     C     kppaconto     KLIST
253800000000     C                   KFLD                    x20societa
253900000000     C                   KFLD                    x20ctb
254000000000     C                   KFLD                    keykcc
254100000000     C                   KFLD                    keyksc
254200000000      * KLIST per file NDPPA0xL (Conto/Partita)
254300000000     C     kppapartit    KLIST
254400000000     C                   KFLD                    x20societa
254500000000     C                   KFLD                    x20ctb
254600000000     C                   KFLD                    keykcc
254700000000     C                   KFLD                    keyksc
254800000000     C                   KFLD                    ppadtpar
254900000000     C                   KFLD                    ppanrpar
255000000000     C                   KFLD                    ppaserpar
255100000000      * KLIST per file NDPAS04L (Conto/Partita)
255200000000     C     kpaspartit    KLIST
255300000000     C                   KFLD                    x20societa
255400000000     C                   KFLD                    x20ctb
255500000000     C                   KFLD                    keykcc
255600000000     C                   KFLD                    keyksc
255700000000     C                   KFLD                    ppadtpar
255800000000     C                   KFLD                    ppanrpar
255900000000     C                   KFLD                    ppaserpar
256000000000      * KLIST per file NDEFA01L
256100000000     C     KEFA          KLIST
256200000000     C                   KFLD                    passys
256300000000     C                   KFLD                    pasnrasreg
256400000000      * KLIST per file NDEFP01L
256500000000     C     KEFP          KLIST
256600000000     C                   KFLD                    passys
256700000000     C                   KFLD                    pasnrasreg
256800000000      * KLIST per file NDSCP01L (Conto)
256900000000     C     KSCPC         KLIST
257000000000     C                   KFLD                    x20societa
257100000000     C                   KFLD                    x20ctb
257200000000     c                   kfld                    x20flgtpsc
257300000000     C                   KFLD                    SCPDEW
257400000000     C                   KFLD                    keykcc
257500000000     C                   KFLD                    keyksc
257600000000      * KLIST per file NDSCP01L (Conto/Partita)
257700000000     C     KSCPCP        KLIST
257800000000     C                   KFLD                    x20societa
257900000000     C                   KFLD                    x20ctb
258000000000     c                   kfld                    x20flgtpsc
258100000000     C                   KFLD                    SCPDEW
258200000000     C                   KFLD                    keykcc
258300000000     C                   KFLD                    keyksc
258400000000     C                   KFLD                    keydtpar
258500000000     C                   KFLD                    keynrpar
258600000000     C                   KFLD                    keyserpar
258700000000      * KLIST per file NDSCD01L (Conto)
258800000000     C     KSCDC         KLIST
258900000000     C                   KFLD                    x20societa
259000000000     C                   KFLD                    x20ctb
259100000000     C                   KFLD                    x20flgtpsc
259200000000     C                   KFLD                    keykcc
259300000000     C                   KFLD                    keyksc
259400000000      * KLIST per file NDSCD01L (Conto/Partita)
259500000000     C     KSCDCP        KLIST
259600000000     C                   KFLD                    x20societa
259700000000     C                   KFLD                    x20ctb
259800000000     c                   kfld                    x20flgtpsc
259900000000     C                   KFLD                    keykcc
260000000000     C                   KFLD                    keyksc
260100000000     C                   KFLD                    keydtpar
260200000000     C                   KFLD                    keynrpar
260300000000     C                   KFLD                    keyserpar
260400000000      * KLIST per file NDMOV01L
260500000000     C     KMOV          KLIST
260600000000     C                   KFLD                    passys
260700000000     C                   KFLD                    pasnrasreg
260800000000     C                   KFLD                    pasnrrigam
260900000000D1955C* Ndmra04l
261000000000  "  C     K06mra04      KLIST
261100000000  "  C                   KFLD                    mrasocieta
261200000000  "  C                   KFLD                    mraKCC
261300000000  "  C                   KFLD                    mraKSC
261400000000  "  C                   KFLD                    mraDRP
261500000000  "  C                   KFLD                    mranrp
261600000000D1955C                   KFLD                    mraSERPAR
261700000000      *****************************
261800000000      * defin.dei campi di lavoro *
261900000000      *****************************
262000000000     C     *LIKE         DEFINE    paskcc        keykcc
262100000000     C     *LIKE         DEFINE    pasksc        keyksc
262200000000     C     *LIKE         DEFINE    pasnrpar      keynrpar
262300000000     C     *LIKE         DEFINE    passerpar     keyserpar
262400000000      *
262500000000     C     *LIKE         DEFINE    scptdld       wptdld
262600000000     C     *LIKE         DEFINE    scptald       wptald
262700000000     C     *LIKE         DEFINE    scptdlp       wptdlp
262800000000     C     *LIKE         DEFINE    scptalp       wptalp
262900000000     C     *LIKE         DEFINE    scptdlg       wptdlg
263000000000     C     *LIKE         DEFINE    scptalg       wptalg
263100000000     C     *LIKE         DEFINE    scptddd       wptddd
263200000000     C     *LIKE         DEFINE    scptadd       wptadd
263300000000     C     *LIKE         DEFINE    scptddp       wptddp
263400000000     C     *LIKE         DEFINE    scptadp       wptadp
263500000000     C     *LIKE         DEFINE    scptddg       wptddg
263600000000     C     *LIKE         DEFINE    scptadg       wptadg
263700000000      *
263800000000     C     *LIKE         DEFINE    scptdld       wctdld
263900000000     C     *LIKE         DEFINE    scptald       wctald
264000000000     C     *LIKE         DEFINE    scptdlp       wctdlp
264100000000     C     *LIKE         DEFINE    scptalp       wctalp
264200000000     C     *LIKE         DEFINE    scptdlg       wctdlg
264300000000     C     *LIKE         DEFINE    scptalg       wctalg
264400000000     C     *LIKE         DEFINE    scptddd       wctddd
264500000000     C     *LIKE         DEFINE    scptadd       wctadd
264600000000     C     *LIKE         DEFINE    scptddp       wctddp
264700000000     C     *LIKE         DEFINE    scptadp       wctadp
264800000000     C     *LIKE         DEFINE    scptddg       wctddg
264900000000     C     *LIKE         DEFINE    scptadg       wctadg
265000000000      *
265100000000     C     *LIKE         DEFINE    pascausrig    CFDCOD
265200000000     C     *LIKE         DEFINE    pascausrig    CFACOD
265300000000     C     *LIKE         DEFINE    pascausrig    CINCOD
265400000000     C     *LIKE         DEFINE    pascausrig    CPACOD
265500000000     C     *LIKE         DEFINE    pascausrig    CPDCOD
265600000000     C     *LIKE         DEFINE    pascausrig    CVDCOD
265700000000     C     *LIKE         DEFINE    pascausrig    CVACOD
265800000000     C     *LIKE         DEFINE    pascausrig    CPRCOD
265900000000     C     *LIKE         DEFINE    pascausrig    CICCOD
266000000000     C     *LIKE         DEFINE    pascausrig    CPGCOD
266100000000     C     *LIKE         DEFINE    pascausrig    CDCCOD
266200000000     C     *LIKE         DEFINE    opecausale    OPECOW
266300000000     C     *LIKE         DEFINE    pasimporto    wplscaduto
266400000000     C     *LIKE         DEFINE    pasimporto    wplascader
266500000000     C     *LIKE         DEFINE    pasimportd    wpdscaduto
266600000000     C     *LIKE         DEFINE    pasimportd    wpdascader
266700000000     C     *LIKE         DEFINE    pasimporto    wclscaduto
266800000000     C     *LIKE         DEFINE    pasimporto    wclascader
266900000000     C     *LIKE         DEFINE    pasimportd    wcdscaduto
267000000000     C     *LIKE         DEFINE    pasimportd    wcdascader
267100000000     C     *LIKE         DEFINE    x20write      SAVWRT
267200000000     C     *LIKE         DEFINE    x20tabcf      savtabcf
267300000000     C     *LIKE         DEFINE    scpdett       SCPDEW
267400000000     C     *LIKE         DEFINE    pasdivisa     wdivisa
267500000000     C     *LIKE         DEFINE    scpimpins     wimpins
267600000000     C     *LIKE         DEFINE    scpivadare    wivadare
267700000000     C     *LIKE         DEFINE    scpivaaver    wivaaver
267800000000     C     *LIKE         DEFINE    scpportaf     wportafo
267900000000     C     *LIKE         DEFINE    x20portaf     wpportaf
268000000000     C     *LIKE         DEFINE    x20portaf     wcportaf
268100000000     C     *LIKE         DEFINE    x20dportaf    wpdportaf
268200000000     C     *LIKE         DEFINE    x20dportaf    wcdportaf
268300000000     C     *LIKE         DEFINE    x20scains     wpscains
268400000000     C     *LIKE         DEFINE    x20scains     wcscains
268500000000     C     *LIKE         DEFINE    x20dscains    wpdscains
268600000000     C     *LIKE         DEFINE    x20dscains    wcdscains
268700000000     C     *LIKE         DEFINE    scppaginc     wpaginc
268800000000     C     *LIKE         DEFINE    pasimporto    wprisc
268900000000     C     *LIKE         DEFINE    pasimporto    wcrisc
269000000000     C     *LIKE         DEFINE    pasimportd    wpdrisc
269100000000     C     *LIKE         DEFINE    pasimportd    wcdrisc
269200000000     C     *LIKE         DEFINE    pasimporto    wgimporto
269300000000     C     *LIKE         DEFINE    pasimportd    wgimportd
269400000000     C     *LIKE         DEFINE    x20nrmov      wnrmov
269500000000     C     *LIKE         DEFINE    pasimporto    wplincascp
269600000000     C     *LIKE         DEFINE    pasimporto    wplincascd
269700000000     C     *LIKE         DEFINE    pasimportd    wpdincascp
269800000000     C     *LIKE         DEFINE    pasimportd    wpdincascd
269900000000D2496C     *LIKE         DEFINE    CFDCod        CFDCodSav
270000000000D2496C     *LIKE         DEFINE    CFACod        CFACodSav
270100000000      * Numero massimo elementi schiere IML, IMD, DSC
270200000000     C                   Z-ADD     100           WMAXEL            3 0
270300000000     C                   Z-ADD     0             ICX               1 0
270400000000      * Apre files
270500000000     C                   OPEN      NDPAS04L
270600000000     C                   OPEN      NDMOV01L
270700000000     C                   OPEN      NDEFA01L
270800000000     C                   OPEN      NDEFP01L
270900000000      *
271000000000      * Imposta data del giorno nella forma AAAA/MM/GG
271100000000     C
271200000000     C                   CALLB     'XDT4000'
271300000000     C     WUDATE        PARM                    XDTAMG           10
271400000000C1309C*****              PARM      UDATE         XDTGMA            6 0
271500000000C1309C                   PARM      ZDate         XDTGMA            6 0
271600000000     C                   PARM      4             XDTSTA            1 0
271700000000      *
271800130618>>>>> * Modifica per togliere gli effetti a scadere
271900130618>>>>> * dalle griglie di Amerighi ('GENN'-'DICE').
272000130618>>>>> * YOmEfaAsc *ON :     omettere gli effetti a scadere.
272100130618>>>>> * YOmEfaAsc *OFF: non omettere gli effetti a scadere.
272200130618>>>>>C                   MOVEL     *OFF          YOmEfaAsc         1
272300130618>>>>>C     *DTAARA       DEFINE                  YOmEfaAsc
272400130618>>>>>C                   IN(E)     YOmEfaAsc
272500130618>>>>> *
272600000000     C                   ENDSR
272700000000     C/EJECT
272800000000      * EXCPT per allocazione
272900000000     Ondscp000  E            AGGSCP
273000000000     O                       scpalloc
273100000000      * EXCPT per sblocco finale conto
273200000000     Ondscp000  E            FINSCP
273300000000     O                       scpdata
273400000000     O                       scptime
273500000000     O                       scputente
273600000000     O                       scpalloc
