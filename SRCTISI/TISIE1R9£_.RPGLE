000100000000     H*------------------------------------------------------------------------*
000200151021     H* PULIZIA FILE LEGAMI DPPDC  DEPOT DPD
000300000000     H*------------------------------------------------------------------------*
000400151026     H DECEDIT('0,') DATEDIT(*YMD.)
000500060427     F*------------------------------------------------------------------------*
000600060427     F* DATA BASE
000700060427     F*------------------------------------------------------------------------*
000800151026     FDPCVE02L  IF   E           K DISK
000900151026     FDPCDP01L  IF   E           K DISK
001000151026     FTABEL00F  IF   E           K DISK
001100151026     FCNIND00F  IF   E           K DISK
001200151021     FDPPDC01L  UF A E           K DISK
001300151026     D*------------------------------------------------------------------------*
001400060427     D* DS PARAMETRI DI LANCIO PGM
001500060427     D*-------------------
001600060427     D PARAM           DS                  INZ
001700060427     D  PARVER                        5  0
001800000000     D*-------------------
001900000000     D* ARCHITETTURA
002000000000     D*-------------------
002100000000     D KPJBA         E DS
002200151026     D*----------------
002300151027     D    wrkEsito     s              1
002400151026     D    wrkEml       s            253
002500151027     D    wrkEmlcc     s            253
002600151026     D    wrkOgg       s             44
002700151026     D    wrkMsg       s           5000
002800151026     D   MsgError      s           5000
002900151026     D*----------------
003000151026     D* DS ESTERNE
003100151026     D*-------------------
003200151026     D DpdcFLO       E DS
003300151026     D DS15          E DS                  INZ
003400151026     D skNaz           S              2    DIM(1000)
003500151026     D skNazKey        S              3    DIM(1000)
003600151026     D jNaz            S              4  0 INZ
003700060427     D*------------------
003800151026     D DATA_eur        S               D   DATFMT(*eur)
003900151026     D DATA_iso        S               D   DATFMT(*iso)
004000151026     D DATA_sca        S               D   DATFMT(*iso)
004100151026     D DATA_udate      S               D   DATFMT(*iso)
004200151026     D DATA_oggi       S              8s 0
004300000000     C*------------------------------------------------------------------------*
004400000000     C* MAIN LINE
004500000000     C*------------------------------------------------------------------------*
004600060427     C*
004700151026     C* Scorro tutto il file
004800151026     C     *START        SETLL     DPPDC01L
004900151026     C                   READ      DPPDC01L
005000151026     C                   DOW       not %eof(DPPDC01L)
005100151026     C*
005200151026     C* Pulisce i records scaduti da oltre 3 mesi
005300151026     c     *iso          movel     pdcdsc        DATA_iso
005400151026     C*
005500151026     c                   if        DATA_iso < DATA_sca
005600151026     C                   delete    DPPDC000
005700151026     c                   else
005800151026     C*
005900151026     C* se non era già scaduta da oltre 3 mesi
006000151026     C*  controlliamo se deve essere fatta scadere se non più presente nel
006100151026     C*  attuale cappario di DPD.
006200151026     c     chkdepot      chain     dpcdp01l
006300151026     C*
006400151026     c                   if        not %Found(dpcdp01l)
006500151026     c                   z-add     DATA_oggi     pdcdsc
006600151026     C                   update    DPPDC000
006700151026     c                   else
006800151026     C*
006900151026     C* La nazione del Cappario
007000151026     c                   clear                   nazione           3
007100151026     C                   IF        CDPISO2 <> *blanks
007200151026     C                   Z-ADD     1             jNaz
007300151026     C     CDPISO2       LOOKUP    skNaz(jNaz)                            55
007400151026     C                   IF        %equal
007500151026     C                   EVAL       nazione = skNazKey(jNaz)
007600151026     c                   end
007700151026     C*
007800151026     C*   se trovato il cappario il DEPOT esiste ed è in vigore
007900151026     c                   if        pdcNAR = ' '
008000151026     C                               and Nazione <> *blank
008100151026     C* La nazione del Cappario
008200151026     C                   EVAL        pdcNAR = nazione
008300151026     C                   update    DPPDC000
008400151026     c                   end
008500151026     C*
008600151026     c                   else
008700151026     c                   end
008800151026     C*
008900151026     C*  controllo ed invio MAIL x LNP =0 o CAP e NAZIONE diversi sul Cappario DPD
009000151026     c                   movel     pdcflo        dpdcflo
009100151026     c                   if        §PDCFLOLNP = *blank or
009200151026     c                             §PDCFLOLNP = *zeros
009300151026     c                   eval      msgerror = %trim(msgerror) +':/N'+
009400151026     c                             'Depot: ' + PDCDPC + ' senza LNP '
009500151026     c                   end
009600151026     C*
009700151026     C*   se c'è il cliente posso verificare uguaglianza fra cap e/o Nazione
009800151026     C*   in anagrafica.
009900151026     c                   if        pdcKSC > 0
010000151026     C*
010100151026     c                   z-add     pdcksc        indksc
010200151026     c     Anagr_IND     chain     CNIND00F
010300151026     c                   if        %Found(CNIND00F)
010400151026      *
010500151026      ***  CAP  diversa da Anagrafica
010600151026     c                   if        %subst(CDPPTC:1:5) <> %editc(indCAP:'X')
010700151026     c                   eval      msgerror = %trim(msgerror) +':/N'+
010800151026     c                             'Depot: ' + PDCDPC + 'Cap DPD:' +
010900151026     c                             CDPPTC + ' diverso da anagr.CNIND:' +
011000151026     c                             %editc(indCAP:'X')
011100151026     c                   end
011200151026      *
011300151026      ***  NAZIONE diversa da Anagrafica
011400151026     c                   if        Nazione <> *blank and
011500151026     c                             Nazione <> PDCNAR
011600151026     c                   eval      msgerror = %trim(msgerror) +':/N'+
011700151026     c                             'Depot: ' + PDCDPC + 'Nazione:' +
011800151026     c                             Nazione + ' diversa da anagr.CNIND:' +
011900151027     c                             indSTA
012000151026     c                   end
012100151026     c                   end
012200151026     c                   end
012300151027     c                   end
012400151026     C*
012500151026     c                   end
012600151026     C*
012700151026     C                   READ      DPPDC01L
012800060427     C                   ENDDO
012900151026     C*
013000151026     C*  Prima di chiudere
013100151026     C                   Exsr      In_fine
013200151026     C*
013300151026     c                   return
013400000000     C*------------------------------------------------------------------------*
013500980403     C* *INZSR - OPERAZIONI INIZIALI
013600000000     C*------------------------------------------------------------------------*
013700980403     C     *INZSR        BEGSR
013800000000     C*
013900000000     C* RICEVIMENTO PARAMETRI
014000000000     C     *ENTRY        PLIST
014100000000     C                   PARM                    KPJBA
014200060427     C                   MOVEL     KPJBU         PARAM
014300000000     C*
014400151026     c     chkdepot      klist
014500151026     c                   kfld                    versione
014600151026     c                   kfld                    pdcDPC
014700151026     C*
014800151026     c     Anagr_IND     klist
014900151026     c                   kfld                    indKUT
015000151026     c                   kfld                    indKCC
015100151026     c                   kfld                    indKSC
015200151026     C*
015300151026     c                   z-add     1             indKUT
015400151026     c                   z-add     151           indKCC
015500151026     C*
015600151026     C* TABEL00F - Parziale
015700151026     C     KEYtabel_P    KLIST
015800151026     C                   KFLD                    tblKUT
015900151026     C                   KFLD                    tblCOD
016000151026     C*
016100151026     c                   exsr      CARTAB
016200151026     C*
016300151026     C                   SETON                                        LR
016400151026     c                   CLEAR                   msgerror
016500151026     C*
016600151026     c                   CLEAR                   versione          5 0
016700151026     c                   z-add     *date         DATA_oggi
016800151026     c                   move      *date         DATA_udate
016900151026     c     DATA_udate    subdur    3:*m          DATA_sca
017000151027     c     data_OGGI     setgt     dpCVE02l
017100151027     c                   readp     dpCVE02l
017200151026      ***
017300151026     c                   if        not %EoF(dpCVE02l) and
017400151026     c                             cveDDE <= data_OGGI and
017500151026     c                             data_OGGI <= cveDSC
017600151026     c                   z-add     cvever        versione
017700151026     c                   else
017800151026      ***
017900151026      *- se non l'ha trovata prende l'ultima
018000151026     c     *hival        setll     dpCVE02l
018100151026     c                   readp     dpCVE02l
018200151026     c                   if        not %EoF(dpCVE02l)
018300151026     c                   z-add     cvever        versione
018400151026     c                   end
018500151026     c                   end
018600151026     C*
018700000000     C                   ENDSR
018800151026     C*------------------------------------------------------------------------*
018900151026     C* pima di uscire dal pgm
019000151026     C*------------------------------------------------------------------------*
019100151026     C     In_Fine       BEGSR
019200151026     C*
019300151026     c                   if        MsgError <> *blank
019400151026     c                   exsr      invia_mail
019500151026     c                   end
019600151026     C*
019700151026     C                   ENDSR
019800151026     C*------------------------------------------------------------------------*
019900151026     C* CARTAB - CARICAMENTO DATI TABELLATI
020000151026     C*------------------------------------------------------------------------*
020100151026     C     cartab        BEGSR
020200151026     C*
020300151026     C* Reperisco tutte le nazioni (ISO2) servite da DPD
020400151026     C                   Z-ADD     *zeros        jNaz
020500151026     C                   CLEAR                   skNaz
020600151026     C                   CLEAR                   skNazKey
020700151026     C                   CLEAR                   DS15
020800151026     C                   EVAL      tblKUT = 1
020900151026     C                   EVAL      tblCOD = '15'
021000151026     C     KEYtabel_P    SETLL     TABEL00F
021100151026     C                   IF        %found(TABEL00F)
021200151026     C     KEYtabel_P    READE     TABEL00F
021300151026     C                   DOW       not %eof(TABEL00F)
021400151026     C                   IF        tblFLG = *blanks
021500151026     C                   EVAL      DS15 = tblUNI
021600151026     C                   IF        §15LAD > *zeros OR
021700151026     C                             §15RED = 'S'
021800151026     C                   ADD       1             jNaz
021900151026     C                   MOVEL(P)  §15COD        skNaz(jNaz)
022000151026     C                   MOVEL(P)  tblKEY        skNazKey(jNaz)
022100151026     C                   ENDIF
022200151026     C                   ENDIF
022300151026     C     KEYtabel_P    READE     TABEL00F
022400151026     C                   ENDDO
022500151026     C                   ENDIF
022600151026     C*
022700151026     C                   ENDSR
022800151026      *--------------------------------------------------------------------
022900151026      *   INVIA MAIL
023000151026      *____________________________________________________________________
023100151026     c     INVIA_MAIL    BEGSR
023200151026      *
023300151026     C* Inizializzo variabili
023400151026     C                   eval      wrkEml = 'Andrea.Bertocchi@brt.it'
023500151026     C                   eval      wrkOgg = 'Pulizia file Legami DPD - Anomalie'
023600151026     C                   eval      wrkMsg = MsgError
023700151026      *
023800151026      * Sostituito perchè il TIS701C ha chiodato anche l'indirizzo CED
023900151026      *  da non modificare:
024000151026     C                   call(e)   'TIS701C1'
024100151026     C                   parm                    wrkEml
024200151026     C                   parm                    wrkEmlcc
024300151026     C                   parm                    wrkOgg
024400151026     C                   parm                    wrkMsg
024500151026     C                   parm                    wrkesito
024600151026      *
024700151026      *
024800151026     c                   ENDSR
024900151026      * ?------------------------------------------------------------------ */
