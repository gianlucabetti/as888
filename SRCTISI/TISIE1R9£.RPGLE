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
001300151028     FWFTISIE1R9O  A E             DISK
001400151026     D*------------------------------------------------------------------------*
001500060427     D* DS PARAMETRI DI LANCIO PGM
001600060427     D*-------------------
001700060427     D PARAM           DS                  INZ
001800060427     D  PARVER                        5  0
001900000000     D*-------------------
002000000000     D* ARCHITETTURA
002100000000     D*-------------------
002200000000     D KPJBA         E DS
002300151026     D*----------------
002400151028     D    flgsupdc     s              1
002500151028     D    flgerror     s              1
002600151027     D    wrkEsito     s              1
002700151026     D    wrkEml       s            253
002800151027     D    wrkEmlcc     s            253
002900151026     D    wrkOgg       s             44
003000151027     D    wrkMsg       s           5000
003100151027     D   MsgError      s           5000
003200151026     D*----------------
003300151026     D* DS ESTERNE
003400151026     D*-------------------
003500151026     D DpdcFLO       E DS
003600151026     D DS15          E DS                  INZ
003700151026     D skNaz           S              2    DIM(1000)
003800151026     D skNazKey        S              3    DIM(1000)
003900151026     D jNaz            S              4  0 INZ
004000060427     D*------------------
004100151026     D DATA_eur        S               D   DATFMT(*eur)
004200151026     D DATA_iso        S               D   DATFMT(*iso)
004300151026     D DATA_sca        S               D   DATFMT(*iso)
004400151026     D DATA_udate      S               D   DATFMT(*iso)
004500151026     D DATA_oggi       S              8s 0
004600151028     D ORA_oggi        S              6s 0
004700151028     D count0          S              3s 0 inz(0)
004800151028     D count1          S              3s 0 inz(0)
004900151028     D count2          S              3s 0 inz(0)
005000151028     D count3          S              3s 0 inz(0)
005100151028     D count4          S              3s 0 inz(0)
005200151028     D count5          S              3s 0 inz(0)
005300151029     D count8          S              3s 0 inz(0)
005400151028     D count9          S              3s 0 inz(0)
005500000000     C*------------------------------------------------------------------------*
005600000000     C* MAIN LINE
005700000000     C*------------------------------------------------------------------------*
005800151027     C                   SETON                                        LR
005900060427     C*
006000151026     C* Scorro tutto il file
006100151026     C     *START        SETLL     DPPDC01L
006200151026     C                   READ      DPPDC01L
006300151026     C                   DOW       not %eof(DPPDC01L)
006400151029     c                   movel     pdcflo        dpdcflo
006500151026     C* Pulisce i records scaduti da oltre 3 mesi
006600151026     c     *iso          movel     pdcdsc        DATA_iso
006700151027     c                   if        DATA_iso > DATA_udate
006800151026     C*
006900151026     c                   if        DATA_iso < DATA_sca
007000151026     C                   delete    DPPDC000
007100151029     c                   eval      msgerror =
007200151028     c                             'Depot: ' + PDCDPC + ' (' + pdcNAR + ')'
007300151028     c                             + ' ELIMINATO poiché SCADUTO da oltre -
007400151028     c                             3 mesi'
007500151028     c                   eval      flgsupdc = 'D'
007600151028     c                   eval      flgerror = '0'
007700151028     c                   add       1             count0
007800151028     c                   exsr      wrt_wfile
007900151026     c                   else
008000151026     C*
008100151026     C* se non era già scaduta da oltre 3 mesi
008200151026     C*  controlliamo se deve essere fatta scadere se non più presente nel
008300151026     C*  attuale cappario di DPD.
008400151026     c     chkdepot      chain     dpcdp01l
008500151026     C*
008600151026     c                   if        not %Found(dpcdp01l)
008700151026     c                   z-add     DATA_oggi     pdcdsc
008800151026     C                   update    DPPDC000
008900151027     c                   if        pdcKSC <> 0
009000151029     c                   eval      msgerror =
009100151028     c                             'Depot: ' + PDCDPC + ' (' + pdcNAR + ')'
009200151027     c                             + ' SCADUTO   con   cod.cliente : ' +
009300151110     c                             %editc(pdcKSC:'X') + ' NON su CAPPAR.DPD'
009400151028     c                   eval      flgerror = '1'
009500151028     c                   eval      flgsupdc = 'U'
009600151028     c                   add       1             count1
009700151028     c                   exsr      wrt_wfile
009800151027     c                   end
009900151026     c                   else
010000151026     C*
010100151026     C* La nazione del Cappario
010200151026     c                   clear                   nazione           3
010300151026     C                   IF        CDPISO2 <> *blanks
010400151026     C                   Z-ADD     1             jNaz
010500151026     C     CDPISO2       LOOKUP    skNaz(jNaz)                            55
010600151026     C                   IF        %equal
010700151026     C                   EVAL       nazione = skNazKey(jNaz)
010800151026     c                   end
010900151026     C*
011000151026     C*   se trovato il cappario il DEPOT esiste ed è in vigore
011100151026     c                   if        pdcNAR = ' '
011200151026     C                               and Nazione <> *blank
011300151026     C* La nazione del Cappario
011400151026     C                   EVAL        pdcNAR = nazione
011500151026     C                   update    DPPDC000
011600151029     c                   eval      msgerror =
011700151028     c                             'Depot: ' + PDCDPC
011800151028     c                              + ' (' + pdcNAR + ')' + ' Nazione sui LEGAM-
011900151028     c                             I assente ed impostata dal CAPPARIO DPD'
012000151028     c                   eval      flgsupdc = 'U'
012100151028     c                   eval      flgerror = '9'
012200151028     c                   add       1             count9
012300151028     c                   exsr      wrt_wfile
012400151026     c                   end
012500151026     C*
012600151026     c                   else
012700151026     c                   end
012800151026     C*
012900151026     C*  controllo ed invio MAIL x LNP =0 o CAP e NAZIONE diversi sul Cappario DPD
013000151026     c                   if        §PDCFLOLNP = *blank or
013100151026     c                             §PDCFLOLNP = *zeros
013200151029     c                   eval      msgerror =
013300151027     c                             'Depot: ' + PDCDPC
013400151110     c                              + ' (' + pdcNAR + ')' + ' senza LNP su ' +
013500151110     c                             'PDCFLO (campo LNP) dei LEGAMI'
013600151028     c                   eval      flgsupdc = ' '
013700151028     c                   eval      flgerror = '2'
013800151028     c                   add       1             count2
013900151028     c                   exsr      wrt_wfile
014000151026     c                   end
014100151027      *
014200151029      ***  Giorni a Zero.
014300151029     c                   if        §PDCFLOGGI = *blank or
014400151029     c                             §PDCFLOGGI = '000'
014500151029     c                   eval      msgerror =
014600151029     c                             'Depot: ' + PDCDPC + ' (' + pdcNAR + ')'
014700151110     c                             + ' senza GIORNI di TRAZIONE ' +
014800151110     c                             'PDCFLO (campo GG.) dei LEGAMI'
014900151029     c                   eval      flgsupdc = ' '
015000151029     c                   eval      flgerror = '8'
015100151029     c                   add       1             count8
015200151029     c                   exsr      wrt_wfile
015300151029     c                   end
015400151029     C*
015500151027     C*  SENZA cliente legato
015600151027     c                   if        pdcKSC = 0
015700151029     c                   eval      msgerror =
015800151027     c                             'Depot: ' + PDCDPC
015900151027     c                              + ' (' + pdcNAR + ')'
016000151027     c                              + ' NO Legame Cliente su Anagrafica'
016100151028     c                   eval      flgsupdc = ' '
016200151028     c                   eval      flgerror = '3'
016300151028     c                   add       1             count3
016400151028     c                   exsr      wrt_wfile
016500151027     C*   se c'è il cliente posso verificare uguaglianza fra cap e/o Nazione
016600151027     C*   in anagrafica.
016700151027     c                   elseIF    pdcKSC <> 0
016800151026     C*
016900151026     c                   z-add     pdcksc        indksc
017000151026     c     Anagr_IND     chain     CNIND00F
017100151026     c                   if        %Found(CNIND00F)
017200151026      *
017300151026      ***  CAP  diversa da Anagrafica
017400151026     c                   if        %subst(CDPPTC:1:5) <> %editc(indCAP:'X')
017500151029     c                   eval      msgerror =
017600151028     c                             'Depot: ' + PDCDPC + ' (' + pdcNAR + ')'
017700151027     c                             + '   Cap DPD : ' +
017800151027     c                             CDPPTC + ' <> ' + %editc(indCAP:'X')
017900151027     c                             + ' su anagr.CNIND00F del cliente : '
018000151027     c                             + %editc(pdcKSC:'X')
018100151028     c                   eval      flgsupdc = ' '
018200151028     c                   eval      flgerror = '4'
018300151028     c                   add       1             count4
018400151028     c                   exsr      wrt_wfile
018500151026     c                   end
018600151026      *
018700151026      ***  NAZIONE diversa da Anagrafica
018800151026     c                   if        Nazione <> *blank and
018900151026     c                             Nazione <> PDCNAR
019000151029     c                   eval      msgerror =
019100151027     c                             'Depot: ' + PDCDPC + ' (' + pdcNAR + ')'
019200151027     c                             + ' <> da Naz.: (' + Nazione
019300151027     c                             + ') su cappario DPD'
019400151028     c                   eval      flgsupdc = ' '
019500151028     c                   eval      flgerror = '5'
019600151028     c                   add       1             count5
019700151028     c                   exsr      wrt_wfile
019800151026     c                   end
019900151029      *
020000151029      *
020100151026     c                   end
020200151026     c                   end
020300151027     c                   end
020400151026     C*
020500151026     c                   end
020600151027     c                   end
020700151026     C*
020800151026     C                   READ      DPPDC01L
020900060427     C                   ENDDO
021000151026     C*
021100151026     C*  Prima di chiudere
021200151026     C                   Exsr      In_fine
021300151026     C*
021400151026     c                   return
021500000000     C*------------------------------------------------------------------------*
021600980403     C* *INZSR - OPERAZIONI INIZIALI
021700000000     C*------------------------------------------------------------------------*
021800151027     C     *INZSR        BEGSR
021900000000     C*
022000000000     C* RICEVIMENTO PARAMETRI
022100000000     C     *ENTRY        PLIST
022200000000     C                   PARM                    KPJBA
022300060427     C                   MOVEL     KPJBU         PARAM
022400000000     C*
022500151026     c     chkdepot      klist
022600151026     c                   kfld                    versione
022700151026     c                   kfld                    pdcDPC
022800151026     C*
022900151026     c     Anagr_IND     klist
023000151026     c                   kfld                    indKUT
023100151026     c                   kfld                    indKCC
023200151026     c                   kfld                    indKSC
023300151026     C*
023400151026     c                   z-add     1             indKUT
023500151026     c                   z-add     151           indKCC
023600151026     C*
023700151026     C* TABEL00F - Parziale
023800151026     C     KEYtabel_P    KLIST
023900151026     C                   KFLD                    tblKUT
024000151026     C                   KFLD                    tblCOD
024100151026     C*
024200151026     c                   exsr      CARTAB
024300151026     C*
024400151026     c                   CLEAR                   msgerror
024500151028     c                   clear                   flgerror
024600151026     C*
024700151026     c                   CLEAR                   versione          5 0
024800151110     c                   if        kpjbu <> *blank
024900151103     c                   z-add     parver        versione
025000151110     c                   end
025100151028     C                   TIME                    W0140            14 0
025200151028     c                   movel     W0140         ORA_oggi
025300151026     c                   z-add     *date         DATA_oggi
025400151026     c                   move      *date         DATA_udate
025500151026     c     DATA_udate    subdur    3:*m          DATA_sca
025600151103     c                   if        versione = 0
025700151027     c     data_OGGI     setgt     dpCVE02l
025800151027     c                   readp     dpCVE02l
025900151026      ***
026000151026     c                   if        not %EoF(dpCVE02l) and
026100151026     c                             cveDDE <= data_OGGI and
026200151026     c                             data_OGGI <= cveDSC
026300151026     c                   z-add     cvever        versione
026400151026     c                   else
026500151026      ***
026600151026      *- se non l'ha trovata prende l'ultima
026700151026     c     *hival        setll     dpCVE02l
026800151026     c                   readp     dpCVE02l
026900151026     c                   if        not %EoF(dpCVE02l)
027000151026     c                   z-add     cvever        versione
027100151026     c                   end
027200151026     c                   end
027300151103     c                   endIF
027400151026     C*
027500000000     C                   ENDSR
027600151026     C*------------------------------------------------------------------------*
027700151026     C* pima di uscire dal pgm
027800151026     C*------------------------------------------------------------------------*
027900151026     C     In_Fine       BEGSR
028000151026     C*
028100151028     c                   eval      msgerror =
028200151028     c                             'Depot Obsoleti: scaduti da oltre 3 mesi : '
028300151110     c                              + %editc(count0:'3') +' CANCELLATI' +':/N'+
028400151028     c                             'Depot non presenti sul  cappario  DPD   : '
028500151110     c                              + %editc(count1:'3') +' AGG.DAT_SCA'+':/N'+
028600151029     c                             'Depot  sui  Legami   senza  LNP         : '
028700151028     c                              + %editc(count2:'3')                +':/N'+
028800151029     c                             'Depot senza GIORNI Trazione     gg=(0)  : '
028900151029     c                              + %editc(count8:'3')                +':/N'+
029000151029     c                             'Depot  sui  Legami   senza cod. Cliente : '
029100151028     c                              + %editc(count3:'3')                +':/N'+
029200151028     c                             'Depot con CAP diverso fra anag.e CapDPD : '
029300151028     c                              + %editc(count4:'3')                +':/N'+
029400151028     c                             'Depot con NAZ diversa fra anag.e CapDPD : '
029500151028     c                              + %editc(count5:'3')                +':/N'+
029600151028     c                             'Depot senza NAZ. su legami/presa da DPD : '
029700151110     c                              + %editc(count9:'3')       +':/N' +':/N'+
029800151110     c                             'per il Dettaglio consultare il file ' +
029900151110     c                             'WFTISIE1R9'
030000151026     c                   exsr      invia_mail
030100151026     C*
030200151026     C                   ENDSR
030300151026     C*------------------------------------------------------------------------*
030400151026     C* CARTAB - CARICAMENTO DATI TABELLATI
030500151026     C*------------------------------------------------------------------------*
030600151026     C     cartab        BEGSR
030700151026     C*
030800151026     C* Reperisco tutte le nazioni (ISO2) servite da DPD
030900151026     C                   Z-ADD     *zeros        jNaz
031000151026     C                   CLEAR                   skNaz
031100151026     C                   CLEAR                   skNazKey
031200151026     C                   CLEAR                   DS15
031300151026     C                   EVAL      tblKUT = 1
031400151026     C                   EVAL      tblCOD = '15'
031500151026     C     KEYtabel_P    SETLL     TABEL00F
031600151026     C                   IF        %found(TABEL00F)
031700151026     C     KEYtabel_P    READE     TABEL00F
031800151026     C                   DOW       not %eof(TABEL00F)
031900151026     C                   IF        tblFLG = *blanks
032000151026     C                   EVAL      DS15 = tblUNI
032100151026     C                   IF        §15LAD > *zeros OR
032200151026     C                             §15RED = 'S'
032300151026     C                   ADD       1             jNaz
032400151026     C                   MOVEL(P)  §15COD        skNaz(jNaz)
032500151026     C                   MOVEL(P)  tblKEY        skNazKey(jNaz)
032600151026     C                   ENDIF
032700151026     C                   ENDIF
032800151026     C     KEYtabel_P    READE     TABEL00F
032900151026     C                   ENDDO
033000151026     C                   ENDIF
033100151026     C*
033200151026     C                   ENDSR
033300151026      *--------------------------------------------------------------------
033400151026      *   INVIA MAIL
033500151026      *____________________________________________________________________
033600151026     c     INVIA_MAIL    BEGSR
033700151026      *
033800151026     C* Inizializzo variabili
033900151026     C                   eval      wrkEml = 'Andrea.Bertocchi@brt.it'
034000151103     C                   eval      wrkEmlcc = 'Nicola.Delledonne@brt.it'
034100151027     C                   eval      wrkOgg = 'Anomalie Pulizia file Legami DPD -
034200151027     c                             DPPDC00F'
034300151026     C                   eval      wrkMsg = MsgError
034400151026      *
034500151026      * Sostituito perchè il TIS701C ha chiodato anche l'indirizzo CED
034600151026      *  da non modificare:
034700151027     C                   call(e)   'TIS701C1'
034800151026     C                   parm                    wrkEml
034900151026     C                   parm                    wrkEmlcc
035000151026     C                   parm                    wrkOgg
035100151026     C                   parm                    wrkMsg
035200151026     C                   parm                    wrkesito
035300151026      *
035400151026     c                   ENDSR
035500151026      * ?------------------------------------------------------------------ */
035600151028     c     wrt_wfile     begsr
035700151027      *
035800151028     c                   eval      E1R9ORA  =  ORA_oggi
035900151028     c                   eval      E1R9DAT  =  DATA_oggi
036000151028     c                   eval      E1R9TANO =  flgerror
036100151028     c                   eval      E1R9EXEC =  flgsupdc
036200151028     c                   eval      E1R9VCAP =  versione
036300151028     c                   eval      E1R9DESC =  msgerror
036400151028     c*
036500151028     c                   write     wftisie00
036600151027      *
036700151027     c                   ENDSR
036800151027      * ?------------------------------------------------------------------ */
