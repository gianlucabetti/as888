000100000000     H*------------------------------------------------------------------------*
000200171109     H* PULIZIA FILE LEGAMI DPLEG10F  DEPOT DPD
000300000000     H*------------------------------------------------------------------------*
000400151026     H DECEDIT('0,') DATEDIT(*YMD.)
000500060427     F*------------------------------------------------------------------------*
000600060427     F* DATA BASE
000700060427     F*------------------------------------------------------------------------*
000800171109     FDPCVE12i  IF   E           K DISK
000900171116     FDPCVE11i  IF   E           K DISK    rename(DPCVE100:DPCVE11)
001000171116     f                                     Prefix(cv_)
001100171116     FDPCDP11i  IF   E           K DISK
001200151026     FTABEL00F  IF   E           K DISK
001300151026     FCNIND00F  IF   E           K DISK
001400171109     FDPleg11L  UF A E           K DISK
001500171109      *
001600171109     FWFTISID0R9O  A E             DISK
001700151026     D*------------------------------------------------------------------------*
001800171109     D    flgsuleg     s              1
001900151028     D    flgerror     s              1
002000151027     D    wrkEsito     s              1
002100151026     D    wrkEml       s            253
002200151027     D    wrkEmlcc     s            253
002300151026     D    wrkOgg       s             44
002400151027     D    wrkMsg       s           5000
002500151027     D   MsgError      s           5000
002600151026     D*----------------
002700151026     D* DS ESTERNE
002800151026     D*-------------------
002900171109     D DlegFLO       E DS
003000151026     D DS15          E DS                  INZ
003100151026     D skNaz           S              2    DIM(1000)
003200151026     D skNazKey        S              3    DIM(1000)
003300151026     D jNaz            S              4  0 INZ
003400171116     D vers            S                   like(versione)
003500060427     D*------------------
003600151026     D DATA_eur        S               D   DATFMT(*eur)
003700151026     D DATA_iso        S               D   DATFMT(*iso)
003800151026     D DATA_sca        S               D   DATFMT(*iso)
003900151026     D DATA_udate      S               D   DATFMT(*iso)
004000151026     D DATA_oggi       S              8s 0
004100151028     D ORA_oggi        S              6s 0
004200151028     D count0          S              3s 0 inz(0)
004300151028     D count1          S              3s 0 inz(0)
004400151028     D count2          S              3s 0 inz(0)
004500151028     D count3          S              3s 0 inz(0)
004600151028     D count4          S              3s 0 inz(0)
004700151028     D count5          S              3s 0 inz(0)
004800151029     D count8          S              3s 0 inz(0)
004900151028     D count9          S              3s 0 inz(0)
005000000000     C*------------------------------------------------------------------------*
005100000000     C* MAIN LINE
005200000000     C*------------------------------------------------------------------------*
005300151027     C                   SETON                                        LR
005400060427     C*
005500151026     C* Scorro tutto il file
005600171109     C     *START        SETLL     DPleg11L
005700171109     C                   READ      DPleg11L
005800171109     C                   DOW       not %eof(DPleg11L)
005900171109     c                   movel     legflo        dlegflo
006000151026     C* Pulisce i records scaduti da oltre 3 mesi
006100171109     c     *iso          movel     legdsc        DATA_iso
006200151027     c                   if        DATA_iso > DATA_udate
006300151026     C*
006400151026     c                   if        DATA_iso < DATA_sca
006500171109     C                   delete    DPleg000
006600151029     c                   eval      msgerror =
006700171109     c                             'Depot: ' + legDEP7+ ' (' + legNAR + ')'
006800151028     c                             + ' ELIMINATO poiché SCADUTO da oltre -
006900151028     c                             3 mesi'
007000171109     c                   eval      flgsuleg = 'D'
007100151028     c                   eval      flgerror = '0'
007200151028     c                   add       1             count0
007300151028     c                   exsr      wrt_wfile
007400151026     c                   else
007500151026     C*
007600151026     C* se non era già scaduta da oltre 3 mesi
007700151026     C*  controlliamo se deve essere fatta scadere se non più presente nel
007800151026     C*  attuale cappario di DPD.
007900171116     c                   z-add     versione      vers
008000171109     c     chkdepot      chain     dpcdp11i
008100151026     C*
008200171109     c                   if        not %Found(dpcdp11i)
008300171116     C*
008400171116     C* Con la Versione precedente prende la data scadenza
008500171116     c                   eval      vers = vers -1
008600171116     c     vers          chain     dpcve11i
008700171116     c                   if         %Found(dpcve11i)
008800171116     c                   z-add     cv_cveDSC     legdsc
008900171116     c                   else
009000171109     c                   z-add     DATA_oggi     legdsc
009100171116     c                   end
009200171116     C*
009300171109     C                   update    DPleg000
009400171109     c                   if        legKSC <> 0
009500151029     c                   eval      msgerror =
009600171109     c                             'Depot: ' + legDEP7+ ' (' + legNAR + ')'
009700151027     c                             + ' SCADUTO   con   cod.cliente : ' +
009800171109     c                             %editc(legKSC:'X') + ' NON su CAPPAR.DPD'
009900151028     c                   eval      flgerror = '1'
010000171109     c                   eval      flgsuleg = 'U'
010100151028     c                   add       1             count1
010200151028     c                   exsr      wrt_wfile
010300151027     c                   end
010400151026     c                   else
010500151026     C*
010600151026     C* La nazione del Cappario
010700151026     c                   clear                   nazione           3
010800151026     C                   IF        CDPISO2 <> *blanks
010900151026     C                   Z-ADD     1             jNaz
011000151026     C     CDPISO2       LOOKUP    skNaz(jNaz)                            55
011100151026     C                   IF        %equal
011200151026     C                   EVAL       nazione = skNazKey(jNaz)
011300151026     c                   end
011400151026     C*
011500151026     C*   se trovato il cappario il DEPOT esiste ed è in vigore
011600171109     c                   if        legNAR = ' '
011700151026     C                               and Nazione <> *blank
011800151026     C* La nazione del Cappario
011900171109     C                   EVAL        legNAR = nazione
012000171109     C                   update    DPleg000
012100151029     c                   eval      msgerror =
012200171109     c                             'Depot: ' + legDEP7
012300171109     c                              + ' (' + legNAR + ')' + ' Nazione sui LEGAM-
012400151028     c                             I assente ed impostata dal CAPPARIO DPD'
012500171109     c                   eval      flgsuleg = 'U'
012600151028     c                   eval      flgerror = '9'
012700151028     c                   add       1             count9
012800151028     c                   exsr      wrt_wfile
012900151026     c                   end
013000151026     C*
013100151026     c                   else
013200151026     c                   end
013300151026     C*
013400151026     C*  controllo ed invio MAIL x LNP =0 o CAP e NAZIONE diversi sul Cappario DPD
013500171109     c                   if        §legFLOLNP = *blank or
013600171109     c                             §legFLOLNP = *zeros
013700151029     c                   eval      msgerror =
013800171109     c                             'Depot: ' + legDEP7
013900171109     c                              + ' (' + legNAR + ')' + ' senza LNP su ' +
014000171109     c                             'LEGFLO (campo LNP) dei LEGAMI'
014100171109     c                   eval      flgsuleg = ' '
014200151028     c                   eval      flgerror = '2'
014300151028     c                   add       1             count2
014400151028     c                   exsr      wrt_wfile
014500151026     c                   end
014600151027      *
014700151029      ***  Giorni a Zero.
014800171109     c                   if        §legFLOGGI = *blank or
014900171109     c                             §legFLOGGI = '000'
015000151029     c                   eval      msgerror =
015100171109     c                             'Depot: ' + legDEP7+ ' (' + legNAR + ')'
015200151110     c                             + ' senza GIORNI di TRAZIONE ' +
015300171109     c                             'LEGFLO (campo GG.) dei LEGAMI'
015400171109     c                   eval      flgsuleg = ' '
015500151029     c                   eval      flgerror = '8'
015600151029     c                   add       1             count8
015700151029     c                   exsr      wrt_wfile
015800151029     c                   end
015900151029     C*
016000151027     C*  SENZA cliente legato
016100171109     c                   if        legKSC = 0
016200151029     c                   eval      msgerror =
016300171109     c                             'Depot: ' + legDEP7
016400171109     c                              + ' (' + legNAR + ')'
016500151027     c                              + ' NO Legame Cliente su Anagrafica'
016600171109     c                   eval      flgsuleg = ' '
016700151028     c                   eval      flgerror = '3'
016800151028     c                   add       1             count3
016900151028     c                   exsr      wrt_wfile
017000151027     C*   se c'è il cliente posso verificare uguaglianza fra cap e/o Nazione
017100151027     C*   in anagrafica.
017200171109     c                   elseIF    legKSC <> 0
017300151026     C*
017400171109     c                   z-add     legksc        indksc
017500151026     c     Anagr_IND     chain     CNIND00F
017600151026     c                   if        %Found(CNIND00F)
017700151026      *
017800151026      ***  CAP  diversa da Anagrafica
017900151026     c                   if        %subst(CDPPTC:1:5) <> %editc(indCAP:'X')
018000151029     c                   eval      msgerror =
018100171109     c                             'Depot: ' + legDEP7+ ' (' + legNAR + ')'
018200151027     c                             + '   Cap DPD : ' +
018300151027     c                             CDPPTC + ' <> ' + %editc(indCAP:'X')
018400151027     c                             + ' su anagr.CNIND00F del cliente : '
018500171109     c                             + %editc(legKSC:'X')
018600171109     c                   eval      flgsuleg = ' '
018700151028     c                   eval      flgerror = '4'
018800151028     c                   add       1             count4
018900151028     c                   exsr      wrt_wfile
019000151026     c                   end
019100151026      *
019200151026      ***  NAZIONE diversa da Anagrafica
019300151026     c                   if        Nazione <> *blank and
019400171109     c                             Nazione <> legNAR
019500151029     c                   eval      msgerror =
019600171109     c                             'Depot: ' + legDEP7+ ' (' + legNAR + ')'
019700151027     c                             + ' <> da Naz.: (' + Nazione
019800151027     c                             + ') su cappario DPD'
019900171109     c                   eval      flgsuleg = ' '
020000151028     c                   eval      flgerror = '5'
020100151028     c                   add       1             count5
020200151028     c                   exsr      wrt_wfile
020300151026     c                   end
020400151029      *
020500151029      *
020600151026     c                   end
020700151026     c                   end
020800151027     c                   end
020900151026     C*
021000151026     c                   end
021100151027     c                   end
021200151026     C*
021300171109     C                   READ      DPLEG11L
021400060427     C                   ENDDO
021500151026     C*
021600151026     C*  Prima di chiudere
021700151026     C                   Exsr      In_fine
021800151026     C*
021900151026     c                   return
022000000000     C*------------------------------------------------------------------------*
022100980403     C* *INZSR - OPERAZIONI INIZIALI
022200000000     C*------------------------------------------------------------------------*
022300151027     C     *INZSR        BEGSR
022400000000     C*
022500000000     C* RICEVIMENTO PARAMETRI
022600000000     C     *ENTRY        PLIST
022700171113     C                   PARM                    PARAM             5
022800000000     C*
022900151026     c     chkdepot      klist
023000171109     c                   kfld                    flgatb            1
023100171116     c                   kfld                    vers
023200171109     c                   kfld                    legDEP7
023300171109     c                   clear                   flgatb            1
023400151026     C*
023500151026     c     Anagr_IND     klist
023600151026     c                   kfld                    indKUT
023700151026     c                   kfld                    indKCC
023800151026     c                   kfld                    indKSC
023900151026     C*
024000151026     c                   z-add     1             indKUT
024100151026     c                   z-add     151           indKCC
024200151026     C*
024300151026     C* TABEL00F - Parziale
024400151026     C     KEYtabel_P    KLIST
024500151026     C                   KFLD                    tblKUT
024600151026     C                   KFLD                    tblCOD
024700151026     C*
024800151026     c                   exsr      CARTAB
024900151026     C*
025000151026     c                   CLEAR                   msgerror
025100151028     c                   clear                   flgerror
025200151026     C*
025300151026     c                   CLEAR                   versione          5 0
025400171109     c                   if        param <> *blank
025500171113     c                   move      param         versione
025600151110     c                   end
025700151028     C                   TIME                    W0140            14 0
025800151028     c                   movel     W0140         ORA_oggi
025900151026     c                   z-add     *date         DATA_oggi
026000151026     c                   move      *date         DATA_udate
026100151026     c     DATA_udate    subdur    3:*m          DATA_sca
026200151103     c                   if        versione = 0
026300171109     c     data_OGGI     setgt     dpCVE12i
026400171109     c                   readp     dpCVE12i
026500151026      ***
026600171109     c                   if        not %EoF(dpCVE12i) and
026700151026     c                             cveDDE <= data_OGGI and
026800151026     c                             data_OGGI <= cveDSC
026900151026     c                   z-add     cvever        versione
027000151026     c                   else
027100151026      ***
027200151026      *- se non l'ha trovata prende l'ultima
027300171109     c     *hival        setll     dpCVE12i
027400171109     c                   readp     dpCVE12i
027500171109     c                   if        not %EoF(dpCVE12i)
027600151026     c                   z-add     cvever        versione
027700151026     c                   end
027800151026     c                   end
027900151103     c                   endIF
028000151026     C*
028100000000     C                   ENDSR
028200151026     C*------------------------------------------------------------------------*
028300151026     C* pima di uscire dal pgm
028400151026     C*------------------------------------------------------------------------*
028500151026     C     In_Fine       BEGSR
028600151026     C*
028700151028     c                   eval      msgerror =
028800151028     c                             'Depot Obsoleti: scaduti da oltre 3 mesi : '
028900151110     c                              + %editc(count0:'3') +' CANCELLATI' +':/N'+
029000151028     c                             'Depot non presenti sul  cappario  DPD   : '
029100151110     c                              + %editc(count1:'3') +' AGG.DAT_SCA'+':/N'+
029200151029     c                             'Depot  sui  Legami   senza  LNP         : '
029300151028     c                              + %editc(count2:'3')                +':/N'+
029400151029     c                             'Depot senza GIORNI Trazione     gg=(0)  : '
029500151029     c                              + %editc(count8:'3')                +':/N'+
029600151029     c                             'Depot  sui  Legami   senza cod. Cliente : '
029700151028     c                              + %editc(count3:'3')                +':/N'+
029800151028     c                             'Depot con CAP diverso fra anag.e CapDPD : '
029900151028     c                              + %editc(count4:'3')                +':/N'+
030000151028     c                             'Depot con NAZ diversa fra anag.e CapDPD : '
030100151028     c                              + %editc(count5:'3')                +':/N'+
030200151028     c                             'Depot senza NAZ. su legami/presa da DPD : '
030300151110     c                              + %editc(count9:'3')       +':/N' +':/N'+
030400151110     c                             'per il Dettaglio consultare il file ' +
030500171109     c                             'WFTISID0R9'
030600151026     c                   exsr      invia_mail
030700151026     C*
030800151026     C                   ENDSR
030900151026     C*------------------------------------------------------------------------*
031000151026     C* CARTAB - CARICAMENTO DATI TABELLATI
031100151026     C*------------------------------------------------------------------------*
031200151026     C     cartab        BEGSR
031300151026     C*
031400151026     C* Reperisco tutte le nazioni (ISO2) servite da DPD
031500151026     C                   Z-ADD     *zeros        jNaz
031600151026     C                   CLEAR                   skNaz
031700151026     C                   CLEAR                   skNazKey
031800151026     C                   CLEAR                   DS15
031900151026     C                   EVAL      tblKUT = 1
032000151026     C                   EVAL      tblCOD = '15'
032100151026     C     KEYtabel_P    SETLL     TABEL00F
032200151026     C                   IF        %found(TABEL00F)
032300151026     C     KEYtabel_P    READE     TABEL00F
032400151026     C                   DOW       not %eof(TABEL00F)
032500151026     C                   IF        tblFLG = *blanks
032600151026     C                   EVAL      DS15 = tblUNI
032700151026     C                   IF        §15LAD > *zeros OR
032800151026     C                             §15RED = 'S'
032900151026     C                   ADD       1             jNaz
033000151026     C                   MOVEL(P)  §15COD        skNaz(jNaz)
033100151026     C                   MOVEL(P)  tblKEY        skNazKey(jNaz)
033200151026     C                   ENDIF
033300151026     C                   ENDIF
033400151026     C     KEYtabel_P    READE     TABEL00F
033500151026     C                   ENDDO
033600151026     C                   ENDIF
033700151026     C*
033800151026     C                   ENDSR
033900151026      *--------------------------------------------------------------------
034000151026      *   INVIA MAIL
034100151026      *____________________________________________________________________
034200151026     c     INVIA_MAIL    BEGSR
034300151026      *
034400151026     C* Inizializzo variabili
034500151026     C                   eval      wrkEml = 'Andrea.Bertocchi@brt.it'
034600151103     C                   eval      wrkEmlcc = 'Nicola.Delledonne@brt.it'
034700151027     C                   eval      wrkOgg = 'Anomalie Pulizia file Legami DPD -
034800171109     c                             DPLEG10F'
034900151026     C                   eval      wrkMsg = MsgError
035000151026      *
035100151026      * Sostituito perchè il TIS701C ha chiodato anche l'indirizzo CED
035200151026      *  da non modificare:
035300151027     C                   call(e)   'TIS701C1'
035400151026     C                   parm                    wrkEml
035500151026     C                   parm                    wrkEmlcc
035600151026     C                   parm                    wrkOgg
035700151026     C                   parm                    wrkMsg
035800151026     C                   parm                    wrkesito
035900151026      *
036000151026     c                   ENDSR
036100151026      * ?------------------------------------------------------------------ */
036200151028     c     wrt_wfile     begsr
036300151027      *
036400171109     c                   eval      D0R9ORA  =  ORA_oggi
036500171109     c                   eval      D0R9DAT  =  DATA_oggi
036600171109     c                   eval      D0R9TANO =  flgerror
036700171109     c                   eval      D0R9EXEC =  flgsuleg
036800171109     c                   eval      D0R9VCAP =  versione
036900171109     c                   eval      D0R9DESC =  msgerror
037000151028     c*
037100171109     c                   write     wftisid00
037200151027      *
037300151027     c                   ENDSR
037400151027      * ?------------------------------------------------------------------ */
