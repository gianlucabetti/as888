000100110930      /TITLE Upload via Internet: traduzione in EDIVABWR/EDIVATWR.
000200121030      *
000300121030      * PARTICOLARITA':
000400121030      * RMA composto da un dato in colonna + RMN.
000500121030      * Per i dati RSD, IND, CAD, LOD, PRD ci sono 2 colonne: se valorizzata la seconda è primaria.
000600121030      * IAS e QFT hanno lo stesso valore.
000700121030      * Barcode da sviluppare rispetto al nr.colli.
000800121030      * Se la colonna  totpal  è valorizzata (ergo maggiore di 0) occorre:
000900121030      * · Valorizzare i bytes 1 e 2 del campo VABANT con  01?
001000121030      * · Valorizzare i bytes 3 e 4 del campo VABANT con il valore numerico indicato nella
001100121030      *   colonna  totpal
001200121030      * · Valorizzare il campo VABGVA =  B?
001300121217     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP(*caller)
001400120209     F*
001500990910     Ftivin00r  uF   E             DISK    usropn
001600110930     FEDIVABwr  O    E             DISK    usropn prefix(ok_)
001700110930     FEDIVATwr  O    E             DISK    usropn
001800000313     D*
001900040113     D*------------
002000040113     D* SCHIEDE A PROGRAMMA
002100040113     D*------------
002200080429     D intcol          S            100    DIM(5) CTDATA PERRCD(1)
002300010330     D*----------------------------------------------------
002400010330     D* DICHIARAZIOINE VARIABILI DI WRK
002500010330     D*----------------------------------------------------
002600010330     D dscmz         e ds                  inz
002700010330     D psds           sds
002800010330     D  procname         *PROC
002900010330     D tivlrds       e ds                  extname(tivlr00f)
003000040128     D tisi95ds      e ds
003100110930     D edivabs       e ds                  extname(edivab0f)
003200110930     D edivabs_sav   e ds                  extname(edivab0f) prefix(sav_)
003300110930     D edivabs_ok    e ds                  extname(edivab0f) prefix(ok_)
003400010330     D esito           s              1
003500010330     D prmlit          s             10
003600010330     D prmfir          s             10
003700010330     D wrkesito        s                   like(esito)
003800010330     D rrnum           s              6  0 INZ(*zeros)
003900040510     D parccm          s              8    INZ(*blanks)
004000040119     D parmbr          s             10    INZ(*blanks)
004100040119     D paropz          s              1    INZ(*blanks)
004200040119     D chkcall         s              1    INZ(*blanks)
004300080513     D depspe          s                   LIKE(VABNSP) INZ(*zeros)
004400080418     D wFlgCAS         s              1    INZ(*blanks)
004500121030     D* variabile Intestazioni = *on se nel .csv la prima riga contiene le intestazioni di colonna
004600121030     D* variabile Intestazioni = *off se nel .csv ci sono solo dati
004700121030     D Intestazioni    s               n   inz(*on)
004800120203     D wNomeFile       s             30    INZ(*blanks)
004900100326     D*------------------
005000100326     D* DS REPERIMENTO NUMERATORE
005100100326     D*------------------
005200100326     D trul33ds      e ds                  inz
005300100326     D*------------------
005400100326     D* DS ARCHITETTURA
005500100326     D*------------------
005600100326     D kpjba         e ds                  inz
005700100326
005800030715     D*------------------
005900030715     D* VARIABILI X LO SPLIT DELLA STRINGA CSV IN CAMPI
006000030715     D*------------------
006100030715     D SkSplitFLD      S             10    DIM(100)
006200030715     D SkSplitCSV      S            256    DIM(100)
006300030715     D CharCSV         S              1
006400030715     D CharTXT         S              1
006500030715     D CharNUM         S              1
006600100318     D CharSOS         S              1
006700030715     D posDa           S              3  0 INZ(*zeros)
006800030715     D posA            S              3  0 INZ(*zeros)
006900030715     D i               s              3  0 INZ(1)
007000030715     D wGiro           s              1  0 INZ(*zeros)
007100070823     D wVATNOT_A       s                   LIKE(VATNOT)
007200060331     D wVATNOT_B       s                   LIKE(VATNOT)
007300060331     D wVATNOT_E       s                   LIKE(VATNOT)
007400110927     D wVATNOT_I       s                   LIKE(VATNOT)
007500120202     D wVATNOT_J       s                   LIKE(VATNOT)
007600120202     D wVATNOT_IJ      s             70
007700140904     D wVATNOT_S       s                   LIKE(VATNOT)
007800121030     D wVABRMA         s                   LIKE(VABRMA)
007900121030     D wVABRMN         s                   LIKE(VABRMN)
008000121030     D wVABRSD01       s                   LIKE(VABRSD)
008100121030     D wVABIND01       s                   LIKE(VABIND)
008200121030     D wVABCAD01       s                   LIKE(VABCAD)
008300121030     D wVABLOD01       s                   LIKE(VABLOD)
008400121030     D wVABPRD01       s                   LIKE(VABPRD)
008500121030     D wVABRSD02       s                   LIKE(VABRSD)
008600121030     D wVABIND02       s                   LIKE(VABIND)
008700121030     D wVABCAD02       s                   LIKE(VABCAD)
008800121030     D wVABLOD02       s                   LIKE(VABLOD)
008900121030     D wVABPRD02       s                   LIKE(VABPRD)
009000121030     D wVABIAS         s                   LIKE(VABIAS)
009100030822     D*------------------
009200030822     D* VARIABILI X LO SPLIT DEI VALORI DI DEFAULT PROVENIENTI DAI PARAMETRI DEL TRADUTTORE
009300030822     D*------------------
009400030822     D posDaDft        S              3  0 INZ(*zeros)
009500030822     D posADft         S              3  0 INZ(*zeros)
009600030822     D j               s              3  0 INZ(1)
009700030822     D wGiroDft        s              1  0 INZ(*zeros)
009800080418     D*------------------
009900080418     D* Costanti
010000080418     D*------------------
010100080418     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
010200080418     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
010300100318
010400100318     D*------------------
010500100318     D* LINKING A DEFINIZIONI ESTERNE
010600100318     D*------------------
010700100318     D/COPY GAITRASRC/SRCPROTOPR,UBISNUM
010800100318     D/COPY GAITRASRC/SRCPROTOPI,UBISNUM
010900100318
011000100909     D*------------------
011100100909     D* DS CALCOLO CHECK-DIGITS
011200100909     D*------------------
011300100909     D TRUL28DSX     e ds                  inz
011400010330
011500010330
011600010330
011700000913     C                   reset                   rrnum
011800990921     C                   reset                   esito
011900990921     C                   reset                   wrkesito
012000010601     C*
012100010601     C                   exsr      opeini
012200010605     C                   exsr      rwvab
012300010601     C*
012400010601     C                   seton                                        lr
012500010601
012600010601
012700010601
012800010601     C*--------------------------------------------------------
012900010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
013000010601     C*--------------------------------------------------------
013100010601     C     PREELA        BEGSR
013200010601     C*
013300010601     C* SE OCCORRE SPEDIRE IN FILIALE
013400010601     C                   if        invfil <> *zeros and
013500010601     C                             flgGiro = '0'
013600010601     C*
013700010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
013800010601     C                   eval      flgGiro = '1'
013900010601     C*
014000010601     C                   endif
014100010601     C*
014200010601     C                   ENDSR
014300010601     C***
014400010601
014500010601
014600010601
014700010601
014800010601     C*--------------------------------------------------------
014900010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
015000010601     C*--------------------------------------------------------
015100010601     C     ENDELA        BEGSR
015200040119     C*
015300040119     C* Effettuo la chiamata al CLLE preposto
015400110930     C                   call(e)   'TITVEVTC'
015500040510     C                   parm                    parccm
015600040119     C                   parm                    parmbr
015700040119     C                   parm      '2'           paropz
015800050201     C*
015900050201     C* Effettuo lancio TISI95 solo x chiusura
016000050201     C                   CLEAR                   TISI95DS
016100050201     C                   EVAL      I95TLA = 'C'
016200050201     C                   CALL      'TISI95R'
016300050201     C                   PARM                    TISI95DS
016400000616     C*
016500010601     C                   ENDSR
016600010601
016700010601
016800010330     C*--------------------------------------------------------
016900110930     C* RWVAB   LEGGE TIVIN00R E SCRIVE EDIVABWF              *
017000010330     C*--------------------------------------------------------
017100010605     C     RWVAB         BEGSR
017200010330     C*
017300010330     C                   if        not %open(tivin00r)
017400010330     C                   open      tivin00r
017500010330     C                   endif
017600110930     C                   if        not %open(edivabwr)
017700110930     C                   open      edivabwr
017800010330     C                   endif
017900040119     C*
018000110930     C* Eseguo operazioni di aggiunta nuovo membro in EDIVATWR
018100040119     C                   exsr      prevat
018200040119     C*
018300040119     C                   if        chkcall = '0'
018400040119     C*
018500110930     C                   if        not %open(edivatwr)
018600110930     C                   open      edivatwr
018700040119     C                   endif
018800010330     C*
018900040119     C                   clear                   §CTROKVB          7 0
019000040119     C                   clear                   §CTROKVT          7 0
019100040119     C                   clear                   §CTRMO            7 0
019200040119     C                   clear                   §CTRNO            7 0
019300121030     C*
019400121030     C* variabile Intestazioni = *on se nel .csv la prima riga contiene le intestazioni di colonna
019500121030     C* variabile Intestazioni = *off se nel .csv ci sono solo dati
019600121030     C                   IF        Intestazioni = *off
019700121030     C* Eseguo primo giro x by-pasare la mancanza della 1° riga con le intestazioni
019800121030     C* colonne nel file del cliente (che contiene quindi solo le righe d dati).
019900121030     C                   exsr      inzvar
020000121030     C                   exsr      impvab
020100121030     C                   ENDIF
020200010330     C*
020300030822     C                   do        *HIVAL
020400010330     C*
020500030822     C                   read      tivin00r                               70
020600010618     C*
020700010618     C* Dopo ogni lettura verifico se ci sono stati record OK
020800010618     C                   if        vinflg = '1'
020900010618     C                   eval      flgOk = '1'
021000010618     C                   endif
021100040510     C*
021200040510     C* Verifico che il record nn contenga unicamente i caratteri d separatore campi
021300040510     C                   z-add     *zeros        wLenVINDTA        4 0
021400040510     C                   z-add     *zeros        wFlgVINDTA        1 0
021500040510     C                   dow       wLenVINDTA < %len(%trim(vindta))
021600040510     C                   eval      wLenVINDTA = wLenVINDTA + 1
021700040510     C                   if        %subst(%trim(vindta):wLenVINDTA:1)<>CharCSV
021800040510     C                   z-add     1             wFlgVINDTA
021900040510     C                   leave
022000040510     C                   endif
022100040510     C                   enddo
022200010618     C*
022300040510     C                   if        vindta > *blanks AND
022400040510     C                             wFlgVINDTA = 1
022500010330     C                   add       1             rrnum
022600010330     C*
022700010601     C                   if        *in70 = *off and
022800010330     C                             (vinflg = *blanks
022900010330     C                              or vinflg = '0'
023000010330     C                              or vinflg = '2')
023100010330     C*
023200010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
023300010711     C                   if        vinflg = *blanks or vinflg = '0'
023400010711     C                   clear                   vinmsg
023500010711     C                   endif
023600010601     C*
023700070925     C* Aggiungo un byte a blanks all'inizio d ogni record
023800070925     C                   eval      vindta  = ' ' + %trim(vindta)
023900070925     C*
024000080527     C                   exsr      inzvar
024100100318     C                   exsr      normCSV
024200010605     C                   exsr      impvab
024300010601     C*
024400010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
024500010605     C                   eval      depfil = VABLNP
024600010601     C                   exsr      repfil
024700010601     C                   if        depfil = invfil
024800021025     C                   if        vlrpoi = 999
024900030822     C                   move(P)   invfil        VABFGS
025000021025     C                   else
025100030822     C                   move(P)   vlrpoi        VABFGS
025200021025     C                   endif
025300010601     C*
025400010601     C                   exsr      PREELA
025500010601     C*
025600010604     C* Ebbene...
025700010604     C*
025800040119     C  N31              add       1             §CTROKVB
025900040119     C   32              add       1             §CTRMO
026000040119     C   31              add       1             §CTRNO
026100080418     C*
026200030822     C                   if        wGiro = 2
026300080513     C* Scrivo sempre il VAT
026400080513     C  N31              exsr      wrivat
026500110201     C                   exsr      chkvab
026600080513     C                   endif
026700010604     C*
026800010604     C                   if        *in31 = *off and
026900010604     C                             *in32 = *off
027000010604     C                   eval      vinflg = '1'
027100010604     C                   else
027200010604     C                   eval      vinflg = '2'
027300010604     C                   endif
027400010604     C                   endif
027500010601     C*
027600010604     C                   endif
027700010604     C*
027800010330     C                   else
027900010330     C                   eval      vinflg = '1'
028000010330     C                   endif
028100010601     C*
028200010601     C  N70              update    tivin000
028300010330     C*
028400030822     C  N70              enddo
028500040119     C*
028600080527     C* Verifico dati rimasti "in cannna"...
028700080527     C                   seton                                        65
028800110201     C                   exsr      chkvab
028900080527     C*
029000040119     C                   endif                                                  (endif - chkcall)
029100010601     C*
029200010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
029300010601     C                   if        cntNonEl = *zeros or
029400010601     C                             flgMulti = '0'
029500010330     C* Se non ci sono record con errori ...
029600010601     C                   if        §ctrno = 0 and
029700010604     C                             §ctrmo = 0 and
029800010601     C                             flgStato <> '2'
029900010330     C* ... restituisco esito OK.
030000010330     C                   eval      wrkesito = '0'
030100010330     C                   else
030200040119     C                   if        §ctrokvb > 0
030300010330     C                   eval      wrkesito = '1'
030400010330     C                   else
030500010615     C                   if        flgOk = '0'
030600010615     C                   eval      wrkesito = '2'
030700010615     C                   else
030800010615     C                   eval      wrkesito = '6'
030900010615     C                   endif
031000010330     C                   endif
031100010330     C                   endif
031200010601     C                   else
031300010601     C                   eval      wrkesito = '9'
031400010601     C                   endif
031500010330     C*
031600010330     C                   if        %open(tivin00r)
031700010330     C                   close     tivin00r
031800010330     C                   endif
031900110930     C                   if        %open(edivabwr)
032000110930     C                   close     edivabwr
032100010330     C                   endif
032200110930     C                   if        %open(edivatwr)
032300110930     C                   close     edivatwr
032400040512     C                   endif
032500010601     C*
032600010601     C                   if        vlrpoi <> 999
032700010601     C                   eval      invfil = vlrpoi
032800010601     C                   endif
032900010330     C*
033000040119     C                   if        §ctrokvb > 0
033100010601     C                             and invfil > *zeros
033200010330     C                   exsr      invio
033300010330     C                   endif
033400010601     C*
033500010618     C                   if        flgGiro = '1'
033600010601     C                   exsr      endela
033700010618     C                   endif
033800010330     C*
033900010330     C                   ENDSR
034000010330     C***
034100080527
034200080527
034300080527
034400080527     C*----------------------------------------------------*
034500080527     C*  CONSIDERAZIONI SU SCRITTURA BUFFER RECORDS VAB
034600080527     C*----------------------------------------------------*
034700100413     C     CHKVAB        BEGSR
034800110201     C*
034900110201     C* Se richiesti ignorare colli / peso / volume
035000110315 xxx C***                if        *in35
035100120203 xxx C***                z-add     *zeros        VABNCL              F0461
035200120203 xxx C***                z-add     *zF046190081  VABPKB
035300110315 xxx C***                z-add     *zeros        VABVLB
035400110315 xxx C***                endif
035500080527     C*
035600080527     C* Scrivo il VAB solo a rottura d spedizione
035700080527     C                   if        depspe = *zeros
035800110930     C  N31              eval      edivabs_sav = edivabs
035900080527     C                   eval      depspe = vabNSP
036000080527     C                   else
036100080527     C                   if        depspe = vabNSP AND *in65 = *off
036200100413     C   51              add       VABNCL        sav_VABNCL
036300100413     C   51              add       VABPKB        sav_VABPKB
036400100413     C   51              add       VABVLB        sav_VABVLB
036500100720     C   52              z-add     VABNCL        sav_VABNCL
036600100720     C   52              z-add     VABPKB        sav_VABPKB
036700100720     C   52              z-add     VABVLB        sav_VABVLB
036800100720     C   53              add       VABNCL        sav_VABNCL
036900100720     C   53              z-add     VABPKB        sav_VABPKB
037000100720     C   53              z-add     VABVLB        sav_VABVLB
037100080527     C                   else
037200120203     C  N31              eval      edivabs_ok = edivabs_sav
037300080527     C  N31              exsr      wrivab
037400110930     C  N31              eval      edivabs_sav = edivabs
037500080527     C                   eval      depspe = vabNSP
037600080527     C                   endif
037700080527     C                   endif
037800080527     C*
037900080527     C                   ENDSR
038000040119
038100040119
038200040119
038300040119     C*----------------------------------------------------*
038400040119     C*  SCARICAMENTO BUFFER RECORDS VAB
038500040119     C*----------------------------------------------------*
038600040510     C     WRIVAB        BEGSR
038700080513     C*
038800120203     C* EDIVABWR ha il prefisso "ok_"
038900120203     C*
039000120203     C* Gestisco l'eventuale rottura x numero spedizione
039100080527     C                   if        VABNSP <> depspe or *in65 = *on
039200070301     C                   if        wVATNOT_E <> *blanks
039300120203     C                   eval      ok_VABCTM = '7Q'
039400070301     C                   endif
039500110930     C*
039600110930     C* VALORIZZO CAMPI RELATIVI AL "CMR"
039700120203     C                   MOVE(P)   vlrMSG        wNomeFile
039800120203     C                   EVAL      ok_VABCMR = wNomeFile
039900120203     C                   EVAL      ok_VABDCM = DATCOR
040000120203     C                   EVAL      ok_VABDTS = DATCOR
040100120203     C                   EVAL      ok_VABHMS = ORACOR
040200120203     C                   EVAL      ok_VABCNT = 1
040300110930     C*
040400110930     C                   write     edivab00
040500080513     C                   endif
040600040119     C*
040700040119     C                   ENDSR
040800040119
040900040119
041000040119
041100040119     C*----------------------------------------------------*
041200040119     C*  SCARICAMENTO BUFFER RECORDS VAT
041300040119     C*----------------------------------------------------*
041400040510     C     WRIVAT        BEGSR
041500060512     C*
041600060512     C* Inizializzo i campi chiave
041700060512     C                   eval      VATFGS = VABFGS
041800060512     C                   eval      VATAAS = VABAAS
041900060512     C                   eval      VATCCM = VABCCM
042000070823     C                   eval      VATNRS = VABNRS
042100060512     C                   eval      VATNSP = VABNSP
042200060512     C                   eval      VATLNP = VABLNP
042300040119     C*
042400040510     C* Se tipo record nn espressamente indicato assumo x default (e x diffusione d uso) = 'E'
042500040510     C                   if        VATTRC = *blanks
042600040510     C                   eval      VATTRC = 'E'
042700040510     C                   endif
042800110318     C                   if        VATNOT <> *blanks
042900120203     C                   MOVE(P)   vlrMSG        wNomeFile
043000120203     C                   EVAL      VATCMR = wNomeFile
043100120203     C***                eval      VATCMR = %char(datcor)+%char(oracor)
043200110930     C                   eval      VATCNT = 1
043300110930     C                   write     EDIVAT00
043400040119     C                   add       1             §CTROKVT
043500110318     C                   endif
043600060331     C*
043700060331     C* Gestisco estensioni VAT "specificatamente esplicitate"
043800060331     C                   if        wVATNOT_A <> *blanks
043900060331     C                   eval      VATNOT = wVATNOT_A
044000060331     C                   eval      VATTRC = 'A'
044100120203     C                   MOVE(P)   vlrMSG        wNomeFile
044200120203     C                   EVAL      VATCMR = wNomeFile
044300120203     C***                eval      VATCMR = %char(datcor)+%char(oracor)
044400110930     C                   eval      VATCNT = 1
044500110930     C                   write     EDIVAT00
044600060331     C                   add       1             §CTROKVT
044700060331     C                   endif
044800060331     C*
044900060331     C                   if        wVATNOT_B <> *blanks
045000060331     C                   eval      VATNOT = wVATNOT_B
045100060331     C                   eval      VATTRC = 'B'
045200120203     C                   MOVE(P)   vlrMSG        wNomeFile
045300120203     C                   EVAL      VATCMR = wNomeFile
045400120203     C***                eval      VATCMR = %char(datcor)+%char(oracor)
045500110930     C                   eval      VATCNT = 1
045600110930     C                   write     EDIVAT00
045700060331     C                   add       1             §CTROKVT
045800060331     C                   endif
045900060331     C*
046000060331     C                   if        wVATNOT_E <> *blanks
046100110201     C                   eval      VATNOT = wVATNOT_E
046200060331     C                   eval      VATTRC = 'E'
046300121030     C                   MOVE(P)   vlrMSG        wNomeFile
046400121030     C                   EVAL      VATCMR = wNomeFile
046500121030     C                   eval      VATCNT = 1
046600121030     C*
046700121030     C* Sviluppati i "CHI SONO" in relazione al numero colli
046800121030     C*
046900121030     C     1             DO        VABNCL        wNumCollo         3 0
047000121030     C*** da analisi del POC il progressivo colli è max 3 char e deve ccontiene gli zeri non
047100121030     C*** significativi
047200121030     C                   EVAL      VATNOT = %trimr(wVATNOT_E) +
047300121030     C                                %editc(wNumCollo:'X')
047400121030     C                   write     EDIVAT00
047500121030     C                   add       1             §CTROKVT
047600121030     C                   ENDDO
047700121030     C*
047800121030     C                   endif
047900110927     C*
048000110927     C                   if        wVATNOT_I <> *blanks
048100110927     C                   eval      VATNOT = wVATNOT_I
048200110927     C                   eval      VATTRC = 'I'
048300120203     C                   MOVE(P)   vlrMSG        wNomeFile
048400120203     C                   EVAL      VATCMR = wNomeFile
048500120203     C***                eval      VATCMR = %char(datcor)+%char(oracor)
048600110930     C                   eval      VATCNT = 1
048700110930     C                   write     EDIVAT00
048800110927     C                   add       1             §CTROKVT
048900110927     C                   endif
049000120202     C*
049100120202     C                   if        wVATNOT_J <> *blanks
049200120202     C                   eval      VATNOT = wVATNOT_J
049300120202     C                   eval      VATTRC = 'J'
049400120203     C                   MOVE(P)   vlrMSG        wNomeFile
049500120203     C                   EVAL      VATCMR = wNomeFile
049600120203     C***                eval      VATCMR = %char(datcor)+%char(oracor)
049700120202     C                   eval      VATCNT = 1
049800120202     C                   write     EDIVAT00
049900120202     C                   add       1             §CTROKVT
050000120202     C                   endif
050100040119     C*
050200140904     C                   if        wVATNOT_S <> *blanks
050300140904     C                   eval      VATNOT = wVATNOT_S
050400140904     C* i 2 flag devono essere blank
050500140904     C                   EVAL      %subst(VATNOT:17:1)=*blank
050600140904     C                   EVAL      %subst(VATNOT:18:1)=*blank
050700140904     C                   eval      VATTRC = 'S'
050800140904     C                   MOVE(P)   vlrMSG        wNomeFile
050900140904     C                   EVAL      VATCMR = wNomeFile
051000140904     C***                eval      VATCMR = %char(datcor)+%char(oracor)
051100140904     C                   eval      VATCNT = 1
051200140904     C                   write     EDIVAT00
051300140904     C                   add       1             §CTROKVT
051400140904     C                   endif
051500140904     C*
051600040119     C                   ENDSR
051700040119
051800040119
051900040119
052000040119     C*----------------------------------------------------*
052100110930     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE EDIVATWR
052200040119     C*----------------------------------------------------*
052300040119     C     PREVAT        BEGSR
052400040119     C*
052500110930     C* Compongo il nome del membro da dare al EDIVATWR
052600040119     C                   eval      parmbr = vlrhdl
052700040119     C                   movel     'M'           parmbr
052800040510     C                   eval      parccm = vlrksc
052900040119     C                   eval      paropz = '1'
053000040119     C* Effettuo la chiamata al CLLE preposto
053100110930     C                   call(e)   'TITVEVTC'
053200040119     C                   parm                    parccm
053300040119     C                   parm                    parmbr
053400040119     C                   parm                    paropz
053500040119     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
053600040119     C                   if        %error
053700040119     C                   movel     '1'           chkcall
053800040119     C                   else
053900040119     C                   movel     '0'           chkcall
054000040119     C                   endif
054100040119     C*
054200040119     C                   ENDSR
054300010601
054400010601
054500010601
054600010330     C*----------------------------------------------------*
054700030715     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
054800010330     C*----------------------------------------------------*
054900040119     C     INZVAR        BEGSR
055000080527     C*
055100080527     C                   setoff                                       65
055200010330     C*
055300030822     C* Inizializzo variabili di wrk
055400020204     C                   Z-ADD     *zeros        Num5_0            5 0
055500101018     C                   MOVEL     *blanks       wFlgCAS
055600060331     C                   MOVEL     *blanks       wVATNOT_A
055700060331     C                   MOVEL     *blanks       wVATNOT_B
055800060331     C                   MOVEL     *blanks       wVATNOT_E
055900110927     C                   MOVEL     *blanks       wVATNOT_I
056000120202     C                   MOVEL     *blanks       wVATNOT_J
056100120202     C                   MOVEL     *blanks       wVATNOT_IJ
056200140904     C                   MOVEL     *blanks       wVATNOT_S
056300121030     C                   MOVEL     *blanks       wVABRSD01
056400121030     C                   MOVEL     *blanks       wVABIND01
056500121030     C                   MOVEL     *blanks       wVABCAD01
056600121030     C                   MOVEL     *blanks       wVABLOD01
056700121030     C                   MOVEL     *blanks       wVABPRD01
056800121030     C                   MOVEL     *blanks       wVABRSD02
056900121030     C                   MOVEL     *blanks       wVABIND02
057000121030     C                   MOVEL     *blanks       wVABCAD02
057100121030     C                   MOVEL     *blanks       wVABLOD02
057200121030     C                   MOVEL     *blanks       wVABPRD02
057300030822     C*
057400030822     C* Inizializzo il buffer del record da scrivere e la schiera d wrk x i dati
057500030822     C                   CLEAR                   SkSplitCSV
057600030822     C                   EVAL      i = 1
057700030822     C                   EVAL      posDa = *zeros
057800030822     C                   EVAL      posA  = *zeros
057900080527     C*
058000110930     C                   CLEAR                   EDIVABS
058100110930     C                   CLEAR                   EDIVAT00
058200030822     C*
058300030822     C* Reimposto i valori di default
058400030822     C                   EXSR      DEFCAM
058500030822     C*
058600010330     C                   ENDSR
058700010330     C*----------------------------------------------------*
058800030822     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
058900010330     C*----------------------------------------------------*
059000010330     C     DEFCAM        BEGSR
059100030822     C*
059200030715     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
059300030715     C* e delimitatore testo.
059400030715     C                   EVAL      CharCSV = %subst(vlrppt:2:1)
059500030715     C                   EVAL      CharTXT = %subst(vlrppt:3:1)
059600030715     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
059700100318     C*
059800100318     C* Determino il carattere sostituente il separatore decimale in caso d conflitto
059900100318     C                   EVAL      CharSOS = CharNUM
060000030822     C*
060100030715     C* Reperisco il flag che mi indica se effettuare o meno la stampa in filiale
060200100720     C                   SETOFF                                       505152
060300100720     C                   SETOFF                                       53
060400100413     C                   SELECT
060500100413     C                   WHEN      %subst(vlrppt:1:1) = 'S'
060600100720     C                   SETON                                        50
060700100413     C                   WHEN      %subst(vlrppt:1:1) = 'A'
060800100413     C                   SETON                                        51
060900100720     C                   WHEN      %subst(vlrppt:1:1) = *blanks
061000100720     C                   SETON                                        52
061100100720     C                   WHEN      %subst(vlrppt:1:1) = 'C'
061200100720     C                   SETON                                        53
061300100413     C                   ENDSL
061400030822     C*
061500030822     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
061600030822     C                   EVAL      posDaDft = 1
061700030822     C                   EVAL      posADft  = 0
061800030822     C                   EVAL      wGiroDft = 0
061900030822     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
062000030822     C                             posDaDft > 0
062100030822     C*
062200030822     C* Gestisco il 1° giro
062300030822     C                   IF        wGiroDft = 0
062400030822     C* Eseguo lo scan x trovare l'inizio del campo corrente
062500030822     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
062600030822     C* Incremento il contatore dei "giri"
062700030822     C                   EVAL      wGiroDft = 1
062800030822     C                   ELSE
062900030822     C                   EVAL      posDaDft = posADft
063000030822     C                   ENDIF
063100030822     C* Eseguo lo scan x trovare la fine del campo corrente
063200030822     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
063300030822     C*
063400030822     C* A questo "estraggo" il parametro (campo e valore) corrente...
063500030822     C* ...solo se entrambe le posizini (DA/A) sono > 0
063600030822     C                   IF        posDaDft > 0 AND
063700030822     C                             posADft  > 0
063800030822     C* NCL
063900030822     C                   IF        %subst(
064000030822     C                             %subst(vlrppt:posDaDft+1:
064100030822     C                             posADft-posDaDft-1):1:3)
064200030822     C                             = 'NCL'
064300030822     C                   EVAL      PiStr=%trim(%subst(
064400030822     C                             %subst(vlrppt:posDaDft+1:
064500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
064600030822     C                   EXSR      CHKNUM
064700030822     C                   IF        PiInt=*on
064800030822     C                   Z-ADD     PiVal         VABNCL
064900030822     C                   ENDIF
065000030822     C                   ENDIF
065100030822     C* CCM
065200030822     C                   IF        %subst(
065300030822     C                             %subst(vlrppt:posDaDft+1:
065400030822     C                             posADft-posDaDft-1):1:3)
065500030822     C                             = 'CCM'
065600030822     C                   EVAL      PiStr=%trim(%subst(
065700030822     C                             %subst(vlrppt:posDaDft+1:
065800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
065900030822     C                   EXSR      CHKNUM
066000030822     C                   IF        PiInt=*on
066100030822     C                   Z-ADD     PiVal         VABCCM
066200030822     C                   ENDIF
066300030822     C                   ENDIF
066400030822     C* LNP
066500030822     C                   IF        %subst(
066600030822     C                             %subst(vlrppt:posDaDft+1:
066700030822     C                             posADft-posDaDft-1):1:3)
066800030822     C                             = 'LNP'
066900030822     C                   EVAL      PiStr=%trim(%subst(
067000030822     C                             %subst(vlrppt:posDaDft+1:
067100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
067200030822     C                   EXSR      CHKNUM
067300030822     C                   IF        PiInt=*on
067400030822     C                   Z-ADD     PiVal         VABLNP
067500030822     C                   ENDIF
067600030822     C                   ENDIF
067700030822     C* NRS
067800030822     C                   IF        %subst(
067900030822     C                             %subst(vlrppt:posDaDft+1:
068000030822     C                             posADft-posDaDft-1):1:3)
068100030822     C                             = 'NRS'
068200030822     C                   EVAL      PiStr=%trim(%subst(
068300030822     C                             %subst(vlrppt:posDaDft+1:
068400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
068500030822     C                   EXSR      CHKNUM
068600030822     C                   IF        PiInt=*on
068700030822     C                   Z-ADD     PiVal         VABNRS
068800030822     C                   ENDIF
068900030822     C                   ENDIF
069000030822     C* CTR
069100030822     C                   IF        %subst(
069200030822     C                             %subst(vlrppt:posDaDft+1:
069300030822     C                             posADft-posDaDft-1):1:3)
069400030822     C                             = 'CTR'
069500030822     C                   EVAL      PiStr=%trim(%subst(
069600030822     C                             %subst(vlrppt:posDaDft+1:
069700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
069800030822     C                   EXSR      CHKNUM
069900030822     C                   IF        PiInt=*on
070000030822     C                   Z-ADD     PiVal         VABCTR
070100030822     C                   ENDIF
070200030822     C                   ENDIF
070300030822     C* PKB
070400030822     C                   IF        %subst(
070500030822     C                             %subst(vlrppt:posDaDft+1:
070600030822     C                             posADft-posDaDft-1):1:3)
070700030822     C                             = 'PKB'
070800030822     C                   EVAL      PiStr=%trim(%subst(
070900030822     C                             %subst(vlrppt:posDaDft+1:
071000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
071100030822     C                   EXSR      CHKNUM
071200030822     C                   IF        PiNum=*on
071300030822     C                   Z-ADD     PiVal         VABPKB
071400030822     C                   ENDIF
071500030822     C                   ENDIF
071600030822     C* VLB
071700030822     C                   IF        %subst(
071800030822     C                             %subst(vlrppt:posDaDft+1:
071900030822     C                             posADft-posDaDft-1):1:3)
072000030822     C                             = 'VLB'
072100030822     C                   EVAL      PiStr=%trim(%subst(
072200030822     C                             %subst(vlrppt:posDaDft+1:
072300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
072400030822     C                   EXSR      CHKNUM
072500030822     C                   IF        PiNum=*on
072600030822     C                   Z-ADD     PiVal         VABVLB
072700030822     C                   ENDIF
072800030822     C                   ENDIF
072900030822     C* QFT
073000030822     C                   IF        %subst(
073100030822     C                             %subst(vlrppt:posDaDft+1:
073200030822     C                             posADft-posDaDft-1):1:3)
073300030822     C                             = 'QFT'
073400030822     C                   EVAL      PiStr=%trim(%subst(
073500030822     C                             %subst(vlrppt:posDaDft+1:
073600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
073700030822     C                   EXSR      CHKNUM
073800030822     C                   IF        PiNum=*on
073900030822     C                   Z-ADD     PiVal         VABQFT
074000030822     C                   ENDIF
074100030822     C                   ENDIF
074200030822     C* CBO
074300030822     C                   IF        %subst(
074400030822     C                             %subst(vlrppt:posDaDft+1:
074500030822     C                             posADft-posDaDft-1):1:3)
074600030822     C                             = 'CBO'
074700030822     C                   EVAL      VABCBO=%trim(%subst(
074800030822     C                             %subst(vlrppt:posDaDft+1:
074900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
075000030822     C                   ENDIF
075100030822     C* TSP
075200030822     C                   IF        %subst(
075300030822     C                             %subst(vlrppt:posDaDft+1:
075400030822     C                             posADft-posDaDft-1):1:3)
075500030822     C                             = 'TSP'
075600030822     C                   EVAL      VABTSP=%trim(%subst(
075700030822     C                             %subst(vlrppt:posDaDft+1:
075800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
075900030822     C                   ENDIF
076000030822     C* VAS
076100030822     C                   IF        %subst(
076200030822     C                             %subst(vlrppt:posDaDft+1:
076300030822     C                             posADft-posDaDft-1):1:3)
076400030822     C                             = 'VAS'
076500030822     C                   EVAL      VABVAS=%trim(%subst(
076600030822     C                             %subst(vlrppt:posDaDft+1:
076700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
076800030822     C                   ENDIF
076900030822     C* VCA
077000030822     C                   IF        %subst(
077100030822     C                             %subst(vlrppt:posDaDft+1:
077200030822     C                             posADft-posDaDft-1):1:3)
077300030822     C                             = 'VCA'
077400030822     C                   EVAL      VABVCA=%trim(%subst(
077500030822     C                             %subst(vlrppt:posDaDft+1:
077600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
077700030822     C                   ENDIF
077800070925     C* TIC
077900070925     C                   IF        %subst(
078000070925     C                             %subst(vlrppt:posDaDft+1:
078100070925     C                             posADft-posDaDft-1):1:3)
078200070925     C                             = 'TIC'
078300070925     C                   EVAL      VABTIC=%trim(%subst(
078400070925     C                             %subst(vlrppt:posDaDft+1:
078500070925     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
078600070925     C                   ENDIF
078700030822     C* GCA
078800030822     C                   IF        %subst(
078900030822     C                             %subst(vlrppt:posDaDft+1:
079000030822     C                             posADft-posDaDft-1):1:3)
079100030822     C                             = 'GCA'
079200030822     C                   EVAL      VABGCA=%trim(%subst(
079300030822     C                             %subst(vlrppt:posDaDft+1:
079400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
079500030822     C                   ENDIF
079600030822     C* CTM
079700030822     C                   IF        %subst(
079800030822     C                             %subst(vlrppt:posDaDft+1:
079900030822     C                             posADft-posDaDft-1):1:3)
080000030822     C                             = 'CTM'
080100030822     C                   EVAL      VABCTM=%trim(%subst(
080200030822     C                             %subst(vlrppt:posDaDft+1:
080300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
080400030822     C                   ENDIF
080500030822     C* FFD
080600030822     C                   IF        %subst(
080700030822     C                             %subst(vlrppt:posDaDft+1:
080800030822     C                             posADft-posDaDft-1):1:3)
080900030822     C                             = 'FFD'
081000030822     C                   EVAL      VABFFD=%trim(%subst(
081100030822     C                             %subst(vlrppt:posDaDft+1:
081200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
081300030822     C                   ENDIF
081400030822     C* VAD
081500030822     C                   IF        %subst(
081600030822     C                             %subst(vlrppt:posDaDft+1:
081700030822     C                             posADft-posDaDft-1):1:3)
081800030822     C                             = 'VAD'
081900030822     C                   EVAL      VABVAD=%trim(%subst(
082000030822     C                             %subst(vlrppt:posDaDft+1:
082100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
082200030822     C                   ENDIF
082300030822     C* GMA
082400030822     C                   IF        %subst(
082500030822     C                             %subst(vlrppt:posDaDft+1:
082600030822     C                             posADft-posDaDft-1):1:3)
082700030822     C                             = 'GMA'
082800030822     C                   EVAL      VABGMA=%trim(%subst(
082900030822     C                             %subst(vlrppt:posDaDft+1:
083000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
083100030822     C                   ENDIF
083200030822     C* GGA
083300030822     C                   IF        %subst(
083400030822     C                             %subst(vlrppt:posDaDft+1:
083500030822     C                             posADft-posDaDft-1):1:3)
083600030822     C                             = 'GGA'
083700030822     C                   EVAL      VABGGA=%trim(%subst(
083800030822     C                             %subst(vlrppt:posDaDft+1:
083900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
084000030822     C                   ENDIF
084100030822     C* GVA
084200030822     C                   IF        %subst(
084300030822     C                             %subst(vlrppt:posDaDft+1:
084400030822     C                             posADft-posDaDft-1):1:3)
084500030822     C                             = 'GVA'
084600030822     C                   EVAL      VABGVA=%trim(%subst(
084700030822     C                             %subst(vlrppt:posDaDft+1:
084800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
084900030822     C                   ENDIF
085000030822     C* TC1
085100030822     C                   IF        %subst(
085200030822     C                             %subst(vlrppt:posDaDft+1:
085300030822     C                             posADft-posDaDft-1):1:3)
085400030822     C                             = 'TC1'
085500030822     C                   EVAL      VABTC1=%trim(%subst(
085600030822     C                             %subst(vlrppt:posDaDft+1:
085700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
085800030822     C                   ENDIF
085900030822     C* TC2
086000030822     C                   IF        %subst(
086100030822     C                             %subst(vlrppt:posDaDft+1:
086200030822     C                             posADft-posDaDft-1):1:3)
086300030822     C                             = 'TC2'
086400030822     C                   EVAL      VABTC2=%trim(%subst(
086500030822     C                             %subst(vlrppt:posDaDft+1:
086600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
086700030822     C                   ENDIF
086800040714     C* VATTRC
086900040714     C                   IF        %subst(
087000040714     C                             %subst(vlrppt:posDaDft+1:
087100040714     C                             posADft-posDaDft-1):1:3)
087200040714     C                             = 'TRC'
087300040714     C                   EVAL      VATTRC=%trim(%subst(
087400040714     C                             %subst(vlrppt:posDaDft+1:
087500040714     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
087600040714     C                   ENDIF
087700030822     C* ...
087800030822     C                   ENDIF
087900030822     C                   ENDDO
088000020204     C*
088100010330     C                   ENDSR
088200010607     C*----------------------------------------------------*
088300110930     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X EDIVAB)
088400010607     C*----------------------------------------------------*
088500010607     C     IMPVAB        BEGSR
088600010607     C*
088700010607     C                   SETOFF                                       3132
088800110315 xxx C***                SETON                                        35
088900030515     C*
089000010607     C                   MOVEL     datcor        VABAAS
089100010607     C                   MOVE      datcor        VABMGS
089200010607     C*
089300030715     C**********
089400030715     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
089500030715     C**********
089600030715     C                   IF        wGiro = *zeros
089700040113     C*
089800040510     C* Se presente inserisco qui la forzatura della intesatazione colonne personalizzata
089900040510     C                   IF        %trim(intcol(1))+%trim(intcol(2))+
090000080429     C                             %trim(intcol(3))+%trim(intcol(4))+
090100080429     C                             %trim(intcol(5))<>*blanks
090200040113     C                   EVAL      vindta = %trim(intcol(1))+%trim(intcol(2))+
090300080429     C                                      %trim(intcol(3))+%trim(intcol(4))+
090400080429     C                                      %trim(intcol(5))
090500070823     C     ';':CharCSV   XLATE     vindta        vindta
090600040510     C                   ENDIF
090700031201     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
090800031201     C     minu:maiu     XLATE     vindta        vindta
090900030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
091000030715     C                   DOW       posDa <= %len(%trim(vindta))
091100030715     C*
091200030715     C* Gestisco il 1° campo
091300030715     C                   IF        i = 1
091400030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
091500030715     C                   EVAL      posDa = 1
091600030715     C* Eseguo lo scan x trovare la fine del primo campo
091700030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
091800030715     C* A questo punto "estraggo" il campo corrente
091900030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
092000030715     C                                                    (posA-posDa))
092100030715     C* X i campi successivi al 1°
092200030715     C                   ELSE
092300030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
092400030715     C                   EVAL      posDa = posA + 1
092500030715     C* Eseguo lo scan x trovare la fine del campo corrente
092600030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
092700030715     C* Gestisco l'ultimo campo
092800030715     C                   IF        posA = *zeros
092900030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
093000030715     C* A questo punto "estraggo" il campo corrente
093100030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
093200030715     C                                                    (posA-posDa))
093300030715     C                   LEAVE
093400030715     C                   ELSE
093500030715     C* A questo punto "estraggo" il campo corrente
093600030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
093700030715     C                                                    (posA-posDa))
093800030715     C                   ENDIF
093900030715     C                   ENDIF
094000030715     C* Incremento il contatore d campo
094100030715     C                   EVAL      i = i +1
094200030715     C                   ENDDO
094300030715     C*
094400030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
094500030715     C* delimitazione testo
094600030715     C                   EVAL      i = 1
094700030715     C                   DOW       i < %elem(SkSplitFLD)
094800030715     C     CharTXT:' '   XLATE     SkSplitFLD(i) SkSplitFLD(i)
094900030715     C                   EVAL      SkSplitFLD(i) = %trim(SkSplitFLD(i))
095000030715     C                   EVAL      i = i + 1
095100030715     C                   ENDDO
095200030715     C                   EVAL      wGiro = 1
095300030715     C                   ELSE
095400030715     C                   EVAL      wGiro = 2
095500031204     C**********
095600031204     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
095700031204     C* d separatore campo
095800031204     C**********
095900031204     C                   EVAL      vindta = %trim(vindta)
096000031204     C                   DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
096100031204     C                   EVAL      vindta = %subst(vindta:2)
096200031204     C                   ENDDO
096300031204     C*
096400031204     C                   Z-ADD     *zeros        lunghInput        4 0
096500031204     C                   EVAL      lunghInput = %len(%trim(vindta))
096600031204     C                   DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
096700031204     C                   EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
096800031204     C                   EVAL      lunghInput = %len(%trim(vindta))
096900110912     C                   ENDDO
097000030715     C**********
097100030715     C* Effettuo lo split del campo dati d input
097200030715     C**********
097300030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
097400030715     C                   DOW       posDa <= %len(%trim(vindta))
097500030715     C*
097600030715     C* Gestisco il 1° campo
097700030715     C                   IF        i = 1
097800030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
097900030715     C                   EVAL      posDa = 1
098000030715     C* Eseguo lo scan x trovare la fine del primo campo
098100030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
098200030715     C* A questo punto "estraggo" il campo corrente
098300030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
098400030715     C                                                    (posA-posDa))
098500030715     C* X i campi successivi al 1°
098600030715     C                   ELSE
098700030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
098800030715     C                   EVAL      posDa = posA + 1
098900031204     C* Verifico che nn vi sia il campo nn valorizzato
099000031204     C                   IF        %subst(vindta:posDa:1) = CharCSV
099100031204     C* Se campo nn valorizzato skippo al prossimo
099200031204     C                   EVAL      SkSplitCSV(i) = *blanks
099300031204     C                   EVAL      posA  = posA + 1
099400031204     C                   ELSE
099500030715     C* Eseguo lo scan x trovare la fine del campo corrente
099600030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
099700030715     C* Gestisco l'ultimo campo
099800030715     C                   IF        posA = *zeros
099900030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
100000030715     C* A questo punto "estraggo" il campo corrente
100100030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
100200030715     C                                                    (posA-posDa))
100300030715     C                   LEAVE
100400030715     C                   ELSE
100500030715     C* A questo punto "estraggo" il campo corrente
100600030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
100700030715     C                                                    (posA-posDa))
100800030715     C                   ENDIF
100900031204     C                   ENDIF
101000030715     C                   ENDIF
101100030715     C* Incremento il contatore d campo
101200030715     C                   EVAL      i = i +1
101300030715     C                   ENDDO
101400030715     C*
101500030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
101600030715     C* delimitazione testo
101700030715     C                   EVAL      i = 1
101800030715     C                   DOW       i < %elem(SkSplitCSV)
101900030715     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
102000030715     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
102100030715     C                   EVAL      i = i + 1
102200030715     C                   ENDDO
102300100720     C*
102400100720     C* Stacco un progressivo x numero spedizione
102500110930 xxx C                   EXSR      repNSP
102600100326     C*
102700030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
102800030715     C                   EVAL      i = 1
102900030715     C                   DOW       i < %elem(SkSplitFLD)
103000030715     C*
103100030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
103200030715     C                   IF        SkSplitFLD(i) <> *blanks AND
103300030715     C                             SkSplitCSV(i) <> *blanks
103400030721     C*** ATB
103500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABATB'
103600030721     C                   EVAL      VABATB = SkSplitCSV(i)
103700030721     C                   ENDIF
103800030715     C*** RSD
103900030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRSD'
104000030715     C                   EVAL      VABRSD = SkSplitCSV(i)
104100030715     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
104200030715     C     '@':'A'       XLATE     VABRSD        VABRSD
104300030715     C* ==
104400030715     C                   ENDIF
104500030721     C*** RD2
104600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRD2'
104700030721     C                   EVAL      VABRD2 = SkSplitCSV(i)
104800030721     C                   ENDIF
104900030715     C*** IND
105000030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABIND'
105100030715     C                   EVAL      VABIND = SkSplitCSV(i)
105200030715     C                   ENDIF
105300030715     C*** LOD
105400030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABLOD'
105500030715     C                   EVAL      VABLOD = SkSplitCSV(i)
105600030715     C                   ENDIF
105700030715     C*** PRD
105800030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABPRD'
105900030715     C                   EVAL      VABPRD = SkSplitCSV(i)
106000030715     C                   ENDIF
106100030721     C*** NZD
106200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNZD'
106300030721     C                   EVAL      VABNZD = SkSplitCSV(i)
106400030721     C                   ENDIF
106500030715     C*** RMA
106600030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMA'
106700030715     C                   EVAL      VABRMA = SkSplitCSV(i)
106800030715     C                   ENDIF
106900030715     C*** NOT
107000030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNOT'
107100030715     C                   EVAL      VABNOT = SkSplitCSV(i)
107200030715     C                   ENDIF
107300030715     C*** NT2
107400030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNT2'
107500030715     C                   EVAL      VABNT2 = SkSplitCSV(i)
107600030715     C                   ENDIF
107700030715     C*** VCA
107800030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVCA'
107900030715     C                   EVAL      VABVCA = SkSplitCSV(i)
108000030715     C                   ENDIF
108100030715     C*** VAS
108200030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAS'
108300030715     C                   EVAL      VABVAS = SkSplitCSV(i)
108400030715     C                   ENDIF
108500030715     C*** FFD
108600030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABFFD'
108700030715     C                   EVAL      VABFFD = SkSplitCSV(i)
108800030715     C                   ENDIF
108900030721     C*** GC1
109000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC1'
109100030721     C                   EVAL      VABGC1 = SkSplitCSV(i)
109200030721     C                   ENDIF
109300030721     C*** GC2
109400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC2'
109500030721     C                   EVAL      VABGC2 = SkSplitCSV(i)
109600030721     C                   ENDIF
109700030721     C*** TSP
109800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTSP'
109900030721     C                   EVAL      VABTSP = SkSplitCSV(i)
110000030721     C                   ENDIF
110100040510     C*** CBO
110200040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABCBO'
110300040510     C                   EVAL      VABCBO = SkSplitCSV(i)
110400040510     C                   ENDIF
110500030721     C*** NAS
110600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNAS'
110700030721     C                   EVAL      VABNAS = SkSplitCSV(i)
110800030721     C                   ENDIF
110900030721     C*** TIC
111000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTIC'
111100030721     C                   EVAL      VABTIC = SkSplitCSV(i)
111200030721     C                   ENDIF
111300030721     C*** GCA
111400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGCA'
111500030721     C                   EVAL      VABGCA = SkSplitCSV(i)
111600030721     C                   ENDIF
111700030721     C*** XCO
111800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABXCO'
111900030721     C                   EVAL      VABXCO = SkSplitCSV(i)
112000030721     C                   ENDIF
112100030721     C*** CTM
112200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTM'
112300030721     C                   EVAL      VABCTM = SkSplitCSV(i)
112400030721     C                   ENDIF
112500030721     C*** TCR
112600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTCR'
112700030721     C                   EVAL      VABTCR = SkSplitCSV(i)
112800030721     C                   ENDIF
112900030721     C*** CTS
113000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTS'
113100030721     C                   EVAL      VABCTS = SkSplitCSV(i)
113200030721     C                   ENDIF
113300030721     C*** FTM
113400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABFTM'
113500030721     C                   EVAL      VABFTM = SkSplitCSV(i)
113600030721     C                   ENDIF
113700030721     C*** VAD
113800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAD'
113900030721     C                   EVAL      VABVAD = SkSplitCSV(i)
114000030721     C                   ENDIF
114100030721     C*** GMA
114200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGMA'
114300030721     C                   EVAL      VABGMA = SkSplitCSV(i)
114400030721     C                   ENDIF
114500030721     C*** GGA
114600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGGA'
114700030721     C                   EVAL      VABGGA = SkSplitCSV(i)
114800030721     C                   ENDIF
114900030721     C*** GVA
115000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGVA'
115100030721     C                   EVAL      VABGVA = SkSplitCSV(i)
115200030721     C                   ENDIF
115300030721     C*** TC1
115400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC1'
115500030721     C                   EVAL      VABTC1 = SkSplitCSV(i)
115600030721     C                   ENDIF
115700030721     C*** TC2
115800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC2'
115900030721     C                   EVAL      VABTC2 = SkSplitCSV(i)
116000030721     C                   ENDIF
116100030721     C*** SCL
116200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABSCL'
116300030721     C                   EVAL      VABSCL = SkSplitCSV(i)
116400030721     C                   ENDIF
116500030721     C*** RMO
116600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMO'
116700030721     C                   EVAL      VABRMO = SkSplitCSV(i)
116800030721     C                   ENDIF
116900030721     C*** NMO
117000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNMO'
117100030721     C                   EVAL      VABNMO = SkSplitCSV(i)
117200030721     C                   ENDIF
117300030721     C*
117400030721     C* Reperisco quindi i campi numerici...
117500040510     C*** NSP
117600040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABNSP'
117700040510     C                   EVAL      PiStr=SkSplitCSV(i)
117800040510     C                   EXSR      CHKNUM
117900040510     C                   IF        PiInt=*on
118000040510     C                   Z-ADD     PiVal         VABNSP
118100040510     C                   ELSE
118200040510     C                   SETON                                        31
118300040510     C                   EVAL      VABNSP = *zeros
118400040510     C                   EVAL      vinmsg = %trimr(vinmsg)
118500040510     C                             + ' ' + 'VABNSP'
118600040510     C                   ENDIF
118700040510     C                   ENDIF
118800071106     C*** RMN
118900071106     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMN'
119000071106     C                   EVAL      PiStr=SkSplitCSV(i)
119100071106     C                   EXSR      CHKNUM
119200071106     C                   IF        PiInt=*on
119300071106     C                   Z-ADD     PiVal         VABRMN
119400071106     C                   ELSE
119500071106     C                   SETON                                        32
119600071106     C                   EVAL      VABRMN = *zeros
119700071106     C                   EVAL      vinmsg = %trimr(vinmsg)
119800071106     C                             + ' ' + 'VABRMN'
119900071106     C                   ENDIF
120000071106     C                   ENDIF
120100030721     C*** CAD
120200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAD'
120300030721     C                   EVAL      PiStr=SkSplitCSV(i)
120400030721     C                   EXSR      CHKNUM
120500030721     C                   IF        PiInt=*on
120600030721     C                   Z-ADD     PiVal         Num5_0
120700030721     C                   MOVEL(p)  Num5_0        VABCAD
120800030721     C                   ELSE
120900030721     C                   SETON                                        32
121000030721     C                   EVAL      VABCAD = *zeros
121100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
121200030721     C                             + ' ' + 'VABCAD'
121300030721     C                   ENDIF
121400030721     C                   ENDIF
121500030721     C*** NCL
121600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCL'
121700030721     C                   EVAL      PiStr=SkSplitCSV(i)
121800030721     C                   EXSR      CHKNUM
121900030721     C                   IF        PiInt=*on
122000030721     C                   Z-ADD     PiVal         VABNCL
122100030721     C                   ELSE
122200030721     C                   SETON                                        32
122300030721     C                   EVAL      VABNCL = *zeros
122400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
122500040122     C                             + ' ' + 'VABNCL'
122600030721     C                   ENDIF
122700030721     C                   ENDIF
122800030721     C*** PKB
122900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABPKB'
123000030721     C                   EVAL      PiStr=SkSplitCSV(i)
123100030721     C                   EXSR      CHKNUM
123200030721     C                   IF        PiNum=*on
123300030721     C                   Z-ADD     PiVal         VABPKB
123400030721     C                   ELSE
123500030721     C                   SETON                                        32
123600030721     C                   EVAL      VABPKB = *zeros
123700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
123800030721     C                             + ' ' + 'VABPKB'
123900030721     C                   ENDIF
124000030721     C                   ENDIF
124100030721     C*** CAS
124200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAS'
124300110506     C                   IF        %trim(SkSplitCSV(i)) <> *blanks AND
124400110506     C                             %trim(SkSplitCSV(i)) <> *zeros
124500101018     C                   EVAL      wFlgCAS = '1'
124600101018     C                   ENDIF
124700030721     C                   EVAL      PiStr=SkSplitCSV(i)
124800030721     C                   EXSR      CHKNUM
124900030721     C                   IF        PiNum=*on
125000030721     C                   Z-ADD     PiVal         VABCAS
125100030721     C                   ELSE
125200030721     C                   SETON                                        32
125300030721     C                   EVAL      VABCAS = *zeros
125400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
125500030721     C                             + ' ' + 'VABCAS'
125600030721     C                   ENDIF
125700030721     C                   ENDIF
125800030721     C*** CCM
125900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCCM'
126000030721     C                   EVAL      PiStr=SkSplitCSV(i)
126100030721     C                   EXSR      CHKNUM
126200030721     C                   IF        PiInt=*on      AND
126300030721     C                             PiVal<=9999999 AND
126400030721     C                             PiVal>*zeros
126500030721     C                   Z-ADD     PiVal         VABCCM
126600030721     C                   ELSE
126700030721     C                   SETON                                        32
126800030721     C                   EVAL      VABCCM = *zeros
126900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
127000040510     C                             + ' ' + 'VABCCM'
127100030721     C                   ENDIF
127200030721     C                   ENDIF
127300030721     C*** LNP
127400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNP'
127500030721     C                   EVAL      PiStr=SkSplitCSV(i)
127600030721     C                   EXSR      CHKNUM
127700030721     C                   IF        PiInt=*on    AND
127800030721     C                             PiVal<=999   AND
127900030721     C                             PiVal>*zeros
128000030721     C                   Z-ADD     PiVal         VABLNP
128100030721     C                   ELSE
128200040715     C                   SETON                                        32
128300030721     C                   EVAL      VABLNP = *zeros
128400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
128500040510     C                             + ' ' + 'VABLNP'
128600030721     C                   ENDIF
128700030721     C                   ENDIF
128800030721     C*** NRS
128900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNRS'
129000030721     C                   EVAL      PiStr=SkSplitCSV(i)
129100030721     C                   EXSR      CHKNUM
129200030721     C                   IF        PiInt=*on AND
129300030721     C                             PiVal<=99
129400030721     C                   Z-ADD     PiVal         VABNRS
129500030721     C                   ELSE
129600030721     C                   SETON                                        32
129700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
129800030721     C                             + ' ' + 'VABNRS'
129900030721     C                   ENDIF
130000030721     C                   ENDIF
130100030721     C*** LNA
130200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNA'
130300030721     C                   EVAL      PiStr=SkSplitCSV(i)
130400030721     C                   EXSR      CHKNUM
130500030721     C                   IF        PiInt=*on    AND
130600030721     C                             PiVal<=999   AND
130700030721     C                             PiVal>*zeros
130800030721     C                   Z-ADD     PiVal         VABLNA
130900030721     C                   ELSE
131000030721     C                   SETON                                        32
131100030721     C                   EVAL      VABLNA = *zeros
131200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
131300030721     C                             + ' ' + 'VABLNA'
131400030721     C                   ENDIF
131500030721     C                   ENDIF
131600030721     C*** CTR
131700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTR'
131800030721     C                   EVAL      PiStr=SkSplitCSV(i)
131900030721     C                   EXSR      CHKNUM
132000030721     C                   IF        PiInt=*on  AND
132100030721     C                             PiVal<=999
132200030721     C                   Z-ADD     PiVal         VABCTR
132300030721     C                   ELSE
132400030721     C                   SETON                                        32
132500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
132600030721     C                             + ' ' + 'VABCTR'
132700030721     C                   ENDIF
132800030721     C                   ENDIF
132900030721     C*** IAS
133000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABIAS'
133100030721     C                   EVAL      PiStr=SkSplitCSV(i)
133200030721     C                   EXSR      CHKNUM
133300030721     C                   IF        PiNum=*on
133400030721     C                   Z-ADD     PiVal         VABIAS
133500030721     C                   ELSE
133600030721     C                   SETON                                        32
133700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
133800030721     C                             + ' ' + 'VABIAS'
133900030721     C                   ENDIF
134000030721     C                   ENDIF
134100030721     C*** VLB
134200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVLB'
134300030721     C                   EVAL      PiStr=SkSplitCSV(i)
134400030721     C                   EXSR      CHKNUM
134500030721     C                   IF        PiNum=*on
134600030721     C                   Z-ADD     PiVal         VABVLB
134700030721     C                   ELSE
134800030721     C                   SETON                                        32
134900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
135000030721     C                             + ' ' + 'VABVLB'
135100030721     C                   ENDIF
135200030721     C                   ENDIF
135300030721     C*** QFT
135400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABQFT'
135500030721     C                   EVAL      PiStr=SkSplitCSV(i)
135600030721     C                   EXSR      CHKNUM
135700030721     C                   IF        PiNum=*on
135800030721     C                   Z-ADD     PiVal         VABQFT
135900030721     C                   ELSE
136000030721     C                   SETON                                        32
136100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
136200030721     C                             + ' ' + 'VABQFT'
136300030721     C                   ENDIF
136400030721     C                   ENDIF
136500030721     C*** NCD
136600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCD'
136700030721     C                   EVAL      PiStr=SkSplitCSV(i)
136800030721     C                   EXSR      CHKNUM
136900030721     C                   IF        PiInt=*on      AND
137000030721     C                             PiVal<=9999999
137100030721     C                   Z-ADD     PiVal         VABNCD
137200030721     C                   ELSE
137300030721     C                   SETON                                        32
137400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
137500030721     C                             + ' ' + 'VABNCD'
137600030721     C                   ENDIF
137700030721     C                   ENDIF
137800030721     C*** NCA
137900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCA'
138000030721     C                   EVAL      PiStr=SkSplitCSV(i)
138100030721     C                   EXSR      CHKNUM
138200030721     C                   IF        PiInt=*on      AND
138300030721     C                             PiVal<=9999999
138400030721     C                   Z-ADD     PiVal         VABNCA
138500030721     C                   ELSE
138600030721     C                   SETON                                        32
138700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
138800030721     C                             + ' ' + 'VABNCA'
138900030721     C                   ENDIF
139000030721     C                   ENDIF
139100030721     C*** ZNC
139200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABZNC'
139300030721     C                   EVAL      PiStr=SkSplitCSV(i)
139400030721     C                   EXSR      CHKNUM
139500030721     C                   IF        PiInt=*on AND
139600030721     C                             PiVal<=99
139700030721     C                   Z-ADD     PiVal         VABZNC
139800030721     C                   ELSE
139900030721     C                   SETON                                        32
140000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
140100030721     C                             + ' ' + 'VABZNC'
140200030721     C                   ENDIF
140300030721     C                   ENDIF
140400030721     C*** DCR
140500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABDCR'
140600030721     C                   EVAL      PiStr=SkSplitCSV(i)
140700030721     C                   EXSR      CHKNUM
140800030721     C                   IF        PiInt=*on       AND
140900030721     C                             PiVal<=99999999
141000030721     C                   Z-ADD     PiVal         VABDCR
141100030721     C                   ELSE
141200030721     C                   SETON                                        32
141300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
141400030721     C                             + ' ' + 'VABDCR'
141500030721     C                   ENDIF
141600030721     C                   ENDIF
141700030721     C*** HCR
141800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABHCR'
141900030721     C                   EVAL      PiStr=SkSplitCSV(i)
142000030721     C                   EXSR      CHKNUM
142100030721     C                   IF        PiInt=*on   AND
142200030721     C                             PiVal<=9999
142300030721     C                   Z-ADD     PiVal         VABHCR
142400030721     C                   ELSE
142500030721     C                   SETON                                        32
142600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
142700030721     C                             + ' ' + 'VABHCR'
142800030721     C                   ENDIF
142900030721     C                   ENDIF
143000030721     C*** VMD
143100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVMD'
143200030721     C                   EVAL      PiStr=SkSplitCSV(i)
143300030721     C                   EXSR      CHKNUM
143400030721     C                   IF        PiNum=*on
143500030721     C                   Z-ADD     PiVal         VABVMD
143600030721     C                   ELSE
143700030721     C                   SETON                                        32
143800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
143900030721     C                             + ' ' + 'VABVMD'
144000030721     C                   ENDIF
144100030721     C                   ENDIF
144200030721     C*** ANT
144300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABANT'
144400030721     C                   EVAL      PiStr=SkSplitCSV(i)
144500030721     C                   EXSR      CHKNUM
144600030721     C                   IF        PiInt=*on        AND
144700030721     C                             PiVal<=999999999
144800030721     C                   Z-ADD     PiVal         VABANT
144900030721     C                   ELSE
145000030721     C                   SETON                                        32
145100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
145200030721     C                             + ' ' + 'VABANT'
145300030721     C                   ENDIF
145400030721     C                   ENDIF
145500030721     C*** CMO
145600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCMO'
145700030721     C                   EVAL      PiStr=SkSplitCSV(i)
145800030721     C                   EXSR      CHKNUM
145900030721     C                   IF        PiInt=*on
146000030721     C                   Z-ADD     PiVal         Num5_0
146100030721     C                   MOVEL(p)  Num5_0        VABCMO
146200030721     C                   ELSE
146300030721     C                   SETON                                        32
146400030721     C                   EVAL      VABCMO = *zeros
146500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
146600030721     C                             + ' ' + 'VABCMO'
146700030721     C                   ENDIF
146800030721     C                   ENDIF
146900040510     C*
147000110930     C* Reperisco i campi del EDIVAT
147100040510     C*** VATNOT
147200040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT'
147300040510     C                   EVAL      VATNOT = %trim(SkSplitCSV(i))
147400040510     C                   ENDIF
147500040510     C*** VATTRC
147600040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATTRC'
147700040510     C                   EVAL      VATTRC = %trim(SkSplitCSV(i))
147800040510     C                   ENDIF
147900060331     C*** VATNOT_A
148000060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_A'
148100060331     C                   EVAL      wVATNOT_A = %trim(SkSplitCSV(i))
148200060331     C                   ENDIF
148300060331     C*** VATNOT_B
148400060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_B'
148500060331     C                   EVAL      wVATNOT_B = %trim(SkSplitCSV(i))
148600060331     C                   ENDIF
148700060331     C*** VATNOT_E
148800070823     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_E'
148900060331     C                   EVAL      wVATNOT_E = %trim(SkSplitCSV(i))
149000060331     C                   ENDIF
149100110927     C*** VATNOT_I
149200110927     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_I'
149300110927     C                   EVAL      wVATNOT_I = %trim(SkSplitCSV(i))
149400110927     C                   ENDIF
149500140904     C*** VATNOT_S
149600140904     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_S'
149700140904     C                   EVAL      wVATNOT_S = %trim(SkSplitCSV(i))
149800140904     C                   ENDIF
149900070823     C*
150000100831     C***  ===>  Gestione campi "particolari" dentro ciclo
150100121030     C*
150200121030     C*** RMA
150300121030     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABRMA'
150400121030     C                   EVAL      wVABRMA = %trim(SkSplitCSV(i))
150500121030     C                   ENDIF
150600121030     C*
150700121030     C*** RMN/RMA
150800121030     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABRMN'
150900121030     C                   EVAL      PiStr=SkSplitCSV(i)
151000121030     C                   EXSR      CHKNUM
151100121030     C                   IF        PiInt=*on
151200121030     C                   Z-ADD     PiVal         wVABRMN
151300121030     C                   Z-ADD     PiVal         VABRMN
151400121030     C                   ELSE
151500121030     C                   SETON                                        31
151600121030     C                   EVAL      VABRMN = 1
151700121030     C                   EVAL      vinmsg = %trimr(vinmsg)
151800121030     C                             + ' ' + 'VABRMN'
151900121030     C                   ENDIF
152000121030     C                   ENDIF
152100120202     C*
152200121030     C*** RSD01
152300121030     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABRSD_1'
152400121030     C                   EVAL      wVABRSD01 = %trim(SkSplitCSV(i))
152500120202     C                   ENDIF
152600121030     C*
152700121030     C*** RSD02
152800121030     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABRSD_2'
152900121030     C                   EVAL      wVABRSD02 = %trim(SkSplitCSV(i))
153000121030     C                   ENDIF
153100121030     C*
153200121030     C*** IND01
153300121030     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABIND_1'
153400121030     C                   EVAL      wVABIND01 = %trim(SkSplitCSV(i))
153500121030     C                   ENDIF
153600121030     C*
153700121030     C*** IND02
153800121030     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABIND_2'
153900121030     C                   EVAL      wVABIND01 = %trim(SkSplitCSV(i))
154000121030     C                   ENDIF
154100121030     C*
154200121030     C*** CAD01
154300121030     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABCAD_1'
154400121030     C                   EVAL      PiStr=SkSplitCSV(i)
154500121030     C                   EXSR      CHKNUM
154600121030     C                   IF        PiInt=*on
154700121030     C                   Z-ADD     PiVal         Num5_0
154800121030     C                   MOVEL(p)  Num5_0        wVABCAD01
154900121030     C                   ELSE
155000121030     C                   SETON                                        32
155100121030     C                   EVAL      wVABCAD01 = *zeros
155200121030     C                   EVAL      vinmsg = %trimr(vinmsg)
155300121030     C                             + ' ' + 'VABCAD'
155400121030     C                   ENDIF
155500121030     C                   ENDIF
155600121030     C*
155700121030     C*** CAD02
155800121030     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABCAD_2'
155900121030     C                   EVAL      PiStr=SkSplitCSV(i)
156000121030     C                   EXSR      CHKNUM
156100121030     C                   IF        PiInt=*on
156200121030     C                   Z-ADD     PiVal         Num5_0
156300121030     C                   MOVEL(p)  Num5_0        wVABCAD02
156400121030     C                   ELSE
156500121030     C                   SETON                                        32
156600121030     C                   EVAL      wVABCAD02 = *zeros
156700121030     C                   EVAL      vinmsg = %trimr(vinmsg)
156800121030     C                             + ' ' + 'VABCAD'
156900121030     C                   ENDIF
157000121030     C                   ENDIF
157100121030     C*
157200121030     C*** LOD01
157300121030     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABLOD_1'
157400121030     C                   EVAL      wVABLOD01 = %trim(SkSplitCSV(i))
157500121030     C                   ENDIF
157600121030     C*
157700121030     C*** LOD02
157800121030     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABLOD_2'
157900121030     C                   EVAL      wVABLOD01 = %trim(SkSplitCSV(i))
158000121030     C                   ENDIF
158100121030     C*
158200121030     C*** PRD01
158300121030     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABPRD_1'
158400121030     C                   EVAL      wVABPRD01 = %trim(SkSplitCSV(i))
158500121030     C                   ENDIF
158600121030     C*
158700121030     C*** PRD02
158800121030     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABPRD_2'
158900121030     C                   EVAL      wVABPRD01 = %trim(SkSplitCSV(i))
159000121030     C                   ENDIF
159100121030     C*
159200121030     C*** IAS/QFT
159300121030     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABIAS'
159400121030     C                   EVAL      PiStr=SkSplitCSV(i)
159500121030     C                   EXSR      CHKNUM
159600121030     C                   IF        PiNum=*on
159700121030     C                   Z-ADD     PiVal         wVABIAS
159800121030     C                   ELSE
159900121030     C                   SETON                                        32
160000121030     C                   EVAL      vinmsg = %trimr(vinmsg)
160100121030     C                             + ' ' + 'VABIAS'
160200121030     C                   ENDIF
160300121030     C                   ENDIF
160400121030     C*
160500121030     C*** ANT/GVA
160600121030     C                   IF        %trim(SkSplitFLD(i)) = 'W_TOTPAL'
160700121030     C                   EVAL      PiStr=SkSplitCSV(i)
160800121030     C                   EXSR      CHKNUM
160900121030     C                   IF        PiNum=*on and PiVal > 0
161000121106     C                   Z-ADD     PiVal         wANT_34           2 0
161100150320     C                   EVAL      VABANT = 030000000+(wANT_34*100000)
161200121030     C                   EVAL      VABGVA = 'B'
161300121031     C                   ENDIF
161400121031     C                   ENDIF
161500140904     C*
161600140904     C*** W_VATNOT_I: è sia VATNOT_I che VATNOT_J
161700140904     C                   IF        %trim(SkSplitFLD(i)) = 'W_VATNOT_I'
161800140904     C                   EVAL      wVATNOT_IJ = %trim(SkSplitCSV(i))
161900140904     C                   ENDIF
162000120202     C*
162100121030     C*** VATNOT_E
162200121030     C                   IF        %trim(SkSplitFLD(i)) = 'W_VATNOT_E'
162300121030     C                   EVAL      wVATNOT_E = %trim(SkSplitCSV(i))
162400120202     C                   ENDIF
162500150508     C*
162600150508     C*** W_VABCBO
162700150508     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABCBO'
162800150508     C                   SELECT
162900150508     C                   WHEN      %trim(SkSplitCSV(i)) = 'F'
163000150508     C                   EVAL      VABCBO='1'
163100150508     C                   WHEN      %trim(SkSplitCSV(i)) = 'A'
163200150508     C                   EVAL      VABCBO='2'
163300150508     C                   ENDSL
163400150508     C                   ENDIF
163500070925     C*
163600071029     C***  <===  -----------------------------------------
163700040510     C*
163800030715     C                   ENDIF
163900030715     C                   EVAL      i = i + 1
164000030715     C                   ENDDO
164100070925     C*
164200071029     C***  ===>  Gestione campi "particolari" fuori ciclo
164300100831     C*
164400121030     C* RMA è composto da una parte reperita da una colonna e dall'RMN
164500121030     C                   EVAL      VABRMA = %trim(wVABRMA)
164600121030     C                               + %subst(%editc(wVABRMN :'X') : 3: 13)
164700121030     C*
164800121030     C* se valorizzato il 2 prendo quello, altrimenti l'1
164900121030     C                   IF        wVABRSD02 <> *blanks
165000121030     C                   EVAL      VABRSD = wVABRSD02
165100121030     C                   ELSE
165200121030     C                   EVAL      VABRSD = wVABRSD01
165300121030     C                   ENDIF
165400121030     C*
165500121030     C* se valorizzato il 2 prendo quello, altrimenti l'1
165600121030     C                   IF        wVABIND02 <> *blanks
165700121030     C                   EVAL      VABIND = wVABIND02
165800121030     C                   ELSE
165900121030     C                   EVAL      VABIND = wVABIND01
166000121030     C                   ENDIF
166100121030     C*
166200121030     C* se valorizzato il 2 prendo quello, altrimenti l'1
166300121030     C                   IF        wVABCAD02 <> *blanks
166400121030     C                   EVAL      VABCAD = wVABCAD02
166500121030     C                   ELSE
166600121030     C                   EVAL      VABCAD = wVABCAD01
166700121030     C                   ENDIF
166800121030     C*
166900121030     C* se valorizzato il 2 prendo quello, altrimenti l'1
167000121030     C                   IF        wVABLOD02 <> *blanks
167100121030     C                   EVAL      VABLOD = wVABLOD02
167200121030     C                   ELSE
167300121030     C                   EVAL      VABLOD = wVABLOD01
167400121030     C                   ENDIF
167500121030     C*
167600121030     C* se valorizzato il 2 prendo quello, altrimenti l'1
167700121030     C                   IF        wVABPRD02 <> *blanks
167800121030     C                   EVAL      VABPRD = wVABPRD02
167900121030     C                   ELSE
168000121030     C                   EVAL      VABPRD = wVABPRD01
168100121030     C                   ENDIF
168200121030     C*
168300121030     C* IAS e QFT sono lo stesso valore
168400121030     C                   EVAL      VABIAS = wVABIAS
168500121030     C                   EVAL      vabVAS = 'EUR'
168600121030     C                   EVAL      VABQFT = wVABIAS
168700140904     C*
168800140904     C* email destinatario
168900140904     C                   IF        %trim(wVATNOT_IJ) <> *blanks
169000140904     C                   EVAL      wVATNOT_I = %subst(wVATNOT_IJ:1:35)
169100140904     C                   EVAL      wVATNOT_J = %subst(wVATNOT_IJ:36:35)
169200140904     C                   ENDIF
169300070925     C*
169400071029     C***  <===  -----------------------------------------
169500070925     C*
169600070925     C* Considerazioni finali su CBO/CAS
169700100318     C                   IF        wFlgCAS = '1'    OR
169800100318     C                             VABCAS  > *zeros
169900070925     C                   IF        VABCBO = '1'
170000070925     C                   EVAL      VABCBO = '4'
170100070925     C                   ENDIF
170200070925     C                   IF        VABCBO = '2'
170300070925     C                   EVAL      VABCBO = '6'
170400070925     C                   ENDIF
170500070925     C                   ENDIF
170600070925     C***  <===  ----------------------------
170700040510     C*
170800040715     C* Se NSP nn valorizzato considero errore bloccante in traduzione
170900040510     C                   IF        VABNSP = *zeros
171000040510     C                   SETON                                        31
171100040510     C                   ENDIF
171200040715     C*
171300040715     C* Se LNP nn valorizzato considero errore bloccante in traduzione
171400040715     C                   IF        VABLNP = *zeros
171500040715     C                   SETON                                        31
171600040715     C                   ENDIF
171700040510     C*
171800040714     C* Se RMN nn valorizzato lo forzo uguale a VABNSP
171900040510     C                   IF        VABRMN = *zeros
172000040510     C                   Z-ADD     VABNSP        VABRMN
172100040510     C                   ENDIF
172200040510     C*
172300040714     C* Se RMA nn valorizzato lo forzo uguale a VABRMN
172400040510     C                   IF        VABRMA = *blanks
172500040714     C                   MOVEL     VABRMN        VABRMA
172600040510     C                   ENDIF
172700040128     C*
172800040510     C* Se provincia nn valorizzata la reperisco
172900040510     C* tramite TISI95R a seconda dei dati d instradamento presenti
173000110912     C                   IF        VABPRD =  *blanks AND
173100110912     C                             VABNZD =  *blanks AND
173200110912     C                             VABCAD <> *blanks AND
173300110912     C                             VABLOD <> *blanks
173400040128     C                   CLEAR                   TISI95DS
173500040128     C                   EVAL      I95TCN = '3'
173600040128     C                   Z-ADD     datcor        I95DAT
173700040128     C                   EVAL      I95NAR = VABNZD
173800040128     C                   EVAL      I95CAP = VABCAD
173900040128     C                   EVAL      I95LOC = VABLOD
174000040128     C                   CALL      'TISI95R'
174100040128     C                   PARM                    TISI95DS
174200040128     C                   EVAL      VABPRD = O95PRV
174300040510     C                   ENDIF
174400020204     C*
174500020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
174600020204     C                   EXSR      CHKIMPDIV
174700030715     C                   ENDIF
174800020204     C*
174900010607     C                   ENDSR
175000010607     C*----------------------------------------------------*
175100010601
175200020204
175300020204     C*----------------------------------------------------*
175400020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
175500020204     C*----------------------------------------------------*
175600020204     C     CHKIMPDIV     BEGSR
175700020204     C*
175800020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
175900020204     C                   Z-ADD     *zeros        wrkDec            9 9
176000020204     C*
176100020204     C* Come prima cosa effettuo considerazioni sulla divisa
176200020204     C                   IF        vabIAS > *zeros
176300020204     C                   IF        vabVAS <> 'EUR'
176400020204     C                   EVAL      vabVAS =  'ITL'
176500020204     C                   ENDIF
176600020204     C                   ENDIF
176700020204     C*
176800020204     C                   IF        vabCAS > *zeros
176900020204     C                   IF        vabVCA <> 'EUR'
177000020204     C                   EVAL      vabVCA =  'ITL'
177100020204     C                   ENDIF
177200020204     C                   ENDIF
177300020204     C*
177400020204     C                   IF        vabVMD > *zeros
177500020204     C                   IF        vabVAD <> 'EUR'
177600020204     C                   EVAL      vabVAD =  'ITL'
177700020204     C                   ENDIF
177800020204     C                   ENDIF
177900020204     C*
178000020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
178100020204     C                   Z-ADD     vabIAS        wrkDec
178200020204     C                   IF        wrkDec > *zeros
178300020204     C                   IF        vabVAS = 'ITL'
178400020204     C                   EVAL      vabIAS = *zeros
178500020204     C                   ENDIF
178600020204     C                   ENDIF
178700020204     C*
178800020204     C* Stabilisco se il contrasegno ha decimali valorizzati
178900020204     C                   Z-ADD     vabCAS        wrkDec
179000020204     C                   IF        wrkDec > *zeros
179100020204     C                   IF        vabVCA = 'ITL'
179200020204     C                   EVAL      vabCAS = *zeros
179300020204     C                   ENDIF
179400020204     C                   ENDIF
179500020204     C*
179600020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
179700020204     C                   Z-ADD     vabVMD        wrkDec
179800020204     C                   IF        wrkDec > *zeros
179900020204     C                   IF        vabVAD = 'ITL'
180000020204     C                   EVAL      vabVMD = *zeros
180100020204     C                   ENDIF
180200020204     C                   ENDIF
180300020204     C*
180400020204     C                   ENDSR
180500020204     C***
180600010330
180700100318
180800100318
180900100318     C*----------------------------------------------------*
181000100318     C*  CONTROLLO NUMERICITA' CAMPI
181100100318     C*----------------------------------------------------*
181200100318     C     CHKNUM        BEGSR
181300100318     C*
181400100318     C                   IF        PiDecChr = *blanks
181500100318     C                   EVAL      PiDecChr = CharNUM
181600100318     C                   ENDIF
181700100318     C*
181800100318     C                   callp     UBISNUM_Check(PiStr
181900100318     C                                          :PiDecChr
182000100318     C                                          :PiVal
182100100318     C                                          :PiNum
182200100318     C                                          :PiInt)
182300100318     C*
182400100318     C                   ENDSR
182500100318     C***
182600010601
182700010601
182800010601
182900010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
183000010601     C     repfil        BEGSR
183100010601     C*
183200010601     C                   if        invfil = *zeros and
183300010601     C                             depfil > *zeros and
183400010629     C                             (vinflg = *blanks or
183500010629     C                              vinflg = *zeros)
183600010601     C
183700010601     C                   eval      invfil = depfil
183800010601     C                   endif
183900010601     C*
184000010601     C                   if        depfil <> invfil and
184100010601     C                             invfil > *zeros
184200010601     C                   eval      flgMulti = '1'
184300010601     C                   if        vinflg = *blanks
184400010601     C                   add       1             cntNonEl
184500010601     C                   endif
184600010601     C                   endif
184700010601     C*
184800010601     C                   if        vinflg = '2'
184900010601     C                   eval      flgStato = '2'
185000010601     C                   endif
185100010601     C*
185200010601     C                   ENDSR
185300010601     C***
185400010601
185500010601
185600010601
185700010330
185800040119      /TITLE Invio dei dati al punto operativo.
185900040119     C     invio         BEGSR
186000040119     C*
186100110930     C* 1° invio EDIVAT
186200040119     C                   reset                   dscmz
186300040119     C                   move      vlrpoi        cmzdst
186400110930     C                   eval      cmzfld = 'EDIVATWR'
186500040119     C                   eval      cmzmbd = vlrhdl
186600040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
186700040119     C***                if        prmfir = *blanks
186800110930     C                   eval      cmzfla = 'EDIVAT0F'
186900110930     C                   eval      cmzmba = 'EDIVAT0F'
187000040119     C***                else
187100040119     C***                eval      cmzfla = prmfir
187200040119     C***                eval      cmzmba = prmfir
187300040119     C***                endif
187400040119     C                   eval      cmznrr = *zeros
187500040119     C                   move      §ctrokvt      cmznrr
187600040119     C                   eval      cmzlba = vlrfl1
187700040119     C                   call(e)   'TIS711C'
187800040119     C                   parm                    dscmz
187900040119     C                   parm      *blanks       esito
188000040119     C                   if        %error
188100040119     C                             or cmzerr = '1'
188200040119     C                             or esito  = '1'
188300040119     C                   eval      wrkesito = '3'
188400040119     C                   else
188500040119     C*
188600110930     C* 2° invio EDIVAB
188700040119     C                   reset                   dscmz
188800040119     C                   move      vlrpoi        cmzdst
188900040119     C                   eval      cmzfld = vlrfou
189000040119     C                   eval      cmzmbd = vlrhdl
189100040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
189200040119     C***                if        prmfir = *blanks
189300110930     C                   eval      cmzfla = 'EDIVAB0F'
189400110930     C                   eval      cmzmba = 'EDIVAB0F'
189500040119     C***                else
189600040119     C***                eval      cmzfla = prmfir
189700040119     C***                eval      cmzmba = prmfir
189800040119     C***                endif
189900040119     C                   eval      cmznrr = *zeros
190000040119     C                   move      §ctrokvb      cmznrr
190100040119     C                   eval      cmzlba = vlrfl1
190200040119     C                   call(e)   'TIS711C'
190300040119     C                   parm                    dscmz
190400040119     C                   parm      *blanks       esito
190500040119     C                   if        %error
190600040119     C                             or cmzerr = '1'
190700040119     C                             or esito  = '1'
190800040119     C                   eval      wrkesito = '3'
190900040119     C                   endif
191000040119     C                   endif
191100040119     C*
191200040119     C                   ENDSR
191300040119     C***
191400010601
191500010601
191600010601
191700010601
191800010601      /TITLE Invio dei dati al punto operativo.
191900010601     C     opeini        BEGSR
192000010601     C*
192100010601     C* Inizializzo flag e contatori operativi
192200010601     C                   movel     '0'           flgGiro           1
192300010601     C                   movel     '0'           flgMulti          1
192400010601     C                   movel     '1'           flgStato          1
192500010615     C                   movel     '0'           flgOk             1
192600010601     C                   z-add     *zeros        cntNonEl         10 0
192700010601     C                   z-add     *zeros        depfil            3 0
192800010601     C                   z-add     *zeros        invfil            3 0
192900010601     C*
193000010601     C                   ENDSR
193100010601     C***
193200070326
193300070326
193400070326
193500070326
193600070326     C     *pssr         BEGSR
193700070329     C*
193800070329     C                   if        %open(tivin00r)
193900070329     C                   close     tivin00r
194000070329     C                   endif
194100110930     C                   if        %open(edivabwr)
194200110930     C                   close     edivabwr
194300070329     C                   endif
194400110930     C                   if        %open(edivatwr)
194500110930     C                   close     edivatwr
194600070329     C                   endif
194700070326     C*
194800070326     C* Effettuo la chiamata al CLLE preposto
194900110930     C                   call(e)   'TITVEVTC'
195000070326     C                   parm                    parccm
195100070326     C                   parm                    parmbr
195200070326     C                   parm      '2'           paropz
195300070326     C*
195400070326     C                   eval      wrkesito = '2'
195500070404     C*
195600070404     C                   seton                                        LR
195700070326     C*
195800070326     C                   ENDSR     '*CANCL'
195900070326     C***
196000100318
196100100318
196200100318
196300100318     C     normCSV       BEGSR
196400100318     C*
196500100318     C* Se previsti caratteri delimitatori testo provvedo a verificare i caratteri
196600100318     C* separatori campi
196700100318     C                   if        CharTXT <> *blanks
196800100318     C                   z-add     *zeros        wLenVINDTA        4 0
196900100318     C                   z-add     *zeros        wFlgVINDTA        1 0
197000100318     C                   setoff                                       80
197100100318     C                   dow       wLenVINDTA < %len(%trim(vindta))
197200100318     C                   eval      wLenVINDTA = wLenVINDTA + 1
197300100318     C                   if        %subst(vindta:wLenVINDTA:1)=CharTXT
197400100318     C                   if        not *in80
197500100318     C                   seton                                        80
197600100318     C                   else
197700100318     C                   setoff                                       80
197800100318     C                   endif
197900100318     C                   endif
198000100318     C                   if        %subst(vindta:wLenVINDTA:1)=CharCSV
198100100318     C                             and *in80 = *on
198200100318     C                   eval      %subst(vindta:wLenVINDTA:1)=CharSOS
198300100318     C                   endif
198400100318     C                   enddo
198500100318     C                   endif
198600100318     C*
198700100318     C                   ENDSR
198800100318     C***
198900070326
199000100326
199100100326
199200100326     C*----------------------------------------------------*
199300100326     C*  STACCO NUMERATORE X NUMERO SPEDIZIONE
199400100326     C*----------------------------------------------------*
199500100326     C     REPNSP        BEGSR
199600100326     C*
199700100326     C* NSP => Stacco un numeratore da AZNUM
199800100326     C                   clear                   TRUL33DS
199900100326     C                   eval      I33OPE = *zeros
200000100326     C                   eval      I33CNU = 302
200100100326     C                   eval      I33NUM = 1
200200100326     C                   movel     TRUL33DS      KPJBU
200300100326     C                   call      'TRUL33R'
200400100326     C                   parm                    KPJBA
200500100326     C                   movel     KPJBU         TRUL33DS
200600100326     C                   if        O33ERR = *zeros
200700100326     C                   z-add     O33NRF        VABNSP
200800100326     C                   z-add     O33NRF        VATNSP
200900100326     C                   else
201000100326     C                   SETON                                        31
201100100326     C                   EVAL      vinmsg = %trimr(vinmsg)
201200100326     C                             + ' ' + 'VABNSP VATNSP'
201300100326     C                   endif
201400100326     C*
201500100326     C                   ENDSR
201600010330
201700100326
201800010330
201900000613     C     *inzsr        BEGSR
202000990910     C*
202100990910     C     *entry        plist
202200990920     C                   parm                    tivlrds
202300990921     C                   parm      wrkesito      esito
202400000724     C                   parm                    prmlit
202500000710     C                   parm                    prmfir
202600010330     C*
202700010330     C* CALCOLA LA DATA CORRENTE
202800110930     C                   time                    wn14             14 0
202900110930     C                   movel     wn14          oracor            6 0          *ORA
203000100318     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
203100100318     C                   eval      datcor = %dec(%date() : *ISO)
203200000613     C*
203300000613     C                   ENDSR
203400000613     C***
203500990908
203600120202** INTCOL - INTESTAZIONE COLONNE PERSONALIZZATA
203700121030W_VABRMA;W_VABRMN;X;X;W_VABRSD_1;W_VABIND_1;W_VABCAD_1;W_VABLOD_1;W_VABPRD_1;
203800121030W_VABRSD_2;W_VABIND_2;W_VABCAD_2;W_VABLOD_2;W_VABPRD_2;
203900150508X;X;X;VABNCL;W_VABIAS;VABPKB;W_TOTPAL;W_VATNOT_E;W_VATNOT_I;VATNOT_S;W_VABCBO;
204000150611VATNOT_B;VABTSP;VABTC1;VABGC1;
204100120202
