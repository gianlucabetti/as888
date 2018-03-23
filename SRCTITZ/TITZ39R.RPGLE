000100040119      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200120604      *
000300120604      * PARTICOLARITA':
000400120703      * RMN è oggi in giuliano senza l'anno.
000500120703      * NSP va staccato.
000600120703      * Il barcode ha una parte alfanumerica (i primi 2 char), sviluppo la parte numerica poi ci
000700120703      * attacco davanti quella alfa.
000800120703      * Il barcode sul csv è il valore di partenza che poi va incrementato per il nr.colli.
000900120604
001000121217     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP(*caller) BNDDIR('UBBNDDIR')
001100120209     F*
001200990910     Ftivin00r  uF   E             DISK    usropn
001300100413     FFIVABwwr  O    E             DISK    usropn prefix(ok_)
001400040119     FFIVATwwr  O    E             DISK    usropn
001500000313     D*
001600040113     D*------------
001700040113     D* SCHIEDE A PROGRAMMA
001800040113     D*------------
001900080429     D intcol          S            100    DIM(5) CTDATA PERRCD(1)
002000010330     D*----------------------------------------------------
002100010330     D* DICHIARAZIOINE VARIABILI DI WRK
002200010330     D*----------------------------------------------------
002300010330     D dscmz         e ds                  inz
002400010330     D psds           sds
002500010330     D  procname         *PROC
002600010330     D tivlrds       e ds                  extname(tivlr00f)
002700040128     D tisi95ds      e ds
002800100413     D fivabds       e ds                  extname(fivab00f)
002900100413     D fivabds_sav   e ds                  extname(fivab00f) prefix(sav_)
003000100413     D fivabds_ok    e ds                  extname(fivab00f) prefix(ok_)
003100010330     D esito           s              1
003200010330     D prmlit          s             10
003300010330     D prmfir          s             10
003400010330     D wrkesito        s                   like(esito)
003500010330     D rrnum           s              6  0 INZ(*zeros)
003600040510     D parccm          s              8    INZ(*blanks)
003700040119     D parmbr          s             10    INZ(*blanks)
003800040119     D paropz          s              1    INZ(*blanks)
003900040119     D chkcall         s              1    INZ(*blanks)
004000080513     D depspe          s                   LIKE(VABNSP) INZ(*zeros)
004100080418     D wFlgCAS         s              1    INZ(*blanks)
004200120410     D* variabile Intestazioni = *on se nel .csv la prima riga contiene le intestazioni di colonna
004300120410     D* variabile Intestazioni = *off se nel .csv ci sono solo dati
004400120604     D Intestazioni    s               n   inz(*on)
004500100326     D*------------------
004600100326     D* DS REPERIMENTO NUMERATORE
004700100326     D*------------------
004800100326     D trul33ds      e ds                  inz
004900100326     D*------------------
005000100326     D* DS ARCHITETTURA
005100100326     D*------------------
005200100326     D kpjba         e ds                  inz
005300120709     D*------------------
005400120709     D TRUL28DSX     e ds                  inz
005500100326
005600030715     D*------------------
005700030715     D* VARIABILI X LO SPLIT DELLA STRINGA CSV IN CAMPI
005800030715     D*------------------
005900030715     D SkSplitFLD      S             10    DIM(100)
006000030715     D SkSplitCSV      S            256    DIM(100)
006100030715     D CharCSV         S              1
006200030715     D CharTXT         S              1
006300030715     D CharNUM         S              1
006400100318     D CharSOS         S              1
006500030715     D posDa           S              3  0 INZ(*zeros)
006600030715     D posA            S              3  0 INZ(*zeros)
006700030715     D i               s              3  0 INZ(1)
006800030715     D wGiro           s              1  0 INZ(*zeros)
006900070823     D wVATNOT_A       s                   LIKE(VATNOT)
007000060331     D wVATNOT_B       s                   LIKE(VATNOT)
007100060331     D wVATNOT_E       s                   LIKE(VATNOT)
007200110927     D wVATNOT_I       s                   LIKE(VATNOT)
007300120411     D wNOT            s             70
007400120411     D Data8A          s              8
007500120709     D wBarcode        s             11
007600120709     D wBarcodeN       s             11s 0
007700120629     D wX              s              3s 0
007800030822     D*------------------
007900030822     D* VARIABILI X LO SPLIT DEI VALORI DI DEFAULT PROVENIENTI DAI PARAMETRI DEL TRADUTTORE
008000030822     D*------------------
008100030822     D posDaDft        S              3  0 INZ(*zeros)
008200030822     D posADft         S              3  0 INZ(*zeros)
008300030822     D j               s              3  0 INZ(1)
008400030822     D wGiroDft        s              1  0 INZ(*zeros)
008500080418     D*------------------
008600080418     D* Costanti
008700080418     D*------------------
008800080418     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
008900080418     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
009000100318
009100100318     D*------------------
009200100318     D* LINKING A DEFINIZIONI ESTERNE
009300100318     D*------------------
009400100318     D/COPY GAITRASRC/SRCPROTOPR,UBISNUM
009500100318     D/COPY GAITRASRC/SRCPROTOPI,UBISNUM
009600120703     D/COPY GAITRASRC/SRCPROTOPR,UBFMTDATE
009700010330
009800000913     C                   reset                   rrnum
009900990921     C                   reset                   esito
010000990921     C                   reset                   wrkesito
010100010601     C*
010200010601     C                   exsr      opeini
010300010605     C                   exsr      rwvab
010400010601     C*
010500010601     C                   seton                                        lr
010600010601
010700010601
010800010601
010900010601     C*--------------------------------------------------------
011000010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
011100010601     C*--------------------------------------------------------
011200010601     C     PREELA        BEGSR
011300010601     C*
011400010601     C* SE OCCORRE SPEDIRE IN FILIALE
011500010601     C                   if        invfil <> *zeros and
011600010601     C                             flgGiro = '0'
011700010601     C*
011800010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
011900010601     C                   eval      flgGiro = '1'
012000010601     C*
012100010601     C                   endif
012200010601     C*
012300010601     C                   ENDSR
012400010601     C***
012500010601
012600010601
012700010601
012800010601
012900010601     C*--------------------------------------------------------
013000010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
013100010601     C*--------------------------------------------------------
013200010601     C     ENDELA        BEGSR
013300040119     C*
013400040119     C* Effettuo la chiamata al CLLE preposto
013500040119     C                   call(e)   'TITVVTC'
013600040510     C                   parm                    parccm
013700040119     C                   parm                    parmbr
013800040119     C                   parm      '2'           paropz
013900050201     C*
014000050201     C* Effettuo lancio TISI95 solo x chiusura
014100050201     C                   CLEAR                   TISI95DS
014200050201     C                   EVAL      I95TLA = 'C'
014300050201     C                   CALL      'TISI95R'
014400050201     C                   PARM                    TISI95DS
014500000616     C*
014600010601     C                   ENDSR
014700010601
014800010601
014900010330     C*--------------------------------------------------------
015000021025     C* RWVAB   LEGGE TIVIN00R E SCRIVE FIVABWWF              *
015100010330     C*--------------------------------------------------------
015200010605     C     RWVAB         BEGSR
015300010330     C*
015400010330     C                   if        not %open(tivin00r)
015500010330     C                   open      tivin00r
015600010330     C                   endif
015700021025     C                   if        not %open(fivabwwr)
015800021025     C                   open      fivabwwr
015900010330     C                   endif
016000040119     C*
016100040119     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
016200040119     C                   exsr      prevat
016300040119     C*
016400040119     C                   if        chkcall = '0'
016500040119     C*
016600040119     C                   if        not %open(fivatwwr)
016700040119     C                   open      fivatwwr
016800040119     C                   endif
016900010330     C*
017000040119     C                   clear                   §CTROKVB          7 0
017100040119     C                   clear                   §CTROKVT          7 0
017200040119     C                   clear                   §CTRMO            7 0
017300040119     C                   clear                   §CTRNO            7 0
017400120410     C*
017500120410     C* variabile Intestazioni = *on se nel .csv la prima riga contiene le intestazioni di colonna
017600120410     C* variabile Intestazioni = *off se nel .csv ci sono solo dati
017700120410     C                   IF        Intestazioni = *off
017800120410     C* Eseguo primo giro x by-pasare la mancanza della 1° riga con le intestazioni
017900120410     C* colonne nel file del cliente (che contiene quindi solo le righe d dati).
018000120410     C                   exsr      inzvar
018100120410     C                   exsr      impvab
018200120410     C                   ENDIF
018300010330     C*
018400030822     C                   do        *HIVAL
018500010330     C*
018600030822     C                   read      tivin00r                               70
018700010618     C*
018800010618     C* Dopo ogni lettura verifico se ci sono stati record OK
018900010618     C                   if        vinflg = '1'
019000010618     C                   eval      flgOk = '1'
019100010618     C                   endif
019200040510     C*
019300040510     C* Verifico che il record nn contenga unicamente i caratteri d separatore campi
019400040510     C                   z-add     *zeros        wLenVINDTA        4 0
019500040510     C                   z-add     *zeros        wFlgVINDTA        1 0
019600040510     C                   dow       wLenVINDTA < %len(%trim(vindta))
019700040510     C                   eval      wLenVINDTA = wLenVINDTA + 1
019800040510     C                   if        %subst(%trim(vindta):wLenVINDTA:1)<>CharCSV
019900040510     C                   z-add     1             wFlgVINDTA
020000040510     C                   leave
020100040510     C                   endif
020200040510     C                   enddo
020300010618     C*
020400040510     C                   if        vindta > *blanks AND
020500040510     C                             wFlgVINDTA = 1
020600010330     C                   add       1             rrnum
020700010330     C*
020800010601     C                   if        *in70 = *off and
020900010330     C                             (vinflg = *blanks
021000010330     C                              or vinflg = '0'
021100010330     C                              or vinflg = '2')
021200010330     C*
021300010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
021400010711     C                   if        vinflg = *blanks or vinflg = '0'
021500010711     C                   clear                   vinmsg
021600010711     C                   endif
021700010601     C*
021800070925     C* Aggiungo un byte a blanks all'inizio d ogni record
021900070925     C                   eval      vindta  = ' ' + %trim(vindta)
022000070925     C*
022100080527     C                   exsr      inzvar
022200100318     C                   exsr      normCSV
022300010605     C                   exsr      impvab
022400010601     C*
022500010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
022600010605     C                   eval      depfil = VABLNP
022700010601     C                   exsr      repfil
022800010601     C                   if        depfil = invfil
022900021025     C                   if        vlrpoi = 999
023000030822     C                   move(P)   invfil        VABFGS
023100021025     C                   else
023200030822     C                   move(P)   vlrpoi        VABFGS
023300021025     C                   endif
023400010601     C*
023500010601     C                   exsr      PREELA
023600010601     C*
023700010604     C* Ebbene...
023800010604     C*
023900040119     C  N31              add       1             §CTROKVB
024000040119     C   32              add       1             §CTRMO
024100040119     C   31              add       1             §CTRNO
024200080418     C*
024300030822     C                   if        wGiro = 2
024400080513     C* Scrivo sempre il VAT
024500080513     C  N31              exsr      wrivat
024600110201     C                   exsr      chkvab
024700080513     C                   endif
024800010604     C*
024900010604     C                   if        *in31 = *off and
025000010604     C                             *in32 = *off
025100010604     C                   eval      vinflg = '1'
025200010604     C                   else
025300010604     C                   eval      vinflg = '2'
025400010604     C                   endif
025500010604     C                   endif
025600010601     C*
025700010604     C                   endif
025800010604     C*
025900010330     C                   else
026000010330     C                   eval      vinflg = '1'
026100010330     C                   endif
026200010601     C*
026300010601     C  N70              update    tivin000
026400010330     C*
026500030822     C  N70              enddo
026600040119     C*
026700080527     C* Verifico dati rimasti "in cannna"...
026800080527     C                   seton                                        65
026900110201     C                   exsr      chkvab
027000080527     C*
027100040119     C                   endif                                                  (endif - chkcall)
027200010601     C*
027300010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
027400010601     C                   if        cntNonEl = *zeros or
027500010601     C                             flgMulti = '0'
027600010330     C* Se non ci sono record con errori ...
027700010601     C                   if        §ctrno = 0 and
027800010604     C                             §ctrmo = 0 and
027900010601     C                             flgStato <> '2'
028000010330     C* ... restituisco esito OK.
028100010330     C                   eval      wrkesito = '0'
028200010330     C                   else
028300040119     C                   if        §ctrokvb > 0
028400010330     C                   eval      wrkesito = '1'
028500010330     C                   else
028600010615     C                   if        flgOk = '0'
028700010615     C                   eval      wrkesito = '2'
028800010615     C                   else
028900010615     C                   eval      wrkesito = '6'
029000010615     C                   endif
029100010330     C                   endif
029200010330     C                   endif
029300010601     C                   else
029400010601     C                   eval      wrkesito = '9'
029500010601     C                   endif
029600010330     C*
029700010330     C                   if        %open(tivin00r)
029800010330     C                   close     tivin00r
029900010330     C                   endif
030000021025     C                   if        %open(fivabwwr)
030100021025     C                   close     fivabwwr
030200010330     C                   endif
030300040512     C                   if        %open(fivatwwr)
030400040512     C                   close     fivatwwr
030500040512     C                   endif
030600010601     C*
030700010601     C                   if        vlrpoi <> 999
030800010601     C                   eval      invfil = vlrpoi
030900010601     C                   endif
031000010330     C*
031100040119     C                   if        §ctrokvb > 0
031200010601     C                             and invfil > *zeros
031300010330     C                   exsr      invio
031400010330     C                   endif
031500010601     C*
031600010618     C                   if        flgGiro = '1'
031700010601     C                   exsr      endela
031800010618     C                   endif
031900010330     C*
032000010330     C                   ENDSR
032100010330     C***
032200080527
032300080527
032400080527
032500080527     C*----------------------------------------------------*
032600080527     C*  CONSIDERAZIONI SU SCRITTURA BUFFER RECORDS VAB
032700080527     C*----------------------------------------------------*
032800100413     C     CHKVAB        BEGSR
032900110201     C*
033000110201     C* Se richiesti ignorare colli / peso / volume
033100110315 xxx C***                if        *in35
033200110315 xxx C***                z-add     *zeros        VABNCL
033300110315 xxx C***                z-add     *zeros        VABPKB
033400110315 xxx C***                z-add     *zeros        VABVLB
033500110315 xxx C***                endif
033600080527     C*
033700080527     C* Scrivo il VAB solo a rottura d spedizione
033800080527     C                   if        depspe = *zeros
033900080527     C  N31              eval      fivabds_sav = fivabds
034000080527     C                   eval      depspe = vabNSP
034100080527     C                   else
034200080527     C                   if        depspe = vabNSP AND *in65 = *off
034300120703     C   51              add       VABNCL        sav_VABNCL
034400100413     C   51              add       VABPKB        sav_VABPKB
034500100413     C   51              add       VABVLB        sav_VABVLB
034600100720     C   52              z-add     VABNCL        sav_VABNCL
034700100720     C   52              z-add     VABPKB        sav_VABPKB
034800100720     C   52              z-add     VABVLB        sav_VABVLB
034900100720     C   53              add       VABNCL        sav_VABNCL
035000100720     C   53              z-add     VABPKB        sav_VABPKB
035100100720     C   53              z-add     VABVLB        sav_VABVLB
035200080527     C                   else
035300080527     C  N31              eval      fivabds_ok = fivabds_sav
035400080527     C  N31              exsr      wrivab
035500080527     C  N31              eval      fivabds_sav = fivabds
035600080527     C                   eval      depspe = vabNSP
035700080527     C                   endif
035800080527     C                   endif
035900080527     C*
036000080527     C                   ENDSR
036100040119
036200040119
036300040119
036400040119     C*----------------------------------------------------*
036500040119     C*  SCARICAMENTO BUFFER RECORDS VAB
036600040119     C*----------------------------------------------------*
036700040510     C     WRIVAB        BEGSR
036800080513     C*
036900080513     C* Gestisco l'eventuale rottura x numero spedizione
037000080527     C                   if        VABNSP <> depspe or *in65 = *on
037100070301     C                   if        wVATNOT_E <> *blanks
037200070301     C                   eval      VABCTM = '7Q'
037300070301     C                   endif
037400100413     C                   write     fivab000
037500080513     C                   endif
037600040119     C*
037700040119     C                   ENDSR
037800040119
037900040119
038000040119
038100040119     C*----------------------------------------------------*
038200040119     C*  SCARICAMENTO BUFFER RECORDS VAT
038300040119     C*----------------------------------------------------*
038400040510     C     WRIVAT        BEGSR
038500060512     C*
038600060512     C* Inizializzo i campi chiave
038700060512     C                   eval      VATFGS = VABFGS
038800060512     C                   eval      VATAAS = VABAAS
038900060512     C                   eval      VATCCM = VABCCM
039000070823     C                   eval      VATNRS = VABNRS
039100060512     C                   eval      VATNSP = VABNSP
039200060512     C                   eval      VATLNP = VABLNP
039300040119     C*
039400040510     C* Se tipo record nn espressamente indicato assumo x default (e x diffusione d uso) = 'E'
039500040510     C                   if        VATTRC = *blanks
039600040510     C                   eval      VATTRC = 'E'
039700040510     C                   endif
039800110318     C                   if        VATNOT <> *blanks
039900040510     C                   write     FIVAT000
040000040119     C                   add       1             §CTROKVT
040100110318     C                   endif
040200060331     C*
040300060331     C* Gestisco estensioni VAT "specificatamente esplicitate"
040400060331     C                   if        wVATNOT_A <> *blanks
040500060331     C                   eval      VATNOT = wVATNOT_A
040600060331     C                   eval      VATTRC = 'A'
040700060331     C                   write     FIVAT000
040800060331     C                   add       1             §CTROKVT
040900060331     C                   endif
041000060331     C*
041100060331     C                   if        wVATNOT_B <> *blanks
041200060331     C                   eval      VATNOT = wVATNOT_B
041300060331     C                   eval      VATTRC = 'B'
041400060331     C                   write     FIVAT000
041500060331     C                   add       1             §CTROKVT
041600060331     C                   endif
041700060331     C*
041800060331     C                   if        wVATNOT_E <> *blanks
041900060331     C                   eval      VATTRC = 'E'
042000120629     C*
042100120629     C* Sviluppati i "CHI SONO" in relazione al numero colli
042200120703     C* siccome il barcode ha una parte alfanumerica (i primi 2 char), sviluppo la parte numerica
042300120703     C* poi ci attacco davanti quella alfa
042400120703     C*
042500120629     C                   FOR       wX = 0 by 1 to VABNCL - 1
042600120709     C                   EVAL      wBarcodeN= %dec(%trim(%subst(wVATNOT_E:3:11))
042700120709     C                              : 11 : 0) + wX
042800120709     C* recupero il check digit
042900120709     C                   clear                   TRUL28DSX
043000120709     C                   eval      I284STD = 'E10'
043100120709     C                   eval      I284COD =
043200120709     C                              %editc(WBarcodeN : 'X')
043300120709     C                   call      'TRUL28R4'
043400120709     C                   parm                    TRUL28DSX
043500120709     C* Se tutto ok scrivo il "chi sono" corrente
043600120709     C                   IF        O284err = *blanks
043700120703     C                   EVAL      VATNOT = %subst(wVATNOT_E:1:2) +
043800120703     C                              %editc(WBarcodeN : 'X')
043900120709     C                              + %trim(O284CkD)
044000120629     C                   write     FIVAT000
044100120629     C                   add       1             §CTROKVT
044200120709     C                   ENDIF
044300120709     C                   ENDFOR
044400120604     C*
044500120604     C                   endif
044600110927     C*
044700110927     C                   if        wVATNOT_I <> *blanks
044800110927     C                   eval      VATNOT = wVATNOT_I
044900110927     C                   eval      VATTRC = 'I'
045000110927     C                   write     FIVAT000
045100110927     C                   add       1             §CTROKVT
045200110927     C                   endif
045300040119     C*
045400040119     C                   ENDSR
045500040119
045600040119
045700040119
045800040119     C*----------------------------------------------------*
045900040119     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
046000040119     C*----------------------------------------------------*
046100040119     C     PREVAT        BEGSR
046200040119     C*
046300040119     C* Compongo il nome del membro da dare al FIVATWWR
046400040119     C                   eval      parmbr = vlrhdl
046500040119     C                   movel     'M'           parmbr
046600040510     C                   eval      parccm = vlrksc
046700040119     C                   eval      paropz = '1'
046800040119     C* Effettuo la chiamata al CLLE preposto
046900040119     C                   call(e)   'TITVVTC'
047000040119     C                   parm                    parccm
047100040119     C                   parm                    parmbr
047200040119     C                   parm                    paropz
047300040119     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
047400040119     C                   if        %error
047500040119     C                   movel     '1'           chkcall
047600040119     C                   else
047700040119     C                   movel     '0'           chkcall
047800040119     C                   endif
047900040119     C*
048000040119     C                   ENDSR
048100010601
048200010601
048300010601
048400010330     C*----------------------------------------------------*
048500030715     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
048600010330     C*----------------------------------------------------*
048700040119     C     INZVAR        BEGSR
048800080527     C*
048900080527     C                   setoff                                       65
049000010330     C*
049100030822     C* Inizializzo variabili di wrk
049200020204     C                   Z-ADD     *zeros        Num5_0            5 0
049300101018     C                   MOVEL     *blanks       wFlgCAS
049400060331     C                   MOVEL     *blanks       wVATNOT_A
049500060331     C                   MOVEL     *blanks       wVATNOT_B
049600060331     C                   MOVEL     *blanks       wVATNOT_E
049700110927     C                   MOVEL     *blanks       wVATNOT_I
049800120411     C                   MOVEL     *blanks       wNOT
049900120411     C                   MOVEL     *blanks       Data8A
050000030822     C*
050100030822     C* Inizializzo il buffer del record da scrivere e la schiera d wrk x i dati
050200030822     C                   CLEAR                   SkSplitCSV
050300030822     C                   EVAL      i = 1
050400030822     C                   EVAL      posDa = *zeros
050500030822     C                   EVAL      posA  = *zeros
050600080527     C*
050700080527     C                   CLEAR                   FIVABDS
050800080527     C                   CLEAR                   FIVAT000
050900030822     C*
051000030822     C* Reimposto i valori di default
051100030822     C                   EXSR      DEFCAM
051200030822     C*
051300010330     C                   ENDSR
051400010330     C*----------------------------------------------------*
051500030822     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
051600010330     C*----------------------------------------------------*
051700010330     C     DEFCAM        BEGSR
051800030822     C*
051900030715     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
052000030715     C* e delimitatore testo.
052100030715     C                   EVAL      CharCSV = %subst(vlrppt:2:1)
052200030715     C                   EVAL      CharTXT = %subst(vlrppt:3:1)
052300030715     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
052400100318     C*
052500100318     C* Determino il carattere sostituente il separatore decimale in caso d conflitto
052600100318     C                   EVAL      CharSOS = CharNUM
052700030822     C*
052800030715     C* Reperisco il flag che mi indica se effettuare o meno la stampa in filiale
052900100720     C                   SETOFF                                       505152
053000100720     C                   SETOFF                                       53
053100100413     C                   SELECT
053200100413     C                   WHEN      %subst(vlrppt:1:1) = 'S'
053300100720     C                   SETON                                        50
053400100413     C                   WHEN      %subst(vlrppt:1:1) = 'A'
053500100413     C                   SETON                                        51
053600100720     C                   WHEN      %subst(vlrppt:1:1) = *blanks
053700100720     C                   SETON                                        52
053800100720     C                   WHEN      %subst(vlrppt:1:1) = 'C'
053900100720     C                   SETON                                        53
054000100413     C                   ENDSL
054100030822     C*
054200030822     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
054300030822     C                   EVAL      posDaDft = 1
054400030822     C                   EVAL      posADft  = 0
054500030822     C                   EVAL      wGiroDft = 0
054600030822     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
054700030822     C                             posDaDft > 0
054800030822     C*
054900030822     C* Gestisco il 1° giro
055000030822     C                   IF        wGiroDft = 0
055100030822     C* Eseguo lo scan x trovare l'inizio del campo corrente
055200030822     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
055300030822     C* Incremento il contatore dei "giri"
055400030822     C                   EVAL      wGiroDft = 1
055500030822     C                   ELSE
055600030822     C                   EVAL      posDaDft = posADft
055700030822     C                   ENDIF
055800030822     C* Eseguo lo scan x trovare la fine del campo corrente
055900030822     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
056000030822     C*
056100030822     C* A questo "estraggo" il parametro (campo e valore) corrente...
056200030822     C* ...solo se entrambe le posizini (DA/A) sono > 0
056300030822     C                   IF        posDaDft > 0 AND
056400030822     C                             posADft  > 0
056500030822     C* NCL
056600030822     C                   IF        %subst(
056700030822     C                             %subst(vlrppt:posDaDft+1:
056800030822     C                             posADft-posDaDft-1):1:3)
056900030822     C                             = 'NCL'
057000030822     C                   EVAL      PiStr=%trim(%subst(
057100030822     C                             %subst(vlrppt:posDaDft+1:
057200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
057300030822     C                   EXSR      CHKNUM
057400030822     C                   IF        PiInt=*on
057500030822     C                   Z-ADD     PiVal         VABNCL
057600030822     C                   ENDIF
057700030822     C                   ENDIF
057800030822     C* CCM
057900030822     C                   IF        %subst(
058000030822     C                             %subst(vlrppt:posDaDft+1:
058100030822     C                             posADft-posDaDft-1):1:3)
058200030822     C                             = 'CCM'
058300030822     C                   EVAL      PiStr=%trim(%subst(
058400030822     C                             %subst(vlrppt:posDaDft+1:
058500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
058600030822     C                   EXSR      CHKNUM
058700030822     C                   IF        PiInt=*on
058800030822     C                   Z-ADD     PiVal         VABCCM
058900030822     C                   ENDIF
059000030822     C                   ENDIF
059100030822     C* LNP
059200030822     C                   IF        %subst(
059300030822     C                             %subst(vlrppt:posDaDft+1:
059400030822     C                             posADft-posDaDft-1):1:3)
059500030822     C                             = 'LNP'
059600030822     C                   EVAL      PiStr=%trim(%subst(
059700030822     C                             %subst(vlrppt:posDaDft+1:
059800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
059900030822     C                   EXSR      CHKNUM
060000030822     C                   IF        PiInt=*on
060100030822     C                   Z-ADD     PiVal         VABLNP
060200030822     C                   ENDIF
060300030822     C                   ENDIF
060400030822     C* NRS
060500030822     C                   IF        %subst(
060600030822     C                             %subst(vlrppt:posDaDft+1:
060700030822     C                             posADft-posDaDft-1):1:3)
060800030822     C                             = 'NRS'
060900030822     C                   EVAL      PiStr=%trim(%subst(
061000030822     C                             %subst(vlrppt:posDaDft+1:
061100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
061200030822     C                   EXSR      CHKNUM
061300030822     C                   IF        PiInt=*on
061400030822     C                   Z-ADD     PiVal         VABNRS
061500030822     C                   ENDIF
061600030822     C                   ENDIF
061700030822     C* CTR
061800030822     C                   IF        %subst(
061900030822     C                             %subst(vlrppt:posDaDft+1:
062000030822     C                             posADft-posDaDft-1):1:3)
062100030822     C                             = 'CTR'
062200030822     C                   EVAL      PiStr=%trim(%subst(
062300030822     C                             %subst(vlrppt:posDaDft+1:
062400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
062500030822     C                   EXSR      CHKNUM
062600030822     C                   IF        PiInt=*on
062700030822     C                   Z-ADD     PiVal         VABCTR
062800030822     C                   ENDIF
062900030822     C                   ENDIF
063000030822     C* PKB
063100030822     C                   IF        %subst(
063200030822     C                             %subst(vlrppt:posDaDft+1:
063300030822     C                             posADft-posDaDft-1):1:3)
063400030822     C                             = 'PKB'
063500030822     C                   EVAL      PiStr=%trim(%subst(
063600030822     C                             %subst(vlrppt:posDaDft+1:
063700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
063800030822     C                   EXSR      CHKNUM
063900030822     C                   IF        PiNum=*on
064000120418     C                   Z-ADD     PiVal         VABPKB
064100030822     C                   ENDIF
064200030822     C                   ENDIF
064300030822     C* VLB
064400030822     C                   IF        %subst(
064500030822     C                             %subst(vlrppt:posDaDft+1:
064600030822     C                             posADft-posDaDft-1):1:3)
064700030822     C                             = 'VLB'
064800030822     C                   EVAL      PiStr=%trim(%subst(
064900030822     C                             %subst(vlrppt:posDaDft+1:
065000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
065100030822     C                   EXSR      CHKNUM
065200030822     C                   IF        PiNum=*on
065300030822     C                   Z-ADD     PiVal         VABVLB
065400030822     C                   ENDIF
065500030822     C                   ENDIF
065600030822     C* QFT
065700030822     C                   IF        %subst(
065800030822     C                             %subst(vlrppt:posDaDft+1:
065900030822     C                             posADft-posDaDft-1):1:3)
066000030822     C                             = 'QFT'
066100030822     C                   EVAL      PiStr=%trim(%subst(
066200030822     C                             %subst(vlrppt:posDaDft+1:
066300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
066400030822     C                   EXSR      CHKNUM
066500030822     C                   IF        PiNum=*on
066600030822     C                   Z-ADD     PiVal         VABQFT
066700030822     C                   ENDIF
066800030822     C                   ENDIF
066900030822     C* CBO
067000030822     C                   IF        %subst(
067100030822     C                             %subst(vlrppt:posDaDft+1:
067200030822     C                             posADft-posDaDft-1):1:3)
067300030822     C                             = 'CBO'
067400030822     C                   EVAL      VABCBO=%trim(%subst(
067500030822     C                             %subst(vlrppt:posDaDft+1:
067600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
067700030822     C                   ENDIF
067800030822     C* TSP
067900030822     C                   IF        %subst(
068000030822     C                             %subst(vlrppt:posDaDft+1:
068100030822     C                             posADft-posDaDft-1):1:3)
068200030822     C                             = 'TSP'
068300030822     C                   EVAL      VABTSP=%trim(%subst(
068400030822     C                             %subst(vlrppt:posDaDft+1:
068500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
068600030822     C                   ENDIF
068700030822     C* VAS
068800030822     C                   IF        %subst(
068900030822     C                             %subst(vlrppt:posDaDft+1:
069000030822     C                             posADft-posDaDft-1):1:3)
069100030822     C                             = 'VAS'
069200030822     C                   EVAL      VABVAS=%trim(%subst(
069300030822     C                             %subst(vlrppt:posDaDft+1:
069400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
069500030822     C                   ENDIF
069600030822     C* VCA
069700030822     C                   IF        %subst(
069800030822     C                             %subst(vlrppt:posDaDft+1:
069900030822     C                             posADft-posDaDft-1):1:3)
070000030822     C                             = 'VCA'
070100030822     C                   EVAL      VABVCA=%trim(%subst(
070200030822     C                             %subst(vlrppt:posDaDft+1:
070300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
070400030822     C                   ENDIF
070500070925     C* TIC
070600070925     C                   IF        %subst(
070700070925     C                             %subst(vlrppt:posDaDft+1:
070800070925     C                             posADft-posDaDft-1):1:3)
070900070925     C                             = 'TIC'
071000070925     C                   EVAL      VABTIC=%trim(%subst(
071100070925     C                             %subst(vlrppt:posDaDft+1:
071200070925     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
071300070925     C                   ENDIF
071400030822     C* GCA
071500030822     C                   IF        %subst(
071600030822     C                             %subst(vlrppt:posDaDft+1:
071700030822     C                             posADft-posDaDft-1):1:3)
071800030822     C                             = 'GCA'
071900030822     C                   EVAL      VABGCA=%trim(%subst(
072000030822     C                             %subst(vlrppt:posDaDft+1:
072100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
072200030822     C                   ENDIF
072300030822     C* CTM
072400030822     C                   IF        %subst(
072500030822     C                             %subst(vlrppt:posDaDft+1:
072600030822     C                             posADft-posDaDft-1):1:3)
072700030822     C                             = 'CTM'
072800030822     C                   EVAL      VABCTM=%trim(%subst(
072900030822     C                             %subst(vlrppt:posDaDft+1:
073000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
073100030822     C                   ENDIF
073200030822     C* FFD
073300030822     C                   IF        %subst(
073400030822     C                             %subst(vlrppt:posDaDft+1:
073500030822     C                             posADft-posDaDft-1):1:3)
073600030822     C                             = 'FFD'
073700030822     C                   EVAL      VABFFD=%trim(%subst(
073800030822     C                             %subst(vlrppt:posDaDft+1:
073900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
074000030822     C                   ENDIF
074100030822     C* VAD
074200030822     C                   IF        %subst(
074300030822     C                             %subst(vlrppt:posDaDft+1:
074400030822     C                             posADft-posDaDft-1):1:3)
074500030822     C                             = 'VAD'
074600030822     C                   EVAL      VABVAD=%trim(%subst(
074700030822     C                             %subst(vlrppt:posDaDft+1:
074800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
074900030822     C                   ENDIF
075000030822     C* GMA
075100030822     C                   IF        %subst(
075200030822     C                             %subst(vlrppt:posDaDft+1:
075300030822     C                             posADft-posDaDft-1):1:3)
075400030822     C                             = 'GMA'
075500030822     C                   EVAL      VABGMA=%trim(%subst(
075600030822     C                             %subst(vlrppt:posDaDft+1:
075700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
075800030822     C                   ENDIF
075900030822     C* GGA
076000030822     C                   IF        %subst(
076100030822     C                             %subst(vlrppt:posDaDft+1:
076200030822     C                             posADft-posDaDft-1):1:3)
076300030822     C                             = 'GGA'
076400030822     C                   EVAL      VABGGA=%trim(%subst(
076500030822     C                             %subst(vlrppt:posDaDft+1:
076600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
076700030822     C                   ENDIF
076800030822     C* GVA
076900030822     C                   IF        %subst(
077000030822     C                             %subst(vlrppt:posDaDft+1:
077100030822     C                             posADft-posDaDft-1):1:3)
077200030822     C                             = 'GVA'
077300030822     C                   EVAL      VABGVA=%trim(%subst(
077400030822     C                             %subst(vlrppt:posDaDft+1:
077500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
077600030822     C                   ENDIF
077700030822     C* TC1
077800030822     C                   IF        %subst(
077900030822     C                             %subst(vlrppt:posDaDft+1:
078000030822     C                             posADft-posDaDft-1):1:3)
078100030822     C                             = 'TC1'
078200030822     C                   EVAL      VABTC1=%trim(%subst(
078300030822     C                             %subst(vlrppt:posDaDft+1:
078400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
078500030822     C                   ENDIF
078600030822     C* TC2
078700030822     C                   IF        %subst(
078800030822     C                             %subst(vlrppt:posDaDft+1:
078900030822     C                             posADft-posDaDft-1):1:3)
079000030822     C                             = 'TC2'
079100030822     C                   EVAL      VABTC2=%trim(%subst(
079200030822     C                             %subst(vlrppt:posDaDft+1:
079300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
079400030822     C                   ENDIF
079500040714     C* VATTRC
079600040714     C                   IF        %subst(
079700040714     C                             %subst(vlrppt:posDaDft+1:
079800040714     C                             posADft-posDaDft-1):1:3)
079900040714     C                             = 'TRC'
080000040714     C                   EVAL      VATTRC=%trim(%subst(
080100040714     C                             %subst(vlrppt:posDaDft+1:
080200040714     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
080300040714     C                   ENDIF
080400030822     C* ...
080500030822     C                   ENDIF
080600030822     C                   ENDDO
080700020204     C*
080800010330     C                   ENDSR
080900010607     C*----------------------------------------------------*
081000021025     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
081100010607     C*----------------------------------------------------*
081200010607     C     IMPVAB        BEGSR
081300010607     C*
081400010607     C                   SETOFF                                       3132
081500110315 xxx C***                SETON                                        35
081600030515     C*
081700010607     C                   MOVEL     datcor        VABAAS
081800010607     C                   MOVE      datcor        VABMGS
081900010607     C*
082000030715     C**********
082100030715     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
082200030715     C**********
082300030715     C                   IF        wGiro = *zeros
082400040113     C*
082500040510     C* Se presente inserisco qui la forzatura della intesatazione colonne personalizzata
082600040510     C                   IF        %trim(intcol(1))+%trim(intcol(2))+
082700080429     C                             %trim(intcol(3))+%trim(intcol(4))+
082800080429     C                             %trim(intcol(5))<>*blanks
082900040113     C                   EVAL      vindta = %trim(intcol(1))+%trim(intcol(2))+
083000080429     C                                      %trim(intcol(3))+%trim(intcol(4))+
083100080429     C                                      %trim(intcol(5))
083200070823     C     ';':CharCSV   XLATE     vindta        vindta
083300040510     C                   ENDIF
083400031201     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
083500031201     C     minu:maiu     XLATE     vindta        vindta
083600030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
083700030715     C                   DOW       posDa <= %len(%trim(vindta))
083800030715     C*
083900030715     C* Gestisco il 1° campo
084000030715     C                   IF        i = 1
084100030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
084200030715     C                   EVAL      posDa = 1
084300030715     C* Eseguo lo scan x trovare la fine del primo campo
084400030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
084500030715     C* A questo punto "estraggo" il campo corrente
084600030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
084700030715     C                                                    (posA-posDa))
084800030715     C* X i campi successivi al 1°
084900030715     C                   ELSE
085000030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
085100030715     C                   EVAL      posDa = posA + 1
085200030715     C* Eseguo lo scan x trovare la fine del campo corrente
085300030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
085400030715     C* Gestisco l'ultimo campo
085500030715     C                   IF        posA = *zeros
085600030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
085700030715     C* A questo punto "estraggo" il campo corrente
085800030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
085900030715     C                                                    (posA-posDa))
086000030715     C                   LEAVE
086100030715     C                   ELSE
086200030715     C* A questo punto "estraggo" il campo corrente
086300030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
086400030715     C                                                    (posA-posDa))
086500030715     C                   ENDIF
086600030715     C                   ENDIF
086700030715     C* Incremento il contatore d campo
086800030715     C                   EVAL      i = i +1
086900030715     C                   ENDDO
087000030715     C*
087100030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
087200030715     C* delimitazione testo
087300030715     C                   EVAL      i = 1
087400030715     C                   DOW       i < %elem(SkSplitFLD)
087500030715     C     CharTXT:' '   XLATE     SkSplitFLD(i) SkSplitFLD(i)
087600030715     C                   EVAL      SkSplitFLD(i) = %trim(SkSplitFLD(i))
087700030715     C                   EVAL      i = i + 1
087800030715     C                   ENDDO
087900030715     C                   EVAL      wGiro = 1
088000030715     C                   ELSE
088100030715     C                   EVAL      wGiro = 2
088200031204     C**********
088300031204     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
088400031204     C* d separatore campo
088500031204     C**********
088600031204     C                   EVAL      vindta = %trim(vindta)
088700031204     C                   DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
088800031204     C                   EVAL      vindta = %subst(vindta:2)
088900031204     C                   ENDDO
089000031204     C*
089100031204     C                   Z-ADD     *zeros        lunghInput        4 0
089200031204     C                   EVAL      lunghInput = %len(%trim(vindta))
089300031204     C                   DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
089400031204     C                   EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
089500031204     C                   EVAL      lunghInput = %len(%trim(vindta))
089600110912     C                   ENDDO
089700030715     C**********
089800030715     C* Effettuo lo split del campo dati d input
089900030715     C**********
090000030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
090100030715     C                   DOW       posDa <= %len(%trim(vindta))
090200030715     C*
090300030715     C* Gestisco il 1° campo
090400030715     C                   IF        i = 1
090500030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
090600030715     C                   EVAL      posDa = 1
090700030715     C* Eseguo lo scan x trovare la fine del primo campo
090800030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
090900030715     C* A questo punto "estraggo" il campo corrente
091000030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
091100030715     C                                                    (posA-posDa))
091200030715     C* X i campi successivi al 1°
091300030715     C                   ELSE
091400030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
091500030715     C                   EVAL      posDa = posA + 1
091600031204     C* Verifico che nn vi sia il campo nn valorizzato
091700031204     C                   IF        %subst(vindta:posDa:1) = CharCSV
091800031204     C* Se campo nn valorizzato skippo al prossimo
091900031204     C                   EVAL      SkSplitCSV(i) = *blanks
092000031204     C                   EVAL      posA  = posA + 1
092100031204     C                   ELSE
092200030715     C* Eseguo lo scan x trovare la fine del campo corrente
092300030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
092400030715     C* Gestisco l'ultimo campo
092500030715     C                   IF        posA = *zeros
092600030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
092700030715     C* A questo punto "estraggo" il campo corrente
092800030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
092900030715     C                                                    (posA-posDa))
093000030715     C                   LEAVE
093100030715     C                   ELSE
093200030715     C* A questo punto "estraggo" il campo corrente
093300030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
093400030715     C                                                    (posA-posDa))
093500030715     C                   ENDIF
093600031204     C                   ENDIF
093700030715     C                   ENDIF
093800030715     C* Incremento il contatore d campo
093900030715     C                   EVAL      i = i +1
094000030715     C                   ENDDO
094100030715     C*
094200030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
094300030715     C* delimitazione testo
094400030715     C                   EVAL      i = 1
094500030715     C                   DOW       i < %elem(SkSplitCSV)
094600030715     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
094700030715     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
094800030715     C                   EVAL      i = i + 1
094900030715     C                   ENDDO
095000100720     C*
095100120629     C* Stacco un progressivo x numero spedizione
095200120703 xxx C                   EXSR      repNSP
095300100326     C*
095400030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
095500030715     C                   EVAL      i = 1
095600030715     C                   DOW       i < %elem(SkSplitFLD)
095700030715     C*
095800030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
095900030715     C                   IF        SkSplitFLD(i) <> *blanks AND
096000030715     C                             SkSplitCSV(i) <> *blanks
096100030721     C*** ATB
096200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABATB'
096300030721     C                   EVAL      VABATB = SkSplitCSV(i)
096400030721     C                   ENDIF
096500030715     C*** RSD
096600030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRSD'
096700030715     C                   EVAL      VABRSD = SkSplitCSV(i)
096800030715     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
096900030715     C     '@':'A'       XLATE     VABRSD        VABRSD
097000030715     C* ==
097100030715     C                   ENDIF
097200030721     C*** RD2
097300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRD2'
097400030721     C                   EVAL      VABRD2 = SkSplitCSV(i)
097500030721     C                   ENDIF
097600030715     C*** IND
097700030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABIND'
097800030715     C                   EVAL      VABIND = SkSplitCSV(i)
097900030715     C                   ENDIF
098000030715     C*** LOD
098100030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABLOD'
098200030715     C                   EVAL      VABLOD = SkSplitCSV(i)
098300030715     C                   ENDIF
098400030715     C*** PRD
098500030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABPRD'
098600030715     C                   EVAL      VABPRD = SkSplitCSV(i)
098700030715     C                   ENDIF
098800030721     C*** NZD
098900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNZD'
099000030721     C                   EVAL      VABNZD = SkSplitCSV(i)
099100030721     C                   ENDIF
099200030715     C*** RMA
099300030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMA'
099400030715     C                   EVAL      VABRMA = SkSplitCSV(i)
099500030715     C                   ENDIF
099600030715     C*** NOT
099700030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNOT'
099800030715     C                   EVAL      VABNOT = SkSplitCSV(i)
099900030715     C                   ENDIF
100000030715     C*** NT2
100100030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNT2'
100200030715     C                   EVAL      VABNT2 = SkSplitCSV(i)
100300030715     C                   ENDIF
100400030715     C*** VCA
100500030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVCA'
100600030715     C                   EVAL      VABVCA = SkSplitCSV(i)
100700030715     C                   ENDIF
100800030715     C*** VAS
100900030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAS'
101000030715     C                   EVAL      VABVAS = SkSplitCSV(i)
101100030715     C                   ENDIF
101200030715     C*** FFD
101300030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABFFD'
101400030715     C                   EVAL      VABFFD = SkSplitCSV(i)
101500030715     C                   ENDIF
101600030721     C*** GC1
101700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC1'
101800030721     C                   EVAL      VABGC1 = SkSplitCSV(i)
101900030721     C                   ENDIF
102000030721     C*** GC2
102100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC2'
102200030721     C                   EVAL      VABGC2 = SkSplitCSV(i)
102300030721     C                   ENDIF
102400030721     C*** TSP
102500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTSP'
102600030721     C                   EVAL      VABTSP = SkSplitCSV(i)
102700030721     C                   ENDIF
102800040510     C*** CBO
102900040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABCBO'
103000040510     C                   EVAL      VABCBO = SkSplitCSV(i)
103100040510     C                   ENDIF
103200030721     C*** NAS
103300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNAS'
103400030721     C                   EVAL      VABNAS = SkSplitCSV(i)
103500030721     C                   ENDIF
103600030721     C*** TIC
103700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTIC'
103800030721     C                   EVAL      VABTIC = SkSplitCSV(i)
103900030721     C                   ENDIF
104000030721     C*** GCA
104100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGCA'
104200030721     C                   EVAL      VABGCA = SkSplitCSV(i)
104300030721     C                   ENDIF
104400030721     C*** XCO
104500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABXCO'
104600030721     C                   EVAL      VABXCO = SkSplitCSV(i)
104700030721     C                   ENDIF
104800030721     C*** CTM
104900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTM'
105000030721     C                   EVAL      VABCTM = SkSplitCSV(i)
105100030721     C                   ENDIF
105200030721     C*** TCR
105300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTCR'
105400030721     C                   EVAL      VABTCR = SkSplitCSV(i)
105500030721     C                   ENDIF
105600030721     C*** CTS
105700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTS'
105800030721     C                   EVAL      VABCTS = SkSplitCSV(i)
105900030721     C                   ENDIF
106000030721     C*** FTM
106100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABFTM'
106200030721     C                   EVAL      VABFTM = SkSplitCSV(i)
106300030721     C                   ENDIF
106400030721     C*** VAD
106500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAD'
106600030721     C                   EVAL      VABVAD = SkSplitCSV(i)
106700030721     C                   ENDIF
106800030721     C*** GMA
106900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGMA'
107000030721     C                   EVAL      VABGMA = SkSplitCSV(i)
107100030721     C                   ENDIF
107200030721     C*** GGA
107300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGGA'
107400030721     C                   EVAL      VABGGA = SkSplitCSV(i)
107500030721     C                   ENDIF
107600030721     C*** GVA
107700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGVA'
107800030721     C                   EVAL      VABGVA = SkSplitCSV(i)
107900030721     C                   ENDIF
108000030721     C*** TC1
108100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC1'
108200030721     C                   EVAL      VABTC1 = SkSplitCSV(i)
108300030721     C                   ENDIF
108400030721     C*** TC2
108500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC2'
108600030721     C                   EVAL      VABTC2 = SkSplitCSV(i)
108700030721     C                   ENDIF
108800030721     C*** SCL
108900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABSCL'
109000030721     C                   EVAL      VABSCL = SkSplitCSV(i)
109100030721     C                   ENDIF
109200030721     C*** RMO
109300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMO'
109400030721     C                   EVAL      VABRMO = SkSplitCSV(i)
109500030721     C                   ENDIF
109600030721     C*** NMO
109700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNMO'
109800030721     C                   EVAL      VABNMO = SkSplitCSV(i)
109900030721     C                   ENDIF
110000030721     C*
110100030721     C* Reperisco quindi i campi numerici...
110200040510     C*** NSP
110300040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABNSP'
110400040510     C                   EVAL      PiStr=SkSplitCSV(i)
110500040510     C                   EXSR      CHKNUM
110600040510     C                   IF        PiInt=*on
110700040510     C                   Z-ADD     PiVal         VABNSP
110800040510     C                   ELSE
110900040510     C                   SETON                                        31
111000040510     C                   EVAL      VABNSP = *zeros
111100040510     C                   EVAL      vinmsg = %trimr(vinmsg)
111200040510     C                             + ' ' + 'VABNSP'
111300040510     C                   ENDIF
111400040510     C                   ENDIF
111500071106     C*** RMN
111600071106     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMN'
111700071106     C                   EVAL      PiStr=SkSplitCSV(i)
111800071106     C                   EXSR      CHKNUM
111900071106     C                   IF        PiInt=*on
112000071106     C                   Z-ADD     PiVal         VABRMN
112100071106     C                   ELSE
112200071106     C                   SETON                                        32
112300071106     C                   EVAL      VABRMN = *zeros
112400071106     C                   EVAL      vinmsg = %trimr(vinmsg)
112500071106     C                             + ' ' + 'VABRMN'
112600071106     C                   ENDIF
112700071106     C                   ENDIF
112800030721     C*** CAD
112900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAD'
113000030721     C                   EVAL      PiStr=SkSplitCSV(i)
113100030721     C                   EXSR      CHKNUM
113200030721     C                   IF        PiInt=*on
113300030721     C                   Z-ADD     PiVal         Num5_0
113400030721     C                   MOVEL(p)  Num5_0        VABCAD
113500030721     C                   ELSE
113600030721     C                   SETON                                        32
113700030721     C                   EVAL      VABCAD = *zeros
113800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
113900030721     C                             + ' ' + 'VABCAD'
114000030721     C                   ENDIF
114100030721     C                   ENDIF
114200030721     C*** NCL
114300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCL'
114400030721     C                   EVAL      PiStr=SkSplitCSV(i)
114500030721     C                   EXSR      CHKNUM
114600030721     C                   IF        PiInt=*on
114700030721     C                   Z-ADD     PiVal         VABNCL
114800030721     C                   ELSE
114900030721     C                   SETON                                        32
115000030721     C                   EVAL      VABNCL = *zeros
115100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
115200040122     C                             + ' ' + 'VABNCL'
115300030721     C                   ENDIF
115400030721     C                   ENDIF
115500030721     C*** PKB
115600120418     C* il cliente passa fino ai decigrammi ma noi gestiamo solo gli etti
115700120418     C* per cui si arrotonda il valore passato agli etti
115800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABPKB'
115900030721     C                   EVAL      PiStr=SkSplitCSV(i)
116000030721     C                   EXSR      CHKNUM
116100030721     C                   IF        PiNum=*on
116200120418     C                   Z-ADD(H)  PiVal         VABPKB
116300030721     C                   ELSE
116400030721     C                   SETON                                        32
116500030721     C                   EVAL      VABPKB = *zeros
116600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
116700030721     C                             + ' ' + 'VABPKB'
116800030721     C                   ENDIF
116900030721     C                   ENDIF
117000030721     C*** CAS
117100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAS'
117200110506     C                   IF        %trim(SkSplitCSV(i)) <> *blanks AND
117300111116     C                             %trim(SkSplitCSV(i)) <> *zeros  AND
117400111116     C                             %trim(SkSplitCSV(i)) <> '0.00'
117500120605     C                   EVAL      VABVCA = 'EUR'
117600101018     C                   EVAL      wFlgCAS = '1'
117700101018     C                   ENDIF
117800030721     C                   EVAL      PiStr=SkSplitCSV(i)
117900030721     C                   EXSR      CHKNUM
118000030721     C                   IF        PiNum=*on
118100030721     C                   Z-ADD     PiVal         VABCAS
118200030721     C                   ELSE
118300030721     C                   SETON                                        32
118400030721     C                   EVAL      VABCAS = *zeros
118500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
118600030721     C                             + ' ' + 'VABCAS'
118700030721     C                   ENDIF
118800030721     C                   ENDIF
118900030721     C*** CCM
119000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCCM'
119100030721     C                   EVAL      PiStr=SkSplitCSV(i)
119200030721     C                   EXSR      CHKNUM
119300030721     C                   IF        PiInt=*on      AND
119400030721     C                             PiVal<=9999999 AND
119500030721     C                             PiVal>*zeros
119600030721     C                   Z-ADD     PiVal         VABCCM
119700030721     C                   ELSE
119800030721     C                   SETON                                        32
119900030721     C                   EVAL      VABCCM = *zeros
120000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
120100040510     C                             + ' ' + 'VABCCM'
120200030721     C                   ENDIF
120300030721     C                   ENDIF
120400030721     C*** LNP
120500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNP'
120600030721     C                   EVAL      PiStr=SkSplitCSV(i)
120700030721     C                   EXSR      CHKNUM
120800030721     C                   IF        PiInt=*on    AND
120900030721     C                             PiVal<=999   AND
121000030721     C                             PiVal>*zeros
121100030721     C                   Z-ADD     PiVal         VABLNP
121200030721     C                   ELSE
121300040715     C                   SETON                                        32
121400030721     C                   EVAL      VABLNP = *zeros
121500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
121600040510     C                             + ' ' + 'VABLNP'
121700030721     C                   ENDIF
121800030721     C                   ENDIF
121900030721     C*** NRS
122000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNRS'
122100030721     C                   EVAL      PiStr=SkSplitCSV(i)
122200030721     C                   EXSR      CHKNUM
122300030721     C                   IF        PiInt=*on AND
122400030721     C                             PiVal<=99
122500030721     C                   Z-ADD     PiVal         VABNRS
122600030721     C                   ELSE
122700030721     C                   SETON                                        32
122800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
122900030721     C                             + ' ' + 'VABNRS'
123000030721     C                   ENDIF
123100030721     C                   ENDIF
123200030721     C*** LNA
123300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNA'
123400030721     C                   EVAL      PiStr=SkSplitCSV(i)
123500030721     C                   EXSR      CHKNUM
123600030721     C                   IF        PiInt=*on    AND
123700030721     C                             PiVal<=999   AND
123800030721     C                             PiVal>*zeros
123900030721     C                   Z-ADD     PiVal         VABLNA
124000030721     C                   ELSE
124100030721     C                   SETON                                        32
124200030721     C                   EVAL      VABLNA = *zeros
124300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
124400030721     C                             + ' ' + 'VABLNA'
124500030721     C                   ENDIF
124600030721     C                   ENDIF
124700030721     C*** CTR
124800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTR'
124900030721     C                   EVAL      PiStr=SkSplitCSV(i)
125000030721     C                   EXSR      CHKNUM
125100030721     C                   IF        PiInt=*on  AND
125200030721     C                             PiVal<=999
125300030721     C                   Z-ADD     PiVal         VABCTR
125400030721     C                   ELSE
125500030721     C                   SETON                                        32
125600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
125700030721     C                             + ' ' + 'VABCTR'
125800030721     C                   ENDIF
125900030721     C                   ENDIF
126000030721     C*** IAS
126100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABIAS'
126200030721     C                   EVAL      PiStr=SkSplitCSV(i)
126300030721     C                   EXSR      CHKNUM
126400030721     C                   IF        PiNum=*on
126500030721     C                   Z-ADD     PiVal         VABIAS
126600030721     C                   ELSE
126700030721     C                   SETON                                        32
126800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
126900030721     C                             + ' ' + 'VABIAS'
127000030721     C                   ENDIF
127100030721     C                   ENDIF
127200030721     C*** VLB
127300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVLB'
127400030721     C                   EVAL      PiStr=SkSplitCSV(i)
127500030721     C                   EXSR      CHKNUM
127600030721     C                   IF        PiNum=*on
127700030721     C                   Z-ADD     PiVal         VABVLB
127800030721     C                   ELSE
127900030721     C                   SETON                                        32
128000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
128100030721     C                             + ' ' + 'VABVLB'
128200030721     C                   ENDIF
128300030721     C                   ENDIF
128400030721     C*** QFT
128500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABQFT'
128600030721     C                   EVAL      PiStr=SkSplitCSV(i)
128700030721     C                   EXSR      CHKNUM
128800030721     C                   IF        PiNum=*on
128900030721     C                   Z-ADD     PiVal         VABQFT
129000030721     C                   ELSE
129100030721     C                   SETON                                        32
129200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
129300030721     C                             + ' ' + 'VABQFT'
129400030721     C                   ENDIF
129500030721     C                   ENDIF
129600030721     C*** NCD
129700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCD'
129800030721     C                   EVAL      PiStr=SkSplitCSV(i)
129900030721     C                   EXSR      CHKNUM
130000030721     C                   IF        PiInt=*on      AND
130100030721     C                             PiVal<=9999999
130200030721     C                   Z-ADD     PiVal         VABNCD
130300030721     C                   ELSE
130400030721     C                   SETON                                        32
130500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
130600030721     C                             + ' ' + 'VABNCD'
130700030721     C                   ENDIF
130800030721     C                   ENDIF
130900030721     C*** NCA
131000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCA'
131100030721     C                   EVAL      PiStr=SkSplitCSV(i)
131200030721     C                   EXSR      CHKNUM
131300030721     C                   IF        PiInt=*on      AND
131400030721     C                             PiVal<=9999999
131500030721     C                   Z-ADD     PiVal         VABNCA
131600030721     C                   ELSE
131700030721     C                   SETON                                        32
131800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
131900030721     C                             + ' ' + 'VABNCA'
132000030721     C                   ENDIF
132100030721     C                   ENDIF
132200030721     C*** ZNC
132300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABZNC'
132400030721     C                   EVAL      PiStr=SkSplitCSV(i)
132500030721     C                   EXSR      CHKNUM
132600030721     C                   IF        PiInt=*on AND
132700030721     C                             PiVal<=99
132800030721     C                   Z-ADD     PiVal         VABZNC
132900030721     C                   ELSE
133000030721     C                   SETON                                        32
133100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
133200030721     C                             + ' ' + 'VABZNC'
133300030721     C                   ENDIF
133400030721     C                   ENDIF
133500030721     C*** DCR
133600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABDCR'
133700030721     C                   EVAL      PiStr=SkSplitCSV(i)
133800030721     C                   EXSR      CHKNUM
133900030721     C                   IF        PiInt=*on       AND
134000030721     C                             PiVal<=99999999
134100030721     C                   Z-ADD     PiVal         VABDCR
134200030721     C                   ELSE
134300030721     C                   SETON                                        32
134400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
134500030721     C                             + ' ' + 'VABDCR'
134600030721     C                   ENDIF
134700030721     C                   ENDIF
134800030721     C*** HCR
134900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABHCR'
135000030721     C                   EVAL      PiStr=SkSplitCSV(i)
135100030721     C                   EXSR      CHKNUM
135200030721     C                   IF        PiInt=*on   AND
135300030721     C                             PiVal<=9999
135400030721     C                   Z-ADD     PiVal         VABHCR
135500030721     C                   ELSE
135600030721     C                   SETON                                        32
135700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
135800030721     C                             + ' ' + 'VABHCR'
135900030721     C                   ENDIF
136000030721     C                   ENDIF
136100030721     C*** VMD
136200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVMD'
136300030721     C                   EVAL      PiStr=SkSplitCSV(i)
136400030721     C                   EXSR      CHKNUM
136500030721     C                   IF        PiNum=*on
136600030721     C                   Z-ADD     PiVal         VABVMD
136700030721     C                   ELSE
136800030721     C                   SETON                                        32
136900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
137000030721     C                             + ' ' + 'VABVMD'
137100030721     C                   ENDIF
137200030721     C                   ENDIF
137300030721     C*** ANT
137400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABANT'
137500030721     C                   EVAL      PiStr=SkSplitCSV(i)
137600030721     C                   EXSR      CHKNUM
137700030721     C                   IF        PiInt=*on        AND
137800030721     C                             PiVal<=999999999
137900030721     C                   Z-ADD     PiVal         VABANT
138000030721     C                   ELSE
138100030721     C                   SETON                                        32
138200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
138300030721     C                             + ' ' + 'VABANT'
138400030721     C                   ENDIF
138500030721     C                   ENDIF
138600030721     C*** CMO
138700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCMO'
138800030721     C                   EVAL      PiStr=SkSplitCSV(i)
138900030721     C                   EXSR      CHKNUM
139000030721     C                   IF        PiInt=*on
139100030721     C                   Z-ADD     PiVal         Num5_0
139200030721     C                   MOVEL(p)  Num5_0        VABCMO
139300030721     C                   ELSE
139400030721     C                   SETON                                        32
139500030721     C                   EVAL      VABCMO = *zeros
139600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
139700030721     C                             + ' ' + 'VABCMO'
139800030721     C                   ENDIF
139900030721     C                   ENDIF
140000040510     C*
140100040510     C* Reperisco i campi del FIVAT
140200040510     C*** VATNOT
140300040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT'
140400040510     C                   EVAL      VATNOT = %trim(SkSplitCSV(i))
140500040510     C                   ENDIF
140600040510     C*** VATTRC
140700040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATTRC'
140800040510     C                   EVAL      VATTRC = %trim(SkSplitCSV(i))
140900040510     C                   ENDIF
141000060331     C*** VATNOT_A
141100060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_A'
141200060331     C                   EVAL      wVATNOT_A = %trim(SkSplitCSV(i))
141300060331     C                   ENDIF
141400060331     C*** VATNOT_B
141500060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_B'
141600060331     C                   EVAL      wVATNOT_B = %trim(SkSplitCSV(i))
141700060331     C                   ENDIF
141800060331     C*** VATNOT_E
141900070823     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_E'
142000060331     C                   EVAL      wVATNOT_E = %trim(SkSplitCSV(i))
142100060331     C                   ENDIF
142200110927     C*** VATNOT_I
142300110927     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_I'
142400110927     C                   EVAL      wVATNOT_I = %trim(SkSplitCSV(i))
142500110927     C                   ENDIF
142600070823     C*
142700100831     C***  ===>  Gestione campi "particolari" dentro ciclo
142800120703
142900120703     C*** VABRMN
143000120703     C* il rif.num. è oggi in giuliano senza l'anno
143100120703     C                   EVAL      VABRMN = %dec(%subst(
143200120703     C                             UBFMTDATE_Convert
143300120703     C                             (%char(%date()):'YYYY-MM-DD':'YYDDD')
143400120703     C                             : 3 : 3) : 3 : 0)
143500120301
143600070925     C*
143700071029     C***  <===  -----------------------------------------
143800040510     C*
143900030715     C                   ENDIF
144000030715     C                   EVAL      i = i + 1
144100030715     C                   ENDDO
144200070925     C*
144300071029     C***  ===>  Gestione campi "particolari" fuori ciclo
144400120411     C*
144500120604     C* non ci sono
144600100831     C*
144700070925     C*
144800071029     C***  <===  -----------------------------------------
144900070925     C*
145000070925     C* Considerazioni finali su CBO/CAS
145100100318     C                   IF        wFlgCAS = '1'    OR
145200100318     C                             VABCAS  > *zeros
145300070925     C                   IF        VABCBO = '1'
145400070925     C                   EVAL      VABCBO = '4'
145500070925     C                   ENDIF
145600070925     C                   IF        VABCBO = '2'
145700070925     C                   EVAL      VABCBO = '6'
145800070925     C                   ENDIF
145900070925     C                   ENDIF
146000070925     C***  <===  ----------------------------
146100040510     C*
146200040715     C* Se NSP nn valorizzato considero errore bloccante in traduzione
146300040510     C                   IF        VABNSP = *zeros
146400040510     C                   SETON                                        31
146500040510     C                   ENDIF
146600040715     C*
146700040715     C* Se LNP nn valorizzato considero errore bloccante in traduzione
146800040715     C                   IF        VABLNP = *zeros
146900040715     C                   SETON                                        31
147000040715     C                   ENDIF
147100040510     C*
147200040714     C* Se RMN nn valorizzato lo forzo uguale a VABNSP
147300120411     C                   IF        VABRMN = *zeros
147400120411     C                   Z-ADD     VABNSP        VABRMN
147500120411     C                   ENDIF
147600120328     C* IN QUESTO CASO LO FORZO = 1
147700120411     C***                IF        VABRMN = *zeros
147800120411     C***                Z-ADD     1             VABRMN
147900120411     C***                ENDIF
148000040510     C*
148100040714     C* Se RMA nn valorizzato lo forzo uguale a VABRMN
148200040510     C                   IF        VABRMA = *blanks
148300040714     C                   MOVEL     VABRMN        VABRMA
148400040510     C                   ENDIF
148500040128     C*
148600040510     C* Se provincia nn valorizzata la reperisco
148700040510     C* tramite TISI95R a seconda dei dati d instradamento presenti
148800110912     C                   IF        VABPRD =  *blanks AND
148900110912     C                             VABNZD =  *blanks AND
149000110912     C                             VABCAD <> *blanks AND
149100110912     C                             VABLOD <> *blanks
149200040128     C                   CLEAR                   TISI95DS
149300040128     C                   EVAL      I95TCN = '3'
149400040128     C                   Z-ADD     datcor        I95DAT
149500040128     C                   EVAL      I95NAR = VABNZD
149600040128     C                   EVAL      I95CAP = VABCAD
149700040128     C                   EVAL      I95LOC = VABLOD
149800040128     C                   CALL      'TISI95R'
149900040128     C                   PARM                    TISI95DS
150000040128     C                   EVAL      VABPRD = O95PRV
150100040510     C                   ENDIF
150200020204     C*
150300020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
150400020204     C                   EXSR      CHKIMPDIV
150500030715     C                   ENDIF
150600020204     C*
150700010607     C                   ENDSR
150800010607     C*----------------------------------------------------*
150900010601
151000020204
151100020204     C*----------------------------------------------------*
151200020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
151300020204     C*----------------------------------------------------*
151400020204     C     CHKIMPDIV     BEGSR
151500020204     C*
151600020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
151700020204     C                   Z-ADD     *zeros        wrkDec            9 9
151800020204     C*
151900020204     C* Come prima cosa effettuo considerazioni sulla divisa
152000020204     C                   IF        vabIAS > *zeros
152100020204     C                   IF        vabVAS <> 'EUR'
152200020204     C                   EVAL      vabVAS =  'ITL'
152300020204     C                   ENDIF
152400020204     C                   ENDIF
152500020204     C*
152600020204     C                   IF        vabCAS > *zeros
152700020204     C                   IF        vabVCA <> 'EUR'
152800020204     C                   EVAL      vabVCA =  'ITL'
152900020204     C                   ENDIF
153000020204     C                   ENDIF
153100020204     C*
153200020204     C                   IF        vabVMD > *zeros
153300020204     C                   IF        vabVAD <> 'EUR'
153400020204     C                   EVAL      vabVAD =  'ITL'
153500020204     C                   ENDIF
153600020204     C                   ENDIF
153700020204     C*
153800020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
153900020204     C                   Z-ADD     vabIAS        wrkDec
154000020204     C                   IF        wrkDec > *zeros
154100020204     C                   IF        vabVAS = 'ITL'
154200020204     C                   EVAL      vabIAS = *zeros
154300020204     C                   ENDIF
154400020204     C                   ENDIF
154500020204     C*
154600020204     C* Stabilisco se il contrasegno ha decimali valorizzati
154700020204     C                   Z-ADD     vabCAS        wrkDec
154800020204     C                   IF        wrkDec > *zeros
154900020204     C                   IF        vabVCA = 'ITL'
155000020204     C                   EVAL      vabCAS = *zeros
155100020204     C                   ENDIF
155200020204     C                   ENDIF
155300020204     C*
155400020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
155500020204     C                   Z-ADD     vabVMD        wrkDec
155600020204     C                   IF        wrkDec > *zeros
155700020204     C                   IF        vabVAD = 'ITL'
155800020204     C                   EVAL      vabVMD = *zeros
155900020204     C                   ENDIF
156000020204     C                   ENDIF
156100020204     C*
156200020204     C                   ENDSR
156300020204     C***
156400010330
156500100318
156600100318
156700100318     C*----------------------------------------------------*
156800100318     C*  CONTROLLO NUMERICITA' CAMPI
156900100318     C*----------------------------------------------------*
157000100318     C     CHKNUM        BEGSR
157100100318     C*
157200100318     C                   IF        PiDecChr = *blanks
157300100318     C                   EVAL      PiDecChr = CharNUM
157400100318     C                   ENDIF
157500100318     C*
157600100318     C                   callp     UBISNUM_Check(PiStr
157700100318     C                                          :PiDecChr
157800100318     C                                          :PiVal
157900100318     C                                          :PiNum
158000100318     C                                          :PiInt)
158100100318     C*
158200100318     C                   ENDSR
158300100318     C***
158400010601
158500010601
158600010601
158700010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
158800010601     C     repfil        BEGSR
158900010601     C*
159000010601     C                   if        invfil = *zeros and
159100010601     C                             depfil > *zeros and
159200010629     C                             (vinflg = *blanks or
159300010629     C                              vinflg = *zeros)
159400010601     C
159500010601     C                   eval      invfil = depfil
159600010601     C                   endif
159700010601     C*
159800010601     C                   if        depfil <> invfil and
159900010601     C                             invfil > *zeros
160000010601     C                   eval      flgMulti = '1'
160100010601     C                   if        vinflg = *blanks
160200010601     C                   add       1             cntNonEl
160300010601     C                   endif
160400010601     C                   endif
160500010601     C*
160600010601     C                   if        vinflg = '2'
160700010601     C                   eval      flgStato = '2'
160800010601     C                   endif
160900010601     C*
161000010601     C                   ENDSR
161100010601     C***
161200010601
161300010601
161400010601
161500010330
161600040119      /TITLE Invio dei dati al punto operativo.
161700040119     C     invio         BEGSR
161800040119     C*
161900040119     C* 1° invio FIVAT
162000040119     C                   reset                   dscmz
162100040119     C                   move      vlrpoi        cmzdst
162200040119     C                   eval      cmzfld = 'FIVATWWR'
162300040119     C                   eval      cmzmbd = vlrhdl
162400040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
162500040119     C***                if        prmfir = *blanks
162600040119     C                   eval      cmzfla = 'FIVAT00F'
162700040119     C                   eval      cmzmba = 'FIVAT00F'
162800040119     C***                else
162900040119     C***                eval      cmzfla = prmfir
163000040119     C***                eval      cmzmba = prmfir
163100040119     C***                endif
163200040119     C                   eval      cmznrr = *zeros
163300040119     C                   move      §ctrokvt      cmznrr
163400040119     C                   eval      cmzlba = vlrfl1
163500040119     C                   call(e)   'TIS711C'
163600040119     C                   parm                    dscmz
163700040119     C                   parm      *blanks       esito
163800040119     C                   if        %error
163900040119     C                             or cmzerr = '1'
164000040119     C                             or esito  = '1'
164100040119     C                   eval      wrkesito = '3'
164200040119     C                   else
164300040119     C*
164400040119     C* 2° invio FIVAB
164500040119     C                   reset                   dscmz
164600040119     C                   move      vlrpoi        cmzdst
164700040119     C                   eval      cmzfld = vlrfou
164800040119     C                   eval      cmzmbd = vlrhdl
164900040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
165000040119     C***                if        prmfir = *blanks
165100040119     C                   eval      cmzfla = 'FIVAB00F'
165200040119     C                   eval      cmzmba = 'FIVAB00F'
165300040119     C***                else
165400040119     C***                eval      cmzfla = prmfir
165500040119     C***                eval      cmzmba = prmfir
165600040119     C***                endif
165700040119     C                   eval      cmznrr = *zeros
165800040119     C                   move      §ctrokvb      cmznrr
165900040119     C                   eval      cmzlba = vlrfl1
166000040119     C                   call(e)   'TIS711C'
166100040119     C                   parm                    dscmz
166200040119     C                   parm      *blanks       esito
166300040119     C                   if        %error
166400040119     C                             or cmzerr = '1'
166500040119     C                             or esito  = '1'
166600040119     C                   eval      wrkesito = '3'
166700040119     C                   endif
166800040119     C                   endif
166900040119     C*
167000040119     C                   ENDSR
167100040119     C***
167200010601
167300010601
167400010601
167500010601
167600010601      /TITLE Invio dei dati al punto operativo.
167700010601     C     opeini        BEGSR
167800010601     C*
167900010601     C* Inizializzo flag e contatori operativi
168000010601     C                   movel     '0'           flgGiro           1
168100010601     C                   movel     '0'           flgMulti          1
168200010601     C                   movel     '1'           flgStato          1
168300010615     C                   movel     '0'           flgOk             1
168400010601     C                   z-add     *zeros        cntNonEl         10 0
168500010601     C                   z-add     *zeros        depfil            3 0
168600010601     C                   z-add     *zeros        invfil            3 0
168700010601     C*
168800010601     C                   ENDSR
168900010601     C***
169000070326
169100070326
169200070326
169300070326
169400070326     C     *pssr         BEGSR
169500070329     C*
169600070329     C                   if        %open(tivin00r)
169700070329     C                   close     tivin00r
169800070329     C                   endif
169900070329     C                   if        %open(fivabwwr)
170000070329     C                   close     fivabwwr
170100070329     C                   endif
170200070329     C                   if        %open(fivatwwr)
170300070329     C                   close     fivatwwr
170400070329     C                   endif
170500070326     C*
170600070326     C* Effettuo la chiamata al CLLE preposto
170700070326     C                   call(e)   'TITVVTC'
170800070326     C                   parm                    parccm
170900070326     C                   parm                    parmbr
171000070326     C                   parm      '2'           paropz
171100070326     C*
171200070326     C                   eval      wrkesito = '2'
171300070404     C*
171400070404     C                   seton                                        LR
171500070326     C*
171600070326     C                   ENDSR     '*CANCL'
171700070326     C***
171800100318
171900100318
172000100318
172100100318     C     normCSV       BEGSR
172200100318     C*
172300100318     C* Se previsti caratteri delimitatori testo provvedo a verificare i caratteri
172400100318     C* separatori campi
172500100318     C                   if        CharTXT <> *blanks
172600100318     C                   z-add     *zeros        wLenVINDTA        4 0
172700100318     C                   z-add     *zeros        wFlgVINDTA        1 0
172800100318     C                   setoff                                       80
172900100318     C                   dow       wLenVINDTA < %len(%trim(vindta))
173000100318     C                   eval      wLenVINDTA = wLenVINDTA + 1
173100100318     C                   if        %subst(vindta:wLenVINDTA:1)=CharTXT
173200100318     C                   if        not *in80
173300100318     C                   seton                                        80
173400100318     C                   else
173500100318     C                   setoff                                       80
173600100318     C                   endif
173700100318     C                   endif
173800100318     C                   if        %subst(vindta:wLenVINDTA:1)=CharCSV
173900100318     C                             and *in80 = *on
174000100318     C                   eval      %subst(vindta:wLenVINDTA:1)=CharSOS
174100100318     C                   endif
174200100318     C                   enddo
174300100318     C                   endif
174400100318     C*
174500100318     C                   ENDSR
174600100318     C***
174700070326
174800100326
174900100326
175000100326     C*----------------------------------------------------*
175100100326     C*  STACCO NUMERATORE X NUMERO SPEDIZIONE
175200100326     C*----------------------------------------------------*
175300100326     C     REPNSP        BEGSR
175400100326     C*
175500100326     C* NSP => Stacco un numeratore da AZNUM
175600100326     C                   clear                   TRUL33DS
175700100326     C                   eval      I33OPE = *zeros
175800100326     C                   eval      I33CNU = 302
175900100326     C                   eval      I33NUM = 1
176000100326     C                   movel     TRUL33DS      KPJBU
176100100326     C                   call      'TRUL33R'
176200100326     C                   parm                    KPJBA
176300100326     C                   movel     KPJBU         TRUL33DS
176400100326     C                   if        O33ERR = *zeros
176500100326     C                   z-add     O33NRF        VABNSP
176600100326     C                   z-add     O33NRF        VATNSP
176700100326     C                   else
176800100326     C                   SETON                                        31
176900100326     C                   EVAL      vinmsg = %trimr(vinmsg)
177000100326     C                             + ' ' + 'VABNSP VATNSP'
177100100326     C                   endif
177200100326     C*
177300100326     C                   ENDSR
177400010330
177500100326
177600010330
177700000613     C     *inzsr        BEGSR
177800990910     C*
177900990910     C     *entry        plist
178000990920     C                   parm                    tivlrds
178100990921     C                   parm      wrkesito      esito
178200000724     C                   parm                    prmlit
178300000710     C                   parm                    prmfir
178400010330     C*
178500010330     C* CALCOLA LA DATA CORRENTE
178600100318     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
178700100318     C                   eval      datcor = %dec(%date() : *ISO)
178800000613     C*
178900000613     C                   ENDSR
179000040113** INTCOL - INTESTAZIONE COLONNE PERSONALIZZATA
179100120629VABRMA;VABLOD;VABCAD;VABRSD;VABIND;VATNOT_A;VATNOT_B;VABNCL;VABPKB;X;X;X;
179200120703VATNOT_E;X
179300120604
179400120604
179500120604
