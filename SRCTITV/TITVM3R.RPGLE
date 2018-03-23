000100050511      /TITLE Upload via Internet: traduzione in EDIVABWR/EDIVATWR/EDIVADWR.
000200990908     H dftactgrp(*yes)
000300000313     F*
000400000724     Fazorg01l  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600050511     FEDIVABwr  O    E             DISK    usropn
000700050511     FEDIVATwr  O    E             DISK    usropn
000800050511     FEDIVADwr  O    E             DISK    usropn
000900050415     Ftitvm3p   O    f  132        PRINTER usropn
001000000621     F                                     oflind(*inoa)
001100070502     F                                     infsr(*pssr)
001200050415     Ftitvm3ps  O    f  198        PRINTER usropn
001300000621     F                                     oflind(*inob)
001400070502     F                                     infsr(*pssr)
001500000313     D*
001600000613     D*------------
001700000613     D* COMANDI
001800000613     D*------------
001900011113     D cmd             S            100    DIM(5) CTDATA PERRCD(1)
002000040113     D*------------
002100040113     D* SCHIEDE A PROGRAMMA
002200040113     D*------------
002300040113     D intcol          S            100    DIM(3) CTDATA PERRCD(1)
002400010330     D*----------------------------------------------------
002500010330     D* DICHIARAZIOINE VARIABILI DI WRK
002600010330     D*----------------------------------------------------
002700010330     D dscmz         e ds                  inz
002800010330     D psds           sds
002900010330     D  procname         *PROC
003000010330     D tivlrds       e ds                  extname(tivlr00f)
003100010330     D esito           s              1
003200010330     D prmlit          s             10
003300010330     D prmfir          s             10
003400010330     D wrkesito        s                   like(esito)
003500010330     D wrkdata         s               d
003600010330     D wrkora          s               t
003700010330     D rrnum           s              6  0 INZ(*zeros)
003800010330     D recko           s            150    INZ(*blanks)
003900011113     D depcmd          s            150    INZ(*blanks)
004000040119     D parccm          s              8    INZ(*blanks)
004100040119     D parmbr          s             10    INZ(*blanks)
004200040119     D paropz          s              1    INZ(*blanks)
004300040119     D chkcall         s              1    INZ(*blanks)
004400040706     D depvinDTA       s                   LIKE(vinDTA) INZ(*blanks)
004500050511     D wContaRec       s             10  0 INZ(*zeros)
004600050511     D wGruppiDa       s             10  0 INZ(100)
004700050511     D wContaGruppi    s             10  0 INZ(1)
004800050511     D wContaGruppiA   s             10    INZ(*blanks)
004900050511     D wDepRMA         s                   LIKE(VABRMA) INZ(*blanks)
005000050415     D*------------------
005100050415     D* DS D RIDEFINIZIONE CAMPI VAB "CONCATENATI"
005200050415     D*------------------
005300050415     D dsRAG           DS
005400050415     D VABRSD                        35
005500050415     D VABRD2                        35
005600050415     D*
005700050415     D dsNOT           DS
005800050415     D VABNOT                        35
005900050415     D VABNT2                        35
006000030715     D*------------------
006100030715     D* VARIABILI X LO SPLIT DELLA STRINGA CSV IN CAMPI
006200030715     D*------------------
006300050415     D SkSplitFLD      S             50    DIM(100)
006400030715     D SkSplitCSV      S            256    DIM(100)
006500030715     D CharCSV         S              1
006600030715     D CharTXT         S              1
006700030715     D CharNUM         S              1
006800030715     D posDa           S              3  0 INZ(*zeros)
006900030715     D posA            S              3  0 INZ(*zeros)
007000030715     D i               s              3  0 INZ(1)
007100030715     D wGiro           s              1  0 INZ(*zeros)
007200050415     D wRAGSOC1        s            200    INZ(*blanks)
007300050415     D wRAGSOC2        s            200    INZ(*blanks)
007400050415     D wNOTE1          s            200    INZ(*blanks)
007500050415     D wNOTE2          s            200    INZ(*blanks)
007600050415     D wPOSTALIZ_R1    s            200    INZ(*blanks)
007700050415     D wPOSTALIZ_R2    s            200    INZ(*blanks)
007800050415     D wPOSTALIZ_R3    s            200    INZ(*blanks)
007900050415     D wLOCALITA       s            200    INZ(*blanks)
008000050415     D wInR2           s               N   INZ(*off)
008100050415     D wInR3           s               N   INZ(*off)
008200050415     D wInLOC          s               N   INZ(*off)
008300030822     D*------------------
008400030822     D* VARIABILI X LO SPLIT DEI VALORI DI DEFAULT PROVENIENTI DAI PARAMETRI DEL TRADUTTORE
008500030822     D*------------------
008600030822     D posDaDft        S              3  0 INZ(*zeros)
008700030822     D posADft         S              3  0 INZ(*zeros)
008800030822     D j               s              3  0 INZ(1)
008900030822     D wGiroDft        s              1  0 INZ(*zeros)
009000010330     D*------------------
009100010330     D* DS "XSRDA8" - CONTROLLA DATA (8)
009200010330     D*------------------
009300010330     D WLBDA8          DS                  INZ
009400010330     D  G08DAT                 1      8  0
009500010330     D  G08INV                 9     16  0
009600010330     D  G08ERR                17     17
009700010330     D  G08TGI                18     22  0
009800031201     D*------------------
009900031201     D* Costanti
010000031201     D*------------------
010100031201     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
010200031201     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
010300010330
010400010330
010500010330
010600990915     C                   time                    wrkdata
010700990915     C                   time                    wrkora
010800000913     C                   reset                   rrnum
010900990921     C                   reset                   esito
011000990921     C                   reset                   wrkesito
011100010601     C*
011200010601     C                   exsr      opeini
011300010605     C                   exsr      rwvab
011400080930     C*
011500080930     C                   exsr      endela
011600010601     C*
011700010601     C                   seton                                        lr
011800010601
011900010601
012000010601
012100010601     C*--------------------------------------------------------
012200010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
012300010601     C*--------------------------------------------------------
012400010601     C     PREELA        BEGSR
012500010601     C*
012600010601     C* SE OCCORRE SPEDIRE IN FILIALE
012700010601     C                   if        invfil <> *zeros and
012800010601     C                             flgGiro = '0'
012900010601     C*
013000010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
013100010601     C                   eval      flgGiro = '1'
013200000724     C*
013300000724     C* REPERISCO L'AS/400 A CUI DOVER INVIARE E LA LIBRERIA DI AZIENDA DEL S.I.
013400000724     C* X COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
013500010601     C     invfil        chain     azorg01l
013600000724     C                   if        %found
013700000724     C                   movel(p)  CMD(1)        depcmd
013800020809     C                   EVAL      depcmd=%trim(depcmd)+%trim(orgde4)+')'
013900000724     C*
014000000724     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODE AS/400 DI FILIALE
014100011113     C                   Z-ADD     150           LENGH            15 5
014200000724     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
014300000724     C                   PARM                    depcmd
014400000724     C                   PARM                    LENGH
014500000724     C*
014600000724     C                   endif
014700000616     C*
014800000616     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
014900000616     C                   movel(p)  CMD(2)        depcmd
015000000616     C                   EVAL      depcmd=%trim(depcmd)+'TRADUZIONI'+')'
015100000616     C*
015200000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA DI SEDE
015300011113     C                   Z-ADD     150           LENGH            15 5
015400000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
015500000616     C                   PARM                    depcmd
015600000616     C                   PARM                    LENGH
015700000613     C*
015800050415     C                   if        not %open(titvm3ps)
015900050415     C                   open      titvm3ps
016000030715     C                   except    testdett
016100000613     C                   endif
016200010601     C*
016300010601     C                   endif
016400010601     C*
016500010601     C                   ENDSR
016600010601     C***
016700010601
016800010601
016900010601
017000010601
017100010601     C*--------------------------------------------------------
017200010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
017300010601     C*--------------------------------------------------------
017400010601     C     ENDELA        BEGSR
017500990915     C*
017600000621     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
017700000613     C*
017800050415     C                   if        %open(titvm3ps)
017900000616     C                   except    findett
018000050415     C                   close     titvm3ps
018100000613     C                   endif
018200080930     C*
018300080930     C                   if        %open(tivin00r)
018400080930     C                   close     tivin00r
018500080930     C                   endif
018600080930     C                   if        %open(edivabwr)
018700080930     C                   close     edivabwr
018800080930     C                   endif
018900080930     C                   if        %open(edivatwr)
019000080930     C                   close     edivatwr
019100080930     C                   endif
019200080930     C                   if        %open(edivadwr)
019300080930     C                   close     edivadwr
019400080930     C                   endif
019500080930     C*
019600080930     C* Effettuo la chiamata al CLLE preposto
019700080930     C                   call(e)   'TITVEVTC'
019800080930     C                   parm                    parccm
019900080930     C                   parm                    parmbr
020000080930     C                   parm      '2'           paropz
020100080930     C                   call(e)   'TITVEVDC'
020200080930     C                   parm                    parccm
020300080930     C                   parm                    parmbr
020400080930     C                   parm      '2'           paropz
020500000616     C*
020600000616     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
020700011113     C                   Z-ADD     150           LENGH            15 5
020800010618     C                   CALL(e)   'QCMDEXC'                                    *LANCIA CMD
020900000616     C                   PARM                    CMD(3)
021000000616     C                   PARM                    LENGH
021100000616     C*
021200010601     C                   ENDSR
021300010601     C***
021400010601
021500010601
021600010601
021700000613
021800000613     C*--------------------------------------------------------
021900000621     C* STPR  -  STAMPA IL RIEPILOGO (VA IN FILIALE)          *
022000000613     C*--------------------------------------------------------
022100000621     C     STPR          BEGSR
022200000613     C*
022300050415     C                   if        not %open(titvm3p)
022400050415     C   50              open      titvm3p
022500990915     C                   endif
022600990915     C*
022700030715     C   50              except    riepilogo
022800990915     C*
022900050415     C                   if        %open(titvm3p)
023000050415     C   50              close     titvm3p
023100990914     C                   endif
023200000613     C*
023300000613     C                   ENDSR
023400000613     C***
023500000613
023600010601
023700010601
023800010330     C*--------------------------------------------------------
023900050511     C* RWVAB   LEGGE TIVIN00R E SCRIVE EDIVABWF              *
024000010330     C*--------------------------------------------------------
024100010605     C     RWVAB         BEGSR
024200010330     C*
024300010330     C                   if        not %open(tivin00r)
024400010330     C                   open      tivin00r
024500010330     C                   endif
024600050511     C                   if        not %open(edivabwr)
024700050511     C                   open      edivabwr
024800010330     C                   endif
024900040119     C*
025000050511     C* Eseguo operazioni di aggiunta nuovo membro in EDIVATWR
025100040119     C                   exsr      prevat
025200040119     C*
025300040119     C                   if        chkcall = '0'
025400040119     C*
025500050511     C                   if        not %open(edivatwr)
025600050511     C                   open      edivatwr
025700040119     C                   endif
025800050511     C*
025900050511     C                   if        not %open(edivadwr)
026000050511     C                   open      edivadwr
026100050511     C                   endif
026200010330     C*
026300040119     C                   clear                   §CTROKVB          7 0
026400040119     C                   clear                   §CTROKVT          7 0
026500050511     C                   clear                   §CTROKVD          7 0
026600040119     C                   clear                   §CTRMO            7 0
026700040119     C                   clear                   §CTRNO            7 0
026800050330     C*
026900050330     C                   do        *HIVAL
027000010330     C*
027100040706     C                   read      tivin00r                               70
027200010618     C*
027300010618     C* Dopo ogni lettura verifico se ci sono stati record OK
027400010618     C                   if        vinflg = '1'
027500010618     C                   eval      flgOk = '1'
027600010618     C                   endif
027700050330     C*
027800050330     C* Verifico che il record nn contenga unicamente i caratteri d separatore campi
027900050330     C                   z-add     *zeros        wLenVINDTA        4 0
028000050330     C                   z-add     *zeros        wFlgVINDTA        1 0
028100050330     C                   dow       wLenVINDTA < %len(%trim(vindta))
028200050330     C                   eval      wLenVINDTA = wLenVINDTA + 1
028300050330     C                   if        %subst(%trim(vindta):wLenVINDTA:1)<>CharCSV
028400050330     C                   z-add     1             wFlgVINDTA
028500050330     C                   leave
028600050330     C                   endif
028700050330     C                   enddo
028800010618     C*
028900050330     C                   if        vindta > *blanks AND
029000050330     C                             wFlgVINDTA = 1
029100010330     C                   add       1             rrnum
029200010330     C*
029300010601     C                   if        *in70 = *off and
029400010330     C                             (vinflg = *blanks
029500010330     C                              or vinflg = '0'
029600010330     C                              or vinflg = '2')
029700010330     C*
029800010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
029900010711     C                   if        vinflg = *blanks or vinflg = '0'
030000010711     C                   clear                   vinmsg
030100010711     C                   endif
030200010601     C*
030300010330     C                   exsr      inzvar
030400010605     C                   exsr      impvab
030500010601     C*
030600010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
030700010605     C                   eval      depfil = VABLNP
030800010601     C                   exsr      repfil
030900010601     C                   if        depfil = invfil
031000021025     C                   if        vlrpoi = 999
031100030822     C                   move(P)   invfil        VABFGS
031200040119     C                   move(P)   invfil        VATFGS
031300050511     C                   move(P)   invfil        VADFGS
031400021025     C                   else
031500030822     C                   move(P)   vlrpoi        VABFGS
031600040119     C                   move(P)   vlrpoi        VATFGS
031700050511     C                   move(P)   vlrpoi        VADFGS
031800021025     C                   endif
031900010601     C*
032000010601     C                   exsr      PREELA
032100010601     C*
032200010604     C* Ebbene...
032300010604     C*
032400040119     C  N31              add       1             §CTROKVB
032500040119     C   32              add       1             §CTRMO
032600040119     C   31              add       1             §CTRNO
032700030822     C                   if        wGiro = 2
032800040119     C  N31              exsr      wrivab
032900040119     C                   endif
033000010604     C*
033100010604     C                   if        *in31 = *off and
033200010604     C                             *in32 = *off
033300010604     C                   eval      vinflg = '1'
033400010604     C                   else
033500010604     C                   eval      recko = vindta
033600010604     C                   except    rigadett
033700010604     C                   eval      vinflg = '2'
033800010604     C                   endif
033900010604     C                   endif
034000010601     C*
034100010604     C                   endif
034200010604     C*
034300010330     C                   else
034400010330     C                   eval      vinflg = '1'
034500010330     C                   endif
034600010601     C*
034700010601     C  N70              update    tivin000
034800010330     C*
034900030822     C  N70              enddo
035000040119     C*
035100040119     C                   endif                                                  (endif - chkcall)
035200010601     C*
035300010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
035400010601     C                   if        cntNonEl = *zeros or
035500010601     C                             flgMulti = '0'
035600010330     C* Se non ci sono record con errori ...
035700010601     C                   if        §ctrno = 0 and
035800010604     C                             §ctrmo = 0 and
035900010601     C                             flgStato <> '2'
036000010330     C* ... restituisco esito OK.
036100010330     C                   eval      wrkesito = '0'
036200010330     C                   else
036300040119     C                   if        §ctrokvb > 0
036400010330     C                   eval      wrkesito = '1'
036500010330     C                   else
036600010615     C                   if        flgOk = '0'
036700010615     C                   eval      wrkesito = '2'
036800010615     C                   else
036900010615     C                   eval      wrkesito = '6'
037000010615     C                   endif
037100010330     C                   endif
037200010330     C                   endif
037300010601     C                   else
037400010601     C                   eval      wrkesito = '9'
037500010601     C                   endif
037600010601     C*
037700010601     C                   if        vlrpoi <> 999
037800010601     C                   eval      invfil = vlrpoi
037900010601     C                   endif
038000010330     C*
038100040119     C                   if        §ctrokvb > 0
038200010601     C                             and invfil > *zeros
038300010330     C                   exsr      invio
038400010330     C                   endif
038500010601     C*
038600010618     C                   if        flgGiro = '1'
038700010601     C                   exsr      endela
038800010618     C                   endif
038900010330     C*
039000010330     C                   ENDSR
039100010330     C***
039200040119
039300040119
039400040119
039500040119     C*----------------------------------------------------*
039600040119     C*  SCARICAMENTO BUFFER RECORDS VAB
039700040119     C*----------------------------------------------------*
039800040119     C     WRIVAB        BEGSR
039900040119     C*
040000050511     C                   write     edivab00                                     => scarico il VAB
040100040119     C*
040200040119     C* Eseguo la scrittura dei "CHI SONO" della spedizione corrente
040300050330     C                   exsr      wrivat
040400050511     C*
040500050511     C* Eseguo la scrittura dei dettagli collo
040600050511     C                   exsr      wrivad
040700040119     C*
040800040119     C                   ENDSR
040900040119
041000040119
041100040119
041200040119     C*----------------------------------------------------*
041300040119     C*  SCARICAMENTO BUFFER RECORDS VAT
041400040119     C*----------------------------------------------------*
041500040706     C     WRIVAT        BEGSR
041600040706     C*
041700040706     C* Attribuisco lo stesso numero numero spedizione del file testate
041800040706     C                   EVAL      VATNSP = VABNSP
041900050511     C*
042000050511     C* Valorizzo i dati relativi al legame testata CMR
042100050511     C                   EVAL      VATCMR = VABCMR
042200050511     C                   EVAL      VATCNT = VABCNT
042300050415     C*
042400050511     C* Quindi scarico il buffer del record EDIVAT ma solo se valorizzato
042500050415     C                   IF        VATNOT <> *blanks
042600050511     C                   WRITE     EDIVAT00
042700050415     C                   ENDIF
042800040119     C*
042900040119     C                   ENDSR
043000050511
043100050511
043200050511
043300050511     C*----------------------------------------------------*
043400050511     C*  SCARICAMENTO BUFFER RECORDS VAD
043500050511     C*----------------------------------------------------*
043600050511     C     WRIVAD        BEGSR
043700050511     C*
043800050511     C* Attribuisco lo stesso numero numero spedizione del file testate
043900050511     C                   EVAL      VADNSP = VABNSP
044000050511     C*
044100050511     C* Valorizzo i dati relativi al legame testata CMR
044200050511     C                   EVAL      VADCMR = VABCMR
044300050511     C                   EVAL      VADCNT = VABCNT
044400050511     C*
044500050511     C* Creo 1 record codice prodotto x ogni Kit indicato nei gruppi
044600050511     C                   IF        VABRMA <> *blanks
044700050511     C                   IF        %scan('+':VABRMA) > 0
044800050511     C                   Z-ADD     1             wPosDa            2 0
044900050511     C                   Z-ADD     1             wPosA             2 0
045000050511     C                   DOW       wPosA < %len(VABRMA)
045100050511     C                   IF        %scan('+':VABRMA:wPosDa) > 0
045200050511     C                   EVAL      wPosA = %scan('+':VABRMA:wPosDa)
045300050520     C                   EVAL      VADCDP = %subst(VABRMA:wPosDa:wPosA-wPosDa)
045400050511     C                   WRITE     EDIVAD00
045500050511     C                   EVAL      wPosDa = wPosA + 1
045600050511     C                   ELSE
045700050511     C                   EVAL      VADCDP = %subst(VABRMA:wPosDa)
045800050511     C                   WRITE     EDIVAD00
045900050511     C                   LEAVE
046000050511     C                   ENDIF
046100050511     C                   ENDDO
046200050511     C*
046300050511     C                   ELSE
046400050511     C                   EVAL      VADCDP = VABRMA
046500050511     C                   WRITE     EDIVAD00
046600050511     C                   ENDIF
046700050511     C                   ENDIF
046800050511     C*
046900050511     C                   ENDSR
047000040119
047100040119
047200040119
047300040119     C*----------------------------------------------------*
047400050511     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE EDIVATWR
047500040119     C*----------------------------------------------------*
047600040119     C     PREVAT        BEGSR
047700040119     C*
047800050511     C* Compongo il nome del membro da dare al EDIVATWR
047900040119     C                   eval      parmbr = vlrhdl
048000040119     C                   movel     'M'           parmbr
048100050330     C                   eval      parccm = %subst(vlrKSC:2:7)
048200040119     C                   eval      paropz = '1'
048300050511     C*
048400040119     C* Effettuo la chiamata al CLLE preposto
048500050511     C                   call(e)   'TITVEVTC'
048600040119     C                   parm                    parccm
048700040119     C                   parm                    parmbr
048800040119     C                   parm                    paropz
048900040119     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
049000040119     C                   if        %error
049100040119     C                   movel     '1'           chkcall
049200040119     C                   else
049300040119     C                   movel     '0'           chkcall
049400040119     C                   endif
049500050511     C*
049600050511     C* Effettuo la chiamata al CLLE preposto
049700050511     C                   call(e)   'TITVEVDC'
049800050511     C                   parm                    parccm
049900050511     C                   parm                    parmbr
050000050511     C                   parm                    paropz
050100050511     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
050200050511     C                   if        %error
050300050511     C                   movel     '1'           chkcall
050400050511     C                   else
050500050511     C                   movel     '0'           chkcall
050600050511     C                   endif
050700040119     C*
050800040119     C                   ENDSR
050900010601
051000010601
051100010601
051200010330     C*----------------------------------------------------*
051300030715     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
051400010330     C*----------------------------------------------------*
051500040119     C     INZVAR        BEGSR
051600010330     C*
051700030822     C* Inizializzo variabili di wrk
051800020204     C                   Z-ADD     *zeros        Num5_0            5 0
051900050415     C                   EVAL      wRAGSOC1 = *blanks
052000050415     C                   EVAL      wRAGSOC2 = *blanks
052100050415     C                   EVAL      wNOTE1   = *blanks
052200050415     C                   EVAL      wNOTE2   = *blanks
052300050415     C                   EVAL      wPOSTALIZ_R1 = *blanks
052400050415     C                   EVAL      wPOSTALIZ_R2 = *blanks
052500050415     C                   EVAL      wPOSTALIZ_R3 = *blanks
052600050415     C                   EVAL      wLOCALITA = *blanks
052700050415     C                   EVAL      wInR2  = *off
052800050415     C                   EVAL      wInR3  = *off
052900050415     C                   EVAL      wInLOC = *off
053000030822     C*
053100030822     C* Inizializzo il buffer del record da scrivere e la schiera d wrk x i dati
053200050511     C                   CLEAR                   EDIVAB00
053300050511     C                   CLEAR                   EDIVAT00
053400050511     C                   CLEAR                   EDIVAD00
053500030822     C                   CLEAR                   SkSplitCSV
053600030822     C                   EVAL      i = 1
053700030822     C                   EVAL      posDa = *zeros
053800030822     C                   EVAL      posA  = *zeros
053900030822     C*
054000030822     C* Reimposto i valori di default
054100030822     C                   EXSR      DEFCAM
054200030822     C*
054300010330     C                   ENDSR
054400010330     C*----------------------------------------------------*
054500030822     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
054600010330     C*----------------------------------------------------*
054700010330     C     DEFCAM        BEGSR
054800040119     C*
054900040119     C* Imposto i valori d default a programma dei campi fondamentali
055000050415     C                   EVAL      VABCCM = 0633320
055100050415     C                   EVAL      VATCCM = 0633320
055200050511     C                   EVAL      VADCCM = 0633320
055300050415     C                   EVAL      VABLNP = 063
055400050415     C                   EVAL      VATLNP = 063
055500050511     C                   EVAL      VADLNP = 063
055600050415     C                   EVAL      VABCTR = 001
055700050415     C                   EVAL      VABCTM = '7'
055800040119     C                   EVAL      VABCBO = '1'
055900050415     C                   EVAL      VATTRC = 'B'
056000050511     C                   EVAL      VADNCL = 1
056100030822     C*
056200030715     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
056300030715     C* e delimitatore testo.
056400030715     C                   EVAL      CharCSV = %subst(vlrppt:2:1)
056500030715     C                   EVAL      CharTXT = %subst(vlrppt:3:1)
056600030715     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
056700050330     C*
056800050330     C* Reperisco il flag che mi indica se effettuare o meno la stampa in filiale
056900050330     C                   IF        %subst(vlrppt:1:1) = 'S'
057000050330     C                   SETON                                        50
057100050330     C                   ELSE
057200050330     C                   SETOFF                                       50
057300050330     C                   ENDIF
057400030822     C*
057500030822     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
057600030822     C                   EVAL      posDaDft = 1
057700030822     C                   EVAL      posADft  = 0
057800030822     C                   EVAL      wGiroDft = 0
057900030822     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
058000030822     C                             posDaDft > 0
058100030822     C*
058200030822     C* Gestisco il 1° giro
058300030822     C                   IF        wGiroDft = 0
058400030822     C* Eseguo lo scan x trovare l'inizio del campo corrente
058500030822     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
058600030822     C* Incremento il contatore dei "giri"
058700030822     C                   EVAL      wGiroDft = 1
058800030822     C                   ELSE
058900030822     C                   EVAL      posDaDft = posADft
059000030822     C                   ENDIF
059100030822     C* Eseguo lo scan x trovare la fine del campo corrente
059200030822     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
059300030822     C*
059400030822     C* A questo "estraggo" il parametro (campo e valore) corrente...
059500030822     C* ...solo se entrambe le posizini (DA/A) sono > 0
059600030822     C                   IF        posDaDft > 0 AND
059700030822     C                             posADft  > 0
059800030822     C* NCL
059900030822     C                   IF        %subst(
060000030822     C                             %subst(vlrppt:posDaDft+1:
060100030822     C                             posADft-posDaDft-1):1:3)
060200030822     C                             = 'NCL'
060300030822     C                   EVAL      PiStr=%trim(%subst(
060400030822     C                             %subst(vlrppt:posDaDft+1:
060500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
060600030822     C                   EXSR      CHKNUM
060700030822     C                   IF        PiInt=*on
060800030822     C                   Z-ADD     PiVal         VABNCL
060900030822     C                   ENDIF
061000030822     C                   ENDIF
061100030822     C* CCM
061200030822     C                   IF        %subst(
061300030822     C                             %subst(vlrppt:posDaDft+1:
061400030822     C                             posADft-posDaDft-1):1:3)
061500030822     C                             = 'CCM'
061600030822     C                   EVAL      PiStr=%trim(%subst(
061700030822     C                             %subst(vlrppt:posDaDft+1:
061800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
061900030822     C                   EXSR      CHKNUM
062000030822     C                   IF        PiInt=*on
062100030822     C                   Z-ADD     PiVal         VABCCM
062200040119     C                   Z-ADD     PiVal         VATCCM
062300050511     C                   Z-ADD     PiVal         VADCCM
062400030822     C                   ENDIF
062500030822     C                   ENDIF
062600030822     C* LNP
062700030822     C                   IF        %subst(
062800030822     C                             %subst(vlrppt:posDaDft+1:
062900030822     C                             posADft-posDaDft-1):1:3)
063000030822     C                             = 'LNP'
063100030822     C                   EVAL      PiStr=%trim(%subst(
063200030822     C                             %subst(vlrppt:posDaDft+1:
063300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
063400030822     C                   EXSR      CHKNUM
063500030822     C                   IF        PiInt=*on
063600030822     C                   Z-ADD     PiVal         VABLNP
063700040119     C                   Z-ADD     PiVal         VATLNP
063800050511     C                   Z-ADD     PiVal         VADLNP
063900030822     C                   ENDIF
064000030822     C                   ENDIF
064100030822     C* NRS
064200030822     C                   IF        %subst(
064300030822     C                             %subst(vlrppt:posDaDft+1:
064400030822     C                             posADft-posDaDft-1):1:3)
064500030822     C                             = 'NRS'
064600030822     C                   EVAL      PiStr=%trim(%subst(
064700030822     C                             %subst(vlrppt:posDaDft+1:
064800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
064900030822     C                   EXSR      CHKNUM
065000030822     C                   IF        PiInt=*on
065100030822     C                   Z-ADD     PiVal         VABNRS
065200030822     C                   ENDIF
065300030822     C                   ENDIF
065400030822     C* CTR
065500030822     C                   IF        %subst(
065600030822     C                             %subst(vlrppt:posDaDft+1:
065700030822     C                             posADft-posDaDft-1):1:3)
065800030822     C                             = 'CTR'
065900030822     C                   EVAL      PiStr=%trim(%subst(
066000030822     C                             %subst(vlrppt:posDaDft+1:
066100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
066200030822     C                   EXSR      CHKNUM
066300030822     C                   IF        PiInt=*on
066400030822     C                   Z-ADD     PiVal         VABCTR
066500030822     C                   ENDIF
066600030822     C                   ENDIF
066700030822     C* PKB
066800030822     C                   IF        %subst(
066900030822     C                             %subst(vlrppt:posDaDft+1:
067000030822     C                             posADft-posDaDft-1):1:3)
067100030822     C                             = 'PKB'
067200030822     C                   EVAL      PiStr=%trim(%subst(
067300030822     C                             %subst(vlrppt:posDaDft+1:
067400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
067500030822     C                   EXSR      CHKNUM
067600030822     C                   IF        PiNum=*on
067700050330     C                   Z-ADD     PiVal         VABPKB
067800030822     C                   ENDIF
067900030822     C                   ENDIF
068000030822     C* VLB
068100030822     C                   IF        %subst(
068200030822     C                             %subst(vlrppt:posDaDft+1:
068300030822     C                             posADft-posDaDft-1):1:3)
068400030822     C                             = 'VLB'
068500030822     C                   EVAL      PiStr=%trim(%subst(
068600030822     C                             %subst(vlrppt:posDaDft+1:
068700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
068800030822     C                   EXSR      CHKNUM
068900030822     C                   IF        PiNum=*on
069000050415     C                   Z-ADD     PiVal         VABVLB
069100030822     C                   ENDIF
069200030822     C                   ENDIF
069300030822     C* QFT
069400030822     C                   IF        %subst(
069500030822     C                             %subst(vlrppt:posDaDft+1:
069600030822     C                             posADft-posDaDft-1):1:3)
069700030822     C                             = 'QFT'
069800030822     C                   EVAL      PiStr=%trim(%subst(
069900030822     C                             %subst(vlrppt:posDaDft+1:
070000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
070100030822     C                   EXSR      CHKNUM
070200030822     C                   IF        PiNum=*on
070300030822     C                   Z-ADD     PiVal         VABQFT
070400030822     C                   ENDIF
070500030822     C                   ENDIF
070600030822     C* CBO
070700030822     C                   IF        %subst(
070800030822     C                             %subst(vlrppt:posDaDft+1:
070900030822     C                             posADft-posDaDft-1):1:3)
071000030822     C                             = 'CBO'
071100030822     C                   EVAL      VABCBO=%trim(%subst(
071200030822     C                             %subst(vlrppt:posDaDft+1:
071300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
071400030822     C                   ENDIF
071500030822     C* TSP
071600030822     C                   IF        %subst(
071700030822     C                             %subst(vlrppt:posDaDft+1:
071800030822     C                             posADft-posDaDft-1):1:3)
071900030822     C                             = 'TSP'
072000030822     C                   EVAL      VABTSP=%trim(%subst(
072100030822     C                             %subst(vlrppt:posDaDft+1:
072200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
072300030822     C                   ENDIF
072400030822     C* VAS
072500030822     C                   IF        %subst(
072600030822     C                             %subst(vlrppt:posDaDft+1:
072700030822     C                             posADft-posDaDft-1):1:3)
072800030822     C                             = 'VAS'
072900030822     C                   EVAL      VABVAS=%trim(%subst(
073000030822     C                             %subst(vlrppt:posDaDft+1:
073100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
073200030822     C                   ENDIF
073300030822     C* VCA
073400030822     C                   IF        %subst(
073500030822     C                             %subst(vlrppt:posDaDft+1:
073600030822     C                             posADft-posDaDft-1):1:3)
073700030822     C                             = 'VCA'
073800030822     C                   EVAL      VABVCA=%trim(%subst(
073900030822     C                             %subst(vlrppt:posDaDft+1:
074000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
074100030822     C                   ENDIF
074200030822     C* GCA
074300030822     C                   IF        %subst(
074400030822     C                             %subst(vlrppt:posDaDft+1:
074500030822     C                             posADft-posDaDft-1):1:3)
074600030822     C                             = 'GCA'
074700030822     C                   EVAL      VABGCA=%trim(%subst(
074800030822     C                             %subst(vlrppt:posDaDft+1:
074900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
075000030822     C                   ENDIF
075100030822     C* CTM
075200030822     C                   IF        %subst(
075300030822     C                             %subst(vlrppt:posDaDft+1:
075400030822     C                             posADft-posDaDft-1):1:3)
075500030822     C                             = 'CTM'
075600030822     C                   EVAL      VABCTM=%trim(%subst(
075700030822     C                             %subst(vlrppt:posDaDft+1:
075800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
075900030822     C                   ENDIF
076000030822     C* FFD
076100030822     C                   IF        %subst(
076200030822     C                             %subst(vlrppt:posDaDft+1:
076300030822     C                             posADft-posDaDft-1):1:3)
076400030822     C                             = 'FFD'
076500030822     C                   EVAL      VABFFD=%trim(%subst(
076600030822     C                             %subst(vlrppt:posDaDft+1:
076700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
076800030822     C                   ENDIF
076900030822     C* VAD
077000030822     C                   IF        %subst(
077100030822     C                             %subst(vlrppt:posDaDft+1:
077200030822     C                             posADft-posDaDft-1):1:3)
077300030822     C                             = 'VAD'
077400030822     C                   EVAL      VABVAD=%trim(%subst(
077500030822     C                             %subst(vlrppt:posDaDft+1:
077600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
077700030822     C                   ENDIF
077800030822     C* GMA
077900030822     C                   IF        %subst(
078000030822     C                             %subst(vlrppt:posDaDft+1:
078100030822     C                             posADft-posDaDft-1):1:3)
078200030822     C                             = 'GMA'
078300030822     C                   EVAL      VABGMA=%trim(%subst(
078400030822     C                             %subst(vlrppt:posDaDft+1:
078500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
078600030822     C                   ENDIF
078700030822     C* GGA
078800030822     C                   IF        %subst(
078900030822     C                             %subst(vlrppt:posDaDft+1:
079000030822     C                             posADft-posDaDft-1):1:3)
079100030822     C                             = 'GGA'
079200030822     C                   EVAL      VABGGA=%trim(%subst(
079300030822     C                             %subst(vlrppt:posDaDft+1:
079400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
079500030822     C                   ENDIF
079600030822     C* GVA
079700030822     C                   IF        %subst(
079800030822     C                             %subst(vlrppt:posDaDft+1:
079900030822     C                             posADft-posDaDft-1):1:3)
080000030822     C                             = 'GVA'
080100030822     C                   EVAL      VABGVA=%trim(%subst(
080200030822     C                             %subst(vlrppt:posDaDft+1:
080300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
080400030822     C                   ENDIF
080500030822     C* TC1
080600030822     C                   IF        %subst(
080700030822     C                             %subst(vlrppt:posDaDft+1:
080800030822     C                             posADft-posDaDft-1):1:3)
080900030822     C                             = 'TC1'
081000030822     C                   EVAL      VABTC1=%trim(%subst(
081100030822     C                             %subst(vlrppt:posDaDft+1:
081200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
081300030822     C                   ENDIF
081400030822     C* TC2
081500030822     C                   IF        %subst(
081600030822     C                             %subst(vlrppt:posDaDft+1:
081700030822     C                             posADft-posDaDft-1):1:3)
081800030822     C                             = 'TC2'
081900030822     C                   EVAL      VABTC2=%trim(%subst(
082000030822     C                             %subst(vlrppt:posDaDft+1:
082100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
082200030822     C                   ENDIF
082300030822     C* ...
082400030822     C                   ENDIF
082500030822     C                   ENDDO
082600020204     C*
082700010330     C                   ENDSR
082800010607     C*----------------------------------------------------*
082900050511     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X EDIVAB)
083000010607     C*----------------------------------------------------*
083100010607     C     IMPVAB        BEGSR
083200010607     C*
083300010607     C                   SETOFF                                       3132
083400030515     C*
083500010607     C                   MOVEL     datcor        VABAAS
083600040119     C                   MOVEL     datcor        VATAAS
083700050511     C                   MOVEL     datcor        VADAAS
083800010607     C                   MOVE      datcor        VABMGS
083900010607     C*
084000030715     C**********
084100030715     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
084200030715     C**********
084300030715     C                   IF        wGiro = *zeros
084400040706     C*
084500040706     C* Salvo la 1° riga originale x poi sovrapporla con quella avente le ns. intestazioni campo
084600040706     C                   EVAL      depvinDTA = vinDTA
084700040113     C*
084800040113     C* Inserisco qui la forzatura della intesatazione colonne personalizzata
084900040113     C                   EVAL      vindta = %trim(intcol(1))+%trim(intcol(2))+
085000040113     C                                      %trim(intcol(3))
085100031201     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
085200031201     C     minu:maiu     XLATE     vindta        vindta
085300030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
085400030715     C                   DOW       posDa <= %len(%trim(vindta))
085500030715     C*
085600030715     C* Gestisco il 1° campo
085700030715     C                   IF        i = 1
085800030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
085900030715     C                   EVAL      posDa = 1
086000030715     C* Eseguo lo scan x trovare la fine del primo campo
086100030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
086200030715     C* A questo punto "estraggo" il campo corrente
086300030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
086400030715     C                                                    (posA-posDa))
086500030715     C* X i campi successivi al 1°
086600030715     C                   ELSE
086700030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
086800030715     C                   EVAL      posDa = posA + 1
086900030715     C* Eseguo lo scan x trovare la fine del campo corrente
087000030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
087100030715     C* Gestisco l'ultimo campo
087200030715     C                   IF        posA = *zeros
087300030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
087400030715     C* A questo punto "estraggo" il campo corrente
087500030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
087600030715     C                                                    (posA-posDa))
087700030715     C                   LEAVE
087800030715     C                   ELSE
087900030715     C* A questo punto "estraggo" il campo corrente
088000030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
088100030715     C                                                    (posA-posDa))
088200030715     C                   ENDIF
088300030715     C                   ENDIF
088400030715     C* Incremento il contatore d campo
088500030715     C                   EVAL      i = i +1
088600030715     C                   ENDDO
088700030715     C*
088800030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
088900030715     C* delimitazione testo
089000030715     C                   EVAL      i = 1
089100030715     C                   DOW       i < %elem(SkSplitFLD)
089200030715     C     CharTXT:' '   XLATE     SkSplitFLD(i) SkSplitFLD(i)
089300030715     C                   EVAL      SkSplitFLD(i) = %trim(SkSplitFLD(i))
089400030715     C                   EVAL      i = i + 1
089500030715     C                   ENDDO
089600030715     C                   EVAL      wGiro = 1
089700040706     C* Ripristino la 1° riga originale
089800040706     C                   EVAL      vinDTA = depvinDTA
089900030715     C                   ELSE
090000030715     C                   EVAL      wGiro = 2
090100031204     C**********
090200031204     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
090300031204     C* d separatore campo
090400031204     C**********
090500031204     C                   EVAL      vindta = %trim(vindta)
090600031204     C                   DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
090700031204     C                   EVAL      vindta = %subst(vindta:2)
090800031204     C                   ENDDO
090900031204     C*
091000031204     C                   Z-ADD     *zeros        lunghInput        4 0
091100031204     C                   EVAL      lunghInput = %len(%trim(vindta))
091200031204     C                   DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
091300031204     C                   EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
091400031204     C                   EVAL      lunghInput = %len(%trim(vindta))
091500031204     C                   ENDDO
091600030715     C**********
091700030715     C* Effettuo lo split del campo dati d input
091800030715     C**********
091900030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
092000030715     C                   DOW       posDa <= %len(%trim(vindta))
092100030715     C*
092200030715     C* Gestisco il 1° campo
092300030715     C                   IF        i = 1
092400030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
092500030715     C                   EVAL      posDa = 1
092600030715     C* Eseguo lo scan x trovare la fine del primo campo
092700030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
092800030715     C* A questo punto "estraggo" il campo corrente
092900030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
093000030715     C                                                    (posA-posDa))
093100030715     C* X i campi successivi al 1°
093200030715     C                   ELSE
093300030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
093400030715     C                   EVAL      posDa = posA + 1
093500031204     C* Verifico che nn vi sia il campo nn valorizzato
093600031204     C                   IF        %subst(vindta:posDa:1) = CharCSV
093700031204     C* Se campo nn valorizzato skippo al prossimo
093800031204     C                   EVAL      SkSplitCSV(i) = *blanks
093900031204     C                   EVAL      posA  = posA + 1
094000031204     C                   ELSE
094100030715     C* Eseguo lo scan x trovare la fine del campo corrente
094200030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
094300030715     C* Gestisco l'ultimo campo
094400030715     C                   IF        posA = *zeros
094500030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
094600030715     C* A questo punto "estraggo" il campo corrente
094700030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
094800030715     C                                                    (posA-posDa))
094900030715     C                   LEAVE
095000030715     C                   ELSE
095100030715     C* A questo punto "estraggo" il campo corrente
095200030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
095300030715     C                                                    (posA-posDa))
095400030715     C                   ENDIF
095500031204     C                   ENDIF
095600030715     C                   ENDIF
095700030715     C* Incremento il contatore d campo
095800030715     C                   EVAL      i = i +1
095900030715     C                   ENDDO
096000030715     C*
096100030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
096200030715     C* delimitazione testo
096300030715     C                   EVAL      i = 1
096400030715     C                   DOW       i < %elem(SkSplitCSV)
096500030715     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
096600030715     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
096700030715     C                   EVAL      i = i + 1
096800030715     C                   ENDDO
096900030715     C*
097000030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
097100030715     C                   EVAL      i = 1
097200030715     C                   DOW       i < %elem(SkSplitFLD)
097300030715     C*
097400030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
097500030715     C                   IF        SkSplitFLD(i) <> *blanks AND
097600030715     C                             SkSplitCSV(i) <> *blanks
097700030721     C*** ATB
097800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABATB'
097900030721     C                   EVAL      VABATB = SkSplitCSV(i)
098000030721     C                   ENDIF
098100050415     C*** RSD
098200050415     C                   IF        %trim(SkSplitFLD(i)) = 'RGS_1'
098300050415     C                   EVAL      wRAGSOC1 = SkSplitCSV(i)
098400030715     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
098500050415     C     '@':'A'       XLATE     wRAGSOC1      wRAGSOC1
098600030715     C* ==
098700030715     C                   ENDIF
098800050415     C*** RD2
098900050415     C                   IF        %trim(SkSplitFLD(i)) = 'RGS_2'
099000050415     C                   EVAL      wRAGSOC2 = SkSplitCSV(i)
099100050415     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
099200050415     C     '@':'A'       XLATE     wRAGSOC2      wRAGSOC2
099300050415     C* ==
099400050415     C                   ENDIF
099500050415     C*** IND
099600050415     C                   IF        %trim(SkSplitFLD(i)) = 'POSTALIZZAZIONE_R1'
099700050415     C                   EVAL      wPOSTALIZ_R1 = SkSplitCSV(i)
099800030715     C                   ENDIF
099900050415     C*** LOD "1"
100000050415     C                   IF        %trim(SkSplitFLD(i)) = 'POSTALIZZAZIONE_R3'
100100050415     C                   EVAL      wPOSTALIZ_R3 = SkSplitCSV(i)
100200050415     C                   ENDIF
100300050415     C*** LOD "2"
100400050415     C                   IF        %trim(SkSplitFLD(i)) = 'POSTALIZZAZIONE_R2'
100500050415     C                   EVAL      wPOSTALIZ_R2 = SkSplitCSV(i)
100600030715     C                   ENDIF
100700050415     C*** LOD "3"
100800050415     C                   IF        %trim(SkSplitFLD(i)) = 'LOCALITA'
100900050415     C                   EVAL      wLOCALITA = SkSplitCSV(i)
101000050415     C                   ENDIF
101100030715     C*** PRD
101200050415     C                   IF        %trim(SkSplitFLD(i)) = 'PROVINCIA'
101300030715     C                   EVAL      VABPRD = SkSplitCSV(i)
101400030715     C                   ENDIF
101500030721     C*** NZD
101600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNZD'
101700030721     C                   EVAL      VABNZD = SkSplitCSV(i)
101800030721     C                   ENDIF
101900050415     C*** RMA
102000050415     C                   IF        %trim(SkSplitFLD(i)) = 'DESC_MERCE'
102100030715     C                   EVAL      VABRMA = SkSplitCSV(i)
102200030715     C                   ENDIF
102300050415     C*** NOT
102400030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNOT'
102500030715     C                   EVAL      VABNOT = SkSplitCSV(i)
102600030715     C                   ENDIF
102700050415     C*** NT2
102800030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNT2'
102900030715     C                   EVAL      VABNT2 = SkSplitCSV(i)
103000030715     C                   ENDIF
103100050415     C*** VATNOT
103200050415     C                   IF        %trim(SkSplitFLD(i)) = 'NUMERO_TELEFONO_1'
103300050415     C                   EVAL      VATNOT = SkSplitCSV(i)
103400050415     C                   ENDIF
103500030715     C*** VCA
103600030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVCA'
103700030715     C                   EVAL      VABVCA = SkSplitCSV(i)
103800030715     C                   ENDIF
103900030715     C*** VAS
104000030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAS'
104100030715     C                   EVAL      VABVAS = SkSplitCSV(i)
104200030715     C                   ENDIF
104300030715     C*** FFD
104400030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABFFD'
104500030715     C                   EVAL      VABFFD = SkSplitCSV(i)
104600030715     C                   ENDIF
104700030721     C*** GC1
104800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC1'
104900030721     C                   EVAL      VABGC1 = SkSplitCSV(i)
105000030721     C                   ENDIF
105100030721     C*** GC2
105200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC2'
105300030721     C                   EVAL      VABGC2 = SkSplitCSV(i)
105400030721     C                   ENDIF
105500030721     C*** TSP
105600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTSP'
105700030721     C                   EVAL      VABTSP = SkSplitCSV(i)
105800030721     C                   ENDIF
105900030721     C*** NAS
106000050415     C                   IF        %trim(SkSplitFLD(i)) = 'VABNAS'
106100030721     C                   EVAL      VABNAS = SkSplitCSV(i)
106200030721     C                   ENDIF
106300030721     C*** TIC
106400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTIC'
106500030721     C                   EVAL      VABTIC = SkSplitCSV(i)
106600030721     C                   ENDIF
106700030721     C*** GCA
106800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGCA'
106900030721     C                   EVAL      VABGCA = SkSplitCSV(i)
107000030721     C                   ENDIF
107100030721     C*** XCO
107200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABXCO'
107300030721     C                   EVAL      VABXCO = SkSplitCSV(i)
107400030721     C                   ENDIF
107500030721     C*** CTM
107600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTM'
107700030721     C                   EVAL      VABCTM = SkSplitCSV(i)
107800030721     C                   ENDIF
107900030721     C*** TCR
108000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTCR'
108100030721     C                   EVAL      VABTCR = SkSplitCSV(i)
108200030721     C                   ENDIF
108300030721     C*** CTS
108400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTS'
108500030721     C                   EVAL      VABCTS = SkSplitCSV(i)
108600030721     C                   ENDIF
108700030721     C*** FTM
108800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABFTM'
108900030721     C                   EVAL      VABFTM = SkSplitCSV(i)
109000030721     C                   ENDIF
109100030721     C*** VAD
109200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAD'
109300030721     C                   EVAL      VABVAD = SkSplitCSV(i)
109400030721     C                   ENDIF
109500030721     C*** GMA
109600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGMA'
109700030721     C                   EVAL      VABGMA = SkSplitCSV(i)
109800030721     C                   ENDIF
109900030721     C*** GGA
110000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGGA'
110100030721     C                   EVAL      VABGGA = SkSplitCSV(i)
110200030721     C                   ENDIF
110300030721     C*** GVA
110400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGVA'
110500030721     C                   EVAL      VABGVA = SkSplitCSV(i)
110600030721     C                   ENDIF
110700030721     C*** TC1
110800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC1'
110900030721     C                   EVAL      VABTC1 = SkSplitCSV(i)
111000030721     C                   ENDIF
111100030721     C*** TC2
111200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC2'
111300030721     C                   EVAL      VABTC2 = SkSplitCSV(i)
111400030721     C                   ENDIF
111500030721     C*** SCL
111600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABSCL'
111700030721     C                   EVAL      VABSCL = SkSplitCSV(i)
111800030721     C                   ENDIF
111900030721     C*** RMO
112000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMO'
112100030721     C                   EVAL      VABRMO = SkSplitCSV(i)
112200030721     C                   ENDIF
112300030721     C*** NMO
112400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNMO'
112500030721     C                   EVAL      VABNMO = SkSplitCSV(i)
112600030721     C                   ENDIF
112700040119     C*** CBO
112800040706     C                   IF        %trim(SkSplitFLD(i)) = 'VABCBO'
112900040706     C                   EVAL      VABCBO = SkSplitCSV(i)
113000040119     C                   ENDIF
113100030721     C*
113200040706     C* Reperisco quindi i campi numerici...
113300040706     C*** NSP
113400050415     C                   IF        %trim(SkSplitFLD(i)) = 'RIF_MITT'
113500040706     C                   EVAL      PiStr=SkSplitCSV(i)
113600040706     C                   EXSR      CHKNUM
113700040706     C                   IF        PiInt=*on
113800040706     C                   Z-ADD     PiVal         VABNSP
113900040706     C                   ELSE
114000040706     C                   SETON                                        31
114100040706     C                   EVAL      VABNSP = *zeros
114200040706     C                   EVAL      vinmsg = %trimr(vinmsg)
114300040706     C                             + ' ' + 'VABNSP'
114400040706     C                   ENDIF
114500040706     C                   ENDIF
114600050415     C*** RMN
114700050415     C                   IF        %trim(SkSplitFLD(i)) = 'RIF_MITT'
114800040706     C                   EVAL      PiStr=SkSplitCSV(i)
114900040706     C                   EXSR      CHKNUM
115000040706     C                   IF        PiInt=*on
115100040706     C                   Z-ADD     PiVal         VABRMN
115200040706     C                   ELSE
115300040706     C                   SETON                                        32
115400040706     C                   EVAL      VABRMN = 1
115500040706     C                   EVAL      vinmsg = %trimr(vinmsg)
115600040706     C                             + ' ' + 'VABRMN'
115700040706     C                   ENDIF
115800040706     C                   ENDIF
115900030721     C*** CAD
116000050415     C                   IF        %trim(SkSplitFLD(i)) = 'CAP'
116100030721     C                   EVAL      PiStr=SkSplitCSV(i)
116200030721     C                   EXSR      CHKNUM
116300030721     C                   IF        PiInt=*on
116400030721     C                   Z-ADD     PiVal         Num5_0
116500030721     C                   MOVEL(p)  Num5_0        VABCAD
116600030721     C                   ELSE
116700030721     C                   SETON                                        32
116800030721     C                   EVAL      VABCAD = *zeros
116900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
117000030721     C                             + ' ' + 'VABCAD'
117100030721     C                   ENDIF
117200030721     C                   ENDIF
117300030721     C*** NCL
117400050415     C                   IF        %trim(SkSplitFLD(i)) = 'N_COLLI'
117500030721     C                   EVAL      PiStr=SkSplitCSV(i)
117600030721     C                   EXSR      CHKNUM
117700030721     C                   IF        PiInt=*on
117800030721     C                   Z-ADD     PiVal         VABNCL
117900030721     C                   ELSE
118000030721     C                   SETON                                        32
118100030721     C                   EVAL      VABNCL = *zeros
118200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
118300040706     C                             + ' ' + 'VABNCL'
118400030721     C                   ENDIF
118500030721     C                   ENDIF
118600030721     C*** PKB
118700050415     C                   IF        %trim(SkSplitFLD(i)) = 'PESO_KG'
118800030721     C                   EVAL      PiStr=SkSplitCSV(i)
118900030721     C                   EXSR      CHKNUM
119000030721     C                   IF        PiNum=*on
119100050415     C                   Z-ADD(H)  PiVal         VABPKB
119200030721     C                   ELSE
119300030721     C                   SETON                                        32
119400030721     C                   EVAL      VABPKB = *zeros
119500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
119600030721     C                             + ' ' + 'VABPKB'
119700030721     C                   ENDIF
119800030721     C                   ENDIF
119900030721     C*** CAS
120000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAS'
120100030721     C                   EVAL      PiStr=SkSplitCSV(i)
120200030721     C                   EXSR      CHKNUM
120300030721     C                   IF        PiNum=*on
120400030721     C                   Z-ADD     PiVal         VABCAS
120500030721     C                   ELSE
120600030721     C                   SETON                                        32
120700030721     C                   EVAL      VABCAS = *zeros
120800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
120900030721     C                             + ' ' + 'VABCAS'
121000030721     C                   ENDIF
121100030721     C                   ENDIF
121200030721     C*** CCM
121300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCCM'
121400030721     C                   EVAL      PiStr=SkSplitCSV(i)
121500030721     C                   EXSR      CHKNUM
121600030721     C                   IF        PiInt=*on      AND
121700030721     C                             PiVal<=9999999 AND
121800030721     C                             PiVal>*zeros
121900030721     C                   Z-ADD     PiVal         VABCCM
122000040119     C                   Z-ADD     PiVal         VATCCM
122100050511     C                   Z-ADD     PiVal         VADCCM
122200030721     C                   ELSE
122300030721     C                   SETON                                        32
122400030721     C                   EVAL      VABCCM = *zeros
122500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
122600050511     C                             + ' ' + 'VABCCM VATCCM VADCCM'
122700030721     C                   ENDIF
122800030721     C                   ENDIF
122900030721     C*** LNP
123000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNP'
123100030721     C                   EVAL      PiStr=SkSplitCSV(i)
123200030721     C                   EXSR      CHKNUM
123300030721     C                   IF        PiInt=*on    AND
123400030721     C                             PiVal<=999   AND
123500030721     C                             PiVal>*zeros
123600030721     C                   Z-ADD     PiVal         VABLNP
123700040119     C                   Z-ADD     PiVal         VATLNP
123800050511     C                   Z-ADD     PiVal         VADLNP
123900030721     C                   ELSE
124000030721     C                   SETON                                        32
124100030721     C                   EVAL      VABLNP = *zeros
124200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
124300050511     C                             + ' ' + 'VABLNP VATLNP VADLNP'
124400030721     C                   ENDIF
124500030721     C                   ENDIF
124600030721     C*** NRS
124700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNRS'
124800030721     C                   EVAL      PiStr=SkSplitCSV(i)
124900030721     C                   EXSR      CHKNUM
125000030721     C                   IF        PiInt=*on AND
125100030721     C                             PiVal<=99
125200030721     C                   Z-ADD     PiVal         VABNRS
125300030721     C                   ELSE
125400030721     C                   SETON                                        32
125500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
125600030721     C                             + ' ' + 'VABNRS'
125700030721     C                   ENDIF
125800030721     C                   ENDIF
125900030721     C*** LNA
126000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNA'
126100030721     C                   EVAL      PiStr=SkSplitCSV(i)
126200030721     C                   EXSR      CHKNUM
126300030721     C                   IF        PiInt=*on    AND
126400030721     C                             PiVal<=999   AND
126500030721     C                             PiVal>*zeros
126600030721     C                   Z-ADD     PiVal         VABLNA
126700030721     C                   ELSE
126800030721     C                   SETON                                        32
126900030721     C                   EVAL      VABLNA = *zeros
127000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
127100030721     C                             + ' ' + 'VABLNA'
127200030721     C                   ENDIF
127300030721     C                   ENDIF
127400030721     C*** CTR
127500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTR'
127600030721     C                   EVAL      PiStr=SkSplitCSV(i)
127700030721     C                   EXSR      CHKNUM
127800030721     C                   IF        PiInt=*on  AND
127900030721     C                             PiVal<=999
128000030721     C                   Z-ADD     PiVal         VABCTR
128100030721     C                   ELSE
128200030721     C                   SETON                                        32
128300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
128400030721     C                             + ' ' + 'VABCTR'
128500030721     C                   ENDIF
128600030721     C                   ENDIF
128700030721     C*** IAS
128800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABIAS'
128900030721     C                   EVAL      PiStr=SkSplitCSV(i)
129000030721     C                   EXSR      CHKNUM
129100030721     C                   IF        PiNum=*on
129200030721     C                   Z-ADD     PiVal         VABIAS
129300030721     C                   ELSE
129400030721     C                   SETON                                        32
129500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
129600030721     C                             + ' ' + 'VABIAS'
129700030721     C                   ENDIF
129800030721     C                   ENDIF
129900030721     C*** VLB
130000050415     C                   IF        %trim(SkSplitFLD(i)) = 'VOLUME'
130100030721     C                   EVAL      PiStr=SkSplitCSV(i)
130200030721     C                   EXSR      CHKNUM
130300030721     C                   IF        PiNum=*on
130400050415     C                   Z-ADD(H)  PiVal         VABVLB
130500030721     C                   ELSE
130600030721     C                   SETON                                        32
130700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
130800030721     C                             + ' ' + 'VABVLB'
130900030721     C                   ENDIF
131000030721     C                   ENDIF
131100030721     C*** QFT
131200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABQFT'
131300030721     C                   EVAL      PiStr=SkSplitCSV(i)
131400030721     C                   EXSR      CHKNUM
131500030721     C                   IF        PiNum=*on
131600030721     C                   Z-ADD     PiVal         VABQFT
131700030721     C                   ELSE
131800030721     C                   SETON                                        32
131900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
132000030721     C                             + ' ' + 'VABQFT'
132100030721     C                   ENDIF
132200030721     C                   ENDIF
132300030721     C*** NCD
132400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCD'
132500030721     C                   EVAL      PiStr=SkSplitCSV(i)
132600030721     C                   EXSR      CHKNUM
132700030721     C                   IF        PiInt=*on      AND
132800030721     C                             PiVal<=9999999
132900030721     C                   Z-ADD     PiVal         VABNCD
133000030721     C                   ELSE
133100030721     C                   SETON                                        32
133200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
133300030721     C                             + ' ' + 'VABNCD'
133400030721     C                   ENDIF
133500030721     C                   ENDIF
133600030721     C*** NCA
133700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCA'
133800030721     C                   EVAL      PiStr=SkSplitCSV(i)
133900030721     C                   EXSR      CHKNUM
134000030721     C                   IF        PiInt=*on      AND
134100030721     C                             PiVal<=9999999
134200030721     C                   Z-ADD     PiVal         VABNCA
134300030721     C                   ELSE
134400030721     C                   SETON                                        32
134500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
134600030721     C                             + ' ' + 'VABNCA'
134700030721     C                   ENDIF
134800030721     C                   ENDIF
134900030721     C*** ZNC
135000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABZNC'
135100030721     C                   EVAL      PiStr=SkSplitCSV(i)
135200030721     C                   EXSR      CHKNUM
135300030721     C                   IF        PiInt=*on AND
135400030721     C                             PiVal<=99
135500030721     C                   Z-ADD     PiVal         VABZNC
135600030721     C                   ELSE
135700030721     C                   SETON                                        32
135800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
135900030721     C                             + ' ' + 'VABZNC'
136000030721     C                   ENDIF
136100030721     C                   ENDIF
136200030721     C*** DCR
136300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABDCR'
136400030721     C                   EVAL      PiStr=SkSplitCSV(i)
136500030721     C                   EXSR      CHKNUM
136600030721     C                   IF        PiInt=*on       AND
136700030721     C                             PiVal<=99999999
136800030721     C                   Z-ADD     PiVal         VABDCR
136900030721     C                   ELSE
137000030721     C                   SETON                                        32
137100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
137200030721     C                             + ' ' + 'VABDCR'
137300030721     C                   ENDIF
137400030721     C                   ENDIF
137500030721     C*** HCR
137600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABHCR'
137700030721     C                   EVAL      PiStr=SkSplitCSV(i)
137800030721     C                   EXSR      CHKNUM
137900030721     C                   IF        PiInt=*on   AND
138000030721     C                             PiVal<=9999
138100030721     C                   Z-ADD     PiVal         VABHCR
138200030721     C                   ELSE
138300030721     C                   SETON                                        32
138400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
138500030721     C                             + ' ' + 'VABHCR'
138600030721     C                   ENDIF
138700030721     C                   ENDIF
138800030721     C*** VMD
138900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVMD'
139000030721     C                   EVAL      PiStr=SkSplitCSV(i)
139100030721     C                   EXSR      CHKNUM
139200030721     C                   IF        PiNum=*on
139300030721     C                   Z-ADD     PiVal         VABVMD
139400030721     C                   ELSE
139500030721     C                   SETON                                        32
139600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
139700030721     C                             + ' ' + 'VABVMD'
139800030721     C                   ENDIF
139900030721     C                   ENDIF
140000030721     C*** ANT
140100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABANT'
140200030721     C                   EVAL      PiStr=SkSplitCSV(i)
140300030721     C                   EXSR      CHKNUM
140400030721     C                   IF        PiInt=*on        AND
140500030721     C                             PiVal<=999999999
140600030721     C                   Z-ADD     PiVal         VABANT
140700030721     C                   ELSE
140800030721     C                   SETON                                        32
140900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
141000030721     C                             + ' ' + 'VABANT'
141100030721     C                   ENDIF
141200030721     C                   ENDIF
141300030721     C*** CMO
141400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCMO'
141500030721     C                   EVAL      PiStr=SkSplitCSV(i)
141600030721     C                   EXSR      CHKNUM
141700030721     C                   IF        PiInt=*on
141800030721     C                   Z-ADD     PiVal         Num5_0
141900030721     C                   MOVEL(p)  Num5_0        VABCMO
142000030721     C                   ELSE
142100030721     C                   SETON                                        32
142200030721     C                   EVAL      VABCMO = *zeros
142300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
142400030721     C                             + ' ' + 'VABCMO'
142500030721     C                   ENDIF
142600030721     C                   ENDIF
142700030715     C*
142800030715     C                   ENDIF
142900030715     C                   EVAL      i = i + 1
143000030715     C                   ENDDO
143100050415     C*
143200050415     C* Effettuo ora considerazioni MOLTO particolari x l'attribuzione il +
143300050415     C* possibile corretta ed efficace ai fini d 1 corretto instradamento
143400050415     C*
143500050415     C* RAGIONE SOCIALE: VABRSD e VABRD2
143600050415     C                   EVAL      dsRAG = %trim(%trim(wRAGSOC1)+' '+
143700050415     C                                           %trim(wRAGSOC2))
143800050415     C*
143900050415     C* LOCALITA: VABLOD
144000050415     C                   IF        wPOSTALIZ_R3 <> *blanks
144100050415     C                   EVAL      VABLOD = %subst(%trim(wPOSTALIZ_R3):7:
144200050415     C                                      %len(%trim(wPOSTALIZ_R3))-3-7+1)
144300050415     C                   EVAL      wInR3 = *on
144400050415     C                   ELSE
144500050415     C                   IF        wPOSTALIZ_R2 <> *blanks
144600050415     C                   EVAL      VABLOD = %subst(%trim(wPOSTALIZ_R2):7:
144700050415     C                                      %len(%trim(wPOSTALIZ_R2))-3-7+1)
144800050415     C                   EVAL      wInR2 = *on
144900050415     C                   ELSE
145000050415     C                   IF        wLOCALITA <> *blanks
145100050415     C                   EVAL      VABLOD = %trim(wLOCALITA)
145200050415     C                   EVAL      wInLOC = *on
145300050415     C                   ENDIF
145400050415     C                   ENDIF
145500050415     C                   ENDIF
145600050415     C*
145700050415     C* INDIRIZZO: VABIND
145800050415     C                   IF        wPOSTALIZ_R1 <> *blanks
145900050415     C                   EVAL      VABIND = wPOSTALIZ_R1
146000050415     C                   ELSE
146100050415     C                   EVAL      VABIND = VABLOD
146200050415     C                   ENDIF
146300050415     C*
146400050415     C* NOTE: VABNOT e VABNT2
146500050415     C                   IF        wInR3 = *on
146600050415     C                   EVAL      wNOTE1 = wPOSTALIZ_R2
146700050415     C                   EVAL      wNOTE2 = wLOCALITA
146800050415     C                   ENDIF
146900050415     C                   IF        wInR2 = *on
147000050415     C                   EVAL      wNOTE1 = wLOCALITA
147100050415     C                   EVAL      wNOTE2 = wPOSTALIZ_R3
147200050415     C                   ENDIF
147300050415     C                   IF        wInLOC = *on
147400050415     C                   EVAL      wNOTE1 = wPOSTALIZ_R3
147500050415     C                   EVAL      wNOTE2 = wPOSTALIZ_R2
147600050415     C                   ENDIF
147700050415     C*
147800050415     C                   EVAL      dsNOT = %trim(%trim(wNOTE1)+' '+
147900050415     C                                           %trim(wNOTE2))
148000050511     C*
148100050511     C* Eseguo considerazioni sul conteggio dei record elaborati x distinguere <n> gruppi x CMR
148200050511     C                   EVAL      wContaRec = wContaRec + 1
148300050511     C                   IF        wContaRec > wGruppiDa OR
148400050511     C                             VABRMA <> wDepRMA
148500050511     C                   EVAL      wContaGruppi = wContaGruppi + 1
148600050511     C                   EVAL      wContaRec = *zeros
148700050511     C                   EVAL      wDepRMA = VABRMA
148800050511     C                   ENDIF
148900050511     C                   MOVEL     wContaGruppi  wContaGruppiA
149000050511     C*
149100050511     C* Eseguo valorizzazioni FARZATE x campi aggiuntivi d EDIVAB/EDIVAT/EDIVAD
149200050511     C                   EVAL      VABCMR = %trim(VABRMA)+'-'+wContaGruppiA
149300050511     C                   EVAL      VABDCM = datcor
149400050511     C                   EVAL      VABDTS = datcor
149500050511     C                   EVAL      VABHMS = 1200
149600050511     C                   EVAL      VABCNT = 1
149700050511     C*
149800040706     C* Eseguo particolari considerazioni in quanto RMA e RMN cerco x quanto possibile
149900040706     C* d valorizzarli sempre entrambi
150000040706     C                   IF        VABNSP <> *zeros  AND VABRMN = *zeros
150100040706     C                   Z-ADD     VABNSP        VABRMN
150200040706     C                   ENDIF
150300040706     C                   IF        VABRMN <> *zeros  AND VABRMA = *blanks
150400040706     C                   MOVEL     VABRMN        VABRMA
150500040706     C                   ENDIF
150600040113     C*
150700040113     C* Eseguo considerazioni sul tipo bolla in relaziione al contrassegno
150800040113     C                   IF        VABCAS > *zeros
150900040119     C                   EVAL      VABVCA = 'EUR'
151000040119     C                   IF        VABCBO = '1'
151100040113     C                   EVAL      VABCBO = '4'
151200040119     C                   ENDIF
151300040119     C                   IF        VABCBO = '2'
151400040119     C                   EVAL      VABCBO = '6'
151500040119     C                   ENDIF
151600040113     C                   ENDIF
151700020204     C*
151800020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
151900020204     C                   EXSR      CHKIMPDIV
152000030715     C                   ENDIF
152100020204     C*
152200010607     C                   ENDSR
152300010607     C*----------------------------------------------------*
152400010601
152500020204
152600020204     C*----------------------------------------------------*
152700020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
152800020204     C*----------------------------------------------------*
152900020204     C     CHKIMPDIV     BEGSR
153000020204     C*
153100020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
153200020204     C                   Z-ADD     *zeros        wrkDec            9 9
153300020204     C*
153400020204     C* Come prima cosa effettuo considerazioni sulla divisa
153500020204     C                   IF        vabIAS > *zeros
153600020204     C                   IF        vabVAS <> 'EUR'
153700020204     C                   EVAL      vabVAS =  'ITL'
153800020204     C                   ENDIF
153900020204     C                   ENDIF
154000020204     C*
154100020204     C                   IF        vabCAS > *zeros
154200020204     C                   IF        vabVCA <> 'EUR'
154300020204     C                   EVAL      vabVCA =  'ITL'
154400020204     C                   ENDIF
154500020204     C                   ENDIF
154600020204     C*
154700020204     C                   IF        vabVMD > *zeros
154800020204     C                   IF        vabVAD <> 'EUR'
154900020204     C                   EVAL      vabVAD =  'ITL'
155000020204     C                   ENDIF
155100020204     C                   ENDIF
155200020204     C*
155300020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
155400020204     C                   Z-ADD     vabIAS        wrkDec
155500020204     C                   IF        wrkDec > *zeros
155600020204     C                   IF        vabVAS = 'ITL'
155700020204     C                   EVAL      vabIAS = *zeros
155800020204     C                   ENDIF
155900020204     C                   ENDIF
156000020204     C*
156100020204     C* Stabilisco se il contrasegno ha decimali valorizzati
156200020204     C                   Z-ADD     vabCAS        wrkDec
156300020204     C                   IF        wrkDec > *zeros
156400020204     C                   IF        vabVCA = 'ITL'
156500020204     C                   EVAL      vabCAS = *zeros
156600020204     C                   ENDIF
156700020204     C                   ENDIF
156800020204     C*
156900020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
157000020204     C                   Z-ADD     vabVMD        wrkDec
157100020204     C                   IF        wrkDec > *zeros
157200020204     C                   IF        vabVAD = 'ITL'
157300020204     C                   EVAL      vabVMD = *zeros
157400020204     C                   ENDIF
157500020204     C                   ENDIF
157600020204     C*
157700020204     C                   ENDSR
157800020204     C***
157900020204
158000010330
158100010330
158200010330     C*----------------------------------------------------*
158300010330     C*  CONTROLLO NUMERICITA' CAMPI
158400010330     C*----------------------------------------------------*
158500010330     C     CHKNUM        BEGSR
158600010330     C*
158700010606     C                   IF        PiDecChr = *blanks
158800030715     C                   EVAL      PiDecChr = CharNUM
158900010606     C                   ENDIF
159000010606     C*
159100010606     C                   CALL(e)   'ISNUMERIC'
159200010330     C                   PARM                    PiStr            30
159300010606     C                   PARM                    PiDecChr          1
159400010330     C                   PARM      *ZEROS        PiVal            30 9
159500010330     C                   PARM      '0'           PiInt             1
159600010330     C                   PARM      '0'           PiNum             1
159700010330     C                   IF        %error
159800010606     C                   EVAL      PiNum=*off
159900010330     C                   ENDIF
160000010330     C*
160100010330     C                   ENDSR
160200010330     C***
160300010330
160400010601
160500010601
160600010601
160700010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
160800010601     C     repfil        BEGSR
160900010601     C*
161000010601     C                   if        invfil = *zeros and
161100010601     C                             depfil > *zeros and
161200010629     C                             (vinflg = *blanks or
161300010629     C                              vinflg = *zeros)
161400010601     C
161500010601     C                   eval      invfil = depfil
161600010601     C                   endif
161700010601     C*
161800010601     C                   if        depfil <> invfil and
161900010601     C                             invfil > *zeros
162000010601     C                   eval      flgMulti = '1'
162100010601     C                   if        vinflg = *blanks
162200010601     C                   add       1             cntNonEl
162300010601     C                   endif
162400010601     C                   endif
162500010601     C*
162600010601     C                   if        vinflg = '2'
162700010601     C                   eval      flgStato = '2'
162800010601     C                   endif
162900010601     C*
163000010601     C                   ENDSR
163100010601     C***
163200010601
163300010601
163400010601
163500010330
163600040119      /TITLE Invio dei dati al punto operativo.
163700040119     C     invio         BEGSR
163800040119     C*
163900050511     C* 1° invio EDIVAT
164000040119     C                   reset                   dscmz
164100040119     C                   move      vlrpoi        cmzdst
164200050511     C                   eval      cmzfld = 'EDIVATWR'
164300040119     C                   eval      cmzmbd = vlrhdl
164400040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
164500040119     C***                if        prmfir = *blanks
164600050511     C                   eval      cmzfla = 'EDIVAT0F'
164700050511     C                   eval      cmzmba = 'EDIVAT0F'
164800040119     C***                else
164900040119     C***                eval      cmzfla = prmfir
165000040119     C***                eval      cmzmba = prmfir
165100040119     C***                endif
165200040119     C                   eval      cmznrr = *zeros
165300040119     C                   move      §ctrokvt      cmznrr
165400040119     C                   eval      cmzlba = vlrfl1
165500040119     C                   call(e)   'TIS711C'
165600040119     C                   parm                    dscmz
165700040119     C                   parm      *blanks       esito
165800040119     C                   if        %error
165900040119     C                             or cmzerr = '1'
166000040119     C                             or esito  = '1'
166100040119     C                   eval      wrkesito = '3'
166200040119     C                   else
166300050511     C*
166400050511     C* 2° invio EDIVAD
166500050511     C                   reset                   dscmz
166600050511     C                   move      vlrpoi        cmzdst
166700050511     C                   eval      cmzfld = 'EDIVADWR'
166800050511     C                   eval      cmzmbd = vlrhdl
166900050511     C                   eval      %subst(cmzmbd:1:1) = 'M'
167000050511     C***                if        prmfir = *blanks
167100050511     C                   eval      cmzfla = 'EDIVAD0F'
167200050511     C                   eval      cmzmba = 'EDIVAD0F'
167300050511     C***                else
167400050511     C***                eval      cmzfla = prmfir
167500050511     C***                eval      cmzmba = prmfir
167600050511     C***                endif
167700050511     C                   eval      cmznrr = *zeros
167800050511     C                   move      §ctrokvd      cmznrr
167900050511     C                   eval      cmzlba = vlrfl1
168000050511     C                   call(e)   'TIS711C'
168100050511     C                   parm                    dscmz
168200050511     C                   parm      *blanks       esito
168300050511     C                   if        %error
168400050511     C                             or cmzerr = '1'
168500050511     C                             or esito  = '1'
168600050511     C                   eval      wrkesito = '3'
168700050511     C                   else
168800040119     C*
168900050511     C* 3° invio EDIVAB
169000040119     C                   reset                   dscmz
169100040119     C                   move      vlrpoi        cmzdst
169200040119     C                   eval      cmzfld = vlrfou
169300040119     C                   eval      cmzmbd = vlrhdl
169400040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
169500040119     C***                if        prmfir = *blanks
169600050511     C                   eval      cmzfla = 'EDIVAB0F'
169700050511     C                   eval      cmzmba = 'EDIVAB0F'
169800040119     C***                else
169900040119     C***                eval      cmzfla = prmfir
170000040119     C***                eval      cmzmba = prmfir
170100040119     C***                endif
170200040119     C                   eval      cmznrr = *zeros
170300040119     C                   move      §ctrokvb      cmznrr
170400040119     C                   eval      cmzlba = vlrfl1
170500040119     C                   call(e)   'TIS711C'
170600040119     C                   parm                    dscmz
170700040119     C                   parm      *blanks       esito
170800040119     C                   if        %error
170900040119     C                             or cmzerr = '1'
171000040119     C                             or esito  = '1'
171100040119     C                   eval      wrkesito = '3'
171200040119     C                   endif
171300040119     C                   endif
171400050511     C                   endif
171500040119     C*
171600040119     C                   ENDSR
171700040119     C***
171800010601
171900010601
172000010601
172100010601
172200010601      /TITLE Invio dei dati al punto operativo.
172300010601     C     opeini        BEGSR
172400010601     C*
172500010601     C* Inizializzo flag e contatori operativi
172600010601     C                   movel     '0'           flgGiro           1
172700010601     C                   movel     '0'           flgMulti          1
172800010601     C                   movel     '1'           flgStato          1
172900010615     C                   movel     '0'           flgOk             1
173000010601     C                   z-add     *zeros        cntNonEl         10 0
173100010601     C                   z-add     *zeros        depfil            3 0
173200010601     C                   z-add     *zeros        invfil            3 0
173300010601     C*
173400010601     C                   ENDSR
173500010601     C***
173600010601
173700010601
173800050421
173900050421
174000070502     C     *pssr         BEGSR
174100070502     C*
174200080930     C                   if        %open(tivin00r)
174300080930     C                   close     tivin00r
174400080930     C                   endif
174500070502     C                   if        %open(edivabwr)
174600070502     C                   close     edivabwr
174700070502     C                   endif
174800070502     C                   if        %open(edivatwr)
174900070502     C                   close     edivatwr
175000070502     C                   endif
175100070502     C                   if        %open(edivadwr)
175200070502     C                   close     edivadwr
175300070502     C                   endif
175400070502     C*
175500070502     C* Effettuo la chiamata al CLLE preposto
175600070502     C                   call(e)   'TITVEVTC'
175700070502     C                   parm                    parccm
175800070502     C                   parm                    parmbr
175900070502     C                   parm      '2'           paropz
176000070502     C                   call(e)   'TITVEVDC'
176100070502     C                   parm                    parccm
176200070502     C                   parm                    parmbr
176300070502     C                   parm      '2'           paropz
176400050421     C*
176500050421     C                   eval      wrkesito = '2'
176600050421     C*
176700050421     C                   ENDSR     '*CANCL'
176800050421     C***
176900050421
177000010601
177100010330
177200010330
177300000613     C     *inzsr        BEGSR
177400990910     C*
177500990910     C     *entry        plist
177600990920     C                   parm                    tivlrds
177700990921     C                   parm      wrkesito      esito
177800000724     C                   parm                    prmlit
177900000710     C                   parm                    prmfir
178000010330     C*
178100010330     C* CALCOLA LA DATA CORRENTE
178200010330     C                   time                    wn14             14 0
178300010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
178400010330     C                   z-add     wn8           g08dat
178500010330     C                   z-add     *zeros        g08inv
178600010330     C                   movel     '0'           g08err
178700010330     C                   call      'XSRDA8'
178800010330     C                   parm                    wlbda8
178900010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
179000000613     C*
179100000613     C                   ENDSR
179200000613     C***
179300990908
179400050415     Otitvm3p   E            riepilogo         2
179500990915     O                                              'Upload via Internet'
179600990915     O                                           +1 'Traduzione TIVIN00R -'
179700050511     O                                           +1 'EDIVABWR'
179800990915     O                                           +1 'Testate bolle'
179900990915     O          E            riepilogo   2
180000990915     O                       wrkdata
180100990915     O                       wrkora              +1
180200990915     O                       procname            +1
180300990915     O          E            riepilogo   2
180400990915     O                                              'Cliente..................:'
180500010605     O                       VABCCM        z     +1
180600990915     O          E            riepilogo   2
180700990920     O                                              'Riferimento Strategi.....:'
180800990920     O                       vlrhdl              +1
180900990915     O          E            riepilogo   2
181000990915     O                                              'Giusti...................:'
181100040119     O                       §CTROKVB      2   +  1
181200990915     O          E            riepilogo   2
181300990915     O                                              'Sbagliati e corretti.....:'
181400971022     O                       §CTRMO        2   +  1
181500990915     O          E            riepilogo   2
181600990915     O                                              'Sbagliati e scartati.....:'
181700971022     O                       §CTRNO        2   +  1
181800000613
181900050415     Otitvm3ps  E            testdett          2
182000000613     O                                              'Upload via Internet'
182100000613     O                                           +1 'Traduzione TIVIN00R -'
182200050511     O                                           +1 'EDIVABWR'
182300000613     O                                           +1 'Testate bolle'
182400000616     O          E            testdett    3
182500000613     O                                           +2 'N° rec'
182600000613     O                                           +3 'Anteprima contenuto'
182700000616     O          E            rigadett    2
182800000613     O                       rrnum               +2
182900000621     O                       recko               +3
183000000616     O          E            findett     2
183100000613     O                       wrkdata
183200000613     O                       wrkora              +1
183300000613     O                       procname            +1
183400000616     O          E            findett     2
183500000613     O                                              'Cliente..................:'
183600010605     O                       VABCCM        z     +1
183700000616     O          E            findett     2
183800000613     O                                              'Riferimento Strategi.....:'
183900000613     O                       vlrhdl              +1
184000000616     O          E            findett     2
184100000613     O                                              'Giusti...................:'
184200040119     O                       §CTROKVB      2   +  1
184300000616     O          E            findett     2
184400000613     O                                              'Sbagliati e corretti.....:'
184500000613     O                       §CTRMO        2   +  1
184600000616     O          E            findett     2
184700000613     O                                              'Sbagliati e scartati.....:'
184800000613     O                       §CTRNO        2   +  1
184900000613** CMD - COMANDI CL
185000050415OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITVM3P)  TOFILE(TIS7T1P)  FORMTYPE(RICCLI) OUTQ(
185100050415OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITVM3PS) TOFILE(TIS7T1PS) FORMTYPE(RICCLI) OUTQ(
185200050415DLTOVR FILE(TITVM3P TITVM3PS) LVL(*)
185300010606
185400000613
185500040113** INTCOL - INTESTAZIONE COLONNE PERSONALIZZATA
185600050415CODICE_PARROCCHIA;RGS_1;RGS_2;POSTALIZZAZIONE_R1;POSTALIZZAZIONE_R2;
185700050415POSTALIZZAZIONE_R3;LOCALITA;CAP;PROVINCIA;NUMERO_TELEFONO_1;
185800050415DESC_MERCE;N_COLLI;PESO_KG;VOLUME;RIF_MITT
